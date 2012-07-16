-- "src/Dao/Tasks.hs"  provides functions for executing the Dao scripts
-- in threads called "tasks" once they are selected by pattern matching.
-- 
-- Copyright (C) 2008-2012  Ramin Honary.
-- This file is part of the Dao System.
--
-- The Dao System is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
-- 
-- The Dao System is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.


{-# LANGUAGE TemplateHaskell #-}

module Dao.Tasks where

-- | This module is pretty much where everything happens. The pattern matching and action execution
-- algorithm that defines the unique nature of the Dao system, the 'execInputString' function, is
-- defined here. So are 'Dao.Types.Job' and 'Dao.Types.Task' management functions, and a simple
-- interactive run loop 'interactiveRuntimeLoop' is also provided.

import           Dao.Debug.ON

import           Dao.Types
import qualified Dao.Tree as T
import           Dao.Pattern
import           Dao.Combination
import           Dao.Parser
import           Dao.Document
import           Dao.Evaluator
import           Dao.Files
import           Dao.Object.Monad
import           Dao.Object.Parsers
import           Dao.Object.Show

import           Control.Exception
import           Control.Concurrent (forkIO, newMVar, modifyMVar)
import           Control.Monad.Reader

import           Data.Maybe
import           Data.List   (intercalate)
import qualified Data.Map    as M

import           System.IO

import Debug.Trace

----------------------------------------------------------------------------------------------------

-- | Get the name 'Dao.Evaluator. of 
taskProgramName :: Task -> Maybe Name
taskProgramName task = case task of
  RuleTask  _ _ _ xunit -> fn xunit
  GuardTask     _ xunit -> fn xunit
  where { fn xunit = fmap programModuleName (currentProgram xunit) }

-- | This actually executes the 'Task', essentially converting it into @IO@ function. The resulting
-- @IO@ function can be evlauted in a separate thread to create an entry in the 'jobTable' of a
-- 'Job'; this is what 'newJob' does with each 'Task' passed to it, it calls 'execTask' within
-- 'Control.Concurrent.forkIO'.
execTask :: Job -> Task -> Run ()
execTask job task = dStack $loc "execTask" $ ask >>= \runtime -> do
  let run patn mtch action xunit fn = do
        exec   <- getCachedExec (nestedExecStack M.empty . fn) action
        result <- dStack $loc "execTask/runExecScript" $ lift $ try $ runIO runtime $
          runExecScript exec $
            xunit{currentPattern = patn, currentMatch = mtch, currentExecJob = Just job}
        let putErr err = dModifyMVar_ $loc (uncaughtErrors xunit) (return . (++[err]))
        case seq result result of
          Right (CEError       err) -> dPutStrErr $loc (showObj 0 err) >> putErr err
          Left  (SomeException err) -> do
            dPutStrErr $loc (show err)
            putErr (OString (ustr (show err)))
          _                         -> return ()
  case task of
    RuleTask  patn mtch action xunit -> run patn  (Just mtch) action xunit execScriptBlock
    GuardTask           action xunit -> run ONull Nothing     action xunit execGuardBlock

----------------------------------------------------------------------------------------------------

-- | Creates zero or one 'Job's, but returns a list of 'Job's because it works better with the rest
-- of the functions in this module if it returns a list. Creating a job with 'newJob' automatically
-- sets-up the threads to handle exceptions, and the job will place itself into, and remove itself
-- from, the given 'Runtime's 'jobTable'.
newJob :: Maybe Int -> UStr -> Run Job
newJob timed instr = dStack $loc "newJob" $ ask >>= \runtime -> do
  jobQSem <- dNewQSem $loc "Job.jobCompletion" 0 -- is singaled when all tasks have completed
  taskEnd <- dNewEmptyMVar $loc "Job.taskCompletion" -- is used as to block the manager loop until tasks complete
  ready   <- dNewEmptyMVar $loc "Job.readyTasks" -- tasks to be executed will be queued here
  timeVar <- dNewMVar $loc "Job.jobTimerThread" Nothing -- contains the timer value, will be filled after defining 'timeKeeper'
  failed  <- dNewMVar $loc "Job.taskFailures" (M.empty) -- contains the table of any exceptions that killed a worker thread
  taskTab <- dNewMVar $loc "Job.taskExecTable" (M.empty) -- this DMVar contains the table of running task threads
  jobMVar <- dNewEmptyMVar $loc "Job/jobMVar" -- every task created needs to set it's 'ExecUnit's 'currentExecJob'.
  let
      taskWaitLoop = dStack $loc "taskWaitLoop" $ do
        -- This loop waits for tasks to end, and signals the 'jobQSem' when the last job has
        -- completed. This loop is actually called by the 'manager' loop, so it executes in the same
        -- thread as the manager loop. Exception handling does not occur here.
        thread  <- dTakeMVar $loc taskEnd -- wait for a task to end.
        allDone <- dModifyMVar $loc taskTab $ \taskTab -> do
          let taskTab' = M.delete thread taskTab -- remove the completed task from the taskTab.
          return (taskTab', M.null taskTab') -- returns whether or not the taskTab is empty.
        if allDone
          then do -- when the taskTab is empty...
            timer <- dSwapMVar $loc timeVar Nothing -- empty the timeVar DMVar.
            case timer of -- and kill the timer thread (if it is waiting)
              Nothing    -> return ()
              Just timer -> dKillThread $loc timer
            dSignalQSem $loc jobQSem -- then signal 'jobCompletion'.
          else taskWaitLoop -- loop if the taskTab is not empty.
      workerException task err@(SomeException e) =
        -- If a worker is flagged, it needs to place this flag into the 'failed' table.
        dModifyMVar_ $loc failed $ \ftab -> case taskProgramName task of
          Nothing   -> dPutStrErr $loc (show e) >> return ftab
          Just name -> return (M.update (Just . (++[(task, err)])) name ftab)
      worker task = dStack $loc "Job/worker" $ do
        -- A worker runs the 'task', catching exceptions with 'workerException', and it always
        -- signals the manager loop when this thread completes by way of the 'taskEnd' DMVar, even
        -- when an exception occurs.
        job <- dReadMVar $loc jobMVar
        dHandle $loc (workerException task) (execTask job task)
        dMyThreadId >>= dPutMVar $loc taskEnd
      timeKeeper time = do
        -- This function evaluates in a separate thread and delays itself then kills all running
        -- tasks in the taskTab. When a task is killed, it signals the manager loop via the
        -- 'taskEnd' DMVar, which will signal 'jobQSem' when the taskTab goes empty, thus timeKeeper
        -- does not kill the manager loop, it lets the manager clean-up and wait on the next batch
        -- of tasks.
        dThreadDelay $loc time
        dSwapMVar $loc timeVar Nothing
        dSwapMVar $loc taskTab (M.empty) >>= mapM_ (dKillThread $loc) . (M.keys)
      managerException (SomeException e) = do
        -- if the manager loop is flagged, it needs to delegate the flag to all of its workers.
        dSwapMVar $loc taskTab (M.empty) >>= mapM_ (\th -> dThrowTo $loc th e) . (M.keys)
        dSwapMVar $loc timeVar Nothing >>= \timer -> case timer of
          Nothing     -> return ()
          Just thread -> dThrowTo $loc thread e
      manager = dStack $loc "Job/manager" $ do
        -- This is the main loop for the 'Job' controlling thread. It takes 'Task's from the
        -- 'readyTasks' table, waiting for the DMVar if necessary.
        tasks <- dTakeMVar $loc ready
        dMessage $loc ("received "++show (length tasks)++" tasks")
        case tasks of
          []    -> manager
          tasks -> do
            dModifyMVar_ $loc taskTab $ \taskExecTable -> do
              -- Launch all threads, filling the 'taskTab' DMVar atomically so that if a worker loop
              -- fails due to an exception and signals the manager that it is ready to be removed
              -- from the taskTab, the manager loop will not be able to modify this taskTab until
              -- the taskTab is completely filled-in.
              taskIDs <- forM tasks $ \task -> do
                this <- dFork forkIO $loc ("worker("++showTask task++")") (worker task)
                return (this, task)
              return (M.union (M.fromList taskIDs) taskExecTable)
            -- Launch a timekeeper thread if necessary.
            case timed of
              Nothing   -> void $ dSwapMVar $loc timeVar Nothing
              Just time -> dFork forkIO $loc ("timer("++show instr++")") (timeKeeper time) >>=
                void . dSwapMVar $loc timeVar . Just
            taskWaitLoop >> manager -- wait for the all tasks to stop, then loop to the next batch.
  -- Finally, start the thread manager loop, wrapped up in it's exception handler.
  jobManager <- dFork forkIO $loc ("jobManager("++show instr++")") $
    dHandle $loc managerException manager
  let job = Job
            { jobTaskThread  = jobManager
            , jobInputString = instr
            , jobTimerThread = timeVar
            , jobCompletion  = jobQSem
            , taskCompletion = taskEnd
            , readyTasks     = ready
            , taskExecTable  = taskTab
            , taskFailures   = failed
            }
  dPutMVar $loc jobMVar job -- as soon as this happens, all tasks will be able to run.
  return job

-- | This function simple places a list of 'Task's into the 'Job's 'readyTasks' table. This function
-- will block if another thread has already evaluated this function, but those tasks have not yet
-- completed or timed-out.
startTasksForJob :: Job -> [Task] -> Run ()
startTasksForJob job tasks = dStack $loc "startTasksForJob" $ dPutMVar $loc (readyTasks job) tasks

-- | Waits for every job in the list to complete, that is, it waits until every 'jobCompletion'
-- 'Control.Concurrent.DQSem.DQSem' has been signalled.
waitForJobs :: [Job] -> Run ()
waitForJobs jobx = dStack $loc "waitForJobs" $ forM_ jobx (dWaitQSem $loc . jobCompletion)

-- If you created a 'Dao.Types.Job' using 'newJob', that 'Dao.Types.Job' is automatically inserted
-- into the 'Dao.Types.jobTable' of the 'Dao.Types.Runtime' of this 'Run' monad. To remove it, from
-- the table, use this function. The 'Job' is uniqely identified by it's 'Dao.Types.jobTaskThread'
-- 'Control.Concurrent.ThreadId'.
removeJobFromTable :: Job -> Run ()
removeJobFromTable job = ask >>= \runtime ->
  dModifyMVar_ $loc (jobTable runtime) $ \jtab -> return (M.delete (jobTaskThread job) jtab)

----------------------------------------------------------------------------------------------------

-- | Given an input string, and a program, return all patterns and associated match results and
-- actions that matched the input string, but do not execute the actions. This is done by tokenizing
-- the input string and matching the tokens to the program using 'Dao.Pattern.matchTree'.
matchStringToProgram :: UStr -> CachedProgram -> Run [(Pattern, Match, CachedAction)]
matchStringToProgram instr program = dStack $loc "matchStringToProgram" $ do
  tree <- trace "(ruleSet program)" $ dReadMVar $loc (ruleSet program)
  let eq = (==) -- TODO: the matching predicate needs to be retrieved from a declaration in the program.
      tox = tokens (uchars instr) -- TODO: tokenizing needs to be retrieved from a declaration in the program.
      matched = trace "(matchTree...)" $ matchTree eq (trace "(matchTree (ruleSet program))" $ tree) tox
  lift (traceIO ("(matched "++show (length matched)++" rules)"))
  fmap concat $ forM (trace "(forM matchTree (ruleSet program))" matched) $ \ (patn, mtch, cxrefx) -> do
    forM cxrefx $ \cxref -> do
      trace "(take ExecScript from CXRef)" $ dModifyMVar_ $loc cxref $ \cx -> return $ case cx of
        OnlyCache m -> cx
        HasBoth _ m -> cx
        OnlyAST ast ->
          HasBoth
          { sourceScript = ast
          , cachedScript = nestedExecStack M.empty (execScriptBlock ast)
          } -- This may be unnecessary, the cached function is stored in 'execInputString' as well.
      return (patn, mtch, cxref)

-- | Given a list of 'Program's and an input string, generate a set of 'Task's to be executed in a
-- 'Job'. The 'Task's are selected according to the input string, which is tokenized and matched
-- against every rule in the 'Program' according to the 'Dao.Pattern.matchTree' equation.
makeTasksForInput :: [ExecUnit] -> UStr -> Run [Task]
makeTasksForInput xunits instr = dStack $loc "makeTasksForInput" $ fmap concat $ forM xunits $ \xunit -> do
  let name    = currentProgram xunit >>= Just . programModuleName
      program = flip fromMaybe (currentProgram xunit) $
        error "execInputString: currentProgram of execution unit is not defined"
  dMessage $loc ("(match string to "++show (length xunits)++" running programs)")
  matched <- matchStringToProgram instr program
  dMessage $loc ("(construct RuleTasks with "++show (length matched)++" matched rules)")
  forM matched $ \ (patn, mtch, action) -> return $
    RuleTask
    { taskPattern     = OPattern patn
    , taskMatch       = mtch
    , taskAction      = action
    , taskExecUnit    = xunit
    }

-- | A "guard script" is any block of code in the source script denoted by the @BEGIN@, @END@,
-- @SETUP@ and @TAKEDOWN@ keywords. These scripts must be run in separate phases, that is, every
-- guard script must be fully executed or be timed-out before any other scripts are executed.
-- This function creates the 'Task's that for any given guard script: 'Dao.Types.preExecScript',
-- 'Dao.Types.postExecScript'.
makeTasksForGuardScript
  :: (CachedProgram -> [CXRef [Com ScriptExpr] (ExecScript ())])
  -> [ExecUnit]
  -> Run [Task]
makeTasksForGuardScript select xunits = dStack $loc "makeTasksForGuardScript" $ lift $ fmap concat $ forM xunits $ \xunit -> do
  let cachedProg = currentProgram xunit
  case fmap select cachedProg of
    Nothing    -> return []
    Just progx -> return $ flip map progx $ \prog ->
      GuardTask
      { taskGuardAction = prog
      , taskExecUnit    = xunit
      }

-- | When executing strings against Dao programs (e.g. using 'execInputString'), you often want to
-- execute the string against only a subset of the number of total programs. Pass the logical names
-- of every module you want to execute strings against, and this function will return them. If you
-- pass an empty list, all 'PublicType' modules (in the 'programs' table of the 'Runtime') will be
-- returned. Pass @'Data.Maybe.Just' 'Dao.Evaluator.ExecUnit'@ to allow 'PrivateType'
-- functions to also be selected, however only modules imported by the program associated with that
-- 'ExecUnit' are allowed to be selected.
selectModules :: Maybe ExecUnit -> [Name] -> Run [File]
selectModules xunit names = dStack $loc "selectModules" $ ask >>= \runtime -> case names of
  []    -> do
    ax <- dReadMVar $loc (logicalNameIndex runtime)
    dMessage $loc ("selected modules:\n"++unlines (map show (M.keys ax)))
    (return . filter (\file -> isProgramFile file && publicFile file) . M.elems) ax
  names -> do
    pathTab <- dReadMVar $loc (pathIndex runtime)
    let set               = M.fromList . map (\mod -> (mod, undefined))
        request           = set names
        (public, private) = M.partition publicFile (M.filter isProgramFile pathTab)
    imports <- case xunit of
      Nothing    -> return M.empty
      Just xunit -> return $
        set $ concat $ maybeToList $ fmap programImports $ currentProgram xunit
    ax <- return $ M.union (M.intersection public request) $
      M.intersection private (M.intersection imports request)
    dMessage $loc ("selected modules:\n"++unlines (map show (M.keys ax)))
    return $ M.elems ax

-- | This is the "heart" of the Dao system; it is the algorithm you wanted to use when you decided
-- to install the Dao system. Select from the 'Dao.Types.Runtime's 'modules' table a list of Dao
-- programs using 'selectModules'. Once the list of modules is selected, for each module tokenize
-- the input string, then select all rules in the module matching the input. Create a list of
-- 'Task's to run using 'makeTasksForInput' and execute them in a new 'Job' created by 'newJob'.
-- Before and after this happens, the "guard scripts" (@BEGIN@ and @END@ rules written into each
-- module) will be executed as a separate set of tasks. This function must wait for all tasks to
-- finish, each phase of execution (run @BEGIN@, run matching rules, run @END@) must be executed to
-- completion before the next phase can be run. Waiting for the 'Job' to complete (using
-- 'waitForJobs') is performed in the same thread that evaluates this function, so this function
-- will block until execution is completed.
execInputString :: Bool -> UStr -> [File] -> Run ()
execInputString guarded instr select = dStack $loc "execInputString" $ ask >>= \runtime -> do
  xunits <- forM (filter isProgramFile select) (dReadMVar $loc . execUnit)
  unless (null xunits) $ do
    dMessage $loc ("selected "++show (length xunits)++" modules")
    -- Create a new 'Job' for this input string, 'newJob' will automatically place it into the
    -- 'Runtime's job table.
    job <- newJob (defaultTimeout runtime) instr
    let run fn = fn >>= \tasks -> case tasks of
          []    -> return ()
          tasks -> do
            dMessage $loc (show (length tasks)++" tasks created")
            startTasksForJob job tasks >> waitForJobs [job]
        exception (SomeException e) = removeJobFromTable job >> dThrowIO e
    -- here begins the three phases of executing a string:
    dHandle $loc exception $ do
      -- (1) run all 'preExecScript' actions as a task, wait for all tasks to complete
      when guarded $ do
        dMessage $loc "pre-string-execution"
        run $ makeTasksForGuardScript preExecScript xunits
      -- (2) run each actions for each rules that matches the input, wait for all tasks to complete
      dMessage $loc "execute string"
      run $ makeTasksForInput xunits instr
      -- (3) run all 'postExecScript' actions as a task, wait for all tasks to complete.
      when guarded $ do
        dMessage $loc "post-string-execution"
        run $ makeTasksForGuardScript postExecScript xunits
    removeJobFromTable job

-- | In the current thread, and using the given 'Runtime' environment, parse an input string as
-- 'Dao.Types.Script' and then evaluate it. This is used for interactive evaluation. The parser
-- used in this function will parse a block of Dao source code, the opening and closing curly-braces
-- are not necessary. Therefore you may enter a semi-colon separated list of commands and all will
-- be executed.
evalScriptString :: ExecUnit -> String -> Run ()
evalScriptString xunit instr = dStack $loc "evalScriptString" $
  void $ flip runExecScript xunit $ nestedExecStack M.empty $ execScriptBlock $
    case map fst $ filter (null . inputString . snd) $
      runCombination interactiveScript (parserState{inputString=instr}) of
        []               -> error "cannot parse expression"
        (Left  msg  : _) -> error (showObj 0 msg)
        (Right expr : _) -> Com expr

