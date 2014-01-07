-- "src/Dao.hs"  the smallest interface that can be imported by any
-- Haskell program that makes use of the Dao System by way of linking
-- to the modules in the dao package.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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

-- | This module is pretty much where everything begins. It is the smallest interface that can be
-- imported by any Haskell program making use of the Dao System. You can use the functions in this
-- module to initialize a 'Dao.Object.Runtime' data structure, then use it to start an input query
-- loop with 'inputQueryLoop'. The query loop requires you pass a callback function that, on each
-- evaluation, returns the next string to be used the query to the 'Dao.Object.Runtime'.
--
-- To have more control over execution of string queries, you will need to import the "Dao.Tasks"
-- module and make use of those functions to create 'Dao.Object.Job's from string queries, then wait
-- for those 'Dao.Object.Job's to complete.
module Dao
  ( module Dao.String
  , module Dao.Object
  , module Dao.Evaluator
  , module Dao
  ) where

import           Dao.String
import qualified Dao.Tree as T
import           Dao.Glob
import           Dao.Object
import           Dao.Predicate
import           Dao.Evaluator
import           Dao.PPrint
import           Dao.Token
import           Dao.Parser
import           Dao.Object.Parser

import           Data.Function
import           Data.Monoid
import           Data.List (nub)
import           Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class

----------------------------------------------------------------------------------------------------

-- | The minimum amount of time allowable for a single input string to execute before timing out.
-- Any time smaller than this ammount, and it may not be possible to execute anything before it
-- times out, so if you are setting a timeout limit, make sure it is as large as what you have
-- patience for.
min_exec_time :: Int
min_exec_time = 200000

singleThreaded :: [UStr] -> Exec ()
singleThreaded args = do
  deps   <- importFullDepGraph args
  mapM_ loadModule (getDepFiles deps)

----------------------------------------------------------------------------------------------------

-- | Simply converts an 'Dao.Object.AST.AST_SourceCode' directly to a list of
-- 'Dao.Object.TopLevelExpr's.
evalTopLevelAST :: AST_SourceCode -> Exec Program
evalTopLevelAST ast = case toInterm ast of
  [o] -> return o
  []  -> fail "converting AST_SourceCode to Program by 'toInterm' returned null value"
  _   -> fail "convertnig AST_SourceCode to Program by 'toInterm' returned ambiguous value"

-- Called by 'loadModHeader' and 'loadModule', throws a Dao exception if the source file could not
-- be parsed.
loadModParseFailed :: Maybe UPath -> DaoParseErr -> Exec ig
loadModParseFailed path err = execThrow $ obj $ concat $
  [ maybe [] (\path -> [obj $ uchars path ++ maybe "" show (parseErrLoc err)]) path
  , maybe [] (return . obj) (parseErrMsg err)
  , maybe [] (\tok -> [obj "on token", obj (show tok)]) (parseErrTok err)
  ]

-- | Load only the "require" and "import" statements from a Dao script file at the given filesystem
-- path. Called by 'importDepGraph'.
loadModHeader :: UPath -> Exec [(Name, Object, Location)]
loadModHeader path = do
  text <- liftIO (readFile (uchars path))
  case parse daoGrammar mempty text of
    OK    ast -> do
      let attribs = takeWhile isAST_Attribute (directives ast) >>= attributeToList
      forM attribs $ \ (attrib, astobj, loc) -> case toInterm (unComment astobj) of
        []  -> execThrow $ obj $
          [obj ("bad "++uchars attrib++" statement"), obj (prettyShow astobj)]
        o:_ -> do
          o <- execute o
          case o of
            Nothing -> execThrow $ obj $
              [ obj $ "parameter to "++uchars attrib++" evaluated to void"
              , obj (prettyShow astobj)
              ]
            Just  o -> return (attrib, o, loc)
    Backtrack -> execThrow $ obj $
      [obj path, obj "does not appear to be a valid Dao source file"]
    PFail err -> loadModParseFailed (Just path) err

-- | Lookup an 'Dao.Evaluator.ExecUnit' used by the current module by it's file path 'UPath'.
childExecUnit :: Maybe UPath -> Exec ExecUnit
childExecUnit path = liftIO (initExecUnit path)

-- | Creates a child 'ExecUnit' for the current 'ExecUnit' and populates it with data by parsing and
-- evaluating the contents of a Dao script file at the given filesystem path.
loadModule :: UPath -> Exec ExecUnit
loadModule path = do
  text <- liftIO (readFile (uchars path))
  case parse daoGrammar mempty text of
    OK    ast -> deepseq ast $! do
      mod <- childExecUnit (Just path)
      local (const mod) (evalTopLevelAST ast >>= execute) -- updates and returns 'mod' 
    Backtrack -> execThrow $ obj [obj path, obj "does not appear to be a valid Dao source file"]
    PFail err -> loadModParseFailed (Just path) err

-- | Takes a non-dereferenced 'Dao.Object.Object' expression which was returned by 'execute'
-- and converts it to a file path. This is how "import" statements in Dao scripts are evaluated.
-- This function is called by 'importDepGraph', and 'importFullDepGraph'.
objectToImport :: UPath -> Object -> Location -> Exec [UPath]
objectToImport file o lc = case o of
  OString str -> return [str]
  o           -> execThrow $ obj $ 
    [ obj (uchars file ++ show lc)
    , obj "contains import expression evaluating to an object that is not a file path", o
    ]

objectToRequirement :: UPath -> Object -> Location -> Exec UStr
objectToRequirement file o lc = case o of
  OString str -> return str
  o           -> execThrow $ obj $ 
    [ obj (uchars file ++ show lc)
    , obj "contains import expression evaluating to an object that is not a file path", o
    ]

type DepGraph = M.Map UPath [UPath]

getDepFiles :: DepGraph -> [UPath]
getDepFiles = M.keys

-- | Calls 'loadModHeader' for several filesystem paths, creates a dependency graph for every import
-- statement. This function is not recursive, it only gets the imports for the paths listed. It
-- takes an existing 'DepGraph' of any files that have already checked so they are not checked
-- again. The returned 'DepGraph' will contain only the lists of imports for files in the given list
-- of file paths that are not already in the given 'DepGraph', so you may need to
-- 'Data.Monoid.mappend' the returned 'DepGraph's to the one given as a parameter. If the returned
-- 'DepGraph' is 'Data.Monoid.mempty', there is no more work to be done.
importDepGraph :: DepGraph -> [UPath] -> Exec DepGraph
importDepGraph graph files = do
  let isImport  attrib = attrib == ustr "import"  || attrib == ustr "imports"
  -- let isRequire attrib = attrib == ustr "require" || attrib == ustr "requires"
  fhdrs <- forM files (\file -> loadModHeader file >>= \hdrs -> return (file, hdrs))
  fmap mconcat $ forM fhdrs $ \ (file, attribs) ->
    if M.member file graph
      then  return mempty
      else  do
        imports <- fmap mconcat $ forM attribs $ \ (attrib, o, lc) ->
          if isImport attrib
            then objectToImport file o lc
            else return mempty
        return (M.singleton file imports)

-- | Recursively 'importDepGraph' until the full dependency graph is generated.
importFullDepGraph :: [UPath] -> Exec DepGraph
importFullDepGraph = loop mempty where
  loop graph files = importDepGraph graph files >>= \newGraph ->
    if M.null newGraph then return graph else loop (mappend graph newGraph) (M.keys newGraph)

----------------------------------------------------------------------------------------------------

-- | Blocks until every thread in the given 'Dao.Object.Task' completes evaluation.
taskWaitThreadLoop :: Task -> Exec ()
taskWaitThreadLoop task = do
  threads <- liftIO $ readMVar (taskRunningThreads task)
  if S.null threads then return () else liftIO loop
  where 
    loop = do
      thread <- takeMVar (taskWaitMVar task)
      isDone <- modifyMVar (taskRunningThreads task) $ \threads_ -> do
        let threads = S.delete thread threads_
        return (threads, S.null threads)
      if isDone then return () else loop

-- | Registers the threads created by 'runStringAgainstExecUnits' into the
-- 'Dao.Object.runningExecThreads' field of the 'Dao.Object.Runtime'.
taskRegisterThreads :: Task -> Exec [ThreadId] -> Exec ()
taskRegisterThreads task makeThreads =
  execModifyRef_ (taskRunningThreads task) $ \threads ->
    (flip S.union threads . S.fromList) <$> makeThreads
    -- TODO: create versions of modifyMVar which work in the Exec monad.

-- | Signal to the 'Dao.Object.Task' that the task has completed. This must be the last thing a
-- thread does if it is registered into a 'Dao.Object.Task' using 'taskRegisterThreads'.
completedThreadInTask :: Task -> Exec ()
completedThreadInTask task = liftIO (myThreadId >>= putMVar (taskWaitMVar task))

-- | Evaluate an 'Dao.Object.Action' in the current thread.
execAction :: ExecUnit -> Action -> Exec (Maybe Object)
execAction xunit act = local (const xunit) $
  runCodeBlock (fmap (obj . fmap obj) $ actionMatch act) (actionCodeBlock act)

-- | Create a new thread and evaluate an 'Dao.Object.Action' in that thread. This thread is defined
-- such that when it completes, regardless of whether or not an exception occurred, it signals
-- completion to the 'Dao.Object.waitForActions' 'Dao.Debug.DMVar' of the 'Dao.Object.ExecUnit'
-- associated with this 'Dao.Object.Action'.
forkExecAction :: ExecUnit -> Action -> Exec ThreadId
forkExecAction xunit act = liftIO $ forkIO $ void $ flip ioExec xunit $ do
  execCatchIO (void $ execAction xunit act) [execErrorHandler, execIOHandler]
  completedThreadInTask (taskForActions xunit)

-- | For every 'Dao.Object.Action' in the 'Dao.Object.ActionGroup', evaluate that
-- 'Dao.Object.Action' in a new thread in the 'Task' associated with the 'Dao.Object.ExecUnit' of
-- the 'Dao.Object.ActionGroup'.
forkActionGroup :: ActionGroup -> Exec ()
forkActionGroup actgrp = do
  let xunit = actionExecUnit actgrp
      task  = taskForActions xunit
  taskRegisterThreads task (forM (getActionList actgrp) (forkExecAction (actionExecUnit actgrp)))
  taskWaitThreadLoop task >>= liftIO . evaluate

-- | For every 'Dao.Object.Action' in the 'Dao.Object.ActionGroup', evaluate that
-- 'Dao.Object.Action' in a the current thread but in using the 'Dao.Object.ExecUnit' of the
-- given 'Dao.Object.ActionGroup'.
execActionGroup :: ActionGroup -> Exec ()
execActionGroup actgrp = mapM_ (execAction (actionExecUnit actgrp)) (getActionList actgrp)

-- | This is the most important algorithm of the Dao system, in that it matches strings to all
-- rule-patterns in the program that is associated with the 'Dao.Object.ExecUnit', and it dispatches
-- execution of the rule-actions associated with matched patterns. This is the function that runs in
-- the thread which manages all the other threads that are launched in response to a matching input
-- string. You could just run this loop in the current thread, if you only have one 'ExecUnit'.
execInputStringsLoop :: ExecUnit -> Exec ()
execInputStringsLoop xunit = do
  execCatchIO loop [execIOHandler]
  completedThreadInTask (taskForActions xunit)
  where
    loop = do
      --instr <- dModifyMVar xloc (recursiveInput xunit) $ \ax -> return $ case nub ax of
      instr <- liftIO $ atomicModifyIORef (recursiveInput xunit) $ \ax -> case nub ax of
        []   -> ([], Nothing)
        a:ax -> (ax, Just a)
      case instr of
        Nothing    -> return ()
        Just instr -> waitAll (makeActionsForQuery instr xunit) >>= liftIO . evaluate >> loop

waitAll :: Exec ActionGroup -> Exec ()
waitAll getActionGroup = getActionGroup >>= forkActionGroup

-- | Given an input string, and a program, return all patterns and associated match results and
-- actions that matched the input string, but do not execute the actions. This is done by tokenizing
-- the input string and matching the tokens to the program using 'Dao.Glob.matchTree'.
-- NOTE: Rules that have multiple patterns may execute more than once if the input matches more than
-- one of the patterns associated with the rule. *This is not a bug.* Each pattern may produce a
-- different set of match results, it is up to the programmer of the rule to handle situations where
-- the action may execute many times for a single input.
makeActionsForQuery :: UStr -> ExecUnit -> Exec ActionGroup
makeActionsForQuery instr xunit = do
  --tokenizer <- asks programTokenizer
  --tokenizer instr >>= match -- TODO: put the customizable tokenizer back in place
  match (map toUStr $ words $ fromUStr instr)
  where
    match tox = do
      --tree <- dReadMVar xloc (ruleSet xunit)
      tree <- liftIO $ readIORef (ruleSet xunit)
      return $
        ActionGroup
        { actionExecUnit = xunit
        , getActionList = flip concatMap (matchTree False tree tox) $ \ (patn, mtch, execs) ->
            flip map execs $ \exec -> seq exec $! seq instr $! seq patn $! seq mtch $!
              Action
              { actionQuery      = Just instr
              , actionPattern    = Just patn
              , actionMatch      = mtch
              , actionCodeBlock  = exec
              }
        }

-- | Create a list of 'Dao.Object.Action's for every BEGIN or END statement in the Dao program. Pass
-- 'Dao.Object.preExec' as the first parameter to get the BEGIN scrpits, pass 'Dao.Object.postExec'
-- to get the END scripts.
getBeginEndScripts :: (ExecUnit -> [Subroutine]) -> ExecUnit -> ActionGroup
getBeginEndScripts select xunit =
  ActionGroup
  { actionExecUnit = xunit
  , getActionList  = flip map (select xunit) $ \exe ->
      Action
      { actionQuery      = Nothing
      , actionPattern    = Nothing
      , actionMatch      = T.Void
      , actionCodeBlock  = exe
      }
  }

-- | For each given execution unit, place the input string into the 'Dao.Object.recursiveInput'
-- queue of that 'Dao.Object.ExecUnit' and then start a thread running 'execInputStrinsLoop' for
-- that 'Dao.Object.ExecUnit'. This function evaluates syncrhonously, that is, you must wait for all
-- threads created by this string query to complete before this function evaluation returns.
-- The threads are created and registered into the 'Dao.Object.runningExecThreads' field of the
-- 'Dao.Object.Runtime' using 'taskRegisterThreads'. Then, 'taskWaitThreadLoop' is called to wait
-- for the string execution to complete.
runStringQuery :: UStr -> [ExecUnit] -> Exec ()
runStringQuery inputString xunits = do
  task <- asks taskForExecUnits
  taskRegisterThreads task $ do
    forM xunits $ \xunit -> do
      liftIO $ modifyIORef (recursiveInput xunit) (++[inputString])
      liftIO $ forkIO $ void $ flip ioExec xunit $ do
        execHandleIO [execErrorHandler, execIOHandler] $ do
            waitAll (return (getBeginEndScripts preExec xunit)) >>= liftIO . evaluate
            execInputStringsLoop xunit >>= liftIO . evaluate -- Exec RULES and PATTERNS
            waitAll (return (getBeginEndScripts postExec xunit)) >>= liftIO . evaluate
        completedThreadInTask task
  taskWaitThreadLoop task

-- | Clears any string queries waiting in the 'Dao.Object.recurisveInput' of an
-- 'Dao.Object.ExecUnit'.
clearStringQueries :: ExecUnit -> Exec ()
clearStringQueries xunit = --dModifyMVar_ xloc (recursiveInput xunit) (\_ -> return [])
  liftIO $ modifyIORef (recursiveInput xunit) (const [])

-- | This is the main input loop. Pass an input function callback to be called on every loop.
daoInputLoop :: (Exec (Maybe UStr)) -> Exec ()
daoInputLoop getString = fix $ \loop -> do
  inputString <- getString
  case inputString of
    Nothing          -> return ()
    Just inputString -> do
      xunits <- asks pathIndex >>= liftIO . fmap M.elems . readMVar
      mapM_ clearStringQueries xunits
      runStringQuery inputString xunits
      loop

-- | Evaluates the @EXIT@ scripts for every presently loaded dao program, and then clears the
-- 'Dao.Object.pathIndex', effectively removing every loaded dao program and idea file from memory.
daoShutdown :: Exec ()
daoShutdown = do
  idx <- asks pathIndex
  liftIO $ modifyMVar_ idx $ (\_ -> return (M.empty))
  xunits <- liftIO $ fmap M.elems (readMVar idx)
  forM_ xunits $ \xunit -> local (const xunit) $ asks quittingTime >>= mapM_ execute

-- | When executing strings against Dao programs (e.g. using 'Dao.Tasks.execInputString'), you often
-- want to execute the string against only a subset of the number of total programs. Pass the
-- logical names of every module you want to execute strings against, and this function will return
-- them.
selectModules :: [Name] -> Exec [ExecUnit]
selectModules names = do
  xunit <- ask
  ax <- case names of
    []    -> liftIO $ readMVar (pathIndex xunit)
    names -> do
      pathTab <- liftIO $ readMVar (pathIndex xunit)
      let set msg           = M.fromList . map (\mod -> (toUStr mod, error msg))
          request           = set "(selectModules: request files)" names
      return (M.intersection pathTab request)
  return (M.elems ax)

-- | In the current thread parse an input string as 'Dao.Object.Script' and then evaluate it. This
-- is used for interactive evaluation. The parser used in this function will parse a block of Dao
-- source code, the opening and closing curly-braces are not necessary. Therefore you may enter a
-- semi-colon separated list of commands and all will be executed.
evalScriptString :: String -> Exec ()
evalScriptString instr =
  void $ execNested T.Void $ mapM_ execute $
    case parse (daoGrammar{mainParser = concat <$> (many script <|> return [])}) mempty instr of
      Backtrack -> error "cannot parse expression"
      PFail tok -> error ("error: "++show tok)
      OK   expr -> concatMap toInterm expr

-- | This is the simplest form of string execution, everything happens in the current thread, no
-- "BEGIN" or "END" scripts are executed. Simply specify a list of programs (as file paths) and a
-- list of strings to be executed against each program.
execStringsAgainst :: [Name] -> [UStr] -> Exec ()
execStringsAgainst selectPrograms execStrings = do
  otherXUnits <- selectModules selectPrograms
  forM_ otherXUnits $ \xunit ->
    forM_ execStrings $ \execString ->
      makeActionsForQuery execString xunit >>= execActionGroup

