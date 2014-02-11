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
-- module to initialize a 'Dao.Interpreter.Runtime' data structure, then use it to start an input query
-- loop with 'inputQueryLoop'. The query loop requires you pass a callback function that, on each
-- evaluation, returns the next string to be used the query to the 'Dao.Interpreter.Runtime'.
--
-- To have more control over execution of string queries, you will need to import the "Dao.Tasks"
-- module and make use of those functions to create 'Dao.Interpreter.Job's from string queries, then wait
-- for those 'Dao.Interpreter.Job's to complete.
module Dao
  ( module Dao.String
  , module Dao.Interpreter
  , module Dao
  ) where

import           Dao.String
import           Dao.Glob
import           Dao.Interpreter
import           Dao.Predicate
import           Dao.PPrint
import           Dao.Token
import           Dao.Parser
import           Dao.Interpreter.Parser
import qualified Dao.Tree as T

import           Data.Function
import           Data.Monoid
import           Data.IORef
import qualified Data.Map as M

import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class

import           System.IO

----------------------------------------------------------------------------------------------------

-- | The minimum amount of time allowable for a single input string to execute before timing out.
-- Any time smaller than this ammount, and it may not be possible to execute anything before it
-- times out, so if you are setting a timeout limit, make sure it is as large as what you have
-- patience for.
min_exec_time :: Int
min_exec_time = 200000

-- | Evaluate this function as one of the instructions in the monadic function passed to the
-- 'setupDao' function in order to install the most fundamental functions into the Dao evaluator.
-- This function must be evaluated in order to have access to the following functions:
-- > exec, prove
daoFuncs :: DaoSetup
daoFuncs = do
  daoClass "RuleSet" (haskellType :: PatternTree Object [Subroutine])
  let f x = daoFunc{ autoDerefParams=True, daoForeignCall=x }
  daoFunction "do"          $ f queryDo
  daoFunction "doAll"       $ f queryDoAll
  daoFunction "doLocal"     $ f queryDoLocal
  daoFunction "doLocalAll"  $ f queryDoLocalAll
  daoFunction "doGlobal"    $ f queryDoGlobal
  daoFunction "doGlobalAll" $ f queryDoGlobalAll
  daoFunction "doIn"        $ f queryDoIn
  daoFunction "doInAll"     $ f queryDoAllIn

----------------------------------------------------------------------------------------------------

-- | An 'Action' is the result of a pattern match that occurs during an input string query. It is a
-- data structure that contains all the information necessary to run an 'Subroutine' assocaited with
-- a 'Glob', including the parent 'ExecUnit', the 'Dao.Glob.Glob' and the 'Dao.Glob.Match' objects,
-- and the 'Executables'. Use 'Dao.Evaluator.execute' to evaluate a 'Dao.Action' in the current
-- thread.
-- 
-- To execute an action in a separate thread, use 'forkExecAction'.
data Action
  = Action
    { actionQuery     :: Maybe UStr
    , actionPattern   :: Maybe (Glob Object)
    , actionMatch     :: M.Map Name [Object]
    , actionCodeBlock :: Subroutine
    }

instance Executable Action (Maybe Object) where
  execute act = local setup $ runCodeBlock localVars (actionCodeBlock act) where
    setThisVar = maybe id (M.union . (M.singleton (ustr "this") . obj)) (actionQuery act)
    localVars  = setThisVar $ fmap (obj . fmap obj) (actionMatch act)
    setup xunit =
      xunit
      { currentQuery     = actionQuery act
      , currentPattern   = actionPattern act
      , currentCodeBlock = StaticStore (Just $ actionCodeBlock act)
      }

-- | An 'ActionGroup' is a group of 'Action's created within a given 'ExecUnit', this data structure
-- contains both the list of 'Action's and the 'ExecUnit' from which the actions were generated. The
-- 'Action's within the group will all be evaluated inside of the 'ExecUnit'. Use
-- 'Dao.Evaluator.execute' to execute an 'ActionGroup' in the current thread.
-- 
-- Instantiates 'Executable' such that for every 'Dao.Interpreter.Action' in the
-- 'Dao.Interpreter.ActionGroup', evaluate that 'Dao.Interpreter.Action' in a the current thread but in using
-- the 'Dao.Interpreter.ExecUnit' of the given 'Dao.Interpreter.ActionGroup'.
data ActionGroup
  = ActionGroup
    { actionExecUnit :: ExecUnit
    , getActionList  :: [Action]
    }

instance Executable ActionGroup [Object] where
  execute o = local (const (actionExecUnit o)) $ do
    xunit <- ask
    mapM_ execute $ preExec xunit
    result <- mapM (catchPredicate . execute) $ getActionList o
    mapM_ execute $ postExec xunit
    let f p = case p of { OK (Just o) -> [o]; _ -> [] }
    return $ result>>=f

----------------------------------------------------------------------------------------------------

type DepGraph = M.Map UPath [UPath]

getDepFiles :: DepGraph -> [UPath]
getDepFiles = M.keys

loadEveryModule :: [UPath] -> Exec ()
loadEveryModule args = do
  deps <- importFullDepGraph args
  mapM_ loadModule (getDepFiles deps)

-- | Simply converts an 'Dao.Interpreter.AST_SourceCode' directly to a list of
-- 'Dao.Interpreter.TopLevelExpr's.
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

-- | Creates a child 'ExecUnit' for the current 'ExecUnit' and populates it with data by parsing and
-- evaluating the contents of a Dao script file at the given filesystem path. If the module at the
-- given path has already been loaded, the already loaded module 'ExecUnit' is returned.
loadModule :: UPath -> Exec ExecUnit
loadModule path = do
  xunit <- ask
  mod   <- fmap (M.lookup path) (asks importGraph >>= liftIO . readMVar)
  case mod of
    Just mod -> return mod
    Nothing  ->  do
      text <- liftIO (readFile (uchars path))
      case parse daoGrammar mempty text of
        OK    ast -> deepseq ast $! do
          mod <- newExecUnit (Just path)
          mod <- local (const mod) (evalTopLevelAST ast >>= execute) -- modifeis and returns 'mod' 
          liftIO $ modifyMVar_ (pathIndex xunit) (return . M.insert path mod)
          return mod
        Backtrack -> execThrow $ obj [obj path, obj "does not appear to be a valid Dao source file"]
        PFail err -> loadModParseFailed (Just path) err

-- | Takes a non-dereferenced 'Dao.Interpreter.Object' expression which was returned by 'execute'
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

-- | This is the main input loop. Pass an input function callback to be called on every loop. This
-- function should return strings to be evaluated, and return 'Data.Maybe.Nothing' to signal that
-- this loop should break.
daoInputLoop :: Exec (Maybe UStr) -> Exec ()
daoInputLoop getString = fix $ \loop -> do
  inputString <- getString
  case inputString of
    Nothing          -> return ()
    Just inputString -> execStringQuery inputString >> loop

-- | Match a given input string to the 'Dao.Evaluator.currentPattern' of the current 'ExecUnit'.
-- Return all patterns and associated match results and actions that matched the input string, but
-- do not execute the actions. This is done by tokenizing the input string and matching the tokens
-- to the program using 'Dao.Glob.matchTree'. NOTE: Rules that have multiple patterns may execute
-- more than once if the input matches more than one of the patterns associated with the rule. *This
-- is not a bug.* Each pattern may produce a different set of match results, it is up to the
-- programmer of the rule to handle situations where the action may execute many times for a single
-- input.
-- 
-- Once you have created an action group, you can execute it with 'Dao.Evaluator.execute'.
makeActionsForQuery :: [PatternTree Object [Subroutine]] -> UStr -> Exec ActionGroup
makeActionsForQuery tree instr = do
  -- TODO: need to make use of the programTokenizer set in the current 'ExecUnit'
  match (map (OString . toUStr) $ simpleTokenizer $ fromUStr instr)
  where
    match tox = ask >>= \xunit -> return $
      ActionGroup
      { actionExecUnit = xunit
      , getActionList  = flip concatMap (matchTree False (T.unionsWith (++) tree) tox) $ \ (patn, mtch, execs) ->
          flip map execs $ \exec -> seq exec $! seq instr $! seq patn $! seq mtch $!
            Action
            { actionQuery      = Just instr
            , actionPattern    = Just patn
            , actionMatch      = mtch
            , actionCodeBlock  = exec
            }
      }

-- Checks the list of parameters, and if there are more than one, checks if the first parameter is
-- an 'OHaskell' object which can be converted to 'CallableCode' using 'objToCallable'. If so, the
-- callables are returned and the results of pattern matching can be used as parameters to these
-- functions.
_getFuncStringParams :: [Object] -> Exec (Maybe [CallableCode], [UStr])
_getFuncStringParams ox = case ox of
  []   -> fail "no parameters passed to function"
  [o]  -> (,) Nothing <$> oneOrMoreStrings [o]
  o:lst -> do
    calls <- (Just <$> objToCallable o) <|> return Nothing
    (,) calls <$> oneOrMoreStrings lst
  where
    oneOrMoreStrings ox = case concatMap extractStringElems ox of
      [] -> fail "parameter arguments contain no string values"
      ox -> return ox

_getRuleSetParam :: [Object] -> Exec (Maybe (PatternTree Object [Subroutine]), [Object])
_getRuleSetParam ox = return $ case ox of
  []   -> (Nothing, [])
  o:ox -> maybe ((Nothing, o:ox)) (\o -> (Just o, ox)) (fromObj o)

-- Match the strings, collect the actions to evaluate, pass a list 'Exec' functions that are the
-- actions to execute to the given handler function. Use the boolean parameters to declare whether
-- or not the rule set supplied as the first parameter (if at all) should be used, whether or not
-- locally defined rules should be used, and whether or not globally defined rules should be used,
-- respectively.
_queryFunc
  :: Bool -> Bool -> Bool
  -> ([Exec [Object]] -> Exec b)
  -> [Object] -> Exec b
_queryFunc useArgs useLocal useGlobal strLoop ox = do
  (rulset,  ox) <- _getRuleSetParam ox
  (calls, strs) <- _getFuncStringParams ox
  rulset <- fmap concat $ sequence $
    [ maybe (return []) (\o -> return $ if useArgs then [o] else []) rulset
    , if useGlobal then asks ruleSet >>= liftIO . fmap (:[]) . readIORef else return []
    , if useLocal
      then do 
        (StaticStore o) <- asks currentCodeBlock
        maybe (return []) (liftIO . fmap (:[]) . readIORef . staticRules) o
      else return []
    ]
  if and [null rulset, useArgs, not useLocal, not useGlobal]
  then fail "expecting first argument to contain a ruleset"
  else return ()
  let call :: [Object] -> Exec [Object]
      call o = maybe (return o) (fmap (maybe [] (:[])) . flip callCallables o) calls
  strLoop $ flip map strs $ \str -> do
    actns <- makeActionsForQuery rulset str
    let xunit = actionExecUnit actns
    local (const xunit) $ execute actns >>= call

-- The general "do()" function, but you can customize whether or not it limits itself to rule sets
-- passed as arguments, or local rule sets, or global rule sets, or all of the above.
_queryDoFunc :: Bool -> Bool -> Bool -> [Object] -> Exec (Maybe Object)
_queryDoFunc a b c ox = _queryFunc a b c (fmap (Just . OList) . msum) ox <|> return (Just ONull)

-- The general "doAll()" function, but you can customize whether or not it limits itself to rule
-- sets passed as arguments, or local rule sets, or global rule sets, or all of the above. 
_queryDoAllFunc :: Bool -> Bool -> Bool -> [Object] -> Exec (Maybe Object)
_queryDoAllFunc a b c = _queryFunc a b c $
  fmap (Just . OList . concat . (>>=okToList)) . sequence . map catchPredicate

-- | The 'queryDo' function maps to the built-in function "do()". It will try to execute every
-- string query passed to it as a parameter, and return the return value of the first string query
-- execution that does not backtrack or fail.  Passing a callable object as the first parameter asks
-- this function to evaluate that object with the return value passed as the first parameter, and
-- the evaluation of this function may backtrack to request further string queries be tried.
-- Execution happens in the current thread. The return value is the value returned by the function
-- passed as the first parameter. This function return "null" if none of the given strings matched.
-- If the first parameter is a "ruleset" object, the string will be executed against those rules as
-- well.
queryDo :: [Object] -> Exec (Maybe Object)
queryDo = _queryDoFunc True True True

-- | The 'queryDoAll' function maps to the "doAll()" built-in function. It works like the 'queryDo'
-- function but executes as many matching patterns as possible, regardless of backtracking or
-- exceptions thrown. Returns the number of successfully matched and evaluated pattern actions.  All
-- execution happens in the current thread. Every rule action that does not fail and also returns a
-- value will have the value placed in a list and returned as the result of this function.
queryDoAll :: [Object] -> Exec (Maybe Object)
queryDoAll = _queryDoAllFunc True True True

-- | The 'queryDoLocal' function maps to the built-in function "doLocal()". It will try to execute
-- every string query passed to it as a parameter /against only locally defined rules/, and return
-- the return value of the first string query execution that does not backtrack or fail.  Passing a
-- callable object as the first parameter asks this function to evaluate that object with the return
-- value passed as the first parameter, and the evaluation of this function may backtrack to request
-- further string queries be tried.  Execution happens in the current thread. The return value is
-- the value returned by the function passed as the first parameter. This function return "null" if
-- none of the given strings matched.  If the first parameter is a "ruleset" object, the string will
-- be executed against those rules as well.
queryDoLocal :: [Object] -> Exec (Maybe Object)
queryDoLocal = _queryDoFunc True True False

-- | The 'queryDoAllLocal' function maps to the "doAllLocal()" built-in function. It works like the
-- 'queryDoLocal' function but executes as many matching patterns as possible, regardless of
-- backtracking or exceptions thrown. Returns the number of successfully matched and evaluated
-- pattern actions. All execution happens in the current thread. Every rule action that does not
-- fail and also returns a value will have the value placed in a list and returned as the result of
-- this function.
queryDoLocalAll :: [Object] -> Exec (Maybe Object)
queryDoLocalAll = _queryDoAllFunc True True False

-- | The 'queryDoGlobal' function maps to the built-in function "doGlobal()". It will try to execute
-- every string query passed to it as a parameter /against only globally defined rules/, and return
-- the return value of the first string query execution that does not backtrack or fail.  Passing a
-- callable object as the first parameter asks this function to evaluate that object with the return
-- value passed as the first parameter, and the evaluation of this function may backtrack to request
-- further string queries be tried.  Execution happens in the current thread. The return value is
-- the value returned by the function passed as the first parameter. This function return "null" if
-- none of the given strings matched.  If the first parameter is a "ruleset" object, the string will
-- be executed against those rules as well.
queryDoGlobal :: [Object] -> Exec (Maybe Object)
queryDoGlobal = _queryDoFunc True False True

-- | The 'queryDoAllGlobal' function maps to the "doAllGlobal()" built-in function. It works like
-- the 'queryDoGlobal' function but executes as many matching patterns as possible, regardless of
-- backtracking or exceptions thrown. Returns the number of successfully matched and evaluated
-- pattern actions. All execution happens in the current thread. Every rule action that does not
-- fail and also returns a value will have the value placed in a list and returned as the result of
-- this function.
queryDoGlobalAll :: [Object] -> Exec (Maybe Object)
queryDoGlobalAll = _queryDoAllFunc True False True

-- | The 'queryDoIn' function maps to the built-in function "doIn()". It will try to execute
-- every string query passed to it as a parameter /against only the rule set passed as the first
-- parameter argument/, and return the return value of the first string query execution that does
-- not backtrack or fail.  Passing a callable object as the first parameter asks this function to
-- evaluate that object with the return value passed as the first parameter, and the evaluation of
-- this function may backtrack to request further string queries be tried.  Execution happens in the
-- current thread. The return value is the value returned by the function passed as the first
-- parameter. This function return "null" if none of the given strings matched.
queryDoIn :: [Object] -> Exec (Maybe Object)
queryDoIn = _queryDoFunc True False False

-- | The 'queryDoAllIn' function maps to the "doInAll()" built-in function. It works like the
-- 'queryDoIn' function but executes as many matching patterns as possible, regardless of
-- backtracking or exceptions thrown. Returns the number of successfully matched and evaluated
-- pattern actions. All execution happens in the current thread. Every rule action that does not
-- fail and also returns a value will have the value placed in a list and returned as the result of
-- this function.
queryDoAllIn :: [Object] -> Exec (Maybe Object)
queryDoAllIn = _queryDoAllFunc True False False

-- | When executing strings against Dao programs (e.g. using 'Dao.Tasks.execInputString'), you often
-- want to execute the string against only a subset of the number of total programs. Pass the
-- logical names of every module you want to execute strings against, and this function will return
-- them.
selectModules :: [UStr] -> Exec [ExecUnit]
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

-- | Like 'execStringQueryWith', but executes against every loaded module.
execStringQuery :: UStr -> Exec ()
execStringQuery instr =
  asks pathIndex >>= liftIO . fmap M.elems . readMVar >>= execStringQueryWith instr

-- | This is the most important function in the Dao universe. It executes a string query with the
-- given module 'ExecUnit's. The string query is executed in each module, execution in each module
-- runs in it's own thread. This function is syncrhonous; it blocks until all threads finish
-- working.
execStringQueryWith :: UStr -> [ExecUnit] -> Exec ()
execStringQueryWith instr xunitList = do
  task <- asks taskForExecUnits
  liftIO $ taskLoop_ task $ flip map xunitList $ \xunit -> do
    rulset <- readIORef (ruleSet xunit)
    pdicat <- flip ioExec xunit $ makeActionsForQuery [rulset] instr >>= execute
    -- TODO: this case statement should really call into some callback functions installed into the
    -- root 'ExecUnit'.
    case pdicat of
      PFail (ExecReturn{}) -> return ()
      PFail err            -> liftIO $ hPutStrLn stderr $ prettyShow err
      Backtrack            -> case programModuleName xunit of
        Nothing   -> return ()
        Just name -> liftIO $ hPutStrLn stderr $
          uchars name ++ ": does not compute.\n\t"++show instr
      OK                _  -> return ()

-- | Runs a single line of Dao scripting language code. In the current thread parse an input string
-- of type 'Dao.Evaluator.ScriptExpr' and then evaluate it. This is used for interactive evaluation.
-- The parser used in this function will parse a block of Dao source code, the opening and closing
-- curly-braces are not necessary. Therefore you may enter a semi-colon separated list of commands
-- and all will be executed.
evalScriptString :: String -> Exec ()
evalScriptString instr =
  void $ execNested M.empty $ mapM_ execute $
    case parse (daoGrammar{mainParser = concat <$> (many script <|> return [])}) mempty instr of
      Backtrack -> error "cannot parse expression"
      PFail tok -> error ("error: "++show tok)
      OK   expr -> concatMap toInterm expr

-- | Evaluates the @EXIT@ scripts for every presently loaded dao program, and then clears the
-- 'Dao.Interpreter.pathIndex', effectively removing every loaded dao program and idea file from memory.
daoShutdown :: Exec ()
daoShutdown = do
  idx <- asks pathIndex
  liftIO $ modifyMVar_ idx $ (\_ -> return (M.empty))
  xunits <- liftIO $ fmap M.elems (readMVar idx)
  forM_ xunits $ \xunit -> local (const xunit) $ asks quittingTime >>= mapM_ execute

