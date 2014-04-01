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
import           Dao.Interpreter.AST
import qualified Dao.Tree    as T

import           Data.Function
import           Data.Monoid
import qualified Data.Map as M

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State

import           System.IO

----------------------------------------------------------------------------------------------------

-- | The minimum amount of time allowable for a single input string to execute before timing out.
-- Any time smaller than this ammount, and it may not be possible to execute anything before it
-- times out, so if you are setting a timeout limit, make sure it is as large as what you have
-- patience for.
min_exec_time :: Int
min_exec_time = 200000

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
evalTopLevelAST :: AST_SourceCode Object -> Exec (Program Object)
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
      let (requires, imports) = getRequiresAndImports (directives ast)
      forM_ requires $ \_attrib -> do -- TODO: check require statements
        return ()
      forM imports $ \ (attrib, namesp) -> case namesp of
        NamespaceExpr (Just nm) _ -> case attrib of
          AttribStringExpr str loc -> return (nm, obj str, loc)
          AttribDotNameExpr dots -> do -- TODO: resolve logical module names properly
            let path = foldl (\s t -> s++"/"++t) "." (map uchars $ dotLabelToNameList dots)++".dao"
            return (nm, obj path, getLocation dots)
        _ -> execThrow $ obj $ -- TODO: actually do something useful when a namespace is NOT specified
          [obj "import string", obj (prettyShow attrib), obj "specified without namespace"]
    Backtrack -> execThrow $ obj $
      [obj path, obj "does not appear to be a valid Dao source file"]
    PFail err -> loadModParseFailed (Just path) err

-- | Creates a child 'ExecUnit' for the current 'ExecUnit' and populates it with data by parsing and
-- evaluating the contents of a Dao script file at the given filesystem path. If the module at the
-- given path has already been loaded, the already loaded module 'ExecUnit' is returned.
loadModule :: UPath -> Exec ExecUnit
loadModule path = do
  mod <- M.lookup path <$> gets importGraph
  case mod of
    Just mod -> return mod
    Nothing  ->  do
      text <- liftIO (readFile (uchars path))
      case parse daoGrammar mempty text of
        OK    ast -> do
          mod       <- newExecUnit (Just path)
          ((), mod) <- inModule (mod{programModuleName=Just path}) $
            deepseq ast $! evalTopLevelAST ast >>= execute
          modify $ \xunit -> xunit{ importGraph = M.insert path mod (importGraph xunit) }
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

-- | An 'Action' is the result of a pattern match that occurs during an input string query. It is a
-- data structure that contains all the information necessary to run an 'Subroutine' assocaited with
-- a 'Glob', including the parent 'ExecUnit', the 'Dao.Glob.Glob' and the 'Dao.Glob.Match' objects,
-- and the 'Executables'. Use 'execute' to evaluate a 'Action' in the current thread.
-- 
-- To execute an action in a separate thread, use 'forkExecAction'.
data Action
  = Action
    { actionQuery     :: Maybe UStr
    , actionTokens    :: Maybe [Object]
    , actionPattern   :: Maybe (Glob Object)
    , actionMatch     :: M.Map Name [Object]
    , actionCodeBlock :: Subroutine
    }

instance Executable Action (Maybe Object) where
  execute act = do
    cq  <- gets currentQuery
    cp  <- gets currentPattern
    ccb <- gets currentCodeBlock
    let setVar :: ObjectClass o => String -> (Action -> Maybe o) -> T_dict -> T_dict
        setVar name f = maybe id (M.insert (ustr name) . obj) (f act)
    let setTokensVar = setVar "tokens" actionTokens
    let setQueryVar = setVar "query"  actionQuery
    let setThisVar = M.union (M.singleton (ustr "this") (obj $ setTokensVar $ setQueryVar $ mempty))
    let localVars = setThisVar $ fmap (obj . fmap obj) (actionMatch act)
    modify $ \xunit -> 
      xunit
      { currentQuery     = actionQuery act
      , currentPattern   = actionPattern act
      , currentCodeBlock = StaticStore (Just $ actionCodeBlock act)
      }
    success <- optional $ runCodeBlock localVars (actionCodeBlock act)
    modify (\xunit -> xunit{ currentQuery=cq, currentPattern=cp, currentCodeBlock=ccb })
    xmaybe success

instance Executable [Action] [Object] where
  execute = fmap concat .
    mapM (\act -> catchPredicate (execute act) >>= \p -> case p of
             OK (Just o) -> return [o]
             PFail  err  -> logUncaughtErrors [err] >> return []
             _           -> return []
         )

-- | Evaluate an executable function between evaluating all of the "BEGIN{}" and "END{}" statements.
betweenBeginAndEnd :: Exec a -> Exec a
betweenBeginAndEnd runInBetween = get >>= \xunit -> do
  -- Run all "BEGIN{}" procedures.
  mapM_ execute (preExec xunit)
  clearUncaughtErrorLog
  -- Run the given function, presumably it performs a string execution.
  a <- runInBetween
  -- Update the "global this" pointer to include the uncaught exceptions.
  errs <- (OList . map new) <$> getUncaughtErrorLog
  let ref = Dao.Interpreter.reference GLOBAL (ustr "this")
  let upd = M.union (M.singleton (ustr "errors") errs)
  clearUncaughtErrorLog
  referenceUpdate ref $ \o -> return $ Just $ ODict $ upd $ case o of { Just (ODict o) -> o; _ -> mempty; }
  -- Run all "END{}" procedures.
  mapM_ execute (postExec xunit)
  return a

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
makeActionsForQuery :: [PatternTree Object [Subroutine]] -> UStr -> Exec [Action]
makeActionsForQuery tree instr = do
  -- TODO: need to make use of the programTokenizer set in the current 'ExecUnit'
  match (map (OString . toUStr) $ simpleTokenizer $ fromUStr instr)
  where
    match tox = return $
      flip concatMap (matchTree False (T.unionsWith (++) tree) tox) $ \ (patn, mtch, execs) ->
        flip map execs $ \exec -> seq exec $! seq instr $! seq patn $! seq mtch $!
          Action
          { actionQuery      = Just instr
          , actionPattern    = Just patn
          , actionTokens     = Just tox
          , actionMatch      = mtch
          , actionCodeBlock  = exec
          }

-- | This is the most important function in the Dao universe. It executes a string query with the
-- given module 'ExecUnit's. The string query is executed in each module, execution in each module
-- runs in it's own thread. This function is syncrhonous; it blocks until all threads finish
-- working. Pass a list of modules against which you want to execute the string, or
-- 'Prelude.Nothing' to execute the string against all modules.
execStringQuery :: UStr -> Maybe [UPath] -> Exec ()
execStringQuery instr modnames = do
  let select names = concat . M.elems .
        M.intersectionWith (++) (M.fromList $ zip names $ repeat []) . fmap (:[])
  xunits <- maybe M.elems select modnames <$> gets importGraph
  task   <- gets taskForExecUnits
  liftIO $ taskLoop_ task $ flip map xunits $ \xunit -> do
    let rulset = ruleSet xunit
    pdicat <- flip ioExec xunit $ betweenBeginAndEnd $ makeActionsForQuery [rulset] instr >>= execute
    -- TODO: this case statement should really call into some callback functions installed into the
    -- root 'ExecUnit'.
    case fst pdicat of
      PFail (ExecReturn{}) -> return ()
      PFail err            -> liftIO $ hPutStrLn stderr $ prettyShow err
      Backtrack            -> case programModuleName xunit of
        Nothing   -> return ()
        Just name -> liftIO $ hPutStrLn stderr $
          uchars name ++ ": does not compute.\n\t"++show instr
      OK                _  -> return ()

-- | Evaluates the @EXIT@ scripts for every presently loaded dao program, and then clears the
-- 'Dao.Interpreter.importGraph', effectively removing every loaded dao program and idea file from memory.
daoShutdown :: Exec ()
daoShutdown = (M.elems <$> gets importGraph) >>=
  mapM_ (\xunit -> inModule xunit $ gets quittingTime >>= mapM_ execute)

-- | Runs a single line of Dao scripting language code. In the current thread parse an input string
-- of type 'Dao.Evaluator.ScriptExpr' and then evaluate it. This is used for interactive evaluation.
-- The parser used in this function will parse a block of Dao source code, the opening and closing
-- curly-braces are not necessary. Therefore you may enter a semi-colon separated list of commands
-- and all will be executed.
evalScriptString :: String -> Exec ()
evalScriptString instr =
  execNested_ M.empty $ case concat <$> parse (daoGrammar{ mainParser=many script }) mempty instr of
    Backtrack -> liftIO $ hPutStrLn stderr "backtracking on malformed expression" >> return mempty
    PFail tok -> liftIO $ hPutStrLn stderr ("error: "++show tok) >> return mempty
    OK   expr -> mapM_ execute (expr >>= toInterm >>= \scrpt -> [TopScript scrpt $ getLocation scrpt])

-- | This is the main input loop. Pass an input function callback to be called on every loop. This
-- function should return strings to be evaluated, and return 'Data.Maybe.Nothing' to signal that
-- this loop should break.
daoInputLoop :: Exec (Maybe UStr) -> Exec ()
daoInputLoop getString = fix $ \loop -> do
  inputString <- getString
  case inputString of
    Nothing          -> return ()
    Just inputString -> execStringQuery inputString Nothing >> loop

