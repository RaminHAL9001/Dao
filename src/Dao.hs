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
import           Dao.Predicate
import           Dao.PPrint
import           Dao.Token
import           Dao.Parser
import           Dao.Interpreter.Parser
import           Dao.Interpreter.AST
import           Dao.Interpreter

import qualified Dao.Lib.Array       as Dao
import qualified Dao.Lib.File        as Dao
import qualified Dao.Lib.ListEditor  as Dao

import           Data.Function
import           Data.Monoid
import qualified Data.Map as M
import           Data.Typeable

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State

import           System.IO

----------------------------------------------------------------------------------------------------

-- | This is the most important function in the Dao universe. It executes a string query with the
-- given module 'ExecUnit's. The string query is executed in each module, execution in each module
-- runs in it's own thread. This function is syncrhonous; it blocks until all threads finish
-- working. Pass a list of modules against which you want to execute the string, or
-- 'Prelude.Nothing' to execute the string against all modules.
execStringQuery :: UStr -> Maybe [UPath] -> Exec ()
execStringQuery instr modnames = do
  let select names = M.assocs .
        M.intersectionWith (flip const) (M.fromList $ zip names $ repeat ())
  xunits  <- maybe M.assocs select modnames <$> gets importGraph
  task    <- gets taskForExecUnits
  updates <- liftIO $ newMVar M.empty
  liftIO $ taskLoop_ task $ flip map xunits $ \ (modname, xunit) -> do
    (result, xunit) <- flip ioExec xunit $ do
      let rulset = ruleSet xunit
      tokens <- runTokenizer [obj instr]
      betweenBeginAndEnd $ makeActionsForQuery [rulset] tokens >>= execute
    modifyMVar_ updates (return . M.insert modname xunit)
    case result of
      PFail (ExecReturn{}) -> return ()
      PFail err            -> liftIO $ hPutStrLn stderr $ prettyShow err
      Backtrack            -> case programModuleName xunit of
        Nothing   -> return ()
        Just name -> liftIO $ hPutStrLn stderr $
          uchars name ++ ": does not compute.\n\t"++show instr
      OK                _  -> return ()
  updated <- liftIO (takeMVar updates)
  modify $ \xunit -> xunit{ importGraph = M.union updated (importGraph xunit) }

----------------------------------------------------------------------------------------------------

-- | This is a 'DaoSetup' function used to install the standard library into the runtime.
loadDaoStandardLibrary :: DaoSetup
loadDaoStandardLibrary = do
  loadLibrary_Program
  Dao.loadLibrary_Array
  Dao.loadLibrary_File
  Dao.loadLibrary_ListEditor
  daoFunction "Pattern"       builtin_Pattern
  daoFunction "DaoPattern"    builtin_DaoPattern
  daoFunction "SimplePattern" builtin_SimplePattern
  daoFunction "FuzzyPattern"  builtin_FuzzyPattern

----------------------------------------------------------------------------------------------------

-- | This function will load a module from the source code at the given file path and registers it
-- into the current Dao runtine. This function creates a child 'ExecUnit' for the current 'ExecUnit'
-- and populates it with data by parsing and evaluating the contents of a Dao script file at the
-- given filesystem path. This function returns a pair of values, first is the source code Abstract
-- Syntax Tree (AST) loaded from the source code file, and second is the 'ExecUnit' associated with
-- the source code. If the module at the given path has already been loaded, 'Prelude.Nothing' is
-- returned as the first value, and the already loaded module 'ExecUnit' is returned as the second
-- value.
loadModule :: UPath -> Exec (Maybe (AST_SourceCode Object), ExecUnit)
loadModule path = do
  mod <- M.lookup path <$> gets importGraph
  case mod of
    Just mod -> return (Nothing, mod)
    Nothing  ->  do
      ast <- readSource    path
      mod <- compileSource path ast
      modify $ \xunit -> xunit{ importGraph = M.insert path mod (importGraph xunit) }
      return (Just ast, mod)

-- | Create an Abstract Syntax Tree (AST) of a Dao program by reading the source code for the
-- program from a file at the given path. This function only does the parsing, it does /NOT/ create
-- an 'ExecUnit' or register the 'ExecUnit' into the runtime for execution.
readSource :: UPath -> Exec (AST_SourceCode Object)
readSource path = do
  text <- liftIO $ readFile (uchars path)
  case parse daoGrammar mempty text of
    OK    ast -> deepseq ast $! return ast
    Backtrack -> execThrow $ obj [obj path, obj "does not appear to be a valid Dao source file"]
    PFail err -> loadModParseFailed (Just path) err

-- | Create a new 'ExecUnit' from a 'AST_SourceCode' object that was produced by 'readSource'. The
-- resulting 'ExecUnit' is /NOT/ registered into the Dao runtime for execution. Pass the a name to
-- be used for the source file in error reports.
compileSource :: UPath -> AST_SourceCode Object -> Exec ExecUnit
compileSource path ast = do
  mod <- newExecUnit (Just path)
  ((), mod) <- inModule (mod{programModuleName=Just path}) $ evalTopLevelAST ast >>= execute
  return mod

----------------------------------------------------------------------------------------------------

type DepGraph = M.Map UPath [UPath]

getDepFiles :: DepGraph -> [UPath]
getDepFiles = M.keys

loadEveryModule :: [UPath] -> Exec ()
loadEveryModule args = do
  deps <- importFullDepGraph args
  mapM_ (fmap snd . loadModule) (getDepFiles deps)

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

----------------------------------------------------------------------------------------------------

-- | This is a data type used to represent Dao programs in the Dao runtime. It makes available to
-- the Dao runtime several of the functions in this module.
data DaoProgram
  = DaoProgram
    { programPath       :: UPath
    , programModified   :: Bool
    , programSourceCode :: AST_SourceCode Object
    , programExecUnit   :: Maybe ExecUnit
    }
  deriving Typeable

instance HasNullValue DaoProgram where
  nullValue =
    DaoProgram
    { programPath=nil
    , programModified=False
    , programSourceCode=nullValue
    , programExecUnit=Nothing
    }
  testNull (DaoProgram{programModified=modified, programSourceCode=code, programExecUnit=subxunit}) =
    not modified && testNull code && maybe True (const False) subxunit

instance PPrintable DaoProgram where { pPrint = pPrint . programSourceCode }

instance ToDaoStructClass DaoProgram where
  toDaoStruct = void $ do
    renameConstructor "DaoProgram"
    "path" .=@ programPath
    "AST"  .=@ programSourceCode

instance FromDaoStructClass DaoProgram where
  fromDaoStruct = return DaoProgram <*> req "path" <*> pure True <*> req "AST" <*> pure Nothing

loadLibrary_Program :: DaoSetup
loadLibrary_Program = do
  daoClass (haskellType::DaoProgram)
  daoFunction "DaoProgram" $
    daoFunc
    { daoForeignFunc = \ () ox -> case ox of
        [path] -> do
          path <- xmaybe (fromObj path)
          (isNew, src) <- execCatchIO
            (readSource path >>= \src -> return (False, src))
            [ newExecIOHandler $ \e -> case e::IOException of
                _ -> return (True, nullValue)
            ]
          return $ flip (,) () $ Just $ obj $
            DaoProgram
            { programPath       = path, programModified = isNew
            , programSourceCode = src , programExecUnit = Nothing
            }
        _ -> execThrow $ obj $
          [obj "loadProgram", obj "function requires a single file path parameter"]
    }

instance ObjectClass DaoProgram where { obj=new; fromObj=objFromHata; }

instance HataClass DaoProgram where
  haskellDataInterface = interface "DaoProgram" $ do
    autoDefPPrinter >> autoDefFromStruct >> autoDefToStruct >> autoDefNullTest
    defMethod0 "isModified" $ \prog -> return (Just $ obj $ programModified prog, prog)
    defMethod0 "isCompiled" $ \prog -> return (Just $ obj $ maybe False (const True) (programExecUnit prog), prog)
    defMethod0 "compile" $ programCompile >=> \prog -> return (Just $ obj prog, prog)
    defMethod  "query"   $ daoFunc{ daoForeignFunc = makeEvalActions $ return . Just . obj . fmap obj }
    defMethod  "do"      $ daoFunc{ daoForeignFunc = makeEvalActions $ msum . fmap execute }
    defMethod  "doAll"   $ daoFunc{ daoForeignFunc = makeEvalActions $ fmap (Just . obj) . execute }
    defMethod0 "save"    $ fmap ((,) Nothing) . programSave

programCompile :: DaoProgram -> Exec DaoProgram
programCompile prog = do
  if maybe True (const $ programModified prog) (programExecUnit prog)
  then do
    subxunit <- compileSource (programPath prog) (programSourceCode prog)
    return $ prog{ programExecUnit=Just subxunit }
  else return prog

programSave :: DaoProgram -> Exec DaoProgram
programSave prog =
  if programModified prog
  then do
    prog <- programCompile prog
    liftIO $ writeFile (uchars $ programPath prog) (prettyShow $ programSourceCode prog)
    return $ prog{ programModified=False }
  else return prog

makeEvalActions :: ([Action] -> Exec (Maybe Object)) -> DaoProgram -> [Object] -> Exec (Maybe Object, DaoProgram)
makeEvalActions f prog ox = do 
  -- automatically compile if it has not been already
  (prog, subxunit) <- case programExecUnit prog of
    Just subxunit -> return (prog, subxunit)
    Nothing       -> do
      prog <- programCompile prog
      return (prog, maybe (error "programCompile did not set sub ExecUnit") id (programExecUnit prog))
  -- create actions and pass them to the evaluator function 'f'
  (o, subxunit) <- inModule subxunit $
    join (return makeActionsForQuery <*> getGlobalRuleSet <*> runTokenizer ox) >>= f
  return (o, prog{ programExecUnit=Just subxunit })

----------------------------------------------------------------------------------------------------

_makePatternizerFunc :: ExecTokenizer -> () -> [Object] -> Exec (Maybe Object, ())
_makePatternizerFunc tok () = fmap (flip (,) () . Just . obj) . constructPatternWith tok

builtin_Pattern :: DaoFunc ()
builtin_Pattern =
  daoFunc
  { daoForeignFunc = \ () ox -> gets programTokenizer >>= \tok -> _makePatternizerFunc tok () ox }

builtin_DaoPattern :: DaoFunc ()
builtin_DaoPattern = 
  daoFunc
  { daoForeignFunc = _makePatternizerFunc $ ExecTokenizer $ \strs -> do
      let (success, (toks, rem)) = runLexer (tokenDBLexer daoTokenDB) (uchars strs)
      if success
      then return $ fmap (obj . tokenToUStr) toks
      else execThrow $ obj [ obj "could not tokenizer string", obj rem]
  }

builtin_SimplePattern :: DaoFunc ()
builtin_SimplePattern = daoFunc{ daoForeignFunc = _makePatternizerFunc defaultTokenizer }

builtin_FuzzyPattern :: DaoFunc ()
builtin_FuzzyPattern = daoFunc{ daoForeignFunc = _makePatternizerFunc defaultTokenizer }

