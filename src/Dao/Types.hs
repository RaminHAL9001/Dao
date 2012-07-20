{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- "src/Dao/Types.hs"  provides data types that are used throughout
-- the Dao System to facilitate execution of Dao programs, but are not
-- used directly by the Dao scripting language as Objects are.
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


module Dao.Types
  ( module Dao.Types
  , module Dao.Object
  , module Control.Monad.Reader
  ) where

import           Dao.Debug.OFF
import           Dao.Object
import           Dao.Object.Monad
import           Dao.Predicate
import           Dao.Combination
import           Dao.Pattern
import           Dao.Tree as T

import           Numeric

import           Data.Typeable
import           Data.Dynamic
import           Data.Maybe
import           Data.Either
import           Data.List
import           Data.Complex
import           Data.Int
import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Array.IArray
import           Data.Time hiding (parseTime)

import qualified Data.Map                  as M
import qualified Data.IntMap               as I
import qualified Data.Set                  as S
import qualified Data.ByteString.Lazy      as B

import           Control.Exception
import           Control.Concurrent

import           Control.Monad
import           Control.Monad.Reader

catchErrorCall :: Run a -> Run (Either ErrorCall a)
catchErrorCall fn = ReaderT $ \r -> try (runReaderT fn r)

----------------------------------------------------------------------------------------------------

-- | A 'SourceCode' is the structure loaded from source code. A 'Program' object is constructed from
-- 'SourceCode', and can be serialized to a binary format.
data SourceCode
  = SourceCode
    { sourceFullPath   :: UStr
      -- ^ the URL (full file path) from where this source code was received.
    , sourceModuleName :: Com UStr
      -- ^ the logical name of this program defined by the "module" keyword in the Dao script.
    , directives       :: Com [Com Directive]
    }
  deriving (Eq, Ord, Show, Typeable)

-- | When a script is executed, it's abstract syntax tree is converted into a monadic computation.
-- When this happens, the Haskell runtime system evaluates many "thunks" in memory to collapse the
-- computation to a simpler form. To improve efficiency, these thunks can be saved by keeping a
-- reference to the monadic computation, preventing it from being garbage coellcted and allowing it
-- to execute faster the next time.
data CachedExec ast m
  = OnlyAST   { sourceScript :: ast }
  | OnlyCache { cachedScript :: m }
  | HasBoth
    { sourceScript :: ast
    , cachedScript :: m
    }

-- | A 'CachedExec' stored in an 'Dao.Debug.DMVar'.
type CXRef ast m = DMVar (CachedExec ast m)

-- | Similar to 'Data.Maybe.fromMaybe', retrieve the 'cachedScript' item, or a given default.
getCached ::  m -> CachedExec ast m -> m
getCached deflt ce = case ce of
  OnlyAST   _   -> deflt
  OnlyCache   m -> m
  HasBoth   _ m -> m

-- | Similar to 'Data.Maybe.fromMaybe', retrieve the 'sourceScript' item, or a given default.
getSource ::  ast -> CachedExec ast m -> ast
getSource deflt ce = case ce of
  OnlyAST   ast   -> ast
  OnlyCache     _ -> deflt
  HasBoth   ast _ -> ast

modifyCXRef :: MLoc -> CXRef ast m -> (CachedExec ast m -> Run (CachedExec ast m, a)) -> Run a
modifyCXRef loc cxref = dModifyMVar loc cxref 

modifyCXRef_ :: MLoc -> CXRef ast m -> (CachedExec ast m -> Run (CachedExec ast m)) -> Run ()
modifyCXRef_ loc cxref = dModifyMVar_ loc cxref

-- | Cache the execution of the abstract syntax tree.
cacheExec :: (ast -> m) -> CachedExec ast m -> CachedExec ast m
cacheExec conv ca = case ca of
  OnlyAST ast -> HasBoth{sourceScript = ast, cachedScript = conv ast}
  ca          -> ca

-- | Cache execution of the abstract syntax tree inside of the 'Control.Concurrent.DMVar.DMVar' while
-- returning the cached method.
getCachedExec :: (ast -> m) -> CXRef ast m -> Run m
getCachedExec conv mvar = dModifyMVar xloc mvar $ \cc ->
  let cc' = cacheExec conv cc in return (cc', getCached undefined cc')

-- | De-reference (and hence free) the cached computation, unless the value of this 'CachedExec' is
-- 'OnlyCache', in which case no change is made.
freeCached :: CachedExec ast m -> CachedExec ast m
freeCached ca = case ca of
  HasBoth ast _ -> OnlyAST{sourceScript = ast}
  ca            -> ca

-- | De-reference (and hence free) the abstract syntax tree, unless the value of this 'CachedExec'
-- is 'OnlyAST', in which case no change is made.
freeAST :: CachedExec ast m -> CachedExec ast m
freeAST ca = case ca of
  HasBoth _ m -> OnlyCache{cachedScript = m}
  ca          -> ca

-- | A 'Directive' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data Directive
  = Attribute      (Com Name) (Com Name)
  | ToplevelDefine (Com [Name]) (Com ObjectExpr) 
  | RuleExpr       (Com Rule)
  | SetupExpr      (Com [Com ScriptExpr])
  | BeginExpr      (Com [Com ScriptExpr])
  | EndExpr        (Com [Com ScriptExpr])
  | TakedownExpr   (Com [Com ScriptExpr])
  | ToplevelFunc   (Com ()) (Com Name) (Com [Com Name]) (Com [Com ScriptExpr])
  deriving (Eq, Ord, Show, Typeable)

type TopLevelFunc = DMVar ([Object] -> ExecScript Object)

-- | This is the executable form of the 'SourceCode', which cannot be serialized, but is structured
-- in such a way as to make execution more efficient. It caches computed 'ScriptExpr'ns as some type
-- of monadic computation 'm'.
data Program m
  = Program
    { programModuleName :: Name
    , programImports    :: [UStr]
    , constructScript   :: [[Com ScriptExpr]]
    , destructScript    :: [[Com ScriptExpr]]
    , requiredBuiltins  :: [Name]
    , programAttributes :: M.Map Name Name
    , preExecScript     :: [CXRef [Com ScriptExpr] m]
      -- ^ the "guard scripts" that are executed before every string execution.
    , postExecScript    :: [CXRef [Com ScriptExpr] m]
      -- ^ the "guard scripts" that are executed after every string execution.
    , programTokenizer  :: Tokenizer
      -- ^ the tokenizer used to break-up string queries before being matched to the rules in the
      -- module associated with this runtime.
    , programComparator :: CompareToken
      -- ^ used to compare string tokens to 'Dao.Pattern.Single' pattern constants.
    , ruleSet           :: DMVar (PatternTree [CXRef (Com [Com ScriptExpr]) m])
    , staticData        :: DMVar (T.Tree Name Object)
    }

initProgram :: Name -> Run CachedProgram
initProgram modName = do
  pat  <- dNewMVar xloc "Program.ruleSet" T.Void
  dat  <- dNewMVar xloc "Program.staticData" T.Void
  -- pre  <- dNewMVar xloc "Program.preExecScript" []
  -- post <- dNewMVar xloc "Program.postExecScript" []
  return $
    Program
    { programModuleName = modName
    , programImports    = []
    , constructScript   = []
    , destructScript    = []
    , requiredBuiltins  = []
    , programAttributes = M.empty
    , preExecScript     = []
    , programTokenizer  = return . tokens . uchars
    , programComparator = (==)
    , postExecScript    = []
    , ruleSet           = pat
    , staticData        = dat
    }

----------------------------------------------------------------------------------------------------

-- | The magic number is the first 8 bytes to every 'Document'. It is the ASCII value of the string
-- "DaoData\0".
document_magic_number :: Word64
document_magic_number = 0x44616F4461746100

-- | This is the version number of the line protocol for transmitting document objects.
document_data_version :: Word64
document_data_version = 0

-- | A 'Document' is a simple binary protocol for storing 'Objects' to disk.
data Document
  = Document
    { docRefCount   :: Word
    , docModified   :: Word64
    , docInfo       :: UStr
    , docVersion    :: Word64
    , docRootObject :: DocData
    }

initDoc :: DocData -> Document
initDoc docdata =
  Document
  { docRefCount = 0
  , docModified = 0
  , docInfo = nil
  , docVersion = document_data_version
  , docRootObject = docdata
  }

-- | The data stored in a 'Document' is a 'Dao.Tree.Tree' that maps @['UStr']@ addresses to objects.
-- These addresses are much like filesystem paths, but are lists of 'Name's. Every node in the tree
-- can contain one 'Object' and/or another tree containing more objects (like a filesystem
-- directory).
type DocData = T.Tree Name Object

-- | A document stored in an 'Control.Concurrent.DMVar.DMVar'.
type DocHandle = DMVar Document

-- | Keeps track of the documents that are currently being used.
type DocList = M.Map UPath DocHandle
type DocListHandle = DMVar DocList

-- | A program table is an 'Control.Concurrent.DMVar.DMVar' associating 'programModuleName's to
-- 'ExecUnit's. The 'ExecUnit's themselves are also stored in an 'Control.Concurrent.DMVar.DMVar',
-- which must be extracted from in order to evaluate any of the 'ExecScript' functions in the IO
-- monad, that way the 'ExecScript' monad itself does not need to constantly extract the 'ExecUnit'
-- from it's containing 'Control.Concurrent.DMVar.DMVar' every time it needs to refer to one of the
-- 'ExecUnit' properties.
type ProgramTable  = DMVar (M.Map Name (DMVar ExecUnit))

type CachedAction      = CXRef (Com [Com ScriptExpr]) (ExecScript ())
type CachedGuardAction = CXRef [Com ScriptExpr] (ExecScript ())
type CachedProgram     = Program (ExecScript ())

-- | All evaluation of the Dao language takes place in the 'ExecScript' monad. It allows @IO@
-- functions to be lifeted into it so functions from "Control.Concurrent", "Dao.Document",
-- "System.IO", and other modules, can be evaluated.
type ExecScript a  = CEReader ExecUnit IO a

-- | Like 'Control.Monad.Reader.runReaderT' but specific to the 'Dao.Object.Evaluator.ExecScript'
-- monad, and is lifted into the 'Run' monad for convenience.
runExecScript :: ExecScript a -> ExecUnit -> Run (ContErr a)
runExecScript fn xunit = ReaderT $ \runtime ->
  runReaderT (runContErrT fn) (xunit{parentRuntime = runtime})

-- | Execute a 'Run' monad within an 'ExecScript' monad. Every 'ExecUnit' contains a pointer to the
-- 'Runtime' object that manages it, 
execScriptRun :: Run a -> ExecScript a
execScriptRun fn = ask >>= \xunit -> execIO (runReaderT fn (parentRuntime xunit))

-- | Pair an error message with an object that can help to describe what went wrong.
objectError :: Monad m => Object -> String -> ContErrT m err
objectError o msg = ceError (OPair (OString (ustr msg), o))

----------------------------------------------------------------------------------------------------

-- | All functions that are built-in to the Dao language, or built-in to a library extending the Dao
-- language, are stored in 'Data.Map.Map's from the functions name to an object of this type.
-- Functinos of this type are called by 'evalObject' to evaluate expressions written in the Dao
-- language.
newtype CheckFunc = CheckFunc { checkFunc :: [Object] -> Check (ContErr Object) }

-- | The 'Check' monad (which is a type of 'Dao.Predicate.PredicateIO' and lets you check types of
-- objects, particularly the argument statements passed to a built-in Dao function, to determine
-- which 'ExecScript' action to perform. 'Dao.Predicate.PredicateIO's safely handle
-- Haskell-language-level pattern match and case failures, so you don't need to write swathes of
-- Haskell case statements for every possible combination of input arguments.
type Check a = PredicateIO ExecUnit a

-- | Supply a 'Data.Map.Map' of 'Func's by placing them into the 'ExecUnit' before
-- calling 'execScriptCall'. This defines which functions are enabled by default to the evaluation
-- algorithm.
newtype Func = Func { evalBuiltin :: [Object] -> ExecScript Object }

-- | This function lets you run a 'Check' monad within a 'ExecScript' monad. The purpose is to check
-- the input arguments to built-in Dao functions. Express the fact that a predicate matches by
-- 'Control.Monad.return'ing a @'ExecScript' 'Dao.Types.Object'@ function. The first matching
-- predicate that executes without throwing a 'Dao.Object.Monad.ceError' will determine the value of
-- the 'Dao.Types.Object' returned by this function.
checkToExecScript :: UStr -> Object -> Check (ContErr a) -> ExecScript a
checkToExecScript exprType inType ckfn =
  do  xunit <- ask
      execIO (runCombinationT (runPredicateIO ckfn) xunit) >>= loop xunit []
  where
    loop :: ExecUnit -> [Object] -> [(Either Object (ContErr a), ExecUnit)] -> ExecScript a
    loop xunit errs optx = case optx of
      (Left  ONull, _):optx -> loop xunit errs optx
      (Left  err  , _):optx -> loop xunit (err:errs) optx
      (Right o    , _):_    -> returnContErr o
      [] -> returnContErr $ CEError $ OList $
              [ OString exprType
              , OString (ustr "cannot operate with given parameter types")
              , inType
              ] ++ errs

-- | Convert a 'TopLevelFunc' to a 'CheckFunc'.
toplevelToCheckFunc :: TopLevelFunc -> ExecScript CheckFunc
toplevelToCheckFunc top = execScriptRun (dReadMVar xloc top) >>= \top ->
  return $ CheckFunc $ \args -> execScriptToCheck id (top args) >>= checkOK

-- | Run an 'ExecScript' monad inside the 'Check'. This has the further effect of halting all
-- 'CEReturn's, so function call evaluation does not collapse the entire expression. 'CEError's are
-- converted to failed predicates.
execScriptToCheck :: (Object -> a) -> ExecScript a -> Check a
execScriptToCheck onCEReturn esfn = get >>= \xunit -> do
  ce <- liftIO (runReaderT (runExecScript esfn xunit) (parentRuntime xunit))
  case ce of
    CENext   ce -> return ce
    CEReturn ce -> return (onCEReturn ce)
    CEError  ce -> falseIO ce

runToCheck :: Run a -> Check (ContErr a)
runToCheck run = do
  PredicateIO $ CombinationT $ \xunit ->
    catches (runReaderT run (parentRuntime xunit) >>= \a -> return [(Right (CENext a), xunit)]) $
      let err e = return [(Right (CEError (OString (ustr (show e)))), xunit)]
      in  [ Handler $ \ (e::IOException   ) -> err e
          , Handler $ \ (e::ErrorCall     ) -> err e
          , Handler $ \ (e::ArithException) -> err e
          , Handler $ \ (e::ArrayException) -> err e
          ]

runToCheck_ :: Run a -> Check (ContErr Object)
runToCheck_ run = runToCheck run >> return (CENext OTrue)

-- | A 'CheckFunc' returns a 'Dao.Object.Monad.ContErr', which means you need to use an equation
-- like: @('Control.Monad.return' ('Dao.Object.Monad.CENext' a))@. But since this combinator is used
-- so often, it is provided here as an easy-to-remember function.
checkOK :: a -> PredicateIO st (ContErr a)
checkOK = return . CENext

-- | Use 'falseIO' at the point in the equation where the predicate can be declared as false, but
-- take a string and an object so an appropriate error message can be constructed.
checkFail :: String -> Object -> PredicateIO st ignored
checkFail msg obj = falseIO (OPair (OString (ustr msg), obj))

----------------------------------------------------------------------------------------------------

-- | This is the state that is used to run the evaluation algorithm.
data ExecUnit
  = ExecUnit
    { parentRuntime      :: Runtime
      -- ^ a reference to the 'Runtime' that spawned this 'ExecUnit'. Some built-in functions in the
      -- Dao scripting language may make calls that modify the state of the Runtime.
    , verbosePrint       :: Int -> UStr -> ExecScript ()
      -- ^ a hook to be used to print un-caught error messages.
    , commentPrint       :: [UStr] -> ExecScript ()
      -- ^ a hook to be used to print comments as the Dao script is executed.
    , currentExecJob     :: Maybe Job
      -- ^ a reference to the 'Job' that is currently running the 'ExecScript' that is using this
      -- 'ExecUnit' state.
    , currentDocument    :: Maybe DocHandle
      -- ^ the current document is set by the @with@ statement during execution of a Dao script.
    , currentProgram     :: Maybe CachedProgram
      -- ^ the program that is running in this execution unit, this may not be defined in the case
      -- that strings are being executed in an interactive session.
    , currentPattern     :: Object
      -- ^ the 'OPattern' that resulted in execution of this script, which may not be defined (set
      -- to 'ONull') in the case that execution was not triggered by a pattern match.
    , currentMatch       :: Maybe Match
      -- ^ the 'Dao.Pattern.Match' structure that resulted in execution of this script, which may
      -- not be defined (set to 'ONull') in the case that execution was not triggered by a pattern
      -- match.
    , currentBranch      :: [Name]
      -- ^ set by the @with@ statement during execution of a Dao script. It is used to prefix this
      -- to all global references before reading from or writing to those references.
    , importsTable       :: [DMVar ExecUnit]
      -- ^ a pointer to the ExecUnit of every Dao program imported with the @import@ keyword.
    , execAccessRules    :: FileAccessRules
      -- ^ restricting which files can be loaded by the program associated with this ExecUnit, these
      -- are the rules assigned this program by the 'ProgramRule' which allowed it to be loaded.
    , builtinFuncs       :: M.Map Name CheckFunc
      -- ^ a pointer to the builtin function table provided by the runtime.
    , toplevelFuncs      :: DMVar (M.Map Name TopLevelFunc)
    , execHeap           :: DMVar T_tree
    , execStack          :: DMVar [T_dict]
    , recursiveInput     :: DMVar [UStr]
    , uncaughtErrors     :: DMVar [Object]
    }

instance Bugged ExecUnit where
  askDebug           = fmap (runtimeDebugger . parentRuntime) ask
  setDebug dbg xunit = xunit{parentRuntime = (parentRuntime xunit){runtimeDebugger = dbg}}

----------------------------------------------------------------------------------------------------

-- | Rules dictating which files a particular 'ExecUnit' can load at runtime.
data FileAccessRules
  = RestrictFiles  Pattern
    -- ^ files matching this pattern will never be loaded
  | AllowFiles     Pattern
    -- ^ files matching this pattern can be loaded
  | ProgramRule    Pattern [FileAccessRules] [FileAccessRules]
    -- ^ programs matching this pattern can be loaded and will be able to load files by other rules.
    -- Also has a list of rules dictating which built-in function sets are allowed for use, but
    -- these rules are not matched to files, they are matched to the function sets provided by the
    -- 'Runtime'.
  | DirectoryRule  UPath   [FileAccessRules]
    -- ^ access rules will apply to every file in the path of this directory, but other rules
    -- specific to certain files will override these rules.

-- | Anything that can be loaded from the filesystem and used by the Dao 'Runtime' is a type of
-- this.
data File
  = ProgramFile -- ^ a program loaded and executable
    { publicFile  :: Bool
    , filePath    :: Name
    , logicalName :: Name
    , execUnit    :: DMVar ExecUnit
    }
  | ProgramEdit -- ^ a program executable and editable.
    { filePath   :: Name
    , sourceCode :: SourceCode
    , execUnit   :: DMVar ExecUnit
    }
  | IdeaFile -- ^ a file containing a 'Dao.Tree.Tree' of serialized 'Dao.Object.Object's.
    { filePath :: Name
    , fileData :: DocHandle
    }

-- | Used to select programs from the 'pathIndex' that are currently available for recursive
-- execution.
isProgramFile :: File -> Bool
isProgramFile file = case file of
  ProgramFile _ _ _ _ -> True
  _                   -> False

-- | A type of function that can split an input query string into 'Dao.Pattern.Tokens'. The default
-- splits up strings on white-spaces, numbers, and punctuation marks.
type Tokenizer = UStr -> ExecScript Tokens

-- | A type of function that can match 'Dao.Pattern.Single' patterns to 'Dao.Pattern.Tokens', the
-- default is the 'Dao.Pattern.exact' function. An alternative is 'Dao.Pattern.approx', which
-- matches strings approximately, ignoring transposed letters and accidental double letters in words.
type CompareToken = UStr -> UStr -> Bool

data Runtime
  = Runtime
    { pathIndex            :: DMVar (M.Map UPath File)
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , logicalNameIndex     :: DMVar (M.Map Name File)
      -- ^ program files have logical names. This index allows for easily looking up 'File's by
      -- their logical name.
    , jobTable             :: DMVar (M.Map ThreadId Job)
      -- ^ A job is any string that has caused execution across loaded dao scripts. This table keeps
      -- track of any jobs started by this runtime.
    , defaultTimeout       :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , initialBuiltins      :: M.Map Name CheckFunc
      -- ^ fundamental built-in functions common to all programs.
    , functionSets         :: M.Map Name (M.Map Name CheckFunc)
      -- ^ every labeled set of built-in functions provided by this runtime is listed here. This
      -- table is checked when a Dao program is loaded that has "requires" directives.
    , availableTokenizers  :: M.Map Name Tokenizer
      -- ^ a table of available string tokenizers.
    , availableComparators :: M.Map Name CompareToken
      -- ^ a table of available string matching functions.
    , fileAccessRules      :: [FileAccessRules]
      -- ^ rules loaded by config file dicating programs and ideas can be loaded by Dao, and also,
      -- which programs can load which programs and ideas.
    , runtimeDebugger      :: DebugHandle
    }

-- | This is the monad used for most all methods that operate on the 'Runtime' state.
type Run a = ReaderT Runtime IO a

-- | Unlift a 'Run' monad.
runIO :: Runtime -> Run a -> IO a
runIO runtime runFunc = runReaderT runFunc runtime

instance Bugged Runtime where
  askDebug             = fmap runtimeDebugger ask
  setDebug dbg runtime = runtime{runtimeDebugger = dbg}

----------------------------------------------------------------------------------------------------

-- | A 'Task' represents a single thread running a single 'ScriptExpr' in response to a
-- pattern matching its associated rule.
data Task
  = RuleTask
    { taskPattern     :: Object -- ^ Either 'OPattern' or 'ONull'.
    , taskMatch       :: Match
    , taskAction      :: CachedAction
    , taskExecUnit    :: ExecUnit
    }
  | GuardTask -- ^ Tasks that are created from @BEGIN@ and @END@ blocks in a Dao script.
    { taskGuardAction :: CachedGuardAction
    , taskExecUnit    :: ExecUnit
    }

-- | A 'Job' keeps track of all threads that are executing in response to an input string.  You can
-- signal the Job to signal all associated threads, you can wait on the 'Job's
-- 'Dao.Runtime.jobTaskCompletion' semaphore to wait for the job to complete, and you can set a
-- timer to time-out this 'Job'.
data Job
  = Job
    { jobTaskThread  :: ThreadId
      -- ^ The thread that loops, waiting for tasks in the queue to complete.
    , jobInputString :: UStr
      -- ^ the input string that triggered this job.
    , jobTimerThread :: DMVar (Maybe ThreadId)
      -- ^ If there is a time limit on this job, the thread sleeping until the timeout occurs is
      -- identified here and can be killed if the last 'Task' finishes before the timeout event.
    , jobCompletion  :: DQSem
      -- ^ This semaphore is signaled when the last 'Task' in the 'Dao.Runtime.taskExecTable' below
      -- completes, and just before the 'jobTaskThread' reads the 'readyTasks'
      -- 'Control.Concurrent.DMVar.DMVar' to launch the next set of waiting tasks.
    , taskCompletion :: DMVar ThreadId
      -- ^ whenever one thread completes, it signals this 'Control.Concurrent.DMVar.DMVar' with it's
      -- own 'Control.Concurrent.ThreadId' so it can be removed from the 'taskExecTable' below.
    , readyTasks     :: DMVar [Task]
      -- ^ used as a channel to launch new task threads. Use 'Control.Concurrent.DMVar.putMVar' to
      -- place 'Task's into this 'Control.Concurrent.DMVar.DMVar'. The task manager algorithm running
      -- in the 'jobTaskThread' above will take these tasks and execute each one in a separate
      -- thread, mapping each task to a 'Control.Concurrent.ThreadId' in the 'taskExecTable'.
    , taskExecTable  :: DMVar (M.Map ThreadId Task)
      -- ^ all running 'Task's associated with this job are stored in this table. It contains a list
      -- of 'Task's, each task is mapped to a 'Control.Concurrent.ThreadId', and each group of
      -- threads is mapped to the 'CachedProgram' from where the executing tasks originated.
    , taskFailures   :: DMVar (M.Map Name [(Task, SomeException)])
      -- ^ if a task dies due to an exception raised, then the exception is caught and mapped to the
      -- task. This is different from an error thrown from a script, these are uncaught Haskell
      -- exceptions from "Control.Exception" resulting in a thread being terminated.
    }

