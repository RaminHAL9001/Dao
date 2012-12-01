-- "src/Dao/Evaluator.hs"  provides functions for executing the Dao
-- scripting language, i.e. functions evaluating the parsed abstract
-- syntax tree.
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


-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Evaluator where

import           Dao.Debug.OFF
import           Dao.Types
import qualified Dao.Tree as T
import           Dao.Pattern
import           Dao.Resource
import           Dao.Predicate
import           Dao.Combination

import           Dao.Object.Monad
import           Dao.Object.Data
import           Dao.Object.Show
import           Dao.Object.Binary

import           Control.Exception
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State -- for constructing 'Program's from 'SourceCode's.

import           Data.Maybe
import           Data.Either
import           Data.Array.IArray
import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.List
import           Data.Complex
import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.IntMap as I
import qualified Data.ByteString.Lazy.UTF8 as U

--debug: for use with "trace"
-- import Debug.Trace

----------------------------------------------------------------------------------------------------

initExecUnit :: Runtime -> TreeResource -> Run ExecUnit
initExecUnit runtime initGlobalData = do
  unctErrs <- dNewMVar xloc "ExecUnit.uncaughtErrors" []
  recurInp <- dNewMVar xloc "ExecUnit.recursiveInput" []
  qheap    <- newTreeResource  "ExecUnit.queryTimeHeap" T.Void
  xstack   <- dNewMVar xloc "ExecUnit.execStack" emptyStack
  toplev   <- dNewMVar xloc "ExecUnit.toplevelFuncs" M.empty
  files    <- dNewMVar xloc "ExecUnit.execOpenFiles" M.empty
  cache    <- dNewMVar xloc "ExecUnit.referenceCache" M.empty
  return $
    ExecUnit
    { parentRuntime      = runtime
    , verbosePrint       = \_ _ -> return ()
    , commentPrint       = \_   -> return ()
    , currentExecJob     = Nothing
    , currentDocument    = Nothing
    , currentProgram     = Nothing
    , currentPattern     = ONull
    , currentMatch       = Nothing
    , currentBranch      = []
    , importsTable       = []
    , execAccessRules    = RestrictFiles (Pattern{getPatUnits = [Wildcard], getPatternLength = 1})
    , builtinFuncs       = initialBuiltins runtime
    , toplevelFuncs      = toplev
    , execHeap           = initGlobalData
    , queryTimeHeap      = qheap
    , referenceCache     = cache
    , execStaticVars     = Nothing
    , execStack          = xstack
    , execOpenFiles      = files
    , recursiveInput     = recurInp
    , uncaughtErrors     = unctErrs
    }

setupExecutable :: Bugged r => Com [Com ScriptExpr] -> ReaderT r IO Executable
setupExecutable scrp = do
  staticRsrc <- newMapResource "Executable.staticVars" M.empty
  return $
    Executable
    { staticVars = staticRsrc
    , executable = map (stripComments . unComment) (unComment scrp)
    }

runExecutable :: Executable -> ExecScript ()
runExecutable exe = localCE (\xunit -> return (xunit{execStaticVars = Just (staticVars exe)})) $ do
  ce <- catchContErr (execGuardBlock (map Com (executable exe)))
  returnContErr $ case ce of
    CEReturn _ -> CENext ()
    CENext   _ -> CENext ()
    CEError  e -> CEError e

-- | Execute a 'Dao.Types.Script' with paramters passed as a list of 
-- @'Dao.Types.Com' 'Dao.Object.ObjectExpr'@. This essentially treats the application of
-- paramaters to a script as a static abstract syntax tree, and converts this tree to an
-- @'ExecScript' 'Dao.Types.Object'@ function.
execScriptCall :: [Com ObjectExpr] -> Script -> ExecScript Object
execScriptCall args scrp = bindArgsExpr (unComment (scriptArgv scrp)) args $
  catchCEReturn (execScriptBlock (scriptCode scrp) >> ceReturn ONull)

-- | Execute a 'Dao.Types.Rule' object as though it were a script that could be called. The
-- parameters passed will be stored into the 'currentMatch' slot durring execution, but all
-- parameters passed must be of type 'Dao.Types.OString', or an error is thrown.
execRuleCall :: [Com ObjectExpr] -> Rule -> ExecScript Object
execRuleCall ax rule = do
  let typeErr o =
        typeError o "when calling a Rule object as though it were a function, all parameters" $
          show ListType++", where each list contains only objects of type "++show StringType
  ax <- forM ax $ \a -> evalObject a >>= \a -> case a of
    OList ax -> forM ax $ \a -> case a of
      OString a -> return a
      _ -> typeErr a
    _ -> typeErr a
  flip local (pushExecStack M.empty (execScriptBlock (ruleAction rule) >> ceReturn ONull)) $
    \xunit -> xunit{currentMatch = Just (matchFromList [] (length ax) ax)}

-- | Very simply executes every given script item. Does not use catchCEReturn, does not use
-- 'nestedExecStack'. CAUTION: you cannot assign to local variables unless you call this method
-- within the 'nestedExecStack' or 'pushExecStack' functions. Failure to do so will cause a stack
-- underflow exception.
execScriptBlock :: Com [Com ScriptExpr] -> ExecScript ()
execScriptBlock block = mapM_ execScriptExpr (unComment block)

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code founf in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [Com ScriptExpr] -> ExecScript ()
execGuardBlock block = void (pushExecStack M.empty (execScriptBlock (Com block) >> return ONull))

-- $BasicCombinators
-- These are the most basic combinators for converting working with the 'ExecUnit' of an
-- 'ExecScript' monad.

----------------------------------------------------------------------------------------------------
-- $StackOperations
-- Operating on the local stack.

stack_underflow = error "INTERNAL ERROR: stack underflow"

-- | Push a new empty local-variable context onto the stack. Does NOT 'catchCEReturn', so it can be
-- used to push a new context for every level of nested if/else/for/try/catch statement, or to
-- evaluate a macro, but not a function call. Use 'pushExecStack' to perform a function call within
-- a function call.
nestedExecStack :: T_dict -> ExecScript a -> ExecScript a
nestedExecStack init exe = do
  stack <- fmap execStack ask
  execRun (dModifyMVar_ xloc stack (return . stackPush init))
  ce <- catchContErr exe
  execRun (dModifyMVar_ xloc stack (return . stackPop))
  returnContErr ce

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. Use 'catchCEReturn' to prevent return calls from halting execution beyond this
-- function. This is what you should use to perform a Dao function call within a Dao function call.
pushExecStack :: T_dict -> ExecScript Object -> ExecScript Object
pushExecStack dict exe = do
  stackMVar <- execRun (dNewMVar xloc "pushExecStack/ExecUnit.execStack" (Stack [dict]))
  ce <- catchContErr (local (\xunit -> xunit{execStack = stackMVar}) exe)
  case ce of
    CEReturn obj -> return obj
    _            -> returnContErr ce

----------------------------------------------------------------------------------------------------

-- | Used to evaluate an expression like @$1@, retrieves the matched pattern associated with an
-- integer. Specifically, it returns a list of 'Dao.ObjectObject's where each object is an
-- 'Dao.Types.OString' contained at the integer index of the 'Dao.Pattern.matchGaps' of a
-- 'Dao.Pattern.Pattern'.
evalIntRef :: Int -> ExecScript Object
evalIntRef i = do
  match <- fmap currentMatch ask
  let oi = OInt (fromIntegral i)
  case match of
    Nothing -> do
      objectError oi ("not in pattern match context, cannot evaluate $"++show i)
    Just ma -> case matchGaps ma of
      Nothing -> do
        objectError oi ("currently matching pattern has no variables, cannot evaluate $"++show i)
      Just ma | i==0 -> return $ OArray $
        listArray (let (a, b) = bounds ma in (fromIntegral a, fromIntegral b)) $
          map (OList . map OString) (elems ma)
      Just ma | inRange (bounds ma) i -> return (OList (map OString (ma!i)))
      Just ma -> do
        objectError oi $ concat $
          [ "pattern match variable $"
          , show i ++ " is out of range "
          , show (bounds ma)
          , " in the current pattern match context"
          ]

-- | Lookup an object in the 'execHeap' for this 'ExecUnit'.
execHeapLookup :: [Name] -> ExecScript (Maybe Object)
execHeapLookup name = ask >>= \xunit -> inEvalDoReadResource (execHeap xunit) name

-- | Lookup an object in the 'execHeap' for this 'ExecUnit'.
execHeapUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
execHeapUpdate name runUpdate = ask >>= \xunit ->
  inEvalDoUpdateResource (execHeap xunit) name runUpdate

execHeapDefine :: [Name] -> Object -> ExecScript (Maybe Object)
execHeapDefine name obj = execHeapUpdate name (return . const (Just obj))

execHeapDelete :: [Name] -> Object -> ExecScript (Maybe Object)
execHeapDelete name obj = execHeapUpdate name (return . const Nothing)

-- | Lookup a reference value in the durrent document, if the current document has been set with a
-- "with" statement.
curDocVarLookup :: [Name] -> ExecScript (Maybe Object)
curDocVarLookup name = do
  xunit <- ask
  case currentDocument xunit of
    Nothing                  -> return Nothing
    Just file@(IdeaFile _ _) -> inEvalDoReadResource (fileData file) (currentBranch xunit ++ name)
    _ -> error ("current document is not an idea file, cannot lookup reference "++showRef name)

-- | Update a reference value in the durrent document, if the current document has been set with a
-- "with" statement.
curDocVarUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
curDocVarUpdate name runUpdate = do
  xunit <- ask
  case currentDocument xunit of
    Nothing                  -> return Nothing
    Just file@(IdeaFile _ _) ->
      inEvalDoUpdateResource (fileData file) (currentBranch xunit ++ name) runUpdate
    _ -> error ("current document is not an idea file, cannot update reference "++showRef name)

curDocVarDefine :: [Name] -> Object -> ExecScript (Maybe Object)
curDocVarDefine ref obj = curDocVarUpdate ref (return . const (Just obj))

curDocVarDelete :: [Name] -> Object -> ExecScript (Maybe Object)
curDocVarDelete ref obj = curDocVarUpdate ref (return . const Nothing)

-- | Lookup a value in the 'execStack'.
localVarLookup :: Name -> ExecScript (Maybe Object)
localVarLookup sym =
  fmap execStack ask >>= execRun . dReadMVar xloc >>= return . msum . map (M.lookup sym) . mapList

-- | Apply an altering function to the map at the top of the local variable stack.
localVarUpdate :: Name -> (Maybe Object -> Maybe Object) -> ExecScript (Maybe Object)
localVarUpdate name alt = ask >>= \xunit -> execRun $
  dModifyMVar xloc (execStack xunit) $ \ax -> case mapList ax of
    []   -> stack_underflow
    a:ax ->
      let obj = alt (M.lookup name a)
      in  return (Stack (M.alter (const obj) name a : ax), obj)

-- | Force the local variable to be defined in the top level 'execStack' context, do not over-write
-- a variable that has already been defined in lower in the context stack.
localVarDefine :: Name -> Object -> ExecScript (Maybe Object)
localVarDefine name obj = localVarUpdate name (const (Just obj))

-- | To define a global variable, first the 'currentDocument' is checked. If it is set, the variable
-- is assigned to the document at the reference location prepending 'currentBranch' reference.
-- Otherwise, the variable is assigned to the 'execHeap'.
localVarModify :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
localVarModify name alt = do
  xunit <- ask
  let prefixName = currentBranch xunit ++ name
  case currentDocument xunit of
    Nothing                     -> inEvalDoUpdateResource (execHeap xunit) name alt
    Just file | isIdeaFile file -> error "TODO" -- execRun $ dModifyMVar_ xloc (fileData file) $ \doc -> return $
      -- doc{docRootObject = T.update prefixName alt (docRootObject doc), docModified = 1 + docModified doc}
    Just file                   -> ceError $ OList $ map OString $
      [ustr "current document is not a database", filePath file]

localVarDelete :: Name -> ExecScript (Maybe Object)
localVarDelete nm = localVarUpdate nm (const Nothing)

-- | Lookup an object, first looking in the current document, then in the 'execHeap'.
globalVarLookup :: [Name] -> ExecScript (Maybe Object)
globalVarLookup ref = ask >>= \xunit ->
  (if isJust (currentDocument xunit) then curDocVarLookup else execHeapLookup) ref

globalVarUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
globalVarUpdate ref runUpdate = ask >>= \xunit ->
  (if isJust (currentDocument xunit) then curDocVarUpdate else execHeapUpdate) ref runUpdate

-- | To define a global variable, first the 'currentDocument' is checked. If it is set, the variable
-- is assigned to the document at the reference location prepending 'currentBranch' reference.
-- Otherwise, the variable is assigned to the 'execHeap'.
globalVarDefine :: [Name] -> Object -> ExecScript (Maybe Object)
globalVarDefine name obj = globalVarUpdate name (return . const (Just obj))

-- | To delete a global variable, the same process of searching for the address of the object is
-- followed for 'globalVarDefine', except of course the variable is deleted.
globalVarDelete :: [Name] -> ExecScript (Maybe Object)
globalVarDelete name = globalVarUpdate name (return . const Nothing)

qTimeVarLookup :: [Name] -> ExecScript (Maybe Object)
qTimeVarLookup ref = ask >>= \xunit -> inEvalDoReadResource (queryTimeHeap xunit) ref

qTimeVarUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
qTimeVarUpdate ref runUpdate = ask >>= \xunit ->
  inEvalDoUpdateResource (queryTimeHeap xunit) ref runUpdate

qTimeVarDefine :: [Name] -> Object -> ExecScript (Maybe Object)
qTimeVarDefine name obj = qTimeVarUpdate name (return . const (Just obj))

qTimeVarDelete :: [Name] -> ExecScript (Maybe Object)
qTimeVarDelete name = qTimeVarUpdate name (return . const Nothing)

clearAllQTimeVars :: ExecUnit -> Run ()
clearAllQTimeVars xunit = modifyUnlocked_ (queryTimeHeap xunit) (return . const T.Void)

-- | Lookup a reference value in the static object table of the currently loaded program. Static
-- objects are objects defined in the top level of the program's source code.
staticVarLookup :: [Name] -> ExecScript (Maybe Object)
staticVarLookup name = error "Dao.Evaluator.staticVarLookup not yet defined"

-- | Lookup a reference value in the static object table of the currently loaded program. Static
-- objects are objects defined in the top level of the program's source code.
staticVarUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
staticVarUpdate name runUpdate = error "Dao.Evaluator.staticVarUpdate not yet defined"

staticVarDefine :: [Name] -> Object -> ExecScript (Maybe Object)
staticVarDefine name obj = error "Dao.Evaluator.staticVarDefine not yet defined"

staticVarDelete :: [Name] -> Object -> ExecScript (Maybe Object)
staticVarDelete name obj = error "Dao.Evaluator.staticVarDelete not yet defined"

----------------------------------------------------------------------------------------------------

-- | TODO: needs to cache all lookups to make sure the same reference produces the same value across
-- all lookups. The cache will be cleared at the start of every run of 'evalObject'.
readReference :: Reference -> ExecScript (Maybe Object)
readReference ref = case ref of
  IntRef     i     -> fmap Just (evalIntRef i)
  LocalRef   nm    -> localVarLookup nm
  QTimeRef   ref   -> qTimeVarLookup ref
  StaticRef  ref   -> error "TODO: haven't yet defined lookup behavior for static references"
  GlobalRef  ref   -> globalVarLookup ref
  ProgramRef p ref -> error "TODO: haven't yet defined lookup behavior for Program references"
  FileRef    f ref -> error "TODO: haven't yet defined lookup behavior for file references"
  MetaRef    _     -> error "cannot dereference a reference-to-a-reference"

-- | All assignment operations are executed with this function. To modify any variable at all, you
-- need a reference value and a function used to update the value. This function will select the
-- correct value to modify based on the reference type and value, and modify it according to this
-- function. TODO: the use of "dModifyMVar" to update variables is just a temporary fix, and will
-- almost certainly cause a deadlock. But I need this to compile before I begin adding on the
-- deadlock-free code.
updateReference :: Reference -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
updateReference ref modf = do
  xunit <- ask
  let updateRef :: DMVar a -> (a -> Run (a, ContErr Object)) -> ExecScript (Maybe Object)
      updateRef dmvar runUpdate = fmap Just (execScriptRun (dModifyMVar xloc dmvar runUpdate) >>= returnContErr)
      execUpdate :: ref -> a -> Maybe Object -> ((Maybe Object -> Maybe Object) -> a) -> Run (a, ContErr Object)
      execUpdate ref store lkup upd = do
        err <- flip runExecScript xunit $ modf lkup
        case err of
          CENext   val -> return (upd (const val), CENext (fromMaybe ONull val))
          CEReturn val -> return (upd (const (Just val)), CEReturn val)
          CEError  err -> return (store, CEError err)
  case ref of
    IntRef     i          -> error "cannot assign values to a pattern-matched reference"
    LocalRef   ref        -> localVarLookup ref >>= \obj -> localVarUpdate ref (const obj)
    GlobalRef  ref        -> globalVarUpdate ref modf
    QTimeRef   ref        -> qTimeVarUpdate ref modf
    StaticRef  ref        -> error "TODO: you haven't yet defined update behavior for Static references"
    ProgramRef progID ref -> error "TODO: you haven't yet defined update behavior for Program references"
    FileRef    path   ref -> error "TODO: you haven't yet defined update behavior for File references"
    MetaRef    _          -> error "cannot assign values to a meta-reference"

-- | A 'Dao.Types.Check' monad computation to run 'globalVarLookup' and return whether or not a
-- reference is defined. If the given object is not of data type 'Dao.Object.ORef', then the
-- ordinary 'Dao.Object.Data.objToBool' function is used to evaluate the given object.
checkObjToBool :: Object -> Check Bool
checkObjToBool o = case o of
  ORef o -> fmap isJust $ execScriptToCheck objToMaybe (fmap (>>=objToMaybe) (readReference o))
  o      -> return (objToBool o)

-- | Retrieve a 'Dao.Types.CheckFunc' function from one of many possible places in the
-- 'Dao.Types.ExecUnit'. Every function call that occurs during execution of the Dao script will
-- use this Haskell function to seek the correct Dao function to use. Pass an error message to be
-- reported if the lookup fails. The order of lookup is: this module's 'Dao.Types.TopLevelFunc's,
-- the 'Dao.Types.TopLevelFunc's of each imported module (from first to last listed import), and
-- finally the built-in functions provided by the 'Dao.Types.Runtime'
lookupFunction :: String -> Name -> ExecScript CheckFunc
lookupFunction msg op = do
  xunit <- ask
  let toplevs xunit = do
        top <- execRun (fmap (M.lookup op) (dReadMVar xloc (toplevelFuncs xunit)))
        case top of
          Nothing  -> return Nothing
          Just top -> fmap Just (toplevelToCheckFunc top)
      lkup xunitMVar = execRun (dReadMVar xloc xunitMVar) >>= toplevs
  funcs <- sequence (toplevs xunit : map lkup (importsTable xunit))
  case msum $ funcs++[M.lookup op (builtinFuncs xunit)] of
    Nothing   -> objectError (OString op) $ "undefined "++msg++" ("++uchars op++")"
    Just func -> return func

----------------------------------------------------------------------------------------------------

-- $ErrorReporting
-- The 'ContErrT' is a continuation monad that can evaluate to an error message without evaluating
-- to "bottom". The error message is any value of type 'Dao.Types.Object'. These functions provide
-- a simplified method for constructing error 'Dao.Types.Object's.

simpleError :: String -> ExecScript a
simpleError msg = ceError (OString (ustr msg))

-- | Like 'Dao.Object.Data.objectError', but simply constructs a 'Dao.Object.Monad.CEError' value
-- that can be returned in an inner monad that has been lifted into a 'Dao.Object.Monad.ContErrT'
-- monad.
objectErrorCE :: Object -> String -> ContErr a
objectErrorCE obj msg = CEError (OPair (OString (ustr msg), obj))

typeError :: Object -> String -> String -> ExecScript a
typeError o cons expect = objectError (OType (objType o)) (cons++" must be of type "++expect)

derefError :: Reference -> ExecScript a
derefError ref = objectError (ORef ref) "undefined reference"

----------------------------------------------------------------------------------------------------

bindArgs_internal
  :: (n -> o -> ExecScript (Name, Object))
  -> [n] -> [o]
  -> ExecScript Object
  -> ExecScript Object
bindArgs_internal binder nx ox fn = loop (M.empty) nx ox >>= flip pushExecStack fn where
  loop ctx nx ox = case (nx, ox) of
    (n:nx, o:ox) -> binder n o >>= \ (n,o) -> loop (M.insert n o ctx) nx ox
    ([]  , []  ) -> return ctx
    ([]  , ox  ) -> simpleError "too many parameters passed to function"
    (nx  , []  ) -> simpleError $
      "not enough parameters, some function arguments could not be uninitialized"

-- | Set the current local variable context using a list of bindings and a list of parameters.
bindArgs :: [Name] -> [Object] -> ExecScript Object -> ExecScript Object
bindArgs = bindArgs_internal (\n o -> return (n, o))

-- | Like 'bindArgs', but evaluates every parameter.
bindArgsExpr :: [Com Name] -> [Com ObjectExpr] -> ExecScript Object -> ExecScript Object
bindArgsExpr = bindArgs_internal (\n o -> evalObject o >>= \o -> return (unComment n, o))

----------------------------------------------------------------------------------------------------

-- | Convert a single 'ScriptExpr' into a function of value @'ExecScript' 'Dao.Types.Object'@.
execScriptExpr :: Com ScriptExpr -> ExecScript ()
execScriptExpr script = case unComment script of
  NO_OP                        -> return ()
  EvalObject   o               -> unless (isNO_OP (unComment o)) (void (evalObject o))
  IfThenElse   ifn  thn  els   -> nestedExecStack M.empty $ do
    ifn <- evalObject ifn
    case ifn of
      ORef o -> do
        true <- fmap isJust (readReference o)
        execScriptBlock (if true then thn else els)
      o      -> execScriptBlock (if objToBool o then thn else els)
  TryCatch     try  name catch -> do
    ce <- withContErrSt (nestedExecStack M.empty (execScriptBlock try)) return
    void $ case ce of
      CEError o -> nestedExecStack (M.singleton (unComment name) o) (execScriptBlock catch)
      ce        -> returnContErr ce
  ForLoop    varName inObj thn -> nestedExecStack M.empty $ do
    inObj   <- evalObject inObj 
    let block thn = if null thn then return True else scrpExpr (head thn) >> block (tail thn)
        ctrlfn ifn com thn = do
          ifn <- evalObject ifn
          case ifn of
            ONull -> return (not thn)
            _     -> return thn
        scrpExpr expr = case unComment expr of
          ContinueExpr a ifn com -> ctrlfn ifn com (unComment a)
          _                      -> execScriptExpr expr >> return True
        loop thn name ix = case ix of
          []   -> return ()
          i:ix -> localVarDefine name i >> block (unComment thn) >>= flip when (loop thn name ix)
        inObjType = OType (objType inObj)
    checkToExecScript (ustr "for") inObjType (objToList inObj >>= checkOK) >>= loop thn (unComment varName)
  ContinueExpr a    _    _     -> simpleError $
    '"':(if unComment a then "continue" else "break")++"\" expression is not within a \"for\" loop"
  ReturnExpr   a    obj  com   -> evalObject obj >>= \obj ->
    (if unComment a then ceReturn else ceError) obj
  WithDoc      lval thn        -> nestedExecStack (M.empty) $ do
    lval <- evalObject lval
    let setBranch ref xunit = return (xunit{currentBranch = ref})
        setFile path xunit = do
          file <- execRun (fmap (M.lookup path) (dReadMVar xloc (execOpenFiles xunit)))
          case file of
            Nothing  -> ceError $ OList $ map OString $
              [ustr "with file path", path, ustr "file has not been loaded"]
            Just file -> return (xunit{currentDocument = Just file})
        run upd = ask >>= upd >>= \r -> local (const r) (execScriptBlock thn)
    case lval of -- TODO: change the type definition and parser for WithDoc such that it takes ONLY a Reference, not an Literal.
      ORef (GlobalRef ref)              -> run (setBranch ref)
      ORef (FileRef path [])            -> run (setFile path)
      ORef (FileRef path ref)           -> run (setFile path >=> setBranch ref)
      _ -> typeError lval "operand to \"with\" statement" $
             "file path (String type), or a Ref type, or a Pair of the two"

showObjType :: Object -> String
showObjType obj = showObj 0 (OType (objType obj))

-- | 'Dao.Types.ObjectExpr's can be evaluated anywhere in a 'Dao.Object.Script'. However, a
-- 'Dao.Types.ObjectExpr' is evaluated as a lone command expression, and not assigned to any
-- variables, and do not have any other side-effects, then evaluating an object is a no-op. This
-- function checks the kind of 'Dao.Types.ObjectExpr' and evaluates to 'True' if it is impossible
-- for an expression of this kind to produce any side effects. Otherwise, this function evaluates to
-- 'False', which indicates it is OK to evaluate the expression and disgard the resultant 'Object'.
isNO_OP :: ObjectExpr -> Bool
isNO_OP o = case o of
  Literal      _     -> True
  ParenExpr    o     -> isNO_OP (unComment o)
  ArraySubExpr _ _   -> True
  DictExpr     _ _   -> True
  ArrayExpr    _ _ _ -> True
  LambdaExpr   _ _ _ -> True
  _                  -> False

called_nonfunction_object :: String -> Object -> ExecScript e
called_nonfunction_object op obj =
  typeError obj (show op++" was called as a function but is not a function type, ") $
    show ScriptType++" or "++show RuleType

-- | Cache a single value so that multiple lookups of the given reference will always return the
-- same value, even if that value is modified by another thread between separate lookups of the same
-- reference value during evaluation of an 'Dao.Object.ObjectExpr'.
cacheReference :: Reference -> Maybe Object -> ExecScript ()
cacheReference r obj = case obj of
  Nothing  -> return ()
  Just obj -> ask >>= \xunit -> execRun $ dModifyMVar_ xloc (referenceCache xunit) $ \cache ->
    return (M.insert r obj cache)

-- | Evaluate an 'ObjectExpr' to an 'Dao.Types.Object' value, and does not de-reference objects of
-- type 'Dao.Types.ORef'
evalObject :: Com ObjectExpr -> ExecScript Object
evalObject obj = case unComment obj of
  Literal         o              -> return (unComment o)
  AssignExpr    nm  op  expr    -> do -- TODO, needs to be redefined to execute assigments or updates according to the 'op' param
    nm   <- evalObject nm
    case nm of
      ORef (MetaRef _) -> error "cannot assign to a reference-to-a-reference"
      ORef r           -> do
        obj <- updateReference r (\obj -> cacheReference r obj >> fmap Just (evalObject expr))
        case obj of
          Nothing  -> return ONull
          Just obj -> return obj
      _                -> objectError nm "left hand side of assignment does not evaluate to a reference value."
  FuncCall      op'  args'      -> do -- a built-in function call
    let op   = unComment op'
        args = unComment args'
    func <- localVarLookup op
    case func of
      Nothing -> do
        -- Find the name of the function in the built-in table and execute that one if it exists.
        -- NOTE: Built-in function calls do not get their own new stack, 'pushExecStack' is not
        -- used, only 'catchCEREturn'.
        fn <- lookupFunction "function call" op
        args <- mapM evalObject args
        let argTypes = OList (map (OType . objType) args)
        checkToExecScript op argTypes (checkFunc fn args)
      Just fn -> case fn of
        OScript o -> execScriptCall args o
        ORule   o -> execRuleCall   args o
        _ -> called_nonfunction_object (show op) fn
  LambdaCall    _    ref  args' -> do
    fn <- evalObject ref
    let args = unComment args'
    fn <- case fn of
      OScript _ -> return fn
      ORule   _ -> return fn
      _ -> called_nonfunction_object (showObjectExpr 0 ref) fn
    case fn of
      OScript o -> execScriptCall args o
      ORule   o -> execRuleCall   args o
  ParenExpr     o               -> evalObject o -- when in doubt, do not use 'evalObject'
  ArraySubExpr  o    i          -> evalArraySubscript o i
  Equation      left op   right -> do
    left  <- evalObject left
    right <- evalObject right
    bi    <- lookupFunction "operator" (unComment op)
    checkToExecScript (unComment op) (OPair (right, left)) (checkFunc bi [left, right])
  DictExpr      cons' args'     -> do
    let cons = unComment cons'
        args = unComment args'
        loop fn = forM args $ \arg -> do
            case unComment arg of
              AssignExpr a op b -> evalObject a >>= \a -> evalObject b >>= \b -> fn a b -- TODO: needs to update the dict expression according to the 'op' parameter
              _ -> simpleError $ show cons ++
                     " must be constructed from a comma-separated list of assignment expressions"
        intmap o x = do
          let ck o x = checkIntMapBounds o x >> return (fromIntegral x)
          o <- case o of
            OInt  x -> ck o x
            OWord x -> ck o x
            _       ->
              typeError o ("assigning to a value in an "++show IntMapType++" constructor") $
                show IntType++ " or "++show WordType
          return (o, x)
        dict   o x = case o of
          OString o -> return (o, x)
          _ ->
            typeError o ("assigning to a value in a "++show DictType++" constructor") $
              show StringType
    case () of
      () | cons == ustr "list"   -> fmap OList (mapM evalObject args)
      () | cons == ustr "dict"   -> fmap (ODict   . M.fromList) (loop dict)
      () | cons == ustr "intmap" -> fmap (OIntMap . I.fromList) (loop intmap)
      _ -> error ("INTERNAL ERROR: unknown dictionary declaration "++show cons)
  ArrayExpr     _    rang ox    -> do
    case unComment rang of
      (_ : _ : _ : _) ->
        simpleError "internal error: array range expression has more than 2 arguments"
      [lo, hi] -> do
        lo <- evalObject lo
        hi <- evalObject hi
        case (lo, hi) of
          (OInt lo, OInt hi) -> do
            (lo, hi) <- return (if lo<hi then (lo,hi) else (hi,lo))
            ox <- mapM evalObject (unComment ox)
            return (OArray (listArray (lo, hi) ox))
          _ -> objectError (OPair (OType (objType lo), OType (objType hi))) $
                   "range specified to an "++show ArrayType
                 ++" constructor must evaluate to two "++show IntType++"s"
      _ -> simpleError "internal error: array range expression has fewer than 2 arguments"
  LambdaExpr    _    argv code  -> return (OScript (Script{scriptArgv = argv, scriptCode = code}))

evalArraySubscript :: Com ObjectExpr -> Com ObjectExpr -> ExecScript Object
evalArraySubscript o i = evalObject o >>= \o -> case o of
  ORef (IntRef o) -> do
    let fn i =
          if o==0
            then evalIntRef (fromIntegral i)
            else do
              (OList mx) <- evalIntRef (fromIntegral o)
              ContErrT $ lift $ Control.Exception.catch (return . CENext $! mx!!fromIntegral i) $
                \ (ErrorCall _) -> return $ CEError $ OPair $
                    ( OPair (OInt (fromIntegral o), OInt (fromIntegral i))
                    , OString $ ustr $
                      "pattern match reference was indexed with a value out of bounds")
        ilkup i = case i of
          OInt  i -> fn i
          OWord i -> fn i
          OLong i -> fn i
          OList i -> fmap OList (mapM ilkup i)
          OSet  i -> fmap OList (mapM ilkup (S.elems i))
          _ -> indexTypeError "pattern match reference" i
    evalObject i >>= ilkup
  o -> do
    i <- evalObject i
    let ck i x fn = checkIntMapBounds i x >> fn (fromIntegral x)
        ilkup t i fn = case i of
          OInt  x -> ck i x fn
          OWord x -> ck i x fn
          OLong x -> ck i x fn
          OList x -> fmap OList (mapM (\i -> ilkup t i fn) x)
          OSet  x -> fmap OList (mapM (\i -> fmap (OPair . (,) i) (ilkup t i fn)) (S.elems x))
          _ -> indexTypeError (show t) i
        ixmsg t = objectError i ((show t)++" was accessed at an index that is out of bounds")
        mlkup o i lk = case i of
          OList   x -> fmap OList (mapM (\i -> mlkup o i lk) x)
          OSet    x -> fmap OList (mapM (\i -> fmap (OPair . ((,) i)) (mlkup o i lk)) (S.elems x))
          o         -> lk o
        mxmsg t u i = objectError i (show t++" must be indexed with items of type "++show u)
    case o of
      OList   o -> ilkup ListType i $ \x ->
        execIO (handle (\ (ErrorCall _) -> return ONull) (return $! (o!!x)))
      OArray  o -> ilkup ArrayType i $ \x ->
        if inRange (bounds o) x then return (o!x) else ixmsg ArrayType
      OIntMap m -> ilkup IntMapType i $ \x -> do
        checkIntMapBounds o x
        return (fromMaybe ONull (I.lookup (fromIntegral x) m))
      ODict   o -> mlkup o i $ \i -> case i of
        OString x -> return (fromMaybe ONull (M.lookup x o))
        _ -> mxmsg DictType StringType i
      OTree   o -> mlkup o i $ \i -> case i of
        ORef (GlobalRef x) -> return (fromMaybe ONull (T.lookup x o))
        _ -> mxmsg TreeType RefType i
      _ -> objectError o $
        "objects of type "++show (objType o)++" cannot be accessed by an index subscript"
  where
    indexTypeError t o = objectError (OType (objType o)) $
      t++" index must be a value of type "++show IntType++", "
        ++show WordType++", or "++show LongType

-- | Simply checks if an 'Prelude.Integer' is within the maximum bounds allowed by 'Data.Int.Int'
-- for 'Data.IntMap.IntMap'.
checkIntMapBounds :: Integral i => Object -> i -> ExecScript ()
checkIntMapBounds o i = do
  let (lo, hi) = (minBound::Int, maxBound::Int)
  unless (fromIntegral lo < i && i < fromIntegral hi) $
    objectError o $ show IntMapType++" index is beyond the limits allowd by this data type"

----------------------------------------------------------------------------------------------------

-- | Checks if this ExecUnit is allowed to use a set of built-in rules requested by an "require"
-- attribute. Returns any value you pass as the second parameter, throws a
-- 'Dao.Object.Monad.ceError' if it access is prohibited.
verifyRequirement :: Name -> a -> ExecScript a
verifyRequirement nm a = return a -- TODO: the rest of this function.

-- | Checks if this ExecUnit is allowed to import the file requested by an "import" statement
-- attribute. Returns any value you pass as the second parameter, throws a
-- 'Dao.Object.Monad.ceError'
verifyImport :: Name -> a -> ExecScript a
verifyImport nm a = return a -- TODO: the rest of this function.

-- | When the 'programFromSource' is scanning through a 'Dao.Types.SourceCode' object, it first
-- constructs an 'IntermediateProgram', which contains no 'Dao.Debug.DMVar's. Once all the data
-- structures are in place, a 'Dao.Types.CachedProgram' is constructed from this intermediate
-- representation.
data IntermediateProgram
  = IntermediateProgram
    { inmpg_programModuleName :: Name
    , inmpg_programImports    :: [UStr]
    , inmpg_constructScript   :: [Com [Com ScriptExpr]]
    , inmpg_destructScript    :: [Com [Com ScriptExpr]]
    , inmpg_requiredBuiltins  :: [Name]
    , inmpg_programAttributes :: M.Map Name Name
    , inmpg_preExecScript     :: [Com [Com ScriptExpr]]
    , inmpg_postExecScript    :: [Com [Com ScriptExpr]]
    , inmpg_programTokenizer  :: Tokenizer
    , inmpg_programComparator :: CompareToken
    , inmpg_ruleSet           :: PatternTree [Com [Com ScriptExpr]]
    , inmpg_globalData        :: T.Tree Name Object
    }

initIntermediateProgram =
  IntermediateProgram
  { inmpg_programModuleName = nil
  , inmpg_programImports    = []
  , inmpg_constructScript   = []
  , inmpg_destructScript    = []
  , inmpg_requiredBuiltins  = []
  , inmpg_programAttributes = M.empty
  , inmpg_preExecScript     = []
  , inmpg_postExecScript    = []
  , inmpg_programTokenizer  = return . tokens . uchars
  , inmpg_programComparator = (==)
  , inmpg_ruleSet           = T.Void
  , inmpg_globalData        = T.Void
  }

initProgram :: IntermediateProgram -> TreeResource -> ExecScript Program
initProgram inmpg initGlobalData = do
  rules <- execScriptRun $
    T.mapLeavesM (mapM setupExecutable) (inmpg_ruleSet inmpg) >>= dNewMVar xloc "Program.ruleSet"
  pre   <- execRun (mapM setupExecutable (inmpg_preExecScript  inmpg))
  post  <- execRun (mapM setupExecutable (inmpg_postExecScript inmpg))
  inEvalDoModifyUnlocked_ initGlobalData (return . const (inmpg_globalData inmpg))
  return $
    Program
    { programModuleName = inmpg_programModuleName inmpg
    , programImports    = inmpg_programImports    inmpg
    , constructScript   = map unComment (inmpg_constructScript inmpg)
    , destructScript    = map unComment (inmpg_destructScript  inmpg)
    , requiredBuiltins  = inmpg_requiredBuiltins  inmpg
    , programAttributes = M.empty
    , preExecScript     = pre
    , programTokenizer  = return . tokens . uchars
    , programComparator = (==)
    , postExecScript    = post
    , ruleSet           = rules
    , globalData        = initGlobalData
    }

-- | To parse a program, use 'Dao.Object.Parsers.source' and pass the resulting
-- 'Dao.Object.SourceCode' object to this funtion. It is in the 'ExecScript' monad because it needs
-- to evaluate 'Dao.ObjectObject's defined in the top-level of the source code, which requires
-- 'evalObject'.
-- Attributes in Dao scripts are of the form:
--   a.b.C.like.name  dot.separated.value;
-- The three built-in attributes are "requires", "string.tokenizer" and "string.compare". The
-- allowed attrubites can be extended by passing a call-back predicate which modifies the given
-- program, or returns Nothing to reject the program. If you are not sure what to pass, just pass
-- @(\ _ _ _ -> return Nothing)@ which always rejects the program. This predicate will only be
-- called if the attribute is not allowed by the minimal Dao system.
programFromSource
  :: TreeResource
      -- ^ the global variables initialized at the top level of the program file are stored here.
  -> (Name -> UStr -> IntermediateProgram -> ExecScript Bool)
      -- ^ a callback to check attributes written into the script. If the attribute is bogus, Return
      -- False to throw a generic error, or throw your own CEError. Otherwise, return True.
  -> SourceCode
      -- ^ the script file to use
  -> ExecScript Program
programFromSource globalResource checkAttribute script = do
  interm <- execStateT (mapM_ foldDirectives (unComment (directives script))) initIntermediateProgram
  initProgram interm globalResource
  where
    err lst = lift $ ceError $ OList $ map OString $ (sourceFullPath script : lst)
    attrib req nm getRuntime putProg = do
      runtime <- lift $ fmap parentRuntime ask
      let item = M.lookup nm (getRuntime runtime)
      case item of
        Just item -> modify (putProg item)
        Nothing   -> err [req, ustr "attribute", nm, ustr "is not available"]
    foldDirectives directive = case unComment directive of
      Attribute  req nm -> ask >>= \xunit -> do
        let setName = unComment nm
            runtime = parentRuntime xunit
            builtins = M.lookup setName $ functionSets runtime
        case unComment req of
          req | req==ustr "import"           -> do
            lift $ verifyImport setName () -- TODO: verifyImport will evaluate to a CEError if the import fails.
            modify (\p -> p{inmpg_programImports = inmpg_programImports p ++ [setName]})
          req | req==ustr "require"          -> case builtins of
            Just  _ -> do
              lift $ verifyRequirement setName ()
              modify (\p -> p{inmpg_requiredBuiltins = inmpg_requiredBuiltins p++[setName]})
            Nothing -> err $
              [ustr "requires", setName, ustr "not provided by this version of the Dao system"]
          req | req==ustr "string.tokenizer" ->
            attrib req setName availableTokenizers (\item p -> p{inmpg_programTokenizer = item})
          req | req==ustr "string.compare"   ->
            attrib req setName availableComparators (\item p -> p{inmpg_programComparator = item})
          req -> do
            p  <- get
            ok <- lift (checkAttribute req setName p)
            if ok
              then return ()
              else err [ustr "script contains unknown attribute declaration", req]
      ToplevelDefine name obj -> do
        obj <- lift $ evalObject obj
        modify (\p -> p{inmpg_globalData = T.insert (unComment name) obj (inmpg_globalData p)})
      RuleExpr    rule' -> modify (\p -> p{inmpg_ruleSet = foldl fol (inmpg_ruleSet p) rulePat}) where
        rule    = unComment rule'
        rulePat = map unComment (unComment (rulePattern rule))
        fol tre pat = T.merge T.union (++) tre (toTree pat [ruleAction rule])
      SetupExpr    scrp -> modify (\p -> p{inmpg_constructScript = inmpg_constructScript p ++ [scrp]})
      TakedownExpr scrp -> modify (\p -> p{inmpg_destructScript  = inmpg_destructScript  p ++ [scrp]})
      BeginExpr    scrp -> modify (\p -> p{inmpg_preExecScript   = inmpg_preExecScript   p ++ [scrp]})
      EndExpr      scrp -> modify (\p -> p{inmpg_postExecScript  = inmpg_postExecScript  p ++ [scrp]})
      ToplevelFunc _ nm argv code -> lift $ do
        xunit <- ask
        let func objx = execScriptCall (map (Com . Literal . Com) objx) $
              Script{scriptArgv = argv, scriptCode = code}
        execScriptRun $ do
          let name = unComment nm
          func <- dNewMVar xloc ("Program.topLevelFunc("++uchars name++")") func :: Run TopLevelFunc
          dModifyMVar_ xloc (toplevelFuncs xunit) (return . M.insert name func)

