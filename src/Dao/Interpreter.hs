-- "src/Dao/Interpreter.hs"  declares the "Object" data type which is the
-- fundamental data type used througout the Dao System.
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

{-# LANGUAGE CPP #-}

module Dao.Interpreter(
    DaoSetupM(), DaoSetup, haskellType, daoProvides, daoClass, daoConstant, daoFunction,
    daoInitialize, setupDao, DaoFunc, daoFunc, autoDerefParams, daoForeignCall, executeDaoFunc,
    evalFuncs,
    ObjectClass(obj, fromObj, castToCoreType),
    execCastToCoreType, listToObj, listFromObj, new, opaque, objFromHaskellData,
    Struct(Nullary, Struct),
    ToDaoStructClass(toDaoStruct), FromDaoStructClass(fromDaoStruct),
    StructError(StructError),
    structErrMsg, structErrName, structErrField, structErrValue, structErrExtras,
    mkLabel, mkStructName, mkFieldName,
    ToDaoStruct(),
    fromData, innerToStruct, innerToStructWith, renameConstructor, makeNullary, putNullaryUsingShow,
    define, optionalField, setField, defObjField, (.=), putObjField, (.=@), defMaybeObjField, (.=?),
    FromDaoStruct(),
    toData, constructor, innerFromStruct, nullary, getNullaryWithRead, structCurrentField,
    tryCopyField, tryField, copyField, field, checkEmpty,
    convertFieldData, req, opt, reqList, optList, 
    Object(
      ONull, OTrue, OType, OInt, OWord, OLong,
      OFloat, ORatio, OComplex, OAbsTime, ORelTime,
      OChar, OString, ORef, OList, ODict, OTree, OBytes, OHaskell
    ),
    T_type, T_int, T_word, T_long, T_ratio, T_complex, T_float, T_time, T_diffTime,
    T_char, T_string, T_ref, T_bytes, T_list, T_dict, T_struct,
    isNumeric, typeMismatchError,
    initializeGlobalKey, destroyGlobalKey,
    RefQualifier(UNQUAL, LOCAL, CONST, STATIC, GLOBAL, GLODOT),
    Reference(Reference, RefObject), reference, refObject,
    refNames, referenceFromUStr, fmapReference, setQualifier, modRefObject,
    refAppendSuffix, referenceLookup, referenceUpdate,
    CoreType(
      NullType, TrueType, TypeType, IntType, WordType, DiffTimeType, FloatType,
      LongType, RatioType, ComplexType, TimeType, CharType, StringType, RefType,
      ListType, DictType, TreeType, BytesType, HaskellType
    ),
    typeOfObj, coreType,
    TypeSym(CoreType, TypeVar), TypeStruct(TypeStruct), ObjType(ObjType), typeChoices,
    RefSuffix(NullRef, DotRef, Subscript, FuncCall),
    dotRef, subscript, funcCall,
    Complex(Complex),
    realPart, imagPart, mkPolar, cis, polar, magnitude, phase, conjugate, complex,
    minAccumArray, minArray,
    FuzzyStr(FuzzyStr),
    StaticStore(StaticStore),
    ExecUnit(),
    globalMethodTable, pathIndex, defaultTimeout, importGraph, currentWithRef, taskForExecUnits,
    currentQuery, currentPattern, currentBranch, providedAttributes, programModuleName,
    preExec, postExec, quittingTime, programTokenizer, currentCodeBlock, ruleSet,
    newExecUnit,
    Task(), initTask, throwToTask, killTask, taskLoop, taskLoop_,
    Executable(execute), RefReducible(reduceToRef),
    ExecRef(execReadRef, execTakeRef, execPutRef, execSwapRef, execModifyRef, execModifyRef_),
    Store(storeLookup, storeUpdate, storeDefine, storeDelete),
    objectReferenceAccess, objectReferenceUpdate, 
    ExecControl(ExecReturn, ExecError), execReturnValue, execErrorInfo,
    ExecErrorInfo(ExecErrorInfo), execUnitAtError, execErrExpr, execErrScript, execErrTopLevel,
    mkExecErrorInfo, mkExecError, updateExecErrorInfo, setCtrlReturnValue,
    logUncaughtErrors, getUncaughtErrorLog, clearUncaughtErrorLog,
    Exec(Exec), execToPredicate, XPure(XPure), xpureToState, runXPure, evalXPure, xpure, xobj, xnote, xonUTF8, xmaybe,
    ExecThrowable(toExecError, execThrow), ioExec,
    ExecHandler(ExecHandler), execHandler,
    newExecIOHandler, execCatchIO, execHandleIO, execIOHandler, execErrorHandler, catchReturn,
    execNested, execFuncPushStack,
    Intermediate(toInterm, fromInterm), pPrintInterm, putAST, getAST,
    Canonical(canonicalize),
    Comment(InlineComment, EndlineComment), commentString,
    Com(Com, ComBefore, ComAfter, ComAround),
    pPrintComWith, pListOfComsWith, pListOfComs, randComWith, appendComments, com,
    setCommentBefore, setCommentAfter, unComment, getComment, 
    CodeBlock(CodeBlock), codeBlock, setupCodeBlock,
    Subroutine(Subroutine),
    origSourceCode, staticVars, staticRules, staticLambdas, executable, runCodeBlock,
    CallableCode(CallableCode), argsPattern, returnType, codeSubroutine, 
    GlobAction(GlobAction), globPattern, globSubroutine, 
    AST_CodeBlock(AST_CodeBlock), getAST_CodeBlock, 
    TyChkExpr(NotTypeChecked, TypeChecked, DisableCheck),
    tyChkItem, tyChkExpr, tyChkLoc, typChkResult, checkedExpr,
    AST_TyChk(AST_NotChecked, AST_Checked), checkedAST,
    ParamExpr(ParamExpr), 
    AST_Param(AST_NoParams, AST_Param), 
    ParamListExpr(ParamListExpr), getTypeCheckList, paramsToGlobExpr,
    AST_ParamList(AST_ParamList), 
    RuleHeadExpr(RuleStringExpr, RuleHeadExpr), 
    AST_RuleHeader(AST_NullRules, AST_RuleString, AST_RuleHeader), 
    asReference, asInteger, asRational, asPositive, asComplex, objConcat,
    objToBool, extractStringElems,
    requireAllStringArgs, getStringsToDepth, derefStringsToDepth, recurseGetAllStrings, 
    shiftLeft, shiftRight,
    UpdateOp(UCONST, UADD, USUB, UMULT, UDIV, UMOD, UPOW, UORB, UANDB, UXORB, USHL, USHR), 
    RefPfxOp(REF, DEREF), 
    ArithPfxOp(INVB, NOT, NEGTIV, POSTIV), 
    InfixOp(
      ADD, SUB, MULT, DIV, MOD, POW, ORB, ANDB, XORB, SHL, SHR, OR, AND,
      EQUL, NEQUL, GTN, LTN, GTEQ, LTEQ, ARROW
    ),
    allUpdateOpStrs, allPrefixOpChars, allPrefixOpStrs, allInfixOpChars, allInfixOpStrs,
    TopLevelEventType(BeginExprType, EndExprType, ExitExprType), 
    evalArithPrefixOp, evalInfixOp, evalUpdateOp, 
    RefSuffixExpr(NullRefExpr, DotRefExpr, SubscriptExpr, FuncCallExpr),
    ReferenceExpr(ReferenceExpr, RefObjectExpr),
    matchFuncParams, execGuardBlock, objToCallable, callCallables,
    callObject, checkPredicate, checkVoid,
    AST_RefSuffix(AST_RefNull, AST_DotRef, AST_Subscript, AST_FuncCall),
    AST_Reference(AST_Reference, AST_RefObject),
    ParenExpr(ParenExpr), evalConditional, AST_Paren(AST_Paren), 
    IfExpr(IfExpr), AST_If(AST_If), 
    ElseExpr(ElseExpr), AST_Else(AST_Else), 
    IfElseExpr(IfElseExpr), AST_IfElse(AST_IfElse), 
    WhileExpr(WhileExpr), AST_While(AST_While), 
    ScriptExpr(
      IfThenElse, WhileLoop, RuleFuncExpr, EvalObject,
      TryCatch, ForLoop, ContinueExpr, ReturnExpr, WithDoc
    ),
    localVarDefine, maybeDerefObject, derefObjectGetReference, derefObject,
    AST_Script(
      AST_Comment, AST_IfThenElse, AST_WhileLoop, AST_RuleFunc, AST_EvalObject,
      AST_TryCatch, AST_ForLoop, AST_ContinueExpr, AST_ReturnExpr, AST_WithDoc
    ),
    ObjListExpr(ObjListExpr), AST_ObjList(AST_ObjList),
    OptObjListExpr(OptObjListExpr), updateExecError, AST_OptObjList(AST_OptObjList),
    LiteralExpr(LiteralExpr), AST_Literal(AST_Literal),
    DotNameExpr(DotNameExpr), AST_DotName(AST_DotName), getDotNameAST, undotNameExpr,
    DotLabelExpr(DotLabelExpr), dotLabelToRefExpr, refToDotLabelExpr, dotLabelToNameList,
    AST_DotLabel(AST_DotLabel), dotLabelToRefAST, refToDotLabelAST,
    RefPrefixExpr(PlainRefExpr, RefPrefixExpr), cleanupRefPrefixExpr,
    AST_RefPrefix(AST_RefPrefix, AST_PlainRef),
    RuleFuncExpr(LambdaExpr, FuncExpr, RuleExpr), AST_RuleFunc(AST_Lambda, AST_Func, AST_Rule),
    ObjectExpr(
      VoidExpr, ObjLiteralExpr, ObjSingleExpr, ObjRuleFuncExpr,
      ArithPfxExpr, InitExpr, StructExpr, MetaEvalExpr
    ), objectIndexAccess, objectIndexUpdate,
    AST_Object(
      AST_Void, AST_ObjLiteral, AST_ObjSingle, AST_ObjRuleFunc,
      AST_ArithPfx, AST_Init, AST_Struct, AST_MetaEval
    ),
    ArithExpr(ObjectExpr, ArithExpr),
    AST_Arith(AST_Object, AST_Arith), 
    AssignExpr(EvalExpr, AssignExpr),
    AST_Assign(AST_Eval, AST_Assign), assignUnqualifiedOnly,
    ObjTestExpr(ObjArithExpr, ObjTestExpr),
    AST_ObjTest(AST_ObjArith, AST_ObjTest),
    TopLevelExpr(RequireExpr, ImportExpr, TopScript, EventExpr), isAttribute, 
    AST_TopLevel(AST_Require, AST_Import, AST_TopScript, AST_Event, AST_TopComment),
    isAST_Attribute, getRequiresAndImports,
    AttributeExpr(AttribDotNameExpr, AttribStringExpr),
    AST_Attribute(AST_AttribDotName, AST_AttribString),
    NamespaceExpr(NamespaceExpr), AST_Namespace(AST_NoNamespace, AST_Namespace),
    Program(Program), topLevelExprs, program_magic_number, 
    AST_SourceCode(AST_SourceCode), sourceModified, sourceFullPath, directives, 
    MethodTable(), execGetObjTable, lookupMethodTable, typeRepToUStr,
    ReadIterable(readIter, readForLoop), readForLoopWith,
    UpdateIterable(updateIter, updateForLoop), updateForLoopWith,
    HaskellDataClass(haskellDataInterface), toHaskellData, fromHaskellData,
    Interface(),
    objCastFrom, objEquality, objOrdering, objBinaryFormat, objNullTest, objPPrinter,
    objSizer, objIndexer, objIndexUpdater, objToStruct, objFromStruct, objDictInit, objListInit,
    objInfixOpTable, objArithPfxOpTable, objCallable, objDereferencer,
    interfaceAdapter, interfaceToDynamic,
    DaoClassDefM(), interface, DaoClassDef,
    defCastFrom, autoDefEquality, defEquality, autoDefOrdering, defOrdering, autoDefBinaryFmt,
    defBinaryFmt, autoDefNullTest, defNullTest, defPPrinter, autoDefPPrinter, defReadIterable,
    autoDefReadIterable, defUpdateIterable, autoDefUpdateIterable, defIndexer, defIndexUpdater,
    defSizer, autoDefToStruct, defToStruct, autoDefFromStruct, defFromStruct, defDictInit,
    defListInit, defInfixOp, defPrefixOp, defCallable, defDeref, defLeppard
  )
  where

import           Dao.Glob
import           Dao.PPrint
import           Dao.Predicate
import           Dao.Procedural
import           Dao.Random
import           Dao.Stack
import           Dao.String
import           Dao.Token
import           Dao.RefTable
import qualified Dao.HashMap as H
import qualified Dao.Binary  as B
import qualified Dao.EnumSet as Es
import qualified Dao.Tree    as T

import           Data.Array.IArray
import           Data.Binary (encode)
import           Data.Bits
import           Data.Char
import           Data.Dynamic
import           Data.IORef
import           Data.List
import           Data.Monoid
import           Data.Ratio
import           Data.Time.Clock
import           Data.Word
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy      as B
import qualified Data.Binary               as D
import qualified Data.Binary.Put           as D
import qualified Data.Binary.Get           as D
import qualified Data.Complex              as C
import qualified Data.Map                  as M
import qualified Data.Set                  as S

import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State

import           System.IO

#if 0
import Debug.Trace
strace :: PPrintable s => String -> s -> s
strace msg s = trace (msg++": "++prettyShow s) s
dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr . ("(DEBUG) "++) . (>>=(\c -> if c=='\n' then "\n(DEBUG) " else [c]))
dbg' :: MonadIO m => String -> m a -> m a
dbg' msg f = f >>= \a -> dbg msg >> return a
dbg0 :: (MonadPlus m, MonadIO m, MonadError e m) => String -> m a -> m a
dbg0 msg f = do
  dbg (msg++" (BEGIN)")
  catchError
    (mplus (f >>= \a -> dbg (msg++" (DONE)") >> return a) (dbg (msg++" (BACKTRACKED)") >> mzero))
    (\e -> dbg (msg++" (ERROR)") >> throwError e)
updbg :: MonadIO m => String -> (Maybe Object -> m (Maybe Object)) -> Maybe Object -> m (Maybe Object)
updbg msg f o = dbg ("(update with "++msg++")") >> f o >>= \o -> dbg ("(update complete "++show o++")") >> return o
#endif
#if 0
_randTrace :: String -> RandO a -> RandO a
_randTrace = Dao.Random.randTrace
#else
_randTrace :: String -> RandO a -> RandO a
_randTrace _ = id
#endif

----------------------------------------------------------------------------------------------------

-- A note on the binary format.
--     Most constructors have a unique prefix byte, and this allows more efficient encoding because
-- it is not necessary to place null terminators everywhere and you can determine exactly which
-- constructor is under the decoder cursor just from the byte prefix. This means there is an
-- address space for prefix bytes between 0x00 and 0xFF. This is an overview of that address space.
-- 
-- 0x00..0x07 > The "Dao.Binary" module declares a few unique prefixes of its own for booleans,
--              variable-length integers, and maybe types, and of course the null terminator.
-- 0x08..0x1A > Each prefix here used alone indicates a 'CoreType's. But each prefix may be followed
--              by data which indicates that it is actuall one of the constructors for the 'Object'
--              data type.
-- 
-- 0x25..0x26 'Struct'
-- 0x2E..0x2F 'TypeSym'
-- 0x33       'TypeStruct'
-- 0x37       'ObjType' (T_type)
-- 
-- 0x42..0x45 > The 'RefSuffix' data type. These prefixes are re-used for the 'ReferenceExpr' data type
--              because there is a one-to-one mapping between these two data types.
-- 0x48..0x4F > The 'Reference' data type. These prefixes are re-used for the 'ReferenceExpr' data type
--              execpt for the 'RefWrapper' constructor which is mapped to @'RefPrefixExpr' 'REF'@.
-- 
-- -- the abstract syntax tree -- --
-- 
-- 0x52..0x53 'RefPrefixExpr'
-- 0x59       'ParenExpr'
-- 0x60..0x64 'ObjectExpr'
-- 0x6A       'ArithExpr'
-- 0x6F       'AssignExpr'
-- 0x73       'ObjTestExpr'
-- 0x74..0x76 'RuleFuncExpr'
-- 0x7A..0x7B 'RuleHeadExpr'
-- 0x81       'DotLabelExpr'
-- 0x82       'AttributeExpr'
-- 0x86       'ObjListExpr'
-- 0xBA..0xCD 'InfixOp'
-- 0x8D..0x9D 'UpdateOp' -- Partially overlaps with 'InfixOp'
-- 0x8E..0x9B 'ArithPfxOp' -- Partially overlaps with 'InfixOp'
-- 0xA8..0xAF 'ScriptExpr'
-- 0xB6       'ElseExpr'
-- 0xBA       'IfElseExpr'
-- 0xBE       'WhileExpr'
-- 0xC5..0xC7 'TyChkExpr'
-- 0xCF..0xD0 'ParamExpr'
-- 0xD6       'ParamListExpr'
-- 0xDD       'CodeBlock'
-- 0xE9..0xEE 'TopLevelExpr'

----------------------------------------------------------------------------------------------------

-- The stateful data for the 'DaoSetup' monad.
data SetupModState
  = SetupModState
    { daoSatisfies      :: M.Map UStr ()
      -- ^ a set of references that can satisfy "required" statements in Dao scripts.
    , daoSetupConstants :: M.Map Name Object
    , daoClasses        :: MethodTable
    , daoEntryPoint     :: Exec ()
    }

-- | This monadic type allows you to define a built-in module using procedural
-- programming syntax. Simply define each addition to the module one line at a time. Functions that
-- you can use include 'modProvides', 'modFunction', 'daoClass', and 'daoInitalize'.
-- 
-- Define clever names for every 'DaoSetup' you write, then 
type DaoSetup = DaoSetupM ()
newtype DaoSetupM a = DaoSetup{ daoSetupM :: State SetupModState a }
  deriving (Functor, Applicative, Monad)

-- | This function is a placeholder used by the type system. The value of this function is
-- undefined, so strictly evaluating it will throw an exception. Fortunately, the only time you will
-- ever use this function is with the 'daoClass' function, which uses the type of this function but
-- never it's value. Refer to the documentation on 'daoClass' to see how to properly use this
-- function.
haskellType :: HaskellDataClass o => o
haskellType = error $ unwords $
  [ "the haskellType function is just a placeholder"
  , "used by the type system, it must not be evaluated."
  ]

_updateSetupModState :: (SetupModState -> SetupModState) -> DaoSetup
_updateSetupModState f = DaoSetup (modify f)

-- | Dao programs can declare "requires" statements along with it's imports. If your built-in module
-- provides what Dao programs might "required", then declare that this module provides that feature
-- using this function.
daoProvides :: UStrType s => s -> DaoSetup
daoProvides label = _updateSetupModState $ \st ->
  st{ daoSatisfies = M.insert (toUStr label) () $ daoSatisfies st }

-- | Associate an 'HaskellDataClass' with a 'Name'. This 'Name' will be callable from within Dao scripts.
-- > newtype MyClass = MyClass { ... } deriving (Eq, Ord)
-- >
-- > instance 'HaskellDataClass' MyClass where
-- >     'haskellDataInterface' = 'interface' $ do
-- >         'autoDefEquality'
-- >         'autoDefOrdering'
-- >         ...
-- >
-- > setupDao :: 'DaoSetup'
-- > setupDao = do
-- >     daoClass "myClass" (haskellType::MyClass)
-- >     ...
daoClass :: (UStrType name, Typeable o, HaskellDataClass o) => name -> o -> DaoSetup
daoClass name ~o = _updateSetupModState $ \st ->
    st{ daoClasses = _insertMethodTable o (fromUStr $ toUStr name) haskellDataInterface (daoClasses st) }

-- | Define a built-in function. Examples of built-in functions provided in this module are
-- "print()", "join()", and "exec()".
daoFunction :: (Show name, UStrType name) => name -> DaoFunc -> DaoSetup
daoFunction name func = _updateSetupModState $ \st -> let nm = (fromUStr $ toUStr name) in
  st{ daoSetupConstants = M.insert nm (new $ func{ daoFuncName=nm }) (daoSetupConstants st) }

daoConstant :: (Show name, UStrType name) => name -> Object -> DaoSetup
daoConstant name o = _updateSetupModState $ \st ->
  st{ daoSetupConstants = M.insert (fromUStr $ toUStr name) o (daoSetupConstants st) }

-- | Provide an 'Exec' monad to perform when 'setupDao' is evaluated. You may use this function as
-- many times as you wish, every 'Exec' monad will be executed in the order they are specified. This
-- is a good way to create a read-eval-print loop.
daoInitialize :: Exec () -> DaoSetup
daoInitialize f = _updateSetupModState $ \st -> st{ daoEntryPoint = daoEntryPoint st >> f }

-- | Use this function evaluate a 'DaoSetup' in the IO () monad. Use this to define the 'main'
-- function of your program.
setupDao :: DaoSetup -> IO ()
setupDao setup0 = do
  let setup = execState (daoSetupM setup0) $
        SetupModState
        { daoSatisfies      = M.empty
        , daoSetupConstants = M.empty
        , daoClasses        = mempty
        , daoEntryPoint     = return ()
        }
  xunit  <- _initExecUnit
  result <- ioExec (daoEntryPoint setup) $
    xunit
    { providedAttributes = daoSatisfies setup
    , builtinConstants   = daoSetupConstants setup
    , globalMethodTable  = daoClasses setup
    }
  case result of
    OK    ()                -> return ()
    PFail (ExecReturn  obj) -> maybe (return ()) (putStrLn . prettyShow) obj
    PFail (err@ExecError{}) -> hPutStrLn stderr (prettyShow err)
    Backtrack               -> hPutStrLn stderr "(does not compute)"

----------------------------------------------------------------------------------------------------

-- | All functions that are built-in to the Dao language, or built-in to a library extending the Dao
-- language, are stored in 'Data.Map.Map's from the functions name to an object of this type.
-- Functions of this type are called by 'evalObject' to evaluate expressions written in the Dao
-- language.
data DaoFunc
  = DaoFunc
    { daoFuncName     :: Name
    , autoDerefParams :: Bool
    , daoForeignCall  :: [Object] -> Exec (Maybe Object)
    }
  deriving Typeable
instance Eq   DaoFunc where { a == b = daoFuncName a == daoFuncName b; }
instance Ord  DaoFunc where { compare a b = compare (daoFuncName a) (daoFuncName b) }
instance Show DaoFunc where { show = show . daoFuncName }
instance PPrintable DaoFunc where { pPrint = pShow }

-- | Use this as the constructor of a 'DaoFunc'.
daoFunc :: DaoFunc
daoFunc = DaoFunc{ daoFuncName=nil, autoDerefParams=True, daoForeignCall = \_ -> return Nothing }

-- | Execute a 'DaoFunc' 
executeDaoFunc :: Maybe Reference -> DaoFunc -> [Object] -> Exec (Maybe Object)
executeDaoFunc qref fn params = do
  args <- (if autoDerefParams fn then mapM derefObject else return) params
  pval <- catchPredicate (daoForeignCall fn args)
  case pval of
    OK                 obj  -> return obj
    PFail (ExecReturn  obj) -> return obj
    PFail (err@ExecError{}) -> throwError $ case qref of
      Nothing -> err
      Just qref -> err{
        execReturnValue = let e = [obj "error in function", obj qref] in Just $
          flip (maybe (obj e)) (execReturnValue err) $ \ret -> OList $ case ret of
            OList ox -> e++ox
            o        -> e++[o] }
    Backtrack               -> mzero

-- | Evaluate this function as one of the instructions in the monadic function passed to the
-- 'setupDao' function in order to install the most fundamental functions into the Dao evaluator.
-- This function must be evaluated in order to have access to the following functions:
-- > print, join, defined, delete
evalFuncs :: DaoSetup
evalFuncs = do
  daoClass "HashMap" (haskellType :: H.HashMap Object Object)
  daoFunction "print"    builtin_print
  daoFunction "println"  builtin_println
  daoFunction "join"     builtin_join
  daoFunction "str"      builtin_str
  daoFunction "quote"    builtin_quote
  daoFunction "concat"   builtin_concat
  daoFunction "concat1"  builtin_concat1
  daoFunction "int"      builtin_int
  daoFunction "long"     builtin_long
  daoFunction "ratio"    builtin_ratio
  daoFunction "float"    builtin_float
  daoFunction "complex"  builtin_complex
  daoFunction "imag"     builtin_imag
  daoFunction "phase"    builtin_phase
  daoFunction "conj"     builtin_conj
  daoFunction "abs"      builtin_abs
  daoFunction "time"     builtin_time
  daoFunction "now"      builtin_now
  daoFunction "defined"  builtin_check_if_defined
  daoFunction "delete"   builtin_delete
  daoFunction "typeof"   builtin_typeof
  daoFunction "sizeof"   builtin_sizeof
  mapM_ (uncurry daoConstant) $ map (\t -> (toUStr (show t), OType (coreType t))) [minBound..maxBound]

instance ObjectClass DaoFunc where { obj=new; fromObj=objFromHaskellData }

instance HaskellDataClass DaoFunc where
  haskellDataInterface = interface daoFunc $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

-- | This class provides a consistent interface, the 'obj' function, for converting a wide range of
-- types to an 'Object' type.
class ObjectClass o where
  obj   :: o -> Object
  fromObj :: Object -> Maybe o
  castToCoreType :: CoreType -> o -> XPure Object
  castToCoreType _ _ = mzero

execCastToCoreType :: ObjectClass o => CoreType -> o -> Exec Object
execCastToCoreType t = execute . castToCoreType t

instance ObjectClass () where
  obj () = ONull
  fromObj o = case o of { ONull -> return (); _ -> mzero; }
  castToCoreType t () = case t of
    NullType     -> return ONull
    CharType     -> return $ OChar '\0'
    IntType      -> return $ OInt 0
    WordType     -> return $ OWord 0
    LongType     -> return $ OLong 0
    DiffTimeType -> return $ ORelTime 0
    FloatType    -> return $ OFloat 0
    RatioType    -> return $ ORatio 0
    ComplexType  -> return $ OComplex $ complex 0 0
    StringType   -> return $ OString nil
    BytesType    -> return $ OBytes mempty
    ListType     -> return $ OList []
    DictType     -> return $ ODict mempty
    _            -> mzero

instance ObjectClass Bool where
  obj true = if true then OTrue else ONull
  fromObj o = case o of { OTrue -> return True; ONull -> return False; _ -> mzero }
  castToCoreType t o = case t of
    NullType     -> guard (not o) >> return ONull
    TrueType     -> guard o >> return OTrue
    CharType     -> return $ OChar    $ if o then '1' else '0'
    IntType      -> return $ OInt     $ if o then 1 else 0
    WordType     -> return $ OWord    $ if o then 1 else 0
    LongType     -> return $ OLong    $ if o then 1 else 0
    DiffTimeType -> return $ ORelTime $ if o then 1 else 0
    FloatType    -> return $ OFloat   $ if o then 1 else 0
    RatioType    -> return $ ORatio   $ if o then 1 else 0
    ComplexType  -> return $ OComplex $ if o then complex 1 0 else complex 0 0
    StringType   -> return $ obj      $ if o then "true" else "false"
    BytesType    -> return $ OBytes $ B.pack $ return $ if o then 1 else 0
    _            -> mzero

instance ObjectClass Char where
  obj = OChar
  fromObj o = case o of { OChar o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (o=='\0') >> return ONull
    TrueType     -> \o -> case o of
      '0' -> return ONull
      '1' -> return OTrue
      _   -> mzero
    CharType     -> return . OChar
    IntType      -> return . OInt     . ord
    WordType     -> return . OWord    . fromIntegral . ord
    LongType     -> return . OLong    . fromIntegral . ord
    DiffTimeType -> return . ORelTime . fromRational . toRational . ord
    FloatType    -> return . OFloat   . fromRational . toRational . ord
    RatioType    -> return . ORatio   . toRational   . ord
    ComplexType  -> return . OComplex . flip complex 0 . fromRational . toRational . ord
    StringType   -> return . obj      . (:[])
    BytesType    -> return . OBytes . D.runPut . D.putWord64le . fromIntegral . ord
    _            -> \ _ -> mzero

charFromIntegral :: (MonadPlus m, Integral i) => i -> m Char
charFromIntegral i0 =
  let i = fromIntegral i0
  in if ord(minBound::Char) <= i && i <= ord(maxBound::Char) then return (chr i) else mzero

instance ObjectClass Int where
  obj = OInt
  fromObj o = case o of { OInt o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (o==0) >> return ONull
    TrueType     -> \o -> return $ if o==0 then ONull else OTrue
    CharType     -> fmap OChar . charFromIntegral
    IntType      -> return . OInt
    WordType     -> return . OWord    . fromIntegral
    LongType     -> return . OLong    . toInteger
    FloatType    -> return . OFloat   . fromRational   . toRational
    RatioType    -> return . ORatio   . toRational
    ComplexType  -> return . OComplex . flip complex 0 . fromRational . toRational
    DiffTimeType -> return . ORelTime . fromRational   . toRational
    StringType   -> return . obj      . prettyShow     . obj
    BytesType    -> return . OBytes . D.runPut . D.putWord64le . fromIntegral
    _            -> \ _ -> mzero

instance ObjectClass Word where
  obj = OWord . fromIntegral
  fromObj o = case o of { OWord o -> return (fromIntegral o); _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (o==0) >> return ONull
    TrueType     -> \o -> return $ if o==0 then ONull else OTrue
    CharType     -> fmap OChar . charFromIntegral
    IntType      -> return . OInt     . fromIntegral
    WordType     -> return . OWord    . fromIntegral
    LongType     -> return . OLong    . toInteger
    FloatType    -> return . OFloat   . fromRational . toRational
    RatioType    -> return . ORatio   . toRational
    ComplexType  -> return . OComplex . flip complex 0 . fromRational . toRational
    DiffTimeType -> return . ORelTime . fromRational . toRational
    StringType   -> return . obj      . prettyShow . obj
    BytesType    -> return . OBytes . D.runPut . D.putWord64le . fromIntegral
    _            -> \ _ -> mzero

instance ObjectClass Word64 where
  obj = OWord
  fromObj o = case o of { OWord o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (o==0) >> return ONull
    TrueType     -> \o -> return $ if o==0 then ONull else OTrue
    CharType     -> fmap OChar . charFromIntegral
    IntType      -> return . OInt     . fromIntegral
    WordType     -> return . OWord
    LongType     -> return . OLong    . toInteger
    FloatType    -> return . OFloat   . fromRational . toRational
    RatioType    -> return . ORatio   . toRational
    ComplexType  -> return . OComplex . flip complex 0 . fromRational . toRational
    DiffTimeType -> return . ORelTime . fromRational . toRational
    StringType   -> return . obj      . prettyShow . obj
    BytesType    -> return . OBytes   . D.runPut . D.putWord64le . fromIntegral
    _            -> \ _ -> mzero

instance ObjectClass Integer where
  obj = OLong
  fromObj o = case o of { OLong o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (o==0) >> return ONull
    TrueType     -> \o -> return $ if o==0 then ONull else OTrue
    CharType     -> fmap OChar . charFromIntegral
    IntType      -> return . OInt     . fromInteger
    WordType     -> return . OWord    . fromInteger
    LongType     -> return . OLong
    FloatType    -> return . OFloat   . fromRational . toRational
    RatioType    -> return . ORatio   . toRational
    ComplexType  -> return . OComplex . flip complex 0 . fromRational . toRational
    DiffTimeType -> return . ORelTime . fromRational . toRational
    StringType   -> return . obj      . show
    BytesType    -> return . OBytes . B.reverse . D.encode
    _            -> \ _ -> mzero

instance ObjectClass NominalDiffTime where
  obj = ORelTime
  fromObj o = case o of { ORelTime o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (toRational o == 0) >> return ONull
    TrueType     -> \o -> return $ if toRational o == 0 then ONull else OTrue
    IntType      -> return . OInt . round
    WordType     -> return . OWord . round
    LongType     -> return . OLong . round
    FloatType    -> return . OFloat . fromRational . toRational
    DiffTimeType -> return . ORelTime
    ComplexType  -> return . OComplex . flip complex 0 . fromRational . toRational
    _            -> \ _ -> mzero

instance ObjectClass Double where
  obj = OFloat
  fromObj o = case o of { OFloat o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (o==0) >> return ONull
    TrueType     -> \o -> return $ if o==0 then ONull else OTrue
    CharType     -> fmap OChar . charFromIntegral . (round :: Double -> Int)
    IntType      -> return . OInt     . round
    WordType     -> return . OWord    . round
    LongType     -> return . OLong    . round
    FloatType    -> return . OFloat
    RatioType    -> return . ORatio   . toRational
    ComplexType  -> return . OComplex . flip complex 0 . fromRational . toRational
    DiffTimeType -> return . ORelTime . fromRational . toRational
    StringType   -> return . obj      . show
    BytesType    -> return . OBytes . D.encode
    _            -> \ _ -> mzero

instance ObjectClass Rational where
  obj = ORatio
  fromObj o = case o of { ORatio o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (o==0) >> return ONull
    TrueType     -> \o -> return $ if o==0 then ONull else OTrue
    CharType     -> fmap OChar . charFromIntegral . (round :: Rational -> Int)
    IntType      -> return . OInt     . round
    WordType     -> return . OWord    . round
    LongType     -> return . OLong    . round
    FloatType    -> return . OFloat   . fromRational
    RatioType    -> return . ORatio
    ComplexType  -> return . OComplex . flip complex 0 . fromRational
    DiffTimeType -> return . ORelTime . fromRational . toRational
    StringType   -> return . obj      . prettyShow . obj
    _            -> \ _ -> mzero

instance ObjectClass Complex where
  obj = OComplex
  fromObj o = case o of { OComplex o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType     -> \o -> guard (complex 0 0 == o) >> return ONull
    TrueType     -> \o -> return $ if complex 0 0 == o then ONull else OTrue
    IntType      -> i OInt
    WordType     -> return . OWord . round . magnitude
    LongType     -> i OLong
    FloatType    -> f OFloat
    RatioType    -> f ORatio
    ComplexType  -> return . OComplex
    DiffTimeType -> f ORelTime
    StringType   -> return . obj . prettyShow
    _            -> \ _ -> mzero
    where
      f constr o = guard (imagPart o == 0) >> return (constr $ fromRational $ toRational $ realPart o)
      i constr = f (constr . fromInteger . (round :: Rational -> Integer))

instance ObjectClass UStr where
  obj = OString
  fromObj o = case o of { OString o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    StringType -> return . OString
    RefType    -> fmap obj . xmaybe . (maybeFromUStr :: UStr -> Maybe Name)
    _          -> castToCoreType StringType . uchars

instance ObjectClass String where
  obj = obj . toUStr
  fromObj = fromObj >=> maybeFromUStr
  castToCoreType t = case t of
      NullType  -> \o -> guard (o=="null") >> return ONull
      TrueType  -> \o -> case map toLower o of
        "true"  -> return OTrue
        "yes"   -> return OTrue
        "no"    -> return ONull
        "false" -> return ONull
        "null"  -> return ONull
        _       -> mzero
      IntType   -> pars OInt
      WordType  -> pars OWord
      LongType  -> pars OLong
      FloatType -> pars OFloat
      TimeType  -> pars OAbsTime
      _         -> \ _ -> mzero
    where
      nospc = dropWhile isSpace
      pars f str = case fmap (reverse . nospc . reverse) <$> readsPrec 0 (nospc str) of
        [(o, "")] -> return (f o)
        _         -> mzero

instance ObjectClass B.ByteString where
  obj = OBytes
  fromObj o = case o of { OBytes o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    NullType  -> f (D.isEmpty >>= guard >> return ONull)
    TrueType  ->
      f (D.getWord8 >>= \w ->
          return $ case w of { 0->Just ONull; 1->Just OTrue; _->mzero; }) >=> xmaybe
    CharType  -> fmap OChar . (f D.getWord64le >=> charFromIntegral)
    IntType   -> fmap (OInt . fromIntegral) . f D.getWord64le
    WordType  -> fmap (OWord . fromIntegral) . f D.getWord64le
    LongType  -> return . OLong . D.decode . B.reverse
    FloatType -> return . OFloat . D.decode
    _         -> \ _ -> mzero
    where
      f :: D.Get o -> B.ByteString -> XPure o
      f get = return . D.runGet get

instance ObjectClass [Object] where
  obj = OList
  fromObj o = case o of { OList o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    StringType -> fmap OList . loop return
    BytesType  -> fmap (OBytes . D.runPut . mapM_ D.putLazyByteString) . loop (\ (OBytes o) -> [o])
    ListType   -> return . OList
    _          -> \ _ -> mzero
    where
      loop f = fmap concat .
        mapM (\o -> (xmaybe (fromObj o) >>= loop f) <|> (f <$> castToCoreType t o))

instance ObjectClass (M.Map Name Object) where
  obj = ODict
  fromObj o = case o of { ODict o -> return o; _ -> mzero; }
  castToCoreType t o = case t of
    NullType -> guard (M.null o) >> return ONull
    DictType -> return $ ODict o
    _        -> mzero

instance ObjectClass Reference where
  obj = ORef
  fromObj o = case o of { ORef o -> return o; _ -> mzero; }
  castToCoreType t o = case t of
    StringType -> return $ obj $ '$':prettyShow o
    RefType    -> return (ORef o)
    _          -> mzero

instance ObjectClass Name where
  obj n = ORef $ Reference UNQUAL n NullRef
  fromObj o = case o of
    ORef (Reference UNQUAL name NullRef) -> return name
    _ -> mzero

instance ObjectClass ObjType where
  obj = OType
  fromObj o = case o of { OType o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    TypeType   -> return . OType
    StringType -> return . obj . prettyShow
    _          -> \ _ -> mzero

instance ObjectClass CoreType where
  obj = OType . coreType
  fromObj o = case o of
    OType (ObjType [TypeStruct [CoreType o]]) -> return o
    _ -> mzero
  castToCoreType t = case t of
    IntType    -> return . OInt  . fromIntegral . fromEnum
    WordType   -> return . OWord . fromIntegral . fromEnum
    LongType   -> return . OLong . fromIntegral . fromEnum
    StringType -> return . obj   . show
    TypeType   -> return . obj
    _          -> \ _ -> mzero

instance ObjectClass Struct where
  obj = OTree
  fromObj o = case o of { OTree o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    TreeType   -> return . OTree
    RefType    -> return . obj . structName
    StringType -> return . obj . prettyShow
    _          -> \ _ -> mzero

instance ObjectClass UTCTime where
  obj = OAbsTime
  fromObj o = case o of { OAbsTime o -> return o; _ -> mzero; }
  castToCoreType t = case t of
    StringType -> return . obj . prettyShow . obj
    TimeType   -> return . OAbsTime
    _          -> \ _ -> mzero

instance ObjectClass HaskellData where
  obj = OHaskell
  fromObj o = case o of { OHaskell o -> return o; _ -> mzero; }

instance ObjectClass Dynamic where
  obj = opaque
  fromObj o = case o of { OHaskell (HaskellData o _) -> return o; _ -> mzero; }

instance ObjectClass Object where
  obj = id;
  fromObj = return;
  castToCoreType t o = case o of
    ONull      ->  f False
    OTrue      ->  f True
    OChar    o ->  f o
    OInt     o ->  f o
    OWord    o ->  f o
    OLong    o ->  f o
    OAbsTime o ->  f o
    OFloat   o ->  f o
    ORatio   o ->  f o
    OComplex o ->  f o
    OString  o ->  f o
    OBytes   o ->  f o
    OList    o ->  f o
    ODict    o ->  f o
    ORef     o ->  f o
    OType    o ->  f o
    OTree    o ->  f o
    ORelTime o ->  f o
    OHaskell _ -> mzero
    where
      f :: ObjectClass o => o -> XPure Object
      f = castToCoreType t

instance ObjectClass Location where { obj=new; fromObj=objFromHaskellData; }

listToObj :: ObjectClass o => [o] -> Object
listToObj = OList . map obj

listFromObj :: ObjectClass o => Object -> Maybe [o]
listFromObj o = case o of
  OList o -> mapM fromObj o
  _       -> mzero

-- | Create a new 'Object' containing the original value and a reference to the 'Interface'
-- retrieved by the instance of 'haskellDataInterface' for the data type.
new :: (HaskellDataClass typ, Typeable typ) => typ -> Object
new = OHaskell . toHaskellData

-- | Create a completely opaque haskell data type that can be used stored to a Dao language
-- variable, but never inspected or modified in any way.
opaque :: Typeable typ => typ -> Object
opaque o = OHaskell $ HaskellData (toDyn o) $
  interfaceToDynamic $ interface o $ return ()

-- | The inverse operation of 'new', uses 'fromObj' and 'fromHaskellData' to extract the data type
-- wrapped up in the 'Object', assuming the 'Object' is the 'OHaskell' constructor holding a
-- 'HaskellData' container.
objFromHaskellData :: (Typeable o, HaskellDataClass o) => Object -> Maybe o
objFromHaskellData = fromObj >=> fromHaskellData

----------------------------------------------------------------------------------------------------

-- | This is the data type used to associate a Haskell data type with the 'Interface' used by the
-- runtime to read and modify the data. Whenever an non-primitive 'Object' is created, the data is
-- converted to a 'Data.Dynamic.Dynamic' value and paired with a copy of the 'Interface'.
data HaskellData = HaskellData Dynamic (Interface Dynamic) deriving Typeable

instance Eq HaskellData where
  HaskellData a ifcA == HaskellData b ifcB =
    ((ifcA==ifcB)&&) $ maybe False id $ objEquality ifcA >>= \eq -> return (eq a b)

instance Ord HaskellData where
  compare (HaskellData a ifcA) (HaskellData b ifcB) = maybe err id $
    guard (ifcA==ifcB) >> objOrdering ifcA >>= \comp -> return (comp a b) where
      err = error $ unwords $
        [ "cannot compare object of type", show (objHaskellType ifcA)
        , "with obejct of type", show (objHaskellType ifcB)
        ]

instance Show HaskellData where { show (HaskellData _ o) = show (objHaskellType o) }

instance NFData HaskellData where { rnf (HaskellData _ _) = () }

instance PPrintable Object where
  pPrint o = case o of
    ONull            -> pString "null"
    OTrue            -> pString "true"
    OChar      o     -> pShow o
    OInt       o     -> pShow o
    OWord      o     -> pString (show o++"U")
    OLong      o     -> pString (show o++"L")
    ORelTime   o     -> pShow o
    OFloat     o     -> pString (show o++"f")
    ORatio     o     ->
      if denominator o == 1
        then  pString (show (numerator o)++"R")
        else  pWrapIndent $
                [ pString "(", pString (show (numerator o)), pString "/"
                , pString (show (denominator o)++"R"), pString ")"
                ]
    OComplex   o     -> pPrint o
    OString    o     -> pShow o
    OBytes     o     ->
      if B.null o
        then  pString "data{}"
        else  pList (pString "data") "{" ", " "}" (map (pString . showHex) (B.unpack o))
    OList      ox    -> if null ox then pString "list{}" else pContainer "list " pPrint ox
    ODict      o     ->
      if M.null o
      then pString "dict{}"
      else pContainer "dict " (\ (a, b) -> pWrapIndent [pPrint a, pString " = ", pPrint b]) (M.assocs o)
    ORef       o     -> pPrint o
    OType      o     -> pPrint o
    OTree      o     -> pPrint o
    OAbsTime   o     -> pString ("date "++show o)
    OHaskell   (HaskellData o ifc) -> case objPPrinter ifc of
      Nothing -> fail $ "cannot pretty print Haskell data type: "++show (objHaskellType ifc)
      Just pp -> pp o

instance B.Binary HaskellData MTab where
  put (HaskellData o ifc) = do
    let typ = objHaskellType ifc 
    let tid = typeRepToUStr typ
    mtab <- B.getCoderTable
    case B.getEncoderForType tid mtab of
      Just fn -> do
        tid  <- B.newInStreamID tid
        B.put tid >> B.putWithBlockStream1M (fn o)
      Nothing -> fail $ unwords ["no binary format method defied for Haskell type", show typ]
  get = do
    B.updateTypes
    mtab <- B.getCoderTable
    tid  <- B.get >>= B.decodeIndexLookup
    maybe mzero id $ do
      tid <- tid
      fn  <- B.getDecoderForType tid mtab
      tab <- lookupMethodTable (fromUStr tid) mtab
      return (flip HaskellData tab <$> B.getWithBlockStream1M fn)

instance HasNullValue HaskellData where
  nullValue = toHaskellData ()
  testNull (HaskellData o ifc) = case objNullTest ifc of
    Nothing -> error ("to check whether objects of type "++show (objHaskellType ifc)++" are null is undefined behavior")
    Just fn -> fn o

----------------------------------------------------------------------------------------------------

class Sizeable o where { getSizeOf :: o -> Exec Object  }

instance Sizeable Char where { getSizeOf = return . obj . ord }
instance Sizeable Word64 where { getSizeOf = return . obj }
instance Sizeable Int where { getSizeOf = return . obj . abs }
instance Sizeable Double where { getSizeOf = return . obj . abs }
instance Sizeable Integer where { getSizeOf = return . obj . abs }
instance Sizeable NominalDiffTime where { getSizeOf = return . obj . abs }
instance Sizeable Rational where { getSizeOf = return . obj . abs }
instance Sizeable Complex where { getSizeOf = return . obj . magnitude }
instance Sizeable UStr where { getSizeOf = return . obj . ulength }
instance Sizeable [Object] where { getSizeOf = return . obj . length }
instance Sizeable (M.Map Name Object) where { getSizeOf = return . obj . M.size }
instance Sizeable HaskellData where { getSizeOf (HaskellData o ifc) = maybe mzero ($ o) (objSizer ifc) }

instance Sizeable Object where
  getSizeOf o = case o of
    OChar    o -> getSizeOf o
    OWord    o -> getSizeOf o
    OInt     o -> getSizeOf o
    OLong    o -> getSizeOf o
    ORelTime o -> getSizeOf o
    OFloat   o -> getSizeOf o
    ORatio   o -> getSizeOf o
    OComplex o -> getSizeOf o
    OString  o -> getSizeOf o
    OList    o -> getSizeOf o
    ODict    o -> getSizeOf o
    OHaskell o -> getSizeOf o
    _          -> mzero

----------------------------------------------------------------------------------------------------

-- $Building_structs
-- Here are all the basic functions for converting between Haskell language data types and Dao
-- language structures.
-- 
-- Most 'FromDaoStruct' functions will backtrack when they fail to get the necessary data. This
-- function can make a backtracking function fail. For example:
-- > 'tryField' "x" >>= 'objType'
-- backtracks in any case
-- 
-- > required (tryField "x" >>= objType)
-- > tryField "x" >>= required objType
-- These two forms do the same thing: fails if 'objType' backtracks, but not if the field doesn't
-- exist.
-- 
-- > 'Control.Applicative.optional' ('tryField' "x" >>= 'objType')
-- returns 'Prelude.Nothing' if the field does not exist or if 'objType' backtracks
-- 
-- > 'field' "x" >>= 'objType'
-- fails if the field does not exist, backtracks if it exists but is the wrong type
-- (you probably don't ever want to do this).
-- 
-- > 'required' ('field' "x" >>= 'objType')
-- > 'field' "x" >>= 'required' 'objType'
-- These two forms are the same: fails if either the field does not exist or if 'objType'
-- backtracks.

-- | This is the data type used as the intermediary between Haskell objects and Dao objects. If you
-- would like your Haskell data type to be used as a non-opaque data type in a Dao language script,
-- the first step is to instantiate your data type into this class. The next step would be to
-- instantiate your object into the 'HaskellDataClass' class. Instantiating the
-- 'HaskellDataClass' class alone will make your object usable in Dao language scripts, but
-- it will be an opaque type. Instantiating 'Struct' and declaring 'autoDefStruct' in the
-- 'defObjectInterface' will allow functions in the Dao language script to read and write
-- information to your data structure, modifying it during runtime.
-- 
-- 'Struct' values are used lazily, so your data types will only be converted to and from 'Struct's
-- when absolutely necessary. This helps to conserver memory usage.
data Struct
  = Nullary{ structName :: Name }
    -- ^ models a constructor with no fields, for example 'Prelude.EQ', 'Prelude.GT' and
    -- 'Prelude.LT'.
  | Struct
    { structName :: Name -- ^ provide the name for this constructor.
    , fieldMap   :: M.Map Name Object
    }
  deriving (Eq, Ord, Show, Typeable)

instance NFData Struct where
  rnf (Nullary a  ) = deepseq a ()
  rnf (Struct  a b) = deepseq a $! deepseq b ()

instance HasNullValue Struct where
  nullValue = Nullary{ structName=ustr "NULL" }
  testNull (Nullary{ structName=name }) = name == ustr "NULL"
  testNull _ = False

-- binary 0x25 0x26
instance B.Binary Struct MTab where
  put o = case o of
    Nullary  o -> B.putWord8 0x25 >> B.put o
    Struct n o -> B.putWord8 0x26 >> B.put n >> B.put o
  get = B.word8PrefixTable <|> fail "expecting Struct"

instance B.HasPrefixTable Struct B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "Struct" 0x25 0x26 $
    [ Nullary <$> B.get
    , return Struct <*> B.get <*> B.get
    ]

instance PPrintable Struct where
  pPrint o = case o of
    Nullary{ structName=name } -> pString ('#' : uchars (toUStr name))
    Struct{ structName=name, fieldMap=dict } ->
      pList (pString ('#' : uchars (toUStr name))) "{" ", " "}" $
        flip map (M.assocs dict) $ \ (left, right) -> pInline $
          [pPrint left, pString " = ", pPrint right]

instance HasRandGen Struct where
  randO = _randTrace "Struct" $ countNode $ runRandChoice 
  randChoice = randChoiceList $
    [ scramble $
        return Struct <*> randO <*> (M.fromList <$> randListOf 1 4 (pure (,) <*> randO <*> randO))
    , Nullary <$> randO
    ]
  defaultO = _randTrace "D.Struct" $ Nullary <$> defaultO

instance ToDaoStructClass Struct where { toDaoStruct = return () }

instance FromDaoStructClass Struct where { fromDaoStruct = FromDaoStruct $ lift get }

instance HaskellDataClass Struct where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

-- | You can make your data type readable but not writable in the Dao runtime. That means a Dao
-- script can inspect elements of your data type, but not modify them As an example lets say you
-- have a 3D-point data type you would like to use in your Dao script.
-- > data Point =
-- >     Point2D{ get_x::'T_float', get_y::'T_float' }
-- >   | Point3D{ get_x::'T_float', get_y::'T_float', get_z::'T_float' }
-- 
-- Lets say you have already instantiated the 'HaskellDataClass' class and provided the Dao runtime with
-- a 'DaoFunc' (via 'setupDao') that constructs a Point3D at runtime:
-- > p = Point3D(1.9, 4.4, -2.1);
-- Now you would like to extend the 'HaskellDataClass' of your Point3D to also be readable at runtime.
-- If you instantiate 'ToDaoStructClass' your Dao language script could also read elements from the
-- point like so:
-- > distFromOrigin = sqrt(p.x*p.x + p.y*p.y + p.z*p.z);
-- However you cannot modify the point unless you also instantiate 'FromDaoStructClass'. So a statement
-- like this would result in an error:
-- > p.x /= distFromOrigin;
-- > p.y /= distFromOrigin;
-- > p.z /= distFromOrigin;
-- 
-- You can convert this to a 'Struct' type using the 'fromData' function. There are many ways to
-- define fields in a 'Struct', here are a few:
-- > instance 'ToDaoStructClass' Point3D 'Object' where
-- >     'toDaoStruct' = 'fromData' "@Point2D@" $ do
-- >         'putPrimField' "x" get_x
-- >         'putPrimField' "y" get_y
-- >          obj <- 'Control.Monad.Reader.Class.ask'
-- >          case obj of
-- >             Point3D _ _ z -> do
-- >                 'renameConstructor' "@Point3D@"
-- >                 'define' "z" ('obj' z)
-- >             _             -> return ()
-- 
-- Finally, you should define the instantiation of Point3D into the 'HaskellDataClass' class so it
-- includes the directive 'autoDefToStruct'.
class ToDaoStructClass haskData where { toDaoStruct :: ToDaoStruct haskData () }

-- | Continuing the example from above, if you do want your data type to be modifyable by functions
-- running in the Dao language runtime, you must instantiate this class, which is facilitated by the
-- 'toData' function.
-- > instance 'FromDaoStructClass' 'Point3D' where
-- >     fromDaoStruct = 'toData' $ 'Control.Monad.msum' $
-- >         [ do 'constructor' "@Point2D@"
-- >              return Point3D 'Control.Applicative.<*>' 'req' "x" 'Control.Applicative.<*>' 'req' "y"
-- >         , do 'constructor' "@Point3D@"
-- >              return Point3D 'Control.Applicative.<*>' 'req' "x" 'Control.Applicative.<*>' 'req' "y" 'Control.Applicative.<*>' 'req' "z"
-- >         ]
-- 
-- Do not forget to define the instantiation of Point3D into the 'HaskellDataClass' class so it
-- includes the directive 'autoDefFromStruct'.
-- 
-- Note that an instance of 'FromDaoStructClass' must also instantiate 'ToDaoStructClass'. I can see no
-- use for objects that are only writable, that is they can be created at runtime but never
-- inspected at runtime.
class ToDaoStructClass haskData => FromDaoStructClass haskData where
  fromDaoStruct :: FromDaoStruct haskData

-- | If there is ever an error converting to or from your Haskell data type, you can
-- 'Control.Monad.Error.throwError' a 'StructError'.
data StructError
  = StructError
    { structErrMsg    :: Maybe UStr
    , structErrName   :: Maybe UStr
    , structErrField  :: Maybe UStr
    , structErrValue  :: Maybe Object
    , structErrExtras :: [Name]
    }
  deriving (Eq, Ord, Typeable)

instance HasNullValue StructError where
  nullValue =
    StructError
    { structErrMsg=Nothing
    , structErrName=Nothing
    , structErrField=Nothing
    , structErrValue=Nothing
    , structErrExtras=[]
    }
  testNull
    ( StructError
      { structErrMsg=Nothing
      , structErrName=Nothing
      , structErrField=Nothing
      , structErrValue=Nothing
      , structErrExtras=[]
      }
    ) = True
  testNull _ = False

instance ToDaoStructClass StructError where
  toDaoStruct = do
    asks structErrMsg    >>= optionalField "message" . fmap OString
    asks structErrName   >>= optionalField "structName" . fmap OString
    asks structErrField  >>= optionalField "field" . fmap OString
    asks structErrValue  >>= optionalField "value"
    asks structErrExtras >>= optionalField "extras" . fmap obj . refNames
    return ()

instance FromDaoStructClass StructError where
  fromDaoStruct = do
    constructor "StructError"
    let str o = case o of
          OString o -> return o
          _         -> fail "expecting string value"
    let ref o = case o of
          ORef    o -> case o of
            Reference UNQUAL o NullRef -> return o
            _ -> fail "not an unqualified reference singleton"
          _ -> fail "not a reference type"
    let lst o = case o of
          OList   o -> forM o ref
          _         -> fail "expecting list value"
    return StructError
      <*> optional (tryField "message" $ str)
      <*> optional (tryField "structName" $ str)
      <*> optional (tryField "field" $ str)
      <*> optional (tryField "value" return)
      <*> (tryField "extras" $ lst)

instance ObjectClass StructError where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass StructError where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest
    autoDefToStruct >> autoDefFromStruct

-- | Used to convert a 'Prelude.String' to a 'Dao.String.Name' by functions like 'define' and
-- 'setField'. Usually you will not need to use it.
mkLabel :: (UStrType name, MonadPlus m) => name -> m Name
mkLabel name = xmaybe $ maybeFromUStr (toUStr name)

mkStructName :: (UStrType name, MonadPlus m) => name -> m Name
mkStructName name = mplus (mkLabel name) $ fail "invalid constructor name"

mkFieldName :: (UStrType name, MonadPlus m) => name -> m Name
mkFieldName name = mplus (mkLabel name) $ fail "invalid field name"

-- | This is a handy monadic and 'Data.Functor.Applicative' interface for instantiating
-- 'toDaoStruct' in the 'ToDaoStructClass' class.
newtype ToDaoStruct haskData a
  = ToDaoStruct
    { run_toDaoStruct :: PredicateT StructError (State (Struct, haskData)) a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad (ToDaoStruct haskData) where
  return = ToDaoStruct . return
  m >>= f = ToDaoStruct $ run_toDaoStruct m >>= run_toDaoStruct . f
  fail msg = throwError $ nullValue{ structErrMsg = Just $ ustr msg }

instance MonadState Struct (ToDaoStruct haskData) where
  state f = ToDaoStruct $ lift $ state $ \ (struct, haskData) ->
    let (a, struct') = f struct in (a, (struct', haskData))

instance MonadReader haskData (ToDaoStruct haskData) where
  ask = ToDaoStruct $ lift $ fmap snd get
  local upd f = ToDaoStruct $ PredicateT $ do
    haskData <- gets snd
    modify (\ (struct, _) -> (struct, upd haskData))
    a <- runPredicateT $ run_toDaoStruct f
    modify (\ (struct, _) -> (struct, haskData))
    return a

instance MonadError StructError (ToDaoStruct haskData) where
  throwError err = get >>= \struct -> ToDaoStruct $ throwError $
    err{ structErrName = structErrName err <|> (Just $ toUStr $ structName struct) }
  catchError f catch = ToDaoStruct $ catchError (run_toDaoStruct f) (run_toDaoStruct . catch)

instance MonadPlusError StructError (ToDaoStruct haskData) where
  catchPredicate = ToDaoStruct . catchPredicate . run_toDaoStruct
  predicate      = ToDaoStruct . predicate

-- | This function is typically used to evaluate the instantiation of 'toDaoStruct'. It takes two
-- parameters: first a computation to convert your data type to the 'Struct' using the 'ToDaoStruct'
-- monad, and second the data type you want to convert. You can use functions like 'defineWith' and
-- 'setField' to build your 'ToDaoStruct' computation. For example, lets say you have a Haskell data
-- type called @mydat::MyData@ where @MyData@ instantiates 'ToDaoStruct', you can convert it to a
-- Dao 'Struct' like so:
-- > 'fromData' 'toDaoStruct' mydat
-- Notice how it reads similar to ordinary English, "convert from (Haskell) data to a Dao 'Struct'"
fromData
  :: ToDaoStruct haskData x
  -> haskData
  -> Predicate StructError Struct
fromData pred hask = evalState (runPredicateT $ run_toDaoStruct $ pred >> get) $
  (Struct{ structName=nil, fieldMap=M.empty }, hask)

-- | Overwrite the current 'Struct' with a 'Struct' produced by a 'toDaoStruct' instance of a
-- different type. This is useful when instantiating a newtype or a data type constructor that
-- contains only one item (the "inner" item), and the data type of the inner item instantiates
-- 'ToDaoStructClass', you can simply use the instance of 'toDaoStruct' for that data type to
-- instantiate 'toDaoStruct' for the outer data type. Just be sure that the constructor name for the
-- inner type does not conflict with the constructor name for the outer data type. For example:
-- > data X = X1 { ... } | X2 { ... }
-- > instance 'DaoToStructClass' X 'Object' where { ... }
-- > data Y = Y1 { ... } | Y2 { ... }
-- > instance 'DaoToStructClass' Y 'Object' where { ... }
-- > 
-- > newtype WrapX = WrapX { unwrapX :: X }
-- > instance 'DaoToStructClass' WrapX 'Object' where
-- >     'toDaoStruct' = 'Control.Monad.Reader.ask' >>= 'innerToStruct' . unwrapX
-- > 
-- > data X_or_Y = Is_X { getX :: X } | Is_Y { getY :: Y }
-- > instance 'DaoToStructClass' X_or_Y 'Object' where
-- >     'toDaoStruct' = 'Control.Monad.Reader.ask' >>= \xy -> case xy of
-- >         Is_X x -> 'innerToStruct' x
-- >         Is_Y y -> 'innerToStruct' y
-- 
-- The inverse of this operation in the 'FromDaoStructClass' is 'Prelude.fmap', or equivalently the
-- 'Control.Applicative.<$>' operator. Here is an example using 'Control.Applicative.<$>':
-- > instance 'FromDaoStructClass' WrapX 'Object' where
-- >     'fromDaoStruct' = WrapX <$> 'fromDaoStruct'
-- > 
-- > instance 'FromDaoStructClass' X_or_Y 'Object' where
-- >     'fromDaoStruct' = Is_X <$> 'fromDaoStruct' <|> Is_Y <$> 'fromDaoStruct'
-- 
-- Another way to do exactly the same thing as the example above is:
-- > instance 'FromDaoStructClass' WrapX 'Object' where
-- >     'fromDaoStruct' = 'Prelude.fmap' WrapX 'fromDaoStruct'
-- > 
-- > instance 'FromDaoStructClass' X_or_Y 'Object' where
-- >     'fromDaoStruct' = 'Prelude.fmap' Is_X 'fromDaoStruct' `'Control.Monad.mplus'` 'Prelude.fmap' Is_Y 'fromDaoStruct'
-- 
-- It is possible to use 'renameConstructor' after evaluating 'innerToStruct' to use a different
-- constructor name while keeping all of the fields set by the evaluation of 'innerToStruct',
-- however if this is done, 'Prelude.fmap' will backtrack, so you should use 'innerFromStruct'
-- instead.
innerToStruct :: ToDaoStructClass inner => inner -> ToDaoStruct haskData ()
innerToStruct = innerToStructWith toDaoStruct

-- | Like 'innerToStruct' but lets you supply a 'ToDaoStruct' function for an arbitrary data type,
-- not just one that instantiates 'ToDaoStructClass'.
innerToStructWith :: ToDaoStruct inner () -> inner -> ToDaoStruct haskData ()
innerToStructWith toDaoStruct o = ask >>= \haskData ->
  predicate (fromData toDaoStruct o) >>= ToDaoStruct . lift . put . flip (,) haskData

-- | Use this function to set the 'structName' name of the constructor at some point, for example
-- when you observe some condition of the @haskData@ type that merits an alternative constructor
-- name.
renameConstructor :: UStrType name => name -> ToDaoStruct haskData ()
renameConstructor name = mkStructName name >>= \name -> modify $ \struct -> struct{ structName=name }

-- | Like 'renameConstructor' but deletes everything and makes the 'Struct' being constructed into a
-- 'Nullary'. You would typically do this only when you are instantiating 'toDaoStruct' and you
-- only have one constructor to define.
makeNullary :: UStrType name => name -> ToDaoStruct haskData ()
makeNullary name = mkStructName name >>= \name -> put $ Nullary{ structName=name }

-- | Use this when you have derived the "Prelude.Show" class for a data type where every constructor
-- in that data type takes no parameters, for example, the 'Prelude.Ordering' data type.
putNullaryUsingShow :: Show haskData => ToDaoStruct haskData ()
putNullaryUsingShow = ask >>= makeNullary . show

define :: UStrType name => name -> Object -> ToDaoStruct haskData Object
define name value = do
  name <- mkFieldName name
  modify $ \struct -> struct{ fieldMap = M.insert name value (fieldMap struct) }
  return value

-- | Defines an optional field. If the value given is 'Prelude.Nothing', nothing happens. Otherwise
-- the value is placed into the 'Struct' at the given @name@d field. This is the inverse opreation
-- of using 'Control.Applicative.optional' in the 'FromDaoStruct' monad.
optionalField :: UStrType name => name -> Maybe Object -> ToDaoStruct haskData (Maybe Object)
optionalField name = maybe (return Nothing) (fmap Just . define name)

setField :: UStrType name => name -> (haskData -> Object) -> ToDaoStruct haskData Object
setField name f = ask >>= define name . f

-- | This is an important function for instantiating 'ToDaoStructClass'. It takes any
-- value instantiating 'HaskellDataClass', converts it to an 'Object' using the 'new'
-- function. It is the inverse of 'objType'.
--
-- It is recommended you use this function instead of 'defStructField', 'defPrimField', or
-- 'defDynField' whenever it is possible, i.e. whenever the data type you are putting instantiated
-- the 'HaskellDataClass' class.
defObjField
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> o -> ToDaoStruct haskData Object
defObjField name o = define name (obj o)

-- | Synonym for 'defObjField'
(.=) :: (UStrType name, Typeable o, ObjectClass o) => name -> o -> ToDaoStruct haskData Object
(.=) = defObjField
infixr 2 .=

-- | Like 'defObjField' but takes a field accessor to extract the data to be stored from the object
-- being converted. This function is defined as:
-- > \name accessor -> asks accessor >>= defObjField name
putObjField
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> (haskData -> o) -> ToDaoStruct haskData Object
putObjField name which = asks which >>= defObjField name

-- | Synonym for 'putObjField'
(.=@)
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> (haskData -> o) -> ToDaoStruct haskData Object
(.=@) = putObjField
infixr 2 .=@

-- | Like 'putObjField' but operates on an object wrapped in a 'Prelude.Maybe', not doing anything
-- in the case of 'Prelude.Nothing'.
defMaybeObjField
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> Maybe o -> ToDaoStruct haskData (Maybe Object)
defMaybeObjField name = maybe (return Nothing) (fmap Just . defObjField name)

(.=?) 
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> Maybe o -> ToDaoStruct haskData (Maybe Object)
(.=?) = defMaybeObjField

-- | This is a handy monadic and 'Data.Functor.Applicative' interface for instantiating
-- 'fromDaoStruct' in the 'FromDaoStructClass' class. It takes the form of a reader because what you
-- /read/ from the 'Struct' here in the Haskell language was /written/ by the Dao language
-- runtime. Think of it as "this is the data type used when the Dao runtime wants to write
-- information to my data structure."
-- 
-- Because Dao is such a messy, fuzzy, not statically typed, interpreted language, the information
-- coming in from the Dao runtime requires a lot of sanitization. Therefore this monad provides
-- several functions for checking the type of information you are using to build your Haskell data
-- type.
--
-- Be sure to make ample use of the 'Control.Monad.guard', 'Control.Monad.Error.throwError', and
-- 'Control.Monad.fail' functions.
-- 
-- /NOTE:/ refer to the documentation of the 'constructor' monad for an important note on reading
-- Haskell data types with multiple constructors.
newtype FromDaoStruct a =
  FromDaoStruct{ run_fromDaoStruct :: PredicateT StructError (State Struct) a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad FromDaoStruct where
  return = FromDaoStruct . return
  m >>= f = FromDaoStruct $ run_fromDaoStruct m >>= run_fromDaoStruct . f
  fail msg = throwError $ nullValue{ structErrMsg = Just (ustr msg) }

instance MonadReader Struct FromDaoStruct where
  ask = FromDaoStruct $ lift get
  local upd f = FromDaoStruct $ PredicateT $ get >>= \st ->
   return $ evalState (runPredicateT $ run_fromDaoStruct f) (upd st)

instance MonadError StructError FromDaoStruct where
  throwError err = ask >>= \struct -> FromDaoStruct $ throwError $
    err { structErrName = structErrName err <|> (Just $ toUStr $ structName struct) }
  catchError (FromDaoStruct f) catch = FromDaoStruct $ catchError f (run_fromDaoStruct . catch)

instance MonadPlusError StructError FromDaoStruct where
  catchPredicate = FromDaoStruct . catchPredicate . run_fromDaoStruct
  predicate = FromDaoStruct . predicate

-- | This function is typically used to evaluate the instantiation of 'fromDaoStruct'. It takes two
-- parameters: first a computation to convert your data type to the Haskell data type from a
-- 'Struct' using the 'FromDaoStruct' monad, and second the 'Struct' you want to convert. For
-- example, if you have a Haskell data type 'MyData' which instantiates 'FromDaoStruct', you could
-- construct it from a properly formatted Dao 'Struct' using this statement:
-- > 'toData' 'fromDaoStruct' struct
-- Notice that this reads similar to ordinary English: "convert to (Haskell) data from a dao
-- struct."
toData :: FromDaoStruct haskData -> Struct -> Predicate StructError haskData
toData (FromDaoStruct f) = evalState (runPredicateT f)

-- | Checks if the 'structName' is equal to the given name, and if not then backtracks. This is
-- important when constructing Haskell data types with multiple constructors.
--
-- A haskell data type with multiple constructors should be constructed with the
-- 'Control.Monad.msum' function like so:
-- > data MyData = A | B Int | C Int Int
-- > instance 'FromDaoStruct' ('Object) where
-- >     'fromDaoStruct' = 'toData' $ 'Control.Monad.msum' $
-- >         [ 'constructor' "A" >> return a,
-- >           do 'constructor' "B"
-- >              B 'Control.Applicative.<$>' ('field' "b1" >>= 'primType')
-- >           do 'constructor' "C"
-- >              'Control.Applicative.return' C
-- >                  'Control.Applicative.<*>' 'required' ('field' "c1" >>= 'primType')
-- >                  'Control.Applicative.<*>' 'required' ('field' "c2" >>= 'primType')
-- >         ]
-- /NOTE/ that if all three 'constructor's backtrack (evaluate to 'Control.Monad.mzero') the whole
-- monad will backtrack. By convention, you should let the monad backtrack, rather than writing a
-- 'Control.Monad.fail' statement as the final item in the 'Control.Monad.msum' list.
constructor :: UStrType name => name -> FromDaoStruct ()
constructor name = (return (==) <*> mkStructName name <*> asks structName) >>= guard

-- | The inverse operation of 'innerToStruct', but looks for a constructor of a different name. This
-- is important because every 'toDaoStruct' should set it's own unique constructor name, and if you
-- set a different constructor name while using the same 'fromDaoStruct' function to read the fields
-- of the struct, the 'fromDaoStruct' function will backtrack seeing the wrong constructor name.
-- If you have not renamed the constructor with 'renameConstructor' after using 'innerToStruct', do
-- not use this function, simply use 'Prelude.fmap' or the 'Control.Applicative.<$>' operator
-- instead.
-- 
-- This function temporarily changes the constructor name to the constructor set by the @inner@
-- type, that way the 'fromDaoStruct' instance of the @inner@ type will be fooled and read the
-- 'Struct' fields without backtracking. For example:
-- > newtype X = X{ getX :: Int }
-- > instance 'ToDataStruct' X where
-- >     'toDaoStruct' = do
-- >         'renameConstructor' "X"
-- >         "getX" '.=@' getX
-- > 
-- > newtype Y = Y{ innerX :: X }
-- > instance 'ToDataStruct' Y where
-- >     'toDaoStruct' = do
-- >         -- the 'innerToStruct' function will use the 'toDaoStruct' for X
-- >         'Control.Monad.Reader.ask' >>= 'innerToStruct' . innerX
-- >         -- then rename the constructor from "X" to "Y"
-- >         'renameConstructor' "Y"
-- > 
-- Now when we want to define the accompanying 'FromDaoStructClass', we need to remember that we
-- used 'innerToStruct' and changed the 'structName' from "X" to "Y". Simply using 'Prelude.fmap'
-- (or equivalently 'Control.Applicative.<$>') will not work because the instance of 'fromDaoStruct'
-- for the @X@ data type will backtrack when it sees the 'structName' is "Y".
-- > instance 'FromDaoStructClass' Y where
-- >     'fromDaoStruct' = Y 'Control.Applicative.<$>' 'fromDaoStruct' -- /WRONG!/ This will always backtrack.
-- 
-- The correct way to do it is to use 'innerFromStruct' like so:
-- > instance 'FromDaoStructClass' Y where
-- >     'fromDaoStruct' = Y 'Control.Applicative.<$> 'innerFromStruct' "X" -- CORRECT!
-- 
innerFromStruct :: (UStrType name, FromDaoStructClass inner) => name -> FromDaoStruct inner
innerFromStruct tempName = do
  name     <- asks structName
  tempName <- mkStructName tempName
  let setname name = FromDaoStruct $ lift $ modify $ \struct -> struct{ structName=name }
  o <- setname tempName >> mplus fromDaoStruct (setname name >> mzero)
  setname name >> return o

-- | Succeeds if the current 'Struct' is a 'Nullary' where the 'structName' is equal to the name
-- given to this function.
nullary :: UStrType name => name -> FromDaoStruct ()
nullary name = ask >>= \struct -> case struct of
  Nullary{} -> constructor name
  _         -> mzero

-- | Use the instantiation of 'Prelude.Read' derived for a type @haskData@ to construct the
-- @haskData from the 'structName' stored in a 'Nullary' 'Struct'.
getNullaryWithRead :: Read haskData => FromDaoStruct haskData
getNullaryWithRead = ask >>= \struct -> case struct of
  Nullary{ structName=name } -> case readsPrec 0 (uchars name) of
    [(haskData, "")] -> return haskData
    _ -> mzero
  _ -> mzero

-- | If an error is thrown using 'Control.Monad.Error.throwError' or 'Control.Monad.fail' within the
-- given 'FromDaoStruct' function, the 'structErrField' will automatically be set to the provided
-- 'Name' value.
structCurrentField :: Name -> FromDaoStruct o -> FromDaoStruct o
structCurrentField name (FromDaoStruct f) = FromDaoStruct $ catchPredicate f >>= \o -> case o of
  PFail err -> throwError $ err{ structErrField=Just (toUStr name) }
  Backtrack -> mzero
  OK      o -> return o

-- | Retrieves an arbitrary 'Object' by it's field name, and backtraks if no such field is defined.
-- The value of the field is copied, and can be copied again after this operation. It is best not to
-- use this function, rather use 'tryField' to make sure each field is retrieved exactly once, then
-- use 'checkEmpty' to make sure there is no hidden extraneous data in the struct.
tryCopyField :: UStrType name => name -> (Object -> FromDaoStruct o) -> FromDaoStruct o
tryCopyField name f = (return M.lookup <*> mkFieldName name <*> asks fieldMap) >>=
  xmaybe >>= structCurrentField (fromUStr $ toUStr name) . f

-- | Like 'copyField', retrieves an arbitrary 'Object' by it's field name, and backtraks if no such
-- field is defined. However unlike 'tryCopyField', if the item is retrieved, it is deleted from the
-- inner 'Struct' so that it may not be used again. The reason for this is to use 'checkEmpty' and
-- 'requireEmpty', which can backtrack or fail if there are extraneous fields in the structure.
tryField :: UStrType name => name -> (Object -> FromDaoStruct o) -> FromDaoStruct o
tryField name f = do
  name <- mkFieldName name
  o    <- tryCopyField name f
  FromDaoStruct $ lift $ modify $ \st ->
    case st of{ Struct{ fieldMap=m } -> st{ fieldMap=M.delete name m }; s -> s; }
  return o

_throwMissingFieldError :: Name -> FromDaoStruct o
_throwMissingFieldError name = throwError $
  nullValue
  { structErrMsg   = Just $ ustr "missing required field"
  , structErrField = Just $ toUStr name
  }

-- | Like 'field' but evaluates 'Control.Monad.Error.throwError' if the 'FromDaoStruct' function
-- backtracks or throws it's own error. Internally, this function makes use of 'copyField' and /not/
-- 'tryField', so the field is preserved if it exists.
copyField :: UStrType name => name -> (Object -> FromDaoStruct o) -> FromDaoStruct o
copyField name f = mkFieldName name >>= \name ->
  mplus (tryCopyField name f) (_throwMissingFieldError name)

-- | Like 'field' but evaluates 'Control.Monad.Error.throwError' if the 'FromDaoStruct' function
-- backtracks or throws it's own error. Internally, this function makes use of 'tryField' and /not/
-- 'tryCopyField', so the field is removed if it exists -- two consecutive calls to this function
-- with the same key absolutely will fail.
field :: UStrType name => name -> (Object -> FromDaoStruct o) -> FromDaoStruct o
field name f = mkFieldName name >>= \name -> mplus (tryField name f) (_throwMissingFieldError name)

-- | As you make calls to 'field' and 'tryField', the items in these fields in the 'Struct' are
-- being removed. Once you have all of the nata neccessary to construct the data 'Object', you can
-- check to make sure there are no extraneous unused data fields. If the 'Struct' is empty, this
-- function evaluates to @return ()@. If there are extranous fields in the 'Struct', 'throwError' is
-- evaluated.
checkEmpty :: FromDaoStruct ()
checkEmpty = FromDaoStruct (lift get) >>= \st -> case st of
  Struct{ fieldMap=m } -> if M.null m then return () else throwError $
    nullValue
    { structErrMsg    = Just $ ustr "extraneous data fields"
    , structErrExtras = M.keys m
    }
  Nullary{} -> return ()

-- | Takes a conversion as the first parameter. The second parameter will be provided by 'field' or
-- 'tryField' when you pass it as a partial function application. If the conversion function
-- backtracks, 'Control.Monad.Error.throwError' is evaluated with the appropriate error data set.
-- This function should usually not be required, as it is called by functions like 'opt', 'req', and
-- 'reqList'.
convertFieldData :: (Object -> FromDaoStruct o) -> Object -> FromDaoStruct o
convertFieldData f o = mplus (f o) $ throwError $ nullValue{ structErrValue=Just o }

-- | A required 'Struct' 'field'. This function is defined as
req :: (UStrType name, Typeable o, ObjectClass o) => name -> FromDaoStruct o
req name = field name (convertFieldData (xmaybe . fromObj))

-- | Check if a 'Struct' field exists using 'tryField', if it exists, convert it to the necessary
-- data type using 'fromObj' (which fails if an unexpected type is stored in that field).
opt :: (UStrType name, Typeable o, ObjectClass o) => name -> FromDaoStruct (Maybe o)
opt name = Just <$> tryField name (convertFieldData (xmaybe . fromObj)) <|> return Nothing

-- | Like 'req' but internally uses 'listFromObj' instead of 'fromObj'. The field must exist, if it
-- does not this function evaluates to 'Control.Monad.Error.throwError'. Use 'optList' instead if
-- you can accept an empty list when the field is not defined.
reqList :: (UStrType name, Typeable o, ObjectClass o) => name -> FromDaoStruct [o]
reqList name = field name $ convertFieldData (xmaybe . listFromObj)

-- | Like 'opt' but internally uses 'listFromObj' instead of 'fromObj'. The field may not exist, and
-- if it does not this function returns an empty list. Use 'reqList' to evaluate to
-- 'Control.Monad.Error.throwError' in the case the field does not exist.
optList :: (UStrType name, Typeable o, ObjectClass o) => name -> FromDaoStruct [o]
optList name = tryField name $ convertFieldData (maybe (return []) return . listFromObj)

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass Location where
  toDaoStruct = ask >>= \lo -> case lo of
    LocationUnknown -> makeNullary "Void"
    Location{} -> void $ do
      renameConstructor "Location"
      "startingLine"   .=@ startingLine
      "startingColumn" .=@ startingColumn
      "endingLine"     .=@ endingLine
      "endingColumn"   .=@ endingColumn

instance FromDaoStructClass Location where
  fromDaoStruct = msum $
    [ nullary "Void" >> return LocationUnknown
    , do  constructor "Location"
          return Location
            <*> req "startingLine"
            <*> req "startingColumn"
            <*> req "endingLine"
            <*> req "endingColumn"
    ]

putLocation :: Location -> ToDaoStruct haskData ()
putLocation loc = case loc of
  LocationUnknown -> return ()
  Location{} -> void $ "location" .= loc

location :: FromDaoStruct Location
location = req "location"

putComments :: [Comment] -> ToDaoStruct haskData ()
putComments = void . defObjField "comments"

comments :: FromDaoStruct [Comment]
comments = req "comments"

optComments :: FromDaoStruct (Maybe [Comment])
optComments = opt "comments"

instance HasRandGen Object where
  randO = _randTrace "Object" $ countNode $ recurse $ runRandChoice
  randChoice = mappend (fmap unlimitObject defaultChoice) $ randChoiceList $
    [ ORef  <$> randO
    , depthLimitedInt 24 >>= \x ->
        scramble $ OList <$> randList 0 x
    , depthLimitedInt 24 >>= \x ->
        scramble $ ODict . M.fromList <$> randListOf 0 x (return (,) <*> randO <*> randO)
    , OType <$> randO
    , OTree <$> randO
    , ORatio <$> randO
    , OComplex <$> randO
    ]
  defaultO = _randTrace "D.Object" runDefaultChoice
  defaultChoice = randChoiceList $
    [ do  i <- nextInt 10 -- OBytes
          fmap (OBytes . B.concat) $ replicateM i $
            fmap (encode . (\i -> fromIntegral i :: Word32)) randInt
    ]

newtype LimitedObject = LimitedObject { unlimitObject :: Object }

instance HasRandGen LimitedObject where
  randO =  _randTrace "LimitedObject" $ countNode $ runRandChoice
  randChoice = fmap LimitedObject $ randChoiceList $
    [ return ONull, return OTrue
    , OInt     <$> defaultO
    , OWord    <$> defaultO
    , OLong    <$> defaultO
    , OFloat   <$> defaultO
    , OString  <$> defaultO
    , OAbsTime <$> defaultO
    , ORelTime <$> defaultO
    , OChar . chr . flip mod (ord(maxBound::Char)) <$> defaultO
    ]
  defaultO = randO

----------------------------------------------------------------------------------------------------

-- | The 'Object' type extends the 'Data.Dynamic.Dynamic' data type with a few more constructors for
-- data types that are fundamental to a programming language, like integers, strings, and lists.
data Object
  = ONull
  | OTrue
  | OChar      T_char
  | OInt       T_int
  | OWord      T_word
  | OLong      T_long
  | ORelTime   T_diffTime
  | OFloat     T_float
  | ORatio     T_ratio
  | OComplex   T_complex
  | OString    T_string
  | OBytes     T_bytes
  | OList      T_list
  | ODict      T_dict
  | ORef       T_ref
  | OType      T_type
  | OTree      T_struct
  | OAbsTime   T_time
  | OHaskell   HaskellData
  deriving (Eq, Ord, Typeable, Show)

type T_char     = Char
type T_int      = Int
type T_word     = Word64
type T_long     = Integer
type T_diffTime = NominalDiffTime
type T_float    = Double
type T_ratio    = Rational
type T_complex  = Complex
type T_string   = UStr
type T_bytes    = B.ByteString
type T_list     = [Object]
type T_dict     = M.Map Name Object
type T_ref      = Reference
type T_type     = ObjType
type T_struct   = Struct
type T_time     = UTCTime

instance NFData Object where
  rnf  ONull         = ()
  rnf  OTrue         = ()
  rnf (OChar      a) = deepseq a ()
  rnf (OInt       a) = deepseq a ()
  rnf (OWord      a) = deepseq a ()
  rnf (OLong      a) = deepseq a ()
  rnf (ORelTime   a) = deepseq a ()
  rnf (OFloat     a) = deepseq a ()
  rnf (ORatio     a) = deepseq a ()
  rnf (OComplex   a) = deepseq a ()
  rnf (OString    a) = deepseq a ()
  rnf (OBytes     a) = seq a ()
  rnf (OList      a) = deepseq a ()
  rnf (ODict      a) = deepseq a ()
  rnf (ORef       a) = deepseq a ()
  rnf (OType      a) = deepseq a ()
  rnf (OTree      a) = deepseq a ()
  rnf (OAbsTime   a) = deepseq a ()
  rnf (OHaskell   a) = deepseq a ()

instance Monoid (XPure Object) where
  mempty = return ONull
  mappend a b = a >>= \a -> b >>= \b -> case a of
    ONull     -> return b
    OTrue     -> case b of
      OTrue     -> return OTrue
      _         -> mzero
    a         -> case b of
      ONull     -> return a
      b         -> xpure a + xpure b

instance HasNullValue Object where
  nullValue = ONull
  testNull a = case a of
    ONull        -> True
    OChar     c  -> testNull c
    OInt      i  -> testNull i
    OWord     i  -> testNull i
    OLong     i  -> testNull i
    OFloat    f  -> testNull f
    ORelTime  s  -> testNull s
    ORatio    r  -> testNull r
    OComplex  c  -> testNull c
    OString   s  -> testNull s
    OBytes    o  -> testNull o
    OList     s  -> testNull s
    ODict     m  -> testNull m
    OTree     t  -> testNull t
    OHaskell  o  -> testNull o
    _            -> False

-- binary 0x08 0x1A Object-->CoreType
instance B.Binary Object MTab where
  put o = do
    let t   = B.put (typeOfObj o)
        p o = t >> B.put o
    case o of
      ONull      -> t
      OTrue      -> t
      OChar    o -> p o
      OInt     o -> p o
      OWord    o -> p o
      OLong    o -> p o
      ORelTime o -> p o
      OFloat   o -> p o
      ORatio   o -> p o
      OComplex o -> p o
      OString  o -> p o
      OBytes   o -> p o
      OList    o -> t >> B.putUnwrapped o
      ODict    o -> p o
      ORef     o -> p o
      OType    o -> p o
      OTree    o -> p o
      OAbsTime o -> p o
      OHaskell o -> B.put o
  get = B.word8PrefixTable <|> fail "expecting Object"

instance B.HasPrefixTable Object B.Byte MTab where
  prefixTable =
    let g f = fmap f B.get
    in  mappend (OTree <$> B.prefixTable) $ B.mkPrefixTableWord8 "Object" 0x08 0x1A $
          [ return ONull
          , return OTrue
          , g OChar
          , g OInt
          , g OWord
          , g OLong
          , g ORelTime
          , g OFloat
          , g ORatio
          , g OComplex
          , g OString
          , g OBytes
          , OList <$> B.getUnwrapped
          , g ODict
          , g ORef
          , g OType
          , g OTree
          , g OAbsTime
          , mplus (OHaskell <$> B.get)
                  (B.get >>= \ (B.BlockStream1M bs1m) -> return (OBytes bs1m))
          ]

----------------------------------------------------------------------------------------------------

-- | Direct a reference at a particular tree in the runtime.
data RefQualifier
  = UNQUAL -- ^ unqualified
  | LOCAL  -- ^ refers to the current local variable stack
  | CONST  -- ^ refers to a built-in constant
  | STATIC -- ^ a local variable stack specific to a 'Subroutine' that lives on even after the
           -- subroutine has completed.
  | GLOBAL -- ^ the global variable space for the current module.
  | GLODOT -- ^ a relative reference, gets it's name because it begins with a dot (".") character.
           -- Similar to the "this" keyword in C++ and Java, refers to the object of the current
           -- context set by the "with" statement, but defaults to the global variable space when
           -- not within a "with" statement. This is necessary to differentiate between local
           -- variables and references to the "with" context.
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance NFData RefQualifier where { rnf a = seq a () }

instance PPrintable RefQualifier where { pPrint = pUStr . toUStr }

instance PrecedeWithSpace RefQualifier where
   precedeWithSpace o = case o of
     LOCAL  -> True
     CONST  -> True
     STATIC -> True
     GLOBAL -> True
     _      -> False

instance UStrType RefQualifier where
  toUStr a = ustr $ case a of
    UNQUAL -> ""
    LOCAL  -> "local"
    CONST  -> "const"
    STATIC -> "static"
    GLOBAL -> "global"
    GLODOT -> "."
  maybeFromUStr str = case uchars str of
    "local"  -> Just LOCAL
    "const"  -> Just CONST
    "static" -> Just STATIC
    "global" -> Just GLOBAL
    "."      -> Just GLODOT
    ""       -> Just UNQUAL
    _        -> Nothing
  fromUStr str = maybe (error (show str++" is not a reference qualifier")) id (maybeFromUStr str)

instance HasRandGen RefQualifier where
  randO = fmap toEnum (nextInt (1+fromEnum (minBound::RefQualifier)))
  defaultO = randO

instance ToDaoStructClass RefQualifier where { toDaoStruct=putNullaryUsingShow; }

instance FromDaoStructClass RefQualifier where { fromDaoStruct=getNullaryWithRead; }

instance ObjectClass RefQualifier where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass RefQualifier where
  haskellDataInterface = interface LOCAL $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data Reference
  = Reference  RefQualifier Name RefSuffix
  | RefObject  Object RefSuffix
  | RefWrapper Reference
  deriving (Eq, Ord, Typeable, Show)

reference :: RefQualifier -> Name -> Reference
reference q name = Reference q name NullRef

refObject :: Object -> Reference
refObject = flip RefObject NullRef

instance NFData Reference where
  rnf (Reference q n r) = deepseq q $! deepseq n $! deepseq r ()
  rnf (RefObject o r  ) = deepseq o $! deepseq r ()
  rnf (RefWrapper  r  ) = deepseq r ()

instance PPrintable Reference where
  pPrint qref = case qref of
    Reference q n r -> case q of
      UNQUAL -> pInline [pPrint n, pPrint r]
      q      -> pInline [pPrint q, pString " ", pPrint n, pPrint r]
    RefObject o r -> pInline [pString "(", pPrint o, pString ")", pPrint r]
    RefWrapper  r -> pInline [pString "$", pPrint r]

-- binary 0x48 0x4E
instance B.Binary Reference MTab where
  put qref = case qref of
    Reference q n r -> prefix q $ B.put n >> B.put r where
      prefix q = B.prefixByte $ case q of
        { UNQUAL -> 0x48; LOCAL -> 0x49; CONST -> 0x4A; STATIC -> 0x4B; GLOBAL -> 0x4C; GLODOT -> 0x4D; }
    RefObject o r -> B.prefixByte 0x4E $ B.put o >> B.put r
    RefWrapper  r -> B.prefixByte 0x4F $ B.put r
  get = B.word8PrefixTable <|> fail "expecting Reference"

instance B.HasPrefixTable Reference B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "Reference" 0x48 0x4F $
    [ f UNQUAL, f LOCAL, f CONST, f STATIC, f GLOBAL, f GLODOT
    , return RefObject  <*> B.get <*> B.get
    , return RefWrapper <*> B.get
    ] where { f q = return (Reference q) <*> B.get <*> B.get }

instance HasRandGen Reference where
  randO = _randTrace "Reference" $ recurse $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return Reference <*> randO   <*> randO <*> randO
    , return RefObject <*> scrambO <*> randO
    , RefWrapper <$> scrambO
    ]
  defaultO = _randTrace "D.Reference" runDefaultChoice
  defaultChoice = randChoiceList $
    [ return Reference <*> defaultO <*> defaultO <*> defaultO
    , return RefObject <*> defaultO <*> defaultO
    ]

-- 'execute'-ing a 'Reference' will dereference it, essentially reading the
-- value associated with that reference from the 'ExecUnit'.
instance Executable Reference (Maybe (Reference, Object)) where { execute qref = referenceLookup qref }

instance RefReducible Reference where { reduceToRef = return }

refAppendSuffix :: Reference -> RefSuffix -> Reference
refAppendSuffix qref appref = case qref of
  Reference q name ref -> Reference q name (ref<>appref)
  RefObject   o    ref -> RefObject   o    (ref<>appref)
  RefWrapper      qref -> RefWrapper $ refAppendSuffix qref appref

referenceLookup :: Reference -> Exec (Maybe (Reference, Object))
referenceLookup qref = mplus resolve (return Nothing) where
  resolve = _resolveRefQualifier qref access getLocal getConst getStatic getGlobal getGloDot
  access ref = maybe mzero (objectReferenceAccess (Just qref) ref)
  getLocal  nm ref = asks execStack        >>= doLookup nm ref
  getConst  nm ref = (ConstantStore <$> asks builtinConstants) >>= doLookup nm ref
  getStatic nm ref = asks currentCodeBlock >>= doLookup nm ref
  getGlobal nm ref = asks globalData       >>= doLookup nm ref
  getGloDot nm ref = asks currentWithRef   >>= doLookup nm ref
  doLookup :: Store store => Name -> RefSuffix -> store -> Exec (Maybe Object)
  doLookup nm ref store = storeLookup store nm >>= xmaybe >>=
    objectReferenceAccess (Just qref) ref
    -- TODO: on exception, update the exception structure with information about the 'Reference'
    -- given above.

refNames :: [Name] -> Maybe Reference
refNames nx = case nx of
  []   -> Nothing
  n:nx -> Just $ Reference UNQUAL n $ refSuffixFromNames nx

referenceFromUStr :: UStr -> Maybe Reference
referenceFromUStr s = breakup [] $ uchars s where
  breakup refs s = case break (=='.') s of
    (n, '.':s) -> breakup (refs++[ustr n]) s
    (n, ""   ) -> refNames $ refs++[ustr n]
    _          -> Nothing

fmapReference :: (RefSuffix -> RefSuffix) -> Reference -> Reference
fmapReference fn ref = case ref of
  Reference q nm ref -> Reference q nm (fn ref)
  RefObject   o  ref -> RefObject   o  (fn ref)
  RefWrapper    qref -> RefWrapper $ fmapReference fn qref

setQualifier :: RefQualifier -> Reference -> Reference
setQualifier q ref = case ref of
  Reference _ name ref -> Reference q name ref
  RefObject   o    ref -> RefObject   o    ref
  RefWrapper      qref -> RefWrapper $ setQualifier q qref

modRefObject :: (Object -> Object) -> Reference -> Reference
modRefObject mod ref = case ref of
  RefObject o ref -> RefObject (mod o) ref
  ref             -> ref

-- | This function performs an update on a 'Reference', it is the complement to the 'referenceLookup'
-- function. Evaluating 'referenceUpdate' on a 'Reference' will write/update the value associated with
-- it.
referenceUpdate :: Reference -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
referenceUpdate qref upd = fmap snd <$> checkUNQUAL where
  checkUNQUAL = case qref of
    Reference UNQUAL _ _ -> mplus (resolve False qref) (resolve True $ setQualifier LOCAL qref)
    _                    -> resolve True qref
  onLocal  force nm ref = asks execStack        >>= doUpdate force nm ref
  onConst  force nm ref = if not force then mzero else execThrow $ obj $
    [obj "cannot modify constant variable", obj (Reference UNQUAL nm ref)]
  onStatic force nm ref = asks currentCodeBlock >>= doUpdate force nm ref
  onGlobal force nm ref = asks globalData       >>= doUpdate force nm ref
  onGloDot force nm ref = asks currentWithRef   >>= doUpdate force nm ref
  doUpdate :: Store store => Bool -> Name -> RefSuffix -> store -> Exec (Maybe Object)
  doUpdate force nm ref store = storeUpdate store nm $
    maybe (if force then return Nothing else mzero) (return . Just) >=>
      objectReferenceUpdate upd (Just qref) ref
    -- if 'force' is False, produce an update function that backtracks if the input is 'Nothing',
    -- otherwise produe an update function that never backtracks.
  access a b = objectReferenceUpdate upd (Just qref) a b
  resolve f qref =
    _resolveRefQualifier qref access (onLocal f) (onConst f) (onStatic f) (onGlobal f) (onGloDot f)
  -- TODO: on exception, update the exception structure with information about the 'Reference'
  -- given above.

-- Used by 'referenceUpdate' and 'referenceLookup': given a 'Reference' and six functions, resolves
-- which object store to be used based on the 'RefQualifier' in a 'Reference', and evaluates the
-- appropriate function. Unqualified references (marked with 'UNQUAL') will attempt to evaluate
-- several of the given functions in a particular order and returns the value returned by the first
-- function that does not backtrack, therfore 'referenceLookup' passes functions that backtrack when
-- the input is 'Nothing'.
-- The functions parameters passed will be evaluated in this order
-- 1. The function to be evaluated when a reference is not qualified but an object value constructed
--       with 'RefObject'.
-- 2. The function to be evaluated when the 'Reference' is a 'LOCAL' ref.
-- 3. The function to be evaluated when the 'Reference' is a 'CONST' ref.
-- 4. The function to be evaluated when the 'Reference' is a 'STATIC' ref.
-- 5. The function to be evaluated when the 'Reference' is a 'GLOBAL' ref.
-- 6. The function to be evaluated when the 'Reference' is a 'GLODOT' ref.
-- In the case of a 'Reference' marked with 'UNQUAL', four of the above functions are tried in this
-- order: the function for 'LOCAL's, the function for 'CONST's, the function for 'STATIC's, and
-- finally, the functionf for 'GLOBAL's. Each function is tried in an 'Control.Monad.msum' sequence,
-- so the first function that does notbacktrack has it's return value returned. If all functions
-- backtrack, 'Prelude.Nothing' is returned. Otherwise, the 'Reference' value used to select the
-- 'Object' value is returned with the selected 'Object' value. If the 'Reference' is marked
-- 'UNQUAL', this mark is modified to reflect which function returned the value. For example, if
-- every function backtracked except for the function one evaluated for 'GLOBAL' expressions, the
-- returned 'Reference' value is marked with the 'GLOBAL' qualifier.
_resolveRefQualifier
  :: Reference
  -> (RefSuffix -> Maybe Object -> Exec (Maybe Object)) -- parens optionally followed by dot
  -> (Name -> RefSuffix -> Exec (Maybe Object)) -- local store
  -> (Name -> RefSuffix -> Exec (Maybe Object)) -- const store
  -> (Name -> RefSuffix -> Exec (Maybe Object)) -- static store
  -> (Name -> RefSuffix -> Exec (Maybe Object)) -- global store
  -> (Name -> RefSuffix -> Exec (Maybe Object)) -- with-ref store
  -> Exec (Maybe (Reference, Object))
_resolveRefQualifier qref onObj onLocal onConst onStatic onGlobal onGloDot = case qref of
  RefWrapper    qref -> attachRef $ onObj NullRef $ Just $ obj qref
  RefObject o    ref -> attachRef $ onObj ref $ Just o
  Reference q nm ref -> msum $ case q of
    LOCAL  -> [f LOCAL  onLocal  nm ref]
    CONST  -> [f CONST  onConst  nm ref]
    STATIC -> [f STATIC onStatic nm ref]
    GLOBAL -> [f GLOBAL onGlobal nm ref]
    GLODOT -> [f GLODOT onGloDot nm ref, f GLOBAL onGlobal nm ref]
    UNQUAL ->
      [ f LOCAL  onLocal  nm ref, f CONST  onConst nm ref
      , f STATIC onStatic nm ref, f GLOBAL onGlobal nm ref
      ] -- The order of this list determines in what order the various variables stores should be
        -- searched when resolving an unqualified variable.
  where
    f q m nm ref = fmap (\o -> (Reference q nm ref, o)) <$> m nm ref
    attachRef = fmap (fmap (\o -> (qref, o)))

----------------------------------------------------------------------------------------------------

-- $Object_types
-- Here we have a lambda calculus for describing types. Computationally, it is very similar to the
-- Prolog programming language, however an 'ObjType' is written using a subset the Dao scripting
-- langauge.

data CoreType
  = NullType
  | TrueType
  | CharType
  | IntType
  | WordType
  | LongType
  | DiffTimeType
  | FloatType
  | RatioType
  | ComplexType
  | StringType
  | BytesType
  | ListType
  | DictType
  | RefType
  | TypeType
  | TreeType
  | TimeType
  | HaskellType
  deriving (Eq, Ord, Typeable, Enum, Bounded)

instance Show CoreType where
  show t = case t of
    NullType     -> "Null"
    TrueType     -> "True"
    CharType     -> "Char"
    IntType      -> "Int"
    WordType     -> "Word"
    LongType     -> "Long"
    DiffTimeType -> "Diff"
    FloatType    -> "Float"
    RatioType    -> "Ratio"
    ComplexType  -> "Complex"
    StringType   -> "String"
    BytesType    -> "Bytes"
    ListType     -> "List"
    DictType     -> "Dict"
    RefType      -> "Ref"
    TypeType     -> "Type"
    TreeType     -> "Tree"
    TimeType     -> "Time"
    HaskellType  -> "Haskell"

instance Read CoreType where
  readsPrec _ str = map (\a -> (a, "")) $ case str of
    "Null"    -> [NullType]
    "True"    -> [TrueType]
    "Char"    -> [CharType]
    "Int"     -> [IntType]
    "Word"    -> [WordType]
    "Long"    -> [LongType]
    "Diff"    -> [DiffTimeType]
    "Float"   -> [FloatType]
    "Ratio"   -> [RatioType]
    "Complex" -> [ComplexType]
    "String"  -> [StringType]
    "Bytes"   -> [BytesType]
    "List"    -> [ListType]
    "Dict"    -> [DictType]
    "Ref"     -> [RefType]
    "Type"    -> [TypeType]
    "Tree"    -> [TreeType]
    "Time"    -> [TimeType]
    "Haskell" -> [HaskellType]
    _         -> []

instance NFData CoreType where { rnf a = seq a () }

instance UStrType CoreType where
  toUStr = derive_ustr
  maybeFromUStr a = case readsPrec 0 (uchars a) of
    [(o, "")] -> Just o
    _         -> Nothing
  fromUStr a = case maybeFromUStr a of
    Nothing -> error (show a++" is not a valid type identifier")
    Just  a -> a

instance Es.InfBound CoreType where
  minBoundInf = Es.Point minBound
  maxBoundInf = Es.Point maxBound

instance PPrintable CoreType where { pPrint = pShow }

-- binary 0x08 0x1A CoreType
instance B.Binary CoreType mtab where
  put t = B.putWord8 $ case t of
    NullType     -> 0x08
    TrueType     -> 0x09
    CharType     -> 0x0A
    IntType      -> 0x0B
    WordType     -> 0x0C
    LongType     -> 0x0D
    DiffTimeType -> 0x0E
    FloatType    -> 0x0F
    RatioType    -> 0x10
    ComplexType  -> 0x11
    StringType   -> 0x12
    BytesType    -> 0x13
    ListType     -> 0x14
    DictType     -> 0x15
    RefType      -> 0x16
    TypeType     -> 0x17
    TreeType     -> 0x18
    TimeType     -> 0x19
    HaskellType  -> 0x1A
  get = B.word8PrefixTable <|> fail "expecting CoreType"

instance B.HasPrefixTable CoreType B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "CoreType" 0x08 0x1A $ map return $
    [ NullType
    , TrueType
    , CharType
    , IntType
    , WordType
    , LongType
    , DiffTimeType
    , FloatType
    , RatioType
    , ComplexType
    , StringType
    , BytesType
    , ListType
    , DictType
    , RefType
    , TypeType
    , TreeType
    , TimeType
    , HaskellType
    ]

instance HasRandGen CoreType where
  randO = toEnum <$> nextInt (fromEnum (maxBound::CoreType))
  defaultO = randO

typeOfObj :: Object -> CoreType
typeOfObj o = case o of
  ONull      -> NullType
  OTrue      -> TrueType
  OChar    _ -> CharType
  OInt     _ -> IntType
  OWord    _ -> WordType
  OLong    _ -> LongType
  ORelTime _ -> DiffTimeType
  OFloat   _ -> FloatType
  ORatio   _ -> RatioType
  OComplex _ -> ComplexType
  OString  _ -> StringType
  OBytes   _ -> BytesType
  OList    _ -> ListType
  ODict    _ -> DictType
  ORef     _ -> RefType
  OType    _ -> TypeType
  OTree    _ -> TreeType
  OAbsTime _ -> TimeType
  OHaskell _ -> HaskellType

----------------------------------------------------------------------------------------------------

-- | A symbol in the type calculus.
data TypeSym
  = CoreType CoreType
    -- ^ used when the type of an object is equal to it's value, for example Null and True,
    -- or in situations where the type of an object has a value, for example the dimentions of a
    -- matrix.
  | TypeVar  Name [ObjType]
    -- ^ a polymorphic type, like 'AnyType' but has a name.
  deriving (Eq, Ord, Show, Typeable)

instance NFData TypeSym where
  rnf (CoreType a  ) = deepseq a ()
  rnf (TypeVar  a b) = deepseq a $! deepseq b ()

instance HasRandGen TypeSym where
  randO = _randTrace "TypeSym" $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [CoreType <$> randO, scramble $ return TypeVar <*> randO <*> randList 1 4]
  defaultO = _randTrace "D.TypeSym" $ CoreType <$> defaultO

instance PPrintable TypeSym where
  pPrint t = case t of
    CoreType t     -> pPrint t
    TypeVar  t ctx -> pInline $
      concat [[pPrint t], guard (not (null ctx)) >> [pList_ "[" ", " "]" (map pPrint ctx)]]

-- binary 0x2E 0x2F
instance B.Binary TypeSym mtab where
  put o = case o of
    CoreType o       -> B.prefixByte 0x2E $ B.put o
    TypeVar  ref ctx -> B.prefixByte 0x2F $ B.put ref >> B.put ctx
  get = B.word8PrefixTable <|> fail "expecting TypeSym"

instance B.HasPrefixTable TypeSym B.Byte mtab where
  prefixTable =
    B.mkPrefixTableWord8 "TypeSym" 0x2E 0x2F [CoreType <$> B.get, return TypeVar <*> B.get <*> B.get]

----------------------------------------------------------------------------------------------------

-- | Complex type structures can be programmed by combining 'ObjSimpleType's. An empty 'TypeStruct'
-- is the "any-type", which matches anything.
newtype TypeStruct = TypeStruct [TypeSym] deriving (Eq, Ord, Show, Typeable)

instance NFData TypeStruct where { rnf (TypeStruct a) = deepseq a () }

instance HasNullValue TypeStruct where { nullValue = TypeStruct []; testNull (TypeStruct a) = null a; }

instance PPrintable TypeStruct where
  pPrint (TypeStruct tx) = case tx of
    [] -> pString "AnyType"
    tx -> pList (pString "type") "(" ", " ")" (map pPrint tx)

-- binary 0x33 
instance B.Binary TypeStruct mtab where
  put (TypeStruct o) = B.prefixByte 0x33 $ B.put o
  get = B.word8PrefixTable <|> fail "expecting TypeStruct"

instance B.HasPrefixTable TypeStruct B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "TypeStruct" 0x33 0x33 [TypeStruct <$> B.get]

instance HasRandGen TypeStruct where
  randO    = _randTrace "TypeStruct" $ TypeStruct <$> randList 0 4
  defaultO = _randTrace "D.TypeStruct" $ TypeStruct <$> defaultList 0 4

----------------------------------------------------------------------------------------------------

-- | The fundamental 'Type' used to reason about whether an object is fit to be used for a
-- particular function. Any empty 'ObjType' is the "void-type" which matches nothing.
newtype ObjType = ObjType { typeChoices :: [TypeStruct] } deriving (Eq, Ord, Show, Typeable)

instance NFData ObjType where { rnf (ObjType a) = deepseq a () }

instance HasNullValue ObjType where { nullValue = ObjType []; testNull (ObjType a) = null a; }

instance PPrintable ObjType where
  pPrint t@(ObjType tx) = case fromObj (obj t) of
    Just  t -> pString $ show (t::CoreType)
    Nothing -> case tx of
      [] -> pString "VoidType"
      tx -> pList (pString "anyOf") "(" ", " ")" (map pPrint tx)

-- binary 0x37 
instance B.Binary ObjType mtab where
  put (ObjType o) = B.prefixByte 0x37 $ B.put o
  get = B.word8PrefixTable <|> fail "expecting ObjType"

instance B.HasPrefixTable ObjType B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "ObjType" 0x37 0x37 [ObjType <$> B.get]

instance HasRandGen ObjType where
  randO = _randTrace "ObjType" $ recurse $ ObjType <$> randList 0 3
  defaultO = _randTrace "D.ObjType" $ ObjType <$> defaultList 1 4

coreType :: CoreType -> ObjType
coreType = ObjType . return . TypeStruct . return . CoreType

----------------------------------------------------------------------------------------------------

-- | This is actually a part of the 'Reference' constructor, and 'Reference' is one of the built-in
-- 'Object' data types.  There is a one-to-one mapping from this type to the 'RefSuffixExpr' and
-- 'AST_Ref' data types produced by the parser.
data RefSuffix
  = NullRef
  | DotRef     Name    RefSuffix
  | Subscript [Object] RefSuffix
  | FuncCall  [Object] RefSuffix
  deriving (Eq, Ord, Typeable, Show)

-- | Construct a 'DotRef' with a 'NullRef' suffix.
dotRef :: Name -> RefSuffix
dotRef = flip DotRef NullRef

-- | Construct a 'Subscript' with a 'NullRef' suffix.
subscript :: [Object] -> RefSuffix
subscript = flip Subscript NullRef

-- | Construct a 'FuncCall' with a 'NullRef' suffix.
funcCall :: [Object] -> RefSuffix
funcCall = flip FuncCall NullRef

instance Monoid RefSuffix where
  mempty = NullRef
  mappend left right = case left of
    NullRef           -> right
    DotRef    nm left -> DotRef    nm $ left<>right
    Subscript ox left -> Subscript ox $ left<>right
    FuncCall  ox left -> FuncCall  ox $ left<>right

instance HasNullValue RefSuffix where
  nullValue = NullRef
  testNull r = case r of { NullRef -> True; _ -> False }

refSuffixFromNames :: [Name] -> RefSuffix
refSuffixFromNames nx = case nx of { [] -> NullRef; n:nx -> DotRef n $ refSuffixFromNames nx; }

instance Read RefSuffix where
  readsPrec _ str = case str of
    '.':c:str  | isAlpha c ->
      case break (\c -> c=='.' || isAlphaNum c) (c:str) of
        (cx, str) ->
          maybe [] (return . (\ref -> (refSuffixFromNames ref, str))) $ sequence $
            fix (\loop str -> case break (=='.') str of
                    (cx, str) -> case cx of
                      [] -> []
                      cx -> maybeFromUStr (ustr (dropWhile (=='.') cx)) : loop str
                ) cx
    str -> [(NullRef, str)]

instance NFData RefSuffix where
  rnf  NullRef        = ()
  rnf (DotRef    a b) = deepseq a $! deepseq b ()
  rnf (Subscript a b) = deepseq a $! deepseq b ()
  rnf (FuncCall  a b) = deepseq a $! deepseq b ()

instance PPrintable RefSuffix where
  pPrint = pWrapIndent . loop where 
    loop r = case r of
      NullRef       -> []
      DotRef    a b -> pString "." : pUStr (toUStr a) : loop b
      Subscript a b -> pList_ "[" ", " "]" (map pPrint a) : loop b
      FuncCall  a b -> pList_ "(" ", " ")" (map pPrint a) : loop b

instance HasRandGen RefSuffix where
  randO = _randTrace "RefSuffix" $ recurse $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return NullRef
    , scramble $ return DotRef <*> randO <*> randO
    , depthLimitedInt 8 >>= \x -> return Subscript <*> randList 0 x <*> scrambO
    , depthLimitedInt 8 >>= \x -> return FuncCall  <*> randList 0 x <*> scrambO
    ]
  defaultO = _randTrace "D.RefSuffix" runDefaultChoice
  defaultChoice = randChoiceList $ 
    [ return NullRef
    , return Subscript <*> defaultList 0 1 <*> pure NullRef
    , return FuncCall  <*> defaultList 0 1 <*> pure NullRef
    ]

-- binary 0x42 0x45
instance B.Binary RefSuffix MTab where
  put r = case r of
    NullRef       -> B.putWord8   0x42
    DotRef    a b -> B.prefixByte 0x43 $ B.put a >> B.put b
    Subscript a b -> B.prefixByte 0x44 $ B.put a >> B.put b
    FuncCall  a b -> B.prefixByte 0x45 $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting RefSuffix"

instance B.HasPrefixTable RefSuffix B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RefSuffix" 0x42 0x45 $
    [ return NullRef
    , return DotRef    <*> B.get <*> B.get
    , return Subscript <*> B.get <*> B.get
    , return FuncCall  <*> B.get <*> B.get
    ]

----------------------------------------------------------------------------------------------------

newtype Complex = Complex (C.Complex Double)
  deriving (Eq, Typeable, Floating, Fractional, Num)

-- | Since 'Object' requires all of it's types instantiate 'Prelude.Ord', I have defined
-- 'Prelude.Ord' of 'Data.Complex.Complex' numbers to be the distance from 0, that is, the radius of
-- the polar form of the 'Data.Complex.Complex' number, ignoring the angle argument.
instance Ord Complex where
  compare (Complex a) (Complex b) = compare (C.polar a) (C.polar b)

instance Show Complex where
  show (Complex a) = "("++show re++(if im<0 then "-" else "+")++show im++"i)" where
    re = C.realPart a
    im = C.imagPart a

instance NFData Complex where { rnf (Complex a) = deepseq a $! () }

instance HasNullValue Complex where
  nullValue = Complex (0 C.:+ 0)
  testNull (Complex c) = C.realPart c == 0 && C.imagPart c == 0

instance B.Binary Complex mtab where
  put o = B.put (realPart o) >> B.put (imagPart o)
  get   = return complex <*> B.get <*> B.get

instance PPrintable Complex where
  pPrint (Complex (a C.:+ b))
    | a==0.0 && b==0.0 = pString "0i"
    | a==0.0           = pString (show b++"i")
    | b==0.0           = pShow a
    | otherwise        = pInline [pShow a, pString (if b<0 then "-" else "+"), pString (show b++"i")]

instance HasRandGen Complex where { randO = return mkPolar <*> randO <*> randO; defaultO = randO; }

realPart :: Complex -> Double
realPart (Complex o) = C.realPart o

imagPart :: Complex -> Double
imagPart (Complex o) = C.imagPart o

mkPolar :: Double -> Double -> Complex
mkPolar a b = Complex (C.mkPolar a b)

cis :: Double -> Complex
cis = Complex . C.cis

polar :: Complex -> (Double, Double)
polar (Complex o) = C.polar o

magnitude :: Complex -> Double
magnitude (Complex o) = C.magnitude o

phase :: Complex -> Double
phase (Complex o) = C.phase o

conjugate :: Complex -> Complex
conjugate (Complex o) = Complex (C.conjugate o)

complex :: Double -> Double -> Complex
complex a b = Complex (a C.:+ b)

----------------------------------------------------------------------------------------------------

-- | Create the minimum-sized array that can store all of the indices in the given list, setting the
-- 'Data.Array.IArray.bounds' of the array automatically. Evaluates to 'Prelude.Nothing' if the
-- given list of elements is empty.
minAccumArray :: Ix i => (e -> e' -> e) -> e -> [(i, e')] -> Maybe (Array i e)
minAccumArray accfn deflt elems =
  if null elems then Nothing else Just (accumArray accfn deflt bnds elems) where
    idxs = map fst elems
    i0   = head idxs
    bnds = foldl (\ (lo, hi) i -> (min lo i, max hi i)) (i0, i0) (tail idxs)

-- | Create the minimum-sized array that can store all of the indices in the given list, and setting
-- the 'Data.Array.IArray.bounds' of the array automatically. Evaluates to 'Prelude.Nothing' if the
-- given list of elements is empty.
minArray :: Ix i => e -> [(i, e)] -> Maybe (Array i e)
minArray deflt elems = minAccumArray (flip const) deflt elems

----------------------------------------------------------------------------------------------------

-- not for export
lu :: Location
lu  = LocationUnknown
fd :: HasLocation a => a -> a
fd = delLocation
fd1 :: (HasLocation a, Functor f) => f a -> f a
fd1 = fmap delLocation

----------------------------------------------------------------------------------------------------

-- | An alternative to 'Glob' expressions containing ordinary 'Dao.String.UStr's is a 'Glob'
-- expression containing 'FuzzyStr's. These strings approximately match the input string, ignoring
-- minor spelling errors and transposed characters.
newtype FuzzyStr = FuzzyStr UStr deriving (Ord, Typeable)

instance Eq FuzzyStr where
  a==b = 
    let ax = S.map toLower (S.fromList (uchars a))
        bx = S.map toLower (S.fromList (uchars b))
    in     a == b
        || ax == bx
        || S.size (S.difference (S.union ax bx) (if S.size ax < S.size bx then ax else bx)) <= 1

instance Show FuzzyStr where { show (FuzzyStr str) = show str }

instance Read FuzzyStr where
  readsPrec p input = readsPrec p input >>= \ (s, rem) -> return (FuzzyStr (ustr s), rem)

instance Monoid FuzzyStr where
  mempty = FuzzyStr mempty
  mappend (FuzzyStr a) (FuzzyStr b) = FuzzyStr (a<>b)

instance HasNullValue FuzzyStr where
  nullValue = FuzzyStr nullValue
  testNull (FuzzyStr s) = testNull s

instance UStrType FuzzyStr where { fromUStr = FuzzyStr; toUStr (FuzzyStr u) = u; }

instance Show (Glob FuzzyStr) where { show = show . fmap toUStr }

instance Read (Glob FuzzyStr) where
  readsPrec prec str = readsPrec prec str >>= \ (glob, str) -> [(fmap fromUStr glob, str)]

instance Show (GlobUnit Object) where
  show o = case o of
    Single o -> show o
    globunit -> show (fmap (const "") globunit)

instance Show (Glob Object) where
  show glob = (++"\"") $ ('"':) $ do
    o <- getPatUnits glob
    let other o = "$("++prettyShow o++")"
    case o of
      Single o -> case o of
        OString  o -> uchars o
        OHaskell (HaskellData dyn _ifc) -> case fromDynamic dyn of
          Nothing           -> other o
          Just (FuzzyStr o) -> uchars o
        _ -> other o
      globunit -> show (fmap (const "") globunit)

instance PPrintable (Glob Object) where { pPrint = pShow }

----------------------------------------------------------------------------------------------------

-- | This is the state that is used to run the evaluation algorithm. Every Dao program file that has
-- been loaded will have a single 'ExecUnit' assigned to it. Parameters that are stored in
-- 'Dao.Debug.DMVar's or 'Dao.Type.Resource's will be shared across all rules which are executed in
-- parallel, so for example 'execHeap' contains the variables global to all rules in a given
-- program. The remainder of the parameters, those not stored in 'Dao.Debug.DMVar's or
-- 'Dao.Type.Resource's, will have a unique copy of those values assigned to each rule as it
-- executes.
data ExecUnit
  = ExecUnit
    { globalMethodTable  :: MethodTable
      -- ^ In this slot will be stored a read-only @'Data.Map.Lazy.Map' 'Dao.String.UStr'
      -- 'Interface'@ object that will allow any method with access to this
      -- 'GenRuntime' to retrieve a 'Interface' by it's name string. Specifically,
      -- this will be used by objects stored in the 'OHaskell' constructor.
    , pathIndex          :: MVar (M.Map UPath ExecUnit)
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , defaultTimeout     :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , importGraph        :: MVar (M.Map UPath ExecUnit)
    , currentWithRef     :: WithRefStore
      -- ^ the current document is set by the @with@ statement during execution of a Dao script.
    , taskForExecUnits   :: Task
    , currentQuery       :: Maybe UStr
    , currentPattern     :: Maybe (Glob Object)
    , currentCodeBlock   :: StaticStore
      -- ^ when evaluating a 'Subroutine' selected by a string query, the action resulting from
      -- that query is defnied here. It is only 'Data.Maybe.Nothing' when the module is first being
      -- loaded from source code.
    , currentBranch      :: [Name]
      -- ^ set by the @with@ statement during execution of a Dao script. It is used to prefix this
      -- to all global-dot references before reading from or writing to those references.
    , execStack          :: LocalStore
      -- ^ stack of local variables used during evaluation
    , globalData         :: GlobalStore
    , providedAttributes :: M.Map UStr ()
    , builtinConstants   :: M.Map Name Object
    , execOpenFiles      :: IORef (M.Map UPath ExecUnit)
    , programModuleName  :: Maybe UPath
    , preExec            :: [Subroutine]
      -- ^ the "guard scripts" that are executed before every string execution.
    , postExec           :: [Subroutine]
      -- ^ the "guard scripts" that are executed after every string execution.
    , quittingTime       :: [Subroutine]
    , programTokenizer   :: Object -- TODO: needs to be set after evaluating module top-level
    , ruleSet            :: IORef (PatternTree Object [Subroutine])
    , lambdaSet          :: IORef [CallableCode]
    , uncaughtErrors     :: IORef [ExecControl]
    , runtimeRefTable    :: RefTable Object Dynamic
    }

-- Initializes a completely empty 'ExecUnit'
_initExecUnit :: IO ExecUnit
_initExecUnit = do
  paths    <- newMVar mempty
  igraph   <- newMVar mempty
  global   <- newMVar M.empty
  execTask <- initTask
  xstack   <- newIORef emptyStack
  files    <- newIORef M.empty
  rules    <- newIORef T.Void
  lambdas  <- newIORef []
  uncaught <- newIORef []
  reftable <- newRefTable
  return $
    ExecUnit
    { globalMethodTable  = mempty
    , pathIndex          = paths
    , defaultTimeout     = Nothing
    , importGraph        = igraph
    , currentWithRef     = WithRefStore Nothing
    , currentQuery       = Nothing
    , currentPattern     = Nothing
    , currentCodeBlock   = StaticStore Nothing
    , currentBranch      = []
    , globalData         = GlobalStore global
    , providedAttributes = M.empty
    , builtinConstants   = M.empty
    , taskForExecUnits   = execTask
    , execStack          = LocalStore xstack
    , execOpenFiles      = files
    , programModuleName  = Nothing
    , preExec            = []
    , quittingTime       = mempty
    , programTokenizer   = ONull
    , postExec           = []
    , ruleSet            = rules
    , lambdaSet          = lambdas
    , uncaughtErrors     = uncaught
    , runtimeRefTable    = reftable
    }

-- | Creates a new 'ExecUnit'. This is the only way to create a new 'ExecUnit', and it must be run
-- within the 'Exec' monad. The 'ExecUnit' produced by this function will have it's parent
-- 'ExecUnit' set to the value returned by the 'Control.Monad.Reader.Class.ask' instance of the
-- 'Exec' monad.
--
-- The parent of all other 'ExecUnit's, the root of the family tree, is initalized internally by the
-- 'startDao' function.
newExecUnit :: Maybe UPath -> Exec ExecUnit
newExecUnit modName = ask >>= \parent -> liftIO _initExecUnit >>= \child -> return $
  child
  { programModuleName = modName
  , builtinConstants  = builtinConstants  parent
  , defaultTimeout    = defaultTimeout    parent
  , globalMethodTable = globalMethodTable parent
  , runtimeRefTable   = runtimeRefTable   parent
  }

----------------------------------------------------------------------------------------------------

-- | A 'Task' is simply a group of threads executing in parallel, but evaluating a task is still
-- synchronous, i.e. evaluating 'taskLoop' on a 'Task' will block until every thread in the task has
-- completed.
data Task
  = Task
    { taskWaitChan       :: Chan (ThreadId, Int)
    , taskRunningThreads :: MVar (S.Set ThreadId)
    }

-- | Create a new 'Task'.
initTask :: IO Task
initTask = do
  wait    <- newChan
  running <- newMVar S.empty
  return $ Task{ taskWaitChan=wait, taskRunningThreads=running }

-- | To halt a single thread in a 'Task', simply signal it with 'Control.Concurrent.killThread'. But
-- to halt everything the task is doing, use this function. Use of this function will never result in
-- deadlocks (I hope).
throwToTask :: Exception e => Task -> e -> IO ()
throwToTask task e = do
  let mvar = taskRunningThreads task
  ((S.elems <$> readMVar mvar) >>= mapM_ (flip throwTo e))
    `finally` getChanContents (taskWaitChan task) >> return ()

-- | Like 'throwToTask', but throws 'Control.Exception.ThreadKilled'.
killTask :: Task -> IO ()
killTask = flip throwToTask ThreadKilled

-- | This is a better way to manage a 'Task' because all tasks evaluated are waited for
-- synchronously, but you can provide a callback that is evaluated after each task completes. This
-- prevents exceptions from occurring, for example:
-- > "thread blocked indefinitely in an MVar operation"
-- 
-- Provide a list of IO functions to be evaluated in parallel. Also provide a callback function
-- that will be evaluated after each thread completes. This function should take two parameters and
-- return a bool: the 'Control.Concurrent.ThreadId' of the thread that completed and a positive
-- integer value indicating the number of threads that are still running, and the bool returned
-- should indicate whether or not the loop should continue. If you should halt the loop by returning
-- 'Prelude.False', the threads in the task that are still running will continue running, and you
-- should call 'killTask' after 'taskLoop' to halt them if halting them should be necessary.
-- 
-- This function is also exception safe. All tasks evaluated in parallel will not fail to singal the
-- callback, even if the thread halts with an exception or asynchronous signal from a function like
-- 'Control.Concurrent.killThread'. If the thread evaluating this function is halted by an
-- exception, all threads in the 'Task' are also killed.
taskLoop :: Task -> [IO ()] -> (ThreadId -> Int -> IO Bool) -> IO ()
taskLoop task parallelIO threadHaltedEvent = unless (null parallelIO) $
  (do mapM_ (forkInTask task) parallelIO
      fix $ \loop -> waitFirst task >>= \ (thread, remain) ->
        threadHaltedEvent thread remain >>= \contin -> unless (not contin || remain==0) loop
  ) `onException` killTask task
  where
    waitFirst :: Task -> IO (ThreadId, Int)
    waitFirst task = readChan (taskWaitChan task)
    forkInTask :: Task -> IO () -> IO ThreadId
    forkInTask task run = forkIO $ do
      self <- myThreadId
      bracket
        (modifyMVar (taskRunningThreads task) $ \s' -> do
            let s = S.delete self s'
            return (s, S.size s)
        )
        (\i -> writeChan (taskWaitChan task) (self, i))
        (\ _i -> run)

-- | Works exactly like 'taskLoop', except you do not need to provide a callback function to be
-- evaluated after every task completes. Essentially, every IO function is evaluated in the 'Task'
-- in parallel, and this function blocks until all tasks have completed.
taskLoop_ :: Task -> [IO ()] -> IO ()
taskLoop_ task inits = taskLoop task inits (\ _ _ -> return True)

----------------------------------------------------------------------------------------------------

-- | This simple, humble little class is one of the most important in the Dao program because it
-- defines the 'execute' function. Any data type that can result in procedural execution in the
-- 'Exec' monad can instantiate this class. This will allow the instnatiated data type to be used as
-- a kind of executable code that can be passed around and evaluated at arbitrary points in your Dao
-- program.
-- 
-- Note that there the @result@ type parameter is functionally dependent on the @exec@ type
-- parameter. This guarantees there is a one-to-one mapping from independent @exec@ types to
-- dependent @result@ types, i.e. if you data type @MyDat@ maps to a data type @Rzlt@, then @Rzlt@
-- is the only possible data type that could ever be evaluated by 'execute'-ing the @MyDat@
-- function.
--
-- As a reminder, functional dependencies do not necessitate a one-to-one mapping from the
-- dependent type to the independent type, so the @result@ parameter may be the same for many
-- different @exec@ types. But once the compiler infers that the @exec@ parameter of the 'Executable'
-- class is @MyDat@, the @result@ type /must/ be @Rzlt@ and nothing else.
-- > instance Executable MyDat Rzlt
-- > instance Executable A     () -- OK (different @exec@ parameters, same @result@ parameters)
-- > instance Executable B     () -- OK
-- > instance Executable C     () -- OK
-- > 
-- > instance Executable D     ()   -- COMPILER ERROR (same @exec@ parameters, different @result@ parameters)
-- > instance Executable D     Int  -- COMPILER ERROR
-- > instance Executable D     Char -- COMPILER ERROR
-- In this example, should D instantiate () or Int or Char as it's result? You must choose only one.
class Executable exec result | exec -> result where { execute :: exec -> Exec result }

-- | This class provides a function that allows a executable expression to possibly be reduced to a
-- 'Reference'. This is used on the expressions to the left of an assignment operator. This is very
-- different from 'asReference', which is a function for checking if a given 'Object' was
-- constructed with the 'ORef' constructor containing a 'Reference' value. 'Refereneable' actually
-- converts a data type to a 'Reference' through a series of reduction steps.
-- > refToORef :: 'ObjectExpr' -> 'Exec' 'Object'
-- > refToORef = 'Prelude.fmap' 'ORef' . 'reduceToRef'
-- > 
-- > refFromORef :: 'Object' -> 'Exec' Reference
-- > refFromORef = 'execute' . 'asReference'
-- This function will backtrack when the data type cannot be converted to a 'Reference'.
class RefReducible o where { reduceToRef :: o -> Exec Reference }

instance RefReducible Object where
  reduceToRef o = return $ case o of
    ORef r -> r
    o      -> RefObject o NullRef

----------------------------------------------------------------------------------------------------

-- | Since the 'ExecUnit' deals with a few different kinds of pointer values, namely
-- 'Data.IORef.IORef' and 'MVar', which all have similar functions for reading and updating, I have
-- defined this class to provide a consistent set of functions for working with the various pointers
-- data types.
class ExecRef var where
  execReadRef    :: var a -> Exec a
  execTakeRef    :: var a -> Exec a
  execPutRef     :: var a -> a -> Exec ()
  execSwapRef    :: var a -> a -> Exec a
  execModifyRef  :: var a -> (a -> Exec (a, b)) -> Exec b
  execModifyRef_ :: var a -> (a -> Exec  a    ) -> Exec ()
  execModifyRef_ var upd = execModifyRef var (\a -> upd a >>= \a -> return (a, ()))

instance ExecRef MVar where
  execModifyRef mvar upd = do
    xunit <- ask
    (>>=predicate) $ liftIO $ modifyMVar mvar $ \var -> do
      result <- flip ioExec xunit $ execCatchIO (upd var) $
        [ newExecIOHandler $ \e -> execThrow $ obj $
            [obj "ErrorCall:", obj (show (e::ErrorCall))]
        , newExecIOHandler $ \e -> execThrow $ obj $
            [obj "IOException:" , obj (show (e::IOException))]
        ]
      return $ case result of
        Backtrack   -> (var, Backtrack )
        OK (var, o) -> (var, OK       o)
        PFail   err -> (var, PFail  err)
  execModifyRef_ mvar upd = execModifyRef mvar (\var -> upd var >>= \var -> return (var, ()))
  execReadRef      = liftIO . readMVar
  execTakeRef      = liftIO . takeMVar
  execPutRef  mvar = liftIO . putMVar  mvar
  execSwapRef mvar = liftIO . swapMVar mvar

instance ExecRef IORef where
  execModifyRef  ref upd = liftIO (readIORef ref) >>= upd >>= \ (var, b) -> liftIO (writeIORef ref var) >> return b
  execModifyRef_ ref upd = liftIO (readIORef ref) >>= upd >>= liftIO . writeIORef ref
  execReadRef            = liftIO . readIORef
  execTakeRef            = execReadRef
  execPutRef     ref     = liftIO . writeIORef ref
  execSwapRef    ref obj = liftIO (readIORef ref >>= \sw -> writeIORef ref obj >> return sw)

----------------------------------------------------------------------------------------------------

class Store store where
  storeLookup :: store -> Name -> Exec (Maybe Object)
  storeUpdate :: store -> Name -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
  storeDefine :: store -> Name -> Object -> Exec ()
  storeDefine store nm o = storeUpdate store nm (return . const (Just o)) >> return ()
  storeDelete :: store -> Name -> Exec ()
  storeDelete store nm = storeUpdate store nm (return . const Nothing) >> return ()

instance ExecRef ref => Store (ref T_dict) where
  storeLookup store ref     = fmap (M.lookup ref    ) (execReadRef    store)
  storeDefine store ref obj = execModifyRef_ store (return . M.insert ref obj)
  storeDelete store ref     = execModifyRef_ store (return . M.delete ref    )
  storeUpdate store ref upd = execModifyRef  store $ \tree -> do
    obj <- upd (M.lookup ref tree)
    return $ case obj of
      Nothing  -> (M.delete ref     tree, Nothing)
      Just obj -> (M.insert ref obj tree, Just obj)

instance ExecRef ref => Store (ref (Stack Name Object)) where
  storeLookup store ref     = execReadRef store >>= return . stackLookup ref
  storeDefine store ref obj = execModifyRef_ store (return . stackDefine ref (Just obj))
  storeDelete store ref     = execModifyRef_ store (return . stackDefine ref Nothing)
  storeUpdate store ref upd = execModifyRef  store (stackUpdateM upd ref)

newtype LocalStore  = LocalStore  (IORef (Stack Name Object))

instance Store LocalStore where
  storeLookup (LocalStore  store) = storeLookup store
  storeDefine (LocalStore  store) = storeDefine store
  storeDelete (LocalStore  store) = storeDelete store
  storeUpdate (LocalStore  store) ref upd = storeUpdate store ref upd

newtype GlobalStore = GlobalStore (MVar T_dict)

instance Store GlobalStore where
  storeLookup (GlobalStore store) nm = storeLookup store nm
  storeDefine (GlobalStore store) = storeDefine store
  storeDelete (GlobalStore store) = storeDelete store
  storeUpdate (GlobalStore store) ref upd = storeUpdate store ref upd

newtype StaticStore = StaticStore (Maybe Subroutine)

instance Store StaticStore where
  storeLookup (StaticStore store) ref     =
    maybe mzero (\store -> storeLookup store ref    ) (fmap staticVars store)
  storeDefine (StaticStore store) ref obj =
    maybe mzero (\store -> storeDefine store ref obj) (fmap staticVars store)
  storeDelete (StaticStore store) ref     =
    maybe mzero (\store -> storeDelete store ref    ) (fmap staticVars store)
  storeUpdate (StaticStore store) ref upd =
    maybe mzero (\store -> storeUpdate store ref upd) (fmap staticVars store)

newtype WithRefStore = WithRefStore (Maybe (IORef Object))

_withRefStore :: WithRefStore -> (IORef Object -> Exec b) -> Exec b
_withRefStore (WithRefStore o) upd = maybe mzero upd o

instance Store WithRefStore where
  storeLookup sto name     = _withRefStore sto $ -- this is such an incredibly point-free function!
    liftIO . readIORef >=> objectReferenceAccess (Just $ Reference GLODOT name NullRef) NullRef
  storeUpdate sto name upd = _withRefStore sto $ \sto -> do
    let ref = Reference GLODOT name NullRef
    o <- liftIO (readIORef sto) >>= objectReferenceUpdate upd (Just ref) NullRef . Just
    liftIO $ writeIORef sto $ maybe ONull id o
    return o

newtype ConstantStore = ConstantStore T_dict

instance Store ConstantStore where
  storeLookup (ConstantStore sto) nm = return $ M.lookup nm sto
  storeUpdate _ nm _ = execThrow $ obj [obj "cannot update constant value:", obj nm]


_appRef :: Maybe Reference -> RefSuffix -> Maybe Reference
_appRef back ref = return refAppendSuffix <*> back <*> pure ref

_objMaybeRef :: Maybe Reference -> [Object]
_objMaybeRef = maybe [] (return . obj)

-- | Read elements within a Dao 'Object' using a 'RefSuffix'. This works on 'ODict' and 'OTree'
-- constructed types and any 'OHaskell' data which provided a function for 'defToStruct' in it's
-- instantiation of 'HaskellDataClass'. Supply an optional 'Reference' that indicates where the
-- 'Object' was retrieved from, this 'Reference' can be used to construct more helpful error
-- messages.
objectReferenceAccess :: Maybe Reference -> RefSuffix -> Object -> Exec (Maybe Object)
objectReferenceAccess = loop where
  loop back suf o = flip mplus (cantAccess back) $ case suf of
    NullRef -> return $ Just o
    DotRef name suf -> return (_appRef back $ dotRef name) >>= \back -> case o of
      ODict o -> Just <$> _dictReferenceAccess   back name suf o
      OTree o -> Just <$> _structReferenceAccess back name suf o
      OHaskell (HaskellData o ifc) -> case objToStruct ifc of
        Nothing     -> opaqueObj back
        Just access -> access o >>= fmap Just . _structReferenceAccess back name suf
      _                            -> mzero
    Subscript idx suf -> objectIndexAccess o idx >>= loop (_appRef back $ subscript idx) suf
    FuncCall args suf -> do
      result <- callObject back o args
      case suf of
        NullRef -> return result
        suf     -> maybe (funcEvaldNull back) (loop (_appRef back $ funcCall args) suf) result
  opaqueObj     qref = execThrow $ obj $ [obj "cannot inspect opaque value"] ++ _objMaybeRef qref
  cantAccess    qref = execThrow $ obj $ [obj "undefined reference"] ++ _objMaybeRef qref
  funcEvaldNull qref = execThrow $ obj $
    [obj "function call evaluation returned void"] ++ _objMaybeRef qref

_dictReferenceAccess :: Maybe Reference -> Name -> RefSuffix -> T_dict -> Exec Object
_dictReferenceAccess back name suf o =
  maybe mzero (objectReferenceAccess back suf) (M.lookup name o) >>= maybe mzero return

_structReferenceAccess :: Maybe Reference -> Name -> RefSuffix -> Struct -> Exec Object
_structReferenceAccess back name suf o = case o of
  Struct{ fieldMap=fm } -> _dictReferenceAccess back name suf fm
  Nullary{}             -> mzero

-- | Update elements within a Dao 'Object' using a 'RefSuffix'. This works on 'ODict' and 'OTree'
-- constructed types and any 'OHaskell' data which provided functions for both 'defToStruct' and
-- 'defFromStruct' in it's instantiation of 'HaskellDataClass'.
objectReferenceUpdate
  :: (Maybe Object -> Exec (Maybe Object))
  -> Maybe Reference -> RefSuffix -> Maybe Object -> Exec (Maybe Object)
objectReferenceUpdate upd = loop where
  loop back suf o = case suf of
    NullRef               -> upd o
    DotRef         nm suf -> return (_appRef back $ dotRef nm) >>= \back -> case o of
      Nothing -> execThrow $ obj $ [obj "undefined reference"] ++ _objMaybeRef back
      Just  o -> case o of
        OTree   o -> _structReferenceUpdate upd back nm suf o >>= putBack OTree
        ODict   o -> _dictReferenceUpdate   upd back nm suf o >>= putBack ODict
        OHaskell (HaskellData o ifc) -> case return (,) <*> objToStruct ifc <*> objFromStruct ifc of
          Just (toStruct, fromStruct) -> toStruct o >>= _structReferenceUpdate upd back nm suf >>=
            maybe (return Nothing) (fmap (Just . OHaskell . flip HaskellData ifc) . fromStruct)
          Nothing -> execThrow $ obj $
            [ obj "cannot update opaque data type"
            , obj $ Reference UNQUAL nm suf
            ]
        o -> execThrow $ obj $
          [ obj "cannot update atomic data type", obj o
          , obj "at reference", obj $ Reference UNQUAL nm suf
          ]
    Subscript idx suf -> case o of
      Nothing -> execThrow $ obj $ concat $
        [[obj "indexed undefined reference"], maybe [] (return . obj) back]
      Just  o ->
        let step = (objectReferenceUpdate upd (_appRef back $ subscript idx) suf)
        in  objectIndexUpdate step o idx
    FuncCall args suf -> do
      back <- return (_appRef back $ funcCall args)
      case o of
        Nothing -> execThrow $ obj $ [obj "called undefined function"] ++ _objMaybeRef back
        Just  o -> callObject back o args >>=
          objectReferenceUpdate upd (_appRef back $ funcCall args) suf
  putBack constr = maybe (return Nothing) (return . Just . constr)

_structReferenceUpdate
  :: (Maybe Object -> Exec (Maybe Object))
  -> Maybe Reference -> Name -> RefSuffix -> Struct -> Exec (Maybe Struct)
_structReferenceUpdate upd back suf name o = case o of
  Nullary{ structName=name } -> execThrow $ obj $ concat $
    [ [obj "on structure", obj name]
    , maybe [] (\back -> [obj "no element named", obj back]) back
    ]
  Struct{ fieldMap=inner } -> _dictReferenceUpdate upd back suf name inner >>=
    maybe (return Nothing) (\inner -> return $ Just $ o{ fieldMap=inner })

_dictReferenceUpdate
  :: (Maybe Object -> Exec (Maybe Object))
  -> Maybe Reference -> Name -> RefSuffix -> T_dict -> Exec (Maybe T_dict)
_dictReferenceUpdate upd back name suf o = do
  o <- (\item -> M.alter (const item) name o) <$>
    objectReferenceUpdate upd back suf (M.lookup name o)
  return (if M.null o then Nothing else Just o)

_check1D :: [Object] -> String -> (Object -> Exec a) -> Exec a
_check1D idx typ f = case idx of
  [i] -> derefObject i >>= f
  _   -> execThrow $ obj $
    [ obj (typ++" object is 1-dimensional, but is subscripted with")
    , obj (length idx), obj "dimensional index", obj idx
    ]

_badRefIndex :: Reference -> Exec ig
_badRefIndex i = execThrow $ obj $
  [obj "qualified reference is undefined when used as subscript", ORef i]

-- | Read elements within a Dao 'Object' using an index value. This is the function used to
-- implement Dao language expressions of the form:
-- > ... = item[i1, i2, ..., iN];
-- This works on 'OList' data with 'OInt' indecies, 'ODict' and 'OTree' data with 'ORef' indecies,
-- and any 'OHaskell' data which provided a function for 'defIndexer' in it's instantiation of
-- 'HaskellDataClass'.
objectIndexAccess :: Object -> [Object] -> Exec Object
objectIndexAccess o idx = case o of
  OList []  -> execThrow $ obj $ [obj "indexing empty list with", obj idx]
  OList lst -> _check1D idx "list" $ \i -> do
    idx <- derefObject i
    idx <- execute (asInteger idx) <|>
      execThrow (obj $ [obj "must index list with integer, instead used", i])
    if idx<0
      then  execThrow $ obj [obj "list index value is negative:", i, obj "indexing:", o]
      else  case dropWhile ((<idx) . fst) (zip [0..] lst) of
              []       -> execThrow $ obj [obj "index out of bounds:", i, obj "indexing:", o]
              (_, o):_ -> return o
  ODict dict -> lookupDict (obj "dictionary") dict
  OTree (Struct{ structName=n, fieldMap=dict }) -> lookupDict (obj n) dict
  OHaskell (HaskellData d ifc) -> case objIndexer ifc of
    Nothing    -> _check1D idx "data structure" $ \i -> do
      i <- execute (asReference i) <|> errmsg i
      case i of
        Reference UNQUAL name suf ->
          objectReferenceAccess (Just $ RefObject o $ subscript idx) (DotRef name suf) o >>=
            maybe (fail "reference evaluated to void") return
        _ -> _badRefIndex i
    Just index -> index d idx
  _         -> errmsg $ obj idx
  where
    errmsg i = execThrow $ obj [obj "incorrect type used as index", i]
    lookupDict typ dict = _check1D idx "dict" $ \i -> do
      i <- execute (asReference i) <|>
        execThrow (obj [obj "cannot index", typ, obj "with value", i])
      case i of
        Reference UNQUAL name suf -> _dictReferenceAccess (Just i) name suf dict
        _ -> _badRefIndex i

-- | Read elements within a Dao 'Object' using an index value. This is the function used to
-- implement Dao language expressions of the form:
-- > item[i1, i2, ..., iN] += ... ;
-- This works on 'OList' data with 'OInt' indecies, 'ODict' and 'OTree' data with 'ORef' indecies,
-- and any 'OHaskell' data which provided a function for both 'defIndexer' and 'defIndexUpdater' in
-- it's instantiation of 'HaskellDataClass'.
objectIndexUpdate
  :: (Maybe Object -> Exec (Maybe Object))
  -> Object -> [Object] -> Exec (Maybe Object)
objectIndexUpdate upd o idx = case o of 
  OList ox -> _check1D idx "list" $ \i -> do
    idx <- execute (fromIntegral <$> asInteger i) <|>
      execThrow (obj [obj "cannot index object", o, obj "with value", i])
    if idx == negate 1
    then upd Nothing >>= return . Just . OList . (++ox) . maybe [] return
    else do
      let (lo, hi) = splitAt idx ox
      if length lo == idx
      then
        if null hi
        then upd Nothing >>= return . Just . OList . (ox++) . maybe [] return
        else upd (Just $ head hi) >>= return . Just . OList . (lo++) . (++(tail hi)) . maybe [] return
      else execThrow $ obj $ [obj "index out of bounds", i]
  ODict dict -> _check1D idx "dictionary" $ updRef $ \name suf ->
    fmap ODict <$> _dictReferenceUpdate upd (Just $ refObject o) name suf dict
  OTree struct -> _check1D idx (uchars $ structName struct) $ updRef $ \name suf ->
    fmap OTree <$> _structReferenceUpdate upd (Just $ refObject o) name suf struct
  OHaskell (HaskellData d ifc) -> case objIndexUpdater ifc of
    Nothing -> _check1D idx "structure" $ updRef $ \name suf ->
      objectReferenceUpdate upd (Just $ refObject o) (DotRef name suf) (Just o)
    Just index -> fmap (OHaskell . flip HaskellData ifc) <$> index d upd idx
  _ -> execThrow $ obj $ [obj "cannot update object type", obj (typeOfObj o), obj "at index", obj idx]
  where
    updRef upd i = do
      i <- execute (asReference i) <|> execThrow (obj [obj "cannot access index", obj idx])
      case i of
        Reference UNQUAL name suf -> upd name suf
        i -> _badRefIndex i

----------------------------------------------------------------------------------------------------

_withGlobalKey :: Object -> (H.Index Object -> RefMonad Object Dynamic a) -> Exec a
_withGlobalKey idx f = asks globalMethodTable >>= \mt -> 
  asks runtimeRefTable >>= liftIO . runReaderT (f $ H.hashNewIndex (H.deriveHash128_DaoBinary mt) idx)

-- | Some objects may refer to an object that serves as a unique identifier created by the system,
-- for example objects refereing to file handles. These unique identifying objects should always be
-- stored in this table. The Dao 'Object' wrapper should be used as the index to retrieve the Object
-- in the table. This function takes the object to be stored, a destructor function to be called on
-- releasing the object, and an indexing object used to identify the stored object in the table.
initializeGlobalKey :: Typeable o => o -> (o -> IO ()) -> Object -> Exec (H.Index Object)
initializeGlobalKey o destructor idx = _withGlobalKey idx $ \key ->
  initializeWithKey (toDyn o) (destructor o) key >> return key

-- | Destroy an object that was stored into the global key table using 'initializeGlobalKey'. The
-- destructor function passed to the 'initializeGlobalKey' will be evaluated, and the object will
-- be removed from the table. This function takes an indexing object used to select the stored
-- object from the table.
destroyGlobalKey :: Object -> Exec ()
destroyGlobalKey = flip _withGlobalKey destroyWithKey

----------------------------------------------------------------------------------------------------

-- | This data type is use to halt normal evaluation and force the result of evaluating the code to
-- be a particular value of this type. The 'Exec' monad instantiates
-- 'Control.Monad.Error.Class.MonadError' such that 'Control.Monad.Error.Class.throwError' throws a
-- value of this type. However, it is not only used for exceptions. The Dao scripting language's
-- "return" statement throws an 'ExecReturn' value which is caught using
-- 'Control.Monad.Error.Class.catchError' when evaluating function calls.
data ExecControl
  = ExecReturn { execReturnValue :: Maybe Object }
  | ExecError
    { execReturnValue :: Maybe Object
    , execErrorInfo   :: ExecErrorInfo
    }
  deriving Typeable

data ExecErrorInfo
  = ExecErrorInfo
    { execUnitAtError :: Maybe ExecUnit
    , execErrExpr     :: Maybe ObjectExpr
    , execErrScript   :: Maybe ScriptExpr
    , execErrTopLevel :: Maybe TopLevelExpr
    }
  deriving Typeable

mkExecErrorInfo :: ExecErrorInfo
mkExecErrorInfo = ExecErrorInfo Nothing Nothing Nothing Nothing

mkExecError :: ExecControl
mkExecError = ExecError Nothing mkExecErrorInfo

-- | Evaluate an 'Exec', but if it throws an exception, set record an 'ObjectExpr' where
-- the exception occurred in the exception information.
updateExecErrorInfo :: (ExecErrorInfo -> ExecErrorInfo) -> Exec a -> Exec a
updateExecErrorInfo upd fn = catchError fn $ \err -> case err of
  ExecReturn{} -> throwError err
  ExecError{ execErrorInfo=info } -> throwError $ err{ execErrorInfo = upd info }

instance HasNullValue ExecControl where
  nullValue = ExecReturn Nothing
  testNull (ExecReturn Nothing) = True
  testNull  _                   = False

instance PPrintable ExecControl where
  pPrint err = case err of 
    ExecError{ execReturnValue=o, execErrorInfo=info } -> maybe (return ()) pperr o where
      fileName = execUnitAtError info >>= programModuleName
      apLabel which label =
        fmap (\o -> (pInline [pString label, pString " ", pPrint o], getLocation o)) (which info)
      errInfo = msum
        [ apLabel execErrExpr     "in expression" 
        , apLabel execErrScript   "in statement"
        , apLabel execErrTopLevel "in top-level directive"
        ]
      unquot o = case o of
        OString o -> pUStr o
        o         -> pPrint o
      pplist o = case o of
        OList   o -> pInline $ intersperse (pString " ") (map unquot o)
        o         -> unquot o
      ppmap o = case o of
        ODict o -> forM_ (M.assocs o) $ \ (ref, o) -> pWrapIndent [pPrint ref, pplist o]
        o -> pplist o
      pperr o = do
        pWrapIndent $
          [ case errInfo of
              Nothing -> pString "Error: "
              Just  o -> pString $ concat $
                [maybe "" ((++":") . uchars) fileName , show (snd o) ++ ": "]
          , ppmap o
          ]
        pEndLine
        maybe (return ()) fst errInfo
    ExecReturn{ execReturnValue=o } ->
      maybe (return ()) (\o -> pWrapIndent [pString "Evaluated to: ", pPrint o]) o

instance ToDaoStructClass ExecControl where
  toDaoStruct = ask >>= \o -> case o of
    ExecReturn a -> flip (maybe (void $ makeNullary "ExecReturn")) a $ \a ->
      renameConstructor "ExecReturn" >> void ("value" .= a)
    ExecError o (ExecErrorInfo _ a b c) -> void $ renameConstructor "ExecError" >>
      "value" .=? o >> "objectExpr" .=? a >> "scriptExpr" .=? b >> "topLevelExpr" .=? c

instance FromDaoStructClass ExecControl where
  fromDaoStruct = msum $
    [ constructor "ExecReturn" >> ExecReturn <$> opt "value"
    , do  constructor "ExecError"
          return ExecError <*> opt "value" <*>
            (return ExecErrorInfo
              <*> pure Nothing
              <*> opt "objectExpr"
              <*> opt "scriptExpr"
              <*> opt "topLevelExpr"
            )
    ]

instance HaskellDataClass ExecControl where
  haskellDataInterface = interface nullValue $ do
    autoDefPPrinter >> autoDefNullTest >> autoDefToStruct

setCtrlReturnValue :: Object -> ExecControl -> ExecControl
setCtrlReturnValue obj ctrl = case ctrl of
  ExecReturn{}   -> ExecReturn (Just obj)
  ExecError{ execErrorInfo=info } -> ExecError{ execReturnValue=Just obj, execErrorInfo=info }

_setErrorInfoExpr :: (ExecErrorInfo -> ExecErrorInfo) -> Exec a -> Exec a
_setErrorInfoExpr upd exec = catchPredicate exec >>= \a -> case a of
  OK      a -> return a
  Backtrack -> mzero
  PFail err -> case err of
    ExecReturn{} -> throwError err
    ExecError{ execErrorInfo=info } -> throwError $ err{ execErrorInfo=upd info }

_setObjectExprError :: ObjectExpr -> Exec a -> Exec a
_setObjectExprError o = _setErrorInfoExpr (\info -> info{ execErrExpr=Just o })

_setScriptExprError :: ScriptExpr -> Exec a -> Exec a
_setScriptExprError o = _setErrorInfoExpr (\info -> info{ execErrScript=Just o })

_setTopLevelExprError :: TopLevelExpr -> Exec (ExecUnit -> ExecUnit) -> Exec (ExecUnit ->ExecUnit)
_setTopLevelExprError o = _setErrorInfoExpr (\info -> info{ execErrTopLevel=Just o })

-- | If an error has not been caught, log it in the module where it can be retrieved later. This
-- function only stores errors constructed with 'ExecError', the 'ExecReturn' constructed objects
-- are ignored.
logUncaughtErrors :: [ExecControl] -> Exec ()
logUncaughtErrors errs = asks uncaughtErrors >>= \ioref -> liftIO $ modifyIORef ioref $
  (++(errs >>= \e -> case e of { ExecReturn{} -> []; ExecError{} -> [e]; }))

-- | Retrieve all the logged uncaught 'ExecError' values stored by 'logUncaughtErrors'.
getUncaughtErrorLog :: Exec [ExecControl]
getUncaughtErrorLog = asks uncaughtErrors >>= liftIO . readIORef 

-- | Clear the log of uncaught 'ExecError' values stored by 'logUncaughtErrors'.
clearUncaughtErrorLog :: Exec ()
clearUncaughtErrorLog = asks uncaughtErrors >>= \ioref -> liftIO $ modifyIORef ioref $ const []

----------------------------------------------------------------------------------------------------

-- | All evaluation of the Dao language takes place in the 'Exec' monad. It instantiates
-- 'Control.Monad.MonadIO.MonadIO' to allow @IO@ functions to be lifeted into it. It instantiates
-- 'Control.Monad.Error.MonadError' and provides it's own exception handling mechanism completely
-- different from the Haskell runtime, so as to allow for more control over exception handling in
-- the Dao runtime.
newtype Exec a  = Exec{ execToPredicate :: PredicateT ExecControl (ReaderT ExecUnit IO) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadReader ExecUnit Exec where
  local upd (Exec (PredicateT fn)) = Exec $ PredicateT (local upd fn)
  ask                              = Exec $ lift ask

instance MonadError ExecControl Exec where
  throwError = Exec . throwError
  catchError (Exec try) catch = Exec (catchError try (execToPredicate . catch))

instance MonadPlusError ExecControl Exec where
  catchPredicate (Exec f) = Exec (catchPredicate f)
  predicate = Exec . predicate

----------------------------------------------------------------------------------------------------

-- | The 'XPure' type is like 'Exec' but does not lift IO or contain any reference to any
-- 'ExecUnit', so it is guaranteed to work without side-effects, but it also instantiates the
-- 'Control.Monad.MonadPlus', 'Control.Applicative.Alternative', 'Control.Monad.Error.MonadError'
-- and 'Dao.Predicate.MonadPlusError' classes so you can do computation with backtracking and
-- exceptions. Although this monad evaluates to a pure function, it does have stateful data: a
-- 'Dao.String.UStr' that will call a "print stream", which is provided for general purpose; a place
-- to print information throughout evaluation like a "print()" statement.  The 'xnote' function
-- serves as the "print()" function for this monad. Use the ordinary 'Control.Monad.State.get' and
-- 'Control.Monad.State.modify' APIs for working with the print stream data.
-- 
-- You can also evaluate an 'XPure' monad within an 'Exec' monad by simply using the 'execute'
-- function. This will automatically convert the internal 'Dao.Predicate.Predicate' of the 'XPure'
-- monad to the 'Dao.Predicate.Predicate' of the 'Exec' monad, meaning if you 'execute' an 'XPure'
-- monad that backtracks or throws an error, the 'Exec' monad will backtrack or throw the same
-- error.
newtype XPure a = XPure { xpureToState :: PredicateT Object (State UStr) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

-- | Convert a value wrapped in an XPure monad to a pair containing the internal state 'Dao.String.UStr' and
-- 'Dao.Predicate.Predicate' value.
runXPure :: XPure a -> (Predicate Object a, UStr)
runXPure = flip runState nil . runPredicateT . xpureToState

-- | Like 'Control.Monad.State.evalState', but works on the 'XPure' monad, i.e. it is defined as
-- > 'Prelude.fst' . 'runXPure'
evalXPure :: XPure a -> Predicate Object a
evalXPure = fst . runXPure

-- | Like 'evalXPure' but evaluates to 'Prelude.Maybe' instead of a 'Dao.Predicate.Predicate'.
-- 'Dao.Predicate.Backtrack' and 'Dao.Predicate.PFail' both map to 'Prelude.Nothing',
-- 'Dao.Predicate.OK' maps to 'Prelude.Just'.
extractXPure :: XPure a -> Maybe a
extractXPure = okToJust . evalXPure

instance MonadError Object XPure where
  throwError = XPure . throwError
  catchError (XPure f) catch = XPure $ catchError f (xpureToState . catch)

instance MonadPlusError Object XPure where
  predicate = XPure . predicate
  catchPredicate (XPure f) = XPure $ catchPredicate f

instance MonadState UStr XPure where { state = XPure . lift . state }

instance Executable (XPure a) a where
  execute (XPure f) = predicate $ fmapPFail (\err -> mkExecError{ execReturnValue=Just err }) $
    evalState (runPredicateT f) mempty

-- | Like 'Control.Applicative.pure' or 'Control.Monad.return' but the type is not polymorphic so
-- there is no need to annotate the monad to which you are 'Control.Monad.return'ing, which is
-- helpful when using functions like 'exceute' to convert the 'XPure' monad to the 'Exec' monad.
xpure :: a -> XPure a
xpure = pure

-- | Like 'xpure' but wraps any data type that instantiates the 'ObjectClass' class.
xobj :: ObjectClass a => a -> XPure Object
xobj = xpure . obj

-- | Append a string of any 'UStrType' to the general-purpose print stream contained within the
-- 'XPure' monad.
xnote :: UStrType s => s -> XPure ()
xnote = modify . flip mappend . toUStr

-- | Like 'xnote' but lets you operate on the 'Data.ByteString.Lazy.UTF8.ByteString'.
xonUTF8 :: (U.ByteString -> U.ByteString) -> XPure ()
xonUTF8 = modify . fmapUTF8String

-- | Works on any 'Control.Monad.MonadPlus' type, including 'Prelude.Maybe', 'Exec' and 'XPure', is
-- defined as: > 'Prelude.maybe' 'Control.Monad.mzero' 'Control.Monad.return' which is useful
-- shorthand for converting a value wrapped in a 'Prelude.Maybe' data type to a value wrapped in the
-- 'Control.Monad.MonadPlus' type.
xmaybe :: MonadPlus m => Maybe a -> m a
xmaybe = maybe mzero return

----------------------------------------------------------------------------------------------------

class ExecThrowable o where
  toExecError :: o -> ExecControl
  -- | Like 'Prelude.error' but works for the 'Exec' monad, throws an 'ExecControl' using
  -- 'Control.Monad.Error.throwError' constructed using the given 'Object' value as the
  -- 'execReturnValue'.
  execThrow :: ExecThrowable o => o -> Exec ig
  execThrow o = ask >>= \xunit -> throwError $
    let err = toExecError o
    in  err{execErrorInfo=(execErrorInfo err){execUnitAtError=Just xunit}}

instance ExecThrowable Object where
  toExecError err = setCtrlReturnValue err mkExecError

instance ExecThrowable ExecControl where { toExecError = id }

ioExec :: Exec a -> ExecUnit -> IO (Predicate ExecControl a)
ioExec func xunit = runReaderT (runPredicateT (execToPredicate func)) xunit

----------------------------------------------------------------------------------------------------

-- | This is the data type analogous to the 'Exec' monad what 'Control.Exception.Handler' is to the
-- @IO@ monad.
newtype ExecHandler a = ExecHandler { execHandler :: ExecUnit -> Handler (Predicate ExecControl a) }

instance Functor ExecHandler where { fmap f (ExecHandler h) = ExecHandler (fmap (fmap (fmap f)) h) }

-- | Create an 'ExecHandler'.
newExecIOHandler :: Exception e => (e -> Exec a) -> ExecHandler a
newExecIOHandler h = ExecHandler (\xunit -> Handler (\e -> ioExec (h e) xunit))

-- | Using an 'ExecHandler' like 'execIOHandler', catch any exceptions thrown by the Haskell
-- language runtime and wrap them up in the 'Exec' monad.
execCatchIO :: Exec a -> [ExecHandler a] -> Exec a
execCatchIO tryFunc handlers = do
  xunit <- ask
  ctrl  <- liftIO $ catches (ioExec tryFunc xunit) (fmap (\h -> execHandler h xunit) handlers)
  predicate ctrl

-- | Like 'execCatchIO' but with the arguments 'Prelude.flip'ped.
execHandleIO :: [ExecHandler a] -> Exec a -> Exec a
execHandleIO = flip execCatchIO

-- | An 'ExecHandler' for catching 'Control.Exception.ErrorCall's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execIOHandler :: ExecHandler ()
execIOHandler = newExecIOHandler $ \e -> execThrow (obj $ show (e::IOException))

-- | An 'ExecHandler' for catching 'Control.Exception.ErrorCall's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execErrorHandler :: ExecHandler ()
execErrorHandler = newExecIOHandler $ \e -> execThrow (obj $ show (e::ErrorCall))

-- | This will catch an 'ExecControl' thrown by 'Control.Monad.Error.throwError', but re-throw
-- 'ExecError's.
catchReturn :: (Maybe Object -> Exec a) -> Exec a -> Exec a
catchReturn catch f = catchPredicate f >>= \pval -> case pval of
  PFail (ExecReturn a) -> catch a
  pval                 -> predicate pval

----------------------------------------------------------------------------------------------------
-- $StackOperations
-- Operating on the local stack.

-- | Push a new empty local-variable context onto the stack. Does NOT 'catchReturnObj', so it can be
-- used to push a new context for every level of nested if/else/for/try/catch statement, or to
-- evaluate a macro, but not a function call. Use 'execFuncPushStack' to perform a function call within
-- a function call.
execNested :: T_dict -> Exec a -> Exec a
execNested init exe = do
  (LocalStore stack) <- asks execStack
  liftIO $ modifyIORef stack (stackPush init)
  result <- exe
  liftIO $ modifyIORef stack (fst . stackPop)
  return result

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. This function is different from 'nestedExecStak' in that it acually removes the current
-- execution stack so a function call cannot modify the local variables of the function which called
-- it. Furthermore it catches evaluation of a "return" statement allowing the function which called
-- it to procede with execution after this call has returned.
execFuncPushStack :: T_dict -> Exec (Maybe Object) -> Exec (Maybe Object)
execFuncPushStack dict exe = do
  pval <- catchPredicate $ do
    stack <- liftIO (newIORef emptyStack)
    local (\xunit -> xunit{execStack=LocalStore stack}) (execNested dict exe)
  case pval of
    OK                obj  -> return obj
    Backtrack              -> mzero
    PFail (ExecReturn obj) -> return obj
    PFail             err  -> throwError err

----------------------------------------------------------------------------------------------------

-- | Elements of the symantic data structures that instantiate 'Executable' and do not instantiate
-- 'Dao.PPrint.PPrintable', 'Dao.Struct.Structured', or any parsers. Elements of the abstract syntax
-- tree (AST) instantiate 'Dao.PPrint.PPrintable', 'Dao.Struct.Structured', and all of the parsers,
-- but are not executable and do not instantiate 'Executable'. This separates concerns pretty well,
-- but leaves us with the problem of having to convert back and forth between these various data
-- types.
--
-- The 'Intermediate' class allows us to declare a one-to-one relationship between AST types and
-- executable types. For example, 'ObjectExpr' is the intermediate representation of
-- 'AST_Object', so our instance for this relationship is @instane 'Intermediate'
-- 'ObjectExpr' 'AST_Object'@.
class Intermediate obj ast | obj -> ast, ast -> obj where
  toInterm   :: ast -> [obj]
  fromInterm :: obj -> [ast]

-- | This class is used to classify data types that can be generated at random by
-- 'Dao.Random.HasRandGen' and parsed from source code, which might generate data types with
-- identical functionality, identical pretty-printed forms, identical 'Executable' semantics but may
-- have differences in the recursive structure that the 'Prelude.Eq' class would compute as not
-- identical. The job of the 'canonicalize' function is to eliminate these discrepancies by reducing
-- the data structure to a canonical form.
class Canonical a where { canonicalize :: a -> a }

instance Intermediate Name Name where { toInterm = return; fromInterm = return; }

-- Not for export: here are a bunch of shortcuts to converting the AST to the intermediate data
-- type. Sinec 'toInterm' returns a single item in a list to indicate success and an empty list to
-- indicate failure, all of these items have their evaluated type wrapped in a list type. This is to
-- allow the 'toInterm' instances use the 'Control.Monad.liftM' family of functions.
ti :: Intermediate obj ast => ast -> [obj]
ti = toInterm
uc :: Com a -> [a]
uc = return . unComment
uc0 :: Intermediate obj ast =>  Com ast  -> [obj]
uc0 = toInterm . unComment
um0 :: Maybe (Com a) -> [Maybe a]
um0 = maybe [Nothing] (return . Just . unComment)
um1 :: Intermediate obj ast => Maybe ast -> [Maybe obj]
um1 = maybe [Nothing] (fmap Just . toInterm)

fi :: Intermediate obj ast => obj -> [ast]
fi = fromInterm
nc :: a -> [Com a]
nc = return . Com
nc0 :: Intermediate obj ast => obj -> [Com ast]
nc0 = fmap Com . fromInterm
nm0 :: Maybe a -> [Maybe (Com a)]
nm0 = maybe [Nothing] (return . Just . Com)
nm1 :: Intermediate obj ast => Maybe obj -> [Maybe ast]
nm1 = maybe [Nothing] (fmap Just . fromInterm)

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is pretty-printable ('Dao.PPrint.PPrintable').
pPrintInterm :: (Intermediate o ast, PPrintable ast) => o -> PPrint
pPrintInterm = mapM_ pPrint . fromInterm

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is 'Dao.Binary.GPut'.
putAST :: (Intermediate obj ast, B.Binary obj mtab) => ast -> B.GPut mtab
putAST ast = case toInterm ast of
    [obj] -> B.put obj
    _     -> fail "binary encoder could not convert AST to intermediate expression"

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is 'Dao.Binary.GGet'.
getAST :: (Intermediate obj ast, B.Binary obj mtab) => B.GGet mtab ast
getAST = B.get >>= \obj -> case fromInterm obj of
    [ast] -> return ast
    _     -> fail "binary decoder constructed object that could not be converted to an AST representation"

----------------------------------------------------------------------------------------------------

-- | Comments in the Dao language are not interpreted, but they are not disgarded either. Dao is
-- intended to manipulate natural language, and itself, so that it can "learn" new semantic
-- structures. Dao scripts can manipulate the syntax tree of other Dao scripts, and so it might be
-- helpful if the syntax tree included comments.
data Comment
  = InlineComment  UStr
  | EndlineComment UStr
  deriving (Eq, Ord, Typeable, Show)

commentString :: Comment -> UStr
commentString com = case com of
  InlineComment  a -> a
  EndlineComment a -> a

instance NFData Comment where
  rnf (InlineComment  a) = seq a ()
  rnf (EndlineComment a) = seq a ()

instance HasNullValue Comment where
  nullValue = EndlineComment nil
  testNull (EndlineComment c) = c==nil
  testNull (InlineComment  c) = c==nil

instance PPrintable Comment where
  pPrint com = do
    case com of
      EndlineComment c -> pString ("//"++uchars c) >> pForceNewLine
      InlineComment  c -> pGroup True $ pInline $
        concat [[pString " /*"], map pString (lines (uchars c)), [pString "*/ "]]

instance PrecedeWithSpace a => PrecedeWithSpace (Com a) where
  precedeWithSpace o = case o of
    Com         b   -> precedeWithSpace b
    ComBefore a b   -> precedeWithSpace a || precedeWithSpace b
    ComAfter    b _ -> precedeWithSpace b
    ComAround a b _ -> precedeWithSpace a || precedeWithSpace b
    -- there should always be a space before a comment.

instance ToDaoStructClass Comment where
  toDaoStruct = let nm = renameConstructor in ask >>= \co -> void $ case co of
    InlineComment  o -> nm "InlineComment"  >> "comment" .= o
    EndlineComment o -> nm "EndlineComment" >> "comment" .= o

instance FromDaoStructClass Comment where
  fromDaoStruct = msum $
    [ constructor "InlineComment"  >> InlineComment  <$> req "comment"
    , constructor "EndlineComment" >> EndlineComment <$> req "comment"
    ]

instance ObjectClass Comment where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass Comment where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

instance PPrintable [Comment] where { pPrint = mapM_ pPrint }

instance PrecedeWithSpace [Comment] where { precedeWithSpace = not . null }

instance HasRandGen [Comment] where { randO = return []; defaultO = return []; }
--  randO = do
--    i0 <- randInt
--    let (i1, many) = divMod i0 4
--        (i2, typn) = divMod i1 16
--        typx = take many (randToBase 2 typn ++ replicate 4 0)
--        lenx = map (+1) (randToBase 29 i2)
--        com typ = if typ==0 then EndlineComment else InlineComment
--    forM (zip typx lenx) $ \ (typ, len) ->
--      fmap (com typ . ustr . unwords . map (B.unpack . getRandomWord)) (replicateM len randInt)

instance ObjectClass [Comment] where { obj=listToObj; fromObj=listFromObj; }

----------------------------------------------------------------------------------------------------

-- | Symbols in the Dao syntax tree that can actually be manipulated can be surrounded by comments.
-- The 'Com' structure represents a space-efficient means to surround each syntactic element with
-- comments that can be ignored without disgarding them.
data Com a = Com a | ComBefore [Comment] a | ComAfter a [Comment] | ComAround [Comment] a [Comment]
  deriving (Eq, Ord, Typeable, Show)

instance Functor Com where
  fmap fn c = case c of
    Com          a    -> Com          (fn a)
    ComBefore c1 a    -> ComBefore c1 (fn a)
    ComAfter     a c2 -> ComAfter     (fn a) c2
    ComAround c1 a c2 -> ComAround c1 (fn a) c2

instance NFData a => NFData (Com a) where
  rnf (Com         a  ) = deepseq a ()
  rnf (ComBefore a b  ) = deepseq a $! deepseq b ()
  rnf (ComAfter    a b) = deepseq a $! deepseq b ()
  rnf (ComAround a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue a => HasNullValue (Com a) where
  nullValue = Com nullValue
  testNull (Com a) = testNull a
  testNull _ = False

instance HasLocation a => HasLocation (Com a) where
  getLocation = getLocation . unComment
  setLocation com loc = fmap (\a -> setLocation a loc) com
  delLocation = fmap delLocation

instance PPrintable a => PPrintable (Com a) where { pPrint = pPrintComWith pPrint }

comToDaoStruct :: (ToDaoStruct a ()) -> ToDaoStruct (Com a) ()
comToDaoStruct toDaoStruct = do
  co <- ask
  let put = innerToStructWith toDaoStruct
  case co of
    Com          a    ->                   put a >> return ()
    ComBefore c1 a    -> "before" .= c1 >> put a >> return ()
    ComAfter     a c2 ->                   put a >> "after" .= c2 >> return ()
    ComAround c1 a c2 -> "before" .= c1 >> put a >> "after" .= c2 >> return ()

comFromDaoStruct :: FromDaoStruct a -> FromDaoStruct (Com a)
comFromDaoStruct fromStruct = do
  let f name = tryField name (maybe (fail name) return . fromObj)
  before <- optional $ f "before"
  after  <- optional $ f "after"
  a      <- fromStruct 
  return $ maybe (Com a) id $ msum $
    [ return ComAround <*> before <*> pure a <*> after
    , return ComBefore <*> before <*> pure a
    , return ComAfter  <*> pure a <*> after
    ]

instance (Typeable a, ObjectClass a, ToDaoStructClass a) => ToDaoStructClass (Com a) where
  toDaoStruct = comToDaoStruct toDaoStruct

instance (Typeable a, ObjectClass a, ToDaoStructClass a, FromDaoStructClass a) => FromDaoStructClass (Com a) where
  fromDaoStruct = comFromDaoStruct fromDaoStruct

instance HasRandGen a => HasRandGen (Com a) where
  randO    = Com <$> randO
  defaultO = Com <$> defaultO

instance (Typeable a, ObjectClass a) =>
  ObjectClass (Com a) where { obj=new; fromObj=objFromHaskellData; }
instance Typeable a => HaskellDataClass (Com a) where
  haskellDataInterface = interface (Com $ error "undefined Com") $ do
    return ()
    -- autoDefToStruct >> autoDefFromStruct

instance (Typeable a, ObjectClass a) =>
  ObjectClass [Com a] where { obj=listToObj; fromObj=listFromObj; }

pPrintComWith :: (a -> PPrint) -> Com a -> PPrint
pPrintComWith prin com = case com of
  Com          c    -> prin c
  ComBefore ax c    -> pcom ax >> prin c
  ComAfter     c bx -> prin c >> pcom bx
  ComAround ax c bx -> pcom ax >> prin c >> pcom bx
  where { pcom = pInline . map pPrint }

pListOfComsWith :: (a -> PPrint) -> [Com a] -> PPrint
pListOfComsWith prin = sequence_ . map (pPrintComWith prin)

pListOfComs :: PPrintable a => [Com a] -> PPrint
pListOfComs = pListOfComsWith pPrint

randComWith :: RandO a -> RandO (Com a)
randComWith rand = fmap Com rand
--  randComWith :: RandO a -> RandO (Com a)
--  randComWith rand = do
--    typ <- fmap (flip mod 24 . unsign) randInt
--    a <- rand
--    case typ of
--      0 -> do
--        before <- randO
--        after  <- randO
--        return (ComAround before a after)
--      1 -> do
--        before <- randO
--        return (ComBefore before a)
--      2 -> do
--        after <- randO
--        return (ComAfter a after)
--      _ -> return (Com a)

-- not for export
no :: RandO Location
no = return LocationUnknown

appendComments :: Com a -> [Comment] -> Com a
appendComments com cx = case com of
  Com          a    -> ComAfter     a cx
  ComAfter     a ax -> ComAfter     a (ax++cx)
  ComBefore ax a    -> ComAround ax a cx
  ComAround ax a bx -> ComAround ax a (bx++cx)

com :: [Comment] -> a -> [Comment] -> Com a
com before a after = case before of
  [] -> case after of
    [] -> Com a
    dx -> ComAfter a dx
  cx -> case after of
    [] -> ComBefore cx a
    dx -> ComAround cx a dx

setCommentBefore :: [Comment] -> Com a -> Com a
setCommentBefore cx com = case com of
  Com         a    -> ComBefore cx a
  ComBefore _ a    -> ComBefore cx a
  ComAfter    a dx -> ComAround cx a dx
  ComAround _ a dx -> ComAround cx a dx

setCommentAfter :: [Comment] -> Com a -> Com a
setCommentAfter cx com = case com of
  Com          a   -> ComAfter     a cx
  ComBefore dx a   -> ComAround dx a cx
  ComAfter     a _ -> ComAfter     a cx
  ComAround dx a _ -> ComAround dx a cx

unComment :: Com a -> a
unComment com = case com of
  Com         a   -> a
  ComBefore _ a   -> a
  ComAfter    a _ -> a
  ComAround _ a _ -> a

getComment :: Com a -> ([Comment], [Comment])
getComment com = case com of
  Com         _   -> ([], [])
  ComBefore a _   -> (a, [])
  ComAfter    _ b -> ([], b)
  ComAround a _ b -> (a, b)

----------------------------------------------------------------------------------------------------

-- | Defined such that the instantiation of 'CodeBlock' into the 'Executable' class executes each
-- 'ScriptExpr' in the 'CodeBlock', one after the other. Execution does not
-- occur within a 'execNested' because many other expressions which execute 'CodeBlock's,
-- especially 'TryCatch' expressions and 'ForLoop's need to be able to choose
-- when the stack is pushed so they can define temporary local variables.
newtype CodeBlock = CodeBlock { codeBlock :: [ScriptExpr] } deriving (Eq, Ord, Show, Typeable)

setupCodeBlock :: CodeBlock -> Exec Subroutine
setupCodeBlock scrp = do
  -- create the 'Data.IORef.IORef' for storing static variables
  statvars    <- liftIO (newIORef mempty)
  statrules   <- liftIO (newIORef mempty)
  statlambdas <- liftIO (newIORef mempty)
  return $
    Subroutine
    { origSourceCode = scrp
    , staticVars     = statvars
    , staticRules    = statrules
    , staticLambdas  = statlambdas
    , executable     = execute scrp >> return Nothing
    }

instance Monoid CodeBlock where
  mempty      = CodeBlock []
  mappend a b = CodeBlock (mappend (codeBlock a) (codeBlock b))

instance HasNullValue CodeBlock where
  nullValue = mempty
  testNull (CodeBlock []) = True
  testNull _ = False

instance HasLocation CodeBlock where
  getLocation o = case codeBlock o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = CodeBlock (fmap delLocation (codeBlock o))

-- binary 0xDD 
instance B.Binary CodeBlock MTab where
  put (CodeBlock o) = B.prefixByte 0xDD $ B.put o
  get = B.tryWord8 0xDD $ CodeBlock <$> B.get

instance PPrintable CodeBlock where { pPrint = pPrintInterm }

instance Executable CodeBlock () where { execute (CodeBlock ox) = mapM_ execute ox }

instance ObjectClass CodeBlock where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass CodeBlock where
  haskellDataInterface = interface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> autoDefPPrinter
    defDeref $ \o -> catchError (execute o >> return Nothing) $ \e -> case e of
      ExecReturn o -> return o
      ExecError{}  -> throwError e
    -- TODO: define autoDefIterator, defIndexer, autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A subroutine is contains a 'CodeBlock' and an 'Data.IORef.IORef' to it's own static data. It
-- also has a reference to the last evaluation of 'execute' over it's 'CodeBlock', which provides a
-- hint to the Haskell runtime system that this code can be cached rather than evaluating the
-- 'CodeBlock' fresh every time. In a sense, it is a "live" 'CodeBlock' that can actually be
-- executed.
data Subroutine
  = Subroutine
    { origSourceCode :: CodeBlock
    , staticVars     :: IORef (M.Map Name Object)
    , staticRules    :: IORef (PatternTree Object [Subroutine])
    , staticLambdas  :: IORef [CallableCode]
    , executable     :: Exec (Maybe Object)
    }
  deriving Typeable

instance Show Subroutine where { show o = "Subroutine "++show (codeBlock (origSourceCode o)) }

instance NFData Subroutine where { rnf (Subroutine a _ _ _ _) = deepseq a () }

instance HasNullValue Subroutine where
  nullValue =
    Subroutine
    { origSourceCode = nullValue
    , staticVars = error "accessed staticVars or null Subroutine"
    , staticRules = error "accessed staticRules of null Subroutine"
    , staticLambdas = error "accessed staticLambdas of null Subroutine"
    , executable = return Nothing
    }
  testNull (Subroutine a _ _ _ _) = testNull a

instance PPrintable Subroutine where { pPrint = mapM_ pPrint . codeBlock . origSourceCode }

instance Executable Subroutine (Maybe Object) where
  execute sub = local (\x->x{currentCodeBlock=StaticStore(Just sub)}) $
    catchReturn return ((execute (origSourceCode sub) :: Exec ()) >> return Nothing) :: Exec (Maybe Object)

-- | Although 'Subroutine' instantiates 'Executable', this function allows you to easily place a
-- group of defined local variables onto the call stack before and the have the 'Subroutine'
-- executed.
runCodeBlock :: T_dict -> Subroutine -> Exec (Maybe Object)
runCodeBlock initStack exe = local (\xunit -> xunit{currentCodeBlock = StaticStore (Just exe)}) $!
  execFuncPushStack initStack (executable exe >>= liftIO . evaluate)

----------------------------------------------------------------------------------------------------

instance ObjectClass (PatternTree Object [Subroutine]) where { obj=new ; fromObj=objFromHaskellData; }

instance HaskellDataClass (PatternTree Object [Subroutine]) where
  haskellDataInterface = interface mempty $ do
    autoDefNullTest
    let initParams ox = case ox of
          [] -> return mempty
          _  -> execThrow $ obj [obj "\"RuleSet\" constructor takes no intializing parameters"]
          -- TODO: ^ the constructor for a 'PatternTree' should take tokenizer function.
    let listParams tree =
          foldM (\ tree (i, o) -> case fromObj o >>= \ (HaskellData o _) -> fromDynamic o of
            Nothing -> execThrow $ obj $
              [obj "item #", obj (i::Int), obj "in the initializing list is not a rule object"]
            Just (GlobAction{ globPattern=pat, globSubroutine=sub }) -> return $
              insertMultiPattern (++) pat [sub] tree ) tree . zip [1..]
    defListInit initParams listParams
    defInfixOp ORB $ \ _ tree o -> (new . T.unionWith (++) tree) <$> xmaybe (fromObj o)
    defSizer $ return . obj . T.size

----------------------------------------------------------------------------------------------------

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion with Haskell's "Data.Function"). 
data CallableCode
  = CallableCode
    { argsPattern    :: ParamListExpr
    , returnType     :: ObjType
    , codeSubroutine :: Subroutine
    }
  deriving (Show, Typeable)

-- Used by the instantiation of CallableCode and GlobAction into the PPrintable class.
ppCallableAction :: String -> PPrint -> ObjType -> Subroutine -> PPrint
ppCallableAction what pats typ exe =
  pClosure (pString what >> pats >> pPrint typ) "{" "}" (map pPrint (codeBlock (origSourceCode exe)))

-- | Interface used during evaluation of Dao scripts to determine whether or not an if or while
-- statement should continue. Also, turns out to be handy for plenty of other reasons.
instance HasNullValue CallableCode where
  nullValue =
    CallableCode{argsPattern=nullValue, returnType=nullValue, codeSubroutine=nullValue}
  testNull (CallableCode a b c) = testNull a && testNull b && testNull c

instance NFData CallableCode  where { rnf (CallableCode  a b _) = deepseq a $! deepseq b () }

instance PPrintable CallableCode where 
  pPrint (CallableCode pats ty exe) = ppCallableAction "function" (pPrint pats) ty exe

instance ObjectClass CallableCode where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass CallableCode where
  haskellDataInterface = interface (CallableCode undefined undefined undefined) $ do
    autoDefNullTest >> autoDefPPrinter
    defCallable (return . return)

----------------------------------------------------------------------------------------------------

-- A subroutine that is executed when a query string matches it's @['Dao.Glob.Glob']@ expression.
data GlobAction
  = GlobAction
    { globPattern    :: [Glob Object]
    , globSubroutine :: Subroutine
    }
  deriving (Show, Typeable)

instance NFData GlobAction where { rnf (GlobAction a b) = deepseq a $! deepseq b () }

instance HasNullValue GlobAction where
  nullValue = GlobAction{globPattern=[], globSubroutine=nullValue}
  testNull (GlobAction a b) = null a && testNull b

instance PPrintable GlobAction where
  pPrint (GlobAction pats exe) = (\a -> ppCallableAction "rule" a nullValue exe) $ case pats of
    []    -> pString "()"
    [pat] -> pPrint pat
    pats  -> pList_ "(" ", " ")" (map pPrint pats)

----------------------------------------------------------------------------------------------------

-- | This node in the AST typically represents the list of 'AST_Script' expressions found between
-- curly-brackets in expressions like "if" and "else" statement, "for" statements and "while"
-- statements, "with" satements, "try" and "catch" statements and function declrataions.
newtype AST_CodeBlock = AST_CodeBlock{ getAST_CodeBlock :: [AST_Script] }
  deriving (Eq, Ord, Typeable, Show)
  -- A code block is never standing on it's own, it is always part of a larger expression, so there
  -- is no 'Dao.Token.Location' parameter for 'AST_CodeBlock'.

instance Monoid AST_CodeBlock where
  mempty      = AST_CodeBlock []
  mappend a b = AST_CodeBlock (mappend (getAST_CodeBlock a) (getAST_CodeBlock b))

instance NFData AST_CodeBlock where { rnf (AST_CodeBlock a) = deepseq a () }

instance HasNullValue AST_CodeBlock where
  nullValue = AST_CodeBlock []
  testNull (AST_CodeBlock a) = null a

instance HasLocation AST_CodeBlock where                                      
  getLocation o = case getAST_CodeBlock o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = AST_CodeBlock (fmap delLocation (getAST_CodeBlock o))

-- 'pPrintComWith' wasn't good enough for this, because the comments might occur after the header
-- but before the opening bracket.
pPrintComCodeBlock :: PPrint -> Com AST_CodeBlock -> PPrint
pPrintComCodeBlock header c = case c of
  Com          c    -> run [] c []
  ComBefore bx c    -> run bx c []
  ComAfter     c ax -> run [] c ax
  ComAround bx c ax -> run bx c ax
  where
    run :: [Comment] -> AST_CodeBlock -> [Comment] -> PPrint
    run before cx after = case getAST_CodeBlock cx of
      [] -> header >> pInline (map pPrint before) >> pString " {}" >> pInline (map pPrint after)
      cx -> do
        pClosure (header >> pInline (map pPrint before)) " { " " }" (map (pGroup True . pPrint) cx)
        pInline (map pPrint after)

pPrintSubBlock :: PPrint -> AST_CodeBlock -> PPrint
pPrintSubBlock header px = pPrintComCodeBlock header (Com px)

instance PPrintable AST_CodeBlock where { pPrint o = mapM_ pPrint (getAST_CodeBlock o) }

instance ToDaoStructClass AST_CodeBlock where
  toDaoStruct = void $ renameConstructor "CodeBlock" >> "block" .=@ getAST_CodeBlock

instance FromDaoStructClass AST_CodeBlock where
  fromDaoStruct = constructor "CodeBlock" >> AST_CodeBlock <$> req "block"

instance HasRandGen AST_CodeBlock where
  randO = _randTrace "AST_CodeBlock" $ countNode $ AST_CodeBlock . concat <$> sequence [return <$> scrambO, depthLimitedInt 16 >>= \x -> randList 0 x]
  defaultO = _randTrace "D.AST_CodeBlock" $ return $ AST_CodeBlock []

instance Intermediate CodeBlock AST_CodeBlock where
  toInterm   (AST_CodeBlock ast) = [CodeBlock     $ ast >>= toInterm  ]
  fromInterm (CodeBlock     obj) = [AST_CodeBlock $ obj >>= fromInterm]

instance ObjectClass AST_CodeBlock where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_CodeBlock where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Functions and function parameters can specify optional type-checking expressions. This is a
-- data type that wraps a dao-typeable expression with type information.
data TyChkExpr a
  = NotTypeChecked{tyChkItem::a}
    -- ^ no type information was specified for this item
  | TypeChecked   {tyChkItem::a, tyChkExpr::ArithExpr, tyChkLoc::Location}
    -- ^ type check information was specified and should be checked every time it is evaluated.
  | DisableCheck  {tyChkItem::a, tyChkExpr::ArithExpr, typChkResult::Object, tyChkLoc::Location}
    -- ^ type check information was specified but has been disabled for efficiency reasons because
    -- we have verified that the item will always return a succesfull type-check.
  deriving (Eq, Ord, Typeable, Show)

checkedExpr :: TyChkExpr a -> a
checkedExpr o = case o of
  NotTypeChecked o       -> o
  TypeChecked    o _ _   -> o
  DisableCheck   o _ _ _ -> o

instance Functor TyChkExpr where
  fmap f (NotTypeChecked   a  ) = NotTypeChecked (f a)
  fmap f (TypeChecked  a b c  ) = TypeChecked  (f a) b c
  fmap f (DisableCheck a b c d) = DisableCheck (f a) b c d

instance NFData a => NFData (TyChkExpr a) where
  rnf (NotTypeChecked   a  ) = deepseq a ()
  rnf (TypeChecked  a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (DisableCheck a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue a => HasNullValue (TyChkExpr a) where
  nullValue = NotTypeChecked nullValue
  testNull (NotTypeChecked a) = testNull a
  testNull _ = False

instance HasLocation a => HasLocation (TyChkExpr a) where
  getLocation a     = case a of
    NotTypeChecked a         -> getLocation a
    TypeChecked    a _   loc -> getLocation a <> loc
    DisableCheck   a _ _ loc -> getLocation a <> loc
  setLocation a loc = case a of
    NotTypeChecked a       -> NotTypeChecked (setLocation a loc)
    TypeChecked    a b   _ -> TypeChecked  a b loc
    DisableCheck   a b c _ -> DisableCheck a b c loc
  delLocation a     = case a of
    NotTypeChecked a       -> NotTypeChecked (delLocation a)
    TypeChecked    a b   _ -> TypeChecked (delLocation a) (delLocation b) LocationUnknown
    DisableCheck   a b c _ -> DisableCheck a b c LocationUnknown

instance PPrintable a => PPrintable (TyChkExpr a) where
  pPrint a = case a of
    NotTypeChecked a        -> pPrint a
    TypeChecked    a expr _ -> pInline [pPrint a, pString ": ", pPrint expr]
    DisableCheck   a  _ _ _ -> pInline [pPrint a]

-- binary 0xC5 0xC7
instance B.Binary a MTab => B.Binary (TyChkExpr a) MTab where
  put o = case o of
    NotTypeChecked a       -> B.prefixByte 0xC5 $ B.put a
    TypeChecked    a b c   -> B.prefixByte 0xC6 $ B.put a >> B.put b >> B.put c
    DisableCheck   a b c d -> B.prefixByte 0xC7 $ B.put a >> B.put b >> B.put c >> B.put d
  get = B.word8PrefixTable <|> fail "expecting TyChkExpr"

instance B.Binary a MTab => B.HasPrefixTable (TyChkExpr a) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "TyChkExpr" 0xC5 0xC7 $
    [ NotTypeChecked <$> B.get
    , return TypeChecked  <*> B.get <*> B.get <*> B.get
    , return DisableCheck <*> B.get <*> B.get <*> B.get <*> B.get
    ]

instance (Eq a, Ord a, Typeable a, ObjectClass a) =>
  ObjectClass (TyChkExpr a) where { obj=new; fromObj=objFromHaskellData; }

instance (Eq a, Ord a, Typeable a, ObjectClass a) =>
  HaskellDataClass (TyChkExpr a) where
    haskellDataInterface = interface (NotTypeChecked $ error "undefined TyChkExpr") $ do
      autoDefEquality >> autoDefOrdering

----------------------------------------------------------------------------------------------------

-- | This node can be found in a few different syntactic structures. When a name or function or
-- expression is followed by a colon and some type checking information, this node is used for that
-- purpose.
data AST_TyChk a
  = AST_NotChecked a
  | AST_Checked    a (Com ()) AST_Arith Location
  deriving (Eq, Ord, Typeable, Show)

checkedAST :: AST_TyChk a -> a
checkedAST a = case a of { AST_NotChecked a -> a; AST_Checked a _ _ _ -> a; }

astTyChkDelLocWith :: (a -> a) -> AST_TyChk a -> AST_TyChk a
astTyChkDelLocWith del a = case a of
  AST_NotChecked a       -> AST_NotChecked (del a)
  AST_Checked    a b c _ -> AST_Checked    (del a) b (delLocation c) LocationUnknown

instance Functor AST_TyChk where
  fmap f (AST_NotChecked a      ) = AST_NotChecked (f a)
  fmap f (AST_Checked    a b c d) = AST_Checked    (f a) b c d

instance NFData a => NFData (AST_TyChk a) where
  rnf (AST_NotChecked    a) = deepseq a ()
  rnf (AST_Checked a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue a => HasNullValue (AST_TyChk a) where
  nullValue = AST_NotChecked nullValue
  testNull (AST_NotChecked  a  ) = testNull a
  testNull (AST_Checked _ _ a _) = testNull a

instance PPrintable a => PPrintable (AST_TyChk a) where
  pPrint a = case a of
    AST_NotChecked a          -> pPrint a
    AST_Checked    a coms expr _ -> pInline $
      [ pPrint a
      , pPrintComWith (\ () -> pString ": ") coms
      , pPrint expr
      ]

instance ObjectClass a => ToDaoStructClass (AST_TyChk a) where
  toDaoStruct = ask >>= \o -> renameConstructor "TypeChecked" >> case o of
    AST_NotChecked o              -> void $ define "data" (obj o)
    AST_Checked    o coms typ loc -> do
      define "data" (obj o)
      "colon"    .= coms
      "typeExpr" .= typ
      putLocation loc

instance HasLocation a => HasLocation (AST_TyChk a) where
  getLocation a     = case a of
    AST_NotChecked a         -> getLocation a
    AST_Checked    a _ _ loc -> getLocation a <> loc
  setLocation a loc = case a of
    AST_NotChecked a         -> AST_NotChecked (setLocation a loc)
    AST_Checked    a b c _   -> AST_Checked a b c loc
  delLocation       = astTyChkDelLocWith delLocation

instance HasRandGen a => HasRandGen (AST_TyChk a) where
  randO = _randTrace "(AST_TyChk a)" $ countNode $ AST_NotChecked <$> randO
  --randChoice = randChoiceList [AST_NotChecked <$> randO, return AST_Checked <*> randO <*> randO <*> randO <*> no]
  defaultO = _randTrace "D.(AST_TyChk a)" $ AST_NotChecked <$> defaultO

instance (Eq a, Ord a, PPrintable a, Typeable a, ObjectClass a) =>
  ObjectClass (AST_TyChk a) where { obj=new; fromObj=objFromHaskellData; }

instance (Eq a, Ord a, PPrintable a, Typeable a) => HaskellDataClass (AST_TyChk a) where
  haskellDataInterface = interface (AST_NotChecked $ error "undefined AST_TyChk") $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

tyChkToInterm :: (b -> [a]) -> AST_TyChk b -> [TyChkExpr a]
tyChkToInterm ti a = case a of
  AST_NotChecked a         -> NotTypeChecked <$> ti a
  AST_Checked    a _ b loc -> [TypeChecked] <*> ti a <*> toInterm b <*> [loc]

tyChkFromInterm :: (a -> [b]) -> TyChkExpr a -> [AST_TyChk b]
tyChkFromInterm fi a = case a of
  NotTypeChecked a         -> AST_NotChecked <$> fi a
  TypeChecked    a b   loc -> [AST_Checked] <*> fi a <*> [Com ()] <*> fromInterm b <*> [loc]
  DisableCheck   a b _ loc -> [AST_Checked] <*> fi a <*> [Com ()] <*> fromInterm b <*> [loc]

----------------------------------------------------------------------------------------------------

-- | 'ParamExpr' is a part of the Dao language semantics, and is also used in the the 'CallableCode'
-- data type when evaluating parameters to be passed to the callable code function execution. The
-- boolean parameter here indicates whether or not the parameter should be passed by reference.
data ParamExpr = ParamExpr Bool (TyChkExpr Name) Location deriving (Eq, Ord, Typeable, Show)

instance NFData ParamExpr where
  rnf (ParamExpr       a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasLocation ParamExpr where
  getLocation (ParamExpr _ _ loc)     = loc
  setLocation (ParamExpr a b _  ) loc = ParamExpr a b loc
  delLocation (ParamExpr a b _  )     = ParamExpr a b LocationUnknown

instance PPrintable ParamExpr where
  pPrint (ParamExpr byRef tychk _) = when byRef (pString "$") >> pPrint tychk

instance PPrintable [ParamExpr] where { pPrint lst = pList_ "(" ", " ")" (fmap pPrint lst) }

-- binary 0xCF 0xD0
instance B.Binary ParamExpr MTab where
  put (ParamExpr True  a b) = B.prefixByte 0xCF $ B.put a >> B.put b
  put (ParamExpr False a b) = B.prefixByte 0xD0 $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting ParamExpr"

instance B.HasPrefixTable ParamExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParamExpr" 0xCF 0xD0 $
    [ return (ParamExpr True ) <*> B.get <*> B.get
    , return (ParamExpr False) <*> B.get <*> B.get
    ]

instance ObjectClass ParamExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ParamExpr where
  haskellDataInterface = interface (ParamExpr False (error "undefined ParamExpr") LocationUnknown) $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_Param
  = AST_NoParams
  | AST_Param (Maybe [Comment]) (AST_TyChk Name) Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Param where
  rnf  AST_NoParams     = ()
  rnf (AST_Param a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_Param where
  nullValue = AST_NoParams
  testNull AST_NoParams = True
  testNull _ = False

instance HasLocation AST_Param where
  getLocation a     = case a of
    AST_NoParams      -> LocationUnknown
    AST_Param _ _ loc -> loc
  setLocation a loc = case a of
    AST_NoParams    -> AST_NoParams
    AST_Param a b _ -> AST_Param a b loc
  delLocation a     = case a of
    AST_NoParams    -> AST_NoParams
    AST_Param a b _ -> AST_Param a (delLocation b) LocationUnknown

instance PPrintable AST_Param where
  pPrint o = case o of
    AST_NoParams            -> return ()
    AST_Param mcoms tychk _ -> pInline $
      [ maybe (return ()) (\coms -> pString "$" >> pPrint coms) mcoms
      , pPrint tychk
      ]

instance PPrintable [Com AST_Param] where
  pPrint lst = pList_ "(" ", " ")" (fmap pPrint lst)

instance ToDaoStructClass AST_Param where
  toDaoStruct = ask >>= \o -> case o of
    AST_NoParams             -> makeNullary "Void"
    AST_Param coms tychk loc -> do
      renameConstructor "Parameter"
      maybe (return ()) putComments coms
      "typeCheck" .= tychk
      putLocation loc

instance FromDaoStructClass AST_Param where
  fromDaoStruct = msum $
    [ nullary "Void" >> return AST_NoParams
    , constructor "Parameter" >> return AST_Param <*> optComments <*> req "typeCheck" <*> location
    ]

instance HasRandGen AST_Param where
  randO = _randTrace "AST_Param" $ countNode $ return AST_Param <*> randO <*> randO <*> no
  defaultO = _randTrace "D.AST_Param" $ return AST_Param <*> defaultO <*> defaultO <*> no

instance HasRandGen [Com AST_Param] where
  randO = _randTrace "[Com AST_Param]" $ recurse $ depthLimitedInt 8 >>= \x -> randListOf 0 x scrambO
  defaultO = _randTrace "D.[Com AST_Param]" $ defaultList 0 1

instance Intermediate ParamExpr AST_Param where
  toInterm   a = case a of
    AST_NoParams      -> []
    AST_Param a b loc ->
      [ParamExpr] <*> [maybe False (const True) a]     <*> tyChkToInterm   return b <*> [loc]
  fromInterm o = case o of
    ParamExpr a b loc ->
      [AST_Param] <*> [if a then Just [] else Nothing] <*> tyChkFromInterm return b <*> [loc]

instance Intermediate [ParamExpr] [Com AST_Param] where
  toInterm   ax = [ax >>= toInterm . unComment]
  fromInterm ax = [ax >>= fmap Com . fromInterm]

instance ObjectClass AST_Param where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Param where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A list of function parameters (arguments) to a function in an object representing a function
-- expression.
data ParamListExpr = ParamListExpr (TyChkExpr [ParamExpr]) Location
  deriving (Eq, Ord, Typeable, Show)

getTypeCheckList :: ParamListExpr -> [ParamExpr]
getTypeCheckList (ParamListExpr tychk _) = tyChkItem tychk 

instance NFData ParamListExpr where { rnf (ParamListExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue ParamListExpr where
  nullValue = ParamListExpr (NotTypeChecked []) LocationUnknown
  testNull (ParamListExpr (NotTypeChecked []) _) = True
  testNull _ = False

-- binary 0xD6 
instance B.Binary ParamListExpr MTab where
  put (ParamListExpr tyChk loc) = B.prefixByte 0xD6 $ B.put tyChk >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting ParamListExpr"

instance B.HasPrefixTable ParamListExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParamListExpr" 0xD6 0xD6 $ [return ParamListExpr <*> B.get <*> B.get]

instance HasLocation ParamListExpr where
  getLocation (ParamListExpr _ loc)     = loc
  setLocation (ParamListExpr a _  ) loc = ParamListExpr a loc
  delLocation (ParamListExpr a _  )     = ParamListExpr a LocationUnknown

instance PPrintable ParamListExpr where { pPrint (ParamListExpr lst _) = pPrint lst }

instance ObjectClass ParamListExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ParamListExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

-- | Convert an 'ObjectExpr' to an 'Dao.Glob.Glob'.
paramsToGlobExpr :: ObjectExpr -> Exec (Glob UStr)
paramsToGlobExpr o = case o of
  ObjLiteralExpr (LiteralExpr (OString str) _) -> return (read (uchars str))
  _ -> execThrow $ obj $ [obj "does not evaluate to a \"glob\" pattern"]

----------------------------------------------------------------------------------------------------

data AST_ParamList
  = AST_ParamList (AST_TyChk [Com AST_Param]) Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_ParamList where { rnf (AST_ParamList a b) = deepseq a $! deepseq b () }

instance HasNullValue AST_ParamList where
  nullValue = AST_ParamList nullValue LocationUnknown
  testNull (AST_ParamList a _) = testNull a

instance HasLocation AST_ParamList where
  getLocation (AST_ParamList _ loc)     = loc
  setLocation (AST_ParamList a _  ) loc = AST_ParamList a loc
  delLocation (AST_ParamList a _  )     = AST_ParamList (astTyChkDelLocWith (fmap delLocation) a) LocationUnknown

instance PPrintable AST_ParamList where
  pPrint (AST_ParamList lst _) = pInline [pPrint lst]

instance ToDaoStructClass AST_ParamList where
  toDaoStruct = ask >>= \o -> case o of
    AST_ParamList tychk loc -> do
      renameConstructor "ParamList"
      "typeCheck" .= tychk
      putLocation loc

instance FromDaoStructClass AST_ParamList where
  fromDaoStruct = constructor "ParamList" >> return AST_ParamList <*> req "typeCheck" <*> location

instance HasRandGen AST_ParamList where
  randO = _randTrace "AST_ParamList" $ countNode $ return AST_ParamList <*> randO <*> no
  defaultO = _randTrace "D.AST_ParamList" $ return $ AST_ParamList nullValue LocationUnknown

instance Intermediate ParamListExpr AST_ParamList where
  toInterm   (AST_ParamList ox loc) = [ParamListExpr] <*> tyChkToInterm   toInterm   ox <*> [loc]
  fromInterm (ParamListExpr ox loc) = [AST_ParamList] <*> tyChkFromInterm fromInterm ox <*> [loc]

instance ObjectClass AST_ParamList where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_ParamList where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data RuleHeadExpr
  = RuleStringExpr  UStr      Location
  | RuleHeadExpr [AssignExpr] Location
  deriving (Eq, Ord, Typeable, Show)

instance HasNullValue RuleHeadExpr where
  nullValue = RuleStringExpr nil LocationUnknown
  testNull (RuleStringExpr a _) = a==nil
  testNull _ = False

instance HasLocation RuleHeadExpr where
  getLocation o     = case o of
    RuleStringExpr _ o -> o
    RuleHeadExpr   _ o -> o
  setLocation o loc = case o of
    RuleStringExpr o _ -> RuleStringExpr o loc
    RuleHeadExpr   o _ -> RuleHeadExpr o loc
  delLocation o     = case o of
    RuleStringExpr o _ -> RuleStringExpr o LocationUnknown
    RuleHeadExpr   o _ -> RuleHeadExpr (fmap delLocation o) LocationUnknown

instance NFData RuleHeadExpr where
  rnf (RuleStringExpr a b) = deepseq a $! deepseq b ()
  rnf (RuleHeadExpr     a b) = deepseq a $! deepseq b ()

-- binary 0x7A 0x7B
instance B.Binary RuleHeadExpr MTab where
  put o = case o of
    RuleStringExpr a b -> B.prefixByte 0x7A $ B.put a >> B.put b
    RuleHeadExpr   a b -> B.prefixByte 0x7B $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting RuleHeadExpr"

instance B.HasPrefixTable RuleHeadExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RuleHeadExpr" 0x7A 0x7B
    [ return RuleStringExpr <*> B.get <*> B.get
    , return RuleHeadExpr   <*> B.get <*> B.get
    ]

instance Executable RuleHeadExpr [UStr] where
  execute o = case o of
    RuleStringExpr r _ -> return [r]
    RuleHeadExpr   r _ ->
      forM r (\o ->
          execute o >>= checkVoid (getLocation o) "item in rule header" >>= derefObject
        ) >>= requireAllStringArgs "in rule header"

instance ObjectClass RuleHeadExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass RuleHeadExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_RuleHeader
  = AST_NullRules  [Comment]  Location
  | AST_RuleString (Com UStr) Location
  | AST_RuleHeader [Com AST_Assign] Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_RuleHeader where
  rnf (AST_NullRules  a b) = deepseq a $! deepseq b ()
  rnf (AST_RuleString a b) = deepseq a $! deepseq b ()
  rnf (AST_RuleHeader a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_RuleHeader where
  nullValue = AST_NullRules [] LocationUnknown
  testNull (AST_NullRules _ _) = True
  testNull _ = False

instance HasLocation AST_RuleHeader where
  getLocation o     = case o of
    AST_NullRules  _ o -> o
    AST_RuleString _ o -> o
    AST_RuleHeader _ o -> o
  setLocation o loc = case o of
    AST_NullRules  a _ -> AST_NullRules  a loc
    AST_RuleString a _ -> AST_RuleString a loc
    AST_RuleHeader a _ -> AST_RuleHeader a loc
  delLocation o     = case o of
    AST_NullRules  a _ -> AST_NullRules  a LocationUnknown
    AST_RuleString a _ -> AST_RuleString a LocationUnknown
    AST_RuleHeader a _ -> AST_RuleHeader (fmap delLocation a) LocationUnknown

instance PPrintable AST_RuleHeader where
  pPrint o = case o of
    AST_NullRules  coms _ -> pInline [pString "rule(", pPrint coms, pString ")"]
    AST_RuleString r    _ -> pInline [pString "rule ", pPrintComWith pShow r, pString " "]
    AST_RuleHeader ruls _ -> pList (pString "rule") "(" ", " ")" (fmap pPrint ruls)

instance ToDaoStructClass AST_RuleHeader where
  toDaoStruct = ask >>= \o -> case o of
    AST_NullRules coms loc -> do
      renameConstructor "NoStrings"
      putComments coms >> putLocation loc
    AST_RuleString itm loc -> do
      renameConstructor "StringItem"
      "items" .= itm >> putLocation loc
    AST_RuleHeader lst loc -> do
      renameConstructor "ValuesList"
      "items" .= lst >> putLocation loc

instance FromDaoStructClass AST_RuleHeader where
  fromDaoStruct = msum $
    [ constructor "NoStrings"  >> return AST_NullRules  <*> comments <*> location
    , constructor "StringItem" >> return AST_RuleString <*> req "items" <*> location
    , constructor "ValuesList" >> return AST_RuleHeader <*> reqList "items" <*> location
    ]

instance HasRandGen AST_RuleHeader where
  randO = _randTrace "AST_RuleHeader" $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_RuleHeader <*> randList 0 3 <*> no
    , return AST_RuleString <*> randO   <*> no
    , return AST_NullRules  <*> scrambO <*> no
    ]
  defaultO = _randTrace "D.AST_RuleHeader" runDefaultChoice
  defaultChoice = randChoiceList $
    [ return AST_NullRules  <*> defaultO <*> no
    , return AST_RuleString <*> defaultO <*> no
    ]

instance Intermediate RuleHeadExpr AST_RuleHeader where
  toInterm   o = case o of
    AST_NullRules  _ loc -> [RuleHeadExpr              []     loc]
    AST_RuleString o loc -> [RuleStringExpr (unComment o)     loc]
    AST_RuleHeader o loc -> [RuleHeadExpr] <*> [o>>=uc0] <*> [loc]
  fromInterm o = case o of
    RuleHeadExpr   [] loc -> [AST_NullRules               []     loc]
    RuleStringExpr o  loc -> [AST_RuleString       (Com   o)     loc]
    RuleHeadExpr   o  loc -> [AST_RuleHeader] <*> [o>>=nc0] <*> [loc]

instance ObjectClass AST_RuleHeader where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_RuleHeader where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- $Exec_helpers
-- Functions for working with object values when building built-in functions callable from within a
-- dao script.

asReference :: Object -> XPure Reference
asReference = xmaybe . fromObj

asInteger :: Object -> XPure Integer
asInteger o = case o of
  OWord    o -> return (toInteger o)
  OInt     o -> return (toInteger o)
  OLong    o -> return o
  OFloat   o -> return (round o)
  ORatio   o -> return (round o)
  ORelTime o -> return (round (toRational o))
  _          -> mzero

asRational :: Object -> XPure Rational
asRational o = case o of
  OInt     o -> return (toRational o)
  OWord    o -> return (toRational o)
  OLong    o -> return (toRational o)
  OFloat   o -> return (toRational o)
  ORelTime o -> return (toRational o)
  ORatio   o -> return o
  OComplex o | imagPart o == 0 -> return (toRational (realPart o))
  _          -> mzero

asComplex :: Object -> XPure T_complex
asComplex o = case o of
  OComplex o -> return o
  o          -> asRational o >>= return . flip complex 0 . fromRational

-- | A function which is basically the absolute value function, except it also works on 'Complex'
-- numbers, returning the magnitude of the number if it is 'Complex'.
asPositive :: Object -> XPure Object
asPositive o = case o of
  OInt     o -> return (OInt     $ abs       o)
  OWord    o -> return (OWord                o)
  OLong    o -> return (OLong    $ abs       o)
  OFloat   o -> return (OFloat   $ abs       o)
  ORelTime o -> return (ORelTime $ abs       o)
  OComplex o -> return (OFloat   $ magnitude o)
  _          -> mzero

-- | Remove one layer of 'OList' objects, i.e. any objects in the list that are 'OList' constructors
-- will have the contents of those lists concatenated, and all non-'OList' constructors are treated
-- as lists of single objects. Also returns the number of concatenations.
objConcat :: [Object] -> (Int, [Object])
objConcat ox = (sum a, concat b) where
  (a, b) = unzip (ox >>= \o -> maybe [(0, [o])] (return . (,) 1) (fromObj o))

-- | Checks if an 'Object' is a numerical type, returns the numeric 'CoreType' if so, evaluates to
-- 'Control.Monad.mzero' if not.
isNumeric :: Object -> XPure CoreType
isNumeric o = do
  let t = typeOfObj o
  guard (CharType <= t && t <= ComplexType)
  return t

eval_Prefix_op :: ArithPfxOp -> Object -> XPure Object
eval_Prefix_op op o = join $ xmaybe $
  fromObj o >>= \ (HaskellData o ifc) -> objArithPfxOpTable ifc >>= (! op) >>= \f -> return (f op o)

-- Pass the 'InfixOp' associated with the 'Prelude.Num' function so it can check whether the
-- 'defInfixOp' for that operator has been defined for objects of 'HaskellType'.
eval_Infix_op :: InfixOp -> Object -> Object -> XPure Object
eval_Infix_op op a b = join $ xmaybe $ onhask a b <|> (guard isCommut >> onhask b a) where
  isCommut = infixOpCommutativity op
  onhask a b = fromObj a >>= \ (HaskellData a ifc) ->
    (\f -> f op a b) <$> (objInfixOpTable ifc >>= (! op))

_evalNumOp1 :: (forall a . Num a => a -> a) -> Object -> XPure Object
_evalNumOp1 f o = case o of
  OChar    o -> return $ OChar    $ chr $ mod (f $ ord o) (ord maxBound)
  OInt     o -> return $ OInt     $ f o
  OWord    o -> return $ OWord    $ f o
  OLong    o -> return $ OLong    $ f o
  ORelTime o -> return $ ORelTime $ f o
  OFloat   o -> return $ OFloat   $ f o
  ORatio   o -> return $ ORatio   $ f o
  OComplex o -> return $ OComplex $ f o
  _          -> mzero

-- Evaluate a 2-ary function for any core type that instantiates the 'Prelude.Num' class.
_evalNumOp2 :: (forall a . Num a => a -> a -> a) -> Object -> Object -> XPure Object
_evalNumOp2 f a b = do
  let t = max (typeOfObj a) (typeOfObj b)
  a <- castToCoreType t a
  b <- castToCoreType t b
  case (a, b) of
    (OChar     a, OChar     b) -> return $ OChar    $ chr $ mod (f (ord a) (ord b)) (ord maxBound)
    (OInt      a, OInt      b) -> return $ OInt     $ f a b
    (OWord     a, OWord     b) -> return $ OWord    $ f a b
    (OLong     a, OLong     b) -> return $ OLong    $ f a b
    (ORelTime  a, ORelTime  b) -> return $ ORelTime $ f a b
    (OFloat    a, OFloat    b) -> return $ OFloat   $ f a b
    (ORatio    a, ORatio    b) -> return $ ORatio   $ f a b
    (OComplex  a, OComplex  b) -> return $ OComplex $ f a b
    _ -> mzero

instance Num (XPure Object) where
  a + b = a >>= \a -> b >>= \b -> case (a, b) of
    (OString a, OString b) -> return $ OString $ a<>b
    (OBytes  a, OBytes  b) -> return $ OBytes  $ a<>b
    (OList   a, OList   b) -> return $ OList   $ a++b
    (ODict   a, ODict   b) -> return $ ODict   $ M.union b a
    (a, b) -> _evalNumOp2 (+) a b <|> eval_Infix_op ADD a b
  a * b = a >>= \a -> case a of
    OList ax -> OList <$> mapM ((* b) . xpure) ax
    ODict ax -> (ODict . M.fromList) <$>
      mapM (\ (key, a) -> fmap ((,) key) (xpure a * b)) (M.assocs ax)
    a        -> b >>= \b -> case b of
      OList bx -> OList <$> mapM (((xpure a) *) . xpure) bx
      ODict bx -> (ODict . M.fromList) <$>
        mapM (\ (key, b) -> fmap ((,) key) (xpure a * xpure b)) (M.assocs bx)
      b        -> _evalNumOp2 (*) a b <|> eval_Infix_op MULT a b
  a - b = a >>= \a -> b >>= \b -> case a of
    -- on strings, the inverse operation of "join(b, a)", every occurence of b from a.
    OString a -> case b of
      OString b -> return $ OString $ mconcat $ splitString a b
      _         -> mzero
    -- on lists, removes the item b from the list a
    OList   a -> return $ OList $ filter (/= b) a
    ODict   a -> case b of
      ODict b -> return $ ODict $ M.difference a b
      ORef (Reference UNQUAL b NullRef) -> return $ ODict $ M.delete b a
      ORef _                             -> throwError $ obj $
        [ obj "cannot remove element from dictionary", b
        , obj "reference must be a single unqualified name"
        ]
      _ -> mzero
    a         -> _evalNumOp2 (-) a b <|> eval_Infix_op SUB a b
  negate a = (a >>= _evalNumOp1 negate) <|> (a >>= eval_Prefix_op NEGTIV)
  abs    a = (a >>= _evalNumOp1 abs   ) <|> (a >>= eval_Prefix_op NEGTIV)
  signum a = a >>= _evalNumOp1 signum
  fromInteger = return . obj

_xpureApplyError :: String -> String -> a
_xpureApplyError name msg = error $ concat $ concat $
  [["cannot evaluate ", name], guard (not $ null msg) >> [": ", msg]]

_xpureApply2 :: String -> (Object -> Object -> a) -> XPure Object -> XPure Object -> a
_xpureApply2 name f a b = case evalXPure $ a >>= \a -> b >>= \b -> return (f a b) of
  PFail   e -> _xpureApplyError name (prettyShow e)
  Backtrack -> _xpureApplyError name ""
  OK      o -> o

_xpureApplyM :: String -> (Object -> XPure a) -> XPure Object -> a
_xpureApplyM name f o = case evalXPure $ o >>= f of
  PFail   e -> _xpureApplyError name (prettyShow e)
  Backtrack -> _xpureApplyError name ""
  OK      o -> o

instance Eq (XPure Object) where
  (==) = _xpureApply2 "(==)" (==)
  (/=) = _xpureApply2 "(/=)" (/=)

instance Ord (XPure Object) where
  compare = _xpureApply2 "compare" compare
  (<)     = _xpureApply2 "(<)"  (<)
  (<=)    = _xpureApply2 "(<=)" (<=)
  (>)     = _xpureApply2 "(>)"  (>)
  (>=)    = _xpureApply2 "(>=)" (>=)

instance Real (XPure Object) where
  toRational = _xpureApplyM "toRational" $ castToCoreType LongType >=> xmaybe . fromObj

eval_Int_op1 :: String -> (forall a . Integral a => a -> a) -> XPure Object -> XPure Object
eval_Int_op1 name f o = o >>= \o -> case o of
  OChar o -> return $ OChar (chr $ flip mod (ord maxBound) $ f $ ord o)
  OInt  o -> return $ OInt  (f o)
  OWord o -> return $ OWord (f o)
  OLong o -> return $ OLong (f o)
  _       -> throwError $ obj [obj "cannot apply function", obj name, obj "on object", o]

_xpureCastTo :: XPure CoreType -> XPure Object -> XPure Object
_xpureCastTo typ a = join $ xpure castToCoreType <*> typ <*> a

-- In order for @('XPure' 'Object')@ to be used with the 'Prelude.Div' and 'Prelude.mod' functions,
-- it must instantiate 'Integral', which means it must instantiate 'Prelude.Enum'. This
-- instantiation is an attempt at making the functions behave as they would for ordinary enumerated
-- data types; it is /NOT/ pretty, but it basically works.
instance Enum (XPure Object) where
  succ = eval_Int_op1 "succ" succ
  pred = eval_Int_op1 "pred" pred
  toEnum = return . obj
  fromEnum = _xpureApplyM "fromEnum" $ castToCoreType IntType >=> xmaybe . fromObj
  enumFrom = fix (\loop o -> o : loop (succ o))
  enumFromThen lo hi = fix (\loop o -> o : loop (hi-lo+o)) lo
  enumFromTo a b =
    if maybe False (const True) (extractXPure typ)
    then  case compare aa bb of
            EQ -> repeat aa
            LT -> loop (<bb)         inc  aa
            GT -> loop (>bb) (negate inc) aa
    else  []
    where
      loop ok inc a = a : let b = a+inc in if ok b then loop ok inc b else []
      typ   = a >>= \a -> b >>= \b -> do
        let t = max (typeOfObj a) (typeOfObj b)
        guard (CharType <= t && t <= ComplexType) >> xpure t
      inc = typ >>= flip castToCoreType (OChar '\x01')
      aa  = _xpureCastTo typ a
      bb  = _xpureCastTo typ b
  enumFromThenTo a b c =
    if maybe False (const True) $ extractXPure typ
    then  case compare aa bb of
            EQ -> repeat aa
            LT -> if aa<cc then loop (<cc) aa else []
            GT -> if aa>cc then loop (>cc) aa else []
    else  []
    where
      loop ok a = a : let b = a+inc in if ok b then loop ok b else []
      typ = a >>= \a -> b >>= \b -> c >>= \c -> do
        let t = max (typeOfObj a) $ max (typeOfObj b) $ (typeOfObj c)
        guard (CharType <= t && t <= RatioType) >> xpure t
      inc = bb-aa
      aa  = _xpureCastTo typ a
      bb  = _xpureCastTo typ b
      cc  = _xpureCastTo typ c

_xpureDivFunc
  :: String
  -> (forall a . Integral a => a -> a -> (a, a))
  -> XPure Object -> XPure Object -> (XPure Object, XPure Object)
_xpureDivFunc name div a b = _xpureApply2 name f aa bb where
    f a b = case (a, b) of
      (OChar a, OChar b) -> pair (ord a) (ord b) (OChar . chr)
      (OInt  a, OInt  b) -> pair a b OInt
      (OWord a, OWord b) -> pair a b OWord
      (OLong a, OLong b) -> pair a b OLong
      _                  -> (mzero, mzero)
    pair a b constr = let (c, d) = div a b in (xpure $ constr c, xpure $ constr d)
    typ = a >>= \a -> b >>= \b -> do
      let t = max (typeOfObj a) (typeOfObj b)
      guard (CharType <= t && t <= LongType) >> xpure t
    aa = _xpureCastTo typ a
    bb = _xpureCastTo typ b

instance Integral (XPure Object) where
  toInteger = _xpureApplyM "toInteger" (castToCoreType LongType >=> xmaybe . fromObj)
  quotRem a b = _xpureDivFunc "quoteRem" quotRem a b
  divMod  a b = _xpureDivFunc "divMod"   divMod  a b
  div     a b = a >>= \a -> b >>= \b -> case (a, b) of
    (OString a, OString b) -> return $ OWord $ fromIntegral $ length $ splitString a b
    _ -> fst (_xpureDivFunc "(/)" divMod (xpure a) (xpure b)) <|> eval_Infix_op DIV a b
  mod     a b = a >>= \a -> b >>= \b -> case (a, b) of
    (OString a, OString b) -> return $ OList $ map OString $ splitString a b
    _ -> snd (_xpureDivFunc "(%)" divMod (xpure a) (xpure b)) <|> eval_Infix_op MOD a b

_xpureFrac :: (forall a . Floating a => a -> a) -> XPure Object -> XPure Object
_xpureFrac f a = a >>= \a -> case a of
  ORelTime a -> xpure $ ORelTime $ fromRational $ toRational $
    f (fromRational (toRational a) :: Double)
  OFloat   a -> xpure $ OFloat   $ f a
  ORatio   a -> xpure $ ORatio   $ toRational $ f $ (fromRational a :: Double)
  OComplex a -> xpure $ OComplex $ f a
  _          -> mzero

_xpureFrac2 :: (forall a . Floating a => a -> a -> a) -> XPure Object -> XPure Object -> XPure Object
_xpureFrac2 f a b = a >>= \a -> b >>= \b -> do
  let t = max (typeOfObj a) (typeOfObj b)
  a <- castToCoreType t a
  b <- castToCoreType t b
  case (a, b) of
    (ORelTime a, ORelTime b) -> xpure $ ORelTime $ fromRational $ toRational $
      f (fromRational (toRational a) :: Double) (fromRational (toRational b) :: Double)
    (OFloat   a, OFloat   b) -> xpure $ OFloat   $ f a b
    (ORatio   a, ORatio   b) -> xpure $ ORatio   $ toRational $
      f (fromRational a :: Double) (fromRational b :: Double)
    (OComplex a, OComplex b) -> xpure $ OComplex $ f a b
    _                        -> mzero

instance Fractional (XPure Object) where
  a / b  = _xpureFrac2 (/) a b
  recip = _xpureFrac recip
  fromRational = xpure . ORatio

instance Floating (XPure Object) where
  pi      = xpure $ OFloat pi
  logBase = _xpureFrac2 logBase
  (**)    = _xpureFrac2 (**)
  exp     = _xpureFrac exp
  sqrt    = _xpureFrac sqrt
  log     = _xpureFrac log
  sin     = _xpureFrac sin
  tan     = _xpureFrac tan
  cos     = _xpureFrac cos
  asin    = _xpureFrac asin
  atan    = _xpureFrac atan
  acos    = _xpureFrac acos
  sinh    = _xpureFrac sinh
  tanh    = _xpureFrac tanh
  cosh    = _xpureFrac cosh
  asinh   = _xpureFrac asinh
  atanh   = _xpureFrac atanh
  acosh   = _xpureFrac acosh

_xpureRealFrac :: Integral b => String -> (forall a . RealFrac a => a -> b) -> XPure Object -> b
_xpureRealFrac name f = _xpureApplyM name $ \o -> case o of
  ORelTime a -> xpure $ f a
  OFloat   a -> xpure $ f a
  ORatio   a -> xpure $ f a
  _          -> mzero

instance RealFrac (XPure Object) where
  properFraction = let name = "properFraction" in _xpureApplyM name $ \o -> case o of
    ORelTime o -> f ORelTime o
    OFloat   o -> f OFloat   o
    ORatio   o -> f ORatio   o
    _          -> xpure (error $ "cannot evaluate properFraction on object "++prettyShow o, mzero)
    where { f constr o = let (i, b) = properFraction o in xpure (i, xpure $ constr b) }
  truncate = _xpureRealFrac "truncate" truncate
  round    = _xpureRealFrac "round"    round
  ceiling  = _xpureRealFrac "ceiling"  ceiling
  floor    = _xpureRealFrac "floor"    floor

_xpureBits :: (forall a . Bits a => a -> a) -> (B.ByteString -> B.ByteString) -> XPure Object -> XPure Object
_xpureBits f g o = o >>= \o -> case o of
  OChar  o -> return $ OChar  $ chr $ mod (f $ ord o) (ord maxBound)
  OInt   o -> return $ OInt   $ f o
  OWord  o -> return $ OWord  $ f o
  OLong  o -> return $ OLong  $ f o
  OBytes o -> return $ OBytes $ g o
  _        -> mzero

_xpureBits2 :: InfixOp -> (forall a . Bits a => a -> a -> a) -> (T_dict -> T_dict -> T_dict) -> (B.ByteString -> B.ByteString -> B.ByteString) -> XPure Object -> XPure Object -> XPure Object
_xpureBits2 op bits dict bytes a b = a >>= \a -> b >>= \b -> do
  let t = max (typeOfObj a) (typeOfObj b)
  a <- castToCoreType t a
  b <- castToCoreType t b
  case (a, b) of
    (OChar  a, OChar  b) -> return $ OChar  $ chr $ mod (bits (ord a) (ord b)) (ord maxBound)
    (OInt   a, OInt   b) -> return $ OInt   $ bits a b
    (OWord  a, OWord  b) -> return $ OWord  $ bits a b
    (OLong  a, OLong  b) -> return $ OLong  $ bits a b
    (ODict  a, ODict  b) -> return $ ODict  $ dict a b
    (OTree  a, OTree  b) -> case (a, b) of
      (Struct{ structName=na, fieldMap=ma }, Struct{ structName=nb, fieldMap=mb }) | na==nb ->
        xpure $ OTree $ a{ fieldMap = dict ma mb }
      _ -> throwError $ obj $
        [ obj "cannot operate on dissimilar struct types"
        , obj (structName a), obj (structName b)
        , OTree a, OTree b
        ]
    (OBytes a, OBytes b) -> return $ OBytes $ bytes a b
    _                    -> eval_Infix_op op a b

_dict_XOR :: (Object -> Object -> Object) -> T_dict -> T_dict -> T_dict
_dict_XOR f a b = M.difference (M.unionWith f a b) (M.intersectionWith f a b)

instance Bits (XPure Object) where
  a .&. b = _xpureBits2 AND (.&.) (M.intersectionWith (flip const)) (bytesBitArith (.&.)) a b <|>
    (a >>= \a -> b >>= \b -> eval_Infix_op ANDB a b)
  a .|. b = _xpureBits2 ORB (.|.) (M.unionWith (flip const))        (bytesBitArith (.|.)) a b <|>
    (a >>= \a -> b >>= \b -> eval_Infix_op ORB  a b)
  xor a b = _xpureBits2 XORB xor  (_dict_XOR (flip const))          (bytesBitArith  xor ) a b <|>
    (a >>= \a -> b >>= \b -> eval_Infix_op XORB a b)
  complement  = _xpureBits complement (B.map complement)
  shift   o i = o >>= \o -> case o of
    OList o -> xpure $ OList $ case compare i 0 of
      EQ -> o
      LT -> reverse $ drop i $ reverse o
      GT -> drop i o
    _ -> _xpureBits (flip shift i) (flip bytesShift (fromIntegral i)) (xpure o)
  rotate  o i = _xpureBits (flip shift i) (flip bytesRotate (fromIntegral i)) o
  bit       i = xpure $ if i<64 then OWord (bit i) else OBytes (bytesBit (fromIntegral i))
  testBit o i = _xpureApplyM "testBit" testbit o where
    testbit o = case o of
      OChar  o -> xpure $ testBit (ord o) i
      OInt   o -> xpure $ testBit o i
      OWord  o -> xpure $ testBit o i
      OLong  o -> xpure $ testBit o i
      OBytes o -> xpure $ bytesTestBit o (fromIntegral i)
      _        -> mzero
  bitSize = _xpureApplyM "bitSize" $ \o -> case o of
    OInt   o -> xpure $ bitSize o
    OWord  o -> xpure $ bitSize o
    OBytes o -> xpure $ fromIntegral $ bytesBitSize o
    _        -> mzero
  isSigned = _xpureApplyM "isSigned" $ \o -> case o of
    OChar  _ -> xpure False
    OInt   _ -> xpure True
    OWord  _ -> xpure False
    OLong  _ -> xpure True
    OBytes _ -> xpure False
    _        -> mzero
  popCount = _xpureApplyM "popCount" $ \o -> case o of
    OChar  o -> xpure $ popCount (ord o)
    OInt   o -> xpure $ popCount o
    OWord  o -> xpure $ popCount o
    OLong  o -> xpure $ popCount o
    OBytes o -> xpure $ fromIntegral $ bytesPopCount o
    _        -> mzero

_shiftOp :: (Int -> Int) -> Object -> Object -> XPure Object
_shiftOp neg a b = case b of
  OInt  b -> shift (xpure a) (neg b)
  OWord b -> shift (xpure a) (neg $ fromIntegral b)
  OLong b -> shift (xpure a) (neg $ fromIntegral b)
  _       -> mzero

-- | Evaluate the shift-left operator in the 'XPure' monad.
shiftLeft :: Object -> Object -> XPure Object
shiftLeft a b = _shiftOp id a b <|> eval_Infix_op SHL a b

-- | Evaluate the shift-right operator in the 'XPure' monad.
shiftRight :: Object -> Object -> XPure Object
shiftRight a b = _shiftOp negate a b <|> eval_Infix_op SHR a b

-- | Throw an error declaring that the two types cannot be used together because their types are
-- incompatible. Provide the a string describing the /what/ could not be done as a result of the
-- type mismatch, it will be placed in the message string:
-- > "could not <WHAT> the item <A> of type <A-TYPE> with the item <B> of type <B-TYPE>"
typeMismatchError :: String -> Object -> Object -> XPure ig
typeMismatchError msg a b = throwError $ obj $
  [ obj ("could not "++msg++" the item"), obj (prettyShow a), obj "of type", obj (typeOfObj a)
  , obj "with the item"                 , obj (prettyShow b), obj "of type", obj (typeOfObj b)
  ]

eval_ADD :: Object -> Object -> XPure Object
eval_ADD a b = (xpure a + xpure b) <|> typeMismatchError "add" a b

eval_SUB :: Object -> Object -> XPure Object
eval_SUB a b = (xpure a - xpure b) <|> typeMismatchError "subtract" a b

eval_MULT :: Object -> Object -> XPure Object
eval_MULT a b = (xpure a * xpure b) <|> typeMismatchError "multiply" a b

eval_DIV :: Object -> Object -> XPure Object
eval_DIV a b = do
  let { xa = xpure a; xb = xpure b; }
  (div xa xb <|> xa/xb) <|> typeMismatchError "divide" a b

eval_MOD :: Object -> Object -> XPure Object
eval_MOD a b = do
  let { xa = xpure a; xb = xpure b; }
  (mod xa xb) <|> typeMismatchError "modulus" a b

eval_POW :: Object -> Object -> XPure Object
eval_POW a b = do
  let { xa = xpure a; xb = xpure b; }
  (xa^xb <|> xa**xb) <|> typeMismatchError "exponent" a b

eval_ORB :: Object -> Object -> XPure Object
eval_ORB a b = do
  let { xa = xpure a; xb = xpure b; }
  (xa.|.xb) <|> typeMismatchError "bitwise-OR" a b

eval_ANDB :: Object -> Object -> XPure Object
eval_ANDB a b = do
  let { xa = xpure a; xb = xpure b; }
  (xa.&.xb) <|> typeMismatchError "bitwise-AND" a b

eval_XORB :: Object -> Object -> XPure Object
eval_XORB a b = do
  let { xa = xpure a; xb = xpure b; }
  (xor xa xb) <|> typeMismatchError "bitwise-XOR" a b

eval_EQUL :: Object -> Object -> XPure Object
eval_EQUL a b = return $ obj $ xpure a == xpure b

eval_NEQUL :: Object -> Object -> XPure Object
eval_NEQUL a b = return $ obj $ xpure a /= xpure b

eval_GTN :: Object -> Object -> XPure Object
eval_GTN a b = return $ obj $ xpure a > xpure b

eval_LTN :: Object -> Object -> XPure Object
eval_LTN a b = return $ obj $ xpure a < xpure b

eval_GTEQ :: Object -> Object -> XPure Object
eval_GTEQ a b = return $ obj $ xpure a >= xpure b

eval_LTEQ :: Object -> Object -> XPure Object
eval_LTEQ a b = return $ obj $ xpure a <= xpure b

eval_SHR :: Object -> Object -> XPure Object
eval_SHR a b = shiftRight a b

eval_SHL :: Object -> Object -> XPure Object
eval_SHL a b = shiftLeft a b

eval_NEG :: Object -> XPure Object
eval_NEG = _evalNumOp1 negate

eval_INVB :: Object -> XPure Object
eval_INVB = complement . xpure

eval_NOT :: Object -> XPure Object
eval_NOT = fmap (obj . not) . objToBool

objToBool :: Object -> XPure Bool
objToBool o = case o of
  OHaskell (HaskellData d ifc) -> case objNullTest ifc of
    Nothing   -> throwError $ obj [obj "cannot be used as a boolean value:", o]
    Just test -> return (test d)
  o -> return $ not $ testNull o

-- | Traverse the entire object, returning a list of all 'OString' elements.
extractStringElems :: Object -> [UStr]
extractStringElems o = case o of
  OString  o   -> [o]
  OList    o   -> concatMap extractStringElems o
  _            -> []

requireAllStringArgs :: String -> [Object] -> Exec [UStr]
requireAllStringArgs msg ox = case mapM check (zip (iterate (+(1::Integer)) 0) ox) of
  OK      obj -> return obj
  Backtrack   -> execThrow $ obj [obj msg]
  PFail   err -> execThrow err
  where
    check (i, o) = case o of
      OString o -> return o
      _         -> throwError $
        mkExecError
        { execReturnValue = Just $ obj [obj msg, obj "param number:", obj i] }

-- | Given an object, if it is a string return the string characters. If it not a string,
-- depth-recurse into it and extract strings, or if there is an object into which recursion is not
-- possible, pretty-print the object and return the pretty-printed string. The first integer
-- parameter is a depth limit, if recursion into the object exceeds this limit, recursion no longer
-- steps into these objects, the strings returned are the pretty-printed representation of the
-- objects. A pair of 'Data.Either.Either's are returned, references are 'Data.Either.Left',
-- 'Prelude.String's are 'Data.Either.Right'. References are accompanied with their depth so you can
-- choose whether or not you want to dereference or pretty-print them.
getStringsToDepth :: Int -> Object -> [Either (Int, Reference) String]
getStringsToDepth maxDepth o = loop (0::Int) maxDepth o where
  loop depth remDep o = case o of
    OString   o -> return (Right (uchars o))
    OList    ox -> recurse o ox
    o           -> return (Right (prettyShow o))
    where
      recurse o ox =
        if remDep==0
          then  return (Right (prettyShow o))
          else  ox >>= loop (depth+1) (if remDep>0 then remDep-1 else remDep)

----------------------------------------------------------------------------------------------------

-- | Calls 'getStringsToDepth' and dereferences all 'Data.Either.Left' values below a depth limit,
-- this depth limit is specified by the first argument to this function. The second and third
-- argument to this function are passed directly to 'getStringsToDepth'. Pass a handler to handle
-- references that are undefined.
derefStringsToDepth :: (Reference -> Object -> Exec [String]) -> Int -> Int -> Object -> Exec [String]
derefStringsToDepth handler maxDeref maxDepth o =
  fmap concat (mapM deref (getStringsToDepth maxDepth o)) where
    deref o = case o of
      Right    o    -> return [o]
      Left (i, ref) ->
        if i>=maxDeref
          then  return [prettyShow ref]
          else  do
            let newMax = if maxDepth>=0 then (if i>=maxDepth then 0 else maxDepth-i) else (0-1)
                recurse = fmap concat . mapM (derefStringsToDepth handler (maxDeref-i) newMax)
            catchReturn (\ _ -> return Nothing) (fmap (fmap snd) $ referenceLookup ref) >>=
              recurse . maybe [] (:[])

-- | Returns a list of all string objects that can be found from within the given list of objects.
-- This function might fail if objects exist that cannot resonably contain strings. If you want to
-- pretty-print non-string objects, try using 'getStringsToDepth'.
recurseGetAllStrings :: Object -> Exec [UStr]
recurseGetAllStrings o = catch (loop [] o) where
  loop ix o = case o of
    OString  o   -> return [o]
    OList    o   -> next OInt (zip [0..] o)
    o            -> throwError $ OList $
      [ obj "object at index", OList ix
      , obj "cannot be evaluated to a string", o
      ]
    where
      next fn = fmap concat . mapM (\ (i, o) -> loop (fn i : ix) o)
  catch ox = case ox of
    FlowErr  err -> execThrow err
    FlowOK    ox -> return ox
    FlowReturn _ -> undefined

----------------------------------------------------------------------------------------------------

data UpdateOp
  = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UPOW | UORB | UANDB | UXORB | USHL | USHR
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)
instance NFData UpdateOp where { rnf a = seq a () }

_updateToInfixOp :: UpdateOp -> InfixOp
_updateToInfixOp = (arr!) where
  arr :: Array UpdateOp InfixOp
  arr = array (UADD, maxBound) $
    [ (UADD  , ADD )
    , (USUB  , SUB )
    , (UMULT , MULT)
    , (UDIV  , DIV )
    , (UMOD  , MOD )
    , (UPOW  , POW )
    , (UORB  , ORB )
    , (UANDB , ANDB)
    , (UXORB , XORB)
    , (USHL  , SHL )
    , (USHR  , SHR )
    ]

allUpdateOpStrs :: String
allUpdateOpStrs = " = += -= *= /= %= **= |= &= ^= <<= >>= "

instance UStrType UpdateOp where
  toUStr a = ustr $ case a of
    UCONST -> "="
    UADD   -> "+="
    USUB   -> "-="
    UMULT  -> "*="
    UDIV   -> "/="
    UMOD   -> "%="
    UPOW   -> "**="
    UORB   -> "|="
    UANDB  -> "&="
    UXORB  -> "^="
    USHL   -> "<<="
    USHR   -> ">>="
  maybeFromUStr str = case uchars str of
    "="   -> Just UCONST
    "+="  -> Just UADD  
    "-="  -> Just USUB  
    "*="  -> Just UMULT 
    "/="  -> Just UDIV  
    "%="  -> Just UMOD  
    "**=" -> Just UPOW
    "|="  -> Just UORB  
    "&="  -> Just UANDB 
    "^="  -> Just UXORB 
    "<<=" -> Just USHL  
    ">>=" -> Just USHR  
    _     -> Nothing
  fromUStr str =
    maybe (error (show str++" is not an assignment/update operator")) id (maybeFromUStr str)

instance PPrintable UpdateOp where { pPrint op = pString (' ':uchars op++" ") }

-- binary 0x8D 0x9D UpdateOp-->InfixOp
instance B.Binary UpdateOp MTab where
  put o = B.putWord8 $ case o of
    UCONST -> 0x8D
    UADD   -> 0x93
    USUB   -> 0x94
    UMULT  -> 0x95
    UDIV   -> 0x96
    UMOD   -> 0x97
    UPOW   -> 0x98
    UORB   -> 0x99
    UANDB  -> 0x9A
    UXORB  -> 0x9B
    USHL   -> 0x9C
    USHR   -> 0x9D
  get = B.word8PrefixTable <|> fail "expecting UpdateOp"
instance B.HasPrefixTable UpdateOp B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "UpdateOp" 0x8D 0x9D $ let {r=return;z=mzero} in
    [ r UCONST -- 0x8D
    , z, z, z, z, z -- 0x8E,0x8F,0x90,0x91,0x92
    , r UADD, r USUB, r UMULT, r UDIV, r UMOD, r UPOW, r UORB -- 0x93,0x94,0x95,0x96,0x97,0x98,0x99
    , r UANDB, r UXORB, r USHL, r USHR -- 0x9A,0x9B,0x9C,0x9D
    ]

instance HasRandGen UpdateOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::UpdateOp)))
  defaultO = randO

instance ToDaoStructClass UpdateOp where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass UpdateOp where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass UpdateOp where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass UpdateOp where
  haskellDataInterface = interface UCONST $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data RefPfxOp = REF | DEREF deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance NFData RefPfxOp where { rnf a = seq a () }

instance UStrType RefPfxOp where
  toUStr op = ustr $ case op of
    REF         -> "$"
    DEREF       -> "@"
  maybeFromUStr str = case uchars str of
    "$" -> Just REF
    "@" -> Just DEREF
    _   -> Nothing
  fromUStr str = maybe (error (show str++" is not a prefix opretor")) id (maybeFromUStr str)

instance PPrintable RefPfxOp where { pPrint = pUStr . toUStr }

instance HasRandGen RefPfxOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::RefPfxOp)))
  defaultO = randO

instance ToDaoStructClass RefPfxOp where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass RefPfxOp where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass RefPfxOp where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass RefPfxOp where
  haskellDataInterface = interface REF $ do
    autoDefEquality >> autoDefOrdering
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Unary operators.
data ArithPfxOp = INVB | NOT | NEGTIV | POSTIV deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance NFData ArithPfxOp where { rnf a = seq a () }

instance UStrType ArithPfxOp where
  toUStr op = ustr $ case op of
    INVB   -> "~"
    NOT    -> "!"
    NEGTIV -> "-"
    POSTIV -> "+"
  maybeFromUStr str = case uchars str of
    "~" -> Just INVB
    "!" -> Just NOT
    "-" -> Just NEGTIV
    "+" -> Just POSTIV
    _   -> Nothing
  fromUStr str = maybe (error (show str++" is not a prefix opretor")) id (maybeFromUStr str)

instance PPrintable ArithPfxOp where { pPrint = pUStr . toUStr }

-- binary 0x8E 0x9B
instance B.Binary ArithPfxOp MTab where
  put o = B.putWord8 $ case o of { INVB -> 0x9B; NOT -> 0x8E; NEGTIV -> 0x94; POSTIV -> 0x93 }
  get = B.word8PrefixTable <|> fail "expecting ArithPfxOp"

instance B.HasPrefixTable ArithPfxOp B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ArithPfxOp" 0x8E 0x9F $ let {r=return;z=mzero} in
    [ r NOT -- 0x8E
    , z, z, z, z -- 0x8F,0x90,0x91,0x92
    , r POSTIV, r NEGTIV -- 0x93,0x94
    , z, z, z, z, z, z -- 0x95,0x96,0x97,0x98,0x99,0x9A
    , r INVB -- 0x9B
    ]

instance HasRandGen ArithPfxOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::ArithPfxOp)))
  defaultO = randO

instance ToDaoStructClass ArithPfxOp where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass ArithPfxOp where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass ArithPfxOp where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ArithPfxOp where
  haskellDataInterface = interface POSTIV $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

allPrefixOpChars :: String
allPrefixOpChars = "$@~!-+"

allPrefixOpStrs :: String
allPrefixOpStrs = " $ @ ~ - + ! "

----------------------------------------------------------------------------------------------------

-- | Binary operators.
data InfixOp
  = ADD   | SUB   | MULT
  | DIV   | MOD   | POW
  | ORB   | ANDB  | XORB
  | SHL   | SHR
  | OR    | AND
  | EQUL  | NEQUL      
  | GTN   | LTN
  | GTEQ  | LTEQ
  | ARROW
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance UStrType InfixOp where
  toUStr a = ustr $ case a of
    { ADD  -> "+" ; SUB  -> "-" ; MULT  -> "*"
    ; DIV  -> "/" ; MOD  -> "%" ; POW   -> "**"
    ; ORB  -> "|" ; ANDB -> "&" ; XORB  -> "^"
    ; SHL  -> "<<"; SHR  -> ">>"
    ; OR   -> "||"; AND  -> "&&"
    ; EQUL -> "=="; NEQUL-> "!="
    ; LTN  -> "<" ; GTN  -> ">"
    ; LTEQ -> "<="; GTEQ -> ">="
    ; ARROW -> "->";
    }
  maybeFromUStr str = case uchars str of
    { "+"  -> Just ADD  ; "-"  -> Just SUB  ; "*"  -> Just MULT 
    ; "/"  -> Just DIV  ; "%"  -> Just MOD  ; "**" -> Just POW  
    ; "|"  -> Just ORB  ; "&"  -> Just ANDB ; "^"  -> Just XORB 
    ; "<<" -> Just SHL  ; ">>" -> Just SHR
    ; "||" -> Just OR   ; "&&" -> Just AND
    ; "==" -> Just EQUL ; "!=" -> Just NEQUL
    ; "<"  -> Just LTN  ; ">"  -> Just GTN
    ; "<=" -> Just LTEQ ; ">=" -> Just GTEQ 
    ; "->" -> Just ARROW;
    ; _    -> Nothing
    }
  fromUStr str = maybe (error (show str++" is not an infix operator")) id (maybeFromUStr str)

instance NFData InfixOp  where { rnf a = seq a () }

instance PPrintable InfixOp  where { pPrint = pUStr . toUStr }

infixOpCommutativity :: InfixOp -> Bool
infixOpCommutativity = (arr !) where
  arr :: Array InfixOp Bool
  arr = array (minBound, maxBound) $
    [ (ADD, True), (SUB, False), (MULT, True), (DIV, False), (MOD, False), (POW, False)
    , (ORB, True), (ANDB, True), (XORB, True), (SHL, False), (SHR, False), (OR, False), (AND, False)
    , (EQUL, True), (NEQUL, True), (LTN, False), (GTN, False), (LTEQ, False), (GTEQ, False)
    , (ARROW, False)
    ]

-- binary 0x8D 0xA0
instance B.Binary InfixOp MTab where
  put o = B.putWord8 $ case o of
    { EQUL -> 0x8D; NEQUL -> 0x8E; GTN  -> 0x8F; LTN   -> 0x90; GTEQ -> 0x91; LTEQ -> 0x92
    ; ADD  -> 0x93; SUB   -> 0x94; MULT -> 0x95; DIV   -> 0x96
    ; MOD  -> 0x97; POW   -> 0x98; ORB  -> 0x99; ANDB  -> 0x9A
    ; XORB -> 0x9B; SHL   -> 0x9C; SHR  -> 0x9D; ARROW -> 0x9E
    ; OR   -> 0x9F; AND   -> 0xA0 } 
  get = B.word8PrefixTable <|> fail "expecting InfixOp"

-- The byte prefixes overlap with the update operators of similar function to
-- the operators, except for the comparison opeators (EQUL, NEQUL, GTN, LTN,
-- GTEQ, LTEQ) which overlap with the prefix operators (INVB, NOT, NEGTIV, POSTIV, REF, DEREF)
instance B.HasPrefixTable InfixOp B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "InfixOp" 0x8D 0xA0 $ let {r=return} in
    [ r EQUL , r NEQUL, r GTN , r LTN, r GTEQ , r LTEQ -- 0x8D,0x8E,0x8F,0x90,0x91,0x92
    , r ADD  , r SUB  , r MULT, r DIV, r MOD  , r POW  , r ORB -- 0x93,0x94,0x95,0x96,0x97,0x98,0x99
    , r ANDB , r XORB , r SHL , r SHR, r ARROW, r OR   , r AND -- 0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,0xA0
    ]

instance HasRandGen InfixOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::InfixOp)))
  defaultO = randO

instance ToDaoStructClass InfixOp where { toDaoStruct=putNullaryUsingShow; }

instance FromDaoStructClass InfixOp where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass InfixOp where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass InfixOp where
  haskellDataInterface = interface ADD $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    autoDefToStruct >> autoDefFromStruct

allInfixOpChars :: String
allInfixOpChars = "+-*/%<>^&|.?:"

allInfixOpStrs :: String
allInfixOpStrs = " + - * / % ** -> . || && == != | & ^ << >> < > <= >= . -> <- ? : :: "

----------------------------------------------------------------------------------------------------

data TopLevelEventType
  = BeginExprType | EndExprType | ExitExprType
  deriving (Eq, Ord, Typeable, Enum)

instance Show TopLevelEventType where
  show t = case t of
    BeginExprType -> "BEGIN"
    EndExprType   -> "END"
    ExitExprType  -> "EXIT"

instance Read TopLevelEventType where
  readsPrec _ str = map (\t -> (t, "")) $ case str of
    "BEGIN" -> [BeginExprType]
    "END"   -> [EndExprType]
    "EXIT"  -> [ExitExprType]
    _       -> []

instance NFData TopLevelEventType where { rnf a = seq a () }

instance HasRandGen TopLevelEventType where
  randO = fmap toEnum (nextInt 3)
  defaultO = randO

instance ToDaoStructClass TopLevelEventType where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass TopLevelEventType where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass TopLevelEventType where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass TopLevelEventType where
  haskellDataInterface = interface BeginExprType $ do
    autoDefEquality >> autoDefOrdering
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

evalArithPrefixOp :: ArithPfxOp -> Object -> XPure Object
evalArithPrefixOp = (_arithPrefixOps!)

_arithPrefixOps :: Array ArithPfxOp (Object -> XPure Object)
_arithPrefixOps = array (minBound, maxBound) $ defaults ++
  [ o NEGTIV eval_NEG
  , o POSTIV return
  , o INVB   eval_INVB
  , o NOT    eval_NOT
  ]
  where
    o = (,)
    defaults = flip map [minBound..maxBound] $ \op ->
      (op, \_ -> error $ "no builtin function for prefix "++show op++" operator")

evalInfixOp :: InfixOp -> Object -> Object -> XPure Object
evalInfixOp op a b = msum $ f a ++ f b ++ [(_infixOps!op) a b] where
  f = maybe [] return . (fromObj >=> getOp)
  getOp (HaskellData a ifc) = (\f -> f op a b) <$> (objInfixOpTable ifc >>= (! op))

_infixOps :: Array InfixOp (Object -> Object -> XPure Object)
_infixOps = array (minBound, maxBound) $ defaults ++
  [ o ADD   eval_ADD
  , o SUB   eval_SUB
  , o MULT  eval_MULT
  , o DIV   eval_DIV
  , o MOD   eval_MOD
  , o POW   eval_POW
  , o SHL   eval_SHL
  , o SHR   eval_SHR
  , o ORB   eval_ORB
  , o ANDB  eval_ANDB
  , o XORB  eval_XORB
  , o OR    (error (e "logical-OR" )) -- These probably wont be evaluated. Locgical and/or is a
  , o AND   (error (e "logical-AND")) -- special case to be evaluated in 'evalObjectExprWithLoc'.
  , o EQUL  eval_EQUL
  , o NEQUL eval_NEQUL
  , o GTN   eval_GTN
  , o LTN   eval_LTN
  , o GTEQ  eval_GTEQ
  , o LTEQ  eval_LTEQ
  , o ARROW (error (e "ARROW"))
  ]
  where
    o = (,)
    defaults = flip map [minBound..maxBound] $ \op ->
      (op, \_ _ -> error $ "no builtin function for infix "++show op++" operator")
    e msg = msg ++
      " operator should have been evaluated within the 'execute' function."

evalUpdateOp :: Maybe Reference -> UpdateOp -> Object -> Maybe Object -> Exec (Maybe Object)
evalUpdateOp qref op newObj oldObj = case op of
  UCONST -> return $ Just newObj
  op     -> case oldObj of
    Nothing     -> execThrow $ obj $ concat $
      [ [obj "cannot update undefined reference"], maybe [] (return . obj) qref
      , [obj "with operator", obj (prettyShow op)]
      ]
    Just oldObj -> Just <$> execute (evalInfixOp (_updateToInfixOp op) oldObj newObj)

_updatingOps :: Array UpdateOp (Object -> Object -> XPure Object)
_updatingOps = let o = (,) in array (minBound, maxBound) $ defaults ++
  [ o UCONST (\_ b -> return b)
  , o UADD   eval_ADD
  , o USUB   eval_SUB
  , o UMULT  eval_MULT
  , o UDIV   eval_DIV
  , o UMOD   eval_MOD
  , o UORB   eval_ORB
  , o UANDB  eval_ANDB
  , o UXORB  eval_XORB
  , o USHL   eval_SHL
  , o USHR   eval_SHR
  ]
  where
    defaults = flip map [minBound..maxBound] $ \op ->
      (op, \_ _ -> error $ "no builtin function for update operator "++show op)

----------------------------------------------------------------------------------------------------

_strObjConcat :: [Object] -> String
_strObjConcat ox = ox >>= \o -> maybe [toUStr $ prettyShow o] return (fromObj o) >>= uchars

_builtin_print :: (String -> IO ()) -> DaoFunc
_builtin_print print = DaoFunc nil True $ \ox -> liftIO (print $ _strObjConcat ox) >> return Nothing

builtin_print :: DaoFunc
builtin_print   = _builtin_print putStr

builtin_println :: DaoFunc
builtin_println = _builtin_print putStrLn

-- join string elements of a container, pretty prints non-strings and joins those as well.
builtin_join :: DaoFunc
builtin_join = DaoFunc (ustr "join") True $ \ox -> return $ Just $ obj $ case ox of
  OString j : ox -> (>>=uchars) $
    intersperse j $ snd (objConcat ox) >>= \o ->
      [maybe (ustr $ prettyShow o) id (fromObj o :: Maybe UStr)]
  ox -> _strObjConcat ox

builtin_str :: DaoFunc
builtin_str = DaoFunc (ustr "str") True $ return . Just . obj . _strObjConcat

builtin_quote :: DaoFunc
builtin_quote = DaoFunc (ustr "quote") True $ return . Just . obj . show . _strObjConcat

builtin_concat :: DaoFunc
builtin_concat = DaoFunc (ustr "concat") True $ return . Just . obj .
  fix (\loop ox -> ox >>= \o -> maybe [o] loop (fromObj o))

builtin_concat1 :: DaoFunc
builtin_concat1 = DaoFunc (ustr "concat1") True $ return . Just . obj . snd . objConcat

_castNumerical :: String -> (Object -> Exec Object) -> DaoFunc
_castNumerical name f = let n = ustr name :: Name in DaoFunc n True $ \ox -> do
  case ox of
    [o] -> (Just <$> f o) <|> execThrow (obj [obj n, obj "could not cast from value", o])
    _   -> execThrow $ obj [obj n, obj "function should take only one parameter argument", OList ox]

builtin_int :: DaoFunc
builtin_int = _castNumerical "int" $ fmap (OInt . fromIntegral) . execute . asInteger

builtin_long :: DaoFunc
builtin_long = _castNumerical "long" $ fmap obj . execute . asInteger

builtin_ratio :: DaoFunc
builtin_ratio = _castNumerical "ratio" $ fmap obj . execute . asRational

builtin_float :: DaoFunc
builtin_float = _castNumerical "float" $ fmap (OFloat . fromRational) . execute . asRational

builtin_complex :: DaoFunc
builtin_complex = _castNumerical "complex" $ fmap OComplex . execute . asComplex

builtin_imag :: DaoFunc
builtin_imag = _castNumerical "imag" $ fmap (OFloat . imagPart) . execute . asComplex

builtin_phase :: DaoFunc
builtin_phase = _castNumerical "phase" $ fmap (OFloat . phase) . execute . asComplex

builtin_conj :: DaoFunc
builtin_conj = _castNumerical "conj" $ fmap (OComplex . conjugate) . execute . asComplex

builtin_abs :: DaoFunc
builtin_abs = _castNumerical "abs" $ execute . asPositive

builtin_time :: DaoFunc
builtin_time = _castNumerical "time" $ \o -> case o of
  ORelTime _ -> return o
  o          -> (ORelTime . fromRational) <$> execute (asRational o)

_funcWithoutParams :: String -> Exec (Maybe Object) -> DaoFunc
_funcWithoutParams name f = let n = ustr name in DaoFunc n False $ \ox -> case ox of
  [] -> f
  ox -> execThrow $ obj $
    [obj n, obj "should take no parameter arguments, but received: ", obj ox]

builtin_now :: DaoFunc
builtin_now = _funcWithoutParams "now" $ (Just . obj) <$> liftIO getCurrentTime

builtin_check_if_defined :: DaoFunc
builtin_check_if_defined = DaoFunc (ustr "defined") False $ \args -> do
  fmap (Just . obj . and) $ forM args $ \arg -> case arg of
    ORef o -> fmap (maybe False (const True)) (fmap (fmap snd) $ referenceLookup o)
    _      -> return True

builtin_delete :: DaoFunc
builtin_delete = DaoFunc (ustr "delete") False $ \args -> do
  forM_ args $ \arg -> case arg of
    ORef o -> void $ referenceUpdate o (const $ return Nothing)
    _      -> return ()
  return (Just ONull)

builtin_typeof :: DaoFunc
builtin_typeof = DaoFunc (ustr "typeof") True $ \ox -> return $ case ox of
  []  -> Nothing
  [o] -> Just $ OType $ coreType $ typeOfObj o
  ox  -> Just $ OList $ map (OType . coreType . typeOfObj) ox

builtin_sizeof :: DaoFunc
builtin_sizeof = DaoFunc (ustr "sizeof") True $ \ox -> case ox of
  [o] -> Just <$> getSizeOf o
  _   -> execThrow $ obj $
    [obj "sizeof() function must take exactly one parameter argument, instead got", obj ox]

----------------------------------------------------------------------------------------------------

data RefSuffixExpr
  = NullRefExpr
  | DotRefExpr    Name        RefSuffixExpr Location
  | SubscriptExpr ObjListExpr RefSuffixExpr
  | FuncCallExpr  ObjListExpr RefSuffixExpr
  deriving (Eq, Ord, Typeable, Show)

instance NFData RefSuffixExpr where
  rnf  NullRefExpr          = ()
  rnf (DotRefExpr    a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (SubscriptExpr a b  ) = deepseq a $! deepseq b ()
  rnf (FuncCallExpr  a b  ) = deepseq a $! deepseq b ()

instance HasNullValue RefSuffixExpr where
  nullValue = NullRefExpr
  testNull NullRefExpr = True
  testNull _           = False

instance HasLocation RefSuffixExpr where
  getLocation o = case o of
    NullRefExpr           -> LocationUnknown
    DotRefExpr    _ _ loc -> loc
    SubscriptExpr a _     -> getLocation a
    FuncCallExpr  a _     -> getLocation a
  setLocation o loc = case o of
    NullRefExpr           -> NullRefExpr
    DotRefExpr    a b _   -> DotRefExpr    a b loc
    SubscriptExpr a b     -> SubscriptExpr (setLocation a loc) b
    FuncCallExpr  a b     -> FuncCallExpr  (setLocation a loc) b
  delLocation o     = case o of
    NullRefExpr           -> NullRefExpr
    DotRefExpr    a b _   -> DotRefExpr    a (delLocation b) LocationUnknown
    SubscriptExpr a b     -> SubscriptExpr a (delLocation b)
    FuncCallExpr  a b     -> FuncCallExpr  a (delLocation b)

-- binary 0x42 0x45 RefSuffixExpr-->RefSuffix
instance B.Binary RefSuffixExpr MTab where
  put o = case o of
    NullRefExpr         -> B.putWord8   0x42
    DotRefExpr    a b c -> B.prefixByte 0x43 $ B.put a >> B.put b >> B.put c
    SubscriptExpr a b   -> B.prefixByte 0x44 $ B.put a >> B.put b
    FuncCallExpr  a b   -> B.prefixByte 0x45 $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting RefSuffixExpr"

instance B.HasPrefixTable RefSuffixExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RefSuffixExpr" 0x42 0x45 $
    [ return NullRefExpr
    , return DotRefExpr    <*> B.get <*> B.get <*> B.get
    , return SubscriptExpr <*> B.get <*> B.get
    , return FuncCallExpr  <*> B.get <*> B.get
    ]

instance Executable RefSuffixExpr RefSuffix where
  execute o = let execargs = fmap (fmap $ ORef . snd) . execute in case o of
    NullRefExpr              -> return NullRef
    DotRefExpr    name ref _ -> DotRef name <$> execute ref
    SubscriptExpr args ref   -> return Subscript <*> execargs args <*> execute ref
    FuncCallExpr  args ref   -> return FuncCall  <*> execargs args <*> execute ref

----------------------------------------------------------------------------------------------------

-- | To evaluate an 'Object' value against a type expression, you can store the
-- 'Object' into a 'TyChkExpr' and 'execute' it. This instance of
-- 'execute' evaluates a type checking monad computing over the 'tyChkExpr' in
-- the 'TyChkExpr'. If the type check determines the 'Object' value does not match, this
-- function backtracks. If the type check is successful, the most general type value for the object
-- if that type value is less-general or as-general as the 'TyChkExpr' provided.
instance Executable (TyChkExpr Object) Object where
  execute tc = case tc of
    NotTypeChecked _          -> return OTrue -- TODO: this needs to return the 'AnyType', not 'OTrue'.
    TypeChecked    _ _ _      -> return OTrue -- TODO: evaluate the actual type checking algorithm here
    DisableCheck   _ _ rslt _ -> return rslt

-- | Called by 'callFunction' to match the list of 'Object's passed as arguments to the function.
-- Returns two 'T_dict's: the first is the 'T_dict' to be passed to 'execFuncPushStack', the second
-- is the dictionary of local variables passed by reference. Backtracks if any types do not match,
-- or if there are an incorrect number of parameters. Backtracking is important because of function
-- overloading.
matchFuncParams :: ParamListExpr -> [Object] -> Exec T_dict
matchFuncParams (ParamListExpr params _) ox =
  loop (0::Int) M.empty (tyChkItem params) ox where
    loop i dict params ox = case ox of
      [] | null params -> return dict
      [] -> mzero -- not enough parameters passed to function
      o:ox -> case params of
        [] -> mzero -- too many parameters passed to function
        ParamExpr passByRef tychk _ : params -> do
          let name = tyChkItem tychk
          execute $ fmap (const o) tychk -- execute (TyChkExpr Object)
          o <- if passByRef then (case o of { ORef _ -> return o; _ -> mzero }) else derefObject o
          loop (i+1) (M.insert name o dict) params ox

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code found in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [ScriptExpr] -> Exec ()
execGuardBlock block = void $
  execFuncPushStack M.empty (mapM_ execute block >> return Nothing) >> return ()

-- | Takes two parameters: first is an error message parameter, the second is the 'Object' to be
-- called. The 'Object' to be called should be an 'OHaskell' constructed value containing a
-- 'HaskellData' where the 'interface' has defined 'defCallable'. If so, the 'CallableCode' objects
-- returned by 'objCallable' will be returned by this function. If not, 
objToCallable :: Object -> Exec [CallableCode]
objToCallable o = case fromObj o >>= \ (HaskellData o ifc) -> fmap ($ o) (objCallable ifc) of
  Nothing -> mzero
  Just  f -> f

-- | 'CallableCode' objects are usually stored in lists because of function overloading: e.g. a
-- function with a single name but is defined with multiple parameter lists would have several
-- 'CallableCode' objects associated with that function mame. This function tries to perform a
-- function call with a list of parameters. The parameters are matched to each 'CallableCode'
-- object's 'argsPattern', the first 'argsPattern' that matches without backtracking will evaluate
-- the function body.
callCallables :: [CallableCode] -> [Object] -> Exec (Maybe Object)
callCallables funcs params = 
  msum $ flip map funcs $ \f -> matchFuncParams (argsPattern f) params >>=
    flip execFuncPushStack (execute $ codeSubroutine f)

-- | If the given object provides a 'defCallable' callback, the object can be called with parameters
-- as if it were a function.
callObject :: Maybe Reference -> Object -> [Object] -> Exec (Maybe Object)
callObject qref o params = case o of
  OHaskell (HaskellData o ifc) -> case objCallable ifc of
    Just getFuncs -> getFuncs o >>= \funcs -> callCallables funcs params
    Nothing -> case fromDynamic o of
      Just func -> executeDaoFunc qref func params
      Nothing -> err
  _ -> err
  where
    err = execThrow $ obj $ concat $
      [ maybe [] (return . ORef) qref
      , [obj "not a callable object", o]
      , [obj "called with parameters:", obj params]
      , [obj "value of object at reference is:", o]
      ]

-- | Evaluate to 'procErr' if the given 'Predicate' is 'Backtrack' or 'PFail'. You must pass a
-- 'Prelude.String' as the message to be used when the given 'Predicate' is 'Backtrack'. You can also
-- pass a list of 'Object's that you are checking, these objects will be included in the
-- 'procErr' value.
--     This function should be used for cases when you have converted 'Object' to a
-- Haskell value, because 'Backtrack' values indicate type exceptions, and 'PFail' values indicate a
-- value error (e.g. out of bounds, or some kind of assert exception), and the messages passed to
-- 'procErr' will indicate this.
checkPredicate :: String -> [Object] -> Exec a -> Exec a
checkPredicate altmsg tried f = do
  pval <- catchPredicate f
  let err = fail (altmsg++" evaulated to void expression")
  case pval of
    OK    a                     -> return a
    Backtrack                   -> err
    PFail (ExecReturn Nothing)  -> err
    PFail  err                  -> throwError $
      err{  execReturnValue = Just $ case execReturnValue err of
              Just (OList ox) -> obj $ tried ++ ox
              Just        o   -> obj $ tried ++ [o]
              Nothing         -> obj tried
         }

-- | 'evalObjectExprExpr' can return 'Data.Maybe.Nothing', and usually this happens when something has
-- failed (e.g. reference lookups), but it is not always an error (e.g. a void list of argument to
-- functions). If you want 'Data.Maybe.Nothing' to cause an error, evaluate your
-- @'Exec' ('Data.Maybe.Maybe' 'Object')@ as a parameter to this function.
checkVoid :: Location -> String -> Maybe a -> Exec a
checkVoid loc msg fn = case fn of
  Nothing -> execThrow $ obj $ obj loc : [obj msg, obj "evaluates to a void"]
  Just  a -> return a

----------------------------------------------------------------------------------------------------

-- | Anything that follows a reference, which could be square-bracketed indecies, function
-- parameters, or a dot and another reference. This is actually only a partial reference. The
-- 'Reference' is the full reference. The item selected by the 'Reference' is then further inspected
-- using a 'RefSuffix'; a 'RefSuffix' may be null, but a 'Reference' is never null.
data AST_RefSuffix
  = AST_RefNull
  | AST_DotRef    (Com ()) Name AST_RefSuffix Location
  | AST_Subscript AST_ObjList   AST_RefSuffix
  | AST_FuncCall  AST_ObjList   AST_RefSuffix
  deriving (Eq, Ord, Typeable, Show)

instance HasNullValue AST_RefSuffix where
  nullValue = AST_RefNull
  testNull AST_RefNull = True
  testNull _ = False

instance HasLocation AST_RefSuffix where
  getLocation o     = case o of
    AST_RefNull             -> LocationUnknown
    AST_DotRef    _ _ _ loc -> loc
    AST_Subscript a _       -> getLocation a
    AST_FuncCall  a _       -> getLocation a
  setLocation o loc = case o of
    AST_RefNull           -> AST_RefNull
    AST_DotRef    a b c _ -> AST_DotRef  a b c loc
    AST_Subscript a b     -> AST_Subscript (setLocation a loc) b
    AST_FuncCall  a b     -> AST_FuncCall  (setLocation a loc) b
  delLocation o     = case o of
    AST_RefNull           -> AST_RefNull
    AST_DotRef    a b c _ -> AST_DotRef  a b (delLocation c) LocationUnknown
    AST_Subscript a b     -> AST_Subscript (delLocation a) (delLocation b)
    AST_FuncCall  a b     -> AST_FuncCall  (delLocation a) (delLocation b)

instance PPrintable AST_RefSuffix where
  pPrint = pWrapIndent . loop where
    loop ref = case ref of
      AST_RefNull                  -> []
      AST_DotRef    dot name ref _ -> pPrintComWith (\ () -> pString ".") dot : pUStr (toUStr name) : loop ref
      AST_Subscript args     ref   -> pArgs "[]" args ++ loop ref
      AST_FuncCall  args     ref   -> pArgs "()" args ++ loop ref
      where
        pArgs str args = case str of
          (open:close:_) -> [pString [open], pPrint args, pString [close]]
          _              -> error "INTERNAL: bad instance definition of PPrintable AST_RefSuffix"

instance NFData AST_RefSuffix where
  rnf  AST_RefNull            = ()
  rnf (AST_DotRef    a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Subscript a b    ) = deepseq a $! deepseq b ()
  rnf (AST_FuncCall  a b    ) = deepseq a $! deepseq b ()

instance ToDaoStructClass AST_RefSuffix where
  toDaoStruct = ask >>= \o -> case o of
    AST_RefNull                 -> makeNullary "Null"
    AST_DotRef dot name ref loc -> do
      renameConstructor "DotRef"
      predicate (fromData (comToDaoStruct $ renameConstructor "Com") dot) >>= defObjField "dot" . obj
      "name" .= name >> "tail" .= ref >> putLocation loc
    AST_Subscript  args ref -> renameConstructor "Subscript" >>
      "args" .= args >> "tail" .= ref >> return ()
    AST_FuncCall   args ref -> renameConstructor "FuncCall" >>
      "args" .= args >> "tail" .= ref >> return ()

instance FromDaoStructClass AST_RefSuffix where
  fromDaoStruct = msum $
    [ constructor "Null"   >> return AST_RefNull
    , constructor "DotRef" >> return AST_DotRef <*> getDot <*> req "head" <*> req "tail" <*> location
    , constructor "Subscript" >> return AST_Subscript <*> req "args" <*> req "tail"
    , constructor "FuncCall"  >> return AST_FuncCall  <*> req "args" <*> req "tail"
    ] where { getDot = structCurrentField (fromUStr $ ustr "dot") $ comFromDaoStruct (return ()) }

instance HasRandGen AST_RefSuffix where
  randO = _randTrace "AST_RefSuffix" $ recurse $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_RefNull
    , scramble $ return AST_DotRef    <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_Subscript <*> randO <*> randO
    , scramble $ return AST_FuncCall  <*> randO <*> randO
    ]
  defaultO = _randTrace "D.AST_RefSuffix" runDefaultChoice
  defaultChoice = randChoiceList $
    [ return AST_RefNull
    , return AST_DotRef <*> defaultO <*> randO <*> pure AST_RefNull <*> no
    , return AST_Subscript <*> pure nullValue <*> pure AST_RefNull
    , return AST_FuncCall  <*> pure nullValue <*> pure AST_RefNull
    ]

instance Intermediate RefSuffixExpr AST_RefSuffix where
  toInterm   ast = case ast of
    AST_RefNull                -> [NullRefExpr]
    AST_DotRef  _ name ref loc -> [DotRefExpr]    <*> [name]        <*> toInterm ref <*> [loc]
    AST_Subscript args ref     -> [SubscriptExpr] <*> toInterm args <*> toInterm ref
    AST_FuncCall  args ref     -> [FuncCallExpr]  <*> toInterm args <*> toInterm ref
  fromInterm obj = case obj of
    NullRefExpr                -> [AST_RefNull]
    DotRefExpr    name ref loc -> [AST_DotRef] <*> [Com ()] <*> [name] <*> fromInterm ref <*> [loc]
    SubscriptExpr args ref     -> [AST_Subscript] <*> fromInterm args <*> fromInterm ref
    FuncCallExpr  args ref     -> [AST_FuncCall]  <*> fromInterm args <*> fromInterm ref

instance ObjectClass AST_RefSuffix where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_RefSuffix where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Required parenthesese.
data ParenExpr = ParenExpr AssignExpr Location deriving (Eq, Ord, Typeable, Show)

-- | If an expression is inside of a 'ParenExpr', like it usually is after being parsed as part of
-- an @if@ or @while@ statement, this function evaluates the expression to a 'Prelude.Bool' value
-- used to determine if the conditional expression should be 'execute'd.
evalConditional :: ParenExpr -> Exec Bool
evalConditional o =
  (execute o :: Exec (Maybe Object)) >>=
    checkVoid (getLocation o) "conditional expression to if statement" >>= derefObject >>=
      execHandleIO [fmap (const False) execIOHandler] . return . not . testNull

instance HasLocation ParenExpr where
  getLocation (ParenExpr _ loc)     = loc
  setLocation (ParenExpr o _  ) loc = ParenExpr o loc
  delLocation (ParenExpr o _  )     = ParenExpr (delLocation o) LocationUnknown

instance HasNullValue ParenExpr where
  nullValue = ParenExpr nullValue LocationUnknown
  testNull (ParenExpr a _) = testNull a

instance NFData ParenExpr where { rnf (ParenExpr a b) = deepseq a $! deepseq b () }

instance PPrintable ParenExpr where { pPrint = pPrintInterm }

-- binary 0x59 
instance B.Binary ParenExpr MTab where
  put (ParenExpr a b) = B.prefixByte 0x59 $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting ParenExpr"
instance B.HasPrefixTable ParenExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParenExpr" 0x59 0x59 $
    [return ParenExpr <*> B.get <*> B.get]

instance Executable ParenExpr (Maybe Object) where { execute (ParenExpr a _) = execute a }

instance RefReducible ParenExpr where { reduceToRef (ParenExpr a _) = reduceToRef a }

instance ObjectClass ParenExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ParenExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AST_Paren = AST_Paren (Com AST_Assign) Location deriving (Eq, Ord, Typeable, Show)

instance HasLocation AST_Paren where
  getLocation (AST_Paren _ loc)     = loc
  setLocation (AST_Paren o _  ) loc = AST_Paren o loc
  delLocation (AST_Paren o _  )     = AST_Paren (delLocation o) LocationUnknown

instance HasNullValue AST_Paren where
  nullValue = AST_Paren nullValue LocationUnknown
  testNull (AST_Paren a _) = testNull a

instance NFData AST_Paren where { rnf (AST_Paren a b) = deepseq a $! deepseq b () }

instance PPrintable AST_Paren where
  pPrint (AST_Paren o _) = pInline [pString "(", pPrint o, pString ")"]

instance Intermediate ParenExpr AST_Paren where
  toInterm   (AST_Paren o loc) = [ParenExpr] <*> uc0 o <*> [loc]
  fromInterm (ParenExpr o loc) = [AST_Paren] <*> nc0 o <*> [loc]

instance ToDaoStructClass AST_Paren where
  toDaoStruct = renameConstructor "Paren" >> ask >>= \o -> case o of
    AST_Paren paren loc -> "inside" .= paren >> putLocation loc

instance FromDaoStructClass AST_Paren where
  fromDaoStruct = constructor "Paren" >> return AST_Paren <*> req "inside" <*> location

instance HasRandGen AST_Paren where
  randO = _randTrace "AST_Paren" $ recurse $ return AST_Paren <*> randO <*> no
  defaultO = _randTrace "D.AST_Paren" $ return AST_Paren <*> defaultO <*> no

instance ObjectClass AST_Paren where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Paren where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data IfExpr = IfExpr ParenExpr CodeBlock Location deriving (Eq, Ord, Typeable, Show)

instance NFData IfExpr where { rnf (IfExpr a b c) = deepseq a $! deepseq b $! deepseq c () }

instance NFData CodeBlock where { rnf (CodeBlock a) = deepseq a () }

instance HasNullValue IfExpr where
  nullValue = IfExpr nullValue nullValue LocationUnknown
  testNull (IfExpr a b _) = testNull a && testNull b

instance HasLocation IfExpr where
  getLocation (IfExpr _ _ loc)     = loc
  setLocation (IfExpr a b _  ) loc = IfExpr a b loc
  delLocation (IfExpr a b _  )     = IfExpr (delLocation a) (delLocation b) LocationUnknown

instance B.Binary IfExpr MTab where
  put (IfExpr a b c) = B.put a >> B.put b >> B.put c
  get = return IfExpr <*> B.get <*> B.get <*> B.get

instance Executable IfExpr Bool where
  execute (IfExpr ifn thn _) = execNested M.empty $
    evalConditional ifn >>= \test -> when test (execute thn) >> return test

instance ObjectClass IfExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass IfExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_If = AST_If (Com AST_Paren) AST_CodeBlock Location deriving (Eq, Ord, Typeable, Show)

instance NFData AST_If where { rnf (AST_If a b c) = deepseq a $! deepseq b $! deepseq c () }

instance HasLocation AST_If where
  getLocation (AST_If _ _ loc)     = loc
  setLocation (AST_If a b _  ) loc = AST_If a b loc
  delLocation (AST_If a b _  )     = AST_If (delLocation a) (delLocation b) LocationUnknown

instance HasNullValue AST_If where
  nullValue = AST_If nullValue nullValue LocationUnknown
  testNull (AST_If a b _) = testNull a && testNull b

instance PPrintable AST_If where
  pPrint (AST_If ifn thn _) =
    pClosure (pString "if" >> pPrint ifn) "{" "}" [pPrint thn]

instance ToDaoStructClass AST_If where
  toDaoStruct = renameConstructor "Conditional" >> ask >>= \o -> case o of
    AST_If ifn thn loc -> "condition" .= ifn >> "action" .= thn >> putLocation loc

instance FromDaoStructClass AST_If where
  fromDaoStruct = constructor "Conditional" >>
    return AST_If <*> req "condition" <*> req "action" <*> location

instance HasRandGen AST_If where
  randO = _randTrace "AST_If" $ countNode $ return AST_If <*> randO <*> randO <*> no
  defaultO = _randTrace "D.AST_If" $ return AST_If <*> defaultO <*> defaultO <*> no

instance Intermediate IfExpr AST_If where
  toInterm   (AST_If a b loc) = [IfExpr] <*> uc0 a <*> ti b <*> [loc]
  fromInterm (IfExpr a b loc) = [AST_If] <*> nc0 a <*> fi b <*> [loc]

instance ObjectClass AST_If where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_If where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data ElseExpr = ElseExpr IfExpr Location deriving (Eq, Ord, Typeable, Show)

instance NFData ElseExpr where { rnf (ElseExpr   a b  ) = deepseq a $! deepseq b $! () }

instance HasNullValue ElseExpr where
  nullValue = ElseExpr nullValue LocationUnknown
  testNull (ElseExpr a _) = testNull a

instance HasLocation ElseExpr where
  getLocation (ElseExpr _ loc)     = loc
  setLocation (ElseExpr a _  ) loc = ElseExpr a loc
  delLocation (ElseExpr a _  )     = ElseExpr (delLocation a) LocationUnknown

-- binary 0xB6 
instance B.Binary ElseExpr MTab where
  put (ElseExpr a b) = B.prefixByte 0xB6 $ B.put a >> B.put b
  get = (B.tryWord8 0xB6 $ return ElseExpr <*> B.get <*> B.get) <|> fail "expecting ElseExpr"

instance Executable ElseExpr Bool where { execute (ElseExpr ifn _) = execute ifn }

instance ObjectClass ElseExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ElseExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_Else = AST_Else (Com ()) AST_If Location deriving (Eq, Ord, Typeable, Show)
  -- ^ @/**/ else /**/ if /**/ obj /**/ {}@

instance NFData AST_Else where { rnf (AST_Else a b c) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue AST_Else where
  nullValue = AST_Else nullValue nullValue LocationUnknown
  testNull (AST_Else a b _) = testNull a && testNull b

instance HasLocation AST_Else where
  getLocation (AST_Else _ _ loc)     = loc
  setLocation (AST_Else a b _  ) loc = AST_Else a b loc
  delLocation (AST_Else a b _  )     = AST_Else a (delLocation b) LocationUnknown

instance PPrintable AST_Else where
  pPrint (AST_Else coms (AST_If ifn thn _) _) =
    pClosure (pPrintComWith (\ () -> pString "else ") coms >> pString "if" >> pPrint ifn) "{" "}" [pPrint thn]

instance ToDaoStructClass AST_Else where
  toDaoStruct = ask >>= \o -> case o of
    AST_Else coms ifn loc ->
      renameConstructor "ElseIf" >> "comments" .= coms >> "elseIf" .= ifn >> putLocation loc

instance FromDaoStructClass AST_Else where
  fromDaoStruct = constructor "ElseIf" >>
    return AST_Else <*> req "comments" <*> req "elseIf" <*> location

instance HasRandGen AST_Else where
  randO = _randTrace "AST_Else" $ countNode $ return AST_Else <*> randO <*> randO <*> no
  defaultO = _randTrace "D.AST_Else" $ return AST_Else <*> defaultO <*> defaultO <*> no

instance Intermediate ElseExpr AST_Else where
  toInterm   (AST_Else _ a loc) = [ElseExpr]              <*> ti a <*> [loc]
  fromInterm (ElseExpr   a loc) = [AST_Else] <*> [Com ()] <*> fi a <*> [loc]

instance ObjectClass AST_Else where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Else where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data IfElseExpr = IfElseExpr IfExpr [ElseExpr] (Maybe CodeBlock) Location deriving (Eq, Ord, Typeable, Show)

instance NFData IfElseExpr where
  rnf (IfElseExpr a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue IfElseExpr where
  nullValue = IfElseExpr nullValue [] Nothing LocationUnknown
  testNull (IfElseExpr a [] Nothing _) = testNull a
  testNull _ = False

instance HasLocation IfElseExpr where
  getLocation (IfElseExpr _ _ _ loc)     = loc
  setLocation (IfElseExpr a b c _  ) loc = IfElseExpr a b c loc
  delLocation (IfElseExpr a b c _  )     =
    IfElseExpr (delLocation a) (fmap delLocation b) (fmap delLocation c) LocationUnknown

-- binary 0xBA 
instance B.Binary IfElseExpr MTab where
  put (IfElseExpr a b c d) = B.prefixByte 0xBA $ B.put a >> B.put b >> B.put c >> B.put d
  get = B.word8PrefixTable <|> fail "expecting IfElseExpr"

instance B.HasPrefixTable IfElseExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "IfElseExpr" 0xBA 0xBA $
    [return IfElseExpr <*> B.get <*> B.get <*> B.get <*> B.get]

instance Executable IfElseExpr () where
  execute (IfElseExpr ifn elsx final _loc) = do
    let tryEach elsx = case elsx of
          []       -> return False
          els:elsx -> execute els >>= \ok -> if ok then return ok else tryEach elsx
    (execute ifn >>= \ok ->
      if ok then return Nothing
            else tryEach elsx >>= \ok ->
                 if ok then return Nothing
                       else return final) >>= maybe (return ()) execute

instance ObjectClass IfElseExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass IfElseExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_IfElse = AST_IfElse AST_If [AST_Else] (Com ()) (Maybe AST_CodeBlock) Location
  -- ^ @if /**/ obj /**/ {} /**/ else /**/ if /**/ obj /**/ {} /**/ else {}@
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_IfElse where
  rnf (AST_IfElse a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()

instance HasNullValue AST_IfElse where
  nullValue = AST_IfElse nullValue [] nullValue Nothing LocationUnknown
  testNull (AST_IfElse a [] (Com ()) Nothing _) = testNull a
  testNull _ = False

instance HasLocation AST_IfElse where
  getLocation (AST_IfElse _ _ _ _ loc)     = loc
  setLocation (AST_IfElse a b c d _  ) loc = AST_IfElse a b c d loc
  delLocation (AST_IfElse a b c d _  )     = AST_IfElse (delLocation a) (fmap delLocation b) c (fmap delLocation d) LocationUnknown

instance PPrintable AST_IfElse where
  pPrint (AST_IfElse ifn els coms deflt _) = do
    pPrint ifn >> pNewLine
    mapM_ pPrint els >> pNewLine
    case deflt of
      Nothing    -> return ()
      Just deflt -> pClosure (pPrintComWith (\ () -> pString "else") coms) "{" "}" [pPrint deflt]

instance ToDaoStructClass AST_IfElse where
  toDaoStruct = ask >>= \o -> case o of
    AST_IfElse ifn els coms block loc -> do
      renameConstructor "If"
      "test" .= ifn >> "alt" .= (listToObj els) >> "comments" .= coms
      maybe (return ()) (void . defObjField "finalElse") block
      putLocation loc

instance FromDaoStructClass AST_IfElse where
  fromDaoStruct = constructor "If" >>
    return AST_IfElse
      <*> req "test"
      <*> reqList "alt"
      <*> req "comments"
      <*> opt "finalElse"
      <*> location

instance HasRandGen AST_IfElse where
  randO = _randTrace "AST_IfElse" $ countNode $ depthLimitedInt 8 >>= \x ->
    return AST_IfElse <*> randO <*> randList 0 x <*> randO <*> scrambO <*> no
  defaultO = _randTrace "D.AST_IfElse" $ return AST_IfElse <*> defaultO <*> defaultList 0 1 <*> randO <*> randO <*> no

instance Intermediate IfElseExpr AST_IfElse where
  toInterm   (AST_IfElse a b _ c loc) =
    [IfElseExpr] <*> ti a <*> [b>>=ti]              <*> um1 c <*> [loc]
  fromInterm (IfElseExpr a b   c loc) =
    [AST_IfElse] <*> fi a <*> [b>>=fi] <*> [Com ()] <*> nm1 c <*> [loc]

instance ObjectClass AST_IfElse where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_IfElse where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

newtype WhileExpr = WhileExpr  IfExpr deriving (Eq, Ord, Typeable, Show)

instance NFData WhileExpr  where { rnf (WhileExpr (IfExpr a b c)) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue WhileExpr where
  nullValue = WhileExpr nullValue
  testNull (WhileExpr a) = testNull a

instance HasLocation WhileExpr where
  getLocation (WhileExpr a)     = getLocation a
  setLocation (WhileExpr a) loc = WhileExpr (setLocation a loc)
  delLocation (WhileExpr a)     = WhileExpr (delLocation a)

-- binary 0x85 
instance B.Binary WhileExpr MTab where
  put (WhileExpr o) = B.prefixByte 0x85 $ B.put o
  get = B.word8PrefixTable <|> fail "expecting WhileExpr"

instance B.HasPrefixTable WhileExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "WhileExpr" 0x85 0x85 [WhileExpr <$> B.get]

instance Executable WhileExpr () where
  execute (WhileExpr ifn) = let loop = execute ifn >>= flip when loop in loop

instance ObjectClass WhileExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass WhileExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

newtype AST_While = AST_While AST_If deriving (Eq, Ord, Typeable, Show)

instance NFData AST_While where { rnf (AST_While (AST_If a b c)) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue AST_While where
  nullValue = AST_While nullValue
  testNull (AST_While a) = testNull a

instance HasLocation AST_While where
  getLocation (AST_While a) = getLocation a
  setLocation (AST_While a) loc = AST_While (setLocation a loc)
  delLocation (AST_While a)     = AST_While (delLocation a)

instance PPrintable AST_While where
  pPrint (AST_While (AST_If ifn thn _)) =
    pClosure (pInline [pString "while", pPrint ifn]) "{" "}" [pPrint thn]

instance Intermediate WhileExpr AST_While where
  toInterm   (AST_While a) = WhileExpr <$> ti a
  fromInterm (WhileExpr a) = AST_While <$> fi a

instance ToDaoStructClass AST_While where
  toDaoStruct = ask >>= \ (AST_While o) -> innerToStruct o >> renameConstructor "While"

instance FromDaoStructClass AST_While where
  fromDaoStruct = constructor "While" >> AST_While <$> innerFromStruct "Conditional"

instance HasRandGen AST_While  where
  randO    = _randTrace "AST_While" $ AST_While <$> randO
  defaultO = _randTrace "D.AST_While" $ AST_While <$> defaultO

instance ObjectClass AST_While where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_While where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data ScriptExpr
  = IfThenElse   IfElseExpr
  | WhileLoop    WhileExpr
  | RuleFuncExpr RuleFuncExpr
  | EvalObject   AssignExpr                                 Location -- location of the semicolon
  | TryCatch     CodeBlock (Maybe Name)   (Maybe CodeBlock) Location
  | ForLoop      Name       RefPrefixExpr CodeBlock         Location
  | ContinueExpr Bool       AssignExpr                      Location
  | ReturnExpr   Bool       AssignExpr                      Location
  | WithDoc      ParenExpr  CodeBlock                       Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData ScriptExpr where
  rnf (IfThenElse   a      ) = deepseq a ()
  rnf (WhileLoop    a      ) = deepseq a ()
  rnf (RuleFuncExpr a      ) = deepseq a ()
  rnf (EvalObject   a b    ) = deepseq a $! deepseq b ()
  rnf (TryCatch     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ForLoop      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ContinueExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ReturnExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (WithDoc      a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue ScriptExpr where
  nullValue = EvalObject nullValue LocationUnknown
  testNull (EvalObject a _) = testNull a
  testNull _ = False

instance HasLocation ScriptExpr where
  getLocation o = case o of
    EvalObject   _     o -> o
    IfThenElse         o -> getLocation o
    RuleFuncExpr       o -> getLocation o
    WhileLoop          o -> getLocation o
    TryCatch     _ _ _ o -> o
    ForLoop      _ _ _ o -> o
    ContinueExpr _ _   o -> o
    ReturnExpr   _ _   o -> o
    WithDoc      _ _   o -> o
  setLocation o loc = case o of
    EvalObject   a     _ -> EvalObject   a     loc
    IfThenElse   a       -> IfThenElse   (setLocation a loc)
    WhileLoop    a       -> WhileLoop    (setLocation a loc)
    RuleFuncExpr a       -> RuleFuncExpr (setLocation a loc)
    TryCatch     a b c _ -> TryCatch     a b c loc
    ForLoop      a b c _ -> ForLoop      a b c loc
    ContinueExpr a b   _ -> ContinueExpr a b   loc
    ReturnExpr   a b   _ -> ReturnExpr   a b   loc
    WithDoc      a b   _ -> WithDoc      a b   loc
  delLocation o = case o of
    EvalObject   a     _ -> EvalObject   (fd a)                   lu
    IfThenElse   a       -> IfThenElse   (fd a)
    WhileLoop    a       -> WhileLoop    (fd a)
    RuleFuncExpr a       -> RuleFuncExpr (fd a)
    TryCatch     a b c _ -> TryCatch     (fd a)     b (fmap fd c) lu
    ForLoop      a b c _ -> ForLoop          a  (fd b)     (fd c) lu
    ContinueExpr a b   _ -> ContinueExpr     a  (fd b)            lu
    ReturnExpr   a b   _ -> ReturnExpr       a  (fd b)            lu
    WithDoc      a b   _ -> WithDoc      (fd a) (fd b)            lu
    where
      lu = LocationUnknown
      fd :: HasLocation a => a -> a
      fd = delLocation

-- binary 0xA8 0xAF
instance B.Binary ScriptExpr MTab where
  put o = case o of
    IfThenElse   a           -> B.put a
    WhileLoop    a           -> B.put a
    RuleFuncExpr a           -> B.put a
    EvalObject   a         z -> B.prefixByte 0xA8 $ B.put a >> B.put z
    TryCatch     a     b c z -> B.prefixByte 0xA9 $ B.put a >> B.put b >> B.put c >> B.put z
    ForLoop      a     b c z -> B.prefixByte 0xAA $ B.put a >> B.put b >> B.put c >> B.put z
    ContinueExpr True  b   z -> B.prefixByte 0xAB $ B.put b >> B.put z
    ContinueExpr False b   z -> B.prefixByte 0xAC $ B.put b >> B.put z
    ReturnExpr   True  b   z -> B.prefixByte 0xAD $ B.put b >> B.put z
    ReturnExpr   False b   z -> B.prefixByte 0xAE $ B.put b >> B.put z
    WithDoc      a     b   z -> B.prefixByte 0xAF $ B.put a >> B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting ScriptExpr"

instance B.HasPrefixTable ScriptExpr B.Byte MTab where
  prefixTable = mconcat $
    [ fmap IfThenElse B.prefixTable
    , fmap WhileLoop  B.prefixTable
    , fmap RuleFuncExpr B.prefixTable
    , B.mkPrefixTableWord8 "ScriptExpr" 0xA8 0xAF $ -- 0x89 0x8A 0x8B 0x8C 0x8D 0x8E 0x8F 0x90
        [ return EvalObject   <*> B.get <*> B.get
        , return TryCatch     <*> B.get <*> B.get <*> B.get <*> B.get
        , return ForLoop      <*> B.get <*> B.get <*> B.get <*> B.get
        , return (ContinueExpr True ) <*> B.get <*> B.get
        , return (ContinueExpr False) <*> B.get <*> B.get
        , return (ReturnExpr   True ) <*> B.get <*> B.get
        , return (ReturnExpr   False) <*> B.get <*> B.get
        , return WithDoc      <*> B.get <*> B.get <*> B.get
        ]
    ]

-- | Convert a single 'ScriptExpr' into a function of value @'Exec' 'Object'@.
instance Executable ScriptExpr () where
  execute script = _setScriptExprError script $ case script of
    IfThenElse   ifn    -> execute ifn
    WhileLoop    ifn    -> execute ifn
    EvalObject   o _loc -> void (execute o :: Exec (Maybe Object))
    RuleFuncExpr rulfn  -> do
      o <- execute rulfn
      (StaticStore sub) <- asks currentCodeBlock
      let dyn o = case o of
            OHaskell (HaskellData h _) -> fromDynamic h
            _ -> Nothing
      let pushItem insert = case o >>= dyn of
            Just  o -> liftIO (insert o)
            Nothing -> error "executing RuleFuncExpr does not produce object of correct data type"
      case sub of
        Nothing  -> case rulfn of
          LambdaExpr{} -> asks lambdaSet >>= \s -> pushItem $ \o -> modifyIORef s (++[o])
          RuleExpr{}   -> asks ruleSet >>= \s ->
            pushItem $ \o -> modifyIORef s (insertMultiPattern (++) (globPattern o) [globSubroutine o])
          FuncExpr{}   -> return ()
            -- function expressions are placed in the correct store by 'execute'
        Just sub -> case rulfn of
          LambdaExpr{} -> pushItem $ \o -> modifyIORef (staticLambdas sub) (++[o])
          RuleExpr{}   -> pushItem $ \o ->
            modifyIORef (staticRules sub) (insertMultiPattern (++) (globPattern o) [globSubroutine o])
          FuncExpr{}   -> return ()
            -- function expressions are placed in the correct store by 'execute'
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    TryCatch try  name  catch _loc -> do
      ce <- catchPredicate (execNested M.empty $ execute try)
      let runCatch err = case return (,,) <*> name <*> err <*> catch of
            Nothing               -> maybe (return ()) (execNested M.empty . execute) catch
            Just (nm, err, catch) -> execNested M.empty (localVarDefine nm (new err) >> execute catch)
      case ce of
        OK               ()  -> return ()
        Backtrack            -> runCatch (Nothing :: Maybe ExecControl)
        PFail (ExecReturn{}) -> predicate ce
        PFail            err -> runCatch (Just err)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ForLoop varName inObj thn _loc -> do
      qref <- reduceToRef inObj
      let nested f o = execNested (M.singleton varName o) f
      let run = void $ execute thn
      let getvar = asks execStack >>= \ (LocalStore sto) ->
            fmap (stackLookup varName) (liftIO $ readIORef sto)
      let readLoop   o = readForLoop o (maybe run $ nested run)
      let updateLoop o = updateForLoop (OList []) o (maybe (run>>getvar) (nested $ run>>getvar))
      case qref of
        RefWrapper{}        -> execThrow $ obj $
          [obj "cannot iterate over value of type reference", obj qref]
        RefObject o NullRef -> readLoop o
        _                   -> void $ referenceUpdate qref $ \o -> case o of
          Nothing -> execThrow $ obj [obj "cannot iterate over undefined reference", obj qref]
          Just  o -> case o of -- determine wheter this is an OHaskell object that can be updated
            OHaskell (HaskellData _ ifc) -> case objUpdateIterable ifc of
              Just  _ -> Just <$> updateLoop o
              Nothing -> case objReadIterable ifc of -- if it's not updatable but readable
                Just  _ -> readLoop o >> return (Just o)
                Nothing -> execThrow $ obj $
                  [obj "cannot iterate over value for reference", obj qref]
            _ -> Just <$> (updateLoop o >>= \o -> return o)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ContinueExpr a    _      _loc -> execThrow $ obj $
      '"':(if a then "continue" else "break")++"\" expression is not within a \"for\" or \"while\" loop"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ReturnExpr returnStmt o _loc -> do
      o <- (execute o :: Exec (Maybe Object)) >>= maybe (return Nothing) (fmap Just . derefObject)
      if returnStmt then throwError (ExecReturn o) else maybe mzero execThrow o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    WithDoc   expr   _thn    _loc -> void $ execNested M.empty $ do
      o   <- execute expr >>= checkVoid (getLocation expr) "expression in the focus of \"with\" statement"
      ref <- mplus (execute $ asReference o) $ execThrow $ obj $
        [obj "expression in \"with\" statement does not evaluate to a reference, evaluates to a", o]
      referenceUpdate ref $ \o -> case o of
        Nothing -> execThrow $ obj [obj "undefined reference:", ORef ref]
        Just o  -> do
          let ok = do
                ioref <- liftIO (newIORef o)
                execNested M.empty $
                  local (\x->x{currentWithRef=WithRefStore (Just ioref)}) (execute expr)
                liftIO (fmap Just (readIORef ioref))
          case o of
            OTree    _ -> ok
            OHaskell (HaskellData _ ifc) -> case objFromStruct ifc of
              Just  _ -> ok
              Nothing -> execThrow $ obj $
                [ obj "object of type:", obj (toUStr (show (objHaskellType ifc)))
                , obj "is not defined to be used in \"with\" statements"
                ]
            _ -> execThrow $ obj $
              [ obj "object value at reference:", ORef ref
              , obj "has no method of being used in a \"with\" statement, value is", o
              ]
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass ScriptExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ScriptExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

localVarDefine :: Name -> Object -> Exec ()
localVarDefine nm obj = asks execStack >>= \sto -> storeDefine sto nm obj

-- | Like evaluating 'execute' on a value of 'Reference', except the you are evaluating an
-- 'Object' type. If the value of the 'Object' is not constructed with
-- 'ORef', the object value is returned unmodified.
derefObject :: Object -> Exec Object
derefObject = fmap snd . derefObjectGetReference

-- | Like 'derefObject' but also returns the 'Reference' value that was stored in the 'Object' that
-- was dereferenced, along with the dereferenced value. If the 'Object' is not constructed with
-- 'ORef', 'Prelude.Nothing' is returned instead of a 'Reference'.
derefObjectGetReference :: Object -> Exec (Maybe Reference, Object)
derefObjectGetReference o = maybeDerefObject (Just o) >>= \ (r, derefd) -> case derefd of
  Nothing     -> execThrow $ obj [obj "reference evaluated to void", o]
  Just derefd -> return (r, derefd)

-- | Tries to dereference an 'Object'. If the 'Object' is an 'ORef' constructed 'Reference', the
-- reference is de-referenced, which may evaluate to 'Prelude.Nothing'. The dereferenced value is
-- returned in the 'Prelude.snd' of the pair. If the given 'Object' is an 'ORef', regardless of the
-- dereferenced value, the 'Reference' is returned in the 'Prelude.fst' of the pair. If the given
-- 'Object' is not an 'ORef' constructed 'Object', it is returned unmodified along with
-- 'Prelude.Nothing' in the 'Prelude.fst' of the pair.
maybeDerefObject :: Maybe Object -> Exec (Maybe Reference, Maybe Object)
maybeDerefObject = maybe (return (Nothing, Nothing)) $ \o -> case o of
  ORef r -> referenceLookup r >>= \o -> case o of
    Nothing     -> return (Just r, Nothing)
    Just (r, o) -> return (Just r, Just o)
  o      -> return (Nothing, Just o)

----------------------------------------------------------------------------------------------------

-- | This data type instantates the 'execute' function for use in for-loop expressions.
data ForLoopBlock = ForLoopBlock Name Object CodeBlock

instance Executable ForLoopBlock (Bool, Maybe Object) where
  execute (ForLoopBlock name o block) = 
    execNested (M.singleton name o) $ loop (codeBlock block) where
      done cont = do
        (LocalStore ref) <- asks execStack
        newValue <- liftIO (fmap (M.lookup name . head . mapList) (readIORef ref))
        return (cont, newValue)
      loop ex = case ex of
        []   -> done True
        e:ex -> case e of
          ContinueExpr a cond _loc -> case cond of
            EvalExpr (ObjArithExpr (ObjectExpr VoidExpr)) -> done a
            cond -> execute cond >>= maybe err (execute . objToBool) >>= done . (if a then id else not) where
              err = fail "expression does not evaluate to boolean"
          e -> execute e >> loop ex

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data AST_Script
  = AST_Comment     [Comment] 
  | AST_IfThenElse  AST_IfElse
  | AST_WhileLoop   AST_While
  | AST_RuleFunc    AST_RuleFunc
  | AST_EvalObject  AST_Assign  [Comment]                                              Location
    -- ^ @some.object.expression = for.example - equations || function(calls) /**/ ;@
  | AST_TryCatch     (Com AST_CodeBlock)     (Maybe (Com Name)) (Maybe AST_CodeBlock)  Location
    -- ^ @try /**/ {} /**/ catch /**/ errVar /**/ {}@              
  | AST_ForLoop      (Com Name)              (Com AST_RefPrefix)       AST_CodeBlock   Location
    -- ^ @for /**/ var /**/ in /**/ objExpr /**/ {}@
  | AST_ContinueExpr Bool  [Comment]         (Com AST_Assign)                          Location
    -- ^ The boolean parameter is True for a "continue" statement, False for a "break" statement.
    -- @continue /**/ ;@ or @continue /**/ if /**/ objExpr /**/ ;@
  | AST_ReturnExpr   Bool                    (Com AST_Assign)                          Location
    -- ^ The boolean parameter is True for a "return" statement, False for a "throw" statement.
    -- ^ @return /**/ ;@ or @return /**/ objExpr /**/ ;@
  | AST_WithDoc      (Com AST_Paren)         AST_CodeBlock                             Location
    -- ^ @with /**/ objExpr /**/ {}@
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Script where
  rnf (AST_Comment      a      ) = deepseq a ()
  rnf (AST_IfThenElse   a      ) = deepseq a ()
  rnf (AST_WhileLoop    a      ) = deepseq a ()
  rnf (AST_RuleFunc     a      ) = deepseq a ()
  rnf (AST_EvalObject   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_TryCatch     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ForLoop      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ContinueExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ReturnExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_WithDoc      a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_Script where
  nullValue = AST_EvalObject nullValue [] LocationUnknown
  testNull (AST_EvalObject a _ _) = testNull a
  testNull _ = False

instance HasLocation AST_Script where
  getLocation o = case o of
    AST_Comment      _       -> lu
    AST_EvalObject   _   _ o -> o
    AST_IfThenElse         o -> getLocation o
    AST_WhileLoop          o -> getLocation o
    AST_RuleFunc           o -> getLocation o
    AST_TryCatch     _ _ _ o -> o
    AST_ForLoop      _ _ _ o -> o
    AST_ContinueExpr _ _ _ o -> o
    AST_ReturnExpr   _ _   o -> o
    AST_WithDoc      _ _   o -> o
  setLocation o loc = case o of
    AST_Comment      a       -> AST_Comment      a
    AST_EvalObject   a b   _ -> AST_EvalObject   a b   loc
    AST_IfThenElse   a       -> AST_IfThenElse   (setLocation a loc)
    AST_WhileLoop    a       -> AST_WhileLoop    (setLocation a loc)
    AST_RuleFunc     a       -> AST_RuleFunc     (setLocation a loc)
    AST_TryCatch     a b c _ -> AST_TryCatch     a b c loc
    AST_ForLoop      a b c _ -> AST_ForLoop      a b c loc
    AST_ContinueExpr a b c _ -> AST_ContinueExpr a b c loc
    AST_ReturnExpr   a b   _ -> AST_ReturnExpr   a b   loc
    AST_WithDoc      a b   _ -> AST_WithDoc      a b   loc
  delLocation o = case o of
    AST_Comment      a       -> AST_Comment           a
    AST_EvalObject   a b   _ -> AST_EvalObject   (fd  a)      b              lu
    AST_IfThenElse   a       -> AST_IfThenElse   (fd  a)
    AST_WhileLoop    a       -> AST_WhileLoop    (fd  a)
    AST_RuleFunc     a       -> AST_RuleFunc     (fd  a)
    AST_TryCatch     a b c _ -> AST_TryCatch     (fd1 a)      b  (fmap fd c) lu
    AST_ForLoop      a b c _ -> AST_ForLoop           a  (fd1 b) (fd  c)     lu
    AST_ContinueExpr a b c _ -> AST_ContinueExpr      a       b  (fd1 c)     lu
    AST_ReturnExpr   a b   _ -> AST_ReturnExpr        a  (fd1 b)             lu
    AST_WithDoc      a b   _ -> AST_WithDoc      (fd1 a) (fd  b)             lu

instance PPrintable AST_Script where
  pPrint expr = pGroup True $ case expr of
    AST_Comment             coms -> mapM_ pPrint coms
    AST_EvalObject   objXp  coms                    _ ->
      pPrint objXp >> mapM_ pPrint coms >> pString ";"
    AST_IfThenElse   ifXp                             -> pPrint ifXp
    AST_WhileLoop    whileLoop                        -> pPrint whileLoop
    AST_RuleFunc     ruleOrFunc                       -> pPrint ruleOrFunc
    AST_TryCatch     cxcScrpXp  cQRef     xcScrpXp  _ -> do
      pClosure (pString "try") "{" "}" [pPrint cxcScrpXp]
      maybe (return ()) id $ msum $
        [ cQRef >>= \qref -> xcScrpXp >>= \xscrp -> Just $
            pClosure (pString "catch " >> pPrint qref) "{" "}" [pPrint xscrp]
        , cQRef    >>= \qref  -> Just $ pString "catch " >> pPrint qref >> pString ";"
        , xcScrpXp >>= \xscrp -> Just $ pClosure (pString "catch ") "{" "}" [pPrint xscrp]
        ]
    AST_ForLoop      cNm        cObjXp    xcScrpXp  _ ->
      pPrintSubBlock (pString "for " >> pPrint cNm >> pString " in " >> pPrint cObjXp) xcScrpXp
    AST_ContinueExpr contin     coms      cObjXp    _ -> pWrapIndent $
      [ pString (if contin then "continue" else "break")
      , pInline (map pPrint coms)
      , case unComment cObjXp of
          AST_Eval (AST_ObjArith (AST_Object AST_Void)) -> return ()
          _ -> pString " if" >> when (precedeWithSpace cObjXp) (pString " ") >> pPrint cObjXp
      , pString ";"
      ]
    AST_ReturnExpr   retrn                cObjXp    _ -> pWrapIndent $
      [pString (if retrn then "return " else "throw "), pPrint cObjXp, pString ";"]
    AST_WithDoc      cObjXp               xcScrpXp  _ ->
      pPrintSubBlock (pString "with " >> pPrint cObjXp) xcScrpXp

instance ToDaoStructClass AST_Script where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Comment      a         -> nm "Comment" >> putComments a
    AST_IfThenElse   a         -> innerToStruct a
    AST_WhileLoop    a         -> innerToStruct a
    AST_RuleFunc     a         -> innerToStruct a
    AST_EvalObject   a b   loc -> nm "ObjectExpr" >> "expr" .= a >> putComments b >> putLocation loc
    AST_TryCatch     a b c loc -> nm "TryCatch" >>
      "tryBlock" .= a >> "varName" .=? b >> "catchBlock" .=? c >> putLocation loc
    AST_ForLoop      a b c loc -> nm "ForLoop" >>
      "varName" .= a >> "iterate" .= b >> "block" .= c >> putLocation loc
    AST_ContinueExpr a b c loc -> do
      nm (if a then "Continue" else "Break")
      putComments b >> "condition" .= c >> putLocation loc
    AST_ReturnExpr   a b   loc -> do
      nm (if a then "Return" else "Throw")
      "expr" .= b >> putLocation loc
    AST_WithDoc      a b   loc -> nm "WithDoc" >> "expr" .= a >> "block" .= b >> putLocation loc

instance FromDaoStructClass AST_Script where
  fromDaoStruct = msum $
    [ constructor "Comment" >> AST_Comment <$> comments
    , AST_IfThenElse <$> fromDaoStruct
    , AST_WhileLoop  <$> fromDaoStruct
    , AST_RuleFunc   <$> fromDaoStruct
    , constructor "ObjectExpr" >> return AST_EvalObject <*> req "expr" <*> comments <*> location
    , constructor "TryCatch" >>
        return AST_TryCatch <*> req "tryBlock" <*> opt "varName" <*> opt "catchBlock" <*> location
    , constructor "ForLoop" >>
        return AST_ForLoop <*> req "varName" <*> req "iterate" <*> req "block" <*> location
    , constructor "Continue" >>
        return (AST_ContinueExpr True ) <*> comments <*> req "condition" <*> location
    , constructor "Break" >>
        return (AST_ContinueExpr False) <*> comments <*> req "condition" <*> location
    , constructor "Return" >> return (AST_ReturnExpr True ) <*> req "expr" <*> location
    , constructor "Throw"  >> return (AST_ReturnExpr False) <*> req "expr" <*> location
    , constructor "WithDoc" >> return AST_WithDoc <*> req "expr" <*> req "block" <*> location
    ]

instance HasRandGen AST_Script where
  randO = _randTrace "AST_Script" $ countNode $ recurse $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_EvalObject   <*> randO <*> randO <*> no
    , return AST_IfThenElse   <*> randO
    , return AST_WhileLoop    <*> randO
    , return AST_RuleFunc     <*> randO
    , scramble $ return AST_TryCatch     <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_ForLoop      <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_ContinueExpr <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_ReturnExpr   <*> randO <*> randO <*> no
    , scramble $ return AST_WithDoc      <*> randO <*> randO <*> no
    ]
  defaultO = _randTrace "D.AST_Script" runDefaultChoice
  defaultChoice = randChoiceList $
    [ AST_IfThenElse <$> defaultO
    , AST_WhileLoop  <$> defaultO
    , AST_RuleFunc   <$> defaultO
    ]

instance PPrintable ScriptExpr where { pPrint = pPrintInterm }

instance Intermediate ScriptExpr AST_Script where
  toInterm   ast = case ast of
    AST_Comment      _         -> mzero
    AST_EvalObject   a _   loc -> [EvalObject  ] <*> ti  a                     <*> [loc]
    AST_IfThenElse   a         -> [IfThenElse  ] <*> ti  a
    AST_WhileLoop    a         -> [WhileLoop   ] <*> ti  a
    AST_RuleFunc     a         -> [RuleFuncExpr] <*> ti  a
    AST_TryCatch     a b c loc -> [TryCatch    ] <*> uc0 a <*> um0 b <*> um1 c <*> [loc]
    AST_ForLoop      a b c loc -> [ForLoop     ] <*> uc  a <*> uc0 b <*> ti  c <*> [loc]
    AST_ContinueExpr a _ c loc -> [ContinueExpr] <*> [a]   <*> uc0 c           <*> [loc]
    AST_ReturnExpr   a b   loc -> [ReturnExpr  ] <*> [a]   <*> uc0 b           <*> [loc]
    AST_WithDoc      a b   loc -> [WithDoc     ] <*> uc0 a <*> ti  b           <*> [loc]
  fromInterm obj = case obj of
    EvalObject   a     loc -> [AST_EvalObject  ] <*> fi  a  <*> [[]]           <*> [loc]
    IfThenElse   a         ->  AST_IfThenElse    <$> fi  a
    WhileLoop    a         ->  AST_WhileLoop     <$> fi  a
    RuleFuncExpr a         ->  AST_RuleFunc      <$> fi  a
    TryCatch     a b c loc -> [AST_TryCatch    ] <*> nc0 a  <*> nm0 b <*> nm1 c <*> [loc]
    ForLoop      a b c loc -> [AST_ForLoop     ] <*> nc  a  <*> nc0 b <*> fi  c <*> [loc]
    ContinueExpr a b   loc -> [AST_ContinueExpr] <*>    [a] <*> [[]]  <*> nc0 b <*> [loc]
    ReturnExpr   a b   loc -> [AST_ReturnExpr  ] <*>    [a] <*> nc0 b           <*> [loc]
    WithDoc      a b   loc -> [AST_WithDoc     ] <*> nc0 a  <*> fi  b           <*> [loc]

instance ObjectClass [AST_Script] where { obj=listToObj; fromObj=listFromObj; }

instance ObjectClass AST_Script where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Script where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Contains a list of 'ObjectExpr's, which are used to encode parameters to function calls, and
-- intialization lists.
data ObjListExpr = ObjListExpr [AssignExpr] Location deriving (Eq, Ord, Typeable)

instance Show ObjListExpr where { show (ObjListExpr o loc) = show o++show loc }

instance Monoid ObjListExpr where
  mempty = ObjListExpr [] LocationUnknown
  mappend (ObjListExpr a locA) (ObjListExpr b locB) = ObjListExpr (a++b) (locA<>locB)

instance NFData ObjListExpr where { rnf (ObjListExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue ObjListExpr where
  nullValue = mempty
  testNull (ObjListExpr a _) = null a

instance HasLocation ObjListExpr where
  getLocation (ObjListExpr _ loc)     = loc
  setLocation (ObjListExpr a _  ) loc = ObjListExpr (fmap delLocation a) loc
  delLocation (ObjListExpr a _  )     = ObjListExpr (fmap delLocation a) LocationUnknown

-- binary 0x86 
instance B.Binary ObjListExpr MTab where
  put (ObjListExpr lst loc) = B.prefixByte 0x86 $ B.putUnwrapped lst >> B.put loc
  get = (B.tryWord8 0x86 $ return ObjListExpr <*> B.getUnwrapped <*> B.get) <|> fail "expecting ObjListExpr"

instance Executable ObjListExpr [(Location, Reference)] where
  execute (ObjListExpr exprs loc) = mapM (fmap ((,) loc) . reduceToRef) exprs

_reduceArgs :: String -> [(Location, Reference)] -> Exec [Object]
_reduceArgs msg = mapM $ \ (loc, ref) -> fmap snd <$> referenceLookup ref >>= checkVoid loc msg

instance PPrintable ObjListExpr where { pPrint = pPrintInterm }

instance ObjectClass ObjListExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ObjListExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_ObjList = AST_ObjList [Comment] [Com AST_Assign] Location deriving (Eq, Ord, Typeable, Show)

instance Monoid AST_ObjList where
  mempty = AST_ObjList [] [] LocationUnknown
  mappend (AST_ObjList a1 a2 aloc) (AST_ObjList b1 b2 bloc) = AST_ObjList (a1++b1) (a2++b2) (aloc<>bloc)

instance HasNullValue AST_ObjList where
  nullValue = mempty
  testNull (AST_ObjList [] [] _) = True
  testNull _ = False

instance HasLocation AST_ObjList where
  getLocation (AST_ObjList _ _ loc)     = loc
  setLocation (AST_ObjList a b _  ) loc = AST_ObjList a      b  loc
  delLocation (AST_ObjList a b _  )     = AST_ObjList a (fd1 b) lu

instance PPrintable AST_ObjList where
  pPrint (AST_ObjList coms lst _) = pPrint coms >>
    pInline (intersperse (pString ", ") (map pPrint lst))

instance NFData AST_ObjList where { rnf (AST_ObjList a b c) = deepseq a $! deepseq b $! deepseq c () }

instance ToDaoStructClass AST_ObjList where
  toDaoStruct = ask >>= \o -> case o of
    AST_ObjList coms lst loc -> renameConstructor "ObjectList" >>
      putComments coms >> defObjField "items" (listToObj lst) >> putLocation loc

instance FromDaoStructClass AST_ObjList where
  fromDaoStruct = constructor "ObjectList" >>
    return AST_ObjList <*> comments <*> reqList "items" <*> location

instance HasRandGen AST_ObjList where
  randO = _randTrace "AST_ObjList" $ recurse $ depthLimitedInt 8 >>= \x ->
    AST_ObjList <$> randO <*> randList 0 x <*> no
  defaultO = _randTrace "D.ObjList" $ return AST_ObjList <*> defaultO <*> pure [] <*> no

instance Intermediate ObjListExpr AST_ObjList where
  toInterm   (AST_ObjList _ lst loc) = [ObjListExpr]          <*> [lst>>=uc0] <*> [loc]
  fromInterm (ObjListExpr   lst loc) = [AST_ObjList] <*> [[]] <*> [lst>>=nc0] <*> [loc]

instance ObjectClass AST_ObjList where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_ObjList where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

newtype OptObjListExpr = OptObjListExpr (Maybe ObjListExpr) deriving (Eq, Ord, Typeable, Show)

instance NFData OptObjListExpr where { rnf (OptObjListExpr a) = deepseq a () }

instance HasLocation OptObjListExpr where
  getLocation (OptObjListExpr o)     = maybe LocationUnknown getLocation o
  setLocation (OptObjListExpr o) loc = OptObjListExpr (setLocation o loc)
  delLocation (OptObjListExpr o)     = OptObjListExpr (delLocation o    )

instance HasNullValue OptObjListExpr where
  nullValue = OptObjListExpr Nothing
  testNull (OptObjListExpr Nothing) = True
  testNull _ = False

instance B.Binary OptObjListExpr MTab where
  put (OptObjListExpr o) = B.put o
  get = OptObjListExpr <$> B.get

instance Executable OptObjListExpr [(Location, Reference)] where
  execute (OptObjListExpr lst) = maybe (return []) execute lst

instance ObjectClass OptObjListExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass OptObjListExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

-- | Evaluate an 'Exec', but if it throws an exception, set record an 'ObjectExpr' where
-- the exception occurred in the exception information.
updateExecError :: (ExecControl -> ExecControl) -> Exec a -> Exec a
updateExecError upd fn = catchError fn (\err -> throwError (upd err))

----------------------------------------------------------------------------------------------------

data AST_OptObjList = AST_OptObjList [Comment] (Maybe AST_ObjList) deriving (Eq, Ord, Typeable, Show)

instance NFData AST_OptObjList where
  rnf (AST_OptObjList a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_OptObjList where
  nullValue = AST_OptObjList [] Nothing
  testNull (AST_OptObjList _ a) = maybe True testNull a

pPrintObjList :: String -> String -> String -> AST_ObjList -> PPrint
pPrintObjList open comma close (AST_ObjList coms lst _) = pList (pPrint coms) open comma close (map pPrint lst)

pPrintOptObjList :: String -> String -> String -> AST_OptObjList -> PPrint
pPrintOptObjList open comma close (AST_OptObjList coms o) =
  maybe (return ()) (\o -> pPrint coms >> pPrintObjList open comma close o) o

instance HasLocation AST_OptObjList where
  getLocation (AST_OptObjList _ o)     = maybe LocationUnknown getLocation o
  setLocation (AST_OptObjList c o) loc = AST_OptObjList c (fmap (flip setLocation loc) o)
  delLocation (AST_OptObjList c o)     = AST_OptObjList c (fmap delLocation o)

instance PPrintable AST_OptObjList where { pPrint o = pPrintOptObjList "{" ", " "}" o }

instance ToDaoStructClass AST_OptObjList where
  toDaoStruct = ask >>= \o -> case o of
    AST_OptObjList coms o -> renameConstructor "OptObjList" >> "params" .=? o >> putComments coms

instance FromDaoStructClass AST_OptObjList where
  fromDaoStruct = constructor "OptObjList" >> return AST_OptObjList <*> comments <*> opt "params"

instance HasRandGen AST_OptObjList where
  randO = _randTrace "AST_OptObjList" $ countNode $ return AST_OptObjList <*> randO <*> randO
  defaultO = _randTrace "D.AST_OptObjList" $ return AST_OptObjList <*> defaultO <*> defaultO

instance Intermediate OptObjListExpr AST_OptObjList where
  toInterm   (AST_OptObjList _ o) = OptObjListExpr    <$> um1 o
  fromInterm (OptObjListExpr   o) = AST_OptObjList [] <$> nm1 o

instance ObjectClass AST_OptObjList where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_OptObjList where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data LiteralExpr = LiteralExpr Object Location deriving (Eq, Ord, Typeable, Show)

instance NFData LiteralExpr where
  rnf (LiteralExpr a b) = deepseq a $! deepseq b ()

instance HasNullValue LiteralExpr where
  nullValue = LiteralExpr ONull LocationUnknown
  testNull (LiteralExpr ONull _) = True
  testNull _ = False

instance HasLocation LiteralExpr where
  getLocation (LiteralExpr _ loc)     = loc
  setLocation (LiteralExpr o _  ) loc = LiteralExpr o loc
  delLocation (LiteralExpr o _  )     = LiteralExpr o LocationUnknown

instance B.Binary LiteralExpr MTab where
  put (LiteralExpr a loc) = B.put a >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting LiteralExpr"

instance B.HasPrefixTable LiteralExpr B.Byte MTab where
  prefixTable = B.bindPrefixTable B.prefixTable $ \o -> LiteralExpr o <$> B.get

instance Executable LiteralExpr (Maybe Object) where { execute (LiteralExpr o _) = return (Just o) }

instance RefReducible LiteralExpr where { reduceToRef (LiteralExpr o _) = reduceToRef o }

instance ObjectClass LiteralExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass LiteralExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> defDeref execute

----------------------------------------------------------------------------------------------------

data AST_Literal = AST_Literal Object Location deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Literal where
  rnf (AST_Literal a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_Literal where
  nullValue = AST_Literal ONull LocationUnknown
  testNull (AST_Literal ONull _) = True
  testNull _                     = False

instance HasLocation AST_Literal where
  getLocation (AST_Literal _ loc)     = loc
  setLocation (AST_Literal a _  ) loc = AST_Literal a loc
  delLocation (AST_Literal a _  )     = AST_Literal a lu

instance PPrintable AST_Literal where
  pPrint (AST_Literal o _  ) = pPrint o

instance PrecedeWithSpace AST_Literal where
  precedeWithSpace (AST_Literal _ _) = True

instance ToDaoStructClass AST_Literal where
  toDaoStruct = ask >>= \o -> case o of
    AST_Literal o loc -> renameConstructor "Literal" >> "obj" .= o >> putLocation loc

instance FromDaoStructClass AST_Literal where
  fromDaoStruct = constructor "Literal" >> return AST_Literal <*> req "obj" <*> location

instance HasRandGen AST_Literal where
  randO = _randTrace "AST_Literal" $ scramble $
    return AST_Literal <*> fmap unlimitObject defaultO <*> no
  defaultO = randO

instance Intermediate LiteralExpr AST_Literal where
  toInterm   (AST_Literal a loc) = [LiteralExpr] <*> [a] <*> [loc]
  fromInterm (LiteralExpr a loc) = [AST_Literal] <*> [a] <*> [loc]

instance ObjectClass AST_Literal where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Literal where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

newtype DotNameExpr = DotNameExpr{ undotNameExpr :: Name } deriving (Eq, Ord, Show, Typeable)

instance NFData DotNameExpr where { rnf (DotNameExpr n) = deepseq n () }

instance B.Binary DotNameExpr MTab where { put (DotNameExpr n) = B.put n; get = DotNameExpr <$> B.get; }

instance PPrintable DotNameExpr where { pPrint (DotNameExpr n) = pPrint n }

instance ObjectClass DotNameExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass DotNameExpr where
  haskellDataInterface = interface (DotNameExpr undefined) $ do
    autoDefEquality >> autoDefBinaryFmt >> autoDefPPrinter

-- | A 'DotName' is simply a ".name" expression in the Dao language. It is a component of the
-- 'DotLabelExpr' and 'AST_DotLabel' data types.
data AST_DotName = AST_DotName (Com ()) Name deriving (Eq, Ord, Show, Typeable)

getDotNameAST :: AST_DotName -> Name
getDotNameAST (AST_DotName _ n) = n

instance NFData AST_DotName where { rnf (AST_DotName a b) = deepseq a $! deepseq b () }

instance PPrintable AST_DotName where
  pPrint (AST_DotName c n) = pInline [pPrintComWith (\ () -> pString ".") c, pPrint n]

instance ToDaoStructClass AST_DotName where
  toDaoStruct = ask >>= \ (AST_DotName coms n) -> do
    renameConstructor "DotName"
    void $ case coms of
      Com () -> "name" .= n
      coms   -> "comments" .= coms >> "name" .= n

instance FromDaoStructClass AST_DotName where
  fromDaoStruct = constructor "DotName" >>
    return AST_DotName <*> (maybe (Com ()) id <$> opt "comments") <*> req "name"

instance Intermediate DotNameExpr AST_DotName where
  toInterm   (AST_DotName _ n) = [DotNameExpr n]
  fromInterm (DotNameExpr   n) = [AST_DotName (Com ()) n]

instance ObjectClass AST_DotName where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_DotName where
  haskellDataInterface = interface (AST_DotName (Com ()) undefined) $ do
    autoDefEquality >> autoDefPPrinter >> autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | The intermediate form of 'AST_DotLabel'.
data DotLabelExpr = DotLabelExpr DotNameExpr [DotNameExpr] Location 
  deriving (Eq, Ord, Show, Typeable)

instance NFData DotLabelExpr where
  rnf (DotLabelExpr n nm loc) = deepseq n $! deepseq nm $! deepseq loc ()

instance HasLocation DotLabelExpr where
  getLocation (DotLabelExpr _ _  loc)     = loc
  setLocation (DotLabelExpr n nx _  ) loc = DotLabelExpr n nx loc
  delLocation (DotLabelExpr n nx _  )     = DotLabelExpr n nx LocationUnknown

instance B.Binary DotLabelExpr MTab where
  put (DotLabelExpr n nx loc) = B.prefixByte 0x81 $ B.put n >> B.put nx >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting DotLabelExpr"

instance B.HasPrefixTable DotLabelExpr Word8 MTab where
  prefixTable = B.mkPrefixTableWord8 "DotLabelExpr" 0x81 0x81 $
    [return DotLabelExpr <*> B.get <*> B.get <*> B.get]

instance PPrintable DotLabelExpr where { pPrint = pPrintInterm }

dotLabelToRefExpr :: DotLabelExpr -> ReferenceExpr
dotLabelToRefExpr (DotLabelExpr (DotNameExpr n) nx loc) =
  ReferenceExpr UNQUAL n (loop nx) loc where
    loop nx = case nx of
      []                 -> NullRefExpr
      (DotNameExpr n):nx -> DotRefExpr n (loop nx) LocationUnknown

refToDotLabelExpr :: ReferenceExpr -> Maybe (DotLabelExpr, Maybe ObjListExpr)
refToDotLabelExpr o = case o of
  ReferenceExpr UNQUAL n suf loc ->
    loop (\nx loc ol -> (DotLabelExpr (DotNameExpr n) nx loc, ol)) [] loc suf
  _ -> mzero
  where
    loop f nx loc suf = case suf of
      NullRefExpr                 -> return (f nx loc Nothing)
      DotRefExpr   n  suf    loc' -> loop f (nx++[DotNameExpr n]) (loc<>loc') suf
      FuncCallExpr ol NullRefExpr -> return (f nx loc (Just ol))
      _                           -> mzero

dotLabelToNameList :: DotLabelExpr -> [Name]
dotLabelToNameList (DotLabelExpr n nx _) = map undotNameExpr (n:nx)

instance ObjectClass DotLabelExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass DotLabelExpr where
  haskellDataInterface = interface (DotLabelExpr undefined [] LocationUnknown) $ do
    autoDefEquality >> autoDefBinaryFmt >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

-- | This is a list of 'Dao.String.Name's separated by dots. It is a pseudo-reference used to denote
-- things like constructor names in 'InitExpr', or setting the logical names of "import" modules
-- statements. It is basically a list of 'Dao.String.Name's that always has at least one element.
data AST_DotLabel = AST_DotLabel Name [AST_DotName] Location deriving (Eq, Ord, Show, Typeable)

instance NFData AST_DotLabel where
  rnf (AST_DotLabel n nx loc) = deepseq n $! deepseq nx $! deepseq loc ()

instance HasLocation AST_DotLabel where
  getLocation (AST_DotLabel _ _  loc)     = loc
  setLocation (AST_DotLabel n nx _  ) loc = AST_DotLabel n nx loc
  delLocation (AST_DotLabel n nx _  )     = AST_DotLabel n nx LocationUnknown

instance PPrintable AST_DotLabel where
  pPrint (AST_DotLabel n nx _) = pWrapIndent $ pPrint n : map pPrint nx

instance HasRandGen AST_DotLabel where
  randO = return AST_DotLabel <*> randO <*> randListOf 0 3 (return AST_DotName <*> randO <*> randO) <*> no
  defaultO = randO

instance Intermediate DotLabelExpr AST_DotLabel where
  toInterm   (AST_DotLabel              n  nx loc) = [DotLabelExpr] <*> [DotNameExpr n] <*> [nx >>= ti] <*> [loc]
  fromInterm (DotLabelExpr (DotNameExpr n) nx loc) = [AST_DotLabel] <*>             [n] <*> [nx >>= fi] <*> [loc]

instance ToDaoStructClass AST_DotLabel where
  toDaoStruct = ask >>= \ (AST_DotLabel n nx loc) -> renameConstructor "DotLabel" >>
    "head" .= n >> "tail" .= OList (map obj nx) >> putLocation loc >> return ()

instance FromDaoStructClass AST_DotLabel where
  fromDaoStruct = do
    constructor "DotLabel"
    let convert o = case sequence (map fromObj o) of
          Nothing -> fail "\"tail\" field must contain a list of \"#DotName\" data types."
          Just ox -> return ox
    return AST_DotLabel <*> req "head" <*> (req "tail" >>= convert) <*> location

dotLabelToRefAST :: AST_DotLabel -> AST_Reference
dotLabelToRefAST (AST_DotLabel n nx loc) = AST_Reference UNQUAL [] n (loop nx) loc where
  loop nx = case nx of
    []                   -> AST_RefNull
    (AST_DotName c n):nx -> AST_DotRef c n (loop nx) LocationUnknown

refToDotLabelAST :: AST_Reference -> Maybe (AST_DotLabel, Maybe AST_ObjList)
refToDotLabelAST o = case o of
  AST_Reference UNQUAL _ n suf loc -> loop (\nx loc ol -> (AST_DotLabel n nx loc, ol)) [] loc suf
  _                                -> mzero
  where
    loop f nx loc suf = case suf of
      AST_RefNull                 -> return (f nx loc Nothing)
      AST_DotRef c n  suf    loc' -> loop f (nx++[AST_DotName c n]) (loc<>loc') suf
      AST_FuncCall ol AST_RefNull -> return (f nx loc (Just ol))
      _                           -> mzero

instance ObjectClass AST_DotLabel where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_DotLabel where
  haskellDataInterface = interface (AST_DotLabel undefined [] LocationUnknown) $ do
    autoDefEquality >> autoDefPPrinter >> autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data ReferenceExpr
  = RefObjectExpr ParenExpr         RefSuffixExpr Location
  | ReferenceExpr RefQualifier Name RefSuffixExpr Location
    -- ^ reference suffixed by square brackets or round brackets. If the 3rd parameter is False, it
    -- is suffixed by square brackets, and True means suffixed by round brackets. Square brackets
    -- indicates an indexing expression, round brackets indicates a function call.
  deriving (Eq, Ord, Show, Typeable)

instance NFData ReferenceExpr where
  rnf (RefObjectExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ReferenceExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue ReferenceExpr where
  nullValue = RefObjectExpr nullValue NullRefExpr LocationUnknown
  testNull (RefObjectExpr a NullRefExpr LocationUnknown) = testNull a
  testNull _                                             = False

instance HasLocation ReferenceExpr where
  getLocation o     = case o of
    RefObjectExpr _ _   loc -> loc
    ReferenceExpr _ _ _ loc -> loc
  setLocation o loc = case o of
    RefObjectExpr a b _     -> RefObjectExpr a b   loc
    ReferenceExpr a b c loc -> ReferenceExpr a b c loc
  delLocation o    = case o of
    RefObjectExpr a b _     -> RefObjectExpr (delLocation a) b   LocationUnknown
    ReferenceExpr a b c _   -> ReferenceExpr              a  b c LocationUnknown

instance PPrintable ReferenceExpr where { pPrint = pPrintInterm }

-- binary 0x3C 0x42 ReferenceExpr-->Reference
instance B.Binary ReferenceExpr MTab where
  put qref = case qref of
    ReferenceExpr q n r loc -> prefix q $ B.put n >> B.put r >> B.put loc where
      prefix q = B.prefixByte $ case q of
        { UNQUAL -> 0x48; LOCAL -> 0x49; CONST -> 0x4A; STATIC -> 0x4B; GLOBAL -> 0x4C; GLODOT -> 0x4D; }
    RefObjectExpr o r loc -> B.putWord8 0x4E >> B.put o >> B.put r >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting Reference"

instance B.HasPrefixTable ReferenceExpr Word8 MTab where
  prefixTable = B.mkPrefixTableWord8 "ReferenceExpr" 0x48 0x4E $
    [ f UNQUAL, f LOCAL, f CONST, f STATIC, f GLOBAL, f GLODOT
    , return RefObjectExpr <*> B.get <*> B.get <*> B.get
    ] where { f q = return (ReferenceExpr q) <*> B.get <*> B.get <*> B.get }

instance Executable ReferenceExpr (Maybe Object) where
  execute = reduceToRef >=> fmap (fmap snd) . referenceLookup

instance RefReducible ReferenceExpr where
  reduceToRef o = _setObjectExprError (ObjSingleExpr $ PlainRefExpr o) $ case o of
    RefObjectExpr o ref    _ -> return refAppendSuffix <*> reduceToRef o <*> execute ref
    ReferenceExpr q nm ref _ -> return (Reference q nm) <*> execute ref

instance ObjectClass ReferenceExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ReferenceExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> defDeref execute

----------------------------------------------------------------------------------------------------

data AST_Reference
  = AST_RefObject AST_Paren                   AST_RefSuffix Location
  | AST_Reference RefQualifier [Comment] Name AST_RefSuffix Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AST_Reference where
  rnf (AST_RefObject a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_Reference a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()

instance HasLocation AST_Reference where
  getLocation o     = case o of
    AST_RefObject _ _     loc -> loc
    AST_Reference _ _ _ _ loc -> loc
  setLocation o loc = case o of
    AST_RefObject a b     _   -> AST_RefObject a b     loc
    AST_Reference a b c d _   -> AST_Reference  a b c d loc
  delLocation o     = case o of
    AST_RefObject a b     _   -> AST_RefObject (delLocation a) (delLocation b) LocationUnknown
    AST_Reference a b c d _   -> AST_Reference a b c (delLocation d) LocationUnknown

instance PPrintable AST_Reference where
  pPrint o = pInline $ case o of
    AST_RefObject  o           ref _ -> [pPrint o, pPrint ref]
    AST_Reference  q coms name ref _ -> concat $
      [ if q==UNQUAL then [] else [pPrint q, pString " "]
      , [pPrint coms, pUStr (toUStr name), pPrint ref]
      ]

instance PrecedeWithSpace AST_Reference where { precedeWithSpace _ = True }

instance ToDaoStructClass AST_Reference where
  toDaoStruct = ask >>= \o -> case o of
    AST_RefObject o           ref loc -> renameConstructor "ParenExpr" >>
      "paren" .= o >> "suffix" .= ref >> putLocation loc
    AST_Reference  q coms name ref loc -> renameConstructor "Reference" >>
      "qualifier" .= q >> putComments coms >> "name" .= name >> "suffix" .= ref >> putLocation loc

instance FromDaoStructClass AST_Reference where
  fromDaoStruct = msum $
    [ constructor "ParenExpr" >>
        return AST_RefObject <*> req "paren" <*> req "suffix" <*> location
    , constructor "Reference" >>
        return AST_Reference <*> req "qualifier"
          <*> comments <*> req "name" <*> req "suffix" <*> location
    ]

instance HasRandGen AST_Reference where
  randO = _randTrace "AST_Reference" $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ scramble $ return AST_Reference <*> randO <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_RefObject <*> randO <*> randO <*> no
    ]
  defaultO = _randTrace "D.AST_Reference" runDefaultChoice
  defaultChoice = randChoiceList $
    [ return AST_Reference <*> defaultO <*> defaultO <*> defaultO <*> defaultO <*> no
    , return AST_RefObject <*> defaultO <*> defaultO <*> no
    ]

instance Intermediate ReferenceExpr AST_Reference where
  toInterm ast = case ast of
    AST_RefObject paren    ref loc -> [RefObjectExpr] <*> ti paren <*> ti ref            <*> [loc]
    AST_Reference q _ name ref loc -> [ReferenceExpr] <*> [q]      <*> [name] <*> ti ref <*> [loc]
  fromInterm o = case o of
    RefObjectExpr paren  ref loc -> [AST_RefObject] <*> fi paren <*> fi ref <*> [loc]
    ReferenceExpr q name ref loc ->
      [AST_Reference] <*> [q] <*> [[]] <*> [name]  <*> fi ref <*> [loc]

instance ObjectClass AST_Reference where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Reference where
  haskellDataInterface = interface (AST_RefObject nullValue nullValue nullValue) $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct >> defDeref (msum . map execute . toInterm)

----------------------------------------------------------------------------------------------------

data RefPrefixExpr
  = PlainRefExpr  ReferenceExpr
  | RefPrefixExpr RefPfxOp RefPrefixExpr Location
  deriving (Eq, Ord, Typeable, Show)

-- | Eliminate composed DEREF-REF operations
-- > @$a == a
cleanupRefPrefixExpr :: RefPrefixExpr -> RefPrefixExpr
cleanupRefPrefixExpr o =
  case o of { RefPrefixExpr DEREF (RefPrefixExpr REF o _) _ -> cleanupRefPrefixExpr o; _ -> o; }

instance NFData RefPrefixExpr where
  rnf (RefPrefixExpr a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (PlainRefExpr  a    ) = deepseq a ()

instance HasNullValue RefPrefixExpr where
  nullValue = PlainRefExpr nullValue
  testNull (PlainRefExpr a) = testNull a
  testNull _ = False

instance HasLocation RefPrefixExpr where
  getLocation o     = case o of
    PlainRefExpr      o -> getLocation o
    RefPrefixExpr _ _ o -> o
  setLocation o loc = case o of
    PlainRefExpr  a     -> PlainRefExpr $ setLocation a loc
    RefPrefixExpr a b _ -> RefPrefixExpr a b loc
  delLocation o     = case o of
    PlainRefExpr  a     -> PlainRefExpr $ delLocation a
    RefPrefixExpr a b _ -> RefPrefixExpr a (delLocation b) lu

-- binary 0x52 0x53
instance B.Binary RefPrefixExpr MTab where
  put o = case o of
    PlainRefExpr  a     -> B.put a
    RefPrefixExpr a b z -> let f = B.put b >> B.put z in case a of
      REF   -> B.prefixByte 0x52 f
      DEREF -> B.prefixByte 0x53 f
  get = B.word8PrefixTable <|> fail "expecting RefPrefixExpr"

instance B.HasPrefixTable RefPrefixExpr Word8 MTab where
  prefixTable = fmap PlainRefExpr B.prefixTable <>
    (B.mkPrefixTableWord8 "RefPrefixExpr" 0x52 0x53 $
      let f q = return (RefPrefixExpr q) <*> B.get <*> B.get in [f REF, f DEREF])

instance Executable RefPrefixExpr (Maybe Object) where
  execute o = _setObjectExprError (ObjSingleExpr o) $ do
    o <- reduceToRef o <|>
      execThrow (obj [obj "expression does not reduce to valid reference value"])
    fmap snd <$> referenceLookup o

instance RefReducible RefPrefixExpr where
  reduceToRef expr = _setObjectExprError (ObjSingleExpr expr) $ loop expr where
    err = execThrow . OList
    loop o = case o of
      PlainRefExpr o         -> reduceToRef o >>= \o -> case o of
        RefObject  o NullRef -> case o of
          ORef r -> return r
          o      -> return (RefObject o NullRef)
        o -> return o
      RefPrefixExpr op o _ -> case op of
        REF   -> loop o >>= \o -> case o of
          RefObject o NullRef  -> case o of
            OString s -> refStr s
            ORef    r -> return (RefWrapper r) 
            r         -> return (RefObject  r NullRef)
          o -> return (RefWrapper o)
        DEREF -> loop o >>= \r -> case r of
          RefWrapper r    -> return r
          RefObject o NullRef -> case o of
            OString s -> refStr s >>= deref
            ORef    r -> deref r
            OHaskell (HaskellData d ifc) -> case objDereferencer ifc of
              Nothing    -> err [obj "cannot dereference", o]
              Just deref -> do
                o <- deref d >>= maybe (err [obj "reference object evaluates to void", o]) return
                case o of
                  ORef r -> return r
                  o      -> return (RefObject o NullRef)
            o -> err [obj "cannot dereference object of type", obj (typeOfObj o)]
          o -> deref o
    deref r = do
      (r, o) <- fmap (maybe (Just r, Nothing) (\ (r, o) -> (Just r, Just o))) $ referenceLookup r
      let cantDeref msg = err $
            concat [[obj "cannot dereference"], maybe [] (return . obj) r, [obj msg]]
      case o of
        Nothing -> cantDeref "evaluates to void"
        Just  o -> execute (asReference o) <|> return (RefObject o NullRef)
    refStr s = case referenceFromUStr s of
      Nothing -> err [obj "string value cannot be parsed as reference", obj s]
      Just  r -> return r

instance ObjectClass RefPrefixExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass RefPrefixExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_RefPrefix
  = AST_PlainRef  AST_Reference
  | AST_RefPrefix RefPfxOp [Comment] AST_RefPrefix Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_RefPrefix where
  rnf (AST_PlainRef  a      ) = deepseq a ()
  rnf (AST_RefPrefix a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d  ()

instance HasLocation AST_RefPrefix where
  getLocation o     = case o of
    AST_PlainRef  a         -> getLocation a
    AST_RefPrefix _ _ _ loc -> loc
  setLocation o loc = case o of
    AST_PlainRef  a         -> AST_PlainRef $ setLocation a loc
    AST_RefPrefix a b c _   -> AST_RefPrefix a b c loc
  delLocation o     = case o of
    AST_PlainRef  a         -> AST_PlainRef $ delLocation a
    AST_RefPrefix a b c _   -> AST_RefPrefix a b (delLocation c) LocationUnknown

instance PPrintable AST_RefPrefix where
  pPrint o = case o of
    AST_PlainRef o                   -> pPrint o
    AST_RefPrefix ariOp coms objXp _ -> pWrapIndent [pPrint ariOp, pPrint coms, pPrint objXp]

instance PrecedeWithSpace AST_RefPrefix where
  precedeWithSpace o = case o of
    AST_PlainRef _ -> True
    _              -> False

instance ToDaoStructClass AST_RefPrefix where
  toDaoStruct = ask >>= \ (AST_RefPrefix a b c loc) -> renameConstructor "RefPrefix" >>
    "op" .= a >> putComments b >> "expr" .= c >> putLocation loc

instance FromDaoStructClass AST_RefPrefix where
  fromDaoStruct = constructor "RefPrefix" >>
    return AST_RefPrefix <*> req "op" <*> req "expr" <*> req "expr" <*> location

instance HasRandGen AST_RefPrefix where
  randO = _randTrace "AST_RefPrefix" $ recurse $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_PlainRef  <*> randO
    , return AST_RefPrefix <*> randO <*> randO <*> randO <*> no
    ]
  defaultO = _randTrace "D.AST_RefPrefix" runDefaultChoice
  defaultChoice = randChoiceList $
    [ AST_PlainRef <$> defaultO
    , return AST_RefPrefix <*> defaultO <*> defaultO <*> (AST_PlainRef <$> defaultO) <*> no
    ]

instance Intermediate RefPrefixExpr AST_RefPrefix where
  toInterm ast = case ast of
    AST_PlainRef  a         -> PlainRefExpr <$> toInterm a
    AST_RefPrefix a _ c loc -> [RefPrefixExpr] <*> [a] <*> toInterm c <*> [loc]
  fromInterm o = case o of
    PlainRefExpr  a       -> AST_PlainRef <$> fromInterm a
    RefPrefixExpr a c loc -> [AST_RefPrefix] <*> [a] <*> [[]] <*> fromInterm c <*> [loc]

instance ObjectClass AST_RefPrefix where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_RefPrefix where
  haskellDataInterface =
    interface (AST_PlainRef $ AST_RefObject nullValue nullValue nullValue) $ do
      autoDefEquality >> autoDefOrdering >> autoDefPPrinter
      autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data RuleFuncExpr
  = LambdaExpr    ParamListExpr CodeBlock Location
  | FuncExpr Name ParamListExpr CodeBlock Location
  | RuleExpr      RuleHeadExpr  CodeBlock Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData RuleFuncExpr where
  rnf (LambdaExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (FuncExpr   a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (RuleExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue RuleFuncExpr where
  nullValue = LambdaExpr nullValue nullValue LocationUnknown
  testNull (RuleExpr a b _) = testNull a && testNull b
  testNull _                = False

instance HasLocation RuleFuncExpr where
  getLocation o = case o of
    LambdaExpr _ _   o -> o
    FuncExpr   _ _ _ o -> o
    RuleExpr   _ _   o -> o
  setLocation o loc = case o of
    LambdaExpr a b   _ -> LambdaExpr a b   loc
    FuncExpr   a b c _ -> FuncExpr   a b c loc
    RuleExpr   a b   _ -> RuleExpr   a b   loc
  delLocation o = case o of
    LambdaExpr a b   _ -> LambdaExpr (fd a) (fd b)        lu
    FuncExpr   a b c _ -> FuncExpr       a  (fd b) (fd c) lu
    RuleExpr   a b   _ -> RuleExpr       a  (fd b)        lu

instance PPrintable RuleFuncExpr where { pPrint = pPrintInterm }

-- binary 0x74 0x76
instance B.Binary RuleFuncExpr MTab where
  put o = case o of
    LambdaExpr a b   z -> B.prefixByte 0x74 $ B.put a >> B.put b >> B.put z
    FuncExpr   a b c z -> B.prefixByte 0x75 $ B.put a >> B.put b >> B.put c >> B.put z
    RuleExpr   a b   z -> B.prefixByte 0x76 $ B.put a >> B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting RuleFuncExpr"

instance B.HasPrefixTable RuleFuncExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RuleFuncExpr" 0x74 0x76 $
    [ return LambdaExpr <*> B.get <*> B.get <*> B.get
    , return FuncExpr   <*> B.get <*> B.get <*> B.get <*> B.get
    , return RuleExpr   <*> B.get <*> B.get <*> B.get
    ]

instance Executable RuleFuncExpr (Maybe Object) where
  execute o = case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    LambdaExpr params scrpt _ -> do
      exec <- setupCodeBlock scrpt
      let callableCode = CallableCode{argsPattern=params, codeSubroutine=exec, returnType=nullValue}
      return $ Just $ new callableCode
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    FuncExpr name params scrpt _ -> do
      exec <- setupCodeBlock scrpt
      let callableCode = CallableCode{argsPattern=params, codeSubroutine=exec, returnType=nullValue}
      let o = Just $ new callableCode
      store <- asks execStack
      storeUpdate store name (return . const o)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    RuleExpr rs scrpt _ -> do
      params <- execute rs
      exec   <- setupCodeBlock scrpt
      globs  <- forM params $ \param -> do
        case readsPrec 0 $ uchars param of
          [(pat, "")] -> do
            -- TODO: tokenize the pattern Single's with the 'programTokenizer'
            return $ parseOverSingles pat (fmap (OString . ustr) . simpleTokenizer)
          _           -> execThrow $ obj [obj "cannot parse pattern expression:", obj param]
      return $ Just $ new $ GlobAction globs exec
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass RuleFuncExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass RuleFuncExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

data AST_RuleFunc
  = AST_Lambda              (Com AST_ParamList)  AST_CodeBlock Location
  | AST_Func [Comment] Name (Com AST_ParamList)  AST_CodeBlock Location
  | AST_Rule                (Com AST_RuleHeader) AST_CodeBlock Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_RuleFunc where
  rnf (AST_Lambda a b c    ) = deepseq a $! deepseq b $! deepseq c () 
  rnf (AST_Func   a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()
  rnf (AST_Rule   a b c    ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_RuleFunc where
  nullValue = AST_Lambda nullValue nullValue LocationUnknown
  testNull (AST_Lambda a b _) = testNull a && testNull b
  testNull _                = False

instance HasLocation AST_RuleFunc where
  getLocation o = case o of
    AST_Lambda _ _     o -> o
    AST_Func   _ _ _ _ o -> o
    AST_Rule   _ _     o -> o
  setLocation o loc = case o of
    AST_Lambda a b     _ -> AST_Lambda a b     loc
    AST_Func   a b c d _ -> AST_Func   a b c d loc
    AST_Rule   a b     _ -> AST_Rule   a b     loc
  delLocation o = case o of                            
    AST_Lambda a b     _ -> AST_Lambda (fd  a) (fd  b)                 lu
    AST_Func   a b c d _ -> AST_Func        a       b  (fd  c) (fd  d) lu
    AST_Rule   a b     _ -> AST_Rule   (fd  a) (fd  b)                 lu

instance PPrintable AST_RuleFunc where
  pPrint expr = case expr of
    AST_Lambda         ccNmx   xcObjXp     _ ->
      pPrintSubBlock (pInline [pString "function", pPrintComWith pPrint ccNmx]) xcObjXp
    AST_Func     co nm ccNmx   xcObjXp     _ ->
      pClosure (pInline [pString "function ", pPrint co, pPrint nm, pPrint ccNmx]) "{" "}" [pPrint xcObjXp]
    AST_Rule           ccNmx   xcObjXp     _ -> pClosure (pPrint ccNmx) "{" "}" [pPrint xcObjXp]

instance ToDaoStructClass AST_RuleFunc where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Lambda a b     loc -> nm "Lambda"   >> "params" .= a >> "block" .= b >> putLocation loc
    AST_Func   a b c d loc -> nm "Function" >>
      putComments a >> "name"  .= b >> "params" .= c >> "block" .= d >> putLocation loc
    AST_Rule   a b     loc -> nm "Rule" >> "params" .= a >> "block" .= b >> putLocation loc

instance FromDaoStructClass AST_RuleFunc where
  fromDaoStruct = msum $
    [ constructor "Lambda" >> return AST_Lambda <*> req "params" <*> req "block"  <*> location
    , constructor "Function" >>
        return AST_Func <*> comments <*> req "name" <*> req "params" <*> req "block" <*> location
    , constructor "Rule" >> return AST_Rule <*> req "params" <*> req "block" <*> location
    ]

instance HasRandGen AST_RuleFunc where
  randO = _randTrace "AST_RuleFunc" $ recurse $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ scramble $ return AST_Lambda <*> randO <*> randO <*> no
    , scramble $ return AST_Func   <*> randO <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_Rule   <*> randO <*> randO <*> no
    ]
  defaultO = _randTrace "D.AST_RuleFunc" runDefaultChoice
  defaultChoice = randChoiceList $
    [ scramble $ return AST_Lambda <*> defaultO <*> defaultO <*> no
    , scramble $ return AST_Func   <*> defaultO <*> defaultO <*> defaultO <*> defaultO <*> no
    , scramble $ return AST_Rule   <*> defaultO <*> defaultO <*> no
    ]

instance Intermediate RuleFuncExpr AST_RuleFunc where
  toInterm ast = case ast of
    AST_Lambda a b   loc -> [LambdaExpr]       <*> uc0 a <*> ti b <*> [loc]
    AST_Func _ a b c loc -> [FuncExpr] <*> [a] <*> uc0 b <*> ti c <*> [loc]
    AST_Rule   a b   loc -> [RuleExpr]         <*> uc0 a <*> ti b <*> [loc]
  fromInterm o = case o of
    LambdaExpr a b   loc -> [AST_Lambda]                <*> nc0 a <*> fi b <*> [loc]
    FuncExpr   a b c loc -> [AST_Func] <*> [[]] <*> [a] <*> nc0 b <*> fi c <*> [loc]
    RuleExpr   a b   loc -> [AST_Rule]                  <*> nc0 a <*> fi b <*> [loc]

instance ObjectClass AST_RuleFunc where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_RuleFunc where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data ObjectExpr
  = VoidExpr
  | ObjLiteralExpr  LiteralExpr
  | ObjSingleExpr   RefPrefixExpr
  | ObjRuleFuncExpr RuleFuncExpr
  | ArithPfxExpr                  ArithPfxOp      ObjectExpr   Location
  | InitExpr        DotLabelExpr  OptObjListExpr  ObjListExpr  Location
  | StructExpr      Name          OptObjListExpr               Location
  | MetaEvalExpr                                  CodeBlock    Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData ObjectExpr where
  rnf  VoidExpr                 = ()
  rnf (ObjLiteralExpr  a      ) = deepseq a ()
  rnf (ObjSingleExpr   a      ) = deepseq a $! ()
  rnf (ObjRuleFuncExpr a      ) = deepseq a $! ()
  rnf (ArithPfxExpr    a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (InitExpr        a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (StructExpr      a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (MetaEvalExpr    a b    ) = deepseq a $! deepseq b ()

instance HasNullValue ObjectExpr where
  nullValue = VoidExpr
  testNull VoidExpr = True
  testNull _        = False

instance HasLocation ObjectExpr where
  getLocation o = case o of
    VoidExpr                -> LocationUnknown
    ObjLiteralExpr        o -> getLocation o
    ObjSingleExpr         o -> getLocation o
    ObjRuleFuncExpr       o -> getLocation o
    ArithPfxExpr    _ _   o -> o
    InitExpr        _ _ _ o -> o
    StructExpr      _ _   o -> o
    MetaEvalExpr    _     o -> o
  setLocation o loc = case o of
    VoidExpr                -> VoidExpr
    ObjLiteralExpr  a       -> ObjLiteralExpr  (setLocation a loc)
    ObjSingleExpr   a       -> ObjSingleExpr   (setLocation a loc)
    ObjRuleFuncExpr a       -> ObjRuleFuncExpr (setLocation a loc)
    ArithPfxExpr    a b   _ -> ArithPfxExpr    a b   loc
    InitExpr        a b c _ -> InitExpr        a b c loc
    StructExpr      a b   _ -> StructExpr      a b   loc
    MetaEvalExpr    a     _ -> MetaEvalExpr    a     loc
  delLocation o = case o of
    VoidExpr                -> VoidExpr
    ObjLiteralExpr  a       -> ObjLiteralExpr  (fd a)
    ObjSingleExpr   a       -> ObjSingleExpr   (fd a)
    ObjRuleFuncExpr a       -> ObjRuleFuncExpr (fd a)
    ArithPfxExpr    a b   _ -> ArithPfxExpr        a  (fd b)        lu
    InitExpr        a b c _ -> InitExpr        (fd a) (fd b) (fd c) lu
    StructExpr      a b   _ -> StructExpr      (fd a) (fd b)        lu
    MetaEvalExpr    a     _ -> MetaEvalExpr    (fd a)               lu
    where
      lu = LocationUnknown
      fd :: HasLocation a => a -> a
      fd = delLocation

instance PPrintable ObjectExpr where { pPrint = pPrintInterm }

-- binary 0x60 0x65
instance B.Binary ObjectExpr MTab where
  put o = case o of
    ObjSingleExpr   a       -> B.put a
    ObjLiteralExpr  a       -> B.put a
    ObjRuleFuncExpr a       -> B.put a
    VoidExpr                -> B.putWord8   0x60
    ArithPfxExpr    a b   z -> B.prefixByte 0x61 $ B.put a >> B.put b >> B.put z
    InitExpr        a b c z -> B.prefixByte 0x62 $ B.put a >> B.put b >> B.put c >> B.put z
    StructExpr      a b   z -> B.prefixByte 0x63 $ B.put a >> B.put b >> B.put z
    MetaEvalExpr    a     z -> B.prefixByte 0x64 $ B.put a >> B.put z
  get = B.word8PrefixTable <|> fail "expecting ObjectExpr"

instance B.HasPrefixTable ObjectExpr B.Byte MTab where
  prefixTable = mconcat $
    [ ObjLiteralExpr  <$> B.prefixTable
    , ObjSingleExpr   <$> B.prefixTable
    , ObjRuleFuncExpr <$> B.prefixTable
    , B.mkPrefixTableWord8 "ObjectExpr" 0x60 0x64 $
        [ return VoidExpr
        , return ArithPfxExpr <*> B.get <*> B.get <*> B.get
        , return InitExpr     <*> B.get <*> B.get <*> B.get <*> B.get
        , return StructExpr   <*> B.get <*> B.get <*> B.get
        , return MetaEvalExpr <*> B.get <*> B.get
        ]
    ]

instance Executable ObjectExpr (Maybe Object) where
  execute o = _setObjectExprError o $ case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    VoidExpr -> return Nothing
      -- 'VoidExpr's only occur in return statements. Returning 'ONull' where nothing exists is
      -- probably the most intuitive thing to do on an empty return statement.
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ObjLiteralExpr  o -> execute o
    ObjSingleExpr   o -> execute o
    ObjRuleFuncExpr o -> execute o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ArithPfxExpr op expr loc -> do
      expr <- execute expr >>= fmap snd . maybeDerefObject >>=
        checkVoid loc ("operand to prefix operator "++show op)
      execute $ fmap Just (evalArithPrefixOp op expr)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    InitExpr ref bnds initMap _ -> Just <$> _evalInit (dotLabelToRefExpr ref) bnds initMap
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    StructExpr name (OptObjListExpr items) _ -> case items of
      Nothing -> return (Just $ OTree $ Nullary{ structName=name })
      Just (ObjListExpr items _) -> execNested M.empty $ do
        forM_ items $ \item -> case item of 
          AssignExpr{} -> execute item -- fill the local stack by executing each assignment
          _            -> _setScriptExprError (EvalObject item LocationUnknown) $
            fail "struct initializer is not an assignment expression"
        (LocalStore stack) <- asks execStack
        items <- liftIO $ (head . mapList) <$> readIORef stack
        return $ Just $ OTree $
          if M.null items
          then Nullary{ structName=name }
          else Struct{ fieldMap=items, structName=name }
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    MetaEvalExpr expr _ -> return $ Just $ new expr
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

_evalInit :: ReferenceExpr -> OptObjListExpr -> ObjListExpr -> Exec Object
_evalInit ref bnds initMap = do
  let (ObjListExpr items _) = initMap
  ref <- reduceToRef ref
  ref <- case ref of
    Reference UNQUAL name NullRef -> pure name
    ref -> execThrow $ obj [obj "cannot use reference as initializer", obj ref]
  ref <- pure $ toUStr ref
  bnds <- execute bnds >>= _reduceArgs "initializer parameters"
  let cantUseBounds msg = execThrow $ obj $
        [obj msg, obj "must be defined without bounds parameters", OList bnds]
  let initBacktracked = fail "backtracked during initialization"
  case uchars ref of
    "list" -> case bnds of
      [] -> execNested M.empty $ fmap OList $ execute initMap >>= _reduceArgs "list item"
      _  -> cantUseBounds "for list constructor"
    "dict" -> case bnds of
      [] -> execNested M.empty $ do
        mapM_ assignUnqualifiedOnly items
        (LocalStore stack) <- asks execStack
        liftIO $ (ODict . head . mapList) <$> readIORef stack
      _ -> cantUseBounds "for dict constructor"
    _ -> execGetObjTable ref >>= \tab -> case tab of
      Nothing  -> execThrow $ obj [obj "unknown object constructor", obj ref]
      Just tab -> do
        let make = OHaskell . flip HaskellData tab
        execNested M.empty $ msum $
          [ case objDictInit tab of
              Nothing           -> mzero
              Just (init, fold) -> init bnds >>= \o -> flip mplus initBacktracked $ do
                items <- forM items $ \item -> case item of
                  AssignExpr a op b _ -> do
                    let check msg expr = do
                          o <- execute expr
                          case o of
                            Just  o -> return o
                            Nothing -> execThrow $ obj $
                              [ obj $ msg++
                                  "-hand side of initalizer expression evaluates to void"
                              , new expr
                              ]
                    pure (,,) <*> check "left" a <*> pure op <*> check "right" b
                  EvalExpr arith -> execThrow $ obj $
                    [obj "expression does not assign a value to a key", new arith]
                make <$> fold o items
          , case objListInit tab of
              Nothing           -> mzero
              Just (init, fold) -> init bnds >>= \o -> flip mplus initBacktracked $ fmap make $
                execute initMap >>= _reduceArgs "initializer list" >>= fold o
          , execThrow $ obj [obj "cannot declare constant object of type", obj ref]
          ]


instance RefReducible ObjectExpr where
  reduceToRef o = _setObjectExprError o $ case o of
    ObjLiteralExpr r -> reduceToRef r
    ObjSingleExpr  r -> reduceToRef r
    o                -> execute o >>=
      maybe (fail "evaluates to null") (return . flip RefObject NullRef)

instance ObjectClass ObjectExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ObjectExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data AST_Object
  = AST_Void -- ^ Not a language construct, but used where an object expression is optional.
  | AST_ObjLiteral  AST_Literal
  | AST_ObjSingle   AST_RefPrefix
  | AST_ObjRuleFunc AST_RuleFunc
  | AST_ArithPfx    ArithPfxOp    [Comment]       AST_Object    Location
  | AST_Init        AST_DotLabel  AST_OptObjList  AST_ObjList   Location
  | AST_Struct      Name          AST_OptObjList                Location
  | AST_MetaEval                                  AST_CodeBlock Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Object where
  rnf AST_Void = ()
  rnf (AST_ObjLiteral  a      ) = deepseq a ()
  rnf (AST_ObjSingle   a      ) = deepseq a ()
  rnf (AST_ObjRuleFunc a      ) = deepseq a ()
  rnf (AST_ArithPfx    a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Init        a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Struct      a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_MetaEval    a b    ) = deepseq a $! deepseq b ()

instance HasNullValue AST_Object where
  nullValue = AST_Void
  testNull AST_Void = True
  testNull _        = False

instance HasLocation AST_Object where
  getLocation o = case o of
    AST_Void               -> LocationUnknown
    AST_ObjLiteral       o -> getLocation o
    AST_ObjSingle        o -> getLocation o
    AST_ObjRuleFunc      o -> getLocation o
    AST_ArithPfx _ _ _   o -> o
    AST_Init     _ _ _   o -> o
    AST_Struct   _ _     o -> o
    AST_MetaEval _       o -> o
  setLocation o loc = case o of
    AST_Void                  -> AST_Void
    AST_ObjLiteral  a         -> AST_ObjLiteral  (setLocation a loc)
    AST_ObjSingle   a         -> AST_ObjSingle   (setLocation a loc)
    AST_ObjRuleFunc a         -> AST_ObjRuleFunc (setLocation a loc)
    AST_ArithPfx    a b c   _ -> AST_ArithPfx    a b c   loc
    AST_Init        a b c   _ -> AST_Init        a b c   loc
    AST_Struct      a b     _ -> AST_Struct      a b     loc
    AST_MetaEval    a       _ -> AST_MetaEval    a       loc
  delLocation o = case o of                            
    AST_Void                  -> AST_Void
    AST_ObjLiteral  a         -> AST_ObjLiteral  (fd  a)
    AST_ObjSingle   a         -> AST_ObjSingle   (fd  a)
    AST_ObjRuleFunc a         -> AST_ObjRuleFunc (fd  a)
    AST_ArithPfx    a b c   _ -> AST_ArithPfx         a       b  (fd  c)         lu
    AST_Init        a b c   _ -> AST_Init                (fd  a) (fd  b) (fd  c) lu
    AST_Struct      a b     _ -> AST_Struct           a  (fd  b)                 lu
    AST_MetaEval    a       _ -> AST_MetaEval    (fd  a)                         lu

instance PPrintable AST_Object where
  pPrint expr = case expr of
    AST_Void                         -> return ()
    AST_ObjLiteral  o                -> pPrint o
    AST_ObjSingle   o                -> pPrint o
    AST_ObjRuleFunc o                -> pPrint o
    AST_ArithPfx    op coms objXp  _ -> pWrapIndent $
      [pPrint op, pPrint coms, pPrint objXp]
    AST_Init          ref objs     elems   _ ->
      pInline [pPrint ref, pPrintOptObjList "(" ", " ")" objs, pPrintObjList "{" ", " "}" elems]
    AST_Struct      nm itms        _ -> case itms of
      AST_OptObjList coms items -> do
        let name = pString $ '#' : uchars (toUStr nm)
        pPrint coms
        case items of
          Nothing -> name
          Just (AST_ObjList coms items _) -> do
            pPrint coms
            pList name "{" ", " "}" $ map pPrint items
    AST_MetaEval cObjXp                    _ -> pInline [pString "${", pPrint cObjXp, pString "}"]

instance PrecedeWithSpace AST_Object where
  precedeWithSpace o = case o of
    AST_Void         -> False
    AST_MetaEval{}   -> False
    AST_ObjSingle  o -> precedeWithSpace o
    _                -> True

instance ToDaoStructClass AST_Object where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Void                   -> makeNullary "Void"
    AST_ObjLiteral   a         -> innerToStruct a
    AST_ObjSingle    a         -> innerToStruct a
    AST_ObjRuleFunc  a         -> innerToStruct a
    AST_ArithPfx     a b c loc -> nm "ArithPrefix" >>
      "op" .= a >> putComments b >> "expr" .= c >> putLocation loc
    AST_Init         a b c loc -> nm "Init" >>
      "name" .= a >> "params" .= b >> "initList" .= c >> putLocation loc
    AST_Struct       a b   loc -> nm "Struct" >> "name" .= a >> "initList" .= b >> putLocation loc
    AST_MetaEval     a     loc -> nm "MetaEval" >> "block" .= a >> putLocation loc

instance FromDaoStructClass AST_Object where
  fromDaoStruct = msum $
    [ nullary "Void" >> return AST_Void
    , AST_ObjLiteral  <$> fromDaoStruct
    , AST_ObjSingle   <$> fromDaoStruct
    , AST_ObjRuleFunc <$> fromDaoStruct
    , constructor "ArithPrefix" >>
        pure AST_ArithPfx <*> req "op" <*> comments <*> req "expr" <*> location
    , constructor "Init" >>
        pure AST_Init     <*> req "name" <*> req "params" <*> req "initList" <*> location
    , constructor "Struct" >> pure AST_Struct <*> req "name" <*> req "initList" <*> location
    , constructor "MetaEval" >> pure AST_MetaEval <*> req "block" <*> location
    ]

instance HasRandGen AST_Object where
  randO = _randTrace "AST_Object" $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ AST_ObjLiteral  <$> randO
    , AST_ObjSingle   <$> randO
    , AST_ObjRuleFunc <$> randO
    , pure AST_ArithPfx <*> randO <*> randO <*> randO <*> no
    , pure AST_Init     <*> randO <*> randO <*> randO <*> no
    , pure AST_MetaEval <*> randO <*> no
    ]
  defaultO = _randTrace "D.AST_Object" runDefaultChoice
  defaultChoice = randChoiceList $
    [ AST_ObjLiteral  <$> defaultO
    , AST_ObjSingle   <$> defaultO
    , AST_ObjRuleFunc <$> defaultO
    , return AST_ArithPfx <*> defaultO <*> defaultO <*> (AST_ObjLiteral <$> defaultO) <*> no
    , return AST_Init     <*> defaultO <*> defaultO <*> defaultO <*> no
    , return AST_Struct   <*> defaultO <*> defaultO <*> no
    ]

instance HasRandGen [Com AST_Object] where
  randO = depthLimitedInt 24 >>= \x -> countNode $ randList 1 x

instance Intermediate ObjectExpr AST_Object where
  toInterm ast = case ast of
    AST_Void                  -> [VoidExpr       ]
    AST_ObjLiteral  a         ->  ObjLiteralExpr   <$> ti a
    AST_ObjSingle   a         -> [ObjSingleExpr  ] <*> ti a
    AST_ObjRuleFunc a         -> [ObjRuleFuncExpr] <*> ti a
    AST_ArithPfx    a _ c loc -> [ArithPfxExpr   ] <*>   [a]          <*> ti c <*> [loc]
    AST_Init        a b c loc -> [InitExpr       ] <*> ti a  <*> ti b <*> ti c <*> [loc]
    AST_Struct      a b   loc -> [StructExpr     ] <*>   [a] <*> ti b          <*> [loc]
    AST_MetaEval    a     loc -> [MetaEvalExpr   ] <*> ti a                    <*> [loc]
  fromInterm o = case o of
    VoidExpr                  -> [AST_Void       ]
    ObjLiteralExpr  a         ->  AST_ObjLiteral   <$> fi a
    ObjSingleExpr   a         ->  AST_ObjSingle    <$> fi a
    ObjRuleFuncExpr a         ->  AST_ObjRuleFunc  <$> fi a
    ArithPfxExpr    a b   loc -> [AST_ArithPfx   ] <*>   [a] <*> [[]] <*> fi b <*> [loc]
    InitExpr        a b c loc -> [AST_Init       ] <*> fi a  <*> fi b <*> fi c <*> [loc]
    StructExpr      a b   loc -> [AST_Struct     ] <*>   [a] <*> fi b          <*> [loc]
    MetaEvalExpr    a     loc -> [AST_MetaEval   ] <*> fi a                    <*> [loc]

instance ObjectClass AST_Object where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Object where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data ArithExpr
  = ObjectExpr ObjectExpr
  | ArithExpr  ArithExpr InfixOp ArithExpr Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData ArithExpr where
  rnf (ObjectExpr a      ) = deepseq a ()
  rnf (ArithExpr  a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue ArithExpr where
  nullValue = ObjectExpr nullValue
  testNull (ObjectExpr a) = testNull a
  testNull _ = False

instance HasLocation ArithExpr where
  getLocation o     = case o of
    ObjectExpr      o -> getLocation o
    ArithExpr _ _ _ o -> o
  setLocation o loc = case o of
    ObjectExpr  a     -> ObjectExpr (setLocation a loc)
    ArithExpr a b c _ -> ArithExpr a b c loc
  delLocation o     = case o of
    ObjectExpr  a     -> ObjectExpr (delLocation a)
    ArithExpr a b c _ -> ArithExpr (delLocation a) b (delLocation c) LocationUnknown

instance PPrintable ArithExpr where { pPrint = pPrintInterm }

-- binary 0x6A 
instance B.Binary ArithExpr MTab where
  put o = case o of
    ObjectExpr  a     -> B.put a
    ArithExpr a b c z -> B.prefixByte 0x6A $ B.put a >> B.put b >> B.put c >> B.put z
  get = B.word8PrefixTable <|> fail "expecting arithmetic expression"

instance B.HasPrefixTable ArithExpr B.Byte MTab where
  prefixTable = mappend (ObjectExpr <$> B.prefixTable) $
    B.mkPrefixTableWord8 "ArithExpr" 0x6A 0x6A $
      [pure ArithExpr <*> B.get <*> B.get <*> B.get <*> B.get]

instance Executable ArithExpr (Maybe Object) where
  execute o = case o of
    ObjectExpr o -> execute o
    ArithExpr left' op right' loc -> do
      let err1 msg = msg++"-hand operand of "++show op++ "operator "
          evalLeft   = execute left'  >>= checkVoid loc (err1 "left" )
          evalRight  = execute right' >>= checkVoid loc (err1 "right")
          derefLeft  = evalLeft  >>= derefObject
          derefRight = evalRight >>= derefObject
          logical isAndOp = fmap Just $ do
            left <- derefLeft >>= execute . objToBool
            if left
              then  if isAndOp then derefRight else return OTrue
              else  if isAndOp then return ONull else derefRight
      case op of
        AND -> logical True
        OR  -> logical False
        op  -> do
          (left, right) <- case op of
            ARROW -> liftM2 (,) derefLeft evalRight
            _     -> liftM2 (,) derefLeft derefRight
          execute (fmap Just $ evalInfixOp op left right)

instance RefReducible ArithExpr where
  reduceToRef o = case o of
    ObjectExpr o -> reduceToRef o
    o            -> _setScriptExprError (EvalObject (EvalExpr $ ObjArithExpr o) LocationUnknown) $ do
      exp <- execute o >>= maybe (fail "evaluates to null") return
      return $ RefObject exp NullRef

instance ObjectClass ArithExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ArithExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

data AST_Arith 
  = AST_Object AST_Object
  | AST_Arith  AST_Arith (Com InfixOp) AST_Arith Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Arith where
  rnf (AST_Object   a        ) = deepseq a ()
  rnf (AST_Arith a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue AST_Arith where
  nullValue = AST_Object nullValue
  testNull (AST_Object a) = testNull a
  testNull _ = False

instance HasLocation AST_Arith where
  getLocation o     = case o of
    AST_Object      o -> getLocation o
    AST_Arith _ _ _ o -> o
  setLocation o loc = case o of
    AST_Object   a    -> AST_Object (setLocation a loc)
    AST_Arith a b c _ -> AST_Arith a b c loc
  delLocation o     = case o of
    AST_Object   a    -> AST_Object (delLocation a)
    AST_Arith a b c _ -> AST_Arith (fd a) b (fd c) lu

instance PPrintable AST_Arith where
  pPrint o = case o of
    AST_Object o -> pPrint o
    AST_Arith objXp1 comAriOp objXp2 _ -> pWrapIndent [pPrint objXp1, pPrint comAriOp, pPrint objXp2]

instance PrecedeWithSpace AST_Arith where
  precedeWithSpace o = case o of
    AST_Object o       -> precedeWithSpace o
    AST_Arith  o _ _ _ -> precedeWithSpace o

instance ToDaoStructClass AST_Arith where
  toDaoStruct = ask >>= \o -> case o of
    AST_Object a         -> innerToStruct a
    AST_Arith  a b c loc -> renameConstructor "Arithmetic" >>
      "left" .= a >> "op" .= b >> "right" .= c >> putLocation loc

instance FromDaoStructClass AST_Arith where
  fromDaoStruct = msum $
    [ AST_Object <$> fromDaoStruct
    , constructor "Arithmetic" >>
        pure AST_Arith <*> req "left" <*> req "op" <*> req "right" <*> location
    ]

instance HasRandGen AST_Arith where
  randO = _randTrace "AST_Arith" $ countNode $ runRandChoice
  defaultChoice = randChoiceList $
    [ AST_Object <$> defaultO
    , return AST_Arith <*> (AST_Object <$> defaultO) <*> defaultO <*> (AST_Object <$> defaultO) <*> no
    ]
  defaultO = _randTrace "AST_Arith" runDefaultChoice
  randChoice = randChoiceList $
    [ AST_Object <$> randO
    , do  left <- AST_Object <$> randO
          x    <- getCurrentDepth
          ops  <- randListOf 0 (max 1 (4-x)) $
            pure (,) <*> randInfixOp <*> (AST_Object <$> randO)
          return $ foldPrec left ops
    ] where
      randInfixOp :: RandO (Com InfixOp, Int, Bool)
      randInfixOp = do
        (op, prec, assoc) <- runRandChoiceOf opGroups
        op <- randComWith (return op)
        return (op, prec, assoc)
      left  op = (True , op)
      right op = (False, op)
      opGroups :: RandChoice (InfixOp, Int, Bool)
      opGroups = randChoiceList $ map return $ do
        (precedence, (associativity, operators)) <- zip [1..] $ concat $
          [ map right [[OR], [AND], [EQUL, NEQUL]]
          , map left $
              [ [GTN, LTN, GTEQ, LTEQ], [SHL, SHR]
              , [ORB], [XORB], [ANDB]
              , [ADD, SUB], [MULT, DIV, MOD]
              ]
          , map right [[POW], [ARROW]]
          ]
        operator <- operators
        return (operator, precedence, associativity)
      bind left op right = AST_Arith left op right LocationUnknown
      foldPrec left ops = case ops of
        [] -> left
        ((op, prec, _), right):ops -> case scanRight prec right ops of
          (right, ops) -> foldPrec (bind left op right) ops
      scanRight prevPrec left ops = case ops of
        [] -> (left, [])
        ((op, prec, assoc), right):next -> 
          if prevPrec<prec || (prevPrec==prec && not assoc)
          then  case scanRight prec right next of
                  (right, next) -> scanRight prevPrec (bind left op right) next
          else  (left, ops)

instance Intermediate ArithExpr AST_Arith where
  toInterm o = case o of
    AST_Object  a       -> ObjectExpr <$> ti a
    AST_Arith a b c loc -> [ArithExpr ] <*> ti a <*> uc b <*> ti c <*> [loc]
  fromInterm o = case o of
    ObjectExpr  a       -> AST_Object <$> fi a
    ArithExpr a b c loc -> [AST_Arith ] <*> fi a <*> nc b <*> fi c <*> [loc]

instance ObjectClass AST_Arith where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Arith where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AssignExpr
  = EvalExpr   ObjTestExpr
  | AssignExpr ObjTestExpr UpdateOp AssignExpr Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AssignExpr where
  rnf (EvalExpr   a      ) = deepseq a ()
  rnf (AssignExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue AssignExpr where
  nullValue = EvalExpr nullValue
  testNull (EvalExpr a) = testNull a
  testNull _ = False

instance HasLocation AssignExpr where
  getLocation o     = case o of
    EvalExpr         o -> getLocation o
    AssignExpr _ _ _ o -> o
  setLocation o loc = case o of
    EvalExpr   a       -> EvalExpr  (setLocation a loc)
    AssignExpr a b c _ -> AssignExpr a b c loc
  delLocation o     = case o of
    EvalExpr   a       -> EvalExpr   (delLocation a)
    AssignExpr a b c _ -> AssignExpr (delLocation a) b (delLocation c) LocationUnknown

instance PPrintable AssignExpr where { pPrint = pPrintInterm }

-- binary 0x6F 
instance B.Binary AssignExpr MTab where
  put o = case o of
    EvalExpr   a       -> B.put a
    AssignExpr a b c z -> B.prefixByte 0x6F $ B.put a >> B.put b >> B.put c >> B.put z
  get = B.word8PrefixTable <|> fail "expecting AssignExpr"

instance B.HasPrefixTable AssignExpr B.Byte MTab where
  prefixTable = mappend (EvalExpr <$> B.prefixTable) $
    B.mkPrefixTableWord8 "AssignExpr" 0x6F 0x6F $
      [pure AssignExpr <*> B.get <*> B.get <*> B.get <*> B.get]

_executeAssignExpr
  :: (Reference -> UpdateOp -> Object -> Exec (Maybe Object))
  -> AssignExpr -> Exec (Maybe Object)
_executeAssignExpr update o = case o of
  EvalExpr   o -> execute o
  AssignExpr nm op expr loc -> do
    qref <- mplus (reduceToRef nm) $ execThrow $
      obj [obj $ "left-hand side of assignment expression is not a reference value"]
    newObj <- execute expr >>= checkVoid loc "right-hand side of assignment" >>= derefObject 
    update qref op newObj

instance Executable AssignExpr (Maybe Object) where
  execute = _executeAssignExpr $ \qref op newObj ->
    referenceUpdate qref (evalUpdateOp (Just qref) op newObj)

instance RefReducible AssignExpr where
  reduceToRef o = case o of
    EvalExpr   o -> reduceToRef o
    AssignExpr{} -> execute o >>=
      maybe (fail "left-hand side of assignment evaluates to null") reduceToRef

-- | This function works a bit like how 'execute' works on an 'AssignExpr' data type, but every
-- assignment is checked to make sure it is local or unqualified. Furthurmore, all assignments are
-- forced into the top of the local variable stack, already-defined vairables at higher points in
-- the local variable stack are not updated in place. This function is used to define items in
-- is one important difference: it is specifically modified to work for evaluation of 'InitExpr'
-- data types, for example in the Dao language expression: @a = dict {a=1, b=2};@ Using this
-- function instead of 'execute' will always assign variables in the top of the local variable
-- stack, regardless of whether the variable has been defined before. This makes it possible to
-- write Dao language statements like this: @a=1; a = dict {a=a, b=2};@ which would create a
-- dictionary @a = dict {a=1, b=2};@, because before the "dict{}" expression, "a" had a value of 1.
assignUnqualifiedOnly :: AssignExpr -> Exec (Maybe Object)
assignUnqualifiedOnly = _executeAssignExpr $ \qref op newObj -> case qref of
  Reference UNQUAL r NullRef -> do
    (LocalStore store) <- asks execStack
    oldObj <- liftIO $ stackLookup r <$> readIORef store
    newObj <- evalUpdateOp (Just qref) op newObj oldObj
    liftIO $ atomicModifyIORef store (stackUpdateTop (const newObj) r)
  _ -> execThrow $ obj [obj "cannot assign to reference", obj qref , obj "in current context"]

instance ObjectClass AssignExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AssignExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

data AST_Assign
  = AST_Eval   AST_ObjTest
  | AST_Assign AST_ObjTest (Com UpdateOp) AST_Assign Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Assign where
  rnf (AST_Eval   a      ) = deepseq a ()
  rnf (AST_Assign a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue AST_Assign where
  nullValue = AST_Eval  nullValue
  testNull (AST_Eval  a) = testNull a
  testNull _ = False

instance HasLocation AST_Assign where
  getLocation o = case o of
    AST_Eval         o -> getLocation o
    AST_Assign _ _ _ o -> o
  setLocation o loc = case o of
    AST_Eval         o -> AST_Eval  (setLocation o loc)
    AST_Assign a b c _ -> AST_Assign a b c loc
  delLocation o = case o of                            
    AST_Eval         o -> AST_Eval  (delLocation o)
    AST_Assign a b c _ -> AST_Assign (delLocation a) b (delLocation c) lu

instance PPrintable AST_Assign where
  pPrint expr = case expr of
    AST_Eval  eq -> pPrint eq
    AST_Assign objXp1 comUpdOp objXp2 _ -> pWrapIndent $
      [pPrint objXp1, pPrint comUpdOp, pPrint objXp2]

instance PrecedeWithSpace AST_Assign where
  precedeWithSpace o = case o of
    AST_Eval   o       -> precedeWithSpace o
    AST_Assign o _ _ _ -> precedeWithSpace o

instance ToDaoStructClass AST_Assign where
  toDaoStruct = ask >>= \o -> case o of
    AST_Eval o ->  innerToStruct o
    AST_Assign to op from loc -> renameConstructor "Assign" >>
      "to" .= to >> "op" .= op >> "from" .= from >> putLocation loc

instance FromDaoStructClass AST_Assign where
  fromDaoStruct = msum $
    [ AST_Eval <$> fromDaoStruct
    , pure AST_Assign <*> req "to" <*> req "op" <*> req "from" <*> location
    ]

instance HasRandGen AST_Assign where
  randO = _randTrace "AST_Assign" $ countNode $ recurse $ runRandChoice
  randChoice = randChoiceList $
    [ AST_Eval <$> randO
    , do ox <- randListOf 0 3 (pure (,) <*> randO <*> randO)
         o  <- randO
         return (foldr (\ (left, op) right -> AST_Assign left op right LocationUnknown) o ox)
    ]
  defaultO = _randTrace "D.AST_Assign" runDefaultChoice
  defaultChoice = randChoiceList $
    [ AST_Eval <$> defaultO
    , return AST_Assign <*> defaultO <*> defaultO <*> (AST_Eval <$> defaultO) <*> no
    ]

instance Intermediate AssignExpr AST_Assign where
  toInterm ast = case ast of
    AST_Eval   a         -> EvalExpr <$> ti a
    AST_Assign a b c loc -> [AssignExpr] <*> ti a <*> uc b <*> ti c <*> [loc]
  fromInterm o = case o of
    EvalExpr   a         -> AST_Eval <$> fi a
    AssignExpr a b c loc -> [AST_Assign] <*> fi a <*> nc b <*> fi c <*> [loc]

instance ObjectClass AST_Assign where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Assign where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A conditional expression of the form @a==b ? "yes" : "no"@
data ObjTestExpr
  = ObjArithExpr ArithExpr
  | ObjTestExpr  ArithExpr ArithExpr ArithExpr Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData ObjTestExpr where
  rnf (ObjArithExpr  a    ) = deepseq a ()
  rnf (ObjTestExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue ObjTestExpr where
  nullValue = ObjArithExpr nullValue
  testNull (ObjArithExpr a) = testNull a
  testNull _ = False

instance HasLocation ObjTestExpr where
  getLocation o = case o of
    ObjArithExpr      a   -> getLocation a
    ObjTestExpr _ _ _ loc -> loc
  setLocation o loc = case o of
    ObjArithExpr      a   -> ObjArithExpr (setLocation a loc)
    ObjTestExpr a b c _   -> ObjTestExpr a b c loc
  delLocation o     = case o of
    ObjArithExpr      a   -> ObjArithExpr (delLocation a)
    ObjTestExpr a b c _   ->
      ObjTestExpr (delLocation a) (delLocation b) (delLocation c) LocationUnknown

instance PPrintable ObjTestExpr where { pPrint = pPrintInterm }

instance B.Binary ObjTestExpr MTab where
  put o = case o of
    ObjArithExpr      a -> B.put a
    ObjTestExpr a b c d -> B.prefixByte 0x73 $ B.put a >> B.put b >> B.put c >> B.put d
  get = B.word8PrefixTable <|> fail "expecting ObjTestExpr"

instance B.HasPrefixTable ObjTestExpr Word8 MTab where
  prefixTable = fmap ObjArithExpr B.prefixTable <>
    (B.mkPrefixTableWord8 "ObjTestExpr" 0x73 0x73 $
      [return ObjTestExpr <*> B.get <*> B.get <*> B.get <*> B.get])

instance Executable ObjTestExpr (Maybe Object) where
  execute o = case o of
    ObjArithExpr      a -> execute a
    ObjTestExpr a b c _ ->
      execute a >>= checkVoid (getLocation a) "conditional expression evaluates to void" >>=
        execute . objToBool >>= \ok -> if ok then execute b else execute c

instance RefReducible ObjTestExpr where
  reduceToRef o = case o of
    ObjArithExpr o -> reduceToRef o
    ObjTestExpr{}  -> fmap (flip RefObject NullRef) $ execute o >>=
      checkVoid (getLocation o) "conditional expression assignment evaluates to void, not reference"

instance ObjectClass ObjTestExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass ObjTestExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

-- | A conditional expression of the form @a==b ? "yes" : "no"@
data AST_ObjTest
  = AST_ObjArith AST_Arith
  | AST_ObjTest  AST_Arith (Com ()) AST_Arith (Com ()) AST_Arith Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AST_ObjTest where
  rnf (AST_ObjArith  a        ) = deepseq a ()
  rnf (AST_ObjTest a b c d e f) =
    deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e $! deepseq f ()

instance HasNullValue AST_ObjTest where
  nullValue = AST_ObjArith nullValue
  testNull (AST_ObjArith a) = testNull a
  testNull _ = False

instance HasLocation AST_ObjTest where
  getLocation o     = case o of
    AST_ObjArith          a   -> getLocation a
    AST_ObjTest _ _ _ _ _ loc -> loc
  setLocation o loc = case o of
    AST_ObjArith          a   -> AST_ObjArith (setLocation a loc)
    AST_ObjTest a b c d e _   -> AST_ObjTest a b c d e loc
  delLocation o     = case o of
    AST_ObjArith          a   -> AST_ObjArith (delLocation a)
    AST_ObjTest a b c d e _   -> 
      AST_ObjTest (delLocation a) b (delLocation c) d (delLocation e) LocationUnknown

instance PPrintable AST_ObjTest where
  pPrint o = case o of
    AST_ObjArith  a         -> pPrint a
    AST_ObjTest a b c d e _ -> pWrapIndent $
      [ pPrint a
      , pPrintComWith (\ () -> pString " ? ") b, pPrint c
      , pPrintComWith (\ () -> pString " : ") d, pPrint e
      ]

instance PrecedeWithSpace AST_ObjTest where
  precedeWithSpace o = case o of
    AST_ObjArith          o -> precedeWithSpace o
    AST_ObjTest o _ _ _ _ _ -> precedeWithSpace o

instance HasRandGen AST_ObjTest where
  randO = _randTrace "AST_ObjTest" $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ AST_ObjArith <$> randO
    , let p = pure (Com ()) in
        recurse $ scramble $ return AST_ObjTest <*> randO <*> p <*> randO <*> p <*> randO <*> no
    ]
  defaultO = _randTrace "D.AST_ObjTest" $ AST_ObjArith <$> defaultO
  defaultChoice = randChoiceList [defaultO]

instance ToDaoStructClass AST_ObjTest where
  toDaoStruct = ask >>= \o -> case o of
    AST_ObjArith  a -> innerToStruct a
    AST_ObjTest a b c d e f -> do
      renameConstructor "ObjTest"
      "condition" .= a
      "quesMarkComs" .= b >> "action" .= c
      "colonComs"    .= d >> "alt"    .= e
      putLocation f

instance FromDaoStructClass AST_ObjTest where
  fromDaoStruct = msum $
    [ AST_ObjArith <$> fromDaoStruct
    , do  constructor "ObjTest"
          return AST_ObjTest
            <*> req "condition" 
            <*> (maybe (Com ()) id <$> opt "quesMarkComs") <*> req "action"
            <*> (maybe (Com ()) id <$> opt "colonComs"   ) <*> req "alt"
            <*> location
    ]

instance Intermediate ObjTestExpr AST_ObjTest where
  toInterm   o = case o of
    AST_ObjArith    o         ->  ObjArithExpr <$> ti o
    AST_ObjTest a _ b _ c loc -> [ObjTestExpr] <*> ti a <*> ti b <*> ti c <*> [loc]
  fromInterm o = case o of
    ObjArithExpr  o       -> AST_ObjArith <$> fi o
    ObjTestExpr a b c loc ->
      [AST_ObjTest] <*> fi a <*> [Com ()] <*> fi b <*> [Com ()] <*> fi c <*> [loc]

instance ObjectClass AST_ObjTest where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_ObjTest where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AttributeExpr
  = AttribDotNameExpr DotLabelExpr
  | AttribStringExpr  UStr        Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AttributeExpr where
  rnf (AttribDotNameExpr a  ) = deepseq a ()
  rnf (AttribStringExpr  a b) = deepseq a $! deepseq b ()

instance HasNullValue AttributeExpr where
  nullValue = AttribStringExpr nil LocationUnknown
  testNull (AttribStringExpr a _) = a==nil
  testNull _ = False

instance HasLocation AttributeExpr where
  getLocation o     = case o of
    AttribDotNameExpr   o   -> getLocation o
    AttribStringExpr  _ loc -> loc
  setLocation o loc = case o of
    AttribDotNameExpr o     -> AttribDotNameExpr (setLocation o loc)
    AttribStringExpr  o _   -> AttribStringExpr o loc
  delLocation o     = case o of
    AttribDotNameExpr o     -> AttribDotNameExpr (delLocation o)
    AttribStringExpr  o _   -> AttribStringExpr o LocationUnknown

instance PPrintable AttributeExpr where { pPrint = pPrintInterm }

-- binary 0x81 0x82 -- placed next to with 'DotNameExpr'
instance B.Binary AttributeExpr MTab where
  put o = case o of
    AttribDotNameExpr a     -> B.put a
    AttribStringExpr  a loc -> B.prefixByte 0x82 $ B.put a >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting AttributeExpr"

instance B.HasPrefixTable AttributeExpr Word8 MTab where
  prefixTable = (AttribDotNameExpr <$> B.prefixTable) <>
    B.mkPrefixTableWord8 "AttributeExpr" 0x82 0x82 [return AttribStringExpr <*> B.get <*> B.get]

----------------------------------------------------------------------------------------------------

data AST_Attribute
  = AST_AttribDotName AST_DotLabel
  | AST_AttribString  UStr        Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AST_Attribute where
  rnf (AST_AttribDotName a  ) = deepseq a ()
  rnf (AST_AttribString  a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_Attribute where
  nullValue = AST_AttribString nil LocationUnknown
  testNull (AST_AttribString a _) = a==nil
  testNull _ = False

instance HasLocation AST_Attribute where
  getLocation o     = case o of
    AST_AttribDotName   o   -> getLocation o
    AST_AttribString  _ loc -> loc
  setLocation o loc = case o of
    AST_AttribDotName o     -> AST_AttribDotName (setLocation o loc)
    AST_AttribString  o _   -> AST_AttribString o loc
  delLocation o     = case o of
    AST_AttribDotName o     -> AST_AttribDotName (delLocation o)
    AST_AttribString  o _   -> AST_AttribString o LocationUnknown

instance PPrintable AST_Attribute where
  pPrint o = case o of
    AST_AttribDotName nm    -> pPrint nm
    AST_AttribString  str _ -> pPrint str

instance HasRandGen AST_Attribute where
  randChoice = randChoiceList $
    [ AST_AttribDotName <$> randO
    , return AST_AttribString  <*> randO <*> no
    ]
  randO = _randTrace "AST_Attribute" $ countNode $ runRandChoice
  defaultO = randO
  defaultChoice = randChoiceList [defaultO]

instance ToDaoStructClass AST_Attribute where
  toDaoStruct = ask >>= \o -> case o of
    AST_AttribDotName str     -> innerToStruct str >> renameConstructor "AttributeDotName"
    AST_AttribString  str loc ->
      renameConstructor "AttributeString"  >> "value" .= str >> putLocation loc

instance FromDaoStructClass AST_Attribute where
  fromDaoStruct = msum $
    [ constructor "AttributeDotName" >> AST_AttribDotName <$> innerFromStruct "DotLabel"
    , constructor "AttributeString"  >> return AST_AttribString  <*> req "value" <*> location
    ]

instance Intermediate AttributeExpr AST_Attribute where
  toInterm o   = case o of
    AST_AttribDotName a     -> AttribDotNameExpr <$> ti a
    AST_AttribString  a loc -> [AttribStringExpr a loc]
  fromInterm o = case o of
    AttribDotNameExpr a     -> AST_AttribDotName <$> fi a
    AttribStringExpr  a loc -> [AST_AttribString  a loc]

instance ObjectClass AST_Attribute where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Attribute where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data NamespaceExpr = NamespaceExpr (Maybe Name) Location deriving (Eq, Ord, Show, Typeable)

instance NFData NamespaceExpr where { rnf (NamespaceExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue NamespaceExpr where
  nullValue = NamespaceExpr Nothing LocationUnknown
  testNull (NamespaceExpr Nothing _) = True
  testNull _ = False

instance HasLocation NamespaceExpr where
  getLocation (NamespaceExpr _ loc)     = loc
  setLocation (NamespaceExpr a _  ) loc = NamespaceExpr a loc
  delLocation (NamespaceExpr a _  )     = NamespaceExpr a LocationUnknown

instance PPrintable NamespaceExpr where { pPrint = pPrintInterm }

instance B.Binary NamespaceExpr MTab where
  put (NamespaceExpr a loc) = B.put a >> B.put loc
  get = return NamespaceExpr <*> B.get <*> B.get

----------------------------------------------------------------------------------------------------

data AST_Namespace
  = AST_NoNamespace
  | AST_Namespace (Com Name) Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AST_Namespace where
  rnf  AST_NoNamespace    = ()
  rnf (AST_Namespace a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_Namespace where
  nullValue = AST_NoNamespace
  testNull AST_NoNamespace = True
  testNull _ = False

instance HasLocation AST_Namespace where
  getLocation o     = case o of
    AST_NoNamespace     -> LocationUnknown
    AST_Namespace _ loc -> loc
  setLocation o loc = case o of
    AST_NoNamespace     -> AST_NoNamespace
    AST_Namespace a _   -> AST_Namespace a loc
  delLocation o     = case o of
    AST_NoNamespace     -> AST_NoNamespace
    AST_Namespace a _   -> AST_Namespace a LocationUnknown

instance PPrintable AST_Namespace where
  pPrint o = case o of { AST_NoNamespace -> return (); AST_Namespace a _ -> pPrint a; }

instance HasRandGen AST_Namespace where
  randO = _randTrace "AST_Namespace" $ countNode $ runRandChoice
  randChoice = randChoiceList [return AST_NoNamespace, return AST_Namespace <*> randO <*> no]
  defaultO = randO
  defaultChoice = randChoiceList [defaultO]

instance ToDaoStructClass AST_Namespace where
  toDaoStruct = ask >>= \a -> case a of
    AST_NoNamespace     -> makeNullary "NoNamespace"
    AST_Namespace n loc -> renameConstructor "Namespace" >> "name" .= n >> putLocation loc

instance FromDaoStructClass AST_Namespace where
  fromDaoStruct = msum $
    [ nullary "NoNamespace" >> return AST_NoNamespace
    , constructor "Namespace" >> return AST_Namespace <*> req "name" <*> location
    ]

instance Intermediate NamespaceExpr AST_Namespace where
  toInterm o = case o of
    AST_NoNamespace     -> [nullValue]
    AST_Namespace n loc -> [NamespaceExpr (Just $ unComment n) loc]
  fromInterm (NamespaceExpr n loc) = case n of
    Nothing -> [nullValue]
    Just  n -> [AST_Namespace (Com n) loc]

instance ObjectClass AST_Namespace where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_Namespace where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A 'TopLevelExpr' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data TopLevelExpr
  = RequireExpr AttributeExpr                   Location
  | ImportExpr  AttributeExpr     NamespaceExpr Location
  | TopScript   ScriptExpr                      Location
  | EventExpr   TopLevelEventType CodeBlock     Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData TopLevelExpr where
  rnf (RequireExpr a b  ) = deepseq a $! deepseq b ()
  rnf (ImportExpr  a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (TopScript   a b  ) = deepseq a $! deepseq b ()
  rnf (EventExpr   a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue TopLevelExpr where
  nullValue = TopScript nullValue LocationUnknown
  testNull (TopScript a LocationUnknown) = testNull a
  testNull _ = False

isAttribute :: TopLevelExpr -> Bool
isAttribute toplevel = case toplevel of { RequireExpr{} -> True; ImportExpr{} -> True; _ -> False; }

instance HasLocation TopLevelExpr where
  getLocation o = case o of
    RequireExpr _   o -> o
    ImportExpr  _ _ o -> o
    TopScript   _   o -> o
    EventExpr   _ _ o -> o
  setLocation o loc = case o of
    RequireExpr a    _ -> RequireExpr a loc
    ImportExpr  a b  _ -> ImportExpr  a b loc
    TopScript   a    _ -> TopScript   a   loc
    EventExpr   a b  _ -> EventExpr   a b loc
  delLocation o = case o of
    RequireExpr a    _ -> RequireExpr (delLocation a) LocationUnknown
    ImportExpr  a b  _ -> ImportExpr  (delLocation a) (delLocation b) LocationUnknown
    TopScript   a    _ -> TopScript   (delLocation a) LocationUnknown
    EventExpr   a b  _ -> EventExpr a (delLocation b) LocationUnknown

instance PPrintable TopLevelExpr where { pPrint = pPrintInterm }

-- binary 0xE9 0xEE
instance B.Binary TopLevelExpr MTab where
  put o = case o of
    RequireExpr a               z -> B.prefixByte 0xE9 $ B.put a >> B.put z
    ImportExpr  a             b z -> B.prefixByte 0xEA $ B.put a >> B.put b >> B.put z
    TopScript   a               z -> B.prefixByte 0xEB $ B.put a >> B.put z
    EventExpr   BeginExprType b z -> B.prefixByte 0xEC $ B.put b >> B.put z
    EventExpr   ExitExprType  b z -> B.prefixByte 0xED $ B.put b >> B.put z
    EventExpr   EndExprType   b z -> B.prefixByte 0xEE $ B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting TopLevelExpr"

instance B.HasPrefixTable TopLevelExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "TopLevelExpr" 0xE9 0xEE $
    [ return RequireExpr <*> B.get <*> B.get
    , return ImportExpr  <*> B.get <*> B.get <*> B.get
    , return TopScript   <*> B.get <*> B.get
    , return (EventExpr BeginExprType) <*> B.get <*> B.get
    , return (EventExpr ExitExprType ) <*> B.get <*> B.get
    , return (EventExpr EndExprType  ) <*> B.get <*> B.get
    ]

-- Since 'TopLevelExpr's can modify the 'ExecUnit', and since 'Exec' is not a stateful monad, a
-- simple hack is used: every update that should occur on executing the expression is returned as a
-- function which can be applied by the context which called it. Refer to the instance for
-- 'Executable' for the 'Program' type to see how the calling context is used to update the state.
instance Executable TopLevelExpr (ExecUnit -> ExecUnit) where
  execute o = _setTopLevelExprError o $ ask >>= \xunit -> case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    RequireExpr{} -> attrib "require"
    ImportExpr{}  -> attrib "import"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    TopScript scrpt _ -> do
      let (LocalStore stor) = execStack xunit
      -- push a namespace onto the stack
      liftIO $ modifyIORef stor (stackPush M.empty)
      -- get the functions declared this far
      pval <- catchPredicate $ execute scrpt
      case pval of
        OK                _  -> return ()
        PFail (ExecReturn _) -> return ()
        PFail           err  -> throwError err
        Backtrack            -> return () -- do not backtrack at the top-level
      -- pop the namespace, keep any local variable declarations
      dict <- liftIO $ atomicModifyIORef stor stackPop
      -- merge the local variables into the global varaibles resource.
      let (GlobalStore stor) = globalData xunit
      liftIO $ modifyMVar_ stor (return . M.union dict)
      return id
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    EventExpr typ scrpt _ -> do
      exec <- setupCodeBlock scrpt
      let f = (++[exec])
      return $ case typ of
        BeginExprType -> \xunit -> xunit{ preExec      = f (preExec      xunit) }
        EndExprType   -> \xunit -> xunit{ postExec     = f (postExec     xunit) }
        ExitExprType  -> \xunit -> xunit{ quittingTime = f (quittingTime xunit) }
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    where
      attrib a = fail $ a++" expression must occur only at the top of a dao script file"

instance ObjectClass TopLevelExpr where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass TopLevelExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

-- | A 'AST_TopLevel' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data AST_TopLevel
  = AST_Require    (Com AST_Attribute)                       Location
  | AST_Import     (Com AST_Attribute)         AST_Namespace Location
  | AST_TopScript  AST_Script                                Location
  | AST_Event      TopLevelEventType [Comment] AST_CodeBlock Location
  | AST_TopComment [Comment]
  deriving (Eq, Ord, Typeable, Show)

isAST_Attribute :: AST_TopLevel -> Bool
isAST_Attribute o = case o of { AST_Require{} -> True; AST_Import{} -> True; _ -> False; }

-- | Split a list of 'AST_TopLevel' items into a tripple, the "require" statements, the "import"
-- statements. This function scans the list lazily and returns as soon as an 'AST_TopLevel' item in
-- the list is found that is not one of 'AST_Require', 'AST_Import' or 'AST_TopComment'.
-- 
-- Notice that this function takes an 'AST_TopLevel' and returns a list of 'AttributeExpr's, not a
-- list of 'AST_Attribute's. This is because this function is designed for assisting in evaluation
-- of the import statements of a Dao script file, specifically in order to generate a dependency
-- graph.
getRequiresAndImports :: [AST_TopLevel] -> ([AttributeExpr], [(AttributeExpr, NamespaceExpr)])
getRequiresAndImports = loop [] [] where
  loop requires imports ox = case ox of
    AST_Require a   _ : ox -> loop (requires ++ uc0 a) imports ox
    AST_Import  a b _ : ox -> loop requires (imports ++ (pure (,) <*> uc0 a <*> toInterm b)) ox
    AST_TopComment{}  : ox -> loop requires imports ox
    _                      -> (requires, imports)

instance NFData AST_TopLevel where
  rnf (AST_Require    a b    ) = deepseq a $! deepseq b ()
  rnf (AST_Import     a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_TopScript  a b    ) = deepseq a $! deepseq b ()
  rnf (AST_Event      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_TopComment a      ) = deepseq a ()

instance HasNullValue AST_TopLevel where
  nullValue = AST_TopScript nullValue LocationUnknown
  testNull (AST_TopScript a _) = testNull a
  testNull _ = False

instance HasLocation AST_TopLevel where
  getLocation o = case o of
    AST_Require    _     o -> o
    AST_Import     _ _   o -> o
    AST_TopScript  _     o -> o
    AST_Event      _ _ _ o -> o
    AST_TopComment _       -> lu
  setLocation o loc = case o of
    AST_Require    a      _ -> AST_Require    a       loc
    AST_Import     a b    _ -> AST_Import     a b     loc
    AST_TopScript  a      _ -> AST_TopScript  a       loc
    AST_Event      a b c  _ -> AST_Event      a b c   loc
    AST_TopComment a        -> AST_TopComment a
  delLocation o = case o of
    AST_Require    a      _ -> AST_Require    (delLocation a) LocationUnknown
    AST_Import     a b    _ -> AST_Import     (delLocation a) (delLocation b) LocationUnknown
    AST_TopScript  a      _ -> AST_TopScript  (delLocation a) LocationUnknown
    AST_Event      a b c  _ -> AST_Event      a b (delLocation c) LocationUnknown
    AST_TopComment a        -> AST_TopComment a  

instance PPrintable AST_TopLevel where
  pPrint o = case o of
    AST_Require   a      _ -> pWrapIndent [pString "require ", pPrint a, pString ";"]
    AST_Import    a b    _ -> pWrapIndent [pString "import  ", pPrint a, pPrint b, pString ";"]
    AST_TopScript a      _ -> pPrint a
    AST_Event     a b c  _ -> pClosure (pShow a >> mapM_ pPrint b) " { " " }" (map pPrint (getAST_CodeBlock c))
    AST_TopComment a       -> mapM_ (\a -> pPrint a >> pNewLine) a

instance ToDaoStructClass AST_TopLevel where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Require    a     loc -> nm "Require" >> "attribute" .= a >> putLocation loc
    AST_Import     a b   loc -> nm "Import" >> "attribute" .= a >> "namespace" .= b >> putLocation loc
    AST_TopScript  a     loc -> nm "TopLevel" >> "script" .= a >> putLocation loc
    AST_TopComment a         -> nm "Comment" >> putComments a
    AST_Event      a b c loc ->
      nm "Event" >> "type" .= a >> "block" .= c >> putComments b >> putLocation loc

instance FromDaoStructClass AST_TopLevel where
  fromDaoStruct = msum $
    [ constructor "Import"   >> return AST_Import    <*> req "attribute" <*> req "namespace" <*> location
    , constructor "Require"  >> return AST_Require   <*> req "attribute" <*> location
    , constructor "TopLevel" >> return AST_TopScript <*> req "script"    <*> location
    , constructor "Event"    >> return AST_Event     <*> req "type"      <*> comments <*> req "block" <*> location
    , constructor "Comment"  >> AST_TopComment     <$> comments
    ]

instance HasRandGen AST_TopLevel where
  randO = _randTrace "AST_TopLevel" $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_Require   <*> randO                     <*> no
    , return AST_Import    <*> randO <*> randO           <*> no
    , return AST_TopScript <*> randO                     <*> no
    , return AST_Event     <*> randO <*> randO <*> randO <*> no
    , AST_TopComment <$> defaultO
    ]
  defaultO = _randTrace "D.AST_TopLevel" runDefaultChoice
  defaultChoice = randChoiceList $
    [ return AST_Import    <*> defaultO <*> defaultO              <*> no
    , return AST_Require   <*> defaultO                           <*> no
    , return AST_TopScript <*> defaultO                           <*> no
    , return AST_Event     <*> defaultO <*> defaultO <*> defaultO <*> no
    ]

instance Intermediate TopLevelExpr AST_TopLevel where
  toInterm   ast = case ast of
    AST_Require   a     loc -> [RequireExpr] <*> uc0 a           <*> [loc]
    AST_Import    a   b loc -> [ImportExpr ] <*> uc0 a  <*> ti b <*> [loc]
    AST_TopScript a     loc -> [TopScript  ] <*> ti  a           <*> [loc]
    AST_Event     a _ b loc -> [EventExpr  ] <*>    [a] <*> ti b <*> [loc]
    AST_TopComment     _loc -> mzero
  fromInterm obj = case obj of
    RequireExpr a   loc -> [AST_Require  ] <*> nc0 a                    <*> [loc]
    ImportExpr  a b loc -> [AST_Import   ] <*> nc0 a           <*> fi b <*> [loc]
    TopScript   a   loc -> [AST_TopScript] <*> fi  a                    <*> [loc]
    EventExpr   a b loc -> [AST_Event    ] <*>    [a] <*> [[]] <*> fi b <*> [loc]

instance ObjectClass AST_TopLevel where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_TopLevel where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A program is just a list of 'TopLevelExpr's. It serves as the 'Intermediate'
-- representation of a 'AST_SourceCode'.
newtype Program = Program { topLevelExprs :: [TopLevelExpr] } deriving (Eq, Ord, Typeable)

instance Show Program where { show (Program o) = unlines (map show o) }

instance HasNullValue Program where
  nullValue = Program []
  testNull (Program p) = null p

instance HasLocation Program where
  getLocation o = case topLevelExprs o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = Program (fmap delLocation (topLevelExprs o))

-- the number is encoded by the ASCII encoded string "DaoProg\0"
program_magic_number :: Word64
program_magic_number = 0x44616f50726f6700

instance B.Binary Program MTab where
  put o = do
    -- place a magic number first, 
    B.putWord64be program_magic_number
    mapM_ B.put $ topLevelExprs o
  get = do
    magic <- B.lookAhead B.getWord64be
    guard (magic == program_magic_number)
    B.getWord64be >> fmap Program B.get

-- | Initialized the current 'ExecUnit' by evaluating all of the 'TopLevel' data in a
-- 'AST.AST_SourceCode'.
instance Executable Program ExecUnit where
  execute (Program ast) = do
    (LocalStore stackRef) <- asks execStack
    liftIO $ modifyIORef stackRef (stackPush M.empty)
    updxunit  <- foldl (.) id <$> mapM execute (dropWhile isAttribute ast)
    -- Now, the local variables that were defined in the top level need to be moved to the global
    -- variable store.
    localVars <- liftIO $ atomicModifyIORef stackRef stackPop
    (GlobalStore globalVars) <- asks globalData
    liftIO $ modifyMVar_ globalVars $ \dict -> do
      return (M.union dict localVars)
    fmap updxunit ask

instance ObjectClass Program where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass Program where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

-- | A 'SourceCode' is the structure loaded from source code. An 'ExecUnit' object is constructed from
-- 'SourceCode'.
data AST_SourceCode
  = AST_SourceCode
    { sourceModified :: Int
    , sourceFullPath :: UStr
      -- ^ the URL (full file path) from where this source code was received.
    , directives     :: [AST_TopLevel]
    }
  deriving (Eq, Ord, Typeable)

instance NFData AST_SourceCode where
  rnf (AST_SourceCode a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_SourceCode where
  nullValue = (AST_SourceCode 0 nil [])
  testNull (AST_SourceCode 0 a []) | a==nil = True
  testNull _ = False

instance PPrintable AST_SourceCode where
  pPrint sc = do
    let (attrs, dirs) = span isAST_Attribute (directives sc)
    mapM_ pPrint attrs
    pForceNewLine
    mapM_ (\dir -> pPrint dir >> pForceNewLine) dirs

instance ToDaoStructClass AST_SourceCode where
  toDaoStruct = void $ do
    renameConstructor "SourceCode"
    "modified" .=@ sourceModified
    "path"     .=@ sourceFullPath
    asks directives >>= define "code" . listToObj

instance FromDaoStructClass AST_SourceCode where
  fromDaoStruct = constructor "SourceCode" >>
    return AST_SourceCode <*> req "modified" <*> req "path" <*> reqList "code"

instance Intermediate Program AST_SourceCode where
  toInterm   ast = [Program $ directives ast >>= toInterm]
  fromInterm obj = return $
    AST_SourceCode
    { sourceModified = 0
    , sourceFullPath = nil
    , directives     = topLevelExprs obj >>= fromInterm
    }

instance ObjectClass AST_SourceCode where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass AST_SourceCode where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------
-- $Builtin_object_interfaces
-- The following functions provide object interfaces for essential data types.

instance HaskellDataClass () where { haskellDataInterface = interface () (return ()) }

instance HaskellDataClass GlobAction where
  haskellDataInterface = interface (GlobAction [] undefined) $ do
    defCallable $ \rule -> do
      let vars o = case o of {Wildcard _ -> 1; AnyOne _ -> 1; Single _ -> 0; }
      let m = maximum $ map (sum . map vars . getPatUnits) $ globPattern rule
      let lu = LocationUnknown
      let params = flip ParamListExpr lu $ NotTypeChecked $
            map (flip (ParamExpr False) lu . NotTypeChecked . ustr . ("var"++) . show) [(1::Int)..m]
      return $ return $
        CallableCode
        { argsPattern    = params
        , returnType     = nullValue
        , codeSubroutine = globSubroutine rule
            -- TODO: the subroutine should be scanned for integer references and replaced with local
            -- variables called "varN" where N is the number of the integer reference.
        }

----------------------------------------------------------------------------------------------------

type Get     a = B.GGet  MethodTable a
type Put       = B.GPut  MethodTable

-- This is only necessary to shorten the name 'MethodTable' because it is used throughout so many
-- instance declarations and type contexts.
type MTab = MethodTable

----------------------------------------------------------------------------------------------------

newtype MethodTable = MethodTable (M.Map UStr (Interface Dynamic))

instance Monoid MethodTable where
  mempty  = MethodTable mempty
  mappend (MethodTable a) (MethodTable b) = MethodTable (M.union b a)

-- | Lookup an 'Interface' by it's name from within the 'Exec' monad.
execGetObjTable :: UStr -> Exec (Maybe (Interface Dynamic))
execGetObjTable nm = asks (lookupMethodTable nm . globalMethodTable)

lookupMethodTable :: UStr -> MethodTable -> Maybe (Interface Dynamic)
lookupMethodTable nm (MethodTable tab) = M.lookup nm tab

-- not for export, use 'daoClass'
_insertMethodTable
  :: (Typeable o, HaskellDataClass o)
  => o
  -> UStr
  -> Interface o
  -> MethodTable
  -> MethodTable
_insertMethodTable _ nm ifc = flip mappend $
  MethodTable (M.singleton nm (interfaceToDynamic ifc))

typeRepToUStr :: TypeRep -> UStr
typeRepToUStr a = let con = typeRepTyCon a in ustr (tyConModule con ++ '.' : tyConName con)

instance B.HasCoderTable MethodTable where
  getEncoderForType nm mtab = fmap fst $ lookupMethodTable nm mtab >>= objBinaryFormat
  getDecoderForType nm mtab = fmap snd $ lookupMethodTable nm mtab >>= objBinaryFormat

----------------------------------------------------------------------------------------------------

-- | This is a function very much like the 'readForLoop' function, but the @iter@ type does not need
-- to instantiate the 'ReadIterable' class, instead the function that would be used to instantiate
-- 'readIter' is provided to this function as the first function parameter.
readForLoopWith :: (iter -> Exec (val, iter)) -> iter -> (val -> Exec ()) -> Exec ()
readForLoopWith readIter iter f = do
  iter <- mplus (Just <$> readIter iter) (return Nothing)
  case iter of
    Nothing          -> return ()
    Just (val, iter) -> f val >> readForLoopWith readIter iter f

-- | A class that provides the 'readIter' function, which is a function that will iterate over types
-- which can be read sequentially, but not modified. The minimal complete definition is 'readIter'.
-- 
-- Notice how the 'readIter' function type is defined such that it can be passed to the
-- 'Control.Monad.State.Lazy.StateT' constructor to construct a monad of type:
-- > 'Control.Monad.State.Lazy.StateT' iter 'Exec' val
-- The value over which we are iterating is modified in a stateful way: after each iteration the
-- iterator value is modified to remove the current item and set it to retrieve the next item for
-- the next iteration.
class ReadIterable iter val | iter -> val where
  -- | 'readIter' takes an iterated value @iter@ and will return a value of type @val@ which will be
  -- the next item retrieved from @iter@, returning the updated iterator @iter@ with the value of
  -- type @val@ in a tuple. 'readIter' should evaluate to 'Control.Monad.mzero' when there are no
  -- more items over which to iterate.
  readIter :: iter -> Exec (val, iter)
  -- | By default, implements a "for" loop using a 'ReadIterable' item. You can of course customize
  -- this function if your iterator is a bit more complicated.
  readForLoop :: iter -> (val -> Exec ()) -> Exec ()
  readForLoop = readForLoopWith readIter

instance ReadIterable [Object] (Maybe Object) where
  readIter ox = case ox of
    []   -> mzero
    o:ox -> return (Just o, ox)

instance ReadIterable Object (Maybe Object) where
  readIter o = case o of
    OList []     -> mzero
    OList (o:ox) -> return (Just o, OList ox)
    OHaskell (HaskellData d ifc) -> case objReadIterable ifc of
      Nothing -> execThrow $ obj [obj "cannot iterate over object", o]
      Just  f -> f d >>= \ (o, d) -> return (o, OHaskell $ HaskellData d ifc)
    _       -> execThrow $ obj [obj "cannot iterate over object", o]

----------------------------------------------------------------------------------------------------

-- | This is a function very much like the 'updateForLoop' function, but the @iter@ type does not
-- need to instantiate the 'ReadIterable' or 'UpdateIterable' classes, instead pass the function
-- that would be used to instantiate 'readIter' is provided to this function as the first function
-- parameter, and the function that would be used to instantiate 'updateIter' is provided to this
-- function as the second function parameter. The remaining parameters are the same as for
-- 'updateForLoop'.
updateForLoopWith
  :: (iter -> Exec (val, iter))
  -> (iter -> val -> Exec iter)
  -> iter -> iter -> (val -> Exec val) -> Exec iter
updateForLoopWith readIter updateIter mempty iter f = loop iter mempty where
  loop iter updated = do
    next <- optional (readIter iter) -- catch backtracking of the iterator as an end-of-loop signal
    case next of
      Nothing          -> return updated
      Just (val, iter) -> f val >>= updateIter updated >>= loop iter
        -- if backtracking occurs here in the for-loop body, it is not caught.

-- | A class that provides the 'updateIter' function, which is a function that will iterate over
-- types which can be read sequentially and modified as they are read. The minimal complete
-- definition is 'updateIter'.
-- 
-- The iterator item must implement the 'Data.Monoid.Monoid' class, which makes 'UpdateIterable'
-- just a little bit like the 'Data.Foldable.Foldable' class: iteration produces values which are
-- folded back into an iterator.
class ReadIterable iter val => UpdateIterable iter val | iter -> val where
  -- | 'updateIter' is a fold-like function that takes an iterator value @iter@ and an 'Object' that
  -- have been returned by a single evaluation of 'readIter'. 'updateIter' must then perform some
  -- modification to the iterator value @iter@ using the 'Object' value, for example it could place
  -- items back into the iterator, or remove items from the iterator.
  updateIter :: iter -> val -> Exec iter
  -- | By default, implements a "for" loop using an 'UpdateIterable' item. You can of course
  -- customize this function if your iterator is a bit more complicated. This function behaves
  -- somewhat like a fold operation: the first parameter is an initial value (for example an empty
  -- list) which will be passed to the 'updateIter' function after each iteration.  The second
  -- parameter is the iterator value (for example, the list of elements over which to iterate) is
  -- used to retrieve each next iterated item via the 'readIter' function. The third parameter is
  -- the actual function to be evaluated on for each iteration, it should take the item to be
  -- iterated, may upate the item, and should return it.
  updateForLoop :: iter -> iter -> (val -> Exec val) -> Exec iter
  updateForLoop = updateForLoopWith readIter updateIter

instance UpdateIterable [Object] (Maybe Object) where
  updateIter iter val = return $ case val of
    Just (OList val) -> iter++val
    Just        val  -> iter++[val]
    Nothing          -> iter

instance UpdateIterable Object (Maybe Object) where
  updateIter o val = case o of
    ONull   -> return $ OList $ maybe [] return val
    OList o -> OList <$> updateIter o val
    OHaskell (HaskellData d ifc) -> case objUpdateIterable ifc of
      Nothing -> execThrow $ obj [obj "cannot iterate over object", o]
      Just  f -> f d val >>= \d -> return $ OHaskell $ HaskellData d ifc
    o       -> return $ OList $ [o] ++ maybe [] return val
  updateForLoop = updateForLoopWith readIter updateIter

----------------------------------------------------------------------------------------------------

-- | This class only exists to allow many different Haskell data types to declare their
-- 'Interface' under the same funcion name: 'haskellDataInterface'. Instantiate this function with
-- the help of the 'interface' function.
class HaskellDataClass typ where { haskellDataInterface :: Interface typ }

instance HaskellDataClass Location where
  haskellDataInterface = interface LocationUnknown $ do
    autoDefEquality >> autoDefOrdering
    autoDefToStruct >> autoDefFromStruct
    autoDefPPrinter

instance ObjectClass (H.HashMap Object Object) where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass (H.HashMap Object Object) where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt >> autoDefPPrinter
    defSizer $ return . obj . H.size
    let un _ a b = xmaybe (fromObj b) >>= \b -> return $ new $ H.union b a
    defInfixOp ADD  un
    defInfixOp ORB  un
    defInfixOp ANDB $ \ _ a b -> xmaybe (fromObj b) >>= \b -> return $ new $ (H.intersection b a :: H.HashMap Object Object)
    defInfixOp SUB  $ \ _ a b -> xmaybe (fromObj b) >>= \b -> return $ new $ (H.difference b a :: H.HashMap Object Object)
    let initParams ox = case ox of
          [] -> return H.empty
          _  -> execThrow $ obj [obj "\"HashMap\" constructor takes no initializing parameters"]
    let dictParams hmap ox = do
          mt <- asks globalMethodTable
          let hash128 = H.deriveHash128_DaoBinary mt
          let f hmap (i, op, o) = let idx = H.hashNewIndex hash128 i in case op of
                UCONST -> return $ H.hashInsert idx o hmap
                op     -> case H.hashLookup idx hmap of
                  Nothing -> execThrow $ obj [obj "item", i, obj "is undefined, cannot update"]
                  Just  n -> do
                    o <- execute $ (_updatingOps!op) n o
                    return $ H.hashInsert idx o hmap
          foldM f hmap ox
    defDictInit initParams dictParams

-- | This is a convenience function for calling 'OHaskell' using just an initial value of type
-- @typ@. The 'Interface' is retrieved automatically using the instance of 'haskellDataInterface' for
-- the @typ@.
toHaskellData :: (HaskellDataClass typ, Typeable typ) => typ -> HaskellData
toHaskellData t = HaskellData (toDyn t) (interfaceTo t haskellDataInterface) where
  interfaceTo :: Typeable typ => typ -> Interface typ -> Interface Dynamic
  interfaceTo _ ifc = interfaceToDynamic ifc

-- | Inverse operation of 'toHaskellData', useful when instantiating 'ObjectClass', uses
-- 'Data.Dynamic.fromDynamic' to extract the value that has been wrapped in up the 'HaskellData'
-- constructor.
fromHaskellData :: (HaskellDataClass typ, Typeable typ) => HaskellData -> Maybe typ
fromHaskellData (HaskellData o _) = fromDynamic o

-- | This is all of the functions used by the "Dao.Evaluator" when manipulating objects in a Dao
-- program. Behavior of objects when they are used in "for" statements or "with" statements, or when
-- they are dereferenced using the "@" operator, or when they are used in equations are all defined
-- here.
-- 
-- So this table is the reason you instantiate 'HaskellDataClass'.
-- 
-- @obj@ specifies the container type that will wrap-up data of type @typ@. @obj@ is the type used
-- throughout the runtime system to symbolize the basic unit of information operated on by
-- computations.
-- 
-- @typ@ specifies the type that you want to wrap-up into an @obj@ constructor. When you want to,
-- for example, check for equality between object of type @typ@, you can define a function for
-- 'objEquality'. All of the other polymorphic types are bound to the @typ@ types by the functional
-- dependencies mechanism of the Haskell language.
-- 
-- @exec@ specifies a monad in which to evaluate functions which may need to cause side-effects.
-- This should usually be a 'Control.Monad.Monad'ic type like @IO@ or 'Exec'.
data Interface typ =
  Interface
  { objHaskellType     :: TypeRep -- ^ this type is deduced from the initial value provided to the 'interface'.
  , objCastFrom        :: Maybe (Object -> typ)                                                                -- ^ defined by 'defCastFrom'
  , objEquality        :: Maybe (typ -> typ -> Bool)                                                           -- ^ defined by 'defEquality'
  , objOrdering        :: Maybe (typ -> typ -> Ordering)                                                       -- ^ defined by 'defOrdering'
  , objBinaryFormat    :: Maybe (typ -> Put, Get typ)                                                          -- ^ defined by 'defBinaryFmt'
  , objNullTest        :: Maybe (typ -> Bool)                                                                  -- ^ defined by 'defNullTest'
  , objPPrinter        :: Maybe (typ -> PPrint)                                                                -- ^ defined by 'defPPrinter'
  , objReadIterable    :: Maybe (typ -> Exec (Maybe Object, typ))                                              -- ^ defined by 'defReadIterator'
  , objUpdateIterable  :: Maybe (typ -> Maybe Object -> Exec typ)                                              -- ^ defined by 'defUpdateIterator'
  , objIndexer         :: Maybe (typ -> [Object] -> Exec Object)                                               -- ^ defined by 'defIndexer'
  , objIndexUpdater    :: Maybe (typ -> (Maybe Object -> Exec (Maybe Object)) -> [Object] -> Exec (Maybe typ)) -- ^ defined by 'defIndexUpdater'
  , objSizer           :: Maybe (typ -> Exec Object)                                                           -- ^ defined by 'defSizer'
  , objToStruct        :: Maybe (typ -> Exec T_struct)                                                         -- ^ defined by 'defStructFormat'
  , objFromStruct      :: Maybe (T_struct -> Exec typ)                                                         -- ^ defined by 'defStructFormat'
  , objDictInit        :: Maybe ([Object] -> Exec typ, typ -> [(Object, UpdateOp, Object)] -> Exec typ)        -- ^ defined by 'defDictInit'
  , objListInit        :: Maybe ([Object] -> Exec typ, typ -> [Object] -> Exec typ)                            -- ^ defined by 'defDictInit'
  , objInfixOpTable    :: Maybe (Array InfixOp  (Maybe (InfixOp  -> typ -> Object -> XPure Object)))           -- ^ defined by 'defInfixOp'
  , objArithPfxOpTable :: Maybe (Array ArithPfxOp (Maybe (ArithPfxOp -> typ -> XPure Object)))                 -- ^ defined by 'defPrefixOp'
  , objCallable        :: Maybe (typ -> Exec [CallableCode])                                                   -- ^ defined by 'defCallable'
  , objDereferencer    :: Maybe (typ -> Exec (Maybe Object))
  }
  deriving Typeable

instance Eq  (Interface typ) where { a==b = objHaskellType a == objHaskellType b }

instance Ord (Interface typ) where { compare a b = compare (objHaskellType a) (objHaskellType b) }

-- | This function works a bit like 'Data.Functor.fmap', but maps an 'Interface' from one type
-- to another. This requires two functions: one that can cast from the given type to the adapted
-- type (to convert outputs of functions), and one that can cast back from the adapted type to the
-- original type (to convert inputs of functions). Each coversion function takes a string as it's
-- first parameter, this is a string containing the name of the function that is currently making
-- use of the conversion operation. Should you need to use 'Prelude.error' or 'mkExecError', this
-- string will allow you to throw more informative error messages. WARNING: this function leaves
-- 'objHaskellType' unchanged because the original type value should usually be preserved.
interfaceAdapter
  :: (Typeable typ_a, Typeable typ_b)
  => (String -> typ_a -> typ_b)
  -> (String -> typ_b -> typ_a)
  -> Interface typ_a
  -> Interface typ_b
interfaceAdapter a2b b2a ifc = 
  ifc
  { objCastFrom        = let n="objCastFrom"       in fmap (fmap (a2b n)) (objCastFrom ifc)
  , objEquality        = let n="objEquality"       in fmap (\eq  a b -> eq  (b2a n a) (b2a n b)) (objEquality ifc)
  , objOrdering        = let n="objOrdering"       in fmap (\ord a b -> ord (b2a n a) (b2a n b)) (objOrdering ifc)
  , objBinaryFormat    = let n="objBinaryFormat"   in fmap (\ (toBin , fromBin) -> (toBin . b2a n, fmap (a2b n) fromBin)) (objBinaryFormat ifc)
  , objNullTest        = let n="objNullTest"       in fmap (\null b -> null (b2a n b)) (objNullTest ifc)
  , objPPrinter        = let n="objPPrinter"       in fmap (\eval -> eval . b2a n) (objPPrinter ifc)
  , objReadIterable    = let n="objReadIterable"   in fmap (\for -> fmap (fmap (a2b n)) . for . b2a n) (objReadIterable ifc)
  , objUpdateIterable  = let n="objUpdateIterable" in fmap (\for i -> fmap (a2b n) . for (b2a n i)) (objUpdateIterable ifc)
  , objIndexer         = let n="objIndexer"        in fmap (\f i -> f (b2a n i)) (objIndexer ifc)
  , objIndexUpdater    = let n="objIndexUpdater"   in fmap (\f o u i -> fmap (fmap (a2b n)) $ f (b2a n o) u i) (objIndexUpdater ifc)
  , objSizer           = let n="objSizer"          in fmap (\f o -> f (b2a n o)) (objSizer ifc)
  , objToStruct        = let n="objToStruct"       in fmap (\toTree -> toTree . b2a n) (objToStruct ifc)
  , objFromStruct      = let n="objFromStruct"     in fmap (\fromTree -> fmap (a2b n) . fromTree) (objFromStruct ifc)
  , objDictInit        = let n="objDictInit"       in fmap (\ (init, eval) -> (\ox -> fmap (a2b n) (init ox), \typ ox -> fmap (a2b n) (eval (b2a n typ) ox))) (objDictInit ifc)
  , objListInit        = let n="objListInit"       in fmap (\ (init, eval) -> (\ox -> fmap (a2b n) (init ox), \typ ox -> fmap (a2b n) (eval (b2a n typ) ox))) (objListInit ifc)
  , objInfixOpTable    = let n="objInfixOpTable"   in fmap (fmap (fmap (\infx op b -> infx op (b2a n b)))) (objInfixOpTable  ifc)
  , objArithPfxOpTable = let n="objPrefixOpTabl"   in fmap (fmap (fmap (\prfx op b -> prfx op (b2a n b)))) (objArithPfxOpTable ifc)
  , objCallable        = let n="objCallable"       in fmap (\eval -> eval . b2a n) (objCallable ifc)
  , objDereferencer    = let n="objDerferencer"    in fmap (\eval -> eval . b2a n) (objDereferencer ifc)
  }

interfaceToDynamic :: Typeable typ => Interface typ -> Interface Dynamic
interfaceToDynamic oi = interfaceAdapter (\ _ -> toDyn) (from oi) oi where
  from :: Typeable typ => Interface typ -> String -> Dynamic -> typ
  from oi msg dyn = fromDyn dyn (dynErr oi msg dyn)
  dynErr :: Typeable typ => Interface typ -> String -> Dynamic -> typ
  dynErr oi msg dyn = error $ concat $
    [ "The '", msg
    , "' function defined for objects of type ", show (objHaskellType oi)
    , " was evaluated on an object of type ", show (dynTypeRep dyn)
    ]

-- Used to construct an 'Interface' in a "Control.Monad.State"-ful way. Instantiates
-- 'Data.Monoid.Monoid' to provide 'Data.Monoid.mempty' an allows multiple inheritence by use of the
-- 'Data.Monoid.mappend' function in the same way as
data HDIfcBuilder typ =
  HDIfcBuilder
  { objIfcCastFrom       :: Maybe (Object -> typ)
  , objIfcEquality       :: Maybe (typ -> typ -> Bool)
  , objIfcOrdering       :: Maybe (typ -> typ -> Ordering)
  , objIfcBinaryFormat   :: Maybe (typ -> Put, Get typ)
  , objIfcNullTest       :: Maybe (typ -> Bool)
  , objIfcPPrinter       :: Maybe (typ -> PPrint)
  , objIfcReadIterable   :: Maybe (typ -> Exec (Maybe Object, typ))
  , objIfcUpdateIterable :: Maybe (typ -> Maybe Object -> Exec typ)
  , objIfcIndexer        :: Maybe (typ -> [Object] -> Exec Object)
  , objIfcIndexUpdater   :: Maybe (typ -> (Maybe Object -> Exec (Maybe Object)) -> [Object] -> Exec (Maybe typ))
  , objIfcSizer          :: Maybe (typ -> Exec Object)
  , objIfcToStruct       :: Maybe (typ -> Exec T_struct)
  , objIfcFromStruct     :: Maybe (T_struct -> Exec typ)
  , objIfcDictInit       :: Maybe ([Object] -> Exec typ, typ -> [(Object, UpdateOp, Object)] -> Exec typ)
  , objIfcListInit       :: Maybe ([Object] -> Exec typ, typ -> [Object] -> Exec typ)
  , objIfcInfixOpTable   :: [(InfixOp , InfixOp  -> typ -> Object -> XPure Object)]
  , objIfcPrefixOpTable  :: [(ArithPfxOp, ArithPfxOp -> typ -> XPure Object)]
  , objIfcCallable       :: Maybe (typ -> Exec [CallableCode])
  , objIfcDerefer        :: Maybe (typ -> Exec (Maybe Object))
  }

initHDIfcBuilder :: HDIfcBuilder typ
initHDIfcBuilder =
  HDIfcBuilder
  { objIfcCastFrom       = Nothing
  , objIfcEquality       = Nothing
  , objIfcOrdering       = Nothing
  , objIfcBinaryFormat   = Nothing
  , objIfcNullTest       = Nothing
  , objIfcPPrinter       = Nothing
  , objIfcReadIterable   = Nothing
  , objIfcUpdateIterable = Nothing
  , objIfcIndexer        = Nothing
  , objIfcIndexUpdater   = Nothing
  , objIfcSizer          = Nothing
  , objIfcToStruct       = Nothing
  , objIfcFromStruct     = Nothing
  , objIfcDictInit       = Nothing
  , objIfcListInit       = Nothing
  , objIfcInfixOpTable   = []
  , objIfcPrefixOpTable  = []
  , objIfcCallable       = Nothing
  , objIfcDerefer        = Nothing
  }

-- | A handy monadic interface for defining an 'Interface' using nice, clean procedural
-- syntax.
type    DaoClassDef typ = DaoClassDefM typ ()
newtype DaoClassDefM typ a = DaoClassDefM { daoClassDefState :: State (HDIfcBuilder typ) a }
instance Typeable typ => Functor (DaoClassDefM typ) where
  fmap f (DaoClassDefM m) = DaoClassDefM (fmap f m)
instance Typeable typ => Monad (DaoClassDefM typ) where
  return = DaoClassDefM . return
  (DaoClassDefM m) >>= f = DaoClassDefM (m >>= daoClassDefState . f)
instance Typeable typ => Applicative (DaoClassDefM typ) where { pure=return; (<*>)=ap; }

_updHDIfcBuilder :: Typeable typ => (HDIfcBuilder typ -> HDIfcBuilder typ) -> DaoClassDefM typ ()
_updHDIfcBuilder = DaoClassDefM . modify

-- | The callback function defined here is used when objects of your @typ@ can be constructed from
-- some other 'Object'. This function is used to convert an 'Object' of another types to an data
-- type of your @typ@ when it is necessary to do so (for example, evaluating the @==@ or @!=@
-- operator).
defCastFrom :: Typeable typ => (Object -> typ) -> DaoClassDefM typ ()
defCastFrom fn = _updHDIfcBuilder(\st->st{objIfcCastFrom=Just fn})

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @==@ and @!=@ operators in Dao programs. However using this is slightly
-- different than simply overriding the @==@ or @!=@ operators. Defining an equality reliation with
-- this function also allows Haskell language programs to compare your object to other objects
-- without unwrapping them from the 'Object' wrapper.
--
-- This function automatically define an equality operation over your @typ@ using the
-- instantiation of 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function.
-- The 'defCastFrom' function is used to cast 'Object's to a value of your @typ@, and then the
-- @Prelude.==@ function is evaluated. If you eventually never define a type casting funcion using
-- 'defCastFrom', this function will fail, but it will fail lazily and at runtime, perhaps when you
-- least expect it, so be sure to define 'defCastFrom' at some point.
autoDefEquality :: (Typeable typ, Eq typ) => DaoClassDefM typ ()
autoDefEquality = defEquality (==)

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @==@ and @!=@ operators in Dao programs. However using this is slightly
-- different than simply overriding the @==@ or @!=@ operators. Defining an equality relation with
-- this function also allows Haskell language programs to compare your object to other objects
-- without unwrapping them from the 'Object' wrapper.
--
-- This function differs from 'autoDefEquality' because you must provide a customized equality
-- relation for your @typ@, if the 'autoDefEquality' and 'defCastFrom' functions are to be avoided
-- for some reason.
defEquality :: (Typeable typ, Eq typ) => (typ -> typ -> Bool) -> DaoClassDefM typ ()
defEquality fn = _updHDIfcBuilder(\st->st{objIfcEquality=Just fn})

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @<@, @>@, @<=@, and @>=@ operators in Dao programs. However using this is
-- slightly different than simply overriding the @<@, @>@, @<=@, or @>=@ operators. Defining an
-- equality relation with this function also allows Haskell language programs to compare your obejct
-- to other objects without unwrapping them from the 'Object' wrapper.
-- 
-- Automatically define an ordering for your @typ@ using the instantiation of
-- 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function. The 'defCastFrom'
-- function is used to cast 'Object's to a value of your @typ@, and then the @Prelude.==@ function
-- is evaluated. If you eventually never define a type casting funcion using 'defCastFrom', this
-- function will fail, but it will fail lazily and at runtime, perhaps when you least expect it, so
-- be sure to define 'defCastFrom' at some point.
autoDefOrdering :: (Typeable typ, Ord typ) => DaoClassDefM typ ()
autoDefOrdering = defOrdering compare

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @<@, @>@, @<=@, and @>=@ operators in Dao programs. However using this is
-- slightly different than simply overriding the @<@, @>@, @<=@, or @>=@ operators. Defining an
-- equality relation with this function also allows Haskell language programs to compare your obejct
-- to other objects without unwrapping them from the 'Object' wrapper.
-- 
-- Define a customized ordering for your @typ@, if the 'autoDefEquality' and 'defCastFrom'
-- functions are to be avoided for some reason.
defOrdering :: (Typeable typ) => (typ -> typ -> Ordering) -> DaoClassDefM typ ()
defOrdering fn = _updHDIfcBuilder(\st->st{objIfcOrdering=Just fn})

-- | The callback function defined here is used if an object of your @typ@ should ever need to be
-- stored into a binary file in persistent storage (like your filesystem) or sent across a channel
-- (like a UNIX pipe or a socket).
-- 
-- It automatically define the binary encoder and decoder using the 'Data.Binary.Binary' class
-- instantiation for this @typ@.
autoDefBinaryFmt :: (Typeable typ, B.Binary typ MethodTable) => DaoClassDefM typ ()
autoDefBinaryFmt = defBinaryFmt B.put B.get

-- | This function is used if an object of your @typ@ should ever need to be stored into a binary
-- file in persistent storage (like your filesystem) or sent across a channel (like a UNIX pipe or a
-- socket).
-- 
-- If you have binary coding and decoding methods for your @typ@ but for some silly reason not
-- instantiated your @typ@ into the 'Data.Binary.Binary' class, your @typ@ can still be used as a
-- binary formatted object by the Dao system if you define the encoder and decoder using this
-- function. However, it would be better if you instantiated 'Data.Binary.Binary' and used
-- 'autoDefBinaryFmt' instead.
defBinaryFmt :: (Typeable typ) => (typ -> Put) -> Get typ -> DaoClassDefM typ ()
defBinaryFmt put get = _updHDIfcBuilder(\st->st{objIfcBinaryFormat=Just(put,get)})

autoDefNullTest :: (Typeable typ, HasNullValue typ) => DaoClassDefM typ ()
autoDefNullTest = defNullTest testNull

-- | The callback function defined here is used if an object of your @typ@ is ever used in an @if@
-- or @while@ statement in a Dao program. This function will return @Prelude.True@ if the object is
-- of a null value, which will cause the @if@ or @while@ test to fail and execution of the Dao
-- program will branch accordingly. There is no default method for this function so it must be
-- defined by this function, otherwise your object cannot be tested by @if@ or @while@ statements.
defNullTest :: Typeable typ => (typ -> Bool) -> DaoClassDefM typ ()
defNullTest fn = _updHDIfcBuilder(\st->st{objIfcNullTest=Just fn})

-- | The callback function to be called when the "print" built-in function is used.
defPPrinter :: Typeable typ => (typ -> PPrint) -> DaoClassDefM typ ()
defPPrinter fn = _updHDIfcBuilder(\st->st{objIfcPPrinter=Just fn})

-- | The callback function to be called when the "print" built-in function is used.
autoDefPPrinter :: (Typeable typ, PPrintable typ) => DaoClassDefM typ ()
autoDefPPrinter = defPPrinter pPrint

-- | The callback function defined here is used if an object of your @typ@ is ever used in a @for@
-- statement in a Dao program. However it is much better to instantiate your @typ@ into the
-- 'ReadIterable' class and use 'autoDefIterator' instead. If 'defUpdateIterator' is also defined,
-- the function defined here will never be used.
defReadIterable :: Typeable typ => (typ -> Exec (Object, typ)) -> DaoClassDefM typ ()
defReadIterable iter = _updHDIfcBuilder $ \st ->
  st{ objIfcReadIterable=Just $ iter >=> \ (o, typ) -> return (Just o, typ) }

-- | Define 'defReadIterable' automatically using the instance of @typ@ in the 'ReadIterable' class.
autoDefReadIterable :: (Typeable typ, ReadIterable typ Object) => DaoClassDefM typ ()
autoDefReadIterable = defReadIterable readIter

-- | The callback function defined here is used if an object of your @typ@ is ever used in a @for@
-- statement in a Dao program. However it is much better to instantiate your @typ@ into the
-- 'UpdateIterable' class and use 'autoDefIterator' instead. If 'defReadIterator' is also defined,
-- the read iterator is always ignored in favor of this function.
defUpdateIterable :: Typeable typ => (typ -> Maybe Object -> Exec typ) -> DaoClassDefM typ ()
defUpdateIterable iter = _updHDIfcBuilder(\st->st{objIfcUpdateIterable=Just iter})

-- | Define 'defUpdateIterable' automatically using the instance of @typ@ in the 'ReadIterable'
-- class.
autoDefUpdateIterable :: (Typeable typ, UpdateIterable typ (Maybe Object)) => DaoClassDefM typ ()
autoDefUpdateIterable = defUpdateIterable updateIter

-- | The callback function defined here is used at any point in a Dao program where an expression
-- containing your object typ is subscripted with square brackets, for example in the statement:
-- @x = t[0][A][B];@ The object passed to your callback function is the object containing the
-- subscript value. So in the above example, if the local variable @t@ is a value of
-- your @typ@, this callback function will be evaluated three times:
-- 1.  with the given 'Object' parameter being @('OInt' 0)@ and the @typ@ parameter as the value
--     stored in the local variable @y@.
-- 2.  once with the 'Object' parameter being the result of dereferencing the local varaible @A@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 3.  once the given 'Object' parameter being the result of dereferencing the local variable @B@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 
-- Statements like this:
-- > ... = a[0,1,2]
-- access a single multi-dimensional index, in this case 3-dimensions with the tuple [0,1,2].
-- > ... = a[0][1][2]
-- accesses a sequence of single-dimensional elements, each element being accessed by the next
-- snigle-dimensional index in the sequence. Although this is one method of programming
-- multi-dimensional data types, it is evaluated differently than an index expressed as a tuple.
defIndexer :: Typeable typ => (typ -> [Object] -> Exec Object) -> DaoClassDefM typ ()
defIndexer fn = _updHDIfcBuilder(\st->st{objIfcIndexer=Just fn})

-- | The callback function defined here is used at any point in a Dao program where an expression
-- containing your object typ is subscripted with square brackets on the left-hand side of an
-- assignment expression:
-- @x[0][A][B] = t;@
-- This function must take the original object of your @typ@ and return the updated object along
-- with the value used to updated it.  The object passed to your callback function is the object
-- containing the subscript value. So in the above example, if the local variables @x@ is a value of
-- your @typ@, this callback function will be evaluated three times:
-- 1.  with the given 'Object' parameter being @('OInt' 0)@ and the @typ@ parameter as the value
--     stored in the local variable @y@.
-- 2.  once with the 'Object' parameter being the result of dereferencing the local varaible @A@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 3.  once the given 'Object' parameter being the result of dereferencing the local variable @B@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 
-- Statements like this:
-- > a[0,1,2] = ...
-- access a single multi-dimensional index, in this case 3-dimensions with the tuple [0,1,2].
-- > a[0][1][2] = ...
-- accesses a sequence of single-dimensional elements, each element being accessed by the next
-- snigle-dimensional index in the sequence. Although this is one method of programming
-- multi-dimensional data types, it is evaluated differently than an index expressed as a tuple.
defIndexUpdater
  :: Typeable typ
  => (typ -> (Maybe Object -> Exec (Maybe Object)) -> [Object] -> Exec (Maybe typ))
  -> DaoClassDefM typ ()
defIndexUpdater fn = _updHDIfcBuilder(\st->st{ objIfcIndexUpdater=Just fn })

-- | Define a function used by the built-in "size()" function to return an value indicating the size
-- of your @typ@ object.
defSizer :: Typeable typ => (typ -> Exec Object) -> DaoClassDefM typ ()
defSizer fn = _updHDIfcBuilder(\st->st{objIfcSizer=Just fn})

-- | Use your data type's instantiation of 'ToDaoStructClass' to call 'defToStruct'.
autoDefToStruct :: forall typ . (Typeable typ, ToDaoStructClass typ) => DaoClassDefM typ ()
autoDefToStruct = defToStruct ((predicate :: Predicate ExecControl T_struct -> Exec T_struct) . fmapPFail ((\o -> mkExecError{ execReturnValue=Just o}) . new) . fromData toDaoStruct)

-- | When a label referencing your object has a field record accessed, for example:
-- > c = a.b;
-- if your object is referenced by @a@ and the script expression wants to access a record called @b@
-- from within it, then function defined here will be used.
defToStruct :: Typeable typ => (typ -> Exec T_struct) -> DaoClassDefM typ ()
defToStruct encode = _updHDIfcBuilder (\st -> st{ objIfcToStruct=Just encode })

-- | When a label referencing your object has a field record updated, for example:
-- > a.b = c;
-- if your object is referenced by @a@ and the script expression wants to update a record called @b@
-- within it by assigning it the value referenced by @c@, then the function defined here will be
-- used.
autoDefFromStruct :: (Typeable typ, FromDaoStructClass typ) => DaoClassDefM typ ()
autoDefFromStruct = defFromStruct (predicate . fmapPFail ((\o -> mkExecError{ execReturnValue=Just o }) . new) . toData fromDaoStruct)

-- | If for some reason you need to define a tree encoder and decoder for the 'Interface' of your
-- @typ@ without instnatiating 'ToDaoStructClass' or 'FromDaoStructClass', use
-- this function to define the tree encoder an decoder directly
defFromStruct :: Typeable typ => (T_struct -> Exec typ) -> DaoClassDefM typ ()
defFromStruct decode = _updHDIfcBuilder (\st -> st{ objIfcFromStruct=Just decode })

-- | The callback defined here is used when a Dao program makes use of the static initialization
-- syntax of the Dao programming language, which are expression of this form:
-- > a = MyType { paramA=initA, paramB=initB, .... };
-- > a = MyType(param1, param2, ...., paramN) { paramA=initA, paramB=initB, .... };
-- When the interpreter sees this form of expression, it looks up the 'Interface' for your
-- @typ@ and checks if a callback has been defined by 'defDictInit'. If so, then the callback is
-- evaluated with a list of object values passed as the first parameter which contain the object
-- values written in the parentheses, and a 'T_dict' as the second paramter containing the tree
-- structure that was constructed with the expression in the braces.
defDictInit :: Typeable typ => ([Object] -> Exec typ) -> (typ -> [(Object, UpdateOp, Object)] -> Exec typ) -> DaoClassDefM typ ()
defDictInit fa fb = _updHDIfcBuilder(\st->st{objIfcDictInit=Just (fa, fb)})

-- | The callback defined here is used when a Dao program makes use of the static initialization
-- syntax of the Dao programming language, which are expression of this form:
-- > a = MyType { initA, initB, .... };
-- > a = MyType(param1, param2, ...., paramN) { initA, initB, .... };
-- When the interpreter sees this form of expression, it looks up the 'Interface' for your
-- @typ@ and checks if a callback has been defined by 'defDictInit'. If so, then the callback is
-- evaluated with a list of object values passed as the first parameter which contain the object
-- values written in the parentheses, and a 'T_dict' as the second paramter containing the tree
-- structure that was constructed with the expression in the braces.
-- 
-- You can define both 'defDictInit' and 'defListInit', but if both are defined, only 'defDictInit'
-- will be used.
defListInit :: Typeable typ => ([Object] -> Exec typ) -> (typ -> [Object] -> Exec typ) -> DaoClassDefM typ ()
defListInit fa fb = _updHDIfcBuilder(\st->st{objIfcListInit=Just(fa,fb)})

-- | Overload infix operators in the Dao programming language, for example @+@, @*@, or @<<@.
-- 
-- Like with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
--
-- If you define two callbacks for the same 'UpdateOp', this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defInfixOp :: Typeable typ => InfixOp -> (InfixOp -> typ -> Object -> XPure Object) -> DaoClassDefM typ ()
defInfixOp op fn = _updHDIfcBuilder $ \st -> st{objIfcInfixOpTable  = objIfcInfixOpTable  st ++ [(op, fn)] }

-- | Overload prefix operators in the Dao programming language, for example @!@, @~@, @-@, and @+@.
-- 
-- Like with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
-- 
-- If you define two callbacks for the same 'UpdateOp', this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defPrefixOp :: Typeable typ => ArithPfxOp -> (ArithPfxOp -> typ -> XPure Object) -> DaoClassDefM typ ()
defPrefixOp op fn = _updHDIfcBuilder $ \st -> st{objIfcPrefixOpTable = objIfcPrefixOpTable st ++ [(op, fn)] }

defCallable :: Typeable typ => (typ -> Exec [CallableCode]) -> DaoClassDefM typ ()
defCallable fn = _updHDIfcBuilder (\st -> st{objIfcCallable=Just fn})

defDeref :: Typeable typ => (typ -> Exec (Maybe Object)) -> DaoClassDefM typ ()
defDeref  fn = _updHDIfcBuilder (\st -> st{objIfcDerefer=Just fn})

-- | Rocket. Yeah. Sail away with you.
defLeppard :: Typeable typ => rocket -> yeah -> DaoClassDefM typ ()
defLeppard _ _ = return ()

-- | This is the Dao 'Object' interface to the Haskell language. Every function in this data type
-- allows you to customize the behavior of the Dao evaluator for a particular Haskell data type
-- @typ@. In order for your type to be useful, it must be possible to pass your data type to the
-- 'OHaskell' constructor, which requires a data type of 'Data.Dynamic.Dynamic', which means your
-- @typ@ must derive a class instance for 'Data.Typeable.Typeable'. The first parameter of type
-- @typ@ is not used except to retrieve it's 'Data.Typeable.TypeRep' using the
-- 'Data.Typealble.typeOf' function, it is safe to pass any data constructor with all of it's fields
-- 'Prelude.undefined', just the constructor itself must not be 'Prelude.undefined'.
-- 
-- The @'DaoClassDefM'@ parameter you pass to this function is a monadic function so you can simply
-- declare the functionality you would like to include in this object one line at a time using
-- the procedural coding style. Each line in the "procedure" will be one of the @def*@ functions,
-- for example 'autoDefEquality' or 'autoDefOrdering'.
interface :: Typeable typ => typ -> DaoClassDefM typ ig -> Interface typ
interface init defIfc =
  Interface
  { objHaskellType     = typ
  , objCastFrom        = objIfcCastFrom       ifc
  , objEquality        = objIfcEquality       ifc
  , objOrdering        = objIfcOrdering       ifc
  , objBinaryFormat    = objIfcBinaryFormat   ifc
  , objNullTest        = objIfcNullTest       ifc
  , objPPrinter        = objIfcPPrinter       ifc
  , objReadIterable    = objIfcReadIterable   ifc
  , objUpdateIterable  = objIfcUpdateIterable ifc
  , objIndexer         = objIfcIndexer        ifc
  , objIndexUpdater    = objIfcIndexUpdater   ifc
  , objSizer           = objIfcSizer          ifc
  , objToStruct        = objIfcToStruct       ifc
  , objFromStruct      = objIfcFromStruct     ifc
  , objDictInit        = objIfcDictInit       ifc
  , objListInit        = objIfcListInit       ifc
  , objCallable        = objIfcCallable       ifc
  , objDereferencer    = objIfcDerefer        ifc
  , objInfixOpTable    = mkArray "defInfixOp"  $ objIfcInfixOpTable  ifc
  , objArithPfxOpTable = mkArray "defPrefixOp" $ objIfcPrefixOpTable ifc
  }
  where
    typ               = typeOf init
    ifc               = execState (daoClassDefState defIfc) initHDIfcBuilder
    mkArray oiName elems =
      minAccumArray (onlyOnce oiName) Nothing $ map (\ (i, e) -> (i, (i, Just e))) elems
    onlyOnce oiName a (i, b)  = case a of
      Nothing -> b
      Just  _ -> conflict oiName ("the "++show i++" operator")
    conflict oiName funcName = error $ concat $
      [ "'", oiName
      , "' has conflicting functions for ", funcName
      , " for the 'HaskellDataClass' instantiation of the '", show typ
      , "' Haskell data type."
      ]

