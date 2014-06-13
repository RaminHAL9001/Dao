-- "src/Dao/Interpreter.hs"  defines the Dao programming language semantics.
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
    Action(Action), actionTokens, actionPattern, actionMatch, actionCodeBlock,
    makeActionsForQuery, betweenBeginAndEnd, daoShutdown, getLocalRuleSet,
    getGlobalRuleSet, defaultTokenizer, constructPatternWith, constructPattern,
    DaoSetupM(), DaoSetup, haskellType, daoProvides, daoClass, daoConstant, daoFunction,
    daoFunction0, daoInitialize, setupDao, evalDao, DaoFunc, daoFunc, funcAutoDerefParams,
    daoForeignFunc, executeDaoFunc,
    Sizeable(getSizeOf),
    ObjectClass(obj, fromObj, castToCoreType),
    execCastToCoreType, listToObj, listFromObj, new, opaque, objFromHata,
    Struct(Nullary, Struct), structLookup,
    ToDaoStructClass(toDaoStruct), toDaoStructExec, pPrintStructForm,
    FromDaoStructClass(fromDaoStruct), withFromDaoStructExec, fromDaoStructExec,
    StructError(StructError),
    structErrName, structErrField, structErrValue, structErrExtras,
    mkLabel, mkStructName, mkFieldName,
    ToDaoStruct(),
    fromData, innerToStruct, innerToStructWith, renameConstructor, makeNullary, putNullaryUsingShow,
    define, optionalField, setField, defObjField, (.=), putObjField, (.=@), defMaybeObjField, (.=?),
    FromDaoStruct(),
    toData, constructor, innerFromStruct, nullary, getNullaryWithRead, structCurrentField,
    tryCopyField, tryField, copyField, field,
    convertFieldData, req, opt, reqList, optList,
    ObjectUpdate, ObjectTraverse,
    ObjectLens(updateIndex), ObjectFunctor(objectFMap),
    ObjectFocus, ObjFocusState(), getFocalReference,
    execToFocusUpdater, withInnerLens, runObjectFocus, focusObjectClass, focusStructAsDict,
    focusLiftExec, focalPathSuffix, focusGuardStructName, updateHataAsStruct, callMethod,
    innerDataUpdateIndex, referenceUpdateName, referenceLookupName,
    Object(
      ONull, OTrue, OType, OInt, OWord, OLong,
      OFloat, ORatio, OComplex, OAbsTime, ORelTime,
      OChar, OString, ORef, OList, ODict, OTree, OBytes, OHaskell
    ),
    T_type, T_int, T_word, T_long, T_ratio, T_complex, T_float, T_time, T_diffTime,
    T_char, T_string, T_ref, T_bytes, T_list, T_dict, T_struct,
    isNumeric, typeMismatchError,
    initializeGlobalKey, destroyGlobalKey, evalTopLevelAST,
    Reference(Reference, RefObject), reference, refObject, referenceHead, refUnwrap,
    refNames, referenceFromUStr, fmapReference, setQualifier, modRefObject,
    refAppendSuffix, referenceLookup, referenceUpdate,
    CoreType(
      NullType, TrueType, TypeType, IntType, WordType, DiffTimeType, FloatType,
      LongType, RatioType, ComplexType, TimeType, CharType, StringType, RefType,
      ListType, DictType, TreeType, BytesType, HaskellType
    ),
    typeOfObj, coreType, hataType, objTypeFromCoreType, objTypeFromName,
    TypeSym(CoreType, TypeSym, TypeVar), TypeStruct(TypeStruct), ObjType(ObjType), typeChoices,
    RefSuffix(NullRef, DotRef, Subscript, FuncCall),
    refSuffixHead, refSuffixHasFuncCall, refSuffixToList, dotRef, subscript, funcCall,
    Complex(Complex),
    realPart, imagPart, mkPolar, cis, polar, magnitude, phase, conjugate, complex,
    minAccumArray, minArray,
    FuzzyStr(FuzzyStr),
    ExecUnit(), ExecTokenizer(ExecTokenizer), runExecTokenizer,
    globalMethodTable, defaultTimeout, importGraph, currentWithRef, taskForExecUnits,
    currentQuery, currentPattern, currentBranch, providedAttributes, programModuleName,
    preExec, postExec, quittingTime, programTokenizer, currentCodeBlock, ruleSet,
    newExecUnit, inModule,
    Task(), initTask, throwToTask, killTask, taskLoop, taskLoop_,
    Executable(execute), DerefAssignExpr,
    ExecRef(execReadRef, execTakeRef, execPutRef, execSwapRef, execModifyRef, execModifyRef_),
    ExecControl(ExecReturn, ExecError), execReturnValue,
    execErrorMessage, execErrorInModule, execErrorLocation, execErrorSubtype, execErrorInfo,
    ExecErrorSubtype(
      ExecErrorUntyped, ExecThrow, ExecStructError, ExecUndefinedRef, ExecTypeError,
      ExecUpdateOpError, ExecInfixOpError, ExecIOException, ExecHaskellError
    ),
    errInFunc, errInConstr, errInInitzr, errOfReference, argNum, numArgsPassed,
    expectNumArgs, exectDimension, expectType, actualType, leftSideType, rightSideType,
    modifiedConst, assertFailed, returnedVoid, errorDict,
    newError, throwArityError, throwParseError, throwBadTypeError, errLocation, errModule,
    errCurrentModule, errInfo, updateExecErrorInfo, logUncaughtErrors, clearUncaughtErrorLog,
    Exec(Exec), execToPredicate, XPure(XPure), xpureToState, runXPure, evalXPure, xpure, xobj,
    xnote, xonUTF8, xmaybe,
    ExecThrowable(toExecErrorInfo, execThrow), ioExec,
    ExecHandler(ExecHandler), execHandler,
    newExecIOHandler, execCatchIO, execHandleIO, execIOHandler,
    execErrorHandler, catchReturn, execNested, execNested_, execFuncPushStack,
    execFuncPushStack_, execWithStaticStore, execWithWithRefStore, withExecTokenizer,
    Subroutine(Subroutine), setupCodeBlock,
    origSourceCode, staticVars, staticRules, staticLambdas, executable, runCodeBlock, runCodeBlock_,
    RuleSet(), CallableCode(CallableCode), argsPattern, returnType, codeSubroutine, 
    PatternRule(PatternRule), rulePatterns, ruleAction, 
    asReference, asInteger, asRational, asPositive, asComplex, objConcat,
    objToBool, extractStringElems, requireAllStringArgs,
    shiftLeft, shiftRight,
    evalArithPrefixOp, evalInfixOp, evalUpdateOp, runTokenizerWith, runTokenizer, makePrintFunc,
    paramsToGlobExpr, matchFuncParams, execGuardBlock, objToCallable, callCallables,
    callObject, checkPredicate, checkVoid,
    evalConditional,
    localVarDefine, localVarUpdate, localVarLookup, maybeDerefObject, derefObjectGetReference, derefObject,
    updateExecError,
    assignUnqualifiedOnly,
    LimitedObject(LimitedObject, unlimitObject),
    MethodTable(), execGetObjTable, lookupMethodTable,
    ReadIterable(readForLoop), UpdateIterable(updateForLoop),
    HataClass(haskellDataInterface), toHata, fromHata,
    InitItem(InitSingle, InitAssign),
    Interface(),
    objCastFrom, objEquality, objOrdering, objBinaryFormat, objNullTest, objPPrinter,
    objSizer, objIndexer, objIndexUpdater, objToStruct, objFromStruct, objInitializer, objTraverse,
    objInfixOpTable, objArithPfxOpTable, objCallable, objDereferencer,
    interfaceAdapter, interfaceToDynamic,
    DaoClassDefM(), interface, DaoClassDef,
    defCastFrom, autoDefEquality, defEquality, autoDefOrdering, defOrdering, autoDefBinaryFmt,
    defBinaryFmt, autoDefNullTest, defNullTest, defPPrinter, autoDefPPrinter, defReadIterable,
    autoDefReadIterable, defUpdateIterable, autoDefUpdateIterable, defIndexer, defIndexUpdater,
    defSizer, autoDefSizeable, autoDefToStruct, defToStruct, autoDefFromStruct, defFromStruct,
    defInitializer, defTraverse, autoDefTraverse, defInfixOp, defPrefixOp, defCallable, defDeref,
    defMethod, defMethod0, defLeppard
  )
  where

import           Dao.Glob
import           Dao.PPrint
import           Dao.Predicate
import           Dao.Random
import           Dao.Stack
import           Dao.String
import           Dao.Token
import           Dao.RefTable
import qualified Dao.HashMap as H
import qualified Dao.Binary  as B
import qualified Dao.EnumSet as Es
import qualified Dao.Tree    as T
import           Dao.Interpreter.Tokenizer
import           Dao.Interpreter.AST

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

#if 0
import Debug.Trace
import System.IO
strace :: PPrintable s => String -> s -> s
strace msg s = trace (msg++": "++prettyShow s) s
dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr . ("(DEBUG) "++) . (>>=(\c -> if c=='\n' then "\n(DEBUG) " else [c]))
dbg' :: MonadIO m => String -> m a -> m a
dbg' msg f = f >>= \a -> dbg msg >> return a
dbg0 :: (MonadPlus m, MonadIO m, MonadError e m, PPrintable e) => String -> m a -> m a
dbg0 msg f = do
  dbg (msg++" (BEGIN)")
  catchError
    (mplus (f >>= \a -> dbg (msg++" (DONE)") >> return a) (dbg (msg++" (BACKTRACKED)") >> mzero))
    (\e -> dbg (msg++" (ERROR) "++prettyShow e) >> throwError e)
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

-- | An 'Action' is the result of a pattern match that occurs during an input string query. It is a
-- data structure that contains all the information necessary to run an 'Subroutine' assocaited with
-- a 'Glob', including the parent 'ExecUnit', the 'Dao.Glob.Glob' and the 'Dao.Glob.Match' objects,
-- and the 'Executables'. Use 'execute' to evaluate a 'Action' in the current thread.
-- 
-- To execute an action in a separate thread, use 'forkExecAction'.
data Action
  = Action
    { actionTokens    :: [Object]
    , actionPattern   :: Glob Object
    , actionMatch     :: M.Map Name Object
    , actionCodeBlock :: Subroutine
    }
  deriving (Eq, Typeable)

instance PPrintable Action where { pPrint = pPrintStructForm }

instance ToDaoStructClass Action where
  toDaoStruct = renameConstructor "Action" $ do
    "tokens"  .=@ actionTokens
    "pattern" .=@ actionPattern
    "match"   .=@ actionMatch
    "code"    .=@ actionCodeBlock

instance FromDaoStructClass Action where
  fromDaoStruct = do
    constructor "Action"
    let err name o = flip (execThrow "match dictionary items must contain lists of tokens") [] $
          StructError
          { structErrName   = Nothing
          , structErrField  = Just (toUStr name)
          , structErrValue  = Just $ obj (typeOfObj o)
          , structErrExtras = []
          }
    let fmtMatches = fmap M.fromList .
          mapM (\ (name, o) -> xmaybe (fromObj o) <|> err name o >>= return . (,) name) . M.assocs
    return Action <*> req "tokens" <*> req "pattern" <*> (req "match" >>= fmtMatches) <*> req "code"

instance ObjectClass Action where { obj=new; fromObj=objFromHata; }

instance HataClass Action where
  haskellDataInterface = interface "Action" $ do
    autoDefEquality >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

instance Executable Action (Maybe Object) where
  execute act = do
    cq  <- gets currentQuery
    cp  <- gets currentPattern
    ccb <- gets currentCodeBlock
    let setVar :: ObjectClass o => String -> (Action -> o) -> T_dict -> T_dict
        setVar name f = M.insert (ustr name) (obj $ f act)
    let setTokensVar = setVar "tokens" actionTokens
    let setSelfVar = M.union (M.singleton (ustr "self") (obj $ setTokensVar $ mempty))
    let localVars = setSelfVar $ actionMatch act
    modify $ \xunit -> 
      xunit
      { currentQuery     = Just $ actionTokens act
      , currentPattern   = Just $ actionPattern act
      , currentCodeBlock = Just $ actionCodeBlock act
      }
    success <- optional $ runCodeBlock_ localVars (actionCodeBlock act)
    modify (\xunit -> xunit{ currentQuery=cq, currentPattern=cp, currentCodeBlock=ccb })
    xmaybe success

instance Executable [Action] [Object] where
  execute = fmap concat .
    mapM (\act -> catchPredicate (execute act) >>= \p -> case p of
             OK                (Just o)  -> return [o]
             PFail (ExecReturn (Just o)) -> return [o]
             PFail              err      -> logUncaughtErrors [err] >> return []
             _                           -> return []
         )

-- | Using an 'ExecTokenizer' function, break up a string into a list of tokens, returning them as a
-- 'TokenList' object. Input paramaters can be strings or lists of strings. If an input paramter is
-- a string, it is tokenized to a list of strings. If an input parameter is a list of strings, it is
-- considered to be already tokenized and simply appended to list of tokens produced by previous
-- parameters.
runTokenizerWith :: ExecTokenizer -> [Object] -> Exec [Object]
runTokenizerWith tok ox = fmap concat $ forM ox $ \o -> case o of
  OString o -> fmap obj <$> runExecTokenizer tok o
  OList  ox -> return ox
  o         -> return [o]

-- | Like 'runTokenizerWith', but uses the default tokenizer function set for this module.
runTokenizer :: [Object] -> Exec [Object]
runTokenizer ox = gets programTokenizer >>= \tok -> runTokenizerWith tok ox

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
makeActionsForQuery :: [PatternTree Object [Subroutine]] -> [Object] -> Exec [Action]
makeActionsForQuery tree tokens = do
  let match = matchTree False (T.unionsWith (++) tree) tokens
  fmap concat $ forM match $ \ (patn, match, execs) -> do
    match <- catchPredicate $ fmap M.fromList $ -- evaluate pattern type checkers
      forM (M.assocs match) $ \ (name, (vartyp, ox)) -> case vartyp of
        Nothing     -> return (name, obj ox)
        Just vartyp -> do
          match <- catchPredicate $ referenceLookup $ Reference UNQUAL vartyp $ FuncCall [OList ox] NullRef
          case match of
            OK (_, Nothing)             -> return (name, obj ox)
            OK (_, Just  o)             -> return (name, o)
            PFail (ExecReturn Nothing)  -> return (name, obj ox)
            PFail (ExecReturn (Just o)) -> return (name, o)
            PFail err                   -> throwError err
            Backtrack                   -> mzero
    case match of
      Backtrack -> return []
      PFail err -> logUncaughtErrors [err] >> return []
      OK  match -> return $
        flip fmap execs $ \exec -> deepseq exec $! deepseq tokens $! deepseq patn $! deepseq match $!
          Action
          { actionPattern   = patn
          , actionTokens    = fmap obj tokens
          , actionMatch     = match
          , actionCodeBlock = exec
          }

-- | Evaluate an executable function between evaluating all of the "BEGIN{}" and "END{}" statements.
betweenBeginAndEnd :: Exec a -> Exec a
betweenBeginAndEnd runInBetween = get >>= \xunit -> do
  -- Run all "BEGIN{}" procedures.
  mapM_ execute (preExec xunit)
  clearUncaughtErrorLog
  -- Run the given function, presumably it performs a string execution.
  a <- runInBetween
  -- Update the "global this" pointer to include the uncaught exceptions.
  errs <- OList . map new <$> clearUncaughtErrorLog
  let upd = M.union (M.singleton (ustr "errors") errs)
  referenceUpdate (Dao.Interpreter.reference GLOBAL (ustr "self")) False $ \o ->
    return $ Just $ ODict $ upd $ case o of { Just (ODict o) -> o; _ -> mempty; }
  -- Run all "END{}" procedures.
  mapM_ execute (postExec xunit)
  return a

-- | Evaluates the @EXIT@ scripts for every presently loaded dao program, and then clears the
-- 'Dao.Interpreter.importGraph', effectively removing every loaded dao program and idea file from memory.
daoShutdown :: Exec ()
daoShutdown = (M.elems <$> gets importGraph) >>=
  mapM_ (\xunit -> inModule xunit $ gets quittingTime >>= mapM_ execute)

-- | Returns a list of @'PatternTree' 'Object' ['Subroutine']@ objects from the global rule set and
-- the local rule set.
getLocalRuleSet :: Exec [PatternTree Object [Subroutine]]
getLocalRuleSet = do
  sub <- gets currentCodeBlock
  return $ maybe [] (return . staticRules) sub

-- | Returns a list of @'PatternTree' 'Object' ['Subroutine']@ objects from the global rule set and
-- the local rule set.
getGlobalRuleSet :: Exec [PatternTree Object [Subroutine]]
getGlobalRuleSet = return <$> gets ruleSet

_mkDoFunc :: String -> [Exec [PatternTree Object [Subroutine]]] -> (DaoFunc (), DaoFunc (), DaoFunc ())
_mkDoFunc name selectors = (mkDo, mkDoAll, mkQuery) where
  run f () ox = do
    inTrees <- concat <$> sequence selectors
    flip (,) () <$> (runTokenizer ox >>= makeActionsForQuery inTrees >>= f)
  mkQuery = daoFunc{daoFuncName=ustr("query"++name), daoForeignFunc=run(return . Just . obj . fmap obj)}
  mkDo    = daoFunc{daoFuncName=ustr("do"   ++name), daoForeignFunc=run(betweenBeginAndEnd . msum . fmap execute)}
  mkDoAll = daoFunc{daoFuncName=ustr("doAll"++name), daoForeignFunc=run(betweenBeginAndEnd . fmap (Just . obj) . execute)}

builtin_do    :: DaoFunc ()
builtin_doAll :: DaoFunc ()
builtin_query :: DaoFunc ()
(builtin_do, builtin_doAll, builtin_query) = _mkDoFunc "" [getLocalRuleSet, getGlobalRuleSet]

builtin_doLocal    :: DaoFunc ()
builtin_doAllLocal :: DaoFunc ()
builtin_queryLocal :: DaoFunc ()
(builtin_doLocal, builtin_doAllLocal, builtin_queryLocal) = _mkDoFunc "Local" [getLocalRuleSet]

builtin_doGlobal    :: DaoFunc ()
builtin_doAllGlobal :: DaoFunc ()
builtin_queryGlobal :: DaoFunc ()
(builtin_doGlobal, builtin_doAllGlobal, builtin_queryGlobal) = _mkDoFunc "Global" [getGlobalRuleSet]

----------------------------------------------------------------------------------------------------

-- | When a 'Dao.Interpreter.AST.RuleExpr' is evaluated to an 'Object', it takes this form.
-- 'PatternRule' instantiats 'Executable' such that 'execute' converts it to a 'PatternTree'.
data PatternRule
  = PatternRule{ rulePatterns :: [Object], ruleAction :: Subroutine }
  deriving (Show, Typeable)

instance NFData PatternRule where { rnf (PatternRule a b) = deepseq a $! deepseq b () }

instance HasNullValue PatternRule where
  nullValue = PatternRule{rulePatterns=[], ruleAction=nullValue}
  testNull (PatternRule a b) = null a && testNull b

instance PPrintable PatternRule where
  pPrint (PatternRule pats exe) = (\a -> ppCallableAction "rule" a nullValue exe) $ case pats of
    []    -> pString "()"
    [pat] -> pPrint pat
    pats  -> pList (pString "rule") "(" ", " ")" (map pPrint pats)

instance ToDaoStructClass PatternRule where
  toDaoStruct = renameConstructor "PatternRule" $ do
    "patterns" .=@ rulePatterns
    "action"   .=@ ruleAction

instance FromDaoStructClass PatternRule where
  fromDaoStruct = return PatternRule <*> req "patterns" <*> req "action"

instance Executable PatternRule (PatternTree Object [Subroutine]) where
  execute (PatternRule{ rulePatterns=pats, ruleAction=sub }) = do
    globs <- mapM (constructPattern . return) pats
    return $ insertMultiPattern (++) globs [sub] mempty

instance ObjectClass PatternRule where { obj=new; fromObj=objFromHata; }

instance HataClass PatternRule where
  haskellDataInterface = interface "PatternRule" $ do
    autoDefPPrinter >> autoDefToStruct >> autoDefFromStruct

defaultTokenizer :: ExecTokenizer
defaultTokenizer = ExecTokenizer $ return . fmap obj . simpleTokenizer . uchars

-- | This function takes a list of objects and constructs a list of @('Dao.Glob.Glob' 'Object')@s to
-- be inserted into a 'Dao.Glob.PatternTree' object. The input list of @['Object']@s will each form
-- a single pattern, then all of the patterns are unioned together to form the pattern tree. This
-- means token strings matched against the resulting @('Dao.Glob.Glob' 'Object')@ constructed by
-- this function will match any and all of the patterns.  If any of the objects in the input list
-- are strings, the strings will be parsed into 'Dao.Glob.Glob' objects, and each string constant
-- within the 'Dao.Glob.Glob' object will be further tokenized with the 'programTokenizer'.
constructPatternWith :: ExecTokenizer -> [Object] -> Exec (Glob Object)
constructPatternWith tok = fmap (mconcat . mconcat) . mapM (derefObject>=>construct) where
  construct o = case o of
    OString o -> case readsPrec 0 (uchars o) of
      [(glob, "")] -> fmap return $ parseOverSinglesM glob $ \str -> case str of
        ""  -> return []
        str -> fmap obj <$> runExecTokenizer tok (ustr str)
      _ -> execThrow "unable to parse pattern" ExecErrorUntyped []
    OList ox -> return [makeGlob $ fmap Single ox]
    OHaskell (Hata _ d) -> do
      let err = throwBadTypeError "could not use as pattern expression" o []
      maybe err return $ msum $
        [ return <$> fromDynamic d
        , fmap (makeGlob . fst) . T.assocs . ruleSetRules <$> fromDynamic d
        ]
    _ -> throwBadTypeError "could not create pattern from data of type" o []

-- | Like 'constructPatternWith' but uses the default 'ExecTokenizer' that has been set for the
-- current 'ExecUnit'.
constructPattern :: [Object] -> Exec (Glob Object)
constructPattern ox = gets programTokenizer >>= flip constructPatternWith ox

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
haskellType :: HataClass o => o
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

-- | Associate an 'HataClass' with a 'Name'. This 'Name' will be callable from within Dao scripts.
-- > newtype MyClass = MyClass { ... } deriving (Eq, Ord)
-- >
-- > instance 'HataClass' MyClass where
-- >     'haskellDataInterface' = 'interface' $ do
-- >         'autoDefEquality'
-- >         'autoDefOrdering'
-- >         ...
-- >
-- > setupDao :: 'DaoSetup'
-- > setupDao = do
-- >     daoClass "myClass" (haskellType::MyClass)
-- >     ...
daoClass :: (Typeable o, HataClass o) => o -> DaoSetup
daoClass ~o = _updateSetupModState $ \st ->
  st{ daoClasses = _insertMethodTable o haskellDataInterface (daoClasses st) }

-- | Define a built-in top-level function that is not a member method of any object. Examples of
-- built-in functions provided in this module are "println()" and "typeof()".
daoFunction :: (Show name, UStrType name) => name -> DaoFunc () -> DaoSetup
daoFunction name func = _updateSetupModState $ \st -> let nm = (fromUStr $ toUStr name) in
  st{ daoSetupConstants = M.insert nm (new $ func{ daoFuncName=nm }) (daoSetupConstants st) }

-- | Like 'daoFunction' but creates a function that takes no parameters.
daoFunction0 :: Name -> Exec (Maybe Object) -> DaoSetup
daoFunction0 name f = daoFunction name $
  DaoFunc
  { daoFuncClass = []
  , daoFuncName  = nil
  , funcAutoDerefParams = False
  , daoForeignFunc = \ () ox -> case ox of
      [] -> flip (,) () <$> f
      _  -> throwArityError "" 0 ox [(errInFunc, obj $ reference UNQUAL name)]
  }

-- | Define a constant value for any arbitrary 'Object'.
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
setupDao :: DaoSetup -> IO (Predicate ExecControl ())
setupDao setup0 = do
  let setup = execState (daoSetupM $ loadEssentialFunctions >> setup0) $
        SetupModState
        { daoSatisfies      = M.empty
        , daoSetupConstants = M.empty
        , daoClasses        = mempty
        , daoEntryPoint     = return ()
        }
  xunit  <- _initExecUnit
  fmap fst $ ioExec (daoEntryPoint setup) $
    xunit
    { providedAttributes = daoSatisfies setup
    , builtinConstants   = daoSetupConstants setup
    , globalMethodTable  = daoClasses setup
    }

-- | Simply run a single 'Exec' function in a fresh environment with no setup, and delete the
-- envrionment when finished returning only the 'Dao.Predicate.Predicate' result of the 'Exec'
-- evaluation. If you want to have more control over the runtime in which the 'Exec' function runs,
-- use 'setupDao' with 'daoInitialize'.
evalDao :: Exec a -> IO (Predicate ExecControl a)
evalDao f = _initExecUnit >>= fmap fst . ioExec f

----------------------------------------------------------------------------------------------------

-- | All object methods that operate on object data types built-in to the Dao language, or built-in
-- to a library extending the Dao language, are stored in 'Data.Map.Map's from the functions name to
-- an object of this type.
--
-- The @this@ of this function is the data type of what languages like C++ or Java would call the
-- "self" variable. 'DaoFunc's where the @this@ is () are considered ordinary functions that do not
-- operate on any object apart from their input parameters.
data DaoFunc this
  = DaoFunc
    { daoFuncClass        :: [Name]
    , daoFuncName         :: Name
    , funcAutoDerefParams :: Bool
    , daoForeignFunc      :: this -> [Object] -> Exec (Maybe Object, this)
    }
  deriving Typeable
instance Eq   (DaoFunc this) where { a == b = daoFuncName a == daoFuncName b; }
instance Ord  (DaoFunc this) where { compare a b = compare (daoFuncName a) (daoFuncName b) }
instance Show (DaoFunc this) where
  show func =
    if null (daoFuncClass func)
    then uchars (daoFuncName func)
    else foldr (\name str -> uchars name ++ "." ++ str) (uchars $ daoFuncName func) (daoFuncClass func)
instance PPrintable (DaoFunc this) where { pPrint = pShow }

-- | Use this as the constructor of a 'DaoFunc'. By default the @this@ type is (). To change the
-- @this@ type, simply supply a different function type for the 'daoForeignFunc' field. For example:
-- > daoFunc{ daoFuncName=ustr "add", daoForeignFunc = retrun . (+1) } :: DaoFunc Int
daoFunc :: DaoFunc typ
daoFunc =
  DaoFunc
  { daoFuncClass        = []
  , daoFuncName         = nil
  , funcAutoDerefParams = True
  , daoForeignFunc      = \typ _ -> return (Nothing, typ)
  }

-- | Execute a 'DaoFunc' 
executeDaoFunc :: DaoFunc this -> this -> [Object] -> Exec (Maybe Object, this)
executeDaoFunc fn this params = do
  args <- (if funcAutoDerefParams fn then mapM derefObject else return) params
  pval <- catchPredicate (daoForeignFunc fn this args)
  case pval of
    OK            (o, this) -> return (o, this)
    PFail (ExecReturn    o) -> return (o, this)
    PFail              err  -> throwError err
    Backtrack               -> mzero

-- Evaluate this function as one of the instructions in the monadic function passed to the
-- 'setupDao' function in order to install the most fundamental functions into the Dao evaluator.
-- This function must be evaluated in order to have access to the following functions:
-- > print, join, defined, delete
loadEssentialFunctions :: DaoSetup
loadEssentialFunctions = do
  daoClass (haskellType :: H.HashMap Object Object)
  daoClass (haskellType :: RuleSet)
  daoClass (haskellType :: Pair)
  daoFunction "print"    builtin_print
  daoFunction "println"  builtin_println
  daoFunction "join"     builtin_join
  daoFunction "str"      builtin_str
  daoFunction "quote"    builtin_quote
  daoFunction "concat"   builtin_concat
  daoFunction "concat1"  builtin_concat1
  daoFunction "reverse"  builtin_reverse
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
  daoFunction "ref"      builtin_ref
  daoFunction "defined"  builtin_check_if_defined
  daoFunction "delete"   builtin_delete
  daoFunction "typeof"   builtin_typeof
  daoFunction "sizeof"   builtin_sizeof
  daoFunction "call"     builtin_call
  daoFunction "toHash"   builtin_toHash
  daoFunction "fromHash" builtin_fromHash
  daoFunction "tokenize" builtin_tokenize
  daoFunction "query"    builtin_query
  daoFunction "doAll"    builtin_doAll
  daoFunction "do"       builtin_do
  daoFunction "fromStruct"  builtin_fromStruct
  daoFunction "toStruct"    builtin_toStruct
  daoFunction "queryGlobal" builtin_queryGlobal
  daoFunction "doAllGlobal" builtin_doAllGlobal
  daoFunction "doGlobal"    builtin_doGlobal
  daoFunction "queryLocal"  builtin_queryLocal
  daoFunction "doAllLocal"  builtin_doAllLocal
  daoFunction "doLocal"     builtin_doLocal
  daoFunction "HashMap"     builtin_HashMap
  daoFunction "assocs"      builtin_assocs
  daoFunction "Pair"        builtin_Pair
  mapM_ (uncurry daoConstant) $ flip fmap [minBound..maxBound] $ \t ->
    (toUStr $ show t, OType $ objTypeFromCoreType t)

instance ObjectClass (DaoFunc ())      where { obj=new; fromObj=objFromHata; }
instance ObjectClass (DaoFunc Dynamic) where { obj=new; fromObj=objFromHata; }
instance ObjectClass (DaoFunc Hata)    where { obj=new; fromObj=objFromHata; }
instance ObjectClass (DaoFunc Object)  where { obj=new; fromObj=objFromHata; }

instance HataClass (DaoFunc ()) where
  haskellDataInterface = interface "Builtin_Function" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter

instance HataClass (DaoFunc Dynamic) where
  haskellDataInterface = interface "Builtin_Dynamic_Method" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter

instance HataClass (DaoFunc Hata) where
  haskellDataInterface = interface "Builtin_Haskell_Data_Method" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter

instance HataClass (DaoFunc Object) where
  haskellDataInterface = interface "Builtin_Object_Method" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

-- | This class provides a consistent interface, the 'obj' function, for converting a wide range of
-- types to an 'Object' type.
class ObjectClass o where
  obj            :: o -> Object
  fromObj        :: Object -> Maybe o
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
    _          -> castToCoreType t . uchars

instance ObjectClass String where
  obj = obj . toUStr
  fromObj = fromObj >=> maybeFromUStr
  castToCoreType t = case t of
      NullType   -> \o -> guard (o=="null") >> return ONull
      TrueType   -> \o -> case map toLower o of
        "true"   -> return OTrue
        "yes"    -> return OTrue
        "no"     -> return ONull
        "false"  -> return ONull
        "null"   -> return ONull
        _        -> mzero
      IntType    -> pars OInt
      WordType   -> pars OWord
      LongType   -> pars OLong
      FloatType  -> pars OFloat
      TimeType   -> pars OAbsTime
      StringType -> return . OString . ustr
      RefType    -> pars ORef
      _          -> \ _ -> mzero
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
    OString o -> maybeFromUStr o
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
  obj = OType . objTypeFromCoreType
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

instance ObjectClass Hata where
  obj = OHaskell
  fromObj o = case o of { OHaskell o -> return o; _ -> mzero; }

instance ObjectClass Dynamic where
  obj = opaque
  fromObj o = case o of { OHaskell (Hata _ o) -> return o; _ -> mzero; }

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

instance ObjectClass Location where { obj=new; fromObj=objFromHata; }

instance ObjectClass Comment where { obj=new; fromObj=objFromHata; }

instance ObjectClass [Comment] where { obj=listToObj; fromObj=listFromObj; }

instance ObjectClass DotNameExpr where { obj=new; fromObj=objFromHata; }

instance ObjectClass AST_DotName where { obj=new; fromObj=objFromHata; }

instance ObjectClass DotLabelExpr where { obj=new; fromObj=objFromHata; }

instance ObjectClass AST_DotLabel where { obj=new; fromObj=objFromHata; }

listToObj :: ObjectClass o => [o] -> Object
listToObj = OList . map obj

listFromObj :: ObjectClass o => Object -> Maybe [o]
listFromObj o = case o of
  OList o -> mapM fromObj o
  _       -> mzero

-- | Create a new 'Object' containing the original value and a reference to the 'Interface'
-- retrieved by the instance of 'haskellDataInterface' for the data type.
new :: (HataClass typ, Typeable typ) => typ -> Object
new = OHaskell . toHata

-- | Create a completely opaque haskell data type that can be used stored to a Dao language
-- variable, but never inspected or modified in any way.
opaque :: forall typ . Typeable typ => typ -> Object
opaque o = OHaskell $ flip Hata (toDyn o) $
  interfaceToDynamic (interface (show $ typeOf o) (return ()) :: Interface typ)

-- | The inverse operation of 'new', uses 'fromObj' and 'fromHata' to extract the data type
-- wrapped up in the 'Object', assuming the 'Object' is the 'OHaskell' constructor holding a
-- 'Hata' container.
objFromHata :: (Typeable o, HataClass o) => Object -> Maybe o
objFromHata = fromObj >=> fromHata

----------------------------------------------------------------------------------------------------

-- | This is the "Haskell Data" data type used to wrap-up a Haskell data types into a
-- 'Data.Dynamic.Dynamic' data type and associate this dynamic data with the 'Interface' used by the
-- runtime to read and modify the data. Whenever an non-primitive 'Object' is created, the data is
-- converted to a 'Data.Dynamic.Dynamic' value and paired with a copy of the 'Interface'.
data Hata = Hata (Interface Dynamic) Dynamic deriving Typeable

instance Eq Hata where
  Hata ifcA a == Hata ifcB b =
    ((ifcA==ifcB)&&) $ maybe False id $ objEquality ifcA >>= \eq -> return (eq a b)

instance Ord Hata where
  compare (Hata ifcA a) (Hata ifcB b) = maybe err id $
    guard (ifcA==ifcB) >> objOrdering ifcA >>= \comp -> return (comp a b) where
      err = error $ unwords $
        [ "cannot compare object of type", show (objHaskellType ifcA)
        , "with obejct of type", show (objHaskellType ifcB)
        ]

instance Show Hata where { show (Hata o _) = show (objHaskellType o) }

instance NFData Hata where { rnf (Hata _ _) = () }

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
    OHaskell (Hata ifc o) -> case objPPrinter ifc of
      Nothing -> fail $ "cannot pretty print Haskell data type: "++show (objHaskellType ifc)
      Just pp -> pp o

instance B.Binary Hata MTab where
  put (Hata ifc o) = do
    let typeName = objInterfaceName ifc 
    mtab <- B.getCoderTable
    case B.getEncoderForType typeName mtab of
      Just fn -> do
        tid  <- B.newInStreamID typeName
        B.put tid >> B.putWithBlockStream1M (fn o)
      Nothing -> fail $ unwords ["no binary format method defied for Haskell type", uchars (toUStr typeName)]
  get = do
    B.updateTypes
    mtab <- B.getCoderTable
    tid  <- B.get >>= B.decodeIndexLookup
    maybe mzero id $ do
      tid <- tid
      fn  <- B.getDecoderForType tid mtab
      tab <- lookupMethodTable tid mtab
      return (Hata tab <$> B.getWithBlockStream1M fn)

instance HasNullValue Hata where
  nullValue = toHata ()
  testNull (Hata ifc o) = case objNullTest ifc of
    Nothing -> error ("to check whether objects of type "++show (objHaskellType ifc)++" are null is undefined behavior")
    Just fn -> fn o

-- | This is a convenience function for calling 'OHaskell' using just an initial value of type
-- @typ@. The 'Interface' is retrieved automatically using the instance of 'haskellDataInterface' for
-- the @typ@.
toHata :: (HataClass typ, Typeable typ) => typ -> Hata
toHata t = flip Hata (toDyn t) (interfaceTo t haskellDataInterface) where
  interfaceTo :: Typeable typ => typ -> Interface typ -> Interface Dynamic
  interfaceTo _ ifc = interfaceToDynamic ifc

-- | Inverse operation of 'toHata', useful when instantiating 'ObjectClass', uses
-- 'Data.Dynamic.fromDynamic' to extract the value that has been wrapped in up the 'Hata'
-- constructor.
fromHata :: (HataClass typ, Typeable typ) => Hata -> Maybe typ
fromHata (Hata _ o) = fromDynamic o

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
instance Sizeable (H.HashMap Object Object) where { getSizeOf = return . obj . H.size }
instance Sizeable Hata where { getSizeOf (Hata ifc o) = maybe mzero ($ o) (objSizer ifc) }

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
-- instantiate your object into the 'HataClass' class. Instantiating the
-- 'HataClass' class alone will make your object usable in Dao language scripts, but
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

structLookup :: Name -> Struct -> Maybe Object
structLookup name struct = case struct of
  Nullary{}            -> Nothing
  Struct{ fieldMap=m } -> M.lookup name m

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

instance HataClass Struct where
  haskellDataInterface = interface "Struct" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

-- | You can make your data type readable but not writable in the Dao runtime. That means a Dao
-- script can inspect elements of your data type, but not modify them As an example lets say you
-- have a 3D-point data type you would like to use in your Dao script.
-- > data Point =
-- >     Point2D{ get_x::'T_float', get_y::'T_float' }
-- >   | Point3D{ get_x::'T_float', get_y::'T_float', get_z::'T_float' }
-- 
-- Lets say you have already instantiated the 'HataClass' class and provided the Dao runtime with
-- a 'DaoFunc' (via 'setupDao') that constructs a Point3D at runtime:
-- > p = Point3D(1.9, 4.4, -2.1);
-- Now you would like to extend the 'HataClass' of your Point3D to also be readable at runtime.
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
-- >             Point3D _ _ z -> 'renameConstructor' "@Point3D@" $ do
-- >                 'define' "z" ('obj' z)
-- >             _             -> return ()
-- 
-- Finally, you should define the instantiation of Point3D into the 'HataClass' class so it
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
-- Do not forget to define the instantiation of Point3D into the 'HataClass' class so it
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
    { structErrName   :: Maybe UStr
    , structErrField  :: Maybe UStr
    , structErrValue  :: Maybe Object
    , structErrExtras :: [Name]
    }
  deriving (Eq, Ord, Typeable)

instance PPrintable StructError where
  pPrint err = do
    let pp p msg f = case f err of
          Nothing -> return ()
          Just  o -> pString (msg++": ") >> p o >> pNewLine
    pp pUStr "on constructor" structErrName
    pp pUStr "on field" structErrField
    pp pPrint "with value" structErrValue
    let extras = structErrExtras err
    if null extras then return () else pString ("non-member fields: "++show extras)

instance HasNullValue StructError where
  nullValue =
    StructError
    { structErrName=Nothing
    , structErrField=Nothing
    , structErrValue=Nothing
    , structErrExtras=[]
    }
  testNull
    ( StructError
      { structErrName=Nothing
      , structErrField=Nothing
      , structErrValue=Nothing
      , structErrExtras=[]
      }
    ) = True
  testNull _ = False

pPrintStructForm :: ToDaoStructClass o => o -> PPrint
pPrintStructForm o = case fromData toDaoStruct o of
  PFail err -> pPrint err
  Backtrack -> pString "(### FAILED TO CONVERT OBJECT TO STRUCT ###)"
  OK struct -> pPrint struct

----------------------------------------------------------------------------------------------------

-- Used to instantiate 'MonadError.throwError' by both the 'ToDaoStruct' and 'FromDaoStruct' monads.
_structThrowError
  :: (MonadError ExecControl m)
  => (forall a . PredicateT ExecControl (State st) a -> m a) -> (st -> Struct) -> ExecControl -> m b
_structThrowError constr inside err =
  constr (lift $ gets inside) >>= \struct -> constr $ throwError $ case err of
    ExecError{execErrorSubtype = ExecStructError info} ->
      err { execErrorSubtype = ExecStructError $
              info{ structErrName = structErrName info <|> Just (toUStr $ structName struct) } }
    err -> err

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass StructError where
  toDaoStruct = renameConstructor "StructError" $ do
    asks structErrName   >>= optionalField "structName" . fmap OString
    asks structErrField  >>= optionalField "field" . fmap OString
    asks structErrValue  >>= optionalField "value"
    asks structErrExtras >>= optionalField "extras" . fmap obj . refNames

instance ToDaoStructClass (ParseError () DaoTT) where
  toDaoStruct = renameConstructor "ParseError" $ do
    asks parseErrMsg >>= ("message" .=?)
    asks parseErrMsg >>= ("onToken" .=?) . fmap show
    asks parseErrLoc >>= putLocation

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
      <*> optional (tryField "structName" $ str)
      <*> optional (tryField "field" $ str)
      <*> optional (tryField "value" return)
      <*> (tryField "extras" $ lst)

instance ToDaoStructClass Comment where
  toDaoStruct = ask >>= \co -> case co of
    InlineComment  o -> renameConstructor "InlineComment"  $ "comment" .= o
    EndlineComment o -> renameConstructor "EndlineComment" $ "comment" .= o

instance FromDaoStructClass Comment where
  fromDaoStruct = msum $
    [ constructor "InlineComment"  >> InlineComment  <$> req "comment"
    , constructor "EndlineComment" >> EndlineComment <$> req "comment"
    ]

instance ToDaoStructClass AST_DotName where
  toDaoStruct = renameConstructor "DotName" $ ask >>= \ (AST_DotName coms n) -> case coms of
    Com () -> "name"     .= n
    coms   -> "comments" .= coms >> "name" .= n

instance FromDaoStructClass AST_DotName where
  fromDaoStruct = constructor "DotName" >>
    return AST_DotName <*> (maybe (Com ()) id <$> opt "comments") <*> req "name"

instance ToDaoStructClass AST_DotLabel where
  toDaoStruct = renameConstructor "DotLabel" $ do
    (AST_DotLabel n nx loc) <- ask
    "head" .= n
    "tail" .= OList (map obj nx)
    putLocation loc

instance FromDaoStructClass AST_DotLabel where
  fromDaoStruct = do
    constructor "DotLabel"
    let convert o = case sequence (map fromObj o) of
          Nothing -> fail "\"tail\" field must contain a list of \"#DotName\" data types."
          Just ox -> return ox
    return AST_DotLabel <*> req "head" <*> (req "tail" >>= convert) <*> location

instance ObjectClass StructError where { obj=new; fromObj=objFromHata; }

instance HataClass StructError where
  haskellDataInterface = interface "StructError" $ do
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
    { _runToDaoStruct :: PredicateT ExecControl (State (Struct, haskData)) a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad (ToDaoStruct haskData) where
  return = ToDaoStruct . return
  m >>= f = ToDaoStruct $ _runToDaoStruct m >>= _runToDaoStruct . f
  fail msg = execThrow msg (ExecStructError nullValue) []

instance MonadState Struct (ToDaoStruct haskData) where
  state f = ToDaoStruct $ lift $ state $ \ (struct, haskData) ->
    let (a, struct') = f struct in (a, (struct', haskData))

instance MonadReader haskData (ToDaoStruct haskData) where
  ask = ToDaoStruct $ lift $ fmap snd get
  local upd f = ToDaoStruct $ PredicateT $ do
    haskData <- gets snd
    modify (\ (struct, _) -> (struct, upd haskData))
    a <- runPredicateT $ _runToDaoStruct f
    modify (\ (struct, _) -> (struct, haskData))
    return a

instance MonadError ExecControl (ToDaoStruct haskData) where
  throwError = _structThrowError ToDaoStruct fst
  catchError f catch = ToDaoStruct $ catchError (_runToDaoStruct f) (_runToDaoStruct . catch)

instance MonadPlusError ExecControl (ToDaoStruct haskData) where
  catchPredicate = ToDaoStruct . catchPredicate . _runToDaoStruct
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
  -> Predicate ExecControl Struct
fromData pred hask = evalState (runPredicateT $ _runToDaoStruct $ pred >> get) $
  (Struct{ structName=nil, fieldMap=M.empty }, hask)

toDaoStructExec :: ToDaoStruct typ x -> typ -> Exec Struct
toDaoStructExec toDaoStruct = (predicate :: Predicate ExecControl T_struct -> Exec T_struct) .
  fmapPFail ((\o -> newError{ execReturnValue=Just o}) . new) . fromData toDaoStruct

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

fmapHaskDataToStruct :: (haskData -> dyn) -> (dyn -> haskData) -> ToDaoStruct haskData a -> ToDaoStruct dyn a
fmapHaskDataToStruct to from (ToDaoStruct (PredicateT f)) =
  ToDaoStruct $ PredicateT $ state $ fmap (fmap to) . runState f . fmap from

-- | Use this function to set the 'structName' name of the constructor at some point, for example
-- when you observe some condition of the @haskData@ type that merits an alternative constructor
-- name.
renameConstructor :: UStrType name => name -> ToDaoStruct haskData ig -> ToDaoStruct haskData ()
renameConstructor name f = do
  name <- mkStructName name
  modify $ \struct -> struct{ structName=name }
  void f

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
-- value instantiating 'HataClass', converts it to an 'Object' using the 'new'
-- function. It is the inverse of 'objType'.
--
-- It is recommended you use this function instead of 'defStructField', 'defPrimField', or
-- 'defDynField' whenever it is possible, i.e. whenever the data type you are putting instantiated
-- the 'HataClass' class.
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

----------------------------------------------------------------------------------------------------

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
  FromDaoStruct{ _runFromDaoStruct :: PredicateT ExecControl (State Struct) a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad FromDaoStruct where
  return = FromDaoStruct . return
  m >>= f = FromDaoStruct $ _runFromDaoStruct m >>= _runFromDaoStruct . f
  fail msg = FromDaoStruct (lift $ gets structName) >>= \name ->
    execThrow msg (ExecStructError $ nullValue{ structErrName = Just $ toUStr name }) []

instance MonadReader Struct FromDaoStruct where
  ask = FromDaoStruct $ lift get
  local upd f = FromDaoStruct $ PredicateT $ get >>= \st ->
   return $ evalState (runPredicateT $ _runFromDaoStruct f) (upd st)

instance MonadError ExecControl FromDaoStruct where
  throwError = _structThrowError FromDaoStruct id
  catchError (FromDaoStruct f) catch = FromDaoStruct $ catchError f (_runFromDaoStruct . catch)

instance MonadPlusError ExecControl FromDaoStruct where
  catchPredicate = FromDaoStruct . catchPredicate . _runFromDaoStruct
  predicate      = FromDaoStruct . predicate

-- | This function is typically used to evaluate the instantiation of 'fromDaoStruct'. It takes two
-- parameters: first a computation to convert your data type to the Haskell data type from a
-- 'Struct' using the 'FromDaoStruct' monad, and second the 'Struct' you want to convert. For
-- example, if you have a Haskell data type 'MyData' which instantiates 'FromDaoStruct', you could
-- construct it from a properly formatted Dao 'Struct' using this statement:
-- > 'toData' 'fromDaoStruct' struct
-- Notice that this reads similar to ordinary English: "convert to (Haskell) data from a dao
-- struct."
toData :: FromDaoStruct haskData -> Struct -> Predicate ExecControl haskData
toData f = evalState (runPredicateT $ _runFromDaoStruct $ f >>= \o -> checkEmpty >> return o)

-- | Using a 'FromDaoStruct' monadic function, convert a given 'Struct' to a Haskell data type
-- @typ@.
withFromDaoStructExec :: FromDaoStruct typ -> Struct -> Exec typ
withFromDaoStructExec fromDaoStruct =
  predicate . fmapPFail ((\o -> newError{ execReturnValue=Just o }) . new) .  toData fromDaoStruct

-- | Given a 'Struct', use the 'structName' to lookup a 'FromDaoStruct' monadic function in the
-- current 'ExecUnit' suitable for constructing a 'Hata' Haskell data type.
fromDaoStructExec :: Struct -> Exec Hata
fromDaoStructExec struct = do
  let name = structName struct
  (MethodTable mtab) <- gets globalMethodTable
  let badType msg = execThrow msg (ExecTypeError $ objTypeFromName name) []
  case M.lookup (structName struct) mtab of
    Nothing  -> badType "no available built-in data type"
    Just ifc -> case objFromStruct ifc of
      Nothing   -> badType "data type cannot be constructed from hashed structure"
      Just from -> Hata ifc <$> withFromDaoStructExec from struct

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
  PFail (err@(ExecError{execErrorSubtype=ExecStructError info})) -> throwError $ 
    err{ execErrorSubtype = ExecStructError $ info{ structErrField = Just (toUStr name) } }
  PFail err -> throwError err
  OK      o -> return o
  Backtrack -> mzero

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
  newError{ execErrorSubtype = ExecStructError $ nullValue{ structErrField = Just $ toUStr name } }

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

-- As you make calls to 'field' and 'tryField', the items in these fields in the 'Struct' are
-- being removed. Once you have all of the nata neccessary to construct the data 'Object', you can
-- check to make sure there are no extraneous unused data fields. If the 'Struct' is empty, this
-- function evaluates to @return ()@. If there are extranous fields in the 'Struct', 'throwError' is
-- evaluated. It is highly recommended that this function always be used as the last function
-- evaluated in the 'FromDaoStruct' monadic function.
checkEmpty :: FromDaoStruct ()
checkEmpty = FromDaoStruct (lift get) >>= \st -> case st of
  Struct{ fieldMap=m } -> when (not $ M.null m) $
    execThrow "assigned to non-member fields of structure"
      (ExecStructError $ nullValue{ structErrExtras = M.keys m }) []
  Nullary{} -> return ()

-- | Takes a conversion as the first parameter. The second parameter will be provided by 'field' or
-- 'tryField' when you pass it as a partial function application. If the conversion function
-- backtracks, 'Control.Monad.Error.throwError' is evaluated with the appropriate error data set.
-- This function should usually not be required, as it is called by functions like 'opt', 'req', and
-- 'reqList'.
convertFieldData :: (Object -> FromDaoStruct o) -> Object -> FromDaoStruct o
convertFieldData f o = mplus (f o) $ throwError $
  newError{ execErrorSubtype = ExecStructError $ nullValue{ structErrValue=Just o } }

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

builtin_toStruct :: DaoFunc ()
builtin_toStruct =
  daoFunc
  { daoForeignFunc = \ () ox -> do
      let qref = reference UNQUAL (ustr "toStruct")
      let wrongTypeErr o = throwBadTypeError "cannot convert to a struct from object of the given type" o []
      case ox of
        [o] -> case o of
          OTree              _  -> return (Just o, ())
          OHaskell (Hata ifc d) -> case objToStruct ifc of
            Just to -> flip (,) () . Just . obj <$> toDaoStructExec to d
            Nothing -> wrongTypeErr o
          o                     -> wrongTypeErr o
        ox  -> throwArityError "" 1 ox [(errInFunc, obj qref)]
  }

builtin_fromStruct :: DaoFunc ()
builtin_fromStruct =
  daoFunc
  { daoForeignFunc = \ () ox -> do
      let qref = reference UNQUAL (ustr "fromStruct")
      case ox of
        [o] -> case o of
          OTree o -> flip (,) () . Just . OHaskell <$> fromDaoStructExec o
          o       -> throwBadTypeError "argument parameter is not a struct data type" o []
        ox -> throwArityError "" 1 ox [(errInFunc, obj qref)]
  }

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass Location where
  toDaoStruct = ask >>= \lo -> case lo of
    LocationUnknown -> makeNullary "NoLocation"
    Location{} -> renameConstructor "Location" $ do
      "startingLine"   .=@ startingLine
      "startingColumn" .=@ startingColumn
      "endingLine"     .=@ endingLine
      "endingColumn"   .=@ endingColumn

instance FromDaoStructClass Location where
  fromDaoStruct = msum $
    [ nullary "NoLocation" >> return LocationUnknown
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
location = opt "location" >>= maybe (return LocationUnknown) return

putComments :: [Comment] -> ToDaoStruct haskData ()
putComments = void . defObjField "comments"

comments :: FromDaoStruct [Comment]
comments = req "comments"

optComments :: FromDaoStruct (Maybe [Comment])
optComments = opt "comments"

instance HasRandGen Object where
  randO = countNode $ recurse $ runRandChoice
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

-- | This is a newtype of 'Object' with a specially defined instance for 'HasRandGen' that
-- guarantees the 'Object' values generated randomly can be pretty-printed an re-parsed back to the
-- exact same value, unambiguously. For example, the instance of 'HasRandGen' for 'LimitedObject'
-- will not produce any values of:
-- > 'Dao.Interpreter.OList' ['Dao.Interpreter.OInt' 1, 'Dao.Interpreter.OInt' 2, 'Dao.Interpreter.OInt' 3]
-- because this will be pretty-printed to "list {1,2,3}" and parsing that pretty printed object will
-- yield the data type:
-- > ('Dao.Interpreter.AST_Init'
-- >     ('Dao.Interpreter.AST_DotLabel' ('Dao.String.Name' "list") [] 'Dao.Token.LocationUnknown')
-- >     ('Dao.Interpreter.AST_OptObjList' [] 'Prelude.Nothing')
-- >     ('Dao.Interpreter.AST_ObjList' []
-- >         [ 'Dao.Interpreter.Com' ('Dao.Interpreter.AST_Eval' ('Dao.Interpreter.AST_ObjArith' ('Dao.Interpreter.AST_Object' ('Dao.Interpreter.AST_ObjLiteral' (OInt 1 'Dao.Token.LocationUnknown')))))
-- >         , 'Dao.Interpreter.Com' ('Dao.Interpreter.AST_Eval' ('Dao.Interpreter.AST_ObjArith' ('Dao.Interpreter.AST_Object' ('Dao.Interpreter.AST_ObjLiteral' (OInt 2 'Dao.Token.LocationUnknown')))))
-- >         , 'Dao.Interpreter.Com' ('Dao.Interpreter.AST_Eval' ('Dao.Interpreter.AST_ObjArith' ('Dao.Interpreter.AST_Object' ('Dao.Interpreter.AST_ObjLiteral' (OInt 3 'Dao.Token.LocationUnknown')))))
-- >         ]
-- >     )
-- > )
-- Obviously this is a completely different data structure than the data originally randomly
-- generated. If one were to evaluate it using 'Dao.Interpreter.execute', it would evaluate to the
-- originally generated random object value. But for simplicity the test suit does not evaluate
-- anything, it only compares the original randomly generated test object value to the object value
-- that was constructed by parsing the pretty printed form.
--
-- Therefore, the only data structures that should be randomly generated for testing are the data
-- structures that pretty print to a form that can be parsed back to an identical value when
-- compared to the original. This limits the objects that can be generated to simple string and
-- integer literals, hence the name 'LimitedObject'.
newtype LimitedObject = LimitedObject { unlimitObject :: Object } deriving (Eq, Ord, Show)

instance HasNullValue LimitedObject where
  nullValue = LimitedObject nullValue
  testNull (LimitedObject o) = testNull o

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
  | OHaskell   Hata
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

instance ToDaoStructClass RefQualifier where { toDaoStruct=putNullaryUsingShow; }

instance FromDaoStructClass RefQualifier where { fromDaoStruct=getNullaryWithRead; }

instance ObjectClass RefQualifier where { obj=new; fromObj=objFromHata; }

instance HataClass RefQualifier where
  haskellDataInterface = interface "RefQualifier" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data Reference
  = Reference  RefQualifier Name RefSuffix
  | RefObject  Object RefSuffix
  | RefWrapper Reference
  deriving (Eq, Ord, Typeable, Show)

instance Monoid (XPure Reference) where
  mempty = mzero
  mappend a b = msum $
    [ a >>= \a -> b >>= \b -> case b of
        Reference UNQUAL name suf -> let suf2 = DotRef name suf in case a of
          Reference  q name suf1 -> return $ Reference q name (suf1 <> suf2)
          RefObject  o      suf1 -> return $ RefObject   o    (suf1 <> suf2)
          RefWrapper a           -> return a <> return b
        _  -> execThrow
                "only unqualified references can be appended to other references"
                  ExecErrorUntyped [(assertFailed, obj b)]
    , a, b
    ]

instance Read Reference where
  readsPrec _ str = loop [] (sp str) where
    sp = dropWhile isSpace
    loop rx str = do
      (a,  str) <- pure (span (\c -> isAlpha    c || c=='_') str)
      guard (not $ null a)
      (ax, str) <- pure (span (\c -> isAlphaNum c || c=='_') str)
      ax  <- pure (fromUStr $ toUStr $ a++ax)
      str <- pure (sp str)
      case str of
        '.':str            -> loop (rx++[ax]) (sp str)
        "" | not $ null rx -> [(Reference UNQUAL (head rx) $ refSuffixFromNames (tail $ rx++[ax]), "")]
        ""                 -> [(Reference UNQUAL ax NullRef, "")]
        _                  -> error $ concat ["a=", show a, "ax=", show ax, "str=", show str]

-- | Construct a 'Reference' with a 'RefQualifier' and a 'Name'.
reference :: RefQualifier -> Name -> Reference
reference q name = Reference q name NullRef

-- | Construct a 'Reference' with an object.
refObject :: Object -> Reference
refObject = flip RefObject NullRef

-- | Strip the 'RefSuffix' from the given 'Reference', changing it 'NullRef' and returning the
-- updated 'Referene' along with the 'RefSuffix' that was removed. If the 'Reference' is a
-- 'RefWrapper', nothing is changed.
referenceHead :: Reference -> (Reference, Maybe RefSuffix)
referenceHead qref = case qref of
  Reference q name suf -> (Reference q name NullRef, Just suf)
  RefObject   o    suf -> (RefObject   o    NullRef, Just suf)
  RefWrapper  r        -> (RefWrapper  r           , Nothing )

-- | The 'Reference' data type has a 'RefWrapper' constructor which wraps a 'Reference' value,
-- protecting it from being de-referenced. This function unwraps the inner 'Reference' if it is
-- within a 'RefWrapper', or else returns the 'Reference' unchanged.
refUnwrap :: Reference -> Reference
refUnwrap r = case r of { RefWrapper r -> r; r -> r; }

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

-- 'execute'-ing a 'Reference' will dereference it, essentially reading the value associated with
-- that reference from the 'ExecUnit'.
instance Executable Reference (Reference, Maybe Object) where { execute qref = referenceLookup qref }

refAppendSuffix :: Reference -> RefSuffix -> Reference
refAppendSuffix qref appref = case qref of
  Reference q name ref -> Reference q name (ref<>appref)
  RefObject   o    ref -> RefObject   o    (ref<>appref)
  RefWrapper      qref -> RefWrapper $ refAppendSuffix qref appref

-- | This is an important function used throughout most of the intepreter to lookup 'Object's
-- associated with 'Reference's. It returns a a pair containing updated copy of the given
-- 'Reference' and the 'Object' that was looked-up. The 'Reference' returned is a copy of the
-- 'Reference' parameter given but updated with information about where the reference was looked up.
-- For example, if you pass an 'UNQUAL' (unqualified) reference, it may be looked up in the local,
-- global, or const variable tables.  The reference returned will not be 'UNQUAL', it will be either
-- 'GLOBAL', 'LOCAL', or 'CONST', depending on where the 'Object' returned was found.
--
-- If the 'Reference' is a function call, the object returned will be the evaluation of the function
-- call, which may be void (a.k.a. 'Prelude.Nothing').
referenceLookup :: Reference -> Exec (Reference, Maybe Object)
referenceLookup qref = case qref of
  RefWrapper ref -> return $ (qref, Just (obj ref))
  qref           -> do
    (a, (qref, _, _)) <- runObjectFocus (updateIndex qref get) True qref ()
    return (qref, a)

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
-- it. If the boolean parameter is 'Prelude.True' it indicates that the value updated must already
-- exist, and an undefined reference error will be thrown if it does not exist.
referenceUpdate :: Reference -> Bool -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Reference, Maybe Object)
referenceUpdate qref mustExist upd = do
  -- The 'ExecUnit' is not actually modified in any way by 'updateIndex'. It is only used to
  -- instruct Haskell's type system to select the class instance of 'updateIndex' for the data type:
  -- > 'ObjectLens' 'ExecUnit' 'Reference'
  (result, (qref, _, _)) <-
    runObjectFocus (updateIndex qref $ execToFocusUpdater upd) mustExist (fst $ referenceHead qref) ()
  return (qref, result)

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

-- | Get the 'CoreType' o an 'Object'.
coreType :: Object -> CoreType
coreType o = case o of
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
  | TypeSym  Name
  | TypeVar  Name [ObjType]
    -- ^ a polymorphic type, like 'AnyType' but has a name.
  deriving (Eq, Ord, Show, Typeable)

instance NFData TypeSym where
  rnf (CoreType a  ) = deepseq a ()
  rnf (TypeSym  a  ) = deepseq a ()
  rnf (TypeVar  a b) = deepseq a $! deepseq b ()

instance HasRandGen TypeSym where
  randO = _randTrace "TypeSym" $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [CoreType <$> randO, TypeSym <$> randO, scramble $ return TypeVar <*> randO <*> randList 1 4]
  defaultO = _randTrace "D.TypeSym" $ CoreType <$> defaultO

instance PPrintable TypeSym where
  pPrint t = case t of
    CoreType t     -> pPrint t
    TypeSym  t     -> pPrint t
    TypeVar  t ctx -> pInline $
      concat [[pPrint t], guard (not (null ctx)) >> [pList_ "[" ", " "]" (map pPrint ctx)]]

-- binary 0x2E 0x2F
instance B.Binary TypeSym mtab where
  put o = case o of
    CoreType o      -> B.prefixByte 0x2D $ B.put o
    TypeSym  o      -> B.prefixByte 0x2E $ B.put o
    TypeVar ref ctx -> B.prefixByte 0x2F $ B.put ref >> B.put ctx
  get = B.word8PrefixTable <|> fail "expecting TypeSym"

instance B.HasPrefixTable TypeSym B.Byte mtab where
  prefixTable =
    B.mkPrefixTableWord8 "TypeSym" 0x2D 0x2F [CoreType <$> B.get, return TypeVar <*> B.get <*> B.get]

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

typeOfObj :: Object -> ObjType
typeOfObj o = case o of
  OHaskell o -> hataType o
  o          -> ObjType [TypeStruct [CoreType $ coreType o]]

hataType :: Hata -> ObjType
hataType (Hata ifc _) = ObjType [TypeStruct [TypeSym $ objInterfaceName ifc]]

objTypeFromCoreType :: CoreType -> ObjType
objTypeFromCoreType = ObjType . return . TypeStruct . return . CoreType

objTypeFromName :: Name -> ObjType
objTypeFromName name = ObjType{ typeChoices = [TypeStruct [TypeSym name]] }

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

instance Monoid RefSuffix where
  mempty = NullRef
  mappend left right = case left of
    NullRef           -> right
    DotRef    nm left -> DotRef    nm $ left<>right
    Subscript ox left -> Subscript ox $ left<>right
    FuncCall  ox left -> FuncCall  ox $ left<>right

-- | If the 'RefSuffix' is 'DotRef', 'Subscript', or 'FuncCall', the second parameter to these
-- constructors is overwritten with 'NullRef' so only the first parameter remains.
refSuffixHead :: RefSuffix -> RefSuffix
refSuffixHead suf = let lst = refSuffixToList suf in if null lst then NullRef else head lst

-- | Evaluates to 'Prelude.True' if ay of the constructors within the 'RefSuffix' are 'FuncCall'.
refSuffixHasFuncCall :: RefSuffix -> Bool
refSuffixHasFuncCall suf = case suf of
  NullRef         -> False
  DotRef    _ suf -> refSuffixHasFuncCall suf
  Subscript _ suf -> refSuffixHasFuncCall suf
  FuncCall  _ _   -> True

-- | The 'RefSuffix' is a list-like data type, where most of the constructors may contain another
-- 'RefSuffix' structure as the "tail" of the list. This function "explodes" a 'RefSuffix' into a
-- list of 'RefSuffix's where "tail" is 'NullRef'. This is the inverse operation of
-- 'Data.Monoid.mconcat', so the following equality is always True:
-- > \r -> mconcat (refSuffixToList r) == r
refSuffixToList :: RefSuffix -> [RefSuffix]
refSuffixToList suf = case suf of
  NullRef         -> []
  DotRef    a suf -> DotRef    a NullRef : refSuffixToList suf
  Subscript a suf -> Subscript a NullRef : refSuffixToList suf
  FuncCall  a suf -> FuncCall  a NullRef : refSuffixToList suf

-- | Construct a 'DotRef' with a 'NullRef' suffix.
dotRef :: Name -> RefSuffix
dotRef = flip DotRef NullRef

-- | Construct a 'Subscript' with a 'NullRef' suffix.
subscript :: [Object] -> RefSuffix
subscript = flip Subscript NullRef

-- | Construct a 'FuncCall' with a 'NullRef' suffix.
funcCall :: [Object] -> RefSuffix
funcCall = flip FuncCall NullRef

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

instance PPrintable FuzzyStr where { pPrint (FuzzyStr str) = pShow str }

instance ObjectClass FuzzyStr where { obj=new; fromObj=objFromHata; }

instance HataClass FuzzyStr where
  haskellDataInterface = interface "FuzzyStr" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter 

--instance Show (Glob FuzzyStr) where { show = show . fmap toUStr }

--instance Read (Glob FuzzyStr) where
--  readsPrec prec str = readsPrec prec str >>= \ (glob, str) -> [(fmap fromUStr glob, str)]

instance Show (GlobUnit Object) where
  show o = case o of
    Single o -> show o
    globunit -> show (fmap (const "") globunit)

instance PPrintable (GlobUnit Object) where { pPrint = pShow }

instance Show (Glob Object) where
  show glob = (++"\"") $ ('"':) $ do
    o <- getPatUnits glob
    let other o = "$("++prettyShow o++")"
    case o of
      Single o -> case o of
        OString  o -> uchars o
        OHaskell (Hata _ifc dyn) -> case fromDynamic dyn of
          Nothing           -> other o
          Just (FuzzyStr o) -> uchars o
        _ -> other o
      globunit -> show (fmap (const "") globunit)

instance PPrintable (Glob Object) where { pPrint = pShow }

instance ToDaoStructClass (GlobUnit Object) where
  toDaoStruct = ask >>= \o -> case o of
    Wildcard a t -> renameConstructor "Wildcard" $ "name" .= reference UNQUAL a >> "type" .=? t
    AnyOne   a t -> renameConstructor "AnyOne"   $ "name" .= reference UNQUAL a >> "type" .=? t
    Single   a   -> renameConstructor "Single"   $ "item" .= a

instance FromDaoStructClass (GlobUnit Object) where
  fromDaoStruct = msum $
    [ constructor "Wildcard" >> return Wildcard <*> req "name" <*> opt "type"
    , constructor "AnyOne"   >> return AnyOne   <*> req "name" <*> opt "type"
    , constructor "Single"   >>        Single   <$> req "item"
    ]

instance ObjectClass (GlobUnit Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (GlobUnit Object) where
  haskellDataInterface = interface "GlobUnit" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefFromStruct >> autoDefToStruct

instance ToDaoStructClass (Glob Object) where
  toDaoStruct = renameConstructor "GlobPattern" $ "items" .=@ obj . map obj . getPatUnits

instance FromDaoStructClass (Glob Object) where
  fromDaoStruct = do
    constructor "GlobPattern"
    items <- reqList "items"
    return (Glob{ getPatUnits=items, getGlobLength=length items })

instance ObjectClass (Glob Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (Glob Object) where
  haskellDataInterface = interface "GlobPattern" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct
    defMethod "match" $
      daoFunc
      { daoForeignFunc = \glob ox -> fmap (flip (,) glob . Just . obj) $
          forM (matchPattern False glob ox) $ \match -> fmap (obj . M.fromList . concat) $
            forM (M.assocs match) $ \ (name, (vartyp, ox)) -> case vartyp of
              Nothing     -> return [(name, obj ox)]
              Just vartyp -> do
                match <- catchPredicate $ referenceLookup $ Reference UNQUAL vartyp $ FuncCall ox NullRef
                case match of
                  Backtrack       -> return []
                  OK (_, Nothing) -> return [(name, obj ox)]
                  OK (_, Just  o) -> return [(name, obj o)]
                  PFail err       -> throwError err
      }

----------------------------------------------------------------------------------------------------

newtype Pair = Pair (Object, Object) deriving (Eq, Ord, Show, Typeable)

instance PPrintable Pair where
  pPrint (Pair (a,b)) = pList (pString "Pair") "(" ", " ")" [pPrint a, pPrint b]

instance ToDaoStructClass Pair where
  toDaoStruct = renameConstructor "Pair" $ ask >>= \ (Pair (a, b)) -> "fst" .= a >> "snd" .= b

instance FromDaoStructClass Pair where
  fromDaoStruct = constructor "Pair" >> Pair <$> (return (,) <*> req "fst" <*> req "snd")

instance ObjectClass Pair where { obj=new; fromObj=objFromHata; }

instance HataClass Pair where
  haskellDataInterface = interface "Pair" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct
    defIndexer $ \ (Pair (a,b)) ix -> do
      let casti = extractXPure . castToCoreType IntType >=> fromObj
      let badindex = execThrow "index for Pair data type must be a either 0 or 1" ExecErrorUntyped
      let qref = reference UNQUAL (ustr "Pair")
      case ix of
        [i] -> do
          i <- derefObject i >>= maybe (badindex [(actualType, obj (typeOfObj i))]) return . casti
          case i of { 0 -> return a; 1 -> return b; i -> badindex [(assertFailed, OInt i)]; }
        ix  -> throwArityError "for subscript to Pair data type" 1 ix [(errInConstr, obj qref)]

builtin_assocs :: DaoFunc ()
builtin_assocs =
  daoFunc
  { daoForeignFunc = \ () ox -> do
      let qref = reference UNQUAL (ustr "assocs")
      case ox of
        [o] -> do
          let fromDict o = return $
                (Just $ obj $ fmap (\ (a,b) -> obj $ Pair (obj a, obj b)) $ M.assocs o, ())
          let badtype = throwBadTypeError "object does not contain association Pairs" o []
          case o of
            ODict                  d   -> fromDict d
            OTree (Struct{fieldMap=d}) -> fromDict d
            OHaskell               _   -> maybe badtype (return . flip (,) () . Just) $ msum $
              [ fromObj o >>= return . obj . fmap (\ (ix, o) -> obj $ Pair (H.indexKey ix, o)) . H.assocs
              ]
            _ -> badtype
        ox -> throwArityError "" 1 ox [(errInFunc, obj qref)]
  }

builtin_Pair :: DaoFunc ()
builtin_Pair =
  daoFunc
  { daoForeignFunc = \ () ox -> case ox of
      [a, b] -> return $ (Just $ obj $ Pair(a, b), ())
      ox -> throwArityError "Pair() constructor requires exactly two arguments" 2 ox []
  }

----------------------------------------------------------------------------------------------------

-- | This is a newtype wrapper around a function that tokenizes a 'UStr' into smaller 'UStr's used
-- for constructing rule patterns, and also for tokenizing input strings into objects that can be
-- matched against rule patterns.
newtype ExecTokenizer = ExecTokenizer { runExecTokenizer :: UStr -> Exec [Object] }
  deriving Typeable

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
    , importGraph        :: M.Map UPath ExecUnit
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , defaultTimeout     :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , currentWithRef     :: Maybe Object
      -- ^ the current document is set by the @with@ statement during execution of a Dao script.
    , taskForExecUnits   :: Task
    , currentQuery       :: Maybe [Object]
    , currentPattern     :: Maybe (Glob Object)
    , currentCodeBlock   :: Maybe Subroutine
      -- ^ when evaluating a 'Subroutine' selected by a string query, the action resulting from
      -- that query is defnied here. It is only 'Data.Maybe.Nothing' when the module is first being
      -- loaded from source code.
    , currentBranch      :: [Name]
      -- ^ set by the @with@ statement during execution of a Dao script. It is used to prefix this
      -- to all global-dot references before reading from or writing to those references.
    , execStack          :: Stack Name Object
      -- ^ stack of local variables used during evaluation
    , globalData         :: T_dict
    , providedAttributes :: M.Map UStr ()
    , builtinConstants   :: T_dict
    , execOpenFiles      :: M.Map UPath ExecUnit
    , programModuleName  :: Maybe UPath
    , preExec            :: [Subroutine]
      -- ^ the "guard scripts" that are executed before every string execution.
    , postExec           :: [Subroutine]
      -- ^ the "guard scripts" that are executed after every string execution.
    , quittingTime       :: [Subroutine]
    , programTokenizer   :: ExecTokenizer
    , ruleSet            :: PatternTree Object [Subroutine]
    , lambdaSet          :: [CallableCode]
    , uncaughtErrors     :: [ExecControl]
    , runtimeRefTable    :: RefTable Object Dynamic
    }

-- Initializes a completely empty 'ExecUnit'
_initExecUnit :: IO ExecUnit
_initExecUnit = do
  execTask <- initTask
  reftable <- newRefTable
  return $
    ExecUnit
    { globalMethodTable  = mempty
    , defaultTimeout     = Nothing
    , importGraph        = mempty
    , currentWithRef     = Nothing
    , currentQuery       = Nothing
    , currentPattern     = Nothing
    , currentCodeBlock   = Nothing
    , currentBranch      = []
    , globalData         = mempty
    , providedAttributes = mempty
    , builtinConstants   = mempty
    , taskForExecUnits   = execTask
    , execStack          = emptyStack
    , execOpenFiles      = mempty
    , programModuleName  = Nothing
    , preExec            = []
    , quittingTime       = mempty
    , programTokenizer   = defaultTokenizer
    , postExec           = []
    , ruleSet            = T.Void
    , lambdaSet          = []
    , uncaughtErrors     = []
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
newExecUnit modName = get >>= \parent -> liftIO _initExecUnit >>= \child -> return $
  child
  { programModuleName = modName
  , builtinConstants  = builtinConstants  parent
  , defaultTimeout    = defaultTimeout    parent
  , globalMethodTable = globalMethodTable parent
  , runtimeRefTable   = runtimeRefTable   parent
  }

-- | Execute an 'Exec' monadic function within a different 'ExecUnit' module. The result of the
-- 'Exec' monadic function is the first value in the tuple returned, any modifications to the given
-- 'ExecUnit' module are stored as the second value of the tuple returned.
inModule :: ExecUnit -> Exec a -> Exec (a, ExecUnit)
inModule subxunit exe = do
  xunit    <- get
  result   <- put subxunit >> catchPredicate exe
  subxunit <- get
  put    xunit
  result   <- predicate result
  return (result, subxunit)

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
  execModifyRef mvar upd =
    Exec $ PredicateT $ StateT $ \xunit -> modifyMVar mvar $ \var -> do
      (result, xunit) <- flip ioExec xunit $ execCatchIO (upd var) $
        [ newExecIOHandler $ flip (execThrow "") [] . ExecHaskellError
        , newExecIOHandler $ flip (execThrow "") [] . ExecIOException
        ]
      let x var p = return (var, (p, xunit))
      case result of
        Backtrack   -> x var $ Backtrack
        OK (var, o) -> x var $ OK      o
        PFail   err -> x var $ PFail err
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

data ObjFocusState o
  = ObjFocusState
    { targetReference      :: Reference
      -- ^ the whole reference we intend to use
    , focalReference       :: Reference
      -- ^ the reference that is constructed piecewise as each part of the 'targetReference' is resolved.
    , objectInFocus        :: o
    , objectInFocusChanged :: Bool
      -- ^ this value is automatically set whenever the 'Control.Monad.State.put' function, or
      -- 'Control.Monad.State.modify' functions are evaluated. This value is returned by functions
      -- like 'withInnerFocus' and 'runObjectFocus' to indicate when the item being modified was
      -- actually modified.
    , focusLookup          :: Bool
      -- ^ when true, indicates that the current focus operation is a lookup. Lookups are different
      -- from updates in that an update can continue recursive searching through an object tree even
      -- if the reference to be updated is void. Assignment operations, for example, update void
      -- references by writing to the reference. Lookup operations, on the other hand, will always
      -- fail as soon as a void is encountered.
    }

-- Although the 'ObjectFocus' monad is a wrapper around a 'Dao.Predicate.PredicateT' monad
-- transformer that lifts the 'Exec' monad, backtracking (the instantiation  of
-- 'Control.Monad.mzero') has a very different semantical meaning from the 'Exec' monad's semantical
-- meaning of backtracking. In the case of 'ObjectFocus', evaluating 'Control.Monad.mzero' indicates
-- a refernece being looked-up is undefined. To allow for 'Exec' to be lifted into 'ObjectFocus' and
-- also possibly evaluate to 'Control.Monad.mzero' without triggering a backtracking in the
-- 'ObjectFocus' monad, backtracking in the 'Exec' monad needs to be "caught" and re-thrown as an
-- error using 'Control.Monad.Error.throwError' wrapped up in the 'ObjFocusError' data type. However
-- this is all done "under the hood," the public API of the 'ObjectFocus' monad still has
-- 'Control.Monad.Error.throwError' instantiated to throw 'ExecControl' just like the 'Exec' monad:
-- > 'Control.Monad.Error.Class.MonadError' 'ExecControl' 'ObjectFocus'
-- and when evaluting the 'ObjectFocus' monad in the 'Exec' monad using 'runObjectFocus', this
-- error type is automatically caught again and converted back to 'Control.Monad.mzero', initiating
-- ordinary backtracking in the 'Exec' monad.
data ObjFocusError
  = InnerExecBacktrack -- ^ a lifted 'Exec' monad evaluated to 'Control.Monad.mzero'
  | InnerExecPFail ExecControl
    -- ^ a lifted 'Exec' monad evaluated to 'Control.Monad.Error.throwError'

-- | This is the stateful monad used by the 'ObjectLens' and 'ObjectFunctor' classes. It is called a
-- "focus" because the object we are focused on (on which we are looking up indicies or modifying
-- indicies) is stored in the state of the monad. The 'Control.Monad.State.modify' function modifies
-- the object in the focus.
newtype ObjectFocus o a
  = ObjectFocus{ mapObjectLensToPredicate :: PredicateT ObjFocusError (StateT (ObjFocusState o) Exec) a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad (ObjectFocus o) where
  return = ObjectFocus . return
  (ObjectFocus m) >>= f = ObjectFocus $ m >>= mapObjectLensToPredicate . f
  fail msg = _mapStructState (gets focalReference) >>= flip (execThrow msg) []

instance MonadError ExecControl (ObjectFocus o) where
  throwError = ObjectFocus . throwError . InnerExecPFail
  catchError (ObjectFocus f) catch = ObjectFocus $ catchError f $ \err -> case err of
    InnerExecPFail err -> mapObjectLensToPredicate (catch err)
    InnerExecBacktrack -> throwError InnerExecBacktrack

instance MonadPlusError ExecControl (ObjectFocus o) where
  catchPredicate (ObjectFocus f) = ObjectFocus $ catchPredicate f >>= \p -> case p of
    OK                    o  -> return $ OK o
    PFail (InnerExecPFail e) -> return $ PFail e
    PFail InnerExecBacktrack -> throwError InnerExecBacktrack
    Backtrack                -> return Backtrack
  predicate = ObjectFocus . predicate . fmapPFail InnerExecPFail

instance MonadIO (ObjectFocus o) where { liftIO = ObjectFocus . liftIO }

instance MonadState o (ObjectFocus o) where
  get   = _mapStructState $ gets objectInFocus
  put o = _mapStructState $ modify $ \st -> st{objectInFocus=o, objectInFocusChanged=True}

-- | An 'ObjectFocus' function that updates a value within the data of type @o@ in focus at a given
-- @index@ using an inner 'ObjectFocus' function that focuses on the 'Object' value at the index if
-- it exists. The type of the inner 'ObjectFocus' is @('Prelude.Maybe' 'Object')@ because there may
-- be no value defined at the given index.
type ObjectUpdate o index = index -> ObjectFocus (Maybe Object) (Maybe Object) -> ObjectFocus o (Maybe Object)

-- | An 'ObjectFocus' that traverses the 'Object' in the focus by calling the provided traversal
-- function for every @index -> 'Object'@ relation defined within the data of type @o@. The
-- traversal function is an inner focus of type @[(index, 'Object')]@. The list in focus should
-- initially be empty when the inner traversal function is called. When the inner traversal function
-- is completed, it should contain every @(index, 'Object')@ that is intended to be stored back into
-- the data of type @o@. The outer 'ObjectFocus' should retrieve this list of pairs and determine
-- how to update the data of type @o@ accordingly.
type ObjectTraverse o index = (index -> Object -> ObjectFocus [(index, Object)] ()) -> ObjectFocus o ()

-- | This class provides functions that can be used to establish 'ObjectFocus' for various data
-- types. The class allows you to define an association between an index and and object type o, and
-- how the index is used to read and update the object o.
-- There are no functional dependencies between the object type and the index type, so using these
-- function may require type annotation.
class ObjectLens o index where { updateIndex :: ObjectUpdate o index }

-- | This class provides the 'objectFMap' function for evaluating a functor over every item in an
-- 'bject in the focus of an 'ObjectFocus'. The function that maps to the functor object takes a
-- polymorphic index type and the 'Object' associated with that index. For example, in the case of a
-- 'T_dict' type, the index would be a 'Dao.String.Name', in the case of a 'T_list' type, the index
-- would be a 'Prelude.Integer'.
class ObjectFunctor o index where { objectFMap :: ObjectTraverse o index }

_mapStructState :: StateT (ObjFocusState o) Exec a -> ObjectFocus o a
_mapStructState = ObjectFocus . lift

_getTargetRefInfo :: ObjectFocus o Reference
_getTargetRefInfo = _mapStructState $ gets targetReference

_setTargetRefInfo :: Reference -> ObjectFocus o ()
_setTargetRefInfo qref = _mapStructState $ modify $ \st -> st{targetReference=qref}

-- | This function will check if the 'focusLookup' boolean is set. If it is set, it checks if the
-- current 'objectInFocus' is 'Prelude.Nothing' and backtracks if it is. Otherwise it evaluates the
-- given function.
focusNext :: ObjectFocus (Maybe o) a -> ObjectFocus (Maybe o) a
focusNext f = do
  isLookup <- _mapStructState (gets focusLookup)
  if isLookup then get >>= maybe mzero (const f) else f

-- | When instantiating the 'ObjectLens' class with a 'RefSuffix' index type, it is useful to
-- record the head of the 'RefSuffix' that is being used to resolve the index. When an
-- 'Control.Monad.fail' is evaluated, the current path (the 'RefSuffix') to the part of the object
-- where the failure occurred will be used in the error report. Bracketing your 'ObjectFocus'
-- evaluation in this function will help create better error reports.
focalPathSuffix :: RefSuffix -> ObjectFocus o a -> ObjectFocus o a
focalPathSuffix suf f = do
  r <- _mapStructState $ get >>= \st -> do
    let r = focalReference st
    put (st{ focalReference=refAppendSuffix r suf }) >> return r
  f >>= \a -> _mapStructState (modify $ \st -> st{ focalReference=r }) >> return a

focusLiftExec :: Exec a -> ObjectFocus o a
focusLiftExec exec = ObjectFocus $ do
  p <- lift $ lift $ catchPredicate exec
  case p of
    Backtrack -> throwError InnerExecBacktrack
    PFail err -> throwError $ InnerExecPFail err
    OK    o   -> return o

execToFocusUpdater :: (Maybe Object -> Exec (Maybe Object)) -> ObjectFocus (Maybe Object) (Maybe Object)
execToFocusUpdater f = get >>= focusLiftExec . f >>= \o -> put o >> return o

getFocalReference :: ObjectFocus o Reference
getFocalReference = _mapStructState (gets focalReference)

-- | This is a kind of entry-point to the 'ObjectFocus' group of functions. First provide a boolean
-- value indicating whether this operation is a ('Prelude.True') lookup and should fail as soon as a
-- void address is encountered, or ('Prelude.False') an update that may insert a value at a void
-- address rather than failing. Second, provide a 'Reference' value for error reporting, to indicate
-- where a 'lookupIndex' or 'updateIndex' function failed. Note that using 'lookupIndex' and
-- 'updateIndex' functions instantiated for 'RefSuffix' indicies will append these indicies to the
-- 'Reference', so it might be better to pass the 'referenceHead' of the 'Reference'. Then supply an
-- object upon which the 'lookupIndex', 'updateIndex', or 'objectFMap' functions will be evaluating.
runObjectFocus :: ObjectFocus o a -> Bool -> Reference -> o -> Exec (a, (Reference, Bool, o))
runObjectFocus f isLookup qref o = _runObjectFocus f st >>= \p -> case p of
  OK                    o  -> return o
  PFail (InnerExecPFail e) -> throwError e
  PFail InnerExecBacktrack -> mzero
  Backtrack                -> execThrow "undefined reference" qref []
  where
    st= ObjFocusState
        { targetReference      = qref
        , focalReference       = fst (referenceHead qref)
        , objectInFocus        = o
        , objectInFocusChanged = False
        , focusLookup          = isLookup
        }

_runObjectFocus :: ObjectFocus o a -> ObjFocusState o -> Exec (Predicate ObjFocusError (a, (Reference, Bool, o)))
_runObjectFocus (ObjectFocus f) st = flip evalStateT st $ runPredicateT $ f >>= \a -> do
  o <- lift $ return (,,) <*> gets targetReference <*> gets objectInFocusChanged <*> gets objectInFocus
  return (a, o)

-- | This is a very important function because it allows you to evaluate an inner 'ObjectFocus'
-- monad that is focused on a different type from the type focus of the monad in the context in
-- which this function is evaluated. It allows you to select a sub-field of the current focus (for
-- example using 'Control.Monad.State.gets') and evaluate an updating function or lookup function
-- that uses value of the sub-field. This function returns the result of the evaluation, and the
-- updated value which can then be placed back into the sub-field if necessary. By composing
-- 'withInnerLens' functions, it is possible to construct a lens that can read and update any value
-- in arbitrarily complex data types.
withInnerLens :: sub -> ObjectFocus sub a -> ObjectFocus o (a, (Bool, sub))
withInnerLens sub f = do
  st <- _mapStructState get
  o  <- focusLiftExec $ _runObjectFocus f $ st{objectInFocus=sub}
  ObjectFocus $ predicate $ o >>= \ (a, (_ref, changed, o)) -> return (a, (changed, o))

-- Used in the 'Interface' table to convert between a @typ@ and 'Data.Dynamic.Dynamic' value.
convertFocus :: (a -> b) -> (b -> a) -> ObjectFocus a x -> ObjectFocus b x
convertFocus a2b b2a f = get >>= flip withInnerLens f . b2a >>= \ (x, (changed, a)) ->
  when changed (put $ a2b a) >> return x

focusObjectClass :: ObjectClass o => ObjectFocus o a -> ObjectFocus Object a
focusObjectClass f = do
  (a, (changed, o)) <- get >>= xmaybe . fromObj >>= flip withInnerLens f
  when changed (put $ obj o) >> return a

instance ObjectLens T_dict Name where
  updateIndex name f = do
    (result, (changed, o)) <- get >>= flip withInnerLens (focusNext f) . (M.lookup name)
    when changed (modify $ M.alter (const o) name)
    return result

instance ObjectFunctor T_dict Name where
  objectFMap f = get >>=
     mapM (\ (name, o) -> focalPathSuffix (DotRef name NullRef) $ withInnerLens [] $ f name o
          ) . M.assocs >>= put . M.fromList . concatMap (snd . snd)

focusGuardStructName :: Name -> ObjectFocus T_struct ()
focusGuardStructName name = get >>= guard . (==name) . structName

focusStructAsDict :: ObjectFocus T_dict a -> ObjectFocus T_struct a
focusStructAsDict f = get >>= \struct -> case struct of
  Nullary{ structName=name } -> do
    (a, (changed, sub)) <- withInnerLens M.empty f
    if M.null sub
    then return ()
    else when changed (put $ Struct{ structName=name, fieldMap=sub })
    return a
  Struct{ structName=name, fieldMap=sub } -> do
    (a, (changed, sub)) <- withInnerLens sub f
    when changed $ do
      if M.null sub
      then put (Nullary{ structName=name })
      else put (struct{ fieldMap=sub })
    return a

instance ObjectLens T_struct Name where
  updateIndex name f = focusStructAsDict $ updateIndex name f

instance ObjectFunctor T_struct Name where
  objectFMap f = focusStructAsDict $ objectFMap f

updateHataAsStruct :: ObjectFocus T_struct a -> ObjectFocus Hata a
updateHataAsStruct f = do
  (Hata ifc o) <- get
  (fromStruct, toStruct) <- xmaybe (return (,) <*> objFromStruct ifc <*> objToStruct ifc)
    -- here ^ evaluation backtracks if the field cannot be accessed
  struct <- predicate $ fromData toStruct o
  (a, (changed, struct)) <- withInnerLens struct f
  when changed (predicate (toData fromStruct struct) >>= put . Hata ifc)
  return a

lookupHataAsStruct :: ObjectFocus T_struct a -> ObjectFocus Hata a
lookupHataAsStruct f = do
  (Hata ifc o) <- get
  toStruct <- xmaybe (objToStruct ifc)
  struct <- predicate $ fromData toStruct o
  fst <$> withInnerLens struct f

instance ObjectLens Hata Name where
  updateIndex name f = updateHataAsStruct $ updateIndex name f

instance ObjectFunctor Hata Name where
  objectFMap = flip mplus (return ()) . updateHataAsStruct . focusStructAsDict . objectFMap

instance ObjectLens [Object] Integer where
  updateIndex idx f = get >>= \ox ->
    if idx == negate 1
    then do
      (result, (changed, o)) <- withInnerLens Nothing f
      when changed (put $ maybe ox (:ox) o)
      return result
    else do
      let splitlen i rx ox = case ox of
            []   -> (i, rx, [])
            o:ox -> if i<idx then splitlen (i+1) (rx++[o]) ox else (i, rx, o:ox)
      let (len, lo, hi) = splitlen 0 [] ox
      if 0<=idx && idx<=len
      then
        if null hi
        then do
          (result, (changed, o)) <- withInnerLens Nothing f
          when changed (put $ maybe ox ((ox++) . return) o)
          return result
        else do
          (result, (changed, o)) <- withInnerLens (Just $ head hi) f
          when changed (put $ lo ++ maybe [] return o ++ tail hi)
          return result
      else execThrow "index ouf of bounds" ExecErrorUntyped [(assertFailed, OLong idx)]

instance ObjectFunctor [Object] Integer where
  objectFMap f = get >>=
     mapM (\ (idx, o) -> focalPathSuffix (Subscript [obj idx] NullRef) $ withInnerLens [] $ f idx o
          ) . zip [0..] >>= put . map snd . sortBy (\a b -> compare (fst a) (fst b)) . concatMap (snd . snd)

_dictSubscriptUpdate
  :: ObjectLens o Name
  => String -> [Object]
  -> ObjectFocus (Maybe Object) (Maybe Object)
  -> ObjectFocus o (Maybe Object)
_dictSubscriptUpdate msg ix f = focusLiftExec (mapM derefObject ix) >>= \ix -> case ix of
  []  -> fail $ "void subscript used to index "++msg
  [ORef (Reference UNQUAL name suf)] -> updateIndex name $ updateIndex suf f
  [_] -> fail $ "non-reference subscript used to update index of "++msg
  _   -> fail $ "multi-dimensional subscript used to update index of "++msg

-- Converts the function @f@ that is passed to an 'objectFMap' which takes an index value of type
-- @i@ to a value suitable for invoking an 'objectFMap' function instantiated for a different type
-- @fi@.
objectFMapConvert
  :: (i -> ObjectFocus [(fi, Object)] fi) -> (fi -> ObjectFocus [(i, Object)] i)
  -> (fi -> Object -> ObjectFocus [(fi, Object)] ())
  -> i -> Object
  -> ObjectFocus [(i, Object)] ()
objectFMapConvert i2fi fi2i f i o = getFocalReference >>= \qref -> do
  (_, (_, changed, o)) <- focusLiftExec (runObjectFocus (i2fi i >>= flip f o) False qref [])
  o <- forM o (\ (fi, o) -> fi2i fi >>= \i -> return (i, o))
  _mapStructState $ modify $ \st -> st{objectInFocusChanged=changed, objectInFocus=o}

_dictSubscriptFMap
  :: ObjectFunctor o Name
  => String -> ([Object] -> Object -> ObjectFocus [([Object], Object)] ()) -> ObjectFocus o ()
_dictSubscriptFMap msg f = objectFMap $ objectFMapConvert i2fi fi2i f where
  i2fi :: Name -> ObjectFocus [([Object], Object)] [Object]
  i2fi name = return [ORef $ Reference UNQUAL name NullRef]
  fi2i :: [Object] -> ObjectFocus [(Name, Object)] Name
  fi2i ix = focusLiftExec (mapM derefObject ix) >>= \ix -> case ix of
    [ORef (Reference UNQUAL name NullRef)] -> return name
    _ -> fail $ "improper index value used to update field while traversing "++msg

_index1DIntegral :: Show a => String -> String -> [Object] -> (Integer -> ObjectFocus o a) -> ObjectFocus o a
_index1DIntegral msg1 msg2 ix f = focusLiftExec (mapM derefObject ix) >>= \ix -> case ix of
  []  -> fail $ "void subscript used to "++msg1++" list"++msg2
  [i] -> case extractXPure (castToCoreType LongType i) >>= fromObj of
    Nothing -> fail $ "non-integer subscript used to "++msg1++" list"++msg2
    Just  i -> f i
  _   -> fail $ "multi-dimensional subscript used to "++msg1++" list"++msg2

instance ObjectLens [Object] [Object] where
  updateIndex ix f = _index1DIntegral "update" "" ix $ flip updateIndex f

instance ObjectFunctor [Object] [Object] where
  objectFMap = objectFMap .
    objectFMapConvert (\i -> return [OLong i]) (\ix -> _index1DIntegral "traverse" "" ix return)

instance ObjectLens T_dict [Object] where
  updateIndex = _dictSubscriptUpdate "dictionary"

instance ObjectFunctor T_dict [Object] where
  objectFMap = _dictSubscriptFMap   "dictionary"

instance ObjectLens T_struct [Object] where
  updateIndex = _dictSubscriptUpdate "struct"

instance ObjectFunctor T_struct [Object] where
  objectFMap = _dictSubscriptFMap "struct"

_hataUpdateSubscript
  :: (String -> ObjectFocus T_struct (Maybe Object))
  -> ObjectFocus Hata (Maybe Object)
_hataUpdateSubscript f = do
  (Hata ifc _) <- get
  updateHataAsStruct (f $ show $ objHaskellType ifc) <|> fail "cannot update field"

_hataLookupSubscript :: (String -> ObjectFocus T_struct Object) -> ObjectFocus Hata Object
_hataLookupSubscript f = do
  (Hata ifc _) <- get
  lookupHataAsStruct $ f $ show $ objHaskellType ifc

instance ObjectLens Hata [Object] where
  updateIndex ix f = get >>= \ (Hata ifc o) -> case objIndexUpdater ifc of
    Nothing     -> _hataUpdateSubscript $ \msg -> _dictSubscriptUpdate msg ix f
    Just update -> do
      (result, (changed, o)) <- withInnerLens o $ update ix f
      when changed (put $ Hata ifc o) >> return result

instance ObjectFunctor Hata [Object] where
  objectFMap f = get >>= \ (Hata ifc o) -> case objTraverse ifc of
    Nothing       -> updateHataAsStruct (_dictSubscriptFMap (show $ objHaskellType ifc) f) <|>
      fail "cannot update field"
    Just traverse -> do
      ((), (changed, o)) <- withInnerLens o (traverse f)
      when changed (put $ Hata ifc o)

_tryFuncCall
  :: Maybe Object -> [Object] -> RefSuffix
  -> ObjectFocus (Maybe Object) (Maybe Object)
  -> ObjectFocus (Maybe Object) (Maybe Object)
  -> ObjectFocus (Maybe Object) (Maybe Object)
_tryFuncCall func args suf f els = maybe els id $ do
  (Hata ifc d) <- func >>= fromObj
  calls <- objCallable ifc
  return $ do
    -- Get the result of the function call, the result of the operation will become the focus.
    this <- get
    (result, this) <- focusLiftExec (calls d >>= flip (callCallables this) args)
    put this
    -- Focus on the result of the function call and evaluate an update on it.
    -- The updated function result is ignored, any changes made to it are lost.
    (result, _) <- case suf of
      NullRef -> withInnerLens result f
      _       -> withInnerLens result (updateIndex suf f)
    -- The function call may have updated the "this" value, place this updated value back into the focus.
    return result

_refSuffixUpdate
  :: (ObjectClass o, ObjectLens o i)
  => o -> i -> RefSuffix
  -> ObjectFocus (Maybe Object) (Maybe Object)
  -> ObjectFocus (Maybe Object) (Maybe Object)
_refSuffixUpdate o i suf f = do
  (result, (changed, o)) <- withInnerLens o $ updateIndex i $ updateIndex suf f
  when changed (put $ Just $ obj o) >> return result

-- | This is an 'ObjectUpdate' function which operates in the monad
-- > 'ObjectFocus' (Maybe 'Object') (Maybe 'Object')
-- If the object in the focus is an object constructed with 'ODict', 'OTree', or 'OHaskell', then
-- the 'Name' parameter passed to this function is used to lookup a function stored in the object in
-- focus. This function sets the object in focus to the "this" variable in the Dao runtime and then
-- calls the function with the given @['Object']@ arguments. The object in focus is updated by the
-- function call and the result of the function call is placed in the focus and updated by the next
-- update function with the next 'RefSuffix' provided (as the arguments to the 'ObjectUpdate'). This
-- is the semantics for Dao language expressions of the kind:
-- a.b(c).d(e)
-- that is, an objet stored in the variable "a" has a method "b" called with a parameter "(c)" and
-- the result of this call is an object with a method "d" that is called with the parameter "(e)".
-- Of course if the 'RefSuffix' is 'NullRef', the result of the method call is simply returned.  The
-- result of "a.b(c)" is stored on the stack and used to select and evaluate the method "d(e)",
-- however if "d(e)" modified the value on the stack, this modified value is lost when it is popped
-- off of the stack after evaluation completes. There is currently no way to update objects in this
-- way, as the Dao runtime does not have a way to update arbitrary points in it's working memory.
-- This will hopefully be improved in future versions.
callMethod :: Name -> [Object] -> ObjectUpdate (Maybe Object) RefSuffix
callMethod name args suf f = do
  qref <- _getTargetRefInfo
  let err msg = execThrow msg qref []
  o <- get
  case o of
    Nothing -> err "method call on undefined reference"
    Just  o -> case o of
      ODict    d -> _tryFuncCall (M.lookup     name d) args suf f (_refSuffixUpdate d name suf f)
      OTree    d -> _tryFuncCall (structLookup name d) args suf f (_refSuffixUpdate d name suf f)
      OHaskell (Hata ifc d) -> case M.lookup name (objMethodTable ifc) of
        Just func -> do
          (result, d) <- focusLiftExec $ executeDaoFunc func d args
          put (Just $ OHaskell $ Hata ifc d)
          -- Next we take the result of this method call and let any 'RefSuffix's that may exist to
          -- operate on it. Like _tryFuncCall, this will ignore changes made to the result of the
          -- function call.
          (result, _) <- case suf of
            NullRef -> withInnerLens result f
            _       -> withInnerLens result (updateIndex suf f)
          return result
        Nothing   -> case objToStruct ifc of
          Just toStruct -> do
            struct <- predicate (fromData toStruct d)
            _tryFuncCall (structLookup name struct) args suf f (_refSuffixUpdate (Hata ifc d) name suf f)
          Nothing       -> err "not a callable method function"
      _ -> err "method call on atomic object"
            -- TODO: provide a set of built-in methods available to every object.

instance ObjectLens (Maybe Object) RefSuffix where
  updateIndex suf f = _getTargetRefInfo >>= \qref -> focalPathSuffix suf $ get >>= \o -> case suf of
    NullRef         -> focusNext $ get >>= flip withInnerLens f >>= \ (result, (changed, o)) ->
      when changed (put o) >> return result
    DotRef name (FuncCall args suf) -> focusNext $ callMethod name args suf f
    DotRef name suf -> case o of
      Nothing -> mzero
      Just  o -> case o of
        ODict    o -> _refSuffixUpdate o name suf f
        OTree    o -> _refSuffixUpdate o name suf f
        OHaskell o -> _refSuffixUpdate o name suf f
        _          -> throwBadTypeError "referenced element of non-container object" o []
    Subscript ix suf -> focusNext $ case o of
      Nothing -> mzero
      Just  o -> case o of
        OList    o -> _refSuffixUpdate o ix suf f
        ODict    o -> _refSuffixUpdate o ix suf f
        OTree    o -> _refSuffixUpdate o ix suf f
        OHaskell o -> _refSuffixUpdate o ix suf f
        _          -> throwBadTypeError "cannot subscript of non-indexed object" o []
    FuncCall  args suf -> focusNext $ case o of
      Nothing -> mzero
      Just  o -> do
        (result, o) <- focusLiftExec (callObject qref o args)
        _mapStructState $ modify $ \st -> st{ objectInFocus=o }
        -- -^ Here we put the updated function back (it may have had it's static var table updated),
        -- but we use '_mapStructState' to do it. This is because using 'modify' or 'put' will
        -- automatically set the 'objectInFocusChanged' flag. Since many functions 'CONST' and we
        -- would like to update the function's static table without modifying the function itself
        -- without also triggering the exception thrown when a const variable is modified, we must
        -- make sure we update it in a way that would not indicate that the function object itself
        -- has been modified.
        (result, _) <- case suf of
          NullRef -> withInnerLens result f
          suf     -> withInnerLens result (updateIndex suf f)
        return result

instance ObjectFunctor Object RefSuffix where
  objectFMap f = get >>= \o -> case o of
    OList    o -> travers o int2subs subs2int OList
    ODict    o -> travers o ref2subs subs2ref ODict
    OTree    o -> travers o ref2subs subs2ref OTree
    OHaskell o -> travers o ref2subs subs2ref OHaskell
    _          -> return ()
    where
      int2subs i = return $ Subscript [OLong i] NullRef
      subs2int i = case i of
        Subscript [o] NullRef -> case extractXPure (castToCoreType LongType o) >>= fromObj of
          Nothing -> fail "while traversing list, updating function returned non-integer index value"
          Just  i -> return i
        _ -> fail "while traversing list, updating function returned non-subscript reference as index"
      ref2subs i = return $ DotRef i NullRef
      subs2ref i = case i of
        DotRef name NullRef -> return name
        _ -> fail "while traversing structure, updating function returned invalid reference"
      travers o to from constr = do
        (_, (changed, o)) <- withInnerLens o (objectFMap $ objectFMapConvert to from f)
        when changed (put $ constr o)

instance ObjectFunctor (Maybe Object) RefSuffix where
  objectFMap f = get >>= maybe (return ()) (fmap fst . flip withInnerLens (objectFMap f))

-- | If you have a data type @o@ instantiating @'ObjectLens' o index@ with a given index type, and
-- this data type @o@ is a field of another @data@ type, you can instantiate 'updateIndex' for this
-- data type by providing functions to unwrap and wrap the data type @o@ inside of it. For
-- @newtype@s, the wrapper function can be given as @('Prelude.const' MyNewtype)@ where
-- @MyNewtype@ is the newtype constructor.
innerDataUpdateIndex
  :: (Show o, ObjectLens o index)
  => (dt -> o) -> (dt -> o -> dt)
  -> index -> ObjectFocus (Maybe Object) (Maybe Object) -> ObjectFocus dt (Maybe Object)
innerDataUpdateIndex unwrap wrap i upd = get >>= \dt -> do
  (result, (changed, o)) <- withInnerLens (unwrap dt) (updateIndex i upd)
  when changed (modify $ flip wrap o) >> return result

----------------------------------------------------------------------------------------------------

instance ObjectLens (Stack Name Object) Name where
  updateIndex name upd = do
    ((result, changed), stack) <- get >>=
      stackUpdateM (flip withInnerLens upd >=> \ (result, (changed, o)) -> return ((result, changed), o)) name
    when changed (put stack) >> return result

-- | This function can be used to automatically instantiate 'updateIndex' for any type @o@ that also
-- instantiates @'ObjectLens' o 'Dao.String.Name'@.
referenceUpdateName :: ObjectLens o Name => Reference -> ObjectFocus (Maybe Object) (Maybe Object) -> ObjectFocus o (Maybe Object)
referenceUpdateName qref f = case qref of
  Reference _ name suf -> updateIndex name $ updateIndex suf f
  RefObject o suf -> case o of
    ORef o -> referenceUpdateName (refAppendSuffix o suf) f
    _      -> fail "cannot update reference"
  RefWrapper _ -> fail "cannot update reference"

-- | This function can be used to automatically instantiate 'lookupIndex' for any type @o@ that also
-- instantiates @'ObjectLens' o 'Dao.String.Name'@. This function may also performs updates on variables
-- in place if the variable contains an object and the reference is a method call which updates the
-- object.
referenceLookupName :: ObjectLens o Name => Reference -> ObjectFocus o Object
referenceLookupName qref = case qref of
  Reference _ name suf -> updateIndex name get >>=
    fmap fst . flip withInnerLens (updateIndex suf get) >>= maybe (execThrow "" qref []) return
  RefObject o      suf -> case o of
    ORef o -> referenceLookupName (refAppendSuffix o suf)
    _      -> fail "cannot update reference"
  RefWrapper _ -> fail "cannot update reference"

instance ObjectLens (Stack Name Object) Reference where { updateIndex = referenceUpdateName }

----------------------------------------------------------------------------------------------------

updateLocal :: Name -> RefSuffix -> ObjectFocus (Maybe Object) (Maybe Object) -> ObjectFocus () (Reference, Maybe Object)
updateLocal name suf f = do
  stack <- focusLiftExec (gets execStack)
  (result, (changed, o)) <- withInnerLens (stackLookup name stack) (updateIndex suf f)
  -- The updateIndex function may have performed a function call that updated the local stack so we
  -- need to get the (possibly) updated stack from the 'ExecUnit' once again and operate on that.
  when changed $ focusLiftExec (gets execStack) >>= \stack -> focusLiftExec $ modify $ \xunit ->
    xunit{ execStack = snd $ stackUpdate (const ((), o)) name stack }
  return (Reference LOCAL name suf, result)

updateConst :: Name -> RefSuffix -> ObjectFocus (Maybe Object) (Maybe Object) -> ObjectFocus () (Reference, Maybe Object)
updateConst name suf f = do
  let qref = Reference CONST name suf
  consts <- focusLiftExec (gets builtinConstants)
  (result, (changed, _)) <- withInnerLens (M.lookup name consts) (updateIndex suf f)
  when changed $ case suf of -- Modifying a const variable directly fails.
    NullRef -> execThrow "attempted modification of immutable value" ExecErrorUntyped [(modifiedConst, obj qref)]
    _       -> return () -- modifying a member of a const value is OK but the updated value is disgarded.
  return (qref, result)

updateStatic :: Name -> RefSuffix -> ObjectFocus (Maybe Object) (Maybe Object) -> ObjectFocus () (Reference, Maybe Object)
updateStatic name suf f = do
  let qref = Reference STATIC name suf
  store <- focusLiftExec (gets currentCodeBlock) >>= maybe mzero return
  (result, (changed, o)) <- withInnerLens (M.lookup name $ staticVars store) (updateIndex suf f)
  when changed $ focusLiftExec (gets currentCodeBlock) >>= \store -> case store of
    Nothing    -> return ()
    Just store -> focusLiftExec $ modify $ \xunit ->
      xunit
      { currentCodeBlock = Just $
          store{ staticVars = M.alter (const o) name (staticVars store) }
      }
  return (qref, result)

updateGlobal :: Name -> RefSuffix -> ObjectFocus (Maybe Object) (Maybe Object) -> ObjectFocus () (Reference, Maybe Object)
updateGlobal name suf f = do
  let qref = Reference GLOBAL name suf
  store <- focusLiftExec (gets globalData)
  (result, (changed, o)) <- withInnerLens (M.lookup name store) (updateIndex suf f)
  when changed $ focusLiftExec (gets globalData) >>= \store -> focusLiftExec $ modify $ \xunit ->
    xunit{ globalData = M.alter (const o) name store }
  return (qref, result)

updateWithRef :: Name -> RefSuffix -> ObjectFocus (Maybe Object) (Maybe Object) -> ObjectFocus () (Reference, Maybe Object)
updateWithRef name suf f = do
  let qref = Reference GLODOT name suf
  store <- focusLiftExec (gets currentWithRef)
  case store of
    Nothing -> updateGlobal name suf f
    Just  o -> do
      (result, (changed, o)) <- withInnerLens (Just o) (updateIndex suf f)
      when changed $ focusLiftExec $ modify $ \xunit -> xunit{ currentWithRef=o }
      return (qref, result)

instance ObjectLens () Name where
  updateIndex name = updateIndex (Reference UNQUAL name NullRef)

instance ObjectLens () Reference where
  updateIndex qref f = do
    (qref, result) <- case qref of
      Reference q name suf -> case q of
        UNQUAL -> fmap fst $ withInnerLens () $ msum $
          [ updateLocal   name suf f
          , updateConst   name suf f
          , updateStatic  name suf f
          , updateGlobal  name suf f
          , updateWithRef name suf f
          ]
        LOCAL  -> updateLocal   name suf f
        CONST  -> updateConst   name suf f
        STATIC -> updateStatic  name suf f
        GLOBAL -> updateGlobal  name suf f
        GLODOT -> updateWithRef name suf f
      _ -> execThrow "cannot update reference" qref []
    _setTargetRefInfo qref >> return result

----------------------------------------------------------------------------------------------------

-- | Error report indicating which function was being evaluated when the error occurred.
errInFunc :: Name
errInFunc = ustr "errInFunc"

-- | Error report indicating which function was being evaluated when the error occurred.
errInConstr :: Name
errInConstr = ustr "errInConstr"

-- | Error report indicating which function was being evaluated when the error occurred.
errInInitzr :: Name
errInInitzr = ustr "errInInitzr"

-- | Error report indicating which function was being evaluated when the error occurred.
errOfReference :: Name
errOfReference = ustr "errOfReference"

-- | Error report for function calls indicating which argument to the function was incorrect.
argNum :: Name
argNum = ustr "argNum"

-- | Error report for function calls indicating the number of arguments passed to the function.
numArgsPassed :: Name
numArgsPassed = ustr "numArgsPassed"

-- | Error report for subscript expressions indicating the number of dimensions expected
expectNumArgs :: Name
expectNumArgs = ustr "expectNumArgs"

-- | Error report for subscript expressions indicating the number of dimensions expected
exectDimension :: Name
exectDimension = ustr "expectDimension"

-- | Error report used any place an object value of an incorrect data type was given.
expectType :: Name
expectType = ustr "expectType"

-- | Error report indicating that the data type of the 'Object' given was incorrect.
actualType :: Name
actualType = ustr "actualType"

-- | Error report indicating the data type of the right-hand side of an infix operator.
leftSideType :: Name
leftSideType = ustr "leftSideType"

-- | Error report indicating the data type of the right-hand side of an infix operator.
rightSideType :: Name
rightSideType = ustr "rightSideType"

-- | Error report indicating an attempt to modify a 'CONST' 'Reference'.
modifiedConst :: Name
modifiedConst = ustr "modifiedConst"

-- | Error report indicating an object value was of the correct type but was out of bounds or was
-- otherwise not correct.
assertFailed :: Name
assertFailed = ustr "assertFailed"

-- | Error report indicating a function call evalauted to void.
returnedVoid :: Name
returnedVoid = ustr "returnedVoid"

-- | This is a simple dictionary of strings that can translate the keys of the 'execErrorInfo'
-- dictionary to more meaningful explanatory strings when reporting error messages.
errorDict :: M.Map Name UStr
errorDict = M.fromList $ fmap (fmap ustr) $
  [(errInFunc     , "in function call")
  ,(errInConstr   , "in constructor")
  ,(errInInitzr   , "in initializer list")
  ,(errOfReference, "of reference")
  ,(argNum        , "argument number")
  ,(expectNumArgs , "number of arguments expected")
  ,(exectDimension, "dimensional data type")
  ,(numArgsPassed , "number of arguments given")
  ,(expectType    , "expecting type")
  ,(actualType    , "actual type used")
  ,(leftSideType  , "data type of the left-hand operand")
  ,(rightSideType , "data type of the right-hand operand")
  ,(modifiedConst , "modification on constant reference")
  ,(assertFailed  , "object value fails assertion test")
  ,(returnedVoid  , "expression evaluated to void")
  ]

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
    { execErrorMessage  :: UStr
    , execErrorInModule :: Maybe UStr
    , execErrorLocation :: Location
    , execErrorSubtype  :: ExecErrorSubtype
    , execErrorInfo     :: T_dict
    }
  deriving Typeable

instance HasNullValue ExecControl where
  nullValue = ExecReturn Nothing
  testNull (ExecReturn Nothing) = True
  testNull  _                   = False

instance PPrintable ExecControl where
  pPrint err = case err of 
    ExecError{ execErrorMessage=msg, execErrorLocation=loc } -> do
      maybe (return ()) (pString . (++":") . uchars) (execErrorInModule err)
      pShow loc >> pString (if testNull loc then " " else ": ")
      when (not $ msg==nil) (pUStr msg)
      pIndent $ do
        pEndLine >> pPrint (execErrorSubtype err)
        forM_ (M.assocs $ execErrorInfo err) $ \ (key, val) -> do
          pEndLine
          maybe (pPrint key >> pString ": ") (pString . (++" ") . uchars) (M.lookup key errorDict)
          pPrint val
    ExecReturn{ execReturnValue=o } -> maybe (return ()) pPrint o

instance ToDaoStructClass ExecControl where
  toDaoStruct = ask >>= \o -> case o of
    ExecReturn{} -> renameConstructor "ExecReturn" $ asks execReturnValue >>= ("value" .=?)
    ExecError {} -> renameConstructor "ExecError" $ do
      asks execErrorInModule >>= ("inModule" .=?)
      "message"  .=@ execErrorMessage
      "location" .=@ execErrorLocation
      "subtype"  .=@ execErrorSubtype
      "info"     .=@ execErrorInfo

instance HataClass ExecControl where
  haskellDataInterface = interface "ExecControl" $ do
    autoDefPPrinter >> autoDefNullTest >> autoDefToStruct

instance HasNullValue ExecErrorSubtype where
  nullValue = ExecErrorUntyped
  testNull ExecErrorUntyped = True
  testNull _               = False

newError :: ExecControl
newError =
  ExecError
  { execErrorMessage  = nil
  , execErrorInModule = Nothing
  , execErrorLocation = LocationUnknown
  , execErrorSubtype  = ExecErrorUntyped
  , execErrorInfo     = mempty
  }

throwArityError :: MonadError ExecControl m => String -> Int -> [o] -> [(Name, Object)] -> m ig
throwArityError msg i ox info = execThrow fullmsg ExecErrorUntyped moreInfo where
  (before, after) = splitAt 100 ox
  fullmsg = (++(if null msg then "" else ", "++msg)) $
    if null after then "incorrect number of arguments given" else "over 100 arguments given"
  moreInfo = concat $
    [ if null after then [(numArgsPassed, OInt $ length before)] else []
    , [(expectNumArgs, OInt i)], info
    ]

throwBadTypeError :: MonadError ExecControl m => String -> Object -> [(Name, Object)] -> m ig
throwBadTypeError msg o info = execThrow msg (ExecTypeError $ typeOfObj o) info

throwParseError :: MonadError ExecControl m => String -> Maybe UPath -> ParseError () DaoTT -> [(Name, Object)] -> m ig
throwParseError msg mod err info = throwError $
  newError
  { execErrorMessage  = ustr msg
  , execErrorInfo     = M.fromList info
  , execErrorLocation = parseErrLoc err
      -- -^ set the error location in the 'ExecControl' structure
  , execErrorSubtype  = ExecParseError $ err{ parseErrLoc = LocationUnknown }
      -- -^ delete the location to prevent it from being displayed twice
  , execErrorInModule = mod
  }

-- | Evaluate a monadic function which may throw an 'ExecControl' 'Dao.Predicate.PFail' predicate
-- value. If the monadic function does fail, the 'Doa.Predicate.PFail' value will be updated with
-- the 'Dao.Token.Location' value retrieved by evaluating 'Dao.Token.getLocation' on the object
-- provided. If the evaluation failed and has already set an error location, the location is not
-- modified by this function, which guarantees the inner-most call to 'errLocation' will set the
-- location of the error.
errLocation :: (MonadError ExecControl m, HasLocation o) => o -> m a -> m a
errLocation o f = catchError f $ \err -> case err of
  ExecError{execErrorLocation=LocationUnknown} ->
    throwError $ err{execErrorLocation=getLocation o}
  err -> throwError err

-- | Evaluate a monadic function which may throw an 'ExecControl' 'Dao.Predicate.PFail' predicate
-- value. If the monadic function does fail, the 'Doa.Predicate.PFail' value will be updated with
-- the given module name. If the evaluation failed and has already set an error module, the module
-- is not modified by this function, which guarantees the inner-most call to 'errModule' will set
-- the module of the error.
errModule :: MonadError ExecControl m => UPath -> m a -> m a
errModule path f = catchError f $ \err -> case err of
  ExecError{execErrorInModule=mod} -> throwError $ err{execErrorInModule=mplus mod (Just path)}
  err -> throwError err

-- | Like 'errModule' but sets the module reported by the error to be the current module of the
-- 'ExecUnit' if it is defined. If it is not defined, this function is equivalent to 'Prelude.id'.
errCurrentModule :: Exec a -> Exec a
errCurrentModule f = gets programModuleName >>= \mod -> maybe id errModule mod $ f

-- | Evaluate a monadic function which may throw an 'ExecControl' 'Dao.Predicate.PFail' predicate
-- value. If the monadic function does fail, the 'execErrorInfo' field of the 'ExecError' value in
-- the 'Dao.Predicate.PFail' predicate will be updated with the field 'Name' and 'Object' value
-- provided here.
errInfo :: MonadError ExecControl m => Name -> Object -> m a -> m a
errInfo name o f = catchError f $ \err -> case err of
  ExecError{execErrorInfo=info} -> throwError $ err{execErrorInfo=M.insert name o info}
  err -> throwError err

-- | Evaluate an 'Exec', but if it throws an exception, set record an 'ObjectExpr' where
-- the exception occurred in the exception information.
updateExecErrorInfo :: Name -> Object -> Exec a -> Exec a
updateExecErrorInfo name o fn = catchError fn $ \err -> case err of
  ExecReturn{}                  -> throwError err
  ExecError{execErrorInfo=info} -> throwError $ err{ execErrorInfo = M.insert name o info }

-- | If an error has not been caught, log it in the module where it can be retrieved later. This
-- function only stores errors constructed with 'ExecError', the 'ExecReturn' constructed objects
-- are ignored.
logUncaughtErrors :: [ExecControl] -> Exec ()
logUncaughtErrors errs = modify $ \xunit ->
  xunit{ uncaughtErrors = uncaughtErrors xunit ++
    (errs >>= \e -> case e of { ExecReturn{} -> []; ExecError{} -> [e]; }) }

-- | Clear the log of uncaught 'ExecError' values stored by 'logUncaughtErrors'.
clearUncaughtErrorLog :: Exec [ExecControl]
clearUncaughtErrorLog = do
  errs <- gets uncaughtErrors
  modify $ \xunit -> xunit{ uncaughtErrors = [] }
  return errs

----------------------------------------------------------------------------------------------------

data ExecErrorSubtype
  = ExecErrorUntyped
  | ExecThrow         Object
    -- ^ thrown when evaluating a "throw" statement, that is 'Dao.Interpreter.AST.ReturnExpr'
  | ExecStructError   StructError -- ^ thrown by 'toDaoStruct' or 'fromDaoStruct'
  | ExecUndefinedRef  Reference   -- ^ signals reference lookup failed
  | ExecTypeError     ObjType     -- ^ catch-all exception thrown when wrong data type is used.
  | ExecUpdateOpError UpdateOp    -- ^ thrown when an update operator fails
  | ExecIOException   IOException -- ^ re-thrown when caught from the IO monad
  | ExecHaskellError  ErrorCall   -- ^ re-thrown when caught from the IO monad
  | ExecParseError    (ParseError () DaoTT)
  | ExecInfixOpError  ObjType InfixOp ObjType -- ^ thrown when an infix operator fails
  deriving (Eq, Typeable)

instance ToDaoStructClass ExecErrorSubtype where
  toDaoStruct = ask >>= \o -> case o of
    ExecErrorUntyped    -> makeNullary "Error"
    ExecThrow         o -> renameConstructor "Exception"          $ "threw"     .= o
    ExecStructError   o -> innerToStruct o
    ExecUndefinedRef  o -> renameConstructor "UndefinedRef"       $ "reference" .= o
    ExecTypeError     o -> renameConstructor "TypeMismatch"       $ "usedType"  .= o
    ExecUpdateOpError o -> renameConstructor "UpdateOpError"      $ "operator"  .= o
    ExecIOException   o -> renameConstructor "HaskellIOException" $ "message"   .= obj (show o)
    ExecHaskellError  o -> renameConstructor "HaskellError"       $ "message"   .= obj (show o)
    ExecParseError    o -> innerToStruct o
    ExecInfixOpError  a o b -> renameConstructor "InfixOpError" $
      "operator" .= o >> "left" .= a >> "right" .= b

instance PPrintable ExecErrorSubtype where
  pPrint o = case o of
    ExecErrorUntyped    -> return ()
    ExecThrow         o -> pString "threw exception " >> pPrint o
    ExecStructError   o -> pPrint o
    ExecUndefinedRef  o -> pString "undefined reference " >> pPrint o
    ExecTypeError     o -> pString "cannot use value of type " >> pPrint o
    ExecUpdateOpError o -> pString "cannot apply update with operator " >> pPrint o
    ExecIOException   o -> pString (show o)
    ExecHaskellError  o -> pString (show o)
    ExecParseError    o -> pPrint o
    ExecInfixOpError  a o b -> do
      pString "incompatible types on either side of operator " >> pPrint o
      pIndent $ do
        pEndLine >> pString "left-hand side of operator is value of type: "  >> pPrint a
        pEndLine >> pString "right-hand side of operator is value of type: " >> pPrint b

instance ObjectClass ExecErrorSubtype where { obj=new; fromObj=objFromHata; }

instance HataClass ExecErrorSubtype where
  haskellDataInterface = interface "ErrorSubtype" $ do
    autoDefEquality >> autoDefPPrinter >> autoDefToStruct

----------------------------------------------------------------------------------------------------

-- | All evaluation of the Dao language takes place in the 'Exec' monad. It instantiates
-- 'Control.Monad.MonadIO.MonadIO' to allow @IO@ functions to be lifeted into it. It instantiates
-- 'Control.Monad.Error.MonadError' and provides it's own exception handling mechanism completely
-- different from the Haskell runtime, so as to allow for more control over exception handling in
-- the Dao runtime.
newtype Exec a  = Exec{ execToPredicate :: PredicateT ExecControl (StateT ExecUnit IO) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadState ExecUnit Exec where { state = Exec . lift . state }

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
newtype XPure a = XPure { xpureToState :: PredicateT ExecControl (State UStr) a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Show a => Show (XPure a) where
  show (XPure p) = case evalState (runPredicateT p) nil of
    Backtrack -> "(BACKTRACK)"
    PFail err -> "(PFAIL "++prettyShow err++")"
    OK     o  -> "(OK "++show o++")"

instance Monad XPure where
  return = XPure . return
  (XPure a) >>= f = XPure $ a >>= xpureToState . f
  fail msg = execThrow msg ExecErrorUntyped []

-- | Convert a value wrapped in an XPure monad to a pair containing the internal state 'Dao.String.UStr' and
-- 'Dao.Predicate.Predicate' value.
runXPure :: XPure a -> (Predicate ExecControl a, UStr)
runXPure = flip runState nil . runPredicateT . xpureToState

-- | Like 'Control.Monad.State.evalState', but works on the 'XPure' monad, i.e. it is defined as
-- > 'Prelude.fst' . 'runXPure'
evalXPure :: XPure a -> Predicate ExecControl a
evalXPure = fst . runXPure

-- | Like 'evalXPure' but evaluates to 'Prelude.Maybe' instead of a 'Dao.Predicate.Predicate'.
-- 'Dao.Predicate.Backtrack' and 'Dao.Predicate.PFail' both map to 'Prelude.Nothing',
-- 'Dao.Predicate.OK' maps to 'Prelude.Just'.
extractXPure :: XPure a -> Maybe a
extractXPure = okToJust . evalXPure

instance MonadError ExecControl XPure where
  throwError = XPure . throwError
  catchError (XPure f) catch = XPure $ catchError f (xpureToState . catch)

instance MonadPlusError ExecControl XPure where
  predicate = XPure . predicate
  catchPredicate (XPure f) = XPure $ catchPredicate f

instance MonadState UStr XPure where { state = XPure . lift . state }

instance Executable (XPure a) a where
  execute (XPure f) = predicate $ evalState (runPredicateT f) mempty

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
  toExecErrorInfo :: o -> ExecErrorSubtype
  -- | Like 'Prelude.error' but works for the 'Exec' monad, throws an 'ExecControl' using
  -- 'Control.Monad.Error.throwError' constructed using the given 'Object' value as the
  -- 'execReturnValue'.
  execThrow
    :: (Monad m, MonadError ExecControl m, ExecThrowable o, UStrType msg)
    => msg -> o -> [(Name, Object)] -> m ig
  execThrow msg o info = throwError $
    newError
    { execErrorMessage  = toUStr msg
    , execErrorSubtype  = toExecErrorInfo o
    , execErrorInfo     = M.fromList info
    }

instance ExecThrowable ExecErrorSubtype      where { toExecErrorInfo = id                }
instance ExecThrowable Object                where { toExecErrorInfo = ExecThrow         }
instance ExecThrowable StructError           where { toExecErrorInfo = ExecStructError   }
instance ExecThrowable Reference             where { toExecErrorInfo = ExecUndefinedRef  }
instance ExecThrowable IOException           where { toExecErrorInfo = ExecIOException   }
instance ExecThrowable ErrorCall             where { toExecErrorInfo = ExecHaskellError  }
instance ExecThrowable UpdateOp              where { toExecErrorInfo = ExecUpdateOpError }
instance ExecThrowable (ParseError () DaoTT) where { toExecErrorInfo = ExecParseError    }

ioExec :: Exec a -> ExecUnit -> IO (Predicate ExecControl a, ExecUnit)
ioExec func xunit = runStateT (runPredicateT (execToPredicate func)) xunit

----------------------------------------------------------------------------------------------------

-- | This is the data type analogous to the 'Exec' monad what 'Control.Exception.Handler' is to the
-- @IO@ monad.
newtype ExecHandler a =
  ExecHandler { execHandler :: ExecUnit -> Handler (Predicate ExecControl a, ExecUnit) }

instance Functor ExecHandler where
  fmap f (ExecHandler h) = ExecHandler (fmap (fmap (\ (p, xunit) -> (fmap f p, xunit))) h)

-- | Create an 'ExecHandler'.
newExecIOHandler :: Exception e => (e -> Exec a) -> ExecHandler a
newExecIOHandler h = ExecHandler (\xunit -> Handler (\e -> ioExec (h e) xunit))

-- | Using an 'ExecHandler' like 'execIOHandler', catch any exceptions thrown by the Haskell
-- language runtime and wrap them up in the 'Exec' monad.
execCatchIO :: Exec a -> [ExecHandler a] -> Exec a
execCatchIO tryFunc handlers = Exec $ PredicateT $ StateT $ \xunit ->
  liftIO $ catches (ioExec tryFunc xunit) (fmap (\h -> execHandler h xunit) handlers)

-- | Like 'execCatchIO' but with the arguments 'Prelude.flip'ped.
execHandleIO :: [ExecHandler a] -> Exec a -> Exec a
execHandleIO = flip execCatchIO

-- | An 'ExecHandler' for catching 'Control.Exception.ErrorCall's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execIOHandler :: ExecHandler ()
execIOHandler = newExecIOHandler $ flip (execThrow "") [] . ExecIOException

-- | An 'ExecHandler' for catching 'Control.Exception.ErrorCall's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execErrorHandler :: ExecHandler ()
execErrorHandler = newExecIOHandler $ flip (execThrow "") [] . ExecHaskellError

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
-- a function call. The stack is always poped when this function is done evaluating, even if the
-- given 'Exec' function evaluates to 'Control.Monad.mzero' or 'Control.Monad.Error.throwError'.
execNested :: T_dict -> Exec a -> Exec (a, T_dict)
execNested init exe = do
  store <- gets execStack
  modify $ \xunit -> xunit{ execStack = stackPush init store }
  result <- catchPredicate exe
  store <- gets execStack
  (store, dict) <- pure (stackPop store)
  modify $ \xunit -> xunit{ execStack = store }
  result <- predicate result
  return (result, dict)

-- | Like 'execNested' but immediately disgards the local variables when the inner 'Exec' function
-- has completed evaluation.
execNested_ :: T_dict -> Exec a -> Exec a
execNested_ init = fmap fst . execNested init

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. This function is different from 'nestedExecStak' in that it acually removes the current
-- execution stack so a function call cannot modify the local variables of the function which called
-- it. Furthermore it catches evaluation of a "return" statement allowing the function which called
-- it to procede with execution after this call has returned.
execFuncPushStack :: T_dict -> Exec (Maybe Object) -> Exec (Maybe Object, T_dict)
execFuncPushStack dict exe = execNested dict (catchPredicate exe) >>= \ (pval, dict) -> case pval of
  OK                o  -> return (o, dict)
  Backtrack            -> mzero
  PFail (ExecReturn o) -> return (o, dict)
  PFail           err  -> throwError err

execFuncPushStack_ :: T_dict -> Exec (Maybe Object) -> Exec (Maybe Object)
execFuncPushStack_ dict = fmap fst . execFuncPushStack dict

execWithStaticStore :: Subroutine -> Exec a -> Exec a
execWithStaticStore sub exe = do
  store <- gets currentCodeBlock
  modify (\st -> st{ currentCodeBlock=Just sub })
  result <- catchPredicate exe
  modify (\st -> st{ currentCodeBlock=store })
  predicate result

execWithWithRefStore :: Object -> Exec a -> Exec a
execWithWithRefStore o exe = do
  store <- gets currentWithRef
  modify (\st -> st{ currentWithRef=Just o })
  result <- catchPredicate exe
  modify (\st -> st{ currentWithRef=store })
  predicate result

withExecTokenizer :: ExecTokenizer -> Exec a -> Exec a
withExecTokenizer newtokzer f = do
  oldtokzer <- gets programTokenizer
  modify $ \xunit -> xunit{ programTokenizer=newtokzer }
  p <- catchPredicate f
  modify $ \xunit -> xunit{ programTokenizer=oldtokzer }
  predicate p

----------------------------------------------------------------------------------------------------

instance (Typeable a, ObjectClass a) => ToDaoStructClass (Com a) where
  toDaoStruct = renameConstructor "Com" $ do
    co <- ask
    let put o = "com" .= obj o
    case co of
      Com          o    ->                   put o >> return ()
      ComBefore c1 o    -> "before" .= c1 >> put o >> return ()
      ComAfter     o c2 ->                   put o >> "after" .= c2 >> return ()
      ComAround c1 o c2 -> "before" .= c1 >> put o >> "after" .= c2 >> return ()

instance (Typeable a, ObjectClass a) => FromDaoStructClass (Com a) where
  fromDaoStruct = do
    constructor "Com"
    let f name = tryField name (maybe (fail name) return . fromObj)
    before <- optional $ f "before"
    after  <- optional $ f "after"
    o      <- req "com"
    return $ maybe (Com o) id $ msum $
      [ return ComAround <*> before <*> pure o <*> after
      , return ComBefore <*> before <*> pure o
      , return ComAfter  <*> pure o <*> after
      ]

instance (Typeable a, ObjectClass a) =>
  ObjectClass (Com a) where { obj=new; fromObj=objFromHata; }
instance (Typeable a, ObjectClass a) => HataClass (Com a) where
  haskellDataInterface = interface "Com" $ do
    autoDefToStruct >> autoDefFromStruct

instance (Typeable a, ObjectClass a) =>
  ObjectClass [Com a] where { obj=listToObj; fromObj=listFromObj; }

----------------------------------------------------------------------------------------------------

setupCodeBlock :: CodeBlock Object -> Subroutine
setupCodeBlock scrp =
  Subroutine
  { origSourceCode = scrp
  , staticVars     = mempty
  , staticRules    = mempty
  , staticLambdas  = []
  , executable     = execute scrp >> return Nothing
  }

-- binary 0xDD 
instance B.Binary (CodeBlock Object) MTab where
  put (CodeBlock o) = B.prefixByte 0xDD $ B.put o
  get = B.tryWord8 0xDD $ CodeBlock <$> B.get

instance Executable (CodeBlock Object) () where { execute (CodeBlock ox) = mapM_ execute ox }

instance ObjectClass (CodeBlock Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (CodeBlock Object) where
  haskellDataInterface = interface "CodeBlock" $ do
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
    { origSourceCode :: CodeBlock Object
    , staticVars     :: T_dict
    , staticRules    :: PatternTree Object [Subroutine]
    , staticLambdas  :: [CallableCode]
    , executable     :: Exec (Maybe Object)
    }
  deriving Typeable

instance Eq Subroutine where { a==b = origSourceCode a == origSourceCode b }

instance Ord Subroutine where { compare a b = compare (origSourceCode a) (origSourceCode b) }

instance Show Subroutine where { show o = "Subroutine "++show (codeBlock (origSourceCode o)) }

instance NFData Subroutine where { rnf (Subroutine a _ _ _ _) = deepseq a () }

instance HasNullValue Subroutine where
  nullValue =
    Subroutine
    { origSourceCode = nullValue
    , staticVars     = mempty
    , staticRules    = mempty
    , staticLambdas  = []
    , executable     = return Nothing
    }
  testNull (Subroutine a _ _ _ _) = testNull a

instance PPrintable Subroutine where
  pPrint = pPrint . flip MetaEvalExpr LocationUnknown . origSourceCode

instance ToDaoStructClass Subroutine where
  toDaoStruct = renameConstructor "Subroutine" $ do
    "code"    .=@ origSourceCode
    "vars"    .=@ staticVars
    "rules"   .=@ (\rs -> RuleSet{ ruleSetRules=rs, ruleSetTokenizer=Nothing }) . staticRules
    "lambdas" .=@ staticLambdas

instance FromDaoStructClass Subroutine where
  fromDaoStruct = do
    constructor "Subroutine"
    sub <- setupCodeBlock <$> req "code"
    vars <- req "vars"
    (RuleSet{ ruleSetRules=rules }) <- req "rules"
    lambdas <- req "lambdas"
    return $ sub{ staticVars=vars, staticRules=rules, staticLambdas=lambdas }

instance Executable Subroutine (Maybe Object) where
  execute sub = execWithStaticStore sub $
    catchReturn return ((execute (origSourceCode sub) :: Exec ()) >> return Nothing) :: Exec (Maybe Object)

instance ObjectClass Subroutine where { obj=new; fromObj=objFromHata; }

instance HataClass Subroutine where
  haskellDataInterface = interface "Subroutine" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

-- | Although 'Subroutine' instantiates 'Executable', this function allows you to easily place a
-- group of defined local variables onto the call stack before and the have the 'Subroutine'
-- executed.
runCodeBlock :: T_dict -> Subroutine -> Exec (Maybe Object, T_dict)
runCodeBlock initStack sub = execWithStaticStore sub $
  execFuncPushStack initStack (executable sub >>= liftIO . evaluate)

runCodeBlock_ :: T_dict -> Subroutine -> Exec (Maybe Object)
runCodeBlock_ initStack = fmap fst . runCodeBlock initStack

----------------------------------------------------------------------------------------------------

data RuleSet
  = RuleSet
    { ruleSetRules     :: PatternTree Object [Subroutine]
    , ruleSetTokenizer :: Maybe ExecTokenizer
    }
  deriving Typeable

instance HasNullValue RuleSet where
  nullValue = RuleSet{ ruleSetRules=nullValue, ruleSetTokenizer=Nothing }
  testNull (RuleSet{ruleSetRules=r, ruleSetTokenizer=tok}) =
    testNull r && maybe True (const False) tok

instance Monoid RuleSet where
  mempty = nullValue
  mappend (RuleSet{ruleSetRules=a, ruleSetTokenizer=tokA}) (RuleSet{ruleSetRules=b, ruleSetTokenizer=tokB}) =
    RuleSet{ ruleSetRules=mappend a b, ruleSetTokenizer=mplus tokA tokB }

instance Sizeable RuleSet where { getSizeOf = return . obj . T.size . ruleSetRules }

instance PPrintable RuleSet where
  pPrint (RuleSet{ ruleSetRules=tree }) = pList (pString "RuleSet") "{ " ", " "}" $
    T.assocs tree >>= \ (ix, subs) -> do
      let rule ix = case ix of
            []                      -> ""
            Single (OString s) : ix -> uchars s ++ rule ix
            Single o           : ix -> prettyShow o ++ rule ix
            i                  : ix -> show i ++ rule ix
      sub <- subs
      [ pClosure (pString "rule " >> pShow (rule ix)) "{" "}" $
          map pPrint $ codeBlock $ origSourceCode sub ]

instance ObjectClass RuleSet where { obj=new ; fromObj=objFromHata; }

instance HataClass RuleSet where
  haskellDataInterface = interface "RuleSet" $ do
    autoDefNullTest >> autoDefSizeable >> autoDefPPrinter
    let qrefRuleSet = reference UNQUAL (ustr "RuleSet")
    let initParams ox = case ox of
          []  -> return $ RuleSet{ ruleSetRules=mempty, ruleSetTokenizer=Nothing }
          [o] -> do
            let err :: Exec RuleSet
                err = throwBadTypeError "" o [(errInConstr, obj qrefRuleSet)]
            maybe err return $ do
              fromObj o >>= \ (Hata ifc _) -> objCallable ifc
              let tok = ExecTokenizer $ \ox -> do
                    toks <- fst <$> callObject (RefObject o NullRef) o [obj ox]
                    mplus (xmaybe $ toks >>= fromObj) $
                      execThrow "tokenizer for rule function did not return a list of objects" ExecErrorUntyped []
              return $ RuleSet{ ruleSetRules=mempty, ruleSetTokenizer=Just tok }
          _  -> throwArityError "" 1 ox [(errInInitzr, obj qrefRuleSet)]
          -- TODO: ^ the constructor for a 'PatternTree' should take tokenizer function.
    let listParams tree =
          foldM (\ rs@(RuleSet{ruleSetRules=tree, ruleSetTokenizer=maybeTok}) (i, o) -> case o of
            InitSingle o -> case fromObj o >>= \ (Hata _ d) -> fromDynamic d of
              Nothing -> throwBadTypeError "expecting rule or RuleSet" o $
                [(errInFunc, obj qrefRuleSet), (errInConstr, OInt i)]
              Just  p -> do
                newtree <- maybe id withExecTokenizer maybeTok $ execute (p::PatternRule)
                return $ rs{ ruleSetRules=T.unionWith (++) tree newtree }
            InitAssign{} -> fail "cannot use assignment expression in initializer of RuleSet"
                ) tree . zip [1..]
    defInitializer initParams listParams
    defInfixOp ORB $ \ _ rs o -> fmap (obj . mappend rs) $ mplus (xmaybe $ fromObj o) $
      (throwBadTypeError "when uninioning RuleSet values" o [])
    let run f =
          daoFunc
          { daoForeignFunc = \rs ->
              runTokenizer >=> makeActionsForQuery [ruleSetRules rs] >=> fmap (flip (,) rs) . f
          }
    defMethod "query" $ run $ return . Just . obj . fmap obj
    defMethod "do"    $ run $ msum . fmap execute
    defMethod "doAll" $ run $ fmap (Just . obj . fmap obj) . execute
    defMethod "tokenize" $
      daoFunc
      { daoForeignFunc = \rs -> fmap (flip (,) rs . Just . obj) .
          maybe runTokenizer runTokenizerWith (ruleSetTokenizer rs)
      }

----------------------------------------------------------------------------------------------------

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion with Haskell's "Data.Function"). 
data CallableCode
  = CallableCode
    { argsPattern    :: ParamListExpr Object
    , returnType     :: ObjType
    , codeSubroutine :: Subroutine
    }
  deriving (Show, Typeable)

-- Used by the instantiation of CallableCode and PatternRule into the PPrintable class.
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

instance PPrintable [CallableCode] where 
  pPrint = sequence_ . intersperse (pString " ^ ") .
    fmap (\ (CallableCode pats ty exe) -> ppCallableAction "function" (pPrint pats) ty exe >> pEndLine)

instance ObjectClass [CallableCode] where { obj=new; fromObj=objFromHata; }

instance HataClass [CallableCode] where
  haskellDataInterface = interface "Function" $ do
    autoDefNullTest >> autoDefPPrinter
    defCallable return
    defInfixOp XORB $ \ _ a o -> case fromObj o of
      Just  b -> return $ obj (a++b)
      Nothing -> fail "left-hand side of bitwise-XOR operator (^) is a Function, right hand side is not"

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_CodeBlock Object) where
  toDaoStruct = renameConstructor "CodeBlock" $ "list" .=@ getAST_CodeBlock

instance FromDaoStructClass (AST_CodeBlock Object) where
  fromDaoStruct = constructor "CodeBlock" >> AST_CodeBlock <$> req "list"

instance ObjectClass (AST_CodeBlock Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_CodeBlock Object) where
  haskellDataInterface = interface "CodeBlockExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0xC5 0xC7
instance B.Binary a MTab => B.Binary (TyChkExpr a Object) MTab where
  put o = case o of
    NotTypeChecked a       -> B.prefixByte 0xC5 $ B.put a
    TypeChecked    a b c   -> B.prefixByte 0xC6 $ B.put a >> B.put b >> B.put c
    DisableCheck   a b c d -> B.prefixByte 0xC7 $ B.put a >> B.put b >> B.put c >> B.put d
  get = B.word8PrefixTable <|> fail "expecting TyChkExpr"

instance B.Binary a MTab => B.HasPrefixTable (TyChkExpr a Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "TyChkExpr" 0xC5 0xC7 $
    [ NotTypeChecked <$> B.get
    , return TypeChecked  <*> B.get <*> B.get <*> B.get
    , return DisableCheck <*> B.get <*> B.get <*> B.get <*> B.get
    ]

instance (Eq a, Ord a, Typeable a, ObjectClass a) =>
  ObjectClass (TyChkExpr Object a) where { obj=new; fromObj=objFromHata; }

instance (Eq a, Ord a, Typeable a, ObjectClass a) =>
  HataClass (TyChkExpr Object a) where
    haskellDataInterface = interface "TypedExec" $ do
      autoDefEquality >> autoDefOrdering

----------------------------------------------------------------------------------------------------

instance ObjectClass a => ToDaoStructClass (AST_TyChk a Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_NotChecked o              -> renameConstructor "UntypedExpression" $ "expr" .= obj o
    AST_Checked    o coms typ loc -> renameConstructor "TypedExpression" $ do
      "expr"     .= obj o
      "colon"    .= coms
      "typeExpr" .= typ
      putLocation loc

instance (Typeable a, ObjectClass a) => FromDaoStructClass (AST_TyChk a Object) where
  fromDaoStruct = msum $
    [do constructor "UntypedExpression"
        AST_NotChecked <$> req "expr"
    ,do constructor "TypedExpression"
        return AST_Checked <*> req "expr" <*> req "colon" <*> req "typeExpr" <*> location
    ]

instance (Eq a, Ord a, PPrintable a, Typeable a, ObjectClass a) =>
  ObjectClass (AST_TyChk a Object) where { obj=new; fromObj=objFromHata; }

instance (Eq a, Ord a, PPrintable a, Typeable a) => HataClass (AST_TyChk a Object) where
  haskellDataInterface = interface "TypedExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0xCF 0xD0
instance B.Binary (ParamExpr Object) MTab where
  put (ParamExpr True  a b) = B.prefixByte 0xCF $ B.put a >> B.put b
  put (ParamExpr False a b) = B.prefixByte 0xD0 $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting ParamExpr"

instance B.HasPrefixTable (ParamExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParamExpr" 0xCF 0xD0 $
    [ return (ParamExpr True ) <*> B.get <*> B.get
    , return (ParamExpr False) <*> B.get <*> B.get
    ]

instance ObjectClass (ParamExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ParamExpr Object) where
  haskellDataInterface = interface "ParamExpr" $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Param Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_NoParams             -> makeNullary "NoParameters"
    AST_Param coms tychk loc -> renameConstructor "Parameter" $ do
      maybe (return ()) putComments coms
      "typeCheck" .= tychk
      putLocation loc

instance FromDaoStructClass (AST_Param Object) where
  fromDaoStruct = msum $
    [ nullary "NoParameters" >> return AST_NoParams
    , constructor "Parameter" >> return AST_Param <*> optComments <*> req "typeCheck" <*> location
    ]

instance ObjectClass (AST_Param Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Param Object) where
  haskellDataInterface = interface "ParameterExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0xD6 
instance B.Binary (ParamListExpr Object) MTab where
  put (ParamListExpr tyChk loc) = B.prefixByte 0xD6 $ B.put tyChk >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting ParamListExpr"

instance B.HasPrefixTable (ParamListExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParamListExpr" 0xD6 0xD6 $
    [return ParamListExpr <*> B.get <*> B.get]

instance ObjectClass (ParamListExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ParamListExpr Object) where
  haskellDataInterface = interface "ParameterList" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_ParamList Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_ParamList tychk loc -> renameConstructor "ParamList" $ do
      "typeCheck" .= tychk
      putLocation loc

instance FromDaoStructClass (AST_ParamList Object) where
  fromDaoStruct = constructor "ParamList" >> return AST_ParamList <*> req "typeCheck" <*> location

instance ObjectClass (AST_ParamList Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_ParamList Object) where
  haskellDataInterface = interface "ParameterListExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0x7A 0x7B
instance B.Binary (RuleHeadExpr Object) MTab where
  put o = case o of
    RuleStringExpr a b -> B.prefixByte 0x7A $ B.put a >> B.put b
    RuleHeadExpr   a b -> B.prefixByte 0x7B $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting RuleHeadExpr"

instance B.HasPrefixTable (RuleHeadExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RuleHeadExpr" 0x7A 0x7B
    [ return RuleStringExpr <*> B.get <*> B.get
    , return RuleHeadExpr   <*> B.get <*> B.get
    ]

instance Executable (RuleHeadExpr Object) [Object] where
  execute o = case o of
    RuleStringExpr r _ -> return [obj r]
    RuleHeadExpr   r _ -> forM r $
      execute . DerefAssignExpr >=> checkVoid (getLocation o) "item in rule header"

instance ObjectClass (RuleHeadExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (RuleHeadExpr Object) where
  haskellDataInterface = interface "RuleHeader" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_RuleHeader Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_NullRules coms loc -> renameConstructor "NoStrings" $ do
      putComments coms >> putLocation loc
    AST_RuleString itm loc -> renameConstructor "StringItem" $ do
      "items" .= itm >> putLocation loc
    AST_RuleHeader lst loc -> renameConstructor "ValuesList" $ do
      "items" .= lst >> putLocation loc

instance FromDaoStructClass (AST_RuleHeader Object) where
  fromDaoStruct = msum $
    [ constructor "NoStrings"  >> return AST_NullRules  <*> comments <*> location
    , constructor "StringItem" >> return AST_RuleString <*> req "items" <*> location
    , constructor "ValuesList" >> return AST_RuleHeader <*> reqList "items" <*> location
    ]

instance ObjectClass (AST_RuleHeader Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_RuleHeader Object) where
  haskellDataInterface = interface "RuleHeaderExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- $Exec_helpers
-- Functions for working with object values when building built-in functions callable from within a
-- dao script.

asReference :: Object -> XPure Reference
asReference = xmaybe . fromObj

_getRuleSetParam :: [Object] -> Exec (Maybe RuleSet, [Object])
_getRuleSetParam ox = return $ case ox of
  []   -> (Nothing, [])
  o:ox -> maybe ((Nothing, o:ox)) (\o -> (Just o, ox)) (fromObj o)

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
  let t = coreType o
  guard (CharType <= t && t <= ComplexType)
  return t

eval_Prefix_op :: ArithPfxOp -> Object -> XPure Object
eval_Prefix_op op o = join $ xmaybe $
  fromObj o >>= \ (Hata ifc o) -> objArithPfxOpTable ifc >>= (! op) >>= \f -> return (f op o)

-- Pass the 'InfixOp' associated with the 'Prelude.Num' function so it can check whether the
-- 'defInfixOp' for that operator has been defined for objects of 'HaskellType'.
eval_Infix_op :: InfixOp -> Object -> Object -> XPure Object
eval_Infix_op op a b = join $ xmaybe $ onhask a b <|> (guard isCommut >> onhask b a) where
  isCommut = infixOpCommutativity op
  onhask a b = fromObj a >>= \ (Hata ifc a) ->
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
  let t = max (coreType a) (coreType b)
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
      ORef _                            ->
        execThrow "dictionary key must be a single unqualified name" ExecErrorUntyped [(assertFailed, b)]
      _ -> mzero
    a         -> _evalNumOp2 (-) a b <|> eval_Infix_op SUB a b
  negate a = (a >>= _evalNumOp1 negate) <|> (a >>= eval_Prefix_op NEGTIV)
  abs    a = (a >>= _evalNumOp1 abs   ) <|> (a >>= eval_Prefix_op NEGTIV)
  signum a =  a >>= _evalNumOp1 signum
  fromInteger = return . obj

_xpureApplyError :: String -> String -> a
_xpureApplyError name msg = error $ concat $ concat $
  [["cannot evaluate ", name], guard (not $ null msg) >> [": ", msg]]

_xpureApply2 :: String -> (Object -> Object -> a) -> XPure Object -> XPure Object -> a
_xpureApply2 name f a b = case evalXPure ab of
  PFail   e -> _xpureApplyError name (prettyShow e)
  Backtrack -> _xpureApplyError name ""
  OK      o -> o
  where
    ab = do
      ta <- coreType <$> a
      tb <- coreType <$> b
      let t = max ta tb
      a <- a >>= castToCoreType t
      b <- b >>= castToCoreType t
      return (f a b)

_xpureApply1 :: String -> (Object -> XPure a) -> XPure Object -> a
_xpureApply1 name f o = case evalXPure $ o >>= f of
  PFail   e -> _xpureApplyError name (prettyShow e)
  Backtrack -> _xpureApplyError name ""
  OK      o -> o

_xpureMaybeApply1 :: (Object -> XPure a) -> XPure Object -> Maybe a
_xpureMaybeApply1 f o = case evalXPure $ o >>= f of
  PFail   _ -> Nothing
  Backtrack -> Nothing
  OK      o -> Just o

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
  toRational = _xpureApply1 "toRational" $ castToCoreType LongType >=> xmaybe . fromObj

eval_Int_op1 :: String -> (forall a . Integral a => a -> a) -> XPure Object -> XPure Object
eval_Int_op1 name f o = o >>= \o -> case o of
  OChar o -> return $ OChar (chr $ flip mod (ord maxBound) $ f $ ord o)
  OInt  o -> return $ OInt  (f o)
  OWord o -> return $ OWord (f o)
  OLong o -> return $ OLong (f o)
  _       -> throwBadTypeError "wrong data type for object passed to function" o $
                [(errInFunc, obj (ustr name :: Name))]

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
  fromEnum = _xpureApply1 "fromEnum" $ castToCoreType IntType >=> xmaybe . fromObj
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
        let t = max (coreType a) (coreType b)
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
        let t = max (coreType a) $ max (coreType b) $ (coreType c)
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
      let t = max (coreType a) (coreType b)
      guard (CharType <= t && t <= LongType) >> xpure t
    aa = _xpureCastTo typ a
    bb = _xpureCastTo typ b

instance Integral (XPure Object) where
  toInteger = _xpureApply1 "toInteger" (castToCoreType LongType >=> xmaybe . fromObj)
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
  let t = max (coreType a) (coreType b)
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
  (**)    a b = _xpureFrac2 (**) a b
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
_xpureRealFrac name f = _xpureApply1 name $ \o -> case o of
  ORelTime a -> xpure $ f a
  OFloat   a -> xpure $ f a
  ORatio   a -> xpure $ f a
  _          -> mzero

instance RealFrac (XPure Object) where
  properFraction = let name = "properFraction" in _xpureApply1 name $ \o -> case o of
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
  let t = max (coreType a) (coreType b)
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
      _ -> throwBadTypeError "cannot operate on dissimilar struct types" (OTree b) $
              [(expectType, obj (typeOfObj (OTree a)))]
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
      LT -> reverse $ drop (negate i) $ reverse o
      GT -> drop i o
    _ -> _xpureBits (flip shift i) (flip bytesShift (fromIntegral i)) (xpure o)
  rotate  o i = _xpureBits (flip shift i) (flip bytesRotate (fromIntegral i)) o
  bit       i = xpure $ if i<64 then OWord (bit i) else OBytes (bytesBit (fromIntegral i))
  testBit o i = _xpureApply1 "testBit" testbit o where
    testbit o = case o of
      OChar  o -> xpure $ testBit (ord o) i
      OInt   o -> xpure $ testBit o i
      OWord  o -> xpure $ testBit o i
      OLong  o -> xpure $ testBit o i
      OBytes o -> xpure $ bytesTestBit o (fromIntegral i)
      _        -> mzero
  bitSize = _xpureApply1 "bitSize" $ \o -> case o of
    OInt   o -> xmaybe $ bitSizeMaybe o
    OWord  o -> xmaybe $ bitSizeMaybe o
    OBytes o -> xpure $ fromIntegral $ bytesBitSize o
    _        -> mzero
  bitSizeMaybe = _xpureMaybeApply1 $ \o -> case o of
    OInt   o -> xmaybe $ bitSizeMaybe o
    OWord  o -> xmaybe $ bitSizeMaybe o
    OBytes o -> xpure $ fromIntegral $ bytesBitSize o
    _        -> mzero
  isSigned = _xpureApply1 "isSigned" $ \o -> case o of
    OChar  _ -> xpure False
    OInt   _ -> xpure True
    OWord  _ -> xpure False
    OLong  _ -> xpure True
    OBytes _ -> xpure False
    _        -> mzero
  popCount = _xpureApply1 "popCount" $ \o -> case o of
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
typeMismatchError :: InfixOp -> Object -> Object -> XPure ig
typeMismatchError op a b = throwError $
  newError{ execErrorSubtype = ExecInfixOpError (typeOfObj a) op (typeOfObj b) }

eval_ADD :: Object -> Object -> XPure Object
eval_ADD a b = (xpure a + xpure b) <|> typeMismatchError ADD a b

eval_SUB :: Object -> Object -> XPure Object
eval_SUB a b = (xpure a - xpure b) <|> typeMismatchError SUB a b

eval_MULT :: Object -> Object -> XPure Object
eval_MULT a b = (xpure a * xpure b) <|> typeMismatchError MULT a b

eval_DIV :: Object -> Object -> XPure Object
eval_DIV a b = do
  let { xa = xpure a; xb = xpure b; }
  (div xa xb <|> xa/xb) <|> typeMismatchError DIV a b

eval_MOD :: Object -> Object -> XPure Object
eval_MOD a b = do
  let { xa = xpure a; xb = xpure b; }
  (mod xa xb) <|> typeMismatchError MOD a b

eval_POW :: Object -> Object -> XPure Object
eval_POW a b = do
  let { xa = xpure a; xb = xpure b; }
  xa^^xb <|> xa**xb <|> typeMismatchError POW a b

eval_ORB :: Object -> Object -> XPure Object
eval_ORB a b = do
  let { xa = xpure a; xb = xpure b; }
  (xa.|.xb) <|> typeMismatchError ORB a b

eval_ANDB :: Object -> Object -> XPure Object
eval_ANDB a b = do
  let { xa = xpure a; xb = xpure b; }
  (xa.&.xb) <|> typeMismatchError ANDB a b

eval_XORB :: Object -> Object -> XPure Object
eval_XORB a b = do
  let { xa = xpure a; xb = xpure b; }
  (xor xa xb) <|> typeMismatchError XORB a b

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
  OHaskell (Hata ifc d) -> case objNullTest ifc of
    Nothing   -> throwBadTypeError "cannot be used as a boolean value" o [(assertFailed, o)]
    Just test -> return (test d)
  o -> return $ not $ testNull o

-- | Traverse the entire object, returning a list of all 'OString' elements.
extractStringElems :: Object -> [UStr]
extractStringElems o = case o of
  OString  o   -> [o]
  OList    o   -> concatMap extractStringElems o
  _            -> []

-- | Useful for building 'DaoFunc' objects, checks every parameter in a list of 'Object's to be a
-- string, and throws an exception if one of the 'Object's is not a string.
requireAllStringArgs :: String -> [Object] -> Exec [UStr]
requireAllStringArgs msg ox = case mapM check (zip [1..] ox) of
  OK      obj -> return obj
  Backtrack   -> fail msg
  PFail   err -> throwError err
  where
    check (i, o) = case o of
      OString o -> return o
      _         -> throwBadTypeError msg o [(argNum, OInt i)]

----------------------------------------------------------------------------------------------------

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

instance ToDaoStructClass UpdateOp where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass UpdateOp where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass UpdateOp where { obj=new; fromObj=objFromHata; }

instance HataClass UpdateOp where
  haskellDataInterface = interface "UpdateOperator" $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass RefPfxOp where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass RefPfxOp where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass RefPfxOp where { obj=new; fromObj=objFromHata; }

instance HataClass RefPfxOp where
  haskellDataInterface = interface "ReferenceOperator" $ do
    autoDefEquality >> autoDefOrdering
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass ArithPfxOp where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass ArithPfxOp where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass ArithPfxOp where { obj=new; fromObj=objFromHata; }

instance HataClass ArithPfxOp where
  haskellDataInterface = interface "ArithmeticPrefixOperator" $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass InfixOp where { toDaoStruct=putNullaryUsingShow; }

instance FromDaoStructClass InfixOp where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass InfixOp where { obj=new; fromObj=objFromHata; }

instance HataClass InfixOp where
  haskellDataInterface = interface "ArithmeticInfixOperator" $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass TopLevelEventType where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass TopLevelEventType where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass TopLevelEventType where { obj=new; fromObj=objFromHata; }

instance HataClass TopLevelEventType where
  haskellDataInterface = interface "TopLevelEventType" $ do
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
  getOp (Hata ifc a) = (\f -> f op a b) <$> (objInfixOpTable ifc >>= (! op))

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

-- | Evaluate an 'UpdateOp' operator. Provide an optional 'Reference' indicating the reference
-- location of the value being updated, then the 'UpdateOp' operator, the right-hand side 'Object'
-- value, and finally the current value stored at the 'Reference' location that needs to be updated.
-- If the 'UpdateOp' is 'UCONST', the current value may be 'Prelude.Nothing' as any current value
-- will be overwritten. If the 'UpdateOp' is not 'UCONST' and the current value is
-- 'Prelude.Nothing', this is an error. Otherwise the current value is removed from the
-- 'Prelude.Just' constructor and used as the left-hand operand with the right-hand operand of the
-- appropriate arithmetic function associated with the 'UpdateOp'.
evalUpdateOp :: Maybe Reference -> UpdateOp -> Object -> Maybe Object -> Exec (Maybe Object)
evalUpdateOp qref op newObj oldObj = case op of
  UCONST -> return $ Just newObj
  op     -> case oldObj of
    Nothing     ->
      execThrow "performed update on void value" (ExecUpdateOpError op)
        (maybe [] (\qref -> [(errOfReference, obj qref)]) qref)
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

makePrintFunc :: (typ -> String -> Exec ()) -> DaoFunc typ
makePrintFunc print =
  daoFunc
  { funcAutoDerefParams = True
  , daoForeignFunc = \typ ox -> print typ (_strObjConcat ox) >> return (Nothing, typ)
  }

builtin_print :: DaoFunc ()
builtin_print   = makePrintFunc (\ () -> liftIO . (putStr >=> evaluate))

builtin_println :: DaoFunc ()
builtin_println = makePrintFunc (\ () -> liftIO . (putStrLn >=> evaluate))

-- join string elements of a container, pretty prints non-strings and joins those as well.
builtin_join :: DaoFunc ()
builtin_join =
  daoFunc
  { funcAutoDerefParams = True
  , daoForeignFunc = \ () ox -> return $ flip (,) () $ Just $ obj $ case ox of
      OString j : ox -> (>>=uchars) $
        intersperse j $ snd (objConcat ox) >>= \o ->
          [maybe (ustr $ prettyShow o) id (fromObj o :: Maybe UStr)]
      ox -> _strObjConcat ox
  }

builtin_str :: DaoFunc ()
builtin_str =
  daoFunc
  { funcAutoDerefParams = True
  , daoForeignFunc = \ () -> return . flip (,) () . Just . obj . _strObjConcat
  }

builtin_quote :: DaoFunc ()
builtin_quote =
  daoFunc
  { funcAutoDerefParams = True
  , daoForeignFunc = \ () -> return . flip (,) () . Just . obj . show . _strObjConcat
  }

builtin_concat :: DaoFunc ()
builtin_concat =
  daoFunc
  { funcAutoDerefParams = True
  , daoForeignFunc = \ () -> return . flip (,) () . Just . obj . fix (\loop ox -> ox >>= \o -> maybe [o] loop (fromObj o))
  }

builtin_concat1 :: DaoFunc ()
builtin_concat1 =
  daoFunc
  { funcAutoDerefParams = True
  , daoForeignFunc = \ () -> return . flip (,) () . Just . obj . snd . objConcat
  }

builtin_reverse :: DaoFunc ()
builtin_reverse =
  daoFunc
  { funcAutoDerefParams = True
  , daoForeignFunc = \ () -> return . flip (,) () . Just . obj . reverse . snd . objConcat
  }

_castNumerical :: String -> (Object -> Exec Object) -> DaoFunc ()
_castNumerical name f = let n = ustr name :: Name in
  daoFunc
  { funcAutoDerefParams = True
  , daoForeignFunc = \ () ox -> case ox of
      [o] -> (flip (,) () . Just <$> f o) <|> throwBadTypeError "cannot cast to numerical type" o []
      ox  -> throwArityError "" 1 ox [(errInFunc, obj $ reference UNQUAL n)]
  }

builtin_int :: DaoFunc ()
builtin_int = _castNumerical "int" $
  fmap OInt . (execute . castToCoreType IntType >=> xmaybe . fromObj)

builtin_long :: DaoFunc ()
builtin_long = _castNumerical "long" $
  fmap OLong . (execute . castToCoreType LongType >=> xmaybe . fromObj)

builtin_ratio :: DaoFunc ()
builtin_ratio = _castNumerical "ratio" $
  fmap ORatio . (execute . castToCoreType RatioType >=> xmaybe . fromObj)

builtin_float :: DaoFunc ()
builtin_float = _castNumerical "float" $
  fmap OFloat . (execute . castToCoreType FloatType >=> xmaybe . fromObj)

builtin_complex :: DaoFunc ()
builtin_complex = _castNumerical "complex" $
  fmap OComplex . (execute . castToCoreType ComplexType >=> xmaybe . fromObj)

builtin_imag :: DaoFunc ()
builtin_imag = _castNumerical "imag" $
  fmap (OFloat . imagPart) . (execute . castToCoreType ComplexType >=> xmaybe . fromObj)

builtin_phase :: DaoFunc ()
builtin_phase = _castNumerical "phase" $
  fmap (OFloat . phase) . (execute . castToCoreType ComplexType >=> xmaybe . fromObj)

builtin_conj :: DaoFunc ()
builtin_conj = _castNumerical "conj" $
  fmap (OComplex . conjugate) . (execute . castToCoreType ComplexType >=> xmaybe . fromObj)

builtin_abs :: DaoFunc ()
builtin_abs = _castNumerical "abs" $ execute . asPositive

builtin_time :: DaoFunc ()
builtin_time = _castNumerical "time" $ \o -> case o of
  ORelTime _ -> return o
  o          -> (ORelTime . fromRational) <$> execute (asRational o)

_funcWithoutParams :: String -> Exec (Maybe Object) -> DaoFunc ()
_funcWithoutParams name f =
  daoFunc
  { daoForeignFunc = \ () ox -> case ox of
      [] -> flip (,) () <$> f
      ox -> throwArityError "function takes no parameters" 0 ox $
        [(errInFunc, obj $ reference UNQUAL (ustr name))]
  }

builtin_now :: DaoFunc ()
builtin_now = _funcWithoutParams "now" $ (Just . obj) <$> liftIO getCurrentTime

builtin_ref :: DaoFunc ()
builtin_ref =
  daoFunc
  { daoForeignFunc = \ () -> fmap (flip (,) () . Just . ORef) . execute . mconcat .
      fmap (\o -> (castToCoreType RefType o) <|>
             (throwBadTypeError "could not convert to reference" o []) >>=
               (castToCoreType RefType >=> xmaybe . fromObj)
           )
  }

builtin_check_if_defined :: DaoFunc ()
builtin_check_if_defined =
  daoFunc
  { funcAutoDerefParams = False
  , daoForeignFunc = \ () args -> fmap (flip (,) () . Just . obj . and) $ forM args $ \arg -> case arg of
      ORef o -> catchError (referenceLookup o >> return True) $ \err -> case err of
        ExecError{ execErrorSubtype=ExecUndefinedRef _ } -> return False
        err                                              -> throwError err
      _      -> return True
  }

builtin_delete :: DaoFunc ()
builtin_delete =
  daoFunc
  { funcAutoDerefParams = False
  , daoForeignFunc = \ () args -> do
      forM_ args $ \arg -> case arg of
        ORef o -> void $ referenceUpdate o True (const $ return Nothing)
        _      -> return ()
      return (Nothing, ())
  }

builtin_typeof :: DaoFunc ()
builtin_typeof =
  daoFunc
  { daoForeignFunc = \ () ox -> return $ flip (,) () $ case ox of
      []  -> Nothing
      [o] -> Just $ OType $ typeOfObj o
      ox  -> Just $ OList $ map (OType . typeOfObj) ox
  }

builtin_sizeof :: DaoFunc ()
builtin_sizeof =
  daoFunc
  { daoForeignFunc = \ () ox -> case ox of
      [o] -> flip (,) () . Just <$> getSizeOf o
      ox  -> throwArityError "" 1 ox [(errInFunc, obj $ reference UNQUAL (ustr "sizeof"))]
  }

builtin_call :: DaoFunc ()
builtin_call =
  daoFunc
  { funcAutoDerefParams = False
  , daoForeignFunc = \ () ox -> case ox of
      [func, params] -> do
        let nonlist_err = fail "second parameter to \"call()\" function is not a list of arguments"
        params <- case params of
          OList params -> return params
          ORef  params -> referenceLookup params >>= \ (_, params) -> case params of
            Nothing     -> fail "second parameter parameter to \"call()\" function evaluated to null"
            Just params -> xmaybe (fromObj params) <|> nonlist_err
          _ -> nonlist_err
        qref <- xmaybe (fromObj func)
          <|> fail "first parameter to \"call()\" function is not a reference to a function"
        (qref, func) <- referenceLookup qref
        case func of
          Nothing   -> fail "first parameter to \"call()\" function evaluated to null"
          Just func -> fmap (const ()) <$> callObject qref func params
      _ -> fail $ unwords $
        [ "the \"call()\" function was evaluated with incorrect arguments."
        , "Expecting a reference to function as first parameter"
        , "and a list of arguments as the second parameter."
        ]
  }

builtin_toHash :: DaoFunc ()
builtin_toHash =
  daoFunc
  { daoForeignFunc = \ () ox -> do
      let qref = reference UNQUAL (ustr "toHash")
      let err  = throwArityError "" 1 ox [(errInFunc, obj qref)]
      case ox of
        [o] -> case o of
          OTree              _  -> return (Just o, ())
          OHaskell (Hata ifc d) -> case objToStruct ifc of
            Just to -> flip (,) () . Just . OTree <$> toDaoStructExec to d
            Nothing -> throwBadTypeError "data type is opaque, cannot do binary conversion for hash" o $
              [(errInFunc, obj qref)]
          _                     -> err
        _   -> err
  }

builtin_fromHash :: DaoFunc ()
builtin_fromHash =
  daoFunc
  { daoForeignFunc = \ () ox -> do
      let qref = reference UNQUAL (ustr "fromHash")
      case ox of
        [o] -> do
          let err = throwBadTypeError "hashed Struct parameter required" o [(errInFunc, obj qref)]
          xmaybe (fromObj o) <|> err >>= fmap (flip (,) () . Just . obj) . fromDaoStructExec 
        ox  -> throwArityError "" 1 ox [(errInFunc, obj qref)]
  }

builtin_tokenize :: DaoFunc ()
builtin_tokenize =
  daoFunc{ daoForeignFunc = \ () -> fmap (flip (,) () . Just . obj . map obj) . runTokenizer }

----------------------------------------------------------------------------------------------------

-- binary 0x42 0x45 RefSuffixExpr-->RefSuffix
instance B.Binary (RefSuffixExpr Object) MTab where
  put o = case o of
    NullRefExpr         -> B.putWord8   0x42
    DotRefExpr    a b c -> B.prefixByte 0x43 $ B.put a >> B.put b >> B.put c
    SubscriptExpr a b   -> B.prefixByte 0x44 $ B.put a >> B.put b
    FuncCallExpr  a b   -> B.prefixByte 0x45 $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting RefSuffixExpr"

instance B.HasPrefixTable (RefSuffixExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RefSuffixExpr" 0x42 0x45 $
    [ return NullRefExpr
    , return DotRefExpr    <*> B.get <*> B.get <*> B.get
    , return SubscriptExpr <*> B.get <*> B.get
    , return FuncCallExpr  <*> B.get <*> B.get
    ]

instance Executable (RefSuffixExpr Object) RefSuffix where
  execute o = errLocation o $ case o of
    NullRefExpr              -> return NullRef
    DotRefExpr    name ref _ -> DotRef name <$> execute ref
    SubscriptExpr args ref   -> return Subscript <*> execute args <*> execute ref
    FuncCallExpr  args ref   -> return FuncCall  <*> execute args <*> execute ref

----------------------------------------------------------------------------------------------------

-- | To evaluate an 'Object' value against a type expression, you can store the
-- 'Object' into a 'TyChkExpr' and 'execute' it. This instance of
-- 'execute' evaluates a type checking monad computing over the 'tyChkExpr' in
-- the 'TyChkExpr'. If the type check determines the 'Object' value does not match, this
-- function backtracks. If the type check is successful, the most general type value for the object
-- if that type value is less-general or as-general as the 'TyChkExpr' provided.
instance Executable (TyChkExpr Object Object) Object where
  execute tc = case tc of
    NotTypeChecked _          -> return OTrue -- TODO: this needs to return the 'AnyType', not 'OTrue'.
    TypeChecked    _ _ _      -> return OTrue -- TODO: evaluate the actual type checking algorithm here
    DisableCheck   _ _ rslt _ -> return rslt

-- | Convert an 'ObjectExpr' to an 'Dao.Glob.Glob'.
paramsToGlobExpr :: ObjectExpr Object -> Exec (Glob UStr)
paramsToGlobExpr o = case o of
  ObjLiteralExpr (LiteralExpr (OString str) _) -> return (read (uchars str))
  _ -> fail "does not evaluate to a \"glob\" pattern"

-- | Called by 'callFunction' to match the list of 'Object's passed as arguments to the function.
-- Returns two 'T_dict's: the first is the 'T_dict' to be passed to 'execFuncPushStack', the second
-- is the dictionary of local variables passed by reference. Backtracks if any types do not match,
-- or if there are an incorrect number of parameters. Backtracking is important because of function
-- overloading.
matchFuncParams :: ParamListExpr Object -> [Object] -> Exec T_dict
matchFuncParams (ParamListExpr params _) ox = loop (0::Int) M.empty (tyChkItem params) ox where
  loop i dict params ox = case ox of
    [] | null params -> return dict
    [] -> mzero -- not enough parameters passed to function
    o:ox -> case params of
      [] -> mzero -- too many parameters passed to function
      ParamExpr passByRef tychk _ : params -> do
        let name = tyChkItem tychk
        execute $ fmapCheckedValueExpr (const o) tychk -- execute (TyChkExpr Object)
        o <- if passByRef then (case o of { ORef _ -> return o; _ -> mzero }) else derefObject o
        loop (i+1) (M.insert name o dict) params ox

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code found in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [ScriptExpr Object] -> Exec ()
execGuardBlock block = void $
  execFuncPushStack M.empty (mapM_ execute block >> return Nothing) >> return ()

-- | Takes two parameters: first is an error message parameter, the second is the 'Object' to be
-- called. The 'Object' to be called should be an 'OHaskell' constructed value containing a
-- 'Hata' where the 'interface' has defined 'defCallable'. If so, the 'CallableCode' objects
-- returned by 'objCallable' will be returned by this function. If not, 
objToCallable :: Object -> Exec [CallableCode]
objToCallable o = case fromObj o >>= \ (Hata ifc o) -> fmap ($ o) (objCallable ifc) of
  Nothing -> mzero
  Just  f -> f

-- | 'CallableCode' objects are usually stored in lists because of function overloading: e.g. a
-- function with a single name but is defined with multiple parameter lists would have several
-- 'CallableCode' objects associated with that function mame. This function tries to perform a
-- function call with a list of parameters. The parameters are matched to each 'CallableCode'
-- object's 'argsPattern', the first 'argsPattern' that matches without backtracking will evaluate
-- the function body. The value returned is a pair containing the result of the function call as the
-- 'Prelude.fst', and update "this" value as 'Prelude.snd'
callCallables :: Maybe Object -> [CallableCode] -> [Object] -> Exec (Maybe Object, Maybe Object)
callCallables this funcs params = fmap (fmap (M.lookup (ustr "this"))) $ join $
  msum $ flip fmap funcs $ \call -> matchFuncParams (argsPattern call) params >>=
    return . flip execFuncPushStack (execute $ codeSubroutine call) . M.alter (const this) (ustr "this")

-- | This function assumes you have retrieved a callable function-like 'Object' using a 'Reference'.
-- This function evaluates 'callCallables', and extracts the 'CallableCode' from the 'Object'
-- provided as the second parameter using 'objToCallable'. The 'Reference' passed as the first
-- parameter should be the reference used to retrieve the function 'Object'. If the given object
-- provides a 'defCallable' callback, the object can be called with parameters as if it were a
-- function.
callObject :: Reference -> Object -> [Object] -> Exec (Maybe Object, Maybe Object)
callObject qref o params = case o of
  OHaskell (Hata ifc d) -> case fromDynamic d of
    Just func -> fmap (const Nothing) <$> executeDaoFunc func () params -- try calling an ordinary function
    Nothing   -> case objCallable ifc of
      Just getFuncs -> getFuncs d >>= \func -> callCallables (Just o) func params
      Nothing       -> err
  _ -> err
  where { err = throwBadTypeError "not a callable object" o [(errInFunc, obj qref)] }

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
  Nothing -> throwError $
    newError
    { execErrorMessage  = ustr (msg++" evaluated to void")
    , execErrorLocation = loc
    }
  Just  a -> return a

instance ToDaoStructClass (AST_RefSuffix Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_RefNull                 -> makeNullary "Null"
    AST_DotRef dot name ref loc -> renameConstructor "DotRef" $ do
      "dot" .= dot >> "head" .= name >> "tail" .= ref >> putLocation loc
    AST_Subscript  args ref -> renameConstructor "Subscript" $ do
      "args" .= args >> "tail" .= ref >> return ()
    AST_FuncCall   args ref -> renameConstructor "FuncCall" $ do
      "args" .= args >> "tail" .= ref >> return ()

instance FromDaoStructClass (AST_RefSuffix Object) where
  fromDaoStruct = msum $
    [ constructor "Null"   >> return AST_RefNull
    , constructor "DotRef" >> return AST_DotRef <*> req "dot" <*> req "head" <*> req "tail" <*> location
    , constructor "Subscript" >> return AST_Subscript <*> req "args" <*> req "tail"
    , constructor "FuncCall"  >> return AST_FuncCall  <*> req "args" <*> req "tail"
    ]

instance ObjectClass (AST_RefSuffix Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_RefSuffix Object) where
  haskellDataInterface = interface "RefSuffixExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | If an expression is inside of a 'ParenExpr', like it usually is after being parsed as part of
-- an @if@ or @while@ statement, this function evaluates the expression to a 'Prelude.Bool' value
-- used to determine if the conditional expression should be 'execute'd.
evalConditional :: ParenExpr Object -> Exec Bool
evalConditional o =
  (execute o :: Exec (Maybe Object)) >>=
    checkVoid (getLocation o) "conditional expression to if statement" >>= derefObject >>=
      execHandleIO [fmap (const False) execIOHandler] . return . not . testNull

-- binary 0x59 
instance B.Binary (ParenExpr Object) MTab where
  put (ParenExpr a b) = B.prefixByte 0x59 $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting ParenExpr"

instance B.HasPrefixTable (ParenExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParenExpr" 0x59 0x59 $
    [return ParenExpr <*> B.get <*> B.get]

instance Executable (ParenExpr Object) (Maybe Object) where { execute (ParenExpr a _) = execute a }

instance ObjectClass (ParenExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ParenExpr Object) where
  haskellDataInterface = interface "Parentheses" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Paren Object) where
  toDaoStruct = renameConstructor "Paren" $ ask >>= \o -> case o of
    AST_Paren paren loc -> "inside" .= paren >> putLocation loc

instance FromDaoStructClass (AST_Paren Object) where
  fromDaoStruct = constructor "Paren" >> return AST_Paren <*> req "inside" <*> location

instance ObjectClass (AST_Paren Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Paren Object) where
  haskellDataInterface = interface "ParenthesesExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance B.Binary (IfExpr Object) MTab where
  put (IfExpr a b c) = B.put a >> B.put b >> B.put c
  get = return IfExpr <*> B.get <*> B.get <*> B.get

instance Executable (IfExpr Object) Bool where
  execute (IfExpr ifn thn _) = execNested_ M.empty $
    evalConditional ifn >>= \test -> when test (execute thn) >> return test

instance ObjectClass (IfExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (IfExpr Object) where
  haskellDataInterface = interface "Conditional" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_If Object) where
  toDaoStruct = renameConstructor "Conditional" $ ask >>= \o -> case o of
    AST_If ifn thn loc -> "condition" .= ifn >> "action" .= thn >> putLocation loc

instance FromDaoStructClass (AST_If Object) where
  fromDaoStruct = constructor "Conditional" >>
    return AST_If <*> req "condition" <*> req "action" <*> location

instance ObjectClass (AST_If Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_If Object) where
  haskellDataInterface = interface "ConditionalExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0xB6 
instance B.Binary (ElseExpr Object) MTab where
  put (ElseExpr a b) = B.prefixByte 0xB6 $ B.put a >> B.put b
  get = (B.tryWord8 0xB6 $ return ElseExpr <*> B.get <*> B.get) <|> fail "expecting ElseExpr"

instance Executable (ElseExpr Object) Bool where { execute (ElseExpr ifn _) = execute ifn }

instance ObjectClass (ElseExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ElseExpr Object) where
  haskellDataInterface = interface "Else" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Else Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_Else coms ifn loc -> renameConstructor "ElseIf" $ do
      "comments" .= coms >> "elseIf" .= ifn >> putLocation loc

instance FromDaoStructClass (AST_Else Object) where
  fromDaoStruct = constructor "ElseIf" >>
    return AST_Else <*> req "comments" <*> req "elseIf" <*> location

instance ObjectClass (AST_Else Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Else Object) where
  haskellDataInterface = interface "ElseExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0xBA 
instance B.Binary (IfElseExpr Object) MTab where
  put (IfElseExpr a b c d) = B.prefixByte 0xBA $ B.put a >> B.put b >> B.put c >> B.put d
  get = B.word8PrefixTable <|> fail "expecting IfElseExpr"

instance B.HasPrefixTable (IfElseExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "IfElseExpr" 0xBA 0xBA $
    [return IfElseExpr <*> B.get <*> B.get <*> B.get <*> B.get]

instance Executable (IfElseExpr Object) () where
  execute (IfElseExpr ifn elsx final _loc) = do
    let tryEach elsx = case elsx of
          []       -> return False
          els:elsx -> execute els >>= \ok -> if ok then return ok else tryEach elsx
    (execute ifn >>= \ok ->
      if ok then return Nothing
            else tryEach elsx >>= \ok ->
                 if ok then return Nothing
                       else return final) >>= maybe (return ()) execute

instance ObjectClass (IfElseExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (IfElseExpr Object) where
  haskellDataInterface = interface "IfElse" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_IfElse Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_IfElse ifn els block loc -> renameConstructor "If" $ do
      "test" .= ifn >> "alt" .= listToObj els
      maybe (return ()) (void . defObjField "finalElse") block
      putLocation loc

instance FromDaoStructClass (AST_IfElse Object) where
  fromDaoStruct = constructor "If" >>
    return AST_IfElse
      <*> req "test"
      <*> reqList "alt"
      <*> opt "finalElse"
      <*> location

instance ObjectClass (AST_IfElse Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_IfElse Object) where
  haskellDataInterface = interface "IfElseExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary (not prefixed, always occurs within a list)
instance B.Binary (LastElseExpr Object) MTab where
  put (LastElseExpr a loc) = B.put a >> B.put loc
  get = (return LastElseExpr <*> B.get <*> B.get) <|> fail "expecting LastElseExpr"

instance Executable (LastElseExpr Object) () where { execute (LastElseExpr code _) = execute code }

instance ObjectClass (LastElseExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (LastElseExpr Object) where
  haskellDataInterface = interface "FinalElse" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_LastElse Object) where
  toDaoStruct = renameConstructor "LastElse" $ do
    (AST_LastElse coms code loc) <- ask
    "comments" .= coms >> "action" .= code >> putLocation loc

instance FromDaoStructClass (AST_LastElse Object) where
  fromDaoStruct = constructor "LastElse" >>
    return AST_LastElse <*> req "comments" <*> req "action" <*> location

instance ObjectClass (AST_LastElse Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_LastElse Object) where
  haskellDataInterface = interface "FinalElseExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary (not prefixed, always occurs within a list)
instance B.Binary (CatchExpr Object) MTab where
  put (CatchExpr a b loc) = B.put a >> B.put b >> B.put loc
  get = return CatchExpr <*> B.get <*> B.get <*> B.get

instance ObjectClass (CatchExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (CatchExpr Object) where
  haskellDataInterface = interface "CatchExpr" $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt

-- | Returns the 'Exec' function to be evaluated if the 'ExecControl' matches the 'CatchExpr' type
-- constraint. If the type constraint does not match, this function evaluates to
-- 'Control.Monad.mzero'.
executeCatchExpr :: ExecControl -> CatchExpr Object -> Exec (Exec ())
executeCatchExpr err (CatchExpr (ParamExpr _refd param _) catch _loc) = case param of -- TODO: do something with _refd
  NotTypeChecked name             -> ex name catch
  TypeChecked    name _check _loc -> ex name catch -- TODO: do something with _check
  DisableCheck   name _  _   _    -> ex name catch
  where
    ex name catch = return $ execNested_ M.empty $ localVarDefine name (new err) >> execute catch

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Catch Object) where
  toDaoStruct = ask >>= \ (AST_Catch coms param action loc) -> renameConstructor "Catch" $ do
    "comments" .= coms >> "test" .= param >> "action" .= action >> putLocation loc

instance FromDaoStructClass (AST_Catch Object) where
  fromDaoStruct = constructor "Catch" >>
    return AST_Catch <*> req "comments" <*> req "test" <*> req "action" <*> location

instance ObjectClass (AST_Catch Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Catch Object) where
  haskellDataInterface = interface "CatchExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0x85 
instance B.Binary (WhileExpr Object) MTab where
  put (WhileExpr o) = B.prefixByte 0x85 $ B.put o
  get = B.word8PrefixTable <|> fail "expecting WhileExpr"

instance B.HasPrefixTable (WhileExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "WhileExpr" 0x85 0x85 [WhileExpr <$> B.get]

instance Executable (WhileExpr Object) () where
  execute (WhileExpr ifn) = let loop = execute ifn >>= flip when loop in loop

instance ObjectClass (WhileExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (WhileExpr Object) where
  haskellDataInterface = interface "While" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_While Object) where
  toDaoStruct = renameConstructor "While" $ ask >>= \ (AST_While o) -> innerToStruct o

instance FromDaoStructClass (AST_While Object) where
  fromDaoStruct = constructor "While" >> AST_While <$> innerFromStruct "Conditional"

instance ObjectClass (AST_While Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_While Object) where
  haskellDataInterface = interface "WhileExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0xA8 0xAF
instance B.Binary (ScriptExpr Object) MTab where
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

instance B.HasPrefixTable (ScriptExpr Object) B.Byte MTab where
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
instance Executable (ScriptExpr Object) () where
  execute script = errCurrentModule $ errLocation script $ case script of
    IfThenElse   ifn    -> execute ifn
    WhileLoop    ifn    -> execute ifn
    EvalObject   o _loc -> execute (DerefAssignExpr o) >>= return . maybe () (`seq` ())
    RuleFuncExpr rulfn  -> do
      o <- execute rulfn -- this 'execute' handles function expressions
      let dyn o = case o of
            OHaskell (Hata _ h) -> fromDynamic h
            _ -> Nothing
      let getObj :: (ObjectClass o, Typeable o) => Exec o
          getObj = mplus (xmaybe $ o >>= dyn) $ case o of
            Nothing -> fail "RuleFuncExpr evaluated to void"
            Just  o -> throwBadTypeError "RuleFuncExpr evaluated to object of incorrect data type" o []
      let fsub sub = modify $ \xunit -> xunit{currentCodeBlock=Just sub}
      sub <- gets currentCodeBlock
      case sub of
        Nothing  -> case rulfn of
          LambdaExpr{} -> getObj >>= \o ->
            modify $ \xunit -> xunit{ lambdaSet = lambdaSet xunit ++ o }
          RuleExpr{}   -> do
            newtree <- getObj >>= \p -> execute (p::PatternRule)
            modify $ \xunit -> xunit{ ruleSet=T.unionWith (++) (ruleSet xunit) newtree }
          FuncExpr{}   -> return ()
            -- function expressions are placed in the correct store by the above 'execute'
        Just sub -> case rulfn of
          LambdaExpr{} -> getObj >>= \o -> fsub $ sub{ staticLambdas = staticLambdas sub ++ o }
          RuleExpr{}   -> do
            newtree <- getObj >>= \p -> execute (p::PatternRule)
            fsub $ sub{ staticRules=T.unionWith (++) (staticRules sub) newtree }
          FuncExpr{}   -> return ()
            -- function expressions are placed in the correct store by the above 'execute'
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    TryCatch try els catchers _loc -> do
      ce <- catchPredicate $ execNested_ M.empty (execute try) <|> msum (fmap execute els)
      case ce of
        OK               ()  -> return ()
        Backtrack            -> mzero
        PFail (ExecReturn{}) -> predicate ce
        PFail           err  -> join $ msum $ fmap (executeCatchExpr err) catchers
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ForLoop varName inObj thn _loc -> do
      let run     = void $ execute thn
      let readIter   o = execNested_ (M.singleton varName o) run
      let updateIter o = M.lookup varName . snd <$> execNested (maybe M.empty (M.singleton varName) o) run
      let readLoop   o = void $ readForLoop o readIter
      let updateLoop qref o = void $ fmap snd $
            updateForLoop o updateIter >>= referenceUpdate qref False . const . return . Just
      errLocation inObj $ execute inObj >>= maybeDerefObject >>= \ (qref, iter) -> case iter of
        Nothing   -> fail "iterator of for-loop expression evaluated to void"
        Just iter -> case qref of
          Nothing   -> void $ readLoop iter
          Just qref -> case iter of
            OHaskell (Hata ifc _) -> case objUpdateIterable ifc of
              Just  _ -> void $ updateLoop qref iter
              Nothing -> case objReadIterable ifc of
                Just  _ -> void $ readLoop iter
                Nothing -> throwBadTypeError "data type not iterable" iter [(errOfReference, obj qref)]
            _                     -> void $ updateLoop qref iter
      -- NOTE: the for loop iterator IS NOT passed to the 'referenceUpdate' function to be
      -- evaluated. This is to avoid introducing deadlocks in data types that may store their values
      -- in an MVar. The 'referenceLookup' function first evaluates the reference, creating a copy
      -- in the current thread, the for loop is evaluated, and THEN the 'referenceUpdate' function
      -- is evaluated, in those three discrete steps.  It is not possible to evaluate a for loop in
      -- the dao programming language as an atomic action, so race conditions may occur when
      -- updating MVars -- but deadlocks will not occur.
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ContinueExpr a    _      _loc -> fail $
      '"':(if a then "continue" else "break")++"\" expression is not within a \"for\" or \"while\" loop"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ReturnExpr returnStmt o _loc -> do
      o <- (execute o :: Exec (Maybe Object)) >>= maybe (return Nothing) (fmap Just . derefObject)
      if returnStmt then throwError (ExecReturn o) else maybe mzero (flip (execThrow "") [] . ExecThrow) o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    WithDoc   expr   thn    _loc -> execute expr >>=
      checkVoid (getLocation expr) "target of \"with\" statement" >>=
        flip execWithWithRefStore (execute thn)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass (ScriptExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ScriptExpr Object) where
  haskellDataInterface = interface "Script" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

localVarDefine :: Name -> Object -> Exec (Maybe Object)
localVarDefine name o = fmap (snd . fst) $
  runObjectFocus (updateLocal name NullRef $ modify (const $ Just o) >> get) False (reference UNQUAL name) ()

localVarUpdate :: Name -> (Maybe Object -> Maybe Object) -> Exec (Maybe Object)
localVarUpdate name f = fmap (snd . fst) $
  runObjectFocus (updateLocal name NullRef $ modify f >> get) False (reference UNQUAL name) ()

localVarLookup :: Name -> Exec Object
localVarLookup name = 
  (snd . fst) <$> runObjectFocus (updateLocal name NullRef get) True (reference UNQUAL name) () >>=
    maybe mzero return

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
  Nothing -> case r of
    Nothing -> throwBadTypeError "dereferenced a non-reference value" o []
    Just  r -> execThrow "reference evaluated to void" r [(errOfReference, o)]
  Just derefd -> return (r, derefd)

-- | Tries to dereference an 'Object'. If the 'Object' is an 'ORef' constructed 'Reference', the
-- reference is de-referenced, which may evaluate to 'Prelude.Nothing'. The dereferenced value is
-- returned in the 'Prelude.snd' of the pair. If the given 'Object' is an 'ORef', regardless of the
-- dereferenced value, the 'Reference' is returned in the 'Prelude.fst' of the pair. If the given
-- 'Object' is not an 'ORef' constructed 'Object', it is returned unmodified along with
-- 'Prelude.Nothing' in the 'Prelude.fst' of the pair.
maybeDerefObject :: Maybe Object -> Exec (Maybe Reference, Maybe Object)
maybeDerefObject = maybe (return (Nothing, Nothing)) $ \o -> case o of
  ORef r -> referenceLookup r >>= \ (r, o) -> case o of
    Nothing -> return (Just r, Nothing)
    Just  o -> return (Just r, Just o)
  o      -> return (Nothing, Just o)

----------------------------------------------------------------------------------------------------

-- | This data type instantates the 'execute' function for use in for-loop expressions.
data ForLoopBlock = ForLoopBlock Name Object (CodeBlock Object)

instance Executable ForLoopBlock (Bool, Maybe Object) where
  execute (ForLoopBlock name o block) = 
    execNested_ (M.singleton name o) $ loop (codeBlock block) where
      done cont = do
        ref <- gets execStack
        newValue <- return $ M.lookup name $ head $ mapList ref
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

instance ToDaoStructClass (AST_Script Object) where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Comment      a           -> nm "Comment" $ putComments a
    AST_IfThenElse   a           -> innerToStruct a
    AST_WhileLoop    a           -> innerToStruct a
    AST_RuleFunc     a           -> innerToStruct a
    AST_EvalObject   a b     loc -> nm "ObjectExpr" $ "expr" .= a >> putComments b >> putLocation loc
    AST_TryCatch     a b c d loc -> nm "TryCatch" $ do
      "comments" .= a >> "tryBlock" .= b >> "elseBlocks" .= listToObj c
      "catchBlocks" .= listToObj d >> putLocation loc
    AST_ForLoop      a b c loc -> nm "ForLoop" $ do
      "varName" .= a >> "iterate" .= b >> "block" .= c >> putLocation loc
    AST_ContinueExpr a b c     loc -> nm (if a then "Continue" else "Break") $ do
      putComments b >> "condition" .= c >> putLocation loc
    AST_ReturnExpr   a b       loc -> nm (if a then "Return" else "Throw") $ do
      "expr" .= b >> putLocation loc
    AST_WithDoc      a b       loc -> nm "WithDoc" $ "expr" .= a >> "block" .= b >> putLocation loc

instance FromDaoStructClass (AST_Script Object) where
  fromDaoStruct = msum $
    [ constructor "Comment" >> AST_Comment <$> comments
    , AST_IfThenElse <$> fromDaoStruct
    , AST_WhileLoop  <$> fromDaoStruct
    , AST_RuleFunc   <$> fromDaoStruct
    , constructor "ObjectExpr" >> return AST_EvalObject <*> req "expr" <*> comments <*> location
    , constructor "TryCatch" >>
        return AST_TryCatch
          <*> comments
          <*> req "tryBlock"
          <*> reqList "elseBlocks"
          <*> reqList "catchBlocks"
          <*> location
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

instance ObjectClass [AST_Script Object] where { obj=listToObj; fromObj=listFromObj; }

instance ObjectClass (AST_Script Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Script Object) where
  haskellDataInterface = interface "ScriptExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0x86 
instance B.Binary (ObjListExpr Object) MTab where
  put (ObjListExpr lst loc) = B.prefixByte 0x86 $ B.putUnwrapped lst >> B.put loc
  get = (B.tryWord8 0x86 $ return ObjListExpr <*> B.getUnwrapped <*> B.get) <|> fail "expecting ObjListExpr"

instance Executable (ObjListExpr Object) [Object] where
  execute (ObjListExpr exprs _) = forM (zip exprs [1..]) $ \ (a, i) -> execute a >>= maybe (err i) return where
    err i = execThrow "item in list literal expression evaluates to void" ExecErrorUntyped [(argNum, obj (i::Int))]

instance PPrintable (ObjListExpr Object) where { pPrint = pPrintInterm }

instance ObjectClass (ObjListExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ObjListExpr Object) where
  haskellDataInterface = interface "ListLiteral" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_ObjList Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_ObjList coms lst loc -> renameConstructor "ListLiteralExpression" $ do
      putComments coms >> defObjField "items" (listToObj lst) >> putLocation loc

instance FromDaoStructClass (AST_ObjList Object) where
  fromDaoStruct = constructor "ListLiteralExpression" >>
    return AST_ObjList <*> comments <*> reqList "items" <*> location

instance ObjectClass (AST_ObjList Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_ObjList Object) where
  haskellDataInterface = interface "ListLiteralExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance B.Binary (OptObjListExpr Object) MTab where
  put (OptObjListExpr o) = B.put o
  get = OptObjListExpr <$> B.get

instance Executable (OptObjListExpr Object) [Object] where
  execute (OptObjListExpr lst) = maybe (return []) execute lst

instance ObjectClass (OptObjListExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (OptObjListExpr Object) where
  haskellDataInterface = interface "OptionalListLiteral" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

-- | Evaluate an 'Exec', but if it throws an exception, set record an 'ObjectExpr' where
-- the exception occurred in the exception information.
updateExecError :: (ExecControl -> ExecControl) -> Exec a -> Exec a
updateExecError upd fn = catchError fn (\err -> throwError (upd err))

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_OptObjList Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_OptObjList coms o -> renameConstructor "OptObjList" $ "params" .=? o >> putComments coms

instance FromDaoStructClass (AST_OptObjList Object) where
  fromDaoStruct = constructor "OptObjList" >> return AST_OptObjList <*> comments <*> opt "params"

instance ObjectClass (AST_OptObjList Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_OptObjList Object) where
  haskellDataInterface = interface "OptionalListLiteralExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance B.Binary (LiteralExpr Object) MTab where
  put (LiteralExpr a loc) = B.put a >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting LiteralExpr"

instance B.HasPrefixTable (LiteralExpr Object) B.Byte MTab where
  prefixTable = B.bindPrefixTable B.prefixTable $ \o -> LiteralExpr o <$> B.get

instance Executable (LiteralExpr Object) (Maybe Object) where { execute (LiteralExpr o _) = return (Just o) }

instance ObjectClass (LiteralExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (LiteralExpr Object) where
  haskellDataInterface = interface "Literal" $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> defDeref execute

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Literal Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_Literal o loc -> renameConstructor "Literal" $ "obj" .= o >> putLocation loc

instance FromDaoStructClass (AST_Literal Object) where
  fromDaoStruct = constructor "Literal" >> return AST_Literal <*> req "obj" <*> location

instance ObjectClass (AST_Literal Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Literal Object) where
  haskellDataInterface = interface "LiteralExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0x3C 0x42 ReferenceExpr-->Reference
instance B.Binary (ReferenceExpr Object) MTab where
  put qref = case qref of
    ReferenceExpr q n r loc -> prefix q $ B.put n >> B.put r >> B.put loc where
      prefix q = B.prefixByte $ case q of
        { UNQUAL -> 0x48; LOCAL -> 0x49; CONST -> 0x4A; STATIC -> 0x4B; GLOBAL -> 0x4C; GLODOT -> 0x4D; }
    RefObjectExpr o r loc -> B.putWord8 0x4E >> B.put o >> B.put r >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting Reference"

instance B.HasPrefixTable (ReferenceExpr Object) Word8 MTab where
  prefixTable = B.mkPrefixTableWord8 "ReferenceExpr" 0x48 0x4E $
    [ f UNQUAL, f LOCAL, f CONST, f STATIC, f GLOBAL, f GLODOT
    , return RefObjectExpr <*> B.get <*> B.get <*> B.get
    ] where { f q = return (ReferenceExpr q) <*> B.get <*> B.get <*> B.get }

instance Executable (ReferenceExpr Object) (Maybe Object) where
  execute qref = errLocation qref $ case qref of
    RefObjectExpr o NullRefExpr _ -> execute o
    RefObjectExpr o suf _ -> do
      o <- execute o >>=
        checkVoid (getLocation o) "function call on item in parentheses which evaluated to a void value"
      suf <- execute suf
      return $ Just $ obj $ RefObject o suf
    ReferenceExpr q ref suf _loc -> execute suf >>= return . Just . ORef . Reference q ref

instance ObjectClass (ReferenceExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ReferenceExpr Object) where
  haskellDataInterface = interface "ReferenceLiteral" $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref (execute >=> fmap snd . maybeDerefObject)

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Reference Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_RefObject o           ref loc -> renameConstructor "ParenExpr" $ do
      "paren" .= o >> "suffix" .= ref >> putLocation loc
    AST_Reference  q coms name ref loc -> renameConstructor "Reference" $ do
      "qualifier" .= q >> putComments coms >> "name" .= name >> "suffix" .= ref >> putLocation loc

instance FromDaoStructClass (AST_Reference Object) where
  fromDaoStruct = msum $
    [ constructor "ParenExpr" >>
        return AST_RefObject <*> req "paren" <*> req "suffix" <*> location
    , constructor "Reference" >>
        return AST_Reference <*> req "qualifier"
          <*> comments <*> req "name" <*> req "suffix" <*> location
    ]

instance ObjectClass (AST_Reference Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Reference Object) where
  haskellDataInterface = interface "ReferenceExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct
    defDeref (msum . map (execute >=> fmap snd . maybeDerefObject) . toInterm)

----------------------------------------------------------------------------------------------------

-- binary 0x52 0x53
instance B.Binary (RefPrefixExpr Object) MTab where
  put o = case o of
    PlainRefExpr  a     -> B.put a
    RefPrefixExpr a b z -> let f = B.put b >> B.put z in case a of
      REF   -> B.prefixByte 0x52 f
      DEREF -> B.prefixByte 0x53 f
  get = B.word8PrefixTable <|> fail "expecting RefPrefixExpr"

instance B.HasPrefixTable (RefPrefixExpr Object) B.Byte MTab where
  prefixTable = fmap PlainRefExpr B.prefixTable <>
    (B.mkPrefixTableWord8 "RefPrefixExpr" 0x52 0x53 $
      let f q = return (RefPrefixExpr q) <*> B.get <*> B.get in [f REF, f DEREF])

instance Executable (RefPrefixExpr Object) (Maybe Object) where
  execute ref = errLocation ref $ case ref of
    PlainRefExpr     ref     -> execute ref
    RefPrefixExpr op ref loc -> case op of
      REF   -> do
        ref <- execute ref >>= checkVoid loc "operand of referencing operator ($)"
        case ref of
          ORef ref -> return $ Just $ ORef $ RefWrapper ref
          ref      -> return $ Just $ ORef $ RefObject ref NullRef
      DEREF -> execute ref >>= checkVoid loc "operand of dereferencing operator (@)" >>=
        fmap Just . derefObject

instance ObjectClass (RefPrefixExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (RefPrefixExpr Object) where
  haskellDataInterface = interface "RefPrefixLiteral" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_RefPrefix Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_PlainRef  a         -> renameConstructor "PlainRef" $ "ref" .= a >> return ()
    AST_RefPrefix a b c loc -> renameConstructor "RefPrefix" $ do
      "op" .= a >> putComments b >> "expr" .= c >> putLocation loc

instance FromDaoStructClass (AST_RefPrefix Object) where
  fromDaoStruct = msum $
    [ constructor "RefPrefix" >>
        return AST_RefPrefix <*> req "op" <*> req "expr" <*> req "expr" <*> location
    , constructor "PlainRef" >> AST_PlainRef <$> req "ref"
    ]

instance ObjectClass (AST_RefPrefix Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_RefPrefix Object) where
  haskellDataInterface = interface "ReferencePrefixExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0x74 0x76
instance B.Binary (RuleFuncExpr Object) MTab where
  put o = case o of
    LambdaExpr a b   z -> B.prefixByte 0x74 $ B.put a >> B.put b >> B.put z
    FuncExpr   a b c z -> B.prefixByte 0x75 $ B.put a >> B.put b >> B.put c >> B.put z
    RuleExpr   a b   z -> B.prefixByte 0x76 $ B.put a >> B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting RuleFuncExpr"

instance B.HasPrefixTable (RuleFuncExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RuleFuncExpr" 0x74 0x76 $
    [ return LambdaExpr <*> B.get <*> B.get <*> B.get
    , return FuncExpr   <*> B.get <*> B.get <*> B.get <*> B.get
    , return RuleExpr   <*> B.get <*> B.get <*> B.get
    ]

instance Executable (RuleFuncExpr Object) (Maybe Object) where
  execute o = case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    LambdaExpr params script _ -> do
      let exec = setupCodeBlock script
      return $ Just $ new $
        [CallableCode{argsPattern=params, codeSubroutine=exec, returnType=nullValue}]
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    FuncExpr name params script _ -> do
      let exec = setupCodeBlock script
      let callableCode = CallableCode{argsPattern=params, codeSubroutine=exec, returnType=nullValue}
      localVarUpdate name $ \o -> case o>>=fromObj of
        Nothing -> Just $ obj [callableCode]
        Just cc -> Just $ obj $ cc++[callableCode]
      return (Just $ obj [callableCode])
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    RuleExpr rs script _ -> do
      let sub = setupCodeBlock script
      pats <- execute rs
      return $ Just $ obj $ PatternRule{ rulePatterns=pats, ruleAction=sub }
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass (RuleFuncExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (RuleFuncExpr Object) where
  haskellDataInterface = interface "FunctionLiteral" $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_RuleFunc Object) where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Lambda a b     loc -> nm "Lambda"   $ "params" .= a >> "block" .= b >> putLocation loc
    AST_Func   a b c d loc -> nm "Function" $
      putComments a >> "name"  .= b >> "params" .= c >> "block" .= d >> putLocation loc
    AST_Rule   a b     loc -> nm "Rule" $ "params" .= a >> "block" .= b >> putLocation loc

instance FromDaoStructClass (AST_RuleFunc Object) where
  fromDaoStruct = msum $
    [ constructor "Lambda" >> return AST_Lambda <*> req "params" <*> req "block"  <*> location
    , constructor "Function" >>
        return AST_Func <*> comments <*> req "name" <*> req "params" <*> req "block" <*> location
    , constructor "Rule" >> return AST_Rule <*> req "params" <*> req "block" <*> location
    ]

instance ObjectClass (AST_RuleFunc Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_RuleFunc Object) where
  haskellDataInterface = interface "FunctionLiteralExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0x60 0x65
instance B.Binary (ObjectExpr Object) MTab where
  put o = case o of
    ObjSingleExpr   a       -> B.put a
    ObjLiteralExpr  a       -> B.put a
    VoidExpr                -> B.putWord8   0x60
    ArithPfxExpr    a b   z -> B.prefixByte 0x61 $ B.put a >> B.put b >> B.put z
    InitExpr        a b c z -> B.prefixByte 0x62 $ B.put a >> B.put b >> B.put c >> B.put z
    StructExpr      a b   z -> B.prefixByte 0x63 $ B.put a >> B.put b >> B.put z
    MetaEvalExpr    a     z -> B.prefixByte 0x64 $ B.put a >> B.put z
  get = B.word8PrefixTable <|> fail "expecting ObjectExpr"

instance B.HasPrefixTable (ObjectExpr Object) B.Byte MTab where
  prefixTable = mconcat $
    [ ObjLiteralExpr  <$> B.prefixTable
    , ObjSingleExpr   <$> B.prefixTable
    , B.mkPrefixTableWord8 "ObjectExpr" 0x60 0x64 $
        [ return VoidExpr
        , return ArithPfxExpr <*> B.get <*> B.get <*> B.get
        , return InitExpr     <*> B.get <*> B.get <*> B.get <*> B.get
        , return StructExpr   <*> B.get <*> B.get <*> B.get
        , return MetaEvalExpr <*> B.get <*> B.get
        ]
    ]

instance Executable (ObjectExpr Object) (Maybe Object) where
  execute o = errLocation o $ case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    VoidExpr -> return Nothing
      -- 'VoidExpr's only occur in return statements. Returning 'ONull' where nothing exists is
      -- probably the most intuitive thing to do on an empty return statement.
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ObjLiteralExpr  o -> execute o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ObjSingleExpr   o -> execute o
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
      Just (ObjListExpr items _) -> execNested_ M.empty $ do
        forM_ items $ \item -> case item of 
          AssignExpr{} -> execute item -- fill the local stack by executing each assignment
          _            -> fail "struct initializer is not an assignment expression"
        stack <- gets execStack
        let items = head $ mapList stack
        return $ Just $ OTree $
          if M.null items
          then Nullary{ structName=name }
          else Struct{ fieldMap=items, structName=name }
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    MetaEvalExpr expr _ -> return $ Just $ new expr
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

_evalInit :: ReferenceExpr Object -> OptObjListExpr Object -> ObjListExpr Object -> Exec Object
_evalInit ref bnds initMap = do
  ref <- execute ref >>= checkVoid (getLocation ref) "initializer label"
  ref <- case ref of
    ORef (Reference UNQUAL name NullRef) -> pure name
    ref -> throwBadTypeError "cannot use reference as initalizer" ref []
  bnds <- execute bnds >>= mapM derefObject
  let cantUseBounds msg =
        execThrow ("initializer "++msg++" must be defined without bounding parameters")
          ExecErrorUntyped [(assertFailed, OList bnds)]
  let list = case bnds of
        [] -> execNested_ M.empty $ fmap OList $ execute initMap >>= mapM derefObject
        _  -> cantUseBounds "for list constructor"
  let (ObjListExpr items _) = initMap
  let dict = case bnds of
        [] -> (ODict . snd) <$> execNested M.empty (mapM_ assignUnqualifiedOnly items)
        _  -> cantUseBounds "for dict constructor"
  case uchars ref of
    "list"       -> list
    "List"       -> list
    "dict"       -> dict
    "Dict"       -> dict
    "Dictionary" -> dict
    _ -> do
      tab <- execGetObjTable ref
      let qref = Reference UNQUAL ref NullRef
      case tab of
        Nothing  -> execThrow "unknown object constructor" qref []
        Just tab -> execNested_ M.empty $ case objInitializer tab of
          Nothing           -> execThrow "cannot declare constant object of type" qref []
          Just (init, fold) -> do
            o     <- init bnds
            items <- forM items $ \item -> case item of
              AssignExpr a op b _ -> do
                a <- execute a >>= checkVoid (getLocation a) "left-hand side of initializer assignemt"
                b <- execute b >>= checkVoid (getLocation b) "right-hand side of initializer assignment" >>= derefObject
                return $ InitAssign a op b
              EvalExpr arith -> fmap InitSingle $
                execute arith >>= checkVoid (getLocation arith) "initializer item"
            OHaskell . Hata tab <$> fold o items

instance ObjectClass (ObjectExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ObjectExpr Object) where
  haskellDataInterface = interface "ObjectLiteral" $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Object Object) where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Void                   -> makeNullary "Void"
    AST_ObjLiteral   a         -> innerToStruct a
    AST_ObjSingle    a         -> innerToStruct a
    AST_ArithPfx     a b c loc -> nm "ArithPrefix" $ do
      "op" .= a >> putComments b >> "expr" .= c >> putLocation loc
    AST_Init         a b c loc -> nm "Init" $ do
      "name" .= a >> "params" .= b >> "initList" .= c >> putLocation loc
    AST_Struct       a b   loc -> nm "Struct" $ "name" .= a >> "initList" .= b >> putLocation loc
    AST_MetaEval     a     loc -> nm "MetaEval" $ "block" .= a >> putLocation loc

instance FromDaoStructClass (AST_Object Object) where
  fromDaoStruct = msum $
    [ nullary "Void" >> return AST_Void
    , AST_ObjLiteral  <$> fromDaoStruct
    , AST_ObjSingle   <$> fromDaoStruct
    , constructor "ArithPrefix" >>
        pure AST_ArithPfx <*> req "op" <*> comments <*> req "expr" <*> location
    , constructor "Init" >>
        pure AST_Init     <*> req "name" <*> req "params" <*> req "initList" <*> location
    , constructor "Struct" >> pure AST_Struct <*> req "name" <*> req "initList" <*> location
    , constructor "MetaEval" >> pure AST_MetaEval <*> req "block" <*> location
    ]

instance ObjectClass (AST_Object Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Object Object) where
  haskellDataInterface = interface "ObjectLiteralExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0x6A 
instance B.Binary (ArithExpr Object) MTab where
  put o = case o of
    ObjectExpr  a     -> B.put a
    ArithExpr a b c z -> B.prefixByte 0x6A $ B.put a >> B.put b >> B.put c >> B.put z
  get = B.word8PrefixTable <|> fail "expecting arithmetic expression"

instance B.HasPrefixTable (ArithExpr Object) B.Byte MTab where
  prefixTable = mappend (ObjectExpr <$> B.prefixTable) $
    B.mkPrefixTableWord8 "ArithExpr" 0x6A 0x6A $
      [pure ArithExpr <*> B.get <*> B.get <*> B.get <*> B.get]

instance Executable (ArithExpr Object) (Maybe Object) where
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

instance ObjectClass (ArithExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ArithExpr Object) where
  haskellDataInterface = interface "Arithmetic" $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Arith Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_Object a         -> innerToStruct a
    AST_Arith  a b c loc -> renameConstructor "Arithmetic" $ do
      "left" .= a >> "op" .= b >> "right" .= c >> putLocation loc

instance FromDaoStructClass (AST_Arith Object) where
  fromDaoStruct = msum $
    [ AST_Object <$> fromDaoStruct
    , constructor "Arithmetic" >>
        pure AST_Arith <*> req "left" <*> req "op" <*> req "right" <*> location
    ]

instance ObjectClass (AST_Arith Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Arith Object) where
  haskellDataInterface = interface "ArithmeticExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

newtype DerefAssignExpr = DerefAssignExpr (AssignExpr Object)
-- ^ This data type instantiates 'Executable' such that the result is always dereference once. This
-- is necessary because an 'AssignExpr' always evaluates to an 'Object' literal expression, meaning
-- if the 'AssignExpr' contains  'ReferenceExpr', it will evaluate to an 'Object' constructing a
-- literal 'Reference' (using the 'ORef' constructor). Sometimes this is desirable, sometimes it is
-- not. It is desirable when evaluating arguments for a function call that requests it's arguments
-- not be dereferenced. It is not desirable when evaluating an arithmetic equation and the integer
-- value stored at a 'Reference' variable is required, and not the 'Reference' value itself.
--     The calling context cannot know what the result will be, or whether or not it is necessary to
-- call 'derefObject' on the result unless the calling context inspects the 'AssignExpr' value with
-- a case statement. By wrapping the 'AssignExpr' in this data type first and then evaluating
-- 'execute', you are guranteed that any 'ReferenceExpr' will evaluate to the value stored at the
-- resulting 'Reference' literal, and not the 'Reference' literal itself.

instance Executable DerefAssignExpr (Maybe Object) where
  execute (DerefAssignExpr o) = case o of
    EvalExpr{}   -> execute o >>= fmap snd . maybeDerefObject
    AssignExpr{} -> execute o

_executeAssignExpr
  :: (Reference -> UpdateOp -> Object -> Exec (Maybe Object))
  -> AssignExpr Object -> Exec (Maybe Object)
_executeAssignExpr update o = case o of
  EvalExpr           expr     -> execute expr
  AssignExpr qref op expr loc -> do
    qref <- execute qref >>=
      checkVoid (getLocation qref) "left-hand side of assignment expression evaluated to void"
    case qref of
      ORef qref -> do
        newObj <- execute expr >>= checkVoid loc "right-hand side of assignment" >>= derefObject 
        update qref op newObj
      _    -> fail "left-hand side of assignment expression is not a reference value"

-- binary 0x6F 
instance B.Binary (AssignExpr Object) MTab where
  put o = case o of
    EvalExpr   a       -> B.put a
    AssignExpr a b c z -> B.prefixByte 0x6F $ B.put a >> B.put b >> B.put c >> B.put z
  get = B.word8PrefixTable <|> fail "expecting AssignExpr"

instance B.HasPrefixTable (AssignExpr Object) B.Byte MTab where
  prefixTable = mappend (EvalExpr <$> B.prefixTable) $
    B.mkPrefixTableWord8 "AssignExpr" 0x6F 0x6F $
      [pure AssignExpr <*> B.get <*> B.get <*> B.get <*> B.get]

instance Executable (AssignExpr Object) (Maybe Object) where
  execute = _executeAssignExpr $ \qref op newObj ->
    snd <$> referenceUpdate qref (op/=UCONST) (evalUpdateOp (Just qref) op newObj)

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
assignUnqualifiedOnly :: AssignExpr Object -> Exec (Maybe Object)
assignUnqualifiedOnly = _executeAssignExpr $ \qref op newObj -> case qref of
  Reference UNQUAL r NullRef -> do
    store <- gets execStack
    let oldObj = stackLookup r store
    newObj <- evalUpdateOp (Just qref) op newObj oldObj
    (result, store) <- pure $ stackUpdateTop (const (newObj, newObj)) r store
    modify $ \xunit -> xunit{ execStack = store }
    return result
  _ -> execThrow "assignment must be unqualified" ExecErrorUntyped [(errOfReference, obj qref)]

instance ObjectClass (AssignExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AssignExpr Object) where
  haskellDataInterface = interface "Assignment" $ do
    autoDefNullTest >> autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_Assign Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_Eval o ->  innerToStruct o
    AST_Assign to op from loc -> renameConstructor "Assign" $ do
      "to" .= to >> "op" .= op >> "from" .= from >> putLocation loc

instance FromDaoStructClass (AST_Assign Object) where
  fromDaoStruct = msum $
    [ AST_Eval <$> fromDaoStruct
    , pure AST_Assign <*> req "to" <*> req "op" <*> req "from" <*> location
    ]

instance ObjectClass (AST_Assign Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_Assign Object) where
  haskellDataInterface = interface "AssignmentExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance B.Binary (ObjTestExpr Object) MTab where
  put o = case o of
    ObjArithExpr      a -> B.put a
    ObjTestExpr a b c d -> B.prefixByte 0x73 $ B.put a >> B.put b >> B.put c >> B.put d
    ObjRuleFuncExpr a       -> B.put a
  get = B.word8PrefixTable <|> fail "expecting ObjTestExpr"

instance B.HasPrefixTable (ObjTestExpr Object) Word8 MTab where
  prefixTable = mconcat $ 
    [ ObjArithExpr <$> B.prefixTable
    , ObjRuleFuncExpr <$> B.prefixTable
    , B.mkPrefixTableWord8 "ObjTestExpr" 0x73 0x73 $
        [return ObjTestExpr <*> B.get <*> B.get <*> B.get <*> B.get]
    ]

instance Executable (ObjTestExpr Object) (Maybe Object) where
  execute o = errCurrentModule $ case o of
    ObjArithExpr      a -> execute a
    ObjTestExpr a b c _ ->
      execute a >>= checkVoid (getLocation a) "conditional expression evaluates to void" >>=
        execute . objToBool >>= \ok -> if ok then execute b else execute c
    ObjRuleFuncExpr o -> execute o

instance ObjectClass (ObjTestExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (ObjTestExpr Object) where
  haskellDataInterface = interface "ObjectTest" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass (AST_ObjTest Object) where
  toDaoStruct = ask >>= \o -> case o of
    AST_ObjArith  a -> innerToStruct a
    AST_ObjTest a b c d e f -> renameConstructor "ObjTest" $ do
      "condition" .= a
      "quesMarkComs" .= b >> "action" .= c
      "colonComs"    .= d >> "alt"    .= e
      putLocation f
    AST_ObjRuleFunc  a         -> innerToStruct a

instance FromDaoStructClass (AST_ObjTest Object) where
  fromDaoStruct = msum $
    [ AST_ObjArith <$> fromDaoStruct
    , do  constructor "ObjTest"
          return AST_ObjTest
            <*> req "condition" 
            <*> (maybe (Com ()) id <$> opt "quesMarkComs") <*> req "action"
            <*> (maybe (Com ()) id <$> opt "colonComs"   ) <*> req "alt"
            <*> location
    , AST_ObjRuleFunc <$> fromDaoStruct
    ]

instance ObjectClass (AST_ObjTest Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_ObjTest Object) where
  haskellDataInterface = interface "ObjectTestExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass AST_Namespace where
  toDaoStruct = ask >>= \a -> case a of
    AST_NoNamespace     -> makeNullary "NoNamespace"
    AST_Namespace n loc -> renameConstructor "Namespace" $ "name" .= n >> putLocation loc

instance FromDaoStructClass AST_Namespace where
  fromDaoStruct = msum $
    [ nullary "NoNamespace" >> return AST_NoNamespace
    , constructor "Namespace" >> return AST_Namespace <*> req "name" <*> location
    ]

instance ObjectClass AST_Namespace where { obj=new; fromObj=objFromHata; }

instance HataClass AST_Namespace where
  haskellDataInterface = interface "NamespaceExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- binary 0x81 0x82 -- placed next to with 'DotNameExpr'
instance B.Binary AttributeExpr MTab where
  put o = case o of
    AttribDotNameExpr a     -> B.put a
    AttribStringExpr  a loc -> B.prefixByte 0x82 $ B.put a >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting AttributeExpr"

instance B.HasPrefixTable AttributeExpr Word8 MTab where
  prefixTable = (AttribDotNameExpr <$> B.prefixTable) <>
    B.mkPrefixTableWord8 "AttributeExpr" 0x82 0x82 [return AttribStringExpr <*> B.get <*> B.get]

-- binary 0xE9 0xEE
instance B.Binary (TopLevelExpr Object) MTab where
  put o = case o of
    RequireExpr a               z -> B.prefixByte 0xE9 $ B.put a >> B.put z
    ImportExpr  a             b z -> B.prefixByte 0xEA $ B.put a >> B.put b >> B.put z
    TopScript   a               z -> B.prefixByte 0xEB $ B.put a >> B.put z
    EventExpr   BeginExprType b z -> B.prefixByte 0xEC $ B.put b >> B.put z
    EventExpr   ExitExprType  b z -> B.prefixByte 0xED $ B.put b >> B.put z
    EventExpr   EndExprType   b z -> B.prefixByte 0xEE $ B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting TopLevelExpr"

instance B.HasPrefixTable (TopLevelExpr Object) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "TopLevelExpr" 0xE9 0xEE $
    [ return RequireExpr <*> B.get <*> B.get
    , return ImportExpr  <*> B.get <*> B.get <*> B.get
    , return TopScript   <*> B.get <*> B.get
    , return (EventExpr BeginExprType) <*> B.get <*> B.get
    , return (EventExpr ExitExprType ) <*> B.get <*> B.get
    , return (EventExpr EndExprType  ) <*> B.get <*> B.get
    ]

instance Executable (TopLevelExpr Object) () where
  execute o = errCurrentModule $ case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    RequireExpr{} -> attrib "require"
    ImportExpr{}  -> attrib "import"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    TopScript script _ -> do
      ((), dict) <- execNested mempty $ catchPredicate (execute script) >>= \pval -> case pval of
        OK                _  -> return ()
        PFail (ExecReturn _) -> return ()
        PFail           err  -> throwError err
        Backtrack            -> return () -- do not backtrack at the top-level
      let addIfFuncs a b = maybe b id $ do -- overwrite previously declared variables...
            (a, b) <- Just (,) <*> fromObj a <*> fromObj b
            Just $ obj ((a++b)::[CallableCode]) -- ...unless both variables are [CallableCode]
      modify $ \xunit -> xunit{ globalData = M.unionWith addIfFuncs (globalData xunit) dict }
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    EventExpr typ script _ -> do
      let exec = setupCodeBlock script
      let f = (++[exec])
      modify $ \xunit -> case typ of
        BeginExprType -> xunit{ preExec      = f (preExec      xunit) }
        EndExprType   -> xunit{ postExec     = f (postExec     xunit) }
        ExitExprType  -> xunit{ quittingTime = f (quittingTime xunit) }
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    where
      attrib a = fail $ a++" expression must occur only at the top of a dao script file"

instance ObjectClass (TopLevelExpr Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (TopLevelExpr Object) where
  haskellDataInterface = interface "TopLevel" $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> autoDefPPrinter

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass AST_Attribute where
  toDaoStruct = ask >>= \o -> case o of
    AST_AttribDotName str     -> renameConstructor "AttributeDotName" $ innerToStruct str
    AST_AttribString  str loc ->
      renameConstructor "AttributeString" $ "value" .= str >> putLocation loc

instance FromDaoStructClass AST_Attribute where
  fromDaoStruct = msum $
    [ constructor "AttributeDotName" >> AST_AttribDotName <$> innerFromStruct "DotLabel"
    , constructor "AttributeString"  >> return AST_AttribString  <*> req "value" <*> location
    ]

instance ObjectClass AST_Attribute where { obj=new; fromObj=objFromHata; }

instance HataClass AST_Attribute where
  haskellDataInterface = interface "AttributeExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

instance ToDaoStructClass (AST_TopLevel Object) where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Require    a     loc -> nm "Require" $ "attribute" .= a >> putLocation loc
    AST_Import     a b   loc -> nm "Import" $ "attribute" .= a >> "namespace" .= b >> putLocation loc
    AST_TopScript  a     loc -> nm "TopLevel" $ "script" .= a >> putLocation loc
    AST_TopComment a         -> nm "Comment" $ putComments a
    AST_Event      a b c loc ->
      nm "Event" $ "type" .= a >> "block" .= c >> putComments b >> putLocation loc

instance FromDaoStructClass (AST_TopLevel Object) where
  fromDaoStruct = msum $
    [ constructor "Import"   >> return AST_Import    <*> req "attribute" <*> req "namespace" <*> location
    , constructor "Require"  >> return AST_Require   <*> req "attribute" <*> location
    , constructor "TopLevel" >> return AST_TopScript <*> req "script"    <*> location
    , constructor "Event"    >> return AST_Event     <*> req "type"      <*> comments <*> req "block" <*> location
    , constructor "Comment"  >> AST_TopComment     <$> comments
    ]

instance ObjectClass (AST_TopLevel Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_TopLevel Object) where
  haskellDataInterface = interface "TopLevelExpression" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- the number is encoded by the ASCII encoded string "DaoProg\0"
program_magic_number :: Word64
program_magic_number = 0x44616f50726f6700

instance B.Binary (Program Object) MTab where
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
instance Executable (Program Object) () where
  execute (Program ast) = do
    ((), localVars) <- execNested mempty $ mapM_ execute (dropWhile isAttribute ast)
    -- Now, the local variables that were defined in the top level need to be moved to the global
    -- variable store.
    modify $ \xunit -> xunit{ globalData = M.union localVars (globalData xunit) }

instance ObjectClass (Program Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (Program Object) where
  haskellDataInterface = interface "ProgramData" $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

_withGlobalKey :: Object -> (H.Index Object -> RefMonad Object Dynamic a) -> Exec a
_withGlobalKey idx f = gets globalMethodTable >>= \mt -> 
  gets runtimeRefTable >>= liftIO . runReaderT (f $ H.hashNewIndex (H.deriveHash128_DaoBinary mt) idx)

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

instance ToDaoStructClass (AST_SourceCode Object) where
  toDaoStruct = renameConstructor "SourceCode" $ do
    "modified" .=@ sourceModified
    "path"     .=@ sourceFullPath
    asks directives >>= define "code" . listToObj

instance FromDaoStructClass (AST_SourceCode Object) where
  fromDaoStruct = constructor "SourceCode" >>
    return AST_SourceCode <*> req "modified" <*> req "path" <*> reqList "code"

instance ObjectClass (AST_SourceCode Object) where { obj=new; fromObj=objFromHata; }

instance HataClass (AST_SourceCode Object) where
  haskellDataInterface = interface "SourceCode" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

-- | Simply converts an 'Dao.Interpreter.AST_SourceCode' directly to a list of
-- 'Dao.Interpreter.TopLevelExpr's.
evalTopLevelAST :: AST_SourceCode Object -> Exec (Program Object)
evalTopLevelAST ast = case toInterm ast of
  [o] -> return o
  []  -> fail "converting AST_SourceCode to Program by 'toInterm' returned null value"
  _   -> fail "convertnig AST_SourceCode to Program by 'toInterm' returned ambiguous value"

----------------------------------------------------------------------------------------------------
-- $Builtin_object_interfaces
-- The following functions provide object interfaces for essential data types.

instance HataClass () where { haskellDataInterface = interface "HaskellNullValue" (return ()) }

type Get a = B.GGet  MethodTable a
type Put   = B.GPut  MethodTable

-- This is only necessary to shorten the name 'MethodTable' because it is used throughout so many
-- instance declarations and type contexts.
type MTab = MethodTable

----------------------------------------------------------------------------------------------------

newtype MethodTable = MethodTable (M.Map Name (Interface Dynamic))

instance Monoid MethodTable where
  mempty  = MethodTable mempty
  mappend (MethodTable a) (MethodTable b) = let dups = M.intersection a b in
    if M.null dups
    then MethodTable (M.union b a)
    else error ("Namespace conflict when installing built-in data type interfaces: "++show (M.keys dups))

-- | Lookup an 'Interface' by it's name from within the 'Exec' monad.
execGetObjTable :: Name -> Exec (Maybe (Interface Dynamic))
execGetObjTable nm = gets (lookupMethodTable nm . globalMethodTable)

lookupMethodTable :: Name -> MethodTable -> Maybe (Interface Dynamic)
lookupMethodTable nm (MethodTable tab) = M.lookup nm tab

-- not for export, use 'daoClass'
_insertMethodTable :: (Typeable o, HataClass o) => o -> Interface o -> MethodTable -> MethodTable
_insertMethodTable _ ifc = flip mappend $
  MethodTable (M.singleton (objInterfaceName ifc) (interfaceToDynamic ifc))

instance B.HasCoderTable MethodTable where
  getEncoderForType nm mtab = fmap fst $ lookupMethodTable nm mtab >>= objBinaryFormat
  getDecoderForType nm mtab = fmap snd $ lookupMethodTable nm mtab >>= objBinaryFormat

----------------------------------------------------------------------------------------------------

-- | Implements a "for" loop using a 'ReadIterable' item. This function should receive the
-- 'iter' object produced by 'initReadIter' that can be used by this function to extract each
-- 'val' and a function that is evaluated using a 'val' on every iteration of the loop.
-- 'readForLoop' should be defined call the given function as many times as necessary to
-- exaust the 'val's in the 'iter'.
class ReadIterable iter val | iter -> val where
  readForLoop :: iter -> (val -> Exec ()) -> Exec ()

instance ReadIterable [Object] Object where { readForLoop iter = forM_ iter }

instance ReadIterable Hata Object where
  readForLoop h@(Hata ifc d) f = case objReadIterable ifc of
    Nothing  -> throwBadTypeError "cannot iterate over object" (obj h) []
    Just for -> for d f

instance ReadIterable Object Object where
  readForLoop o f = case o of
    OList    o -> readForLoop o f
    OHaskell o -> readForLoop o f
    _          -> throwBadTypeError "cannot iterate over object" o []

----------------------------------------------------------------------------------------------------

-- | A class that provides the 'updateForLoop' function, which is a function that will iterate over
-- types which can be read sequentially and modified as they are read.
class UpdateIterable iter val | iter -> val where
  updateForLoop :: iter -> (val -> Exec val) -> Exec iter

instance UpdateIterable [Object] (Maybe Object) where
  updateForLoop iter f =
    fmap (concatMap $ \o -> maybe [] id $ (o>>=fromObj) <|> fmap return o) (forM iter $ f . Just)

instance UpdateIterable Hata (Maybe Object) where
  updateForLoop h@(Hata ifc d) f = case objUpdateIterable ifc of
    Nothing  -> throwBadTypeError "cannot iterate over object" (obj h) []
    Just for -> Hata ifc <$> for d f

instance UpdateIterable T_dict (Maybe Object) where
  updateForLoop m f = fmap (M.fromList . concat) $ forM (M.assocs m) $ \ (i, o) -> do
    p <- f (Just $ obj $ Pair (obj i, o))
    case p of
      Nothing -> return []
      Just  p -> case fromObj p of
        Just (Pair (ref, o)) -> case fromObj ref of
          Just ref -> return [(ref, o)]
          Nothing  -> throwBadTypeError "iterator cannot updte dictionary item" ref []
        Nothing -> throwBadTypeError "a dictionary iterator must store Pair objects" p []

instance UpdateIterable Object (Maybe Object) where
  updateForLoop o f = case o of
    OList    o -> OList    <$> updateForLoop o f
    ODict    o -> ODict    <$> updateForLoop o f
    OHaskell o -> OHaskell <$> updateForLoop o f
    o          -> throwBadTypeError "cannot iterae over object" o []

----------------------------------------------------------------------------------------------------

-- | This class only exists to allow many different Haskell data types to declare their
-- 'Interface' under the same funcion name: 'haskellDataInterface'. Instantiate this function with
-- the help of the 'interface' function.
class HataClass typ where { haskellDataInterface :: Interface typ }

instance HataClass Location where
  haskellDataInterface = interface "Location" $ do
    autoDefEquality >> autoDefOrdering
    autoDefToStruct >> autoDefFromStruct
    autoDefPPrinter

instance HataClass Comment where
  haskellDataInterface = interface "Comment" $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

instance HataClass DotNameExpr where
  haskellDataInterface = interface "DotName" $ do
    autoDefEquality >> autoDefBinaryFmt >> autoDefPPrinter

instance HataClass AST_DotName where
  haskellDataInterface = interface "DotNameExpression" $ do
    autoDefEquality >> autoDefPPrinter >> autoDefToStruct >> autoDefFromStruct

instance HataClass DotLabelExpr where
  haskellDataInterface = interface "DotLabel" $ do
    autoDefEquality >> autoDefBinaryFmt >> autoDefPPrinter

instance HataClass AST_DotLabel where
  haskellDataInterface = interface "DotLabelExpression" $ do
    autoDefEquality >> autoDefPPrinter >> autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance UpdateIterable (H.HashMap Object Object) (Maybe Object) where
  updateForLoop hm f = fmap (H.fromList . concat) $ forM (H.assocs hm) $ \ (ix, o) -> do
    hash128 <- getObjectHash128
    p <- f (Just $ obj $ Pair (H.indexKey ix, o))
    case p of
      Nothing -> return []
      Just  p -> do
        let badtype = throwBadTypeError "cannot update hash map in iterator with item" p []
        maybe badtype (\ (Pair(a,b)) -> return [(H.hashNewIndex hash128 a, b)]) (fromObj p)

instance ObjectClass (H.HashMap Object Object) where { obj=new; fromObj=objFromHata; }

-- | The hash function for 'Object's relies on the binary serialization of the object, which
-- requires access to the 'MethodTable' of the current 'ExecUnit'. Therefore the hash function must
-- be derived from the 'Exec' monad, the 'Object' data type unfortunately cannot simply derive the
-- 'Data.HashMap.Int128Hashable' class.
getObjectHash128 :: Exec (Object -> H.Hash128)
getObjectHash128 = gets globalMethodTable >>= \mt -> return (H.deriveHash128_DaoBinary mt)

instance HataClass (H.HashMap Object Object) where
  haskellDataInterface = interface "HashMap" $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt >> autoDefPPrinter
    autoDefSizeable >> autoDefUpdateIterable
    let un _ a b = xmaybe (fromObj b) >>= \b -> return $ new $ H.union b a
    defInfixOp ADD  un
    defInfixOp ORB  un
    defInfixOp ANDB $ \ _ a b -> xmaybe (fromObj b) >>= \b -> return $ new $ (H.intersection b a :: H.HashMap Object Object)
    defInfixOp SUB  $ \ _ a b -> xmaybe (fromObj b) >>= \b -> return $ new $ (H.difference b a :: H.HashMap Object Object)
    let initItems hmap ox = do
          hash128 <- getObjectHash128
          let f hmap o = case o of
                InitSingle o -> do
                  let idx = H.hashNewIndex hash128 o
                  return $ H.hashInsert idx o hmap
                InitAssign i op o -> do
                  i <- derefObject i
                  let idx = H.hashNewIndex hash128 i
                  let ref = Just (RefObject i NullRef) <|> fromObj i
                  o <- evalUpdateOp ref op o (H.hashLookup idx hmap)
                  return $ case o of
                    Nothing -> H.hashDelete idx hmap
                    Just  o -> H.hashInsert idx o hmap
          foldM f hmap ox
    defInitializer hashMapFromList initItems
    let single_index :: Monad m => (Object -> m a) -> [Object] -> m a
        single_index f ix = case ix of
          [i] -> f i
          []  -> fail "no index value provided in subscript to HashMap data type"
          _   -> fail "HashMap is a one-dimensional data type, indexed with multi-dimesional subscript"
    defIndexer $ \hm -> single_index $ \i -> do
      hash128 <- getObjectHash128
      i <- derefObject i
      xmaybe (H.hashLookup (H.hashNewIndex hash128 i) hm)
    defIndexUpdater $ \ix upd -> flip single_index ix $ \i -> do
      hash128 <- focusLiftExec getObjectHash128
      i <- pure (H.hashNewIndex hash128 i)
      hm <- get
      (result, (changed, o)) <- withInnerLens (H.hashLookup i hm) upd
      when changed (put $ H.hashAlter (const o) i hm)
      return result

hashMapFromList :: [Object] -> Exec (H.HashMap Object Object)
hashMapFromList ox = do
  hash128 <- getObjectHash128
  let fromDict = fmap (\ (i, o) -> (H.hashNewIndex hash128 (obj i), o)) . M.assocs
      f (a, o) = case o of
        OList ox -> case mapM (\ (i, o) -> maybe (Left (i, o)) Right (fromObj o)) (zip [0..] ox) of
          Left (i, t) -> throwBadTypeError "list item in hash map initializer" t $
            [(errInInitzr, OInt a), (ustr "listIndex", OInt i)]
          Right ox -> return $ fmap (\ (Pair(a,b)) -> (H.hashNewIndex hash128 a, b)) ox
        OTree (Struct{fieldMap=ox}) -> return $ fromDict ox
        ODict   ox -> return $ fromDict ox
        o -> do
          let badtype = throwBadTypeError "hash map initializer from list" o [(errInInitzr, OInt a)]
          maybe badtype return $ msum $
            [ fromObj o >>= \ (Pair(a,b)) -> Just [(H.hashNewIndex hash128 a, b)]
            , H.assocs <$> fromObj o
            ]
  H.fromList . concat <$> mapM f (zip [1..] ox)

builtin_HashMap :: DaoFunc ()
builtin_HashMap =
  daoFunc{ daoForeignFunc = \ () -> fmap (flip (,) () . Just . obj) . hashMapFromList }

----------------------------------------------------------------------------------------------------

-- | When defining the function used by the Dao interpreter to construct your object from an
-- initializer statement, a statement which looks like the following code:
-- > MyObj(0, a) { item = 1, item += 3, x, y };
-- you will need to receive the list of items expressed in curly-brackets, which could be an
-- assignment operation, or a single object value expression. This data type provides the necessary
-- data to your initializer function.
data InitItem
  = InitSingle Object
  | InitAssign Object UpdateOp Object
  deriving (Eq, Ord, Typeable)

----------------------------------------------------------------------------------------------------

-- | This is all of the functions used by the "Dao.Evaluator" when manipulating objects in a Dao
-- program. Behavior of objects when they are used in "for" statements or "with" statements, or when
-- they are dereferenced using the "@" operator, or when they are used in equations are all defined
-- here.
-- 
-- So this table is the reason you instantiate 'HataClass'.
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
  { objInterfaceName   :: Name
  , objHaskellType     :: TypeRep -- ^ this type is deduced from the initial value provided to the 'interface'.
  , objCastFrom        :: Maybe (Object -> typ)                                                      -- ^ defined by 'defCastFrom'
  , objEquality        :: Maybe (typ -> typ -> Bool)                                                 -- ^ defined by 'defEquality'
  , objOrdering        :: Maybe (typ -> typ -> Ordering)                                             -- ^ defined by 'defOrdering'
  , objBinaryFormat    :: Maybe (typ -> Put, Get typ)                                                -- ^ defined by 'defBinaryFmt'
  , objNullTest        :: Maybe (typ -> Bool)                                                        -- ^ defined by 'defNullTest'
  , objPPrinter        :: Maybe (typ -> PPrint)                                                      -- ^ defined by 'defPPrinter'
  , objReadIterable    :: Maybe (typ -> (Object -> Exec ()) -> Exec ())                              -- ^ defined by 'defReadIterator'
  , objUpdateIterable  :: Maybe (typ -> (Maybe Object -> Exec (Maybe Object)) -> Exec typ)           -- ^ defined by 'defUpdateIterator'
  , objIndexer         :: Maybe (typ -> [Object] -> Exec Object)                                     -- ^ defined by 'defIndexer'
  , objIndexUpdater    :: Maybe (ObjectUpdate typ [Object])                                          -- ^ defined by 'defIndexUpdater'
  , objSizer           :: Maybe (typ -> Exec Object)                                                 -- ^ defined by 'defSizer'
  , objToStruct        :: Maybe (ToDaoStruct typ ())                                                 -- ^ defined by 'defStructFormat'
  , objFromStruct      :: Maybe (FromDaoStruct typ)                                                  -- ^ defined by 'defStructFormat'
  , objInitializer     :: Maybe ([Object] -> Exec typ, typ -> [InitItem] -> Exec typ)                -- ^ defined by 'defDictInit'
  , objTraverse        :: Maybe (ObjectTraverse typ [Object])                                        -- ^ defined by 'defTraverse'
  , objInfixOpTable    :: Maybe (Array InfixOp  (Maybe (InfixOp  -> typ -> Object -> XPure Object))) -- ^ defined by 'defInfixOp'
  , objArithPfxOpTable :: Maybe (Array ArithPfxOp (Maybe (ArithPfxOp -> typ -> XPure Object)))       -- ^ defined by 'defPrefixOp'
  , objCallable        :: Maybe (typ -> Exec [CallableCode])                                         -- ^ defined by 'defCallable'
  , objDereferencer    :: Maybe (typ -> Exec (Maybe Object))
  , objMethodTable     :: M.Map Name (DaoFunc typ)
  }
  deriving Typeable

instance Eq  (Interface typ) where { a==b = objHaskellType a == objHaskellType b }

instance Ord (Interface typ) where { compare a b = compare (objHaskellType a) (objHaskellType b) }

-- | This function works a bit like 'Data.Functor.fmap', but maps an 'Interface' from one type
-- to another. This requires two functions: one that can cast from the given type to the adapted
-- type (to convert outputs of functions), and one that can cast back from the adapted type to the
-- original type (to convert inputs of functions). Each coversion function takes a string as it's
-- first parameter, this is a string containing the name of the function that is currently making
-- use of the conversion operation. Should you need to use 'Prelude.error' or 'newError', this
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
  , objReadIterable    = let n="objReadIterable"   in fmap (\for t -> for (b2a n t)) (objReadIterable ifc)
  , objUpdateIterable  = let n="objUpdateIterable" in fmap (\for t -> fmap (a2b n) . for (b2a n t)) (objUpdateIterable ifc)
  , objIndexer         = let n="objIndexer"        in fmap (\f i -> f (b2a n i)) (objIndexer ifc)
  , objIndexUpdater    = let n="objIndexUpdater"   in fmap (\upd i f -> convertFocus (a2b n) (b2a n) (upd i f)) (objIndexUpdater ifc)
  , objSizer           = let n="objSizer"          in fmap (\f o -> f (b2a n o)) (objSizer ifc)
  , objToStruct        = let n="objToStruct"       in fmap (fmapHaskDataToStruct (a2b n) (b2a n)) (objToStruct ifc)
  , objFromStruct      = let n="objFromStruct"     in fmap (fmap (a2b n)) (objFromStruct ifc)
  , objInitializer     = let n="objInitializer"    in fmap (\ (init, eval) -> (\ox -> fmap (a2b n) (init ox), \typ ox -> fmap (a2b n) (eval (b2a n typ) ox))) (objInitializer ifc)
  , objTraverse        = let n="objTraverse"       in fmap (\focus f -> convertFocus (a2b n) (b2a n) (focus f)) (objTraverse ifc)
  , objInfixOpTable    = let n="objInfixOpTable"   in fmap (fmap (fmap (\infx op b -> infx op (b2a n b)))) (objInfixOpTable  ifc)
  , objArithPfxOpTable = let n="objPrefixOpTable"  in fmap (fmap (fmap (\prfx op b -> prfx op (b2a n b)))) (objArithPfxOpTable ifc)
  , objMethodTable     = let n="objMethodTable"    in fmap (\func -> func{ daoForeignFunc = \t -> fmap (fmap (a2b n)) . daoForeignFunc func (b2a n t) }) (objMethodTable ifc)
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
  { objIfcHaskellType    :: TypeRep
  , objIfcCastFrom       :: Maybe (Object -> typ)
  , objIfcEquality       :: Maybe (typ -> typ -> Bool)
  , objIfcOrdering       :: Maybe (typ -> typ -> Ordering)
  , objIfcBinaryFormat   :: Maybe (typ -> Put, Get typ)
  , objIfcNullTest       :: Maybe (typ -> Bool)
  , objIfcPPrinter       :: Maybe (typ -> PPrint)
  , objIfcReadIterable   :: Maybe (typ -> (Object -> Exec ()) -> Exec ())
  , objIfcUpdateIterable :: Maybe (typ -> (Maybe Object -> Exec (Maybe Object)) -> Exec typ)
  , objIfcIndexer        :: Maybe (typ -> [Object] -> Exec Object)
  , objIfcIndexUpdater   :: Maybe (ObjectUpdate typ [Object])
  , objIfcSizer          :: Maybe (typ -> Exec Object)
  , objIfcToStruct       :: Maybe (ToDaoStruct typ ())
  , objIfcFromStruct     :: Maybe (FromDaoStruct typ)
  , objIfcInitializer    :: Maybe ([Object] -> Exec typ, typ -> [InitItem] -> Exec typ)
  , objIfcTraverse       :: Maybe (ObjectTraverse typ [Object])
  , objIfcInfixOpTable   :: [(InfixOp , InfixOp  -> typ -> Object -> XPure Object)]
  , objIfcPrefixOpTable  :: [(ArithPfxOp, ArithPfxOp -> typ -> XPure Object)]
  , objIfcMethodTable    :: M.Map Name (DaoFunc typ)
  , objIfcCallable       :: Maybe (typ -> Exec [CallableCode])
  , objIfcDerefer        :: Maybe (typ -> Exec (Maybe Object))
  }

initHDIfcBuilder :: TypeRep -> HDIfcBuilder typ
initHDIfcBuilder typ =
  HDIfcBuilder
  { objIfcHaskellType    = typ
  , objIfcCastFrom       = Nothing
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
  , objIfcInitializer    = Nothing
  , objIfcTraverse       = Nothing
  , objIfcInfixOpTable   = []
  , objIfcPrefixOpTable  = []
  , objIfcMethodTable    = mempty
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
defReadIterable :: Typeable typ => (typ -> (Object -> Exec ()) -> Exec ()) -> DaoClassDefM typ ()
defReadIterable iter = _updHDIfcBuilder $ \st -> st{ objIfcReadIterable=Just iter }

-- | Define 'defReadIterable' automatically using the instance of @typ@ in the 'ReadIterable' class.
autoDefReadIterable :: (Typeable typ, ReadIterable typ Object) => DaoClassDefM typ ()
autoDefReadIterable = defReadIterable readForLoop

-- | The callback function defined here is used if an object of your @typ@ is ever used in a @for@
-- statement in a Dao program. However it is much better to instantiate your @typ@ into the
-- 'UpdateIterable' class and use 'autoDefIterator' instead. If 'defReadIterator' is also defined,
-- the read iterator is always ignored in favor of this function.
defUpdateIterable :: Typeable typ => (typ -> (Maybe Object -> Exec (Maybe Object)) -> Exec typ) -> DaoClassDefM typ ()
defUpdateIterable iter = _updHDIfcBuilder(\st->st{objIfcUpdateIterable=Just iter})

-- | Define 'defUpdateIterable' automatically using the instance of @typ@ in the 'ReadIterable'
-- class.
autoDefUpdateIterable :: (Typeable typ, UpdateIterable typ (Maybe Object)) => DaoClassDefM typ ()
autoDefUpdateIterable = defUpdateIterable updateForLoop

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
defIndexUpdater :: Typeable typ => ObjectUpdate typ [Object] -> DaoClassDefM typ ()
defIndexUpdater fn = _updHDIfcBuilder(\st->st{ objIfcIndexUpdater=Just fn })

-- | Define a function used by the built-in "size()" function to return an value indicating the size
-- of your @typ@ object.
defSizer :: Typeable typ => (typ -> Exec Object) -> DaoClassDefM typ ()
defSizer fn = _updHDIfcBuilder(\st->st{objIfcSizer=Just fn})

autoDefSizeable :: (Typeable typ, Sizeable typ) => DaoClassDefM typ ()
autoDefSizeable = defSizer getSizeOf

-- | Use your data type's instantiation of 'ToDaoStructClass' to call 'defToStruct'.
autoDefToStruct :: forall typ . (Typeable typ, ToDaoStructClass typ) => DaoClassDefM typ ()
autoDefToStruct = defToStruct toDaoStruct

-- | When a label referencing your object has a field record accessed, for example:
-- > c = a.b;
-- if your object is referenced by @a@ and the script expression wants to access a record called @b@
-- from within it, then function defined here will be used.
defToStruct :: Typeable typ => ToDaoStruct typ () -> DaoClassDefM typ ()
defToStruct encode = _updHDIfcBuilder (\st -> st{ objIfcToStruct=Just encode })

-- | When a label referencing your object has a field record updated, for example:
-- > a.b = c;
-- if your object is referenced by @a@ and the script expression wants to update a record called @b@
-- within it by assigning it the value referenced by @c@, then the function defined here will be
-- used.
autoDefFromStruct :: (Typeable typ, FromDaoStructClass typ) => DaoClassDefM typ ()
autoDefFromStruct = defFromStruct fromDaoStruct

-- | If for some reason you need to define a tree encoder and decoder for the 'Interface' of your
-- @typ@ without instnatiating 'ToDaoStructClass' or 'FromDaoStructClass', use
-- this function to define the tree encoder an decoder directly
defFromStruct :: Typeable typ => FromDaoStruct typ -> DaoClassDefM typ ()
defFromStruct decode = _updHDIfcBuilder (\st -> st{ objIfcFromStruct=Just decode })

-- | The callback defined here is used when a Dao program makes use of the static initialization
-- syntax of the Dao programming language, which are expression of this form:
-- > a = MyType { paramA=initA, paramB=initB, .... };
-- > a = MyType(param1, param2, ...., paramN) { paramA=initA, paramB=initB, .... };
-- When the interpreter sees this form of expression, it looks up the 'Interface' for your
-- @typ@ and checks if a callback has been defined by 'defDictInit'. If so, then the callback is
-- evaluated with a list of object values passed as the first parameter which contain the object
-- values written in the parentheses, and a list of 'InitItem's as the second parameter containing
-- the contents of the curly-brackets.
defInitializer :: Typeable typ => ([Object] -> Exec typ) -> (typ -> [InitItem] -> Exec typ) -> DaoClassDefM typ ()
defInitializer fa fb = _updHDIfcBuilder(\st->st{objIfcInitializer=Just (fa, fb)})

-- | Data structures in the Dao programming language can be traversed if you provide a function that
-- can update every 'Object' contained wihtin the data structure.
defTraverse :: Typeable typ => (([Object] -> Object -> ObjectFocus [([Object], Object)] ()) -> ObjectFocus typ ()) -> DaoClassDefM typ ()
defTraverse f = _updHDIfcBuilder(\st->st{objIfcTraverse=Just f})

-- | Define the 'defTraverse' function using the instance of 'objectFMap' for your @typ@ in the
-- @('ObjectFunctor' ['Object'] typ)@ class.
autoDefTraverse :: (Typeable typ, ObjectFunctor typ [Object]) => DaoClassDefM typ ()
autoDefTraverse = defTraverse objectFMap

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

defMethod :: (UStrType name, Typeable typ) => name -> DaoFunc typ -> DaoClassDefM typ ()
defMethod inname infn = do
  let name = fromUStr $ toUStr inname
  let fn = infn{ daoFuncName=name }
  let dupname st _  = error $ concat $ 
        [ "Internal error: duplicate method name \"", show name
        , "\" for data type ", show (objIfcHaskellType st)
        ] 
  _updHDIfcBuilder $ \st ->
    st{ objIfcMethodTable = M.alter (maybe (Just fn) (dupname st)) name $ objIfcMethodTable st }

-- | Like 'detMethod' but creates a function that takes no parameters.
defMethod0 :: (UStrType name, Typeable this) => name -> (this -> Exec (Maybe Object, this)) -> DaoClassDefM this ()
defMethod0 name f = defMethod name $
  daoFunc
  { funcAutoDerefParams = False
  , daoForeignFunc = \this ox -> case ox of
      [] -> f this
      ox -> throwArityError "" 0 ox [(errInFunc, obj $ reference UNQUAL (fromUStr $ toUStr name))]
  }

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
interface :: (UStrType name, Typeable typ) => name -> DaoClassDefM typ ig -> Interface typ
interface nm defIfc = let name = toUStr nm in case maybeFromUStr name of
  Nothing   -> error $
    "Failed to install built-in data type interface, invalid type name provided: "++uchars nm
  Just name ->
    Interface
    { objHaskellType     = typ
    , objInterfaceName   = name
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
    , objInitializer     = objIfcInitializer    ifc
    , objTraverse        = objIfcTraverse       ifc
    , objCallable        = objIfcCallable       ifc
    , objDereferencer    = objIfcDerefer        ifc
    , objInfixOpTable    = mkArray "defInfixOp"  $ objIfcInfixOpTable  ifc
    , objArithPfxOpTable = mkArray "defPrefixOp" $ objIfcPrefixOpTable ifc
    , objMethodTable     = objIfcMethodTable    ifc
    }
  where
    mktyp :: Typeable typ => DaoClassDefM typ ig -> typ -> TypeRep
    mktyp _ undefd = typeOf undefd
    typ = mktyp defIfc $
      error "'Dao.Interpreter.interface' evaluated 'Data.Typeable.typeOf' on undefined value"
    ifc = execState (daoClassDefState defIfc) (initHDIfcBuilder typ)
    mkArray oiName elems =
      minAccumArray (onlyOnce oiName) Nothing $ map (\ (i, e) -> (i, (i, Just e))) elems
    onlyOnce oiName a (i, b)  = case a of
      Nothing -> b
      Just  _ -> conflict oiName ("the "++show i++" operator")
    conflict oiName funcName = error $ concat $
      [ "'", oiName
      , "' has conflicting functions for ", funcName
      , " for the 'HataClass' instantiation of the '", show typ
      , "' Haskell data type."
      ]

