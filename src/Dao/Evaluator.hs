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


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Evaluator where

import           Dao.Debug.ON
import           Dao.Token
import           Dao.Object
import qualified Dao.Tree as T
import           Dao.Pattern
import           Dao.Resource
import           Dao.Predicate
import           Dao.Files
import           Dao.Parser

import           Dao.Object.Math
import           Dao.Object.Show
import           Dao.Object.Binary
import           Dao.Object.Pattern
import           Dao.Object.Parser

import           Control.Exception
import           Control.Concurrent
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State -- for constructing 'Program's from 'SourceCode's.

import           Data.Monoid
import           Data.Maybe
import           Data.Either
import           Data.Array.IArray
import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.List
import           Data.Time.Clock
import           Data.Ratio
import           Data.Complex
import           Data.IORef
import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.IntMap as I
import qualified Data.ByteString.Lazy.UTF8 as U

import           System.IO

----------------------------------------------------------------------------------------------------

initExecUnit :: Runtime -> UPath -> TreeResource -> Run ExecUnit
initExecUnit runtime modName initGlobalData = do
  unctErrs <- dNewMVar $loc "ExecUnit.uncaughtErrors" []
  recurInp <- dNewMVar $loc "ExecUnit.recursiveInput" []
  qheap    <- newTreeResource  "ExecUnit.queryTimeHeap" T.Void
  running  <- dNewMVar $loc "ExecUnit.runningThreads" S.empty
  xstack   <- dNewMVar $loc "ExecUnit.execStack" emptyStack
  toplev   <- dNewMVar $loc "ExecUnit.toplevelFuncs" M.empty
  files    <- dNewMVar $loc "ExecUnit.execOpenFiles" M.empty
  rules    <- dNewMVar $loc "ExecUnit.ruleSet" T.Void
  return $
    ExecUnit
    { parentRuntime      = runtime
    , currentDocument    = Nothing
    , currentQuery       = Nothing
    , currentPattern     = Nothing
    , currentMatch       = Nothing
    , currentExecutable  = error "ExecUnit.currentExecutable is undefined"
    , currentBranch      = []
    , importsTable       = []
    , execAccessRules    = RestrictFiles (Pattern{getPatUnits = [Wildcard], getPatternLength = 1})
    , builtinFuncs       = initBuiltinFuncs
    , toplevelFuncs      = toplev
    , queryTimeHeap      = qheap
    , runningThreads     = running
    , execStack          = xstack
    , execOpenFiles      = files
    , recursiveInput     = recurInp
    , uncaughtErrors     = unctErrs
      ---- items that were in the Program data structure ----
    , programModuleName = modName
    , programImports    = []
    , constructScript   = []
    , destructScript    = []
    , requiredBuiltins  = []
    , programAttributes = M.empty
    , preExecScript     = []
    , programTokenizer  = return . tokens . uchars
    , programComparator = (==)
    , postExecScript    = []
    , ruleSet           = rules
    , globalData        = initGlobalData
    }

setupExecutable :: HasDebugRef r => Com [Com ScriptExpr] -> ReaderT r IO Executable
setupExecutable scrp = do
  staticRsrc <- lift (newIORef M.empty)
  return $
    Executable
    { staticVars = staticRsrc
    , executable = execScriptBlock (unComment scrp)
    }

runExecutable :: T_tree -> Executable -> ExecScript Object
runExecutable initStack exe = local (\xunit -> xunit{currentExecutable = exe}) $!
  execFuncPushStack initStack (executable exe >>= liftIO . evaluate >> return ONull)

-- | Given a list of arguments, matches these arguments toe the given subroutine's
-- 'Dao.Object.ObjPat'. If it matches, the 'Dao.Object.getSubExecutable' of the 'Dao.Object.Executable'
-- is evaluated with 'runExecutable'. If the pattern does not match, 'Nothing' is returned to the
-- 'Dao.Object.ExecScript' monad, which allows multiple 'Dao.Object.Subroutine's to be tried before
-- evaluating to an error in the calling context.
runSubroutine :: [Object] -> Subroutine -> ExecScript (Maybe Object)
runSubroutine args sub =
  case evalMatcher (matchObjectList (argsPattern sub) args >> gets matcherTree) of
    OK       tree -> fmap Just (runExecutable tree (getSubExecutable sub))
    Backtrack     -> return Nothing
    PFail ref msg -> procErr (OPair (OString msg, ORef ref))

-- | Execute a 'Dao.Object.Rule' object as though it were a script that could be called. The
-- parameters passed will be stored into the 'currentMatch' slot durring execution, but all
-- parameters passed must be of type 'Dao.Object.OString', or an error is thrown.
execRuleCall :: [Com ObjectExpr] -> RuleExpr -> ExecScript Object
execRuleCall ax rule = do
  let typeErr o =
        typeError o "when calling a Rule object as though it were a function, all parameters" $
          show ListType++", where each list contains only objects of type "++show StringType
  ax <- forM ax $ \a -> evalObject (unComment a) >>= \a -> case a of
    OList ax -> forM ax $ \a -> case a of
      OString a -> return a
      _ -> typeErr a
    _ -> typeErr a
  local (\xunit -> xunit{currentMatch = Just (matchFromList [] (iLength ax) ax)}) $
    execFuncPushStack T.Void (execScriptBlock (unComment (ruleAction rule)) >> procReturn ONull)

-- | Very simply executes every given script item. Does not use catchReturnObj, does not use
-- 'nestedExecStack'. CAUTION: you cannot assign to local variables unless you call this method
-- within the 'nestedExecStack' or 'execFuncPushStack' functions. Failure to do so will cause a stack
-- underflow exception.
execScriptBlock :: [Com ScriptExpr] -> ExecScript ()
execScriptBlock block = mapM_ execScriptExpr block

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code found in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [Com ScriptExpr] -> ExecScript ()
execGuardBlock block = void (execFuncPushStack T.Void (execScriptBlock block >> return ONull))

-- $BasicCombinators
-- These are the most basic combinators for converting working with the 'ExecUnit' of an
-- 'ExecScript' monad.

----------------------------------------------------------------------------------------------------
-- $StackOperations
-- Operating on the local stack.

stack_underflow = error "INTERNAL ERROR: stack underflow"

-- | Push a new empty local-variable context onto the stack. Does NOT 'catchReturnObj', so it can be
-- used to push a new context for every level of nested if/else/for/try/catch statement, or to
-- evaluate a macro, but not a function call. Use 'execFuncPushStack' to perform a function call within
-- a function call.
nestedExecStack :: T_tree -> ExecScript a -> ExecScript a
nestedExecStack init exe = do
  stack <- fmap execStack ask
  lift (dModifyMVar_ $loc stack (return . stackPush init))
  ce <- procCatch exe
  lift (dModifyMVar_ $loc stack (return . stackPop))
  joinFlowCtrl ce

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. Use 'catchReturnObj' to prevent return calls from halting execution beyond this
-- function. This is what you should use to perform a Dao function call within a Dao function call.
execFuncPushStack :: T_tree -> ExecScript Object -> ExecScript Object
execFuncPushStack dict exe = do
  stackMVar <- lift (dNewMVar $loc "execFuncPushStack/ExecUnit.execStack" (Stack [dict]))
  ce <- procCatch (local (\xunit -> xunit{execStack = stackMVar}) exe)
  case ce of
    FlowReturn obj -> return obj
    _            -> joinFlowCtrl ce

----------------------------------------------------------------------------------------------------

-- | Used to evaluate an expression like @$1@, retrieves the matched pattern associated with an
-- integer. Specifically, it returns a list of 'Dao.ObjectObject's where each object is an
-- 'Dao.Object.OString' contained at the integer index of the 'Dao.Pattern.matchGaps' of a
-- 'Dao.Pattern.Pattern'.
evalIntRef :: Word -> ExecScript Object
evalIntRef i = do
  ma <- fmap currentMatch ask
  let oi = OInt (fromIntegral i)
  case ma >>= matchGaps of
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

-- | Lookup an object in the 'globalData' for this 'ExecUnit'.
execHeapLookup :: [Name] -> ExecScript (Maybe Object)
execHeapLookup name = ask >>= \xunit -> lift (readResource (globalData xunit) name)

-- | Lookup an object in the 'globalData' for this 'ExecUnit'.
execHeapUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
execHeapUpdate name runUpdate = ask >>= \xunit ->
  inEvalDoUpdateResource (globalData xunit) name runUpdate

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
    Nothing                      -> return Nothing
    Just file@(DocumentFile res) -> lift (readResource res (currentBranch xunit ++ name))
    _ -> error ("current document is not an idea file, cannot lookup reference "++showRef name)

-- | Update a reference value in the durrent document, if the current document has been set with a
-- "with" statement.
curDocVarUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
curDocVarUpdate name runUpdate = do
  xunit <- ask
  case currentDocument xunit of
    Nothing                  -> return Nothing
    Just file@(DocumentFile res) ->
      inEvalDoUpdateResource res (currentBranch xunit ++ name) runUpdate
    _ -> error ("current document is not an idea file, cannot update reference "++showRef name)

curDocVarDefine :: [Name] -> Object -> ExecScript (Maybe Object)
curDocVarDefine ref obj = curDocVarUpdate ref (return . const (Just obj))

curDocVarDelete :: [Name] -> Object -> ExecScript (Maybe Object)
curDocVarDelete ref obj = curDocVarUpdate ref (return . const Nothing)

-- | Lookup a value in the 'execStack'.
localVarLookup :: Name -> ExecScript (Maybe Object)
localVarLookup sym =
  fmap execStack ask >>= lift . dReadMVar $loc >>= return . msum . map (T.lookup [sym]) . mapList

-- | Apply an altering function to the map at the top of the local variable stack.
localVarUpdate :: Name -> (Maybe Object -> Maybe Object) -> ExecScript (Maybe Object)
localVarUpdate name alt = ask >>= \xunit -> lift $
  dModifyMVar $loc (execStack xunit) $ \ax -> case mapList ax of
    []   -> stack_underflow
    a:ax ->
      let obj = alt (T.lookup [name] a)
      in  return (Stack (T.update [name] (const obj) a : ax), obj)

-- | Force the local variable to be defined in the top level 'execStack' context, do not over-write
-- a variable that has already been defined in lower in the context stack.
localVarDefine :: Name -> Object -> ExecScript (Maybe Object)
localVarDefine name obj = localVarUpdate name (const (Just obj))

localVarDelete :: Name -> ExecScript (Maybe Object)
localVarDelete nm = localVarUpdate nm (const Nothing)

staticVarLookup :: Name -> ExecScript (Maybe Object)
staticVarLookup nm = do
  exe <- fmap (staticVars . currentExecutable) ask
  liftIO (readIORef exe) >>= return . M.lookup nm

staticVarUpdate :: Name -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
staticVarUpdate nm upd = do
  ref <- fmap (staticVars . currentExecutable) ask
  val <- lift (lift (readIORef ref)) >>= return . (M.lookup nm) >>= upd
  lift (lift (modifyIORef ref (M.update (const val) nm)))
  return val

staticVarDefine :: Name -> Object -> ExecScript (Maybe Object)
staticVarDefine nm obj = staticVarUpdate nm (return . const (Just obj))

staticVarDelete :: Name -> ExecScript (Maybe Object)
staticVarDelete nm = staticVarUpdate nm (return . const Nothing)

-- | Lookup an object, first looking in the current document, then in the 'globalData'.
globalVarLookup :: [Name] -> ExecScript (Maybe Object)
globalVarLookup ref = ask >>= \xunit ->
  (if isJust (currentDocument xunit) then curDocVarLookup else execHeapLookup) ref

globalVarUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
globalVarUpdate ref runUpdate = ask >>= \xunit ->
  (if isJust (currentDocument xunit) then curDocVarUpdate else execHeapUpdate) ref runUpdate

-- | To define a global variable, first the 'currentDocument' is checked. If it is set, the variable
-- is assigned to the document at the reference location prepending 'currentBranch' reference.
-- Otherwise, the variable is assigned to the 'globalData'.
globalVarDefine :: [Name] -> Object -> ExecScript (Maybe Object)
globalVarDefine name obj = globalVarUpdate name (return . const (Just obj))

-- | To delete a global variable, the same process of searching for the address of the object is
-- followed for 'globalVarDefine', except of course the variable is deleted.
globalVarDelete :: [Name] -> ExecScript (Maybe Object)
globalVarDelete name = globalVarUpdate name (return . const Nothing)

qTimeVarLookup :: [Name] -> ExecScript (Maybe Object)
qTimeVarLookup ref = ask >>= \xunit -> lift (readResource (queryTimeHeap xunit) ref)

qTimeVarUpdate :: [Name] -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
qTimeVarUpdate ref runUpdate = ask >>= \xunit ->
  inEvalDoUpdateResource (queryTimeHeap xunit) ref runUpdate

qTimeVarDefine :: [Name] -> Object -> ExecScript (Maybe Object)
qTimeVarDefine name obj = qTimeVarUpdate name (return . const (Just obj))

qTimeVarDelete :: [Name] -> ExecScript (Maybe Object)
qTimeVarDelete name = qTimeVarUpdate name (return . const Nothing)

clearAllQTimeVars :: ExecUnit -> Run ()
clearAllQTimeVars xunit = modifyUnlocked_ (queryTimeHeap xunit) (return . const T.Void)

----------------------------------------------------------------------------------------------------

-- $Built_in_functions
-- Built-in functions, retrieved from an array 'infixOps' or 'prefixOps' by a 'Dao.Object.ArithOp'
-- value, or from 'updateOps' by a 'Dao.Object.UpdateOp' value. Built-in functions check object
-- parameters passed to them with the 'BuiltinOp' monad, which is a fully lazy monad based on
-- 'Dao.Predicate.PValue'.

type BuiltinOp = PValue Location Object

evalBooleans :: (Bool -> Bool -> Bool) -> Object -> Object -> BuiltinOp
evalBooleans fn a b = return (if fn (objToBool a) (objToBool b) then OTrue else ONull)

eval_OR :: Object -> Object -> BuiltinOp
eval_OR = evalBooleans (||)

eval_AND :: Object -> Object -> BuiltinOp
eval_AND = evalBooleans (&&)

asReference :: Object -> PValue Location Reference
asReference o = case o of
  ORef o -> return o
  _      -> mzero

asInteger :: Object -> PValue Location Integer
asInteger o = case o of
  OWord o -> return (toInteger o)
  OInt  o -> return (toInteger o)
  OLong o -> return o
  _       -> mzero

asRational :: Object -> PValue Location Rational
asRational o = case o of
  OFloat     o     -> return (toRational o)
  ODiffTime  o     -> return (toRational o)
  OComplex  (o:+0) -> return (toRational o)
  ORatio     o     -> return o
  _                -> mzero

asStringNoConvert :: Object -> PValue Location UStr
asStringNoConvert o = case o of
  OString o -> return o
  _         -> mzero

asString :: Object -> PValue Location UStr
asString o = case o of
  OString o -> return o
  o         -> return (ustr (showObj 0 o))

asListNoConvert :: Object -> PValue Location [Object]
asListNoConvert o = case o of
  OList o -> return o
  _       -> mzero

asList :: Object -> PValue Location [Object]
asList o = case o of
  OList   o -> return o
  OArray  o -> return (elems o)
  OSet    o -> return (S.elems o)
  ODict   o -> return (map (\ (i, o) -> OPair (OString i, o)) (M.assocs o))
  OIntMap o -> return (map (\ (i, o) -> OPair (OInt (fromIntegral i), o)) (I.assocs o))
  OTree   o -> return (map (\ (i, o) -> OPair (OList (map OString i), o)) (T.assocs o))
  _         -> mzero

-- | Combines two lists of objects, then removes one "layer of lists", that is, if the combined
-- lists are of the form:
-- @list {a, b, ... , list {c, d, ... , list {e, f, ...}, ...} }@ 
-- the resulting list will be @list {a, b, ... , c, d, ... , list {e, f, ... }, ...}@
objListAppend :: [Object] -> [Object] -> Object
objListAppend ax bx = OList $ flip concatMap (ax++bx) $ \a -> case a of
  OList ax -> ax
  a        -> [a]

asHaskellInt :: Object -> PValue Location Int
asHaskellInt o = asInteger o >>= \o ->
  if (toInteger (minBound::Int)) <= o && o <= (toInteger (maxBound::Int))
    then return (fromIntegral o)
    else mzero

evalInt :: (Integer -> Integer -> Integer) -> Object -> Object -> BuiltinOp
evalInt ifunc a b = do
  ia <- asInteger a
  ib <- asInteger b
  let x = ifunc ia ib
  return $ case max (fromEnum (objType a)) (fromEnum (objType b)) of
    t | t == fromEnum WordType -> OWord (fromIntegral x)
    t | t == fromEnum IntType  -> OInt  (fromIntegral x)
    t | t == fromEnum LongType -> OLong (fromIntegral x)
    _ -> error "asInteger returned a value for an object of an unexpected type"

evalNum
  :: (Integer -> Integer -> Integer)
  -> (Rational -> Rational -> Rational)
  -> Object -> Object -> BuiltinOp
evalNum ifunc rfunc a b = msum $
  [ evalInt ifunc a b
  , do  ia <- asRational a
        ib <- asRational b
        let x = rfunc ia ib
        return $ case (max (fromEnum (objType a)) (fromEnum (objType b))) of
          t | t == fromEnum FloatType    -> OFloat    (fromRational x)
          t | t == fromEnum DiffTimeType -> ODiffTime (fromRational x)
          t | t == fromEnum RatioType    -> ORatio    (fromRational x)
          t | t == fromEnum ComplexType  -> OComplex  (fromRational x)
          _ -> error "asRational returned a value for an object of an unexpected type"
  ]

setToMapFrom :: (Object -> PValue Location i) -> ([(i, [Object])] -> m [Object]) -> S.Set Object -> m [Object]
setToMapFrom convert construct o = construct (zip (concatMap (okToList . convert) (S.elems o)) (repeat []))

evalSets
  ::  ([Object] -> Object)
  -> (([Object] -> [Object] -> [Object]) -> M.Map Name [Object] -> M.Map Name [Object] -> M.Map Name [Object])
  -> (([Object] -> [Object] -> [Object]) -> I.IntMap   [Object] -> I.IntMap   [Object] -> I.IntMap   [Object])
  -> (T_set -> T_set  -> T_set)
  -> Object -> Object -> BuiltinOp
evalSets combine dict intmap set a b = msum $
  [ do  a <- case a of
                ODict a -> return (fmap (:[]) a)
                _       -> mzero
        b <- case b of
                OSet  b -> return $ setToMapFrom asStringNoConvert M.fromList b
                ODict b -> return (fmap (:[]) b)
                _       -> mzero
        return (ODict (fmap combine (dict (++) a b)))
  , do  a <- case a of
                OIntMap a -> return (fmap (:[]) a)
                _         -> mzero
        b <- case b of
                OSet    b -> return $ setToMapFrom asHaskellInt I.fromList b
                OIntMap b -> return (fmap (:[]) b)
                _         -> mzero
        return (OIntMap (fmap combine (intmap (++) a b)))
  , do  let toSet s = case s of 
                        OSet s -> return s
                        _      -> mzero
        a <- toSet a
        b <- toSet b
        return (OSet (set a b))
  ]

eval_ADD :: Object -> Object -> BuiltinOp
eval_ADD a b = msum
  [ evalNum (+) (+) a b
  , timeAdd a b, timeAdd b a
  , listAdd a b, listAdd b a
  , stringAdd (++) a b, stringAdd (flip (++)) b a
  ]
  where
    timeAdd a b = case (a, b) of
      (OTime a, ODiffTime b) -> return (OTime (addUTCTime b a))
      (OTime a, ORatio    b) -> return (OTime (addUTCTime (fromRational (toRational b)) a))
      (OTime a, OFloat    b) -> return (OTime (addUTCTime (fromRational (toRational b)) a))
      _                      -> mzero
    listAdd a b = do
      ax <- asListNoConvert a
      bx <- case b of
        OList  bx -> return bx
        OSet   b  -> return (S.elems b)
        OArray b  -> return (elems b)
        _         -> mzero
      return (objListAppend ax bx)
    stringAdd add a b = case a of
      OString a -> do
        b <- asString b
        return (OString (ustr (add (uchars a) (uchars b))))
      _         -> mzero

eval_SUB :: Object -> Object -> BuiltinOp
eval_SUB a b = msum $
  [ evalNum (-) (-) a b
  , evalSets (\a -> head a) (\ _ a b -> M.difference a b) (\ _ a b -> I.difference a b) S.difference a b
  , case (a, b) of
      (OTime a, OTime     b) -> return (ODiffTime (diffUTCTime a b))
      (OTime a, ODiffTime b) -> return (OTime (addUTCTime (negate b) a))
      (OTime a, ORatio    b) -> return (OTime (addUTCTime (fromRational (toRational (negate b))) a))
      (OTime a, OFloat    b) -> return (OTime (addUTCTime (fromRational (toRational (negate b))) a))
      _                  -> mzero
  , do  ax <- asListNoConvert a
        case b of
          OList bx -> return $
            let lenA = length ax
                lenB = length bx
                loop lenA zx ax = case ax of
                  ax'@(a:ax) | lenA>=lenB ->
                    if isPrefixOf bx ax' then  zx++a:ax else  loop (lenA-1) (zx++[a]) ax
                  _                       -> [a]
            in  if lenA <= lenB
                  then  if isInfixOf bx ax then OList [] else a
                  else  OList (loop lenA [] ax)
          OSet  b -> return (OList (filter (flip S.notMember b) ax))
          _       -> mzero
  ]

-- Distributed property of operators is defined. Pass a function to be mapped across containers.
evalDist :: (Object -> Object -> BuiltinOp) -> Object -> Object -> BuiltinOp
evalDist fn a b = multContainer a b where
  mapDist        = concatMap (okToList . fn a)
  mapDistSnd alt = concatMap $ okToList . \ (i, b) ->
    mplus (fn a b >>= \b -> return (i, b)) (if alt then return (i, ONull) else mzero)
  multContainer a b = case a of
    OList   bx -> return $ OList   (mapDist bx)
    OSet    b  -> return $ OSet    (S.fromList (mapDist (S.elems b)))
    OArray  b  -> return $ OArray  $ array (bounds b) $ mapDistSnd True $ assocs b
    ODict   b  -> return $ ODict   $ M.fromList $ mapDistSnd False $ M.assocs b
    OIntMap b  -> return $ OIntMap $ I.fromList $ mapDistSnd False $ I.assocs b
    _          -> mzero

evalDistNum
  :: (Integer  -> Integer  -> Integer )
  -> (Rational -> Rational -> Rational) 
  -> Object -> Object -> BuiltinOp
evalDistNum intFn rnlFn a b = msum $
  [ evalNum intFn rnlFn a b
  , if isNumeric a
      then  evalDist (evalDistNum intFn rnlFn) a b
      else  if isNumeric b then evalDist (flip (evalDistNum intFn rnlFn)) b a else mzero
  ]

eval_MULT :: Object -> Object -> BuiltinOp
eval_MULT a b = evalDistNum (*) (*) a b

eval_DIV :: Object -> Object -> BuiltinOp
eval_DIV a b = evalDistNum div (/) a b

eval_MOD :: Object -> Object -> BuiltinOp
eval_MOD a b = evalDistNum mod (\a b -> let r = a/b in (abs r - abs (floor r % 1)) * signum r) a b

eval_POW :: Object -> Object -> BuiltinOp
eval_POW = evalNum (^) (\a b -> toRational ((fromRational a :: Double) ** (fromRational b :: Double)))

evalBitsOrSets
  :: ([Object]  -> Object)
  -> (([Object] -> [Object] -> [Object]) -> M.Map Name [Object] -> M.Map Name [Object] -> M.Map Name [Object])
  -> (([Object] -> [Object] -> [Object]) -> I.IntMap   [Object] -> I.IntMap   [Object] -> I.IntMap   [Object])
  -> (T_set -> T_set  -> T_set)
  -> (Integer -> Integer -> Integer)
  -> Object -> Object -> BuiltinOp
evalBitsOrSets combine dict intmap set num a b =
  mplus (evalSets combine dict intmap set a b) (evalInt num a b)

eval_ORB :: Object -> Object -> BuiltinOp
eval_ORB  a b = evalBitsOrSets OList M.unionWith        I.unionWith        S.union        (.|.) a b

eval_ANDB :: Object -> Object -> BuiltinOp
eval_ANDB a b = evalBitsOrSets OList M.intersectionWith I.intersectionWith S.intersection (.&.) a b

eval_XORB :: Object -> Object -> BuiltinOp
eval_XORB a b = evalBitsOrSets (\a -> head a) mfn ifn sfn xor a b where
  sfn = fn S.union S.intersection S.difference head
  mfn = fn M.union M.intersection M.difference
  ifn = fn I.union I.intersection I.difference
  fn u n del _ a b = (a `u` b) `del` (a `n` b)

evalShift :: (Int -> Int) -> Object -> Object -> BuiltinOp
evalShift fn a b = asHaskellInt b >>= \b -> case a of
  OInt  a -> return (OInt  (shift a (fn b)))
  OWord a -> return (OWord (shift a (fn b)))
  OLong a -> return (OLong (shift a (fn b)))
  _       -> mzero

evalSubscript :: Object -> Object -> BuiltinOp
evalSubscript a b = case a of
  OArray  a -> fmap fromIntegral (asInteger b) >>= \b ->
    if inRange (bounds a) b then return (a!b) else pfail (ustr "array index out of bounds")
  OList   a -> asHaskellInt b >>= \b ->
    let err = pfail (ustr "list index out of bounds")
        ax  = drop b a
    in  if b<0 then err else if null ax then err else return (OList ax)
  OIntMap a -> asHaskellInt b >>= \b -> case I.lookup b a of
    Nothing -> pfail (ustr "no item at index requested of intmap")
    Just  b -> return b
  ODict   a -> msum $
    [ do  asStringNoConvert b >>= \b -> case M.lookup b a of
            Nothing -> pfail (ustr (show b++" is not defined in dict"))
            Just  b -> return b
    , do  asReference b >>= \b -> case b of
            LocalRef  b -> case M.lookup b a of
              Nothing -> pfail (ustr (show b++" is not defined in dict"))
              Just  b -> return b
            GlobalRef bx -> loop [] a bx where
              err = pfail (ustr (show b++" is not defined in dict"))
              loop zx a bx = case bx of
                []   -> return (ODict a)
                b:bx -> case M.lookup b a of
                  Nothing -> err
                  Just  a -> case a of
                    ODict a -> loop (zx++[b]) a bx
                    _       ->
                      if null bx
                        then return a
                        else pfail (ustr (show (GlobalRef zx)++" does not point to dict object"))
    ]
  OTree   a -> msum $ 
    [ asStringNoConvert b >>= \b -> done (T.lookup [b] a)
    , asReference b >>= \b -> case b of
        LocalRef  b  -> done (T.lookup [b] a)
        GlobalRef bx -> done (T.lookup bx  a)
    ] where
        done a = case a of
          Nothing -> pfail (ustr (show b++" is not defined in struct"))
          Just  a -> return a
  _         -> mzero

eval_SHR :: Object -> Object -> BuiltinOp
eval_SHR = evalShift negate

eval_SHL :: Object -> Object -> BuiltinOp
eval_SHL = evalShift id

eval_DOT :: Object -> Object -> BuiltinOp
eval_DOT a b = asReference a >>= \a -> asReference b >>= \b -> case appendReferences a b of
  Nothing -> pfail (ustr (show b++" cannot be appended to "++show a))
  Just  a -> return (ORef a)

eval_NEG :: Object -> BuiltinOp
eval_NEG o = case o of
  OWord     o -> return $
    let n = negate (toInteger o)
    in  if n < toInteger (minBound::T_int)
           then  OLong n
           else  OInt (fromIntegral n)
  OInt      o -> return $ OInt      (negate o)
  OLong     o -> return $ OLong     (negate o)
  ODiffTime o -> return $ ODiffTime (negate o)
  OFloat    o -> return $ OFloat    (negate o)
  ORatio    o -> return $ ORatio    (negate o)
  OComplex  o -> return $ OComplex  (negate o)
  _           -> mzero

eval_INVB :: Object -> BuiltinOp
eval_INVB o = case o of
  OWord o -> return $ OWord (complement o)
  OInt  o -> return $ OInt  (complement o)
  OLong o -> return $ OLong (complement o)
  _       -> mzero

eval_REF :: Object -> BuiltinOp
eval_REF r = case r of
  ORef    r -> return (ORef (MetaRef r))
  OString s -> return (ORef (LocalRef s))
  _         -> mzero

eval_DEREF :: Object -> BuiltinOp
eval_DEREF r = case r of
  ORef (MetaRef r) -> return (ORef r)
  ORef r           -> return (ORef r)
  _                -> mzero

eval_NOT :: Object -> BuiltinOp
eval_NOT = return . boolToObj . testNull

-- | Traverse the entire object, returning a list of all 'Dao.Object.OString' elements.
extractStringElems :: Object -> [UStr]
extractStringElems o = case o of
  OString  o   -> [o]
  OList    o   -> concatMap extractStringElems o
  OSet     o   -> concatMap extractStringElems (S.elems o)
  OArray   o   -> concatMap extractStringElems (elems o)
  ODict    o   -> concatMap extractStringElems (M.elems o)
  OIntMap  o   -> concatMap extractStringElems (I.elems o)
  OTree    o   -> concatMap extractStringElems (T.elems o)
  OPair (a, b) -> concatMap extractStringElems [a, b]
  _            -> []

prefixOps :: Array ArithOp (Object -> BuiltinOp)
prefixOps = let o = (,) in array (REF, SUB) $
  [ o REF   eval_REF
  , o DEREF eval_DEREF
  , o INVB  eval_INVB
  , o NOT   eval_NOT
  , o NEG   eval_NEG
  ]

infixOps :: Array ArithOp (Object -> Object -> BuiltinOp)
infixOps = let o = (,) in array (POINT, POW) $
  [ o POINT evalSubscript
  , o DOT   eval_DOT
  , o OR    (evalBooleans (||))
  , o AND   (evalBooleans (&&))
  , o ORB   eval_ORB
  , o ANDB  eval_ANDB
  , o XORB  eval_XORB
  , o SHL   eval_SHL
  , o SHR   eval_SHR
  , o ADD   eval_ADD
  , o SUB   eval_SUB
  , o MULT  eval_MULT
  , o DIV   eval_DIV
  , o MOD   eval_MOD
  , o POW   eval_POW
  ]

updatingOps :: Array UpdateOp (Object -> Object -> BuiltinOp)
updatingOps = let o = (,) in array (minBound, maxBound) $
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

----------------------------------------------------------------------------------------------------

requireAllStringArgs :: [Object] -> ExecScript [UStr]
requireAllStringArgs ox = case mapM check (zip (iterate (+1) 0) ox) of
  OK      obj -> return obj
  Backtrack   -> procErr $ OList [OString (ustr "all input parameters must be strings")]
  PFail i msg -> procErr $ OList [OString msg, OWord i, OString (ustr "is not a string")]
  where
    check (i, o) = case o of
      OString o -> return o
      _         -> PFail i (ustr "requires string parameter, param number")

recurseGetAllStringArgs :: [Object] -> ExecScript [UStr]
recurseGetAllStringArgs ox = catch (loop [0] [] ox) where
  loop ixs@(i:ix) zx ox = case ox of
    []                -> return zx
    OString    o : ox -> loop (i+1:ix) (zx++[o]) ox
    OList      o : ox -> next ixs zx          o  ox
    OSet       o : ox -> next ixs zx (S.elems o) ox
    OArray     o : ox -> next ixs zx   (elems o) ox
    ODict      o : ox -> next ixs zx (M.elems o) ox
    OIntMap    o : ox -> next ixs zx (I.elems o) ox
    OPair  (a,b) : ox -> next ixs zx      [a, b] ox
    _                 -> PFail ix (ustr "is not a string value")
  next (i:ix) zx nx ox = loop (0:i:ix) zx nx >>= \zx -> loop (i+1:ix) zx ox
  catch ox = case ox of
    PFail ix msg -> procErr $ OList $
      [OString (ustr "function parameter"), OList (map OWord (reverse ix)), OString msg]
    Backtrack   -> return []
    OK       ox -> return ox

builtin_print :: DaoFunc
builtin_print = DaoFunc $ \ox_ -> do
  let ox = flip map ox_ $ \o -> case o of
        OString o -> o
        o         -> ustr (showObj 0 o)
  lift (lift (mapM_ (putStrLn . uchars) ox))
  return (OList (map OString ox))

sendStringsToPrograms :: Bool -> [Name] -> [UStr] -> ExecScript ()
sendStringsToPrograms permissive names strings = do
  xunit <- ask
  index <- execScriptRun (dReadMVar $loc (pathIndex (parentRuntime xunit)))
  let lookup notFound nonPrograms ok namex = case namex of
        []         -> (notFound, nonPrograms, ok)
        name:namex -> case M.lookup name index of
          Nothing                 -> lookup (notFound++[name]) nonPrograms ok namex
          Just (ProgramFile xunit) -> lookup notFound  nonPrograms (ok++[(name, xunit)]) namex
          Just  _                 -> lookup notFound (nonPrograms++[name]) ok namex
      (notFound, nonPrograms, found) = lookup [] [] [] names
      errMsg = OList $
          if null notFound
            then []
            else [OString (ustr "could not find program files:"), OList (map OString notFound)]
       ++ if null nonPrograms
            then []
            else [OString (ustr "not program files:"), OList (map OString nonPrograms)]
       ++ if null found
            then [OString (ustr "no files found"), OList (map OString names)]
            else []
       ++ [ OString (ustr "cannot execute strings"), OList (map OString strings) ]
  if null found || not permissive && not (null notFound && null nonPrograms)
    then  procErr errMsg
    else  forM_ found $ \ (name, xunit) -> execScriptRun $
            dModifyMVar_ $loc (recursiveInput xunit) (return . (++strings))

builtin_do :: DaoFunc
builtin_do = DaoFunc $ \ox -> do
  xunit <- ask
  let currentProg = [programModuleName xunit]
      isProgRef r = case r of
        ORef (ProgramRef a _) -> return a
        _ -> procErr $ OList $
          [ OString $ ustr $
              "first argument to \"do\" function must be all strings, or all file references"
          , r
          ]
  (selectFiles, execStrings) <- case ox of
    [OString file , OList strs] -> return ([file], strs )
    [OList   files, OList strs] -> mapM isProgRef files >>= \files -> return (files , strs )
    [OString file , str       ] -> return ([file], [str])
    [OList   files, str       ] -> mapM isProgRef files >>= \files -> return (files , [str])
    [OString str ] -> return (currentProg, [OString str])
    [OList   strs] -> return (currentProg, strs)
    _              -> procErr $ OList $
      OString (ustr "require query strings as parameters to \"do\" function, but received") : ox
  execStrings <- requireAllStringArgs execStrings
  return (OList (map OString execStrings))

builtin_join :: DaoFunc
builtin_join = DaoFunc $ \ox -> do
  ox <- recurseGetAllStringArgs ox
  return (OString (ustr (concatMap uchars ox)))

-- | The map that contains the built-in functions that are used to initialize every
-- 'Dao.Object.ExecUnit'.
initBuiltinFuncs :: M.Map Name DaoFunc
initBuiltinFuncs = let o a b = (ustr a, b) in M.fromList $
  [ o "print" builtin_print
  , o "do"    builtin_do
  , o "join"  builtin_join
  ]

----------------------------------------------------------------------------------------------------

-- | If an 'Dao.Object.Object' value is a 'Dao.Object.Reference' (constructed with
-- 'Dao.Object.ORef'), then the reference is looked up using 'readReference'. Otherwise, the object
-- value is returned. This is used to evaluate every reference in an 'Dao.Object.ObjectExpr'.
evalObjectRef :: Object -> ExecScript Object
evalObjectRef obj = case obj of
  ORef (MetaRef o) -> return (ORef o)
  ORef ref         -> readReference ref >>= \o -> case o of
    Nothing  -> procErr $ OList [obj, OString (ustr "undefined reference")]
    Just obj -> return obj
  obj              -> return obj

-- | Will return any value from the 'Dao.Object.ExecUnit' environment associated with a
-- 'Dao.Object.Reference'.
readReference :: Reference -> ExecScript (Maybe Object)
readReference ref = case ref of
  IntRef     i     -> fmap Just (evalIntRef i)
  LocalRef   nm    -> localVarLookup nm
  QTimeRef   ref   -> qTimeVarLookup ref
  StaticRef  ref   -> staticVarLookup ref
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
  let updateRef :: DMVar a -> (a -> Run (a, FlowCtrl Object)) -> ExecScript (Maybe Object)
      updateRef dmvar runUpdate = fmap Just (execScriptRun (dModifyMVar $loc dmvar runUpdate) >>= joinFlowCtrl)
      execUpdate :: ref -> a -> Maybe Object -> ((Maybe Object -> Maybe Object) -> a) -> Run (a, FlowCtrl Object)
      execUpdate ref store lkup upd = do
        err <- flip runExecScript xunit $ modf lkup
        case err of
          FlowOK   val -> return (upd (const val), FlowOK (fromMaybe ONull val))
          FlowReturn val -> return (upd (const (Just val)), FlowReturn val)
          FlowErr  err -> return (store, FlowErr err)
  case ref of
    IntRef     i          -> error "cannot assign values to a pattern-matched reference"
    LocalRef   ref        -> localVarLookup ref >>= modf >>= localVarUpdate ref . const
    GlobalRef  ref        -> globalVarUpdate ref modf
    QTimeRef   ref        -> qTimeVarUpdate ref modf
    StaticRef  ref        -> staticVarUpdate ref modf
    ProgramRef progID ref -> error "TODO: you haven't yet defined update behavior for Program references"
    FileRef    path   ref -> error "TODO: you haven't yet defined update behavior for File references"
    MetaRef    _          -> error "cannot assign values to a meta-reference"

-- | Retrieve a 'Dao.Object.CheckFunc' function from one of many possible places in the
-- 'Dao.Object.ExecUnit'. Every function call that occurs during execution of the Dao script will
-- use this Haskell function to seek the correct Dao function to use. Pass an error message to be
-- reported if the lookup fails. The order of lookup is: this module's 'Dao.Object.Subroutine's,
-- the 'Dao.Object.Subroutine's of each imported module (from first to last listed import), and
-- finally the built-in functions provided by the 'Dao.Object.Runtime'
lookupFunction :: String -> Name -> ExecScript [Subroutine]
lookupFunction msg op = do
  xunit <- ask
  let toplevs xunit = lift (fmap (M.lookup op) (dReadMVar $loc (toplevelFuncs xunit)))
      lkup p = case p of
        ProgramFile xunit -> toplevs xunit
        _                 -> return Nothing
  funcs <- fmap (concatMap maybeToList) (sequence (toplevs xunit : map lkup (importsTable xunit)))
  if null funcs
    then  objectError (OString op) $ "undefined "++msg++" ("++uchars op++")"
    else  return (concat funcs)

----------------------------------------------------------------------------------------------------

-- $ErrorReporting
-- The 'Procedural' is a continuation monad that can evaluate to an error message without evaluating
-- to "bottom". The error message is any value of type 'Dao.Object.Object'. These functions provide
-- a simplified method for constructing error 'Dao.Object.Object's.

simpleError :: String -> ExecScript a
simpleError msg = procErr (OString (ustr msg))

-- | Like 'Dao.Object.Data.objectError', but simply constructs a 'Dao.Object.Monad.FlowErr' value
-- that can be returned in an inner monad that has been lifted into a 'Dao.Object.Monad.Procedural'
-- monad.
objectErrorCE :: Object -> String -> FlowCtrl a
objectErrorCE obj msg = FlowErr (OPair (OString (ustr msg), obj))

typeError :: Object -> String -> String -> ExecScript a
typeError o cons expect = objectError (OType (objType o)) (cons++" must be of type "++expect)

derefError :: Reference -> ExecScript a
derefError ref = objectError (ORef ref) "undefined reference"

-- | Evaluate to 'procErr' if the given 'PValue' is 'Backtrack' or 'PFail'. You must pass a
-- 'Prelude.String' as the message to be used when the given 'PValue' is 'Backtrack'. You can also
-- pass a list of 'Dao.Object.Object's that you are checking, these objects will be included in the
-- 'procErr' value.
--     This function should be used for cases when you have converted 'Dao.Object.Object' to a
-- Haskell value, because 'Backtrack' values indicate type exceptions, and 'PFail' values indicate a
-- value error (e.g. out of bounds, or some kind of assert exception), and the messages passed to
-- 'procErr' will indicate this.
checkPValue :: String -> [Object] -> PValue Location a -> ExecScript a
checkPValue altmsg tried pval = case pval of
  OK a         -> return a
  Backtrack    -> procErr $ OList $
    OString (ustr "bad data type") : (if null altmsg then [] else [OString (ustr altmsg)]) ++ tried
  PFail lc msg -> procErr $ OList $
    OString (ustr "bad data value") :
      (if null altmsg then [] else [OString (ustr altmsg)]) ++ OString msg : tried

----------------------------------------------------------------------------------------------------

-- | Convert a single 'ScriptExpr' into a function of value @'ExecScript' 'Dao.Object.Object'@.
execScriptExpr :: Com ScriptExpr -> ExecScript ()
execScriptExpr script = case unComment script of
  EvalObject  o  _             lc  -> unless (isNO_OP o) (void (evalObject o))
  IfThenElse  _  ifn  thn  els lc  -> nestedExecStack T.Void $ do
    ifn <- evalObject ifn
    case ifn of
      ORef o -> do
        true <- fmap isJust (readReference o)
        execScriptBlock (unComment (if true then thn else els))
      o      -> execScriptBlock (unComment (if objToBool o then thn else els))
  TryCatch    try  name catch  lc  -> do
    ce <- procCatch (nestedExecStack T.Void (execScriptBlock (unComment try)))
    void $ case ce of
      FlowErr o -> nestedExecStack (T.insert [unComment name] o T.Void) (execScriptBlock catch)
      ce        -> joinFlowCtrl ce
  ForLoop    varName inObj thn lc  -> nestedExecStack T.Void $ do
    inObj <- evalObject (unComment inObj)
    let block thn = if null thn then return True else scrpExpr (head thn) >> block (tail thn)
        ctrlfn ifn thn = do
          ifn <- evalObject ifn
          case ifn of
            ONull -> return (not thn)
            _     -> return thn
        scrpExpr expr = case unComment expr of
          ContinueExpr a _  ifn lc -> ctrlfn (unComment ifn) a
          _                        -> execScriptExpr expr >> return True
        loop thn name ix = case ix of
          []   -> return ()
          i:ix -> localVarDefine name i >> block thn >>= flip when (loop thn name ix)
        inObjType = OType (objType inObj)
    case asList inObj of
      OK        ox  -> loop thn (unComment varName) ox
      Backtrack     -> objectError inObj "cannot be represented as list"
      PFail loc msg -> objectError inObj (uchars msg) -- TODO: also report the location of the failure.
  ContinueExpr a    _    _     lc  -> simpleError $
    '"':(if a then "continue" else "break")++"\" expression is not within a \"for\" loop"
  ReturnExpr   a    obj        lc  -> evalObject (unComment obj) >>= \obj -> (if a then procReturn else procErr) obj
  WithDoc      lval thn        lc  -> nestedExecStack T.Void $ do
    lval <- evalObject (unComment lval)
    let setBranch ref xunit = return (xunit{currentBranch = ref})
        setFile path xunit = do
          file <- lift (fmap (M.lookup path) (dReadMVar $loc (execOpenFiles xunit)))
          case file of
            Nothing  -> procErr $ OList $ map OString $
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

-- | 'Dao.Object.ObjectExpr's can be evaluated anywhere in a 'Dao.Object.Script'. However, a
-- 'Dao.Object.ObjectExpr' is evaluated as a lone command expression, and not assigned to any
-- variables, and do not have any other side-effects, then evaluating an object is a no-op. This
-- function checks the kind of 'Dao.Object.ObjectExpr' and evaluates to 'True' if it is impossible
-- for an expression of this kind to produce any side effects. Otherwise, this function evaluates to
-- 'False', which indicates it is OK to evaluate the expression and disgard the resultant 'Object'.
isNO_OP :: ObjectExpr -> Bool
isNO_OP o = case o of
  Literal      _     _ -> True
  ParenExpr    _ o   _ -> isNO_OP (unComment o)
  ArraySubExpr _ _ _ _ -> True
  DictExpr     _ _ _ _ -> True
  ArrayExpr    _ _   _ -> True
  LambdaExpr   _ _   _ -> True
  _                    -> False

called_nonfunction_object :: String -> Object -> ExecScript e
called_nonfunction_object op obj =
  typeError obj (show op++" was called as a function but is not a function type, ") $
    show ScriptType++" or "++show RuleType

-- | Evaluate an 'ObjectExpr' to an 'Dao.Object.Object' value, and does not de-reference objects of
-- type 'Dao.Object.ORef'
evalObject :: ObjectExpr -> ExecScript Object
evalObject obj = case obj of
  VoidExpr                      -> return ONull
  Literal       o            lc -> return o
  AssignExpr    nm  op_ expr lc -> do
    let op = unComment op_
    nm   <- evalObject nm
    nm   <- checkPValue ("left-hand side of "++show op) [nm] (asReference nm)
    expr <- evalObject expr >>= evalObjectRef
    fmap (fromMaybe ONull) $ updateReference nm $ \maybeObj -> case maybeObj of
      Nothing  -> case op of
        UCONST -> return (Just expr)
        _      -> procErr $ OList $ [OString $ ustr "undefined refence", ORef nm]
      Just obj -> fmap Just $ checkPValue "assignment expression" [obj, expr] $ (updatingOps!op) obj expr
  FuncCall   op  _  args     lc -> do -- a built-in function call
    bif  <- fmap builtinFuncs ask
    args <- evalArgsList args
    case M.lookup op bif of
      Nothing -> do
        fn   <- lookupFunction "function call" op
        ~obj <- mapM (runSubroutine args) fn
        case msum obj of
          Just obj -> return obj
          Nothing  -> procErr $ OList $
            [OString (ustr "incorrect parameters passed to function") , OString op, OList args]
      Just fn -> daoForeignCall fn args
  LambdaCall ref  args       lc -> do
    fn   <- evalObject (unComment ref) >>= evalObjectRef
    case fn of
      OScript fn -> do
        args <- evalArgsList args
        obj <- runSubroutine args fn
        case obj of
          Just obj -> return obj
          Nothing  -> procErr $ OList $
            [OString (ustr "incorrect parameters passed to lambda call"), OScript fn, OList args]
      ORule   fn -> execRuleCall  args fn
      _          -> called_nonfunction_object (showObjectExpr 0 ref) fn
  ParenExpr     _     o      lc -> evalObject (unComment o)
  ArraySubExpr  o  _  i      lc -> do
    o <- evalObject o
    i <- evalObject (unComment i)
    case evalSubscript o i of
      OK          a -> return a
      PFail loc msg -> procErr (OString msg)
      Backtrack     -> procErr (OList [i, OString (ustr "cannot be used as index of"), o])
  Equation   left  op_ right lc -> do
    let op = unComment op_
    left  <- evalObject left
    right <- evalObject right
    (left, right) <- case op of
      DOT   -> return (left, right)
      POINT -> liftM2 (,) (evalObjectRef left) (return right)
      _     -> liftM2 (,) (evalObjectRef left) (evalObjectRef right)
    case (infixOps!op) left right of
      OK result -> return result
      Backtrack -> procErr $ OList $
        [OString $ ustr (show op), OString $ ustr "cannot operate on objects of type", left, right]
      PFail lc msg -> procErr $ OList [OString msg]
  DictExpr   cons  _  args   lc -> do
    let loop insfn getObjVal map argx = case argx of
          []       -> return map
          arg:argx -> case unComment arg of
            AssignExpr ixObj op_ new lc -> do
              let op = unComment op_
              ixObj <- evalObject ixObj >>= evalObjectRef
              ixVal <- checkPValue (show cons++" assignment expression") [ixObj] (getObjVal ixObj)
              new   <- evalObject new >>= evalObjectRef
              map   <- insfn map ixObj ixVal op new
              loop insfn getObjVal map argx
            _ -> error "dictionary constructor contains an expression that is not an assignment"
        assign lookup insert map ixObj ixVal op new = case lookup ixVal map of
          Nothing  -> case op of
            UCONST -> return (insert ixVal new map)
            op     -> procErr $ OList [OString (ustr ("undefined left-hand side of "++show op)), ixObj]
          Just old -> case op of
            UCONST -> procErr $ OList [OString (ustr ("twice defined left-hand side "++show op)), ixObj]
            op     -> do
              new <- checkPValue (show cons++" assignment expression "++show op) [ixObj, old, new] $ (updatingOps!op) old new
              return (insert ixVal new map)
        intmap = assign I.lookup I.insert
        dict   = assign M.lookup M.insert
    case () of
      () | cons == ustr "list"   -> fmap OList (mapM (evalObject . unComment) args)
      () | cons == ustr "dict"   -> fmap ODict   (loop dict   asStringNoConvert M.empty args)
      () | cons == ustr "intmap" -> fmap OIntMap (loop intmap asHaskellInt      I.empty args)
      _ -> error ("INTERNAL ERROR: unknown dictionary declaration "++show cons)
  ArrayExpr  rang  ox        lc -> do
    case unComment rang of
      (_ : _ : _ : _) ->
        simpleError "internal error: array range expression has more than 2 arguments"
      [lo, hi] -> do
        lo <- evalObject (unComment lo)
        hi <- evalObject (unComment hi)
        case (lo, hi) of
          (OInt lo, OInt hi) -> do
            (lo, hi) <- return (if lo<hi then (lo,hi) else (hi,lo))
            ox <- mapM (evalObject . unComment) ox
            return (OArray (listArray (lo, hi) ox))
          _ -> objectError (OPair (OType (objType lo), OType (objType hi))) $
                   "range specified to an "++show ArrayType
                 ++" constructor must evaluate to two "++show IntType++"s"
      _ -> simpleError "internal error: array range expression has fewer than 2 arguments"
  LambdaExpr argv  code      lc -> do
    let argv' = map (flip ObjLabel ObjAny1 . unComment) (unComment argv)
    exe <- lift (setupExecutable (Com code))
    return $ OScript $
      Subroutine{argsPattern = argv', subSourceCode = code, getSubExecutable = exe}

evalArgsList :: [Com ObjectExpr] -> ExecScript [Object]
evalArgsList = mapM ((evalObject >=> evalObjectRef) . unComment)

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
-- 'Dao.Object.Monad.procErr' if it access is prohibited.
verifyRequirement :: Name -> a -> ExecScript a
verifyRequirement nm a = return a -- TODO: the rest of this function.

-- | Checks if this ExecUnit is allowed to import the file requested by an "import" statement
-- attribute. Returns any value you pass as the second parameter, throws a
-- 'Dao.Object.Monad.procErr'
verifyImport :: Name -> a -> ExecScript a
verifyImport nm a = return a -- TODO: the rest of this function.

-- | When the 'programFromSource' is scanning through a 'Dao.Object.SourceCode' object, it first
-- constructs an 'IntermediateProgram', which contains no 'Dao.Debug.DMVar's. Once all the data
-- structures are in place, a 'Dao.Object.CachedProgram' is constructed from this intermediate
-- representation.
data IntermediateProgram
  = IntermediateProgram
    { inmpg_programImports    :: [UStr]
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
  { inmpg_programImports    = []
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

initProgram :: IntermediateProgram -> ExecScript ExecUnit
initProgram inmpg = do
  xunit <- ask
  let rules = ruleSet xunit
  pre   <- lift (mapM setupExecutable (inmpg_preExecScript  inmpg))
  post  <- lift (mapM setupExecutable (inmpg_postExecScript inmpg))
  inEvalDoModifyUnlocked_ (globalData xunit) (return . const (inmpg_globalData inmpg))
  return $
    xunit
    { programImports    = inmpg_programImports    inmpg
    , constructScript   = map unComment (inmpg_constructScript inmpg)
    , destructScript    = map unComment (inmpg_destructScript  inmpg)
    , requiredBuiltins  = inmpg_requiredBuiltins  inmpg
    , programAttributes = M.empty
    , preExecScript     = pre
    , programTokenizer  = return . tokens . uchars
    , programComparator = (==)
    , postExecScript    = post
    , ruleSet           = rules
    }

-- | To parse a program, use 'Dao.Object.Parsers.source' and pass the resulting
-- 'Dao.Object.SourceCode' object to this funtion. It is in the 'ExecScript' monad because it needs
-- to evaluate 'Dao.ObjectObject's defined in the top-level of the source code, which requires
-- 'evalObject'.
-- Attributes in Dao scripts are of the form:
--     a.b.C.like.name  dot.separated.value;
-- The three built-in attributes are "requires", "string.tokenizer" and "string.compare". The
-- allowed attrubites can be extended by passing a call-back predicate which modifies the given
-- program, or returns Nothing to reject the program. If you are not sure what to pass, just pass
-- @(\ _ _ _ -> return Nothing)@ which always rejects the program. This predicate will only be
-- called if the attribute is not allowed by the minimal Dao system.
--     This is a 'Control.Monad.Reader.ReaderT' monad with an 'ExecUnit' state. This 'ExecUnit'
-- state cannot be updated (otherwise it would be a 'Control.Monad.State.StateT') so this function
-- evaluates to an 'Dao.Object.ExecUnit' that is the 'Control.Monad.Reader.ReaderT' state which has
-- been updated by evaluating the intermediate program. The function which evaluated this function
-- must then update the 'Dao.Object.ExecUnit' it used to evaluate this
-- 'Control.Monad.Reader.ReaderT' monad.
programFromSource
  :: TreeResource
      -- ^ the global variables initialized at the top level of the program file are stored here.
  -> (Name -> UStr -> IntermediateProgram -> ExecScript Bool)
      -- ^ a callback to check attributes written into the script. If the attribute is bogus, Return
      -- False to throw a generic error, or throw your own FlowErr. Otherwise, return True.
  -> SourceCode
      -- ^ the script file to use
  -> ExecScript ExecUnit
programFromSource globalResource checkAttribute script =
  execStateT (mapM_ foldDirectives (unComment (directives script))) initIntermediateProgram >>= initProgram
  where
    err lst = lift $ procErr $ OList $ map OString $ (sourceFullPath script : lst)
    attrib req nm getRuntime putProg = do
      runtime <- lift $ fmap parentRuntime ask
      let item = M.lookup nm (getRuntime runtime)
      case item of
        Just item -> modify (putProg item)
        Nothing   -> err [req, ustr "attribute", nm, ustr "is not available"]
    foldDirectives directive = case unComment directive of
      Attribute  req nm lc -> ask >>= \xunit -> do
        let setName = unComment nm
            runtime = parentRuntime xunit
            builtins = M.lookup setName $ functionSets runtime
        case unComment req of
          req | req==ustr "import"           -> do
            lift $ verifyImport setName () -- TODO: verifyImport will evaluate to a FlowErr if the import fails.
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
      ToplevelDefine name obj lc -> do
        obj <- lift $ evalObject (unComment obj)
        modify (\p -> p{inmpg_globalData = T.insert (unComment name) obj (inmpg_globalData p)})
      TopRuleExpr rule' lc -> modify (\p -> p{inmpg_ruleSet = foldl fol (inmpg_ruleSet p) rulePat}) where
        rule    = unComment rule'
        rulePat = map unComment (unComment (rulePattern rule))
        fol tre pat = T.merge T.union (++) tre (toTree pat [ruleAction rule])
      SetupExpr    scrp lc -> modify (\p -> p{inmpg_constructScript = inmpg_constructScript p ++ [scrp]})
      TakedownExpr scrp lc -> modify (\p -> p{inmpg_destructScript  = inmpg_destructScript  p ++ [scrp]})
      BeginExpr    scrp lc -> modify (\p -> p{inmpg_preExecScript   = inmpg_preExecScript   p ++ [scrp]})
      EndExpr      scrp lc -> modify (\p -> p{inmpg_postExecScript  = inmpg_postExecScript  p ++ [scrp]})
      ToplevelFunc nm argv code lc -> lift $ do
        xunit <- ask
        exe <- execScriptRun (setupExecutable code)
        let sub =
              Subroutine
              { argsPattern  = map (flip ObjLabel ObjAny1 . unComment) argv
              , subSourceCode  = unComment code
              , getSubExecutable = exe
              }
        execScriptRun $ do
          let name = unComment nm
          dModifyMVar_ $loc (toplevelFuncs xunit) $ return .
            M.alter (\funcs -> mplus (fmap (++[sub]) funcs) (return [sub])) name

----------------------------------------------------------------------------------------------------

-- | Blocks until every thread in the guven@'DMVar' ('Data.Set.Set' 'Dao.Debug.DThread')@ has
-- completed evaluation.
execWaitThreadLoop :: MLoc -> String -> DMVar (S.Set DThread) -> DMVar DThread -> Run ()
execWaitThreadLoop lc msg running wait = dStack lc msg $ do
  threads <- dReadMVar $loc running
  if S.null threads then return () else loop
  where 
    loop = do
      thread <- dTakeMVar $loc wait
      isDone <- dModifyMVar $loc running $ \threads_ -> do
        let threads = S.delete thread threads_
        return (threads, S.null threads)
      if isDone then return () else loop

-- | Evaluate an 'Dao.Object.Action' in the current thread.
execAction :: Action -> Run ()
execAction action = dStack $loc ("execAction for ("++show (actionPattern action)++")") $!
  void (runExecScript (runExecutable T.Void (currentExecutable xunit)) xunit >>= liftIO . evaluate)
  where
    xunit =
      (actionExecUnit action)
      { currentQuery      = Just (actionQuery   action)
      , currentPattern    = Just (actionPattern action)
      , currentMatch      = Just (actionMatch   action)
      , currentExecutable = actionExecutable action
      }

-- | This is the most important algorithm of the Dao system, in that it matches strings to all
-- rule-patterns in the program that is associated with the 'Dao.Object.ExecUnit', and it dispatches
-- execution of the rule-actions associated with matched patterns. This is the function that runs in
-- the thread which manages all the other threads that are launched in response to a matching input
-- string. You could just run this loop in the current thread, if you only have one 'ExecUnit'.
execInputStringsLoop :: ExecUnit -> Run ()
execInputStringsLoop xunit = dCatch $loc start handler where
  running = runningThreads xunit
  start = do
    runtime <- ask
    dNewEmptyMVar $loc "execInputStringsLoop.waitChild" >>= loop
    dMyThreadId >>= dPutMVar $loc (waitExecUnitsMVar runtime)
  loop waitChild = do
    -- (1) Get the next input string. Also nub the list of queued input strings.
    instr <- dModifyMVar $loc (recursiveInput xunit) $ \ax -> return $ case nub ax of
      []   -> ([], Nothing)
      a:ax -> (ax, Just a)
    case instr of
      Nothing    -> return ()
      Just instr -> dStack $loc ("execInputString "++show instr) $ do
        -- (2) Run "BEGIN" scripts.
        runExecs $loc "preExecScript" preExecScript waitChild
        -- (3) Match input string to all patterns, get resulting actions.
        matched <- matchStringToProgram instr xunit
        dMessage $loc ("matched "++show (length matched)++" rules")
        -- (4) Run 'execPatternMatchExecutable' for every matching item.
        putThreads $ forM matched $ \act -> do
          dFork forkIO $loc "execInputStringsLoop.loop" (execAction act)
        let msg = "execInputStringLoop.loop.(execWaitThreadLoop execPatternMatchExecutable)"
        execWaitThreadLoop $loc msg running waitChild
        -- (5) Run "END" scripts.
        runExecs $loc "postExecScript" postExecScript waitChild
        -- (6) Run the next string.
        loop waitChild
  putThreads mkThreads = dModifyMVar_ $loc (runningThreads xunit) $ \threads -> do
    -- lock the 'runningThreads' mvar when creating threads so if the parent thread tries to
    -- kill all running threads, it won't be able to get a list of running threads until this mvar
    -- is released, which will be after all threads have already been started.
    newThreads <- mkThreads
    return (S.union (S.fromList newThreads) threads)
  runExecs lc msg_ select waitChild = do
    putThreads $ forM (select xunit) $ \exec ->
      dFork forkIO $loc "execInputStringsLoop.runExecs" $
        void $ flip runExecScript xunit $ runExecutable T.Void exec
    let msg = "execInputStringLoop.runExecs.(execWaitThreadLoop "++msg_++")"
    execWaitThreadLoop lc msg running waitChild
  handler :: SomeException -> Run ()
  handler (SomeException e) = dModifyMVar_ $loc running $ \threads -> do
    mapM_ (\thread -> dThrowTo $loc thread e) (S.elems threads)
    return S.empty

-- | Launch a new thread for an 'Dao.Object.ExecUnit'. You can launch several threads for a single
-- 'Dao.Object.ExecUnit', but the threads will only share the work, there is no particular advantage
-- to calling this function more than once per 'Dao.Object.ExecUnit'. Returns the
-- 'Dao.Debug.DThread' of the thread created.
startExecUnitThread :: ExecUnit -> Run DThread
startExecUnitThread xunit = dFork forkIO $loc "startExecUnitThread" (execInputStringsLoop xunit)

-- | Place a string into the queues and then start one thread for each of the 'Dao.Object.ExecUnit's
-- specified. The threads are created but not registered into the 'Dao.Object.Runtime'
-- 'Dao.Object.runningExecThreads' field. To do this, evaluate this function as a parameter to
-- 'daoRegisterThreads'.
runStringAgainstExecUnits :: UStr -> [ExecUnit] -> Run (S.Set DThread)
runStringAgainstExecUnits inputString xunits = dStack $loc "runStringsAgainstExecUnits" $ do
  runtime <- ask
  forM_ xunits $ \xunit ->
    dModifyMVar_ $loc (recursiveInput xunit) (return . (++[inputString]))
  fmap (S.fromList) (mapM startExecUnitThread xunits)

-- | Registers the threads created by 'runStringAgainstExecUnits' into the
-- 'Dao.Object.runningExecThreads' field of the 'Dao.Object.Runtime'.
daoRegisterThreads :: Run (S.Set DThread) -> Run ()
daoRegisterThreads makeThreads = do
  runtime <- ask
  dModifyMVar_ $loc (runningExecUnits runtime) $ \threads -> do
    newThreads <- makeThreads
    return (S.union newThreads threads)

-- | This is the main input loop. Pass an input function callback to be called on every loop.
daoInputLoop :: (Run (Maybe UStr)) -> Run ()
daoInputLoop getString = ask >>= loop where
  loop runtime = do
    inputString <- getString
    case inputString of
      Nothing          -> return ()
      Just inputString -> dStack $loc ("(daoInputLoop "++show inputString++")") $ do
        xunits <- fmap (concatMap isProgramFile . M.elems) (dReadMVar $loc (pathIndex runtime))
        daoRegisterThreads (runStringAgainstExecUnits inputString xunits)
        let msg = "daoInputLoop.(wait for ExecUnits to finish)"
        execWaitThreadLoop $loc msg (runningExecUnits runtime) (waitExecUnitsMVar runtime)
        loop runtime

-- | When executing strings against Dao programs (e.g. using 'Dao.Tasks.execInputString'), you often
-- want to execute the string against only a subset of the number of total programs. Pass the
-- logical names of every module you want to execute strings against, and this function will return
-- them. If you pass an empty list, all 'PublicType' modules (in the 'programs' table of the
-- 'Runtime') will be returned. Pass @'Data.Maybe.Just' 'Dao.Evaluator.ExecUnit'@ to allow
-- 'PrivateType' functions to also be selected, however only modules imported by the program
-- associated with that 'ExecUnit' are allowed to be selected.
selectModules :: Maybe ExecUnit -> [Name] -> Run [File]
selectModules xunit names = dStack $loc "selectModules" $ ask >>= \runtime -> case names of
  []    -> do
    ax <- dReadMVar $loc (pathIndex runtime)
    dMessage $loc ("selected modules: "++intercalate ", " (map show (M.keys ax)))
    return (filter (not . null . isProgramFile) (M.elems ax))
  names -> do
    pathTab <- dReadMVar $loc (pathIndex runtime)
    let set msg           = M.fromList . map (\mod -> (mod, error msg))
        request           = set "(selectModules: request files)" names
    imports <- case xunit of
      Nothing    -> return M.empty
      Just xunit -> return $ set "(selectModules: imported files)" $ programImports xunit
    ax <- return $ M.intersection pathTab request
    dMessage $loc ("selected modules:\n"++unlines (map show (M.keys ax)))
    return $ M.elems ax

-- | Given an input string, and a program, return all patterns and associated match results and
-- actions that matched the input string, but do not execute the actions. This is done by tokenizing
-- the input string and matching the tokens to the program using 'Dao.Pattern.matchTree'.
-- NOTE: Rules that have multiple patterns may execute more than once if the input matches more than
-- one of the patterns associated with the rule. *This is not a bug.* Each pattern may produce a
-- different set of match results, it is up to the programmer of the rule to handle situations where
-- the action may execute many times for a single input.
matchStringToProgram :: UStr -> ExecUnit -> Run [Action]
matchStringToProgram instr xunit = dStack $loc "matchStringToProgram" $ do
  let eq = programComparator xunit
      match tox = do
        tree <- dReadMVar $loc (ruleSet xunit)
        dMessage $loc ("match to pattern tree: "++show (map fst (T.assocs tree)))
        fmap concat $ forM (matchTree eq tree tox) $ \ (patn, mtch, execs) ->
          return $ flip map execs $ \exec ->
            Action
            { actionExecUnit   = xunit
            , actionQuery      = instr
            , actionPattern    = patn
            , actionMatch      = mtch
            , actionExecutable = exec
            }
  tox <- runExecScript (programTokenizer xunit instr) xunit
  case tox of
    FlowErr obj -> do
      dModifyMVar_ $loc (uncaughtErrors xunit) $ \objx -> return $ (objx++) $
        [ OList $
            [ obj, OString (ustr "error occured while tokenizing input string")
            , OString instr, OString (ustr "in the program")
            , OString (programModuleName xunit)
            ]
        ]
      return []
    FlowReturn tox -> match (extractStringElems tox)
    FlowOK   tox -> match tox

-- | In the current thread, and using the given 'Runtime' environment, parse an input string as
-- 'Dao.Object.Script' and then evaluate it. This is used for interactive evaluation. The parser
-- used in this function will parse a block of Dao source code, the opening and closing curly-braces
-- are not necessary. Therefore you may enter a semi-colon separated list of commands and all will
-- be executed.
evalScriptString :: ExecUnit -> String -> Run ()
evalScriptString xunit instr = dStack $loc "evalScriptString" $
  void $ flip runExecScript xunit $ nestedExecStack T.Void $ execScriptBlock $
  case fst (runParser parseInteractiveScript instr) of
    Backtrack     -> error "cannot parse expression"
    PFail tok msg -> error ("error: "++uchars msg++show tok)
    OK expr       -> expr

----------------------------------------------------------------------------------------------------
-- src/Dao/Files.hs

-- | Initialize a source code file into the given 'Runtime'. This function checks that the
-- 'Dao.Object.sourceFullPath' is unique in the 'programs' table of the 'Runtime', then evaluates
-- 'initSourceCode' and associates the resulting 'Dao.Evaluator.ExecUnit' with the
-- 'sourceFullPath' in the 'programs' table. Returns the logical "module" name of the script along
-- with an initialized 'Dao.Object.ExecUnit'.
registerSourceCode :: UPath -> SourceCode -> Run (FlowCtrl ExecUnit)
registerSourceCode upath script = dStack $loc "registerSourceCode" $ ask >>= \runtime -> do
  let modName = unComment (sourceModuleName script)
      pathTab = pathIndex runtime
  alreadyLoaded <- fmap (M.lookup upath) (dReadMVar $loc pathTab)
  -- Check to make sure the logical name in the loaded program does not conflict with that
  -- of another loaded previously.
  case alreadyLoaded of
    Just (ProgramFile  f) -> return (FlowOK f)
    Just (DocumentFile f) -> return $ FlowErr $ OList $
      [ OString upath
      , OString (ustr "is already loaded as an \"idea\" file, cannot be loaded as a \"dao\" file")
      ]
    Nothing -> do
      -- Call 'initSourceCode' which creates the 'ExecUnit', then place it in an 'MVar'.
      -- 'initSourceCode' calls 'Dao.Evaluator.programFromSource'.
      xunit <- initSourceCode upath script >>= lift . evaluate
      xunitMVar <- dNewMVar $loc ("ExecUnit("++show upath++")") xunit
      dModifyMVar_ $loc pathTab $ return . M.insert upath (ProgramFile xunit)
      return (FlowOK xunit)

-- | You should not normally need to call evaluate this function, you should use
-- 'registerSourceCode' which will evaluate this function and also place the
-- 'Dao.Object.SourceCode' into the 'programs' table. This function will use
-- 'Dao.Evaluator.programFromSource' to construct a 'Dao.Evaluator.CachedProgram'
-- and then execute the initialization code for that program, that is, use
-- 'Dao.Evaluator.execScriptExpr' to evaluate every 'Dao.Object.ExecScript' in the
-- 'Dao.Object.constructScript'. Returns the 'Dao.Evaluator.ExecUnit' used to initialize the
-- program, and the logical name of the program (determined by the "module" keyword in the source
-- code). You need to pass the 'Runtime' to this function because it needs to initialize a new
-- 'Dao.Evaluator.ExecUnit' with the 'programs' and 'runtimeDocList' but these values are not
-- modified.
initSourceCode :: UPath -> SourceCode -> Run ExecUnit
initSourceCode modName script = ask >>= \runtime -> do
  grsrc <- newTreeResource "Program.globalData" T.Void
  xunit <- initExecUnit runtime modName grsrc
  -- An execution unit is required to load a program, so of course, while a program is being
  -- loaded, the program is not in the program table, and is it's 'currentProgram' is 'Nothing'.
  cachedProg <- runExecScript (programFromSource grsrc (\_ _ _ -> return False) script) xunit
  case cachedProg of
    FlowErr  obj        -> error ("script err: "++showObj 0 obj)
    FlowOK   cachedProg -> do
      -- Run all initializer scripts (denoted with the @SETUP@ rule in the Dao language).
      setupTakedown constructScript xunit
      -- Place the initialized module into the 'Runtime', mapping to the module's handle.
      return xunit
    FlowReturn _          ->
      error "INTERNAL ERROR: source code evaluation returned before completion"

-- | Load a Dao script program from the given file handle, return a 'Dao.Object.SourceCode' object.
-- Specify a path to be used when reporting parsing errors. Does not register the source code into
-- the given runtime.
sourceFromHandle :: UPath -> Handle -> Run (FlowCtrl SourceCode)
sourceFromHandle upath h = lift $ do
  hSetBinaryMode h False
  fmap (FlowOK . loadSourceCode upath) (hGetContents h) >>= evaluate

-- | Load a Dao script program from the given file handle (calls 'sourceFromHandle') and then
-- register it into the 'Dao.Object.Runtime' as with the given 'Dao.String.UPath'.
registerSourceFromHandle :: UPath -> Handle -> Run (FlowCtrl ExecUnit)
registerSourceFromHandle upath h = do
  source <- sourceFromHandle upath h
  case source of
    FlowOK source -> registerSourceCode upath source
    FlowReturn    _ -> error "registerSourceFromHandle: sourceFromHandle evaluated to FlowReturn"
    FlowErr   err -> error ("registerSourceFromHandle: "++show err)

-- | Updates the 'Runtime' to include the Dao source code loaded from the given 'FilePath'. This
-- function tries to load a file in three different attempts: (1) try to load the file as a binary
-- @('Dao.Document.Document' 'Dao.Evaluator.DocData')@ object. (2) Try to load the file as a
-- binary @'Dao.Object.SourceCode'@ object. (3) Treat the file as text (using the current locale set
-- by the system, e.g. @en.UTF-8@) and parse a 'Dao.Object.SourceCode' object using
-- 'Dao.Object.Parsers.source'. If all three methods fail, an error is thrown. Returns
-- the 'TypedFile', although all source code files are returned as 'PrivateType's. Use
-- 'asPublic' to force the type to be a 'PublicType'd file.
loadFilePath :: FilePath -> Run (FlowCtrl File)
loadFilePath path = dontLoadFileTwice (ustr path) $ \upath -> do
  dPutStrErr $loc ("Lookup file path "++show upath)
  h    <- lift (openFile path ReadMode)
  zero <- lift (hGetPosn h)
  enc  <- lift (hGetEncoding h)
  -- First try to load the file as a binary program file, and then try it as a binary data file.
  doc <- catchErrorCall (ideaLoadHandle upath h) :: Run (Either ErrorCall DocResource)
  case doc of
    Right doc -> return (FlowOK (DocumentFile doc))
    Left  _   -> do -- The file does not seem to be a document, try parsing it as a script.
      lift (hSetPosn zero >> hSetEncoding h (fromMaybe localeEncoding enc))
      fmap (fmap ProgramFile) (registerSourceFromHandle upath h)

-- | When a program is loaded, and when it is released, any block of Dao code in the source script
-- that is denoted with the @SETUP@ or @TAKEDOWN@ rules will be executed. This function performs
-- that execution in the current thread.
setupTakedown :: (ExecUnit -> [[Com ScriptExpr]]) -> ExecUnit -> Run ()
setupTakedown select xunit = ask >>= \runtime ->
  forM_ (select xunit) $ \block -> runExecScript (execGuardBlock block) xunit >>= lift . evaluate

