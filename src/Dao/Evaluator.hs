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
    , currentExecJob     = Nothing
    , currentDocument    = Nothing
    , currentProgram     = Nothing
    , currentTask        = error "ExecUnit.currentTask is undefined"
    , currentBranch      = []
    , importsTable       = []
    , execAccessRules    = RestrictFiles (Pattern{getPatUnits = [Wildcard], getPatternLength = 1})
    , builtinFuncs       = initBuiltinFuncs
    , toplevelFuncs      = toplev
    , execHeap           = initGlobalData
    , queryTimeHeap      = qheap
    , referenceCache     = cache
    , execStack          = xstack
    , execOpenFiles      = files
    , recursiveInput     = recurInp
    , uncaughtErrors     = unctErrs
    }

setupExecutable :: Bugged r => Com [Com ScriptExpr] -> ReaderT r IO Executable
setupExecutable scrp = do
  staticRsrc <- lift (newIORef M.empty)
  return $
    Executable
    { staticVars = staticRsrc
    , executable = execScriptBlock (unComment scrp)
    }

runExecutable :: T_tree -> Executable -> ExecScript Object
runExecutable initStack exe =
  localCE (\xunit -> return (xunit{currentTask = (currentTask xunit){taskAction = exe}})) $
    execFuncPushStack initStack (executable exe >> return ONull)

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
    PFail ref msg -> ceError (OPair (OString msg, ORef ref))

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
  flip local (execFuncPushStack T.Void (execScriptBlock (unComment (ruleAction rule)) >> ceReturn ONull)) $
    \xunit -> xunit{currentTask = (currentTask xunit){taskMatch = matchFromList [] (iLength ax) ax}}

-- | Very simply executes every given script item. Does not use catchCEReturn, does not use
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

-- | Push a new empty local-variable context onto the stack. Does NOT 'catchCEReturn', so it can be
-- used to push a new context for every level of nested if/else/for/try/catch statement, or to
-- evaluate a macro, but not a function call. Use 'execFuncPushStack' to perform a function call within
-- a function call.
nestedExecStack :: T_tree -> ExecScript a -> ExecScript a
nestedExecStack init exe = do
  stack <- fmap execStack ask
  execRun (dModifyMVar_ xloc stack (return . stackPush init))
  ce <- catchContErr exe
  execRun (dModifyMVar_ xloc stack (return . stackPop))
  returnContErr ce

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. Use 'catchCEReturn' to prevent return calls from halting execution beyond this
-- function. This is what you should use to perform a Dao function call within a Dao function call.
execFuncPushStack :: T_tree -> ExecScript Object -> ExecScript Object
execFuncPushStack dict exe = do
  stackMVar <- execRun (dNewMVar xloc "execFuncPushStack/ExecUnit.execStack" (Stack [dict]))
  ce <- catchContErr (local (\xunit -> xunit{execStack = stackMVar}) exe)
  case ce of
    CEReturn obj -> return obj
    _            -> returnContErr ce

----------------------------------------------------------------------------------------------------

-- | Used to evaluate an expression like @$1@, retrieves the matched pattern associated with an
-- integer. Specifically, it returns a list of 'Dao.ObjectObject's where each object is an
-- 'Dao.Object.OString' contained at the integer index of the 'Dao.Pattern.matchGaps' of a
-- 'Dao.Pattern.Pattern'.
evalIntRef :: Word -> ExecScript Object
evalIntRef i = do
  task <- fmap currentTask ask
  let oi = OInt (fromIntegral i)
  case task of
    GuardTask _ _ -> do
      objectError oi ("not in pattern match context, cannot evaluate $"++show i)
    RuleTask pat ma act exec -> case matchGaps ma of
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
    Just file@(DocumentFile res) -> inEvalDoReadResource res (currentBranch xunit ++ name)
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
  fmap execStack ask >>= execRun . dReadMVar xloc >>= return . msum . map (T.lookup [sym]) . mapList

-- | Apply an altering function to the map at the top of the local variable stack.
localVarUpdate :: Name -> (Maybe Object -> Maybe Object) -> ExecScript (Maybe Object)
localVarUpdate name alt = ask >>= \xunit -> execRun $
  dModifyMVar xloc (execStack xunit) $ \ax -> case mapList ax of
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
staticVarLookup nm =
  fmap (staticVars . taskAction . currentTask) ask >>= lift . lift . readIORef >>= return . M.lookup nm

staticVarUpdate :: Name -> (Maybe Object -> ExecScript (Maybe Object)) -> ExecScript (Maybe Object)
staticVarUpdate nm upd = fmap (staticVars . taskAction . currentTask) ask >>= \ref ->
  lift (lift (readIORef ref)) >>= return . (M.lookup nm) >>= upd >>= \val ->
    lift (lift (modifyIORef ref (M.update (const val) nm))) >> return val

staticVarDefine :: Name -> Object -> ExecScript (Maybe Object)
staticVarDefine nm obj = staticVarUpdate nm (return . const (Just obj))

staticVarDelete :: Name -> ExecScript (Maybe Object)
staticVarDelete nm = staticVarUpdate nm (return . const Nothing)

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
  Backtrack   -> ceError $ OList [OString (ustr "all input parameters must be strings")]
  PFail i msg -> ceError $ OList [OString msg, OWord i, OString (ustr "is not a string")]
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
    PFail ix msg -> ceError $ OList $
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

execInputString_ = error "TODO: somehow make it so Dao.Files does not depend on Dao.Evaluator"

recursiveExecQuery :: [UStr] -> [UStr] -> ExecScript ()
recursiveExecQuery selectFiles execStrings = case selectFiles of
  [] -> ceError $ OList $
    [OString (ustr "cannot execute \"do\" without specific modules, no default module defined")]
  selectFiles -> do
    xunit <- ask :: ExecScript ExecUnit
    execScriptRun $ do
      -- dModifyMVar_ xloc (recursiveInput xunit) (return . (++ox)) -- is this necessary?
      files <- selectModules (Just xunit) selectFiles
      let job = currentExecJob xunit
      case job of
        Nothing  -> forM_ execStrings (flip (execInputString_ False) files)
        Just job ->
          forM execStrings (makeTasksForInput (map programExecUnit (concatMap isProgramFile files))) >>=
            startTasksForJob job . concat

builtin_do :: DaoFunc
builtin_do = DaoFunc $ \ox -> do
  xunit <- ask
  let currentProg = maybeToList (fmap programModuleName (currentProgram xunit))
  (selectFiles, execStrings) <- case ox of
    [OString file , OList strs] -> return ([file], strs )
    [OList   files, OList strs] -> requireAllStringArgs files >>= \files -> return (files , strs )
    [OString file , str       ] -> return ([file], [str])
    [OList   files, str       ] -> requireAllStringArgs files >>= \files -> return (files , [str])
    [OString str ] -> return (currentProg, [OString str])
    [OList   strs] -> return (currentProg, strs)
    _              -> ceError $ OList $
      OString (ustr "require query strings as parameters to \"do\" function, but received") : ox
  execStrings <- requireAllStringArgs execStrings
  recursiveExecQuery selectFiles execStrings
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
    Nothing  -> ceError $ OList [obj, OString (ustr "undefined reference")]
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
  let toplevs xunit = execRun (fmap (M.lookup op) (dReadMVar xloc (toplevelFuncs xunit)))
      lkup p = case p of
        ProgramFile p -> toplevs (programExecUnit p)
        _             -> return Nothing
  funcs <- fmap (concatMap maybeToList) (sequence (toplevs xunit : map lkup (importsTable xunit)))
  if null funcs
    then  objectError (OString op) $ "undefined "++msg++" ("++uchars op++")"
    else  return (concat funcs)

----------------------------------------------------------------------------------------------------

-- $ErrorReporting
-- The 'ContErrT' is a continuation monad that can evaluate to an error message without evaluating
-- to "bottom". The error message is any value of type 'Dao.Object.Object'. These functions provide
-- a simplified method for constructing error 'Dao.Object.Object's.

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

-- | Evaluate to 'ceError' if the given 'PValue' is 'Backtrack' or 'PFail'. You must pass a
-- 'Prelude.String' as the message to be used when the given 'PValue' is 'Backtrack'. You can also
-- pass a list of 'Dao.Object.Object's that you are checking, these objects will be included in the
-- 'ceError' value.
--     This function should be used for cases when you have converted 'Dao.Object.Object' to a
-- Haskell value, because 'Backtrack' values indicate type exceptions, and 'PFail' values indicate a
-- value error (e.g. out of bounds, or some kind of assert exception), and the messages passed to
-- 'ceError' will indicate this.
checkPValue :: String -> [Object] -> PValue Location a -> ExecScript a
checkPValue altmsg tried pval = case pval of
  OK a         -> return a
  Backtrack    -> ceError $ OList $
    OString (ustr "bad data type") : (if null altmsg then [] else [OString (ustr altmsg)]) ++ tried
  PFail lc msg -> ceError $ OList $
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
    ce <- withContErrSt (nestedExecStack T.Void (execScriptBlock (unComment try))) return
    void $ case ce of
      CEError o -> nestedExecStack (T.insert [unComment name] o T.Void) (execScriptBlock catch)
      ce        -> returnContErr ce
  ForLoop    varName inObj thn lc  -> nestedExecStack T.Void $ do
    inObj   <- evalObject (unComment inObj)
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
  ReturnExpr   a    obj        lc  -> evalObject (unComment obj) >>= \obj -> (if a then ceReturn else ceError) obj
  WithDoc      lval thn        lc  -> nestedExecStack T.Void $ do
    lval <- evalObject (unComment lval)
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

-- | Cache a single value so that multiple lookups of the given reference will always return the
-- same value, even if that value is modified by another thread between separate lookups of the same
-- reference value during evaluation of an 'Dao.Object.ObjectExpr'.
cacheReference :: Reference -> Maybe Object -> ExecScript ()
cacheReference r obj = case obj of
  Nothing  -> return ()
  Just obj -> ask >>= \xunit -> execRun $ dModifyMVar_ xloc (referenceCache xunit) $ \cache ->
    return (M.insert r obj cache)

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
        _      -> ceError $ OList $ [OString $ ustr "undefined refence", ORef nm]
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
          Nothing  -> ceError $ OList $
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
          Nothing  -> ceError $ OList $
            [OString (ustr "incorrect parameters passed to lambda call"), OScript fn, OList args]
      ORule   fn -> execRuleCall  args fn
      _          -> called_nonfunction_object (showObjectExpr 0 ref) fn
  ParenExpr     _     o      lc -> evalObject (unComment o)
  ArraySubExpr  o  _  i      lc -> do
    o <- evalObject o
    i <- evalObject (unComment i)
    case evalSubscript o i of
      OK          a -> return a
      PFail loc msg -> ceError (OString msg)
      Backtrack     -> ceError (OList [i, OString (ustr "cannot be used as index of"), o])
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
      Backtrack -> ceError $ OList $
        [OString $ ustr (show op), OString $ ustr "cannot operate on objects of type", left, right]
      PFail lc msg -> ceError $ OList [OString msg]
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
            op     -> ceError $ OList [OString (ustr ("undefined left-hand side of "++show op)), ixObj]
          Just old -> case op of
            UCONST -> ceError $ OList [OString (ustr ("twice defined left-hand side "++show op)), ixObj]
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
    exe <- execRun (setupExecutable (Com code))
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
-- 'Dao.Object.Monad.ceError' if it access is prohibited.
verifyRequirement :: Name -> a -> ExecScript a
verifyRequirement nm a = return a -- TODO: the rest of this function.

-- | Checks if this ExecUnit is allowed to import the file requested by an "import" statement
-- attribute. Returns any value you pass as the second parameter, throws a
-- 'Dao.Object.Monad.ceError'
verifyImport :: Name -> a -> ExecScript a
verifyImport nm a = return a -- TODO: the rest of this function.

-- | When the 'programFromSource' is scanning through a 'Dao.Object.SourceCode' object, it first
-- constructs an 'IntermediateProgram', which contains no 'Dao.Debug.DMVar's. Once all the data
-- structures are in place, a 'Dao.Object.CachedProgram' is constructed from this intermediate
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
    , programExecUnit   = error "Program.programExecUnit not defined before use"
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
  program <- initProgram interm globalResource
  xunit <- ask
  return (program{programExecUnit = xunit{currentProgram = Just program}})
  where
    err lst = lift $ ceError $ OList $ map OString $ (sourceFullPath script : lst)
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
          dModifyMVar_ xloc (toplevelFuncs xunit) $ return .
            M.alter (\funcs -> mplus (fmap (++[sub]) funcs) (return [sub])) name

----------------------------------------------------------------------------------------------------
-- src/Dao/Tasks.hs

-- | This module is pretty much where everything happens. The pattern matching and action execution
-- algorithm that defines the unique nature of the Dao system, the 'execInputString' function, is
-- defined here. So are 'Dao.Object.Job' and 'Dao.Object.Task' management functions, and a simple
-- interactive run loop 'interactiveRuntimeLoop' is also provided.

-- | Get the name 'Dao.Evaluator. of 
taskProgramName :: Task -> Maybe Name
taskProgramName task = case task of
  RuleTask  _ _ _ xunit -> fn xunit
  GuardTask     _ xunit -> fn xunit
  where { fn xunit = fmap programModuleName (currentProgram xunit) }

-- | Creates a 'Job' which is an object that manages a number of tasks. Creating a job with 'newJob'
-- automatically sets-up the threads to execute any number of tasks while handling exceptions. Once
-- you have a 'Job', you create tasks inside of it. With the 'Job' object, you can control whether
-- you want to wait for all tasks to finish, or have control returned to the evaluating context
-- immediately after the tasks have been created. You can also set how long the 'Job' will wait
-- before forcefully terminating all tasks.
newJob :: Maybe Int -> UStr -> Run Job
newJob timed instr = dStack xloc "newJob" $ ask >>= \runtime -> do
  jobQSem <- dNewQSem xloc "Job.jobCompletion" 0 -- is singaled when all tasks have completed
  taskEnd <- dNewEmptyMVar xloc "Job.taskCompletion" -- is used as to block the manager loop until tasks complete
  ready   <- dNewEmptyMVar xloc "Job.readyTasks" -- tasks to be executed will be queued here
  timeVar <- dNewMVar xloc "Job.jobTimerThread" Nothing -- contains the timer value, will be filled after defining 'timeKeeper'
  failed  <- dNewMVar xloc "Job.taskFailures" (M.empty) -- contains the table of any exceptions that killed a worker thread
  taskTab <- dNewMVar xloc "Job.taskExecTable" (M.empty) -- this DMVar contains the table of running task threads
  jobMVar <- dNewEmptyMVar xloc "Job/jobMVar" -- every task created needs to set it's 'ExecUnit's 'currentExecJob'.
  let
      taskWaitLoop = do
        -- This loop waits for tasks to end, and signals the 'jobQSem' when the last job has
        -- completed. This loop is actually called by the 'manager' loop, so it executes in the same
        -- thread as the manager loop. Exception handling does not occur here.
        thread  <- dTakeMVar xloc taskEnd -- wait for a task to end.
        allDone <- dModifyMVar xloc taskTab $ \taskTab -> do
          let taskTab' = M.delete thread taskTab -- remove the completed task from the taskTab.
          return (taskTab', M.null taskTab') -- returns whether or not the taskTab is empty.
        if allDone
          then do -- when the taskTab is empty...
            timer <- dSwapMVar xloc timeVar Nothing -- empty the timeVar DMVar.
            case timer of -- and kill the timer thread (if it is waiting)
              Nothing    -> return ()
              Just timer -> dKillThread xloc timer
            dSignalQSem xloc jobQSem -- then signal 'jobCompletion'.
          else taskWaitLoop -- loop if the taskTab is not empty.
      workerException task err@(SomeException e) =
        -- If a worker is flagged, it needs to place this flag into the 'failed' table.
        dModifyMVar_ xloc failed $ \ftab -> case taskProgramName task of
          Nothing   -> dPutStrErr xloc (show e) >> return ftab
          Just name -> return (M.update (Just . (++[(task, err)])) name ftab)
      worker task = dStack xloc "Job/worker" $ do
        -- A worker runs the 'task', catching exceptions with 'workerException', and it always
        -- signals the manager loop when this thread completes by way of the 'taskEnd' DMVar, even
        -- when an exception occurs.
        job <- dReadMVar xloc jobMVar
        dHandle xloc (workerException task) (execTask job task)
        dMyThreadId >>= dPutMVar xloc taskEnd
      timeKeeper time = do
        -- This function evaluates in a separate thread and delays itself then kills all running
        -- tasks in the taskTab. When a task is killed, it signals the manager loop via the
        -- 'taskEnd' DMVar, which will signal 'jobQSem' when the taskTab goes empty, thus timeKeeper
        -- does not kill the manager loop, it lets the manager clean-up and wait on the next batch
        -- of tasks.
        dThreadDelay xloc time
        dSwapMVar xloc timeVar Nothing
        dSwapMVar xloc taskTab (M.empty) >>= mapM_ (dKillThread xloc) . (M.keys)
      managerException (SomeException e) = do
        -- if the manager loop is flagged, it needs to delegate the flag to all of its workers.
        dSwapMVar xloc taskTab (M.empty) >>= mapM_ (\th -> dThrowTo xloc th e) . (M.keys)
        dSwapMVar xloc timeVar Nothing >>= \timer -> case timer of
          Nothing     -> return ()
          Just thread -> dThrowTo xloc thread e
      manager = do
        -- This is the main loop for the 'Job' controlling thread. It takes 'Task's from the
        -- 'readyTasks' table, waiting for the DMVar if necessary.
        tasks <- dTakeMVar xloc ready
        dMessage xloc ("received "++show (length tasks)++" tasks")
        case tasks of
          []    -> manager
          tasks -> do
            dModifyMVar_ xloc taskTab $ \taskExecTable -> do
              -- Launch all threads, filling the 'taskTab' DMVar atomically so that if a worker loop
              -- fails due to an exception and signals the manager that it is ready to be removed
              -- from the taskTab, the manager loop will not be able to modify this taskTab until
              -- the taskTab is completely filled-in.
              taskIDs <- forM tasks $ \task -> do
                this <- dFork forkIO xloc ("worker("++showTask task++")") (worker task)
                return (this, task)
              return (M.union (M.fromList taskIDs) taskExecTable)
            -- Launch a timekeeper thread if necessary.
            case timed of
              Nothing   -> void $ dSwapMVar xloc timeVar Nothing
              Just time -> dFork forkIO xloc ("timer("++show instr++")") (timeKeeper time) >>=
                void . dSwapMVar xloc timeVar . Just
            (dStack xloc "taskWaitLoop" taskWaitLoop) >> manager -- wait for the all tasks to stop, then loop to the next batch.
  -- Finally, start the thread manager loop, wrapped up in it's exception handler.
  jobManager <- dFork forkIO xloc ("jobManager("++show instr++")") $
    dHandle xloc managerException (dStack xloc "Job/manager" manager)
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
  dPutMVar xloc jobMVar job -- as soon as this happens, all tasks will be able to run.
  return job

-- | Waits for every job in the list to complete, that is, it waits until every 'jobCompletion'
-- 'Control.Concurrent.DQSem.DQSem' has been signalled.
waitForJobs :: [Job] -> Run ()
waitForJobs jobx = dStack xloc "waitForJobs" $ forM_ jobx (dWaitQSem xloc . jobCompletion)

-- If you created a 'Dao.Object.Job' using 'newJob', that 'Dao.Object.Job' is automatically inserted
-- into the 'Dao.Object.jobTable' of the 'Dao.Object.Runtime' of this 'Run' monad. To remove it, from
-- the table, use this function. The 'Job' is uniqely identified by it's 'Dao.Object.jobTaskThread'
-- 'Control.Concurrent.ThreadId'.
removeJobFromTable :: Job -> Run ()
removeJobFromTable job = ask >>= \runtime ->
  dModifyMVar_ xloc (jobTable runtime) $ \jtab -> return (M.delete (jobTaskThread job) jtab)


-- | When executing strings against Dao programs (e.g. using 'Dao.Tasks.execInputString'), you often
-- want to execute the string against only a subset of the number of total programs. Pass the
-- logical names of every module you want to execute strings against, and this function will return
-- them. If you pass an empty list, all 'PublicType' modules (in the 'programs' table of the
-- 'Runtime') will be returned. Pass @'Data.Maybe.Just' 'Dao.Evaluator.ExecUnit'@ to allow
-- 'PrivateType' functions to also be selected, however only modules imported by the program
-- associated with that 'ExecUnit' are allowed to be selected.
selectModules :: Maybe ExecUnit -> [Name] -> Run [File]
selectModules xunit names = dStack xloc "selectModules" $ ask >>= \runtime -> case names of
  []    -> do
    ax <- dReadMVar xloc (pathIndex runtime)
    dMessage xloc ("selected modules: "++intercalate ", " (map show (M.keys ax)))
    return (filter (not . null . isProgramFile) (M.elems ax))
  names -> do
    pathTab <- dReadMVar xloc (pathIndex runtime)
    let set msg           = M.fromList . map (\mod -> (mod, error msg))
        request           = set "(selectModules: request files)" names
    imports <- case xunit of
      Nothing    -> return M.empty
      Just xunit -> return $
        set "(selectModules: imported files)" $ concat $ maybeToList $ fmap programImports $ currentProgram xunit
    ax <- return $ M.intersection pathTab request
    dMessage xloc ("selected modules:\n"++unlines (map show (M.keys ax)))
    return $ M.elems ax

-- | Given an input string, and a program, return all patterns and associated match results and
-- actions that matched the input string, but do not execute the actions. This is done by tokenizing
-- the input string and matching the tokens to the program using 'Dao.Pattern.matchTree'.
-- NOTE: Rules that have multiple patterns may execute more than once if the input matches more than
-- one of the patterns associated with the rule. *This is not a bug.* Each pattern may produce a
-- different set of match results, it is up to the programmer of the rule to handle situations where
-- the action may execute many times for a single input.
matchStringToProgram :: UStr -> Program -> ExecUnit -> Run [(Pattern, Match, Executable)]
matchStringToProgram instr program xunit = dStack xloc "matchStringToProgram" $ do
  let eq = programComparator program
      match tox = do
        tree <- dReadMVar xloc (ruleSet program)
        fmap concat $ forM (matchTree eq tree tox) $ \ (patn, mtch, execs) ->
          return (concatMap (\exec -> [(patn, mtch, exec)]) execs)
--           forM cxrefx $ \cxref -> do
--             dModifyMVar_ xloc cxref $ \cx -> return $ case cx of
--               OnlyCache m -> cx
--               HasBoth _ m -> cx
--               OnlyAST ast ->
--                 HasBoth
--                 { sourceScript = ast
--                 , cachedScript = nestedExecStack M.empty (execScriptBlock ast)
--                 }
--             return (patn, mtch, cxref)
  tox <- runExecScript (programTokenizer program instr) xunit
  case tox of
    CEError obj -> do
      dModifyMVar_ xloc (uncaughtErrors xunit) $ \objx -> return $ (objx++) $
        [ OList $
            [ obj, OString (ustr "error occured while tokenizing input string")
            , OString instr, OString (ustr "in the program")
            , OString (programModuleName program)
            ]
        ]
      return []
    CEReturn tox -> match (extractStringElems tox)
    CENext   tox -> match tox

-- | Given a list of 'Program's and an input string, generate a set of 'Task's to be executed in a
-- 'Job'. The 'Task's are selected according to the input string, which is tokenized and matched
-- against every rule in the 'Program' according to the 'Dao.Pattern.matchTree' equation.
makeTasksForInput :: [ExecUnit] -> UStr -> Run [Task]
makeTasksForInput xunits instr = dStack xloc "makeTasksForInput" $ fmap concat $ forM xunits $ \xunit -> do
  let name    = currentProgram xunit >>= Just . programModuleName
      program = flip fromMaybe (currentProgram xunit) $
        error "execInputString: currentProgram of execution unit is not defined"
  dMessage xloc ("(match string to "++show (length xunits)++" running programs)")
  matched <- matchStringToProgram instr program xunit
  dMessage xloc ("(construct RuleTasks with "++show (length matched)++" matched rules)")
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
-- This function creates the 'Task's that for any given guard script: 'Dao.Object.preExecScript',
-- 'Dao.Object.postExecScript'.
makeTasksForGuardScript
  :: (Program -> [Executable])
  -> [ExecUnit]
  -> Run [Task]
makeTasksForGuardScript select xunits =
  dStack xloc "makeTasksForGuardScript" $ lift $ fmap concat $ forM xunits $ \xunit ->
    case fmap select (currentProgram xunit) of
      Nothing    -> return []
      Just progx -> return $ flip map progx $ \prog ->
        GuardTask
        { taskGuardAction = prog
        , taskExecUnit    = xunit
        }

-- | This function simple places a list of 'Task's into the 'Job's 'readyTasks' table. This function
-- will block if another thread has already evaluated this function, but those tasks have not yet
-- completed or timed-out.
startTasksForJob :: Job -> [Task] -> Run ()
startTasksForJob job tasks = dStack xloc "startTasksForJob" $ dPutMVar xloc (readyTasks job) tasks

-- | This is the "heart" of the Dao system; it is the algorithm you wanted to use when you decided
-- to install the Dao system. Select from the 'Dao.Object.Runtime's 'modules' table a list of Dao
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
execInputString guarded instr select = dStack xloc "execInputString" $ ask >>= \runtime -> do
  let xunits = map programExecUnit (concatMap isProgramFile select)
  unless (null xunits) $ do
    dMessage xloc ("selected "++show (length xunits)++" modules")
    -- Create a new 'Job' for this input string, 'newJob' will automatically place it into the
    -- 'Runtime's job table.
    job <- newJob (defaultTimeout runtime) instr
    let run fn = fn >>= \tasks -> case tasks of
          []    -> return ()
          tasks -> do
            dMessage xloc (show (length tasks)++" tasks created")
            startTasksForJob job tasks >> waitForJobs [job]
        exception (SomeException e) = removeJobFromTable job >> dThrowIO e
    -- here begins the three phases of executing a string:
    dHandle xloc exception $ do
      -- (1) run all 'preExecScript' actions as a task, wait for all tasks to complete
      when guarded $ do
        dMessage xloc "pre-string-execution"
        run $ makeTasksForGuardScript preExecScript xunits
      -- (2) run each actions for each rules that matches the input, wait for all tasks to complete
      dMessage xloc "execute string"
      run $ makeTasksForInput xunits instr
      -- (3) run all 'postExecScript' actions as a task, wait for all tasks to complete.
      when guarded $ do
        dMessage xloc "post-string-execution"
        run $ makeTasksForGuardScript postExecScript xunits
      -- (4) clear the Query-Time variables that were set during this past run in each ExecUnit.
      mapM_ clearAllQTimeVars xunits
    removeJobFromTable job

-- | In the current thread, and using the given 'Runtime' environment, parse an input string as
-- 'Dao.Object.Script' and then evaluate it. This is used for interactive evaluation. The parser
-- used in this function will parse a block of Dao source code, the opening and closing curly-braces
-- are not necessary. Therefore you may enter a semi-colon separated list of commands and all will
-- be executed.
evalScriptString :: ExecUnit -> String -> Run ()
evalScriptString xunit instr = dStack xloc "evalScriptString" $
  void $ flip runExecScript xunit $ nestedExecStack T.Void $ execScriptBlock $
  case fst (runParser parseInteractiveScript instr) of
    Backtrack     -> error "cannot parse expression"
    PFail tok msg -> error ("error: "++uchars msg++show tok)
    OK expr       -> expr

-- | This actually executes the 'Task', essentially converting it into @IO@ function. The resulting
-- @IO@ function can be evlauted in a separate thread to create an entry in the 'jobTable' of a
-- 'Job'; this is what 'newJob' does with each 'Task' passed to it, it calls 'execTask' within
-- 'Control.Concurrent.forkIO'.
execTask :: Job -> Task -> Run ()
execTask job task = dStack xloc "execTask" $ ask >>= \runtime -> do
  let run patn mtch action xunit fn = do
        result <- dStack xloc "execTask/runExecScript" $ lift $ try $ runIO runtime $
          runExecScript (runExecutable T.Void action) $
            xunit{currentTask = task, currentExecJob = Just job}
        let putErr err = dModifyMVar_ xloc (uncaughtErrors xunit) (return . (++[err]))
        case seq result result of
          Right (CEError       err) -> dPutStrErr xloc (showObj 0 err) >> putErr err
          Left  (SomeException err) -> do
            dPutStrErr xloc (show err)
            putErr (OString (ustr (show err)))
          _                         -> return ()
  case task of
    RuleTask  patn mtch action xunit -> run patn  (Just mtch) action xunit execScriptBlock
    GuardTask           action xunit -> run ONull Nothing     action xunit execGuardBlock

----------------------------------------------------------------------------------------------------
-- src/Dao/Files.hs

-- | Initialize a source code file into the given 'Runtime'. This function checks that the
-- 'Dao.Object.sourceFullPath' is unique in the 'programs' table of the 'Runtime', then evaluates
-- 'initSourceCode' and associates the resulting 'Dao.Evaluator.ExecUnit' with the
-- 'sourceFullPath' in the 'programs' table. Returns the logical "module" name of the script along
-- with an initialized 'Dao.Object.ExecUnit'.
registerSourceCode :: Bool -> UPath -> SourceCode -> Run (ContErr Program)
registerSourceCode public upath script = dStack xloc "registerSourceCode" $ ask >>= \runtime -> do
  let modName  = unComment (sourceModuleName script)
      pathTab  = pathIndex runtime
  alreadyLoaded <- fmap (M.lookup upath) (dReadMVar xloc pathTab)
  -- Check to make sure the logical name in the loaded program does not conflict with that
  -- of another loaded previously.
  case alreadyLoaded of
    Just (ProgramFile  f) -> return (CENext f)
    Just (DocumentFile f) -> return $ CEError $ OList $
      [ OString upath
      , OString (ustr "is already loaded as an idea file, cannot be loaded as a dao file")
      ]
    Nothing -> do
      -- Call 'initSourceCode' which creates the 'ExecUnit', then place it in an 'MVar'.
      -- 'initSourceCode' calls 'Dao.Evaluator.programFromSource'.
      xunit <- initSourceCode script >>= lift . evaluate
      let (Just prog) = currentProgram xunit
          file = ProgramFile prog
      xunitMVar <- seq file $ dNewMVar xloc ("ExecUnit("++show upath++")") xunit
      dModifyMVar_ xloc pathTab $ return . M.insert upath file
      return (CENext prog)

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
initSourceCode :: SourceCode -> Run ExecUnit
initSourceCode script = ask >>= \runtime -> do
  grsrc <- newTreeResource "Program.globalData" T.Void
  xunit <- initExecUnit runtime grsrc
  -- An execution unit is required to load a program, so of course, while a program is being
  -- loaded, the program is not in the program table, and is it's 'currentProgram' is 'Nothing'.
  cachedProg <- runExecScript (programFromSource grsrc (\_ _ _ -> return False) script) xunit
  case cachedProg of
    CEError  obj        -> error ("script err: "++showObj 0 obj)
    CENext   cachedProg -> do
      -- Run all initializer scripts (denoted with the @SETUP@ rule in the Dao language).
      let xunit' = xunit{ currentProgram = Just (cachedProg{programExecUnit = xunit'}) }
      setupTakedown constructScript xunit'
      -- Place the initialized module into the 'Runtime', mapping to the module's handle.
      return xunit'
    CEReturn _          ->
      error "INTERNAL ERROR: source code evaluation returned before completion"

-- | Load a Dao script program from the given file handle. You must pass the path name to store the
-- resulting 'File' into the 'Dao.Object.pathIndex' table. The handle must be set to the proper
-- encoding using 'System.IO.hSetEncoding'.
scriptLoadHandle :: Bool -> UPath -> Handle -> Run (ContErr Program)
scriptLoadHandle public upath h = do
  script <- lift $ do
    hSetBinaryMode h False
    fmap (loadSourceCode upath) (hGetContents h) >>= evaluate
  registerSourceCode public upath script

-- | Updates the 'Runtime' to include the Dao source code loaded from the given 'FilePath'. This
-- function tries to load a file in three different attempts: (1) try to load the file as a binary
-- @('Dao.Document.Document' 'Dao.Evaluator.DocData')@ object. (2) Try to load the file as a
-- binary @'Dao.Object.SourceCode'@ object. (3) Treat the file as text (using the current locale set
-- by the system, e.g. @en.UTF-8@) and parse a 'Dao.Object.SourceCode' object using
-- 'Dao.Object.Parsers.source'. If all three methods fail, an error is thrown. Returns
-- the 'TypedFile', although all source code files are returned as 'PrivateType's. Use
-- 'asPublic' to force the type to be a 'PublicType'd file.
loadFilePath :: Bool -> FilePath -> Run (ContErr File)
loadFilePath public path = dontLoadFileTwice (ustr path) $ \upath -> do
  dPutStrErr xloc ("Lookup file path "++show upath)
  h    <- lift (openFile path ReadMode)
  zero <- lift (hGetPosn h)
  enc  <- lift (hGetEncoding h)
  -- First try to load the file as a binary program file, and then try it as a binary data file.
  doc <- catchErrorCall (ideaLoadHandle upath h) :: Run (Either ErrorCall DocResource)
  case doc of
    Right doc -> return (CENext (DocumentFile doc))
    Left  _   -> do -- The file does not seem to be a document, try parsing it as a script.
      lift (hSetPosn zero >> hSetEncoding h (fromMaybe localeEncoding enc))
      fmap (fmap ProgramFile) (scriptLoadHandle public upath h)

-- | When a program is loaded, and when it is released, any block of Dao code in the source script
-- that is denoted with the @SETUP@ or @TAKEDOWN@ rules will be executed. This function performs
-- that execution in the current thread.
setupTakedown :: (Program -> [[Com ScriptExpr]]) -> ExecUnit -> Run ()
setupTakedown select xunit = ask >>= \runtime ->
  forM_ (concat $ maybeToList $ currentProgram xunit >>= Just . select) $ \block ->
    runExecScript (execGuardBlock block) xunit >>= lift . evaluate

