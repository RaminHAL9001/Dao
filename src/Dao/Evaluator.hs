-- "src/Dao/Evaluator.hs"  provides functions for executing the Dao
-- scripting language, i.e. functions evaluating the parsed abstract
-- syntax tree.
-- 
-- Copyright (C) 2008-2013  Ramin Honary.
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
import           Dao.Object.AST
import           Dao.PPrint
import qualified Dao.Tree as T
import           Dao.Glob
import           Dao.Resource
import           Dao.Predicate
import           Dao.Files
import           Dao.Parser

import           Dao.Object.Math
import           Dao.Object.PPrint
import           Dao.Object.Binary
import           Dao.Object.Pattern
import           Dao.Object.Parser

import           Control.Exception
import           Control.Concurrent
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State -- for constructing 'Program's from 'AST_SourceCode's.

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
import qualified Data.Binary as B

import           System.IO

import Debug.Trace

----------------------------------------------------------------------------------------------------

showObj :: PPrintable a => a -> String
showObj = prettyPrint 80 "    "

initExecUnit :: UPath -> TreeResource -> Run ExecUnit
initExecUnit modName initGlobalData = do
  runtime  <- ask
  unctErrs <- dNewMVar xloc "ExecUnit.uncaughtErrors" []
  recurInp <- dNewMVar xloc "ExecUnit.recursiveInput" []
  qheap    <- newTreeResource  "ExecUnit.queryTimeHeap" T.Void
  task     <- initTask
  xstack   <- dNewMVar xloc "ExecUnit.execStack" emptyStack
  files    <- dNewMVar xloc "ExecUnit.execOpenFiles" M.empty
  rules    <- dNewMVar xloc "ExecUnit.ruleSet" T.Void
  return $
    ExecUnit
    { parentRuntime      = runtime
    , currentWithRef     = Nothing
    , currentQuery       = Nothing
    , currentPattern     = Nothing
    , currentMatch       = Nothing
    , currentExecutable  = Nothing
    , currentBranch      = []
    , importsTable       = M.empty
    , patternTable       = []
    , execAccessRules    = RestrictFiles (Glob{getPatUnits = [Wildcard], getGlobLength = 1})
    , builtinFuncs       = initBuiltinFuncs
    , topLevelFuncs      = M.empty
    , queryTimeHeap      = qheap
    , taskForActions     = task
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
    , preExec           = []
    , programTokenizer  = return . tokens . uchars
    , programComparator = (==)
    , postExec          = []
    , ruleSet           = rules
    , globalData        = initGlobalData
    }

setupExecutable :: HasDebugRef r => [ScriptExpr] -> ReaderT r IO Executable
setupExecutable scrp = do
  staticRsrc <- lift (newIORef M.empty)
  return $
    Executable
    { origSourceCode = scrp
    , staticVars     = staticRsrc
    , executable     = execScriptBlock scrp >>= liftIO . evaluate
    }

runExecutable :: T_tree -> Executable -> Exec [Object]
runExecutable initStack exe = local (\xunit -> xunit{currentExecutable = Just exe}) $!
  execFuncPushStack initStack (executable exe >>= liftIO . evaluate >> return [])

-- | Given a list of arguments, matches these arguments toe the given subroutine's
-- 'Dao.Object.Pattern'. If it matches, the 'Dao.Object.getSubExecutable' of the 'Dao.Object.Executable'
-- is evaluated with 'runExecutable'. If the pattern does not match, an empty list is returned to the
-- 'Dao.Object.Exec' monad, which allows multiple 'Dao.Object.Subroutine's to be tried before
-- evaluating to an error in the calling context.
runSubroutine :: [Object] -> Subroutine -> Exec (Maybe [Object])
runSubroutine args sub =
  case evalMatcher (matchObjectList (argsPattern sub) args >> gets matcherTree) of
    OK       tree -> fmap Just (runExecutable tree (getSubExecutable sub))
    Backtrack     -> return Nothing
    PFail ref msg -> procErr (OList [OString msg, ORef ref])

-- | Very simply executes every given script item. Does not use catchReturnObj, does not use
-- 'nestedExecStack'. CAUTION: you cannot assign to local variables unless you call this method
-- within the 'nestedExecStack' or 'execFuncPushStack' functions. Failure to do so will cause a stack
-- underflow exception.
execScriptBlock :: [ScriptExpr] -> Exec ()
execScriptBlock block = mapM_ execScriptExpr block

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code found in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [ScriptExpr] -> Exec ()
execGuardBlock block = void (execFuncPushStack T.Void (execScriptBlock block >> return []))

-- $BasicCombinators
-- These are the most basic combinators for converting working with the 'ExecUnit' of an
-- 'Exec' monad.

----------------------------------------------------------------------------------------------------
-- $StackOperations
-- Operating on the local stack.

stack_underflow = error "INTERNAL ERROR: stack underflow"

-- | Push a new empty local-variable context onto the stack. Does NOT 'catchReturnObj', so it can be
-- used to push a new context for every level of nested if/else/for/try/catch statement, or to
-- evaluate a macro, but not a function call. Use 'execFuncPushStack' to perform a function call within
-- a function call.
nestedExecStack :: T_tree -> Exec a -> Exec a
nestedExecStack init exe = do
  stack <- fmap execStack ask
  lift (dModifyMVar_ xloc stack (return . stackPush init))
  ce <- procCatch exe
  lift (dModifyMVar_ xloc stack (return . stackPop))
  joinFlowCtrl ce

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. Use 'catchReturnObj' to prevent return calls from halting execution beyond this
-- function. This is what you should use to perform a Dao function call within a Dao function call.
execFuncPushStack :: T_tree -> Exec [Object] -> Exec [Object]
execFuncPushStack dict exe = do
  stackMVar <- lift (dNewMVar xloc "execFuncPushStack/ExecUnit.execStack" (Stack [dict]))
  ce <- procCatch (local (\xunit -> xunit{execStack=stackMVar}) exe)
  case ce of
    FlowReturn obj -> return obj
    _              -> joinFlowCtrl ce

----------------------------------------------------------------------------------------------------

-- | Used to evaluate an expression like @$1@, retrieves the matched pattern associated with an
-- integer. Specifically, it returns a list of 'Dao.ObjectObject's where each object is an
-- 'Dao.Object.OString' contained at the integer index of the 'Dao.Glob.matchGaps' of a
-- 'Dao.Glob.Glob'.
evalIntRef :: Word -> Exec Object
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
execHeapLookup :: [Name] -> Exec [Object]
execHeapLookup name = ask >>= \xunit -> fmap maybeToList $ lift $
  readResource (globalData xunit) name

-- | Lookup an object in the 'globalData' for this 'ExecUnit'.
execHeapUpdate :: [Name] -> (Maybe Object -> Exec (Maybe Object)) -> Exec [Object]
execHeapUpdate name runUpdate = ask >>= \xunit -> fmap maybeToList $
  inEvalDoUpdateResource (globalData xunit) name runUpdate

execHeapDefine :: [Name] -> Object -> Exec [Object]
execHeapDefine name obj = execHeapUpdate name (return . const (Just obj))

execHeapDelete :: [Name] -> Object -> Exec [Object]
execHeapDelete name obj = execHeapUpdate name (return . const Nothing)

-- | Lookup a reference value in the durrent document, if the current document has been set with a
-- "with" statement.
curDocVarLookup :: [Name] -> Exec [Object]
curDocVarLookup name = do
  xunit <- ask
  case currentWithRef xunit of
    Nothing                      -> return []
    Just file@(DocumentFile res) -> fmap maybeToList $ lift $
      readResource res (currentBranch xunit ++ name)
    _ -> error $ concat $
           [ "current document is not an idea file, cannot lookup reference "
           , intercalate "." (map uchars name)
           ]

-- | Update a reference value in the durrent document, if the current document has been set with a
-- "with" statement.
curDocVarUpdate :: [Name] -> (Maybe Object -> Exec (Maybe Object)) -> Exec [Object]
curDocVarUpdate name runUpdate = do
  xunit <- ask
  case currentWithRef xunit of
    Nothing                  -> return []
    Just file@(DocumentFile res) -> fmap maybeToList $
      inEvalDoUpdateResource res (currentBranch xunit ++ name) runUpdate
    _ -> error $ concat $
           [ "current document is not an idea file, cannot update reference "
           , intercalate "." (map uchars name)
           ]

curDocVarDefine :: [Name] -> Object -> Exec [Object]
curDocVarDefine ref obj = curDocVarUpdate ref (return . const (Just obj))

curDocVarDelete :: [Name] -> Object -> Exec [Object]
curDocVarDelete ref obj = curDocVarUpdate ref (return . const Nothing)

-- | Lookup a value in the 'execStack'.
localVarLookup :: Name -> Exec [Object]
localVarLookup sym = fmap execStack ask >>= lift . dReadMVar xloc >>=
  return . msum . map (maybeToList . T.lookup [sym]) . mapList

-- | Apply an altering function to the map at the top of the local variable stack.
localVarUpdate :: Name -> (Maybe Object -> Maybe Object) -> Exec [Object]
localVarUpdate name alt = ask >>= \xunit -> lift $
  dModifyMVar xloc (execStack xunit) $ \ax -> case mapList ax of
    []   -> stack_underflow
    a:ax ->
      let obj = alt (T.lookup [name] a)
      in  return (Stack (T.update [name] (const obj) a : ax), maybeToList obj)

-- | Force the local variable to be defined in the top level 'execStack' context, do not over-write
-- a variable that has already been defined in lower in the context stack.
localVarDefine :: Name -> Object -> Exec [Object]
localVarDefine name obj = localVarUpdate name (const (Just obj))

localVarDelete :: Name -> Exec [Object]
localVarDelete nm = localVarUpdate nm (const Nothing)

staticVarLookup :: Name -> Exec [Object]
staticVarLookup nm = do
  exe <- fmap (currentExecutable >=> return . staticVars) ask
  case exe of
    Nothing  -> return []
    Just exe -> liftIO (readIORef exe) >>= return . maybeToList . M.lookup nm

staticVarUpdate :: Name -> (Maybe Object -> Exec (Maybe Object)) -> Exec [Object]
staticVarUpdate nm upd = do
  ref <- fmap (currentExecutable >=> return . staticVars) ask
  case ref of
    Nothing  -> return []
    Just ref -> do
      val <- lift (lift (readIORef ref)) >>= return . (M.lookup nm) >>= upd
      lift (lift (modifyIORef ref (M.update (const val) nm)))
      return (maybeToList val)

staticVarDefine :: Name -> Object -> Exec [Object]
staticVarDefine nm obj = staticVarUpdate nm (return . const (Just obj))

staticVarDelete :: Name -> Exec [Object]
staticVarDelete nm = staticVarUpdate nm (return . const Nothing)

-- | Lookup an object, first looking in the current document, then in the 'globalData'.
globalVarLookup :: [Name] -> Exec [Object]
globalVarLookup ref = ask >>= \xunit ->
  (if isJust (currentWithRef xunit) then curDocVarLookup else execHeapLookup) ref

globalVarUpdate :: [Name] -> (Maybe Object -> Exec (Maybe Object)) -> Exec [Object]
globalVarUpdate ref runUpdate = ask >>= \xunit ->
  (if isJust (currentWithRef xunit) then curDocVarUpdate else execHeapUpdate) ref runUpdate

-- | To define a global variable, first the 'currentWithRef' is checked. If it is set, the variable
-- is assigned to the document at the reference location prepending 'currentBranch' reference.
-- Otherwise, the variable is assigned to the 'globalData'.
globalVarDefine :: [Name] -> Object -> Exec [Object]
globalVarDefine name obj = globalVarUpdate name (return . const (Just obj))

-- | To delete a global variable, the same process of searching for the address of the object is
-- followed for 'globalVarDefine', except of course the variable is deleted.
globalVarDelete :: [Name] -> Exec [Object]
globalVarDelete name = globalVarUpdate name (return . const Nothing)

qTimeVarLookup :: [Name] -> Exec [Object]
qTimeVarLookup ref = ask >>= \xunit -> fmap maybeToList $
  lift (readResource (queryTimeHeap xunit) ref)

qTimeVarUpdate :: [Name] -> (Maybe Object -> Exec (Maybe Object)) -> Exec [Object]
qTimeVarUpdate ref runUpdate = ask >>= \xunit -> fmap maybeToList $
  inEvalDoUpdateResource (queryTimeHeap xunit) ref runUpdate

qTimeVarDefine :: [Name] -> Object -> Exec [Object]
qTimeVarDefine name obj = qTimeVarUpdate name (return . const (Just obj))

qTimeVarDelete :: [Name] -> Exec [Object]
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
  o         -> return (ustr (showObj o))

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
        return (ostr (add (uchars a) (uchars b)))
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
eval_POW = evalNum (^) (\ a b -> toRational ((fromRational a :: Double) ** (fromRational b :: Double)))

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
        ax  = take 1 (drop b a)
    in  if b<0 then err else if null ax then err else return (head ax)
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

eval_EQUL :: Object -> Object -> BuiltinOp
eval_EQUL a b = return (if a==b then OTrue else ONull)

eval_NEQUL :: Object -> Object -> BuiltinOp
eval_NEQUL a b = return (if a==b then ONull else OTrue)

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
eval_NOT o = return (boolToObj (testNull o))

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

prefixOps :: Array ArithOp1 (Object -> BuiltinOp)
prefixOps = let o = (,) in array (minBound, maxBound) $
  [ o REF   eval_REF
  , o DEREF eval_DEREF
  , o INVB  eval_INVB
  , o NOT   eval_NOT
  , o NEG   eval_NEG
  ]

infixOps :: Array ArithOp2 (Object -> Object -> BuiltinOp)
infixOps = let o = (,) in array (minBound, maxBound) $
  [ o POINT evalSubscript
  , o DOT   eval_DOT
  , o OR    (evalBooleans (||))
  , o AND   (evalBooleans (&&))
  , o EQUL  eval_EQUL
  , o NEQUL eval_NEQUL
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

requireAllStringArgs :: [Object] -> Exec [UStr]
requireAllStringArgs ox = case mapM check (zip (iterate (+1) 0) ox) of
  OK      obj -> return obj
  Backtrack   -> procErr $ OList [ostr "all input parameters must be strings"]
  PFail i msg -> procErr $ OList [OString msg, OWord i, ostr "is not a string"]
  where
    check (i, o) = case o of
      OString o -> return o
      _         -> PFail i (ustr "requires string parameter, param number")

recurseGetAllStringArgs :: [Object] -> Exec [UStr]
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
      [ostr "function parameter", OList (map OWord (reverse ix)), OString msg]
    Backtrack   -> return []
    OK       ox -> return ox

builtin_print :: DaoFunc
builtin_print = DaoFuncAutoDeref $ \ox_ -> do
  let ox = flip map ox_ $ \o -> case o of
        OString o -> o
        o         -> ustr (showObj o)
  lift (lift (mapM_ (putStrLn . uchars) ox))
  return [OList (map OString ox)]

sendStringsToPrograms :: Bool -> [Name] -> [UStr] -> Exec ()
sendStringsToPrograms permissive names strings = do
  xunit <- ask
  index <- inExecEvalRun (dReadMVar xloc (pathIndex (parentRuntime xunit)))
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
            else [ostr "could not find program files:", OList (map OString notFound)]
       ++ if null nonPrograms
            then []
            else [ostr "not program files:", OList (map OString nonPrograms)]
       ++ if null found
            then [ostr "no files found", OList (map OString names)]
            else []
       ++ [ostr "cannot execute strings", OList (map OString strings) ]
  if null found || not permissive && not (null notFound && null nonPrograms)
    then  procErr errMsg
    else  forM_ found $ \ (name, xunit) -> inExecEvalRun $
            dModifyMVar_ xloc (recursiveInput xunit) (return . (++strings))

builtin_convertRef :: String -> DaoFunc
builtin_convertRef nm = DaoFuncNoDeref $ \ax -> case ax of
  []  -> procErr $ ostr ("\""++nm++"\" must be used on some reference value.")
  [a] -> fmap concat (mapM ref [a])
  ax  -> fmap ((:[]) . OList . concat) (mapM ref ax)
  where
    msg = ostr ("cannot use as a "++nm++" reference")
    ref a = case a of
      ORef a -> case nm of
        "global" -> case a of
          GlobalRef    a  -> return [ORef (GlobalRef a)]
          LocalRef     a  -> return [ORef (GlobalRef [a])]
          StaticRef    a  -> return [ORef (GlobalRef [a])]
          QTimeRef     ax -> return [ORef (GlobalRef ax)]
          FileRef    _ ax -> return [ORef (GlobalRef ax)]
          a -> procErr $ OList [ORef a, msg]
        "local" -> case a of
          GlobalRef    [a] -> return [ORef (LocalRef a)]
          LocalRef      a  -> return [ORef (LocalRef a)]
          StaticRef     a  -> return [ORef (LocalRef a)]
          QTimeRef     [a] -> return [ORef (LocalRef a)]
          FileRef    _ [a] -> return [ORef (LocalRef a)]
          _ -> procErr $ OList [ORef a, msg]
        "qtime" -> case a of
          GlobalRef    ax -> return [ORef (QTimeRef ax)]
          LocalRef     a  -> return [ORef (QTimeRef [a])]
          StaticRef    a  -> return [ORef (QTimeRef [a])]
          QTimeRef     ax -> return [ORef (QTimeRef ax)]
          FileRef    _ ax -> return [ORef (QTimeRef ax)]
        "static" -> case a of
          GlobalRef    [a] -> return [ORef (StaticRef a)]
          LocalRef      a  -> return [ORef (StaticRef a)]
          StaticRef     a  -> return [ORef (StaticRef a)]
          QTimeRef     [a] -> return [ORef (StaticRef a)]
          FileRef    _ [a] -> return [ORef (StaticRef a)]
      _ -> procErr $ OList [msg, a]

builtin_do :: DaoFunc
builtin_do = DaoFuncAutoDeref $ \ox -> do
  xunit <- ask
  let currentProg = [programModuleName xunit]
      isProgRef r = case r of
        ORef (ProgramRef a _) -> return a
        _ -> procErr $ OList $
          [ostr "first argument to \"do\" function must be all strings, or all file references", r]
  (selectFiles, execStrings) <- case ox of
    [OString file , OList strs] -> return ([file], strs )
    [OList   files, OList strs] -> mapM isProgRef files >>= \files -> return (files , strs )
    [OString file , str       ] -> return ([file], [str])
    [OList   files, str       ] -> mapM isProgRef files >>= \files -> return (files , [str])
    [OString str ] -> return (currentProg, [OString str])
    [OList   strs] -> return (currentProg, strs)
    _              -> procErr $ OList $
      ostr "require query strings as parameters to \"do\" function, but received" : ox
  execStrings <- requireAllStringArgs execStrings
  sendStringsToPrograms False selectFiles execStrings
  return [OList (map OString execStrings)]

builtin_join :: DaoFunc
builtin_join = DaoFuncAutoDeref $ \ox -> do
  ox <- recurseGetAllStringArgs ox
  return [ostr (concatMap uchars ox)]

builtin_open :: DaoFunc
builtin_open = DaoFuncAutoDeref $ \ox -> case ox of
  [OString path] -> do
    file <- inExecEvalRun (loadFilePath (uchars path)) >>= joinFlowCtrl
    return $
      [ ORef $ case file of
          ProgramFile  _ -> ProgramRef{progID=path, subRef=GlobalRef{globalRef=[]}}
          DocumentFile _ -> FileRef{filePath=path, globalRef=[]}
      ]
  _ -> procErr $ OList $
    [ostr "Argument provided to \"open()\" function must be a single file path", OList ox]

builtin_close_write :: String -> (FilePath -> Run a) -> (a -> Exec b) -> DaoFunc
builtin_close_write funcName runFunc joinFunc = DaoFuncAutoDeref $ \ox -> case ox of
  [OString path] -> unload path
  [ORef    path] -> case path of
    FileRef    path _ -> unload path
    ProgramRef path _ -> unload path
    _                 -> err (ORef path)
  [o]                 -> err o
  ox                  -> err (OList ox)
  where
    unload path = inExecEvalRun (runFunc (uchars path)) >>= joinFunc >> return [OTrue]
    err obj = procErr $ OList $
      [ ostr $ concat $
          [ "Argument provided to \"", funcName
          , "()\" must be a single file path or file reference"
          ]
      , obj
      ]

builtin_close :: DaoFunc
builtin_close = builtin_close_write "close" unloadFilePath return

builtin_write :: DaoFunc
builtin_write = builtin_close_write "write" writeFilePath joinFlowCtrl

builtin_call :: DaoFunc
builtin_call = DaoFuncAutoDeref $ \args -> 
  if null args
    then  simpleError "call function must have at least one argument"
    else  case args of
            OScript fn : args -> do
              obj <- runSubroutine args fn
              case obj of
                Just obj -> return obj
                Nothing  -> procErr $ OList $
                  [ ostr "incorrect parameters passed to lambda call"
                  , OScript fn, OList args
                  ]
            obj:args          -> called_nonfunction_object obj args

builtin_check_ref :: DaoFunc
builtin_check_ref = DaoFuncNoDeref $ \args ->
  fmap ((:[]) . boolToObj . and) $ forM args $ \arg -> case arg of
    ORef (MetaRef _) -> return True
    ORef o -> readReference o >>= return . not . null
    o -> return True

builtin_delete :: DaoFunc
builtin_delete = DaoFuncNoDeref $ \args -> do
  forM_ args $ \arg -> case arg of
    ORef o -> void $ updateReference o (const (return Nothing))
    _      -> return ()
  return [ONull]

-- | The map that contains the built-in functions that are used to initialize every
-- 'Dao.Object.ExecUnit'.
initBuiltinFuncs :: M.Map Name DaoFunc
initBuiltinFuncs = let o a b = (ustr a, b) in M.fromList $
  [ o "print"   builtin_print
  , o "call"    builtin_call
  , o "do"      builtin_do
  , o "join"    builtin_join
  , o "global"  $ builtin_convertRef "global"
  , o "local"   $ builtin_convertRef "local"
  , o "static"  $ builtin_convertRef "static"
  , o "qtime"   $ builtin_convertRef "qtime"
  , o "open"    builtin_open
  , o "close"   builtin_close
  , o "write"   builtin_write
  , o "defined" builtin_check_ref
  , o "delete"  builtin_delete
  ]

----------------------------------------------------------------------------------------------------

-- | If an 'Dao.Object.Object' value is a 'Dao.Object.Reference' (constructed with
-- 'Dao.Object.ORef'), then the reference is looked up using 'readReference'. Otherwise, the object
-- value is returned. This is used to evaluate every reference in an 'Dao.Object.ObjectExpr'.
evalObjectRefWithLoc :: (Location, Object) -> Exec [(Location, Object)]
evalObjectRefWithLoc (loc, obj) = case obj of
  ORef (MetaRef o) -> return [(loc, ORef o)]
  ORef ref         -> readReference ref >>= \o -> case o of
    [] -> procErr $ OList $ errAt loc ++ [obj, ostr "undefined reference"]
    ox -> return (map ((,)loc) ox)
  obj              -> return [(loc, obj)]

-- | Like 'evalObjectRefWithLoc' but only uses the location if there is an error and does not return
-- the location.
evalObjectRef :: (Location, Object) -> Exec [Object]
evalObjectRef = fmap (map snd) . evalObjectRefWithLoc

-- | Will return any value from the 'Dao.Object.ExecUnit' environment associated with a
-- 'Dao.Object.Reference'.
readReference :: Reference -> Exec [Object]
readReference ref = case ref of
  IntRef     i     -> {- trace ("read int ref: "   ++show i  ) $ -} fmap (:[]) (evalIntRef i)
  LocalRef   nm    -> {- trace ("read local ref: " ++show ref) $ -} localVarLookup nm
  QTimeRef   ref   -> {- trace ("read qtime ref: " ++show ref) $ -} qTimeVarLookup ref
  StaticRef  ref   -> {- trace ("read static ref: "++show ref) $ -} staticVarLookup ref
  GlobalRef  ref   -> {- trace ("read global ref: "++show ref) $ -} globalVarLookup ref
  ProgramRef p ref -> error "TODO: haven't yet defined lookup behavior for Program references"
  FileRef    f ref -> error "TODO: haven't yet defined lookup behavior for file references"
  MetaRef    _     -> procErr $ OList $
    [ostr "cannot dereference a reference-to-a-reference", ORef ref]

-- | All assignment operations are executed with this function. To modify any variable at all, you
-- need a reference value and a function used to update the value. This function will select the
-- correct value to modify based on the reference type and value, and modify it according to this
-- function.
updateReference :: Reference -> (Maybe Object -> Exec (Maybe Object)) -> Exec [Object]
updateReference ref modf = do
  xunit <- ask
  case ref of
    IntRef     i          -> error "cannot assign values to a pattern-matched reference"
    LocalRef   ref        -> do
      ox <- localVarLookup ref
      ox <- modf (foldl (\ _ o -> Just o) Nothing ox)
      localVarUpdate ref (const ox)
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
lookupFunction :: String -> Name -> Exec [Subroutine]
lookupFunction msg op = do
  xunit <- ask
  let toplevs xunit = return (M.lookup op (topLevelFuncs xunit))
      lkup p = case p of
        Just (ProgramFile xunit) -> toplevs xunit
        _                        -> return Nothing
  funcs <- fmap (concatMap maybeToList) $
    sequence (toplevs xunit : map lkup (M.elems (importsTable xunit)))
  if null funcs
    then  objectError (OString op) $ "undefined "++msg++" ("++uchars op++")"
    else  return (concat funcs)

----------------------------------------------------------------------------------------------------

-- $ErrorReporting
-- The 'Procedural' is a continuation monad that can evaluate to an error message without evaluating
-- to "bottom". The error message is any value of type 'Dao.Object.Object'. These functions provide
-- a simplified method for constructing error 'Dao.Object.Object's.

simpleError :: String -> Exec a
simpleError msg = procErr (ostr msg)

-- | Convert a 'Dao.Token.Location' to an 'Dao.Object.Object' value.
errAt :: Location -> [Object]
errAt loc = case loc of
  LocationUnknown -> []
  loc -> [ OPair (OWord (startingLine loc), OWord (fromIntegral (startingColumn loc)))
         , OPair (OWord (endingLine   loc), OWord (fromIntegral (endingColumn   loc)))
         ]

typeError :: Object -> String -> String -> Exec a
typeError o cons expect = objectError (OType (objType o)) (cons++" must be of type "++expect)

-- | Evaluate to 'procErr' if the given 'PValue' is 'Backtrack' or 'PFail'. You must pass a
-- 'Prelude.String' as the message to be used when the given 'PValue' is 'Backtrack'. You can also
-- pass a list of 'Dao.Object.Object's that you are checking, these objects will be included in the
-- 'procErr' value.
--     This function should be used for cases when you have converted 'Dao.Object.Object' to a
-- Haskell value, because 'Backtrack' values indicate type exceptions, and 'PFail' values indicate a
-- value error (e.g. out of bounds, or some kind of assert exception), and the messages passed to
-- 'procErr' will indicate this.
checkPValue :: String -> [Object] -> PValue Location a -> Exec a
checkPValue altmsg tried pval = case pval of
  OK a         -> return a
  Backtrack    -> procErr $ OList $
    ostr "bad data type" : (if null altmsg then [] else [OString (ustr altmsg)]) ++ tried
  PFail lc msg -> procErr $ OList $
    ostr "bad data value" :
      errAt lc ++ (if null altmsg then [] else [ostr altmsg]) ++ OString msg : tried

----------------------------------------------------------------------------------------------------

-- | Convert a single 'ScriptExpr' into a function of value @'Exec' 'Dao.Object.Object'@.
execScriptExpr :: ScriptExpr -> Exec ()
execScriptExpr script = case script of
  --------------------------------------------------------------------------------------------------
  EvalObject  o             loc -> unless (isNO_OP o) (void (evalObject o))
  --------------------------------------------------------------------------------------------------
  IfThenElse  ifn  thn  els loc -> nestedExecStack T.Void $ do
    ifn <- fmap concat (evalObjectWithLoc ifn >>= mapM evalObjectRef)
    if null ifn
      then  procErr $ OList $ errAt loc ++ [ostr "if statement tests a value that does not exist"]
      else  do
        trace (unlines $ map (\i -> "if(" ++ prettyPrint 80 "    " i ++ ")") ifn) $ return ()
        forM_ ifn $ \ifn -> execScriptBlock (if objToBool ifn then thn else els)
  --------------------------------------------------------------------------------------------------
  TryCatch try  name  catch loc -> do
    ce <- procCatch (nestedExecStack T.Void (execScriptBlock try))
    void $ case ce of
      FlowErr o -> nestedExecStack (T.insert [name] o T.Void) (execScriptBlock catch)
      ce        -> joinFlowCtrl ce
  --------------------------------------------------------------------------------------------------
  ForLoop varName inObj thn loc -> nestedExecStack T.Void $ do
    inObj <- fmap concat (evalObjectWithLoc inObj >>= mapM evalObjectRefWithLoc)
    let block thn = if null thn then return True else scrpExpr (head thn) >> block (tail thn)
        ctrlfn ifn thn =
          fmap (or . map objToBool . concat) (evalObjectWithLoc ifn >>= mapM evalObjectRef)
        scrpExpr expr = case expr of
          ContinueExpr a ifn loc -> ctrlfn ifn a
          _                      -> execScriptExpr expr >> return True
        loop thn name ix = case ix of
          []   -> return ()
          i:ix -> localVarDefine name i >> block thn >>= flip when (loop thn name ix)
    forM_ inObj $ \ (loc, inObj) -> case asList inObj of
      OK        ox  -> loop thn varName ox
      Backtrack     -> procErr $ OList $ errAt loc ++
        [inObj, ostr "cannot be represented as list"]
      PFail loc msg -> procErr $ OList (errAt loc ++ [inObj, OString msg])
  --------------------------------------------------------------------------------------------------
  WhileLoop    co   scrp    loc -> outerLoop where
    check co = fmap (or . map objToBool . concat) (evalObjectWithLoc co >>= mapM evalObjectRef)
    outerLoop = do
      condition <- check co
      if condition
        then  innerLoop scrp >>= \contin -> if contin then outerLoop else return ()
        else  return ()
    innerLoop ax = case ax of
      []     -> return True
      a : ax -> case a of
        ContinueExpr  contin condition loc -> do
          condition <- check condition
          if condition
            then  if contin then return True else return False
            else  innerLoop ax
        _                                        -> execScriptExpr a >> innerLoop ax
  --------------------------------------------------------------------------------------------------
  ContinueExpr a    _       loc -> simpleError $
    '"':(if a then "continue" else "break")++"\" expression is not within a \"for\" loop"
  --------------------------------------------------------------------------------------------------
  ReturnExpr returnStmt obj loc -> do
    ox <- fmap concat (evalObjectWithLoc obj >>= mapM evalObjectRef)
    if returnStmt
      then  procNonDeterm ox
      else  case ox of -- else this is a "throw" statement
        [o] -> procErr o
        ox  -> procErr (OList ox)
  --------------------------------------------------------------------------------------------------
  WithDoc   lval    thn     loc -> nestedExecStack T.Void $ do
    lval <- fmap concat (evalObjectWithLoc lval >>= mapM evalObjectRefWithLoc)
    let setBranch ref xunit = return (xunit{currentBranch = ref})
        setFile path xunit = do
          file <- lift (fmap (M.lookup path) (dReadMVar xloc (execOpenFiles xunit)))
          case file of
            Nothing  -> procErr $ OList $ map OString $
              [ustr "with file path", path, ustr "file has not been loaded"]
            Just file -> return (xunit{currentWithRef = Just file})
        run upd = ask >>= upd >>= \r -> local (const r) (execScriptBlock thn)
    forM_ lval $ \ (loc, lval) -> case lval of
      ORef (GlobalRef ref)    -> run (setBranch ref)
      ORef (FileRef path [])  -> run (setFile path)
      ORef (FileRef path ref) -> run (setFile path >=> setBranch ref)
      _ -> procErr $ OList $ errAt loc ++
        [ostr "operand to \"with\" statement is not a reference type"]
  --------------------------------------------------------------------------------------------------

showObjType :: Object -> String
showObjType obj = showObj (OType (objType obj))

-- | 'Dao.Object.ObjectExpr's can be evaluated anywhere in a 'Dao.Object.Script'. However, a
-- 'Dao.Object.ObjectExpr' is evaluated as a lone command expression, and not assigned to any
-- variables, and do not have any other side-effects, then evaluating an object is a no-op. This
-- function checks the kind of 'Dao.Object.ObjectExpr' and evaluates to 'True' if it is impossible
-- for an expression of this kind to produce any side effects. Otherwise, this function evaluates to
-- 'False', which indicates it is OK to evaluate the expression and disgard the resultant 'Object'.
isNO_OP :: ObjectExpr -> Bool
isNO_OP o = case o of
  Literal      _     _ -> True
  ParenExpr    _ o   _ -> isNO_OP o
  ArraySubExpr _ _   _ -> True
  DictExpr     _ _   _ -> True
  ArrayExpr    _ _   _ -> True
  LambdaExpr   _ _ _ _ -> True
  _                    -> False

called_nonfunction_object :: Object -> [Object] -> Exec e
called_nonfunction_object ref args = procErr $ OList $
  [ ostr "first argument to \"call\" function must evaluate to a callable object"
  , ref, OList args
  ]

evalObjectWithLoc :: ObjectExpr -> Exec [(Location, Object)]
evalObjectWithLoc expr = fmap (map (\obj -> (getLocation expr, obj))) (evalObject expr)

-- | Evaluate an 'ObjectExpr' to an 'Dao.Object.Object' value, and does not de-reference objects of
-- type 'Dao.Object.ORef'
evalObject :: ObjectExpr -> Exec [Object]
evalObject obj = case obj of
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  VoidExpr                       -> return []
    -- ^ 'VoidExpr's only occur in return statements. Returning 'ONull' where nothing exists is
    -- probably the most intuitive thing to do on an empty return statement.
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  Literal     o              loc -> return [o]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  AssignExpr  nm  op  expr   loc -> do
    nmx <- evalObjectWithLoc nm
    let lhs = "left-hand side of "++show op
    case nmx of
      []  -> procErr $ OList $ errAt loc ++
        [ostr ("right hand side of "++show op++" evaluates to a void")]
      nmx -> fmap concat $ forM nmx $ \ (leftLoc, nm) -> do
        nm   <- checkPValue lhs [nm] (asReference nm)
        expr <- fmap concat (evalObjectWithLoc expr >>= mapM evalObjectRefWithLoc)
        case foldl (\ _ o -> [o]) [] expr of -- only use the final item
          []         -> procErr $ OList $ errAt loc ++ [ostr (lhs++" evaluates to a void")]
          [(loc, o)] -> updateReference nm $ \maybeObj -> case maybeObj of
            Nothing      -> case op of
                    UCONST -> return (Just o)
                    _      -> procErr $ OList $ errAt loc ++ [ostr "undefined refence", ORef nm]
            Just prevVal -> fmap Just $
              checkPValue "assignment expression" [prevVal, o] $ (updatingOps!op) prevVal o
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  FuncCall   op   args       loc -> do -- a built-in function call
    bif  <- fmap builtinFuncs ask
    args <- mapM evalObjectWithLoc args :: Exec [[(Location, Object)]]
    let combine ox args = -- given the non-deterministic arguments,
          if null args    -- create every possible combination of arguments
            then  return ox
            else  head args >>= \arg -> combine (ox++[arg]) (tail args)
    fmap concat $ forM (combine [] args) $ \args -> case M.lookup op bif of
      Nothing -> do -- no built-ins by the 'op' name
        fn   <- lookupFunction "function call" op
        args <- mapM evalObjectRef args
        fmap concat $ forM (combine [] args) $ \args -> do
          ~obj <- mapM (runSubroutine args) fn
          case msum obj of
            Just obj -> return obj
            Nothing  -> procErr $ OList $ errAt loc ++
              [ostr "incorrect parameters passed to function", OString op, OList args]
      Just bif -> case bif of -- 'op' references a built-in
        DaoFuncNoDeref   bif -> bif (map snd args)
        DaoFuncAutoDeref bif -> mapM evalObjectRef args >>= fmap concat . mapM bif . combine []
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ParenExpr     _     o      loc -> evalObject o
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ArraySubExpr  o     i      loc -> do
    ox <- fmap concat (evalObjectWithLoc o >>= mapM evalObjectRef)
    fmap concat $ forM ox $ \o -> do
      ix <- evalObjectWithLoc i
      forM ix $ \ (loc, i) -> case evalSubscript o i of
        OK          a -> return a
        PFail loc msg -> procErr (OString msg)
        Backtrack     -> procErr (OList $ errAt loc ++ [i, ostr "cannot be used as index of", o])
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  Equation   left' op right' loc -> do
    left  <- evalObjectWithLoc left'
    right <- evalObjectWithLoc right'
    let combine ax bx = ax >>= \a -> bx >>= \b -> return (a,b)
        err lr orig = procErr $ OList $ errAt (getLocation orig) ++
          [ostr (lr++"-hand of "++show op++" operator evaluates to a void")]
    case (left, right) of
      ([]  , _    ) -> err "left"  left'
      (_   , []   ) -> err "right" right'
      (left, right) -> fmap concat $
        forM (combine left right) $ \ (left, right) -> do
          args <- case op of
            DOT   -> return (combine [left] [right])
            POINT -> liftM2 combine (evalObjectRefWithLoc left) (return [right])
            _     -> liftM2 combine (evalObjectRefWithLoc left) (evalObjectRefWithLoc right)
          forM args $ \ (left, right) -> case (infixOps!op) (snd left) (snd right) of
            OK result -> return result
            Backtrack -> procErr $ OList $ concat $
              [ errAt loc, [ostr (show op), ostr "cannot operate on objects of type"]
              , errAt (fst left), [snd left], errAt (fst right), [snd right]
              ]
            PFail _  msg -> procErr $ OList $ errAt loc ++ [OString msg]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  PrefixExpr op       expr   loc -> do
    expr <- evalObject expr
    forM expr $ \expr -> case (prefixOps!op) expr of
      OK result -> return result
      Backtrack -> procErr $ OList $
        [ostr (show op), ostr "cannot operate on objects of type", expr]
      PFail lc msg -> procErr $ OList [OString msg]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  DictExpr   cons     args   loc -> do
    let loop insfn getObjVal mp argx  = case argx of
          []       -> return mp
          arg:argx -> case arg of
            AssignExpr ixObj op  new loc -> do
              ixObj <- fmap concat (evalObjectWithLoc ixObj >>= mapM evalObjectRef)
              mp <- (\a b c -> foldM c a b) mp ixObj $ \mp ixObj -> do
                ixVal <- checkPValue (show cons++" assignment expression") [ixObj] (getObjVal ixObj)
                new   <- fmap (foldl (\ _ o -> [o]) [] . concat) $ -- take the final item
                            evalObjectWithLoc new >>= mapM evalObjectRef
                case new of
                  []    -> procErr $ OList $
                    [ ostr (show op), ostr "assignment to index", ixObj
                    , ostr "failed, right-hand side evaluates to a void"
                    ]
                  [new] -> insfn mp ixObj ixVal op new
              loop insfn getObjVal mp argx
            _ -> error "dictionary constructor contains an expression that is not an assignment"
        assign lookup insert mp ixObj ixVal op new = case lookup ixVal mp of
          Nothing  -> case op of
            UCONST -> return (insert ixVal new mp)
            op     -> procErr $ OList [ostr ("undefined left-hand side of "++show op), ixObj]
          Just old -> case op of
            UCONST -> procErr $ OList [ostr ("twice defined left-hand side "++show op), ixObj]
            op     -> do
              new <- checkPValue (show cons++" assignment expression "++show op) [ixObj, old, new] $
                        (updatingOps!op) old new
              return (insert ixVal new mp)
        intmap = assign I.lookup I.insert
        dict   = assign M.lookup M.insert
    fmap (:[]) $ case () of
      () | cons == ustr "dict"   -> fmap ODict   (loop dict   asStringNoConvert M.empty args)
      () | cons == ustr "intmap" -> fmap OIntMap (loop intmap asHaskellInt      I.empty args)
      () | cons == ustr "list"   ->
        fmap (OList . concat) (mapM evalObjectWithLoc args >>= mapM evalObjectRef . concat)
      _ -> error ("INTERNAL ERROR: unknown dictionary declaration "++show cons)
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ArrayExpr  rang  ox        loc -> do
    case rang of
      (_ : _ : _ : _) -> procErr $ OList $
        [ostr "internal error: array range expression has more than 2 arguments"]
      [lo, hi] -> do
        let getIndex msg bnd = do
              bndObjs <- evalObjectWithLoc bnd
              bnd <- fmap (foldl (\ _ o -> [fmap fromIntegral (asHaskellInt o)]) [] . concat) $
                        mapM evalObjectRef bndObjs
              case bnd of
                []    -> procErr $ OList $
                  [ostr "array expression", ostr msg, ostr "evaluates to a void"]
                [bnd] -> checkPValue msg (map snd bndObjs) bnd
        lo <- getIndex "lower-bound" lo
        hi <- getIndex "upper-bound" hi
        (lo, hi) <- return (if lo<hi then (lo,hi) else (hi,lo))
        ox <- fmap concat (mapM evalObjectWithLoc ox >>= mapM evalObjectRef . concat)
        return [OArray (listArray (lo, hi) ox)]
      _ -> procErr $ OList
        [ostr "internal error: array range expression has fewer than 2 arguments"]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  DataExpr   strs            loc -> case b64Decode (concatMap uchars strs) of
    Right dat -> return [OBytes dat]
    Left  (ch, loc) -> procErr $ OList $
      [ ostr "invalid character in base-64 data expression", OChar ch
      , ostr "at position", OWord loc
      ]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  LambdaExpr typ argv code   loc -> fmap (:[]) (evalLambdaExpr typ argv code)
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  MetaEvalExpr expr          loc -> evalObject expr
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

evalLambdaExpr :: LambdaExprType -> [ObjectExpr] -> [ScriptExpr] -> Exec Object
evalLambdaExpr typ argv code = do
  exe <- lift (setupExecutable (code))
  let convArgv fn = mapM fn argv
  case typ of
    FuncExprType -> do
      argv <- convArgv argsToObjPat
      return $ OScript $
        Subroutine{argsPattern=argv, getSubExecutable=exe}
    RuleExprType -> do
      argv <- convArgv argsToGlobExpr
      return $ OScript $
        GlobAction{globPattern=argv, getSubExecutable=exe}

-- | Convert an 'Dao.Object.ObjectExpr' to an 'Dao.Object.Pattern'.
argsToObjPat :: ObjectExpr -> Exec Pattern
argsToObjPat o = case o of
  Literal (ORef (LocalRef r)) _ -> return (ObjLabel r ObjAny1)
  _ -> simpleError "does not evaluate to an object pattern"
  -- TODO: provide a more expressive way to create object patterns from 'Dao.Object.ObjectExpr's

-- | Convert an 'Dao.Object.ObjectExpr' to an 'Dao.Glob.Glob'.
argsToGlobExpr :: ObjectExpr -> Exec Glob
argsToGlobExpr o = case o of
  Literal (OString str) _ -> return (read (uchars str))
  _ -> simpleError "does not evaluate to a \"glob\" pattern"

-- | Simply checks if an 'Prelude.Integer' is within the maximum bounds allowed by 'Data.Int.Int'
-- for 'Data.IntMap.IntMap'.
checkIntMapBounds :: Integral i => Object -> i -> Exec ()
checkIntMapBounds o i = do
  let (lo, hi) = (minBound::Int, maxBound::Int)
  unless (fromIntegral lo < i && i < fromIntegral hi) $
    objectError o $ show IntMapType++" index is beyond the limits allowd by this data type"

----------------------------------------------------------------------------------------------------

-- | Checks if this ExecUnit is allowed to use a set of built-in rules requested by an "require"
-- attribute. Returns any value you pass as the second parameter, throws a
-- 'Dao.Object.Monad.procErr' if it access is prohibited.
verifyRequirement :: Name -> a -> Exec a
verifyRequirement nm a = return a -- TODO: the rest of this function.

-- | Checks if this ExecUnit is allowed to import the file requested by an "import" statement
-- attribute. Returns any value you pass as the second parameter, throws a
-- 'Dao.Object.Monad.procErr'
verifyImport :: Name -> a -> Exec a
verifyImport nm a = return a -- TODO: the rest of this function.

-- | This function is evaluated to update the 'Dao.Object.ExecUnit' to fill the 'Dao.Object.ExecUnit'
-- with the useful things in the 'Dao.Object.AST_SourceCode'. Before evaluating this function you should
-- evaluate sourceFromHandle' is called to create a 'Dao.Object.AST_SourceCode',
-- 'Dao.Resource.newTreeResource' is called to create a 'Dao.Resource.TreeResource', and
-- 'registerSourceCode' is called to initialize a new 'Dao.Object.ExecUnit'.
programFromSource :: TreeResource -> AST_SourceCode -> ExecUnit -> Run (FlowCtrl ExecUnit)
programFromSource theNewGlobalTable src xunit = do
  runtime <- ask
  let dx = concatMap toInterm (directives src)
  evalStateT (importsLoop dx) (xunit{parentRuntime=runtime})
  where
    importsLoop dx = do
      runtime <- gets parentRuntime
      case dx of
        Attribute kindOfAttr    nm               loc : dx -> do
          case uchars kindOfAttr of
            a | a=="require" || a=="requires" -> do
              case M.lookup nm (functionSets runtime) of
                Nothing -> return $ FlowErr $ OList $
                  [ostr "requires", OString nm, ostr "but not provided"]
                Just mp -> do
                  modify (\xunit -> xunit{builtinFuncs = M.union mp (builtinFuncs xunit)})
                  importsLoop dx
            a | a=="import" || a=="imports" -> do
              modify $ \xunit ->
                xunit{importsTable = M.alter (flip mplus (Just Nothing)) nm (importsTable xunit)}
              importsLoop dx
        dx -> scriptLoop dx
    scriptLoop dx = case dx of
      []                                                  -> fmap FlowOK get
      TopFunc       name       argList    script loc : dx -> do
        xunit  <- get
        result <- mkArgvExe argList script
        case result of
          FlowOK sub -> do
            let alt funcs = mplus (fmap (++[sub]) funcs) (return [sub])
            put (xunit{topLevelFuncs = M.alter alt name (topLevelFuncs xunit)})
          FlowErr err -> return () -- errors are handled in the 'mkArgvExe' function
        scriptLoop dx
      TopScript     expr                         loc : dx -> do
        modify $ \xunit ->
          let cs = constructScript xunit
          in  xunit{constructScript = if null cs then [[expr]] else [head cs ++ [expr]]}
        scriptLoop dx
      TopLambdaExpr ruleOrPat  argList    script loc : dx -> case ruleOrPat of
        FuncExprType -> pat
        PatExprType  -> pat
        RuleExprType -> rule
        where
          pat = do
            result <- mkArgvExe argList script
            case result of
              FlowOK sub -> do
                modify (\xunit -> xunit{patternTable = patternTable xunit ++ [sub]})
              FlowErr _ -> return () -- errors are handled in the 'mkArgvExe' function.
            scriptLoop dx
          rule = do
            exe  <- mkExe script
            argv <- lift $ flip runExec xunit $ mapM argsToGlobExpr argList
            case argv of
              FlowOK argv -> do
                rules <- gets ruleSet
                let fol tre pat = T.merge T.union (++) tre (toTree pat [exe])
                lift $ dModifyMVar_ xloc rules (\patTree -> return (foldl fol patTree argv))
              FlowErr _ -> return () -- errors are handled in the 'mkArgvExe' function.
            scriptLoop dx
      EventExpr     evtType    script            loc : dx -> do
        exe <- lift $ setupExecutable script
        case evtType of
          BeginExprType -> modify $ \xunit -> xunit{preExec  = preExec  xunit ++ [exe]}
          EndExprType   -> modify $ \xunit -> xunit{postExec = postExec xunit ++ [exe]}
          ExitExprType  -> modify $ \xunit ->
            xunit{destructScript = destructScript xunit ++ [script]}
        scriptLoop dx
      Attribute     kindOfAttr attribName        loc : _  -> return $ FlowErr $ OList $
        [ OString kindOfAttr, OString attribName
        , ostr "statement is not at the top of the source file"
        ]
    mkExe script = lift $ setupExecutable (script)
    mkArgvExe argList script = do
      argv <- lift $ flip runExec xunit $ mapM argsToObjPat $ argList
      exe  <- mkExe script
      case argv of
        FlowOK argv  -> return $ FlowOK $
          Subroutine
          { argsPattern      = argv
          , getSubExecutable = exe
          }
        FlowErr err  -> do
          lift $ dModifyMVar_ xloc (uncaughtErrors xunit) (\ex -> return (ex++[err]))
          return (FlowErr err)
        FlowReturn _ -> error "argsToObjPat returned 'FlowReturn' instead of '(FlowOK [Pattern])'"

-- | When the 'programFromSource' is scanning through a 'Dao.Object.AST_SourceCode' object, it first
-- constructs an 'IntermediateProgram', which contains no 'Dao.Debug.DMVar's. Once all the data
-- structures are in place, a 'Dao.Object.CachedProgram' is constructed from this intermediate
-- representation.
data IntermediateProgram
  = IntermediateProgram
    { inmpg_programImports    :: [UStr]
    , inmpg_constructScript   :: [[ScriptExpr]]
    , inmpg_destructScript    :: [[ScriptExpr]]
    , inmpg_requiredBuiltins  :: [Name]
    , inmpg_programAttributes :: M.Map Name Name
    , inmpg_preExec           :: [Executable]
    , inmpg_postExec          :: [Executable]
    , inmpg_programTokenizer  :: Tokenizer
    , inmpg_programComparator :: CompareToken
    , inmpg_ruleSet           :: PatternTree [Executable]
    -- , inmpg_globalData        :: T.Tree Name Object
    , inmpg_topLevelFuncs     :: M.Map Name [Subroutine]
    }

----------------------------------------------------------------------------------------------------

-- | Blocks until every thread in the given 'Dao.Object.Task' completes evaluation.
taskWaitThreadLoop :: Task -> Run ()
taskWaitThreadLoop task = dStack xloc "taskWaitThreadLoop" $ do
  threads <- dReadMVar xloc (taskRunningThreads task)
  if S.null threads then return () else loop
  where 
    loop = do
      thread <- dTakeMVar xloc (taskWaitMVar task)
      isDone <- dModifyMVar xloc (taskRunningThreads task) $ \threads_ -> do
        let threads = S.delete thread threads_
        return (threads, S.null threads)
      if isDone then return () else loop

-- | Registers the threads created by 'runStringAgainstExecUnits' into the
-- 'Dao.Object.runningExecThreads' field of the 'Dao.Object.Runtime'.
taskRegisterThreads :: Task -> Run [DThread] -> Run ()
taskRegisterThreads task makeThreads = do
  runtime <- ask
  dModifyMVar_ xloc (taskRunningThreads task) $ \threads -> do
    newThreads <- makeThreads
    return (S.union (S.fromList newThreads) threads)

-- | Signal to the 'Dao.Object.Task' that the task has completed. This must be the last thing a
-- thread does if it is registered into a 'Dao.Object.Task' using 'taskRegisterThreads'.
completedThreadInTask :: Task -> Run ()
completedThreadInTask task = dMyThreadId >>= dPutMVar xloc (taskWaitMVar task)

-- | Evaluate an 'Dao.Object.Action' in the current thread.
execAction :: ExecUnit -> Action -> Run ()
execAction xunit_ action = dStack xloc ("execAction for ("++show (actionPattern action)++")") $ do
  result <- runExec (runExecutable T.Void (actionExecutable action)) xunit >>= liftIO . evaluate
  case seq result result of
    FlowOK     _ -> return ()
    FlowReturn _ -> return ()
    FlowErr    o -> liftIO (hPutStrLn stderr ("ERROR: "++show o))
  where
    xunit =
      xunit_
      { currentQuery      = actionQuery      action
      , currentPattern    = actionPattern    action
      , currentMatch      = actionMatch      action
      , currentExecutable = Just (actionExecutable action)
      }

-- | Create a new thread and evaluate an 'Dao.Object.Action' in that thread. This thread is defined
-- such that when it completes, regardless of whether or not an exception occurred, it signals
-- completion to the 'Dao.Object.waitForActions' 'Dao.Debug.DMVar' of the 'Dao.Object.ExecUnit'
-- associated with this 'Dao.Object.Action'.
forkExecAction :: ExecUnit -> Action -> Run DThread
forkExecAction xunit act = dFork forkIO xloc "forkExecAction" $ do
  dCatch xloc (execAction xunit act >>= liftIO . evaluate) (\ (SomeException _) -> return ())
  completedThreadInTask (taskForActions xunit)

-- | For every 'Dao.Object.Action' in the 'Dao.Object.ActionGroup', evaluate that
-- 'Dao.Object.Action' in a new thread in the 'Task' associated with the 'Dao.Object.ExecUnit' of
-- the 'Dao.Object.ActionGroup'.
execActionGroup :: ActionGroup -> Run ()
execActionGroup actgrp = dStack xloc ("execActionGroup ("++show (length (getActionList actgrp))++" items)") $ do
  let xunit = actionExecUnit actgrp
      task  = taskForActions xunit
  taskRegisterThreads task (forM (getActionList actgrp) (forkExecAction (actionExecUnit actgrp)))
  taskWaitThreadLoop task >>= liftIO . evaluate

-- | This is the most important algorithm of the Dao system, in that it matches strings to all
-- rule-patterns in the program that is associated with the 'Dao.Object.ExecUnit', and it dispatches
-- execution of the rule-actions associated with matched patterns. This is the function that runs in
-- the thread which manages all the other threads that are launched in response to a matching input
-- string. You could just run this loop in the current thread, if you only have one 'ExecUnit'.
execInputStringsLoop :: ExecUnit -> Run ()
execInputStringsLoop xunit = dStack xloc "execInputStringsLoop" $ do
  runtime <- ask
  dCatch xloc loop (\ (SomeException _) -> return ())
  completedThreadInTask (taskForActions xunit)
  where
    loop = do
      dMessage xloc "Get the next input string. Also nub the list of queued input strings."
      instr <- dModifyMVar xloc (recursiveInput xunit) $ \ax -> return $ case nub ax of
        []   -> ([], Nothing)
        a:ax -> (ax, Just a)
      case instr of
        Nothing    -> return ()
        Just instr -> dStack xloc ("execInputString "++show instr) $ do
          dStack xloc "Run 'execPatternMatchExecutable' for every matching item." $
            waitAll (makeActionsForQuery instr xunit) >>= liftIO . evaluate
          dMessage xloc "Run the next string."
          loop

waitAll :: Run ActionGroup -> Run ()
waitAll getActionGroup = getActionGroup >>= execActionGroup

-- | Given an input string, and a program, return all patterns and associated match results and
-- actions that matched the input string, but do not execute the actions. This is done by tokenizing
-- the input string and matching the tokens to the program using 'Dao.Glob.matchTree'.
-- NOTE: Rules that have multiple patterns may execute more than once if the input matches more than
-- one of the patterns associated with the rule. *This is not a bug.* Each pattern may produce a
-- different set of match results, it is up to the programmer of the rule to handle situations where
-- the action may execute many times for a single input.
makeActionsForQuery :: UStr -> ExecUnit -> Run ActionGroup
makeActionsForQuery instr xunit = dStack xloc "makeActionsForQuery" $ do
  tox <- runExec (programTokenizer xunit instr) xunit
  case tox of
    FlowErr    obj -> do
      dModifyMVar_ xloc (uncaughtErrors xunit) $ \objx -> return $ (objx++) $
        [ OList $
            [ obj, ostr "error occured while tokenizing input string"
            , OString instr, ostr "in the program"
            , OString (programModuleName xunit)
            ]
        ]
      return (ActionGroup{ actionExecUnit = xunit, getActionList = [] })
    FlowReturn tox -> match (concatMap extractStringElems tox)
    FlowOK     tox -> match tox
  where
    eq = programComparator xunit
    match tox = do
      tree <- dReadMVar xloc (ruleSet xunit)
      return $
        ActionGroup
        { actionExecUnit = xunit
        , getActionList = flip concatMap (matchTree eq tree tox) $ \ (patn, mtch, execs) ->
            flip map execs $ \exec -> seq exec $! seq instr $! seq patn $! seq mtch $!
              Action
              { actionQuery      = Just instr
              , actionPattern    = Just patn
              , actionMatch      = Just mtch
              , actionExecutable = exec
              }
        }

-- | Create a list of 'Dao.Object.Action's for every BEGIN or END statement in the Dao program. Pass
-- 'Dao.Object.preExec' as the first parameter to get the BEGIN scrpits, pass 'Dao.Object.postExec'
-- to get the END scripts.
getBeginEndScripts :: (ExecUnit -> [Executable]) -> ExecUnit -> ActionGroup
getBeginEndScripts select xunit =
  ActionGroup
  { actionExecUnit = xunit
  , getActionList  = flip map (select xunit) $ \exe ->
      Action
      { actionQuery      = Nothing
      , actionPattern    = Nothing
      , actionMatch      = Nothing
      , actionExecutable = exe
      }
  }

-- | For each given execution unit, place the input string into the 'Dao.Object.recursiveInput'
-- queue of that 'Dao.Object.ExecUnit' and then start a thread running 'execInputStrinsLoop' for
-- that 'Dao.Object.ExecUnit'. This function evaluates syncrhonously, that is, you must wait for all
-- threads created by this string query to complete before this function evaluation returns.
-- The threads are created and registered into the 'Dao.Object.runningExecThreads' field of the
-- 'Dao.Object.Runtime' using 'taskRegisterThreads'. Then, 'taskWaitThreadLoop' is called to wait
-- for the string execution to complete.
runStringQuery :: UStr -> [ExecUnit] -> Run ()
runStringQuery inputString xunits = dStack xloc "runStringsQuery" $ do
  task <- fmap taskForExecUnits ask
  taskRegisterThreads task $ do
    runtime <- ask
    forM xunits $ \xunit -> do
      dModifyMVar_ xloc (recursiveInput xunit) (return . (++[inputString]))
      dFork forkIO xloc "runStringQuery" $ do
        flip (dCatch xloc) (\ (SomeException _) -> return ()) $ do
          dStack xloc "Run \"BEGIN\" scripts." $
            waitAll (return (getBeginEndScripts preExec xunit)) >>= liftIO . evaluate
          dStack xloc "Call execInputStringsLoop" $
            execInputStringsLoop xunit >>= liftIO . evaluate -- Run RULES and PATTERNS
          dStack xloc "Run \"END\" scripts." $
            waitAll (return (getBeginEndScripts postExec xunit)) >>= liftIO . evaluate
        completedThreadInTask task
  taskWaitThreadLoop task

-- | Clears any string queries waiting in the 'Dao.Object.recurisveInput' of an
-- 'Dao.Object.ExecUnit'.
clearStringQueries :: ExecUnit -> Run ()
clearStringQueries xunit = dModifyMVar_ xloc (recursiveInput xunit) (\_ -> return [])

-- | This is the main input loop. Pass an input function callback to be called on every loop.
daoInputLoop :: (Run (Maybe UStr)) -> Run ()
daoInputLoop getString = ask >>= loop >> daoShutdown where
  loop runtime = do
    inputString <- getString
    case inputString of
      Nothing          -> return ()
      Just inputString -> dStack xloc ("(daoInputLoop "++show inputString++")") $ do
        xunits <- fmap (concatMap isProgramFile . M.elems) (dReadMVar xloc (pathIndex runtime))
        mapM_ clearStringQueries xunits
        let task = taskForExecUnits runtime
        runStringQuery inputString xunits
        loop runtime

-- | Evaluates the @TAKEDOWN@ scripts for every presently loaded dao program, and then clears the
-- 'Dao.Object.pathIndex', effectively removing every loaded dao program and idea file from memory.
daoShutdown :: Run ()
daoShutdown = do
  runtime <- ask
  let idx = pathIndex runtime
  xunits <- fmap (concatMap isProgramFile . M.elems) (dReadMVar xloc idx)
  forM_ xunits (setupOrTakedown destructScript)
  dModifyMVar_ xloc idx $ (\_ -> return (M.empty))

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
    return (map ProgramFile (concatMap isProgramFile (M.elems ax)))
  names -> do
    pathTab <- dReadMVar xloc (pathIndex runtime)
    let set msg           = M.fromList . map (\mod -> (mod, error msg))
        request           = set "(selectModules: request files)" names
    imports <- case xunit of
      Nothing    -> return M.empty
      Just xunit -> return $ set "(selectModules: imported files)" $ programImports xunit
    ax <- return $ M.intersection pathTab request
    dMessage xloc ("selected modules:\n"++unlines (map show (M.keys ax)))
    return $ M.elems ax

-- | In the current thread, and using the given 'Runtime' environment, parse an input string as
-- 'Dao.Object.Script' and then evaluate it. This is used for interactive evaluation. The parser
-- used in this function will parse a block of Dao source code, the opening and closing curly-braces
-- are not necessary. Therefore you may enter a semi-colon separated list of commands and all will
-- be executed.
evalScriptString :: ExecUnit -> String -> Run ()
evalScriptString xunit instr = dStack xloc "evalScriptString" $
  void $ flip runExec xunit $ nestedExecStack T.Void $ execScriptBlock $
    case fst (runParser parseInteractiveScript instr) of
      Backtrack     -> error "cannot parse expression"
      PFail tok msg -> error ("error: "++uchars msg++show tok)
      OK expr       -> concatMap (toInterm . unComment) expr

----------------------------------------------------------------------------------------------------
-- src/Dao/Files.hs

-- | Initialize a source code file into the given 'Runtime'. This function checks that the
-- 'Dao.Object.sourceFullPath' is unique in the 'programs' table of the 'Runtime', then evaluates
-- 'initSourceCode' and associates the resulting 'Dao.Evaluator.ExecUnit' with the
-- 'sourceFullPath' in the 'programs' table. Returns the logical "module" name of the script along
-- with an initialized 'Dao.Object.ExecUnit'.
registerSourceCode :: UPath -> AST_SourceCode -> Run (FlowCtrl ExecUnit)
registerSourceCode upath script = dStack xloc "registerSourceCode" $ ask >>= \runtime -> do
  let pathTab = pathIndex runtime
  alreadyLoaded <- fmap (M.lookup upath) (dReadMVar xloc pathTab)
  -- Check to make sure the logical name in the loaded program does not conflict with that
  -- of another loaded previously.
  case alreadyLoaded of
    Just (ProgramFile  f) -> return (FlowOK f)
    Just (DocumentFile f) -> return $ FlowErr $ OList $
      [ OString upath
      , ostr "is already loaded as an \"idea\" file, cannot be loaded as a \"dao\" file"
      ]
    Nothing -> do
      -- Call 'initSourceCode' which creates the 'ExecUnit', then place it in an 'MVar'.
      -- 'initSourceCode' calls 'Dao.Evaluator.programFromSource'.
      xunit <- initSourceCode upath script >>= lift . evaluate
      dModifyMVar_ xloc pathTab $ return . M.insert upath (ProgramFile xunit)
      return (FlowOK xunit)

-- | You should not normally need to call evaluate this function, you should use
-- 'registerSourceCode' which will evaluate this function and also place the
-- 'Dao.Object.AST_SourceCode' into the 'programs' table. This function will use
-- 'Dao.Evaluator.programFromSource' to construct a 'Dao.Object.ExecUnit'
-- and then execute the initialization code for that program, that is, use
-- 'Dao.Evaluator.execScriptExpr' to evaluate every 'Dao.Object.Exec' in the
-- 'Dao.Object.constructScript'. Returns the 'Dao.Evaluator.ExecUnit' used to initialize the
-- program, and the logical name of the program (determined by the "module" keyword in the source
-- code). You need to pass the 'Runtime' to this function because it needs to initialize a new
-- 'Dao.Evaluator.ExecUnit' with the 'programs' and 'runtimeDocList' but these values are not
-- modified.
initSourceCode :: UPath -> AST_SourceCode -> Run ExecUnit
initSourceCode modName script = ask >>= \runtime -> do
  grsrc <- newTreeResource "Program.globalData" T.Void
  xunit <- initExecUnit modName grsrc
  -- An execution unit is required to load a program, so of course, while a program is being
  -- loaded, the program is not in the program table, and is it's 'currentProgram' is 'Nothing'.
  -- result <- runExec (programFromSource grsrc (\_ _ _ -> return True) script) xunit -- OLD
  result <- programFromSource grsrc script xunit -- NEW
  case result of
    FlowErr  obj   -> error ("script err: "++showObj obj)
    FlowOK   xunit -> do
      -- Run all initializer scripts (denoted with the @SETUP@ rule in the Dao language).
      setupOrTakedown constructScript xunit
      -- Place the initialized module into the 'Runtime', mapping to the module's handle.
      return xunit
    FlowReturn _   ->
      error "INTERNAL ERROR: source code evaluation returned before completion"

-- | Load a Dao script program from the given file handle, return a 'Dao.Object.AST_SourceCode' object.
-- Specify a path to be used when reporting parsing errors. Does not register the source code into
-- the given runtime.
sourceFromHandle :: UPath -> Handle -> Run (FlowCtrl AST_SourceCode)
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
    FlowReturn  _ -> error "registerSourceFromHandle: sourceFromHandle evaluated to FlowReturn"
    FlowErr   err -> error ("registerSourceFromHandle: "++show err)

-- | Updates the 'Runtime' to include the Dao source code loaded from the given 'FilePath'. This
-- function tries to load a file in three different attempts: (1) try to load the file as a binary
-- @('Dao.Document.Document' 'Dao.Evaluator.DocData')@ object. (2) Try to load the file as a
-- binary @'Dao.Object.AST_SourceCode'@ object. (3) Treat the file as text (using the current locale set
-- by the system, e.g. @en.UTF-8@) and parse a 'Dao.Object.AST_SourceCode' object using
-- 'Dao.Object.Parsers.source'. If all three methods fail, an error is thrown. Returns
-- the 'TypedFile', although all source code files are returned as 'PrivateType's. Use
-- 'asPublic' to force the type to be a 'PublicType'd file. If the nothing exists at the file path,
-- the file is marked to be created and an empty document is created in memory.
loadFilePath :: FilePath -> Run (FlowCtrl File)
loadFilePath path = dontLoadFileTwice (ustr path) $ \upath -> do
  dPutStrErr xloc ("Lookup file path "++show upath)
  h <- liftIO (try (openFile path ReadMode)) :: Run (Either IOException Handle)
  case h of
    Left  _ -> do -- create a new file if it does not exist
      idx  <- fmap pathIndex ask
      rsrc <- newDocResource ("open file: "++show path) T.Void
      let file = DocumentFile rsrc
      dModifyMVar_ xloc idx (return . M.insert upath file)
      return (FlowOK file)
    Right h -> do
      zero <- lift (hGetPosn h)
      enc  <- lift (hGetEncoding h)
      -- First try to load the file as a binary program file, and then try it as a binary data file.
      doc  <- catchErrorCall (ideaLoadHandle upath h) :: Run (Either ErrorCall DocResource)
      file <- case doc of
        Right doc -> return (FlowOK (DocumentFile doc))
        Left  _   -> do -- The file does not seem to be a document, try parsing it as a script.
          lift (hSetPosn zero >> hSetEncoding h (fromMaybe localeEncoding enc))
          fmap (fmap ProgramFile) (registerSourceFromHandle upath h)
      liftIO $! (evaluate file >> hClose h >> return file)

-- | If any changes have been made to the file, write these files to persistent storage.
writeFilePath :: FilePath -> Run (FlowCtrl ())
writeFilePath path = do
  let upath = ustr path
  idx  <- fmap pathIndex ask
  file <- fmap (M.lookup upath) (dReadMVar xloc idx)
  case file of
    Nothing -> return $ FlowErr $ OList $
      [ostr "cannot write, file path has not been opened", OString upath]
    Just file -> case file of
      ProgramFile  _   -> return $ FlowErr $ OList $
        [ostr "cannot write, file path is opened as a module", OString upath]
      DocumentFile doc -> do
        let res = resource doc
        doc <- fmap fst (dReadMVar xloc res)
        case doc of
          NotStored tree -> do
            let doc = initDoc tree
            liftIO (B.encodeFile path tree >>= evaluate)
            dModifyMVar_ xloc res (\ (_, locked) -> return (doc, locked))
            return (FlowOK ())
          _ -> do -- StoredFile
           liftIO (B.encodeFile path doc >>= evaluate) 
           dModifyMVar_ xloc res (\ (_, locked) -> return (doc{docModified=0}, locked))
           return (FlowOK ())

unloadFilePath :: FilePath -> Run ()
unloadFilePath path = do
  let upath = ustr path
  idx  <- fmap pathIndex ask
  file <- fmap (M.lookup upath) (dReadMVar xloc idx)
  case file of
    Nothing   -> return ()
    Just file -> dModifyMVar_ xloc idx (return . M.delete upath)

-- | When a program is loaded, and when it is released, any block of Dao code in the source script
-- that is denoted with the @SETUP@ or @TAKEDOWN@ rules will be executed. This function performs
-- that execution in the current thread.
setupOrTakedown :: (ExecUnit -> [[ScriptExpr]]) -> ExecUnit -> Run ()
setupOrTakedown select xunit = ask >>= \runtime ->
  forM_ (select xunit) $ \block -> runExec (execGuardBlock block) xunit >>= lift . evaluate

