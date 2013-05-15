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
-- import           Dao.Token
import           Dao.NewParser
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
import           Control.Monad.Error
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
tra :: Monad m => String -> r -> m r
tra msg r = trace msg (return ()) >> return r

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

setupExecutable :: [ScriptExpr] -> Exec Executable
setupExecutable scrp = do
  -- do meta-expression evaluation right here and now ('Dao.Object.MetaEvalExpr')
  scrp <- mapM evalMetaExpr scrp
  -- create the 'Data.IORef.IORef' for storing static variables
  staticRsrc <- liftIO (newIORef M.empty)
  return $
    Executable
    { origSourceCode = scrp
    , staticVars     = staticRsrc
    , executable     = execScriptBlock scrp >>= liftIO . evaluate
    }

runExecutable :: T_tree -> Executable -> Exec (Maybe Object)
runExecutable initStack exe = local (\xunit -> xunit{currentExecutable = Just exe}) $!
  execFuncPushStack initStack (executable exe >>= liftIO . evaluate)

-- | Given a list of arguments, matches these arguments to the given subroutine's
-- 'Dao.Object.Pattern'. If it matches, the 'Dao.Object.getSubExecutable' of the 'Dao.Object.Executable'
-- is evaluated with 'runExecutable'. If the pattern does not match, an empty list is returned to the
-- 'Dao.Object.Exec' monad, which allows multiple 'Dao.Object.Subroutine's to be tried before
-- evaluating to an error in the calling context. The arguments to this function should be produced by
-- 'evalObjectExprWithLoc', producing a list of values that might contain not-yet-dereferenced local
-- varaibles. If the 'Dao.Object.Suroutine' is a 'Dao.Object.MacroFunc', these object values are
-- passed without being dereferenced at all. Otherwise, each argument will be dereferenced with
-- 'objectDeref' to produce the values that are to be "passed" to the function. The values passed
-- are then bound to the local variables using 'Dao.Object.Pattern.matchObjectList', and the
-- resulting local variables map is set before executing the 'Dao.Object.Subroutine's
-- 'Dao.Object.Executable'.
runSubroutine :: [(Location, Object)] -> Subroutine -> Exec (Maybe Object)
runSubroutine args sub = do
  args <- case sub of
    MacroFunc _ _ -> return (map snd args) -- do NOT dereference the arguments for a macro function.
    _             -> mapM objectDeref args
  case evalMatcher (matchObjectList (argsPattern sub) args >> gets matcherTree) of
    OK   tree -> runExecutable tree (getSubExecutable sub)
    Backtrack -> return Nothing
    PFail ref -> procErr (ORef ref)

-- | Very simply executes every given script item. Does not use catchReturnObj, does not use
-- 'nestedExecStack'. CAUTION: you cannot assign to local variables unless you call this method
-- within the 'nestedExecStack' or 'execFuncPushStack' functions. Failure to do so will cause a stack
-- underflow exception. Always returns 'Data.Maybe.Nothing'.
execScriptBlock :: [ScriptExpr] -> Exec (Maybe a)
execScriptBlock block = mapM_ execScriptExpr block >> return Nothing

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code found in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [ScriptExpr] -> Exec ()
execGuardBlock block = void (execFuncPushStack T.Void (execScriptBlock block) >> return ())

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
execFuncPushStack :: T_tree -> Exec (Maybe Object) -> Exec (Maybe Object)
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
execHeapLookup :: [Name] -> Exec (Maybe Object)
execHeapLookup name = ask >>= \xunit -> lift $
  readResource (globalData xunit) name

-- | Lookup an object in the 'globalData' for this 'ExecUnit'.
execHeapUpdate :: [Name] -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
execHeapUpdate name runUpdate = ask >>= \xunit ->
  inEvalDoUpdateResource (globalData xunit) name runUpdate

execHeapDefine :: [Name] -> Object -> Exec (Maybe Object)
execHeapDefine name obj = execHeapUpdate name (return . const (Just obj))

execHeapDelete :: [Name] -> Object -> Exec (Maybe Object)
execHeapDelete name obj = execHeapUpdate name (return . const Nothing)

-- | Lookup a reference value in the durrent document, if the current document has been set with a
-- "with" statement.
curDocVarLookup :: [Name] -> Exec (Maybe Object)
curDocVarLookup name = do
  xunit <- ask
  case currentWithRef xunit of
    Nothing                      -> return Nothing
    Just file@(DocumentFile res) -> lift $
      readResource res (currentBranch xunit ++ name)
    _ -> error $ concat $
           [ "current document is not an idea file, cannot lookup reference "
           , intercalate "." (map uchars name)
           ]

-- | Update a reference value in the durrent document, if the current document has been set with a
-- "with" statement.
curDocVarUpdate :: [Name] -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
curDocVarUpdate name runUpdate = do
  xunit <- ask
  case currentWithRef xunit of
    Nothing                  -> return Nothing
    Just file@(DocumentFile res) ->
      inEvalDoUpdateResource res (currentBranch xunit ++ name) runUpdate
    _ -> error $ concat $
           [ "current document is not an idea file, cannot update reference "
           , intercalate "." (map uchars name)
           ]

curDocVarDefine :: [Name] -> Object -> Exec (Maybe Object)
curDocVarDefine ref obj = curDocVarUpdate ref (return . const (Just obj))

curDocVarDelete :: [Name] -> Object -> Exec (Maybe Object)
curDocVarDelete ref obj = curDocVarUpdate ref (return . const Nothing)

-- | Lookup a value in the 'execStack'.
localVarLookup :: Name -> Exec (Maybe Object)
localVarLookup sym = fmap execStack ask >>= lift . dReadMVar xloc >>=
  return . msum . map (T.lookup [sym]) . mapList

-- | Apply an altering function to the map at the top of the local variable stack.
localVarUpdate :: Name -> (Maybe Object -> Maybe Object) -> Exec (Maybe Object)
localVarUpdate name alt = ask >>= \xunit -> lift $
  dModifyMVar xloc (execStack xunit) $ \ax -> case mapList ax of
    []   -> stack_underflow
    a:ax ->
      let obj = alt (T.lookup [name] a)
      in  return (Stack (T.update [name] (const obj) a : ax), obj)

-- | Force the local variable to be defined in the top level 'execStack' context, do not over-write
-- a variable that has already been defined in lower in the context stack.
localVarDefine :: Name -> Object -> Exec (Maybe Object)
localVarDefine name obj = localVarUpdate name (const (Just obj))

localVarDelete :: Name -> Exec (Maybe Object)
localVarDelete nm = localVarUpdate nm (const Nothing)

staticVarLookup :: Name -> Exec (Maybe Object)
staticVarLookup nm = do
  exe <- fmap (currentExecutable >=> return . staticVars) ask
  case exe of
    Nothing  -> return Nothing
    Just exe -> liftIO (readIORef exe) >>= return . M.lookup nm

staticVarUpdate :: Name -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
staticVarUpdate nm upd = do
  ref <- fmap (currentExecutable >=> return . staticVars) ask
  case ref of
    Nothing  -> return Nothing
    Just ref -> do
      val <- lift (lift (readIORef ref)) >>= return . (M.lookup nm) >>= upd
      lift (lift (modifyIORef ref (M.update (const val) nm)))
      return val

staticVarDefine :: Name -> Object -> Exec (Maybe Object)
staticVarDefine nm obj = staticVarUpdate nm (return . const (Just obj))

staticVarDelete :: Name -> Exec (Maybe Object)
staticVarDelete nm = staticVarUpdate nm (return . const Nothing)

-- | Lookup an object, first looking in the current document, then in the 'globalData'.
globalVarLookup :: [Name] -> Exec (Maybe Object)
globalVarLookup ref = ask >>= \xunit ->
  (if isJust (currentWithRef xunit) then curDocVarLookup else execHeapLookup) ref

globalVarUpdate :: [Name] -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
globalVarUpdate ref runUpdate = ask >>= \xunit ->
  (if isJust (currentWithRef xunit) then curDocVarUpdate else execHeapUpdate) ref runUpdate

-- | To define a global variable, first the 'currentWithRef' is checked. If it is set, the variable
-- is assigned to the document at the reference location prepending 'currentBranch' reference.
-- Otherwise, the variable is assigned to the 'globalData'.
globalVarDefine :: [Name] -> Object -> Exec (Maybe Object)
globalVarDefine name obj = globalVarUpdate name (return . const (Just obj))

-- | To delete a global variable, the same process of searching for the address of the object is
-- followed for 'globalVarDefine', except of course the variable is deleted.
globalVarDelete :: [Name] -> Exec (Maybe Object)
globalVarDelete name = globalVarUpdate name (return . const Nothing)

qTimeVarLookup :: [Name] -> Exec (Maybe Object)
qTimeVarLookup ref = ask >>= \xunit -> lift (readResource (queryTimeHeap xunit) ref)

qTimeVarUpdate :: [Name] -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
qTimeVarUpdate ref runUpdate = ask >>= \xunit ->
  inEvalDoUpdateResource (queryTimeHeap xunit) ref runUpdate

qTimeVarDefine :: [Name] -> Object -> Exec (Maybe Object)
qTimeVarDefine name obj = qTimeVarUpdate name (return . const (Just obj))

qTimeVarDelete :: [Name] -> Exec (Maybe Object)
qTimeVarDelete name = qTimeVarUpdate name (return . const Nothing)

clearAllQTimeVars :: ExecUnit -> Run ()
clearAllQTimeVars xunit = modifyUnlocked_ (queryTimeHeap xunit) (return . const T.Void)

----------------------------------------------------------------------------------------------------

-- $Built_in_functions
-- Built-in functions, retrieved from an array 'infixOps' or 'prefixOps' by a 'Dao.Object.ArithOp'
-- value, or from 'updateOps' by a 'Dao.Object.UpdateOp' value. Built-in functions check object
-- parameters passed to them with the 'BuiltinOp' monad, which is a fully lazy monad based on
-- 'Dao.Predicate.PValue'.

type BuiltinOp = PValue UStr Object

eval_OR :: Object -> Object -> BuiltinOp
eval_OR = evalBooleans (\a b -> seq a (if a then True else seq b b))

eval_AND :: Object -> Object -> BuiltinOp
eval_AND = evalBooleans (\a b -> seq a (if a then seq b b else False))

-- This function is evaluated by 'infixOps' below to be stored in the array of functions that
-- respond to infix operators. However, because the logical operators 'Dao.Object.OR' and
-- 'Dao.Object.AND' need to be especially lazy so the 'evalObjectExprWithLoc' function which
-- evaluates all 'Dao.Object.Equation' expressions catches 'Dao.Object.OR' and 'Dao.Object.AND' to
-- evaluate these expressions differently. The problem is that in languages in the C family, if the
-- left-hand expression of an @||@ operator evaluates to true, or if the left-hand expression of a
-- @&&@ operator evaluates to false, the right-hand expression is ignored. But in spite of all of
-- Haskell's lazyness, the fact remains that the 'evalObjectExprWithLoc' function evaluates both the
-- right and left hand sides of operators in the 'Exec' monad, which is lifted into the IO monad,
-- and this evaluation occurs before passing the values to the operator function (e.g. this function
-- here). Since they are evaluated in the IO Monad, the order in which evaluation occurs must be
-- honored, and that means both the left and right hand side of the equations will always be
-- evaluated unless special steps are taken to avoid this. Hence this function is not used, it is
-- only here to provide a more complete API.
evalBooleans :: (Bool -> Bool -> Bool) -> Object -> Object -> BuiltinOp
evalBooleans fn a b = return (if fn (objToBool a) (objToBool b) then OTrue else ONull)

asReference :: Object -> PValue UStr Reference
asReference o = case o of
  ORef o -> return o
  _      -> mzero

asInteger :: Object -> PValue UStr Integer
asInteger o = case o of
  OWord o -> return (toInteger o)
  OInt  o -> return (toInteger o)
  OLong o -> return o
  _       -> mzero

asRational :: Object -> PValue UStr Rational
asRational o = case o of
  OFloat     o     -> return (toRational o)
  ODiffTime  o     -> return (toRational o)
  OComplex  (o:+0) -> return (toRational o)
  ORatio     o     -> return o
  _                -> mzero

asStringNoConvert :: Object -> PValue UStr UStr
asStringNoConvert o = case o of
  OString o -> return o
  _         -> mzero

asString :: Object -> PValue UStr UStr
asString o = case o of
  OString o -> return o
  o         -> return (ustr (showObj o))

asListNoConvert :: Object -> PValue UStr [Object]
asListNoConvert o = case o of
  OList o -> return o
  _       -> mzero

asList :: Object -> PValue UStr [Object]
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

asHaskellInt :: Object -> PValue UStr Int
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

setToMapFrom :: (Object -> PValue UStr i) -> ([(i, [Object])] -> m [Object]) -> S.Set Object -> m [Object]
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
    if inRange (bounds a) b then return (a!b) else throwError (ustr "array index out of bounds")
  OList   a -> asHaskellInt b >>= \b ->
    let err = throwError (ustr "list index out of bounds")
        ax  = take 1 (drop b a)
    in  if b<0 then err else if null ax then err else return (head ax)
  OIntMap a -> asHaskellInt b >>= \b -> case I.lookup b a of
    Nothing -> throwError (ustr "no item at index requested of intmap")
    Just  b -> return b
  ODict   a -> msum $
    [ do  asStringNoConvert b >>= \b -> case M.lookup b a of
            Nothing -> throwError (ustr (show b++" is not defined in dict"))
            Just  b -> return b
    , do  asReference b >>= \b -> case b of
            LocalRef  b -> case M.lookup b a of
              Nothing -> throwError (ustr (show b++" is not defined in dict"))
              Just  b -> return b
            GlobalRef bx -> loop [] a bx where
              err = throwError (ustr (show b++" is not defined in dict"))
              loop zx a bx = case bx of
                []   -> return (ODict a)
                b:bx -> case M.lookup b a of
                  Nothing -> err
                  Just  a -> case a of
                    ODict a -> loop (zx++[b]) a bx
                    _       ->
                      if null bx
                        then return a
                        else throwError (ustr (show (GlobalRef zx)++" does not point to dict object"))
    ]
  OTree   a -> msum $ 
    [ asStringNoConvert b >>= \b -> done (T.lookup [b] a)
    , asReference b >>= \b -> case b of
        LocalRef  b  -> done (T.lookup [b] a)
        GlobalRef bx -> done (T.lookup bx  a)
    ] where
        done a = case a of
          Nothing -> throwError (ustr (show b++" is not defined in struct"))
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
  Nothing -> throwError (ustr (show b++" cannot be appended to "++show a))
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
  -- , o OR    (evalBooleans (||)) -- These actually probably wont be evaluated. Locgical and/or is a
  -- , o AND   (evalBooleans (&&)) -- special case to be evaluated in 'evalObjectExprWithLoc'.
  , o OR    (error (e "OR" ))
  , o AND   (error (e "AND"))
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
  where
    e msg = "logical-" ++ msg ++
      " operator should have been evaluated within the 'evalObjectExprWithLoc' function."

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
  PFail   msg -> procErr $ OList [OString msg, ostr "is not a string"]
  where
    check (i, o) = case o of
      OString o -> return o
      _         -> PFail (ustr "requires string parameter, param number")

-- | Given an object, if it is a string return the string characters. If it not a string,
-- depth-recurse into it and extract strings, or if there is an object into which recursion is not
-- possible, pretty-print the object and return the pretty-printed string. The first integer
-- parameter is a depth limit, if recursion into the object exceeds this limit, recursion no longer
-- steps into these objects, the strings returned are the pretty-printed representation of the
-- objects. A pair of 'Data.Either.Either's are returned, references are 'Data.Either.Left',
-- 'Prelude.String's are 'Data.Either.Right'. References are accompanied with their depth so you can
-- choose whether or not you want to dereference or pretty-print them.
getStringsToDepth :: Int -> Object -> [Either (Int, Reference) String]
getStringsToDepth maxDepth o = loop 0 maxDepth o where
  loop depth remDep o = case o of
    OString   o -> return (Right (uchars o))
    OList    ox -> recurse o ox
    OSet     ox -> recurse o (S.elems ox)
    OArray   ox -> recurse o   (elems ox)
    ODict    ox -> recurse o (M.elems ox)
    OIntMap  ox -> recurse o (I.elems ox)
    OPair (a,b) -> recurse o [a,b]
    ORef     ox -> return (Left (depth, ox))
    o           -> return (Right (prettyShow o))
    where
      recurse o ox =
        if remDep==0
          then  return (Right (prettyShow o))
          else  ox >>= loop (depth+1) (if remDep>0 then remDep-1 else remDep)

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
            obj <- procCatch (readReference ref)
            case obj of
              FlowOK     o -> recurse (maybeToList o)
              FlowReturn o -> recurse (maybeToList o)
              FlowErr  err -> handler ref err

-- | Returns a list of all string objects that can be found from within the given list of objects.
-- This function might fail if objects exist that cannot resonably contain strings. If you want to
-- pretty-print non-string objects, try using 'getStringsToDepth'.
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
    _                 -> PFail (ustr "is not a string value")
  next (i:ix) zx nx ox = loop (0:i:ix) zx nx >>= \zx -> loop (i+1:ix) zx ox
  catch ox = case ox of
    PFail   msg -> procErr $ OList [ostr "function parameter", OString msg]
    Backtrack   -> return []
    OK       ox -> return ox

builtin_print :: DaoFunc
builtin_print = DaoFuncAutoDeref $ \ox_ -> do
  let ox = flip map ox_ $ \o -> case o of
        OString o -> o
        o         -> ustr (showObj o)
  lift (lift (mapM_ (putStrLn . uchars) ox))
  return $ Just $ OList $ map OString ox

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
  [a] -> fmap Just (ref a)
  _   -> procErr $ ostr ("\""++nm++"\" must be used on some reference value.")
  where
    msg = ostr ("cannot use as a "++nm++" reference")
    ref a = case a of
      ORef a -> case nm of
        "global" -> case a of
          GlobalRef    a  -> return $ ORef (GlobalRef a)
          LocalRef     a  -> return $ ORef (GlobalRef [a])
          StaticRef    a  -> return $ ORef (GlobalRef [a])
          QTimeRef     ax -> return $ ORef (GlobalRef ax)
          FileRef    _ ax -> return $ ORef (GlobalRef ax)
          a -> procErr $ OList [ORef a, msg]
        "local" -> case a of
          GlobalRef    [a] -> return $ ORef (LocalRef a)
          LocalRef      a  -> return $ ORef (LocalRef a)
          StaticRef     a  -> return $ ORef (LocalRef a)
          QTimeRef     [a] -> return $ ORef (LocalRef a)
          FileRef    _ [a] -> return $ ORef (LocalRef a)
          _ -> procErr $ OList [ORef a, msg]
        "qtime" -> case a of
          GlobalRef    ax -> return $ ORef (QTimeRef ax)
          LocalRef     a  -> return $ ORef (QTimeRef [a])
          StaticRef    a  -> return $ ORef (QTimeRef [a])
          QTimeRef     ax -> return $ ORef (QTimeRef ax)
          FileRef    _ ax -> return $ ORef (QTimeRef ax)
        "static" -> case a of
          GlobalRef    [a] -> return $ ORef (StaticRef a)
          LocalRef      a  -> return $ ORef (StaticRef a)
          StaticRef     a  -> return $ ORef (StaticRef a)
          QTimeRef     [a] -> return $ ORef (StaticRef a)
          FileRef    _ [a] -> return $ ORef (StaticRef a)
      _ -> procErr $ OList [msg, a]

-- queue strings for recursive string execution, does not execute in the current thread.
builtin_exec :: DaoFunc
builtin_exec = builtin_exec_do (sendStringsToPrograms False)

-- retrieve matching actions for given strings, execute all actions recursively in the current
-- thread as though they were ordinary function calls.
builtin_do :: DaoFunc
builtin_do = builtin_exec_do execStringsAgainst

builtin_exec_do :: ([UStr] -> [UStr] -> Exec ()) -> DaoFunc
builtin_exec_do execFunc = DaoFuncAutoDeref $ \ox -> do
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
  execFunc selectFiles execStrings
  return $ Just $ OList (map OString execStrings)

-- Returns the current string query.
builtin_doing :: DaoFunc
builtin_doing = DaoFuncAutoDeref $ \ox ->
  fmap currentQuery ask >>= \query -> case query of
    Nothing    -> return (Just ONull)
    Just query -> case ox of
      []          -> return $ Just $ OString query
      [OString o] -> return $ Just $ boolToObj (o==query)
      [OSet    o] -> return $ Just $ boolToObj (S.member (OString query) o)
      _ -> procErr $ OList $
        [ostr "doing() must take as parameers a single string, a single set, or nothing", OList ox]

-- join string elements of a container, pretty prints non-strings and joins those as well.
builtin_join :: DaoFunc
builtin_join = DaoFuncAutoDeref $ \ox -> case ox of
  [OString j, a] -> joinWith (uchars j) a
  [a]            -> joinWith "" a
  _ -> procErr $ OList $
    [ostr "join() function requires one or two parameters", OList ox]
  where
    joinWith j =
      fmap (Just . OString . ustr . intercalate j) . derefStringsToDepth (\ _ o -> procErr o) 1 1

builtin_open :: DaoFunc
builtin_open = DaoFuncAutoDeref $ \ox -> case ox of
  [OString path] -> do
    file <- inExecEvalRun (loadFilePath (uchars path)) >>= joinFlowCtrl
    return $ Just $
      ORef $ case file of
        ProgramFile  _ -> ProgramRef{progID=path, subRef=GlobalRef{globalRef=[]}}
        DocumentFile _ -> FileRef{filePath=path, globalRef=[]}
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
    unload path = inExecEvalRun (runFunc (uchars path)) >>= joinFunc >> return (Just OTrue)
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
    then  procErr (ostr "call function must have at least one argument")
    else  case args of
            OScript fn : args -> do
              obj <- runSubroutine (map (\arg -> (LocationUnknown, arg)) args) fn
              case obj of
                Just obj -> return (Just obj)
                Nothing  -> procErr $ OList $
                  [ ostr "incorrect parameters passed to lambda call"
                  , OScript fn, OList args
                  ]
            obj:args          -> procErr $ OList $
              [ ostr "first argument to \"call\" function must evaluate to a callable object"
              , obj, OList args
              ]

builtin_check_ref :: DaoFunc
builtin_check_ref = DaoFuncNoDeref $ \args -> do
  fmap (Just . boolToObj . and) $ forM args $ \arg -> case arg of
    ORef (MetaRef _) -> return True
    ORef          o  -> readReference o >>= return . isJust >>= \a -> return a
    o                -> return True

builtin_delete :: DaoFunc
builtin_delete = DaoFuncNoDeref $ \args -> do
  forM_ args $ \arg -> case arg of
    ORef o -> void $ updateReference o (const (return Nothing))
    _      -> return ()
  return (Just ONull)

-- | The map that contains the built-in functions that are used to initialize every
-- 'Dao.Object.ExecUnit'.
initBuiltinFuncs :: M.Map Name DaoFunc
initBuiltinFuncs = let o a b = (ustr a, b) in M.fromList $
  [ o "print"   builtin_print
  , o "call"    builtin_call
  , o "do"      builtin_do
  , o "exec"    builtin_exec
  , o "doing"   builtin_doing
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

-- | Will return any value from the 'Dao.Object.ExecUnit' environment associated with a
-- 'Dao.Object.Reference'.
readReference :: Reference -> Exec (Maybe Object)
readReference ref = case ref of
    IntRef     i     -> {- trace ("read int ref: "   ++show i  ) $ -} fmap Just (evalIntRef i)
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
updateReference :: Reference -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
updateReference ref modf = do
  xunit <- ask
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

-- | Convert a 'Dao.Token.Location' to an 'Dao.Object.Object' value.
errAt :: Location -> [Object]
errAt loc = case loc of
  LocationUnknown -> []
  loc -> [ OPair (OWord (fromIntegral (startingLine loc)), OWord (fromIntegral (startingColumn loc)))
         , OPair (OWord (fromIntegral (endingLine   loc)), OWord (fromIntegral (endingColumn   loc)))
         ]

-- | Evaluate to 'procErr' if the given 'PValue' is 'Backtrack' or 'PFail'. You must pass a
-- 'Prelude.String' as the message to be used when the given 'PValue' is 'Backtrack'. You can also
-- pass a list of 'Dao.Object.Object's that you are checking, these objects will be included in the
-- 'procErr' value.
--     This function should be used for cases when you have converted 'Dao.Object.Object' to a
-- Haskell value, because 'Backtrack' values indicate type exceptions, and 'PFail' values indicate a
-- value error (e.g. out of bounds, or some kind of assert exception), and the messages passed to
-- 'procErr' will indicate this.
checkPValue :: Location -> String -> [Object] -> PValue UStr a -> Exec a
checkPValue loc altmsg tried pval = case pval of
  OK     a  -> return a
  Backtrack -> procErr $ OList $ ostr "bad data type" :
    (if null altmsg then [] else [OString (ustr altmsg)]) ++ tried
  PFail msg -> procErr $ OList $ ostr "bad data value" :
    errAt loc ++ (if null altmsg then [] else [ostr altmsg]) ++ OString msg : tried

-- | 'evalObjectExprExpr' can return 'Data.Maybe.Nothing', and usually this happens when something has
-- failed (e.g. reference lookups), but it is not always an error (e.g. a void list of argument to
-- functions). If you want 'Data.Maybe.Nothing' to cause an error, evaluate your
-- @'Dao.Object.Exec' ('Data.Maybe.Maybe' 'Dao.Object.Object')@ as a parameter to this function.
checkVoid :: String -> Exec (Location, Maybe a) -> Exec (Location, a)
checkVoid msg fn = fn >>= \ (loc, obj) -> case obj of
  Nothing -> procErr $ OList $ errAt loc ++ [ostr msg, ostr "evaluates to a void"]
  Just  a -> return (loc, a)

----------------------------------------------------------------------------------------------------

-- | Convert a single 'ScriptExpr' into a function of value @'Exec' 'Dao.Object.Object'@.
execScriptExpr :: ScriptExpr -> Exec ()
execScriptExpr script = case script of
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  EvalObject  o             loc -> unless (isNO_OP o) (void (evalObjectExpr o))
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  IfThenElse  ifn  thn  els loc -> nestedExecStack T.Void $ do
    (loc, ifn) <- checkVoid "conditional expression to if statement" (evalObjectExprDerefWithLoc ifn)
    execScriptBlock (if objToBool ifn then thn else els)
    return ()
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  TryCatch try  name  catch loc -> do
    ce <- procCatch (nestedExecStack T.Void (execScriptBlock try))
    void $ case ce of
      FlowErr o -> nestedExecStack (T.insert [name] o T.Void) (execScriptBlock catch)
      ce        -> joinFlowCtrl ce
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ForLoop varName inObj thn loc -> nestedExecStack T.Void $ do
    inObj <- evalObjectExprDeref inObj
    let scrpExpr expr = case expr of
          ContinueExpr contin condition loc  -> do
            obj <- evalObjectExprDeref condition
            case obj of
              Nothing  -> return contin
              Just obj -> return ((if contin then id else not) (objToBool obj))
          _                                  -> execScriptExpr expr >> return True
        execBlock thn =
          if null thn -- end of the "for" script block
            then  return True -- signals the 'loop' to loop again if there are items left
            else  do
              contin <- scrpExpr (head thn)
              if contin then execBlock (tail thn) else return False
        loop thn name ix = case ix of
          []   -> return ()
          i:ix -> localVarDefine name i >> execBlock thn >>= flip when (loop thn name ix)
        err ex = procErr $ OList $ errAt loc ++ ex
    case inObj of
      Nothing    -> err [ostr "object over which to iterate evaluates to a void"]
      Just inObj -> case asList inObj of
        OK     ix -> loop thn varName ix
        Backtrack -> err [inObj, ostr "cannot be represented as list"]
        PFail msg -> err [inObj, OString msg]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  WhileLoop    co   scrp    loc -> outerLoop where
    check obj = fmap (fromMaybe False . fmap objToBool) (evalObjectExprDeref obj)
    outerLoop = do
      condition <- check co
      if condition
        then  innerLoop scrp >>= \contin -> if contin then outerLoop else return ()
        else  return ()
    innerLoop ax = case ax of
      []   -> return True
      a:ax -> case a of
        ContinueExpr  contin co loc -> do
          co <- check co
          if co then if contin then return True else return False else innerLoop ax
        _                           -> execScriptExpr a >> innerLoop ax
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ContinueExpr a    _       loc -> procErr $ ostr $
    '"':(if a then "continue" else "break")++"\" expression is not within a \"for\" or \"while\" loop"
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ReturnExpr returnStmt obj loc -> do
    o <- evalObjectExprDeref obj :: Exec (Maybe Object)
    if returnStmt then joinFlowCtrl (FlowReturn o) else procErr (fromMaybe ONull o)
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  WithDoc   lval    thn     loc -> nestedExecStack T.Void $ do
    lval <- evalObjectExpr lval
    let setBranch ref xunit = return (xunit{currentBranch = ref})
        setFile path xunit = do
          file <- lift (fmap (M.lookup path) (dReadMVar xloc (execOpenFiles xunit)))
          case file of
            Nothing  -> procErr $ OList $ map OString $
              [ustr "with file path", path, ustr "file has not been loaded"]
            Just file -> return (xunit{currentWithRef = Just file})
        run upd = ask >>= upd >>= \r -> local (const r) (execScriptBlock thn)
    void $ case lval of
      Just lval -> case lval of
        ORef (GlobalRef ref)    -> run (setBranch ref)
        ORef (FileRef path [])  -> run (setFile path)
        ORef (FileRef path ref) -> run (setFile path >=> setBranch ref)
        _ -> procErr $ OList $ errAt loc ++
          [ostr "operand to \"with\" statement is not a reference type"]
      Nothing -> procErr $ OList $ errAt loc ++
        [ostr "target of \"with\" statement evaluates to a void"]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

-- | Runs through a 'Dao.Object.ScriptExpr' and any 'Dao.Object.ObjectExpr' inside of it evaluating
-- every 'Dao.Object.MetaEvalExpr' and replacing it with the evaluation result.
evalMetaExpr :: ScriptExpr -> Exec ScriptExpr
evalMetaExpr script = case script of
  EvalObject   o             loc -> liftM2 EvalObject   (me o)                       (r loc)
  IfThenElse   o    thn els  loc -> liftM4 IfThenElse   (me o)   (ms thn)  (ms els)  (r loc)
  TryCatch     try  nm  ctch loc -> liftM4 TryCatch     (ms try) (r nm)    (ms ctch) (r loc)
  ForLoop      nm   o   loop loc -> liftM4 ForLoop      (r nm)   (me o)    (ms loop) (r loc)
  WhileLoop    o    loop     loc -> liftM3 WhileLoop    (me o)   (ms loop)           (r loc)
  ContinueExpr bool o        loc -> liftM3 ContinueExpr (r bool) (me o)              (r loc)
  ReturnExpr   bool o        loc -> liftM3 ReturnExpr   (r bool) (me o)              (r loc)
  WithDoc      o    loop     loc -> liftM3 WithDoc      (me o)   (ms loop)           (r loc)
  where
    r = return -- alias for return
    me o = case o of -- meta-eval an object
      MetaEvalExpr o loc -> evalObjectExprDerefWithLoc o >>= \ (loc, o) -> return $ case o of
        Nothing -> VoidExpr
        Just  o -> Literal o loc
      o                  -> return o
    ms = mapM evalMetaExpr -- meta-eval a script

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

-- | Like 'objectDerefWithLoc' but only uses the location if there is an error and does not return
-- the location.
objectDeref :: (Location, Object) -> Exec Object
objectDeref = fmap snd . objectDerefWithLoc

-- | If an 'Dao.Object.Object' value is a 'Dao.Object.Reference' (constructed with
-- 'Dao.Object.ORef'), then the reference is looked up using 'readReference'. Otherwise, the object
-- value is returned. This is used to evaluate every reference in an 'Dao.Object.ObjectExpr'.
objectDerefWithLoc :: (Location, Object) -> Exec (Location, Object)
objectDerefWithLoc (loc, obj) = case obj of
  ORef (MetaRef o) -> return (loc, ORef o)
  ORef ref         -> readReference ref >>= \o -> case o of
    Nothing -> procErr $ OList $ errAt loc ++ [obj, ostr "undefined reference"]
    Just o  -> return (loc, o)
  obj              -> return (loc, obj)

evalObjectExpr :: ObjectExpr -> Exec (Maybe Object)
evalObjectExpr expr = fmap snd (evalObjectExprWithLoc expr)

-- | Combines 'evalObjectExprWithLoc' and 'objectDeref' 
evalObjectExprDerefWithLoc :: ObjectExpr -> Exec (Location, Maybe Object)
evalObjectExprDerefWithLoc expr = evalObjectExprWithLoc expr >>= \ (loc, obj) -> case obj of
  Nothing  -> return (loc, Nothing)
  Just obj -> fmap (\ (loc', o) -> (min loc loc', Just o)) (objectDerefWithLoc (loc, obj))

-- | Calls 'evalObjectExprDerefWithLoc' but drops the 'Dao.Token.Location' from the returned pair
-- @('Dao.Token.Location', 'Dao.Object.Object')@, returning just the 'Dao.Object.Object'.
evalObjectExprDeref :: ObjectExpr -> Exec (Maybe Object)
evalObjectExprDeref = fmap snd . evalObjectExprDerefWithLoc

-- | Evaluate an 'ObjectExpr' to an 'Dao.Object.Object' value, and does not de-reference objects of
-- type 'Dao.Object.ORef'
evalObjectExprWithLoc :: ObjectExpr -> Exec (Location, Maybe Object)
evalObjectExprWithLoc obj = case obj of
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  VoidExpr                       -> return (LocationUnknown, Nothing)
    -- ^ 'VoidExpr's only occur in return statements. Returning 'ONull' where nothing exists is
    -- probably the most intuitive thing to do on an empty return statement.
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  Literal     o              loc -> return (loc, Just o)
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  AssignExpr  nm  op  expr   loc -> setloc loc $ do
    (nmloc, nm) <- checkVoid "left-hand side of assignment" (evalObjectExprWithLoc nm)
    let lhs = "left-hand side of "++show op
    nm   <- checkPValue nmloc lhs [nm] (asReference nm)
    (exprloc, expr) <- checkVoid "right-hand side of assignment" (evalObjectExprDerefWithLoc expr)
    updateReference nm $ \maybeObj -> case maybeObj of
      Nothing      -> case op of
              UCONST -> return (Just expr)
              _      -> procErr $ OList $ errAt nmloc ++ [ostr "undefined refence", ORef nm]
      Just prevVal -> fmap Just $
        checkPValue exprloc "assignment expression" [prevVal, expr] $ (updatingOps!op) prevVal expr
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  FuncCall   op   args       loc -> setloc loc $ do -- a built-in function call
    bif  <- fmap builtinFuncs ask
    let ignoreVoids (loc, obj) = case obj of
          Nothing  -> []
          Just obj -> [(loc, obj)]
    args <- fmap (concatMap ignoreVoids) (mapM evalObjectExprWithLoc args) :: Exec [(Location, Object)]
    let allCombinations ox args = -- given the non-deterministic arguments,
          if null args    -- create every possible combination of arguments
            then  return ox
            else  head args >>= \arg -> allCombinations (ox++[arg]) (tail args)
    case M.lookup op bif of
      Nothing -> do -- no built-ins by the 'op' name
        fn   <- lookupFunction "function call" op
        ~obj <- mapM (runSubroutine args) fn
        case msum obj of
          Just obj -> return (Just obj)
          Nothing  -> procErr $ OList $ errAt loc ++
            [ostr "incorrect parameters passed to function", OString op, OList (map snd args)]
      Just bif -> case bif of -- 'op' references a built-in
        DaoFuncNoDeref   bif -> bif (map snd args)
        DaoFuncAutoDeref bif -> mapM objectDeref args >>= bif
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ParenExpr     _     o      loc -> evalObjectExprWithLoc o
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ArraySubExpr  o     i      loc -> setloc loc $ do
    (oloc, o) <- checkVoid "operand of subscript expression" (evalObjectExprDerefWithLoc o)
    (iloc, i) <- checkVoid "index of subscript expression" (evalObjectExprWithLoc i)
    case evalSubscript o i of
      OK      a -> return (Just a)
      PFail msg -> procErr (OString msg)
      Backtrack -> procErr (OList $ errAt loc ++ [i, ostr "cannot be used as index of", o])
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  Equation   left' op right' loc -> setloc loc $ do
    let err1 msg = msg++"-hand operand of "++show op++ "operator "
        evalLeft   = checkVoid (err1 "left" ) (evalObjectExprWithLoc left')
        evalRight  = checkVoid (err1 "right") (evalObjectExprWithLoc right')
        derefLeft  = evalLeft  >>= objectDerefWithLoc
        derefRight = evalRight >>= objectDerefWithLoc
        logical isAndOp = fmap Just $ do
          (leftloc, left) <- derefLeft
          if objToBool left
            then  if isAndOp then fmap snd derefRight else return OTrue
            else  if isAndOp then return ONull else fmap snd derefRight
    case op of
      AND -> logical True
      OR  -> logical False
      op  -> do
        ((leftloc, left), (rightloc, right)) <- case op of
          DOT   -> liftM2 (,) evalLeft  evalRight
          POINT -> liftM2 (,) derefLeft evalRight
          _     -> liftM2 (,) derefLeft derefRight
        case (infixOps!op) left right of
          OK result -> return (Just result)
          Backtrack -> procErr $ OList $ concat $
            [ errAt loc, [ostr (show op), ostr "cannot operate on objects of type"]
            , errAt leftloc, [left], errAt rightloc, [right]
            ]
          PFail    msg -> procErr $ OList $ errAt loc ++ [OString msg]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  PrefixExpr op       expr   loc -> setloc loc $ do
    (locexpr, expr) <- checkVoid ("operand to prefix operator "++show op) (evalObjectExprWithLoc expr)
    case (prefixOps!op) expr of
      OK result -> return (Just result)
      Backtrack -> procErr $ OList $
        [ostr (show op), ostr "cannot operate on objects of type", expr]
      PFail    msg -> procErr $ OList [OString msg]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  DictExpr   cons     args   loc -> setloc loc $ do
    let loop insfn getObjVal mp argx  = case argx of
          []       -> return mp
          arg:argx -> case arg of
            AssignExpr ixObj op  new loc -> do
              let err msg = (msg++"-hand side of assignment in "++show cons)
              (ixloc, ixObj) <- checkVoid (err "left") (evalObjectExprDerefWithLoc ixObj)
              ixVal <- checkPValue ixloc (err "right") [ixObj] (getObjVal ixObj)
              (newloc, new ) <- evalObjectExprDerefWithLoc new
              case new of
                Nothing  -> procErr $ OList $ errAt newloc ++
                  [ ostr (show op), ostr "assignment to index", ixObj
                  , ostr "failed, right-hand side evaluates to a void"
                  ]
                Just new -> insfn mp ixObj ixVal op newloc new
              loop insfn getObjVal mp argx
            _ -> error "dictionary constructor contains an expression that is not an assignment"
        assign lookup insert mp ixObj ixVal op newloc new = case lookup ixVal mp of
          Nothing  -> case op of
            UCONST -> return (insert ixVal new mp)
            op     -> procErr $ OList [ostr ("undefined left-hand side of "++show op), ixObj]
          Just old -> case op of
            UCONST -> procErr $ OList [ostr ("twice defined left-hand side "++show op), ixObj]
            op     -> do
              new <- checkPValue newloc (show cons++" assignment expression "++show op) [ixObj, old, new] $
                        (updatingOps!op) old new
              return (insert ixVal new mp)
        intmap = assign I.lookup I.insert
        dict   = assign M.lookup M.insert
    fmap Just $ case () of
      () | cons == ustr "dict"   -> fmap ODict   (loop dict   asStringNoConvert M.empty args)
      () | cons == ustr "intmap" -> fmap OIntMap (loop intmap asHaskellInt      I.empty args)
      () | cons == ustr "list"   -> fmap (OList . concatMap maybeToList) (mapM evalObjectExprDeref args)
      _ -> error ("INTERNAL ERROR: unknown dictionary declaration "++show cons)
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  ArrayExpr  rang  ox        loc -> setloc loc $ case rang of
    (_ : _ : _ : _) -> procErr $ OList $ errAt loc ++
      [ostr "internal error: array range expression has more than 2 arguments"]
    [lo, hi] -> do
      let getIndex msg bnd = do
            (bndloc, bnd) <- checkVoid (msg++" of array declaration") (evalObjectExprDerefWithLoc bnd)
            case fmap fromIntegral (asHaskellInt bnd) of
              OK          i -> return i
              Backtrack     -> procErr $ OList $ errAt bndloc ++
                [ostr msg, ostr "of array declaration does not evaluate to an integer value"]
              PFail     msg -> procErr $ OList $ errAt bndloc ++
                [OString msg, ostr "in array declaration"]
      lo <- getIndex "lower-bound" lo
      hi <- getIndex "upper-bound" hi
      (lo, hi) <- return (if lo<hi then (lo,hi) else (hi,lo))
      ox <- fmap (concatMap maybeToList) (mapM evalObjectExprDeref ox)
      return (Just (OArray (listArray (lo, hi) ox)))
    _ -> procErr $ OList $ errAt loc ++
      [ostr "internal error: array range expression has fewer than 2 arguments"]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  DataExpr   strs            loc -> setloc loc $ case b64Decode (concatMap uchars strs) of
    Right dat       -> return (Just (OBytes dat))
    Left  (ch, loc) -> procErr $ OList $
      [ ostr "invalid character in base-64 data expression", OChar ch
      , ostr "at position", OWord loc
      ]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  LambdaExpr typ argv code   loc -> setloc loc $ fmap Just (evalLambdaExpr typ argv code)
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  MetaEvalExpr expr          loc -> evalObjectExprWithLoc expr
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  where { setloc loc fn = fmap (\o -> (loc, o)) fn }
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

evalLambdaExpr :: LambdaExprType -> [ObjectExpr] -> [ScriptExpr] -> Exec Object
evalLambdaExpr typ argv code = do
  exe <- setupExecutable (code)
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
  _ -> procErr $ OList $ [ostr "does not evaluate to an object pattern"]
  -- TODO: provide a more expressive way to create object patterns from 'Dao.Object.ObjectExpr's

-- | Convert an 'Dao.Object.ObjectExpr' to an 'Dao.Glob.Glob'.
argsToGlobExpr :: ObjectExpr -> Exec Glob
argsToGlobExpr o = case o of
  Literal (OString str) _ -> return (read (uchars str))
  _ -> procErr $ OList $ [ostr "does not evaluate to a \"glob\" pattern"]

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
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    importsLoop :: [TopLevelExpr] -> StateT ExecUnit (ReaderT Runtime IO) (FlowCtrl ExecUnit)
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
      --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
        dx -> scriptLoop dx
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    scriptLoop :: [TopLevelExpr] -> StateT ExecUnit (ReaderT Runtime IO) (FlowCtrl ExecUnit)
    scriptLoop dx = case dx of
      []                                                  -> fmap FlowOK get
      --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
      TopFunc       name       argList    script loc : dx -> do
        result <- mkArgvExe argList script
        case result of
          FlowOK sub -> do
            let alt funcs = mplus (fmap (++[sub]) funcs) (return [sub])
            modify (\xunit -> xunit{topLevelFuncs = M.alter alt name (topLevelFuncs xunit)})
          FlowErr err -> return () -- errors are handled in the 'mkArgvExe' function
        scriptLoop dx
      --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
      TopScript     script                       loc : dx -> do
        modify (\xunit -> xunit{constructScript = constructScript xunit ++ [script]})
        scriptLoop dx
      --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
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
            argv <- lift $ runExec (mapM argsToGlobExpr argList) xunit 
            case liftM2 (,) argv exe of
              FlowOK (argv, exe) -> do
                rules <- gets ruleSet
                let fol tre pat = T.unionWith (++) tre (toTree pat [exe])
                lift $ dModifyMVar_ xloc rules (\patTree -> return (foldl fol patTree argv))
              FlowErr err -> void (storeInitErr err)
            scriptLoop dx
      --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
      EventExpr     evtType    script            loc : dx -> do
        xunit <- get
        exe   <- lift (runExec (setupExecutable script) xunit)
        case exe of
          FlowOK exe ->
            case evtType of
              BeginExprType -> modify $ \xunit -> xunit{preExec  = preExec  xunit ++ [exe]}
              EndExprType   -> modify $ \xunit -> xunit{postExec = postExec xunit ++ [exe]}
              ExitExprType  -> modify $ \xunit ->
                xunit{destructScript = destructScript xunit ++ script}
          FlowReturn obj -> throwFailure obj
          FlowErr    err -> void (storeInitErr err)
        scriptLoop dx
      --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
      Attribute     kindOfAttr attribName        loc : _  -> return $ FlowErr $ OList $
        [ OString kindOfAttr, OString attribName
        , ostr "statement is not at the top of the source file"
        ]
  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    mkExe script = get >>= lift . runExec (setupExecutable (script))
    mkArgvExe argList script = do
      argv <- lift $ flip runExec xunit $ mapM argsToObjPat $ argList
      exe  <- mkExe script
      case liftM2 (,) argv exe of
        FlowOK (argv, exe)  -> return $ FlowOK $
          Subroutine
          { argsPattern      = argv
          , getSubExecutable = exe
          }
        FlowErr    err -> storeInitErr err
        FlowReturn obj -> throwFailure obj
    storeInitErr err = do
      lift $ dModifyMVar_ xloc (uncaughtErrors xunit) (\ex -> return (ex++[err]))
      return (FlowErr err)
    throwFailure :: Maybe Object -> e
    throwFailure obj = error $ concat $
      [ "argsToObjPat returned 'FlowReturn' instead of '(FlowOK [Pattern])'\n"
      , case obj of
          Nothing  -> ""
          Just obj -> "Object returned:\n" ++ prettyShow obj
      ]

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
forkActionGroup :: ActionGroup -> Run ()
forkActionGroup actgrp = dStack xloc ("forkActionGroup ("++show (length (getActionList actgrp))++" items)") $ do
  let xunit = actionExecUnit actgrp
      task  = taskForActions xunit
  taskRegisterThreads task (forM (getActionList actgrp) (forkExecAction (actionExecUnit actgrp)))
  taskWaitThreadLoop task >>= liftIO . evaluate

-- | For every 'Dao.Object.Action' in the 'Dao.Object.ActionGroup', evaluate that
-- 'Dao.Object.Action' in a the current thread but in using the 'Dao.Object.ExecUnit' of the
-- given 'Dao.Object.ActionGroup'.
execActionGroup :: ActionGroup -> Run ()
execActionGroup actgrp = mapM_ (execAction (actionExecUnit actgrp)) (getActionList actgrp)

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
waitAll getActionGroup = getActionGroup >>= forkActionGroup

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
    FlowReturn tox -> match (concatMap extractStringElems (maybeToList tox))
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
selectModules :: Maybe ExecUnit -> [Name] -> Run [ExecUnit]
selectModules xunit names = dStack xloc "selectModules" $ do
  runtime <- ask
  ax <- case names of
    []    -> dReadMVar xloc (pathIndex runtime)
    names -> do
      pathTab <- dReadMVar xloc (pathIndex runtime)
      let set msg           = M.fromList . map (\mod -> (mod, error msg))
          request           = set "(selectModules: request files)" names
      imports <- case xunit of
        Nothing    -> return M.empty
        Just xunit -> return $ set "(selectModules: imported files)" $ programImports xunit
      return (M.intersection pathTab request)
  return (M.elems ax >>= isProgramFile)

-- | In the current thread, and using the given 'Runtime' environment, parse an input string as
-- 'Dao.Object.Script' and then evaluate it. This is used for interactive evaluation. The parser
-- used in this function will parse a block of Dao source code, the opening and closing curly-braces
-- are not necessary. Therefore you may enter a semi-colon separated list of commands and all will
-- be executed.
evalScriptString :: ExecUnit -> String -> Run ()
evalScriptString xunit instr = dStack xloc "evalScriptString" $
  void $ flip runExec xunit $ nestedExecStack T.Void $ execScriptBlock $
    case fst (runParser parseInteractiveScript instr) of
      Backtrack -> error "cannot parse expression"
      PFail tok -> error ("error: "++show tok)
      OK   expr -> concatMap (toInterm . unComment) expr

-- | This is the simplest form of string execution, everything happens in the current thread, no
-- "BEGIN" or "END" scripts are executed. Simply specify a list of programs (as file paths) and a
-- list of strings to be executed against each program.
execStringsAgainst :: [UStr] -> [UStr] -> Exec ()
execStringsAgainst selectPrograms execStrings = do
  xunit <- ask
  otherXUnits <- inExecEvalRun (selectModules (Just xunit) selectPrograms)
  forM_ otherXUnits $ \xunit ->
    forM_ execStrings $ \execString ->
      inExecEvalRun (makeActionsForQuery execString xunit >>= execActionGroup)

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
setupOrTakedown :: (ExecUnit -> [ScriptExpr]) -> ExecUnit -> Run ()
setupOrTakedown select xunit = ask >>= \runtime ->
  void (runExec (execGuardBlock (select xunit)) xunit) >>= lift . evaluate

