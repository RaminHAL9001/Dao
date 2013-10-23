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
import           Dao.Token  hiding (asString)
import           Dao.Parser hiding (shift)
import           Dao.Object
import           Dao.Object.AST
import           Dao.Object.DeepSeq
import           Dao.PPrint
import qualified Dao.Tree as T
import           Dao.Glob
import           Dao.Predicate
import           Dao.Procedural
import           Dao.Resource
import           Dao.Struct

import           Dao.Object.Math
import           Dao.Object.PPrint
import           Dao.Object.Binary
import           Dao.Object.Struct
import           Dao.Object.Parser

import           Control.Exception
import           Control.Applicative
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
import           Data.Dynamic
import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.IntMap as I
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.Binary as B

import           System.IO

import Control.DeepSeq
import Debug.Trace
tra :: Monad m => String -> r -> m r
tra msg r = trace msg (return ()) >> return r

----------------------------------------------------------------------------------------------------

indexObject :: Object -> Object -> Exec Object
indexObject obj idx = case obj of
  OList []  -> execThrow $ OList [ostr "indexing empty list", obj, idx]
  OList lst -> do
    i <- mplus (asInteger idx) $
      execThrow (OList [ostr "must index list with integer", idx, ostr "indexing", obj])
    if i<0
      then  execThrow $ OList [ostr "list index value is negative", idx, ostr "indexing", obj]
      else  case dropWhile ((<i) . fst) (zip [0..] lst) of
              []       -> execThrow $ OList [ostr "index out of bounds", idx, ostr "indexing", obj]
              (_, o):_ -> return o
  OTree tree -> case idx of
    OString                      i   -> case maybeFromUStr i of
      Nothing -> execThrow $ OList [ostr "string does not form valid identifier", OString i]
      Just i  -> doIdx obj idx [i] tree
    ORef (Unqualified (Reference r)) -> doIdx obj idx  r  tree
    _  -> execThrow $ OList [ostr "cannot index tree with value", idx, ostr "indexing", obj]
    where
      doIdx obj idx i t = case T.lookup i t of
        Nothing -> execThrow $ OList [ostr "tree has no branch index: ", idx, ostr "indexing", obj]
        Just  o -> return o
  obj       -> join $ fmap ($idx) $ evalObjectMethod errmsg obj objIndexer where
    errmsg = OList [ostr "cannot index object", obj]

----------------------------------------------------------------------------------------------------

showObj :: PPrintable a => a -> String
showObj = prettyPrint 80 "    "

initExecUnit :: Maybe UPath -> Runtime -> IO ExecUnit
initExecUnit modName runtime = do
  --unctErrs <- dNewMVar xloc "ExecUnit.uncaughtErrors" []
  unctErrs <- newIORef []
  --recurInp <- dNewMVar xloc "ExecUnit.recursiveInput" []
  recurInp <- newIORef []
  --qheap    <- newTreeResource  "ExecUnit.queryTimeHeap" T.Void
  qheap    <- newMVar T.Void
  --global   <- newTreeResource  "ExecUnit.globalData" T.Void
  global   <- newMVar T.Void
  task     <- runReaderT initTask runtime
  --xstack   <- dNewMVar xloc "ExecUnit.execStack" emptyStack
  xstack   <- newIORef emptyStack
  --files    <- dNewMVar xloc "ExecUnit.execOpenFiles" M.empty
  files    <- newIORef M.empty
  --rules    <- dNewMVar xloc "ExecUnit.ruleSet" T.Void
  rules    <- newIORef T.Void
  return $
    ExecUnit
    { parentRuntime      = runtime
    , currentWithRef     = WithRefStore Nothing
    , currentQuery       = Nothing
    , currentPattern     = Nothing
    , currentMatch       = Nothing
    , currentCodeBlock   = CurrentCodeBlock Nothing
    , currentBranch      = []
    , importsTable       = []
    , patternTable       = []
    , topLevelFuncs      = M.empty
    , queryTimeHeap      = QTimeStore qheap
    , globalData         = GlobalStore global
    , taskForActions     = task
    , execStack          = LocalStore xstack
    , execOpenFiles      = files
    , recursiveInput     = recurInp
    , uncaughtErrors     = unctErrs
      ---- items that were in the Program data structure ----
    , programModuleName = modName
    , programImports    = []
    , requiredBuiltins  = []
    , programAttributes = mempty
    , preExec           = []
    , quittingTime      = mempty
    , programTokenizer  = return . tokens . uchars
    , programComparator = (==)
    , postExec          = []
    , ruleSet           = rules
    }

childExecUnit :: Maybe UPath -> Exec ExecUnit
childExecUnit path = ask >>= \xunit -> liftIO (initExecUnit path (parentRuntime xunit))

setupCodeBlock :: CodeBlock -> Exec Subroutine
setupCodeBlock scrp = do
  -- create the 'Data.IORef.IORef' for storing static variables
  staticRsrc <- liftIO (newIORef mempty)
  return $
    Subroutine
    { origSourceCode = scrp
    , staticVars     = staticRsrc
    , executable     = execute scrp >> return Nothing
    }

-- | Simply executes each 'Dao.Object.ScriptExpr' in the
-- 'Dao.Object.CodeBlock', one after the other. Execution does not occur within
-- a 'execNested' because many other expressions which execute
-- 'Dao.Object.CodeBlock's, especially 'Dao.Object.TryCatch' expressions and
-- 'Dao.Object.ForLoop's need to be able to choose when the stack is pushed so
-- they can define temporary local variables.
instance Executable CodeBlock () where { execute (CodeBlock ox) = mapM_ execute ox }
instance Executable CallableCode (Maybe Object) where { execute = execute . codeSubroutine }
instance Executable Subroutine (Maybe Object) where
  execute sub = local (\x->x{currentCodeBlock=CurrentCodeBlock(Just sub)}) $
    catchReturn (execute (origSourceCode sub))

runCodeBlock :: T_tree -> Subroutine -> Exec (Maybe Object)
runCodeBlock initStack exe = local (\xunit -> xunit{currentCodeBlock = CurrentCodeBlock (Just exe)}) $!
  execFuncPushStack initStack (executable exe >>= liftIO . evaluate)

-- | To evaluate an 'Dao.Object.Object' value against a type expression, you can store the
-- 'Dao.Object.Object' into a 'Dao.Object.ParamValue' and into a 'Dao.Object.TyChkExpr' and
-- 'Dao.Object.execute' it. This instance of 'Dao.Object.execute' evaluates a type checking monad
-- computing over the 'Dao.Object.tyChkExpr' in the 'Dao.Object.TyChkExpr'.
instance Executable (TyChkExpr ParamValue) (Maybe Object) where
  execute tc = case tc of
    NotTypeChecked pv          -> return (Just OTrue) -- TODO: this needs to return the 'Dao.Object.anyType', not 'OTrue'.
    TypeChecked    pv _ _      -> return (Just OTrue) -- TODO: evaluate the actual type checking algorithm here
    DisableCheck   pv _ rslt _ -> return (Just rslt)

-- | Matches 'Dao.Object.ParamValue's passed in function calls to 'Dao.Object.TyChkExpr's in
-- function declarations. Returns a pair: 'Prelude.fst' is the value contained in the
-- 'Data.Object.TyChkExpr', 'Prelude.snd' is the most general type value for the object if that type
-- value is less-general or as-general as the 'TyChkExpr' provided, otherwise throws a type
-- exception.
checkType :: TyChkExpr a -> ParamValue -> Exec (a, Object)
checkType tychk val = case tychk of
  NotTypeChecked a              -> return (a, OTrue) -- TODO: should return (a, Just anyType)
  DisableCheck   a _    val _   -> return (a, val)
  TypeChecked    a tychk    loc -> do
    verdict <- execute (TypeChecked val tychk loc)
    case verdict of
      Just typ -> return (a, typ)
      Nothing  -> execThrow $ OList $
        [ ostr "expression", ostr (prettyShow (paramOrigExpr val))
        , ostr "evaluates to", ostr (prettyShow (paramValue val))
        , ostr "which does not match type", ostr (prettyShow tychk)
        ]

matchFuncParams :: ParamListExpr -> [ParamValue] -> Exec T_tree
matchFuncParams (ParamListExpr params _) values = loop T.Void (tyChkItem params) values where
  loop tree ax bx
    | null ax && null bx = return tree
    | null ax || null bx = mzero
    | otherwise          = do
        let param@(ParamExpr  dontDeref tychk _) = head ax
        let value@(ParamValue inObj inObjExpr  ) = head bx
        val       <- (if dontDeref then return else derefObject) inObj
        (name, _) <- checkType tychk  value
        -- Here ^ we ignore the most-general type value returned,
        -- all we care about is whether or not the value matches the type.
        loop (T.insert [name] val tree) (tail ax) (tail bx)

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code found in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [ScriptExpr] -> Exec ()
execGuardBlock block = void (execFuncPushStack T.Void (mapM_ execute block >> return Nothing) >> return ())

-- $BasicCombinators
-- These are the most basic combinators for converting working with the 'ExecUnit' of an
-- 'Exec' monad.

----------------------------------------------------------------------------------------------------
-- $StackOperations
-- Operating on the local stack.

-- | Push a new empty local-variable context onto the stack. Does NOT 'catchReturnObj', so it can be
-- used to push a new context for every level of nested if/else/for/try/catch statement, or to
-- evaluate a macro, but not a function call. Use 'execFuncPushStack' to perform a function call within
-- a function call.
execNested :: T_tree -> Exec a -> Exec a
execNested init exe = do
  (LocalStore stack) <- fmap (execStack) ask
  --lift $ dModifyMVar_ xloc stack (return . stackPush init)
  liftIO $ modifyIORef stack (stackPush init)
  result <- exe
  --lift $ dModifyMVar xloc stack (return . stackPop)
  liftIO $ modifyIORef stack (fst . stackPop)
  return result

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. This function is different from 'nestedExecStak' in that it acually removes the current
-- execution stack so a function call cannot modify the local variables of the function which called
-- it. Furthermore it catches evaluation of a "return" statement allowing the function which called
-- it to procede with execution after this call has returned.
execFuncPushStack :: T_tree -> Exec (Maybe Object) -> Exec (Maybe Object)
execFuncPushStack dict exe = do
  flow <- procCatch $ do
    stackMVar <- liftIO (newIORef (Stack [dict]))
    local (\xunit -> xunit{execStack=LocalStore stackMVar}) exe
  case flow of
    FlowOK     obj -> return obj
    FlowReturn obj -> return obj
    FlowErr    err -> throwError err

----------------------------------------------------------------------------------------------------

-- | Used to evaluate an expression like @$1@, retrieves the matched pattern associated with an
-- integer. Specifically, it returns a list of 'Dao.ObjectObject's where each object is an
-- 'Dao.Object.OString' contained at the integer index of the 'Dao.Glob.matchGaps' of a
-- 'Dao.Glob.Glob'.
--evalIntRef :: Word -> Exec Object
--evalIntRef i = do
--  ma <- asks currentMatch
--  let oi = OInt (fromIntegral i)
--  case ma >>= matchGaps of
--    Nothing ->
--      objectError oi ("currently matching pattern has no variables, cannot evaluate $"++show i)
--    Just ma | i==0 -> return $ OArray $
--      listArray (let (a, b) = bounds ma in (fromIntegral a, fromIntegral b)) $
--        map (OList . map OString) (elems ma)
--    Just ma | inRange (bounds ma) i -> return (OList (map OString (ma!i)))
--    Just ma ->
--      objectError oi $ concat $
--        [ "pattern match variable $"
--        , show i ++ " is out of range "
--        , show (bounds ma)
--        , " in the current pattern match context"
--        ]

_checkRef :: [Name] -> Exec Name
_checkRef nx = case nx of
  [n] -> return n
  nx  -> execThrow $ OList [ostr "bad reference", ORef $ Unqualified $ Reference nx]

instance ExecRef ref => Store (ref (M.Map Name Object)) where
  storeLookup store (Reference ref)     = _checkRef ref >>= \ref -> fmap (M.lookup ref    ) (execReadRef    store)
  storeDefine store (Reference ref) obj = _checkRef ref >>= \ref -> execModifyRef_ store (return . M.insert ref obj)
  storeDelete store (Reference ref)     = _checkRef ref >>= \ref -> execModifyRef_ store (return . M.delete ref    )
  storeUpdate store (Reference ref) upd = execModifyRef  store $ \tree -> do
    r   <- _checkRef ref
    obj <- upd (M.lookup r tree)
    return $ case obj of
      Nothing  -> (M.delete r     tree, Nothing)
      Just obj -> (M.insert r obj tree, Just obj)

instance ExecRef ref => Store (ref T_tree) where
  storeLookup store (Reference ref)     = fmap (T.lookup ref) (execReadRef store)
  storeDefine store (Reference ref) obj = execModifyRef_ store (return . T.insert ref obj)
  storeDelete store (Reference ref)     = execModifyRef_ store (return . T.delete ref    )
  storeUpdate store (Reference ref) upd = execModifyRef  store $ \tree -> do
    obj <- upd (T.lookup ref tree)
    return $ case obj of
      Nothing  -> (T.delete ref     tree, Nothing)
      Just obj -> (T.insert ref obj tree, Just obj)

instance ExecRef ref => Store (ref (Stack Name Object)) where
  storeLookup store (Reference ref)     = execReadRef store >>= flip execReadTopStackItem (return . T.lookup ref)
  storeDefine store (Reference ref) obj = execModifyRef_ store (flip execModifyTopStackItem_ (return . T.insert ref obj))
  storeDelete store (Reference ref)     = execModifyRef_ store (flip execModifyTopStackItem_ (return . T.delete ref    ))
  storeUpdate store (Reference ref) upd = execModifyRef  store $ \stk ->
    execModifyTopStackItem stk $ \tree -> upd (T.lookup ref tree) >>= \obj -> return $ case obj of
      Nothing  -> (T.delete ref     tree, Nothing)
      Just obj -> (T.insert ref obj tree, Just obj)
      -- FIXME: should scan the entire stack, if the reference is defined
      -- anywhere, the variable needs to be updated in that layer of the stack.
      -- If it is defined nowhere, it should be inserted in the top layer.

instance Store LocalStore where
  storeLookup (LocalStore  store) = storeLookup store
  storeDefine (LocalStore  store) = storeDefine store
  storeDelete (LocalStore  store) = storeDelete store
  storeUpdate (LocalStore  store) = storeUpdate store

instance Store GlobalStore where
  storeLookup (GlobalStore store) = storeLookup store
  storeDefine (GlobalStore store) = storeDefine store
  storeDelete (GlobalStore store) = storeDelete store
  storeUpdate (GlobalStore store) = storeUpdate store

instance Store QTimeStore where
  storeLookup (QTimeStore  store) = storeLookup store
  storeDefine (QTimeStore  store) = storeDefine store
  storeDelete (QTimeStore  store) = storeDelete store
  storeUpdate (QTimeStore  store) = storeUpdate store

instance Store CurrentCodeBlock where
  storeLookup (CurrentCodeBlock store) ref     =
    maybe (return Nothing) (\store -> storeLookup store ref    ) (fmap staticVars store)
  storeDefine (CurrentCodeBlock store) ref obj =
    maybe (return ())      (\store -> storeDefine store ref obj) (fmap staticVars store)
  storeDelete (CurrentCodeBlock store) ref     =
    maybe (return ())      (\store -> storeDelete store ref    ) (fmap staticVars store)
  storeUpdate (CurrentCodeBlock store) ref upd =
    maybe (return Nothing) (\store -> storeUpdate store ref upd) (fmap staticVars store)

instance Store WithRefStore where
  storeLookup (WithRefStore sto) (Reference ref)     = flip (maybe (return Nothing)) sto $ \sto ->
    withObject sto $ \tree -> return (False, tree, T.lookup ref tree)
  storeDefine (WithRefStore sto) (Reference ref) obj = flip (maybe (return ())) sto $ \sto ->
    withObject sto $ \tree -> return (True, T.insert ref obj tree, ())
  storeDelete (WithRefStore sto) (Reference ref)     = flip (maybe (return ())) sto $ \sto ->
    withObject sto $ \tree -> return (True, T.delete ref tree, ())
  storeUpdate (WithRefStore sto) (Reference ref) upd = flip (maybe (return Nothing)) sto $ \sto ->
    withObject sto $ \tree -> upd (T.lookup ref tree) >>= \obj -> return $ case obj of
      Nothing  -> (False, tree, Nothing)
      Just obj -> (True, T.insert ref obj tree, Just obj)

-- Used internally by the instantiation of 'WithRefStore' into 'Store'. The updating function takes
-- a tree and returns a tripple: 1. a boolean value answering the question "was the tree updated and
-- do we need to convert it back to an object?", 2. the updated tree (or maybe the same tree), 3.
-- an arbitrary result produced during the update which is returned when evaluation is successful.
withObject :: ExecRef ref => ref Object -> (T_tree -> Exec (Bool, T_tree, a)) -> Exec a
withObject store upd = execModifyRef store $ \target -> case target of
  OTree     tree -> upd tree >>= \ (_, tree, a) -> return (OTree tree, a)
  OHaskell o ifc -> case objTreeFormat ifc of
    Just (toTree, fromTree) -> do
      (needToWriteBack, tree, result) <- toTree o >>= upd
      if needToWriteBack
        then  fromTree tree >>= \newValue -> return (OHaskell newValue ifc, result)
        else  return (OHaskell o ifc, result)
    Nothing                 -> execThrow $ OList $
      [ ostr $ unwords $
          [ "no method defining how to use objects of type"
          , show (objHaskellType ifc)
          , "in \"with\" statements"
          ]
      , OHaskell o ifc
      ]

---- | Lookup an object in the 'globalData' for this 'ExecUnit'.
--  execHeapLookup :: Reference -> Exec (Maybe Object)
--  execHeapLookup (Reference name) = ask >>= \xunit ->
--    --readResource (globalData xunit) name
--    liftIO (fmap (T.lookup name) (readIORef (globalData xunit)))

--  -- | Lookup an object in the 'globalData' for this 'ExecUnit'.
--  execHeapUpdate :: Reference -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
--  execHeapUpdate (Reference name) runUpdate = ask >>= \xunit -> do
--    --inEvalDoUpdateResource (globalData xunit) name runUpdate
--    upd <- liftIO (fmap (T.lookup name) (readIORef (globalData xunit))) >>= runUpdate
--    case upd of
--      Nothing  -> return upd
--      Just obj -> liftIO (modifyIORef (globalData xunit) (T.insert name obj)) >> return upd

--  execHeapDefine :: Reference -> Object -> Exec (Maybe Object)
--  execHeapDefine name obj = execHeapUpdate name (return . const (Just obj))

--  execHeapDelete :: Reference -> Object -> Exec (Maybe Object)
--  execHeapDelete name obj = execHeapUpdate name (return . const Nothing)

--  -- | Lookup a reference value in the durrent document, if the current document has been set with a
--  -- "with" statement.
--  curDocVarLookup :: Reference -> Exec (Maybe Object)
--  curDocVarLookup (Reference name) = do
--    xunit <- ask
--    case currentWithRef xunit of
--      Nothing                      -> return Nothing
--  --    Just file@(DocumentFile res) -> Exec $ Procedural $ fmap FlowOK $
--  --      readResource res (currentBranch xunit ++ name)
--      _ -> error $ concat $
--             [ "current document is not an idea file, cannot lookup reference "
--             , intercalate "." (map uchars name)
--             ]

--  -- | Update a reference value in the durrent document, if the current document has been set with a
--  -- "with" statement.
--  curDocVarUpdate :: Reference -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
--  curDocVarUpdate (Reference name) runUpdate = do
--    xunit <- ask
--    case currentWithRef xunit of
--      Nothing                  -> return Nothing
--  --    Just file@(DocumentFile res) ->
--  --      inEvalDoUpdateResource res (currentBranch xunit ++ name) runUpdate
--      _ -> error $ concat $
--             [ "current document is not an idea file, cannot update reference "
--             , intercalate "." (map uchars name)
--             ]

--  curDocVarDefine :: Reference -> Object -> Exec (Maybe Object)
--  curDocVarDefine ref obj = curDocVarUpdate ref (return . const (Just obj))

--  curDocVarDelete :: Reference -> Object -> Exec (Maybe Object)
--  curDocVarDelete name obj = curDocVarUpdate name (return . const Nothing)

--  -- | Lookup a value in the 'execStack'.
--  localVarLookup :: Name -> Exec (Maybe Object)
--  localVarLookup sym =
--    --fmap execStack ask >>= lift . dReadMVar xloc >>= return . msum . map (T.lookup [sym]) . mapList
--    ask >>= \xunit -> liftIO (fmap (msum . fmap (T.lookup [sym]) . mapList) (readIORef (execStack xunit)))

--  -- | Apply an altering function to the map at the top of the local variable stack.
--  localVarUpdate :: Name -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
--  localVarUpdate name alt = do
--    xunit <- ask
--    --dModifyMVar xloc (execStack xunit) $ \ax -> case mapList ax of
--    liftIO $ atomicModifyIORef (execStack xunit) $ \ax -> case mapList ax of
--      []   -> stack_underflow
--      a:ax -> do
--        let obj = alt (T.lookup [name] a)
--        --in  return (Stack (T.update [name] (const obj) a : ax), obj)
--        in  (Stack (T.update [name] (const obj) a : ax), obj)

--  -- | Force the local variable to be defined in the top level 'execStack' context, do not over-write
--  -- a variable that has already been defined in lower in the context stack.
--  localVarDefine :: Name -> Object -> Exec (Maybe Object)
--  localVarDefine name obj = localVarUpdate name (const (Just obj))

localVarDefine :: Name -> Object -> Exec ()
localVarDefine nm obj = asks execStack >>= \sto -> storeDefine sto (Reference [nm]) obj

--  localVarDelete :: Name -> Exec (Maybe Object)
--  localVarDelete nm = localVarUpdate nm (const Nothing)

--  staticVarLookup :: Name -> Exec (Maybe Object)
--  staticVarLookup nm = do
--    exe <- fmap (currentCodeBlock >=> return . staticVars) ask
--    case exe of
--      Nothing  -> return Nothing
--      Just exe -> liftIO (readIORef exe) >>= return . M.lookup nm

--  staticVarUpdate :: Name -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
--  staticVarUpdate nm upd = do
--    ref <- fmap (currentCodeBlock >=> return . staticVars) ask
--    case ref of
--      Nothing  -> return Nothing
--      Just ref -> do
--        val <- liftIO (fmap (M.lookup nm) (readIORef ref)) >>= upd
--        liftIO $ modifyIORef ref (M.update (const val) nm)
--        return val

--  staticVarDefine :: Name -> Object -> Exec (Maybe Object)
--  staticVarDefine nm obj = staticVarUpdate nm (return . const (Just obj))

--  staticVarDelete :: Name -> Exec (Maybe Object)
--  staticVarDelete nm = staticVarUpdate nm (return . const Nothing)

--  -- | Lookup an object, first looking in the current document, then in the 'globalData'.
--  globalVarLookup :: Reference -> Exec (Maybe Object)
--  globalVarLookup ref = ask >>= \xunit ->
--    (if isJust (currentWithRef xunit) then curDocVarLookup else execHeapLookup) ref

--  globalVarUpdate :: Reference -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
--  globalVarUpdate ref runUpdate = ask >>= \xunit ->
--    (if isJust (currentWithRef xunit) then curDocVarUpdate else execHeapUpdate) ref runUpdate

--  -- | To define a global variable, first the 'currentWithRef' is checked. If it is set, the variable
--  -- is assigned to the document at the reference location prepending 'currentBranch' reference.
--  -- Otherwise, the variable is assigned to the 'globalData'.
--  globalVarDefine :: Reference -> Object -> Exec (Maybe Object)
--  globalVarDefine name obj = globalVarUpdate name (return . const (Just obj))

--  -- | To delete a global variable, the same process of searching for the address of the object is
--  -- followed for 'globalVarDefine', except of course the variable is deleted.
--  globalVarDelete :: Reference -> Exec (Maybe Object)
--  globalVarDelete name = globalVarUpdate name (return . const Nothing)

--  qTimeVarLookup :: Reference -> Exec (Maybe Object)
--  qTimeVarLookup (Reference name) = do --ask >>= \xunit -> lift (readResource (queryTimeHeap xunit) ref)
--    xunit <- ask
--    liftIO $ fmap (T.lookup name) (readIORef (queryTimeHeap xunit))

--  qTimeVarUpdate :: Reference -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
--  qTimeVarUpdate (Reference name) runUpdate = do --ask >>= \xunit -> inEvalDoUpdateResource (queryTimeHeap xunit) name runUpdate
--    xunit <- ask
--    upd <- liftIO $ fmap (T.lookup name) (readIORef (queryTimeHeap xunit))
--    case upd of
--      Nothing  -> return upd
--      Just obj -> liftIO (modifyIORef (queryTimeHeap xunit) (T.insert name obj) >> return upd)

--  qTimeVarDefine :: Reference -> Object -> Exec (Maybe Object)
--  qTimeVarDefine name obj = qTimeVarUpdate name (return . const (Just obj))

--  qTimeVarDelete :: Reference -> Exec (Maybe Object)
--  qTimeVarDelete name = qTimeVarUpdate name (return . const Nothing)

--  clearAllQTimeVars :: ExecUnit -> Exec ()
--  clearAllQTimeVars xunit = do --modifyUnlocked_ (queryTimeHeap xunit) (return . const T.Void)
--    liftIO $ modifyIORef (queryTimeHeap xunit) (const T.Void)

----------------------------------------------------------------------------------------------------

-- $Built_in_functions
-- Built-in functions, retrieved from an array 'infixOps' or 'prefixOps' by a 'Dao.Object.ArithOp'
-- value, or from 'updateOps' by a 'Dao.Object.UpdateOp' value. Built-in functions check object
-- parameters passed to them with the 'BuiltinOp' monad, which is a fully lazy monad based on
-- 'Dao.Predicate.PValue'.

type BuiltinOp = Exec Object

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

asReference :: Object -> Exec QualRef
asReference o = case o of
  ORef r -> return r
  _      -> mzero

asInteger :: Object -> Exec Integer
asInteger o = case o of
  OWord    o -> return (toInteger o)
  OInt     o -> return (toInteger o)
  OLong    o -> return o
  OFloat   o -> return (round o)
  ORatio   o -> return (round o)
  ORelTime o -> return (round (toRational o))
  _          -> mzero

asRational :: Object -> Exec Rational
asRational o = case o of
  OInt      o     -> return (toRational o)
  OWord     o     -> return (toRational o)
  OLong     o     -> return (toRational o)
  OFloat    o     -> return (toRational o)
  ORelTime  o     -> return (toRational o)
  OComplex (o:+0) -> return (toRational o)
  ORatio    o     -> return o
  _               -> mzero

asComplex :: Object -> Exec T_complex
asComplex o = case o of
  OComplex o -> return o
  o          -> fmap (\o -> o:+0) (fmap fromRational (asRational o))

asStringNoConvert :: Object -> Exec UStr
asStringNoConvert o = case o of
  OString o -> return o
  _         -> mzero

asString :: Object -> Exec UStr
asString o = case o of
  OString o -> return o
  o         -> return (ustr (showObj o))

asListNoConvert :: Object -> Exec [Object]
asListNoConvert o = case o of
  OList o -> return o
  _       -> mzero

asList :: Object -> Exec [Object]
asList o = case o of
  OList   o -> return o
--OArray  o -> return (elems o)
--OSet    o -> return (S.elems o)
--ODict   o -> return (map (\ (i, o) -> OPair (OString i, o)) (M.assocs o))
--OIntMap o -> return (map (\ (i, o) -> OPair (OInt (fromIntegral i), o)) (I.assocs o))
--OTree   o -> return (map (\ (i, o) -> OPair (OList (map OString i), o)) (T.assocs o))
  _         -> mzero

-- | Combines two lists of objects, then removes one "layer of lists", that is, if the combined
-- lists are of the form:
-- @list {a, b, ... , list {c, d, ... , list {e, f, ...}, ...} }@ 
-- the resulting list will be @list {a, b, ... , c, d, ... , list {e, f, ... }, ...}@
objListAppend :: [Object] -> [Object] -> Object
objListAppend ax bx = OList $ flip concatMap (ax++bx) $ \a -> case a of
  OList ax -> ax
  a        -> [a]

asHaskellInt :: Object -> Exec Int
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
--  t | t == fromEnum WordType -> OWord (fromIntegral x)
    t | t == fromEnum IntType  -> OInt  (fromIntegral x)
--  t | t == fromEnum LongType -> OLong (fromIntegral x)
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
--        t | t == fromEnum FloatType    -> OFloat    (fromRational x)
          t | t == fromEnum DiffTimeType -> ORelTime (fromRational x)
--        t | t == fromEnum RatioType    -> ORatio    (fromRational x)
--        t | t == fromEnum ComplexType  -> OComplex  (fromRational x)
          _ -> error "asRational returned a value for an object of an unexpected type"
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
      (OAbsTime a, ORelTime b) -> return (OAbsTime (addUTCTime b a))
--    (OAbsTime a, ORatio    b) -> return (OAbsTime (addUTCTime (fromRational (toRational b)) a))
--    (OAbsTime a, OFloat    b) -> return (OAbsTime (addUTCTime (fromRational (toRational b)) a))
      _                      -> mzero
    listAdd a b = do
      ax <- asListNoConvert a
      bx <- case b of
        OList  bx -> return bx
--      OSet   b  -> return (S.elems b)
--      OArray b  -> return (elems b)
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
  , case (a, b) of
      (OAbsTime a, OAbsTime     b) -> return (ORelTime (diffUTCTime a b))
      (OAbsTime a, ORelTime b) -> return (OAbsTime (addUTCTime (negate b) a))
--    (OAbsTime a, ORatio    b) -> return (OAbsTime (addUTCTime (fromRational (toRational (negate b))) a))
--    (OAbsTime a, OFloat    b) -> return (OAbsTime (addUTCTime (fromRational (toRational (negate b))) a))
      _                  -> mzero
  ]

evalDistNum
  :: (Integer  -> Integer  -> Integer )
  -> (Rational -> Rational -> Rational) 
  -> Object -> Object -> BuiltinOp
evalDistNum intFn rnlFn a b = evalNum intFn rnlFn a b

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
-- -> (T_set -> T_set  -> T_set)
  -> (Integer -> Integer -> Integer)
  -> Object -> Object -> BuiltinOp
evalBitsOrSets combine dict intmap {-set-} num a b = evalInt num a b

eval_ORB :: Object -> Object -> BuiltinOp
eval_ORB  a b = evalBitsOrSets OList M.unionWith        I.unionWith        {-S.union-}        (.|.) a b

eval_ANDB :: Object -> Object -> BuiltinOp
eval_ANDB a b = evalBitsOrSets OList M.intersectionWith I.intersectionWith {-S.intersection-} (.&.) a b

eval_XORB :: Object -> Object -> BuiltinOp
eval_XORB a b = evalBitsOrSets (\a -> head a) mfn ifn {-sfn-} xor a b where
--sfn = fn S.union S.intersection S.difference head
  mfn = fn M.union M.intersection M.difference
  ifn = fn I.union I.intersection I.difference
  fn u n del _ a b = (a `u` b) `del` (a `n` b)

evalShift :: (Int -> Int) -> Object -> Object -> BuiltinOp
evalShift fn a b = asHaskellInt b >>= \b -> case a of
  OInt  a -> return (OInt  (shift a (fn b)))
--OWord a -> return (OWord (shift a (fn b)))
--OLong a -> return (OLong (shift a (fn b)))
  _       -> mzero

--evalSubscript :: Object -> Object -> BuiltinOp
--evalSubscript a b = case a of
--OArray  a -> fmap fromIntegral (asInteger b) >>= \b ->
--  if inRange (bounds a) b then return (a!b) else fail "array index out of bounds"
--  OList   a -> asHaskellInt b >>= \b ->
--    let err = fail "list index out of bounds"
--        ax  = take 1 (drop b a)
--    in  if b<0 then err else if null ax then err else return (head ax)
--OIntMap a -> asHaskellInt b >>= \b -> case I.lookup b a of
--  Nothing -> fail "no item at index requested of intmap"
--  Just  b -> return b
--ODict   a -> msum $
--  [ do  asStringNoConvert b >>= \b -> case M.lookup b a of
--          Nothing -> fail (show b++" is not defined in dict")
--          Just  b -> return b
--  , do  asReference b >>= \b -> case b of
--          PlainRef b -> case M.lookup b a of
--            Nothing -> fail (show b++" is not defined in dict")
--            Just  b -> return b
--  ]
--  OTree   a -> msum $ 
--    [ asStringNoConvert b >>= \b -> case fromUStr b of
--        Nothing -> execThrow $ OList [ostr "string does not form valid identifier", OString b]
--        Just b  -> done (T.lookup [b] a)
--    , asReference b >>= \b -> case b of
--        UnqualRefExpr (RefExpr b _) -> done (T.lookup [b] a)
--        _                           -> mzero
--    ] where
--        done a = case a of
--          Nothing -> fail (prettyShow b++" is not defined in struct")
--          Just  a -> return a
--  _         -> mzero

evalCompare
  :: (Integer -> Integer -> Bool) -> (Rational -> Rational -> Bool) -> Object -> Object -> BuiltinOp
evalCompare compI compR a b = msum $
  [ asInteger  a >>= \a -> asInteger  b >>= \b -> done (compI a b)
  , asRational a >>= \a -> asRational b >>= \b -> done (compR a b)
  ]
  where { done true = if true then return OTrue else return ONull }

eval_EQUL :: Object -> Object -> BuiltinOp
eval_EQUL a b = evalCompare (==) (==) a b

eval_NEQUL :: Object -> Object -> BuiltinOp
eval_NEQUL a b = evalCompare (/=) (/=) a b

eval_GTN :: Object -> Object -> BuiltinOp
eval_GTN a b = evalCompare (>) (>) a b

eval_LTN :: Object -> Object -> BuiltinOp
eval_LTN a b = evalCompare (<) (<) a b

eval_GTEQ :: Object -> Object -> BuiltinOp
eval_GTEQ a b = evalCompare (>=) (>=) a b

eval_LTEQ :: Object -> Object -> BuiltinOp
eval_LTEQ a b = evalCompare (<=) (<=) a b

eval_SHR :: Object -> Object -> BuiltinOp
eval_SHR = evalShift negate

eval_SHL :: Object -> Object -> BuiltinOp
eval_SHL = evalShift id

eval_DOT :: Object -> Object -> BuiltinOp
eval_DOT a b = error "eval_DOT is not defined"

eval_NEG :: Object -> BuiltinOp
eval_NEG o = case o of
--OWord     o -> return $
--  let n = negate (toInteger o)
--  in  if n < toInteger (minBound::T_int)
--         then  OLong n
--         else  OInt (fromIntegral n)
  OInt      o -> return $ OInt      (negate o)
--OLong     o -> return $ OLong     (negate o)
  ORelTime o -> return $ ORelTime (negate o)
--OFloat    o -> return $ OFloat    (negate o)
--ORatio    o -> return $ ORatio    (negate o)
--OComplex  o -> return $ OComplex  (negate o)
  _           -> mzero

eval_INVB :: Object -> BuiltinOp
eval_INVB o = case o of
--OWord o -> return $ OWord (complement o)
  OInt  o -> return $ OInt  (complement o)
--OLong o -> return $ OLong (complement o)
  _       -> mzero

eval_REF :: Object -> BuiltinOp
eval_REF r = error "eval_REF is not defined"

eval_DEREF :: Object -> BuiltinOp
eval_DEREF r = error "eval_DEREF is not defined"

eval_NOT :: Object -> BuiltinOp
eval_NOT o = return (boolToObj (testNull o))

-- | Traverse the entire object, returning a list of all 'Dao.Object.OString' elements.
extractStringElems :: Object -> [UStr]
extractStringElems o = case o of
  OString  o   -> [o]
  OList    o   -> concatMap extractStringElems o
--OSet     o   -> concatMap extractStringElems (S.elems o)
--OArray   o   -> concatMap extractStringElems (elems o)
--ODict    o   -> concatMap extractStringElems (M.elems o)
--OIntMap  o   -> concatMap extractStringElems (I.elems o)
  OTree    o   -> concatMap extractStringElems (T.elems o)
--OPair (a, b) -> concatMap extractStringElems [a, b]
  _            -> []

eval_multiRef :: ([Name] -> Reference) -> Object -> BuiltinOp
eval_multiRef mk o = error "eval_multiRef is not defined"

eval_singleRef :: (Name -> Reference) -> Object -> BuiltinOp
eval_singleRef mk o = error "eval_singleRef is not defined"

prefixOps :: Array PrefixOp (Object -> BuiltinOp)
prefixOps = array (minBound, maxBound) $ defaults ++
  [ o REF       eval_REF
  , o DEREF     eval_DEREF
  , o INVB      eval_INVB
  , o NOT       eval_NOT
  , o NEGTIV    eval_NEG
  , o POSTIV    return
--  , o GLDOT     (error "GLDOT prefixOp is not defined")
--  , o GLOBALPFX (error "GLOBALPFX prefixOp is not defined")
--  , o LOCALPFX  (error "LOCALPFX prefixOp is not defined")
--  , o QTIMEPFX  (error "QTIMEPFX prefixOp is not defined")
--  , o STATICPFX (error "STATICPFX prefixOp is not defined")
  ]
  where
    o = (,)
    defaults = flip map [minBound..maxBound] $ \op ->
      (op, \_ -> error$"no builtin function for prefix "++show op++" operator")

infixOps :: Array InfixOp (Object -> Object -> BuiltinOp)
infixOps = array (minBound, maxBound) $ defaults ++
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
  , o OR    (error (e "logical-OR" ))
  , o AND   (error (e "logical-AND"))
  -- , o OR    (evalBooleans (||)) -- These probably wont be evaluated. Locgical and/or is a
  -- , o AND   (evalBooleans (&&)) -- special case to be evaluated in 'evalObjectExprWithLoc'.
  , o EQUL  eval_EQUL
  , o NEQUL eval_NEQUL
  , o GTN   eval_GTN
  , o LTN   eval_LTN
  , o GTEQ  eval_GTEQ
  , o LTEQ  eval_LTEQ
  , o ARROW (error (e "ARROW"))
--  , o DOT   eval_DOT
  ]
  where
    o = (,)
    defaults = flip map [minBound..maxBound] $ \op ->
      (op, \_ _ -> error$"no builtin function for infix "++show op++" operator")
    e msg = msg ++
      " operator should have been evaluated within the 'execute' function."

updatingOps :: Array UpdateOp (Object -> Object -> BuiltinOp)
updatingOps = let o = (,) in array (minBound, maxBound) $ defaults ++
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
      (op, \_ _ -> error$"no builtin function for update operator "++show op)

----------------------------------------------------------------------------------------------------

requireAllStringArgs :: [Object] -> Exec [UStr]
requireAllStringArgs ox = case mapM check (zip (iterate (+1) 0) ox) of
  OK      obj -> return obj
  Backtrack   -> execThrow $ OList [ostr "all input parameters must be strings"]
  PFail   msg -> execThrow $ OList [OString msg, ostr "is not a string"]
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
--  OSet     ox -> recurse o (S.elems ox)
--  OArray   ox -> recurse o   (elems ox)
--  ODict    ox -> recurse o (M.elems ox)
--  OIntMap  ox -> recurse o (I.elems ox)
--  OPair (a,b) -> recurse o [a,b]
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
            catchReturn (execute (Unqualified ref)) >>= recurse . maybeToList

-- | Returns a list of all string objects that can be found from within the given list of objects.
-- This function might fail if objects exist that cannot resonably contain strings. If you want to
-- pretty-print non-string objects, try using 'getStringsToDepth'.
recurseGetAllStrings :: Object -> Exec [UStr]
recurseGetAllStrings o = catch (loop [] o) where
  loop ix o = case o of
    OString  o   -> return [o]
    OList    o   -> next OInt (zip [0..] o)
--  OSet     o   -> next OInt (zip [0..] (S.elems o))
--  OArray   o   -> next OInt (assocs   o)
--  ODict    o   -> next OString (M.assocs o)
--  OIntMap  o   -> next (OInt . fromIntegral) (I.assocs o)
--  OPair  (a,b) -> next OInt [(0,a), (1,b)]
    o            -> throwError $ OList $
      [ ostr "object at index", OList ix
      , ostr "cannot be evaluated to a string", o
      ]
    where
      next fn = fmap concat . mapM (\ (i, o) -> loop (fn i : ix) o)
  catch ox = case ox of
    FlowErr  err -> execThrow err
    FlowOK    ox -> return ox

builtin_print :: DaoFunc
builtin_print = DaoFunc True $ \ox_ -> do
  let ox = flip map ox_ $ \o -> case o of
        OString o -> o
        o         -> ustr (showObj o)
  liftIO $ mapM_ (putStrLn . uchars) ox
  return $ Just $ OList $ map OString ox

-- Returns the current string query.
builtin_doing :: DaoFunc
builtin_doing = DaoFunc True $ \ox ->
  fmap currentQuery ask >>= \query -> case query of
    Nothing    -> return (Just ONull)
    Just query -> case ox of
      []          -> return $ Just $ OString query
      [OString o] -> return $ Just $ boolToObj (o==query)
    --[OSet    o] -> return $ Just $ boolToObj (S.member (OString query) o)
      _ -> execThrow $ OList $
        [ostr "doing() must take as parameers a single string, a single set, or nothing", OList ox]

-- join string elements of a container, pretty prints non-strings and joins those as well.
builtin_join :: DaoFunc
builtin_join = DaoFunc True $ \ox -> case ox of
  [OString j, a] -> joinWith (uchars j) a
  [a]            -> joinWith "" a
  _ -> objectError (OList ox) "join() function requires one or two parameters"
  where
    joinWith j =
      fmap (Just . OString . ustr . intercalate j) . derefStringsToDepth (\ _ o -> execThrow o) 1 1

builtin_check_ref :: DaoFunc
builtin_check_ref = DaoFunc True $ \args -> do
  fmap (Just . boolToObj . and) $ forM args $ \arg -> case arg of
    ORef o -> fmap (maybe False (const True)) (execute o :: Exec (Maybe Object))
    o      -> return True

builtin_delete :: DaoFunc
builtin_delete = DaoFunc True $ \args -> do
  forM_ args $ \arg -> case arg of
    ORef o -> void $ updateReference o (const (return Nothing))
    _      -> return ()
  return (Just ONull)

-- | The map that contains the built-in functions that are used to initialize every
-- 'Dao.Object.ExecUnit'.
initBuiltinFuncs :: M.Map Name DaoFunc
initBuiltinFuncs = let o a b = (ustr a, b) in M.fromList $
  [ o "print"   builtin_print
  , o "doing"   builtin_doing
  , o "join"    builtin_join
  , o "defined" builtin_check_ref
  , o "delete"  builtin_delete
  ]

----------------------------------------------------------------------------------------------------

-- | All assignment operations are executed with this function. To modify any variable at all, you
-- need a reference value and a function used to update the value. This function will select the
-- correct value to modify based on the reference type and value, and modify it according to this
-- function.
updateReference :: QualRef -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
updateReference qref upd = do
  let fn ref getter = asks getter >>= \store -> storeUpdate store ref upd
  case qref of
    Unqualified ref -> case refNameList ref of
      [_] -> fn ref execStack
      _   -> fn ref globalData
    Qualified q ref -> case q of
      LOCAL  -> fn ref execStack
      QTIME  -> fn ref globalData
      STATIC -> fn ref currentCodeBlock
      GLOBAL -> fn ref globalData
      GLODOT -> fn ref currentWithRef

instance Executable ExecDaoFunc (Maybe Object) where
  execute (MkExecDaoFunc op params fn) = do
    args <- if autoDerefParams fn then mapM execute params else return (fmap paramValue params)
    flow <- procCatch (daoForeignCall fn args)
    case flow of
      FlowOK     obj -> return obj
      FlowReturn obj -> return obj
      FlowErr    err -> throwError err

instance Executable ParamValue Object where { execute (ParamValue obj _) = derefObject obj }

callFunction :: QualRef -> [ParamValue] -> Exec (Maybe Object)
callFunction qref params = do
  obj <- execute qref
  obj <- case obj of
    Just obj -> return obj
    Nothing  -> execThrow $ OList $
      [ ostr "function call on undefined reference", ORef qref
      , ostr "called with parameters", OList (map paramValue params)
      ]
  let err = execThrow $ OList $
        [ ostr "reference does not point to callable object: ", ORef qref
        , ostr "called with parameters: ", OList (map paramValue params)
        , ostr "value of object at reference is: ", obj
        ]
  case obj of
    OHaskell o ifc -> case objCallable ifc of
      Just fn  -> do
        callables <- fn o
        msum $ flip fmap callables $ \code ->
          matchFuncParams (argsPattern code) params >>= flip execFuncPushStack (execute code)
      Nothing -> err
    obj -> err

----------------------------------------------------------------------------------------------------

-- $ErrorReporting
-- The 'Procedural' is a continuation monad that can evaluate to an error message without evaluating
-- to "bottom". The error message is any value of type 'Dao.Object.Object'. These functions provide
-- a simplified method for constructing error 'Dao.Object.Object's.

-- | Convert a 'Dao.Token.Location' to an 'Dao.Object.Object' value.
errAt :: Location -> [Object]
errAt loc = case loc of
  LocationUnknown -> []
  loc -> [ OInt (fromIntegral (startingLine loc)), OInt (fromIntegral (startingColumn loc))
         , OInt (fromIntegral (endingLine   loc)), OInt (fromIntegral (endingColumn   loc))
         ]

-- | Evaluate to 'procErr' if the given 'PValue' is 'Backtrack' or 'PFail'. You must pass a
-- 'Prelude.String' as the message to be used when the given 'PValue' is 'Backtrack'. You can also
-- pass a list of 'Dao.Object.Object's that you are checking, these objects will be included in the
-- 'procErr' value.
--     This function should be used for cases when you have converted 'Dao.Object.Object' to a
-- Haskell value, because 'Backtrack' values indicate type exceptions, and 'PFail' values indicate a
-- value error (e.g. out of bounds, or some kind of assert exception), and the messages passed to
-- 'procErr' will indicate this.
checkPValue :: Location -> String -> [Object] -> Exec a -> Exec a
checkPValue loc altmsg tried pval = procCatch pval >>= \pval -> case pval of
  FlowOK     a        -> return a
  FlowReturn Nothing  -> fail "evaulated to void expression"
  FlowReturn (Just o) -> procReturn (Just o)
  FlowErr    err      -> throwError err

-- | 'evalObjectExprExpr' can return 'Data.Maybe.Nothing', and usually this happens when something has
-- failed (e.g. reference lookups), but it is not always an error (e.g. a void list of argument to
-- functions). If you want 'Data.Maybe.Nothing' to cause an error, evaluate your
-- @'Dao.Object.Exec' ('Data.Maybe.Maybe' 'Dao.Object.Object')@ as a parameter to this function.
checkVoid :: Location -> String -> Maybe a -> Exec a
checkVoid loc msg fn = case fn of
  Nothing -> execThrow $ OList $ errAt loc ++ [ostr msg, ostr "evaluates to a void"]
  Just  a -> return a

----------------------------------------------------------------------------------------------------

evalConditional :: ParenExpr -> Exec Bool
evalConditional obj =
  (execute obj :: Exec (Maybe Object)) >>=
    checkVoid (getLocation obj) "conditional expression to if statement" >>=
      execHandleIO [fmap (const False) execIOHandler] . return . testNull

instance Executable ParenExpr (Maybe Object) where { execute (ParenExpr a _) = execute a }

instance Executable IfExpr Bool where
  execute (IfExpr ifn thn loc) = execNested T.Void $
    evalConditional ifn >>= \test -> when test (void (execute thn)) >> return test

instance Executable ElseExpr Bool where { execute (ElseExpr ifn _) = execute ifn }

instance Executable IfElseExpr () where
  execute (IfElseExpr ifn elsx final loc) = do
    let tryEach elsx = case elsx of
          []       -> return False
          els:elsx -> execute els >>= \ok -> if ok then return ok else tryEach elsx
    (execute ifn >>= \ok ->
      if ok then return Nothing
            else tryEach elsx >>= \ok ->
                 if ok then return Nothing
                       else return final) >>= maybe (return ()) execute

instance Executable WhileExpr () where
  execute (WhileExpr ifn) = let loop = execute ifn >>= flip when loop in loop

-- | Convert a single 'ScriptExpr' into a function of value @'Exec' 'Dao.Object.Object'@.
instance Executable ScriptExpr () where
  execute script = updateExecError (\err->err{execErrScript=Just script}) $ case script of
    IfThenElse  ifn     -> execute ifn
    WhileLoop   ifn     -> execute ifn
    EvalObject  o   loc -> void (execute o :: Exec (Maybe Object))
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    TryCatch try  name  catch loc -> do
      ce <- procCatch (execNested T.Void $ execute try)
      case ce of
        FlowOK    () -> return ()
        FlowReturn o -> proc ce
        FlowErr  err -> do
          let errObj = specificErrorData err -- TODO: pass the whole error wrapped in an 'OHaskell' constructor.
          case name of
            Nothing -> maybe (return ()) (execNested T.Void . execute) catch
            Just nm -> case catch of
              Nothing    -> localVarDefine nm errObj
              Just catch -> execNested T.Void (localVarDefine nm errObj >> execute catch)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ForLoop varName inObj thn loc -> do
      let loop newList ox = case ox of
            []   -> return newList
            o:ox -> do
              (shouldContinue, o) <- execute (ForLoopBlock varName o thn)
              let next = newList ++ maybe [] (:[]) o
              if shouldContinue then loop next ox else return (next++ox)
      objRef <- execute inObj >>=
        checkVoid (getLocation inObj) "value over which to iterate \"for\" statement"
      case objRef of
        ORef qref -> void $ updateReference qref $ \obj -> case obj of
          Nothing  -> return Nothing
          Just obj -> execute qref >>=
            checkVoid (getLocation inObj) "reference over which to iterate evaluates to void" >>= \objRef ->
              fmap Just (iterateObject objRef >>= loop [] >>= foldObject objRef)
        obj       -> void (iterateObject obj >>= loop [])
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ContinueExpr a    _       loc -> execThrow $ ostr $
      '"':(if a then "continue" else "break")++"\" expression is not within a \"for\" or \"while\" loop"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ReturnExpr returnStmt obj loc -> do
      o <- (execute obj :: Exec (Maybe Object)) >>= maybe (return Nothing) (fmap Just . derefObject)
      if returnStmt then proc (FlowReturn o) else execThrow (fromMaybe ONull o)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    WithDoc   expr    thn     loc -> void $ execNested T.Void $ do
      obj <- execute expr >>= checkVoid (getLocation expr) "expression in the focus of \"with\" statement"
      ref <- mplus (asReference obj) $ execThrow $ OList $
        [ostr "expression in \"with\" statement does not evaluate to a reference, evaluates to a", obj]
      updateReference ref $ \obj -> case obj of
        Nothing  -> execThrow $ OList [ostr "undefined reference", ORef ref]
        Just obj -> do
          let ok = do
                ioref <- liftIO (newIORef obj)
                execNested T.Void $
                  local (\x->x{currentWithRef=WithRefStore (Just ioref)}) (execute expr)
                liftIO (fmap Just (readIORef ioref))
          case obj of
            OTree    _ -> ok
            OHaskell o ifc -> case objTreeFormat ifc of
              Just  _ -> ok
              Nothing -> execThrow $ OList $
                [ ostr "object of type", ostr (toUStr (show (objHaskellType ifc)))
                , ostr "is not defined to be used in \"with\" statements"
                ]
            _ -> execThrow $ OList $
              [ ostr "object value at reference", ORef ref
              , ostr "has no method of being used in a \"with\" statement, value is", obj
              ]
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

-- | This data type instantates the 'execute' function for use in for-loop expressions.
data ForLoopBlock = ForLoopBlock Name Object CodeBlock
instance Executable ForLoopBlock (Bool, Maybe Object) where
  execute (ForLoopBlock name obj block) = 
    execNested (T.insert [name] obj T.Void) $ loop (codeBlock block) where
      done cont = do
        (LocalStore ref) <- asks execStack
        newValue <- liftIO (fmap (T.lookup [name] . head . mapList) (readIORef ref))
        return (cont, newValue)
      loop ex = case ex of
        []   -> done True
        e:ex -> case e of
          ContinueExpr a cond loc -> let stmt = if a then "continue" else "break" in case cond of
            VoidExpr -> done a
            cond     -> evalConditional (ParenExpr cond LocationUnknown) >>= done . (if a then id else not)
          e -> execute e >> loop ex

instance Executable QualRef (Maybe Object) where
  -- | 'Dao.Object.execute'-ing a 'Dao.Object.QualRef' will dereference it, essentially reading the
  -- value associated with that reference from the 'Dao.Object.ExecUnit'.
  execute ref = case ref of
    Unqualified ref -> case refNameList ref of
      []  -> emptyRefErr
      [r] -> asks execStack  >>= flip storeLookup ref
      _   -> asks globalData >>= flip storeLookup ref
    Qualified q ref -> case q of
      LOCAL  -> case refNameList ref of
        [ ] -> emptyRefErr
        [r] -> asks execStack >>= flip storeLookup ref
        ref -> badRef "local"
      QTIME  -> asks queryTimeHeap >>= flip storeLookup ref
      GLODOT -> ask >>= \xunit -> case currentWithRef xunit of
        WithRefStore Nothing -> execThrow $ OList $
          [ ostr "cannot use reference prefixed with a dot unless in a \"with\" statement"
          , ORef (Qualified q ref)
          ]
        store -> storeLookup store ref
      STATIC -> case refNameList ref of
        [ ] -> emptyRefErr
        [r] -> asks currentCodeBlock >>= flip storeLookup ref
        ref -> badRef "static"
      GLOBAL -> asks globalData >>= flip storeLookup ref
    where
      emptyRefErr = execThrow $ OList [ostr "dereferenced empty reference", ORef ref]
      badRef  typ = execThrow $ OList [ostr "bad reference", ostr typ, ORef ref]

-- | Like evaluating 'execute' on a value of 'Dao.Object.QualRef', except the you are evaluating an
-- 'Dao.Object.Object' type. If the value of the 'Dao.Object.Object' is not constructed with
-- 'Dao.Object.ORef', the object value is returned unmodified.
derefObject :: Object -> Exec Object
derefObject obj = case obj of
  ORef ref -> execute ref >>= maybe (execThrow $ OList [ostr "undefined reference", obj]) return
  obj      -> return obj

instance Executable ObjListExpr [ParamValue] where
  execute (ObjListExpr exprs _) = fmap concat $ forM exprs $ \expr -> case expr of
    VoidExpr -> return []
    expr     -> execute expr >>= \val -> case val of
      Nothing  -> execThrow $ OList [ostr "expression used in list evaluated to void"]
      Just val -> return [ParamValue{paramValue=val, paramOrigExpr=expr}]

-- | Evaluate an 'Exec', but if it throws an exception, set record an 'Dao.Object.ObjectExpr' where
-- the exception occurred in the exception information.
updateExecError :: (ExecError -> ExecError) -> Exec a -> Exec a
updateExecError upd fn = catchError fn (\err -> throwError (upd err))

instance Executable ObjectExpr (Maybe Object) where
  execute obj = (updateExecError (\err->err{execErrExpr=Just obj}) :: Exec (Maybe Object) -> Exec (Maybe Object)) $ case obj of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    VoidExpr -> return Nothing
      -- ^ 'VoidExpr's only occur in return statements. Returning 'ONull' where nothing exists is
      -- probably the most intuitive thing to do on an empty return statement.
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Literal o _ -> return (Just o)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ObjParenExpr o -> execute o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    AssignExpr nm0 op  expr0  loc -> do
      nm <- execute nm0 >>= checkVoid loc "left-hand side of assignment"
      let lhs = "left-hand side of "++show op
      nm   <- mplus (execute nm0 >>= checkVoid (getLocation nm0) (lhs++" evaluates to void") >>= asReference) $
        execThrow $ OList [ostr (lhs++" is not a reference value")]
      expr <- execute expr0 >>= checkVoid loc "right-hand side of assignment" >>= derefObject
      updateReference nm $ \maybeObj -> case maybeObj of
        Nothing      -> case op of
          UCONST -> return (Just expr)
          _      -> execThrow $ OList [ostr "undefined refence", ORef nm]
        Just prevVal -> fmap Just $
          checkPValue (getLocation expr0) "assignment expression" [prevVal, expr] $ (updatingOps!op) prevVal expr
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    FuncCall op args loc -> do -- a built-in function call
      obj  <- execute op >>= checkVoid loc "function selector evaluates to void"
      op   <- mplus (asReference obj) $ execThrow $
        OList [ostr "function selector does not evaluate to reference", obj]
      execute args >>= callFunction op
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ArraySubExpr o i loc -> do
      o <- execute o >>= checkVoid loc "operand of subscript expression" >>= derefObject
      fmap Just $ execute i >>= mapM (derefObject . paramValue) >>= foldM indexObject o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Equation  left' op right' loc -> do
      let err1 msg = msg++"-hand operand of "++show op++ "operator "
          evalLeft   = execute left'  >>= checkVoid loc (err1 "left" )
          evalRight  = execute right' >>= checkVoid loc (err1 "right")
          derefLeft  = evalLeft  >>= derefObject
          derefRight = evalRight >>= derefObject
          logical isAndOp = fmap Just $ do
            left <- derefLeft
            if objToBool left
              then  if isAndOp then derefRight else return OTrue
              else  if isAndOp then return ONull else derefRight
      case op of
        AND -> logical True
        OR  -> logical False
        op  -> do
          (left, right) <- case op of
          --DOT   -> liftM2 (,) evalLeft  evalRight
            ARROW -> liftM2 (,) derefLeft evalRight
            _     -> liftM2 (,) derefLeft derefRight
          fmap Just ((infixOps!op) left right)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    PrefixExpr op expr loc -> do
      expr <- execute expr >>= checkVoid loc ("operand to prefix operator "++show op )
      fmap Just ((prefixOps!op) expr)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    LambdaExpr params scrpt lc -> do
      exec       <- setupCodeBlock scrpt
      let newFunc = CallableCode params nullValue exec
      return $ Just $ error "TODO: LambdaExpr needs to evaluate to an Object containing a CallableCode"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    FuncExpr name params scrpt lc -> do
      exec   <- setupCodeBlock scrpt
      let newFunc = CallableCode{argsPattern=params, codeSubroutine=exec, returnType=nullValue}
      return $ Just $ error "TODO: FuncExpr needs to evaluate to an Object containing a CallableCode"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    RuleExpr (RuleStrings params _) scrpt lc -> do
      exec <- setupCodeBlock scrpt
      let mkCallable = GlobAction (fmap (parsePattern . uchars) params) exec
      let fol tre pat = T.unionWith (++) tre (toTree pat [exec])
      return $ Just $ error "TODO: RuleExpr needs to evaluate to an Object containing a GlobAction"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    MetaEvalExpr expr loc -> error "TODO: MetaEvalExpr needs to evaluate to an Object containing an AST_Script"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance Executable LValueExpr (Maybe Object) where { execute (LValueExpr expr) = execute expr }

--evalLambdaExpr :: LambdaExprType -> [ObjectExpr] -> CodeBlock -> Exec Object
--evalLambdaExpr typ argv code = do
--  exe <- setupCodeBlock (code)
--  let convArgv fn = mapM fn argv
--  case typ of
--    FuncExprType -> do
--      argv <- convArgv paramsToObjPatExpr
--      return $ OScript $
--        CallableCode{argsPattern=argv, getSubroutine=exe}
--    RuleExprType -> do
--      argv <- convArgv paramsToGlobExpr
--      return $ OScript $
--        GlobAction{globPattern=argv, getSubroutine=exe}

-- | Convert an 'Dao.Object.ObjectExpr' to an 'Dao.Glob.Glob'.
paramsToGlobExpr :: ObjectExpr -> Exec Glob
paramsToGlobExpr o = case o of
  Literal (OString str) _ -> return (read (uchars str))
  _ -> execThrow $ OList $ [ostr "does not evaluate to a \"glob\" pattern"]

----------------------------------------------------------------------------------------------------

-- | Blocks until every thread in the given 'Dao.Object.Task' completes evaluation.
taskWaitThreadLoop :: Task -> Exec ()
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
taskRegisterThreads :: Task -> Exec [DThread] -> Exec ()
taskRegisterThreads task makeThreads =
  dModifyMVar_ xloc (taskRunningThreads task) $ \threads -> do
    newThreads <- makeThreads
    return (S.union (S.fromList newThreads) threads)

-- | Signal to the 'Dao.Object.Task' that the task has completed. This must be the last thing a
-- thread does if it is registered into a 'Dao.Object.Task' using 'taskRegisterThreads'.
completedThreadInTask :: Task -> Exec ()
completedThreadInTask task = dMyThreadId >>= dPutMVar xloc (taskWaitMVar task)

-- | Evaluate an 'Dao.Object.Action' in the current thread.
execAction :: ExecUnit -> Action -> Exec (Maybe Object)
execAction xunit_ action = runCodeBlock T.Void (actionCodeBlock action) where
  xunit =
    xunit_
    { currentQuery      = actionQuery      action
    , currentPattern    = actionPattern    action
    , currentMatch      = actionMatch      action
    , currentCodeBlock  = CurrentCodeBlock (Just (actionCodeBlock action))
    }

-- | Create a new thread and evaluate an 'Dao.Object.Action' in that thread. This thread is defined
-- such that when it completes, regardless of whether or not an exception occurred, it signals
-- completion to the 'Dao.Object.waitForActions' 'Dao.Debug.DMVar' of the 'Dao.Object.ExecUnit'
-- associated with this 'Dao.Object.Action'.
forkExecAction :: ExecUnit -> Action -> Exec DThread
forkExecAction xunit act = dFork forkIO xloc "forkExecAction" $ do
  execCatchIO (void $ execAction xunit act) [execErrorHandler, execIOHandler]
  completedThreadInTask (taskForActions xunit)

-- | For every 'Dao.Object.Action' in the 'Dao.Object.ActionGroup', evaluate that
-- 'Dao.Object.Action' in a new thread in the 'Task' associated with the 'Dao.Object.ExecUnit' of
-- the 'Dao.Object.ActionGroup'.
forkActionGroup :: ActionGroup -> Exec ()
forkActionGroup actgrp = dStack xloc ("forkActionGroup ("++show (length (getActionList actgrp))++" items)") $ do
  let xunit = actionExecUnit actgrp
      task  = taskForActions xunit
  taskRegisterThreads task (forM (getActionList actgrp) (forkExecAction (actionExecUnit actgrp)))
  taskWaitThreadLoop task >>= liftIO . evaluate

-- | For every 'Dao.Object.Action' in the 'Dao.Object.ActionGroup', evaluate that
-- 'Dao.Object.Action' in a the current thread but in using the 'Dao.Object.ExecUnit' of the
-- given 'Dao.Object.ActionGroup'.
execActionGroup :: ActionGroup -> Exec ()
execActionGroup actgrp = mapM_ (execAction (actionExecUnit actgrp)) (getActionList actgrp)

-- | This is the most important algorithm of the Dao system, in that it matches strings to all
-- rule-patterns in the program that is associated with the 'Dao.Object.ExecUnit', and it dispatches
-- execution of the rule-actions associated with matched patterns. This is the function that runs in
-- the thread which manages all the other threads that are launched in response to a matching input
-- string. You could just run this loop in the current thread, if you only have one 'ExecUnit'.
execInputStringsLoop :: ExecUnit -> Exec ()
execInputStringsLoop xunit = dStack xloc "execInputStringsLoop" $ do
  runtime <- ask
  dCatch xloc loop (\ (SomeException _) -> return ())
  completedThreadInTask (taskForActions xunit)
  where
    loop = do
      dMessage xloc "Get the next input string. Also nub the list of queued input strings."
      --instr <- dModifyMVar xloc (recursiveInput xunit) $ \ax -> return $ case nub ax of
      instr <- liftIO $ atomicModifyIORef (recursiveInput xunit) $ \ax -> case nub ax of
        []   -> ([], Nothing)
        a:ax -> (ax, Just a)
      case instr of
        Nothing    -> return ()
        Just instr -> dStack xloc ("execInputString "++show instr) $ do
          dStack xloc "Exec 'execPatternMatchCodeBlock' for every matching item." $
            waitAll (makeActionsForQuery instr xunit) >>= liftIO . evaluate
          dMessage xloc "Exec the next string."
          loop

waitAll :: Exec ActionGroup -> Exec ()
waitAll getActionGroup = getActionGroup >>= forkActionGroup

-- | Given an input string, and a program, return all patterns and associated match results and
-- actions that matched the input string, but do not execute the actions. This is done by tokenizing
-- the input string and matching the tokens to the program using 'Dao.Glob.matchTree'.
-- NOTE: Rules that have multiple patterns may execute more than once if the input matches more than
-- one of the patterns associated with the rule. *This is not a bug.* Each pattern may produce a
-- different set of match results, it is up to the programmer of the rule to handle situations where
-- the action may execute many times for a single input.
makeActionsForQuery :: UStr -> ExecUnit -> Exec ActionGroup
makeActionsForQuery instr xunit = do
  tokenizer <- asks programTokenizer
  tokenizer instr >>= match
  where
    eq = programComparator xunit
    match tox = do
      --tree <- dReadMVar xloc (ruleSet xunit)
      tree <- liftIO $ readIORef (ruleSet xunit)
      return $
        ActionGroup
        { actionExecUnit = xunit
        , getActionList = flip concatMap (matchTree eq tree tox) $ \ (patn, mtch, execs) ->
            flip map execs $ \exec -> seq exec $! seq instr $! seq patn $! seq mtch $!
              Action
              { actionQuery      = Just instr
              , actionPattern    = Just patn
              , actionMatch      = Just mtch
              , actionCodeBlock = exec
              }
        }

-- | Create a list of 'Dao.Object.Action's for every BEGIN or END statement in the Dao program. Pass
-- 'Dao.Object.preExec' as the first parameter to get the BEGIN scrpits, pass 'Dao.Object.postExec'
-- to get the END scripts.
getBeginEndScripts :: (ExecUnit -> [Subroutine]) -> ExecUnit -> ActionGroup
getBeginEndScripts select xunit =
  ActionGroup
  { actionExecUnit = xunit
  , getActionList  = flip map (select xunit) $ \exe ->
      Action
      { actionQuery      = Nothing
      , actionPattern    = Nothing
      , actionMatch      = Nothing
      , actionCodeBlock = exe
      }
  }

-- | For each given execution unit, place the input string into the 'Dao.Object.recursiveInput'
-- queue of that 'Dao.Object.ExecUnit' and then start a thread running 'execInputStrinsLoop' for
-- that 'Dao.Object.ExecUnit'. This function evaluates syncrhonously, that is, you must wait for all
-- threads created by this string query to complete before this function evaluation returns.
-- The threads are created and registered into the 'Dao.Object.runningExecThreads' field of the
-- 'Dao.Object.Runtime' using 'taskRegisterThreads'. Then, 'taskWaitThreadLoop' is called to wait
-- for the string execution to complete.
runStringQuery :: UStr -> [ExecUnit] -> Exec ()
runStringQuery inputString xunits = dStack xloc "runStringsQuery" $ do
  task <- asks (taskForExecUnits . parentRuntime)
  taskRegisterThreads task $ do
    runtime <- ask
    forM xunits $ \xunit -> do
      --dModifyMVar_ xloc (recursiveInput xunit) (return . (++[inputString]))
      liftIO $ modifyIORef (recursiveInput xunit) (++[inputString])
      --dFork forkIO xloc "runStringQuery" $ do
      liftIO $ fmap DThread $ forkIO $ void $ flip ioExec xunit $ do
        --flip (dCatch xloc) (\ (SomeException _) -> return ()) $ do
        execHandleIO [execErrorHandler, execIOHandler] $ do
          --dStack xloc "Exec \"BEGIN\" scripts." $
            waitAll (return (getBeginEndScripts preExec xunit)) >>= liftIO . evaluate
          --dStack xloc "Call execInputStringsLoop" $
            execInputStringsLoop xunit >>= liftIO . evaluate -- Exec RULES and PATTERNS
          --dStack xloc "Exec \"END\" scripts." $
            waitAll (return (getBeginEndScripts postExec xunit)) >>= liftIO . evaluate
        completedThreadInTask task
  taskWaitThreadLoop task

-- | Clears any string queries waiting in the 'Dao.Object.recurisveInput' of an
-- 'Dao.Object.ExecUnit'.
clearStringQueries :: ExecUnit -> Exec ()
clearStringQueries xunit = --dModifyMVar_ xloc (recursiveInput xunit) (\_ -> return [])
  liftIO $ modifyIORef (recursiveInput xunit) (const [])

-- | This is the main input loop. Pass an input function callback to be called on every loop.
daoInputLoop :: (Exec (Maybe UStr)) -> Exec ()
daoInputLoop getString = asks parentRuntime >>= loop >> daoShutdown where
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

-- | Evaluates the @EXIT@ scripts for every presently loaded dao program, and then clears the
-- 'Dao.Object.pathIndex', effectively removing every loaded dao program and idea file from memory.
daoShutdown :: Exec ()
daoShutdown = do
  runtime <- asks parentRuntime
  let idx = pathIndex runtime
  xunits <- fmap (concatMap isProgramFile . M.elems) (dReadMVar xloc idx)
  dModifyMVar_ xloc idx $ (\_ -> return (M.empty))

-- | When executing strings against Dao programs (e.g. using 'Dao.Tasks.execInputString'), you often
-- want to execute the string against only a subset of the number of total programs. Pass the
-- logical names of every module you want to execute strings against, and this function will return
-- them. If you pass an empty list, all 'PublicType' modules (in the 'programs' table of the
-- 'Runtime') will be returned. Pass @'Data.Maybe.Just' 'Dao.Evaluator.ExecUnit'@ to allow
-- 'PrivateType' functions to also be selected, however only modules imported by the program
-- associated with that 'ExecUnit' are allowed to be selected.
selectModules :: Maybe ExecUnit -> [Name] -> Exec [ExecUnit]
selectModules xunit names = dStack xloc "selectModules" $ do
  runtime <- asks parentRuntime
  ax <- case names of
    []    -> dReadMVar xloc (pathIndex runtime)
    names -> do
      pathTab <- dReadMVar xloc (pathIndex runtime)
      let set msg           = M.fromList . map (\mod -> (toUStr mod, error msg))
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
evalScriptString :: ExecUnit -> String -> Exec ()
evalScriptString xunit instr =
  void $ execNested T.Void $ mapM_ execute $
    case parse (daoGrammar{mainParser = many script <|> return []}) mempty instr of
      Backtrack -> error "cannot parse expression"
      PFail tok -> error ("error: "++show tok)
      OK   expr -> concatMap toInterm expr

-- | This is the simplest form of string execution, everything happens in the current thread, no
-- "BEGIN" or "END" scripts are executed. Simply specify a list of programs (as file paths) and a
-- list of strings to be executed against each program.
execStringsAgainst :: [Name] -> [UStr] -> Exec ()
execStringsAgainst selectPrograms execStrings = do
  xunit <- ask
  otherXUnits <- selectModules (Just xunit) selectPrograms
  forM_ otherXUnits $ \xunit ->
    forM_ execStrings $ \execString ->
      makeActionsForQuery execString xunit >>= execActionGroup

----------------------------------------------------------------------------------------------------

-- | Initialized the current 'ExecUnit' by evaluating all of the 'Dao.Object.TopLevel' data in a
-- 'Dao.Object.AST.AST_SourceCode'.
execTopLevel :: Program -> Exec ExecUnit
execTopLevel (Program ast) = do
  xunit  <- ask
  funcs  <- liftIO (newIORef mempty)
  macros <- liftIO (newIORef [])
  pre    <- liftIO (newIORef [])
  post   <- liftIO (newIORef [])
  onExit <- liftIO (newIORef [])
  forM_ (dropWhile isAttribute ast) $ \dirctv -> case dirctv of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Attribute a b c -> execThrow $ OList $ concat $
      [ maybe [] ((:[]) . ostr . (++(show c)) . uchars) (programModuleName xunit)
      , [ostr $ uchars a ++ " expression must occur only at the top of a dao script"]
      , [ostr $ prettyShow (Attribute a b c)]
      ]
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    TopScript scrpt lc -> do
      -- push a namespace onto the stack
      --lift $ dModifyMVar_ xloc (execStack xunit) (return . stackPush T.Void)
      let (LocalStore stor) = execStack xunit
      liftIO $ modifyIORef stor (stackPush T.Void)
      -- get the functions declared this far
      funcMap <- liftIO (readIORef funcs)
      ce <- procCatch $ local (\_ -> xunit{topLevelFuncs=funcMap}) (execute scrpt)
      case ce of
        FlowOK     _ -> return ()
        FlowReturn _ -> return ()
        FlowErr  err -> proc (FlowErr err)
      -- pop the namespace, keep any local variable declarations
      --tree <- lift $ dModifyMVar xloc (execStack xunit) (return . stackPop)
      let (LocalStore stor) = execStack xunit
      tree <- liftIO $ atomicModifyIORef stor stackPop
      -- merge the local variables into the global varaibles resource.
      --lift (modifyUnlocked_ (globalData xunit) (return . T.union tree))
      let (GlobalStore stor) = globalData xunit
      liftIO $ modifyMVar_ stor (return . T.union tree)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    EventExpr typ scrpt lc -> do
      exec   <- setupCodeBlock scrpt
      liftIO $ flip modifyIORef (++[exec]) $ case typ of
        BeginExprType -> pre
        EndExprType   -> post
        ExitExprType  -> onExit
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  funcs  <- liftIO (readIORef funcs)
  macros <- liftIO (readIORef macros)
  pre    <- liftIO (readIORef pre)
  post   <- liftIO (readIORef post)
  onExit <- liftIO (readIORef onExit)
  return $ xunit{ preExec=pre, postExec=post, quittingTime=onExit, topLevelFuncs=funcs }

----------------------------------------------------------------------------------------------------

-- | Simply converts an 'Dao.Object.AST.AST_SourceCode' directly to a list of
-- 'Dao.Object.TopLevelExpr's.
evalTopLevelAST :: AST_SourceCode -> Exec Program
evalTopLevelAST ast = case toInterm ast of
  [o] -> return o
  []  -> fail "converting AST_SourceCode to Program by 'toInterm' returned null value"
  _   -> fail "convertnig AST_SourceCode to Program by 'toInterm' returned ambiguous value"

-- Called by 'loadModHeader' and 'loadModule', throws a Dao exception if the source file could not
-- be parsed.
loadModParseFailed :: Maybe UPath -> DaoParseErr -> Exec ig
loadModParseFailed path err = execThrow $ OList $ concat $
  [ maybe [] (\path -> [ostr $ uchars path ++ maybe "" show (parseErrLoc err)]) path
  , maybe [] (return . ostr) (parseErrMsg err)
  , maybe [] (\tok -> [ostr "on token", ostr (show tok)]) (parseErrTok err)
  ]

-- | Load only the "require" and "import" statements from a Dao script file at the given filesystem
-- path. Called by 'importDepGraph'.
loadModHeader :: UPath -> Exec [(Name, Object, Location)]
loadModHeader path = do
  text <- liftIO (readFile (uchars path))
  case parse daoGrammar mempty text of
    OK    ast -> do
      let attribs = takeWhile isAST_Attribute (directives ast) >>= attributeToList
      forM attribs $ \ (attrib, astobj, loc) -> case toInterm (unComment astobj) of
        []  -> execThrow $ OList $
          [ostr ("bad "++uchars attrib++" statement"), ostr (prettyShow astobj)]
        o:_ -> do
          o <- execute o
          case o of
            Nothing -> execThrow $ OList $
              [ ostr $ "parameter to "++uchars attrib++" evaluated to void"
              , ostr (prettyShow astobj)
              ]
            Just  o -> return (attrib, o, loc)
    Backtrack -> execThrow $ OList $
      [ostr path, ostr "does not appear to be a valid Dao source file"]
    PFail err -> loadModParseFailed (Just path) err

-- | Creates a child 'ExecUnit' for the current 'ExecUnit' and populates it with data by parsing and
-- evaluating the contents of a Dao script file at the given filesystem path.
loadModule :: UPath -> Exec ExecUnit
loadModule path = do
  text <- liftIO (readFile (uchars path))
  case parse daoGrammar mempty text of
    OK    ast -> deepseq ast $! do
      mod <- childExecUnit (Just path)
      local (const mod) (evalTopLevelAST ast >>= execTopLevel) -- updates and returns 'mod' 
    Backtrack -> execThrow $ OList [ostr path, ostr "does not appear to be a valid Dao source file"]
    PFail err -> loadModParseFailed (Just path) err

-- | Takes a non-dereferenced 'Dao.Object.Object' expression which was returned by 'execute'
-- and converts it to a file path. This is how "import" statements in Dao scripts are evaluated.
-- This function is called by 'importDepGraph', and 'importFullDepGraph'.
objectToImport :: UPath -> Object -> Location -> Exec [UPath]
objectToImport file obj lc = case obj of
  OString str -> return [str]
  obj         -> execThrow $ OList $ 
    [ ostr (uchars file ++ show lc)
    , ostr "contains import expression evaluating to an object that is not a file path", obj
    ]

objectToRequirement :: UPath -> Object -> Location -> Exec UStr
objectToRequirement file obj lc = case obj of
  OString str -> return str
  obj         -> execThrow $ OList $ 
    [ ostr (uchars file ++ show lc)
    , ostr "contains import expression evaluating to an object that is not a file path", obj
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
  let isRequire attrib = attrib == ustr "require" || attrib == ustr "requires"
  fhdrs <- forM files (\file -> loadModHeader file >>= \hdrs -> return (file, hdrs))
  fmap mconcat $ forM fhdrs $ \ (file, attribs) ->
    if M.member file graph
      then  return mempty
      else  do
        imports <- fmap mconcat $ forM attribs $ \ (attrib, obj, lc) ->
          if isImport attrib
            then  objectToImport file obj lc
            else
              if isRequire attrib
                then  do
                  req <- objectToRequirement file obj lc
                  runtime <- asks parentRuntime
                  if S.member req (provides runtime)
                    then  return mempty
                    else  execThrow $ OList $
                            [ostr file, ostr "requires feature not provided", ostr req]
                else  return mempty
        return (M.singleton file imports)

-- | Recursively 'importDepGraph' until the full dependency graph is generated.
importFullDepGraph :: [UPath] -> Exec DepGraph
importFullDepGraph = loop mempty where
  loop graph files = importDepGraph graph files >>= \newGraph ->
    if M.null newGraph then return graph else loop (mappend graph newGraph) (M.keys newGraph)

