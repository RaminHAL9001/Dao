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
import           Dao.Predicate
import           Dao.Combination
import           Dao.Parser

import           Dao.Object.Monad
import           Dao.Object.Data
import           Dao.Object.Show
import           Dao.Object.Binary
import           Dao.Object.Parsers

import           Control.Exception
import           Control.Monad.Trans
import           Control.Monad.Reader

import           Data.Maybe (fromMaybe)
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
import Debug.Trace

----------------------------------------------------------------------------------------------------

initExecUnit :: Runtime -> Run ExecUnit
initExecUnit runtime = do
  unctErrs <- dNewMVar xloc "ExecUnit.uncaughtErrors" []
  recurInp <- dNewMVar xloc "ExecUnit.recursiveInput" []
  xheap    <- dNewMVar xloc "ExecUnit.execHeap" T.Void
  xstack   <- dNewMVar xloc "ExecUnit.execStack" []
  toplev   <- dNewMVar xloc "ExecUnit.toplevelFuncs" M.empty
  files    <- dNewMVar xloc "ExecUnit.execOpenFiles" M.empty
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
    , execHeap           = xheap
    , toplevelFuncs      = toplev
    , execStack          = xstack
    , execOpenFiles      = files
    , recursiveInput     = recurInp
    , uncaughtErrors     = unctErrs
    }

-- $BasicCombinators
-- These are the most basic combinators for converting working with the 'ExecUnit' of an
-- 'ExecScript' monad.

-- | The 'uncom' function works in the 'ExecScript' monad to uncomment the Dao scripting code as it
-- is executed, and calls a hook in the 'ExecUnit' to print comments as code runs to help visualize
-- code execution.
uncom :: Com a -> ExecScript a
uncom com = fmap commentPrint ask >>= \prin -> prin (getComment com) >> return (unComment com)

-- | Execute two transformation, "push" and "pop", on the contents of one of the
-- 'Control.Concurrent.DMVar.DMVar's in the 'ExecUnit'.
mvarContext
  :: (ExecUnit -> DMVar stack) -- ^ select the 'Control.Concurrent.DMVar.DMVar' from the 'ExecUnit'
  -> (stack -> IO stack) -- ^ the "push" computation to evaluate before the inner computation.
  -> (stack -> IO stack) -- ^ the "pop" computation to evaluate after the inner computation.
  -> ExecScript a -- ^ the "inner" computation, evaluation is @push >> inner >> pop@
  -> ExecScript a
mvarContext getMVar push pop fn = do
  mvar <- fmap getMVar ask
  execRun (dModifyMVar_ xloc mvar (lift . push))
  ce <- withContErrSt fn return
  execRun (dModifyMVar_ xloc mvar (lift . pop))
  returnContErr ce

modifyXUnit :: (ExecUnit -> DMVar a) -> (a -> IO (a, b)) -> ExecScript b
modifyXUnit getMVar updMVar = fmap getMVar ask >>= \mvar ->
  execRun (dModifyMVar xloc mvar (lift . updMVar))

-- | Modify an 'Control.Concurrent.DMVar.DMVar' value in the 'ExecUnit' atomically.
modifyXUnit_ :: (ExecUnit -> DMVar a) -> (a -> IO a) -> ExecScript ()
modifyXUnit_ getMVar updMVar = fmap getMVar ask >>= \mvar ->
  execRun (dModifyMVar_ xloc mvar (lift . updMVar))

----------------------------------------------------------------------------------------------------
-- $StackOperations
-- Operating on the local stack.

stack_underflow = error "INTERNAL ERROR: stack underflow"

-- | Push a new empty local-variable context onto the stack. Does NOT 'catchCEReturn', so it can be
-- used to push a new context for every level of nested if/else/for/try/catch statement. Use
-- 'pushExecStack' to perform a function call within a function call.
nestedExecStack :: T_dict -> ExecScript a -> ExecScript a
nestedExecStack dict fn = mvarContext execStack (return . (dict:)) pop fn where
  pop ax = case ax of
    []   -> stack_underflow
    _:ax -> return ax

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. Use 'catchCEReturn' to prevent return calls from halting execution beyond this
-- function. This is what you should use to perform a Dao function call within a Dao function call.
pushExecStack :: T_dict -> ExecScript Object -> ExecScript Object
pushExecStack dict fn = do
  stackMVar <- execRun (dNewMVar xloc "pushExecStack/ExecUnit.execStack" [dict])
  local (\xunit -> xunit{execStack = stackMVar}) (catchCEReturn fn)

-- | Lookup a value in the 'execStack'.
execLocalLookup :: Name -> ExecScript (Maybe Object)
execLocalLookup sym =
  fmap execStack ask >>= execRun . dReadMVar xloc >>= return . msum . map (M.lookup sym)

-- | Try to assign to an already-delcared local variable. If the assignment is successful, the
-- 'execStack' is updated.
assignLocalVar :: Name -> Object -> ExecScript ()
assignLocalVar name obj = modifyXUnit_ execStack (loop []) where
  loop rx ax = case ax of
    []   -> case rx of
      []   -> stack_underflow
      r:rx -> return (M.insert name obj r : rx)
    a:ax -> case M.updateLookupWithKey (\ _ _ -> Just obj) name a of
      (Nothing, a) -> loop (rx++[a]) ax
      (Just _ , a) -> return (rx++a:ax)

-- | Force the local variable to be defined in the top level 'execStack' context, do not over-write
-- a variable that has already been defined in lower in the context stack.
putLocalVar :: Name -> Object -> ExecScript ()
putLocalVar name obj = modifyXUnit_ execStack $ \ax -> case ax of
  []   -> stack_underflow
  a:ax -> return (M.insert name obj a : ax)

-- | To define a global variable, first the 'currentDocument' is checked. If it is set, the variable
-- is assigned to the document at the reference location prepending 'currentBranch' reference.
-- Otherwise, the variable is assigned to the 'execHeap'.
modifyGlobalVar :: [Name] -> ([Name] -> T_tree -> T_tree) -> ExecScript ()
modifyGlobalVar name alt = do
  xunit <- ask
  let prefixName = currentBranch xunit ++ name
  case currentDocument xunit of
    Nothing                     -> modifyXUnit_ execHeap (return . alt prefixName)
    Just file | isIdeaFile file -> execRun $ dModifyMVar_ xloc (fileData file) $ \doc -> return $
      doc{docRootObject = alt prefixName (docRootObject doc)}
    Just file                   -> ceError $ OList $ map OString $
      [ustr "current document is not a database", filePath file]

-- | To delete a global variable, the same process of searching for the address of the object is
-- followed for 'defineGlobalVar', except of course the variable is deleted.
deleteGlobalVar :: [Name] -> ExecScript ()
deleteGlobalVar name = modifyGlobalVar name (\name -> T.delete name)

-- | To define a global variable, first the 'currentDocument' is checked. If it is set, the variable
-- is assigned to the document at the reference location prepending 'currentBranch' reference.
-- Otherwise, the variable is assigned to the 'execHeap'.
defineGlobalVar :: [Name] -> Object -> ExecScript ()
defineGlobalVar name obj = modifyGlobalVar name (\name -> T.insert name obj)

--  defineGlobalVar :: [Name] -> Object -> ExecScript ()
--  defineGlobalVar name obj = do
--    xunit <- ask
--    let prefixName = currentBranch xunit ++ name
--    case currentDocument xunit of
--      Nothing  -> modifyXUnit_ execHeap (return . T.insert prefixName obj)
--      Just doc -> execRun $ dModifyMVar_ xloc doc $ \doc -> return $
--        doc{docRootObject = T.insert prefixName obj (docRootObject doc)}

-- | Lookup a reference value in the static object table of the currently loaded program. Static
-- objects are objects defined in the top level of the program's source code.
staticDataLookup :: [Name] -> ExecScript (Maybe Object)
staticDataLookup name = do
  xunit <- ask
  case currentProgram xunit of
    Nothing   -> return Nothing
    Just prog -> execRun $ dReadMVar xloc (staticData prog) >>=
      return . (T.lookup (currentBranch xunit ++ name))

-- | Lookup an object in the 'execHeap' for this 'ExecUnit'.
lookupExecHeap :: [Name] -> ExecScript (Maybe Object)
lookupExecHeap name = ask >>= \xunit -> execRun $
  dReadMVar xloc (execHeap xunit) >>= return . (T.lookup (currentBranch xunit ++ name))

-- | Lookup a reference value in the durrent document, if the current document has been set with a
-- "with" statement.
currentDocumentLookup :: [Name] -> ExecScript (Maybe Object)
currentDocumentLookup name = do
  xunit <- ask
  case currentDocument xunit of
    Nothing   -> return Nothing
    Just file -> execRun $ dReadMVar xloc (fileData file) >>=
      return . (T.lookup (currentBranch xunit ++ name)) . docRootObject

-- | Lookup an object, first looking in the current document, then in the 'execHeap'.
execGlobalLookup :: [Name] -> ExecScript (Maybe Object)
execGlobalLookup ref =
  sequence [currentDocumentLookup ref, lookupExecHeap ref, staticDataLookup ref] >>= return . msum

-- | Retrieve a 'CheckFunc' function from one of many possible places in the 'Dao.Types.ExecUnit'.
-- Every function call that occurs during execution of the Dao script will use this Haskell function
-- to seek the correct Dao function to use. Pass an error message to be reported if the lookup
-- fails. The order of lookup is: this module's 'Dao.Types.TopLevelFunc's, the
-- 'Dao.Types.TopLevelFunc's of each imported module (from first to last listed import), and finally
-- the built-in functions provided by the 'Dao.Types.Runtime'
lookupFunction :: String -> Name -> ExecScript CheckFunc
lookupFunction msg op = do
  xunit <- ask
  let toplevs xunit = do
        top <- execScriptRun (fmap (M.lookup op) (dReadMVar xloc (toplevelFuncs xunit)))
        case top of
          Nothing  -> return Nothing
          Just top -> fmap Just (toplevelToCheckFunc top)
      lkup xunitMVar = execScriptRun (dReadMVar xloc xunitMVar) >>= toplevs
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

derefError :: [Name] -> ExecScript a
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
bindArgsExpr = bindArgs_internal (\n o -> liftM2 (,) (uncom n) (evalObject o))

----------------------------------------------------------------------------------------------------

-- | Execute a 'Dao.Types.Script' with paramters passed as a list of 
-- @'Dao.Types.Com' 'Dao.Object.ObjectExpr'@. This essentially treats the application of
-- paramaters to a script as a static abstract syntax tree, and converts this tree to an
-- @'ExecScript' 'Dao.Types.Object'@ function.
execScriptCall :: [Com ObjectExpr] -> Script -> ExecScript Object
execScriptCall args scrp = do
  argv <- uncom (scriptArgv scrp)
  bindArgsExpr argv args (catchCEReturn (execScriptBlock (scriptCode scrp) >> ceReturn ONull))

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
-- underflow exception. It is better to use the 'execGuardBlock' function when evaluating a lone
-- block of code with no context.
execScriptBlock :: Com [Com ScriptExpr] -> ExecScript ()
execScriptBlock block = uncom block >>= mapM_ execScriptExpr

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code founf in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [Com ScriptExpr] -> ExecScript ()
execGuardBlock block = void (pushExecStack M.empty (execScriptBlock (Com block) >> return ONull))

-- | Convert a single 'ScriptExpr' into a function of value @'ExecScript' 'Dao.Types.Object'@.
execScriptExpr :: Com ScriptExpr -> ExecScript ()
execScriptExpr script = uncom script >>= \script -> case script of
  NO_OP                        -> return ()
  EvalObject   o               -> unless (isNO_OP (unComment o)) (void (evalObject o))
  IfThenElse   ifn  thn  els   -> nestedExecStack M.empty $ do
    ifn <- fmap objToBool (evalObject ifn)
    execScriptBlock (if ifn then thn else els)
  TryCatch     try  name catch -> do
    ce <- withContErrSt (nestedExecStack M.empty (execScriptBlock try)) return
    case ce of
      CEError o -> do
        name <- uncom name
        nestedExecStack (M.singleton name o) (execScriptBlock catch)
      ce        -> returnContErr ce
  ForLoop    varName inObj thn -> nestedExecStack M.empty $ do
    varName <- uncom varName
    inObj   <- evalObject inObj 
    let block thn = if null thn then return True else scrpExpr (head thn) >> block (tail thn)
        ctrlfn ifn com thn = do
          ifn <- evalObject ifn
          uncom com
          case ifn of
            ONull -> return (not thn)
            _     -> return thn
        scrpExpr expr = case unComment expr of
          ContinueExpr a ifn com -> uncom a >>= ctrlfn ifn com
          _                      -> execScriptExpr expr >> return True
        loop thn name ix = case ix of
          []   -> return ()
          i:ix -> putLocalVar name i >> uncom thn >>= block >>= flip when (loop thn name ix)
        inObjType = OType (objType inObj)
    checkToExecScript (ustr "for") inObjType (objToList inObj >>= checkOK) >>= loop thn varName
  ContinueExpr a    _    _     -> uncom a >>= \a -> simpleError $
    '"':(if a then "continue" else "break")++"\" expression is not within a \"for\" loop"
  ReturnExpr   a    obj  com   -> uncom a >>= \a -> evalObject obj >>= \obj ->
    uncom com >> (if a then ceReturn else ceError) obj
  WithDoc      lval thn        -> nestedExecStack (M.empty) $ do
    lval <- evalObject lval
    let setBranch ref xunit = return (xunit{currentBranch = ref})
        setFile path xunit = do
          file <- execScriptRun (fmap (M.lookup path) (dReadMVar xloc (execOpenFiles xunit)))
          case file of
            Nothing  -> ceError $ OList $ map OString $
              [ustr "with file path", path, ustr "file has not been loaded"]
            Just file -> return (xunit{currentDocument = Just file})
        run upd = ask >>= upd >>= \r -> local (const r) (execScriptBlock thn)
    case lval of
      ORef prefix                       -> run (setBranch prefix)
      OString path                      -> run (setFile path)
      OPair (OString path, ORef prefix) -> run (setFile path >=> setBranch prefix)
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
  IntRef       _     -> True
  LocalRef     _     -> True
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

-- | Evaluate an 'ObjectExpr' to an 'Dao.Types.Object' value, and does not de-reference objects of
-- type 'Dao.Types.ORef'
evalObject :: Com ObjectExpr -> ExecScript Object
evalObject obj = uncom obj >>= \obj -> case obj of
  Literal       o               -> uncom o
  IntRef        i               -> uncom i >>= evalIntRef . fromIntegral
  LocalRef      n               -> do
    n <- uncom n
    o <- execLocalLookup n
    case o of
      Nothing -> objectError (OString n) ("local variable "++show n++" is undefined in current context")
      Just o  -> return o
  GlobalRef     nm              -> do
    nm <- uncom nm
    o  <- execGlobalLookup nm
    case o of
      Nothing -> derefError nm
      Just o  -> return o
  AssignExpr    nm  val         -> do
    nm  <- uncom nm
    val <- evalObject val
    case nm of -- First check if it is a local reference. If so, don't evaluate it.
      LocalRef  nm -> uncom nm >>= \nm -> assignLocalVar nm val
      GlobalRef nm -> uncom nm >>= \nm -> defineGlobalVar nm val
      nm -> do
        nm <- evalObject (Com nm)
        case nm of
          ORef nm -> defineGlobalVar nm val
          o -> objectError o $
            "left-hand side of the assignment expression is not a local or global reference"
    return val
  FuncCall      op   args       -> do -- a built-in function call
    op   <- uncom op
    func <- execLocalLookup op
    args <- uncom args
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
  LambdaCall    com  ref  args  -> do
    uncom com
    fn   <- evalObject ref
    args <- uncom args
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
    op    <- uncom op
    right <- evalObject right
    bi    <- lookupFunction "operator" op
    checkToExecScript op (OPair (right, left)) (checkFunc bi [left, right])
  DictExpr      cons args       -> do
    cons <- uncom cons
    args <- uncom args
    let loop fn = forM args $ \arg -> do
            arg  <- uncom arg
            case arg of
              AssignExpr a b -> evalObject a >>= \a -> evalObject b >>= \b -> fn a b
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
  ArrayExpr     com  rang ox    -> do
    rang <- uncom com >> uncom rang
    case rang of
      (_ : _ : _ : _) ->
        simpleError "internal error: array range expression has more than 2 arguments"
      [lo, hi] -> do
        lo <- evalObject lo
        hi <- evalObject hi
        case (lo, hi) of
          (OInt lo, OInt hi) -> do
            (lo, hi) <- return (if lo<hi then (lo,hi) else (hi,lo))
            ox <- uncom ox >>= mapM evalObject
            return (OArray (listArray (lo, hi) ox))
          _ -> objectError (OPair (OType (objType lo), OType (objType hi))) $
                   "range specified to an "++show ArrayType
                 ++" constructor must evaluate to two "++show IntType++"s"
      _ -> simpleError "internal error: array range expression has fewer than 2 arguments"
  LambdaExpr    com  argv code  -> uncom com >>
    return (OScript (Script{scriptArgv = argv, scriptCode = code}))

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

evalArraySubscript :: Com ObjectExpr -> Com ObjectExpr -> ExecScript Object
evalArraySubscript o i = uncom o >>= \o -> case o of
  IntRef o -> do
    o <- uncom o
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
  _ -> do
    o <- evalObject (Com o)
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
        ORef    x -> return (fromMaybe ONull (T.lookup x o))
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
  :: (Name -> UStr -> CachedProgram -> ExecScript (Maybe CachedProgram))
      -- ^ check attributes written into the script.
  -> SourceCode
      -- ^ the script file to use
  -> ExecScript CachedProgram
programFromSource checkAttribute script = do
  program <- execScriptRun (initProgram (unComment (sourceModuleName script)))
  foldM foldDirectives program (map unComment (unComment (directives script)))
  where
    err lst = ceError $ OList $ map OString $ (sourceFullPath script : lst)
    upd :: (CachedProgram -> DMVar a) -> CachedProgram -> (a -> a) -> ExecScript CachedProgram
    upd select program fn = execScriptRun $
      dModifyMVar xloc (select program) (\a -> return (fn a, program))
    mkCachedFunc scrp = execScriptRun $ do
      let scrp' = unComment scrp
      dNewMVar xloc "CXRef(guardScript)" $
        HasBoth{sourceScript = scrp', cachedScript = execGuardBlock scrp'}
    attrib req nm getRuntime putProg = do
      runtime <- fmap parentRuntime ask
      let item = M.lookup nm (getRuntime runtime)
      case item of
        Just item -> return (putProg item)
        Nothing   -> err [req, ustr "attribute", nm, ustr "is not available"]
    foldDirectives p directive = case directive of
      Attribute  req nm -> ask >>= \xunit -> do
        let setName = unComment nm
            runtime = parentRuntime xunit
            builtins = M.lookup setName $ functionSets runtime
        case unComment req of
          req | req==ustr "import"           -> verifyImport setName $
            p{programImports = programImports p ++ [setName]}
          req | req==ustr "require"          -> case builtins of
            Just  _ -> verifyRequirement setName $
              p{requiredBuiltins = requiredBuiltins p++[setName]}
            Nothing -> err $
              [ustr "requires", setName, ustr "not provided by this version of the Dao system"]
          req | req==ustr "string.tokenizer" ->
            attrib req setName availableTokenizers (\item -> p{programTokenizer = item})
          req | req==ustr "string.compare"   ->
            attrib req setName availableComparators (\item -> p{programComparator = item})
          req -> do
            p <- checkAttribute req setName p
            case p of
              Just p  -> return p
              Nothing -> err [ustr "script contains unknown attribute declaration", req]
      ToplevelDefine name obj -> do
        obj <- evalObject obj
        upd staticData p (\objectTree -> T.insert (unComment name) obj objectTree)
      RuleExpr    rule' -> do
        let rule    = unComment rule'
            rulePat = map unComment (unComment (rulePattern rule))
        cxref <- execScriptRun $
          dNewMVar xloc "CXRef(ruleAction)" $ OnlyAST{sourceScript = ruleAction rule}
        let fol tre pat = T.merge T.union (++) tre (toTree pat [cxref])
        upd ruleSet p (\patternTree -> foldl fol patternTree rulePat)
      SetupExpr    scrp -> return $ p{constructScript = constructScript p ++ [unComment scrp]}
      TakedownExpr scrp -> return $ p{destructScript  = destructScript  p ++ [unComment scrp]}
      BeginExpr    scrp -> do
        scrp <- mkCachedFunc scrp
        return $ p{preExecScript = preExecScript p++[scrp]}
      EndExpr      scrp -> do
        scrp <- mkCachedFunc scrp
        return $ p{postExecScript = postExecScript p++[scrp]}
      ToplevelFunc _ nm argv code -> do
        xunit <- ask
        let func objx = execScriptCall (map (Com . Literal . Com) objx) $
              Script{scriptArgv = argv, scriptCode = code}
        execScriptRun $ do
          let name = unComment nm
          func <- dNewMVar xloc ("Program.topLevelFunc("++uchars name++")") func :: Run TopLevelFunc
          dModifyMVar_ xloc (toplevelFuncs xunit) (return . M.insert name func)
        return p

