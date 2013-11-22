-- "src/Dao/Simple.hs"  a simplified version of the Dao runtime.
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | A simplified version of the Dao runtime.
module Dao.Simple where

import           Dao.Predicate
import qualified Dao.Tree                  as T

import           Data.Char
import           Data.Bits
import           Data.List (intercalate)
import           Data.Monoid
import           Data.Dynamic
import           Data.Array.IArray
import qualified Data.Map                  as M
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy      as Z

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Error hiding (Error)
import           Control.Monad.Trans
import           Control.Monad.IO.Class

class Typeable a => Translatable a where
  toXData   :: a -> XData
  fromXData :: XData -> Maybe a

readLabel :: String -> [(String, String)]
readLabel str = case dropWhile isSpace str of
  c:_ | isAlpha c || c=='_' -> return (span isAlphaNum str)
  [] -> fail "expecting Label"

readAddress :: String -> [(String, String)]
readAddress = readLabel >=> uncurry loop where 
  loop nm str = case dropWhile isSpace str of
    '.':str -> do
      (lbl, str) <- readLabel str
      mplus (loop (nm++'.':lbl) str) (fail "expecting Address")
    str     -> return (nm, str)

newtype Label = Label { label :: Text } deriving (Eq, Ord)
instance Show Label where { show (Label u) = textChars u }
instance Read Label where
  readsPrec _ = readLabel >=> \ (lbl, rem) -> return (Label (text lbl), rem)

newtype Address = Address  { name :: U.ByteString } deriving (Eq, Ord)
instance Show Address where { show (Address u) = U.toString u }
instance Read Address where
  readsPrec _ = readAddress >=> \ (nm, rem) -> return (Address (U.fromString nm), rem)

addrToLabels :: Address -> [Label]
addrToLabels (Address nm) = loop [] (U.toString nm) where
  loop lx str = case break (=='.') str of
    ("" , ""     ) -> lx
    (lbl, '.':str) -> loop (lx++[Label $ Text $ U.fromString lbl]) str
    _              -> fail "could not split address to label"

labelsToAddr :: [Label] -> Address
labelsToAddr = Address . U.fromString . intercalate "." . map (\ (Label o) -> textChars o)

newtype Text = Text { byteString :: U.ByteString } deriving (Eq, Ord)
instance Show Text where { show (Text u) = show (U.toString u) }
instance Read Text where
  readsPrec p = readsPrec p >=> \ (str, rem) -> return (Text (U.fromString str), rem)

text :: String -> Text
text = Text . U.fromString

textChars :: Text -> String
textChars (Text u) = U.toString u

----------------------------------------------------------------------------------------------------

class RWX io x | io -> x, x -> io where { io_x :: io -> x; x_io :: x -> io }

newtype Hidden a = Hidden { unhide :: Maybe a }
instance Eq  (Hidden a) where { _==_ = True }
instance Ord (Hidden a) where { compare _ _ = EQ }
instance Show (Hidden a) where { show _ = "" }
instance Read (Hidden a) where { readsPrec _ s = [(Hidden Nothing, s)] }

data RWData
  = NULL | TRUE | INT Int | FLOAT Double | STR Text | PTR Address
  | LIST [RWData]
  | DATA Address [RWDef]
  | FUNC [Label] [RWCommand]
  deriving (Eq, Ord, Show, Read)
data XData
  = XNULL | XTRUE | XINT Int | XFLOAT Double | XSTR Text | XPTR Address
  | XLIST (Maybe (Array Int XData))
  | XDATA Address (M.Map Label XData)
  | XFUNC [Label] XBlock
  deriving (Eq, Ord)
instance RWX RWData XData where
  io_x io = case io of
    NULL     -> XNULL
    TRUE     -> XTRUE
    INT   io -> XINT       io
    FLOAT io -> XFLOAT     io
    STR   io -> XSTR       io
    PTR   io -> XPTR       io
    LIST  io -> XLIST $ mkXArray (map io_x io)
    DATA a b -> XDATA a (M.fromList $ map ((fmap io_x) . defToPair) b)
    FUNC a b -> XFUNC a (io_x b)
  x_io x  = case x  of
    XNULL     -> NULL
    XTRUE     -> TRUE
    XINT    x -> INT        x
    XFLOAT  x -> FLOAT      x
    XSTR    x -> STR        x
    XPTR    x -> PTR        x
    XLIST   x -> LIST $ maybe [] (map x_io . elems) x
    XDATA a b -> DATA a (map (uncurry DEFINE . fmap x_io) (M.assocs b))
    XFUNC a b -> FUNC a (x_io b)

mkXArray :: [a] -> Maybe (Array Int a)
mkXArray o = if null o then Nothing else Just $ listArray (0, length o - 1) o

data RWModule
  = MODULE
    { imports :: [Address]
    , private :: [RWDef]
    , public  :: [RWDef]
    , rules   :: [RWRule]
    }
  deriving (Eq, Ord, Show, Read)
data XModule
  = XMODULE
    { ximports :: [Address]
    , xprivate :: XDefines
    , xpublic  :: XDefines
    , xrules   :: [XRule]
    }
  deriving (Eq, Ord)
instance RWX RWModule XModule where
  io_x (MODULE imp pri exp act) = XMODULE imp (io_x pri) (io_x exp) (map io_x act) 
  x_io (XMODULE imp pri exp act) = MODULE imp (x_io pri) (x_io exp) (map x_io act) 

data RWRule = RULE [Text] [RWCommand] deriving (Eq, Ord, Show, Read)
data XRule = XRULE [Text] XBlock deriving (Eq, Ord)
instance RWX RWRule XRule where
  io_x (RULE a b) = XRULE a (io_x b :: XBlock)
  x_io (XRULE a b) = RULE a (x_io b :: [RWCommand])

data RWDef = DEFINE Label RWData deriving (Eq, Ord, Show, Read)
defToPair :: RWDef -> (Label, RWData)
defToPair (DEFINE a b) = (a, b)

newtype XDefines = XDefines { defsToMap :: M.Map Label XData } deriving (Eq, Ord)
instance RWX [RWDef] XDefines where
  io_x = XDefines . M.fromList . map (\ (DEFINE a b) -> (a, io_x b) )
  x_io (XDefines x) = map (\ (a, b) -> DEFINE a (x_io b)) (M.assocs x)

data RWLookup
  = RESULT        -- ^ return the result register
  | CONST  RWData -- ^ return a constant value
  | VAR    Label  -- ^ return a value in a register
  | DEREF  Label  -- ^ lookup a value in the current module
  | LOOKUP Address Label -- ^ lookup a value in another module
  deriving (Eq, Ord, Show, Read)
data XLookup
  = XRESULT | XCONST XData | XVAR Label | XDEREF Label | XLOOKUP Address Label
  deriving (Eq, Ord)
instance RWX RWLookup XLookup where
  io_x io = case io of
    RESULT     -> XRESULT
    CONST   io -> XCONST (io_x io)
    VAR     io -> XVAR         io 
    DEREF   io -> XDEREF       io
    LOOKUP a b -> XLOOKUP a b
  x_io x  = case x  of
    XRESULT     -> RESULT
    XCONST   x  -> CONST (x_io x)
    XVAR     x  -> VAR         x 
    XDEREF   x  -> DEREF       x
    XLOOKUP a b -> LOOKUP a b

data RWCommand
  = LOAD    Label  -- ^ load a register to the result register
  | STORE   Label  -- ^ store the result register to the given register
  | SETJUMP Label
  | JUMP    Label
  | PUSH    RWLookup
  | PEEK
  | POP
  | EVAL    RWEval -- ^ evaluate a lisp-like expression
  | DO      RWCondition  -- ^ conditional evaluation
  | RETURN  RWLookup     -- ^ end evaluation
  | THROW   RWLookup
  | FOREACH Label Label RWLookup [RWCommand]
  | BREAK
  deriving (Eq, Ord, Show, Read)
data XCommand
  = XLOAD    Label -- ^ copy a register to the last result
  | XSTORE   Label -- ^ copy the last result to a register
  | XSETJUMP Label -- ^ set a jump point
  | XJUMP    Label -- ^ goto a jump point
  | XPUSH    XLookup -- ^ push a value onto the stack
  | XPEEK    -- ^ copy the top item off of the stack
  | XPOP     -- ^ remove the top item off of the stack
  | XEVAL    XEval -- ^ evaluate a lisp-like expression
  | XDO      XCondition  -- ^ branch conditional
  | XRETURN  XLookup     -- ^ end evaluation
  | XTHROW   XLookup
  deriving (Eq, Ord)
instance RWX RWCommand XCommand where
  io_x io = case io of
    LOAD    a   -> XLOAD          a
    STORE   a   -> XSTORE         a
    SETJUMP a   -> XSETJUMP       a
    JUMP    a   -> XJUMP          a
    PUSH    a   -> XPUSH    (io_x a)
    PEEK        -> XPEEK
    POP         -> XPOP
    EVAL    a   -> XEVAL    (io_x a)
    DO      a   -> XDO      (io_x a)
    RETURN  a   -> XRETURN  (io_x a)
    THROW   a   -> XTHROW   (io_x a)
  x_io x  = case x of
    XLOAD    a   -> LOAD          a
    XSTORE   a   -> STORE         a
    XSETJUMP a   -> SETJUMP       a
    XJUMP    a   -> JUMP          a
    XPUSH    a   -> PUSH    (x_io a)
    XPEEK        -> PEEK
    XPOP         -> POP
    XEVAL    a   -> EVAL    (x_io a)
    XDO      a   -> DO      (x_io a)
    XRETURN  a   -> RETURN  (x_io a)
    XTHROW   a   -> THROW   (x_io a)

newtype XBlock = XBlock { toCommands :: Maybe (Array Int XCommand) } deriving (Eq, Ord)
instance RWX [RWCommand] XBlock where
  io_x io = XBlock (mkXArray $ map io_x io)
  x_io (XBlock x) = maybe [] (map x_io . elems) x

data RWCondition
  = IF      RWLookup RWCommand
  | IFNOT   RWLookup RWCommand
    -- ^ iterate over a 'LIST' or 'DATA' value. If it is a data value to be iterated, the first to
    -- registers provide where to store the data header and the key of each iteration. The result
    -- register always stored the value to be scrutinized.
  deriving (Eq, Ord, Show, Read)
data XCondition
  = XIF      XLookup XCommand
  | XIFNOT   XLookup XCommand
  deriving (Eq, Ord)
instance RWX RWCondition XCondition where
  io_x io = case io of
    IF      a b     -> XIF          (io_x a) (io_x b)
    IFNOT   a b     -> XIFNOT       (io_x a) (io_x b)
  x_io io = case io of
    XIF      a b     -> IF          (x_io a) (x_io b)
    XIFNOT   a b     -> IFNOT       (x_io a) (x_io b)

data RWEval
  = TAKE   RWLookup
  | NOT    RWEval
  | ABS    RWEval
  | SIZE   RWEval
  | ADD    RWEval RWEval
  | SUB    RWEval RWEval
  | MULT   RWEval RWEval
  | DIV    RWEval RWEval
  | MOD    RWEval RWEval
  | INDEX  RWEval RWEval
  | GRTR   RWEval RWEval
  | GREQ   RWEval RWEval
  | LESS   RWEval RWEval
  | LSEQ   RWEval RWEval
  | EQUL   RWEval RWEval
  | NEQL   RWEval RWEval
  | APPEND RWEval RWEval
  | AND    RWEval RWEval
  | OR     RWEval RWEval
  | XOR    RWEval RWEval
  | SHIFTR RWEval RWEval
  | SHIFTL RWEval RWEval
  | SYS    Address  [RWEval] -- ^ system call
  | CALL   Address  RWLookup [RWEval]
  | LOCAL  RWLookup [RWEval] -- ^ call a local function, push the result onto the stack
  | GOTO   RWLookup [RWEval] -- ^ goto a local function, never return
    -- ^ Select the data type from register #1, call a function in it's corresponding module by the
    -- name in register #2 using the parameters 
  deriving (Eq, Ord, Show, Read)
data XEval
  = XTAKE   XLookup
  | XNOT    XEval
  | XSIZE   XEval       -- ^ also functions as the absolute value operator
  | XADD    XEval XEval
  | XSUB    XEval XEval
  | XMULT   XEval XEval
  | XDIV    XEval XEval
  | XMOD    XEval XEval
  | XINDEX  XEval XEval
  | XGRTR   XEval XEval
  | XGREQ   XEval XEval
  | XLESS   XEval XEval
  | XLSEQ   XEval XEval
  | XEQUL   XEval XEval
  | XNEQL   XEval XEval
  | XAPPEND XEval XEval
  | X_AND   XEval XEval
  | X_OR    XEval XEval -- ^ not to be confused with ORX
  | X_XOR   XEval XEval
  | XSHIFTR XEval XEval
  | XSHIFTL XEval XEval
  | XSYS    Address [XEval] -- ^ system call
  | XCALL   Address XLookup [XEval] -- ^ call code in a module
  | XLOCAL  XLookup [XEval] -- ^ call a local function, push the result onto the stack
  | XGOTO   XLookup [XEval] -- ^ goto a local function, never return
  deriving (Eq, Ord)
instance RWX RWEval XEval where
  io_x x  = case x  of
    TAKE   a   -> XTAKE   (io_x a)
    NOT    a   -> XNOT    (io_x a)  
    SIZE   a   -> XSIZE   (io_x a)  
    ADD    a b -> XADD    (io_x a) (io_x b)
    SUB    a b -> XSUB    (io_x a) (io_x b)
    MULT   a b -> XMULT   (io_x a) (io_x b)
    DIV    a b -> XDIV    (io_x a) (io_x b)
    MOD    a b -> XMOD    (io_x a) (io_x b)
    INDEX  a b -> XINDEX  (io_x a) (io_x b)
    GRTR   a b -> XGRTR   (io_x a) (io_x b)
    GREQ   a b -> XGREQ   (io_x a) (io_x b)
    LESS   a b -> XLESS   (io_x a) (io_x b)
    LSEQ   a b -> XLSEQ   (io_x a) (io_x b)
    EQUL   a b -> XEQUL   (io_x a) (io_x b)
    NEQL   a b -> XNEQL   (io_x a) (io_x b)
    APPEND a b -> XAPPEND (io_x a) (io_x b)
    AND    a b -> X_AND   (io_x a) (io_x b)
    OR     a b -> X_OR    (io_x a) (io_x b)
    XOR    a b -> X_XOR   (io_x a) (io_x b)
    SHIFTR a b -> XSHIFTR (io_x a) (io_x b)
    SHIFTL a b -> XSHIFTL (io_x a) (io_x b)
    SYS    a b   -> XSYS  a   (map io_x b)
    CALL   a b c -> XCALL a (io_x b) (map io_x c)
    LOCAL  a b   -> XLOCAL  (io_x a) (map io_x b)
    GOTO   a b   -> XGOTO   (io_x a) (map io_x b)
  x_io io = case io of
    XTAKE   a   -> TAKE   (x_io a)
    XNOT    a   -> NOT    (x_io a)  
    XSIZE   a   -> SIZE   (x_io a)  
    XADD    a b -> ADD    (x_io a) (x_io b)
    XSUB    a b -> SUB    (x_io a) (x_io b)
    XMULT   a b -> MULT   (x_io a) (x_io b)
    XDIV    a b -> DIV    (x_io a) (x_io b)
    XMOD    a b -> MOD    (x_io a) (x_io b)
    XINDEX  a b -> INDEX  (x_io a) (x_io b)
    XGRTR   a b -> GRTR   (x_io a) (x_io b)
    XLESS   a b -> LESS   (x_io a) (x_io b)
    XEQUL   a b -> EQUL   (x_io a) (x_io b)
    XAPPEND a b -> APPEND (x_io a) (x_io b)
    X_AND   a b -> AND    (x_io a) (x_io b)
    X_OR    a b -> OR     (x_io a) (x_io b)
    X_XOR   a b -> XOR    (x_io a) (x_io b)
    XSHIFTR a b -> SHIFTR (x_io a) (x_io b)
    XSHIFTL a b -> SHIFTL (x_io a) (x_io b)
    XSYS    a b -> SYS  a   (map x_io b)
    XCALL a b c -> CALL a (x_io b) (map x_io c)
    XLOCAL  a b -> LOCAL  (x_io a) (map x_io b)
    XGOTO   a b -> GOTO   (x_io a) (map x_io b)

----------------------------------------------------------------------------------------------------

data XThrow
  = XError  { thrownXData :: XData }
  | XReturn { thrownXData :: XData }

throwXData :: Monad m => XData -> RunT m ig
throwXData = throwError . XError

data Runtime m
  = Runtime
    { systemCalls     :: T.Tree Label (RunT m XData)
    , builtinModules  :: T.Tree Label (XModule, Maybe (XEval -> RunT m XData))
      -- ^ contains a directory of built-in modules paired with their XEval evaluators, which is an
      -- evaluator function which is used if the operand is constructed with 'XDATA'.
    , importedModules :: T.Tree Label XModule
    }

data EvalState
  = EvalState
    { evalCounter     :: Integer -- ^ counts how many evaluation steps have been taken
    , lastResult      :: XData
    , registers       :: M.Map Label XData
    , pcRegisters     :: M.Map Label Int
    , evalStack       :: [XData]
    , currentModule   :: Maybe XModule
    , currentBlock    :: Maybe (Array Int XCommand)
    , programCounter  :: Int
    }

initEvalState :: EvalState
initEvalState =
  EvalState
  { evalCounter    = 0
  , lastResult     = XNULL
  , registers      = mempty
  , pcRegisters    = mempty
  , evalStack      = []
  , currentModule  = Nothing
  , currentBlock   = Nothing
  , programCounter = 0
  }

-- | This is the monad used for evaluating the mini-Dao language. It is a
-- 'Control.Monad.Reader.MonadReader' where 'Control.Monad.Reader.ask' provides the an interface to
-- the 'Runtime' data structure, 'Control.Monad.State.state' provides an interface to update the
-- 'EvalState' data structure, 'Control.Monad.Trans.lift' allows you to lift the 'RunT' monad into
-- your own custom monad, and 'Control.Monad.IO.Class.liftIO' allows you to lift the 'RunT' monad
-- into your own custom @IO@ monad. The 'Control.Monad.Error.throwError' interface is provided which
-- lets you safely (without having to catch exceptions in the IO monad) throw an exception of type
-- 'XData', Lifting 'RunT' into the 'Control.Monad.Trans.Identity' monad is will allow all
-- evaluation to be converted to a pure function, however it will make it difficult to provide
-- system calls that do things like communicate with threads or read or write files.
newtype RunT m a =
  RunT{ runToPTrans :: PTrans XThrow (ReaderT (Runtime m) (StateT EvalState m)) a }

evalRun :: Monad m => Runtime m -> EvalState -> RunT m a -> m (PValue XThrow a)
evalRun env st (RunT f) = evalStateT (runReaderT (runPTrans f) env) st

instance Functor m => Functor (RunT m) where { fmap f (RunT m) = RunT (fmap f m) }
instance Monad m => Monad (RunT m) where
  return = RunT . return
  RunT a >>= f = RunT $ a >>= runToPTrans . f
  RunT a >> RunT b = RunT (a >> b)
  fail = RunT . pvalue . PFail . XError . XSTR . text
instance MonadPlus m => MonadPlus (RunT m) where
  mzero = RunT mzero
  mplus (RunT a) (RunT b) = RunT (mplus a b)
instance (Functor m, Monad     m) => Applicative (RunT m) where { pure = return; (<*>) = ap; }
instance (Functor m, MonadPlus m) => Alternative (RunT m) where { empty = mzero; (<|>) = mplus; }
instance Monad m => MonadError XThrow (RunT m) where
  throwError = RunT . throwError
  catchError (RunT try) catch = RunT (catchError try (runToPTrans . catch))
instance MonadIO m => MonadIO (RunT m) where { liftIO = RunT . liftIO }
instance MonadTrans RunT where { lift = RunT . lift . lift . lift }
instance Monad m => MonadState  EvalState   (RunT m) where { state = RunT . lift . lift . state }
instance Monad m => MonadReader (Runtime m) (RunT m) where
  local f (RunT m) = RunT (PTrans (local f (runPTrans m)))
  ask = RunT (lift ask)
instance (MonadPlus m, Monad m) => MonadPlusError XThrow (RunT m) where
  catchPValue (RunT f) = RunT (catchPValue f)
  assumePValue p = RunT (assumePValue p)

----------------------------------------------------------------------------------------------------

-- | The monadic function used to declare a 'Module'. Pass a monadic computation of this type to the
-- 'newModule' function.
newtype DeclareMethodsM m a
  = DeclareMethods{ runDeclMethods :: State (M.Map Label (RunT m XData)) a }
  deriving (Functor, Monad)
type DeclareMethods m = DeclareMethodsM m ()

-- | Defines a method for a module
newMethod :: String -> RunT m XData -> DeclareMethods m
newMethod nm fn = case readsPrec 0 nm of
  [(nm, "")] -> DeclareMethods (modify (\m -> M.insert (read nm) fn m))
  _          -> fail ("could not define function, invalid name string: "++show nm)

-- | The monadic function used to declare a 'Runtime'. You can use this monad to build up a kind of
-- "default" module. When it comes time to run a mini-Dao program, this default module will be used
-- to initialize the runtime for the mini-Dao evaluator.
newtype DeclareRuntimeM m a
  = DeclareRuntime { runDeclModule :: State (Runtime m) a } deriving (Functor, Monad)
type DeclareRuntime m = DeclareRuntimeM m ()

-- | Declare a built-in module for this runtime. The first paramater is the name of this module.
-- Here are the details about how this works:
-- 
-- Suppose you are declaring a module "myModule", and you declare a function @myFunc@ with
-- @'newMethod' "myFunc" (...)@. When 'newModule' is evaluated, a new system call will be added to
-- the 'systemCalls' table at the address @"myModule.myFunc"@. Then, a 'MODULE' is constructed,
-- and in this module a public variable declaration @"myFunc"@ is created which stores a 'FUNC'
-- object, and this 'FUNC' object is a system call to the @"myModule.myFunc"@ function.
newModule :: String -> DeclareMethods m -> Maybe (XEval -> RunT m XData) -> DeclareRuntime m
newModule name decls opfunc = DeclareRuntime $ case readsPrec 0 name of
  [(addr, "")] -> do
    bi <- gets builtinModules
    let modlbl   = addrToLabels addr
        defs     = execState (runDeclMethods decls) M.empty
        syscalls = T.Branch (M.map T.Leaf defs)
        mod =
          XMODULE
          { ximports = []
          , xprivate = XDefines mempty
          , xpublic  = XDefines $ flip M.mapWithKey defs $ \func _ ->
              XFUNC [] $ XBlock $ mkXArray $
                [ XEVAL   (XSYS (labelsToAddr (modlbl++[func])) [])
                , XRETURN XRESULT
                ]
          , xrules   = []
          }
    modify $ \st ->
      st{ builtinModules = T.insert modlbl (mod, opfunc) (builtinModules st)
        , systemCalls    = T.alter (T.union syscalls) modlbl (systemCalls st)
        }
  _ -> fail ("initializing built-in module, string cannot be used as module name: "++show name)

-- | Declare a system call in the runtime.
newSystemCall :: String -> RunT m XData -> DeclareRuntime m
newSystemCall name func = DeclareRuntime $ modify $ \st ->
  st{ systemCalls = T.insert [Label $ text name] func (systemCalls st) }

----------------------------------------------------------------------------------------------------

-- | A class of evaluate-able data types: Any data type that can be treated as "code" to be run in
-- the mini-Dao runtime should instantiate this class.
class Evaluable a where { eval :: forall m . Monad m => a -> RunT m XData }

setRegister :: Monad m => Label -> XData -> RunT m ()
setRegister a b = modify (\st -> st{registers = M.insert a b (registers st)})

setResult :: Monad m => XData -> RunT m XData
setResult a = modify (\st -> st{lastResult=a}) >> return a

evalError :: Monad m => String -> String -> XData -> RunT m ig
evalError clas msg dat = case (readsPrec 0 clas, readsPrec 0 msg) of
  ([(clas, "")], [(msg, "")]) -> throwXData $ XDATA clas (M.singleton msg dat)
  _ -> fail $ concat [clas, ": ", msg, "\n\t", show (x_io dat)]

lookupModule :: Monad m => Address -> RunT m XModule
lookupModule addr = asks builtinModules >>= \bi -> asks importedModules >>= \im ->
  maybe (evalError "Undefined" "moduleName" (XPTR addr)) return $
    let lbls = addrToLabels addr in mplus (fmap fst $ T.lookup lbls bi) (T.lookup lbls im)

instance Evaluable XLookup where
  eval o = case o of
    XRESULT     -> gets lastResult
    XCONST   o  -> return o
    XVAR     o  -> gets registers >>= \reg -> case M.lookup o reg of
      Nothing -> evalError "Undefined" "variableName" (XSTR (text $ show o))
      Just  o -> return o
    XDEREF   o  -> do
      mod <- gets currentModule
      case msum $ map (fmap defsToMap >=> M.lookup o) [fmap xprivate mod, fmap xpublic mod] of
        Nothing -> evalError "Undefined" "moduleVariable" (XSTR (text $ show o))
        Just  o -> return o
    XLOOKUP a b -> do
      mod <- lookupModule a
      case M.lookup b (defsToMap $ xpublic mod) of
        Nothing -> throwXData $ XDATA (read "Undefined") $ M.fromList $
          [(read "inModule", XPTR a), (read "variableName", XSTR (text $ show b))]
        Just  o -> return o

-- Increment evaluation counter
incEC :: Monad m => RunT m ()
incEC = modify (\st -> st{evalCounter=evalCounter st + 1})

-- Increment program counter
incPC :: Monad m => RunT m ()
incPC = modify (\st -> st{programCounter=programCounter st + 1})

instance Evaluable XCommand where
  eval o = incEC >> incPC >> case o of
    XLOAD    a -> do
      reg <- gets registers
      case M.lookup a reg of
        Nothing -> evalError "Undefined" "variableName" (XSTR (text $ show a))
        Just  a -> setResult a
    XSTORE   a -> gets lastResult >>= \b -> setRegister a b >> return b
    XSETJUMP a -> gets lastResult
    XJUMP    a -> do
      pcreg <- gets pcRegisters
      let badJump = evalError "Undefined" "jumpTo" (XSTR (text $ show a))
      case M.lookup a pcreg of
        Nothing -> badJump
        Just  i -> get >>= \st -> case currentBlock st of
          Nothing -> badJump
          Just  b ->
            if inRange (bounds b) i then put (st{programCounter=i}) >> return XNULL else badJump
    XPUSH    a -> eval a >>= \b -> modify (\st -> st{evalStack = b : evalStack st}) >> return b
    XPEEK      -> gets evalStack >>= \ax -> case ax of
      []  -> throwXData $ XDATA (read "StackUnderflow") mempty
      a:_ -> setResult a
    XPOP       -> get >>= \st -> case evalStack st of
      []   -> throwXData $ XDATA (read "StackUnderflow") mempty
      a:ax -> put (st{evalStack=ax}) >> setResult a
    XEVAL    a -> eval a
    XDO      a -> eval a
    XRETURN  a -> eval a >>= \a -> throwError (XReturn a)
    XTHROW   a -> eval a >>= \a -> throwError (XError  a)

instance Evaluable XCondition where
  eval o = incEC >> case o of
    XIF      a b -> eval a >>= \a -> case a of
      XNULL -> return XNULL
      _     -> eval b
    XIFNOT   a b -> eval a >>= \a -> case a of
      XNULL -> eval b
      _     -> return XNULL

instance Evaluable XBlock where
  eval (XBlock cmds) = fix $ \loop -> incEC >> do
    pc <- gets programCounter
    block <- gets currentBlock
    case block of
      Just block | inRange (bounds block) pc -> eval (block!pc) >> loop
      _                                      -> gets lastResult

badEval :: Monad m => XEval -> [(Label, XData)] -> RunT m ig
badEval o other = throwXData $ XDATA (read "BadInstruction") $ M.fromList $
  [ (Label $ text "instruction", XSTR $ text $ show $ x_io o)
  , (Label $ text "problem"
    , XSTR $ text ("evaluated operator on incorrect type of data")
    )
  ] ++ other

evalDataOperand :: Monad m => Address -> XEval -> RunT m XData
evalDataOperand addr o = do
  -- avoids use of 'fmap' or '(<$>)' so we are not forced to put Functor in the context
  tabl <- asks builtinModules >>= return . join . fmap snd . T.lookup (addrToLabels addr)
  case tabl of
    Nothing   -> badEval o [(Label $ text "dataType", XPTR addr)]
    Just eval -> eval o

matchArgs :: Monad m => [Label] -> [XData] -> RunT m ()
matchArgs argVars argValues = do
  let loop m ax bx = case (ax, bx) of
        (ax  , []  ) -> Just (m, [])
        ([]  , _   ) -> Nothing
        (a:ax, b:bx) -> loop ((a,b):m) ax bx
  case loop [] argVars argValues of
    Nothing -> evalError "FuncCall" "notEnoughParams" (XLIST (mkXArray argValues))
    Just (elms, rem) -> get >>= \old ->
      modify $ \st -> st{registers=M.fromList elms, evalStack=rem}

setupJumpRegisters :: Monad m => Array Int XCommand -> RunT m ()
setupJumpRegisters block = modify $ \st ->
  st{ pcRegisters = M.fromList $
        flip concatMap (assocs block) $ \ (i, x) -> case x of
          XSETJUMP x -> [(x, i)]
          _          -> []
    }

callLocalMethod :: Monad m => XLookup -> [XData] -> RunT m XData
callLocalMethod lbl argValues = do
  func <- eval lbl
  case func of
    XFUNC argVars (XBlock (Just block)) -> do
      let loop m ax bx = case (ax, bx) of
            (ax  , []  ) -> Just (m, [])
            ([]  , _   ) -> Nothing
            (a:ax, b:bx) -> loop ((a,b):m) ax bx
      case loop [] argVars argValues of
        Nothing -> evalError "FuncCall" "notEnoughParams" (XLIST (mkXArray argValues))
        Just (elms, rem) -> get >>= \old -> do
          matchArgs argVars argValues
          setupJumpRegisters block
          modify $ \st ->
            st{ currentBlock   = Just block
              , programCounter = programCounter old
              }
          catchError (eval (XBlock $ Just block)) $ \err -> case err of
            XError  err -> throwXData err
            XReturn dat -> do
              modify $ \st ->
                st{ registers      = registers old
                  , pcRegisters    = pcRegisters old
                  , evalStack      = evalStack old
                  , currentBlock   = currentBlock old
                  , programCounter = programCounter old
                  }
              return dat
    XFUNC _ (XBlock Nothing) -> gets lastResult
    _ -> throwXData $ XDATA (read "FuncCall") $ M.fromList $
            [ (read "problem", XSTR (text "target of call is not executable data"))
            , (read "targetRegister", XSTR (text $ show $ x_io lbl))
            ]

arithmetic
  :: Monad m
  => XEval
  -> (forall a . Integral a => a -> a -> a)
  -> (forall b . Fractional b => b -> b -> b)
  -> XEval -> XEval -> RunT m XData
arithmetic o num frac a b = do
  a <- eval a
  b <- eval b
  case (a, b) of
    (XINT   a, XINT   b) -> return (XINT   $ num  a b)
    (XFLOAT a, XFLOAT b) -> return (XFLOAT $ frac a b)
    _                    -> badEval o []

boolean :: Monad m => XEval -> (forall a . Ord a => a -> a -> Bool) -> XEval -> XEval -> RunT m XData
boolean o comp a b = eval a >>= \a -> eval b >>= \b -> case (a, b) of
  (XINT   a, XINT   b) -> return (if comp a b then XTRUE else XNULL)
  (XFLOAT a, XFLOAT b) -> return (if comp a b then XTRUE else XNULL)
  _                    -> badEval o []

bitwise :: Monad m => XEval -> (Int -> Int -> Int) -> XEval -> XEval -> RunT m XData
bitwise o bit a b = eval a >>= \a -> eval b >>= \b -> case (a, b) of
  (XINT a, XINT b) -> return (XINT $ bit a b)
  _                -> badEval o []

instance Evaluable XEval where
  eval o = incEC >> case o of
    XTAKE   a     -> eval a
    XNOT    a     -> eval a >>= \a -> case a of
      XNULL     -> return XTRUE
      XTRUE     -> return XNULL
      XINT  a   -> return (XINT $ a `xor` 0xFFFFFFFFFFFFFFFF)
      XDATA a _ -> evalDataOperand a o
      _         -> badEval o []
    XSIZE   a     -> eval a >>= \a -> case a of
      XNULL     -> return XNULL
      XTRUE     -> return XTRUE
      XINT    a -> return (XINT   $ abs a)
      XFLOAT  a -> return (XFLOAT $ abs a)
      XLIST   a -> return (XINT   $ maybe 0 (fromIntegral .(+1).abs. uncurry subtract . bounds) a)
      XDATA a _ -> evalDataOperand a o
      _         -> badEval o []
    XADD    a b -> arithmetic o (+) (+) a b
    XSUB    a b -> arithmetic o (-) (-) a b
    XMULT   a b -> arithmetic o (*) (*) a b
    XDIV    a b -> arithmetic o div (/) a b
    XMOD    a b -> eval a >>= \a -> eval b >>= \b -> case (a, b) of
      (XINT a, XINT b) -> return (XINT $ mod a b)
      _                -> badEval o []
    XINDEX  a b -> eval a >>= \a -> eval b >>= \b -> case (a, b) of
      (XINT i, XLIST l  ) -> return (maybe XNULL (!i) l)
      (XSTR i, XDATA _ m) -> return (maybe XNULL id (M.lookup (Label i) m))
      (XTRUE , XDATA p _) -> return (XPTR p)
    XGRTR   a b -> boolean o (>)  a b
    XGREQ   a b -> boolean o (>=) a b
    XLESS   a b -> boolean o (<)  a b
    XLSEQ   a b -> boolean o (<=) a b
    XEQUL   a b -> return (if a==b then XTRUE else XNULL)
    XNEQL   a b -> return (if a/=b then XTRUE else XNULL)
    XAPPEND a b -> eval a >>= \a -> eval b >>= \b -> case (a, b) of
      (XSTR (Text a), XSTR (Text b)) -> return (XSTR $ Text $ U.fromString (U.toString a ++ U.toString b))
      (XLIST a      , XLIST b      ) -> let el = maybe [] elems in return (XLIST (mkXArray (el a ++ el b)))
    X_AND   a b -> bitwise o (.&.) a b
    X_OR    a b -> bitwise o (.|.) a b
    X_XOR   a b -> bitwise o  xor  a b
    XSHIFTR a b -> bitwise o shift a b
    XSHIFTL a b -> bitwise o (\a b -> shift a (negate b)) a b
    XSYS    a b -> do
      syscall <- asks systemCalls >>= return . T.lookup (addrToLabels a)
      case syscall of
        Nothing -> evalError "Undefined" "systemCall" (XPTR a)
        Just fn -> do
          oldstk <- gets evalStack
          b      <- mapM eval b
          modify (\st -> st{evalStack=reverse b})
          dat <- catchError fn $ \err -> case err of
            XReturn dat -> return dat
            XError  err -> throwXData $
              XDATA (read "SystemCall") (M.singleton (Label $ text "exception") err)
          modify (\st -> st{evalStack=oldstk})
          setResult dat
    XCALL   a b c -> do
      mod <- lookupModule a
      curmod <- gets currentModule
      c      <- mapM eval c
      modify (\st -> st{currentModule=Just mod})
      catchError (callLocalMethod b c) $ \err -> case err of
        XReturn dat -> modify (\st -> st{currentModule=curmod}) >> return dat
        XError  err -> throwXData err
    XLOCAL  a b   -> mapM eval b >>= callLocalMethod a
    XGOTO   a b   -> eval a >>= \a -> case a of
      XFUNC argParams (XBlock (Just block)) -> do
        stk <- gets evalStack
        argValues <- mapM eval b
        modify $ \st ->
          st{ registers      = mempty
            , evalStack      = []
            , programCounter = 0
            , currentBlock   = Just block
            }
        matchArgs argParams (reverse stk ++ argValues)
        setupJumpRegisters block
        gets lastResult
      XFUNC _ (XBlock Nothing) -> gets lastResult
      _ -> badEval o [(read "register", XSTR (text "target of GOTO is non-function data type"))]

