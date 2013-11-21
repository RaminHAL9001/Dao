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

-- | A simplified version of the Dao runtime.
module Dao.Simple where

import           Dao.Predicate
import qualified Dao.Tree                  as T

import           Data.Char
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
  deriving (Eq, Ord, Show, Read)
data XData
  = XNULL | XTRUE | XINT Int | XFLOAT Double | XSTR Text | XPTR Address
  | XLIST (Maybe (Array Int XData))
  | XDATA Address (M.Map Label XData)
  deriving (Eq, Ord)
instance RWX RWData XData where
  io_x io = case io of
    NULL     -> XNULL
    TRUE     -> XTRUE
    INT   io -> XINT       io
    FLOAT io -> XFLOAT     io
    STR   io -> XSTR       io
    PTR   io -> XPTR       io
    LIST  io -> XLIST $ if null io then Nothing else Just $ listArray (0, length io - 1) (map io_x io)
    DATA a b -> XDATA a (M.fromList $ map ((fmap io_x) . defToPair) b)
  x_io x  = case x  of
    XNULL     -> NULL
    XTRUE     -> TRUE
    XINT   x  -> INT        x
    XFLOAT x  -> FLOAT      x
    XSTR   x  -> STR        x
    XPTR   x  -> PTR        x
    XLIST  x  -> LIST $ maybe [] (map x_io . elems) x
    XDATA a b -> DATA a (map (uncurry DEFINE . fmap x_io) (M.assocs b))

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
  io_x (RULE a b) = XRULE a (io_x b)
  x_io (XRULE a b) = RULE a (x_io b)

data RWDef = DEFINE Label RWData deriving (Eq, Ord, Show, Read)
defToPair :: RWDef -> (Label, RWData)
defToPair (DEFINE a b) = (a, b)

newtype XDefines = XDefines { defsToMap :: M.Map Label XData } deriving (Eq, Ord)
instance RWX [RWDef] XDefines where
  io_x = XDefines . M.fromList . map (\ (DEFINE a b) -> (a, io_x b) )
  x_io (XDefines x) = map (\ (a, b) -> DEFINE a (x_io b)) (M.assocs x)

data RWLookup
  = RESULT | CONST RWData | VAR Label | DEREF Label | LOOKUP Address Label
  deriving (Eq, Ord, Show, Read)
data XLookup
  = XRESULT | XCONST XData | XVAR Label | XDEREF Label | XLOOKUP Address Label
  deriving (Eq, Ord)
instance RWX RWLookup XLookup where
  io_x io = case io of
    RESULT     -> XRESULT
    CONST   io -> XCONST  (io_x io)
    VAR     io -> XVAR          io 
    DEREF   io -> XDEREF        io
    LOOKUP a b -> XLOOKUP       a   b
  x_io x  = case x  of
    XRESULT     -> RESULT
    XCONST   x  -> CONST   (x_io x)
    XVAR     x  -> VAR           x 
    XDEREF   x  -> DEREF         x
    XLOOKUP a b -> LOOKUP        a  b

data RWCommand
  = EVAL   RWEval           -- ^ evaluate a lisp-like expression
  | DO     RWCondition      -- ^ conditional evaluation
  | COPY   Label RWLookup   -- ^ copy data directly to a register
  | LOCAL  Label [RWEval]    -- ^ call a local function, push the result onto the stack
  | GOTO   Label [RWEval]    -- ^ goto a local function, never return
  | RETURN RWLookup          -- ^ end evaluation
  deriving (Eq, Ord, Show, Read)
data XCommand
  = XEVAL   XEval            -- ^ evaluate a lisp-like expression
  | XDO     XCondition       -- ^ branch conditional
  | XCOPY   Label XLookup    -- ^ copy data directly to a register
  | XLOCAL  Label [XEval]    -- ^ call a local function, push the result onto the stack
  | XGOTO   Label [XEval]    -- ^ goto a local function, never return
  | XRETURN XLookup          -- ^ end evaluation
  deriving (Eq, Ord)
instance RWX RWCommand XCommand where
  io_x io = case io of
    EVAL   a     -> XEVAL   (io_x a)
    DO     a     -> XDO     (io_x a)
    COPY   a b   -> XCOPY         a  (io_x b)
    LOCAL  a b   -> XLOCAL        a  (map io_x b)
    GOTO   a b   -> XGOTO         a  (map io_x b)
    RETURN a     -> XRETURN (io_x a)
  x_io x  = case x of
    XEVAL   a     -> EVAL   (x_io a)
    XDO     a     -> DO     (x_io a)
    XCOPY   a b   -> COPY         a  (x_io b)
    XLOCAL  a b   -> LOCAL        a  (map x_io b)
    XGOTO   a b   -> GOTO         a  (map x_io b)
    XRETURN a     -> RETURN (x_io a)

newtype XBlock = XBlock { toCommands :: [XCommand] } deriving (Eq, Ord)
instance RWX [RWCommand] XBlock where
  io_x io = XBlock (map io_x io)
  x_io (XBlock x) = map x_io x

data RWFunc = FUNC [Label] [RWCommand] deriving (Eq, Ord, Show, Read)
data XFunc = XFUNC [Label] XBlock deriving (Eq, Ord)
instance RWX RWFunc XFunc where
  io_x (FUNC lbls cmds) = XFUNC lbls (io_x cmds)
  x_io (XFUNC lbls cmds) = FUNC lbls (x_io cmds)

data RWCondition = IF RWLookup [RWCommand] | IFNOT RWLookup [RWCommand] | FOREACH RWLookup [RWCommand] deriving (Eq, Ord, Show, Read)
data XCondition = XIF XLookup XBlock | XIFNOT XLookup XBlock | XFOREACH XLookup XBlock deriving (Eq, Ord)
instance RWX RWCondition XCondition where
  io_x io = case io of
    IF      a b -> XIF      (io_x a) (io_x b)
    IFNOT   a b -> XIFNOT   (io_x a) (io_x b)
    FOREACH a b -> XFOREACH (io_x a) (io_x b)
  x_io io = case io of
    XIF      a b -> IF      (x_io a) (x_io b)
    XIFNOT   a b -> IFNOT   (x_io a) (x_io b)
    XFOREACH a b -> FOREACH (x_io a) (x_io b)

data RWEval
  = TAKE   RWLookup
  | NOT    RWLookup
  | ABS    RWLookup
  | SIZE   RWLookup
  | ADD    RWLookup RWLookup
  | SUB    RWLookup RWLookup
  | MULT   RWLookup RWLookup
  | DIV    RWLookup RWLookup
  | MOD    RWLookup RWLookup
  | INDEX  RWLookup RWLookup
  | GRTR   RWLookup RWLookup
  | LESS   RWLookup RWLookup
  | EQUL   RWLookup RWLookup
  | APPEND RWLookup RWLookup
  | AND    RWLookup RWLookup
  | OR     RWLookup RWLookup
  | XOR    RWLookup RWLookup
  | SHIFTR RWLookup RWLookup
  | SHIFTL RWLookup RWLookup
  | SYS    Address  [RWEval] -- ^ system call
  | CALL   RWLookup Label [RWEval]
    -- ^ Select the data type from register #1, call a function in it's corresponding module by the
    -- name in register #2 using the parameters 
  deriving (Eq, Ord, Show, Read)
data XEval
  = XTAKE   XLookup
  | XNOT    XLookup
  | XABS    XLookup
  | XSIZE   XLookup
  | XADD    XLookup XLookup
  | XSUB    XLookup XLookup
  | XMULT   XLookup XLookup
  | XDIV    XLookup XLookup
  | XMOD    XLookup XLookup
  | XINDEX  XLookup XLookup
  | XGRTR   XLookup XLookup
  | XLESS   XLookup XLookup
  | XEQUL   XLookup XLookup
  | XAPPEND XLookup XLookup
  | X_AND   XLookup XLookup
  | X_OR    XLookup XLookup -- ^ not to be confused with ORX
  | X_XOR   XLookup XLookup
  | XSHIFTR XLookup XLookup
  | XSHIFTL XLookup XLookup
  | XSYS    Address  [XEval] -- ^ system call
  | XCALL   XLookup Label [XEval] -- ^ call code in a module
  deriving (Eq, Ord)
instance RWX RWEval XEval where
  io_x x  = case x  of
    TAKE   a   -> XTAKE   (io_x a)
    NOT    a   -> XNOT    (io_x a)  
    ABS    a   -> XABS    (io_x a)  
    SIZE   a   -> XSIZE   (io_x a)  
    ADD    a b -> XADD    (io_x a) (io_x b)
    SUB    a b -> XSUB    (io_x a) (io_x b)
    MULT   a b -> XMULT   (io_x a) (io_x b)
    DIV    a b -> XDIV    (io_x a) (io_x b)
    MOD    a b -> XMOD    (io_x a) (io_x b)
    INDEX  a b -> XINDEX  (io_x a) (io_x b)
    GRTR   a b -> XGRTR   (io_x a) (io_x b)
    LESS   a b -> XLESS   (io_x a) (io_x b)
    EQUL   a b -> XEQUL   (io_x a) (io_x b)
    APPEND a b -> XAPPEND (io_x a) (io_x b)
    AND    a b -> X_AND   (io_x a) (io_x b)
    OR     a b -> X_OR    (io_x a) (io_x b)
    XOR    a b -> X_XOR   (io_x a) (io_x b)
    SHIFTR a b -> XSHIFTR (io_x a) (io_x b)
    SHIFTL a b -> XSHIFTL (io_x a) (io_x b)
    SYS    a b   -> XSYS  a   (map io_x b)
    CALL   a b c -> XCALL (io_x a) b (map io_x c)
  x_io io = case io of
    XTAKE   a   -> TAKE   (x_io a)
    XNOT    a   -> NOT    (x_io a)  
    XABS    a   -> ABS    (x_io a)  
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
    XCALL a b c -> CALL (x_io a) b (map x_io c)

----------------------------------------------------------------------------------------------------

newtype Error = Error { errorToXData :: XData }

data Runtime m
  = Runtime
    { systemCalls    :: M.Map Label (RunT m XData)
    , builtinModules :: M.Map Label (Module m)
    }

newtype RunT m a = RunT { runToPTrans :: PTrans Error (ReaderT (Runtime m) m) a }

instance Functor m => Functor (RunT m) where { fmap f (RunT m) = RunT (fmap f m) }
instance Monad m => Monad (RunT m) where
  return = RunT . return
  RunT a >>= f = RunT $ a >>= runToPTrans . f
  RunT a >> RunT b = RunT (a >> b)
  fail = RunT . pvalue . PFail . Error . XSTR . text
instance MonadPlus m => MonadPlus (RunT m) where
  mzero = RunT mzero
  mplus (RunT a) (RunT b) = RunT (mplus a b)
instance (Functor m, Monad     m) => Applicative (RunT m) where { pure = return; (<*>) = ap; }
instance (Functor m, MonadPlus m) => Alternative (RunT m) where { empty = mzero; (<|>) = mplus; }
instance Monad m => MonadError Error (RunT m) where
  throwError = RunT . throwError
  catchError (RunT try) catch = RunT (catchError try (runToPTrans . catch))
instance MonadIO m => MonadIO (RunT m) where { liftIO = RunT . liftIO }
instance MonadTrans RunT where { lift = RunT . lift . lift }

----------------------------------------------------------------------------------------------------

-- | The monadic function used to declare a 'Module'. Pass a monadic computation of this type to the
-- 'newModule' function.
newtype DeclareMethodsM m a
  = DeclareMethods { runDeclMethods :: State (M.Map Label (RunT m XData)) a } deriving (Functor, Monad)
type DeclareMethods m = DeclareMethodsM m ()

-- | Defines a method for a module
newMethod :: String -> RunT m XData -> DeclareMethods m
newMethod nm fn = case readsPrec 0 nm of
  [(nm, "")] -> DeclareMethods (modify (\m -> M.insert (read nm) fn m))
  _          -> fail ("could not define function, invalid name string: "++show nm)

-- | The data type used to provide a built-in module.
data Module m
  = Module
    { modName      :: Label
      -- ^ the name used to index this module in the runtime
    , modPublic    :: M.Map Label (RunT m XData)
      -- ^ the 'DeclareMethods' monad used to define public methods for this module.
    , modOperators :: Maybe (XEval -> RunT m XData)
      -- ^ a function used to evaluate operators.
    }

-- | The monadic function used to declare a 'Runtime'.
newtype DeclareRuntimeM m a
  = DeclareRuntime { runDeclModule :: State (Runtime m) a } deriving (Functor, Monad)
type DeclareRuntime m = DeclareRuntimeM m ()

-- | Declare a module in the runtime.
newModule :: String -> DeclareMethods m -> Maybe (XEval -> RunT m XData) -> DeclareRuntime m
newModule name decls opfunc = DeclareRuntime $ modify $ \st -> let lbl = Label (text name) in
  st{ builtinModules = flip (M.insert lbl) (builtinModules st) $
        Module
        { modName      = lbl
        , modOperators = opfunc
        , modPublic    = execState (runDeclMethods decls) M.empty
        }
    }

-- | Declare a system call in the runtime.
newSystemCall :: String -> RunT m XData -> DeclareRuntime m
newSystemCall name func = DeclareRuntime $ modify $ \st ->
  st{ systemCalls = M.insert (Label $ text name) func (systemCalls st) }

----------------------------------------------------------------------------------------------------

class Executable a m where { exec :: a -> RunT m XData }

