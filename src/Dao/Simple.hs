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
import           Data.List (intercalate, stripPrefix)
import           Data.Monoid
import           Data.Dynamic
import           Data.Array.IArray
import qualified Data.Map                  as M
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy      as Z
import           Data.Functor.Identity

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Error hiding (Error)
import           Control.Monad.Trans
import           Control.Monad.IO.Class

import           System.IO

----------------------------------------------------------------------------------------------------

data DaoIOState st
  = DaoIOState
    { userState   :: st
    , ioRuntime   :: Runtime (DaoIO st)
    , fileHandles :: M.Map Text Handle
    }

initDaoIOState :: st -> DaoIOState st
initDaoIOState ust = DaoIOState{userState=ust, ioRuntime=initRuntime, fileHandles=mempty}

-- | This is a simple monadic interface for interacting with a mini-Dao runtime. You can evaluate
-- computations of this type using 'runDaoIO' in your main function. It instantiates
-- 'Control.Monad.IO.MonadIO' so you can read from and write to a command line interface, load
-- modules from files fork threads, and do anything else you need to do. This monad takes an
-- optional state parameter which could be @()@, or it could contain a data structure containing
-- stateful data, for example a command line history.
-- 
-- When you run a computation using 'runDaoIO', you should provide builtin modules using
-- 'installModule', and load modules from files using 'loadModule'. You can then construct a
-- read-eval-print loop which pre-processes input into a list of @['Text']@ objects, and broadcasts
-- that input to the various modules. You can select which modules you want using 'selectModule',
-- and you can evaluate a query in a module using 'evalQuery'.
newtype DaoIO st a = DaoIO{ daoStateT :: StateT (DaoIOState st) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
instance MonadState st (DaoIO st) where
  state f = DaoIO $ state $ \st -> let (a, ust) = f (userState st) in (a, st{userState=ust})

-- | Using the 'DeclareRuntime' monad, you can install built-in modules. Built-in modules are
-- constructed using the 'DeclareRuntime' interface, the 'installModule' function applies this
-- function to the runtime. For example, suppose you have a function called @xmlFile@ of type
-- 'DeclareRuntime' which constructs a built-in module, and this module provides an interface for
-- the mini-Dao language to work with XML files. If you want this module to be provided in your Dao
-- program, simply evaluate @'installModule' xmlFile@ in the main 'runDaoIO' function.
installModule :: DeclareRuntime (DaoIO st) -> DaoIO st ()
installModule (DeclareRuntime mod) = do
  st  <- DaoIO get
  run <- execStateT mod (ioRuntime st)
  DaoIO $ put (st{ioRuntime=run})

-- | Provide a list of 'Prelude.String's that will be used to select modules. Modules that could not
-- be selected will ouput an error message.
selectModule :: [String] -> DaoIO st [XModule]
selectModule qs = fmap concat $ forM qs $ \q -> do
  let err msg = liftIO (hPutStrLn stderr ("(selectModule) "++msg)) >> return []
  case readsPrec 0 q of
    [(addr, "")] -> do
      run <- DaoIO $ gets ioRuntime
      (pval, run) <- evalRun run $ lookupModule addr
      case pval of
        OK    mod -> return [mod]
        Backtrack -> err ("unknown error occurred when selecting "++show addr)
        PFail msg -> case msg of
          XError  msg -> err (show $ x_io msg)
          XReturn msg -> err (show $ x_io msg)
    _ -> err ("invalid address "++show q) >> return []

-- | Select all modules.
selectAllModules :: DaoIO st [XModule]
selectAllModules = DaoIO $ gets (map getXModule . T.elems . loadedModules . ioRuntime)

-- | Evaluate a pre-processed string query. It is up to your implementation to do the preprocessing
-- of the string, for example, converting to all lower-case or splitting the string by whitespaces.
-- As a reminder, you can use the 'text' function to convert a 'Prelude.String' to a 'Text' data
-- type.
evalQuery :: [Text] -> [XModule] -> DaoIO st ()
evalQuery qs mods = forM_ mods $ \mod ->
  DaoIO (gets ioRuntime) >>= \run -> evalRun run (evalRulesInMod qs mod)

----------------------------------------------------------------------------------------------------

-- | The monadic function used to declare a 'Module'. Pass a monadic computation of this type to the
-- 'newModule' function.
newtype DeclareMethodsM m a
  = DeclareMethods{ runDeclMethods :: State (M.Map Label (RunT m XData), [XRule]) a }
  deriving (Functor, Monad)
type DeclareMethods m = DeclareMethodsM m ()

-- | Defines a method for a module with the given name.
newMethod :: String -> RunT m XData -> DeclareMethods m
newMethod nm fn = case readsPrec 0 nm of
  [(nm, "")] -> DeclareMethods (modify (\ (m, r) -> (M.insert (read nm) fn m, r)))
  _          -> fail ("could not define function, invalid name string: "++show nm)

-- | Declare a new rule for this module.
newRule :: XRule -> DeclareMethods m
newRule rul = DeclareMethods (modify (\ (m, r) -> (m, rul:r)))

-- | The monadic function used to declare a 'Runtime'. You can use this monad to build up a kind of
-- "default" module. When it comes time to run a mini-Dao program, this default module will be used
-- to initialize the runtime for the mini-Dao evaluator.
type DeclareRuntime m = DeclareRuntimeM m ()
newtype DeclareRuntimeM m a
  = DeclareRuntime { runDeclModule :: StateT (Runtime m) m a } deriving (Functor, Monad)
instance Monad m => MonadState (Runtime m) (DeclareRuntimeM m) where
  state f = DeclareRuntime (state f)
instance MonadTrans DeclareRuntimeM where { lift = DeclareRuntime . lift }

-- | Declare a built-in module for this runtime. The first paramater is the name of this module.
-- Here are the details about how this works:
-- 
-- Suppose you are declaring a module "myModule", and you declare a function @myFunc@ with
-- @'newMethod' "myFunc" (...)@. When 'newModule' is evaluated, a new system call will be added to
-- the 'systemCalls' table at the address @"myModule.myFunc"@. Then, a 'MODULE' is constructed,
-- and in this module a public variable declaration @"myFunc"@ is created which stores a 'FUNC'
-- object, and this 'FUNC' object is a system call to the @"myModule.myFunc"@ function.
newModule :: Monad m => String -> DeclareMethods m -> Maybe (XEval -> RunT m XData) -> DeclareRuntime m
newModule name decls opfunc = DeclareRuntime $ case readsPrec 0 name of
  [(addr, "")] -> do
    bi <- gets loadedModules
    let modlbl       = addrToLabels addr
        (defs, ruls) = execState (runDeclMethods decls) (M.empty, [])
        syscalls     = T.Branch (M.map T.Leaf defs)
        mod =
          XMODULE
          { ximports = []
          , xprivate = XDefines mempty
          , xpublic  = XDefines $ flip M.mapWithKey defs $ \func _ ->
              XFUNC $ XFunc [] $ XBlock $ mkXArray $
                [ XEVAL  (XSYS (labelsToAddr (modlbl++[func])) [])
                , XRETURN XRESULT
                ]
          , xrules   = ruls
          }
    modify $ \st ->
      st{ loadedModules = T.insert modlbl (BuiltinModule mod opfunc) (loadedModules st)
        , systemCalls   = T.alter (T.union syscalls) modlbl (systemCalls st)
        }
  _ -> fail ("initializing built-in module, string cannot be used as module name: "++show name)

-- | Declare a system call in the runtime. The system call need not be part of any module, it can
-- simply be declared with a name like "print" and the system call will be accessible to the 'SYS'
-- instruction.
newSystemCall :: Monad m => String -> RunT m XData -> DeclareRuntime m
newSystemCall name func = DeclareRuntime $ modify $ \st ->
  st{ systemCalls = T.insert [Label $ text name] func (systemCalls st) }

printer :: MonadIO m => Handle -> RunT m XData
printer h = do
  let tostr o = case o of
        XNULL         -> "FALSE"
        XTRUE         -> "TRUE"
        XSTR (Text o) -> U.toString o
        XINT       o  -> show o
        XFLOAT     o  -> show o
        XLIST      o  -> show (maybe [] (map x_io . elems) o)
        XPTR       o  -> show o
        XDATA addr o  -> concat ["(DATA ", show addr, ' ':show (map (fmap x_io) $ M.assocs o)]
        XFUNC (XFunc args  o) -> concat ["(FUNC ", show args, ' ' : show (x_io o), ")"]
  str <- gets evalStack >>= return . concatMap tostr . reverse
  liftIO (hPutStrLn h str)
  return (mkXStr str)

-- | This is the default module which provides basic IO services to the mini-Dao runtime, like
-- print.
basicIO :: MonadIO m => DeclareRuntime m
basicIO = do
  newSystemCall "print" (printer stdout)
  newSystemCall "error" (printer stderr)

fileIO :: DeclareRuntime (DaoIO st)
fileIO = do
  let getPath = do
        str <- popEvalStack
        case str of
          XSTR path -> return (str, path)
          path      -> throwXData "FunctionParamater" $
            [ ("stackItem", path)
            , ("problem", mkXStr "expecting file path parameter")
            ]
      open func mode = do
        (str, path) <- getPath
        evalStackEmptyElse func []
        h <- liftIO (openFile (textChars path) mode)
        lift $ DaoIO $ modify (\st -> st{fileHandles = M.insert path h (fileHandles st)})
        return $ mkXData "file" [("path", XSTR path)]
      newOpenMethod func mode = newMethod func $ open func mode
      newHandleMethod func fn = newMethod func $ do
        xdat <- gets lastResult
        path <- mplus (tryXMember "path" xdat >>= asXSTR) $ throwXData "DataType" $
          [("object", xdat), ("problem", mkXStr "does not contain a file path")]
        h <- lift $ DaoIO $ gets fileHandles >>= return . M.lookup path
        case h of
          Nothing -> throwXData "file.NotOpen" [("path", XSTR path)]
          Just  h -> fn (path, h)
  newModule "File"
    (do newOpenMethod "openRead"      ReadMode
        newOpenMethod "openReadWrite" ReadWriteMode
        newOpenMethod "openAppend"    AppendMode
        newHandleMethod "close" $ \ (path, h) -> do
          evalStackEmptyElse "close" []
          liftIO (hClose h)
          lift $ DaoIO $ modify (\st -> st{fileHandles = M.delete path (fileHandles st)})
          return XTRUE
        newHandleMethod "write" $ \ (_, h) -> printer h
        newHandleMethod "read"  $ \ (_, h) -> do
          evalStackEmptyElse "read" []
          liftIO (hGetLine h) >>= setResult . mkXStr
        newHandleMethod "readAll" $ \ (_, h) -> do
          evalStackEmptyElse "readAll" []
          liftIO (hGetContents h) >>= setResult . mkXStr
    )
    Nothing

----------------------------------------------------------------------------------------------------

-- | A class of data types that can be isomorphically translated to and from an 'XData' data type.
class Translatable t where
  buildXData :: (Functor m, Monad m, Applicative m) => BuilderT m t

newtype BuilderT m a = BuilderT { builderToPTrans :: PTrans XThrow (StateT DataBuilder m) a }
  deriving (Functor, Monad, MonadPlus)
instance (Functor m, Monad m) => Applicative (BuilderT m) where {pure=return; (<*>)=ap;}
instance (Functor m, Monad m) => Alternative (BuilderT m) where {empty=mzero; (<|>)=mplus;}
instance MonadTrans BuilderT where {lift = BuilderT . lift . lift}
instance Monad m => MonadError XThrow (BuilderT m) where
  throwError = BuilderT . throwError
  catchError (BuilderT try) catch = BuilderT (catchError try (builderToPTrans . catch))
instance Monad m => MonadPlusError XThrow (BuilderT m) where
  catchPValue (BuilderT f) = BuilderT (catchPValue f)
  assumePValue = BuilderT . assumePValue

-- | Run a 'BuilderT' monad.
runBuilderT :: BuilderT m t -> XData -> m (PValue XThrow t, DataBuilder)
runBuilderT (BuilderT fn) init = runStateT (runPTrans fn) $
  DataBuilder{builderItem=buildStep init, builderPath=[], builderModified=0}

data DataBuilder
  = DataBuilder
    { builderItem :: BuildStep
    , builderPath :: [BuildStep]
    , builderModified :: Integer
      -- ^ is the current item modified, must everything before it be rebuilt?
    }

incMod :: Integer -> DataBuilder -> DataBuilder
incMod i st = st{builderModified = builderModified st + i}

data BuildStep
  = DataConst XData
  | DataStep
    { buildDataAddress :: Address
    , buildDataDict    :: M.Map Label XData
    }
  | FieldStep
    { buildFieldIndex :: Label
    , buildFieldItem  :: Maybe XData
    }
  | ListStep
    { buildIndex      :: Int
    , buildListBefore :: [XData]
    , buildListAfter  :: [XData]
    }
  | FuncStep
    { buildFuncArgs  :: [Label]
    , buildFuncBlock :: XBlock
    }
  | BlockStep
    { buildIndex       :: Int
    , buildBlockBefore :: [XCommand]
    , buildBlockAfter  :: [XCommand]
    }

buildStep :: XData -> BuildStep
buildStep o = case o of
  XDATA addr dict ->
    DataStep{buildDataAddress=addr, buildDataDict=dict}
  XLIST list ->
    ListStep{buildIndex=0, buildListBefore=[], buildListAfter=maybe [] elems list}
  XFUNC (XFunc args block) -> FuncStep args block
  o -> DataConst o

newtype WithDataT m a = WithDataT { withDataToBuilderT :: BuilderT m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)
instance Monad m => MonadError XThrow (WithDataT m) where
  throwError = WithDataT . throwError
  catchError (WithDataT try) catch = WithDataT $ catchError try (withDataToBuilderT . catch)
instance Monad m => MonadPlusError XThrow (WithDataT m) where
  catchPValue (WithDataT f) = WithDataT (catchPValue f)
  assumePValue = WithDataT . assumePValue

-- | Force the current focus to become an 'XDATA' constructor. If the item is already an 'XDATA'
-- constructor, the fields will not be changed, otherwise a new field map is created.
putXData :: Monad m => String -> WithDataT m a -> BuilderT m a
putXData addr (WithDataT next) = do
  addr <- parseAddress addr
  BuilderT $ lift $ modify $ incMod 1 . \st -> case builderItem st of
    DataStep _ dict -> st{builderItem=DataStep addr dict}
    _ -> st{builderItem=DataStep{buildDataAddress=addr, buildDataDict=mempty}}
  next

putXDataFields :: Monad m => [(String, XData)] -> WithDataT m ()
putXDataFields items = WithDataT $ forM_ items $ \ (lbl, dat) -> do
  lbl <- parseLabel lbl
  BuilderT $ lift $ do
    st <- get
    let (DataStep{buildDataAddress=addr, buildDataDict=dict}) = builderItem st
    put $ incMod 1 $
      st{ builderItem =
            DataStep
            { buildDataAddress = addr
            , buildDataDict    = M.insert lbl dat dict
            }
        }

-- | Check if the current focus is an 'XDATA' constructor, if not evaluate to 'Control.Monad.mzero'.
-- If so, evaluate a function to read or write the fields of the object.
withXData :: Monad m => String -> WithDataT m t -> BuilderT m t
withXData addr (WithDataT (BuilderT next)) = BuilderT $ do
  addr <- parseAddress addr
  item <- lift (gets builderItem)
  case item of
    DataStep curAddr dict | curAddr==addr -> next
    _                                     -> mzero

-- | Evaluates 'withXData' using 'buildXData' converted to a 'WithDataT' data type.
xdata :: (Applicative m, Monad m, Translatable t) => String -> BuilderT m t
xdata addrStr = withXData addrStr $ mplus (WithDataT buildXData) $ do
  addr <- parseAddress addrStr
  throwXData "DataBuilder.Error" [("expecting", XPTR addr)]

newtype WithFieldT m a = WithFieldT { withFieldToBuilderT :: BuilderT m a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)
instance Monad m => MonadError XThrow (WithFieldT m) where
  throwError = WithFieldT . throwError
  catchError (WithFieldT try) catch = WithFieldT $ catchError try (withFieldToBuilderT . catch)
instance Monad m => MonadPlusError XThrow (WithFieldT m) where
  catchPValue (WithFieldT f) = WithFieldT (catchPValue f)
  assumePValue = WithFieldT . assumePValue
instance Monad m => MonadState (Maybe XData) (WithFieldT m) where
  state f = WithFieldT $ BuilderT $ do
    st <- lift get
    let doUpd lbl dat = lift $ put $ incMod 1 $
          st{ builderItem = FieldStep{buildFieldIndex=lbl, buildFieldItem=dat} }
    case builderItem st of
      FieldStep{buildFieldIndex=lbl, buildFieldItem=dat} -> case dat of
        Nothing -> case f Nothing of
          (a, Nothing ) -> return a
          (a, Just dat) -> doUpd lbl (Just dat) >> return a
        dat -> let (a, dat') = f dat in doUpd lbl dat' >> return a
      _ -> mzero

-- | Evaluates a 'WithFieldT' monad on a particular field identified by a 'Label' constructed from
-- the 'Prelude.String' parameter.
withXField :: Monad m => String -> WithFieldT m t -> WithDataT m t
withXField lbl (WithFieldT next) = WithDataT $ do
  lbl <- parseLabel lbl
  oldCount <- BuilderT $ do
    st <- lift get
    case builderItem st of
      DataStep{buildDataAddress=addr, buildDataDict=dict} -> lift $ put $
        st{ builderItem = FieldStep{buildFieldIndex=lbl, buildFieldItem=M.lookup lbl dict}
          , builderPath = builderItem st : builderPath st
          , builderModified = 0
          }
      _ -> mzero
    return (builderModified st)
  a <- next
  BuilderT $ do
    st <- lift get
    let count = builderModified st
    if count<=0
    then return a
    else do
      let path = builderPath st
      case builderItem st of
        FieldStep{buildFieldIndex=lbl, buildFieldItem=dat} -> do
          let (DataStep{buildDataAddress=addr, buildDataDict=dict}) = head path
          lift $ put $ incMod oldCount $
            st{ builderItem =
                  DataStep
                  { buildDataAddress = addr
                  , buildDataDict    = M.alter (const dat) lbl dict
                  }
              }
        _ -> return ()
      lift $ modify $ \st -> st{builderPath=tail path}
      return a

-- | Evaluates 'withXField' using 'buildXData' converted to a 'WithFieldT' data type.
xfield :: (Monad m, Applicative m, Translatable t) => String -> WithDataT m t
xfield lbl = withXField lbl $ WithFieldT $ BuilderT $ do
  mplus
    (do step     <- lift (gets builderItem)
        oldCount <- lift (gets builderModified)
        case buildFieldItem step of
          Just item -> do
            lift $ modify $ \st -> st{builderItem=buildStep item, builderModified=0}
            a <- builderToPTrans buildXData
            lift $ modify $ \st -> incMod oldCount $ st{builderItem=step}
            return a
          Nothing   -> mzero
    )
    (do lbl <- parseAddress lbl
        throwXData "DataBuilder.Error" [("expectingField", XPTR lbl)]
    )

-- | This module lets you construct arbitrary data types with a simple semantics similar to that of
-- the Bourne shell, that is using commands like @ls@, @cd@, @pwd@, @cp@, @mv@, @rm@, @cat@, @echo@,
-- @sed@, @grep@, @find@, @touch@, @type@, and more.
dataBuilder :: Monad m => DeclareRuntime m
dataBuilder =
  newModule "DataBuilder"
    (do return ()
    )
    Nothing

----------------------------------------------------------------------------------------------------

data XThrow
  = XError  { thrownXData :: XData }
  | XReturn { thrownXData :: XData }

-- | Pass an 'Address' as a 'Prelude.String' and a list of string-data pairs to be used to construct
-- a 'Data.Map.Lazy.Map' to be used for the 'XDATA' constructor. The string-data pairs will be
-- converted to a @('Data.Map.Lazy.Map' 'Label' 'XData')@ data structure which is how 'XDATA' types
-- store values. If the string constructing the 'Address' or any of the strings constructing
-- 'Label's for the 'Data.Map.Lazy.Map' cannot be parsed, a generic exception constructed from
-- 'XLIST' is thrown instead.
throwXData :: MonadError XThrow m => String -> [(String, XData)] -> m ig
throwXData addr itms = throwError $ XError $ mkXData addr itms

data LoadedModule m
  = PlainModule   { getXModule :: XModule }
  | BuiltinModule { getXModule :: XModule, getBuiltinEval :: Maybe (XEval -> RunT m XData) }
    -- ^ contains a directory of built-in modules paired with their XEval evaluators. An XEval
    -- evaluator is used when an object of a type matching this module is found in an 'XEval'
    -- operator like 'XADD' or 'XINDEX'. The data type in the 'XEval' is stored with the object
    -- and is used to select the pair
    -- > ('XModule', 'Prelude.Maybe' ('XEval' -> 'RunT' m 'XData'))
    -- If the 'Prelude.snd' item in the pair is not 'Prelude.Nothing', it is used to evaluate the
    -- 'XEval'.

data Runtime m
  = Runtime
    { evalCounter     :: Integer -- ^ counts how many evaluation steps have been taken
    , lastResult      :: XData
    , registers       :: M.Map Label XData
    , pcRegisters     :: M.Map Label Int
    , evalStack       :: [XData]
    , currentModule   :: Maybe XModule
    , currentBlock    :: Maybe (Array Int XCommand)
    , programCounter  :: Int
    , systemCalls     :: T.Tree Label (RunT m XData)
    , loadedModules   :: T.Tree Label (LoadedModule m)
    }

initRuntime :: Runtime m
initRuntime =
  Runtime
  { evalCounter    = 0
  , lastResult     = XNULL
  , registers      = mempty
  , pcRegisters    = mempty
  , evalStack      = []
  , currentModule  = Nothing
  , currentBlock   = Nothing
  , programCounter = 0
  , systemCalls    = T.Void
  , loadedModules  = T.Void
  }

-- | This is the monad used for evaluating the mini-Dao language. It is a
-- 'Control.Monad.Reader.MonadReader' where 'Control.Monad.Reader.ask' provides the an interface to
-- the 'Runtime' data structure, 'Control.Monad.State.state' provides an interface to update the
-- 'Runtime' data structure, 'Control.Monad.Trans.lift' allows you to lift the 'RunT' monad into
-- your own custom monad, and 'Control.Monad.IO.Class.liftIO' allows you to lift the 'RunT' monad
-- into your own custom @IO@ monad. The 'Control.Monad.Error.throwError' interface is provided which
-- lets you safely (without having to catch exceptions in the IO monad) throw an exception of type
-- 'XData', Lifting 'RunT' into the 'Control.Monad.Trans.Identity' monad is will allow all
-- evaluation to be converted to a pure function, however it will make it difficult to provide
-- system calls that do things like communicate with threads or read or write files.
newtype RunT m a = RunT{ runToPTrans :: PTrans XThrow (StateT (Runtime m) m) a }

evalRun :: Monad m => Runtime m -> RunT m a -> m (PValue XThrow a, Runtime m)
evalRun st (RunT f) = runStateT (runPTrans f) st

instance Functor m => Functor (RunT m) where { fmap f (RunT m) = RunT (fmap f m) }
instance Monad m => Monad (RunT m) where
  return = RunT . return
  RunT a >>= f = RunT $ a >>= runToPTrans . f
  RunT a >> RunT b = RunT (a >> b)
  fail = RunT . pvalue . PFail . XError . mkXStr
instance Monad m => MonadPlus (RunT m) where
  mzero = RunT mzero
  mplus (RunT a) (RunT b) = RunT (mplus a b)
instance (Functor m, Monad     m) => Applicative (RunT m) where { pure = return; (<*>) = ap; }
instance (Functor m, MonadPlus m) => Alternative (RunT m) where { empty = mzero; (<|>) = mplus; }
instance Monad m => MonadError XThrow (RunT m) where
  throwError = RunT . throwError
  catchError (RunT try) catch = RunT (catchError try (runToPTrans . catch))
instance MonadIO m => MonadIO (RunT m) where { liftIO = RunT . liftIO }
instance MonadTrans RunT where { lift = RunT . lift . lift }
instance Monad m => MonadState  (Runtime m) (RunT m) where { state = RunT . lift . state }
instance (MonadPlus m, Monad m) => MonadPlusError XThrow (RunT m) where
  catchPValue (RunT f) = RunT (catchPValue f)
  assumePValue p = RunT (assumePValue p)

----------------------------------------------------------------------------------------------------

readLabel :: String -> [(String, String)]
readLabel str = case dropWhile isSpace str of
  c:_ | isAlpha c || c=='_' -> return (span isAlphaNum str)
  [] -> fail "expecting Label"

parseLabel :: MonadError XThrow m => String -> m Label
parseLabel str = case readsPrec 0 str of
  [(addr, "")] -> return addr
  _ -> throwXData "Label.Error" $
    [ ("problem", mkXStr "Labels must have no dots, commas, parens, spaces, and must not be null")
    , ("parseInput", mkXStr str)
    ]

readAddress :: String -> [(String, String)]
readAddress = readLabel >=> uncurry loop where 
  loop nm str = case dropWhile isSpace str of
    '.':str -> do
      (lbl, str) <- readLabel str
      mplus (loop (nm++'.':lbl) str) (fail "expecting Address")
    str     -> return (nm, str)

parseAddress :: MonadError XThrow m => String -> m Address
parseAddress str = case readsPrec 0 str of
  [(addr, "")] -> return addr
  _ -> throwXData "Address.Error" $
    [ ("problem", mkXStr "address must have no commas, parens, spaces, and must not be null")
    , ("parseInput", mkXStr str)
    ]

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
  | XFUNC XFunc
  deriving (Eq, Ord)
instance RWX RWData XData where
  io_x io = case io of
    NULL     -> XNULL
    TRUE     -> XTRUE
    INT   io -> XINT       io
    FLOAT io -> XFLOAT     io
    STR   io -> XSTR       io
    PTR   io -> XPTR       io
    LIST  io -> mkXList (map io_x io)
    DATA a b -> XDATA a (M.fromList $ map ((fmap io_x) . defToPair) b)
    FUNC a b -> XFUNC (XFunc{funcArgVars=a, funcBlock=io_x b})
  x_io x  = case x  of
    XNULL     -> NULL
    XTRUE     -> TRUE
    XINT    x -> INT        x
    XFLOAT  x -> FLOAT      x
    XSTR    x -> STR        x
    XPTR    x -> PTR        x
    XLIST   x -> LIST $ maybe [] (map x_io . elems) x
    XDATA a b -> DATA a (map (uncurry DEFINE . fmap x_io) (M.assocs b))
    XFUNC   x -> FUNC (funcArgVars x) (x_io $ funcBlock x)

data XFunc = XFunc { funcArgVars :: [Label], funcBlock :: XBlock } deriving (Eq, Ord)

mkXStr :: String -> XData
mkXStr = XSTR . text

mkXArray :: [a] -> Maybe (Array Int a)
mkXArray o = if null o then Nothing else Just $ listArray (0, length o - 1) o

mkXList :: [XData] -> XData
mkXList = XLIST . mkXArray

mkXData :: String -> [(String, XData)] -> XData
mkXData addr itms = do
  let mkDefault = mkXList $ concat $
        [[mkXStr addr], flip concatMap itms $ (\ (lbl, dat) -> [mkXStr lbl, dat])]
  maybe mkDefault id $ do
    addr <- case readsPrec 0 addr of
      [(addr, "")] -> return addr
      _            -> mzero
    itms <- forM itms $ \ (lbl, dat) -> case readsPrec 0 lbl of
      [(lbl, "")] -> return (lbl, dat)
      _           -> mzero
    return $ XDATA addr $ M.fromList itms

asXBool :: MonadPlus m => XData -> m Bool
asXBool  o = case o of {XTRUE -> return True; XNULL -> return False; _ -> mzero;}

asXINT :: MonadPlus m => XData -> m Int
asXINT   o = case o of {XINT o -> return o; _ -> mzero;}

asXFLOAT :: MonadPlus m => XData -> m Double
asXFLOAT o = case o of {XFLOAT o -> return o; _ -> mzero;}

asXSTR :: MonadPlus m => XData -> m Text
asXSTR   o = case o of {XSTR o -> return o; _ -> mzero;}

asXPTR :: MonadPlus m => XData -> m Address
asXPTR   o = case o of {XPTR o -> return o; _ -> mzero;}

asXLIST :: MonadPlus m => XData -> m [XData]
asXLIST  o = case o of {XLIST o -> return (maybe [] elems o); _ -> mzero;}

asXFUNC :: MonadPlus m => XData -> m XFunc
asXFUNC  o = case o of {XFUNC o -> return o; _ -> mzero; }

-- | Get an 'XData' type constructed with 'XDATA' of a specific 'Address' where the 'Address'
-- represents the mini-Dao language type of the 'XDATA'. The 'Address' is passed here as a
-- 'Prelude.String'. Returns the 'Data.Map.Lazy.Map' of elements that construct the mini-Dao
-- language data type.
asXDATA :: MonadPlus m => String -> XData -> m (M.Map Label XData)
asXDATA str o = case readsPrec 0 str of
  [(getAddr, "")] -> case o of
    XDATA addr o | addr==getAddr -> return o
    _  -> mzero
  _  -> fail $ "asXDATA: could not parse Address from string "++show str

-- | Get an 'XData' type constructed with 'XDATA', and from within the 'XDATA' lookup a 'Label' in
-- the 'Data.Map.Lazy.Map' of member objects. The 'Label' is passed here as a 'Prelude.String'.
tryXMember :: MonadPlus m => String -> XData -> m XData
tryXMember str o = case readsPrec 0 str of
  [(lbl, "")] -> case o of
    XDATA _ o -> maybe mzero return (M.lookup lbl o)
    _ -> mzero
  _ -> fail $ "tryXMember: could not parse Label from string "++show str

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
  | UPDATE  RWLookup Label -- ^ copy a value into a local variable of the current module.
  | DELETE  Label
  | SETJUMP Label
  | JUMP    Label
  | PUSH    RWLookup
  | PEEK
  | POP
  | CLRFWD  -- ^ clear the stack into a LIST object in the RESULT register
  | CLRREV  -- ^ like 'CLRFWD' but reveres the lits of items.
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
  | XUPDATE  XLookup Label -- ^ copy a value into a local variable of the current module.
  | XSETJUMP Label -- ^ set a jump point
  | XJUMP    Label -- ^ goto a jump point
  | XPUSH    XLookup -- ^ push a value onto the stack
  | XPEEK    -- ^ copy the top item off of the stack
  | XPOP     -- ^ remove the top item off of the stack
  | XCLRFWD
  | XCLRREV
  | XEVAL    XEval -- ^ evaluate a lisp-like expression
  | XDO      XCondition  -- ^ branch conditional
  | XRETURN  XLookup     -- ^ end evaluation
  | XTHROW   XLookup
  deriving (Eq, Ord)
instance RWX RWCommand XCommand where
  io_x io = case io of
    LOAD    a   -> XLOAD          a
    STORE   a   -> XSTORE         a
    UPDATE  a b -> XUPDATE  (io_x a) b
    SETJUMP a   -> XSETJUMP       a
    JUMP    a   -> XJUMP          a
    PUSH    a   -> XPUSH    (io_x a)
    PEEK        -> XPEEK
    POP         -> XPOP
    CLRFWD      -> XCLRFWD
    CLRREV      -> XCLRREV
    EVAL    a   -> XEVAL    (io_x a)
    DO      a   -> XDO      (io_x a)
    RETURN  a   -> XRETURN  (io_x a)
    THROW   a   -> XTHROW   (io_x a)
  x_io x  = case x of
    XLOAD    a   -> LOAD          a
    XSTORE   a   -> STORE         a
    XUPDATE  a b -> UPDATE  (x_io a) b
    XSETJUMP a   -> SETJUMP       a
    XJUMP    a   -> JUMP          a
    XPUSH    a   -> PUSH    (x_io a)
    XPEEK        -> PEEK
    XPOP         -> POP
    XCLRFWD      -> CLRFWD
    XCLRREV      -> CLRREV
    XEVAL    a   -> EVAL    (x_io a)
    XDO      a   -> DO      (x_io a)
    XRETURN  a   -> RETURN  (x_io a)
    XTHROW   a   -> THROW   (x_io a)

newtype XBlock = XBlock { toCommands :: Maybe (Array Int XCommand) } deriving (Eq, Ord)
instance RWX [RWCommand] XBlock where
  io_x io = XBlock (mkXArray $ map io_x io)
  x_io (XBlock x) = maybe [] (map x_io . elems) x

data RWCondition
  = WHEN      RWLookup RWCommand
  | UNLESS   RWLookup RWCommand
    -- ^ iterate over a 'LIST' or 'DATA' value. If it is a data value to be iterated, the first to
    -- registers provide where to store the data header and the key of each iteration. The result
    -- register always stored the value to be scrutinized.
  deriving (Eq, Ord, Show, Read)
data XCondition
  = XWHEN   XLookup XCommand
  | XUNLESS XLookup XCommand
  deriving (Eq, Ord)
instance RWX RWCondition XCondition where
  io_x io = case io of
    WHEN   a b -> XWHEN          (io_x a) (io_x b)
    UNLESS a b -> XUNLESS       (io_x a) (io_x b)
  x_io io = case io of
    XWHEN   a b -> WHEN          (x_io a) (x_io b)
    XUNLESS a b -> UNLESS       (x_io a) (x_io b)

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
  | IF     RWEval RWEval RWEval
  | IFNOT  RWEval RWEval RWEval
  | SYS    Address  [RWEval] -- ^ system call
  | CALL   Address  RWLookup [RWEval]
  | LOCAL  RWLookup [RWEval] -- ^ call a local function, push the result onto the stack
  | GOTO   RWLookup [RWEval] -- ^ goto a local function, never return. Good for tail recursion.
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
  | XIF     XEval XEval XEval
  | XIFNOT  XEval XEval XEval
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
    IF     a b c -> XIF    (io_x a) (io_x b) (io_x c)
    IFNOT  a b c -> XIFNOT (io_x a) (io_x b) (io_x c)
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
    XIF     a b c -> IF    (x_io a) (x_io b) (x_io c)
    XIFNOT  a b c -> IFNOT (x_io a) (x_io b) (x_io c)
    XSYS    a b   -> SYS  a   (map x_io b)
    XCALL   a b c -> CALL a (x_io b) (map x_io c)
    XLOCAL  a b   -> LOCAL  (x_io a) (map x_io b)
    XGOTO   a b   -> GOTO   (x_io a) (map x_io b)

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
  ([(clas, "")], [(msg, "")]) -> throwXData clas [(msg, dat)]
  _ -> fail $ concat [clas, ": ", msg, "\n\t", show (x_io dat)]

lookupModule :: Monad m => Address -> RunT m XModule
lookupModule addr = gets loadedModules >>= \mods ->
  maybe (evalError "Undefined" "moduleName" (XPTR addr)) return $
    let lbls = addrToLabels addr in fmap getXModule $ T.lookup lbls mods

instance Evaluable XLookup where
  eval o = case o of
    XRESULT     -> gets lastResult
    XCONST   o  -> return o
    XVAR     o  -> gets registers >>= \reg -> case M.lookup o reg of
      Nothing -> evalError "Undefined" "variableName" (mkXStr $ show o)
      Just  o -> return o
    XDEREF   o  -> do
      mod <- gets currentModule
      case msum $ map (fmap defsToMap >=> M.lookup o) [fmap xprivate mod, fmap xpublic mod] of
        Nothing -> evalError "Undefined" "moduleVariable" (mkXStr $ show o)
        Just  o -> return o
    XLOOKUP a b -> do
      mod <- lookupModule a
      case M.lookup b (defsToMap $ xpublic mod) of
        Nothing -> throwXData "Undefined" $
          [(read "inModule", XPTR a), (read "variableName", mkXStr $ show b)]
        Just  o -> return o

-- Increment evaluation counter
incEC :: Monad m => RunT m ()
incEC = modify (\st -> st{evalCounter=evalCounter st + 1})

-- Increment program counter
incPC :: Monad m => RunT m ()
incPC = modify (\st -> st{programCounter=programCounter st + 1})

popEvalStack :: Monad m => RunT m XData
popEvalStack = get >>= \st -> case evalStack st of
  []   -> throwXData "StackUnderflow" []
  a:ax -> put (st{evalStack=ax}) >> setResult a

pushEvalStack :: Monad m => [XData] -> RunT m ()
pushEvalStack dx = get >>= \st -> modify (\st -> st{evalStack=reverse dx++evalStack st})

evalStackEmptyElse :: Monad m => String -> [(String, XData)] -> RunT m ()
evalStackEmptyElse functionLabel params = gets evalStack >>= \stk ->
  if null stk
    then  return ()
    else  throwXData "FuncArgs" $
            [ ("problem", mkXStr "too many arguments to function")
            , ("function", mkXStr functionLabel)
            , ("extraneousArguments", mkXList stk)
            ]

instance Evaluable XCommand where
  eval o = incEC >> incPC >> case o of
    XLOAD    a   -> do
      reg <- gets registers
      case M.lookup a reg of
        Nothing -> evalError "Undefined" "variableName" (mkXStr $ show a)
        Just  a -> setResult a
    XSTORE   a   -> gets lastResult >>= \b -> setRegister a b >> return b
    XUPDATE  a b -> gets currentModule >>= \mod -> case mod of
      Nothing  -> throwXData "Undefined" $
        [ ("problem", mkXStr "update occurred with no current module")
        , ("instruction", mkXStr (show (x_io o)))
        ]
      Just mod -> case M.lookup b $ defsToMap $ xprivate mod of
        Nothing  -> throwXData "Undefined" [("variableName", mkXStr (show (x_io a)))]
        Just var -> do
          a <- eval a
          modify $ \st ->
            st{ lastResult    = var
              , currentModule = Just $
                  mod
                  { xprivate =
                      XDefines{ defsToMap = M.insert b a (defsToMap (xprivate mod)) }
                  }
              }
          gets lastResult
    XSETJUMP a   -> gets lastResult
    XJUMP    a   -> do
      pcreg <- gets pcRegisters
      let badJump = evalError "Undefined" "jumpTo" (mkXStr $ show a)
      case M.lookup a pcreg of
        Nothing -> badJump
        Just  i -> get >>= \st -> case currentBlock st of
          Nothing -> badJump
          Just  b ->
            if inRange (bounds b) i then put (st{programCounter=i}) >> return XNULL else badJump
    XPUSH    a   -> eval a >>= \b -> modify (\st -> st{evalStack = b : evalStack st}) >> return b
    XPEEK        -> gets evalStack >>= \ax -> case ax of
      []  -> throwXData "StackUnderflow" []
      a:_ -> setResult a
    XPOP         -> popEvalStack
    XCLRFWD      -> do
      modify (\st -> st{lastResult=mkXList (evalStack st), evalStack=[]})
      gets lastResult
    XCLRREV      -> do
      modify (\st -> st{lastResult=mkXList (reverse $ evalStack st), evalStack=[]})
      gets lastResult
    XEVAL    a   -> eval a
    XDO      a   -> eval a
    XRETURN  a   -> eval a >>= \a -> throwError (XReturn a)
    XTHROW   a   -> eval a >>= \a -> throwError (XError  a)

instance Evaluable XCondition where
  eval o = incEC >> case o of
    XWHEN a b -> eval a >>= \a -> case a of
      XNULL     -> return XNULL
      _         -> eval b
    XUNLESS a b -> eval a >>= \a -> case a of
      XNULL       -> eval b
      _           -> return XNULL

instance Evaluable XBlock where
  eval (XBlock cmds) = fix $ \loop -> incEC >> do
    pc <- gets programCounter
    block <- gets currentBlock
    case block of
      Just block | inRange (bounds block) pc -> eval (block!pc) >> loop
      _                                      -> gets lastResult

badEval :: Monad m => XEval -> [(String, XData)] -> RunT m ig
badEval o other = throwXData "BadInstruction" $
  [ ("instruction", mkXStr (show $ x_io o))
  , ("problem", mkXStr "evaluated operator on incorrect type of data")
  ] ++ other

evalDataOperand :: Monad m => Address -> XEval -> RunT m XData
evalDataOperand addr o = do
  -- avoids use of 'fmap' or '(<$>)' so we are not forced to put Functor in the context
  tabl <- gets loadedModules >>= return . join . fmap getBuiltinEval . T.lookup (addrToLabels addr)
  case tabl of
    Nothing   -> badEval o [("dataType", XPTR addr)]
    Just eval -> eval o

-- Automatically reverse and clear the stack, append the given parameters, and pair them with the
-- 'Label' parameters, placing the remaining unpaired items back onto the stack and setting the
-- registers by the pairs. Throw an error if there are not enough arguments to pair with the 'Label'
-- parameters.
matchArgs :: Monad m => [Label] -> [XData] -> RunT m ()
matchArgs argVars argValues = do
  stk <- gets evalStack
  let loop m ax bx = case (ax, bx) of
        (ax  , []  ) -> Just (m, [])
        ([]  , _   ) -> Nothing
        (a:ax, b:bx) -> loop ((a,b):m) ax bx
  case loop [] argVars (reverse stk ++ argValues) of
    Nothing -> evalError "FuncCall" "notEnoughParams" (mkXList argValues)
    Just (elms, rem) -> get >>= \old ->
      modify $ \st -> st{registers=M.fromList elms, evalStack=reverse rem}

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
    XFUNC (XFunc argVars (XBlock (Just block))) -> do
      let loop m ax bx = case (ax, bx) of
            (ax  , []  ) -> Just (m, [])
            ([]  , _   ) -> Nothing
            (a:ax, b:bx) -> loop ((a,b):m) ax bx
      case loop [] argVars argValues of
        Nothing -> evalError "FuncCall" "notEnoughParams" (mkXList argValues)
        Just (elms, rem) -> get >>= \old -> do
          matchArgs argVars argValues
          setupJumpRegisters block
          modify $ \st ->
            st{ currentBlock   = Just block
              , programCounter = programCounter old
              }
          catchError (eval (XBlock $ Just block)) $ \err -> case err of
            XError  err -> throwError (XError err)
            XReturn dat -> do
              modify $ \st ->
                st{ registers      = registers      old
                  , pcRegisters    = pcRegisters    old
                  , evalStack      = evalStack      old
                  , currentBlock   = currentBlock   old
                  , programCounter = programCounter old
                  }
              return dat
    XFUNC (XFunc _ (XBlock Nothing)) -> gets lastResult
    _ -> throwXData "FuncCall" $
            [ (read "problem", mkXStr "target of call is not executable data")
            , (read "targetRegister", mkXStr $ show $ x_io lbl)
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

evalCondition :: Monad m => XEval -> Bool -> XEval -> XEval -> XEval -> RunT m XData
evalCondition o inv a b c = do
  a <- mplus (eval a >>= asXBool) $
    badEval o [("problem", mkXStr "conditional does not evaluate to boolean value")]
  if not inv && a || inv && not a then eval b else eval c

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
      (XLIST a      , XLIST b      ) -> let el = maybe [] elems in return (mkXList (el a ++ el b))
    X_AND   a b -> bitwise o (.&.) a b
    X_OR    a b -> bitwise o (.|.) a b
    X_XOR   a b -> bitwise o  xor  a b
    XSHIFTR a b -> bitwise o shift a b
    XSHIFTL a b -> bitwise o (\a b -> shift a (negate b)) a b
    XIF     a b c -> evalCondition o True  a b c
    XIFNOT  a b c -> evalCondition o False a b c
    XSYS    a b   -> do
      syscall <- gets systemCalls >>= return . T.lookup (addrToLabels a)
      case syscall of
        Nothing -> evalError "Undefined" "systemCall" (XPTR a)
        Just fn -> do
          oldstk <- gets evalStack
          b      <- mapM eval b
          modify (\st -> st{evalStack=reverse b})
          dat <- catchError fn $ \err -> case err of
            XReturn dat -> return dat
            XError  err -> throwXData "SystemCall" [("exception", err)]
          modify (\st -> st{evalStack=oldstk})
          setResult dat
    XCALL   a b c -> do
      this   <- gets lastResult
      mod    <- lookupModule a
      curmod <- gets currentModule
      c      <- mapM eval c
      modify (\st -> st{currentModule=Just mod, lastResult=this})
      catchError (callLocalMethod b c) $ \err -> case err of
        XReturn dat -> modify (\st -> st{currentModule=curmod}) >> return dat
        XError  err -> throwError (XError err)
    XLOCAL  a b   -> mapM eval b >>= callLocalMethod a
    XGOTO   a b   -> eval a >>= \a -> case a of
      XFUNC (XFunc argParams (XBlock (Just block))) -> do
        argValues <- mapM eval b
        modify $ \st ->
          st{ registers      = mempty
            , evalStack      = []
            , programCounter = 0
            , currentBlock   = Just block
            }
        matchArgs argParams argValues
        setupJumpRegisters block
        gets lastResult
      XFUNC (XFunc _ (XBlock Nothing)) -> gets lastResult
      _ -> badEval o [(read "register", mkXStr "target of GOTO is non-function data type")]

----------------------------------------------------------------------------------------------------

-- | Matches the head of the input text list to each rule in the given module. Matching rules will
-- strip the matched portion of the input text list, the remainder is placed onto the stack such
-- that successive 'POP' operations retrieve each item in the remaining input text from head to
-- tail.
evalRulesInMod :: Monad m => [Text] -> XModule -> RunT m ()
evalRulesInMod qs mod = do
  modify (\st -> st{currentModule=Just mod})
  forM_ (xrules mod) $ \ (XRULE pat block) -> case stripPrefix pat qs of
    Nothing -> return ()
    Just qs -> do
      oldstk <- gets evalStack
      modify (\st -> st{evalStack=map XSTR qs})
      eval block
      modify (\st -> st{evalStack=oldstk})

