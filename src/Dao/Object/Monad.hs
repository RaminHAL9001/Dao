-- "src/Dao/Object/Monad.hs"  defines the monad that is used to evaluate
-- expressions written in the Dao scripting lanugage.
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


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dao.Object.Monad where

import           Dao.String
import           Dao.Debug
import           Dao.Object

import           Control.Exception
import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Control.Monad.State.Lazy
import           Control.Monad.Reader


----------------------------------------------------------------------------------------------------

-- | Used to play the role of an error-handling monad and a continuation monad together. It is
-- basically an identity monad, but can evaluate to 'CEError's instead of relying on
-- 'Control.Exception.throwIO' or 'Prelude.error', and can also work like a continuation by
-- evaluating to 'CEReturn' which signals the execution function finish evaluation immediately. The
-- "Control.Monad" 'Control.Monad.return' function evaluates to 'CENext', which is the identity
-- monad simply returning a value to be passed to the next monad. 'CEError' and 'CEReturn' must
-- contain a value of type 'Dao.Types.Object'.
data ContErr a
  = CENext   a
  | CEError  Object
  | CEReturn Object
  deriving Show

instance Monad ContErr where
  return = CENext
  ma >>= mfa = case ma of
    CENext   a -> mfa a
    CEError  a -> CEError a
    CEReturn a -> CEReturn a

instance Functor ContErr where
  fmap fn mfn = case mfn of
    CENext   a -> CENext (fn a)
    CEError  a -> CEError a
    CEReturn a -> CEReturn a

instance MonadPlus ContErr where
  mzero = CEError ONull
  mplus (CEError _) b = b
  mplus a           _ = a

newtype ContErrT m a = ContErrT { runContErrT :: m (ContErr a) }

instance Monad m => Monad (ContErrT m) where
  return = ContErrT . return . CENext
  (ContErrT ma) >>= mfa = ContErrT $ do
    a <- ma
    case a of
      CENext   a -> runContErrT (mfa a)
      CEError  a -> return (CEError  a)
      CEReturn a -> return (CEReturn a)

instance Monad m => Functor (ContErrT m) where
  fmap fn mfn = mfn >>= return . fn

instance Monad m => MonadPlus (ContErrT m) where
  mzero = ContErrT (return mzero)
  mplus (ContErrT fa) (ContErrT fb) = ContErrT $ do
    a <- fa
    case a of
      CENext   a -> return (CENext   a)
      CEError  _ -> fb
      CEReturn a -> return (CEReturn a)

instance MonadTrans ContErrT where
  lift ma = ContErrT (ma >>= return . CENext)

-- | Force the computation to assume the value of a given 'Dao.Object.Monad.ContErr'. This function
-- can be used to re-throw a 'Dao.Object.Monad.ContErr' value captured by the 'withContErrSt'
-- function.
returnContErr :: Monad m => ContErr a -> ContErrT m a
returnContErr ce = ContErrT (return ce)

ceReturn :: Monad m => Object -> ContErrT m a
ceReturn a = returnContErr (CEReturn a)

ceError :: Monad m => Object -> ContErrT m a
ceError  a = returnContErr (CEError a)

-- | Evaluate a 'ContErrT' function and capture the resultant 'Dao.Object.Monad.ContErr' value,
-- then apply some transformation to that 'ContErr value. For example, you can decide whether or not
-- to evaluate 'Dao.Object.Monad.ceError', 'Dao.Object.Monad.ceReturn', or ordinary
-- 'Control.Monad.return', or just ignore the 'Dao.Object.Monad.ContErr' value entirely.
withContErrSt :: Monad m => CEReader r m a -> (ContErr a -> CEReader r m b) -> CEReader r m b
withContErrSt exe fn = ContErrT $ ReaderT $ \r -> do
  b <- runReaderT (runContErrT exe) r
  runReaderT (runContErrT (fn b)) r

ceMonad :: Monad m => (Object -> ContErrT m a) -> (Object -> ContErrT m a) -> ContErrT m a -> ContErrT m a
ceMonad onReturn onError exe = lift (runContErrT exe) >>= \ce -> case ce of
  CEReturn a -> onReturn a
  CEError  a -> onError  a
  CENext   a -> return   a

-- | Return statements are break the continuation completely, and NO computation will occur after a
-- return statement. So, in a function that calls two subroutines in succession, calling the first
-- subroutine will evaluate to a CEReturn, and when this value is received by the calling function,
-- the calling function will not evaluate the next subroutine call, it will simply assume the
-- CEReturn value from the first subroutine. However this is not the behavior we expect when calling
-- two subroutines in a row, we expect both to be called, and to explicitly decalre the return value
-- after both calls have completed. To achieve this expected behavior, you need to "catch" the
-- CEReturn value and convert it to a CENext value, while leaving CEError values alone.
catchCEReturn :: Monad m => ContErrT m Object -> ContErrT m Object
catchCEReturn exe = ceMonad return (returnContErr . CEError) exe

-- | When an error is thrown, the continuation is collapsed all the way back to the point where this
-- function was used to evaluate the continuation monad.
catchCEError :: Monad m => ContErrT m Object -> ContErrT m Object
catchCEError exe = ceMonad (returnContErr . CEReturn) return exe

-- | Execute a final operation regardless of the resulting 'ContErr' value.
-- ceFinal :: Monad m => ContErrT m a -> ContErrT m ignored -> ContErrT m a
-- ceFinal exe final = ceMonad (\o -> final >> ceReturn o) (\o -> final >> ceError o) exe

----------------------------------------------------------------------------------------------------

-- $MonadReader
-- Since 'ContErrT' will be used often with a lifted 'Control.Monad.Reader.ReaderT', useful
-- combinators for manipulating the reader value are provided here.

type CEReader r m a = ContErrT (ReaderT r m) a

-- | It is useful to lift 'Control.Monad.Reader.ReaderT' into 'ContErrT' and instantiate the
-- combined monad into the 'Control.Monad.Reader.Class.MonadReader' class.
instance Monad m => MonadReader r (ContErrT (ReaderT r m)) where
  ask = ContErrT (ReaderT (\r -> return (CENext r)))
  local fn next = ContErrT (ReaderT (\r -> runReaderT (runContErrT next) (fn r)))

-- Like 'Control.Monad.Reader.local', but the local function is in the @m@ monad.
localCE :: Monad m => (r -> m r) -> CEReader r m a -> CEReader r m a
localCE fn next = ContErrT (ReaderT (\r -> fn r >>= runReaderT (runContErrT next)))

-- | Execute an IO operation inside of the 'CEReader' monad, assuming IO is lifted into the
-- CEReader.
execIO :: IO a -> CEReader r IO a
execIO fn = lift (lift fn)

execRun :: ReaderT r IO a -> CEReader r IO a
execRun run = ContErrT (fmap CENext run)

-- | Catch an exception from "Control.Exception" in the 'CEReader' monad. Uses
-- 'Control.Exception.hander', which is an IO function, so the CEReader must have the IO monad
-- lifted into it.
catchCE :: Exception err => CEReader r IO a -> (err -> CEReader r IO a) -> CEReader r IO a
catchCE exe ifError = ContErrT $ ReaderT $ \r ->
  handle (\e -> runReaderT (runContErrT (ifError e)) r) (runReaderT (runContErrT exe) r)

