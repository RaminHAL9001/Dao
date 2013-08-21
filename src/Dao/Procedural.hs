-- "src/Dao/Procedural.hs"  defines a monadic computations with
-- constructors for behaving as a procedural language with
-- "return" and "throw" statements.
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Procedural where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Trans

-- | Used to play the role of an error-handling monad and a continuation monad together. It is
-- basically an identity monad, but can evaluate to 'FlowErr's instead of relying on
-- 'Control.Exception.throwIO' or 'Prelude.error', and can also work like a continuation by
-- evaluating to 'FlowReturn' which signals the execution function finish evaluation immediately. The
-- "Control.Monad" 'Control.Monad.return' function evaluates to 'FlowOK', which is the identity
-- monad simply returning a value to be passed to the next monad.
data FlowCtrl err ret a
  = FlowOK     a
  | FlowErr    err
  | FlowReturn ret
instance Monad (FlowCtrl e r) where
  return = FlowOK
  ma >>= mfa = case ma of
    FlowOK     a -> mfa a
    FlowErr    a -> FlowErr a
    FlowReturn a -> FlowReturn a
instance Functor (FlowCtrl e r) where
  fmap fn mfn = case mfn of
    FlowOK     a -> FlowOK (fn a)
    FlowErr    a -> FlowErr a
    FlowReturn a -> FlowReturn a
instance MonadError e (FlowCtrl e r) where
  throwError = FlowErr
  catchError ce catch = case ce of
    FlowOK     ce  -> FlowOK     ce
    FlowReturn obj -> FlowReturn obj
    FlowErr    obj -> catch      obj

-- | Since the Dao language is a procedural language, there must exist a monad that mimics the
-- behavior of a procedural program. A 'Procedural' monad is a monad transformer, so you can lift
-- your own monad into it. Procedural itself lifts 'FlowCtrl' so that it may throw errors and return
-- from any point. Thus, 'Control.Monad.Monad' is extended with 'FlowCtrl'.
newtype Procedural err ret m a = Procedural { runProcedural :: m (FlowCtrl err ret a) }
instance Functor m => Functor (Procedural err ret m) where
  fmap fn (Procedural m) = Procedural (fmap (fmap fn) m)
instance Monad m => Monad (Procedural err ret m) where
  return = Procedural . return . FlowOK
  (Procedural ma) >>= mfa = Procedural $ do
    a <- ma
    case a of
      FlowOK   a -> runProcedural (mfa a)
      FlowErr  a -> return (FlowErr  a)
      FlowReturn a -> return (FlowReturn a)
instance MonadTrans (Procedural err ret) where
  lift ma = Procedural (ma >>= return . FlowOK)
instance MonadIO m => MonadIO (Procedural err ret m) where
  liftIO ma = Procedural (liftIO ma >>= return . FlowOK)
instance Monad m => MonadReader reader (Procedural err ret (ReaderT reader m)) where
  local upd mfn = Procedural (local upd (runProcedural mfn))
  ask = Procedural (ask >>= return . FlowOK)
instance Monad m => MonadError e (Procedural e r m) where
  throwError = Procedural . return . FlowErr
  catchError mce catch = Procedural $ runProcedural mce >>= \ce -> case ce of
    FlowOK   a   -> return (FlowOK a)
    FlowReturn obj -> return (FlowReturn obj)
    FlowErr  obj -> runProcedural (catch obj)

-- | Force the computation to assume the value of a given 'FlowCtrl'.
joinFlowCtrl :: Monad m => FlowCtrl err ret a -> Procedural err ret m a
joinFlowCtrl ce = Procedural (return ce)

-- | The inverse operation of 'procJoin', catches the inner 'FlowCtrl' of the given 'Procedural'
-- evaluation, regardless of whether or not this function evaluates to 'procReturn' or 'procErr',
-- whereas ordinarily, if the inner 'FlowCtrl' is 'FlowErr' or 'FlowReturn'.
procCatch :: Monad m => Procedural err ret m a -> Procedural e r m (FlowCtrl err ret a)
procCatch fn = Procedural (runProcedural fn >>= \ce -> return (FlowOK ce))

-- | The inverse operation of 'procCatch', this function evaluates to a 'Procedural' behaving according
-- to the 'FlowCtrl' evaluated from the given function.
procJoin :: Monad m => Procedural err ret m (FlowCtrl err ret a) -> Procedural err ret m a
procJoin mfn = mfn >>= \a -> Procedural (return a)

