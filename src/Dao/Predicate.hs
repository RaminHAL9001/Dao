-- "src/Dao/Predicate.hs"  provides 'PredicateT' which combines the
-- Maybe and Either types into a single monad.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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
{-# LANGUAGE FunctionalDependencies #-}

-- | Provides a monad that essentially combines the monadic functionality of 'Prelude.Maybe' and
-- 'Prelude.Either' into a single monad 'Predicate. Both the 'Prelude.Maybe' and 'Prelude.Either'
-- data types are both monads but it is convenient to have a single data type combining the two.
-- 'PFail' is analogous to @'Prelude.Left'@, 'Backtrack' is analogous to
-- @'Prelude.Right' $ 'Prelude.Nothing'@, and 'OK' is analogous to
-- @'Prelude.Right' . 'Prelude.Just'@. All relevant monad transformers are instnatiated, including
-- 'Control.Applicative.Applicative', 'Control.Applicative.Alternative', 'Control.Monad.MonadPlus',
-- and 'Control.Monad.Error.MonadError'. 
--
-- A new monad transformer 'PredicateT' is also introduced which lifts the 'Predicate' monad into
-- another monad and wraps it into the 'PredicateT' data type which instantiates the
-- 'Control.Monad.Trans.MonadTrans' class. Further, a new class 'MonadPlusError' is defined which
-- allows you to directly manipulate the 'Predicate' value of a 'PredicateT' transformer.
-- 
-- Here is a simple example of how to use this module.
-- > newtype MyErr = MyErr String
-- > newtype MyIO a = WrapMyIO { unwrapMyIO :: 'PredicateT' MyErr IO a }
-- >         deriving (Functor, Applicative, Alternative)
-- >
-- > instance 'Control.Monad.Monad' MyIO where -- this instance can also be derived
-- >     'Control.Monad.return' = WrapMyIO . 'Control.Monad.return'
-- >     f 'Control.Monad.>>= bindTo    =    WrapMyIO $ unwrapMyIO f 'Control.Monad.>>=' unwrapMyIO . bindTo
-- >     'Control.Monad.fail' message = WrapMyIO $ 'PFail' (MyErr message)
-- > 
-- > instance 'Control.Monad.MonadPlus' MyIO where -- this instance can also be derived
-- >     'Control.Monad.mzero' = WrapMyIO 'mzero'
-- >     'Control.Monad.mplus' (WrapMyIO try1) (WrapMyIO try2) = WrapMyIO ('mplus' try1 try2)
-- >
-- > instance 'Control.Monad.Error.Class.MonadError' MyErr MyIO where
-- >     'Control.Monad.Error.Class.MonadError.throwError' = WrapMyIO . 'Control.Monad.Error.Class.MonadError.throwError'
-- >     'Control.Monad.Error.Class.MonadError.catchError' (WrapMyIO try) catch = WrapMyIO ('catchError' try (unwrapMyIO . catch))
-- > 
-- > instance 'Control.Monad.IO.Class.MonadIO' MyIO where
-- >     'Control.Monad.IO.Class.liftIO' = WrapMyIO . 'liftIO'
-- > 
-- > doStep :: MyIO ()
-- > doStep = ...
-- > 
-- > doJump :: MyIO ()
-- > doJump = ...
module Dao.Predicate where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error

import           Data.Monoid

-- | 'Predicate' is a "predicate value" data type allows a monadic computation to backtrack and try
-- another branch of computation, or to fail without causing backtracking. As an example, lets
-- consider a simple string command evaluator which can run a function @doStep@ when given an input
-- string @"step"@, and a function @doJump@ when given a string "jump".
data Predicate err ok
  = Backtrack 
    -- ^ analogous to 'Prelude.Nothing', therefore it is possible to make a function like so:
    -- > doStepOrJump :: 'Prelude.String' -> MyIO ()
    -- > doStepOrJump cmd = mplus (doStep cmd) (doJump cmd)
  | PFail { failedItem :: err }
    -- ^ use this constructor when you wish to throw an error catchable with 'catchPValue' or
    -- 'Control.Monad.Error.Class.catchError'. 'PFail' values cannot be "caught" by
    -- 'Control.Monad.mplus', so it is possible to write a guard function like this:
    -- > commandValidElseFail :: String -> MyIO ()
    -- > commandValidElseFail cmd = 'Control.Monad.mplus' ('Control.Monad.guard' ('Prelude.notElem' cmd $ 'Prelude.words' "step next")) $
    -- >     'Control.Monad.fail' ("invalid command "++cmd)
    -- evaluates to 'PFail' so no characters will be parsed after that, unless the failure is caught
    -- by 'Control.Monad.Error.catchError' or 'catchPredicate'.
  | OK ok -- ^ A parser evaluates to 'OK' when it evaluates 'Control.Monad.return'.
  deriving (Eq, Ord, Show)

instance Functor (Predicate err) where
  fmap fn (OK    a) = OK (fn a)
  fmap _  (PFail u) = PFail u
  fmap _  Backtrack = Backtrack

instance Monad (Predicate err) where
  return = OK
  ma >>= mfn = case ma of
    OK     ok -> mfn    ok
    PFail err -> PFail err
    Backtrack -> Backtrack

instance MonadPlus (Predicate err) where
  mzero = Backtrack
  mplus Backtrack b = b
  mplus a         _ = a

instance MonadError err (Predicate err) where
  throwError           = PFail
  catchError try catch = case try of
    PFail err -> catch err
    try       -> try

instance Applicative (Predicate err) where { pure  = return; (<*>) = ap;    }

instance Alternative (Predicate err) where { empty = mzero;  (<|>) = mplus; }

instance Monoid ok => Monoid (Predicate err ok) where
  mempty                = Backtrack
  mappend (OK a) (OK b) = OK(a<>b)
  mappend     a      _  = a

----------------------------------------------------------------------------------------------------

-- | A monad transformer lifting 'Predicate' into an outer monad. Use 'runPreicateT' to remove the 
-- 'PredicateT' outer monad and retrieve the inner 'Predictate' value.
newtype PredicateT err m ok = PredicateT { runPredicateT :: m (Predicate err ok) }

instance Monad m => Monad (PredicateT err m) where
  return a = PredicateT (return (OK a))
  PredicateT ma >>= fma = PredicateT $ do
    a <- ma
    case a of
      Backtrack -> return Backtrack
      PFail   u -> return (PFail u)
      OK      o -> runPredicateT (fma o)
  PredicateT ma >> PredicateT mb = PredicateT $ do
    a <- ma
    case a of
      Backtrack -> return Backtrack
      PFail   u -> return (PFail u)
      OK      _ -> mb
  fail msg = PredicateT{ runPredicateT = return (PFail (error msg)) }

instance Functor m => Functor (PredicateT err m) where
  fmap f (PredicateT ma) = PredicateT (fmap (fmap f) ma)

instance Monad m => MonadPlus (PredicateT err m) where
  mzero = PredicateT (return Backtrack)
  mplus (PredicateT a) (PredicateT b) = PredicateT $ do
    result <- a
    case result of
      Backtrack -> b
      PFail   u -> return (PFail u)
      OK      o -> return (OK o)

instance Monad m => MonadError err (PredicateT err m) where
  throwError msg = PredicateT{ runPredicateT = return (PFail msg) }
  catchError ptrans catcher = PredicateT $ do
    value <- runPredicateT ptrans
    case value of
      Backtrack -> return Backtrack
      PFail   u -> runPredicateT (catcher u)
      OK      a -> return (OK a)

instance (Functor m, Monad m) => Applicative (PredicateT err m) where { pure = return; (<*>) = ap; }

instance (Functor m, Monad m) => Alternative (PredicateT err m) where { empty = mzero; (<|>) = mplus; }

instance MonadTrans (PredicateT err) where { lift m = PredicateT(m >>= return . OK) }

instance MonadIO m => MonadIO (PredicateT err m) where { liftIO = PredicateT . liftIO . fmap OK }

----------------------------------------------------------------------------------------------------

-- |  If you want to analyze the 'Predicate' value of a 'PredicateT' computation, or any coputation
-- which lifts 'PredicateT' you can use the instances of this class to do so. For example, lets say
-- you have a computation @couldFailOrBacktrack :: MyIO ()@. You can check if the computation failed
-- or backtracked without unlifting the inner monad:
-- > do p <- 'catchPredicate' couldFailOrBacktrack
-- >    case p of
-- >        'OK'    rval -> useReturnValue rval -- use the return value from couldFailOrBacktrack
-- >        'PFail' msg  -> showMyError msg     -- report the error from couldFailOrBacktrack
-- >        'Backtrack'  -> return ()           -- ignore backtracking
-- If you would like to "re-throw" a 'Predicate' that you have received you can use the 'predicate'
-- function. For example, this line of code could be added to the above procedure:
-- >    predicate p
-- and the function will evaluate to the same exact 'Predicate' value that @couldFailOrBacktrack@
-- had produced.
-- 
-- Here is an example of how you would instantiate the 'MyIO' type in the example above into this
-- class:
-- > instance MonadPlusError MyErr MyIO where
-- >     catchPredicate = WrapMyIO . catchPredicate . unwrapMyIO
class MonadPlusError err m where
  -- | Unlifts the whole 'Predicate' value, unlike 'catchError' which only catches the value stored
  -- in a 'PFail' constructor.
  catchPredicate :: m a -> m (Predicate err a)
  -- | This will force the 'Predicate' value of the current computation. The following should
  -- generally be true for all instances of 'MonadPlusError'.
  -- > 'Control.Monad.return' = 'predicate' . 'OK'
  -- > 'Control.Monad.Error.State.throwError' = 'predicate' . 'PFail'
  -- > 'Control.Monad.mzero' = 'predicate' 'Backtrack'
  predicate :: Predicate err a -> m a

instance MonadPlusError err (Predicate err) where { catchPredicate = OK; predicate = id; }

instance Monad m => MonadPlusError err (PredicateT err m) where
  catchPredicate (PredicateT fn) = PredicateT{ runPredicateT = fn >>= \o -> return (OK o) }
  predicate pval = PredicateT (return pval)

-- | Evaluates to an empty list if the given 'Predicate' is 'Backtrack' or 'PFail', otherwise returns a
-- list containing the value in the 'OK' value.
okToList :: Predicate err o -> [o]
okToList pval = case pval of
  OK      o -> [o]
  Backtrack -> []
  PFail   _ -> []

-- | Like 'okToList', but evaluates to 'Data.Maybe.Nothing' if the given 'Predicate' is 'Backtrack' or
-- 'PFail', or 'Data.Maybe.Just' containing the value in the 'OK' value.
okToMaybe :: Predicate err o -> Maybe o
okToMaybe pval = case pval of
  OK      o -> Just o
  Backtrack -> Nothing
  PFail   _ -> Nothing

-- | If given 'Data.Maybe.Nothing', evaluates to 'PFail' with the given error information.
-- Otherwise, evaluates to 'OK'.
maybeToPFail :: err -> Maybe o -> Predicate err o
maybeToPFail err o = case o of
  Nothing -> PFail err
  Just ok -> OK    ok

-- | Like 'Prelude.fmap' but operates on the error report data of the 'Predicate'.
fmapPFail :: (errA -> errB) -> Predicate errA o -> Predicate errB o
fmapPFail f pval = case pval of
  OK      o -> OK o
  Backtrack -> Backtrack
  PFail err -> PFail (f err)

