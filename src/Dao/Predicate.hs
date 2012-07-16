-- "src/Dao/Predicate.hs"  provides 'PredicateIO', a monad for easily
-- overloading functions built-in to the Dao scripting language.
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


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Provides a special monad for building complex predicates that can check the structure of
-- complex data types, but in a way that I find to be much simpler than "Data.Data". The 'Predicate'
-- monad lifts the IO monad into the 'Dao.Combination.Combiation' monad, and catches all pattern
-- matching exceptions, so predicates can be written in plain-old Haskell. Non-exhaustive case
-- statements and lambda expressions that only match on a narow range of types will not evaluate to
-- bottom, they will simply evaluate to @Control.Monad.mzero@. Like the
-- 'Dao.Combination.Combination' monad, the @>>=@ and @>>@ operators compute conjunctive conditions
-- (all must be true to succeede) and 'Control.Monad.msum' and 'Control.Monad.mplus' compute
-- disjunctive conditions (any condition will succeede).

module Dao.Predicate where

import           Dao.Object
import           Dao.Combination

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.State.Class

import           System.IO.Unsafe

import           Debug.Trace

-- | This monad must contain the IO monad because we can only catch exceptions in the IO monad.
newtype PredicateIO st a = PredicateIO { runPredicateIO :: CombinationT st IO a }

-- Will catch exceptions of the types:
-- 'Control.Exception.PatternMatchFail', 'Control.Exception.RecConError',
-- 'Control.Exception.RecUpdError', and 'Control.Exception.AssertionFailed'. This function evaluates
-- to a failed 'Dao.Combination.CombinationT' monad rather than evaluating to "bottom" on catching
-- these exceptions.
noBadPatternsIO :: IO a -> IO (Either Object a)
noBadPatternsIO fn = catches (fn >>= \e -> seq e (return (Right e))) $
  [ Handler $ \ (PatternMatchFail msg) -> err msg
  , Handler $ \ (AssertionFailed  msg) -> err msg
  , Handler $ \ (RecSelError      msg) -> err msg
  , Handler $ \ (RecUpdError      msg) -> err msg
  ]
  where { err msg = return (Left (OString (ustr msg))) }

-- | The false predicate, uses 'Dao.Combination.failWith' to pass an 'Dao.Types.Object' that
-- signifies why the predicate failed.
falseIO :: Object -> PredicateIO st ignored
falseIO = PredicateIO . failWith

-- | Labeling your predicate means to attach an object that will be used as an error message if the
-- predicate fails. It is a bit like the 'Text.ParserCombinators.Parsec.<?>' operator in the
-- "Text.ParserCombinators.Parsec" library. This function makes use of the 'Dao.Combination.failMsg'
-- equation.
labelIO :: Object -> PredicateIO st a -> PredicateIO st a
labelIO obj fn = PredicateIO (failMsg obj (runPredicateIO fn))

instance Monad (PredicateIO st) where
  PredicateIO fn >>= mfn = PredicateIO $ CombinationT $ \st -> do
    e <- noBadPatternsIO (runCombinationT (fn >>= runPredicateIO . mfn) st >>= evaluate)
    case e of
      Left err -> return [(Left err, st)]
      Right ma -> return ma
  return a = PredicateIO (return a)
  fail msg = PredicateIO (fail msg)

instance Functor (PredicateIO st) where
  fmap f ma = ma >>= return . f

instance MonadPlus (PredicateIO st) where
  mzero = PredicateIO mzero
  mplus (PredicateIO a) (PredicateIO b) = PredicateIO $
    mplus (CombinationT $ \st -> runCombinationT a st >>= evaluate) b

instance MonadState st (PredicateIO st) where
  get = PredicateIO get
  put a = PredicateIO (put a)

instance MonadIO (PredicateIO st) where
  liftIO fn = PredicateIO (lift fn)

