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
import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.State

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
    [ Handler $ \ (PatternMatchFail msg) -> err ONull
    , Handler $ \ (AssertionFailed  msg) -> err (ostr msg)
    , Handler $ \ (RecSelError      msg) -> err (ostr msg)
    , Handler $ \ (RecUpdError      msg) -> err (ostr msg)
    ]
  where
    ostr msg = OString (ustr msg)
    err  msg = return (Left msg)


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

----------------------------------------------------------------------------------------------------

-- | 'PValue' is a "predicate value" data type allows a monadic computation to backtrack and try
-- another branch of computation, or to fail without causing backtracking.  These values are used
-- internally to the 'Dao.Parser.Parser's.
data PValue item a
  = Backtrack
    -- ^ 'Backtrack' is a value used internally to the 'Parser's 'Control.Monad.State.State' monad
    -- to signal a temporary parse failure, which indicates to the 'Parser' monad to stop parsing
    -- and try another choice. Choices are specified by 'Control.Monad.mplus', for example
    -- @'Control.Monad.mplus' a b@, where @a@ and @b@ are 'Parser's, will first try to evaluate @a@,
    -- but if @b@ evaluates to 'mzero', this will internally return the 'Backtrack' value to the
    -- 'Parser's 'Control.Monad.State.State' monad, which will cause @b@ to be evaluated. If the
    -- entire parser evaluates to a 'Backtrack' this means the 'Parser' did not fail, but it did not
    -- match the input string either, at which point you can have your 'Parser' evaluate 'PFail'
    -- using the 'pfail' or 'Control.Monad.fail' functions.
    --
    -- Note that 'Backtrack'ing does not put back the characters that were taken from the input.
    -- Those characters are still gone, parsing simply continues from the next alternative in the
    -- branch. For example, the parser:
    -- @'Control.Monad.mplus' (a >> b) c where@
    -- @      a = 'manyRegex1' ('rxChar' \'A\')@
    -- @      b = 'manyRegex1' ('rxChar' \'B\')@
    -- @      c = 'manyRegex1' ('rxChar' \'C\')@
    -- can evaluate against the string "AAACCC", and the whole string will be parsed and will return
    -- a 'Token' with the characters "CCC". What happens is this: @a@ parses "AAA" and succeeds, the
    -- remaining input string is now "CCC", @b@ tries to parse some "B" characters but fails and
    -- 'Backtrack's. @c@ is the first choice after 'Backtrack'ing and successfully parses "CCC".
    -- 
    -- If you want to "undo" what was parsed by forcing characters back onto the input string, you
    -- can use the 'backtrack' function. But this is inefficient and you should design your parser
    -- to avoid this.
  | PFail { failedItem :: item, failedBecause :: UStr }
    -- ^ If any 'Parser' function in your 'Parser' computation evaluates 'PFail', the whole parser
    -- evaluates to 'PFail' so no characters will be parsed after that, unless the failure is caught
    -- by 'Control.Monad.Error.catchError' or 'pcatch'.
  | OK a -- ^ A parser evaluates to 'OK' when it evaluates 'Control.Monad.return'.
  deriving (Eq, Ord, Show)

instance Functor (PValue tok) where
  fmap fn (OK    a  ) = OK (fn a)
  fmap _  (PFail u v) = PFail u v
  fmap _  Backtrack   = Backtrack

instance Monad (PValue tok) where
  return = OK
  ma >>= mfn = case ma of
    OK    a   -> mfn a
    PFail u v -> PFail u v
    Backtrack -> Backtrack

instance MonadPlus (PValue tok) where
  mzero = Backtrack
  mplus ma mb = case ma of
    Backtrack  -> mb
    PFail ~u v -> PFail u v
    OK     a   -> OK    a

fromPValue :: a -> PValue tok a -> a
fromPValue a pval = case pval of { OK a -> a ; _ -> a }

----------------------------------------------------------------------------------------------------

-- | A monad transformer for 'PValue', this is especially handy with a 'Control.Monad.State.State'
-- monad. For example 'Dao.Parser.Parser' is a 'Control.Monad.State.State' monad lifted into the
-- 'PTrans' monad.
newtype PTrans tok m a = PTrans { runPTrans :: m (PValue tok a) }

pvalue :: Monad m => PValue tok a -> PTrans tok m a
pvalue pval = PTrans (return pval)

instance Monad m => Monad (PTrans tok m) where
  return a = PTrans (return (OK a))
  PTrans ma >>= fma = PTrans $ do
    a <- ma
    case a of
      Backtrack -> return Backtrack
      PFail u v -> return (PFail u v)
      OK    o   -> runPTrans (fma o)
  PTrans ma >> PTrans mb = PTrans $ do
    a <- ma
    case a of
      Backtrack -> return Backtrack
      PFail u v -> return (PFail u v)
      OK    _   -> mb
  fail msg = PTrans{ runPTrans = return (PFail undefined (ustr msg)) }

tokenFail :: Monad m => tok -> String -> PTrans tok m ig
tokenFail tok msg = PTrans{ runPTrans = return (PFail tok (ustr msg)) }

instance Functor m => Functor (PTrans tok m) where
  fmap f (PTrans ma) = PTrans (fmap (fmap f) ma)

-- | 'mzero' introduces backtracking, 'mplus' introduces a choice.
instance Monad m => MonadPlus (PTrans tok m) where
  mzero = PTrans (return Backtrack)
  mplus (PTrans a) (PTrans b) = PTrans $ do
    result <- a
    case result of
      Backtrack  -> b
      PFail ~u v -> return (PFail u v)
      OK     o   -> return (OK o)

instance MonadTrans (PTrans tok) where
  lift m = PTrans{ runPTrans = m >>= return . OK }

-- | 'throwError' is like to 'Parser's instantiation of 'Control.Monad.fail', except it takes a
-- 'Dao.String.UStr'. The @tok@ type is undefeind, so it makes sense to define it once you have
-- caught it, or to override the 'Control.Monad.Error.Class.throwError' function with your own
-- instantiation of a newtype of 'PTrans'.
instance Monad m => MonadError UStr (PTrans tok m) where
  throwError msg = PTrans{ runPTrans = return (PFail undefined msg) }
  catchError ptrans catcher = PTrans $ do
    value <- runPTrans ptrans
    case value of
      Backtrack -> return Backtrack
      PFail u v -> runPTrans (catcher v)
      OK    a   -> return (OK a)

tokenThrowError :: Monad m => tok -> UStr -> PTrans tok m ig
tokenThrowError tok msg = PTrans{ runPTrans = return (PFail tok msg) }

