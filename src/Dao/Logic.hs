-- "Dao/Logic.hs"  A stateful monad that reverts the state in the event of
-- backtracking or throwError.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see the URL:
-- <http://www.gnu.org/licenses/agpl.html>.

{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'LogicT' monad transformer, and 'MonadLogic' class, which can be used
-- to model nondeterministic finite state automata, and is specifically designed for stateful
-- pattern matching. Like the 'Prelude.Maybe' monad, pattern matching on bind, evaluates to
-- 'Control.Monad.mzero', resulting in backtracking.
--
-- 'LogicT' and transformers deriving from it are excellent for parsing command line arguments, but
-- should be avoided for designing parsers of strings, because every branch lazily creates a copy of
-- the state. Use "Dao.Grammar" for defining string parsers.
module Dao.Logic where

import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.State.Class

----------------------------------------------------------------------------------------------------

-- | The 'LogicT' monad transformer allows for stateful pattern matching. 'LogicT' is very similar
-- to the 'Control.Monad.State.StateT' monad transformer, indeed it instantiates
-- 'Control.Monad.State.Class.MonadState'. The main difference is that the 'LogicT' monad can model
-- a function holding many possible, mutually exclusive states, and returning many possible
-- mututally exclusive values. If the monadic function backtracks using 'Control.Applicative.empty'
-- or 'Control.Monad.mzero', the state is reverted.
newtype LogicT st m a = LogicT{ runLogicT :: st -> m [(a, st)] }

type Logic st a = LogicT st Identity a

logic :: (st -> [(a, st)]) -> Logic st a
logic = LogicT . fmap Identity

instance Functor m => Functor (LogicT st m) where
  fmap f (LogicT p) = LogicT $ fmap (fmap (first f)) . p

instance (Functor m, Applicative m, Monad m) => Applicative (LogicT st m) where
  pure = return
  (<*>) = ap

instance (Functor m, Applicative m, Monad m) => Alternative (LogicT st m) where
  empty = mzero
  (<|>) = mplus

instance (Functor m, Applicative m, Monad m) => Monad (LogicT st m) where
  return o = LogicT $ \st -> return [(o, st)]
  (LogicT o) >>= f = LogicT $ \st -> o st >>= fmap concat .  mapM (\ (o, st) -> runLogicT (f o) st)

instance (Functor m, Applicative m, Monad m) => MonadPlus (LogicT st m) where
  mzero                       = LogicT $ const $ return []
  mplus (LogicT a) (LogicT b) = LogicT $ \st -> (++) <$> a st <*> b st

instance (Functor m, Applicative m, Monad m) => MonadState st (LogicT st m) where
  state f = LogicT $ \st -> return [f st]

instance (Functor m, Applicative m, Monad m, MonadIO m) => MonadIO (LogicT st m) where
  liftIO f = LogicT $ \st -> (\a -> [(a, st)]) <$> liftIO f

instance MonadTrans (LogicT st) where
  lift f = LogicT $ \st -> f >>= \a -> return [(a, st)]

-- | Similar to 'Control.Monad.State.evalStateT'.
evalLogicT :: Functor m => LogicT st m a -> st -> m [a]
evalLogicT f = fmap (fmap fst) . runLogicT f

evalLogic :: Logic st a -> st -> [a]
evalLogic f = runIdentity . evalLogicT f

-- | Similar to 'Control.Monad.State.execStateT'.
execLogicT :: Functor m => LogicT st m a -> st -> m [st]
execLogicT f = fmap (fmap snd) . runLogicT f

execLogic :: Logic st a -> st -> [st]
execLogic f = runIdentity . execLogicT f

----------------------------------------------------------------------------------------------------

class (Functor m, Applicative m, Monad m) => MonadLogic st m | m -> st where
  -- | Like 'Control.Monad.State.Class.state', but return multiple possible states and outcomes.
  -- Think of this as creating a computation that is in an uncertain superposition, with many
  -- possible states and outcomes.
  --
  -- There is a law for 'superState' requiring that if the function @(st -> [(a, st)])@ that was
  -- passed to 'superState' evaluates to an empty list, then 'superState' must evaluate to
  -- 'Control.Applicative.empty' and/or 'Control.Monad.mzero'.
  superState :: (st -> [(a, st)]) -> m a
  -- | Given a 'LogicT' monadic function, use the current state with 'runLogicT' to evaluate this
  -- function, producing a list of all possible (outcome,state) pairs. Place the current state back
  -- into the 'runLogicT' monad and return the list of all possible (outcome,state) pairs.
  --
  -- There is a law for 'entangle' requiring that if the function @(m a)@ that was passed to
  -- 'entangle' evaluates to 'Control.Applicative.empty' and/or 'Control.Monad.mzero', then
  -- 'entangle' must evaluate to @'Control.Monad.return' []@.
  entangle :: m a -> m [(a, st)]

instance (Functor m, Applicative m, Monad m) => MonadLogic st (LogicT st m) where
  superState f = LogicT $ \st -> return (f st)
  entangle (LogicT f) = LogicT $ \st -> fmap (\all -> [(all, st)]) (f st)

-- | Like 'Control.Monad.State.modify', but puts the current state of the computation into an
-- uncertain superposition, with many possible states.
superModify :: MonadLogic st m => (st -> [st]) -> LogicT st m ()
superModify f = superState (fmap ((,) ()) . f)

-- | Return many possible results.
possibly :: MonadLogic st m => [a] -> LogicT st m a
possibly o = superState $ \st -> fmap (\o -> (o, st)) o

-- | Equivalent to:
--
-- @\\f -> 'Control.Applicative.fmap' 'Prelude.fst' 'Control.Applicative.<$>' 'entangle' f@
--
outcomes :: MonadLogic st m => LogicT st m a -> LogicT st m [a]
outcomes = fmap (fmap fst) . entangle

-- | Equivalent to:
--
-- @\\f -> 'Control.Applicative.fmap' 'Prelude.snd' 'Control.Applicative.<$>' 'entangle' f@
--
states :: MonadLogic st m => LogicT st m a -> LogicT st m [st]
states = fmap (fmap snd) . entangle

-- | If a given 'LogicT' monad evaluates successfully, make that success a failure by evaluating to
-- 'Control.Monad.mzero', otherwise evaluate to @return ()@.
failIf :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadLogic st m) => m a -> m ()
failIf m = (m >> mzero) <|> return ()

-- | Logical exclusive-OR, matches one rule or the other, but not both. This function uses
-- 'Dao.Logic.entangle', so there is a small performance penalty as the lazy state must be evaluated
-- strictly, but this loss in performance could be regained by reducing the number of branches of
-- logic evaluation.
exclusive :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadLogic st m) => [m a] -> m a
exclusive = entangle . msum >=> \matched ->
  case matched of { [o] -> superState $ const [o]; _ -> mzero; }

-- | Given an 'Prelude.Int' value @n@, take only the first @n@ branches of logic evaluation. This
-- function uses 'Dao.Logic.entangle', so there is a small performance penalty as the lazy state
-- must be evaluated strictly, but this loss in performance could be regained by reducing the number
-- of branches of logic evaluation.
chooseOnly :: (Functor m, Applicative m, Monad m, MonadLogic st m) => Int -> m a -> m a
chooseOnly n = entangle >=> superState . const . take n

-- | When an error is thrown using 'Control.Monad.Except.throwError', the error is not caught right
-- away, rather it lingers in the internal state until it can be caught while branches of execution
-- that have not thrown any errors are evaluated. As evaluation progresses and more and more errors
-- are thrown, the amount of space taken by errors in the internal state starts to grow. When using
-- 'Control.Monad.Except.catchError', all of these errors are applied to the given catching
-- function.
--
-- If you know you never want to catch any errors from a function that throws errors, and you just
-- want to delete them from the internal state, you can call this function.
--
-- This function is defined as:
--
-- @'Prelude.flip' 'Control.Monad.Except.catchError' ('Prelude.const' 'Control.Monad.mzero')@
dropErrors :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError err m, MonadLogic st m) => m a -> m a
dropErrors = flip catchError (const mzero)

