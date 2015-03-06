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
-- Also provided are the 'Conjunction', 'Statement', and 'Satisfy' monads for constraint logic
-- computations.
--
-- 'LogicT' and transformers deriving from it are excellent for parsing command line arguments, but
-- should be avoided for designing parsers of strings, because every branch lazily creates a copy of
-- the state. Use "Dao.Grammar" for defining string parsers.
module Dao.Logic where

import           Dao.Array
import           Dao.Lens
import           Dao.TestNull

import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Except
import           Control.Monad.State.Class

import           Data.List (nub)
import           Data.Monoid
import           Data.Typeable

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

instance (Functor m, Monad m) => Applicative (LogicT st m) where
  pure = return
  (<*>) = ap

instance (Functor m, Monad m) => Alternative (LogicT st m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => Monad (LogicT st m) where
  return o = LogicT $ \st -> return [(o, st)]
  (LogicT o) >>= f = LogicT $ o >=> liftM concat . mapM (uncurry $ runLogicT . f)

instance Monad m => MonadPlus (LogicT st m) where
  mzero                       = LogicT $ const $ return []
  mplus (LogicT a) (LogicT b) = LogicT $ \st -> ap (ap (return (++)) (a st)) (b st)

instance Monad m => MonadState st (LogicT st m) where
  state f = LogicT $ \st -> return [f st]

instance MonadFix m => MonadFix (LogicT st m) where
  mfix f = LogicT $ \st -> mfix $ liftM concat . mapM (\ ~(a, _) -> runLogicT (f a) st)

instance (Monad m, MonadIO m) => MonadIO (LogicT st m) where
  liftIO f = LogicT $ \st -> liftIO f >>= \a -> return [(a, st)]

instance MonadTrans (LogicT st) where
  lift f = LogicT $ \st -> f >>= \a -> return [(a, st)]

-- | Similar to 'Control.Monad.State.evalStateT'.
evalLogicT :: Monad m => LogicT st m a -> st -> m [a]
evalLogicT f = runLogicT f >=> return . fmap fst

evalLogic :: Logic st a -> st -> [a]
evalLogic f = runIdentity . evalLogicT f

-- | Similar to 'Control.Monad.State.execStateT'.
execLogicT :: Functor m => LogicT st m a -> st -> m [st]
execLogicT f = fmap (fmap snd) . runLogicT f

execLogic :: Logic st a -> st -> [st]
execLogic f = runIdentity . execLogicT f

----------------------------------------------------------------------------------------------------

class Monad m => MonadLogic st m | m -> st where
  -- | Like 'Control.Monad.State.Class.state', but return multiple possible states and outcomes.
  -- Think of this as creating a computation that is in an uncertain superposition, with many
  -- possible states and outcomes.
  --
  -- There is a law for 'superState' requiring that if the function @(st -> [(a, st)])@ that was
  -- passed to 'superState' evaluates to an empty list, then 'superState' must evaluate to
  -- 'Control.Monad.mzero'.
  superState :: (st -> [(a, st)]) -> m a
  -- | Given a 'LogicT' monadic function, use the current state with 'runLogicT' to evaluate this
  -- function, producing a list of all possible (outcome,state) pairs. Place the current state back
  -- into the 'runLogicT' monad and return the list of all possible (outcome,state) pairs.
  --
  -- There is a law for 'entangle' requiring that if the function @(m a)@ that was passed to
  -- 'entangle' evaluates to 'Control.Monad.mzero', then
  -- 'entangle' must evaluate to @'Control.Monad.return' []@.
  entangle :: m a -> m [(a, st)]

instance Monad m => MonadLogic st (LogicT st m) where
  superState f = LogicT $ \st -> return (f st)
  entangle (LogicT f) = LogicT $ \st -> f st >>= \all -> return [(all, st)]

-- | Like 'Control.Monad.State.modify', but puts the current state of the computation into an
-- uncertain superposition, with many possible states.
superModify :: MonadLogic st m => (st -> [st]) -> LogicT st m ()
superModify f = superState (f >=> return . (,) ())

-- | Return many possible results.
possibly :: MonadLogic st m => [a] -> LogicT st m a
possibly o = superState $ \st -> fmap (\o -> (o, st)) o

-- | Equivalent to:
--
-- @\\f -> 'Control.Applicative.fmap' 'Prelude.fst' 'Control.Applicative.<$>' 'entangle' f@
--
outcomes :: MonadLogic st m => LogicT st m a -> LogicT st m [a]
outcomes = entangle >=> return . fmap fst

-- | Equivalent to:
--
-- @\\f -> 'Control.Applicative.fmap' 'Prelude.snd' 'Control.Applicative.<$>' 'entangle' f@
--
states :: MonadLogic st m => LogicT st m a -> LogicT st m [st]
states = entangle >=> return . fmap snd

-- | If a given 'LogicT' monad evaluates successfully, make that success a failure by evaluating to
-- 'Control.Monad.mzero', otherwise evaluate to @return ()@.
failIf :: (Monad m, MonadPlus m, MonadLogic st m) => m a -> m ()
failIf m = mplus (m >> mzero) (return ())

-- | Logical exclusive-OR, matches one rule or the other, but not both. This function uses
-- 'Dao.Logic.entangle', so there is a small performance penalty as the lazy state must be evaluated
-- strictly, but this loss in performance could be regained by reducing the number of branches of
-- logic evaluation.
exclusive :: (Monad m, MonadPlus m, MonadLogic st m) => [m a] -> m a
exclusive = entangle . msum >=> \matched ->
  case matched of { [o] -> superState $ const [o]; _ -> mzero; }

-- | Given an 'Prelude.Int' value @n@, take only the first @n@ branches of logic evaluation. This
-- function uses 'Dao.Logic.entangle', so there is a small performance penalty as the lazy state
-- must be evaluated strictly, but this loss in performance could be regained by reducing the number
-- of branches of logic evaluation.
chooseOnly :: (Monad m, MonadLogic st m) => Int -> m a -> m a
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
dropErrors :: (Monad m, MonadPlus m, MonadError err m, MonadLogic st m) => m a -> m a
dropErrors = flip catchError (const mzero)

----------------------------------------------------------------------------------------------------

-- | Functions for running a proof and evaluating whether the proof result is 'Prelude.True' or
-- 'Prelude.False'.
class Provable expr where
  -- | Prove using a monadic predicate.
  prove :: (Monad m, MonadPlus m) => (pat -> m Bool) -> expr pat -> m Bool
  -- | Prove using a pure predicate.
  provePure :: (pat -> Bool) -> expr pat -> Bool

-- | A logical statement, where @pat@ can evaluate to some boolean value depending on whether it
-- matches or "satisfies" some condition given in a 'proof' or 'pureProof'.
newtype Satisfy pat = Satisfy (Bool, pat) deriving (Eq, Ord, Show, Typeable)

instance Functor Satisfy where { fmap f (Satisfy a) = Satisfy $ fmap f a; }

instance TestNull (Conjunction pat) where
  nullValue = Conjunction nullValue
  testNull (Conjunction o) = testNull o

instance Provable Satisfy where
  prove predicate (Satisfy (true, statement)) = liftM (true ==) $ predicate statement
  provePure predicate (Satisfy (true, statement)) = true == predicate statement

satisfyLens :: Monad m => Lens m (Satisfy pat) (Bool, pat)
satisfyLens = newLens (\ (Satisfy a) -> a) (\a _ -> Satisfy a)

shouldBe :: Monad m => Lens m (Satisfy pat) Bool
shouldBe = satisfyLens >>> tuple0

satisfyStatement :: Monad m => Lens m (Satisfy pat) pat
satisfyStatement = satisfyLens >>> tuple1

-- | Logical inverse of a 'Satisfy' statement.
inverseSatisfy :: Satisfy pat -> Satisfy pat
inverseSatisfy (Satisfy (true, statement)) = Satisfy (not true, statement)

-- | Conjoint logical expressions (logical-AND). During a 'proof' or 'pureProof', a 'Conjunction' is
-- true only if all of the conditions within it are satisfied.
newtype Conjunction pat = Conjunction (Array (Satisfy pat)) deriving (Eq, Ord, Show, Typeable)

instance Functor Conjunction where { fmap f (Conjunction o) = Conjunction $ fmap (fmap f) o; }

instance Provable Conjunction where
  prove predicate (Conjunction statements) =
    let loop statement = case statement of
          []             -> return True
          next:statement -> prove predicate next >>= guard >> loop statement
    in  loop $ elems statements
  provePure predicate (Conjunction statements) = and $ provePure predicate <$> elems statements

instance Monoid (Conjunction a) where
  mempty = Conjunction mempty
  mappend (Conjunction a) (Conjunction b) = Conjunction $ a<>b

conjunctionLens :: Monad m => Lens m (Conjunction pat) (Array (Satisfy pat))
conjunctionLens = newLens (\ (Conjunction a) -> a) (\a _ -> Conjunction a)

-- | Logical inverse of a conjunction that produces a 'Statement'.
inverseConjunction :: Eq pat => Conjunction pat -> Statement pat
inverseConjunction (Conjunction o) = Statement $ if testNull o then mempty else array $
  fmap (Conjunction . array . pure . inverseSatisfy) $ elems o

-- | Canonical form for disjoint logical expressions (logical-OR). During a 'proof' or 'pureProof',
-- a 'Statement' is true if any one of the conditions within it are satisfied.
--
-- This data type instantiates 'Dao.Object.ObjectData' so that it can be serialized and transmitted
-- to and from files.
newtype Statement pat = Statement (Array (Conjunction pat)) deriving (Eq, Ord, Show, Typeable)

instance Functor Statement where { fmap f (Statement o) = Statement $ fmap (fmap f) o; }

instance Monoid (Sum (Statement a)) where
  mempty = Sum $ Statement mempty
  mappend (Sum (Statement a)) (Sum (Statement b)) = Sum $ Statement $ a<>b

instance Monoid (Product (Statement a)) where
  mempty = Product $ Statement mempty
  mappend (Product (Statement a)) (Product (Statement b)) =
    Product $ Statement $ array $ filter (not . testNull) $ mappend <$> elems a <*> elems b

instance TestNull (Statement pat) where
  nullValue = Statement nullValue
  testNull (Statement o) = testNull o

instance Provable Statement where
  prove predicate (Statement statements) =
    mplus (msum $ prove predicate <$> elems statements) $ return False
  provePure predicate (Statement statements) = or $ provePure predicate <$> elems statements

-- | The disjunction of all 'Conjunction' data types forms the 'Statement'. You can operate on the
-- disjunction directly using this lens.
disjunctionLens :: Monad m => Lens m (Statement pat) (Array (Conjunction pat))
disjunctionLens = newLens (\ (Statement a) -> a) (\a _ -> Statement a)

-- | Construct a provable statement which asserts that an expression @pat@ is true or that it is
-- false.
posit :: Bool -> pat -> Statement pat
posit that o = Statement $ array [Conjunction $ array [Satisfy (that, o)]]

-- | Simplify a 'Statement' expression. This will remove duplicates, and if there are any empty
-- 'Conjunctions', the whole expression is replaced with a single empty 'Conjunction' to indicate
-- that the 'proof' of the expression is always 'Prelude.True'.
simplify :: Eq pat => Statement pat -> Statement pat
simplify (Statement o) = let ox = elems o in Statement $ array $
  if or (testNull <$> ox) then [nullValue] else nub ox

-- | Invert the logic of the expression. This function satisfies the following law:
-- @
-- 'provePure' predicate ('logicalInverse' statement) == 'Prelude.not' ('provePure' predicate statement)
-- @
--
-- For every possible @predicate@ and @expression@.
logicalInverse :: Eq pat => Statement pat -> Statement pat
logicalInverse (Statement o) =
  case inverseConjunction <$> elems o of
    []                        -> Statement $ array [mempty]
    ox | or (testNull <$> ox) -> Statement $ mempty
    o:ox                      -> by [disjunctionLens $= array . nub . elems] $
      foldl (\ (Statement a) (Statement b) -> Statement $ array $ mappend <$> elems a <*> elems b) o ox

-- | Infix operator with the same fixity as 'Prelude.&&', but evaluates to:
--
-- @
-- 'Data.Monoid.getProduct' (Data.Monoid.Product' a 'Data.Monoid.<>' 'Data.Monoid.Product' b)
-- @
--
-- Useful for combining 'Statement's.
(<>&&) :: Monoid (Product a) => a -> a -> a
(<>&&) a b = getProduct $ Product a <> Product b
infixl 3 <>&&

-- | Infix operator with the same fixity as 'Prelude.||', but evaluates to:
--
-- @
-- 'Data.Monoid.getSum' (Data.Monoid.Sum' a 'Data.Monoid.<>' 'Data.Monoid.Sum' b)
-- @
--
-- Useful for combining 'Statement's.
(<>||) :: Monoid (Sum a) => a -> a -> a
(<>||) a b = getSum $ Sum a <> Sum b
infixl 2 <>||

