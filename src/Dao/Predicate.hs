-- "Dao/Predicate.hs"  provides 'PredicateT' which combines the Maybe and
-- Either types into a single monad.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- The Dao System is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
-- more details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

-- | Provides a monad that essentially combines the monadic functionality of 'Prelude.Maybe' and
-- 'Prelude.Either' into a single monad 'Predicate. Both the 'Prelude.Maybe' and 'Prelude.Either'
-- data types are both monads but it is convenient to have a single data type combining the two.
-- 'PError' is analogous to @'Prelude.Left'@, 'PFalse' is analogous to
-- @'Prelude.Right' $ 'Prelude.Nothing'@, and 'PTrue' is analogous to
-- @'Prelude.Right' . 'Prelude.Just'@. All relevant monad transformers are instnatiated, including
-- 'Control.Applicative.Applicative', 'Control.Applicative.Alternative', 'Control.Monad.MonadPlus',
-- and 'Control.Monad.Except.MonadError'. 
--
-- A new monad transformer 'PredicateT' is also introduced which lifts the 'Predicate' monad into
-- another monad and wraps it into the 'PredicateT' data type which instantiates the
-- 'Control.Monad.Trans.MonadTrans' class. Further, a new class 'PredicateClass' is defined which
-- allows you to directly manipulate the 'Predicate' value of a 'PredicateT' transformer.
-- 
-- Here is a simple example of how to use this module.
--
-- > newtype MyErr = MyErr String
-- > newtype MyIO a = WrapMyIO { unwrapMyIO :: 'PredicateT' MyErr IO a }
-- >         deriving (Functor, Applicative, Alternative)
-- > 
-- > instance 'Control.Monad.Monad' MyIO where -- this instance can also be derived
-- >     'Control.Monad.return' = WrapMyIO . 'Control.Monad.return'
-- >     f 'Control.Monad.>>= bindTo    =    WrapMyIO $ unwrapMyIO f 'Control.Monad.>>=' unwrapMyIO . bindTo
-- >     'Control.Monad.fail' message = WrapMyIO $ 'PError' (MyErr message)
-- > 
-- > instance 'Control.Monad.MonadPlus' MyIO where -- this instance can also be derived
-- >     'Control.Monad.mzero' = WrapMyIO 'mzero'
-- >     'Control.Monad.mplus' (WrapMyIO try1) (WrapMyIO try2) = WrapMyIO ('mplus' try1 try2)
-- > 
-- > instance 'Control.Monad.Except.Class.MonadError' MyErr MyIO where
-- >     'Control.Monad.Except.Class.MonadError.throwError' = WrapMyIO . 'Control.Monad.Except.Class.MonadError.throwError'
-- >     'Control.Monad.Except.Class.MonadError.catchError' (WrapMyIO try) catch = WrapMyIO ('catchError' try (unwrapMyIO . catch))
-- > 
-- > instance 'Control.Monad.IO.Class.MonadIO' MyIO where
-- >     'Control.Monad.IO.Class.liftIO' = WrapMyIO . 'liftIO'
-- > 
-- > doStep :: MyIO ()
-- > doStep = ...
-- > 
-- > doJump :: MyIO ()
-- > doJump = ...
--
module Dao.Predicate where

import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Monoid

-- | 'Predicate' is a data type that combines 'Prelude.Maybe' and 'Prelude.Either' into a single
-- type. The 'Predicate' monad allows you to construct predicate functions that evaluate to 'PTrue'
-- (true) 'PFalse' (false), and also provides a 'PError' constructor for indicating a "does not
-- make sense" condition.
--
-- The truth condition 'PTrue' can 'Control.Monad.return' a value which makes it a 'Data.Functor', an
-- 'Control.Applicative.Applicative', and a 'Control.Monad.Monad'. The false condition 'PFalse'
-- serves as the 'Control.Monad.mzero' for 'Control.Monad.MonadPlus' and 'Control.Applicative.empty'
-- value for 'Control.Applicative.Alternative'. The 'PError' condition, like 'Prelude.Left', can be
-- used as an error condition in the 'Control.Monad.Except.ErrorClass' which can be caught by
-- 'Control.Monad.Except.catchError'.
data Predicate err ok
  = PFalse     -- ^ analogous to 'Prelude.Nothing'
  | PError err -- ^ analogous to 'Prelude.Left'
  | PTrue  ok  -- ^ PTrue is "just right" i.e. it is analogous to 'Prelude.Just' and 'Prelude.Right'.
  deriving (Eq, Ord, Show)

instance Functor (Predicate err) where
  fmap fn (PTrue  a) = PTrue (fn a)
  fmap _  (PError u) = PError u
  fmap _   PFalse    = PFalse

instance Monad (Predicate err) where
  return = PTrue
  ma >>= mfn = case ma of
    PTrue  ok  -> mfn    ok
    PError err -> PError err
    PFalse     -> PFalse

instance MonadPlus (Predicate err) where
  mzero = PFalse
  mplus PFalse b = b
  mplus a      _ = a

instance MonadError err (Predicate err) where
  throwError           = PError
  catchError try catch = case try of
    PError err -> catch err
    try        -> try

instance Applicative (Predicate err) where { pure  = return; (<*>) = ap;    }

instance Alternative (Predicate err) where { empty = mzero;  (<|>) = mplus; }

instance MonadFix (Predicate err) where { mfix f = let m = m >>= f in m; }

instance Monoid ok => Monoid (Predicate err ok) where
  mempty                      = PFalse
  mappend (PTrue a) (PTrue b) = PTrue(a<>b)
  mappend        a         _  = a

----------------------------------------------------------------------------------------------------

-- | A monad transformer lifting 'Predicate' into an outer monad. Use 'runPreicateT' to remove the
-- 'PredicateT' outer monad and retrieve the inner 'Predictate' value.
newtype PredicateT err m ok = PredicateT { runPredicateT :: m (Predicate err ok) }

instance Monad m => Monad (PredicateT err m) where
  return = PredicateT . return . PTrue
  PredicateT ma >>= fma = PredicateT $ do
    a <- ma
    case a of
      PFalse   -> return PFalse
      PError e -> return (PError e)
      PTrue  o -> runPredicateT (fma o)
  PredicateT ma >> PredicateT mb = PredicateT $ do
    a <- ma
    case a of
      PFalse   -> return PFalse
      PError e -> return (PError e)
      PTrue  _ -> mb
  fail _ = mzero

instance Functor m => Functor (PredicateT err m) where
  fmap f (PredicateT ma) = PredicateT (fmap (fmap f) ma)

instance Monad m => MonadPlus (PredicateT err m) where
  mzero = PredicateT $ return PFalse
  mplus (PredicateT a) (PredicateT b) = PredicateT $ do
    result <- a
    case result of
      PFalse   -> b
      PError e -> return (PError e)
      PTrue  o -> return (PTrue  o)

instance Monad m => MonadError err (PredicateT err m) where
  throwError msg = PredicateT{ runPredicateT = return $ PError msg }
  catchError ptrans catcher = PredicateT $ do
    value <- runPredicateT ptrans
    case value of
      PFalse    -> return PFalse
      PError e -> runPredicateT $ catcher e
      PTrue  a -> return $ PTrue a

instance (Functor m, Monad m) => Applicative (PredicateT err m) where { pure = return; (<*>) = ap; }

instance (Functor m, Monad m) => Alternative (PredicateT err m) where { empty = mzero; (<|>) = mplus; }

instance MonadFix m => MonadFix (PredicateT err m) where
  mfix f = PredicateT $ mfix $ \a -> case a of
    PFalse   -> return PFalse
    PError e -> return $ PError e
    PTrue  a -> runPredicateT $ f a

instance MonadTrans (PredicateT err) where { lift m = PredicateT $ liftM PTrue m; }

instance MonadIO m => MonadIO (PredicateT err m) where { liftIO = PredicateT . liftIO . fmap PTrue }

----------------------------------------------------------------------------------------------------

-- | Often it is necessary to evaluate a sub-predicate monad within the 'Predicate' or 'PredicateT'
-- monads within the current 'Predicate' monad. Simply evaluating the sub-predicate would cause the
-- current predicate monad to evaluates to 'PError' or 'PFalse' if the sub-predicate evaluates to
-- either of these values. But using 'catchPredicate', it is possible to safely evaluate the
-- sub-predicate and capture it's 'Predicate' result, where you can then make a decision on how to
-- behave.
--
-- > do p <- 'catchPredicate' couldFailOrBacktrack
-- >    case p of
-- >        'PTrue'  rval -> useReturnValue rval -- use the return value from couldFailOrBacktrack
-- >        'PError' msg  -> printMyError msg    -- report the error from couldFailOrBacktrack
-- >        'PFalse'      -> return ()           -- ignore backtracking
--
-- The above procedure prints the error message created if the sub-predicate evaluated to 'PError'.
-- If you would like to "re-throw" a 'Predicate' that you have received you can use the 'predicate'
-- function. For example, this line of code could be added to the above procedure:
--
-- >    predicate p
--
-- and the function will evaluate to the same exact 'Predicate' value that @couldFailOrBacktrack@
-- had produced after the necessary response to the failure has been made, e.g. after the error
-- message has been printed.
--
-- Minimal complete definition is 'predicate' and 'returnPredicate'.
class Monad m => PredicateClass err m | m -> err where
  -- | Catches the predicate value of a monadic function @m@ and returns it.
  returnPredicate :: m a -> m (Predicate err a)
  -- | Unlifts the whole 'Predicate' value, unlike 'catchError' which only catches the value stored
  -- in a 'PError' constructor.
  catchPredicate :: m a -> (Predicate err a -> m a) -> m a
  catchPredicate try catch = returnPredicate try >>= catch
  -- | This will force the 'Predicate' value of the current computation. The following should
  -- generally be true for all instances of 'PredicateClass'.
  -- > 'Control.Monad.return' = 'predicate' . 'PTrue'
  -- > 'Control.Monad.Except.State.throwError' = 'predicate' . 'PError'
  -- > 'Control.Monad.mzero' = 'predicate' 'PFalse'
  predicate :: Predicate err a -> m a

instance Monad m => MonadReader env (PredicateT err (ReaderT env m)) where
  local f (PredicateT o) = PredicateT $ ReaderT $ runReaderT o . f
  ask = PredicateT $ liftM PTrue ask

instance Monad m => MonadState st (PredicateT err (StateT st m)) where
  state f = PredicateT $ StateT $ \st -> let (o, st') = f st in return (PTrue o, st')

instance PredicateClass err (Predicate err) where
  returnPredicate = PTrue
  catchPredicate  = flip ($)
  predicate       = id

instance Monad m => PredicateClass err (PredicateT err m) where
  predicate = PredicateT . return
  returnPredicate (PredicateT try) = PredicateT $ liftM PTrue try
  catchPredicate try catch = returnPredicate try >>= catch

-- | Evaluates to an empty list if the given 'Predicate' is 'PFalse' or 'PError', otherwise returns a
-- list containing the value in the 'PTrue' value.
okToList :: Predicate err o -> [o]
okToList pval = case pval of
  PTrue  o -> [o]
  PFalse   -> []
  PError _ -> []

-- | Like 'okToList', but evaluates to 'Data.Maybe.Nothing' if the given 'Predicate' is 'PFalse' or
-- 'PError', or 'Data.Maybe.Just' containing the value in the 'PTrue' value.
okToJust :: Predicate err o -> Maybe o
okToJust pval = case pval of
  PTrue  o -> Just o
  PFalse   -> Nothing
  PError _ -> Nothing

-- | If given 'Data.Maybe.Nothing', evaluates to 'PError' with the given error information.
-- Otherwise, evaluates to 'PTrue'.
maybeToPFail :: err -> Maybe o -> Predicate err o
maybeToPFail err o = case o of
  Nothing -> PError err
  Just ok -> PTrue   ok

-- | Like 'Prelude.fmap' but operates on the error report data of the 'Predicate'.
fmapPFail :: (errA -> errB) -> Predicate errA o -> Predicate errB o
fmapPFail f pval = case pval of
  PTrue  o -> PTrue o
  PFalse   -> PFalse
  PError e -> PError $ f e

-- | Like 'Data.Either.partitionEithers', but operates on a list of 'Predicates'.
partitionPredicates :: [Predicate err o] -> ([err], [o])
partitionPredicates = loop [] [] where
  loop errs oks ox = case ox of
    []            -> (errs, oks)
    PTrue  o : ox -> loop  errs        (oks++[o]) ox
    PError e : ox -> loop (errs++[e]) oks       ox
    PFalse   : ox -> loop  errs         oks       ox

----------------------------------------------------------------------------------------------------

-- | A stateful 'PredicateT' monad that remembers state data before 'Control.Monad.mplus',
-- 'Control.Monad.msum', or @('Control.Applicative.<|>')@ is evaluated, and if any branches result
-- in 'PFalse'ing (result in evaluting 'Control.Monad.mzero' or 'Control.Applicative.empty'), the
-- state before the branch is restored.
--
-- This monad takes advantage of the fact that the Haskell runtime's uses copy-on-write to
-- efficiently make many copys of the state when evaluating multiple branches. Obviously, if the
-- state data structure is heavily modified during evaluation of many branches, use of this monad
-- will consume quite a lot of memory. However if the state data is lightweight, for example
-- containing only integer indicies to an immutable array, 'PredicateStateT' can be a valuable, easy
-- to use interface for constructing logic and querying functions.
--
-- The original use case for this monad was to construct a parser that analyzed a stream of tokens,
-- where the tokens were contained in an immutable array. The state data was simply a pair: an
-- integer index to the array and the array itself, and the array was never modified, only the
-- integer index was modified. This allowed for very fast backtracking, even if a parsing branch
-- would backtrack after hundreds of tokens in the stream had been consumed. Every branch
-- essentially would push a single integer index on the stack, and backtracking would unwind the
-- stack, so memory useage remained nearly constant (apart from stack space usage) and the only
-- inefficiency was the computational time lost to backtracking.
--
-- 'PredicateStateT' instantiates all of the necessary monad transformer classes, so use of it is
-- done exclusively through interfaces like 'Control.Monad.State.get', 'Control.Monad.State.put',
-- 'Control.Monad.Except.throwError', 'Control.Monad.Except.catchError', 'Control.Monad.mzero',
-- 'Control.Monad.mplus', 'Control.Monad.msum', 'Control.Applicative.empty',
-- @('Control.Applicative.<|>')@, 'returnPredicate', 'predicate', and so on.
newtype PredicateStateT err st m o =
  PredicateStateT { runPredicateStateT :: st -> m (Predicate (err, st) (o, st)) }

type PredicateState err st = PredicateStateT err st Identity

instance Monad m => Functor (PredicateStateT err st m) where
  fmap f (PredicateStateT o) = PredicateStateT $ fmap (liftM (fmap (first f))) o

instance Monad m => Monad (PredicateStateT st err m) where
  return o = PredicateStateT $ \st -> return $ PTrue (o, st)
  (PredicateStateT o) >>= f = PredicateStateT $ \st -> o st >>= \o -> case o of
    PTrue (o, st) -> (\ (PredicateStateT o) -> o st) $ f o
    PFalse        -> return $ PFalse
    PError err    -> return $ PError err

instance Monad m => Applicative (PredicateStateT err st m) where { pure=return; (<*>) = ap; }

instance Monad m => MonadPlus (PredicateStateT err st m) where
  mzero = PredicateStateT $ const $ return PFalse
  mplus (PredicateStateT a) (PredicateStateT b) = PredicateStateT $ \st -> a st >>= \a -> case a of
    PTrue  o -> return $ PTrue o
    PFalse   -> b st
    PError e -> return $ PError e

instance Monad m => Alternative (PredicateStateT err st m) where { empty=mzero; (<|>) = mplus; }

instance Monad m => MonadError err (PredicateStateT err st m) where
  throwError err = PredicateStateT $ \st -> return $ PError (err, st)
  catchError (PredicateStateT try) catch =
    PredicateStateT $ \st -> try st >>= \o -> case o of
      PError (err, _) -> (\ (PredicateStateT o) -> o st) (catch err)
      PFalse          -> return PFalse
      PTrue   o       -> return $ PTrue o

instance Monad m => MonadState st (PredicateStateT err st m) where
  state f = PredicateStateT $ \st -> return $ PTrue $ f st

instance Monad m => PredicateClass err (PredicateStateT err st m) where
  returnPredicate (PredicateStateT o) = PredicateStateT $ \st -> o st >>= \o -> return $ case o of
    PTrue  (o,  st) -> PTrue (PTrue      o, st)
    PFalse          -> PTrue (PFalse, st)
    PError (err, _) -> PTrue (PError err, st)
  predicate p = PredicateStateT $ \st -> return $ case p of
    PError e -> PError (e, st)
    PFalse   -> PFalse
    PTrue  o -> PTrue (o, st)

instance MonadTrans (PredicateStateT err st) where
  lift f = PredicateStateT $ \st -> f >>= \o -> return (PTrue (o, st))

instance (Monad m, MonadIO m) => MonadIO (PredicateStateT err st m) where
  liftIO f = PredicateStateT $ \st -> liftIO f >>= \o -> return (PTrue (o, st))

-- | Like 'catchError' but lets you see what the value of the state data was at the time the error
-- was thrown.
catchErrorState
  :: Monad m
  => PredicateStateT err st m o
  -> ((err, st) -> PredicateStateT err st m o)
  -> PredicateStateT err st m o
catchErrorState (PredicateStateT try) catch = PredicateStateT $ \st -> try st >>= \o -> case o of
  PError e -> (\ (PredicateStateT o) -> o st) (catch e)
  PFalse   -> return PFalse
  PTrue  o -> return $ PTrue o

runPredicateState :: PredicateState err st o -> st -> Predicate (err, st) (o, st)
runPredicateState f = runIdentity . runPredicateStateT f

evalPredicateStateT :: Monad m => PredicateStateT err st m o -> st -> m (Predicate (err, st) o)
evalPredicateStateT f = liftM (fmap fst) . runPredicateStateT f

evalPredicateState :: PredicateState err st o -> st -> (Predicate (err, st) o)
evalPredicateState f = runIdentity . evalPredicateStateT f

execPredicateStateT :: Monad m => PredicateStateT err st m o -> st -> m (Predicate (err, st) st)
execPredicateStateT f = liftM (fmap snd) . runPredicateStateT f

execPredicateState :: PredicateState err st o -> st -> (Predicate (err, st) st)
execPredicateState f = runIdentity . execPredicateStateT f

----------------------------------------------------------------------------------------------------

-- | This will be defined in "Data.Monoid" starting from version 4.8 of the `base` library. Until
-- then, it is defined here. This defines a 'Data.Monoid.Monoid' for the
-- 'Control.Applicative.Alternative' class of functions, where 'Data.Monoid.mempty' is defined as
-- 'Control.Applicative.empty', and 'Data.Monad.mappend' is defined as
-- @('Control.Applicative.<|>')@.
newtype Alt f o = Alt { getAlt :: f o }
instance MonadPlus f => Monoid (Alt f o) where
  mempty = Alt mzero
  mappend (Alt a) (Alt b) = Alt $ mplus a b

