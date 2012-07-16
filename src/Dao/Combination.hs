-- "src/Dao/Combination.hs"  defines the 'CombinationT' monad which is
-- used to construct the parser for the Dao scripting language.
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

module Dao.Combination
  ( module Dao.Combination
  , module Control.Monad.Identity
  , module Control.Monad.State.Class
  )
  where

import           Dao.Object

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Data.Either

stateEither :: (Either Object a, st) -> (Either (Object, st) (a, st))
stateEither (a, st) = either (\a -> Left (a, st)) (\a -> Right (a, st)) a

eitherState :: (Either (Object, st) (a, st)) -> (Either Object a, st)
eitherState a = either (\ (a, st) -> (Left a, st)) (\ (a, st) -> (Right a, st)) a

leftState :: (Object, st) -> (Either Object a, st)
leftState (a, st) = (Left a, st)

rightState :: (a, st) -> (Either Object a, st)
rightState (a, st) = (Right a, st)

newtype CombinationT st m a =
  CombinationT { runCombinationT :: st -> m [(Either Object a, st)] }

type Combination st a = CombinationT st Identity a

combination :: (st -> [(Either Object a, st)]) -> Combination st a
combination fn = CombinationT (return . fn)

runCombination :: Combination st a -> st -> [(Either Object a, st)]
runCombination fn st = runIdentity (runCombinationT fn st)

eitherToCombination :: Monad m => Either Object a -> CombinationT st m a
eitherToCombination e = CombinationT (\st -> return [(e, st)])

eitherListToCombination :: Monad m => [Either Object a] -> CombinationT st m a
eitherListToCombination ex = CombinationT (\st -> return (map (\e -> (e, st)) ex))

combineWith
  :: Monad m
  => CombinationT st m a
  -> ([(Object, st)] -> [(a, st)] -> m [(Either Object a, st)])
  -> CombinationT st m a
combineWith fn comb = CombinationT $ \st ->
  runCombinationT fn st >>= uncurry comb . partitionEithers . map stateEither

ignoreFailed :: Monad m => CombinationT st m a -> CombinationT st m a
ignoreFailed fn = combineWith fn $ \fails goods -> return $
  if null goods then map leftState fails else map rightState goods

evalCombination :: Monad m => CombinationT st m a -> st -> m [Either Object a]
evalCombination fn st = runCombinationT (ignoreFailed fn) st >>= return . map fst

-- | Pass a 'Combination' and evluate it. If there any error messages occurred, replace all of them
-- with a single error message provided here.
failMsg :: Monad m => Object -> CombinationT st m a -> CombinationT st m a
failMsg msg fn = get >>= \st -> combineWith fn $ \fails goods ->
  return $ map rightState goods ++
    if null fails then [] else [(Left (OPair (msg, OList (map fst fails))), st)]

--  failMsg :: Object -> Combination st a -> Combination st a
--  failMsg msg fn = combineWith fn $ \fails goods -> return $ map rightState goods ++
--    if null fails then [] else flip map fails (\ (_, st) -> (Left msg, st))

-- | Fail with an 'Dao.Types.Object', rather than with a 'Prelude.Stirng' like the
-- 'Control.Monad.Monad' 'Control.Monad.fail' function.
failWith :: Monad m => Object -> CombinationT st m a
failWith obj = CombinationT (\st -> return [(Left obj, st)])

instance Monad m => Monad (CombinationT st m) where
  return a = CombinationT (\st -> return [(Right a, st)])
  CombinationT fa >>= mfa = CombinationT $ \st -> do
    ax <- fa st
    ax <- forM ax $ \ (a, st) -> case a of
      Left  e -> return [(Left  e, st)]
      Right a -> runCombinationT (mfa a) st
    return (concat ax)
  fail msg = CombinationT (\st -> return [(Left (OString (ustr msg)), st)])

instance Monad m => Functor (CombinationT st m) where
  fmap f fa = CombinationT $ \st -> do
    ax <- runCombinationT fa st
    return $ flip map ax $ \ (a, st) -> case a of
      Left  a -> (Left a, st)
      Right a -> (Right (f a), st)

instance Monad m => MonadPlus (CombinationT st m) where
  mzero = CombinationT (return . const [])
  mplus (CombinationT a) (CombinationT b) = CombinationT $ \st ->
    (a st >>= \ax -> b st >>= \bx -> return (ax++bx))

instance Monad m => MonadState st (CombinationT st m) where
  get = CombinationT (\st -> return [(Right st, st)])
  put st = CombinationT (\_ -> return [(Right (), st)])

instance MonadTrans (CombinationT st) where
  lift fn = CombinationT (\st -> fn >>= \a -> return [(Right a, st)])

-- | Takes two 'CombinationT's, @a@ and @b@. If @a@ evaluates to a 'Control.Monad.mzero' (which is a
-- completely null result, no returned values and no errors) then this method will evaluate to @b@.
-- If @a@ is not 'Control.Monad.mzero' then evaluate to @a@.
ifMZero :: Monad m => CombinationT st m a -> CombinationT st m a -> CombinationT st m a
ifMZero (CombinationT a) (CombinationT b) =
  CombinationT (\st -> a st >>= \ax -> if null ax then b st else return ax)

-- | Evaluates as the first function that evaluates as successful, ignoring failures. If everything
-- failed, then evaluates to the list of all failures, i.e. when you use the 'evalCombination'
-- function to evaluate the a 'Combination' where every possiblity has failed, the resultant will be
-- a list of 'Data.Either.Left' values.
first :: Monad m => [CombinationT st m a] -> CombinationT st m a
first ax = combineWith (msum ax) $ \fails goods -> return $
  if null goods then map leftState fails else [rightState (head goods)]

-- | The first parameter is a default value. Evaluate the given 'Combination', but if it fails it
-- will evaluate as the default value, so this function will never fail. *CAUTION*: might conflict
-- with 'Text.ParserCombinators.ReadP.option', so.
option :: Monad m => a -> CombinationT st m a -> CombinationT st m a
option a fn = first [fn, return a]

between :: Monad m => CombinationT st m before -> CombinationT st m after -> CombinationT st m a -> CombinationT st m a
between before after fn = void before >> fn >>= \a -> void after >> return a

-- | Similar to the @<++@ expression from "Text.ParserCombinators.ReadP", the expression @a <++ b@
-- is exactly equavalent to @'first [a, b]@, and @a ++> b@ is exactl equivalent to @'first' [b, a]@.
-- The "arrow" points to the expression it will try first, and away from the epxression it will try
-- second. Only the first successful parse is returned. The '+++' operator is exactly equivalent to
-- 'Control.Monad.mplus' for the 'Combination' monad, and will return all successful parsed values.
infixr 5 +++, <++, ++>
(<++) :: Monad m => CombinationT st m a -> CombinationT st m a -> CombinationT st m a
a <++ b = first [a, b]
(++>) :: Monad m => CombinationT st m a -> CombinationT st m a -> CombinationT st m a
a ++> b = first [b, a]
(+++) :: Monad m => CombinationT st m a -> CombinationT st m a -> CombinationT st m a
a +++ b = mplus a b

sepByEither1 :: Monad m => CombinationT st m a -> CombinationT st m sep -> CombinationT st m [Either sep a]
sepByEither1 fn sep = fn >>= loop . (:[]) . Right where
  loop ax = first [sep >>= \b -> fn >>= \a -> loop (ax++[Left b, Right a]), return ax]

sepBy1 :: Monad m => CombinationT st m a -> CombinationT st m sep -> CombinationT st m [a]
sepBy1 fn sep = fmap rights (sepByEither1 fn sep)

sepByEither :: Monad m => CombinationT st m a -> CombinationT st m sep -> CombinationT st m [Either sep a]
sepByEither fn sep = first [sepByEither1 fn sep, return []]

sepBy :: Monad m => CombinationT st m a -> CombinationT st m sep -> CombinationT st m [a]
sepBy fn sep = fmap rights (sepByEither fn sep)

many :: Monad m => CombinationT st m a -> CombinationT st m [a]
many fn = loop [] where { loop ax = first [fn >>= \a -> loop (ax++[a]), return ax] }

many1 :: Monad m => CombinationT st m a -> CombinationT st m [a]
many1 fn = fn >>= \a -> fmap (a:) (many fn)

