-- "src/Dao/Stack.hs"  provides the stack mechanism used for storing
-- local variables during evaluation.
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

module Dao.Stack where

import qualified Data.Map as M

import           Control.Monad
import           Control.Monad.Identity

newtype Stack key val = Stack { mapList :: [M.Map key val] } deriving Show

emptyStack :: Stack key val
emptyStack = Stack []

stackLookup :: Ord key => key -> Stack key val -> Maybe val
stackLookup key stack = foldl (\f -> mplus f . (M.lookup key)) Nothing (mapList stack)

-- | Always update in the top of the stack, regardless of whether the key being updated has been
-- defined at some lower level in the stack. If the stack is empty, the update function is evaluated
-- with 'Prelude.Nothing' and the result @a@ is returned, but the updated @val@ is disgarded.
stackUpdateTopM
  :: (Monad m, Ord key)
  => (Maybe val -> m (a, Maybe val)) -> key -> Stack key val -> m (a, Stack key val)
stackUpdateTopM updVal key (Stack stack) = case stack of
  []      -> updVal Nothing >>= \ (a, _) -> return (a, Stack [])
  s:stack -> updVal (M.lookup key s) >>= \ (a, o) -> return (a, Stack $ M.alter (const o) key s : stack)

-- | If the key does not exist, the update will occur in the top level of the stack. If the key does
-- exist, regardless of whether the key exists in the top or in some lower level, the value at that
-- key will be updated in the level in which it is defined. If the stack is empty, the update
-- function is evaluated with 'Prelude.Nothing' and the result @a@ is returned, but the updated
-- @val@ is disgarded.
stackUpdateM
  :: (Monad m, Ord key)
  => (Maybe val -> m (a, Maybe val)) -> key -> Stack key val -> m (a, Stack key val)
stackUpdateM updVal key (Stack stack) = loop [] stack where
  loop rx stack = case stack of
    []      -> atTop (reverse rx)
    s:stack -> case M.lookup key s of
      Just  o -> updVal (Just o) >>= \ (a, o) ->
        return (a, Stack $ reverse rx ++ M.alter (const o) key s : stack)
      Nothing -> loop (s:rx) stack
  atTop stack = case stack of
    []      -> updVal Nothing >>= \ (a, _) -> return (a, Stack [])
    s:stack -> updVal Nothing >>= \ (a, o) -> return (a, Stack $ M.alter (const o) key s : stack)

stackUpdate
  :: Ord key
  => (Maybe val -> (a, Maybe val))
  -> key -> Stack key val -> (a, Stack key val)
stackUpdate upd key = runIdentity . stackUpdateM (return . upd) key

stackUpdateTop
  :: Ord key
  => (Maybe val -> (a, Maybe val))
  -> key -> Stack key val -> (a, Stack key val)
stackUpdateTop upd key = runIdentity . stackUpdateTopM (return . upd) key

-- | Define or undefine a value at an address on the top tree in the stack.
stackDefine :: Ord key => key -> Maybe val -> Stack key val -> Stack key val
stackDefine key val = snd . stackUpdate (const ((), val)) key

stackPush :: Ord key => M.Map key val -> Stack key val -> Stack key val
stackPush init stack = stack{ mapList = init : mapList stack }

stackPop :: Ord key => Stack key val -> (Stack key val, M.Map key val)
stackPop stack =
  let mx = mapList stack
  in  if null mx then (stack, M.empty) else (stack{mapList=tail mx}, head mx)

