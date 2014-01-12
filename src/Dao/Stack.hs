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

import qualified Dao.Tree as T

import           Control.Monad
import           Control.Monad.Identity

newtype Stack key val = Stack { mapList :: [T.Tree key val] }

emptyStack :: Stack key val
emptyStack = Stack []

stackLookup :: Ord key => [key] -> Stack key val -> Maybe val
stackLookup key stack = foldl (\f -> mplus f . (T.lookup key)) Nothing (mapList stack)

stackUpdateM
  :: (Monad m, Ord key)
  => (Maybe val -> m (Maybe val)) -> [key] -> Stack key val -> m (Stack key val, Maybe val)
stackUpdateM updVal key stack = loop [] (mapList stack) where
  loop rx stack = case stack of
    []      -> atTop (reverse rx)
    s:stack -> case T.lookup key s of
      Just  o -> updVal (Just o) >>= \o -> return $ case o of
        Just  o -> (Stack (reverse rx ++ T.insert key o s : stack), Just  o)
        Nothing -> (Stack (reverse rx ++ T.delete key   s : stack), Nothing)
      Nothing -> loop (s:rx) stack
  atTop stack = case stack of
    []      -> return (Stack [], Nothing)
    s:stack -> updVal Nothing >>= \o -> return $ case o of
      Nothing -> (Stack (T.delete key   s : stack), Nothing)
      Just  o -> (Stack (T.insert key o s : stack), Just  o)

stackUpdate :: Ord key => (Maybe val -> Maybe val) -> [key] -> Stack key val -> (Stack key val, Maybe val)
stackUpdate upd key = runIdentity . stackUpdateM (return . upd) key

-- | Define or undefine a value at an address on the top tree in the stack.
stackDefine :: Ord key => [key] -> Maybe val -> Stack key val -> Stack key val
stackDefine key val = fst . stackUpdate (const val) key

stackPush :: Ord key => T.Tree key val -> Stack key val -> Stack key val
stackPush init stack = stack{ mapList = init : mapList stack }

stackPop :: Ord key => Stack key val -> (Stack key val, T.Tree key val)
stackPop stack =
  let mx = mapList stack
  in  if null mx then (stack, T.Void) else (stack{mapList=tail mx}, head mx)

