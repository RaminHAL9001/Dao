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

newtype Stack key val = Stack { mapList :: [T.Tree key val] }

emptyStack :: Stack key val
emptyStack = Stack []

stackLookup :: Ord key => [key] -> Stack key val -> Maybe val
stackLookup key stack = case mapList stack of
  []        -> Nothing
  (stack:_) -> T.lookup key stack

stackUpdate :: Ord key => [key] -> (Maybe val -> Maybe val) -> Stack key val -> Stack key val
stackUpdate key updVal stack =
  Stack
  { mapList = case mapList stack of
      []   -> []
      m:mx -> T.update key updVal m : mx
  }

-- | Define or undefine a value at an address on the top tree in the stack.
stackDefine :: Ord key => [key] -> Maybe val -> Stack key val -> Stack key val
stackDefine key val = stackUpdate key (const val)

stackPush :: Ord key => T.Tree key val -> Stack key val -> Stack key val
stackPush init stack = stack{ mapList = init : mapList stack }

stackPop :: Ord key => Stack key val -> (Stack key val, T.Tree key val)
stackPop stack =
  let mx = mapList stack
  in  if null mx then (stack, T.Void) else (stack{mapList=tail mx}, head mx)

