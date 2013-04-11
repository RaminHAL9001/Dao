-- "src/Dao/Struct.hs"  provides a class that can convert between
-- Hasekll "data" types and 'Dao.Tree.Tree's.
-- 
-- Copyright (C) 2008-2013  Ramin Honary.
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

module Dao.Struct where

import           Dao.String
import           Dao.Object
import           Dao.Tree
import           Dao.Predicate

import qualified Data.Map as M

import           Control.Monad
import           Control.Monad.State

class Structured a where
  dataToStruct :: a -> Tree Name Object
  structToData :: Tree Name Object -> a

type Update a = PTrans Object (State (Tree Name Object)) a

runUpdate :: Update a -> Tree Name Object -> (PValue Object a, Tree Name Object)
runUpdate = runState . runPTrans

instance MonadState (Tree Name Object) (PTrans Object (State (Tree Name Object)))

maybeToUpdate :: Maybe a -> Update a
maybeToUpdate = pvalue . maybeToBacktrack

-- | Return the value stored in the current node. Evaluates to 'Control.Monad.mzero' if the current
-- node is empty, so it can be used to check if an item exists at the current node as well.
this :: Update Object
this = get >>= maybeToUpdate . getLeaf

-- | Change the value stored in the current node.
change :: ModLeaf Object -> Update ()
change fn = modify (alterData fn)

-- | Applies a function only if there is an item at the current node.
mapThis :: (Object -> Object) -> Update ()
mapThis fn = change (\item -> fmap fn item)

-- | Step into a named branch and evaluate an 'Update' function on that node.
with :: [Name] -> Update a -> Update a
with nm fn =
  if null nm
    then  fn
    else
      case lookupNode [head nm] st of
        Nothing    -> mzero
        Just inner -> do
          (result, inner) <- fmap (runUpdate (with (tail nm) fn)) get
          put inner >> pvalue result

