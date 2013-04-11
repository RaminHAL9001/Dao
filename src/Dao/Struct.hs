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

import           Prelude hiding (lookup)

import           Dao.String
import           Dao.Object
import           Dao.Tree
import           Dao.Predicate

import qualified Data.Map as M

import           Control.Monad
import           Control.Monad.State

class Structured a where
  dataToStruct :: a -> Tree Name Object
  structToData :: Tree Name Object -> PValue Object a

type Update a = PTrans Object (State (Tree Name Object)) a

runUpdate :: Update a -> Tree Name Object -> (PValue Object a, Tree Name Object)
runUpdate = runState . runPTrans

evalUpdate :: Update a -> Tree Name Object -> PValue Object a
evalUpdate fn tree = fst (runUpdate fn tree)

-- | Update a data type in the 'Structured' class using an 'Update' monadic function.
onStruct :: Structured a => Update ig -> a -> PValue Object a
onStruct ufn a = (fst . runUpdate (ufn>>get)) (dataToStruct a) >>= structToData

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

-- | Step into a named branch and evaluate an 'Update' function on that node, if 'Prelude.True' is
-- given as the first parameter, then a non existent branch is forcefully updated. If
-- 'Prelude.False' is given as the first parameter then you can choose to evaluate to
-- 'Control.Monad.mzero' or a failure. If 'Nothing' is given as the second parameter, stepping into
-- a non-existent branch will evaluate to 'Control.Monad.mzero'. If a
-- @'Data.Maybe.Just' errorMessage@  is given as the second parameter, the error message is used to
-- evaluate to a failure.
alter :: Bool -> Maybe String -> [Name] -> Update a -> Update a
alter force msg nm doUpdate = loop [] nm doUpdate where
  loop ref nm doUpdate = case nm of
    []   -> doUpdate
    n:nm -> do
      st <- get
      case lookupNode [n] st of
        Nothing -> case msg of
          Nothing  -> 
            if force
              then  do
                modify (alterBranch (flip mplus (Just (M.singleton n Void))))
                loop (ref++[n]) nm doUpdate
              else  mzero
          Just msg -> pvalue $ PFail (ORef (GlobalRef ref)) (ustr msg)
        Just st -> do
          (result, st) <- fmap (runUpdate $ loop (ref++[n]) nm doUpdate) get
          put st >> pvalue result

-- | Modify or write a new a data structure at a given address using the given 'Update' function.
with :: [Name] -> Update a -> Update a
with = Dao.Struct.alter True Nothing

-- | Modify or write a new data structure at a given address using the given 'Update' function only
-- if there is already a data structure at that address, or fail with a given error message if there
-- is nothing at that address. The address requested will be included in resulting
-- 'Dao.Predicate.PFail' data.
must :: String -> [Name] -> Update a -> Update a
must msg = Dao.Struct.alter False (Just msg)

-- | Modify a data structure ata given address using the given 'Update' function only if there is
-- already a data structure at that address, or do nothing if there is nothing at that address.
check :: [Name] -> Update a -> Update a
check = Dao.Struct.alter False Nothing

-- | Lookup an 'Dao.Object.Object' value at a given address.
lookup :: [Name] -> Update Object
lookup nm = get >>= maybeToUpdate . Dao.Tree.lookup nm

lookupStrs :: [String] -> Update Object
lookupStrs = Dao.Struct.lookup . map ustr

-- | If a given 'Dao.Object.Object' is a @"struct"@ type of object, which is constrcuted by
-- 'Dao.Object.OTree', then return the tree in this object, otherwise evaluate to failure. The
-- failure message is appended to the string @"was expecting structured data to construct "@
asNode :: String -> Object -> Update (Tree Name Object)
asNode msg o = case o of
  OTree o -> return o
  _       -> pvalue $ PFail o $ ustr $ "was expecting structured data to construct "++msg

-- | If a given 'Dao.Object.Object is a 'Dao.Object.OList' object, return the list data, otherwise
-- fail with a message.
asList :: String -> Object -> Update [Object]
asList msg o = case o of
  OList o -> return o
  _       -> pvalue $ PFail o $ ustr $ "was expecting list data to construct "++msg

-- | If a given 'Dao.Object.Object is a 'Dao.Object.OString' object, return the string data, otherwise
-- fail with a message.
asString :: String -> Object -> Update UStr
asString msg o = case o of
  OString o -> return o
  _         -> pvalue $ PFail o $ ustr $ "was expecting a string to construct "++msg

