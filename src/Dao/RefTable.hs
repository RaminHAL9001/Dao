-- "src/Dao/RefTable.hs"  a data type which maps opaque reference
-- handles to 'Dynamic' objects.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
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

-- | This module is used to implement a kind of un-typed global state for the Dao runtime which can
-- store object of arbitrary type in a place that can be accessed anywhere. It is used for data
-- types like file handles which have their own unique identifiers and you don't need Dao to assign
-- it a new unique ID to it them for you, you can just index it by a hash of the the unique ID.
-- 
-- Every value stored in a table gets it's own MVar, so updating the value is thread safe, but it is
-- a bit memory intensive. So a 'RefTable' should only be used for things that really need to be
-- well protected, like UNIX file descriptors or sockets, ordinary file handles, credentials,
-- handles to access databases -- anything that needs to be allocated and released in order for it
-- to be used during runtime.
module Dao.RefTable
  ( RefMonad,
    RefNode, refNodeDestructor, refNodeValue, refTableNode,
    RefTable, newRefTable, initializeWithKey, destroyWithKey
  )
  where

import qualified Dao.HashMap as H

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad.Reader

----------------------------------------------------------------------------------------------------

type RefMonad key val a = ReaderT (RefTable key val) IO a

data RefNode a = RefNode{ refNodeDestructor :: IO (), refNodeValue :: MVar a }

refTableNode :: (a -> IO ()) -> a -> IO (RefNode a)
refTableNode destructor a = newMVar a >>= \mvar ->
  return $ RefNode{ refNodeDestructor=destructor a, refNodeValue=mvar }

newtype RefTable key a = RefTable { refTableMVar :: MVar (H.HashMap key (RefNode a)) }

newRefTable :: IO (RefTable key a)
newRefTable = RefTable <$> newMVar H.empty

initializeWithKey :: Ord key => a -> IO () -> H.Index key -> RefMonad key a (MVar a)
initializeWithKey o destructor key = do
  mvar <- H.hashLookup key <$> (asks refTableMVar >>= liftIO . readMVar)
  case mvar of
    Just node -> return (refNodeValue node)
    Nothing   -> do
      mvar <- liftIO $ newMVar o
      asks refTableMVar >>=
        liftIO . flip modifyMVar (\hmap -> return (H.hashInsert key (RefNode destructor mvar) hmap, mvar))

destroyWithKey :: Ord key => H.Index key -> RefMonad key a ()
destroyWithKey key = do
  mvar <- H.hashLookup key <$> (asks refTableMVar >>= liftIO . readMVar)
  case mvar of
    Nothing -> return ()
    Just  _ -> asks refTableMVar >>= liftIO . flip modifyMVar_ (return . H.hashDelete key)

