-- "src/Dao/Binary.hs"  declares the binary serializing monad.
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
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides an essential wrapper around the 'Data.Binary.Binary' monad which allows a
-- binary serializer to read data type tags in the byte stream and select the next parser to be used
-- by looking up the parser with data type tag.
module Dao.Binary where

import           Dao.String
import           Dao.Object

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader

import           Data.Monoid
import           Data.Function
import           Data.Typeable
import           Data.Dynamic
import           Data.Word
import           Data.Bits
import           Data.Array.IArray
import qualified Data.Map as M

import qualified Data.Binary     as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

----------------------------------------------------------------------------------------------------

type Byte = Word8

type CoderConfig = ObjIfcTable

lookupObjIfc :: (Functor m, Monad m) => TypeRep -> ReaderT CoderConfig m (Maybe T_haskell)
lookupObjIfc typ = fmap (M.lookup typ) ask

newtype Get a = Get { decoderToReader :: ReaderT CoderConfig B.Get a }
instance Functor Get where { fmap f (Get a) = Get (fmap f a) }
instance Monad Get where
  return = Get . return
  (Get a) >>= fn = Get (a >>= decoderToReader . fn)
instance MonadPlus Get where
  mzero = Get mzero
  mplus (Get a) (Get b) = Get (mplus a b)
instance Applicative Get where { pure=return; (<*>)=ap; }
instance Monoid a => Monoid (Get a) where
  mempty=return mempty
  mappend a b = a >>= \a -> b >>= \b -> return (mappend a b)

type Put = PutM ()
newtype PutM a = PutM { encoderToReader :: ReaderT CoderConfig B.PutM a }
instance Functor PutM where { fmap f (PutM a) = PutM (fmap f a) }
instance Monad PutM where
  return = PutM . return
  (PutM a) >>= fn = PutM (a >>= encoderToReader . fn)
instance Applicative PutM where { pure=return; (<*>)=ap; }
instance Monoid a => Monoid (PutM a) where
  mempty=return mempty
  mappend a b = a >>= \a -> b >>= \b -> return (mappend a b)

-- | A data type used to help instantiate the 'Dao.Binary.Binary' class.
data Serializer a = Serializer{ runGet :: Get a, runPut :: a -> PutM () }

class Binary a where
  get :: Get a
  get = runGet serializer
  put :: a -> PutM ()
  put = runPut serializer
  serializer :: Serializer a
  serializer = Serializer{runGet=get,runPut=put}

instance B.Binary a => Binary a where
  get = Get (lift B.get)
  put = PutM . lift . B.put

-- | Construct a 'Serializer' from a list of serializers, and each list item will be prefixed with a
-- byte in the range given.
bytePrefixTable :: String -> Byte -> Byte -> [Get a] -> Array Byte (Get a)
bytePrefixTable msg lo hi ser = accumArray (flip const) err (lo, hi) (zip [lo..hi] ser) where
  err = fail ("expecting "++msg)

