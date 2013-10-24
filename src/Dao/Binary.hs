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
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module provides an essential wrapper around the 'Data.Binary.Binary' monad which allows a
-- binary serializer to read data type tags in the byte stream and select the next parser to be used
-- by looking up the parser with data type tag.
-- 
-- Dao's binary protocol is compact and efficient, with every fundamental data type prefixed with a
-- single byte of information. Integers of arbitrary length are stored using Variable Length Integer
-- (VLI) encoding. However the byte stream is not compressed, and there are no functions in this
-- module which facilitate this, it is up to you to do compression. Using algorithms like GZip or
-- BZip2 will almost certainly decrese the size of the byte stream as Dao's binary protocol makes
-- no attempt to reduce data entropy.
--
-- Arbitrary data types can be encoded as long as they instantiate 'Data.Typeable.Typeable' and
-- 'Dao.Object.ObjectInterface' and have been made available to the 'Dao.Object.Runtime' during
-- initialization of the Dao program. Each new type placed in the stream creates an integer tag in
-- an index with the 'Data.Typeable.TypeRep', and every item of the type that is placed after that
-- tag is prefixed with the integer index value. When decoding, the index of tags is constructed on
-- the fly as they are read from arbitrary points in the stream, and the index is used to select the
-- correct binary decoder from the 'Dao.Object.ObjectInterface' stored in the 'Dao.Object.Runtime'.
-- 
-- Of course, this module is not a full re-writing of "Data.Binary", it relies heavily on the
-- "Data.Binary" module, and provides a Dao-friendly wrapper around it.
module Dao.Binary where

import           Dao.String
import           Dao.Object

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Exception (assert)

import           Data.Monoid
import           Data.Function
import           Data.Typeable
import           Data.Dynamic
import           Data.Int
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
instance MonadPlus   Get where { mzero = Get mzero; mplus (Get a) (Get b) = Get (mplus a b); }
instance Applicative Get where { pure=return; (<*>)=ap; }
instance Alternative Get where { empty=mzero; (<|>)=mplus; }
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

-- | A data type used to help instantiate the 'Dao.Binary.Binary' class. Refer to the
-- 'fromDataBinary' function for more details.
data Serializer a = Serializer{ serializeGet :: Get a, serializePut :: a -> PutM () }

-- | Minimal complete definition is to either instantiate both 'get' and 'put', or to instnatiate
-- just 'serializer'. You can instantiate all three if you want but that may cause a lot of
-- confusion. Apart from 'serializer', it is identical to the 'Data.Binary.Binary' class, so please
-- refer to that module for more background information on how to use this one.
class Binary a where
  get :: Get a
  get = serializeGet serializer
  put :: a -> PutM ()
  put = serializePut serializer
  serializer :: Serializer a
  serializer = Serializer{serializeGet=get,serializePut=put}

-- | This is a polymorphic object that has been constructed using the instances of the canonical
-- 'Data.Binary.Binary'. This makes it possible to write your binary instances like so:
-- > import Data.Binary
-- > import Dao.Binary
-- > import MyObject -- exports data type MyObject which instantiates the canonical 'Data.Binary.Binary'.
-- > instance Dao.Binary.Binary MyObject where { serializer = fromDataBinary }
--
-- I cannot just use GHC's @UndecidableInstances@ feature to declare all type which instantiate the
-- canonical 'Data.Binary.Binary' to also instantiate my own 'Dao.Binary.Binary' because
-- I need my own versions of 'Data.Binary.get' and 'Data.Binary.put' for certain types like
-- 'Data.Maybe' and list types. So unfortunately, we are stuck declaring a new instance for every
-- data type that needs serialization.
fromDataBinary :: B.Binary a => Serializer a
fromDataBinary =
  Serializer
  { serializeGet = Get (lift B.get)
  , serializePut = PutM . lift . B.put
  }

lookAhead :: Get a -> Get a
lookAhead (Get fn) = Get (ask >>= lift . B.lookAhead . runReaderT fn)

putWord8    = PutM . lift . B.putWord8
putWord16be = PutM . lift . B.putWord16be
putWord16le = PutM . lift . B.putWord16le
putWord32be = PutM . lift . B.putWord32be
putWord32le = PutM . lift . B.putWord32le
putWord64be = PutM . lift . B.putWord64be
putWord64le = PutM . lift . B.putWord64le

getWord8    = Get  $ lift $ B.getWord8
getWord16be = Get  $ lift $ B.getWord16be
getWord16le = Get  $ lift $ B.getWord16le
getWord32be = Get  $ lift $ B.getWord32be
getWord32le = Get  $ lift $ B.getWord32le
getWord64be = Get  $ lift $ B.getWord64be
getWord64le = Get  $ lift $ B.getWord64le

-- | Look ahead one byte, if the byte is the number you are expecting drop the byte and evaluate the
-- given 'Get' function, otherwise backtrack.
tryWord8 :: Word8 -> Get a -> Get a
tryWord8 w fn = lookAhead getWord8 >>= guard . (w==) >> getWord8 >> fn

instance Binary Int8   where { serializer = fromDataBinary }
instance Binary Int16  where { serializer = fromDataBinary }
instance Binary Int32  where { serializer = fromDataBinary }
instance Binary Int64  where { serializer = fromDataBinary }
instance Binary Int    where { serializer = fromDataBinary }
instance Binary Word8  where { serializer = fromDataBinary }
instance Binary Word16 where { serializer = fromDataBinary }
instance Binary Word32 where { serializer = fromDataBinary }
instance Binary Word64 where { serializer = fromDataBinary }
instance Binary Word   where { serializer = fromDataBinary }
instance Binary ()     where { get = return (); put () = return (); }

newtype NullTerm = NullTerm () deriving (Eq, Ord)
nullTerm = NullTerm ()
instance Binary NullTerm where
  put (NullTerm ()) = putWord8 0x00
  get = tryWord8 0x00 $ return nullTerm

instance Binary a => Binary (Maybe a) where
  put o = maybe (return ()) put o
  get   = optional get

instance Binary a => Binary [a] where
  put o = mapM_ put o >> put nullTerm
  get   = mplus (many get >>= \o -> get >>= \ (NullTerm ()) -> return o) $
    fail "list not null terminated"

----------------------------------------------------------------------------------------------------

class (Ix i, Binary i, Binary a) => HasPrefixTable i a where { prefixTable :: PrefixTable i a }

-- | For data types with many constructors, especially enumerated types, it is effiecient if your
-- decoder performs a single look-ahead to retrieve an index, then use the index to lookup the next
-- parser in a table. This is a lookup table using 'Data.Array.IArray.Array' as the table which does
-- exactly that.
newtype PrefixTable i a = PrefixTable (Maybe (Array i (Get a)))

instance (Ix i, Binary i, Binary a) => Monoid (PrefixTable i a) where
  mempty = PrefixTable Nothing
  mappend (PrefixTable a) (PrefixTable b) = PrefixTable $ a >>= \a -> b >>= \b -> do
    let ((loA, hiA), (loB, hiB)) = (bounds    a, bounds    b)
    let ( lo       ,  hi       ) = (min loA loB, max hiA hiB)
    Just $ accumArray (flip mplus) mzero (lo, hi) (assocs a ++ assocs b) where

indexDecoderForTable :: Binary i => PrefixTable i a -> Get i
indexDecoderForTable _ = get

-- | Construct a 'Serializer' from a list of serializers, and each list item will be prefixed with a
-- byte in the range given. It is necesary for the data type to instantiate 'Data.Typeable.Typeable'
-- in order to 
mkPrefixTable :: (Ix i, Num i, Typeable a) => String -> i -> i -> [Get a] -> PrefixTable i a
mkPrefixTable msg lo' hi' ser = assert (0<len && len<=hi-lo) table where
  len   = fromIntegral (length ser)
  lo    = min lo' hi'
  hi    = max lo' hi'
  idxs  = takeWhile (<=hi) (iterate (+1) lo)
  table = PrefixTable $ Just $ accumArray (flip mplus) mzero (lo, hi) (zip idxs ser)

mkPrefixTableWord8 :: Typeable a => String -> Byte -> Byte -> [Get a] -> PrefixTable Byte a
mkPrefixTableWord8 msg lo' hi' ser = mkPrefixTable msg lo hi ser where
  lo = min lo' hi'
  hi = max lo' hi'

runPrefixTable :: (Ix i, Binary i, Typeable a) => PrefixTable i a -> Get a
runPrefixTable tab@(PrefixTable t) = flip (maybe mzero) t $ \decoderArray -> do
  prefix <- indexDecoderForTable tab
  guard $ inRange (bounds decoderArray) prefix
  decoderArray!prefix

prefixByte :: Byte -> Put -> Put
prefixByte w fn = putWord8 w >> fn

instance HasPrefixTable Byte Bool where
  prefixTable = mkPrefixTableWord8 "Prelude.Bool" 0x01 0x02 [return False, return True]

instance Binary Bool where
  put o = putWord8 (if o then 0x01 else 0x02)
  get   = runPrefixTable (prefixTable :: PrefixTable Byte Bool)

