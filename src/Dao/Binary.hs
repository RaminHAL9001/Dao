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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Dao.Runtime
import           Dao.String
import qualified Dao.Tree             as T

import           Control.Exception (assert)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import qualified Control.Monad.State  as S

import           Data.Monoid
import           Data.Function
import           Data.Typeable
import           Data.Dynamic
import           Data.Char
import           Data.Int
import           Data.Ratio
import           Data.Complex
import           Data.Word
import           Data.Bits
import           Data.Time
import           Data.Array.IArray
import qualified Data.Map             as M
import qualified Data.IntMap          as Im
import qualified Data.Set             as S
import qualified Data.IntSet          as Is
import qualified Data.ByteString.Lazy as Z

import           Data.Digest.SHA1     as SHA1
import qualified Data.Binary.IEEE754  as B
import qualified Data.ByteString      as B
import qualified Data.Binary          as B
import qualified Data.Binary.Get      as B
import qualified Data.Binary.Put      as B

----------------------------------------------------------------------------------------------------

type Byte = Word8
newtype GGet gtab a = GGet  { decoderToStateT :: S.StateT gtab B.Get a }
instance Functor (GGet gtab) where { fmap f (GGet a) = GGet (fmap f a) }
newtype GPutM gtab a = GPutM { encoderToStateT :: S.StateT gtab B.PutM a }
type GPut gtab = GPutM gtab ()

class HasCoderTable mtab exec where
  getEncoderForType :: TypeRep -> exec (Dynamic -> GPut mtab)
  getDecoderForType :: TypeRep -> exec (GGet mtab Dynamic)

-- | A data type used to help instantiate the 'Dao.Binary.Binary' class. Refer to the
-- 'fromDataBinary' function for more details.
data Serializer gtab a = Serializer{ serializeGet :: GGet gtab a, serializePut :: a -> GPutM gtab () }

-- | Minimal complete definition is to either instantiate both 'get' and 'put', or to instnatiate
-- just 'serializer'. You can instantiate all three if you want but that may cause a lot of
-- confusion. Apart from 'serializer', it is identical to the 'Data.Binary.Binary' class, so please
-- refer to that module for more background information on how to use this one.
class Binary a gtab where
  get :: GGet gtab a
  get = serializeGet serializer
  put :: a -> GPutM gtab ()
  put = serializePut serializer
  serializer :: Serializer gtab a
  serializer = Serializer{serializeGet=Dao.Binary.get,serializePut=Dao.Binary.put}

instance Monad (GGet gtab) where
  return = GGet . return
  (GGet a) >>= fn = GGet (a >>= decoderToStateT . fn)
instance MonadPlus   (GGet gtab) where { mzero = GGet mzero; mplus (GGet a) (GGet b) = GGet (mplus a b); }
instance Applicative (GGet gtab) where { pure=return; (<*>)=ap; }
instance Alternative (GGet gtab) where { empty=mzero; (<|>)=mplus; }
instance Monoid a => Monoid (GGet gtab a) where
  mempty=return mempty
  mappend a b = a >>= \a -> b >>= \b -> return (mappend a b)

instance Functor (GPutM gtab) where { fmap f (GPutM a) = GPutM (fmap f a) }
instance Monad (GPutM gtab) where
  return = GPutM . return
  (GPutM a) >>= fn = GPutM (a >>= encoderToStateT . fn)
instance Applicative (GPutM gtab) where { pure=return; (<*>)=ap; }
instance Monoid a => Monoid (GPutM gtab a) where
  mempty=return mempty
  mappend a b = a >>= \a -> b >>= \b -> return (mappend a b)

----------------------------------------------------------------------------------------------------

class (Ix i, Binary i gtab, Binary a gtab) => HasPrefixTable a i gtab where { prefixTable :: PrefixTable gtab i a }

-- | For data types with many constructors, especially enumerated types, it is effiecient if your
-- decoder performs a single look-ahead to retrieve an index, then use the index to lookup the next
-- parser in a table. This is a lookup table using 'Data.Array.IArray.Array' as the table which does
-- exactly that.
newtype PrefixTable gtab i a = PrefixTable (Maybe (Array i (GGet gtab a)))

instance Ix i => Functor (PrefixTable gtab i) where
  fmap f (PrefixTable t) = PrefixTable (fmap (amap (fmap f)) t)
instance (Ix i, Binary a gtab) => Monoid (PrefixTable gtab i a) where
  mempty = PrefixTable Nothing
  mappend (PrefixTable a) (PrefixTable b) = PrefixTable $ a >>= \a -> b >>= \b -> do
    let ((loA, hiA), (loB, hiB)) = (bounds    a, bounds    b)
    let ( lo       ,  hi       ) = (min loA loB, max hiA hiB)
    Just $ accumArray (flip mplus) mzero (lo, hi) (assocs a ++ assocs b) where

indexDecoderForTable :: Binary i gtab => PrefixTable gtab i a -> GGet gtab i
indexDecoderForTable _ = get

-- | Construct a 'Serializer' from a list of serializers, and each list item will be prefixed with a
-- byte in the range given. It is necesary for the data type to instantiate 'Data.Typeable.Typeable'
-- in order to 
mkPrefixTable :: (Ix i, Num i) => String -> i -> i -> [GGet gtab a] -> PrefixTable gtab i a
mkPrefixTable msg lo' hi' ser = assert (0<len && len<=hi-lo) table where
  len   = fromIntegral (length ser)
  lo    = min lo' hi'
  hi    = max lo' hi'
  idxs  = takeWhile (<=hi) (iterate (+1) lo)
  table = PrefixTable $ Just $ accumArray (flip mplus) mzero (lo, hi) (zip idxs ser)

mkPrefixTableWord8 :: String -> Byte -> Byte -> [GGet gtab a] -> PrefixTable gtab Byte a
mkPrefixTableWord8 msg lo' hi' ser = mkPrefixTable msg lo hi ser where
  lo = min lo' hi'
  hi = max lo' hi'

runPrefixTable :: (Ix i, Binary i gtab) => PrefixTable gtab i a -> GGet gtab a
runPrefixTable tab@(PrefixTable t) = flip (maybe mzero) t $ \decoderArray -> do
  prefix <- indexDecoderForTable tab
  guard $ inRange (bounds decoderArray) prefix
  decoderArray!prefix

word8PrefixTable :: HasPrefixTable a Byte gtab => GGet gtab a
word8PrefixTable = runPrefixTable (prefixTable :: HasPrefixTable a Byte gtab => PrefixTable gtab Byte a)

prefixByte :: Byte -> GPut gtab -> GPut gtab
prefixByte w fn = putWord8 w >> fn

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
fromDataBinary :: B.Binary a => Serializer gtab a
fromDataBinary =
  Serializer
  { serializeGet = dataBinaryGet B.get
  , serializePut = dataBinaryPut . B.put
  }

dataBinaryPut :: B.PutM a -> GPutM gtab a
dataBinaryPut = GPutM . lift

dataBinaryGet :: B.Get a -> GGet gtab a
dataBinaryGet = GGet . lift

lookAhead :: GGet gtab a -> GGet gtab a
lookAhead (GGet fn) = GGet (S.get >>= lift . B.lookAhead . S.evalStateT fn)

putWord8    = GPutM . lift . B.putWord8
putWord16be = GPutM . lift . B.putWord16be
putWord16le = GPutM . lift . B.putWord16le
putWord32be = GPutM . lift . B.putWord32be
putWord32le = GPutM . lift . B.putWord32le
putWord64be = GPutM . lift . B.putWord64be
putWord64le = GPutM . lift . B.putWord64le

getWord8    = GGet  $ lift $ B.getWord8
getWord16be = GGet  $ lift $ B.getWord16be
getWord16le = GGet  $ lift $ B.getWord16le
getWord32be = GGet  $ lift $ B.getWord32be
getWord32le = GGet  $ lift $ B.getWord32le
getWord64be = GGet  $ lift $ B.getWord64be
getWord64le = GGet  $ lift $ B.getWord64le

putVLInt :: (Integral a, Bits a) => a -> GPut gtab
putVLInt = dataBinaryPut . Dao.String.putVLInt

getFromVLInt :: (Integral a, Bits a) => GGet gtab a
getFromVLInt = dataBinaryGet Dao.String.getFromVLInt

putByteString :: B.ByteString -> GPut gtab
putByteString = dataBinaryPut . B.putByteString

putLazyByteString :: Z.ByteString -> GPut gtab
putLazyByteString = dataBinaryPut . B.putLazyByteString

getByteString :: Int -> GGet gtab B.ByteString
getByteString = dataBinaryGet . B.getByteString

getLazyByteString :: Int64 -> GGet gtab Z.ByteString
getLazyByteString = dataBinaryGet . B.getLazyByteString

-- | Look ahead one byte, if the byte is the number you are expecting drop the byte and evaluate the
-- given 'GGet' function, otherwise backtrack.
tryWord8 :: Word8 -> GGet gtab a -> GGet gtab a
tryWord8 w fn = lookAhead getWord8 >>= guard . (w==) >> getWord8 >> fn

instance Binary Int8   gtab  where
  put = putWord8 . fromIntegral
  get = fmap fromIntegral getWord8
instance Binary Int16  gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Int32  gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Int64  gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Int    gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Word8  gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Word16 gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Word32 gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Word64 gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Word   gtab  where { put = Dao.Binary.putVLInt; get = Dao.Binary.getFromVLInt }
instance Binary Float  gtab  where
  put = dataBinaryPut . B.putFloat32be
  get = dataBinaryGet B.getFloat32be
instance Binary Double gtab  where
  put = dataBinaryPut . B.putFloat64be
  get = dataBinaryGet B.getFloat64be

instance (Num a, Integral a, Binary a gtab) => Binary (Ratio a) gtab where
  put o = let p = dataBinaryPut . putVLInteger . fromIntegral in p (numerator o) >> p (denominator o)
  get = let g = fmap fromIntegral (dataBinaryGet getVLInteger) in pure (%) <*> g <*> g

instance Binary Integer gtab where
  put = dataBinaryPut . putVLInteger
  get = dataBinaryGet getVLInteger

instance Binary Char gtab where
  put = Dao.Binary.putVLInt . ord
  get = fmap chr Dao.Binary.getFromVLInt

instance Binary (Complex Float) gtab where
  put o = put (realPart o) >> put (imagPart o)
  get = pure (:+) <*> get <*> get

instance Binary (Complex Double) gtab where
  put o = put (realPart o) >> put (imagPart o)
  get = pure (:+) <*> get <*> get

instance Binary UTCTime gtab where { serializer = fromDataBinary }
instance B.Binary UTCTime where
  put t = do
    B.put (toModifiedJulianDay (utctDay t))
    B.put (toRational (utctDayTime t))
  get = do
    d <- fmap ModifiedJulianDay B.get
    t <- fmap fromRational B.get
    return (UTCTime{ utctDay = d, utctDayTime = t })

instance Binary   NominalDiffTime gtab where { serializer = fromDataBinary }
instance B.Binary NominalDiffTime where
  put t = B.put (toRational t)
  get = fmap fromRational B.get

instance Binary B.ByteString gtab where
  put o = Dao.Binary.putVLInt (B.length o) >> Dao.Binary.putByteString o
  get = Dao.Binary.getFromVLInt >>= Dao.Binary.getByteString

instance Binary Z.ByteString gtab where
  put o = Dao.Binary.putVLInt (Z.length o) >> Dao.Binary.putLazyByteString o
  get = Dao.Binary.getFromVLInt >>= Dao.Binary.getLazyByteString

instance (Binary a t, Binary b t) => Binary (a, b) t where
  put (a, b) = put a >> put b
  get = pure (,) <*> get <*> get
instance (Binary a t, Binary b t, Binary c t) => Binary (a, b, c) t where
  put (a, b, c) = put a >> put b >> put c
  get = pure (,,) <*> get <*> get <*> get
instance (Binary a t, Binary b t, Binary c t, Binary d t) => Binary (a, b, c, d) t where
  put (a, b, c, d) = put a >> put b >> put c >> put d
  get = pure (,,,) <*> get <*> get <*> get <*> get
instance (Binary a t, Binary b t, Binary c t, Binary d t, Binary e t) => Binary (a, b, c, d, e) t where
  put (a, b, c, d, e) = put a >> put b >> put c >> put d >> put e
  get = pure (,,,,) <*> get <*> get <*> get <*> get <*> get
instance (Binary a t, Binary b t, Binary c t, Binary d t, Binary e t, Binary f t) => Binary (a, b, c, d, e, f) t where
  put (a, b, c, d, e, f) = put a >> put b >> put c >> put d >> put e >> put f
  get = pure (,,,,,) <*> get <*> get <*> get <*> get <*> get <*> get
instance (Binary a t, Binary b t, Binary c t, Binary d t, Binary e t, Binary f t, Binary g t) => Binary (a, b, c, d, e, f, g) t where
  put (a, b, c, d, e, f, g) = put a >> put b >> put c >> put d >> put e >> put f >> put g
  get = pure (,,,,,,) <*> get <*> get <*> get <*> get <*> get <*> get <*> get
instance (Binary a t, Binary b t, Binary c t, Binary d t, Binary e t, Binary f t, Binary g t, Binary h t) => Binary (a, b, c, d, e, f, g, h) t where
  put (a, b, c, d, e, f, g, h) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
  get = pure (,,,,,,,) <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

instance (Ord i, Binary i gtab, Binary a gtab) => Binary (M.Map   i a) gtab where { put = put . M.assocs ; get = M.fromList  <$> get; }
instance (                      Binary a gtab) => Binary (Im.IntMap a) gtab where { put = put . Im.assocs; get = Im.fromList <$> get; }
instance (Ord a,                Binary a gtab) => Binary (S.Set     a) gtab where { put = put . S.elems  ; get = S.fromList  <$> get; }
instance                                          Binary (Is.IntSet  ) gtab where { put = put . Is.elems ; get = Is.fromList <$> get; }

instance (Eq p, Ord p, Binary p gtab, Binary a gtab) => Binary (T.Tree p a) gtab where
  put t = case t of
    T.Void           -> prefixByte 0x03 $ return ()
    T.Leaf       a   -> prefixByte 0x04 $ put a
    T.Branch       t -> prefixByte 0x05 $ put t
    T.LeafBranch a t -> prefixByte 0x06 $ put a >> put t where
  get = word8PrefixTable

instance (Eq p, Ord p, Binary p gtab, Binary a gtab) => HasPrefixTable (T.Tree p a) Byte gtab where
  prefixTable = mkPrefixTableWord8 "Dao.Tree.Tree" 0x03 0x06 $
    [ return T.Void
    , T.Leaf   <$> get
    , T.Branch <$> get
    , pure T.LeafBranch <*> get <*> get
    ]

instance Binary () t where { get = return (); put () = return (); }
newtype NullTerm = NullTerm () deriving (Eq, Ord)
nullTerm = NullTerm ()
instance Binary NullTerm t where
  put (NullTerm ()) = putWord8 0x00
  get = tryWord8 0x00 $ return nullTerm

instance Binary a t => Binary (Maybe a) t where
  put o = maybe (return ()) put o
  get   = optional get

instance Binary a t => Binary [a] t where
  put o = mapM_ put o >> put nullTerm
  get   = mplus (many get >>= \o -> get >>= \ (NullTerm ()) -> return o) $
    fail "list not null terminated"

instance HasPrefixTable Bool Byte gtab where
  prefixTable = mkPrefixTableWord8 "Prelude.Bool" 0x01 0x02 [return False, return True]

instance Binary Bool gtab where
  put o = putWord8 (if o then 0x01 else 0x02)
  get   = word8PrefixTable

