-- "src/Dao/Binary.hs"  declares the binary serializing monad.
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
-- 'Dao.Interpreter.ObjectInterface' and have been made available to the 'Dao.Interpreter.Runtime' during
-- initialization of the Dao program. Each new type placed in the stream creates an integer tag in
-- an index with the 'Data.Typeable.TypeRep', and every item of the type that is placed after that
-- tag is prefixed with the integer index value. When decoding, the index of tags is constructed on
-- the fly as they are read from arbitrary points in the stream, and the index is used to select the
-- correct binary decoder from the 'Dao.Interpreter.ObjectInterface' stored in the 'Dao.Interpreter.Runtime'.
-- 
-- Of course, this module is not a full re-writing of "Data.Binary", it relies heavily on the
-- "Data.Binary" module, and provides a Dao-friendly wrapper around it.
module Dao.Binary where

import           Dao.String
import qualified Dao.Tree             as T
import           Dao.Token
import           Dao.Predicate

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import qualified Control.Monad.State  as S

import           Data.Monoid
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
type InStreamID = Word32
type ByteOffset = B.ByteOffset

-- | A data type used to help instantiate the 'Dao.Binary.Binary' class. Refer to the
-- 'fromDataBinary' function for more details.
data Serializer mtab a = Serializer{ serializeGet :: GGet mtab a, serializePut :: a -> GPutM mtab () }

-- | Minimal complete definition is to either instantiate both 'get' and 'put', or to instnatiate
-- just 'serializer'. You can instantiate all three if you want but that may cause a lot of
-- confusion. Apart from 'serializer', it is identical to the 'Data.Binary.Binary' class, so please
-- refer to that module for more background information on how to use this one.
class Binary a mtab where
  get :: GGet mtab a
  get = serializeGet serializer
  put :: a -> GPutM mtab ()
  put = serializePut serializer
  serializer :: Serializer mtab a
  serializer = Serializer{serializeGet=Dao.Binary.get,serializePut=Dao.Binary.put}

class HasCoderTable mtab where
  getEncoderForType :: UStr -> mtab -> Maybe (Dynamic -> GPut mtab)
  getDecoderForType :: UStr -> mtab -> Maybe (GGet mtab Dynamic)

-- | To evaluate a 'GPut' or 'GGet' function without providing any coder table, simply pass @()@.
instance HasCoderTable () where
  getEncoderForType _ _ = Nothing
  getDecoderForType _ _ = Nothing

data EncodeIndex mtab
  = EncodeIndex
    { indexCounter :: InStreamID
    , encodeIndex  :: M.Map UStr InStreamID
    , encMTabRef   :: mtab
    }

data DecodeIndex mtab
  = DecodeIndex
    { decodeIndex  :: M.Map InStreamID UStr
    , decMTabRef   :: mtab
    }

newtype GPutM mtab a = PutM{ encoderToStateT :: S.StateT (EncodeIndex mtab) B.PutM a }
type GPut mtab = GPutM mtab ()

data GGetErr = GetErr { gGetErrOffset :: ByteOffset, gGetErrMsg :: UStr }
instance Show GGetErr where { show (GetErr ofst msg) = "(offset="++show ofst++") "++uchars msg }

newtype GGet mtab a = Get{ decoderToStateT :: PredicateT GGetErr (S.StateT (DecodeIndex mtab) B.Get) a }

instance Functor (GPutM mtab) where { fmap f (PutM a) = PutM (fmap f a) }
instance Monad (GPutM mtab) where
  return = PutM . return
  (PutM a) >>= fn = PutM (a >>= encoderToStateT . fn)
  fail = PutM . fail
instance Applicative (GPutM mtab) where { pure=return; (<*>)=ap; }
instance Monoid a => Monoid (GPutM mtab a) where
  mempty=return mempty
  mappend a b = a >>= \a -> b >>= \b -> return (mappend a b)
instance HasCoderTable mtab => S.MonadState (EncodeIndex mtab) (GPutM mtab) where
  state fn = PutM (S.state fn)

instance Functor (GGet mtab) where { fmap f (Get a) = Get (fmap f a) }
instance Monad (GGet mtab) where
  return = Get . return
  (Get a) >>= fn = Get (a >>= decoderToStateT . fn)
  Get a >> Get b = Get (a >> b)
  fail msg = bytesRead >>= \ofst -> Get (throwError (GetErr ofst (toUStr msg)))
instance MonadPlus   (GGet mtab) where { mzero = Get mzero; mplus (Get a) (Get b) = Get (mplus a b); }
instance Applicative (GGet mtab) where { pure=return; (<*>)=ap;    }
instance Alternative (GGet mtab) where { empty=mzero; (<|>)=mplus; }
instance Monoid a => Monoid (GGet mtab a) where
  mempty=return mempty
  mappend a b = a >>= \a -> b >>= \b -> return (mappend a b)
instance S.MonadState (DecodeIndex mtab) (GGet mtab) where
  state = Get . lift . S.state
instance MonadError GGetErr (GGet mtab) where
  throwError = Get . throwError
  catchError (Get fn) catch = Get (catchError fn (decoderToStateT . catch))

-- | This class only exists to provide the the function 'getCoderTable' with the exact same function
-- in both the 'GPutM' and 'GGet' monads, rather than having a separate function for each monad.
class HasCoderTable mtab => ProvidesCoderTable m mtab where { getCoderTable :: m mtab }
instance HasCoderTable mtab => ProvidesCoderTable (GPutM mtab) mtab where { getCoderTable = S.gets encMTabRef }
instance HasCoderTable mtab => ProvidesCoderTable (GGet  mtab) mtab where { getCoderTable = S.gets decMTabRef }

data InStreamIndex = InStreamIndex{ inStreamIndexID :: InStreamID, inStreamIndexLabel :: UStr }
  deriving (Eq, Ord, Show)
instance HasCoderTable mtab => Binary InStreamIndex mtab where
  put (InStreamIndex a b) = prefixByte 0x01 $ put a >> put b
  get = tryWord8 0x01 $ pure InStreamIndex <*> get <*> get

-- | Find the 'Dao.String.UStr' that was associated with this 'InStreamID' when the byte stream was
-- constructed when 'newInStreamID' was called.
decodeIndexLookup :: HasCoderTable mtab => InStreamID -> GGet mtab (Maybe UStr)
decodeIndexLookup tid = M.lookup tid <$> S.gets decodeIndex

-- | Given an 'Data.Int.Int64' length value, compute how many VLI bytes of hash code should be
-- necessary to for a byte stream of that length, and return an 'Data.Word.Word64' value trimmed to
-- that byte length.
trimIntegerHash :: Int64 -> Integer -> (Int, Integer)
trimIntegerHash i h = snd $ head $ dropWhile ((i<) . fst) $
  map (\x -> (2^(8+4*(fromIntegral x :: Int)), (x, h .&. (2^(7*x)-1)))) [0..14::Int]

-- | A trimmed hash is an hash produced by SHA1 along with the length of the original data. However
-- it instantiates 'Prelude.Eq', and 'Binary' in such a way that only a maximum of 5 bytes of the
-- hash value are ever stored and used for verification. This is to conserve space in a byte stream
-- when writing smaller chunks of data. For example, it is not necessary to store all 20 bytes of
-- the hash code when the data you are storing or reading is itself 20 bytes long.
data TrimmedHash = TrimmedHash Int64 Integer deriving (Eq, Ord)
instance Binary TrimmedHash mtab where
  put (TrimmedHash i h0) =
    let (len, h) = trimIntegerHash i h0
    in  if len>0 then putPosIntegral h else return ()
  get = TrimmedHash (error "TrimmedHash length not set") <$> getPosIntegral

-- | Create a 'TrimmedHash' a list of bytes as the second parameter and the maximum length of the
-- list as the first parameter.
trimmedHash :: Z.ByteString -> TrimmedHash
trimmedHash blk = mkTrimmedHash blk where
  len = Z.length blk
  mkTrimmedHash = TrimmedHash len . snd . trimIntegerHash len . SHA1.toInteger . hash .
    map snd . takeWhile ((>0) . fst) . zip (iterate (\x->x-1) len) . Z.unpack

-- not for export
setTrimmedHashLength :: Int64 -> TrimmedHash -> TrimmedHash
setTrimmedHashLength i (TrimmedHash _ h) = TrimmedHash i h

trimmedHashVerify :: Z.ByteString -> TrimmedHash -> Bool
trimmedHashVerify b = (trimmedHash b ==)

-- | A lazy block stream that wraps up a 'Data.ByteString.Lazy.ByteString' in a data type that
-- instantiates 'Binary' in such a way that the bytes are written lazily in chunks of 1 megabyte
-- blocks with checksums, providing a protocol that can encode and decode arbitrarily large data
-- without interfearing with the protocol used by this module.
newtype BlockStream1M = BlockStream1M { block1MStreamToByteString :: Z.ByteString }
instance Binary BlockStream1M mtab where
  put =
    mapM_ (\ blk -> put blk >> put (trimmedHash blk)
          ) . fix (\ loop blk ->
                      let (a,b) = Z.splitAt (2^(20::Int)) blk in a : if Z.null b then [] else loop b
                  ) . block1MStreamToByteString
  get = (BlockStream1M . Z.concat) <$> loop [] where
    loop bx = get >>= \b -> get >>= \cksum ->
      if trimmedHashVerify b (setTrimmedHashLength (Z.length b) cksum)
        then loop (bx++[b])
        else fail "bad checksum"

-- | If the type signature in the given 'Dao.String.UStr' already has an associated type ID in the
-- encoder table, the existing ID is returned rather than creating a new one, and nothing changes.
-- If a new ID is created, the 'Dao.String.UStr' is paired with the new ID and written to the byte
-- stream.
newInStreamID :: HasCoderTable mtab => UStr -> GPutM mtab InStreamID
newInStreamID typ = S.get >>= \st -> let idx = encodeIndex st in case M.lookup typ idx of
  Nothing  -> do
    let nextID = indexCounter st + 1
    S.put $ st{indexCounter=nextID, encodeIndex=M.insert typ nextID idx}
    put $ InStreamIndex{inStreamIndexID=nextID, inStreamIndexLabel=typ}
    return nextID
  Just tid -> return tid

-- | When decoding a byte stream, it is up to you to check for the 'InStreamIndex'ies that are
-- scattered throughout. To do this, 'newInStreamID' is only called after a special escape byte
-- prefix is seen, for example, the byte prefix used when the 'Dao.Interpreter.OHaskell' constructor is
-- to be encoded. Once this prefix is decoded you should call 'newInStreamID'. That way, when you
-- are decoding the byte stream, you will know that 'updateTypes' must be called whenever you decode
-- the byte prefix for 'Dao.Interpreter.OHaskell'.
-- 
-- This function simply checks if a 'InStreamIndex' exists at the current location in the byte
-- stream. If it does not exist, this function simply returns and does nothing. If it does exist,
-- the data is pulled out of the stream and the index is updated.
updateTypes :: HasCoderTable mtab => GGet mtab ()
updateTypes = tryWord8 0x01 $ do
  (InStreamIndex tid label) <- get
  S.modify $ \st -> st{decodeIndex = M.insert tid label (decodeIndex st)}

runPut :: HasCoderTable mtab => mtab -> GPut mtab -> Z.ByteString
runPut mtab fn = B.runPut $ S.evalStateT (encoderToStateT fn) $
  EncodeIndex{indexCounter=1, encodeIndex=mempty, encMTabRef=mtab}

runGet :: HasCoderTable mtab => mtab -> GGet mtab a -> Z.ByteString -> Predicate GGetErr a
runGet mtab fn = B.runGet $ S.evalStateT (runPredicateT $ decoderToStateT fn) $
  DecodeIndex{decodeIndex=mempty, decMTabRef=mtab}

encode :: (HasCoderTable mtab, Binary a mtab) => mtab -> a -> Z.ByteString
encode mtab = runPut mtab . put

decode :: (HasCoderTable mtab, Binary a mtab) => mtab -> Z.ByteString -> Predicate GGetErr a
decode mtab = runGet mtab get

encodeFile :: (HasCoderTable mtab, Binary a mtab) => mtab -> FilePath -> a -> IO ()
encodeFile mtab path = Z.writeFile path . encode mtab

decodeFile :: (HasCoderTable mtab, Binary a mtab) => mtab -> FilePath -> IO (Predicate GGetErr a)
decodeFile mtab path = decode mtab <$> Z.readFile path

putWithBlockStream1M :: HasCoderTable mtab => GPut mtab -> GPut mtab
putWithBlockStream1M fn = getCoderTable >>= \mtab -> put $ BlockStream1M $ runPut mtab fn

getWithBlockStream1M :: HasCoderTable mtab => GGet mtab a -> GGet mtab a
getWithBlockStream1M fn = do
  mtab <- getCoderTable
  (BlockStream1M bs1m) <- get
  Get (predicate (runGet mtab fn bs1m))

----------------------------------------------------------------------------------------------------

class (Ix i, Binary i mtab, Binary a mtab) => HasPrefixTable a i mtab where { prefixTable :: PrefixTable mtab i a }

-- | For data types with many constructors, especially enumerated types, it is effiecient if your
-- decoder performs a single look-ahead to retrieve an index, then use the index to lookup the next
-- parser in a table. This is a lookup table using 'Data.Array.IArray.Array' as the table which does
-- exactly that.
data PrefixTable mtab i a = PrefixTable String (Maybe (GGet mtab i)) (Maybe (Array i (GGet mtab a)))

instance Ix i => Functor (PrefixTable mtab i) where
  fmap f (PrefixTable msg getIdx t) = PrefixTable msg getIdx (fmap (amap (fmap f)) t)
instance (Integral i, Show i, Ix i, Binary a mtab) => Monoid (PrefixTable mtab i a) where
  mempty = PrefixTable "" Nothing Nothing
  mappend (PrefixTable msgA getIdxA a) (PrefixTable msgB getIdxB b) =
    PrefixTable (msgA<>"<>"<>msgB) (msum [getIdxA >> getIdxB, getIdxB, getIdxA]) $ msum $
      [ a >>= \a -> b >>= \b -> do
          let ((loA, hiA), (loB, hiB)) = (bounds    a, bounds    b)
          let ( lo       ,  hi       ) = (min loA loB, max hiA hiB)
          Just $ accumArray (flip mplus) mzero (lo, hi) (assocs a ++ assocs b)
      , a, b
      ]

-- | For each 'GGet' function stored in the 'PrefixTable', bind it to the function provided here and
-- store the bound functions back into the table. It works similar to the 'Control.Monad.>>='
-- function.
bindPrefixTable :: (Ix i, Binary b mtab) => PrefixTable mtab i a -> (a -> GGet mtab b) -> PrefixTable mtab i b
bindPrefixTable (PrefixTable msg getIdx arr) fn = PrefixTable msg getIdx (fmap (amap (>>=fn)) arr)

-- | Construct a 'Serializer' from a list of serializers, and each list item will be prefixed with a
-- byte in the range given. It is necesary for the data type to instantiate 'Data.Typeable.Typeable'
-- in order to 
mkPrefixTable
  :: (Integral i, Show i, Ix i, Num i)
  => String -> GGet mtab i -> i -> i -> [GGet mtab a] -> PrefixTable mtab i a
mkPrefixTable msg getIdx lo' hi' ser =
  let len   = fromIntegral (length ser)
      lo    = min lo' hi'
      hi    = max lo' hi'
      idxs  = takeWhile (<=hi) (iterate (+1) lo)
      table = PrefixTable msg (Just getIdx) $ Just $
        accumArray (flip const) (fail ("in "++msg++" table")) (lo, hi) (zip idxs ser)
  in  if null ser
      then PrefixTable msg (Just getIdx) Nothing
      else
        if 0<len && len<=hi-lo+1
        then table
        else error ("too many prefix table items for mkPrefixTable for "++msg)

mkPrefixTableWord8 :: String -> Byte -> Byte -> [GGet mtab a] -> PrefixTable mtab Byte a
mkPrefixTableWord8 msg = mkPrefixTable msg getWord8

runPrefixTable :: (Integral i, Show i, Ix i, Binary i mtab) => PrefixTable mtab i a -> GGet mtab a
runPrefixTable (PrefixTable _msg getIdx t) = flip (maybe mzero) t $ \decoderArray -> do
  prefix <- lookAhead (maybe get id getIdx)
  guard $ inRange (bounds decoderArray) prefix
  prefix <- maybe get id getIdx
  decoderArray!prefix

word8PrefixTable :: HasPrefixTable a Byte mtab => GGet mtab a
word8PrefixTable = runPrefixTable (prefixTable :: HasPrefixTable a Byte mtab => PrefixTable mtab Byte a)

prefixByte :: Byte -> GPut mtab -> GPut mtab
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
fromDataBinary :: B.Binary a => Serializer mtab a
fromDataBinary =
  Serializer
  { serializeGet = dataBinaryGet B.get
  , serializePut = dataBinaryPut . B.put
  }

dataBinaryPut :: B.PutM a -> GPutM mtab a
dataBinaryPut = PutM . lift

dataBinaryGet :: B.Get a -> GGet mtab a
dataBinaryGet = Get . lift . lift

lookAhead :: GGet mtab a -> GGet mtab a
lookAhead (Get fn) = S.get >>= Get . lift . lift . B.lookAhead . S.evalStateT (runPredicateT fn) >>= Get . predicate

bytesRead :: GGet mtab ByteOffset
bytesRead = Get $ lift $ lift B.bytesRead

isEmpty :: GGet mtab Bool
isEmpty = dataBinaryGet B.isEmpty

putWord8 :: Word8 -> GPut mtab
putWord8    = PutM . lift . B.putWord8

putWord16be :: Word16 -> GPut mtab
putWord16be = PutM . lift . B.putWord16be

putWord16le :: Word16 -> GPut mtab
putWord16le = PutM . lift . B.putWord16le

putWord32be :: Word32 -> GPut mtab
putWord32be = PutM . lift . B.putWord32be

putWord32le :: Word32 -> GPut mtab
putWord32le = PutM . lift . B.putWord32le

putWord64be :: Word64 -> GPut mtab
putWord64be = PutM . lift . B.putWord64be

putWord64le :: Word64 -> GPut mtab
putWord64le = PutM . lift . B.putWord64le

getWord8 :: GGet mtab Word8
getWord8    = Get $ lift $ lift $ B.getWord8

getWord16be :: GGet mtab Word16
getWord16be = Get $ lift $ lift $ B.getWord16be

getWord16le :: GGet mtab Word16
getWord16le = Get $ lift $ lift $ B.getWord16le

getWord32be :: GGet mtab Word32
getWord32be = Get $ lift $ lift $ B.getWord32be

getWord32le :: GGet mtab Word32
getWord32le = Get $ lift $ lift $ B.getWord32le

getWord64be :: GGet mtab Word64
getWord64be = Get $ lift $ lift $ B.getWord64be

getWord64le :: GGet mtab Word64
getWord64le = Get $ lift $ lift $ B.getWord64le

putIntegral :: (Integral a, Bits a) => a -> GPut mtab
putIntegral = putInteger . fromIntegral

getIntegral :: (Integral a, Bits a) => GGet mtab a
getIntegral = fromIntegral <$> getInteger

putPosIntegral :: (Integral a, Bits a) => a -> GPut mtab
putPosIntegral = putPosInteger . fromIntegral

getPosIntegral :: (Integral a, Bits a) => GGet mtab a
getPosIntegral = fromIntegral <$> getPosInteger

putInteger :: Integer -> GPut mtab
putInteger = dataBinaryPut . vlPutInteger

getInteger :: GGet mtab Integer
getInteger = fmap fromIntegral $ dataBinaryGet vlGetInteger

putPosInteger :: Integer -> GPut mtab
putPosInteger = dataBinaryPut . vlPutPosInteger

getPosInteger :: GGet mtab Integer
getPosInteger = dataBinaryGet vlGetPosInteger

putByteString :: B.ByteString -> GPut mtab
putByteString = dataBinaryPut . B.putByteString

getByteString :: Int -> GGet mtab B.ByteString
getByteString = dataBinaryGet . B.getByteString

putLazyByteString :: Z.ByteString -> GPut mtab
putLazyByteString = dataBinaryPut . B.putLazyByteString

getLazyByteString :: Int64 -> GGet mtab Z.ByteString
getLazyByteString = dataBinaryGet . B.getLazyByteString

-- | Look ahead one byte, if the byte is the number you are expecting drop the byte and evaluate the
-- given 'GGet' function, otherwise backtrack.
tryWord8 :: Word8 -> GGet mtab a -> GGet mtab a
tryWord8 w fn = lookAhead getWord8 >>= guard . (w==) >> getWord8 >> fn

instance Binary Int8   mtab  where
  put = putWord8 . fromIntegral
  get = fmap fromIntegral getWord8
instance Binary Int16  mtab  where { put = putIntegral;    get = Dao.Binary.getIntegral    }
instance Binary Int32  mtab  where { put = putIntegral;    get = Dao.Binary.getIntegral    }
instance Binary Int64  mtab  where { put = putIntegral;    get = Dao.Binary.getIntegral    }
instance Binary Int    mtab  where { put = putIntegral;    get = Dao.Binary.getIntegral    }
instance Binary Word8  mtab  where { put = putPosIntegral; get = Dao.Binary.getPosIntegral }
instance Binary Word16 mtab  where { put = putPosIntegral; get = Dao.Binary.getPosIntegral }
instance Binary Word32 mtab  where { put = putPosIntegral; get = Dao.Binary.getPosIntegral }
instance Binary Word64 mtab  where { put = putPosIntegral; get = Dao.Binary.getPosIntegral }
instance Binary Word   mtab  where { put = putPosIntegral; get = Dao.Binary.getPosIntegral }
instance Binary Float  mtab  where
  put = dataBinaryPut . B.putFloat32be
  get = dataBinaryGet B.getFloat32be
instance Binary Double mtab  where
  put = dataBinaryPut . B.putFloat64be
  get = dataBinaryGet B.getFloat64be
instance (RealFloat a, Binary a mtab) => Binary (Complex a) mtab where
  put (a :+ b) = put a >> put b
  get = pure (:+) <*> get <*> get

instance (Num a, Bits a, Integral a, Binary a mtab) => Binary (Ratio a) mtab where
  put o = putIntegral (numerator o) >> putPosIntegral (denominator o)
  get = pure (%) <*> getIntegral <*> getPosIntegral

instance Binary Integer mtab where { put = putInteger; get = getInteger; }
instance Binary Char    mtab where { put = putPosIntegral . ord; get = chr <$> getPosIntegral; }

instance Binary UTCTime mtab where
  put t = do
    put (toModifiedJulianDay (utctDay t))
    put (toRational (utctDayTime t))
  get = do
    d <- fmap ModifiedJulianDay get
    t <- fmap fromRational get
    return (UTCTime{ utctDay=d, utctDayTime=t })

instance Binary NominalDiffTime mtab where
  put t = put (toRational t)
  get = fmap fromRational get

instance Binary B.ByteString mtab where
  put o = putPosIntegral (B.length o) >> Dao.Binary.putByteString o
  get = getPosIntegral >>= Dao.Binary.getByteString

instance Binary Z.ByteString mtab where
  put o = putPosIntegral (Z.length o) >> Dao.Binary.putLazyByteString o
  get = getPosIntegral >>= Dao.Binary.getLazyByteString

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

instance (Ord i, Binary i mtab, Binary a mtab) => Binary (M.Map   i a) mtab where { put = put . M.assocs ; get = M.fromList  <$> get; }
instance (                      Binary a mtab) => Binary (Im.IntMap a) mtab where { put = put . Im.assocs; get = Im.fromList <$> get; }
instance (Ord a,                Binary a mtab) => Binary (S.Set     a) mtab where { put = put . S.elems  ; get = S.fromList  <$> get; }
instance                                          Binary (Is.IntSet  ) mtab where { put = put . Is.elems ; get = Is.fromList <$> get; }

instance (Eq p, Ord p, Binary p mtab, Binary a mtab) => Binary (T.Tree p a) mtab where
  put t = case t of
    T.Void           -> prefixByte 0x00 $ return ()
    T.Leaf       a   -> prefixByte 0x01 $ put a
    T.Branch       t -> prefixByte 0x02 $ put t
    T.LeafBranch a t -> prefixByte 0x03 $ put a >> put t where
  get = word8PrefixTable

instance (Eq p, Ord p, Binary p mtab, Binary a mtab) => HasPrefixTable (T.Tree p a) Byte mtab where
  prefixTable = mkPrefixTableWord8 "Tree" 0x00 0x03 $
    [ return T.Void
    , T.Leaf   <$> get
    , T.Branch <$> get
    , pure T.LeafBranch <*> get <*> get
    ]

instance Binary () mtab where { get = return (); put () = return (); }

putNullTerm :: GPut mtab
putNullTerm = putWord8 0x00

getNullTerm :: GGet mtab ()
getNullTerm = tryWord8 0x00 $ return ()

instance Binary Bool mtab where
  put o = putWord8 (if o then 0x04 else 0x05)
  get   = word8PrefixTable
instance HasPrefixTable Bool Byte mtab where
  prefixTable = mkPrefixTableWord8 "Bool" 0x04 0x05 [return False, return True]

instance Binary a mtab => Binary (Maybe a) mtab where
  put = maybe (putWord8 0x00) (\o -> putWord8 0x01 >> put o) 
  get = word8PrefixTable <|> fail "expecting Data.Maybe.Maybe"
instance Binary a mtab => HasPrefixTable (Maybe a) Byte mtab where
  prefixTable = mkPrefixTableWord8 "Maybe" 0x00 0x01 [return Nothing, Just <$> get]

instance Binary a mtab => Binary [a] mtab where
  put o = mapM_ (put . Just) o >> putNullTerm
  get   = concatMap (maybe [] return) <$> loop [] where
    loop ox = msum $
      [ getNullTerm >> return ox
        -- It is important to check for the null terminator first, then try to parse, that way the
        -- parser dose not backtrack (which may cause it to fail) if we are at a null terminator.
      , optional get >>= maybe (fail "expecting list element") (loop . (ox++) . (:[]))
      ]

-- | The inverse of 'getUnwrapped', this function is simply defined as:
-- > \list -> 'Control.Monad.mapM_' 'put' list >> 'putNullterm'
-- This is useful when you want to place a list of items, but you dont want to waste space on a
-- prefix byte for each list element. In lists of millions of elements, this can save you megabytes
-- of space, but placing any elements which are prefixed with a null byte will result in undefined
-- behavior when decoding.
putUnwrapped :: Binary a mtab => [a] -> GPut mtab
putUnwrapped list = mapM_ put list >> putNullTerm

-- | The inverse of 'putUnwrapped', this function is simply defined as: 'Control.Applicative.many'
-- 'get' >>= \list -> 'getNullTerm' >> 'Control.Monad.return' list This assumes that
-- every element placed by 'get' has a non-null prefixed encoding. If any elements in the list might
-- be encoded such that they start with a 0x00 byte, the @'Control.Applicative.many'@ 'get'
-- expression will parse the null terminator of the list as though it were an element and continue
-- looping which results in undefined behavior. Examples of elements that may start with null @0x00@
-- bytes are 'Dao.String.UStr', 'Dao.String.Name', 'Prelude.Integer', or any 'Prelude.Integral' type.
getUnwrapped :: Binary a mtab => GGet mtab [a]
getUnwrapped = fix (\loop ox -> (getNullTerm >> return ox) <|> (get >>= \o -> loop (ox++[o]))) []

instance Binary UStr mtab where
  put o = dataBinaryPut (B.put o)
  get = dataBinaryGet B.get

instance Binary Name mtab where
  put o = dataBinaryPut (B.put o)
  get = dataBinaryGet B.get

instance Binary Location mtab where
  put o = case o of
    LocationUnknown  -> return ()
    Location a b c d -> prefixByte 0x7F $ put a >> put b >> put c >> put d
  get = msum $
    [ isEmpty >>= guard >> return LocationUnknown
    , tryWord8 0x7F $ pure Location <*> get <*> get <*> get <*> get
    , return LocationUnknown
    ]

