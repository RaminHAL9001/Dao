-- "src/Dao/HashMap.hs"  a simple hash map using Data.IntMap
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

module Dao.HashMap
  ( Hash128, Int128Hashable(hash128),
    hash128_md5, deriveHash128_Show, deriveHash128_UStr,
    deriveHash128_DataBinary, deriveHash128_DaoBinary,
    Index, indexToPair, hashNewIndex, newIndex, indexHash, indexKey,
    HashMap, hashLookup, hashAlter, hashInsert, hashDelete, hashAdjust, hashModify,
    Dao.HashMap.lookup, alter, Dao.HashMap.insert, Dao.HashMap.delete, adjust, modify,
    unionWith, unionsWith, Dao.HashMap.union, unions, differenceWith, difference, intersectionWith, intersection,
    assocs, elems, keys, fromListWith, fromList, size, empty
  ) where

import           Dao.String
import           Dao.PPrint
import           Dao.Binary

import           Control.Applicative hiding (empty)
import           Control.Monad

import           Data.Typeable
import           Data.Monoid
import           Data.List
import           Data.Char
import           Data.Word
import qualified Data.Binary          as B
import qualified Data.Binary.Get      as B
import qualified Data.Binary.Put      as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Map             as M
import qualified Data.IntMap          as I
import qualified Data.Digest.MD5      as MD5

----------------------------------------------------------------------------------------------------

type Hash128 = (Word64, Word64)
type Int128Map a = I.IntMap (I.IntMap a)

_lookup :: Hash128 -> Int128Map a -> Maybe a
_lookup (h1, h2) = I.lookup (fromIntegral h1) >=> I.lookup (fromIntegral h2)

_alter :: (Maybe a -> Maybe a) -> Hash128 -> Int128Map a -> Int128Map a
_alter alt (h1, h2) =
  I.alter
    ((\m -> if I.null m then Nothing else Just m) . I.alter alt (fromIntegral h2) . maybe mempty id)
    (fromIntegral h1)

_unionWith :: (a -> a -> a) -> Int128Map a -> Int128Map a -> Int128Map a
_unionWith f = I.unionWith (I.unionWith f)

_differenceWith :: (a -> b -> Maybe a) -> Int128Map a -> Int128Map b -> Int128Map a
_differenceWith f = I.differenceWith (\a b -> let m = I.differenceWith f a b in guard (not $ I.null m) >> return m)

_intersectionWith :: (a -> b -> c) -> Int128Map a -> Int128Map b -> Int128Map c
_intersectionWith f = I.intersectionWith (I.intersectionWith f)

_assocs :: Int128Map a -> [(Hash128, a)]
_assocs m = I.assocs m >>= \ (h1, m) -> I.assocs m >>= \ (h2, a) -> [((fromIntegral h1, fromIntegral h2), a)]

_fromListWith :: (a -> a -> a) -> [(Hash128, a)] -> Int128Map a
_fromListWith f = fmap (I.fromListWith f) . I.fromListWith (++) . fmap (\ ((h1, h2), a) -> (fromIntegral h1, [(fromIntegral h2, a)]))

----------------------------------------------------------------------------------------------------

class Ord key => Int128Hashable key where { hash128 :: key -> Hash128 }

hash128_md5 :: [Word8] -> Hash128
hash128_md5 = B.runGet (pure (,) <*> B.getWord64be <*> B.getWord64be) . B.pack . MD5.hash

deriveHash128_Show :: Show a => a -> Hash128
deriveHash128_Show = deriveHash128_UStr . show

deriveHash128_UStr :: UStrType a => a -> Hash128
deriveHash128_UStr = hash128_md5 . utf8bytes

deriveHash128_DataBinary :: B.Binary a => a -> Hash128
deriveHash128_DataBinary = hash128_md5 . B.unpack . B.encode

deriveHash128_DaoBinary :: (HasCoderTable mtab, Binary a mtab) => mtab -> a -> Hash128
deriveHash128_DaoBinary mtab = hash128_md5 . B.unpack . encode mtab

----------------------------------------------------------------------------------------------------

data Index key = Index { indexHash :: Hash128, indexKey :: key } deriving (Eq, Ord, Typeable)

indexToPair :: Index key -> (B.ByteString, key)
indexToPair (Index{ indexHash=(h1, h2), indexKey=key }) =
  (B.runPut (B.putWord64be h1 >> B.putWord64be h2), key)

instance Show key => Show (Index key) where
  show (Index{ indexKey=key }) = concat ["Index (", show key, ")"]

instance (Read key, Int128Hashable key) => Read (Index key) where
  readsPrec p s = do
    s <- maybe [] return $ stripPrefix "Index" $ dropWhile isSpace s
    (key, s) <- case dropWhile isSpace s of { '(':s -> readsPrec p s; _ -> [] }
    case dropWhile isSpace s of
      ')':s -> [(Index{ indexHash=hash128 key, indexKey=key }, s)]
      _     -> []

instance Binary key mtab => Binary (Index key) mtab where
  put (Index{ indexHash=(h1, h2), indexKey=key }) = putWord64be h1 >> putWord64be h2 >> put key
  get = do
    h1 <- getWord64be
    h2 <- getWord64be
    key <- get
    return $ Index{ indexHash=(h1, h2), indexKey=key }

instance PPrintable key => PPrintable (Index key) where { pPrint i = pPrint (indexKey i) }

hashNewIndex :: (key -> Hash128) -> key -> Index key
hashNewIndex hash key = Index{ indexHash = hash key, indexKey = key }

newIndex :: Int128Hashable key => key -> Index key
newIndex = hashNewIndex hash128

----------------------------------------------------------------------------------------------------

newtype HashMap key a = HashMap (Int128Map (M.Map key a)) deriving (Eq, Ord, Typeable)

instance (Show key, Show a) => Show (HashMap key a) where
  show = ("fromList "++) . show . assocs

instance (Read key, Int128Hashable key, Read a) => Read (HashMap key a) where
  readsPrec p s = maybe [] return (stripPrefix "fromList" $ dropWhile isSpace s) >>=
    readsPrec p . dropWhile isSpace >>= \ (hmap, s) -> return (fromList hmap, s)

instance Functor (HashMap key) where
  fmap f (HashMap m) = HashMap $ fmap (fmap (fmap f)) m

instance (Ord key, Monoid a) => Monoid (HashMap key a) where
  mempty = HashMap mempty
  mappend a b = unionWith mappend a b

instance HasNullValue (HashMap key a) where
  nullValue = HashMap mempty
  testNull = Dao.HashMap.null

instance (Ord key, Binary key mtab, Binary a mtab) => Binary (HashMap key a) mtab where
  put = put . assocs
  get = fromList <$> get

instance (PPrintable key, PPrintable a) => PPrintable (HashMap key a) where
  pPrint = pList (pString "HashMap") "{" ", " "}" .
    map (\ (a, b) -> pInline [pPrint a, pString " = ", pPrint b]) . assocs

null :: HashMap key a -> Bool
null (HashMap a) = I.null a

hashLookup :: Ord key => Index key -> HashMap key a -> Maybe a
hashLookup key (HashMap intmap) = _lookup (indexHash key) intmap >>= M.lookup (indexKey key)

lookup :: Int128Hashable key => key -> HashMap key a -> (Index key, Maybe a)
lookup key hmap = let i = Index{ indexHash=hash128 key, indexKey=key } in (i, hashLookup i hmap)

hashAlter :: Ord key => (Maybe a -> Maybe a) -> Index key -> HashMap key a -> HashMap key a
hashAlter alt key (HashMap intmap) = HashMap $
  _alter
    ((\m -> guard (not $ M.null m) >> Just m) . M.alter alt (indexKey key) . maybe mempty id)
    (indexHash key)
    intmap

alter :: Int128Hashable key => (Maybe a -> Maybe a) -> key -> HashMap key a -> HashMap key a
alter f key = hashAlter f (Index{ indexHash=hash128 key, indexKey=key })

hashInsert :: Ord key => Index key -> a -> HashMap key a -> HashMap key a
hashInsert key a = hashAlter (const $ Just a) key

insert :: Int128Hashable key => key -> a -> HashMap key a -> HashMap key a
insert key = hashInsert (Index{ indexHash=hash128 key, indexKey=key})

hashDelete :: Ord key => Index key -> HashMap key a -> HashMap key a
hashDelete = hashAlter (const Nothing)

delete :: Int128Hashable key => key -> HashMap key a -> HashMap key a
delete key = hashDelete (Index{ indexHash=hash128 key, indexKey=key })

hashAdjust :: Ord key => (a -> a) -> Index key -> HashMap key a -> HashMap key a
hashAdjust f = hashAlter (fmap f)

adjust :: Int128Hashable key => (a -> a) -> key -> HashMap key a -> HashMap key a
adjust f key = hashAdjust f (Index{ indexHash=hash128 key, indexKey=key })

hashModify :: Ord key => (a -> Maybe a) -> Index key -> HashMap key a -> HashMap key a
hashModify f = hashAlter (>>=f)

modify :: Int128Hashable key => (a -> Maybe a) -> key -> HashMap key a -> HashMap key a
modify f key = hashModify f (Index{ indexHash=hash128 key, indexKey=key })

unionWith :: Ord key => (a -> a -> a) -> HashMap key a -> HashMap key a -> HashMap key a
unionWith f (HashMap a) (HashMap b) = HashMap $ _unionWith (M.unionWith f) a b

union :: Ord key => HashMap key a -> HashMap key a -> HashMap key a
union = unionWith const

unionsWith :: Ord key => (a -> a -> a) -> [HashMap key a] -> HashMap key a
unionsWith f = foldl (unionWith f) empty

unions :: Ord key => [HashMap key a] -> HashMap key a
unions = unionsWith const

differenceWith :: Ord key => (a -> b -> Maybe a) -> HashMap key a -> HashMap key b -> HashMap key a
differenceWith f (HashMap a) (HashMap b) = HashMap $
  _differenceWith (\a b -> let m = M.differenceWith f a b in guard (not $ M.null m) >> return m) a b

difference :: Ord key => HashMap key a -> HashMap key b -> HashMap key a
difference = differenceWith (\ _ _ -> Nothing)

intersectionWith :: Ord key => (a -> b -> c) -> HashMap key a -> HashMap key b -> HashMap key c
intersectionWith f (HashMap a) (HashMap b) = HashMap $ _intersectionWith (M.intersectionWith f) a b

intersection :: Ord key => HashMap key a -> HashMap key a -> HashMap key a
intersection = intersectionWith const

assocs :: HashMap key a -> [(Index key, a)]
assocs (HashMap m) = do
  (hash, m) <- _assocs m
  (key , a) <- M.assocs m
  [(Index{ indexHash=hash, indexKey=key }, a)]

elems :: HashMap key a -> [a]
elems = fmap snd . assocs

keys :: HashMap key a -> [Index key]
keys = fmap fst . assocs

fromListWith :: Ord key => (a -> a -> a) -> [(Index key, a)] -> HashMap key a
fromListWith f = HashMap . fmap (fmap $ M.fromListWith f) . _fromListWith (++) .
  fmap (\ (Index{ indexHash=hash, indexKey=key }, a) -> (hash, [(key, a)]))

fromList :: Ord key => [(Index key, a)] -> HashMap key a
fromList = fromListWith (flip const)

size :: HashMap key a -> Integer
size (HashMap m) = I.foldl (I.foldl (\i -> (i+) . toInteger . M.size)) 0 m

empty :: HashMap key a
empty = HashMap I.empty

