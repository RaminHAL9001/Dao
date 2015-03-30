-- "Dao/Tree.hs"  provides a fundamental data type used by Dao.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see the URL:
-- <http://www.gnu.org/licenses/agpl.html>.

module Dao.Array
  ( Array, array, arraySpan, size, indexOK, elems, lastElem,
    indexElems, (!), toIArray, arrayIsNull,
    levenshteinDistanceMatrix,
    levenshteinIArrayDistance,
    levenshteinArrayDistance,
    levenshteinStringDistance,
    fuzzyCompare, showMatrix
  )
  where

import           Dao.Text
import           Dao.TestNull

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad hiding (mapM, msum)

import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A
import           Data.Maybe
import           Data.Foldable
import           Data.Monoid
import qualified Data.Text as Strict
import           Data.Traversable
import           Data.Typeable

----------------------------------------------------------------------------------------------------

newtype Array o = Array (Maybe (A.Array Int o)) deriving (Eq, Ord, Show, Typeable)

instance NFData o => NFData (Array o) where { rnf (Array o) = deepseq o () }

instance Functor Array where { fmap f (Array o) = Array $ fmap (fmap f) o }

instance Monad Array where
  return = Array . Just . A.array (0, 0) . return . (,) 0
  o >>= f = array $ elems o >>= elems . f

instance MonadPlus Array where { mzero=mempty; mplus=mappend; }

instance Applicative Array where { pure=return; (<*>)=ap; }

instance Alternative Array where { empty=mzero; (<|>)=mplus; }

instance Traversable Array where { traverse f = fmap array . traverse f . elems; }

instance Foldable Array where { foldr f i = Data.Foldable.foldr f i . elems; }

instance Monoid (Array o) where
  mempty = Array Nothing
  mappend a@(Array arrA) b@(Array arrB) = Array $ msum
    [ (\arrA arrB -> A.listArray (0, size a + size b - 1) $ A.elems arrA ++ A.elems arrB)
        <$> arrA <*> arrB
    , arrA, arrB
    ]

instance TestNull (Array o) where
  nullValue = mempty
  testNull (Array o) = isNothing o

-- | Returns the minimum bounds that contains the bounds for both given 'Data.Array.IArray.Array's.
-- *NOTE* that this operates on arrays from the "Data.Array.IArray" module.
arraySpan :: A.Ix i => A.Array i x -> A.Array i y -> (i, i)
arraySpan a b =
  let (loA, hiA) = A.bounds a
      (loB, hiB) = A.bounds b
  in  (min loA loB, max hiA hiB)

array :: [o] -> Array o
array ox = case ox of
  [] -> mempty
  ox -> Array $ Just $ A.listArray (0, length ox - 1) ox

elems :: Array o -> [o]
elems (Array o) = maybe [] A.elems o

lastElem :: Array o -> Maybe o
lastElem (Array o) = o >>= \o -> Just $ o A.! snd (A.bounds o)

size :: Array o -> Int
size (Array a) = maybe 0 ((1+) . uncurry subtract . A.bounds) a

indexOK :: Array o -> Int -> Bool
indexOK (Array o) i = maybe False (flip A.inRange i . A.bounds) o

indexElems :: Array o -> (Int, Int) -> [o]
indexElems (Array o) (loB, hiB) = flip (maybe []) o $ \o ->
  let (loA, hiA) = A.bounds o in (o A.!) <$> A.range (max loA loB, min hiA hiB)

(!) :: Array o -> Int -> Maybe o
(!) arr@(Array o) i = guard (indexOK arr i) >> (A.! i) <$> o
infixr 8 !

toIArray :: Array o -> Maybe (A.Array Int o)
toIArray (Array o) = o

arrayIsNull :: Array o -> Bool
arrayIsNull o = case o of
  Array Nothing -> True
  _             -> False

----------------------------------------------------------------------------------------------------

-- | This function efficiently computes the Levenshtein distance matrix for any two lists.
-- <http://en.wikipedia.org/wiki/Levenshtein_distance>
-- Takes a distance function that compares a list of @a@ to a list of @b@, where the distance
-- function must return zero for matching elements, and a non-zero value for non-matching elements.
--
-- The inputs to the matrix will be stored in boxed arrays, so this function is more general and
-- less efficient. Use 'levenshteinDistanceMatrixUnboxed' to compute efficiently using unboxed
-- arrays.
levenshteinDistanceMatrix
  :: forall array i a b dist
    . (Ord dist, Enum dist, Num dist, Enum i, A.Ix i,
        A.IArray array a, A.IArray array b, A.IArray A.UArray dist)
  => (a -> b -> dist) -> array i a -> array i b -> A.UArray(Int,Int) dist
levenshteinDistanceMatrix diff s t = A.array (A.bounds arr) (A.assocs arr) where
  bs = A.bounds s :: (i, i)
  bt = A.bounds t :: (i, i)
  m = A.rangeSize bs :: Int
  n = A.rangeSize bt :: Int
  arr :: A.Array(Int,Int) dist
  arr = A.array ((0, 0), (m, n)) $ Data.Foldable.concat
    [ [((0, 0), 0)]
    , ([1..m] `zip` repeat 0) `zip` (toEnum . succ . A.index bs <$> A.range bs)
    , (repeat 0 `zip` [1..n]) `zip` (toEnum . succ . A.index bt <$> A.range bt)
    , do  (i, j) <- (,) <$> A.range bs <*> A.range bt
          let i0 = A.index bs i
          let i1 = succ i0
          let j0 = A.index bt j
          let j1 = succ j0
          return $
            ( (i1, j1)
            , let dist = diff (s A.! i) (t A.! j) in
              if dist==0
              then arr A.! (i0, j0)
              else Data.Foldable.minimum $
                (+ dist) <$> [arr A.! (i0, j1), arr A.! (i1, j0), arr A.! (i0, j0)]
            )
    ]

-- | This function efficiently computes the Levenshtein distance between any two arrays that
-- instantiate the 'Data.Array.IArray.IArray' class using the 'levenshteinDistanceMatrix'.
levenshteinIArrayDistance
  :: (Enum i, A.Ix i, Ord dist, Enum dist, Num dist,
      A.IArray array b, A.IArray array a, A.IArray A.UArray dist)
  => (a -> b -> dist) -> array i a -> array i b -> dist
levenshteinIArrayDistance dist a b = m  A.!  snd (A.bounds m) where
  m = levenshteinDistanceMatrix dist a b

-- | Uses the 'levenshteinIArrayDistance' to compute a distance on any two strings that instantiate
-- 'ToText'.
levenshteinArrayDistance :: (a -> b -> Int) -> Array a -> Array b -> Int
levenshteinArrayDistance diff (Array a) (Array b) = case a of
  Nothing -> case b of
    Nothing -> 0
    Just  b -> A.rangeSize $ A.bounds b
  Just  a -> case b of
    Nothing -> A.rangeSize $ A.bounds a
    Just  b -> levenshteinIArrayDistance diff a b

-- | Uses the 'levenshteinIArrayDistance' to compute a distance on any two strings that instantiate
-- 'ToText'.
levenshteinStringDistance :: ToText t => t -> t -> Int
levenshteinStringDistance s' t' = let { s = toText s'; t = toText t'; } in
  if Strict.null s && Strict.null t then 0 else
    if Strict.null s then Strict.length t else if Strict.null t then Strict.length s else
      levenshteinIArrayDistance (\s t -> if s==t then 0 else 1)
        (A.listArray (1, Strict.length s) (Strict.unpack s) :: A.UArray Int Char)
        (A.listArray (1, Strict.length t) (Strict.unpack t) :: A.UArray Int Char)

-- | Use the 'levenshteinStringDistance' function to compute the difference value between two strings @s@
-- and @t@, return a 'Prelude.Rational' value such that equal strings evaluate to a value of @1.0@,
-- and different strings evaluate to some value between @1.0@ and @0.0@, computed by:
--
-- @
-- let maxLen = 'Prelude.max' ('Data.Text.length' s) ('Data.Text.length' t)
-- in  (maxLen - 'levenshteinStringDistance' s t) / maxLen
-- @
--
-- **NOTE:** This function does not modify the strings in any way, for example converting to
-- lower-case. The caller of this function is responsible for such preprocessing.
fuzzyCompare :: ToText t => t -> t -> Rational
fuzzyCompare s' t' =
  let (s, t) = (toText s', toText t')
      maxLen = max (Strict.length s) (Strict.length t)
  in toRational (maxLen - levenshteinStringDistance s t) / toRational maxLen

showMatrix :: forall array i o . (A.Ix i, Show o, A.IArray array o) => array(i,i) o -> Strict.Text
showMatrix arr = Strict.intercalate (Strict.singleton '\n') $ do
  let strArr :: A.Array(i,i) Strict.Text
      strArr = A.array (A.bounds arr) $ fmap (toText . show) <$> A.assocs arr
  let maxLen = 1 + Data.Foldable.maximum (Strict.length <$> A.elems strArr)
  let sp   t = Strict.pack (replicate (max 2 $ maxLen - Strict.length t) ' ') <> t
  let ((loX, loY), (hiX, hiY)) = A.bounds strArr
  A.range (loY, hiY) >>= \y -> [Strict.concat $ (\x -> sp $ strArr A.! (x, y)) <$> A.range (loX, hiX)]

