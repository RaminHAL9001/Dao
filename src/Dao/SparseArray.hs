-- "Dao/SparseArray.hs"  defines an 'Dao.EnumMap.EnumMap' data type
-- that contains 'Data.Array.IArrays'
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

module Dao.SparseArray
  ( SparseArray, emptySparseArray, sparseArrayBounds, sparseArrayComponents,
    Dao.SparseArray.null, Dao.SparseArray.lookup, indicies, assocsBySet,
    elemsBySet, assocsByBounds, elemsByBounds, insertArray, insertArrayWith,
    fromList, fromListWith, unionArrays, unionArraysWith, intersectArrays,
    intersectArraysWith, union, unionWith, unions, unionsWith
  )
  where

import           Prelude hiding (lookup)

import qualified Dao.Interval as Iv
import           Dao.Range

import           Control.Applicative hiding (empty)
import           Control.Monad

import qualified Data.Array.IArray as A
import           Data.Monoid

----------------------------------------------------------------------------------------------------

data SparseArray i o
  = SparseArray
    { sparseArrayBounds     :: Range i
    , sparseArrayComponents :: [A.Array i o]
    }
    deriving Eq

instance A.Ix i => Functor (SparseArray i) where
  fmap f (SparseArray span arrs) = SparseArray span $ fmap (fmap f) arrs

instance (A.Ix i, Enum i, Bounded i, Iv.InfBound i, Monoid o) =>
  Monoid (SparseArray i o) where
    mempty  = emptySparseArray
    mappend = union

instance Ord i => HasRange (SparseArray i o) i where { rangeOf = sparseArrayBounds; }

null :: SparseArray i o -> Bool
null = Prelude.null . sparseArrayComponents

indicies :: (A.Ix i, Ord i, Enum i, Iv.InfBound i) => SparseArray i o -> Iv.Set i
indicies (SparseArray _ ox) = Iv.fromList $ uncurry Iv.interval . A.bounds <$> ox

lookup :: A.Ix i => SparseArray i o -> i -> Maybe o
lookup (SparseArray _ ox) i = loop ox where
  loop ox = case ox of
    []   -> Nothing
    o:ox -> if A.inRange (A.bounds o) i then Just (o A.! i) else loop ox

emptySparseArray :: SparseArray i o
emptySparseArray = SparseArray emptyRange []

-- | Enumerate all elements indicies the given set, and use these indicies to lookup elements from
-- the given array, pairing the indicies with the looked-up elements.
assocsBySet :: (Ord i, Enum i, A.Ix i, Iv.InfBound i) => A.Array i o -> Iv.Set i -> [(i, o)]
assocsBySet arr set = do
  span <- Iv.toList set
  (loA, hiA) <- [A.bounds arr]
  (loB, hiB) <- [Iv.toPair span]
  let getI deflt i = pure $ maybe deflt id $ Iv.toFinite i
  loB <- getI loA loB
  hiB <- getI hiA hiB
  (\i -> (i, arr A.! i)) <$> A.range (loB, hiB)

-- | Like 'assocsBySet' but unpairs the indecies from the elements and just return the elements.
elemsBySet :: (Ord i, Enum i, A.Ix i, Iv.InfBound i) => A.Array i o -> Iv.Set i -> [o]
elemsBySet arr = fmap snd . assocsBySet arr

-- | Like 'assocsBySet' but returns only the elements without their associated indicies.
assocsByBounds
  :: (Enum i, A.Ix i, Bounded i, Iv.InfBound i)
  => A.Array i o -> Range i -> [(i, o)]
assocsByBounds arr bnds = guard (rangesOverlap bnds $ rangeOf arr) >> _assocsByBounds arr bnds

_assocsByBounds
  :: (Enum i, A.Ix i, Bounded i, Iv.InfBound i)
  => A.Array i o -> Range i -> [(i, o)]
_assocsByBounds arr = fmap (\i -> (i, arr A.! i)) . rangeEnum

-- | Like 'assocsByBounds' but unpairs the indecies from the elements and just return the elements.
elemsByBounds
  :: (Enum i, A.Ix i, Bounded i, Iv.InfBound i)
  => A.Array i o -> Range i -> [o]
elemsByBounds arr = fmap snd . assocsByBounds arr

----------------------------------------------------------------------------------------------------

-- | Insert a single 'Data.Array.IArray.Array' into the 'SparseArray', elements of overlapping
-- indicies are unified with 'Data.Monoid.mappend'.
insertArray
  :: (Eq i, Enum i, Bounded i, Iv.InfBound i, A.Ix i, Monoid o)
  => A.Array i o -> SparseArray i o -> SparseArray i o
insertArray = insertArrayWith mappend

-- | Insert a single 'Data.Array.IArray.Array' into the 'SparseArray', and unify elements of
-- overlapping indicies with a appending function provided as the first parameter.
insertArrayWith
  :: (Eq i, Enum i, Bounded i, A.Ix i, Iv.InfBound i)
  => (o -> o -> o) -> A.Array i o -> SparseArray i o -> SparseArray i o
insertArrayWith mappend a (SparseArray spanB bx) = SparseArray (spanA<>spanB) $
  if not $ rangesOverlap spanB spanA
    then if spanB<spanA then a:bx else bx++[a]
    else loop bx a
  where
    spanA = rangeOf a
    loop bx a = case bx of
      []   -> [a]
      b:bx -> do
        let spanB = rangeOf b
        if rangesOverlap spanA spanB
          then do
            let (SparseArray _ ax) = unionArraysWith mappend a b
            take 1 ax >>= loop bx
          else if spanA<spanB then a:b:bx else b : loop bx a

-- | Construct a 'SparseArray' from a list of component 'Data.Array.IArray.Array's. Elements of
-- overlapping indicies are unified with 'Data.Monoid.mappend'.
fromList
  :: (A.Ix i, Enum i, Bounded i, Iv.InfBound i, Monoid o)
  => [A.Array i o] -> SparseArray i o
fromList = fromListWith mappend

-- | Construct a 'SparseArray' from a list of component 'Data.Array.IArray.Array's, and unify
-- elements of overlapping indicies with a appending function provided as the first parameter.
fromListWith
  :: (Enum i, A.Ix i, Bounded i, Iv.InfBound i)
  => (o -> o -> o) -> [A.Array i o] -> SparseArray i o
fromListWith mappend = foldr (insertArrayWith mappend) emptySparseArray

----------------------------------------------------------------------------------------------------

-- | Create an initial array with only a single initial element copied to every index. The indicies
-- to initialize must be provided in an interval 'Dao.Interval.Set'. *USE WITH CAUTION:* if your
-- interval 'Dao.Interval.Set' was constructed with possibly infinite values, this will attempt to
-- allocate impossibly large arrays.
--initArray :: Bounded i => Iv.Set i -> o -> SparseArray i o
--initArray ix o =
--  if Iv.null ix
--    then  emptySparseArray
--    else  SparseArray (Iv.spanOf ix) $
--            fmap (\seg -> A.accumArray const o (Iv.toBoundedPair seg) []) (Iv.toList ix)

-- | Combine two 'Data.Array.IArray.Array's together only if they are range over intersecting or
-- consecutive indicies.  Return 'Prelude.Nothing' if the two 'Data.Array.IArray.Array's do not
-- intersect. For arrays that have overlapping elements, use 'Data.Monoid.mappend' to combine these
-- elements together.
unionArrays
  :: (A.Ix i, Bounded i, Enum i, Iv.InfBound i, Monoid o)
  => A.Array i o -> A.Array i o -> SparseArray i o
unionArrays = unionArraysWith mappend

-- | Combine two 'Data.Array.IArray.Array's together only if they are range over intersecting or
-- consecutive indicies.  Return 'Prelude.Nothing' if the two arrays do not intersect. For
-- 'Data.Array.IArray.Array's that have overlapping elements, use a provided combining function to
-- combine these elements together.
unionArraysWith
  :: (A.Ix i, Bounded i, Enum i, Iv.InfBound i)
  => (o -> o -> o) -> A.Array i o -> A.Array i o -> SparseArray i o
unionArraysWith mappend a b =
  let spanA = rangeOf a
      spanB = rangeOf b
      span  = spanA<>spanB
      overlap = rangeIntersect spanA spanB
  in  SparseArray span $ case rangeSegment overlap of
        Just  _ -> A.array <$> maybe [] return (rangePair span) <*>
          [ assocsBySet a (rangeDiff spanA spanB)
          , assocsBySet b (rangeDiff spanA spanB)
          , assocsBySet a (rangeDiff spanB spanA)
          , assocsBySet b (rangeDiff spanB spanA)
          , _intersectArrays mappend overlap a b
          ]
        Nothing ->
          if rangesTouch spanA spanB
            then A.array <$> maybe [] return (rangePair span) <*> [A.assocs a, A.assocs b]
            else [a, b]

_intersectArrays
  :: forall i a b c . (A.Ix i, Enum i, Bounded i, Iv.InfBound i)
  => (a -> b -> c) -> Range i -> A.Array i a -> A.Array i b -> [(i, c)]
_intersectArrays mappend span a b = do
  let indicies :: A.Array i x -> [x]
      indicies = fmap snd . flip _assocsByBounds span
  zip (rangeEnum span) $ uncurry mappend <$> zip (indicies a) (indicies b)

-- | Combine two 'Data.Array.IArray.Array's together only if there are some overlapping indicies,
-- otherwise construct an empty 'SparseArray'. Unify elements of overlapping indicies with
-- 'Data.Monoid.mappend'.
intersectArrays
  :: (A.Ix i, Enum i, Bounded i, Iv.InfBound i, Monoid o)
  => A.Array i o -> A.Array i o -> SparseArray i o
intersectArrays = intersectArraysWith mempty

-- | Combine two 'Data.Array.IArray.Array's together only if there are some overlapping indicies.
-- Unify elements of overlapping indicies with a appending function provided as the first parameter.
intersectArraysWith
  :: (A.Ix i, Enum i, Bounded i, Iv.InfBound i)
  => (a -> b -> c) -> A.Array i a -> A.Array i b -> SparseArray i c
intersectArraysWith mappend a b = 
  let spanA = rangeOf a
      spanB = rangeOf b
      isect = rangeIntersect spanA spanB
  in  SparseArray isect $ do
        let span = rangeIntersect spanA spanB
        (lo, hi) <- maybe [] return $ rangePair span
        [A.array (lo, hi) $ _intersectArrays mappend isect a b]

-- | Combine two 'SparseArray's together. Unify elements of overlapping indicies with
-- 'Data.Monoid.mappend'.
union
  :: (A.Ix i, Enum i, Bounded i, Iv.InfBound i, Monoid o)
  => SparseArray i o -> SparseArray i o -> SparseArray i o
union = unionWith mappend

-- | Combine two 'SparseArray's together. Unify elements of overlapping indicies with a appending
-- function provided as the first parameter.
unionWith
  :: (A.Ix i, Enum i, Bounded i, Iv.InfBound i)
  => (o -> o -> o) -> SparseArray i o -> SparseArray i o -> SparseArray i o
unionWith mappend (SparseArray spanA a) (SparseArray spanB b) =
  let mk o = foldr (insertArrayWith mappend) (SparseArray (spanA<>spanB) o) in
  case a of
    [] -> SparseArray spanB b
    a  -> case b of
      [] -> SparseArray spanA a
      b  ->
        if rangesOverlap spanA spanB
          then if arrayRange (head a) < arrayRange (head b) then mk a b else mk b a
          else SparseArray (spanA<>spanB) $ if spanA<spanB then a++b else b++a

-- | Union a list of 'SparseArray's together. Unify elements of overlapping indicies with
-- 'Data.Monoid.mappend'.
unions
  :: (A.Ix i, Enum i, Bounded i, Iv.InfBound i, Monoid o)
  => [SparseArray i o] -> SparseArray i o
unions = foldl union mempty

-- | Union a list of 'SparseArray's together. Unify elements of overlapping indicies with a appending
-- function provided as the first parameter.
unionsWith
  :: (A.Ix i, Enum i, Bounded i, Iv.InfBound i)
  => (o -> o -> o) -> [SparseArray i o] -> SparseArray i o
unionsWith mappend = foldl (unionWith mappend) Dao.SparseArray.emptySparseArray

