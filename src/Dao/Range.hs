-- "Dao/Range.hs"  defines an 'Dao.EnumMap.EnumMap' data type that contains
-- 'Data.Array.IArrays'
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

module Dao.Range
  ( Range(Range), HasRange(rangeOf),
    rangeSegment, emptyRange, arrayRange, subBoundsRange,
    rangesOverlap, rangesTouch, rangeDiff, rangeSet, rangeIntersect, rangeEnum, rangePair
  )
  where

import qualified Dao.Interval as Iv

import           Control.Applicative hiding (empty)
import           Control.Monad

import qualified Data.Array.IArray as A
import           Data.Monoid

----------------------------------------------------------------------------------------------------

newtype Range i = Range { rangeSegment :: Maybe (Iv.Interval i) } deriving Eq

instance Functor Range where { fmap f (Range o) = Range $ fmap (fmap f) o }

instance Ord i => Ord (Range i) where
  compare (Range a) (Range b) = maybe EQ id $
    compare <$> a <*> b <|> const GT <$> a <|> const LT <$> b

instance (Ord i, Iv.InfBound i) => Monoid (Range i) where
  mempty = emptyRange
  mappend (Range a) (Range b) = Range $
    Iv.intervalSpan <$> a <*> b <|> a <|> b

class HasRange dat i where { rangeOf :: dat -> Range i }

instance HasRange (Range i) i where { rangeOf = id; }
instance Ord i => HasRange (Iv.Interval i) i where { rangeOf = subBoundsRange; }
instance (A.Ix i, Ord i, Iv.InfBound i) => HasRange (A.Array i o) i where { rangeOf = arrayRange; }
instance (A.Ix i, Ord i, Iv.InfBound i) => HasRange (i, i) i where { rangeOf = subBoundsRange; }
instance (A.Ix i, Ord i, Iv.InfBound i) => HasRange [A.Array i o] i where
  rangeOf ox = if null ox then emptyRange else
    (\ (o:ox) -> foldl mappend o ox) (rangeOf <$> ox)

emptyRange :: Range i
emptyRange = Range Nothing

arrayRange :: (A.Ix i, Iv.InfBound i) => A.Array i o -> Range i
arrayRange = subBoundsRange

-- | Create a 'Range' value from any data type in the 'Dao.Interval.IsSegment' class using the
-- 'Dao.Interval.subBounds' function.
subBoundsRange :: Iv.SubBounded a i => a -> Range i
subBoundsRange = Range . Just . Iv.subBounds

-- | Return true if two 'Range's are overlapping.
rangesOverlap :: Ord i => Range i -> Range i -> Bool
rangesOverlap (Range a) (Range b) = maybe False id $ Iv.areIntersecting <$> a <*> b

-- | If two 'Range's are consecutive, that is if the last index of the left value is exactly
-- one less than the first index of the right value, then the bounds are said to 'touch'.
rangesTouch :: (Ord i, Enum i, Iv.InfBound i) => Range i -> Range i -> Bool
rangesTouch (Range a) (Range b) = maybe False id $ Iv.areConsecutive <$> a <*> b

-- | Creates an 'Dao.Interval.Set' containing the difference of the two 'Range' values.
rangeDiff :: (Ord i, Enum i, Iv.InfBound i) => Range i -> Range i -> Iv.Set i
rangeDiff (Range a) (Range b) = let mk = Iv.fromList . return in
  maybe Iv.empty id $ Iv.intervalDelete <$> a <*> b <|> mk <$> a <|> mk <$> b

-- | Converts a 'Range' value to a 'Dao.Interval.Set' value.
--
-- > 'assocsBySet' ('rangeSet' s)
--
-- will produce the same result as 'assocs'.
rangeSet :: (Ord i, Enum i, Iv.InfBound i) => Range i -> Iv.Set i
rangeSet (Range o) = maybe Iv.empty (Iv.fromList . return) o

-- | Creates an 'Dao.Interval.Set' containing the intersection of the two 'Range' values.
rangeIntersect :: (Ord i, Enum i, Iv.InfBound i) => Range i -> Range i -> Range i
rangeIntersect (Range a) (Range b) = Range $
  Iv.toList <$> (Iv.intervalIntersect <$> a <*> b) >>= \ab -> guard (not $ null ab) >> return (head ab)

-- | Enumerate all elements in the 'Range'
rangeEnum :: (Bounded i, Enum i) => Range i -> [i]
rangeEnum = maybe [] Iv.enumBoundedPair . rangeSegment

-- | If the range is not empty, return the upper and lower bounds as a pair.
rangePair :: Bounded i => Range i -> Maybe (i, i)
rangePair = fmap Iv.toBoundedPair . rangeSegment


