-- "Dao/Certainty.hs"  Creates a newtype of Real-number that behaves as a
-- certainty value for a neuron in a neural network.
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

-- | This module provides the 'Certainty' data type. This data type contains a kind of
-- 'Prelude.Rational' number representing an arbitrary 'average' value, where the sum of the sample
-- points and the number of sample points are stored as the numerator and denominator of the number.
--
-- The purpose of the 'Certainty' value, however, is not to simply store an arbitrary rational
-- number value, but also to provides a method of converting this 'Certainty' value to
-- 'Prelude.Double' data type that is always between @0.0@ and @1.0@. This is accomplished by
-- applying the rational number value in the 'Certainty' value to the 'sigmoidal' function defined
-- in this module. You can define your own sigmoidal for the 'Certainty' value by creating a
-- @newtype@ of 'Certainty' and instantiating the 'CertaintyClass' with your own 'certainty'
-- 'Dao.Lens.Lens'.
--
-- The utility of the 'Certainty' data type mostly comes from it's ability to keep a running
-- 'average' value through use of 'Data.Monoid.mconcat'. It is easy to convert a list of
-- 'Prelude.Double' values to a list of 'Certainty' values using 'Prelude.fromRational',
-- 'Data.Monoid.mconcat' them together, retrieve the 'average', and then continue
-- 'Data.Monoid.mconcat'ing values and retrieving updated 'average' values.
--
-- The instantiation of 'Prelude.Num' and 'Prelude.Fractional' otherwise allows for treating the
-- 'Certainty' data type as ordinary 'Prelude.Double'-percsion number data.
module Dao.Certainty
  ( Certainty, certaintyRatio, sumTotal, sampleSize, average, onTotals,
    CertaintyClass(certainty), sigmoidal, invSigmoidal,
    MatchDistance(MatchDistance), exactSame, distanceSimilarity, invertDistance,
    matchPermutationPattern, matchStatement, treeDistanceWith,
    MatchByDistance(matchDistance)
  )
  where

import           Prelude hiding (id, (.))

import           Dao.Array
import           Dao.Lens
import           Dao.Logic
import           Dao.TestNull
import qualified Dao.Tree as T

import           Control.Applicative
import           Control.Category
import           Control.Monad

import           Data.List (sortBy)
import           Data.Monoid
import           Data.Typeable

-- not exported
i2d :: Integer -> Double
i2d = fromRational . toRational

fourPi :: Floating t => t
fourPi = 4.0*pi

----------------------------------------------------------------------------------------------------

-- | This data type encodes an 'average' value, where the 'sampleSize' is maintained to indicate the
-- percision of the 'average'. Combining 'Certainty' values using 'Data.Monoid.mappend' will sum both
-- the 'sumTotal' and the 'sampleSize'. The best way to compute a running 'average' is to use
-- 'Prelude.fromInteger' or 'Prelude.fromRational' to construct a 'Certainty' value with a
-- 'sampleSize' of @1@, then fold the 'Certainty's using 'Data.Monoid.mappend'. For example:
--
-- @
-- getSample :: IO 'Prelude.Double'
-- getSample = ...
--
-- getNSamples :: IO 'Certainty'
-- getNSamples n = 'Data.Monoid.mconcat' . 'Data.Functor.fmap' 'Prelude.fromRational' 'Control.Applicative.<$>' 'Control.Monad.sequence' ('Control.Monad.replicateM' n getSamples)
-- @
--
-- The 'Certainty' data type instantiates 'Prelude.Num' and 'Prelude.Fractional' as well, so you can
-- perform addition, subtraction, multiplication, and division on 'Certainty' values, however these
-- computations make the 'Certainty' subject to down-sampling such that the resulting 'Certainty'
-- always has the lower 'sampleSize' of the two 'Certainty' values.
--
-- The 'Certainty' data type does *NOT* instantiate 'Prelude.Real' (so 'Prelude.toRational' cannot
-- be used) as a 'Certainty' value should not be approximated by a 'Prelude.Rational' number. The
-- 'Certainty' data type does *NOT* instantiate 'Prelude.Fractional' either, because it does not
-- make sense to perform logarithmic or trigonometric functions on a 'Certainty' value.
--
-- However, the 'average' 'Dao.Lens.Lens' can be used to retrieve a 'Prelude.Double'-percision
-- floating-point number representing the 'sumTotal' divided by the 'sampleSize', and you can use
-- the 'certainty' 'Dao.Lens.Lens' to retrieve a 'Prelude.Double'-percision number representing the
-- 'average' value applied to the 'sigmoidal' function, which guarantees a value between @0.0@ and
-- @1.0@, hence the name 'Certainty'.
--
-- It is the 'certainty' function, and the use of the 'sigmoidal' function to compute a 'certainty'
-- value between @0.0@ and @1.0@ that makes the 'Certainty' value useful for constructing neural
-- networks.
data Certainty = Certainty { _total :: Double, sampleSize :: Integer } deriving Typeable

-- | Create a new 'Certainty' value from a ratio expressed as a 'Prelude.Double' divided by an
-- 'Prelude.Integer'.
certaintyRatio :: Double -> Integer -> Certainty
certaintyRatio = Certainty

-- | Use this 'Dao.Lens.Lens' to get the value of the average of this 'Certainty' value, which is
-- computed by dividing the 'sumTotal' by the 'sampleSize'.
average :: Monad m => Lens m Certainty Double
average =
  newLens (\  (Certainty a b) -> a / i2d b)
          (\a (Certainty _ b) -> Certainty (a * i2d b) b)

-- | Use this 'Dao.Lens.Lens' to modify the accumulated sum total of the 'Certainty' value without
-- changing the 'sampleSize'.
sumTotal :: Monad m => Lens m Certainty Double
sumTotal = newLens (\ (Certainty a _) -> a) (\a (Certainty _ b) -> Certainty a b)

instance Eq Certainty where { (==) a b = EQ == compare a b; }

instance Ord Certainty where
  compare a b = compare (a & certainty) (b & certainty)

instance Show Certainty where { show c = "Certainty{ average="++show (c & average)++" }"; }

instance TestNull Certainty where
  nullValue = Certainty 0.0 1
  testNull (Certainty a b) = a==0.0 && b==1

instance Monoid Certainty where
  mempty = nullValue
  mappend (Certainty a1 b1) (Certainty a2 b2) = Certainty (a1+a2) (b1+b2)

instance Num Certainty where
  fromInteger = flip Certainty 1 . fromRational . toRational
  (+) = onTotals (+)
  (*) = onTotals (*)
  negate (Certainty a b) = Certainty (negate a) b
  abs (Certainty a b) = Certainty (abs a) b
  signum (Certainty a b) = Certainty (signum a) b

instance Fractional Certainty where
  (/) = onTotals (/)
  fromRational = flip Certainty 1 . fromRational

----------------------------------------------------------------------------------------------------

-- | This class defines a way to convert the 'average' 'Certainty' value to some
-- 'Prelude.Double'-precision number (which should be between 0.0 and 1.0). The default certainty
-- used, the instantiation of this class that the 'Certainty' data type defines, is a sigmoidal
-- function.
--
-- When developing Artificial Neural Network models, you can use the 'Certainty' type's default
-- instnatiation of the 'certainty' function which uses the 'sigmoidal' function defined in this
-- module. But to define your own sigmoidal function, you can create a @newtype@ of 'Certainty' and
-- define your own sigmoidal to 'Dao.Lens.fetch' or 'Dao.Lens.alter' the 'certainty' of the
-- 'Certainty' data type.
class CertaintyClass o where { certainty :: Monad m => Lens m o Double; }

instance CertaintyClass Certainty where
  certainty =
    newLens (\  (Certainty a b) -> sigmoidal $ a / i2d b)
            (\a (Certainty _ b) -> Certainty (invSigmoidal a * i2d b) b)

----------------------------------------------------------------------------------------------------

-- | Apply a binary infix operator, or any function on two 'Prelude.Double' values, to two
-- 'Certainty' values. If the 'sampleSize' of the two 'Certainty' values are not equal, the 'sumTotal'
-- value of the 'Certainty' with the larger sample size is scaled-down such that it's 'sampleSize'
-- is equal to 'Certainty' value with the lesser 'sampleSize' before being applied to the function.
-- Scaling the value to the smaller sample size encodes the loss of percision caused by combining
-- values of different sample sizes.
onTotals :: (Double -> Double -> Double) -> Certainty -> Certainty -> Certainty
onTotals f (Certainty a x) (Certainty b y) = case compare x y of
  EQ -> Certainty (f a b) x
  LT -> Certainty (f a (b * i2d x / i2d y)) x
  GT -> Certainty (f (a * i2d y / i2d x) b) y

-- | This is a computationally expensive but highly accurate sigmoidal function tailored to produce
-- a certainty value between 0 and 1 from the 'average' value of a 'Certainty' data type. This
-- computation is tailored to compute a 'sigmoidal' function that evaluates @0.5@ to exactly @0.5@,
-- evaluates @0.0@ to a value close to @0.0@, and evaluates @1.0@ to a value close to @1.0@. This
-- function is defined as:
--
-- @
-- 1 / (1 + 'Prelude.exp'( -4*'Prelude.pi'*( -1/2 + x)))
-- @
--
-- Where @x@ is the 'average' of the 'Certainty'.
--
-- The instantiation of the 'Certainty' data type into the 'CertaintyClass' uses this function for
-- 'Dao.Lens.fetch'ing the 'certainty' value, and 'invSigmoidal' for 'Dao.Lens.alter'ing the
-- 'certainty' value.
sigmoidal :: Floating t => t -> t
sigmoidal t = recip $ 1.0 + exp (negate $ fourPi * (t-0.5))

-- | This is the inverse of the above 'sigmoidal' function. Since the image of 'sigmoidal' is
-- between 0 and 1, this function is 'Prelude.undefined' for values outside of this range.
invSigmoidal :: (Ord t, Floating t, Fractional t) => t -> t
invSigmoidal t = if 0.0 <= t && t <= 1.0 then log (recip t - 1.0) / negate fourPi + 0.5 else
  error "Dao.Certainty.invSigmoidal input value out of bounds (must be between 0.0 and 1.0)"

----------------------------------------------------------------------------------------------------

-- | Sometimes a 'Certainty' value could be used when comparing two objects using a fuzzy logic
-- comparison algorithm. But usually the more different the objects are the larger the fuzzy value
-- grows, and the more similar the objects are, the closer the 'Certainty' value is to zero. This
-- inverts the fuzzy logic: zero values do match, non-zero values do not match. This data type wraps
-- a 'MatchDistance' but has redefined the ordering and matching logic
newtype MatchDistance = MatchDistance Certainty deriving (Eq, Ord, Typeable)

instance Show MatchDistance where
  show (MatchDistance s) = "(MatchDistance "++show (s & average)++")"

_matchDist2 :: (Certainty -> Certainty -> Certainty) -> MatchDistance -> MatchDistance -> MatchDistance
_matchDist2 f (MatchDistance a) (MatchDistance b) = MatchDistance $ f a b

_matchDist1 :: (Certainty -> Certainty) -> MatchDistance -> MatchDistance
_matchDist1 f (MatchDistance a) = MatchDistance $ f a

instance Num MatchDistance where
  (+) = _matchDist2 (+)
  (-) = _matchDist2 (-)
  (*) = _matchDist2 (*)
  abs = _matchDist1 abs
  negate = _matchDist1 negate
  signum = _matchDist1 signum
  fromInteger = MatchDistance . fromInteger

instance Fractional MatchDistance where
  fromRational = MatchDistance . fromRational
  (/) = _matchDist2 (/)

instance Monoid MatchDistance where
  mempty = MatchDistance 1.0
  mappend (MatchDistance a) (MatchDistance b) = MatchDistance $ a <> b

-- | A lens to access the 'Certainty' value within the 'MatchDistance'.
distanceSimilarity :: Monad m => Lens m MatchDistance Certainty
distanceSimilarity = newLens (\ (MatchDistance a) -> a) (\a (MatchDistance _) -> MatchDistance a)

-- | This is the 'MatchDistance' value that indicates two matched data values are exactly the same.
exactSame :: MatchDistance
exactSame = MatchDistance 0.0

-- | A 'MatchDistance' is often used as a fuzzy predicate, where a value closer to zero is
-- 'Prelude.True'. Sometimes it is useful to invert this logic, as in logical-'Prelude.not'. This
-- function inverts a 'MatchDistance' so things that are different are more 'Prelude.True', and
-- things that are more similar are less 'Prelude.True'.
invertDistance :: MatchDistance -> MatchDistance
invertDistance (MatchDistance (Certainty a b)) = MatchDistance $
  let scale = fromRational (toRational b) in Certainty (scale - a) b

-- | This function is useful for comparing patterns in 'Dao.Tree.Tree's to data types in
-- 'Dao.Tree.Tree's. Provide a function for matching a @pat@ pattern to a @dat@ data type that
-- evaluates a 'Dao.Certainty.Certainty' value.
treeDistanceWith
  :: (Eq p, Ord p)
  => (a -> b -> MatchDistance) -> T.Tree p a -> T.Tree p b -> MatchDistance
treeDistanceWith match pat dat = let t = T.intersectionWith match pat dat in
  if T.size t == T.size pat then mconcat $ T.elems T.BreadthFirst t else mempty

-- | Matches a permutation pattern using 'MatchDistance's evaluated by a given distance function to
-- determine whether a pattern element @a@ matches a target element @b@, and sorted with the closest
-- 'MatchDistance's first. It is an ordinary permutation pattern matching function except the given
-- distance function is used to compare elements, rather than an equality function @('Prelude.==')@.
-- The 'distanceSimilarity' function is used internally to decide whether the distance computed
-- between a pattern element @a@ and a target element @b@ are close enough to consider the two
-- elements to be similar.
--
-- The first parameter to this function is a 'Prelude.Double'-percision value indicating the
-- threshold for similarity. If the distance function provided returns a 'MatchDistance' value who's
-- 'average' value is greater than this threshold, the item is not returned to the resulting list.
-- The resulting list contains all matching values where the 'average' 'distanceSimilarity' are
-- within the threshold value, and the list is also sorted so more similar items appear towards the
-- head of the list.
--
-- This function evaluates to an empty list if it is not possible to match every single pattern
-- element @a@. If every single pattern element @a@ does match some target element @b@, the
-- 'Data.Monoid.mappend'ed 'MatchDistance' for each element match is returned, paired with the
-- portion of the target list @[b]@ split into two parts: the part that matched @[a]@ and the part
-- remaining that has not yet matched @[a]@.
matchPermutationPattern
  :: Double -> (a -> b -> MatchDistance) -> [a] -> [b] -> [(MatchDistance, ([b], [b]))]
matchPermutationPattern threshold diff ax = sortBy compareFirst . loop exactSame [] ax where
  compareFirst a = compare (fst a) . fst
  scan ax bx = case bx of
    []   -> return (ax, [])
    b:bx -> let rev = ax++[b] in mplus (return (rev, bx)) (scan rev bx)
  loop c keep ax bx = case ax of
    []   -> return (c, (keep, bx))
    a:ax -> case bx of
      []   -> mzero
      b:bx -> let d = diff a b in
        if abs (d & average . distanceSimilarity) < abs threshold
        then loop (c <> d) (keep ++ [b]) ax bx
        else scan [b] bx >>= \ (skip, bx) -> loop c (keep++skip) (a:ax) bx

-- | Create a 'MatchDistance' value using the 'Dao.Logic.prove' function to prove a
-- 'Dao.Logic.Statement' containing pattern expressions, and a given 'matchDistance' function
matchStatement :: (a -> b -> MatchDistance) -> Statement a -> b -> MatchDistance
matchStatement diff (Statement p) o = loop (elems p) mempty where
  loop px md = case px of
    []                 -> md
    (Conjunction p):px -> loop px $ max md $ mconcat $
      (\ (Satisfy (pos, p)) -> (if pos then id else invertDistance) $ diff p o) <$> elems p

----------------------------------------------------------------------------------------------------

-- | Many data types in this module can do partial comparisons, where a function 'matchDistance' can
-- take a "pattern" parameter and use it to match a "target" parameter. For most data types in this
-- module, 'similarTree's is used to define the 'matchDistance' instance, so expect this function to
-- never be commutative. The first parameter must always be a subset of the second.
class MatchByDistance o where { matchDistance :: o -> o -> MatchDistance; }

instance MatchByDistance MatchDistance where { matchDistance a b = abs $ a-b; }

instance MatchByDistance Certainty where { matchDistance a b = MatchDistance $ abs $ a-b; }

instance (Ord p, MatchByDistance o) => MatchByDistance (T.Tree p o) where
  matchDistance = treeDistanceWith matchDistance

