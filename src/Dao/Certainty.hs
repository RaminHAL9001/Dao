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
-- applying the rational number value in the 'Certainty' value to the sigmoidal function.
--
-- The utility of the 'Certainty' data type mostly comes from it's ability to keep a running
-- 'average' value through use of 'Data.Monoid.mconcat'. It is easy to convert a list of
-- 'Prelude.Double' values to a list of 'Certainty' values, 'Data.Monoid.mconcat' them together,
-- retrieve the 'average', continue 'Data.Monoid.mconcat'ing the values, and retrieve the new
-- 'average'.
--
-- while the instantiation of 'Prelude.Num'
-- and 'Prelude.Fractional' otherwise allows for treating the 'Certainty' data type as ordinary
-- 'Prelude.Double'-percsion number data.
module Dao.Certainty
  ( Certainty, sumTotal, sampleSize, average, certainty, onTotals,
    sigmoidal, invSigmoidal
  )
  where

import           Dao.Lens
import           Dao.TestNull

import           Data.Monoid
import           Data.Typeable

-- not exported
i2d :: Integer -> Double
i2d = fromRational . toRational

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

-- | Use this 'Dao.Lens.Lens' to get the value of the sigmoidal of the inner rational value of the
-- 'Certainty', or to set what the value of the sigmoidal should be given the 'sampleSize' and
-- 'sumTotal' value of the 'Certainty' value.
certainty :: Monad m => Lens m Certainty Double
certainty =
  newLens (\  (Certainty a b) -> sigmoidal $ a / i2d b)
          (\a (Certainty _ b) -> Certainty (invSigmoidal a * i2d b) b)

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

sigmoidal :: Floating t => t -> t
sigmoidal t = recip $ 1.0 + exp (negate t)

invSigmoidal :: (Ord t, Floating t, Fractional t) => t -> t
invSigmoidal t = if 0.0 <= t && t <= 1.0 then negate $ log $ recip t - 1.0 else
  error "Dao.Certainty.invSigmoidal input value out of bounds (must be betwee 0.0 and 1.0)"

