-- "Dao/Count.hs"  newtype wrapping the 'Data.Int.Int64' data type but
-- instantiating 'Data.Monoid.Monoid' with integer addition.
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

-- | This module provides the 'Count' data type, which is a newtype wrapping
-- the 'Data.Word.Word' data type used to keep count characters, lines,
-- columns, or whatever. Instantiates 'Data.Monoid.Monoid' with the functions
-- for integer addition.
module Dao.Count (Count(Count), countToInt) where

import           Control.DeepSeq

import qualified Data.Array.IArray as A
import           Data.Int
import           Data.Monoid

-- | A newtype wrapping the 'Data.Word.Word' data type used to keep count characters, lines,
-- columns, or whatever. Instantiates 'Data.Monoid.Monoid' with the functions for integer addition.
-- All classes necessary for using a 'Count' as an ordinary integer value are instantiated. This
-- means you can write @1::'Count'@ and it will wokr.
newtype Count = Count { countToInt :: Int64 } deriving (Eq, Ord, Bounded, A.Ix)

instance Show Count where { show (Count o) = show o }

instance Read Count where { readsPrec p = fmap (\ (a, rem) -> (Count a, rem)) . readsPrec p }

instance Monoid Count where
  mempty = Count 0
  mappend = (+)

instance Num Count where
  (Count a) + (Count b) = Count $ a+b
  (Count a) * (Count b) = Count $ a*b
  (Count a) - (Count b) = Count $ a-b
  negate (Count a) = Count $ negate a
  abs = id
  signum (Count a) = if a==0 then 0 else 1
  fromInteger = Count . fromInteger

instance Integral Count where
  quotRem (Count a) (Count b) = let (a', b') = quotRem a b in (Count a', Count b')
  toInteger (Count a) = toInteger a

instance Real Count where { toRational (Count a) = toRational a }

instance Enum Count where
  succ (Count a) = Count $ succ a
  pred (Count a) = Count $ pred a
  toEnum = Count . toEnum
  fromEnum (Count a) = fromIntegral a
  enumFrom (Count a) = fmap Count $ enumFrom a
  enumFromThen (Count a) (Count b) = fmap Count $ enumFromThen a b
  enumFromTo (Count a) (Count b) = fmap Count $ enumFromTo a b
  enumFromThenTo (Count a) (Count b) (Count c) = fmap Count $ enumFromThenTo a b c

instance NFData Count where { rnf (Count i) = deepseq i (); }

