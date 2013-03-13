-- "tests/RandObj.hs"  generates objects from integers that can be used
-- to test the parsers/pretty-printer, and the binary encoder/decoder.
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


module RandObj where

import           Dao.Object

import           Control.Monad.State

import           Data.Char
import           Data.Bits
import           Data.Word
import           Data.Ratio
import           Data.Complex
import           Data.Time
import           Data.Array.IArray
import qualified Data.ByteString.Char8 as B

import           System.Random

----------------------------------------------------------------------------------------------------

data RandOState = RandOState{ integerState :: Integer, stdGenState :: StdGen }

type RandO a = State RandOState a

nextInt :: Int -> RandO Int
nextInt maxval = do
  st <- get
  let (i, rem) = divMod (integerState st) (fromIntegral maxval)
  put (st{integerState=i})
  return (fromIntegral rem)

randInt :: RandO Int
randInt = state (\st -> let (i, gen) = next (stdGenState st) in (i, st{stdGenState=gen}))

class HasRandGen o where { randO :: RandO o }

-- | The number of unique values a 'Prelude.Int' can be, which is @('Prelude.maxBound'+1)*2@.
intBase :: Integer
intBase = (fromIntegral (maxBound::Int) + 1) * 2

-- | Take an ordinary 'Prelude.Int' and make it unsigned by checking if it is a negative value, and
-- if it is, returning the maximum unsigned value plus the negative value, otherwise returning the
-- positive value unchanged. For example, -1 will return @2*('Prelude.maxBound'+1)-1@ and @+1@ will
-- return @1@.
unsign :: Int -> Integer
unsign i = if i<0 then intBase + fromIntegral i else fromIntegral i

-- | When generating 'Prelude.Integers' from 'Int's, treat a list of 'Int's as a list of symbols in
-- a base M number, where M is the @('Prelude.maxBound'::'Prelude.Int')@ multiplied by two to allow
-- for every negative number to also be considered a unique symbol.
longFromInts :: [Int] -> Integer
longFromInts = foldl (\a b -> a*intBase + unsign b) 0

instance HasRandGen Object where
  randO = join (fmap (arr!) (nextInt $ rangeSize $ bounds arr)) where
    arr :: Array Int (RandO Object)
    arr = listArray (0, length items) items
    items =
      [ return ONull
      , return OTrue
      , randO >>= return . OType
      , randInteger (OInt  0) $ \i -> randInt >>= \j -> return (OInt$fromIntegral$ i*j)
      , randInteger (OWord 0) $ \i -> randInt >>= \j -> return (OWord$fromIntegral$abs$ i*j)
      , randInteger (OLong 0) $ \i ->
          replicateM (mod i 4 + 1) randInt >>= return . OLong . longFromInts
      , randInteger (OFloat 0) (fmap (OFloat . fromRational) . randRational)
      , randInteger (ORatio 0) (fmap ORatio . randRational)
      , randInteger (OComplex 0) $ \i0 -> do
          let (i1, rem) = divMod i0 4
          real <- fmap fromRational (randRational i1)
          cplx <- fmap fromRational (randRational rem)
          return (OComplex (real:+cplx))
      , do  day <- fmap (ModifiedJulianDay . unsign) randInt
            sec <- fmap (fromRational . toRational . flip mod 86400) randInt
            return (OTime (UTCTime{utctDay=day, utctDayTime=sec}))
      , randInteger (ODiffTime 0) $ \i -> do
          div <- randInt
          fmap (ODiffTime . fromRational . (% fromIntegral div) . longFromInts) $
            replicateM (mod i 2 + 1) randInt
      , randInteger (OChar '\n') (\i -> return (OChar $ chr $ mod i $ ord (maxBound::Char)))
      , randInteger (OString nil) $ \i -> do
          len <- nextInt 50
          let n = rangeSize (bounds randomWords)
          flip fmap (replicateM (len+1) randInt) $
            OString . ustr . unwords . map (B.unpack . (randomWords!) . (flip mod n))
      ]

instance HasRandGen TypeID where
  randO = fmap toEnum (nextInt (fromEnum AnyType))

randInteger :: Object -> (Int -> RandO Object) -> RandO Object
randInteger zero mkOther = do
  i <- randInt
  let (x, r) = divMod i 2
  if r==0 then return zero else mkOther x

randRational :: Int -> RandO Rational
randRational i0 = do
  let (i1, len1) = divMod i0 4
      (_ , len2) = divMod i1 4
  a <- fmap longFromInts (replicateM (len1+1) randInt)
  b <- fmap longFromInts (replicateM (len2+1) randInt)
  return (a%b)

----------------------------------------------------------------------------------------------------

randomWords :: Array Int B.ByteString
randomWords = listArray (0, length list) (map B.pack list) where
  list = words $ concat $
    [ "a academia accomplished added also an analysis and application applications apply are arent"
    , "argument arguments as at avoids be because been behavior between book both by calculus"
    , "calling can change changes code commercial computability computation computer concepts"
    , "constructs contrast Conversely data declarative definition depending depends describing"
    , "designed developed development difference different domains domainspecific easier effects"
    , "elaborations elements eliminating emphasized emphasizes entscheidungsproblem eschewing"
    , "especially evaluation example executing expression facilitate financial for For formal from"
    , "function functional Functional functions has have hope how however imperative in industrial"
    , "input investigate is it key lack lambda language languages largely like make many math"
    , "mathematical may motivations much mutable notion numeric of on one ones only organizations"
    , "output paradigm pecific Perl pioneering practice predict produce program programming"
    , "prominent purely rather recursion referential result roots same science side so software"
    , "some specifically SQL state statistics style subject such supported symbolic system than"
    , "that the they this times to transparency treats twice understand use used value values"
    , "variety viewed which wide will with XML"
    ]

