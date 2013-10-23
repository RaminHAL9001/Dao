-- "src/Dao/Random.hs"  generates objects from integers that can be used
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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Random where

import           Dao.Token
import           Dao.Glob
import           Dao.Object
import           Dao.Object.AST
import qualified Dao.Tree              as T

import           Control.Monad.State

import           Data.List
import           Data.Char
import           Data.Bits
import           Data.Word
import           Data.Ratio
import           Data.Complex
import           Data.Time
import           Data.Array.IArray
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as Bz
import qualified Data.Set              as S
import qualified Data.Map              as M
import qualified Data.IntMap           as I

import           System.Random

----------------------------------------------------------------------------------------------------

-- | A simple, stateful monad for generating arbitrary data types based on pseudo-random numbers
-- without lifting the @IO@ or @ST@ monads, i.e. it can be evaluated in a pure way.
type RandO a = State RandOState a

data RandOState
  = RandOState
    { integerState :: Integer
    , stdGenState  :: StdGen
    , subDepthLim  :: Int
    , subDepth     :: Int
    }

-- | Initializes the 'RandOState' with two integer values: a maximium depth value (limits the number
-- of times you can recursively call 'limSubRandO') and the seed value passed to
-- 'System.Random.mkStdGen'.
initRandOState :: Int -> Int -> RandOState
initRandOState maxDepth seed =
  RandOState
  { integerState = fromIntegral seed
  , stdGenState  = mkStdGen seed
  , subDepthLim  = maxDepth
  , subDepth     = 0
  }

-- | Instantiate your data types into this class if you can generate arbitrary objects from random
-- numbers using the 'RandO' monad.
class HasRandGen o where { randO :: RandO o }

instance HasRandGen () where { randO = return () }
instance HasRandGen Int where { randO = randInt }
instance HasRandGen Integer where { randO = fmap fromIntegral randInt }
instance HasRandGen Char where { randO = fmap chr randInt }
instance HasRandGen UTCTime where
  randO = do
    day <- fmap (ModifiedJulianDay . unsign . flip mod 73000) randInt
    sec <- fmap (fromRational . toRational . flip mod 86400) randInt
    return (UTCTime{utctDay=day, utctDayTime=sec})
instance HasRandGen NominalDiffTime where
  randO = randInteger (fromRational 0) $ \i -> do
    div <- randInt
    fmap (fromRational . (% fromIntegral div) . longFromInts) (replicateM (mod i 2 + 1) randInt)
instance HasRandGen Name where { randO = fmap (fromUStr . randUStr) randInt }
instance HasRandGen UStr where
  randO = fmap (ustr . unwords . fmap (uchars . toUStr)) (randList 0 9 :: RandO [Name])
instance HasRandGen Bool where { randO = fmap (0/=) (nextInt 2) }
instance HasRandGen a => HasRandGen (Maybe a) where
  randO = randO >>= \n -> if n then return Nothing else fmap Just randO

-- | Construct a value from an 'Prelude.Int'. Actually, you have a 50/50 chance of drawing a zero,
-- but this is because zeros are used often for you data type.
randInteger :: a -> (Int -> RandO a) -> RandO a
randInteger zero mkOther = do
  i <- randInt
  let (x, r) = divMod i 2
  if r==0 then return zero else mkOther x

-- | Generate a random object given a maximum recursion limit, a seed value, and a 'RandO' generator
-- function.
genRandWith :: RandO a -> Int -> Int -> a
genRandWith gen maxDepth seed = evalState gen (initRandOState maxDepth seed)

-- | This function you probably will care most about. does the work of evaluating the
-- 'Control.Monad.State.evalState' function with a 'RandOState' defined by the same two parameters
-- you would pass to 'initRandOState'. In other words, arbitrary random values for any data type @a@
-- that instantates 'HasRandGen' can be generated using two integer values passed to this function.
genRand :: HasRandGen a => Int -> Int -> a
genRand maxDepth seed = genRandWith randO maxDepth seed

-- | Take another integer from the seed value. Provide a maximum value, the pseudo-random integer
-- returned will be the seed value modulo this maximum value (so passing 0 will result in a
-- divide-by-zero exception, passing 1 will always return 0). The seed value is then updated with
-- the result of this division. For example, if the seed value is 10023, and you pass 10 to this
-- function, the result returned will be 3, and the new seed value will be 1002.
--    Using numbers generated from this seed value is very useful for generating objects that are
-- somewhat predictable, but the contents of which are otherwise unpredictable. For example, if you
-- want to generate random functions but always with the names "a", "b", or "c", like so:
-- > a(...), b(...), c(...)
-- where the arguments to these functions can be arbitrary, then have your function generator
-- generate the names of these functions using 'nextInt' like so:
-- > 'Prelude.fmap' ('Prelude.flip' 'Prelude.lookup ('Prelude.zip' "abc" [0,1,2]))'nextInt' 3
-- then the arguments to these functions can be generated using 'randInt'. The names of the
-- functions will be predictable for your seed values: any seed value divisible by 3 will generate a
-- function named "a", but the arguments will be arbitrary because they were generated by 'randInt'.
nextInt :: Int -> RandO Int
nextInt maxval = do
  st <- get
  let (i, rem) = divMod (integerState st) (fromIntegral (abs maxval))
  put (st{integerState=i})
  return (fromIntegral rem)

-- | Generate a random integer from the pseudo-random number generator.
randInt :: RandO Int
randInt = state (\st -> let (i, gen) = next (stdGenState st) in (i, st{stdGenState=gen}))

-- | Mark a recursion point. The recusion depth limit set when evaluating a 'randO' computation will
-- not be exceeded. When the number of 'recurse' functions called without returning has reached this
-- limit and this function is evaluated again, the given 'RandO' generator will not be evaluated,
-- the default value will be returned.
recurse :: a -> RandO a -> RandO a
recurse defaultVal fn = do
  st <- get
  if subDepth st > subDepthLim st
    then return defaultVal
    else withState (\st -> st{subDepth = subDepth st + 1}) fn

-- | The number of unique values a 'Prelude.Int' can be, which is @('Prelude.maxBound'+1)*2@.
intBase :: Integer
intBase = (fromIntegral (maxBound::Int) + 1) * 2

-- | Take an ordinary 'Prelude.Int' and make it unsigned by checking if it is a negative value, and
-- if it is, returning the maximum unsigned value plus the negative value, otherwise returning the
-- positive value unchanged. For example, -1 will return @2*('Prelude.maxBound'+1)-1@ and @+1@ will
-- return @1@.
unsign :: Int -> Integer
unsign i = if i<0 then intBase + fromIntegral i else fromIntegral i

-- | Creates a string of digits from 0 to the given @base@ value by converting a random unsigned
-- integer to the list of digits that represents the random integer in that @base@. For example, if
-- you want a list of digits from 0 to 4 to be produced from a number 54, pass 4 as the base, then
-- the number 54. Each digit of the base-4 number representation of 54 will be returned as a
-- separate integer: @[2,1,3]@ (from lowest to highest place value, where 123 in base 10 would
-- return the list @[3,2,1]@).
randToBase :: Int -> Int -> [Int]
randToBase base i = loop (unsign i)  where
  loop i = if i==0 then [] else let (i' , sym) = divMod i b in fromIntegral sym : loop i'
  b      = fromIntegral base

-- | When generating 'Prelude.Integers' from 'Int's, treat a list of 'Int's as a list of symbols in
-- a base M number, where M is the @('Prelude.maxBound'::'Prelude.Int')@ multiplied by two to allow
-- for every negative number to also be considered a unique symbol.
longFromInts :: [Int] -> Integer
longFromInts = foldl (\a b -> a*intBase + unsign b) 0

randEnum :: (Bounded x, Enum x) => x -> x -> RandO x
randEnum lo hi = fmap toEnum (nextInt (abs (fromEnum lo - fromEnum hi)))

----------------------------------------------------------------------------------------------------

randChoice :: forall a . [RandO a] -> RandO a
randChoice items = join (fmap (arr!) (nextInt len)) where
  len = length items
  arr :: Array Int (RandO a)
  arr = listArray (0, len) items

randUStr :: Int -> UStr
randUStr = ustr . B.unpack . getRandomWord

randListOf :: Int -> Int -> RandO a -> RandO [a]
randListOf minlen maxlen rando = recurse [] $ do
  -- half of all lists will be null, unless the 'minlen' parameter is greater than 0
  minlen <- return (min minlen maxlen)
  maxlen <- return (max minlen maxlen)
  empt <- if minlen==0 then nextInt 2 else return 0
  if empt==1
    then return []
    else do
      ln <- nextInt (maxlen-minlen)
      replicateM (minlen+ln) rando

randList :: HasRandGen a => Int -> Int -> RandO [a]
randList lo hi = randListOf lo hi randO

randRational :: Int -> RandO Rational
randRational i0 = do
  let (i1, len1) = divMod i0 4
      (_ , len2) = divMod i1 4
  a <- fmap longFromInts (replicateM (len1+1) randInt)
  b <- fmap longFromInts (replicateM (len2+1) randInt)
  return (a%b)

getRandomWord :: Int -> B.ByteString
getRandomWord i = randomWords ! (mod i (rangeSize (bounds randomWords) - 1))

randomWords :: Array Int B.ByteString
randomWords = listArray (0, length list) (map B.pack list) where
  list = words $ unwords $
    [ "a academia accomplished added also an analysis and application applications apply are arent slim"
    , "argument arguments as at avoids be because been behavior between book both by calculus plus were"
    , "calling can change changes code commercial computability computation computer concepts earth was"
    , "constructs contrast conversely declarative definition depending depends describing metal key fee"
    , "designed developed development difference different domains domain easier effects fire water add"
    , "elaborations elements eliminating emphasize entscheidungsproblem eschewing star best least being"
    , "especially evaluation example executing expression facilitate financial formal greatest open etc"
    , "functional has have hope how however imperative industrial input investigate is home close where"
    , "it key lack lambda language languages largely like make many math mathematical may from flow she"
    , "motivations much mutable notion numeric of on one ones only organizations output paradigm pit he"
    , "specific pioneering practice predict produce program programming prominent purely rather trust I"
    , "recursion referential result roots same science side so software some specifically state move me"
    , "statistics style subject such supported symbolic system than that the they child this super mesh"
    , "transparency treats twice understand use used value values variety viewed which wide will bill X"
    , "dates times database structured listing setting dictionary returning throwing catching law factor"
    , "option procedure alpha beta electron proton neutron shift hard soft bean beam fix drug undo minus"
    , "field magic latice jump assemble area volume interesting slice sector region cylinder sphere plan"
    , "inside without trying patterned rules"
    ]

