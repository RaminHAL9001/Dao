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

{-# LANGUAGE ScopedTypeVariables #-}

module RandObj where

import           Dao.Pattern
import           Dao.Object
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

type RandO a = State RandOState a

data RandOState = RandOState{ integerState :: Integer, stdGenState :: StdGen }

class HasRandGen o where { randO :: RandO o }

nextInt :: Int -> RandO Int
nextInt maxval = do
  st <- get
  let (i, rem) = divMod (integerState st) (fromIntegral maxval)
  put (st{integerState=i})
  return (fromIntegral rem)

randInt :: RandO Int
randInt = state (\st -> let (i, gen) = next (stdGenState st) in (i, st{stdGenState=gen}))

subRandOFunc :: RandO a -> RandO a
subRandOFunc fn = randInt >>= \i -> withState (\st -> st{integerState = fromIntegral i}) fn

subRandO :: HasRandGen a => RandO a
subRandO = subRandOFunc randO

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
-- integer to the list of digits that represents the random integer in that @base@.
randToBase :: Int -> Int -> [Int]
randToBase base i = loop (unsign i)  where
  loop i = if i==0 then [] else let (i' , sym) = divMod i b in fromIntegral sym : loop i'
  b      = fromIntegral base

-- | When generating 'Prelude.Integers' from 'Int's, treat a list of 'Int's as a list of symbols in
-- a base M number, where M is the @('Prelude.maxBound'::'Prelude.Int')@ multiplied by two to allow
-- for every negative number to also be considered a unique symbol.
longFromInts :: [Int] -> Integer
longFromInts = foldl (\a b -> a*intBase + unsign b) 0

randOFromList :: forall a . HasRandGen a => [RandO a] -> RandO a
randOFromList items = join (fmap (arr!) (nextInt len)) where
  len = length items
  arr :: Array Int (RandO a)
  arr = listArray (0, len) items

randUStr :: Int -> UStr
randUStr = ustr . B.unpack . getRandomWord

randListOf :: RandO a -> RandO [a]
randListOf rando = do
  i0 <- nextInt 40
  let (i1, nonNull) = divMod i0 2
  if nonNull==0 then return [] else replicateM i1 rando

randList :: HasRandGen a => RandO [a]
randList = randListOf subRandO

randObjMap :: (map Object -> Object) -> ([(key, Object)] -> map Object) -> RandO key -> RandO Object
randObjMap objConstruct mapConstruct keygen = randList >>= \ox ->
  fmap (objConstruct . mapConstruct) (forM ox (\obj -> keygen >>= \key -> return (key, obj)))

instance HasRandGen Object where
  randO = randOFromList $
    [ return ONull
    , return OTrue
    , fmap OType randO
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
        len <- nextInt 4
        flip fmap (replicateM (len+1) randInt) $
          OString . ustr . unwords . map (B.unpack . getRandomWord)
    , fmap ORef randO
    , fmap OPair (liftM2 (,) randO randO)
    , fmap OList randList
    , fmap (OSet . S.fromList) randList
    , do  hi <- nextInt 12
          lo <- nextInt 8
          fmap (OArray . listArray (fromIntegral lo, fromIntegral (lo+hi))) (replicateM hi subRandO)
    , randObjMap ODict   M.fromList (fmap randUStr randInt)
    , randObjMap OIntMap I.fromList randInt
    , do  branchCount <- nextInt 4
          cuts <- fmap (map (+1) . randToBase 6) randInt
          fmap (OTree . T.fromList . concat) $ replicateM (branchCount+1) $ do
            wx <- replicateM 6 (fmap randUStr randInt)
            forM cuts $ \cut -> do
              obj <- subRandO
              return (take cut wx, obj)
    , fmap OPattern randO
    , fmap OScript randO
    , fmap ORule randO
    , do  i <- nextInt 10
          fmap (OBytes . Bz.pack . map fromIntegral . concat) $
            replicateM i (fmap (randToBase 256) randInt)
    ]

instance HasRandGen TypeID where
  randO = fmap toEnum (nextInt (fromEnum AnyType))

instance HasRandGen Reference where
  randO = randOFromList $
    [ fmap (IntRef . fromIntegral . abs) randInt
    , fmap LocalRef  single
    , fmap StaticRef single
    , fmap QTimeRef  multi
    , fmap GlobalRef multi
    , liftM2 ProgramRef (fmap (ustr . (++".dao") . uchars) single) subRandO
    , liftM2 FileRef (fmap (ustr . (++".idea") . uchars) single) multi
    , liftM2 Subscript subRandO subRandO
    , fmap MetaRef subRandO
    ]
    where
      single = fmap randUStr randInt
      multi = do
        i0 <- randInt
        let (i1, len) = divMod i0 4
        fmap ((randUStr i1 :) . map randUStr) (replicateM len randInt)

instance HasRandGen Pattern where
  randO = do
    len <- fmap (+6) (nextInt 6)
    i <- randInt
    let loop len i =
          if len<=1 then [] else let (i', n) = divMod i len 
          in  (n+1) : loop (len-n-1) i'
        cuts = loop len i
    tx <- fmap (([]:) . map (\t -> if t==0 then [AnyOne] else [Wildcard]) . randToBase 2) randInt
    let loop tx cuts ax = case cuts of
          []       -> [ax]
          cut:cuts ->
            let (wx, ax') = splitAt cut ax
                (t,  tx') = splitAt 1 tx
            in  t ++ wx : loop tx' cuts ax'
    patUnits <- fmap (concat . loop tx cuts) $
      replicateM len (fmap (Single . randUStr) randInt)
    return (Pattern{getPatUnits=patUnits, getPatternLength=length patUnits})

instance HasRandGen Subroutine where
  randO = do
    pats <- randList
    scrp <- randList >>= mapM randComment
    return (Subroutine{argsPattern = pats, subSourceCode = scrp, getSubExecutable = undefined})

instance HasRandGen ObjPat where
  randO = error "TODO: instantiate ObjPat into the HasRandGen class"

instance HasRandGen RuleExpr where
  randO = do
    pat <- randList >>= mapM randComment >>= randComment
    act <- randList >>= mapM randComment >>= randComment
    return (RuleExpr{rulePattern = pat, ruleAction = act})

instance HasRandGen ScriptExpr where
  randO = undefined

instance HasRandGen ObjectExpr where
  randO = undefined

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

randComment :: a -> RandO (Com a)
randComment a = do
  typ <- fmap (flip mod 24 . unsign) randInt
  case typ of
    0 -> do
      before <- genComments
      after  <- genComments
      return (ComAround before a after)
    1 -> do
      before <- genComments
      return (ComBefore before a)
    2 -> do
      after <- genComments
      return (ComAfter a after)
    _ -> return (Com a)
    where
      genComments = do
        i0 <- randInt
        let (i1, many) = divMod i0 4
            (i2, typn) = divMod i1 16
            typx = take many (randToBase 2 typn ++ replicate 4 0)
            lenx = map (+1) (randToBase 29 i2)
            com typ = if typ==0 then EndlineComment else InlineComment
        forM (zip typx lenx) $ \ (typ, len) ->
          fmap (com typ . ustr . unwords . map (B.unpack . getRandomWord)) (replicateM len randInt)

----------------------------------------------------------------------------------------------------

randomWords :: Array Int B.ByteString
randomWords = listArray (0, length list) (map B.pack list) where
  list = words $ unwords $
    [ "a academia accomplished added also an analysis and application applications apply are arent"
    , "argument arguments as at avoids be because been behavior between book both by calculus"
    , "calling can change changes code commercial computability computation computer concepts"
    , "constructs contrast Conversely data declarative definition depending depends describing"
    , "designed developed development difference different domains domainspecific easier effects"
    , "elaborations elements eliminating emphasized emphasizes entscheidungsproblem eschewing"
    , "especially evaluation example executing expression facilitate financial for formal from"
    , "function functional has have hope how however imperative in industrial input investigate is"
    , "it key lack lambda language languages largely like make many math mathematical may with"
    , "motivations much mutable notion numeric of on one ones only organizations output paradigm"
    , "pecific pioneering practice predict produce program programming prominent purely rather"
    , "recursion referential result roots same science side so software some specifically state"
    , "statistics style subject such supported symbolic system than that the they this times to"
    , "transparency treats twice understand use used value values variety viewed which wide will"
    ]

getRandomWord :: Int -> B.ByteString
getRandomWord i = randomWords ! (mod i $ rangeSize $ bounds randomWords)

