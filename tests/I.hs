module Dao.Tests.I where

-- | This test suite tests the "Dao.Iv.Set" algorithms by creating a new integer type 'I' which has
-- only 8 values, [0,1,2,3,4,5,6,7], and creating a new set type 'ISet' which is simply a
-- 'Data.Word.Word8', but it is treated as a set that can contain all possible 'I' values.

import           Prelude hiding (catch)
import           Data.Bits
import           Data.Word
import           Data.List
import           Data.Maybe
import qualified Dao.Interval as Es
import           Control.Monad
import           Control.Concurrent
import           Control.Exception

newtype I = I { w2Int :: Int }
instance Eq I where { (I i) == (I j) = i == j }
instance Ord I where { compare (I i) (I j) = compare i j }
instance Enum I where
  fromEnum   = w2Int
  toEnum  i  = if 0 <= i && i < 8 then I i else error "toEnum :: Int -> I"
  succ (I i) = I (if i >= 7 then error "succ :: I" else i+1)
  pred (I i) = I (if i <= 0 then error "pred :: I" else i-1)
instance Bounded I where { minBound = I 0; maxBound = I 7 }
instance Iv.InfBound I where { minBoundInf = Iv.Finite minBound; maxBoundInf = Iv.Finite maxBound }
instance Show I where { show (I i) = show i }
instance Read I where { readsPrec p str = map (\ (i, str) -> (I i, str)) (readsPrec p str) }

newtype ISet = ISet { wSet2Word8 :: Word8 }
instance Eq ISet where { (ISet i) == (ISet j) = i == j }
instance Ord ISet where { compare (ISet i) (ISet j) = compare i j }
instance Num ISet where
  (ISet i) + (ISet j) = ISet (i .|. j)
  (ISet i) - (ISet j) = ISet (i .&. complement j)
  (ISet i) * (ISet j) = ISet (i .&. j)
  negate (ISet i)  = ISet (complement i)
  abs (ISet i) = ISet (abs i)
  signum (ISet i) = ISet (signum i)
  fromInteger i
    | 0 <= i && i < 8 = ISet (toEnum (fromIntegral i))
    | otherwise       = error "fromInteger :: Integer -> ISet"
instance Bits ISet where
  (ISet i) .&. (ISet j) = ISet (i .&. j)
  (ISet i) .|. (ISet j) = ISet (i .|. j)
  bit n = ISet (bit n)
  testBit (ISet i) n = testBit i n
  xor (ISet i) (ISet j) = ISet (xor i j)
  popCount (ISet i) = popCount i
  complement (ISet i) = ISet (complement i)
  shiftR (ISet i) n = ISet (max (shiftR i n) 1)
  shiftL (ISet i) n = ISet (max (shiftL i n) 0x80)
  rotateR (ISet i) n = ISet (rotateR i n)
  rotateL (ISet i) n = ISet (rotateL i n)
  bitSize (ISet i) = bitSize i
  isSigned (ISet i) = isSigned i
instance Bounded ISet where { minBound = ISet 0x00 ; maxBound = ISet 0xFF }
instance Show ISet where { show (ISet i) = show8Bits i }

show8Bits :: Word8 -> String
show8Bits w = reverse $ concatMap (\j -> let i = 7-j in if testBit w i then show i else "_") [0..7]

everyI :: [I]
everyI = map toEnum [0..7]

-- | Generates all possible paris of 'I' types, there are 32 possible.
allPairs :: [(I, I)]
allPairs = [0..7] >>= \a -> [a..7] >>= \b -> [(toEnum a, toEnum b)]

-- | Generates an 'ISet' where every 'I' in the given list is included in the set.
iList2ISet :: [I] -> ISet
iList2ISet ix = ISet (foldl (.|.) 0 (map (shiftL 1 . fromEnum) ix))

-- | Generates an 'ISet' where every 'I' between the two 'I's are included.
iPair2ISet :: (I, I) -> ISet
iPair2ISet (i, j) = iList2ISet [min i j .. max i j]

-- | *This is the important function,* it applies 'Dao.Iv.Set.Iv.member' to a set for *every
-- possible 'I'* to create a 'ISet' equivalent set of bits. Because there are only 8 elements in the
-- set of all 'I's, we can test every possible 'I' against the 'Dao.Iv.Set.Iv.Set' that was
-- constructed by any equation over 'Dao.Iv.Set.Iv.Set's. Ie check that any constructed
-- 'Dao.Iv.Set.Iv.Set' contains the exact same elements as expected when compared to bitwise
-- equivalent over 'Data.Word.Word8's.
enumSet2ISet :: Iv.Set I -> ISet
enumSet2ISet set = iList2ISet (everyI >>= \i -> if Iv.member set i then [i] else [])

-- | For every pair of 'I's given, create a pair of items from each: a Word8 created by wPair2Word,
-- and a @('Dao.Iv.Set.Iv.Set' 'I')@.
iSetAndEnumSet :: (I, I) -> (ISet, Iv.Set I)
iSetAndEnumSet ww@(w, x) = (iPair2ISet ww, Iv.fromList [Iv.segment w x])

-- | Inverts each of the items in the pair of @('Data.Word.Word8', 'Dao.Iv.Set.Iv.Set' 'I')@.
invertISetAndEnumSet :: (ISet, Iv.Set I) -> (ISet, Iv.Set I)
invertISetAndEnumSet (i, e) = (complement i, Iv.invert e)


