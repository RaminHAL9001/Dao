-- "src/Dao/EnumSet.hs"  defines the Segment data type used to denote
-- a possibly infinite subset of contiguous elements of an Enum data type.
-- 
-- Copyright (C) 2008-2012  Ramin Honary.
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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Dao.EnumSet
  ( -- * The 'EnumInf' data type
    EnumInf(EnumNegInf, EnumPosInf, EnumPoint)
  , stepDown, stepUp, toPoint, enumIsInf
  , BoundedInf, minBoundInf, maxBoundInf
    -- * the 'Segment' data type
  , Segment, segment, single, negInfTo, toPosInf, infinite, enumInfSeg
  , canonicalSegment, toBounded, toBoundedPair, segmentMember
    -- * Predicates on 'Segment's
  , containingSet, numElems, isWithin, segmentHasEnumInf, segmentIsInfinite
    -- * Set Operators for 'Segment's
  , areIntersecting, areConsecutive
  , segmentUnion, segmentIntersect, segmentDelete, segmentInvert, segmentNub
    -- * The 'EnumSet' data type
  , EnumSet, emptySet, infiniteSet, enumSet, range, point
  , listSegments, setMember, setIsNull, setIsSingleton
    -- * Set Operators for 'EnumSet's
  , setUnion, setIntersect, setDelete, setInvert, setXUnion
    -- * Miscelaneous
  , upperTriangular, associativeProduct, nonAssociativeProduct
  )
  where

import           Data.Monoid
import           Data.Bits
import           Data.List
import           Data.Ratio
import           Control.Monad

-- | Like 'Prelude.Bounded', except the bounds might be infinite, and return 'EnumNegInf' or
-- 'EnumPosInf' for the bounds. Using the GHC "flexible instances" and "undecidable instances"
-- feature, any data type that is an instance of 'Prelude.Bounded' is also a member of 'BoundInf'.
class BoundedInf c where
  minBoundInf :: EnumInf c
  maxBoundInf :: EnumInf c

instance Bounded c => BoundedInf c where
  minBoundInf = EnumPoint minBound
  maxBoundInf = EnumPoint maxBound

instance BoundedInf Integer where
  minBoundInf = EnumNegInf
  maxBoundInf = EnumPosInf

instance BoundedInf (Ratio Integer) where
  minBoundInf = EnumNegInf
  maxBoundInf = EnumPosInf

-- | Enumerable elements with the possibility of infinity.
data EnumInf c
  = EnumNegInf -- ^ negative infinity
  | EnumPosInf -- ^ positive infinity
  | EnumPoint c -- ^ a single point
  deriving Eq

enumIsInf :: EnumInf c -> Bool
enumIsInf c = case c of
  EnumNegInf -> True
  EnumPosInf -> True
  _          -> False

instance Ord c => Ord (EnumInf c) where
  compare a b = f a b where
    f a b | a == b = EQ
    f EnumNegInf _ = LT
    f _ EnumNegInf = GT
    f EnumPosInf _ = GT
    f _ EnumPosInf = LT
    f (EnumPoint a) (EnumPoint b) = compare a b

instance Show c => Show (EnumInf c) where
  show e = case e of
    EnumPoint c -> show c
    EnumNegInf  -> "-inf"
    EnumPosInf  -> "+inf"

instance Functor EnumInf where
  fmap f e = case e of
    EnumNegInf  -> EnumNegInf
    EnumPosInf  -> EnumPosInf
    EnumPoint e -> EnumPoint (f e)

-- | Increment a given value, but if the value is 'Prelude.maxBound', return 'EnumPosInf'. In some
-- circumstances this is better than incrementing with @'Data.Functor.fmap' 'Prelude.succ'@ because
-- 'Prelude.succ' evaluates to an error when passing 'Prelude.maxBound' as the argument. This
-- function will never evaluate to an error.
stepUp :: (Eq c, Enum c, BoundedInf c) => EnumInf c -> EnumInf c
stepUp x = if x==maxBoundInf then EnumPosInf else fmap succ x

-- | Decrement a given value, but if the value is 'Prelude.minBound', returns 'EnumNegInf'. In some
-- circumstances this is better than incrementing @'Data.Functor.fmap' 'Prelude.pred'@ because
-- 'Prelude.pred' evaluates to an error when passing 'Prelude.maxBound' as the argument. This
-- function will never evaluate to an error.
stepDown :: (Eq c, Enum c, BoundedInf c) => EnumInf c -> EnumInf c
stepDown x = if x==minBoundInf then EnumNegInf else fmap pred x

-- | Retrieve the value contained in an 'EnumInf', if it exists.
toPoint :: EnumInf c -> Maybe c
toPoint c = case c of
  EnumPoint c -> Just c
  _           -> Nothing

-- | A enumInfSeg of 'EnumInf' items is a subset of consectutive items in the set of all @c@ where @c@
-- is any type satisfying the 'Prelude.Enum' class. To construct a 'Segment' object, use 'enumInfSeg'.
data Segment c
  = Single  (EnumInf c)
  | Segment (EnumInf c) (EnumInf c)
  deriving Eq
  -- NOTE: the constructor for this data type is not exported because all of the functions in this
  -- module that operate on 'Segment's make the assumption that the first parameter *less than* the
  -- second parameter. To prevent anyone from screwing it up, the constructor is hidden and
  -- constructing a 'Segment' must be done with the 'enumInfSeg' function which checks the parameters.

-- not exported
mkSegment :: Eq c => EnumInf c -> EnumInf c -> Segment c
mkSegment a b
  | a==b      = Single a
  | otherwise = Segment a b

-- | 'Prelude.Ord'ering is defined for 'Segment's to make certain 'EnumSet' operations more
-- efficient, not for the sake of intuition. 'Segment's are ordered firstly by their position, and
-- secondly by the size of their set of elements second.
instance Ord c => Ord (Segment c) where
  compare x y = case x of
    Single    a    -> case y of
      Single  b      -> compare a b
      Segment b  b'  -> if a==b then LT else compare a b
    Segment   a  b -> case y of
      Single  a'     -> if a==b then GT else compare a a'
      Segment a' b'  -> if a==a' then compare b' b else compare a a'

showSegment :: Show c => Segment c -> String
showSegment seg = case seg of
  Single  x   -> "at "++show x
  Segment x y -> "from "++show x++" to "++show y

-- | This gets rid of as many intinite elements as possible. All @'Single' 'EnumPosInf'@ and
-- @'Single' 'EnumNegInf'@ points are eliminated, and if an 'EnumNegInf' or 'EnumPosInf' can be
-- replaced with a corresponding 'minBoundInf' or 'maxBoundInf', then it is. This function is
-- intended to be used as a list monadic function, so use it like so:
-- @let myListOfSegments = [...] in myListOfSegments >>= 'delInfPoints'@
canonicalSegment :: (Eq c, BoundedInf c) => Segment c -> [Segment c]
canonicalSegment seg = nonInf seg >>= \seg -> case seg of
  Segment a b -> nonInf (mkSegment (bounds a) (bounds b))
  seg         -> [seg]
  where
    nonInf seg = case seg of
      Single  EnumNegInf -> []
      Single  EnumPosInf -> []
      Single  e          -> [Single e]
      seg                -> [seg]
    bounds x = case x of
      EnumNegInf -> minBoundInf
      EnumPosInf -> maxBoundInf
      x          -> x

instance Show c => Show (Segment c) where { show seg = "("++showSegment seg++")" }

-- | A predicate evaluating whether or not a segment includes an 'EnumPosInf' or 'EnumNegInf' value.
-- This should not be confused with a predicate evaluating whether the set of elements included by
-- the range is infinite, because types that are instances of 'Prelude.Bounded' may also contain
-- 'EnumPosInf' or 'EnumNegInf' elements, values of these types may be evaluated as "infintie" by
-- this function, even though they are 'Prelude.Bounded'. To check if a segment is infinite, use
-- 'segmentIsInfinite' instead.
segmentHasEnumInf :: Segment c -> Bool
segmentHasEnumInf seg = case seg of
  Single  c   -> enumIsInf c
  Segment a b -> enumIsInf a || enumIsInf b

-- | A predicate evaluating whether or not a segment is infinite. Types that are 'Prelude.Bounded'
-- are always finite, and thus this function will always evaluate to 'Prelude.False' for these
-- types.
segmentIsInfinite :: BoundedInf c => Segment c -> Bool
segmentIsInfinite seg = case [Single minBoundInf, Single maxBoundInf, seg] of
  [Single a, Single b, c] | enumIsInf a || enumIsInf b -> case c of
    Single  c   -> enumIsInf c
    Segment a b -> enumIsInf a || enumIsInf b
  _             -> False

-- | Construct a 'Segment' from two 'EnumInf' items. *NOTE* if the 'EnumInf' type you are
-- constructing is an instance of 'Prelude.Bounded', use the 'boundedSegment' constructor instead of
-- this function.
enumInfSeg :: (Ord c, Enum c, BoundedInf c) => EnumInf c -> EnumInf c -> Segment c
enumInfSeg a b = seg a b where
  seg a b = construct (ck minBoundInf EnumNegInf a) (ck maxBoundInf EnumPosInf b)
  ck inf subst x = if inf==x then subst else x
  construct a b
    | a == b    = Single  a
    | a < b     = Segment a b
    | otherwise = Segment b a

-- | Construct a 'Segment' from two values.
segment :: Ord c => c -> c -> Segment c
segment a b = mkSegment (EnumPoint (min a b)) (EnumPoint (max a b))

-- | Construct a 'Segment' that is only a single unit, i.e. it starts at X and ends at X.
single :: Ord c => c -> Segment c
single a = segment a a

-- | Construct a 'Segment' from negative infinity to a given value.
negInfTo :: BoundedInf c => c -> Segment c
negInfTo a = Segment minBoundInf (EnumPoint a)

-- | Construct a 'Segment' from a given value to positive infinity.
toPosInf :: BoundedInf c => c -> Segment c
toPosInf a = Segment (EnumPoint a) maxBoundInf

-- | Construct the infinite 'Segment'
infinite :: Segment c
infinite = Segment EnumNegInf EnumPosInf

-- | Tests whether an element is a member is enclosed by the 'Segment'.
segmentMember :: Ord c => c -> Segment c -> Bool
segmentMember c seg = case seg of
  Single  (EnumPoint x) -> c == x
  Segment lo hi         -> let e = EnumPoint c in lo <= e && e <= hi
  _                     -> False

-- | Construct a 'Segment', like the 'enumInfSeg' constructor above, however does not require 'EnumInf'
-- parameters as inputs. This function performs the additional check of testing whether or not a
-- value is equivalent to 'Prelude.minBound' or 'Prelude.maxBound', and if it is, replaces that
-- value with 'EnumNegInf' or 'EnumPosInf' respectively. In other words, you can use
-- 'Prelude.minBound' in place of EnumNegInf and 'Prelude.maxBound' in place of 'EnumPosInf' without
-- changing the semantics of the data structure as it is used throughout the program.
-- boundedSegment :: (Ord c, Enum c, Bounded c) => c -> c -> Segment c
-- boundedSegment a b = if a>b then co b a else co a b where
--    co a b = enumInfSeg (f a minBound EnumNegInf) (f b maxBound EnumPosInf)
--    f x bound inf = if x==bound then inf else EnumPoint x

-- | If an 'EnumInf' is also 'Prelude.Bounded' then you can convert it to some value in the set of
-- 'Prelude.Bounded' items. 'EnumNegInf' translates to 'Prelude.minBound', 'EnumPosInf' translates
-- to 'Prelude.maxBound', and 'EnumPoint' translates to the value at that point.
toBounded :: Bounded c => EnumInf c -> c
toBounded r = case r of
  EnumNegInf  -> minBound
  EnumPosInf  -> maxBound
  EnumPoint c -> c

-- | Like 'toBounded', but operates on a pair.
toBoundedPair :: (Enum c, Bounded c) => Segment c -> (c, c)
toBoundedPair r = case r of
  Single c  -> (toBounded c, toBounded c)
  Segment c d -> (toBounded c, toBounded d)

-- | Computes the minimum 'Segment' that can contain the list of all given 'EnumRanges'.
-- 'Data.Maybe.Nothing' indicates the empty set.
containingSet :: (Ord c, Enum c, BoundedInf c) => [Segment c] -> Maybe (Segment c)
containingSet ex = foldl fe Nothing ex where
  fe Nothing a  = Just a
  fe (Just a) c = Just $ case a of
    Single a   -> case c of
      Single c   -> enumInfSeg (min a c) (max a c)
      Segment  c d -> enumInfSeg (min a c) (max a d)
    Segment  a b -> case c of
      Single c   -> enumInfSeg (min a b) (max b c)
      Segment  c d -> enumInfSeg (min a c) (max b d)

-- | Evaluates to the number of elements covered by this region. Returns 'Prelude.Nothing' if there
-- are an infinite number of elements. For data of a type that is not an instance of 'Prelude.Num',
-- for example @'Segment' 'Data.Char.Char'@, it is recommended you first convert to the type
-- @'Segment' 'Data.Int.Int'@ using @'Control.Functor.fmap' 'Prelude.fromEnum'@ before using this
-- function, then convert the result back using @'Control.Functor.fmap' 'Prelude.toEnum'@ if
-- necessary.
numElems :: (Integral c, Enum c) => Segment c -> Maybe Integer
numElems seg = case seg of
  Single  (EnumPoint _)               -> Just 1
  Segment (EnumPoint a) (EnumPoint b) -> Just (fromIntegral a - fromIntegral b + 1)
  _                                   -> Nothing

-- | Tests whether an 'EnumInf' is within the enumInfSeg. It is handy when used with backquote noation:
-- @enumInf `isWithin` enumInfSeg@
isWithin :: (Ord c, Enum c) => EnumInf c -> Segment c -> Bool
isWithin point seg = case seg of
  Single x              -> point == x
  Segment EnumNegInf hi -> point <= hi
  Segment lo EnumPosInf -> lo <= point
  Segment lo hi         -> lo <= point && point <= hi

-- | Returns true if two 'Segment's are intersecting.
areIntersecting :: (Ord c, Enum c) => Segment c -> Segment c -> Bool
areIntersecting a b = case a of
  Single aa -> case b of
    Single bb   -> aa == bb
    Segment _ _ -> aa `isWithin` b
  Segment x y -> case b of
    Single bb -> bb `isWithin` a
    Segment x' y' -> x' `isWithin` a || y' `isWithin` a || x `isWithin` b || y `isWithin` b

-- | Returns true if two 'Segment's are consecutive, that is, if the end is the 'Prelude.pred'essor
-- of the start of the other.
areConsecutive :: (Ord c, Enum c, BoundedInf c) => Segment c -> Segment c -> Bool
areConsecutive a b = case a of
  Single a -> case b of
    Single b
      | a < b     -> consec a b
      | otherwise -> consec b a
    Segment x y
      | a < x     -> consec a x
      | otherwise -> consec y a
  Segment x y -> case b of
    Single a
      | a < x     -> consec a x
      | otherwise -> consec y a
    Segment x' y'
      | y < x'    -> consec y x'
      | otherwise -> consec y' x
  where { consec a b = stepUp a == b || a == stepDown b }

-- | Performs a set union on two 'Segment's of elements to create a new enumInfSeg. If the elements of
-- the new enumInfSeg are not contiguous, each enumInfSeg is returned separately and unchanged. The first
-- item in the pair of items returned is 'Prelude.True' if any of the items were modified.
segmentUnion :: (Ord c, Enum c, BoundedInf c) => Segment c -> Segment c -> (Bool, [Segment c])
segmentUnion a b
  | areIntersecting a b = case a of
      Single    _  -> case b of
        Single  _      -> (True, [a])
        Segment _ _    -> (True, [b])
      Segment x y  -> case b of
        Single  _      -> (True, [a])
        Segment x' y'  -> (True, [enumInfSeg (min x x') (max y y')])
  | areConsecutive a b = case a of
      Single  aa   -> case b of
        Single  bb     -> (True, [enumInfSeg aa bb])
        Segment x  y   -> (True, [enumInfSeg (min aa x) (max aa y)])
      Segment x y  -> case b of
        Single  bb     -> (True, [enumInfSeg (min bb x) (max bb y)])
        Segment x' y'  -> (True, [enumInfSeg (min x x') (max y y')])
  | otherwise = (False, [a, b])

-- | Performs a set intersection on two 'Segment's of elements to create a new enumInfSeg. If the
-- elements of the new enumInfSeg are not contiguous, this function evaluates to an empty list.
segmentIntersect :: (Ord c, Enum c, BoundedInf c) => Segment c -> Segment c -> (Bool, [Segment c])
segmentIntersect a b = if areIntersecting a b then joined else (False, []) where
  joined = case a of
    Single  _   -> (True, [a])
    Segment x y -> case b of
      Single  _     -> (True, [b])
      Segment x' y' -> (True, [enumInfSeg (max x x') (min y y')]) 

-- | Performs a set "delete" operation, deleteing any elements selected by the first enumInfSeg if
-- they are contained in the second enumInfSeg. This operation is not associative, i.e.
-- @'segmentDelete' a b /= 'segmentDelete' b a@.
segmentDelete :: (Ord c, Enum c, BoundedInf c) => Segment c -> Segment c -> (Bool, [Segment c])
segmentDelete a b = if not (areIntersecting a b) then (False, [a]) else del where
  del = case a of
    Single  x   -> case b of
      Single  x'    -> (True, [])
      Segment x' y' -> (True, [])
    Segment x y -> case b of
      Single  x'
        | x==x'     -> (True, [enumInfSeg (stepUp x) y])
        | y==x'     -> (True, [enumInfSeg x (stepDown y)])
        | otherwise -> (True, [enumInfSeg x (stepDown x'), enumInfSeg (stepUp x') y])
      Segment x' y'
        | x' >  x && y' <  y -> (True, [enumInfSeg x (stepDown x'), enumInfSeg (stepUp y') y])
        | x' <= x && y' >= y -> (True, [])
        | x' <= x && y' <  y -> (True, [enumInfSeg (stepUp y') y])
        | x' >  x && y' >= y -> (True, [enumInfSeg x (stepDown x')])

-- | Evaluates to the set of all elements not selected by the given 'Segment'.
segmentInvert :: (Ord c, Enum c, BoundedInf c) => Segment c -> [Segment c]
segmentInvert seg = canonicalSegment =<< case seg of
  Single  x   -> case x of
    EnumNegInf  -> [] -- [Single EnumPosInf]
    EnumPosInf  -> [] -- [Single EnumNegInf]
    EnumPoint _ -> [mkSegment EnumNegInf (stepDown x), mkSegment (stepUp x) EnumPosInf]
  Segment x y -> case x of
    EnumNegInf  -> case y of
      EnumNegInf  -> [] -- [Single  EnumPosInf]
      EnumPosInf  -> [] -- []
      EnumPoint _ -> [mkSegment (stepUp y) EnumPosInf]
    EnumPosInf  -> case y of
      EnumPosInf  -> [] -- [Single  EnumNegInf]
      EnumNegInf  -> [] -- []
      EnumPoint _ -> [mkSegment EnumNegInf (stepDown y)]
    EnumPoint _ -> case y of
      EnumNegInf  -> [mkSegment (stepUp x) EnumPosInf]
      EnumPosInf  -> [mkSegment EnumNegInf (stepDown x)]
      EnumPoint _ ->
        [ mkSegment EnumNegInf (min (stepDown x) (stepDown y))
        , mkSegment (max (stepUp x) (stepUp y)) EnumPosInf
        ]

-- | Eliminate overlapping and duplicate 'Segment's from a list of segments.
segmentNub :: (Ord c, Enum c, BoundedInf c) => [Segment c] -> [Segment c]
segmentNub ax = loop (sort ax) >>= canonicalSegment where
  loop ax = case ax of
    []     -> []
    [a]    -> [a]
    a:b:ax ->
      let (changed, bx) = segmentUnion a b
      in  if changed
            then loop (bx++ax)
            else if null bx then loop ax else head bx : loop (tail bx ++ ax)

----------------------------------------------------------------------------------------------------

-- | This function is used by 'associativeProduct' to generate the list of pairs on which to execute the
-- inner production function. It is a general function that may come in handy, but otherwise does
-- not specifically relate to 'EnumSet' or 'Segment' types.
--
-- What it does is, Given two lists of items, returns every possible unique combination of two
-- items. For example the pair (1,2) and (2,1) are considered to be the same combination, so only
-- (1,2) is selected. The selected items are returned as a list. The name of this function derives
-- from a similar matrix operation were all possible pairs are placed in a matrix and the
-- upper-triangluar elements are selected and returned. Pass 'Prelude.True' as the first parameter
-- to select items on the main diagonal of the matrix. Passing 'Prelude.False' is handy when you are
-- trying to evaluate a function on every possible 2-element combination of elements from a single
-- list, but don't need to evaluate each element with itself.
upperTriangular :: Bool -> [a] -> [b] -> [(a, b)]
upperTriangular mainDiag ax bx = do
  let iter bx = if null bx then [] else bx : iter (tail bx)
  (a, bx) <- zip ax (if mainDiag then iter bx else if null bx then [] else iter (tail bx))
  map (\b -> (a, b)) bx

-- | Used by the various set operations, including 'setUnion', 'setIntersect', and 'setDelete', to
-- compute a new set from two parameter sets and a single operation on the compnent 'Segment's. What
-- it does is, given two lists of elements, the largest possible upper triangular matrix (using
-- 'upperTriangular') of all possible pairs of elements from a and b is formed, and on each pair a
-- given inner product function is executed. The first parameter, the product function, is intended
-- to be a function like 'segmentUnion', 'segmentIntersect', or 'segmentDelete'. 
associativeProduct
  :: (Ord c, Enum c, BoundedInf c)
  => (Segment c -> Segment c -> (Bool, [Segment c]))
  -> EnumSet c -> EnumSet c -> EnumSet c
associativeProduct reduce a b =
  let (ax,bx) = (listSegments a, listSegments b)
      f ax bx = upperTriangular True ax bx >>= snd . uncurry reduce
      product = segmentNub (if enumSetElemCount b > enumSetElemCount a then f ax bx else f bx ax)
  in  EnumSet{ enumSetElemCount = length product, listSegments = product }

-- not for export
-- This equation assumes list arguments passed to it are already sorted list. This alrorithm works
-- and is O(log n).
exclusiveProduct :: (c -> c -> (Bool, [c])) -> [c] -> [c] -> [c]
exclusiveProduct product ax bx = ax >>= loop False bx where
  loop hitOne bx a = case bx of
    []   -> if hitOne then [] else [a]
    b:bx ->
      let (hit, ax) = product a b
      in  if hit
            then  ax >>= loop False bx
            else  if hitOne then [] else loop False bx a
   -- The logic is this: we are deleting or XOR-ing items bounded by segments in B from items
   -- bounded by segments in A. Both A and B are sorted. For every segment 'a' in A, the following
   -- evaluations take place: every element 'b' in B is checked against 'a' until we find a segment
   -- 'b[first]' that hits (intersects with) 'a'. The 'hitOne' boolean is set to True as soon as
   -- 'b[first]' is found.  Now we continue with every 'b' segment after 'b[first]' until we find a
   -- segment 'b[missed]' that does not hit (intersect with) 'a'. Since 'b[missed]' does not
   -- intersect, every element 'b' above 'b[missed]' will also miss (not intersect with) 'a',
   -- assuming 'b' elements are sorted. Therefore, we can stop scanning for further elements in B,
   -- we know they will all miss (not intersect). If every element in B misses (does not intersect
   -- with) 'a', then the segment 'a' is returned unmodified. However if even one segment in B hit
   -- this 'a', the only the segments produced by 'segmentDelete' are returned.

-- | Unlike inner product, which works with associative operators, 'segmentExclusive'
-- works with non-associative operators, like 'segmentDelete' and 'segmentXOR'. Lists of elements
-- passed to this function are sorted. Lists that are already sorted can be multiplied in
-- in O(log (n*m)) time. The product function you pass will return @(Prelude.True, result)@ if the
-- two arguments passed to it "react" with each other, that is, if they can be multiplied to a
-- non-null or non-zero result. This function is used to implement set deletion.
nonAssociativeProduct :: Ord c => (c -> c -> (Bool, [c])) -> [c] -> [c] -> [c]
nonAssociativeProduct product ax bx = exclusiveProduct product (sort ax) (sort bx)

----------------------------------------------------------------------------------------------------

-- | A set-union of serveral 'Segment's.
data EnumSet c
  = EnumSet
    { enumSetElemCount :: Int
    , listSegments :: [Segment c]
    }
  deriving Eq

instance (Ord c, Enum c, BoundedInf c) => Monoid (EnumSet c) where
  mempty  = emptySet
  mappend = setUnion

instance Show c => Show (EnumSet c) where
  show s = "("++intercalate ", " (map showSegment (listSegments s))++")"

-- | Initialize a new empty 'EnumSet'.
emptySet :: EnumSet c
emptySet = EnumSet{ enumSetElemCount = 0, listSegments = [] }

-- | Initialize a new intinite 'EnumSet', that is, the set that contains all possible elements.
infiniteSet :: EnumSet c
infiniteSet = EnumSet{ enumSetElemCount = 1, listSegments = [infinite] }

-- | Initialize a new 'EnumSet' object with a list of 'Segment's, which are 'segmentUnion'ed
-- together to create the set.
enumSet :: (Ord c, Enum c, BoundedInf c) => [Segment c] -> EnumSet c
enumSet ax = EnumSet{ enumSetElemCount = length list , listSegments = list } where
  list = segmentNub ax

-- | Create a set with a single range of elements, no gaps.
range :: (Ord c, Enum c, BoundedInf c) => c -> c -> EnumSet c
range a b = enumSet [segment a b]

-- | Create a set with a single element.
point :: (Ord c, Enum c, BoundedInf c) => c -> EnumSet c
point c = range c c

-- | Tests if an element is a member of the set.
setMember :: Ord c => EnumSet c -> c -> Bool
setMember set c = or $ map (segmentMember c) $ listSegments set

-- | Test if a set encompases only one element, and if so, returns that one element.
setIsSingleton :: EnumSet c -> Maybe c
setIsSingleton set = case listSegments set of
  [Single (EnumPoint a)] -> Just a
  _                      -> Nothing

-- | Tests if a set is empty.
setIsNull :: EnumSet c -> Bool
setIsNull s = null (listSegments s)

-- | Union the set of elements in two 'EnumSet's. This is the operation used for overriding
-- 'Data.Monoid.mappend'.
setUnion :: (Ord c, Enum c, BoundedInf c) => EnumSet c -> EnumSet c -> EnumSet c
setUnion a b = if setIsNull a then b else if setIsNull b then a else associativeProduct segmentUnion a b

-- | Intersect the set of elements in two 'EnumSet's, i.e. create a new set of elements that are
-- where every element must be included in both of the sets that were given as parameters. This
-- function is defined as @('associativeProduct' 'segmentIntersect')@.
setIntersect :: (Ord c, Enum c, BoundedInf c) => EnumSet c -> EnumSet c -> EnumSet c
setIntersect = associativeProduct segmentIntersect

-- | Delete every element from the first set if it exists in the second set. This operation is not
-- associative.
setDelete :: (Ord c, Enum c, BoundedInf c) => EnumSet c -> EnumSet c -> EnumSet c
setDelete a b = if null bx then a else delSet where
  bx     = listSegments b
  delSet = EnumSet{ enumSetElemCount = length del, listSegments = del }
  del    = segmentNub (exclusiveProduct segmentDelete (listSegments a) bx)

-- | Invert the items selected by a given set.
setInvert :: (Show c, Ord c, Enum c, BoundedInf c) => EnumSet c -> EnumSet c
setInvert set = EnumSet{ enumSetElemCount = length inv, listSegments = inv } where
  inv = canonicalSegment =<< case listSegments set of
    []                      -> [infinite]
    segs | segs==[infinite] -> []
    segs                    -> loop EnumNegInf segs where
      loop mark segs = case segs of
        []                          -> [mkSegment (stepUp mark) EnumPosInf]
        [Segment a EnumPosInf]      -> [mkSegment (stepUp mark) (stepDown a)]
        Segment EnumNegInf b : segs -> loop b segs
        Segment a          b : segs -> mkSegment (stepUp mark) (stepDown a) : loop b segs
        Single  a            : segs -> mkSegment (stepUp mark) (stepDown a) : loop a segs

-- | Exclusive-OR-like union of set elements.
setXUnion :: (Ord c, Enum c, BoundedInf c) => EnumSet c -> EnumSet c -> EnumSet c
setXUnion a b = (a `setUnion` b) `setDelete` (a `setIntersect` b)

