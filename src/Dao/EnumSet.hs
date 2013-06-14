-- "src/Dao/SetM.hs"  defines the Segment data type used to denote
-- a possibly infinite subset of contiguous elements of an Enum data type.
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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dao.EnumSet
  ( -- * The 'Inf' data type
    Inf(NegInf, PosInf, Point)
  , stepDown, stepUp, toPoint, enumIsInf
  , InfBound, minBoundInf, maxBoundInf
    -- * the 'Segment' data type
  , Segment, segment, single, negInfTo, toPosInf, enumInfSeg
  , toBounded, toBoundedPair, segmentMember, singular, plural, segmentNub
    -- * Predicates on 'Segment's
  , containingSet, numElems, isWithin, segmentHasEnumInf, segmentIsInfinite
    -- * The 'SetM' monadic data type
  , SetM, infiniteM, fromListM, rangeM, pointM
  , toListM, memberM, nullM, isSingletonM
    -- * Set Operators for monadic 'SetM's
  , unionWithM, intersectWithM, deleteWithM, invertM, setXUnionM
  , unionM, intersectM, deleteM
    -- * The 'Set' non-monadic data type
  , Set, infinite, fromList, range, point
  , toList, member, Dao.EnumSet.null, isSingleton
    -- * Set Operators for non-monadic 'Set's
  , Dao.EnumSet.invert, setXUnion, Dao.EnumSet.union, Dao.EnumSet.intersect, Dao.EnumSet.delete
    -- * Miscelaneous
  , upperTriangular, nonAssociativeProduct
  )
  where

import           Data.Monoid
import           Data.Bits
import           Data.List
import           Data.Ratio
import           Data.Binary

import           Control.Monad
import           Control.Applicative
import           Control.DeepSeq

-- | Like 'Prelude.Bounded', except the bounds might be infiniteM, and return 'NegInf' or
-- 'PosInf' for the bounds. Using the GHC "flexible instances" and "undecidable instances"
-- feature, any data type that is an instance of 'Prelude.Bounded' is also a memberM of 'BoundInf'.
class InfBound c where
  minBoundInf :: Inf c
  maxBoundInf :: Inf c

instance InfBound ()              where { minBoundInf = Point (); maxBoundInf = Point (); }
instance InfBound Int             where { minBoundInf = Point minBound; maxBoundInf = Point maxBound; }
instance InfBound Char            where { minBoundInf = Point minBound; maxBoundInf = Point maxBound; }
instance InfBound Integer         where { minBoundInf = NegInf; maxBoundInf = PosInf; }
instance InfBound (Ratio Integer) where { minBoundInf = NegInf; maxBoundInf = PosInf; }
instance InfBound Float           where { minBoundInf = NegInf; maxBoundInf = PosInf; }
instance InfBound Double          where { minBoundInf = NegInf; maxBoundInf = PosInf; }

-- | Enumerable elements with the possibility of infinity.
data Inf c
  = NegInf -- ^ negative infinity
  | PosInf -- ^ positive infinity
  | Point c -- ^ a single pointM
  deriving Eq

enumIsInf :: Inf c -> Bool
enumIsInf c = case c of
  NegInf -> True
  PosInf -> True
  _          -> False

instance Ord c => Ord (Inf c) where
  compare a b = f a b where
    f a b | a == b = EQ
    f NegInf _ = LT
    f _ NegInf = GT
    f PosInf _ = GT
    f _ PosInf = LT
    f (Point a) (Point b) = compare a b

instance Show c => Show (Inf c) where
  show e = case e of
    Point c -> show c
    NegInf  -> "-inf"
    PosInf  -> "+inf"

instance Functor Inf where
  fmap f e = case e of
    NegInf  -> NegInf
    PosInf  -> PosInf
    Point e -> Point (f e)

-- | Increment a given value, but if the value is 'Prelude.maxBound', return 'PosInf'. In some
-- circumstances this is better than incrementing with @'Data.Functor.fmap' 'Prelude.succ'@ because
-- 'Prelude.succ' evaluates to an error when passing 'Prelude.maxBound' as the argument. This
-- function will never evaluate to an error.
stepUp :: (Eq c, Enum c, InfBound c) => Inf c -> Inf c
stepUp x = if x==maxBoundInf then PosInf else fmap succ x

-- | Decrement a given value, but if the value is 'Prelude.minBound', returns 'NegInf'. In some
-- circumstances this is better than incrementing @'Data.Functor.fmap' 'Prelude.pred'@ because
-- 'Prelude.pred' evaluates to an error when passing 'Prelude.maxBound' as the argument. This
-- function will never evaluate to an error.
stepDown :: (Eq c, Enum c, InfBound c) => Inf c -> Inf c
stepDown x = if x==minBoundInf then NegInf else fmap pred x

-- | Retrieve the value contained in an 'Inf', if it exists.
toPoint :: Inf c -> Maybe c
toPoint c = case c of
  Point c -> Just c
  _           -> Nothing

-- | A enumInfSeg of 'Inf' items is a subset of consectutive items in the set of all @c@ where @c@
-- is any type satisfying the 'Prelude.Ix' class. To construct a 'Segment' object, use 'enumInfSeg'.
data Segment c
  = Single  { startPoint :: Inf c }
  | Segment { startPoint :: Inf c, endPoint :: Inf c }
  deriving Eq
  -- NOTE: the constructor for this data type is not exported because all of the functions in this
  -- module that operate on 'Segment's make the assumption that the first parameter *less than* the
  -- second parameter. To prevent anyone from screwing it up, the constructor is hidden and
  -- constructing a 'Segment' must be done with the 'enumInfSeg' function which checks the parameters.

-- not exported
mkSegment :: Eq c => Inf c -> Inf c -> Segment c
mkSegment a b
  | a==b      = Single  a
  | otherwise = Segment a b

-- | If the 'Segment' was constructed with 'single', return the pointM (possibly 'PosInf' or
-- 'NegInf') value used to construct it, otherwise return 'Data.Maybe.Nothing'.
singular :: Segment a -> Maybe (Inf a)
singular seg = case seg of
  Segment _ _ -> mzero
  Single  a   -> return a

-- | If the 'Segment' was constructed with 'segment', return a pair of points (possibly 'PosInf'
-- or 'NegInf') value used to construct it, otherwise return 'Data.Maybe.Nothing'.
plural :: Segment a -> Maybe (Inf a, Inf a)
plural a = case a of
  Segment a b -> return (a, b)
  Single  _   -> mzero

-- | Compare segments with a parameterized function for ordering the contained object (the object of
-- type @x@).
compareSegments :: Ord c => Segment c -> Segment c -> Ordering
compareSegments x y = case x of
  Single  a     -> case y of
    Single  b     -> compare a b
    Segment b  b' -> compare a b
  Segment a  b  -> case y of
    Single  a'    -> compare a b
    Segment a' b' -> if a==a' then compare b' b else compare a a'

showSegment :: Show c => Segment c -> String
showSegment seg = case seg of
  Single  a   -> "at "++show a
  Segment a b -> "from "++show a++" to "++show b

-- | This gets rid of as many infiniteM elements as possible. All @'Single' 'PosInf'@ and
-- @'Single' 'NegInf'@ points are eliminated, and if an 'NegInf' or 'PosInf' can be
-- replaced with a corresponding 'minBoundInf' or 'maxBoundInf', then it is. This function is
-- intended to be used as a list monadic function, so use it like so:
-- @let myListOfSegments = [...] in myListOfSegments >>= 'delInfPoints'@
canonicalSegment :: (Eq c, InfBound c) => Segment c -> [Segment c]
canonicalSegment seg = nonInf seg >>= \seg -> case seg of
  Single  a   -> [Single a]
  Segment a b -> nonInf (mkSegment (bounds a) (bounds b))
  where
    nonInf seg = case seg of
      Single  NegInf -> []
      Single  PosInf -> []
      Single  a          -> [Single  a  ]
      Segment a b        -> [Segment a b]
    bounds x = case x of
      NegInf -> minBoundInf
      PosInf -> maxBoundInf
      x          -> x

instance Show c =>
  Show (Segment c) where { show seg = "("++showSegment seg++")" }

-- | A predicate evaluating whether or not a segment includes an 'PosInf' or 'NegInf' value.
-- This should not be confused with a predicate evaluating whether the set of elements included by
-- the rangeM is infiniteM, because types that are instances of 'Prelude.Bounded' may also contain
-- 'PosInf' or 'NegInf' elements, values of these types may be evaluated as "infintie" by
-- this function, even though they are 'Prelude.Bounded'. To check if a segment is infiniteM, use
-- 'segmentIsInfinite' instead.
segmentHasEnumInf :: Segment c -> Bool
segmentHasEnumInf seg = case seg of
  Single  c   -> enumIsInf c
  Segment a b -> enumIsInf a || enumIsInf b

-- | A predicate evaluating whether or not a segment is infiniteM. Types that are 'Prelude.Bounded'
-- are always finite, and thus this function will always evaluate to 'Prelude.False' for these
-- types.
segmentIsInfinite :: InfBound c => Segment c -> Bool
segmentIsInfinite seg = case [Single minBoundInf, Single maxBoundInf, seg] of
  [Single a, Single b, c] | enumIsInf a || enumIsInf b -> case c of
    Single  c   -> enumIsInf c
    Segment a b -> enumIsInf a || enumIsInf b
  _ -> False

-- | Construct a 'Segment' from two 'Inf' items. /NOTE/ if the 'Inf' type you are
-- constructing is an instance of 'Prelude.Bounded', use the 'boundedSegment' constructor instead of
-- this function.
enumInfSeg :: (Ord c, Enum c, InfBound c) => Inf c -> Inf c -> Segment c
enumInfSeg a b = seg a b where
  seg a b = construct (ck minBoundInf NegInf a) (ck maxBoundInf PosInf b)
  ck inf subst ab = if inf==ab then subst else ab
  construct a b
    | a == b    = Single  a
    | a < b     = Segment a b
    | otherwise = Segment b a

-- | Construct a 'Segment' from two values.
segment :: Ord c => c -> c -> Segment c
segment a b = mkSegment (Point (min a b)) (Point (max a b))

-- | Construct a 'Segment' that is only a single unit, i.e. it starts at X and ends at X.
single :: Ord c => c -> Segment c
single a = segment a a

-- | Construct a 'Segment' from negative infinity to a given value.
negInfTo :: InfBound c => c -> Segment c
negInfTo a = Segment minBoundInf (Point a)

-- | Construct a 'Segment' from a given value to positive infinity.
toPosInf :: InfBound c => c -> Segment c
toPosInf a = Segment (Point a) maxBoundInf

-- | Construct the infiniteM 'Segment'
infiniteSegment :: Segment c
infiniteSegment = Segment NegInf PosInf

-- | Tests whether an element is a memberM is enclosed by the 'Segment'.
segmentMember :: Ord c => c -> Segment c -> Bool
segmentMember c seg = case seg of
  Single  (Point d) -> c == d
  Segment lo hi         -> let e = Point c in lo <= e && e <= hi
  _                     -> False

-- | Construct a 'Segment', like the 'enumInfSeg' constructor above, however does not require 'Inf'
-- parameters as inputs. This function performs the additional check of testing whether or not a
-- value is equivalent to 'Prelude.minBound' or 'Prelude.maxBound', and if it is, replaces that
-- value with 'NegInf' or 'PosInf' respectively. In other words, you can use
-- 'Prelude.minBound' in place of NegInf and 'Prelude.maxBound' in place of 'PosInf' without
-- changing the semantics of the data structure as it is used throughout the program.
-- boundedSegment :: (Ord c, Enum c, Bounded c) => c -> c -> Segment c
-- boundedSegment a b = if a>b then co b a else co a b where
--    co a b = enumInfSeg (f a minBound NegInf) (f b maxBound PosInf)
--    f x bound inf = if x==bound then inf else Point x

-- | If an 'Inf' is also 'Prelude.Bounded' then you can convert it to some value in the set of
-- 'Prelude.Bounded' items. 'NegInf' translates to 'Prelude.minBound', 'PosInf' translates
-- to 'Prelude.maxBound', and 'Point' translates to the value at that pointM.
toBounded :: Bounded c => Inf c -> c
toBounded r = case r of
  NegInf  -> minBound
  PosInf  -> maxBound
  Point c -> c

-- | Like 'toBounded', but operates on a segment and returns a pair of values.
toBoundedPair :: (Enum c, Bounded c) => Segment c -> (c, c)
toBoundedPair r = case r of
  Single  c   -> (toBounded c, toBounded c)
  Segment c d -> (toBounded c, toBounded d)

-- | Computes the minimum 'Segment' that can contain the list of all given 'EnumRanges'.
-- 'Data.Maybe.Nothing' indicates the empty set.
containingSet :: (Ord c, Enum c, InfBound c) => [Segment c] -> Maybe (Segment c)
containingSet ex = foldl fe Nothing ex where
  fe Nothing a  = Just a
  fe (Just a) c = Just $ case a of
    Single  a   -> case c of
      Single   c   -> enumInfSeg (min a c) (max a c)
      Segment  c d -> enumInfSeg (min a c) (max a d)
    Segment a b -> case c of
      Single   c   -> enumInfSeg (min a b) (max b c)
      Segment  c d -> enumInfSeg (min a c) (max b d)

-- | Evaluates to the number of elements covered by this region. Returns 'Prelude.Nothing' if there
-- are an infiniteM number of elements. For data of a type that is not an instance of 'Prelude.Num',
-- for example @'Segment' 'Data.Char.Char'@, it is recommended you first convert to the type
-- @'Segment' 'Data.Int.Int'@ using @'Control.Functor.fmap' 'Prelude.fromEnum'@ before using this
-- function, then convert the result back using @'Control.Functor.fmap' 'Prelude.toEnum'@ if
-- necessary.
numElems :: (Integral c, Enum c) => Segment c -> Maybe Integer
numElems seg = case seg of
  Single  (Point _)               -> Just 1
  Segment (Point a) (Point b) -> Just (fromIntegral a - fromIntegral b + 1)
  _                                   -> Nothing

-- | Tests whether an 'Inf' is within the enumInfSeg. It is handy when used with backquote noation:
-- @enumInf `isWithin` enumInfSeg@
isWithin :: (Ord c, Enum c) => Inf c -> Segment c -> Bool
isWithin pointM seg = case seg of
  Single x              -> pointM == x
  Segment NegInf hi -> pointM <= hi
  Segment lo PosInf -> lo <= pointM
  Segment lo hi         -> lo <= pointM && pointM <= hi

-- | Returns true if two 'Segment's are intersecting.
areIntersecting :: (Ord c, Enum c) => Segment c -> Segment c -> Bool
areIntersecting a b = case a of
  Single  aa  -> case b of
    Single  bb    -> aa == bb
    Segment _  _  -> aa `isWithin` b
  Segment x y -> case b of
    Single  bb    -> bb `isWithin` a
    Segment x' y' -> x' `isWithin` a || y' `isWithin` a || x `isWithin` b || y `isWithin` b

-- | Returns true if two 'Segment's are consecutive, that is, if the end is the 'Prelude.pred'essor
-- of the start of the other.
areConsecutive :: (Ord c, Enum c, InfBound c) => Segment c -> Segment c -> Bool
areConsecutive a b = case a of
  Single  a   -> case b of
    Single  b
      | a < b     -> consec a  b
      | otherwise -> consec b  a
    Segment x  y
      | a < x     -> consec a  x
      | otherwise -> consec y  a
  Segment x y -> case b of    
    Single  a
      | a < x     -> consec a  x
      | otherwise -> consec y  a
    Segment x' y'
      | y < x'    -> consec y  x'
      | otherwise -> consec y' x
  where { consec a b = stepUp a == b || a == stepDown b }

-- | Performs a set union on two 'Segment's of elements to create a new enumInfSeg. If the elements of
-- the new enumInfSeg are not contiguous, each enumInfSeg is returned separately and unchanged. The first
-- item in the pair of items returned is 'Prelude.True' if any of the items were modified.
segmentUnion :: (Ord c, Enum c, InfBound c) => Segment c -> Segment c -> (Bool, [Segment c])
segmentUnion a b
  | areIntersecting a b = case a of
      Single  _   -> case b of
        Single  _      -> (True, [a])
        Segment _  _   -> (True, [b])
      Segment x y -> case b of
        Single  _      -> (True, [a])
        Segment x' y'  -> (True, [enumInfSeg (min x x') (max y y')])
  | areConsecutive a b = case a of
      Single  aa  -> case b of
        Single  bb     -> (True, [enumInfSeg      aa         bb   ])
        Segment x  y   -> (True, [enumInfSeg (min aa x) (max aa y)])
      Segment x y -> case b of
        Single  bb     -> (True, [enumInfSeg (min bb x) (max bb y)])
        Segment x' y'  -> (True, [enumInfSeg (min x x') (max y y')])
  | otherwise = (False, [a, b])

-- | Performs a set intersection on two 'Segment's of elements to create a new enumInfSeg. If the
-- elements of the new enumInfSeg are not contiguous, this function evaluates to an empty list.
segmentIntersect :: (Ord c, Enum c, InfBound c) => Segment c -> Segment c -> (Bool, [Segment c])
segmentIntersect a b = if areIntersecting a b then joined else (False, []) where
  joined = case a of
    Single  aa    -> case b of
      Single  aa    -> (True, [Single aa])
      Segment _  _  -> (True, [Single aa])
    Segment x  y  -> case b of
      Single  aa    -> (True, [Single aa])
      Segment x' y' -> (True, [enumInfSeg (max x x') (min y y')]) 

-- | Performs a set "delete" operation, deleteing any elements selected by the first enumInfSeg if
-- they are contained in the second enumInfSeg. This operation is not associative, i.e.
-- @'segmentDelete' a b /= 'segmentDelete' b a@.
segmentDelete :: (Ord c, Enum c, InfBound c) =>
  Segment c -> Segment c -> (Bool, [Segment c])
segmentDelete a b = if not (areIntersecting a b) then (False, [a]) else del where
  del = case a of
    Single  x   -> case b of
      Single  x'    -> (True, [])
      Segment x' y' -> (True, [])
    Segment x y -> case b of
      Single  x'
        | x==x'     -> (True, [enumInfSeg (stepUp x)  y ])
        | y==x'     -> (True, [enumInfSeg x (stepDown y)])
        | otherwise -> (True, [enumInfSeg x (stepDown x'), enumInfSeg (stepUp x') y])
      Segment x' y'
        | x' >  x && y' <  y -> (True, [enumInfSeg x (stepDown x'), enumInfSeg (stepUp y') y])
        | x' <= x && y' >= y -> (True, [])
        | x' <= x && y' <  y -> (True, [enumInfSeg (stepUp y') y])
        | x' >  x && y' >= y -> (True, [enumInfSeg x (stepDown x')])

-- | Evaluates to the set of all elements not selected by the given 'Segment'.
segmentInvert :: (Ord c, Enum c, InfBound c) => Segment c -> [Segment c]
segmentInvert seg = canonicalSegment =<< case seg of
  Single  x   -> case x of
    NegInf  -> [] -- [Single PosInf]
    PosInf  -> [] -- [Single NegInf]
    Point _ -> [mkSegment NegInf (stepDown x), mkSegment (stepUp x) PosInf]
  Segment x y -> case x of
    NegInf  -> case y of
      NegInf  -> [] -- [Single  PosInf]
      PosInf  -> [] -- []
      Point _ -> [mkSegment (stepUp y) PosInf]
    PosInf  -> case y of
      PosInf  -> [] -- [Single  NegInf]
      NegInf  -> [] -- []
      Point _ -> [mkSegment NegInf (stepDown y)]
    Point _ -> case y of
      NegInf  -> [mkSegment (stepUp x) PosInf  ]
      PosInf  -> [mkSegment NegInf (stepDown x)]
      Point _ ->
        [ mkSegment NegInf (min (stepDown x) (stepDown y))
        , mkSegment (max (stepUp x) (stepUp y))  PosInf
        ]

-- | Eliminate overlapping and duplicate 'Segment's from a list of segments. Requires a union
-- function for combining elements that overlap.
segmentNub :: (Ord c, Enum c, InfBound c) => [Segment c] -> [Segment c]
segmentNub ax = loop (sortBy compareSegments ax) >>= canonicalSegment where
  loop ax = case ax of
    []     -> []
    [a]    -> [a]
    a:b:ax ->
      let (changed, bx) = segmentUnion a b
      in  if changed
            then loop (bx++ax)
            else if Data.List.null bx then loop ax else head bx : loop (tail bx ++ ax)

----------------------------------------------------------------------------------------------------

-- | This function is used by 'associativeProduct' to generate the list of pairs on which to execute the
-- inner production function. It is a general function that may come in handy, but otherwise does
-- not specifically relate to 'SetM' or 'Segment' types.
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
  let iter bx = if Data.List.null bx then [] else bx : iter (tail bx)
  (a, bx) <- zip ax (if mainDiag then iter bx else if Data.List.null bx then [] else iter (tail bx))
  map (\b -> (a, b)) bx

-- Used by the various set operations, including 'unionWithM', 'intersectWithM', and 'deleteWithM', to
-- compute a new set from two parameter sets and a single operation on the compnent 'Segment's. What
-- it does is, given two lists of elements, the largest possible upper triangular matrix (using
-- 'upperTriangular') of all possible pairs of elements from a and b is formed, and on each pair a
-- given inner product function is executed. The first parameter, the product function, is intended
-- to be a function like 'segmentUnion', 'segmentIntersect', or 'segmentDelete'.
associativeProduct
  :: (Ord c, Enum c, InfBound c)
  => (Segment c -> Segment c -> (Bool, [Segment c]))
  -> [Segment c] -> [Segment c] -> [Segment c]
associativeProduct reduce a b =
  let f a b = upperTriangular True a b >>= snd . uncurry reduce
  in  segmentNub (if length b > length a then f a b else f b a)

-- not for export
-- not for export
-- This equation assumes list arguments passed to it are already sorted list. This alrorithm works
-- in O(log (n^2)) time. Pass two functions, a function for combining intersecting items, and a
-- function for converting non-intersecting items in the list of @a@ to the list of @b@.
exclusiveProduct :: (a -> b -> (Bool, [a])) -> [a] -> [b] -> [a]
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
   -- with) 'a', then the segment 'a' is returned unmodified (because of the definition of XOR).
   -- However if even one segment in B hit this 'a', the only the segments produced by
   -- 'segmentDelete' are returned.

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
data SetM c x
  = EmptyEnumSet
  | InverseSet  { invertedSet :: SetM c x, enumSetValue :: x }
  | InfiniteSet { enumSetValue :: x }
  | SetM    { toListM :: [Segment c], enumSetValue :: x }
  deriving Eq
instance (Ord c, Enum c, InfBound c, Monoid x) =>
  Monoid (SetM c x) where
    mempty  = EmptyEnumSet
    mappend = unionWithM mappend
instance
  Functor (SetM c) where
    fmap f a = case a of
      EmptyEnumSet     -> EmptyEnumSet
      InverseSet   a x -> fmap f a
      InfiniteSet    x -> InfiniteSet (f x)
      SetM     a x -> SetM  a (f x)
instance (Ord c, Enum c, InfBound c) =>
  Monad (SetM c) where
    return  = InfiniteSet
    a >>= b = case a of
      EmptyEnumSet      -> EmptyEnumSet
      InverseSet   a ax -> forceInvert a ax >>= b
      InfiniteSet    ax -> b ax
      SetM     a ax -> intersectWithM (flip const) (SetM a ax) (b ax)
instance (Ord c, Enum c, InfBound c) =>
  MonadPlus (SetM c) where
    mzero = EmptyEnumSet
    mplus = unionWithM const
instance (Ord c, Enum c, InfBound c) =>
  Applicative (SetM c) where { pure = return; (<*>) = ap; }
instance (Ord c, Enum c, InfBound c) =>
  Alternative (SetM c) where { empty = mzero; (<|>) = mplus; }

--  instance (Show c, Show x) =>
--    Show (SetM c x) where
--      show s = "("++intercalate ", " (map (showSegment show) (toListM s))++")"

-- | Initialize a new intinite 'SetM', that is, the set that contains all possible elements.
infiniteM :: (Ord c, Enum c, InfBound c) => x -> SetM c x
infiniteM = InfiniteSet

-- | Initialize a new 'SetM' object with a list of 'Segment's, which are 'segmentUnion'ed
-- together to create the set.
fromListM :: (Ord c, Enum c, InfBound c) => [Segment c] -> x -> SetM c x
fromListM a ax = enumSet_ (segmentNub a) ax

-- Not exported. Assumes the segments list proivded was produced within this module and does not
-- need to be 'segmentNub'bed.
enumSet_ :: (Ord c, Enum c, InfBound c) => [Segment c] -> x -> SetM c x
enumSet_ a ax = case a of
  []                              -> EmptyEnumSet
  [Segment NegInf PosInf] -> InfiniteSet ax
  a                               -> SetM   a ax

-- | Create a set with a single rangeM of elements, no gaps.
rangeM :: (Ord c, Enum c, InfBound c) => c -> c -> x -> SetM c x
rangeM a b x = SetM [segment a b] x

-- | Create a set with a single element.
pointM :: (Ord c, Enum c, InfBound c) => c -> x -> SetM c x
pointM c = rangeM c c

-- | Tests if an element is a memberM of the set.
memberM :: Ord c => SetM c x -> c -> Bool
memberM a c = case a of
  EmptyEnumSet     -> False
  InfiniteSet    _ -> True
  InverseSet   a _ -> not (memberM a c)
  SetM     a _ -> or $ map (segmentMember c) a

-- | Test if a set encompases only one element, and if so, returns that one element.
isSingletonM :: (Ord c, Enum c, InfBound c) => SetM c x -> Maybe c
isSingletonM a = case void a of
  SetM     a _ -> case a of
    [Single (Point a)] -> Just a
    _                      -> Nothing
  InverseSet   a x -> isSingletonM (forceInvert a x)
  _                -> Nothing

-- | Tests if a set is empty.
nullM :: SetM c x -> Bool
nullM a = case a of
  EmptyEnumSet                   -> True
  InfiniteSet                  _ -> False
  SetM     _               _ -> False
  InverseSet   EmptyEnumSet    _ -> False
  InverseSet  (InfiniteSet  _) _ -> True
  InverseSet  (InverseSet a _) _ -> nullM a
  InverseSet   a               _ -> nullM a
    -- simply 'not'ting the result of (null s) will not work, the inverse of a set may or may
    -- not be null.

-- | Inverting an empty set will produce the 'infinity' set, but this set cannot be 'null' so you
-- must provide a value to be used in the case the given set is 'mempty'. If the given set is not
-- 'mempty' the value provided to this function is not used.
invertM :: (Ord c, Enum c, InfBound c) => SetM c x -> x -> SetM c x
invertM a y = case a of
  EmptyEnumSet     -> InfiniteSet y
  InfiniteSet  _   -> EmptyEnumSet
  InverseSet   a _ -> a
  SetM     a x -> InverseSet (SetM a x) y

-- | Union the set of elements in two 'SetM's. This is the operation used for overriding
-- 'Data.Monoid.mappend'.
unionWithM :: (Ord c, Enum c, InfBound c) =>
  (x -> x -> x) -> SetM c x -> SetM c x -> SetM c x
unionWithM add a b = case a of
  EmptyEnumSet       -> b
  InfiniteSet     ax -> case b of
    EmptyEnumSet       -> InfiniteSet ax
    InfiniteSet     bx -> InfiniteSet (add ax bx)
    SetM      b bx -> SetM  b (add ax bx)
    InverseSet    b bx -> unionWithM add (InfiniteSet ax) (forceInvert b bx)
  InverseSet    a ax -> unionWithM add (forceInvert a ax) b
  SetM      a ax -> case b of
    EmptyEnumSet       -> SetM a ax
    InfiniteSet     bx -> InfiniteSet (add ax bx)
    InverseSet    b bx -> unionWithM add (SetM a ax) (forceInvert b bx)
    SetM      b bx -> enumSet_ (associativeProduct segmentUnion a b) (add ax bx)

-- | Intersect the set of elements in two 'SetM's, i.e. create a new set of elements that are
-- where every element must be included in both of the sets that were given as parameters. This
-- function is defined as @('associativeProduct' 'segmentIntersect')@.
intersectWithM :: (Ord c, Enum c, InfBound c) =>
  (x -> y -> z) -> SetM c x -> SetM c y -> SetM c z
intersectWithM mult a b = case a of
  EmptyEnumSet      -> EmptyEnumSet
  InfiniteSet    ax -> case b of
    EmptyEnumSet      -> EmptyEnumSet
    InfiniteSet    bx -> InfiniteSet (mult ax bx)
    InverseSet   b bx -> intersectWithM mult (InfiniteSet ax) (forceInvert b bx)
    SetM     b bx -> enumSet_ b (mult ax bx)
  InverseSet   a ax -> intersectWithM mult (forceInvert a ax) b
  SetM     a ax -> case b of
    EmptyEnumSet      -> EmptyEnumSet
    InfiniteSet    bx -> InfiniteSet (mult ax bx)
    InverseSet   b bx -> intersectWithM mult (SetM a ax) (forceInvert b bx)
    SetM     b bx -> enumSet_ (associativeProduct segmentIntersect a b) (mult ax bx)

-- | Delete every element from the first set if it exists in the second set. This operation is not
-- associative.
deleteWithM :: (Ord c, Enum c, InfBound c) =>
  (x -> y -> x) -> SetM c x -> SetM c y -> SetM c x
deleteWithM sub a b = case a of
  EmptyEnumSet      -> EmptyEnumSet
  InfiniteSet    ax -> case b of
    EmptyEnumSet      -> InfiniteSet ax
    InfiniteSet    bx -> InfiniteSet (sub ax bx)
    InverseSet   b bx -> deleteWithM sub (InfiniteSet ax) (forceInvert b bx)
    SetM     _ bx -> InfiniteSet (sub ax bx)
  InverseSet   a ax -> deleteWithM sub (forceInvert a ax) b
  SetM     a ax -> case b of
    EmptyEnumSet      -> EmptyEnumSet
    InfiniteSet    _  -> EmptyEnumSet
    InverseSet   b bx -> deleteWithM sub (SetM a ax) (forceInvert b bx)
    SetM     b bx -> enumSet_ (exclusiveProduct segmentDelete a b) (sub ax bx)
      -- WARNING: used to apply 'segmentNub' to the whole of this function (which would now be done
      -- by constructing using 'fromListM' instead of 'enumSet_'). Wether or not we can do without it
      -- must be tested.

-- Not for export. Invert the items selected by a given set.
forceInvert :: (Ord c, Enum c, InfBound c) => SetM c x -> x -> SetM c x
forceInvert a ax = case a of
  EmptyEnumSet      -> InfiniteSet ax
  InfiniteSet    _  -> EmptyEnumSet
  InverseSet   a ax -> fmap (const ax) a -- use the 'ax' value from the previous call to 'invertM'
  SetM     a ax -> enumSet_ (inv a) ax
  where
    inv segs = canonicalSegment =<< loop NegInf segs
    loop mark segs = case segs of
      []                          -> [mkSegment (stepUp mark)  PosInf ]
      [Segment a PosInf]      -> [mkSegment (stepUp mark) (stepDown a)]
      Segment NegInf b : segs -> loop b segs
      Segment a          b : segs -> mkSegment (stepUp mark) (stepDown a) : loop b segs
      Single  a            : segs -> mkSegment (stepUp mark) (stepDown a) : loop a segs

-- | Exclusive-OR-like union of set elements.
setXUnionM :: (Ord c, Enum c, InfBound c) =>
  (x -> x -> x) -> SetM c x -> SetM c x -> SetM c x
setXUnionM add a b = case a of
  EmptyEnumSet      -> case b of
    EmptyEnumSet      -> EmptyEnumSet
    InfiniteSet    bx -> InfiniteSet    bx
    InverseSet   b bx -> InverseSet   b bx
    SetM     b bx -> SetM     b bx
  InfiniteSet    ax -> case b of
    EmptyEnumSet      -> InfiniteSet ax
    InfiniteSet    _  -> EmptyEnumSet
    InverseSet   b bx -> fmap (add ax) b
    SetM     b bx -> xorWithInf (SetM b bx) bx ax
  InverseSet   a ax -> setXUnionM add (forceInvert a ax) b
  SetM     a ax -> case b of
    EmptyEnumSet      -> SetM a ax
    InfiniteSet    bx -> xorWithInf (SetM a ax) ax bx
    InverseSet   b bx -> setXUnionM add (SetM a ax) (forceInvert b bx)
    SetM     b bx ->
      let va = SetM a ()
          vb = SetM b ()
      in  const (add ax bx) <$> deleteWithM const (unionWithM const va vb) (intersectWithM const va vb)
  where
    xorWithInf a ax bx = const (add ax bx) <$> forceInvert a ax

unionM :: (Ord c, Enum c, InfBound c) => SetM c x -> SetM c x -> SetM c x
unionM = unionWithM const

intersectM :: (Ord c, Enum c, InfBound c) => SetM c x -> SetM c y -> SetM c x
intersectM = intersectWithM const

deleteM :: (Ord c, Enum c, InfBound c) => SetM c x -> SetM c y -> SetM c x
deleteM = deleteWithM const

----------------------------------------------------------------------------------------------------

newtype Set c = Set (SetM c ()) deriving Eq
instance (Ord c, Enum c, InfBound c) =>
  Monoid (Set c) where { mempty = Set mempty; mappend (Set a) (Set b) = Set (mappend a b); }
instance Show c =>
  Show (Set c) where
    show (Set s) = "enumSet("++intercalate ", " (map show $ toListM s)++")"

infinite :: (Ord c, Enum c, InfBound c) => Set c
infinite = Set (infiniteM ())

fromList :: (Ord c, Enum c, InfBound c) => [Segment c] -> Set c
fromList = Set . flip fromListM ()

range :: (Ord c, Enum c, InfBound c) => c -> c -> Set c
range a b = Set (rangeM a b ())

point :: (Ord c, Enum c, InfBound c) => c -> Set c
point a = Set (pointM a ())

toList (Set a) = toListM a

member :: Ord c => Set c -> c -> Bool
member (Set a) b = memberM a b

null :: Set c -> Bool
null (Set a) = nullM a

isSingleton :: (Ord c, Enum c, InfBound c) => Set c -> Maybe c
isSingleton (Set a) = isSingletonM a

invert :: (Ord c, Enum c, InfBound c) => Set c -> Set c
invert (Set a) = Set (invertM a ())

setXUnion :: (Ord c, Enum c, InfBound c) => Set c -> Set c -> Set c
setXUnion (Set a) (Set b) = Set (setXUnionM const a b)

union :: (Ord c, Enum c, InfBound c) => Set c -> Set c -> Set c
union (Set a) (Set b) = Set (unionWithM const a b)

intersect :: (Ord c, Enum c, InfBound c) => Set c -> Set c -> Set c
intersect (Set a) (Set b) = Set (intersectWithM const a b)

delete :: (Ord c, Enum c, InfBound c) => Set c -> Set c -> Set c
delete (Set a) (Set b) = Set (deleteWithM const a b)

----------------------------------------------------------------------------------------------------

instance NFData a =>
  NFData (Inf a) where
    rnf  NegInf   = ()
    rnf  PosInf   = ()
    rnf (Point c) = deepseq c ()

instance NFData a =>
  NFData (Segment a) where
    rnf (Single  a  ) = deepseq a ()
    rnf (Segment a b) = deepseq a $! deepseq b ()

instance (NFData a, NFData x) =>
  NFData (SetM a x) where
    rnf a = case a of
      EmptyEnumSet    -> ()
      InfiniteSet  ax -> deepseq ax ()
      InverseSet a ax -> deepseq a $! deepseq ax ()
      SetM   a ax -> deepseq a $! deepseq ax ()

instance NFData a => NFData (Set a) where { rnf (Set a) = deepseq a () }

