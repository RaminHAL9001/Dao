-- "Dao/EnumSet.hs"  defines the Interval data type used to denote a possibly
-- whole subset of contiguous elements of an Enum data type.
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

module Dao.Interval
  ( -- * The 'Inf' data type
    Inf(NegInf, PosInf, Finite),
    stepDown, stepUp, toFinite, enumIsInf,
    InfBound, minBoundInf, maxBoundInf,
    -- * the 'Interval' data type
    Interval, startPoint, endPoint, toPair, interval, point, wholeInterval,
    negInfTo, toPosInf, toBounded, toBoundedPair, enumBoundedPair,
    intervalMember, singular, plural, canonicalInterval, intervalNub,
    intervalInvert, intervalUnion, intervalIntersect, intervalDelete, intervalExclusion,
    areIntersecting, areConsecutive,
    SubBounded(subBounds),
    -- * Predicates on 'Interval's
    intervalSpan, intervalSpanAll, numElems, intervalSize, isWithin,
    intervalHasEnumInf, intervalIsInfinite,
    -- * The 'Set' non-monadic data type
    Set, Dao.Interval.empty, whole, fromList, fromPairs, fromPoints, range,
    singleton, toList, size, elems, member, Dao.Interval.null, isWhole,
    isSingleton,
    -- * Set Operators for non-monadic 'Set's
    Dao.Interval.invert, exclusive,
    Dao.Interval.union, Dao.Interval.unions,
    Dao.Interval.intersections, Dao.Interval.intersect,
    Dao.Interval.delete,
    -- * Miscelaneous
    innerProduct
  )
  where

import           Data.Array.IArray (Array, Ix, bounds)
import           Data.Char
import           Data.Monoid
import           Data.List
import           Data.Ratio

import           Control.Arrow
import           Control.Monad
import           Control.Applicative
import           Control.DeepSeq

-- | Like 'Prelude.Bounded', except the bounds might be infinite, and return 'NegInf' or
-- 'PosInf' for the bounds. Using the GHC "flexible instances" and "undecidable instances"
-- feature, any data type that is an instance of 'Prelude.Bounded' is also a memberM of 'BoundInf'.
class InfBound c where
  minBoundInf :: Inf c
  maxBoundInf :: Inf c

instance InfBound ()              where { minBoundInf = Finite (); maxBoundInf = Finite (); }
instance InfBound Int             where { minBoundInf = Finite minBound; maxBoundInf = Finite maxBound; }
instance InfBound Char            where { minBoundInf = Finite minBound; maxBoundInf = Finite maxBound; }
instance InfBound Integer         where { minBoundInf = NegInf; maxBoundInf = PosInf; }
instance InfBound (Ratio Integer) where { minBoundInf = NegInf; maxBoundInf = PosInf; }
instance InfBound Float           where { minBoundInf = NegInf; maxBoundInf = PosInf; }
instance InfBound Double          where { minBoundInf = NegInf; maxBoundInf = PosInf; }

----------------------------------------------------------------------------------------------------

_paren :: Show o => o -> String
_paren o = let cx = show o in if or (fmap isSpace cx) then "("++cx++")" else cx

----------------------------------------------------------------------------------------------------

-- | Describes a class of data types that contained a range of values described by an upper and
-- lower bound, a subset of the range of values of the 'minBoundInf' and 'maxBoundInf'. Unlike
-- 'Prelude.Bounded' or 'InfBound', this class describes data types like 'Data.Array.IArray.Array's
-- which contain information about the upper and lower bound of the data type.
class Ord i => SubBounded dat i where { subBounds :: dat -> Interval i }

instance Ord i => SubBounded (Interval i) i where { subBounds = id; }
instance (Ord i, Ix i, InfBound i) => SubBounded (Array i o) i where
  subBounds = uncurry interval . Data.Array.IArray.bounds
instance (Ord i, Ix i, InfBound i) => SubBounded (i, i) i where { subBounds = uncurry interval }

----------------------------------------------------------------------------------------------------

-- | Enumerable elements with the possibility of infinity.
data Inf c
  = NegInf  -- ^ negative infinity
  | PosInf  -- ^ positive infinity
  | Finite c -- ^ a single point
  deriving (Eq, Show, Read)

instance Functor Inf where
  fmap f o = case o of
    NegInf   -> NegInf
    PosInf   -> PosInf
    Finite c -> Finite $ f c

instance NFData a =>
  NFData (Inf a) where
    rnf  NegInf    = ()
    rnf  PosInf    = ()
    rnf (Finite c) = deepseq c ()

instance Ord c => Ord (Inf c) where
  compare a b = case a of
    NegInf   -> case b of
      NegInf   -> EQ
      _        -> LT
    PosInf   -> case b of
      PosInf   -> EQ
      _        -> GT
    Finite a -> case b of
      NegInf   -> GT
      PosInf   -> LT
      Finite b -> compare a b

enumIsInf :: Inf c -> Bool
enumIsInf c = case c of
  NegInf -> True
  PosInf -> True
  _      -> False

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
toFinite :: Inf c -> Maybe c
toFinite c = case c of
  Finite c -> Just c
  _        -> Nothing

-- | This is a data type specifying an inclusive interval between exactly two points (which may be
-- the same point). This is the building block of an 'Interval' 'Set'.
data Interval c
  = Single   { startPoint :: Inf c }
  | Interval { startPoint :: Inf c, endPoint :: Inf c }
  deriving Eq

instance Functor Interval where
  fmap f o = case o of
    Single  loA     -> Single  (fmap f loA)
    Interval loA hiA -> Interval (fmap f loA) (fmap f hiA)

instance Ord c => Ord (Interval c) where
  compare a b = case a of
    Single    loA    -> case b of
      Single  loB    -> compare loA loB
      Interval loB _   -> if loA==loB then LT else compare loA loB
    Interval   loA hiA -> case b of
      Single  loB    -> if loA==hiA then GT else compare loA loB
      Interval loB hiB -> if loA==loB then compare hiB hiA else compare loA loB

instance Show c => Show (Interval c) where
  show o = case o of
    Single (Finite o) -> "point "++_paren o
    Single NegInf     -> error "(Single NegInf)"
    Single PosInf     -> error "(Single PosInf)"
    Interval NegInf (Finite o) -> "negInfTo "++_paren o
    Interval (Finite o) PosInf -> "toPosInf "++_paren o
    Interval NegInf     PosInf -> "wholeInterval"
    Interval (Finite a) (Finite b) -> unwords ["interval", _paren a, _paren b]
    Interval         a          b  -> error $ unwords ["interval", _paren a, _paren b]

instance (Eq c, Ord c, InfBound c, Read c) => Read (Interval c) where
  readsPrec p str = do
    let sp = dropWhile isSpace
    let next str = readsPrec p (sp str)
    case span isAlpha (sp str) of
      ("point"        , str) -> next str >>= \ (o, str) -> return (point    o, str)
      ("negInfTo"     , str) -> next str >>= \ (o, str) -> return (negInfTo o, str)
      ("toPosInf"     , str) -> next str >>= \ (o, str) -> return (toPosInf o, str)
      ("wholeInterval", str) -> return (wholeInterval, str)
      ("interval"     , str) -> do
        (a, str) <- next str
        (b, str) <- next str
        return (interval a b, str)
      _                      -> []

instance NFData a =>
  NFData (Interval a) where
    rnf (Single   a  ) = deepseq a ()
    rnf (Interval a b) = deepseq a $! deepseq b ()

_mkSeg :: (Eq c, Ord c, InfBound c) => Inf c -> Inf c -> Interval c
_mkSeg a b
  | a==b                             = Single   a
  | a==minBoundInf && b==maxBoundInf = wholeInterval
  | otherwise                        = Interval a b

toPair :: Interval c -> (Inf c, Inf c)
toPair seg = case seg of
  Single   a   -> (a, a)
  Interval a b -> (a, b)

-- | If the 'Interval' was constructed with 'single', return the pointM (possibly 'PosInf' or
-- 'NegInf') value used to construct it, otherwise return 'Data.Maybe.Nothing'.
singular :: Interval a -> Maybe (Inf a)
singular seg = case seg of
  Interval _ _ -> mzero
  Single   a   -> return a

-- | If the 'Interval' was constructed with 'interval', return a pair of points (possibly 'PosInf'
-- or 'NegInf') value used to construct it, otherwise return 'Data.Maybe.Nothing'.
plural :: Interval a -> Maybe (Inf a, Inf a)
plural seg = case seg of
  Interval loA hiA -> return (loA, hiA)
  Single   _       -> mzero

-- | This gets rid of as many infiniteM elements as possible. All @'Single' 'PosInf'@ and
-- @'Single' 'NegInf'@ points are eliminated, and if an 'NegInf' or 'PosInf' can be
-- replaced with a corresponding 'minBoundInf' or 'maxBoundInf', then it is. This function is
-- intended to be used as a list monadic function, so use it like so:
-- @let myListOfSegments = [...] in myListOfSegments >>= 'delInfPoints'@
canonicalInterval :: (Eq c, Ord c, InfBound c) => Interval c -> [Interval c]
canonicalInterval seg = nonInf seg >>= \seg -> case seg of
  Single   a   -> [Single a]
  Interval a b -> nonInf (_mkSeg (bounds a) (bounds b))
  where
    nonInf seg = case seg of
      Single  NegInf -> []
      Single  PosInf -> []
      Single  a          -> [Single  a  ]
      Interval a b        -> [Interval a b]
    bounds x = case x of
      NegInf -> minBoundInf
      PosInf -> maxBoundInf
      x          -> x

_canonicalize :: (Eq c, Ord c, InfBound c) => Interval c -> [Interval c]
_canonicalize seg = nonInf seg >>= \seg -> case seg of
  Single   loA     -> [Single loA]
  Interval loA hiA -> nonInf (_mkSeg (bounds loA) (bounds hiA))
  where
    nonInf seg = case seg of
      Single   NegInf  -> []
      Single   PosInf  -> []
      Single   loA     -> [Single  loA    ]
      Interval loA hiA -> [Interval loA hiA]
    bounds loA = case loA of
      NegInf -> minBoundInf
      PosInf -> maxBoundInf
      loA    -> loA

-- | A predicate evaluating whether or not a interval includes an 'PosInf' or 'NegInf' value.
-- This should not be confused with a predicate evaluating whether the set of elements included by
-- the rangeM is infiniteM, because types that are instances of 'Prelude.Bounded' may also contain
-- 'PosInf' or 'NegInf' elements, values of these types may be evaluated as "infintie" by
-- this function, even though they are 'Prelude.Bounded'. To check if a interval is infiniteM, use
-- 'intervalIsInfinite' instead.
intervalHasEnumInf :: Interval c -> Bool
intervalHasEnumInf seg = case seg of
  Single   loA     -> enumIsInf loA
  Interval loA hiA -> enumIsInf loA || enumIsInf hiA

-- | A predicate evaluating whether or not a interval is infiniteM. Types that are 'Prelude.Bounded'
-- are always finite, and thus this function will always evaluate to 'Prelude.False' for these
-- types.
intervalIsInfinite :: InfBound c => Interval c -> Bool
intervalIsInfinite seg = case [Single minBoundInf, Single maxBoundInf, seg] of
  [Single a, Single b, c] | enumIsInf a || enumIsInf b -> case c of
    Single   c   -> enumIsInf c
    Interval a b -> enumIsInf a || enumIsInf b
  _ -> False

-- | Construct a 'Interval' from two 'Inf' items. *NOTE* if the 'Inf' type you are constructing is
-- an instance of 'Prelude.Bounded', use the 'boundedInterval' constructor instead of this function.
_interval :: (Eq c, Ord c, InfBound c) => Inf c -> Inf c -> Interval c
_interval = seg where
  seg lo hi          = construct (ck minBoundInf NegInf lo) (ck maxBoundInf PosInf hi)
  ck infnt subst seg = if infnt==seg then subst else seg
  construct lo hi
    | lo == hi  = Single   lo
    | lo <  hi  = Interval lo hi
    | otherwise = Interval hi lo

-- | Construct a 'Interval' from two values.
interval :: (Ord c, InfBound c) => c -> c -> Interval c
interval lo hi = _mkSeg (Finite $ min lo hi) (Finite $ max lo hi)

-- | Construct a 'Interval' that is only a single unit, i.e. it starts at X and ends at X.
point :: Ord c => c -> Interval c
point = Single . Finite

-- | Construct a 'Interval' from negative infinity to a given value.
negInfTo :: InfBound c => c -> Interval c
negInfTo loA = Interval minBoundInf (Finite loA)

-- | Construct a 'Interval' from a given value to positive infinity.
toPosInf :: InfBound c => c -> Interval c
toPosInf a = Interval (Finite a) maxBoundInf

-- | Construct the infiniteM 'Interval'
wholeInterval :: Interval c
wholeInterval = Interval NegInf PosInf

-- | Tests whether an element is a memberM is enclosed by the 'Interval'.
intervalMember :: (Eq c, Ord c) => Interval c -> c -> Bool
intervalMember a b = case a of
  Single (Finite a) -> a==b
  Interval loA hiA  -> let loB = Finite b in loA <= loB && loB <= hiA
  _                 -> False

-- | If an 'Inf' is also 'Prelude.Bounded' then you can convert it to some value in the set of
-- 'Prelude.Bounded' items. 'NegInf' translates to 'Prelude.minBound', 'PosInf' translates
-- to 'Prelude.maxBound', and 'Finite' translates to the value at that pointM.
toBounded :: Bounded c => Inf c -> c
toBounded r = case r of
  NegInf   -> minBound
  PosInf   -> maxBound
  Finite c -> c

-- | Like 'toBounded', but operates on a interval and returns a pair of values.
toBoundedPair :: Bounded c => Interval c -> (c, c)
toBoundedPair seg = case seg of
  Single   lo    -> (toBounded lo, toBounded lo)
  Interval lo hi -> (toBounded lo, toBounded hi)

enumBoundedPair :: (Enum c, Bounded c) => Interval c -> [c]
enumBoundedPair seg = let (lo, hi) = toBoundedPair seg in [lo..hi]

-- | Construct the minimum 'Interval' that is big enough to hold both given segments.
intervalSpan :: (Ord c, InfBound c) => Interval c -> Interval c -> Interval c
intervalSpan a b = case a of
  Single   loA     -> case b of
    Single   loB     -> _interval (min loA loB) (max loA loB)
    Interval loB hiB -> _interval (min loA loB) (max loA hiB)
  Interval loA hiA -> case b of
    Single   loB     -> _interval (min loA loB) (max hiA loB)
    Interval loB hiB -> _interval (min loA loB) (max hiA hiB)

-- | Computes the minimum 'Interval' that can contain the list of all given 'EnumRanges'.
-- 'Data.Maybe.Nothing' indicates the empty set.
intervalSpanAll :: (Ord c, InfBound c) => [Interval c] -> Maybe (Interval c)
intervalSpanAll ex = if Prelude.null ex then Nothing else Just $ foldl1 intervalSpan ex

-- | Evaluates to the number of elements covered by this region. Returns 'Prelude.Nothing' if there
-- are an infiniteM number of elements. For data of a type that is not an instance of 'Prelude.Num',
-- for example @'Interval' 'Data.Char.Char'@, it is recommended you first convert to the type
-- @'Interval' 'Data.Int.Int'@ using @'Control.Functor.fmap' 'Prelude.fromEnum'@ before using this
-- function, then convert the result back using @'Control.Functor.fmap' 'Prelude.toEnum'@ if
-- necessary.
numElems :: Integral c => Interval c -> Inf Integer
numElems seg = case seg of
  Single   (Finite _)            -> Finite 1
  Interval (Finite a) (Finite b) -> Finite (fromIntegral a - fromIntegral b + 1)
  _                              -> PosInf

-- | Return the number of points included the set for sets of points that are both 'Prelude.Bounded'
-- and 'Prelude.Integral'.
intervalSize :: (Bounded c, Integral c) => Interval c -> Integer
intervalSize = pred . uncurry subtract . (toInteger *** toInteger) . toBoundedPair

-- | Tests whether an 'Inf' is within the _interval. It is handy when used with backquote noation:
-- @enumInf `isWithin` _interval@
isWithin :: (Eq c, Ord c) => Inf c -> Interval c -> Bool
isWithin point seg = case seg of
  Single   lo        -> point == lo
  Interval NegInf hi -> point <= hi
  Interval lo PosInf -> lo <= point
  Interval lo hi     -> lo <= point && point <= hi

-- | Returns true if two 'Interval's are intersecting.
areIntersecting :: (Eq c, Ord c) => Interval c -> Interval c -> Bool
areIntersecting a b = case a of
  Single   loA   -> case b of
    Single   loB     -> loA == loB
    Interval _  _    -> loA `isWithin` b
  Interval lo hi -> case b of
    Single   loB     -> loB `isWithin` a
    Interval loB hiB -> loB `isWithin` a || hiB `isWithin` a || lo `isWithin` b || hi `isWithin` b

-- | Returns true if two 'Interval's are consecutive, that is, if the end is the
-- 'Prelude.pred'ecessor of the start of the other.
areConsecutive :: (Ord c, Enum c, InfBound c) => Interval c -> Interval c -> Bool
areConsecutive loA loB = case loA of
  Single  loA   -> case loB of
    Single  loB
      | loA < loB -> consec loA  loB
      | otherwise -> consec loB  loA
    Interval lo  hi
      | loA < lo  -> consec loA  lo
      | otherwise -> consec hi  loA
  Interval lo hi -> case loB of    
    Single  loA
      | loA < lo  -> consec loA  lo
      | otherwise -> consec hi  loA
    Interval loB hiB
      | hi < loB  -> consec hi  loB
      | otherwise -> consec hiB lo
  where { consec loA loB = stepUp loA == loB || loA == stepDown loB }

-- | Performs a set union on two 'Interval's of elements to create a new _interval. If the elements of
-- the new _interval are not contiguous, each _interval is returned separately and unchanged. The first
-- item in the pair of items returned is 'Prelude.True' if any of the items were modified.
intervalUnion :: (Ord c, Enum c, InfBound c) => Interval c -> Interval c -> Set c
intervalUnion a b
  | areIntersecting a b = case a of
      Single  _     -> _fromList $ return $ case b of
        Single   _       -> a
        Interval _  _    -> b
      Interval lo hi -> _fromList $ return $ case b of
        Single   _       -> a
        Interval loB hiB -> _interval (min lo loB) (max hi hiB)
  | areConsecutive a b = _fromList $ return $ case a of
      Single  loA   -> case b of
        Single   loB     -> _interval      loA         loB   
        Interval lo  hi  -> _interval (min loA lo) (max loA hi)
      Interval lo hi -> case b of
        Single   loB     -> _interval (min loB lo) (max loB hi)
        Interval loB hiB -> _interval (min lo loB) (max hi hiB)
  | otherwise = _fromList $ if a<=b then [a, b] else [b, a]

-- | Performs a set intersection on two 'Interval's of elements to create a new _interval. If the
-- elements of the new _interval are not contiguous, this function evaluates to an empty list.
intervalIntersect :: (Ord c, Enum c, InfBound c) => Interval c -> Interval c -> Set c
intervalIntersect a b = if not (areIntersecting a b) then EmptySet else Set $ return $ case a of
  Single   loA  -> case b of
    Single   loA -> Single loA
    Interval _  _ -> Single loA
  Interval loA hiA -> case b of
    Single   loA    -> Single loA
    Interval loB hiB -> _interval (max loA loB) (min hiA hiB)

-- | Performs a set "delete" operation, deleteing any elements selected by the first _interval if
-- they are contained in the second _interval. This operation is not associative, i.e.
-- @'intervalDelete' a b /= 'intervalDelete' b a@.
intervalDelete :: (Ord c, Enum c, InfBound c) => Interval c -> Interval c -> Set c
intervalDelete a b = if not (areIntersecting a b) then Set [a] else Set $ case a of
  Single  _   -> case b of
    Single  _     -> []
    Interval _  _  -> []
  Interval loA hiA -> case b of
    Single   loB
      | loA==loB  -> [_interval (stepUp loA)  hiA ]
      | hiA==loB  -> [_interval loA (stepDown hiA)]
      | otherwise -> [_interval loA (stepDown loB), _interval (stepUp loB) hiA]
    Interval loB hiB
      | loB >  loA && hiB <  hiA -> [_interval loA (stepDown loB), _interval (stepUp hiB) hiA]
      | loB <= loA && hiB >= hiA -> []
      | loB <= loA && hiB <  hiA -> [_interval (stepUp hiB) hiA]
      | loB >  loA && hiB >= hiA -> [_interval loA (stepDown loB)]
      | otherwise                -> error "intervalDelete"

-- | Analogous to a bitwise exclusive-OR operation, returns the set of 'Interval's produced from
-- combining two 'Interval's such that only the portions of the 'Interval's that do not interlap are
-- included.
intervalExclusion :: (Ord c, Enum c, InfBound c) => Interval c -> Interval c -> Set c
intervalExclusion a b = Dao.Interval.union (intervalDelete a b) (intervalDelete b a)

-- | Evaluates to the set of all elements not selected by the given 'Interval'.
intervalInvert :: (Ord c, Enum c, InfBound c) => Interval c -> Set c
intervalInvert seg = (\o -> if Prelude.null o then EmptySet else Set o) $
  _canonicalize =<< case seg of
    Single   loA   -> case loA of
      NegInf   -> [] -- [Single PosInf]
      PosInf   -> [] -- [Single NegInf]
      Finite _ -> [_mkSeg NegInf (stepDown loA), _mkSeg (stepUp loA) PosInf]
    Interval loA hiA -> case loA of
      NegInf  -> case hiA of
        NegInf   -> [] -- [Single  PosInf]
        PosInf   -> [] -- []
        Finite _ -> [_mkSeg (stepUp hiA) PosInf]
      PosInf  -> case hiA of
        PosInf   -> [] -- [Single  NegInf]
        NegInf   -> [] -- []
        Finite _ -> [_mkSeg NegInf (stepDown hiA)]
      Finite _ -> case hiA of
        NegInf   -> [_mkSeg (stepUp loA) PosInf  ]
        PosInf   -> [_mkSeg NegInf (stepDown loA)]
        Finite _ ->
          [ _mkSeg NegInf (min (stepDown loA) (stepDown hiA))
          , _mkSeg (max (stepUp loA) (stepUp hiA))  PosInf
          ]

-- | Eliminate overlapping and duplicate 'Interval's from a list of segments.
intervalNub :: (Ord c, Enum c, InfBound c) => [Interval c] -> [Interval c]
intervalNub ax = loop (sort ax) >>= canonicalInterval where
  loop ax = case ax of
    []     -> []
    [a]    -> [a]
    a:b:ax -> case toList (intervalUnion a b) of
      [a, b] -> a : loop (b:ax)
      ab     -> loop (ab++ax)

----------------------------------------------------------------------------------------------------

-- | This function takes a multiplication function, usually 'intervalIntersect' or 'intervalDelete'.
-- It works like polynomial multiplication, with the provided reduction function computing the
-- product of every pair of 'Interval's, and then the "sum" (actually the 'intervalUnion') of all
-- products are taken. The 'intersect' and 'exclusive' functions are defined to use this function.
innerProduct
  :: (Ord c, Enum c, InfBound c)
  => (Interval c -> Interval c -> Set c)
  -> [Interval c] -> [Interval c] -> Set c
innerProduct reduce a b = fromList $ reduce <$> a <*> b >>= toList

-- This equation assumes list arguments passed to it are already sorted list. This alrorithm works
-- in O(log (n^2)) time. Pass two functions, a function for combining intersecting items, and a
-- function for converting non-intersecting items in the list of @a@ to the list of @b@.
_exclusive
  :: (Ord a, InfBound a, Enum a)
  => (Interval a -> Interval a -> Set a) -> [Interval a] -> [Interval a] -> [Interval a]
_exclusive delete ax bx = ax >>= loop False bx where
  loop hitOne bx a = case bx of
    []   -> if hitOne then [] else [a]
    b:bx -> let result = toList (delete a b) in
      if areIntersecting a b
      then result >>= loop False bx
      else if hitOne then [] else loop False bx a
   -- The logic is this: we are deleting or XOR-ing items bounded by segments in B from items
   -- bounded by segments in A. Both A and B are sorted. For every interval 'a' in A, the following
   -- evaluations take place: every element 'b' in B is checked against 'a' until we find a interval
   -- 'b[first]' that hits (intersects with) 'a'. The 'hitOne' boolean is set to True as soon as
   -- 'b[first]' is found.  Now we continue with every 'b' interval after 'b[first]' until we find a
   -- interval 'b[missed]' that does not hit (intersect with) 'a'. Since 'b[missed]' does not
   -- intersect, every element 'b' above 'b[missed]' will also miss (not intersect with) 'a',
   -- assuming 'b' elements are sorted. Therefore, we can stop scanning for further elements in B,
   -- we know they will all miss (not intersect). If every element in B misses (does not intersect
   -- with) 'a', then the interval 'a' is returned unmodified (because of the definition of XOR).
   -- However if even one interval in B hit this 'a', the only the segments produced by
   -- 'intervalDelete' are returned.

----------------------------------------------------------------------------------------------------

data Set c
  = EmptySet    
  | InfiniteSet
  | InverseSet (Set c)
  | Set        [Interval c]

instance Functor Set where
  fmap f o = case o of
    EmptySet     -> EmptySet
    InfiniteSet  -> InfiniteSet
    InverseSet o -> InverseSet (fmap f o)
    Set        o -> Set (fmap (fmap f) o)

instance (Eq c, Ord c, Enum c, InfBound c) => Eq (Set c) where
  a == b = case a of
    EmptySet     -> case b of
      EmptySet      -> True
      Set        [] -> True
      _             -> False
    InfiniteSet  -> case b of
      InfiniteSet      -> True
      Set [s] | s==wholeInterval -> True
      _                -> False
    InverseSet a -> case b of
      InverseSet b -> a==b
      _            -> _invert a == b
    Set        a -> case b of
      Set        b -> a==b
      _            -> False

instance (Ord c, Enum c, InfBound c, Bounded c, Integral c) => Ord (Set c) where
  compare a b = compare (size a) (size b)

instance (Ord c, Enum c, InfBound c) => Monoid (Sum (Set c)) where
  mempty  = Sum EmptySet
  mappend (Sum a) (Sum b) = Sum $ Dao.Interval.union a b

instance (Ord c, Enum c, InfBound c) => Monoid (Product (Set c)) where
  mempty  = Product EmptySet
  mappend (Product a) (Product b) = Product $ Dao.Interval.union a b

instance (Eq c, Ord c, Enum c, Show c, InfBound c) => Show (Set c) where
  show s = case s of
    EmptySet     -> "empty"
    InfiniteSet  -> "whole"
    InverseSet s -> "invert ("++_paren s++")"
    Set        s -> "fromList "++show s

instance (Eq c, Ord c, Enum c, Read c, InfBound c) => Read (Set c) where
  readsPrec p str = do
    let sp       = dropWhile isSpace
    let next :: Read o => ReadS o
        next str = readsPrec p (sp str)
    case span isAlpha (sp str) of
      ("empty"   , str) -> [(EmptySet   , str)]
      ("whole"   , str) -> [(InfiniteSet, str)]
      ("invert"  , str) -> case sp str of
        '(':str -> next str >>= \ (s, str) -> case sp str of
            ')':str -> return (InverseSet s, str)
            _       -> error "expecting close-paren for parameter to 'invert'"
        str     -> next str >>= \ (s, str) -> return (InverseSet s, str)
      ("fromList", str) -> next str >>= \ (s, str) -> return (fromList s, str)
      _                 -> []

instance NFData a => NFData (Set a) where
  rnf EmptySet       = ()
  rnf InfiniteSet    = ()
  rnf (Set a)        = deepseq a ()
  rnf (InverseSet a) = deepseq a ()

empty :: Set c
empty = EmptySet

whole :: Set c
whole = InfiniteSet

-- Creates a list from segments, but does not clean it with 'intervalNub'
_fromList :: (Ord c, Enum c, InfBound c) => [Interval c] -> Set c
_fromList a
  | Data.List.null a   = EmptySet
  | a==[wholeInterval] = InfiniteSet
  | otherwise          = Set a

fromList :: (Ord c, Enum c, InfBound c) => [Interval c] -> Set c
fromList a = if Data.List.null a then EmptySet else _fromList (intervalNub a)

fromPairs :: (Ord c, Enum c, InfBound c) => [(c, c)] -> Set c
fromPairs = fromList . fmap (uncurry interval)

fromPoints :: (Ord c, Enum c, InfBound c) => [c] -> Set c
fromPoints = fromList . fmap point

range :: (Ord c, Enum c, InfBound c) => c -> c -> Set c
range a b = Set [interval a b]

singleton :: (Ord c, Enum c, InfBound c) => c -> Set c
singleton a = Set [point a]

toList :: (Ord c, Enum c, InfBound c) => Set c -> [Interval c]
toList s = case s of
  EmptySet     -> []
  InfiniteSet  -> [wholeInterval]
  InverseSet s -> toList (_invert s)
  Set        s -> s

elems :: (Ord c, Enum c, Bounded c, InfBound c) => Set c -> [c]
elems = concatMap enumBoundedPair . toList

size :: (Ord c, Enum c, InfBound c, Bounded c, Integral c) => Set c -> Integer
size = sum . fmap intervalSize . toList

member :: (Ord c, InfBound c) => Set c -> c -> Bool
member s b = case s of
  EmptySet     -> False
  InfiniteSet  -> True
  InverseSet s -> not (member s b)
  Set       [] -> False
  Set        s -> any (`intervalMember` b) s

-- | 'Prelude.True' if the 'Set' is null.
null :: (Ord c, Enum c, InfBound c) => Set c -> Bool
null s = case s of
  EmptySet     -> True
  InfiniteSet  -> False
  InverseSet s -> isWhole s
  Set       [] -> True
  Set        _ -> False

-- | 'Prelude.True' if the 'Set' spans the 'whole' interval.
isWhole :: (Ord c, Enum c, InfBound c) => Set c -> Bool
isWhole s = case toList s of
  [Interval NegInf PosInf] -> True
  _                        -> False

isSingleton :: (Ord c, Enum c, InfBound c) => Set c -> Maybe c
isSingleton s = case s of
  InverseSet   s -> isSingleton (_invert s)
  Set [Single c] -> toFinite c
  _              -> mzero

invert :: (Ord c, Enum c, InfBound c) => Set c -> Set c
invert s = case s of
  EmptySet     -> InfiniteSet
  InfiniteSet  -> EmptySet    
  InverseSet s -> s
  Set        s -> InverseSet (Set s)

-- Compute the set inversion, rather than just marking it as inverted. Only used internally.
_invert :: (Ord c, Enum c, InfBound c) => Set c -> Set c
_invert s = case s of
  EmptySet     -> InfiniteSet
  InfiniteSet  -> EmptySet    
  InverseSet s -> s
  Set      []  -> InfiniteSet
  Set     [s] | s==wholeInterval -> EmptySet    
  Set       s  -> _fromList (loop NegInf s >>= _canonicalize) where
    loop mark s = case s of
      []                   -> [_mkSeg (stepUp mark) PosInf]
      [Interval a PosInf]   -> [_mkSeg (stepUp mark) (stepDown a)]
      Interval NegInf b : s -> loop b s
      Interval a      b : s -> _mkSeg (stepUp mark) (stepDown a) : loop b s
      Single  a        : s -> _mkSeg (stepUp mark) (stepDown a) : loop a s

exclusive :: (Ord c, Enum c, InfBound c) => Set c -> Set c -> Set c
exclusive a b = Dao.Interval.delete (Dao.Interval.union a b) (Dao.Interval.intersect a b)

union :: (Ord c, Enum c, InfBound c) => Set c -> Set c -> Set c
union a b = case a of
  EmptySet     -> b
  InfiniteSet  -> InfiniteSet
  InverseSet a -> Dao.Interval.union (_invert a) b
  Set       [] -> b
  Set        a -> case b of
    EmptySet     -> Set a
    InfiniteSet  -> InfiniteSet
    InverseSet b -> Dao.Interval.union (Set a) (_invert b)
    Set       [] -> Set a
    Set        b -> innerProduct intervalUnion a b

unions :: (Ord c, Enum c, InfBound c) => [Set c] -> Set c
unions = foldl Dao.Interval.union Dao.Interval.empty

intersect :: (Ord c, Enum c, InfBound c) => Set c -> Set c -> Set c
intersect a b = case a of
  EmptySet     -> EmptySet    
  InfiniteSet  -> b
  InverseSet a -> Dao.Interval.intersect (_invert a) b
  Set       [] -> EmptySet    
  Set        a -> case b of
    EmptySet     -> EmptySet    
    InfiniteSet  -> Set a
    InverseSet b -> Dao.Interval.intersect (Set a) (_invert b)
    Set       [] -> EmptySet    
    Set        b -> innerProduct intervalIntersect a b

intersections :: (Ord c, Enum c, InfBound c) => [Set c] -> Set c
intersections = foldl Dao.Interval.intersect whole

delete :: (Ord c, Enum c, InfBound c) => Set c -> Set c -> Set c
delete a b = case b of
  EmptySet     -> a
  InfiniteSet  -> EmptySet    
  InverseSet b -> Dao.Interval.delete a (_invert b)
  Set       [] -> a
  Set        b -> case a of
    EmptySet     -> EmptySet    
    InfiniteSet  -> _invert (Set b)
    InverseSet a -> Dao.Interval.delete (_invert a) (Set b)
    Set       [] -> EmptySet    
    Set        a -> fromList (_exclusive intervalDelete a b)
      -- Here we call 'fromList' instead of '_fromList' because an additional 'intervalNub'
      -- operation is required.

