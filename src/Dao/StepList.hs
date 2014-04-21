-- "src/Dao/StepList.hs"  provides a fundamental data type used in the
-- Dao System, the "StepList", which is a cursor that can step forward
-- or backward through a list.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This is a line-editor object, but it works with arbitrary lists of objects, but this will work
-- for editing arbitrary lists. You could use it to create an ordinary line editor by representing a
-- file as be a list of strings representing a file. each string could further be converted to a
-- StepList containing characters to edit the line. 
--
-- This module provides a basic list cursor interface, that is, a list that you can step through
-- forward or backward, item by item. This is useful for building line editors. This module export
-- operators, so it is best not to import this module qualified. Therefore functions similar to
-- 'Data.Set.empty' or 'Data.Set.singleton' are named 'slEmpty' and 'slSingleton' to prevent name
-- conflicts without using qualified importing.
--
module Dao.StepList where

import           Control.Applicative
import           Control.Monad

import           Data.Array.IArray
import qualified Data.IntMap as I
import           Data.List
import           Data.Monoid
import           Data.Typeable

----------------------------------------------------------------------------------------------------

data StepList a
  = StepList
    { slCursor :: Int
    , slLength :: Int
    , slLeftOfCursor :: [a]
      -- ^ items to the left of the cursor are stored in reverse order. If your 'StepList' is:
      -- > list_A = [0, 1, 2, 3, 4] <> [5, 6, 7, 8]
      -- evaluating @'leftOfCursor' list_A@ produces:
      -- > [4, 3, 2, 1, 0]
    , slRightOfCursor :: [a]
      -- ^ items to the left of the cursor are stored in forward order. If your 'StepList' is:
      -- > list_A = [0, 1, 2, 3, 4] <> [5, 6, 7, 8]
      -- evaluating @'leftOfCursor' list_A@ produces:
      -- > [5, 6, 7, 8]
    }
  deriving (Eq, Ord, Show, Read, Typeable)

instance Monoid (StepList a) where { mempty = slEmpty; mappend = (+:+); }
instance Functor     StepList where { fmap  = slMap }
instance MonadPlus   StepList where { mzero = mempty; mplus = (+:+); }
instance Applicative StepList where { pure  = return; (<*>) = ap;      }
instance Alternative StepList where { empty = mempty; (<|>) = (+:+); }
instance Monad       StepList where
  -- | Return is defined by 'slSingletonL'. This is because when lists are used as a monad, the
  -- 'Control.Monad.return' operations that occur earlier in the computation place items in the list
  -- earlier than 'Control.Monad.return' operations that occur later in the computation. Therefore a
  -- monadic computation like @a >> b@ will have @a@ placed to the left of @b@.
  return = slSingletonL
  -- | Just like how the '(Prelude.>>=)' operator instantiated for Haskell lists is the
  -- 'Prelude.concatMap' function, the '(Prelude.>>=)' operator for 'StepList's is the 'slConcatMap'
  -- function.
  (>>=) = flip slConcatMap

-- | Test if the 'StepList' contains no elements.
slNull :: StepList a -> Bool
slNull sl = null (slLeftOfCursor sl) && null (slRightOfCursor sl)

-- | Create an empty 'StepList'.
slEmpty :: StepList a
slEmpty = StepList 0 0 [] []

-- | Create a 'StepList' with a single item to the right of the cursor.
slSingletonR :: a -> StepList a
slSingletonR a = StepList 1 1 [a] []

-- | Create a 'StepList' with a single item to the left of the cursor.
slSingletonL :: a -> StepList a
slSingletonL a = StepList 1 1 [a] []

-- | Create a 'StepList' from a list of elements and an initial cursor position. The cursor position
-- can be out of range, it will be placed at the end by default.
slFromList :: Int -> [a] -> StepList a
slFromList i lst =
  let len = length lst
      cur = max 0 i
      (left, right) = splitAt cur lst
  in  StepList
      { slCursor        = min cur len
      , slLength        = len
      , slLeftOfCursor  = left
      , slRightOfCursor = right
      }

-- | Create a 'StepList' with two lists, the items to the left of the cursor, and the items to the
-- right of the cursor. The elements of the list to the left of the cursor are not reversed when
-- constructing the 'StepList' with this constructor. When the cursor is moved back to the
-- beginning of the list, the items will be reversed, i.e.
-- > slToList (slFromList [0,1,2,3] [4,5,6,7])
-- will evaluate to the list:
-- > [3,2,1,0,4,5,6,7]
slFromLeftRight :: [a] -> [a] -> StepList a
slFromLeftRight left right =
  StepList{ slCursor=cur, slLength=cur+rightlen, slLeftOfCursor=left, slRightOfCursor=right } where
    cur      = length left
    rightlen = length right

-- | Assign values to integer indicies in the 'Data.IntMap.IntMap' data type, and this function will
-- convert the 'Data.IntMap.IntMap' to a 'StepList'. Negative indicies will be placed to the left of
-- the cursor, with the lowest value being furthest left, positive indicies will be placed to the
-- right of the cursor with the highest value being furthest right.
slFromIntMap :: I.IntMap a -> StepList a
slFromIntMap im = slFromLeftRight (fmap snd $ sortfst left) (fmap snd $ sortfst right) where
  sortfst = sortBy (\a b -> compare (fst a) (fst b))
  (left, right) = partition ((<0) . fst) $ I.assocs im

slToArray :: StepList a -> Maybe (Array Int a)
slToArray (StepList cur len left right) =
  if len==0
  then Nothing
  else Just $ array (negate cur, len-cur-1)
          (zip (iterate (subtract 1) (negate 1)) left ++ zip (iterate (+1) 0) right)

-- | Creates an array storing the list where the indicies are 'Data.Array.IArray.bounds' such that
-- value at index zero is the value returned by 'slHeadR', items before the cursor have negative
-- indicies, and items after the cursor are zero or positive. If the cursor is at the right-most
-- position of the list, the resulting array, every item is assigned to a negative index.
slFromArray :: Maybe (Array Int a) -> StepList a
slFromArray arr = case arr of
  Nothing  -> mempty
  Just arr ->
    let bnds = bounds arr
        (lo, hi) = maybe bnds id $ do
          (lo, hi) <- return bnds
          (lo, hi) <- return (min lo hi, max lo hi)
          return $ if lo>0 then (0, hi-lo) else (lo, hi)
        len   = hi - lo + 1
        cur   = negate lo
        left  = map (arr!) (takeWhile (>=lo) $ iterate (subtract 1) (negate 1))
        right = map (arr!) (takeWhile (<=hi) $ iterate (+1) 0)
    in  StepList cur len left right

-- | Convert the 'StepList' to an ordinary Haskell list.
slToList :: StepList a -> [a]
slToList (StepList _ _ left right) = reverse left ++ right

-- | When two 'StepList's are concatenated with @a +:+ b@ think of @b@ being inserted into the cursor
-- position at @a@, with all the items to the left of the cursor in @b@ being placed to the right of
-- the cursor of @a@ and all the items to the right of the cursor of @b@ being placed to the left
-- the cursor of @a@. Here is an ASCII illustration:
-- > list_A         = [a0, a1, a2, a3, a4]         <>                             [a5, a6, a7, a8]
-- >         list_B =                     [b0, b1] <> [b2, b3, b4, b5, b6, b7, b8]
-- > ----- concatenate -----
-- > list_A<>list_B = [a0, a1, a2, a3, a4, b0, b1] <> [b2, b3, b4, b5, b6, b7, b8, a5, a6, a7, a8]
(+:+) :: StepList a -> StepList a -> StepList a
(StepList c1 n1 b1 a1) +:+ (StepList c2 n2 b2 a2) = StepList (c1+c2) (n1+n2) (b1++b2) (a2++a1)
infixr 5 +:+

slMap :: (a -> b) -> StepList a -> StepList b
slMap f (StepList cur len left right) = StepList cur len (map f left) (map f right)

slConcat :: [StepList a] -> StepList a
slConcat = foldl (+:+) slEmpty

-- | The @(Control.Monad.>>=)@ operator applied to lists is the same as the 'Data.List.concatMap'
-- function. In the case of 'StepList', the @(Control.Monad.>>=)@ operator is defined with
-- 'Data.Monoid.mconcat' and 'Prelude.map'.
slConcatMap :: (a -> StepList b) -> StepList a -> StepList b
slConcatMap f = slConcat . map f . slToList

-- | Place an item to the left of the cursor. This operator binds to the RIGHT with a precedence of
-- 5, it does not bind left. The reason is that this operator is more similar to the Haskell list
-- operator @(:)@.
-- > x <: ([0, 1, 2] <> [3, 4])
-- >      ([0, 1, 2, x] <> [3, 4])
(<|) :: a -> (StepList a -> StepList a)
(<|) a (StepList cur len left right) = StepList (cur+1) (len+1) (a:left) right
infixr 5 <|

-- | Place an item to the right of the cursor. This operator binds to the right with a precedence of
-- 5, just like the Haskell list operator @(:)@.
-- > x :> ([0, 1, 2] <> [3, 4])
-- >      ([0, 1, 2] <> [x, 3, 4])
(|>) :: a -> StepList a -> StepList a
(|>) a (StepList cur len left right) = StepList cur (len+1) left (a:right)
infixr 5 |>

-- | Place an item to the left of the cursor. This operator binds to the RIGHT with a precedence of
-- 5, it does not bind left. The reason is that this operator is more similar to the Haskell list
-- operator @(++)@.
-- > [a0, a1, a2] <++ ([b0, b1, b2] <> [b3, b4])
-- >                  ([b0, ab, b2, a0, a1, a2] <> [b3, b4])
(<++) :: [a] -> StepList a -> StepList a
(<++) = flip (foldr (<|))
infixr 5 <++

-- | Place an item to the right of the cursor. This operator binds to the right with a precedence of
-- 5, just like the Haskell list operator @(++)@.
-- > [a0, a1, a2] ++> ([b0, b1, b2] <> [b3, b4])
-- >                  ([b0, b1, b2] <> [a0, a1, a2, b3, b4])
(++>) :: [a] -> StepList a -> StepList a
(++>) = flip (foldr (|>))
infixr 5 ++>

-- | Returns 'Prelude.True' if it is possible to move the cursor left or right by @n@ steps.
slShiftCheck :: Int -> StepList a -> Bool
slShiftCheck delta (StepList cur len _ _) = inRange (0, len) (cur+delta)

-- | Returns 'Prelude.True' if it the index is within the bounds of the list.
slIndexCheck :: Int -> StepList a -> Bool
slIndexCheck i (StepList _ len _ _) = inRange (0, len) i

-- | Shift the cursor @delta@ elements to the left if @delta@ is negative, or @delta@ elements to
-- the right if @delta@ is positive.
slCursorShift :: Int -> StepList a -> StepList a
slCursorShift delta a@(StepList cur len left right)
  | delta==0 = a
  | delta<0 && abs delta <= cur =
      let (middle, left') = splitAt (abs delta) left
      in StepList (cur+delta) len left' (reverse middle ++ right)
  | delta>0 && delta <= len-cur =
      let (middle, right') = splitAt delta right
      in StepList (cur+delta) len (left ++ reverse middle) right'
  | otherwise = error "moved cursor past end of list"

-- | Place the cursor at an index position.
slCursorTo :: Int -> StepList a -> StepList a
slCursorTo i a@(StepList cur _ _ _) = slCursorShift (i-cur) a

-- | Lookup a value at an absolute index (not relative to the cursor).
slIndex :: Int -> StepList a -> Maybe a
slIndex i sl =
  let (StepList _ _ _ right) = slCursorShift i sl
  in case right of { [] -> Nothing; o:_ -> return o; }

-- | A bounds value expressed as a pair of indicies relative to the current cursor position can be
-- converted to an bounds value expressed as a pair of indicies relative to the 0th element in the
-- 'StepList'. This is the inverse operation of 'slAbsToRel'.
slRelToAbs :: StepList a -> (Int, Int) -> (Int, Int)
slRelToAbs (StepList cur _ _ _) (lo, hi) = (cur+lo, cur+hi)

-- | A bounds value expressed as a pair of indicies relative to the 0th position in the 'StepList'
-- can be converted to a bounds value expressed as a pair of indicies relative to the current
-- cursor position. This is the inverse operation of 'slRelToAbs'.
slAbsToRel :: StepList a -> (Int, Int) -> (Int, Int)
slAbsToRel (StepList cur _ _ _) (lo, hi) = (lo-cur, hi-cur)

-- | Selects items around the cursor, with the option of deleting the items selected from the
-- 'StepList'. The first boolean parameter indicates whether or not the list should be altered by
-- deleting the items that were selected. Items to the left of the cursor are selected if the
-- indicies are negative and items to the right of the cursor are selected if the indicies are
-- positive. Specify the range to select by passing a lower and upper bound relative to the cursor.
-- For example, to select a range of ten items, five before the cursor and five after the cursor,
-- use @slGetRelRange False (negate 5) 5@. To delete that same range from the 'StepList' and also
-- return it use @slGetRelRange True (negate 5) 5@.
--
-- This function evaluates to a pair of 'StepList's. The 'Prelude.fst' is the selected items, the
-- 'Prelude.snd' is the modified 'StepList' which may be identical to the parameter 'StepList' if
-- the 'doDelete::Bool' parameter is 'Prelude.False'.
slCutRelRange :: Bool -> (Int, Int) -> StepList a -> (StepList a, StepList a)
slCutRelRange doDelete (lo, hi) o@(StepList cur len left right) = maybe (mempty, o) id $ do
  let cutBiased minLen list = do
        let lim = Just . min minLen . abs
        lo <- lim $ min lo hi
        hi <- lim $ max lo hi
        let cutLen = hi-lo
        guard (cutLen/=0)
        (keep, list) <- Just $ splitAt lo     list
        (cut , list) <- Just $ splitAt cutLen list
        return (cutLen, keep, cut, list)
  case lo of
    lo|lo<0  -> case hi of
      hi|hi<=0 -> do
        (cutLen, keep, cut, left) <- cutBiased cur left
        cut <- Just $ StepList cutLen cutLen cut []
        Just $
          if doDelete
          then (cut, StepList (max 0 (cur-cutLen)) (len-cutLen) (left++keep) right)
          else (cut, o)
      hi|hi>0  -> do
        lo <- Just $ min cur (abs lo)
        hi <- Just $ min (len-cur) hi
        let cutLen = hi+lo
        (midLeft,  left ) <- Just $ splitAt lo left
        (midRight, right) <- Just $ splitAt hi right
        middle            <- Just $ StepList lo cutLen midLeft midRight
        Just $
          if doDelete
          then (middle, StepList (max 0 (cur-cutLen)) (len-cutLen) left right)
          else (middle, o)
      _ -> undefined
    lo|lo>=0 -> case hi of
      hi|hi<=0 -> if lo==hi then Nothing else Just $ slCutRelRange doDelete (hi, lo) o
      hi|hi>0  -> do
        (cutLen, keep, cut, right) <- cutBiased (len-cur) right
        cut <- Just $ StepList 0 cutLen [] cut
        Just $
          if doDelete
          then (cut, StepList cur (max cur (len-cutLen)) left (keep++right))
          else (cut, o)
      _ -> undefined
    _ -> undefined

-- | Like 'slCutRelRange' but operates on an upper and lower bound indicated by absolute indicies,
-- rather than indicies relative to the cursor.
slCutAbsRange :: Bool -> (Int, Int) -> StepList a -> (StepList a, StepList a)
slCutAbsRange doDelete bnds list = slCutRelRange doDelete (slAbsToRel list bnds) list

slDeleteRelRange :: (Int, Int) -> StepList a -> StepList a
slDeleteRelRange bnds = snd . slCutAbsRange True bnds

slDeleteAbsRange :: (Int, Int) -> StepList a -> StepList a
slDeleteAbsRange bnds list = slDeleteRelRange (slAbsToRel list bnds) list

slCopyRelRange :: (Int, Int) -> StepList a -> StepList a
slCopyRelRange bnds = fst . slCutRelRange False bnds

slCopyAbsRange :: (Int, Int) -> StepList a -> StepList a
slCopyAbsRange bnds list = slCopyRelRange (slAbsToRel list bnds) list

-- | Many functions may need to modify a 'StepList' but only on the elements to the left or right of
-- the cursor. These functions take a boolean type called 'Bias'
data Bias = ToLeft | ToRight deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

-- | Modify the items to the left or right of the cursor. The function will return a polymorphic
-- value paired with an updated list of items to the left of the cursor.
biasedApply :: Bias -> ([elems] -> (result, [elems])) -> StepList elems -> (result, StepList elems)
biasedApply bias f (StepList cur len left right) = case bias of
  ToRight -> let (result, right') = f right in (result, StepList cur (length right') left right')
  ToLeft  -> 
    let (result, left') = f left
        newlen = length left'
    in (result, StepList newlen (len-cur+newlen) left' right)

-- | Evaluates to true if the cursor is beyond the left or right-most position in the list.
slAtEnd :: Bias -> StepList a -> Bool
slAtEnd bias (StepList cur len _ _) = case bias of
  ToRight -> cur==len
  ToLeft  -> cur==0

-- | Move the cursor to the right-most or left-most end of the 'StepList', depending on the 'Bias'
-- value given. The word Return has nothing to do with monad, we use this term because this function
-- is similar to the /carriage return/ of a typewriter.
slReturn :: Bias -> StepList a -> StepList a
slReturn bias (StepList cur len left right) = case bias of
  ToLeft  -> StepList 0 (cur+len) [] (reverse left ++ right)
  ToRight -> StepList len len (left ++ reverse right) []

slHeadR :: StepList a -> a
slHeadR (StepList _ _ _ a) = head a

slHeadL :: StepList a -> a
slHeadL (StepList _ _ a _) = head a

slTail :: Bias -> StepList elems -> StepList elems
slTail bias = snd . biasedApply bias ((,) () . tail)

slTake :: Bias -> Int -> StepList a -> StepList a
slTake bias n = snd . biasedApply bias ((,) () . take n)

slTakeWhile :: Bias -> (a -> Bool) -> StepList a -> StepList a
slTakeWhile bias p = snd . biasedApply bias ((,) () . takeWhile p)

slDrop :: Bias -> Int -> StepList a -> StepList a
slDrop bias n = snd . biasedApply bias ((,) () . drop n)

slDropWhile :: Bias -> (a -> Bool) -> StepList a -> StepList a
slDropWhile bias p = snd . biasedApply bias ((,) () . dropWhile p)

slSplitAt :: Bias -> Int -> StepList a -> ([a], StepList a)
slSplitAt bias n = biasedApply bias (splitAt n)

slSpan :: Bias -> (a -> Bool) -> StepList a -> ([a], StepList a)
slSpan bias p = biasedApply bias (span p)

slBreak :: Bias -> (a -> Bool) -> StepList a -> ([a], StepList a)
slBreak bias p = biasedApply bias (break p)

-- | Apply a modifier to the whole list, try to keep the cursor in the same place. If the 'StepList'
-- shrinks below where the cursor was, the cursor is placed at the end of the 'StepList'.
slMapAll :: ([a] -> [b]) -> StepList a -> StepList b
slMapAll fn (StepList cur _ left right) =
  let (left', right') = splitAt cur (fn $ reverse left ++ right)
      newlen = length left' + length right'
  in StepList (min cur newlen) newlen (reverse left') right'

