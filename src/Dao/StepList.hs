-- "src/Dao/StepList.hs"  provides a fundamental data type used in the
-- Dao System, the "StepList", which is a cursor that can step forward
-- or backward through a list.
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


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module provides a basic lits cursor interface, that is, a list that you can step through
-- forward or backward, item by item. This is useful for building line editors. This module export
-- operators, so it is best not to import this module qualified. Therefore functions similar to
-- 'Data.Set.empty' or 'Data.Set.singleton' are named 'slEmpty' and 'slSingleton' to prevent name
-- conflicts without using qualified importing.
module Dao.StepList where

import           Control.Applicative
import           Control.Monad

import           Data.Array.IArray
import           Data.Typeable
import           Data.Monoid

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

slToArray :: StepList a -> Maybe (Array Int a)
slToArray (StepList cur len left right) =
  if len==0
  then Nothing
  else Just $ array (negate cur, len-cur-1)
          (zip (iterate (subtract 1) (negate 1)) left ++ zip (iterate (+1) 0) right)

-- | Creates an array storing the list where the indecies are 'Data.Array.IArray.bounds' such that
-- value at index zero is the value returned by 'slHeadR', items before the cursor have negative
-- indecies, and items after the cursor are zero or positive. If the cursor is at the right-most
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
slMoveCheck :: Int -> StepList a -> Bool
slMoveCheck delta (StepList cur len _ _) = inRange (0, len) (cur+delta)

-- | Returns 'Prelude.True' if it the index is within the bounds of the list.
slIndexCheck :: Int -> StepList a -> Bool
slIndexCheck i (StepList _ len _ _) = inRange (0, len) i

-- | Shift the cursor @delta@ elements to the left if @delta@ is negative, or @delta@ elements to
-- the right if @delta@ is positive.
slMoveCursor :: Int -> StepList a -> StepList a
slMoveCursor delta a@(StepList cur len left right)
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
slCursorTo i a@(StepList cur _ _ _) = slMoveCursor (i-cur) a

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

