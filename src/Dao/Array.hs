-- "Dao/Tree.hs"  provides a fundamental data type used by Dao.
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

module Dao.Array
  ( Array, array, arraySpan, size, indexOK, elems, lastElem, (!)
  , toIArray
  )
  where

import           Dao.Text
import           Dao.TestNull

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad hiding (mapM, msum)

import qualified Data.Array.IArray as A
import           Data.Maybe
import           Data.Foldable
import           Data.Monoid
import           Data.Traversable
import           Data.Typeable

----------------------------------------------------------------------------------------------------

newtype Array o = Array (Maybe (A.Array Int o)) deriving (Eq, Ord, Show, Typeable)

instance NFData o => NFData (Array o) where { rnf (Array o) = deepseq o () }

instance Functor Array where { fmap f (Array o) = Array $ fmap (fmap f) o }

instance Monad Array where
  return = Array . Just . A.array (0, 0) . return . (,) 0
  o >>= f = array $ elems o >>= elems . f

instance MonadPlus Array where { mzero=mempty; mplus=mappend; }

instance Applicative Array where { pure=return; (<*>)=ap; }

instance Alternative Array where { empty=mzero; (<|>)=mplus; }

instance Traversable Array where { traverse f = fmap array . traverse f . elems; }

instance Foldable Array where { foldr f i = Data.Foldable.foldr f i . elems; }

instance Monoid (Array o) where
  mempty = Array Nothing
  mappend a@(Array arrA) b@(Array arrB) = Array $ msum
    [ (\arrA arrB -> A.listArray (0, size a + size b - 1) $ A.elems arrA ++ A.elems arrB)
        <$> arrA <*> arrB
    , arrA, arrB
    ]

instance TestNull (Array o) where
  nullValue = mempty
  testNull (Array o) = isJust o

instance ToText o => ToText (Array o) where
  toText (Array o) = case o of
    Nothing -> toText "()"
    Just  o -> listToText (A.elems o)

instance ToTextIO o => ToTextIO (Array o) where
  toTextIO (Array o) = case o of
    Nothing -> return $ toText "()"
    Just  o -> listToText <$> traverse toTextIO (A.elems o)

-- | Returns the minimum bounds that contains the bounds for both given 'Data.Array.IArray.Array's.
-- *NOTE* that this operates on arrays from the "Data.Array.IArray" module.
arraySpan :: A.Ix i => A.Array i x -> A.Array i y -> (i, i)
arraySpan a b =
  let (loA, hiA) = A.bounds a
      (loB, hiB) = A.bounds b
  in  (min loA loB, max hiA hiB)

array :: [o] -> Array o
array ox = case ox of
  [] -> mempty
  ox -> Array $ Just $ A.listArray (0, length ox - 1) ox

elems :: Array o -> [o]
elems (Array o) = maybe [] A.elems o

lastElem :: Array o -> Maybe o
lastElem (Array o) = o >>= \o -> Just $ o A.! snd (A.bounds o)

size :: Array o -> Int
size (Array a) = maybe 0 ((1+) . uncurry subtract . A.bounds) a

indexOK :: Array o -> Int -> Bool
indexOK (Array o) i = maybe False (flip A.inRange i . A.bounds) o

(!) :: Array o -> Int -> Maybe o
(!) arr@(Array o) i = guard (indexOK arr i) >> (A.! i) <$> o
infixr 8 !

toIArray :: Array o -> Maybe (A.Array Int o)
toIArray (Array o) = o

