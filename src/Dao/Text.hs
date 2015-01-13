-- "Dao/Text.hs"  classes for working with the Text data type that is imported
-- from the Data.Text module.
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
-- this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

-- | 'TestNull' is a class used to test whether data types being evaluated in
-- the Dao runtime are null or not, especially in conditional statements.
module Dao.Text
  ( StrictText, LazyText,
    FromText(maybeFromText, fromText),
    ToText(toText), listToTextWith, listToText,
    ToTextIO(toTextIO), listToTextWithIO, listToTextIO,
    binaryPutText, binaryGetText, isAlphaNum_, lazyLengthCompare,
    levenshteinDistanceMatrix, levenshteinDistance, fuzzyCompare, showMatrix
  ) where

import           Dao.Int

import           Control.Applicative

import           Data.Array.Unboxed
import           Data.Char
import           Data.Int
import           Data.Monoid
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import qualified Data.Text.Encoding as T

import qualified Data.Binary     as B
import qualified Data.Binary.Get as B

----------------------------------------------------------------------------------------------------

type StrictText = Strict.Text
type LazyText   = Lazy.Text

-- | Like 'Data.Char.isAlphaNum' but also matches an underscore character.
isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = isAlphaNum c || c=='_'

-- | Compare the length of the given 'LazyText', avoiding (if possible) fusing the whole string.
-- Return a 'Prelude.Ordering'.
lazyLengthCompare :: Int64 -> LazyText -> Ordering
lazyLengthCompare n t = if null tx then compare n 0 else loop n tx where
  tx = Lazy.toChunks t
  loop n tx = if n<0 then LT else case tx of
    []   -> compare n 0
    t:tx -> loop (n - fromIntegral (Strict.length t)) tx


----------------------------------------------------------------------------------------------------

class FromText o where
  fromText      :: Strict.Text -> o
  maybeFromText :: Strict.Text -> Maybe o
  maybeFromText = Just . fromText

instance FromText Strict.Text where { maybeFromText = Just; fromText = id; }
instance FromText Lazy.Text   where { maybeFromText = Just . fromText; fromText = Lazy.fromChunks . return; }
instance FromText String      where { maybeFromText = Just . fromText; fromText = Strict.unpack; }

----------------------------------------------------------------------------------------------------

class ToText o where { toText :: o -> Strict.Text }

instance ToText Strict.Text where { toText = id }
instance ToText Lazy.Text   where { toText = mconcat . Lazy.toChunks }
instance ToText String      where { toText = Strict.pack }

listToTextWith :: (o -> Strict.Text) -> [o] -> Strict.Text
listToTextWith toText ox = let ts = Strict.singleton in
  ts '(' <> Strict.intercalate (ts ' ') (fmap toText ox) <> ts ')'

listToText :: ToText o => [o] -> Strict.Text
listToText = listToTextWith toText

----------------------------------------------------------------------------------------------------

class ToTextIO o where { toTextIO :: o -> IO Strict.Text }

instance ToTextIO Strict.Text where { toTextIO = return }
instance ToTextIO String      where { toTextIO = return . toText }

listToTextWithIO :: (o -> IO Strict.Text) -> [o] -> IO Strict.Text
listToTextWithIO toTextIO ox = let ts = return . Strict.singleton in fmap mconcat $ sequence $
  [ts '(', Strict.intercalate <$> (ts ' ') <*> mapM toTextIO ox, ts ')']

listToTextIO :: ToTextIO o => [o] -> IO Strict.Text
listToTextIO = listToTextWithIO toTextIO

----------------------------------------------------------------------------------------------------

binaryPutText :: Strict.Text -> B.Put
binaryPutText o = vlPutInteger (toInteger $ Strict.length o) >> B.put (T.encodeUtf8 o)

binaryGetText :: B.Get Strict.Text
binaryGetText = fmap fromInteger B.get >>= fmap T.decodeUtf8 . B.getByteString

----------------------------------------------------------------------------------------------------

-- | This function efficiently computes the Levenshtein distance matrix for any two strings.
-- <http://en.wikipedia.org/wiki/Levenshtein_distance>
levenshteinDistanceMatrix :: ToText t => t -> t -> UArray(Int,Int) Int
levenshteinDistanceMatrix s' t' = array (bounds d) $ assocs d where
  index  = Strict.index
  length = Strict.length
  (s, t) = (toText s', toText t')
  (m, n) = (length s , length t )
  d :: Array(Int,Int) Int
  d = array ((0, 0), (m, n)) $ concat $
    [ [((0, 0), 0)]
    , zip (zip [1..m] $ repeat 0) [1..m]
    , zip (zip (repeat 0) [1..n]) [1..n] ,
      (,) <$> [1..n] <*> [1..m] >>= \ (j, i) -> let { i0 = pred i; j0 = pred j; } in
        [ ( (i, j)
          , if index s i0 == index t j0
            then d!(i0, j0)
            else minimum $ fmap (+ 1) $ [d!(i0, j), d!(i, j0), d!(i0, j0)]
          )
        ]
    ]

-- | This function efficiently computes the Levenshtein distance between any two strings.
-- <http://en.wikipedia.org/wiki/Levenshtein_distance>
levenshteinDistance :: ToText t => t -> t -> Int
levenshteinDistance s t = let m = levenshteinDistanceMatrix s t in m ! (snd $ bounds m)

-- | Use the 'levenshteinDistance' function to compute the difference value between two strings @s@
-- and @t@, return a 'Prelude.Rational' value such that equal strings evaluate to a value of @1.0@,
-- and different strings evaluate to some value between @1.0@ and @0.0@, computed by:
--
-- @
-- let maxLen = 'Prelude.max' ('Data.Text.length' s) ('Data.Text.length' t)
-- in  (maxLen - 'levenshteinDistance' s t) / maxLen
-- @
--
-- **NOTE:** This function does not modify the strings in any way, for example converting to
-- lower-case. The caller of this function is responsible for such preprocessing.
fuzzyCompare :: ToText t => t -> t -> Rational
fuzzyCompare s' t' =
  let (s, t) = (toText s', toText t')
      maxLen = max (Strict.length s) (Strict.length t)
  in toRational (maxLen - levenshteinDistance s t) / toRational maxLen

showMatrix :: forall array i o . (Ix i, Show o, IArray array o) => array(i,i) o -> Strict.Text
showMatrix arr = Strict.intercalate (Strict.singleton '\n') $ do
  let strArr :: Array(i,i) Strict.Text
      strArr = array (bounds arr) $ fmap (fmap $ toText . show) $ assocs arr
  let maxLen = 1 + maximum (fmap Strict.length $ elems strArr)
  let sp   t = Strict.pack (replicate (max 2 $ maxLen - Strict.length t) ' ') <> t
  let ((loX, loY), (hiX, hiY)) = bounds strArr
  range (loY, hiY) >>= \y -> [Strict.concat $ (\x -> sp $ strArr!(x, y)) <$> range (loX, hiX)]

