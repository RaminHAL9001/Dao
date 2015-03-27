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

-- | 'TestNull' is a class used to test whether data types being evaluated in the Dao runtime are
-- null or not, especially in conditional statements.
--
-- This module may seem like a good place to find the 'Dao.Array.levenshteinStringDistance', but
-- that function is actually in the "Dao.Array" module because the algorithm requires arrays in
-- order to compute efficiently.
module Dao.Text
  ( StrictText, LazyText, StringLength(stringLength),
    FromText(maybeFromText, fromText), FromLazyText(maybeFromLazyText, fromLazyText),
    ToText(toText), ToLazyText(toLazyText),
    binaryPutText, binaryGetText, isAlphaNum_, lazyLengthCompare,
  ) where

import           Dao.Int
import           Dao.Count

import           Data.Char
import           Data.Int
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import qualified Data.Text.Encoding as T

import qualified Data.Binary     as B
import qualified Data.Binary.Put as B
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

class StringLength o where { stringLength :: o -> Count; }

instance StringLength Lazy.Text   where { stringLength = Count . Lazy.length; }
instance StringLength Strict.Text where { stringLength = Count . fromIntegral . Strict.length; }
instance StringLength String      where { stringLength = Count . fromIntegral . Prelude.length; }

----------------------------------------------------------------------------------------------------

class FromText o where
  fromText      :: Strict.Text -> o
  maybeFromText :: Strict.Text -> Maybe o
  maybeFromText = Just . fromText

instance FromText Strict.Text where { maybeFromText = Just; fromText = id; }
instance FromText Lazy.Text   where { maybeFromText = Just . fromText; fromText = Lazy.fromChunks . return; }
instance FromText String      where { maybeFromText = Just . fromText; fromText = Strict.unpack; }

----------------------------------------------------------------------------------------------------

class FromLazyText o where
  fromLazyText      :: Lazy.Text -> o
  maybeFromLazyText :: Lazy.Text -> Maybe o
  maybeFromLazyText = Just . fromLazyText

instance FromLazyText Lazy.Text   where { fromLazyText = id; }
instance FromLazyText Strict.Text where { fromLazyText = Lazy.toStrict; }
instance FromLazyText String      where { fromLazyText = Lazy.unpack; }

----------------------------------------------------------------------------------------------------

class ToText o where { toText :: o -> Strict.Text; }

instance ToText Strict.Text where { toText = id; }
instance ToText Lazy.Text   where { toText = Lazy.toStrict; }
instance ToText String      where { toText = Strict.pack; }

----------------------------------------------------------------------------------------------------

class ToLazyText o where { toLazyText :: o -> Lazy.Text; }

instance ToLazyText Strict.Text where { toLazyText = Lazy.fromStrict; }
instance ToLazyText Lazy.Text   where { toLazyText = id; }
instance ToLazyText String      where { toLazyText = Lazy.pack; }

----------------------------------------------------------------------------------------------------

binaryPutText :: Strict.Text -> B.Put
binaryPutText o = vlPutInteger (toInteger $ Strict.length o) >> B.putByteString (T.encodeUtf8 o)

binaryGetText :: B.Get Strict.Text
binaryGetText = fmap fromInteger B.get >>= fmap T.decodeUtf8 . B.getByteString

