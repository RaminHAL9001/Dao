-- "Dao/Text/Location.hs"  a data type for locations in text files.
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

module Dao.Text.Location where

import           Prelude hiding ((.), id)

import           Dao.Count
import           Dao.Lens
import           Dao.TestNull
import           Dao.Text
import           Dao.Text.PPrint

import           Control.Applicative
import           Control.Category
import           Control.DeepSeq
import           Control.Monad

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as Strict
import           Data.Typeable

----------------------------------------------------------------------------------------------------

-- | A count of how many characters have been read during parsing.
type CharCount    = Count

-- | A count of how many columns have been parsed since the start of the last line.
type ColumnNumber = Count

-- | A count of how many lines have been encounted since the start of parsing.
type LineNumber   = Count

----------------------------------------------------------------------------------------------------

-- | A 'LineNumber' and 'ColumnNumber' useful for error reporting.
newtype TextPoint = TextPoint (LineNumber, ColumnNumber) deriving (Eq, Ord, Typeable)

instance PPrintable TextPoint where
  pPrint p = [pShow $ p~>lineNumber, pChar ':', pShow $ p~>columnNumber]

instance Show TextPoint where { show = showPPrint 4 80 . pPrint; }

instance NFData TextPoint where { rnf (TextPoint o) = deepseq o (); }

textPointTuple :: Monad m => Lens m TextPoint (LineNumber, ColumnNumber)
textPointTuple = newLens (\ (TextPoint o) -> o) (\o _ -> TextPoint o)

lineNumber :: Monad m => Lens m TextPoint LineNumber
lineNumber = textPointTuple >>> tuple0

columnNumber :: Monad m => Lens m TextPoint LineNumber
columnNumber = textPointTuple >>> tuple1

----------------------------------------------------------------------------------------------------

class HasTextPoint o where { textPoint :: Monad m => Lens m o TextPoint; }

instance HasTextPoint TextPoint where { textPoint = id; }

----------------------------------------------------------------------------------------------------

-- | Two 'TextPoints' marking the start and end of some substring in the parsed text.
newtype TextRegion = TextRegion (Maybe (TextPoint, TextPoint)) deriving (Eq, Ord, Typeable)

instance TestNull TextRegion where
  testNull (TextRegion o) = isNothing o
  nullValue = TextRegion Nothing

instance Monoid TextRegion where
  mempty                                = TextRegion Nothing
  mappend (TextRegion a) (TextRegion b) = TextRegion $ msum
    [ a >>= \ (startA, endA) -> b >>= \ (startB, endB) ->
        Just (min startA startB, max endA endB)
    , a, b
    ]

instance PPrintable TextRegion where
  pPrint (TextRegion o) = case o of
    Nothing     -> []
    Just (a, b) -> pPrint a ++ [pChar '-'] ++ pPrint b

instance Show TextRegion where { show = showPPrint 4 80 . pPrint; }

instance NFData TextRegion where
  rnf (TextRegion  Nothing     ) = ()
  rnf (TextRegion (Just (a, b))) = deepseq a $! deepseq b ()

-- | Construct a textRegion from two 'TextPoint's.
textRegion :: TextPoint -> TextPoint -> TextRegion
textRegion a b = TextRegion $ Just (min a b, max a b)

----------------------------------------------------------------------------------------------------

-- | This data type contains an inner type that can be described with a 'Dao.Grammar.Grammar'. When
-- the grammar for this type is converted to a parser, the source location before and after the
-- parse are recorded and stored with the inner type so analyzing the 'Location' will tell you from
-- where in the source file the inner type was parsed.
newtype Location = Location (Maybe TextPoint, Maybe TextPoint) deriving (Eq, Ord, Show, Typeable)

instance TestNull Location where
  testNull (Location (a, b)) = isNothing a && isNothing b
  nullValue = Location (Nothing, Nothing)

instance Monoid Location where
  mempty = nullValue
  mappend (Location (loA, hiA)) (Location (loB, hiB)) =
    Location (min <$> loA <*> loB, max <$> hiA <*> hiB)

instance NFData Location where
  rnf (Location (a, b)) = maybe () (flip deepseq ()) a <> maybe () (flip deepseq ()) b

locationFromPoint :: TextPoint -> Location
locationFromPoint o = new [locationStart <~ Just o, locationEnd <~ Just o]

locationTuple :: Monad m => Lens m Location (Maybe TextPoint, Maybe TextPoint)
locationTuple = newLens (\ (Location o) -> o) (\o _ -> Location o)

locationStart :: Monad m => Lens m Location (Maybe TextPoint)
locationStart = locationTuple >>> tuple0

locationEnd :: Monad m => Lens m Location (Maybe TextPoint)
locationEnd = locationTuple >>> tuple1

----------------------------------------------------------------------------------------------------

-- | Class of data types that contain text location information.
class HasLocation o where { location :: Monad m => Lens m o Location; }

instance HasLocation Location where { location = id; }

class Monad m => HasCurrentLocation m where { currentLocation :: m Location; }

----------------------------------------------------------------------------------------------------

-- | Convert a 'Location' to a 'TextRegion', if possible.
locationRegion :: Location -> TextRegion
locationRegion (Location (lo, hi)) = TextRegion $ (,) <$> lo <*> hi

-- | The 'asciiLineCounter' is a nice default line counter that simply increments the 'TextPoint'
-- for as many @'\n'@ characters as there are in the given text.
asciiLineCounter :: Count -> StrictText -> TextPoint -> TextPoint
asciiLineCounter tab t =
  let column n (TextPoint (ln, col)) = TextPoint (ln, n+col)
      line n col (TextPoint (ln, _)) = TextPoint (n+ln, col)
      loop cx st = case cx of
        []   -> st
        c:cx -> case c of
          '\t' -> loop cx $ column tab st
          '\n' -> loop cx $ line 1 1 st
          c | ' '<=c -> loop cx $ column 1 st
          _          -> loop cx st
  in  loop (Strict.unpack t)

