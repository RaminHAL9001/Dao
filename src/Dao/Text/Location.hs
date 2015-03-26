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

import           Dao.Count
import           Dao.PPrint
import           Dao.Text

import           Control.DeepSeq

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
data TextPoint = TextPoint{ lineNumber :: LineNumber, columnNumber :: ColumnNumber }
  deriving (Eq, Ord, Typeable)

instance PPrintable TextPoint where
  pPrint p = [pShow $ lineNumber p, pText ":", pShow $ columnNumber p]

instance Show TextPoint where { show (TextPoint num col) = show num ++ ':' : show col; }

instance NFData TextPoint where
  rnf (TextPoint a b) = deepseq a $! deepseq b ()

----------------------------------------------------------------------------------------------------

-- | A class defining a single function for data types which store tokens or source location
-- information where it is possible to clear the information to save space, without changing the
-- meaning of the data. This is true of the 'Space' and 'LineSpace' data types which keep the space
-- characters until you clear them, and for the 'TextRegion' data type which stores information
-- about locations of tokens. Although a text region is meaningless without the location
-- information, it is convenient in cases where the data type storing the location information is
-- still useful after the location information has been dicarded.
class DeleteContents o where { deleteContents :: o -> o; }

----------------------------------------------------------------------------------------------------

-- | Two 'TextPoints' demarcing the start and end of some substring in the parsed text.
newtype TextRegion = TextRegion{ textRegionToPair :: Maybe (TextPoint, TextPoint) }
  deriving (Eq, Ord, Typeable)

instance Monoid TextRegion where
  mempty                                = TextRegion Nothing
  mappend (TextRegion a) (TextRegion b) = case a of
    Nothing               -> TextRegion b
    (Just (startA, endA)) -> case b of
      Nothing               -> TextRegion a
      (Just (startB, endB)) -> TextRegion $ Just (min startA startB, max endA endB)

instance Show TextRegion where
  show (TextRegion o) = case o of
    Nothing     -> ""
    Just (a, b) -> show a ++ '-' : show b

instance NFData TextRegion where
  rnf (TextRegion  Nothing     ) = ()
  rnf (TextRegion (Just (a, b))) = deepseq a $! deepseq b ()

instance DeleteContents TextRegion where { deleteContents _ = TextRegion Nothing; }

textRegion :: TextPoint -> TextPoint -> TextRegion
textRegion a b = TextRegion $ Just (min a b, max a b)

----------------------------------------------------------------------------------------------------

-- | This data type contains an inner type that can be described with a 'Dao.Grammar.Grammar'. When
-- the grammar for this type is converted to a parser, the source location before and after the
-- parse are recorded and stored with the inner type so analyzing the 'Location' will tell you from
-- where in the source file the inner type was parsed.
data Location o
  = NoLocation              o
  | StartLocation TextPoint o
  | EndLocation             o TextPoint
  | Location      TextPoint o TextPoint
  deriving (Eq, Ord, Show, Typeable)

instance Functor Location where
  fmap f o = case o of
    NoLocation       o    -> NoLocation       (f o)
    StartLocation lo o    -> StartLocation lo (f o)
    EndLocation      o hi -> EndLocation      (f o) hi
    Location      lo o hi -> Location      lo (f o) hi

instance DeleteContents (Location o) where
  deleteContents o = case o of
    NoLocation      o   -> NoLocation o
    StartLocation _ o   -> NoLocation o
    EndLocation     o _ -> NoLocation o
    Location      _ o _ -> NoLocation o

instance Monoid o => Monoid (Location o) where
  mempty = NoLocation mempty
  mappend a b = case a of
    NoLocation        a     -> case b of
      NoLocation        b     -> NoLocation                  (a<>b)
      StartLocation loB b     -> StartLocation      loB      (a<>b)
      EndLocation       b hiB -> EndLocation                 (a<>b)      hiB
      Location      loB b hiB -> Location           loB      (a<>b)      hiB
    StartLocation loA a     -> case b of
      NoLocation        b     -> StartLocation      loA      (a<>b)
      StartLocation loB b     -> StartLocation (min loA loB) (a<>b)
      EndLocation       b hiB -> Location           loA      (a<>b)          hiB
      Location      loB b hiB -> Location      (min loA loB) (a<>b)          hiB
    EndLocation       a hiA -> case b of
      NoLocation        b     -> EndLocation                 (a<>b)      hiA
      StartLocation loB b     -> Location               loB  (a<>b)      hiA
      EndLocation       b hiB -> Location               hiB  (a<>b) (max hiA hiB)
      Location      loB b hiB -> Location               loB  (a<>b) (max hiA hiB)
    Location      loA a hiA -> case b of
      NoLocation        b     -> Location           loA      (a<>b)      hiA
      StartLocation loB b     -> Location      (min loA loB) (a<>b)      hiA
      EndLocation       b hiB -> Location           loA      (a<>b) (max hiA hiB)
      Location      loB b hiB -> Location      (min loA loB) (a<>b) (max hiA hiB)

instance NFData o => NFData (Location o) where
  rnf a = case a of
    NoLocation      b   -> deepseq b ()
    StartLocation a b   -> deepseq a $! deepseq b ()
    EndLocation     b c -> deepseq b $! deepseq c ()
    Location      a b c -> deepseq a $! deepseq b $! deepseq c ()

----------------------------------------------------------------------------------------------------

class LocationFunctor dat o where { fmapLocation :: (Location o -> Location o) -> dat -> dat; }

instance LocationFunctor (Location o) o where { fmapLocation = ($); }

unwrapLocation :: Location o -> o
unwrapLocation o = case o of
  NoLocation      o   -> o
  StartLocation _ o   -> o
  EndLocation     o _ -> o
  Location      _ o _ -> o

-- | Convert a 'Location' to a 'TextRegion', if possible.
locationRegion :: Location o -> TextRegion
locationRegion o = case o of
  Location      lo _ hi -> TextRegion $ Just (lo, hi)
  _                     -> TextRegion Nothing

startLocation :: Location o -> Maybe TextPoint
startLocation o = case o of
  StartLocation o _   -> Just  o
  Location      o _ _ -> Just  o
  _                   -> Nothing

endLocation :: Location o -> Maybe TextPoint
endLocation o = case o of
  EndLocation   _ o -> Just  o
  Location    _ _ o -> Just  o
  _                 -> Nothing

-- | The 'asciiLineCounter' is a nice default line counter that can be used with the 'movePoint'
-- function. 
asciiLineCounter :: Count -> StrictText -> TextPoint -> TextPoint
asciiLineCounter tab t =
  let column n (TextPoint ln col) = TextPoint ln $ col+n
      line n col (TextPoint ln _) = TextPoint (ln+n) col
      loop cx st = case cx of
        []   -> st
        c:cx -> case c of
          '\t' -> loop cx $ column tab st
          '\n' -> loop cx $ line 1 1 st
          c | ' '<=c -> loop cx $ column 1 st
          _          -> loop cx st
  in  loop (Strict.unpack t)

