-- "Dao/Text/Builder.hs"  a class for generalizing text and string types which
-- can be constructed and deconstructed as 'Data.Text.Lazy.Text' data types.
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

-- | This module provides the 'Builder' class, which is a class of data types that can be
-- constructed and deconstructed from lazy 'Data.Text.Lazy.Text' data.
module Dao.Text.Builder where

import           Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text      as Strict

----------------------------------------------------------------------------------------------------

-- | Like 'Dao.Text.ToText', but operates on lazy 'Data.Text.Lazy.Text' data instead of strict
-- 'Data.Text.Text'.
class Monoid text => TextBuilder text where
  toLazyText :: text -> T.Text
  fromLazyText :: T.Text -> text
  consChar :: Char -> text -> text
  snocChar :: text -> Char -> text
  pack :: [Char] -> text
  unpack :: text -> [Char]

instance TextBuilder String where
  toLazyText = T.pack
  fromLazyText = T.unpack
  consChar = (:)
  snocChar cx c = cx++[c]
  pack = id
  unpack = id

instance TextBuilder T.Text where
  toLazyText = id
  fromLazyText = id
  consChar = T.cons
  snocChar = T.snoc
  pack = T.pack
  unpack = T.unpack

instance TextBuilder Strict.Text where
  toLazyText = T.fromChunks . return
  fromLazyText = mconcat . T.toChunks
  consChar = Strict.cons
  snocChar = Strict.snoc
  pack = Strict.pack
  unpack = Strict.unpack

