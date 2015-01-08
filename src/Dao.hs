-- "Dao.hs"  Combinators for constructing modules and functions in the Dao runtime.
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

-- | Dao is a collection of handy APIs for rule-based artificial intelligence programming. This
-- module re-exports the most useful modules.
--
-- For those who are just getting started, documentation for these modules provides a good starting
-- point:
--
-- * "Dao.Rule"   -- provides logic programming with production rules
-- * "Dao.Object" -- extends the 'Data.Dynamic.Dynamic' data type with useful class instantiations
-- * "Dao.Lens"   -- provides a very simple but powerful lens interface
--
-- Dao also provides some in-depth APIs for creating parsers:
-- 
-- * "Dao.Interval"    -- provides non-contiguous intervals, useful for defining character sets
-- * "Dao.Grammar"     -- provides regular expressions and grammars that can be converted to parsers
-- * "Dao.Text.Parser" -- a 'Data.Text.Lazy.Text'-based parser that can be automatically generated from a 'Dao.Grammar.Grammar'
-- * "Dao.PPrint"      -- simple pretty printing library with facilities for plain-text and HTML output.
--
module Dao
  ( module Dao.Array,
    module Dao.Count,
    module Dao.Lens,
    module Dao.Logic,
    module Dao.Object,
    module Dao.Predicate,
    module Dao.Rule,
    module Dao.TestNull,
    module Dao.Text
  )
  where

import Dao.Array
import Dao.Count
import Dao.Lens
import Dao.Logic
import Dao.Object
import Dao.Predicate
import Dao.Rule
import Dao.TestNull
import Dao.Text

