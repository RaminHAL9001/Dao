-- "Dao/TestNull.hs"  a class used by conditionals in the Dao runtime
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
module Dao.TestNull( TestNull(testNull, nullValue) ) where

import           Dao.Count
import qualified Dao.Interval   as Iv

import           Data.Monoid
import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as T

----------------------------------------------------------------------------------------------------

class TestNull o where { testNull :: o -> Bool; nullValue :: o; }

instance TestNull ()          where { testNull = const True;  nullValue = (); }
instance TestNull [o]         where { testNull = null;        nullValue = []; }
instance TestNull T.Text      where { testNull = T.null;      nullValue = T.empty; }
instance TestNull Strict.Text where { testNull = Strict.null; nullValue = Strict.empty; }
instance TestNull (M.Map i o) where { testNull = M.null;      nullValue = M.empty; }
instance TestNull (S.Set   o) where { testNull = S.null;      nullValue = S.empty; }
instance TestNull Int         where { testNull = (==0);       nullValue = 0; }
instance TestNull Count       where { testNull = (==0);       nullValue = 0; }
instance TestNull Integer     where { testNull = (==0);       nullValue = 0; }
instance TestNull Double      where { testNull = (==0);       nullValue = 0; }
instance (Ord o, Enum o, Iv.InfBound o) =>
  TestNull (Iv.Set  o) where { testNull = Iv.null;     nullValue = Iv.empty; }
instance TestNull o => TestNull (Sum o) where 
  testNull (Sum o) = testNull o
  nullValue = Sum nullValue
instance TestNull o => TestNull (Product o) where 
  testNull (Product o) = testNull o
  nullValue = Product nullValue
instance TestNull o => TestNull (Dual o) where 
  testNull (Dual o) = testNull o
  nullValue = Dual nullValue
instance TestNull Any where { nullValue = Any False; testNull = not . getAny; }
instance TestNull All where { nullValue = All False; testNull = not . getAll; }

----------------------------------------------------------------------------------------------------

instance (TestNull a, TestNull b) => TestNull (a, b) where
  nullValue = (nullValue, nullValue)
  testNull (a, b) = testNull a && testNull b

instance (TestNull a, TestNull b, TestNull c) => TestNull (a, b, c) where
  nullValue = (nullValue, nullValue, nullValue)
  testNull (a, b, c) = testNull a && testNull b && testNull c

instance (TestNull a, TestNull b, TestNull c, TestNull d) => TestNull (a, b, c, d) where
  nullValue = (nullValue, nullValue, nullValue, nullValue)
  testNull (a, b, c, d) = testNull a && testNull b && testNull c && testNull d

instance (TestNull a, TestNull b, TestNull c, TestNull d, TestNull e) => TestNull (a, b, c, d, e) where
  nullValue = (nullValue, nullValue, nullValue, nullValue, nullValue)
  testNull (a, b, c, d, e) = testNull a && testNull b && testNull c && testNull d && testNull e

instance (TestNull a, TestNull b, TestNull c, TestNull d, TestNull e, TestNull f) => TestNull (a, b, c, d, e, f) where
  nullValue = (nullValue, nullValue, nullValue, nullValue, nullValue, nullValue)
  testNull (a, b, c, d, e, f) = testNull a && testNull b && testNull c && testNull d && testNull e && testNull f

instance (TestNull a, TestNull b, TestNull c, TestNull d, TestNull e, TestNull f, TestNull g) => TestNull (a, b, c, d, e, f, g) where
  nullValue = (nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue)
  testNull (a, b, c, d, e, f, g) = testNull a && testNull b && testNull c && testNull d && testNull e && testNull f && testNull g

instance (TestNull a, TestNull b, TestNull c, TestNull d, TestNull e, TestNull f, TestNull g, TestNull h) => TestNull (a, b, c, d, e, f, g, h) where
  nullValue = (nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue)
  testNull (a, b, c, d, e, f, g, h) = testNull a && testNull b && testNull c && testNull d && testNull e && testNull f && testNull g && testNull h

instance (TestNull a, TestNull b, TestNull c, TestNull d, TestNull e, TestNull f, TestNull g, TestNull h, TestNull i) => TestNull (a, b, c, d, e, f, g, h, i) where
  nullValue = (nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue)
  testNull (a, b, c, d, e, f, g, h, i) = testNull a && testNull b && testNull c && testNull d && testNull e && testNull f && testNull g && testNull h && testNull i

instance (TestNull a, TestNull b, TestNull c, TestNull d, TestNull e, TestNull f, TestNull g, TestNull h, TestNull i, TestNull j) => TestNull (a, b, c, d, e, f, g, h, i, j) where
  nullValue = (nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue, nullValue)
  testNull (a, b, c, d, e, f, g, h, i, j) = testNull a && testNull b && testNull c && testNull d && testNull e && testNull f && testNull g && testNull h && testNull i && testNull j

