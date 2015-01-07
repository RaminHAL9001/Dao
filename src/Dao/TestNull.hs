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
module Dao.TestNull
  ( TestNull(testNull, nullValue),
    TestNullIO(testNullIO, nullValueIO)
  )
  where

import qualified Dao.Interval   as Iv

import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as T
import qualified Data.Map       as M
import qualified Data.Set       as S

----------------------------------------------------------------------------------------------------

class TestNull o where { testNull :: o -> Bool; nullValue :: o; }

instance TestNull ()          where { testNull = const True;  nullValue = (); }
instance TestNull [o]         where { testNull = null;        nullValue = []; }
instance TestNull T.Text      where { testNull = T.null;      nullValue = T.empty; }
instance TestNull Strict.Text where { testNull = Strict.null; nullValue = Strict.empty; }
instance TestNull (M.Map i o) where { testNull = M.null;      nullValue = M.empty; }
instance TestNull (S.Set   o) where { testNull = S.null;      nullValue = S.empty; }
instance TestNull Int         where { testNull = (==0);       nullValue = 0; }
instance TestNull Integer     where { testNull = (==0);       nullValue = 0; }
instance TestNull Double      where { testNull = (==0);       nullValue = 0; }
instance (Ord o, Enum o, Iv.InfBound o) =>
  TestNull (Iv.Set  o) where { testNull = Iv.null;     nullValue = Iv.empty; }

----------------------------------------------------------------------------------------------------

class TestNullIO o where { testNullIO :: o -> IO Bool; nullValueIO :: IO o; }

instance TestNullIO ()          where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance TestNullIO [o]         where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance TestNullIO T.Text      where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance TestNullIO Strict.Text where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance TestNullIO (M.Map i o) where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance TestNullIO (S.Set   o) where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance TestNullIO Int         where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance TestNullIO Integer     where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance TestNullIO Double      where { testNullIO = return . testNull; nullValueIO = return nullValue; }
instance (Eq o, Ord o, Enum o, Iv.InfBound o) =>
  TestNullIO (Iv.Set  o) where { testNullIO = return . testNull; nullValueIO = return nullValue; }

