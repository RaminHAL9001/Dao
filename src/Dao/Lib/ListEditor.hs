-- "src/Dao/Lib/ListEditor.hs"  built-in object for Dao programs that can
-- functions like a line editor, but for arbitrary types, not just strings.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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

-- | This is a line-editor object, but it works with arbitrary lists of objects, but this will work
-- for editing arbitrary lists. You could use it to create an ordinary line editor by representing a
-- file as be a list of strings representing a file. each string could further be converted to a
-- StepList containing characters to edit the line. 
module Dao.Lib.ListEditor where

import           Dao.String
import           Dao.StepList
import           Dao.Predicate
import           Dao.PPrint
import qualified Dao.Binary as B
import           Dao.Interpreter

----------------------------------------------------------------------------------------------------

loadLibrary_ListEditor :: DaoSetup
loadLibrary_ListEditor = do
  return ()
  daoFunction "ListEditor" $
    daoFunc
    { funcAutoDerefParams = True
    , daoForeignFunc = \ () _ox -> return (Nothing, ())
    }


