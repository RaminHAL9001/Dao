-- "src/Ext/TheoryOfMind.hs"  the Dao program is intended to be used as
-- an AI. To that end, I am creating a module for modeling a "theory of
-- mind", an idea from psychology about a person's ability to understand
-- that there are other people that behave autonomously and have
-- different information than ones self.
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


-- {-# LANGUAGE TemplateHaskell #-}

-- | This module is pretty much where everything begins. It is the smallest interface that can be
-- imported by any Haskell program making use of the Dao System. You can use the functions in this
-- module to initialize a 'Dao.Interpreter.Runtime' data structure, then use it to start an input query
-- loop with 'inputQueryLoop'. The query loop requires you pass a callback function that, on each
-- evaluation, returns the next string to be used the query to the 'Dao.Interpreter.Runtime'.
--
-- To have more control over execution of string queries, you will need to import the "Dao.Tasks"
-- module and make use of those functions to create 'Dao.Interpreter.Job's from string queries, then wait
-- for those 'Dao.Interpreter.Job's to complete.

module Dao.Ext.TheoryOfMind where

import           Dao.String
import           Dao.Interpreter
import qualified Dao.Tree as T

import           Data.Monoid
import           Data.List

