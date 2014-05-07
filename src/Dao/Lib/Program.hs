-- "src/Dao/Lib/Program.hs"  built-in object for Dao programs that can
-- be edited and run in the same runtime that is editing them, allowing
-- a larger program to "learn" new rules.
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

module Dao.Lib.Program where

import           Dao.String
import           Dao.Predicate
import           Dao.PPrint
import qualified Dao.Binary as B
import           Dao.Interpreter
import           Dao.Interpreter.AST

import qualified Data.ByteString.Lazy as B
import           Data.Typeable

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State

import           System.IO

