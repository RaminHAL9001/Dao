-- "src/Dao/Foreign.hs"  declares the "ObjectInterface" data type which
-- provides all of the callbacks necessary to allow arbitrary Dynamic
-- types to be treated as Objects in the Dao interpreter.
-- 
-- Copyright (C) 2008-2013  Ramin Honary.
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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | A module providing 'ObjectClass', a clean and consistent interface for constructing objects
-- that can be used in the Dao runtime environment, and associating method tables with those objects
-- so the methods can be evaluated at runtime. This module is specifically designed to be imported
-- by any module throughout this software. If your data type needs to be used in the Dao system,
-- this is the set of APIs you use to make your data types available. In fact, the only Dao module
-- imported by this one is "Dao.String", which provides the most fundamental string interface.
--
-- Originally, this was all done with concrete types, but it made too many things dependent on the
-- "Dao.Object" module. So the necessary APIs were moved out of "Dao.Object" and into this module
-- and made polymorphic. As a result, the APIs are actually so polymorphic is might be diffictult to
-- understand why it would be necessary at all. Again, it is to provide a consistent interface to
-- all other modules without making everything depend on the "Dao.Object" module.
module Dao.Runtime where

import           Dao.String

import           Control.Concurrent.MVar

import qualified Data.Map       as M

----------------------------------------------------------------------------------------------------

