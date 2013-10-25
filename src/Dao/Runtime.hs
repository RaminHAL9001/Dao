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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module providing 'GenObjectClass', a clean and consistent interface for constructing objects
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

import           Dao.Debug.OFF
import           Dao.String

import           Control.Concurrent.MVar

import           Data.Typeable
import qualified Data.Map       as M

----------------------------------------------------------------------------------------------------

-- | In the Dao runtime, there is a universal @obj@ data type which contains every data type that
-- can possibly be used by the Runtime environment. @obj@ must be some kind of wrapper around the
-- 'Data.Dynamic.Dynamic' data type provided by Haskell. In the "Dao.Object" module, this universal
-- object is the 'Dao.Object.Object'.
--
-- The 'GenObjectClass' provides methods that can construct an object of type @obj@ storing /any/ type
-- which instantiates this class, which means you must provide a constructor that wraps a @typ@ in
-- an @obj@. This should be done using 'Data.Dynamic.Dynamic', which is why it is necessary to
-- instantiate 'Data.Typeable.Typeable'. In "Dao.Object", that constructor is 'Dao.Object.OHaskell'
-- because it stores types from the Haskell language.
--
-- Of course, an object that simply exists but cannot be analyzed or modified in any way is useless.
-- So 'GenObjectClass' also requires you to provide a 'MethodTable'. There will be only one
-- 'MethodTable' for any given Haskell @typ@, and this is guaranteed by the Haskell compiler by
-- using the functional dependency feature. The functions in this table will be used during Dao
-- program execution if an object is encountered that needs to be, for example, checked if it is
-- null in an if statement, or added to an integer using the addition operator. In these cases, the
-- object 
--
-- In case you are wondering why we don't just use RankNTypes the answer is simple: RankNTypes are
-- only useful for compiling Haskell code. We need callbacks that are actual objects that can be
-- used in the Dao runtime.
class (Functor mtabl, Typeable typ) => GenObjectClass typ mtabl obj | typ -> mtabl, typ -> obj where
  -- | This is the table that will contain everything that your function can do. Again, there is
  -- only one of these per @typ@.
  objectInterface :: mtabl typ
  -- | When converting a method table of type @mtabl typ@ to a table of type @mtabl obj@, this
  -- function will perform the task of converting input variables to functions.
  wrapInObj :: typ -> obj
  -- | The inverse operation of 'wrapInObj', re-wraps the @typ@ object in universal @obj@ object.
  -- When converting a method of table @mtabl typ@ to @mtabl obj@, this function performs the task
  -- of re-wrapping objects of type @typ@ resulting from function evaluation after evaluation
  -- completes.
  extractObj :: obj -> typ
  -- | The 'objectInterface' for this table can be defined using your data type @typ@, and then
  -- provide a converter function to convert a table with functions of type @typ@ to functions of
  -- the universal type @obj@.
  convertTable :: mtabl typ -> mtabl obj
  -- | The final essential detail is to provide a universal object type constructor which can take
  -- an @mtabl@ and construct an object of type @obj@ storing a reference to the table with it. The
  -- table used with the constructor is of type @mtab obj@ rather than @mtab typ@, it is expected
  -- that the functions within the @mtab typ@ which operated on your specific type @typ@ will all be
  -- converted to functions that do the same thing but now perform the additional function of unwrap
  -- the @typ@ from the @obj@ using 'extractObj' before evaluating inputs, and re-wrapping the
  -- resulting @typ@ objects back into @obj@ objects after evaluation has completed.
  constrObjWithTab :: mtabl obj -> typ -> obj
  

-- | Like the C++ language keyword, this is a conveneince function that constructs an @obj@ with
-- data of type @typ@ and, automatically calls 'constrObjWithTab' for you without having to
-- specify the specific method table type, the method table is inferred from @typ@ alone.
new :: (Typeable typ, GenObjectClass typ mtabl obj) => typ -> obj
new a = mkNew objectInterface a where
  mkNew :: (Typeable typ, GenObjectClass typ mtabl obj) => mtabl typ -> typ -> obj
  mkNew ifc a = constrObjWithTab (convertTable ifc) a

----------------------------------------------------------------------------------------------------

-- | The 'GenRuntime' is the shared state visible to every module. Every process will have a single
-- 'Runtime' created by the main function, and every 'ExecUnit' created will receive a pointer to
-- thie 'GenRuntime'. Modules communicate with each other by calling the correct API functions
data GenRuntime mtabl obj xunit
  = Runtime
    { globalMethodTable    :: mtabl obj
    , pathIndex            :: MVar (M.Map UPath xunit)
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , defaultTimeout       :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , importGraph          :: MVar (M.Map UPath xunit)
    }

