-- "src/Dao/Object/Pattern.hs" a data type that can match
-- 'Dao.Object.Object's as a predicate.
-- 
-- Copyright (C) 2008-2012  Ramin Honary.
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dao.Object.Pattern where

import           Dao.String
import           Dao.Object
import           Dao.Object.Math
import qualified Dao.Tree as T
import           Dao.Predicate
import           Dao.EnumSet

import           Data.Maybe
import           Data.Typeable
import           Data.Array.IArray
import qualified Data.Set as S

import           Control.Monad
import           Control.Monad.State

----------------------------------------------------------------------------------------------------

-- | When matching an object to a predicate, this value is used to control matching. If the match
-- fails (rather than backtracks), the location of the failure is recorded as a
-- 'Dao.Object.Reference' that can be used to select the value from the object. This is typically a
-- 'Dao.Object.Subscript' value.
type MatchValue a = PValue Reference a

data MatcherState
  = MatcherState
    { matcherRef :: Reference
    , matcherTree :: T.Tree Name Object
    }

-- | A 'Control.Monad.State.State' monad used to execute matches
newtype Matcher a = Matcher { matcherPTransState :: PTrans Reference (State MatcherState) a }

instance Monad Matcher where
  return = Matcher . return
  (Matcher f) >>= mfa = Matcher (f >>= matcherPTransState . mfa)
  (Matcher fa) >> (Matcher fb) = Matcher (fa >> fb)
  fail msg = gets matcherRef >>= \ref -> Matcher (tokenFail ref msg)

instance Functor Matcher where
  fmap f (Matcher a) = Matcher (fmap f a)

instance MonadPlus Matcher where
  mplus (Matcher a) (Matcher b) = Matcher (mplus a b)
  mzero = Matcher mzero

instance MonadState MatcherState Matcher where
  get = Matcher $ lift get
  put = Matcher . lift . put
  state = Matcher . lift . state

----------------------------------------------------------------------------------------------------

data ObjSetOp = OnlySet | AnyOfSet | AllOfSet | NoneOfSet | OnlyOneOf
  deriving (Eq, Ord, Typeable, Show, Read)

-- | An object pattern, a data type that can be matched against objects,
-- assigning portions of that object to variables stored in a
-- 'Dao.Tree.Tree' structure.
data ObjPat 
  = ObjAnyX -- ^ matches any number of objects, introduces backtracking when matching in a list
  | ObjAny1 -- ^ matches any one object
  | ObjEQ Object -- ^ simply checks if the object is exactly equivalent
  | ObjType (EnumSet TypeID) -- ^ checks if the object type is any of the given types.
  | ObjBounded T_ratio T_ratio -- ^ checks that numeric types are in a certain range.
  | ObjList TypeID [ObjPat]
    -- ^ recurse into a list-like object given by TypeID (OType for any list-like object)
  | ObjNameSet ObjSetOp (S.Set Name) -- ^ checks if a map object contains every name
  | ObjIntSet  ObjSetOp (EnumSet Int)
    -- ^ checks if an intmap or array object contains every index
  | ObjElemSet TypeID ObjSetOp (S.Set ObjPat)
    -- ^ recurse into a set-like object given by TypeID, match elements in the set according to
    -- ObjSetOp.
  | ObjLabel Name ObjPat
    -- ^ if the object matching matches this portion of the 'ObjPat', then save the object into the
    -- resulting 'Dao.Tree.Tree' under this name.

matchObject :: ObjPat -> Object -> Matcher Object
matchObject pat o = case pat of
  ObjAnyX       -> return o
  ObjAny1       -> return o
  ObjEQ q   | q == o                  -> return o
  ObjType t | setMember t (objType o) -> return o
  ObjBounded  lo hi                   -> Matcher $ pvalue $ msum $
    [ objToRational o >>= \r -> guard (lo <= r && r <= hi) >> return o
    , case o of
        OArray arr -> let (a, b) = bounds arr in guard (lo <= toRational a && toRational b <= hi) >> return o
        _          -> mzero
    ]

