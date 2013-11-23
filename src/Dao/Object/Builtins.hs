-- "src/Dao/Object/Builtins.hs"  provides the object interface between
-- Dao and Haskell language data types that are essential to the Dao
-- runtime.
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.Builtins where

import           Dao.String
import qualified Dao.Tree     as T
import           Dao.Token
import           Dao.Object
import           Dao.Runtime
import           Dao.Binary
import           Dao.Parser
import           Dao.Struct

import           Dao.Object.AST
import           Dao.Object.Binary
import           Dao.Object.Struct
import           Dao.Object.Parser
import           Dao.Object.DeepSeq

listClass :: DaoClassDef [Object]
listClass = do
  autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefIterator
  defBinaryFmt putUnwrapped getUnwrapped
  defTreeFormat (return . T.Leaf . OList) $ \t -> case T.getLeaf t of
    Just (OList o) -> return o
    _              -> fail "expecting list"
  defInitializer $ \args o -> do
    unless (null args) (fail "list constructor takes no arguments")
    return (OList o)
  let append fn op = fn $ \o p -> case p of
    OList p -> OList (o++p)
    _       -> fail "appending to list, object not of list type"
  append defUpdateOp UADD
  append defInfixOp  ADD



