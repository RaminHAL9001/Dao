-- "src/Dao/Object/DeepSeq.hs"  instantiating 'Dao.Object.Object' into
-- the 'Control.DeepSeq.NFData' class.
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


-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.DeepSeq where

import           Dao.Debug.OFF
import           Dao.Object
import           Dao.Token
import           Dao.Glob
import           Dao.EnumSet
import qualified Dao.Tree    as T
import           Dao.Object.Parser

import           Control.DeepSeq

import           Data.Maybe
import           Data.List (partition, isSuffixOf)
import qualified Data.Map    as M
import qualified Data.IntMap as I

import qualified Data.ByteString.Lazy as B

----------------------------------------------------------------------------------------------------

instance NFData UStr where
  rnf (UStr a) = seq a ()

instance NFData Comment where
  rnf (InlineComment  a) = seq a ()
  rnf (EndlineComment a) = seq a ()

instance NFData a => NFData (Com a) where
  rnf (Com         a  ) = deepseq a ()
  rnf (ComBefore a b  ) = deepseq a $! deepseq b ()
  rnf (ComAfter    a b) = deepseq a $! deepseq b ()
  rnf (ComAround a b c) = deepseq a $! deepseq b $! deepseq c ()

instance NFData UpdateOp where { rnf a = seq a () }
instance NFData ArithOp1 where { rnf a = seq a () }
instance NFData ArithOp2 where { rnf a = seq a () }
instance NFData LambdaExprType where { rnf a = seq a () }
instance NFData TypeID   where { rnf a = seq a () }
instance NFData PatUnit  where { rnf a = seq a () }

instance NFData ObjectExpr where
  rnf VoidExpr = ()
  rnf (Literal      a b    ) = deepseq a $! deepseq b $! ()
  rnf (AssignExpr   a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (Equation     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (PrefixExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ParenExpr    a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ArraySubExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (FuncCall     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (DictExpr     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ArrayExpr    a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (StructExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (DataExpr     a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (LambdaExpr   a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d () 
  rnf (MetaEvalExpr a b    ) = deepseq a $! deepseq b $! ()

instance NFData Location where
  rnf LocationUnknown = ()
  rnf (Location a b c d e f) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e $! deepseq f ()

instance NFData ScriptExpr where
  rnf (EvalObject   a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (IfThenElse   a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()
  rnf (TryCatch     a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ForLoop      a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (WhileLoop    a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ContinueExpr a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ReturnExpr   a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (WithDoc      a b c    ) = deepseq a $! deepseq b $! deepseq c ()

instance NFData Reference where
  rnf (IntRef     a  ) = deepseq a ()
  rnf (LocalRef   a  ) = deepseq a ()
  rnf (StaticRef  a  ) = deepseq a ()
  rnf (QTimeRef   a  ) = deepseq a ()
  rnf (GlobalRef  a  ) = deepseq a ()
  rnf (ProgramRef a b) = deepseq a $! deepseq b ()
  rnf (FileRef    a b) = deepseq a $! deepseq b ()
  rnf (Subscript  a b) = deepseq a $! deepseq b ()
  rnf (MetaRef    a  ) = deepseq a ()

instance NFData Object where
  rnf  ONull         = ()
  rnf  OTrue         = ()
  rnf (OType      a) = deepseq a ()
  rnf (OInt       a) = deepseq a ()
  rnf (OWord      a) = deepseq a ()
  rnf (OLong      a) = deepseq a ()
  rnf (OFloat     a) = deepseq a ()
  rnf (ORatio     a) = deepseq a ()
  rnf (OComplex   a) = deepseq a ()
  rnf (OTime      a) = deepseq a ()
  rnf (ODiffTime  a) = deepseq a ()
  rnf (OChar      a) = deepseq a ()
  rnf (OString    a) = deepseq a ()
  rnf (ORef       a) = deepseq a ()
  rnf (OPair  (a,b)) = deepseq a $! deepseq b ()
  rnf (OList      a) = deepseq a ()
  rnf (OSet       a) = deepseq a ()
  rnf (OArray     a) = deepseq a ()
  rnf (ODict      a) = deepseq a ()
  rnf (OIntMap    a) = deepseq a ()
  rnf (OTree      a) = deepseq a ()
  rnf (OGlob      a) = deepseq a ()
  rnf (OScript    a) = deepseq a ()
  rnf (ORule      a) = deepseq a ()
  rnf (OBytes     a) = seq a ()

instance (NFData a, NFData b) => NFData (T.Tree a b) where
  rnf  T.Void            = ()
  rnf (T.Leaf       a  ) = deepseq a ()
  rnf (T.Branch       b) = deepseq b ()
  rnf (T.LeafBranch a b) = deepseq a $! deepseq b ()

instance NFData Glob where
  rnf (Glob a b) = deepseq a $! deepseq b ()

instance NFData Subroutine where
  rnf (Subroutine a b c) = deepseq a $! deepseq b $! seq c ()

instance NFData Rule where
  rnf (Rule a b c) = deepseq a $! deepseq b $! seq c ()

instance NFData Pattern where
  rnf  ObjAnyX         = ()
  rnf  ObjMany         = ()
  rnf  ObjAny1         = ()
  rnf (ObjEQ      a  ) = deepseq a ()
  rnf (ObjType    a  ) = deepseq a ()
  rnf (ObjBounded a b) = deepseq a ()
  rnf (ObjList    a b) = seq a $! deepseq b ()
  rnf (ObjNameSet a b) = seq a $! deepseq b ()
  rnf (ObjIntSet  a b) = seq a $! deepseq b ()
  rnf (ObjElemSet a b) = seq a $! deepseq b ()
  rnf (ObjChoice  a b) = seq a $! deepseq b ()
  rnf (ObjLabel   a b) = seq a $! deepseq b ()
  rnf (ObjFailIf  a b) = seq a $! deepseq b ()
  rnf (ObjNot     a  ) = deepseq a ()

instance NFData ObjSetOp where { rnf a = seq a () }

