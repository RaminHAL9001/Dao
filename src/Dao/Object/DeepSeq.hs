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

import           Dao.Object
import           Dao.Object.AST
import           Dao.Token
import           Dao.Glob
import           Dao.EnumSet
import           Dao.Token
import           Dao.Parser
import qualified Dao.Tree    as T

import           Control.DeepSeq

import           Data.Maybe
import           Data.List (partition, isSuffixOf)
import qualified Data.Map    as M
import qualified Data.IntMap as I

import qualified Data.ByteString.Lazy as B

----------------------------------------------------------------------------------------------------

instance NFData Comment where
  rnf (InlineComment  a) = seq a ()
  rnf (EndlineComment a) = seq a ()

instance NFData a => NFData (Com a) where
  rnf (Com         a  ) = deepseq a ()
  rnf (ComBefore a b  ) = deepseq a $! deepseq b ()
  rnf (ComAfter    a b) = deepseq a $! deepseq b ()
  rnf (ComAround a b c) = deepseq a $! deepseq b $! deepseq c ()

instance NFData UpdateOp where { rnf a = seq a () }
instance NFData PrefixOp where { rnf a = seq a () }
instance NFData InfixOp  where { rnf a = seq a () }
instance NFData CoreType where { rnf a = seq a () }
instance NFData GlobUnit where { rnf a = seq a () }

instance NFData LValueExpr where { rnf (LValueExpr a) = deepseq a () }
instance NFData AST_LValue where { rnf (AST_LValue a) = deepseq a () }

instance NFData AST_ObjList where { rnf (AST_ObjList a b c) = deepseq a $! deepseq b $! deepseq c () }
instance NFData ObjListExpr where { rnf (ObjListExpr a b) = deepseq a $! deepseq b () }

instance NFData RefExpr where { rnf (RefExpr a b) = deepseq a $! deepseq b () }
instance NFData AST_Ref where
  rnf  AST_RefNull    = ()
  rnf (AST_Ref a b c) = deepseq a $! deepseq b $! deepseq c ()

instance NFData QualRefExpr where
  rnf (UnqualRefExpr b  ) = deepseq b ()
  rnf (QualRefExpr a b c) = seq a $! deepseq b $! deepseq c ()
instance NFData AST_QualRef where
  rnf (AST_Unqualified   c  ) = deepseq c ()
  rnf (AST_Qualified a b c d) = seq a $! deepseq b $! deepseq c $! deepseq d ()

instance NFData AST_StringList where
  rnf (AST_NoStrings  a b) = deepseq a $! deepseq b ()
  rnf (AST_StringList a b) = deepseq a $! deepseq b ()

instance NFData AST_OptObjList where
  rnf (AST_NoObjList  a  ) = deepseq a ()
  rnf (AST_OptObjList a b) = deepseq a $! deepseq b ()

instance NFData AST_Paren where { rnf (AST_Paren a b) = deepseq a $! deepseq b () }

instance NFData AST_Object where
  rnf AST_Void = ()
  rnf (AST_ObjParen   a        ) = deepseq a ()
  rnf (AST_ObjQualRef a        ) = deepseq a ()
  rnf (AST_Literal    a b      ) = deepseq a $! deepseq b ()
  rnf (AST_Assign     a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Equation   a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Prefix     a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_ArraySub   a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_FuncCall   a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_Init       a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Struct     a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_Lambda     a b c    ) = deepseq a $! deepseq b $! deepseq c () 
  rnf (AST_Func       a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()
  rnf (AST_Rule       a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_MetaEval   a b      ) = deepseq a $! deepseq b ()

instance NFData Location where
  rnf LocationUnknown = ()
  rnf (Location a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

--instance NFData ElseIfExpr where
--  rnf  NullElseIfExpr      = ()
--  rnf (ElseExpr   a b    ) = deepseq a $! deepseq b ()
--  rnf (ElseIfExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

--instance NFData AST_ElseIf where
--  rnf  AST_NullElseIf      = ()
--  rnf (AST_Else   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
--  rnf (AST_ElseIf a b c d) = deepseq a $! deepseq b $! deepseq c ()

instance NFData AST_If     where { rnf (AST_If     a b c) = deepseq a $! deepseq b $! deepseq c () }
instance NFData IfExpr     where { rnf (IfExpr     a b c) = deepseq a $! deepseq b $! deepseq c () }
instance NFData AST_Else   where { rnf (AST_Else   a b c) = deepseq a $! deepseq b $! deepseq c () }
instance NFData ElseExpr   where { rnf (ElseExpr   a b  ) = deepseq a $! deepseq b $! () }
instance NFData AST_IfElse where
  rnf (AST_IfElse a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()
instance NFData IfElseExpr where
  rnf (IfElseExpr a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance NFData AST_While  where { rnf (AST_While (AST_If a b c)) = deepseq a $! deepseq b $! deepseq c () }
instance NFData WhileExpr  where { rnf (WhileExpr (IfExpr a b c)) = deepseq a $! deepseq b $! deepseq c () }

instance NFData AST_Script where
  rnf (AST_Comment      a      ) = deepseq a ()
  rnf (AST_IfThenElse   a      ) = deepseq a ()
  rnf (AST_WhileLoop    a      ) = deepseq a ()
  rnf (AST_EvalObject   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_TryCatch     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ForLoop      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ContinueExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ReturnExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_WithDoc      a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance NFData Reference where { rnf (Reference  a) = deepseq a () }

instance NFData QualRef where
  rnf (Unqualified b) = deepseq b ()
  rnf (Qualified a b) = seq a $! deepseq b ()
  rnf (ObjRef      b) = deepseq b ()

instance NFData Object where
  rnf  ONull         = ()
  rnf  OTrue         = ()
--rnf (OType      a) = deepseq a ()
  rnf (OInt       a) = deepseq a ()
--rnf (OWord      a) = deepseq a ()
--rnf (OLong      a) = deepseq a ()
--rnf (OFloat     a) = deepseq a ()
--rnf (ORatio     a) = deepseq a ()
--rnf (OComplex   a) = deepseq a ()
  rnf (OAbsTime      a) = deepseq a ()
  rnf (ORelTime  a) = deepseq a ()
  rnf (OChar      a) = deepseq a ()
  rnf (OString    a) = deepseq a ()
  rnf (ORef       a) = deepseq a ()
--rnf (OPair  (a,b)) = deepseq a $! deepseq b ()
  rnf (OList      a) = deepseq a ()
--rnf (OSet       a) = deepseq a ()
--rnf (OArray     a) = deepseq a ()
--rnf (ODict      a) = deepseq a ()
--rnf (OIntMap    a) = deepseq a ()
  rnf (OTree      a) = deepseq a ()
--rnf (OGlob      a) = deepseq a ()
--rnf (OScript    a) = deepseq a ()
  rnf (OBytes     a) = seq a ()

instance (NFData a, NFData b) => NFData (T.Tree a b) where
  rnf  T.Void            = ()
  rnf (T.Leaf       a  ) = deepseq a ()
  rnf (T.Branch       b) = deepseq b ()
  rnf (T.LeafBranch a b) = deepseq a $! deepseq b ()

instance NFData TypeSym where
  rnf (CoreType a  ) = deepseq a ()
  rnf (TypeVar  a b) = deepseq a $! deepseq b ()
instance NFData TypeStruct where { rnf (TypeStruct a) = deepseq a () }
instance NFData ObjType where { rnf (ObjType a) = deepseq a () }

instance NFData Glob          where { rnf (Glob          a b  ) = deepseq a $! deepseq b () }
instance NFData Subroutine    where { rnf (Subroutine    a _ _) = deepseq a () }
instance NFData CallableCode  where { rnf (CallableCode  a b _) = deepseq a $! deepseq b () }
instance NFData GlobAction    where { rnf (GlobAction    a b  ) = deepseq a $! deepseq b () }

--instance NFData ObjSetOp where { rnf a = seq a () }
instance NFData TopLevelEventType where { rnf a = seq a () }

instance NFData AST_SourceCode where
  rnf (AST_SourceCode a b c) = deepseq a $! deepseq b $! deepseq c ()

instance NFData AST_CodeBlock where { rnf (AST_CodeBlock a) = deepseq a () }

instance NFData a => NFData (TyChkExpr a) where
  rnf (NotTypeChecked  a) = deepseq a ()
  rnf (TypeChecked a b c) = deepseq a $! deepseq b $! deepseq c ()

instance NFData a => NFData (AST_TyChk a) where
  rnf (AST_NotChecked    a) = deepseq a ()
  rnf (AST_Checked a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance NFData ParamExpr where
  rnf (ParamExpr       a b c) = deepseq a $! deepseq b $! deepseq c ()
instance NFData AST_Param where
  rnf  AST_NoParams     = ()
  rnf (AST_Param a b c) = deepseq a $! deepseq b $! deepseq c ()

instance NFData ParamListExpr where { rnf (ParamListExpr a b) = deepseq a $! deepseq b () }
instance NFData AST_ParamList where { rnf (AST_ParamList a b) = deepseq a $! deepseq b () }

instance NFData AST_TopLevel where
  rnf (AST_Attribute  a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_TopScript  a b      ) = deepseq a $! deepseq b ()
  rnf (AST_Event      a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_TopComment a        ) = deepseq a ()

instance NFData TopLevelExpr where
  rnf (Attribute      a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (TopScript      a b    ) = deepseq a $! deepseq b ()
  rnf (EventExpr      a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance NFData CodeBlock where { rnf (CodeBlock a) = deepseq a () }

instance NFData ScriptExpr where
  rnf (IfThenElse   a      ) = deepseq a ()
  rnf (WhileLoop    a      ) = deepseq a ()
  rnf (EvalObject   a b    ) = deepseq a $! deepseq b ()
  rnf (TryCatch     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ForLoop      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ContinueExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ReturnExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (WithDoc      a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance NFData RuleStrings where { rnf (RuleStrings a b) = deepseq a $! deepseq b () }

instance NFData OptObjListExpr where { rnf (OptObjListExpr a) = deepseq a () }

instance NFData ParenExpr where { rnf (ParenExpr a b) = deepseq a $! deepseq b () }

instance NFData ObjectExpr where
  rnf (Literal        a b    ) = deepseq a $! deepseq b ()
  rnf (ObjParenExpr   a      ) = deepseq a ()
  rnf (ObjQualRefExpr a      ) = deepseq a ()
  rnf (AssignExpr     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (Equation       a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (PrefixExpr     a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ArraySubExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (FuncCall       a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (InitExpr       a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (StructExpr     a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (LambdaExpr     a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (FuncExpr       a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (RuleExpr       a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (MetaEvalExpr   a b    ) = deepseq a $! deepseq b ()

