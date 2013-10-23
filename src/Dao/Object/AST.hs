-- "src/Dao/Object/AST.hs"  the abstract syntax tree for the Dao
-- scripting language.
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Dao.Object.AST where

import           Dao.String
import           Dao.Token
import           Dao.Parser
import           Dao.Object

import           Control.Monad

import           Data.Typeable
import           Data.Monoid
import           Data.List
import Debug.Trace
----------------------------------------------------------------------------------------------------

data AST_Ref
  = AST_RefNull
  | AST_Ref  Name [Com Name] Location
  deriving (Eq, Ord, Typeable, Show)

data AST_QualRef
  = AST_Unqualified                      AST_Ref
  | AST_Qualified RefQualifier [Comment] AST_Ref Location
  deriving (Eq, Ord, Typeable, Show)

data AST_ObjList = AST_ObjList [Comment] [Com AST_Object] Location deriving (Eq, Ord, Typeable, Show)
instance Monoid AST_ObjList where
  mempty = AST_ObjList [] [] LocationUnknown
  mappend (AST_ObjList a1 a2 aloc) (AST_ObjList b1 b2 bloc) = AST_ObjList (a1++b1) (a2++b2) (aloc<>bloc)

setObjListPreComments :: [Comment] -> AST_ObjList -> AST_ObjList
setObjListPreComments coms (AST_ObjList _ a loc) = AST_ObjList coms a loc

mkObjList :: [Com AST_Object] -> AST_ObjList
mkObjList ox = AST_ObjList [] ox (mconcat $ fmap (getLocation . unComment) ox)

newtype AST_LValue = AST_LValue AST_Object deriving (Eq, Ord, Typeable, Show)
instance HasLocation AST_LValue where
  getLocation (AST_LValue o)     = getLocation o
  setLocation (AST_LValue o) loc = AST_LValue (setLocation o loc)
  delLocation (AST_LValue o)     = AST_LValue (delLocation o)

data AST_TyChk a
  = AST_NotChecked a
  | AST_Checked    a (Com ()) AST_Object Location
  deriving (Eq, Ord, Typeable, Show)

checkedAST :: AST_TyChk a -> a
checkedAST a = case a of { AST_NotChecked a -> a; AST_Checked a _ _ _ -> a; }

instance HasLocation a => HasLocation (AST_TyChk a) where
  getLocation a     = case a of
    AST_NotChecked a         -> getLocation a
    AST_Checked    a _ _ loc -> getLocation a <> loc
  setLocation a loc = case a of
    AST_NotChecked a         -> AST_NotChecked (setLocation a loc)
    AST_Checked    a b c _   -> AST_Checked a b c loc
  delLocation a     = case a of
    AST_NotChecked a         -> AST_NotChecked (delLocation a)
    AST_Checked    a b c _   -> AST_Checked (delLocation a) b (delLocation c) LocationUnknown

data AST_Param
  = AST_NoParams
  | AST_Param (Maybe [Comment]) (AST_TyChk Name) Location
  deriving (Eq, Ord, Typeable, Show)
instance HasLocation AST_Param where
  getLocation a     = case a of
    AST_NoParams      -> LocationUnknown
    AST_Param _ _ loc -> loc
  setLocation a loc = case a of
    AST_NoParams    -> AST_NoParams
    AST_Param a b _ -> AST_Param a b loc
  delLocation a     = case a of
    AST_NoParams    -> AST_NoParams
    AST_Param a b _ -> AST_Param a (delLocation b) LocationUnknown

data AST_ParamList
  = AST_ParamList (AST_TyChk [Com AST_Param]) Location
  deriving (Eq, Ord, Typeable, Show)

instance HasLocation AST_ParamList where
  getLocation (AST_ParamList _ loc)     = loc
  setLocation (AST_ParamList a _  ) loc = AST_ParamList a loc
  delLocation (AST_ParamList a _  )     = AST_ParamList a LocationUnknown

data AST_StringList
  = AST_NoStrings  [Comment]  Location
  | AST_StringList [Com UStr] Location
  deriving (Eq, Ord, Typeable, Show)

data AST_OptObjList
  = AST_NoObjList  [Comment]
  | AST_OptObjList AST_ObjList [Comment]
  deriving (Eq, Ord, Typeable, Show)
instance HasLocation AST_OptObjList where
  getLocation o     = case o of
    AST_NoObjList{}    -> LocationUnknown
    AST_OptObjList o _ -> getLocation o
  setLocation o loc = case o of
    AST_NoObjList{}    -> o
    AST_OptObjList o c -> AST_OptObjList (setLocation o loc) c
  delLocation o     = case o of
    AST_NoObjList{}    -> o
    AST_OptObjList o c -> AST_OptObjList (delLocation o) c

data AST_Paren = AST_Paren (Com AST_Object) Location deriving (Eq, Ord, Typeable, Show)
instance HasLocation AST_Paren where
  getLocation (AST_Paren o loc)     = loc
  setLocation (AST_Paren o _  ) loc = AST_Paren o loc
  delLocation (AST_Paren o _  )     = AST_Paren (delLocation o) LocationUnknown

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data AST_Object
  = AST_Void -- ^ Not a language construct, but used where an object expression is optional.
  | AST_ObjQualRef AST_QualRef
  | AST_ObjParen   AST_Paren
  | AST_Literal    Object                                              Location
  | AST_Assign     AST_LValue     (Com UpdateOp)         AST_Object    Location
  | AST_Equation   AST_Object     (Com InfixOp)          AST_Object    Location
  | AST_Prefix     PrefixOp                         (Com AST_Object)   Location
  | AST_ArraySub   AST_Object                            AST_ObjList   Location
  | AST_FuncCall   AST_Object                            AST_ObjList   Location
  | AST_Init       AST_Ref             AST_OptObjList    AST_ObjList   Location
  | AST_Struct                    (Com AST_Object)       AST_ObjList   Location
  | AST_Lambda                    (Com AST_ParamList)    AST_CodeBlock Location
  | AST_Func       [Comment] Name (Com AST_ParamList)    AST_CodeBlock Location
  | AST_Rule                      (Com AST_StringList)   AST_CodeBlock Location
  | AST_MetaEval                                         AST_CodeBlock Location
  deriving (Eq, Ord, Typeable, Show)

--data AST_ElseIf
--  = AST_NullElseIf
--  | AST_Else   [Comment]        AST_CodeBlock            Location
--    -- ^ else /**/ {}
--  | AST_ElseIf (Com AST_Object) AST_CodeBlock AST_ElseIf Location
--    -- ^ else /**/ if /**/ objExpr /**/ {}
--  deriving (Eq, Ord, Typeable, Show)

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data AST_Script
  = AST_Comment                   [Comment] 
  | AST_IfThenElse   AST_IfElse
  | AST_WhileLoop    AST_While
  | AST_EvalObject   AST_Object   [Comment]                                            Location
    -- ^ @some.object.expression = for.example - equations || function(calls) /**/ ;@
  | AST_TryCatch     (Com AST_CodeBlock)     (Maybe (Com Name)) (Maybe AST_CodeBlock)  Location
    -- ^ @try /**/ {} /**/ catch /**/ errVar /**/ {}@              
  | AST_ForLoop      (Com Name)              (Com AST_Paren)           AST_CodeBlock   Location
    -- ^ @for /**/ var /**/ in /**/ objExpr /**/ {}@
  | AST_ContinueExpr Bool  [Comment]         (Com AST_Object)                          Location
    -- ^ The boolean parameter is True for a "continue" statement, False for a "break" statement.
    -- @continue /**/ ;@ or @continue /**/ if /**/ objExpr /**/ ;@
  | AST_ReturnExpr   Bool                    (Com AST_Object)                          Location
    -- ^ The boolean parameter is True for a "return" statement, False for a "throw" statement.
    -- ^ @return /**/ ;@ or @return /**/ objExpr /**/ ;@
  | AST_WithDoc      (Com AST_Paren)         AST_CodeBlock                            Location
    -- ^ @with /**/ objExpr /**/ {}@
  deriving (Eq, Ord, Typeable, Show)

newtype AST_CodeBlock = AST_CodeBlock{ getAST_CodeBlock :: [AST_Script] } deriving (Eq, Ord, Typeable, Show)
  -- A code block is never standing on it's own, it is always part of a larger expression, so there
  -- is no 'Dao.Token.Location' parameter for 'AST_CodeBlock'.
instance Monoid AST_CodeBlock where
  mempty      = AST_CodeBlock []
  mappend a b = AST_CodeBlock (mappend (getAST_CodeBlock a) (getAST_CodeBlock b))

-- | A 'AST_TopLevel' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data AST_TopLevel
  = AST_Attribute  Name              (Com AST_Object)                  Location
  | AST_TopScript  AST_Script                                          Location
  | AST_Event      TopLevelEventType [Comment]           AST_CodeBlock Location
  | AST_TopComment [Comment]
  deriving (Eq, Ord, Typeable, Show)

-- | A 'SourceCode' is the structure loaded from source code. An 'ExecUnit' object is constructed from
-- 'SourceCode'.
data AST_SourceCode
  = AST_SourceCode
    { sourceModified :: Int
    , sourceFullPath :: UStr
      -- ^ the URL (full file path) from where this source code was received.
    , directives     :: [AST_TopLevel]
    }
  deriving (Eq, Ord, Typeable)

----------------------------------------------------------------------------------------------------

lu  = LocationUnknown
fd :: HasLocation a => a -> a
fd = delLocation
fd1 :: (HasLocation a, Functor f) => f a -> f a
fd1 = fmap delLocation
--fd2 :: (HasLocation a, Functor f, Functor g) => f (g a) -> f (g a)
--fd2 = fmap (fmap delLocation)
--fd3 :: (HasLocation a, Functor f, Functor g, Functor h) => f (g (h a)) -> f (g (h a))
--fd3 = fmap (fmap (fmap delLocation))

instance HasLocation AST_Ref where
  getLocation o     = case o of
    AST_RefNull     -> LocationUnknown
    AST_Ref _ _ loc -> loc
  setLocation o loc = case o of
    AST_RefNull   -> AST_RefNull
    AST_Ref a b _ -> AST_Ref a b loc
  delLocation o     = case o of
    AST_RefNull   -> AST_RefNull
    AST_Ref a b _ -> AST_Ref a b lu

instance HasLocation AST_QualRef where
  getLocation o     = case o of
    AST_Unqualified   r     -> getLocation r
    AST_Qualified _ _ _ loc -> loc
  setLocation o loc = case o of
    AST_Unqualified   r     -> AST_Unqualified (setLocation r loc)
    AST_Qualified q c r _   -> AST_Qualified q c r loc
  delLocation o     = case o of
    AST_Unqualified   r     -> AST_Unqualified   (fd r)
    AST_Qualified q c r _   -> AST_Qualified q c (fd r) LocationUnknown

instance HasLocation AST_ObjList where
  getLocation (AST_ObjList _ _ loc)     = loc
  setLocation (AST_ObjList a b _  ) loc = AST_ObjList a      b  loc
  delLocation (AST_ObjList a b _  )     = AST_ObjList a (fd1 b) lu

instance HasLocation AST_StringList where
  getLocation o     = case o of
    AST_NoStrings  _ o -> o
    AST_StringList _ o -> o
  setLocation o loc = case o of
    AST_NoStrings  a _ -> AST_NoStrings  a loc
    AST_StringList a _ -> AST_StringList a loc
  delLocation o     = case o of
    AST_NoStrings  a _ -> AST_NoStrings  a LocationUnknown
    AST_StringList a _ -> AST_StringList a LocationUnknown

instance HasLocation AST_Object where
  getLocation o = case o of
    AST_Void               -> LocationUnknown
    AST_ObjQualRef       o -> getLocation o
    AST_ObjParen         o -> getLocation o
    AST_Literal  _       o -> o
    AST_Assign   _ _ _   o -> o
    AST_Equation _ _ _   o -> o
    AST_Prefix   _ _     o -> o
    AST_ArraySub _ _     o -> o
    AST_FuncCall _ _     o -> o
    AST_Init     _ _ _   o -> o
    AST_Struct   _ _     o -> o
    AST_Lambda   _ _     o -> o
    AST_Func     _ _ _ _ o -> o
    AST_Rule     _ _     o -> o
    AST_MetaEval _       o -> o
  setLocation o loc = case o of
    AST_Void                 -> AST_Void
    AST_ObjQualRef a         -> AST_ObjQualRef (setLocation a loc)
    AST_ObjParen   a         -> AST_ObjParen   (setLocation a loc)
    AST_Literal    a       _ -> AST_Literal    a       loc
    AST_Assign     a b c   _ -> AST_Assign     a b c   loc
    AST_Equation   a b c   _ -> AST_Equation   a b c   loc
    AST_Prefix     a b     _ -> AST_Prefix     a b     loc
    AST_ArraySub   a b     _ -> AST_ArraySub   a b     loc
    AST_FuncCall   a b     _ -> AST_FuncCall   a b     loc
    AST_Init       a b c   _ -> AST_Init       a b c   loc
    AST_Struct     a b     _ -> AST_Struct     a b     loc
    AST_Lambda     a b     _ -> AST_Lambda     a b     loc
    AST_Func       a b c d _ -> AST_Func       a b c d loc
    AST_Rule       a b     _ -> AST_Rule       a b     loc
    AST_MetaEval   a       _ -> AST_MetaEval   a       loc
  delLocation o = case o of                            
    AST_Void                 -> AST_Void                 
    AST_ObjQualRef a         -> AST_ObjQualRef (fd  a)
    AST_ObjParen   a         -> AST_ObjParen   (fd  a)
    AST_Literal    a       _ -> AST_Literal         a                          lu
    AST_Assign     a b c   _ -> AST_Assign     (fd  a)      b  (fd  c)         lu
    AST_Equation   a b c   _ -> AST_Equation   (fd  a)      b  (fd  c)         lu
    AST_Prefix     a b     _ -> AST_Prefix          a  (fd  b)                 lu
    AST_ArraySub   a b     _ -> AST_ArraySub   (fd  a) (fd  b)                 lu
    AST_FuncCall   a b     _ -> AST_FuncCall   (fd  a) (fd  b)                 lu
    AST_Init       a b c   _ -> AST_Init       (fd  a) (fd  b) (fd  c)         lu
    AST_Struct     a b     _ -> AST_Struct     (fd  a) (fd  b)                 lu
    AST_Lambda     a b     _ -> AST_Lambda          a  (fd  b)                 lu
    AST_Func       a b c d _ -> AST_Func            a       b  (fd  c) (fd  d) lu
    AST_Rule       a b     _ -> AST_Rule            a  (fd  b)                 lu
    AST_MetaEval   a       _ -> AST_MetaEval   (fd  a)                         lu
                                                                              
instance HasLocation AST_CodeBlock where                                      
  getLocation o = case getAST_CodeBlock o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = AST_CodeBlock (fmap delLocation (getAST_CodeBlock o))

data AST_If     = AST_If (Com AST_Paren) AST_CodeBlock Location deriving (Eq, Ord, Typeable, Show)
data AST_Else   = AST_Else (Com ()) AST_If Location deriving (Eq, Ord, Typeable, Show)
  -- ^ @/**/ else /**/ if /**/ obj /**/ {}@
data AST_IfElse = AST_IfElse AST_If [AST_Else] (Com ()) (Maybe AST_CodeBlock) Location
  -- ^ @if /**/ obj /**/ {} /**/ else /**/ if /**/ obj /**/ {} /**/ else {}@
  deriving (Eq, Ord, Typeable, Show)

instance HasLocation AST_If where
  getLocation (AST_If _ _ loc)     = loc
  setLocation (AST_If a b _  ) loc = AST_If a b loc
  delLocation (AST_If a b _  )     = AST_If (delLocation a) (delLocation b) LocationUnknown
instance HasLocation AST_IfElse where
  getLocation (AST_IfElse _ _ _ _ loc)     = loc
  setLocation (AST_IfElse a b c d _  ) loc = AST_IfElse a b c d loc
  delLocation (AST_IfElse a b c d _  )     = AST_IfElse a b c d LocationUnknown
instance HasLocation AST_While where
  getLocation (AST_While a) = getLocation a
  setLocation (AST_While a) loc = AST_While (setLocation a loc)
  delLocation (AST_While a)     = AST_While (delLocation a)
newtype AST_While = AST_While AST_If deriving (Eq, Ord, Typeable, Show)

--instance HasLocation AST_ElseIf where
--  getLocation o     = case o of
--    AST_NullElseIf       -> LocationUnknown
--    AST_Else   _ _   loc -> loc
--    AST_ElseIf _ _ _ loc -> loc
--  setLocation o loc = case o of
--    AST_NullElseIf       -> AST_NullElseIf
--    AST_Else   a b   _   -> AST_Else   a b   loc
--    AST_ElseIf a b c _   -> AST_ElseIf a b c loc
--  delLocation o     = case o of
--    AST_NullElseIf     -> AST_NullElseIf
--    AST_Else   a b   _ -> AST_Else   a (fd  b)        lu
--    AST_ElseIf a b c _ -> AST_ElseIf a (fd  b) (fd c) lu

instance HasLocation AST_Script where
  getLocation o = case o of
    AST_Comment      _       -> lu
    AST_EvalObject   _   _ o -> o
    AST_IfThenElse         o -> getLocation o
    AST_WhileLoop          o -> getLocation o
    AST_TryCatch     _ _ _ o -> o
    AST_ForLoop      _ _ _ o -> o
    AST_ContinueExpr _ _ _ o -> o
    AST_ReturnExpr   _ _   o -> o
    AST_WithDoc      _ _   o -> o
  setLocation o loc = case o of
    AST_Comment      a       -> AST_Comment      a
    AST_EvalObject   a b   _ -> AST_EvalObject   a b   loc
    AST_IfThenElse   a       -> AST_IfThenElse   (setLocation a loc)
    AST_WhileLoop    a       -> AST_WhileLoop    (setLocation a loc)
    AST_TryCatch     a b c _ -> AST_TryCatch     a b c loc
    AST_ForLoop      a b c _ -> AST_ForLoop      a b c loc
    AST_ContinueExpr a b c _ -> AST_ContinueExpr a b c loc
    AST_ReturnExpr   a b   _ -> AST_ReturnExpr   a b   loc
    AST_WithDoc      a b   _ -> AST_WithDoc      a b   loc
  delLocation o = case o of
    AST_Comment      a       -> AST_Comment           a
    AST_EvalObject   a b   _ -> AST_EvalObject   (fd  a)      b              lu
    AST_IfThenElse   a       -> AST_IfThenElse   (fd  a)
    AST_WhileLoop    a       -> AST_WhileLoop    (fd  a)
    AST_TryCatch     a b c _ -> AST_TryCatch     (fd1 a)      b  (fmap fd c) lu
    AST_ForLoop      a b c _ -> AST_ForLoop           a  (fd1 b) (fd  c)     lu
    AST_ContinueExpr a b c _ -> AST_ContinueExpr      a       b  (fd1 c)     lu
    AST_ReturnExpr   a b   _ -> AST_ReturnExpr        a  (fd1 b)             lu
    AST_WithDoc      a b   _ -> AST_WithDoc      (fd1 a) (fd  b)             lu

instance HasLocation AST_TopLevel where
  getLocation o = case o of
    AST_Attribute  _ _     o -> o
    AST_TopScript  _       o -> o
    AST_Event      _ _ _   o -> o
    AST_TopComment _         -> lu
  setLocation o loc = case o of
    AST_Attribute  a b     _ -> AST_Attribute  a b     loc
    AST_TopScript  a       _ -> AST_TopScript  a       loc
    AST_Event      a b c   _ -> AST_Event      a b c   loc
    AST_TopComment a         -> AST_TopComment a
  delLocation o = case o of
    AST_Attribute  a b     _ -> AST_Attribute       a (fd1 b)                 lu
    AST_TopScript  a       _ -> AST_TopScript (fd   a)                        lu
    AST_Event      a b c   _ -> AST_Event           a      b  (fd  c)         lu
    AST_TopComment a         -> AST_TopComment      a

----------------------------------------------------------------------------------------------------

isAST_Attribute :: AST_TopLevel -> Bool
isAST_Attribute o = case o of { AST_Attribute _ _ _ -> True; _ -> False; }

attributeToList :: AST_TopLevel -> [(Name, Com AST_Object, Location)]
attributeToList o = case o of { AST_Attribute a b c -> return (a,b,c); _ -> mzero; }

----------------------------------------------------------------------------------------------------

-- | Comments in the Dao language are not interpreted, but they are not disgarded either. Dao is
-- intended to manipulate natural language, and itself, so that it can "learn" new semantic
-- structures. Dao scripts can manipulate the syntax tree of other Dao scripts, and so it might be
-- helpful if the syntax tree included comments.
data Comment
  = InlineComment  UStr
  | EndlineComment UStr
  deriving (Eq, Ord, Typeable, Show)

instance HasLocation a => HasLocation (Com a) where
  getLocation = getLocation . unComment
  setLocation com loc = fmap (\a -> setLocation a loc) com
  delLocation = fmap delLocation

commentString :: Comment -> UStr
commentString com = case com of
  InlineComment  a -> a
  EndlineComment a -> a

-- | Symbols in the Dao syntax tree that can actually be manipulated can be surrounded by comments.
-- The 'Com' structure represents a space-efficient means to surround each syntactic element with
-- comments that can be ignored without disgarding them.
data Com a = Com a | ComBefore [Comment] a | ComAfter a [Comment] | ComAround [Comment] a [Comment]
  deriving (Eq, Ord, Typeable, Show)

appendComments :: Com a -> [Comment] -> Com a
appendComments com cx = case com of
  Com          a    -> ComAfter     a cx
  ComAfter     a ax -> ComAfter     a (ax++cx)
  ComBefore ax a    -> ComAround ax a cx
  ComAround ax a bx -> ComAround ax a (bx++cx)

com :: [Comment] -> a -> [Comment] -> Com a
com before a after = case before of
  [] -> case after of
    [] -> Com a
    dx -> ComAfter a dx
  cx -> case after of
    [] -> ComBefore cx a
    dx -> ComAround cx a dx

setCommentBefore :: [Comment] -> Com a -> Com a
setCommentBefore cx com = case com of
  Com         a    -> ComBefore cx a
  ComBefore _ a    -> ComBefore cx a
  ComAfter    a dx -> ComAround cx a dx
  ComAround _ a dx -> ComAround cx a dx

setCommentAfter :: [Comment] -> Com a -> Com a
setCommentAfter cx com = case com of
  Com          a   -> ComAfter     a cx
  ComBefore dx a   -> ComAround dx a cx
  ComAfter     a _ -> ComAfter     a cx
  ComAround dx a _ -> ComAround dx a cx

unComment :: Com a -> a
unComment com = case com of
  Com         a   -> a
  ComBefore _ a   -> a
  ComAfter    a _ -> a
  ComAround _ a _ -> a

getComment :: Com a -> ([Comment], [Comment])
getComment com = case com of
  Com         _   -> ([], [])
  ComBefore a _   -> (a, [])
  ComAfter    _ b -> ([], b)
  ComAround a _ b -> (a, b)

instance Functor Com where
  fmap fn c = case c of
    Com          a    -> Com          (fn a)
    ComBefore c1 a    -> ComBefore c1 (fn a)
    ComAfter     a c2 -> ComAfter     (fn a) c2
    ComAround c1 a c2 -> ComAround c1 (fn a) c2

----------------------------------------------------------------------------------------------------

-- | Elements of the abstract syntax tree all have a one-to-one conversion mapping to elements in
-- the "Dao.Object"  module. The 'Intermediate' class allows us to declare that relationship between
-- these types, and also define functions for converting and de-converting between these types. For
-- example, 'Dao.Object.ObjectExpr' is the intermediate representation of 'AST_Object', so our
-- instance for this relationship is @instane 'Intermediate' 'Dao.Object.ObjectExpr' 'AST_Object'@.
class Intermediate obj ast | obj -> ast, ast -> obj where
  toInterm   :: ast -> [obj]
  fromInterm :: obj -> [ast]
  -- | The default implementation is to convert an @ast@ to an @[obj]@ using 'toInterm' and then
  -- immediately convert the @[obj]@ back to an @[ast]@ using 'fromInterm'.
  canonicalize :: ast -> [ast]
  canonicalize ast = toInterm ast >>= fromInterm

-- Not for export: here are a bunch of shortcuts to converting the AST to the intermediate data
-- type. Sinec 'toInterm' returns a single item in a list to indicate success and an empty list to
-- indicate failure, all of these items have their evaluated type wrapped in a list type. This is to
-- allow the 'toInterm' instances use the 'Control.Monad.liftM' family of functions.
ti :: Intermediate obj ast => ast -> [obj]
ti = toInterm
uc :: Com a -> [a]
uc = return . unComment
uc0 :: Intermediate obj ast =>  Com ast  -> [obj]
uc0 = toInterm . unComment
uc1 :: Intermediate obj ast => [Com ast] -> [[obj]]
uc1 = return . concatMap (toInterm . unComment)
uc2 :: Intermediate obj ast => Com [Com ast] -> [[obj]]
uc2 = uc1 . unComment
um0 :: Maybe (Com a) -> [Maybe a]
um0 = maybe [Nothing] (return . Just . unComment)
um1 :: Intermediate obj ast => Maybe ast -> [Maybe obj]
um1 = maybe [Nothing] (fmap Just . toInterm)
um2 :: Intermediate obj ast => Maybe (Com ast) -> [Maybe obj]
um2 = maybe [Nothing] (fmap Just . toInterm . unComment)

fi :: Intermediate obj ast => obj -> [ast]
fi = fromInterm
nc :: a -> [Com a]
nc = return . Com
nc0 :: Intermediate obj ast => obj -> [Com ast]
nc0 = fmap Com . fromInterm
nc1 :: Intermediate obj ast => [obj] -> [[Com ast]]
nc1 = return . map Com . concatMap fromInterm
nc2 :: Intermediate obj ast => [obj] -> [Com [Com ast]]
nc2 = fmap Com . nc1
nm0 :: Maybe a -> [Maybe (Com a)]
nm0 = maybe [Nothing] (return . Just . Com)
nm1 :: Intermediate obj ast => Maybe obj -> [Maybe ast]
nm1 = maybe [Nothing] (fmap Just . fromInterm)
nm2 :: Intermediate obj ast => Maybe obj -> [Maybe (Com ast)]
nm2 = maybe [Nothing] (fmap (Just . Com) . fromInterm)

ll :: Location -> [Location]
ll = return

instance Intermediate RefExpr AST_Ref where
  toInterm   ast = case ast of
    AST_RefNull             -> [RefExpr (Reference []) LocationUnknown]
    AST_Ref  n nx  loc -> [RefExpr (Reference (n : fmap unComment nx)) loc]
  fromInterm (RefExpr (Reference nx) loc) = case nx of
    []   -> [AST_RefNull]
    n:nx -> [AST_Ref n (fmap Com nx) LocationUnknown]

instance Intermediate QualRefExpr AST_QualRef where
  toInterm   ast = case ast of
    AST_Unqualified   r     -> liftM  UnqualRefExpr   (ti r)
    AST_Qualified q _ r loc -> liftM3 QualRefExpr [q] (ti r) [loc]
  fromInterm obj = case obj of
    UnqualRefExpr r     -> liftM  AST_Unqualified        (fi r)
    QualRefExpr q r loc -> liftM4 AST_Qualified [q] [[]] (fi r) [loc]

instance Intermediate a b => Intermediate (TyChkExpr a) (AST_TyChk b) where
  toInterm   a = case a of
    AST_NotChecked a         -> liftM  NotTypeChecked (ti a)
    AST_Checked    a _ b loc -> liftM3 TypeChecked (ti a) (ti b) [loc]
  fromInterm a = case a of
    NotTypeChecked a         -> liftM  AST_NotChecked (fi a)
    TypeChecked    a b   loc -> liftM4 AST_Checked (fi a) [Com ()] (fi b) [loc]
    DisableCheck   a b _ loc -> liftM4 AST_Checked (fi a) [Com ()] (fi b) [loc]

instance Intermediate Name Name where { toInterm = return; fromInterm = return; }

instance Intermediate ObjListExpr AST_ObjList where
  toInterm   (AST_ObjList _ lst loc) = liftM2 ObjListExpr      [lst>>=uc0] [loc]
  fromInterm (ObjListExpr   lst loc) = liftM3 AST_ObjList [[]] [lst>>=nc0] [loc]

instance Intermediate LValueExpr AST_LValue where
  toInterm   (AST_LValue o) = liftM LValueExpr (ti o)
  fromInterm (LValueExpr o) = liftM AST_LValue (fi o)

instance Intermediate ParamExpr AST_Param where
  toInterm   a = case a of
    AST_NoParams      -> []
    AST_Param a b loc -> liftM3 ParamExpr [maybe False (const True) a] (ti b) [loc]
  fromInterm o = case o of
    ParamExpr a b loc -> liftM3 AST_Param [if a then Just [] else Nothing] (fi b) [loc]

instance Intermediate [ParamExpr] [Com AST_Param] where
  toInterm   ax = [ax >>= toInterm . unComment]
  fromInterm ax = [ax >>= fmap Com . fromInterm]

instance Intermediate ParamListExpr AST_ParamList where
  toInterm   (AST_ParamList ox loc) = liftM2 ParamListExpr (ti ox) [loc]
  fromInterm (ParamListExpr ox loc) = liftM2 AST_ParamList (fi ox) [loc]

instance Intermediate RuleStrings AST_StringList where
  toInterm   o = case o of
    AST_NoStrings  _ loc -> liftM2 RuleStrings [[]] [loc]
    AST_StringList o loc -> liftM2 RuleStrings [fmap unComment o] [loc]
  fromInterm o = case o of
    RuleStrings [] loc -> liftM2 AST_NoStrings  [[]]         [loc]
    RuleStrings o  loc -> liftM2 AST_StringList [fmap Com o] [loc]

instance Intermediate OptObjListExpr AST_OptObjList where
  toInterm   o = case o of
    AST_NoObjList{}    -> [OptObjListExpr Nothing]
    AST_OptObjList o _ -> fmap (OptObjListExpr . Just) (ti o)
  fromInterm o = case o of
    OptObjListExpr o -> maybe [AST_NoObjList []] (fmap (flip AST_OptObjList []) . fi) o

instance Intermediate ParenExpr AST_Paren where
  toInterm   (AST_Paren o loc) = liftM2 ParenExpr (uc0 o) [loc]
  fromInterm (ParenExpr o loc) = liftM2 AST_Paren (nc0 o) [loc]

instance Intermediate ObjectExpr AST_Object where
  toInterm   ast = case ast of
    AST_Void                 -> return VoidExpr
    AST_ObjQualRef a         -> liftM  ObjQualRefExpr (ti  a)
    AST_ObjParen   a         -> liftM  ObjParenExpr   (ti  a)
    AST_Literal    a     loc -> liftM2 Literal            [a]                 [loc]
    AST_Assign     a b c loc -> liftM4 AssignExpr     (ti  a) (uc  b) (ti  c) [loc]
    AST_Equation   a b c loc -> liftM4 Equation       (ti  a) (uc  b) (ti  c) [loc]
    AST_Prefix     a b   loc -> liftM3 PrefixExpr     [a]     (uc0 b)         [loc]
    AST_ArraySub   a b   loc -> liftM3 ArraySubExpr   (ti  a) (ti  b)         [loc]
    AST_FuncCall   a b   loc -> liftM3 FuncCall       (ti  a) (ti  b)         [loc]
    AST_Init       a b c loc -> liftM4 InitExpr       (ti  a) (ti  b) (ti  c) [loc]
    AST_Struct     a b   loc -> liftM3 StructExpr     (uc0 a)         (ti  b) [loc]
    AST_Lambda     a b   loc -> liftM3 LambdaExpr     (uc0 a)         (ti  b) [loc]
    AST_Func     _ a b c loc -> liftM4 FuncExpr       [a]     (uc0 b) (ti  c) [loc]
    AST_Rule       a b   loc -> liftM3 RuleExpr               (uc0 a) (ti  b) [loc]
    AST_MetaEval   a     loc -> liftM2 MetaEvalExpr                   (ti  a) [loc]
  fromInterm obj = case obj of
    VoidExpr                 -> return AST_Void
    ObjQualRefExpr a         -> liftM  AST_ObjQualRef (fi  a)
    ObjParenExpr   a         -> liftM  AST_ObjParen   (fi  a)
    Literal        a     loc -> liftM2 AST_Literal        [a]                 [loc]
    AssignExpr     a b c loc -> liftM4 AST_Assign     (fi  a) (nc  b) (fi  c) [loc]
    Equation       a b c loc -> liftM4 AST_Equation   (fi  a) (nc  b) (fi  c) [loc]
    PrefixExpr     a b   loc -> liftM3 AST_Prefix     [a]             (nc0 b) [loc]
    ArraySubExpr   a b   loc -> liftM3 AST_ArraySub   (fi  a) (fi  b)         [loc]
    FuncCall       a b   loc -> liftM3 AST_FuncCall   (fi  a) (fi  b)         [loc]
    InitExpr       a b c loc -> liftM4 AST_Init       (fi  a) (fi  b) (fi  c) [loc]
    StructExpr     a b   loc -> liftM3 AST_Struct     (nc0 a)         (fi  b) [loc]
    LambdaExpr     a b   loc -> liftM3 AST_Lambda             (nc0 a) (fi  b) [loc]
    FuncExpr       a b c loc -> liftM5 AST_Func   [[]]    [a] (nc0 b) (fi  c) [loc]
    RuleExpr       a b   loc -> liftM3 AST_Rule               (nc0 a) (fi  b) [loc]
    MetaEvalExpr   a     loc -> liftM2 AST_MetaEval   (fi  a)                 [loc]

instance Intermediate CodeBlock AST_CodeBlock where
  toInterm   (AST_CodeBlock ast) = return $ CodeBlock     (ast >>= toInterm  )
  fromInterm (CodeBlock     obj) = return $ AST_CodeBlock (obj >>= fromInterm)

--instance Intermediate ElseIfExpr AST_ElseIf where
--  toInterm   ast = case ast of
--    AST_NullElseIf       -> [NullElseIfExpr]
--    AST_Else   _ b   loc -> liftM2 ElseExpr           (ti  b)         [loc]
--    AST_ElseIf a b c loc -> liftM4 ElseIfExpr (uc0 a) (ti  b) (ti  c) [loc]
--  fromInterm obj = case obj of
--    NullElseIfExpr         -> [AST_NullElseIf]
--    ElseExpr       b   loc -> liftM3 AST_Else   [[]]    (fi  b)         [loc]
--    ElseIfExpr   a b c loc -> liftM4 AST_ElseIf (nc0 a) (fi  b) (fi  c) [loc]

instance Intermediate IfExpr AST_If where
  toInterm   (AST_If a b loc) = liftM3 IfExpr (uc0 a) (ti  b) [loc]
  fromInterm (IfExpr a b loc) = liftM3 AST_If (nc0 a) (fi  b) [loc]

instance Intermediate ElseExpr AST_Else where
  toInterm   (AST_Else _ a loc) = liftM2 ElseExpr          (ti  a) [loc]
  fromInterm (ElseExpr   a loc) = liftM3 AST_Else [Com ()] (fi  a) [loc]

instance Intermediate IfElseExpr AST_IfElse where
  toInterm   (AST_IfElse a b _ c loc) = liftM4 IfElseExpr (ti  a) [b>>=ti]          (um1 c) [loc]
  fromInterm (IfElseExpr a b   c loc) = liftM5 AST_IfElse (fi  a) [b>>=fi] [Com ()] (nm1 c) [loc]

instance Intermediate WhileExpr AST_While where
  toInterm   (AST_While a) = liftM WhileExpr (ti a)
  fromInterm (WhileExpr a) = liftM AST_While (fi a)

instance Intermediate ScriptExpr AST_Script where
  toInterm   ast = case ast of
    AST_Comment      _         -> mzero
    AST_EvalObject   a _   loc -> liftM2 EvalObject   (ti  a)                 [loc]
    AST_IfThenElse   a         -> liftM  IfThenElse   (ti  a)
    AST_WhileLoop    a         -> liftM  WhileLoop    (ti  a)
    AST_TryCatch     a b c loc -> liftM4 TryCatch     (uc0 a) (um0 b) (um1 c) [loc]
    AST_ForLoop      a b c loc -> liftM4 ForLoop      (uc  a) (uc0 b) (ti  c) [loc]
    AST_ContinueExpr a _ c loc -> liftM3 ContinueExpr [a]             (uc0 c) [loc]
    AST_ReturnExpr   a b   loc -> liftM3 ReturnExpr   [a]     (uc0 b)         [loc]
    AST_WithDoc      a b   loc -> liftM3 WithDoc      (uc0 a) (ti  b)         [loc]
  fromInterm obj = case obj of
    EvalObject   a     loc -> liftM3 AST_EvalObject   (fi  a) [[]]            [loc]
    IfThenElse   a         -> liftM  AST_IfThenElse   (fi  a)
    WhileLoop    a         -> liftM  AST_WhileLoop    (fi  a)
    TryCatch     a b c loc -> liftM4 AST_TryCatch     (nc0 a) (nm0 b) (nm1 c) [loc]
    ForLoop      a b c loc -> liftM4 AST_ForLoop      (nc  a) (nc0 b) (fi  c) [loc]
    ContinueExpr a b   loc -> liftM4 AST_ContinueExpr [a]     [[]]    (nc0 b) [loc]
    ReturnExpr   a b   loc -> liftM3 AST_ReturnExpr   [a]     (nc0 b)         [loc]
    WithDoc      a b   loc -> liftM3 AST_WithDoc      (nc0 a) (fi  b)         [loc]

instance Intermediate TopLevelExpr AST_TopLevel where
  toInterm   ast = case ast of
    AST_Attribute  a b   loc -> liftM3 Attribute     [a]    (uc0 b)        (ll loc)
    AST_TopScript  a     loc -> liftM2 TopScript     (ti a)                (ll loc)
    AST_Event      a _ b loc -> liftM3 EventExpr     [a]    (ti  b)        (ll loc)
    AST_TopComment a         -> mzero
  fromInterm obj = case obj of
    Attribute a b loc -> liftM3 AST_Attribute [a]         (nc0 b)         [loc]
    TopScript a   loc -> liftM2 AST_TopScript (fi a)                      [loc]
    EventExpr a b loc -> liftM4 AST_Event     [a]    [[]] (fi  b)         [loc]

instance Intermediate Program AST_SourceCode where
  toInterm   ast = return $ Program (directives ast >>= toInterm)
  fromInterm obj = return $
    AST_SourceCode
    { sourceModified = 0
    , sourceFullPath = nil
    , directives     = topLevelExprs obj >>= fromInterm
    }

