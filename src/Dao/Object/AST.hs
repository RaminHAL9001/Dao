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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Dao.Object.AST where

import           Dao.String
import           Dao.Token
import           Dao.Parser
import           Dao.Object

import           Control.Monad

import           Data.Typeable
import           Data.List
import Debug.Trace
----------------------------------------------------------------------------------------------------

-- | A 'AST_TopLevel' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data AST_TopLevel
  = AST_Attribute  Name              (Com AST_Object)                    Location
  | AST_TopFunc    [Comment]   Name  (Com [Com AST_Object]) [AST_Script] Location
  | AST_TopScript  AST_Script                                            Location
  | AST_TopLambda  LambdaExprType    (Com [Com AST_Object]) [AST_Script] Location
  | AST_Event      TopLevelEventType [Comment]              [AST_Script] Location
  | AST_TopComment [Comment]
  deriving (Eq, Ord, Show, Typeable)

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data AST_Script
  = AST_Comment                   [Comment]
  | AST_EvalObject   AST_Object   [Comment]                                         Location
  | AST_IfThenElse   [Comment]    AST_Object  (Com [AST_Script]) (Com [AST_Script]) Location
    -- ^ @if /**/ objExpr /**/ {} /**/ else /**/ if /**/ {} /**/ else /**/ {} /**/@
  | AST_TryCatch     (Com [AST_Script])       (Com UStr)              [AST_Script]  Location
    -- ^ @try /**/ {} /**/ catch /**/ errVar /**/ {}@              
  | AST_ForLoop      (Com Name)               (Com AST_Object)        [AST_Script]  Location
    -- ^ @for /**/ var /**/ in /**/ objExpr /**/ {}@
  | AST_WhileLoop    (Com AST_Object)                                 [AST_Script]  Location
    -- ^ @while objExpr {}@
  | AST_ContinueExpr Bool  [Comment]          (Com AST_Object)                      Location
    -- ^ The boolean parameter is True for a "continue" statement, False for a "break" statement.
    -- @continue /**/ ;@ or @continue /**/ if /**/ objExpr /**/ ;@
  | AST_ReturnExpr   Bool                     (Com AST_Object)                      Location
    -- ^ The boolean parameter is True for a "return" statement, False for a "throw" statement.
    -- ^ @return /**/ ;@ or @return /**/ objExpr /**/ ;@
  | AST_WithDoc      (Com AST_Object)         [AST_Script]                          Location
    -- ^ @with /**/ objExpr /**/ {}@
  deriving (Eq, Ord, Show, Typeable)

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data AST_Object
  = AST_Void -- ^ Not a language construct, but used where an object expression is optional.
  | AST_Literal  Object                                                 Location
  | AST_Assign   AST_Object          (Com UpdateOp)         AST_Object  Location
  | AST_Equation AST_Object          (Com ArithOp2)         AST_Object  Location
  | AST_Prefix   ArithOp1                              (Com AST_Object) Location
  | AST_Paren                                          (Com AST_Object) Location
  | AST_ArraySub AST_Object          [Comment]         [Com AST_Object] Location
  | AST_FuncCall AST_Object          [Comment]         [Com AST_Object] Location
  | AST_Dict     Name                [Comment]         [Com AST_Object] Location
  | AST_Array                   (Com [Com AST_Object]) [Com AST_Object] Location
  | AST_Struct                       (Com AST_Object)  [Com AST_Object] Location
  | AST_Data                         [Comment]         [Com UStr]       Location
  | AST_Lambda   LambdaExprType (Com [Com AST_Object]) [AST_Script]     Location
  | AST_MetaEval                     (Com AST_Object)                   Location
  deriving (Eq, Ord, Show, Typeable)

-- | A 'SourceCode' is the structure loaded from source code. An 'ExecUnit' object is constructed from
-- 'SourceCode'.
data AST_SourceCode
  = AST_SourceCode
    { sourceModified   :: Int
    , sourceFullPath   :: UStr
      -- ^ the URL (full file path) from where this source code was received.
    , directives       :: [AST_TopLevel]
    }
  deriving (Eq, Ord, Show, Typeable)

----------------------------------------------------------------------------------------------------

lu  = LocationUnknown
fd0 :: HasLocation a => a -> a
fd0 = delLocation
fd1 :: (HasLocation a, Functor f) => f a -> f a
fd1 = fmap delLocation
fd2 :: (HasLocation a, Functor f, Functor g) => f (g a) -> f (g a)
fd2 = fmap (fmap delLocation)
fd3 :: (HasLocation a, Functor f, Functor g, Functor h) => f (g (h a)) -> f (g (h a))
fd3 = fmap (fmap (fmap delLocation))

instance HasLocation AST_TopLevel where
  getLocation o = case o of
    AST_Attribute  _ _     o -> o
    AST_TopFunc    _ _ _ _ o -> o
    AST_TopScript  _       o -> o
    AST_TopLambda  _ _ _   o -> o
    AST_Event      _ _ _   o -> o
    AST_TopComment _         -> LocationUnknown
  setLocation o loc = case o of
    AST_Attribute  a b     _ -> AST_Attribute  a b     loc
    AST_TopFunc    a b c d _ -> AST_TopFunc    a b c d loc
    AST_TopScript  a       _ -> AST_TopScript  a       loc
    AST_TopLambda  a b c   _ -> AST_TopLambda  a b c   loc
    AST_Event      a b c   _ -> AST_Event      a b c   loc
    AST_TopComment a         -> AST_TopComment a
  delLocation o = case o of
    AST_Attribute  a b     _ -> AST_Attribute       a (fd1 b)                 lu
    AST_TopFunc    a b c d _ -> AST_TopFunc         a      b  (fd3 c) (fd1 d) lu
    AST_TopScript  a       _ -> AST_TopScript (fd0  a)                        lu
    AST_TopLambda  a b c   _ -> AST_TopLambda       a (fd3 b) (fd1 c)         lu
    AST_Event      a b c   _ -> AST_Event           a      b  (fd1 c)         lu
    AST_TopComment a         -> AST_TopComment      a

instance HasLocation AST_Script where
  getLocation o = case o of
    AST_Comment      _         -> LocationUnknown
    AST_EvalObject   _ _     o -> o
    AST_IfThenElse   _ _ _ _ o -> o
    AST_TryCatch     _ _ _   o -> o
    AST_ForLoop      _ _ _   o -> o
    AST_WhileLoop    _ _     o -> o
    AST_ContinueExpr _ _ _   o -> o
    AST_ReturnExpr   _ _     o -> o
    AST_WithDoc      _ _     o -> o
  setLocation o loc = case o of
    AST_Comment      a         -> AST_Comment      a
    AST_EvalObject   a b     _ -> AST_EvalObject   a b     loc
    AST_IfThenElse   a b c d _ -> AST_IfThenElse   a b c d loc
    AST_WhileLoop    a b     _ -> AST_WhileLoop    a b     loc
    AST_TryCatch     a b c   _ -> AST_TryCatch     a b c   loc
    AST_ForLoop      a b c   _ -> AST_ForLoop      a b c   loc
    AST_ContinueExpr a b c   _ -> AST_ContinueExpr a b c   loc
    AST_ReturnExpr   a b     _ -> AST_ReturnExpr   a b     loc
    AST_WithDoc      a b     _ -> AST_WithDoc      a b     loc
  delLocation o = case o of
    AST_Comment      a         -> AST_Comment           a
    AST_EvalObject   a b     _ -> AST_EvalObject   (fd0 a)      b                  lu
    AST_IfThenElse   a b c d _ -> AST_IfThenElse        a  (fd0 b) (fd2 c) (fd2 d) lu
    AST_WhileLoop    a b     _ -> AST_WhileLoop    (fd1 a) (fd1 b)                 lu
    AST_TryCatch     a b c   _ -> AST_TryCatch     (fd2 a)      b  (fd1 c)         lu
    AST_ForLoop      a b c   _ -> AST_ForLoop           a  (fd1 b) (fd1 c)         lu
    AST_ContinueExpr a b c   _ -> AST_ContinueExpr      a       b  (fd1 c)         lu
    AST_ReturnExpr   a b     _ -> AST_ReturnExpr        a  (fd1 b)                 lu
    AST_WithDoc      a b     _ -> AST_WithDoc      (fd1 a) (fd1 b)                 lu

instance HasLocation AST_Object where
  getLocation o = case o of
    AST_Void -> LocationUnknown
    AST_Literal    _     o -> o
    AST_Assign     _ _ _ o -> o
    AST_Equation   _ _ _ o -> o
    AST_Prefix     _ _   o -> o
    AST_Paren      _     o -> o
    AST_ArraySub   _ _ _ o -> o
    AST_FuncCall   _ _ _ o -> o
    AST_Dict       _ _ _ o -> o
    AST_Array      _ _   o -> o
    AST_Struct     _ _   o -> o
    AST_Data       _ _   o -> o
    AST_Lambda     _ _ _ o -> o
    AST_MetaEval   _     o -> o
  setLocation o loc = case o of
    AST_Void             -> AST_Void
    AST_Literal  a     _ -> AST_Literal  a     loc
    AST_Assign   a b c _ -> AST_Assign   a b c loc
    AST_Equation a b c _ -> AST_Equation a b c loc
    AST_Prefix   a b   _ -> AST_Prefix   a b   loc
    AST_Paren    a     _ -> AST_Paren    a     loc
    AST_ArraySub a b c _ -> AST_ArraySub a b c loc
    AST_FuncCall a b c _ -> AST_FuncCall a b c loc
    AST_Dict     a b c _ -> AST_Dict     a b c loc
    AST_Array    a b   _ -> AST_Array    a b   loc
    AST_Struct   a b   _ -> AST_Struct   a b   loc
    AST_Data     a b   _ -> AST_Data     a b   loc
    AST_Lambda   a b c _ -> AST_Lambda   a b c loc
    AST_MetaEval a     _ -> AST_MetaEval a     loc
  delLocation o = case o of
    AST_Void             -> AST_Void
    AST_Literal  a     _ -> AST_Literal       a                  lu
    AST_Assign   a b c _ -> AST_Assign   (fd0 a)      b  (fd0 c) lu
    AST_Equation a b c _ -> AST_Equation (fd0 a)      b  (fd0 c) lu
    AST_Prefix   a b   _ -> AST_Prefix        a  (fd1 b)         lu
    AST_Paren    a     _ -> AST_Paren    (fd1 a)                 lu
    AST_ArraySub a b c _ -> AST_ArraySub (fd0 a)      b  (fd2 c) lu
    AST_FuncCall a b c _ -> AST_FuncCall (fd0 a)      b  (fd2 c) lu
    AST_Dict     a b c _ -> AST_Dict          a       b  (fd2 c) lu
    AST_Array    a b   _ -> AST_Array    (fd3 a) (fd2 b)         lu
    AST_Struct   a b   _ -> AST_Struct   (fd1 a) (fd2 b)         lu
    AST_Data     a b   _ -> AST_Data          a       b          lu
    AST_Lambda   a b c _ -> AST_Lambda        a  (fd3 b) (fd1 c) lu
    AST_MetaEval a     _ -> AST_MetaEval (fd1 a)                 lu

----------------------------------------------------------------------------------------------------

-- | Comments in the Dao language are not interpreted, but they are not disgarded either. Dao is
-- intended to manipulate natural language, and itself, so that it can "learn" new semantic
-- structures. Dao scripts can manipulate the syntax tree of other Dao scripts, and so it might be
-- helpful if the syntax tree included comments.
data Comment
  = InlineComment  UStr
  | EndlineComment UStr
  deriving (Eq, Ord, Show, Typeable)

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
  deriving (Eq, Ord, Show, Typeable)

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
class Show obj => Intermediate obj ast | obj -> ast, ast -> obj where
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
uc :: Com a -> [a]
uc = return . unComment
ucx :: [Com a] -> [[a]]
ucx = return . map unComment
uc0 :: Intermediate obj ast =>  Com ast  -> [obj]
uc0 = toInterm . unComment
uc1 :: Intermediate obj ast => [Com ast] -> [[obj]]
uc1 = return . concatMap (toInterm . unComment)
uc2 :: Intermediate obj ast => Com [Com ast] -> [[obj]]
uc2 = uc1 . unComment

ti :: Intermediate obj ast => ast -> [obj]
ti = toInterm
ti0 :: Intermediate obj ast => [ast] -> [[obj]]
ti0 = return . (>>=toInterm)
ti1 :: Intermediate obj ast => Com [ast] -> [[obj]]
ti1 = ti0 . unComment

nc :: a -> [Com a]
nc = return . Com
ncx :: [a] -> [[Com a]]
ncx = return . map Com
nc0 :: Intermediate obj ast => obj -> [Com ast]
nc0 = fmap Com . fromInterm
nc1 :: Intermediate obj ast => [obj] -> [[Com ast]]
nc1 = return . map Com . concatMap fromInterm
nc2 :: Intermediate obj ast => [obj] -> [Com [Com ast]]
nc2 = fmap Com . nc1

fi :: Intermediate obj ast => obj -> [ast]
fi = fromInterm
fi0 :: Intermediate obj ast => [obj] -> [[ast]]
fi0 = return . (>>=fromInterm)
fi1 :: Intermediate obj ast => [obj] -> [Com [ast]]
fi1 = fmap Com . fi0

ll :: Location -> [Location]
ll = return

instance Intermediate TopLevelExpr AST_TopLevel where
  toInterm   ast = case ast of
    AST_Attribute  a b     loc -> liftM3 Attribute         [a]     (uc0 b)         (ll loc)
    AST_TopFunc    _ a b c loc -> liftM4 TopFunc           [a]     (uc2 b) (ti0 c) (ll loc)
    AST_TopScript  a       loc -> liftM2 TopScript         (ti  a)                 (ll loc)
    AST_TopLambda  a b c   loc -> liftM4 TopLambdaExpr [a] (uc2 b) (ti0 c)         (ll loc)
    AST_Event      a _ b   loc -> liftM3 EventExpr     [a] (ti0 b)                 (ll loc)
    AST_TopComment a           -> mzero
  fromInterm obj = case obj of
    Attribute     a b   loc -> liftM3 AST_Attribute [a]         (nc0 b)         [loc]
    TopFunc       a b c loc -> liftM5 AST_TopFunc   [[]]   [a]  (nc2 b) (fi0 c) [loc]
    TopScript     a     loc -> liftM2 AST_TopScript (fi a)                      [loc]
    TopLambdaExpr a b c loc -> liftM4 AST_TopLambda [a]         (nc2 b) (fi0 c) [loc]
    EventExpr     a b   loc -> liftM4 AST_Event     [a]    [[]] (fi0 b)         [loc]

instance Intermediate ScriptExpr AST_Script where
  toInterm   ast = case ast of
    AST_Comment      _           -> mzero
    AST_EvalObject   a b     loc -> liftM2 EvalObject   (ti  a)                 (ll loc)
    AST_IfThenElse   _ b c d loc -> liftM4 IfThenElse   (ti  b) (ti1 c) (ti1 d) (ll loc)
    AST_TryCatch     a b c   loc -> liftM4 TryCatch     (ti1 a) (uc  b) (ti0 c) (ll loc)
    AST_ForLoop      a b c   loc -> liftM4 ForLoop      (uc  a) (uc0 b) (ti0 c) (ll loc)
    AST_WhileLoop    a b     loc -> liftM3 WhileLoop    (uc0 a) (ti0 b)         (ll loc)
    AST_ContinueExpr a _ c   loc -> liftM3 ContinueExpr [a]     (uc0 c)         (ll loc)
    AST_ReturnExpr   a b     loc -> liftM3 ReturnExpr   [a]     (uc0 b)         (ll loc)
    AST_WithDoc      a b     loc -> liftM3 WithDoc      (uc0 a) (ti0 b)         (ll loc)
  fromInterm obj = case obj of
    EvalObject   a     loc -> liftM3 AST_EvalObject   (fi  a) [[]]                    [loc]
    IfThenElse   a b c loc -> liftM5 AST_IfThenElse   [[]]    (fi  a) (fi1 b) (fi1 c) [loc]
    TryCatch     a b c loc -> liftM4 AST_TryCatch     (fi1 a) (nc  b) (fi0 c)         [loc]
    ForLoop      a b c loc -> liftM4 AST_ForLoop      (nc  a) (nc0 b) (fi0 c)         [loc]
    WhileLoop    a b   loc -> liftM3 AST_WhileLoop    (nc0 a) (fi0 b)                 [loc]
    ContinueExpr a b   loc -> liftM4 AST_ContinueExpr [a]     [[]]    (nc0 b)         [loc]
    ReturnExpr   a b   loc -> liftM3 AST_ReturnExpr   [a]     (nc0 b)                 [loc]
    WithDoc      a b   loc -> liftM3 AST_WithDoc      (nc0 a) (fi0 b)                 [loc]

instance Intermediate ObjectExpr AST_Object where
  toInterm   ast = case ast of
    AST_Void               -> return VoidExpr
    AST_Literal  a     loc -> liftM2 Literal      [a]                     (ll loc)
    AST_Assign   a b c loc -> liftM4 AssignExpr   (ti  a) (uc  b) (ti  c) (ll loc)
    AST_Equation a b c loc -> liftM4 Equation     (ti  a) (uc  b) (ti  c) (ll loc)
    AST_Prefix   a b   loc -> liftM3 PrefixExpr   [a]     (uc0 b)         (ll loc)
    AST_Paren    a     loc -> liftM2 ParenExpr            (uc0 a)         (ll loc)
    AST_ArraySub a _ c loc -> liftM3 ArraySubExpr (ti  a) (uc1 c)         (ll loc)
    AST_FuncCall a _ c loc -> liftM3 FuncCall     (ti  a) (uc1 c)         (ll loc)
    AST_Dict     a _ c loc -> liftM3 DictExpr     [a]     (uc1 c)         (ll loc)
    AST_Array    a b   loc -> liftM3 ArrayExpr    (uc2 a) (uc1 b)         (ll loc)
    AST_Struct   a b   loc -> liftM3 StructExpr   (uc0 a) (uc1 b)         (ll loc)
    AST_Data     _ b   loc -> liftM2 DataExpr     (ucx b)                 (ll loc)
    AST_Lambda   a b c loc -> liftM4 LambdaExpr   [a]     (uc2 b) (ti0 c) (ll loc)
    AST_MetaEval a     loc -> liftM2 MetaEvalExpr (uc0 a)                 (ll loc)
  fromInterm obj = case obj of
    VoidExpr               -> return AST_Void
    Literal      a     loc -> liftM2 AST_Literal  [a]                     [loc]
    AssignExpr   a b c loc -> liftM4 AST_Assign   (fi  a) (nc  b) (fi  c) [loc]
    Equation     a b c loc -> liftM4 AST_Equation (fi  a) (nc  b) (fi  c) [loc]
    PrefixExpr   a b   loc -> liftM3 AST_Prefix   [a]             (nc0 b) [loc]
    ParenExpr    a     loc -> liftM2 AST_Paren                    (nc0 a) [loc]
    ArraySubExpr a b   loc -> liftM4 AST_ArraySub (fi  a) [[]]    (nc1 b) [loc]
    FuncCall     a b   loc -> liftM4 AST_FuncCall (fi  a) [[]]    (nc1 b) [loc]
    DictExpr     a b   loc -> liftM4 AST_Dict     [a]     [[]]    (nc1 b) [loc]
    ArrayExpr    a b   loc -> liftM3 AST_Array    (nc2 a)         (nc1 b) [loc]
    StructExpr   a b   loc -> liftM3 AST_Struct   (nc0 a)         (nc1 b) [loc]
    DataExpr     a     loc -> liftM3 AST_Data     [[]]            (ncx a) [loc]
    LambdaExpr   a b c loc -> liftM4 AST_Lambda   [a]     (nc2 b) (fi0 c) [loc]
    MetaEvalExpr a     loc -> liftM2 AST_MetaEval (nc0 a)                 [loc]

