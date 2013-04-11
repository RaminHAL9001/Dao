-- "src/Dao/Object/Struct.hs"  instantiation of Dao objects into the
-- 'Dao.Struct.Structured' class.
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

module Dao.Object.Struct where

import           Prelude hiding (lookup)

import           Dao.Object
import           Dao.Struct
import           Dao.Predicate
import           Dao.Token
import           Dao.Glob
import           Dao.EnumSet
import           Dao.Parser
import qualified Dao.Tree    as T
import           Dao.Parser

import           Control.Monad
import           Control.Monad.State
import           Control.DeepSeq

import           Data.Maybe
import           Data.List (partition, isSuffixOf)
import qualified Data.Map    as M
import qualified Data.IntMap as I

import qualified Data.ByteString.Lazy as B

----------------------------------------------------------------------------------------------------

ostr :: String -> Object
ostr = OString . ustr

failed :: String -> Update ig
failed msg = pvalue (PFail ONull (ustr msg))

allStrings :: [Object] -> PValue Object [UStr]
allStrings ox = forM (zip (iterate (+1) 0) ox) $ \ (i, o) -> case o of
  OString o -> return o
  _         -> PFail (OList [ostr "in list, item number", OWord i]) (ustr "must be a string value")

instance Structured UStr where
  dataToStruct a = T.Leaf (OString a)
  structToData = evalUpdate $ do
    a <- this
    case a of
      OString a -> return a
      _         -> failed "expecing string constant"

instance Structured Comment where
  dataToStruct a = case a of
    InlineComment  a -> done "inline"  a
    EndlineComment a -> done "endline" a
    where
      done msg a = T.Leaf $ OPair (ostr msg, OString a)
  structToData = evalUpdate $ do
    a <- this
    case a of
      OString a -> return (InlineComment a)
      OPair (msg, OString a)
        | msg == ostr "inline"  -> return (InlineComment  a)
        | msg == ostr "endline" -> return (EndlineComment a)
        | otherwise             -> nope
      _         -> nope
    where { nope = failed "must be a comment string or typed comment string" }

instance Structured a => Structured (Com a) where
  dataToStruct a = case a of
    Com         a   -> T.Leaf (OTree (dataToStruct a))
    ComBefore a b   -> ba "before" a $ dataToStruct b
    ComAfter    a b -> ba "after"  b $ dataToStruct a
    ComAround a b c -> ba "before" a $ ba "after" c $ dataToStruct b
    where
      ba msg coms = T.insert [ustr msg] (OList (map (OTree . dataToStruct) coms))
  structToData = evalUpdate $ do
    c <- this >>= asNode "commented sub-structure" >>= pvalue . structToData
    let getcoms nm =
          mplus (lookup [ustr nm]) (return (OList []))
            >>= asList "comments"
            >>= mapM (asNode "comments" >=> pvalue . structToData)
    befor <- getcoms "before"
    after <- getcoms "after"
    return $ case (befor, after) of
      ([], []) -> Com c
      (ax, []) -> ComBefore ax c
      ([], bx) -> ComAfter     c bx
      (ax, bx) -> ComAround ax c bx

fromShowable :: Show a => a -> T.Tree Name Object
fromShowable = T.Leaf . OString . ustr . show

fromReadable :: Read a => String -> T.Tree Name Object -> PValue Object a
fromReadable msg = evalUpdate $ do
  let msg = "an "++msg++" expressed as a string value"
  a <- this >>= asString msg -- TODO: OChar should also be acceptable here
  case readsPrec 0 (uchars a) of
    [(o, "")] -> return o
    _         ->  pvalue (PFail (OString a) (ustr ("was expecting "++msg)))

instance Structured UpdateOp where
  dataToStruct = fromShowable
  structToData = fromReadable "assignment operator"

instance Structured ArithOp1 where
  dataToStruct = fromShowable
  structToData = fromReadable "unary prefix operator"
  
instance Structured ArithOp2 where
  dataToStruct = fromShowable
  structToData = fromReadable "binary infix operator"

instance Structured LambdaExprType where
  dataToStruct a = undefined
  structToData = evalUpdate $ undefined

instance Structured TypeID   where
  dataToStruct a = undefined
  structToData = evalUpdate $ undefined

instance Structured GlobUnit  where
  dataToStruct a = undefined
  structToData = evalUpdate $ undefined

instance Structured ObjectExpr where
  dataToStruct a = case a of
    VoidExpr             -> undefined
    Literal      a b     -> undefined
    AssignExpr   a b c d -> undefined
    Equation     a b c d -> undefined
    PrefixExpr   a b c   -> undefined
    ParenExpr    a b c   -> undefined
    ArraySubExpr a b c d -> undefined
    FuncCall     a b c d -> undefined
    DictExpr     a b c d -> undefined
    ArrayExpr    a b c   -> undefined
    StructExpr   a b c   -> undefined
    DataExpr     a b c   -> undefined
    LambdaExpr   a b c d -> undefined
    MetaEvalExpr a b     -> undefined
  structToData = evalUpdate $ undefined

instance Structured Location where
  dataToStruct a = case a of
    LocationUnknown      -> undefined
    Location a b c d e f -> undefined
  structToData = evalUpdate $ undefined

instance Structured ScriptExpr where
  dataToStruct a = case a of
    EvalObject   a b c     -> undefined
    IfThenElse   a b c d e -> undefined
    TryCatch     a b c d   -> undefined
    ForLoop      a b c d   -> undefined
    WhileLoop    a b c     -> undefined
    ContinueExpr a b c d   -> undefined
    ReturnExpr   a b c     -> undefined
    WithDoc      a b c     -> undefined
  structToData = evalUpdate $ undefined

instance Structured Reference where
  dataToStruct a = case a of
     IntRef     a   -> undefined
     LocalRef   a   -> undefined
     StaticRef  a   -> undefined
     QTimeRef   a   -> undefined
     GlobalRef  a   -> undefined
     ProgramRef a b -> undefined
     FileRef    a b -> undefined
     Subscript  a b -> undefined
     MetaRef    a   -> undefined
  structToData = evalUpdate $ undefined

instance Structured Object where
  dataToStruct a = case a of
    ONull        -> undefined
    OTrue        -> undefined
    OType      a -> undefined
    OInt       a -> undefined
    OWord      a -> undefined
    OLong      a -> undefined
    OFloat     a -> undefined
    ORatio     a -> undefined
    OComplex   a -> undefined
    OTime      a -> undefined
    ODiffTime  a -> undefined
    OChar      a -> undefined
    OString    a -> undefined
    ORef       a -> undefined
    OPair  (a,b) -> undefined
    OList      a -> undefined
    OSet       a -> undefined
    OArray     a -> undefined
    ODict      a -> undefined
    OIntMap    a -> undefined
    OTree      a -> undefined
    OGlob      a -> undefined
    OScript    a -> undefined
    ORule      a -> undefined
    OBytes     a -> undefined
  structToData = evalUpdate $ undefined

instance (Structured a, Structured b) => Structured (T.Tree a b) where
  dataToStruct a = case a of
    T.Void           -> undefined
    T.Leaf       a   -> undefined
    T.Branch       b -> undefined
    T.LeafBranch a b -> undefined
  structToData = evalUpdate $ undefined

instance Structured Glob       where
  dataToStruct a = case a of
    Glob       a b   -> undefined
  structToData = evalUpdate $ undefined

instance Structured Subroutine where
  dataToStruct a = case a of
    Subroutine a b _ -> undefined
  structToData = evalUpdate $ undefined

instance Structured Rule       where
  dataToStruct a = case a of
    Rule       a b _ -> undefined
  structToData = evalUpdate $ undefined

instance Structured Pattern where
  dataToStruct a = case a of
    ObjAnyX        -> undefined
    ObjMany        -> undefined
    ObjAny1        -> undefined
    ObjEQ      a   -> undefined
    ObjType    a   -> undefined
    ObjBounded a b -> undefined
    ObjList    a b -> undefined
    ObjNameSet a b -> undefined
    ObjIntSet  a b -> undefined
    ObjElemSet a b -> undefined
    ObjChoice  a b -> undefined
    ObjLabel   a b -> undefined
    ObjFailIf  a b -> undefined
    ObjNot     a   -> undefined
  structToData = evalUpdate $ undefined

instance Structured ObjSetOp where
  dataToStruct a = case a of
    a -> undefined
  structToData = evalUpdate $ undefined

instance Structured TopLevelEventType where
  dataToStruct a = case a of
    a -> undefined
  structToData = evalUpdate $ undefined

instance Structured TopLevelExpr where
  dataToStruct a = case a of
    Attribute      a b c   -> undefined
    TopFunc        a b c d -> undefined
    TopScript      a b     -> undefined
    TopLambdaExpr  a b c d -> undefined
    EventExpr      a b c   -> undefined
    TopComment     a       -> undefined
  structToData = evalUpdate $ undefined

