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
import           Dao.Object.AST
import           Dao.Struct
import           Dao.Predicate
import           Dao.EnumSet
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
import           Data.Int
import           Data.Word
import qualified Data.Map    as M
import qualified Data.Set    as S
import qualified Data.IntMap as I
import qualified Data.IntSet as IS

import qualified Data.ByteString.Lazy as B

----------------------------------------------------------------------------------------------------

ostr :: String -> Object
ostr = OString . ustr

allStrings :: [Object] -> PValue UpdateErr [UStr]
allStrings ox = forM (zip (iterate (+1) 0) ox) $ \ (i, o) -> case o of
  OString o -> return o
  _         ->
    PFail ([], OList [ostr "in list, item number", OWord i]) (ustr "must be a string value")

instance Structured Comment where
  dataToStruct a = case a of
    InlineComment  a -> done "inline"  a
    EndlineComment a -> done "endline" a
    where
      done msg a = T.Leaf $ OPair (ostr msg, OString a)
  structToData = reconstruct $ do
    a <- this
    case a of
      OString a -> return (InlineComment a)
      OPair (msg, OString a)
        | msg == ostr "inline"  -> return (InlineComment  a)
        | msg == ostr "endline" -> return (EndlineComment a)
        | otherwise             -> nope (OString a)
      _         -> nope a
    where { nope a = updateFailed a "must be a comment string or typed comment string" }

instance Structured a => Structured (Com a) where
  dataToStruct a = deconstruct $ case a of
    Com         c   -> putData c
    ComBefore b c   -> putData c >> com "Before" b
    ComAfter    c a -> putData c >> com "After"  a
    ComAround b c a -> putData c >> com "Before" b >> com "After" a
    where { com msg = putDataAt ("comments"++msg) }
  structToData = reconstruct $ do
    c <- getData
    befor <- getDataAt "commentsBefore"
    after <- getDataAt "commentsAfter"
    return $ case (befor, after) of
      ([], []) -> Com c
      (ax, []) -> ComBefore ax c
      ([], bx) -> ComAfter     c bx
      (ax, bx) -> ComAround ax c bx

fromShowable :: Show a => a -> T.Tree Name Object
fromShowable = T.Leaf . OString . ustr . show

fromReadableOrChars :: Read a => [Char] -> String -> T.Tree Name Object -> PValue UpdateErr a
fromReadableOrChars chrs msg = reconstruct $ do
  let msg = "an "++msg++" expressed as a string value"
      asChar o = case o of
        OChar c | elem c chrs -> return [c]
        _ -> mzero
  a <- getStringData msg
  case readsPrec 0 a of
    [(o, "")] -> return o
    _         -> updateFailed (OString (ustr a)) ("was expecting "++msg)

fromReadable :: Read a => String -> T.Tree Name Object -> PValue UpdateErr a
fromReadable = fromReadableOrChars ""

instance Structured UpdateOp where
  dataToStruct = fromShowable
  structToData = fromReadableOrChars allUpdateOpChars "assignment operator"

instance Structured ArithOp1 where
  dataToStruct = fromShowable
  structToData = fromReadableOrChars allArithOp1Chars "unary prefix operator"
  
instance Structured ArithOp2 where
  dataToStruct = fromShowable
  structToData = fromReadableOrChars allArithOp2Chars "binary infix operator"

instance Structured LambdaExprType where
  dataToStruct = fromShowable
  structToData = fromReadable "type of lambda expression"

instance Structured TypeID   where
  dataToStruct = fromShowable
  structToData = fromReadable "type identifier"

-- This instance never places data in the immediate ('Data.Struct.this') node, so it is safe to call
-- 'putData' on a value of type 'Data.Token.Location' even if you have already placed data in the
-- immedate node with 'putData'.
instance Structured Location where
  dataToStruct loc = case loc of
    LocationUnknown      -> T.Void
    Location _ _ _ _ _ _ -> deconstruct $ do
      with "from" $ do
        with "line"   (place $ OWord $ startingLine   loc)
        with "column" (place $ OWord $ fromIntegral $ startingColumn loc)
        with "char"   (place $ OWord $ startingChar   loc)
      if   startingLine   loc == endingLine   loc
        && startingColumn loc == endingColumn loc
        && startingChar   loc == endingChar   loc
      then  return ()
      else  with "to"   $ do
              with "line"   (place $ OWord $ endingLine     loc)
              with "column" (place $ OWord $ fromIntegral $ endingColumn   loc)
              with "char"   (place $ OWord $ endingChar     loc)
  structToData = reconstruct $ do
    let getPos = liftM3 (,,) (getDataAt "line") (getDataAt "column") (getDataAt "char")
    (a,b,c) <- mplus (with "from" getPos) (updateFailed ONull "location data")
    flip mplus (return (Location a b c a b c)) $ do
      (d,e,f) <- getPos
      return (Location a b c d e f)

-- optionally put a location
optPutLoc :: Location -> Update ()
optPutLoc loc = case loc of
  LocationUnknown -> return ()
  loc             -> with "location" (putData loc)

instance Structured AST_Object where
  dataToStruct a = deconstruct $ case a of
    AST_Void               -> return ()
    AST_Literal      a     loc -> with "literal" (place a >> putData loc)
    AST_Assign   a b c loc -> with "assign" $
      putDataAt "to" a >> putDataAt "op" b >> putData c >> putData loc
    AST_Equation     a b c loc -> with "equation" $
      putData a >> putDataAt "op" b >> putDataAt "next" c >> putData loc
    AST_Prefix   a b   loc -> with "prefix" $
      putDataAt "op" a >> putData b >> putData loc
    AST_Paren    a b   loc -> with "paren" $
      putDataAt "visible" a >> putData b >> putData loc
    AST_ArraySub a b c loc -> with "subscript" $
      putDataAt "bounds" a >> putDataAt "comments" b >> putData c >> putData loc
    AST_FuncCall     a b c loc -> with "funcCall" $
      putData a >> putDataAt "comments" b >> putDataAt "arguments" c >> putData loc
    AST_Dict     a b c loc -> with "constructor" $
      putData a >> putDataAt "comments" b >> putDataAt "arguments" c >> putData loc
    AST_Array    a b   loc -> with "constructor" $
      putData (ustr "array") >> putDataAt "bounds" a >> putDataAt "arguments" b >> putData loc
    AST_Struct   a b   loc -> with "constructor" $
      putData (ustr "struct") >> putData a >> putDataAt "arguments" b >> putData loc
    AST_Data     a b   loc -> with "constrcutor" $
      putData (ustr "data") >> putDataAt "comments" a >> putDataAt "arguments" b >> putData loc
    AST_Lambda   a b c loc -> with "constructor" $
      putData (ustr (show a)) >> putDataAt "arguments" b >> putDataAt "script" c >> putData loc
    AST_MetaEval a     loc -> with "metaEval" $ putData a >> putData loc
  structToData = reconstruct $ msum $
    [ with "literal"   $ liftM2 AST_Literal      getData getData
    , with "assign"    $ liftM4 AST_Assign   (getDataAt "to") (getDataAt "op") getData getData
    , with "equation"  $ liftM4 AST_Equation     getData (getDataAt "op") (getDataAt "next") getData
    , with "prefix"    $ liftM3 AST_Prefix   (getDataAt "op") getData getData
    , with "paren"     $ liftM3 AST_Paren    (mplus (getDataAt "visible") (return False)) getData getData
    , with "subscript" $ liftM4 AST_ArraySub (getDataAt "bounds") (getDataAt "comments") getData getData
    , with "funcCall"  $ liftM4 AST_FuncCall     getData (getDataAt "comments") (getDataAt "arguments") getData
    , with "constructor" $ do
        kind <- getStringData "constructor type"
        case kind of
          "array"  -> liftM3 AST_Array (getDataAt "bounds") (getDataAt "arguments") getData
          "struct" -> liftM3 AST_Struct getData (getDataAt "arguments") getData
          "data"   -> liftM3 AST_Data   (getDataAt "comments") (getDataAt "arguments") getData
          kind -> case readsPrec 0 kind of
            [(kind, "")] -> liftM3 (AST_Lambda kind) (getDataAt "arguments") (getDataAt "script") getData
            _ -> updateFailed (ostr kind) "unknown type of object expression contructor"
    , with "metaEval"  $ liftM2 AST_MetaEval getData getData
    , mplus this (return ONull) >>= \o -> updateFailed o "object expression"
    ]

instance Structured AST_Script where
  dataToStruct a = deconstruct $ case a of
    AST_EvalObject   a b     loc -> with "eval" $
      putData a >> putDataAt "comments" b >> putData loc
    AST_IfThenElse   a b c d loc -> with "if" $
      putDataAt "comments" a >> putData b >> putDataAt "then" c >> putDataAt "else" d >> putData loc
    AST_TryCatch     a b c   loc -> with "try" $ do
      putData a
      if unComment b == nil then return () else putDataAt "varName" b >> putDataAt "catch" c
      putData loc
    AST_ForLoop      a b c   loc -> with "for" $
      putDataAt "varName" a >> putData b >> putDataAt "script" c >> putData loc
    AST_WhileLoop    a b     loc -> with "while" $
      putData a >> putDataAt "script" b >> putData loc
    AST_ContinueExpr a b c   loc -> with (if a then "continue" else "break") $
      putDataAt "comments" b >> putData c >> putData loc
    AST_ReturnExpr   a b     loc -> with (if a then "return" else "throw") $
      putData b >> putData loc
    AST_WithDoc      a b     loc -> with "with" $
      putData a >> putDataAt "script" b >> putData loc
  structToData = reconstruct $ msum $
    [ with "eval"  $ liftM3 AST_EvalObject getData (getDataAt "comments") getData
    , with "if"    $
        liftM5 AST_IfThenElse (getDataAt "comments") getData (getDataAt "then") (getDataAt "else") getData
    , with "try"   $ do
        script <- getData
        str <- getDataAt "varName"
        catch <- if unComment str == nil then return [] else getDataAt "catch"
        loc <- getData
        return (AST_TryCatch script str catch loc)
    , with "for"   $ liftM4 AST_ForLoop (getDataAt "varName") getData (getDataAt "script") getData
    , with "while" $ liftM3 AST_WhileLoop getData (getDataAt "script") getData
    , with "continue" $ getContinue True
    , with "break"    $ getContinue False
    , with "return"   $ getReturn True
    , with "throw"    $ getReturn False
    , with "with"  $ liftM3 AST_WithDoc getData (getDataAt "script") getData
    , mplus this (return ONull) >>= \o -> updateFailed o "script expression"
    ]
    where
      getContinue tf = liftM3 (AST_ContinueExpr tf) (getDataAt "comments") getData getData
      getReturn   tf = liftM2 (AST_ReturnExpr   tf)                        getData getData

instance Structured Reference where
  dataToStruct = deconstruct . place . ORef
  structToData = reconstruct $ do
    a <- this
    case a of
      ORef a -> return a
      _ -> updateFailed a "reference"

instance Structured GlobUnit  where
  dataToStruct a = T.Leaf $ OString $ case a of
    Wildcard -> ustr "$*"
    AnyOne   -> ustr "$?"
    Single s -> s
  structToData = reconstruct $ do
    str <- getStringData "glob-unit"
    return $ case str of
      "$*" -> Wildcard
      "*"  -> Wildcard
      "$?" -> AnyOne
      "?"  -> AnyOne
      str  -> Single (ustr str)

instance Structured Glob       where
  dataToStruct a = deconstruct $ case a of
    Glob       a _   -> putData a
  structToData = reconstruct $ getData >>= \a -> return (Glob a (length a))

uninitialized_err a = error $ concat $
  [ a, " was just constructed from 'structToData',"
  , "the executable has not yet been initialized"
  ]

instance Structured Subroutine where
  dataToStruct a = deconstruct $ case a of
    Subroutine a _ -> putData a
  structToData = reconstruct $ do
    a <- getData
    return $ Subroutine a $ uninitialized_err "subroutine"

instance Structured Pattern where
  dataToStruct a = deconstruct $ case a of
    ObjAnyX        -> putStringData "any"
    ObjMany        -> putStringData "many"
    ObjAny1        -> putStringData "any1"
    ObjEQ      a   -> with "equals" (place a)
    ObjType    a   -> putDataAt "type" a
    ObjBounded a b -> with "bounded" (putData a >> putDataAt "to" b)
    ObjList    a b -> with "list" (putDataAt "op" a >> putData b)
    ObjNameSet a b -> with "nameSet" (putDataAt "op" a >> putData (S.elems b))
    ObjIntSet  a b -> with "intSet" $
      putDataAt "op" a >> place (OList (map (OInt . fromIntegral) (IS.elems b)))
    ObjElemSet a b -> with "elemSet" (putDataAt "op" a >> putData (S.elems b))
    ObjChoice  a b -> with "choice" (putDataAt "op" a >> putData (S.elems b))
    ObjLabel   a b -> with "label" (putDataAt "name" a >> putData b)
    ObjFailIf  a b -> with "require" (putDataAt "message" a >> putData b)
    ObjNot     a   -> with "not" (putData a)
  structToData = reconstruct $ msum $
    [ getStringData "pattern" >>= \a -> case a of
        "any"  -> return ObjAnyX
        "many" -> return ObjMany
        "any1" -> return ObjAny1
        _      -> pvalue Backtrack
    , with "equals"  $ liftM  ObjEQ       this
    , with "type"    $ liftM  ObjType     getData
    , with "bounded" $ liftM2 ObjBounded  getData           (getDataAt "to")
    , with "list"    $ liftM2 ObjList    (getDataAt "op")    getData
    , with "nameSet" $ liftM2 ObjNameSet (getDataAt "op")   (fmap S.fromList  getData)
    , with "intSet"  $ liftM2 ObjIntSet  (getDataAt "op")   (fmap IS.fromList getData)
    , with "elemSet" $ liftM2 ObjElemSet (getDataAt "op")   (fmap S.fromList  getData)
    , with "choice"  $ liftM2 ObjChoice  (getDataAt "op")   (fmap S.fromList  getData)
    , with "label"   $ liftM2 ObjLabel   (getDataAt "name")  getData
    , with "require" $ liftM2 ObjFailIf  (getDataAt "message") getData
    , with "not"     $ liftM  ObjNot     getData
    , mplus this (return ONull) >>= \a -> updateFailed a "pattern"
    ]

instance Structured ObjSetOp where
  dataToStruct a = deconstruct $ place $ ostr $ case a of
    ExactSet  -> "exact"
    AnyOfSet  -> "any"
    AllOfSet  -> "all"
    OnlyOneOf -> "only"
    NoneOfSet -> "none"
  structToData = reconstruct $ getStringData "pattern set operator" >>= \a -> case a of
    "exact" -> return ExactSet
    "any"   -> return AnyOfSet
    "all"   -> return AllOfSet
    "only"  -> return OnlyOneOf
    "none"  -> return NoneOfSet
    a       -> updateFailed (ostr a) "pattern set operator"

instance Structured TopLevelEventType where
  dataToStruct a = deconstruct $ place $ ostr $ case a of
    BeginExprType -> "BEGIN"
    EndExprType   -> "END"
    ExitExprType  -> "EXIT"
  structToData = reconstruct $ getStringData "event type" >>= \a -> case a of
    "BEGIN" -> return BeginExprType
    "END"   -> return EndExprType
    "EXIT"  -> return ExitExprType
    "QUIT"  -> return ExitExprType
    _       -> updateFailed (ostr a) "top-level event type"

instance Structured AST_TopLevel where
  dataToStruct a = deconstruct $ case a of
    AST_Attribute      a b   loc -> with "attribute" $
      putData a >> putDataAt "value" b >> putData loc
    AST_TopFunc        a b c loc -> with "function" $
      putDataAt "name" a >> putData b >> putDataAt "script" c >> putData loc
    AST_TopScript      a     loc -> with "directive" $ putData a >> putData loc
    AST_TopLambda  a b c loc -> with "lambda" $
      putDataAt "type" a >> putData b >> putDataAt "script" c >> putData loc
    AST_Event      a b   loc -> with "event" $
      putDataAt "type" a >> putDataAt "script" b
    AST_TopComment     a         -> putDataAt "comment" a
  structToData = reconstruct $ msum $
    [ with "attribute" $ liftM3 AST_Attribute getData (getDataAt "value") getData
    , with "function"  $ liftM4 AST_TopFunc (getDataAt "name") getData (getDataAt "script") getData
    , with "directive" $ liftM2 AST_TopScript getData getData
    , with "lambda"    $ liftM4 AST_TopLambda (getDataAt "type") getData (getDataAt "script") getData
    , with "event"     $ liftM3 AST_Event (getDataAt "type") (getDataAt "script") getData
    , with "comment"   $ liftM AST_TopComment getData
    ]

instance (Ord a, Enum a, Structured a) => Structured (EnumInf a) where
  dataToStruct a = deconstruct $ case a of
    EnumPosInf  -> putStringData "+inf"
    EnumNegInf  -> putStringData "-inf"
    EnumPoint a -> putData a
  structToData = reconstruct $ msum $
    [ fmap EnumPoint getData
    , getStringData msg >>= \a -> case a of
        "+inf" -> return EnumPosInf
        "-inf" -> return EnumNegInf
    , mplus this (return ONull) >>= \o -> updateFailed o msg
    ]
    where { msg = "unit of a segment of an enum set" }

instance (Ord a, Enum a, Bounded a, Structured a) => Structured (Segment a) where
  dataToStruct a = deconstruct $
    mplus (maybeToUpdate (singular a) >>= putDataAt "at") $
      maybeToUpdate (plural a) >>= \ (a, b) -> putDataAt "to" a >> putDataAt "from" b
  structToData = reconstruct $ msum $
    [ getDataAt "to" >>= \a -> getDataAt "from" >>= \b -> return (segment a b)
    , fmap single (getDataAt "at")
    , mplus this (return ONull) >>= \o -> updateFailed o "unit segment of an enum set"
    ]

instance (Ord a, Enum a, Bounded a, Structured a) => Structured (EnumSet a) where
  dataToStruct a = deconstruct (putData (listSegments a))
  structToData = reconstruct (fmap enumSet getData)

