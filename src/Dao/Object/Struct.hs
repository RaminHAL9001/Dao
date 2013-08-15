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
import           Dao.Parser
import           Dao.Glob
import qualified Dao.EnumSet as Es
import           Dao.Token
import qualified Dao.Tree    as T

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

import Debug.Trace

----------------------------------------------------------------------------------------------------

allStrings :: [Object] -> PValue UpdateErr [UStr]
allStrings ox = forM (zip [0..] ox) $ \ (i, o) -> case o of
  OString o -> return o
  OChar   c -> return (ustr [c])
  _         -> fail ("in list, item number "++show i++" must be a string value")

putUStrList :: [UStr] -> Update ()
putUStrList = place . OList . map OString 

getUStrList :: Update [UStr]
getUStrList = do
  ls <- this
  case ls of
    OList ls -> assumePValue (allStrings ls)
    ls       -> fail "expecting a list of strings of base64-encoded data"

----------------------------------------------------------------------------------------------------

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
        | otherwise             -> nope
      _         -> nope
    where { nope = fail "must be a comment string or typed comment string" }

instance Structured a => Structured (Com a) where
  dataToStruct a = deconstruct $ case a of
    Com         c   -> putData c
    ComBefore b c   -> putData c >> com "before" b
    ComAfter    c a -> putData c >> com "after"  a
    ComAround b c a -> putData c >> com "before" b >> com "after" a
    where { com msg = putDataAt ("comments"++msg) }
  structToData = reconstruct $ do
    c <- getData
    befor <- mplus (getDataAt "commentsBefore") (return [])
    after <- mplus (getDataAt "commentsAfter")  (return [])
    return $ case (befor, after) of
      ([], []) -> Com c
      (ax, []) -> ComBefore ax c
      ([], bx) -> ComAfter     c bx
      (ax, bx) -> ComAround ax c bx

putComments :: [Comment] -> Update ()
putComments coms = if null coms then return () else putDataAt "comments" coms

getComments :: Update [Comment]
getComments = mplus (tryGetDataAt "comments") (return [])

ustrToStruct :: UStrType a => a -> T.Tree Name Object
ustrToStruct = deconstruct . place . ostr

structToUStr :: UStrType a => String -> T.Tree Name Object -> PValue UpdateErr a
structToUStr msg = reconstruct $ do
  a <- getUStrData msg
  case maybeFromUStr a of
    Just  a -> return a
    Nothing -> fail ("was expecting "++msg)

instance Structured UpdateOp where
  dataToStruct = ustrToStruct
  structToData = structToUStr "assignment operator"

instance Structured PrefixOp where
  dataToStruct = ustrToStruct
  structToData = structToUStr "unary prefix operator"
  
instance Structured InfixOp where
  dataToStruct = ustrToStruct
  structToData = structToUStr "binary infix operator"

instance Structured LambdaExprType where
  dataToStruct = ustrToStruct
  structToData = structToUStr "type of lambda expression"

instance Structured TypeID   where
  dataToStruct = ustrToStruct
  structToData = structToUStr "type identifier"

-- This instance never places data in the immediate ('Data.Struct.this') node, so it is safe to call
-- 'putData' on a value of type 'Data.Token.Location' even if you have already placed data in the
-- immedate node with 'putData'.
instance Structured Location where
  dataToStruct loc = case loc of
    LocationUnknown  -> T.Void
    Location _ _ _ _ -> deconstruct $ with "location" $ do
      with "from" $ do
        with "line"   (place $ OWord $ fromIntegral $ startingLine   loc)
        with "column" (place $ OWord $ fromIntegral $ startingColumn loc)
      if   startingLine   loc == endingLine   loc
        && startingColumn loc == endingColumn loc
      then  return ()
      else  with "to"   $ do
              with "line"   (place $ OWord $ fromIntegral $ endingLine     loc)
              with "column" (place $ OWord $ fromIntegral $ endingColumn   loc)
  structToData = reconstruct $ flip mplus (return LocationUnknown) $ tryWith "location" $ do
    let getPos = liftM2 (,) (getDataAt "line") (getDataAt "column")
    (a,b) <- with "from" getPos
    flip mplus (return (Location a b a b)) $ tryWith "to" $ do
      (c,d) <- getPos
      return (Location a b c d)

instance Structured AST_Object where
  dataToStruct a = deconstruct $ case a of
    AST_Void               -> return ()
    AST_Literal  a     loc -> with "object"    $ place              a                                                  >> putData loc
    AST_Assign   a b c loc -> with "assign"    $ putDataAt "to"     a  >> putDataAt "op"     b >> putDataAt "from"   c >> putData loc
    AST_Equation a b c loc -> with "equation"  $ putDataAt "left"   a  >> putDataAt "op"     b >> putDataAt "right"  c >> putData loc
    AST_Prefix   a b   loc -> with "prefix"    $ putDataAt "op"     a  >> putDataAt "right"  b                         >> putData loc
    AST_Paren    a     loc -> with "paren"     $ putDataAt "inner"  a                                                  >> putData loc
    AST_ArraySub a b c loc -> with "subscript" $ putDataAt "header" a  >> putComments        b >> putDataAt "params" c >> putData loc
    AST_FuncCall a b c loc -> with "funcCall"  $ putDataAt "header" a  >> putComments        b >> putDataAt "params" c >> putData loc
    AST_MetaEval a     loc -> with "metaEval"  $ putDataAt "inner"  a                                                  >> putData loc
    AST_Data     a b   loc -> with "data"      $ putComments        a  >> putUStrList (map unComment b)                >> putData loc
    AST_Dict     a b c loc -> with "container" $ place (ostr        a) >> putComments        b >> putDataAt "params" c >> putData loc
    AST_Array    a b   loc -> with "container" $ place (ostr  "array") >> putDataAt "bounds" a >> putDataAt "params" b >> putData loc
    AST_Struct   a b   loc -> with "container" $ place (ostr "struct") >> putData            a >> putDataAt "params" b >> putData loc
    AST_Lambda   a b c loc -> with "container" $ place (ostr        a) >> putDataAt "params" b >> putDataAt "script" c >> putData loc
  structToData = reconstruct $ msum $
    [ tryWith "object"    $ liftM2 AST_Literal   this                                                       getData
    , tryWith "assign"    $ liftM4 AST_Assign   (getDataAt "to")    (getDataAt "op")   (getDataAt "from")   getData
    , tryWith "equation"  $ liftM4 AST_Equation (getDataAt "left")  (getDataAt "op")   (getDataAt "right")  getData
    , tryWith "prefix"    $ liftM3 AST_Prefix   (getDataAt "op")    (getDataAt "right")                     getData
    , tryWith "paren"     $ liftM2 AST_Paren    (getDataAt "inner")                                         getData
    , tryWith "subscript" $ liftM4 AST_ArraySub (getDataAt "header") getComments        getData             getData
    , tryWith "funcCall"  $ liftM4 AST_FuncCall (getDataAt "header") getComments       (getDataAt "params") getData
    , tryWith "metaEval"  $ liftM2 AST_MetaEval (getDataAt "inner")                                         getData
    , tryWith "data"      $ liftM3 AST_Data      getComments        (fmap (fmap Com) getUStrList)           getData
    , tryWith "container" $ do
        kind <- fmap uchars $ getUStrData "constructor type"
        let mkDict = liftM3 (AST_Dict (ustr kind)) (getComments) (getDataAt "params") getData
        case kind of
          kind | elem kind (words "set list dict intmap") -> mkDict
          "array"  -> liftM3 AST_Array  (getDataAt "bounds") (getDataAt "params") getData
          "struct" -> liftM3 AST_Struct  getData             (getDataAt "params") getData
          kind     -> case maybeFromUStr (ustr kind) of
            Just kind -> liftM3 (AST_Lambda kind) (getDataAt "params") (getDataAt "script") getData
            Nothing   -> fail "unknown type of object expression contructor"
    , fail "object expression"
    ]

instance Structured AST_Script where
  dataToStruct a = deconstruct $ case a of
    AST_Comment      a           -> putComments a
    AST_EvalObject   a b     loc -> with "equation" $  putComments          b                                                                         >> putData loc
    AST_IfThenElse   a b c d loc -> with "if"       $  putComments          a >> putDataAt "condition" b >> putDataAt "then"  c >> putDataAt "else" d >> putData loc
    AST_TryCatch     a b c   loc -> with "try"      $ do
      putData a
      if unComment b == nil then return () else putDataAt "varName" b >> putDataAt "catch" c
      putData loc
    AST_ForLoop      a b c   loc -> with "for"      $ putDataAt "varName"   a >> putDataAt "iterator" b >> putDataAt "script" c                       >> putData loc
    AST_WhileLoop    a b     loc -> with "while"    $ putDataAt "iterator"  a >> putDataAt "script"   b                                               >> putData loc
    AST_ContinueExpr a b c   loc -> with (if a then "continue" else "break") $ putComments b            >> putData c                                  >> putData loc
    AST_ReturnExpr   a b     loc -> with (if a then "return"   else "throw") $ putData     b                                                          >> putData loc
    AST_WithDoc      a b     loc -> with "with"     $ putDataAt "reference" a >> putDataAt "script"   b                                               >> putData loc
  structToData = reconstruct $ msum $
    [ fmap AST_Comment getComments
    , tryWith "equation" $ liftM3 AST_EvalObject (getDataAt "equation")   getComments                                                  getData
    , tryWith "if"       $ liftM5 AST_IfThenElse  getComments            (getDataAt "condition") (getDataAt "then") (getDataAt "else") getData
    , tryWith "try"      $ do
        script <- getDataAt "script"
        str    <- getDataAt "varName"
        catch  <- if unComment str == nil then return [] else getDataAt "catch"
        loc    <- getData
        return (AST_TryCatch script str catch loc)
    , tryWith "for"      $ liftM4 AST_ForLoop    (getDataAt "varName")   (getDataAt "iterator") (getDataAt "script")                   getData
    , tryWith "while"    $ liftM3 AST_WhileLoop  (getDataAt "iterator")  (getDataAt "script")                                          getData
    , tryWith "continue" $ getContinue True
    , tryWith "break"    $ getContinue False
    , tryWith "return"   $ getReturn   True
    , tryWith "throw"    $ getReturn   False
    , tryWith "with"     $ liftM3 AST_WithDoc    (getDataAt "reference") (getDataAt "script")                                          getData
    , fail "script expression"
    ]
    where
      getContinue tf = liftM3 (AST_ContinueExpr tf) getComments (getDataAt "condition") getData
      getReturn   tf = liftM2 (AST_ReturnExpr   tf)             (getDataAt "object")    getData

instance Structured Reference where
  dataToStruct = deconstruct . place . ORef
  structToData = reconstruct $ do
    a <- this
    case a of
      ORef a -> return a
      _ -> fail "reference"

instance Structured GlobUnit  where
  dataToStruct a = T.Leaf $ OString $ case a of
    Wildcard -> ustr "$*"
    AnyOne   -> ustr "$?"
    Single s -> s
  structToData = reconstruct $ do
    str <- getUStrData "glob-unit"
    return $ case uchars str of
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
    ObjAnyX        -> putUStrData "any"
    ObjMany        -> putUStrData "many"
    ObjAny1        -> putUStrData "any1"
    ObjEQ      a   -> with "equals"  (place a)
    ObjType    a   -> putDataAt "type" a
    ObjBounded a b -> with "bounded" (putData a >> putDataAt "to" b)
    ObjList    a b -> with "list"    (putDataAt "op" a >> putData b)
    ObjNameSet a b -> with "nameSet" (putDataAt "op" a >> putData (S.elems b))
    ObjIntSet  a b -> with "intSet" $ putDataAt "op" a >> place (OList (map (OInt . fromIntegral) (IS.elems b)))
    ObjElemSet a b -> with "elemSet" (putDataAt "op" a >> putData (S.elems b))
    ObjChoice  a b -> with "choice"  (putDataAt "op" a >> putData (S.elems b))
    ObjLabel   a b -> with "label"   (putDataAt "name" a >> putData b)
    ObjFailIf  a b -> with "require" (putDataAt "message" a >> putData b)
    ObjNot     a   -> with "not"     (putData a)
  structToData = reconstruct $ msum $
    [ getUStrData "src/Dao/Object/Struct.hs:302:pattern" >>= \a -> case uchars a of
        "any"  -> return ObjAnyX
        "many" -> return ObjMany
        "any1" -> return ObjAny1
        _      -> assumePValue Backtrack
    , tryWith "equals"  $ liftM  ObjEQ       this
    , tryWith "type"    $ liftM  ObjType     getData
    , tryWith "bounded" $ liftM2 ObjBounded  getData           (getDataAt "to")
    , tryWith "list"    $ liftM2 ObjList    (getDataAt "op")    getData
    , tryWith "nameSet" $ liftM2 ObjNameSet (getDataAt "op")   (fmap S.fromList  getData)
    , tryWith "intSet"  $ liftM2 ObjIntSet  (getDataAt "op")   (fmap IS.fromList getData)
    , tryWith "elemSet" $ liftM2 ObjElemSet (getDataAt "op")   (fmap S.fromList  getData)
    , tryWith "choice"  $ liftM2 ObjChoice  (getDataAt "op")   (fmap S.fromList  getData)
    , tryWith "label"   $ liftM2 ObjLabel   (getDataAt "name")  getData
    , tryWith "require" $ liftM2 ObjFailIf  (getDataAt "message") getData
    , tryWith "not"     $ liftM  ObjNot     getData
    , mplus this (return ONull) >>= \a -> fail "pattern"
    ]

instance Structured ObjSetOp where
  dataToStruct a = deconstruct $ place $ ostr $ case a of
    ExactSet  -> "exact"
    AnyOfSet  -> "any"
    AllOfSet  -> "all"
    OnlyOneOf -> "only"
    NoneOfSet -> "none"
  structToData = reconstruct $ getUStrData "pattern set operator" >>= \a -> case uchars a of
    "exact" -> return ExactSet
    "any"   -> return AnyOfSet
    "all"   -> return AllOfSet
    "only"  -> return OnlyOneOf
    "none"  -> return NoneOfSet
    a       -> fail "pattern set operator"

instance Structured TopLevelEventType where
  dataToStruct a = deconstruct $ place $ ostr $ case a of
    BeginExprType -> "BEGIN"
    EndExprType   -> "END"
    ExitExprType  -> "EXIT"
  structToData = reconstruct $ getUStrData "event type" >>= \a -> case uchars a of
    "BEGIN" -> return BeginExprType
    "END"   -> return EndExprType
    "EXIT"  -> return ExitExprType
    "QUIT"  -> return ExitExprType
    _       -> fail "top-level event type"

instance Structured AST_TopLevel where
  dataToStruct a = deconstruct $ case a of
    AST_TopComment a           -> putComments a
    AST_Attribute  a b     loc -> with "attribute" $ putDataAt "type"   a >> putDataAt "value"    b                                                 >> putData loc
    AST_TopFunc    a b c d loc -> with "function"  $ putComments        a >> putDataAt "name"     b >> putDataAt "params" c >> putDataAt "script" d >> putData loc
    AST_TopScript  a       loc -> with "directive" $ putData            a                                                                           >> putData loc
    AST_TopLambda  a b c   loc -> with "lambda"    $ putDataAt "type"   a >> putDataAt "function" b >> putDataAt "script" c                         >> putData loc
    AST_Event      a b c   loc -> with "event"     $ putDataAt "type"   a >> putComments          b >> putDataAt "script" c
  structToData = reconstruct $ msum $
    [ liftM AST_TopComment getComments
    , with "attribute" $ liftM3 AST_Attribute (getDataAt "type")   (getDataAt "value")                                              getData
    , with "function"  $ liftM5 AST_TopFunc    getComments         (getDataAt "name")     (getDataAt "params") (getDataAt "script") getData
    , with "directive" $ liftM2 AST_TopScript  getData                                                                              getData
    , with "lambda"    $ liftM4 AST_TopLambda (getDataAt "type")   (getDataAt "function") (getDataAt "script")                      getData
    , with "event"     $ liftM4 AST_Event     (getDataAt "type")    getComments           (getDataAt "script")                      getData
    , fail "top-level directive"
    ]

putIntermediate :: (Intermediate obj ast, Structured ast) => String -> obj -> T.Tree Name Object
putIntermediate typ a = deconstruct $ case fromInterm a of
  []  -> return ()
  [a] -> putTree (dataToStruct a)
  _   -> error ("fromInterm returned more than one possible intermediate data structure for "++typ)

getIntermediate :: (Intermediate obj ast, Structured ast) => String -> T.Tree Name Object -> PValue UpdateErr obj
getIntermediate typ = reconstruct $ do
  a <- getData
  case toInterm a of
    []  -> fail ("could not convert "++typ++" expression to it's intermediate")
    [a] -> return a
    _   -> error ("toInterm returned more than one possible abstract syntax tree for "++typ)

toplevel_intrm = "top-level intermedaite node"
script_intrm = "script intermedaite node"
object_intrm = "object intermedaite node"

instance Structured TopLevelExpr where
  dataToStruct = putIntermediate toplevel_intrm
  structToData = getIntermediate toplevel_intrm

instance Structured ScriptExpr where
  dataToStruct = putIntermediate script_intrm
  structToData = getIntermediate script_intrm

instance Structured ObjectExpr where
  dataToStruct = putIntermediate object_intrm
  structToData = getIntermediate object_intrm

instance (Ord a, Enum a, Structured a) => Structured (Es.Inf a) where
  dataToStruct a = deconstruct $ case a of
    Es.PosInf  -> putUStrData "+inf"
    Es.NegInf  -> putUStrData "-inf"
    Es.Point a -> putData a
  structToData = reconstruct $ msum $
    [ fmap Es.Point getData
    , getUStrData msg >>= \a -> case uchars a of
        "+inf" -> return Es.PosInf
        "-inf" -> return Es.NegInf
    , fail msg
    ]
    where { msg = "unit of a segment of an enum set" }

instance (Ord a, Enum a, Bounded a, Structured a) => Structured (Es.Segment a) where
  dataToStruct a = deconstruct $
    mplus (maybe mzero return (Es.singular a) >>= putDataAt "at") $
      maybe mzero return (Es.plural a) >>= \ (a, b) -> putDataAt "to" a >> putDataAt "from" b
  structToData = reconstruct $ msum $
    [ getDataAt "to" >>= \a -> getDataAt "from" >>= \b -> return (Es.segment a b)
    , fmap Es.single (getDataAt "at")
    , mplus this (return ONull) >>= \o -> fail "unit segment of an enum set"
    ]

instance (Ord a, Enum a, Bounded a, Es.InfBound a, Structured a) => Structured (Es.Set a) where
  dataToStruct a = deconstruct (putData (Es.toList a))
  structToData = reconstruct (fmap Es.fromList getData)

