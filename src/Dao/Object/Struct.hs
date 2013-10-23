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

import           Control.Applicative
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
  dataToStruct a = deconstruct $ case a of
    InlineComment  a -> putDataAt "inline"  (ostr a)
    EndlineComment a -> putDataAt "endline" (ostr a)
  structToData = reconstruct $ do
    a <- this
    msum $
      [ fmap InlineComment  (getDataAt "inline")
      , fmap EndlineComment (getDataAt "endline")
      , fail "must be a comment string or typed comment string"
      ]

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

instance Structured Name where
  dataToStruct = ustrToStruct
  structToData = fmap fromUStr . structToUStr "a valid label"

instance Structured UpdateOp where
  dataToStruct = ustrToStruct
  structToData = structToUStr "assignment operator"

instance Structured PrefixOp where
  dataToStruct = ustrToStruct
  structToData = structToUStr "unary prefix operator"
  
instance Structured InfixOp where
  dataToStruct = ustrToStruct
  structToData = structToUStr "binary infix operator"

instance Structured CoreType   where
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
        with "line"   (place $ OInt $ fromIntegral $ startingLine   loc)
        with "column" (place $ OInt $ fromIntegral $ startingColumn loc)
      if   startingLine   loc == endingLine   loc
        && startingColumn loc == endingColumn loc
      then  return ()
      else  with "to"   $ do
              with "line"   (place $ OInt $ fromIntegral $ endingLine     loc)
              with "column" (place $ OInt $ fromIntegral $ endingColumn   loc)
  structToData = reconstruct $ flip mplus (return LocationUnknown) $ tryWith "location" $ do
    let getPos = pure (,) <*> getDataAt "line" <*> getDataAt "column"
    (a,b) <- with "from" getPos
    flip mplus (return (Location a b a b)) $ tryWith "to" $ do
      (c,d) <- getPos
      return (Location a b c d)

--instance Structured RefInfixOp where
--  dataToStruct a = deconstruct $ place $ ostr $ case a of
--    DOT   -> "."
--    POINT -> "->"
--  structToData   = reconstruct $ tryGetData >>= \s -> case uchars s of
--    "."  -> return DOT
--    "->" -> return POINT
--    s    -> fail ("expecting reference infix operator, instead found string "++show s)

instance Structured AST_Ref where
  dataToStruct a = deconstruct $ case a of
    AST_RefNull          -> place ONull
    AST_Ref r comref loc -> putData r >>
      with "refExpr" (putListWith putData comref >> putData loc)
  structToData = reconstruct $ this >>= \o -> case o of
    ONull -> return AST_RefNull
    _     -> do
      ref <- getData
      loc <- getData
      multi <- with "refExpr" $ mplus (fmap Just (getListWith getData)) (return Nothing)
      case multi of
        Nothing    -> return (AST_Ref ref []    loc)
        Just multi -> return (AST_Ref ref multi loc)

instance Structured AST_QualRef where
  dataToStruct ref = deconstruct $ case ref of
    AST_Qualified q com ref loc -> do
      let qref addr = with addr $ putComments com >> putData ref >> putData loc
      qref $ case q of
        LOCAL  -> "local"
        QTIME  -> "qtime"
        GLODOT -> "globalDot"
        STATIC -> "static"
        GLOBAL -> "global"
    AST_Unqualified ref -> with "unqualified" $ putData ref
  structToData = reconstruct $ msum $ map qref tries ++
    [with "unqualfied" $ pure AST_Unqualified <*> getData] where
      tries = zip (words "local qtime globalDot static global") $
        [LOCAL, QTIME, GLODOT, STATIC, GLOBAL]
      qref (addr, q) = with addr $ pure (AST_Qualified q) <*> getComments <*> getData <*> getData

instance Structured AST_ObjList where
  dataToStruct (AST_ObjList coms lst loc) = deconstruct $ with "objList" (putComments coms >> putData lst >> putData loc)
  structToData = reconstruct $ with "objList" (pure AST_ObjList <*> getComments <*> getData <*> getData)

instance Structured AST_LValue where
  dataToStruct (AST_LValue o) = deconstruct (with "to" (putData o))
  structToData = reconstruct (with "to" $ pure AST_LValue <*> getData)

instance Structured a => Structured (AST_TyChk a) where
  dataToStruct o = deconstruct $ case o of
    AST_NotChecked o              -> putData o
    AST_Checked    o coms obj loc -> putData o >> putDataAt "colon" coms >> putDataAt "typeExpr" obj >> putData loc
  structToData   = reconstruct $ msum $
    [ getDataAt "colon" >>= \coms -> getDataAt "typeExpr" >>= \obj -> getData >>= \o -> fmap (AST_Checked o coms obj) getData
    , fmap AST_NotChecked getData
    ]

instance Structured AST_Param where
  dataToStruct o = deconstruct $ case o of
    AST_NoParams          -> place ONull
    AST_Param coms nm loc -> do
      with "passByRef" (maybe (place ONull) (putComments >=> \ () -> place OTrue) coms)
      putData nm >> putData loc
  structToData = reconstruct $ msum $
    [ let getfn = this >>= \o -> case o of
            ONull -> return Nothing
            OTrue -> fmap Just getComments
            _     -> fail "expecting boolean with optional comments"
      in  pure AST_Param <*> with "passByRef" getfn <*> getData <*> getData
    , this >>= \o -> case o of
        ONull -> return AST_NoParams
        _     -> fail "expecting function parameter declaration"
    ]

instance Structured AST_ParamList where
  dataToStruct (AST_ParamList lst loc) = deconstruct $ putData lst >> putData loc
  structToData = reconstruct $ liftM2 AST_ParamList getData getData

instance Structured AST_StringList where
  dataToStruct o = deconstruct $ case o of
    AST_NoStrings  coms loc -> place ONull >> putData coms >> putData loc
    AST_StringList strs loc -> putData strs >> putData loc
  structToData   = reconstruct $ this >>= \o -> case o of
    ONull -> pure AST_NoStrings  <*> getData <*> getData
    _     -> pure AST_StringList <*> getData <*> getData

instance Structured (Maybe AST_ObjList) where
  dataToStruct a = deconstruct $ case a of
    Nothing -> place ONull
    Just  a -> putData a
  structToData   = reconstruct $ mplus getData $ this >>= \a -> case a of
    ONull -> return Nothing
    _     -> fail "expecting either null value or optional AST_ObjList expression"

instance Structured AST_OptObjList where
  dataToStruct a = deconstruct $ case a of
    AST_NoObjList coms -> with "noParams" (putComments coms)
    AST_OptObjList o _ -> putDataAt "params" o
  structToData   = reconstruct $ msum $
    [ with "noParams" $ fmap AST_NoObjList getComments
    , AST_OptObjList <$> getDataAt "params" <*> getComments
    ]

instance Structured AST_Paren where
  dataToStruct (AST_Paren a loc) = deconstruct $ with "paren" $ putData a >> putData loc
  structToData = reconstruct $ with "paren" $ pure AST_Paren <*> getData <*> getData

instance Structured AST_Object where
  dataToStruct a = deconstruct $ case a of
    AST_Void                   -> place ONull
    AST_ObjQualRef a           -> putData a
    AST_ObjParen   a           -> putData a
    AST_Literal    a       loc -> with "literal"   $ place a >> putData loc
    AST_Assign     a b c   loc -> with "assign"    $ putDataAt "to"     a  >> putDataAt "op"     b >> putDataAt "from"   c >> putData loc
    AST_Equation   a b c   loc -> with "equation"  $ putDataAt "left"   a  >> putDataAt "op"     b >> putDataAt "right"  c >> putData loc
    AST_Prefix     a b     loc -> with "prefix"    $ putDataAt "op"     a  >> putDataAt "right"  b                         >> putData loc
    AST_ArraySub   a b     loc -> with "subscript" $ putDataAt "header" a  >>                         putDataAt "params" b >> putData loc
    AST_FuncCall   a b     loc -> with "funcCall"  $ putDataAt "header" a  >>                         putDataAt "params" b >> putData loc
    AST_Init       a b c   loc -> with "initExpr"  $ putDataAt "header" a  >> putDataAt "params" b >> putDataAt "elems"  c >> putData loc
    AST_Struct     a b     loc -> with "structExpr"$ putDataAt "header" a  >> putDataAt "params" b >> putData loc
    AST_Lambda     a b     loc -> with "lambdaExpr"$                          putDataAt "params" a >> putDataAt "script" b >> putData loc
    AST_Func       a b c d loc -> with "funcExpr"  $ putComments        a  >> putDataAt "name"   b >> putDataAt "params" c >> putDataAt "script" d >> putData loc
    AST_Rule       a b     loc -> with "ruleExpr"  $                          putDataAt "params" a >> putDataAt "script" b >> putData loc
    AST_MetaEval   a       loc -> with "metaEval"  $ putDataAt "inner"  a                                                  >> putData loc
  structToData =   reconstruct $ msum $
    [ AST_ObjQualRef <$> getData
    , AST_ObjParen   <$> getData
    , tryWith "literal"   $ pure AST_Literal     <*> this               <*> getData
    , tryWith "assign"    $ pure AST_Assign      <*> getDataAt "to"     <*> getDataAt "op"     <*> getDataAt "from"   <*> getData
    , tryWith "equation"  $ pure AST_Equation    <*> getDataAt "left"   <*> getDataAt "op"     <*> getDataAt "right"  <*> getData
    , tryWith "prefix"    $ pure AST_Prefix      <*> getDataAt "op"     <*> getDataAt "right"  <*> getData
    , tryWith "subscript" $ pure AST_ArraySub    <*> getDataAt "header" <*>                        getData            <*> getData
    , tryWith "funcCall"  $ pure AST_FuncCall    <*> getDataAt "header" <*>                        getDataAt "params" <*> getData
    , tryWith "initExpr"  $ pure AST_Init        <*> getDataAt "header" <*> getDataAt "params" <*> getDataAt "elems"  <*> getData
    , tryWith "structExpr"$ pure AST_Struct      <*> getDataAt "header" <*> getDataAt "params" <*> getData
    , tryWith "lambdaExpr"$ pure AST_Lambda                             <*> getDataAt "params" <*> getDataAt "script" <*> getData
    , tryWith "funcExpr"  $ pure AST_Func        <*> getComments        <*> getDataAt "name"   <*> getDataAt "params" <*> getDataAt "script" <*> getData
    , tryWith "ruleExpr"  $ pure AST_Rule                               <*> getDataAt "params" <*> getDataAt "script" <*> getData
    , tryWith "metaEval"  $ pure AST_MetaEval    <*> getDataAt "inner"  <*> getData
    , this >>= \o -> case ONull of
        ONull -> return AST_Void
        _     -> fail "object expression"
    ]

instance Structured AST_CodeBlock where
  dataToStruct a = deconstruct (putDataAt "block" (getAST_CodeBlock a))
  structToData = reconstruct (fmap AST_CodeBlock (getDataAt "block"))

instance Structured AST_If where
  dataToStruct (AST_If ifn thn loc) = deconstruct $
    with "ifExpr" $ putData ifn >> putDataAt "then" thn >> putData loc
  structToData = reconstruct $ with "ifExpr" $ liftM3 AST_If getData (getDataAt "then") getData

instance Structured AST_Else where
  dataToStruct (AST_Else coms ifn loc) = deconstruct $
    with "elseIfExpr" $ putData coms >> putData ifn >> putData loc
  structToData = reconstruct $ with "elseIfExpr" $ liftM3 AST_Else getData getData getData

instance Structured AST_IfElse where
  dataToStruct (AST_IfElse ifn els coms deflt loc) = deconstruct $ with "ifExpr" $
    putData ifn >> putListWith putData els >> putData coms >> maybe (return ()) (putDataAt "elseExpr") deflt >> putData loc
  structToData = reconstruct $ with "ifExpr" $
    liftM5 AST_IfElse getData (getListWith getData) getData (optional (getDataAt "elseExpr")) getData

instance Structured AST_While where
  dataToStruct (AST_While (AST_If ifn thn loc)) = deconstruct $ with "whileExpr" $
    putData ifn >> putDataAt "script" thn >> putData loc
  structToData = reconstruct $ with "whileExpr" $
    liftM3 (\a b c -> AST_While (AST_If a b c)) getData (getDataAt "script") getData

--instance Structured AST_ElseIf where
--  dataToStruct a = deconstruct $ with "elseExpr" $ case a of
--    AST_NullElseIf                 -> place ONull
--    AST_Else   coms block      loc -> putComments coms >> putDataAt "thenExpr" block >> putData loc
--    AST_ElseIf obj  block next loc -> putDataAt "ifExpr" obj >> putData block >> putData next >> putData loc
--  structToData   = reconstruct $ with "elseExpr" $ msum
--    [ do  n <- this
--          case n of
--            ONull -> return AST_NullElseIf
--            _     -> mzero
--    , pure AST_ElseIf <*> (getDataAt "ifExpr") <*> getDataAt "thenExpr" <*> getData <*> getData
--    , pure AST_Else   <*> getComments          <*> getData              <*> getData
--    , fail "expecting if-then-else expression"
--    ]

instance Structured AST_Script where
  dataToStruct a = deconstruct $ case a of
    AST_Comment      a         -> putComments a
    AST_EvalObject   a b   loc -> with "equation"  $               putComments           b                                      >> putData loc
    AST_TryCatch     a b c loc -> with "tryExpr"   $  putData a >> putMaybeAt "varName"  b >> putMaybeAt "catchBlock" c         >> putData loc
    AST_ForLoop      a b c loc -> with "forExpr"   $  putDataAt "varName"   a >> putDataAt "iterator" b >> putDataAt "script" c >> putData loc
    AST_ContinueExpr a b c loc -> with (if a then "continueExpr" else "breakExpr") $ putComments b      >> putData c            >> putData loc
    AST_ReturnExpr   a b   loc -> with (if a then "returnExpr"   else "throwExpr") $ putData     b                              >> putData loc
    AST_WithDoc      a b   loc -> with "withExpr"  $  putDataAt  "reference" a >> putDataAt "script"   b                        >> putData loc
    AST_IfThenElse   a         -> putData a
    AST_WhileLoop    a         -> putData a
  structToData = reconstruct $ msum $
    [ fmap AST_Comment getComments
    , tryWith "equation"    $ pure AST_EvalObject <*> getDataAt "equation"  <*> getComments            <*> getData
    , tryWith "tryExpr"     $ pure AST_TryCatch   <*> getDataAt "script"    <*> getMaybeAt "varName"   <*> getMaybe           <*> getData
    , tryWith "forExpr"     $ pure AST_ForLoop    <*> getDataAt "varName"   <*> getDataAt  "iterator"  <*> getDataAt "script" <*> getData
    , tryWith "continueExpr"$ getContinue True
    , tryWith "breakExpr"   $ getContinue False
    , tryWith "returnExpr"  $ getReturn   True
    , tryWith "throwExpr"   $ getReturn   False
    , tryWith "withExpr"    $ pure AST_WithDoc    <*> getDataAt "reference" <*> getDataAt "script"     <*> getData
    , guardBranch "ifExpr" >> liftM AST_IfThenElse getData
    , guardBranch "whileExpr" >> liftM AST_WhileLoop  getData
    , fail "script expression"
    ]
    where
      getContinue tf = pure (AST_ContinueExpr tf) <*> getComments        <*> getDataAt "condition"     <*> getData
      getReturn   tf = pure (AST_ReturnExpr   tf) <*> getDataAt "object" <*> getData

instance Structured QualRef where
  dataToStruct = deconstruct . place . ORef
  structToData = reconstruct $ do
    a <- this
    case a of
      ORef a -> return a
      _      -> fail "reference"

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

--instance Structured ObjSetOp where
--  dataToStruct a = deconstruct $ place $ ostr $ case a of
--    ExactSet  -> "exact"
--    AnyOfSet  -> "any"
--    AllOfSet  -> "all"
--    OnlyOneOf -> "only"
--    NoneOfSet -> "none"
--  structToData = reconstruct $ getUStrData "pattern set operator" >>= \a -> case uchars a of
--    "exact" -> return ExactSet
--    "any"   -> return AnyOfSet
--    "all"   -> return AllOfSet
--    "only"  -> return OnlyOneOf
--    "none"  -> return NoneOfSet
--    a       -> fail "pattern set operator"

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
    AST_Attribute  a b     loc -> with "attribute" $ putDataAt "type" a >> putDataAt "value"    b                                                 >> putData loc
    AST_TopScript  a       loc -> with "directive" $ putData          a                                                                           >> putData loc
    AST_Event      a b c   loc -> with "event"     $ putDataAt "type" a >> putComments          b >> putDataAt "script" c
  structToData = reconstruct $ msum $
    [ pure AST_TopComment <*> getComments
    , with "attribute" $ pure AST_Attribute <*> getDataAt "type" <*> getDataAt "value"    <*> getData
    , with "directive" $ pure AST_TopScript <*> getData          <*> getData
    , with "event"     $ pure AST_Event     <*> getDataAt "type" <*> getComments          <*> getDataAt "script" <*> getData
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

