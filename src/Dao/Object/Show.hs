-- "src/Dao/Object/Show.hs"  provides functions for pretty printing Dao
-- objects, Dao scripts, and Dao programs.
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


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.Show where

import           Dao.String
import           Dao.PPrint
import           Dao.Token
import           Dao.Object
import           Dao.Pattern
import           Dao.EnumSet
import qualified Dao.Tree as T

import           Control.Monad
import           Control.Monad.State

import           Numeric

import           Data.Maybe (fromMaybe, maybeToList)
import           Data.List
import           Data.Ratio
import           Data.Complex
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Set as S
import qualified Data.ByteString           as Byt (pack, unpack, take, drop, null)
-- import qualified Data.ByteString.Base64    as B64 (encode)
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Int
import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Array.IArray
import           Data.Time hiding (parseTime)

import Debug.Trace

----------------------------------------------------------------------------------------------------

-- | A commonly used pattern, like 'pHeader' but the contents of it is always a list of items which
-- can be pretty-printed by the given @(o -> 'PPrint' ())@ function.
pContainer :: String -> (o -> PPrint ()) -> [o] -> PPrint ()
pContainer label prin ox = pList (pString label) "{ " ", " " }" (map prin ox)

pMapAssoc :: Show a => (a, Object) -> PPrint ()
pMapAssoc (a, obj) = pWrapIndent [pShow a, pString " = ", pPrint obj]

instance PPrintable T_tree where
  pPrint t = case t of
    T.Void            -> pString "struct{}"
    T.Leaf       o    -> leaf o
    T.Branch       ox -> pList (pString "struct") "{ " ", " " }" (branch ox)
    T.LeafBranch o ox -> pList (leaf o) " { " ", " " }" (branch ox)
    where
      leaf o = pWrapIndent [pString "struct(", pPrint o, pString ")"]
      branch = map (\ (lbl, obj) -> pMapAssoc (lbl, OTree obj)) . M.assocs

instance PPrintable Pattern where { pPrint = pShow }

instance PPrintable Object where
  pPrint obj = case obj of
    ONull            -> pString "null"
    OTrue            -> pString "true"
    OType      o     -> pPrint o
    OInt       o     -> pShow o
    OWord      o     -> pString (show o++"U")
    OLong      o     -> pString (show o++"L")
    OFloat     o     -> pString (show o++"f")
    ORatio     o     -> pString ("ratio("++show (numerator o)++"/"++show (denominator o)++")")
    OComplex  (r:+i) -> pString (show r++'+':show i++"j")
    ODiffTime  o     -> pShow o
    OTime      o     -> pString ("time("++show o++")")
    OChar      o     -> pShow o
    OString    o     -> pShow o
    ORef       o     -> pPrint o
    OPair     (a, b) -> pList (pString "pair") "(" ", " ")" [pPrint a, pPrint b]
    OList      ox    -> if null ox then pString "list{}" else pContainer "list " pPrint ox
    OSet       o     -> if S.null o then pString "set{}" else pContainer "set "  pPrint (S.elems o)
    OArray     o     -> do
      let (lo, hi)   = bounds o
          showBounds = pList (pString "array") "(" ", " ")" [pShow lo, pShow hi]
      pList showBounds " { " ", " " }" (map pPrint (elems o))
    ODict      o     ->
      if M.null o then pString "dict{}" else pContainer "dict " pMapAssoc (M.assocs o)
    OIntMap    o     ->
      if I.null o then pString "intmap{}" else pContainer "intmap " pMapAssoc (I.assocs o)
    OTree      o     -> pPrint o
    OPattern   o     -> pPrint o
    ORule      o     -> pPrint o
    OScript    o     -> pPrint o
    OBytes     o     ->
      if B.null o
        then  pString "data{}"
        else  pClosure (pString "data ") "{ " " }" (map pString (b64Encode o))

----------------------------------------------------------------------------------------------------

instance PPrintable Reference where
  pPrint ref = case ref of
    IntRef     w      -> pString ('$' : show w)
    LocalRef   nm     -> pPrint nm
    StaticRef  nm     -> pInline [pString "static ", pRef [nm]]
    QTimeRef   rx     -> pInline [pString "qtime ", pRef rx]
    GlobalRef  rx     -> pRef rx
    ProgramRef nm ref -> pInline [pString ("program("++show nm++", "), pPrint ref, pString ")"]
    FileRef    p   rx -> pInline [pString ("file("++show p++", "), pRef rx, pString ")"]
    Subscript  rx   o -> pInline [pPrint rx, pString "[", pPrint o, pString "]"]
    MetaRef    ref    -> pInline [pString "$(", pPrint ref, pString ")"]
    where
      pRef rx = pString (intercalate "." (map prin rx))
      prin r  = case uchars r of
        []                                                      -> "$\"\""
        r:rx | isAlpha r && (null rx || or (map isAlphaNum rx)) -> r:rx
        rx                                                      -> '$' : show rx

instance PPrintable Comment where
  pPrint com = do
    case com of
      EndlineComment c -> pString ("//"++uchars c) >> pForceNewLine
      InlineComment  c -> pGroup True $ pInline $
        concat [[pString " /*"], map pString (lines (uchars c)), [pString "*/ "]]

pPrintComWith :: (a -> PPrint ()) -> Com a -> PPrint ()
pPrintComWith prin com = case com of
  Com          c    -> prin c
  ComBefore ax c    -> pcom ax >> prin c
  ComAfter     c bx -> prin c >> pcom bx
  ComAround ax c bx -> pcom ax >> prin c >> pcom bx
  where { pcom = pInline . map pPrint }

pListOfComsWith :: (a -> PPrint ()) -> [Com a] -> PPrint ()
pListOfComsWith prin = sequence_ . map (pPrintComWith prin)

pListOfComs :: PPrintable a => [Com a] -> PPrint ()
pListOfComs = pListOfComsWith pPrint

instance PPrintable a => PPrintable (Com a) where { pPrint = pPrintComWith pPrint }

-- 'pPrintComWith' wasn't good enough for this, because the comments might occur after the header
-- but before the opening bracket.
pPrintComSubBlock :: PPrint () -> Com [Com ScriptExpr] -> PPrint ()
pPrintComSubBlock header c = case c of
  Com          c    -> run [] c []
  ComBefore bx c    -> run bx c []
  ComAfter     c ax -> run [] c ax
  ComAround bx c ax -> run bx c ax
  where
    run :: [Comment] -> [Com ScriptExpr] -> [Comment] -> PPrint ()
    run before cx after = case cx of
      [] -> header >> pInline (map pPrint before) >> pString " {}" >> pInline (map pPrint after)
      cx -> do
        pClosure (header >> pInline (map pPrint before)) " { " " }" (map (pGroup True . pPrint) cx)
        pInline (map pPrint after)

pPrintSubBlock :: PPrint () -> [Com ScriptExpr] -> PPrint ()
pPrintSubBlock header px = pPrintComSubBlock header (Com px)

instance PPrintable Rule where
  pPrint rule = do
    let pat = rulePattern rule
    flip pPrintSubBlock (ruleAction rule) $ case pat of
      []  -> pString "rule()" 
      [p] -> pInline [pString "rule ", pPrint p]
      _   -> pList (pString "rule") "(" ", " ")" (map pPrint pat)

instance PPrintable Subroutine where
  pPrint sub = flip pPrintSubBlock (subSourceCode sub) $
    pList (pString "function") "(" ", " ")" (map pPrint (argsPattern sub))

instance PPrintable ScriptExpr where
  pPrint expr = pGroup True $ case expr of
    EvalObject   objXp  coms                    _ ->
      pPrint objXp >> mapM_ pPrint coms >> pString ";"
    IfThenElse   coms   ifXp  thenXp  elseXp    _ -> do
      case ifXp of
        ParenExpr _ obXp _ -> printIfXp obXp
        _                  -> printIfXp ifXp
      case unComment elseXp of
        []                   -> return ()
        [p] -> case unComment p of
          (IfThenElse _ _ _ _ _) -> pEndLine >> pString "else " >> pPrint p
          _                      -> done
        px                       -> done
        where
          printIfXp ifXp = do
            pInline (map pPrint coms)
            pPrintComSubBlock (pWrapIndent [pString "if(", pPrint ifXp, pString ")"]) thenXp
          done = pEndLine >> pPrintComSubBlock (pString "else") elseXp
    TryCatch     cxcScrpXp  cUStr     xcScrpXp  _ -> do
      pPrintComSubBlock (pString "try") cxcScrpXp
      if null xcScrpXp
        then  return ()
        else  pPrintSubBlock (pString "catch " >> pPrint cUStr) xcScrpXp
    ForLoop      cNm        cObjXp    xcScrpXp  _ -> do
      let hdr = do
            pString "for "
            pPrint cNm
            pString " in "
            pList_ "(" "" ")" [pPrint cObjXp]
      pPrintSubBlock hdr xcScrpXp
    ContinueExpr contin     coms      cObjXp    _ -> pWrapIndent $
      [ pString (if contin then "continue" else "break")
      , pInline (map pPrint coms)
      , case unComment cObjXp of
          VoidExpr -> return ()
          _        -> pString " if " >> pPrint cObjXp
      , pString ";"
      ]
    ReturnExpr   retrn                cObjXp    _ -> pWrapIndent $
      [ pString (if retrn then "return " else "throw ")
      , case unComment cObjXp of
          VoidExpr -> return ()
          _        -> pPrint cObjXp
      , pString ";"
      ]
    WithDoc      cObjXp               xcScrpXp  _ ->
      pPrintSubBlock (pString "with " >> pPrint cObjXp) xcScrpXp

instance PPrintable ArithOp1  where { pPrint = pShow }
instance PPrintable ArithOp2  where { pPrint = pShow }
instance PPrintable UpdateOp where { pPrint op = pString (' ':show op++" ") }

complicated :: ObjectExpr -> Bool
complicated obj = case obj of
  Literal (OScript _) _ -> True
  Literal (OTree   _) _ -> True
  Literal (ORule   _) _ -> True
  StructExpr _ _      _ -> True
  Equation   _ _ _    _ -> True
  AssignExpr _ _ _    _ -> True
  ArrayExpr  _ _      _ -> True
  LambdaExpr _ _ _    _ -> True
  ParenExpr  _ _      _ -> True
  _                     -> False

forceParen :: ObjectExpr -> ObjectExpr
forceParen o =
  if complicated o
    then  case o of
            ParenExpr True  _ _   -> o
            ParenExpr False o loc -> ParenExpr True o loc
            _                     -> ParenExpr True (Com o) (getLocation o)
    else  o

instance PPrintable ObjectExpr where
  pPrint expr = case expr of
    VoidExpr                                 -> return ()
    Literal      o                         _ -> pPrint o
    AssignExpr   objXp1  comUpdOp  objXp2  _ -> pWrapIndent $
      [pPrint objXp1, pPrint comUpdOp, pPrint objXp2]
    Equation     objXp1  comAriOp  objXp2  _ -> pWrapIndent $
      [pPrint objXp1, pPrint comAriOp, pPrint objXp2]
    PrefixExpr   ariOp    c_ObjXp          _ -> pPrint ariOp >> pPrint c_ObjXp
    ParenExpr    bool     c_ObjXp          _ ->
      if bool then pWrapIndent [pString "(", pPrint c_ObjXp, pString ")"]
              else pWrapIndent [pPrint c_ObjXp]
    ArraySubExpr objXp    coms     c_ObjXp _ -> pWrapIndent $
      [pPrint objXp, mapM_ pPrint coms, pString "[", pGroup True (pPrint c_ObjXp), pString "]"]
    FuncCall     nm       coms     xcObjXp _ -> do
      pList (pPrint nm >> mapM_ pPrint coms) "(" ", " ")" (map pPrint xcObjXp)
    DictExpr     dict     coms     xcObjXp _ ->
      if null xcObjXp
        then  pString (uchars dict++"{}")
        else  pList (pPrint dict >> mapM_ pPrint coms) " {" ", " " }" (map pPrint xcObjXp)
    ArrayExpr    cxcObjXp xcObjXp          _ -> do
      let tag = pString "array"
          hdr = pPrintComWith (pList tag "(" ", " ")" . map (pGroup True . pPrint)) cxcObjXp
      case xcObjXp of
        [] -> hdr >> pString "{}"
        _  -> pList hdr  " { " ", " " }" (map pPrint xcObjXp)
    StructExpr   cObjXp   xcObjXp          _ ->
      pList (pString "struct " >> printObj cObjXp) "{" ", " "}" (map pPrint xcObjXp) where
        printObj obj =
          if complicated (unComment obj)
            then  pInline [pString "(", pPrint cObjXp, pString ")"]
            else  pPrint cObjXp
    DataExpr     com   xcStr               _ ->
      if null xcStr
        then  pString "data{}"
        else  pClosure (pString "data" >> mapM_ pPrint com) "{" "}" (map pPrint xcStr)
    LambdaExpr   typ   ccNmx   xcObjXp     _ -> do
      let hdr = pPrintComWith (pList_ (show typ++"(") ", " ")" . map (pPrintComWith pPrint)) ccNmx
      pPrintSubBlock hdr xcObjXp
    MetaEvalExpr cObjXp                    _ -> pInline [pString "#{", pPrint cObjXp, pString "}#"]

instance PPrintable TopLevelExpr where
  pPrint o = case o of
    Attribute      a b   _ -> pInline [pPrint a, pString "  ", pPrint b, pString ";"]
    ToplevelFunc   a b c _ -> pPrintComWith (pClosure header " { " " }" . map pPrint) c where
      header = pString "function " >> pPrint a >> pList_ "(" ", " ")" (map pPrint b)
    ToplevelScript a     _ -> pPrint a
    TopLambdaExpr  a b c _ -> pClosure header " { " " }" (map pPrint c) where
      header = pShow a >> pPrintComWith (pList_ "(" ", " ")" . map pPrint) b
    EventExpr      a b   _ -> pPrintComWith (pClosure (pShow a) " { " " }" . map pPrint) b

instance PPrintable ObjPat where
  pPrint pat = case pat of
    ObjAnyX                             -> pString "some"
    ObjMany                             -> pString "all"
    ObjAny1                             -> pString "any1"
    ObjEQ       o                       -> pList (pString "eq") "(" "" ")" [pPrint o]
    ObjType     enumSet_TypeID          ->
      pList_ "(" "|" ")" (map pPrint (filter (setMember enumSet_TypeID) [NullType .. BytesType]))
    ObjBounded  loRatio        hiRatio  -> undefined
    ObjList     typeID         oPatx    -> undefined
    ObjNameSet  objSetOp       sSetName -> undefined
    ObjIntSet   objSetOp       isIntSet -> undefined
    ObjElemSet  objSetOp       sSetOPat -> undefined
    ObjChoice   objSetOp       sSetOPat -> undefined
    ObjLabel    name           oPat     -> undefined
    ObjFailIf   ustr           oPat     -> undefined
    ObjNot                     oPat     -> undefined

instance PPrintable TypeID where
  pPrint t = pString $ case t of
    NullType     -> "null"
    TrueType     -> "true"
    TypeType     -> "type"
    IntType      -> "int"
    WordType     -> "word"
    DiffTimeType -> "difftime"
    FloatType    -> "float"
    LongType     -> "long"
    RatioType    -> "ratio"
    ComplexType  -> "complex"
    TimeType     -> "time"
    CharType     -> "char"
    StringType   -> "string"
    PairType     -> "pair"
    RefType      -> "ref"
    ListType     -> "list"
    SetType      -> "set"
    ArrayType    -> "array"
    IntMapType   -> "intmap"
    DictType     -> "dict"
    TreeType     -> "struct"
    PatternType  -> "glob"
    ScriptType   -> "function"
    RuleType     -> "rule"
    BytesType    -> "data"

