-- "src/Dao/Object/PPrint.hs"  provides functions for pretty printing Dao
-- objects, Dao scripts, and Dao programs.
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


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.PPrint where

import           Dao.String
import           Dao.PPrint
import           Dao.Token
import           Dao.Object
import           Dao.Object.AST
import           Dao.Glob
import qualified Dao.EnumSet as Es
import qualified Dao.Tree    as T

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

-- | A commonly used pattern, like 'pClosure' but the contents of it is always a list of items which
-- can be pretty-printed by the given @(o -> 'PPrint' ())@ function.
pContainer :: String -> (o -> PPrint ()) -> [o] -> PPrint ()
pContainer label prin ox = pList (pString label) "{ " ", " " }" (map prin ox)

pMapAssoc :: Show a => (a, Object) -> PPrint ()
pMapAssoc (a, obj) = pWrapIndent [pShow a, pString " = ", pPrint obj]

instance PPrintable T_tree where
  pPrint t = case t of
    T.Void            -> pString "struct"
    T.Leaf       o    -> leaf o
    T.Branch       ox -> pList (pString "struct") "{ " ", " " }" (branch ox)
    T.LeafBranch o ox -> pList (leaf o) " { " ", " " }" (branch ox)
    where
      leaf o = pWrapIndent [pString "struct(", pPrint o, pString ")"]
      branch = map (\ (lbl, obj) -> pMapAssoc (lbl, OTree obj)) . M.assocs

instance PPrintable Glob where { pPrint = pShow }

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
    OTime      o     -> pString ("date "++show o)
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
    OGlob      o     -> pPrint o
    OScript    o     -> pPrint o
    OBytes     o     ->
      if B.null o
        then  pString "data{}"
        else  pClosure (pString "data ") "{ " " }" (map pString (b64Encode o))

----------------------------------------------------------------------------------------------------

-- Statements like "if" or "while" take a condition, and the Dao languages does not require these
-- conditions be enclosed in parenthases. The question is, should there be a space after the "if" or
-- "while" statement? This function resolves that question by checking if an object expression
-- already is enclosed in parentheses, and if so, does not put a space. Otherwise, a space will be
-- printed between the "if" tag or "while" tag and the condition.
class PrecedeWithSpace a where { precedeWithSpace :: a -> Bool }
instance PrecedeWithSpace AST_Object where
  precedeWithSpace o = case o of
    AST_Void             -> False
    AST_Paren    _     _ -> False
    AST_MetaEval _     _ -> False
    AST_Assign   o _ _ _ -> precedeWithSpace o
    AST_Equation o _ _ _ -> precedeWithSpace o
    AST_Prefix   o _   _ -> precedeWithSpace o
    AST_ArraySub o _ _ _ -> precedeWithSpace o
    AST_FuncCall o _ _ _ -> precedeWithSpace o
    _                    -> True
instance PrecedeWithSpace ArithOp1 where
   precedeWithSpace o = case o of
     GLOBALPFX  -> True
     LOCALPFX   -> True
     QTIMEPFX   -> True
     STATICPFX  -> True
     _          -> False
instance PrecedeWithSpace a => PrecedeWithSpace (Com a) where
  precedeWithSpace o = case o of
    Com         b   -> precedeWithSpace b
    ComBefore a b   -> precedeWithSpace a || precedeWithSpace b
    ComAfter    b _ -> precedeWithSpace b
    ComAround a b _ -> precedeWithSpace a || precedeWithSpace b
    -- there should always be a space before a comment.
instance PrecedeWithSpace [Comment] where { precedeWithSpace = not . null }

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
pPrintComSubBlock :: PPrint () -> Com [AST_Script] -> PPrint ()
pPrintComSubBlock header c = case c of
  Com          c    -> run [] c []
  ComBefore bx c    -> run bx c []
  ComAfter     c ax -> run [] c ax
  ComAround bx c ax -> run bx c ax
  where
    run :: [Comment] -> [AST_Script] -> [Comment] -> PPrint ()
    run before cx after = case cx of
      [] -> header >> pInline (map pPrint before) >> pString " {}" >> pInline (map pPrint after)
      cx -> do
        pClosure (header >> pInline (map pPrint before)) " { " " }" (map (pGroup True . pPrint) cx)
        pInline (map pPrint after)

pPrintSubBlock :: PPrint () -> [AST_Script] -> PPrint ()
pPrintSubBlock header px = pPrintComSubBlock header (Com px)

instance PPrintable AST_Script where
  pPrint expr = pGroup True $ case expr of
    AST_Comment             coms -> mapM_ pPrint coms
    AST_EvalObject   objXp  coms                    _ ->
      pPrint objXp >> mapM_ pPrint coms >> pString ";"
    AST_IfThenElse   coms   ifXp  thenXp  elseXp    _ -> do
      case ifXp of
        AST_Paren   obXp _ -> printIfXp obXp
        _                  -> printIfXp ifXp
      case unComment elseXp of
        []                   -> return ()
        [p]                  -> case p of
          (AST_IfThenElse _ _ _ _ _) -> pEndLine >> pString "else " >> pPrint p
          _                          -> done
        px                   -> done
        where
          printIfXp ifXp = do
            pInline (map pPrint coms)
            flip pPrintComSubBlock thenXp $ pWrapIndent $ concat $
              [ [pString "if"]
              , guard(precedeWithSpace coms || precedeWithSpace ifXp) >> [pString " "]
              , [pPrint ifXp]
              ]
          done = pEndLine >> pPrintComSubBlock (pString "else") elseXp
    AST_TryCatch     cxcScrpXp  cUStr     xcScrpXp  _ -> do
      pPrintComSubBlock (pString "try") cxcScrpXp
      if nil == unComment cUStr
        then  pPrint cUStr
        else  pPrintSubBlock (pString "catch " >> pPrint cUStr) xcScrpXp
    AST_ForLoop      cNm        cObjXp    xcScrpXp  _ ->
      pPrintSubBlock (pString "for " >> pPrint cNm >> pString " in " >> pPrint cObjXp) xcScrpXp
    AST_ContinueExpr contin     coms      cObjXp    _ -> pWrapIndent $
      [ pString (if contin then "continue" else "break")
      , pInline (map pPrint coms)
      , case unComment cObjXp of
          AST_Void -> return ()
          _        ->
            pString " if" >> unless (not $ precedeWithSpace cObjXp) (pString " ") >> pPrint cObjXp
      , pString ";"
      ]
    AST_ReturnExpr   retrn                cObjXp    _ -> pWrapIndent $
      [ pString (if retrn then "return " else "throw ")
      , case unComment cObjXp of
          AST_Void -> return ()
          _        -> pPrint cObjXp
      , pString ";"
      ]
    AST_WithDoc      cObjXp               xcScrpXp  _ ->
      pPrintSubBlock (pString "with " >> pPrint cObjXp) xcScrpXp

instance PPrintable ArithOp1  where { pPrint = pShow }
instance PPrintable ArithOp2  where { pPrint = pShow }
instance PPrintable UpdateOp where { pPrint op = pString (' ':show op++" ") }

instance PPrintable AST_Object where
  pPrint expr = case expr of
    AST_Void                                 -> return ()
    AST_Literal      o                         _ -> pPrint o
    AST_Assign   objXp1  comUpdOp  objXp2  _ -> pWrapIndent $
      [pPrint objXp1, pPrint comUpdOp, pPrint objXp2]
    AST_Equation     objXp1  comAriOp  objXp2  _ -> pWrapIndent $
      [pPrint objXp1, pPrint comAriOp, pPrint objXp2]
    AST_Prefix   ariOp    c_ObjXp          _ -> pWrapIndent $ concat $
      [ [pPrint ariOp]
      , guard (precedeWithSpace ariOp) >> [pString " "]
      , [pPrint c_ObjXp]
      ]
    AST_Paren             c_ObjXp          _ -> pWrapIndent [pString "(", pPrint c_ObjXp, pString ")"]
    AST_ArraySub objXp    coms     xcObjXp _ ->
      pList (pPrint objXp >> mapM_ pPrint coms) "[" ", " "]" (map pPrint xcObjXp)
    AST_FuncCall objXp    coms     xcObjXp _ ->
      pList (pPrint objXp >> mapM_ pPrint coms) "(" ", " ")" (map pPrint xcObjXp)
    AST_Dict     dict     coms     xcObjXp _ ->
      if null xcObjXp
        then  pString (uchars dict++"{}")
        else  pList (pPrint dict >> mapM_ pPrint coms) " {" ", " " }" (map pPrint xcObjXp)
    AST_Array    cxcObjXp xcObjXp          _ -> do
      let tag = pString "array"
          hdr = pPrintComWith (pList tag "(" ", " ")" . map (pGroup True . pPrint)) cxcObjXp
      case xcObjXp of
        [] -> hdr >> pString "{}"
        _  -> pList hdr  " { " ", " " }" (map pPrint xcObjXp)
    AST_Struct   cObjXp   xcObjXp          _ ->
      pList (pString "struct" >> printObj cObjXp) "{" ", " "}" (map pPrint xcObjXp) where
        printObj obj = pWrapIndent [pString " ", pInline [pPrint cObjXp]]
    AST_Data     com   xcStr               _ ->
      if null xcStr
        then  pString "data{}"
        else  pClosure (pString "data" >> mapM_ pPrint com) "{" "}" (map pPrint xcStr)
    AST_Lambda   typ   ccNmx   xcObjXp     _ -> do
      let hdr = pPrintComWith (pList_ (show typ++"(") ", " ")" . map (pPrintComWith pPrint)) ccNmx
      pPrintSubBlock hdr xcObjXp
    AST_MetaEval cObjXp                    _ -> pInline [pString "#{", pPrint cObjXp, pString "}#"]

instance PPrintable AST_TopLevel where
  pPrint o = case o of
    AST_Attribute a b     _ -> pInline [pPrint a, pString "  ", pPrint b, pString ";"]
    AST_TopFunc   a b c d _ -> pClosure header " { " " }" (map pPrint d) where
      header = do
        pString "function "
        mapM_ pPrint a
        pPrint b
        pPrintComWith (pList_ "(" ", " ")" . map pPrint) c
    AST_TopScript a       _ -> pPrint a
    AST_TopLambda a b c   _ -> pClosure header " { " " }" (map pPrint c) where
      header = pShow a >> pPrintComWith (pList_ "(" ", " ")" . map pPrint) b
    AST_Event     a b c   _ -> pClosure (pShow a >> mapM_ pPrint b) " { " " }" (map pPrint c)
    AST_TopComment a        -> mapM_ (\a -> pPrint a >> pNewLine) a

pPrintInterm :: (Intermediate obj ast, PPrintable ast) => obj -> PPrint ()
pPrintInterm = mapM_ pPrint . fromInterm

instance PPrintable TopLevelExpr where { pPrint = pPrintInterm }
instance PPrintable ScriptExpr   where { pPrint = pPrintInterm }
instance PPrintable ObjectExpr   where { pPrint = pPrintInterm }

instance PPrintable Subroutine where
  pPrint a = case a of
    Subroutine pats exe -> prin "function" pats exe
    GlobAction pats exe -> prin "rule"     pats exe
    where
      prin typ pats exe =
        pClosure (pList (pString typ) "(" "," ")" (map pPrint pats)) "{" "}" $
          map pPrint (origSourceCode exe)

instance PPrintable Executable where { pPrint = mapM_ pPrint . origSourceCode }

instance PPrintable Pattern where
  pPrint pat = case pat of
    ObjAnyX                             -> pString "some"
    ObjMany                             -> pString "all"
    ObjAny1                             -> pString "any1"
    ObjEQ       o                       -> pList (pString "eq") "(" "" ")" [pPrint o]
    ObjType     enumSet_TypeID          ->
      pList_ "(" "|" ")" (map pPrint (filter (Es.member enumSet_TypeID) [NullType .. BytesType]))
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
    GlobType     -> "glob"
    ScriptType   -> "function"
    RuleType     -> "rule"
    BytesType    -> "data"

