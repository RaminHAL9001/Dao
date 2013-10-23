-- "src/Dao/Object/PPrintM.hs"  provides functions for pretty printing Dao
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

module Dao.Object.PPrintM where

import           Dao.String
import           Dao.PPrintM
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
import           Data.Monoid
import           Data.Char
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
-- can be pretty-printed by the given @(o -> 'PPrintM' ())@ function.
pContainer :: String -> (o -> PPrint) -> [o] -> PPrint
pContainer label prin ox = pList (pString label) "{ " ", " " }" (map prin ox)

pMapAssoc :: Show a => (a, Object) -> PPrint
pMapAssoc (a, obj) = pWrapIndent [pShow a, pString " = ", pPrint obj]

instance PPrintable Name where { pPrint = pUStr . toUStr }

instance PPrintable T_tree where
  pPrint t = case t of
    T.Void            -> pString "tree"
    T.Leaf       o    -> leaf o
    T.Branch       ox -> pList (pString "tree") "{ " ", " " }" (branch ox)
    T.LeafBranch o ox -> pList (leaf o) " { " ", " " }" (branch ox)
    where
      leaf o = pWrapIndent [pString "tree(", pPrint o, pString ")"]
      branch = map (\ (lbl, obj) -> pMapAssoc (lbl, OTree obj)) . M.assocs

instance PPrintable Reference where
  pPrint (Reference r) = pInline $ intercalate [pString "."] (fmap (return . pPrint) r)

instance PPrintable QualRef where
  pPrint r = case r of
    Unqualified r -> pPrint r
    Qualified q r -> pInline $ concat $
      [[pUStr (toUStr q)], guard (precedeWithSpace q) >> [pString " "], [pPrint r]]
    ObjRef      o -> pInline [pString "(", pPrint o, pString ")"]

instance PPrintable Glob where { pPrint = pShow }
instance PPrintable Object where
  pPrint obj = case obj of
    ONull            -> pString "null"
    OTrue            -> pString "true"
--    OType      o     -> pPrint o
    OInt       o     -> pShow o
    OWord      o     -> pString (show o++"U")
    OLong      o     -> pString (show o++"L")
    OFloat     o     -> pString (show o++"f")
    ORatio     o     ->
      if denominator o == 1
        then  pString (show (numerator o)++"R")
        else  pWrapIndent $
                [ pString "(", pString (show (numerator o)), pString "/"
                , pString (show (denominator o)++"R"), pString ")"
                ]
    OComplex  (r:+i) ->
      if r==0
        then  pString (show i++"i")
        else  pWrapIndent $
                [ pString "(", pString (show r), unless (i<0) (pString "+")
                , pString (show i++"i"), pString ")"
                ]
    ORelTime   o     -> pShow o
    OAbsTime   o     -> pString ("date "++show o)
    OChar      o     -> pShow o
    OString    o     -> pShow o
    ORef       o     -> pPrint o
--  OPair     (a, b) -> pList (pString "pair") "(" ", " ")" [pPrint a, pPrint b]
    OList      ox    -> if null ox then pString "list{}" else pContainer "list " pPrint ox
--  OSet       o     -> if S.null o then pString "set{}" else pContainer "set "  pPrint (S.elems o)
--  OArray     o     -> do
--    let (lo, hi)   = bounds o
--        showBounds = pList (pString "array") "(" ", " ")" [pShow lo, pShow hi]
--    pList showBounds " { " ", " " }" (map pPrint (elems o))
--  ODict      o     ->
--    if M.null o then pString "dict{}" else pContainer "dict " pMapAssoc (M.assocs o)
--  OIntMap    o     ->
--    if I.null o then pString "intmap{}" else pContainer "intmap " pMapAssoc (I.assocs o)
    OTree      o     -> pPrint o
--  OGlob      o     -> pPrint o
--  OScript    o     -> pPrint o
    OBytes     o     ->
      if B.null o
        then  pString "data{}"
        else  pClosure (pString "data ") "{ " " }" (map pString (b64Encode o))
    OHaskell   o ifc -> error $ "cannot pretty print Haskell data type: "++show (objHaskellType ifc)

----------------------------------------------------------------------------------------------------

-- Statements like "if" or "while" take a condition, and the Dao languages does not require these
-- conditions be enclosed in parenthases. The question is, should there be a space after the "if" or
-- "while" statement? This function resolves that question by checking if an object expression
-- already is enclosed in parentheses, and if so, does not put a space. Otherwise, a space will be
-- printed between the "if" tag or "while" tag and the condition.
class PrecedeWithSpace a where { precedeWithSpace :: a -> Bool }
instance PrecedeWithSpace Name where { precedeWithSpace _ = True }
instance PrecedeWithSpace AST_LValue where { precedeWithSpace (AST_LValue o) = precedeWithSpace o }
instance PrecedeWithSpace AST_Object where
  precedeWithSpace o = case o of
    AST_Void                -> False
    AST_ObjParen{}          -> False
    AST_MetaEval{}          -> False
    AST_Prefix{}            -> False
    AST_ObjQualRef  o       -> precedeWithSpace o
    AST_Assign      o _ _ _ -> precedeWithSpace o
    AST_Equation    o _ _ _ -> precedeWithSpace o
    AST_ArraySub    o _ _   -> precedeWithSpace o
    AST_FuncCall    o _ _   -> precedeWithSpace o
    _                       -> True
instance PrecedeWithSpace RefQualifier where
   precedeWithSpace o = case o of
     LOCAL    -> True
     QTIME    -> True
     STATIC   -> True
     GLOBAL   -> True
     _        -> False
instance PrecedeWithSpace a => PrecedeWithSpace (Com a) where
  precedeWithSpace o = case o of
    Com         b   -> precedeWithSpace b
    ComBefore a b   -> precedeWithSpace a || precedeWithSpace b
    ComAfter    b _ -> precedeWithSpace b
    ComAround a b _ -> precedeWithSpace a || precedeWithSpace b
    -- there should always be a space before a comment.
instance PrecedeWithSpace [Comment] where { precedeWithSpace = not . null }
instance PrecedeWithSpace AST_Ref where
  precedeWithSpace r = case r of
    AST_RefNull   -> False
    AST_Ref _ _ _ -> True
instance PrecedeWithSpace AST_QualRef where
  precedeWithSpace r = case r of
    AST_Unqualified   r   -> precedeWithSpace r
    AST_Qualified _ _ r _ -> precedeWithSpace r

----------------------------------------------------------------------------------------------------

instance PPrintable [Comment]   where { pPrint = mapM_ pPrint }
instance PPrintable Comment where
  pPrint com = do
    case com of
      EndlineComment c -> pString ("//"++uchars c) >> pForceNewLine
      InlineComment  c -> pGroup True $ pInline $
        concat [[pString " /*"], map pString (lines (uchars c)), [pString "*/ "]]

pPrintComWith :: (a -> PPrint) -> Com a -> PPrint
pPrintComWith prin com = case com of
  Com          c    -> prin c
  ComBefore ax c    -> pcom ax >> prin c
  ComAfter     c bx -> prin c >> pcom bx
  ComAround ax c bx -> pcom ax >> prin c >> pcom bx
  where { pcom = pInline . map pPrint }

pListOfComsWith :: (a -> PPrint) -> [Com a] -> PPrint
pListOfComsWith prin = sequence_ . map (pPrintComWith prin)

pListOfComs :: PPrintable a => [Com a] -> PPrint
pListOfComs = pListOfComsWith pPrint

instance PPrintable a => PPrintable (Com a) where { pPrint = pPrintComWith pPrint }

----------------------------------------------------------------------------------------------------

--instance PPrintable RefInfixOp  where { pPrint = pString . uchars . ustr }
instance PPrintable AST_Ref where
  pPrint ref = case ref of
    AST_RefNull    -> return ()
    AST_Ref n nx _ -> pInline $ pPrint n :
      fmap (\n -> pPrintComWith (\ () -> pString ".") (fmap (const ()) n) >> pPrint (unComment n)) nx
instance PPrintable RefQualifier where { pPrint = pUStr . toUStr }
instance PPrintable AST_QualRef where
  pPrint ref = case ref of
    AST_Unqualified     r   -> pPrint r
    AST_Qualified q com r _ -> pInline [pPrint q, pString " ", pPrint com, pPrint r]

instance PPrintable RefExpr where { pPrint = mapM_ pPrint . fromInterm }

----------------------------------------------------------------------------------------------------

-- 'pPrintComWith' wasn't good enough for this, because the comments might occur after the header
-- but before the opening bracket.
pPrintComCodeBlock :: PPrint -> Com AST_CodeBlock -> PPrint
pPrintComCodeBlock header c = case c of
  Com          c    -> run [] c []
  ComBefore bx c    -> run bx c []
  ComAfter     c ax -> run [] c ax
  ComAround bx c ax -> run bx c ax
  where
    run :: [Comment] -> AST_CodeBlock -> [Comment] -> PPrint
    run before cx after = case getAST_CodeBlock cx of
      [] -> header >> pInline (map pPrint before) >> pString " {}" >> pInline (map pPrint after)
      cx -> do
        pClosure (header >> pInline (map pPrint before)) " { " " }" (map (pGroup True . pPrint) cx)
        pInline (map pPrint after)

pPrintSubBlock :: PPrint -> AST_CodeBlock -> PPrint
pPrintSubBlock header px = pPrintComCodeBlock header (Com px)

instance PPrintable AST_CodeBlock where { pPrint o = mapM_ pPrint (getAST_CodeBlock o) }

instance PPrintable AST_Paren where
  pPrint (AST_Paren o _) = pInline [pString "(", pPrint o, pString ")"]

instance PPrintable AST_If where
  pPrint (AST_If ifn thn _) =
    pClosure (pString "if" >> pPrint ifn) "{" "}" [pPrint thn]

instance PPrintable AST_Else where
  pPrint (AST_Else coms (AST_If ifn thn _) _) =
    pClosure (pPrintComWith (\ () -> pString "else ") coms >> pString "if") "{" "}" [pPrint thn]

instance PPrintable AST_IfElse where
  pPrint (AST_IfElse ifn els coms deflt _) = do
    pPrint ifn >> pNewLine
    mapM_ pPrint els >> pNewLine
    case deflt of
      Nothing    -> return ()
      Just deflt -> pClosure (pPrintComWith (\ () -> pString "else") coms) "{" "}" [pPrint deflt]

instance PPrintable AST_While where
  pPrint (AST_While (AST_If ifn thn _)) =
    pClosure (pString "while") "{" "}" [pPrint thn]

--instance PPrintable AST_ElseIf where
--  pPrint o = case o of
--    AST_NullElseIf                -> return ()
--    AST_Else   coms  block      _ -> pClosure (hdr coms) "{" "}" [pPrint block] where
--      hdr coms = pString (if precedeWithSpace coms then "else " else "else") >> pPrint coms
--    AST_ElseIf objXp block next _ -> do
--      pClosure (pString (if precedeWithSpace objXp then "if " else "if")) "{" "}" [pPrint block]
--      pPrint next

instance PPrintable AST_Script where
  pPrint expr = pGroup True $ case expr of
    AST_Comment             coms -> mapM_ pPrint coms
    AST_EvalObject   objXp  coms                    _ ->
      pPrint objXp >> mapM_ pPrint coms >> pString ";"
    AST_IfThenElse   ifXp                             -> pPrint ifXp
    AST_WhileLoop    whileLoop                        -> pPrint whileLoop
    AST_TryCatch     cxcScrpXp  cQRef     xcScrpXp  _ -> do
      pClosure (pString "try") "{" "}" [pPrint cxcScrpXp]
      maybe (return ()) id $ msum $
        [ cQRef >>= \qref -> xcScrpXp >>= \xscrp -> Just $
            pClosure (pString "catch " >> pPrint qref) "{" "}" [pPrint xscrp]
        , cQRef    >>= \qref  -> Just $ pString "catch " >> pPrint qref
        , xcScrpXp >>= \xscrp -> Just $ pClosure (pString "catch ") "{" "}" [pPrint xscrp]
        ]
    AST_ForLoop      cNm        cObjXp    xcScrpXp  _ ->
      pPrintSubBlock (pString "for " >> pPrint cNm >> pString " in " >> pPrint cObjXp) xcScrpXp
    AST_ContinueExpr contin     coms      cObjXp    _ -> pWrapIndent $
      [ pString (if contin then "continue" else "break")
      , pInline (map pPrint coms)
      , case unComment cObjXp of
          AST_Void -> return ()
          _        ->
            pString " if" >> when (precedeWithSpace cObjXp) (pString " ") >> pPrint cObjXp
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

instance PPrintable PrefixOp where { pPrint = pUStr . toUStr }
instance PPrintable InfixOp  where { pPrint = pUStr . toUStr }
instance PPrintable UpdateOp where { pPrint op = pString (' ':uchars op++" ") }

instance PPrintable AST_ObjList where
  pPrint (AST_ObjList coms lst _) = pList (pPrint coms) "{" ", " "}" (map pPrint lst)

instance PPrintable AST_OptObjList where
  pPrint o = case o of
    AST_NoObjList                           com2 -> pPrint com2
    AST_OptObjList (AST_ObjList com1 lst _) com2 ->
      pInline [pPrint com1, pList_ "{" ", " "}" (map pPrint lst), pPrint com2]

instance PPrintable AST_LValue where { pPrint (AST_LValue o) = pPrint o }

instance PPrintable AST_Param where
  pPrint o = case o of
    AST_NoParams            -> return ()
    AST_Param mcoms tychk _ -> pInline $
      [ maybe (return ()) (\coms -> pString "$" >> pPrint coms) mcoms
      , pPrint tychk
      ]

instance PPrintable [Com AST_Param] where
  pPrint lst = pList_ "(" ", " ")" (fmap pPrint lst)

instance PPrintable AST_ParamList where
  pPrint (AST_ParamList lst _) = pInline [pPrint lst]

instance PPrintable AST_StringList where
  pPrint o = case o of
    AST_NoStrings  coms _ -> pInline [pString "rule(", pPrint coms, pString ")"]
    AST_StringList [r]  _ -> pInline [pString "rule ", pPrint r]
    AST_StringList ruls _ -> pList (pString "rule") "(" ", " ")" (fmap pPrint ruls)

instance PPrintable AST_Object where
  pPrint expr = case expr of
    AST_Void                                 -> return ()
    AST_ObjQualRef   o                       -> pPrint o
    AST_ObjParen          c_ObjXp            -> pWrapIndent [pString "(", pPrint c_ObjXp, pString ")"]
    AST_Literal      o                     _ -> pPrint o
    AST_Assign   objXp1  comUpdOp  objXp2  _ -> pWrapIndent $
      [pPrint objXp1, pPrint comUpdOp, pPrint objXp2]
    AST_Equation     objXp1  comAriOp  objXp2  _ -> pWrapIndent $
      [pPrint objXp1, pPrint comAriOp, pPrint objXp2]
    AST_Prefix   ariOp    c_ObjXp          _ -> pWrapIndent [pPrint ariOp, pPrint c_ObjXp]
    AST_ArraySub objXp             xcObjXp _ -> pList (pPrint objXp) "[" ", " "]" [pPrint xcObjXp]
    AST_FuncCall objXp             xcObjXp _ -> pInline [pPrint objXp, pPrint xcObjXp]
    AST_Init     ref      objs     elems   _ ->
      pList (pInline [pPrint ref, pPrint objs]) "{" ", " "}" [pPrint elems]
    AST_Struct   cObjXp   xcObjXp          _ ->
      pList (pInline [pString "tree", printObj cObjXp]) "{" ", " "}" [pPrint xcObjXp] where
        printObj obj = pWrapIndent [pString " ", pInline [pPrint cObjXp]]
    AST_Lambda         ccNmx   xcObjXp     _ -> pPrintSubBlock (pPrintComWith pPrint ccNmx) xcObjXp
    AST_Func     co nm ccNmx   xcObjXp     _ ->
      pClosure (pInline [pString "function ", pPrint co, pPrint nm, pPrint ccNmx]) "{" "}" [pPrint xcObjXp]
    AST_Rule           ccNmx   xcObjXp     _ -> pClosure (pPrint ccNmx) "{" "}" [pPrint xcObjXp]
    AST_MetaEval cObjXp                    _ -> pInline [pString "{#", pPrint cObjXp, pString "#}"]

instance PPrintable AST_TopLevel where
  pPrint o = case o of
    AST_Attribute a b     _ -> pInline [pPrint a, pString "  ", pPrint b, pString ";"]
    AST_TopScript a       _ -> pPrint a
    AST_Event     a b c   _ -> pClosure (pShow a >> mapM_ pPrint b) " { " " }" (map pPrint (getAST_CodeBlock c))
    AST_TopComment a        -> mapM_ (\a -> pPrint a >> pNewLine) a

pPrintInterm :: (Intermediate obj ast, PPrintable ast) => obj -> PPrint
pPrintInterm = mapM_ pPrint . fromInterm

instance PPrintable AST_SourceCode where
  pPrint sc = do
    let (attrs, dirs) = span isAST_Attribute (directives sc)
    mapM_ pPrint attrs
    pForceNewLine
    mapM_ (\dir -> pPrint dir >> pForceNewLine) dirs

instance PPrintable TopLevelExpr where { pPrint = pPrintInterm }
instance PPrintable ScriptExpr   where { pPrint = pPrintInterm }
instance PPrintable ObjectExpr   where { pPrint = pPrintInterm }

instance PPrintable a => PPrintable (AST_TyChk a) where
  pPrint a = case a of
    AST_NotChecked a          -> pPrint a
    AST_Checked    a coms expr _ -> pInline $
      [ pPrint a
      , pPrintComWith (\ () -> pString ": ") coms
      , pPrint expr
      ]

instance PPrintable a => PPrintable (TyChkExpr a) where
  pPrint a = case a of
    NotTypeChecked a        -> pPrint a
    TypeChecked    a expr _ -> pInline [pPrint a, pString ": ", pPrint expr]

instance PPrintable ParamExpr where
  pPrint (ParamExpr byRef tychk _) = when byRef (pString "$") >> pPrint tychk

instance PPrintable [ParamExpr] where { pPrint lst = pList_ "(" ", " ")" (fmap pPrint lst) }

instance PPrintable ParamListExpr where { pPrint (ParamListExpr lst _) = pPrint lst }

-- Used by the instantiation of CallableCode and GlobAction into the PPrintable class.
callableAction :: String -> PPrint -> ObjType -> Subroutine -> PPrint
callableAction what pats typ exe =
  pClosure (pString what >> pats >> pPrint typ) "{" "}" (map pPrint (codeBlock (origSourceCode exe)))

instance PPrintable CallableCode where 
  pPrint (CallableCode pats ty exe) = callableAction "function" (pPrint pats) ty exe

instance PPrintable GlobAction   where
  pPrint (GlobAction pats exe) = (\a -> callableAction "rule" a nullValue exe) $ case pats of
    []    -> pString "()"
    [pat] -> pPrint pat
    pats  -> pList_ "(" ", " ")" (map pPrint pats)

instance PPrintable Subroutine where { pPrint = mapM_ pPrint . codeBlock . origSourceCode }

-- | Pretty-prints a 'Type' without a colon prefix. The instantiation of 'Dao.Object.Type' into the
-- 'Dao.PPrintM.PPrintable' class always prefixes the type with a colon except when the type is the
-- 'Dao.Object.nullType' value. The instantiation is defined in terms of this function.
pPrintType :: ObjType -> PPrint
pPrintType t = error "pPrintType, used to instantiate 'Dao.Object.Type' into 'PPrintable', has not been defined yet."

instance PPrintable TypeCtx where { pPrint (TypeCtx tx) = pPrint tx }
instance PPrintable TypeSym where
  pPrint t = case t of
    CoreType t     -> pPrint t
    TypeVar  t ctx -> pInline $
      concat [[pPrint t], guard (not (null ctx)) >> [pList_ "[" ", " "]" (map pPrint ctx)]]

instance PPrintable TypeStruct where
  pPrint (TypeStruct tx) = case tx of
    []  -> pString "void"
    [a] -> pPrint a
    ax  -> pList (pString "type") "(" ", " ")" (map pPrint ax)

instance PPrintable ObjType where
  pPrint (ObjType tx) = case tx of
    []  -> return ()
    [a] -> pPrint a
    ax  -> pList (pString "anyOf") "(" ", " ")" (map pPrint tx)

instance PPrintable CoreType where
  pPrint t = pString $ case t of
    NullType     -> "null"
    TrueType     -> "true"
--    TypeType     -> "type"
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
    RefType      -> "ref"
    ListType     -> "list"
    TreeType     -> "tree"
    BytesType    -> "data"
    HaskellType  -> "HaskellDataType"

----------------------------------------------------------------------------------------------------

instance PPrintable ExecError where
  pPrint err = do
    pInline [pString "error message: ", pPrint (specificErrorData err)]
    let f get msg = maybe (return ()) (\o -> pString (msg++":") >> pPrint o) (get err)
    f execErrExpr     "in expression" 
    f execErrScript   "in statement"
    f execErrTopLevel "in top-level directive"

