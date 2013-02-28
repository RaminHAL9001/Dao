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
import           Dao.Object
import           Dao.Pattern
import           Dao.EnumSet
import qualified Dao.Tree as T

import           Control.Monad
import           Control.Monad.State

import           Numeric

import           Data.Maybe (fromMaybe, maybeToList)
import           Data.List
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

----------------------------------------------------------------------------------------------------

-- | A commonly used pattern, like 'pHeader' but the contents of it is always a list of items which
-- can be pretty-printed by the given @(o -> 'PPrint' ())@ function.
pContainer :: String -> (o -> PPrint ()) -> [o] -> PPrint ()
pContainer label prin ox = pList (pString label) "{ " ", " "}" (mapM_ prin ox)

pMapAssoc :: Show a => (a, Object) -> PPrint ()
pMapAssoc (a, obj) = pInline (pShow a >> pString " = " >> pPrint obj)

instance PPrintable T_tree where
  pPrint t = case t of
    T.Void            -> pString "struct{}"
    T.Leaf       o    -> pClosure (pString "struct") "(" ")" (pPrint o)
    T.Branch       ox -> pClosure (pString "struct") "{ " "}" (branch ox)
    T.LeafBranch o ox ->
      pClosure (pList (pString "struct") "(" "" ")" (pPrint o)) "{ " "}" (branch ox)
    where
      branch = mapM_ (\ (lbl, obj) -> pMapAssoc (lbl, OTree obj)) . M.assocs

instance PPrintable Pattern where { pPrint = pShow }

instance PPrintable Object where
  pPrint obj = case obj of
    ONull            -> pString "null"
    OTrue            -> pString "true"
    OInt       o     -> pShow o
    OWord      o     -> pString (show o++"U")
    OLong      o     -> pString (show o++"L")
    OFloat     o     -> pString (show o++"f")
    ORatio     o     -> pString ("ratio("++show o++")")
    OComplex  (r:+i) -> pString (show r++'+':show i++"j")
    ODiffTime  o     -> pShow o
    OChar      o     -> pShow o
    OString    o     -> pShow o
    ORef       o     -> pPrint o
    OPair     (a, b) -> pList_ "(" ", " ")" (pPrint a >> pPrint b)
    OList      ox    -> pContainer "list " pPrint ox
    OSet       o     -> pContainer "set "  pPrint (S.elems o)
    OArray     o     -> do
      let (lo, hi)   = bounds o
          showBounds = pList (pString "array ") "(" ", " ")" (pShow lo >> pShow hi)
      pList showBounds "{" ", " "}" (mapM_ pPrint (elems o))
    ODict      o     -> pContainer "dict "   pMapAssoc (M.assocs o)
    OIntMap    o     -> pContainer "intmap " pMapAssoc (I.assocs o)
    OTree      o     -> pPrint o
    OPattern   o     -> pPrint o
    ORule      o     -> pPrint o
    OScript    o     -> pPrint o
    OBytes     o     -> pClosure (pString "data ") "{ " "}" (mapM_ pString (b64Encode o))

----------------------------------------------------------------------------------------------------

instance PPrintable Reference where
  pPrint ref = case ref of
    IntRef     w      -> pString ('$' : show w)
    LocalRef   nm     -> pPrint nm
    StaticRef  nm     -> pInline (pString "static " >> pRef [nm])
    QTimeRef   rx     -> pInline (pString "qtime " >> pRef rx)
    GlobalRef  rx     -> pRef rx
    ProgramRef nm ref -> pInline (pString ("program("++show nm++", ") >> pPrint ref >> pString ")")
    FileRef    p   rx -> pInline (pString ("file("++show p++", ") >> pRef rx >> pString ")")
    Subscript  rx   o -> pInline (pPrint rx >> pString "[" >> pPrint o >> pString "]")
    MetaRef    ref    -> pInline (pString "$(" >> pPrint ref >> pString ")")
    where
      pRef rx = pString (intercalate "." (map prin rx))
      prin r  = case uchars r of
        []                                                      -> "$\"\""
        r:rx | isAlpha r && (null rx || or (map isAlphaNum rx)) -> r:rx
        rx                                                      -> '$' : show rx

instance PPrintable Comment where
  pPrint com = do
    case com of
      InlineComment  c -> pInline (pString "/* " >> pPrint c >> pString " */")
      EndlineComment c -> pInline (pString "// " >> pPrint c >> pNewLine)

pPrintComWith :: (a -> PPrint ()) -> Com a -> PPrint ()
pPrintComWith prin com = case com of
  Com          c    -> prin c
  ComBefore ax c    -> pcom ax >> prin c
  ComAfter     c bx -> prin c  >> pcom bx
  ComAround ax c bx -> pcom ax >> prin c >> pcom bx
  where { pcom = mapM_ pPrint }

pListOfComs :: PPrintable a => [Com a] -> PPrint ()
pListOfComs = mapM_ pPrint

instance PPrintable a => PPrintable (Com a) where { pPrint = pPrintComWith pPrint }

pPrintSubBlock :: PPrint () -> [Com ScriptExpr] -> PPrint ()
pPrintSubBlock hdr = pClosure hdr "{" "}" .  mapM_ (pPrintComWith (\s -> pPrint s >> pString ";"))

instance PPrintable RuleExpr where
  pPrint rule = do
    let pat = rulePattern rule
    flip pPrintComWith (ruleAction rule) $ pPrintSubBlock $ case unComment pat of
      []  -> pString "rule()" 
      [_] -> pPrintComWith (\pat -> pInline (pString "rule " >> mapM_ pPrint pat)) pat
      _   -> pPrintComWith (pList (pString "rule") "(" "," ")" . mapM_ pPrint) pat

instance PPrintable Subroutine where
  pPrint sub = flip pPrintSubBlock (subSourceCode sub) $
    pList_ "(" "," ")" (mapM_ pPrint (argsPattern sub))

instance PPrintable ScriptExpr where
  pPrint expr = case expr of
    EvalObject   objXp  coms                    _ -> pPrint objXp >> mapM_ pPrint coms
    IfThenElse   coms   objXp  thenXp  elseXp   _ -> do
      pClosure (pList (pString "if") "(" "" ")" (pPrint objXp)) "{" "}" $
        pPrintComWith pListOfComs thenXp
      case unComment elseXp of
        []                   -> return ()
        [p] -> case unComment p of
          (IfThenElse _ _ _ _ _) -> pString "else " >> pPrintComWith pListOfComs elseXp
          _                      -> done
        px                   -> done
        where { done = pClosure (pString "else ") "{" "}" (pPrintComWith (mapM_ pPrint) elseXp) }
    TryCatch     cxcScrpXp  cUStr     xcScrpXp  _ -> do
      pClosure (pString "try ") "{" "}" $
        pPrintComWith (mapM_ (\p -> pPrint p  >> pString ";")) cxcScrpXp
      if null xcScrpXp
        then  return ()
        else  pPrintSubBlock (pString "catch " >> pPrint cUStr) xcScrpXp
    ForLoop      cNm        cObjXp    xcScrpXp  _ -> do
      let hdr = do
            pString "for "
            pPrint cNm
            pString " in "
            pList_ "(" "" ")" (pPrint cObjXp)
      pPrintSubBlock hdr xcScrpXp
    ContinueExpr contin     coms      cObjXp    _ -> pInline $ do
      pString (if contin then "continue" else "break")
      mapM_ pPrint coms 
      case unComment cObjXp of
        VoidExpr -> return ()
        _        -> do
          pString " if "
          pList_ " (" "" ")" (pPrint cObjXp)
      pString ";"
    ReturnExpr   retrn                cObjXp    _ -> do
      pString (if retrn then "return" else "throw")
      case unComment cObjXp of
        VoidExpr -> return ()
        _        -> pPrint cObjXp
      pString ";"
    WithDoc      cObjXp               xcScrpXp  _ ->
      pPrintSubBlock (pString "with " >> pPrint cObjXp) xcScrpXp

instance PPrintable ArithOp  where { pPrint = pShow }
instance PPrintable UpdateOp where { pPrint = pShow }

instance PPrintable ObjectExpr where
  pPrint expr = case expr of
    VoidExpr                                 -> return ()
    Literal      o                         _ -> pPrint o
    AssignExpr   objXp1  comUpdOp  objXp2  _ -> pInline $
      pPrint objXp1 >> pPrint comUpdOp >> pPrint objXp2 >> pString "; "
    Equation     objXp1  comAriOp  objXp2  _ -> pInline $
      pPrint objXp1 >> pPrint comAriOp >> (seq objXp2 (pPrint objXp2))
    PrefixExpr   ariOp    c_ObjXp          _ -> pInline $
      pShow ariOp >> pList_ "(" "" ")" (pPrint c_ObjXp)
    ParenExpr    bool     c_ObjXp          _ -> pInline $
      if bool then  pList_ "(" "" ")" (pPrint c_ObjXp)
              else  pPrint c_ObjXp
    ArraySubExpr objXp    coms     c_ObjXp _ -> pInline $
      pPrint objXp >> mapM_ pPrint coms >> pList_ "[" "" "]" (pPrint c_ObjXp)
    FuncCall     nm       coms     xcObjXp _ -> do
      pList (pPrint nm >> mapM_ pPrint coms) "(" "," ")" (mapM_ pPrint xcObjXp)
      pString "; "
    DictExpr     dict     coms     xcObjXp _ -> do
      pList (pPrint dict >> mapM_ pPrint coms) " {" "," "}" (mapM_ pPrint xcObjXp)
      pString "; "
    ArrayExpr    cxcObjXp xcObjXp          _ -> do
      let hdr = do
            pString "array"
            pPrintComWith (pList_ "(" ", " ")" . mapM_ pPrint) cxcObjXp
      pList hdr  "{" ", " "}" (mapM_ pPrint xcObjXp)
    StructExpr   cObjXp   xcObjXp          _ ->
      pList (pString "struct " >> pPrint cObjXp) "{" ", " "}" (mapM_ pPrint xcObjXp)
    LambdaCall   cObjXp   xcObjXp          _ -> do
      pList (pString "call " >> pPrint cObjXp) "(" ", " ")" (mapM_ pPrint xcObjXp)
      pString "; "
    LambdaExpr   ccNmx    xcObjXp          _ -> do
      let hdr = pString "call " >> pPrintComWith (pList_ "(" ", " ")" . mapM_ pPrint) ccNmx
      pPrintSubBlock hdr xcObjXp

instance PPrintable ObjPat where
  pPrint pat = case pat of
    ObjAnyX                             -> pString "some"
    ObjMany                             -> pString "all"
    ObjAny1                             -> pString "any1"
    ObjEQ       o                       -> pList (pString "eq") "(" "" ")" (pPrint o)
    ObjType     enumSet_TypeID          ->
      pList_ "(" "|" ")" (mapM_ pPrint (filter (setMember enumSet_TypeID) [NullType .. BytesType]))
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
    PatternType  -> "strpat"
    ScriptType   -> "script"
    RuleType     -> "rule"
    BytesType    -> "data"

----------------------------------------------------------------------------------------------------

indent :: Int -> String
indent = flip replicate '\t'

showRef :: [Name] -> String
showRef nmx = "${" ++ intercalate "." (map str nmx) ++ "}" where
  str nm = case uchars nm of
    "" -> "$\"\""
    a:ax | (isAlpha a || a=='_') && and (map isPrint ax) -> a:ax
    _ -> '$':show nm

showObj :: Int -> Object -> String
showObj idnc o = case o of
  ONull       -> "null"
  OTrue       -> "true"
  OType     o -> show o
  OInt      o -> show o
  OWord     o -> sh "word" o
  OLong     o -> sh "long" o
  OFloat    o -> show o
  ORatio    o -> "ratio ("++(map (\c -> if c=='%' then '/' else c) (show o))++")"
  OComplex  o -> show o
  OTime     o -> "date ("++show o++")"
  ODiffTime o -> "difftime ("++show o++")"
  OChar     o -> show o
  OString   o -> show o
  ORef      o -> '$':showReference o
  OPair (a,b) -> let i = idnc+1 in "pair ("++showObj i a++", "++showObj i b++")"
  OList     o -> "list "++simplelist idnc o
  OSet      o -> "set "++simplelist idnc (S.elems o)
  OArray    o ->
    let (a,b) = bounds o
    in  multiline ("array ("++show a++',':show b++")") "[" "," "]" (elems o)
  ODict     o -> pairlist "dict " show (M.assocs o)
  OIntMap   o -> pairlist "intmap " show (I.assocs o)
  OTree     o -> pairlist "tree " showRef (T.assocs o)
  OPattern  o -> "pattern "++show o
  ORule     o -> showRule idnc o
  OScript   o -> showScript idnc Nothing o
  OBytes    o -> "data " ++
    '{' : unlines (map ((indent (idnc+1))++) (b64Encode o)) ++ '\n' : indent idnc ++ "}"
  where
    sh lbl o = lbl++" "++show o
    simplelist idnc o = '{':intercalate ", " (map (showObj (idnc+1)) o)++"}" 
    pairlist prefix showKey items =
      let tabs = indent (idnc+1)
          line (key, o) = tabs ++ showKey key ++ ": " ++ showObj (idnc+1) o ++ ",\n"
      in  prefix ++ "{\n" ++ concatMap line items ++ '\n':indent idnc ++ "}"
    multiline prefix open itemsep close items =
      let tabs = indent (idnc+1)
      in  prefix ++ ' ':open ++ "\n"
            ++ intercalate (itemsep++"\n") (map (showObj (idnc+1)) items)
            ++ ('\n':indent idnc++close)

showComments :: [Comment] -> String
showComments comx = flip concatMap comx $ \com -> case com of
  InlineComment  com -> " /*"++uchars com++"*/ "
  EndlineComment com -> " //"++uchars com++"\n"

showCom :: (Int -> a -> String) -> Int -> Com a -> String
showCom showItem idnc com = case com of
  Com         b   ->         showItem idnc b
  ComBefore a b   -> sh a ++ showItem idnc b
  ComAfter    b c ->         showItem idnc b ++ sh c
  ComAround a b c -> sh a ++ showItem idnc b ++ sh c
  where
    idnc' = idnc+1
    sh c = showComments c ++ indent idnc'

showScript :: Int -> Maybe Name -> Subroutine -> String
showScript idnc maybeName scrp =
     "func"++ fromMaybe "" (maybeName >>= Just . (" "++) . uchars)
  ++ showCom argv idnc (Com $ map Com $ argsPattern scrp) ++ " {"
  ++ showScriptBlock (idnc+1) (Com $ subSourceCode scrp)
  ++ '\n':indent idnc ++ "}"
  where
    argv idnc ax = "("
      ++ intercalate ", " (map (\a -> showCom (\_ a -> show a) (idnc+1) a) ax)
      ++ ")"

showScriptBlock :: Int -> Com [Com ScriptExpr] -> String
showScriptBlock idnc linex = " {" ++ showCom loop (idnc+1) linex ++ '\n':indent idnc ++ "}" where
  loop idnc linex = case linex of
    []         -> ""
    line:linex -> '\n':indent idnc++showScriptExpr idnc line++loop idnc linex

showScriptExpr :: Int -> Com ScriptExpr -> String
showScriptExpr idnc line = showCom expr idnc line where
  expr idnc line = case line of
    EvalObject   obj c _          -> showObjectExpr idnc (ComAfter obj c) ++ ";"
    IfThenElse   c ifx thx elx _  -> showComments c ++ ifloop (Com ifx) thx elx where
      ifloop ifx thx elx =
           "if(" ++ showObjectExpr idnc ifx ++ ')' : showScriptBlock idnc thx
        ++ let showElse = "else " ++ showScriptBlock idnc elx
           in case unComment elx of
                [] -> ""
                [elexpr] -> case unComment elexpr of
                  IfThenElse c ifx thx elx _ -> showComments c ++ "else " ++ ifloop (Com ifx) thx elx
                  _ -> showElse
                _ -> showElse
    TryCatch     try nm ctch _    -> "try " ++ showScriptBlock idnc try
      ++ '\n':indent idnc ++ "catch " ++ showCom (\_ -> uchars) (idnc+1) nm ++ showScriptBlock idnc (Com ctch)
    ForLoop      nm obj scrp _    -> "foreach " ++ showCom (\_ -> uchars) idnc nm
      ++ " in (" ++ showObjectExpr idnc obj ++ ")" ++ showScriptBlock idnc (Com scrp)
    ContinueExpr fn  com obj _    -> contBrkRetnThrow idnc fn com obj "continue" "break"
    ReturnExpr   fn  obj     _    -> contBrkRetnThrow idnc fn []  obj "return"   "throw"
    WithDoc      obj with    _    -> "with " ++ showObjectExpr idnc obj ++ showScriptBlock idnc (Com with)

-- | Used to show 'Dao.Object.ContinueExpr' and 'Dao.Object.ReturnExpr'.
contBrkRetnThrow :: Int -> Bool -> [Comment] -> Com ObjectExpr -> String -> String -> String
contBrkRetnThrow idnc fn com obj cont brk =
  (if fn then cont else brk) ++ showComments com ++ " (" ++ showObjectExpr idnc obj ++ ")"

showReference :: Reference -> String
showReference o = case o of
  IntRef       o -> '$':show o
  LocalRef     o -> uchars o
  QTimeRef     o -> "qtime "++sh o
  StaticRef    o -> "static "++uchars o
  GlobalRef    o -> showRef o
  ProgramRef p o -> "program("++uchars p++", "++showReference o++")"
  FileRef    f o -> "file("++uchars f++", "++showRef o++")"
  MetaRef      o -> "$("++showReference o++")"
  where { sh = intercalate "." . map uchars }

showObjectExpr :: Int -> Com ObjectExpr -> String
showObjectExpr idnc obj = showCom loop idnc obj where
  tuple sh idnc args = 
    '(':showCom (\idnc args -> intercalate ", " (map (sh idnc) args)) (idnc+1) args++")"
  assignExpr idnc expr = case expr of
    AssignExpr   ref  op  obj    _ -> showObjectExpr idnc (Com ref)
      ++ showCom (\_ -> show) idnc op ++ showObjectExpr (idnc+1) (Com obj)
    _ -> loop idnc expr
  dictExpr idnc objx = intercalate (",\n"++indent idnc) (map (showCom assignExpr (idnc+1)) objx)
  loop idnc obj = case obj of
    Literal      obj             _ -> showObj idnc obj
    AssignExpr   ref op   obj    _ ->
      showObjectExpr idnc (Com ref) ++ showCom (\_ -> show) idnc op ++ showObjectExpr (idnc+1) (Com obj)
    FuncCall     name c    args  _ -> uchars name ++ showComments c ++ tuple showObjectExpr idnc (Com args)
    LambdaCall   obj       args  _ -> "call "
      ++ showObjectExpr (idnc+1) obj
      ++ tuple showObjectExpr idnc (Com args)
    ParenExpr    grp  sub        _ ->
      let str = showObjectExpr (idnc+1) sub in if grp then '(' : str ++ ")" else str
    Equation     left op  right  _ -> showObjectExpr idnc (Com left)
      ++ showCom (\_ -> show) idnc op
      ++ showObjectExpr idnc (Com right)
    PrefixExpr   op       expr   _ -> show op ++ showObjectExpr idnc expr
    DictExpr     dict c objx     _ -> uchars dict ++ ' ' : showComments c
      ++ "{\n" ++ indent (idnc+1) ++ showCom dictExpr idnc (Com objx) ++ "}"
    ArrayExpr    bnds   elms     _ -> "array "
      ++ tuple showObjectExpr (idnc+1) bnds ++ '['
      :  showCom (\idnc elms -> intercalate ", " (map (showObjectExpr (idnc+1)) elms)) idnc (Com elms)
      ++ "]"
    ArraySubExpr obj  com  sub   _ -> showObjectExpr idnc (Com obj)
      ++ showComments com
      ++ '[':showObjectExpr (idnc+1) sub++"]"
    LambdaExpr  argv  scrp       _ -> tuple (showCom (\_ -> uchars)) (idnc+1) argv
      ++ "{\n" ++ indent (idnc+1) ++ showScriptBlock (idnc+1) (Com scrp) ++ '\n':indent idnc++"}"

showRule :: Int -> RuleExpr -> String
showRule idnc rule = "rule "
  ++ showCom (\ _ pat -> show pat) idnc (rulePattern rule)
  ++ showScriptBlock idnc (ruleAction rule)

showRuleSet :: Int -> PatternTree [Com [Com ScriptExpr]] -> String
showRuleSet idnc rules = unlines $ map mkpat $ T.assocs $ rules where
  showScrp rule scrp = rule++showScriptBlock 1 scrp ++ '\n':indent idnc
  mkpat (pat, scrpx) =
    let rule = indent idnc ++ "rule "
                ++ show (Pattern{getPatUnits = pat, getPatternLength = length pat})
    in  unlines (map (showScrp rule) scrpx)

