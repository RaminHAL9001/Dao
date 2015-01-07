-- "src/Dao/Interpreter/AST.hs"  defines the data types for the Dao
-- programming language abstract syntax tree and related type classes.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
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

module Dao.Interpreter.AST
  ( Intermediate(toInterm, fromInterm), Canonical(canonicalize),
    Comment(InlineComment, EndlineComment),
    Com(Com, ComBefore, ComAfter, ComAround),
    NamespaceExpr(NamespaceExpr), AST_Namespace(AST_NoNamespace, AST_Namespace),
    setCommentBefore, setCommentAfter, unComment, getComment, 
    pPrintInterm, putAST, getAST, commentString, pPrintComWith,
    pListOfComsWith, pListOfComs, randComWith, appendComments, com,
    DotNameExpr(DotNameExpr), AST_DotName(AST_DotName), getDotNameAST, undotNameExpr,
    DotLabelExpr(DotLabelExpr), AST_DotLabel(AST_DotLabel), 
    dotLabelToNameList, dotLabelToRefExpr, refToDotLabelExpr, dotLabelToRefAST, refToDotLabelAST,
    RefSuffixExpr(NullRefExpr, DotRefExpr, SubscriptExpr, FuncCallExpr),
    ReferenceExpr(ReferenceExpr, RefObjectExpr),
    RefPfxOp(REF, DEREF), 
    RefQualifier(UNQUAL, LOCAL, CONST, STATIC, GLOBAL, GLODOT),
    UpdateOp(UCONST, UADD, USUB, UMULT, UDIV, UMOD, UPOW, UORB, UANDB, UXORB, USHL, USHR), 
    ArithPfxOp(INVB, NOT, NEGTIV, POSTIV), 
    InfixOp(
      ADD, SUB, MULT, DIV, MOD, POW, ORB, ANDB, XORB, SHL, SHR, OR, AND,
      EQUL, NEQUL, GTN, LTN, GTEQ, LTEQ, ARROW
    ), infixOpCommutativity,
    allUpdateOpStrs, allPrefixOpChars, allPrefixOpStrs, allInfixOpChars, allInfixOpStrs,
    AST_RefSuffix(AST_RefNull, AST_DotRef, AST_Subscript, AST_FuncCall),
    AST_Reference(AST_Reference, AST_RefObject),
    ObjListExpr(ObjListExpr), AST_ObjList(AST_ObjList),
    OptObjListExpr(OptObjListExpr), AST_OptObjList(AST_OptObjList),
    LiteralExpr(LiteralExpr), AST_Literal(AST_Literal),
    ParenExpr(ParenExpr), AST_Paren(AST_Paren), 
    AssignExpr(EvalExpr, AssignExpr), AST_Assign(AST_Eval, AST_Assign),
    ObjTestExpr(ObjArithExpr, ObjTestExpr, ObjRuleFuncExpr),
    AST_ObjTest(AST_ObjArith, AST_ObjTest, AST_ObjRuleFunc),
    ArithExpr(ObjectExpr, ArithExpr), AST_Arith(AST_Object, AST_Arith), 
    RuleFuncExpr(LambdaExpr, FuncExpr, RuleExpr), AST_RuleFunc(AST_Lambda, AST_Func, AST_Rule),
    TyChkExpr(NotTypeChecked, TypeChecked, DisableCheck), fmapCheckedValueExpr,
    tyChkItem, tyChkExpr, tyChkLoc, typChkResult, checkedExpr,
    AST_TyChk(AST_NotChecked, AST_Checked), checkedAST, fmapCheckedValueAST,
    ParamExpr(ParamExpr), AST_Param(AST_NoParams, AST_Param), 
    ParamListExpr(ParamListExpr), getTypeCheckList,
    AST_ParamList(AST_ParamList), 
    RuleHeadExpr(RuleStringExpr, RuleHeadExpr), 
    AST_RuleHeader(AST_NullRules, AST_RuleString, AST_RuleHeader), 
    CodeBlock(CodeBlock), codeBlock,
    AST_CodeBlock(AST_CodeBlock), getAST_CodeBlock, 
    RefPrefixExpr(PlainRefExpr, RefPrefixExpr), cleanupRefPrefixExpr,
    AST_RefPrefix(AST_RefPrefix, AST_PlainRef),
    ObjectExpr(
      VoidExpr, ObjLiteralExpr, ObjSingleExpr,
      ArithPfxExpr, InitExpr, StructExpr, MetaEvalExpr
    ),
    AST_Object(
      AST_Void, AST_ObjLiteral, AST_ObjSingle,
      AST_ArithPfx, AST_Init, AST_Struct, AST_MetaEval
    ),
    ScriptExpr(
      IfThenElse, WhileLoop, RuleFuncExpr, EvalObject,
      TryCatch, ForLoop, ContinueExpr, ReturnExpr, WithDoc
    ),
    IfExpr(IfExpr), AST_If(AST_If), 
    ElseExpr(ElseExpr), AST_Else(AST_Else), 
    IfElseExpr(IfElseExpr), AST_IfElse(AST_IfElse), 
    LastElseExpr(LastElseExpr), AST_LastElse(AST_LastElse),
    CatchExpr(CatchExpr), AST_Catch(AST_Catch),
    WhileExpr(WhileExpr), AST_While(AST_While), 
    AST_Script(
      AST_Comment, AST_IfThenElse, AST_WhileLoop, AST_RuleFunc, AST_EvalObject,
      AST_TryCatch, AST_ForLoop, AST_ContinueExpr, AST_ReturnExpr, AST_WithDoc
    ),
    TopLevelEventType(BeginExprType, EndExprType, ExitExprType), 
    TopLevelExpr(RequireExpr, ImportExpr, TopScript, EventExpr), isAttribute, 
    AST_TopLevel(AST_Require, AST_Import, AST_TopScript, AST_Event, AST_TopComment),
    isAST_Attribute, getRequiresAndImports,
    AttributeExpr(AttribDotNameExpr, AttribStringExpr),
    AST_Attribute(AST_AttribDotName, AST_AttribString),
    Program(Program), topLevelExprs,
    AST_SourceCode(AST_SourceCode), sourceModified, sourceFullPath, directives, 
  )
  where

import qualified Dao.Binary  as B
import           Dao.String
import           Dao.PPrint
import           Dao.Random
import           Dao.Token

import           Data.Array.IArray
import           Data.List (intersperse)
import           Data.Monoid
import           Data.Typeable
import           Data.Word

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad

----------------------------------------------------------------------------------------------------

-- | Elements of the symantic data structures that instantiate 'Executable' and do not instantiate
-- 'Dao.PPrint.PPrintable', 'Dao.Struct.Structured', or any parsers. Elements of the abstract syntax
-- tree (AST) instantiate 'Dao.PPrint.PPrintable', 'Dao.Struct.Structured', and all of the parsers,
-- but are not executable and do not instantiate 'Executable'. This separates concerns pretty well,
-- but leaves us with the problem of having to convert back and forth between these various data
-- types.
--
-- The 'Intermediate' class allows us to declare a one-to-one relationship between AST types and
-- executable types. For example, 'ObjectExpr' is the intermediate representation of
-- 'AST_Object', so our instance for this relationship is @instane 'Intermediate'
-- 'ObjectExpr' 'AST_Object'@.
class Intermediate obj ast | obj -> ast, ast -> obj where
  toInterm   :: ast -> [obj]
  fromInterm :: obj -> [ast]

-- | This class is used to classify data types that can be generated at random by
-- 'Dao.Random.HasRandGen' and parsed from source code, which might generate data types with
-- identical functionality, identical pretty-printed forms, identical 'Executable' semantics but may
-- have differences in the recursive structure that the 'Prelude.Eq' class would compute as not
-- identical. The job of the 'canonicalize' function is to eliminate these discrepancies by reducing
-- the data structure to a canonical form.
class Canonical a where { canonicalize :: a -> a }

instance Intermediate Name Name where { toInterm = return; fromInterm = return; }

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
um1 :: Intermediate obj ast => Maybe ast -> [Maybe obj]
um1 = maybe [Nothing] (fmap Just . toInterm)

fi :: Intermediate obj ast => obj -> [ast]
fi = fromInterm
nc :: a -> [Com a]
nc = return . Com
nc0 :: Intermediate obj ast => obj -> [Com ast]
nc0 = fmap Com . fromInterm
nm1 :: Intermediate obj ast => Maybe obj -> [Maybe ast]
nm1 = maybe [Nothing] (fmap Just . fromInterm)

-- not for export
no :: RandO Location
no = return LocationUnknown

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is pretty-printable ('Dao.PPrint.PPrintable').
pPrintInterm :: (Intermediate o ast, PPrintable ast) => o -> PPrint
pPrintInterm = mapM_ pPrint . fromInterm

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is 'Dao.Binary.GPut'.
putAST :: (Intermediate obj ast, B.Binary obj mtab) => ast -> B.GPut mtab
putAST ast = case toInterm ast of
    [obj] -> B.put obj
    _     -> fail "binary encoder could not convert AST to intermediate expression"

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is 'Dao.Binary.GGet'.
getAST :: (Intermediate obj ast, B.Binary obj mtab) => B.GGet mtab ast
getAST = B.get >>= \obj -> case fromInterm obj of
    [ast] -> return ast
    _     -> fail "binary decoder constructed object that could not be converted to an AST representation"

----------------------------------------------------------------------------------------------------

-- | Comments in the Dao language are not interpreted, but they are not disgarded either. Dao is
-- intended to manipulate natural language, and itself, so that it can "learn" new semantic
-- structures. Dao scripts can manipulate the syntax tree of other Dao scripts, and so it might be
-- helpful if the syntax tree included comments.
data Comment
  = InlineComment  UStr
  | EndlineComment UStr
  deriving (Eq, Ord, Typeable, Show)

commentString :: Comment -> UStr
commentString com = case com of
  InlineComment  a -> a
  EndlineComment a -> a

instance NFData Comment where
  rnf (InlineComment  a) = seq a ()
  rnf (EndlineComment a) = seq a ()

instance HasNullValue Comment where
  nullValue = EndlineComment nil
  testNull (EndlineComment c) = c==nil
  testNull (InlineComment  c) = c==nil

instance PPrintable Comment where
  pPrint com = do
    case com of
      EndlineComment c -> pString ("//"++uchars c) >> pForceNewLine
      InlineComment  c -> pGroup True $ pInline $
        concat [[pString " /*"], map pString (lines (uchars c)), [pString "*/ "]]

instance PrecedeWithSpace a => PrecedeWithSpace (Com a) where
  precedeWithSpace o = case o of
    Com         b   -> precedeWithSpace b
    ComBefore a b   -> precedeWithSpace a || precedeWithSpace b
    ComAfter    b _ -> precedeWithSpace b
    ComAround a b _ -> precedeWithSpace a || precedeWithSpace b
    -- there should always be a space before a comment.

instance PPrintable [Comment] where { pPrint = mapM_ pPrint }

instance PrecedeWithSpace [Comment] where { precedeWithSpace = not . null }

instance HasRandGen [Comment] where { randO = return []; defaultO = return []; }
--  randO = do
--    i0 <- randInt
--    let (i1, many) = divMod i0 4
--        (i2, typn) = divMod i1 16
--        typx = take many (randToBase 2 typn ++ replicate 4 0)
--        lenx = map (+1) (randToBase 29 i2)
--        com typ = if typ==0 then EndlineComment else InlineComment
--    forM (zip typx lenx) $ \ (typ, len) ->
--      fmap (com typ . ustr . unwords . map (B.unpack . getRandomWord)) (replicateM len randInt)

----------------------------------------------------------------------------------------------------

-- | Symbols in the Dao syntax tree that can actually be manipulated can be surrounded by comments.
-- The 'Com' structure represents a space-efficient means to surround each syntactic element with
-- comments that can be ignored without disgarding them.
data Com a = Com a | ComBefore [Comment] a | ComAfter a [Comment] | ComAround [Comment] a [Comment]
  deriving (Eq, Ord, Typeable, Show)

instance Functor Com where
  fmap fn c = case c of
    Com          a    -> Com          (fn a)
    ComBefore c1 a    -> ComBefore c1 (fn a)
    ComAfter     a c2 -> ComAfter     (fn a) c2
    ComAround c1 a c2 -> ComAround c1 (fn a) c2

instance NFData a => NFData (Com a) where
  rnf (Com         a  ) = deepseq a ()
  rnf (ComBefore a b  ) = deepseq a $! deepseq b ()
  rnf (ComAfter    a b) = deepseq a $! deepseq b ()
  rnf (ComAround a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue a => HasNullValue (Com a) where
  nullValue = Com nullValue
  testNull (Com a) = testNull a
  testNull _ = False

instance HasLocation a => HasLocation (Com a) where
  getLocation = getLocation . unComment
  setLocation com loc = fmap (\a -> setLocation a loc) com
  delLocation = fmap delLocation

instance HasRandGen a => HasRandGen (Com a) where
  randO    = Com <$> randO
  defaultO = Com <$> defaultO

instance PPrintable a => PPrintable (Com a) where { pPrint = pPrintComWith pPrint }

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

randComWith :: RandO a -> RandO (Com a)
randComWith rand = fmap Com rand
--  randComWith :: RandO a -> RandO (Com a)
--  randComWith rand = do
--    typ <- fmap (flip mod 24 . unsign) randInt
--    a <- rand
--    case typ of
--      0 -> do
--        before <- randO
--        after  <- randO
--        return (ComAround before a after)
--      1 -> do
--        before <- randO
--        return (ComBefore before a)
--      2 -> do
--        after <- randO
--        return (ComAfter a after)
--      _ -> return (Com a)

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

----------------------------------------------------------------------------------------------------

-- | Direct a reference at a particular tree in the runtime.
data RefQualifier
  = UNQUAL -- ^ unqualified
  | LOCAL  -- ^ refers to the current local variable stack
  | CONST  -- ^ refers to a built-in constant
  | STATIC -- ^ a local variable stack specific to a 'Subroutine' that lives on even after the
           -- subroutine has completed.
  | GLOBAL -- ^ the global variable space for the current module.
  | GLODOT -- ^ a relative reference, gets it's name because it begins with a dot (".") character.
           -- Similar to the "this" keyword in C++ and Java, refers to the object of the current
           -- context set by the "with" statement, but defaults to the global variable space when
           -- not within a "with" statement. This is necessary to differentiate between local
           -- variables and references to the "with" context.
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance NFData RefQualifier where { rnf a = seq a () }

instance PPrintable RefQualifier where { pPrint = pUStr . toUStr }

instance PrecedeWithSpace RefQualifier where
   precedeWithSpace o = case o of
     LOCAL  -> True
     CONST  -> True
     STATIC -> True
     GLOBAL -> True
     _      -> False

instance UStrType RefQualifier where
  toUStr a = ustr $ case a of
    UNQUAL -> ""
    LOCAL  -> "local"
    CONST  -> "const"
    STATIC -> "static"
    GLOBAL -> "global"
    GLODOT -> "."
  maybeFromUStr str = case uchars str of
    "local"  -> Just LOCAL
    "const"  -> Just CONST
    "static" -> Just STATIC
    "global" -> Just GLOBAL
    "."      -> Just GLODOT
    ""       -> Just UNQUAL
    _        -> Nothing
  fromUStr str = maybe (error (show str++" is not a reference qualifier")) id (maybeFromUStr str)

instance HasRandGen RefQualifier where
  randO = fmap toEnum (nextInt (1+fromEnum (minBound::RefQualifier)))
  defaultO = randO

----------------------------------------------------------------------------------------------------

-- | Binary operators.
data InfixOp
  = ADD   | SUB   | MULT
  | DIV   | MOD   | POW
  | ORB   | ANDB  | XORB
  | SHL   | SHR
  | OR    | AND
  | EQUL  | NEQUL      
  | GTN   | LTN
  | GTEQ  | LTEQ
  | ARROW
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance UStrType InfixOp where
  toUStr a = ustr $ case a of
    { ADD  -> "+" ; SUB  -> "-" ; MULT  -> "*"
    ; DIV  -> "/" ; MOD  -> "%" ; POW   -> "**"
    ; ORB  -> "|" ; ANDB -> "&" ; XORB  -> "^"
    ; SHL  -> "<<"; SHR  -> ">>"
    ; OR   -> "||"; AND  -> "&&"
    ; EQUL -> "=="; NEQUL-> "!="
    ; LTN  -> "<" ; GTN  -> ">"
    ; LTEQ -> "<="; GTEQ -> ">="
    ; ARROW -> "->";
    }
  maybeFromUStr str = case uchars str of
    { "+"  -> Just ADD  ; "-"  -> Just SUB  ; "*"  -> Just MULT 
    ; "/"  -> Just DIV  ; "%"  -> Just MOD  ; "**" -> Just POW  
    ; "|"  -> Just ORB  ; "&"  -> Just ANDB ; "^"  -> Just XORB 
    ; "<<" -> Just SHL  ; ">>" -> Just SHR
    ; "||" -> Just OR   ; "&&" -> Just AND
    ; "==" -> Just EQUL ; "!=" -> Just NEQUL
    ; "<"  -> Just LTN  ; ">"  -> Just GTN
    ; "<=" -> Just LTEQ ; ">=" -> Just GTEQ 
    ; "->" -> Just ARROW;
    ; _    -> Nothing
    }
  fromUStr str = maybe (error (show str++" is not an infix operator")) id (maybeFromUStr str)

instance NFData InfixOp  where { rnf a = seq a () }

instance PPrintable InfixOp  where { pPrint = pUStr . toUStr }

infixOpCommutativity :: InfixOp -> Bool
infixOpCommutativity = (arr !) where
  arr :: Array InfixOp Bool
  arr = array (minBound, maxBound) $
    [ (ADD, True), (SUB, False), (MULT, True), (DIV, False), (MOD, False), (POW, False)
    , (ORB, True), (ANDB, True), (XORB, True), (SHL, False), (SHR, False), (OR, False), (AND, False)
    , (EQUL, True), (NEQUL, True), (LTN, False), (GTN, False), (LTEQ, False), (GTEQ, False)
    , (ARROW, False)
    ]

-- binary 0x8D 0xA0
instance B.Binary InfixOp mtab where
  put o = B.putWord8 $ case o of
    { EQUL -> 0x8D; NEQUL -> 0x8E; GTN  -> 0x8F; LTN   -> 0x90; GTEQ -> 0x91; LTEQ -> 0x92
    ; ADD  -> 0x93; SUB   -> 0x94; MULT -> 0x95; DIV   -> 0x96
    ; MOD  -> 0x97; POW   -> 0x98; ORB  -> 0x99; ANDB  -> 0x9A
    ; XORB -> 0x9B; SHL   -> 0x9C; SHR  -> 0x9D; ARROW -> 0x9E
    ; OR   -> 0x9F; AND   -> 0xA0 } 
  get = B.word8PrefixTable <|> fail "expecting InfixOp"

-- The byte prefixes overlap with the update operators of similar function to
-- the operators, except for the comparison opeators (EQUL, NEQUL, GTN, LTN,
-- GTEQ, LTEQ) which overlap with the prefix operators (INVB, NOT, NEGTIV, POSTIV, REF, DEREF)
instance B.HasPrefixTable InfixOp B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "InfixOp" 0x8D 0xA0 $ let {r=return} in
    [ r EQUL , r NEQUL, r GTN , r LTN, r GTEQ , r LTEQ -- 0x8D,0x8E,0x8F,0x90,0x91,0x92
    , r ADD  , r SUB  , r MULT, r DIV, r MOD  , r POW  , r ORB -- 0x93,0x94,0x95,0x96,0x97,0x98,0x99
    , r ANDB , r XORB , r SHL , r SHR, r ARROW, r OR   , r AND -- 0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,0xA0
    ]

instance HasRandGen InfixOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::InfixOp)))
  defaultO = randO

allPrefixOpChars :: String
allPrefixOpChars = "$@~!-+"

allPrefixOpStrs :: String
allPrefixOpStrs = " $ @ ~ - + ! "

----------------------------------------------------------------------------------------------------

-- | Unary operators.
data ArithPfxOp = INVB | NOT | NEGTIV | POSTIV
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance NFData ArithPfxOp where { rnf a = seq a () }

instance UStrType ArithPfxOp where
  toUStr op = ustr $ case op of
    INVB   -> "~"
    NOT    -> "!"
    NEGTIV -> "-"
    POSTIV -> "+"
  maybeFromUStr str = case uchars str of
    "~" -> Just INVB
    "!" -> Just NOT
    "-" -> Just NEGTIV
    "+" -> Just POSTIV
    _   -> Nothing
  fromUStr str = maybe (error (show str++" is not a prefix opretor")) id (maybeFromUStr str)

instance PPrintable ArithPfxOp where { pPrint = pUStr . toUStr }

-- binary 0x8E 0x9B
instance B.Binary ArithPfxOp mtab where
  put o = B.putWord8 $ case o of { INVB -> 0x9B; NOT -> 0x8E; NEGTIV -> 0x94; POSTIV -> 0x93 }
  get = B.word8PrefixTable <|> fail "expecting ArithPfxOp"

instance B.HasPrefixTable ArithPfxOp B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "ArithPfxOp" 0x8E 0x9F $ let {r=return;z=mzero} in
    [ r NOT -- 0x8E
    , z, z, z, z -- 0x8F,0x90,0x91,0x92
    , r POSTIV, r NEGTIV -- 0x93,0x94
    , z, z, z, z, z, z -- 0x95,0x96,0x97,0x98,0x99,0x9A
    , r INVB -- 0x9B
    ]

instance HasRandGen ArithPfxOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::ArithPfxOp)))
  defaultO = randO

allInfixOpChars :: String
allInfixOpChars = "+-*/%<>^&|.?:"

allInfixOpStrs :: String
allInfixOpStrs = " + - * / % ** -> . || && == != | & ^ << >> < > <= >= . -> <- ? : :: "

----------------------------------------------------------------------------------------------------

newtype DotNameExpr = DotNameExpr{ undotNameExpr :: Name } deriving (Eq, Ord, Show, Typeable)

instance NFData DotNameExpr where { rnf (DotNameExpr n) = deepseq n () }

instance B.Binary DotNameExpr mtab where { put (DotNameExpr n) = B.put n; get = DotNameExpr <$> B.get; }

instance PPrintable DotNameExpr where { pPrint (DotNameExpr n) = pPrint n }

-- | A 'DotName' is simply a ".name" expression in the Dao language. It is a component of the
-- 'DotLabelExpr' and 'AST_DotLabel' data types.
data AST_DotName = AST_DotName (Com ()) Name deriving (Eq, Ord, Show, Typeable)

getDotNameAST :: AST_DotName -> Name
getDotNameAST (AST_DotName _ n) = n

instance NFData AST_DotName where { rnf (AST_DotName a b) = deepseq a $! deepseq b () }

instance PPrintable AST_DotName where
  pPrint (AST_DotName c n) = pInline [pPrintComWith (\ () -> pString ".") c, pPrint n]

instance Intermediate DotNameExpr AST_DotName where
  toInterm   (AST_DotName _ n) = [DotNameExpr n]
  fromInterm (DotNameExpr   n) = [AST_DotName (Com ()) n]

----------------------------------------------------------------------------------------------------

data NamespaceExpr = NamespaceExpr (Maybe Name) Location deriving (Eq, Ord, Show, Typeable)

instance NFData NamespaceExpr where { rnf (NamespaceExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue NamespaceExpr where
  nullValue = NamespaceExpr Nothing LocationUnknown
  testNull (NamespaceExpr Nothing _) = True
  testNull _ = False

instance HasLocation NamespaceExpr where
  getLocation (NamespaceExpr _ loc)     = loc
  setLocation (NamespaceExpr a _  ) loc = NamespaceExpr a loc
  delLocation (NamespaceExpr a _  )     = NamespaceExpr a LocationUnknown

instance PPrintable NamespaceExpr where { pPrint = pPrintInterm }

instance B.Binary NamespaceExpr mtab where
  put (NamespaceExpr a loc) = B.put a >> B.put loc
  get = return NamespaceExpr <*> B.get <*> B.get

----------------------------------------------------------------------------------------------------

data AST_Namespace
  = AST_NoNamespace
  | AST_Namespace (Com Name) Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AST_Namespace where
  rnf  AST_NoNamespace    = ()
  rnf (AST_Namespace a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_Namespace where
  nullValue = AST_NoNamespace
  testNull AST_NoNamespace = True
  testNull _ = False

instance HasLocation AST_Namespace where
  getLocation o     = case o of
    AST_NoNamespace     -> LocationUnknown
    AST_Namespace _ loc -> loc
  setLocation o loc = case o of
    AST_NoNamespace     -> AST_NoNamespace
    AST_Namespace a _   -> AST_Namespace a loc
  delLocation o     = case o of
    AST_NoNamespace     -> AST_NoNamespace
    AST_Namespace a _   -> AST_Namespace a LocationUnknown

instance PPrintable AST_Namespace where
  pPrint o = case o of { AST_NoNamespace -> return (); AST_Namespace a _ -> pPrint a; }

instance HasRandGen AST_Namespace where
  randO = countNode $ runRandChoice
  randChoice = randChoiceList [return AST_NoNamespace, return AST_Namespace <*> randO <*> no]
  defaultO = randO
  defaultChoice = randChoiceList [defaultO]

instance Intermediate NamespaceExpr AST_Namespace where
  toInterm o = case o of
    AST_NoNamespace     -> [nullValue]
    AST_Namespace n loc -> [NamespaceExpr (Just $ unComment n) loc]
  fromInterm (NamespaceExpr n loc) = case n of
    Nothing -> [nullValue]
    Just  n -> [AST_Namespace (Com n) loc]

----------------------------------------------------------------------------------------------------

-- | The intermediate form of 'AST_DotLabel'.
data DotLabelExpr = DotLabelExpr DotNameExpr [DotNameExpr] Location 
  deriving (Eq, Ord, Show, Typeable)

instance NFData DotLabelExpr where
  rnf (DotLabelExpr n nm loc) = deepseq n $! deepseq nm $! deepseq loc ()

instance HasLocation DotLabelExpr where
  getLocation (DotLabelExpr _ _  loc)     = loc
  setLocation (DotLabelExpr n nx _  ) loc = DotLabelExpr n nx loc
  delLocation (DotLabelExpr n nx _  )     = DotLabelExpr n nx LocationUnknown

instance B.Binary DotLabelExpr mtab where
  put (DotLabelExpr n nx loc) = B.prefixByte 0x81 $ B.put n >> B.put nx >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting DotLabelExpr"

instance B.HasPrefixTable DotLabelExpr Word8 mtab where
  prefixTable = B.mkPrefixTableWord8 "DotLabelExpr" 0x81 0x81 $
    [return DotLabelExpr <*> B.get <*> B.get <*> B.get]

instance PPrintable DotLabelExpr where { pPrint = pPrintInterm }

dotLabelToNameList :: DotLabelExpr -> [Name]
dotLabelToNameList (DotLabelExpr n nx _) = map undotNameExpr (n:nx)

----------------------------------------------------------------------------------------------------

-- | This is a list of 'Dao.String.Name's separated by dots. It is a pseudo-reference used to denote
-- things like constructor names in 'InitExpr', or setting the logical names of "import" modules
-- statements. It is basically a list of 'Dao.String.Name's that always has at least one element.
data AST_DotLabel = AST_DotLabel Name [AST_DotName] Location deriving (Eq, Ord, Show, Typeable)

instance NFData AST_DotLabel where
  rnf (AST_DotLabel n nx loc) = deepseq n $! deepseq nx $! deepseq loc ()

instance HasLocation AST_DotLabel where
  getLocation (AST_DotLabel _ _  loc)     = loc
  setLocation (AST_DotLabel n nx _  ) loc = AST_DotLabel n nx loc
  delLocation (AST_DotLabel n nx _  )     = AST_DotLabel n nx LocationUnknown

instance PPrintable AST_DotLabel where
  pPrint (AST_DotLabel n nx _) = pWrapIndent $ pPrint n : map pPrint nx

instance HasRandGen AST_DotLabel where
  randO = return AST_DotLabel <*> randO <*> randListOf 0 3 (return AST_DotName <*> randO <*> randO) <*> no
  defaultO = randO

instance Intermediate DotLabelExpr AST_DotLabel where
  toInterm   (AST_DotLabel              n  nx loc) = [DotLabelExpr] <*> [DotNameExpr n] <*> [nx >>= ti] <*> [loc]
  fromInterm (DotLabelExpr (DotNameExpr n) nx loc) = [AST_DotLabel] <*>             [n] <*> [nx >>= fi] <*> [loc]

dotLabelToRefExpr :: DotLabelExpr -> ReferenceExpr o
dotLabelToRefExpr (DotLabelExpr (DotNameExpr n) nx loc) =
  ReferenceExpr UNQUAL n (loop nx) loc where
    loop nx = case nx of
      []                 -> NullRefExpr
      (DotNameExpr n):nx -> DotRefExpr n (loop nx) LocationUnknown

refToDotLabelExpr :: ReferenceExpr o -> Maybe (DotLabelExpr, Maybe (ObjListExpr o))
refToDotLabelExpr o = case o of
  ReferenceExpr UNQUAL n suf loc ->
    loop (\nx loc ol -> (DotLabelExpr (DotNameExpr n) nx loc, ol)) [] loc suf
  _ -> mzero
  where
    loop f nx loc suf = case suf of
      NullRefExpr                 -> return (f nx loc Nothing)
      DotRefExpr   n  suf    loc' -> loop f (nx++[DotNameExpr n]) (loc<>loc') suf
      FuncCallExpr ol NullRefExpr -> return (f nx loc (Just ol))
      _                           -> mzero

dotLabelToRefAST :: AST_DotLabel -> AST_Reference o
dotLabelToRefAST (AST_DotLabel n nx loc) = AST_Reference UNQUAL [] n (loop nx) loc where
  loop nx = case nx of
    []                   -> AST_RefNull
    (AST_DotName c n):nx -> AST_DotRef c n (loop nx) LocationUnknown

refToDotLabelAST :: AST_Reference o -> Maybe (AST_DotLabel, Maybe (AST_ObjList o))
refToDotLabelAST o = case o of
  AST_Reference UNQUAL _ n suf loc -> loop (\nx loc ol -> (AST_DotLabel n nx loc, ol)) [] loc suf
  _                                -> mzero
  where
    loop f nx loc suf = case suf of
      AST_RefNull                 -> return (f nx loc Nothing)
      AST_DotRef c n  suf    loc' -> loop f (nx++[AST_DotName c n]) (loc<>loc') suf
      AST_FuncCall ol AST_RefNull -> return (f nx loc (Just ol))
      _                           -> mzero

----------------------------------------------------------------------------------------------------

data RefSuffixExpr o
  = NullRefExpr
  | DotRefExpr    Name        (RefSuffixExpr o) Location
  | SubscriptExpr (ObjListExpr o) (RefSuffixExpr o)
  | FuncCallExpr  (ObjListExpr o) (RefSuffixExpr o)
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (RefSuffixExpr o) where
  rnf  NullRefExpr          = ()
  rnf (DotRefExpr    a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (SubscriptExpr a b  ) = deepseq a $! deepseq b ()
  rnf (FuncCallExpr  a b  ) = deepseq a $! deepseq b ()

instance HasNullValue (RefSuffixExpr o) where
  nullValue = NullRefExpr
  testNull NullRefExpr = True
  testNull _           = False

instance HasLocation (RefSuffixExpr o) where
  getLocation o = case o of
    NullRefExpr           -> LocationUnknown
    DotRefExpr    _ _ loc -> loc
    SubscriptExpr a _     -> getLocation a
    FuncCallExpr  a _     -> getLocation a
  setLocation o loc = case o of
    NullRefExpr           -> NullRefExpr
    DotRefExpr    a b _   -> DotRefExpr    a b loc
    SubscriptExpr a b     -> SubscriptExpr (setLocation a loc) b
    FuncCallExpr  a b     -> FuncCallExpr  (setLocation a loc) b
  delLocation o     = case o of
    NullRefExpr           -> NullRefExpr
    DotRefExpr    a b _   -> DotRefExpr    a (delLocation b) LocationUnknown
    SubscriptExpr a b     -> SubscriptExpr a (delLocation b)
    FuncCallExpr  a b     -> FuncCallExpr  a (delLocation b)

----------------------------------------------------------------------------------------------------

-- | Anything that follows a reference, which could be square-bracketed indecies, function
-- parameters, or a dot and another reference. This is actually only a partial reference. The
-- 'Reference' is the full reference. The item selected by the 'Reference' is then further inspected
-- using a 'RefSuffix'; a 'RefSuffix' may be null, but a 'Reference' is never null.
data AST_RefSuffix o
  = AST_RefNull
  | AST_DotRef    (Com ()) Name (AST_RefSuffix o) Location
  | AST_Subscript (AST_ObjList o)   (AST_RefSuffix o)
  | AST_FuncCall  (AST_ObjList o)   (AST_RefSuffix o)
  deriving (Eq, Ord, Typeable, Show, Functor)

instance HasNullValue (AST_RefSuffix o) where
  nullValue = AST_RefNull
  testNull AST_RefNull = True
  testNull _ = False

instance HasLocation (AST_RefSuffix o) where
  getLocation o     = case o of
    AST_RefNull             -> LocationUnknown
    AST_DotRef    _ _ _ loc -> loc
    AST_Subscript a _       -> getLocation a
    AST_FuncCall  a _       -> getLocation a
  setLocation o loc = case o of
    AST_RefNull           -> AST_RefNull
    AST_DotRef    a b c _ -> AST_DotRef  a b c loc
    AST_Subscript a b     -> AST_Subscript (setLocation a loc) b
    AST_FuncCall  a b     -> AST_FuncCall  (setLocation a loc) b
  delLocation o     = case o of
    AST_RefNull           -> AST_RefNull
    AST_DotRef    a b c _ -> AST_DotRef  a b (delLocation c) LocationUnknown
    AST_Subscript a b     -> AST_Subscript (delLocation a) (delLocation b)
    AST_FuncCall  a b     -> AST_FuncCall  (delLocation a) (delLocation b)

instance PPrintable o => PPrintable (AST_RefSuffix o) where
  pPrint = pWrapIndent . loop where
    loop ref = case ref of
      AST_RefNull                  -> []
      AST_DotRef    dot name ref _ -> pPrintComWith (\ () -> pString ".") dot : pUStr (toUStr name) : loop ref
      AST_Subscript args     ref   -> pArgs "[]" args ++ loop ref
      AST_FuncCall  args     ref   -> pArgs "()" args ++ loop ref
      where
        pArgs str args = case str of
          (open:close:_) -> [pString [open], pPrint args, pString [close]]
          _              -> error "INTERNAL: bad instance definition of PPrintable AST_RefSuffix"

instance NFData o => NFData (AST_RefSuffix o) where
  rnf  AST_RefNull            = ()
  rnf (AST_DotRef    a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Subscript a b    ) = deepseq a $! deepseq b ()
  rnf (AST_FuncCall  a b    ) = deepseq a $! deepseq b ()

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_RefSuffix o) where
  randO = recurse $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_RefNull
    , scramble $ return AST_DotRef    <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_Subscript <*> randO <*> randO
    , scramble $ return AST_FuncCall  <*> randO <*> randO
    ]
  defaultO = runDefaultChoice
  defaultChoice = randChoiceList $
    [ return AST_RefNull
    , return AST_DotRef <*> defaultO <*> randO <*> pure AST_RefNull <*> no
    , return AST_Subscript <*> pure nullValue <*> pure AST_RefNull
    , return AST_FuncCall  <*> pure nullValue <*> pure AST_RefNull
    ]

instance Intermediate (RefSuffixExpr o) (AST_RefSuffix o) where
  toInterm   ast = case ast of
    AST_RefNull                -> [NullRefExpr]
    AST_DotRef  _ name ref loc -> [DotRefExpr]    <*> [name]        <*> toInterm ref <*> [loc]
    AST_Subscript args ref     -> [SubscriptExpr] <*> toInterm args <*> toInterm ref
    AST_FuncCall  args ref     -> [FuncCallExpr]  <*> toInterm args <*> toInterm ref
  fromInterm obj = case obj of
    NullRefExpr                -> [AST_RefNull]
    DotRefExpr    name ref loc -> [AST_DotRef] <*> [Com ()] <*> [name] <*> fromInterm ref <*> [loc]
    SubscriptExpr args ref     -> [AST_Subscript] <*> fromInterm args <*> fromInterm ref
    FuncCallExpr  args ref     -> [AST_FuncCall]  <*> fromInterm args <*> fromInterm ref

----------------------------------------------------------------------------------------------------

data ReferenceExpr o
  = RefObjectExpr (ParenExpr o)     (RefSuffixExpr o) Location
  | ReferenceExpr RefQualifier Name (RefSuffixExpr o) Location
    -- ^ reference suffixed by square brackets or round brackets. If the 3rd parameter is False, it
    -- is suffixed by square brackets, and True means suffixed by round brackets. Square brackets
    -- indicates an indexing expression, round brackets indicates a function call.
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (ReferenceExpr o) where
  rnf (RefObjectExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ReferenceExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue (ReferenceExpr o) where
  nullValue = RefObjectExpr nullValue NullRefExpr LocationUnknown
  testNull (RefObjectExpr a NullRefExpr LocationUnknown) = testNull a
  testNull _                                             = False

instance HasLocation (ReferenceExpr o) where
  getLocation o     = case o of
    RefObjectExpr _ _   loc -> loc
    ReferenceExpr _ _ _ loc -> loc
  setLocation o loc = case o of
    RefObjectExpr a b _     -> RefObjectExpr a b   loc
    ReferenceExpr a b c loc -> ReferenceExpr a b c loc
  delLocation o    = case o of
    RefObjectExpr a b _     -> RefObjectExpr (delLocation a) b   LocationUnknown
    ReferenceExpr a b c _   -> ReferenceExpr              a  b c LocationUnknown

instance PPrintable o => PPrintable (ReferenceExpr o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

data AST_Reference o
  = AST_RefObject (AST_Paren o)               (AST_RefSuffix o) Location
  | AST_Reference RefQualifier [Comment] Name (AST_RefSuffix o) Location
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (AST_Reference o) where
  rnf (AST_RefObject a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_Reference a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()

instance HasLocation (AST_Reference o) where
  getLocation o     = case o of
    AST_RefObject _ _     loc -> loc
    AST_Reference _ _ _ _ loc -> loc
  setLocation o loc = case o of
    AST_RefObject a b     _   -> AST_RefObject a b     loc
    AST_Reference a b c d _   -> AST_Reference  a b c d loc
  delLocation o     = case o of
    AST_RefObject a b     _   -> AST_RefObject (delLocation a) (delLocation b) LocationUnknown
    AST_Reference a b c d _   -> AST_Reference a b c (delLocation d) LocationUnknown

instance PPrintable o => PPrintable (AST_Reference o) where
  pPrint o = pInline $ case o of
    AST_RefObject  o           ref _ -> [pPrint o, pPrint ref]
    AST_Reference  q coms name ref _ -> concat $
      [ if q==UNQUAL then [] else [pPrint q, pString " "]
      , [pPrint coms, pUStr (toUStr name), pPrint ref]
      ]

instance PrecedeWithSpace (AST_Reference o) where { precedeWithSpace _ = True }

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_Reference o) where
  randO = countNode $ runRandChoice
  randChoice = randChoiceList $
    [ scramble $ return AST_Reference <*> randO <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_RefObject <*> randO <*> randO <*> no
    ]
  defaultO = runDefaultChoice
  defaultChoice = randChoiceList $
    [ return AST_Reference <*> defaultO <*> defaultO <*> defaultO <*> defaultO <*> no
    , return AST_RefObject <*> defaultO <*> defaultO <*> no
    ]

instance Intermediate (ReferenceExpr o) (AST_Reference o) where
  toInterm ast = case ast of
    AST_RefObject paren    ref loc -> [RefObjectExpr] <*> ti paren <*> ti ref            <*> [loc]
    AST_Reference q _ name ref loc -> [ReferenceExpr] <*> [q]      <*> [name] <*> ti ref <*> [loc]
  fromInterm o = case o of
    RefObjectExpr paren  ref loc -> [AST_RefObject] <*> fi paren <*> fi ref <*> [loc]
    ReferenceExpr q name ref loc ->
      [AST_Reference] <*> [q] <*> [[]] <*> [name]  <*> fi ref <*> [loc]

----------------------------------------------------------------------------------------------------

data RefPrefixExpr o
  = PlainRefExpr  (ReferenceExpr o)
  | RefPrefixExpr RefPfxOp (RefPrefixExpr o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

-- | Eliminate composed DEREF-REF operations
-- > @$a == a
cleanupRefPrefixExpr :: RefPrefixExpr o -> RefPrefixExpr o
cleanupRefPrefixExpr o =
  case o of { RefPrefixExpr DEREF (RefPrefixExpr REF o _) _ -> cleanupRefPrefixExpr o; _ -> o; }

instance NFData o => NFData (RefPrefixExpr o) where
  rnf (RefPrefixExpr a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (PlainRefExpr  a    ) = deepseq a ()

instance HasNullValue (RefPrefixExpr o) where
  nullValue = PlainRefExpr nullValue
  testNull (PlainRefExpr a) = testNull a
  testNull _ = False

instance HasLocation (RefPrefixExpr o) where
  getLocation o     = case o of
    PlainRefExpr      o -> getLocation o
    RefPrefixExpr _ _ o -> o
  setLocation o loc = case o of
    PlainRefExpr  a     -> PlainRefExpr (setLocation a loc)
    RefPrefixExpr a b _ -> RefPrefixExpr a b loc
  delLocation o     = case o of
    PlainRefExpr  a     -> PlainRefExpr (delLocation a)
    RefPrefixExpr a b _ -> RefPrefixExpr a (delLocation b) LocationUnknown

----------------------------------------------------------------------------------------------------

data AST_RefPrefix o
  = AST_PlainRef  (AST_Reference o)
  | AST_RefPrefix RefPfxOp [Comment] (AST_RefPrefix o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_RefPrefix o) where
  rnf (AST_PlainRef  a      ) = deepseq a ()
  rnf (AST_RefPrefix a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d  ()

instance HasLocation (AST_RefPrefix o) where
  getLocation o     = case o of
    AST_PlainRef  a         -> getLocation a
    AST_RefPrefix _ _ _ loc -> loc
  setLocation o loc = case o of
    AST_PlainRef  a         -> AST_PlainRef $ setLocation a loc
    AST_RefPrefix a b c _   -> AST_RefPrefix a b c loc
  delLocation o     = case o of
    AST_PlainRef  a         -> AST_PlainRef $ delLocation a
    AST_RefPrefix a b c _   -> AST_RefPrefix a b (delLocation c) LocationUnknown

instance PPrintable o => PPrintable (AST_RefPrefix o) where
  pPrint o = case o of
    AST_PlainRef o                   -> pPrint o
    AST_RefPrefix ariOp coms objXp _ -> pWrapIndent [pPrint ariOp, pPrint coms, pPrint objXp]

instance PrecedeWithSpace (AST_RefPrefix o) where
  precedeWithSpace o = case o of
    AST_PlainRef _ -> True
    _              -> False

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_RefPrefix o) where
  randO      = recurse $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_PlainRef  <*> randO
    , return AST_RefPrefix <*> randO <*> randO <*> randO <*> no
    ]
  defaultO      = runDefaultChoice
  defaultChoice = randChoiceList $
    [ AST_PlainRef <$> defaultO
    , return AST_RefPrefix <*> defaultO <*> defaultO <*> (AST_PlainRef <$> defaultO) <*> no
    ]

instance Intermediate (RefPrefixExpr o) (AST_RefPrefix o) where
  toInterm ast = case ast of
    AST_PlainRef  a         -> PlainRefExpr <$> toInterm a
    AST_RefPrefix a _ c loc -> [RefPrefixExpr] <*> [a] <*> toInterm c <*> [loc]
  fromInterm o = case o of
    PlainRefExpr  a       -> AST_PlainRef <$> fromInterm a
    RefPrefixExpr a c loc -> [AST_RefPrefix] <*> [a] <*> [[]] <*> fromInterm c <*> [loc]

----------------------------------------------------------------------------------------------------

-- | Contains a list of 'ObjectExpr's, which are used to encode parameters to function calls, and
-- intialization lists.
data ObjListExpr o = ObjListExpr [AssignExpr o] Location deriving (Eq, Ord, Typeable, Show, Functor)

instance Monoid (ObjListExpr o) where
  mempty = ObjListExpr [] LocationUnknown
  mappend (ObjListExpr a locA) (ObjListExpr b locB) = ObjListExpr (a++b) (locA<>locB)

instance NFData o => NFData (ObjListExpr o) where { rnf (ObjListExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue (ObjListExpr o) where
  nullValue = mempty
  testNull (ObjListExpr a _) = null a

instance HasLocation (ObjListExpr o) where
  getLocation (ObjListExpr _ loc)     = loc
  setLocation (ObjListExpr a _  ) loc = ObjListExpr (fmap delLocation a) loc
  delLocation (ObjListExpr a _  )     = ObjListExpr (fmap delLocation a) LocationUnknown

----------------------------------------------------------------------------------------------------

data AST_ObjList o = AST_ObjList [Comment] [Com (AST_Assign o)] Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance Monoid (AST_ObjList o) where
  mempty = AST_ObjList [] [] LocationUnknown
  mappend (AST_ObjList a1 a2 aloc) (AST_ObjList b1 b2 bloc) = AST_ObjList (a1++b1) (a2++b2) (aloc<>bloc)

instance HasNullValue (AST_ObjList o) where
  nullValue = mempty
  testNull (AST_ObjList [] [] _) = True
  testNull _ = False

instance HasLocation (AST_ObjList o) where
  getLocation (AST_ObjList _ _ loc)     = loc
  setLocation (AST_ObjList a b _  ) loc = AST_ObjList a      b  loc
  delLocation (AST_ObjList a b _  )     = AST_ObjList a (fmap delLocation b) LocationUnknown

instance PPrintable o => PPrintable (AST_ObjList o) where
  pPrint (AST_ObjList coms lst _) = pPrint coms >>
    pInline (intersperse (pString ", ") (map pPrint lst))

instance NFData o => NFData (AST_ObjList o) where { rnf (AST_ObjList a b c) = deepseq a $! deepseq b $! deepseq c () }

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_ObjList o) where
  randO = recurse $ depthLimitedInt 8 >>= \x -> AST_ObjList <$> randO <*> randList 0 x <*> no
  defaultO = return AST_ObjList <*> defaultO <*> pure [] <*> no

instance Intermediate (ObjListExpr o) (AST_ObjList o) where
  toInterm   (AST_ObjList _ lst loc) = [ObjListExpr]          <*> [lst>>=uc0] <*> [loc]
  fromInterm (ObjListExpr   lst loc) = [AST_ObjList] <*> [[]] <*> [lst>>=nc0] <*> [loc]

----------------------------------------------------------------------------------------------------

newtype OptObjListExpr o = OptObjListExpr (Maybe (ObjListExpr o))
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (OptObjListExpr o) where { rnf (OptObjListExpr a) = deepseq a () }

instance HasLocation (OptObjListExpr o) where
  getLocation (OptObjListExpr o)     = maybe LocationUnknown getLocation o
  setLocation (OptObjListExpr o) loc = OptObjListExpr (setLocation o loc)
  delLocation (OptObjListExpr o)     = OptObjListExpr (delLocation o    )

instance HasNullValue (OptObjListExpr o) where
  nullValue = OptObjListExpr Nothing
  testNull (OptObjListExpr Nothing) = True
  testNull _ = False

----------------------------------------------------------------------------------------------------

data AST_OptObjList o = AST_OptObjList [Comment] (Maybe (AST_ObjList o))
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_OptObjList o) where
  rnf (AST_OptObjList a b) = deepseq a $! deepseq b ()

instance HasNullValue (AST_OptObjList o) where
  nullValue = AST_OptObjList [] Nothing
  testNull (AST_OptObjList _ a) = maybe True testNull a

pPrintObjList :: PPrintable o => String -> String -> String -> AST_ObjList o -> PPrint
pPrintObjList open comma close (AST_ObjList coms lst _) = pList (pPrint coms) open comma close (map pPrint lst)

pPrintOptObjList :: PPrintable o => String -> String -> String -> AST_OptObjList o -> PPrint
pPrintOptObjList open comma close (AST_OptObjList coms o) =
  maybe (return ()) (\o -> pPrint coms >> pPrintObjList open comma close o) o

instance HasLocation (AST_OptObjList o) where
  getLocation (AST_OptObjList _ o)     = maybe LocationUnknown getLocation o
  setLocation (AST_OptObjList c o) loc = AST_OptObjList c (fmap (flip setLocation loc) o)
  delLocation (AST_OptObjList c o)     = AST_OptObjList c (fmap delLocation o)

instance PPrintable o => PPrintable (AST_OptObjList o) where { pPrint o = pPrintOptObjList "{" ", " "}" o }

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_OptObjList o) where
  randO = countNode $ return AST_OptObjList <*> randO <*> randO
  defaultO = return AST_OptObjList <*> defaultO <*> defaultO

instance Intermediate (OptObjListExpr o) (AST_OptObjList o) where
  toInterm   (AST_OptObjList _ o) = OptObjListExpr    <$> um1 o
  fromInterm (OptObjListExpr   o) = AST_OptObjList [] <$> nm1 o

----------------------------------------------------------------------------------------------------

data LiteralExpr o = LiteralExpr o Location deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (LiteralExpr o) where
  rnf (LiteralExpr a b) = deepseq a $! deepseq b ()

instance {- KEEP -} HasNullValue o => HasNullValue (LiteralExpr o) where
  nullValue = LiteralExpr nullValue LocationUnknown
  testNull (LiteralExpr o _) = testNull o

instance HasLocation (LiteralExpr o) where
  getLocation (LiteralExpr _ loc)     = loc
  setLocation (LiteralExpr o _  ) loc = LiteralExpr o loc
  delLocation (LiteralExpr o _  )     = LiteralExpr o LocationUnknown

----------------------------------------------------------------------------------------------------

data AST_Literal o = AST_Literal o Location deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_Literal o) where
  rnf (AST_Literal a b) = deepseq a $! deepseq b ()

instance {- KEEP -} HasNullValue o => HasNullValue (AST_Literal o) where
  nullValue = AST_Literal nullValue LocationUnknown
  testNull (AST_Literal o _) = testNull o

instance HasLocation (AST_Literal o) where
  getLocation (AST_Literal _ loc)     = loc
  setLocation (AST_Literal a _  ) loc = AST_Literal a loc
  delLocation (AST_Literal a _  )     = AST_Literal a LocationUnknown

instance PPrintable o => PPrintable (AST_Literal o) where
  pPrint (AST_Literal o _  ) = pPrint o

instance PrecedeWithSpace (AST_Literal o) where
  precedeWithSpace (AST_Literal _ _) = True

instance HasRandGen o => HasRandGen (AST_Literal o) where
  randO    = scramble $ return AST_Literal <*> defaultO <*> no
  defaultO = randO

instance Intermediate (LiteralExpr o) (AST_Literal o) where
  toInterm   (AST_Literal a loc) = [LiteralExpr] <*> [a] <*> [loc]
  fromInterm (LiteralExpr a loc) = [AST_Literal] <*> [a] <*> [loc]

----------------------------------------------------------------------------------------------------

-- | Required parenthesese.
data ParenExpr o = ParenExpr (AssignExpr o) Location deriving (Eq, Ord, Typeable, Show, Functor)

instance HasLocation (ParenExpr o) where
  getLocation (ParenExpr _ loc)     = loc
  setLocation (ParenExpr o _  ) loc = ParenExpr o loc
  delLocation (ParenExpr o _  )     = ParenExpr (delLocation o) LocationUnknown

instance HasNullValue (ParenExpr o) where
  nullValue = ParenExpr nullValue LocationUnknown
  testNull (ParenExpr a _) = testNull a

instance NFData o => NFData (ParenExpr o) where { rnf (ParenExpr a b) = deepseq a $! deepseq b () }

instance PPrintable o => PPrintable (ParenExpr o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

data AST_Paren o = AST_Paren (Com (AST_Assign o)) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance HasLocation (AST_Paren o) where
  getLocation (AST_Paren _ loc)     = loc
  setLocation (AST_Paren o _  ) loc = AST_Paren o loc
  delLocation (AST_Paren o _  )     = AST_Paren (delLocation o) LocationUnknown

instance HasNullValue (AST_Paren o) where
  nullValue = AST_Paren nullValue LocationUnknown
  testNull (AST_Paren a _) = testNull a

instance NFData o => NFData (AST_Paren o) where { rnf (AST_Paren a b) = deepseq a $! deepseq b () }

instance PPrintable o => PPrintable (AST_Paren o) where
  pPrint (AST_Paren o _) = pInline [pString "(", pPrint o, pString ")"]

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_Paren o) where
  randO    = recurse $ return AST_Paren <*> randO <*> no
  defaultO = return AST_Paren <*> defaultO <*> no

instance Intermediate (ParenExpr o) (AST_Paren o) where
  toInterm   (AST_Paren o loc) = [ParenExpr] <*> uc0 o <*> [loc]
  fromInterm (ParenExpr o loc) = [AST_Paren] <*> nc0 o <*> [loc]

----------------------------------------------------------------------------------------------------

data AssignExpr o
  = EvalExpr   (ObjTestExpr o)
  | AssignExpr (ObjTestExpr o) UpdateOp (AssignExpr o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AssignExpr o) where
  rnf (EvalExpr   a      ) = deepseq a ()
  rnf (AssignExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue (AssignExpr o) where
  nullValue = EvalExpr nullValue
  testNull (EvalExpr a) = testNull a
  testNull _ = False

instance HasLocation (AssignExpr o) where
  getLocation o     = case o of
    EvalExpr         o -> getLocation o
    AssignExpr _ _ _ o -> o
  setLocation o loc = case o of
    EvalExpr   a       -> EvalExpr  (setLocation a loc)
    AssignExpr a b c _ -> AssignExpr a b c loc
  delLocation o     = case o of
    EvalExpr   a       -> EvalExpr   (delLocation a)
    AssignExpr a b c _ -> AssignExpr (delLocation a) b (delLocation c) LocationUnknown

instance PPrintable o => PPrintable (AssignExpr o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

data AST_Assign o
  = AST_Eval   (AST_ObjTest o)
  | AST_Assign (AST_ObjTest o) (Com UpdateOp) (AST_Assign o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_Assign o) where
  rnf (AST_Eval   a      ) = deepseq a ()
  rnf (AST_Assign a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue (AST_Assign o) where
  nullValue = AST_Eval  nullValue
  testNull (AST_Eval  a) = testNull a
  testNull _ = False

instance HasLocation (AST_Assign o) where
  getLocation o = case o of
    AST_Eval         o -> getLocation o
    AST_Assign _ _ _ o -> o
  setLocation o loc = case o of
    AST_Eval         o -> AST_Eval  (setLocation o loc)
    AST_Assign a b c _ -> AST_Assign a b c loc
  delLocation o = case o of                            
    AST_Eval         o -> AST_Eval  (delLocation o)
    AST_Assign a b c _ -> AST_Assign (delLocation a) b (delLocation c) LocationUnknown

instance PPrintable o => PPrintable (AST_Assign o) where
  pPrint expr = case expr of
    AST_Eval  eq -> pPrint eq
    AST_Assign objXp1 comUpdOp objXp2 _ -> pWrapIndent $
      [pPrint objXp1, pPrint comUpdOp, pPrint objXp2]

instance PrecedeWithSpace (AST_Assign o) where
  precedeWithSpace o = case o of
    AST_Eval   o       -> precedeWithSpace o
    AST_Assign o _ _ _ -> precedeWithSpace o

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_Assign o) where
  randO = countNode $ recurse $ runRandChoice
  randChoice = randChoiceList $
    [ AST_Eval <$> randO
    , do ox <- randListOf 0 3 (pure (,) <*> randO <*> randO)
         o  <- randO
         return (foldr (\ (left, op) right -> AST_Assign left op right LocationUnknown) o ox)
    ]
  defaultO      = runDefaultChoice
  defaultChoice = randChoiceList $
    [ AST_Eval <$> defaultO
    , return AST_Assign <*> defaultO <*> defaultO <*> (AST_Eval <$> defaultO) <*> no
    ]

instance Intermediate (AssignExpr o) (AST_Assign o) where
  toInterm ast = case ast of
    AST_Eval   a         -> EvalExpr <$> ti a
    AST_Assign a b c loc -> [AssignExpr] <*> ti a <*> uc b <*> ti c <*> [loc]
  fromInterm o = case o of
    EvalExpr   a         -> AST_Eval <$> fi a
    AssignExpr a b c loc -> [AST_Assign] <*> fi a <*> nc b <*> fi c <*> [loc]

----------------------------------------------------------------------------------------------------

-- | A conditional expression of the form @a==b ? "yes" : "no"@
data ObjTestExpr o
  = ObjArithExpr    (ArithExpr    o)
  | ObjTestExpr     (ArithExpr    o) (ArithExpr o) (ArithExpr o) Location
  | ObjRuleFuncExpr (RuleFuncExpr o)
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (ObjTestExpr o) where
  rnf (ObjArithExpr  a    ) = deepseq a ()
  rnf (ObjTestExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ObjRuleFuncExpr a      ) = deepseq a $! ()

instance HasNullValue (ObjTestExpr o) where
  nullValue = ObjArithExpr nullValue
  testNull (ObjArithExpr a) = testNull a
  testNull _ = False

instance HasLocation (ObjTestExpr o) where
  getLocation o = case o of
    ObjArithExpr      a   -> getLocation a
    ObjTestExpr _ _ _ loc -> loc
    ObjRuleFuncExpr       o -> getLocation o
  setLocation o loc = case o of
    ObjArithExpr      a   -> ObjArithExpr (setLocation a loc)
    ObjTestExpr a b c _   -> ObjTestExpr a b c loc
    ObjRuleFuncExpr a       -> ObjRuleFuncExpr (setLocation a loc)
  delLocation o     = case o of
    ObjArithExpr      a   -> ObjArithExpr (delLocation a)
    ObjTestExpr a b c _   ->
      ObjTestExpr (delLocation a) (delLocation b) (delLocation c) LocationUnknown
    ObjRuleFuncExpr a       -> ObjRuleFuncExpr (delLocation a)

instance PPrintable o => PPrintable (ObjTestExpr o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

-- | A conditional expression of the form @a==b ? "yes" : "no"@
data AST_ObjTest o
  = AST_ObjArith    (AST_Arith    o)
  | AST_ObjTest     (AST_Arith    o) (Com ()) (AST_Arith o) (Com ()) (AST_Arith o) Location
  | AST_ObjRuleFunc (AST_RuleFunc o)
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (AST_ObjTest o) where
  rnf (AST_ObjArith  a        ) = deepseq a ()
  rnf (AST_ObjTest a b c d e f) =
    deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e $! deepseq f ()
  rnf (AST_ObjRuleFunc a      ) = deepseq a ()

instance HasNullValue (AST_ObjTest o) where
  nullValue = AST_ObjArith nullValue
  testNull (AST_ObjArith a) = testNull a
  testNull _ = False

instance HasLocation (AST_ObjTest o) where
  getLocation o     = case o of
    AST_ObjArith          a   -> getLocation a
    AST_ObjTest _ _ _ _ _ loc -> loc
    AST_ObjRuleFunc      o -> getLocation o
  setLocation o loc = case o of
    AST_ObjArith          a   -> AST_ObjArith (setLocation a loc)
    AST_ObjTest a b c d e _   -> AST_ObjTest a b c d e loc
    AST_ObjRuleFunc a         -> AST_ObjRuleFunc (setLocation a loc)
  delLocation o     = case o of
    AST_ObjArith          a   -> AST_ObjArith (delLocation a)
    AST_ObjTest a b c d e _   -> 
      AST_ObjTest (delLocation a) b (delLocation c) d (delLocation e) LocationUnknown
    AST_ObjRuleFunc a         -> AST_ObjRuleFunc (delLocation  a)

instance PPrintable o => PPrintable (AST_ObjTest o) where
  pPrint o = case o of
    AST_ObjArith  a         -> pPrint a
    AST_ObjTest a b c d e _ -> pWrapIndent $
      [ pPrint a
      , pPrintComWith (\ () -> pString " ? ") b, pPrint c
      , pPrintComWith (\ () -> pString " : ") d, pPrint e
      ]
    AST_ObjRuleFunc o                -> pPrint o

instance PrecedeWithSpace (AST_ObjTest o) where
  precedeWithSpace o = case o of
    AST_ObjArith          o -> precedeWithSpace o
    AST_ObjTest o _ _ _ _ _ -> precedeWithSpace o
    AST_ObjRuleFunc{}       -> True

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_ObjTest o) where
  randO      = countNode $ runRandChoice
  randChoice = randChoiceList $
    [ AST_ObjArith <$> randO
    , let p = pure (Com ()) in
        recurse $ scramble $ return AST_ObjTest <*> randO <*> p <*> randO <*> p <*> randO <*> no
    , AST_ObjRuleFunc <$> randO
    ]
  defaultO      = AST_ObjArith <$> defaultO
  defaultChoice = randChoiceList [defaultO]

instance Intermediate (ObjTestExpr o) (AST_ObjTest o) where
  toInterm   o = case o of
    AST_ObjArith    o         ->  ObjArithExpr <$> ti o
    AST_ObjTest a _ b _ c loc -> [ObjTestExpr] <*> ti a <*> ti b <*> ti c <*> [loc]
    AST_ObjRuleFunc a         -> [ObjRuleFuncExpr] <*> ti a
  fromInterm o = case o of
    ObjArithExpr  o       -> AST_ObjArith <$> fi o
    ObjTestExpr a b c loc ->
      [AST_ObjTest] <*> fi a <*> [Com ()] <*> fi b <*> [Com ()] <*> fi c <*> [loc]
    ObjRuleFuncExpr a         ->  AST_ObjRuleFunc  <$> fi a

----------------------------------------------------------------------------------------------------

data RefPfxOp = REF | DEREF deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance NFData RefPfxOp where { rnf a = seq a () }

instance UStrType RefPfxOp where
  toUStr op = ustr $ case op of
    REF         -> "$"
    DEREF       -> "@"
  maybeFromUStr str = case uchars str of
    "$" -> Just REF
    "@" -> Just DEREF
    _   -> Nothing
  fromUStr str = maybe (error (show str++" is not a prefix opretor")) id (maybeFromUStr str)

instance PPrintable RefPfxOp where { pPrint = pUStr . toUStr }

instance HasRandGen RefPfxOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::RefPfxOp)))
  defaultO = randO

----------------------------------------------------------------------------------------------------

data UpdateOp
  = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UPOW | UORB | UANDB | UXORB | USHL | USHR
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)
instance NFData UpdateOp where { rnf a = seq a () }

allUpdateOpStrs :: String
allUpdateOpStrs = " = += -= *= /= %= **= |= &= ^= <<= >>= "

instance UStrType UpdateOp where
  toUStr a = ustr $ case a of
    UCONST -> "="
    UADD   -> "+="
    USUB   -> "-="
    UMULT  -> "*="
    UDIV   -> "/="
    UMOD   -> "%="
    UPOW   -> "**="
    UORB   -> "|="
    UANDB  -> "&="
    UXORB  -> "^="
    USHL   -> "<<="
    USHR   -> ">>="
  maybeFromUStr str = case uchars str of
    "="   -> Just UCONST
    "+="  -> Just UADD  
    "-="  -> Just USUB  
    "*="  -> Just UMULT 
    "/="  -> Just UDIV  
    "%="  -> Just UMOD  
    "**=" -> Just UPOW
    "|="  -> Just UORB  
    "&="  -> Just UANDB 
    "^="  -> Just UXORB 
    "<<=" -> Just USHL  
    ">>=" -> Just USHR  
    _     -> Nothing
  fromUStr str =
    maybe (error (show str++" is not an assignment/update operator")) id (maybeFromUStr str)

instance PPrintable UpdateOp where { pPrint op = pString (' ':uchars op++" ") }

-- binary 0x8D 0x9D UpdateOp-->InfixOp
instance B.Binary UpdateOp mtab where
  put o = B.putWord8 $ case o of
    UCONST -> 0x8D
    UADD   -> 0x93
    USUB   -> 0x94
    UMULT  -> 0x95
    UDIV   -> 0x96
    UMOD   -> 0x97
    UPOW   -> 0x98
    UORB   -> 0x99
    UANDB  -> 0x9A
    UXORB  -> 0x9B
    USHL   -> 0x9C
    USHR   -> 0x9D
  get = B.word8PrefixTable <|> fail "expecting UpdateOp"

instance B.HasPrefixTable UpdateOp B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "UpdateOp" 0x8D 0x9D $ let {r=return;z=mzero} in
    [ r UCONST -- 0x8D
    , z, z, z, z, z -- 0x8E,0x8F,0x90,0x91,0x92
    , r UADD, r USUB, r UMULT, r UDIV, r UMOD, r UPOW, r UORB -- 0x93,0x94,0x95,0x96,0x97,0x98,0x99
    , r UANDB, r UXORB, r USHL, r USHR -- 0x9A,0x9B,0x9C,0x9D
    ]

instance HasRandGen UpdateOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::UpdateOp)))
  defaultO = randO

----------------------------------------------------------------------------------------------------

data ArithExpr o
  = ObjectExpr (ObjectExpr o)
  | ArithExpr  (ArithExpr  o) InfixOp (ArithExpr o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (ArithExpr o) where
  rnf (ObjectExpr a      ) = deepseq a ()
  rnf (ArithExpr  a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue (ArithExpr o) where
  nullValue = ObjectExpr nullValue
  testNull (ObjectExpr a) = testNull a
  testNull _ = False

instance HasLocation (ArithExpr o) where
  getLocation o     = case o of
    ObjectExpr      o -> getLocation o
    ArithExpr _ _ _ o -> o
  setLocation o loc = case o of
    ObjectExpr  a     -> ObjectExpr (setLocation a loc)
    ArithExpr a b c _ -> ArithExpr a b c loc
  delLocation o     = case o of
    ObjectExpr  a     -> ObjectExpr (delLocation a)
    ArithExpr a b c _ -> ArithExpr (delLocation a) b (delLocation c) LocationUnknown

instance PPrintable o => PPrintable (ArithExpr o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

data AST_Arith o
  = AST_Object (AST_Object o)
  | AST_Arith  (AST_Arith  o) (Com InfixOp) (AST_Arith o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_Arith o) where
  rnf (AST_Object   a        ) = deepseq a ()
  rnf (AST_Arith a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue (AST_Arith o) where
  nullValue = AST_Object nullValue
  testNull (AST_Object a) = testNull a
  testNull _ = False

instance HasLocation (AST_Arith o) where
  getLocation o     = case o of
    AST_Object      o -> getLocation o
    AST_Arith _ _ _ o -> o
  setLocation o loc = case o of
    AST_Object   a    -> AST_Object (setLocation a loc)
    AST_Arith a b c _ -> AST_Arith a b c loc
  delLocation o     = case o of
    AST_Object   a    -> AST_Object (delLocation a)
    AST_Arith a b c _ -> AST_Arith (delLocation a) b (delLocation c) LocationUnknown

instance PPrintable o => PPrintable (AST_Arith o) where
  pPrint o = case o of
    AST_Object o -> pPrint o
    AST_Arith objXp1 comAriOp objXp2 _ -> pWrapIndent [pPrint objXp1, pPrint comAriOp, pPrint objXp2]

instance PrecedeWithSpace (AST_Arith o) where
  precedeWithSpace o = case o of
    AST_Object o       -> precedeWithSpace o
    AST_Arith  o _ _ _ -> precedeWithSpace o

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_Arith o) where
  randO = countNode $ runRandChoice
  defaultChoice = randChoiceList $
    [ AST_Object <$> defaultO
    , return AST_Arith <*> (AST_Object <$> defaultO) <*> defaultO <*> (AST_Object <$> defaultO) <*> no
    ]
  defaultO   = runDefaultChoice
  randChoice = randChoiceList $
    [ AST_Object <$> randO
    , do  left <- AST_Object <$> randO
          x    <- getCurrentDepth
          ops  <- randListOf 0 (max 1 (4-x)) $
            pure (,) <*> randInfixOp <*> (AST_Object <$> randO)
          return $ foldPrec left ops
    ] where
      randInfixOp :: RandO (Com InfixOp, Int, Bool)
      randInfixOp = do
        (op, prec, assoc) <- runRandChoiceOf opGroups
        op <- randComWith (return op)
        return (op, prec, assoc)
      left  op = (True , op)
      right op = (False, op)
      opGroups :: RandChoice (InfixOp, Int, Bool)
      opGroups = randChoiceList $ map return $ do
        (precedence, (associativity, operators)) <- zip [1..] $ concat $
          [ map right [[OR], [AND], [EQUL, NEQUL]]
          , map left $
              [ [GTN, LTN, GTEQ, LTEQ], [SHL, SHR]
              , [ORB], [XORB], [ANDB]
              , [ADD, SUB], [MULT, DIV, MOD]
              ]
          , map right [[POW], [ARROW]]
          ]
        operator <- operators
        return (operator, precedence, associativity)
      bind left op right = AST_Arith left op right LocationUnknown
      foldPrec left ops = case ops of
        [] -> left
        ((op, prec, _), right):ops -> case scanRight prec right ops of
          (right, ops) -> foldPrec (bind left op right) ops
      scanRight prevPrec left ops = case ops of
        [] -> (left, [])
        ((op, prec, assoc), right):next -> 
          if prevPrec<prec || (prevPrec==prec && not assoc)
          then  case scanRight prec right next of
                  (right, next) -> scanRight prevPrec (bind left op right) next
          else  (left, ops)

instance Intermediate (ArithExpr o) (AST_Arith o) where
  toInterm o = case o of
    AST_Object  a       -> ObjectExpr <$> ti a
    AST_Arith a b c loc -> [ArithExpr ] <*> ti a <*> uc b <*> ti c <*> [loc]
  fromInterm o = case o of
    ObjectExpr  a       -> AST_Object <$> fi a
    ArithExpr a b c loc -> [AST_Arith ] <*> fi a <*> nc b <*> fi c <*> [loc]

----------------------------------------------------------------------------------------------------

data ObjectExpr o
  = VoidExpr
  | ObjLiteralExpr  (LiteralExpr   o)
  | ObjSingleExpr   (RefPrefixExpr o)
  | ArithPfxExpr     ArithPfxOp       (ObjectExpr     o)                  Location
  | InitExpr         DotLabelExpr     (OptObjListExpr o)  (ObjListExpr o) Location
  | StructExpr       Name             (OptObjListExpr o)                  Location
  | MetaEvalExpr    (CodeBlock     o)                                     Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (ObjectExpr o) where
  rnf  VoidExpr                 = ()
  rnf (ObjLiteralExpr  a      ) = deepseq a ()
  rnf (ObjSingleExpr   a      ) = deepseq a $! ()
  rnf (ArithPfxExpr    a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (InitExpr        a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (StructExpr      a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (MetaEvalExpr    a b    ) = deepseq a $! deepseq b ()

instance HasNullValue (ObjectExpr o) where
  nullValue = VoidExpr
  testNull VoidExpr = True
  testNull _        = False

instance HasLocation (ObjectExpr o) where
  getLocation o = case o of
    VoidExpr                -> LocationUnknown
    ObjLiteralExpr        o -> getLocation o
    ObjSingleExpr         o -> getLocation o
    ArithPfxExpr    _ _   o -> o
    InitExpr        _ _ _ o -> o
    StructExpr      _ _   o -> o
    MetaEvalExpr    _     o -> o
  setLocation o loc = case o of
    VoidExpr                -> VoidExpr
    ObjLiteralExpr  a       -> ObjLiteralExpr  (setLocation a loc)
    ObjSingleExpr   a       -> ObjSingleExpr   (setLocation a loc)
    ArithPfxExpr    a b   _ -> ArithPfxExpr    a b   loc
    InitExpr        a b c _ -> InitExpr        a b c loc
    StructExpr      a b   _ -> StructExpr      a b   loc
    MetaEvalExpr    a     _ -> MetaEvalExpr    a     loc
  delLocation o = case o of
    VoidExpr                -> VoidExpr
    ObjLiteralExpr  a       -> ObjLiteralExpr  (fd a)
    ObjSingleExpr   a       -> ObjSingleExpr   (fd a)
    ArithPfxExpr    a b   _ -> ArithPfxExpr        a  (fd b)        lu
    InitExpr        a b c _ -> InitExpr        (fd a) (fd b) (fd c) lu
    StructExpr      a b   _ -> StructExpr      (fd a) (fd b)        lu
    MetaEvalExpr    a     _ -> MetaEvalExpr    (fd a)               lu
    where
      lu = LocationUnknown
      fd :: HasLocation a => a -> a
      fd = delLocation

instance PPrintable o => PPrintable (ObjectExpr o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data AST_Object o
  = AST_Void -- ^ Not a language construct, but used where an object expression is optional.
  | AST_ObjLiteral  (AST_Literal o)
  | AST_ObjSingle   (AST_RefPrefix o)
  | AST_ArithPfx     ArithPfxOp      [Comment]          (AST_Object  o)   Location
  | AST_Init         AST_DotLabel    (AST_OptObjList o) (AST_ObjList o)   Location
  | AST_Struct       Name            (AST_OptObjList o)                   Location
  | AST_MetaEval                     (AST_CodeBlock  o)                   Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_Object o) where
  rnf AST_Void = ()
  rnf (AST_ObjLiteral  a      ) = deepseq a ()
  rnf (AST_ObjSingle   a      ) = deepseq a ()
  rnf (AST_ArithPfx    a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Init        a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Struct      a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_MetaEval    a b    ) = deepseq a $! deepseq b ()

instance HasNullValue (AST_Object o) where
  nullValue = AST_Void
  testNull AST_Void = True
  testNull _        = False

instance HasLocation (AST_Object o) where
  getLocation o = case o of
    AST_Void               -> LocationUnknown
    AST_ObjLiteral       o -> getLocation o
    AST_ObjSingle        o -> getLocation o
    AST_ArithPfx _ _ _   o -> o
    AST_Init     _ _ _   o -> o
    AST_Struct   _ _     o -> o
    AST_MetaEval _       o -> o
  setLocation o loc = case o of
    AST_Void                  -> AST_Void
    AST_ObjLiteral  a         -> AST_ObjLiteral  (setLocation a loc)
    AST_ObjSingle   a         -> AST_ObjSingle   (setLocation a loc)
    AST_ArithPfx    a b c   _ -> AST_ArithPfx    a b c   loc
    AST_Init        a b c   _ -> AST_Init        a b c   loc
    AST_Struct      a b     _ -> AST_Struct      a b     loc
    AST_MetaEval    a       _ -> AST_MetaEval    a       loc
  delLocation o = case o of                            
    AST_Void                  -> AST_Void
    AST_ObjLiteral  a         -> AST_ObjLiteral   (delLocation  a)
    AST_ObjSingle   a         -> AST_ObjSingle    (delLocation  a)
    AST_ArithPfx    a b c   _ -> AST_ArithPfx a b (delLocation  c) LocationUnknown
    AST_Init        a b c   _ -> AST_Init         (delLocation  a) (delLocation  b) (delLocation  c) LocationUnknown
    AST_Struct      a b     _ -> AST_Struct   a   (delLocation  b) LocationUnknown
    AST_MetaEval    a       _ -> AST_MetaEval     (delLocation  a) LocationUnknown

instance PPrintable o => PPrintable (AST_Object o) where
  pPrint expr = case expr of
    AST_Void                         -> return ()
    AST_ObjLiteral  o                -> pPrint o
    AST_ObjSingle   o                -> pPrint o
    AST_ArithPfx    op coms objXp  _ -> pWrapIndent $
      [pPrint op, pPrint coms, pPrint objXp]
    AST_Init          ref objs     elems   _ ->
      pInline [pPrint ref, pPrintOptObjList "(" ", " ")" objs, pPrintObjList "{" ", " "}" elems]
    AST_Struct      nm itms        _ -> case itms of
      AST_OptObjList coms items -> do
        let name = pString $ '#' : uchars (toUStr nm)
        pPrint coms
        case items of
          Nothing -> name
          Just (AST_ObjList coms items _) -> do
            pPrint coms
            pList name "{" ", " "}" $ map pPrint items
    AST_MetaEval cObjXp                    _ -> pInline [pString "${", pPrint cObjXp, pString "}"]

instance PrecedeWithSpace (AST_Object o) where
  precedeWithSpace o = case o of
    AST_Void         -> False
    AST_MetaEval{}   -> False
    AST_ObjSingle  o -> precedeWithSpace o
    _                -> True

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_Object o) where
  randO      = countNode $ runRandChoice
  randChoice = randChoiceList $
    [ AST_ObjLiteral  <$> randO
    , AST_ObjSingle   <$> randO
    , pure AST_ArithPfx <*> randO <*> randO <*> randO <*> no
    , pure AST_Init     <*> randO <*> randO <*> randO <*> no
    , pure AST_MetaEval <*> randO <*> no
    ]
  defaultO      = runDefaultChoice
  defaultChoice = randChoiceList $
    [ AST_ObjLiteral  <$> defaultO
    , AST_ObjSingle   <$> defaultO
    , return AST_ArithPfx <*> defaultO <*> defaultO <*> (AST_ObjLiteral <$> defaultO) <*> no
    , return AST_Init     <*> defaultO <*> defaultO <*> defaultO <*> no
    , return AST_Struct   <*> defaultO <*> defaultO <*> no
    ]

instance (HasNullValue o, HasRandGen o) => HasRandGen [Com (AST_Object o)] where
  randO = depthLimitedInt 24 >>= \x -> countNode $ randList 1 x

instance Intermediate (ObjectExpr o) (AST_Object o) where
  toInterm ast = case ast of
    AST_Void                  -> [VoidExpr       ]
    AST_ObjLiteral  a         ->  ObjLiteralExpr   <$> ti a
    AST_ObjSingle   a         -> [ObjSingleExpr  ] <*> ti a
    AST_ArithPfx    a _ c loc -> [ArithPfxExpr   ] <*>   [a]          <*> ti c <*> [loc]
    AST_Init        a b c loc -> [InitExpr       ] <*> ti a  <*> ti b <*> ti c <*> [loc]
    AST_Struct      a b   loc -> [StructExpr     ] <*>   [a] <*> ti b          <*> [loc]
    AST_MetaEval    a     loc -> [MetaEvalExpr   ] <*> ti a                    <*> [loc]
  fromInterm o = case o of
    VoidExpr                  -> [AST_Void       ]
    ObjLiteralExpr  a         ->  AST_ObjLiteral   <$> fi a
    ObjSingleExpr   a         ->  AST_ObjSingle    <$> fi a
    ArithPfxExpr    a b   loc -> [AST_ArithPfx   ] <*>   [a] <*> [[]] <*> fi b <*> [loc]
    InitExpr        a b c loc -> [AST_Init       ] <*> fi a  <*> fi b <*> fi c <*> [loc]
    StructExpr      a b   loc -> [AST_Struct     ] <*>   [a] <*> fi b          <*> [loc]
    MetaEvalExpr    a     loc -> [AST_MetaEval   ] <*> fi a                    <*> [loc]

----------------------------------------------------------------------------------------------------

-- | Functions and function parameters can specify optional type-checking expressions. This is a
-- data type that wraps a dao-typeable expression with type information.
data TyChkExpr a o
  = NotTypeChecked{tyChkItem::a}
    -- ^ no type information was specified for this item
  | TypeChecked   {tyChkItem::a, tyChkExpr::ArithExpr o, tyChkLoc::Location}
    -- ^ type check information was specified and should be checked every time it is evaluated.
  | DisableCheck  {tyChkItem::a, tyChkExpr::ArithExpr o, typChkResult::o, tyChkLoc::Location}
    -- ^ type check information was specified but has been disabled for efficiency reasons because
    -- we have verified that the item will always return a succesfull type-check.
  deriving (Eq, Ord, Typeable, Show)

checkedExpr :: TyChkExpr a o -> a
checkedExpr o = case o of
  NotTypeChecked o       -> o
  TypeChecked    o _ _   -> o
  DisableCheck   o _ _ _ -> o

instance Functor (TyChkExpr a) where
  fmap _ (NotTypeChecked   a  ) = NotTypeChecked a
  fmap f (TypeChecked  a b c  ) = TypeChecked  a (fmap f b) c
  fmap f (DisableCheck a b c d) = DisableCheck a (fmap f b) (f c) d

fmapCheckedValueExpr :: (a -> b) -> TyChkExpr a o -> TyChkExpr b o
fmapCheckedValueExpr f a = case a of
  NotTypeChecked   a   -> NotTypeChecked (f a)
  TypeChecked  a b c   -> TypeChecked  (f a) b c
  DisableCheck a b c d -> DisableCheck (f a) b c d

instance (NFData o, NFData a) => NFData (TyChkExpr a o) where
  rnf (NotTypeChecked   a  ) = deepseq a ()
  rnf (TypeChecked  a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (DisableCheck a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance (HasNullValue o, HasNullValue a) => HasNullValue (TyChkExpr a o) where
  nullValue = NotTypeChecked nullValue
  testNull (NotTypeChecked a) = testNull a
  testNull _ = False

instance HasLocation a => HasLocation (TyChkExpr a o) where
  getLocation a     = case a of
    NotTypeChecked a         -> getLocation a
    TypeChecked    a _   loc -> getLocation a <> loc
    DisableCheck   a _ _ loc -> getLocation a <> loc
  setLocation a loc = case a of
    NotTypeChecked a       -> NotTypeChecked (setLocation a loc)
    TypeChecked    a b   _ -> TypeChecked  a b loc
    DisableCheck   a b c _ -> DisableCheck a b c loc
  delLocation a     = case a of
    NotTypeChecked a       -> NotTypeChecked (delLocation a)
    TypeChecked    a b   _ -> TypeChecked (delLocation a) (delLocation b) LocationUnknown
    DisableCheck   a b c _ -> DisableCheck a b c LocationUnknown

instance (PPrintable o, PPrintable a) => PPrintable (TyChkExpr a o) where
  pPrint a = case a of
    NotTypeChecked a        -> pPrint a
    TypeChecked    a expr _ -> pInline [pPrint a, pString ": ", pPrint expr]
    DisableCheck   a  _ _ _ -> pInline [pPrint a]

----------------------------------------------------------------------------------------------------

-- | This node can be found in a few different syntactic structures. When a name or function or
-- expression is followed by a colon and some type checking information, this node is used for that
-- purpose.
data AST_TyChk a o
  = AST_NotChecked a
  | AST_Checked    a (Com ()) (AST_Arith o) Location
  deriving (Eq, Ord, Typeable, Show)

checkedAST :: AST_TyChk a o -> a
checkedAST a = case a of { AST_NotChecked a -> a; AST_Checked a _ _ _ -> a; }

astTyChkDelLocWith :: (a -> a) -> AST_TyChk a o -> AST_TyChk a o
astTyChkDelLocWith del a = case a of
  AST_NotChecked a       -> AST_NotChecked (del a)
  AST_Checked    a b c _ -> AST_Checked    (del a) b (delLocation c) LocationUnknown

instance Functor (AST_TyChk o) where
  fmap _ (AST_NotChecked a      ) = AST_NotChecked a
  fmap f (AST_Checked    a b c d) = AST_Checked    a b (fmap f c) d

fmapCheckedValueAST :: (a -> b) -> AST_TyChk a o -> AST_TyChk b o
fmapCheckedValueAST f a = case a of
  AST_NotChecked a       -> AST_NotChecked (f a)
  AST_Checked    a b c d -> AST_Checked    (f a) b c d

instance (NFData o, NFData a) => NFData (AST_TyChk a o) where
  rnf (AST_NotChecked    a) = deepseq a ()
  rnf (AST_Checked a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance (HasNullValue o, HasNullValue a) => HasNullValue (AST_TyChk a o) where
  nullValue = AST_NotChecked nullValue
  testNull (AST_NotChecked  a  ) = testNull a
  testNull (AST_Checked _ _ a _) = testNull a

instance (PPrintable o, PPrintable a) => PPrintable (AST_TyChk a o) where
  pPrint a = case a of
    AST_NotChecked a          -> pPrint a
    AST_Checked    a coms expr _ -> pInline $
      [ pPrint a
      , pPrintComWith (\ () -> pString "::") coms
      , pPrint expr
      ]

instance HasLocation a => HasLocation (AST_TyChk a o) where
  getLocation a     = case a of
    AST_NotChecked a         -> getLocation a
    AST_Checked    a _ _ loc -> getLocation a <> loc
  setLocation a loc = case a of
    AST_NotChecked a         -> AST_NotChecked (setLocation a loc)
    AST_Checked    a b c _   -> AST_Checked a b c loc
  delLocation       = astTyChkDelLocWith delLocation

instance (HasRandGen o, HasRandGen a) => HasRandGen (AST_TyChk a o) where
  randO    = countNode $ AST_NotChecked <$> randO
  --randChoice = randChoiceList [AST_NotChecked <$> randO, return AST_Checked <*> randO <*> randO <*> randO <*> no]
  defaultO = AST_NotChecked <$> defaultO

tyChkToInterm :: (b -> [a]) -> AST_TyChk b o -> [TyChkExpr a o]
tyChkToInterm ti a = case a of
  AST_NotChecked a         -> NotTypeChecked <$> ti a
  AST_Checked    a _ b loc -> [TypeChecked] <*> ti a <*> toInterm b <*> [loc]

tyChkFromInterm :: (a -> [b]) -> TyChkExpr a o -> [AST_TyChk b o]
tyChkFromInterm fi a = case a of
  NotTypeChecked a         -> AST_NotChecked <$> fi a
  TypeChecked    a b   loc -> [AST_Checked] <*> fi a <*> [Com ()] <*> fromInterm b <*> [loc]
  DisableCheck   a b _ loc -> [AST_Checked] <*> fi a <*> [Com ()] <*> fromInterm b <*> [loc]

----------------------------------------------------------------------------------------------------

-- | A list of function parameters (arguments) to a function in an object representing a function
-- expression.
data ParamListExpr o = ParamListExpr (TyChkExpr [ParamExpr o] o) Location
  deriving (Eq, Ord, Typeable, Show)

instance Functor ParamListExpr where
  fmap f (ParamListExpr a loc) =
    ParamListExpr (fmapCheckedValueExpr (fmap (fmap f)) $ fmap f a) loc

instance NFData o => NFData (ParamListExpr o) where { rnf (ParamListExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue (ParamListExpr o) where
  nullValue = ParamListExpr (NotTypeChecked []) LocationUnknown
  testNull (ParamListExpr (NotTypeChecked []) _) = True
  testNull _ = False

instance HasLocation (ParamListExpr o) where
  getLocation (ParamListExpr _ loc)     = loc
  setLocation (ParamListExpr a _  ) loc = ParamListExpr a loc
  delLocation (ParamListExpr a _  )     = ParamListExpr a LocationUnknown

instance PPrintable o => PPrintable (ParamListExpr o) where { pPrint (ParamListExpr lst _) = pPrint lst }

getTypeCheckList :: ParamListExpr o -> [ParamExpr o]
getTypeCheckList (ParamListExpr tychk _) = tyChkItem tychk 

----------------------------------------------------------------------------------------------------

-- | 'ParamExpr' is a part of the Dao language semantics, and is also used in the the 'CallableCode'
-- data type when evaluating parameters to be passed to the callable code function execution. The
-- boolean parameter here indicates whether or not the parameter should be passed by reference.
data ParamExpr o = ParamExpr Bool (TyChkExpr Name o) Location deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (ParamExpr o) where
  rnf (ParamExpr       a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasLocation (ParamExpr o) where
  getLocation (ParamExpr _ _ loc)     = loc
  setLocation (ParamExpr a b _  ) loc = ParamExpr a b loc
  delLocation (ParamExpr a b _  )     = ParamExpr a b LocationUnknown

instance PPrintable o => PPrintable (ParamExpr o) where
  pPrint (ParamExpr byRef tychk _) = when byRef (pString "$") >> pPrint tychk

instance PPrintable o => PPrintable [ParamExpr o] where { pPrint lst = pList_ "(" ", " ")" (fmap pPrint lst) }

----------------------------------------------------------------------------------------------------

data AST_Param o
  = AST_NoParams
  | AST_Param (Maybe [Comment]) (AST_TyChk Name o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_Param o) where
  rnf  AST_NoParams     = ()
  rnf (AST_Param a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue (AST_Param o) where
  nullValue = AST_NoParams
  testNull AST_NoParams = True
  testNull _ = False

instance HasLocation (AST_Param o) where
  getLocation a     = case a of
    AST_NoParams      -> LocationUnknown
    AST_Param _ _ loc -> loc
  setLocation a loc = case a of
    AST_NoParams    -> AST_NoParams
    AST_Param a b _ -> AST_Param a b loc
  delLocation a     = case a of
    AST_NoParams    -> AST_NoParams
    AST_Param a b _ -> AST_Param a (astTyChkDelLocWith delLocation b) LocationUnknown

instance PPrintable o => PPrintable (AST_Param o) where
  pPrint o = case o of
    AST_NoParams            -> return ()
    AST_Param mcoms tychk _ -> pInline $
      [ maybe (return ()) (\coms -> pString "$" >> pPrint coms) mcoms
      , pPrint tychk
      ]

instance PPrintable o => PPrintable [Com (AST_Param o)] where
  pPrint lst = pList_ "(" ", " ")" (fmap pPrint lst)

----------------------------------------------------------------------------------------------------

instance HasRandGen o => HasRandGen (AST_Param o) where
  randO    = countNode $ return AST_Param <*> randO <*> randO <*> no
  defaultO = return AST_Param <*> defaultO <*> defaultO <*> no

instance HasRandGen o => HasRandGen [Com (AST_Param o)] where
  randO    = recurse $ depthLimitedInt 8 >>= \x -> randListOf 0 x scrambO
  defaultO = defaultList 0 1

instance Intermediate (ParamExpr o) (AST_Param o) where
  toInterm   a = case a of
    AST_NoParams      -> []
    AST_Param a b loc ->
      [ParamExpr] <*> [maybe False (const True) a]     <*> tyChkToInterm   return b <*> [loc]
  fromInterm o = case o of
    ParamExpr a b loc ->
      [AST_Param] <*> [if a then Just [] else Nothing] <*> tyChkFromInterm return b <*> [loc]

instance Intermediate [ParamExpr o] [Com (AST_Param o)] where
  toInterm   ax = [ax >>= toInterm . unComment]
  fromInterm ax = [ax >>= fmap Com . fromInterm]

----------------------------------------------------------------------------------------------------

data AST_ParamList o
  = AST_ParamList (AST_TyChk [Com (AST_Param o)] o) Location
  deriving (Eq, Ord, Typeable, Show)

instance Functor AST_ParamList where
  fmap f (AST_ParamList a loc) =
    AST_ParamList (fmapCheckedValueAST (fmap (fmap (fmap f))) $ fmap f a) loc

instance NFData o => NFData (AST_ParamList o) where { rnf (AST_ParamList a b) = deepseq a $! deepseq b () }

instance HasNullValue o => HasNullValue (AST_ParamList o) where
  nullValue = AST_ParamList nullValue LocationUnknown
  testNull (AST_ParamList a _) = testNull a

instance HasLocation (AST_ParamList o) where
  getLocation (AST_ParamList _ loc)     = loc
  setLocation (AST_ParamList a _  ) loc = AST_ParamList a loc
  delLocation (AST_ParamList a _  )     = AST_ParamList (astTyChkDelLocWith (fmap delLocation) a) LocationUnknown

instance PPrintable o => PPrintable (AST_ParamList o) where
  pPrint (AST_ParamList lst _) = pInline [pPrint lst]

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_ParamList o) where
  randO    = countNode $ return AST_ParamList <*> randO <*> no
  defaultO = return $ AST_ParamList nullValue LocationUnknown

instance Intermediate (ParamListExpr o) (AST_ParamList o) where
  toInterm   (AST_ParamList ox loc) = [ParamListExpr] <*> tyChkToInterm   toInterm   ox <*> [loc]
  fromInterm (ParamListExpr ox loc) = [AST_ParamList] <*> tyChkFromInterm fromInterm ox <*> [loc]

----------------------------------------------------------------------------------------------------

data RuleFuncExpr o
  = LambdaExpr    (ParamListExpr o) (CodeBlock o) Location
  | FuncExpr Name (ParamListExpr o) (CodeBlock o) Location
  | RuleExpr      (RuleHeadExpr  o) (CodeBlock o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (RuleFuncExpr o) where
  rnf (LambdaExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (FuncExpr   a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (RuleExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue (RuleFuncExpr o) where
  nullValue = LambdaExpr nullValue nullValue LocationUnknown
  testNull (RuleExpr a b _) = testNull a && testNull b
  testNull _                = False

instance HasLocation (RuleFuncExpr o) where
  getLocation o = case o of
    LambdaExpr _ _   o -> o
    FuncExpr   _ _ _ o -> o
    RuleExpr   _ _   o -> o
  setLocation o loc = case o of
    LambdaExpr a b   _ -> LambdaExpr a b   loc
    FuncExpr   a b c _ -> FuncExpr   a b c loc
    RuleExpr   a b   _ -> RuleExpr   a b   loc
  delLocation o = case o of
    LambdaExpr a b   _ -> LambdaExpr (delLocation a) (delLocation b) LocationUnknown
    FuncExpr   a b c _ -> FuncExpr a (delLocation b) (delLocation c) LocationUnknown
    RuleExpr   a b   _ -> RuleExpr a (delLocation b)                 LocationUnknown

instance PPrintable o => PPrintable (RuleFuncExpr o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

data AST_RuleFunc o
  = AST_Lambda              (Com (AST_ParamList  o)) (AST_CodeBlock o) Location
  | AST_Func [Comment] Name (Com (AST_ParamList  o)) (AST_CodeBlock o) Location
  | AST_Rule                (Com (AST_RuleHeader o)) (AST_CodeBlock o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_RuleFunc o) where
  rnf (AST_Lambda a b c    ) = deepseq a $! deepseq b $! deepseq c () 
  rnf (AST_Func   a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()
  rnf (AST_Rule   a b c    ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue o => HasNullValue (AST_RuleFunc o) where
  nullValue = AST_Lambda nullValue nullValue LocationUnknown
  testNull (AST_Lambda a b _) = testNull a && testNull b
  testNull _                  = False

instance HasLocation (AST_RuleFunc o) where
  getLocation o = case o of
    AST_Lambda _ _     o -> o
    AST_Func   _ _ _ _ o -> o
    AST_Rule   _ _     o -> o
  setLocation o loc = case o of
    AST_Lambda a b     _ -> AST_Lambda a b     loc
    AST_Func   a b c d _ -> AST_Func   a b c d loc
    AST_Rule   a b     _ -> AST_Rule   a b     loc
  delLocation o = case o of                            
    AST_Lambda a b     _ -> AST_Lambda (delLocation  a) (delLocation  b) LocationUnknown
    AST_Func   a b c d _ -> AST_Func a b  (delLocation  c) (delLocation  d) LocationUnknown
    AST_Rule   a b     _ -> AST_Rule   (delLocation  a) (delLocation  b) LocationUnknown

instance PPrintable o => PPrintable (AST_RuleFunc o) where
  pPrint expr = case expr of
    AST_Lambda         ccNmx   xcObjXp     _ ->
      pPrintSubBlock (pInline [pString "function", pPrintComWith pPrint ccNmx]) xcObjXp
    AST_Func     co nm ccNmx   xcObjXp     _ ->
      pClosure (pInline [pString "function ", pPrint co, pPrint nm, pPrint ccNmx]) "{" "}" [pPrint xcObjXp]
    AST_Rule           ccNmx   xcObjXp     _ -> pClosure (pPrint ccNmx) "{" "}" [pPrint xcObjXp]

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_RuleFunc o) where
  randO      = recurse $ countNode $ runRandChoice
  randChoice = randChoiceList $
    [ scramble $ return AST_Lambda <*> randO <*> randO <*> no
    , scramble $ return AST_Func   <*> randO <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_Rule   <*> randO <*> randO <*> no
    ]
  defaultO      = runDefaultChoice
  defaultChoice = randChoiceList $
    [ scramble $ return AST_Lambda <*> defaultO <*> defaultO <*> no
    , scramble $ return AST_Func   <*> defaultO <*> defaultO <*> defaultO <*> defaultO <*> no
    , scramble $ return AST_Rule   <*> defaultO <*> defaultO <*> no
    ]

instance Intermediate (RuleFuncExpr o) (AST_RuleFunc o) where
  toInterm ast = case ast of
    AST_Lambda a b   loc -> [LambdaExpr]       <*> uc0 a <*> ti b <*> [loc]
    AST_Func _ a b c loc -> [FuncExpr] <*> [a] <*> uc0 b <*> ti c <*> [loc]
    AST_Rule   a b   loc -> [RuleExpr]         <*> uc0 a <*> ti b <*> [loc]
  fromInterm o = case o of
    LambdaExpr a b   loc -> [AST_Lambda]                <*> nc0 a <*> fi b <*> [loc]
    FuncExpr   a b c loc -> [AST_Func] <*> [[]] <*> [a] <*> nc0 b <*> fi c <*> [loc]
    RuleExpr   a b   loc -> [AST_Rule]                  <*> nc0 a <*> fi b <*> [loc]

----------------------------------------------------------------------------------------------------

data RuleHeadExpr o
  = RuleStringExpr  UStr        Location
  | RuleHeadExpr [AssignExpr o] Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance HasNullValue (RuleHeadExpr o) where
  nullValue = RuleStringExpr nil LocationUnknown
  testNull (RuleStringExpr a _) = a==nil
  testNull _ = False

instance HasLocation (RuleHeadExpr o) where
  getLocation o     = case o of
    RuleStringExpr _ o -> o
    RuleHeadExpr   _ o -> o
  setLocation o loc = case o of
    RuleStringExpr o _ -> RuleStringExpr o loc
    RuleHeadExpr   o _ -> RuleHeadExpr o loc
  delLocation o     = case o of
    RuleStringExpr o _ -> RuleStringExpr o LocationUnknown
    RuleHeadExpr   o _ -> RuleHeadExpr (fmap delLocation o) LocationUnknown

instance NFData o => NFData (RuleHeadExpr o) where
  rnf (RuleStringExpr a b) = deepseq a $! deepseq b ()
  rnf (RuleHeadExpr     a b) = deepseq a $! deepseq b ()

----------------------------------------------------------------------------------------------------

data AST_RuleHeader o
  = AST_NullRules  [Comment]  Location
  | AST_RuleString (Com UStr) Location
  | AST_RuleHeader [Com (AST_Assign o)] Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_RuleHeader o) where
  rnf (AST_NullRules  a b) = deepseq a $! deepseq b ()
  rnf (AST_RuleString a b) = deepseq a $! deepseq b ()
  rnf (AST_RuleHeader a b) = deepseq a $! deepseq b ()

instance HasNullValue (AST_RuleHeader o) where
  nullValue = AST_NullRules [] LocationUnknown
  testNull (AST_NullRules _ _) = True
  testNull _ = False

instance HasLocation (AST_RuleHeader o) where
  getLocation o     = case o of
    AST_NullRules  _ o -> o
    AST_RuleString _ o -> o
    AST_RuleHeader _ o -> o
  setLocation o loc = case o of
    AST_NullRules  a _ -> AST_NullRules  a loc
    AST_RuleString a _ -> AST_RuleString a loc
    AST_RuleHeader a _ -> AST_RuleHeader a loc
  delLocation o     = case o of
    AST_NullRules  a _ -> AST_NullRules  a LocationUnknown
    AST_RuleString a _ -> AST_RuleString a LocationUnknown
    AST_RuleHeader a _ -> AST_RuleHeader (fmap delLocation a) LocationUnknown

instance PPrintable o => PPrintable (AST_RuleHeader o) where
  pPrint o = case o of
    AST_NullRules  coms _ -> pInline [pString "rule(", pPrint coms, pString ")"]
    AST_RuleString r    _ -> pInline [pString "rule ", pPrintComWith pShow r, pString " "]
    AST_RuleHeader ruls _ -> pList (pString "rule") "(" ", " ")" (fmap pPrint ruls)

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_RuleHeader o) where
  randO      = countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_RuleHeader <*> randList 0 3 <*> no
    , return AST_RuleString <*> randO   <*> no
    , return AST_NullRules  <*> scrambO <*> no
    ]
  defaultO      = runDefaultChoice
  defaultChoice = randChoiceList $
    [ return AST_NullRules  <*> defaultO <*> no
    , return AST_RuleString <*> defaultO <*> no
    ]

instance Intermediate (RuleHeadExpr o) (AST_RuleHeader o) where
  toInterm   o = case o of
    AST_NullRules  _ loc -> [RuleHeadExpr              []     loc]
    AST_RuleString o loc -> [RuleStringExpr (unComment o)     loc]
    AST_RuleHeader o loc -> [RuleHeadExpr] <*> [o>>=uc0] <*> [loc]
  fromInterm o = case o of
    RuleHeadExpr   [] loc -> [AST_NullRules               []     loc]
    RuleStringExpr o  loc -> [AST_RuleString       (Com   o)     loc]
    RuleHeadExpr   o  loc -> [AST_RuleHeader] <*> [o>>=nc0] <*> [loc]

----------------------------------------------------------------------------------------------------

-- | Defined such that the instantiation of 'CodeBlock' into the 'Executable' class executes each
-- 'ScriptExpr' in the 'CodeBlock', one after the other. Execution does not
-- occur within a 'execNested' because many other expressions which execute 'CodeBlock's,
-- especially 'TryCatch' expressions and 'ForLoop's need to be able to choose
-- when the stack is pushed so they can define temporary local variables.
newtype CodeBlock o = CodeBlock { codeBlock :: [ScriptExpr o] }
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (CodeBlock o) where { rnf (CodeBlock a) = deepseq a () }

instance Monoid (CodeBlock o) where
  mempty      = CodeBlock []
  mappend a b = CodeBlock (mappend (codeBlock a) (codeBlock b))

instance HasNullValue (CodeBlock o) where
  nullValue = mempty
  testNull (CodeBlock []) = True
  testNull _ = False

instance HasLocation (CodeBlock o) where
  getLocation o = case codeBlock o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = CodeBlock (fmap delLocation (codeBlock o))

instance PPrintable o => PPrintable (CodeBlock o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

-- | This node in the AST typically represents the list of 'AST_Script' expressions found between
-- curly-brackets in expressions like "if" and "else" statement, "for" statements and "while"
-- statements, "with" satements, "try" and "catch" statements and function declrataions.
newtype AST_CodeBlock o = AST_CodeBlock{ getAST_CodeBlock :: [AST_Script o] }
  deriving (Eq, Ord, Typeable, Show, Functor)
  -- A code block is never standing on it's own, it is always part of a larger expression, so there
  -- is no 'Dao.Token.Location' parameter for 'AST_CodeBlock'.

instance Monoid (AST_CodeBlock o) where
  mempty      = AST_CodeBlock []
  mappend a b = AST_CodeBlock (mappend (getAST_CodeBlock a) (getAST_CodeBlock b))

instance NFData o => NFData (AST_CodeBlock o) where { rnf (AST_CodeBlock a) = deepseq a () }

instance HasNullValue (AST_CodeBlock o) where
  nullValue = AST_CodeBlock []
  testNull (AST_CodeBlock a) = null a

instance HasLocation (AST_CodeBlock o) where                                      
  getLocation o = case getAST_CodeBlock o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = AST_CodeBlock (fmap delLocation (getAST_CodeBlock o))

-- 'pPrintComWith' wasn't good enough for this, because the comments might occur after the header
-- but before the opening bracket.
pPrintComCodeBlock :: PPrintable o => PPrint -> Com (AST_CodeBlock o) -> PPrint
pPrintComCodeBlock header c = case c of
  Com          c    -> run [] c []
  ComBefore bx c    -> run bx c []
  ComAfter     c ax -> run [] c ax
  ComAround bx c ax -> run bx c ax
  where
    run :: PPrintable o => [Comment] -> (AST_CodeBlock o) -> [Comment] -> PPrint
    run before cx after = case getAST_CodeBlock cx of
      [] -> header >> pInline (map pPrint before) >> pString " {}" >> pInline (map pPrint after)
      cx -> do
        pClosure (header >> pInline (map pPrint before)) " { " " }" (map (pGroup True . pPrint) cx)
        pInline (map pPrint after)

pPrintSubBlock :: PPrintable o => PPrint -> (AST_CodeBlock o) -> PPrint
pPrintSubBlock header px = pPrintComCodeBlock header (Com px)

instance PPrintable o => PPrintable (AST_CodeBlock o) where { pPrint o = mapM_ pPrint (getAST_CodeBlock o) }

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_CodeBlock o) where
  randO    = countNode $ AST_CodeBlock . concat <$> sequence [return <$> scrambO, depthLimitedInt 16 >>= \x -> randList 0 x]
  defaultO = return $ AST_CodeBlock []

instance Intermediate (CodeBlock o) (AST_CodeBlock o) where
  toInterm   (AST_CodeBlock ast) = [CodeBlock     $ ast >>= toInterm  ]
  fromInterm (CodeBlock     obj) = [AST_CodeBlock $ obj >>= fromInterm]

----------------------------------------------------------------------------------------------------

data IfExpr o = IfExpr (ParenExpr o) (CodeBlock o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (IfExpr o) where { rnf (IfExpr a b c) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue (IfExpr o) where
  nullValue = IfExpr nullValue nullValue LocationUnknown
  testNull (IfExpr a b _) = testNull a && testNull b

instance HasLocation (IfExpr o) where
  getLocation (IfExpr _ _ loc)     = loc
  setLocation (IfExpr a b _  ) loc = IfExpr a b loc
  delLocation (IfExpr a b _  )     = IfExpr (delLocation a) (delLocation b) LocationUnknown

----------------------------------------------------------------------------------------------------

data AST_If o = AST_If (Com (AST_Paren o)) (AST_CodeBlock o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_If o) where { rnf (AST_If a b c) = deepseq a $! deepseq b $! deepseq c () }

instance HasLocation (AST_If o) where
  getLocation (AST_If _ _ loc)     = loc
  setLocation (AST_If a b _  ) loc = AST_If a b loc
  delLocation (AST_If a b _  )     = AST_If (delLocation a) (delLocation b) LocationUnknown

instance HasNullValue (AST_If o) where
  nullValue = AST_If nullValue nullValue LocationUnknown
  testNull (AST_If a b _) = testNull a && testNull b

instance PPrintable o => PPrintable (AST_If o) where
  pPrint (AST_If ifn thn _) =
    pClosure (pString "if" >> pPrint ifn) "{" "}" [pPrint thn]

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_If o) where
  randO    = countNode $ return AST_If <*> randO <*> randO <*> no
  defaultO = return AST_If <*> defaultO <*> defaultO <*> no

instance Intermediate (IfExpr o) (AST_If o) where
  toInterm   (AST_If a b loc) = [IfExpr] <*> uc0 a <*> ti b <*> [loc]
  fromInterm (IfExpr a b loc) = [AST_If] <*> nc0 a <*> fi b <*> [loc]

----------------------------------------------------------------------------------------------------

data ElseExpr o = ElseExpr (IfExpr o) Location deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (ElseExpr o) where { rnf (ElseExpr   a b  ) = deepseq a $! deepseq b $! () }

instance HasNullValue (ElseExpr o) where
  nullValue = ElseExpr nullValue LocationUnknown
  testNull (ElseExpr a _) = testNull a

instance HasLocation (ElseExpr o) where
  getLocation (ElseExpr _ loc)     = loc
  setLocation (ElseExpr a _  ) loc = ElseExpr a loc
  delLocation (ElseExpr a _  )     = ElseExpr (delLocation a) LocationUnknown

----------------------------------------------------------------------------------------------------

data AST_Else o = AST_Else (Com ()) (AST_If o) Location deriving (Eq, Ord, Typeable, Show, Functor)
  -- ^ @/**/ else /**/ if /**/ obj /**/ {}@

instance NFData o => NFData (AST_Else o) where { rnf (AST_Else a b c) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue (AST_Else o) where
  nullValue = AST_Else nullValue nullValue LocationUnknown
  testNull (AST_Else a b _) = testNull a && testNull b

instance HasLocation (AST_Else o) where
  getLocation (AST_Else _ _ loc)     = loc
  setLocation (AST_Else a b _  ) loc = AST_Else a b loc
  delLocation (AST_Else a b _  )     = AST_Else a (delLocation b) LocationUnknown

instance PPrintable o => PPrintable (AST_Else o) where
  pPrint (AST_Else coms (AST_If ifn thn _) _) =
    pClosure (pPrintComWith (\ () -> pString "else ") coms >> pString "if" >> pPrint ifn) "{" "}" [pPrint thn]

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_Else o) where
  randO    = countNode $ return AST_Else <*> randO <*> randO <*> no
  defaultO = return AST_Else <*> defaultO <*> defaultO <*> no

instance Intermediate (ElseExpr o) (AST_Else o) where
  toInterm   (AST_Else _ a loc) = [ElseExpr]              <*> ti a <*> [loc]
  fromInterm (ElseExpr   a loc) = [AST_Else] <*> [Com ()] <*> fi a <*> [loc]

----------------------------------------------------------------------------------------------------

data IfElseExpr o = IfElseExpr (IfExpr o) [ElseExpr o] (Maybe (LastElseExpr o)) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (IfElseExpr o) where
  rnf (IfElseExpr a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue (IfElseExpr o) where
  nullValue = IfElseExpr nullValue [] Nothing LocationUnknown
  testNull (IfElseExpr a [] Nothing _) = testNull a
  testNull _ = False

instance HasLocation (IfElseExpr o) where
  getLocation (IfElseExpr _ _ _ loc)     = loc
  setLocation (IfElseExpr a b c _  ) loc = IfElseExpr a b c loc
  delLocation (IfElseExpr a b c _  )     =
    IfElseExpr (delLocation a) (fmap delLocation b) (fmap delLocation c) LocationUnknown

----------------------------------------------------------------------------------------------------

data AST_IfElse o = AST_IfElse (AST_If o) [AST_Else o] (Maybe (AST_LastElse o)) Location
  -- ^ @if /**/ obj /**/ {} /**/ else /**/ if /**/ obj /**/ {} /**/ else {}@
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_IfElse o) where
  rnf (AST_IfElse a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue (AST_IfElse o) where
  nullValue = AST_IfElse nullValue [] Nothing LocationUnknown
  testNull (AST_IfElse a [] Nothing _) = testNull a
  testNull _ = False

instance HasLocation (AST_IfElse o) where
  getLocation (AST_IfElse _ _ _ loc)     = loc
  setLocation (AST_IfElse a b c _  ) loc = AST_IfElse a b c loc
  delLocation (AST_IfElse a b c _  )     = AST_IfElse (delLocation a) (fmap delLocation b) (fmap delLocation c) LocationUnknown

instance PPrintable o => PPrintable (AST_IfElse o) where
  pPrint (AST_IfElse ifn els deflt _) = do
    pPrint ifn >> pNewLine
    mapM_ pPrint els >> pNewLine
    maybe (return ()) pPrint deflt

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_IfElse o) where
  randO    = countNode $ depthLimitedInt 8 >>= \x ->
    return AST_IfElse <*> randO <*> randList 0 x <*> randO <*> no
  defaultO = return AST_IfElse <*> defaultO <*> defaultList 0 1 <*> randO <*> no

instance Intermediate (IfElseExpr o) (AST_IfElse o) where
  toInterm   (AST_IfElse a b c loc) =
    [IfElseExpr] <*> ti a <*> [b>>=ti] <*> um1 c <*> [loc]
  fromInterm (IfElseExpr a b c loc) =
    [AST_IfElse] <*> fi a <*> [b>>=fi] <*> nm1 c <*> [loc]

----------------------------------------------------------------------------------------------------

data LastElseExpr o = LastElseExpr (CodeBlock o) Location
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (LastElseExpr o) where
  rnf (LastElseExpr a b) = deepseq a $! deepseq b ()

instance HasNullValue (LastElseExpr o) where
  nullValue = LastElseExpr nullValue LocationUnknown
  testNull (LastElseExpr o _) = testNull o

instance HasLocation (LastElseExpr o) where
  getLocation (LastElseExpr _ loc)     = loc
  setLocation (LastElseExpr a _  ) loc = LastElseExpr a loc
  delLocation (LastElseExpr a _  )     = LastElseExpr (delLocation a) LocationUnknown

----------------------------------------------------------------------------------------------------

data AST_LastElse o = AST_LastElse (Com ()) (AST_CodeBlock o) Location
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (AST_LastElse o) where
  rnf (AST_LastElse a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue (AST_LastElse o) where
  nullValue = AST_LastElse (Com ()) nullValue LocationUnknown
  testNull (AST_LastElse a b _) = testNull a && testNull b

instance HasLocation (AST_LastElse o) where
  getLocation (AST_LastElse _ _ loc)     = loc
  setLocation (AST_LastElse a b _  ) loc = AST_LastElse a b loc
  delLocation (AST_LastElse a b _  )     = AST_LastElse a (delLocation b) LocationUnknown

instance PPrintable o => PPrintable (AST_LastElse o) where
  pPrint (AST_LastElse coms code _) =
    pClosure (pPrintComWith (\ () -> pString "else") coms) "{" "}" [pPrint code]

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_LastElse o) where
  randO         = countNode $ return AST_LastElse <*> randO <*> randO <*> no
  randChoice    = randChoiceList [randO]
  defaultO      = return AST_LastElse <*> defaultO <*> defaultO <*> no
  defaultChoice = randChoiceList [defaultO]

instance Intermediate (LastElseExpr o) (AST_LastElse o) where
  toInterm   (AST_LastElse _ o loc) = [LastElseExpr] <*>              toInterm   o <*> [loc]
  fromInterm (LastElseExpr   o loc) = [AST_LastElse] <*> [Com ()] <*> fromInterm o <*> [loc]

----------------------------------------------------------------------------------------------------

data CatchExpr o = CatchExpr (ParamExpr o) (CodeBlock o) Location
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (CatchExpr o) where
  rnf (CatchExpr a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasLocation (CatchExpr o) where
  getLocation (CatchExpr _ _ loc)     = loc
  setLocation (CatchExpr a b _  ) loc = CatchExpr a b loc
  delLocation (CatchExpr a b _  )     = CatchExpr (delLocation a) (delLocation b) LocationUnknown

----------------------------------------------------------------------------------------------------

data AST_Catch o = AST_Catch [Comment] (Com (AST_Param o)) (AST_CodeBlock o) Location
  deriving (Eq, Ord, Show, Typeable, Functor)

instance NFData o => NFData (AST_Catch o) where
  rnf (AST_Catch a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasLocation (AST_Catch o) where
  getLocation (AST_Catch _ _ _ loc)     = loc
  setLocation (AST_Catch a b c _  ) loc = AST_Catch a b c loc
  delLocation (AST_Catch a b c _  )     = AST_Catch a (delLocation b) (delLocation c) LocationUnknown

instance PPrintable o => PPrintable (AST_Catch o) where
  pPrint (AST_Catch coms param code _) = pPrint coms >>
    pClosure (pString "catch " >> pPrint param) "{" "}" [pPrint code]

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_Catch o) where
  randO         = countNode $ return AST_Catch <*> randO <*> randO <*> randO <*> no
  randChoice    = randChoiceList [randO]
  defaultO      = return AST_Catch <*> defaultO <*> defaultO <*> defaultO <*> no
  defaultChoice = randChoiceList [defaultO]

instance Intermediate (CatchExpr o) (AST_Catch o) where
  toInterm   (AST_Catch _ a b loc) = [CatchExpr]          <*> uc0 a <*> ti b <*> [loc]
  fromInterm (CatchExpr   a b loc) = [AST_Catch] <*> [[]] <*> nc0 a <*> fi b <*> [loc]

----------------------------------------------------------------------------------------------------

newtype WhileExpr o = WhileExpr (IfExpr o) deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (WhileExpr o)  where { rnf (WhileExpr (IfExpr a b c)) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue (WhileExpr o) where
  nullValue = WhileExpr nullValue
  testNull (WhileExpr a) = testNull a

instance HasLocation (WhileExpr o) where
  getLocation (WhileExpr a)     = getLocation a
  setLocation (WhileExpr a) loc = WhileExpr (setLocation a loc)
  delLocation (WhileExpr a)     = WhileExpr (delLocation a)

----------------------------------------------------------------------------------------------------

newtype AST_While o = AST_While (AST_If o) deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_While o) where { rnf (AST_While (AST_If a b c)) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue (AST_While o) where
  nullValue = AST_While nullValue
  testNull (AST_While a) = testNull a

instance HasLocation (AST_While o) where
  getLocation (AST_While a) = getLocation a
  setLocation (AST_While a) loc = AST_While (setLocation a loc)
  delLocation (AST_While a)     = AST_While (delLocation a)

instance PPrintable o => PPrintable (AST_While o) where
  pPrint (AST_While (AST_If ifn thn _)) =
    pClosure (pInline [pString "while", pPrint ifn]) "{" "}" [pPrint thn]

instance Intermediate (WhileExpr o) (AST_While o) where
  toInterm   (AST_While a) = WhileExpr <$> ti a
  fromInterm (WhileExpr a) = AST_While <$> fi a

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_While o)  where
  randO    = AST_While <$> randO
  defaultO = AST_While <$> defaultO

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data ScriptExpr o
  = IfThenElse   (IfElseExpr   o)
  | WhileLoop    (WhileExpr    o)
  | RuleFuncExpr (RuleFuncExpr o)
  | EvalObject   (AssignExpr   o)                                 Location -- location of the semicolon
  | TryCatch     (CodeBlock    o) [LastElseExpr  o] [CatchExpr o] Location
  | ForLoop       Name            (RefPrefixExpr o) (CodeBlock o) Location
  | ContinueExpr  Bool            (AssignExpr    o)               Location
  | ReturnExpr    Bool            (AssignExpr    o)               Location
  | WithDoc      (ParenExpr    o) (CodeBlock     o)               Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (ScriptExpr o) where
  rnf (IfThenElse   a      ) = deepseq a ()
  rnf (WhileLoop    a      ) = deepseq a ()
  rnf (RuleFuncExpr a      ) = deepseq a ()
  rnf (EvalObject   a b    ) = deepseq a $! deepseq b ()
  rnf (TryCatch     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ForLoop      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ContinueExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ReturnExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (WithDoc      a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue o => HasNullValue (ScriptExpr o) where
  nullValue = EvalObject nullValue LocationUnknown
  testNull (EvalObject a _) = testNull a
  testNull _ = False

instance HasLocation (ScriptExpr o) where
  getLocation o = case o of
    EvalObject   _     o -> o
    IfThenElse         o -> getLocation o
    RuleFuncExpr       o -> getLocation o
    WhileLoop          o -> getLocation o
    TryCatch     _ _ _ o -> o
    ForLoop      _ _ _ o -> o
    ContinueExpr _ _   o -> o
    ReturnExpr   _ _   o -> o
    WithDoc      _ _   o -> o
  setLocation o loc = case o of
    EvalObject   a     _ -> EvalObject   a     loc
    IfThenElse   a       -> IfThenElse   (setLocation a loc)
    WhileLoop    a       -> WhileLoop    (setLocation a loc)
    RuleFuncExpr a       -> RuleFuncExpr (setLocation a loc)
    TryCatch     a b c _ -> TryCatch     a b c loc
    ForLoop      a b c _ -> ForLoop      a b c loc
    ContinueExpr a b   _ -> ContinueExpr a b   loc
    ReturnExpr   a b   _ -> ReturnExpr   a b   loc
    WithDoc      a b   _ -> WithDoc      a b   loc
  delLocation o = case o of
    EvalObject   a     _ -> EvalObject   (delLocation a)                    LocationUnknown
    IfThenElse   a       -> IfThenElse   (delLocation a)
    WhileLoop    a       -> WhileLoop    (delLocation a)
    RuleFuncExpr a       -> RuleFuncExpr (delLocation a)
    TryCatch     a b c _ -> TryCatch     (delLocation a) (fmap delLocation b) (fmap delLocation c) LocationUnknown
    ForLoop      a b c _ -> ForLoop      a (delLocation b) (delLocation c) LocationUnknown
    ContinueExpr a b   _ -> ContinueExpr a (delLocation b)                 LocationUnknown
    ReturnExpr   a b   _ -> ReturnExpr   a (delLocation b)                 LocationUnknown
    WithDoc      a b   _ -> WithDoc      (delLocation a) (delLocation b)   LocationUnknown

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data AST_Script o
  = AST_Comment     [Comment] 
  | AST_IfThenElse  (AST_IfElse o)
  | AST_WhileLoop   (AST_While o)
  | AST_RuleFunc    (AST_RuleFunc o)
  | AST_EvalObject  (AST_Assign o)  [Comment]                                          Location
    -- ^ @some.object.expression = for.example - equations || function(calls) /**/ ;@
  | AST_TryCatch    [Comment] (AST_CodeBlock o)   [AST_LastElse  o]  [AST_Catch     o] Location
    -- ^ @try /**/ {} /**/ else /**/ {} /**/ catch /**/ errVar /**/ {}@              
  | AST_ForLoop     (Com Name)               (Com (AST_RefPrefix o)) (AST_CodeBlock o) Location
    -- ^ @for /**/ var /**/ in /**/ objExpr /**/ {}@
  | AST_ContinueExpr Bool  [Comment]         (Com (AST_Assign    o))                   Location
    -- ^ The boolean parameter is True for a "continue" statement, False for a "break" statement.
    -- @continue /**/ ;@ or @continue /**/ if /**/ objExpr /**/ ;@
  | AST_ReturnExpr   Bool                    (Com (AST_Assign    o))                   Location
    -- ^ The boolean parameter is True for a "return" statement, False for a "throw" statement.
    -- ^ @return /**/ ;@ or @return /**/ objExpr /**/ ;@
  | AST_WithDoc      (Com (AST_Paren o))          (AST_CodeBlock o)                    Location
    -- ^ @with /**/ objExpr /**/ {}@
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (AST_Script o) where
  rnf (AST_Comment      a        ) = deepseq a ()
  rnf (AST_IfThenElse   a        ) = deepseq a ()
  rnf (AST_WhileLoop    a        ) = deepseq a ()
  rnf (AST_RuleFunc     a        ) = deepseq a ()
  rnf (AST_EvalObject   a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_TryCatch     a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()
  rnf (AST_ForLoop      a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ContinueExpr a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ReturnExpr   a b c    ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_WithDoc      a b c    ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue (AST_Script o) where
  nullValue = AST_EvalObject nullValue [] LocationUnknown
  testNull (AST_EvalObject a _ _) = testNull a
  testNull _ = False

instance HasLocation (AST_Script o) where
  getLocation o = case o of
    AST_Comment      _         -> LocationUnknown
    AST_EvalObject   _   _   o -> o
    AST_IfThenElse           o -> getLocation o
    AST_WhileLoop            o -> getLocation o
    AST_RuleFunc             o -> getLocation o
    AST_TryCatch     _ _ _ _ o -> o
    AST_ForLoop      _ _ _   o -> o
    AST_ContinueExpr _ _ _   o -> o
    AST_ReturnExpr   _ _     o -> o
    AST_WithDoc      _ _     o -> o
  setLocation o loc = case o of
    AST_Comment      a         -> AST_Comment      a
    AST_EvalObject   a b     _ -> AST_EvalObject   a b   loc
    AST_IfThenElse   a         -> AST_IfThenElse   (setLocation a loc)
    AST_WhileLoop    a         -> AST_WhileLoop    (setLocation a loc)
    AST_RuleFunc     a         -> AST_RuleFunc     (setLocation a loc)
    AST_TryCatch     a b c d _ -> AST_TryCatch     a b c d loc
    AST_ForLoop      a b c   _ -> AST_ForLoop      a b c   loc
    AST_ContinueExpr a b c   _ -> AST_ContinueExpr a b c   loc
    AST_ReturnExpr   a b     _ -> AST_ReturnExpr   a b     loc
    AST_WithDoc      a b     _ -> AST_WithDoc      a b     loc
  delLocation o = case o of
    AST_Comment      a         -> AST_Comment      a
    AST_EvalObject   a b     _ -> AST_EvalObject   (delLocation  a) b LocationUnknown
    AST_IfThenElse   a         -> AST_IfThenElse   (delLocation  a)
    AST_WhileLoop    a         -> AST_WhileLoop    (delLocation  a)
    AST_RuleFunc     a         -> AST_RuleFunc     (delLocation  a)
    AST_TryCatch     a b c d _ -> AST_TryCatch     a (delLocation b) (fmap delLocation c) (fmap delLocation d) LocationUnknown
    AST_ForLoop      a b c   _ -> AST_ForLoop      a (fmap delLocation b) (delLocation c) LocationUnknown
    AST_ContinueExpr a b c   _ -> AST_ContinueExpr a b (fmap delLocation c) LocationUnknown
    AST_ReturnExpr   a b     _ -> AST_ReturnExpr   a (fmap delLocation b) LocationUnknown
    AST_WithDoc      a b     _ -> AST_WithDoc      (fmap delLocation a) (delLocation b) LocationUnknown

instance PPrintable o => PPrintable (AST_Script o) where
  pPrint expr = pGroup True $ case expr of
    AST_Comment             coms -> mapM_ pPrint coms
    AST_EvalObject   objXp  coms                      _ ->
      pPrint objXp >> mapM_ pPrint coms >> pString ";"
    AST_IfThenElse   ifXp                          -> pPrint ifXp
    AST_WhileLoop    whileLoop                     -> pPrint whileLoop
    AST_RuleFunc     ruleOrFunc                    -> pPrint ruleOrFunc
    AST_TryCatch     coms scrpXp elsXp catchExpr _ -> do
      pClosure (pString "try" >> pPrint coms) "{" "}" [pPrint scrpXp]
      mapM_ (\o -> pPrint o >> pEndLine) elsXp
      mapM_ (\o -> pPrint o >> pEndLine) catchExpr
    AST_ForLoop      cNm      cObjXp  xcScrpXp   _ ->
      pPrintSubBlock (pString "for " >> pPrint cNm >> pString " in " >> pPrint cObjXp) xcScrpXp
    AST_ContinueExpr contin   coms    cObjXp     _ -> pWrapIndent $
      [ pString (if contin then "continue" else "break")
      , pInline (map pPrint coms)
      , case unComment cObjXp of
          AST_Eval (AST_ObjArith (AST_Object AST_Void)) -> return ()
          _ -> pString " if" >> when (precedeWithSpace cObjXp) (pString " ") >> pPrint cObjXp
      , pString ";"
      ]
    AST_ReturnExpr   retrn           cObjXp      _ -> pWrapIndent $
      [pString (if retrn then "return " else "throw "), pPrint cObjXp, pString ";"]
    AST_WithDoc      cObjXp          xcScrpXp    _ ->
      pPrintSubBlock (pString "with " >> pPrint cObjXp) xcScrpXp

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_Script o) where
  randO      = countNode $ recurse $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_EvalObject   <*> randO <*> randO <*> no
    , return AST_IfThenElse   <*> randO
    , return AST_WhileLoop    <*> randO
    , return AST_RuleFunc     <*> randO
    , scramble $ depthLimitedInt 4 >>= \x -> depthLimitedInt 4 >>= \y -> 
        return AST_TryCatch <*> randO <*> randO <*> randList 0 x <*> randList 0 y <*> no
    , scramble $ return AST_ForLoop      <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_ContinueExpr <*> randO <*> randO <*> randO <*> no
    , scramble $ return AST_ReturnExpr   <*> randO <*> randO <*> no
    , scramble $ return AST_WithDoc      <*> randO <*> randO <*> no
    ]
  defaultO      = runDefaultChoice
  defaultChoice = randChoiceList $
    [ AST_IfThenElse <$> defaultO
    , AST_WhileLoop  <$> defaultO
    , AST_RuleFunc   <$> defaultO
    , return AST_TryCatch     <*> pure []  <*> defaultO <*> defaultList 0 1 <*> defaultList 0 1 <*> no
    , return AST_ContinueExpr <*> defaultO <*> defaultO <*> defaultO <*> no
    , return AST_ReturnExpr   <*> defaultO <*> defaultO <*> no
    ]

instance PPrintable o => PPrintable (ScriptExpr o) where { pPrint = pPrintInterm }

instance Intermediate (ScriptExpr o) (AST_Script o) where
  toInterm   ast = case ast of
    AST_Comment      _           -> mzero
    AST_EvalObject   a _     loc -> [EvalObject  ] <*> ti  a                     <*> [loc]
    AST_IfThenElse   a           -> [IfThenElse  ] <*> ti  a
    AST_WhileLoop    a           -> [WhileLoop   ] <*> ti  a
    AST_RuleFunc     a           -> [RuleFuncExpr] <*> ti  a
    AST_TryCatch     _ a b c loc -> [TryCatch    ] <*> ti  a <*> mapM ti b <*> mapM ti c <*> [loc]
    AST_ForLoop      a b c   loc -> [ForLoop     ] <*> uc  a <*> uc0 b <*> ti  c <*> [loc]
    AST_ContinueExpr a _ c   loc -> [ContinueExpr] <*> [a]   <*> uc0 c           <*> [loc]
    AST_ReturnExpr   a b     loc -> [ReturnExpr  ] <*> [a]   <*> uc0 b           <*> [loc]
    AST_WithDoc      a b     loc -> [WithDoc     ] <*> uc0 a <*> ti  b           <*> [loc]
  fromInterm obj = case obj of
    EvalObject   a     loc -> [AST_EvalObject  ] <*> fi  a  <*> [[]]           <*> [loc]
    IfThenElse   a         ->  AST_IfThenElse    <$> fi  a
    WhileLoop    a         ->  AST_WhileLoop     <$> fi  a
    RuleFuncExpr a         ->  AST_RuleFunc      <$> fi  a
    TryCatch     a b c loc -> [AST_TryCatch    ] <*> [[]]   <*> fi  a <*> mapM fi b <*> mapM fi c <*> [loc]
    ForLoop      a b c loc -> [AST_ForLoop     ] <*> nc  a  <*> nc0 b <*> fi  c <*> [loc]
    ContinueExpr a b   loc -> [AST_ContinueExpr] <*>    [a] <*> [[]]  <*> nc0 b <*> [loc]
    ReturnExpr   a b   loc -> [AST_ReturnExpr  ] <*>    [a] <*> nc0 b           <*> [loc]
    WithDoc      a b   loc -> [AST_WithDoc     ] <*> nc0 a  <*> fi  b           <*> [loc]
                         
----------------------------------------------------------------------------------------------------

data AttributeExpr
  = AttribDotNameExpr DotLabelExpr
  | AttribStringExpr  UStr        Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AttributeExpr where
  rnf (AttribDotNameExpr a  ) = deepseq a ()
  rnf (AttribStringExpr  a b) = deepseq a $! deepseq b ()

instance HasNullValue AttributeExpr where
  nullValue = AttribStringExpr nil LocationUnknown
  testNull (AttribStringExpr a _) = a==nil
  testNull _ = False

instance HasLocation AttributeExpr where
  getLocation o     = case o of
    AttribDotNameExpr   o   -> getLocation o
    AttribStringExpr  _ loc -> loc
  setLocation o loc = case o of
    AttribDotNameExpr o     -> AttribDotNameExpr (setLocation o loc)
    AttribStringExpr  o _   -> AttribStringExpr o loc
  delLocation o     = case o of
    AttribDotNameExpr o     -> AttribDotNameExpr (delLocation o)
    AttribStringExpr  o _   -> AttribStringExpr o LocationUnknown

instance PPrintable AttributeExpr where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

data AST_Attribute
  = AST_AttribDotName AST_DotLabel
  | AST_AttribString  UStr        Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AST_Attribute where
  rnf (AST_AttribDotName a  ) = deepseq a ()
  rnf (AST_AttribString  a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_Attribute where
  nullValue = AST_AttribString nil LocationUnknown
  testNull (AST_AttribString a _) = a==nil
  testNull _ = False

instance HasLocation AST_Attribute where
  getLocation o     = case o of
    AST_AttribDotName   o   -> getLocation o
    AST_AttribString  _ loc -> loc
  setLocation o loc = case o of
    AST_AttribDotName o     -> AST_AttribDotName (setLocation o loc)
    AST_AttribString  o _   -> AST_AttribString o loc
  delLocation o     = case o of
    AST_AttribDotName o     -> AST_AttribDotName (delLocation o)
    AST_AttribString  o _   -> AST_AttribString o LocationUnknown

instance PPrintable AST_Attribute where
  pPrint o = case o of
    AST_AttribDotName nm    -> pPrint nm
    AST_AttribString  str _ -> pPrint str

instance HasRandGen AST_Attribute where
  randChoice = randChoiceList $
    [ AST_AttribDotName <$> randO
    , return AST_AttribString  <*> randO <*> no
    ]
  randO    = countNode $ runRandChoice
  defaultO = randO
  defaultChoice = randChoiceList [defaultO]

instance Intermediate AttributeExpr AST_Attribute where
  toInterm o   = case o of
    AST_AttribDotName a     -> AttribDotNameExpr <$> ti a
    AST_AttribString  a loc -> [AttribStringExpr a loc]
  fromInterm o = case o of
    AttribDotNameExpr a     -> AST_AttribDotName <$> fi a
    AttribStringExpr  a loc -> [AST_AttribString  a loc]

----------------------------------------------------------------------------------------------------

data TopLevelEventType
  = BeginExprType | EndExprType | ExitExprType
  deriving (Eq, Ord, Typeable, Enum)

instance Show TopLevelEventType where
  show t = case t of
    BeginExprType -> "BEGIN"
    EndExprType   -> "END"
    ExitExprType  -> "EXIT"

instance Read TopLevelEventType where
  readsPrec _ str = map (\t -> (t, "")) $ case str of
    "BEGIN" -> [BeginExprType]
    "END"   -> [EndExprType]
    "EXIT"  -> [ExitExprType]
    _       -> []

instance NFData TopLevelEventType where { rnf a = seq a () }

instance HasRandGen TopLevelEventType where
  randO = fmap toEnum (nextInt 3)
  defaultO = randO

----------------------------------------------------------------------------------------------------

-- | A 'TopLevelExpr' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data TopLevelExpr o
  = RequireExpr AttributeExpr                   Location
  | ImportExpr  AttributeExpr     NamespaceExpr Location
  | TopScript   (ScriptExpr o)                  Location
  | EventExpr   TopLevelEventType (CodeBlock o) Location
  deriving (Eq, Ord, Typeable, Show, Functor)

instance NFData o => NFData (TopLevelExpr o) where
  rnf (RequireExpr a b  ) = deepseq a $! deepseq b ()
  rnf (ImportExpr  a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (TopScript   a b  ) = deepseq a $! deepseq b ()
  rnf (EventExpr   a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue o => HasNullValue (TopLevelExpr o) where
  nullValue = TopScript nullValue LocationUnknown
  testNull (TopScript a LocationUnknown) = testNull a
  testNull _ = False

isAttribute :: TopLevelExpr o -> Bool
isAttribute toplevel = case toplevel of { RequireExpr{} -> True; ImportExpr{} -> True; _ -> False; }

instance HasLocation (TopLevelExpr o) where
  getLocation o = case o of
    RequireExpr _   o -> o
    ImportExpr  _ _ o -> o
    TopScript   _   o -> o
    EventExpr   _ _ o -> o
  setLocation o loc = case o of
    RequireExpr a    _ -> RequireExpr a loc
    ImportExpr  a b  _ -> ImportExpr  a b loc
    TopScript   a    _ -> TopScript   a   loc
    EventExpr   a b  _ -> EventExpr   a b loc
  delLocation o = case o of
    RequireExpr a    _ -> RequireExpr (delLocation a) LocationUnknown
    ImportExpr  a b  _ -> ImportExpr  (delLocation a) (delLocation b) LocationUnknown
    TopScript   a    _ -> TopScript   (delLocation a) LocationUnknown
    EventExpr   a b  _ -> EventExpr a (delLocation b) LocationUnknown

instance PPrintable o => PPrintable (TopLevelExpr o) where { pPrint = pPrintInterm }

----------------------------------------------------------------------------------------------------

-- | A 'AST_TopLevel' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data AST_TopLevel o
  = AST_Require    (Com AST_Attribute)                       Location
  | AST_Import     (Com AST_Attribute)         AST_Namespace Location
  | AST_TopScript  (AST_Script o)                            Location
  | AST_Event      TopLevelEventType [Comment] (AST_CodeBlock o) Location
  | AST_TopComment [Comment]
  deriving (Eq, Ord, Typeable, Show, Functor)

instance (HasNullValue o, HasRandGen o) => HasRandGen (AST_TopLevel o) where
  randO      = countNode $ runRandChoice
  randChoice = randChoiceList $
    [ return AST_Require   <*> randO                     <*> no
    , return AST_Import    <*> randO <*> randO           <*> no
    , return AST_TopScript <*> randO                     <*> no
    , return AST_Event     <*> randO <*> randO <*> randO <*> no
    , AST_TopComment <$> defaultO
    ]
  defaultO      = runDefaultChoice
  defaultChoice = randChoiceList $
    [ return AST_Import    <*> defaultO <*> defaultO              <*> no
    , return AST_Require   <*> defaultO                           <*> no
    , return AST_TopScript <*> defaultO                           <*> no
    , return AST_Event     <*> defaultO <*> defaultO <*> defaultO <*> no
    ]

instance Intermediate (TopLevelExpr o) (AST_TopLevel o) where
  toInterm   ast = case ast of
    AST_Require   a     loc -> [RequireExpr] <*> uc0 a           <*> [loc]
    AST_Import    a   b loc -> [ImportExpr ] <*> uc0 a  <*> ti b <*> [loc]
    AST_TopScript a     loc -> [TopScript  ] <*> ti  a           <*> [loc]
    AST_Event     a _ b loc -> [EventExpr  ] <*>    [a] <*> ti b <*> [loc]
    AST_TopComment     _loc -> mzero
  fromInterm obj = case obj of
    RequireExpr a   loc -> [AST_Require  ] <*> nc0 a                    <*> [loc]
    ImportExpr  a b loc -> [AST_Import   ] <*> nc0 a           <*> fi b <*> [loc]
    TopScript   a   loc -> [AST_TopScript] <*> fi  a                    <*> [loc]
    EventExpr   a b loc -> [AST_Event    ] <*>    [a] <*> [[]] <*> fi b <*> [loc]

isAST_Attribute :: AST_TopLevel o -> Bool
isAST_Attribute o = case o of { AST_Require{} -> True; AST_Import{} -> True; _ -> False; }

-- | Split a list of 'AST_TopLevel' items into a tripple, the "require" statements, the "import"
-- statements. This function scans the list lazily and returns as soon as an 'AST_TopLevel' item in
-- the list is found that is not one of 'AST_Require', 'AST_Import' or 'AST_TopComment'.
-- 
-- Notice that this function takes an 'AST_TopLevel' and returns a list of 'AttributeExpr's, not a
-- list of 'AST_Attribute's. This is because this function is designed for assisting in evaluation
-- of the import statements of a Dao script file, specifically in order to generate a dependency
-- graph.
getRequiresAndImports :: [AST_TopLevel o] -> ([AttributeExpr], [(AttributeExpr, NamespaceExpr)])
getRequiresAndImports = loop [] [] where
  loop requires imports ox = case ox of
    AST_Require a   _ : ox -> loop (requires ++ uc0 a) imports ox
    AST_Import  a b _ : ox -> loop requires (imports ++ (pure (,) <*> uc0 a <*> toInterm b)) ox
    AST_TopComment{}  : ox -> loop requires imports ox
    _                      -> (requires, imports)

instance NFData o => NFData (AST_TopLevel o) where
  rnf (AST_Require    a b    ) = deepseq a $! deepseq b ()
  rnf (AST_Import     a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_TopScript  a b    ) = deepseq a $! deepseq b ()
  rnf (AST_Event      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_TopComment a      ) = deepseq a ()

instance HasNullValue (AST_TopLevel o) where
  nullValue = AST_TopScript nullValue LocationUnknown
  testNull (AST_TopScript a _) = testNull a
  testNull _ = False

instance HasLocation (AST_TopLevel o) where
  getLocation o = case o of
    AST_Require    _     o -> o
    AST_Import     _ _   o -> o
    AST_TopScript  _     o -> o
    AST_Event      _ _ _ o -> o
    AST_TopComment _       -> LocationUnknown
  setLocation o loc = case o of
    AST_Require    a      _ -> AST_Require    a       loc
    AST_Import     a b    _ -> AST_Import     a b     loc
    AST_TopScript  a      _ -> AST_TopScript  a       loc
    AST_Event      a b c  _ -> AST_Event      a b c   loc
    AST_TopComment a        -> AST_TopComment a
  delLocation o = case o of
    AST_Require    a      _ -> AST_Require    (delLocation a) LocationUnknown
    AST_Import     a b    _ -> AST_Import     (delLocation a) (delLocation b) LocationUnknown
    AST_TopScript  a      _ -> AST_TopScript  (delLocation a) LocationUnknown
    AST_Event      a b c  _ -> AST_Event      a b (delLocation c) LocationUnknown
    AST_TopComment a        -> AST_TopComment a  

instance PPrintable o => PPrintable (AST_TopLevel o) where
  pPrint o = case o of
    AST_Require   a      _ -> pWrapIndent [pString "require ", pPrint a, pString ";"]
    AST_Import    a b    _ -> pWrapIndent [pString "import  ", pPrint a, pPrint b, pString ";"]
    AST_TopScript a      _ -> pPrint a
    AST_Event     a b c  _ -> pClosure (pShow a >> mapM_ pPrint b) " { " " }" (map pPrint (getAST_CodeBlock c))
    AST_TopComment a       -> mapM_ (\a -> pPrint a >> pNewLine) a

----------------------------------------------------------------------------------------------------

-- | A program is just a list of 'TopLevelExpr's. It serves as the 'Intermediate'
-- representation of a 'AST_SourceCode'.
newtype Program o = Program { topLevelExprs :: [TopLevelExpr o] } deriving (Eq, Ord, Typeable)

instance Show o => Show (Program o) where { show (Program o) = unlines (map show o) }

instance HasNullValue (Program o) where
  nullValue = Program []
  testNull (Program p) = null p

instance HasLocation (Program o) where
  getLocation o = case topLevelExprs o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = Program (fmap delLocation (topLevelExprs o))

----------------------------------------------------------------------------------------------------

-- | A 'SourceCode' is the structure loaded from source code. An 'ExecUnit' object is constructed from
-- 'SourceCode'.
data AST_SourceCode o
  = AST_SourceCode
    { sourceModified :: Int
    , sourceFullPath :: UStr
      -- ^ the URL (full file path) from where this source code was received.
    , directives     :: [AST_TopLevel o]
    }
  deriving (Eq, Ord, Typeable)

instance NFData o => NFData (AST_SourceCode o) where
  rnf (AST_SourceCode a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue (AST_SourceCode o) where
  nullValue = (AST_SourceCode 0 nil [])
  testNull (AST_SourceCode 0 a []) | a==nil = True
  testNull _ = False

instance PPrintable o => PPrintable (AST_SourceCode o) where
  pPrint sc = do
    let (attrs, dirs) = span isAST_Attribute (directives sc)
    mapM_ pPrint attrs
    pForceNewLine
    mapM_ (\dir -> pPrint dir >> pForceNewLine) dirs

instance Intermediate (Program o) (AST_SourceCode o) where
  toInterm   ast = [Program $ directives ast >>= toInterm]
  fromInterm obj = return $
    AST_SourceCode
    { sourceModified = 0
    , sourceFullPath = nil
    , directives     = topLevelExprs obj >>= fromInterm
    }

