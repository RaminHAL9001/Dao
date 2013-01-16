-- "src/Dao/Object.hs"  declares the "Object" data type which is the
-- fundamental data type used througout the Dao System.
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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Object
  ( module Dao.String
  , module Dao.Object
  ) where

import           Dao.Debug.OFF
import           Dao.String
import           Dao.Token
import           Dao.Pattern
import           Dao.EnumSet
import           Dao.Tree as T
import           Dao.Predicate

import           Data.Typeable
import           Data.Dynamic
import           Data.Unique
import           Data.Maybe
import           Data.Either
import           Data.List
import           Data.Complex
import           Data.Ix
import           Data.Int
import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Array.IArray
import           Data.Time hiding (parseTime)
import           Data.IORef

import           Numeric

import qualified Data.Map                  as M
import qualified Data.IntMap               as IM
import qualified Data.Set                  as S
import qualified Data.IntSet               as IS
import qualified Data.ByteString.Lazy      as B

import           Control.Monad
import           Control.Exception
import           Control.Concurrent
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State

----------------------------------------------------------------------------------------------------

showEncoded :: [Word8] -> String
showEncoded encoded = seq encoded (concatMap (\b -> showHex b " ") encoded)

type T_int      = Int64
type T_word     = Word64
type T_long     = Integer
type T_ratio    = Rational
type T_complex  = Complex T_float
type T_float    = Double
type T_time     = UTCTime
type T_diffTime = NominalDiffTime
type T_char     = Char
type T_string   = UStr
type T_ref      = Reference
type T_pair     = (Object, Object)
type T_list     = [Object]
type T_set      = S.Set Object
type T_array_ix = T_int
type T_array    = Array T_array_ix Object
type T_intMap   = IM.IntMap Object
type T_dict     = M.Map Name Object
type T_tree     = T.Tree Name Object
type T_pattern  = Pattern
type T_rule     = RuleExpr
type T_script   = Subroutine
type T_bytes    = B.ByteString

data TypeID
  = NullType
  | TrueType
  | TypeType
  | IntType
  | WordType
  | DiffTimeType
  | FloatType
  | LongType
  | RatioType
  | ComplexType
  | TimeType
  | CharType
  | StringType
  | PairType
  | RefType
  | ListType
  | SetType
  | ArrayType
  | IntMapType
  | DictType
  | TreeType
  | PatternType
  | ScriptType
  | RuleType
  | BytesType
  deriving (Eq, Ord, Show, Enum, Typeable)

oBool :: Bool -> Object
oBool a = if a then OTrue else ONull

-- | References used throughout the executable script refer to differer places in the Runtime where
-- values can be stored. Because each store is accessed slightly differently, it is necessary to
-- declare, in the abstract syntax tree (AST) representation of the script exactly why types of
-- variables are being accessed so the appropriate read, write, or update action can be planned.
data Reference
  = IntRef     { intRef    :: Word }  -- ^ reference to a read-only pattern-match variable.
  | LocalRef   { localRef  :: Name } -- ^ reference to a local variable.
  | StaticRef  { localRef  :: Name } -- ^ reference to a permanent static variable (stored per rule/function).
  | QTimeRef   { globalRef :: [Name] } -- ^ reference to a query-time static variable.
  | GlobalRef  { globalRef :: [Name] } -- ^ reference to in-memory data stored per 'Dao.Types.ExecUnit'.
  | ProgramRef { progID    :: Name , subRef    :: Reference } -- ^ reference to a portion of a 'Dao.Types.Program'.
  | FileRef    { fileID    :: UPath, globalRef :: [Name] } -- ^ reference to a variable in a 'Dao.Types.File'
  | Subscript  { dereference :: Reference, subscriptValue :: Object } -- ^ reference to value at a subscripted slot in a container object
  | MetaRef    { dereference :: Reference } -- ^ wraps up a 'Reference' as a value that cannot be used as a reference.
  deriving (Eq, Ord, Show, Typeable)

refSameClass :: Reference -> Reference -> Bool
refSameClass a b = case (a, b) of
  (IntRef       _, IntRef        _) -> True
  (LocalRef     _, LocalRef      _) -> True
  (QTimeRef     _, QTimeRef      _) -> True
  (StaticRef    _, StaticRef     _) -> True
  (GlobalRef    _, GlobalRef     _) -> True
  (ProgramRef _ _, ProgramRef  _ _) -> True
  (FileRef    _ _, FileRef     _ _) -> True
  (MetaRef      _, MetaRef       _) -> True
  _                                 -> False

appendReferences :: Reference -> Reference -> Maybe Reference
appendReferences a b = case b of
  IntRef     _   -> mzero
  LocalRef     b -> fn [b]
  StaticRef    b -> fn [b]
  QTimeRef     b -> fn  b
  GlobalRef    b -> fn  b
  ProgramRef _ b -> appendReferences a b
  FileRef    _ b -> fn  b
  MetaRef    _   -> mzero
  Subscript  b j -> case a of
    Subscript a i -> appendReferences a b >>= \c -> return (Subscript (Subscript c i) j)
    a             -> appendReferences a b >>= \c -> return (Subscript c j)
  where
    fn b = case a of
      IntRef     _   -> mzero
      LocalRef     a -> return (GlobalRef (a:b))
      StaticRef    a -> mzero
      QTimeRef     a -> return (QTimeRef     (a++b))
      GlobalRef    a -> return (GlobalRef    (a++b))
      ProgramRef f a -> appendReferences a (GlobalRef b) >>= \ref -> return (ProgramRef f ref)
      FileRef    f a -> return (FileRef    f (a++b))
      MetaRef    _   -> mzero

-- | The 'Object' type is clumps together all of Haskell's most convenient data structures into a
-- single data type so they can be used in a non-functional, object-oriented way in the Dao runtime.
data Object
  = ONull
  | OTrue
  | OType      TypeID
  | OInt       T_int
  | OWord      T_word
  | OLong      T_long
  | OFloat     T_float
  | ORatio     T_ratio
  | OComplex   T_complex
  | OTime      T_time
  | ODiffTime  T_diffTime
  | OChar      T_char
  | OString    T_string
  | ORef       T_ref
  | OPair      T_pair
  | OList      T_list
  | OSet       T_set
  | OArray     T_array
  | ODict      T_dict
  | OIntMap    T_intMap
  | OTree      T_tree
  | OPattern   T_pattern
  | OScript    T_script
  | ORule      T_rule
  | OBytes     T_bytes
  deriving (Eq, Ord, Show, Typeable)

instance Exception Object

-- | Since 'Object' requires all of it's types instantiate 'Prelude.Ord', I have defined
-- 'Prelude.Ord' of 'Data.Complex.Complex' numbers to be the distance from 0, that is, the radius of
-- the polar form of the 'Data.Complex.Complex' number, ignoring the angle argument.
instance RealFloat a => Ord (Complex a) where
  compare a b = compare (magnitude a) (magnitude b)

----------------------------------------------------------------------------------------------------

objType :: Object -> TypeID
objType o = case o of
  ONull       -> NullType
  OTrue       -> TrueType
  OType     _ -> TypeType
  OInt      _ -> IntType
  OWord     _ -> WordType
  OLong     _ -> LongType
  OFloat    _ -> FloatType
  ORatio    _ -> RatioType
  OComplex  _ -> ComplexType
  OTime     _ -> TimeType
  ODiffTime _ -> DiffTimeType
  OChar     _ -> CharType
  OString   _ -> StringType
  ORef      _ -> RefType
  OPair     _ -> PairType
  OList     _ -> ListType
  OSet      _ -> SetType
  OArray    _ -> ArrayType
  OIntMap   _ -> IntMapType
  ODict     _ -> DictType
  OTree     _ -> TreeType
  OPattern  _ -> PatternType
  OScript   _ -> ScriptType
  ORule     _ -> RuleType
  OBytes    _ -> BytesType

instance Enum Object where
  toEnum   i = OType (toEnum i)
  fromEnum o = fromEnum (objType o)
  pred o = case o of
    OInt  i -> OInt  (pred i)
    OWord i -> OWord (pred i)
    OLong i -> OLong (pred i)
    OType i -> OType (pred i)
  succ o = case o of
    OInt  i -> OInt  (succ i)
    OWord i -> OWord (succ i)
    OLong i -> OLong (succ i)
    OType i -> OType (succ i)

----------------------------------------------------------------------------------------------------

object2Dynamic :: Object -> Dynamic
object2Dynamic o = case o of
  ONull       -> toDyn False
  OTrue       -> toDyn True
  OType     o -> toDyn o
  OInt      o -> toDyn o
  OWord     o -> toDyn o
  OLong     o -> toDyn o
  OFloat    o -> toDyn o
  ORatio    o -> toDyn o
  OComplex  o -> toDyn o
  OTime     o -> toDyn o
  ODiffTime o -> toDyn o
  OChar     o -> toDyn o
  OString   o -> toDyn o
  ORef      o -> toDyn o
  OPair     o -> toDyn o
  OList     o -> toDyn o
  OSet      o -> toDyn o
  OArray    o -> toDyn o
  OIntMap   o -> toDyn o
  ODict     o -> toDyn o
  OTree     o -> toDyn o
  OScript   o -> toDyn o
  OPattern  o -> toDyn o
  ORule     o -> toDyn o
  OBytes    o -> toDyn o

castObj :: Typeable t => Object -> t
castObj o = fromDyn (object2Dynamic o) (throw (OType (objType o)))

obj :: Typeable t => Object -> [t]
obj o = maybeToList (fromDynamic (object2Dynamic o))

objectsOfType :: Typeable t => [Object] -> [t]
objectsOfType ox = concatMap obj ox

readObjUStr :: Read a => (a -> Object) -> UStr -> Object
readObjUStr mkObj = mkObj . read . uchars

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

getComment :: Com a -> [UStr]
getComment com = map commentString $ case com of
  Com         _   -> []
  ComBefore a _   -> a
  ComAfter    _ b -> b
  ComAround a _ b -> a++b

instance Functor Com where
  fmap fn c = case c of
    Com          a    -> Com          (fn a)
    ComBefore c1 a    -> ComBefore c1 (fn a)
    ComAfter     a c2 -> ComAfter     (fn a) c2
    ComAround c1 a c2 -> ComAround c1 (fn a) c2

----------------------------------------------------------------------------------------------------

-- | This is the data structure used to store rules as serialized data, although when a bytecode
-- program is loaded, rules do not exist, the 'ORule' object constructor contains this structure.
data RuleExpr
  = RuleExpr
    { rulePattern :: Com [Com Pattern]
    , ruleAction  :: Com [Com ScriptExpr]
    }
    deriving (Eq, Ord, Show, Typeable)

-- | An executable is either a rule action, or a function.
data Executable
  = Executable
    { staticVars :: IORef (M.Map Name Object)
    , executable :: ExecScript ()
    }

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion with Haskell's "Data.Function"). 
data Subroutine
  = Subroutine
    { argsPattern   :: [ObjPat]
    , subSourceCode :: [Com ScriptExpr]
    , getSubExecutable :: Executable
    }
    deriving Typeable

instance Eq Subroutine where
  a == b = argsPattern a == argsPattern b && subSourceCode a == subSourceCode b
instance Ord Subroutine where
  compare a b =
    if subSourceCode a == subSourceCode b
      then  if argsPattern a == argsPattern b then EQ else compare (argsPattern a) (argsPattern b)
      else  compare (subSourceCode a) (subSourceCode b)
instance Show Subroutine where
  show a = concat $
    [ "func(", intercalate ", " (map show (argsPattern a))
    , ") { " , concatMap (\a -> show a++"; ") (subSourceCode a), "}"
    ]

-- | All evaluation of the Dao language takes place in the 'ExecScript' monad. It allows @IO@
-- functions to be lifeted into it so functions from "Control.Concurrent", "Dao.Document",
-- "System.IO", and other modules, can be evaluated.
type ExecScript a  = CEReader ExecUnit IO a

----------------------------------------------------------------------------------------------------

data UpdateOp = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UORB | UANDB | UXORB | USHL | USHR
  deriving (Eq, Ord, Enum, Ix, Typeable)

instance Show UpdateOp where
  show a = case a of
    UCONST -> "="
    UADD   -> "+="
    USUB   -> "-="
    UMULT  -> "*="
    UDIV   -> "/="
    UMOD   -> "%="
    UORB   -> "|="
    UANDB  -> "&="
    UXORB  -> "^="
    USHL   -> "<<="
    USHR   -> ">>="

instance Read UpdateOp where
  readsPrec _ str = case str of
    "="   -> [(UCONST, "")]
    "+="  -> [(UADD  , "")]
    "-="  -> [(USUB  , "")]
    "*="  -> [(UMULT , "")]
    "/="  -> [(UDIV  , "")]
    "%="  -> [(UMOD  , "")]
    "|="  -> [(UORB  , "")]
    "&="  -> [(UANDB , "")]
    "^="  -> [(UXORB , "")]
    "<<=" -> [(USHL  , "")]
    ">>=" -> [(USHR  , "")]
    _     -> []

instance Bounded UpdateOp where {minBound = UCONST; maxBound = USHR}

data ArithOp
  = REF   | DEREF | INVB  | NOT   | NEG -- ^ unary
  | POINT | DOT                         -- ^ special reference
  | OR    | AND                         -- ^ boolean logical
  | ORB   | ANDB  | XORB  | SHL   | SHR -- ^ bitwise
  | ADD   | SUB   | MULT  | DIV   | MOD -- ^ basic arithmetic
  | POW   | EXP   | SQRT  | LOG         -- ^ root and exponents
  | ABS   | ROUND | TRUNC               -- ^ special arithmetic
  | SIN   | COS   | TAN   | ASIN  | ACOS  | ATAN  -- ^ trigonometric
  | SINH  | COSH  | TANH  | ASINH | ACOSH | ATANH -- ^ hyperbolic
  deriving (Eq, Ord, Enum, Ix, Typeable)

instance Show ArithOp where
  show a = case a of
    { ADD  -> "+";    SUB  -> "-";    MULT  -> "*";    DIV   -> "/";     MOD   -> "%"; ORB  -> "|"
    ; NOT  -> "!";    OR   -> "||";   AND   -> "&&";   ANDB  -> "&";     XORB  -> "^"; INVB -> "~"
    ; SHL  -> "<<";   SHR  -> ">>";   ABS   -> "abs";  NEG   -> "-";     POW   -> "**"
    ; SQRT -> "sqrt"; EXP  -> "exp";  LOG   -> "log";  ROUND -> "round"; TRUNC -> "trunc"
    ; SIN  -> "sin";  COS  -> "cos";  TAN   -> "tan";  ASIN  -> "asin";  ACOS  -> "acos";  ATAN  -> "atan"
    ; SINH -> "sinh"; COSH -> "cosh"; TANH  -> "tanh"; ASINH -> "asinh"; ACOSH -> "acosh"; ATANH -> "atanh"
    ; DOT  -> ".";    REF  -> "$";    DEREF -> "@";    POINT -> "->"
    }

instance Read ArithOp where
  readsPrec _ str = case str of
    { "+"    -> [(ADD  , "")]; "-"     -> [(SUB  , "")]; "*"     -> [(MULT , "")]
    ; "/"    -> [(DIV  , "")]; "%"     -> [(MOD  , "")]; "**"    -> [(POW  , "")]
    ; "exp"  -> [(EXP  , "")]; "|"     -> [(ORB  , "")]; "!"     -> [(NOT  , "")]
    ; "||"   -> [(OR   , "")]; "&&"    -> [(AND  , "")]
    ; "&"    -> [(ANDB , "")]; "^"     -> [(XORB , "")]; "~"     -> [(INVB , "")]
    ; "<<"   -> [(SHL  , "")]; ">>"    -> [(SHR  , "")]; "."     -> [(DOT  , "")]
    ; "$"    -> [(REF  , "")]; "@"     -> [(DEREF, "")]; "->"    -> [(POINT, "")]
    ; "sin"  -> [(SIN  , "")]; "cos"   -> [(COS  , "")]; "tan"   -> [(TAN  , "")]
    ; "asin" -> [(ASIN , "")]; "acos"  -> [(ACOS , "")]; "atan"  -> [(ATAN , "")]
    ; "sinh" -> [(SINH , "")]; "cosh"  -> [(COSH , "")]; "tanh"  -> [(TANH , "")]
    ; "asinh"-> [(ASINH, "")]; "acosh" -> [(ACOSH, "")]; "atanh" -> [(ATANH, "")]
    ; _      -> []
    }

instance Bounded ArithOp where {minBound = REF; maxBound = POINT}

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data ObjectExpr
  = VoidExpr -- ^ Not a language construct, but used where an object expression is optional.
  | Literal      Object                                   Location
  | AssignExpr   ObjectExpr  (Com UpdateOp)  ObjectExpr   Location
  | Equation     ObjectExpr  (Com ArithOp)   ObjectExpr   Location
  | ParenExpr    Bool                   (Com ObjectExpr)  Location
  | ArraySubExpr ObjectExpr  [Comment]  (Com ObjectExpr)  Location
  | FuncCall     Name        [Comment]  [Com ObjectExpr]  Location
  | DictExpr     Name        [Comment]  [Com ObjectExpr]  Location
  | ArrayExpr    (Com [Com ObjectExpr]) [Com ObjectExpr]  Location
  | StructExpr   (Com ObjectExpr)       [Com ObjectExpr]  Location
  | LambdaCall   (Com ObjectExpr)       [Com ObjectExpr]  Location
  | LambdaExpr   (Com [Com Name])       [Com ScriptExpr]  Location
    -- ^ Bool is True if the parenthases really exist.
  deriving (Eq, Ord, Show, Typeable)

instance HasLocation ObjectExpr where
  getLocation o = case o of
    VoidExpr -> LocationUnknown
    Literal      _     o -> o
    AssignExpr   _ _ _ o -> o
    Equation     _ _ _ o -> o
    ParenExpr    _ _   o -> o
    ArraySubExpr _ _ _ o -> o
    FuncCall     _ _ _ o -> o
    DictExpr     _ _ _ o -> o
    ArrayExpr    _ _   o -> o
    StructExpr   _ _   o -> o
    LambdaCall   _ _   o -> o
    LambdaExpr   _ _   o -> o
  setLocation o loc = case o of
    VoidExpr             -> VoidExpr
    Literal      a     _ -> Literal      a     loc
    AssignExpr   a b c _ -> AssignExpr   a b c loc
    Equation     a b c _ -> Equation     a b c loc
    ParenExpr    a b   _ -> ParenExpr    a b   loc
    ArraySubExpr a b c _ -> ArraySubExpr a b c loc
    FuncCall     a b c _ -> FuncCall     a b c loc
    DictExpr     a b c _ -> DictExpr     a b c loc
    ArrayExpr    a b   _ -> ArrayExpr    a b   loc
    StructExpr   a b   _ -> StructExpr   a b   loc
    LambdaCall   a b   _ -> LambdaCall   a b   loc
    LambdaExpr   a b   _ -> LambdaExpr   a b   loc

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data ScriptExpr
  = EvalObject   ObjectExpr   [Comment]                                                  Location
  | IfThenElse   [Comment]    ObjectExpr  (Com [Com ScriptExpr])  (Com [Com ScriptExpr]) Location
    -- ^ @if /**/ objExpr /**/ {} /**/ else /**/ if /**/ {} /**/ else /**/ {} /**/@
  | TryCatch     (Com [Com ScriptExpr])   (Com UStr)                   [Com ScriptExpr]  Location
    -- ^ @try /**/ {} /**/ catch /**/ errVar /**/ {}@              
  | ForLoop      (Com Name)               (Com ObjectExpr)             [Com ScriptExpr]  Location
    -- ^ @for /**/ var /**/ in /**/ objExpr /**/ {}@
  | ContinueExpr Bool  [Comment]          (Com ObjectExpr)                               Location
    -- ^ The boolean parameter is True for a "continue" statement, False for a "break" statement.
    -- @continue /**/ ;@ or @continue /**/ if /**/ objExpr /**/ ;@
  | ReturnExpr   Bool                     (Com ObjectExpr)                               Location
    -- ^ The boolean parameter is True foe a "return" statement, False for a "throw" statement.
    -- ^ @return /**/ ;@ or @return /**/ objExpr /**/ ;@
  | WithDoc      (Com ObjectExpr)         [Com ScriptExpr]                               Location
    -- ^ @with /**/ objExpr /**/ {}@
  deriving (Eq, Ord, Show, Typeable)

instance HasLocation ScriptExpr where
  getLocation o = case o of
    EvalObject   _ _     o -> o
    IfThenElse   _ _ _ _ o -> o
    TryCatch     _ _ _   o -> o
    ForLoop      _ _ _   o -> o
    ContinueExpr _ _ _   o -> o
    ReturnExpr   _ _     o -> o
    WithDoc      _ _     o -> o
  setLocation o loc = case o of
    EvalObject   a b   _   -> EvalObject   a b     loc
    IfThenElse   a b c d _ -> IfThenElse   a b c d loc
    TryCatch     a b c _   -> TryCatch     a b c   loc
    ForLoop      a b c _   -> ForLoop      a b c   loc
    ContinueExpr a b c _   -> ContinueExpr a b c   loc
    ReturnExpr   a b   _   -> ReturnExpr   a b     loc
    WithDoc      a b   _   -> WithDoc      a b     loc

-- | A 'TopLevelExpr' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data TopLevelExpr
  = Attribute      (Com Name)    (Com Name)        Location
  | ToplevelDefine (Com [Name])  (Com ObjectExpr)  Location
  | TopRuleExpr    (Com RuleExpr)                  Location
  | SetupExpr      (Com [Com ScriptExpr])          Location
  | BeginExpr      (Com [Com ScriptExpr])          Location
  | EndExpr        (Com [Com ScriptExpr])          Location
  | TakedownExpr   (Com [Com ScriptExpr])          Location
  | ToplevelFunc   (Com Name) [Com Name] (Com [Com ScriptExpr]) Location
  deriving (Eq, Ord, Show, Typeable)

instance HasLocation TopLevelExpr where
  getLocation o = case o of
    Attribute      _ _   o -> o
    ToplevelDefine _ _   o -> o
    TopRuleExpr    _     o -> o
    SetupExpr      _     o -> o
    BeginExpr      _     o -> o
    EndExpr        _     o -> o
    TakedownExpr   _     o -> o
    ToplevelFunc   _ _ _ o -> o
  setLocation o loc = case o of
    Attribute      a b   _ -> Attribute      a b   loc
    ToplevelDefine a b   _ -> ToplevelDefine a b   loc
    TopRuleExpr    a     _ -> TopRuleExpr    a     loc
    SetupExpr      a     _ -> SetupExpr      a     loc
    BeginExpr      a     _ -> BeginExpr      a     loc
    EndExpr        a     _ -> EndExpr        a     loc
    TakedownExpr   a     _ -> TakedownExpr   a     loc
    ToplevelFunc   a b c _ -> ToplevelFunc   a b c loc

----------------------------------------------------------------------------------------------------

-- | Some matching operations can operate on objects with set-like properties ('Dao.Object.OSet',
-- 'Dao.Object.ODict', 'Dao.Object.OIntMap', 'Dao.Object.OTree'). This data type represents the
-- set operation for a set-like matching pattern. See also: 'ObjNameSet', 'ObjIntSet', 'ObjElemSet',
-- 'ObjChoice'.
data ObjSetOp
  = ExactSet -- ^ every pattern must match every item, no missing items, no spare items.
  | AnyOfSet -- ^ any of the patterns match any of the items in the set
  | AllOfSet -- ^ all of the patterns match, but it doesn't matter if not all items were matched.
  | OnlyOneOf -- ^ only one of the patterns in the set matches only one of the items.
  | NoneOfSet -- ^ all of the patterns do not match any of the items in the set.
  deriving (Eq, Ord, Enum, Typeable, Show, Read)

-- | An object pattern, a data type that can be matched against objects,
-- assigning portions of that object to variables stored in a
-- 'Dao.Tree.Tree' structure.
data ObjPat 
  = ObjAnyX -- ^ matches any number of objects, matches lazily (not greedily).
  | ObjMany -- ^ like ObjAnyX but matches greedily.
  | ObjAny1 -- ^ matches any one object
  | ObjEQ      Object -- ^ simply checks if the object is exactly equivalent
  | ObjType    (EnumSet TypeID) -- ^ checks if the object type is any of the given types.
  | ObjBounded (EnumInf T_ratio) (EnumInf T_ratio)
    -- ^ checks that numeric types are in a certain range.
  | ObjList    TypeID            [ObjPat]
    -- ^ recurse into a list-like object given by TypeID (TrueType for any list-like object)
  | ObjNameSet ObjSetOp          (S.Set [Name])
    -- ^ checks if a map object contains every name
  | ObjIntSet  ObjSetOp          IS.IntSet
    -- ^ checks if an intmap or array object contains every index
  | ObjElemSet ObjSetOp          (S.Set ObjPat)
    -- ^ recurse into a set-like object given by TypeID, match elements in the set according to
    -- ObjSetOp.
  | ObjChoice  ObjSetOp          (S.Set ObjPat) -- ^ execute a series of tests on a single object
  | ObjLabel   Name  ObjPat
    -- ^ if the object matching matches this portion of the 'ObjPat', then save the object into the
    -- resulting 'Dao.Tree.Tree' under this name.
  | ObjFailIf  UStr  ObjPat -- ^ fail with a message if the pattern does not match
  | ObjNot           ObjPat -- ^ succedes if the given pattern fails to match.
  deriving (Eq, Show, Typeable)

instance Ord ObjPat where
  compare a b
    | a==b      = EQ
    | otherwise = compare (toInt a) (toInt b) where
        f s = foldl max 0 (map toInt (S.elems s))
        toInt a = case a of
          ObjAnyX   -> 1
          ObjMany   -> 2
          ObjAny1   -> 3
          ObjEQ   _ -> 4
          ObjType _ -> 5
          ObjBounded _ _ -> 6
          ObjList    _ _ -> 7
          ObjNameSet _ _ -> 8
          ObjIntSet  _ _ -> 9
          ObjElemSet _ s -> f s
          ObjChoice  _ s -> f s
          ObjNot       a -> toInt a
          ObjLabel   _ a -> toInt a
          ObjFailIf  _ w -> toInt a

----------------------------------------------------------------------------------------------------

-- "src/Dao/Types.hs"  provides data types that are used throughout
-- the Dao System to facilitate execution of Dao programs, but are not
-- used directly by the Dao scripting language as Objects are.
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

catchErrorCall :: Run a -> Run (Either ErrorCall a)
catchErrorCall fn = ReaderT $ \r -> try (runReaderT fn r)

newtype Stack key val = Stack { mapList :: [T.Tree key val] }

emptyStack :: Stack key val
emptyStack = Stack []

stackLookup :: Ord key => [key] -> Stack key val -> Maybe val
stackLookup key stack = case mapList stack of
  []        -> Nothing
  (stack:_) -> T.lookup key stack

stackUpdate :: Ord key => [key] -> (Maybe val -> Maybe val) -> Stack key val -> Stack key val
stackUpdate key updVal stack =
  Stack
  { mapList = case mapList stack of
      []   -> []
      m:mx -> T.update key updVal m : mx
  }

-- | Define or undefine a value at an address on the top tree in the stack.
stackDefine :: Ord key => [key] -> Maybe val -> Stack key val -> Stack key val
stackDefine key val = stackUpdate key (const val)

stackPush :: Ord key => T.Tree key val -> Stack key val -> Stack key val
stackPush init stack = stack{ mapList = init : mapList stack }

stackPop :: Ord key => Stack key val -> Stack key val
stackPop stack = stack{ mapList = let mx = mapList stack in if null mx then [] else tail mx }

----------------------------------------------------------------------------------------------------

-- | In several sections of the Dao System internals, a mutext containing some map or tree object is
-- used to store values, for example @'Dao.Debug.DMVar' ('Data.Map.Map' a)@. It is a race condition
-- if multiple threads try to update and read these values without providing some kind of locking.
-- The 'Resource' type provides this locking mechanism. Threads can read without blocking, threads
-- must wait their turn to write/update/delete values if they don't have the lock. All exceptions
-- are handled appropriately. However, caching must be performed by each thread to make sure an
-- update does not produce two different values across two separate read operations, the 'Resource'
-- mechanism does *NOT* provide this caching.
data Resource stor ref =
  Resource
  { resource       :: DMVar (stor Object, stor (DQSem, Maybe Object))
  , updateUnlocked :: ref -> Maybe Object -> stor Object -> stor Object
  , lookupUnlocked :: ref -> stor Object -> Maybe Object
  , updateLocked   :: ref -> Maybe (DQSem, Maybe Object) -> stor (DQSem, Maybe Object) -> stor (DQSem, Maybe Object)
  , lookupLocked   :: ref -> stor (DQSem, Maybe Object) -> Maybe (DQSem, Maybe Object)
  } -- NOTE: this data type needs to be opaque.
    -- Do not export the constructor or any of the accessor functions.

type StackResource = Resource (Stack             Name) [Name]
type TreeResource  = Resource (T.Tree            Name) [Name]
type MapResource   = Resource (M.Map             Name) Name
type DocResource   = Resource (StoredFile T.Tree Name) [Name]

----------------------------------------------------------------------------------------------------

-- | A 'SourceCode' is the structure loaded from source code. A 'Program' object is constructed from
-- 'SourceCode'.
data SourceCode
  = SourceCode
    { sourceModified   :: Int
    , sourceFullPath   :: UStr
      -- ^ the URL (full file path) from where this source code was received.
    , sourceModuleName :: Com UStr
      -- ^ the logical name of this program defined by the "module" keyword in the Dao script.
    , directives       :: Com [Com TopLevelExpr]
    }
  deriving (Eq, Ord, Show, Typeable)

-- | This is the executable form of the 'SourceCode', which cannot be serialized, but is structured
-- in such a way as to make execution more efficient. It caches computed 'ScriptExpr'ns as some type
-- of monadic computation 'm'.
data Program
  = Program
    { programModuleName :: Name
    , programImports    :: [UStr]
    , constructScript   :: [[Com ScriptExpr]]
    , destructScript    :: [[Com ScriptExpr]]
    , requiredBuiltins  :: [Name]
    , programAttributes :: M.Map Name Name
    , preExecScript     :: [Executable]
      -- ^ the "guard scripts" that are executed before every string execution.
    , postExecScript    :: [Executable]
      -- ^ the "guard scripts" that are executed after every string execution.
    , programTokenizer  :: Tokenizer
      -- ^ the tokenizer used to break-up string queries before being matched to the rules in the
      -- module associated with this runtime.
    , programComparator :: CompareToken
      -- ^ used to compare string tokens to 'Dao.Pattern.Single' pattern constants.
    , ruleSet           :: DMVar (PatternTree [Executable])
    , globalData        :: TreeResource
    }

----------------------------------------------------------------------------------------------------

-- | The magic number is the first 8 bytes to every 'Document'. It is the ASCII value of the string
-- "DaoData\0".
document_magic_number :: Word64
document_magic_number = 0x44616F4461746100

-- | This is the version number of the line protocol for transmitting document objects.
document_data_version :: Word64
document_data_version = 0

-- | This data type keeps track of information loaded from a file. It allows you to keep track of
-- how many times the object has been updated since it was loaded from disk, and how many times the
-- file has been requested to be opened (so it doesn't have to load the file twice). The real reason
-- this type exists is to make it easier to fit into the 'Dao.Types.Resource' data type, so I never
-- really intended this type to be used for anything other than that.
data StoredFile stor ref dat
  = NotStored { docRootObject :: stor ref dat }
  | StoredFile
    { docRefCount   :: Word
    , docModified   :: Word64
    , docInfo       :: UStr
    , docVersion    :: Word64
    , docRootObject :: stor ref dat
    }

-- | The data stored in a 'Document' is a 'Dao.Tree.Tree' that maps @['UStr']@ addresses to objects.
-- These addresses are much like filesystem paths, but are lists of 'Name's. Every node in the tree
-- can contain one 'Object' and/or another tree containing more objects (like a filesystem
-- directory).
type DocData = T.Tree Name Object
type Document = StoredFile T.Tree Name Object

initDoc :: T_tree -> Document
initDoc docdata =
  StoredFile
  { docRefCount = 0
  , docModified = 0
  , docInfo = nil
  , docVersion = document_data_version
  , docRootObject = docdata
  }

----------------------------------------------------------------------------------------------------

-- | A program table is an 'Control.Concurrent.DMVar.DMVar' associating 'programModuleName's to
-- 'ExecUnit's. The 'ExecUnit's themselves are also stored in an 'Control.Concurrent.DMVar.DMVar',
-- which must be extracted from in order to evaluate any of the 'ExecScript' functions in the IO
-- monad, that way the 'ExecScript' monad itself does not need to constantly extract the 'ExecUnit'
-- from it's containing 'Control.Concurrent.DMVar.DMVar' every time it needs to refer to one of the
-- 'ExecUnit' properties.
type ProgramTable  = DMVar (M.Map Name (DMVar ExecUnit))

-- | Like 'Control.Monad.Reader.runReaderT' but specific to the 'Dao.Object.Evaluator.ExecScript'
-- monad, and is lifted into the 'Run' monad for convenience.
runExecScript :: ExecScript a -> ExecUnit -> Run (ContErr a)
runExecScript fn xunit = ReaderT $ \runtime ->
  runReaderT (runContErrT fn) (xunit{parentRuntime = runtime})

-- Execute a 'Run' monad within an 'ExecScript' monad. Every 'ExecUnit' contains a pointer to the
-- 'Runtime' object that manages it, 
execScriptRun :: Run a -> ExecScript a
execScriptRun fn = ask >>= \xunit -> execIO (runReaderT fn (parentRuntime xunit))

-- | Pair an error message with an object that can help to describe what went wrong.
objectError :: Monad m => Object -> String -> ContErrT m err
objectError o msg = ceError (OPair (OString (ustr msg), o))

----------------------------------------------------------------------------------------------------

-- | All functions that are built-in to the Dao language, or built-in to a library extending the Dao
-- language, are stored in 'Data.Map.Map's from the functions name to an object of this type.
-- Functions of this type are called by 'evalObject' to evaluate expressions written in the Dao
-- language.
newtype DaoFunc = DaoFunc { daoForeignCall :: [Object] -> ExecScript Object }

-- | This is the state that is used to run the evaluation algorithm. Every Dao program file that has
-- been loaded will have a single 'ExecUnit' assigned to it. Parameters that are stored in
-- 'Dao.Debug.DMVar's or 'Dao.Type.Resource's will be shared across all rules which are executed in
-- parallel, so for example 'execHeap' contains the variables global to all rules in a given
-- program. The remainder of the parameters, those not stored in 'Dao.Debug.DMVar's or
-- 'Dao.Type.Resource's, will have a unique copy of those values assigned to each rule as it
-- executes.
data ExecUnit
  = ExecUnit
    { parentRuntime      :: Runtime
      -- ^ a reference to the 'Runtime' that spawned this 'ExecUnit'. Some built-in functions in the
      -- Dao scripting language may make calls that modify the state of the Runtime.
    , currentExecJob     :: Maybe Job
      -- ^ a reference to the 'Job' that is currently running the 'ExecScript' that is using this
      -- 'ExecUnit' state.
    , currentDocument    :: Maybe File
      -- ^ the current document is set by the @with@ statement during execution of a Dao script.
    , currentProgram     :: Maybe Program
      -- ^ the program that is running in this execution unit, this may not be defined in the case
      -- that strings are being executed in an interactive session.
    , currentTask        :: Task
      -- ^ the 'Task' that is currently running. This item includes the 'Executable' and (if
      -- applicable) the current 'Pattern' and 'Match'
    , currentBranch      :: [Name]
      -- ^ set by the @with@ statement during execution of a Dao script. It is used to prefix this
      -- to all global references before reading from or writing to those references.
    , importsTable       :: [DMVar ExecUnit]
      -- ^ a pointer to the ExecUnit of every Dao program imported with the @import@ keyword.
    , execAccessRules    :: FileAccessRules
      -- ^ restricting which files can be loaded by the program associated with this ExecUnit, these
      -- are the rules assigned this program by the 'ProgramRule' which allowed it to be loaded.
    , builtinFuncs       :: M.Map Name DaoFunc
      -- ^ a pointer to the builtin function table provided by the runtime.
    , toplevelFuncs      :: DMVar (M.Map Name [Subroutine])
    , execHeap           :: TreeResource
      -- ^ referes to the same resource as 'Dao.Types.globalData' in 'Dao.Types.Program'
    , execStack          :: DMVar (Stack Name Object)
      -- ^ stack of local variables used during evaluation
    , queryTimeHeap      :: TreeResource
      -- ^ global variables cleared after every string execution
    , referenceCache     :: DMVar (M.Map Reference Object)
      -- ^ Caches lookups. A single 'Dao.Object.ObjectExpr' is not evaluated atomically, it may
      -- require several lookups. If a value at a reference is updated between lookups by a separate
      -- thread, the same reference may evaluate to two different values. Caching prevents this from
      -- happening.
    , execOpenFiles      :: DMVar (M.Map UPath File)
    , recursiveInput     :: DMVar [UStr]
    , uncaughtErrors     :: DMVar [Object]
    }

instance Bugged ExecUnit where
  askDebug           = fmap (runtimeDebugger . parentRuntime) ask
  setDebug dbg xunit = xunit{parentRuntime = (parentRuntime xunit){runtimeDebugger = dbg}}

----------------------------------------------------------------------------------------------------

-- | Rules dictating which files a particular 'ExecUnit' can load at runtime.
data FileAccessRules
  = RestrictFiles  Pattern
    -- ^ files matching this pattern will never be loaded
  | AllowFiles     Pattern
    -- ^ files matching this pattern can be loaded
  | ProgramRule    Pattern [FileAccessRules] [FileAccessRules]
    -- ^ programs matching this pattern can be loaded and will be able to load files by other rules.
    -- Also has a list of rules dictating which built-in function sets are allowed for use, but
    -- these rules are not matched to files, they are matched to the function sets provided by the
    -- 'Runtime'.
  | DirectoryRule  UPath   [FileAccessRules]
    -- ^ access rules will apply to every file in the path of this directory, but other rules
    -- specific to certain files will override these rules.

-- | Anything that can be loaded from the filesystem and used by the Dao 'Runtime' is a type of
-- this.
data File
  = ProgramFile -- ^ a program loaded and executable
    { publicFile  :: Bool
    , filePath    :: Name
    , logicalName :: Name
    , execUnit    :: DMVar ExecUnit
    }
  | ProgramEdit -- ^ a program executable and editable.
    { filePath   :: Name
    , sourceCode :: DMVar SourceCode
    , execUnit   :: DMVar ExecUnit
    }
  | IdeaFile -- ^ a file containing a 'Dao.Tree.Tree' of serialized 'Dao.Object.Object's.
    { filePath :: Name
    , fileData :: DocResource
    }

-- | Used to select programs from the 'pathIndex' that are currently available for recursive
-- execution.
isProgramFile :: File -> Bool
isProgramFile file = case file of
  ProgramFile _ _ _ _ -> True
  _                   -> False

-- | Used to select programs from the 'pathIndex' that are currently available for recursive
-- execution.
isIdeaFile :: File -> Bool
isIdeaFile file = case file of
  IdeaFile _ _ -> True
  _            -> False

-- | A type of function that can split an input query string into 'Dao.Pattern.Tokens'. The default
-- splits up strings on white-spaces, numbers, and punctuation marks.
type Tokenizer = UStr -> ExecScript Tokens

-- | A type of function that can match 'Dao.Pattern.Single' patterns to 'Dao.Pattern.Tokens', the
-- default is the 'Dao.Pattern.exact' function. An alternative is 'Dao.Pattern.approx', which
-- matches strings approximately, ignoring transposed letters and accidental double letters in words.
type CompareToken = UStr -> UStr -> Bool

data Runtime
  = Runtime
    { pathIndex            :: DMVar (M.Map UPath File)
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , logicalNameIndex     :: DMVar (M.Map Name File)
      -- ^ program files have logical names. This index allows for easily looking up 'File's by
      -- their logical name.
    , jobTable             :: DMVar (M.Map ThreadId Job)
      -- ^ A job is any string that has caused execution across loaded dao scripts. This table keeps
      -- track of any jobs started by this runtime.
    , defaultTimeout       :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , functionSets         :: M.Map Name (M.Map Name DaoFunc)
      -- ^ every labeled set of built-in functions provided by this runtime is listed here. This
      -- table is checked when a Dao program is loaded that has "requires" directives.
    , availableTokenizers  :: M.Map Name Tokenizer
      -- ^ a table of available string tokenizers.
    , availableComparators :: M.Map Name CompareToken
      -- ^ a table of available string matching functions.
    , fileAccessRules      :: [FileAccessRules]
      -- ^ rules loaded by config file dicating programs and ideas can be loaded by Dao, and also,
      -- which programs can load which programs and ideas.
    , runtimeDebugger      :: DebugHandle
    }

-- | This is the monad used for most all methods that operate on the 'Runtime' state.
type Run a = ReaderT Runtime IO a

-- | Unlift a 'Run' monad.
runIO :: Runtime -> Run a -> IO a
runIO runtime runFunc = runReaderT runFunc runtime

instance Bugged Runtime where
  askDebug             = fmap runtimeDebugger ask
  setDebug dbg runtime = runtime{runtimeDebugger = dbg}

----------------------------------------------------------------------------------------------------

-- | A 'Task' represents a single thread running a single 'ScriptExpr' in response to a
-- pattern matching its associated rule.
data Task
  = RuleTask
    { taskPattern     :: Object -- ^ Either 'OPattern' or 'ONull'.
    , taskMatch       :: Match
    , taskAction      :: Executable
    , taskExecUnit    :: ExecUnit
    }
  | GuardTask -- ^ Tasks that are created from @BEGIN@ and @END@ blocks in a Dao script.
    { taskGuardAction :: Executable
    , taskExecUnit    :: ExecUnit
    }

-- | A 'Job' keeps track of all threads that are executing in response to an input string.  You can
-- signal the Job to signal all associated threads, you can wait on the 'Job's
-- 'Dao.Runtime.jobTaskCompletion' semaphore to wait for the job to complete, and you can set a
-- timer to time-out this 'Job'.
data Job
  = Job
    { jobTaskThread  :: ThreadId
      -- ^ The thread that loops, waiting for tasks in the queue to complete.
    , jobInputString :: UStr
      -- ^ the input string that triggered this job.
    , jobTimerThread :: DMVar (Maybe ThreadId)
      -- ^ If there is a time limit on this job, the thread sleeping until the timeout occurs is
      -- identified here and can be killed if the last 'Task' finishes before the timeout event.
    , jobCompletion  :: DQSem
      -- ^ This semaphore is signaled when the last 'Task' in the 'Dao.Runtime.taskExecTable' below
      -- completes, and just before the 'jobTaskThread' reads the 'readyTasks'
      -- 'Control.Concurrent.DMVar.DMVar' to launch the next set of waiting tasks.
    , taskCompletion :: DMVar ThreadId
      -- ^ whenever one thread completes, it signals this 'Control.Concurrent.DMVar.DMVar' with it's
      -- own 'Control.Concurrent.ThreadId' so it can be removed from the 'taskExecTable' below.
    , readyTasks     :: DMVar [Task]
      -- ^ used as a channel to launch new task threads. Use 'Control.Concurrent.DMVar.putMVar' to
      -- place 'Task's into this 'Control.Concurrent.DMVar.DMVar'. The task manager algorithm running
      -- in the 'jobTaskThread' above will take these tasks and execute each one in a separate
      -- thread, mapping each task to a 'Control.Concurrent.ThreadId' in the 'taskExecTable'.
    , taskExecTable  :: DMVar (M.Map ThreadId Task)
      -- ^ all running 'Task's associated with this job are stored in this table. It contains a list
      -- of 'Task's, each task is mapped to a 'Control.Concurrent.ThreadId', and each group of
      -- threads is mapped to the 'Program' from where the executing tasks originated.
    , taskFailures   :: DMVar (M.Map Name [(Task, SomeException)])
      -- ^ if a task dies due to an exception raised, then the exception is caught and mapped to the
      -- task. This is different from an error thrown from a script, these are uncaught Haskell
      -- exceptions from "Control.Exception" resulting in a thread being terminated.
    }

----------------------------------------------------------------------------------------------------

-- "src/Dao/Object/Monad.hs"  defines the monad that is used to evaluate
-- expressions written in the Dao scripting lanugage.
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

-- | Used to play the role of an error-handling monad and a continuation monad together. It is
-- basically an identity monad, but can evaluate to 'CEError's instead of relying on
-- 'Control.Exception.throwIO' or 'Prelude.error', and can also work like a continuation by
-- evaluating to 'CEReturn' which signals the execution function finish evaluation immediately. The
-- "Control.Monad" 'Control.Monad.return' function evaluates to 'CENext', which is the identity
-- monad simply returning a value to be passed to the next monad. 'CEError' and 'CEReturn' must
-- contain a value of type 'Dao.Types.Object'.
data ContErr a
  = CENext   a
  | CEError  Object
  | CEReturn Object
  deriving Show

instance Monad ContErr where
  return = CENext
  ma >>= mfa = case ma of
    CENext   a -> mfa a
    CEError  a -> CEError a
    CEReturn a -> CEReturn a

instance Functor ContErr where
  fmap fn mfn = case mfn of
    CENext   a -> CENext (fn a)
    CEError  a -> CEError a
    CEReturn a -> CEReturn a

instance MonadPlus ContErr where
  mzero = CEError ONull
  mplus (CEError _) b = b
  mplus a           _ = a

newtype ContErrT m a = ContErrT { runContErrT :: m (ContErr a) }

instance Monad m => Monad (ContErrT m) where
  return = ContErrT . return . CENext
  (ContErrT ma) >>= mfa = ContErrT $ do
    a <- ma
    case a of
      CENext   a -> runContErrT (mfa a)
      CEError  a -> return (CEError  a)
      CEReturn a -> return (CEReturn a)

instance Monad m => Functor (ContErrT m) where
  fmap fn mfn = mfn >>= return . fn

instance Monad m => MonadPlus (ContErrT m) where
  mzero = ContErrT (return mzero)
  mplus (ContErrT fa) (ContErrT fb) = ContErrT $ do
    a <- fa
    case a of
      CENext   a -> return (CENext   a)
      CEError  _ -> fb
      CEReturn a -> return (CEReturn a)

instance MonadTrans ContErrT where
  lift ma = ContErrT (ma >>= return . CENext)

-- | Force the computation to assume the value of a given 'Dao.Object.Monad.ContErr'. This function
-- can be used to re-throw a 'Dao.Object.Monad.ContErr' value captured by the 'withContErrSt'
-- function.
returnContErr :: Monad m => ContErr a -> ContErrT m a
returnContErr ce = ContErrT (return ce)

ceReturn :: Monad m => Object -> ContErrT m a
ceReturn a = returnContErr (CEReturn a)

ceError :: Monad m => Object -> ContErrT m a
ceError  a = returnContErr (CEError a)

-- | Evaluate a 'ContErrT' function and capture the resultant 'Dao.Object.Monad.ContErr' value,
-- then apply some transformation to that 'ContErr value. For example, you can decide whether or not
-- to evaluate 'Dao.Object.Monad.ceError', 'Dao.Object.Monad.ceReturn', or ordinary
-- 'Control.Monad.return', or just ignore the 'Dao.Object.Monad.ContErr' value entirely.
withContErrSt :: Monad m => CEReader r m a -> (ContErr a -> CEReader r m b) -> CEReader r m b
withContErrSt exe fn = ContErrT $ ReaderT $ \r -> do
  b <- runReaderT (runContErrT exe) r
  runReaderT (runContErrT (fn b)) r

-- | Run an inner function, return its result as a 'ContErr' regardless of the vaue of 'ContErr', in
-- other words, 'ceReturn' and 'ceError' executed by this function will not collapse the
-- continuation beyond this point, you will see the result of the continuation returned by the
-- evaluation of this monadic function. The inner function will evaluate to a 'Dao.Object.Object'
-- wrapped in a 'CENext', 'CEError', or 'CEReturn'. You can "rethrow" the 'ContErr' evaluated by
-- this function by calling 'returnContErr'
catchContErr :: Monad m => ContErrT m a -> ContErrT m (ContErr a)
catchContErr exe = lift (runContErrT exe)

-- | Takes an inner 'ContErrT' monad. If this inner monad evaluates to a 'CEReturn', it will not
-- collapse the continuation monad, and the outer monad will continue evaluation as if a
-- @('CENext' 'Dao.Object.Object')@ value were evaluated.
catchCEReturn :: Monad m => ContErrT m Object -> ContErrT m Object
catchCEReturn exe = catchContErr exe >>= \ce -> case ce of
  CEReturn obj -> return obj
  _            -> returnContErr ce

-- | Takes an inner 'ContErrT' monad. If this inner monad evaluates to a 'CEError', it will not
-- collapse the continuation monad, and the outer monad will continue evaluation as if a
-- @('CENext' 'Dao.Object.Object')@ value were evaluated.
catchCEError :: Monad m => ContErrT m Object -> ContErrT m Object
catchCEError exe = catchContErr exe >>= \ce -> case ce of
  CEError obj -> return obj
  _           -> returnContErr ce

----------------------------------------------------------------------------------------------------

-- $MonadReader
-- Since 'ContErrT' will be used often with a lifted 'Control.Monad.Reader.ReaderT', useful
-- combinators for manipulating the reader value are provided here.

type CEReader r m a = ContErrT (ReaderT r m) a

-- | It is useful to lift 'Control.Monad.Reader.ReaderT' into 'ContErrT' and instantiate the
-- combined monad into the 'Control.Monad.Reader.Class.MonadReader' class.
instance Monad m => MonadReader r (ContErrT (ReaderT r m)) where
  ask = ContErrT (ReaderT (\r -> return (CENext r)))
  local fn next = ContErrT (ReaderT (\r -> runReaderT (runContErrT next) (fn r)))

-- Like 'Control.Monad.Reader.local', but the local function is in the @m@ monad.
localCE :: Monad m => (r -> m r) -> CEReader r m a -> CEReader r m a
localCE fn next = ContErrT (ReaderT (\r -> fn r >>= runReaderT (runContErrT next)))

-- | Execute an IO operation inside of the 'CEReader' monad, assuming IO is lifted into the
-- CEReader.
execIO :: IO a -> CEReader r IO a
execIO fn = lift (lift fn)

execRun :: ReaderT r IO a -> CEReader r IO a
execRun run = ContErrT (fmap CENext run)

-- | Catch an exception from "Control.Exception" in the 'CEReader' monad. Uses
-- 'Control.Exception.hander', which is an IO function, so the CEReader must have the IO monad
-- lifted into it.
-- catchCE :: Exception err => CEReader r IO a -> (err -> CEReader r IO a) -> CEReader r IO a
-- catchCE exe ifError = ContErrT $ ReaderT $ \r ->
--   handle (\e -> runReaderT (runContErrT (ifError e)) r) (runReaderT (runContErrT exe) r)

