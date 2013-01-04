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


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Object
  ( module Dao.String
  , module Dao.Object
  ) where

import           Dao.String
import           Dao.Token
import           Dao.Pattern
import           Dao.EnumSet
import           Dao.Tree as T

import           Data.Typeable
import           Data.Dynamic
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

import           Numeric

import qualified Data.Map                  as M
import qualified Data.IntMap               as IM
import qualified Data.Set                  as S
import qualified Data.IntSet               as IS
import qualified Data.ByteString.Lazy      as B

import           Control.Monad
import           Control.Exception

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
type T_script   = FuncExpr
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
  = IntRef     { intRef    :: Int }  -- ^ reference to a read-only pattern-match variable.
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

-- | A 'FuncExpr' is really more of an executable function or subroutine, it has a list of input
-- arguments and an executable block of code of type @['ScriptExrp']@. But the word @Function@ has
-- other meanings in Haskell, so the word 'FuncExpr' is used instead.
data FuncExpr
  = FuncExpr
    { scriptArgv :: Com [Com Name]
    , scriptCode :: Com [Com ScriptExpr]
    }
  deriving (Show, Typeable)

simpleScript :: [Com ScriptExpr] -> FuncExpr
simpleScript exprs = FuncExpr{scriptArgv = Com [], scriptCode = Com exprs}

instance Eq  FuncExpr where { _ == _ = False } -- | TODO: there ought to be a bisimilarity test here
instance Ord FuncExpr where { compare _ _ = LT }

-- | This is the data structure used to store rules as serialized data, although when a bytecode
-- program is loaded, rules do not exist, the 'ORule' object constructor contains this structure.
data RuleExpr
  = RuleExpr
    { rulePattern :: Com [Com Pattern]
    , ruleAction  :: Com [Com ScriptExpr]
    }
    deriving (Eq, Ord, Show, Typeable)

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
  = REF   | DEREF | INVB  | NOT  | NEG
  | ADD   | SUB   | MULT  | DIV  | OR    | AND
  | ANDB  | XORB  | SHL   | SHR  | ABS
  | MOD   | ORB   | SQRT  | EXP  | LOG
  | ROUND | TRUNC | SIN   | COS  | TAN
  | ASIN  | ACOS  | ATAN  | SINH | COSH  | TANH
  | ASINH | ACOSH | ATANH | DOT  | POINT
  deriving (Eq, Ord, Enum, Ix, Typeable)

instance Show ArithOp where
  show a = case a of
    { ADD  -> "+";    SUB  -> "-";    MULT  -> "*";    DIV   -> "/";     MOD   -> "%"; ORB  -> "|"
    ; NOT  -> "!";    OR   -> "||";   AND   -> "&&";   ANDB  -> "&";     XORB  -> "^"; INVB -> "~"
    ; SHL  -> "<<";   SHR  -> ">>";   ABS   -> "abs";  NEG   -> "-";    
    ; SQRT -> "sqrt"; EXP  -> "exp";  LOG   -> "log";  ROUND -> "round"; TRUNC -> "trunc"
    ; SIN  -> "sin";  COS  -> "cos";  TAN   -> "tan";  ASIN  -> "asin";  ACOS  -> "acos";  ATAN  -> "atan"
    ; SINH -> "sinh"; COSH -> "cosh"; TANH  -> "tanh"; ASINH -> "asinh"; ACOSH -> "acosh"; ATANH -> "atanh"
    ; DOT  -> ".";    REF  -> "$";    DEREF -> "@";    POINT -> "->"
    }

instance Read ArithOp where
  readsPrec _ str = case str of
    { "+"    -> [(ADD  , "")]; "-"     -> [(SUB  , "")]; "*"     -> [(MULT , "")]
    ; "/"    -> [(DIV  , "")]; "%"     -> [(MOD  , "")]; "|"     -> [(ORB  , "")]
    ; "!"    -> [(NOT  , "")]; "||"    -> [(OR   , "")]; "&&"    -> [(AND  , "")]
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
  = Attribute      (Com Name) (Com Name)          Location
  | ToplevelDefine (Com [Name]) (Com ObjectExpr)  Location
  | TopRuleExpr    (Com RuleExpr)                 Location
  | SetupExpr      (Com [Com ScriptExpr])         Location
  | BeginExpr      (Com [Com ScriptExpr])         Location
  | EndExpr        (Com [Com ScriptExpr])         Location
  | TakedownExpr   (Com [Com ScriptExpr])         Location
  | ToplevelFunc   (Com ()) (Com Name) (Com [Com Name]) (Com [Com ScriptExpr]) Location
  deriving (Eq, Ord, Show, Typeable)

instance HasLocation TopLevelExpr where
  getLocation o = case o of
    Attribute      _ _     o -> o
    ToplevelDefine _ _     o -> o
    TopRuleExpr    _       o -> o
    SetupExpr      _       o -> o
    BeginExpr      _       o -> o
    EndExpr        _       o -> o
    TakedownExpr   _       o -> o
    ToplevelFunc   _ _ _ _ o -> o
  setLocation o loc = case o of
    Attribute      a b     _ -> Attribute      a b     loc
    ToplevelDefine a b     _ -> ToplevelDefine a b     loc
    TopRuleExpr    a       _ -> TopRuleExpr    a       loc
    SetupExpr      a       _ -> SetupExpr      a       loc
    BeginExpr      a       _ -> BeginExpr      a       loc
    EndExpr        a       _ -> EndExpr        a       loc
    TakedownExpr   a       _ -> TakedownExpr   a       loc
    ToplevelFunc   a b c d _ -> ToplevelFunc   a b c d loc

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

