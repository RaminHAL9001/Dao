-- "src/Dao/Object.hs"  declares the "Object" data type which is the
-- fundamental data type used througout the Dao System.
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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Object
  ( module Dao.String
  , module Dao.Object
  ) where

import           Dao.Debug.OFF
import           Dao.String
import           Dao.Token
import           Dao.Glob
import           Dao.EnumSet
import           Dao.Tree as T hiding (map)
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
import           Control.Monad.Error.Class

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
type T_pattern  = Glob
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
  | GlobType
  | ScriptType
  | RuleType
  | BytesType
  deriving (Eq, Ord, Enum, Typeable, Bounded)

instance Show TypeID where
  show t = case t of
    NullType     -> "null"
    TrueType     -> "true"
    TypeType     -> "type"
    IntType      -> "int"
    WordType     -> "word"
    DiffTimeType -> "diff"
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
    IntMapType   -> "intMap"
    DictType     -> "dict"
    TreeType     -> "tree"
    GlobType     -> "glob"
    ScriptType   -> "script"
    RuleType     -> "rule"
    BytesType    -> "bytes"

instance Read TypeID where
  readsPrec _ str = map (\a -> (a, "")) $ case str of
    "null"    -> [NullType]
    "true"    -> [TrueType]
    "type"    -> [TypeType]
    "int"     -> [IntType]
    "word"    -> [WordType]
    "diff"    -> [DiffTimeType]
    "float"   -> [FloatType]
    "long"    -> [LongType]
    "ratio"   -> [RatioType]
    "complex" -> [ComplexType]
    "time"    -> [TimeType]
    "char"    -> [CharType]
    "string"  -> [StringType]
    "pair"    -> [PairType]
    "ref"     -> [RefType]
    "list"    -> [ListType]
    "set"     -> [SetType]
    "array"   -> [ArrayType]
    "intMap"  -> [IntMapType]
    "dict"    -> [DictType]
    "tree"    -> [TreeType]
    "glob"    -> [GlobType]
    "script"  -> [ScriptType]
    "rule"    -> [RuleType]
    "bytes"   -> [BytesType]
    _         -> []

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
  | ProgramRef { progID    :: Name , subRef    :: Reference } -- ^ reference to a portion of a 'ExecUnit'.
  | FileRef    { filePath  :: UPath, globalRef :: [Name] } -- ^ reference to a variable in a 'File'
  | Subscript  { dereference :: Reference, subscriptValue :: Object } -- ^ reference to value at a subscripted slot in a container object
  | MetaRef    { dereference :: Reference } -- ^ wraps up a 'Reference' as a value that cannot be used as a reference.
  deriving (Eq, Ord, Show, Typeable)

refSameClass :: Reference -> Reference -> Bool
refSameClass a b = case (a, b) of
  (IntRef       _, IntRef        _) -> True
  (LocalRef     _, LocalRef      _) -> True
  (StaticRef    _, StaticRef     _) -> True
  (QTimeRef     _, QTimeRef      _) -> True
  (GlobalRef    _, GlobalRef     _) -> True
  (ProgramRef _ _, ProgramRef  _ _) -> True
  (FileRef    _ _, FileRef     _ _) -> True
  (MetaRef      _, MetaRef       _) -> True
  (Subscript  _ _, Subscript   _ _) -> True
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
  | OGlob      T_pattern
  | OScript    T_script
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
  OGlob     _ -> GlobType
  OScript   _ -> ScriptType
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
  OGlob     o -> toDyn o
  OBytes    o -> toDyn o

castObj :: Typeable t => Object -> t
castObj o = fromDyn (object2Dynamic o) (throw (OType (objType o)))

obj :: Typeable t => Object -> [t]
obj o = maybeToList (fromDynamic (object2Dynamic o))

objectsOfType :: Typeable t => [Object] -> [t]
objectsOfType ox = concatMap obj ox

readObjUStr :: Read a => (a -> Object) -> UStr -> Object
readObjUStr mkObj = mkObj . read . uchars

ostr :: String -> Object
ostr = OString . ustr

----------------------------------------------------------------------------------------------------

-- | An executable is either a rule action, or a function.
data Executable
  = Executable
    { origSourceCode :: [ScriptExpr]
    , staticVars     :: IORef (M.Map Name Object)
    , executable     :: Exec (Maybe Object)
    }

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion with Haskell's "Data.Function"). 
data Subroutine
  = Subroutine
    { argsPattern      :: [Pattern]
    , getSubExecutable :: Executable
    }
  | MacroFunc
    { argsPattern      :: [Pattern]
    , getSubExecutable :: Executable
    }
  | GlobAction
    { globPattern      :: [Glob]
    , getSubExecutable :: Executable
    }
  deriving Typeable

instance Eq Subroutine where
  a == b = argsPattern a == argsPattern b

instance Ord Subroutine where
  compare a b =
    let c = compare (argsPattern a) (argsPattern b)
    in  if c==EQ then compare (argsPattern a) (argsPattern b) else c

instance Show Subroutine where
  show a = concat $
    [ "Subroutine{argsPattern=", intercalate ", " (map show (argsPattern a)), "}" ]

-- | All evaluation of the Dao language takes place in the 'Exec' monad. It allows @IO@
-- functions to be lifeted into it so functions from "Control.Concurrent", "Dao.Document",
-- "System.IO", and other modules, can be evaluated.
type Exec a  = ProcReader ExecUnit IO a

----------------------------------------------------------------------------------------------------

data UpdateOp = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UORB | UANDB | UXORB | USHL | USHR
  deriving (Eq, Ord, Enum, Ix, Typeable)

instance Bounded UpdateOp where {minBound = UCONST; maxBound = USHR}

allUpdateOpChars = "="
allUpdateOpStrs = " = += -= *= /= %= |= &= ^= <<= >>= "

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
  readsPrec _ str = map (\a -> (a, "")) $ case str of
    "="   -> [UCONST]
    "+="  -> [UADD  ]
    "-="  -> [USUB  ]
    "*="  -> [UMULT ]
    "/="  -> [UDIV  ]
    "%="  -> [UMOD  ]
    "|="  -> [UORB  ]
    "&="  -> [UANDB ]
    "^="  -> [UXORB ]
    "<<=" -> [USHL  ]
    ">>=" -> [USHR  ]
    _     -> []

-- | Unary operators.
data ArithOp1
  = REF | DEREF | INVB  | NOT | NEG | GLDOT -- ^ unary
  deriving (Eq, Ord, Enum, Ix, Typeable)

allArithOp1Chars = "$@~!-"
allArithOp1Strs = " $ @ ~ - ! "

instance Bounded ArithOp1 where {minBound = REF; maxBound = NEG}

instance Show ArithOp1 where
  show op = case op of
    { REF -> "$"; DEREF -> "@"; INVB -> "~"; NOT -> "!"; NEG -> "-" }

instance Read ArithOp1 where
  readsPrec _ str = map (\a -> (a, "")) $ case str of
    { "$" -> [REF]; "@" -> [DEREF]; "~" -> [INVB]; "!" -> [NOT]; "-" -> [NEG]; _ -> [] }

-- | Binary operators.
data ArithOp2
  = ADD   | SUB   | MULT
  | DIV   | MOD   | POW
  | POINT | DOT   | OR
  | AND   | EQUL  | NEQUL      
  | ORB   | ANDB  | XORB
  | SHL   | SHR
  | GTN   | LTN   | GTEQ  | LTEQ
  deriving (Eq, Ord, Enum, Ix, Typeable)

allArithOp2Chars = "+-*/%<>^&|."
allArithOp2Strs = " + - * / % ** -> . || && == != | & ^ << >> < > <= => "

instance Show ArithOp2 where
  show a = case a of
    { ADD   -> "+" ; SUB  -> "-" ; MULT  -> "*"
    ; DIV   -> "/" ; MOD  -> "%" ; POW   -> "**"
    ; POINT -> "->"; DOT  -> "." ; OR    -> "||"
    ; AND   -> "&&"; EQUL -> "=="; NEQUL -> "!="
    ; ORB   -> "|" ; ANDB -> "&" ; XORB  -> "^"
    ; SHL   -> "<<"; SHR  -> ">>"
    ; GTN -> ">"; LTN -> "<"; GTEQ -> ">="; LTEQ -> "<="
    }

instance Read ArithOp2 where
  readsPrec _ str = map (\a -> (a, "")) $ case str of
    { "+"  -> [ADD  ]; "-"  -> [SUB  ]; "*"  -> [MULT ]
    ; "/"  -> [DIV  ]; "%"  -> [MOD  ]; "**" -> [POW  ]
    ; "->" -> [POINT]; "."  -> [DOT  ]; "||" -> [OR   ]
    ; "&&" -> [AND  ]; "==" -> [EQUL ]; "!=" -> [NEQUL]
    ; "|"  -> [ORB  ]; "&"  -> [ANDB ]; "^"  -> [XORB ]
    ; "<<" -> [SHL  ]; ">>" -> [SHR  ]; "<"  -> [LTN  ]
    ; ">"  -> [GTN  ]; "<=" -> [GTEQ ]; ">=" -> [GTEQ ]
    ; _    -> []
    }

instance Bounded ArithOp2 where {minBound = ADD; maxBound = SHR}

data LambdaExprType = FuncExprType | RuleExprType | PatExprType deriving (Eq, Ord, Enum, Typeable)
instance Show LambdaExprType where
  show a = case a of
    FuncExprType -> "function"
    RuleExprType -> "rule"
    PatExprType  -> "pattern"
instance Read LambdaExprType where
  readsPrec _ str = map (\a->(a,"")) $ case str of
    "func"     -> [FuncExprType]
    "function" -> [FuncExprType]
    "rule"     -> [RuleExprType]
    "pattern"  -> [PatExprType]
    "pat"      -> [PatExprType]
    _          -> []

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data ObjectExpr
  = VoidExpr
  | Literal       Object                                   Location
  | AssignExpr    ObjectExpr      UpdateOp     ObjectExpr  Location
  | Equation      ObjectExpr      ArithOp2     ObjectExpr  Location
  | PrefixExpr    ArithOp1        ObjectExpr               Location
  | ParenExpr     Bool            ObjectExpr               Location
  | ArraySubExpr  ObjectExpr      ObjectExpr               Location
  | FuncCall      Name           [ObjectExpr]              Location
  | DictExpr      Name           [ObjectExpr]              Location
  | ArrayExpr     [ObjectExpr]   [ObjectExpr]              Location
  | StructExpr     ObjectExpr    [ObjectExpr]              Location
  | DataExpr      [UStr]                                   Location
  | LambdaExpr    LambdaExprType [ObjectExpr] [ScriptExpr] Location
  | MetaEvalExpr  ObjectExpr                               Location
  deriving (Eq, Ord, Show, Typeable)

instance HasLocation ObjectExpr where
  getLocation o = case o of
    VoidExpr              -> LocationUnknown
    Literal       _     o -> o
    AssignExpr    _ _ _ o -> o
    Equation      _ _ _ o -> o
    PrefixExpr    _ _   o -> o
    ParenExpr     _ _   o -> o
    ArraySubExpr  _ _   o -> o
    FuncCall      _ _   o -> o
    DictExpr      _ _   o -> o
    ArrayExpr     _ _   o -> o
    StructExpr    _ _   o -> o
    DataExpr      _     o -> o
    LambdaExpr    _ _ _ o -> o
    MetaEvalExpr  _     o -> o
  setLocation o loc = case o of
    VoidExpr              -> VoidExpr
    Literal       a     _ -> Literal       a     loc
    AssignExpr    a b c _ -> AssignExpr    a b c loc
    Equation      a b c _ -> Equation      a b c loc
    PrefixExpr    a b   _ -> PrefixExpr    a b   loc
    ParenExpr     a b   _ -> ParenExpr     a b   loc
    ArraySubExpr  a b   _ -> ArraySubExpr  a b   loc
    FuncCall      a b   _ -> FuncCall      a b   loc
    DictExpr      a b   _ -> DictExpr      a b   loc
    ArrayExpr     a b   _ -> ArrayExpr     a b   loc
    StructExpr    a b   _ -> StructExpr    a b   loc
    DataExpr      a     _ -> DataExpr      a     loc
    LambdaExpr    a b c _ -> LambdaExpr    a b c loc
    MetaEvalExpr  a     _ -> MetaEvalExpr  a     loc

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data ScriptExpr
  = EvalObject   ObjectExpr                              Location
  | IfThenElse   ObjectExpr   [ScriptExpr] [ScriptExpr]  Location
  | TryCatch     [ScriptExpr]  UStr        [ScriptExpr]  Location
  | ForLoop      Name          ObjectExpr  [ScriptExpr]  Location
  | WhileLoop    ObjectExpr   [ScriptExpr]               Location
  | ContinueExpr Bool          ObjectExpr                Location
  | ReturnExpr   Bool          ObjectExpr                Location
  | WithDoc      ObjectExpr   [ScriptExpr]               Location
  deriving (Eq, Ord, Show, Typeable)

instance HasLocation ScriptExpr where
  getLocation o = case o of
    EvalObject   _     o -> o
    IfThenElse   _ _ _ o -> o
    TryCatch     _ _ _ o -> o
    ForLoop      _ _ _ o -> o
    WhileLoop    _ _   o -> o
    ContinueExpr _ _   o -> o
    ReturnExpr   _ _   o -> o
    WithDoc      _ _   o -> o
  setLocation o loc = case o of
    EvalObject   a     _ -> EvalObject   a     loc
    IfThenElse   a b c _ -> IfThenElse   a b c loc
    TryCatch     a b c _ -> TryCatch     a b c loc
    ForLoop      a b c _ -> ForLoop      a b c loc
    WhileLoop    a b   _ -> WhileLoop    a b   loc
    ContinueExpr a b   _ -> ContinueExpr a b   loc
    ReturnExpr   a b   _ -> ReturnExpr   a b   loc
    WithDoc      a b   _ -> WithDoc      a b   loc

data TopLevelEventType
  = BeginExprType | EndExprType | ExitExprType
  deriving (Eq, Ord, Enum, Typeable)
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
    ""      -> []

-- | A 'TopLevelExpr' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data TopLevelExpr
  = Attribute      Name               Name                       Location
  | TopFunc        Name               [ObjectExpr]  [ScriptExpr] Location
  | TopScript      ScriptExpr                                    Location
  | TopLambdaExpr  LambdaExprType     [ObjectExpr]  [ScriptExpr] Location
  | EventExpr      TopLevelEventType  [ScriptExpr]               Location
  deriving (Eq, Ord, Show, Typeable)

instance HasLocation TopLevelExpr where
  getLocation o = case o of
    Attribute      _ _   o -> o
    TopFunc        _ _ _ o -> o
    TopScript      _     o -> o
    TopLambdaExpr  _ _ _ o -> o
    EventExpr      _ _   o -> o
  setLocation o loc = case o of
    Attribute      a b   _ -> Attribute      a b   loc
    TopFunc        a b c _ -> TopFunc        a b c loc
    TopScript      a     _ -> TopScript      a     loc
    TopLambdaExpr  a b c _ -> TopLambdaExpr  a b c loc
    EventExpr      a b   _ -> EventExpr      a b   loc

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
data Pattern 
  = ObjAnyX -- ^ matches any number of objects, matches lazily (not greedily).
  | ObjMany -- ^ like ObjAnyX but matches greedily.
  | ObjAny1 -- ^ matches any one object
  | ObjEQ      Object -- ^ simply checks if the object is exactly equivalent
  | ObjType    (EnumSet TypeID) -- ^ checks if the object type is any of the given types.
  | ObjBounded (EnumInf T_ratio) (EnumInf T_ratio)
    -- ^ checks that numeric types are in a certain range.
  | ObjList    TypeID            [Pattern]
    -- ^ recurse into a list-like object given by TypeID (TrueType for any list-like object)
  | ObjNameSet ObjSetOp          (S.Set [Name])
    -- ^ checks if a map object contains every name
  | ObjIntSet  ObjSetOp          IS.IntSet
    -- ^ checks if an intmap or array object contains every index
  | ObjElemSet ObjSetOp          (S.Set Pattern)
    -- ^ recurse into a set-like object given by TypeID, match elements in the set according to
    -- ObjSetOp.
  | ObjChoice  ObjSetOp          (S.Set Pattern) -- ^ execute a series of tests on a single object
  | ObjLabel   Name  Pattern
    -- ^ if the object matching matches this portion of the 'Pattern', then save the object into the
    -- resulting 'Dao.Tree.Tree' under this name.
  | ObjFailIf  UStr  Pattern -- ^ fail with a message if the pattern does not match
  | ObjNot           Pattern -- ^ succedes if the given pattern fails to match.
  deriving (Eq, Show, Typeable)

instance Ord Pattern where
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
  { resource        :: DMVar (stor Object, stor (DQSem, Maybe Object))
  , updateUnlocked  :: ref -> Maybe Object -> stor Object -> stor Object
  , lookupUnlocked  :: ref -> stor Object -> Maybe Object
  , updateLocked    :: ref -> Maybe (DQSem, Maybe Object) -> stor (DQSem, Maybe Object) -> stor (DQSem, Maybe Object)
  , lookupLocked    :: ref -> stor (DQSem, Maybe Object) -> Maybe (DQSem, Maybe Object)
  } -- NOTE: this data type needs to be opaque.
    -- Do not export the constructor or any of the accessor functions.

type StackResource = Resource (Stack             Name) [Name]
type TreeResource  = Resource (T.Tree            Name) [Name]
type MapResource   = Resource (M.Map             Name) Name
type DocResource   = Resource (StoredFile T.Tree Name) [Name]

----------------------------------------------------------------------------------------------------

-- | The magic number is the first 8 bytes to every 'Document'. It is the ASCII value of the string
-- @"DaoData\0"@.
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
    { docModified   :: Word64
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
  { docModified = 0
  , docInfo = nil
  , docVersion = document_data_version
  , docRootObject = docdata
  }

----------------------------------------------------------------------------------------------------

-- $Relating_Exec_and_Run
-- The 'Exec' monad is what evaluates Dao code, and keeps the state of an individual Dao
-- module. The 'Run' monad keeps the state of the the Dao runtime, including every Dao module
-- currently ready for execution. Functions in the 'Run' monad evaluate functions in the
-- 'Exec' monad. Functions in the 'Exec' monad have a reference to the 'Run' monad state
-- which evaluated them, and so can use this reference to evaluate 'Run' monadic functions.

-- | Evaluate an 'Exec' monadic function within the 'Run' monad.
runExec :: Exec a -> ExecUnit -> Run (FlowCtrl a)
runExec fn xunit = ReaderT $ \runtime ->
  runReaderT (runProcedural fn) (xunit{parentRuntime = runtime})

-- Evaluate a 'Run' monadic function within an 'Exec' monad.
inExecEvalRun :: Run a -> Exec a
inExecEvalRun fn = ask >>= \xunit -> liftIO (runReaderT fn (parentRuntime xunit))

-- | Pair an error message with an object that can help to describe what went wrong.
objectError :: Monad m => Object -> String -> Procedural m err
objectError o msg = procErr (OPair (OString (ustr msg), o))

----------------------------------------------------------------------------------------------------

-- | All functions that are built-in to the Dao language, or built-in to a library extending the Dao
-- language, are stored in 'Data.Map.Map's from the functions name to an object of this type.
-- Functions of this type are called by 'evalObject' to evaluate expressions written in the Dao
-- language.
data DaoFunc
  = DaoFuncNoDeref { daoForeignCall :: [Object] -> Exec (Maybe Object) }
    -- ^ do not dereference the parameters passed to this function.
  | DaoFuncAutoDeref { daoForeignCall :: [Object] -> Exec (Maybe Object) }
    -- ^ automatically dereference the parameters passed to the function.

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
    , currentWithRef     :: Maybe File
      -- ^ the current document is set by the @with@ statement during execution of a Dao script.
    , currentQuery       :: Maybe UStr
    , currentPattern     :: Maybe Glob
    , currentMatch       :: Maybe Match
    , currentExecutable  :: Maybe Executable
      -- ^ when evaluating an 'Executable' selected by a string query, the 'Action' resulting from
      -- that query is defnied here. It is only 'Data.Maybe.Nothing' when the module is first being
      -- loaded from source code.
    , currentBranch      :: [Name]
      -- ^ set by the @with@ statement during execution of a Dao script. It is used to prefix this
      -- to all global references before reading from or writing to those references.
    , importsTable       :: M.Map Name (Maybe File)
      -- ^ a pointer to the ExecUnit of every Dao program imported with the @import@ keyword.
    , patternTable       :: [Subroutine]
      -- ^ contains functions which are evaluated not by name but by passing objects to them that
      -- match their argument list.
    , execAccessRules    :: FileAccessRules
      -- ^ restricting which files can be loaded by the program associated with this ExecUnit, these
      -- are the rules assigned this program by the 'ProgramRule' which allowed it to be loaded.
    , builtinFuncs       :: M.Map Name DaoFunc
      -- ^ a pointer to the builtin function table provided by the runtime.
    , topLevelFuncs      :: M.Map Name [Subroutine]
    , execStack          :: DMVar (Stack Name Object)
      -- ^ stack of local variables used during evaluation
    , queryTimeHeap      :: TreeResource
      -- ^ the global vairables that are assigned only during a single query, and are deleted after
      -- the query has completed.
    , globalData         :: TreeResource
      -- ^ global variables cleared after every string execution
    , taskForActions     :: Task
    , execOpenFiles      :: DMVar (M.Map UPath File)
    , recursiveInput     :: DMVar [UStr]
    , uncaughtErrors     :: DMVar [Object]
    ---- used to be elements of Program ----
    , programModuleName :: UPath
    , programImports    :: [UPath]
    , constructScript   :: [ScriptExpr]
    , destructScript    :: [ScriptExpr]
    , requiredBuiltins  :: [Name]
    , programAttributes :: M.Map Name Name
    , preExec     :: [Executable]
      -- ^ the "guard scripts" that are executed before every string execution.
    , postExec    :: [Executable]
      -- ^ the "guard scripts" that are executed after every string execution.
    , programTokenizer  :: Tokenizer
      -- ^ the tokenizer used to break-up string queries before being matched to the rules in the
      -- module associated with this runtime.
    , programComparator :: CompareToken
      -- ^ used to compare string tokens to 'Dao.Glob.Single' pattern constants.
    , ruleSet           :: DMVar (PatternTree [Executable])
    }
instance HasDebugRef ExecUnit where
  getDebugRef = runtimeDebugger . parentRuntime
  setDebugRef dbg xunit = xunit{parentRuntime = (parentRuntime xunit){runtimeDebugger = dbg}}

-- | An 'Action' is the result of a pattern match that occurs during an input string query. It is a
-- data structure that contains all the information necessary to run an 'Executable' assocaited with
-- a 'Glob', including the parent 'ExecUnit', the 'Dao.Glob.Glob' and the
-- 'Dao.Glob.Match' objects, and the 'Executables'.
data Action
  = Action
    { actionQuery      :: Maybe UStr
    , actionPattern    :: Maybe Glob
    , actionMatch      :: Maybe Match
    , actionExecutable :: Executable
    }

-- | An 'ActionGroup' is a group of 'Action's created within a given 'ExecUnit', this data structure
-- contains both the list of 'Action's and the 'ExecUnit' from which the actions were generated. The
-- 'Action's within the group will all be evaluated inside of the 'ExecUnit'.
data ActionGroup
  = ActionGroup
    { actionExecUnit :: ExecUnit
    , getActionList  :: [Action]
    }

-- | When an 'ActionGroup' is being executed, each 'Action' in the group is evaluated in it's own
-- thread. The 'Task' keeps track of which threads are running, and provides a 'Dao.Debug.DMVar' for
-- threads to register their completion. 'Dao.Evaluator.taskWaitThreadLoop' can be used to wait for
-- every thread associated with a 'Task' to complete before returning.
data Task
  = Task
    { taskWaitMVar       :: DMVar DThread
    , taskRunningThreads :: DMVar (S.Set DThread)
    }

initTask :: Bugged r (ReaderT r IO) => ReaderT r IO Task
initTask = do
  wait <- dNewEmptyMVar xloc "Task.taskWaitMVar"
  running <- dNewMVar xloc "Task.taskRunningThreads" S.empty
  return (Task{ taskWaitMVar = wait, taskRunningThreads = running })

----------------------------------------------------------------------------------------------------

-- | Rules dictating which files a particular 'ExecUnit' can load at runtime.
data FileAccessRules
  = RestrictFiles  Glob
    -- ^ files matching this pattern will never be loaded
  | AllowFiles     Glob
    -- ^ files matching this pattern can be loaded
  | ProgramRule    Glob [FileAccessRules] [FileAccessRules]
    -- ^ programs matching this pattern can be loaded and will be able to load files by other rules.
    -- Also has a list of rules dictating which built-in function sets are allowed for use, but
    -- these rules are not matched to files, they are matched to the function sets provided by the
    -- 'Runtime'.
  | DirectoryRule  UPath   [FileAccessRules]
    -- ^ access rules will apply to every file in the path of this directory, but other rules
    -- specific to certain files will override these rules.

-- | The Dao 'Runtime' keeps track of all files loaded into memory in a 'Data.Map.Map' that
-- associates 'Dao.String.UPath's to this items of this data type.
data File
  = ProgramFile     ExecUnit    -- ^ "*.dao" files, a module loaded from the file system.
  | DocumentFile    DocResource -- ^ "*.idea" files, 'Object' data loaded from the file system.

-- | Used to select programs from the 'pathIndex' that are currently available for recursive
-- execution.
isProgramFile :: File -> [ExecUnit]
isProgramFile file = case file of
  ProgramFile p -> [p]
  _             -> []

-- | Used to select programs from the 'pathIndex' that are currently available for recursive
-- execution.
isDocumentFile :: File -> [DocResource]
isDocumentFile file = case file of
  DocumentFile d -> [d]
  _              -> []

-- | A type of function that can split an input query string into 'Dao.Glob.Tokens'. The default
-- splits up strings on white-spaces, numbers, and punctuation marks.
type Tokenizer = UStr -> Exec Tokens

-- | A type of function that can match 'Dao.Glob.Single' patterns to 'Dao.Glob.Tokens', the
-- default is the 'Dao.Glob.exact' function. An alternative is 'Dao.Glob.approx', which
-- matches strings approximately, ignoring transposed letters and accidental double letters in words.
type CompareToken = UStr -> UStr -> Bool

data Runtime
  = Runtime
    { pathIndex            :: DMVar (M.Map UPath File)
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , defaultTimeout       :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , functionSets         :: M.Map Name (M.Map Name DaoFunc)
      -- ^ every labeled set of built-in functions provided by this runtime is listed here. This
      -- table is checked when a Dao program is loaded that has "requires" directives.
    , taskForExecUnits     :: Task
    , availableTokenizers  :: M.Map Name Tokenizer
      -- ^ a table of available string tokenizers.
    , availableComparators :: M.Map Name CompareToken
      -- ^ a table of available string matching functions.
    , fileAccessRules      :: [FileAccessRules]
      -- ^ rules loaded by config file dicating programs and ideas can be loaded by Dao, and also,
      -- which programs can load which programs and ideas.
    , runtimeDebugger      :: DebugRef
    , globalExecUnit       :: ExecUnit
      -- ^ the 'ExecUnit' used to initalize all others. Also keeps track of files loaded on the
      -- command line, and can be used to evaluate script expressions.
    }

-- | This is the monad used for most all methods that operate on the 'Runtime' state.
type Run a = ReaderT Runtime IO a

instance HasDebugRef Runtime where
  getDebugRef             = runtimeDebugger
  setDebugRef dbg runtime = runtime{runtimeDebugger = dbg}

----------------------------------------------------------------------------------------------------

-- | Used to play the role of an error-handling monad and a continuation monad together. It is
-- basically an identity monad, but can evaluate to 'FlowErr's instead of relying on
-- 'Control.Exception.throwIO' or 'Prelude.error', and can also work like a continuation by
-- evaluating to 'FlowReturn' which signals the execution function finish evaluation immediately. The
-- "Control.Monad" 'Control.Monad.return' function evaluates to 'FlowOK', which is the identity
-- monad simply returning a value to be passed to the next monad. 'FlowErr' and 'FlowReturn' must
-- contain a value of type 'Dao.Types.Object'.
data FlowCtrl a
  = FlowOK     a
  | FlowErr    Object
  | FlowReturn (Maybe Object)
  deriving Show

instance Monad FlowCtrl where
  return = FlowOK
  ma >>= mfa = case ma of
    FlowOK     a -> mfa a
    FlowErr    a -> FlowErr a
    FlowReturn a -> FlowReturn a

instance Functor FlowCtrl where
  fmap fn mfn = case mfn of
    FlowOK     a -> FlowOK (fn a)
    FlowErr    a -> FlowErr a
    FlowReturn a -> FlowReturn a

instance MonadPlus FlowCtrl where
  mzero = FlowErr ONull
  mplus (FlowErr _) b = b
  mplus a           _ = a

instance MonadError Object FlowCtrl where
  throwError = FlowErr
  catchError ce catch = case ce of
    FlowOK     ce  -> FlowOK     ce
    FlowReturn obj -> FlowReturn obj
    FlowErr    obj -> catch      obj

-- | Since the Dao language is a procedural language, there must exist a monad that mimics the
-- behavior of a procedural program. A procedure may throw errors and return from any point. Thus,
-- 'Control.Monad.Monad' is extended with 'FlowCtrl'.
newtype Procedural m a = Procedural { runProcedural :: m (FlowCtrl a) }

-- | Procedural languages can modify the state of the program anywhere. To mimic this behavior,
-- 'Procedural' is extended with the 'Control.Monad.Reader.ReaderT' monad, where the
-- 'Control.Monad.Reader.ReaderT' can contain the program state in an 'Data.IORef.IORef' or
-- 'Control.Concurrent.MVar.MVar'.
type ProcReader r m a = Procedural (ReaderT r m) a

instance Monad m => Monad (Procedural m) where
  return = Procedural . return . FlowOK
  (Procedural ma) >>= mfa = Procedural $ do
    a <- ma
    case a of
      FlowOK   a -> runProcedural (mfa a)
      FlowErr  a -> return (FlowErr  a)
      FlowReturn a -> return (FlowReturn a)

instance Monad m => Functor (Procedural m) where
  fmap fn mfn = mfn >>= return . fn

instance Monad m => MonadPlus (Procedural m) where
  mzero = Procedural (return mzero)
  mplus (Procedural fa) (Procedural fb) = Procedural $ do
    a <- fa
    case a of
      FlowOK     a -> return (FlowOK   a)
      FlowErr    _ -> fb
      FlowReturn a -> return (FlowReturn a)

instance MonadTrans Procedural where
  lift ma = Procedural (ma >>= return . FlowOK)

instance MonadIO m => MonadIO (Procedural m) where
  liftIO ma = Procedural (liftIO ma >>= return . FlowOK)

instance Monad m => MonadReader r (Procedural (ReaderT r m)) where
  local upd mfn = Procedural (local upd (runProcedural mfn))
  ask = Procedural (ask >>= return . FlowOK)

instance Monad m => MonadError Object (Procedural m) where
  throwError = Procedural . return . FlowErr
  catchError mce catch = Procedural $ runProcedural mce >>= \ce -> case ce of
    FlowOK   a   -> return (FlowOK a)
    FlowReturn obj -> return (FlowReturn obj)
    FlowErr  obj -> runProcedural (catch obj)

catchReturn :: Monad m => Procedural m a -> (Maybe Object -> Procedural m a) -> Procedural m a
catchReturn fn catch = Procedural $ runProcedural fn >>= \ce -> case ce of
  FlowReturn obj -> runProcedural (catch obj)
  FlowOK     a   -> return (FlowOK a)
  FlowErr    obj -> return (FlowErr obj)

-- | Force the computation to assume the value of a given 'FlowCtrl'. This function can be used to
-- re-throw a 'Dao.Object.Monad.FlowCtrl' value captured by the 'procCatch' function.
joinFlowCtrl :: Monad m => FlowCtrl a -> Procedural m a
joinFlowCtrl ce = Procedural (return ce)

-- | Evaluate this function when the proceudre must return.
procReturn :: Monad m => Object -> Procedural m a
procReturn a = joinFlowCtrl (FlowReturn (Just a))

-- | Evaluate this function when procedure must throw an error.
procErr :: Monad m => Object -> Procedural m a
procErr  a = joinFlowCtrl (FlowErr a)

-- | The inverse operation of 'procJoin', catches the inner 'FlowCtrl' of the given 'Procedural'
-- evaluation, regardless of whether or not this function evaluates to 'procReturn' or 'procErr',
-- whereas ordinarily, if the inner 'FlowCtrl' is 'FlowErr' or 'FlowReturn'.
procCatch :: Monad m => Procedural m a -> Procedural m (FlowCtrl a)
procCatch fn = Procedural (runProcedural fn >>= \ce -> return (FlowOK ce))

-- | The inverse operation of 'procCatch', this function evaluates to a 'Procedural' behaving according
-- to the 'FlowCtrl' evaluated from the given function.
procJoin :: Monad m => Procedural m (FlowCtrl a) -> Procedural m a
procJoin mfn = mfn >>= \a -> Procedural (return a)

-- | Takes an inner 'Procedural' monad. If this inner monad evaluates to a 'FlowReturn', it will not
-- collapse the continuation monad, and the outer monad will continue evaluation as if a
-- @('FlowOK' 'Dao.Object.Object')@ value were evaluated.
catchReturnObj :: Monad m => Procedural m Object -> Procedural m (Maybe Object)
catchReturnObj exe = procCatch exe >>= \ce -> case ce of
  FlowReturn obj -> return obj
  _              -> fmap Just (joinFlowCtrl ce)

