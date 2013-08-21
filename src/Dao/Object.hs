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
import           Dao.Parser
import           Dao.Glob
import qualified Dao.EnumSet as Es
import           Dao.Tree as T hiding (map)
import           Dao.Predicate
import           Dao.Procedural

import           Data.Typeable
import           Data.Dynamic
import           Data.Monoid
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

import           Control.Applicative
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
type T_script   = CallableCode
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

instance Es.InfBound TypeID where
  minBoundInf = Es.Point minBound
  maxBoundInf = Es.Point maxBound

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

instance UStrType TypeID where
  ustr = ustr . show
  maybeFromUStr a = case readsPrec 0 (uchars a) of
    [(o, "")] -> Just o
    _         -> Nothing
  fromUStr a = case maybeFromUStr a of
    Nothing -> error (show a++" is not a valid type identifier")
    Just  a -> a

oBool :: Bool -> Object
oBool a = if a then OTrue else ONull

-- | References used throughout the executable script refer to differer places in the Runtime where
-- values can be stored. Because each store is accessed slightly differently, it is necessary to
-- declare, in the abstract syntax tree (AST) representation of the script exactly why types of
-- variables are being accessed so the appropriate read, write, or update action can be planned.
data Reference
  = NullRef
  | IntRef     { intRef    :: Word }  -- ^ reference to a read-only pattern-match variable.
  | LocalRef   { localRef  :: Name } -- ^ reference to a local variable.
  | StaticRef  { localRef  :: Name } -- ^ reference to a permanent static variable (stored per rule/function).
  | QTimeRef   { globalRef :: [Name] } -- ^ reference to a query-time static variable.
  | GlobalRef  { globalRef :: [Name] } -- ^ reference to in-memory data stored per 'Dao.Types.ExecUnit'.
  | ProgramRef { progID    :: Name , subRef    :: Reference } -- ^ reference to a portion of a 'ExecUnit'.
  | FileRef    { filePath  :: UPath, globalRef :: [Name] } -- ^ reference to a variable in a 'File'
  | Subscript  { dereference :: Reference, subscriptValue :: Object } -- ^ reference to value at a subscripted slot in a container object
  | MetaRef    { dereference :: Reference } -- ^ wraps up a 'Reference' as a value that cannot be used as a reference.
  deriving (Eq, Ord, Show, Typeable)
instance Monoid Reference where
  mempty = NullRef
  mappend a b = case b of
    IntRef     _   -> mempty
    LocalRef     b -> fn [b]
    StaticRef    b -> fn [b]
    QTimeRef     b -> fn  b
    GlobalRef    b -> fn  b
    ProgramRef _ b -> mappend a b
    FileRef    _ b -> fn  b
    MetaRef    _   -> mempty
    Subscript  b j -> case a of
      Subscript a i -> Subscript (Subscript (mappend a b) i) j
      a             -> Subscript (mappend a b) j
    where
      fn b = case a of
        IntRef     _   -> mempty
        LocalRef     a -> GlobalRef (a:b)
        StaticRef    a -> mempty
        QTimeRef     a -> QTimeRef     (a++b)
        GlobalRef    a -> GlobalRef    (a++b)
        ProgramRef f a -> ProgramRef f (mappend a (GlobalRef b))
        FileRef    f a -> FileRef    f (a++b)
        MetaRef    _   -> mempty

refSameClass :: Reference -> Reference -> Bool
refSameClass a b = case (a, b) of
  (NullRef       , NullRef        ) -> True
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

ostr :: UStrType u => u -> Object
ostr = OString . ustr

----------------------------------------------------------------------------------------------------

newtype CodeBlock = CodeBlock { codeBlock :: [ScriptExpr] } deriving (Eq, Ord, Show, Typeable)
instance HasLocation CodeBlock where
  getLocation o = case codeBlock o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = CodeBlock (fmap delLocation (codeBlock o))
instance Monoid CodeBlock where
  mempty      = CodeBlock []
  mappend a b = CodeBlock (mappend (codeBlock a) (codeBlock b))

-- | A code block is either a rule action, or a function, and contains an 'Data.IORef.IORef' to it's
-- own static data.
data Subroutine
  = Subroutine
    { origSourceCode :: CodeBlock
    , staticVars     :: IORef (M.Map Name Object)
    , executable     :: Exec (Maybe Object)
    }

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion with Haskell's "Data.Function"). 
data CallableCode
  = CallableCode
    { argsPattern   :: [Pattern]
    , getSubroutine :: Subroutine
    }
  | MacroFunc
    { argsPattern   :: [Pattern]
    , getSubroutine :: Subroutine
    }
  | GlobAction
    { globPattern   :: [Glob]
    , getSubroutine :: Subroutine
    }
  deriving Typeable

instance Eq CallableCode where
  a == b = argsPattern a == argsPattern b

instance Ord CallableCode where
  compare a b =
    let c = compare (argsPattern a) (argsPattern b)
    in  if c==EQ then compare (argsPattern a) (argsPattern b) else c

instance Show CallableCode where
  show a = concat $
    [ "CallableCode{argsPattern=", intercalate ", " (map show (argsPattern a)), "}" ]

----------------------------------------------------------------------------------------------------

-- | All evaluation of the Dao language takes place in the 'Exec' monad. It allows @IO@
-- functions to be lifeted into it so functions from "Control.Concurrent", "Dao.Document",
-- "System.IO", and other modules, can be evaluated.
newtype Exec a  = Exec{ execToProcedural :: Procedural Object (Maybe Object) (ReaderT ExecUnit IO) a }
instance Functor Exec where { fmap f (Exec m) = Exec (fmap f m) }
instance Monad Exec where
  return = Exec . return
  (Exec m) >>= f = Exec (m >>= execToProcedural . f)
  fail = Exec . throwError . ostr
instance MonadReader ExecUnit Exec where
  local upd (Exec fn) = Exec (local upd fn)
  ask = Exec ask
instance MonadError Object Exec where
  throwError = Exec . throwError
  catchError (Exec try) catch = Exec (catchError try (execToProcedural . catch))
instance MonadIO Exec where { liftIO io = Exec (liftIO io) }
instance Applicative Exec where { pure = return; (<*>) = ap; }
instance Bugged ExecUnit Exec where
  askDebug = asks (runtimeDebugger . parentRuntime)
  askState = ask
  setState = local
  debugUnliftIO exe xunit = ioExec exe xunit >>= \ce -> case ce of
    FlowOK     a -> return a
    FlowReturn _ -> error "Exec.debugUnliftIO evaluated to 'FlowReturn'"
    FlowErr  err -> error "Exec.debugUnliftIO evaluated to 'FlowErr'"
instance ProceduralClass Object (Maybe Object) Exec where
  proc         = Exec . proc
  procCatch    = Exec . procCatch . execToProcedural

----------------------------------------------------------------------------------------------------

-- | Any data type that can result in procedural execution in the 'Exec' monad can instantiate this
-- class. This will allow the data type to be used as a kind of executable code that can be passed
-- around and evaluated at arbitrary points in your Dao program.
class Executable exec where { execute :: exec -> Exec (Maybe Object) }

----------------------------------------------------------------------------------------------------

data UpdateOp = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UORB | UANDB | UXORB | USHL | USHR
  deriving (Eq, Ord, Enum, Ix, Typeable, Show)

instance Bounded UpdateOp where {minBound = UCONST; maxBound = USHR}

allUpdateOpChars = "="
allUpdateOpStrs = " = += -= *= /= %= |= &= ^= <<= >>= "

instance UStrType UpdateOp where
  ustr a = ustr $ case a of
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
  maybeFromUStr str = case uchars str of
    "="   -> Just UCONST
    "+="  -> Just UADD  
    "-="  -> Just USUB  
    "*="  -> Just UMULT 
    "/="  -> Just UDIV  
    "%="  -> Just UMOD  
    "|="  -> Just UORB  
    "&="  -> Just UANDB 
    "^="  -> Just UXORB 
    "<<=" -> Just USHL  
    ">>=" -> Just USHR  
    _     -> Nothing
  fromUStr str =
    maybe (error (show str++" is not an assignment/update operator")) id (maybeFromUStr str)

-- | Unary operators.
data PrefixOp
  = REF | DEREF | INVB  | NOT | NEGTIV | POSTIV | GLDOT -- ^ unary
  | GLOBALPFX | LOCALPFX | QTIMEPFX | STATICPFX
  deriving (Eq, Ord, Enum, Ix, Typeable, Show)

allPrefixOpChars = "$@~!-+"
allPrefixOpStrs = " $ @ ~ - + ! "

instance Bounded PrefixOp where { minBound=REF; maxBound=STATICPFX; }

instance UStrType PrefixOp where
  ustr op = ustr $ case op of
    REF    -> "$"
    DEREF  -> "@"
    INVB   -> "~"
    NOT    -> "!"
    NEGTIV -> "-"
    POSTIV -> "+"
    GLDOT  -> "."
    GLOBALPFX -> "global"
    LOCALPFX  -> "local"
    QTIMEPFX  -> "qtime"
    STATICPFX -> "static"
  maybeFromUStr str = case uchars str of
    "$"      -> Just REF
    "@"      -> Just DEREF
    "~"      -> Just INVB
    "!"      -> Just NOT
    "-"      -> Just NEGTIV
    "+"      -> Just POSTIV
    "."      -> Just GLDOT
    "global" -> Just GLOBALPFX
    "local"  -> Just LOCALPFX
    "qtime"  -> Just QTIMEPFX
    "static" -> Just STATICPFX
    str      -> Nothing
  fromUStr str = maybe (error (show str++" is not a prefix opretor")) id (maybeFromUStr str)

-- | Binary operators.
data InfixOp
  = ADD   | SUB   | MULT
  | DIV   | MOD   | POW
  | POINT | DOT   | OR
  | AND   | EQUL  | NEQUL      
  | ORB   | ANDB  | XORB
  | SHL   | SHR
  | GTN   | LTN   | GTEQ  | LTEQ
  deriving (Eq, Ord, Enum, Ix, Typeable, Show)

allInfixOpChars = "+-*/%<>^&|."
allInfixOpStrs = " + - * / % ** -> . || && == != | & ^ << >> < > <= >= "

instance UStrType InfixOp where
  ustr a = ustr $ case a of
    { ADD   -> "+" ; SUB  -> "-" ; MULT  -> "*"
    ; DIV   -> "/" ; MOD  -> "%" ; POW   -> "**"
    ; POINT -> "->"; DOT  -> "." ; OR    -> "||"
    ; AND   -> "&&"; EQUL -> "=="; NEQUL -> "!="
    ; ORB   -> "|" ; ANDB -> "&" ; XORB  -> "^"
    ; SHL   -> "<<"; SHR  -> ">>"
    ; GTN   -> ">" ; LTN  -> "<" ; GTEQ  -> ">="; LTEQ -> "<="
    }
  maybeFromUStr str = case uchars str of
    { "+"  -> Just ADD  ; "-"  -> Just SUB  ; "*"  -> Just MULT 
    ; "/"  -> Just DIV  ; "%"  -> Just MOD  ; "**" -> Just POW  
    ; "->" -> Just POINT; "."  -> Just DOT  ; "||" -> Just OR   
    ; "&&" -> Just AND  ; "==" -> Just EQUL ; "!=" -> Just NEQUL
    ; "|"  -> Just ORB  ; "&"  -> Just ANDB ; "^"  -> Just XORB 
    ; "<<" -> Just SHL  ; ">>" -> Just SHR  ; "<"  -> Just LTN  
    ; ">"  -> Just GTN  ; "<=" -> Just GTEQ ; ">=" -> Just GTEQ 
    ; _    -> Nothing
    }
  fromUStr str = maybe (error (show str++" is not an infix operator")) id (maybeFromUStr str)

instance Bounded InfixOp where { minBound = ADD; maxBound = LTEQ; }

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
instance UStrType LambdaExprType where
  ustr = ustr . show
  maybeFromUStr a = case readsPrec 0 (uchars a) of
    [(a, "")] -> Just a
    _         -> Nothing
  fromUStr a = case maybeFromUStr a of
    Nothing -> error (show a++" is not a valid lambda expression type")
    Just  a -> a

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data ObjectExpr
  = VoidExpr
  | Literal       Object                                   Location
  | AssignExpr    ObjectExpr      UpdateOp     ObjectExpr  Location
  | Equation      ObjectExpr      InfixOp     ObjectExpr  Location
  | PrefixExpr    PrefixOp        ObjectExpr               Location
  | ParenExpr                     ObjectExpr               Location
  | ArraySubExpr  ObjectExpr     [ObjectExpr]              Location
  | FuncCall      ObjectExpr     [ObjectExpr]              Location
  | DictExpr      Name           [ObjectExpr]              Location
  | ArrayExpr     [ObjectExpr]   [ObjectExpr]              Location
  | StructExpr     ObjectExpr    [ObjectExpr]              Location
  | DataExpr      [UStr]                                   Location
  | LambdaExpr    LambdaExprType [ObjectExpr] CodeBlock    Location
  | MetaEvalExpr  ObjectExpr                               Location
  deriving (Eq, Ord, Show, Typeable)

instance HasLocation ObjectExpr where
  getLocation o = case o of
    VoidExpr              -> LocationUnknown
    Literal       _     o -> o
    AssignExpr    _ _ _ o -> o
    Equation      _ _ _ o -> o
    PrefixExpr    _ _   o -> o
    ParenExpr     _     o -> o
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
    ParenExpr     a     _ -> ParenExpr     a     loc
    ArraySubExpr  a b   _ -> ArraySubExpr  a b   loc
    FuncCall      a b   _ -> FuncCall      a b   loc
    DictExpr      a b   _ -> DictExpr      a b   loc
    ArrayExpr     a b   _ -> ArrayExpr     a b   loc
    StructExpr    a b   _ -> StructExpr    a b   loc
    DataExpr      a     _ -> DataExpr      a     loc
    LambdaExpr    a b c _ -> LambdaExpr    a b c loc
    MetaEvalExpr  a     _ -> MetaEvalExpr  a     loc
  delLocation o = case o of
    VoidExpr              -> VoidExpr
    Literal       a     _ -> Literal           a                  lu
    AssignExpr    a b c _ -> AssignExpr   (fd0 a)      b  (fd0 c) lu
    Equation      a b c _ -> Equation     (fd0 a)      b  (fd0 c) lu
    PrefixExpr    a b   _ -> PrefixExpr        a  (fd0 b)         lu
    ParenExpr     a     _ -> ParenExpr    (fd0 a)                 lu
    ArraySubExpr  a b   _ -> ArraySubExpr (fd0 a) (fd1 b)         lu
    FuncCall      a b   _ -> FuncCall     (fd0 a) (fd1 b)         lu
    DictExpr      a b   _ -> DictExpr          a  (fd1 b)         lu
    ArrayExpr     a b   _ -> ArrayExpr    (fd1 a) (fd1 b)         lu
    StructExpr    a b   _ -> StructExpr   (fd0 a) (fd1 b)         lu
    DataExpr      a     _ -> DataExpr          a                  lu
    LambdaExpr    a b c _ -> LambdaExpr        a  (fd1 b) (fd0 c) lu
    MetaEvalExpr  a     _ -> MetaEvalExpr (fd0 a)                 lu
    where
      lu = LocationUnknown
      fd0 :: HasLocation a => a -> a
      fd0 = delLocation
      fd1 :: (HasLocation a, Functor f) => f a -> f a
      fd1 = fmap delLocation

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data ScriptExpr
  = EvalObject   ObjectExpr                              Location
  | IfThenElse   ObjectExpr    CodeBlock   CodeBlock     Location
  | TryCatch     CodeBlock     UStr        CodeBlock     Location
  | ForLoop      Name          ObjectExpr  CodeBlock     Location
  | WhileLoop    ObjectExpr    CodeBlock                 Location
  | ContinueExpr Bool          ObjectExpr                Location
  | ReturnExpr   Bool          ObjectExpr                Location
  | WithDoc      ObjectExpr    CodeBlock                 Location
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
  delLocation o = case o of
    EvalObject   a     _ -> EvalObject   (fd0 a)                 lu
    IfThenElse   a b c _ -> IfThenElse   (fd0 a) (fd0 b) (fd0 c) lu
    TryCatch     a b c _ -> TryCatch     (fd0 a)      b  (fd0 c) lu
    ForLoop      a b c _ -> ForLoop           a  (fd0 b) (fd0 c) lu
    WhileLoop    a b   _ -> WhileLoop    (fd0 a) (fd0 b)         lu
    ContinueExpr a b   _ -> ContinueExpr      a  (fd0 b)         lu
    ReturnExpr   a b   _ -> ReturnExpr        a  (fd0 b)         lu
    WithDoc      a b   _ -> WithDoc      (fd0 a) (fd0 b)         lu
    where
      lu = LocationUnknown
      fd0 :: HasLocation a => a -> a
      fd0 = delLocation
      fd1 :: (HasLocation a, Functor f) => f a -> f a
      fd1 = fmap delLocation

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
  = Attribute      Name               ObjectExpr                 Location
  | TopFunc        Name               [ObjectExpr]  CodeBlock    Location
  | TopScript      ScriptExpr                                    Location
  | TopLambdaExpr  LambdaExprType     [ObjectExpr]  CodeBlock    Location
  | EventExpr      TopLevelEventType  CodeBlock                  Location
  deriving (Eq, Ord, Show, Typeable)

isAttribute :: TopLevelExpr -> Bool
isAttribute toplevel = case toplevel of { Attribute _ _ _ -> True; _ -> False; }

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
  delLocation o = case o of
    Attribute      a b   _ -> Attribute          a  (fd0 b)         lu
    TopFunc        a b c _ -> TopFunc            a  (fd1 b) (fd0 c) lu
    TopScript      a     _ -> TopScript     (fd0 a)                 lu
    TopLambdaExpr  a b c _ -> TopLambdaExpr      a  (fd1 b) (fd0 c) lu
    EventExpr      a b   _ -> EventExpr          a  (fd0 b)         lu
    where
      lu = LocationUnknown
      fd0 :: HasLocation a => a -> a
      fd0 = delLocation
      fd1 :: (HasLocation a, Functor f) => f a -> f a
      fd1 = fmap delLocation

-- | A program is just a list of 'TopLevelExpr's. It serves as the 'Dao.Object.AST.Intermediate'
-- representation of a 'Dao.Object.AST.AST_SourceCode'.
newtype Program = Program { topLevelExprs :: [TopLevelExpr] } deriving (Eq, Ord, Show)
instance HasLocation Program where
  getLocation o = case topLevelExprs o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = Program (fmap delLocation (topLevelExprs o))

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
  | ObjType    (Es.Set TypeID) -- ^ checks if the object type is any of the given types.
  | ObjBounded (Es.Inf T_ratio) (Es.Inf T_ratio)
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

stackPop :: Ord key => Stack key val -> (Stack key val, T.Tree key val)
stackPop stack =
  let mx = mapList stack
  in  if null mx then (stack, T.Void) else (stack{mapList=tail mx}, head mx)

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

-- | Pair an error message with an object that can help to describe what went wrong.
objectError :: Object -> String -> Exec ig
objectError o msg = throwError $ OList [OString (ustr msg), o]

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
    , currentCodeBlock  :: Maybe Subroutine
      -- ^ when evaluating a 'Subroutine' selected by a string query, the 'Action' resulting from
      -- that query is defnied here. It is only 'Data.Maybe.Nothing' when the module is first being
      -- loaded from source code.
    , currentBranch      :: [Name]
      -- ^ set by the @with@ statement during execution of a Dao script. It is used to prefix this
      -- to all global references before reading from or writing to those references.
    , importsTable       :: M.Map Name (Maybe File)
      -- ^ a pointer to the ExecUnit of every Dao program imported with the @import@ keyword.
    , patternTable       :: [CallableCode]
      -- ^ contains functions which are evaluated not by name but by passing objects to them that
      -- match their argument list.
    , builtinFuncs       :: M.Map Name DaoFunc
      -- ^ a pointer to the builtin function table provided by the runtime.
    , topLevelFuncs      :: M.Map Name [CallableCode]
    , execStack          :: IORef (Stack Name Object)
      -- ^ stack of local variables used during evaluation
    , queryTimeHeap      :: IORef T_tree
      -- ^ the global vairables that are assigned only during a single query, and are deleted after
      -- the query has completed.
    , globalData         :: IORef T_tree
      -- ^ global variables cleared after every string execution
    , taskForActions     :: Task
    , execOpenFiles      :: IORef (M.Map UPath File)
    , recursiveInput     :: IORef [UStr]
    , uncaughtErrors     :: IORef [Object]
    ---- used to be elements of Program ----
    , programModuleName :: Maybe UPath
    , programImports    :: [UPath]
    , requiredBuiltins  :: [Name]
    , programAttributes :: M.Map Name Name
    , preExec      :: [Subroutine]
      -- ^ the "guard scripts" that are executed before every string execution.
    , postExec     :: [Subroutine]
      -- ^ the "guard scripts" that are executed after every string execution.
    , quittingTime :: [Subroutine]
    , programTokenizer  :: InputTokenizer
      -- ^ the tokenizer used to break-up string queries before being matched to the rules in the
      -- module associated with this runtime.
    , programComparator :: CompareToken
      -- ^ used to compare string tokens to 'Dao.Glob.Single' pattern constants.
    , ruleSet           :: IORef (PatternTree [Subroutine])
    }
instance HasDebugRef ExecUnit where
  getDebugRef = runtimeDebugger . parentRuntime
  setDebugRef dbg xunit = xunit{parentRuntime = (parentRuntime xunit){runtimeDebugger = dbg}}

-- | An 'Action' is the result of a pattern match that occurs during an input string query. It is a
-- data structure that contains all the information necessary to run an 'Subroutine' assocaited with
-- a 'Glob', including the parent 'ExecUnit', the 'Dao.Glob.Glob' and the
-- 'Dao.Glob.Match' objects, and the 'Executables'.
data Action
  = Action
    { actionQuery      :: Maybe UStr
    , actionPattern    :: Maybe Glob
    , actionMatch      :: Maybe Match
    , actionCodeBlock :: Subroutine
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
type InputTokenizer = UStr -> Exec Tokens

-- | A type of function that can match 'Dao.Glob.Single' patterns to 'Dao.Glob.Tokens', the
-- default is the 'Dao.Glob.exact' function. An alternative is 'Dao.Glob.approx', which
-- matches strings approximately, ignoring transposed letters and accidental double letters in words.
type CompareToken = UStr -> UStr -> Bool

-- | The 'Runtime' is the shared state visible to every module. Every process will have a single
-- 'Runtime' created by the main function, and every 'ExecUnit' created will receive a pointer to
-- thie 'Runtime'. Modules communicate with each other by calling the correct API functions
data Runtime
  = Runtime
    { pathIndex            :: DMVar (M.Map UPath File)
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , provides             :: S.Set UStr
      -- ^ the set of features provided by the current version of Dao, and features are checked by
      -- the "require" statements in Dao script files.
    , defaultTimeout       :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , functionSets         :: M.Map Name (M.Map Name DaoFunc)
      -- ^ every labeled set of built-in functions provided by this runtime is listed here. This
      -- table is checked when a Dao program is loaded that has "requires" directives.
    , taskForExecUnits     :: Task
    , availableTokenizers  :: M.Map Name InputTokenizer
      -- ^ a table of available string tokenizers.
    , availableComparators :: M.Map Name CompareToken
      -- ^ a table of available string matching functions.
    , runtimeDebugger      :: DebugRef
    }
instance HasDebugRef Runtime where
  getDebugRef = asks runtimeDebugger
  setDebugRef dbg runtime = runtime{runtimeDebugger=dbg}

----------------------------------------------------------------------------------------------------

ioExec :: Exec a -> ExecUnit -> IO (FlowCtrl Object (Maybe Object) a)
ioExec func xunit = runReaderT (runProcedural (execToProcedural func)) xunit

newtype ExecHandler a = ExecHandler { execHandler :: ExecUnit -> Handler (FlowCtrl Object (Maybe Object) a) }
instance Functor ExecHandler where { fmap f (ExecHandler h) = ExecHandler (fmap (fmap (fmap f)) h) }

-- | Create an 'ExecHandler'.
ioExecHandler :: Exception e => (e -> Exec a) -> ExecHandler a
ioExecHandler h = ExecHandler (\xunit -> Handler (\e -> ioExec (h e) xunit))

-- | Using an 'ExecHandler' like 'execIOException', catch any exceptions thrown by the Haskell
-- language runtime and wrap them up in the 'Exec' monad.
execCatch :: Exec a -> [ExecHandler a] -> Exec a
execCatch tryFunc handlers = do
  xunit <- ask
  ctrl  <- liftIO $ catches (ioExec tryFunc xunit) (fmap (\h -> execHandler h xunit) handlers)
  proc ctrl

-- | Like 'execCatch' but with the arguments 'Prelude.flip'ped.
execHandle :: [ExecHandler a] -> Exec a -> Exec a
execHandle = flip execCatch

-- | An 'ExecHandler' for catching 'Control.Exception.IOException's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execIOException :: ExecHandler ()
execIOException = ioExecHandler $ \e -> throwError (ostr $ show (e::IOException))

catchReturn :: Monad m => Exec a -> (Maybe Object -> Exec a) -> Exec a
catchReturn fn catch = procCatch fn >>= \ce -> case ce of
  FlowReturn obj -> catch obj
  FlowOK     a   -> proc (FlowOK a)
  FlowErr    obj -> proc (FlowErr obj)

----------------------------------------------------------------------------------------------------

type DepGraph = M.Map UPath [UPath]

getDepFiles :: DepGraph -> [UPath]
getDepFiles = M.keys

