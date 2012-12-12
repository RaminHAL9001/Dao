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
import           Dao.Pattern
import           Dao.Tree as T

import           Data.Typeable
import           Data.Dynamic
import           Data.Maybe
import           Data.Either
import           Data.List
import           Data.Complex
import           Data.Int
import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Array.IArray
import           Data.Time hiding (parseTime)

import           Numeric

import qualified Data.Map                  as M
import qualified Data.IntMap               as I
import qualified Data.Set                  as S
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
type T_intMap   = I.IntMap Object
type T_dict     = M.Map Name Object
type T_tree     = T.Tree Name Object
type T_pattern  = Pattern
type T_rule     = Rule
type T_script   = Script
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

class Commented a where { stripComments :: a -> a }
instance Commented (Com a) where { stripComments = Com . unComment }
instance Commented a => Commented [a] where { stripComments = map stripComments }

----------------------------------------------------------------------------------------------------

-- | A 'Script' is really more of an executable function or subroutine, it has a list of input
-- arguments and an executable block of code of type @['ScriptExrp']@. But the word @Function@ has
-- other meanings in Haskell, so the word 'Script' is used instead.
data Script
  = Script
    { scriptArgv :: Com [Com Name]
    , scriptCode :: Com [Com ScriptExpr]
    }
  deriving (Show, Typeable)

simpleScript :: [Com ScriptExpr] -> Script
simpleScript exprs = Script{scriptArgv = Com [], scriptCode = Com exprs}

instance Eq  Script where { _ == _ = False } -- | TODO: there ought to be a bisimilarity test here
instance Ord Script where { compare _ _ = LT }
instance Commented Script where
  stripComments sc =
    sc{ scriptArgv = stripComments (scriptArgv sc)
      , scriptCode = stripComments (scriptCode sc)
      }

-- | This is the data structure used to store rules as serialized data, although when a bytecode
-- program is loaded, rules do not exist, the 'ORule' object constructor contains this structure.
data Rule
  = Rule
    { rulePattern :: Com [Com Pattern]
    , ruleAction  :: Com [Com ScriptExpr]
    }
    deriving (Eq, Ord, Show, Typeable)

instance Commented Rule where
  stripComments ru =
    ru{ rulePattern = stripComments (rulePattern ru)
      , ruleAction  = stripComments (ruleAction  ru)
      }

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data ObjectExpr
  = Literal       Object
  | AssignExpr    ObjectExpr  (Com Name) ObjectExpr
  | Equation      ObjectExpr  (Com Name) ObjectExpr
  | ArraySubExpr  ObjectExpr  [Comment]  (Com ObjectExpr)
  | FuncCall      Name        [Comment]  [Com ObjectExpr]
  | DictExpr      Name        [Comment]  [Com ObjectExpr]
  | ArrayExpr     (Com [Com ObjectExpr]) [Com ObjectExpr]
  | LambdaCall    (Com ObjectExpr)       [Com ObjectExpr]
  | StructExpr    (Com ObjectExpr)       [Com ObjectExpr]
  | LambdaExpr    (Com [Com Name])       [Com ScriptExpr]
  | ParenExpr     Bool                   (Com ObjectExpr)
    -- ^ Bool is True if the parenthases really exist.
  deriving (Eq, Ord, Show, Typeable)

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data ScriptExpr
  = NO_OP
  | EvalObject   ObjectExpr [Comment]
  | IfThenElse   [Comment]  ObjectExpr  (Com [Com ScriptExpr])  (Com [Com ScriptExpr])
    -- ^ @if /**/ objExpr /**/ {} /**/ else /**/ if /**/ {} /**/ else /**/ {} /**/@
  | TryCatch     (Com [Com ScriptExpr]) (Com UStr)                 [Com ScriptExpr]
    -- ^ @try /**/ {} /**/ catch /**/ errVar /**/ {}@
  | ForLoop      (Com Name)             (Com ObjectExpr)           [Com ScriptExpr]
    -- ^ @for /**/ var /**/ in /**/ objExpr /**/ {}@
  | ContinueExpr Bool  [Comment]        (Com ObjectExpr)
    -- ^ The boolean parameter is True for a "continue" statement, False for a "break" statement.
    -- @continue /**/ ;@ or @continue /**/ if /**/ objExpr /**/ ;@
  | ReturnExpr   Bool                   (Com ObjectExpr)
    -- ^ The boolean parameter is True foe a "return" statement, False for a "throw" statement.
    -- @return /**/ ;@ or @return /**/ objExpr /**/ ;@
  | WithDoc      (Com ObjectExpr)       [Com ScriptExpr]
    -- ^ @with /**/ objExpr /**/ {}@
  deriving (Eq, Ord, Show, Typeable)

instance Commented ObjectExpr where
  stripComments o = case o of
    Literal       a     -> Literal         a
    AssignExpr    a b c -> AssignExpr      a  (u b)    c
    Equation      a b c -> Equation        a  (u b)    c
    ArraySubExpr  a _ c -> ArraySubExpr    a  []    (u c)
    FuncCall      a _ c -> FuncCall        a  []    (u c)
    DictExpr      a _ c -> DictExpr        a  []    (u c)
    ArrayExpr     a b   -> ArrayExpr    (u a) (u b)
    LambdaCall    a b   -> LambdaCall   (u a) (u b)
    StructExpr    a b   -> StructExpr   (u a) (u b)
    LambdaExpr    a b   -> LambdaExpr   (u a) (u b)
    ParenExpr     a b   -> ParenExpr       a  (u b)
    where
      u :: Commented a => a -> a
      u = stripComments

instance Commented ScriptExpr where
  stripComments s = case s of
    NO_OP                 -> NO_OP
    EvalObject    a _     -> EvalObject      a  []
    IfThenElse    _ b c d -> IfThenElse   []       b  (u c) (u d)
    TryCatch      a b c   -> TryCatch     (u a) (u b) (u c)
    ForLoop       a b c   -> ForLoop      (u a) (u b) (u c)
    ContinueExpr  a _ c   -> ContinueExpr    a  []    (u c)
    ReturnExpr    a b     -> ReturnExpr      a  (u b)
    WithDoc       a b     -> WithDoc      (u a) (u b)
    where
      u :: Commented a => a -> a
      u = stripComments

