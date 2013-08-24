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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Dao.Tree as T
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
import qualified Data.Binary               as B

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Concurrent

import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Error.Class

----------------------------------------------------------------------------------------------------

-- | Instantiate your data type into this class by creating an 'ObjectInterface' for your data type
-- and then wrapping your data type into a 'Data.Dynamic.Dynamic' data type and storing it with an
-- into the 'ObjectInterface'. Minimal complete definition is 'objectInterface'
class Typeable typ => ObjectClass typ where
  -- | This combinator evaluates to a data type used to encapsulate the object-oriented behavior of
  -- your @typ@ at runtime. Use the 'defClass' function to create an 'objectInterface' for your type
  -- using the convenient 'DaoClassDef' monad:
  -- > instance ObjectClass MyType where
  -- >     objectInterface = 'defClass' ('autoDefEquality' >> 'autoDefOrering' >> 'autoDefBinaryFmt' >> ... )
  objectInterface :: ObjectInterface typ
  -- | The default instantiation of 'new' works almost exactly as a C++ programmer would expect:
  -- simply provide a value of your @typ@ and a new 'Object' encapsulaing that value is created.
  new :: typ -> Object
  new a = mkNew a objectInterface where
    mkNew :: (Typeable typ, ObjectClass typ) => typ -> ObjectInterface typ -> Object
    mkNew a ifc = OHaskell $ objectInterfaceToDynamic $ ifc{objectValue = Just a}

-- | This class is used to define methods of converting arbitrary types to 'Dao.Tree.Tree's, where
-- the leaves in the 'Dao.Tree.Tree' are Dao 'Object's. The branches of the 'Dao.Tree.Tree's are all
-- labeled with 'Dao.String.Name's. It is an important interface for being able to maniuplate
-- objects within a script written in the Dao language. 
class Structured a where
  dataToStruct :: a -> T.Tree Name Object
  structToData :: T.Tree Name Object -> PValue UpdateErr a

-- | This is the error type used to report errors that might occur while updating a 'Dao.Tree.Tree'
-- with the 'structToData' function. If an error occurs while stepping through the branches of a
-- tree, you can throw an error with this information using 'Control.Monad.Error.throwError'.
data UpdateErr
  = UpdateErr
    { updateErrMsg  :: Maybe UStr -- ^ the message explaining why the error ocurred.
    , updateErrAddr :: [UStr]     -- ^ the address at which the error was thrown.
    , updateErrTree :: T_tree     -- ^ the sub-tree at the address at which the error was thrown.
    }
instance Show UpdateErr where
  show err = concat $
    [ "constructor failed" ++ maybe " " ((++"\n") . (": "++) . uchars) (updateErrMsg err)
    , "at index: ", intercalate "." (fmap uchars (updateErrAddr err))
    ]

-- | This is all of the functions used by the "Dao.Evaluator" when manipulating objects in a Dao
-- program. Behavior of objects when they are used in "for" statements or "with" statements, or when
-- they are dereferenced using the "@" operator, or when they are used in equations are all defined
-- here.
-- 
-- The 'ObjectInterface' data type instantiates the 'Data.Monoid.Monoid' class in such a way that
-- 'Data.Monoid.mappend' will union the two 'ObjectInterface's, which allows you to basically do
-- multiple inheritance using any number of base classes. The resulting unioned 'ObjectInterface'
-- is otherwise equivalent to an 'Data.Monoid.mempty', unititialized class, /only/ methods in the
-- 'objMethods' tree are unioned. This is because functions like 'objNullTest' and 'objIterator' are
-- Haskell functions that take a 'Data.Dynamic.Dynamic' type which will fail if the inherted class
-- value contains a value of a different Haskell data type than it's inherited classes (which it
-- almost always will).
--
-- If method names overlap, classes on the right of the 'Data.Monoid.mappend' function will
-- overwrite the methods of the classes on the left. Since Dao does not have the @::@ operator from
-- the C++ language, calling methods from super-classes can be done by referencing
-- @this.superClassName.methodName@ or @this.superSuperClassName.superClassName.methodName@.
data ObjectInterface a =
  ObjectInterface
  { objHaskellType  :: TypeRep
  , objSuperClasses :: [ObjectInterface a]
  , objCastFrom     :: Maybe (Object -> a)
  , objEquality     :: Maybe (a -> Object -> Bool)
  , objOrdering     :: Maybe (a -> Object -> Ordering)
  , objBinaryFmt    :: Maybe (a -> T_bytes, T_bytes -> a)
  , objNullTest     :: Maybe (a -> Bool)
    -- ^ return true if the value is null, used by "if" and "while" statements.
  , objIterator     :: Maybe (a -> Exec [Object])
    -- ^ convert this object to a list of other objects, used by "for" statements.
  , objIndexer      :: Maybe (a -> Object -> Exec Object)
    -- ^ the function used when a square-brackets subscripting operation is used on an object of
    -- this type.
  , objTreeFormat   :: Maybe (a -> Exec T_tree, T_tree -> Exec a)
    -- ^ a pair of functions for converting this object to and from a 'T_tree'. Used when
    -- dereferencing this object, that is, with the "@" operator, the point "->" operator, and in
    -- "with" statements.
  , objCodeFormat   :: Maybe (a -> Exec CodeBlock, CodeBlock -> Exec a)
    -- ^ objects do not have pretty printers, but you can convert your object to and from a
    -- 'CodeBlock' data type. 'CodeBlock's can be pretty-printed and parsed by Dao, so this function
    -- provides a method of both parsing and pretty printing an object to/from source code.
  , objUpdateOpTable :: Maybe (Array UpdateOp (UpdateOp -> a -> Object -> Exec Object))
    -- ^ overload the update/assignment operators, @=@, @+=@, @-=@, etc.
  , objInfixOpTable  :: Maybe (Array InfixOp  (InfixOp  -> a -> Object -> Exec Object))
    -- ^ overload the infix operators, @+@, @*@, @&@, etc.
  , objPrefixOpTable :: Maybe (Array PrefixOp (PrefixOp -> a -> Exec Object))
    -- ^ overload the prefix operators, @!@, @-@, @+@, etc.
  , objMethods       :: T.Tree Name (a -> DaoFunc)
    -- ^ provide a list of 'DaoFunc's that can be called on this object, each function mapped to a
    -- name. In the Dao source code, to call one of these functions, you would write code like so:
    -- > myObj.funcName(param1, param2);
    -- In the map returned by this function, you would map the string "funcName" to a function of
    -- type @('Data.Dynamic.Dynamic -> 'DaoFunc')@, where the 'Data.Dynamic.Dynamic' parameter will
    -- contain the "this" pointer.
  , objectValue     :: Maybe a
    -- ^ Contains the actual object.
  }
  deriving Typeable
instance Eq  a => Eq  (ObjectInterface a) where { a==b = objectValue a == objectValue b }
instance Ord a => Ord (ObjectInterface a) where { compare a b = compare (objectValue a) (objectValue b) }

typeRepToName :: TypeRep -> [Name]
typeRepToName = split . show where
  split str = case break ('.'==) str of
    ("", "") -> []
    (nm, "") -> [ustr nm]
    (nm, '.':str) -> ustr nm : split str

-- | This function works a bit like 'Data.Functor.fmap', but maps an 'ObjectInterface' from one type
-- to another. This requires two functions: one that can cast from the given type to the adapted
-- type (to convert outputs of functions), and one that can cast back from the adapted type to the
-- original type (to convert inputs of functions). Each coversion function takes a string as it's
-- first parameter, this is a string containing the name of the function that is currently making
-- use of the conversion operation. Should you need to use 'Prelude.error' or 'execError', this
-- string will allow you to throw more informative error messages.
objectInterfaceAdapter
  :: (Typeable typ_a, Typeable typ_b)
  => (String -> typ_a -> typ_b)
  -> (String -> typ_b -> typ_a)
  -> ObjectInterface typ_a
  -> ObjectInterface typ_b
objectInterfaceAdapter a2b b2a ifc = 
  let uninit  = error "'Dao.Object.fmapObjectInterface' evaluated on uninitialized object"
      newVal  = maybe uninit id (fmap (a2b "objectValue") (objectValue ifc))
      newType = typeOf newVal
  in  ObjectInterface
      { objHaskellType   = newType
      , objSuperClasses  = let n="objSuperClasses"  in fmap (objectInterfaceAdapter (\ _ -> a2b n) (\ _ -> b2a n)) (objSuperClasses ifc)
      , objCastFrom      = let n="objCastFrom"      in fmap (fmap (a2b n)) (objCastFrom ifc)
      , objEquality      = let n="objEquality"      in fmap (\eq   b -> eq   (b2a n b)) (objEquality ifc)
      , objOrdering      = let n="objOrdering"      in fmap (\ord  b -> ord  (b2a n b)) (objOrdering ifc)
      , objBinaryFmt     = let n="objBinaryFmt"     in fmap (\ (toBin , fromBin) -> (toBin  . b2a n, a2b n . fromBin )) (objBinaryFmt  ifc)
      , objNullTest      = let n="objNullTest"      in fmap (\null b -> null (b2a n b)) (objNullTest ifc)
      , objIterator      = let n="objIterator"      in fmap (\iter b -> iter (b2a n b)) (objIterator ifc)
      , objIndexer       = let n="objIndexer"       in fmap (\indx b -> indx (b2a n b)) (objIndexer  ifc)
      , objTreeFormat    = let n="objTreeFormat"    in fmap (\ (toTree, fromTree) -> (toTree . b2a n, fmap (a2b n) . fromTree)) (objTreeFormat ifc)
      , objCodeFormat    = let n="objCodeFormat"    in fmap (\ (toCode, fromCode) -> (toCode . b2a n, fmap (a2b n) . fromCode)) (objCodeFormat ifc)
      , objUpdateOpTable = let n="objUpdateOpTable" in fmap (fmap (\updt op b -> updt op (b2a n b))) (objUpdateOpTable ifc)
      , objInfixOpTable  = let n="objInfixOpTable"  in fmap (fmap (\infx op b -> infx op (b2a n b))) (objInfixOpTable  ifc)
      , objPrefixOpTable = let n="objPrefixOpTabl"  in fmap (fmap (\prfx op b -> prfx op (b2a n b))) (objPrefixOpTable ifc)
      , objMethods       = let n="objMethods"       in fmap (.(b2a n)) (objMethods ifc)
      , objectValue      = let n="objectValue"      in fmap (a2b n) (objectValue ifc)
      }

objectInterfaceToDynamic :: Typeable typ => ObjectInterface typ -> ObjectInterface Dynamic
objectInterfaceToDynamic oi = objectInterfaceAdapter (\ _ -> toDyn) (from oi) oi where
  from :: Typeable typ => ObjectInterface typ -> String -> Dynamic -> typ
  from oi msg dyn = fromDyn dyn (dynErr oi msg dyn)
  origType :: Typeable typ => ObjectInterface typ -> typ
  origType oi = maybe uninit id (objectValue oi)
  uninit = error $
    "'Dao.Object.objectInterfaceToDynamic' evaluated an uninitialized 'ObjectInterface'"
  dynErr :: Typeable typ => ObjectInterface typ -> String -> Dynamic -> typ
  dynErr oi msg dyn = error $ concat $
    [ "The 'Dao.Object.", msg
    , "' function defined for objects of type ", show (typeOf (origType oi))
    , " was evaluated on an object of type ", show (dynTypeRep dyn)
    ]

-- Used to construct an 'ObjectInterface' in a "Control.Monad.State"-ful way. Instantiates
-- 'Data.Monoid.Monoid' to provide 'Data.Monoid.mempty' an allows multiple inheritence by use of the
-- 'Data.Monoid.mappend' function in the same way as
data ObjIfc typ =
  ObjIfc
  { objIfcSuperClasses  :: [ObjIfc typ]
  , objIfcCastFrom      :: Maybe (Object -> Exec typ)
  , objIfcEquality      :: Maybe (typ -> Object -> Bool)
  , objIfcOrdering      :: Maybe (typ -> Object -> Ordering)
  , objIfcBinaryFmt     :: Maybe (typ -> T_bytes, T_bytes -> typ)
  , objIfcNullTest      :: Maybe (typ -> Bool)
  , objIfcIterator      :: Maybe (typ -> Exec [Object])
  , objIfcIndexer       :: Maybe (typ -> Object -> Exec Object)
  , objIfcTreeFormat    :: Maybe (typ -> Exec T_tree, T_tree -> Exec typ)
  , objIfcExprFormat    :: Maybe (typ -> Exec CodeBlock, CodeBlock -> Exec typ)
  , objIfcUpdateOpTable :: [(UpdateOp, UpdateOp -> typ -> Object -> Exec Object)]
  , objIfcInfixOpTable  :: [(InfixOp , InfixOp  -> typ -> Object -> Exec Object)]
  , objIfcPrefixOpTable :: [(PrefixOp, PrefixOp -> typ -> Exec Object)]
  , objIfcMethods       :: T.Tree Name (typ -> DaoFunc)
  , objIfcInitValue     :: Maybe typ
  }
instance Monoid (ObjIfc typ) where
  mempty = 
    ObjIfc
    { objIfcSuperClasses  = []
    , objIfcCastFrom      = Nothing
    , objIfcEquality      = Nothing
    , objIfcOrdering      = Nothing
    , objIfcBinaryFmt     = Nothing
    , objIfcNullTest      = Nothing
    , objIfcIterator      = Nothing
    , objIfcIndexer       = Nothing
    , objIfcTreeFormat    = Nothing
    , objIfcExprFormat    = Nothing
    , objIfcUpdateOpTable = []
    , objIfcInfixOpTable  = []
    , objIfcPrefixOpTable = []
    , objIfcMethods       = T.Void
    , objIfcInitValue     = Nothing
    }
  mappend a b = 
    mempty
    { objIfcSuperClasses = objIfcSuperClasses (a{objIfcInitValue=Nothing}) ++
        objIfcSuperClasses (b{objIfcInitValue=Nothing})
    , objIfcMethods = T.union (objIfcMethods b) (objIfcMethods a)
    , objIfcInitValue = Nothing
    }

-- | A handy monadic interface for defining an 'ObjectInterface'.
type DaoClassDef typ a = State (ObjIfc typ) a

defCastFrom :: Typeable typ => (Object -> Exec typ) -> DaoClassDef typ ()
defCastFrom fn = modify(\st->st{objIfcCastFrom=Just fn})

-- | Automatically define an equality operation over your @typ@ using the instantiation of
-- 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function. The 'defCastFrom'
-- function is used to cast 'Object's to a value of your @typ@, and then the @Prelude.==@ function
-- is evaluated. If you eventually never define a type casting funcion using 'defCastFrom', this
-- function will fail, but it will fail lazily and at runtime, perhaps when you least expect it, so
-- be sure to define 'defCastFrom' at some point.
autoDefEquality :: (Typeable typ, Eq typ, ObjectClass typ) => DaoClassDef typ ()
autoDefEquality = modify (\st -> st{objIfcEquality=Just (equals objectInterface)}) where
  equals :: (Typeable typ, Eq typ, ObjectClass typ) => ObjectInterface typ -> typ -> Object -> Bool
  equals ifc a obj = maybe (err a) (\cast -> a == cast obj) (objCastFrom ifc)
  err a = error ("type casting not defined for "++show (typeOf a)++" type")

-- | Define a customized equality relation for your @typ@, if the 'autoDefEquality' and
-- 'defCastFrom' functions are to be avoided for some reason.
defEquality :: Typeable typ => (typ -> Object -> Bool) -> DaoClassDef typ ()
defEquality fn = modify(\st->st{objIfcEquality=Just fn})

-- | Automatically define an ordering for your @typ@ using the instantiation of
-- 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function. The 'defCastFrom'
-- function is used to cast 'Object's to a value of your @typ@, and then the @Prelude.==@ function
-- is evaluated. If you eventually never define a type casting funcion using 'defCastFrom', this
-- function will fail, but it will fail lazily and at runtime, perhaps when you least expect it, so
-- be sure to define 'defCastFrom' at some point.
autoDefOrdering :: (Typeable typ, Ord typ, ObjectClass typ) => DaoClassDef typ ()
autoDefOrdering = modify (\st -> st{objIfcOrdering=Just (comp objectInterface)}) where
  comp :: (Typeable typ, Ord typ, ObjectClass typ) => ObjectInterface typ -> typ -> Object -> Ordering
  comp ifc a obj = maybe (err a) (\cast -> compare a (cast obj)) (objCastFrom ifc)
  err a = error ("type casting not defined for "++show (typeOf a)++" type")

-- | Define a customized ordering for your @typ@, if the 'autoDefEquality' and 'defCastFrom'
-- functions are to be avoided for some reason.
defOrdering :: Typeable typ => (typ -> Object -> Ordering) -> DaoClassDef typ ()
defOrdering fn = modify(\st->st{objIfcOrdering=Just fn})

-- | Automatically define the binary encoder and decoder using the 'Data.Binary.Binary' class
-- instantiation for this @typ@.
autoDefBinaryFmt :: (Typeable typ, B.Binary typ) => DaoClassDef typ ()
autoDefBinaryFmt = modify(\st->st{objIfcBinaryFmt=Just(B.encode,B.decode)})

-- | If you have binary coding and decoding methods for your @typ@ but for some silly reason not
-- instantiated your @typ@ into the 'Data.Binary.Binary' class, your @typ@ can still be used as a
-- binary formatted object by the Dao system if you define the encoder and decoder using this
-- function. However, it would be better if you instantiated 'Data.Binary.Binary' and used
-- 'autoDefBinaryFmt' instead.
defBinaryFmt :: (Typeable typ, B.Binary typ) => (typ -> T_bytes) -> (T_bytes -> typ) -> DaoClassDef typ ()
defBinaryFmt encode decode = modify(\st->st{objIfcBinaryFmt=Just(encode,decode)})

defNullTest :: Typeable typ => (typ -> Bool) -> DaoClassDef typ ()
defNullTest fn = modify(\st->st{objIfcNullTest=Just fn})

defIterator :: Typeable typ => (typ -> Exec [Object]) -> DaoClassDef typ ()
defIterator fn = modify(\st->st{objIfcIterator=Just fn})

defIndexer :: Typeable typ => (typ -> Object -> Exec Object) -> DaoClassDef typ ()
defIndexer fn = modify(\st->st{objIfcIndexer=Just fn})

-- | Automatically define the tree encoder and decoder using the 'Structured' class instantiation
-- for this @typ@.
autoDefTreeFormat :: (Typeable typ, Structured typ) => DaoClassDef typ ()
autoDefTreeFormat = defTreeFormat (return . dataToStruct) (execFromPValue . structToData)

-- | If for some reason you need to define a tree encoder and decoder for the 'ObjectInterface' of
-- your @typ@ without instnatiating 'Structured', use this function to define the tree encoder an
-- decoder directly
defTreeFormat :: Typeable typ => (typ -> Exec T_tree) -> (T_tree -> Exec typ) -> DaoClassDef typ ()
defTreeFormat encode decode = modify $ \st -> st{objIfcTreeFormat=Just(encode,decode)}

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
type T_ref      = QualRef
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
type T_haskell  = ObjectInterface Dynamic

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
  | HaskellType
  deriving (Eq, Ord, Typeable, Enum, Bounded)

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
    HaskellType  -> "haskell"

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
    "haskell" -> [HaskellType]
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
  | PlainRef  { localRef  :: Name     } -- ^ reference to a local variable.
  | DerefOp   { refObject :: Object   } -- ^ the at-sign operator
  | MetaRef   { refObject :: Object   } -- ^ the dollar-sign operator
  | DotRef    { refLeft   :: Reference, refRight  :: Reference }
  | PointRef  { refLeft   :: Reference, refRight  :: Reference }
  | Subscript { refLeft   :: Reference, refParams :: [Object]  } -- ^ reference to value at a subscripted slot in a container object
  | CallWith  { refLeft   :: Reference, refParams :: [Object]  } -- ^ reference prepended with arguments in parentheses.
  deriving (Eq, Ord, Typeable)
instance Monoid Reference where
  mempty      = NullRef
  mappend b c = loop b c where
    loop b c = case b of
      NullRef       -> c
      PlainRef  _   -> DotRef   b c
      DerefOp   _   -> DotRef   b c
      MetaRef   _   -> DotRef   b c
      DotRef    a b -> DotRef   a (loop b c)
      PointRef  a b -> PointRef a (loop b c)
      Subscript _ _ -> DotRef   b c
      CallWith  _ _ -> DotRef   b c

-- | A 'Reference' type with an optional qualifier. 'Unqualified' means there is no qualifier.
data QualRef = QualRef { qualifier :: RefQualifier, qualRef :: Reference }
  deriving (Eq, Ord, Typeable)

-- | Create a 'QualRef' from any 'Dao.String.UStr'.
bareword :: UStr -> QualRef
bareword = QualRef Unqualified . PlainRef

-- | Reference qualifiers, specify which area of the execution unit a reference is pointing to.
data RefQualifier = Unqualified | LocalRef | QTimeRef | GloDotRef | StaticRef | GlobalRef
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded)
instance Show RefQualifier where
  show q = case q of
    Unqualified -> ""
    LocalRef    -> "local"
    QTimeRef    -> "qtime"
    GloDotRef   -> "."
    StaticRef   -> "static"
    GlobalRef   -> "global"
instance Read RefQualifier where
  readsPrec _ str = fmap (\o -> (o, "")) $ case str of
    ""       -> [Unqualified]
    "local"  -> [LocalRef]
    "qtime"  -> [QTimeRef]
    "."      -> [GloDotRef]
    "static" -> [StaticRef]
    "global" -> [GlobalRef]
    _        -> []

-- | The 'Object' type has constructors for primitive types, and for boxed Haskell types. Haskell
-- types are boxed in 'Data.Dynamic.Dynamic' wrappers.
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
  | OHaskell   T_haskell
  deriving Typeable
instance Eq Object where
  a==b = case (a,b) of
    (ONull       , ONull       ) -> True
    (OTrue       , OTrue       ) -> True
    (OType      a, OType      b) -> a==b
    (OInt       a, OInt       b) -> a==b
    (OWord      a, OWord      b) -> a==b
    (OLong      a, OLong      b) -> a==b
    (OFloat     a, OFloat     b) -> a==b
    (ORatio     a, ORatio     b) -> a==b
    (OComplex   a, OComplex   b) -> a==b
    (OTime      a, OTime      b) -> a==b
    (ODiffTime  a, ODiffTime  b) -> a==b
    (OChar      a, OChar      b) -> a==b
    (OString    a, OString    b) -> a==b
    (ORef       a, ORef       b) -> a==b
    (OPair      a, OPair      b) -> a==b
    (OList      a, OList      b) -> a==b
--  (OSet       a, OSet       b) -> a==b
--  (OArray     a, OArray     b) -> a==b
--  (ODict      a, ODict      b) -> a==b
--  (OIntMap    a, OIntMap    b) -> a==b
    (OTree      a, OTree      b) -> a==b
--  (OGlob      a, OGlob      b) -> a==b
--  (OScript    a, OScript    b) -> a==b
    (OBytes     a, OBytes     b) -> a==b
    (OHaskell   a, OHaskell   b) -> maybe False id $ do
      eq  <- objEquality a
      obj <- objectValue a
      return (eq obj (OHaskell b))
    _                            -> False
instance Ord Object where
  compare a b = case (a,b) of
    (OType      a, OType      b) -> compare a b
    (OInt       a, OInt       b) -> compare a b
    (OWord      a, OWord      b) -> compare a b
    (OLong      a, OLong      b) -> compare a b
    (OFloat     a, OFloat     b) -> compare a b
    (ORatio     a, ORatio     b) -> compare a b
    (OComplex   a, OComplex   b) -> compare a b
    (OTime      a, OTime      b) -> compare a b
    (ODiffTime  a, ODiffTime  b) -> compare a b
    (OChar      a, OChar      b) -> compare a b
    (OString    a, OString    b) -> compare a b
    (ORef       a, ORef       b) -> compare a b
    (OPair      a, OPair      b) -> compare a b
    (OList      a, OList      b) -> compare a b
--  (OSet       a, OSet       b) -> compare a b
--  (OArray     a, OArray     b) -> compare a b
--  (ODict      a, ODict      b) -> compare a b
--  (OIntMap    a, OIntMap    b) -> compare a b
    (OTree      a, OTree      b) -> compare a b
--  (OGlob      a, OGlob      b) -> compare a b
--  (OScript    a, OScript    b) -> compare a b
    (OBytes     a, OBytes     b) -> compare a b
    (OHaskell   a, OHaskell   b) -> maybe (err a b) id $ do
      comp <- objOrdering a
      obj  <- objectValue a
      return (comp obj (OHaskell b))
      where
        err a b = error $ unwords $
          [ "cannot compare object of type", show (objHaskellType a)
          , "with obejct of type", show (objHaskellType b)
          ]
    _                            -> compare (objType a) (objType b)

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
  OHaskell  _ -> HaskellType

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
  OHaskell  o -> toDyn o

ostr :: UStrType u => u -> Object
ostr = OString . ustr

----------------------------------------------------------------------------------------------------

newtype CodeBlock = CodeBlock { codeBlock :: [ScriptExpr] } deriving (Eq, Ord, Typeable)
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

-- | A subroutine is contains a 'CodeBlock' and an 'Data.IORef.IORef' to it's own static data. It
-- also has a reference to the last evaluation of 'execute' over it's 'CodeBlock', which provides a
-- hint to the Haskell runtime system that this code can be cached rather than evaluating the
-- 'CodeBlock' fresh every time. In a sense, it is a "live" 'CodeBlock' that can actually be
-- executed.
data Subroutine
  = Subroutine
    { origSourceCode :: CodeBlock
    , staticVars     :: IORef (M.Map Name Object)
    , executable     :: Exec (Maybe Object)
    }

-- | Dao will have a compile-time type checker in the near future.
data TypeCheck = TypeCheck{ typeCheckSource :: Name }

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion with Haskell's "Data.Function"). 
data CallableCode
  = CallableCode
    { argsPattern   :: [TypeCheck]
    , getSubroutine :: Subroutine
    }
  | GlobAction
    { globPattern   :: [Glob]
    , getSubroutine :: Subroutine
    }
  deriving Typeable

----------------------------------------------------------------------------------------------------

-- | All evaluation of the Dao language takes place in the 'Exec' monad. It instantiates
-- 'Control.Monad.MonadIO.MonadIO' to allow @IO@ functions to be lifeted into it. It instantiates
-- 'Control.Monad.Error.MonadError' and provides it's own exception handling mechanism completely
-- different from the Haskell runtime, so as to allow for more control over exception handling in
-- the Dao runtime.
newtype Exec a  = Exec{ execToProcedural :: Procedural ExecError (Maybe Object) (ReaderT ExecUnit IO) a }
instance Functor Exec where { fmap f (Exec m) = Exec (fmap f m) }
instance Monad Exec where
  return = Exec . return
  (Exec m) >>= f = Exec (m >>= execToProcedural . f)
  fail = execThrow . ostr
instance MonadPlus Exec where
  mzero = throwError (ExecBadParam mempty)
  mplus a b = procCatch a >>= \a -> case a of
    FlowOK      a -> proc (FlowOK     a)
    FlowReturn  a -> proc (FlowReturn a)
    FlowErr   err -> case err of { ExecBadParam _ -> b; _ -> proc (FlowErr err); }
instance MonadReader ExecUnit Exec where
  local upd (Exec fn) = Exec (local upd fn)
  ask = Exec ask
instance MonadError ExecError Exec where
  throwError = Exec . throwError
  catchError (Exec try) catch = Exec (catchError try (execToProcedural . catch))
instance MonadIO Exec where { liftIO io = Exec (liftIO io) }
instance Applicative Exec where { pure = return; (<*>) = ap; }
instance Alternative Exec where { empty = mzero; (<|>) = mplus; }
instance Bugged ExecUnit Exec where
  askDebug = asks (runtimeDebugger . parentRuntime)
  askState = ask
  setState = local
  debugUnliftIO exe xunit = ioExec exe xunit >>= \ce -> case ce of
    FlowOK     a -> return a
    FlowReturn _ -> error "Exec.debugUnliftIO evaluated to 'FlowReturn'"
    FlowErr  err -> error "Exec.debugUnliftIO evaluated to 'FlowErr'"
instance ProceduralClass ExecError (Maybe Object) Exec where
  proc      = Exec . proc
  procCatch = Exec . procCatch . execToProcedural

-- | When calling Dao program functions, arguments to functions are wrapped in this data type.
data ParamValue
  = NoParamInfo
  | ParamValue{ paramValue :: Object, paramOrigExpr :: Maybe ObjectExpr }
instance Monoid ParamValue where { mempty=NoParamInfo; mappend=const; }

-- | Nearly all execution in the 'Exec' monad that could result in an error will throw an
-- 'ExecError's. Even the use of 'Prelude.error' will throw an 'Control.Exception.ErrorCall' that
-- will eventually be caught by the 'Exec' monad and converted to an 'ExecError'.
data ExecError
  = ExecBadParam ParamValue
    -- ^ Exceptions of this type are thrown when passing a 'Object' of the wrong type as a parameter
    -- to a function. The 'Exec' class instantiates 'Control.Monad.MonadPlus' in such a way that if
    -- this type parameter is thrown, it is automatically caught and allows alternative expressions
    -- to be tried.
  | ExecError
    { execUnitAtError   :: Maybe ExecUnit
    , specificErrorData :: Object
    }
instance Monoid ExecError where { mempty = ExecBadParam mempty; mappend = const; }

-- | Like 'Prelude.error' but works for the 'Exec' monad, throws an 'ExecError' using
-- 'Control.Monad.Error.throwError' constructed using the given 'Object' value as the
-- 'specificErrorData'.
execThrow :: Object -> Exec ig
execThrow obj = ask >>= \xunit ->
  throwError $ ExecError{execUnitAtError=Just xunit, specificErrorData=obj}

execFromPValue :: PValue UpdateErr a -> Exec a
execFromPValue pval = case pval of
  OK      a -> return a
  Backtrack -> mzero
  PFail err -> throwError (errConv err)
  where
    errConv err = ExecBadParam $ -- TODO: create an 'ObjectInterface' for 'UpdateErr'.
      ParamValue
      { paramValue = OList $ concat $
          [ maybe [] (return . ostr) (updateErrMsg err)
          , let addr = updateErrAddr err
            in  if null addr
                  then []
                  else [ ORef $ QualRef Unqualified $
                           foldr (\a b -> DotRef (PlainRef a) b) (PlainRef (head addr)) (tail addr)
                        ]
          , let tree = updateErrTree err in if T.null tree then [] else [OTree $ updateErrTree err]
          ]
      , paramOrigExpr = Nothing
      }

----------------------------------------------------------------------------------------------------

-- | This simple, humble little class is one of the most important in the Dao program because it
-- defines the 'execute' function. Any data type that can result in procedural execution in the
-- 'Exec' monad can instantiate this class. This will allow the instnatiated data type to be used as
-- a kind of executable code that can be passed around and evaluated at arbitrary points in your Dao
-- program.
-- 
-- Note that there the @result@ type parameter is functional dependent on the @exec@ type parameter.
-- This guarantees there is a one-to-one mapping from independent @exec@ types to dependent @result@
-- types, i.e. if you data type @MyDat@ maps to a data type @Rzlt@, then @Rzlt@ is the only possible
-- data type that could ever be evaluated by 'execute'-ing the @MyDat@ function.
--
-- As a reminder, functional dependencies do not necessitate a one-to-one mapping from the
-- dependent type to the independent type, so the @result@ parameter may be the same for many
-- different @exec@ types. But once the compiler infers that the @exec@ parameter of the 'Executable'
-- class is @MyDat@, the @result@ type /must/ be @Rzlt@ and nothing else.
-- > instance Executable MyDat Rzlt
-- > instance Executable A     () -- OK (different @exec@ parameters, same @result@ parameters)
-- > instance Executable B     () -- OK
-- > instance Executable C     () -- OK
-- > 
-- > instance Executable D     ()   -- COMPILER ERROR (same @exec@ parameters, different @result@ parameters)
-- > instance Executable D     Int  -- COMPILER ERROR
-- > instance Executable D     Char -- COMPILER ERROR
-- > -- Should D instantiate () or Int or Char as it's result? You must choose only one.
class Executable exec result | exec -> result where { execute :: exec -> Exec result }

----------------------------------------------------------------------------------------------------

data UpdateOp = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UORB | UANDB | UXORB | USHL | USHR
  deriving (Eq, Ord, Typeable, Enum, Ix, Show)

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
  deriving (Eq, Ord, Typeable, Enum, Ix, Show)

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
  deriving (Eq, Ord, Typeable, Enum, Ix, Show)

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

data LambdaExprType = FuncExprType | RuleExprType | PatExprType deriving (Eq, Ord, Typeable, Enum)
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
  | Equation      ObjectExpr      InfixOp      ObjectExpr  Location
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
  deriving (Eq, Ord, Typeable)

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
  deriving (Eq, Ord, Typeable)

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
    ""      -> []

-- | A 'TopLevelExpr' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data TopLevelExpr
  = Attribute      Name               ObjectExpr                 Location
  | TopFunc        Name               [ObjectExpr]  CodeBlock    Location
  | TopScript      ScriptExpr                                    Location
  | TopLambdaExpr  LambdaExprType     [ObjectExpr]  CodeBlock    Location
  | EventExpr      TopLevelEventType  CodeBlock                  Location
  deriving (Eq, Ord, Typeable)

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
newtype Program = Program { topLevelExprs :: [TopLevelExpr] } deriving (Eq, Ord, Typeable)
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
  deriving (Eq, Ord, Typeable, Enum, Show, Read)

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
--data Resource stor ref =
--  Resource
--  { resource        :: DMVar (stor Object, stor (DQSem, Maybe Object))
--  , updateUnlocked  :: ref -> Maybe Object -> stor Object -> stor Object
--  , lookupUnlocked  :: ref -> stor Object -> Maybe Object
--  , updateLocked    :: ref -> Maybe (DQSem, Maybe Object) -> stor (DQSem, Maybe Object) -> stor (DQSem, Maybe Object)
--  , lookupLocked    :: ref -> stor (DQSem, Maybe Object) -> Maybe (DQSem, Maybe Object)
--  } -- NOTE: this data type needs to be opaque.
--    -- Do not export the constructor or any of the accessor functions.
--
--type StackResource = Resource (Stack             Name) [Name]
--type TreeResource  = Resource (T.Tree            Name) [Name]
--type MapResource   = Resource (M.Map             Name) Name
--type DocResource   = Resource (StoredFile T.Tree Name) [Name]

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
objectError o msg = execThrow $ OList [OString (ustr msg), o]

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
--  | DocumentFile    DocResource -- ^ "*.idea" files, 'Object' data loaded from the file system.

-- | Used to select programs from the 'pathIndex' that are currently available for recursive
-- execution.
isProgramFile :: File -> [ExecUnit]
isProgramFile file = case file of
  ProgramFile p -> [p]
--  _             -> []

-- | Used to select programs from the 'pathIndex' that are currently available for recursive
-- execution.
--isDocumentFile :: File -> [DocResource]
--isDocumentFile file = case file of
--  DocumentFile d -> [d]
--  _              -> []

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

ioExec :: Exec a -> ExecUnit -> IO (FlowCtrl ExecError (Maybe Object) a)
ioExec func xunit = runReaderT (runProcedural (execToProcedural func)) xunit

newtype ExecHandler a = ExecHandler { execHandler :: ExecUnit -> Handler (FlowCtrl ExecError (Maybe Object) a) }
instance Functor ExecHandler where { fmap f (ExecHandler h) = ExecHandler (fmap (fmap (fmap f)) h) }

-- | Create an 'ExecHandler'.
newExecIOHandler :: Exception e => (e -> Exec a) -> ExecHandler a
newExecIOHandler h = ExecHandler (\xunit -> Handler (\e -> ioExec (h e) xunit))

-- | Using an 'ExecHandler' like 'execIOHandler', catch any exceptions thrown by the Haskell
-- language runtime and wrap them up in the 'Exec' monad.
execCatchIO :: Exec a -> [ExecHandler a] -> Exec a
execCatchIO tryFunc handlers = do
  xunit <- ask
  ctrl  <- liftIO $ catches (ioExec tryFunc xunit) (fmap (\h -> execHandler h xunit) handlers)
  proc ctrl

-- | Like 'execCatchIO' but with the arguments 'Prelude.flip'ped.
execHandleIO :: [ExecHandler a] -> Exec a -> Exec a
execHandleIO = flip execCatchIO

-- | An 'ExecHandler' for catching 'Control.Exception.ErrorCall's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execIOHandler :: ExecHandler ()
execIOHandler = newExecIOHandler $ \e -> execThrow (ostr $ show (e::IOException))

-- | An 'ExecHandler' for catching 'Control.Exception.ErrorCall's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execErrorHandler :: ExecHandler ()
execErrorHandler = newExecIOHandler $ \e -> execThrow (ostr $ show (e::ErrorCall))

catchReturn :: Exec a -> (Maybe Object -> Exec a) -> Exec a
catchReturn fn catch = procCatch fn >>= \ce -> case ce of
  FlowReturn obj -> catch obj
  FlowOK     a   -> proc (FlowOK a)
  FlowErr    obj -> proc (FlowErr obj)

----------------------------------------------------------------------------------------------------

type DepGraph = M.Map UPath [UPath]

getDepFiles :: DepGraph -> [UPath]
getDepFiles = M.keys

