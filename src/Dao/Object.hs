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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Object
  ( module Dao.String
  , module Dao.Object
  ) where

import           Dao.Debug.OFF
import           Dao.Runtime
import           Dao.String
import           Dao.Token
import           Dao.Parser
import           Dao.Glob
import qualified Dao.EnumSet   as Es
import qualified Dao.Tree      as T
import qualified Dao.Binary    as D
import           Dao.Stack
import           Dao.Struct
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
import qualified Data.Binary.Get           as B
import qualified Data.Binary.Put           as B

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Concurrent

import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Error.Class

----------------------------------------------------------------------------------------------------

-- | The 'Object' type extends the 'Data.Dynamic.Dynamic' data type with a few more constructors for
-- data types that are fundamental to a programming language, like integers, strings, and lists.
data Object
  = ONull
  | OTrue
  | OType      T_type
  | OInt       T_int
  | OWord      T_word
  | OLong      T_long
  | OFloat     T_float
  | ORatio     T_ratio
  | OComplex   T_complex
  | OAbsTime   T_time
  | ORelTime   T_diffTime
  | OChar      T_char
  | OString    T_string
  | ORef       T_ref
  | OList      T_list
  | OTree      T_tree
  | OBytes     T_bytes
  | OHaskell   Dynamic T_haskell
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
    (OAbsTime      a, OAbsTime      b) -> a==b
    (ORelTime  a, ORelTime  b) -> a==b
    (OChar      a, OChar      b) -> a==b
    (OString    a, OString    b) -> a==b
    (ORef       a, ORef       b) -> a==b
    (OList      a, OList      b) -> a==b
    (OTree      a, OTree      b) -> a==b
    (OBytes     a, OBytes     b) -> a==b
    (OHaskell a ifcA, OHaskell b ifcB) -> ((ifcA==ifcB)&&) $ maybe False id $
      objEquality ifcA >>= \eq -> return (eq a b)
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
    (OAbsTime   a, OAbsTime   b) -> compare a b
    (ORelTime   a, ORelTime   b) -> compare a b
    (OChar      a, OChar      b) -> compare a b
    (OString    a, OString    b) -> compare a b
    (ORef       a, ORef       b) -> compare a b
    (OList      a, OList      b) -> compare a b
    (OTree      a, OTree      b) -> compare a b
    (OBytes     a, OBytes     b) -> compare a b
    (OHaskell a ifcA, OHaskell b ifcB) -> maybe (err a b) id $ do
        guard (ifcA==ifcB)
        objOrdering ifcA >>= \comp -> return (comp a b)
      where
        err a b = error $ unwords $
          [ "cannot compare object of type", show (objHaskellType ifcA)
          , "with obejct of type", show (objHaskellType ifcB)
          ]
    _                            -> compare (objType a) (objType b)

instance Show Object where
  show o = case o of
    ONull      -> "ONull"
    OTrue      -> "OTrue"
    OType    o -> "OType "++show o
    OInt     o -> "OInt "++show o
    OWord    o -> "OWord "++show o
    OLong    o -> "OLong "++show o
    OFloat   o -> "OFloat "++show o
    ORatio   o -> "ORatio "++show o
    OComplex o -> "OComplex "++show o
    OString  o -> "OString "++show o
    OAbsTime o -> "OAbsTime "++show o
    ORelTime o -> "ORelTime "++show o
    OChar    o -> "OChar "++show o
    OList    o -> "OList "++show o
    OTree    o -> "OTree "++show o
    OBytes   o -> "OBytes "++unwords (b64Encode o)
    OHaskell _ ifc -> "OHaskell "++show (objHaskellType ifc)

instance Structured Object Object where
  dataToStruct = deconstruct . place
  structToData = reconstruct this

putUStrData :: UStrType str => str -> Update ()
putUStrData s =
  let u = toUStr s in place (if ulength u == 1 then OChar (head (uchars u)) else OString u)

getUStrData :: UStrType str => str -> Update UStr
getUStrData msg = do
  a <- this
  case a of
    OString a -> return a
    OChar   c -> return (ustr [c])
    _         -> fail ("was expecting a string for constructing a "++uchars msg++" object")

getIntegerData :: Integral a => String -> Update a
getIntegerData msg = do
  a <- this
  case a of
--  OLong a -> return (fromIntegral a)
    OInt  a -> return (fromIntegral a)
--  OWord a -> return (fromIntegral a)
    _ -> fail ("was expecting an integer value for constructing a "++msg++" object")

getBoolData :: String -> String -> String -> Update Bool
getBoolData msg tru fals = do
  a <- this
  case a of
    ONull -> return False
    OTrue -> return True
    OString str
      | uchars str == tru  -> return True
      | uchars str == fals -> return False
      | uchars str == "true" -> return True
      | uchars str == "false" -> return True
      | uchars str == "yes" -> return True
      | uchars str == "no" -> return True
    OInt i -> return (i/=0)
--  OWord i -> return (i/=0)
--  OLong i -> return (i/=0)
    _ -> fail $ concat $
      [ "was expecting a boolean value ("
      , show tru, " or ", fals
      , ") for constructing ", msg, " object"
      ]

instance Structured () Object where
  dataToStruct _ = deconstruct $ place ONull
  structToData = reconstruct $ this >>= \o -> case o of
    ONull -> return ()
    o     -> fail $ "expecting () as ONull value, instead got "++show (objType o)

instance Structured UStr Object where
  dataToStruct a = deconstruct $ place (OString a)
  structToData = reconstruct $ do
    a <- this
    case a of
      OString a -> return a
      _         -> fail "expecing string constant"

instance Structured Bool Object where
  dataToStruct a = deconstruct $ place (if a then OTrue else ONull)
  structToData = reconstruct $ getBoolData "strucutred boolean" "true" "false"

--instance Structured Word64 where
--  dataToStruct a = deconstruct $ place (OWord a)
----  structToData = reconstruct (fmap fromIntegral (getIntegerData "unsigned integer"))

instance Structured Word Object where
  dataToStruct a = deconstruct $ place (OInt (fromIntegral a))
  structToData = reconstruct (fmap fromIntegral (getIntegerData "unsigned integer"))

--instance Structured Int64 where
--  dataToStruct a = deconstruct $ place (OInt a)
--  structToData = reconstruct (fmap fromIntegral (getIntegerData "integer"))

instance Structured Int Object where
  dataToStruct a = deconstruct $ place (OInt (fromIntegral a))
  structToData = reconstruct (fmap fromIntegral (getIntegerData "integer"))

--instance Structured Integer where
--  dataToStruct a = deconstruct $ place (OLong a)
--  structToData = reconstruct (fmap toInteger (getIntegerData "long-integer"))

newtype StructChar = StructChar Char
instance Structured StructChar Object where
  dataToStruct (StructChar c) = deconstruct (place (OChar c))
  structToData = reconstruct $ this >>= \c -> case c of
    OChar c -> return (StructChar c)
    _       -> fail "singleton character"

--instance Structured (Ratio Integer) where
--  dataToStruct a = deconstruct (place (OPair (OLong (numerator a), OLong (denominator a))))
--  structToData = reconstruct $ this >>= \a -> case a of
--    OPair (a, b) -> assumePValue $
--      objToIntegral a >>= \a -> objToIntegral b >>= \b -> return (a % b)

putListWith :: (a -> Update ()) -> [a] -> Update ()
putListWith dat2srct ox = place (OList (fmap (OTree . deconstruct . dat2srct) ox))

getListWith :: Update a -> Update [a]
getListWith srct2dat = do
  o <- this
  case o of
    OList ox -> forM ox $ \o -> case o of
      OTree o -> assumePValue (reconstruct srct2dat o)
      _       -> fail "was expecting structured data in each list item"
    _        -> fail "was expecting a list object"

instance Structured a Object => Structured [a] Object where
  dataToStruct ax = deconstruct (putListWith putData ax)
  structToData    = reconstruct (getListWith getData)

instance Structured (T.Tree Name Object) Object where
  dataToStruct a = deconstruct $ place (OTree a)
  structToData = reconstruct $ do
    o <- this
    case o of
      OTree o -> return o
      _       -> fail $
        "was expecting an 'OTree' object containing structured data to construct "

----------------------------------------------------------------------------------------------------

-- $Object_types
-- Here we have a lambda calculus for describing types. Computationally, it is very similar to the
-- Prolog programming language, however an 'ObjType' is written using a subset the Dao scripting
-- langauge.

showEncoded :: [Word8] -> String
showEncoded encoded = seq encoded (concatMap (\b -> showHex b " ") encoded)

type T_type     = ObjType
type T_int      = Int
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
type T_list     = [Object]
type T_tree     = T.Tree Name Object
type T_bytes    = B.ByteString
type T_haskell  = ObjectInterface Dynamic

data CoreType
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
  | RefType
  | ListType
  | TreeType
  | BytesType
  | HaskellType
  deriving (Eq, Ord, Typeable, Enum, Bounded)

instance Es.InfBound CoreType where
  minBoundInf = Es.Point minBound
  maxBoundInf = Es.Point maxBound

instance Show CoreType where
  show t = case t of
    NullType     -> "null"
    TrueType     -> "true"
    TypeType     -> "type"
    IntType      -> "int"
    DiffTimeType -> "diff"
    TimeType     -> "time"
    CharType     -> "char"
    StringType   -> "string"
    RefType      -> "ref"
    ListType     -> "list"
    TreeType     -> "tree"
    BytesType    -> "bytes"
    HaskellType  -> "haskell"

instance Read CoreType where
  readsPrec _ str = map (\a -> (a, "")) $ case str of
    "null"    -> [NullType]
    "true"    -> [TrueType]
    "type"    -> [TypeType]
    "int"     -> [IntType]
    "diff"    -> [DiffTimeType]
    "time"    -> [TimeType]
    "char"    -> [CharType]
    "string"  -> [StringType]
    "ref"     -> [RefType]
    "list"    -> [ListType]
    "tree"    -> [TreeType]
    "bytes"   -> [BytesType]
    "haskell" -> [HaskellType]
    _         -> []

instance UStrType CoreType where
  toUStr = derive_ustr
  maybeFromUStr a = case readsPrec 0 (uchars a) of
    [(o, "")] -> Just o
    _         -> Nothing
  fromUStr a = case maybeFromUStr a of
    Nothing -> error (show a++" is not a valid type identifier")
    Just  a -> a
oBool :: Bool -> Object
oBool a = if a then OTrue else ONull

----------------------------------------------------------------------------------------------------

-- | A 'Reference' is basically an address that can be used to lookup and update various objects in
-- the runtime.
newtype Reference = Reference { refNameList :: [Name] } deriving (Eq, Ord, Typeable, Show)
instance Monoid Reference where
  mempty = Reference []
  mappend (Reference a) (Reference b) = Reference (a++b)

instance HasNullValue Reference where
  nullValue = mempty
  testNull (Reference a) = null a

instance Read Reference where
  readsPrec _ str = case str of
    c:cx | isAlpha c ->
      case break (\c -> c=='.' || isAlphaNum c) str of
        (cx, str) ->
          maybe [] (return . (\ref -> (Reference ref, str))) $ sequence $
            fix (\loop str -> case break (=='.') str of
                    (cx, str) -> case cx of
                      [] -> []
                      cx -> maybeFromUStr (ustr (dropWhile (=='.') cx)) : loop str
                ) cx
    _ -> mzero

instance UStrType Reference where
  toUStr (Reference nx) = ustr $ intercalate "." $ map uchars nx
  maybeFromUStr str = case readsPrec 0 (uchars str) of
    [(ref, "")] -> Just ref
    _           -> Nothing
  nil = mempty

-- | Direct a reference at a particular tree in the runtime.
data RefQualifier
  = LOCAL -- ^ the default unless in a "with" statement, refers to the current local variable stack
  | QTIME -- ^ refers to a global variable stack that is alive only during a query, and is cleared
          -- when the query completes.
  | GLODOT -- ^ a relative reference, gets it's name because it begins with a dot (".") character.
           -- Similar to the "this" keyword in C++ and Java, refers to the object of the current
           -- context set by the "with" statement, but defaults to the global variable space when
           -- not within a "with" statement. This is necessary to differentiate between local
           -- variables and references to the "with" context.
  | STATIC -- ^ a local variable stack specific to a 'Subroutine' that lives on even after the
           -- subroutine has completed.
  | GLOBAL -- ^ the global variable space for the current module.
  deriving (Eq, Ord, Typeable, Enum, Ix, Show)
instance Bounded RefQualifier where { minBound=LOCAL; maxBound=GLOBAL; }

-- | A 'Reference' qualified with a 'RefQualifier'. This allows references to be directed at
-- different trees of the runtime.
data QualRef
  = Unqualified Reference
  | Qualified RefQualifier Reference
  | ObjRef    Object
  deriving (Eq, Ord, Typeable, Show)
instance HasNullValue QualRef where
  nullValue = Unqualified nullValue
  testNull (Unqualified a) = testNull a
fmapQualRef :: (Reference -> Reference) -> QualRef -> QualRef
fmapQualRef fn r = case r of
  Unqualified r -> Unqualified (fn r)
  Qualified q r -> Qualified q (fn r)
setQualifier :: RefQualifier -> QualRef -> QualRef
setQualifier q ref = case ref of
  Unqualified ref -> Qualified q ref
  Qualified _ ref -> Qualified q ref
delQualifier :: QualRef -> QualRef
delQualifier ref = case ref of
  Unqualified r -> Unqualified r
  Qualified _ r -> Unqualified r

-- | Since 'Object' requires all of it's types instantiate 'Prelude.Ord', I have defined
-- 'Prelude.Ord' of 'Data.Complex.Complex' numbers to be the distance from 0, that is, the radius of
-- the polar form of the 'Data.Complex.Complex' number, ignoring the angle argument.
instance RealFloat a => Ord (Complex a) where
  compare a b = compare (magnitude a) (magnitude b)

ostr :: UStrType u => u -> Object
ostr = OString . toUStr

----------------------------------------------------------------------------------------------------

objType :: Object -> CoreType
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
  OAbsTime     _ -> TimeType
  ORelTime _ -> DiffTimeType
  OChar     _ -> CharType
  OString   _ -> StringType
  ORef      _ -> RefType
  OList     _ -> ListType
  OTree     _ -> TreeType
  OBytes    _ -> BytesType
  OHaskell _ _ -> HaskellType

-- | A symbol in the type calculus.
data TypeSym
  = CoreType CoreType
    -- ^ used when the type of an object is equal to it's value, for example Null and True,
    -- or in situations where the type of an object has a value, for example the dimentions of a
    -- matrix.
  | TypeVar  Reference [ObjType]
    -- ^ a polymorphic type, like 'AnyType' but has a name.
  deriving (Eq, Ord, Show, Typeable)

-- | Complex type structures can be programmed by combining 'ObjSimpleType's.
newtype TypeStruct = TypeStruct [TypeSym] deriving (Eq, Ord, Show, Typeable)

-- | The fundamental 'Type' used to reason about whether an object is fit to be used for a
-- particular function.
newtype ObjType = ObjType { typeChoices :: [TypeStruct] } deriving (Eq, Ord, Show, Typeable)

instance HasNullValue TypeStruct where { nullValue = TypeStruct []; testNull (TypeStruct a) = null a; }
instance HasNullValue ObjType    where { nullValue = ObjType []; testNull (ObjType a) = null a; }

----------------------------------------------------------------------------------------------------

instance HasNullValue ()   where { nullValue = (); testNull () = True; }
instance HasNullValue UStr where { nullValue = mempty; testNull = (==mempty); }
instance HasNullValue [a]  where { nullValue = []; testNull = null; }
instance HasNullValue Char where { nullValue = '\0'; testNull = (==nullValue); }
instance HasNullValue Int  where { nullValue = 0; testNull = (==nullValue); }
instance HasNullValue Int64  where { nullValue = 0; testNull = (==nullValue); }
instance HasNullValue Word   where { nullValue = 0; testNull = (==nullValue); }
instance HasNullValue Word64 where { nullValue = 0; testNull = (==nullValue); }
instance HasNullValue Double where { nullValue = 0; testNull = (==nullValue); }
instance HasNullValue Integer where { nullValue = 0; testNull = (==nullValue); }
instance HasNullValue (Ratio Integer) where { nullValue = 0%1; testNull = (==nullValue); }
instance HasNullValue (Complex Double) where { nullValue = 0:+0; testNull = (==nullValue); }
instance HasNullValue NominalDiffTime where { nullValue = fromRational 0; testNull = (==nullValue); }
instance HasNullValue (IM.IntMap a)   where { nullValue = IM.empty; testNull = IM.null }
instance HasNullValue (M.Map k a)     where { nullValue = M.empty; testNull = M.null }
instance HasNullValue (S.Set a)       where { nullValue = S.empty; testNull = S.null }
instance HasNullValue B.ByteString    where { nullValue = mempty; testNull = (==mempty); }
instance HasNullValue (T.Tree Name Object) where { nullValue = T.Void; testNull = T.null; }
instance HasNullValue Object where
  nullValue = ONull
  testNull a = case a of
    ONull        -> True
    OInt      i  -> testNull i
    OWord     i  -> testNull i
    OLong     i  -> testNull i
    ORatio    r  -> testNull r
    OComplex  c  -> testNull c
    OString   s  -> testNull s
    OChar     c  -> testNull c
    ORelTime  s  -> testNull s
    OList     s  -> testNull s
    OTree     t  -> testNull t
    OBytes    o  -> testNull o
    OHaskell  o ifc -> case objNullTest ifc of
      Nothing -> error ("to check whether objects of type "++show (objHaskellType ifc)++" are null is undefined behavior")
      Just fn -> fn o
    _            -> False

-- | Create the minimum-sized array that can store all of the indices in the given list, setting the
-- 'Data.Array.IArray.bounds' of the array automatically. Evaluates to 'Prelude.Nothing' if the
-- given list of elements is empty.
minAccumArray :: Ix i => (e -> e' -> e) -> e -> [(i, e')] -> Maybe (Array i e)
minAccumArray accfn deflt elems =
  if null elems then Nothing else Just (accumArray accfn deflt bnds elems) where
    idxs = map fst elems
    i0   = head idxs
    bnds = foldl (\ (lo, hi) i -> (min lo i, max hi i)) (i0, i0) (tail idxs)

-- | Create the minimum-sized array that can store all of the indices in the given list, and setting
-- the 'Data.Array.IArray.bounds' of the array automatically. Evaluates to 'Prelude.Nothing' if the
-- given list of elements is empty.
minArray :: Ix i => e -> [(i, e)] -> Maybe (Array i e)
minArray deflt elems = minAccumArray (flip const) deflt elems

----------------------------------------------------------------------------------------------------

newtype CodeBlock = CodeBlock { codeBlock :: [ScriptExpr] } deriving (Eq, Ord, Typeable)
instance Show CodeBlock where { show (CodeBlock o) = "(code "++show o++")" }
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
instance HasNullValue CodeBlock where
  nullValue = mempty
  testNull (CodeBlock []) = True
  testNull _ = False

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
instance Show Subroutine where { show o = "Subroutine "++show (codeBlock (origSourceCode o)) }
instance HasNullValue Subroutine where
  nullValue =
    Subroutine{origSourceCode=nullValue, staticVars=error "null Subroutine", executable=return Nothing}
  testNull (Subroutine a _ _) = testNull a

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion with Haskell's "Data.Function"). 
data CallableCode
  = CallableCode
    { argsPattern    :: ParamListExpr
    , returnType     :: ObjType
    , codeSubroutine :: Subroutine
    }
  deriving (Show, Typeable)

-- | Interface used during evaluation of Dao scripts to determine whether or not an if or while
-- statement should continue. Also, turns out to be handy for plenty of other reasons.
instance HasNullValue CallableCode where
  nullValue =
    CallableCode{argsPattern=nullValue, returnType=nullValue, codeSubroutine=nullValue}
  testNull (CallableCode a b c) = testNull a && testNull b && testNull c

-- A subroutine that is executed when a query string matches it's @['Dao.Glob.Glob']@ expression.
data GlobAction
  = GlobAction
    { globPattern    :: [Glob]
    , globSubroutine :: Subroutine
    }
  deriving (Show, Typeable)

instance HasNullValue GlobAction where
  nullValue = GlobAction{globPattern=[], globSubroutine=nullValue}
  testNull (GlobAction a b) = null a && testNull b

-- | When calling Dao program functions, arguments to functions are wrapped in this data type. This
-- data type exists mostly to allow for it to be instantiated into the 'Executable' class.
-- Evaluating this data type with 'execute' will simply return the 'paramValue' unless the
-- 'paramValue' is constructed with 'ORef', in which case the 'QualRef' in used to retrieve an
-- object value associated with that 'QualRef'.
data ParamValue = ParamValue{paramValue::Object, paramOrigExpr::ObjectExpr}

-- | This data type is essentially a "thunk" used to store a 'DaoFunc' and a list of 'ParamValue's.
-- It is primarily used to instantiate the 'Executable' class. When evaluating this data type with
-- 'execute' the 'DaoFunc' is executed by passing it the list of 'ParamValues'.
data ExecDaoFunc = MkExecDaoFunc Name [ParamValue] DaoFunc

----------------------------------------------------------------------------------------------------

data RefExpr = RefExpr Reference Location deriving (Eq, Ord, Typeable, Show)

instance HasNullValue RefExpr where
  nullValue = RefExpr nullValue LocationUnknown
  testNull (RefExpr a _) = testNull a

instance Read RefExpr where
  readsPrec prec = readsPrec prec >=> (\ (ref, str) -> return (RefExpr ref LocationUnknown, str))

instance UStrType RefExpr where
  toUStr (RefExpr ref _) = toUStr ref
  maybeFromUStr str = flip RefExpr LocationUnknown <$> maybeFromUStr str
  nil = RefExpr nil LocationUnknown

refFromExpr :: RefExpr -> Reference
refFromExpr (RefExpr ref _) = ref

data QualRefExpr
  = UnqualRefExpr            RefExpr
  | QualRefExpr RefQualifier RefExpr Location
  deriving (Eq, Ord, Typeable, Show)

refNames :: UStrType str => [str] -> QualRef
refNames nx = Unqualified $ Reference $ fmap (fromUStr . toUStr) nx

maybeRefNames :: UStrType str => [str] -> Maybe QualRef
maybeRefNames nx = fmap (Unqualified . Reference) $ sequence $ fmap (maybeFromUStr . toUStr) nx

-- | Create a 'QualRefExpr' from any single 'Dao.String.UStr'.
bareword :: UStrType str => str -> QualRefExpr
bareword = UnqualRefExpr . flip RefExpr LocationUnknown . Reference . return . fromUStr . toUStr

instance HasNullValue QualRefExpr where
  nullValue = UnqualRefExpr nullValue
  testNull (UnqualRefExpr a) = testNull a

instance HasLocation RefExpr where
  getLocation (RefExpr _ loc)     = loc
  setLocation (RefExpr o _  ) loc = RefExpr o loc
  delLocation (RefExpr o _  )     = RefExpr o LocationUnknown

instance HasLocation QualRefExpr where
  getLocation o     = case o of
    UnqualRefExpr r     -> getLocation r
    QualRefExpr _ _ loc -> loc
  setLocation o loc = case o of
    UnqualRefExpr r     -> UnqualRefExpr (setLocation r loc)
    QualRefExpr q r _   -> QualRefExpr q r loc
  delLocation o     = case o of
    UnqualRefExpr r     -> UnqualRefExpr (delLocation r)
    QualRefExpr q r _   -> QualRefExpr q r LocationUnknown

instance UStrType RefQualifier where
  toUStr a = ustr $ case a of
    LOCAL  -> "local"
    QTIME  -> "qtime"
    STATIC -> "static"
    GLOBAL -> "global"
    GLODOT -> "."
  maybeFromUStr str = case uchars str of
    "local"  -> Just LOCAL
    "qtime"  -> Just QTIME
    "static" -> Just STATIC
    "global" -> Just GLOBAL
    "."      -> Just GLODOT
    _        -> Nothing
  fromUStr str = maybe (error (show str++" is not a reference qualifier")) id (maybeFromUStr str)

----------------------------------------------------------------------------------------------------

allInfixOpChars = "+-*/%<>^&|."
allInfixOpStrs = " + - * / % ** -> . || && == != | & ^ << >> < > <= >= . -> <- "

-- | Contains a list of 'ObjectExpr's, which are used to encode parameters to function calls, and
-- intialization lists.
data ObjListExpr = ObjListExpr [ObjectExpr] Location deriving (Eq, Ord, Typeable)
instance Show ObjListExpr where { show (ObjListExpr o loc) = show o++show loc }
instance HasLocation ObjListExpr where
  getLocation (ObjListExpr _ loc)     = loc
  setLocation (ObjListExpr a _  ) loc = ObjListExpr (fmap delLocation a) loc
  delLocation (ObjListExpr a _  )     = ObjListExpr (fmap delLocation a) LocationUnknown
instance Monoid ObjListExpr where
  mempty = ObjListExpr [] LocationUnknown
  mappend (ObjListExpr a locA) (ObjListExpr b locB) = ObjListExpr (a++b) (locA<>locB)
instance HasNullValue ObjListExpr where
  nullValue = mempty
  testNull (ObjListExpr a _) = null a

data UpdateOp
  = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UPOW | UORB | UANDB | UXORB | USHL | USHR | UARROW
  deriving (Eq, Ord, Typeable, Enum, Ix, Show)

instance Bounded UpdateOp where {minBound = UCONST; maxBound = UARROW}

allUpdateOpStrs = " = += -= *= /= %= **= |= &= ^= <<= >>= <- "

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
    UARROW -> "<-"
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
    "<-"  -> Just UARROW
    _     -> Nothing
  fromUStr str =
    maybe (error (show str++" is not an assignment/update operator")) id (maybeFromUStr str)

-- | Unary operators.
data PrefixOp = INVB | NOT | NEGTIV | POSTIV | REF | DEREF
  deriving (Eq, Ord, Typeable, Enum, Ix, Show)
instance Bounded  PrefixOp where { minBound=INVB; maxBound=DEREF; }
instance UStrType PrefixOp where
  toUStr op = ustr $ case op of
    INVB   -> "~"
    NOT    -> "!"
    NEGTIV -> "-"
    POSTIV -> "+"
    REF    -> "$"
    DEREF  -> "@"
  maybeFromUStr str = case uchars str of
    "~" -> Just INVB
    "!" -> Just NOT
    "-" -> Just NEGTIV
    "+" -> Just POSTIV
    "$" -> Just REF
    "@" -> Just DEREF
    str -> Nothing
  fromUStr str = maybe (error (show str++" is not a prefix opretor")) id (maybeFromUStr str)

allPrefixOpChars = "$@~!-+"
allPrefixOpStrs = " $ @ ~ - + ! "

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
  deriving (Eq, Ord, Typeable, Enum, Ix, Show)
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
    ; "<=" -> Just GTEQ ; ">=" -> Just GTEQ 
    ; _    -> Nothing
    }
  fromUStr str = maybe (error (show str++" is not an infix operator")) id (maybeFromUStr str)
instance Bounded InfixOp where { minBound = ADD; maxBound = LTEQ; }

-- | Functions and function parameters can specify optional type-checking expressions.
data TyChkExpr a
  = NotTypeChecked{tyChkItem::a}
    -- ^ no type information was specified for this item
  | TypeChecked   {tyChkItem::a, tyChkExpr::ObjectExpr, tyChkLoc::Location}
    -- ^ type check information was specified and should be checked every time it is evaluated.
  | DisableCheck  {tyChkItem::a, tyChkExpr::ObjectExpr, typChkResult::Object, tyChkLoc::Location}
    -- ^ type check information was specified but has been disabled for efficiency reasons because
    -- we have verified that the item will always return a succesfull type-check.
  deriving (Eq, Ord, Typeable, Show)

instance HasNullValue a => HasNullValue (TyChkExpr a) where
  nullValue = NotTypeChecked nullValue
  testNull (NotTypeChecked a) = testNull a
  testNull _ = False

instance Functor TyChkExpr where
  fmap f (NotTypeChecked   a  ) = NotTypeChecked (f a)
  fmap f (TypeChecked  a b c  ) = TypeChecked  (f a) b c
  fmap f (DisableCheck a b c d) = DisableCheck (f a) b c d

checkedExpr :: TyChkExpr a -> a
checkedExpr o = case o of { NotTypeChecked o -> o; TypeChecked o _ _ -> o; }

instance HasLocation a => HasLocation (TyChkExpr a) where
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

newtype LValueExpr = LValueExpr ObjectExpr deriving (Eq, Ord, Typeable, Show)
instance HasLocation LValueExpr where
  getLocation (LValueExpr o)     = getLocation o
  setLocation (LValueExpr o) loc = LValueExpr (setLocation o loc)
  delLocation (LValueExpr o)     = LValueExpr (delLocation o)
instance HasNullValue LValueExpr where
  nullValue = LValueExpr nullValue
  testNull (LValueExpr a) = testNull a

-- | 'ParamExpr' is a part of the Dao language semantics, and is also used in the the 'CallableCode'
-- data type when evaluating parameters to be passed to the callable code function execution. The
-- boolean parameter here indicates whether or not the parameter should be passed by reference.
data ParamExpr = ParamExpr Bool (TyChkExpr Name) Location deriving (Eq, Ord, Typeable, Show)
instance HasLocation ParamExpr where
  getLocation (ParamExpr _ _ loc)     = loc
  setLocation (ParamExpr a b _  ) loc = ParamExpr a b loc
  delLocation (ParamExpr a b _  )     = ParamExpr a b LocationUnknown

data ParamListExpr = ParamListExpr (TyChkExpr [ParamExpr]) Location
  deriving (Eq, Ord, Typeable, Show)

instance HasNullValue ParamListExpr where
  nullValue = ParamListExpr (NotTypeChecked []) LocationUnknown
  testNull (ParamListExpr (NotTypeChecked []) _) = True
  testNull _ = False

getTypeCheckList :: ParamListExpr -> [ParamExpr]
getTypeCheckList (ParamListExpr tychk _) = tyChkItem tychk 

instance HasLocation ParamListExpr where
  getLocation (ParamListExpr _ loc)     = loc
  setLocation (ParamListExpr a _  ) loc = ParamListExpr a loc
  delLocation (ParamListExpr a _  )     = ParamListExpr a LocationUnknown

data RuleStrings = RuleStrings [UStr] Location deriving (Eq, Ord, Typeable, Show)

instance HasNullValue RuleStrings where
  nullValue = RuleStrings [] LocationUnknown
  testNull (RuleStrings a _) = null a

instance HasLocation RuleStrings where
  getLocation (RuleStrings _ o)     = o
  setLocation (RuleStrings a _) loc = RuleStrings a loc
  delLocation (RuleStrings a _)     = RuleStrings a LocationUnknown

newtype OptObjListExpr = OptObjListExpr (Maybe ObjListExpr) deriving (Eq, Ord, Typeable, Show)
instance HasLocation OptObjListExpr where
  getLocation (OptObjListExpr o)     = maybe LocationUnknown getLocation o
  setLocation (OptObjListExpr o) loc = OptObjListExpr (setLocation o loc)
  delLocation (OptObjListExpr o)     = OptObjListExpr (delLocation o    )

instance HasNullValue OptObjListExpr where
  nullValue = OptObjListExpr Nothing
  testNull (OptObjListExpr Nothing) = True
  testNull _ = False

-- | Required parenthesese.
data ParenExpr = ParenExpr ObjectExpr Location deriving (Eq, Ord, Typeable, Show)
instance HasLocation ParenExpr where
  getLocation (ParenExpr _ loc)     = loc
  setLocation (ParenExpr o _  ) loc = ParenExpr o loc
  delLocation (ParenExpr o _  )     = ParenExpr (delLocation o) LocationUnknown
instance HasNullValue ParenExpr where
  nullValue = ParenExpr nullValue LocationUnknown
  testNull (ParenExpr a _) = testNull a

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data ObjectExpr
  = VoidExpr
  | ObjQualRefExpr QualRefExpr
  | ObjParenExpr   ParenExpr
  | Literal        Object                                    Location
  | AssignExpr     LValueExpr   UpdateOp        ObjectExpr   Location
  | Equation       ObjectExpr   InfixOp         ObjectExpr   Location
  | PrefixExpr     PrefixOp     ObjectExpr                   Location
  | ArraySubExpr   ObjectExpr   ObjListExpr                  Location
  | FuncCall       ObjectExpr   ObjListExpr                  Location
  | InitExpr       RefExpr      OptObjListExpr  ObjListExpr  Location
  | StructExpr     ObjectExpr   ObjListExpr                  Location
  | LambdaExpr                  ParamListExpr   CodeBlock    Location
  | FuncExpr       Name         ParamListExpr   CodeBlock    Location
  | RuleExpr       RuleStrings                  CodeBlock    Location
  | MetaEvalExpr                                CodeBlock    Location
  deriving (Eq, Ord, Typeable, Show)

instance HasNullValue ObjectExpr where
  nullValue = VoidExpr
  testNull VoidExpr = True
  testNull _ = False

instance HasLocation ObjectExpr where
  getLocation o = case o of
    VoidExpr               -> LocationUnknown
    ObjQualRefExpr       o -> getLocation o
    ObjParenExpr         o -> getLocation o
    Literal        _     o -> o
    AssignExpr     _ _ _ o -> o
    Equation       _ _ _ o -> o
    PrefixExpr     _ _   o -> o
    ArraySubExpr   _ _   o -> o
    FuncCall       _ _   o -> o
    InitExpr       _ _ _ o -> o
    StructExpr     _ _   o -> o
    LambdaExpr     _ _   o -> o
    FuncExpr       _ _ _ o -> o
    RuleExpr       _ _   o -> o
    MetaEvalExpr   _     o -> o
  setLocation o loc = case o of
    VoidExpr               -> VoidExpr
    ObjQualRefExpr a       -> ObjQualRefExpr (setLocation a loc)
    ObjParenExpr   a       -> ObjParenExpr   (setLocation a loc)
    Literal        a     _ -> Literal       a     loc
    AssignExpr     a b c _ -> AssignExpr    a b c loc
    Equation       a b c _ -> Equation      a b c loc
    PrefixExpr     a b   _ -> PrefixExpr    a b   loc
    ArraySubExpr   a b   _ -> ArraySubExpr  a b   loc
    FuncCall       a b   _ -> FuncCall      a b   loc
    InitExpr       a b c _ -> InitExpr      a b c loc
    StructExpr     a b   _ -> StructExpr    a b   loc
    LambdaExpr     a b   _ -> LambdaExpr    a b   loc
    FuncExpr       a b c _ -> FuncExpr      a b c loc
    RuleExpr       a b   _ -> RuleExpr      a b   loc
    MetaEvalExpr   a     _ -> MetaEvalExpr  a     loc
  delLocation o = case o of
    VoidExpr               -> VoidExpr
    ObjQualRefExpr a       -> ObjQualRefExpr (delLocation a)
    ObjParenExpr   a       -> ObjParenExpr   (delLocation a)
    Literal        a     _ -> Literal           a                  lu
    AssignExpr     a b c _ -> AssignExpr   (fd0 a)      b  (fd0 c) lu
    Equation       a b c _ -> Equation     (fd0 a)      b  (fd0 c) lu
    PrefixExpr     a b   _ -> PrefixExpr        a  (fd0 b)         lu
    ArraySubExpr   a b   _ -> ArraySubExpr (fd0 a) (fd0 b)         lu
    FuncCall       a b   _ -> FuncCall     (fd0 a) (fd0 b)         lu
    InitExpr       a b c _ -> InitExpr     (fd0 a) (fd0 b) (fd0 c) lu
    StructExpr     a b   _ -> StructExpr   (fd0 a) (fd0 b)         lu
    LambdaExpr     a b   _ -> LambdaExpr   (fd0 a) (fd0 b)         lu
    FuncExpr       a b c _ -> FuncExpr          a  (fd0 b) (fd0 c) lu
    RuleExpr       a b   _ -> RuleExpr          a  (fd0 b)         lu
    MetaEvalExpr   a     _ -> MetaEvalExpr (fd0 a)                 lu
    where
      lu = LocationUnknown
      fd0 :: HasLocation a => a -> a
      fd0 = delLocation
      fd1 :: (HasLocation a, Functor f) => f a -> f a
      fd1 = fmap delLocation

data IfExpr       = IfExpr ParenExpr CodeBlock Location deriving (Eq, Ord, Typeable, Show)
data ElseExpr     = ElseExpr   IfExpr Location deriving (Eq, Ord, Typeable, Show)
data IfElseExpr   = IfElseExpr IfExpr [ElseExpr] (Maybe CodeBlock) Location deriving (Eq, Ord, Typeable, Show)
newtype WhileExpr = WhileExpr  IfExpr deriving (Eq, Ord, Typeable, Show)
instance HasLocation IfExpr where
  getLocation (IfExpr _ _ loc)     = loc
  setLocation (IfExpr a b _  ) loc = IfExpr a b loc
  delLocation (IfExpr a b _  )     = IfExpr (delLocation a) (delLocation b) LocationUnknown
instance HasLocation ElseExpr where
  getLocation (ElseExpr _ loc)     = loc
  setLocation (ElseExpr a _  ) loc = ElseExpr a loc
  delLocation (ElseExpr a _  )     = ElseExpr (delLocation a) LocationUnknown
instance HasLocation IfElseExpr where
  getLocation (IfElseExpr _ _ _ loc)     = loc
  setLocation (IfElseExpr a b c _  ) loc = IfElseExpr a b c loc
  delLocation (IfElseExpr a b c _  )     = IfElseExpr a (fmap delLocation b) c LocationUnknown
instance HasLocation WhileExpr where
  getLocation (WhileExpr a)     = getLocation a
  setLocation (WhileExpr a) loc = WhileExpr (setLocation a loc)
  delLocation (WhileExpr a)     = WhileExpr (delLocation a)

instance HasNullValue IfExpr where
  nullValue = IfExpr nullValue nullValue LocationUnknown
  testNull (IfExpr a b _) = testNull a && testNull b
instance HasNullValue ElseExpr where
  nullValue = ElseExpr nullValue LocationUnknown
  testNull (ElseExpr a _) = testNull a
instance HasNullValue IfElseExpr where
  nullValue = IfElseExpr nullValue [] Nothing LocationUnknown
  testNull (IfElseExpr a [] Nothing _) = testNull a
  testNull _ = False
instance HasNullValue WhileExpr where
  nullValue = WhileExpr nullValue
  testNull (WhileExpr a) = testNull a

--data ElseIfExpr
--  = NullElseIfExpr
--  | ElseExpr              CodeBlock            Location
--  | ElseIfExpr ObjectExpr CodeBlock ElseIfExpr Location
--  deriving (Eq, Ord, Typeable, Show)
--instance HasLocation ElseIfExpr where
--  getLocation o     = case o of
--    NullElseIfExpr       -> LocationUnknown
--    ElseExpr       _ loc -> loc
--    ElseIfExpr _ _ _ loc -> loc
--  setLocation o loc = case o of
--    NullElseIfExpr       -> NullElseIfExpr
--    ElseExpr       c _   -> ElseExpr       c loc
--    ElseIfExpr a b c _   -> ElseIfExpr a b c loc
--  delLocation o     = case o of
--    NullElseIfExpr       -> NullElseIfExpr
--    ElseExpr       c _   -> ElseExpr       c LocationUnknown
--    ElseIfExpr a b c _   -> ElseIfExpr a b c LocationUnknown

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data ScriptExpr
  = IfThenElse   IfElseExpr
  | WhileLoop    WhileExpr
  | EvalObject   ObjectExpr                               Location -- location of the semicolon
  | TryCatch     CodeBlock (Maybe Name) (Maybe CodeBlock) Location
  | ForLoop      Name       ParenExpr    CodeBlock        Location
  | ContinueExpr Bool       ObjectExpr                    Location
  | ReturnExpr   Bool       ObjectExpr                    Location
  | WithDoc      ParenExpr  CodeBlock                     Location
  deriving (Eq, Ord, Typeable, Show)

instance HasNullValue ScriptExpr where
  nullValue = EvalObject nullValue LocationUnknown
  testNull (EvalObject a _) = testNull a
  testNull _ = False

instance HasLocation ScriptExpr where
  getLocation o = case o of
    EvalObject   _     o -> o
    IfThenElse         o -> getLocation o
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
    TryCatch     a b c _ -> TryCatch     a b c loc
    ForLoop      a b c _ -> ForLoop      a b c loc
    ContinueExpr a b   _ -> ContinueExpr a b   loc
    ReturnExpr   a b   _ -> ReturnExpr   a b   loc
    WithDoc      a b   _ -> WithDoc      a b   loc
  delLocation o = case o of
    EvalObject   a     _ -> EvalObject   (fd0 a)                     lu
    IfThenElse   a       -> IfThenElse   (fd0 a)
    WhileLoop    a       -> WhileLoop    (fd0 a)
    TryCatch     a b c _ -> TryCatch     (fd0 a)      b (fmap fd0 c) lu
    ForLoop      a b c _ -> ForLoop           a  (fd0 b)     (fd0 c) lu
    ContinueExpr a b   _ -> ContinueExpr      a  (fd0 b)             lu
    ReturnExpr   a b   _ -> ReturnExpr        a  (fd0 b)             lu
    WithDoc      a b   _ -> WithDoc      (fd0 a) (fd0 b)             lu
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
  = Attribute Name              ObjectExpr Location
  | TopScript ScriptExpr                   Location
  | EventExpr TopLevelEventType CodeBlock  Location
  deriving (Eq, Ord, Typeable, Show)

isAttribute :: TopLevelExpr -> Bool
isAttribute toplevel = case toplevel of { Attribute _ _ _ -> True; _ -> False; }
instance HasLocation TopLevelExpr where
  getLocation o = case o of
    Attribute      _ _   o -> o
    TopScript      _     o -> o
    EventExpr      _ _   o -> o
  setLocation o loc = case o of
    Attribute      a b   _ -> Attribute a b loc
    TopScript      a     _ -> TopScript a   loc
    EventExpr      a b   _ -> EventExpr a b loc
  delLocation o = case o of
    Attribute      a b   _ -> Attribute      a  (fd0 b) lu
    TopScript      a     _ -> TopScript (fd0 a)         lu
    EventExpr      a b   _ -> EventExpr      a  (fd0 b) lu
    where
      lu = LocationUnknown
      fd0 :: HasLocation a => a -> a
      fd0 = delLocation
      fd1 :: (HasLocation a, Functor f) => f a -> f a
      fd1 = fmap delLocation

-- | A program is just a list of 'TopLevelExpr's. It serves as the 'Dao.Object.AST.Intermediate'
-- representation of a 'Dao.Object.AST.AST_SourceCode'.
newtype Program = Program { topLevelExprs :: [TopLevelExpr] } deriving (Eq, Ord, Typeable)
instance Show Program where { show (Program o) = unlines (map show o) }
instance HasLocation Program where
  getLocation o = case topLevelExprs o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = Program (fmap delLocation (topLevelExprs o))

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

-- | An 'Action' is the result of a pattern match that occurs during an input string query. It is a
-- data structure that contains all the information necessary to run an 'Subroutine' assocaited with
-- a 'Glob', including the parent 'ExecUnit', the 'Dao.Glob.Glob' and the
-- 'Dao.Glob.Match' objects, and the 'Executables'.
data Action
  = Action
    { actionQuery      :: Maybe UStr
    , actionPattern    :: Maybe Glob
    , actionMatch      :: Maybe Match
    , actionCodeBlock  :: Subroutine
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
    { taskWaitMVar       :: MVar ThreadId
    , taskRunningThreads :: MVar (S.Set ThreadId)
    }

initTask :: ReaderT r IO Task
initTask = do
  wait    <- liftIO newEmptyMVar
  running <- liftIO $ newMVar S.empty
  return $ Task{taskWaitMVar=wait, taskRunningThreads=running}

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

class CatchesReturns a where { catchReturn :: a -> Exec (Maybe Object) }
instance CatchesReturns (Exec ()) where
  catchReturn fn = procCatch fn >>= \flow -> case flow of
    FlowOK    () -> return Nothing
    FlowReturn m -> return m
    FlowErr  err -> proc (FlowErr err)
instance CatchesReturns (Exec Object) where
  catchReturn fn = procCatch fn >>= \flow -> case flow of
    FlowOK     o -> return (Just o)
    FlowReturn m -> return m
    FlowErr  err -> proc (FlowErr err)
instance CatchesReturns (Exec (Maybe Object)) where
  catchReturn fn = procCatch fn >>= \flow -> case flow of
    FlowOK     o -> return o
    FlowReturn m -> return m
    FlowErr  err -> proc (FlowErr err)

----------------------------------------------------------------------------------------------------

class ExecRef var where
  execReadRef    :: var a -> Exec a
  execTakeRef    :: var a -> Exec a
  execPutRef     :: var a -> a -> Exec ()
  execSwapRef    :: var a -> a -> Exec a
  execModifyRef  :: var a -> (a -> Exec (a, b)) -> Exec b
  execModifyRef_ :: var a -> (a -> Exec  a    ) -> Exec ()
  execModifyRef_ var upd = execModifyRef var (\a -> upd a >>= \a -> return (a, ()))

instance ExecRef MVar where
  execModifyRef mvar upd = ask >>= \xunit -> procJoin $ liftIO $ modifyMVar mvar $ \a -> do
    result <- flip ioExec xunit $ execCatchIO (upd a) $
      [ newExecIOHandler $ \e -> execThrow $ OList $
          [ostr "ErrorCall", ostr (show (e::ErrorCall))]
      , newExecIOHandler $ \e -> execThrow $ OList $
          [ostr "BlockedIndefinitelyOnMVar" , ostr (show (e::BlockedIndefinitelyOnMVar))]
      , newExecIOHandler $ \e -> execThrow $ OList $
          [ostr "Deadlock", ostr (show(e::Deadlock))]
      ]
    return $ case result of
      FlowOK (a, b) -> (a, FlowOK     b)
      FlowReturn o  -> (a, FlowReturn o)
      FlowErr    o  -> (a, FlowErr    o)
  execModifyRef_ mvar upd = execModifyRef mvar (\a -> upd a >>= \a -> return (a, ()))
  execReadRef      = liftIO . readMVar
  execTakeRef      = liftIO . takeMVar
  execPutRef  mvar = liftIO . putMVar  mvar
  execSwapRef mvar = liftIO . swapMVar mvar

instance ExecRef IORef where
  execModifyRef  ref upd = liftIO (readIORef ref) >>= upd >>= \ (a, b) -> liftIO (writeIORef ref a) >> return b
  execModifyRef_ ref upd = liftIO (readIORef ref) >>= upd >>= liftIO . writeIORef ref
  execReadRef            = liftIO . readIORef
  execTakeRef            = execReadRef
  execPutRef     ref     = liftIO . writeIORef ref
  execSwapRef    ref obj = liftIO (readIORef ref >>= \sw -> writeIORef ref obj >> return sw)

---------------------------------------------------------------------------------------------------

type DepGraph = M.Map UPath [UPath]

getDepFiles :: DepGraph -> [UPath]
getDepFiles = M.keys

-- | Nearly every accesssor function in the 'ObjectInterface' data type take the form
-- > 'ObjectInterface' 'Data.Dynamic.Dynamic' -> Maybe ('Data.Dynamic.Dynamic' -> method)
-- where the first 'Data.Dynamic.Dynamic' value is analogous to the @this@" pointer in C++-like
-- languages, and where @method@ is any function, for example an equality function @a -> a -> Bool@
-- or an iterator function @Exec [Object]@. This function takes the @this@ value, an
-- 'ObjectInterface', and an 'ObjectInterface' accessor (for example 'objEquality' or
-- 'objIterator') and if the accessor is not 'Prelude.Nothing', the @this@ object is applied to the
-- method and the partial application is returned. If the accessor does evaluate to
-- 'Prelude.Nothing' the exception value is thrown. If the @this@ object has not been constructed
-- with 'OHaskell', the exception value is thrown.
evalObjectMethod :: Object -> Object -> (T_haskell -> Maybe (Dynamic -> method)) -> Exec method
evalObjectMethod errmsg this getter = case this of
  OHaskell this ifc -> case getter ifc of
    Nothing -> execThrow errmsg
    Just fn -> return (fn this)
  _ -> execThrow errmsg

----------------------------------------------------------------------------------------------------

newtype LocalStore       = LocalStore   (IORef (Stack Name Object))
newtype CurrentCodeBlock = CurrentCodeBlock  (Maybe Subroutine)
newtype GlobalStore      = GlobalStore  (MVar T_tree)
newtype QTimeStore       = QTimeStore   (MVar T_tree)
-- newtype WithRefStore     = WithRefStore (Maybe Object) -- DEFINED BELOW
class Store store where
  storeLookup :: store -> Reference -> Exec (Maybe Object)
  storeUpdate :: store -> Reference -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
  storeDefine :: store -> Reference -> Object -> Exec ()
  storeDelete :: store -> Reference -> Exec ()

-- The unfortunate thing about a 'WithStoreRef' is that it does not seem to need to be stored in an
-- 'Data.IORef.IORef' because the evaluation of a "with" statement can just make use of the
-- 'Control.Monad.Reader.local' function to update the 'currentWithRef' field of the 'ExecUnit'.
-- However there is no easy way to pass back updated Object, so it must be stored in an IORef.
-- Furthermore, th 'Store' class requires updates be made in the 'Exec' monad and these functions
-- were designed on the assumption that the object to be modified would be stored in some kind of
-- storage device, like an IORef or MVar.
newtype WithRefStore     = WithRefStore (Maybe (IORef Object))

-- | All functions that are built-in to the Dao language, or built-in to a library extending the Dao
-- language, are stored in 'Data.Map.Map's from the functions name to an object of this type.
-- Functions of this type are called by 'evalObject' to evaluate expressions written in the Dao
-- language.
data DaoFunc = DaoFunc { autoDerefParams :: Bool, daoForeignCall :: [Object] -> Exec (Maybe Object) }

execModifyTopStackItem :: Stack name obj -> (T.Tree name obj -> Exec (T.Tree name obj, a)) -> Exec (Stack name obj, a)
execModifyTopStackItem (Stack stks) upd = case stks of
  []       -> execThrow $ OList [ostr "execution stack empty"]
  stk:stks -> upd stk >>= \ (stk, a) -> return (Stack (stk:stks), a)

execModifyTopStackItem_ :: Stack name obj -> (T.Tree name obj -> Exec (T.Tree name obj)) -> Exec (Stack name obj)
execModifyTopStackItem_ stack upd = fmap fst $
  execModifyTopStackItem stack (\tree -> upd tree >>= \tree -> return (tree, ()))

execReadTopStackItem :: Stack name obj -> (T.Tree name obj -> Exec a) -> Exec a
execReadTopStackItem (Stack stks) lkup = case stks of
  []    -> execThrow $ OList [ostr "execution stack empty"]
  stk:_ -> lkup stk

----------------------------------------------------------------------------------------------------

type Get     a = D.GGet  MethodTable a
type PutM    a = D.GPutM MethodTable
type Put       = D.GPut  MethodTable
type Update  a = GenUpdate Object a
type UpdateErr = GenUpdateErr Object

----------------------------------------------------------------------------------------------------

newtype MethodTable = MethodTable (M.Map UStr (ObjectInterface Dynamic))
instance Monoid MethodTable where
  mempty  = MethodTable mempty
  mappend (MethodTable a) (MethodTable b) = MethodTable (M.union b a)

execGetObjTable :: UStr -> Exec (Maybe (ObjectInterface Dynamic))
execGetObjTable nm = lookupMethodTable nm . globalMethodTable <$> asks parentRuntime

lookupMethodTable :: UStr -> MethodTable -> Maybe (ObjectInterface Dynamic)
lookupMethodTable nm (MethodTable tab) = M.lookup nm tab

typeRepToUStr :: TypeRep -> UStr
typeRepToUStr a = let con = typeRepTyCon a in ustr (tyConModule con ++ '.' : tyConName con)

instance D.HasCoderTable MethodTable where
  getEncoderForType nm mtab = fmap fst $ lookupMethodTable nm mtab >>= objBinaryFormat
  getDecoderForType nm mtab = fmap snd $ lookupMethodTable nm mtab >>= objBinaryFormat

-- | The 'Dao.Runtime.GenRuntime' general type is made specific with this type synonym.
type Runtime = GenRuntime MethodTable ExecUnit

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
    , currentWithRef     :: WithRefStore
      -- ^ the current document is set by the @with@ statement during execution of a Dao script.
    , taskForExecUnits   :: Task
    , taskForActions     :: Task
    , currentQuery       :: Maybe UStr
    , currentPattern     :: Maybe Glob
    , currentMatch       :: Maybe Match
    , currentCodeBlock   :: CurrentCodeBlock
      -- ^ when evaluating a 'Subroutine' selected by a string query, the 'Action' resulting from
      -- that query is defnied here. It is only 'Data.Maybe.Nothing' when the module is first being
      -- loaded from source code.
    , currentBranch      :: [Name]
      -- ^ set by the @with@ statement during execution of a Dao script. It is used to prefix this
      -- to all global references before reading from or writing to those references.
    , importsTable       :: [(Name, ExecUnit)]
      -- ^ a pointer to the ExecUnit of every Dao program imported with the @import@ keyword.
    , patternTable       :: [CallableCode]
      -- ^ contains functions which are evaluated not by name but by passing objects to them that
      -- match their argument list.
    , topLevelFuncs      :: M.Map Name [CallableCode]
    , execStack          :: LocalStore
      -- ^ stack of local variables used during evaluation
    , queryTimeHeap      :: QTimeStore
      -- ^ the global vairables that are assigned only during a single query, and are deleted after
      -- the query has completed.
    , globalData         :: GlobalStore
      -- ^ global variables cleared after every string execution
    , execOpenFiles      :: IORef (M.Map UPath ExecUnit)
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
    , ruleSet           :: IORef (PatternTree [Subroutine])
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

-- | Semantical data structures, which are all executable, might contain 'MetaEvalExpr's, which is a
-- constructor of 'ObjectExpr'. Any data structure which contains an 'ObjectExpr' or an expression
-- which itself contains an 'ObjectExpr' can be meta-evaluated. You can tell whether or not a data
-- structure might contain an 'ObjectExpr' if it instantiates this class.
--
-- The 'metaEval' function takes a single boolean parameter which controls recursion. Basically, if
-- you want to evaluate @expr@ itself then pass True for this parameter, if you want to evaluate the
-- sub-expressions within the @expr@ leaving the structure of @expr@ mostly unchanged then pass
-- false as this parameter.
class MetaEvaluable expr where
  metaEval :: Bool -> expr -> Exec expr
  -- ^ the boolean parameter here indicates whether or not 'metaEval' has been called recursively.
  -- This function can therefore decide whether or not to expand a meta-expression or simply return
  -- it unmodified should it be necessary to only expand one layer of meta expressions, leaving
  -- inner layers unmodified. However, most instantiations of 'metaEval' should just copy this value
  -- to any recursive calls to 'metaEval'.

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
  mzero = throwError mempty
  mplus a b = procCatch a >>= \a -> case a of
    FlowOK      a -> proc (FlowOK     a)
    FlowReturn  a -> proc (FlowReturn a)
    FlowErr   err -> b
instance MonadReader ExecUnit Exec where
  local upd (Exec fn) = Exec (local upd fn)
  ask = Exec ask
instance MonadError ExecError Exec where
  throwError = Exec . throwError
  catchError (Exec try) catch = Exec (catchError try (execToProcedural . catch))
instance MonadIO Exec where { liftIO io = Exec (liftIO io) }
instance Applicative Exec where { pure = return; (<*>) = ap; }
instance Alternative Exec where { empty = mzero; (<|>) = mplus; }
instance ProceduralClass ExecError (Maybe Object) Exec where
  proc      = Exec . proc
  procCatch = Exec . procCatch . execToProcedural

-- | Nearly all execution in the 'Exec' monad that could result in an error will throw an
-- 'ExecError's. Even the use of 'Prelude.error' will throw an 'Control.Exception.ErrorCall' that
-- will eventually be caught by the 'Exec' monad and converted to an 'ExecError'.
data ExecError
  = ExecError
    { execUnitAtError   :: Maybe ExecUnit
    , execErrExpr       :: Maybe ObjectExpr
    , execErrScript     :: Maybe ScriptExpr
    , execErrTopLevel   :: Maybe TopLevelExpr
    , specificErrorData :: Object
    }
instance Monoid ExecError where
  mempty      =
    ExecError
    { execUnitAtError   = Nothing
    , execErrExpr       = Nothing
    , execErrScript     = Nothing
    , execErrTopLevel   = Nothing
    , specificErrorData = ONull
    }
  mappend a b =
    a{ execUnitAtError   = execUnitAtError a <|> execUnitAtError b
     , execErrExpr       = execErrExpr     a <|> execErrExpr     b
     , execErrScript     = execErrScript   a <|> execErrScript   b
     , execErrTopLevel   = execErrTopLevel a <|> execErrTopLevel b
     , specificErrorData = case specificErrorData a of
        ONull -> specificErrorData b
        a     -> a
     }

instance HasNullValue ExecError where
  nullValue = mempty
  testNull (ExecError Nothing Nothing Nothing Nothing ONull) = True
  testNull _ = False

class ExecThrowable a where
  toExecError :: a -> ExecError
  -- | Like 'Prelude.error' but works for the 'Exec' monad, throws an 'ExecError' using
  -- 'Control.Monad.Error.throwError' constructed using the given 'Object' value as the
  -- 'specificErrorData'.
  execThrow :: ExecThrowable a => a -> Exec ig
  execThrow obj = ask >>= \xunit -> throwError $ (toExecError obj){execUnitAtError=Just xunit}

instance ExecThrowable Object where
  toExecError err = mempty{specificErrorData=err}

instance ExecThrowable (GenUpdateErr Object) where
  toExecError err = toExecError $ OList $ concat $
    [ maybe [] ((:[]) . ostr) (updateErrMsg err)
    , [ORef  $ Unqualified $ Reference $ updateErrAddr err]
    , [OTree $ updateErrTree err]
    ]

execFromPValue :: PValue UpdateErr a -> Exec a
execFromPValue pval = case pval of
  OK      a -> return a
  Backtrack -> mzero
  PFail err -> execThrow err

----------------------------------------------------------------------------------------------------

-- | Instantiate your data type into this class when it makes sense for your data type to be used in
-- a "for" statement in the Dao programming language.
class HasIterator obj where
  -- | This function converts your object to a list. Conversion is done as lazily as possible to
  -- prevent "for" statements from eating up a huge amount of memory when iteration produces a large
  -- number of objects. For example, lets say your @obj@ is an association list. This function
  -- should return a list of every assocaition in the @obj@. Each association object will be stored
  -- in a local variable and the body of the "for" statement will be evaluated with that local
  -- variable.
  iterateObject :: obj -> Exec [Object]
  -- | This function takes the list of objects that was produced after evaluating the "for"
  -- statement and uses it to create a new @obj@ data. For example, if your data type is an
  -- association list and the "for" loop eliminates every 'Object' not satisfying a predicate, the
  -- object list passed here will be the lists of 'Object's that did satisfy the predicate and you
  -- should construct a new @obj@ using this new list of 'Object's.
  foldObject :: obj -> [Object] -> Exec obj

instance HasIterator [Object] where { iterateObject = return; foldObject _ = return; }
instance HasIterator UStr where
  iterateObject = return . fmap OChar . uchars
  foldObject  _ = fmap toUStr . foldM f "" where
    f str obj = case obj of
      OString o -> return (str++uchars o)
      OChar   o -> return (str++[o])
      obj       -> execThrow $ OList [ostr "object cannot be used to construct string", obj]

cant_iterate :: Object -> T_haskell -> Exec ig
cant_iterate o ifc = execThrow $ OList $
  [ostr $ "object of type "++show (objHaskellType ifc)++" cannot be iterated in a \"for\" statement", o]

instance HasIterator Object where
  iterateObject obj = case obj of
    OString  o -> iterateObject o
    OList    o -> iterateObject o
    OHaskell o ifc -> case objIterator ifc of
      Just (iter, _) -> iter o
      Nothing        -> cant_iterate obj ifc
  foldObject obj ox = case obj of
    OString  o -> fmap OString (foldObject o ox)
    OList    o -> fmap OList   (foldObject o ox)
    OHaskell o ifc -> case objIterator ifc of
      Just (_, fold) -> fmap (flip OHaskell ifc) (fold o ox)
      Nothing        -> cant_iterate obj ifc

----------------------------------------------------------------------------------------------------

-- | This is all of the functions used by the "Dao.Evaluator" when manipulating objects in a Dao
-- program. Behavior of objects when they are used in "for" statements or "with" statements, or when
-- they are dereferenced using the "@" operator, or when they are used in equations are all defined
-- here.
-- 
-- So this table is the reason you instantiate 'ObjectClass'.
-- 
-- @obj@ specifies the container type that will wrap-up data of type @typ@. @obj@ is the type used
-- throughout the runtime system to symbolize the basic unit of information operated on by
-- computations.
-- 
-- @typ@ specifies the type that you want to wrap-up into an @obj@ constructor. When you want to,
-- for example, check for equality between object of type @typ@, you can define a function for
-- 'objEquality'. All of the other polymorphic types are bound to the @typ@ types by the functional
-- dependencies mechanism of the Haskell language.
-- 
-- @exec@ specifies a monad in which to evaluate functions which may need to cause side-effects.
-- This should usually be a 'Control.Monad.Monad'ic type like @IO@ or 'Dao.Object.Exec'.
data ObjectInterface typ =
  ObjectInterface
  { objHaskellType   :: TypeRep -- ^ this type is deduced from the initial value provided to the 'defObjectInterface'.
  , objCastFrom      :: Maybe (Object -> typ)                                                     -- ^ defined by 'defCastFrom'
  , objEquality      :: Maybe (typ -> typ -> Bool)                                                -- ^ defined by 'defEquality'
  , objOrdering      :: Maybe (typ -> typ -> Ordering)                                            -- ^ defined by 'defOrdering'
  , objBinaryFormat  :: Maybe (typ -> Put, Get typ)                                               -- ^ defined by 'defBinaryFmt'
  , objNullTest      :: Maybe (typ -> Bool)                                                       -- ^ defined by 'defNullTest'
  , objIterator      :: Maybe (typ -> Exec [Object], typ -> [Object] -> Exec typ)                 -- ^ defined by 'defIterator'
  , objIndexer       :: Maybe (typ -> Object -> Exec Object)                                      -- ^ defined by 'defIndexer'
  , objTreeFormat    :: Maybe (typ -> Exec T_tree, T_tree -> Exec typ)                            -- ^ defined by 'defTreeFormat'
  , objInitializer   :: Maybe ([Object] -> [Object] -> Exec typ)                                  -- ^ defined by 'defInitializer'
  , objUpdateOpTable :: Maybe (Array UpdateOp (Maybe (UpdateOp -> typ -> Object -> Exec Object))) -- ^ defined by 'defUpdateOp'
  , objInfixOpTable  :: Maybe (Array InfixOp  (Maybe (InfixOp  -> typ -> Object -> Exec Object))) -- ^ defined by 'defInfixOp'
  , objPrefixOpTable :: Maybe (Array PrefixOp (Maybe (PrefixOp -> typ -> Exec Object)))           -- ^ defined by 'defPrefixOp'
  , objCallable      :: Maybe (typ -> Exec [CallableCode])                                        -- ^ defined by 'defCallable'
  }
  deriving Typeable
instance Eq  (ObjectInterface typ) where { a==b = objHaskellType a == objHaskellType b }
instance Ord (ObjectInterface typ) where { compare a b = compare (objHaskellType a) (objHaskellType b) }

instance ObjTableClass (ObjectInterface Dynamic) Object where
  objTable (OHaskell _ tab) = Just tab
  objTable  _               = Nothing

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
-- string will allow you to throw more informative error messages. WARNING: this function leaves
-- 'objHaskellType' unchanged, the calling context must change it.
objectInterfaceAdapter
  :: (Typeable typ_a, Typeable typ_b)
  => (String -> typ_a -> typ_b)
  -> (String -> typ_b -> typ_a)
  -> ObjectInterface typ_a
  -> ObjectInterface typ_b
objectInterfaceAdapter a2b b2a ifc = 
  let uninit  = error "'Dao.Object.fmapObjectInterface' evaluated on uninitialized object"
  in  ifc
      { objCastFrom      = let n="objCastFrom"      in fmap (fmap (a2b n)) (objCastFrom ifc)
      , objEquality      = let n="objEquality"      in fmap (\eq  a b -> eq  (b2a n a) (b2a n b)) (objEquality ifc)
      , objOrdering      = let n="objOrdering"      in fmap (\ord a b -> ord (b2a n b) (b2a n b)) (objOrdering ifc)
      , objBinaryFormat  = let n="objBinaryFormat"  in fmap (\ (toBin , fromBin) -> (toBin . b2a n, fmap (a2b n) fromBin)) (objBinaryFormat ifc)
      , objNullTest      = let n="objNullTest"      in fmap (\null b -> null (b2a n b)) (objNullTest ifc)
      , objIterator      = let n="objIterator"      in fmap (\ (iter, fold) -> (iter . b2a n, \typ -> fmap (a2b n) . fold (b2a n typ))) (objIterator ifc)
      , objIndexer       = let n="objIndexer"       in fmap (\indx b -> indx (b2a n b)) (objIndexer  ifc)
      , objTreeFormat    = let n="objTreeFormat"    in fmap (\ (toTree, fromTree) -> (toTree . b2a n, fmap (a2b n) . fromTree)) (objTreeFormat ifc)
      , objInitializer   = let n="objInitializer"   in fmap (fmap (fmap (fmap (a2b n)))) (objInitializer ifc)
      , objUpdateOpTable = let n="objUpdateOpTable" in fmap (fmap (fmap (\updt op b -> updt op (b2a n b)))) (objUpdateOpTable ifc)
      , objInfixOpTable  = let n="objInfixOpTable"  in fmap (fmap (fmap (\infx op b -> infx op (b2a n b)))) (objInfixOpTable  ifc)
      , objPrefixOpTable = let n="objPrefixOpTabl"  in fmap (fmap (fmap (\prfx op b -> prfx op (b2a n b)))) (objPrefixOpTable ifc)
      , objCallable      = let n="objCallable"      in fmap (\eval typ -> eval (b2a n typ)) (objCallable    ifc)
      }

objectInterfaceToDynamic :: Typeable typ => ObjectInterface typ -> ObjectInterface Dynamic
objectInterfaceToDynamic oi = objectInterfaceAdapter (\ _ -> toDyn) (from oi) oi where
  from :: Typeable typ => ObjectInterface typ -> String -> Dynamic -> typ
  from oi msg dyn = fromDyn dyn (dynErr oi msg dyn)
  uninit = error $
    "'Dao.Object.objectInterfaceToDynamic' evaluated an uninitialized 'ObjectInterface'"
  dynErr :: Typeable typ => ObjectInterface typ -> String -> Dynamic -> typ
  dynErr oi msg dyn = error $ concat $
    [ "The 'Dao.Object.", msg
    , "' function defined for objects of type ", show (objHaskellType oi)
    , " was evaluated on an object of type ", show (dynTypeRep dyn)
    ]

-- Used to construct an 'ObjectInterface' in a "Control.Monad.State"-ful way. Instantiates
-- 'Data.Monoid.Monoid' to provide 'Data.Monoid.mempty' an allows multiple inheritence by use of the
-- 'Data.Monoid.mappend' function in the same way as
data ObjIfc typ =
  ObjIfc
  { objIfcCastFrom      :: Maybe (Object -> typ)
  , objIfcEquality      :: Maybe (typ -> typ -> Bool)
  , objIfcOrdering      :: Maybe (typ -> typ -> Ordering)
  , objIfcBinaryFormat  :: Maybe (typ -> Put, Get typ)
  , objIfcNullTest      :: Maybe (typ -> Bool)
  , objIfcIterator      :: Maybe (typ -> Exec [Object], typ -> [Object] -> Exec typ)
  , objIfcIndexer       :: Maybe (typ -> Object -> Exec Object)
  , objIfcTreeFormat    :: Maybe (typ -> Exec T_tree, T_tree -> Exec typ)
  , objIfcInitializer   :: Maybe ([Object] -> [Object] -> Exec typ)
  , objIfcUpdateOpTable :: [(UpdateOp, UpdateOp -> typ -> Object -> Exec Object)]
  , objIfcInfixOpTable  :: [(InfixOp , InfixOp  -> typ -> Object -> Exec Object)]
  , objIfcPrefixOpTable :: [(PrefixOp, PrefixOp -> typ -> Exec Object)]
  , objIfcCallable      :: Maybe (typ -> Exec [CallableCode])
  }
initObjIfc =
  ObjIfc
  { objIfcCastFrom      = Nothing
  , objIfcEquality      = Nothing
  , objIfcOrdering      = Nothing
  , objIfcBinaryFormat  = Nothing
  , objIfcNullTest      = Nothing
  , objIfcIterator      = Nothing
  , objIfcIndexer       = Nothing
  , objIfcTreeFormat    = Nothing
  , objIfcInitializer   = Nothing
  , objIfcUpdateOpTable = []
  , objIfcInfixOpTable  = []
  , objIfcPrefixOpTable = []
  , objIfcCallable      = Nothing
  }

-- | A handy monadic interface for defining an 'ObjectInterface' using nice, clean procedural
-- syntax.
newtype DaoClassDef typ a = DaoClassDef { daoClassDefState :: State (ObjIfc typ) a }
instance Typeable typ => Functor (DaoClassDef typ) where
  fmap f (DaoClassDef m) = DaoClassDef (fmap f m)
instance Typeable typ => Monad (DaoClassDef typ) where
  return = DaoClassDef . return
  (DaoClassDef m) >>= f = DaoClassDef (m >>= daoClassDefState . f)
instance Typeable typ => Applicative (DaoClassDef typ) where { pure=return; (<*>)=ap; }

-- not for export
updObjIfc :: Typeable typ => (ObjIfc typ -> ObjIfc typ) -> DaoClassDef typ ()
updObjIfc = DaoClassDef . modify

-- | The callback function defined here is used when objects of your @typ@ can be constructed from
-- some other 'Object'. This function is used to convert an 'Object' of another types to an data
-- type of your @typ@ when it is necessary to do so (for example, evaluating the @==@ or @!=@
-- operator).
defCastFrom :: Typeable typ => (Object -> typ) -> DaoClassDef typ ()
defCastFrom fn = updObjIfc(\st->st{objIfcCastFrom=Just fn})

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @==@ and @!=@ operators in Dao programs. However using this is slightly
-- different than simply overriding the @==@ or @!=@ operators. Defining an equality reliation with
-- this function also allows Haskell language programs to compare your object to other objects
-- without unwrapping them from the 'Object' wrapper.
--
-- This function automatically define an equality operation over your @typ@ using the
-- instantiation of 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function.
-- The 'defCastFrom' function is used to cast 'Object's to a value of your @typ@, and then the
-- @Prelude.==@ function is evaluated. If you eventually never define a type casting funcion using
-- 'defCastFrom', this function will fail, but it will fail lazily and at runtime, perhaps when you
-- least expect it, so be sure to define 'defCastFrom' at some point.
autoDefEquality :: (Typeable typ, Eq typ, ObjectClass typ Object (ObjectInterface typ)) => DaoClassDef typ ()
autoDefEquality = defEquality (==)

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @==@ and @!=@ operators in Dao programs. However using this is slightly
-- different than simply overriding the @==@ or @!=@ operators. Defining an equality relation with
-- this function also allows Haskell language programs to compare your object to other objects
-- without unwrapping them from the 'Object' wrapper.
--
-- This function differs from 'autoDefEquality' because you must provide a customized equality
-- relation for your @typ@, if the 'autoDefEquality' and 'defCastFrom' functions are to be avoided
-- for some reason.
defEquality :: (Typeable typ, Eq typ, ObjectClass typ Object (ObjectInterface typ)) => (typ -> typ -> Bool) -> DaoClassDef typ ()
defEquality fn = updObjIfc(\st->st{objIfcEquality=Just fn})

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @<@, @>@, @<=@, and @>=@ operators in Dao programs. However using this is
-- slightly different than simply overriding the @<@, @>@, @<=@, or @>=@ operators. Defining an
-- equality relation with this function also allows Haskell language programs to compare your obejct
-- to other objects without unwrapping them from the 'Object' wrapper.
-- 
-- Automatically define an ordering for your @typ@ using the instantiation of
-- 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function. The 'defCastFrom'
-- function is used to cast 'Object's to a value of your @typ@, and then the @Prelude.==@ function
-- is evaluated. If you eventually never define a type casting funcion using 'defCastFrom', this
-- function will fail, but it will fail lazily and at runtime, perhaps when you least expect it, so
-- be sure to define 'defCastFrom' at some point.
autoDefOrdering :: (Typeable typ, Ord typ, ObjectClass typ Object (ObjectInterface typ)) => DaoClassDef typ ()
autoDefOrdering = defOrdering compare

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @<@, @>@, @<=@, and @>=@ operators in Dao programs. However using this is
-- slightly different than simply overriding the @<@, @>@, @<=@, or @>=@ operators. Defining an
-- equality relation with this function also allows Haskell language programs to compare your obejct
-- to other objects without unwrapping them from the 'Object' wrapper.
-- 
-- Define a customized ordering for your @typ@, if the 'autoDefEquality' and 'defCastFrom'
-- functions are to be avoided for some reason.
defOrdering :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => (typ -> typ -> Ordering) -> DaoClassDef typ ()
defOrdering fn = updObjIfc(\st->st{objIfcOrdering=Just fn})

-- | The callback function defined here is used if an object of your @typ@ should ever need to be
-- stored into a binary file in persistent storage (like your filesystem) or sent across a channel
-- (like a UNIX pipe or a socket).
-- 
-- It automatically define the binary encoder and decoder using the 'Data.Binary.Binary' class
-- instantiation for this @typ@.
autoDefBinaryFmt :: (Typeable typ, D.Binary typ MethodTable, ObjectClass typ Object (ObjectInterface typ)) => DaoClassDef typ ()
autoDefBinaryFmt = defBinaryFmt D.put D.get

-- | This function is used if an object of your @typ@ should ever need to be stored into a binary
-- file in persistent storage (like your filesystem) or sent across a channel (like a UNIX pipe or a
-- socket).
-- 
-- If you have binary coding and decoding methods for your @typ@ but for some silly reason not
-- instantiated your @typ@ into the 'Data.Binary.Binary' class, your @typ@ can still be used as a
-- binary formatted object by the Dao system if you define the encoder and decoder using this
-- function. However, it would be better if you instantiated 'Data.Binary.Binary' and used
-- 'autoDefBinaryFmt' instead.
defBinaryFmt :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => (typ -> Put) -> Get typ -> DaoClassDef typ ()
defBinaryFmt put get = updObjIfc(\st->st{objIfcBinaryFormat=Just(put,get)})

autoDefNullTest :: (Typeable typ, HasNullValue typ, ObjectClass typ Object (ObjectInterface typ)) => DaoClassDef typ ()
autoDefNullTest = defNullTest testNull

-- | The callback function defined here is used if an object of your @typ@ is ever used in an @if@
-- or @while@ statement in a Dao program. This function will return @Prelude.True@ if the object is
-- of a null value, which will cause the @if@ or @while@ test to fail and execution of the Dao
-- program will branch accordingly. There is no default method for this function so it must be
-- defined by this function, otherwise your object cannot be tested by @if@ or @while@ statements.
defNullTest :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => (typ -> Bool) -> DaoClassDef typ ()
defNullTest fn = updObjIfc(\st->st{objIfcNullTest=Just fn})

-- | The callback function defined here is used if an object of your @typ@ is ever used in a @for@
-- statement in a Dao program. However it is much better to instantiate your @typ@ into the
-- 'HasIterator' class and use 'autoDefIterator' instead.
defIterator :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => (typ -> Exec [Object]) -> (typ -> [Object] -> Exec typ) -> DaoClassDef typ ()
defIterator iter fold = updObjIfc(\st->st{objIfcIterator=Just(iter,fold)})

-- | Using the instantiation of the 'HasIterator' class for your @typ@, installs the necessary
-- callbacks into the 'ObjectInterface' to allow your data type to be iterated over in the Dao
-- programming language when it is used in a "for" statement.
autoDefIterator :: (Typeable typ, HasIterator typ, ObjectClass typ Object (ObjectInterface typ)) => DaoClassDef typ ()
autoDefIterator = defIterator iterateObject foldObject

-- | The callback function defined here is used at any point in a Dao program where a variable is
-- subscripted with square brackets, for example in the statement: @x[0] = t[1][A][B];@ The object
-- passed to your callback function is the object containing the subscript value. So in the above
-- example, if the local variables @x@ and @t@ are both values of your @typ@, this callback function
-- will be evaluated four times:
-- 1.  with the given 'Object' parameter being @('OInt' 0)@ and the @typ@ parameter as the value stored in
--     the local variable @x@.
-- 2.  with the given 'Object' parameter being @('OInt' 1)@ and the @typ@ parameter as the value
--     stored in the local variable @y@.
-- 3.  once with the 'Object' parameter being the result of dereferencing the local varaible @A@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 4.  once the given 'Object' parameter being the result of dereferencing the local variable @B@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 
-- Statements like this:
-- > a[0,1,2]
-- are evaluated by the "Dao.Evaluator" module in the exact same way as statements like this:
-- > a[0][1][2]
defIndexer :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => (typ -> Object -> Exec Object) -> DaoClassDef typ ()
defIndexer fn = updObjIfc(\st->st{objIfcIndexer=Just fn})

-- | The callback defined here is used when an object of your @typ@ is on the left-hand side of the
-- dot (@.@) operator in a Dao program. This is a much more elegant and organized way of defining
-- referencing semantics for objects of your @typ@ than simply overriding the dot operator. Dao can
-- provide a consistent programming language interface to all objects that define this callback. By
-- converting your object to, and re-constructing it from, a 'Dao.Tree.Tree' value and updating
-- parts of the 'Dao.Tree.Tree' when reading or writing values from data of your @typ@. Tree
-- construction is done lazily, so even extremely large objects will not produce an entire tree in
-- memory, so efficiency not something you should be concerned about here.
-- 
-- This function automatically defines the tree encoder and decoder using the 'Structured' class
-- instantiation for this @typ@.
autoDefTreeFormat :: (Typeable typ, Structured typ Object, ObjectClass typ Object (ObjectInterface typ)) => DaoClassDef typ ()
autoDefTreeFormat = defTreeFormat (return . dataToStruct) (execFromPValue . structToData)

-- | If for some reason you need to define a tree encoder and decoder for the 'ObjectInterface' of
-- your @typ@ without instnatiating 'Structured', use this function to define the tree encoder an
-- decoder directly
defTreeFormat :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => (typ -> Exec T_tree) -> (T_tree -> Exec typ) -> DaoClassDef typ ()
defTreeFormat encode decode = updObjIfc(\st->st{objIfcTreeFormat=Just(encode,decode)})

-- | The callback defined here is used when a Dao program makes use of the static initialization
-- syntax of the Dao programming language, which are expression of this form:
-- > a = MyType(param1, param2, ...., paramN);
-- > a = MyType { paramA=initA, paramB=initB, .... };
-- > a = MyType(param1, param2, ...., paramN) { paramA=initA, paramB=initB, .... };
-- When the interpreter sees this form of expression, it looks up the 'ObjectInterface' for your
-- @typ@ and checks if a callback has been defined by 'defInitializer'. If so, then the callback is
-- evaluated with a list of object values passed as the first parameter which contain the object
-- values written in the parentheses, and a 'T_tree' as the second paramter containing the tree
-- structure that was constructed with the expression in the braces.
defInitializer :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => ([Object] -> [Object] -> Exec typ) -> DaoClassDef typ ()
defInitializer fn = updObjIfc(\st->st{objIfcInitializer=Just fn})

-- | Overload update/assignment operators in the Dao programming language, for example @=@, @+=@,
-- @<<=@ and so on. Call this method as many times with as many different 'UpdateOp's as necessary.
-- 
-- Like with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
-- 
-- If you define two callbacks for the same 'UpdateOp', this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defUpdateOp :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => UpdateOp -> (UpdateOp -> typ -> Object -> Exec Object) -> DaoClassDef typ ()
defUpdateOp op fn = updObjIfc(\st->st{objIfcUpdateOpTable=objIfcUpdateOpTable st++[(op, fn)]})

-- | Overload infix operators in the Dao programming language, for example @+@, @*@, or @<<@.
-- 
-- Like with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
--
-- If you define two callbacks for the same 'UpdateOp', this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defInfixOp :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => InfixOp -> (InfixOp -> typ -> Object -> Exec Object) -> DaoClassDef typ ()
defInfixOp  op fn = updObjIfc $ \st -> st{objIfcInfixOpTable  = objIfcInfixOpTable  st ++ [(op, fn)] }

-- | Overload prefix operators in the Dao programming language, for example @@@, @$@, @-@, and @+@.
-- 
-- Like with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
-- 
-- If you define two callbacks for the same 'UpdateOp', this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defPrefixOp :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => PrefixOp -> (PrefixOp -> typ -> Exec Object) -> DaoClassDef typ ()
defPrefixOp op fn = updObjIfc $ \st -> st{objIfcPrefixOpTable = objIfcPrefixOpTable st ++ [(op, fn)] }

-- | Define static functions which can be called on objects of your @typ@. If you define a function here
-- with the string "funcName", then in a Dao program, you will be able to write an expression such
-- as: @myObj.funcName(param1, param2, ..., paramN);@.
--
-- It is also possible to instantiate your @typ@ into the 'Structured' class in such a way that the
-- dao expression: @myObj.name1.name2.name3@ evaluates to a 'DaoFunc', in which case that
-- 'DaoFunc' will be evaluated using parameters should a function expression be evaluated, such as:
-- @myObj.name1.name2.name3(param1, param2)@. This means if your 'Structured' instance
-- evaluates to a function for a label @"name1@", the expression @myObj.name1(param1, param2)@
-- will conflict with any method defined with 'defMethod'. Should this happen, the function returned
-- by the 'Structured' instance will be used, rather than the function defined with 'defMethod'. The
-- reasoning for this is that the Dao program may want to modify the functions of an object at
-- runtime, and so the functions defined at runtime should not be masked by functions defined by
-- 'defMethod'. The 'defMethod' functions, therefore, are the immutable default functions for those
-- function names.
--defMethod :: Typeable typ => Name -> (typ -> DaoFunc) -> DaoClassDef typ ()
--defMethod nm fn = updObjIfc $ \st ->
--  st{objIfcMethods = T.execUpdateTree (T.goto [nm] >> T.modifyLeaf(<>(Just [fn]))) (objIfcMethods st)}

defCallable :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => (typ -> Exec [CallableCode]) -> DaoClassDef typ ()
defCallable fn = updObjIfc (\st -> st{objIfcCallable=Just fn})

-- | Rocket. Yeah. Sail away with you.
defLeppard :: (Typeable typ, ObjectClass typ Object (ObjectInterface typ)) => rocket -> yeah -> DaoClassDef typ ()
defLeppard _ _ = return ()

-- | Use this function and the handy 'DaoClassDef' monad to define the 'objectInterface' function of
-- the 'ObjectClass' class:
-- > instance 'ObjectClass' MyData where
-- >     objectInterface = defObjectInterface initVal $ do
-- >         autoDefEquality
-- >         autoDefOrdering
-- >         defCastFrom $ \obj -> ....
-- The initial value (@initVal@ in the above example) /MUST/ be defined because it will be used by
-- 'Data.Typeable.typeOf' to extract a 'Data.Typeable.TypeRep'. If the value is 'Prelude.undefined'
-- or otherwise evalautes to the bottom element, the 'objectInterface' will evaluate to the bottom
-- element as well and will error at runtime, possibly when you least expect it to.
defObjectInterface :: Typeable typ => typ -> DaoClassDef typ ig -> ObjectInterface typ
defObjectInterface init defIfc =
  ObjectInterface
  { objHaskellType    = typ
  , objCastFrom       = objIfcCastFrom     ifc
  , objEquality       = objIfcEquality     ifc
  , objOrdering       = objIfcOrdering     ifc
  , objBinaryFormat   = objIfcBinaryFormat ifc
  , objNullTest       = objIfcNullTest     ifc
  , objIterator       = objIfcIterator     ifc
  , objIndexer        = objIfcIndexer      ifc
  , objTreeFormat     = objIfcTreeFormat   ifc
  , objInitializer    = objIfcInitializer  ifc
  , objUpdateOpTable  = mkArray "defUpdateOp" $ objIfcUpdateOpTable ifc
  , objInfixOpTable   = mkArray "defInfixOp"  $ objIfcInfixOpTable  ifc
  , objPrefixOpTable  = mkArray "defPrefixOp" $ objIfcPrefixOpTable ifc
  , objCallable       = objIfcCallable     ifc
--  , objMethods        = setupMethods (T.assocs (objIfcMethods ifc))
  }
  where
    typ               = typeOf init
    ifc               = execState (daoClassDefState defIfc) initObjIfc
    setupMethods elems = T.fromList $ elems >>= \ (nm, fn) -> case fn of
      []    -> error $ unwords $
        [ "INTERNAL: somehow the 'Dao.Object.defMethod' function"
        , "has inserted a null list for the \""++intercalate "." (fmap uchars nm)++"\" function"
        , "in the methods table of", show typ
        ]
      [fn]  -> return (nm, fn)
      _:_:_ -> conflict "defMethod" ("the function called "++show nm)
    mkArray oiName elems =
      if null elems
        then  Nothing
        else  minAccumArray (onlyOnce oiName) Nothing $ map (\ (i, e) -> (i, (i, Just e))) elems
    onlyOnce oiName a b  = case b of
      (_, Nothing) -> a
      (i, Just  _) -> conflict oiName ("the "++show i++" operator")
    conflict oiName funcName = error $ concat $
      [ "'Dao.Object.", oiName
      , "' has conflicting functions for ", funcName
      , " for the 'Dao.Object.ObjectClass' instantiation of the '", show typ
      , "' Haskell data type."
      ]


