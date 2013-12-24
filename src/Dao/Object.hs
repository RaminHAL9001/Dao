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

module Dao.Object
  ( module Dao.String
  , module Dao.Object
  ) where

import           Dao.String
import qualified Dao.EnumSet   as Es
import qualified Dao.Tree      as T
import qualified Dao.Binary    as B
import           Dao.PPrint
import           Dao.Struct
import           Dao.Random

import           Data.Typeable
import           Data.Monoid
import           Data.List
import qualified Data.Complex  as C
import           Data.Ix
import           Data.Char
import           Data.Word
import           Data.Bits
import           Data.Array.IArray
import           Data.Time hiding (parseTime)

import qualified Data.ByteString.Lazy      as B

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Reader

----------------------------------------------------------------------------------------------------

-- | The 'Object' type extends the 'Data.Dynamic.Dynamic' data type with a few more constructors for
-- data types that are fundamental to a programming language, like integers, strings, and lists.
data Value o
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
  | OList      [Value o]
  | OTree      (T.Tree Name (Value o))
  | OBytes     T_bytes
  | OHaskell   o
  deriving (Eq, Ord, Typeable, Show)

type T_type     = ObjType
type T_int      = Int
type T_word     = Word64
type T_long     = Integer
type T_ratio    = Rational
type T_complex  = Complex
type T_float    = Double
type T_time     = UTCTime
type T_diffTime = NominalDiffTime
type T_char     = Char
type T_string   = UStr
type T_ref      = QualRef
type T_bytes    = B.ByteString

instance NFData obj => NFData (Value obj) where
  rnf  ONull         = ()
  rnf  OTrue         = ()
  rnf (OType      a) = deepseq a ()
  rnf (OInt       a) = deepseq a ()
  rnf (OWord      a) = deepseq a ()
  rnf (OLong      a) = deepseq a ()
  rnf (OFloat     a) = deepseq a ()
  rnf (ORatio     a) = deepseq a ()
  rnf (OComplex   a) = deepseq a ()
  rnf (OAbsTime   a) = deepseq a ()
  rnf (ORelTime   a) = deepseq a ()
  rnf (OChar      a) = deepseq a ()
  rnf (OString    a) = deepseq a ()
  rnf (ORef       a) = deepseq a ()
  rnf (OList      a) = deepseq a ()
  rnf (OTree      a) = deepseq a ()
  rnf (OBytes     a) = seq a ()
  rnf (OHaskell   a) = deepseq a ()

instance HasNullValue obj => HasNullValue (Value obj) where
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
    OHaskell  o  -> testNull o
    _            -> False

instance B.Binary o mtab => B.Binary (Value o) mtab where
  put o = do
    let t   = B.put (objType o)
        p o = t >> B.put o
    case o of
      ONull      -> t
      OTrue      -> t
      OType    o -> p o
      OInt     o -> p o
      OWord    o -> p o
      OLong    o -> p o
      OFloat   o -> p o
      ORatio   o -> p o
      OComplex o -> p o
      OAbsTime o -> p o
      ORelTime o -> p o
      OChar    o -> p o
      OString  o -> p o
      ORef     o -> p o
      OList    o -> t >> B.putUnwrapped o
      OTree    o -> p o
      OBytes   o -> p o
      OHaskell o -> B.put o
  get = B.word8PrefixTable <|> fail "expecting Object"

instance B.Binary o mtab => B.HasPrefixTable (Value o) B.Byte mtab where
  prefixTable =
    let g f = fmap f B.get
    in  B.mkPrefixTableWord8 "Object" 0x08 0x19 $
          [ return ONull
          , return OTrue
          , g OType
          , g OInt
          , g OWord
          , g OLong
          , g OFloat
          , g ORatio
          , g OComplex
          , g OAbsTime
          , g ORelTime
          , g OChar
          , g OString
          , g ORef
          , OList <$> B.getUnwrapped
          , g OTree
          , g OBytes
          , mplus (OHaskell <$> B.get)
                  (B.get >>= \ (B.BlockStream1M bs1m) -> return (OBytes bs1m))
          ]

isNumeric :: Value o -> Bool
isNumeric o = case o of
  OWord     _ -> True
  OInt      _ -> True
  OLong     _ -> True
  ORelTime _ -> True
  OFloat    _ -> True
  ORatio    _ -> True
  OComplex  _ -> True
  _           -> False

isIntegral :: Value o -> Bool
isIntegral o = case o of
  OWord _ -> True
  OInt  _ -> True
  OLong _ -> True
  _       -> False

isRational :: Value o -> Bool
isRational o = case o of
  OWord     _ -> True
  OInt      _ -> True
  OLong     _ -> True
  ORelTime _ -> True
  OFloat    _ -> True
  ORatio    _ -> True
  _           -> False

isFloating :: Value o -> Bool
isFloating o = case o of
  OFloat   _ -> True
  OComplex _ -> True
  _          -> False

objToIntegral :: Value o -> Maybe Integer
objToIntegral o = case o of
  OWord o -> return $ toInteger o
  OInt  o -> return $ toInteger o
  OLong o -> return o
  _       -> mzero

objToRational :: Value o -> Maybe Rational
objToRational o = case o of
  OWord     o -> return $ toRational o
  OInt      o -> return $ toRational o
  ORelTime o -> return $ toRational o
  OFloat    o -> return $ toRational o
  OLong     o -> return $ toRational o
  ORatio    o -> return o
  _           -> mzero

objToInt :: Value o -> Maybe Int
objToInt a = objToIntegral a >>= \a ->
  if minInt <= a && a <= maxInt then return (fromIntegral a) else mzero
  where
    minInt = fromIntegral (minBound::Int)
    maxInt = fromIntegral (maxBound::Int)

-- | Used to implement a version of 'Data.Bits.shift' and 'Data.Bits.rotate', but with an object as
-- the second parameter to these functions.
bitsMove :: (forall a . Bits a => a -> Int -> a) -> Value o -> Value o -> Maybe (Value o)
bitsMove fn a b = objToInt b >>= \b -> bitsMoveInt fn a b

bitsMoveInt :: (forall a . Bits a => a -> Int -> a) -> Value o -> Int -> Maybe (Value o)
bitsMoveInt fn a b = case a of
  OInt  a -> return (OInt  (fn a b))
  OWord a -> return (OWord (fn a b))
  OLong a -> return (OLong (fn a b))
  _       -> mzero

-- | Used to implement a version of 'Data.Bits.testBit' but with an object as the second parameter
-- to these functions.
objTestBit :: Value o -> Value o -> Maybe (Value o)
objTestBit a i = objToInt i >>= \i -> case a of
  OInt  a -> return (oBool (testBit a i))
  OWord a -> return (oBool (testBit a i))
  OLong a -> return (oBool (testBit a i))
  _       -> mzero

----------------------------------------------------------------------------------------------------

-- | Direct a reference at a particular tree in the runtime.
data RefQualifier
  = LOCAL -- ^ the default unless in a "with" statement, refers to the current local variable stack
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

instance PPrintable RefQualifier where { pPrint = pUStr . toUStr }

instance PrecedeWithSpace RefQualifier where
   precedeWithSpace o = case o of
     LOCAL    -> True
     STATIC   -> True
     GLOBAL   -> True
     _        -> False

instance UStrType RefQualifier where
  toUStr a = ustr $ case a of
    LOCAL  -> "local"
    STATIC -> "static"
    GLOBAL -> "global"
    GLODOT -> "."
  maybeFromUStr str = case uchars str of
    "local"  -> Just LOCAL
    "static" -> Just STATIC
    "global" -> Just GLOBAL
    "."      -> Just GLODOT
    _        -> Nothing
  fromUStr str = maybe (error (show str++" is not a reference qualifier")) id (maybeFromUStr str)

instance HasRandGen RefQualifier where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::RefQualifier)))

----------------------------------------------------------------------------------------------------

data QualRef
  = Unqualified Reference
  | Qualified RefQualifier Reference
  deriving (Eq, Ord, Typeable, Show)

instance NFData QualRef where
  rnf (Unqualified r) = deepseq r $! ()
  rnf (Qualified q r) = seq q $! deepseq r ()

instance HasNullValue QualRef where
  nullValue = Unqualified (Reference [])
  testNull o = case o of
    Unqualified     r -> testNull r
    Qualified LOCAL r -> testNull r
    _                 -> False

instance PPrintable QualRef where
  pPrint o = case o of
    Unqualified r -> pInline $ dots r
    Qualified q r -> pInline $ ([pPrint q, pString " "]++) $ dots r
    where { dots r = intercalate [pString "."] $ map (return . pPrint) $ refNameList r }

instance B.Binary QualRef mtab where
  put o = case o of
    Unqualified ref -> B.put ref
    Qualified q ref -> B.prefixByte pfx $ B.put ref where
      pfx = case q of
        LOCAL  -> 0x20
        GLODOT -> 0x21
        STATIC -> 0x22
        GLOBAL -> 0x23
  get = B.word8PrefixTable <|> fail "expecting QualRef"

instance B.HasPrefixTable QualRef B.Byte mtab where
  prefixTable = mconcat $
    [ Unqualified <$> B.prefixTable
    , B.mkPrefixTableWord8 "QualRef" 0x20 0x23 $
        [ Qualified LOCAL  <$> B.get
        , Qualified GLODOT <$> B.get
        , Qualified STATIC <$> B.get
        , Qualified GLOBAL <$> B.get
        ]
    ]

instance Structured QualRef (Value a) where
  dataToStruct = deconstruct . place . ORef
  structToData = reconstruct $ do
    a <- this
    case a of
      ORef a -> return a
      _      -> fail "reference"

instance HasRandGen QualRef where
  randO = do
    let maxbnd = fromEnum(maxBound::RefQualifier)
    i   <- nextInt (2*(maxbnd-fromEnum(minBound::RefQualifier)))
    let (d, m) = divMod i 2
    if m==0 then Unqualified <$> randO else Qualified (toEnum d) <$> randO

----------------------------------------------------------------------------------------------------

-- $Object_types
-- Here we have a lambda calculus for describing types. Computationally, it is very similar to the
-- Prolog programming language, however an 'ObjType' is written using a subset the Dao scripting
-- langauge.

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

instance Show CoreType where
  show t = case t of
    NullType     -> "null"
    TrueType     -> "true"
    TypeType     -> "type"
    IntType      -> "int"
    WordType     -> "word"
    FloatType    -> "float"
    LongType     -> "long"
    RatioType    -> "ratio"
    DiffTimeType -> "diff"
    TimeType     -> "time"
    CharType     -> "char"
    StringType   -> "string"
    RefType      -> "ref"
    ListType     -> "list"
    TreeType     -> "tree"
    BytesType    -> "bytes"
    ComplexType  -> "complex"
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

instance NFData CoreType where { rnf a = seq a () }

instance UStrType CoreType where
  toUStr = derive_ustr
  maybeFromUStr a = case readsPrec 0 (uchars a) of
    [(o, "")] -> Just o
    _         -> Nothing
  fromUStr a = case maybeFromUStr a of
    Nothing -> error (show a++" is not a valid type identifier")
    Just  a -> a

instance Es.InfBound CoreType where
  minBoundInf = Es.Point minBound
  maxBoundInf = Es.Point maxBound

instance PPrintable CoreType where
  pPrint t = pString $ case t of
    NullType     -> "Null"
    TrueType     -> "True"
    TypeType     -> "Type"
    IntType      -> "Int"
    WordType     -> "Word"
    DiffTimeType -> "Difftime"
    FloatType    -> "Float"
    LongType     -> "Long"
    RatioType    -> "Ratio"
    ComplexType  -> "Complex"
    TimeType     -> "Time"
    CharType     -> "Char"
    StringType   -> "String"
    RefType      -> "Ref"
    ListType     -> "List"
    TreeType     -> "Tree"
    BytesType    -> "Data"
    HaskellType  -> "Foreign"

instance B.Binary CoreType mtab where
  put t = B.putWord8 $ case t of
    NullType     -> 0x08
    TrueType     -> 0x09
    TypeType     -> 0x0A
    IntType      -> 0x0B
    WordType     -> 0x0C
    LongType     -> 0x0D
    FloatType    -> 0x0E
    RatioType    -> 0x0F
    ComplexType  -> 0x10
    TimeType     -> 0x11
    DiffTimeType -> 0x12
    CharType     -> 0x13
    StringType   -> 0x14
    RefType      -> 0x15
    ListType     -> 0x16
    TreeType     -> 0x17
    BytesType    -> 0x18
    HaskellType  -> 0x19
  get = B.word8PrefixTable <|> fail "expecting CoreType"

instance B.HasPrefixTable CoreType B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "CoreType" 0x08 0x19 $ map return $
    [ NullType
    , TrueType
    , TypeType
    , IntType
    , WordType
    , LongType
    , FloatType
    , RatioType
    , ComplexType
    , TimeType
    , DiffTimeType
    , CharType
    , StringType
    , RefType
    , ListType
    , TreeType
    , BytesType
    , HaskellType
    ]

instance HasRandGen CoreType   where { randO = toEnum     <$> nextInt (fromEnum (maxBound::CoreType)) }

objType :: Value o -> CoreType
objType o = case o of
  ONull      -> NullType
  OTrue      -> TrueType
  OType    _ -> TypeType
  OInt     _ -> IntType
  OWord    _ -> WordType
  OLong    _ -> LongType
  OFloat   _ -> FloatType
  ORatio   _ -> RatioType
  OComplex _ -> ComplexType
  OAbsTime _ -> TimeType
  ORelTime _ -> DiffTimeType
  OChar    _ -> CharType
  OString  _ -> StringType
  ORef     _ -> RefType
  OList    _ -> ListType
  OTree    _ -> TreeType
  OBytes   _ -> BytesType
  OHaskell _ -> HaskellType

oBool :: Bool -> Value o
oBool a = if a then OTrue else ONull

----------------------------------------------------------------------------------------------------

-- | A symbol in the type calculus.
data TypeSym
  = CoreType CoreType
    -- ^ used when the type of an object is equal to it's value, for example Null and True,
    -- or in situations where the type of an object has a value, for example the dimentions of a
    -- matrix.
  | TypeVar  Reference [ObjType]
    -- ^ a polymorphic type, like 'AnyType' but has a name.
  deriving (Eq, Ord, Show, Typeable)

instance NFData TypeSym where
  rnf (CoreType a  ) = deepseq a ()
  rnf (TypeVar  a b) = deepseq a $! deepseq b ()

instance HasRandGen TypeSym where
  randO = randChoice [CoreType <$> randO, pure TypeVar <*> randO <*> randList 1 4]

instance PPrintable TypeSym where
  pPrint t = case t of
    CoreType t     -> pPrint t
    TypeVar  t ctx -> pInline $
      concat [[pPrint t], guard (not (null ctx)) >> [pList_ "[" ", " "]" (map pPrint ctx)]]

instance B.Binary TypeSym mtab where
  put o = case o of
    CoreType o       -> B.prefixByte 0x1C $ B.put o
    TypeVar  ref ctx -> B.prefixByte 0x1D $ B.put ref >> B.put ctx
  get = B.word8PrefixTable <|> fail "expecting TypeSym"

instance B.HasPrefixTable TypeSym B.Byte mtab where
  prefixTable =
    B.mkPrefixTableWord8 "TypeSym" 0x1C 0x1D [CoreType <$> B.get, pure TypeVar <*> B.get <*> B.get]

----------------------------------------------------------------------------------------------------

-- | Complex type structures can be programmed by combining 'ObjSimpleType's.
newtype TypeStruct = TypeStruct [TypeSym] deriving (Eq, Ord, Show, Typeable)

instance NFData TypeStruct where { rnf (TypeStruct a) = deepseq a () }

instance HasNullValue TypeStruct where { nullValue = TypeStruct []; testNull (TypeStruct a) = null a; }

instance PPrintable TypeStruct where
  pPrint (TypeStruct tx) = pList (pString "type") "(" ", " ")" (map pPrint tx)

instance B.Binary TypeStruct mtab where
  put (TypeStruct o) = B.prefixByte 0x1B $ B.put o
  get = B.word8PrefixTable <|> fail "expecting TypeStruct"

instance B.HasPrefixTable TypeStruct B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "TypeStruct" 0x1B 0x1B [TypeStruct <$> B.get]

instance HasRandGen TypeStruct where { randO = TypeStruct <$> randList 1 4 }

----------------------------------------------------------------------------------------------------

-- | The fundamental 'Type' used to reason about whether an object is fit to be used for a
-- particular function.
newtype ObjType = ObjType { typeChoices :: [TypeStruct] } deriving (Eq, Ord, Show, Typeable)

instance NFData ObjType where { rnf (ObjType a) = deepseq a () }

instance HasNullValue ObjType where { nullValue = ObjType []; testNull (ObjType a) = null a; }

instance PPrintable ObjType where
  pPrint (ObjType tx) = pList (pString "anyOf") "(" ", " ")" (map pPrint tx)

instance B.Binary ObjType mtab where
  put (ObjType o) = B.prefixByte 0x1A $ B.put o
  get = B.word8PrefixTable <|> fail "expecting ObjType"

instance B.HasPrefixTable ObjType B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "ObjType" 0x1A 0x1A [ObjType <$> B.get]

instance HasRandGen ObjType where { randO = ObjType    <$> randList 1 3 }

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
    c:_  | isAlpha c ->
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

instance NFData Reference where { rnf (Reference  a) = deepseq a () }

instance PPrintable Reference where
  pPrint (Reference r) = pInline $ intercalate [pString "."] (fmap (return . pPrint) r)

instance HasRandGen Reference where { randO = fmap Reference (randList 1 6) }

instance B.Binary Reference mtab where
  put (Reference o) = B.prefixByte 0x1E $ B.put o
  get = B.word8PrefixTable <|> fail "expecting Reference"

instance B.HasPrefixTable Reference B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "Reference" 0x1E 0x1E [Reference <$> B.get]

----------------------------------------------------------------------------------------------------

newtype Complex = Complex (C.Complex Double)
  deriving (Eq, Typeable, Floating, Fractional, Num)

-- | Since 'Object' requires all of it's types instantiate 'Prelude.Ord', I have defined
-- 'Prelude.Ord' of 'Data.Complex.Complex' numbers to be the distance from 0, that is, the radius of
-- the polar form of the 'Data.Complex.Complex' number, ignoring the angle argument.
instance Ord Complex where
  compare (Complex a) (Complex b) = compare (C.polar a) (C.polar b)

instance Show Complex where
  show (Complex a) = "("++show re++(if im<0 then "-" else "+")++show im++"i)" where
    re = C.realPart a
    im = C.imagPart a

instance NFData Complex where { rnf (Complex a) = deepseq a $! () }

instance HasNullValue Complex where
  nullValue = Complex (0 C.:+ 0)
  testNull (Complex c) = C.realPart c == 0 && C.imagPart c == 0

instance B.Binary Complex mtab where
  put o = B.put (realPart o) >> B.put (imagPart o)
  get   = pure complex <*> B.get <*> B.get

instance PPrintable Complex where
  pPrint (Complex (a C.:+ b))
    | a==0.0 && b==0.0 = pString "0i"
    | a==0.0           = pString (show b++"i")
    | b==0.0           = pShow a
    | otherwise        = pInline [pShow a, pString (if b<0 then "-" else "+"), pShow b]

realPart :: Complex -> Double
realPart (Complex o) = C.realPart o

imagPart :: Complex -> Double
imagPart (Complex o) = C.imagPart o

mkPolar :: Double -> Double -> Complex
mkPolar a b = Complex (C.mkPolar a b)

cis :: Double -> Complex
cis = Complex . C.cis

polar :: Complex -> (Double, Double)
polar (Complex o) = C.polar o

magnitude :: Complex -> Double
magnitude (Complex o) = C.magnitude o

phase :: Complex -> Double
phase (Complex o) = C.phase o

conjugate :: Complex -> Complex
conjugate (Complex o) = Complex (C.conjugate o)

complex :: Double -> Double -> Complex
complex a b = Complex (a C.:+ b)

----------------------------------------------------------------------------------------------------

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

-- | The magic number is the first 8 bytes to every 'Document'. It is the ASCII value of the string
-- @"DaoData\0"@.
document_magic_number :: Word64
document_magic_number = 0x44616F4461746100

-- | This is the version number of the line protocol for transmitting document objects.
document_data_version :: Word64
document_data_version = 0

-- | The magic number is the first 8 bytes to every bytecode compiled object program. It is the
-- ASCII value of the string "DaoData\n".
program_magic_number :: Word64
program_magic_number = 0x44616f44617461A

-- | This is the version number of the line protocol for transmitting bytecode compiled program
-- objects.
program_data_version :: Word64
program_data_version = 1

