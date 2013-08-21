-- "src/Dao/Object/Binary.hs"  provides the instantiation of the Dao
-- "Object" data type into the "Data.Binary" class that is exported by
-- the "binary" package.
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

{-# LANGUAGE Rank2Types #-}

module Dao.Object.Binary where

import           Dao.Token
import           Dao.Object
import qualified Dao.Tree as T
import           Dao.Glob
import qualified Dao.EnumSet as Es

import           Control.Monad

import           Data.Maybe
import           Data.Function
import           Data.Typeable
import           Data.Dynamic
import           Data.Word
import           Data.Bits
import           Data.Char
import           Data.Complex
import qualified Data.ByteString.Lazy   as B
import qualified Data.Set               as S
import qualified Data.Map               as M
import qualified Data.IntMap            as I
import qualified Data.IntSet            as IS
import           Data.Digest.SHA1       as SHA1
import           Data.Array.IArray
import           Data.Time hiding (parseTime)

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import Debug.Trace

----------------------------------------------------------------------------------------------------

-- | The magic number is the first 8 bytes to every bytecode compiled object program. It is the
-- ASCII value of the string "DaoData\n".
program_magic_number :: Word64
program_magic_number = 0x44616f44617461A

-- | This is the version number of the line protocol for transmitting bytecode compiled program
-- objects.
program_data_version :: Word64
program_data_version = 0

-- | Take a the last four 'Data.Char.Char's in a string and convert them to a 4-byte
-- 'Data.Word.Word', the earliest 'Data.Char.Char' being in the highest byte, the last character
-- being in the lowest byte.
char4code :: String -> Word32
char4code = foldl (\a b -> shift a 8 .|. fromIntegral (ord b)) 0

byteStringSHA1Sum :: B.ByteString -> B.ByteString
byteStringSHA1Sum bytes =
  let (SHA1.Word160 a b c d e) = SHA1.hash (B.unpack bytes)
      tobytes = reverse . map fromIntegral . take 4 . fix (\f w -> (w.&.0xFF) : f (shift w (0-8)))
  in  B.pack $ concatMap tobytes [a,b,c,d,e]

-- | Returns the string of bytes created by 'Data.Binary.Put.Put' and the checksum of those bytes.
putWithChecksum :: (B.ByteString -> s) -> Put -> PutM (B.ByteString, s)
putWithChecksum checkSum puta = let bx = runPut puta in return (bx, checkSum bx)

getWithChecksum :: Binary a => (B.ByteString -> s) -> Get a -> Get (a, s)
getWithChecksum checkSum geta = do
  (a, count) <- lookAhead (geta >>= \a -> bytesRead >>= \count -> return (a, count))
  bx <- getLazyByteString count
  return (a, checkSum bx)

----------------------------------------------------------------------------------------------------

putObjBool :: Bool -> Put
putObjBool a = if a then put OTrue else put ONull

getObjBool :: Get Bool
getObjBool = lookAhead get >>= \a -> case a of
  a | a==OTrue || a==ONull -> fmap (==OTrue) get
  _ -> fail "expecting boolean object value"

putListWith :: (a -> Put) -> [a] -> Put
putListWith p ax = mapM_ p ax >> putWord8 0x00

getListWith :: Get a -> Get [a]
getListWith getx = loop [] where
  loop ax = do
    w <- lookAhead getWord8
    case w of
      0x00 -> getWord8 >> return ax
      _    -> getx >>= \a -> loop (ax++[a])

putList :: Binary a => [a] -> Put
putList = putListWith put

getList :: Binary a => Get [a]
getList = getListWith get

----------------------------------------------------------------------------------------------------

putMapWith :: (Eq k, Ord k) => (k -> Put) -> (v -> Put) -> M.Map k v -> Put
putMapWith putk putv m = putListWith (\ (a, b) -> putk a >> putv b) (M.assocs m)

getMapWith :: (Eq k, Ord k) => Get k -> Get v -> Get (M.Map k v)
getMapWith getk getv = fmap M.fromList (getListWith (liftM2 (,) getk getv))

putMap :: (Eq k, Ord k, Binary k, Binary v) => M.Map k v -> Put
putMap m = putMapWith put put m

getMap :: (Eq k, Ord k, Binary k, Binary v) => Get (M.Map k v)
getMap = getMapWith get get

putObjMap :: Binary a => (m -> [(a, Object)]) -> (a -> Put) -> m -> Put
putObjMap asocs putIndex o = putListWith (\ (i, o) -> putIndex i >> put o) (asocs o)

getObjMap :: Binary a => ([(a, Object)] -> m) -> Get a -> Get m
getObjMap fromList getIndex = fmap fromList (getListWith (getIndex >>= \i -> get >>= \o -> return (i, o)))

putTreeWith :: (Eq p, Ord p) => (p -> Put) -> (a -> Put) -> T.Tree p a -> Put
putTreeWith putp puta t =
  case t of
    T.Void           -> putWord8 0x21
    T.Leaf       a   -> putWord8 0x22 >> puta a
    T.Branch       t -> putWord8 0x23 >> putMapWith putp (putTreeWith putp puta) t
    T.LeafBranch a t -> putWord8 0x24 >> puta a >> putMapWith putp (putTreeWith putp puta) t

getTreeWith :: (Eq p, Ord p) => Get p -> Get a -> Get (T.Tree p a)
getTreeWith getp geta = do
  t <- getWord8
  case t of
    0x21 -> return T.Void
    0x22 -> geta >>= \a -> return (T.Leaf{T.branchData=a})
    0x23 -> getMapWith getp (getTreeWith getp geta) >>= \t -> return (T.Branch{T.branchMap=t})
    0x24 -> do
      a <- geta
      t <- getMapWith getp (getTreeWith getp geta)
      return (T.LeafBranch{T.branchData=a, T.branchMap=t})
    _    -> fail "corrupted T.Tree data"

instance (Eq p, Ord p, Binary p, Binary a) => Binary (T.Tree p a) where
  put t = putTreeWith put put t
  get   = getTreeWith get get

----------------------------------------------------------------------------------------------------

typeIDBytePrefix :: TypeID -> Word8
typeIDBytePrefix t = case t of
  NullType     -> 0x05
  TrueType     -> 0x06
  TypeType     -> 0x07
  IntType      -> 0x08
  WordType     -> 0x09
  LongType     -> 0x0A
  FloatType    -> 0x0B
  RatioType    -> 0x0C
  ComplexType  -> 0x0D
  TimeType     -> 0x0E
  DiffTimeType -> 0x0F
  CharType     -> 0x10
  StringType   -> 0x11
  RefType      -> 0x12
  PairType     -> 0x13
  ListType     -> 0x14
  SetType      -> 0x15
  ArrayType    -> 0x16
  IntMapType   -> 0x17
  DictType     -> 0x18
  TreeType     -> 0x19
  GlobType     -> 0x1A
  ScriptType   -> 0x1B
  RuleType     -> 0x1C
  BytesType    -> 0x1D

bytePrefixToTypeID :: Word8 -> Maybe TypeID
bytePrefixToTypeID t = case t of
  0x05 -> Just NullType
  0x06 -> Just TrueType
  0x07 -> Just TypeType
  0x08 -> Just IntType
  0x09 -> Just WordType
  0x0A -> Just LongType
  0x0B -> Just FloatType
  0x0C -> Just RatioType
  0x0D -> Just ComplexType
  0x0E -> Just TimeType
  0x0F -> Just DiffTimeType
  0x10 -> Just CharType
  0x11 -> Just StringType
  0x12 -> Just RefType
  0x13 -> Just PairType
  0x14 -> Just ListType
  0x15 -> Just SetType
  0x16 -> Just ArrayType
  0x17 -> Just IntMapType
  0x18 -> Just DictType
  0x19 -> Just TreeType
  0x1A -> Just GlobType
  0x1B -> Just ScriptType
  0x1C -> Just RuleType
  0x1D -> Just BytesType
  _    -> Nothing

instance Binary TypeID where
  put t = putWord8 (typeIDBytePrefix t)
  get = do
    w <- getWord8
    case bytePrefixToTypeID w of
      Nothing -> fail "was expecting type data"
      Just  w -> return w

instance Binary Object where
  put o = do
    let x o p = putWord8 (typeIDBytePrefix (objType o)) >> put p
        px o putx = putWord8 (typeIDBytePrefix (objType o)) >> putx
    case o of
      ONull           -> px ONull (return ())
      OTrue           -> px OTrue (return ())
      OType         a -> x o a
      OInt          a -> x o a
      OWord         a -> x o a
      OLong         a -> x o a
      OFloat        a -> x o a
      ORatio        a -> x o a
      OComplex      a -> x o a
      OTime         a -> x o a
      ODiffTime     a -> x o a
      OChar         a -> x o a
      OString       a -> px o (encodeUStr a)
      ORef          a -> x o a
      OPair     (a,b) -> px o (put a >> put b)
      OList         a -> px o (putList a)
      OSet          a -> px o (putList (S.elems a))
      OArray        a -> px o $
        let (lo, hi) = bounds a in put lo >> put hi >> putList (elems a)
      OIntMap       a -> px o (putObjMap I.assocs putVLInt a)
      ODict         a -> px o (putObjMap M.assocs put a)
      OTree         a -> x o a
      OGlob         a -> x o a
      OScript       a -> x o a
      OBytes        a -> x o a
  get = do
    ty <- getWord8
    let x fn = fmap fn get
    case bytePrefixToTypeID ty of
      Nothing -> fail "expecting object, invalid object type prefix"
      Just ty -> case ty of
        NullType     -> return ONull
        TrueType     -> return OTrue
        TypeType     -> x OType
        IntType      -> x OInt
        WordType     -> x OWord
        LongType     -> x OLong
        FloatType    -> x OFloat
        RatioType    -> x ORatio
        ComplexType  -> x OComplex
        TimeType     -> x OTime
        DiffTimeType -> x ODiffTime
        CharType     -> x OChar
        StringType   -> fmap OString decodeUStr
        RefType      -> x ORef
        PairType     -> fmap OPair (liftM2 (,) get get)
        ListType     -> fmap OList getList
        SetType      -> fmap (OSet . S.fromList) getList
        ArrayType    -> do
          get >>= \lo -> get >>= \hi -> getList >>= \ax ->
            return (OArray (listArray (lo, hi) ax))
        IntMapType   -> fmap OIntMap (getObjMap (I.fromList) getFromVLInt)
        DictType     -> fmap ODict   (getObjMap (M.fromList) get)
        TreeType     -> x OTree
        GlobType     -> x OGlob
        ScriptType   -> x OScript
        BytesType    -> x OBytes

instance Binary Reference where
  put o = case o of
    IntRef     o   -> putWord8 0x81 >> mapM_ put (bitsToVLInt o)
    LocalRef   o   -> x 0x82 o
    QTimeRef   o   -> putWord8 0x83 >> putList o
    StaticRef  o   -> x 0x84 o
    GlobalRef  o   -> putWord8 0x85 >> putList o
    ProgramRef o r -> x 0x86 o >> put r
    FileRef    p o -> x 0x87 p >> putList o
    MetaRef    r   -> putWord8 0x88 >> put r
    Subscript  r s -> putWord8 0x89 >> put r >> put s
    where { x a b = putWord8 a >> encodeUStr b }
  get = getWord8 >>= \w -> case w of
    0x81 -> liftM  IntRef     getFromVLInt
    0x82 -> liftM  LocalRef   decodeUStr
    0x83 -> liftM  QTimeRef   getList
    0x84 -> liftM  StaticRef  decodeUStr
    0x85 -> liftM  GlobalRef  getList
    0x86 -> liftM2 ProgramRef decodeUStr get
    0x87 -> liftM2 FileRef    decodeUStr getList
    0x88 -> liftM  MetaRef    get
    0x89 -> liftM2 Subscript  get get
    _ -> fail "expecting reference expression"

instance Binary UTCTime where
  put t = do
    put (toModifiedJulianDay (utctDay t))
    put (toRational (utctDayTime t))
  get = do
    d <- fmap ModifiedJulianDay get
    t <- fmap fromRational get
    return (UTCTime{ utctDay = d, utctDayTime = t })

instance Binary NominalDiffTime where
  put t = put (toRational t)
  get = fmap fromRational get

instance (Binary a, RealFloat a) => Binary (Complex a) where
  put o = put (realPart o) >> put (imagPart o)
  get = liftM2 (:+) get get

----------------------------------------------------------------------------------------------------

instance Binary GlobUnit where
  put p = case p of
    Wildcard -> putWord8 0x29
    AnyOne   -> putWord8 0x2A
    Single o -> putWord8 0x2B >> put o
  get = getWord8 >>= \w -> case w of
    0x29 -> return Wildcard
    0x2A -> return AnyOne
    0x2B -> fmap Single get
    _    -> fail "expecting pattern unit object"

instance Binary Glob where
  put p = putList (getPatUnits p)
  get   = getList >>= \px -> return $
    Glob
    { getPatUnits = px
    , getGlobLength = length px
    }

----------------------------------------------------------------------------------------------------

putNullTermStr :: Name -> Put
putNullTermStr nm = mapM_ putWord8 (uwords nm) >> putWord8 0

getNullTermStr :: Get UStr
getNullTermStr = loop [] where
  loop wx = getWord8 >>= \w -> if w==0 then return (upack wx) else loop (wx++[w])

instance Binary UpdateOp where
  put a = putWord8 $ case a of
    UCONST -> 0x71
    UADD   -> 0x72
    USUB   -> 0x73
    UMULT  -> 0x74
    UDIV   -> 0x75
    UMOD   -> 0x76
    UORB   -> 0x77
    UANDB  -> 0x78
    UXORB  -> 0x79
    USHL   -> 0x7A
    USHR   -> 0x7B
  get = do
    w <- getWord8
    let x = return
    case w of
      0x71 -> x UCONST
      0x72 -> x UADD
      0x73 -> x USUB
      0x74 -> x UMULT
      0x75 -> x UDIV
      0x76 -> x UMOD
      0x77 -> x UORB
      0x78 -> x UANDB
      0x79 -> x UXORB
      0x7A -> x USHL
      0x7B -> x USHR
      _    -> fail "expecting update/assignment operator symbol"

instance Binary ObjectExpr where
  put o = case o of
    VoidExpr             -> putWord8 0x40
    Literal      a     z -> x z 0x41 $ put a
    AssignExpr   a b c z -> x z 0x42 $ put a >> put b >> put c
    Equation     a b c z -> x z 0x43 $ put a >> put b >> put c
    PrefixExpr   a b   z -> x z 0x44 $ put a >> put b
    ParenExpr    a     z -> x z 0x45 $ put a
    ArraySubExpr a b   z -> x z 0x46 $ put a >> put b
    FuncCall     a b   z -> x z 0x47 $ put a >> put b
    DictExpr     a b   z -> x z 0x48 $ put a >> put b
    ArrayExpr    a b   z -> x z 0x49 $ put a >> put b
    StructExpr   a b   z -> x z 0x4A $ put a >> put b
    DataExpr     a     z -> x z 0x4B $ put a
    LambdaExpr   a b c z -> x z (lamexp a) $ put  b >> put c
    MetaEvalExpr a     z -> x z 0x4F $ put a
    where
      x z i putx  = putWord8 i >> putx >> put z
      lamexp t = case t of
        FuncExprType -> 0x4C
        RuleExprType -> 0x4D
        PatExprType  -> 0x4E
  get = do
    w <- getWord8
    case w of
      0x40 -> return VoidExpr
      0x41 -> liftM2 Literal      get          get
      0x42 -> liftM4 AssignExpr   get get  get get
      0x43 -> liftM4 Equation     get get  get get
      0x44 -> liftM3 PrefixExpr   get get      get
      0x45 -> liftM2 ParenExpr    get          get
      0x46 -> liftM3 ArraySubExpr get get      get
      0x47 -> liftM3 FuncCall     get get      get
      0x48 -> liftM3 DictExpr     get get      get
      0x49 -> liftM3 ArrayExpr    get get      get
      0x4A -> liftM3 StructExpr   get get      get
      0x4B -> liftM2 DataExpr     get          get
      0x4C -> lamexp FuncExprType
      0x4D -> lamexp RuleExprType
      0x4E -> lamexp PatExprType
      0x4F -> liftM2 MetaEvalExpr  get                get
      _    -> fail "expecting object expression"
      where { lamexp typ = liftM3 (LambdaExpr typ) get get get }

instance Binary ScriptExpr where
  put s = case s of
    EvalObject   a     z -> x z 0x51 $ put        a
    IfThenElse   a b c z -> x z 0x52 $ put        a >> put b >> put c
    TryCatch     a b c z -> x z 0x53 $ put        a >> put b >> put c
    ForLoop      a b c z -> x z 0x54 $ put        a >> put b >> put c
    WhileLoop    a b   z -> x z 0x55 $ put        a >> put b
    ContinueExpr a b   z -> x z 0x56 $ putObjBool a >> put b
    ReturnExpr   a b   z -> x z 0x57 $ putObjBool a >> put b
    WithDoc      a b   z -> x z 0x58 $ put        a >> put b
    where
      x z i putx = putWord8 i >> putx >> put z
  get = do
    w <- getWord8
    case w of
      0x51 -> liftM2 EvalObject   get                 get
      0x52 -> liftM4 IfThenElse   get         get get get
      0x53 -> liftM4 TryCatch     get         get get get
      0x54 -> liftM4 ForLoop      get         get get get
      0x55 -> liftM3 WhileLoop    get         get     get
      0x56 -> liftM3 ContinueExpr getObjBool  get     get
      0x57 -> liftM3 ReturnExpr   getObjBool  get     get
      0x58 -> liftM3 WithDoc      get         get     get
      _    -> fail "expecting script expression"

instance Binary Location where
  put loc = case loc of
    LocationUnknown -> return ()
    loc             -> do
      putWord8 0x5F
      let fn acc = mapM_ putWord8 (bitsToVLInt (acc loc))
      fn startingLine >> fn startingColumn
      fn endingLine   >> fn endingColumn
  get = do
    is_empty <- isEmpty
    if is_empty
      then return LocationUnknown
      else do
        w <- lookAhead getWord8
        if w==0x5F
          then do
            getWord8
            a <- getFromVLInt
            b <- getFromVLInt
            c <- getFromVLInt
            d <- getFromVLInt
            return (Location a b c d)
          else return LocationUnknown

----------------------------------------------------------------------------------------------------

instance Binary CallableCode where
  put sub = case sub of
    CallableCode pat exe -> putWord8 0x25 >> putList pat >> put exe
    GlobAction pat exe -> putWord8 0x26 >> putList pat >> put exe
  get = getWord8 >>= \w -> case w of
    0x25 -> liftM2 CallableCode getList get
    0x26 -> liftM2 GlobAction getList get

instance Binary CodeBlock where
  put = putList . codeBlock
  get = fmap CodeBlock getList

instance Binary Subroutine where
  put = put . origSourceCode
  get = do
    code <- get
    let msg = "Subroutine retrieved from binary used before being initialized."
    return $
      Subroutine
      { origSourceCode = code
      , staticVars     = error msg
      , executable     = error msg
      }

-- Used by the parser for 'CallableCode', needs to check if it's arguments are 'Dao.Object.Pattern's
-- or 'Dao.Glob.Globs', so it is necessary to report whether or not a byte is a valid prefix for a
-- 'Dao.Object.Pattern' expression.
nextIsPattern :: Get Bool
nextIsPattern = do
  empty <- isEmpty
  if empty then return False else fmap (\c -> 0xB1<=c && c<=0xBE) (lookAhead getWord8)

instance Binary Pattern where
  put a = case a of
    ObjAnyX        -> x 0xB1
    ObjMany        -> x 0xB2
    ObjAny1        -> x 0xB3
    ObjEQ      a   -> x 0xB4 >> put                a
    ObjType    a   -> x 0xB5 >> putEnumSetWith put a
    ObjBounded a b -> x 0xB6 >> putEnumInfWith put a >> putEnumInfWith put             b
    ObjList    a b -> x 0xB7 >> put                a >> putList                        b
    ObjNameSet a b -> x 0xB8 >> put                a >> putList              ( S.elems b)
    ObjIntSet  a b -> x 0xB9 >> put                a >> putListWith putVLInt (IS.elems b)
    ObjElemSet a b -> x 0xBA >> put                a >> putList              ( S.elems b)
    ObjChoice  a b -> x 0xBB >> put                a >> putList              ( S.elems b)
    ObjLabel   a b -> x 0xBC >> put                a >> put                            b
    ObjFailIf  a b -> x 0xBD >> put                a >> put                            b
    ObjNot     a   -> x 0xBE >> put                a
    where { x = putWord8 }
  get   = getWord8 >>= \w -> case w of
    0xB1 -> return ObjAnyX
    0xB2 -> return ObjMany
    0xB3 -> return ObjAny1
    0xB4 -> liftM  ObjEQ       get
    0xB5 -> liftM  ObjType    (getEnumSetWith get)
    0xB6 -> liftM2 ObjBounded (getEnumInfWith get) (getEnumInfWith    get    )
    0xB7 -> liftM2 ObjList     get                  getList
    0xB8 -> liftM2 ObjNameSet  get                 (fmap S.fromList   getList)
    0xB9 -> liftM2 ObjIntSet   get                 (fmap IS.fromList (getListWith getFromVLInt))
    0xBA -> liftM2 ObjElemSet  get                 (fmap S.fromList   getList)
    0xBB -> liftM2 ObjChoice   get                 (fmap S.fromList   getList)
    0xBC -> liftM2 ObjLabel    get                  get
    0xBD -> liftM2 ObjFailIf   get                  get
    0xBE -> liftM  ObjNot      get
    _    -> fail "expecting object-pattern expression"

instance Binary ObjSetOp where
  put op = putWord8 $ case op of
    ExactSet  -> 0xC1
    AnyOfSet  -> 0xC2
    AllOfSet  -> 0xC3
    OnlyOneOf -> 0xC4
    NoneOfSet -> 0xC5
  get = getWord8 >>= \w -> case w of
    0xC1 -> return ExactSet
    0xC2 -> return AnyOfSet
    0xC3 -> return AllOfSet
    0xC4 -> return OnlyOneOf
    0xC5 -> return NoneOfSet
    _    -> fail "expecting set-logical operator for object pattern"

putEnumInfWith :: (a -> Put) -> Es.Inf a -> Put
putEnumInfWith putx a = case a of
  Es.NegInf  -> putWord8 0xC6
  Es.PosInf  -> putWord8 0xC7
  Es.Point a -> putWord8 0xC8 >> putx a

getEnumInfWith :: Get a -> Get (Es.Inf a)
getEnumInfWith getx = getWord8 >>= \w -> case w of
  0xC6 -> return Es.NegInf
  0xC7 -> return Es.PosInf
  0xC8 -> liftM  Es.Point  getx
  _    -> fail "expecting enum-inf data"

instance (Integral a, Bits a) => Binary (Es.Inf a) where
  put = putEnumInfWith putVLInt
  get = getEnumInfWith getFromVLInt

putSegmentWith :: (a -> Put) -> Es.Segment a -> Put
putSegmentWith putA a = flip fromMaybe (mplus plur sing) $
  error "could not extract data from Dao.EnumSet.Segment dataype"
  where
    plur = do
      (lo, hi) <- Es.plural a
      return (putWord8 0xCA >> putEnumInfWith putA lo >> putEnumInfWith putA hi)
    sing = do
      point <- Es.singular a
      return (putWord8 0xC9 >> putEnumInfWith putA point)

getSegmentWith :: (Enum a, Ord a, Es.InfBound a) => Get a -> Get (Es.Segment a)
getSegmentWith getA = getWord8 >>= \w -> case w of
  0xC9 -> getEnumInfWith getA >>= \a -> return (Es.enumInfSeg a a)
  0xCA -> liftM2 Es.enumInfSeg (getEnumInfWith getA) (getEnumInfWith getA)
  _    -> fail "expecting enum-segment expression"

instance (Enum a, Ord a, Es.InfBound a, Integral a, Bits a) => Binary (Es.Segment a) where
  put = putSegmentWith putVLInt
  get = getSegmentWith getFromVLInt

putEnumSetWith :: (Ord a, Enum a, Es.InfBound a) => (a -> Put) -> Es.Set a -> Put
putEnumSetWith putA s = putListWith (putSegmentWith putA) (Es.toList s)

getEnumSetWith :: (Enum a, Ord a, Es.InfBound a) => Get a -> Get (Es.Set a)
getEnumSetWith getA = fmap Es.fromList (getListWith (getSegmentWith getA))

get_binary_enumset_error = error "invalid object set from binary stream (overlapping items)"

instance (Es.InfBound a, Integral a, Bits a) => Binary (Es.Set a) where
  put = putEnumSetWith putVLInt
  get = getEnumSetWith getFromVLInt

instance Binary TopLevelExpr where
  put d = case d of
    Attribute      a b   z -> x 0x61    $ put a >> put b          >> put z
    TopFunc        a b c z -> x 0x62    $ put a >> put b >> put c >> put z
    TopScript      a     z -> x 0x63    $ put a                   >> put z
    TopLambdaExpr  a b c z -> x (top a) $ put b >> put c          >> put z
    EventExpr      a b   z -> x (evt a) $ put b                   >> put z
    where
      x i putx = putWord8 i >> putx
      top typ = case typ of
        FuncExprType  -> 0x64
        RuleExprType  -> 0x65
        PatExprType   -> 0x66
      evt typ = case typ of
        BeginExprType -> 0x67
        EndExprType   -> 0x68
        ExitExprType  -> 0x69
  get = do
    w <- getWord8
    case w of
      0x61 -> liftM3 Attribute      get get get
      0x62 -> liftM4 TopFunc        get get get get
      0x63 -> liftM2 TopScript      get get
      0x64 -> toplam FuncExprType
      0x65 -> toplam RuleExprType
      0x66 -> toplam PatExprType
      0x67 -> evtexp BeginExprType
      0x68 -> evtexp EndExprType
      0x69 -> evtexp ExitExprType
      _    -> fail "expecting top-level expression"
      where
        toplam typ = liftM3 (TopLambdaExpr typ) get get get
        evtexp typ = liftM2 (EventExpr     typ) get get

-- There are only 255 non-zero 8-bit prefixes available, I have reused a few here. To make it easier
-- to remember, operators with the same string representation also have the same 8-bit serialization
-- prefix.
instance Binary PrefixOp where
  put o = putWord8 $ case o of
    REF       -> 0x91
    DEREF     -> 0x92
    INVB      -> 0x93
    NOT       -> 0x94
    GLOBALPFX -> 0x95
    LOCALPFX  -> 0x96
    QTIMEPFX  -> 0x97
    STATICPFX -> 0x98
    POSTIV    -> 0x9A -- same value as ADD
    NEGTIV    -> 0x9B -- same value as SUB
    GLDOT     -> 0xA2 -- same value as DOT
  get = getWord8 >>= \w -> case w of
    0x91 -> return REF
    0x92 -> return DEREF
    0x93 -> return INVB
    0x94 -> return NOT
    0x95 -> return GLOBALPFX
    0x96 -> return LOCALPFX
    0x97 -> return QTIMEPFX
    0x98 -> return STATICPFX
    0x9A -> return POSTIV -- same value as ADD
    0x9B -> return NEGTIV -- same value as SUB
    0xA2 -> return GLDOT  -- same value as DOT
    _    -> fail "expecting prefix operator"

instance Binary InfixOp where
  put o = putWord8 $ case o of
    ADD   -> 0x9A
    SUB   -> 0x9B
    MULT  -> 0x9C
    DIV   -> 0x9D
    MOD   -> 0x9E
    POW   -> 0x9F
    POINT -> 0xA1
    DOT   -> 0xA2
    OR    -> 0xA3
    AND   -> 0xA4
    EQUL  -> 0xA5
    NEQUL -> 0xA6
    ORB   -> 0xA7
    ANDB  -> 0xA8
    XORB  -> 0xA9
    SHL   -> 0xAA
    SHR   -> 0xAB
  get = getWord8 >>= \w -> case w of
    0x9A -> return ADD
    0x9B -> return SUB
    0x9C -> return MULT
    0x9D -> return DIV
    0x9E -> return MOD
    0x9F -> return POW
    0xA1 -> return POINT
    0xA2 -> return DOT
    0xA3 -> return OR
    0xA4 -> return AND
    0xA5 -> return EQUL
    0xA6 -> return NEQUL
    0xA7 -> return ORB
    0xA8 -> return ANDB
    0xA9 -> return XORB
    0xAA -> return SHL
    0xAB -> return SHR
    _ -> fail "expecting infix operator"

