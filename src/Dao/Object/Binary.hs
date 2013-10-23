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
import           Control.Applicative

import           Data.Monoid
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
  ONull -> return False
  OTrue -> return True
  _     -> fail "expecting boolean object value"

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

typeIDBytePrefix :: CoreType -> Word8
typeIDBytePrefix t = case t of
  NullType     -> 0x05
  TrueType     -> 0x06
--TypeType     -> 0x07
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
--PairType     -> 0x13
  ListType     -> 0x14
--SetType      -> 0x15
--ArrayType    -> 0x16
--IntMapType   -> 0x17
--DictType     -> 0x18
  TreeType     -> 0x19
--GlobType     -> 0x1A
--ScriptType   -> 0x1B
--RuleType     -> 0x1C
  BytesType    -> 0x1D
  HaskellType  -> 0x1E

bytePrefixToTypeID :: Word8 -> Maybe CoreType
bytePrefixToTypeID t = case t of
  0x05 -> Just NullType
  0x06 -> Just TrueType
--0x07 -> Just TypeType
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
--0x13 -> Just PairType
  0x14 -> Just ListType
--0x15 -> Just SetType
--0x16 -> Just ArrayType
--0x17 -> Just IntMapType
--0x18 -> Just DictType
  0x19 -> Just TreeType
--0x1A -> Just GlobType
--0x1B -> Just ScriptType
--0x1C -> Just RuleType
  0x1D -> Just BytesType
  0x1E -> Just HaskellType
  _    -> Nothing

instance Binary CoreType where
  put t = putWord8 (typeIDBytePrefix t)
  get = do
    w <- getWord8
    case bytePrefixToTypeID w of
      Nothing -> fail "was expecting type data"
      Just  w -> return w

-- The binary serialization of 'Dao.Object.Reference' and 'Dao.Object.QualRef' are intricately tied
-- together.
instance Binary Reference where
  put (Reference r) = putWord8 0x31 >> putList r
  get = getWord8 >>= \w -> case w of
    0x31 -> liftM Reference getList
    _    -> fail "expecting reference value"
instance Binary QualRef where
  put q = case q of
    Unqualified r -> put r
    Qualified q r -> do
      let f n = putWord8 n >> put r
      f $ case q of
        LOCAL  -> 0x32
        QTIME  -> 0x33
        GLODOT -> 0x34
        STATIC -> 0x35
        GLOBAL -> 0x36
    ObjRef      o -> put o
  get = do
    w <- lookAhead getWord8
    let f q = getWord8 >> liftM (Qualified q) get
    case w of
      0x31 -> liftM Unqualified get
      0x32 -> f LOCAL
      0x33 -> f QTIME
      0x34 -> f GLODOT
      0x35 -> f STATIC
      0x36 -> f GLOBAL
      _    -> liftM ObjRef get

instance Binary Object where
  put o = do
    let x o p = putWord8 (typeIDBytePrefix (objType o)) >> put p
        px o putx = putWord8 (typeIDBytePrefix (objType o)) >> putx
    case o of
      ONull           -> px ONull (return ())
      OTrue           -> px OTrue (return ())
--    OType         a -> x o a
      OInt          a -> x o a
      OWord         a -> x o a
      OLong         a -> x o a
      OFloat        a -> x o a
      ORatio        a -> x o a
      OComplex      a -> x o a
      OAbsTime      a -> x o a
      ORelTime      a -> x o a
      OChar         a -> x o a
      OString       a -> px o (encodeUStr a)
      ORef          a -> x o a
--    OPair     (a,b) -> px o (put a >> put b)
      OList         a -> px o (putList a)
--    OSet          a -> px o (putList (S.elems a))
--    OArray        a -> px o $
--      let (lo, hi) = bounds a in put lo >> put hi >> putList (elems a)
--    OIntMap       a -> px o (putObjMap I.assocs putVLInt a)
--    ODict         a -> px o (putObjMap M.assocs put a)
      OTree         a -> x o a
--    OGlob         a -> x o a
--    OScript       a -> x o a
      OBytes        a -> x o a
      OHaskell  a ifc -> case objBinaryFormat ifc of
        Just (put, _) -> putLazyByteString (put a)
        Nothing       -> error $ unwords $
          ["no binary format method defied for Haskell type", show (objHaskellType ifc)]
  get = do
    ty <- getWord8
    let x fn = fmap fn get
    case bytePrefixToTypeID ty of
      Nothing -> fail "expecting object, invalid object type prefix"
      Just ty -> case ty of
        NullType     -> return ONull
        TrueType     -> return OTrue
--      TypeType     -> x OType
        IntType      -> x OInt
        WordType     -> x OWord
        LongType     -> x OLong
        FloatType    -> x OFloat
        RatioType    -> x ORatio
        ComplexType  -> x OComplex
        TimeType     -> x OAbsTime
        DiffTimeType -> x ORelTime
        CharType     -> x OChar
        StringType   -> fmap OString decodeUStr
        RefType      -> x ORef
--      PairType     -> fmap OPair (liftM2 (,) get get)
        ListType     -> fmap OList getList
--      SetType      -> fmap (OSet . S.fromList) getList
--      ArrayType    -> do
--        get >>= \lo -> get >>= \hi -> getList >>= \ax ->
--          return (OArray (listArray (lo, hi) ax))
--      IntMapType   -> fmap OIntMap (getObjMap (I.fromList) getFromVLInt)
--      DictType     -> fmap ODict   (getObjMap (M.fromList) get)
        TreeType     -> x OTree
--      GlobType     -> x OGlob
--      ScriptType   -> x OScript
        BytesType    -> x OBytes
        HaskellType  -> error "internal: cannot retrieve Haskell types from a binary stream"

instance Binary RefExpr where
  put (RefExpr r loc) = putWord8 0x88 >> put r >> put loc
  get = getWord8 >>= \w -> case w of
    0x88 -> liftM2 RefExpr get get
    _    -> fail "expecting reference expression"

instance Binary QualRefExpr where
  put r = case r of
    UnqualRefExpr   r -> put r
    QualRefExpr typ r loc ->
      let f n = putWord8 n >> put r >> put loc
      in  case typ of
            LOCAL  -> f 0x8B
            QTIME  -> f 0x8C
            GLODOT -> f 0x8D
            STATIC -> f 0x8E
            GLOBAL -> f 0x8F
  get = do
    w <- lookAhead getWord8
    let f typ = getWord8 >> liftM2 (QualRefExpr typ) get get
    case w of
      0x8B -> f LOCAL
      0x8C -> f QTIME
      0x8D -> f GLODOT
      0x8E -> f STATIC
      0x8F -> f GLOBAL
      _    -> liftM UnqualRefExpr get

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
putNullTermStr nm = mapM_ putWord8 (utf8bytes nm) >> putWord8 0

getNullTermStr :: Get UStr
getNullTermStr = loop [] where
  loop wx = getWord8 >>= \w -> if w==0 then return (upack wx) else loop (wx++[w])

-- There are only 255 non-zero 8-bit prefixes available, I have reused a few here. To make it easier
-- to remember, operators with the same string representation also have the same 8-bit serialization
-- prefix.
instance Binary UpdateOp where
  put a = putWord8 $ case a of
    UCONST -> 0x70
    UADD   -> 0x71
    USUB   -> 0x72
    UMULT  -> 0x73
    UDIV   -> 0x74
    UMOD   -> 0x75
    UPOW   -> 0x76
    UORB   -> 0x77
    UANDB  -> 0x78
    UXORB  -> 0x79
    USHL   -> 0x7A
    USHR   -> 0x7B
    UARROW -> 0x7C
  get = do
    w <- getWord8
    let x = return
    case w of
      0x70 -> x UCONST
      0x71 -> x UADD
      0x72 -> x USUB
      0x73 -> x UMULT
      0x74 -> x UDIV
      0x75 -> x UMOD
      0x76 -> x UPOW
      0x77 -> x UORB
      0x78 -> x UANDB
      0x79 -> x UXORB
      0x7A -> x USHL
      0x7B -> x USHR
      0x7C -> x UARROW
      _    -> fail "expecting update/assignment operator symbol"

instance Binary LValueExpr where
  put (LValueExpr o) = putWord8 0x4D >> put o
  get = getWord8 >>= \w -> case w of
    0x4D -> liftM LValueExpr get
    _    -> fail "expecting L-value expression"

putTyChkExpr :: (a -> Put) -> TyChkExpr a -> Put
putTyChkExpr putfn o = case o of
  NotTypeChecked a       -> putfn a
  TypeChecked    a b loc -> putWord8 0x3A >> putfn a >> put b >> put loc

getTyChkExpr :: Get a -> Get (TyChkExpr a)
getTyChkExpr getfn = lookAhead getWord8 >>= \w -> case w of
  0x3A -> getWord8 >> liftM3 TypeChecked getfn get get
  _    -> liftM NotTypeChecked getfn

instance Binary ParamExpr where
  put (ParamExpr a b loc) = putWord8 (if a then 0x39 else 0x38) >> putTyChkExpr put b >> put loc
  get = getWord8 >>= \w -> let getfn = getTyChkExpr get in case w of
    0x38 -> liftM2 (ParamExpr False) getfn get
    0x39 -> liftM2 (ParamExpr True ) getfn get
    _    -> fail "expecting parameter"

instance Binary ParamListExpr where
  put (ParamListExpr lst loc) = putTyChkExpr putList lst >> put loc
  get = liftM2 ParamListExpr (getTyChkExpr getList) get

instance Binary RuleStrings where
  put (RuleStrings a b) = putList a >> put b
  get = liftM2 RuleStrings getList get

instance Binary OptObjListExpr where
  put (OptObjListExpr a) = maybe (return ()) (\a -> putWord8 0x3B >> put a) a
  get = fmap OptObjListExpr (optional (getWord8 >>= \a -> if a==0x38 then get else mzero))

instance Binary ObjectExpr where
  put o = let x z w fn = putWord8 w >> fn >> put z in case o of
    VoidExpr               -> putWord8 0x40
    Literal        a     z -> x z 0x41 $ put a 
    AssignExpr     a b c z -> x z 0x42 $ put a >> put b >> put c
    Equation       a b c z -> x z 0x43 $ put a >> put b >> put c
    PrefixExpr     a b   z -> x z 0x44 $ put a >> put b
    ArraySubExpr   a b   z -> x z 0x45 $ put a >> put b
    FuncCall       a b   z -> x z 0x46 $ put a >> put b
    InitExpr       a b c z -> x z 0x47 $ put a >> put b >> put c
    StructExpr     a b   z -> x z 0x48 $ put a >> put b
    FuncExpr       a b c z -> x z 0x49 $ put a >> put b >> put c
    RuleExpr       a b   z -> x z 0x4A $ put a >> put b
    MetaEvalExpr   a     z -> x z 0x4B $ put a
    ObjQualRefExpr a       -> put a
    ObjParenExpr   a       -> put a
  get = do
    w <- lookAhead getWord8
    let f a = getWord8 >> a
    case w of
      0x40 -> f $ return VoidExpr
      0x41 -> f $ liftM2 Literal      get         get
      0x42 -> f $ liftM4 AssignExpr   get get get get
      0x43 -> f $ liftM4 Equation     get get get get
      0x44 -> f $ liftM3 PrefixExpr   get get     get
      0x45 -> f $ liftM3 ArraySubExpr get get     get
      0x46 -> f $ liftM3 FuncCall     get get     get
      0x47 -> f $ liftM4 InitExpr     get get get get
      0x48 -> f $ liftM3 StructExpr   get get     get
      0x49 -> f $ liftM4 FuncExpr     get get get get
      0x4A -> f $ liftM3 RuleExpr     get get     get
      0x4B -> f $ liftM2 MetaEvalExpr get         get
      _    -> msum $
        [ liftM ObjQualRefExpr get
        , liftM ObjParenExpr   get
        , fail "expecting object expression"
        ]

instance Binary ParenExpr where
  put (ParenExpr a loc) = putWord8 0x4C >> put a >> put loc
  get = lookAhead getWord8 >>= \a -> case a of
    0x4C -> getWord8 >> liftM2 ParenExpr get get
    _    -> mzero

instance Binary IfExpr where
  put (IfExpr a b loc) = putWord8 0x51 >> put a >> put b >> put loc
  get = getWord8 >>= \w -> case w of
    0x51 -> liftM3 IfExpr get get get
    _    -> fail "expecting if expression"

instance Binary ElseExpr where
  put (ElseExpr a loc) = putWord8 0x52 >> put a >> put loc
  get = getWord8 >>= \w -> case w of
    0x52 -> liftM2 ElseExpr get get
    _    -> fail "expecting else-if expression"

instance Binary IfElseExpr where
  put (IfElseExpr a b c loc) = putWord8 0x53 >> put a >> putList b >> maybe (return ()) put c >> put loc
  get = getWord8 >>= \w -> case w of
    0x53 -> liftM4 IfElseExpr get getList (optional get) get
    _    -> fail "expecting if/else-if/else expression"

instance Binary WhileExpr where
  put (WhileExpr (IfExpr a b loc)) = putWord8 0x54 >> put a >> put b >> put loc
  get = getWord8 >>= \w -> case w of
    0x54 -> liftM3 (\a b c -> WhileExpr (IfExpr a b c)) get get get
    _    -> fail "expecting while expression"

--instance Binary ElseIfExpr where
--  put o = case o of
--    NullElseIfExpr       -> putWord8 0x51
--    ElseExpr     b   loc -> putWord8 0x52 >> put b >> put loc
--    ElseIfExpr o b n loc -> putWord8 0x53 >> put o >> put b >> put n >> put loc
--  get = getWord8 >>= \w -> case w of
--    0x51 -> return NullElseIfExpr
--    0x52 -> liftM2 ElseExpr       get     get
--    0x53 -> liftM4 ElseIfExpr get get get get
--    _    -> fail "expecting if-then-else expression"

instance Binary ScriptExpr where
  put s = case s of
    IfThenElse   a       -> put a
    WhileLoop    a       -> put a
    EvalObject   a     z -> x z 0x55 $ put a
    TryCatch     a b c z -> x z 0x56 $ put a >> maybe (return ()) put b >> maybe (return ()) put c
    ForLoop      a b c z -> x z 0x57 $ put        a >> put b >> put c
    ContinueExpr a b   z -> x z 0x58 $ putObjBool a >> put b
    ReturnExpr   a b   z -> x z 0x59 $ putObjBool a >> put b
    WithDoc      a b   z -> x z 0x5A $ put        a >> put b
    where
      x z i putx = putWord8 i >> putx >> put z
  get = do
    w <- lookAhead getWord8
    let x = getWord8
    case w of
      0x53 -> liftM IfThenElse get
      0x54 -> liftM WhileLoop  get
      0x55 -> x >> liftM2 EvalObject   get                 get
      0x56 -> x >> liftM4 TryCatch     get (optional get) (optional get) get
      0x57 -> x >> liftM4 ForLoop      get         get get get
      0x58 -> x >> liftM3 ContinueExpr getObjBool  get     get
      0x59 -> x >> liftM3 ReturnExpr   getObjBool  get     get
      0x5A -> x >> liftM3 WithDoc      get         get     get

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
  put (CallableCode pat ty exe) = putWord8 0x25 >> put     pat >> put ty >> put exe
  get = getWord8 >>= \w -> case w of
    0x25 -> liftM3 CallableCode get     get get
    _    -> fail "expecting CallableCode"

instance Binary GlobAction where
  put (GlobAction pat exe) = putWord8 0x26 >> putList pat >> put exe
  get = getWord8 >>= \w -> case w of
    0x26 -> liftM2 GlobAction getList get
    _    -> fail "expecting GlobAction"

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

--instance Binary ObjSetOp where
--  put op = putWord8 $ case op of
--    ExactSet  -> 0xC1
--    AnyOfSet  -> 0xC2
--    AllOfSet  -> 0xC3
--    OnlyOneOf -> 0xC4
--    NoneOfSet -> 0xC5
--  get = getWord8 >>= \w -> case w of
--    0xC1 -> return ExactSet
--    0xC2 -> return AnyOfSet
--    0xC3 -> return AllOfSet
--    0xC4 -> return OnlyOneOf
--    0xC5 -> return NoneOfSet
--    _    -> fail "expecting set-logical operator for object pattern"

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
putSegmentWith putA a = maybe err id (mplus plur sing)
  where
    err = error "could not extract data from Dao.EnumSet.Segment dataype"
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

instance Binary ObjListExpr where
  put (ObjListExpr lst _) = putList lst
  get = liftM2 ObjListExpr getList (return LocationUnknown)

instance Binary TopLevelExpr where
  put d = case d of
    Attribute      a b   z -> x 0x61    $ put a >> put b          >> put z
    TopScript      a     z -> x 0x62    $ put a                   >> put z
    EventExpr      a b   z -> x (evt a) $ put b                   >> put z
    where
      x i putx = putWord8 i >> putx
      evt typ = case typ of
        BeginExprType -> 0x63
        EndExprType   -> 0x64
        ExitExprType  -> 0x65
  get = do
    w <- getWord8
    case w of
      0x61 -> liftM3 Attribute      get get get
      0x62 -> liftM2 TopScript      get get
      0x63 -> evtexp BeginExprType
      0x64 -> evtexp EndExprType
      0x65 -> evtexp ExitExprType
      _    -> fail "expecting top-level expression"
      where { evtexp typ = liftM2 (EventExpr typ) get get }

instance Binary RefQualifier where
  put o = putWord8 $ case o of
    LOCAL    -> 0x93
    QTIME    -> 0x94
    GLODOT   -> 0x95
    STATIC   -> 0x96
    GLOBAL   -> 0x97 -- same value as DOT
  get = getWord8 >>= \w -> case w of
    0x93 -> return LOCAL
    0x94 -> return QTIME
    0x95 -> return GLODOT
    0x96 -> return STATIC
    0x97 -> return GLOBAL  -- same value as DOT
    _    -> fail "expecting reference qualifier"

-- There are only 255 non-zero 8-bit prefixes available, I have reused a few here. To make it easier
-- to remember, operators with the same string representation also have the same 8-bit serialization
-- prefix.
instance Binary PrefixOp where
  put o = putWord8 $ case o of
    INVB   -> 0x98
    NOT    -> 0x99
    POSTIV -> 0x9A -- same value as ADD
    NEGTIV -> 0x9B -- same value as SUB
  get = getWord8 >>= \w -> case w of
    0x98 -> return INVB
    0x99 -> return NOT
    0x9A -> return POSTIV -- same value as ADD
    0x9B -> return NEGTIV -- same value as SUB
    _    -> fail "expecting reference prefix operator"

--instance Binary RefInfixOp where
--  put o = putWord8 $ case o of
--    POINT -> 0xA1
--    DOT   -> 0xA2
--  get = getWord8 >>= \w -> case w of
--    0xA1 -> return POINT
--    0xA2 -> return DOT
--    _ -> fail "expecting reference infix operator"

instance Binary InfixOp where
  put o = putWord8 $ case o of
    ADD   -> 0x71
    SUB   -> 0x72
    MULT  -> 0x73
    DIV   -> 0x74
    MOD   -> 0x75
    POW   -> 0x76
    ORB   -> 0x77
    ANDB  -> 0x78
    XORB  -> 0x79
    SHL   -> 0x7A
    SHR   -> 0x7B
    OR    -> 0x7C
    AND   -> 0x7D
    EQUL  -> 0x7E
    NEQUL -> 0x7F
    GTN   -> 0x80
    LTN   -> 0x81
    GTEQ  -> 0x82
    LTEQ  -> 0x83
    ARROW -> 0x84
  get = getWord8 >>= \w -> case w of
    0x71 -> return ADD
    0x72 -> return SUB
    0x73 -> return MULT
    0x74 -> return DIV
    0x75 -> return MOD
    0x76 -> return POW
    0x77 -> return ORB
    0x78 -> return ANDB
    0x79 -> return XORB
    0x7A -> return SHL
    0x7B -> return SHR
    0x7C -> return OR
    0x7D -> return AND
    0x7E -> return EQUL
    0x7F -> return NEQUL
    0x80 -> return GTN
    0x81 -> return LTN
    0x82 -> return GTEQ
    0x83 -> return LTEQ
    0x84 -> return ARROW
    _ -> fail "expecting infix operator"

----------------------------------------------------------------------------------------------------

instance Binary TypeCtx where
  put (TypeCtx a) = put a
  get = fmap TypeCtx get

instance Binary TypeSym where
  put t = case t of
    CoreType t   -> putWord8 0xA1 >> putWord8 (fromIntegral (fromEnum t))
    TypeVar  t s -> putWord8 0xA2 >> put t >> putList s
  get = getWord8 >>= \w -> case w of
    0xA1 -> fmap (CoreType . toEnum . fromIntegral) getWord8
    0xA2 -> liftM2 TypeVar get getList
    _    -> fail "expecting TypeSymbol"

instance Binary TypeStruct where
  put (TypeStruct t) = putWord8 0xA3 >> putList t
  get = lookAhead getWord8 >>= \w -> case w of
    0xA3 -> getWord8 >> fmap TypeStruct getList
    _    -> fail "expecting TypeStruct"

instance Binary ObjType where
  put (ObjType tx) = putWord8 0xA4 >> putList tx
  get = lookAhead getWord8 >>= \w -> case w of
    0xA4 -> getWord8 >> fmap ObjType getList
    _    -> fail "expecting ObjType"

