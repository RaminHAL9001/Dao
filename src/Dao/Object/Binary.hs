-- "src/Dao/Object/Binary.hs"  provides the instantiation of the Dao
-- "Object" data type into the "Data.Binary" class that is exported by
-- the "binary" package.
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

{-# LANGUAGE Rank2Types #-}

module Dao.Object.Binary where

import           Dao.Token
import           Dao.Object
import qualified Dao.Tree as T
import           Dao.Pattern

import           Control.Monad

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

putObjMap :: Binary a => (m -> [(a, Object)]) -> m -> Put
putObjMap asocs o = putListWith (\ (i, o) -> put i >> put o) (asocs o)

getObjMap :: Binary a => ([(a, Object)] -> m) -> Get m
getObjMap fromList = fmap fromList (getListWith (get >>= \i -> get >>= \o -> return (i, o)))

putTreeWith :: (Eq p, Ord p) => (p -> Put) -> (a -> Put) -> T.Tree p a -> Put
putTreeWith putp puta t =
  case t of
    T.Void           -> putWord8 0x21
    T.Leaf       a   -> putWord8 0x22 >> puta a
    T.Branch       t -> putWord8 0x23 >> putMapWith putp (putTreeWith putp puta) t
    T.LeafBranch a t -> putWord8 0x24 >> puta a >> putMapWith putp (putTreeWith putp puta) t

getTreeWith :: (Eq p, Ord p, Show p, Show a) => Get p -> Get a -> Get (T.Tree p a)
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
    _    -> error "corrupted T.Tree data"

instance (Eq p, Ord p, Binary p, Binary a, Show p, Show a) => Binary (T.Tree p a) where
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
  LongType     -> 0x0a
  FloatType    -> 0x0b
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
  PatternType  -> 0x1A
  ScriptType   -> 0x1B
  RuleType     -> 0x1C
  BytesType    -> 0x1D

bytePrefixToTypeID :: Word8 -> TypeID
bytePrefixToTypeID t = case t of
  0x05 ->     NullType
  0x06 ->     TrueType
  0x07 ->     TypeType
  0x08 ->      IntType
  0x09 ->     WordType
  0x0a ->     LongType
  0x0b ->    FloatType
  0x0C ->    RatioType
  0x0D ->  ComplexType
  0x0E ->     TimeType
  0x0F -> DiffTimeType
  0x10 ->     CharType
  0x11 ->   StringType
  0x12 ->      RefType
  0x13 ->     PairType
  0x14 ->     ListType
  0x15 ->      SetType
  0x16 ->    ArrayType
  0x17 ->   IntMapType
  0x18 ->     DictType
  0x19 ->     TreeType
  0x1A ->  PatternType
  0x1B ->   ScriptType
  0x1C ->     RuleType
  0x1D ->    BytesType

instance Binary TypeID where
  put t = putWord8 (typeIDBytePrefix t)
  get = do
    w <- getWord8
    if 0x05<=w && w<=0x1D
      then return (bytePrefixToTypeID w)
      else fail "was expecting type data"

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
      OIntMap       a -> px o (putObjMap I.assocs a)
      ODict         a -> px o (putObjMap M.assocs a)
      OTree         a -> x o a
      OPattern      a -> px o (put a)
      OScript       a -> px o (put a)
      ORule         a -> px o (put a)
      OBytes        a -> x o a
  get = do
    ty <- getWord8
    let x fn = fmap fn get
    case bytePrefixToTypeID ty of
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
      PairType     -> get >>= \a -> get >>= \b -> return (OPair (a, b))
      ListType     -> fmap OList getList
      SetType      -> fmap (OSet . S.fromList) getList
      ArrayType    -> do
        get >>= \lo -> get >>= \hi -> getList >>= \ax ->
          return (OArray (listArray (lo, hi) ax))
      IntMapType   -> fmap OIntMap (getObjMap (I.fromList))
      DictType     -> fmap ODict   (getObjMap (M.fromList))
      TreeType     -> x OTree
      PatternType  -> x OPattern
      ScriptType   -> x OScript
      RuleType     -> x ORule
      BytesType    -> x OBytes

instance Binary Reference where
  put o = case o of
    IntRef     o   -> putWord8 0x81 >> put (bitsToVLInt o)
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
    _ -> error "corrupted pattern in Reference value"

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

instance Binary PatUnit where
  put p = case p of
    Wildcard -> putWord8 0x29
    AnyOne   -> putWord8 0x2A
    Single o -> putWord8 0x2B >> put o
  get = getWord8 >>= \w -> case w of
    0x29 -> return Wildcard
    0x2A -> return AnyOne
    0x2B -> fmap Single get
    _    -> error "corrupted Pattern object in binary file"

instance Binary Pattern where
  put p = putList (getPatUnits p)
  get   = getList >>= \px -> return $
    Pattern
    { getPatUnits = px
    , getPatternLength = length px
    }

----------------------------------------------------------------------------------------------------

putNullTermStr :: Name -> Put
putNullTermStr nm = mapM_ putWord8 (uwords nm) >> putWord8 0

getNullTermStr :: Get UStr
getNullTermStr = loop [] where
  loop wx = getWord8 >>= \w -> if w==0 then return (upack wx) else loop (wx++[w])

putCommentList :: [Comment] -> Put
putCommentList comx = flip putListWith comx $ \com ->
  case com of
    InlineComment  com -> putWord8 0x31 >> put com
    EndlineComment com -> putWord8 0x32 >> put com

getCommentList :: Get [Comment]
getCommentList = getListWith $ do
  w <- getWord8
  case w of
    0x31 -> fmap InlineComment  get
    0x32 -> fmap EndlineComment get
    _    -> error "expecting comment string"

putComWith :: (a -> Put) -> Com a -> Put
putComWith p com = case com of
  Com          a    -> p a
  ComBefore c1 a    -> putWord8 0x39 >> putCommentList c1 >> p a
  ComAfter     a c2 -> putWord8 0x3A >> p a >> putCommentList c2
  ComAround c1 a c2 -> putWord8 0x3B >> putCommentList c1 >> p a >> putCommentList c2

getComWith :: Get a -> Get (Com a)
getComWith getx = do
  let g = getWord8
  w <- lookAhead g
  case w of
    0x39 -> g >> liftM2 ComBefore getCommentList getx
    0x3A -> g >> liftM2 ComAfter                 getx getCommentList
    0x3B -> g >> liftM3 ComAround getCommentList getx getCommentList
    _    ->      liftM  Com                      getx

putCom :: Binary a => Com a -> Put
putCom c = putComWith put c

getCom :: Binary a => Get (Com a)
getCom = getComWith get

putComListWith :: (a -> Put) -> [Com a] -> Put
putComListWith fn = putListWith (putComWith fn)

getComListWith :: Binary a => Get a -> Get [Com a]
getComListWith fn = getListWith (getComWith fn)

putComList :: Binary a => [Com a] -> Put
putComList ax = putComListWith put ax

getComList :: Binary a => Get [Com a]
getComList = getComListWith get

getComComList :: Binary a => Get (Com [Com a])
getComComList = getComWith getComList

putComComList :: Binary a => Com [Com a] -> Put
putComComList = putComWith putComList

----------------------------------------------------------------------------------------------------

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

instance Binary ArithOp1 where
  put a = error "Binary instantiation of ArithOp1 not yet implemented"
  get   = error "Binary instantiation of ArithOp1 not yet implemented"

instance Binary ArithOp2 where
  put a = error "Binary instantiation of ArithOp2 not yet implemented"
  get   = error "Binary instantiation of ArithOp2 not yet implemented"

instance Binary ObjectExpr where
  put o = case o of
    VoidExpr             -> putWord8 0x40
    Literal      a     z -> x z 0x41 $ put            a
    AssignExpr   a b c z -> x z 0x42 $ put            a >> putCom         b >> put        c
    Equation     a b c z -> x z 0x43 $ put            a >> putCom         b >> put        c
    PrefixExpr   a b   z -> x z 0x44 $ put            a >> putCom         b
    ParenExpr    a b   z -> x z 0x45 $ putObjBool     a >> putCom         b
    ArraySubExpr a b c z -> x z 0x46 $ put            a >> putCommentList b >> putCom     c
    FuncCall     a b c z -> x z 0x47 $ put            a >> putCommentList b >> putComList c
    DictExpr     a b c z -> x z 0x48 $ put            a >> putCommentList b >> putComList c
    ArrayExpr    a b   z -> x z 0x49 $ putComComList  a >> putComList     b
    StructExpr   a b   z -> x z 0x4A $ putCom         a >> putComList     b
    DataExpr     a b   z -> x z 0x4B $ putCommentList a >> putComList     b
    LambdaExpr   a b c z -> x z (lamexp a) $               putComComList  b >> putComList c
    MetaEvalExpr a     z -> x z 0x4F $ putCom a
    where
      x z i putx  = putWord8 i >> put z >> putx
      lamexp t = case t of
        FuncExprType -> 0x4C
        RuleExprType -> 0x4D
        PatExprType  -> 0x4E
  get = do
    w <- getWord8
    case w of
      0x40 -> return  VoidExpr
      0x41 -> liftM2 Literal      get                                       get
      0x42 -> liftM4 AssignExpr   get            getCom          get        get
      0x43 -> liftM4 Equation     get            getCom          get        get
      0x44 -> liftM3 PrefixExpr   get            getCom                     get
      0x45 -> liftM3 ParenExpr    getObjBool     getCom                     get
      0x46 -> liftM4 ArraySubExpr get            getCommentList  getCom     get
      0x47 -> liftM4 FuncCall     get            getCommentList  getComList get
      0x48 -> liftM4 DictExpr     get            getCommentList  getComList get
      0x49 -> liftM3 ArrayExpr    getComComList  getComList                 get
      0x4A -> liftM3 StructExpr   getCom         getComList                 get
      0x4B -> liftM3 DataExpr     getCommentList getComList                 get
      0x4C -> lamexp FuncExprType
      0x4D -> lamexp RuleExprType
      0x4E -> lamexp PatExprType
      0x4F -> liftM2 MetaEvalExpr getCom                                    get
      _    -> error "could not load, corrupted data in object expression"
      where
        lamexp typ = liftM3 (LambdaExpr typ) getComComList getComList get

instance Binary ScriptExpr where
  put s = case s of
    EvalObject   a b     z -> x z 0x51 $ put            a >> putCommentList b
    IfThenElse   a b c d z -> x z 0x52 $ putCommentList a >> put            b >> putComComList c >> putComComList d
    TryCatch     a b c   z -> x z 0x53 $ putComComList  a >> putCom         b >> putComList    c
    ForLoop      a b c   z -> x z 0x54 $ putCom         a >> putCom         b >> putComList    c
    WhileLoop    a b     z -> x z 0x55 $ putCom         a >> putComList     b                 
    ContinueExpr a b c   z -> x z 0x56 $ putObjBool     a >> putCommentList b >> putCom        c
    ReturnExpr   a b     z -> x z 0x57 $ putObjBool     a >> putCom         b
    WithDoc      a b     z -> x z 0x58 $ putCom         a >> putComList     b
    where
      x z i putx = putWord8 i >> putx >> put z
  get = do
    w <- getWord8
    case w of
      0x51 -> liftM3 EvalObject   get            getCommentList                              get
      0x52 -> liftM5 IfThenElse   getCommentList get            getComComList getComComList  get
      0x53 -> liftM4 TryCatch     getComComList  getCom         getComList                   get
      0x54 -> liftM4 ForLoop      getCom         getCom         getComList                   get
      0x55 -> liftM3 WhileLoop    getCom         getComList                                  get
      0x56 -> liftM4 ContinueExpr getObjBool     getCommentList getCom                       get
      0x57 -> liftM3 ReturnExpr   getObjBool     getCom                                      get
      0x58 -> liftM3 WithDoc      getCom         getComList                                  get
      _    -> error "could not load, script data is corrupted"

instance Binary Location where
  put loc = case loc of
    LocationUnknown -> putWord8 0x5E
    loc             -> do
      putWord8 0x5F
      fn startingLine loc >> fn startingChar loc >> fn startingColumn loc
      fn endingLine   loc >> fn endingChar   loc >> fn endingColumn   loc
      where { fn acc a = mapM_ putWord8 (bitsToVLInt (acc a)) }
  get = getWord8 >>= \w -> case w of
    0x5E -> return LocationUnknown
    0x5F -> do
      a <- getFromVLInt
      b <- getFromVLInt
      c <- getFromVLInt
      d <- getFromVLInt
      e <- getFromVLInt
      f <- getFromVLInt
      return (Location a b c d e f)
    _    -> error "could not load, location data is corrupted"

----------------------------------------------------------------------------------------------------

instance Binary Rule where
  put r = putList (rulePattern r) >> putComList (ruleAction r)
  get   = liftM2 (\a b -> Rule VoidExpr a b err) getList getComList where
    err = error "rule loaded from binary file is used before being converted to an executable"

instance Binary Subroutine where
  put s = putList (argsPattern s) >> putComList (subSourceCode s)
  get   = liftM3 Subroutine getList getComList (return (Executable err (return ()))) where
    err = error "subroutine loaded from binary file is used before being converted to an executable"

instance Binary ObjPat where
  put s = error "TODO: define binary serializer for ObjPat"
  get   = getWord8 >>= getObjPat

-- This function is external to the instantation of Binary ObjPat because it is used by the
-- instantiation of Object as well.
getObjPat :: Word8 -> Get ObjPat
getObjPat = error "TODO: define binary decoder for ObjPat"

instance Binary TopLevelExpr where
  put d = case d of
    Attribute      a b   lc -> x 0x61 (putCom               a >> putCom     b                    >> put lc)
    ToplevelDefine a b   lc -> x 0x62 ((putComWith putList) a >> putCom     b                    >> put lc)
    ToplevelFunc   a b c lc -> x 0x63 (putCom               a >> putComList b >> putComComList c >> put lc)
    ToplevelScript a     lc -> x 0x64 (put                  a                                    >> put lc)
    TopLambdaExpr  a b c lc -> x (top a) (putComComList     b >> putComList c                    >> put lc)
    EventExpr      a b   lc -> x (evt a) (putComComList     b                                    >> put lc)
    where
      x i putx = putWord8 i >> putx
      top typ = case typ of
        FuncExprType  -> 0x65
        RuleExprType  -> 0x66
        PatExprType   -> 0x67
      evt typ = case typ of
        BeginExprType -> 0x68
        EndExprType   -> 0x69
        ExitExprType  -> 0x6A
  get = do
    w <- getWord8
    case w of
      0x61 -> liftM3 Attribute      getCom               getCom                   get
      0x62 -> liftM3 ToplevelDefine (getComWith getList) getCom                   get
      0x63 -> liftM4 ToplevelFunc   getCom               getComList getComComList get
      0x64 -> liftM2 ToplevelScript get                                           get
      0x65 -> toplam FuncExprType
      0x66 -> toplam RuleExprType
      0x67 -> toplam PatExprType
      0x68 -> evtexp BeginExprType
      0x69 -> evtexp EndExprType
      0x6A -> evtexp ExitExprType
      _    -> error "failed decoding binary data for top-level expression"
      where
        toplam typ = liftM3 (TopLambdaExpr typ) getComComList getComList get
        evtexp typ = liftM2 (EventExpr     typ) getComComList            get

instance Binary SourceCode where
  put sc = do
    (bx, cksum) <- putWithChecksum byteStringSHA1Sum $ do
      putWord64be program_magic_number
      putWord64be program_data_version
      putCom (sourceModuleName sc)
      putComComList (directives sc)
    putLazyByteString bx
    put cksum
  get = do
    let chk msg a = get >>= \b -> if b==a then return () else error ("failed reading binary, "++msg)
    (sc, myCksum) <- getWithChecksum byteStringSHA1Sum $ do
      chk "wrong \"magic\" number, this may not be a Dao compiled program" $
        program_magic_number
      chk "this program was compiled with an incompatible version of the Dao binary protocal" $
        program_data_version
      liftM2 (SourceCode 0 nil) getCom getComComList
    theirCksum <- fmap B.pack (replicateM 160 getWord8)
    if myCksum == theirCksum
      then return sc
      else error "the checksum test for the compiled source code failed"

