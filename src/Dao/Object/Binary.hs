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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE RankNTypes #-}

module Dao.Object.Binary where

import           Dao.Token
import           Dao.Object
import qualified Dao.Tree as T
import           Dao.Glob
import qualified Dao.EnumSet as Es
import qualified Dao.Binary  as D

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
import qualified Data.Map               as M
import           Data.Digest.SHA1       as SHA1
import           Data.Array.IArray
import           Data.Time hiding (parseTime)

import qualified Data.ByteString.Lazy   as B
import qualified Data.Binary            as B
import qualified Data.Binary.Get        as B
import qualified Data.Binary.Put        as B

import Debug.Trace

----------------------------------------------------------------------------------------------------

-- This is only necessary to shorten the name 'MethodTable' because it is used throughout so many
-- instance declarations and type contexts.
type MTab = MethodTable

instance D.Binary CoreType MTab where
  put t = D.putWord8 $ case t of
    NullType     -> 0x07
    TrueType     -> 0x08
    TypeType     -> 0x09
    IntType      -> 0x0A
    WordType     -> 0x0B
    LongType     -> 0x0C
    FloatType    -> 0x0D
    RatioType    -> 0x0E
    ComplexType  -> 0x0F
    TimeType     -> 0x10
    DiffTimeType -> 0x11
    CharType     -> 0x12
    StringType   -> 0x13
    RefType      -> 0x14
    ListType     -> 0x15
    TreeType     -> 0x16
    BytesType    -> 0x17
    HaskellType  -> 0x18
  get = D.word8PrefixTable <|> fail "Dao.Object.CoreType"

instance D.HasPrefixTable CoreType D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "CoreType" 0x07 0x18 $ map return $
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

instance D.Binary Object MTab where
  put o = do
    let t   = D.put (objType o)
        p o = t >> D.put o
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
      OList    o -> p o
      OTree    o -> p o
      OBytes   o -> p o
      OHaskell o ifc -> do
        let typ = objHaskellType ifc 
        let tid = typeRepToUStr typ
        mtab <- D.getCoderTable
        case D.getEncoderForType tid mtab of
          Just fn -> do
            tid  <- D.newInStreamID tid
            D.put tid >> D.putWithBlockStream1M (fn o)
          Nothing -> fail $ unwords ["no binary format method defied for Haskell type", show typ]
  get = D.word8PrefixTable <|> fail "Dao.Object.Object"

instance D.HasPrefixTable Object D.Byte MTab where
  prefixTable =
    let g f = fmap f D.get
    in  D.mkPrefixTableWord8 "Object" 0x07 0x18 $
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
          , g OList
          , g OTree
          , g OBytes
          , do  D.updateTypes
                mtab <- D.getCoderTable
                tid  <- D.get >>= D.decodeIndexLookup
                maybe (D.get >>= \ (D.BlockStream1M bs1m) -> return (OBytes bs1m)) id $ do
                  tid <- tid
                  fn  <- D.getDecoderForType tid mtab
                  tab <- lookupMethodTable tid mtab
                  return (flip OHaskell tab <$> D.getWithBlockStream1M fn)
          ]

instance D.Binary ObjType    MTab where { put (ObjType    o) = D.put o; get = ObjType    <$> D.get; }
instance D.Binary TypeStruct MTab where { put (TypeStruct o) = D.put o; get = TypeStruct <$> D.get; }
instance D.Binary TypeSym    MTab where
  put o = case o of
    CoreType o       -> D.put o
    TypeVar  ref ctx -> D.prefixByte 0x1A $ D.put ref >> D.put ctx
  get = D.word8PrefixTable <|> fail "Dao.Object.TypeSym"
instance D.HasPrefixTable TypeSym D.Byte MTab where
  prefixTable = fmap CoreType D.prefixTable <>
    D.mkPrefixTableWord8 "TypeSym" 0x1A 0x1A [pure TypeVar <*> D.get <*> D.get]

instance D.Binary Reference MTab where { put (Reference o) = D.put o; get = Reference <$> D.get; }

instance D.Binary QualRef MTab where
  put o = case o of
    Unqualified ref -> D.prefixByte 0x21 $ D.put ref
    ObjRef    o     -> D.prefixByte 0x22 $ D.put o
    Qualified q ref -> D.prefixByte pfx  $ D.put ref where
      pfx = case q of
        LOCAL  -> 0x23
        QTIME  -> 0x24
        GLODOT -> 0x25
        STATIC -> 0x26
        GLOBAL -> 0x27
  get = D.word8PrefixTable <|> fail "Dao.Object.QualRef"
instance D.HasPrefixTable QualRef D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "QualRef" 0x21 0x27 $
    [ Unqualified      <$> D.get
    , ObjRef           <$> D.get
    , Qualified LOCAL  <$> D.get
    , Qualified QTIME  <$> D.get
    , Qualified GLODOT <$> D.get
    , Qualified STATIC <$> D.get
    , Qualified GLOBAL <$> D.get
    ]

instance D.Binary Location MTab where
  put o = case o of
    LocationUnknown  -> return ()
    Location a b c d -> D.prefixByte 0x29 $ D.put a >> D.put b >> D.put c >> D.put d
  get = D.tryWord8 0x29 $ pure Location <*> D.get <*> D.get <*> D.get <*> D.get

instance D.Binary TopLevelExpr MTab where
  put o = case o of
    Attribute a             b z -> D.prefixByte 0x2B $ D.put a >> D.put b >> D.put z
    TopScript a               z -> D.prefixByte 0x2C $ D.put a >> D.put z
    EventExpr BeginExprType b z -> D.prefixByte 0x2D $ D.put b >> D.put z
    EventExpr ExitExprType  b z -> D.prefixByte 0x2E $ D.put b >> D.put z
    EventExpr EndExprType   b z -> D.prefixByte 0x2F $ D.put b >> D.put z
  get = D.word8PrefixTable <|> fail "Dao.Object.TopLevelExpr"
instance D.HasPrefixTable TopLevelExpr D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "TopLevelExpr" 0x2B 0x2F $
    [ pure Attribute <*> D.get <*> D.get <*> D.get
    , pure TopScript <*> D.get <*> D.get
    , pure (EventExpr BeginExprType) <*> D.get <*> D.get
    , pure (EventExpr ExitExprType ) <*> D.get <*> D.get
    , pure (EventExpr EndExprType  ) <*> D.get <*> D.get
    ]

instance D.Binary ScriptExpr MTab where
  put o = case o of
    IfThenElse   a           -> D.prefixByte 0x31 $ D.put a
    WhileLoop    a           -> D.prefixByte 0x32 $ D.put a
    EvalObject   a         z -> D.prefixByte 0x33 $ D.put a >> D.put z
    TryCatch     a     b c z -> D.prefixByte 0x34 $ D.put a >> D.put b >> D.put c >> D.put z
    ForLoop      a     b c z -> D.prefixByte 0x35 $ D.put a >> D.put b >> D.put c >> D.put z
    ContinueExpr True  b   z -> D.prefixByte 0x36 $ D.put b >> D.put z
    ContinueExpr False b   z -> D.prefixByte 0x37 $ D.put b >> D.put z
    ReturnExpr   True  b   z -> D.prefixByte 0x38 $ D.put b >> D.put z
    ReturnExpr   False b   z -> D.prefixByte 0x39 $ D.put b >> D.put z
    WithDoc      a     b   z -> D.prefixByte 0x3A $ D.put a >> D.put b >> D.put z
  get = D.word8PrefixTable <|> fail "Dao.Object.ScriptExpr"
instance D.HasPrefixTable ScriptExpr D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "ScriptExpr" 0x31 0x3A $
    [ IfThenElse <$> D.get
    , WhileLoop  <$> D.get
    , pure EvalObject   <*> D.get <*> D.get
    , pure TryCatch     <*> D.get <*> D.get <*> D.get <*> D.get
    , pure ForLoop      <*> D.get <*> D.get <*> D.get <*> D.get
    , pure (ContinueExpr True ) <*> D.get <*> D.get
    , pure (ContinueExpr False) <*> D.get <*> D.get
    , pure (ReturnExpr   True ) <*> D.get <*> D.get
    , pure (ReturnExpr   False) <*> D.get <*> D.get
    , pure WithDoc      <*> D.get <*> D.get <*> D.get
    ]

instance D.Binary IfExpr MTab where
  put (IfExpr a b c) = D.put a >> D.put b >> D.put c
  get = pure IfExpr <*> D.get <*> D.get <*> D.get

instance D.Binary ElseExpr MTab where
  put (ElseExpr a b) = D.put a >> D.put b
  get = pure ElseExpr <*> D.get <*> D.get

instance D.Binary IfElseExpr MTab where
  put (IfElseExpr a b c d) = D.put a >> D.put b >> D.put c >> D.put d
  get = pure IfElseExpr <*> D.get <*> D.get <*> D.get <*> D.get

instance D.Binary WhileExpr MTab where
  put (WhileExpr o) = D.put o
  get = WhileExpr <$> D.get

instance D.Binary CodeBlock MTab where
  put (CodeBlock o) = D.put o
  get = CodeBlock <$> D.get

instance D.Binary RefExpr MTab where
  put (RefExpr a b) = D.put a >> D.put b
  get = pure RefExpr <*> D.get <*> D.get

instance D.Binary QualRefExpr MTab where
  put o = D.prefixByte 0x28 $ case o of
    UnqualRefExpr (RefExpr o loc)     -> D.put (Unqualified o) >> D.put loc
    QualRefExpr q (RefExpr o lo1) lo2 -> D.put (Qualified q o) >> D.put lo1 >> D.put lo2
  get = D.word8PrefixTable <|> fail "Dao.Object.QualRefExpr"
instance D.HasPrefixTable QualRefExpr D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "QualRefExpr" 0x28 0x28 $
    [ D.get >>= \q -> case q of
        Unqualified o -> UnqualRefExpr . RefExpr o <$> D.get
        Qualified q o -> pure (\lo1 lo2 -> QualRefExpr q (RefExpr o lo1) lo2) <*> D.get <*> D.get
        ObjRef _ -> error "internal: received QualRef where QualRefExpr was expected."
    ]

instance D.Binary ObjectExpr MTab where
  put o = case o of
    VoidExpr               -> D.putWord8   0x41
    ObjQualRefExpr a       -> D.prefixByte 0x42 $ D.put a
    ObjParenExpr   a       -> D.prefixByte 0x43 $ D.put a
    Literal        a     z -> D.prefixByte 0x44 $ D.put a >> D.put z
    AssignExpr     a b c z -> D.prefixByte 0x45 $ D.put a >> D.put b >> D.put c >> D.put z
    Equation       a b c z -> D.prefixByte 0x46 $ D.put a >> D.put b >> D.put c >> D.put z
    PrefixExpr     a b   z -> D.prefixByte 0x47 $ D.put a >> D.put b >> D.put z
    ArraySubExpr   a b   z -> D.prefixByte 0x48 $ D.put a >> D.put b >> D.put z
    FuncCall       a b   z -> D.prefixByte 0x49 $ D.put a >> D.put b >> D.put z
    InitExpr       a b c z -> D.prefixByte 0x4A $ D.put a >> D.put b >> D.put c >> D.put z
    StructExpr     a b   z -> D.prefixByte 0x4B $ D.put a >> D.put b >> D.put z
    LambdaExpr     a b   z -> D.prefixByte 0x4C $ D.put a >> D.put b >> D.put z
    FuncExpr       a b c z -> D.prefixByte 0x4D $ D.put a >> D.put b >> D.put c >> D.put z
    RuleExpr       a b   z -> D.prefixByte 0x4E $ D.put a >> D.put b >> D.put z
    MetaEvalExpr   a     z -> D.prefixByte 0x4F $ D.put a >> D.put z
  get = D.word8PrefixTable <|> fail "Dao.Object.ObjectExpr"
instance D.HasPrefixTable ObjectExpr D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "ObjectExpr" 0x41 0x4F $
    [ return VoidExpr
    , ObjQualRefExpr <$> D.get
    , ObjParenExpr   <$> D.get
    , pure Literal      <*> D.get <*> D.get
    , pure AssignExpr   <*> D.get <*> D.get <*> D.get <*> D.get
    , pure Equation     <*> D.get <*> D.get <*> D.get <*> D.get
    , pure PrefixExpr   <*> D.get <*> D.get <*> D.get
    , pure ArraySubExpr <*> D.get <*> D.get <*> D.get
    , pure FuncCall     <*> D.get <*> D.get <*> D.get
    , pure InitExpr     <*> D.get <*> D.get <*> D.get <*> D.get
    , pure StructExpr   <*> D.get <*> D.get <*> D.get
    , pure LambdaExpr   <*> D.get <*> D.get <*> D.get
    , pure FuncExpr     <*> D.get <*> D.get <*> D.get <*> D.get
    , pure RuleExpr     <*> D.get <*> D.get <*> D.get
    , pure MetaEvalExpr <*> D.get <*> D.get
    ]

instance D.Binary a MTab => D.Binary (TyChkExpr a) MTab where
  put o = case o of
    NotTypeChecked a       -> D.prefixByte 0x51 $ D.put a
    TypeChecked    a b c   -> D.prefixByte 0x52 $ D.put a >> D.put b >> D.put c
    DisableCheck   a b c d -> D.prefixByte 0x53 $ D.put a >> D.put b >> D.put c >> D.put d
  get = D.word8PrefixTable <|> fail "Dao.Object.TyChkExpr"
instance D.Binary a MTab => D.HasPrefixTable (TyChkExpr a) D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "TyChkExpr" 0x51 0x53 $
    [ NotTypeChecked <$> D.get
    , pure TypeChecked  <*> D.get <*> D.get <*> D.get
    , pure DisableCheck <*> D.get <*> D.get <*> D.get <*> D.get
    ]

instance D.Binary ParamExpr MTab where
  put (ParamExpr True  a b) = D.prefixByte 0x54 $ D.put a >> D.put b
  put (ParamExpr False a b) = D.prefixByte 0x55 $ D.put a >> D.put b
  get = D.word8PrefixTable <|> fail "Dao.Object.ParamExpr"
instance D.HasPrefixTable ParamExpr D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "ParamExpr" 0x54 0x55 $
    [ pure (ParamExpr True ) <*> D.get <*> D.get
    , pure (ParamExpr False) <*> D.get <*> D.get
    ]

instance D.Binary PrefixOp MTab where
  put o = D.putWord8 $ case o of
    { INVB   -> 0x5A; NOT -> 0x5B; NEGTIV -> 0x5C
    ; POSTIV -> 0x5D; REF -> 0x5E; DEREF  -> 0x5F }
  get = D.word8PrefixTable <|> fail "Dao.Object.PrefixOp"
instance D.HasPrefixTable PrefixOp D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "PrefixOp" 0x5A 0x5F $
    map return [INVB, NOT, NEGTIV, POSTIV, REF, DEREF]

instance D.Binary UpdateOp MTab where
  put o = D.putWord8 $ case o of
    UCONST -> 0x61
    UADD   -> 0x62
    USUB   -> 0x63
    UMULT  -> 0x64
    UDIV   -> 0x65
    UMOD   -> 0x66
    UPOW   -> 0x67
    UORB   -> 0x68
    UANDB  -> 0x69
    UXORB  -> 0x6A
    USHL   -> 0x6B
    USHR   -> 0x6C
    UARROW -> 0x6D
  get = D.word8PrefixTable <|> fail "Dao.Object.UpdateOp"
instance D.HasPrefixTable UpdateOp D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "UpdateOp" 0x61 0x6D $ map return $
    [UCONST, UADD, USUB, UMULT, UDIV, UMOD, UPOW, UORB, UANDB, UXORB, USHL, USHR, UARROW]

-- The byte prefixes overlap with the update operators of similar function to
-- the operators, except for the comparison opeators (EQUL, NEQUL, GTN, LTN,
-- GTEQ, LTEQ) which overlap with the prefix operators (INVB, NOT, NEGTIV, POSTIV, REF, DEREF)
instance D.Binary InfixOp MTab where
  put o = D.putWord8 $ case o of
    { EQUL -> 0x5A; NEQUL -> 0x5B; GTN -> 0x5C; LTN -> 0x5D; GTEQ -> 0x5E; LTEQ -> 0x5F
    ; ADD  -> 0x62; SUB -> 0x63; MULT -> 0x64; DIV   -> 0x65
    ; MOD  -> 0x66; POW -> 0x67; ORB  -> 0x68; ANDB  -> 0x69
    ; XORB -> 0x6A; SHL -> 0x6B; SHR  -> 0x6C; ARROW -> 0x6D
    ; OR   -> 0x6E; AND -> 0x6F } 
  get = D.word8PrefixTable <|> fail "Dao.Object.InfixOp"
instance D.HasPrefixTable InfixOp D.Byte MTab where
  prefixTable = D.mkPrefixTableWord8 "InfixOp" 0x5A 0x6E $ let {r=return; z=mzero} in
    [ r EQUL , r NEQUL, r GTN , r LTN, r GTEQ , r LTEQ , z, z
    , r ADD  , r SUB  , r MULT, r DIV, r MOD  , r POW  , r ORB
    , r ANDB , r XORB , r SHL , r SHR, r ARROW, r OR   , r AND
    ]

instance D.Binary ParenExpr MTab where
  put (ParenExpr a b) = D.put a >> D.put b
  get = pure ParenExpr <*> D.get <*> D.get

instance D.Binary LValueExpr MTab where
  put (LValueExpr o) = D.put o
  get = LValueExpr <$> D.get

instance D.Binary ParamListExpr MTab where
  put (ParamListExpr lst loc) = D.put lst >> D.put loc
  get = pure ParamListExpr <*> D.get <*> D.get

instance D.Binary RuleStrings MTab where
  put (RuleStrings a b) = D.put a >> D.put b
  get = pure RuleStrings <*> D.get <*> D.get

instance D.Binary ObjListExpr MTab where
  put (ObjListExpr a b) = D.put a >> D.put b
  get = pure ObjListExpr <*> D.get <*> D.get

instance D.Binary OptObjListExpr MTab where
  put (OptObjListExpr o) = D.put o
  get = OptObjListExpr <$> D.get

----------------------------------------------------------------------------------------------------

-- | The magic number is the first 8 bytes to every bytecode compiled object program. It is the
-- ASCII value of the string "DaoData\n".
program_magic_number :: Word64
program_magic_number = 0x44616f44617461A

-- | This is the version number of the line protocol for transmitting bytecode compiled program
-- objects.
program_data_version :: Word64
program_data_version = 1

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

----------------------------------------------------------------------------------------------------

putObjBool :: Bool -> B.Put
putObjBool a = if a then B.put OTrue else B.put ONull

getObjBool :: B.Get Bool
getObjBool = B.lookAhead B.get >>= \a -> case a of
  ONull -> return False
  OTrue -> return True
  _     -> fail "expecting boolean object value"

putListWith :: (a -> B.Put) -> [a] -> B.Put
putListWith p ax = mapM_ p ax >> B.putWord8 0x00

getListWith :: B.Get a -> B.Get [a]
getListWith getx = loop [] where
  loop ax = do
    w <- B.lookAhead B.getWord8
    case w of
      0x00 -> B.getWord8 >> return ax
      _    -> getx >>= \a -> loop (ax++[a])

putList :: B.Binary a => [a] -> B.Put
putList = Dao.Object.Binary.putListWith B.put

getList :: B.Binary a => B.Get [a]
getList = Dao.Object.Binary.getListWith B.get

----------------------------------------------------------------------------------------------------

putMapWith :: (Eq k, Ord k) => (k -> B.Put) -> (v -> B.Put) -> M.Map k v -> B.Put
putMapWith putk putv m = Dao.Object.Binary.putListWith (\ (a, b) -> putk a >> putv b) (M.assocs m)

getMapWith :: (Eq k, Ord k) => B.Get k -> B.Get v -> B.Get (M.Map k v)
getMapWith getk getv = fmap M.fromList (Dao.Object.Binary.getListWith (liftM2 (,) getk getv))

putMap :: (Eq k, Ord k, B.Binary k, B.Binary v) => M.Map k v -> B.Put
putMap m = putMapWith B.put B.put m

getMap :: (Eq k, Ord k, B.Binary k, B.Binary v) => B.Get (M.Map k v)
getMap = getMapWith B.get B.get

putObjMap :: B.Binary a => (m -> [(a, Object)]) -> (a -> B.Put) -> m -> B.Put
putObjMap asocs putIndex o = Dao.Object.Binary.putListWith (\ (i, o) -> putIndex i >> B.put o) (asocs o)

getObjMap :: B.Binary a => ([(a, Object)] -> m) -> B.Get a -> B.Get m
getObjMap fromList getIndex = fmap fromList (Dao.Object.Binary.getListWith (getIndex >>= \i -> B.get >>= \o -> return (i, o)))

putTreeWith :: (Eq p, Ord p) => (p -> B.Put) -> (a -> B.Put) -> T.Tree p a -> B.Put
putTreeWith putp puta t =
  case t of
    T.Void           -> B.putWord8 0x03
    T.Leaf       a   -> B.putWord8 0x04 >> puta a
    T.Branch       t -> B.putWord8 0x05 >> putMapWith putp (putTreeWith putp puta) t
    T.LeafBranch a t -> B.putWord8 0x06 >> puta a >> putMapWith putp (putTreeWith putp puta) t

getTreeWith :: (Eq p, Ord p) => B.Get p -> B.Get a -> B.Get (T.Tree p a)
getTreeWith getp geta = do
  t <- B.getWord8
  case t of
    0x03 -> return T.Void
    0x04 -> geta >>= \a -> return (T.Leaf{T.branchData=a})
    0x05 -> getMapWith getp (getTreeWith getp geta) >>= \t -> return (T.Branch{T.branchMap=t})
    0x06 -> do
      a <- geta
      t <- getMapWith getp (getTreeWith getp geta)
      return (T.LeafBranch{T.branchData=a, T.branchMap=t})
    _    -> fail "Dao.Tree.Tree"

instance (Eq p, Ord p, B.Binary p, B.Binary a) => B.Binary (T.Tree p a) where
  put t = putTreeWith B.put B.put t
  get   = getTreeWith B.get B.get

----------------------------------------------------------------------------------------------------

typeIDBytePrefix :: CoreType -> Word8
typeIDBytePrefix t = case t of
  NullType     -> 0x07
  TrueType     -> 0x08
  TypeType     -> 0x09
  IntType      -> 0x0A
  WordType     -> 0x0B
  LongType     -> 0x0C
  FloatType    -> 0x0D
  RatioType    -> 0x0E
  ComplexType  -> 0x0F
  TimeType     -> 0x10
  DiffTimeType -> 0x11
  CharType     -> 0x12
  StringType   -> 0x13
  RefType      -> 0x14
  ListType     -> 0x15
  TreeType     -> 0x16
  BytesType    -> 0x17
  HaskellType  -> 0x18

bytePrefixToTypeID :: Word8 -> Maybe CoreType
bytePrefixToTypeID t = case t of
  0x07 -> Just NullType
  0x08 -> Just TrueType
  0x09 -> Just TypeType
  0x0A -> Just IntType
  0x0B -> Just WordType
  0x0C -> Just LongType
  0x0D -> Just FloatType
  0x0E -> Just RatioType
  0x0F -> Just ComplexType
  0x10 -> Just TimeType
  0x11 -> Just DiffTimeType
  0x12 -> Just CharType
  0x13 -> Just StringType
  0x14 -> Just RefType
  0x15 -> Just ListType
  0x16 -> Just TreeType
  0x17 -> Just BytesType
  0x18 -> Just HaskellType
  _    -> Nothing

--instance B.Binary CoreType where
--  B.put t = B.putWord8 (typeIDBytePrefix t)
--  B.get = do
--    w <- B.getWord8
--    case bytePrefixToTypeID w of
--      Nothing -> fail "was expecting type data"
--      Just  w -> return w

-- The binary serialization of 'Dao.Object.Reference' and 'Dao.Object.QualRef' are intricately tied
-- together.
instance B.Binary Reference where
  put (Reference r) = B.putWord8 0x31 >> putList r
  get = B.getWord8 >>= \w -> case w of
    0x31 -> liftM Reference getList
    _    -> fail "expecting reference value"
instance B.Binary QualRef where
  put q = case q of
    Unqualified r -> B.put r
    Qualified q r -> do
      let f n = B.putWord8 n >> B.put r
      f $ case q of
        LOCAL  -> 0x32
        QTIME  -> 0x33
        GLODOT -> 0x34
        STATIC -> 0x35
        GLOBAL -> 0x36
    ObjRef      o -> B.put o
  get = do
    w <- B.lookAhead B.getWord8
    let f q = B.getWord8 >> liftM (Qualified q) B.get
    case w of
      0x31 -> liftM Unqualified B.get
      0x32 -> f LOCAL
      0x33 -> f QTIME
      0x34 -> f GLODOT
      0x35 -> f STATIC
      0x36 -> f GLOBAL
      _    -> liftM ObjRef B.get

instance B.Binary Object where
  put o = do
    let x o p = B.putWord8 (typeIDBytePrefix (objType o)) >> B.put p
        px o putx = B.putWord8 (typeIDBytePrefix (objType o)) >> putx
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
      OAbsTime      a -> x o a
      ORelTime      a -> x o a
      OChar         a -> x o a
      OString       a -> px o (encodeUStr a)
      ORef          a -> x o a
      OList         a -> px o (putList a)
      OTree         a -> x o a
      OBytes        a -> x o a
      OHaskell  _ ifc -> error $ unwords $
        ["no binary format method defied for Haskell type", show (objHaskellType ifc)]
  get = do
    ty <- B.getWord8
    let x fn = fmap fn B.get
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
        TimeType     -> x OAbsTime
        DiffTimeType -> x ORelTime
        CharType     -> x OChar
        StringType   -> fmap OString decodeUStr
        RefType      -> x ORef
        ListType     -> fmap OList getList
        TreeType     -> x OTree
        BytesType    -> x OBytes
        HaskellType  -> error "internal: cannot retrieve Haskell types from a binary stream"

instance B.Binary RefExpr where
  put (RefExpr r loc) = B.putWord8 0x88 >> B.put r >> B.put loc
  get = B.getWord8 >>= \w -> case w of
    0x88 -> liftM2 RefExpr B.get B.get
    _    -> fail "expecting reference expression"

instance B.Binary QualRefExpr where
  put r = case r of
    UnqualRefExpr   r -> B.put r
    QualRefExpr typ r loc ->
      let f n = B.putWord8 n >> B.put r >> B.put loc
      in  case typ of
            LOCAL  -> f 0x8B
            QTIME  -> f 0x8C
            GLODOT -> f 0x8D
            STATIC -> f 0x8E
            GLOBAL -> f 0x8F
  get = do
    w <- B.lookAhead B.getWord8
    let f typ = B.getWord8 >> liftM2 (QualRefExpr typ) B.get B.get
    case w of
      0x8B -> f LOCAL
      0x8C -> f QTIME
      0x8D -> f GLODOT
      0x8E -> f STATIC
      0x8F -> f GLOBAL
      _    -> liftM UnqualRefExpr B.get

----------------------------------------------------------------------------------------------------

instance (B.Binary a, RealFloat a) => B.Binary (Complex a) where
  put o = B.put (realPart o) >> B.put (imagPart o)
  get = liftM2 (:+) B.get B.get

instance B.Binary GlobUnit where
  put p = case p of
    Wildcard -> B.putWord8 0x29
    AnyOne   -> B.putWord8 0x2A
    Single o -> B.putWord8 0x2B >> B.put o
  get = B.getWord8 >>= \w -> case w of
    0x29 -> return Wildcard
    0x2A -> return AnyOne
    0x2B -> fmap Single B.get
    _    -> fail "expecting pattern unit object"

instance B.Binary Glob where
  put p = putList (getPatUnits p)
  get   = getList >>= \px -> return $
    Glob
    { getPatUnits = px
    , getGlobLength = length px
    }

----------------------------------------------------------------------------------------------------

putNullTermStr :: Name -> B.Put
putNullTermStr nm = mapM_ B.putWord8 (utf8bytes nm) >> B.putWord8 0

getNullTermStr :: B.Get UStr
getNullTermStr = loop [] where
  loop wx = B.getWord8 >>= \w -> if w==0 then return (upack wx) else loop (wx++[w])

-- There are only 255 non-zero 8-bit prefixes available, I have reused a few here. To make it easier
-- to remember, operators with the same string representation also have the same 8-bit serialization
-- prefix.
instance B.Binary UpdateOp where
  put a = B.putWord8 $ case a of
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
    w <- B.getWord8
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

instance B.Binary LValueExpr where
  put (LValueExpr o) = B.putWord8 0x4D >> B.put o
  get = B.getWord8 >>= \w -> case w of
    0x4D -> liftM LValueExpr B.get
    _    -> fail "expecting L-value expression"

putTyChkExpr :: (a -> B.Put) -> TyChkExpr a -> B.Put
putTyChkExpr putfn o = case o of
  NotTypeChecked a       -> putfn a
  TypeChecked    a b loc -> B.putWord8 0x3A >> putfn a >> B.put b >> B.put loc

getTyChkExpr :: B.Get a -> B.Get (TyChkExpr a)
getTyChkExpr getfn = B.lookAhead B.getWord8 >>= \w -> case w of
  0x3A -> B.getWord8 >> liftM3 TypeChecked getfn B.get B.get
  _    -> liftM NotTypeChecked getfn

instance B.Binary ParamExpr where
  put (ParamExpr a b loc) = B.putWord8 (if a then 0x39 else 0x38) >> putTyChkExpr B.put b >> B.put loc
  get = B.getWord8 >>= \w -> let getfn = getTyChkExpr B.get in case w of
    0x38 -> liftM2 (ParamExpr False) getfn B.get
    0x39 -> liftM2 (ParamExpr True ) getfn B.get
    _    -> fail "expecting parameter"

instance B.Binary ParamListExpr where
  put (ParamListExpr lst loc) = putTyChkExpr putList lst >> B.put loc
  get = liftM2 ParamListExpr (getTyChkExpr getList) B.get

instance B.Binary RuleStrings where
  put (RuleStrings a b) = putList a >> B.put b
  get = liftM2 RuleStrings getList B.get

instance B.Binary OptObjListExpr where
  put (OptObjListExpr a) = maybe (return ()) (\a -> B.putWord8 0x3B >> B.put a) a
  get = fmap OptObjListExpr (optional (B.getWord8 >>= \a -> if a==0x38 then B.get else mzero))

instance B.Binary ObjectExpr where
  put o = let x z w fn = B.putWord8 w >> fn >> B.put z in case o of
    VoidExpr               -> B.putWord8 0x40
    Literal        a     z -> x z 0x41 $ B.put a 
    AssignExpr     a b c z -> x z 0x42 $ B.put a >> B.put b >> B.put c
    Equation       a b c z -> x z 0x43 $ B.put a >> B.put b >> B.put c
    PrefixExpr     a b   z -> x z 0x44 $ B.put a >> B.put b
    ArraySubExpr   a b   z -> x z 0x45 $ B.put a >> B.put b
    FuncCall       a b   z -> x z 0x46 $ B.put a >> B.put b
    InitExpr       a b c z -> x z 0x47 $ B.put a >> B.put b >> B.put c
    StructExpr     a b   z -> x z 0x48 $ B.put a >> B.put b
    FuncExpr       a b c z -> x z 0x49 $ B.put a >> B.put b >> B.put c
    RuleExpr       a b   z -> x z 0x4A $ B.put a >> B.put b
    MetaEvalExpr   a     z -> x z 0x4B $ B.put a
    ObjQualRefExpr a       -> B.put a
    ObjParenExpr   a       -> B.put a
  get = do
    w <- B.lookAhead B.getWord8
    let f a = B.getWord8 >> a
    case w of
      0x40 -> f $ return VoidExpr
      0x41 -> f $ liftM2 Literal      B.get         B.get
      0x42 -> f $ liftM4 AssignExpr   B.get B.get B.get B.get
      0x43 -> f $ liftM4 Equation     B.get B.get B.get B.get
      0x44 -> f $ liftM3 PrefixExpr   B.get B.get     B.get
      0x45 -> f $ liftM3 ArraySubExpr B.get B.get     B.get
      0x46 -> f $ liftM3 FuncCall     B.get B.get     B.get
      0x47 -> f $ liftM4 InitExpr     B.get B.get B.get B.get
      0x48 -> f $ liftM3 StructExpr   B.get B.get     B.get
      0x49 -> f $ liftM4 FuncExpr     B.get B.get B.get B.get
      0x4A -> f $ liftM3 RuleExpr     B.get B.get     B.get
      0x4B -> f $ liftM2 MetaEvalExpr B.get         B.get
      _    -> msum $
        [ liftM ObjQualRefExpr B.get
        , liftM ObjParenExpr   B.get
        , fail "expecting object expression"
        ]

instance B.Binary ParenExpr where
  put (ParenExpr a loc) = B.putWord8 0x4C >> B.put a >> B.put loc
  get = B.lookAhead B.getWord8 >>= \a -> case a of
    0x4C -> B.getWord8 >> liftM2 ParenExpr B.get B.get
    _    -> mzero

instance B.Binary IfExpr where
  put (IfExpr a b loc) = B.putWord8 0x51 >> B.put a >> B.put b >> B.put loc
  get = B.getWord8 >>= \w -> case w of
    0x51 -> liftM3 IfExpr B.get B.get B.get
    _    -> fail "expecting if expression"

instance B.Binary ElseExpr where
  put (ElseExpr a loc) = B.putWord8 0x52 >> B.put a >> B.put loc
  get = B.getWord8 >>= \w -> case w of
    0x52 -> liftM2 ElseExpr B.get B.get
    _    -> fail "expecting else-if expression"

instance B.Binary IfElseExpr where
  put (IfElseExpr a b c loc) = B.putWord8 0x53 >> B.put a >> putList b >> maybe (return ()) B.put c >> B.put loc
  get = B.getWord8 >>= \w -> case w of
    0x53 -> liftM4 IfElseExpr B.get getList (optional B.get) B.get
    _    -> fail "expecting if/else-if/else expression"

instance B.Binary WhileExpr where
  put (WhileExpr (IfExpr a b loc)) = B.putWord8 0x54 >> B.put a >> B.put b >> B.put loc
  get = B.getWord8 >>= \w -> case w of
    0x54 -> liftM3 (\a b c -> WhileExpr (IfExpr a b c)) B.get B.get B.get
    _    -> fail "expecting while expression"

--instance B.Binary ElseIfExpr where
--  B.put o = case o of
--    NullElseIfExpr       -> B.putWord8 0x51
--    ElseExpr     b   loc -> B.putWord8 0x52 >> B.put b >> B.put loc
--    ElseIfExpr o b n loc -> B.putWord8 0x53 >> B.put o >> B.put b >> B.put n >> B.put loc
--  B.get = B.getWord8 >>= \w -> case w of
--    0x51 -> return NullElseIfExpr
--    0x52 -> liftM2 ElseExpr       B.get     B.get
--    0x53 -> liftM4 ElseIfExpr B.get B.get B.get B.get
--    _    -> fail "expecting if-then-else expression"

instance B.Binary ScriptExpr where
  put s = case s of
    IfThenElse   a       -> B.put a
    WhileLoop    a       -> B.put a
    EvalObject   a     z -> x z 0x55 $ B.put a
    TryCatch     a b c z -> x z 0x56 $ B.put a >> maybe (return ()) B.put b >> maybe (return ()) B.put c
    ForLoop      a b c z -> x z 0x57 $ B.put        a >> B.put b >> B.put c
    ContinueExpr a b   z -> x z 0x58 $ putObjBool a >> B.put b
    ReturnExpr   a b   z -> x z 0x59 $ putObjBool a >> B.put b
    WithDoc      a b   z -> x z 0x5A $ B.put        a >> B.put b
    where
      x z i putx = B.putWord8 i >> putx >> B.put z
  get = do
    w <- B.lookAhead B.getWord8
    let x = B.getWord8
    case w of
      0x53 -> liftM IfThenElse B.get
      0x54 -> liftM WhileLoop  B.get
      0x55 -> x >> liftM2 EvalObject   B.get                 B.get
      0x56 -> x >> liftM4 TryCatch     B.get (optional B.get) (optional B.get) B.get
      0x57 -> x >> liftM4 ForLoop      B.get         B.get B.get B.get
      0x58 -> x >> liftM3 ContinueExpr getObjBool  B.get     B.get
      0x59 -> x >> liftM3 ReturnExpr   getObjBool  B.get     B.get
      0x5A -> x >> liftM3 WithDoc      B.get         B.get     B.get

instance B.Binary Location where
  put loc = case loc of
    LocationUnknown -> return ()
    loc             -> do
      B.putWord8 0x5F
      let fn acc = mapM_ B.putWord8 (bitsToVLInt (acc loc))
      fn startingLine >> fn startingColumn
      fn endingLine   >> fn endingColumn
  get = do
    is_empty <- B.isEmpty
    if is_empty
      then return LocationUnknown
      else do
        w <- B.lookAhead B.getWord8
        if w==0x5F
          then do
            B.getWord8
            a <- getFromVLInt
            b <- getFromVLInt
            c <- getFromVLInt
            d <- getFromVLInt
            return (Location a b c d)
          else return LocationUnknown

----------------------------------------------------------------------------------------------------

instance B.Binary CallableCode where
  put (CallableCode pat ty exe) = B.putWord8 0x25 >> B.put     pat >> B.put ty >> B.put exe
  get = B.getWord8 >>= \w -> case w of
    0x25 -> liftM3 CallableCode B.get     B.get B.get
    _    -> fail "expecting CallableCode"

instance B.Binary GlobAction where
  put (GlobAction pat exe) = B.putWord8 0x26 >> putList pat >> B.put exe
  get = B.getWord8 >>= \w -> case w of
    0x26 -> liftM2 GlobAction getList B.get
    _    -> fail "expecting GlobAction"

instance B.Binary CodeBlock where
  put = putList . codeBlock
  get = fmap CodeBlock getList

instance B.Binary Subroutine where
  put = B.put . origSourceCode
  get = do
    code <- B.get
    let msg = "Subroutine retrieved from binary used before being initialized."
    return $
      Subroutine
      { origSourceCode = code
      , staticVars     = error msg
      , executable     = error msg
      }

--instance B.Binary ObjSetOp where
--  B.put op = B.putWord8 $ case op of
--    ExactSet  -> 0xC1
--    AnyOfSet  -> 0xC2
--    AllOfSet  -> 0xC3
--    OnlyOneOf -> 0xC4
--    NoneOfSet -> 0xC5
--  B.get = B.getWord8 >>= \w -> case w of
--    0xC1 -> return ExactSet
--    0xC2 -> return AnyOfSet
--    0xC3 -> return AllOfSet
--    0xC4 -> return OnlyOneOf
--    0xC5 -> return NoneOfSet
--    _    -> fail "expecting set-logical operator for object pattern"

putEnumInfWith :: (a -> B.Put) -> Es.Inf a -> B.Put
putEnumInfWith putx a = case a of
  Es.NegInf  -> B.putWord8 0xC6
  Es.PosInf  -> B.putWord8 0xC7
  Es.Point a -> B.putWord8 0xC8 >> putx a

getEnumInfWith :: B.Get a -> B.Get (Es.Inf a)
getEnumInfWith getx = B.getWord8 >>= \w -> case w of
  0xC6 -> return Es.NegInf
  0xC7 -> return Es.PosInf
  0xC8 -> liftM  Es.Point  getx
  _    -> fail "expecting enum-inf data"

instance (Integral a, Bits a) => B.Binary (Es.Inf a) where
  put = putEnumInfWith putVLInt
  get = getEnumInfWith getFromVLInt

putSegmentWith :: (a -> B.Put) -> Es.Segment a -> B.Put
putSegmentWith putA a = maybe err id (mplus plur sing)
  where
    err = error "could not extract data from Dao.EnumSet.Segment dataype"
    plur = do
      (lo, hi) <- Es.plural a
      return (B.putWord8 0xCA >> putEnumInfWith putA lo >> putEnumInfWith putA hi)
    sing = do
      point <- Es.singular a
      return (B.putWord8 0xC9 >> putEnumInfWith putA point)

getSegmentWith :: (Enum a, Ord a, Es.InfBound a) => B.Get a -> B.Get (Es.Segment a)
getSegmentWith getA = B.getWord8 >>= \w -> case w of
  0xC9 -> getEnumInfWith getA >>= \a -> return (Es.enumInfSeg a a)
  0xCA -> liftM2 Es.enumInfSeg (getEnumInfWith getA) (getEnumInfWith getA)
  _    -> fail "expecting enum-segment expression"

instance (Enum a, Ord a, Es.InfBound a, Integral a, Bits a) => B.Binary (Es.Segment a) where
  put = putSegmentWith putVLInt
  get = getSegmentWith getFromVLInt

putEnumSetWith :: (Ord a, Enum a, Es.InfBound a) => (a -> B.Put) -> Es.Set a -> B.Put
putEnumSetWith putA s = Dao.Object.Binary.putListWith (putSegmentWith putA) (Es.toList s)

getEnumSetWith :: (Enum a, Ord a, Es.InfBound a) => B.Get a -> B.Get (Es.Set a)
getEnumSetWith getA = fmap Es.fromList (Dao.Object.Binary.getListWith (getSegmentWith getA))

get_binary_enumset_error = error "invalid object set from binary stream (overlapping items)"

instance (Es.InfBound a, Integral a, Bits a) => B.Binary (Es.Set a) where
  put = putEnumSetWith putVLInt
  get = getEnumSetWith getFromVLInt

instance B.Binary ObjListExpr where
  put (ObjListExpr lst _) = putList lst
  get = liftM2 ObjListExpr getList (return LocationUnknown)

instance B.Binary TopLevelExpr where
  put d = case d of
    Attribute      a b   z -> x 0x61    $ B.put a >> B.put b          >> B.put z
    TopScript      a     z -> x 0x62    $ B.put a                   >> B.put z
    EventExpr      a b   z -> x (evt a) $ B.put b                   >> B.put z
    where
      x i putx = B.putWord8 i >> putx
      evt typ = case typ of
        BeginExprType -> 0x63
        EndExprType   -> 0x64
        ExitExprType  -> 0x65
  get = do
    w <- B.getWord8
    case w of
      0x61 -> liftM3 Attribute      B.get B.get B.get
      0x62 -> liftM2 TopScript      B.get B.get
      0x63 -> evtexp BeginExprType
      0x64 -> evtexp EndExprType
      0x65 -> evtexp ExitExprType
      _    -> fail "expecting top-level expression"
      where { evtexp typ = liftM2 (EventExpr typ) B.get B.get }

instance B.Binary RefQualifier where
  put o = B.putWord8 $ case o of
    LOCAL    -> 0x93
    QTIME    -> 0x94
    GLODOT   -> 0x95
    STATIC   -> 0x96
    GLOBAL   -> 0x97 -- same value as DOT
  get = B.getWord8 >>= \w -> case w of
    0x93 -> return LOCAL
    0x94 -> return QTIME
    0x95 -> return GLODOT
    0x96 -> return STATIC
    0x97 -> return GLOBAL  -- same value as DOT
    _    -> fail "expecting reference qualifier"

-- There are only 255 non-zero 8-bit prefixes available, I have reused a few here. To make it easier
-- to remember, operators with the same string representation also have the same 8-bit serialization
-- prefix.
instance B.Binary PrefixOp where
  put o = B.putWord8 $ case o of
    INVB   -> 0x98
    NOT    -> 0x99
    POSTIV -> 0x9A -- same value as ADD
    NEGTIV -> 0x9B -- same value as SUB
  get = B.getWord8 >>= \w -> case w of
    0x98 -> return INVB
    0x99 -> return NOT
    0x9A -> return POSTIV -- same value as ADD
    0x9B -> return NEGTIV -- same value as SUB
    _    -> fail "expecting reference prefix operator"

--instance B.Binary RefInfixOp where
--  B.put o = B.putWord8 $ case o of
--    POINT -> 0xA1
--    DOT   -> 0xA2
--  B.get = B.getWord8 >>= \w -> case w of
--    0xA1 -> return POINT
--    0xA2 -> return DOT
--    _ -> fail "expecting reference infix operator"

instance B.Binary InfixOp where
  put o = B.putWord8 $ case o of
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
  get = B.getWord8 >>= \w -> case w of
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

instance B.Binary TypeSym where
  put t = case t of
    CoreType t   -> B.putWord8 0xA1 >> B.putWord8 (fromIntegral (fromEnum t))
    TypeVar  t s -> B.putWord8 0xA2 >> B.put t >> putList s
  get = B.getWord8 >>= \w -> case w of
    0xA1 -> fmap (CoreType . toEnum . fromIntegral) B.getWord8
    0xA2 -> liftM2 TypeVar B.get getList
    _    -> fail "expecting TypeSymbol"

instance B.Binary TypeStruct where
  put (TypeStruct t) = B.putWord8 0xA3 >> putList t
  get = B.lookAhead B.getWord8 >>= \w -> case w of
    0xA3 -> B.getWord8 >> fmap TypeStruct getList
    _    -> fail "expecting TypeStruct"

instance B.Binary ObjType where
  put (ObjType tx) = B.putWord8 0xA4 >> putList tx
  get = B.lookAhead B.getWord8 >>= \w -> case w of
    0xA4 -> B.getWord8 >> fmap ObjType getList
    _    -> fail "expecting ObjType"

