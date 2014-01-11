-- "src/Dao/String.hs"  provides the fundamental string data type
-- called "UStr" which is used throughout the Dao System.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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

-- | This module has two purposes. Firstly, this module depends on no other module in the Dao
-- program, so it may be imported by any other module, and as such it provides the classes and
-- functions that must be available to every module. Most of these essential functions are related
-- to strings, which is why this module is named so.
--
-- Therefore secondly, this module provides the /universal string/ data type 'UStr', and a type
-- class 'UStrType' that allows you to declare arbitrary data types to be convertible to and from
-- universal strings. Universal strings are built upon the "Data.ByteString.Lazy.UTF8" module in
-- the @utf8-string@ package of the Haskell platform. All strings used in the Dao runtime are stored
-- as this data type.
--
-- /NOTE:/ though this module is absolutely essential to every other module in the Dao system, not
-- all data structures should need to instantiate 'UStrType'. By contrst, the
-- 'Dao.Object.Structured' (not defined in this module) should be instantiated by nearly all
-- data structures, especially if it is necessary to manipulate these structures within the Dao
-- programming language.
--
-- Dao is a high-level language, like a macro language or a scripting language. One thing
-- scripting/meta languages all have in common is the use of strings as a way to store and transmit
-- data in a human-readable but structured format; strings are often the universal intermediate code
-- of the runtime environment. Data structures can be converted to a string, stored in memory,
-- transmitted over a socket or pipe, saved to disk. Data from the disk, the socket, or in memory
-- can be parsed to reconstruct the data structures.
--
-- However it is my opinion that use of strings as intermediate data structures is very poor design
-- in any programming language; it is an anti-pattern. I believe the universal data type should the
-- tree rather than the string. Therefore I have provided the "Dao.Tree" and "Dao.Struct" modules,
-- and the 'Dao.Object.Structured' type class which expand on the ideas of 'Prelude.Show' and
-- 'Prelude.Read' by using a 'Dao.Tree.Tree' as the intermediate data structure, rather than a
-- 'Dao.String.UStr'.
module Dao.String where

import           Control.Monad
import           Control.Monad.State
import           Control.DeepSeq
import           Control.Exception (assert)

import           Data.String
import           Data.Monoid
import           Data.Typeable
import qualified Data.Binary               as B
import           Data.Bits
import           Data.Char
import           Data.List (partition)
import           Data.Word
import           Data.Array.Unboxed
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy      as B
import qualified Codec.Binary.UTF8.String  as UTF8

import           Numeric

-- Necessary for the HasNullValue instances
import           Data.Int
import           Data.Ratio
import           Data.Complex
import           Data.Time.Clock
import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Set    as S

-- | Objects which can be used as a predicate testing whether or not the object is null, or of a
-- default value, should instantiate this class.
class HasNullValue a where { nullValue :: a; testNull :: a -> Bool; }
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
instance HasNullValue U.ByteString    where { nullValue = mempty; testNull = (==mempty); }

-- | This is the /universal string/ type. It is a @newtype@ wrapper around
-- 'Data.ByteString.Lazy.UTF8.ByteString', but has an API that is used throughout the Dao system.
-- There is serious consideration to replace this module with "Data.Text", but even if that happens,
-- this module will be kept to provide a stable API to the string package upon which it is built.
newtype UStr = UStr { toUTF8ByteString :: U.ByteString } deriving (Eq, Ord, Typeable)
instance Monoid UStr where { mempty = toUStr ""; mappend a b = toUStr (uchars a ++ uchars b); }

-- | To provide intermediate string representations of data structures is one of the purposes of
-- 'Prelude.Show' and 'Prelude.Read' in the Haskell language. In Haskell, 'Prelude.Read' and
-- 'Prelude.Show' must, by convention, output a string that can be converted back to an exactly
-- equivalent data structure to the structure that produced the output when the output string is
-- parsed by 'Prelude.Read'. In other words @read (show a) == a && show (read a) == a@ should
-- evaluate to 'Prelude.True' for any type of @a@.
-- 
-- This is not merely a convention for the 'UStrType', it is a requirement. The minimal complete
-- definition is 'toUStr' and one or both of 'fromUStr' and 'maybeFromUStr'. The 'nil' function is
-- part of the minimal complete definition, except when your data type is also an instance of
-- 'Data.Monoid.Monoid'. If your data type is also a 'Data.Monoid.Monoid', then the default instance
-- of 'nil' is 'Data.monoid.mempty'.
-- 
-- Another big difference between 'UStrType' and 'Prelude.Show'/'Prelude.Read' is that 'UStrType' is
-- not intented to be used to construct parsers, it is used as an abstract interface to a parser.
-- 'Prelude.Read' provides 'Prelude.lex' for taking a lexeme from the head of the input,
-- 'Prelude.readParen' for parsing items from within parentheses, and 'readsPrec' which
-- parameterizes the current precedence value and allows you to backtrack if a lexeme has a lower
-- prescedence. All of this functionality (and more) is provided in the "Dao.Parser"
-- module, it is not provided here in the 'UStrType'.
-- 
-- When instantiating this class, you will may find the 'uchars' and 'ustr' to be useful if parsing
-- strings is necessary. If you want to use 'Prelude.Show' to instantiate 'toUStr', you can simply
-- use 'derive_toUStr' and 'derive_fromUStr'. The 'uchars' function is used to convert any
-- 'UStrType' to a 'Prelude.String' by first converting the 'UStrType' to a 'UStr', and 'ustr' is
-- does the inverse, however 'UStr' also instantiates 'UStrType' /so the way to convert a 'UStr' to
-- a 'Prelude.String' is to use 'uchars', the way to convert a 'Prelude.String' to a 'UStr' is to
-- use 'ustr'/. 
class UStrType a where
  -- | Like 'Prelude.Show.show', converts your data type to a universal string.
  toUStr :: a -> UStr
  -- | Like 'Prelude.read', constructs your data type from a universal string.
  fromUStr :: UStr -> a
  fromUStr str = maybe (error ("cannot construct data from UStr "++show str)) id (maybeFromUStr str)
  -- | Like 'Prelude.reads' except the entire string must be consumed, and the return type is a
  -- 'Prelude.Maybe' instead of a list. The return type here is not similar to
  -- 'Prelude.ReadS' which is a synonym for @'Prelude.String' -> [(a, 'Prelude.String')]@ a pair
  -- containing the read object and the remainder.
  maybeFromUStr :: UStr -> Maybe a
  maybeFromUStr = Just . fromUStr
  nil :: a
  nil = fromUStr mempty
instance UStrType UStr where { toUStr = id; fromUStr = id; }
instance UStrType String where
  toUStr = UStr . U.fromString
  fromUStr = U.toString . toUTF8ByteString

-- | This function lets you use the instantiation of 'Prelude.Show' to instantiate 'toUStr',
-- typically used when your data type uses Haskell's @deriving@ keyword to derive 'Prelude.Show'.
-- Note that this function also requires you to instantiate 'Prelude.Read' (also, perhaps, by the
-- @deriving@ keyword), because although this function does not use any of the 'Prelude.Read'
-- functions, this requirement emphasizes the importance of 'UStr' being a data structure that is
-- used to store an intermediate representation of structured data.
derive_ustr :: (Enum a, Read a, Show a) => a -> UStr
derive_ustr = toUStr . show

-- | This function lets you use the instantiation of 'Prelude.Read' to instantiate 'toUStr',
-- typically used when your data type uses Haskell's @deriving@ keyword to derive 'Prelude.Read'.
-- Note that this function also requires you to instantiate 'Prelude.Show' (also, perhaps, by the
-- @deriving@ keyword), because although this function does not use any of the 'Prelude.Show'
-- functions, this requirement emphasizes the importance of 'UStr' being a data structure that is
-- used to store an intermediate representation of structured data.
derive_fromUStr :: (Enum a, Read a, Show a) => UStr -> a
derive_fromUStr = read . uchars

-- | This function lets you use the instantiation of 'Prelude.Read' to instantiate 'toUStr',
-- typically used when your data type uses Haskell's @deriving@ keyword to derive 'Prelude.Read'.
-- Note that this function also requires you to instantiate 'Prelude.Show' (also, perhaps, by the
-- @deriving@ keyword), because although this function does not use any of the 'Prelude.Show'
-- functions, this requirement emphasizes the importance of 'UStr' being a data structure that is
-- used to store an intermediate representation of structured data.
derive_maybeFromUStr :: (Enum a, Read a, Show a) => UStr -> Maybe a
derive_maybeFromUStr u = case reads (uchars u) of
  [(a, "")] -> Just a
  _         -> Nothing

-- | Convert a 'Prelude.String' to an object classed as a 'UStrType' by first converting it to a
-- 'UStr' using 'toUStr'. /NOTE:/ this is the function you use to convert a 'Prelude.String' to a
-- 'UStr', and for the 'UStr' type, this function never fails (never evaluates to the "bottom"
-- value).
ustr :: UStrType str => String -> str
ustr = fromUStr . toUStr

-- | Convert a 'Prelude.String' to an object classed as a 'UStrType' by first converting it to a
-- 'UStr' using 'toUStr', but uses 'maybeFromUStr' to convert from the 'UStrType' object. /NOTE:/
-- this is the function you use to convert a 'Prelude.String' to a 'UStr' (this is possible because
-- 'UStr' instantiates 'UStrType'), and for the 'UStr' type, this function never evaluates to
-- 'Prelude.Nothing'.
maybeUStr :: UStrType str => String -> Maybe str
maybeUStr = maybeFromUStr . toUStr

-- | Convert an object classed as a 'UStrType' to a 'Prelude.String'. /NOTE:/ this is the function
-- you should use to convert a 'UStr' to a 'Prelude.String' (this is possible because 'UStr'
-- instantiates 'UStrType').
uchars :: UStrType str => str -> String
uchars = U.toString . toUTF8ByteString . toUStr

-- | Convert an object classed as a 'UStrType' to a @['Data.Word.Word8']@ list. Since 'UStr's store
-- data as UTF-8 encoded strings, this function simply returns the UTF-8 formatted octet stream from
-- that the 'Data.ByteString.Lazy.UTF8.ByteString' data structure. Of course, unless your 'UStrType'
-- is simply a @newtype@ of 'UStr' a conversion to a 'UStr' is done behind the scenes, which will
-- transparently encode a UTF8 string.
utf8bytes :: UStrType str => str -> [Word8]
utf8bytes = UTF8.encode . uchars . toUStr

-- | The inverse of 'utf8bytes', tries to decode a stream of octets into a properly formatted UTF-8
-- 'Data.ByteString.Lazy.UTF8.ByteString'. If encoding fails, this function evaluates to
-- 'Prelude.error' (evaluates to the "bottom" value).
upack :: [Word8] -> UStr
upack ax = toUStr (UTF8.decode ax)

----------------------------------------------------------------------------------------------------

-- | A Variable-Length Integer (VLI) encoder. The bits of a variable-length integer will have a
-- format like so:
-- >      bit column number: 7 6543210
-- >                         ---------
-- > 1st highest order byte: 1 XXXXXXX
-- > 2nd highest order byte: 1 XXXXXXX
-- > 3rd highest order byte: 1 XXXXXXX
-- > ...
-- > lowest order byte     : 0 XXXXXXX
-- If the highest-order bit is a one, it indicates there are more bytes to follow. If the highest
-- order bit is 0, then there are no more bytes. The 7 lower-order bits will be concatenated in
-- /big-endian order/ to form the length value for the string. By this method, most all strings
-- will have a length prefix of only one or two bytes.
vlIntegralToWord8s :: (Integral a, Bits a) => a -> [Word8]
vlIntegralToWord8s = reverse . (\ (a:ax) -> (a .&. 0x7F) : ax) .
  fix (\loop w -> let v = 0x80 .|. fromIntegral (w .&. 0x7F)
                  in  case shiftR w 7 of{ 0 -> [v]; w -> v : loop w; })

-- | Inverse operation of 'bitsToVLI'
vlWord8sToIntegral :: (Integral a, Bits a) => [Word8] -> (a, [Word8])
vlWord8sToIntegral = loop 0 where
  fn   a w  = shiftL a 7 .|. fromIntegral (w .&. 0x7F) 
  loop a wx = case wx of
    []   -> (a, [])
    w:wx -> if w .&. 0x80 == 0 then (fn a w, wx) else loop (fn a w) wx

-- | Since a negative number expressed in a 'Prelude.Integer' type translates to an infinite
-- sequence of 0xFF bytes when converting it to a VLI, it needs to be encoded specially with a
-- negation bit in the very first position.
vlIntegerToWord8s :: Integer -> [Word8]
vlIntegerToWord8s w = reverse $ (\ (b:bx) -> (if w<0 then b .|. 0x40 else b):bx) $ loop (abs w) where
  loop w = fromInteger (w .&. 0x3F) :
    fix (\loop w -> case w of
            0 -> []
            w -> (0x80 .|. fromInteger (w .&. 0x7F)) : loop (shiftR w 7)
        ) (shiftR w 6)

vlWord8sToInteger :: [Word8] -> (Integer, [Word8])
vlWord8sToInteger = loop 0 where
  fn s m a w  = shiftL a s .|. fromIntegral (w .&. m)
  loop a wx = case wx of
    []   -> (a, [])
    w:wx ->
      if w .&. 0x80 == 0
        then ((if w .&. 0x40 == 0 then id else negate) $ fn 6 0x3F a w, wx)
        else loop (fn 7 0x7F a w) wx

-- | When reading from a binary file, gather the bits of a Variable-Length Integer.
vlGatherWord8s :: B.Get [Word8]
vlGatherWord8s = loop [] where
  loop wx = B.getWord8 >>= \w -> if w .&. 0x80 == 0 then return (wx++[w]) else loop (wx++[w])

-- | Encode only positive 'Prelude.Integer's. This differs from 'vlPutInteger' in that the sign of
-- the integer is not stored in the byte stream, saving a single bit of space. This can actually
-- simplify some equations that expect an VLInteger to be encoded as a multiple-of-7 length string
-- of bits as you don't need to make additional rules for the final byte which would only have
-- 6-bits if the sign is stored with it.
vlPutPosInteger :: Integer -> B.Put
vlPutPosInteger i = assert (i>=0) $ mapM_ B.putWord8 $ vlIntegralToWord8s $ i

-- | Decode only positive 'Prelude.Integer's. This differs from 'vlPutInteger' in that the sign of
-- the integer is not stored in the byte stream, saving a single bit of space. This can actually
-- simplify some equations that expect an VLInteger to be encoded as a multiple-of-7 length string
-- of bits as you don't need to make additional rules for the final byte which only have 6-bits if
-- the sign is stored with it.
vlGetPosInteger :: B.Get Integer
vlGetPosInteger = fmap (fst . vlWord8sToIntegral) vlGatherWord8s

-- | Encode a positive or negative 'Prelude.Integer' using 'vlWord8sToInteger'. The sign of the integer
-- is stored in the final byte in the list of encoded bytes, so the final encoded byte only has 6
-- bits of information, rather than 7 in the case of positive integers.
vlPutInteger :: Integer -> B.Put
vlPutInteger = mapM_ B.putWord8 . vlIntegerToWord8s

-- | Decode a positive or negative 'Prelude.Integer' using 'vlWord8sToInteger'. The sign of the integer
-- is stored in the final byte in the list of encoded bytes, so the final encoded byte only has 6
-- bits of information, rather than 7 in the case of positive integers.
vlGetInteger :: B.Get Integer
vlGetInteger = fmap (fst . vlWord8sToInteger) vlGatherWord8s

-- | Return the length of the 'UStr'.
ulength :: UStr -> Int
ulength = U.length . toUTF8ByteString

-- | Length of a list, but unlike 'Data.List.length', allows a polymorphic length type.
iLength :: Num len => [a] -> len
iLength = foldl (+) 0 . map (const 1)

-- | Used to encode a 'UStr' data type without any prefix at all. The instantiation of 'UStr' into
-- the 'Data.Binary.Binary' class places a prefix before every 'UStr' as it is serialized, allowing
-- it to be used more safely in more complex data types.
encodeUStr :: UStr -> B.Put
encodeUStr u = mapM_ B.putWord8 $
  vlIntegralToWord8s (U.length (toUTF8ByteString u)) ++ (UTF8.encode (uchars u))

-- | Used to decode a 'UStr' data type without any prefix. The instantiation of 'UStr' into the
-- 'Data.Binary.Binary' class places a prefix before every 'UStr' as it is serialized, allowing it
-- to be used more safely in more complex data types.
decodeUStr :: B.Get UStr
decodeUStr = do
  (strlen, undecoded) <- fmap vlWord8sToIntegral vlGatherWord8s
  if null undecoded
    then  fmap (toUStr . (UTF8.decode)) (replicateM strlen B.getWord8)
    else  fail "binary data decoder failed on UStr"

----------------------------------------------------------------------------------------------------

instance IsString UStr where { fromString = ustr }
instance Read UStr where { readsPrec n str = map (\ (s, rem) -> (toUStr (s::String), rem)) $ readsPrec n str }
instance Show UStr where { show u = show (uchars u) }
instance B.Binary UStr where
  put u = encodeUStr u
  get   = decodeUStr
instance NFData UStr where { rnf (UStr a) = deepseq a () }

-- | A type synonym for 'UStr' used where a string is used as some kind of identifier.
newtype Name = Name { nameToUStr :: UStr } deriving (Eq, Ord, Typeable)
instance Monoid Name where { mempty = nil; mappend (Name a) (Name b) = Name (a<>b); }
instance Show Name where { show = show . nameToUStr }
instance UStrType Name where
  toUStr = nameToUStr
  maybeFromUStr nm = 
    let str = uchars nm
        ck f c = c=='_' || f c
    in  case str of 
          c:cx | ck isAlpha c || and (fmap (ck isAlphaNum) cx) -> Just (Name nm)
          _ -> Nothing
  fromUStr = maybe (error msg) id . maybeFromUStr where
    msg = "'Dao.String.Name' object must be constructed from alpha-numeric and underscore characters only"
instance IsString Name where { fromString = ustr }
instance B.Binary Name where
  put (Name u) = encodeUStr u
  get = decodeUStr >>= \u -> case maybeFromUStr u of
    Just nm -> return nm
    Nothing -> fail "binary data contains invalid 'Dao.String.Name' object"
instance NFData Name where { rnf (Name a) = deepseq a () }

-- | A type synonym for 'UStr' used where a string is storing a file path or URL.
type UPath = UStr

----------------------------------------------------------------------------------------------------

-- | Breaks a long list into a list of lists no longer than the specified length.
breakInto :: Int -> [a] -> [[a]]
breakInto i bx = if null bx then [] else let (grp, bx') = splitAt i bx in grp : breakInto i bx'

-- | An array mapping 6-bit values to base-64 character symbols
base64Symbols :: UArray Word Char
base64Symbols = listArray (0,63) (['A'..'Z']++['a'..'z']++['0'..'9']++"+/")

-- | Encoding arbitrary bytes in a 'Data.ByteString.Lazy.ByteString' to base-64 character symbols
-- according to RFC 3548.
b64Encode :: B.ByteString -> [[Char]]
b64Encode = breakInto 76 . concatMap enc . breakInto 3 . B.unpack where
  windows = [(0xFC0000, 18), (0x03F000, 12), (0x000FC0, 6), (0x00003F, 0)]
  enc [] = []
  enc bx =
    let len = length bx
        buf = foldl (\buf b -> shiftL buf 8 .|. fromIntegral b) 0 (take 3 (bx++replicate (3-len) 0))
    in  take 4 $ (++"==") $ take (len+1) $ flip map windows $ \ (mask, shft) ->
          base64Symbols ! shiftR (mask.&.buf) shft

-- | An array mapping base-64 character symbols to their 6-bit values.
base64Values :: UArray Char Int
base64Values = array ('+', 'z') $ concat $
  [ zip ['+', 'z'] (repeat 0xAAAAAAA) -- 0xAAAAAAA is the undefined value
  , zip ['A'..'Z']  [0..25]
  , zip ['a'..'z'] [26..51]
  , zip ['0'..'9'] [52..61]
  , [('+', 62), ('/', 63), ('=', 0xFFFFFFF)] -- 0xFFFFFFF is the end-of-input value
  ]

-- | Decoding base-64 character symbols according to RFC 3548 into a string of bytes stored in a
-- 'Data.ByteString.Lazy.ByteString'. If decoding fails, the invalid character and it's position in
-- the input string are returned as a pair in a 'Data.Either.Left' value, otherwise the
-- 'Data.ByteString.Lazy.ByteString' is returned as the 'Data.Either.Right' value.
b64Decode :: [Char] -> Either (Char, Word64) B.ByteString
b64Decode = loop 0 [] . breakInto 4 . filter (flip notElem " \t\r\n\v\f\0") where
  loop i bx cxx = case cxx of
    []     -> Right (B.pack bx)
    cx:cxx -> case sum 0 0 i cx of
      Left  (c, i)   -> Left (c, i)
      Right (i, bx') -> loop i (bx++bx') cxx
  sum tk b i cx = case cx of
    []   -> Right (i, take (3-tk) (splitup b))
    c:cx -> if inRange (bounds base64Values) c
              then  case base64Values!c of
                      0xAAAAAAA -> Left (c, i)
                      0xFFFFFFF -> sum (tk+1) (shiftL b 6)       (i+1) cx
                      c         -> sum  tk    (shiftL b 6 .|. c) (i+1) cx
              else Left (c, i)
  splitup b = map fromIntegral [shiftR (b.&.0xFF0000) 16, shiftR (b.&.0xFF00) 8, b.&.0xFF]

newtype Base64String = Base64String B.ByteString deriving Typeable
instance Show Base64String where { show (Base64String s) = unlines (b64Encode s) }
instance Read Base64String where
  readsPrec _ str =
    case partition (\c -> isSpace c || (inRange (bounds base64Values) c && base64Values!c /= 0xAAAAAAA)) str of
      ("" , _  ) -> []
      (str, rem) -> case b64Decode $ filter (not . isSpace) str of
        Left (ch, pos) -> error ("invalid charcter "++show ch++" at index "++show pos++" in base64-encoded string")
        Right u        -> [(Base64String u, rem)]
newtype Base16String = Base16String B.ByteString deriving Typeable
instance Show Base16String where
  show (Base16String u) = unlines $ fmap (unwords . fmap hex) $ breakInto 32 (B.unpack u) where
    hex b = [arr ! (shiftR (b.&.0xF0) 4), arr ! (b.&.0x0F)]
    arr :: Array Word8 Char
    arr = array (0,15) (zip [0..15] "0123456789ABCDEF")

showHex :: (Show i, Integral i) => i -> String
showHex = ("0x"++) . map toUpper . flip Numeric.showHex ""

----------------------------------------------------------------------------------------------------

-- | This is a simlpe string tokenizer for breaking up strings into tokens that can be easily used
-- in rules in Doa scripts.
simpleTokenizer :: String -> [String]
simpleTokenizer = fix $ \loop cx -> case cx of
  ""   -> []
  c:cx -> maybe ([c] : loop cx) (\ (cx, rem) -> cx : loop rem) $
    foldl (\f split -> mplus f (split (c:cx))) Nothing $ concat $
      [ fmap (\predicate (c:cx) -> guard (predicate c) >> Just (span predicate (c:cx))) $
          [isSpace, isNumber, isAlpha]
      , [\ (c:cx) -> guard (elem c "([{<>}])") >> Just ([c], cx)]
      , [\ (c:cx) -> Just $ span (==c) (c:cx)]
      ]

