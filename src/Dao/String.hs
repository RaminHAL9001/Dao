-- "src/Dao/String.hs"  provides the fundamental string data type
-- called "UStr" which is used throughout the Dao System.
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

module Dao.String where

import           Control.Monad
import           Control.Monad.State

import           Data.Function
import           Data.Typeable
import qualified Data.Binary               as B
import           Data.Bits
import           Data.Word
import           Data.Array.Unboxed
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy      as B
import qualified Codec.Binary.UTF8.String  as UTF8

uStrBinaryPrefix :: Word8
uStrBinaryPrefix = 0x01

-- | UStr's are stored using a Variable-Length Integer (VLI) prefix to indicate the length of the
-- string. The bits of a variable-length integer will have a format like so:
-- @     bit column number: 7 6543210@
-- @                        ---------@
-- @1st highest order byte: 1 XXXXXXX@
-- @2nd highest order byte: 1 XXXXXXX@
-- @3rd highest order byte: 1 XXXXXXX@
-- @...@
-- @lowest order byte     : 0 XXXXXXX@
-- If the highest-order bit is a one, it indicates there are more bytes to follow. If the highest
-- order bit is 0, then there are no more bytes. There will be a maximum of 9 bytes. The 7
-- lower-order bits will be concatenated in big-endian order to form the length value for the
-- string. By this method, most all strings will have a length prefix of only one or two bytes.
bitsToVLInt :: (Integral a, Bits a) => a -> [Word8]
bitsToVLInt w = reverse (zeroLowest (loop w)) where
  zeroLowest (w:wx) = w .&. 0x7F : wx
  loop w = fromIntegral ((w .&. 0x7F) .|. 0x80) : case shiftR w 7 of
    w | w==0 -> []
    w        -> loop w

-- | Inverse operation of 'bitsToVLI'
vlIntToBits :: (Integral a, Bits a) => [Word8] -> (a, [Word8])
vlIntToBits wx = loop 0 wx where
  fn a w = shiftL a 7 .|. fromIntegral (w .&. 0x7F) 
  loop a wx = case wx of
    []   -> (a, [])
    w:wx -> if w .&. 0x80 == 0 then (fn a w, wx) else loop (fn a w) wx

-- | When reading from a binary file, gather the bits of a Variable-Length Integer.
gatherVLInt :: B.Get [Word8]
gatherVLInt = loop [] where
  loop wx = B.getWord8 >>= \w -> if w .&. 0x80 == 0 then return (wx++[w]) else loop (wx++[w])

getFromVLInt :: (Integral a, Bits a) => B.Get a
getFromVLInt = fmap (fst . vlIntToBits) gatherVLInt

-- | A type synonym for 'Data.ByteString.Lazy.UTF8.ByteString'
newtype UStr = UStr { toUTF8ByteString :: U.ByteString } deriving (Eq, Ord, Typeable)

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
  bitsToVLInt (U.length (toUTF8ByteString u)) ++ (UTF8.encode (uchars u))

-- | Used to decode a 'UStr' data type without any prefix. The instantiation of 'UStr' into the
-- 'Data.Binary.Binary' class places a prefix before every 'UStr' as it is serialized, allowing it
-- to be used more safely in more complex data types.
decodeUStr :: B.Get UStr
decodeUStr = do
  (strlen, undecoded) <- fmap vlIntToBits gatherVLInt
  if null undecoded
    then fmap (ustr . (UTF8.decode)) (replicateM strlen B.getWord8)
    else
      error $ concat $
        ["binary data decoder failed, "
        ,"at least one string in this file has corrupted string length prefix"
        ]

instance Read UStr where { readsPrec n str = map (\ (s, rem) -> (ustr s, rem)) $ readsPrec n str }
instance Show UStr where { show u = show (uchars u) }
instance B.Binary UStr where
  put u = B.putWord8 uStrBinaryPrefix >> encodeUStr u
  get = do
    w <- B.getWord8
    if w==uStrBinaryPrefix
      then decodeUStr
      else error "binary data decoder failed while on expecting U-String"

uconcat :: [UStr] -> UStr
uconcat = UStr . U.fromString . concatMap (U.toString . toUTF8ByteString)

-- | A type synonym for 'UStr' used where a string is used as some kind of identifier.
type Name = UStr

-- | A type synonym for 'UStr' used where a string is storing a file path or URL.
type UPath = UStr

-- | Get the [Char] string version of the 'Name' or 'Dao.Types.UStr' object.
uchars :: UStr -> String
uchars = U.toString . toUTF8ByteString

-- | Convert a [Char] string to either a 'Name' or a 'Dao.Types.UStr' object.
ustr :: String -> UStr
ustr = UStr . U.fromString

uwords :: UStr -> [Word8]
uwords str = UTF8.encode (uchars str)

upack :: [Word8] -> UStr
upack ax = ustr (UTF8.decode ax)

nil :: UStr
nil = ustr ""

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

