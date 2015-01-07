-- "Dao/Int.hs"  classes for working with Int data types, especially binary
-- encoding.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

----------------------------------------------------------------------------------------------------

-- | Provides a Variable-Length Integer (VLI) encoder. The bits of a
-- variable-length integer will have a format like so:
--
-- >      bit column number: 7 6543210
-- >                         ---------
-- > 1st highest order byte: 1 XXXXXXX
-- > 2nd highest order byte: 1 XXXXXXX
-- > 3rd highest order byte: 1 XXXXXXX
-- > ...
-- > lowest order byte     : 0 XXXXXXX
--
-- If the highest-order bit is a one, it indicates there are more bytes to follow. If the highest
-- order bit is 0, then there are no more bytes. The 7 lower-order bits will be concatenated in
-- /big-endian order/ to form the length value for the string. By this method, most all strings
-- will have a length prefix of only one or two bytes.
module Dao.Int where

import           Data.Bits
import qualified Data.Binary          as B
import           Data.Function
import           Data.Word

----------------------------------------------------------------------------------------------------

minMax :: Ord a => [a] -> Maybe (a, a)
minMax ax = if null ax then Nothing else Just $
  foldl (\ (lo, hi) a -> (min a lo, max a hi)) (head ax, head ax) ax

-- | Length of a list, but unlike 'Data.List.length', allows a polymorphic length type.
iLength :: Num len => [a] -> len
iLength = foldl (+) 0 . map (const 1)

----------------------------------------------------------------------------------------------------

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

-- | Since a negative number expressed in a 'Prelude.Integer' type translates to an whole
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
vlPutPosInteger i = if (i>=0) then mapM_ B.putWord8 $ vlIntegralToWord8s $ i else
  error "vlPutPosInteger received negative input integer value"

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

