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

import           Data.Typeable
import           Data.Binary
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy      as B
import qualified Codec.Binary.UTF8.String  as UTF8

uStrBinaryPrefix :: Word8
uStrBinaryPrefix = 0x01

-- | A type synonym for 'Data.ByteString.Lazy.UTF8.ByteString'
newtype UStr = UStr { toUTF8ByteString :: U.ByteString } deriving (Eq, Ord, Typeable)
instance Read UStr where { readsPrec n str = map (\(s, rem) -> (ustr s, rem)) $ readsPrec n str }
instance Show UStr where { show u = show (uchars u) }
instance Binary UStr where
  put u = putWord8 uStrBinaryPrefix >> put (B.pack $ (UTF8.encode) $ uchars u)
  get = do
    w <- getWord8
    if w==uStrBinaryPrefix
      then get >>= return . ustr . (UTF8.decode) . (B.unpack)
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

