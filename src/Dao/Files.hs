-- "src/Dao/Files.hs"  functions to load Dao Documents and Dao programs
-- from files in the operating system.
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


-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module takes care of loading Dao 'Dao.Object.SourceCode' and Dao 'Dao.Object.Document's.

module Dao.Files where

import           Dao.Object

import           Data.Binary

import           System.IO

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

----------------------------------------------------------------------------------------------------

putStrErr :: String -> IO ()
putStrErr msg = hSetBuffering stderr LineBuffering >> hPutStrLn stderr msg

-- | Converts a 'System.IO.FilePath' to a 'Dao.Object.UPath' that can be used by many of the
-- functions. in this module.
docRefToFilePath :: UPath -> FilePath
docRefToFilePath = uchars

----------------------------------------------------------------------------------------------------

