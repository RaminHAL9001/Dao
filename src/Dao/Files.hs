-- "src/Dao/Files.hs"  functions to load Dao Documents and Dao programs
-- from files in the operating system.
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


-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module takes care of loading Dao 'Dao.Object.SourceCode' and Dao 'Dao.Object.Document's.

module Dao.Files where

import           Dao.Debug.OFF
import           Dao.Object
import           Dao.Resource
import qualified Dao.Tree    as T
import           Dao.Parser hiding (lookAhead)
import           Dao.Predicate
import           Dao.Object.Parser
import           Dao.Object.Show
import           Dao.Object.Binary

import           Control.Exception
import           Control.Concurrent
import           Control.Monad.Reader

import           Data.Maybe
import           Data.List (partition, isSuffixOf)
import qualified Data.Map    as M

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as B

import           System.IO
import           System.Directory

----------------------------------------------------------------------------------------------------

putStrErr :: String -> IO ()
putStrErr msg = hSetBuffering stderr LineBuffering >> hPutStrLn stderr msg

-- | Converts a 'System.IO.FilePath' to a 'Dao.Object.UPath' that can be used by many of the
-- functions. in this module.
docRefToFilePath :: UPath -> FilePath
docRefToFilePath = uchars

instance Show (StoredFile T.Tree Name Object) where
  show doc =
    "document("++show (docRefCount doc)++","++show (docModified doc)++") = "++show (docInfo doc)

instance Binary (StoredFile T.Tree Name Object) where
  put doc = do
    putWord64be document_magic_number
    putWord64be document_data_version
    put (docInfo doc)
    putWord64be (docVersion doc)
    put (docRootObject doc)
  get = do
    n <- bytesRead
    dat <- lookAhead (mapM (\_ -> getWord8) [0..n])
    magic_number <- getWord64be -- why does getWord64be return zero?
    n <- bytesRead
    data_version <- getWord64be
    let chk a b msg = if a==b then return () else error msg
    chk magic_number document_magic_number "This does not appear to be a Dao data file."
    chk data_version document_data_version "This file Dao data appears to of the wrong version."
    info <- get
    ver  <- getWord64be
    root <- get
    seq root $ return $
      StoredFile
      { docRefCount   = 1
      , docModified   = 0
      , docInfo       = info
      , docVersion    = ver
      , docRootObject = root
      }

----------------------------------------------------------------------------------------------------

-- | Parse Dao program from a 'Prelude.String' containing valid Dao source code, creating a
-- 'Dao.Object.SourceCode' object. This is a pure function called by 'loadFilePath'.
loadSourceCode :: UPath -> String -> SourceCode
loadSourceCode upath sourceString = case fst (runParser parseSourceFile sourceString) of
  Backtrack     -> error ("FILE TYPE: "++show path++" does not appear to be a Dao script.")
  PFail tok msg -> error (path++':':show tok++"\n\t"++uchars msg)
  OK src        -> src
  where { path = uchars upath }

-- | This function will take any file path and return a file associated with it if it has been
-- loaded once before. If not, it runs the function you provide to load the file.
dontLoadFileTwice :: UPath -> (UPath -> Run (ContErr File)) -> Run (ContErr File)
dontLoadFileTwice upath getFile = do
  runtime <- ask
  ptab <- fmap (M.lookup upath) (dReadMVar xloc (pathIndex runtime))
  case ptab of
    Just file -> return (CENext file)
    Nothing   -> getFile upath

-- | Load idea data from the given file handle. You must pass the path name to store the resulting
-- 'File' into the 'Dao.Object.pathIndex' table.
ideaLoadHandle :: UPath -> Handle -> Run DocResource
ideaLoadHandle upath h = ask >>= \runtime -> do
  lift (hSetBinaryMode h True)
  doc <- lift (fmap (decode$!) (B.hGetContents h) >>= evaluate)
  docHandle <- newDocResource ("DocHandle("++show upath++")") $! doc
  dModifyMVar_ xloc (pathIndex runtime) $ \pathTab ->
    seq pathTab $! seq docHandle $! return $! M.insert upath (DocumentFile docHandle) pathTab
  dPutStrErr xloc ("Loaded data file "++show upath)
  return docHandle

-- | Where 'loadFilePath' keeps trying to load a given file by guessing it's type,
-- 'loadFilePathWith' allows you to specify how to load the file by passing a function, like
-- 'ideaLoadHandle' for parsing ideas or 'scriptLoadHandle' for parsing scripts, and only files of
-- that type will be loaded, or else this function will fail.
loadFilePathWith :: (UPath -> Handle -> Run File) -> UPath -> Run File
loadFilePathWith fn upath = lift (openFile (uchars upath) ReadMode) >>= fn upath

-- | Tries to convert the given 'Dao.String.UPath' to it's full path using
-- 'System.Directory.canonicalizePath', returning a pair @(True, fullPath)@ if successful. It does not
-- throw an 'Control.Exception.IOException' if the lookup fails, it returns the pair
-- @(False, originalPath)@.
getFullPath :: UPath -> IO (Bool, UPath)
getFullPath upath = handle (\ (err::IOException) -> return (False, upath)) $
  fmap ((,)True . ustr) (canonicalizePath (uchars upath))

-- | Return all files that have been loaded that match the given file name.
findIndexedFile :: UPath -> Run [File]
findIndexedFile upath = do
  runtime <- ask
  ptab    <- dReadMVar xloc (pathIndex runtime)
  return $ flip concatMap (M.assocs ptab) $ \ (ipath, file) ->
    if isSuffixOf (uchars upath) (uchars ipath) then [file] else []

-- | Once all the files have been loaded, it is possible to check if the @import@ directives of a
-- given Dao script indicate a module name that properly maps to a file that has been loaded. This
-- function preforms that check, and also fills-in the 'Dao.Object.importsTable' of the
-- 'Dao.Object.ExecUnit'. Returns 'Data.Maybe.Nothing' on success. If there is a problem, it returns
-- the name of the module that could not be found.
checkImports :: File -> Run [UPath]
checkImports file = dStack xloc "checkImports[1]" $ ask >>= \runtime -> case file of
  ProgramFile xunit -> do
    pathTab <- dReadMVar xloc (pathIndex runtime)
    dStack xloc "checkImports[2]" $ case programImports xunit of
      []      -> return []
      imports -> do
        let xunits = map (\mod -> (mod, maybeToList (M.lookup mod pathTab))) imports
            (badImports, goodImports) = partition (null . snd) xunits
        return (if null badImports then [] else map fst badImports)
  DocumentFile   _ -> return []
  SourceCodeFile _ -> return []

-- | Like 'checkImports' but checks every file that has been loaded by the Runtime.
checkAllImports :: Run [(UPath, [UPath])]
checkAllImports = ask >>= \runtime -> fmap (filter (not . null . snd) . concat) $ do
  pathTab  <- dReadMVar xloc (pathIndex runtime)
  forM (M.assocs pathTab) $ \ (modName, file) -> do
    notFound <- checkImports file
    return (if null notFound then [] else [(modName, notFound)])

