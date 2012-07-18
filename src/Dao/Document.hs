-- "src/Dao/Document.hs"  functions for managing Dao objects that have
-- been serialized and wirtten to data files (Documents).
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
{-# LANGUAGE FlexibleContexts #-}

module Dao.Document where

import           Prelude hiding (lookup)

import           Dao.Debug.OFF
import           Dao.Types
import           Dao.Object.Binary

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Reader

import           Data.Maybe (fromMaybe)
import           Data.Either
import qualified Data.Map                  as M
import qualified Data.ByteString.Lazy      as B
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           System.Directory
import           System.FilePath
import           System.IO

-- | Converts a 'System.IO.FilePath' to a 'Dao.Types.UPath' that can be used by many of the
-- functions. in this module.
docRefToFilePath :: UPath -> FilePath
docRefToFilePath = uchars

----------------------------------------------------------------------------------------------------

instance Show Document where
  show doc =
    "document("++show (docRefCount doc)++","++show (docModified doc)++") = "++show (docInfo doc)

instance Binary Document where
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
      Document
      { docRefCount   = 1
      , docModified   = 0
      , docInfo       = info
      , docVersion    = ver
      , docRootObject = root
      }

----------------------------------------------------------------------------------------------------

-- | This function can be evaluated in a 'Dao.Types.Run' monad. It simply creates a new
-- 'Dao.Types.DocListHandle'.
newDocList :: Bugged r => ReaderT r IO DocListHandle
newDocList = dNewMVar xloc "DocListHandle" M.empty

-- | This function can be evaluated in a 'Dao.Types.Run' monad. It takes debugging information as
-- well, so pass 'Prelude.Nothing' and an empty string as the first two parameters if you are at a
-- loss. It simply creates a new 'Dao.Types.DocHandle' from a given document (presumably created by
-- 'Data.Binary.decodeFile').
newDocHandle :: Bugged r => MLoc -> String -> Document -> ReaderT r IO DocHandle
newDocHandle loc msg doc = dNewMVar loc msg doc

-- | This function can be evaluated in a 'Dao.Types.Run' monad. Increments the counter that
-- indicates whether or not the 'Dao.Types.Document' in this 'Dao.Types.DocHandle' has been
-- modified.
setModified :: Bugged r => DocHandle -> ReaderT r IO ()
setModified doc = dModifyMVar_ xloc doc $ \doc ->
  return (doc{docModified = 1 + docModified doc})

withDocList :: (DocList -> Run (DocList, a)) -> Run a
withDocList fn = ask >>= \runtime -> dModifyMVar xloc (documentList runtime) fn

withDocList_ :: (DocList -> Run DocList) -> Run ()
withDocList_ fn = ask >>= \runtime -> dModifyMVar_ xloc (documentList runtime) fn

withUPath :: UPath -> (Document -> Run (Document, a)) -> Run a
withUPath docref docUpd = do
  doc <- ask >>= fmap (M.lookup docref) . dReadMVar xloc . documentList
  case doc of
    Just doc -> dModifyMVar xloc doc docUpd
    Nothing  ->
      error ("document reference "++show docref++" has not been loaded from persistent storage")

withUPath_ :: UPath -> (Document -> Run Document) -> Run ()
withUPath_ docref fn = withUPath docref (\doc -> fn doc >>= \doc -> return (doc, ()))

readUPath :: UPath -> (Document -> Run a) -> Run a
readUPath docref fn = withUPath docref (\doc -> fmap ((,)doc) (fn doc))

----------------------------------------------------------------------------------------------------

fullPath :: UPath -> Run UPath
fullPath docref = lift $ fmap ustr (canonicalizePath (uchars docref))

listDocList :: Run [Name]
listDocList = withDocList $ \docs -> return (docs, M.keys docs)

docIsOpen :: UPath -> FilePath -> Run Bool
docIsOpen ref path = withDocList $ \docs -> return (docs, M.member (ustr path) docs)

-- | Create a new 'Dao.Types.Document' in the filesystem at the given 'UPath', fail if this 'UPath'
-- already exists. Store the created 'Dao.Types.Document' in the 'Dao.Types.Runtime' associated with
-- the 'Run' monad that evaluates this function.
newDoc :: UPath -> Document -> Run ()
newDoc docref doc = do
  docref <- fullPath docref
  withDocList_ $ \docs -> case M.lookup docref docs of
    Just _  -> error ("document reference "++show docref++" is already loaded")
    Nothing -> do
      doc <- dNewMVar xloc ("DocHandle("++show docref++")") doc
      return (M.insert docref doc docs)

-- | Open a 'Document', do not create it if it does not exist.
openDoc :: UPath -> Run DocHandle
openDoc docref = do
  docref <- fullPath docref
  withDocList $ \docs -> case M.lookup docref docs of
    Just doc -> do
      dModifyMVar_ xloc doc (\doc -> return (doc{docRefCount = 1 + docRefCount doc}))
      return (docs, doc)
    Nothing  -> do
      bytes <- lift (B.readFile (docRefToFilePath docref))
      let doc = seq bytes $ decode bytes
      doc <- seq doc $ dNewMVar xloc ("DocHandle("++show docref++")") doc
      return (M.insert docref doc docs, doc)

-- | Save a persistent data store to a specified file path referring to, or changing, the
-- 'Document's 'docFilePath'.
saveDocAs :: UPath -> UPath -> Run ()
saveDocAs docref path = withUPath_ docref $ \doc -> do
  docref <- fullPath docref
  path   <- fullPath docref
  lift $ do
    h      <- openFile (docRefToFilePath path) WriteMode
    let encoded = encode doc
    B.hPutStr h encoded
    hFlush h >> hClose h
    return (doc{docModified = 0}) >>= evaluate

-- | Save a persistent data store to the same file path from which it was loaded. Returns an error
-- message if the document reference is not defined, or if there is no file path associated with the
-- given document reference.
saveDoc :: UPath -> Run ()
saveDoc docref = saveDocAs docref docref

-- | Save to file every document that has a non-'Data.Maybe.Nothing' name for 'docFilePath'.
saveAll :: Run ()
saveAll = withDocList_ $ \docs -> do
  forM_ (M.assocs docs) $ \ (docref, doc) -> dReadMVar xloc doc >>= \doc ->
    lift (B.writeFile (docRefToFilePath docref) $! encode (docRootObject doc))
  return docs

-- | Remove a 'Document' from the list of open 'Document's. After this, the 'Document' reference can
-- no longer be used until it is re-opened with 'openDoc', possibly under a different name. If the
-- document has not been saved with 'saveDoc', the data contained in the 'Document' will be lost.
closeDoc :: UPath -> Run ()
closeDoc docref = do
  docref <- fullPath docref
  withDocList_ $ \docs -> case M.lookup docref docs of
    Nothing  -> return docs
    Just doc -> dModifyMVar xloc doc $ \doc ->
      if docRefCount doc == 0
        then return (doc, M.delete docref docs)
        else return (doc{docRefCount = docRefCount doc - 1}, docs)

