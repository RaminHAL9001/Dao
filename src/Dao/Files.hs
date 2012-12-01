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

-- | This module takes care of loading Dao 'Dao.Types.SourceCode' and Dao 'Dao.Types.Document's.

module Dao.Files where

import           Dao.Debug.OFF
import           Dao.Types
import           Dao.Evaluator
import           Dao.Resource
import           Dao.Combination (runCombination)
import qualified Dao.Tree    as T
import           Dao.Parser
import           Dao.Object.Parser
import           Dao.Object.Show
import           Dao.Object.Monad

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

import Debug.Trace

----------------------------------------------------------------------------------------------------

putStrErr :: String -> IO ()
putStrErr msg = hSetBuffering stderr LineBuffering >> hPutStrLn stderr msg

-- | Converts a 'System.IO.FilePath' to a 'Dao.Types.UPath' that can be used by many of the
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

newDocResource :: Bugged r => String -> T_tree -> ReaderT r IO DocResource
newDocResource dbg docdata = do
  resource <- newDMVarsForResource dbg "DocResource" (initDoc docdata) (NotStored T.Void)
  let lookup ref d = T.lookup ref (docRootObject d)
      updater ref obj d = d{ docRootObject = T.update ref (const obj) (docRootObject d) }
  return $
    resource
    { updateUnlocked = \ref obj d -> (updater ref obj d){ docModified = 1 + docModified d }
    , updateLocked   = updater
    , lookupUnlocked = lookup
    , lookupLocked   = lookup
    }

----------------------------------------------------------------------------------------------------

-- | Parse Dao program from a 'Prelude.String' containing valid Dao source code, creating a
-- 'Dao.Types.SourceCode' object. This is a pure function called by 'loadFilePath'.
loadSourceCode :: UPath -> String -> SourceCode
loadSourceCode upath sourceString = case fst (runParser parseSourceFile sourceString) of
  Backtrack     -> error ("FILE TYPE: "++show path++" does not appear to be a Dao script.")
  PFail tok msg -> error (path++':':show tok++"\n\t"++uchars msg)
  OK src        -> src
  where { path = uchars upath }

-- | This function will take any file path and return a file associated with it if it has been
-- loaded once before. If not, it runs the function you provide to load the file.
dontLoadFileTwice :: UPath -> (UPath -> Run File) -> Run File
dontLoadFileTwice upath getFile = do
  runtime <- ask
  ptab <- fmap (M.lookup upath) (dReadMVar xloc (pathIndex runtime))
  case ptab of
    Just file -> return file
    Nothing   -> getFile upath

-- | Updates the 'Runtime' to include the Dao source code loaded from the given 'FilePath'. This
-- function tries to load a file in three different attempts: (1) try to load the file as a binary
-- @('Dao.Document.Document' 'Dao.Evaluator.DocData')@ object. (2) Try to load the file as a
-- binary @'Dao.Types.SourceCode'@ object. (3) Treat the file as text (using the current locale set
-- by the system, e.g. @en.UTF-8@) and parse a 'Dao.Types.SourceCode' object using
-- 'Dao.Object.Parsers.source'. If all three methods fail, an error is thrown. Returns
-- the 'TypedFile', although all source code files are returned as 'PrivateType's. Use
-- 'asPublic' to force the type to be a 'PublicType'd file.
loadFilePath :: Bool -> FilePath -> Run File
loadFilePath public path = dontLoadFileTwice (ustr path) $ \upath -> do
  dPutStrErr xloc ("Lookup file path "++show upath)
  h    <- lift (openFile path ReadMode)
  zero <- lift (hGetPosn h)
  enc  <- lift (hGetEncoding h)
  -- First try to load the file as a binary program file, and then try it as a binary data file.
  file <- catchErrorCall (ideaLoadHandle upath h)
  case file of
    Right file -> return file
    Left  _    -> do -- The file does not seem to be a document, try parsing it as a script.
      lift (hSetPosn zero >> hSetEncoding h (fromMaybe localeEncoding enc))
      scriptLoadHandle public upath h

-- | Load a Dao script program from the given file handle. You must pass the path name to store the
-- resulting 'File' into the 'Dao.Types.pathIndex' table. The handle must be set to the proper
-- encoding using 'System.IO.hSetEncoding'.
scriptLoadHandle :: Bool -> UPath -> Handle -> Run File
scriptLoadHandle public upath h = do
  script <- lift $ do
    hSetBinaryMode h False
    fmap (loadSourceCode upath) (hGetContents h) >>= evaluate
  file <- registerSourceCode public upath script
  dPutStrErr xloc ("Loaded source code "++show upath) >> return file
  return file

-- | Load idea data from the given file handle. You must pass the path name to store the resulting
-- 'File' into the 'Dao.Types.pathIndex' table.
ideaLoadHandle :: UPath -> Handle -> Run File
ideaLoadHandle upath h = ask >>= \runtime -> do
  lift (hSetBinaryMode h True)
  doc <- lift (fmap (decode$!) (B.hGetContents h) >>= evaluate)
  docHandle <- newDocResource ("DocHandle("++show upath++")") $! doc
  let file = IdeaFile{filePath = upath, fileData = docHandle}
  --  dModifyMVar_ xloc (documentList runtime) $ \docTab ->
  --    seq docTab $! seq docHandle $! lift $! return $! M.insert upath docHandle docTab
  dModifyMVar_ xloc (pathIndex runtime) $ \pathTab ->
    seq pathTab $! seq file $! return $! M.insert upath file pathTab
  dPutStrErr xloc ("Loaded data file "++show upath)
  return file

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
findIndexedFile :: UPath -> Run [UPath]
findIndexedFile upath = do
  runtime <- ask
  ptab    <- dReadMVar xloc (pathIndex runtime)
  return $ flip concatMap (M.assocs ptab) $ \ (ipath, file) ->
    if isSuffixOf (uchars upath) (uchars ipath) then [filePath file] else []

-- | Checks that the file path is OK to use for the given 'Dao.Types.ExecUnit', executes the file
-- loading function if it is OK to load, returns a CEError if not.
execReadFile :: UPath -> (UPath -> Handle -> Run File) -> ExecScript File
execReadFile upath fn = do
  xunit <- ask
  (exists, upath) <- execIO (getFullPath upath)
  -- TODO: check if the path is really OK to load.
  execScriptRun $ do
    ~altDoc <- newDocResource ("DocHandle("++show upath++")") T.Void
    let file = IdeaFile{filePath = upath, fileData = altDoc}
    dontLoadFileTwice upath $ \upath ->
      if exists
        then loadFilePathWith fn upath
        else do
          runtime <- ask
          dModifyMVar xloc (pathIndex runtime) $ \ptab ->
            return (M.insert (filePath file) file ptab, file)

-- | If a file has been read, it can be written.
execWriteDB :: UPath -> ExecScript File
execWriteDB upath = do
  (_, upath) <- execIO (getFullPath upath)
  xunit <- ask
  -- TODO: check if the path is OK to write.
  file <- execScriptRun $ do
    runtime <- ask
    fmap (M.lookup upath) (dReadMVar xloc (pathIndex runtime))
  case file of
    Nothing -> ceError $ OList $ map OString $
      [ustr "writeDB", upath, ustr "file is not currently loaded"]
    Just file | isIdeaFile file -> do
      let doc = fileData file
      execRun $ modifyUnlocked doc $ \doc -> do
        when (docModified doc > 0) $ lift $ do
          h <- openFile (docRefToFilePath upath) WriteMode >>= evaluate
          let encoded = encode doc
          seq h $! seq encoded $! B.hPutStr h encoded
          hFlush h >> hClose h >>= evaluate
        return (doc{docModified = 0}, file)

-- | Initialize a source code file into the given 'Runtime'. This function checks that the
-- 'Dao.Types.sourceFullPath' is unique in the 'programs' table of the 'Runtime', then evaluates
-- 'initSourceCode' and associates the resulting 'Dao.Evaluator.ExecUnit' with the
-- 'sourceFullPath' in the 'programs' table. Returns the logical "module" name of the script along
-- with an initialized 'Dao.Types.ExecUnit'.
registerSourceCode :: Bool -> UPath -> SourceCode -> Run File
registerSourceCode public upath script = ask >>= \runtime -> do
  let modName  = unComment (sourceModuleName script)
      pathTab  = pathIndex runtime
      modTab   = logicalNameIndex runtime
  alreadyLoaded <- fmap (M.lookup upath) (dReadMVar xloc pathTab)
  case alreadyLoaded of
    Just file -> case file of
      ProgramFile _ upath' _        _     | upath' /= upath     -> error $
          "INTERNAL ERROR: runtime file table is corrupted\n\ta file indexed as "++show upath
        ++"\n\tis recored as having a filePath of "++show upath'
      ProgramFile _ _      modName' xunit | modName' == modName -> return file
      ProgramFile _ _      modName' _     | modName' /= modName -> error $
          "\t"++show upath++" has been loaded by a different module name.\n"
        ++"\tmodule name "++show modName++"\n"
        ++"\tattempted to load as module name "++show modName
      IdeaFile    _ _                                           -> error $
          "INTERNAL ERROR: "++show upath
        ++" has been loaded as both a data file and as an executable script."
    Nothing   -> do
      moduleNameConflict <- fmap (M.lookup modName) (dReadMVar xloc modTab)
      case moduleNameConflict of
        Just file -> error $
            "ERROR: cannot load source file "++show upath++"\n"
          ++"\tthe module name "++show (logicalName file)
          ++"is identical to that of another Dao script that has been\n"
          ++"\tloaded from the path "++show (filePath file)
        Nothing -> do
          -- Call 'initSourceCode' which creates the 'ExecUnit', then place it in an 'MVar'.
          -- 'initSourceCode' calls 'Dao.Evaluator.programFromSource'.
          xunit     <- initSourceCode script >>= lift . evaluate
          -- Check to make sure the logical name in the loaded program does not conflict with that
          -- of another loaded previously.
          xunitMVar <- dNewMVar xloc ("ExecUnit("++show upath++")") xunit
          let file =
                ProgramFile
                { publicFile  = public
                , filePath    = upath
                , logicalName = unComment (sourceModuleName script)
                , execUnit   = xunitMVar
                }
          dModifyMVar_ xloc pathTab $ return . M.insert upath file
          dModifyMVar_ xloc modTab  $ return . M.insert modName file
          return file

-- | You should not normally need to call evaluate this function, you should use
-- 'registerSourceCode' which will evaluate this function and also place the
-- 'Dao.Types.SourceCode' into the 'programs' table. This function will use
-- 'Dao.Evaluator.programFromSource' to construct a 'Dao.Evaluator.CachedProgram'
-- and then execute the initialization code for that program, that is, use
-- 'Dao.Evaluator.execScriptExpr' to evaluate every 'Dao.Types.ExecScript' in the
-- 'Dao.Types.constructScript'. Returns the 'Dao.Evaluator.ExecUnit' used to initialize the
-- program, and the logical name of the program (determined by the "module" keyword in the source
-- code). You need to pass the 'Runtime' to this function because it needs to initialize a new
-- 'Dao.Evaluator.ExecUnit' with the 'programs' and 'runtimeDocList' but these values are not
-- modified.
initSourceCode :: SourceCode -> Run ExecUnit
initSourceCode script = ask >>= \runtime -> do
  grsrc <- newTreeResource "Program.globalData" T.Void
  xunit <- initExecUnit runtime grsrc
  -- An execution unit is required to load a program, so of course, while a program is being
  -- loaded, the program is not in the program table, and is it's 'currentProgram' is 'Nothing'.
  cachedProg <- runExecScript (programFromSource grsrc (\_ _ _ -> return False) script) xunit
  case cachedProg of
    CEError  obj        -> error ("script err: "++showObj 0 obj)
    CENext   cachedProg -> do
      -- Run all initializer scripts (denoted with the @SETUP@ rule in the Dao language).
      let xunit' = xunit{currentProgram = Just cachedProg}
      setupTakedown constructScript xunit'
      -- Place the initialized module into the 'Runtime', mapping to the module's handle.
      return xunit'
    CEReturn _          ->
      error "INTERNAL ERROR: source code evaluation returned before completion"

-- | When a program is loaded, and when it is released, any block of Dao code in the source script
-- that is denoted with the @SETUP@ or @TAKEDOWN@ rules will be executed. This function performs
-- that execution in the current thread.
setupTakedown :: (Program -> [[Com ScriptExpr]]) -> ExecUnit -> Run ()
setupTakedown select xunit = ask >>= \runtime ->
  forM_ (concat $ maybeToList $ currentProgram xunit >>= Just . select) $ \block ->
    runExecScript (execGuardBlock block) xunit >>= lift . evaluate

-- | Once all the files have been loaded, it is possible to check if the @import@ directives of a
-- given Dao script indicate a module name that properly maps to a file that has been loaded. This
-- function preforms that check, and also fills-in the 'Dao.Types.importsTable' of the
-- 'Dao.Types.ExecUnit'. Returns 'Data.Maybe.Nothing' on success. If there is a problem, it returns
-- the name of the module that could not be found.
checkImports :: File -> Run [(Name, [Name])]
checkImports file = ask >>= \runtime -> case file of
  ProgramFile _ _ modName xunit -> do
    pathTab <- dReadMVar xloc (pathIndex runtime)
    modTab  <- dReadMVar xloc (logicalNameIndex runtime)
    dStack xloc "checkImports" $ do
      dModifyMVar xloc xunit $ \xunit -> case currentProgram xunit of
        Nothing   -> return (xunit, [(modName, [])])
        Just prog -> case programImports prog of
          []      -> return (xunit, [])
          imports -> do
            let xunits = map (\mod -> (mod, maybeToList (M.lookup mod modTab))) imports
                (badImports, goodImports) = partition (null . snd) xunits
            return $
              if null badImports
                then (xunit{importsTable = map execUnit (concatMap snd goodImports)}, [])
                else (xunit, [(modName, map fst badImports)])
  IdeaFile _ _ -> return []

-- | Like 'checkImports' but checks every file that has been loaded by the Runtime.
checkAllImports :: Run [(Name, [Name])]
checkAllImports = ask >>= \runtime -> fmap (filter (not . null . snd) . concat) $
  dReadMVar xloc (pathIndex runtime) >>= mapM checkImports . M.elems

