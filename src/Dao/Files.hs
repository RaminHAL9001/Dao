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


{-# LANGUAGE TemplateHaskell #-}

-- | This module takes care of loading Dao 'Dao.Types.SourceCode' and Dao 'Dao.Types.Document's.

module Dao.Files where

import           Dao.Debug.OFF
import           Dao.Types
import           Dao.Document
import           Dao.Evaluator
import           Dao.Combination
import           Dao.Parser
import           Dao.Object.Parsers
import           Dao.Object.Show
import           Dao.Object.Monad

import           Control.Exception
import           Control.Concurrent
import           Control.Monad.Reader

import           Data.Maybe
import           Data.List (partition)
import qualified Data.Map    as M

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B

import           System.IO

putStrErr :: String -> IO ()
putStrErr msg = hSetBuffering stderr LineBuffering >> hPutStrLn stderr msg

----------------------------------------------------------------------------------------------------

-- | Updates the 'Runtime' to include the Dao source code loaded from the given 'FilePath'. This
-- function tries to load a file in three different attempts: (1) try to load the file as a binary
-- @('Dao.Document.Document' 'Dao.Evaluator.DocData')@ object. (2) Try to load the file as a
-- binary @'Dao.Types.SourceCode'@ object. (3) Treat the file as text (using the current locale set
-- by the system, e.g. @en.UTF-8@) and parse a 'Dao.Types.SourceCode' object using
-- 'Dao.Object.Parsers.source'. If all three methods fail, an error is thrown. Returns
-- the 'TypedFile', although all source code files are returned as 'PrivateType's. Use
-- 'asPublic' to force the type to be a 'PublicType'd file.
loadFile :: Bool -> FilePath -> Run File
loadFile public path = ask >>= \runtime -> do
  let ok msg = dPutStrErr $loc (msg++' ':show path)
      upath = ustr path
  -- First try to load the file as a binary program file, and then try it as a binary data file.
  lift $ putStrLn ("Loading file "++show upath)
  h    <- lift (openFile path ReadMode)
  zero <- lift (hGetPosn h)
  enc  <- lift (hGetEncoding h)
  lift (hSetBinaryMode h True)
  dat  <- catchErrorCall $ lift $ fmap (B.decode$!) (B.hGetContents h) >>= evaluate
  case dat of
    Right doc    -> do -- The file is a document.
      docHandle <- newDocHandle $loc ("DocHandle("++show path++")") doc
      let file = DataFile{filePath = upath, fileData = docHandle}
      dModifyMVar_ $loc (documentList runtime) (\docTab -> return (M.insert upath docHandle docTab))
      dModifyMVar_ $loc (pathIndex runtime) (\pathTab -> return (M.insert upath file pathTab))
      ok "loaded data file"
      return file
    Left  docErr -> do -- The file does not seem to be a document, try parsing it as a script.
      sourceCode <- lift $ do
        hSetPosn zero
        hSetEncoding h (fromMaybe localeEncoding enc)
        fmap (loadSourceCode path) (hGetContents h) >>= evaluate
      file <- registerSourceCode public upath sourceCode
      ok "loaded source code" >> return file

-- | Parse Dao program from a 'Prelude.String' containing valid Dao source code, creating a
-- 'Dao.Types.SourceCode' object. This is a pure function called by 'loadFile'.
loadSourceCode :: FilePath -> String -> SourceCode
loadSourceCode file sourceString = getFirstCompleteParse parseCode where
  parseCode = runCombination source (parserState{inputString = sourceString})
  getFirstCompleteParse = loop ("FILE TYPE: "++show file++" does not appear to be a Dao script.")
  loop msg px = case px of -- take the first parse that consumes all input.
    [] -> error msg
    (Right sc, st):px -> if null (inputString st) then sc else loop err2 px
    (Left msg, st):px -> loop (err1 msg st) px
  err1 msg st = "PARSE ERROR: "++file
    ++':':show (rowNumber st)++':':show (colNumber st)++": "++printParseError file msg
  err2 = "INTERNAL ERROR: parse succeeded without consuming all source code"

-- | Initialize a source code file into the given 'Runtime'. This function checks that the
-- 'Dao.Types.sourceFullPath' is unique in the 'programs' table of the 'Runtime', then evaluates
-- 'initSourceCode' and associates the resulting 'Dao.Evaluator.ExecUnit' with the
-- 'sourceFullPath' in the 'programs' table. Returns the logical "module" name of the script along
-- with an initialized 'Dao.Types.ExecUnit'.
registerSourceCode :: Bool -> UPath -> SourceCode -> Run File
registerSourceCode public upath sourceCode = ask >>= \runtime -> do
  let modName  = unComment (sourceModuleName sourceCode)
      pathTab  = pathIndex runtime
      modTab   = logicalNameIndex runtime
  alreadyLoaded <- fmap (M.lookup upath) (dReadMVar $loc pathTab)
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
      DataFile    _ _                                           -> error $
          "INTERNAL ERROR: "++show upath
        ++" has been loaded as both a data file and as an executable script."
    Nothing   -> do
      moduleNameConflict <- fmap (M.lookup modName) (dReadMVar $loc modTab)
      case moduleNameConflict of
        Just file -> error $
            "ERROR: cannot load source file "++show upath++"\n"
          ++"\tthe module name "++show (logicalName file)
          ++"is identical to that of another Dao script that has been\n"
          ++"\tloaded from the path "++show (filePath file)
        Nothing -> do
          -- Call 'initSourceCode' which creates the 'ExecUnit', then place it in an 'MVar'.
          -- 'initSourceCode' calls 'Dao.Evaluator.programFromSource'.
          xunit     <- initSourceCode sourceCode >>= lift . evaluate
          -- Check to make sure the logical name in the loaded program does not conflict with that
          -- of another loaded previously.
          xunitMVar <- dNewMVar $loc ("ExecUnit("++show upath++")") xunit
          let file =
                ProgramFile
                { publicFile  = public
                , filePath    = upath
                , logicalName = unComment (sourceModuleName sourceCode)
                , execUnit   = xunitMVar
                }
          dModifyMVar_ $loc pathTab $ return . M.insert upath file
          dModifyMVar_ $loc modTab  $ return . M.insert modName file
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
initSourceCode sourceCode = ask >>= \runtime -> do
  xunit <- initExecUnit runtime
  -- An execution unit is required to load a program, so of course, while a program is being
  -- loaded, the program is not in the program table, and is it's 'currentProgram' is 'Nothing'.
  cachedProg <- runExecScript (programFromSource sourceCode) xunit
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
setupTakedown :: (CachedProgram -> [[Com ScriptExpr]]) -> ExecUnit -> Run ()
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
    pathTab <- dReadMVar $loc (pathIndex runtime)
    modTab  <- dReadMVar $loc (logicalNameIndex runtime)
    dStack $loc "checkImports" $ do
      dModifyMVar $loc xunit $ \xunit -> case currentProgram xunit of
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
  DataFile _ _ -> return []

-- | Like 'checkImports' but checks every file that has been loaded by the Runtime.
checkAllImports :: Run [(Name, [Name])]
checkAllImports = ask >>= \runtime -> fmap (filter (not . null . snd) . concat) $
  dReadMVar $loc (pathIndex runtime) >>= mapM checkImports . M.elems

