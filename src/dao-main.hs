-- "src/dao-main.hs"  the Main module for the "dao" executable program.
-- Provides an interactive command line interface to the Dao System.
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
-- along with this program (see the file called "LICENSE"). If not,
-- please see <http://www.gnu.org/licenses/agpl.html>.


-- {-# LANGUAGE TemplateHaskell #-}

module Main where

import           Dao
import           Dao.Predicate
import           Dao.PPrint

import           Data.Char

import           Control.Monad
import           Control.Monad.IO.Class

import           System.Environment
import           System.IO
--import           System.Console.Readline

----------------------------------------------------------------------------------------------------

version :: String
version = "0.0 (experimental)"

disclaimer :: String
disclaimer = unlines $
  [ "\"Dao\" version "++version
  , "Copyright (C) 2008-2014  Ramin Honary."
  , "This program comes with ABSOLUTELY NO WARRANTY."
  , "This is free software, and you are welcome to redistribute it under"
  , "the terms and conditions of the GNU Affero General Public License."
  , "Enter the command \":license\" for details."
  , "-----------------------------------------------"
  ]

inputLoop :: Exec (Maybe UStr)
inputLoop = do
  liftIO $ putStr "dao> " >> hFlush stdout
  closed <- liftIO $ hIsClosed stdin
  eof    <- liftIO isEOF
  if closed || eof
    then return Nothing
    else do
      -- hSetEcho stdin True
      str <- liftIO getLine
      ---------------------------------------------------
      --readline "dao> " >>= \str -> case str of
      --  Nothing  -> return Nothing
      --  Just str -> addHistory str >> return (Just str)
      case words (uchars str) of
        o | o==[":quit"   ] || o==[":", "quit"   ] -> return Nothing
        o | o==[":license"] || o==[":", "license"] -> liftIO (putStrLn license_text) >> inputLoop
        ((':':_) : _) -> do
          evalScriptString (dropWhile (\c -> isSpace c || c==':') str)
          inputLoop
        (a:cmds) | head a==':' -> do
          liftIO $ hPutStr stderr $ unwords $
            ["Error: unknown meta-command"] ++ if null cmds then [] else [show $ head cmds]
          inputLoop
        _ -> return $ Just $ toUStr str

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  argv <- getArgs
  when (elem "--version" argv) (putStr disclaimer)
  argv <- return $ fmap ustr $ filter (/="--version") argv
  --initialize -- initialize the ReadLine library
  result <- setupDao $ do
    loadDaoStandardLibrary
    daoInitialize $ do
      loadEveryModule argv
      daoInputLoop inputLoop
      daoShutdown
  case result of
    OK    ()                -> return ()
    PFail (ExecReturn    o) -> maybe (return ()) (putStrLn . prettyShow) o
    PFail (err@ExecError{}) -> hPutStrLn stderr (prettyShow err)
    Backtrack               -> hPutStrLn stderr "(does not compute)"
  --restorePrompt -- shut-down the ReadLine library
  hPutStrLn stderr "Dao has exited."

license_text :: String
license_text = unlines $
  [ "Dao version: "++version
  , "Copyright (C) 2008-2014  Ramin Honary"
  , ""
  , "This program is free software: you can redistribute it and/or modify"
  , "it under the terms and conditions of the GNU General Public License as"
  , "published by the Free Software Foundation, either version 3 of the"
  , "license, or (at your option) any later version."
  , ""
  , "This program is distributed in the hope that it will be useful"
  , "WITHOUT ANY WARRANTY; without even the implied warranty of"
  , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the"
  , "GNU General Public License for more details."
  , ""
  , "You should have received a copy of the GNU General Public License"
  , "along with this program (see the file called \"LICENSE\"). If not,"
  , "please see <http://www.gnu.org/licenses/agpl.html>."
  ]

