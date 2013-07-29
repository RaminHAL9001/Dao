-- "src/dao-main.hs"  the Main module for the "dao" executable program.
-- Provides an interactive command line interface to the Dao System.
-- 
-- Copyright (C) 2008-2013  Ramin Honary.
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
import           Dao.Evaluator
import           Dao.Debug.OFF

import           Data.List

import           Control.Monad
import           Control.Monad.Reader
import           Control.Exception

import           System.Environment
import           System.IO
--import           System.Console.Readline

import Debug.Trace
import Control.Concurrent

----------------------------------------------------------------------------------------------------

disclaimer :: String
disclaimer = unlines $
  [ "\"Dao\"  Copyright (C) 2008-2013  Ramin Honary."
  , "This program comes with ABSOLUTELY NO WARRANTY."
  , "This is free software, and you are welcome to redistribute it under"
  , "the terms and conditions of the GNU Affero General Public License."
  , "Enter the command \":license\" for details."
  , "-----------------------------------------------"
  ]

inputLoop = do
  putStr "dao> " >> hFlush stdout
  closed <- hIsClosed stdin
  eof    <- isEOF
  if closed || eof
    then return Nothing
    else do
      -- hSetEcho stdin True
      str <- getLine
      ---------------------------------------------------
      --readline "dao> " >>= \str -> case str of
      --  Nothing  -> return Nothing
      --  Just str -> addHistory str >> return (Just str)
      case str of
        str | str==":quit"    || str==":quit\n" -> return Nothing
            | str==":license" || str==":license\n" -> putStrLn license_text >> inputLoop
            | otherwise -> return (Just (ustr str))

main = do
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  argv <- getArgs
  let (q, _) = partition (\a -> a=="-q" || a=="--dont-show-license") argv
  when (null q) (putStr disclaimer)
  --initialize -- initialize the ReadLine library
  -- let debug = Nothing
  -- debug <- debugToFile xloc "main" debugLog "./dao-debug.log" WriteMode
  debuggableProgram xloc  $
    (setupDebugger :: SetupDebugger Runtime (ReaderT Runtime IO))
    { debugEnabled = True
    , debugOutputTo = DebugOutputToFile "./debug.log"
    , initializeRuntime = newRuntime
    , beginProgram = do
        ok <- initRuntimeFiles argv
        if ok then daoInputLoop (liftIO inputLoop) else return ()
    }
  --restorePrompt -- shut-down the ReadLine library
  hPutStrLn stderr "Dao has exited."

license_text = unlines $
  [ "Copyright (C) 2008-2013  Ramin Honary"
  , ""
  , "This program is free software: you can redistribute it and/or modify"
  , "it under the terms of the GNU General Public License as published by"
  , "the Free Software Foundation, either version 3 of the License, or"
  , "(at your option) any later version."
  , ""
  , "This program is distributed in the hope that it will be useful,"
  , "but WITHOUT ANY WARRANTY; without even the implied warranty of"
  , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the"
  , "GNU General Public License for more details."
  , ""
  , "You should have received a copy of the GNU General Public License"
  , "along with this program (see the file called \"LICENSE\"). If not,"
  , "please see <http://www.gnu.org/licenses/agpl.html>."
  ]

