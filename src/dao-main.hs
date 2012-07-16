-- "src/dao-main.hs"  the Main module for the "dao" executable program.
-- Provides an interactive command line interface to the Dao System.
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

module Main where

-- | "dao-main.hs" provides a basic interactive program that can interact with
-- the functions provided in the "Dao" module.
-- 
-- Copyright (C) 2008-2012  Ramin Honary
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/>.

import           Dao

import           Data.List

import Dao.Object.Parsers -- for testing
import Dao.Parser -- for testing

import           Control.Monad
import           Control.Exception

import           System.Environment
import           System.IO
--import           System.Console.Readline

import Debug.Trace
import Control.Concurrent

----------------------------------------------------------------------------------------------------

disclaimer :: String
disclaimer = unlines $
  [ "\"Dao\"  Copyright (C) 2008-2012  Ramin Honary."
  , "This program comes with ABSOLUTELY NO WARRANTY."
  , "This is free software, and you are welcome to redistribute it under"
  , "the terms and conditions of the GNU Affero General Public License."
  , "Enter the command \":license\" for details."
  , "-----------------------------------------------"
  ]

main = do
  argv <- getArgs
  let (q, _) = partition (\a -> a=="-q" || a=="--dont-show-license") argv
  --initialize
  traceIO "(init debugger)"
  debug   <- debugToFile $loc "main" debugLog "./dao-debug.log" WriteMode
  when (null q) (putStrLn disclaimer)
  traceIO "(init runtime with files)"
  runtime <- newRuntimeWithFiles debug (Just 8000000) argv
  traceIO "(begin interactive loop)"
  interactiveRuntimeLoop debug runtime $
    (\ _ -> handle (\ (SomeException e) -> seq e (print e >> return Nothing)) $ do
      let loop = do
            putStr "dao> " >> hFlush stdout
            closed <- hIsClosed stdin
            eof    <- isEOF
            if closed || eof
              then return Nothing
              else do
                -- hSetEcho stdin True
                str <- getLine
                case str of
                  str | str==":quit"    || str==":quit\n" -> return Nothing
                      | str==":license" || str==":license\n" -> putStrLn license_text >> loop
                      | otherwise -> return (Just str)
      loop
          --readline "dao> " >>= \str -> case str of
          --  Nothing  -> return Nothing
          --  Just str -> addHistory str >> return (Just str)
    )
  --restorePrompt
  haltDebugger debug
  hPutStrLn stderr "Dao has exited."

license_text = unlines $
  [ "Copyright (C) 2008-2012  Ramin Honary"
  , ""
  , "This program is free software: you can redistribute it and/or modify"
  , "it under the terms of the GNU General Public License as published by"
  , "the Free Software Foundation, either version 3 of the License, or"
  , "(at your option) any later version."
  , ""
  , "This program is distributed in the hope that it will be useful,"
  , "but WITHOUT ANY WARRANTY; without even the implied warranty of"
  , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
  , "GNU General Public License for more details."
  , ""
  , "You should have received a copy of the GNU General Public License"
  , "along with this program (see the file called \"LICENSE\"). If not, see"
  , "<http://www.gnu.org/licenses/>."
  ]

