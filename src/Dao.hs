-- "src/Dao.hs"  the smallest interface that can be imported by any
-- Haskell program that makes use of the Dao System by way of linking
-- to the modules in the dao package.
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

-- | This module is pretty much where everything begins. It is the smallest interface that can be
-- imported by any Haskell program making use of the Dao System. You can use the functions in this
-- module to initialize a 'Dao.Object.Runtime' data structure, then use it to start an input query
-- loop with 'inputQueryLoop'. The query loop requires you pass a callback function that, on each
-- evaluation, returns the next string to be used the query to the 'Dao.Object.Runtime'.
--
-- To have more control over execution of string queries, you will need to import the "Dao.Tasks"
-- module and make use of those functions to create 'Dao.Object.Job's from string queries, then wait
-- for those 'Dao.Object.Job's to complete.

module Dao
  ( module Dao.String
  , module Dao.Object
  , module Dao
  ) where

import           Dao.Debug.ON

import           Dao.String
import           Dao.Pattern
import           Dao.Object
import           Dao.Resource
import qualified Dao.Tree as T
import           Dao.Files
import           Dao.Evaluator

import           Control.Exception
import           Control.Monad.Reader

import           Data.Maybe
import           Data.List
import qualified Data.Map    as M
import qualified Data.Set    as S

import           System.IO

import Debug.Trace

-- | The minimum amount of time allowable for a single input string to execute before timing out.
-- Any time smaller than this ammount, and it may not be possible to execute anything before it
-- times out, so if you are setting a timeout limit, make sure it is as large as what you have
-- patience for.
min_exec_time :: Int
min_exec_time = 200000

-- | Create a new 'Runtime' with nothing in it except for the 'userData' you pass to it.
newRuntime :: DebugRef -> IO Runtime
newRuntime debugRef = flip runReaderT debugRef $ dStack $loc "newRuntime" $ do
  paths <- dNewMVar $loc "Runtime.pathIndex" (M.empty)
  running  <- dNewMVar $loc "Runtime.runningExecUnits"  (S.empty)
  wait  <- dNewEmptyMVar $loc "Runtime.waitExecUnitsMVar"
  return $
    Runtime
    { pathIndex            = paths
    , defaultTimeout       = Just 8000000
    , functionSets         = M.empty
    , runningExecUnits     = running
    , waitExecUnitsMVar    = wait
    , availableTokenizers  = M.empty -- specifying no tokenizer will cause the default to be used
    , availableComparators = M.fromList $
        [ (ustr "exact"      , exact)
        , (ustr "approximate", approx)
        ]
    , fileAccessRules      = []
    , runtimeDebugger      = Nothing
    }

-- | Provide a labeled set of built-in functions for this runtime. Each label indicates a set of
-- functionality which is checked by the "required" directive of any Dao program that is loaded into
-- this runtime. If all "required" function sets are not available, loading of that Dao program
-- fails.
initRuntimeFunctions :: [(Name, M.Map Name DaoFunc)] -> Runtime -> Runtime
initRuntimeFunctions funcs runtime =
  runtime{ functionSets = M.union (M.fromList funcs) (functionSets runtime) }

-- | Initialize files into the 'Runtime' data structure using a list of files. Each file will be
-- parsed in turn (order is important) by the 'registerFile' function, which can detect the kind of
-- file and load it accordingly. The kinds of files that can be loaded are Dao source files, Dao
-- data files, and Dao compiled programs.
initRuntimeFiles :: [FilePath] -> Run ()
initRuntimeFiles filePaths = dStack $loc "initRuntimeFiles" $ do
  forM_ filePaths (\filePath -> void (loadFilePath filePath))
  problems <- checkAllImports
  if null problems
    then ask >>= \runtime -> dReadMVar $loc (pathIndex runtime) >>= return . M.keys >>= dMessage $loc . ("list of loaded files updated: "++) . intercalate ", " . map show >> return ()
    else error $ "ERROR: some Dao programs have imported modules which were not loaded.\n"
          ++(flip concatMap problems $ \ (mod, imprts) ->
                "\tmodule "++show mod++" could not satisfy the import requirements for:\n\t\t"
              ++intercalate ", " (map show imprts)++"\n")
  where
    handlers = [Handler ioerr, Handler errcall]
    ioerr :: IOException -> IO ()
    ioerr = print
    errcall :: ErrorCall -> IO ()
    errcall = print

