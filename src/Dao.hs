-- "src/Dao.hs"  the smallest interface that can be imported by any
-- Haskell program that makes use of the Dao System by way of linking
-- to the modules in the dao package.
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
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.


-- {-# LANGUAGE TemplateHaskell #-}

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

import           Dao.Runtime
import           Dao.String
import           Dao.Glob
import           Dao.Object
import           Dao.Procedural
import qualified Dao.Tree as T
import           Dao.Evaluator
import           Dao.PPrintM
import           Dao.Object.PPrintM

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Reader

import           Data.Monoid
import           Data.List
import qualified Data.Map    as M
import qualified Data.Set    as S

import           System.Environment (getProgName, getArgs)
import           System.IO

import Debug.Trace

-- | The minimum amount of time allowable for a single input string to execute before timing out.
-- Any time smaller than this ammount, and it may not be possible to execute anything before it
-- times out, so if you are setting a timeout limit, make sure it is as large as what you have
-- patience for.
min_exec_time :: Int
min_exec_time = 200000

-- | Create a new 'Runtime', optionally providing a file to which output debugging information, and
-- an 'Exec' monad to evalaute.
daoRuntime :: Maybe FilePath -> Exec () -> IO ()
daoRuntime debugRef daoMain = do
  progName <- getProgName
  let runtime =
        Runtime
        { pathIndex            = error "Dao:daoRuntime: forgot to set \"pathIndex\""
        , defaultTimeout       = Just 8000000
        , importGraph          = error "Dao:daoRuntime: forgot to set \"importGraph\""
        , globalMethodTable    = error "Dao:daoRuntime: forgot to set \"globalMethodTable\""
        }
  paths    <- newMVar (M.empty)
  runtime  <- return $ runtime{pathIndex=paths}
  xunit    <- initExecUnit Nothing runtime
  result   <- ioExec daoMain xunit
  case result of
    FlowOK     ()  -> return ()
    FlowReturn obj -> maybe (return ()) (putStrLn . prettyShow) obj
    FlowErr    err -> hPutStrLn stderr (prettyShow err)

----------------------------------------------------------------------------------------------------

singleThreaded :: [UStr] -> Exec ()
singleThreaded args = do
  deps   <- importFullDepGraph args
  mapM_ loadModule (getDepFiles deps)

