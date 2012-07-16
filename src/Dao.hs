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

module Dao
  ( module Dao.Debug.ON
  , module Dao.String
  , module Dao.Object
  , module Dao.Types
  , module Dao
  ) where

-- | This module is pretty much where everything begins. I am considering renaming this module to
-- just "Dao", because it is the trunk of the dao package.

import           Dao.Debug.ON

import           Dao.String
import           Dao.Object
import           Dao.Types
import           Dao.Tasks
import           Dao.Files
import           Dao.Document
import           Dao.Evaluator
import           Dao.Builtins

import           Control.Exception
import           Control.Monad.Reader

import           Data.Maybe
import           Data.List
import qualified Data.Map    as M

import           System.IO

import Debug.Trace

-- | The minimum amount of time allowable for a single input string to execute before timing out.
-- Any time smaller than this ammount, and it may not be possible to execute anything before it
-- times out, so if you are setting a timeout limit, make sure it is as large as what you have
-- patience for.
min_exec_time :: Int
min_exec_time = 200000

-- | Create a new 'Runtime' with nothing in it except for the 'userData' you pass to it. This
-- takes an optional 'Dao.Debug.Debugger', which will be installed into the resulting
-- 'Dao.Types.Runtime' and used for debugging.
newRuntime :: Maybe Debugger -> Maybe Int -> M.Map Name CheckFunc -> IO Runtime
newRuntime debug timeout builtins = flip runReaderT debug $ dStack $loc "newRuntime" $ do
  paths <- dNewMVar $loc "Runtime.pathIndex"        (M.empty)
  names <- dNewMVar $loc "Runtime.logicalNameIndex" (M.empty)
  jtab  <- dNewMVar $loc "Runtime.jobTable"         (M.empty)
  dlist <- newDocList
  return $
    Runtime
    { documentList     = dlist
    , pathIndex        = paths
    , logicalNameIndex = names
    , jobTable         = jtab
    , defaultTimeout   = timeout
    , initialBuiltins  = builtins
    , runtimeDebugger  = debug
    }

-- | Initialize a new 'Runtime' data structure using a list of files and an initial user data item.
-- Each file will be parsed in turn (order is important) by the 'registerFile' function, which can
-- detect the kind of file and load it accordingly. The kinds of files that can be loaded are Dao
-- source files, Dao data files, and Dao compiled programs.
newRuntimeWithFiles :: DebugHandle -> Maybe Int -> [FilePath] -> IO Runtime
newRuntimeWithFiles debug timeout fx = do
  runtime <- newRuntime debug timeout basicScriptOperations
  fmap (fromMaybe (error "FAILED to initalized runtime with files")) $
    debugIO $loc "newRuntimeWithFiles" debug runtime $ do
      forM_ fx $ \f -> lift (catches (void $ runIO runtime $ loadFile True f) handlers)
      problems <- checkAllImports
      if null problems
        then return runtime
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

-- | This is a basic runtime loop you can use in your @main@ function. It takes just two parameters:
-- (1) an initializer equation that returns a 'Runtime' value which will be used for the duration of
-- this interactive session, and (2) an input function which is evaluated repeatedly, passing an
-- DMVar containing the 'Runtime' that was evaluated from the initializer equation so that it may be
-- modified by the input function. The input function must return a
-- @Data.Maybe.Just 'Prelude.String'@ which will be passed as the input string to 'execInputString'.
-- Once 'execInputString' completes, the input function will be evaluated again. All exceptions are
-- caught. Input strings returned from the given input function will also be checked for a leading
-- @(:)@ character. If so, the input will be evaluated by 'evalScriptString' in an empty
-- 'Dao.Evaluator.ExecUnit' ("empty" meaning not associated with any source code file, so
-- 'Dao.Evaluator.currentProgram' and 'Dao.Evaluator.sourceFile' are
-- 'Data.Maybe.Nothing'). If the input function returns 'Data.Maybe.Nothing', then the interactive
-- session is ended gracefully. The loop that waits on the input function runs in an exception
-- handler that catches all exceptions and runs every @TAKEDOWN@ script in every loaded Dao module.
interactiveRuntimeLoop
  :: DebugHandle
  -> Runtime
  -> (DMVar Runtime -> IO (Maybe String))
  -> IO ()
interactiveRuntimeLoop debug runtime getNextInput = do
  let ifException (SomeException e) = do
        hSetBuffering stderr LineBuffering
        hPutStrLn stderr ("ERROR: "++show e)
        return True
  traceIO ("(debugger is"++(if isNothing debug then "not" else "")++" defined)")
  void $ debugIO $loc "interactiveRuntimeLoop" debug runtime $ do
    runtimeMVar <- dNewMVar $loc "Runtime" runtime
    lift (traceIO "(init temporary ExecUnit)")
    intrcvXUnit <- initExecUnit runtime
    let runString input = selectModules Nothing [] >>= execInputString True input
        iterateInput = do
          input <- lift (traceIO "(get input)" >> getNextInput runtimeMVar)
          lift (traceIO "(execute input)")
          dModifyMVar $loc runtimeMVar $ \runtime -> do
            continue <- case input of
              Just ('\\':input) -> runString (ustr input) >> return True
              Just (':':input)  -> evalScriptString intrcvXUnit input >> return True
              Just input        -> runString (ustr input) >> return True
              Nothing           -> dPutStrErr $loc ":quit" >> return False
            return (runtime, continue)
        -- 'catchLoop' iterates input and processing, catching exceptions and deciding whether to
        -- continue looping based on the return value of the exception handler or 'iterateInput'
        -- function.
        catchLoop = do
          continue <- dHandle $loc (lift . ifException) iterateInput
          when continue (dYield >> catchLoop)
    catchLoop -- here we start the 'catchLoop'.
    -- Now, takedown all loaded modules.
    dReadMVar $loc (pathIndex runtime) >>= mapM_ (dReadMVar $loc >=> setupTakedown destructScript) .
      (map execUnit . filter isProgramFile . M.elems)

