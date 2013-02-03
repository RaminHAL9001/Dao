-- "src/Dao/Debug/OFF.hs"  provides the exact same API as
-- "src/Dao/Debug.ON.hs" but replaces every function with a NO-OP,
-- effectively disabling debugging on any module which imports it.
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


module Dao.Debug.OFF
  ( module Dao.Debug
  , module Dao.Debug.OFF
  ) where

-- | This module provides replacements for many key functions in "System.IO" and
-- "Control.Concurrent". The replacement functions require you pass additional parameters,
-- particularly one of 'nonloc' or a template Haskell splice "$(loc)".
--
-- *The functions in this module perform absolutely NO debugging*, however this modiule exports
-- excatly the same API as "Dao.Debug.ON". Every function behaves identically to the
-- "Control.Concurrent" function it replaces. The result is, if you change your "import" statement
-- from "Dao.Debug.ON" to "Dao.Debug.OFF", debugging functionality is removed from your code
-- entirely, and can be optimized away during compilation as every function in this module can be
-- cleanly replacaed by the "Control.Concurrent" function it substitutes.
--
-- Of course, you must change *all* of the imports in your program to use either "Dao.Debug.ON" or
-- "Dao.Debug.OFF", you cannot use both. To debug portions of your program while ignoring others,
-- import "Dao.Debug.ON" and enable or disable debugging explicitly by either using a @ReaderT () IO
-- a@ in place of the IO monad at the portions of your code that you do NOT want to debug, and using
-- a @ReaderT Debugger IO a@ monad in place of the @IO@ monad in places that you DO want to debug. You
-- could also compose separate @'debugIO' =<< 'enableDebug' ...@ and
-- @'debugIO' =<< 'disableDebug' ...@ statements at the relevant locations in your code instead of
-- changing the types of your monadic computations.

import           Dao.Debug
import           Dao.String

import           Control.Exception
import           Control.Concurrent
import           Control.Monad.Reader

import           Data.Maybe
import           Data.Word
import           Data.List hiding (lookup)
import qualified Data.Map as M

import           System.IO

import qualified Language.Haskell.TH as Q

----------------------------------------------------------------------------------------------------

loc :: Q.ExpQ
loc = return (Q.ConE (Q.mkName "Nothing"))

xloc :: MLoc
xloc = Nothing

----------------------------------------------------------------------------------------------------

debugToHandle :: MLoc -> String -> LogWriter -> Handle -> IO DebugHandle
debugToHandle _ _ _ _ = return Nothing

debugToFile :: MLoc -> String -> LogWriter -> FilePath -> IOMode -> IO DebugHandle
debugToFile _ _ _ _ _ = return Nothing

disableDebugFile :: MLoc -> String -> LogWriter -> FilePath -> IOMode -> IO DebugHandle
disableDebugFile _ _ _ _ _ = return Nothing

debugTimestamp :: MLoc -> String -> Handle -> IO ()
debugTimestamp _ _ _ = return ()

debugIO :: Bugged r => MLoc -> String -> DebugHandle -> r -> (ReaderT r IO a) -> IO (Maybe a)
debugIO _ _ _ r testFunc = fmap Just (runReaderT testFunc r)

haltDebugger :: DebugHandle -> IO ()
haltDebugger _ = return ()

----------------------------------------------------------------------------------------------------

showThID :: ThreadId -> String -> String
showThID tid msg = "(#"
  ++ let th = show tid in fromMaybe th (stripPrefix "ThreadId " th)
  ++ (if null msg then "" else ' ':msg)
  ++ ")"

dSimple :: Bugged r => IO a -> IO ignored -> ReaderT r IO a
dSimple doSomething _ = lift doSomething

uniqueID :: Debugger -> IO Word64
uniqueID _ = return 0

event :: Debugger -> DEvent -> IO ()
event _ _ = return ()

----------------------------------------------------------------------------------------------------

dMyThreadId :: ReaderT r IO ThreadId
dMyThreadId = lift myThreadId

dYield :: ReaderT r IO ()
dYield = lift yield

dThrowIO :: Exception e => e -> ReaderT r IO any
dThrowIO e = lift (throwIO e)

dThreadDelay :: Bugged r => MLoc -> Int -> ReaderT r IO ()
dThreadDelay _ i = lift (threadDelay i)

dMyThreadLabel :: ThreadId -> ReaderT r IO (Maybe Name)
dMyThreadLabel _ = return Nothing

dMessage :: Bugged r => MLoc -> String -> ReaderT r IO ()
dMessage _ _ = return ()

dStack :: Bugged r => MLoc -> String -> ReaderT r IO a -> ReaderT r IO a
dStack _ _ func = func

dPutString
  :: Bugged r
  => (MLoc -> ThreadId -> String -> DEvent)
  -> (String -> IO ())
  -> MLoc
  -> String
  -> ReaderT r IO ()
dPutString _ io _ msg = lift (io msg)

dPutStrLn :: Bugged r => MLoc -> String -> ReaderT r IO ()
dPutStrLn = dPutString undefined putStrLn

dPutStr :: Bugged r => MLoc -> String -> ReaderT r IO ()
dPutStr = dPutString undefined putStr

dPutStrErr :: Bugged r => MLoc -> String -> ReaderT r IO ()
dPutStrErr _ msg = dPutString undefined (hPutStrLn stderr) undefined msg

----------------------------------------------------------------------------------------------------

dFork :: Bugged r => (IO () -> IO ThreadId) -> MLoc -> String -> ReaderT r IO () -> ReaderT r IO ThreadId
dFork fork _ _ ioFunc = ReaderT $ \r -> fork (runReaderT ioFunc r)

dThrowTo :: (Exception e, Bugged r) => MLoc -> ThreadId -> e -> ReaderT r IO ()
dThrowTo _ target err = lift (throwTo target err)

dKillThread :: Bugged r => MLoc -> ThreadId -> ReaderT r IO ()
dKillThread _ target = lift (killThread target)

dCatch :: (Exception e, Bugged r) => MLoc -> ReaderT r IO a -> (e -> ReaderT r IO a) -> ReaderT r IO a
dCatch _ tryFunc catchFunc = ReaderT $ \r ->
  handle (\e -> runReaderT (catchFunc e) r) (runReaderT tryFunc r)

dHandle :: (Exception e, Bugged r) => MLoc -> (e -> ReaderT r IO a) -> ReaderT r IO a -> ReaderT r IO a
dHandle _ catchFunc tryFunc = dCatch undefined tryFunc catchFunc

dCatches :: Bugged r => ReaderT r IO a -> [DHandler r a] -> ReaderT r IO a
dCatches tryfn dhands = do
  r <- ask
  lift $ catches (runReaderT tryfn r) $
    map (\dhandl -> (getHandler dhandl) r (\_ -> return ())) dhands

dHandles :: Bugged r => [DHandler r a] -> ReaderT r IO a -> ReaderT r IO a
dHandles = flip dCatches

dThrow :: (Exception e, Bugged r) => MLoc -> e -> ReaderT r IO ignored
dThrow _ err = lift (throwIO err)

----------------------------------------------------------------------------------------------------

dMakeVar
  :: Bugged r
  => IO v
  -> (MLoc -> ThreadId -> DVar v -> DEvent)
  -> String
  -> MLoc
  -> String
  -> ReaderT r IO (DVar v)
dMakeVar newIO _ _ _ _ = lift (fmap DVar newIO)

dVar ::
  Bugged r
  => (v -> IO a)
  -> (MLoc -> ThreadId -> DVar v)
  -> MLoc
  -> DVar v
  -> ReaderT r IO a
dVar withVar _ _ dvar = lift (withVar (dbgVar dvar))

----------------------------------------------------------------------------------------------------

dNewChan :: Bugged r => MLoc -> String -> ReaderT r IO (DChan v)
dNewChan _ _ = dMakeVar newChan undefined undefined undefined undefined

dWriteChan :: Bugged r => MLoc -> DChan v -> v -> ReaderT r IO ()
dWriteChan _ var v = dVar (flip writeChan v) undefined undefined var

dReadChan :: Bugged r => MLoc -> DChan v -> ReaderT r IO v
dReadChan _ var = dVar readChan undefined undefined var

----------------------------------------------------------------------------------------------------

dNewQSem :: Bugged r => MLoc -> String -> Int -> ReaderT r IO DQSem
dNewQSem _ _ i = dMakeVar (newQSem i) undefined undefined undefined undefined

dSignalQSem :: Bugged r => MLoc -> DQSem -> ReaderT r IO ()
dSignalQSem _ var = dVar signalQSem undefined undefined var

dWaitQSem :: Bugged r => MLoc -> DQSem -> ReaderT r IO ()
dWaitQSem _ var = dVar waitQSem undefined undefined var

----------------------------------------------------------------------------------------------------

dNewMVar :: Bugged r => MLoc -> String -> v -> ReaderT r IO (DMVar v)
dNewMVar _ _ v = dMakeVar (newMVar v) undefined undefined undefined undefined

dNewEmptyMVar :: Bugged r => MLoc -> String -> ReaderT r IO (DMVar v)
dNewEmptyMVar _ _ = dMakeVar newEmptyMVar undefined undefined undefined undefined

dPutMVar :: Bugged r => MLoc -> DMVar v -> v -> ReaderT r IO ()
dPutMVar _ var v = dVar (flip putMVar v) undefined undefined var

dTakeMVar :: Bugged r => MLoc -> DMVar v -> ReaderT r IO v
dTakeMVar _ var = dVar takeMVar undefined undefined var

dReadMVar :: Bugged r => MLoc -> DMVar v -> ReaderT r IO v
dReadMVar _ var = dVar readMVar undefined undefined var

dSwapMVar :: Bugged r => MLoc -> DMVar v -> v -> ReaderT r IO v
dSwapMVar _ var v = dVar (flip swapMVar v) undefined undefined var

dModifyMVar :: Bugged r => MLoc -> DMVar v -> (v -> ReaderT r IO (v, a)) -> ReaderT r IO a
dModifyMVar _ var updFunc = ask >>= \r ->
  dVar (\v -> modifyMVar v (\v -> runReaderT (updFunc v) r)) undefined undefined var

dModifyMVar_ :: Bugged r => MLoc -> DMVar v -> (v -> ReaderT r IO v) -> ReaderT r IO ()
dModifyMVar_ _ var updFunc = ask >>= \r ->
  dVar (\v -> modifyMVar_ v (\v -> runReaderT (updFunc v) r)) undefined undefined var

----------------------------------------------------------------------------------------------------

debugLog :: LogWriter
debugLog _ _ = return ()

