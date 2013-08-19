-- "src/Dao/Debug/OFF.hs"  provides the exact same API as
-- "src/Dao/Debug.ON.hs" but replaces every function with a NO-OP,
-- effectively disabling debugging on any module which imports it.
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
-- a @ReaderT DebugData IO a@ monad in place of the @IO@ monad in places that you DO want to debug. You
-- could also compose separate @'debugIO' =<< 'enableDebug' ...@ and
-- @'debugIO' =<< 'disableDebug' ...@ statements at the relevant locations in your code instead of
-- changing the types of your monadic computations.

module Dao.Debug.OFF
  ( module Dao.Debug
  , module Dao.Debug.OFF
  ) where

import           Dao.Debug
import           Dao.String

import           Control.Exception
import           Control.Concurrent
import qualified Control.Concurrent.SSem as Sem
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

debuggableProgram :: Bugged r m => MLoc -> SetupDebugger r m -> IO ()
debuggableProgram _ setup =
  initializeRuntime setup Nothing >>= debugUnliftIO (withDebugger Nothing (beginProgram setup))

uniqueID :: DebugData -> IO Word64
uniqueID _ = return 0

event :: DebugData -> DEvent -> IO ()
event _ _ = return ()

----------------------------------------------------------------------------------------------------

dMyThreadId :: Bugged r m => m DThread
dMyThreadId = liftIO myThreadId >>= return . DThread

dYield :: MonadIO m => m ()
dYield = liftIO yield

dThrowIO :: (MonadIO m, Exception e) => e -> m any
dThrowIO e = liftIO (throwIO e)

dThreadDelay :: Bugged r m => MLoc -> Int -> m ()
dThreadDelay _ i = liftIO (threadDelay i)

dMessage :: Bugged r m => MLoc -> String -> m ()
dMessage _ _ = return ()

dStack :: Bugged r m => MLoc -> String -> m a -> m a
dStack _ _ func = func

dPutString
  :: Bugged r m
  => (MLoc -> DThread -> String -> DEvent)
  -> (String -> IO ())
  -> MLoc
  -> String
  -> m ()
dPutString _ io _ msg = liftIO (io msg)

dPutStrLn :: Bugged r m => MLoc -> String -> m ()
dPutStrLn = dPutString (\ _ _ _ -> DIgnore) putStrLn

dPutStr :: Bugged r m => MLoc -> String -> m ()
dPutStr = dPutString (\ _ _ _ -> DIgnore) putStr

dPutStrErr :: Bugged r m => MLoc -> String -> m ()
dPutStrErr = dPutString (\ _ _ _ -> DIgnore) (hPutStrLn stderr)

----------------------------------------------------------------------------------------------------

dFork :: Bugged r m => (IO () -> IO ThreadId) -> MLoc -> String -> m () -> m DThread
dFork fork _ _ ioFunc = do
  r <- askState
  liftIO (fork (debugUnliftIO ioFunc r) >>= return . DThread)

dThrowTo :: (Exception e, Bugged r m) => MLoc -> DThread -> e -> m ()
dThrowTo _ target err = liftIO (throwTo (dThreadGetId target) err)

dKillThread :: Bugged r m => MLoc -> DThread -> m ()
dKillThread _ target = liftIO (killThread (dThreadGetId target))

dCatch :: (Exception e, Bugged r m) => MLoc -> m a -> (e -> m a) -> m a
dCatch _ tryFunc catchFunc = askState >>= \r ->
  liftIO (handle (\e -> debugUnliftIO (catchFunc e) r) (debugUnliftIO tryFunc r))

dHandle :: (Exception e, Bugged r m) => MLoc -> (e -> m a) -> m a -> m a
dHandle loc catchFunc tryFunc = dCatch loc tryFunc catchFunc

dCatches :: Bugged r m => m a -> [DHandler r a] -> m a
dCatches tryfn dhands = do
  r <- askState
  liftIO $ catches (debugUnliftIO tryfn r) $
    map (\dhandl -> (getHandler dhandl) r (\_ -> return ())) dhands

dHandles :: Bugged r m => [DHandler r a] -> m a -> m a
dHandles = flip dCatches

dThrow :: (Exception e, Bugged r m) => MLoc -> e -> m ignored
dThrow _ err = liftIO (throwIO err)

----------------------------------------------------------------------------------------------------

dMakeVar
  :: Bugged r m
  => IO v
  -> (MLoc -> DThread -> DVar v -> DEvent)
  -> String
  -> MLoc
  -> String
  -> m (DVar v)
dMakeVar newIO _ _ _ _ = liftIO (fmap DVar newIO)

dIgnore :: MLoc -> DThread -> DVar v -> DEvent
dIgnore _ _ _ = DIgnore

dVar ::
  Bugged r m
  => (v -> IO a)
  -> MLoc
  -> DVar v
  -> m a
dVar withVar _ dvar = liftIO (withVar (dbgVar dvar))

---------------------------------------------------------------------------------------------------

dNewQSem :: Bugged r m => MLoc -> String -> Int -> m DQSem
dNewQSem loc msg i = dMakeVar (Sem.new i) dIgnore "" loc msg

dSignalQSem :: Bugged r m => MLoc -> DQSem -> m ()
dSignalQSem loc var = dVar Sem.signal loc var

dWaitQSem :: Bugged r m => MLoc -> DQSem -> m ()
dWaitQSem loc var = dVar Sem.wait loc var

----------------------------------------------------------------------------------------------------

dNewMVar :: Bugged r m => MLoc -> String -> v -> m (DMVar v)
dNewMVar loc msg v = dMakeVar (newMVar v) dIgnore "" loc msg

dNewEmptyMVar :: Bugged r m => MLoc -> String -> m (DMVar v)
dNewEmptyMVar loc msg = dMakeVar newEmptyMVar dIgnore "" loc msg

dPutMVar :: Bugged r m => MLoc -> DMVar v -> v -> m ()
dPutMVar loc var v = dVar (flip putMVar v) loc var

dTakeMVar :: Bugged r m => MLoc -> DMVar v -> m v
dTakeMVar loc var = dVar takeMVar loc var

dReadMVar :: Bugged r m => MLoc -> DMVar v -> m v
dReadMVar loc var = dVar readMVar loc var

dSwapMVar :: Bugged r m => MLoc -> DMVar v -> v -> m v
dSwapMVar loc var v = dVar (flip swapMVar v) loc var

dModifyMVar :: Bugged r m => MLoc -> DMVar v -> (v -> m (v, a)) -> m a
dModifyMVar loc var updFunc = askState >>= \r ->
  dVar (\v -> modifyMVar v (\v -> debugUnliftIO (updFunc v) r)) loc var

dModifyMVar_ :: Bugged r m => MLoc -> DMVar v -> (v -> m v) -> m ()
dModifyMVar_ loc var updFunc = askState >>= \r ->
  dVar (\v -> modifyMVar_ v (\v -> debugUnliftIO (updFunc v) r)) loc var

----------------------------------------------------------------------------------------------------

instance Show DUnique where { show _ = "" }
instance Show DEvent  where { show _ = "" }

