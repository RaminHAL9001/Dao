-- "src/Dao/DistVar.hs"  a concurrent API for distributing a function to
-- be "fmap" mapped across multiple threads.
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
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

-- | A better name for this module might be Control.Concurrent.FMap, it is the "map" portion of a
-- map/reduce-like algorithm. There are two main data types: 'DistVar' and 'DistJob'. 'DistVar' is
-- essentially a 'Control.Concurrent.Chan.Chan' that can store only one item. Using
-- "Control.Concurrent.SSem", many threads may write to a 'DistVar' concurrently but all except the
-- thread that got there first will block. If there is another thread reading from the dist var, it
-- will clear the way for another writing thread to unblock and write an item.
-- 
-- Many threads may read from a 'DistVar' as well. They will all block until another thread writes
-- to the 'DistVar', and this will unblock only one reading thread.
module Dao.DistVar where

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

import           Data.IORef
import           Data.Function
import qualified Data.Set as S
import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.SSem

data DistVar a = DistVar{ distVarReadSem :: SSem, distVarWriteSem :: SSem, distVarMonitor :: IORef a }

newDistVar :: a -> IO (DistVar a)
newDistVar a = pure DistVar <*> new 1 <*> new 0 <*> newIORef a

newEmptyDistVar :: IO (DistVar a)
newEmptyDistVar = pure DistVar <*> new 0 <*> new 1 <*> newIORef (error "empty DistVar")

readDistVar :: DistVar a -> IO a
readDistVar (DistVar rsem wsem ref) = withSem rsem (readIORef ref)

swapDistVar :: DistVar a -> a -> IO a
swapDistVar (DistVar rsem wsem ref) a = withSem rsem (readIORef ref >>= \b -> writeIORef ref a >> return b)

putDistVar :: DistVar a -> a -> IO ()
putDistVar (DistVar rsem wsem ref) a = bracket_ (wait wsem) (signal rsem) (writeIORef ref a)

takeDistVar :: DistVar a -> IO a
takeDistVar (DistVar rsem wsem ref) =
  bracketOnError
    (wait rsem >> readIORef ref)
    (\a -> mask_ $ writeIORef ref a >> signal rsem)
    (\a -> mask_ $ writeIORef ref (error "empty DistVar") >> signal wsem >> return a)

modifyDistVar :: DistVar a -> (a -> IO (a, b)) -> IO b
modifyDistVar (DistVar rsem wem ref) upd = withSem rsem $
  readIORef ref >>= upd >>= \ (a, b) -> writeIORef ref a >> return b

modifyDistVar_ :: DistVar a -> (a -> IO a) -> IO ()
modifyDistVar_ var upd = modifyDistVar var (fmap (\a -> (a, ())) . upd)

-- | A distributed job lets you specify an fmap function that can be evaluated across multiple
-- threads. This should be used when the output of the fmap function may produce a list of
-- results in no particular order, and it is more important to have the function evaluated
-- concurrently in multiple threads.
-- 
-- If your producer (the function feeding a list of inputs to the job) and consumer (the function
-- reading the output of the job) are running in a separate thread as well, you can pause the job
-- evaluating 'setWorkerCount' with a zero value, and resume it by setting the worker count to a
-- positive value. Terminating jobs never forces a running job to quit, the current evaluation is
-- always allowed to finish.
-- 
-- It is possible to throw 'Control.Exception.Exception's to all of the workers in a job
-- simultaneously. Should a job forcibly be halted, some inputs may go unevaluated and produce no
-- output. The job can be started back up again by evaluating 'setWorkerCount' with a positive
-- integer value. As long as you have a reference to the DistJob, it can be used regardless of any
-- signals sent to it.
data DistJob i o
  = DistJob
    { inputChan    :: DistVar (Maybe i)
    , outputChan   :: DistVar o
    , busyCounter  :: MVar Int
    , waitingReady :: MVar [SSem] -- a list of semaphores to signal when all workers are idle.
    , roster       :: MVar (S.Set ThreadId)
      -- | Set how many worker threads are avaialable. If more threads than the number of currently
      -- existing threads are requested, more threads are forked until the number of available
      -- workers matches the requested number. If fewer threads than the number of currently
      -- existing threads are requested, existing threads are signalled to finish their current task
      -- and halt. A negative count value results in the same behavior as zero count value. It may
      -- take a long time for this function to take effect if there are many threads sending input
      -- to the job very rapidly.
    , setWorkerCount :: Int -> IO ()
      -- | This function is useful when you would like the current thread to wait for all of the
      -- currently running threads to finish evaluating their inputs. Evaluating this will block
      -- until all worker threads are idle. If the worker threads are already idle when this
      -- function is evaluated, evaluation returns immediately. /NOTE:/ there is no guarantee that a
      -- new input will start a thread working again at some point in between the time this function
      -- unblocks and the time that control returns to where you called it. Unless you have complete
      -- control over all threads sending input to this job, be prepared for the circumstance that
      -- the workers may actually start working again after this function returns.
    , waitForAllIdle :: IO ()
    }

getWorkerCount :: DistJob i o -> IO Int
getWorkerCount (DistJob{roster=wrkrs}) = fmap S.size (readMVar wrkrs)

-- | Asynchronous send a single input item to the 'DistJob' (never blocks).
send :: DistJob i o -> i -> IO ()
send job = putDistVar (inputChan job) . Just

-- | Syncrhonous receive a single output item from the 'DistJob' (blocks until a result comes in).
receive :: DistJob i o -> IO o
receive job = takeDistVar (outputChan job)

newDistJob :: (IO () -> IO ThreadId) -> Int -> (i -> IO o) -> IO (DistJob i o)
newDistJob fork initWorkerCount distFMap = do
  ichan   <- newEmptyDistVar
  ochan   <- newEmptyDistVar
  wrkrs   <- newMVar S.empty
  busy    <- newMVar 0
  waiting <- newMVar []
  let signalReady i = when (i==0) $ modifyMVar_ waiting (mapM_ signal >=> \ () -> return [])
  let nowBusy       = modifyMVar_ busy (return . (+1))
  let nowIdle       = modifyMVar  busy ((\i -> return (i, i)) . subtract 1) >>= signalReady
  let newWorker     = fork $ do
        tid <- myThreadId
        (fix $ \loop -> do
            i <- takeDistVar ichan
            flip (maybe (return ())) i $ \i -> do
              yield >> bracket_ nowBusy nowIdle (distFMap i >>= \o -> putDistVar ochan o)
              yield >> loop
          ) `finally` mask_ (modifyMVar_ wrkrs (return . S.delete tid))
          -- finally: each worker is responsible from removing it's own ID from the roster
  initWrkrs <- replicateM initWorkerCount newWorker
  return $
    DistJob
    { inputChan    = ichan
    , outputChan   = ochan
    , roster       = wrkrs
    , busyCounter  = busy
    , waitingReady = waiting
    , setWorkerCount = \i' -> let i = max 0 i' in do
        size <- S.size <$> readMVar wrkrs
        case compare size i of
          LT -> do
            newWrkrs <- replicateM i newWorker
            modifyMVar_ wrkrs $ return . flip (foldl (flip S.insert)) newWrkrs
          GT -> replicateM_ (size-i) (putDistVar ichan Nothing)
          EQ -> return ()
    , waitForAllIdle = do
        a <- readMVar busy
        when (a/=0) $ new 0 >>= \sem -> modifyMVar_ waiting (return . (sem:)) >> wait sem
    }

signalDistJob :: Exception e => DistJob i o -> e -> IO ()
signalDistJob (DistJob{roster=mvar}) e = readMVar mvar >>= mapM_ (flip throwTo e) . S.elems

