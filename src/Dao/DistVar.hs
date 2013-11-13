-- "src/Dao/DistVar.hs"  a concurrent API for distributing folds across
-- multiple threads.
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

module Control.Concurrent.DistVar where

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

newtype ForkIO = ForkIO (IO () -> IO ThreadId)
newtype ForkOS = ForkOS (IO () -> IO ThreadId)

class Forker f where { newThread :: f -> IO () -> IO ThreadId }
instance Forker ForkIO where { newThread (ForkIO f) = f }
instance Forker ForkOS where { newThread (ForkOS f) = f }

newtype Workers = Workers (MVar (S.Set ThreadId))

-- | A distributed job lets you specify a fold function that can be evaluated across multiple
-- threads. This should be used when the output of the fold function should produce a list of
-- results in no particular order.
data DistJob i o
  = DistJob
    { inputChan    :: DistVar (Maybe i)
    , outputChan   :: DistVar o
    , workers      :: Workers
      -- | Set how many worker threads are avaialable. If more threads than the number
      -- of currently existing threads are requested, more threads are forked until the number of
      -- available workers matches the requested number. If fewer threads than
      -- the number of currently existing threads are requested, existing threads are signalled to
      -- finish their current task and halt.
    , setWorkerCount :: Int -> IO ()
    }

workerCount :: DistJob i o -> IO Int
workerCount (DistJob{workers=Workers wrkrs}) = fmap S.size (readMVar wrkrs)

-- | Asynchronous send a single input item to the 'DistJob' (never blocks).
send :: DistJob i o -> i -> IO ()
send job = putDistVar (inputChan job) . Just

-- | Syncrhonous receive a single output item from the 'DistJob' (blocks until a result comes in).
receive :: DistJob i o -> IO o
receive job = takeDistVar (outputChan job)

newDistJob :: Forker fork => fork -> (i -> IO o) -> IO (DistJob i o)
newDistJob fork fold = do
  ichan <- newEmptyDistVar
  ochan <- newEmptyDistVar
  wrkrs <- newMVar S.empty
  let newWorker func = newThread fork $ do
        tid <- myThreadId
        (fix $ \loop -> takeDistVar ichan >>=
            maybe (return ()) (\i -> yield >> func i >>= \o -> putDistVar ochan o >> yield >> loop)
          ) `finally` modifyMVar_ wrkrs (return . S.delete tid)
  return $
    DistJob
    { inputChan  = ichan
    , outputChan = ochan
    , workers    = Workers wrkrs
    , setWorkerCount = \i' -> let i = max 0 i' in do
        size <- S.size <$> readMVar wrkrs
        case compare size i of
          LT -> do
            newWrkrs <- replicateM i (newWorker fold)
            modifyMVar_ wrkrs $ return . flip (foldl (flip S.insert)) newWrkrs
          GT -> replicateM_ (size-i) (putDistVar ichan Nothing)
          EQ -> return ()
    }

