-- "src/Dao/Concurrent.hs"  utilities for concurrent programming.
-- 
-- Copyright (C) 2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

module Dao.Concurrent where

import           Data.Function
import qualified Data.Text as Strict

import           Control.Concurrent
import           Control.Exception

import           System.IO
import           System.Timeout

-- | Multiply the given integer by the number required to create a value that would cause a 1-second
-- delay when passed to 'Control.Concurrent.threadDelay'.
seconds :: Int -> Int
seconds = (* 1000000)

-- | A thread safe way to send reports to 'System.IO.stdout' or 'System.IO.stderr' without causing
-- garbled text when multiple threads try to write a message.
data Reporter
  = Reporter
    { reporterThreadId :: ThreadId
    , sendToReporter   :: Maybe (Bool, Strict.Text) -> IO ()
    }

-- | Creates a 'Control.Concurrent.MVar.MVar' wrapper around both handles, ensuring sure multiple
-- threads do not write to the 'System.IO.Handle's at the same time, garbling the text. Pass two
-- 'System.IO.Handle's, the output handle, and the error handle.
makeReporter :: Handle -> Handle -> IO Reporter
makeReporter stdout stderr = do
  mvar     <- newEmptyMVar
  threadId <- forkIO $ fix $ \loop -> takeMVar mvar >>= \msg -> case msg of
    Nothing            -> return ()
    Just (notErr, msg) ->
      hPutStrLn (if notErr then stdout else stderr) (Strict.unpack msg) >> loop
  return $
    Reporter
    { reporterThreadId = threadId
    , sendToReporter   = putMVar mvar
    }

-- | Calling this function with 'Prelude.True' will send the report string to 'System.IO.stdout'
-- Calling this function with 'Prelude.False' will send the report string to 'System.IO.stderr'
report :: Reporter -> Bool -> String -> IO ()
report o notErr msg = do
  msg <- evaluate (Strict.pack msg)
  sendToReporter o (Just (notErr, msg))

reportOK :: Reporter -> String -> IO ()
reportOK o = report o True

reportERR :: Reporter -> String -> IO ()
reportERR o = report o False

-- | Halt the reporter thread gracefully.
recallReporter :: Reporter -> IO ()
recallReporter = flip sendToReporter Nothing

-- | Like 'Control.Monad.mapM', but time-limits each execution of the mapping function to the given
-- number of microseconds. This means the maximum time that this function could possibly take, in
-- microseconds is @t * length items@, where @t@ is the time limit of each execution, and @items@ is
-- the list of items over which to map.
timeoutMapM :: Int -> (a -> IO b) -> [a] -> IO [Either a b]
timeoutMapM t f = mapM $ \o -> timeout t (f o) >>= return . maybe (Left o) Right

timoutForM :: Int -> [a] -> (a -> IO b) -> IO [Either a b]
timoutForM i = flip (timeoutMapM i)

-- | Like 'Control.Monad.foldM', but time-limits each execution of the folding function to the given
-- number of microseconds. If any single execution of the fold function exceeds the time limit,
-- evaluation is aborted and 'Prelude.Left' is returned containing the input that caused the
-- execution to timeout.
timeoutFoldM :: Int -> (a -> b -> IO a) -> a -> [b] -> IO (Either b a)
timeoutFoldM t = fix $ \loop f a bx -> case bx of
  []   -> return $ Right a
  b:bx -> timeout t (f a b) >>= maybe (return $ Left b) (flip (loop f) bx)

