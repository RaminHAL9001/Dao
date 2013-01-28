-- "src/Dao/Debug.hs"  data types used for debugging the Dao System.
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


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Debug where

-- | Used to debug multi-threading problems, deadlocks in particular. The basic idea is to rewrite
-- the Haskell code that you want to debug such that all pertinent system calls, like
-- 'Control.Concurrent.forkIO', 'Control.Concurrent.MVar.modifyMVar', and 'System.IO.putStrLn' with
-- debugging equivalents. Each of these calls then logs an event with the debugger, and these events
-- can be written to a file.

import           Dao.String

import           Control.Exception
import           Control.Concurrent
import           Control.Monad.Reader

import           Data.Maybe
import           Data.Word
import           Data.IORef
import qualified Data.Map as M

import           System.IO

-- | Every function in "Dao.Debug.ON" takes a source code 'Language.Haskell.TH.Location', which can
-- be generated by template Haskell using the $('loc') splice given below. If you don't need a
-- "Dao.Debug.ON" function call to emit source code location at a particular call site, you can pass
-- 'nonloc' instead.
type MLoc = Maybe (String, Int, Int)

-- | The most fundamental types in "Control.Concurrent" are 'Control.Concurrent.MVar',
-- 'Control.Concurrent.Chan' and 'Control.Concurrent.QSem'. These types are wrapped in a data type
-- that provides more useful information about the variable, particularly a comment string
-- describing the nature of the variable, and a unique identifier.
data DVar v
  = DVar { dbgVar :: v }
  | IDVar { dbgVar :: v, varID :: Word64, dbgTypeName :: String, dbgVarComment :: String }

-- | To avoid dealing with Haskell "forall"ed types, I have here a type which contains the same
-- information found in a 'IDVar' but without the type specific 'dbgVar'. Use 'dVarInfo' to
-- construct this data.
data DVarInfo
  = DNoInfo { varFunction :: String }
  | DVarInfo
    { varFunction :: String
    , infoVarID :: Word64
    , infoTypeName :: String
    , infoVarComment :: String
    }

-- | Takes the interesting information from 'IDVar' so it can be stored in a 'DEvent' data
-- structure.
dVarInfo :: String -> DVar v -> DVarInfo
dVarInfo func dvar = case dvar of
  DVar  _          -> DNoInfo func
  IDVar _ i nm com ->
    DVarInfo{varFunction = func, infoVarID = i, infoTypeName = nm, infoVarComment = com}

type DQSem = DVar QSem
type DMVar v = DVar (MVar v)
type DChan v = DVar (Chan v)

-- | These are event signals that are sent when certain functions from the "Control.Concurrent"
-- library are used in a debugging context. The neme of each constructor closely matches the name of
-- the "Control.Concurrent" function that signals it.
data DEvent
  = DMsg            MLoc ThreadId           String
    -- ^ does nothing more than send a 'Prelude.String' message to the debugger.
  | DVarAction      MLoc ThreadId           DVarInfo
    -- ^ Some 'DVar' was updated somewhere.
  | DStdout         MLoc ThreadId           String
  | DStderr         MLoc ThreadId           String
  | DFork           MLoc ThreadId ThreadId  String
  | DCatch          MLoc ThreadId           SomeException
  | DUncaught       MLoc ThreadId           SomeException
  | DThrowTo        MLoc ThreadId ThreadId  SomeException
  | DThrow          MLoc ThreadId           SomeException
  | DThreadDelay    MLoc ThreadId Int
  | DThreadUndelay  MLoc ThreadId Int
  | DThreadDied     MLoc ThreadId
    -- ^ a signal sent when a thread dies (assuming the thread must have been created with
    -- 'Dao.Debug.ON.dFork')
  | DThreadKilled   MLoc ThreadId           SomeException
    -- ^ a signal sent when a thread is killed (assuming the thread must have been created with
    -- 'Dao.Debug.ON.dFork').
  | DHalt -- ^ Sent when the thread being debugged is done.

-- | This is a state that is passed around to every function that uses the "Dao.Debug" facilities.
-- If you have a state data type @R@ that you pass around your program via the
-- 'Control.Monad.Reader.ReaderT' monad, instantiate @R@ into the 'Bugged' class, and your monad
-- will be able to use any of the functions in the "Dao.Debug.ON" module.
data Debugger
  = Debugger
    { debugChan        :: Chan DEvent
    , uniqueIDGen      :: MVar Word64
    , debugThreadTable :: ThreadTable
    , debugFileHandle  :: Handle
    , debugComments    :: String
    , debugLogWriter   :: LogWriter
    , debugShutdown    :: IO ()
    }

-- | This function is called by 'Dao.Debug.ON.debugToFile', you should not need to call it yourself.
initDebugger :: String -> LogWriter -> Handle -> IO Debugger
initDebugger com log file = do
  idgen <- newMVar 0
  chan  <- newChan
  ttab  <- newIORef M.empty
  return $
    Debugger
    { debugChan = chan
    , uniqueIDGen = idgen
    , debugThreadTable = ttab
    , debugFileHandle = file
    , debugComments = com
    , debugLogWriter = log
    , debugShutdown = return ()
    }

type LogWriter = Debugger -> DEvent -> IO ()

-- | This is the type for the table that maps 'Control.Concurrent.ThreadId's to 'Dao.String.Name's
-- which allows the debug logger to produce more descriptive reports regarding threads.
--
-- 'Control.Concurrent.ThreadId's are not wrapped up into a debugging data type the way
-- 'Control.Concurrent.MVar.MVar's, 'Control.Concurrent.Chan.Chan's, and
-- 'Control.Concurrent.QSem.QSem's are. Instead, whenever a thread is created using
-- 'Dao.Debug.ON.dFork' it is mapped to a 'Dao.String.Name', and the debug log writer looks up the
-- name mapped to that 'Control.Concurrent.ThreadId' only when it is necessary to produce a log
-- message. If there were some wrapped @DThreadId@ type, it may need to perform this lookup more
-- often (although Haskell's lazy evaluation might prevent that).  It seemed to be the simpler
-- option to leave 'Control.Concurrent.ThreadId's as they were, rather than wrapping them and
-- defining a debugging version of 'Control.Concurrent.myThreadId'.
type ThreadTable = IORef (M.Map ThreadId Name)

-- | The 'Bugged' class uses 'DebugHandle's instead of just plain 'Debugger's. This allows you to
-- more finely tune which portions of the program are debugged. You can disable debugging by simply
-- having the 'askDebug' function return 'Data.Maybe.Nothing'.
type DebugHandle = Maybe Debugger

----------------------------------------------------------------------------------------------------

-- | Debuggers run in the 'Control.Monad.Reader.ReaderT' monad. If you have a Reader state @R@, and
-- @R@ contains a 'DebugHandle', then instantiate @R@ into the 'Bugged' class, and any monad of the
-- type @'Control.Monad.Reader.ReaderT' R IO@ will be able to run any of the debugging functions in
-- the "Dao.Debug.ON" module.
-- 
-- The @()@ type instantiates this class, but it *always* returns 'Data.Maybe.Nothing', so if @()@
-- is the data you are using for your 'Control.Monad.Reader.ReaderT' state, debugging will always be
-- disabled, no matter what you do. This allows you to create a monad like this:
-- @type My a = ReaderT Debugger IO a -- enable debugging everywhere in My monad.@
-- ...now your debugger is enabled...
-- @type My a = ReaderT () IO a -- disable debugging everywhere in My monad.@
-- ...now your debugger is disabled, your monad is equivalent to @IO@, but you can still use all of
-- the functions in "Dao.Debug.ON" without rewriting any code!
--
-- You can enable/disable portions of your program by declaring your monad to be of the type:
-- @type My a = ReaderT DebugHandle IO a@
-- and execute the monad with
-- @'Control.Monad.Reader.runReaderT myMonad ('Data.Maybe.Just' myDebuggerState)@
-- but you can disable debugging in portions of the program by using 'Control.Monad.ReaderT.local'
-- like so:
-- @do  'Control.Monad.Reader.local' ('Prelude.const' 'Data.Maybe.Nothing') myMonad@
class Bugged r where
  askDebug :: ReaderT r IO DebugHandle
  -- ^ retrieve the 'DebugHandle' from the 'Control.Monad.Reader.ReaderT' state @run@. Return
  -- 'Data.Maybe.Nothing' to disable debugging for a portion of the program.
  setDebug :: DebugHandle -> r -> r
  -- ^ places the 'DebugHandle' into your 'Control.Monad.Reader.ReaderT' state.

instance Bugged Debugger where
  askDebug = fmap Just ask
  setDebug dbg r = fromMaybe r dbg

instance Bugged (Maybe Debugger) where
  askDebug = ask
  setDebug dbg _ = dbg

instance Bugged () where
  askDebug = return Nothing
  setDebug _ () = ()

