-- "src/Dao/Debug/ON.hs"  provides a substitute for many functions in
-- "Control.Concurrent". The substitute functions behave similarly to
-- the originals, but also pass messages to a debugging thread.
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
{-# LANGUAGE Rank2Types #-}

module Dao.Debug.ON
  ( module Dao.Debug
  , module Dao.Debug.ON
  ) where

-- | This module provides replacements for many key functions in "System.IO" and
-- "Control.Concurrent". The replacement functions require you pass additional parameters,
-- particularly a 'nonloc' or a template Haskell splace @$('loc')@. By using the functions in this
-- module in place of the functions exported by "Control.Concurrent" is that you messages will be
-- printed to the debug log spcified by the 'enableDebug' function. This module may not be imported
-- at the same time as "Dao.Debug.OFF" becuase it provides the excat same API, the only difference
-- being that the debugging operations are "enabled" with this module.

import           Dao.Debug
import           Dao.String

import           Control.Exception
import           Control.Concurrent
import           Control.Monad.Reader

import           Data.Maybe
import           Data.List hiding (lookup)
import           Data.Word
import           Data.IORef
import qualified Data.Map as M

import           System.IO
import           System.Time

import qualified Language.Haskell.TH as Q

----------------------------------------------------------------------------------------------------

-- | A template-Haskell splice to get the file location. The splice @$'loc'@ appears as an
-- argument to every debugging function, unless you do not want location information printed for
-- that function, in which case you can pass 'nonloc' in its place.
loc :: Q.ExpQ
loc = do
  here <- Q.location
  let i rec = Q.IntegerL (fromIntegral (rec (Q.loc_start here)))
  return $ Q.AppE (Q.ConE (Q.mkName "Prelude.Just")) $ Q.TupE $ map Q.LitE $
    [Q.StringL (Q.loc_filename here), i fst, i snd]

-- | "Exclude location information" from a debug message. If you want to scrub your code of splices
-- and stop using template Haskell, simply replace every @$'loc'@ splice with the plain word
-- 'xloc' using "sed", or your favorite string find/replace function.
xloc :: MLoc
xloc = Nothing

----------------------------------------------------------------------------------------------------

-- | Initializes a 'Dao.Debug.Debugger' to use a 'System.IO.Handle'. You should call this instead of
-- calling 'Dao.Debug.initDebugger' directly.
-- and begins debugging a given function using 'debugIO', closing the file when 'debugIO' completes
-- evluation.
debugToHandle :: MLoc -> String -> LogWriter -> Handle -> IO DebugHandle
debugToHandle loc logmsg log file = fmap Just (initDebugger logmsg log file)

-- | Initializes a 'Dao.Debug.Debugger', and opens a 'System.IO.Handle' with the given 'FilePath'
-- and begins debugging a given function using 'debugIO', closing the file when 'debugIO' completes
-- evluation.
debugToFile :: MLoc -> String -> LogWriter -> FilePath -> IOMode -> IO DebugHandle
debugToFile loc logmsg log path mode = do
  debug <- (if null path then return stderr else openFile path mode) >>= initDebugger logmsg log
  return $ Just $
    debug
    { debugShutdown = do
        let file = debugFileHandle debug
        closed <- hIsClosed file
        debugShutdown debug
        unless (file==stderr || closed) (hClose file)
    }

-- | This function provides an easy way to disable debugging when modifying your Haskell code.
-- Simply replace one of the above '
-- All parameters passed to this function are ignored as debugging is disabled, so it is safe to
-- pass 'Prelude.undefined' if you are at a loss for types to fill in here.
disableDebugFile :: MLoc -> String -> LogWriter -> FilePath -> IOMode -> IO DebugHandle
disableDebugFile _ _ _ _ _ = return Nothing

debugTimestamp :: MLoc -> String -> Handle -> IO ()
debugTimestamp loc msg file = hIsClosed file >>= \closed -> unless closed $ do
  let here = case loc of
        Nothing               -> msg
        Just (path, row, col) ->
          path++':':show row++':':show col++(if null msg then msg else ": "++msg)
  msg <- fmap ((here++) . (' ':) . show) getClockTime >>= evaluate
  hPutStrLn file msg
  putStrLn msg

-- | This is how you "startup" the debugger. Pass a function you wish to debug, 'debugIO' will fork
-- a thread and setup the facilities to debug execution of that function. The supplied 'DebugHandle'
-- is used to execute all debug functions, so you need to modify the code of the @IO@ function
-- passed to the 'debugIO' function such that it sends debug messages using that 'DebugHandle'.
debugIO :: Bugged r => MLoc -> String -> DebugHandle -> r -> (ReaderT r IO a) -> IO (Maybe a)
debugIO loc msg debug r testTarget = case debug of
  Nothing    -> fmap Just $ runReaderT testTarget r
  Just debug -> do
    let file = debugFileHandle debug
        log = debugLog debug
    hSetBuffering file LineBuffering
    hPutStrLn file ("DEBUG HOST started: "++debugComments debug)
    retvar <- newEmptyMVar
    child  <- flip runReaderT (setDebug (Just debug) r) $
      dFork forkIO loc msg (testTarget >>= lift . putMVar retvar . Just)
    let ttab = debugThreadTable debug
    modifyIORef ttab (M.insert child (ustr msg))
    let chan = debugChan debug
        file = debugFileHandle debug
        loopException (SomeException e) = do
          let msg = "DEBUG HOST killed, "++show e
          debugTimestamp loc msg file
          readIORef ttab >>= mapM_ killThread . M.keys
          putMVar retvar Nothing
        loop = do
          event <- readChan chan
          log event >>= evaluate
          t <- readIORef ttab
          continue <- case event of
            DFork         _ _ t name -> modifyIORef ttab (M.insert t (ustr name)) >> return True
            DThreadDied     _ t      -> modifyIORef ttab (M.delete t) >> return True
            DThreadKilled   _ t _    -> modifyIORef ttab (M.delete t) >> return True
            DHalt                    -> return False
            _            | M.null t  -> return False
                         | otherwise -> return True
          empty <- isEmptyMVar retvar
          when (continue && empty) loop
    handle loopException loop
    a <- takeMVar retvar
    hPutStrLn file "DEBUG HOST thread exited normally"
    return a

-- | When you are done with debugging all together, evaluate this function.
haltDebugger :: DebugHandle -> IO ()
haltDebugger debug = case debug of
  Nothing -> return ()
  Just debug -> debugShutdown debug

----------------------------------------------------------------------------------------------------

showThID :: ThreadId -> String -> String
showThID tid msg = "(#"
  ++ let th = show tid in fromMaybe th (stripPrefix "ThreadId " th)
  ++ (if null msg then "" else ' ':msg)
  ++ ")"

-- | Many calls to 'dDebug' will simply need to execute some "Control.Concurrent" function, test if
-- the debugger is defined (i.e. not 'Prelude.Nothing'), and if the debugger is defined then pass a
-- message to it. This function does exactly that given the two functions you pass to it: the
-- "Control.Concurrent" function (in the @IO@ monad) and a pure function that creates the
-- 'Dao.Debug.DEvent' from the 'Control.Concurrent.ThreadId' of the current thread passed to it.
dDebug :: Bugged r => IO a -> (ThreadId -> DEvent) -> ReaderT r IO a
dDebug doSomething sendMessage = askDebug >>= \debug -> lift $ case debug of
  Nothing    -> doSomething
  Just debug -> myThreadId >>= event debug . sendMessage >> doSomething

-- | Generates a unique identifier that can be attached to 'Dao.Debug.DMVar's, and
-- 'Dao.Debug.DQSem's.
uniqueID :: Debugger -> IO Word64
uniqueID debug = modifyMVar (uniqueIDGen debug) (\a -> let b=a+1 in return (b,b))

event :: Debugger -> DEvent -> IO ()
event debug evt = writeChan (debugChan debug) evt >>= evaluate

----------------------------------------------------------------------------------------------------

-- | Does not emit any debuggign information. This function is provided for convenience. It simply
-- lifts the 'Control.Concurrent.myThreadId' function into the 'Control.Monad.Reader.ReaderT' monad.
dMyThreadId :: ReaderT r IO ThreadId
dMyThreadId = lift myThreadId

-- | Does not emit any debuggign information. This function is provided for convenience. It simply
-- lifts the 'Control.Concurrent.yield' function into the 'Control.Monad.Reader.ReaderT' monad.
dYield :: ReaderT r IO ()
dYield = lift yield

dThrowIO :: Exception e => e -> ReaderT r IO any
dThrowIO e = lift (throwIO e)

-- | Emits a 'Dao.Debug.DThreadDelay' signal and called 'Control.Concurrent.threadDelay'.
dThreadDelay :: Bugged r => MLoc -> Int -> ReaderT r IO ()
dThreadDelay loc i = askDebug >>= \debug -> case debug of
  Nothing    -> lift (threadDelay i)
  Just debug -> lift $ do
    this <- myThreadId
    event debug (DThreadDelay loc this i)
    threadDelay i
    event debug (DThreadUndelay loc this i)

-- | Does not emit any debugging information. This function will lookup the 'Dao.String.Name'
-- that was associated with a given 'Control.Concurrent.ThreadId' when it was created by 'dFork'.
dMyThreadLabel :: Bugged r => ThreadId -> ReaderT r IO (Maybe Name)
dMyThreadLabel thread = askDebug >>= \debug -> lift $ case debug of
  Nothing    -> return Nothing
  Just debug -> fmap (M.lookup thread) (readIORef (debugThreadTable debug))

-- | Emits a 'Dao.Debug.DMsg' signal to the debugger.
dMessage :: Bugged r => MLoc -> String -> ReaderT r IO ()
dMessage loc msg = dDebug (return ()) (\this -> DMsg loc this msg)

-- | Given a function to execute, emit a 'Dao.Debug.DMsg' once before and once after the function
-- has been evaluated.
dStack :: Bugged r => MLoc -> String -> ReaderT r IO a -> ReaderT r IO a
dStack loc fname func = askDebug >>= \debug -> case debug of
  Nothing    -> func
  Just debug -> ask >>= \r -> lift $ do
    let ch = debugChan debug
        msg = show fname
    this <- myThreadId
    writeChan (debugChan debug) (DMsg loc this ("begin "++msg)) >>= evaluate
    a <- flip handle (runReaderT func r) $ \ (SomeException e) -> do
      writeChan ch (DMsg loc this (msg++" terminated by exception: "++show e)) >>= evaluate
      throwIO e
    writeChan ch (DMsg loc this ("end   "++msg)) >>= evaluate
    return a

-- | Used to derive 'dPutStrLn', 'dPutStr', and 'dPutStrErr'.
dPutString
  :: Bugged r
  => (MLoc -> ThreadId -> String -> DEvent)
  -> (String -> IO ())
  -> MLoc
  -> String
  -> ReaderT r IO ()
dPutString cons io loc msg = dDebug (io msg) $ \this -> cons loc this msg

-- | Emits a 'Dao.Debug.DStdout' signal to the debugger and calls 'System.IO.putStrLn'.
dPutStrLn :: Bugged r => MLoc -> String -> ReaderT r IO ()
dPutStrLn = dPutString DStdout putStrLn

-- | Emits a 'Dao.Debug.DStdout' signal to the debugger and calls 'System.IO.putStr'.
dPutStr :: Bugged r => MLoc -> String -> ReaderT r IO ()
dPutStr = dPutString DStdout putStr

-- | Emits a 'Dao.Debug.DStderr' signal to the debugger and calls
-- @'System.IO.hPutStrLn' 'System.IO.stderr'@.
dPutStrErr :: Bugged r => MLoc -> String -> ReaderT r IO ()
dPutStrErr = dPutString DStderr (hPutStrLn stderr)

----------------------------------------------------------------------------------------------------

-- | Emits a 'Dao.Debug.DFork' signal and calls whichever fork function you tell it to: either
-- 'Control.Concurrent.forkIO' or 'Control.Concurrent.forkOS'.
dFork
  :: Bugged r
  => (IO () -> IO ThreadId)
  -> MLoc
  -> String
  -> ReaderT r IO ()
  -> ReaderT r IO ThreadId
dFork fork loc name ioFunc = ask >>= \r -> askDebug >>= \debug -> lift $ case debug of
  Nothing    -> fork (runReaderT ioFunc r)
  Just debug -> do
    let end = myThreadId >>= \child -> event debug $ DThreadDied loc child
        h err@(SomeException e) = myThreadId >>= \child ->
          event debug (DThreadKilled loc child (SomeException e)) >> throwIO e
    child  <- fork (handle h (runReaderT ioFunc r >> end))
    parent <- myThreadId
    event debug $ DFork loc parent child name
    return child

-- | Emits a 'Dao.Debug.DThrowTo' signal to the debugger and calls 'Control.Concurrent.throwTo'.
dThrowTo :: (Exception e, Bugged r) => MLoc -> ThreadId -> e -> ReaderT r IO ()
dThrowTo loc target err = dDebug (throwTo target err) $ \this ->
  DThrowTo loc this target (SomeException err)

-- | Emits a 'Dao.Debug.DThrowTo' signal to the debugger and calls
-- 'Control.Concurrent.killThread'.
dKillThread :: Bugged r => MLoc -> ThreadId -> ReaderT r IO ()
dKillThread loc target = dDebug (killThread target) $ \this ->
  DThrowTo loc this target (SomeException ThreadKilled)

-- | Behaves just like 'Control.Exception.catch', but makes sure to send a 'DCatch'
-- signal.
dCatch :: (Exception e, Bugged r) => MLoc -> ReaderT r IO a -> (e -> ReaderT r IO a) -> ReaderT r IO a
dCatch loc tryFunc catchFunc = askDebug >>= \debug -> case debug of
  Nothing    -> ReaderT $ \r -> handle (\e -> runReaderT (catchFunc e) r) (runReaderT tryFunc r)
  Just debug -> ReaderT $ \r -> do
    this <- myThreadId
    flip handle (runReaderT tryFunc r) $ \e ->
      event debug (DCatch loc this (SomeException e)) >> runReaderT (catchFunc e) r

-- | Like 'dCatch' but with it's final two parameters flipped.
dHandle :: (Exception e, Bugged r) => MLoc -> (e -> ReaderT r IO a) -> ReaderT r IO a -> ReaderT r IO a
dHandle loc catchFunc tryFunc = dCatch loc tryFunc catchFunc

-- | Behaves just like 'Control.Exception.catches', but takes a list of 'Dao.Debug.DHandler's which
-- are converted to 'Control.Exception.Handlers' as necessary.
dCatches :: Bugged r => ReaderT r IO a -> [DHandler r a] -> ReaderT r IO a
dCatches tryfn dhands = do
  r <- ask
  debug <- askDebug
  lift (catches (runReaderT tryfn r) (map (mkh r debug) dhands)) where
    mkh r debug dhandl = case debug of
      Nothing    -> (getHandler dhandl) r (\_ -> return ())
      Just debug -> (getHandler dhandl) r $ \err -> do
        this <- myThreadId
        event debug (DCatch (getHandlerMLoc dhandl) this err)

-- | Same as 'dCatches' but with arguments reversed
dHandles :: Bugged r => [DHandler r a] -> ReaderT r IO a -> ReaderT r IO a
dHandles = flip dCatches

-- | Emits a 'DThrow' signal, calls 'Control.Exception.throwIO'.
dThrow :: (Exception e, Bugged r) => MLoc -> e -> ReaderT r IO ignored
dThrow loc err = dDebug (throwIO err) $ \this -> DThrow loc this (SomeException err)

----------------------------------------------------------------------------------------------------

-- | Used to derive the 'dNewChan', 'dNewQSem', and 'dNewMVar' functions.
dMakeVar
  :: Bugged r
  => String
  -> String
  -> IO v
  -> MLoc
  -> String
  -> ReaderT r IO (DVar v)
dMakeVar typeName funcName newIO loc msg = askDebug >>= \debug -> lift $ case debug of
  Nothing    -> fmap DVar newIO
  Just debug -> do
    this <- myThreadId
    i    <- uniqueID debug
    var  <- newIO
    let dvar = IDVar{dbgVar = var, varID = i, dbgTypeName = typeName, dbgVarComment = msg}
    event debug (DVarAction loc this (dVarInfo funcName dvar))
    return dvar

-- | Used to derive 'dReadMVar', 'dSwapMVar', and 'dTakeMVar'. Emits a 'DTakeMVar' signal.
dVar ::
  Bugged r
  => (v -> IO a)
  -> String
  -> MLoc
  -> DVar v
  -> ReaderT r IO a
dVar withVar funcName loc var =
  dDebug (withVar (dbgVar var)) $ \this -> DVarAction loc this (dVarInfo funcName var)

----------------------------------------------------------------------------------------------------

-- | Emits a 'DNewChan' signal and calls 'Control.Concurrent.Chan.newChan'.
dNewChan :: Bugged r => MLoc -> String -> ReaderT r IO (DChan v)
dNewChan = dMakeVar "Chan" "newChan" newChan

-- | Emits a 'DWriteChan' signal and calls 'Control.Concurrent.Chan.writeChan'.
dWriteChan :: Bugged r => MLoc -> DChan v -> v -> ReaderT r IO ()
dWriteChan loc var v = dVar (flip writeChan v) "writeChan" loc var

-- | Emits a 'DWriteChan' signal and calls 'Control.Concurrent.Chan.writehan'.
dReadChan :: Bugged r => MLoc -> DChan v -> ReaderT r IO v
dReadChan = dVar readChan "readChan"

----------------------------------------------------------------------------------------------------

-- | Emits a 'DNewQSem' signal and calls 'Control.Concurrent.Chan.newChan'.
dNewQSem :: Bugged r => MLoc -> String -> Int -> ReaderT r IO DQSem
dNewQSem loc msg i = dMakeVar "QSem" ("newQSem "++show i) (newQSem i) loc msg

-- | Emits a 'DSignqlQSem' signal and calls 'Control.Concurrent.QSem.signalQSem'.
dSignalQSem :: Bugged r => MLoc -> DQSem -> ReaderT r IO ()
dSignalQSem = dVar signalQSem "signalQSem"

-- | Emits a 'DSignalQSem' signal and calls 'Control.Concurrent.QSem.signalQSem'.
dWaitQSem :: Bugged r => MLoc -> DQSem -> ReaderT r IO ()
dWaitQSem = dVar waitQSem "waitQSem"

----------------------------------------------------------------------------------------------------

-- | Emits a 'DNewMVar' signal and calls 'Control.Concurrent.MVar.newMVar'.
dNewMVar :: Bugged r => MLoc -> String -> v -> ReaderT r IO (DMVar v)
dNewMVar loc msg v = dMakeVar "MVar" "newMVar" (newMVar v) loc msg

-- | Emits a 'DNewEmptyMVar' signal and calls 'Control.Concurrent.MVar.newEmptyMVar'.
dNewEmptyMVar :: Bugged r => MLoc -> String -> ReaderT r IO (DMVar v)
dNewEmptyMVar = dMakeVar "MVar" "newEmptyMVar" newEmptyMVar

-- | Emits a 'DPutMVar' signal and calls 'Control.Concurrent.MVar.putMVar'.
dPutMVar :: Bugged r => MLoc -> DMVar v -> v -> ReaderT r IO ()
dPutMVar loc var v = dVar (flip putMVar v) "putMVar" loc var

-- | Emits a 'DTakeMVar' signal and calls 'Control.Concurrent.MVar.takeMVar'.
dTakeMVar :: Bugged r => MLoc -> DMVar v -> ReaderT r IO v
dTakeMVar = dVar takeMVar "takeMVar"

-- | Emits a 'DReadMVar' signal and calls 'Control.Concurrent.MVar.readMVar'.
dReadMVar :: Bugged r => MLoc -> DMVar v -> ReaderT r IO v
dReadMVar = dVar readMVar "readMVar"

-- | Emits a 'DReadMVar' signal and calls 'Control.Concurrent.MVar.swapMVar'.
dSwapMVar :: Bugged r => MLoc -> DMVar v -> v -> ReaderT r IO v
dSwapMVar loc var v = dVar (flip swapMVar v) "swapMVar" loc var

-- | Emits a 'DModifyMVar' signal and calls 'Control.Concurrent.MVar.modifyMVar'.
dModifyMVar :: Bugged r => MLoc -> DMVar v -> (v -> ReaderT r IO (v, a)) -> ReaderT r IO a
dModifyMVar loc var updFunc = ask >>= \r ->
  dVar (flip modifyMVar (\v -> runReaderT (updFunc v) r)) "modifyMVar" loc var

-- | Emits a 'DModifyMVar' signal and calls 'Control.Concurrent.MVar.modifyMVar_'.
dModifyMVar_ :: Bugged r => MLoc -> DMVar v -> (v -> ReaderT r IO v) -> ReaderT r IO ()
dModifyMVar_ loc var updFunc = ask >>= \r ->
  dVar (flip modifyMVar_ (\v -> runReaderT (updFunc v) r)) "modifyMVar" loc var

----------------------------------------------------------------------------------------------------

-- | This is the basic event log writing function. Every event it receives will be translated into a
-- descriptive string and written to the 'System.IO.Handle'. Alternate log writters may be used, but
-- this is the simplest one.
debugLog :: LogWriter
debugLog debug evt = case evt of
  DMsg            loc th      msg -> p loc th $ msg
  DVarAction      loc th      var -> p loc th $ case var of
    DNoInfo  fn          -> fn
    DVarInfo fn i ty com -> fn++" ("++ty++'#':show i++(if null com then "" else ' ':com)++")"
  DStdout         loc th      msg -> p loc th $ prin "stdout" msg
  DStderr         loc th      msg -> p loc th $ prin "stderr" msg
  DFork           loc th chld nm  -> p loc th $ "NEW THREAD "++showThID chld nm
  DThreadDied     loc th          -> p loc th $ "THREAD COMPLETED"
  DThreadDelay    loc th time     -> p loc th $ "threadDelay "++show time
  DThreadUndelay  loc th time     -> p loc th $ "DONE WAITING "++show time
  DCatch          loc th      (SomeException err) -> p loc th $ "caught exception: "++show err
  DThrow          loc th      (SomeException err) -> p loc th $ "throwIO ("++show err++")"
  DThrowTo        loc th targ (SomeException err) ->
    p loc th . (\nm -> "throwTo "++showThID targ nm++" ("++show err++")") =<< lkup targ
  DThreadKilled   loc th      (SomeException err) ->
    p loc th $ "THREAD TERMINATED, uncaught exception "++show err
  DHalt -> hPutStrLn file "DEBUG HOST debug target thread execution halted."
  where
    file = debugFileHandle debug
    ttab = debugThreadTable debug
    here loc = case loc of
      Nothing            -> ""
      Just (fname, a, b) -> fname++':':show a++':':show b++": "
    lkup th = do -- Lookup the name that mapped to this thread when 'dFork' created the thread.
      name <- fmap (M.lookup th) (readIORef ttab)
      return $ showThID th $ case name of
        Nothing   -> ""
        Just name -> uchars name
    prin fd msg = fd++' ':
      if not (length msg < 80 && isJust (findIndex (\c -> c=='\n' || c=='\r') msg))
        then concatMap ("\n\t"++) (lines msg)
        else show msg
    p loc th msg = do
      th <- lkup th
      hPutStrLn file (here loc++th++(if null msg then "" else ' ':msg))

