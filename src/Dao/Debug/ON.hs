-- "src/Dao/Debug/ON.hs"  provides a substitute for many functions in
-- "Control.Concurrent". The substitute functions behave similarly to
-- the originals, but also pass messages to a debugging thread.
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

import           Prelude  hiding (catch)

import           Dao.Debug
import           Dao.String

import           Control.Exception
import           Control.Concurrent
import qualified Control.Concurrent.SSem as Sem
import           Control.Monad.Reader

import           Data.Maybe
import           Data.List hiding (lookup)
import           Data.Word
import           Data.IORef
import           Data.Time
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
-- '$loc' using "sed", or your favorite string find/replace function.
xloc :: MLoc
xloc = Nothing

----------------------------------------------------------------------------------------------------

instance Show DThread where
  show tid = case tid of
    DThread     tid      -> concat ["(#", showThID tid, ")"]
    DebugThread tid u nm -> concat ["(#", showThID tid, show u, " ", show nm, ")"]

instance Show DUnique where
  show d = concat [show (dElapsedTime d), ".", show (dUniqueWord d)]

----------------------------------------------------------------------------------------------------

debuggableProgram :: Bugged r m => MLoc -> SetupDebugger r m -> IO ()
debuggableProgram mloc setup =
  if debugEnabled setup
    then do
      debug <- flip fmap initDebugData $ \debug ->
        debug{debugGetThreadId = (debugGetThreadId debug){dThreadName = ustr "DEBUG THREAD"}}
      event debug (DStarted mloc (debugGetThreadId debug) (debugComment setup) (debugStartTime debug))
      let debugRef = Just debug
      debug <- case debugOutputTo setup of
        DebugOutputToDefault   -> return (debug{debugPrint = hPrint stderr})
        DebugOutputToHandle  h -> return (debug{debugPrint = hPrint h, debugClose = hClose h})
        DebugOutputToChannel c -> return (debug{debugPrint = writeChan c})
        DebugOutputToFile path -> do
          h <- openFile path ReadWriteMode
          return (debug{debugPrint = hPrint h, debugClose = hClose h})
      runtime    <- initializeRuntime setup debugRef
      mvar       <- newEmptyMVar
      mainThread <- forkIO $ do
        debug <- takeMVar mvar
        handle (uncaught debug) $ do
          init (Just debug) runtime
          event debug (DThreadDied mloc (debugGetThreadId debug))
        event debug DHalt
      threadUniq <- uniqueID debug
      mainThread <- return $
        DebugThread
        { dThreadGetId  = mainThread
        , dThreadUnique = threadUniq
        , dThreadName   = ustr "MAIN THREAD"
        }
      putMVar mvar (debug{debugGetThreadId = mainThread})
      handle (\ (SomeException _) -> return ()) (loop debug)
      debugClose debug
    else initializeRuntime setup Nothing >>= init Nothing
  where
    init debugRef = debugUnliftIO (withDebugger debugRef (beginProgram setup))
    uncaught debug e = event debug (DUncaught mloc (debugGetThreadId debug) e)
    loop debug = do
      let debugRef = Just debug
      evt <- takeMVar (debugChan debug)
      debugPrint debug evt >>= evaluate
      case evt of
        DHalt -> return ()
        _     -> loop debug

-- | Many calls to 'dDebug' will simply need to execute some "Control.Concurrent" function, test if
-- the debugger is defined (i.e. not 'Prelude.Nothing'), and if the debugger is defined then pass a
-- message to it. This function does exactly that given the two functions you pass to it: the
-- "Control.Concurrent" function (in the @IO@ monad) and a pure function that creates the
-- 'Dao.Debug.DEvent' from the 'Control.Concurrent.ThreadId' of the current thread passed to it.
dDebug :: Bugged r m => IO a -> (DThread -> DEvent) -> m a
dDebug doSomething sendMessage = askDebug >>= \debugRef -> liftIO $ case debugRef of
  Nothing    -> doSomething
  Just debug -> event debug (sendMessage (debugGetThreadId debug)) >> doSomething

-- | Generates a unique identifier that can be attached to 'Dao.Debug.DMVar's.
uniqueID :: DebugData -> IO DUnique
uniqueID debug = do
  next <- modifyMVar (debugUniqueCount debug) $ \a ->
    let b = if a==maxBound then 0 else a+1 in return (b,b)
  now  <- getCurrentTime
  return $
    DUnique
    { dElapsedTime = fromRational (toRational (diffUTCTime now (debugStartTime debug)))
    , dUniqueWord = next
    }

event :: DebugData -> DEvent -> IO ()
event debug evt = putMVar (debugChan debug) evt >>= evaluate

----------------------------------------------------------------------------------------------------

-- | Does not emit any debuggign information. This function is provided for convenience. It simply
-- lifts the 'Control.Concurrent.myThreadId' function into the 'Control.Monad.Reader.ReaderT' monad.
dMyThreadId :: Bugged r m => m DThread
dMyThreadId = fmap (fmap debugGetThreadId) askDebug >>= \tid -> case tid of
  Nothing  -> liftIO (fmap DThread myThreadId)
  Just tid -> return tid

-- | Does not emit any debuggign information. This function is provided for convenience. It simply
-- lifts the 'Control.Concurrent.yield' function into the 'Control.Monad.Reader.ReaderT' monad.
dYield :: MonadIO m => m ()
dYield = liftIO yield

dThrowIO :: (MonadIO m, Exception e) => e -> m any
dThrowIO e = liftIO (throwIO e)

-- | Emits a 'Dao.Debug.DThreadDelay' signal and called 'Control.Concurrent.threadDelay'.
dThreadDelay :: Bugged r m => MLoc -> Int -> m ()
dThreadDelay loc i = askDebug >>= \debugRef -> case debugRef of
  Nothing    -> liftIO (threadDelay i)
  Just debug -> liftIO $ do
    let this = debugGetThreadId debug
    event debug (DThreadDelay loc this i)
    threadDelay i
    event debug (DThreadUndelay loc this i)

-- | Emits a 'Dao.Debug.DMsg' signal to the debugger.
dMessage :: Bugged r m => MLoc -> String -> m ()
dMessage loc msg = dDebug (return ()) (\this -> DMsg loc this msg)

-- | Given a function to execute, emit a 'Dao.Debug.DMsg' once before and once after the function
-- has been evaluated.
dStack :: Bugged r m => MLoc -> String -> m a -> m a
dStack loc fname func = askDebug >>= \debug -> case debug of
  Nothing    -> func
  Just debug -> askState >>= \r -> liftIO $ do
    let ch  = debugChan debug
        msg = show fname
        this = debugGetThreadId debug
    putMVar (debugChan debug) (DMsg loc this ("begin "++msg)) >>= evaluate
    a <- flip handle (debugUnliftIO func r) $ \ (SomeException e) -> do
      putMVar ch (DMsg loc this (msg++" terminated by exception: "++show e)) >>= evaluate
      throwIO e
    putMVar ch (DMsg loc this ("end   "++msg)) >>= evaluate
    return a

-- | Used to derive 'dPutStrLn', 'dPutStr', and 'dPutStrErr'.
dPutString
  :: Bugged r m
  => (MLoc -> DThread -> String -> DEvent)
  -> (String -> IO ())
  -> MLoc
  -> String
  -> m ()
dPutString cons io loc msg = dDebug (io msg) $ \this -> cons loc this msg

-- | Emits a 'Dao.Debug.DStdout' signal to the debugger and calls 'System.IO.putStrLn'.
dPutStrLn :: Bugged r m => MLoc -> String -> m ()
dPutStrLn = dPutString DStdout putStrLn

-- | Emits a 'Dao.Debug.DStdout' signal to the debugger and calls 'System.IO.putStr'.
dPutStr :: Bugged r m => MLoc -> String -> m ()
dPutStr = dPutString DStdout putStr

-- | Emits a 'Dao.Debug.DStderr' signal to the debugger and calls
-- @'System.IO.hPutStrLn' 'System.IO.stderr'@.
dPutStrErr :: Bugged r m => MLoc -> String -> m ()
dPutStrErr = dPutString DStderr (hPutStrLn stderr)

----------------------------------------------------------------------------------------------------

-- | Emits a 'Dao.Debug.DFork' signal and calls whichever fork function you tell it to: either
-- 'Control.Concurrent.forkIO' or 'Control.Concurrent.forkOS'.
dFork :: Bugged r m => (IO () -> IO ThreadId) -> MLoc -> String -> m () -> m DThread
dFork fork loc name ioFunc = do
  r <- askState
  debug <- askDebug
  liftIO $ case debug of
    Nothing    -> fork (debugUnliftIO ioFunc r) >>= return . DThread
    Just debug -> do
      let end child = event debug $ DThreadDied loc child
          parent = debugGetThreadId debug
          h child err@(SomeException e) = do
            event debug (DUncaught loc child (SomeException e))
            end child
      mvar <- newEmptyMVar
      newThread <- fork $ do
        child <- takeMVar mvar
        handle (h child) $ do
          debugUnliftIO ioFunc (setDebugRef (Just (debug{debugGetThreadId = child})) r)
          end child
      uniq <- uniqueID debug
      let child =
            DebugThread
            { dThreadGetId = newThread
            , dThreadUnique = uniq
            , dThreadName = ustr name
            }
      event debug $ DFork loc parent child name
      putMVar mvar child
      return child

-- | Emits a 'Dao.Debug.DThrowTo' signal to the debugger and calls 'Control.Concurrent.throwTo'.
dThrowTo :: (Exception e, Bugged r m) => MLoc -> DThread -> e -> m ()
dThrowTo loc target err = dDebug (throwTo (dThreadGetId target) err) $ \this ->
  DThrowTo loc this target (SomeException err)

-- | Emits a 'Dao.Debug.DThrowTo' signal to the debugger and calls
-- 'Control.Concurrent.killThread'.
dKillThread :: Bugged r m => MLoc -> DThread -> m ()
dKillThread loc target = dDebug (killThread (dThreadGetId target)) $ \this ->
  DThrowTo loc this target (SomeException ThreadKilled)

-- | Behaves just like 'Control.Exception.catch', but makes sure to send a 'DCatch'
-- signal.
dCatch :: (Exception e, Bugged r m) => MLoc -> m a -> (e -> m a) -> m a
dCatch loc tryFunc catchFunc = askDebug >>= \debug -> case debug of
  Nothing    -> do
    r <- askState
    liftIO (handle (\e -> debugUnliftIO (catchFunc e) r) (debugUnliftIO tryFunc r))
  Just debug -> do
    r <- askState
    liftIO $ do
      let this = debugGetThreadId debug
      catch (debugUnliftIO tryFunc r) $ \e -> do
        event debug (DCatch loc this (SomeException e))
        debugUnliftIO (catchFunc e) r

-- | Like 'dCatch' but with it's final two parameters flipped.
dHandle :: (Exception e, Bugged r m) => MLoc -> (e -> m a) -> m a -> m a
dHandle loc catchFunc tryFunc = dCatch loc tryFunc catchFunc

-- | Behaves just like 'Control.Exception.catches', but takes a list of 'Dao.Debug.DHandler's which
-- are converted to 'Control.Exception.Handlers' as necessary.
dCatches :: Bugged r m => m a -> [DHandler r a] -> m a
dCatches tryfn dhands = do
  r <- askState
  debug <- askDebug
  liftIO (catches (debugUnliftIO tryfn r) (map (mkh r debug) dhands)) where
    mkh r debug dhandl = case debug of
      Nothing    -> (getHandler dhandl) r (\_ -> return ())
      Just debug -> (getHandler dhandl) r $ \err -> do
        let this = debugGetThreadId debug
        event debug (DCatch (getHandlerMLoc dhandl) this err)

-- | Same as 'dCatches' but with arguments reversed
dHandles :: Bugged r m => [DHandler r a] -> m a -> m a
dHandles = flip dCatches

-- | Emits a 'DThrow' signal, calls 'Control.Exception.throwIO'.
dThrow :: (Exception e, Bugged r m) => MLoc -> e -> m ignored
dThrow loc err = dDebug (throwIO err) $ \this -> DThrow loc this (SomeException err)

----------------------------------------------------------------------------------------------------

-- | Used to derive the 'dNewChan', and 'dNewMVar' functions.
dMakeVar :: Bugged r m => String -> String -> IO v -> MLoc -> String -> m (DVar v)
dMakeVar typeName funcName newIO loc msg = askDebug >>= \debug -> liftIO $ case debug of
  Nothing    -> fmap DVar newIO
  Just debug -> do
    i    <- uniqueID debug
    var  <- newIO
    let this = debugGetThreadId debug
        dvar = IDVar{dbgVar = var, varID = i, dbgTypeName = ustr typeName, dbgVarLabel = ustr msg}
    event debug (DVarAction loc this (dVarInfo funcName dvar))
    return dvar

-- | Used to derive 'dReadMVar', 'dSwapMVar', and 'dTakeMVar'. Emits a 'DTakeMVar' signal.
dVar :: Bugged r m => (v -> IO a) -> String -> MLoc -> DVar v -> m a
dVar withVar funcName loc var =
  dDebug (withVar (dbgVar var)) $ \this -> DVarAction loc this (dVarInfo funcName var)

----------------------------------------------------------------------------------------------------

-- | Emits a 'DNewChan' signal and calls 'Control.Concurrent.Chan.newChan'.
dNewChan :: Bugged r m => MLoc -> String -> m (DChan v)
dNewChan = dMakeVar "Chan" "newChan" newChan

-- | Emits a 'DWriteChan' signal and calls 'Control.Concurrent.Chan.writeChan'.
dWriteChan :: Bugged r m => MLoc -> DChan v -> v -> m ()
dWriteChan loc var v = dVar (flip writeChan v) "writeChan" loc var

-- | Emits a 'DWriteChan' signal and calls 'Control.Concurrent.Chan.writehan'.
dReadChan :: Bugged r m => MLoc -> DChan v -> m v
dReadChan = dVar readChan "readChan"

----------------------------------------------------------------------------------------------------

-- | Emits a 'DNewQSem' signal and calls 'Control.Concurrent.Chan.newChan'.
dNewQSem :: Bugged r m => MLoc -> String -> Int -> m DQSem
dNewQSem loc msg i = dMakeVar "QSem" ("newQSem "++show i) (Sem.new i) loc msg

-- | Emits a 'DSignqlQSem' signal and calls 'Control.Concurrent.QSem.signalQSem'.
dSignalQSem :: Bugged r m => MLoc -> DQSem -> m ()
dSignalQSem = dVar Sem.signal "signalQSem"

-- | Emits a 'DSignalQSem' signal and calls 'Control.Concurrent.QSem.signalQSem'.
dWaitQSem :: Bugged r m => MLoc -> DQSem -> m ()
dWaitQSem = dVar Sem.wait "waitQSem"

----------------------------------------------------------------------------------------------------

-- | Emits a 'DNewMVar' signal and calls 'Control.Concurrent.MVar.newMVar'.
dNewMVar :: Bugged r m => MLoc -> String -> v -> m (DMVar v)
dNewMVar loc msg v = dMakeVar "MVar" "newMVar" (newMVar v) loc msg

-- | Emits a 'DNewEmptyMVar' signal and calls 'Control.Concurrent.MVar.newEmptyMVar'.
dNewEmptyMVar :: Bugged r m => MLoc -> String -> m (DMVar v)
dNewEmptyMVar = dMakeVar "MVar" "newEmptyMVar" newEmptyMVar

-- | Emits a 'DPutMVar' signal and calls 'Control.Concurrent.MVar.putMVar'.
dPutMVar :: Bugged r m => MLoc -> DMVar v -> v -> m ()
dPutMVar loc var v = dVar (flip putMVar v) "putMVar" loc var

-- | Emits a 'DTakeMVar' signal and calls 'Control.Concurrent.MVar.takeMVar'.
dTakeMVar :: Bugged r m => MLoc -> DMVar v -> m v
dTakeMVar = dVar takeMVar "takeMVar"

-- | Emits a 'DReadMVar' signal and calls 'Control.Concurrent.MVar.readMVar'.
dReadMVar :: Bugged r m => MLoc -> DMVar v -> m v
dReadMVar = dVar readMVar "readMVar"

-- | Emits a 'DReadMVar' signal and calls 'Control.Concurrent.MVar.swapMVar'.
dSwapMVar :: Bugged r m => MLoc -> DMVar v -> v -> m v
dSwapMVar loc var v = dVar (flip swapMVar v) "swapMVar" loc var

-- | Emits a 'DModifyMVar' signal and calls 'Control.Concurrent.MVar.modifyMVar'.
dModifyMVar :: Bugged r m => MLoc -> DMVar v -> (v -> m (v, a)) -> m a
dModifyMVar loc var updFunc = askState >>= \r ->
  dVar (flip modifyMVar (\v -> debugUnliftIO (updFunc v) r)) "modifyMVar" loc var

-- | Emits a 'DModifyMVar' signal and calls 'Control.Concurrent.MVar.modifyMVar_'.
dModifyMVar_ :: Bugged r m => MLoc -> DMVar v -> (v -> m v) -> m ()
dModifyMVar_ loc var updFunc = askState >>= \r ->
  dVar (flip modifyMVar_ (\v -> debugUnliftIO (updFunc v) r)) "modifyMVar" loc var

----------------------------------------------------------------------------------------------------

-- | Looks up information stored in the 'Dao.Debug.DebugRef', generates a report as a single string.
instance Show DEvent where
  show evt = case evt of
    DStarted        loc th msg time -> p loc th $ (show time++(if null msg then "" else ' ':show msg))
    DMsg            loc th      msg -> p loc th $ msg
    DVarAction      loc th      var -> p loc th $ case var of
      DNoInfo  fn          -> uchars fn
      DVarInfo fn i ty lbl -> concat $
        [uchars fn, " (", uchars ty, '#':show i, (if nil==lbl then "" else ' ':uchars lbl), ")"]
    DStdout         loc th      msg -> p loc th $ prin "stdout" msg
    DStderr         loc th      msg -> p loc th $ prin "stderr" msg
    DFork           loc th chld nm  -> p loc th $ "NEW THREAD "++show chld
    DThreadDied     loc th          -> p loc th $ "THREAD COMPLETED"
    DThreadDelay    loc th time     -> p loc th $ "threadDelay "++show time
    DThreadUndelay  loc th time     -> p loc th $ "DONE WAITING "++show time
    DCatch          loc th      (SomeException err) -> p loc th $ "caught exception: "++show err
    DThrow          loc th      (SomeException err) -> p loc th $ "throwIO ("++show err++")"
    DThrowTo        loc th targ (SomeException err) ->
      p loc th ("throwTo "++show targ++" ("++show err++")"++uchars (dThreadName targ))
    DUncaught       loc th      (SomeException err) ->
      p loc th ("THREAD TERMINATED, uncaught exception "++show err)
    DHalt -> "DEBUG HOST debug target thread execution halted."
    where
      here loc = case loc of
        Nothing            -> ""
        Just (fname, a, b) -> fname++':':show a++':':show b++": "
      prin fd msg = fd++' ':
        if not (length msg < 80 && isJust (findIndex (\c -> c=='\n' || c=='\r') msg))
          then concatMap ("\n\t"++) (lines msg)
          else show msg
      p loc th msg = concat [here loc, show th, if null msg then "" else ' ':msg]

