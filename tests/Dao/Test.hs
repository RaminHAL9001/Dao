-- "tests/Dao/Test.hs" tests the Dao program with randomly generated
-- tests, like Haskell quick-check but in a more specialized way.
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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Dao.Test where

import           Dao.Prelude
import           Dao.String
import           Dao.Predicate
import           Dao.PPrint
import           Dao.Parser
import           Dao.Struct
import           Dao.Random
import qualified Dao.Binary        as D
import           Dao.Object
import           Dao.Object.AST
import           Dao.Object.Parser
import           Dao.Object.Binary
import           Dao.Object.PPrint
import           Dao.Object.Struct
import           Dao.Object.Random
import           Dao.Object.DeepSeq

import           Control.Concurrent
import           Control.Exception
import           Control.DeepSeq
import           Control.Monad.Reader
import           Control.Monad.Trans

import           Data.Time.Clock
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.IORef
import qualified Data.Binary          as B
import qualified Data.Binary          as B
import qualified Data.ByteString.Lazy as B

import           Text.Printf

import           System.IO
import           System.Environment

import           Debug.Trace

----------------------------------------------------------------------------------------------------

type TestRun a = ReaderT TestEnv IO a

-- | This data strcture derives 'Prelude.Read' so the test program can be configured from a plain
-- text file. All of these tests randomly generate objects using the "Dao.Object.Random" module, so
-- this module is also being tested, as well as the "Dao.Object.Parser", "Dao.Object.Binary", and
-- "Dao.Object.Struct".
data TestConfig
  = TestConfig
    { doTestParser      :: Bool
      -- ^ Enable testing of the "Dao.Parser","Dao.Object.Parser", "Dao.PPrintM", and
      -- "Dao.Object.PPrintM" modules. The default value is 'Prelude.True'.
    , doTestSerializer  :: Bool
      -- ^ Enable testing of the "Dao.Object.Binary" module. The default value is 'Prelude.True'.
    , doTestStructizer  :: Bool
      -- ^ Enable testing of the "Dao.Struct" and "Dao.Object.Struct" module. The default value is
      -- 'Prelude.True'.
    , doCompareParsed   :: Bool
      -- ^ if the 'doTestParser' parameter is enabled, you may also enable or disable a sub-test
      -- that checks whether or not the object generated and pretty printed by the
      -- "Dao.Object.Random" and "Dao.Object.PPrintM" modules is identical to the object emitted by
      -- the "Dao.Object.Parser". The default value is 'Prelude.True'.
    , maxRecurseDepth   :: Int
      -- ^ The "Dao.Random" module generates objects within objects recursively. This parameter sets
      -- the maximum recursion depth of objects within objects. The functions within "Dao.Random"
      -- will generate only null objects at a certain depth to guarantee the this recursion limit is
      -- not exceeded. The default value is 5.
    , threadCount       :: Int
      -- ^ How many parallel testing threads should run? Set this parameter to the number of CPU
      -- corse available to you on the hardware running the test program. The default value is 1.
      -- Since there is also a display thread, this does not necessarily mean only one thread is
      -- used. If the test program has not been compiled with multi-threading support, there will
      -- always be only 1 thread regardless of the value of this parameter.
    , maxErrors         :: Int
      -- ^ Every test that fails generates a report. If the testing is left unchecked, tests will
      -- continue and reports will be generated until all disk space is consumed. Set this parameter
      -- when you want the test to run for a while and be sure that if there are failed tests, the
      -- testing will halt once that number of failed tests reaches this limit. The default value is
      -- 20.
    , displayInterval   :: Int
      -- ^ When testing is running, a separate thread is run to keep track of how many tests have
      -- run, and will periodically, on an interval of time set by this parameter, report how many
      -- tests have run per second. This value is passed to the 'Control.Concurrent.threadDelay'
      -- function, so, in GHC at least, the value of this parameter is measured in microseconds. For
      -- example, if you want to see the test program report every second, set this value to one
      -- million (1000000). The default value is 4000000 (four seconds).
    , logFileName       :: String
      -- ^ Every test run that fails will have the reason for failure and the object that caused the
      -- failure written to it's own file. However, there may be exceptions thrown for other reasons
      -- not related to the modules being tested, and these failures must be caught and reported as
      -- well. Failures that appear not to be directly related to a failure in the code of the
      -- modules being tested will be caught and a message will be written to a log file, the path
      -- of which is set by this parameter. The defaut value is "./uncaught.log"
    }
  deriving (Show, Read)

defaultTestConfig =
  TestConfig
  { doTestParser     = True
  , doTestSerializer = True
  , doTestStructizer = True
  , doCompareParsed  = False
  , maxRecurseDepth  = 5
  , displayInterval  = 4000000
  , threadCount      = 1
  , maxErrors        = 20
  , logFileName      = "./uncaught.log"
  }

data TestRateInfo
  = TestRateInfo
    { testStartTime :: UTCTime, numberOfTests :: Int
    , lastCheckTime :: UTCTime, lastTestCount :: Int
    }

newTestRateInfo :: IO (MVar TestRateInfo)
newTestRateInfo = do
  start <- getCurrentTime
  newMVar $
    TestRateInfo
    { testStartTime = start, numberOfTests = 0
    , lastCheckTime = start, lastTestCount = 0
    }

incrementRateInfo :: MVar TestRateInfo -> IO ()
incrementRateInfo mvar = modifyMVar_ mvar $ \rateInfo -> return $
  rateInfo
  { numberOfTests = numberOfTests rateInfo + 1
  , lastTestCount = lastTestCount rateInfo + 1
  }

clearRateInfo :: MVar TestRateInfo -> IO ()
clearRateInfo mvar = modifyMVar_ mvar $ \rateInfo -> return $ rateInfo{lastTestCount=0}

setLastCheckTime :: MVar TestRateInfo -> UTCTime -> IO ()
setLastCheckTime mvar time = modifyMVar_ mvar $ \rateInfo -> return $ rateInfo{lastCheckTime=time}

----------------------------------------------------------------------------------------------------

data TestEnv
  = TestEnv
    { testIDFileHandle  :: MVar (Handle, HandlePosn)
    , testPassedButton  :: MVar Bool
    , testRateInfo      :: MVar TestRateInfo
    , errorCounter      :: MVar Int
    , inputChannel      :: MVar (Maybe Int)
    , uncaughtExceptLog :: MVar Handle
    , testConfig        :: TestConfig
    , testCounterLock   :: MVar (Handle, HandlePosn)
    , testMethodTable   :: MethodTable
    }

startAutomatedTesting :: IO ()
startAutomatedTesting = do
  -- testIDs <- getArgs >>= mapM readIO
  config  <- catches (readFile "./test.cfg" >>= readIO) $ errHandler $ \msg -> do
    hPutStrLn stderr ("Warning: "++msg++", creating default configuration")
    hPutStrLn stderr "Warning: setting default configuration"
    catches (writeFile "./test.cfg" (show defaultTestConfig)) $ errHandler $ \msg -> do
      hPutStrLn stderr ("Error: failed to open file \"./test.cfg\" for writing, "++msg)
    return defaultTestConfig
  i <-  catches (readFile "./testid" >>= readIO) $ errHandler $ \msg -> do
          hPutStrLn stderr ("Warning: could not read file \"./testid\", "++show msg)
          return 0
    -- dont bother reading the test ID file if the test IDs have been specified on the command line
  hPutStrLn stderr ("Starting at test ID #"++show i)
  let reportEnabled msg fn =
        hPutStrLn stderr (msg++" testing is "++(if fn config then "enabled" else "disabled"))
  reportEnabled "Parser" doTestParser
  reportEnabled "Serialization" doTestSerializer
  reportEnabled "Structure" doTestStructizer
  let (msg, run) =
        if threadCount config <= 0 then ("single", singleThreaded) else ("multi", multiThreaded)
  hPutStrLn stderr ("running in "++msg++"-threaded mode")
  run config i

----------------------------------------------------------------------------------------------------

newTestEnv :: TestConfig -> IO TestEnv
newTestEnv config = do
  ch       <- newEmptyMVar
  notify   <- newEmptyMVar
  errcount <- newMVar 0
  rateInfo <- newTestRateInfo
  h        <- openFile "./testid" ReadWriteMode
  pos      <- hGetPosn h
  hlock    <- newMVar (h, pos)
  excplog  <- openFile (logFileName config) WriteMode
  excplogVar <- newMVar excplog
  return $
    TestEnv
    { testIDFileHandle  = hlock
    , testPassedButton  = notify
    , testRateInfo      = rateInfo
    , errorCounter      = errcount
    , inputChannel      = ch
    , uncaughtExceptLog = excplogVar
    , testConfig        = config
    , testCounterLock   = hlock
    , testMethodTable   = mempty
    }

closeFiles :: TestEnv -> IO ()
closeFiles env = do
  modifyMVar_ (testCounterLock env) $ \ lock@(h, pos) -> hClose h >> return lock
  modifyMVar_ (uncaughtExceptLog env) $ \h -> hClose h >> return h

multiThreaded :: TestConfig -> Int -> IO ()
multiThreaded config i = do
  env         <- newTestEnv config
  workThreads <- replicateM (threadCount config) $ forkIO (runReaderT testThread env)
  iterThread  <- forkIO $ runReaderT (iterLoop i) env
  dispThread  <- forkIO $ forever $ do
    threadDelay (displayInterval config)
    runReaderT (displayInfo >>= liftIO . putStrLn) env
    clearRateInfo (testRateInfo env)
  catches (runReaderT ctrlLoop env) (errHandlerMessage "exception in main control loop")
  killThread dispThread >> killThread iterThread
  closeFiles env

singleThreaded :: TestConfig -> Int -> IO ()
singleThreaded config i = do
  env <- newTestEnv config
  startTime <- getCurrentTime
  let loop fn ctrl = maybe (return ()) (uncurry fn >=> loop fn) ctrl
  flip runReaderT env $ flip loop (Just (i, i+1)) $ \testID reportWhenID ->
    if testID == reportWhenID
      then do
        -- Display current statistics. Then, based on the rate of tests completed, try to predict
        -- how many tests will be completed in the next N seconds, where N is the 'displayInterval'.
        -- Returning the current testID + the predicted number of tests effectively asks this
        -- function to wait about N seconds before next displaying the statistics.
        info <- displayInfo
        liftIO $ do
          putStrLn info
          now      <- getCurrentTime
          rateInfo <- readMVar (testRateInfo env)
          return $ Just $ (,) testID $ (testID+) $ ceiling $
            toRational (numberOfTests rateInfo) /
              toRational (diffUTCTime now (testStartTime rateInfo))
      else do
        testID    <- signalNextTest (Just testID)
        iterateOK <- testIterate newTestCase checkTestCase
        resultOK  <- getTestResult
        if iterateOK && resultOK
          then return $ testID >>= \testID -> Just (testID, reportWhenID)
          else return Nothing
  closeFiles env

-- Iterate test sending a test ID into the 'inputChannel', then return the next test ID in the
-- iteration (which is just the input integer incremented). The 'testIteration' function will
-- generate a test from  the test ID on the 'inputChannel' and update the result state. The
-- 'getTestResult' function will check the result state decide whether or not testing should
-- continue.
signalNextTest :: Maybe Int -> TestRun (Maybe Int)
signalNextTest i0 = ask >>= \env -> isOn >>= \continue -> liftIO $ do
  let i = i0 >>= guard . (<maxBound) >> i0
  putMVar (inputChannel env) (if continue then i else Nothing)
  return (if continue then fmap (+1) i else Nothing)

-- Loop over 'testIterate' starting at a given integer test ID value and continuing until either
-- there are no more test ID's to be produced, or until the halt condition returned by 'isOn'
-- goes to 'Prelude.False'.
iterLoop :: Int -> TestRun ()
iterLoop i = signalNextTest (Just i) >>= maybe (return ()) iterLoop

-- This function checks the result of a test after 'testIteration' is evaluated and decides whether
-- or not testing should continue. This function does not call 'signalNextTest' (which puts the next
-- test ID on the 'inputChannel') or 'testIterate' (which reads the 'inputChannel', and generates
-- and runs a test).
getTestResult :: TestRun Bool
getTestResult = do
  ask >>= \env -> liftIO $ do
    passed <- takeMVar (testPassedButton env) -- Wait for a result to come back
    incrementRateInfo (testRateInfo env)
    modifyMVar_ (errorCounter env) $ \count -> do
      let nextCount = if not passed then count+1 else count
      return nextCount
  isOn

-- Loop over the 'getTestResult' function. This function is useful in multi-threaded execution to
-- set the main thread into a loop that blocks, repeatedly waits synchronously on a channel for test
-- results until a halting condition is reached, and then returns to the main function.
ctrlLoop :: TestRun ()
ctrlLoop = getTestResult >>= flip when ctrlLoop

displayInfo :: TestRun String
displayInfo = ask >>= \env -> liftIO $ do
  x   <- readMVar (testRateInfo env)
  now <- getCurrentTime
  setLastCheckTime (testRateInfo env) now
  failed <- readMVar (errorCounter env)
  let count   = numberOfTests x
      running = toRational $ diffUTCTime now (testStartTime x)
      days    = floor $ running / (24*60*60) :: Integer
      hours   = floor $ (running - toRational days*24*60*60) / (60*60) :: Int
      mins    = floor $ (running - toRational days*24*60*60 - toRational hours*60*60) / 60 :: Int
      secs    = fromRational $ running - toRational days*24*60*60 - toRational hours*60*60 - toRational mins*60 :: Double
      runAvg  = fromRational $ toRational count / running :: Double
      recent  = lastTestCount x
      dtime   = toRational $ diffUTCTime now (lastCheckTime x)
      instAvg = fromRational $ toRational recent / dtime :: Double
  return $ unlines $ fmap unwords $
    [ ["many_tests_run     =", show (lastTestCount x)]
    , ["failed_tests_count =", show failed]
    , ["total_running_time =", printf "%.4i:%.2i:%.2i:%2.4f" days hours mins secs]
    , ["total_average_tests_per_second  =", printf "%.2f" runAvg ]
    , ["recent_average_tests_per_second =", printf "%.2f" instAvg]
    ]

----------------------------------------------------------------------------------------------------

-- This function reports whether or not testing should continue based on how many tests have failed
-- so far (determined by the 'maxErrors' value in the 'TestConfig'). The test loop iterator
-- 'loopOnInput' calls this function before deciding whether or not to take another test from the
-- 'inputChannel'.
isOn :: TestRun Bool
isOn = ask >>= \env -> liftIO $ do
  count <- readMVar (errorCounter env)
  return (count < maxErrors (testConfig env))

errHandler :: (String -> IO a) -> [Handler a]
errHandler func =
  [Handler(\ (e::IOException) -> func(show e)), Handler(\ (e::ErrorCall) -> func(show e))]

-- Handles errors by outputting the error string to stderr.
errHandlerMessage :: String -> [Handler ()]
errHandlerMessage msg = errHandler (\e -> hPutStrLn stderr (msg++": "++e))

-- Handles errors by returning the error string in a 'Data.Either.Left' constructor. So if this
-- function is the second parameter to 'Control.Exception.catches', the first parameter should
-- return a value wrapped in an 'Data.Either.Either' data type.
errTryCatch :: String -> [Handler (Either UStr a)]
errTryCatch msg = 
  [ Handler (\ (e::IOException) -> return $ Left $ ustr (msg++": "++show e))
  , Handler (\ (e::ErrorCall)   -> return $ Left $ ustr (msg++": "++show e))
  ]

-- If a test case throws an exception rather than simply returning true or false, this is a sign
-- that something unexpected happened. The exception will be caught and treated as a failed test,
-- and as much information as possible will be appended to the main test run log.
catchToLog :: (Handle -> IO ()) -> TestRun ()
catchToLog func = do
  fileName <- asks (logFileName . testConfig)
  logFile  <- asks uncaughtExceptLog
  liftIO $ flip catches (errHandlerMessage ("while writing "++show fileName)) $
    modifyMVar_ logFile $ \h -> do
      catches
        (catches
          (func h >> hFlush h)
          (errHandlerMessage ("while writing "++show fileName))
        )
        (errHandlerMessage "uncaught exception")
      return h

-- To make it easier to halt the test run and resume it later, the current test ID is written to a
-- persistent file. Every time a test case is completed, this file is updated with the ID number.
updateTestIDFile :: Int -> TestRun ()
updateTestIDFile i = ask >>= \env -> liftIO $
  modifyMVar_ (testIDFileHandle env) $ \ (h, pos) -> do
    handle (\ (e::IOException) -> hPrint stderr e) $ do
      hSetPosn pos
      let out = show i
      hPutStrLn    h out
      hFlush       h
      hSetFileSize h (toInteger (length out))
    return (h, pos)

-- This function pulls the next test ID out of the 'inputChannel' of the 'TestEnv', then passes the
-- test ID to a function that generates a test case for that ID, then checks the result of the test
-- case and updates the result state.
testIterate :: Show a => (Int -> TestRun a) -> (a -> TestRun Bool) -> TestRun Bool
testIterate genFunc checkFunc = ask >>= \env -> isOn >>= \continue -> liftIO $ do
  if continue
    then do
      i <- takeMVar (inputChannel env)
      case i of
        Nothing -> return False
        Just  i -> do
          passed <- handle (\e -> runReaderT (h i Nothing e) env) $ do
            obj <- runReaderT (genFunc i) env
            catch (runReaderT (checkFunc obj) env) (\e -> runReaderT (h i (Just (show obj)) e) env)
          putMVar (testPassedButton env) passed -- wait our turn to report success/failure
          runReaderT (updateTestIDFile i) env
          return True
    else return False
  where
    h i obj (SomeException e) = do
      catchToLog $ \logFile -> do
        hPutStrLn logFile ("ITEM #"++show i++"\n"++show e)
        hPutStrLn logFile $ case obj of
          Nothing  -> "(error occurred while generating test object)"
          Just msg -> msg
      return False

-- This function loops over 'testIterate'. This is the function that should be run in its own thread.
loopOnInput :: Show a => (Int -> TestRun a) -> (a -> TestRun Bool) -> TestRun ()
loopOnInput genFunc checkFunc =
  testIterate genFunc checkFunc >>= flip when (loopOnInput genFunc checkFunc)

----------------------------------------------------------------------------------------------------

sep :: String
sep = "--------------------------------------------------------------------------"

-- There are actually two kinds of object tests I need to perform. First is the ordinary parser
-- test, which generates a random, syntactically correct abstract syntax tree in such a way that the
-- pretty-printed AST object will can be parsed back to exactly the same AST object. The second kind
-- of test simply generates a random object, not an AST, and tests if it's pretty-printed form can
-- be parsed without errors. For both types of object, serialization and strcutization are tested.
data RandObj
  = RandTopLevel AST_TopLevel
  | RandObject   Object
  deriving (Eq, Ord, Show)
instance HasLocation RandObj where
  getLocation a     = case a of
    RandTopLevel a -> getLocation a
    RandObject   _ -> LocationUnknown
  setLocation a loc = case a of
    RandTopLevel a -> RandTopLevel (setLocation a loc)
    RandObject   a -> RandObject a
  delLocation a     = case a of
    RandTopLevel a -> RandTopLevel (delLocation a)
    RandObject   a -> RandObject a
instance HasRandGen RandObj where
  randO = do
    i <- nextInt 2
    if i==0 then fmap RandTopLevel randO else fmap RandObject randO
instance PPrintable RandObj where
  pPrint o = case o of
    RandTopLevel o -> pPrint o
    RandObject   o -> pPrint o
instance D.Binary RandObj MethodTable where
  put o = case o of
    RandTopLevel o -> case toInterm o of
      [ ] -> fail "could not convert AST object to intermediate data for serialization"
      [o] -> D.prefixByte 0xFD $ D.put o
      ox  -> fail $ concat $
        [ "converting AST object to intermediate for serialization"
        , " evaluated to multiple ambiguous data structures"
        ]
    RandObject   o -> D.prefixByte 0xFE $ D.put o
  get = D.word8PrefixTable
    <|> fail "Test program failed while trying to de-serialize it's own test data"
instance D.HasPrefixTable RandObj D.Byte MethodTable where
  prefixTable = D.mkPrefixTableWord8 "RandObj" 0xFD 0xFE $
    [ D.get >>= \o -> case fromInterm o of
        []  -> fail "could not convert deserialized intermediate object to AST"
        [o] -> return (RandTopLevel o)
        ox  -> fail $ concat $
          [ "deserializing intermediate object to AST evaluated"
          , " to mulitple ambiguous data structures"
          ]
    , fmap RandObject D.get
    ]
instance Structured RandObj Object where
  dataToStruct o = deconstruct $ case o of
    RandTopLevel o -> putDataAt "ast" o
    RandObject   o -> putDataAt "obj" o
  structToData = reconstruct $ msum
    [ fmap RandTopLevel (tryGetDataAt "ast")
    , fmap RandObject   (tryGetDataAt "obj")
    , fail "Test program failed while trying to structure it's own test data"
    ]
instance NFData RandObj where
  rnf (RandTopLevel a) = deepseq a ()
  rnf (RandObject   a) = deepseq a ()

data TestCase
  = TestCase
    { testCaseID       :: Int
    , originalObject   :: RandObj
    , parseString      :: Maybe UStr
    , serializedObject :: Maybe B.ByteString
    , structuredObject :: Maybe T_tree
    , testResult       :: UStr
    , failedTestCount  :: Int
    }

instance Show TestCase where
  show obj = unlines $ concat $
    [ [uchars (testResult obj)]
    , ["original object:", prettyShow (originalObject obj)]
    , [sep, show (originalObject obj)]
    , maybe mzero (\bin -> [sep, show (Base16String bin)]) (serializedObject obj)
    ]

-- If the 'failedTestCount' is greater than 0, then this 'TestCase' is printed to a file, where the
-- name of the file is the @'Prelude.show ('testCaseID' tc) ++ ".log"@. Returns 'Prelude.False' for
-- failure, 'Prelude.True' for passed tests.
reportTest :: TestCase -> IO Bool
reportTest tc =
  if failedTestCount tc > 0
    then do
      let fileName = show (testCaseID tc) ++ ".log"
      writeFile fileName (show tc)
      return False
    else return True

-- Generate a test case, which involves using functions in the "Dao.Object.Random" module to create
-- a random object, and then possibly transforming the random object into forms suitable for testing
-- various algorithms, for example generating a serialized binary representation of the random
-- object if serialization is to be tested. This function checks the 'TestConfig' to decide which
-- objects to generate, for example, if 'doTestSerializer' is set to 'Prelude.True', then the binary
-- serialization of the random object is also generated and stored in this test case.  This function
-- is used as a parameter to 'loopOnInput'.
newTestCase :: Int -> TestRun TestCase
newTestCase i = do
  mtab <- asks testMethodTable
  cfg  <- asks testConfig
  let maxDepth      = maxRecurseDepth cfg
  let obj           = genRandWith randO maxDepth i
  let str           = prettyShow obj
  let bin           = D.encode mtab obj
  let tree          = dataToStruct obj
  let setup isSet o = if isSet cfg then Just o else Nothing
  return $
    TestCase
    { testCaseID       = i
    , originalObject   = obj
    , parseString      = setup doTestParser (ustr str)
    , serializedObject = setup doTestSerializer bin
    , structuredObject = setup doTestStructizer tree
    , testResult       = nil
    , failedTestCount  = 0
    }

-- Use 'passTest' or 'failTest' instead.
updateTestCase :: TestCase -> Maybe String -> TestRun TestCase
updateTestCase tc msg = return $
  tc{ testResult = let tr = testResult tc in maybe tr (tr<>) (fmap ustr msg)
    , failedTestCount = (if maybe False (const True) msg then (+1) else id) $ failedTestCount tc
    }

-- Every 'TestCase' stores information as to whether or not the case has passed or failed. Use this
-- function to update the 'TestCase' with information indicating that the test passed.
passTest :: TestCase -> TestRun TestCase
passTest tc = updateTestCase tc Nothing

-- Every 'TestCase' stores information as to whether or not the case has passed or failed. Use this
-- function to update the 'TestCase' with information indicating that the test failed, including a
-- failure message string.
failTest :: TestCase -> String -> TestRun TestCase
failTest tc = updateTestCase tc . Just

-- Every test is wrapped in this error handler (which calls to 'errHandler' above), and the test is
-- considered a failure if an 'Control.Exception.Error' or 'Control.Exception.IOException' is
-- caught. Pass a test case, a selector for selecting the object generated (Maybe) by 'newTestCase',
-- and a function for checking the value of that generated object. If the object was not generated,
-- the 'TestCase' is returned unmodified. If an exception is thrown, the test case is returned with
-- a failure indicated.
tryTest :: TestCase -> (TestCase -> Maybe item) -> (item -> TestRun TestCase) -> TestRun TestCase
tryTest tc getTestItem testFunc = ask >>= \env -> case getTestItem tc of
  Nothing   -> return tc
  Just item -> liftIO $ catches (runReaderT (testFunc item) env) $
    errHandler (flip runReaderT env . failTest tc)

-- Test the pretty printer and the parser. If a randomly generated object can be pretty printed, and
-- the parser can parse the pretty-printed string and create the exact same object, then the test
-- pases. This function is used as a parameter to 'loopOnInput'.
checkTestCase :: TestCase -> TestRun Bool
checkTestCase tc = ask >>= \env -> do
  mtab <- asks testMethodTable
  ------------------- (0) Canonicalize the original object ------------------
  tc <- case originalObject tc of
    RandTopLevel o -> case canonicalize o of
      [o] -> return $ (tc{originalObject = RandTopLevel o})
      [ ] -> error "could not canonicalize original object"
      ox  -> error "original object canonicalized to multiple possible values"
    RandObject   _ -> return tc
  --
  --------------------------- (1) Test the parser ---------------------------
  tc <- tryTest tc parseString $ \str -> case originalObject tc of
    RandObject   orig -> case parse (daoGrammar{mainParser=equation}) mempty (uchars str) of
      -- For ordinary Objects, we only test if the pretty-printed code can be parsed.
      Backtrack -> failTest tc "Parser backtrackd"
      PFail   b -> failTest tc (show b)
      OK      _ -> return tc
    RandTopLevel orig -> case parse daoGrammar mempty (uchars str) of
      -- For AST objects, we also test if the data structure parsed is identical to the data
      -- structure that was pretty-printed.
      Backtrack -> failTest tc "Parser backtrackd"
      PFail   b -> failTest tc (show b)
      OK      o -> do
        let ~diro  = do -- clean up the parsed input a bit
              o <- directives o
              case o of
                AST_TopComment [] -> []
                o                 -> [delLocation o]
        let ~match = diro == [orig]
        if not (doCompareParsed (testConfig env)) || match
          then  passTest tc
          else  
            failTest tc $ unlines $ concat
              [ ["Parsed AST does not match original object, parsed AST is:"]
              , case diro of
                  []  -> ["(Empty AST)"]
                  [o] -> [prettyShow o, show (RandTopLevel o)]
                  ox  -> concat $
                    [ ["(Returned multiple AST items)"]
                    , zip [1..] ox >>= \ (i, o) ->
                        ["Parsed item #"++show i++":", prettyShow o, prettyShow o, sep]
                    ]
              ]
  --
  ------------------------- (2) Test the serializer -------------------------
  tc <- tryTest tc serializedObject $ \binObj -> do
    unbin <- liftIO $ catches (return $ Right $ D.decode mtab binObj) $
      errTryCatch "binary deserialization failed" :: TestRun (Either UStr RandObj)
    case unbin of
      Left  msg   -> failTest tc (uchars msg) :: TestRun TestCase
      Right unbin ->
        if unbin == originalObject tc
          then  passTest tc
          else  failTest tc $ unlines $
                  [ "Original object does not match object deserialized from binary string"
                  , "Received bytes:", showBinary binObj
                  , "Deserialied object:"
                  , prettyShow unbin
                  ]
  --
  ---------------- (3) Test the intermediate tree structures ----------------
  tc <- tryTest tc structuredObject $ \obj ->
    case structToData obj :: PValue UpdateErr RandObj of
      Backtrack -> failTest tc $
        "Backtracked while constructing a Haskell object from a Dao tree"
      PFail err -> failTest tc $ unlines [ show err, prettyShow obj]
      OK struct ->
        if originalObject tc == struct
          then
            failTest tc $ unlines $
              [ "Original object does not match Haskell object constructed from it's Dao tree"
              , prettyShow struct
              ]
          else  passTest tc
  --
  ------------------------------ Report results -----------------------------
  liftIO (reportTest tc)

----------------------------------------------------------------------------------------------------

-- This is the computation that runs a series of tests, looping over the 'inputChannel'.
testThread :: TestRun ()
testThread = loopOnInput newTestCase checkTestCase

