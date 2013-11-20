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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dao.Test where

import           Dao.String
import           Dao.Predicate

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Concurrent.SSem as Sem
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Error

import           Data.Typeable
import           Data.Dynamic
import           Data.Time.Clock
import           Data.Maybe
import           Data.Char (isSpace)
import           Data.List
import           Data.Monoid
import           Data.IORef
import qualified Data.Binary          as B
import qualified Data.ByteString.Lazy as B

import           Text.Printf -- used for printing clocks

import           System.IO
import           System.Environment

sep :: String
sep = "--------------------------------------------------------------------------"

maxCPUCores :: Int
maxCPUCores = 16

main :: GUnitTester configuration environment statistics testCase testResult -> IO ()
main tester = do
  config <- configFromPath tester
  args   <- getArgs
  withNewTestEnv tester config $ ask >>= \env -> liftIO $ do
    if null args
    then do -- run starting from the test number written in the "testid" file.
      i <- testInIO env tryGetSavedTestID
      hPutStrLn stderr ("Starting at test ID #"++show i)
      let (msg, run) =
            if threadCount config <= 1
            then ("single", singleThreaded)
            else ("multi" , multiThreaded )
      hPutStrLn stderr ("Running in "++msg++"-threaded mode.")
      clock <- if displayInterval config > 0 then Just <$> startClockThread env else return Nothing
      testInIO env (run i)
      maybe (return ()) killThread clock
    else case args of
      "-t"          :args -> testInIO env (showTestMode args)
      "--show-tests":args -> testInIO env $ showTestMode args
      ["-"] -> testInIO env stdinMode
      args  -> do -- parse all command line arguments as a test IDs and then run each ID.
        putStrLn "Running tests specified on command line."
        ix <- forM args $ \arg -> case readsPrec 0 arg of
          [(i, "")] -> return i
          _         -> fail $ unwords $
            ["(ERROR) Command line argument:", show arg, "is cannot be used as an integer value."]
        (evaluate $! deepseq ix ix) >>= mapM_ (testInIO env . runSingleTest id)

----------------------------------------------------------------------------------------------------

-- | This is the monad used to control the whole test program.
newtype GTest c e s t r a = Test{ testToReaderT :: ReaderT (GTestEnv c e s t r) IO a }
  deriving (Functor, Monad, Applicative, MonadIO)

instance MonadReader (GTestEnv c e s t r) (GTest c e s t r) where
  ask                   = Test ask
  local f (Test reader) = Test (local f reader)

-- | This is a monad lifting the 'GTest' monad which instantiates
-- 'Control.Monad.State.Class.MonadState' class with the @result@ data type as the state which can
-- be updated. This monad is used to create test results. To signal that the test has failed,
-- evaluate 'Control.Monad.mzero'. To signal that the test has passed, evaluate to
-- @'Control.Monad.return.' ()@. Exceptions of the type 'Control.Exception.IOException' and
-- 'Control.Exception.ErrorCall' are caught and stored in the monad as a 'Dao.Predicate.PFail' value
-- which can be handled without a 'Control.Exception.catch' or similar statement.
newtype GResultM c e s t r a = ResultM { resultMtoPTrans :: PTrans (String, SomeException) (StateT r (GTest c e s t r)) a }
instance Functor (GResultM c e s t r) where { fmap f (ResultM m) = ResultM (fmap f m) }
instance Monad (GResultM c e s t r) where
  ResultM f >>= to = ResultM (f >>= resultMtoPTrans . to)
  return = ResultM . return
  fail msg = ResultM (pvalue $ PFail ("", SomeException $ ErrorCall msg))
  -- 'fail' is overridden to gently place an error into the monad, it never throws the error.
instance MonadPlus (GResultM c e s t r) where
  mzero = ResultM mzero
  mplus (ResultM a) (ResultM b) = ResultM (mplus a b)
instance Applicative (GResultM c e s t r) where { pure = return; (<*>) = ap; }
instance MonadState r (GResultM c e s t r) where { state f = ResultM (lift (state f)) }
instance MonadError (String, SomeException) (GResultM c e s t r) where
  throwError e = ResultM (pvalue $ PFail e)
  catchError f catch = ResultM $ catchError (resultMtoPTrans f) (resultMtoPTrans . catch)
instance MonadIO (GResultM c e s t r) where { liftIO = ResultM . liftIO }

runResultM :: GResultM c e s t r a -> r -> GTest c e s t r (PValue (String, SomeException) a, r)
runResultM f = runStateT (runPTrans (resultMtoPTrans f))

resultM :: (r -> GTest c e s t r (PValue (String, SomeException) a, r)) -> GResultM c e s t r a
resultM = ResultM . PTrans . StateT

testInResultM :: GTest c e s t r a -> GResultM c e s t r a
testInResultM f = resultM (\r -> f >>= \a -> return (OK a, r))

-- | This object contains the functions your test unit will need in order for the test environment
-- to make use of them.
-- @config@ = /configuration/ data: instantiation 'Prelude.Read' and 'Prelude.Show' so it can be read
--            and written from a flat configuration file.
-- @env@    = the /environment/: the read-only parameters established after reading the configuration
--            but do not need to be stored in the configuration file. If everything can be stored in
--            the configuration file, don't bother defining a separate environment data type, use
--            @()@ for this type.
-- @stats@  = /statistics/: records statistics about the entire test run. @()@ is an acceptable
--            type to use for @stats@.
-- @test@   = the /test case/ data type created from an 'Prelude.Integer', usually by way of a
--            pseudo-random algorithm for generating arbitrary data types.
-- @result@ = the /test results/ data type, which must report whether a test passed or failed, and
--            can also contain statistical information about individual test runs.
data GUnitTester config env stats test result
  = UnitTester
    { configFilePath :: FilePath
      -- ^ The file path of the configuration file for this test suite.
    , showConfig     :: GTestConfig config -> String
      -- ^ It is best to just set this to 'Prelude.show'. The unit-specific configuration @config@
      -- should derive 'Prelude.Show' so it can be stored within the 'TestConfig' data structure and
      -- written to flat files in the file system.
    , readConfig     :: String       -> IO (GTestConfig config)
      -- ^ It is best to just set this to 'Prelude.readIO'. The unit-specific configuration @config@
      -- should derive 'Prelude.Read' so it can be stored within the 'TestConfig' data structure and
      -- read from flat files in the file system.
    , checkConfig    :: Maybe config -> IO config
      -- ^ This is a function evaluated after reading the configuration file stored at
      -- 'configFilePath'. The unit-specific data structure @config@ is given to this function to
      -- check whether or not it is valid. If it is not valid, throw an exception or evaluate
      -- 'Preulde.error'. This configuration can be retrieved at any time during testing with the
      -- asksUnitConfig function.
    , newEnvironment :: GTestConfig config -> IO env
      -- ^ This is a function evaluated after 'checkConfig' but before testing begins. The check
      -- @config@ is passed to this function to allow you to setup a unit-specific environment data
      -- structure. This environment may be an 'Control.Concurrent.MVar.MVar'. If not, the
      -- environment is read-only. The environment can be retrieve at any time during testing with
      -- the 'asksUnitEnv' function.
    , newStatistics  :: GTestConfig config -> IO stats
      -- ^ This function is evaluated after 'checkConfig' and 'newEnvironment' but before testing
      -- begins. The @config@ is passed to this function to allow you to setup a unit-specific
      -- statistic data type. This data type will be updated after every test evaluation using
      -- the 'combineStats' function below.
    , showStatistics :: stats -> String
      -- ^ This function may well just be set to 'Prelude.show', this function will be called
      -- periodically, depending on the 'displayInterval' setting in the 'TestConfig'. Every time a
      -- periodic report is generated, this function is called and the resulting string is appended
      -- to the report.
    , generateTest   :: Integer -> GTest    config env stats test result test
      -- ^ This is the function that should generate a @test@ data structure from an
      -- 'Prelude.Integer'. The best way to do this is by instantiating your @test@ data type into
      -- the 'Dao.Random.HasRandGen' class and returning the result of an evaluation of
      -- 'Dao.Random.genRandWith' with the given 'Prelude.Integer'.
    , showTest       :: Maybe (test -> String)
      -- ^ If you ever want to actually display the contents of a test object, define this function.
    , newResult      :: GTest config env stats test result result
      -- ^ This is the function that should create a new blank test result data structure. The
      -- @result@ data should contain useful information about the test run.
    , evaluateTest   :: test    -> GResultM config env stats test result ()
      -- ^ This is the function that actually performs the test. Given a @test@ data structure, a
      -- @result@ data structure is created in the 'GResultM' monad. 'GResultM' instantiates
      -- 'Control.Monad.State.Class.MonadState' such that 'Control.Monad.State.get' and
      -- 'Control.Monad.State.put' will get/put the @result@ data type statefully. The 'GResultM'
      -- monad also lifts the 'GTest' monad so you can still evaluate the 'asksUnitEnv',
      -- 'asksTestConfig', 'asksUnitConfig', and other useful function. You can lift any 'GTest'
      -- function using the 'testInResultM'. To signify a test has passed, evaluate
      -- 'Control.Monad.mzero', 'Control.Monad.fail', or 'Control.Monad.Error.throwError'. You can
      -- also throw an exception using 'Control.Exception.throw' or 'Prelude.error', and these will
      -- safely be caught, but probably it is better not to throw an exception to fail a test
      -- because it is more difficult to control exception handling when generating reports. To pass
      -- a test, simply evaluate to @return ()@
    , showResult     :: result  -> GTest    config env stats test result String
      -- ^ When a test is completed i.e. when the 'evaluateTest' function returns, the result of the
      -- evaluation is checked. If an exception was thrown or one of 'Control.Monad.mzero',
      -- 'Control.Monad.fail', or 'Control.Monad.Error.throwError' was evaluated, then this function
      -- is called to convert the @result@ data structure to a report string that will be written to
      -- a report file. The name of the report file will be the 'Preulde.Integer' of the test that
      -- was used to generate the @test@ that was evaluated with the string ".log" appended to it.
    , combineStats   :: result  -> stats -> GTest config env stats test result stats
      -- Whenever a test is completed the @result@ data structure and the current @stats@ data
      -- structure are used to evaluate this function. This function should update the @stats@ data
      -- structure with statistical information about the tests, for example, execution time data.
      -- The number of tests completed thus far, the number of tests failed, and rate of test
      -- completion in tests-per-second are automatically tracked, so your @stats@ data structure
      -- does not need to contain any data related to this.
    }

data GTestEnv c e s t r
  = TestEnv
    { testIDFileHandle  :: Maybe (MVar Handle)
    , testRateInfo      :: MVar TestRateInfo
    , errorCounter      :: MVar Int
    , uncaughtExceptLog :: MVar Handle
    , unitTestHandle    :: GUnitTester c e s t r
    , unitEnvironment   :: e
    , testStatistics    :: MVar s
    , testConfig        :: GTestConfig c
    , currentTestID     :: Maybe Integer
    }

newTestEnv :: GUnitTester c e s t r -> GTestConfig c -> IO (GTestEnv c e s t r)
newTestEnv tester config = do
  env        <- newEnvironment tester config
  stats      <- newStatistics tester config >>= newMVar
  rateInfo   <- newTestRateInfo
  errcount   <- newMVar 0
  excplog    <- openFile (logFileName config) WriteMode
  excplogVar <- newMVar excplog
  let path = savedTestIDPath config
  hlock <- case path of
    ""   -> return Nothing
    path -> catches (fmap Just $ openFile path ReadWriteMode >>= newMVar) $ errHandler $ \err -> do
      hPutStr stderr $ unlines $
        [ "(WARNING) Could not open "++show path, '\t':err
        , "\tWill run without tracking the latest test ID."
        ]
      return Nothing
  return $
    TestEnv
    { testIDFileHandle  = hlock
    , testRateInfo      = rateInfo
    , errorCounter      = errcount
    , uncaughtExceptLog = excplogVar
    , unitTestHandle    = tester
    , unitEnvironment   = env
    , testStatistics    = stats
    , testConfig        = config
    , currentTestID     = Nothing
    }

withNewTestEnv :: GUnitTester c e s t r -> GTestConfig c -> GTest c e s t r a -> IO a
withNewTestEnv tester config run = newTestEnv tester config >>= flip testInIO run

-- | Similar to 'Control.Monad.Reader.runReaderT', but the 'GTestEnv' environment is the first
-- parameter. Allows you to evaluate a 'GTest' monad in the @IO@ monad.
testInIO :: GTestEnv c e s t r -> GTest c e s t r a -> IO a
testInIO env test = runReaderT (testToReaderT test) env

----------------------------------------------------------------------------------------------------

-- | This data strcture derives 'Prelude.Read' so the test program can be configured from a plain
-- text file. All of these tests randomly generate objects using the "Dao.Object.Random" module, so
-- this module is also being tested, as well as the "Dao.Object.Parser", "Dao.Object.Binary", and
-- "Dao.Object.Struct".
data GTestConfig configuration
  = TestConfig
    { threadCount       :: Int
      -- ^ How many parallel testing threads should run? Set this parameter to the number of CPU
      -- corse available to you on the hardware running the test program. The default value is 1.
      -- Since there is also a display thread, this does not necessarily mean only one thread is
      -- used. If the test program has not been compiled with multi-threading support, there will
      -- always be only 1 thread regardless of the value of this parameter.
    , maxErrors         :: Int
      -- ^ Every test that fails generates a report. If the testing is left unchecked, tests will
      -- continue and reports will be generated until all disk space is consumed. Set this parameter
      -- when you want the test to run for a while and be sure that if there are failed tests, the
      -- testing will halt once that number of failed tests reaches this limit. When running in
      -- multi-threaded mode, you may end up with a few more failed test reports than you specify
      -- here. For example, lets say you have eight CPU cores and you specify a maximum of ten
      -- failed tests. If at some point there are 9 failed tests, then simultaneously all eight
      -- cores evaluate a failed test, all eight cores will write their reports simultaneously
      -- before checking whether or not the maximum error count has been reached. This will give you
      -- 17 failed test reports, or (maxErrors + threadCount - 1) tests in the worst case. I assume
      -- you do not mind a few additional error reports in exchange for having very fast and fully
      -- autonomous testing threads. The default maxErrs value is 20.
    , displayInterval   :: Int
      -- ^ When testing is running, a separate thread is run to keep track of how many tests have
      -- run, and will periodically, on an interval of time set by this parameter, report how many
      -- tests have run per second. This value is passed to the 'Control.Concurrent.threadDelay'
      -- function, so, in GHC at least, the value of this parameter is measured in microseconds. For
      -- example, if you want to see the test program report every second, set this value to one
      -- million (1000000). The default value is 4000000 (four seconds). Set this value to zero if
      -- you want no reports at all.
    , logFileName       :: String
      -- ^ Every test run that fails will have the reason for failure and the object that caused the
      -- failure written to it's own file. However, there may be exceptions thrown for other reasons
      -- not related to the modules being tested, and these failures must be caught and reported as
      -- well. Failures that appear not to be directly related to a failure in the code of the
      -- modules being tested will be caught and a message will be written to a log file, the path
      -- of which is set by this parameter. The defaut value is "./uncaught.log"
    , savedTestIDPath   :: String
      -- ^If you need to interrupt testing, you can simply signal the executing test program with
      -- the POSIX signal SIGINT (usually done from a command line console by pressing the Control+C
      -- keys on the keyboard). When this happens, you may want to save the current test ID number
      -- that is being evaluated so you can continue from that ID number the next time you launch
      -- the test program. The default file name to save the test ID number is "./testid".
    , unitTestConfig    :: configuration
      -- ^ This is the test configuration data specific to a particular unit test program.
    }
  deriving (Show, Read)

defaultTestConfig :: config -> GTestConfig config
defaultTestConfig config =
  TestConfig
  { threadCount      = 1
  , maxErrors        = 20
  , displayInterval  = 4000000
  , logFileName      = "./uncaught.log"
  , savedTestIDPath  = "./testid"
  , unitTestConfig   = config
  }

errHandler :: (String -> IO a) -> [Handler a]
errHandler func =
  [Handler(\ (e::IOException) -> func(show e)), Handler(\ (e::ErrorCall) -> func(show e))]

configFromPath :: GUnitTester c e s t r -> IO (GTestConfig c)
configFromPath handle = do
  let path    = configFilePath handle
  let showCfg = showConfig handle
  let readIO  = readConfig handle
  let next msg a fn = flip (maybe (return Nothing)) a $ \a ->
        catches (Just <$> fn a) $ errHandler $ \err -> do
          hPutStrLn stderr $ concat ["(WARNING) Could not ", msg, " ", show path, "\n\t", err]
          return Nothing
  str  <- next "open configuration file" (Just()) $ \ () ->
    readFile path >>= fmap toUStr . evaluate >>= \cfg -> return $! deepseq cfg cfg
  cfg  <- next "read configuration file" str $ \str -> readIO (uchars str) >>= evaluate
  unit <- checkConfig handle (fmap unitTestConfig cfg)
    -- for 'checkConfig' exceptions are not caught, the unit-specific configuration is essential.
  case cfg of
    Just cfg -> return $ cfg{unitTestConfig=unit}
    Nothing  -> do
      let cfg = defaultTestConfig unit
      unless (maybe False (const True) str) $ do -- Create a config file if it does not exist.
        hPutStrLn stderr ("(WARNING) Saving default configuration to file "++show path)
        next "save default configuration" (Just()) $ \ () -> writeFile path (showConfig handle cfg)
        return ()
      return cfg

-- | Read the file containing the latest testID to have been run.
tryGetSavedTestID :: GTest c e s t r Integer
tryGetSavedTestID = ask >>= \env -> liftIO $ case testIDFileHandle env of
  Nothing   -> return 0
  Just mvar -> modifyMVar mvar $ \file -> do
    empty <- hIsEOF file
    i     <-
      if empty
      then return 0
      else do
        str <- hGetLine file
        if null str
        then return 0
        else
          catches (readIO str) $ errHandler $ \ _ -> do
            let path = savedTestIDPath (testConfig env)
            hPutStr stderr $ unwords $
              [ "(WARNING) The file", show path
              , "does not seem to contain a valid test ID."
              , "The file will be overwritten."
              ]
            return 0
    return (file, i)

trySaveTestID :: Integer -> GTest c e s t r ()
trySaveTestID i = do
  mvar <- asks testIDFileHandle
  case mvar of
    Nothing   -> return ()
    Just mvar -> liftIO $ modifyMVar_ mvar $ \file -> do
      hSeek file AbsoluteSeek 0 >> hSetFileSize file 0
      hPutStrLn file (show i) >> hFlush file
      return file

----------------------------------------------------------------------------------------------------

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

incrementRateInfo :: TestRateInfo -> IO TestRateInfo
incrementRateInfo rateInfo = return $
  rateInfo
  { numberOfTests = numberOfTests rateInfo + 1
  , lastTestCount = lastTestCount rateInfo + 1
  }

clearRateInfo :: TestRateInfo -> IO TestRateInfo
clearRateInfo rateInfo = return $ rateInfo{lastTestCount=0}

setLastCheckTime :: UTCTime -> TestRateInfo -> IO TestRateInfo
setLastCheckTime time rateInfo = return $ rateInfo{lastCheckTime=time}

-- | A clock thread can always be started, even in single-threaded mode, even if the program is not
-- compiled with the GHC @-threaded@ option the 'Control.Concurrent.forkIO' can still be used to
-- create timers, and that is how it is used here. When GHC @-threaded@ is used, and the test
-- program sets up a many threads as there are cores on the CPU running it, the timer often becomes
-- often an extra "pigeon" that has to share a nest (CPU core) with another therad. The timer can
-- become somewhat starved but the GHC runtime implementation does well enough to make sure timers
-- do respond eventually even when all CPU cores are extremely busy. You could specify in the
-- configuration file to use one less core than the CPU has available, but in my opinion it is a
-- waste to devote a whole CPU core to a timer which will only fire once every some seconds, it is
-- better to just fork the timer thread even if it means the test program will have one more thread
-- running than CPU cores available.
startClockThread :: GTestEnv c e s t r -> IO ThreadId
startClockThread env = forkIO $ forever $ do
  threadDelay (displayInterval $ testConfig env)
  testInIO env displayInfo >>= putStrLn
  modifyMVar_ (testRateInfo env) clearRateInfo

updateClock :: GTest c e s t r ()
updateClock = modTestEnvIO_ testRateInfo incrementRateInfo

displayInfo :: GTest c e s t r String
displayInfo = ask >>= \env -> liftIO $ do
  x   <- readMVar (testRateInfo env)
  now <- getCurrentTime
  modifyMVar_ (testRateInfo env) (clearRateInfo >=> setLastCheckTime now)
  stats  <- testInIO env (unit showStatistics <*> (asks testStatistics >>= liftIO . readMVar))
  failed <- testInIO env (asks errorCounter) >>= readMVar
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
    [ ["many_tests_run     =", show count]
    , ["failed_tests_count =", show failed]
    , ["total_running_time =", printf "%.4i:%.2i:%.2i:%2.4f" days hours mins secs]
    , ["tests_since_last_report         =", show recent]
    , ["total_average_tests_per_second  =", printf "%.2f" runAvg ]
    , ["recent_average_tests_per_second =", printf "%.2f" instAvg]
    , if null stats then [] else [stats]
    ]

----------------------------------------------------------------------------------------------------

-- | Get an element from the 'GTestConfig' data structure. Use @'asksTestConig' 'Prelude.id'@ to
-- return the whole 'GTestConfig' data structure.
asksTestConfig :: (GTestConfig c -> a) -> GTest c e s t r a
asksTestConfig f = asks (f . testConfig)

-- | Get an element from the unit-specific configuration data structure. Use @'asksUnitConig'
-- 'Prelude.id'@ to return the whole data structure.
asksUnitConfig :: (c -> a) -> GTest c e s t r a
asksUnitConfig f = asksTestConfig (f . unitTestConfig)

-- | Get an element from the unit-specific environment data structure. Use @'asksUnitEnv'
-- 'Prelude.id'@ to return the whole 'GTestConfig' data structure.
asksUnitEnv :: (e -> a) -> GTest c e s t r a
asksUnitEnv f = asks (f . unitEnvironment)

-- | Evaluate a callback function in the 'GUnitTester' data type that is not in the @IO@ or 'GTest'
-- monad.
unit :: (GUnitTester c e s t r -> a) -> GTest c e s t r a
unit f = asks (f . unitTestHandle)

-- | Evaluate a callback function in the 'GUnitTester' data type in the @IO@ or 'GTest' monad.
unitIO :: (GUnitTester c e s t r -> IO a) -> GTest c e s t r a
unitIO f = asks (f . unitTestHandle) >>= liftIO

-- | Evaluate a callback function in the 'GUnitTester' in the 'GTest' monad.
unitTest :: (GUnitTester c e s t r -> a -> GTest c e s t r b) -> a -> GTest c e s t r b
unitTest f a = asks (f . unitTestHandle) >>= \func -> func a

-- | Evaluate the 'evaluateTest' function in the 'GUnitTester' function. Since 'evaluateTest' is in
-- the 'GResultM' monad, the value returned contains information about whether or not the test
-- passed or failed within a 'Dao.Predicate.PValue' data type.
unitResult :: t -> GTest c e s t r (PValue (String, SomeException) (), r)
unitResult test = do
  run    <- unit evaluateTest
  result <- join $ unit newResult
  runResultM (run test) result

-- | Many of the values in the 'GTestEnv' data structuer are 'Control.Concurrent.MVar.MVar' data
-- types. This function makes it a bit easier to to specify which 'Control.Concurrent.MVar.MVar' you
-- wish to access, and then update value in the 'GTest' monad using a @IO@ monadic updating
-- function.
modTestEnvIO :: (GTestEnv c e s t r -> MVar a) -> (a -> IO (a, b)) -> GTest c e s t r b
modTestEnvIO f modf = asks f >>= liftIO . flip modifyMVar modf

-- | Many of the values in the 'GTestEnv' data structuer are 'Control.Concurrent.MVar.MVar' data
-- types. This function makes it a bit easier to to specify which 'Control.Concurrent.MVar.MVar' you
-- wish to access, and then update value in the 'GTest' monad using a @IO@ monadic updating
-- function.
modTestEnvIO_ :: (GTestEnv c e s t r -> MVar a) -> (a -> IO a) -> GTest c e s t r ()
modTestEnvIO_ f modf = modTestEnvIO f (\a -> modf a >>= \a -> return (a, ()))

-- | Many of the values in the 'GTestEnv' data structuer are 'Control.Concurrent.MVar.MVar' data
-- types. This function makes it a bit easier to to specify which 'Control.Concurrent.MVar.MVar' you
-- wish to access, and then update value in the 'GTest' monad using a pure function which can also
-- return a specific value.
modTestEnv :: (GTestEnv c e s t r -> MVar a) -> (a -> (a, b)) -> GTest c e s t r b
modTestEnv f modf = modTestEnvIO f (return . modf)

-- | Many of the values in the 'GTestEnv' data structuer are 'Control.Concurrent.MVar.MVar' data
-- types. This function makes it a bit easier to to specify which 'Control.Concurrent.MVar.MVar' you
-- wish to access, and then update value in the 'GTest' monad using a pure function.
modTestEnv_ :: (GTestEnv c e s t r -> MVar a) -> (a -> a) -> GTest c e s t r ()
modTestEnv_ f modf = modTestEnvIO_ f (return . modf)

data NonAsyncException = NonAsyncException String Dynamic deriving Typeable
instance Show NonAsyncException where { show (NonAsyncException msg _) = msg }
instance Exception NonAsyncException

-- | Use this function to write a message to the uncaught-exceptions error log file.
uncaught :: String -> String -> GTest c e s t r ()
uncaught msg err = ask >>= \env -> liftIO $
  modifyMVar_ (uncaughtExceptLog env) $ \h -> do
    hPutStr h $ unlines $
      [ concat $
          [ "Uncaught exception"
          , maybe "" (\i -> ", test case #"++show i) (currentTestID env)
          , " while ", msg
          ]
      , err, sep
      ]
    hFlush h >> return h

-- | This function, or some function which uses this function like 'catchToLog', is the best way to
-- catch exceptions. You provide three parameters: First is a message to report when an exception occurs, it
-- is best to indicate what function you were trying to evaluate inside of the 'catchToPValue'
-- function. Second is a function to evaluate when a 'Control.Exception.UserInterrupt' exception is
-- caught. This function should shut down gracefully without reporting any error messages. Third is
-- the function to run which might throw an exception. Exceptions handled are
-- 'Control.Exception.IOException', 'Control.Exception.ErrorCall', and
-- 'Control.Exception.AsyncException' but only the 'Control.Exception.UserInterrupt' exception, all
-- other exceptions of the 'Control.Exception.AsyncException' type are re-thrown.
-- 
-- I define a special exception handler that catches every execption except for those of
-- the type 'Control.Exception.AsyncException'.
catchToPValue :: String -> GTest c e s t r a -> GTest c e s t r (PValue (String, SomeException) a)
catchToPValue msg try = ask >>= \env -> liftIO $ do
  let catchAll (SomeException e) = throwIO (NonAsyncException (show e) (toDyn e))
  let rethrow (NonAsyncException err dyn) =
        maybe (testInIO env (uncaught msg err) >> return Backtrack)
              (\e -> throwIO (e::AsyncException))
              (fromDynamic dyn)
  handle rethrow $ handle catchAll $ fmap OK $ testInIO env try

-- | Like 'catchToPValue' but simply reports the exception caught using 'uncaught' and returns
-- 'Prelude.Nothing'.
catchToLog :: String -> GTest c e s t r a -> GTest c e s t r (Maybe a)
catchToLog msg try = ask >>= \env -> catchToPValue msg try >>= \pval -> case pval of
  OK     a  -> return (Just a)
  Backtrack -> return Nothing
  PFail (msg, (SomeException e)) -> uncaught msg (show e) >> return Nothing

-- | Like 'catchToPValue' except 'Dao.Predicate.Backtrack' results (usually caused by
-- 'Control.Exception.UserInterrupt' exceptions) are translated to the
-- @('Control.Monad.mzero' :: 'ResultM' c e s t r a) value, and uncaught exceptions are translated
-- to the @('Control.Monad.Error.throwError' :: 'ResultM' c e s t r a)@ data type.
catchToResultM :: String -> GResultM c e s t r a -> GResultM c e s t r a
catchToResultM msg try = resultM $ \st -> do
  pval <- catchToPValue msg (runResultM try st)
  case pval of
    OK (Backtrack, st) -> return (Backtrack, st)
    OK (PFail err, st) -> return (PFail err, st)
    OK (OK      a, st) -> return (OK      a, st)
    Backtrack          -> return (Backtrack, st)
    PFail err          -> return (PFail err, st)

writeResult :: Integer -> r -> Maybe (String, SomeException) -> GTest c e s t r ()
writeResult i result err = do
  msg <- unitTest showResult result
  liftIO $ writeFile (show i++".log") $ intercalate "\n" $ concat $
    [maybe [] (\ (msg, SomeException err) -> ["TEST ERROR: on "++msg, show err]) err, [msg]]

runSingleTest :: (GTest c e s t r Bool -> GTest c e s t r Bool) -> Integer -> GTest c e s t r Bool
runSingleTest reportBracket i = local (\env -> env{currentTestID=Just i}) $ do
  env <- ask
  let incErr = modTestEnv errorCounter (\c -> (c+1, False))
  let chk mayb fn = maybe (incErr >> return False) fn mayb
  test <- catchToLog "generating test" (unitTest generateTest i)
  chk test $ \test -> do
    result <- catchToLog "evaluating test" (unitResult test)
    chk result $ \ (pval, result) -> do
      stats <- join $
        unit combineStats <*> pure result <*> (asks testStatistics >>= liftIO . readMVar)
      done  <- catchToLog "writing report file" $ case pval of
        Backtrack -> incErr >> reportBracket (writeResult i result Nothing    >> return False)
        PFail err -> incErr >> reportBracket (writeResult i result (Just err) >> return False)
        OK     () -> modTestEnv_ testStatistics (const stats) >> return True
      return $ maybe False (const True) done

-- | With the list of integers given as command line parameters, generate tests but don't evaluate
-- them, just print them to the standard output channel.
showTestMode :: [String] -> GTest c e s t r ()
showTestMode args = ask >>= \env -> do
  disp <- unit showTest
  case disp of
    Nothing   -> fail "Show Tests mode not available for this unit test"
    Just disp -> case args of
      "-":args -> case args of
        [] -> liftIO $ fix $ \loop -> isEOF >>= \stop -> unless stop $
          getLine >>= readIO >>= (testInIO env . unitTest generateTest) >>=
            liftIO . putStrLn . disp >> loop
        arg:_ -> fail (show arg++"Must not specify any command line arguments after \"-\"")
      args  -> liftIO (forM args $ readIO >=> evaluate) >>=
        mapM_ (unitTest generateTest >=> (liftIO . putStrLn . disp))

-- | Read list of test ID's to be executed from STDIN, execute each test after reading each line of
-- input.
stdinMode :: GTest c e s t r ()
stdinMode = report >> loop 0 0 0 where
  report = liftIO $ putStrLn "(running tests received from standard-input)"
  loop :: Int -> Int -> Int -> GTest c e s t r ()
  loop errCount attempts fails = ask >>= \env -> liftIO $ do
    done <- System.IO.isEOF
    if done
    then putStrLn $ unwords ["(End of input.", show attempts, "tests run,", show fails, "failed.)"]
    else do
      line <- dropWhile isSpace <$> getLine
      if null line
      then testInIO env $ loop errCount attempts fails
      else do
        if head line == '#'
        then testInIO env $ loop 0 attempts fails
        else case readsPrec 0 line of
              [(i, _)] -> do
                print i
                testInIO env $ do
                  pass <- runSingleTest id i
                  loop 0 attempts ((if pass then id else (+1)) fails)
              _        -> do
                hPutStrLn stderr $ unlines $
                  [ "ERROR: line of input cannot be parsed as integer value", line ]
                if errCount>1
                then hPutStrLn stderr $ unlines $
                        [ "Data from STDIN does not appear to be formatted correctly."
                        , "Aborting test."
                        ]
                else testInIO env $ loop (errCount+1) attempts fails

-- | Run all tests in a single thread. This is nice for debugging with GHCi because you can set
-- break points and the entire test program will be paused on the break point. It is not quite as
-- nice as threaded mode because SIGTSTP signalling a test will not result in a clean shut-down.
singleThreaded :: Integer -> GTest c e s t r ()
singleThreaded i = ask >>= \env -> void $ catchToLog "main test cycle" $ do
  failMax <- asksTestConfig maxErrors
  (fix $ \loop i -> do
      passed <- runSingleTest id i
      updateClock
      trySaveTestID i
      failed <- liftIO $ readMVar (errorCounter env)
      if failed<failMax
      then loop (i+1)
      else liftIO $ putStrLn "(Maximum number of failed tests reached. Test program ended.)"
    ) i

-- | Run tests in multi-threaded mode. This allow tests to complete their current work and stop
-- gracefully in the event of a SIGTSTP. Also, if a thread dies unexpectedly, new threads are
-- automatically spawned to try and keep the number of working threads the same as the number of
-- threads specified in the configuration file.
multiThreaded :: Integer -> GTest c e s t r ()
multiThreaded i = ask >>= \env -> asksTestConfig id >>= \config -> liftIO $ do
  onSwitch <- newMVar True
  let isOn        = readMVar onSwitch
  let switchOff   = modifyMVar_ onSwitch (return . const False)
  let workerCount = min maxCPUCores $ max 1 $ threadCount config
  unfinsh <- newMVar [] :: IO (MVar [Integer])
  maxerrs <- testInIO env (asksTestConfig maxErrors)
  flagsem <- Sem.new 0 -- the semaphore to signal the main thread of a completed test
  busyvar <- newMVar (0, 0) -- (number of existing threads, number of busy threads)
  let setBusy (f1, f2) = modifyMVar_ busyvar (\ (x1, x2) -> return (f1 x1, f2 x2))
  let startWork  = setBusy ((+1), id)
  let endWork    = setBusy (subtract 1, id)
  let pauseTask  = setBusy (id, subtract 1)
  let continTask = setBusy (id, (+1))
  let reportBracket write = liftIO $ -- Writing a report is guarded by a semaphore.
        bracket_ -- < This is a bracket that guards 'writeResult' function during 'runSingleTest'.
          (mask_ (pauseTask >> Sem.signal flagsem) >> Sem.wait flagsem) -- < Requests to write.
          (mask_ continTask) -- < Signals that writing is complete and next loop will begin.
          (testInIO env write) -- < The 'writeResult' function.
  let worker   i = bracket_ startWork endWork $
        (fix $ \loop i -> do
            canContinue  <- isOn
            isBelowLimit <- fmap (<maxerrs) $ testInIO env (asks errorCounter) >>= readMVar
            when (canContinue && isBelowLimit) $ do
              flip onException (modifyMVar_ unfinsh (return . (i:)) >> Sem.signal flagsem) $ do
                passed <- testInIO env $ do
                  p <- runSingleTest reportBracket i
                  updateClock >> trySaveTestID i >> return p
                Sem.signal flagsem >> yield >> loop (i + fromIntegral workerCount)
          ) i
  workers <- newMVar [] :: IO (MVar [ThreadId])
  let startWorkersOn = mapM (forkIO . worker) >=> \thds -> modifyMVar_ workers (return . (++thds))
  let forceHalt e = swapMVar workers [] >>= (mapM_ (flip throwTo e))
  -- Start the workers, then begin looping on the flag semaphore. Since the flag semaphore is only
  -- used when writing a test report or signalling that a thread has died, this main loop does
  -- nothing more than wait for signals and spawn new threads if the thread count is lower than what
  -- was requested in the configuration file. It also waits for 'Control.Exception.UserInterrupt'
  -- signals (probably generated by Posix SIGTSTP) and distributes this signal to the workers when
  -- it receives one.
  startWorkersOn (take workerCount [i..])
  catches 
    (fix $ \loop -> do
        Sem.wait   flagsem
        (thcount, busycount) <- readMVar busyvar
        errcount <- testInIO env (asks errorCounter) >>= readMVar
        if errcount<maxerrs
        then do
          unfinished <- swapMVar unfinsh [] -- start more workers if any seem to have died
          unless (null unfinished) (startWorkersOn unfinished) >> loop
        else switchOff
      )
    [ Handler $ \e -> case e of
        UserInterrupt -> putStrLn "(CANCEL signal received, shutting down...)" >> switchOff
        e             -> forceHalt e
    , Handler (\ (SomeException e) -> forceHalt e)
    ]

