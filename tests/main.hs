-- "tests/pprint.hs"  tests the 'Dao.PPrint' module.
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

module Main where

import           Dao.Prelude
import           Dao.String
import           Dao.Predicate
import           Dao.PPrint
-- import           Dao.OldParser
import           Dao.NewParser
import           Dao.Struct
import           Dao.Random
import           Dao.Object
import           Dao.Object.AST
-- import           Dao.Object.OldParser
import           Dao.Object.NewParser
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

import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.IORef
import           Data.Binary
import qualified Data.Binary          as B
import qualified Data.ByteString.Lazy as B

import           Text.Printf

import           System.IO
import           System.Environment
import Debug.Trace
----------------------------------------------------------------------------------------------------

type TestRun a = ReaderT TestEnv IO a

data TestConfig
  = TestConfig
    { doTestParser      :: Bool
    , doTestSerializer  :: Bool
    , doTestStructizer  :: Bool
    , maxRecurseDepth   :: Int
    , threadCount       :: Int
    , maxErrors         :: Int
    , displayInterval   :: Int
    , logFileName       :: String
    }
  deriving (Show, Read)

defaultTestConfig =
  TestConfig
  { doTestParser      = True
  , doTestSerializer  = False
  , doTestStructizer  = False
  , maxRecurseDepth   = 5
  , displayInterval   = 4000000
  , threadCount       = 3
  , maxErrors         = 200
  , logFileName       = "./uncaught.log"
  }

data TestEnv
  = TestEnv
    { testIDFileHandle  :: MVar (Handle, HandlePosn)
    , testPassedButton  :: MVar Bool
    , testCounter       :: MVar (Rational, Rational)
    , errorCounter      :: MVar Int
    , inputChannel      :: MVar (Maybe Int)
    , uncaughtExceptLog :: MVar Handle
    , testConfig        :: TestConfig
    }

main :: IO ()
main = do
  -- testIDs <- getArgs >>= mapM readIO
  config  <- catches (readFile "./test.cfg" >>= readIO) $ errHandler $ \msg -> do
    hPutStrLn stderr ("Warning: "++msg++", creating default configuration")
    hPutStrLn stderr "Warning: setting default configuration"
    catches (writeFile "./test.cfg" (show defaultTestConfig)) $ errHandler $ \msg -> do
      hPutStrLn stderr ("Error: failed to open file \"./test.cfg\" for writing, "++msg)
    return defaultTestConfig
  multiThreaded config

----------------------------------------------------------------------------------------------------

multiThreaded :: TestConfig -> IO ()
multiThreaded config = do
  i <-  catches (readFile "./testid" >>= readIO) $ errHandler $ \msg -> do
          hPutStrLn stderr ("Warning: could not read file \"./testid\", "++show msg)
          hPutStrLn stderr "Warning: startig at test ID #0"
          return 0
    -- dont bother reading the test ID file if the test IDs have been specified on the command line
  ch       <- newEmptyMVar
  notify   <- newEmptyMVar
  errcount <- newMVar 0
  counter  <- newMVar (0, 0)
  h        <- openFile "./testid" ReadWriteMode
  pos      <- hGetPosn h
  hlock    <- newMVar (h, pos)
  excplog  <- openFile (logFileName config) WriteMode
  excplogVar <- newMVar excplog
  let testEnv = 
        TestEnv
        { testIDFileHandle  = hlock
        , testPassedButton  = notify
        , testCounter       = counter
        , errorCounter      = errcount
        , inputChannel      = ch
        , uncaughtExceptLog = excplogVar
        , testConfig        = config
        }
  workThreads <- replicateM (threadCount config) $ forkIO (runReaderT testThread testEnv)
  iterThread  <- forkIO $ runReaderT (iterLoop 0) testEnv
  dispThread  <- forkIO (forever $ threadDelay (displayInterval config) >> runReaderT displayInfo testEnv)
  catches (runReaderT ctrlLoop testEnv) (errHandlerMessage "exception in main control loop")
  killThread dispThread >> killThread iterThread
  hClose excplog        >> hClose     h

ctrlLoop :: TestRun ()
ctrlLoop = ask >>= \env -> liftIO $ do
  passed <- takeMVar (testPassedButton env) -- Wait for a result to come back
  modifyMVar_ (testCounter env) (\ (total, count) -> return (total+1, count+1))
  count <- modifyMVar (errorCounter env) $ \count -> do
    let nextCount = if not passed then count+1 else count
    return (nextCount, nextCount)
  if count < maxErrors(testConfig env) then runReaderT ctrlLoop env else return ()

-- Iterate test IDs by simply incrementing a counter.
iterLoop :: Int -> TestRun ()
iterLoop i = ask >>= \env -> isOn >>= \continue -> liftIO $ do
  putMVar (inputChannel env) (if continue then Just i else Nothing)
  runReaderT (iterLoop (i+1)) env

displayInfo :: TestRun ()
displayInfo = ask >>= \env -> liftIO $ do
  (total, count) <- modifyMVar (testCounter env) $ \ (total, count) ->
    return ((total,0), (total,count))
  putStrLn $ concat $
    [ show (round total), " tests completed at the rate of "
    , printf "%.2f tests per second"
        (fromRational
            (toRational count * 1000000 / toRational (displayInterval(testConfig env))) :: Float)
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

-- This function loops, pulling test IDs out of the 'inputChannel' of the 'TestEnv', then passing
-- the test ID to a function that generates a test case for that ID, then checks the test case.
-- This is the function that should be run in its own thread.
loopOnInput :: Show a => (Int -> TestRun a) -> (a -> TestRun Bool) -> TestRun ()
loopOnInput genFunc checkFunc = loop where
  loop = ask >>= \env -> isOn >>= \continue -> liftIO $ do
    if continue
      then do
        i <- takeMVar (inputChannel env)
        case i of
          Nothing -> return ()
          Just  i -> do
            passed <- handle (\e -> runReaderT (h i Nothing e) env) $ do
              obj <- runReaderT (genFunc i) env
              catch (runReaderT (checkFunc obj) env) (\e -> runReaderT (h i (Just (show obj)) e) env)
            putMVar (testPassedButton env) passed -- wait our turn to report success/failure
            runReaderT (updateTestIDFile i >> loop) env
      else return ()
  h i obj (SomeException e) = do
    catchToLog $ \logFile -> do
      hPutStrLn logFile ("ITEM #"++show i++"\n"++show e)
      hPutStrLn logFile $ case obj of
        Nothing  -> "(error occurred while generating test object)"
        Just msg -> msg
    return False

sep :: String
sep = "--------------------------------------------------------------------------"

----------------------------------------------------------------------------------------------------

data TestCase
  = TestCase
    { testCaseID       :: Int
    , originalObject   :: AST_TopLevel
    , parseString      :: Maybe UStr
    , serializedObject :: Maybe B.ByteString
    , structuredObject :: Maybe T_tree
    , originalInterm   :: Maybe [TopLevelExpr]
    , testResult       :: UStr
    , failedTestCount  :: Int
    }

instance Show TestCase where
  show obj = unlines $
    [ uchars (testResult obj)
    , "original object:", prettyPrint 80 "    " (originalObject obj)
    , show (originalObject obj)
    , sep
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
  cfg <- asks testConfig
  let maxDepth = maxRecurseDepth cfg
  let obj = genRandWith randO maxDepth i
  let intermObj = toInterm obj
  let setup isSet o = if isSet cfg then Just o else Nothing
  return $
    TestCase
    { testCaseID       = i
    , originalObject   = obj
    , parseString      = setup doTestParser     $ ustr (showPPrint 80 "    " (pPrint obj))
    , serializedObject = setup doTestSerializer $ B.encode intermObj
    , structuredObject = setup doTestStructizer $ dataToStruct obj
    , originalInterm   = setup doTestStructizer $ intermObj
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
checkTestCase tc = do
  ------------------- (0) Canonicalize the original object ------------------
  tc <- case canonicalize (originalObject tc) of
    [o] -> return (tc{originalObject=o})
    [ ] -> error "could not canonicalize original object"
    ox  -> error "original object canonicalized to multiple possible values"
  --------------------------- (1) Test the parser ---------------------------
  tc <- tryTest tc parseString $ \str -> do
    case parse daoGrammar mempty (uchars str)  of
    -- case (par, msg) = {-# SCC par #-} runParser (regexMany space >> parseDirective) str of
      OK      o -> do
        let diro = fmap delLocation (directives o)
        if diro == [originalObject tc]
          then  passTest tc
          else  passTest tc -- temporarily disabling the equality condition
      --    failTest tc $ unlines $ concat
      --      [ ["Parsed AST does not match original object, parsed AST is:"]
      --      , case diro of
      --          []  -> ["(Empty AST)"]
      --          [o] -> [prettyPrint 80 "    " o, show o]
      --          ox  -> ["(Returned multiple AST items)"]++
      --            (ox >>= \o -> [prettyPrint 80 "    " o, show o])
      --      ]
      Backtrack -> failTest tc "Parser backtrackd"
      PFail   b -> failTest tc (show b)
  --
  ------------------------- (2) Test the serializer -------------------------
  tc <- tryTest tc (\tc -> liftM2 (,) (serializedObject tc) (originalInterm tc)) $ \binObjPair ->
    case binObjPair of
      (_, []   ) -> failTest tc $
        "Evaluated to null when converting the AST to an intermediate structure"
      (bin, [obj]) -> do
        unbin <- liftIO $ catches (return $ Right $ B.decode bin) $
          errTryCatch "binary deserialization failed"
        case unbin of
          Left  msg   -> failTest tc (uchars msg)
          Right unbin ->
            if obj==unbin
              then  passTest tc
              else
                failTest tc $ unwords $
                  [ "Original object does not match object deserialized from binary string"
                  , "Received bytes:", showBinary bin
                  , "Deserialied object:"
                  , prettyPrint 80 "    " unbin
                  ]
      (_, ox) -> failTest tc $ unlines $
        ["Evaluating the AST to it's intermediate form yielded multiple possible results:"
        , unlines (map ((++(sep++"\n")) . prettyPrint 80 "    ") ox)
        ]
  --
  ---------------- (3) Test the intermediate tree structures ----------------
  tc <- tryTest tc  structuredObject $ \obj ->
    case structToData obj :: PValue UpdateErr AST_TopLevel of
      OK struct ->
        if originalObject tc == struct
          then
            failTest tc $ unwords $
              [ "Original object does not match Haskell object constructed from it's Dao tree"
              , prettyPrint 80 "    " struct
              ]
          else  passTest tc
      Backtrack -> failTest tc "Backtracked while constructing a Haskell object from a Dao tree"
      PFail   b -> failTest tc (show b)
  --
  ------------------------------ Report results -----------------------------
  liftIO (reportTest tc)

----------------------------------------------------------------------------------------------------

-- This is the computation that runs a series of tests, looping over the 'inputChannel'.
testThread :: TestRun ()
testThread = loopOnInput newTestCase checkTestCase

