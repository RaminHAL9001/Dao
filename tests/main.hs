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
-- import           Dao.Parser
import           Dao.NewParser
import           Dao.Struct
import           Dao.Random
import           Dao.Object
import           Dao.Object.AST
-- import           Dao.Object.Parser
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
    { onOffButton       :: MVar Bool
    , testIDFileHandle  :: MVar (Handle, HandlePosn)
    , testPassedButton  :: MVar Bool
    , testCounter       :: MVar (Rational, Rational)
    , errorCounter      :: MVar Int
    , inputChannel      :: MVar (Maybe Int)
    , uncaughtExceptLog :: MVar Handle
    , testConfig        :: TestConfig
    }

main = do
  testIDs <- getArgs >>= mapM readIO
  config  <- catches (readFile "./test.cfg" >>= readIO) $ errHandler $ \msg -> do
    hPutStrLn stderr ("Warning: "++msg++", creating default configuration")
    hPutStrLn stderr "Warning: setting default configuration"
    catches (writeFile "./test.cfg" (show defaultTestConfig)) $ errHandler $ \msg -> do
      hPutStrLn stderr ("Error: failed to open file \"./test.cfg\" for writing, "++msg)
    return defaultTestConfig
  i <-  if null testIDs
          then do
            catches (readFile "./testid" >>= readIO) $ errHandler $ \msg -> do
              hPutStrLn stderr ("Warning: could not read file \"./testid\", "++show msg)
              hPutStrLn stderr "Warning: startig at test ID #0"
              return 0
          else return 0
    -- dont bother reading the test ID file if the test IDs have been specified on the command line
  ch      <- newEmptyMVar
  notify  <- newEmptyMVar
  errcount <- newMVar 0
  counter <- newMVar (0, 0)
  h       <- openFile "./testid" ReadWriteMode
  pos     <- hGetPosn h
  hlock   <- newMVar (h, pos)
  hwait   <- newMVar True
  excplog <- openFile (logFileName config) WriteMode
  excplogVar <- newMVar excplog
  let testEnv = 
        TestEnv
        { onOffButton       = hwait
        , testIDFileHandle  = hlock
        , testPassedButton  = notify
        , testCounter       = counter
        , errorCounter      = errcount
        , inputChannel      = ch
        , uncaughtExceptLog = excplogVar
        , testConfig        = config
        }
  workThreads <- replicateM (threadCount config) $ forkIO (runReaderT testThread testEnv)
  iterThread  <- forkIO $ flip runReaderT testEnv $
    if null testIDs then iterLoop 0 else iterTestIDs testIDs
  dispThread  <- forkIO (forever $ threadDelay (displayInterval config) >> runReaderT displayInfo testEnv)
  catches (runReaderT ctrlLoop testEnv) (errHandlerMessage "exception in main control loop")
  killThread dispThread >> killThread iterThread
  hClose excplog        >> hClose     h

ctrlLoop :: TestRun ()
ctrlLoop = do
  env <- ask
  count <- liftIO $ do
    passed <- takeMVar (testPassedButton env)
    count  <- modifyMVar (errorCounter env) $ \count -> do
      let nextCount = if not passed then count+1 else count
      return (nextCount, nextCount)
    modifyMVar_ (testCounter env) (\ (total, count) -> return (total+1, count+1))
    return count
  if count < maxErrors (testConfig env)
    then  ctrlLoop
    else  liftIO $ modifyMVar_ (onOffButton env) (return . const False)

-- Iterate test IDs by simply incrementing a counter.
iterLoop :: Int -> TestRun ()
iterLoop i = ask >>= \env -> liftIO $ do
  continue <- readMVar (onOffButton env)
  putMVar (inputChannel env) (if continue then Just i else Nothing)
  runReaderT (iterLoop (i+1)) env

-- Iterate over a list of integer test IDs.
iterTestIDs :: [Int] -> TestRun ()
iterTestIDs testIDs = ask >>= \env -> liftIO $ do
  continue <- readMVar (onOffButton env)
  let chan = inputChannel env
  let end  = putMVar chan Nothing
  if not continue
    then  end
    else  case testIDs of
      []   -> end
      i:ix -> putMVar chan (Just i) >> runReaderT (iterTestIDs ix) env

displayInfo :: TestRun ()
displayInfo = ask >>= \env -> liftIO $ do
  (total, count) <- modifyMVar (testCounter env) $ \ (total, count) ->
    return ((total,0), (total,count))
  putStrLn $ concat $
    [ show total, " tests completed at the rate of "
    , printf "%.2f tests per second"
        (fromRational
            (toRational count * 1000000 / toRational (displayInterval(testConfig env))) :: Float)
    ]

----------------------------------------------------------------------------------------------------

isOn :: TestRun Bool
isOn = asks onOffButton >>= liftIO . readMVar

errHandler :: (String -> IO a) -> [Handler a]
errHandler func =
  [Handler(\ (e::IOException) -> func(show e)), Handler(\ (e::ErrorCall) -> func(show e))]

-- Handles errors by outputting the error string to stderr.
errHandlerMessage :: String -> [Handler ()]
errHandlerMessage msg = errHandler (\e -> hPutStrLn stderr (msg++": "++e))

-- Handles errors by returning the error string in a 'Data.Either.Left' constructor.
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
  loop = ask >>= \env -> liftIO $ do
    i <- takeMVar (inputChannel env)
    case i of
      Nothing -> return ()
      Just  i -> do
        passed <- handle (\e -> runReaderT (h i Nothing e) env) $ do
          obj <- runReaderT (genFunc i) env
          catch (runReaderT (checkFunc obj) env) (\e -> runReaderT (h i (Just (show obj)) e) env)
        putMVar (testPassedButton env) passed
        runReaderT (updateTestIDFile i >> loop) env
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
    [uchars (testResult obj), "original object:", prettyPrint 80 "    " (originalObject obj), sep]

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

updateTestCase :: TestCase -> Maybe String -> TestRun TestCase
updateTestCase tc msg = return $
  tc{ testResult = let tr = testResult tc in maybe tr (tr<>) (fmap ustr msg)
    , failedTestCount = (if maybe False (const True) msg then (+1) else id) $ failedTestCount tc
    }

passTest :: TestCase -> TestRun TestCase
passTest tc = updateTestCase tc Nothing

failTest :: TestCase -> String -> TestRun TestCase
failTest tc = updateTestCase tc . Just

-- Test the pretty printer and the parser. If a randomly generated object can be pretty printed, and
-- the parser can parse the pretty-printed string and create the exact same object, then the test
-- pases.
checkTestCase :: TestCase -> TestRun Bool
checkTestCase tc = do
  tc <- case parseString tc of
    Nothing  -> return tc
    Just str -> case parse daoGrammar mempty (uchars str)  of
             -- case (par, msg) = {-# SCC par #-} runParser (regexMany space >> parseDirective) str of
      OK      o -> do
        let diro = directives o
        if diro == [originalObject tc]
          then passTest tc
          else
            failTest tc $ unlines
              [ "Parsed AST does not match original object, parsed AST is:"
              , case diro of
                  []  -> "(Empty AST)"
                  [o] -> prettyPrint 80 "    " o
                  ox  -> "(Returned multiple AST items)\n" ++
                    unlines (map (prettyPrint 80 "    ") ox)
              , sep
              ]
      Backtrack -> failTest tc "Parser backtrackd"
      PFail   b -> failTest tc (show b)
  tc <- case liftM2 (,) (serializedObject tc) (originalInterm tc) of
    Nothing  -> return tc
    Just (_, []   ) -> failTest tc $
      "Evaluated to null when converting the AST to an intermediate structure"
    Just (bin, [obj]) -> do
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
    Just (_, ox) -> failTest tc $ unlines $
      ["Converting the AST evaluated to it's intermediate form yielded in multiple possible results:"
      , unlines (map ((++(sep++"\n")) . prettyPrint 80 "    ") ox)
      ]
  tc <- case structuredObject tc of
    Nothing  -> return tc
    Just obj -> case structToData obj :: PValue UpdateErr AST_TopLevel of
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
  return (failedTestCount tc > 0)

testThread :: TestRun ()
testThread = loopOnInput newTestCase checkTestCase

