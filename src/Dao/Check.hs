-- "Dao/Check.hs"  defines some handy functions for running tests without
-- using QuickCheck.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see the URL:
-- <http://www.gnu.org/licenses/agpl.html>.

-- | Some simple, handy functionality for running tests. This is more useful and much faster than
-- QuickCheck when constructing exhaustive tests.
--
-- The design of this module is to facilitate running tests in GHCi with functions like 'runTest',
-- 'testUntilFail', and 'testAll', but also provides the 'testMultiThread' function for running
-- several related tests all in one run without the need for interaction.
module Dao.Check where

import           Dao.Concurrent

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad

import           Data.Function
import           Data.Monoid
import qualified Data.Map  as M
import qualified Data.Text as Strict
import           Data.Typeable

import           System.IO
import           System.Timeout

-- | This data type sets up a test suite that can be run on a list of inputs. Provide this test
-- suite and a list of items to 'testSuite' to run every test. Each test is Given a time limit (in
-- microseconds) a label (for error reporting), run an IO function with the given time limit, check
-- the result using the given check function. Write a message to standard out and halt if any input
-- causes a timeout, or if any input causes the check function to evaluate to 'Prelude.False'.  If
-- 'testVerbose' is a 'Prelude.Just' wrapper containing a function that can convert the input to a
-- string, then every input is sent to the provided function (predumably the 'Prelude.print'
-- function or something similar) before being evaluated.
data TestParams input output
  = TestParams
    { timeLimit   :: Int
      -- ^ if this value is positive, each test will have that number of microseconds to complete
      -- before it is forcably halted. This feature is implemented with the 'System.Timeout.timeout'
      -- function.
    , testLabel   :: Strict.Text
      -- ^ a label for this test, useful for reporting failed results. It is not used to uniquely
      -- identify the test parameters, but for the sake of your own sanity, every 'testLabel' should
      -- be unique and descriptive.
    , testRunner  :: input -> IO output
      -- ^ this function actually runs the test.
    , testCheck   :: output -> Bool
      -- ^ this function checks the output produced by the 'testRunner'. If this function evaluates to
      -- 'Prelude.False', the test fails and can be reported.
    , testVerbose :: Maybe (input -> IO ())
      -- ^ provide an optional verbose function (for example, 'Prelude.print') to see every test input
      -- evaluated by the provided verbose function before it is evaluated by the 'testRunner'.
    }

data FailedTest input output
  = TIMED_OUT  { on_input :: input }
  | TEST_FAILED{ on_input :: input, unexpected_result :: output }
  | TEST_ERROR { on_input :: input, exception_occurred :: SomeException }
  deriving (Show, Typeable)

-- | This data type is used to configure parallel test runs. This data type is usually not needed
-- for simpler test sutes. Ordinarily, you can just pass the 'testSuiteParams' and 'testInputs'
-- directly to the 'testRunner' function.
data TestSuite input output
  = TestSuite
    { testSuiteParams     :: TestParams input output
      -- ^ Provide the 'TestParam' parameters (see above).
    , testSuiteInputs     :: [input]
      -- ^ Provide the set of inputs to test.
    , testSuiteRunner     :: TestParams input output -> [input] -> IO [FailedTest input output]
      -- ^ Provide a test runner function, one of 'testUntilFail' or 'testAll'.
    , testSuiteVerbose    :: Maybe (input -> Strict.Text)
      -- ^ Provide a printing function for optionally reporting tests as they occur. If this
      -- function is provided, it will actually replace the 'testVerbose' parameter in the
      -- 'TestParams' object of the 'testSuiteParams' value, unless the 'testVerbose' parameter is
      -- also 'Prelude.Nothing', in which case it remains 'Prelude.Nothing'.
    , testSuiteShowFailed :: FailedTest input output -> Strict.Text
      -- ^ Provide a printing function for reporting failed tests as they happen.
    }

-- | Runs a single test with a single input using the given 'TestParams'. Returns an empty list on
-- success, or a list containing the single 'TestResult'.
runTest :: TestParams input output -> input -> IO [FailedTest input output]
runTest (TestParams t _ f check verbose) input = do
  maybe (return ()) ($ input) verbose
  ((if t<=0 then fmap Just else timeout t) (f input) >>= evaluate >>= \output ->
      return $ case output of
        Nothing                    -> [TIMED_OUT input]
        Just output | check output -> []
        Just output                -> [TEST_FAILED input output]
    ) `catches` [Handler $ return . return . TEST_ERROR input]

-- | Runs the test provided in 'TestParams' with the given parameters. Halts after the first failed
-- case, all tests run in the current thread.
testUntilFail :: TestParams input output -> [input] -> IO [FailedTest input output]
testUntilFail test = fix $ \loop inputs -> case inputs of
  []           -> return []
  input:inputs -> runTest test input >>= \result ->
    if null result then loop inputs else return result

-- | Run all tests and return a list of all failed tests, does not halt after the first failure, but
-- the list of results will not contain any 'ALL_PASSED' values. If all tests passed, the resultant
-- list is empty.
testAll :: TestParams input output -> [input] -> IO [FailedTest input output]
testAll test = fmap concat . mapM (runTest test)

runTestSuite :: TestSuite input output -> IO [FailedTest input output]
runTestSuite testSuite =
  testSuiteRunner testSuite (testSuiteParams testSuite) (testSuiteInputs testSuite)

-- | Run a 'TestSuite' in the current thread, print reports to standard output. This is the
-- single-threaded analogue of 'testMultiThreaded'.
_testSingleThread :: (Strict.Text -> IO ()) -> TestSuite input output -> IO Bool
_testSingleThread print testSuite = do
  let report msg = print $ Strict.pack (msg++": ") <> testLabel (testSuiteParams testSuite)
  report "BEGIN"
  results <- runTestSuite testSuite
  let ok = null results
  case ok of
    True  -> report "PASSED"
    False -> forM_ results (print . testSuiteShowFailed testSuite) >> report "FAILED"
  return ok

-- | Run a 'TestSuite' in the current thread, print reports to standard output. This is the
-- single-threaded analogue of 'testMultiThreaded'.
testSingleThread :: TestSuite input output -> IO Bool
testSingleThread = _testSingleThread (putStrLn . Strict.unpack)

-- | Run every 'TestSuite' in a separate thread. Tests that fail are reported as they fail.
testMultiThread :: [TestSuite input output] -> IO Bool
testMultiThread testSuites = do
  theReporter <- makeReporter stdout stderr
  mvar        <- newEmptyMVar
  let print = report theReporter True . Strict.unpack
  let startWorker testSuite = fmap (\pid -> (pid, testSuite)) $ forkIO $
        catches
          (yield >> (,) <$> _testSingleThread print testSuite <*> myThreadId >>= putMVar mvar)
          [ Handler $ \ (SomeException e) -> do
              myThreadId >>= putMVar mvar . (,) False >>= evaluate
              yield >> print (testLabel (testSuiteParams testSuite) <> Strict.pack (": "++show e))
          ]
  let updateVerbose testSuite =
        testSuite
        { testSuiteParams = ($ (testSuiteParams testSuite)) $ \params ->
            params
            { testVerbose = do
                testVerbose params -- Both 'testVerbose' and 'testSuiteVerbose'...
                f <- testSuiteVerbose testSuite -- ...both must be non-Nothing.
                return (print . f)
            }
        }
  table <- M.fromList <$> mapM startWorker (updateVerbose <$> testSuites)
  catches
    (fix (\loop allPassed table -> if M.null table then return allPassed else
            takeMVar mvar >>= \ (pass, pid) -> loop (pass && allPassed) (M.delete pid table)
          ) True table
    )
    [ Handler $ \ (SomeException e) -> do
        mapM_ killThread (M.keys table)
        yield >> reportERR theReporter (show e++": all workers forcibly terminated")
        yield >> throwIO e
    ]

