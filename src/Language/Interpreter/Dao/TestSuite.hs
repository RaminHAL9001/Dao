module Language.Interpreter.Dao.TestSuite where

import           Prelude hiding (error, undefined, print, putStr, putStrLn)

import           Language.Interpreter.Dao

--import           Control.Arrow
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans

--import           Data.Array.IArray
import           Data.Char
import           Data.Function
import           Data.List          (stripPrefix, intercalate)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup
import qualified Data.Text    as Strict
import qualified Data.Text.IO as Strict

import           System.Directory
import           System.Environment
import           System.IO.Unsafe
import           System.IO hiding (print, putStr, putStrLn)
--import           Debug.Trace

----------------------------------------------------------------------------------------------------

verbose :: Bool
verbose = True

report :: (Monad m, MonadIO m) => String -> m ()
report = liftIO . if verbose then putStrLn else const $ return ()

error :: String -> void
error = throw . TestSuiteException . Strict.pack

defaultTestConfigPath :: FilePath
defaultTestConfigPath = "./dao-test-suite.config"

----------------------------------------------------------------------------------------------------

type ShortSwitch = Char
type LongSwitch  = String

checkCommandLineParam :: ShortSwitch -> LongSwitch -> [String] -> IO (Maybe String, [String])
checkCommandLineParam short long args = loop id args where
  loop back = \ case
    a : b : ax | a == '-':short:"" || a == "--"++long
               -> return (Just b, back ax)
    a     : ax -> let param = "--" ++ long ++ "=" in case stripPrefix param a of
        Nothing  -> loop (back . (a :)) ax
        Just str -> return (Just str, back ax)
    []         -> return (Nothing, back [])

checkCommandLineFlag :: Maybe ShortSwitch -> LongSwitch -> [String] -> IO (Maybe Bool, [String])
checkCommandLineFlag short long args = loop id args where
  loop back = \ case
    ('-':a:"") : ax | Just a == short
           -> return (Just True, back ax)
    a : ax          | a == "--"++long
           -> return (Just True , back ax)
    a : ax          | a == "--no-"++long
           -> return (Just False, back ax)
    a : ax -> loop (back . (a :)) ax
    []     -> return (Nothing, back [])

----------------------------------------------------------------------------------------------------

type TestLabel = String

-- | Construct this type with the name of the function in which the failure occurred. Before
-- throwing this exception, it is a good idea to call 'report' with an explanation of what went
-- wrong.
newtype TestSuiteException = TestSuiteException Strict.Text
  deriving (Eq, Ord)

instance Show TestSuiteException where
  show (TestSuiteException msg) = "TEST FAILED on function " ++ Strict.unpack msg

instance Exception TestSuiteException where {}

stdoutLock :: MVar Handle
stdoutLock = unsafePerformIO $ newMVar stdout
{-# NOINLINE stdoutLock #-}

putStr :: String -> IO ()
putStr msg = modifyMVar_ stdoutLock $ \ h -> do
  hSetBuffering h $ BlockBuffering $ Just 4096
  hPutStr h msg
  hFlush h >>= evaluate
  return h

putStrLn :: String -> IO ()
putStrLn = putStr . (++ "\n") >=> evaluate

print :: Show a => a -> IO ()
print = putStrLn . show >=> evaluate

----------------------------------------------------------------------------------------------------

type Timeout t = t

newtype TimeoutException = TimeoutException Float
  deriving (Eq, Ord, Show, Read, Num, Fractional)

instance Exception TimeoutException where {}

-- | 1.0 seconds.
t1sec :: Timeout (Maybe Float)
t1sec = Just 1.0

-- | 5.0 seconds.
t5sec :: Timeout (Maybe Float)
t5sec = Just 5.0

-- | 12.0 seconds.
t12sec :: Timeout (Maybe Float)
t12sec = Just 12.0

-- | 60.0 seconds.
t60sec :: Timeout (Maybe Float)
t60sec = Just 60.0

----------------------------------------------------------------------------------------------------

data TestCount
  = TestCount
    { succeededTests :: !Int
    , failedTests  :: !Int
    , startedTests :: !Int
    }
  deriving (Eq, Ord)

instance Show TestCount where
  show (TestCount{ succeededTests=ok, failedTests=fail, startedTests=ntests }) =
    let total = ok + fail in if ntests == 0 then "(no results yet)" else
      show ok ++ " passed, " ++ show fail ++ " failed (" ++
      percent ok total ++ "), completed " ++ show total ++ " of " ++
      show ntests ++ " tests (" ++ percent total ntests ++ ")"

instance Exception TestCount where {}

percent :: Int -> Int -> String
percent ok total = if total == 0 then "%" else
  let (lo, hi) = splitAt 2 $ show
        (round (realToFrac ok * 1000.0 / realToFrac total :: Float) :: Int)
  in  reverse $ '%' : lo ++ '.' : hi

testCount :: TestCount
testCount = TestCount{ succeededTests = 0, failedTests = 0, startedTests = 0 }

testsComplete :: TestCount -> Bool
testsComplete tc = failedTests tc + succeededTests tc == startedTests tc

incTestCount :: TestCount -> TestCount
incTestCount tc = tc{ startedTests = startedTests tc + 1 }

incSucceeded :: TestCount -> TestCount
incSucceeded tc = tc{ succeededTests = succeededTests tc + 1 }

incFailed :: TestCount -> TestCount
incFailed tc = tc{ failedTests = failedTests tc + 1 }

----------------------------------------------------------------------------------------------------

data AsyncCtrl = AsyncCtrl { asyncSemaphore :: MVar (), asyncCount :: MVar TestCount }

data TimerControlException = TimerReset | TimerStop | TimerHaltAll
  deriving (Eq, Ord, Enum, Show)

instance Exception TimerControlException where {}

newAsyncCtrl :: IO AsyncCtrl
newAsyncCtrl = AsyncCtrl <$> newEmptyMVar <*> newMVar testCount

asyncReport :: TestLabel -> AsyncCtrl -> IO ()
asyncReport label = readMVar . asyncCount >=> putStrLn . ((label ++ ": ") ++) . show

isolateAsync :: AsyncCtrl -> TestLabel -> IO void -> IO ThreadId
isolateAsync ctrl label test = do
  modifyMVar_ (asyncCount ctrl) $ evaluate . incTestCount
  forkIO $ do
    let count f = do
          modifyMVar_ (asyncCount ctrl) $ evaluate . f
          putMVar (asyncSemaphore ctrl) ()
    result <- catches (Right <$> test)
      [ Handler $ \ (SomeAsyncException e) -> count incFailed >>= evaluate >> throwIO e
      , Handler $ \ e@(SomeException _) -> return (Left e)
      ]
    count incSucceeded
    case result of
      Left (SomeException e) -> putStrLn $ label ++ ": " ++ show e
      Right{} -> return ()

-- | This function creates an 'AsyncCtrl' and automatically waits for all tests to finish. Simply
-- pass the list of tests __without__ wrapping each test with 'isolateAsync'. This function is
-- itself synchronous i.e. it will block until the entire list of tests has completedTests, although all
-- tests are evaluated in parallel.
isolateAsyncSequence :: TestLabel -> Timeout (Maybe Float) -> [(TestLabel, IO ())] -> IO ()
isolateAsyncSequence label timeout tests = do
  ctrl <- newAsyncCtrl
  threads <- sequence $ uncurry (isolateAsync ctrl) <$> tests
  timerThread <- case timeout of
    Nothing -> return Nothing
    Just  t -> liftM Just $ forkIO $ fix $ \ loop -> do
      timerCtrl <- (threadDelay (round $! t * 1000000) >> return TimerHaltAll)
        `catches` [Handler return]
      case timerCtrl of
        TimerStop    -> return ()
        TimerReset   -> loop
        TimerHaltAll -> forM_ threads $ flip throwTo (TimeoutException t)
  fix $ \ loop -> do
    takeMVar (asyncSemaphore ctrl)
    tc <- readMVar (asyncCount ctrl)
    let report = when verbose $ putStrLn $ label ++ ": " ++ show tc
    if testsComplete tc then maybe (return ()) (`throwTo` TimerStop) timerThread >>
        report >> when (failedTests tc > 0) (throwIO tc)
      else report >> maybe (return ()) (`throwTo` TimerReset) timerThread >> loop

----------------------------------------------------------------------------------------------------

-- | Isolate a test in it's own thread, with an optional time limit, evaluating synchronously so
-- that this function call does not return until the test completes or times out.
isolateSyncTimed :: TestLabel -> Timeout (Maybe Float) -> IO void -> IO ()
isolateSyncTimed label timeout test = do
  mvar <- newEmptyMVar
  thread <- forkIO $ putMVar mvar =<< catches (Right <$> test)
    [ Handler $ \ (SomeAsyncException e) -> throwIO e
    , Handler $ \ e@(SomeException _) -> return (Left e)
    ]
  case timeout of
    Nothing -> return ()
    Just  t -> void $ forkIO $ do
      threadDelay $ round $ t * 1000000
      throwTo thread $ TimeoutException t
  takeMVar mvar >>= \ case
    Left (SomeException e) -> putStrLn (label ++ '\n' : show e) >> throwIO e
    Right{}  -> return ()

-- | Abbreviation for 'isolateSyncTimed' with a 'Prelude.Float' literal time limit.
isolateSyncT :: TestLabel -> Timeout Float -> IO void -> IO ()
isolateSyncT label = isolateSyncTimed label . Just

-- | Abbreviation for 'isolateSyncTimed' with a one second time limit. Use this for short, simple
-- tests.
isolateSync :: TestLabel -> IO void -> IO ()
isolateSync label = isolateSyncTimed label t1sec

-- | Abbreviation for 'isolateSync'.
isosy :: TestLabel -> IO void -> IO ()
isosy = isolateSync

----------------------------------------------------------------------------------------------------
-- Begin the actual tests.
----------------------------------------------------------------------------------------------------

data TestConfig
  = TestConfig
    { only_immediate_tests :: Bool
      -- ^ This disables all other tests regardless of whether they are set to 'True', and executes
      -- the 'immediate' function. This allows you to easily write a quick test case and run it
      -- immediately without running all other test cases.
    , do_parser_tests      :: Bool
    , do_coder_tests       :: Bool
    , do_eval_tests        :: Bool
    , do_pattern_tests     :: Bool
    }
  deriving (Eq, Ord, Show, Read)

defaultTestConfig :: TestConfig
defaultTestConfig = TestConfig
  { only_immediate_tests = False
  , do_parser_tests      = True
  , do_coder_tests       = True
  , do_eval_tests        = True
  , do_pattern_tests     = True
  }

----------------------------------------------------------------------------------------------------

test1Parse :: (Eq a, Show a, Read a) => a -> String -> IO ()
test1Parse expected str = do
  let fail = error ("test1Parse " ++ show (str :: String))
  let wrong re a = do
        report $ re++" INCORRECT, what was expected:\n    " ++
          show expected ++ "\nWhat was parsed:\n    " ++ show a
        fail
  let cantParse msg = report msg >> fail
  report $ "Parse:    " ++ show str
  a <- case readsPrec 0 str of
    [] -> cantParse "Parser failed entirely"
    [(a, str)] -> do
      unless (null str) $ report $ let (rem, ignor) = splitAt 40 str in
        "WARNING: did not parse the whole string. Remainder is:\n    "++show rem++
        (if null ignor then "" else "...")
      return a
    ax -> cantParse $ (++) "Ambiguous parse:\n" $ zip [1::Int ..] ax >>= \ (i, (a, str)) ->
      flip (++) (show i ++ ": (" ++ show a ++ ") before "++show str++"\n") $ case i of
        i | i <= 9   -> "   "
        i | i <= 99  -> "  "
        i | i <= 999 -> " "
        _            -> ""
  if a /= expected then void $ wrong "Parse" a else do
    report $ "Parse success."
    let b = show a
    report $ "Re-parse: " ++ show b
    b <- readIO b
    if b /= expected then void $ wrong "Re-parse" b else do
      report $ "Re-parse success."

testParsers :: IO ()
testParsers = do
  let test expr str = isosy ("On test input string " ++ show str) $ test1Parse expr str
  test (DaoString "Hello, world") (show ("Hello, world" :: String))
  test (DaoInt 1) "1"
  test (DaoFloat 123.456) "123.456"
  test (DaoFloat 5.0) "5f"
  test (DaoFloat 0.5) "0.5"
  test (DaoFloat 0.5) "5.0e-1"
  test (DaoFloat 0.5) "5.0E-1"
  test (DaoFloat 50.5) "5.05E+1"
  test (DaoFloat 50.5) "5.05e+1"
  test (DaoFloat 50.5) "5.05E1"
  test (DaoFloat 50.5) "5.05e1"
  let atoms = fmap (DaoAtom . plainAtom) . Strict.words
  let ints = fmap DaoInt
  let form = \ case
        a:ax -> plainForm (a :| ax)
        []   -> error "in test program, \"form\" was passed an empty list"
  let list = daoList
  let dict = daoDict 
  test (daoForm $ atoms "hello this is dao lisp") "(hello this is dao lisp)"
  test (daoList $ atoms "hello this is dao lisp") "[hello this is dao lisp]"
  test (daoForm $ atoms "one two three" ++ DaoFloat 123.456 : ints [4,5,6])
    "(one two three 123.456 4 5 6)"
  test (daoForm [DaoVoid, DaoVoid]) "(()())"
  test (daoForm [DaoNull, DaoNull]) "([][])"
  test (daoForm1 [daoForm1 [DaoVoid]]) "((()))"
  test (daoForm1 [daoForm1 $ atoms "hello world"]) "((hello world))"
  test (daoForm $ atoms "TRUE FALSE" ++ [DaoTrue, DaoNull] ++
        atoms "TRUE abc" ++ DaoVoid : DaoAtom "X123" : []
       ) "(TRUE FALSE true false TRUE abc () X123)"
  test (dict [("one", DaoInt 1), ("two", DaoInt 2), ("three", DaoInt 3)])
    "{ :three 3 :two 2 : one 1 }"
  test (dict [ ("-->", DaoString "arrow")
             , (":"  , DaoString "colon")
             , (","  , DaoString "comma")
             , (";"  , DaoString "semicolon")
             ])
    "{ :--> \"arrow\" :: \"colon\" :, \"comma\" :; \"semicolon\" }"
  test (daoForm $ atoms "testing testing" ++ ints [1,2,3]) "(testing testing 1 2 3)"
  test (daoForm
        [DaoComma, DaoComma, DaoComma, DaoSemi, DaoSemi, DaoSemi, DaoColon, DaoColon, DaoColon]
        ) "(,,,;;;:::)" 
  test (daoError "TestError" [("blame", DaoInt 0), ("suggest", DaoString "nothing")])
    "error TestError {:blame 0  :suggest \"nothing\"}"
  let any = pattern1 $ patConst $ Single AnyExpr
  test (daoRule (atoms "d1 d2") (atoms "p1 p2") 0.1 any (form $ atoms "some action"))
    "rule [d1 d2] [p1 p2] 0.1 (any) (some action)"
  test (daoRule [] [] 2.0 any (form $ atoms "some action"))
    "rule 2.0 (any) (some action)"
  test (daoRule (atoms "d1") [] 30.0 any (form $ atoms "some action"))
    "rule [d1] (any) (some action) 30.0"
  test (daoRule (atoms "d1") (atoms "p1 p2 p3") 444.4 any (form $ atoms "some action"))
    "rule [d1] (any) (some action) 444.4 [p1 p2 p3]"
  test (daoRule (atoms "d1 d2") (atoms "p1") 5.0e-5 any (form $ atoms "some action"))
    "rule (any) 5.0e-5 [d1 d2] (some action) [p1]"
  isosy "form parser" $ test1Parse
    (daoForm $ atoms "one two -> three" ++ DaoTrue : DaoNull :
      ints [1,2,3] ++
      DaoString "Hello, world!" :
      atoms "ten ==> twenty thirty" ++ ints [10,20,30] ++
      DaoFloat 123.456 :
      (list $ atoms "four five || six" ++
        ints [4,5,6] ++
        DaoString "This is Dao Lisp!" :
        daoError "TestError" [("blame", DaoInt 0), ("suggest", DaoString "nothing")] :
        dict [] :
        atoms "forty fifty sixty" ++
        ints [40,50,60] ++
        []
      ) :
      DaoFloat 456.789 :
      dict [("one", DaoInt 1), ("two", DaoInt 2), ("three", DaoInt 3)] :
      []
    )
    ( "(one two -> three true false 1 2 3 \"Hello, world!\" ten " ++
              " ==> twenty thirty 10 20 30 123.456\n" ++
      "    [four five || six 4 5 6 \"This is Dao Lisp!\" " ++
              " (error TestError {:blame 0 :suggest \"nothing\"})\n" ++
      "    {} forty fifty sixty 40 50 60]\n" ++
      "    456.789 { :three 3 :two 2 :one 1})"
    )

----------------------------------------------------------------------------------------------------

test1FormCoder :: (Eq a, Show a, Inlining a, Outlining a) => a -> [DaoExpr] -> IO ()
test1FormCoder expr expected = isosy ("On test input form " ++ show expr) $ do
  let fail = error $ "test1FormCoder " ++ show expected
  report $ "Encode: " ++ show expr ++ "\nExpecting: " ++ show expected
  let inlined = inlining expr
  when (inlined /= expected) $ do
    report $ (++) "Encoder incorrect.\nReturned: " . showList inlined .
      (++) "\nExpected: " . showList expected $ ""
    fail
  report $ "Decode: " ++ showList inlined ("\nExpecting: " ++ show expr)
  case runOutliner outline inlined of
    (MatchQuit   err, rem) -> do
      report $ "Decoder backtracked: " ++ show err ++ "\nRemaining: " ++ show rem
      fail
    (MatchFail   err, rem) -> do
      report $ "Decoder failed: " ++ show err ++ "\nRemaining: " ++ show rem
      fail
    (MatchOK decoded, rem) -> do
      when (expr /= decoded) $ do
        report $ (++) "Decoder incorrect.\nReturned: " $ showParen True (showsPrec 0 decoded) $
          "\nExpected: " ++ show expr ++
          "\nRemaining: " ++ show rem
        fail
      unless (null rem) $ do
        report $ (++) "WARNING, after decoding " $ showParen True (showsPrec 0 decoded) $
          " there are unused arguments in the decoder stack:\n" ++ show rem
      report $ (++) "Encode/Decode OK: " . showList inlined .
        (++) " == " . showParen True (showsPrec 0 decoded) $ ""

testCoders :: IO ()
testCoders = do
  let atoms = fmap (DaoAtom . plainAtom) . Strict.words
  let form = \ case
        []   -> error "In TestSuite main \"testCoders\": passed empty list to \"form\""
        a:ax -> plainForm $ a :| ax
  let typf = form $ atoms "call some function"
  -- ExprPatUnit
  test1FormCoder (EqExpr      $ DaoInt 1234) [DaoAtom "==", DaoInt 1234]
  test1FormCoder (NotEqExpr   $ DaoString "Hello, world!")
    [DaoAtom "/=", DaoString "Hello, world!"]
  test1FormCoder (IsPrimType    DaoVoidType) [DaoAtom "type==", DaoAtom "Void"]
  test1FormCoder (IsNotPrimType DaoNullType) [DaoAtom "type/=", DaoAtom "Null"]
  test1FormCoder (IsPrimType    DaoIntType)  [DaoAtom "type==", DaoAtom "Int"]
  test1FormCoder (IsNotPrimType DaoDictType) [DaoAtom "type/=", DaoAtom "Dict"]
  test1FormCoder (IsPrimType    DaoCharType) [DaoAtom "type==", DaoAtom "Char"]
  test1FormCoder (AnyPrimType $ plainList $ DaoVoidType :| [DaoIntType])
    [DaoAtom "type==", daoList $ atoms "Void Int"]
  test1FormCoder AnyExpr [DaoAtom "any"]
  test1FormCoder (TestExpr typf) [DaoAtom "$", DaoForm typf]
  -- GenPatternUnit ExprPatUnit
  let anyOfTyp = plainList $ DaoFloatType :| [DaoCharType]
  let testGenUnits =
        [ (CheckOnce, DaoAtom "*", DaoAtom "?")
        , (CheckLazy, DaoAtom "*?", DaoAtom "??")
        , (CheckGreedy, DaoAtom "*!", DaoAtom "?!")
        ] >>= \ (k, ka, kb) ->
        [ ( Single AnyExpr, [DaoAtom "any"])
        , ( Single (TestExpr typf), [DaoAtom "$", DaoForm typf])
        , ( AtMost 2 5 k AnyExpr
          , [DaoAtom "any", ka, DaoInt 2, DaoInt 5, DaoAtom "!"]
          )
        , ( AtMost 3 6 k $ TestExpr typf
          , [DaoAtom "$", DaoForm typf, ka, DaoInt 3, DaoInt 6, DaoAtom "!"]
          )
        , ( Single (IsPrimType DaoVoidType)
          , [DaoAtom "type==", DaoAtom "Void"]
          )
        , ( Single (AnyPrimType anyOfTyp)
          , [DaoAtom "type==", daoList (atoms "Flo Char")]
          )
        , ( ZeroOrOne k $ IsPrimType DaoVoidType
          , [DaoAtom "type==", DaoAtom "Void", kb]
          )
        , ( ZeroOrOne k $ AnyPrimType anyOfTyp
          , [DaoAtom "type==", daoList (atoms "Flo Char"), kb]
          )
        , ( AtLeast 123 k $ IsPrimType DaoVoidType
          , [DaoAtom "type==", DaoAtom "Void", ka, DaoInt 123]
          )
        , ( AtLeast 0 k $ AnyPrimType anyOfTyp
          , [DaoAtom "type==", daoList (atoms "Flo Char"), ka, DaoInt 0]
          )
        , ( AtMost 123 4567 k $ IsPrimType DaoVoidType
          , [DaoAtom "type==", DaoAtom "Void", ka, DaoInt 123, DaoInt 4567, DaoAtom "!"]
          )
        , ( AtMost 0 3 k $ AnyPrimType anyOfTyp
          , [DaoAtom "type==", daoList (atoms "Flo Char"), ka, DaoInt 0, DaoInt 3, DaoAtom "!"]
          )
        , ( FewerThan 123 4567 k $ IsPrimType DaoVoidType
          , [DaoAtom "type==", DaoAtom "Void", ka, DaoInt 123, DaoInt 4567]
          )
        , ( FewerThan 0 3 k $ AnyPrimType anyOfTyp
          , [DaoAtom "type==", daoList (atoms "Flo Char"), ka, DaoInt 0, DaoInt 3]
          )
        ]
  sequence_ $ fmap (uncurry test1FormCoder) testGenUnits
  let testTyped = testGenUnits >>= \ (obj, expr) ->
        [ (TypedPattern obj typf, expr ++ [DaoColon, DaoForm typf])
        , (UntypedPattern obj, expr)
        ]
  sequence_ $ fmap (uncurry test1FormCoder) testTyped
  let testNamed = testTyped >>= \ (obj, expr) ->
        [ (NamedPattern "varname" obj, DaoColon : DaoAtom "varname" : case obj of
            UntypedPattern{} -> expr
            TypedPattern{}   -> [daoForm expr]
          )
        , (PatConst obj, expr)
        ]
  sequence_ $ fmap (uncurry test1FormCoder) testNamed
  let chunkNamed = \ case
        a:b:c:d:e:f:g:h:i:j:more -> [a] : [b,c] : [d,e,f] : [g,h,i,j] : chunkNamed more
        []                       -> []
        final                    -> [final]
  let seqPats = do
        (patvars, expr) <- unzip <$> chunkNamed testNamed
        case patvars of
          []   -> []
          a:ax -> [(GenPatternSequence $ plainList (a :| ax), intercalate [DaoComma] expr)]
  sequence_ $ fmap (uncurry test1FormCoder) seqPats
  let chunkSeqPats = \ case
        a:b:c:d:more -> [a] : [b,c,d] : chunkSeqPats more
        []           -> []
        final        -> [final]
  let choicePats = do
        (patseqs, expr) <- unzip <$> chunkSeqPats seqPats
        case patseqs of
          []   -> []
          a:ax -> [(GenPatternChoice $ plainList (a :| ax), intercalate [DaoSemi] expr)]
  sequence_ $ fmap (uncurry test1FormCoder) choicePats

----------------------------------------------------------------------------------------------------

testEnv :: Environment
testEnv = newEnvironment $ extendBuiltins
  [ bif "print" $ DaoStrict $ dumpArgs $ lift . daoVoid .
      (filterEvalForms >=> liftIO . Strict.putStrLn . ("(print) " <>) . strInterpolate)
  ]

runEval :: DaoEval DaoExpr -> IO DaoExpr
runEval = evalDaoExprIO testEnv 

eval1 :: DaoExpr -> DaoExpr -> IO ()
eval1 fncall expecting = isosy ("testing evaluator:\n  " ++ show fncall) $ do
  putStrLn $ "(eval) " ++ show fncall
  result <- evalDaoIO testEnv fncall
  putStrLn $ "(result) " ++ show result 
  unless (result == expecting) $ error $ "(expecting) " ++ show expecting

testEvaluator :: IO ()
testEvaluator = do
  eval1 (daoFnCall "print"       [DaoString "Hello, world!"]) DaoVoid
  eval1 (daoFnCall "+"           $ DaoInt <$> [1,2,3,4,5]) (DaoInt 15)
  eval1 (daoFnCall "<="          $ DaoInt <$> [1,2,3,4,5]) DaoTrue
  eval1 (daoFnCall ">="          $ DaoInt <$> [5,4,3,2,1]) DaoTrue
  eval1 (daoFnCall "+"           $ DaoInt <$> [1,2,3,4,0]) (DaoInt 10)
  eval1 (daoFnCall "<="          $ DaoInt <$> [1,2,3,4,0]) DaoNull
  eval1 (daoFnCall ">="          $ DaoInt <$> [4,3,2,1,5]) DaoNull
  eval1 (daoFnCall "interpolate" $ DaoString <$> ["Hello", ",", "world!"])
        (DaoString "Hello,world!")
  flip eval1 DaoVoid $ daoFnCall "print" $
    [ DaoString "\n  [1,2,3,4,5] -> sum = "
    , daoFnCall "+"  $ DaoInt <$> [1,2,3,4,5]
    , DaoString ", increasing? = "        
    , daoFnCall "<=" $ DaoInt <$> [1,2,3,4,5]
    , DaoString "\n  [1,2,3,4,0] -> sum = "
    , daoFnCall "+"  $ DaoInt <$> [1,2,3,4,0]
    , DaoString ", increasing? = "        
    , daoFnCall "<=" $ DaoInt <$> [1,2,3,4,0]
    , DaoString "\n  (interpolate Hello, world!) = "
    , daoFnCall "interpolate" $ DaoString <$> ["Hello", ",", "world!"]
    ]
  let wdict = plainDict [("one", DaoInt 1), ("two", DaoInt 2), ("three", DaoInt 3)]
  let wlist = plainList $ DaoString "zero" :| (DaoString <$> ["one", "two", "three"])
  eval1 (daoFnCall "get" [DaoAtom "two", DaoDict wdict]) (DaoInt 2)
  eval1 (daoFnCall "get" [DaoInt 1, DaoList wlist]) (DaoString "one")
  eval1 (daoFnCall "put" [DaoAtom "four", DaoInt 4, DaoDict wdict])
        (DaoDict $ insertDict wdict [("four", DaoInt 4)])
  eval1 (daoFnCall "put" [DaoInt 2, DaoString "TWO", DaoList wlist])
        (DaoList $ insertList wlist [(2, DaoString "TWO")])
  eval1 (daoFnCall "get" [daoList [DaoAtom "two", DaoAtom "one"], DaoDict wdict])
        (daoList [DaoInt 2, DaoInt 1])
  eval1 (daoFnCall "get" [daoList [DaoInt 1, DaoInt 2], DaoList wlist])
        (daoList [DaoString "one", DaoString "two"])
  eval1 ( daoFnCall "put"
            [ daoList [DaoColon, DaoString "four", DaoInt 4, DaoColon, DaoString "five", DaoInt 5]
            , DaoDict wdict
            ]
        )
        (DaoDict $ insertDict wdict [("four", DaoInt 4), ("five", DaoInt 5)])
  eval1 ( daoFnCall "put"
            [ daoList [DaoColon, DaoInt 2, DaoString "TWO", DaoColon, DaoInt 3, DaoString "THREE"]
            , DaoList wlist
            ]
        )
        (DaoList $ insertList wlist [(2, DaoString "TWO"), (3, DaoString "THREE")])

match1
  :: Show pat
  => (pat -> RuleMatcher DaoExpr) -> pat
  -> [DaoExpr] -> (MatchResult DaoExpr -> Maybe Error) -> IO ()
match1 execPat pat expr check = isosy ("testing pattern match: " ++ show pat) $ do
  putStrLn $ "(match) " ++ showList expr "\n(against) " ++ show pat
  result <- runEval $ do
    result <- runRuleMatcher (execPat pat) expr
    case check result of
      Just err -> throw err
      Nothing  -> return $ DaoString $ Strict.pack $
        "( Match success, got expected result:\n    " ++ show result ++ "\n)"
  case result of
    DaoError  err -> throw err
    DaoString str -> Strict.putStrLn str
    result        -> print result

expectOK :: DaoExpr -> MatchResult DaoExpr -> Maybe Error
expectOK expect result = case result of
  MatchFail err -> Just err
  MatchQuit err -> Just err
  MatchOK value -> if expect == value then Nothing else Just $ plainError "test-suite"
    [("expecting", dao $ MatchOK expect), ("result", dao result)]

expectQuit :: Atom -> MatchResult DaoExpr -> Maybe Error
expectQuit clas result = case result of
  MatchQuit err -> if clas == getErrorClass err then Nothing else Just err
  MatchFail err -> Just err
  MatchOK   {}  -> Just $ plainError "test-suite"
    [("expecting", daoError clas []), ("result", dao result)]

expectFail :: Atom -> MatchResult DaoExpr -> Maybe Error
expectFail clas result = case result of
  MatchFail err -> if clas == getErrorClass err then Nothing else Just err
  MatchQuit err -> Just err
  MatchOK   {}  -> Just $ plainError "test-suite"
    [("expecting", daoError clas []), ("result", dao result)]

testPatterns :: IO ()
testPatterns = do
  match1 matchExprPatUnit AnyExpr
    [DaoAtom "Success!"] (expectOK $ DaoAtom "Success!")
  match1 matchExprPatUnit AnyExpr
    [] (expectQuit "matching")
  match1 matchExprPatUnit (EqExpr $ DaoAtom "Success!")
    [DaoAtom "Success!"] (expectOK $ DaoAtom "Success!")
  match1 matchExprPatUnit (EqExpr $ DaoAtom "Success!")
    [DaoString "Shouldn't match!"] (expectQuit "pattern-match")
  match1 matchExprPatUnit (EqExpr $ DaoAtom "Success!")
    [] (expectQuit "matching")
  let fiveInts = DaoInt <$> [1,2,3,4,5]
  match1 matchGenPatternUnit' (AtLeast 3 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectOK $ daoList fiveInts)
  match1 matchGenPatternUnit' (AtLeast 5 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectOK $ daoList fiveInts)
  match1 matchGenPatternUnit' (AtLeast 6 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectQuit "matching")
  match1 matchGenPatternUnit' (AtLeast 3 CheckOnce $ IsPrimType DaoStringType)
    fiveInts (expectQuit "pattern-match")
  match1 matchGenPatternUnit' (AtLeast 3 CheckOnce $ IsPrimType DaoStringType)
    fiveInts (expectQuit "pattern-match")
  match1 matchGenPatternUnit' (AtLeast 0 CheckOnce $ IsPrimType DaoStringType)
    fiveInts (expectOK DaoNull)
  match1 matchGenPatternUnit' (FewerThan 0 3 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectOK $ daoList $ DaoInt <$> [1,2,3])
  match1 matchGenPatternUnit' (FewerThan 0 3 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectOK $ daoList $ DaoInt <$> [1,2,3])
  match1 matchGenPatternUnit' (FewerThan 0 3 CheckOnce $ IsPrimType DaoStringType)
    fiveInts (expectOK DaoNull)
  match1 matchGenPatternUnit' (FewerThan 1 3 CheckOnce $ IsPrimType DaoStringType)
    fiveInts (expectQuit "pattern-match")
  match1 matchGenPatternUnit' (FewerThan 6 7 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectQuit "matching")
  match1 matchGenPatternUnit' (FewerThan 5 7 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectOK $ daoList fiveInts)
  match1 matchGenPatternUnit' (AtMost 0 5 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectOK $ daoList fiveInts)
  match1 matchGenPatternUnit' (AtMost 0 4 CheckOnce $ IsPrimType DaoIntType)
    fiveInts (expectQuit "matching")
  match1 (flip matchTypedPattern return)
    ( TypedPattern (FewerThan 0 5 CheckOnce $ IsPrimType DaoIntType)
        (plainFnCall "?" [DaoAtom "<="])
    ) fiveInts (expectOK $ daoList fiveInts)
  match1 (flip matchTypedPattern return)
    ( TypedPattern (FewerThan 0 5 CheckOnce $ IsPrimType DaoIntType)
        (plainFnCall "?" [DaoAtom ">="])
    ) fiveInts (expectQuit "predicate")

----------------------------------------------------------------------------------------------------

usage :: String
usage = unlines
  [ "<General Parameters>"
  , ""
  , "  -?  --help :"
  , "     Print this document and exit immediately."
  , ""
  , "  -i  --immediate :"
  , "     Disable all other tests and run only \"immediate\" tests"
  , ""
  , "  -N <path> --new-config=<path> :"
  , "     Create a new default configuration file at the given path,"
  , "     overwriting any existing configuration file without asking,"
  , "     and then exit immediately. If <path> is \".\", the default"
  , "     configuration file is created in the currrent directory."
  , "" 
  , "<Configuration file parameters>"
  , ""
  , "  -c <path> --config=<path> :"
  , "     Will read parameters from configuration file at the given path"
  , "     and proceed with testing."
  , ""
  , "  (The default config file path is "++show defaultTestConfigPath++")"
  , ""
  , "<Enabling/Disabling Tests>"
  , ""
  , "  * Command line parameters override settings in the configuration file."
  , ""
  , "  * If the default configuration file does not exist, all tests are"
  , "    enabled by default."
  , ""
  , "  --parsers  --no-parsers : enable/disable parsing tests"
  , "  --coders   --no-coders  : enable/disable coding tests"
  ]

main :: IO ()
main = do
  (help,   args) <- getArgs >>= checkCommandLineFlag (Just '?') "help"
  (newcfg, args) <- checkCommandLineParam 'N' "new-config" args
  help   <- case help of
    Just True -> hPutStrLn stderr usage >> return True
    _         -> return False
  newcfg <- case newcfg of
    Just path -> do
      path <- pure $ if path == "." || path == "./" then defaultTestConfigPath else path
      writeFile path (show defaultTestConfig)
      return True
    _         -> return False
  unless (help || newcfg) $ do
    (customConfig, args) <- checkCommandLineParam 'c' "config" args
    let configPath = maybe defaultTestConfigPath id customConfig
    configExists <- doesFileExist configPath
    configured   <- if configExists
     then readsPrec 0 <$> readFile configPath >>= \ case
      [(config, str)] | dropWhile isSpace str == "" -> return config
      _ -> fail $ "could not parse config file at path " ++ show configPath
     else return defaultTestConfig
    (doImmed  ,  args) <- checkCommandLineFlag (Just 'i') "immediate" args
    (doParsers,  args) <- checkCommandLineFlag Nothing    "parsers"   args
    (doCoders , _args) <- checkCommandLineFlag Nothing    "coders"    args
    configured <- pure $ configured
      { only_immediate_tests  = maybe id const doImmed   $ only_immediate_tests configured
      , do_parser_tests       = maybe id const doParsers $ do_parser_tests      configured
      , do_coder_tests        = maybe id const doCoders  $ do_coder_tests       configured
      , do_eval_tests         = maybe id const doCoders  $ do_eval_tests        configured
      , do_pattern_tests      = maybe id const doCoders  $ do_pattern_tests     configured
      }
    let doTest lbl test f = if test configured
          then report ("\n<<<< " ++ lbl ++ " Tests >>>>\n") >> f
          else report ("(Configured to Skip " ++ lbl ++ " Tests.)")
    if only_immediate_tests configured
     then do
      report "\n<<<< IMMEDIATE TESTING MODE >>>>\n(All other tests are now disabled.)\n\n"
      immediate
     else do
      doTest "Parser"    do_parser_tests   testParsers
      doTest "Coders"    do_coder_tests    testCoders
      doTest "Evaluator" do_eval_tests     testEvaluator
      doTest "Patterns"  do_pattern_tests  testPatterns

----------------------------------------------------------------------------------------------------

-- | This test allows you to quickly write an independent test case directly into the source code
-- right here, and enable this test in the configuration file, or by the command line parameters.
-- When this test is enabled, all other tests are disabled, allowing you to focus on a single
-- problem.
immediate :: IO ()
immediate = do
  ---------------------------------------------------------------------------------------------- o

  -- TEST FAILED on function test1FormCoder (type== Void : (call some function) *!)
  -- testPatterns
  test1FormCoder
    (ZeroOrOne CheckOnce $ IsPrimType DaoVoidType)
    [DaoAtom "type==", DaoAtom "Void", DaoAtom "?"]

  ---------------------------------------------------------------------------------------------- o
  return ()

