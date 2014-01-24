-- "tests/Dao/CoreTest.hs"  unit testing of the fundamental Dao
-- scripting language, related intermediate data structures, and
-- serialization.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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

module Dao.CoreTests where

import           Dao.Test
import           Dao.String
import           Dao.Token
import           Dao.Predicate
import           Dao.PPrint
import           Dao.Parser  hiding (isEOF)
import           Dao.Random
import qualified Dao.Binary        as D
import           Dao.Object
import           Dao.Object.Parser

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error

import           Data.Monoid
import qualified Data.ByteString.Lazy as B

import           System.IO

----------------------------------------------------------------------------------------------------

type UnitEnv   = MethodTable
data UnitStats
  = UnitStats
    { maxNodeWeight   :: Int
    , nodeWeightCount :: Int
    , nodeWeightSum   :: Int
    }

instance HasNullValue UnitStats where
  nullValue = UnitStats{ maxNodeWeight=0, nodeWeightCount=0, nodeWeightSum=0 }
  testNull (UnitStats{ maxNodeWeight=a, nodeWeightCount=b, nodeWeightSum=c }) = a==0 && b==0 && c==0

-- | See 'unitTester' in this module for detailed comments.
type DaoLangUnit    = GUnitTester UnitConfig UnitEnv UnitStats TestCase TestResult
type DaoLangTest    = GTest       UnitConfig UnitEnv UnitStats TestCase TestResult
type DaoLangResultM = GResultM    UnitConfig UnitEnv UnitStats TestCase TestResult

data UnitConfig
  = UnitConfig
    { doTestParser      :: Bool
      -- ^ Enable testing of the "Dao.Parser","Dao.Object.Parser", "Dao.PPrintM", and
      -- "Dao.Object.PPrintM" modules. The default value is 'Prelude.True'.
    , doTestSerializer  :: Bool
      -- ^ Enable testing of the "Dao.Object.Binary" module. The default value is 'Prelude.True'.
--  , doTestStructizer  :: Bool
      -- ^ Enable testing of the "Dao.Struct" and "Dao.Object.Struct" module. The default value is
      -- 'Prelude.True'.
    , maxRecurseDepth   :: Int
    -- ^ The "Dao.Random" module generates objects within objects recursively. This parameter sets
    -- the maximum recursion depth of objects within objects. The functions within "Dao.Random"
    -- will generate only null objects at a certain depth to guarantee the this recursion limit is
    -- not exceeded. The default value is 5.
    }
    deriving (Show, Read)

unitConfig :: UnitConfig
unitConfig =
  UnitConfig
  { doTestParser     = True
  , doTestSerializer = True
--  , doTestStructizer = True
  , maxRecurseDepth  = 5
  }

data TestCase
  = TestCase
    { testCaseID      :: Integer
    , randGenStats    :: Int
    , testObject      :: RandObj
    , parseString     :: Maybe UStr
    , serializedBytes :: Maybe B.ByteString
--    , treeStructure   :: Maybe T_tree
    }
instance Show TestCase where
  show (TestCase{testCaseID=i, testObject=o}) = unlines $ concat $
    [ ["TestID #"++show i]
    , case o of
        RandObject   _ -> ["Random Object:", prettyShow o]
        RandTopLevel _ -> ["Random Top-Level Expression:", prettyShow o]
    ]

data TestResult
  = TestResult
    { testCase         :: TestCase
    , testResult       :: UStr
    , failedTestCount  :: Int
    , getParserResult  :: (Bool, Maybe RandObj)
    , getDecodedResult :: (Bool, Maybe RandObj)
--  , getConstrResult  :: (Bool, Maybe RandObj)
    }
-- Lens-like accessors for 'TestResult'
data TRLens a
  = TRLens
    { trLensGetter :: TestResult -> a
    , trLensSetter :: a -> TestResult -> TestResult
    }

parsing :: TRLens (Bool, Maybe RandObj)
parsing = TRLens getParserResult  (\a r -> r{getParserResult=a})

serializing :: TRLens (Bool, Maybe RandObj)
serializing = TRLens getDecodedResult (\a r -> r{getDecodedResult=a})

--structuring :: TRLens (Bool, Maybe RandObj)
--structuring = TRLens getConstrResult  (\a r -> r{getConstrResult=a})

setResult :: TRLens a -> (a -> a) -> DaoLangResultM ()
setResult on f = modify (\r -> trLensSetter on (f $ trLensGetter on r) r)

instance Show TestResult where
  show (TestResult
        { testCase =
            TestCase
            { testCaseID      = i
            , testObject      = orig
            , parseString     = ppr
            , serializedBytes = enc
--            , treeStructure   = tree
            }
        , testResult       = msg
        , getParserResult  = parsd
        , getDecodedResult = decod
--      , getConstrResult  = struct
        }) =
    unlines $ concat $ do
      let o msg1 src prin msg2 (passed, item) = do
            guard (not passed)
            concat $
              [ [sep]
              , maybe [] (\src -> ["### original "++msg1++" ###", prin src]) src, [""]
              , maybe [] (\o -> ["### "++msg2++" object ###", show o, "", prettyShow o]) item, [""]
              ]
      [ ["Test #"++show i++' ':uchars msg, show orig, ""]
        , o "pretty printed form:" ppr  uchars                "parsed"        parsd
        , o "encoded bytes:"       enc  (show . Base16String) "decoded"       decod
--      , o "tree structured:"     tree prettyShow            "reconstructed" struct
        ]

getMethodTable :: DaoLangTest MethodTable
getMethodTable = asksUnitEnv id

-- Every 'TestCase' stores information as to whether or not the case has passed or failed. Use this
-- function to update the 'TestCase' with information indicating that the test failed, including a
-- failure message string. This function does not evaluate to 'Control.Monad.mzero' or
-- 'Control.Monad.fail', either of those functions should be evaluated after this if you wish to
-- halt testing.
testFailed :: TRLens (Bool, Maybe a) -> String -> DaoLangResultM ()
testFailed lens msg = do
  setResult lens (\ (_, m) -> (False, m))
  modify $ \r ->
   r{ testResult      = testResult r <> toUStr msg
    , failedTestCount = failedTestCount r + 1
    }

-- This is just @'Prelude.return' ()@ by another name, but it is helpful to indicate where a test
-- passes.
testPassed :: DaoLangResultM ()
testPassed = return ()

-- Every test is wrapped in this error handler (which calls to 'errHandler' above), and the test is
-- considered a failure if an 'Control.Exception.Error' or 'Control.Exception.IOException' is
-- caught. Pass a test case, a selector for selecting the object generated (Maybe) by 'newTestCase',
-- and a function for checking the value of that generated object. If the object was not generated,
-- the 'TestCase' is returned unmodified. If an exception is thrown, the test case is returned with
-- a failure indicated.
tryTest :: (TestCase -> Maybe item) -> (item -> DaoLangResultM ()) -> DaoLangResultM ()
tryTest getTestItem testFunc = get >>= \r -> maybe (return ()) testFunc (getTestItem $ testCase r)

-- newtype GResultM c e s t r a = ResultM { resultMtoStateT :: PredicateT (StateT TestResult (GTest c e s t r)) a }
--  deriving (Functor, Applicative, MonadPlus, MonadIO)

-- | A 'UnitTester' for the Dao language. This does not merely test the parser, it also tests the pretty
-- printer, the binary encoder/decoder, and the tree encoder/decoder, testing the full abstract
-- syntax tree, semantics tree, and related encodings of these trees. Because the random syntax
-- generator in the "Dao.Object.Random" module is used to generate tests, this module is also
-- tested.
unitTester :: DaoLangUnit
unitTester =
  UnitTester
  { readConfig     = readIO
  , showConfig     = show
  , configFilePath = "./test.cfg"
  , checkConfig    = maybe (return unitConfig) $ \o -> do
      let ifReportEnabled msg fn = hPutStrLn stderr $ unwords $
            [if fn o then "(ENABLED)" else "(DISABLED)", msg, "Testing"]
      ifReportEnabled "Parser"        doTestParser
      ifReportEnabled "Serialization" doTestSerializer
--    ifReportEnabled "Structure"     doTestStructizer
      if doTestParser o || doTestSerializer o -- || doTestStructizer o
      then return o
      else fail "Test configuration has disabled all test categories."
  , showResult     = \ r -> return (show r)
  , combineStats   = \ r st -> do
      let w = randGenStats (testCase r)
      return $
        st{ nodeWeightCount = nodeWeightCount st + 1
          , maxNodeWeight   = max w (maxNodeWeight st)
          , nodeWeightSum   = nodeWeightSum st + w
          }
  , newEnvironment  = return . const mempty
  , newStatistics   = \ _ -> return nullValue
  , showStatistics  = \ ustats -> unlines $
      [ "max_node_weight     = "++show (maxNodeWeight ustats)
      , "average_node_weight = "++
          show (fromRational $
            toRational (nodeWeightSum ustats) / toRational (nodeWeightCount ustats) :: Float)
      ]
  , showTest       = Just show
  , newResult      = return $
      TestResult
      { testCase     = error "test result data not initialized with test case"
      , testResult   = nil
      , failedTestCount  = 0
      , getParserResult  = (True, Nothing)
      , getDecodedResult = (True, Nothing)
--    , getConstrResult  = (True, Nothing)
      }
  --------------------------------------------------------------------------------------------------

  , generateTest    = \i -> do
      mtab <- getMethodTable
      cfg  <- asksUnitConfig id
      let maxDepth      = maxRecurseDepth cfg
      --let (o, weight)   = genRandWeighted maxDepth (fromIntegral i)
      (o, weight) <- liftIO $ genRandWeighted maxDepth (fromIntegral i)
      let str           = prettyShow o
      let bin           = D.encode mtab o
--    let tree          = toDaoStruct o
      let setup isSet o = if isSet cfg then Just o else Nothing
      deepseq o $! seq weight $! return $!
        TestCase
        { testCaseID      = i
        , randGenStats    = weight
        , testObject      = o
        , parseString     = setup doTestParser (ustr str)
        , serializedBytes = setup doTestSerializer bin
--        , treeStructure   = setup doTestStructizer tree
        }
  --------------------------------------------------------------------------------------------------

  , evaluateTest   = \tc -> do
      ------------------- (0) Canonicalize the original object ------------------
      modify (\r->r{testCase=tc})
      tc <- case testObject tc of
        RandTopLevel o -> case canonicalize o of
          [o] -> return $ tc{testObject = RandTopLevel o}
          [ ] -> fail "could not canonicalize original object"
          _   -> fail "original object canonicalized to multiple possible values"
        RandObject   _ -> return tc
      --
      --------------------------- (1) Test the parser ---------------------------
      tryTest parseString $ \str -> do
        let withPredicate p fn = case p of
              Backtrack -> testFailed parsing $ unlines ["Parser backtracked", uchars str]
              PFail   b -> testFailed parsing $ unlines ["Parser failed", show b, uchars str]
              OK      a -> fn a
        case testObject tc of
          RandObject  _orig ->
            -- For ordinary Objects, we only test if the pretty-printed code can be parsed.
            withPredicate
              (parse (daoGrammar{mainParser=assignment}) mempty (uchars str))
              (const testPassed)
          RandTopLevel orig -> withPredicate (parse daoGrammar mempty (uchars str)) $ \o -> do
            -- For AST objects, we also test if the data structure parsed is identical to the data
            -- structure that was pretty-printed.
            let ~diro  = do -- clean up the parsed input a bit
                  o <- directives o
                  case o of
                    AST_TopComment [] -> []
                    o                 -> [delLocation o]
            if diro == [orig]
              then testPassed
              else do
                testFailed parsing "Parsed AST does not match original object"
                setResult parsing $
                  (const (False, if null diro then Nothing else Just $ RandTopLevel $ head diro))
      --
      ------------------------- (2) Test the serializer -------------------------
      tryTest serializedBytes $ \binObj -> do
        mtab  <- testInResultM getMethodTable
        unbin <- catchToResultM "binary deserialization" (return $! Right $! D.decode mtab binObj)
          `catchError` (\err -> return $ Left (show err))
        case unbin of
          Left  msg   -> testFailed serializing (uchars msg)
          Right unbin -> case unbin of
            Backtrack -> testFailed serializing "Decoder backtracked"
            PFail err -> testFailed serializing ("Decoder failed: "++show err)
            OK unbin | unbin == testObject tc -> testPassed
            OK unbin | otherwise -> do
              testFailed serializing "Original object does not match object deserialized from binary string"
              setResult serializing (\ (b, _) -> (b, Just unbin))
      --
--    ---------------- (3) Test the intermediate tree structures ----------------
--    tryTest treeStructure $ \o ->
--      case structToData o :: Predicate UpdateErr RandObj of
--        Backtrack -> testFailed structuring $
--          "Backtracked while constructing a Haskell object from a Dao tree"
--        PFail err -> testFailed structuring $ unlines [show err, prettyShow o]
--        OK struct ->
--          if testObject tc == struct
--            then do
--              setResult structuring (const $ (False, Just struct))
--              testFailed structuring $ unlines $
--                [ "Original object does not match Haskell object constructed from it's Dao tree"
--                , prettyShow struct
--                ]
--            else  testPassed
--    --
      gets failedTestCount >>= guard . (0==) -- mzero if the number of failed tests is more than zero
  }

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
      _   -> fail $ concat $
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
        _   -> fail $ concat $
          [ "deserializing intermediate object to AST evaluated"
          , " to mulitple ambiguous data structures"
          ]
    , fmap RandObject D.get
    ]
instance NFData RandObj where
  rnf (RandTopLevel a) = deepseq a ()
  rnf (RandObject   a) = deepseq a ()

