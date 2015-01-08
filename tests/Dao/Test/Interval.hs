module Dao.Test.Interval where

import           Dao.Check
import qualified Dao.Interval as Iv

import           Control.Applicative

import           Data.Bits
import           Data.Ix
import           Data.Typeable
import qualified Data.Text as Strict
import           Data.Word

tenSeconds :: Int
tenSeconds = 10000000

----------------------------------------------------------------------------------------------------

-- | A new integer data type is created where there are only eight possible values, [0..7]. This
-- data type is used to construct 'Dao.Interval.Interval' 'Dao.Interval.Set's that is isomorphic to
-- the 'Data.Word.Word8' data type. It then becomes possible to test operations on
-- 'Dao.Interval.Set's by comparing the result of the set operation to the result of the isomprohic
-- bitwise operation on the 'Data.Word.Word8' data type.
newtype I = I { w2Int :: Int } deriving Typeable

instance Eq I where { (I i) == (I j) = i == j }

instance Ord I where { compare (I i) (I j) = compare i j }

instance Enum I where
  fromEnum   = w2Int
  toEnum  i  = if 0<=i && i<=7 then I i else error ("(toEnum :: Int -> I) ("++show i++")")
  succ (I i) = I (if i >= 7 then error ("succ (I "++show i++") :: I") else i+1)
  pred (I i) = I (if i <= 0 then error ("pred (I "++show i++") :: I") else i-1)

instance Bounded I where { minBound = I 0; maxBound = I 7 }

instance Iv.InfBound I where { minBoundInf = Iv.Finite minBound; maxBoundInf = Iv.Finite maxBound }

instance Show I where { show (I i) = take 0x3FFFF $ "I "++show i }

instance Read I where { readsPrec p str = map (\ (i, str) -> (I i, str)) (readsPrec p str) }

instance Ix I where
  range (a, b) = if a>b then range (b, a) else
    let loop a = a : if a==b then [] else loop (succ a) in loop a
  inRange (a, b) i = a <= i && i <= b
  index (a, b) i = if a>b then index (b, a) i else
    if inRange (a, b) i then fromEnum i - fromEnum a else
      error ("Bad index "++show i++" for range "++show (a, b))
  rangeSize (a, b) = if a<b then fromEnum b - fromEnum a else rangeSize (b, a)

-- | Converts an 'I' value to a 'Data.Word.Word8' value by evaluating 2 to the power of the integer
-- value of 'I'. This creates a 'Data.Word.Word8', or an octet of bits with only one element in the
-- octet set, and that element is a distance from the start of the octet that is equivalent to the
-- interval distance 'I' would have if it were a member of a 'Dao.Interval.Set'.
i2byte :: I -> Word8
i2byte (I i) = setBit 0 i

----------------------------------------------------------------------------------------------------

-- Defines a new data type that wraps a 'Data.Word.Word8' value, but is intended to be used as a
-- data type that is isomophic to 'Dao.Interval.Set's containing 'I' 'Dao.Interval.Interval's.
newtype ISet = ISet { wSet2Word8 :: Word8 }

instance Eq ISet where { (ISet i) == (ISet j) = i == j }

instance Ord ISet where { compare (ISet i) (ISet j) = compare i j }

instance Num ISet where
  (ISet i) + (ISet j) = ISet (i .|. j)
  (ISet i) - (ISet j) = ISet (i .&. complement j)
  (ISet i) * (ISet j) = ISet (i .&. j)
  negate (ISet i)  = ISet (complement i)
  abs (ISet i) = ISet (abs i)
  signum (ISet i) = ISet (signum i)
  fromInteger i
    | 0 <= i && i < 8 = ISet (toEnum (fromIntegral i))
    | otherwise       = error "fromInteger :: Integer -> ISet"

instance Bits ISet where
  (ISet i) .&. (ISet j) = ISet (i .&. j)
  (ISet i) .|. (ISet j) = ISet (i .|. j)
  bit n = ISet (bit n)
  testBit (ISet i) n = testBit i n
  xor (ISet i) (ISet j) = ISet (xor i j)
  popCount (ISet i) = popCount i
  complement (ISet i) = ISet (complement i)
  shiftR (ISet i) n = ISet (max (shiftR i n) 1)
  shiftL (ISet i) n = ISet (max (shiftL i n) 0x80)
  rotateR (ISet i) n = ISet (rotateR i n)
  rotateL (ISet i) n = ISet (rotateL i n)
  bitSize (ISet i) = bitSize i
  bitSizeMaybe (ISet i) = bitSizeMaybe i
  isSigned (ISet i) = isSigned i

instance Bounded ISet where { minBound = ISet 0x00 ; maxBound = ISet 0xFF }

instance Show ISet where { show (ISet i) = take 0x3FFFF $ "ISet "++show8Bits i }

show8Bits :: Word8 -> String
show8Bits w = [0..7] >>= \j -> let i = 7-j in if testBit w i then show i else "_"

everyI :: [I]
everyI = toEnum <$> [0..7]

everyISet :: [ISet]
everyISet = fmap ISet [0..0xFF]

----------------------------------------------------------------------------------------------------

-- | Creates data type of 'Dao.Interval.Set's of 'Dao.Interval.Interval's of 'I's. This data type is
-- isomophric to 'ISet'.
newtype SetOfI = SetOfI { getIntervalSet :: Iv.Set I } deriving (Eq, Typeable)

instance Show SetOfI where { show (SetOfI o) = take 0x3FFFF $ "SetOfI ("++show o++")" }

everySetOfI :: [SetOfI]
everySetOfI = let oi i = [[], [toEnum i]] in do {
  o7 <- oi 7; o6 <- oi 6; o5 <- oi 5; o4 <- oi 4; o3 <- oi 3; o2 <- oi 2; o1 <- oi 1; o0 <- oi 0;
  [SetOfI $ Iv.fromPoints $ concat [o7, o6, o5, o4, o3, o2, o1, o0]]; }

setOfI2Pairs :: SetOfI -> [(I, I)]
setOfI2Pairs (SetOfI o) = Iv.toBoundedPair <$> Iv.toList o

----------------------------------------------------------------------------------------------------

-- | This data type contains an 'ISet' and a 'SetOfI' which should be equivalent to each other.
-- Whether these values are actually equivalent is a predicate that can be checked by
-- 'checkEqISets'. Operations like set inversion, set intersection, set union, set deletion, and
-- set XOR can be defined that use the bitwise operators on the 'ISet' and the isomorphic
-- 'Dao.Interval.Set' operators operate on the 'SetOfI' at the same time. Once the operation has
-- been completed, 'checkEqISets' can be evaluated to check if the two sets are still equivalent.
data EqISets = EqISets ISet SetOfI deriving (Eq, Typeable)

instance Show EqISets where
  show (EqISets i iset) = take 0x3FFFF $ "EqISets ("++show i++" == "++show iset++")"

-- | Checks whether an 'ISet' and a 'SetOfI' contained within a 'EqISets' are indeed equivalent.
-- This is done by iterating over all possible values of 'I', and then testing if 'I' is a member of
-- both 'ISet' and 'SetOfI's. Since an 'ISet' is a 'Data.Word.Word8' value, an 'I' is a member of an
-- 'ISet' if 'i2byte' evaluates to an value that when bitwise-ANDed with the octet contained in the
-- 'ISet' is a non-zero value. The 'I' is a member of the 'SetOfI's if the 'Dao.Interval.member'
-- predicate evaluates to 'Prelude.True' for the 'Dao.Interval.Set' contained within the 'SetOfI's.
checkEqISets :: EqISets -> Bool
checkEqISets (EqISets (ISet a) (SetOfI b)) = and $ do
  i <- everyI
  return $ Iv.member b i == (0 /= i2byte i .&. a)

binOpEqISets :: (Word8 -> Word8 -> Word8) -> (Iv.Set I -> Iv.Set I -> Iv.Set I) -> EqISets -> EqISets -> EqISets
binOpEqISets onBits onSets (EqISets (ISet isA) (SetOfI soiA)) (EqISets (ISet isB) (SetOfI soiB)) =
  EqISets (ISet $ onBits isA isB) (SetOfI $ onSets soiA soiB)

wholeEqISets :: EqISets
wholeEqISets = EqISets (ISet 0xFF) (SetOfI Iv.whole)

emptyEqISets :: EqISets
emptyEqISets = EqISets (ISet 0x00) (SetOfI Iv.empty)

unionEqISets :: EqISets -> EqISets -> EqISets
unionEqISets = binOpEqISets (.|.) (Iv.union)

intersectEqISets :: EqISets -> EqISets -> EqISets
intersectEqISets = binOpEqISets (.&.) (Iv.intersect)

deleteEqISets :: EqISets -> EqISets -> EqISets
deleteEqISets = binOpEqISets (\a b -> xor a (a .&. b)) (Iv.delete)

xorEqISets :: EqISets -> EqISets -> EqISets
xorEqISets = binOpEqISets (\a b -> xor a b) (Iv.exclusive)

invertEqISets :: EqISets -> EqISets
invertEqISets (EqISets (ISet is) (SetOfI soi)) =
  EqISets (ISet $ xor is 0xFF) (SetOfI $ Iv.invert soi)

-- | check 'every_EqISets', make sure all generated tests are internally consistent, that is every
-- 'ISet' generated is indeed equivalent to every 'SetOfI's generated, and that the 'checkEqISets'
-- function can verify that this is true.
testSuite_everyEqISets :: TestSuite EqISets EqISets
testSuite_everyEqISets =
  TestSuite
  { testSuiteParams =
      TestParams
      { timeLimit = 0
      , testLabel = Strict.pack "every ISet should have an equivalent SetOfI's"
      , testRunner = return
      , testCheck = checkEqISets
      , testVerbose = Nothing
      }
  , testSuiteInputs = every_EqISets
  , testSuiteRunner = testAll
  , testSuiteVerbose = Nothing
  , testSuiteShowFailed = Strict.pack . show
  }

-- | Runs the 'testParam_everyEqISets' test parameters with 'every_EqISets' as input.
test_EqISets :: IO Bool
test_EqISets = testSingleThread testSuite_everyEqISets

-- | There are 256 possible 'ISet's and 256 possible equivalient 'SetOfI's.
every_EqISets :: [EqISets]
every_EqISets = uncurry EqISets <$> zip everyISet everySetOfI

isetFromPair :: I -> I -> ISet
isetFromPair lo hi = ISet $ foldl (.|.) 0 $ fmap i2byte $ range (lo, hi)

eqISetsFromPair :: I -> I -> EqISets
eqISetsFromPair lo hi = let p = (lo, hi) in
  EqISets (isetFromPair lo hi) (SetOfI $ Iv.fromPairs [p])

-- | This function generates every possible 'EqISets' where the 'SetOfI' can be constructed from a
-- single 'Dao.Interval.Interval'. These test cases are simpler and easier to check, and should be
-- checked first to weed out any obvious failures.
every_SetFromPair :: [EqISets]
every_SetFromPair = everyI >>= \hi -> [toEnum 0 .. hi] >>= \lo -> [eqISetsFromPair lo hi]

-- | This test basically checks that 'EqISets' and 'checkEqISets' are functioning appropriately.
-- It should be run first.
params_everySetFromPair :: TestParams EqISets EqISets
params_everySetFromPair =
  TestParams
  { timeLimit   = tenSeconds
  , testLabel   = Strict.pack "pre-test"
  , testRunner  = return
  , testCheck   = checkEqISets
  , testVerbose = Just $ \ _ -> return ()
  }

-- | This is the list of every 'EqISet' and it's inverse that is constructed without using
-- 'Dao.Interval.invert', so the 'Dao.Interval.invert' can be tested, as well as other functions,
-- like union and intersection, which should behave predictably when unionining or intersecting
-- disjoint sets. This list only contains 84 elements, so it is great to run tests on these elements
-- first. The purpose of this list of elements is to test if unioning consecutive elements always
-- results in a 'Dao.Interval.Set' with only one 'Dao.Interval.Interval'. This is checked by
-- evaluating 'Dao.Interval.union', calling the 'Dao.Interval.toList', and checking if there is only
-- one element.
every_ConsecutiveISets :: [(EqISets, EqISets)]
every_ConsecutiveISets = do
  mid <- take 7 everyI
  lo  <- [toEnum 0 .. mid]
  hi  <- [succ mid .. toEnum 7]
  [(eqISetsFromPair lo mid,  eqISetsFromPair (succ mid) hi)]

testSuite_ConsecutiveISets :: TestSuite (EqISets, EqISets) EqISets
testSuite_ConsecutiveISets =
  TestSuite
  { testSuiteParams =
      TestParams
      { timeLimit = tenSeconds
      , testLabel = Strict.pack $ unwords $
          [ "two sets consisting single consecutive intervals should union"
          , "to a set consisting of a single consecutive interval"
          ]
      , testRunner = return . uncurry unionEqISets
      , testCheck = \ o@(EqISets _ (SetOfI s)) ->
          checkEqISets o && (case Iv.toList s of { [_] -> True; _ -> False; })
      , testVerbose = Nothing
      }
  , testSuiteInputs = every_ConsecutiveISets
  , testSuiteRunner = testAll
  , testSuiteVerbose = Nothing
  , testSuiteShowFailed = Strict.pack . show
  }

test_ConsecutiveISets :: IO Bool
test_ConsecutiveISets = testSingleThread testSuite_ConsecutiveISets

-- | This is the list of ever 'EqISet' that has an interval starting with @'I' 0@ and is paired with
-- it's inverse. This list contains only 7 elements which can be listed here (using 'ISet's
-- notaion):
-- > [ (_______0, 7654321_)
-- > , (______10, 765432__)
-- > , (_____210, 76543___)
-- > , (____3210, 7654____)
-- > , (___43210, 765_____)
-- > , (__543210, 76______)
-- > , (_6543210, 7_______) ]
-- This list of elements is good for testing properties set operations that operate on sets which
-- are inverses of each other.
every_SplitWholeSet :: [(EqISets, EqISets)]
every_SplitWholeSet =
  take 7 everyI >>= \i ->
    [ ( EqISets
          (isetFromPair (I 0) i)
          (SetOfI $ Iv.fromList [Iv.negInfTo i])
      , EqISets
          (isetFromPair (succ i) (I 7))
          (SetOfI $ Iv.fromList [Iv.toPosInf $ succ i])
      ) ]

-- | Constructs a test suite that can operate on 'every_SplitWholeSet'.
testSuite_everySplitWholeSet
  :: Show output
  => String -> (EqISets -> EqISets -> output) -> (output -> Bool) -> TestSuite (EqISets, EqISets) output
testSuite_everySplitWholeSet msg f check =
  TestSuite
  { testSuiteParams =
      TestParams
      { timeLimit   = tenSeconds
      , testLabel   = Strict.pack msg
      , testRunner  = return . uncurry f
      , testCheck   = check
      , testVerbose = Just $ \ _ -> return ()
      }
  , testSuiteInputs = every_SplitWholeSet
  , testSuiteRunner = testAll
  , testSuiteVerbose = Nothing
  , testSuiteShowFailed = Strict.pack . show
  }

-- | This is very simple tests that can be run right from the beginning to weed out the most
-- obvious mistakes. It will test the 
testSuite_union_intersect_SplitWholeSet :: [TestSuite (EqISets, EqISets) EqISets]
testSuite_union_intersect_SplitWholeSet = let o = testSuite_everySplitWholeSet in
  [ o "union of split whole sets should be the infinite set" unionEqISets (==wholeEqISets)
  , o "intersection of split whole should be the empty set" intersectEqISets (==emptyEqISets)
  ]

-- | This is very simple tests that can be run right from the beginning to weed out the most
-- obvious mistakes.
testSuite_delete_invert_SplitWholeSet :: [TestSuite (EqISets, EqISets) (EqISets, EqISets)]
testSuite_delete_invert_SplitWholeSet = let o = testSuite_everySplitWholeSet in
  [ o "deletion of left split whole sets with right whole set should equal the left whole set"
      (\a b -> (deleteEqISets a b, a)) (uncurry (==))
  , o "inversion of left whole set should equal the right whole set"
      (\a b -> (invertEqISets a, b)) (uncurry (==))
  ]

-- | Runs 'testSuite_union_intersect_everySplitWholeSet' and
-- 'testSuite_delete_invert_everySplitWholeSet'.
test_SplitWholeSet :: IO Bool
test_SplitWholeSet = fmap (and . concat) $ sequence $
  [ mapM testSingleThread testSuite_union_intersect_SplitWholeSet
  , mapM testSingleThread testSuite_delete_invert_SplitWholeSet
  ]

-- | This is the set of all possible pairs of 'EqISets'. Since there are 256 possible 'EqISets',
-- there are @256^2 == 65536@ pairs in the power set.
powerSet_EqISets :: [(EqISets, EqISets)]
powerSet_EqISets = (,) <$> every_EqISets <*> every_EqISets

testSuite_binaryOperations :: Char -> (EqISets -> EqISets -> EqISets) -> [TestSuite (EqISets, EqISets) EqISets]
testSuite_binaryOperations c op =
  (\ (lbl, op) ->
    TestSuite
    { testSuiteParams =
        TestParams
        { timeLimit = tenSeconds
        , testLabel = Strict.pack $
            "testing ("++lbl++") for the power set (A x B) of all possible interval sets"
        , testRunner = return . uncurry op
        , testCheck  = checkEqISets
        , testVerbose = Nothing
        }
    , testSuiteInputs = powerSet_EqISets
    , testSuiteRunner = testUntilFail
    , testSuiteVerbose = Nothing
    , testSuiteShowFailed = Strict.pack . show
    }
  ) <$>
  [ ("A "++c:" B" , \a b -> op a b)
  , ("A "++c:" !B", \a b -> op (invertEqISets a) b)
  , ("!A "++c:" B", \a b -> op a (invertEqISets b))
  ]

testSuite_unionOperations :: [TestSuite (EqISets, EqISets) EqISets]
testSuite_unionOperations = testSuite_binaryOperations '|' unionEqISets

testSuite_intersectionOperations :: [TestSuite (EqISets, EqISets) EqISets]
testSuite_intersectionOperations = testSuite_binaryOperations '&' intersectEqISets

testSuite_deletionOperations :: [TestSuite (EqISets, EqISets) EqISets]
testSuite_deletionOperations = testSuite_binaryOperations '-' deleteEqISets

testSuite_xorOperations :: [TestSuite (EqISets, EqISets) EqISets]
testSuite_xorOperations = testSuite_binaryOperations '^' xorEqISets

test_allBinarySetOperations :: IO Bool
test_allBinarySetOperations = testMultiThread $ concat $
  [ testSuite_unionOperations   , testSuite_intersectionOperations
  , testSuite_deletionOperations, testSuite_xorOperations
  ]

runTests :: IO Bool
runTests = let and_then a b = a >>= \ok -> if ok then b else return False in
  test_EqISets `and_then` test_ConsecutiveISets `and_then` test_SplitWholeSet `and_then`
  test_allBinarySetOperations

