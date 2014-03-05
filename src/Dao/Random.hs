-- "src/Dao/Random.hs"  generates objects from integers that can be used
-- to test the parsers/pretty-printer, and the binary encoder/decoder.
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

-- | Hints for making good random object generators:
-- Avoid using scramble more than once per constructor. If you have a data type constructed with:
-- > return DataType <*> 'scrambO' <*> 'scrambO'
-- then just do this instead
-- > 'scramble' $ return DataType <*> 'randO' <*> 'randO'
-- 
-- Do not bother preceding generators for newtype data types with 'recurse' or 'countNode', it isn't
-- really necessary.
-- 
-- Recursive data types containing lists with 'randList' should probably generate small lists for
-- 'randO' instances, and generate larger lists for 'defaultO' instances.
-- 
-- When instantiating 'randO', use only 'randO' to fill-in all the objects contained in the object.
-- When instantiating 'defaultO', use only 'defaultO' to fill-in all the objects contained in the
-- object.
--
-- When instantiating 'defaultO', it is OK to use 'defaultO' to fill-in other non-recursive data
-- types, but /BE VERY CAREFUL/ not to call 'defaultO' for another non-recursive type.
module Dao.Random where

import           Dao.String
import qualified Dao.Tree as T

import           Control.Exception
import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad.State

import           Data.Monoid
import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Time
import           Data.Array.IArray
import qualified Data.ByteString.Char8 as B

import           System.Random
import           System.IO

----------------------------------------------------------------------------------------------------

-- | A simple, stateful monad for generating arbitrary data types based on pseudo-random numbers
-- without lifting the @IO@ or @ST@ monads, i.e. it can be evaluated in a pure way.
newtype RandT m a = RandT { runRandT :: StateT RandOState m a }
  deriving (Functor, Applicative, Monad, MonadPlus)

instance MonadTrans RandT where { lift = RandT . lift }
instance MonadIO m => MonadIO (RandT m) where { liftIO = RandT . liftIO }

type RandO a = RandT IO a

data RandOState
  = RandOState
    { integerState :: Integer
    , stdGenState  :: StdGen
    , nodeCounter  :: Int
    , subDepthLim  :: Int
    , subDepth     :: Int
    , maxDepth     :: Int
    , traceLevel   :: Int
    }

-- | Initializes the 'RandOState' with two integer values: a maximium depth value (limits the number
-- of times you can recursively call 'limSubRandO') and the seed value passed to
-- 'System.Random.mkStdGen'.
initRandOState :: Int -> Int -> RandOState
initRandOState maxDepth seed =
  RandOState
  { integerState = fromIntegral seed
  , stdGenState  = mkStdGen seed
  , nodeCounter  = 0     
  , subDepthLim  = maxDepth
  , subDepth     = 0
  , maxDepth     = 0
  , traceLevel   = 0
  }

-- | Increment the internal node counter of the random generator state. This is good for measuring
-- the "weight" of randomly generated objects. /NOTE:/ do not use this if you also start your
-- 'randO' instance with 'recurse', because 'recurse' also calls this function.
countNode_ :: RandO Int
countNode_ = do
  i' <- RandT $ gets nodeCounter
  let i = i' + 1
  RandT $ modify (\st -> st{nodeCounter=i})
  return i

-- | Algorithmically identical 'countNode_' but its function type is such that it can be written like so:
-- > 'countNode' $ do ...
-- whereas the 'countNode' function must be written like so:
-- > do { 'countNode_'; ... }
-- or
-- > countNode_ >> ...
-- /NOTE:/ do not use this if you also start your 'randO' instance with 'recurse', because 'recurse'
-- also calls this function.
countNode :: RandO a -> RandO a
countNode fn = countNode_ >> fn

newtype RandChoice o = RandChoice{ getChoiceArray :: Maybe (Array Int (RandO o)) }
instance Functor RandChoice where
  fmap f (RandChoice arr) = RandChoice $ fmap (fmap (fmap f)) arr
instance Monoid (RandChoice o) where
  mempty = RandChoice Nothing
  mappend (RandChoice a) (RandChoice b) = RandChoice $ msum
    [ do  (loA, hiA) <- fmap bounds a
          (loB, hiB) <- fmap bounds b
          listA <- fmap elems a
          listB <- fmap elems b
          return $ listArray (min loA loB, max hiA hiB) (listA++listB)
    , a, b
    ]

-- | Similar to monadic bind, allows you to create a new 'RandChoice' by using the value produced by
-- another 'RandChoice'.
bindRandChoice :: RandChoice o -> (o -> RandChoice p) -> RandChoice p
bindRandChoice (RandChoice arr) f = RandChoice $ fmap (fmap (\o -> o >>= runRandChoiceOf . f)) arr

-- | Instantiate your data types into this class if you can generate arbitrary objects from random
-- numbers using the 'RandO' monad. Minimal complete definition is one of either 'randO' or
-- 'randChoice', and one of either 'defaultO' or 'defaultChoice'.
class HasRandGen o where
  -- | This is the function used to generate a random object of a data type that only has one
  -- constructor. You must define either this or 'randChoice' or both. The 'randChoice' will be
  -- defined as @('randChoiceList' ['randO'])@.
  randO :: RandO o
  randO = runRandChoiceOf randChoice
  -- | This is the function used to generate a random object of a data type that has multiple
  -- constructors. Use 'randChoiceList' to build a list of 'RandO' data types, each item producing
  -- an object with a different constructor. You must define either this or 'randO' or both. The
  -- 'randO' will be defined using @('runChoiceList' 'randChoice')@ by default.
  randChoice :: RandChoice o
  randChoice = randChoiceList [randO]
  -- | The 'randO' and/or 'randChoice' functions can be defined without any restrictions, they can
  -- even be called recursively. But since we are working with randomness, recursion may produce
  -- arbitrarily large objects which may consume all memory and crash the program. The 'recurse'
  -- function can be used to count the depth of the recursive data type constructed and to limit the
  -- depth, and when the limit is reached a non-recursive default value should be constructed
  -- instead. This is the function that should produce the non-recursive data.
  --
  -- Keep in mind that it is impossible to enforce whether or not the data generated by any function
  -- will be recursive or not unless the data type itself is inherently not recursive. So it is the
  -- programmers responsiblity to understand how to use this function. This function must terminate,
  -- it is your responsibility to see that it does, it is your responsability to make sure you never
  -- call a recursive function within this function.
  --
  -- For data types that are inherenly not recursive, for example types instantiating
  -- 'Prelude.Enum', this function may safely be defined by calling 'randO'. For recursive data
  -- types, if the data type instantiates 'Data.Monoid.Monoid', consider returning
  -- 'Data.Monoid.mempty'.
  -- 
  -- Either this function or 'defaultChoice' must be defined.
  defaultO :: RandO o
  defaultO = runRandChoiceOf defaultChoice
  -- | This function is to 'defaultO' what 'randChoice' is to 'randO': it lets you construct a
  -- random object from a list of choices, but like 'defaultO' every choice provided here must NOT
  -- be a recursive function.
  defaultChoice :: RandChoice o
  defaultChoice = randChoiceList [defaultO]

runRandChoiceOf :: RandChoice o -> RandO o
runRandChoiceOf (RandChoice{ getChoiceArray=arr }) = case arr of
  Nothing  -> fail "null RandChoice"
  Just arr -> let (lo, hi) = bounds arr in join $ ((arr!) . (lo+)) <$> nextInt (hi-lo)

runRandChoice :: HasRandGen o => RandO o
runRandChoice = runRandChoiceOf randChoice

randChoiceList :: forall o . [RandO o] -> RandChoice o
randChoiceList items = RandChoice{ getChoiceArray = Just arr } where
  len = length items
  arr :: Array Int (RandO o)
  arr = listArray (0, len) items

instance HasRandGen ()       where { randO = return (); defaultO = randO; }
instance HasRandGen Int      where { randO = randInt; defaultO = randO; }
instance HasRandGen Integer  where { randO = fmap fromIntegral randInt; defaultO = randO; }
instance HasRandGen Char     where { randO = fmap chr randInt; defaultO = randO }
instance HasRandGen Word64   where { randO = fromIntegral <$> (randO::RandO Int); defaultO = randO; }
instance HasRandGen Rational where
  randO = return (%) <*> defaultO <*> ((+1) <$> defaultO)
  defaultO = randO
instance HasRandGen Double   where { randO = fromRational <$> randO; defaultO = randO; }
instance HasRandGen UTCTime  where
  randO = do
    day <- fmap (ModifiedJulianDay . unsign . flip mod 73000) randInt
    sec <- fmap (fromRational . toRational . flip mod 86400) randInt
    return (UTCTime{utctDay=day, utctDayTime=sec})
  defaultO = randO
instance HasRandGen NominalDiffTime where
  randO = randInteger (fromRational 0) $ \i -> do
    div <- randInt
    fmap (fromRational . (% fromIntegral div) . longFromInts) (replicateM (mod i 2 + 1) randInt)
  defaultO = randO
instance HasRandGen Name where { randO = fmap (fromUStr . randUStr) randInt; defaultO = randO; }
instance HasRandGen UStr where
  randO = fmap (ustr . unwords . fmap (uchars . toUStr)) (randList 0 9 :: RandO [Name])
  defaultO = randO
instance HasRandGen Bool where { randO = fmap (0/=) (nextInt 2); defaultO = randO; }
instance HasRandGen a => HasRandGen (Maybe a) where
  randO = randO >>= \n -> if n then return Nothing else fmap Just randO
  defaultO = return Nothing

instance (Ord p, HasRandGen p, HasRandGen o) => HasRandGen (T.Tree p o) where
  defaultO = return T.Void
  randO = recurse $ do
    branchCount <- nextInt 4
    cuts <- fmap (map (+1) . randToBase 6) randInt
    fmap (T.fromList . concat) $ replicateM (branchCount+1) $ do
      wx <- replicateM 6 randO
      forM cuts $ \cut -> do
        obj <- randO
        return (take cut wx, obj)

-- | Construct a value from an 'Prelude.Int'. Actually, you have a 50/50 chance of drawing a zero,
-- but this is because zeros are used often for you data type.
randInteger :: a -> (Int -> RandO a) -> RandO a
randInteger zero mkOther = do
  i <- randInt
  let (x, r) = divMod i 2
  if r==0 then return zero else mkOther x

-- | Generate a random object given a maximum recursion limit, a seed value, and a 'RandO' generator
-- function. The weight (meaning the number of calls to 'countNode', 'countNode_', or 'recurse') of
-- the generated item is also returned.
genRandWeightedWith :: RandO a -> Int -> Int -> IO (a, Int)
genRandWeightedWith (RandT gen) maxDepth seed =
  fmap (fmap nodeCounter) $ runStateT gen (initRandOState maxDepth seed)

-- | This function you probably will care most about. does the work of evaluating the
-- 'Control.Monad.State.evalState' function with a 'RandOState' defined by the same two parameters
-- you would pass to 'initRandOState'. In other words, arbitrary random values for any data type @a@
-- that instantates 'HasRandGen' can be generated using two integer values passed to this function.
genRandWeighted :: HasRandGen a => Int -> Int -> IO (a, Int)
genRandWeighted maxDepth seed = genRandWeightedWith randO maxDepth seed

-- | Like 'genRandWeightedWith' but the weight value is ignored, only being evaluated to the random
-- object,
genRandWith :: RandO a -> Int -> Int -> IO a
genRandWith gen maxDepth seed = fmap fst $ genRandWeightedWith gen maxDepth seed

-- | Like 'genRandWeightedWith' but the weight value is ignored, only being evaluated to the random
-- object.
genRand :: HasRandGen a => Int -> Int -> IO a
genRand maxDepth seed = genRandWith randO maxDepth seed

randTrace :: MonadIO m => String -> RandT m a -> RandT m a
randTrace msg rand = do
  trlev <- RandT $ gets traceLevel
  nc <- RandT $ gets nodeCounter
  sd <- RandT $ gets subDepth
  let prin msg = liftIO $ do
        hPutStrLn stderr (replicate trlev ' ' ++ msg)
        hFlush stderr >>= evaluate
  () <- prin $ "begin "++msg++" c="++show nc++" d="++show sd
  RandT $ modify $ \st -> st{traceLevel=trlev+1}
  a  <- rand >>= liftIO . evaluate
  RandT $ modify $ \st -> st{traceLevel=trlev}
  () <- prin $ "end   "++msg
  return a

-- | Take another integer from the seed value. Provide a maximum value, the pseudo-random integer
-- returned will be the seed value modulo this maximum value (so passing 0 will result in a
-- divide-by-zero exception, passing 1 will always return 0). The seed value is then updated with
-- the result of this division. For example, if the seed value is 10023, and you pass 10 to this
-- function, the result returned will be 3, and the new seed value will be 1002.
--    Using numbers generated from this seed value is very useful for generating objects that are
-- somewhat predictable, but the contents of which are otherwise unpredictable. For example, if you
-- want to generate random functions but always with the names "a", "b", or "c", like so:
-- > a(...), b(...), c(...)
-- where the arguments to these functions can be arbitrary, then have your function generator
-- generate the names of these functions using 'nextInt' like so:
-- > 'Prelude.fmap' ('Prelude.flip' 'Prelude.lookup ('Prelude.zip' "abc" [0,1,2]))'nextInt' 3
-- then the arguments to these functions can be generated using 'randInt'. The names of the
-- functions will be predictable for your seed values: any seed value divisible by 3 will generate a
-- function named "a", but the arguments will be arbitrary because they were generated by 'randInt'.
nextInt :: Int -> RandO Int
nextInt maxval = if abs maxval==1 || maxval==0 then return 0 else do
  st <- RandT $ get
  let (i, rem) = divMod (integerState st) (fromIntegral (abs maxval))
  RandT $ put $ st{integerState=i}
  return (fromIntegral rem)

-- | Generate a random integer from the pseudo-random number generator.
randInt :: RandO Int
randInt = RandT $
  state (\st -> let (i, gen) = next (stdGenState st) in (i, st{stdGenState=gen}))

-- | Mark a recursion point, also increments the 'nodeCounter'. The recusion depth limit set when
-- evaluating a 'randO' computation will not be exceeded.  When the number of 'recurse' functions
-- called without returning has reached this limit and this function is evaluated again, the given
-- 'RandO' generator will not be evaluated, the default value will be returned.
recurse :: HasRandGen a => RandO a -> RandO a
recurse fn = do
  st <- RandT get
  if subDepth st > subDepthLim st
    then defaultO
    else do
      countNode_
      i <- (+1) <$> (RandT $ gets subDepth)
      RandT $ modify (\st -> st{subDepth = i, maxDepth = max (maxDepth st) i})
      a <- fn
      RandT $ modify (\st -> st{subDepth = subDepth st - 1})
      return a

-- | The 'nextInt' function lets you derive objects from a non-random seed value internal to the
-- state of the 'RandT' monad. This is useful for random objects that have multiple constructors,
-- and you want to generate one of every constructor by simply initializing the random seed with
-- incrementing integers.
--
-- However every instantiation of 'randChoice' makes use of this seed value. Consequently if your
-- data type is composed entirely of objects which all instantiate 'randChoice', every node of the
-- object will be generated by a non-random number. In some cases this is desirable, it allows you
-- to generate every possible object with a large enough sequence of random numbers.
--
-- However when you wish to generate a very random, varied set of random objects, this
-- predictability is not desirable. To get around this, you can use the 'scramble' function. This
-- will use the current seed value to initialize a new child random generator with a child random
-- seed, and the provided 'RandT' function will be evaluated with in this child environment. After
-- evaluation is complete, the parent seed is restored. Since the child random seed is derived from the
-- parent seed, you are still guaranteed to always generate the same object from the same seed value,
-- but the child object generated will be much more varied and less predictable.
-- 
-- So instead of generating a child node of your object with ordinary 'randO', use 'scrambO' (which
-- is equivalent to @('scramble' 'randO')@ and this will make your objects more varied, even for
-- predictable input.
scramble :: RandO a -> RandO a
scramble fn = do
  newGen  <- randInt
  oldst   <- RandT get
  RandT (put $ oldst{ stdGenState=mkStdGen newGen })
  let wrap x = toInteger (fromIntegral x :: Word)
  x <- wrap <$> randInt
  y <- wrap <$> randInt
  RandT (modify $ \st -> st{ integerState = x*(1 + (toInteger (maxBound::Word))) + y })
  a <- fn
  RandT (modify $ \st -> st{ integerState=integerState oldst, stdGenState=stdGenState oldst })
  return a

-- | This function is defined simply as @('scramble' 'randO')@, but I expect it to be used often
-- enough it warrants it's own function name.
scrambO :: HasRandGen a => RandO a
scrambO = scramble randO

-- | The number of unique values a 'Prelude.Int' can be, which is @('Prelude.maxBound'+1)*2@.
intBase :: Integer
intBase = (fromIntegral (maxBound::Int) + 1) * 2

-- | Take an ordinary 'Prelude.Int' and make it unsigned by checking if it is a negative value, and
-- if it is, returning the maximum unsigned value plus the negative value, otherwise returning the
-- positive value unchanged. For example, -1 will return @2*('Prelude.maxBound'+1)-1@ and @+1@ will
-- return @1@.
unsign :: Int -> Integer
unsign i = if i<0 then intBase + fromIntegral i else fromIntegral i

-- | Creates a string of digits from 0 to the given @base@ value by converting a random unsigned
-- integer to the list of digits that represents the random integer in that @base@. For example, if
-- you want a list of digits from 0 to 4 to be produced from a number 54, pass 4 as the base, then
-- the number 54. Each digit of the base-4 number representation of 54 will be returned as a
-- separate integer: @[2,1,3]@ (from lowest to highest place value, where 123 in base 10 would
-- return the list @[3,2,1]@).
randToBase :: Int -> Int -> [Int]
randToBase base i = loop (unsign i)  where
  loop i = if i==0 then [] else let (i' , sym) = divMod i b in fromIntegral sym : loop i'
  b      = fromIntegral base

-- | When generating 'Prelude.Integers' from 'Int's, treat a list of 'Int's as a list of symbols in
-- a base M number, where M is the @('Prelude.maxBound'::'Prelude.Int')@ multiplied by two to allow
-- for every negative number to also be considered a unique symbol.
longFromInts :: [Int] -> Integer
longFromInts = foldl (\a b -> a*intBase + unsign b) 0

randEnum :: (Bounded x, Enum x) => x -> x -> RandO x
randEnum lo hi = fmap toEnum (nextInt (abs (fromEnum lo - fromEnum hi)))

----------------------------------------------------------------------------------------------------

randUStr :: Int -> UStr
randUStr = ustr . B.unpack . getRandomWord

randMultiName :: RandO [UStr]
randMultiName = do
  i0 <- randInt
  let (i1, len) = divMod i0 4
  fmap ((randUStr i1 :) . map randUStr) (replicateM len randInt)

randListOf :: Int -> Int -> RandO a -> RandO [a]
randListOf minlen maxlen rando = do
  -- half of all lists will be null, unless the 'minlen' parameter is greater than 0
  minlen <- return (min minlen maxlen)
  maxlen <- return (max minlen maxlen)
  empt <- if minlen==0 then nextInt 2 else return 0
  if empt==1
    then return []
    else do
      ln <- nextInt (maxlen-minlen)
      replicateM (minlen+ln) rando

randList :: HasRandGen a => Int -> Int -> RandO [a]
randList lo hi = randListOf lo hi randO

defaultList :: HasRandGen a => Int -> Int -> RandO [a]
defaultList lo hi = randListOf lo hi defaultO

randRational :: Int -> RandO Rational
randRational i0 = do
  let (i1, len1) = divMod i0 4
      (_ , len2) = divMod i1 4
  a <- fmap longFromInts (replicateM (len1+1) randInt)
  b <- fmap longFromInts (replicateM (len2+1) randInt)
  return (a%b)

getRandomWord :: Int -> B.ByteString
getRandomWord i = randomWords ! (mod i (rangeSize (bounds randomWords) - 1))

randomWords :: Array Int B.ByteString
randomWords = listArray (0, length list) (map B.pack list) where
  list = words $ unwords $
    [ "a academia accomplished added also an analysis and application applications apply are arent slim"
    , "argument arguments as at avoids be because been behavior between book both by calculus plus were"
    , "calling can change changes code commercial computability computation computer concepts earth was"
    , "constructs contrast conversely declarative definition depending depends describing metal key fee"
    , "designed developed development difference different domains domain easier effects fire water add"
    , "elaborations elements eliminating emphasize entscheidungsproblem eschewing star best least being"
    , "especially evaluation example executing expression facilitate financial formal greatest open etc"
    , "functional has have hope how however imperative industrial input investigate is home close where"
    , "it key lack lambda language languages largely like make many math mathematical may from flow she"
    , "motivations much mutable notion numeric of on one ones only organizations output paradigm pit he"
    , "specific pioneering practice predict produce program programming prominent purely rather trust I"
    , "recursion referential result roots same science side so software some specifically state move me"
    , "statistics style subject such supported symbolic system than that the they child this super mesh"
    , "transparency treats twice understand use used value values variety viewed which wide will bill X"
    , "dates times database structured listing setting dictionary returning throwing catching law factor"
    , "option procedure alpha beta electron proton neutron shift hard soft bean beam fix drug undo minus"
    , "field magic latice jump assemble area volume interesting slice sector region cylinder sphere plan"
    , "inside without trying patterned rules"
    ]

