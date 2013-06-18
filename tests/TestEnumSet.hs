-- | This test suite tests the "Dao.Es.Set" algorithms by creating a new integer type 'I' which has
-- only 8 values, [0,1,2,3,4,5,6,7], and creating a new set type 'ISet' which is simply a
-- 'Data.Word.Word8', but it is treated as a set that can contain all possible 'I' values.
--
-- The "Dao.Es.Set.Es.Set' type containing 'I' values should behave exactly like an 'ISet'. A
-- function for producing every possible equation of the form
-- @(neg1 i1) op1 (neg2 i2) op2 (neg3 i3) op3 ...  (negN iN)@
-- generates equations where the @op1, op2, op3...@ are 'Dao.Es.Set.Es.unionWith' with
-- 'Data.Bits.(.|.), Es.intersectWith with 'Data.Bits.(.&.)', or 'Es.deleteWith' with the equation
--  @\a b -> a 'Data.Bits.(.&.)' 'Data.Bits.complement' b@. The terms
--  @i1, i2, --  i3...@ are values of both types 'ISet' and the equivalent
--  @'Dao.Es.Set.Es.Set' 'I'@, and the operators @neg1, neg2, neg3, ..., negN@ are either
--  'Prelude.id', or are 'Data.Bits.complement' with 'Data.Es.Set.Es.invert'.
--  The equations are then evaluated using their respective operators, order of operations is
--  preserved, set negate is of a prescedence lower than set intersect, but greater than set union.
--  
--  Once each equation is evaluated, the resulting 'Dao.Es.Set.Es.Set' is "converted" to an
--  'ISet'; the 'Dao.Enum.Es.Set' is checked against every possible 'I' value to see if that 'I' is
--  in the set, and the 'I's that are in the set are then unioned into an 'ISet'. If the 'ISet'
--  produced from the 'Dao.Es.Set.Es.Set' is identical to the 'ISet' evaluated by the same
--  equation, then the test passes. If not, there is a bug in the implementation of "Dao.Es.Set".

import qualified Dao.EnumSet as Es
import           Dao.Tests.I
import           Dao.Tests.EQN

import           Data.List
import           Data.Time.Clock

import           Control.Monad
import           Control.Concurrent
import           Control.Exception

import           System.IO
import           Numeric

seg (a,b) = Es.segment (I a) (I b)

setInvertTest :: Es.Set I -> IO (Es.Set I)
setInvertTest ax0 = do
  -- The 'Es.invert' function has been "exploded" to this series of steps, where we can see the
  -- result of each step. Use this function to see how the 'Es.invert' function works.
  let prinlst msg ax = putStrLn $ intercalate "\n" $
        (msg++":") :
          map (\ (i, a) -> show i++": "++show a) (zip (iterate (+1) 0) (Es.toList ax))
  prinlst "original" ax0
  let ax1 = Es.invert ax0
  prinlst "inverted" ax1
  return ax1

pretest :: IO ()
pretest = do
  putStrLn "--------- Pre-Test ---------"
  let term w x = uncurry TERM (iSetAndEnumSet (I w, I x))
      eqn = [term 0 1, OR, term 2 3, OR, term 4 5, AND, term 4 7]
      (b, _) = evalEqn eqn
      es = map (results . iSetAndEnumSet) (zip (map I [0,1,2,3,4]) (map I [3,4,5,6,7]))
  putStrLn "* test iPair2ISet"
  mapM_ (\ii -> putStrLn (show ii++" "++show (iPair2ISet ii))) allPairs
  putStrLn "* test evalEqn"
  putStrLn (showEqn eqn)
  print b
  putStrLn "* test enumSet2ISet"
  mapM_ (\ (i, e) -> putStrLn (show i++" -> "++show e)) es
  putStrLn "* test segmentNub"
  let perms = permutations [seg(0,0), seg(1,1), seg(3,4), seg(6,7)]
      pairs = map (\p -> (p, Es.segmentNub p)) perms
      f x y = compare (Es.toBoundedPair x) (Es.toBoundedPair y)
      ax    = nub (map (sortBy f . snd) pairs)
  mapM_ (\ (a, b) -> putStrLn (show a++" -> "++show b)) pairs
  if length ax /= 1
    then error ("ERROR: received conflicting results for segmentNub\n"++intercalate "\n" (map show ax))
    else putStrLn ("segmentNub result: "++show ax)
  putStrLn "* test Es.invert"
  let invTest1 = ([seg(0,1), seg(3,3), seg(5,5), seg(7,7)] , [seg(2,2), seg(4,4), seg(6,6)])
      invTest2 = ([seg(0,1), seg(3,4), seg(6,7)]           , [seg(2,2), seg(5,5)]          )
      invTest3 = ([seg(2,2), seg(5,7)]                     , [seg(0,1), seg(3,4)]          )
      invTest4 = ([seg(0,2), seg(5,5)]                     , [seg(3,4), seg(6,7)]          )
      invTest5 = ([seg(0,0), seg(3,3), seg(5,7)]           , [seg(1,2), seg(4,4)]          )
      check a b = if a==b then return () else error ("was expecting: "++show b)
      run (a,b) = do
        inv <- setInvertTest (Es.fromList a)
        check inv (Es.fromList b)
        inv <- setInvertTest inv
        check inv (Es.fromList a)
  run invTest1 >> run invTest2 >> run invTest3 >> run invTest4 >> run invTest5
  putStrLn "\n---- Pre-Test Completed ----\n"

-- | The length of an generated equation, as in the number of terms. 4 terms means a + b + c + d,
-- three terms means a + b + c.
eqlen = 4

numberOfTests :: Rational
numberOfTests = toRational $
    2^(eqlen-1)  -- number of possible inversions
  * 3^(eqlen-1)  -- number of possible operators
  * 36^eqlen -- number of possible sets, there are 36 possible 'ISet's for every term.

test :: IO ()
test = do
  hSetBuffering stderr LineBuffering
  mv <- newMVar (0, [], False)
  ch <- newEmptyMVar
  qs <- newEmptyMVar -- actually is used as a semaphore
  mainThread <- myThreadId
  let exceptionHandler (SomeException e) = print e
  startTime <- getCurrentTime
  timed <- forkIO (forever (threadDelay 5000000 >> putMVar qs ()))
  let genEquation fn = handle exceptionHandler $ do
        forM_ (genEquations 4) (\ input@(eqnNum, eqn) -> seq eqnNum $! seq eqn $! fn input)
        modifyMVar_ mv (\ (a, b, _) -> return (a, b, True))
        putMVar qs ()
  let compute (eqnNum, eqn) = handle exceptionHandler $ do
        let c@(_, s) = evalEqn eqn
            r@(a, b) = results c
        if uncurry (==) r
          then  modifyMVar_ mv $ \ (_,a,b) -> return (eqnNum,a,b)
          else  modifyMVar_ mv $ \ (_, errx, _) ->
                  return (eqnNum, errx++[(eqnNum, eqn, a, b)], False)
        -- if mod eqnNum 128 == 0 then signalQSem qs else return ()
  let multiThreaded = do
        forkIO (forever (takeMVar ch >>= evaluate >>= compute))
        forkIO (genEquation (\ (eqnNum, eqn) -> putMVar ch (eqnNum, eqn) >> yield))
  let singleThreaded = forkIO (genEquation compute)
  let runTestsLoop = do
        takeMVar qs
        (count, errs, done) <- modifyMVar mv (\ (a,b,c) -> return ((a,[],c), (a,b,c)))
        unless (null errs) (mapM_ (putStrLn . showResults) errs)
        now <- getCurrentTime
        let totalTime   = diffUTCTime now startTime
            rate        = round (toRational count / toRational totalTime)
            remain      = round ((numberOfTests - toRational count) / toRational rate)
            days        = div remain (24*60*60)
            daysS       = days*24*60*60
            hours       = div (remain - daysS) (60*60)
            hoursS      = daysS + hours*60*60
            minutes     = div (remain - hoursS) 60
            minutesS    = hoursS + minutes*60
            seconds     = remain - minutesS
            rem msg val = if val>0 then [show val++' ':msg] else []
            percent     = showFFloat (Just 2) (fromRat (toRational count * 100 / numberOfTests)) ""
        hPutStr stderr $ concat $
          [ "\ncompleted ", show count, " out of ", show (truncate numberOfTests)
          , " tests (", percent, "%)\n"
          , "have worked for ", show totalTime, "\n"
          , "averaging ", show rate, " tests per second\n"
          , "approximating ", show remain, " seconds remaining\n"
          , "("
          , intercalate ", " $
              rem "days" days++rem "hours" hours++rem "minutes" minutes++rem "seconds" seconds
          , ")\n"
          ]
        if done then return () else runTestsLoop
  singleThreaded
  runTestsLoop

main = pretest >> test

