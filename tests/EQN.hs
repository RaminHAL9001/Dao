module Dao.Tests.EQN where

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

import           Prelude hiding (catch)
import           Data.Bits
import           Data.Word
import           Data.List
import           Data.Maybe

import qualified Dao.EnumSet as Es
import           Dao.Tests.I

import           Control.Monad
import           Control.Concurrent
import           Control.Exception

-- import           Debug.Trace

data EQN = OR | AND | DEL | TERM ISet (Es.Set I) | INV [EQN]

-- | Every possible binary operator for an 'EQN'.
allOps :: [EQN]
allOps = [OR, AND, DEL]

-- | Every possible @(bits, enumSets)@ pair given by 'iPair2ISet' and every inverted 'iPair2ISet'.
allTerms :: [EQN]
allTerms = map (uncurry TERM . iSetAndEnumSet) allPairs

-- | Evaluate an equation.
evalEqn :: [EQN] -> (ISet, Es.Set I)
evalEqn eqn = case scan eqn of
  [INV  q  ] -> let (w, s) = evalEqn q in (complement w, Es.invert s)
  [TERM w s] -> (w, s)
  [term]     -> error ("syntax error: extraneous "++showEqn eqn++" operator")
  []         -> error "cannot compute a null eqution"
  eqn        -> evalEqn eqn
  where
    err eqn = error ("syntax error: "++showEqn eqn)
    scan eqn = (pass or . pass del . pass and) eqn
    pass eval eqn = case eqn of
      (a:op:b:eqn) -> case eval a op b of
        Nothing -> a:op : pass eval (b:eqn)
        Just t  -> pass eval (t:eqn)
      [a]          -> [a]
      _            -> err eqn
    term bitF setF a b = Just (TERM (bitF ab bb) (setF as bs)) where
      (ab, as) = evalEqn [a]
      (bb, bs) = evalEqn [b]
    and a AND b = term (*) Es.intersect a b
    and _ _   _ = Nothing
    del a DEL b = term (-) Es.delete a b
    del _ _   _ = Nothing
    or  a OR  b = term (+) Es.union a b
    or  _ _   _ = Nothing

-- | From the resulting pair of values produced by 'evalEqn', convert the second value to an 'ISet'
-- using 'enumSet2ISet' so the two values can be compared.
results :: (ISet, Es.Set I) -> (ISet, ISet)
results (w, s) = (w, enumSet2ISet s)

showEqn :: [EQN] -> String
showEqn = intercalate " " . map f where
  f eqn = case eqn of
    AND      -> " & "
    OR       -> " | "
    DEL      -> " - "
    INV eqn  -> "!("++showEqn eqn++")"
    TERM w s -> show w ++ show s

-- | Generate all possible equations over @n@ terms.
genEquations :: Int -> [(Word64, [EQN])]
genEquations n = zip (iterate (+1) 0) (loop n) where
  loop n
    | n <= 0 = []
    | n == 1 = map (:[]) allTerms
    | n >  1 = allTerms >>= \a -> allOps >>= \op -> loop (n-1) >>= \b -> [[a, op]++b, [a, op, INV b]]

showResults :: (Word64, [EQN], ISet, ISet) -> String
showResults (eqnNum, eqn, i, e) =
  show eqnNum++": "++showEqn eqn++"\n\tbits "++show i++" doesnt match Es.fromList "++show e

