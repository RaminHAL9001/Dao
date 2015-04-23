-- "Dao/Rule.hs"  Monads for defining "intelligent" programs.
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

-- | This module provides the APIs for constructing a knowledge base of production rules, and for
-- matching expressions against the production rules. This module is really the core of the
-- artificial intelligence functionality. A knowledge base is a collection of facts and production
-- rules which can be queried. Evaluating a query matches the query against all facts and production
-- rules.
--
-- The monads defined here create a kind of Domain Specific Language that operates very similar to
-- the historic programming language PROLOG. The PROLOG programming language was infamously
-- difficult to reason about, but as a domain-specific language in Haskell, you can make use of
-- PROLOG's logic algorithm while using Haskell's clean, type-safe semantics for defining data
-- structures.
--
-- For those not familiar with PROLOG, you should at least be familiar with Haskell's type checking
-- system, which operates similarly to the PROLOG language. Polymorphic types (type variables) are
-- variables that can be matched against concrete types, and concrete types are bound to type
-- variables. The difference between Haskell's type checker and PROLOG is that if a type variable
-- matches two different concrete types in the same expression, this results in an error, whereas
-- PROLOG would backtrack and continue trying to find a set of concrete types that could match the
-- type variables unambiguously.
--
-- Another place you may have seen knowledge base algorithms is in the UNIX @make@ program, where
-- you specify a set of production rules in a file called @Makefile@. The @Makefile@ serves as the
-- knowledge base containing the production rules, while the files and directories in the same
-- directory as the @Makefile@ serve as the "facts" of the knowledge base. Executing @make@ will
-- check the "facts" (the state of the files and directories) by matching them to the production
-- rules, except the matching algorithm ignores files older than the time stamp of the "Makefile",
-- this is how @make@ knows which files have changed and need to re-built. Naturally, there is no
-- such restriction in the "Dao.Rule".
module Dao.Rule
  ( -- * The 'Query' type
    Query, QueryState(QueryState), queryStateTuple,
    querySubState, queryWeight, queryIndex, queryInput,
    -- * Patterns for Matching Tokens
    PatternClass(patternCompare),
    Similarity(Dissimilar, Similar, ExactlyEqual), boolSimilar, similarButNotEqual, isSimilar,
    Dissimilarity(Dissimilarity), getSimilarity,
    -- * Production Rules: the 'Rule' Monad
    Rule, evalRuleLogic, queryAll, query, query1, next, part, remainder, done,
    -- * The Throwable and Catchable Data Type
    RuleError(RuleFail, RuleError), pPrintRuleErrorWith,
    bestMatch, resetScore,
    -- * The Branch Structure of a Rule
    -- $Structure_of_a_rule
    RuleStruct, tree, struct, ruleTree, getRuleStruct, trim, mask,
    -- * Predicting User Input
    PartialQueryResult(PartialQueryResult),
    partialQuery, resumePartialQuery, partialQueryResultTuple, guesses, guessPartial,
    partialQueryResults, partialQueryBranches, partialQueryNextSteps,
    RuleAltEvalPath,
    -- * Re-export the "Dao.Logic" module.
    module Dao.Logic
  )
  where

import           Dao.Array
import           Dao.Lens
import           Dao.Logic
import           Dao.Predicate
import           Dao.Text.PPrint
import           Dao.TestNull
import           Dao.Text
import qualified Dao.Tree as T

import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Reader.Class
import           Control.Monad.Except

import           Data.Dynamic
import           Data.Either
import           Data.List
import           Data.Monoid
import qualified Data.Map as M
import           Data.Ratio

----------------------------------------------------------------------------------------------------

-- | A 'Query' is the array of input tokens. Your program will construct a 'Query' and use 'queryAll',
-- 'query', or 'query1' with a 'Rule' monad to perform a logical computation.
type Query tok = Array tok

-- | The 'Query' a list of tokens each paired with an integer score. When evaluating a
-- 'Rule' monad, the 'queryInput' is tested by various 'Rule' functions to produce a result, and the
-- result is 'Control.Monad.return'ed when all the 'Rule's that can possibly match have matched
-- against the 'queryInput', and all possible results that have been successfully
-- 'Control.Monad.return'ed. A 'queryWeight' is kept, which counts how many token items
-- from the 'queryInput' have matched so far. This 'queryWeight' can be helpful in narrowing the list
-- of potential results.
--
-- The 'QueryState' also contains a 'querySubState', which is a polymorphic value that can be
-- whatever you would like it to be. This state value should be light-weight, or at least easily
-- copied and re-written, because the 'Rule' monad in which the 'QueryState' is used will
-- create many copies of the 'QueryState' (taking advantage of Haskell's lazy copy on write to do it
-- efficiently) as multiple branches of 'Rule' evaluation are explored. In the case of the 'Rule'
-- the 'querySubState' is always @()@, not everyone needs to make use of the state.
newtype QueryState tok st = QueryState (st, Double, Int, Query tok) deriving (Eq, Ord, Show, Typeable)

instance TestNull st => TestNull (QueryState tok st) where
  nullValue = QueryState nullValue
  testNull (QueryState o) = testNull o

queryStateTuple :: Monad m => Lens m (QueryState tok st) (st, Double, Int, Query tok)
queryStateTuple = newLens (\ (QueryState o) -> o) (\o _ -> QueryState o)

-- | Keeps the polymorphic stateful data.
querySubState :: Monad m => Lens m (QueryState tok st) st
querySubState = queryStateTuple >>> tuple0

queryWeight :: Monad m => Lens m (QueryState tok st) Double
queryWeight = queryStateTuple >>> tuple1

queryIndex :: Monad m => Lens m (QueryState tok st) Int
queryIndex = queryStateTuple >>> tuple2

queryInput :: Monad m => Lens m (QueryState tok st) (Query tok)
queryInput = queryStateTuple >>> tuple3

_plus :: (Eq tok, Ord tok, Monad m) => RuleAltEvalPath err tok st m a -> RuleAltEvalPath err tok st m a -> RuleAltEvalPath err tok st m a
_plus = M.unionWith (T.unionWith (\a b q -> mplus (a q) (b q)))

_showQS :: Show tok => QueryState tok st -> String
_showQS (QueryState (_, w, i, qx)) = "(..., " ++
  show ((round (w*1000) % 1000) :: Rational) ++ show i ++ ", " ++ show (elems qx) ++ ")"

----------------------------------------------------------------------------------------------------

-- | This is a fuzzy logic value which must be returned by 'patternCompare' in the 'PatternClass' class.
-- In the 'Rule' monad, 'patternCompare' is used to select a branch of execution, and multiple branch
-- choices may exist. If any branches are 'ExactlyEqual', then only those 'ExactlyEqual' branches
-- will be chosen to continue execution. If there are no 'ExactlyEqual' choices then all 'Similar'
-- branches will be chosen to continue execution. If all branches are 'Dissimilar', execution
-- backtracks.
--
-- 'Similarity' instantiates 'Data.Monoid.Monoid' such that 'Data.Monoid.mappend' will multiply
-- 'Similar' values together.
--
-- ### A note about sorting
--
-- When 'Prelude.Ordering' 'Similarity' values, items that are more 'Similar' are greater than those
-- that are less similar. This is good when you use functions like 'Prelude.maximum', but not so
-- good when you use functions like 'Data.List.sort'. When you 'Data.List.sort' a list of
-- 'Similarity' values, the list will be ordered from least similar to most similar, with the most
-- similar items towards the end of the resultant list. This is usually not what you want. You
-- usually want the most similar items at the 'Prelude.head' of the resultant list. To solve this
-- problem, you can wrap 'Similarity' in a 'Dissimilarity' wrapper and then 'Data.List.sort', or
-- just use @('Data.List.sortBy' ('Prelude.flip' 'Prelude.compare'))@
data Similarity
  = Dissimilar
    -- ^ return this value if two 'Object's are below the threshold of what is considered "similar."
  | Similar Double
    -- ^ return this value if two 'Object's are not equal, but above the threshold of what is
    -- considered "similar." The 'Prelude.Double' value is used to further prioritize which branches
    -- to evaluate first, with values closer to @1.0@ having higher priority. If you are not sure
    -- what value to set, set it to @0.0@; a similarity of @0.0@ is still more similar than
    -- 'Dissimilar'.
  | ExactlyEqual
    -- ^ return this value if two 'Object's are equal such that @('Prelude.==')@ would evaluate to
    -- 'Prelude.True'. *Consider that to be a law:* if @('Prelude.==')@ evaluates to true for two
    -- 'Object's, then 'patternCompare' must evaluate to 'ExactlyEqual' for these two 'Object's. The
    -- 'ExactlyEqual' value may be returned for dissimilar types as well, for example:
    --
    -- * 'Prelude.String's and 'Data.Text.Text's
    -- * 'Prelude.Integer's and 'Prelude.Int's
    -- * 'Prelude.Rational's and 'Prelude.Double's
    -- * lists and 'Dao.Array.Array's
  deriving (Eq, Show, Typeable)

-- | This data type is defined for 'Data.List.sort'ing purposes. Usually you want to sort a list of
-- matches such that the most similar items are towards the 'Prelude.head' of the list, but
-- 'Similarity' is defined such that more similar values are greater than less similar values, and
-- as such 'Data.List.sort'ing 'Similarity' values places the most 'Similar' items towards the
-- 'Prelude.tail' end of the resultant list. Wrapping 'Similarity' values in this data type reverses
-- the 'Prelude.Ordering' so that more 'Dissimilar' items have a higher value and are placed towards
-- the 'Prelude.tail' end of the 'Data.List.sort'ed list.
newtype Dissimilarity = Dissimilarity { getSimilarity :: Similarity }
  deriving (Eq, Show, Typeable)

instance Monoid Similarity where
  mempty = Dissimilar
  mappend a b = case a of
    Dissimilar   -> Dissimilar
    ExactlyEqual -> b
    Similar    a -> case b of
      Dissimilar   -> Dissimilar
      ExactlyEqual -> Similar a
      Similar    b -> Similar $ a*b

instance Ord Similarity where
  compare a b = case a of
    Dissimilar   -> case b of
      Dissimilar   -> EQ
      _            -> LT
    Similar    a -> case b of
      Dissimilar   -> GT
      Similar    b -> compare a b
      ExactlyEqual -> LT
    ExactlyEqual -> case b of
      ExactlyEqual -> EQ
      _            -> LT

instance Ord Dissimilarity where { compare (Dissimilarity a) (Dissimilarity b) = compare b a; }

instance Monoid Dissimilarity where
  mempty = Dissimilarity mempty
  mappend (Dissimilarity a) (Dissimilarity b) = Dissimilarity $ mappend a b

-- | Convert a 'Prelude.Bool' value to a 'Similarity' value. 'Prelude.True' is 'ExactlyEqual', and
-- 'Prelude.False' is 'Dissimilar'.
boolSimilar :: Bool -> Similarity
boolSimilar b = if b then ExactlyEqual else Dissimilar

-- | Evaluates to 'Prelude.True' only if the 'Similarity' value is 'Similar', and not 'ExactlyEqual'
-- or 'Dissimilar'.
similarButNotEqual :: Similarity -> Bool
similarButNotEqual o = case o of { Similar _ -> True; _ -> False; }

-- | Evaluates to 'Prelude.True' if the 'Similarity' valus is not 'Dissimilar', both 'Similar' and
-- 'ExactlyEqual' evaluate to 'Prelude.True'.
isSimilar :: Similarity -> Bool
isSimilar o = case o of { Dissimilar -> False; _ -> True; }

-- | This class allows you to define a fuzzy logic pattern matching predicate for your data type.
-- The data type must take an 'Object' as input and use data of your custom pattern type to decide
-- whether your pattern matches the 'Object'. You usually make use of 'fromObj' to convert the
-- 'Object' to a type which you can actually evaluate.
--
-- The 'Object' data type itself instantiates this class, in which case if 'matchable' has not been
-- defined for the data type stored in the 'Dao.Object.Object', 'patternCompare' evaluates to
-- @('Prelude.==')@
class PatternClass o where { patternCompare :: o -> o -> Similarity; }

----------------------------------------------------------------------------------------------------

-- | An error that occurs at the rule level.  When using 'Control.Monad.Except.catchError' within a
-- 'StatefuleRule' monad, you must catch data of this type.
--
-- It wraps an arbitrary error type which can be customized by users of this module. If all you need
-- is string text messages, just use 'Control.Monad.fail' to throw errors in the 'Rule'
-- monad.
data RuleError err
  = RuleFail  StrictText -- ^ thrown when monadic 'Control.Monad.fail' is evaluated.
  | RuleError err        -- ^ an arbitrary error type that users of this module can customize.
  deriving (Eq, Ord, Typeable)

instance Functor RuleError where
  fmap f o = case o of
    RuleError       o -> RuleError   $ f o
    RuleFail      err -> RuleFail      err

instance PPrintable err => PPrintable (RuleError err) where { pPrint = pPrintRuleErrorWith pPrint; }

instance PPrintable err => Show (RuleError err) where { show = showPPrint 4 80 . pPrint; }

pPrintRuleErrorWith :: (err -> [PPrint]) -> RuleError err -> [PPrint]
pPrintRuleErrorWith prin err = case err of
  RuleError       err -> prin err
  RuleFail        err -> [pText  err]

----------------------------------------------------------------------------------------------------

-- | A 'Rule' is a monadic function that defines the behavior of a production rule in a
-- 'KnowledgeBase'. A 'KnowledgeBase' is queried with a list of tokens being matched against a
-- sequence of this 'Rule' data type.
--
-- When query is matched agains a 'Rule', the query is placed into an internal stateful monad, and
-- as the 'Rule' is evaluated, the query is deconstructed. Evaluating to
-- 'Control.Applicative.empty' or 'Control.Monad.mzero' indicates a non-match and evaluation
-- backtracks. Evaluating 'Control.Monad.return' indicates a success, and the returned
-- token is used as the result of the production.
--
-- The 'Rule' monad instantiates 'Control.Monad.Except.Class.MonadError' such that
-- errors can be thrown and caught. The 'Rule' monad instantiates
-- 'Dao.Logic.MonadLogic' so it is possible to pattern match with many
-- 'Control.Applicative.Alternative' branches of evaluation without having to worry about the
-- current state. 'Control.Monad.State.Class.MonadState' is instantiated giving you total control of
-- the state, along with the 'Dao.Logic.MonadLogic' functions. And
-- 'Control.Monad.Reader.Class.MonadReader' is instantiated so that the
-- 'Control.Monad.Reader.Class.local' function can be used to execute rules with a different input
-- in a different context without altering the current context.
data Rule err tok st (m :: * -> *) a
  = RuleEmpty
  | RuleReturn a
  | RuleThrow (RuleError err)
  | RuleLift  (m (Rule err tok st m a))
  | RuleState (QueryState tok st -> [(Rule err tok st m a, QueryState tok st)])
  | RuleOp        RuleOpCode                      (Rule            err tok st m a)
  | RuleChoice   (Rule            err tok st m a) (Rule            err tok st m a)
  | RuleTree     (RuleAltEvalPath err tok st m a) (RuleAltEvalPath err tok st m a)
    -- DepthFirst and BreadthFirst rule trees are kept separate.

-- Not exported
data RuleOpCode = ResetScore | BestMatch Int deriving (Eq, Show)

instance (Monad m, Eq tok, Ord tok) => Functor (Rule err tok st m) where
  fmap f rule = case rule of
    RuleEmpty      -> RuleEmpty
    RuleReturn   o -> RuleReturn $ f o
    RuleThrow  err -> RuleThrow err
    RuleLift     o -> RuleLift $ liftM (liftM f) o
    RuleState    o -> RuleState $ fmap (fmap (first (liftM f))) o
    RuleOp    op o -> RuleOp op $ liftM f o
    RuleChoice x y -> RuleChoice (liftM f x) (liftM f y)
    RuleTree   x y -> let map = fmap (fmap (fmap (fmap f))) in RuleTree (map x) (map y)

instance (Functor m, Monad m, Eq tok, Ord tok) => Applicative (Rule err tok st m) where { pure = return; (<*>) = ap; }

instance (Functor m, Monad m, Eq tok, Ord tok) => Alternative (Rule err tok st m) where { empty = mzero; (<|>) = mplus; }

instance (Monad m, Eq tok, Ord tok) => Monad (Rule err tok st m) where
  return = RuleReturn
  rule >>= f = case rule of
    RuleEmpty      -> RuleEmpty
    RuleReturn   o -> f o
    RuleThrow  err -> RuleThrow err
    RuleLift     o -> RuleLift $ liftM (>>= f) o
    RuleState    o -> RuleState $ fmap (fmap (first (>>= f))) o
    RuleOp    op o -> RuleOp op $ (o >>= f)
    RuleChoice x y -> RuleChoice (x >>= f) (y >>= f)
    RuleTree   x y -> let map = fmap $ fmap $ fmap (>>= f) in RuleTree (map x) (map y)
  a >> b = case a of
    RuleTree   a1 a2 -> case b of
      RuleEmpty        -> RuleEmpty
      RuleThrow  err   -> RuleThrow err
      RuleTree   b1 b2 ->
        let wrap map = T.Tree (Nothing, map)
            unwrap (T.Tree (_, tree)) = tree
            power a b = unwrap $ T.productWith (>>) (wrap a) (wrap b)
        in  RuleTree (power a1 b1) (power a2 b2)
      b                -> let bind = fmap $ fmap $ fmap (>> b) in RuleTree (bind a1) (bind a2)
    _ -> a >>= \ _ -> b
  fail = RuleThrow . RuleFail . toText

instance (Monad m, Eq tok, Ord tok) => MonadPlus (Rule err tok st m) where
  mzero = RuleEmpty
  mplus a b = case a of
    RuleEmpty        -> b
    RuleTree   a1 a2 -> case b of
      RuleEmpty        -> a
      RuleChoice b1 b2 -> RuleChoice (mplus a b1) b2
      RuleTree   b1 b2 -> RuleTree (_plus a1 b1) (_plus a2 b2)
      b                -> RuleChoice a b
    a                -> case b of
      RuleEmpty        -> a
      b                -> RuleChoice a b

instance (Monad m, Eq tok, Ord tok) => MonadError (RuleError err) (Rule err tok st m) where
  throwError            = RuleThrow
  catchError rule catch = case rule of
    RuleThrow o -> catch o
    _           -> rule

instance (Monad m, Eq tok, Ord tok) => PredicateClass (RuleError err) (Rule err tok st m) where
  predicate o = case o of
    PError err -> throwError err
    PTrue    o -> return o
    PFalse     -> mzero
  returnPredicate f = mplus (catchError (liftM PTrue f) (return . PError)) (return PFalse)

instance (Monad m, Eq tok, Ord tok) => MonadState st (Rule err tok st m) where
  state f = RuleState $ \qs ->
    let (o, st) = f (qs~>querySubState) in [(return o, with qs [querySubState <~ st])]

instance (Monad m, Eq tok, Ord tok) => MonadReader st (Rule err tok st m) where
  ask = get;
  local f sub = state (\st -> (st, f st)) >>= \st -> sub >>= \o -> state $ const (o, st)

instance (Eq tok, Ord tok) => MonadTrans (Rule err tok st) where
  lift f = RuleLift $ liftM return f

instance (MonadIO m, Eq tok, Ord tok) => MonadIO (Rule err tok st m) where { liftIO = RuleLift . liftM return . liftIO; }

instance (MonadFix m, Eq tok, Ord tok) => MonadFix (Rule err tok st m) where { mfix f = RuleLift $ mfix (return . (>>= f)); }

instance (Monad m, Eq tok, Ord tok) => Monoid (Rule err tok st m o) where { mempty=mzero; mappend=mplus; }

----------------------------------------------------------------------------------------------------

_queryState :: (Monad m, Eq tok, Ord tok) => (QueryState tok st -> (a, QueryState tok st)) -> Rule err tok st m a
_queryState = RuleState . fmap (return . first return)

_evalRuleSift
  :: (Monad m, Eq tok, Ord tok, PatternClass tok)
  => Rule err tok st m a
  -> LogicT (QueryState tok st) m ([(RuleError err, QueryState tok st)], [(a, QueryState tok st)])
_evalRuleSift rule = flip liftM (entangle $ evalRuleLogic rule) $ flip execState ([], []) .
  mapM_ (\ (lr, qs) -> modify $ \ (l, r) -> case lr of
            Left err -> (l++[(err, qs)], r)
            Right  o -> (l, r++[(o, qs)])
        )

-- | Evaluate a 'Rule' by flattening it's internal 'Dao.Tree.Tree' structure to a 'Dao.Logic.LogicT'
-- monad. This is probably not as useful of a function as 'queryAll' or 'query'.
evalRuleLogic
  :: forall err st tok m a . (Monad m, Eq tok, Ord tok, PatternClass tok)
  => Rule err tok st m a -> LogicT (QueryState tok st) m (Either (RuleError err) a)
evalRuleLogic rule = case rule of
  RuleEmpty      -> mzero
  RuleReturn   o -> return $ Right o
  RuleThrow  err -> return $ Left err
  RuleLift     o -> lift o >>= evalRuleLogic
  RuleState    f -> do
    ox <- liftM f get >>= mapM (\ (rule, st) -> put st >> evalRuleLogic rule)
    superState $ zip ox . repeat
  RuleOp    op f -> case op of
    ResetScore  -> evalRuleLogic $ do
      n <- _queryState $ \qs -> (qs~>queryWeight, with qs [queryWeight <~ 0])
      f >>= \o -> _queryState (\qs -> (o, with qs [queryWeight <~ n]))
    BestMatch n -> do
      let scor     = (~> queryWeight) . snd
      let sortScor = sortBy (\a b -> scor b `compare` scor a)
      (bads, goods) <- _evalRuleSift f
      superState $ const $ if null goods then first Left <$> bads else
        (if n>0 then take n else id) $ first Right <$> sortScor goods
  RuleChoice x y -> mplus (evalRuleLogic x) (evalRuleLogic y)
  RuleTree   x y -> mplus (runMap T.DepthFirst [] x) (runMap T.BreadthFirst [] y) where
    runMap control qx map = if M.null map then mzero else do
      q <- evalRuleLogic next
      case q of
        Left  q -> return $ Left q
        Right q -> do
          let (equal, similar) = partition ((ExactlyEqual ==) . fst)
                (do (o, tree) <- M.assocs map
                    let result = patternCompare o q
                    if result==Dissimilar then [] else [(result, tree)]
                )
          tree <- superState $ \st -> flip zip (repeat st) $
            if null equal
            then snd <$> sortBy (\a b -> compare (fst b) (fst a)) similar
            else snd <$> equal
          loop control (qx++[q]) tree
    loop control qx tree@(T.Tree (rule, map)) = if T.null tree then mzero else
      (if control==T.DepthFirst then id else flip) mplus
        (runMap control qx map)
        (maybe mzero (evalRuleLogic . ($ qx)) rule)

-- | Run a 'Rule' against a token query given as a value of type @[tok]@, return all successful
-- results and all exceptions that may have been thrown by 'Control.Monad.Except.throwError'.
queryAll
  :: (Monad m, Eq tok, Ord tok, PatternClass tok)
  => Rule err tok st m a -> st -> [tok] -> m [Either (RuleError err) a]
queryAll rule st q =
  runLogicT (evalRuleLogic rule) (QueryState (st, 0.0, 0, array q)) >>= return . fmap fst

-- | Run a 'Rule' against a token query given as a value of type @[tok]@ query, return all
-- successful results.
query
  :: (Monad m, Eq tok, Ord tok, PatternClass tok)
  => Rule err tok st m a -> st -> [tok] -> m [a]
query r st = queryAll r st >=> return . (>>= (const [] ||| return))

-- | Like 'query', but only returns the first successful result, if any.
query1
  :: (Monad m, Eq tok, Ord tok, PatternClass tok)
  => Rule err tok st m a -> st -> [tok] -> m (Maybe a)
query1 r st = query r st >=> \ox -> return $ if null ox then Nothing else Just $ head ox

-- | Take the next item from the query input, backtrack if there is no input remaining.
next :: (Monad m, Eq tok, Ord tok) => Rule err tok st m tok
next = RuleState $ \qs -> case (qs~>queryInput) ! (qs~>queryIndex) of
  Nothing -> []
  Just  o -> [(return o, with qs [queryIndex $= (+ 1)])]

-- | Take as many of next items from the query input as necessary to make the rest of the 'Rule'
-- match the input query. This acts as kind of a Kleene star.
part :: (Monad m, Eq tok, Ord tok) => Rule err tok st m [tok]
part = RuleState $
  let out  keep qs = (return keep, qs)
      loop keep qs = case (qs~>queryInput) ! (qs~>queryIndex) of
        Nothing -> [out keep qs]
        Just  o -> out keep qs : loop (keep++[o]) (with qs [queryIndex $= (+ 1)])
  in  loop []

-- | Clear the remainder of the input 'Query' and return it.
remainder :: (Monad m, Eq tok, Ord tok) => Rule err tok st m [tok]
remainder = RuleState $ \qs ->
  [(return $ elems $ qs~>queryInput, with qs [queryIndex <~ size $ qs~>queryInput])]

-- | Match when there are no more arguments, backtrack if there are.
--
-- This function is polymorphic over a monadic type that instantiates 'Dao.Logic.MonadLogic',
-- however consider the type of this function to be:
--
-- @
-- ('Data.Functor.Functor' m, 'Control.Monad.Monad' m) => 'Rule' m token
-- @
done :: (Monad m, Eq tok, Ord tok) => Rule err tok st m ()
done = RuleState $ \qs -> [(return (), qs) | (qs~>queryIndex) >= size (qs~>queryInput)]

-- | Fully evaluate a 'Rule', and gather all possible results along with their 'queryWeight's. These
-- results are then sorted by their 'queryWeight'. If the 'Prelude.Int' value provided is greater
-- than zero, this integer number of results will selected from the top of the sorted list. If the
-- 'Prelude.Int' value provided is zero or less, all gathered results are sorted and selected. Then
-- 'Rule' evaluation will continue using only the selected results.
bestMatch :: Monad m => Int -> Rule err tok st m a -> Rule err tok st m a
bestMatch i = RuleOp (BestMatch i)

-- | Evaluate a monadic function with the 'queryWeight' reset to zero, and when evaluation of the
-- monadic function completes, set the score back to the value it was before.
resetScore :: Monad m => Rule err tok st m a -> Rule err tok st m a
resetScore = RuleOp ResetScore

----------------------------------------------------------------------------------------------------

-- $Structure_of_a_rule
-- A 'Rule' is constructed from 'Dao.Tree.Tree' data types and functions. Some 'Rule's form empty
-- trees, for example 'Control.Monad.return' or 'Control.Monad.State.state'. However 'Rule's
-- constructed with functions like 'tree' or 'struct' produce a 'Dao.Tree.Tree' structure internal
-- to the 'Rule' function which can be retrieved and manipulated. This is useful for
-- meta-programming 'Rule's, for example predictive input applications.

-- | This is the data type that models the branch structure of a 'Rule'. It is a 'Dao.Tree.Tree'
-- with token paths and @()@ leaves. It is possible to perform modifications to some
-- 'Rule's, for example 'trim'-ing of branches, using a 'RuleStruct'.
type RuleStruct tok = T.Tree tok ()

-- | Take a list of lists of a type of 'Dao.Object.ObjectData' and construct a 'Rule' tree that will
-- match any 'Query' similar to this list of 'Dao.Object.ObjectData' values (using
-- 'Dao.Object.patternCompare'). Every list of 'Dao.Object.ObjectData' will become a branch associated
-- with the given 'Rule' monadic function (constructed using the 'Dao.Tree.fromList' function). This
-- 'Rule' function must take a 'Query' as input. When a portion of a 'Query' matches the given
-- 'Dao.Object.ObjectData', the portion of the 'Query' that matched will be passed to this 'Rule'
-- function when it is evaluated.
tree
  :: (Eq tok, Ord tok, Monad m)
  => T.RunTree -> [[tok]] -> ([tok] -> Rule err tok st m b) -> Rule err tok st m b
tree control branches = struct control $ T.fromList $ zip branches $ repeat ()

-- | Construct a 'Rule' tree from a 'Dao.Tree.Tree' data type and a 'Rule' function. The 'Rule' will
-- be copied to every single 'Dao.Tree.Leaf' in the given 'Dao.Tree.Tree'.
struct
  :: (Monad m, Eq tok, Ord tok)
  => T.RunTree -> RuleStruct tok -> ([tok] -> Rule err tok st m a) -> Rule err tok st m a
struct control tree rule =
  let df = control==T.DepthFirst
      (T.Tree (leaf, map)) = fmap (\ () -> rule) tree 
  in  maybe id ((if df then flip else id) mplus . ($ [])) leaf $
        (if df then id else flip) RuleTree map nullValue

-- | Take a 'Dao.Tree.Tree' and turn it into a 'Rule' where every 'Dao.Tree.leaf' becomes a 'Rule'
-- that simply 'Control.Monad.return's the 'Dao.Tree.leaf' value.
ruleTree :: (Eq tok, Ord tok, Monad m) => T.RunTree -> T.Tree tok a -> Rule err tok st m a
ruleTree control (T.Tree (leaf, map)) = maybe id (flip mplus . return) leaf $
  (if control==T.DepthFirst then flip else id) RuleTree nullValue $
    fmap (fmap (const . return)) map

-- | Remove all of the 'Rule's and return only the 'Dao.Tree.Tree' structure. This function cannot
-- retrieve the entire 'Dao.Tree.Tree', it can only see the 'Dao.Tree.Tree' created by the 'tree'
-- function, or some combination of rules created by the 'tree' function (for example two 'tree's
-- 'Control.Monad.mplus'sed together). 'Rule's created with functions like 'Control.Monad.return',
-- @('Control.Monad.>>=')@, @('Control.Applicative.<*>')@, 'Control.Monad.State.state',
-- 'Control.Monad.Trans.lift', and others all introduce opaque function data types into the leaves
-- which cannot be 'Data.Traversal.traverse'd.
getRuleStruct :: (Eq tok, Ord tok) => Rule err tok st m o -> RuleStruct tok
getRuleStruct rule = case rule of
  RuleEmpty      -> T.empty
  RuleTree   x y -> T.Tree (Nothing, M.union (fmap void x) (fmap void y))
  RuleChoice x y -> T.union (getRuleStruct x) (getRuleStruct y)
  _              -> T.Tree (Just (), M.empty)

-- | With a 'RuleStruct' delete any of the matching branches from the 'Rule' tree. Branch matching
-- uses the @('Prelude.==')@ predicate, not 'Dao.Object.patternCompare'. This is the dual of 'mask' in
-- that @'trim' struct t 'Data.Monoid.<>' 'mask' struct t == t@ is always true.
-- This function works by calling 'Dao.Tree.difference' on the 'Rule' and the 'Dao.Tree.Tree'
-- constructed by the 'Dao.Tree.blankTree' of the given list of 'Dao.Object.ObjectData' branches.
trim :: (Monad m, Eq tok, Ord tok) => RuleStruct tok -> Rule err tok st m a -> Rule err tok st m a
trim tree@(T.Tree (leaf, map)) rule = case rule of
  RuleEmpty      -> RuleEmpty
  RuleTree   x y -> RuleTree (del x) (del y)
  RuleChoice x y -> RuleChoice (trim tree x) (trim tree y)
  rule           -> maybe rule (\ () -> mzero) leaf
  where
    treeDiff a b = let c = T.difference a b in guard (not $ T.null c) >> return c
    del          = flip (M.differenceWith treeDiff) map

-- | With 'RuleStruct' and delete any of the branches from the 'Rule' tree that do *not* match the
-- 'RuleStruct' This is the dual of 'trim' in that
-- @'trim' struct t 'Data.Monoid.<>' 'mask' struct t == t@ is always true. Branch matching uses the
-- @('Prelude.==')@ predicate, not 'Dao.Object.patternCompare'. This function works by calling
-- 'Dao.Tree.intersection' on the 'Rule' and the 'Dao.Tree.Tree' constructed by the
-- 'Dao.Tree.blankTree' of the given list of 'Dao.Object.ObjectData' branches.
mask :: (Monad m, Eq tok, Ord tok) => RuleStruct tok -> Rule err tok st m a -> Rule err tok st m a
mask tree@(T.Tree (leaf, map)) rule = case rule of
  RuleEmpty      -> RuleEmpty
  RuleTree   x y -> RuleTree (del x) (del y)
  RuleChoice x y -> RuleChoice (mask tree x) (mask tree y)
  rule           -> maybe mzero (\ () -> rule) leaf
  where
    del = flip (M.intersectionWith T.intersection) map

----------------------------------------------------------------------------------------------------

-- | This data type contains a tree with a depth of no less than one (hence it is a 'Dao.Tree.Tree'
-- inside of a 'Data.Map.Map', the 'Data.Map.Map' matches the first element of the 'Query') which is
-- used to construct all alternative paths of 'Rule' evaluation.
type RuleAltEvalPath err tok st m a = M.Map tok (T.Tree tok ([tok] -> Rule err tok st m a))

-- | This data type is the result of a 'partialQuery'. It holds information about what needs to be
-- appended to a query to make 'Rule' evaluation succeed.
newtype PartialQueryResult err tok st m a =
  PartialQueryResult
    ( T.Tree tok ()
    , [(Either (RuleError err) a, QueryState tok st)]
    , [(RuleAltEvalPath err tok st m a, RuleAltEvalPath err tok st m a, QueryState tok st)]
    )

instance TestNull (PartialQueryResult err tok st m a) where
  nullValue = PartialQueryResult nullValue
  testNull (PartialQueryResult o) = testNull o

instance (Eq tok, Ord tok) => Monoid (PartialQueryResult err tok st m a) where
  mempty = nullValue
  mappend (PartialQueryResult (a1, b1, c1)) (PartialQueryResult (a2, b2, c2)) =
    PartialQueryResult (T.union a1 a2, b1++b2, c1++c2)

partialQueryResultTuple
  :: Monad m
  => Lens m (PartialQueryResult err tok st m a)
            ( T.Tree tok ()
            , [(Either (RuleError err) a, QueryState tok st)]
            , [(RuleAltEvalPath err tok st m a, RuleAltEvalPath err tok st m a, QueryState tok st)]
            )
partialQueryResultTuple = newLens (\ (PartialQueryResult o) -> o) (\o _ -> PartialQueryResult o)

-- | This is the lens that retrieves the potential next steps.
partialQueryNextSteps :: Monad m => Lens m (PartialQueryResult err tok st m a) (T.Tree tok ())
partialQueryNextSteps = partialQueryResultTuple >>> tuple0

-- | This lens retrieves the success and failure results of the current 'partialQuery'.
--
-- If 'partialQueryResults' is empty and the 'partialQueryBranches' value is also empty, this
-- indicates that the 'Query' that was just evaluated by 'partialQuery' is probably nonsense, as no
-- further input could possibly result in any provably wrong or provably right result.
--
-- However if 'partialQueryResults' is empty and 'partialQueryBranches' is not empty, this
-- indicates that the current input query must be incomplete, there must be more input to the query,
-- otherwise evaluating the 'Query' that was just evaluated by 'partialQuery' as it is will
-- certainly backtrack.
--
-- If 'partialQueryResults' contains some items but 'partialQueryBranches' is empty, this indicates
-- that the 'Query' that was just evaluated by 'partialQuery' is complete, and appending more to it
-- will result in backtracking.
--
-- If both 'partialQueryResult' and 'partialQueryBranches' both contain some items, this indicates
-- that the 'Query' that was just evaluated by 'partialQuery' is enough to produce a success or
-- failure, however appending more input to the 'Query' may produce in some other results.
--
-- The 'partialQueryResults' contains both failures and successes. 'Prelude.Right' values indicate
-- that the 'Query' evaluated by 'partialQuery' is provably correct according to at least some
-- 'Rule's, whereas 'Prelude.Left' values indicate that the 'Query' evaluated by 'partialQuery' was
-- provably wrong according to at least some 'Rule's.
partialQueryResults
  :: Monad m
  => Lens m (PartialQueryResult err tok st m a) [(Either (RuleError err) a, QueryState tok st)]
partialQueryResults = partialQueryResultTuple >>> tuple1

-- | This lens retrieves the trees of alternative evaluation paths that could be tried if further
-- input were appended to the current 'Query'.
--
-- If 'partialQueryBranches' value is empty and 'partialQueryResults' is also empty and the, this
-- indicates that the 'Query' that was just evaluated by 'partialQuery' is probably nonsense, as no
-- further input could possibly result in any provably wrong or provably right result.
--
-- However if 'partialQueryBranches' is not empty and 'partialQueryResults' is empty, this
-- indicates that the current input query must be incomplete, there must be more input to the query,
-- otherwise evaluating the 'Query' that was just evaluated by 'partialQuery' as it is will
-- certainly backtrack. 
--
-- If 'partialQueryBranches' is empty but 'partialQueryResults' contains some items, this indicates
-- that the 'Query' that was just evaluated by 'partialQuery'is complete, and appending more to it
-- will result in backtracking.
--
-- If both 'partialQueryBranches' and 'partialQueryResult' both contain some items, this indicates
-- that the 'Query' that was just evaluated by 'partialQuery' is enough to produce a success or
-- failure, however appending more input to the 'Query' may produce in some other results.
--
-- The 'partialQueryResults' contains both failures and successes. 'Prelude.Right' values indicate
-- that the 'Query' evaluated by 'partialQuery' is provably correct according to at least some
-- 'Rule's, whereas 'Prelude.Left' values indicate that the 'Query' evaluated by 'partialQuery' was
-- provably wrong according to at least some 'Rule's.
partialQueryBranches
  :: Monad m
  => Lens m (PartialQueryResult err tok st m a)
            [(RuleAltEvalPath err tok st m a, RuleAltEvalPath err tok st m a, QueryState tok st)]
partialQueryBranches = partialQueryResultTuple >>> tuple2

_partialQuery
  :: (Monad m, Eq tok, Ord tok)
  => Int -> Rule err tok st m a -> QueryState tok st -> m (PartialQueryResult err tok st m a)
_partialQuery lim rule qst = if lim<=0 then return mempty else case rule of
  RuleEmpty      -> return nullValue
  RuleReturn   a -> return $ PartialQueryResult (nullValue, [(Right  a, qst)], [])
  RuleThrow  err -> return $ PartialQueryResult (nullValue, [(Left err, qst)], [])
  RuleLift  rule -> rule >>= \rule -> _partialQuery lim rule qst
  RuleState rule -> liftM mconcat $ mapM (uncurry $ _partialQuery $ lim-1) (rule qst)
  RuleOp _  rule -> _partialQuery (lim-1) rule qst
  RuleChoice a b -> let depth1 = lim-1 in
    liftM2 mappend (_partialQuery depth1 a qst) (_partialQuery depth1 b qst)
  RuleTree   a b -> _partialChoice lim [(a, b, qst)]

_partialChoice
  :: (Monad m, Eq tok, Ord tok)
  => Int -> [(RuleAltEvalPath err tok st m a, RuleAltEvalPath err tok st m a, QueryState tok st)]
  -> m (PartialQueryResult err tok st m a)
_partialChoice lim abx = if lim<=0 then return mempty else liftM (mconcat . join) $
  forM abx $ \ (a, b, qst@(QueryState (st, score, i, qx))) -> case qx!i of
    Just  q -> do
      let part order = maybe [] (T.partitions order $ indexElems qx (i, size qx)) . M.lookup q
      forM (part T.BreadthFirst a ++ part T.DepthFirst b) $ \ ((px, _), rule) ->
        _partialQuery (lim-1) (rule px) $ QueryState (st, score, i+length px, qx)
    Nothing      -> liftM return $ _predictFuture lim qst [] $ T.Tree (Nothing, _plus a b)

_predictFuture
  :: (Monad m, Eq tok, Ord tok)
  => Int -> QueryState tok st -> [tok]
  -> (T.Tree tok ([tok] -> Rule err tok st m a))
  -> m (PartialQueryResult err tok st m a)
_predictFuture lim qst qx tree = if lim<=0 then return mempty else
  liftM mconcat $ forM (T.assocs T.BreadthFirst tree) $ \ (path, rule) -> do
    qx <- return $ qx++path
    let done qx o ab = return $ PartialQueryResult (T.singleton qx (), o, ab)
        loop f   qst = _predictFuture (lim-1) qst qx (T.singleton [] $ const f)
    case rule qx of
      RuleEmpty      -> done [] []                []
      RuleReturn   a -> done qx [(Right  a, qst)] []
      RuleThrow  err -> done [] [(Left err, qst)] []
      RuleLift     f -> f >>= flip loop qst
      RuleState    f -> liftM mconcat $ mapM (uncurry loop) $ f qst
      RuleOp     _ f -> loop f qst
      RuleChoice a b -> liftM2 mappend (loop a qst) (loop b qst)
      RuleTree   a b -> _predictFuture (lim-1) qst qx (T.Tree (Nothing, _plus a b))

-- | When you run a 'Rule' or 'Rule' with a partial query, you are will evaluate the
-- function up to the end of the query with the expectation that there is more to the query that
-- will be supplied.
--
-- This behavior is ideal for (and designed for) predictive input, for example tab completion on
-- command lines. The input written by an end user can be tokenized as a partial query and fed into
-- a 'Rule' using this function. The result will be a new 'Rule' that will begin evaluation
-- from where the previous partial query ended. Along with the result, you may also receive
-- "predictions", or a list of possible query steps that could allow the rule evaluation to succeed
-- without an error or backtracking.
--
-- This function takes a @depth::Int@ as it's first parameter. This is important because it is
-- common practice to create inifinitely large 'Rule' objects which are generated and evaluated
-- lazily. The depth limit parameter instructs this algorithm to stop trying alternative branches of
-- evaluation after a certain number of branch points have been followed. *NOTE* that this does have
-- anything to do with the number of possible branch completions that may be generated. A single
-- branch may contain millions of completions. Likewise, a million branches may contain no
-- completions at all. This depth limit is only related to the number of branches that may be
-- followed according to the algorithm which generated the 'Rule'. A logical branch is usually
-- created by functions like @('Control.Applicative.<|>')@, 'Control.Monad.mplus', or
-- 'Control.Monad.msum'.
--
-- *WARNING:* rule evaluation that is not pure, especially rules that lift the @IO@ monad, may end
-- up evaluating side effects several times. One thing you can do to avoid unwanted side effects is
-- to design your 'Rule' or 'Rule' to be polymorphic over the monadic type @m@ (refer to the
-- definition of 'Rule'). For the portions of your rule that can be used without lifting
-- @IO@, lift these 'Rule's into the 'Control.Monad.Trans.Identity.Identity' monad and use these for
-- evalutating 'partialQuery's. Then, when you need to use these rules with @IO@, since they are
-- polymorphic over the monadic type, you can simply use them with your @IO@ specific 'Rule's and
-- your Haskell compiler will not complain.
partialQuery
  :: (Monad m, Eq tok, Ord tok)
  => Int -> Rule err tok st m a -> st -> [tok] -> m (PartialQueryResult err tok st m a)
partialQuery lim rule st qx = _partialQuery lim rule $ QueryState (st, 0.0, 0, array qx)

-- | Continue a 'partialQuery' from where the previous 'partialQuery' result left off, as if another
-- 'Query' has been appended to the previous 'Query' of the call to the previous 'partialQuery'.
resumePartialQuery
  :: (Monad m, Eq tok, Ord tok)
  => Int -> PartialQueryResult err tok st m a -> [tok] -> m (PartialQueryResult err tok st m a)
resumePartialQuery lim (PartialQueryResult (_, _, abx)) qx = _partialChoice lim $
  (\ (a, b, QueryState (st, score, i, px)) ->
     (a, b, QueryState (st, score, i, px<>array qx)
  )) <$> abx

-- | Take a 'PartialQueryResult' and return a list of possible 'Query' completions, along with any
-- current success and failure values that would be returned if the 'partialQuery' were to be
-- evaluated as is.
guesses :: PartialQueryResult err tok st m a -> ([[tok]], [a], [RuleError err])
guesses (PartialQueryResult (trees, results, _)) =
  let score (_, QueryState (_, a1, a2, _)) (_, QueryState (_, b1, b2, _)) =
        compare a2 b2 <> compare b1 a1
      (lefts, rights) = partitionEithers $ fst <$> sortBy score results
  in  (fst <$> T.assocs T.BreadthFirst trees, rights, lefts)

-- | This is a convenience function used for tab-completion on the last word of an input query.
-- Let's say your user inputs a string like @this is some te@, and your 'Rule' is designed to match
-- an input of the form @this is some text@.
--
-- When implementing tab-completion, the input string needs to be split into token
-- tokens:
--
-- @
-- ["this", "is", "some", "te"]
-- @
--
-- Then all but the last token needs to be 'partialQuery'-ed on a 'Rule':
--
-- @
-- 'partialQuery' 99 myRule myState $ 'Dao.Object.obj' <$> ["this", "is", "some"] -- leaving off "te"
-- @
--
-- The 'partialQuery' will yield a 'PartialQueryResult' that we can pass to the 'guesses' function.
--
-- Finally, we need to use the final token from the user input: @"te"@ to narrow down the list of
-- possible queries produced by the 'guesses' function.
--
-- The 'guessPartial' function much of this busywork for you.
--
-- As the first parameter pass a call to 'partialQuery' or 'resumePartialQuery', for example:
-- 
-- @
-- ('partialQuery' 99 myRule myInitState) -- this
-- ('resumePartialQuery' 99 previousQueryResult) -- or this could be the first parameter to 'guessPartial'
-- @
--
-- As the second parameter, pass the 'Query'. The last token will be removed from this
-- 'Query' and before being passed to the first parameter function.
--
-- Finally, as the third parameter pass a filter function. This filter function will take the last
-- token of the 'Query' and a list of possible 'Query' completions. For example:
--
-- @
-- myCompletionFilter :: 'Prelude.Maybe' tok -> [[tok]] -> m [[tok]]
-- myCompletionFilter finalObj completionsList = return (completionList >>= filterPrefixes) where
--     filterPrefixes completion = if null completion then [] else case finalObj of
--         Nothing         -> [completion]
--         Just finalToken -> do
  --         'Control.Monad.guard' $ 'Dao.Text.toText' finalToken `'Data.Text.isPrefixOf'` Dao.Text.toText' (head completion)
  --         [completion]
-- @
--
-- So your call to 'guessPartial' could look like this:
--
-- @
-- 'guessPartial' ('partialQuery' 99 myRule myInitState) someUserInput $ \finalObj completionList ->
--     let filterPrefixes completion = if null completion then [] else case finalObj of
--             Nothing         -> [completion]
--             Just finalToken -> do
--                 'Control.Monad.guard' $ 'Dao.Text.toText' finalObj `'Data.Text.isPrefixOf'` Dao.Text.toText' (head completion)
--                 [completion]
--     return (completionList >>= filterPrefixes)
-- @
--
-- If the 'Query' given to 'guessPartial' is empty, the filter function is passed 'Prelude.Nothing'
-- as the first parameter to the filter function.
--
-- The returned result of 'guessPartial' is the filtered list of 'Query's and the
-- 'PartialQueryResult' which you can use to call 'resumePartialQuery'.
guessPartial
  :: Monad m
  => ([tok] -> m (PartialQueryResult err tok st m a))
  -> [tok]
  -> (Maybe tok -> [[tok]] -> m [[tok]])
  -> m ([[tok]], PartialQueryResult err tok st m a)
guessPartial callPartialQuery qx filter = do
  (q, qx) <- return $ case reverse qx of
    []   -> (Nothing, [])
    q:qx -> (Just  q, reverse qx)
  result  <- callPartialQuery qx
  let (completions, _, _) = guesses result
  completions <- filter q completions
  return (completions, result)

