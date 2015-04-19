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
    Query, QueryState(QueryState), queryStateTuple, querySubState, queryScore, queryInput,
    -- * Production Rules: the 'Rule' Monad
    StatefulRule, Rule, evalRuleLogic, queryAll, query, query1, next, part, remainder, done,
    limitByScore, bestMatch, resetScore,
    -- * The Branch Structure of a Rule
    -- $Structure_of_a_rule
    RuleStruct, tree, struct, ruleTree, getRuleStruct, trim, mask,
    -- * Convenient Rule Trees
    TypePattern(TypePattern), patternTypeRep, infer,
    -- * Predicting User Input
    PartialQueryResult(PartialQueryResult),
    partialQuery, resumePartialQuery, partialQueryResultTuple,
    partialQueryResults, partialQueryNextSteps, guesses,
    RuleAltEvalPath,
    -- * Re-export the "Dao.Logic" module.
    module Dao.Logic
  )
  where

import           Dao.Lens
import           Dao.Logic
import           Dao.Object
import           Dao.Text.PPrint
import           Dao.TestNull
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

----------------------------------------------------------------------------------------------------

-- | A 'Query' is the list of 'Dao.Object.Object's input by the end user to the knowledge base. Your
-- program will construct a 'Query' and use 'queryAll', 'query', or 'query1' with a 'Rule' monad to
-- perform a logical computation.
type Query = [Object]

-- | The 'Query' a list of 'Dao.Object.Object's each paired with an integer score. When evaluating a
-- 'Rule' monad, the 'queryInput' is tested by various 'Rule' functions to produce a result, and the
-- result is 'Control.Monad.return'ed when all the 'Rule's that can possibly match have matched
-- against the 'queryInput', and all possible results that have been successfully
-- 'Control.Monad.return'ed. A 'queryScore' is kept, which counts how many 'Dao.Object.Object' items
-- from the 'queryInput' have matched so far. This 'queryScore' can be helpful in narrowing the list
-- of potential results.
--
-- The 'QueryState' also contains a 'querySubState', which is a polymorphic value that can be
-- whatever you would like it to be. This state value should be light-weight, or at least easily
-- copied and re-written, because the 'StatefulRule' monad in which the 'QueryState' is used will
-- create many copies of the 'QueryState' (taking advantage of Haskell's lazy copy on write to do it
-- efficiently) as multiple branches of 'Rule' evaluation are explored. In the case of the 'Rule'
-- the 'querySubState' is always @()@, not everyone needs to make use of the state.
newtype QueryState st = QueryState (st, Int, Query) deriving (Eq, Ord, Show, Typeable)

instance TestNull st => TestNull (QueryState st) where
  nullValue = QueryState nullValue
  testNull (QueryState o) = testNull o

queryStateTuple :: Monad m => Lens m (QueryState st) (st, Int, Query)
queryStateTuple = newLens (\ (QueryState o) -> o) (\o _ -> QueryState o)

-- | Keeps the polymorphic stateful data.
querySubState :: Monad m => Lens m (QueryState st) st
querySubState = queryStateTuple >>> tuple0

queryScore :: Monad m => Lens m (QueryState st) Int
queryScore = queryStateTuple >>> tuple1

queryInput :: Monad m => Lens m (QueryState st) Query
queryInput = queryStateTuple >>> tuple2

_plus :: Monad m => RuleAltEvalPath st m a -> RuleAltEvalPath st m a -> RuleAltEvalPath st m a
_plus = M.unionWith (T.unionWith (\a b q -> mplus (a q) (b q)))

----------------------------------------------------------------------------------------------------

-- | A 'Rule' is a monadic function that defines the behavior of a production rule in a
-- 'KnowledgeBase'. A 'KnowledgeBase' is queried with a list of 'Dao.Object.Object's being matched
-- against a sequence of this 'Rule' data type.
--
-- When query is matched agains a 'Rule', the query is placed into an internal stateful monad, and
-- as the 'Rule' is evaluated, the query is deconstructed. Evaluating to
-- 'Control.Applicative.empty' or 'Control.Monad.mzero' indicates a non-match and evaluation
-- backtracks. Evaluating 'Control.Monad.return' indicates a success, and the returned
-- 'Dao.Object.Object' is used as the result of the production.
--
-- The 'Rule' monad instantiates 'Control.Monad.Except.Class.MonadError' such that
-- 'Dao.Object.ErrorObject's can be thrown and caught. The 'Rule' monad instantiates
-- 'Dao.Logic.MonadLogic' so it is possible to pattern match with many
-- 'Control.Applicative.Alternative' branches of evaluation without having to worry about the
-- current state. 'Control.Monad.State.Class.MonadState' is instantiated giving you total control of
-- the state, along with the 'Dao.Logic.MonadLogic' functions. And
-- 'Control.Monad.Reader.Class.MonadReader' is instantiated so that the
-- 'Control.Monad.Reader.Class.local' function can be used to execute rules with a different input
-- in a different context without altering the current context.
data StatefulRule st m a
  = RuleEmpty
  | RuleReturn a
  | RuleError ErrorObject
  | RuleLift (m (StatefulRule st m a))
  | RuleLogic (LogicT (QueryState st) m (Either ErrorObject (StatefulRule st m a)))
  | RuleChoice (StatefulRule st m a) (StatefulRule st m a)
  | RuleTree  (RuleAltEvalPath st m a) (RuleAltEvalPath st m a)
    -- DepthFirst and BreadthFirst rule trees are kept separate.

-- | A 'Rule' is a 'StatefulRule' where the 'querySubState' is @()@.
type Rule m a = StatefulRule () m a

instance Monad m => Functor (StatefulRule st m) where
  fmap f rule = case rule of
    RuleEmpty      -> RuleEmpty
    RuleReturn   o -> RuleReturn $ f o
    RuleError  err -> RuleError err
    RuleLift     o -> RuleLift $ liftM (liftM f) o
    RuleLogic    o -> RuleLogic $ liftM (liftM (liftM f)) o
    RuleChoice x y -> RuleChoice (liftM f x) (liftM f y)
    RuleTree   x y -> let map = fmap (fmap (fmap (fmap f))) in RuleTree (map x) (map y)

instance (Functor m, Monad m) => Applicative (StatefulRule st m) where { pure = return; (<*>) = ap; }

instance (Functor m, Monad m) => Alternative (StatefulRule st m) where { empty = mzero; (<|>) = mplus; }

instance Monad m => Monad (StatefulRule st m) where
  return = RuleReturn
  rule >>= f = case rule of
    RuleEmpty      -> RuleEmpty
    RuleReturn   o -> f o
    RuleError  err -> RuleError err
    RuleLift     o -> RuleLift $ liftM (>>= f) o
    RuleLogic    o -> RuleLogic $ liftM (liftM (>>= f)) o
    RuleChoice x y -> RuleChoice (x >>= f) (y >>= f)
    RuleTree   x y -> let map = fmap $ fmap $ fmap (>>= f) in RuleTree (map x) (map y)
  a >> b = case a of
    RuleEmpty        -> RuleEmpty
    RuleReturn _     -> b
    RuleError  err   -> RuleError err
    RuleLift   a     -> RuleLift $ liftM (>> b) a
    RuleLogic     a  -> case b of
      RuleLogic    b   -> RuleLogic $ a >> b
      b                -> RuleLogic $ liftM (fmap (>> b)) a
    RuleChoice a1 a2 -> RuleChoice (a1 >> b) (a2 >> b)
    RuleTree   a1 a2 -> case b of
      RuleEmpty        -> RuleEmpty
      RuleError  err   -> RuleError err
      RuleTree   b1 b2 ->
        let wrap map = T.Tree (Nothing, map)
            unwrap (T.Tree (_, tree)) = tree
            power a b = unwrap $ T.productWith (>>) (wrap a) (wrap b)
        in  RuleTree (power a1 b1) (power a2 b2)
      b                -> let bind = fmap $ fmap $ fmap (>> b) in RuleTree (bind a1) (bind a2)
  fail = RuleError . return . obj

instance Monad m => MonadPlus (StatefulRule st m) where
  mzero = RuleEmpty
  mplus a b = case a of
    RuleEmpty        -> b
    RuleChoice (RuleChoice a1 a2) a3 -> mplus a1 $ mplus a2 $ mplus a3 b
    RuleChoice a1 (RuleChoice a2 a3) -> RuleChoice a1 $ RuleChoice a2 $ mplus a3 b
    RuleChoice a1 a2 -> case b of
      RuleEmpty        -> a
      b                -> RuleChoice a1 $ RuleChoice a2 b
    RuleTree   a1 a2 -> case b of
      RuleEmpty        -> a
      RuleChoice b1 b2 -> RuleChoice (mplus a b1) b2
      RuleTree   b1 b2 -> RuleTree (_plus a1 b1) (_plus a2 b2)
      b                -> RuleChoice a b
    RuleLogic     a  -> case b of
      RuleEmpty        -> RuleLogic a
      RuleLogic     b  -> RuleLogic $ mplus a b
      b                -> RuleChoice (RuleLogic a) b
    a                -> case b of
      RuleEmpty        -> a
      b                -> RuleChoice a b

instance Monad m => MonadError ErrorObject (StatefulRule st m) where
  throwError            = RuleError
  catchError rule catch = case rule of
    RuleError o -> catch o
    _           -> rule

instance Monad m => MonadState st (StatefulRule st m) where
  state f = RuleLogic $ state $ \st ->
    let (o, q) = f $ st~>querySubState in (Right $ return o, with st [querySubState <~ q])

instance Monad m => MonadReader st (StatefulRule st m) where
  ask = get;
  local f sub = RuleLogic $ do
    st <- state $ \st -> (st, with st [querySubState $= f])
    evalRuleLogic sub >>= return . Left ||| state . const . flip (,) st . Right . return

instance Monad m => MonadLogic st (StatefulRule st m) where
  superState  f = RuleLogic $ superState $ \st ->
    f (st~>querySubState) >>= \ (o, q) -> return (Right $ return o, with st [querySubState <~ q])
  entangle rule = RuleLogic $ do
    let lrzip (o, qx) = (\err -> ([(err, qx)], [])) ||| (\o -> ([], [(o, qx)])) $ o
    (errs, ox) <- liftM ((concat *** concat) . unzip . fmap lrzip) $ entangle (evalRuleLogic rule)
    superState $ \st -> (Right $ return $ second (~> querySubState) <$> ox, st) : fmap (first Left) errs

instance MonadTrans (StatefulRule st) where
  lift f = RuleLift $ liftM return f

instance MonadIO m => MonadIO (StatefulRule st m) where { liftIO = RuleLift . liftM return . liftIO; }

instance MonadFix m => MonadFix (StatefulRule st m) where { mfix f = RuleLift $ mfix (return . (>>= f)); }

instance Monad m => Monoid (StatefulRule st m o) where { mempty=mzero; mappend=mplus; }

----------------------------------------------------------------------------------------------------

-- | Evaluate a 'Rule' by flattening it's internal 'Dao.Tree.Tree' structure to a 'Dao.Logic.LogicT'
-- monad. This is probably not as useful of a function as 'queryAll' or 'query'.
evalRuleLogic
  :: forall st m a . Monad m
  => StatefulRule st m a -> LogicT (QueryState st) m (Either ErrorObject a)
evalRuleLogic rule = case rule of
  RuleEmpty      -> mzero
  RuleReturn   o -> return $ Right o
  RuleError  err -> return $ Left err
  RuleLift     o -> lift o >>= evalRuleLogic
  RuleLogic    o -> o >>= return . Left ||| evalRuleLogic
  RuleChoice x y -> mplus (evalRuleLogic x) (evalRuleLogic y)
  RuleTree   x y -> mplus (runMap T.DepthFirst [] x) (runMap T.BreadthFirst [] y) where
    runMap control qx map = if M.null map then mzero else do
      q <- evalRuleLogic next
      case q of
        Left  q -> return $ Left q
        Right q -> do
          let (equal, similar) = partition ((ExactlyEqual ==) . fst)
                (do (o, tree) <- M.assocs map
                    let result = objMatch o q
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

-- | Run a 'Rule' against an @['Dao.Object.Object']@ query, return all successful results and all
-- exceptions that may have been thrown by 'Control.Monad.Except.throwError'.
queryAll :: Monad m => StatefulRule st m a -> st -> Query -> m [Either ErrorObject a]
queryAll rule st q = runLogicT (evalRuleLogic rule) (QueryState (st, 0, q)) >>= return . fmap fst

-- | Run a 'Rule' against an @['Dao.Object.Object']@ query, return all successful results.
query :: Monad m => StatefulRule st m a -> st -> Query -> m [a]
query r st = queryAll r st >=> return . (>>= (const [] ||| return))

-- | Like 'query', but only returns the first successful result, if any.
query1 :: Monad m => StatefulRule st m a -> st -> Query -> m (Maybe a)
query1 r st = query r st >=> \ox -> return $ if null ox then Nothing else Just $ head ox

-- | Take the next item from the query input, backtrack if there is no input remaining.
next :: Monad m => StatefulRule st m Object
next = RuleLogic $ superState $ \q -> if null $ q~>queryInput then [] else
  [(Right $ return $ head $ q~>queryInput, with q [queryInput $= tail, queryScore $= (+ 1)])]

-- | Take as many of next items from the query input as necessary to make the rest of the 'Rule'
-- match the input query. This acts as kind of a Kleene star.
part :: Monad m => StatefulRule st m Query
part = RuleLogic $ superState $ loop [] where
  out  keep st = (Right $ return keep, st)
  loop keep st = case st~>queryInput of
    []   -> [out keep $ with st [queryInput <~ []]]
    q:qx -> out keep st : loop (keep++[q]) (with st [queryScore $= (+ 1), queryInput <~ qx])

-- | Clear the remainder of the input 'Query' and return it.
remainder :: Monad m => StatefulRule st m Query
remainder = RuleLogic $ state $ \st ->
  (Right $ return $ st~>queryInput, with st [queryInput <~ []])

-- | Match when there are no more arguments, backtrack if there are.
--
-- This function is polymorphic over a monadic type that instantiates 'Dao.Logic.MonadLogic',
-- however consider the type of this function to be:
--
-- @
-- ('Data.Functor.Functor' m, 'Control.Monad.Monad' m) => 'Rule' m 'Dao.Object.Object'
-- @
done :: Monad m => StatefulRule st m ()
done = RuleLogic $ superState $ \q -> [(Right $ return (), q) | null $ q~>queryInput]

-- | Fully evaluate a 'Rule', and collect all possible results along with their 'queryScore's. Pass
-- these results to a filter function, and procede with 'Rule' evaluation using only the results
-- allowed by the filter function. This function uses the 'Dao.Logic.entangle' operation, so it will
-- fully evaluate the given monadic function before returning.
--
-- This function is polymorphic over a monadic type that instantiates 'Dao.Logic.MonadLogic',
-- however consider the type of this function to be:
--
-- @
-- ('Data.Functor.Functor' m, 'Control.Monad.Monad' m) => 'Rule' m 'Dao.Object.Object'
-- @
limitByScore :: Monad m => ([(a, QueryState st)] -> [(a, QueryState st)]) -> StatefulRule st m a -> StatefulRule st m a
limitByScore filter f = do
  a <- RuleLogic $ liftM (Right . return) $ entangle $ evalRuleLogic f
  let (err, ox) = (concat *** concat) $ unzip $
        (\ (o, qs) -> (\o -> ([(Left o, qs)], [])) ||| (\o -> ([], [(o, qs)])) $ o) <$> a
  RuleLogic $ superState $ const $ (first (Right . return) <$> filter ox) ++ err

-- | Like 'limitByScore' but the filter function simply selects the results with the highet
-- 'queryScore'.
bestMatch :: Monad m => StatefulRule st m a -> StatefulRule st m a
bestMatch = let score = (~> queryScore) . snd in limitByScore $ concat .
  take 1 . groupBy (\a b -> score a == score b) . sortBy (\a b -> score b `compare` score a)

-- | Evaluate a monadic function with the 'queryScore' reset to zero, and when evaluation of the
-- monadic function completes, set the score back to the value it was before.
--
-- This function is polymorphic over a monadic type that instantiates 'Dao.Logic.MonadLogic',
-- however consider the type of this function to be:
--
-- @
-- ('Data.Functor.Functor' m, 'Control.Monad.Monad' m) => 'Rule' m 'Dao.Object.Object'
-- @
resetScore :: Monad m => StatefulRule st m a -> StatefulRule st m a
resetScore f = do
  score <- RuleLogic $ state $ \q ->
    (Right . return $ q~>queryScore, with q [queryScore <~ 0])
  a <- f
  RuleLogic $ liftM (Right . return) $ modify (by [queryScore <~ score])
  return a

----------------------------------------------------------------------------------------------------

-- $Structure_of_a_rule
-- A 'Rule' is constructed from 'Dao.Tree.Tree' data types and functions. Some 'Rule's form empty
-- trees, for example 'Control.Monad.return' or 'Control.Monad.State.state'. However 'Rule's
-- constructed with functions like 'tree' or 'struct' produce a 'Dao.Tree.Tree' structure internal
-- to the 'Rule' function which can be retrieved and manipulated. This is useful for
-- meta-programming 'Rule's, for example predictive input applications.

-- | This is the data type that models the branch structure of a 'Rule'. It is a 'Dao.Tree.Tree'
-- with 'Dao.Object.Object' paths and @()@ leaves. It is possible to perform modifications to some
-- 'Rule's, for example 'trim'-ing of branches, using a 'RuleStruct'.
type RuleStruct = T.Tree Object ()

-- | Take a list of lists of a type of 'Dao.Object.ObjectData' and construct a 'Rule' tree that will
-- match any 'Query' similar to this list of 'Dao.Object.ObjectData' values (using
-- 'Dao.Object.objMatch'). Every list of 'Dao.Object.ObjectData' will become a branch associated
-- with the given 'Rule' monadic function (constructed using the 'Dao.Tree.fromList' function). This
-- 'Rule' function must take a 'Query' as input. When a portion of a 'Query' matches the given
-- 'Dao.Object.ObjectData', the portion of the 'Query' that matched will be passed to this 'Rule'
-- function when it is evaluated.
tree
  :: (Monad m, ObjectData a)
  => T.RunTree -> [[a]] -> (Query -> StatefulRule st m b) -> StatefulRule st m b
tree control branches = struct control $ T.fromList $ zip (fmap obj <$> branches) $ repeat ()

-- | Construct a 'Rule' tree from a 'Dao.Tree.Tree' data type and a 'Rule' function. The 'Rule' will
-- be copied to every single 'Dao.Tree.Leaf' in the given 'Dao.Tree.Tree'.
struct
  :: Monad m
  => T.RunTree -> RuleStruct -> (Query -> StatefulRule st m a) -> StatefulRule st m a
struct control tree rule =
  let df = control==T.DepthFirst
      (T.Tree (leaf, map)) = fmap (\ () -> rule) tree 
  in  maybe id ((if df then flip else id) mplus . ($ [])) leaf $
        (if df then id else flip) RuleTree map nullValue

-- | Take a 'Dao.Tree.Tree' and turn it into a 'Rule' where every 'Dao.Tree.leaf' becomes a 'Rule'
-- that simply 'Control.Monad.return's the 'Dao.Tree.leaf' value.
ruleTree :: Monad m => T.RunTree -> T.Tree Object a -> StatefulRule st m a
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
getRuleStruct :: StatefulRule st m o -> RuleStruct
getRuleStruct rule = case rule of
  RuleEmpty      -> T.empty
  RuleTree   x y -> T.Tree (Nothing, M.union (fmap void x) (fmap void y))
  RuleChoice x y -> T.union (getRuleStruct x) (getRuleStruct y)
  _              -> T.Tree (Just (), M.empty)

-- | With a 'RuleStruct' delete any of the matching branches from the 'Rule' tree. Branch matching
-- uses the @('Prelude.==')@ predicate, not 'Dao.Object.objMatch'. This is the dual of 'mask' in
-- that @'trim' struct t 'Data.Monoid.<>' 'mask' struct t == t@ is always true.
-- This function works by calling 'Dao.Tree.difference' on the 'Rule' and the 'Dao.Tree.Tree'
-- constructed by the 'Dao.Tree.blankTree' of the given list of 'Dao.Object.ObjectData' branches.
trim :: Monad m => RuleStruct -> StatefulRule st m a -> StatefulRule st m a
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
-- @('Prelude.==')@ predicate, not 'Dao.Object.objMatch'. This function works by calling
-- 'Dao.Tree.intersection' on the 'Rule' and the 'Dao.Tree.Tree' constructed by the
-- 'Dao.Tree.blankTree' of the given list of 'Dao.Object.ObjectData' branches.
mask :: Monad m => RuleStruct -> StatefulRule st m a -> StatefulRule st m a
mask tree@(T.Tree (leaf, map)) rule = case rule of
  RuleEmpty      -> RuleEmpty
  RuleTree   x y -> RuleTree (del x) (del y)
  RuleChoice x y -> RuleChoice (mask tree x) (mask tree y)
  rule           -> maybe mzero (\ () -> rule) leaf
  where
    del = flip (M.intersectionWith T.intersection) map

----------------------------------------------------------------------------------------------------

-- | This data type is an 'Dao.Object.Object' containing a 'Data.Typeable.TypeRep'. When
-- constructing a 'RuleTree', this pattern will match any object that matches the type it contains.
-- Use 'objTypeOf'
newtype TypePattern = TypePattern { patternTypeRep :: TypeRep } deriving (Eq, Ord, Typeable)

instance HasTypeRep TypePattern where { objTypeOf (TypePattern o) = o; }

instance Show TypePattern where { show (TypePattern o) = show o; }

instance PPrintable TypePattern where { pPrint = return . pShow; }

instance SimpleData TypePattern where
  simple (TypePattern o) = simple o
  fromSimple = fmap TypePattern . fromSimple

instance ObjectPattern TypePattern where
  objMatch (TypePattern p) o = if p==objTypeOf o then Similar 0.0 else Dissimilar

instance ObjectData TypePattern where
  obj p = obj $ printable p $ matchable p $ fromForeign p
  fromObj = defaultFromObj

-- | Use 'next' to take the next item from the current 'Query', evaluate the 'Data.Typeable.TypeRep'
-- of the 'next' 'Dao.Object.Object' using 'objTypeOf', compare this to the to the
-- 'Data.Typeable.TypeRep' of @t@ inferred by 'Data.Typeable.typeOf'. Compare these two types using
-- @('Prelude.==')@, and if 'Prelude.True' evaluate a function on it.  This function makes a new
-- 'RuleTree' where the pattern in the branch is a 'TypePattern'. For example, if you pass a
-- function to 'infer' which is of the type @('Prelude.String' -> 'Rule' m a)@, 'infer' will create
-- a 'RuleTree' that matches if the 'Dao.Object.Object' returned by 'next' can be cast to a value of
-- 'Prelude.String'.
infer
  :: forall st m t a . (Functor m, Monad m, Typeable t, ObjectData t)
  => (t -> StatefulRule st m a) -> StatefulRule st m a
infer f = tree T.BreadthFirst [[typ f err]] just1 where
  just1 ox = case ox of
    [o] -> fromObj o >>= f
    _   -> mzero
  typ :: (t -> StatefulRule st m a) -> t -> TypePattern
  typ _ ~t = TypePattern $ typeOf t
  err :: t
  err = error "in Dao.Rule.infer: typeOf evaluated undefined"

----------------------------------------------------------------------------------------------------

-- | This data type contains a tree with a depth of no less than one (hence it is a 'Dao.Tree.Tree'
-- inside of a 'Data.Map.Map', the 'Data.Map.Map' matches the first element of the 'Query') which is
-- used to construct all alternative paths of 'Rule' evaluation.
type RuleAltEvalPath st m a = M.Map Object (T.Tree Object (Query -> StatefulRule st m a))

-- | This data type is the result of a 'partialQuery'. It holds information about what needs to be
-- appended to a query to make 'Rule' evaluation succeed.
newtype PartialQueryResult st m a =
  PartialQueryResult
    ( [(Either ErrorObject a, QueryState st)]
    , [(RuleAltEvalPath st m a, RuleAltEvalPath st m a, QueryState st)]
    )

instance TestNull (PartialQueryResult st m a) where
  nullValue = PartialQueryResult ([], [])
  testNull (PartialQueryResult (a, b)) = null a && null b

instance Monoid (PartialQueryResult st m a) where
  mempty = nullValue
  mappend (PartialQueryResult (a1, b1)) (PartialQueryResult (a2, b2)) =
    PartialQueryResult (a1++a2, b1++b2)

partialQueryResultTuple
  :: Monad m
  => Lens m (PartialQueryResult st m a)
            ( [(Either ErrorObject a, QueryState st)]
            , [(RuleAltEvalPath st m a, RuleAltEvalPath st m a, QueryState st)]
            )
partialQueryResultTuple = newLens (\ (PartialQueryResult o) -> o) (\o _ -> PartialQueryResult o)

-- | This lens retrieves the success and failure results of the current 'partialQuery'.
--
-- If 'partialQueryResults' is empty and the 'partialQueryNextSteps' value is also empty, this
-- indicates that the 'Query' that was just evaluated by 'partialQuery' is probably nonsense, as no
-- further input could possibly result in any provably wrong or provably right result.
--
-- However if 'partialQueryResults' is empty and 'partialQueryNextSteps' is not empty, this
-- indicates that the current input query must be incomplete, there must be more input to the query,
-- otherwise evaluating the 'Query' that was just evaluated by 'partialQuery' as it is will
-- certainly backtrack.
--
-- If 'partialQueryResults' contains some items but 'partialQueryNextSteps' is empty, this indicates
-- that the 'Query' that was just evaluated by 'partialQuery' is complete, and appending more to it
-- will result in backtracking.
--
-- If both 'partialQueryResult' and 'partialQueryNextSteps' both contain some items, this indicates
-- that the 'Query' that was just evaluated by 'partialQuery' is enough to produce a success or
-- failure, however appending more input to the 'Query' may produce in some other results.
--
-- The 'partialQueryResults' contains both failures and successes. 'Prelude.Right' values indicate
-- that the 'Query' evaluated by 'partialQuery' is provably correct according to at least some
-- 'Rule's, whereas 'Prelude.Left' values indicate that the 'Query' evaluated by 'partialQuery' was
-- provably wrong according to at least some 'Rule's.
partialQueryResults
  :: Monad m
  => Lens m (PartialQueryResult st m a) [(Either ErrorObject a, QueryState st)]
partialQueryResults = partialQueryResultTuple >>> tuple0

-- | This lens retrieves the trees of alternative evaluation paths that could be tried if further
-- input were appended to the current 'Query'.
--
-- If 'partialQueryNextSteps' value is empty and 'partialQueryResults' is also empty and the, this
-- indicates that the 'Query' that was just evaluated by 'partialQuery' is probably nonsense, as no
-- further input could possibly result in any provably wrong or provably right result.
--
-- However if 'partialQueryNextSteps' is not empty and 'partialQueryResults' is empty, this
-- indicates that the current input query must be incomplete, there must be more input to the query,
-- otherwise evaluating the 'Query' that was just evaluated by 'partialQuery' as it is will
-- certainly backtrack. 
--
-- If 'partialQueryNextSteps' is empty but 'partialQueryResults' contains some items, this indicates
-- that the 'Query' that was just evaluated by 'partialQuery'is complete, and appending more to it
-- will result in backtracking.
--
-- If both 'partialQueryNextSteps' and 'partialQueryResult' both contain some items, this indicates
-- that the 'Query' that was just evaluated by 'partialQuery' is enough to produce a success or
-- failure, however appending more input to the 'Query' may produce in some other results.
--
-- The 'partialQueryResults' contains both failures and successes. 'Prelude.Right' values indicate
-- that the 'Query' evaluated by 'partialQuery' is provably correct according to at least some
-- 'Rule's, whereas 'Prelude.Left' values indicate that the 'Query' evaluated by 'partialQuery' was
-- provably wrong according to at least some 'Rule's.
partialQueryNextSteps
  :: Monad m
  => Lens m (PartialQueryResult st m a)
            [(RuleAltEvalPath st m a, RuleAltEvalPath st m a, QueryState st)]
partialQueryNextSteps = partialQueryResultTuple >>> tuple1

_partialQuery
  :: Monad m
  => Int -> StatefulRule st m a -> QueryState st -> m (PartialQueryResult st m a)
_partialQuery depth rule qst@(QueryState (st, score, qx)) = case rule of
  RuleEmpty       -> return nullValue
  RuleReturn    a -> return $ PartialQueryResult ([(Right  a, qst)], [])
  RuleError   err -> return $ PartialQueryResult ([(Left err, qst)], [])
  RuleLift   rule -> rule >>= \rule -> _partialQuery depth rule qst
  RuleLogic logic -> do
    results <- runLogicT logic (QueryState (st, score, qx))
    liftM mconcat $ forM results $ \ (result, st) -> case result of
      Left   err -> return $ PartialQueryResult ([(Left err, st)], [])
      Right rule -> if depth<=0 then return mempty else _partialQuery (depth-1) rule qst
  RuleChoice  a b -> if depth<=0 then return mempty else let depth1 = depth-1 in
    liftM2 mappend (_partialQuery depth1 a qst) (_partialQuery depth1 b qst)
  RuleTree    a b -> _partialChoice depth [(a, b, QueryState (st, 0, qx))]

_partialChoice
  :: Monad m
  => Int -> [(RuleAltEvalPath st m a, RuleAltEvalPath st m a, QueryState st)]
  -> m (PartialQueryResult st m a)
_partialChoice depth abx = liftM (mconcat . join) $
  forM abx $ \ (a, b, QueryState (st, score, qx)) -> case qx of
    q:qx | depth>0 -> do
      let part order = maybe [] (T.partitions order qx) . M.lookup q
      forM (part T.BreadthFirst a ++ part T.DepthFirst b) $ \ ((px, qx), rule) ->
        _partialQuery (depth-1) (rule px) $ QueryState (st, score, qx)
    _            -> return [PartialQueryResult ([], abx)]

-- | When you run a 'StatefulRule' or 'Rule' with a partial query, you are will evaluate the
-- function up to the end of the query with the expectation that there is more to the query that
-- will be supplied.
--
-- This behavior is ideal for (and designed for) predictive input, for example tab completion on
-- command lines. The input written by an end user can be tokenized as a partial query and fed into
-- a 'StatefulRule' using this function. The result will be a new 'Rule' that will begin evaluation
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
-- to design your 'StatefulRule' or 'Rule' to be polymorphic over the monadic type @m@ (refer to the
-- definition of 'StatefulRule'). For the portions of your rule that can be used without lifting
-- @IO@, lift these 'Rule's into the 'Control.Monad.Trans.Identity.Identity' monad and use these for
-- evalutating 'partialQuery's. Then, when you need to use these rules with @IO@, since they are
-- polymorphic over the monadic type, you can simply use them with your @IO@ specific 'Rule's and
-- your Haskell compiler will not complain.
partialQuery
  :: Monad m
  => Int -> StatefulRule st m a -> st -> Query -> m (PartialQueryResult st m a)
partialQuery depth rule st qx = _partialQuery depth rule $ QueryState (st, 0, qx)

-- | Continue a 'partialQuery' from where the previous 'partialQuery' result left off, as if another
-- 'Query' has been appended to the previous 'Query' of the call to the previous 'partialQuery'.
resumePartialQuery
  :: Monad m
  => Int -> PartialQueryResult st m a -> Query -> m (PartialQueryResult st m a)
resumePartialQuery depth (PartialQueryResult (_, abx)) qx = _partialChoice depth $
  (\ (a, b, QueryState (st, score, px)) -> (a, b, QueryState (st, score, px++qx))) <$> abx

-- | Take a 'PartialQueryResult' and return a list of possible 'Query' completions, along with any
-- current success and failure values that would be returned if the 'partialQuery' were to be
-- evaluated as is.
guesses :: PartialQueryResult st m a -> ([Query], [a], [ErrorObject])
guesses (PartialQueryResult (results, trees)) =
  let score (_, QueryState (_, a, _)) (_, QueryState (_, b, _)) = compare b a
      (lefts, rights) = partitionEithers $ fst <$> sortBy score results
      clear o = Sum $ T.Tree (Nothing, void <$> o)
      completions = getSum $ foldl (\tree (a, b, _) -> tree <> clear a <> clear b) nullValue trees
  in  (fst <$> T.assocs T.BreadthFirst completions, rights, lefts)

