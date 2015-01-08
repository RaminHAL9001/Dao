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
    Query,
    -- * Production Rules: the 'Rule' Monad
    Rule, evalRuleLogic, queryAll, query, query1, note, next, part, failIf, exclusive, chooseOnly,
    done, dropErrors,
    -- * Commenting Your 'Rule's with 'Note's
    Note(NoteBacktrack, NoteSuccess, NoteObject, NoteError, NoteSequence, NoteChoice),
    -- * The Branch Structure of a Rule
    -- $Structure_of_a_rule
    RuleStruct, tree, struct, getRuleStruct, prune, mask,
    -- * Convenient Rule Trees
    TypePattern(TypePattern), patternTypeRep, infer
  )
  where

import           Dao.Logic
import           Dao.Object
import           Dao.PPrint
import           Dao.TestNull
import qualified Dao.Tree as T

import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Reader.Class
import           Control.Monad.Except

import           Data.Dynamic
import           Data.Either (partitionEithers)
import           Data.Monoid
import qualified Data.Map as M

----------------------------------------------------------------------------------------------------

-- | The 'Query' type synonym is simply a list of 'Dao.Object.Object's, this is how queries to a
-- knowledge base are modeled. When a 'Query' is evaluated, it is placed into the state of the
-- 'Rule' monad, and every 'Rule' takes turns observing portions of the query and constructing a
-- reply.
type Query = [Object]

----------------------------------------------------------------------------------------------------

-- | 'Note's are structured comments that you can embed into your 'Rule's. This can be helpful for
-- debugging, and can also provied for a bit of meta-programming.
data Note
  = NoteBacktrack
  | NoteSuccess
  | NoteObject   Object
  | NoteError    ErrorObject
  | NoteSequence Note Note
  | NoteChoice   Note Note
  deriving (Eq, Ord, Show, Typeable)

instance Monoid (Sum Note) where
  mempty = Sum NoteBacktrack
  mappend (Sum a) (Sum b) = Sum $ case a of
    NoteBacktrack   -> b
    NoteChoice a a' -> NoteChoice a $ getSum $ Sum a' <> Sum b
    a               -> case b of
      NoteBacktrack   -> a
      b               -> NoteChoice a b

instance Monoid (Product Note) where
  mempty = Product NoteSuccess
  mappend (Product a) (Product b) = Product $ case a of
    NoteSuccess       -> b
    NoteSequence a a' -> NoteSequence a $ getProduct $ Product a' <> Product b
    a                 -> case b of
      NoteSuccess       -> a
      b                 -> NoteSequence a b

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
-- 'Dao.Object.ObjectError's can be thrown and caught. The 'Rule' monad instantiates
-- 'Dao.Logic.MonadLogic' so it is possible to pattern match with many
-- 'Control.Applicative.Alternative' branches of evaluation without having to worry about the
-- current state. 'Control.Monad.State.Class.MonadState' is instantiated giving you total control of
-- the state, along with the 'Dao.Logic.MonadLogic' functions. And
-- 'Control.Monad.Reader.Class.MonadReader' is instantiated so that the
-- 'Control.Monad.Reader.Class.local' function can be used to execute rules with a different input
-- in a different context without altering the current context.
data Rule m a
  = Rule Note (LogicT Query m (Either ErrorObject a))
  | RuleTree Note (T.Tree Object (Query -> Rule m a)) (T.Tree Object (Query -> Rule m a))
    -- DepthFirst and BreadthFirst rule trees are kept separate.

instance Functor m => Functor (Rule m) where
  fmap f rule = case rule of
    Rule     n o   -> Rule n $ fmap (fmap f) o
    RuleTree n x y -> let map o = fmap (fmap (fmap f)) o in RuleTree n (map x) (map y)

instance (Applicative m, Monad m) => Applicative (Rule m) where { pure = return; (<*>) = ap; }

instance (Applicative m, Monad m) => Alternative (Rule m) where { empty = mzero; (<|>) = mplus; }

instance (Functor m, Applicative m, Monad m) => Monad (Rule m) where
  return = Rule (getProduct mempty) . return . Right
  rule >>= f = case rule of
    Rule     n a   -> Rule n $ a >>= return . Left ||| evalRuleLogic . f
    RuleTree n x y -> let t = fmap (fmap (>>= f)) in RuleTree n (t x) (t y)
  fail msg = Rule (NoteError $ return $ obj msg) mzero

instance (Functor m, Applicative m, Monad m) => MonadPlus (Rule m) where
  mzero = Rule (getSum mempty) mzero
  mplus a b = let n a b = getSum $ Sum a <> Sum b in case a of
    Rule     nA xA    -> case b of
      Rule     nB xB    -> Rule (n nA nB) (mplus xA xB)
      RuleTree nB xB yB ->
        let t = fmap (fmap $ mplus a) in RuleTree (n nA nB) (t xB) (t yB)
    RuleTree nA xA yA -> case b of
      Rule     nB _     ->
        let t = fmap (fmap $ flip mplus b) in RuleTree (n nA nB) (t xA) (t yA)
      RuleTree nB xB yB ->
        let t = T.unionWith (\a b q -> mplus (a q) (b q))
        in  RuleTree (n nA nB) (t xA xB) (t yA yB)

instance (Functor m, Applicative m, Monad m) => MonadError ErrorObject (Rule m) where
  throwError o          = Rule (NoteError o) $ return $ Left o
  catchError rule catch = case rule of
    Rule     n x   -> Rule n $ x >>= (\ (Rule _ x) -> x) . catch ||| return . Right
    RuleTree n x y -> let c t = fmap (fmap (flip catchError catch)) t in RuleTree n (c x) (c y)

instance (Functor m, Applicative m, Monad m) => MonadState Query (Rule m) where
  state = Rule (getProduct mempty) . state . fmap (first Right)

instance (Functor m, Applicative m, Monad m) => MonadReader Query (Rule m) where
  ask = get;
  local f sub = state (\st -> (st, f st)) >>= \st ->
    mplus (sub >>= \o -> state (const (o, st))) (put st >> mzero)

instance (Functor m, Applicative m, Monad m) => MonadLogic Query (Rule m) where
  superState          = Rule (getProduct mempty) . superState . fmap (fmap (first Right))
  entangle rule = case rule of
    Rule     n x   -> Rule n $
      partitionEithers . fmap (\ (x, st) -> flip (,) st +++ flip (,) st $ x) <$>
        entangle x >>= \ (err, ok) -> superState (\st -> (Right ok, st) : (first Left <$> err))
    RuleTree n x y -> let e t = fmap (fmap entangle) t in RuleTree n (e x) (e y)

instance (Functor m, Applicative m, Monad m) => Monoid (Rule m o) where
  mempty = mzero
  mappend = mplus

-- | Evaluate a 'Rule' by flattening it's internal 'Dao.Tree.Tree' structure to a 'Dao.Logic.LogicT'
-- monad. This is probably not as useful of a function as 'queryAll' or 'query'.
evalRuleLogic
  :: (Functor m, Applicative m, Monad m)
  => Rule m a -> LogicT Query m (Either ErrorObject a)
evalRuleLogic rule = case rule of
  Rule     _ x   -> x
  RuleTree _ x y -> evalRuleLogic $ loop T.DepthFirst [] x <|> loop T.BreadthFirst [] y where
    runMap control qx map = msum $ flip fmap (M.assocs map) $ \ (o, tree) ->
      next >>= \q -> guard (objMatch o q) >> loop control (qx++[q]) tree
    loop control qx (T.Tree (rule, map)) =
      ((if control==T.DepthFirst then id else flip) mplus)
        (runMap control qx map)
        (maybe mzero ($ qx) rule)

-- | Run a 'Rule' against an @['Dao.Object.Object']@ query, return all successful results and all
-- exceptions that may have been thrown by 'Control.Monad.Except.throwError'.
queryAll :: (Functor m, Applicative m, Monad m) => Rule m a -> Query -> m [Either ErrorObject a]
queryAll rule = fmap (fmap fst) . runLogicT (evalRuleLogic rule)

-- | Run a 'Rule' against an @['Dao.Object.Object']@ query, return all successful results.
query :: (Functor m, Applicative m, Monad m) => Rule m a -> Query -> m [a]
query r = fmap (>>= (const [] ||| return)) . queryAll r

-- | Like 'query', but only returns the first successful result, if any.
query1 :: (Functor m, Applicative m, Monad m) => Rule m a -> Query -> m (Maybe a)
query1 r = fmap (\ox -> if null ox then Nothing else Just $ head ox) . query r

-- | You can write comments into your 'Rule'. This allows for a bit of meta-programming, where you
-- can run queries on rules, which search through the comments.
note :: ObjectData note => note -> Rule m a -> Rule m a
note txt rule = case rule of
  Rule     n x   -> Rule     (f n) x
  RuleTree n x y -> RuleTree (f n) x y
  where
    f = getProduct . mappend (Product $ NoteObject $ obj txt) . Product

-- | Take the next item from the query input, backtrack if there is no input remaining.
next :: (Functor m, Applicative m, Monad m) => Rule m Object
next = superState $ \ox -> if null ox then [] else [(head ox, tail ox)]

-- | Take as many of next items from the query input as necessary to make the reset of the 'Rule'
-- match the input query. This acts as kind of a Kleene star.
part :: (Functor m, Applicative m, Monad m) => Rule m Query
part = superState $ loop . flip (,) [] where
  loop (lo, ox) = case ox of { [] -> [(lo, [])]; o:ox -> (lo, ox) : loop (lo++[o], ox) }

-- | If a given 'Rule' successfully matches, evaluate to 'Control.Monad.mzero', otherwise evaluate
-- to @return ()@.
failIf :: (Functor m, Applicative m, Monad m) => Rule m a -> Rule m ()
failIf m = (m >> mzero) <|> done

-- | Logical exclusive-OR, matches one rule or the other, but not both. This function uses
-- 'Dao.Logic.entangle', so there is a small performance penalty as the lazy state must be evaluated
-- strictly, but this loss in performance could be regained by reducing the number of branches of
-- logic evaluation.
exclusive :: (Functor m, Applicative m, Monad m) => [Rule m a] -> Rule m a
exclusive = entangle . msum >=> \matched ->
  case matched of { [o] -> superState $ const [o]; _ -> mzero; }

-- | Given an 'Prelude.Int' value @n@, take only the first @n@ branches of logic evaluation. This
-- function uses 'Dao.Logic.entangle', so there is a small performance penalty as the lazy state
-- must be evaluated strictly, but this loss in performance could be regained by reducing the number
-- of branches of logic evaluation.
chooseOnly :: (Functor m, Applicative m, Monad m) => Int -> Rule m a -> Rule m a
chooseOnly n = entangle >=> superState . const . take n

-- | Match when there are no more arguments, backtrack if there are.
done :: (Functor m, Applicative m, Monad m) => Rule m ()
done = superState $ \ox -> if null ox then [((), [])] else []

-- | When an error is thrown using 'Control.Monad.Except.throwError', the error is not caught right
-- away, rather it lingers in the internal state until it can be caught while branches of execution
-- that have not thrown any errors are evaluated. As evaluation progresses and more and more errors
-- are thrown, the amount of space taken by errors in the internal state starts to grow. When using
-- 'Control.Monad.Except.catchError', all of these errors are applied to the given catching
-- function.
--
-- If you know you never want to catch any errors from a function that throws errors, and you just
-- want to delete them from the internal state, you can call this function.
--
-- This function is defined as:
--
-- @'Prelude.flip' 'Control.Monad.Except.catchError' ('Prelude.const' 'Control.Monad.mzero')@
--
dropErrors :: (Functor m, Applicative m, Monad m) => Rule m a -> Rule m a
dropErrors = flip catchError (const mzero)

----------------------------------------------------------------------------------------------------

-- $Rule_trees
-- One very good way to construct a knowledge base is to combine 'Dao.Tree.Tree's and
-- 'Dao.Rule.Rule's. When production 'Rule's are organized into 'Dao.Tree.Tree's, these monadic
-- functions tend to be much, much more time and space efficient, as 'Dao.Rule.Rule's that all
-- perform similar checks on the 'Dao.Rule.Query' can be automatically merged-together.
--
-- The 'Dao.Tree.Leaf'es of the 'Dao.Tree.Tree's are 'Dao.Rule.Rule' functions. 'Dao.Tree.Branch'es
-- of the 'Dao.Tree.Tree' are any arbitrary 'Dao.Object.Object', even 'Object's that wrap you own
-- custom data types which have instantiated the 'Dao.Object.ObjectData' type class. Evaluation of a
-- 'RuleTree' monad is nearly identical to evaluation of 'Dao.Rule.Rule's, the difference with
-- 'RuleTree's being that the 'Dao.Object.Object' 'Dao.Tree.Branch'es are mathced against input
-- 'Dao.Rule.Query', and matching 'Dao.Tree.Branch'es proceed with matching the next node until a
-- 'Dao.Tree.Leaf' node containing a 'Dao.Tree.Leaf' 'Dao.Rule.Rule' is found and evaluated.
--
-- When you construct a 'RuleTree', the 'Dao.Tree.Branch' 'Dao.Object.Object's are merged together
-- if they are equal according to the Prelude @('Prelude.==')@ predicate. However, when a 'RuleTree'
-- is evaluating a 'Dao.Rule.Query', the 'Dao.Object.objMatch' function is used instead. So of your
-- custom data type instantiates the 'Dao.Object.obj' function of the 'Dao.Object.ObjectData' class
-- using 'Dao.Object.matchable' to define a customized pattern matching predicate, your data type
-- can be used to match more than whichever portions of the 'Dao.Rule.Query' merely satsify the
-- @('Prelude.==')@ predicate.
--
-- The logical conjunction (set union/logical OR) of 'RuleTree's can be computed by way of
-- 'Data.Monoid.mappend'.

----------------------------------------------------------------------------------------------------

-- $Structure_of_a_rule
-- A 'Rule' is constructed from 'Dao.Tree.Tree' data types and functions. Some 

-- | This is the data type that models the branch structure of a 'Rule'. It is a 'Dao.Tree.Tree'
-- with 'Dao.Object.Object' paths and @()@ leaves. It is possible to perform modifications to some
-- 'Rule's, for example 'prune'-ing of branches, using a 'RuleStruct'.
type RuleStruct = T.Tree Object ()

-- | Take a list of lists of a type of 'Dao.Object.ObjectData' and construct a 'Rule' tree that will
-- match any 'Query' similar to this list of 'Dao.Object.ObjectData' values (using
-- 'Dao.Object.objMatch'). Every list of 'Dao.Object.ObjectData' will become a branch associated
-- with the given 'Rule' monadic function (constructed using the 'Dao.Tree.fromList' function). This
-- 'Rule' function must take a 'Query' as input. When a portion of a 'Query' matches the given
-- 'Dao.Object.ObjectData', the portion of the 'Query' that matched will be passed to this 'Rule'
-- function when it is evaluated.
tree
  :: (Functor m, Applicative m, Monad m, ObjectData a)
  => T.RunTree -> [[a]] -> (Query -> Rule m b) -> Rule m b
tree control branches f =
  ((if control==T.DepthFirst then id else flip) $ RuleTree NoteBacktrack)
    (T.fromList $ zip (fmap (fmap obj) branches) $ repeat f)
    mempty

-- | Construct a 'Rule' tree from a 'Dao.Tree.Tree' data type and a 'Rule' function. The 'Rule' will
-- be copied to every single 'Dao.Tree.Leaf' in the given 'Dao.Tree.Tree'.
struct
  :: (Functor m, Applicative m, Monad m)
  => T.RunTree -> RuleStruct -> (Query -> Rule m a) -> Rule m a
struct control tree rule =
  ((if control==T.DepthFirst then id else flip) $ RuleTree NoteBacktrack)
    (fmap (\ () -> rule) tree)
    mempty

-- | Remove all of the 'Rule's and return only the 'Dao.Tree.Tree' structure. This function cannot
-- retrieve the entire 'Dao.Tree.Tree', it can only see the 'Dao.Tree.Tree' created by the 'tree'
-- function, or some combination of rules created by the 'tree' function (for example two 'tree's
-- 'Control.Monad.mplus'sed together). 'Rule's created with functions like 'Control.Monad.return',
-- @('Control.Monad.>>=')@, @('Control.Applicative.<*>')@, 'Control.Monad.State.state',
-- 'Control.Monad.Trans.lift', and others all introduce opaque function data types into the leaves
-- which cannot be 'Data.Traversal.traverse'd.
getRuleStruct :: Functor m => Rule m o -> RuleStruct
getRuleStruct rule = case rule of
  RuleTree _ x y -> let tree o = fmap (const ()) o in T.union (tree x) (tree y)
  Rule     _ _   -> nullValue

-- | With a 'RuleStruct' delete any of the matching branches from the 'Rule' tree. Branch matching
-- uses the @('Prelude.==')@ predicate, not 'Dao.Object.objMatch'. This is the dual of 'mask' in
-- that @'prune' struct t 'Data.Monoid.<>' 'mask' struct t == t@ is always true.
-- This function works by calling 'Dao.Tree.difference' on the 'Rule' and the 'Dao.Tree.Tree'
-- constructed by the 'Dao.Tree.blankTree' of the given list of 'Dao.Object.ObjectData' branches.
--
-- This function will delve very deeply into the 'Rule', even 'prune'-ing sub-'Dao.Tree.Tree's that
-- are invisible to 'getRuleStruct'
prune :: RuleStruct -> Rule m a -> Rule m a
prune st rule = let del = fmap (fmap (prune st)) . flip T.difference st in case rule of
  Rule     n x   -> Rule     n x
  RuleTree n x y -> RuleTree n (del x) (del y)

-- | With 'RuleStruct' and delete any of the branches from the 'Rule' tree that do *not* match the
-- 'RuleStruct' This is the dual of 'prune' in that
-- @'prune' struct t 'Data.Monoid.<>' 'mask' struct t == t@ is always true. Branch matching uses the
-- @('Prelude.==')@ predicate, not 'Dao.Object.objMatch'. This function works by calling
-- 'Dao.Tree.intersection' on the 'Rule' and the 'Dao.Tree.Tree' constructed by the
-- 'Dao.Tree.blankTree' of the given list of 'Dao.Object.ObjectData' branches.
--
-- This function will delve very deeply into the 'Rule', even 'mask'ing sub-'Dao.Tree.Tree's that
-- are invisible to 'getRuleStruct'
mask :: RuleStruct -> Rule m a -> Rule m a
mask st rule = let del = fmap (fmap (mask st)) . flip T.intersection st in case rule of
  Rule     n x   -> Rule     n x
  RuleTree n x y -> RuleTree n (del x) (del y)

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

instance ObjectPattern TypePattern where { objMatch (TypePattern p) o = p == objTypeOf o; }

instance ObjectData TypePattern where
  obj p = obj $ printable p $ matchable p $ simplifyable p $ toForeign p
  fromObj = defaultFromObj

-- | Infer the type of the 'next' 'Dao.Object.Object' in the current 'Query', if the type inference
-- succeeds, evaluate a function on it. This function makes a new 'RuleTree' where the pattern in
-- the branch is a 'TypePattern'. For example, if you pass a function to 'infer' which is of the
-- type @('Prelude.String' -> 'Rule' m a)@, 'infer' will create a 'RuleTree' that matches if the
-- 'Dao.Object.Object' returned by 'next' can be cast to a value of 'Prelude.String'.
infer
  :: forall m t a . (Functor m, Applicative m, Monad m, Typeable t, ObjectData t)
  => (t -> Rule m a) -> Rule m a
infer f = tree T.BreadthFirst [[typ f err]] $ msum . fmap (fromObj >=> f) where
  typ :: (t -> Rule m a) -> t -> TypePattern
  typ _ ~t = TypePattern $ typeOf t
  err :: t
  err = error "in Dao.Rule.infer: typeOf evaluated undefined"

