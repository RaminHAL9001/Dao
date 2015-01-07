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
    Rule, queryAll, query, query1, note, next, part, failIf, exclusive, chooseOnly, done,
    dropErrors,
    -- * Commenting Your 'Rule's with 'Note's
    Note(NoteBacktrack, NoteSuccess, NoteObject, NoteError, NoteSequence, NoteChoice),
    -- * Rule Trees
    RuleTree(RuleTree), tree, bindRule, bindTree,
    runTree, getTreeStruct, copyRuleToLeaves,
    deleteSubRuleTree, extractSubRuleTree,
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
data Rule m a = Rule Note (LogicT Query m (Either ErrorObject a))

instance Functor m => Functor (Rule m) where { fmap f (Rule n o) = Rule n $ fmap (fmap f) o; }

instance (Applicative m, Monad m) => Applicative (Rule m) where
  pure = return
  (<*>) = ap

instance (Applicative m, Monad m) => Alternative (Rule m) where
  empty = mzero
  (<|>) = mplus

instance (Functor m, Applicative m, Monad m) => Monad (Rule m) where
  return = Rule (getProduct mempty) . return . Right
  (Rule n a) >>= b = Rule n $ a >>= return . Left ||| (\ (Rule _ b) -> b) . b
  fail msg = Rule (NoteError $ return $ obj msg) mzero

instance (Functor m, Applicative m, Monad m) => MonadPlus (Rule m) where
  mzero = Rule (getSum mempty) mzero
  mplus (Rule nA a) (Rule nB b) = Rule (getSum $ Sum nA <> Sum nB) $ mplus a b

instance (Functor m, Applicative m, Monad m) => MonadError ErrorObject (Rule m) where
  throwError         o        = Rule (NoteError o) $ return $ Left o
  catchError (Rule n o) catch = Rule n $ o >>= (\ (Rule _ o) -> o) . catch ||| return . Right

instance (Functor m, Applicative m, Monad m) => MonadState Query (Rule m) where
  state = Rule (getProduct mempty) . state . fmap (first Right)

instance (Functor m, Applicative m, Monad m) => MonadReader Query (Rule m) where
  ask = get;
  local f sub = state (\st -> (st, f st)) >>= \st ->
    mplus (sub >>= \o -> state (const (o, st))) (put st >> mzero)

instance (Functor m, Applicative m, Monad m) => MonadLogic Query (Rule m) where
  superState          = Rule (getProduct mempty) . superState . fmap (fmap (first Right))
  entangle (Rule n o) = Rule n $
    partitionEithers . fmap (\ (o, st) -> flip (,) st +++ flip (,) st $ o) <$>
      entangle o >>= \ (err, ok) -> superState (\st -> (Right ok, st) : (first Left <$> err))

instance (Functor m, Applicative m, Monad m) => Monoid (Rule m o) where
  mempty = mzero
  mappend = mplus

-- | Run a 'Rule' against an @['Dao.Object.Object']@ query, return all successful results and all
-- exceptions that may have been thrown.
queryAll :: (Functor m, Applicative m, Monad m) => Rule m a -> Query -> m [Either ErrorObject a]
queryAll (Rule _ o) = fmap (fmap fst) . runLogicT o

-- | Run a 'Rule' against an @['Dao.Object.Object']@ query, return all successful results.
query :: (Functor m, Applicative m, Monad m) => Rule m a -> Query -> m [a]
query r = fmap (>>= (const [] ||| return)) . queryAll r

-- | Like 'query', but only returns the first successful result, if any.
query1 :: (Functor m, Applicative m, Monad m) => Rule m a -> Query -> m (Maybe a)
query1 r = fmap (\ox -> if null ox then Nothing else Just $ head ox) . query r

-- | You can write comments into your 'Rule'. This allows for a bit of meta-programming, where you
-- can run queries on rules, which search through the comments.
note :: ObjectData note => note -> Rule m a -> Rule m a
note txt (Rule n o) = Rule (getProduct $ Product (NoteObject $ obj txt) <> Product n) o

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

-- | One very good way to construct a knowledge base is to combine 'Dao.Tree.Tree's and
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
newtype RuleTree m o = RuleTree (T.Tree Object (Query -> Rule m o))

instance Functor m => Functor (RuleTree m) where
  fmap f (RuleTree t) = RuleTree $ fmap (fmap (fmap f)) t

instance (Functor m, Applicative m, Monad m) => Monoid (RuleTree m o) where
  mempty                            = RuleTree nullValue
  mappend (RuleTree a) (RuleTree b) = RuleTree $ T.unionWith (\a b q -> mplus (a q) (b q)) a b

-- | Take a list of lists of a type of 'Dao.Object.ObjectData' and construct a 'RuleTree' that will
-- match any 'Query' similar to this list of 'Dao.Object.ObjectData' values (using
-- 'Dao.Object.objMatch'). Every list of 'Dao.Object.ObjectData' will become a branch associated
-- with the given 'Rule' monadic function (constructed using the 'Dao.Tree.fromList' function). This
-- 'Rule' function must take a 'Query' as input. When a portion of a 'Query' matches the given
-- 'Dao.Object.ObjectData', the portion of the 'Query' that matched will be passed to this 'Rule'
-- function when it is evaluated.
tree :: (Functor m, Applicative m, Monad m, ObjectData a) => [[a]] -> (Query -> Rule m b) -> RuleTree m b
tree = flip $ \f -> RuleTree . T.fromList . fmap (fmap obj &&& const f)

-- | Convert a 'RuleTree' to a 'Rule' that evaluates the 'Rule' 'Dao.Tree.Leaf's of the 'RuleTree'.
runTree :: (Functor m, Applicative m, Monad m) => T.RunTree -> RuleTree m a -> Rule m a
runTree control (RuleTree tree) = loop [] tree where
  runMap qx map = msum $ flip fmap (M.assocs map) $ \ (o, tree) ->
    next >>= \q -> guard (objMatch o q) >> loop (qx++[q]) tree
  loop qx (T.Tree (rule, map)) =
    ((if control==T.DepthFirst then id else flip) mplus) (runMap qx map) (maybe mzero ($ qx) rule)

-- | Modify every 'Dao.Tree.Leaf' 'Rule' with a given function by binding the evaluation of each
-- 'Dao.Tree.Leaf' 'Rule' to the function @(a -> 'Rule' m b)@.
--
-- @
-- myRuleTree :: 'RuleTree' m a
-- myRuleTree = ...
-- 
-- updatedRuleTree :: 'RuleTree' m b
-- updatedRuleTree = 'bindRule' myRuleTree $ \\a -> do
--     ...
--     return b
-- @
--
bindRule :: (Functor m, Applicative m, Monad m) => RuleTree m a -> (a -> Rule m b) -> RuleTree m b
bindRule (RuleTree o) f = RuleTree $ fmap (fmap (>>= f)) o

-- | Like 'bindRule' but binds to a 'RuleTree' instead of a 'Rule'. This is done by calling
-- 'runTree' on the bound 'RuleTree' and passing the resulting 'Rule' to 'bindRule'. Since 'runTree'
-- is evaluated, so you must specify a 'Dao.Tree.RunTree' parameter. This function is defined as:
--
-- @
-- \\control binder bindee  -> 'bindRule' binder $ \\a -> 'runTree' control (bindee a)
-- @
bindTree
  :: (Functor m, Applicative m, Monad m)
  => T.RunTree -> RuleTree m a -> (a -> RuleTree m b) -> RuleTree m b
bindTree control tree f = bindRule tree $ \a -> runTree control (f a)

-- | Remove all of the 'Rule's and return only the 'Dao.Tree.Tree' structure.
getTreeStruct :: RuleTree m o -> T.Tree Object ()
getTreeStruct (RuleTree tree) = fmap (const ()) tree

-- | Construct a 'RuleTree' from a 'Dao.Tree.Tree' data type and a 'Rule' function. The 'Rule' will
-- be copied to every single 'Dao.Tree.Leaf' in the given 'Dao.Tree.Tree'.
copyRuleToLeaves :: (Functor m, Applicative m, Monad m) => T.Tree Object () -> Rule m a -> RuleTree m a
copyRuleToLeaves tree rule = RuleTree $ fmap (\ () -> const rule) tree

-- | Take a sequence of branches and delete any of the matching branches from the 'RuleTree'.
-- Matching uses the @('Prelude.==')@ predicate. This function works by calling
-- 'Dao.Tree.difference' on the 'RuleTree' and the 'Dao.Tree.Tree' constructed by the
-- 'Dao.Tree.blankTree' of the given list of 'Dao.Object.ObjectData' branches.
deleteSubRuleTree :: ObjectData o => [[o]] -> RuleTree m a -> RuleTree m a
deleteSubRuleTree = flip $ \ (RuleTree tree) ->
  RuleTree . T.difference tree . T.blankTree . (fmap (fmap obj))

-- | Take a sequence of branches and keep only any of the matching branches from the 'RuleTree',
-- disgarding anything else that does not match any of the branches. Matching uses the
-- @('Prelude.==')@ predicate. This function works by calling 'Dao.Tree.intersection' on the
-- 'RuleTree' and the 'Dao.Tree.Tree' constructed by the 'Dao.Tree.blankTree' of the given list of
-- 'Dao.Object.ObjectData' branches.
extractSubRuleTree
  :: (Functor m, Applicative m, Monad m, ObjectData o)
  => [[o]] -> RuleTree m a -> RuleTree m a
extractSubRuleTree = flip $ \ (RuleTree tree) ->
  RuleTree . T.intersection tree . fmap (\ () a -> a) . T.blankTree . fmap (fmap obj)

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
  => (t -> Rule m a) -> RuleTree m a
infer f = tree [[typ f err]] $ msum . fmap (fromObj >=> f) where
  typ :: (t -> Rule m a) -> t -> TypePattern
  typ _ ~t = TypePattern $ typeOf t
  err :: t
  err = error "in Dao.Rule.infer: typeOf evaluated undefined"

