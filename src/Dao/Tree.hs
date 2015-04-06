-- "Dao/Tree.hs"  provides a fundamental data type used by Dao.
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A "trie" based on 'Data.Map.Map' where you can store objects @o@ to an arbitrary path
-- constructed of paths-segments @p@. The idea of the 'Tree' data structure is that it behaves
-- exctly like a 'Data.Map.Map' where the keys of the map are of type @[p]@, but internally, similar
-- paths are merged together to save memory and 'Data.Traversable.traverse' time.
--
-- Because of the way similar paths @[p]@ are merged, when you perform a 'Data.Foldable.foldr',
-- 'mergeWithKey', or 'Data.Traversable.traverse' operation, you have a choice of how to order the
-- objects @o@, with 'DepthFirst' or 'BreadthFirst'. Functions like 'elems' and 'assocs' require an
-- additional 'RunTree' parameter to decide the ordering of the objects @o@.
--
-- Therefore, this data type instantiates 'Data.Foldable.Foldable' only when
-- it is paired with a 'RunTree' to determine if the 'Data.Foldable.foldr' will occur in
-- 'DepthFirst' or 'BreadthFirst' order.
module Dao.Tree where

import           Prelude hiding (id, (.), mapM, foldr, foldl, sum, concat)
import           Dao.TestNull

import           Dao.Lens

import           Control.Arrow
import           Control.Applicative
import           Control.Category
import           Control.DeepSeq
import           Control.Monad          hiding (mapM, msum)
import           Control.Monad.Identity hiding (mapM, msum)
import           Control.Monad.State    hiding (mapM, msum)

import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable
import qualified Data.Map as M
import           Data.Traversable
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | A 'Tree' is just a @newtype@ around a pair of two elements forming a node, the first being the
-- leaf of the node, and the second being the branches of the node. The leaf may or may not exist,
-- so it is wrapped in a 'Prelude.Maybe' data structure.
--
-- When you associate an object @o@ at a path @[p]@, a walk is performed, with each segment of the
-- path @[p]@ selecting a branch that contains another sub-node. When the path @[p]@ is empty, the
-- walk stops and the object @o@ is placed into the current sub-node.
newtype Tree p o = Tree (Maybe o, M.Map p (Tree p o)) deriving (Eq, Ord, Show, Typeable)

instance TestNull (Tree p o) where
  nullValue = Dao.Tree.empty
  testNull (Tree (o, m)) = M.null m && isNothing o

instance Functor (Tree p) where { fmap f (Tree (o, m)) = Tree (fmap f o, fmap (fmap f) m); }

instance (Ord p, Monoid o) => Monoid (Sum (Tree p o)) where
  mempty  = Sum nullValue
  mappend (Sum a) (Sum b) = Sum $ unionWith mappend a b

instance (Ord p, Monoid o) => Monoid (Product (Tree p o)) where
  mempty  = Product nullValue
  mappend (Product a) (Product b) = Product $ intersectionWith mappend a b

instance (NFData a, NFData b) => NFData (Tree a b) where
  rnf (Tree (o, m)) = deepseq o $! deepseq m ()

instance (Ord p, Monad m) => FocusesWith [p] m (Tree p o) (Maybe o) where { focus=path; }

instance Foldable (ReduceTree p) where
  foldr f b (ReduceTree control tree) = foldr f b $ elems control tree

instance Ord p => Traversable (ReduceTree p) where
  traverse f (ReduceTree control tree) = fmap (ReduceTree control . fromList) $
    traverse (\ (p, o) -> (,) <$> pure p <*> f o) $ assocs control tree

----------------------------------------------------------------------------------------------------

-- | This data type controls algorithms like 'mergeWithKeyM' where monadic evaluation needs to occur
-- in a certain order. This simple operation code decides whether evaluation of leaves happens
-- before evaluation of sub-'Tree's ('BreadthFirst') or whether evaluation of leaves happens after
-- evaluation of sub-'Tree's ('DepthFirst').
data RunTree
  = DepthFirst
    -- ^ will have the 'Rule' 'Dao.Tree.Leaf's evaluated such that the longest branches evaluate
    -- first.
  | BreadthFirst
    -- ^ will have the 'Rule' 'Dao.Tree.Leaf's evaluated such that the shortest branches evaluate
    -- first.
  deriving (Eq, Ord, Show, Typeable, Enum, Bounded)

-- | Like 'RunTree', but pairs the 'RunTree' value with the 'Tree' data type itself. This is used to
-- instantiate 'Data.Foldable.Foldable' and 'Data.Traversable.Traversable', which means in order to
-- use 'Data.Foldable.foldr' or 'Data.Traversable.traverse', it is first necessary to store the tree
-- in this data type along with the 'RunTree' operator indicating the order in which the leaf
-- objects @o@ will be retrieved.
data ReduceTree p o = ReduceTree{ reduceTreeBy :: RunTree, getReduced :: Tree p o }
  deriving (Eq, Ord, Show, Typeable)

instance Functor (ReduceTree p) where
  fmap f (ReduceTree control tree) = ReduceTree control $ fmap f tree

----------------------------------------------------------------------------------------------------

treePair :: (Monad m, Ord p) => Lens m (Tree p o) (Maybe o, M.Map p (Tree p o))
treePair = newLens (\ (Tree t) -> t) (\t (Tree _) -> Tree t)

empty :: Tree p o
empty = Tree (Nothing, M.empty)

leaf :: (Monad m, Ord p) => Lens m (Tree p o) (Maybe o)
leaf = treePair >>> tuple0

branches :: (Monad m, Ord p) => Lens m (Tree p o) (M.Map p (Tree p o))
branches = treePair >>> tuple1

-- | This is a focusing lens that focuses on a sub-'Tree' at a given path @[p]@. A function that
-- takes two 'Tree's and returns a 'Tree' is required for performing 'Dao.Lens.alter' or
-- 'Dao.Lens.update' operations. This function is not used on 'Dao.Lens.fetch' operations.
--
-- When 'Dao.Lens.update'ing or 'Dao.Lens.alter'ing, the tree in the focus of the 'Dao.Lens.Lens' is
-- passed as the first (left-hand) argument to the given altering function, and the sub-'Tree' found
-- at the path @[p]@ is passed as the second (right-hand) argument.
--
-- For most purposes, the 'Dao.Lens.alter'ing function you want to use is the 'union' function.
focusSubTree
  :: (Monad m, Ord p)
  => (Tree p o -> Tree p o -> m (Tree p o)) -> [p] -> Lens m (Tree p o) (Tree p o)
focusSubTree alt px =
  newLensM
    (\  target@(Tree (_, map)) -> case px of
      []    -> return target
      p:px  -> maybe (return nullValue) (fetch $ focusSubTree alt px) (M.lookup p map)
    )
    (case px of
      []    -> flip alt
      p:px  -> Dao.Lens.update $
        branches >>> mapLens p >>> maybeLens True (return nullValue) >>> focusSubTree alt px
    )

-- | Focuses on an individual leaf at the given path.
path :: (Monad m, Ord p) => [p] -> Lens m (Tree p o) (Maybe o)
path px =
  newLensM
    (\  (Tree (o, map)) -> case px of
      []    -> return o
      p:px  -> maybe (return Nothing) (fetch $ path px) (M.lookup p map)
    )
    (\o (Tree (n, map)) -> case px of
      []    -> return $ Tree (o, map)
      p:px  -> do
        t <- Dao.Lens.update (path px) o $ fromMaybe nullValue $ M.lookup p map
        return $ Tree (n, M.alter (const $ if testNull t then Nothing else Just t) p map)
    )

-- | This function merges two trees together, given a leaf-merging function that can optionally
-- create or remove leaves based on whether or not leaves exist on the left and right at any given
-- point in the path @[p]@.
--
-- Also required are two 'Tree' functions: a function that can convert the first (left) 'Tree'
-- parameter to a 'Tree' of the resultant type, and a function that can convert the second (right)
-- 'Tree' parameter to a 'Tree' of the resultant type. These functions are used for when leaves
-- exist only on the left 'Tree', or for when leaves only exist on the right 'Tree'.
--
-- The given leaf-merging function is called for every single sub-'Tree' node where the path @[p]@
-- exists in both the overlay and target 'Tree's. Each sub-'Tree' node may or may not have a 'Leaf'.
--
-- * If the 'Tree' node for the overlay 'Tree' and the target 'Tree' are both without leaves, the
--   merging function is passed 'Prelude.Nothing' as both arguments to the updating function. 
-- * If only the target 'Tree' has a 'Leaf', the overlay 'Leaf' as passed with 'Prelude.Just' as the
--   first (left) argument to the updating function, and 'Prelude.Nothing' is passed as the second
--   (right) argument.
-- * If only the overlay 'Tree' has a leaf, 'Prelude.Nothing' is passed as the first (left) argument
--   to the merging function, and the overlay 'Leaf' is passed with 'Prelude.Just' as the second
--   (right) argument.
-- * If both the target and the overlay 'Tree's have 'Leaf's, both 'Leaf's are passed with
--   'Prelude.Just' to the merging function.
--
-- Also, it is necessary to specify (as the first parameter to this function) the 'RunTree' type,
-- which indicates 'DepthFirst' or 'BreadthFirst' evaluation.
mergeWithKeyM
  :: forall m p a b c . (Monad m, Ord p)
  => RunTree
  -> ([p] -> Maybe a -> Maybe b -> m (Maybe c))
  -> (Tree p a -> m (Tree p c))
  -> (Tree p b -> m (Tree p c))
  -> Tree p a -> Tree p b -> m (Tree p c)
mergeWithKeyM control = loop [] where
  loop px merge left right (Tree (leftLeaf, leftBranches)) (Tree (rightLeaf, rightBranches)) = do
    let leaf = merge px leftLeaf rightLeaf
    let map  = liftM (M.fromList . concat) $
           mapM (\ (p, leftIfPaired) -> do
                  tree <- uncurry (loop (px++[p]) merge left right) ||| id $ leftIfPaired
                  return $ if testNull tree then [] else [(p, tree)]
                )
                ( let wrap f = fmap (Right . f) in M.assocs $ 
                    M.mergeWithKey (\ _ a b -> Just $ Left (a, b))
                      (wrap left) (wrap right) leftBranches rightBranches
                )
    if control==BreadthFirst
    then ap (ap (return $        curry Tree) leaf) map
    else ap (ap (return $ flip $ curry Tree) map) leaf

----------------------------------------------------------------------------------------------------
-- $MapLikeFunctions
-- In this section I have made my best effor to create API functions as similar as possible to that
-- of the "Data.Map" module.
----------------------------------------------------------------------------------------------------

alter :: Ord p => (Maybe o -> Maybe o) -> [p] -> Tree p o -> Tree p o
alter f p = runIdentity . alterM (return . f) p

alterM :: (Monad m, Ord p) => (Maybe o -> m (Maybe o)) -> [p] -> Tree p o -> m (Tree p o)
alterM f p = Dao.Lens.alter (path p) f >=> return . snd

-- | Insert a leaf at a given address, updating it with the combining function if it already exist.
insertWith :: Ord p => (o -> o -> o) -> [p] -> o -> Tree p o -> Tree p o
insertWith append p o = Dao.Tree.alter (Just . maybe o (`append` o)) p 

-- | Insert a leaf at a given address.
insert :: Ord p => [p] -> o -> Tree p o -> Tree p o
insert = insertWith (flip const)

-- | Update a leaf at a given address.
update :: Ord p => (o -> Maybe o) -> [p] -> Tree p o -> Tree p o
update = Dao.Tree.alter . maybe Nothing

-- | Delete a leaf or 'Branch' at a given address.
delete :: Ord p => [p] -> Tree p o -> Tree p o
delete = Dao.Tree.alter (const Nothing)

-- | Create a 'Tree' from a list of associationes, the 'Prelude.fst' element containing the branches,
-- the 'Prelude.snd' element containing the leaf value. This is the inverse operation of 'assocs'.
fromListWith :: Ord p => (o -> o -> o) -> [([p], o)] -> Tree p o
fromListWith append = foldr (uncurry $ insertWith append) nullValue

-- | Like 'fromListWith' but called with @('Prelude.flip' 'Prelude.const')@.
fromList :: Ord p => [([p], o)] -> Tree p o
fromList = fromListWith (flip const)

-- | Create a 'Tree' with @()@ nodes. This is useful for times when the structure of the tree is all
-- you need.
blankTree :: Ord p => [[p]] -> Tree p ()
blankTree = fromList . fmap (id &&& const ())

-- | Create a 'Tree' containing only a single 'path' to a single element.
singleton :: Ord p => [p] -> a -> Tree p a
singleton p o = new [path p <~ Just o]

-- | This function analogous to the 'Data.Map.lookup' function, which returns a value stored in a
-- leaf, or nothing if there is no leaf at the given path.
lookup :: Ord p => [p] -> Tree p a -> Maybe a
lookup px = (~> (path px))

-- | This function works like 'lookup', but takes a key predicate to match keys of the tree, rather
-- than using @('Prelude.==')@. This means the efficient O(log n) 'Data.Map.Map' 'Data.Map.lookup'
-- function in the "Data.Map" module cannot be used, each key must be inspected one-by-one making
-- this algorithm O(n^2). This also means multiple values may match the given key predicate. Lookups
-- are always performed in 'DepthFirst' order, this helps improve efficiency a little bit, as the
-- matches nearest the beggining of each list of 'Data.Map.assocs' are chosen first, and lazily
-- taking only the first few matches will save us from searching the entire tree.
--
-- Take note of the different types @p@ and @b@. This means the path @p@ you use to search the
-- 'Tree' need not be the same type as the branches @b@ of the 'Tree', and what is returned are the
-- actual branches @b@ that matched the path @p@, not the path @p@ itself.
slowLookup :: Ord b => (p -> b -> Bool) -> [p] -> Tree b a -> [([b], a)]
slowLookup f = loop [] where
  loop branchPath px t = case px of
    []   -> maybe [] (\o -> [(branchPath, o)]) $ t~>leaf
    p:px -> do
      (b, t) <- filter (f p . fst) (M.assocs $ t~>branches)
      loop (branchPath++[b]) px t

-- | This function calls 'slowLookup' and returns only the first result. This can be used to take
-- advantage of Haskell's laziness and save time by halting the search for matching paths as soon as
-- the first match is found.
slowLookup1 :: Ord b => (p -> b -> Bool) -> [p] -> Tree b a -> Maybe ([b], a)
slowLookup1 f p t = case slowLookup f p t of { [] -> Nothing; o:_ -> Just o; }

-- | Get all items and their associated path.
assocs :: RunTree -> Tree p a -> [([p], a)]
assocs control = loop [] where
  loop px (Tree (o, m)) =
    (if control==BreadthFirst then id else flip) (++)
      (maybe [] (return . (,) px) o)
      (M.assocs m >>= \ (p, o) -> loop (px++[p]) o)

-- | Apply @'Prelude.map' 'Prelude.snd'@ to the result of 'assocs', behaves just like how
-- 'Data.Map.elems' or 'Data.Array.IArray.elems' works.
elems :: RunTree -> Tree p a -> [a]
elems control = loop where
  append = (case control of{ DepthFirst -> flip; BreadthFirst -> id; }) (++)
  loop (Tree (a, m)) = append (maybe [] return a) $ M.elems m >>= loop
  -- This function is not implemented in terms of 'assocs' to avoid stacking the paths, as the paths
  -- will be ignored.

-- | Counts the number of *nodes*, which includes the number of 'Branch'es and 'Leaf's.
size :: Tree p a -> Word64
size (Tree (o, m)) = maybe 0 (const 1) o + sum (size <$> M.elems m)

leafCount :: Tree p a -> Word64
leafCount = sum . fmap (const 1) . ReduceTree DepthFirst

-- | Counts the number of branches, not leaves.
branchCount :: Tree p a -> Word64
branchCount (Tree (_, m)) = fromIntegral (M.size m) + sum (branchCount <$> M.elems m)

null :: Tree p a -> Bool
null = testNull

----------------------------------------------------------------------------------------------------

-- | Since this function does not merge trees monadically, it is not important whether merging
-- happens in 'DepthFirst' or 'BreadthFirst' order.
mergeWithKey
  :: Ord p
  => ([p] -> Maybe a -> Maybe b -> Maybe c)
  -> (Tree p a -> Tree p c)
  -> (Tree p b -> Tree p c)
  -> Tree p a -> Tree p b -> Tree p c
mergeWithKey a b c d e = runIdentity $
  mergeWithKeyM BreadthFirst (\k o -> return . a k o) (return . b) (return . c) d e

mergeWithM
  :: (Monad m, Ord p)
  => RunTree
  -> (Maybe a -> Maybe b -> m (Maybe c))
  -> (Tree p a -> m (Tree p c))
  -> (Tree p b -> m (Tree p c))
  -> Tree p a -> Tree p b -> m (Tree p c)
mergeWithM control f = mergeWithKeyM control (const f)

mergeWith
  :: Ord p
  => (Maybe a -> Maybe b -> Maybe c)
  -> (Tree p a -> Tree p c)
  -> (Tree p b -> Tree p c)
  -> Tree p a -> Tree p b -> Tree p c
mergeWith f = mergeWithKey (const f)

----------------------------------------------------------------------------------------------------

unionWithKeyM
  :: (Monad m, Ord p)
  => RunTree
  -> ([p] -> a -> a -> m a)
  -> Tree p a -> Tree p a -> m (Tree p a)
unionWithKeyM control f =
  mergeWithKeyM control
    (\k a b -> maybe (return Nothing) (>>= (return . Just)) $
        (f <$> pure k <*> a <*> b) <|> fmap return a <|> fmap return b
      ) return return

unionWithKey :: Ord p => ([p] -> a -> a -> a) -> Tree p a -> Tree p a -> Tree p a
unionWithKey f a = runIdentity . unionWithKeyM BreadthFirst (\k a -> return . f k a) a

unionWithM :: (Monad m, Ord p) => RunTree -> (a -> a -> m a) -> Tree p a -> Tree p a -> m (Tree p a)
unionWithM control f = unionWithKeyM control (const f)

unionWith :: Ord p => (a -> a -> a) -> Tree p a -> Tree p a -> Tree p a
unionWith f a = runIdentity . unionWithM BreadthFirst (\a -> return . f a) a

union :: Ord p => Tree p a -> Tree p a -> Tree p a
union = unionWith const

unionsWith :: Ord p => (a -> a -> a) -> [Tree p a] -> Tree p a
unionsWith overlap = foldl (unionWith overlap) nullValue

unions :: Ord p => [Tree p a] -> Tree p a
unions = unionsWith (flip const)

----------------------------------------------------------------------------------------------------

intersectionWithKeyM
  :: (Monad m, Ord p)
  => RunTree
  -> ([p] -> a -> b -> m c)
  -> Tree p a -> Tree p b -> m (Tree p c)
intersectionWithKeyM control f =
  mergeWithKeyM control
    (\k a b -> maybe (return Nothing) (>>= (return . Just)) $ f <$> pure k <*> a <*> b)
      (return . const nullValue) (return . const nullValue)

intersectionWithKey :: Ord p => ([p] -> a -> b -> c) -> Tree p a -> Tree p b -> Tree p c
intersectionWithKey f a = runIdentity . intersectionWithKeyM BreadthFirst (\k a -> return . f k a) a

intersectionWithM :: (Monad m, Ord p) => RunTree -> (a -> b -> m c) -> Tree p a -> Tree p b -> m (Tree p c)
intersectionWithM control f = intersectionWithKeyM control (const f)

intersectionWith :: Ord p => (a -> b -> c) -> Tree p a -> Tree p b -> Tree p c
intersectionWith f a = runIdentity . intersectionWithM BreadthFirst (\a -> return . f a ) a

intersection :: Ord p => Tree p a -> Tree p b -> Tree p a
intersection = intersectionWith const

intersectionsWith :: Ord p => (a -> a -> a) -> [Tree p a] -> Tree p a
intersectionsWith overlap = foldl (intersectionWith overlap) nullValue

intersections :: Ord p => [Tree p a] -> Tree p a
intersections = intersectionsWith (flip const)

----------------------------------------------------------------------------------------------------

differenceWithKeyM
  :: (Monad m, Ord p)
  => RunTree
  -> ([p] -> a -> b -> m (Maybe a))
  -> Tree p a -> Tree p b -> m (Tree p a)
differenceWithKeyM control f =
  mergeWithKeyM control
    (\k a b -> fromMaybe (return Nothing) $ (f <$> pure k <*> a <*> b) <|> fmap (return . Just) a)
      return (return . const nullValue)

differenceWithKey :: Ord p => ([p] -> a -> b -> Maybe a) -> Tree p a -> Tree p b -> Tree p a
differenceWithKey f a = runIdentity . differenceWithKeyM BreadthFirst (\k a -> return . f k a) a

differenceWithM :: (Monad m, Ord p) => RunTree -> (a -> b -> m (Maybe a)) -> Tree p a -> Tree p b -> m (Tree p a)
differenceWithM control f = differenceWithKeyM control (const f)

differenceWith :: Ord p => (a -> b -> Maybe a) -> Tree p a -> Tree p b -> Tree p a
differenceWith f a = runIdentity . differenceWithM BreadthFirst (\a -> return . f a) a

difference :: Ord p => Tree p a -> Tree p b -> Tree p a
difference = differenceWith (\ _ _ -> Nothing)

differencesWith :: Ord p => (a -> a -> Maybe a) -> [Tree p a] -> Tree p a
differencesWith overlap = foldl (differenceWith overlap) nullValue

differences :: Ord p => [Tree p a] -> Tree p a
differences = differencesWith (\ _ _ -> Nothing)

----------------------------------------------------------------------------------------------------

-- | This function computes the cartesian of two trees. For example, if the 'assocs' of two trees
-- are:
--
-- > -- tree X              tree Y
-- > [( [a, b, c], t ),  [( [b, c], w ),
-- >  ( [a, b   ], u ),   ( [a   ], x )]
-- >  ( [b      ], v )]
--
-- Then the 'product' of these two trees X and Y is the evaluation of 'fromList' on:
--
-- > [( [a, b, c] ++ [b, c], t<>w ),
-- >  ( [a, b, c] ++ [a   ], t<>x ),
-- >  ( [a, b,  ] ++ [b, c], u<>w ),
-- >  ( [a, b,  ] ++ [a,  ], u<>x ),
-- >  ( [b,     ] ++ [b, c], v<>w ),
-- >  ( [b,     ] ++ [a   ], v<>x )]
--
productWith :: Ord p => (a -> b -> c) -> Tree p a -> Tree p b -> Tree p c
productWith append a b = fromList $ do
  (pA, oA) <- assocs BreadthFirst a
  (pB, oB) <- assocs BreadthFirst b
  [(pA++pB, append oA oB)]

-- | Like 'productWith' but uses 'Data.Monoid.mappend' as the function that computes the product of
-- each element.
product :: (Ord p, Monoid a) => Tree p a -> Tree p a -> Tree p a
product = productWith mappend

----------------------------------------------------------------------------------------------------

-- | If you have read the chapter about zippers in "Learn You a Haskell for Great Good", you might
-- appreciate that a zipper is provided for 'Tree' in this module, and a number of useful
-- "Control.Monad.State"ful APIs are also provided, namely 'goto' and 'back'.
-- 
-- Although it should be noted usually, 'Dao.Lens.Lens'es, 'Data.Foldable.fold's,
-- 'Data.Traversable.traversal's, and 'mergeWithKeyM' are all you will need.
data ZipTree p o = ZipTree (Tree p o) [(p, Tree p o)] deriving (Eq, Ord, Typeable)

zipperSubTree :: Monad m => Lens m (ZipTree p o) (Tree p o)
zipperSubTree = newLens (\ (ZipTree t _) -> t) (\t (ZipTree _ h) -> ZipTree t h)

zipperHistory :: Monad m => Lens m (ZipTree p o) [(p, Tree p o)]
zipperHistory = newLens (\ (ZipTree _ h) -> h) (\h (ZipTree t _) -> ZipTree t h)

-- | A monadic function type that keeps the 'ZipTree' in a 'Control.Monad.State.StateT' for you, and
-- instantiates 'Control.Monad.State.MonadState' such that 'Control.Monad.State.get' and
-- 'Control.Monad.State.put' operate on leaves of the 'Tree'. Use 'goto', 'back', and 'home' to
-- navigate the 'Tree'.
newtype UpdateTreeT p o m a = UpdateTreeT (StateT (ZipTree p o) m a)
type UpdateTree  p o   a = UpdateTreeT p o Identity a

instance Functor m => Functor (UpdateTreeT p o m) where
  fmap f (UpdateTreeT o) = UpdateTreeT $ fmap f o

instance (Functor m, Monad m) => Applicative (UpdateTreeT p o m) where { pure=return; (<*>) = ap; }

instance Monad m => Monad (UpdateTreeT p o m) where
  return = UpdateTreeT . return
  (UpdateTreeT o) >>= f = UpdateTreeT $ o >>= (\ (UpdateTreeT o) -> o) . f

instance (Ord p, Monad m) => MonadState (Maybe o) (UpdateTreeT p o m) where
  state f = UpdateTreeT $ StateT $ \st -> do
    (a, l) <- return $ f $ st~>zipperSubTree~>leaf
    return (a, with st [zipperSubTree >>> leaf <~ l])

-- | Run the 'UpdateTreeT' function, returning the modified 'Tree' and the last result returned by
-- the 'UpdateTreeT' function.
runUpdateTreeT :: (Functor m, Applicative m, Monad m, Ord p) => UpdateTreeT p o m a -> Tree p o -> m (a, Tree p o)
runUpdateTreeT f tree = do
  (a, z) <- runStateT ((\ (UpdateTreeT f) -> f) $ f <* home) $ ZipTree tree []
  return (a, z~>zipperSubTree)

-- | Analogous to 'Control.Monad.State.execStateT', does the same thing as 'runUpdateTreeT' but
-- disgards the final return value of the 'UpdateTreeT' function.
execUpdateTreeT :: (Functor m, Applicative m, Monad m, Ord p) => UpdateTreeT p o m a -> Tree p o -> m (Tree p o)
execUpdateTreeT f = fmap snd . runUpdateTreeT f

-- | Analogous to 'Control.Monad.State.execStateT', does the same thing as 'runUpdateTreeT' but
-- disgards the updated 'Tree' and only keeps the last return value of the 'UpdateTreeT' function.
evalUpdateTreeT :: (Functor m, Applicative m, Monad m, Ord p) => UpdateTreeT p o m a -> Tree p o -> m a
evalUpdateTreeT f = runUpdateTreeT f >=> return . fst

-- | Go to the node with the given path. If the path does not exist, it is created.
goto :: (Functor m, Applicative m, Monad m, Ord p) => [p] -> UpdateTreeT p o m ()
goto px = case px of
  []       -> return ()
  (p:px) -> do
    UpdateTreeT $ do
      t <- gets $ fromMaybe nullValue . M.lookup p . (~> (branches . zipperSubTree))
      modify (\st -> with st [zipperSubTree <~ t, zipperHistory $= ((p, st~>zipperSubTree) :)])
    goto px

-- | Go up one level in the tree, storing the current sub-tree into the upper tree, unless the
-- current tree is 'Void', in which case it is deleted from the upper tree. Returns 'Prelude.False'
-- if we are already at the root of the 'Tree' and could not go back.
back :: (Functor m, Applicative m, Monad m, Ord p) => UpdateTreeT p o m Bool
back = UpdateTreeT $ state $ \st -> case st~>zipperHistory of
  []                    -> (False, st)
  (p, Tree (t, m)):hist -> (,) True $ let u = st~>zipperSubTree in with st
    [ zipperSubTree <~ Tree (t, (if testNull u then id else M.insert p u) m)
    , zipperHistory <~ hist
    ]

-- | Returns 'Prelude.True' if we are at the top level of the tree.
atTop :: (Functor m, Applicative m, Monad m) => UpdateTreeT p o m Bool
atTop = Prelude.null <$> UpdateTreeT (gets (~> zipperHistory))

-- | Go back to the top level of the tree.
home :: (Functor m, Applicative m, Monad m, Ord p) => UpdateTreeT p o m ()
home = atTop >>= flip unless (back >> home)

-- | Return the current path.
getPath :: (Functor m, Applicative m, Monad m, Ord p) => UpdateTreeT p o m [p]
getPath = reverse . fmap fst <$> UpdateTreeT (gets (~> zipperHistory))

----------------------------------------------------------------------------------------------------

-- | This data type lets you store a "diff", that is a structure tracking the differences, between
-- two 'Tree's. This is essentially the result of a 'mergeWithKeyM' operation tracking all of the
-- changes that would happen in a data structure without actually applying the changes. Traversing
-- over the 'Tree' of 'TreeDiff's with 'Data.Traversable.traverse' to actually convert the
-- 'TreeDiff's would then apply the changes.
data TreeDiff a b
  = LeftOnly  a -- something exists in the "left" branches but not in the "right" branches.
  | RightOnly b -- something exists in the "right" branches but not in the "left" branches.
  | TreeDiff  a b -- something exists in the "left" and "right" branches but they are not equal
  deriving (Eq, Typeable)

-- | Produce a difference report of two trees with the given comparison predicate. If the predicate
-- returns 'Prelude.True', the node does not appear in the resultant 'Tree'. If there is a
-- difference, the difference is recored into a node in the resultant 'Tree'.
treeDiffWithM
  :: forall m p a b . (Monad m, Ord p)
  => RunTree
  -> ([p] -> a -> b -> m Bool)
  -> Tree p a -> Tree p b -> m (Tree p (TreeDiff a b))
treeDiffWithM control compare =
  mergeWithKeyM control merge (return . fmap LeftOnly) (return . fmap RightOnly) where
    merge p a b = fromMaybe (return Nothing) $ msum
      [ a >>= \a -> b >>= \b -> return $
          compare p a b >>= \same -> return $ if same then Nothing else return $ TreeDiff a b
      , a >>= Just . return . Just . LeftOnly
      , b >>= Just . return . Just . RightOnly
      ]

treeDiffWith :: Ord p => ([p] -> a -> b -> Bool) -> Tree p a -> Tree p b -> Tree p (TreeDiff a b)
treeDiffWith f a = runIdentity . treeDiffWithM BreadthFirst (\p a -> return . f p a) a

-- | Call 'treeDiffWith' using 'Prelude.(==)' as the comparison predicate.
treeDiffM :: (Monad m, Eq a, Ord p) => RunTree -> Tree p a -> Tree p a -> m (Tree p (TreeDiff a a))
treeDiffM control = treeDiffWithM control (\ _ a -> return . (a ==))

-- | Call 'treeDiffWith' using 'Prelude.(==)' as the comparison predicate.
treeDiff :: (Eq a, Ord p) => Tree p a -> Tree p a -> Tree p (TreeDiff a a)
treeDiff a = runIdentity . treeDiffM BreadthFirst a

