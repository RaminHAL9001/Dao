-- "src/Dao/Tree.hs"  provides a fundamental data type used in the Dao
-- System, the "Tree", which is similar to the "Data.Map" data type.
-- 
-- Copyright (C) 2008-2013  Ramin Honary.
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


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dao.Tree where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.State

import           Data.Typeable
import           Data.Monoid
import           Data.List (intercalate)
-- import           Data.Binary
import qualified Data.Map as M
import           Data.Word

import Debug.Trace

----------------------------------------------------------------------------------------------------

data Tree p n
  = Void
  | Leaf   { branchData :: n }
  | Branch { branchMap  :: M.Map p (Tree p n) }
  | LeafBranch 
    { branchData :: n
    , branchMap  :: M.Map p (Tree p n)
    }
  deriving (Show, Typeable)
instance (Eq p, Eq n) => Eq (Tree p n) where
  (==) a b = case (a, b) of
    (Void           , Void           ) -> True
    (Leaf       a   , Leaf       b   ) -> a == b
    (Branch     a   , Branch     b   ) -> a == b
    (LeafBranch a aa, LeafBranch b bb) -> a == b && aa == bb
    _                                  -> False
instance (Ord p, Ord n) => Ord (Tree p n) where
  compare a b = case (a, b) of
    (Void           , Void           ) -> EQ
    (Leaf       a   , Leaf       b   ) -> compare a  b
    (Branch       aa, Branch       bb) -> compare aa bb
    (LeafBranch a aa, LeafBranch b bb) -> case compare a b of
      EQ -> compare aa bb
      e  -> e
    (Void           , _              ) -> LT
    (_              , Void           ) -> GT
    (Leaf       _   , _              ) -> LT
    (_              , Leaf       _   ) -> GT
    (Branch       _ , _              ) -> LT
    (_              , Branch       _ ) -> GT
instance Ord p => Functor (Tree p) where
  fmap f tree = case tree of
    Void           -> Void
    Leaf       a   -> Leaf (f a)
    Branch       m -> Branch (fmap (fmap f) m)
    LeafBranch a m -> LeafBranch (f a) (fmap (fmap f) m)
instance (Ord p, Monoid n) => Monoid (Tree p n) where
  mempty  = Void
  mappend = unionWith mappend

-- | A combinator to modify the data in the 'Leaf' and 'LeafBranch' nodes of a tree when passed to
-- one of the functions below.
type ModLeaf     n = Maybe n -> Maybe n

-- | A combinator to modify the data in the 'Branch' and 'LeafBranch' nodes of a tree when passed to
-- one of the functions below.
type ModBranch p n = Maybe (M.Map p (Tree p n)) -> Maybe (M.Map p (Tree p n))

-- | If a 'Tree' is 'Void' or a contains a branch that is equivalent to 'Data.Map.empty',
-- 'Data.Maybe.Nothing' is returned.
notVoid :: Tree p n -> Maybe (Tree p n)
notVoid t = case t of
  Void                      -> Nothing
  Branch       b | M.null b -> Nothing
  LeafBranch a b | M.null b -> Just (Leaf a)
  _                         -> Just t

-- | If the given node is a 'Leaf' or 'LeafBranch', returns the Leaf portion of the node.
getLeaf :: Tree p n -> Maybe n
getLeaf t = case t of { Leaf n -> Just n; LeafBranch n _ -> Just n; _ -> Nothing }

-- | If the given node is a 'Branch' or 'LeafBranch', returns the branch portion of the node.
getBranch :: Tree p a -> Maybe (M.Map p (Tree p a))
getBranch t = case t of { Branch b -> Just b; LeafBranch _ b -> Just b; _ -> Nothing }

-- | Use a 'ModLeaf' function to insert, update, or remove 'Leaf' and 'LeafBranch' nodes.
alterLeaf :: ModLeaf n -> Tree p n -> Tree p n
alterLeaf alt t = maybe Void id $ case t of
  Void           -> alt Nothing         >>= \o -> Just (Leaf o)
  Leaf       o   -> alt (Just o)        >>= \o -> Just (Leaf o)
  Branch       b -> mplus (alt Nothing  >>= \o -> Just (LeafBranch o b)) (Just (Branch b))
  LeafBranch o b -> mplus (alt (Just o) >>= \o -> Just (LeafBranch o b)) (Just (Branch b))

alterBranch :: (Eq p, Ord p) => ModBranch p n -> Tree p n -> Tree p n
alterBranch alt t = maybe Void id $ case t of
  Void           -> alt Nothing         >>= \b -> Just (Branch b)
  Leaf       o   -> mplus (alt Nothing  >>= \b -> Just (LeafBranch o b)) (Just (Leaf o))
  Branch       b -> alt (Just b)        >>= \b -> Just (Branch b)
  LeafBranch o b -> mplus (alt (Just b) >>= \b -> Just (LeafBranch o b)) (Just (Leaf o))

----------------------------------------------------------------------------------------------------

data ZipTree p n = ZipTree{ focus :: Tree p n, history :: [(p, Tree p n)] }

newtype UpdateTreeT p n m a = UpdateTreeT{ getUpdateTreeStateT :: StateT (ZipTree p n) m a }
type UpdateTree p n a = UpdateTreeT p n Identity a

instance Monad m =>
  Monad (UpdateTreeT p n m) where
    return = UpdateTreeT . return
    (UpdateTreeT a) >>= b = UpdateTreeT (a >>= getUpdateTreeStateT . b)
instance Functor m =>
  Functor (UpdateTreeT p n m) where { fmap f (UpdateTreeT m) = UpdateTreeT (fmap f m); }
instance (Monad m, Functor m) =>
  Applicative (UpdateTreeT p n m) where { pure = return; (<*>) = ap; }
instance Monad m =>
  MonadState (ZipTree p n) (UpdateTreeT p n m) where { state = UpdateTreeT . state; }
instance MonadTrans (UpdateTreeT p n) where { lift m = UpdateTreeT (lift m); }

-- | Like 'Control.Monad.State.runState', evaluates an 'UpdateTree' monad transformer lifting the
-- 'Control.Monad.Identity.Identity' monad, removing the identity monad after evaluation to give you
-- a pure function.
runUpdateTree :: Ord p => UpdateTree p n a -> Tree p n -> (a, Tree p n)
runUpdateTree updfn = runIdentity . runUpdateTreeT updfn

-- | Like 'Control.Monad.State.execState', disgards the value returned to the 'UpdateTree' monad and
-- only returns the 'Tree'.
execUpdateTree :: Ord p => UpdateTree p n a -> Tree p n -> Tree p n
execUpdateTree updfn = snd . runUpdateTree updfn

-- | Update a 'Tree' using an 'UpdateTreeT' monad, much like how 'Control.Monad.State.runStateT'
-- works. Evaluates to a monadic computation of the lifted type @m@ that 'Control.Monad.return's a
-- pair containing the value last 'Control.Monad.return'ed to the lifted monad and the updated
-- 'Tree'.
runUpdateTreeT :: (Ord p, Functor m, Monad m) => UpdateTreeT p n m a -> Tree p n -> m (a, Tree p n)
runUpdateTreeT updfn tree = fmap (fmap focus) $
  runStateT (getUpdateTreeStateT (updfn >>= \a -> home >> return a)) (ZipTree{focus=tree, history=[]})

-- | Go to the node with the given path. If the path does not exist, it is created.
goto :: (Ord p, Monad m) => [p] -> UpdateTreeT p n m (Tree p n)
goto path = case path of
  []       -> gets focus
  (p:path) -> do
    st <- get
    let step tree = put $ st{focus=Void, history=(p, focus st):history st}
    case getBranch (focus st) >>= M.lookup p of
      Nothing   -> step Void
      Just tree -> step tree
    goto path

-- | Go up one level in the tree, storing the current sub-tree into the upper tree, unless the
-- current tree is 'Void', in which case it is deleted from the upper tree.
back :: (Ord p, Monad m) => UpdateTreeT p n m ()
back = modify $ \st -> case history st of
  []             -> st
  (p, tree):hist ->
    st{ history = hist
      , focus   = flip alterBranch tree $ \branch -> flip mplus (fmap (M.delete p) branch) $ do
          subTree <- notVoid (focus st)
          fmap (M.insert p subTree) (mplus branch (return mempty))
      }

-- | Returns 'Prelude.True' if we are at the top level of the tree.
atTop :: (Functor m, Monad m) => UpdateTreeT p n m Bool
atTop = fmap Prelude.null (gets history)

-- | Go back to the top level of the tree.
home :: (Ord p, Functor m, Monad m) => UpdateTreeT p n m ()
home = atTop >>= flip unless (back >> home)

-- | Return the current path.
getPath :: (Ord p, Functor m, Monad m) => UpdateTreeT p n m [p]
getPath = fmap (reverse . fmap fst) (gets history)

-- | Modify the tree node at the current 'focus'. After the update, if there is a leaf attached at
-- the focus, the value of the leaf is returned.
modifyNode :: (Ord p, Functor m, Monad m) => (Tree p n -> Tree p n) -> UpdateTreeT p n m (Maybe n)
modifyNode mod = modify (\st -> st{focus=mod(focus st)}) >> fmap getLeaf (gets focus)

-- | Modify the tree node using a 'ModBranch' function which allows you to alter the 'Data.Map.Map'
-- object containing the branches of the current node.
modifyBranch :: (Ord p, Functor m, Monad m) => ModBranch p n -> UpdateTreeT p n m ()
modifyBranch mod = modifyNode (alterBranch mod) >> return ()

-- | Modify the tree node using a 'ModLeaf' function which allows you to alter the 'Data.Map.Map'
-- object containing the current of the current node.
modifyLeaf :: (Ord p, Functor m, Monad m) => ModLeaf n -> UpdateTreeT p n m (Maybe n)
modifyLeaf mod = modifyNode (alterLeaf mod)

----------------------------------------------------------------------------------------------------
-- $MapLikeFunctions
-- In this section I have made my best effor to create API functions as similar as possible to that
-- of the "Data.Map" module.
----------------------------------------------------------------------------------------------------

alter :: Ord p => (Tree p a -> Tree p a) -> [p] -> Tree p a -> Tree p a
alter mod path = execUpdateTree (goto path >> modifyNode mod)
--alterNode alt px t = runIdentity $ alterNodeM (return . alt) px t

-- | Insert a 'Leaf' at a given address.
insert :: Ord p => [p] -> n -> Tree p n -> Tree p n
insert path n = execUpdateTree (goto path >> modifyLeaf (const (Just n)))
--insert px a = alter (const (Just a)) (flip mplus (Just Void)) px

-- | Update a 'Leaf' at a given address.
update :: Ord p => [p] -> ModLeaf a -> Tree p a -> Tree p a
update path mod = execUpdateTree (goto path >> modifyLeaf mod)
--update path mod = alter mod (flip mplus (Just Void)) path

-- | Delete a 'Leaf' or 'Branch' at a given address.
delete :: Ord p => [p] -> Tree p a -> Tree p a
delete path = execUpdateTree (goto path >> modifyLeaf (const Nothing))
--delete px = alter (const Nothing) id px

-- | Create a 'Tree' from a list of associationes, the 'Prelude.fst' element containing the branch,
-- the 'Prelude.snd' element containing the leaf value. This is the inverse operation of 'assocs'.
fromList :: Ord p => [([p], a)] -> Tree p a
fromList = foldl (\ tree (px, a) -> insert px a tree) Void

-- | Lookup a 'Tree' value (the whole node, not just the data stored in the node) at given address.
-- NOTE: this may not be what you want. If you want return the data that is stored in a 'Leaf' or
-- 'LeafBranch', use 'lookup', or just do @'lookup' atBranch inTree >>= 'getLeaf'@.
lookupNode :: Ord p => [p] -> Tree p a -> Maybe (Tree p a)
lookupNode px t = case px of
  []   -> Just t
  p:px -> case t of
    Branch       t -> next p t
    LeafBranch _ t -> next p t
    _              -> Nothing
    where { next p t = M.lookup p t >>= Dao.Tree.lookupNode px }

-- | This function analogous to the 'Data.Map.lookup' function, which returns a value stored in a
-- leaf, or nothing if there is no leaf at the given path.
lookup :: Ord p => [p] -> Tree p a -> Maybe a
lookup px t = lookupNode px t >>= getLeaf

-- | There are only two kinds values defined as a 'MergeType': 'union' and 'intersection.
type MergeType p a
  = (Tree p a -> Tree p a -> Tree p a)
  -> M.Map p (Tree p a)
  -> M.Map p (Tree p a)
  -> M.Map p (Tree p a)

-- | Merge two trees together.
mergeWithKey :: Ord p
  => ([p] -> Maybe a -> Maybe b -> Maybe c)
  -> (Tree p a -> Tree p c)
  -> (Tree p b -> Tree p c)
  -> Tree p a -> Tree p b -> Tree p c
mergeWithKey overlap leftOnly rightOnly left right = loop [] left right where
  -- loop :: Ord p => [p] -> Tree p a -> Tree p b -> Tree p c
  loop px left right = case left of
    Void           -> case right of
      Void           -> Void
      Leaf       y   -> rightOnly (Leaf       y  )
      Branch       b -> rightOnly (Branch       b)
      LeafBranch y b -> rightOnly (LeafBranch y b)
    Leaf       x   -> case right of
      Void           -> leftOnly  (Leaf       x  )
      Leaf       y   -> maybe Void id (fmap Leaf (overlap px (Just x) (Just y)))
      Branch       b -> leafbranch (Just x) Nothing  M.empty b
      LeafBranch y b -> leafbranch (Just x) (Just y) M.empty b
    Branch       a -> case right of
      Void           -> leftOnly  (Branch       a)
      Leaf       y   -> leafbranch Nothing (Just y) a M.empty
      Branch       b -> leafbranch Nothing Nothing  a b      
      LeafBranch y b -> leafbranch Nothing (Just y) a b      
    LeafBranch x a -> case right of
      Void           -> leftOnly  (LeafBranch x a)
      Leaf       y   -> leafbranch (Just x) (Just y) a M.empty
      Branch       b -> leafbranch (Just x) Nothing  a b      
      LeafBranch y b -> leafbranch (Just x) (Just y) a b      
    where
      -- leafbranch :: Ord p => M.Map p (Tree p a) -> M.Map p (Tree p b) -> Maybe a -> Maybe b -> Tree p c
      leafbranch x y left right = 
        let c = M.mergeWithKey both (bias leftOnly) (bias rightOnly) left right -- :: M.Map p (Tree p c)
        in  case overlap px x y of
              Nothing -> notEmpty Branch c
              Just  z -> notEmpty (LeafBranch z) c
      -- notEmpty :: Ord p => (M.Map p (Tree p a) -> Tree p a) -> M.Map p (Tree p a) -> Tree p a
      notEmpty cons c = if M.null c then Void else cons c
      -- both :: Ord p => p -> Tree p a -> Tree p b -> Maybe (Tree p c)
      both p left right = notVoid (loop (px++[p]) left right)
      -- bias :: Ord p => (Tree p a -> Tree p b) -> M.Map p (Tree p a) -> M.Map p (Tree p b)
      bias fn = M.mapMaybe (notVoid . fn)

mergeWith :: Ord p => (Maybe a -> Maybe b -> Maybe c) -> (Tree p a -> Tree p c) -> (Tree p b -> Tree p c) -> Tree p a -> Tree p b -> Tree p c
mergeWith overlap = mergeWithKey (\ _ -> overlap)

unionWithKey :: Ord p => ([p] -> a -> a -> a) -> Tree p a -> Tree p a -> Tree p a
unionWithKey overlap = mergeWithKey (\k a b -> msum [liftM2 (overlap k) a b, a, b]) id id

unionWith :: Ord p => (a -> a -> a) -> Tree p a -> Tree p a -> Tree p a
unionWith overlap = unionWithKey (\ _ -> overlap)

union :: Ord p => Tree p a -> Tree p a -> Tree p a
union = unionWith const

unionsWith :: Ord p => (a -> a -> a) -> [Tree p a] -> Tree p a
unionsWith overlap = foldl (unionWith overlap) Void

unions :: Ord p => (a -> a -> a) -> [Tree p a] -> Tree p a
unions overlap = unionsWith (flip const)

intersectionWithKey :: Ord p => ([p] -> a -> a -> a) -> Tree p a -> Tree p a -> Tree p a
intersectionWithKey overlap = mergeWithKey (\k -> liftM2 (overlap k)) (const Void) (const Void)

intersectionWith :: Ord p => (a -> a -> a) -> Tree p a -> Tree p a -> Tree p a
intersectionWith overlap = intersectionWithKey (\ _ -> overlap)

intersection :: Ord p => Tree p a -> Tree p a -> Tree p a
intersection = intersectionWith const

intersectionsWith :: Ord p => (a -> a -> a) -> [Tree p a] -> Tree p a
intersectionsWith overlap = foldl (intersectionWith overlap) Void

intersections :: Ord p => [Tree p a] -> Tree p a
intersections = intersectionsWith (flip const)

differenceWithKey :: Ord p => ([p] -> a -> b -> Maybe a) -> Tree p a -> Tree p b -> Tree p a
differenceWithKey overlap = mergeWithKey (\k a b -> mplus (b >>= \b -> a >>= \a -> overlap k a b) a) id (const Void)

differenceWith :: Ord p => (a -> b -> Maybe a) -> Tree p a -> Tree p b -> Tree p a
differenceWith overlap = differenceWithKey (\ _ -> overlap)

difference :: Ord p => Tree p a -> Tree p b -> Tree p a
difference = differenceWith (\ _ _ -> Nothing)

-- | Get all items and their associated path.
assocs :: Tree p a -> [([p], a)]
assocs t = loop [] t where
  recurs px b = M.assocs b >>= \ (p, t) -> loop (px++[p]) t
  loop px t = case t of
    Void           -> []
    Leaf       a   -> [(px, a)]
    Branch       b -> recurs px b
    LeafBranch a b -> (px, a) : recurs px b

-- | Apply @'Prelude.map' 'Prelude.snd'@ to the result of 'assocs', behaves just like how
-- 'Data.Map.elems' or 'Data.Array.IArray.elems' works.
elems :: Tree p a -> [a]
elems t = fmap snd (assocs t)

-- | Counts the number of *nodes*, which includes the number of 'Branch'es and 'Leaf's.
size :: Tree p a -> Word64
size t = case t of
  Void           -> 0
  Leaf       _   -> 1
  Branch       m -> 0 + f m
  LeafBranch _ m -> 1 + f m
  where { f m = foldl (\sz tre -> sz + size tre) (fromIntegral (M.size m)) (M.elems m) }

null :: Tree p a -> Bool
null Void = True
null _    = False

----------------------------------------------------------------------------------------------------

data TreeDiff a b
  = LeftOnly  a -- something exists in the "left" branch but not in the "right" branch.
  | RightOnly b -- something exists in the "right" branch but not in the "left" branch.
  | TreeDiff  a b -- something exists in the "left" and "right" branches but they are not equal
  deriving (Eq, Typeable)

-- | Produce a difference report of two trees with the given comparison predicate. If the predicate
-- returns 'Prelude.True', the node is ignored, otherwise the differences is reported.
treeDiffWith :: Ord p => (a -> b -> Bool) -> Tree p a -> Tree p b -> Tree p (TreeDiff a b)
treeDiffWith compare = mergeWithKey leaf (fmap LeftOnly) (fmap RightOnly) where
  leaf _ a b = msum $
    [ a >>= \a -> b >>= \b -> if compare a b then Nothing else Just (TreeDiff a b)
    , fmap LeftOnly a, fmap RightOnly b
    ]

-- | Call 'treeDiffWith' using 'Prelude.(==)' as the comparison predicate.
treeDiff :: (Eq a, Ord p) => Tree p a -> Tree p a -> Tree p (TreeDiff a a)
treeDiff = treeDiffWith (==)

