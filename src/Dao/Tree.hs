-- "src/Dao/Tree.hs"  provides a fundamental data type used in the Dao
-- System, the "Tree", which is similar to the "Data.Map" data type.
-- 
-- Copyright (C) 2008-2012  Ramin Honary.
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

module Dao.Tree where

import           Control.Monad
import           Control.Monad.Identity

import           Data.Typeable
import           Data.Maybe (fromMaybe)
import           Data.List (intercalate)
import           Data.Binary
import qualified Data.Map as M
import           Data.Word

----------------------------------------------------------------------------------------------------

data Tree p a
  = Void
  | Leaf   { branchData :: a }
  | Branch { branchMap  :: M.Map p (Tree p a) }
  | LeafBranch 
    { branchData :: a
    , branchMap  :: M.Map p (Tree p a)
    }
  deriving (Show, Typeable)

instance (Eq p, Eq a) => Eq (Tree p a) where
  (==) a b =
    case (a, b) of
      (Void           , Void           ) -> True
      (Leaf       a   , Leaf       b   ) -> a == b
      (Branch     a   , Branch     b   ) -> a == b
      (LeafBranch a aa, LeafBranch b bb) -> a == b && aa == bb
      _                                  -> False

instance (Ord p, Ord a) => Ord (Tree p a) where
  compare a b =
    case (a, b) of
      (Void           , Void           ) -> EQ
      (Leaf       a   , Leaf       b   ) -> compare a b
      (Branch     a   , Branch     b   ) -> compare a b
      (LeafBranch a aa, LeafBranch b bb) ->
        case compare a b of
          EQ -> compare aa bb
          e  -> e
      (Void           , _              ) -> LT
      (Leaf       _   , _              ) -> LT
      (Branch     _   , _              ) -> LT
      (LeafBranch _ _ , _              ) -> LT

-- | If the given node is a 'Leaf' or 'LeafBranch', returns the Leaf portion of the node.
getLeaf :: Tree p a -> Maybe a
getLeaf t = case t of { Leaf a -> Just a; LeafBranch a _ -> Just a; _ -> Nothing }

-- | If the given node is a 'Branch' or 'LeafBranch', returns the branch portion of the node.
getBranch :: Tree p a -> Maybe (M.Map p (Tree p a))
getBranch t = case t of { Branch b -> Just b; LeafBranch _ b -> Just b; _ -> Nothing }

-- | A combinator to modify the data in the 'Leaf' and 'LeafBranch' nodes of a tree when passed to
-- one of the functions below..
type ModLeaf     a = Maybe a -> Maybe a

-- | A combinator to modify the data in the 'Branch' and 'LeafBranch' nodes of a tree when passed to
-- one of the functions below..
type ModBranch p a = Maybe (M.Map p (Tree p a)) -> Maybe (M.Map p (Tree p a))

-- | A combinator to modify whole nodes, be they 'Leaf's or 'Branch'es, when passed to one of the
-- functions below..
type ModTree   p a = Maybe (Tree p a) -> Maybe (Tree p a)

-- | Use a 'ModLeaf' function to insert, update, or remove 'Leaf' and 'LeafBranch' nodes.
alterData :: ModLeaf a -> Tree p a -> Tree p a
alterData alt t = fromMaybe Void $
  case t of
    Void           -> alt Nothing >>= \o -> Just (Leaf o)
    Leaf       o   -> alt (Just o) >>= \o -> Just (Leaf o)
    Branch       b -> msum [alt Nothing >>= \o -> Just (LeafBranch o b), Just (Branch b)]
    LeafBranch o b -> msum [alt (Just o) >>= \o -> Just (LeafBranch o b), Just (Branch b)]

alterBranch :: (Eq p, Ord p) => ModBranch p a -> Tree p a -> Tree p a
alterBranch alt t = fromMaybe Void $
  case t of
    Void           -> alt Nothing >>= \b -> Just (Branch b)
    Leaf       o   -> msum [alt Nothing >>= \b -> Just (LeafBranch o b), Just (Leaf o)]
    Branch       b -> alt (Just b) >>= \b -> Just (Branch b)
    LeafBranch o b -> msum [alt (Just b) >>= \b -> Just (LeafBranch o b), Just (Leaf o)]

-- | If a 'Tree' is 'Void' or a contains a branch that is equivalent to 'Data.Map.empty',
-- 'Data.Maybe.Nothing' is returned.
notVoid :: Tree p a -> Maybe (Tree p a)
notVoid t =
  case t of
    Void                      -> Nothing
    Branch       b | M.null b -> Nothing
    LeafBranch a b | M.null b -> Just (Leaf a)
    _                         -> Just t

-- | Alter a 'Tree', like 'Data.Map.alter', but you must provide functions for two situations: the
-- first is the situation where you have reached the end of the given path and you need to choose
-- whther or not to insert or delete a 'Leaf'. the second is the situation where you have selected
-- the next node in the path, and you need to choose how this node will be effected.
alter :: Ord p => ModLeaf a -> ModTree p a -> [p] -> Tree p a -> Tree p a
alter leaf tree px t = fromMaybe Void (loop px (Just t)) where
  loop px t =
    case px of
      []   -> tree t >>= Just . alterData leaf >>= notVoid
      p:px -> tree t >>= notVoid . alterBranch (subAlt p px)
  subAlt p px m = mplus m (Just M.empty) >>= Just . M.alter (loop px) p

-- | Insert a 'Leaf' at a given address.
insert :: Ord p => [p] -> a -> Tree p a -> Tree p a
insert px a = alter (const (Just a)) (flip mplus (Just Void)) px

-- | Update a 'Leaf' at a given address.
update :: Ord p => [p] -> ModLeaf a -> Tree p a -> Tree p a
update path updfn = alter updfn (flip mplus (Just Void)) path

-- | Delete a 'Leaf' or 'Branch' at a given address.
delete :: Ord p => [p] -> Tree p a -> Tree p a
delete px = alter (const Nothing) id px

-- | Lookup a 'Tree' value (the whole node, not just the data stored in the node) at given address.
-- NOTE: this may not be what you want. If you want return the data that is stored in a 'Leaf' or
-- 'LeafBranch', use 'lookup', or just do @'lookup' atBranch inTree >>= 'getLeaf'@.
lookupNode :: Ord p => [p] -> Tree p a -> Maybe (Tree p a)
lookupNode px t =
  case px of
    []     -> Just t
    p:px ->
      case t of
        Branch       t -> next p t
        LeafBranch _ t -> next p t
        _              -> Nothing
      where { next p t = M.lookup p t >>= Dao.Tree.lookupNode px }

-- | This function analogous to the 'Data.Map.lookup' function, which returns a value stored in a
-- leaf, or nothing if there is no leaf at the given path.
lookup :: Ord p => [p] -> Tree p a -> Maybe a
lookup px t = lookupNode px t >>= getLeaf

-- | Pretty-Print a tree using the supplied string-conversion function.
ppTree :: Ord p => (p -> String) -> (a -> String) -> Tree p a -> String
ppTree showKey showVal t = dropWhile (\c->c=='\n'||c=='\r') (loop 0 0 t) where
  pre d  = concat (take d (repeat "| "))
  pp f d a =
    let x = pre d
    in  intercalate ("\n"++x++drop 3 (take f (repeat ' '))++"... ") (lines (showVal a)) ++ "\n"
  loop f d t =
    case t of
      Void           -> ""
      Leaf       a   -> pp f d a
      Branch       b -> "\n" ++ br d b
      LeafBranch a b -> pp f d a ++ br d b
  br d b =
    let keys  = M.keys b
        kp    = 1 + maximum (map (length . showKey) keys)
        bar   = take kp (repeat '-')
        w s b = if null s then b else head s : w (tail s) (tail b)
        sh (k, t) = pre d++"+ "++w (showKey k++" ") bar++" "++loop kp (d+1) t
    in  concatMap sh (M.assocs b)

----------------------------------------------------------------------------------------------------

-- | There are only two kinds values defined as a 'MergeType': 'union' and 'intersection.
type MergeType p a
  = (Tree p a -> Tree p a -> Tree p a)
  -> M.Map p (Tree p a)
  -> M.Map p (Tree p a)
  -> M.Map p (Tree p a)

-- | Used by 'merge' and 'graft' to determine how to merge branches of trees together.
union :: Ord p => MergeType p a
union = M.unionWith

-- | Used by 'merge' and 'graft' to determine how to merge branches of trees together.
intersection :: Ord p => MergeType p a
intersection = M.intersectionWith

-- | Merge two trees together.
merge
  :: Ord p
  => MergeType p a -- ^ is either 'union', 'intersection', or 'difference'.
  -> (a -> a -> a) -- ^ the function to use when combining 'Leaf' or 'LeafBranch' nodes.
  -> Tree p a -> Tree p a -> Tree p a
merge joinWith combine t u =
  case (t, u) of
    (Void          , u             ) -> u
    (t             , Void          ) -> t
    (Leaf       x  , Leaf       y  ) -> Leaf (combine x y)
    (Branch       a, Branch       b) -> Branch (joinWith op a b)
    (Branch       b, Leaf       y  ) -> LeafBranch y b
    (Leaf       x  , Branch       b) -> LeafBranch x b
    (LeafBranch x b, Leaf       y  ) -> LeafBranch (combine x y) b
    (LeafBranch x a, Branch       b) -> LeafBranch x (joinWith op a b)
    (Leaf       x  , LeafBranch y b) -> LeafBranch (combine x y) b
    (Branch       a, LeafBranch y b) -> LeafBranch y (joinWith op a b)
    (LeafBranch x a, LeafBranch y b) -> LeafBranch (combine x y) (joinWith op a b)
  where { op = merge joinWith combine }

-- | Like 'merge', but performs the merge on the branch at the address of one tree.
graft
  :: Ord p
  => MergeType p a -- ^ is either 'union', 'intersection', or 'difference'.
  -> (a -> a -> a) -- ^ the function to use to combine 'Leaf' and 'LeafBranch' nodes.
  -> [p]           -- ^ the path of the branch to which to apply the 'merge'.
  -> Tree p a      -- ^ the tree which that above path will seek through.
  -> Tree p a      -- ^ the tree to apply to the branch of the above tree.
  -> Tree p a
graft joinWith combine px t u =
  alter id (\t -> notVoid (merge joinWith combine (fromMaybe Void t) u)) px t

-- | Remove a branch from the tree.
prune :: Ord p => [p] -> Tree p a -> Tree p a
prune px = alter (const Nothing) (const Nothing) px

-- | Get all items and their associated path.
assocs :: Tree p a -> [([p], a)]
assocs t = loop [] t where
  recurs px b = concatMap (\ (p, t) -> loop (px++[p]) t) (M.assocs b)
  loop px t =
    case t of
      Void           -> []
      Leaf       a   -> [(px, a)]
      Branch       b -> recurs px b
      LeafBranch a b -> (px, a) : recurs px b

-- | Apply @'Prelude.map' 'Prelude.snd'@ to the result of 'assocs', behaves just like how
-- 'Data.Map.elems' or 'Data.Array.IArray.elems' works.
elems :: Tree p a -> [a]
elems t =  map snd (assocs t)

-- | Counts the number of *nodes*, which includes the number of 'Branch'es and 'Leaf's.
size :: Tree p a -> Word64
size t =
  case t of
    Void           -> 0
    Leaf       _   -> 1
    Branch       m -> 0 + f m
    LeafBranch _ m -> 1 + f m
  where { f m = foldl (\sz tre -> sz + size tre) (fromIntegral (M.size m)) (M.elems m) }

-- Transform every node, leaves and branches alike, using monadic functions.
mapNodesM
  :: (Functor m, Monad m, Eq p, Ord p, Eq q, Ord q)
  => (p -> m q) -- ^ modify each branch label with this function
  -> (a -> m b) -- ^ modify each leaf with this function
  -> Tree p a   -- ^ using the above two functions, modify everything in this 'Tree'.
  -> m (Tree q b)
mapNodesM bnf lef t =
  case t of
    Void           -> return Void
    Leaf       a   -> fmap Leaf   (lef a)
    Branch       m -> fmap Branch (branch m)
    LeafBranch a m -> liftM2 LeafBranch (lef a) (branch m)
  where
    branch m = fmap M.fromList $ forM (M.assocs m) $ \ (p, a) ->
      liftM2 (,) (bnf p) (mapNodesM bnf lef a)

-- | Transform every node, leaves and branches alike. This is the function that instantiates
-- 'Data.Functor.fmap'.
mapNodes :: (Eq p, Ord p, Eq q, Ord q) => (p -> q) -> (a -> b) -> Tree p a -> Tree q b
mapNodes bnf lef t = runIdentity (mapNodesM (Identity . bnf) (Identity . lef) t)

instance (Eq p, Ord p) => Functor (Tree p) where { fmap fn t = mapNodes id fn t }

-- | Transform every branch, ignoring leaves.
mapBranches :: (Eq p, Ord p, Eq q, Ord q) => (p -> q) -> Tree p a -> Tree q a
mapBranches bnf t = runIdentity (mapNodesM (Identity . bnf) (Identity . id) t)

-- | Transform every branch using a monadic function, ignoring leaves.
mapBranchesM
  :: (Functor m, Monad m, Eq p, Ord p, Eq q, Ord q) => (p -> m q) -> Tree p a -> m (Tree q a)
mapBranchesM bnf t = mapNodesM bnf return t

