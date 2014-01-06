-- "src/Dao/Struct.hs"  provides a class that can convert between
-- Hasekll "data" types and 'Dao.Tree.Tree's.
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
-- along atAddress this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A module with utility functions to help define instanes to the 'Dao.Object.Structured' class.
-- A stateful monadic interface built on zippers is provided to allow leaves and branches in
-- 'Dao.Tree.Tree's to be read and written using a syntax similar to that of a procedural
-- programming language, which is more intuitive for the Dao programming language which is
-- procedural. Please also see the 'Dao.Object.Structured' class and the 'Dao.Object.GenUpdateErr' data
-- type.
module Dao.Struct where

import           Prelude hiding (lookup)

import           Dao.String
import           Dao.Tree
import           Dao.Predicate

import           Data.Monoid
import           Data.List (intercalate)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Error

----------------------------------------------------------------------------------------------------

-- | This class is used to define methods of converting arbitrary types to 'Dao.Tree.Tree's, where
-- the leaves in the 'Dao.Tree.Tree' are Dao 'Object's. The branches of the 'Dao.Tree.Tree's are all
-- labeled with 'Dao.String.Name's. It is an important interface for being able to maniuplate
-- objects within a script written in the Dao language. 
class Structured typ obj where
  dataToStruct :: typ -> Tree Name obj
  structToData :: Tree Name obj -> Predicate (GenUpdateErr obj) typ

-- | This is the error type used to report errors that might occur while updating a 'Dao.Tree.Tree'
-- with the 'structToData' function. If an error occurs while stepping through the branches of a
-- tree, you can throw an error with this information using 'Control.Monad.Error.throwError'.
data GenUpdateErr obj
  = UpdateErr
    { updateErrMsg  :: Maybe UStr -- ^ the message explaining why the error ocurred.
    , updateErrAddr :: [Name]     -- ^ the address at which the error was thrown.
    , updateErrTree :: Tree Name obj     -- ^ the sub-tree at the address at which the error was thrown.
    }
instance Show obj => Show (GenUpdateErr obj) where
  show err = concat $
    [ "constructor failed" ++ maybe " " ((++"\n") . (": "++) . uchars) (updateErrMsg err)
    , "at index: ", intercalate "." (fmap uchars (updateErrAddr err))
    ]

-- | This is the fundamental data type for Dao's foreign interface. This monad is used to convert
-- data types in your Haskell program into a form that can be manipulated by Dao scripts.
--
-- Essentially, this is a simple zipper monad for traversing 'Dao.Object.T_tree' objects, which are 'Dao.Tree.Tree's with
-- 'Dao.String.Name's on the branches and 'Dao.Object.Object's on the leaves (nodes).
newtype GenUpdate obj a = Update { updateToPredicateT :: PredicateT (GenUpdateErr obj) (UpdateTreeT Name obj Identity) a }
instance Monad (GenUpdate obj) where
  (Update fn) >>= mfn = Update (fn >>= updateToPredicateT . mfn)
  return a = Update (return a)
  fail msg = newUpdateErr (Just msg) >>= throwError
instance Functor (GenUpdate obj) where
  fmap fn (Update a) = Update (fmap fn a)
instance MonadPlus (GenUpdate obj) where
  mzero = Update mzero
  mplus (Update a) (Update b) = Update (mplus a b)
instance MonadState (Tree Name obj) (GenUpdate obj) where
  get = Update $ PredicateT $ gets focus >>= return . OK
  put tree = Update $ PredicateT $ modify (\st -> st{focus=tree}) >> return (OK ())
instance MonadError (GenUpdateErr obj) (GenUpdate obj) where
  throwError = Update . throwError
  catchError (Update func) catcher = Update (catchError func (updateToPredicateT . catcher))
instance MonadPlusError (GenUpdateErr obj) (GenUpdate obj) where
  catchPredicate (Update func) = Update (catchPredicate func)
  predicate = Update . predicate
instance Applicative (GenUpdate obj) where { pure = return; (<*>) = ap; }
instance Alternative (GenUpdate obj) where { empty = mzero; (<|>) = mplus; }
instance Monoid a => Monoid (GenUpdate obj a) where
  mempty      = return mempty
  mappend a b = liftM2 mappend a b

----------------------------------------------------------------------------------------------------

-- | If you would like to create a detailed error message, create an error using the current
-- information, then throw it using 'Control.Monad.Error.throwError'.
newUpdateErr :: Maybe String -> GenUpdate obj (GenUpdateErr obj)
newUpdateErr msg = Update (lift Control.Monad.State.get) >>= \st -> return (err st) where
  err st =
    UpdateErr
    { updateErrMsg  = fmap ustr msg
    , updateErrAddr = reverse $ fmap fst $ history st
    , updateErrTree = focus st
    }

updateError :: GenUpdate obj ig
updateError = newUpdateErr Nothing >>= throwError

-- | It is often a good idea to backtrack unless a precise number of branches exist on the current
-- node, this prevents extraneous, unexpected, possibly malicious data from being hidden among the
-- branches of the tree. Pass an predicate checking an 'Prelude.Int' number of branches of the
-- current node, bracktrack if the predicate evaluates to 'Prelude.False'.
guardBranchCount :: (Int -> Bool) -> GenUpdate obj ()
guardBranchCount p = get >>= guard . p . branchCount

-- | Goto an address. This 'GenUpdate' never fails, even if the address does not exist. Immediately
-- returns the sub-tree to which we traversed.
goto :: [Name] -> GenUpdate obj (Tree Name obj)
goto path = Update (lift (Dao.Tree.goto path))

-- | Go up one node level.
back :: GenUpdate obj ()
back = Update (lift Dao.Tree.back)

-- | Go to the root node.
home :: GenUpdate obj ()
home = Update (lift Dao.Tree.home)

-- | Like 'Control.Monad.StateT.runStateT', evaluates the 'GenUpdate' monad as a pure function,
-- returning a pair containing first the 'Dao.Predicate.Predicate' of that was returned and second the
-- updated 'Dao.Tree.Tree'.
runUpdate :: GenUpdate obj a -> Tree Name obj -> (Predicate (GenUpdateErr obj) a, Tree Name obj)
runUpdate upd = runUpdateTree (runPredicateT (updateToPredicateT upd))

-- | GenUpdate a data type in the 'Structured' class using an 'GenUpdate' monadic function.
onStruct :: Structured a obj => GenUpdate obj ig -> a -> Predicate (GenUpdateErr obj) a
onStruct ufn a = (fst . runUpdate (ufn>>get)) (dataToStruct a) >>= structToData

-- | Useful for instantiating the 'dataToStruct' function of the 'Structured' class, this is
-- essentially the same function as 'Control.Monad.State.execState'.
deconstruct :: GenUpdate obj a -> Tree Name obj
deconstruct fn = snd (runUpdate fn Void)

-- | Useful for instantiating 'structToData', for example:
-- @'structToData' = 'reconstruct' $ do { ... }@
-- Then write everything in the @do@ statement that reconstructs your Haskell data type from a Dao
-- structure. This is essentially the same function as 'Control.Monad.State.evalState'.
reconstruct :: GenUpdate obj a -> Tree Name obj -> Predicate (GenUpdateErr obj) a
reconstruct fn tree = fst (runUpdate fn tree)

----------------------------------------------------------------------------------------------------
-- $Fundamentals
-- These are the most important functions for building instances of 'Structured'.

-- | Return the value stored in the current node. Evaluates to 'Control.Monad.mzero' if the current
-- node is empty, so it can be used to check if an item exists at the current node as well. This
-- function is the counter operation of 'place'.
this :: GenUpdate obj obj
this = get >>= maybe mzero return . getLeaf

-- | This is a 'Control.Monad.guard'-like function. Because 'tryWith' actually steps onto the
-- branch, and there maye be situations where you want to check if a branch exists without actually
-- stepping on to that branch, use this function to check if a branch exists and backtrack (evaluate
-- to 'Control.Monad.mzero') if it does not exist.
guardBranch :: String -> GenUpdate obj ()
guardBranch addr = tryWith addr (return ())

-- | Same as 'atAddress' but but more convenient as it takes just one string and passes it to
-- 'atAddress' as @['Dao.String.ustr' address]@. This function is the counter operation of itself.
-- In other words, @'with' addr putFunc@ is the counter operation of @'with' addr getFunc@ where
-- @getFunc@ and @putFunc@ are any function which are counter operations of each other.
tryWith :: String -> GenUpdate obj a -> GenUpdate obj a
tryWith = atAddress . (:[]) . ustr

-- | Like 'tryWith' but throws fails if nothing exists at the given address.
with :: String -> GenUpdate obj a -> GenUpdate obj a
with addr upd = mplus (tryWith addr upd) updateError

-- | Use 'structToData' to construct data from the current node. This function is the counter
-- operation of 'putData'. 'Dao.Predicate.Backtrack's if the current node is 'Dao.Tree.Void'.
tryGetData :: Structured a obj => GenUpdate obj a
tryGetData = get >>= predicate . structToData

-- | Use 'structToData' to construct data from the current node. This function is the counter
-- operation of 'putData'. 'Dao.Predicate.Backtrack's if the current node is 'Dao.Tree.Void'.
getData :: Structured a obj => GenUpdate obj a
getData = mplus tryGetData updateError

-- | Like 'getData' but takes a default value as a parameter, and if the current 'Dao.Tree.Tree'
-- node returned by 'Control.Monad.State.get' is 'Dao.Tree.Void', the default parameter is returned.
getOptional :: Structured a obj => a -> GenUpdate obj a
getOptional opt = mplus getData (return opt)

-- | Shortcut for @'with' addr 'getData'@. This function is the counter operation of 'putDataAt'.
getDataAt :: Structured a obj => String -> GenUpdate obj a
getDataAt addr = with addr getData

tryGetDataAt :: Structured a obj => String -> GenUpdate obj a
tryGetDataAt addr = tryWith addr tryGetData

getMaybe :: Structured a obj => GenUpdate obj (Maybe a)
getMaybe = mplus (fmap Just getData) (return Nothing)

getMaybeAt :: Structured a obj => String -> GenUpdate obj (Maybe a)
getMaybeAt addr = with addr $ getMaybe

-- | Place an object at in current node. This function is the counter opreation of 'this'.
place :: obj -> GenUpdate obj ()
place obj = placeWith (const (Just obj))

-- | Use 'dataToStruct' to convert a data type to a 'Structured' 'Dao.Tree.Tree' node, then union
-- it with the current node. If the current node is a 'Dao.Tree.Leaf', the leaf might be
-- overwritten if you write a new 'Dao.Tree.Leaf'. This function is the couner operation of
-- 'getData'.
putData :: Structured a obj => a -> GenUpdate obj ()
putData = putTree . dataToStruct

-- | Shortcut for @'with' addr ('putData' a)@. This function is the counter opreation of
-- 'getDataAt'.
putDataAt :: Structured a obj => String -> a -> GenUpdate obj ()
putDataAt addr obj = with addr (putData obj)

putMaybe :: Structured a obj => Maybe a -> GenUpdate obj ()
putMaybe = maybe (return ()) putData

putMaybeAt :: Structured a obj => String -> Maybe a -> GenUpdate obj ()
putMaybeAt addr = with addr . putMaybe

-- | GenUpdate an object at the current node.
placeWith :: ModLeaf obj -> GenUpdate obj ()
placeWith fn = modify (alterLeaf fn)

-- | Applies a function only if there is an item at the current node.
mapThis :: (obj -> obj) -> GenUpdate obj ()
mapThis fn = placeWith (\item -> fmap fn item)

-- | Put an 'Dao.Object.Object' in the current ('this') location, overwriting what is already here.
putObjAt :: obj -> GenUpdate obj ()
putObjAt = modify . alterLeaf . const . Just

-- | Union a tree node with the current node. If the current node is a 'Dao.Tree.Leaf', the leaf
-- might be overwritten if you write a new 'Dao.Tree.Leaf'. *IMPORTANT:* Use this instead of
-- 'Control.Monad.State.put'.
putTree :: Tree Name obj -> GenUpdate obj ()
putTree = modify . union

-- | Modify or write a new a data structure at a given address using the given 'GenUpdate' function.
atAddress :: [Name] -> GenUpdate obj a -> GenUpdate obj a
atAddress path doUpdate = do
  Dao.Struct.goto path
  a <- doUpdate
  sequence_ (fmap (const Dao.Struct.back) path)
  return a

-- | Goes to a given address and tries to return the value stored at that node,
-- 'Dao.Predicate.Backtrack's if nothing is there.
peekAddress :: [Name] -> GenUpdate obj obj
peekAddress addr = atAddress addr this

