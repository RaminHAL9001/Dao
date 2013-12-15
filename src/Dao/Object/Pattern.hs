-- "src/Dao/Object/Pattern.hs" a data type that can match
-- 'Dao.Object.Object's as a predicate.
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
{-# LANGUAGE MultiParamTypeClasses #-}

module Dao.Object.Pattern
  ( MatchValue, Matcher, evalMatcher, MatcherState, initMatcherState, matcherTree, getCurrentRef
  , matchObject, matchObjectList, matchWholeObjectList, matchObjectSet, matchObjectElemSet
  , matchObjectChoice
  )
  where

-- | One of the most important functions of the Dao system is to be able to efficiently analyze data
-- structures. The "Dao.Object" module provides a rich set of fundamental data types from which more
-- complicated data structures can be constructed. However, there needs to be a way to query
-- portions of these complicated structures without resorting to dereferencing and if-else
-- statements everywhere.
--
-- The solution is to provide a predicate data-type, the "object pattern" or 'Dao.Object.Pattern',
-- which can match objects and construct structures containing portions of data structures that
-- match patterns associated with labels for those patterns.
-- 
-- The 'Dao.Object.Pattern' type is used throughout the Dao system, so it is defined in the
-- "Dao.Object" module. This module defines the algorithm for matching 'Dao.Object.Object's to
-- 'Dao.Object.Pattern' patterns.

import           Dao.String
import           Dao.Object
import           Dao.Object.Math
import qualified Dao.Tree as T
import           Dao.Predicate
import qualified Dao.EnumSet as Es

import           Data.Maybe
import           Data.List
import           Data.Array.IArray
import qualified Data.Map    as M
import qualified Data.IntMap as IM
import qualified Data.Set    as S
import qualified Data.IntSet as IS

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error

----------------------------------------------------------------------------------------------------

-- | When matching an object to a predicate, this value is used to control matching. If the match
-- fails (rather than backtracks), the location of the failure is recorded as a
-- 'Dao.Object.Reference' that can be used to select the value from the object. This is typically a
-- 'Dao.Object.Subscript' value.
type MatchValue a = Predicate Reference a

data MatcherState
  = MatcherState
    { matcherRef  :: [Name]
      -- ^ contains labels taken from 'Dao.Object.ObjLabel' used to construct 'matcherTree' entries
      -- when refering to labeled matched objects.
    , matcherIdx  :: [Object]
      -- ^ when delving into container objects like 'Dao.Object.OList's and 'Dao.Object.ODict's, we
      -- can construct a lits of indicies to describe the location of the object we are currently
      -- looking at. This list contains that index value.
    , matcherTree :: T.Tree Name Object
    }

initMatcherState :: MatcherState
initMatcherState = MatcherState{ matcherRef = [], matcherIdx = [], matcherTree = T.Void }

withRef :: Name -> Matcher a -> Matcher a
withRef n fn = modify (\st -> st{matcherRef = n : matcherRef st}) >>
  mplusFinal fn (modify (\st -> st{matcherRef = tail (matcherRef st)}))

withIdx :: Object -> Matcher a -> Matcher a
withIdx i fn = modify (\st -> st{matcherIdx = i : matcherIdx st}) >>
  mplusFinal fn (modify (\st -> st{matcherIdx = tail (matcherIdx st)}))

getCurrentRef :: Matcher Reference
getCurrentRef = do
  idx <- flip fmap (gets matcherIdx) $
    foldl (\fn i -> fn . flip Subscript [i]) id . reverse -- <-- reverse this?
  fmap (idx . GlobalRef . reverse) (gets matcherRef)

-- | A 'Control.Monad.State.State' monad used to execute matches
newtype Matcher a = Matcher { matcherPTransState :: PredicateT Reference (State MatcherState) a }

-- | Like 'Control.Monad.State.evalState', but returns a 'Dao.Predicate.Predicate' based on how the
-- matcher evaluates. 'Control.Monad.mzero' evaluates to 'Dao.Predicate.Backtrack',
-- 'Control.Monad.fail' or 'Control.Monad.Error.Class.throwError' evaluates to
-- 'Dao.Predicate.PFail'. 'Control.Monad.return' evaluates to 'Dao.Predicate.OK'. Evaluate this
-- function in a case statement to take the appropriate action.
evalMatcher :: Matcher a -> Predicate Reference a
evalMatcher m = evalState (runPredicateT (matcherPTransState m)) initMatcherState

instance Monad Matcher where
  return = Matcher . return
  (Matcher f) >>= mfa = Matcher (f >>= matcherPTransState . mfa)
  (Matcher fa) >> (Matcher fb) = Matcher (fa >> fb)
  fail msg = getCurrentRef >>= \ref -> Matcher (throwError ref)

instance Functor Matcher where
  fmap f (Matcher a) = Matcher (fmap f a)

instance MonadPlus Matcher where
  mplus (Matcher a) (Matcher b) = Matcher (mplus a b)
  mzero = Matcher mzero

instance MonadState MatcherState Matcher where
  get = Matcher $ lift get
  put = Matcher . lift . put
  state = Matcher . lift . state

instance MonadError Reference Matcher where
  throwError = Matcher . throwError
  catchError (Matcher m) em = Matcher (catchError m (matcherPTransState . em))

instance MonadPlusError Reference Matcher where
  predicate = Matcher . predicate
  catchPredicate (Matcher fn) = Matcher (catchPredicate fn)

----------------------------------------------------------------------------------------------------

-- | Match a single object pattern to a single object.
matchObject :: Pattern -> Object -> Matcher Object
matchObject pat o = let otype = objType o in case pat of
  ObjAnyX -> return o
  ObjMany -> return o
  ObjAny1 -> return o
  ObjType t | Es.member t otype -> return o
  ObjBounded  lo hi             -> Matcher $ predicate $ msum $
    [ fmap Es.Point (objToRational o) >>= \r -> guard (lo <= r && r <= hi) >> return o
    , case o of
        OArray arr -> do
          let (a, b) = bounds arr
          guard (lo <= Es.Point (toRational a) && Es.Point (toRational b) <= hi)
          return o
        _          -> mzero
    ]
  ObjList t patx | t==otype || t==TrueType -> do
    case o of
      OArray a    -> matchWholeObjectList patx (elems a)
      OList  a    -> matchWholeObjectList patx a
      OPair (a,b) -> matchWholeObjectList patx [a, b]
      _           -> mzero
    return o
  ObjNameSet op set -> do
    case o of
      ODict dict -> matchObjectSet op set (T.Branch (M.map T.Leaf dict))
      OTree tree -> matchObjectSet op set tree 
      _          -> mzero
    return o
  ObjIntSet  op set -> do
    case o of
      OIntMap dict ->
        matchObjectSet op (S.fromList (map (:[]) (IS.elems set))) $
          T.Branch $ M.fromList $ map (\ (i, o) -> (i, T.Leaf o)) $ IM.assocs dict
      _            -> mzero
    return o
  ObjElemSet op set -> matchObjectElemSet op set o
  ObjChoice  op set -> matchObjectChoice  op set o >> return o
  ObjLabel  lbl pat -> withRef lbl $ do
    o <- matchObject pat o
    modify (\st -> st{ matcherTree = T.insert (reverse (matcherRef st)) o (matcherTree st) })
    return o
  ObjFailIf lbl pat -> mplus (matchObject pat o) (throwError (LocalRef lbl))
  ObjNot        pat -> catchPredicate (matchObject pat o) >>= \p -> case p of
    Backtrack  -> return o
    p          -> predicate p

justOnce :: [Bool] -> Bool
justOnce checks = 1 == foldl (\i check -> if check then i+1 else i) 0 checks

-- | Returns the objects that did match the given patterns, and the remaining objects.
matchObjectList :: [Pattern] -> [Object] -> Matcher ([Object], [Object])
matchObjectList patx ax = do
  (matched, ax) <- loop [] patx (zipIndicies ax)
  return (map snd matched, map snd ax)
  where
    loop matched patx ax = case patx of
      [] -> return (matched, ax)
      (ObjAnyX:ObjMany:patx) -> loop matched (ObjMany:patx) ax
      (ObjMany:ObjAnyX:patx) -> loop matched (ObjMany:patx) ax
      (pat            :patx) -> case pat of
        ObjAnyX -> try1 matched [] id               patx           ax
        ObjMany -> try1 matched [] reverse (reverse patx) (reverse ax)
        pat     -> case ax of
          []        -> mzero
          (i, a):ax -> withIdx i $ do
            a <- matchObject pat a
            loop (matched++[(i, a)]) patx ax
    try1 matched skipped rvrsIfRvrsd patx ax =
      flip mplusCatch (skip matched skipped rvrsIfRvrsd patx ax) $ do
        (ax, matched') <- loop [] patx ax
        return (ax, matched ++ rvrsIfRvrsd matched')
    skip matched skipped rvrsIfRvrsd patx ax = case ax of
      []   -> mzero
      a:ax -> try1 matched (skipped++[a]) rvrsIfRvrsd patx ax
    zipIndicies = zip (map OWord (iterate (+1) 0))

matchWholeObjectList :: [Pattern] -> [Object] -> Matcher ()
matchWholeObjectList patx ax =
  matchObjectList patx ax >>= \ (_, ax) -> if null ax then return () else mzero

-- | Match a set of indecies in an 'Pattern' to an object with indecies, for example, matching a
-- @['Dao.String.Name']@ to a 'Dao.Tree.Tree' will check if a branch of the given
-- @['Dao.String.Name']@ is defined in the tree. This is different than 'matchObjectElemSet' which
-- matches on the elements of a set, instead of the indecies. This is a 'Control.Monad.guard'
-- function, so it evaluates to @'Control.Monad.return' ()@ on success, and 'Control.Monad.mzero' on
-- failure.
matchObjectSet :: Ord a => ObjSetOp -> S.Set [a] -> T.Tree a Object -> Matcher ()
matchObjectSet op branches tree = guard $ case op of
  ExactSet  -> T.size isectd == T.size tree
  AnyOfSet  -> not $ null $ T.elems isectd
  AllOfSet  -> T.size isectd == T.size tree0
  OnlyOneOf -> length (T.elems isectd) == 1
  NoneOfSet -> null $ T.elems isectd
  where
    tree0  = makeTree branches
    isectd = T.intersection tree0 tree -- Yes, 'tree0' and then 'tree' is correct.
    -- The 'isectd' is a tree of branches mimicking the input set, but containing only 'OTrue'
    -- values, because in this predicate we don't care about the nature of the leaves, only the
    -- nature of the branches.

makeTree :: Ord a => S.Set [a] -> T.Tree a Object
makeTree branches = (T.fromList (map (\b -> (b, OTrue)) (S.elems branches)))

-- | Recurse into a set object and match a set of 'Pattern's to the elements, matching as many (or as
-- few) as possible to satisfy the given 'ObjSetOp'. Returns the subset of the object that matched
-- the pattern.
matchObjectElemSet :: ObjSetOp -> (S.Set Pattern) -> Object -> Matcher Object
matchObjectElemSet op set o = do
  let foldSet insert matched (i, ref, a) set patx = case patx of
        []       -> return (False, set, matched)
        pat:patx -> flip mplusCatch (foldSet insert matched (i, ref, a) (S.insert pat set) patx) $ do
          a <- withIdx ref (matchObject pat a)
          return (True, S.union set (S.fromList patx), insert i a matched)
      checkLoop construct insert matched checks set ax = case ax of
        []              -> return (checks, set, construct matched)
        ax | S.null set -> return (checks ++ map (const False) ax, set, construct matched)
        a:ax            -> do
          (check, set, matched) <- foldSet insert matched a (S.empty) (S.elems set)
          checkLoop construct insert matched (checks++[check]) set ax
      run construct insert null ax = checkLoop construct insert null [] set ax
      setInsert _  = S.insert
      setAssocs    = zip (iterate (+1) 0) . S.elems
      ixRefObj fn = map (\ (i, a) -> (i, fn i, a))
  (checks, remainder, o) <- case o of
    OSet    a -> run OSet    setInsert S.empty  $ ixRefObj  OWord                $ setAssocs a
    ODict   a -> run ODict   M.insert  M.empty  $ ixRefObj (ORef . LocalRef    ) $ M.assocs a
    OTree   a -> run OTree   T.insert  T.Void   $ ixRefObj (ORef . GlobalRef   ) $ T.assocs a
    OIntMap a -> run OIntMap IM.insert IM.empty $ ixRefObj (OInt . fromIntegral) $ IM.assocs a
    _         -> mzero
  guard $ case op of
    ExactSet  -> S.null remainder && and checks
    AnyOfSet  -> or checks
    AllOfSet  -> S.null remainder
    OnlyOneOf -> justOnce checks
    NoneOfSet -> not (or checks)
  return o

matchObjectChoice :: ObjSetOp -> S.Set Pattern -> Object -> Matcher ()
matchObjectChoice op set o = do
  let fn pat = mplus (matchObject pat o >> return True) (return False)
  checks <- mapM fn (S.elems set)
  guard $ case op of
    ExactSet  -> and checks
    AnyOfSet  -> or checks
    AllOfSet  -> and checks
    OnlyOneOf -> justOnce checks
    NoneOfSet -> not (or checks)

----------------------------------------------------------------------------------------------------

-- | This function is used for constructing built-in Dao-language functions and mathematical
-- operators. Two patterns are given, then a list of objects. If the list contains two objects and
-- each matches the given patterns, the match succeeds. This is more efficient than using the
-- general 'matchParamList', which stores each matched pattern into a tree which would require
-- operators like "+" to have to retrieve two values "fst" and "snd" from a tree once the match is
-- confirmed, hence it is better to use this function. We intended it to be used in this way:
-- @case 'evalMatcher' (matchFst, matchSnd) inputList of@
-- @    'Dao.Predicate.OK' (a, b)     -> evalMyFunc a b@
-- @    'Dao.Predicate.Backtrack'     -> tryMyAlternative@
-- @    'Dao.Predicate.PFail' ref msg -> 'Prelude.error' ('ustr' msg)
-- Be ware of 'Pattern's that evaluate to 'Control.Monad.Error.State.throwError', this will not
-- evaluate to a 'Dao.Predicate.Backtrack'ing value, so the first pattern to throw an error will
-- prevent further patterns from matching, which may or may not be what you expected.
matchPair :: (Pattern, Pattern) -> (Object, Object) -> Matcher (Object, Object)
matchPair (fstPat, sndPat) (fstObj, sndObj) = do
  fstObj <- matchObject fstPat fstObj
  sndObj <- matchObject fstPat sndObj
  return (fstObj, sndObj)

