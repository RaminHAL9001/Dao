-- "src/Dao/Object/Pattern.hs" a data type that can match
-- 'Dao.Object.Object's as a predicate.
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
{-# LANGUAGE MultiParamTypeClasses #-}

module Dao.Object.Pattern where

import           Dao.String
import           Dao.Object
import           Dao.Object.Math
import qualified Dao.Tree as T
import           Dao.Predicate
import           Dao.EnumSet

import           Data.Maybe
import           Data.Typeable
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
type MatchValue a = PValue Reference a

data MatcherState
  = MatcherState
    { matcherRef  :: [Name]
    , matcherIdx  :: [T_word]
    , matcherTree :: T.Tree Name Object
    }

initMatcherState :: MatcherState
initMatcherState = MatcherState{ matcherRef = [], matcherIdx = [], matcherTree = T.Void }

withRef :: Name -> Matcher a -> Matcher a
withRef n fn = modify (\st -> st{matcherRef = n : matcherRef st}) >>
  mplusFinal fn (modify (\st -> st{matcherRef = tail (matcherRef st)}))

withIdx :: T_word -> Matcher a -> Matcher a
withIdx i fn = modify (\st -> st{matcherIdx = i : matcherIdx st}) >>
  mplusFinal fn (modify (\st -> st{matcherIdx = tail (matcherIdx st)}))

getCurrentRef :: Matcher Reference
getCurrentRef = do
  idx <- flip fmap (gets matcherIdx) $
    foldl (\fn i -> fn . flip Subscript i) id . map OWord . reverse -- <-- reverse this?
  fmap (idx . GlobalRef . reverse) (gets matcherRef)

-- | A 'Control.Monad.State.State' monad used to execute matches
newtype Matcher a = Matcher { matcherPTransState :: PTrans Reference (State MatcherState) a }

instance Monad Matcher where
  return = Matcher . return
  (Matcher f) >>= mfa = Matcher (f >>= matcherPTransState . mfa)
  (Matcher fa) >> (Matcher fb) = Matcher (fa >> fb)
  fail msg = getCurrentRef >>= \ref -> Matcher (tokenFail ref msg)

instance Functor Matcher where
  fmap f (Matcher a) = Matcher (fmap f a)

instance MonadPlus Matcher where
  mplus (Matcher a) (Matcher b) = Matcher (mplus a b)
  mzero = Matcher mzero

instance MonadState MatcherState Matcher where
  get = Matcher $ lift get
  put = Matcher . lift . put
  state = Matcher . lift . state

instance MonadError UStr Matcher where
  throwError = Matcher . throwError
  catchError (Matcher m) em = Matcher (catchError m (matcherPTransState . em))

instance ErrorMonadPlus Reference Matcher where
  tokenThrowError tok msg  = Matcher (tokenThrowError tok msg)
  catchPValue (Matcher fn) = Matcher (catchPValue fn)

----------------------------------------------------------------------------------------------------

data ObjSetOp = ExactSet | AnyOfSet | AllOfSet | OnlyOneOf | NoneOfSet
  deriving (Eq, Ord, Typeable, Show, Read)

-- | An object pattern, a data type that can be matched against objects,
-- assigning portions of that object to variables stored in a
-- 'Dao.Tree.Tree' structure.
data ObjPat 
  = ObjAnyX -- ^ matches any number of objects, matches lazily (not greedily).
  | ObjMany -- ^ like ObjAnyX but matches greedily.
  | ObjAny1 -- ^ matches any one object
  | ObjEQ Object -- ^ simply checks if the object is exactly equivalent
  | ObjType (EnumSet TypeID) -- ^ checks if the object type is any of the given types.
  | ObjBounded (EnumInf T_ratio) (EnumInf T_ratio) -- ^ checks that numeric types are in a certain range.
  | ObjList TypeID [ObjPat]
    -- ^ recurse into a list-like object given by TypeID (TrueType for any list-like object)
  | ObjNameSet ObjSetOp (S.Set [Name]) -- ^ checks if a map object contains every name
  | ObjIntSet  ObjSetOp IS.IntSet
    -- ^ checks if an intmap or array object contains every index
  | ObjElemSet ObjSetOp (S.Set ObjPat)
    -- ^ recurse into a set-like object given by TypeID, match elements in the set according to
    -- ObjSetOp.
  | ObjChoice  ObjSetOp (S.Set ObjPat)
    -- ^ execute a series of tests on a single object
  | ObjLabel Name ObjPat
    -- ^ if the object matching matches this portion of the 'ObjPat', then save the object into the
    -- resulting 'Dao.Tree.Tree' under this name.
  | ObjFailIf UStr ObjPat -- ^ fail with a message if the pattern does not match
  deriving (Eq, Show, Typeable)

instance Ord ObjPat where
  compare a b
    | a==b      = EQ
    | otherwise = compare (toInt a) (toInt b) where
        f s = foldl max 0 (map toInt (S.elems s))
        toInt a = case a of
          ObjAnyX   -> 1
          ObjMany   -> 2
          ObjAny1   -> 3
          ObjEQ   _ -> 4
          ObjType _ -> 5
          ObjBounded _ _ -> 6
          ObjList    _ _ -> 7
          ObjNameSet _ _ -> 8
          ObjIntSet  _ _ -> 9
          ObjElemSet _ s -> f s
          ObjChoice  _ s -> f s
          ObjLabel   _ a -> toInt a
          ObjFailIf  _ w -> toInt a

-- | Match a single object pattern to a single object.
matchObject :: ObjPat -> Object -> Matcher Object
matchObject pat o = let otype = objType o in case pat of
  ObjAnyX       -> return o
  ObjMany       -> return o
  ObjAny1       -> return o
  ObjEQ q   | q == o            -> return o
  ObjType t | setMember t otype -> return o
  ObjBounded  lo hi             -> Matcher $ pvalue $ msum $
    [ fmap EnumPoint (objToRational o) >>= \r -> guard (lo <= r && r <= hi) >> return o
    , case o of
        OArray arr ->
          let (a, b) = bounds arr
          in  guard (lo <= EnumPoint (toRational a) && EnumPoint (toRational b) <= hi) >> return o
        _          -> mzero
    ]
  ObjList t patx | t==otype || t==TrueType -> fmap OList $ case o of
    OArray a    -> matchObjectList patx (elems a)
    OList  a    -> matchObjectList patx a
    OPair (a,b) -> matchObjectList patx [a,b]
    _           -> mzero
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
  ObjElemSet op set -> matchObjectElemSet op set o >> return o
  ObjChoice  op set -> matchObjectChoice  op set o >> return o
  ObjLabel  lbl pat -> modify (\st -> st{matcherRef = matcherRef st ++ [lbl]}) >> matchObject pat o
  ObjFailIf lbl pat -> mplus (matchObject pat o) (throwError lbl)

justOnce :: [Bool] -> Bool
justOnce checks = 1 == foldl (\i check -> if check then i+1 else i) 0 checks

-- | Match a list of object patterns to a list of objects.
matchObjectList :: [ObjPat] -> [Object] -> Matcher [Object]
matchObjectList patx ax = loop [] patx ax >>= \ (matched, ax) ->
  if null ax then return matched else mzero where
    loop matched patx ax = case patx of
      [] -> return (matched, ax)
      (ObjAnyX:ObjMany:patx) -> loop matched (ObjMany:patx) ax
      (ObjMany:ObjAnyX:patx) -> loop matched (ObjMany:patx) ax
      (pat            :patx) -> case pat of
        ObjAnyX -> try1 matched [] id               patx           ax
        ObjMany -> try1 matched [] reverse (reverse patx) (reverse ax)
        pat     -> case ax of
          []   -> mzero
          a:ax -> do
            matched' <- matchObject pat a
            loop (matched++[matched']) patx ax
    try1 matched skipped rvrsIfRvrsd patx ax =
      flip mplusCatch (skip matched skipped rvrsIfRvrsd patx ax) $ do
        (ax, matched') <- loop [] patx ax
        return (ax, matched ++ rvrsIfRvrsd matched')
    skip matched skipped rvrsIfRvrsd patx ax = case ax of
      []   -> mzero
      a:ax -> try1 matched (skipped++[a]) rvrsIfRvrsd patx ax

-- | Match a set of indecies in an 'ObjPat' to an object with indecies, for example, matching a
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
    isectd = T.merge T.intersection const tree0 tree -- Yes, 'tree0' and then 'tree' is correct.
    -- The 'isectd' is a tree of branches mimicking the input set, but containing only 'OTrue'
    -- values, because in this predicate we don't care about the nature of the leaves, only the
    -- nature of the branches.

makeTree :: Ord a => S.Set [a] -> T.Tree a Object
makeTree branches = (T.fromList (map (\b -> (b, OTrue)) (S.elems branches)))

-- | Recurse into a set object and match a set of 'ObjPat's to the elements, matching as many (or as
-- few) as possible to satisfy the given 'ObjSetOp'.
matchObjectElemSet :: ObjSetOp -> (S.Set ObjPat) -> Object -> Matcher ()
matchObjectElemSet op set o = do
  let foldSet o set patx = case patx of
        []       -> return (False, set)
        pat:patx -> flip mplusCatch (foldSet o (S.insert pat set) patx) $
          matchObject pat o >> return (True, S.union set (S.fromList patx))
      checkLoop checks set ax = case ax of
        []              -> return (checks, set)
        ax | S.null set -> return (checks ++ map (const False) ax, set)
        a:ax            -> do
          (check, set) <- foldSet a (S.empty) (S.elems set)
          checkLoop (checks++[check]) set ax
      checkEach ax = checkLoop [] set ax
  (checks, remainder) <- case o of
    OSet    o -> checkEach $ S.elems o
    ODict   o -> checkEach $ M.elems o
    OTree   o -> checkEach $ T.elems o
    OIntMap o -> checkEach $ IM.elems o
    _         -> mzero
  guard $ case op of
    ExactSet  -> S.null remainder && and checks
    AnyOfSet  -> or checks
    AllOfSet  -> S.null remainder
    OnlyOneOf -> justOnce checks
    NoneOfSet -> not (or checks)

matchObjectChoice :: ObjSetOp -> S.Set ObjPat -> Object -> Matcher ()
matchObjectChoice op set o = do
  let fn pat = mplus (matchObject pat o >> return True) (return False)
  checks <- mapM fn (S.elems set)
  guard $ case op of
    ExactSet  -> and checks
    AnyOfSet  -> or checks
    AllOfSet  -> and checks
    OnlyOneOf -> justOnce checks
    NoneOfSet -> not (or checks)

