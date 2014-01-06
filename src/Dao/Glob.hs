-- "src/Dao/Glob.hs"  functions and data types related to the Glob
-- data type, for matching unix-like glob patterns to strings.
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
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.


{-# LANGUAGE DeriveDataTypeable #-}

module Dao.Glob where

import           Dao.String
import qualified Dao.Tree as T
import           Dao.PPrint
import           Dao.Random

import           Control.Applicative
import           Control.Monad
import           Control.DeepSeq

import           Data.Typeable
import           Data.Function
import           Data.Monoid
import           Data.List
import           Data.Char
import qualified Data.Set as S
import qualified Data.Map as M

----------------------------------------------------------------------------------------------------

-- | Tokenize a 'Prelude.String' grouping together whitespace, numbers, letters, and punctuation
-- makrs, except for brackets and quote markers which will all be tokenized as single character
-- strings.
simpleTokenize :: String -> [UStr]
simpleTokenize ax = map ustr (loop ax) where
  loop ax =
    case ax of
      [] -> []
      a:ax | elem a "([{}])\"'`" -> [a] : loop ax
      a:ax -> case msum (map (check a ax) kinds) of
        Nothing -> [a] : loop ax
        Just (got, ax) -> got : loop ax
  check a ax fn = if fn a then let (got, ax') = span fn ax in Just (a:got, ax') else Nothing
  kinds = [isSpace, isAlpha, isNumber, isPunctuation, isAscii, not . isAscii]

----------------------------------------------------------------------------------------------------

-- | An alternative to 'Glob' expressions containing ordinary 'Dao.String.UStr's is a 'Glob'
-- expression containing 'FuzzyStr's. These strings approximately match the input string, ignoring
-- minor spelling errors and transposed characters.
newtype FuzzyStr = FuzzyStr UStr deriving Ord

instance Eq FuzzyStr where
  a==b = 
    let ax = S.map toLower (S.fromList (uchars a))
        bx = S.map toLower (S.fromList (uchars b))
    in     a == b
        || ax == bx
        || S.size (S.difference (S.union ax bx) (if S.size ax < S.size bx then ax else bx)) <= 1

instance Show FuzzyStr where { show (FuzzyStr str) = show str }

instance Read FuzzyStr where
  readsPrec p input = readsPrec p input >>= \ (s, rem) -> return (FuzzyStr (ustr s), rem)

instance Monoid FuzzyStr where
  mempty = FuzzyStr mempty
  mappend (FuzzyStr a) (FuzzyStr b) = FuzzyStr (a<>b)

instance HasNullValue FuzzyStr where
  nullValue = FuzzyStr nullValue
  testNull (FuzzyStr s) = testNull s

instance UStrType FuzzyStr where { fromUStr = FuzzyStr; toUStr (FuzzyStr u) = u; }

----------------------------------------------------------------------------------------------------

-- | A general glob type, remeniscent of the good old-fashioned Unix glob expression. Actually, this
-- is only a single unit of a full 'Glob' expression. The unit type need not be a string, but most
-- the instances of 'GlobUnit' into 'Prelude.Show' and 'Prelude.Read' are only defined for
-- 'GlobUnit's of 'Dao.String.UStr's.
data GlobUnit o = Wildcard Name | AnyOne Name | Single o deriving (Eq, Ord, Typeable)

instance Functor GlobUnit where
  fmap f o = case o of
    Single   o -> Single (f o)
    Wildcard n -> Wildcard n
    AnyOne   n -> AnyOne n

instance Show (GlobUnit UStr) where
  show a = case a of { Wildcard nm -> '$':show nm++"*" ; AnyOne nm -> '$':show nm ; Single a -> show a }

instance Read (GlobUnit UStr) where
  readsPrec _prec str = case str of
    '$':c:str | c=='_' || isAlpha c -> [span isAlphaNum str] >>= \ (cx, str) -> case str of
      '*':str -> [(Wildcard $ ustr (c:cx), str)]
      str     -> [(AnyOne   $ ustr (c:cx), str)]
    '$':c:str -> [(Single   $ ustr [c]   , str)]
    _         -> []

instance UStrType (GlobUnit UStr) where
  maybeFromUStr str = case readsPrec 0 (uchars str) of { [(o, "")] -> Just o; _ -> Nothing; }
  toUStr = ustr . show

instance NFData o => NFData (GlobUnit o) where
  rnf (Wildcard a) = deepseq a ()
  rnf (AnyOne   a) = deepseq a ()
  rnf (Single   a) = deepseq a ()

instance HasRandGen o => HasRandGen (GlobUnit o) where
  randO = countRunRandChoice
  randChoice = randChoiceList [Single <$> randO, Wildcard <$> randO, AnyOne <$> randO]

----------------------------------------------------------------------------------------------------

-- | Patterns are lists of 'Data.Maybe.Maybe' elements, where constant strings are given by
-- 'Data.Maybe.Just' or wildcards given by 'Data.Maybe.Nothing'. Wildcards runMatchPattern zero or more other
-- Tokens when used as a 'Glob'.
data Glob o = Glob { getPatUnits :: [GlobUnit o], getGlobLength :: Int }
  deriving (Eq, Ord, Typeable)

instance Functor Glob where
  fmap f g = g{ getPatUnits = fmap (fmap f) (getPatUnits g) }

instance Show (Glob UStr) where { show = concatMap show . getPatUnits }

instance Read (Glob UStr) where
  readsPrec prec str = do
    (units, str) <- loop [] str
    return (Glob{ getPatUnits=units, getGlobLength=length units }, str)
    where
      loop units str = case break (=='$') str of
        ("", "" ) -> return (units, "")
        ("", str) -> readsPrec prec str >>= \ (unit, str) -> loop (units++[unit]) str
        (cx, str) -> loop (units++[Single $ ustr cx]) str

instance Monoid (Glob o) where
  mempty = nullValue
  mappend (Glob{ getPatUnits=a, getGlobLength=lenA }) (Glob{ getPatUnits=b, getGlobLength=lenB }) =
    Glob{ getPatUnits=a++b, getGlobLength=lenA+lenB }

instance NFData o => NFData (Glob o) where { rnf (Glob a b) = deepseq a $! deepseq b () }

instance HasNullValue (Glob o) where
  nullValue = Glob{ getPatUnits=[], getGlobLength=0 }
  testNull (Glob{ getPatUnits=ax }) = null ax

instance UStrType (Glob UStr) where
  maybeFromUStr str = case readsPrec 0 (uchars str) of { [(o, "")] -> Just o; _ -> Nothing; }
  toUStr = ustr . show

instance PPrintable (Glob UStr) where { pPrint = pShow }

instance HasRandGen o => HasRandGen (Glob o) where
  randO = randList 1 6 >>= \o -> return $ Glob{ getPatUnits=o, getGlobLength=length o }

instance Show (Glob FuzzyStr) where { show = show . fmap toUStr }

instance Read (Glob FuzzyStr) where
  readsPrec prec str = readsPrec prec str >>= \ (glob, str) -> [(fmap fromUStr glob, str)]

----------------------------------------------------------------------------------------------------

-- | 'PatternTree's contain many patterns in an tree structure, which is more efficient when you
-- have many patterns that start with similar sequences of 'GlobUnit's.
type PatternTree g o = T.Tree (GlobUnit g) o

-- | Insert an item at multiple points in the 'PatternTree'
insertMultiPattern :: (Eq g, Ord g) => (o -> o -> o) -> [Glob g] -> o -> PatternTree g o -> PatternTree g o
insertMultiPattern plus pats o tree =
  foldl (\tree pat -> T.update (getPatUnits pat) (maybe (Just o) (Just . flip plus o)) tree) tree pats

-- | By converting an ordinary 'Glob' to a pattern tree, you are able to use all of the methods
-- in the "Dao.Tree" module to modify the patterns in it.
globTree :: (Eq g, Ord g) => Glob g -> o -> PatternTree g o
globTree pat a = T.insert (getPatUnits pat) a T.Void

matchPattern :: (Eq g, Ord g) => Bool -> Glob g -> [g] -> [T.Tree Name [g]]
matchPattern greedy pat tokx = matchTree greedy (globTree pat ()) tokx >>= \ (_, m, ()) -> [m]

-- | Match a list of token items to a set of 'Glob' expressions that have been combined into a
-- single 'PatternTree', matching every possible pattern in the 'PatternTree' to the list of token
-- items in depth-first order. The first boolean parameter indicates whether 'Wildcard's should be
-- matched greedily (pass 'Prelude.False' for non-greedy matching). Be aware that greedy matching is
-- /not lazy/ which could cause freezes if you are working with infinitely recursive data types.
-- Non-greedy matching is lazy and works fine with everything.
--
-- Each match is returned as a triple indicating 1. the 'Glob' that matched the token list, 2. the
-- token list items that were bound to the 'Dao.String.Name's in the 'Wildcard' and 'AnyOne'
-- 'GlobUnit's, and 3. the item associated with the 'Glob' expression that matched.
matchTree :: (Eq g, Ord g) => Bool -> PatternTree g o -> [g] -> [(Glob g, T.Tree Name [g], o)]
matchTree greedy tree tokx = loop T.Void 0 [] tree tokx where
  loop vars p path tree tokx = case (tree, tokx) of
    (T.Leaf       a  , []  ) -> done vars p path a
    (T.LeafBranch a _, []  ) -> done vars p path a
    (T.Branch       b, tokx) -> branch vars p path b tokx
    (T.LeafBranch _ b, tokx) -> branch vars p path b tokx
    _                        -> []
  done vars p path a = [(Glob{ getPatUnits = path, getGlobLength = p }, vars, a)]
  partStep bind tokx = (if greedy then reverse else id) $ (bind, tokx) :
    fix (\loop bind tokx -> if null tokx then [] else do
            bind <- [bind++[head tokx]]
            tokx <- [tail tokx]
            ((bind, tokx) : loop bind tokx)
        ) bind tokx
  branch vars p path branch tokx = do
    (pat, tree) <- M.assocs branch
    let next vars = loop vars (p+1) (pat:path) tree
    let defVar nm mkAssoc =
          maybe (mkAssoc >>= \ (bind, tokx) -> next (T.insert [nm] bind vars) tokx) (next vars) $
            T.lookup [nm] vars >>= flip stripPrefix tokx
    case pat of
      Wildcard nm -> defVar nm (partStep [] tokx)
      AnyOne   nm -> case tokx of { tok:tokx -> defVar nm [([tok], tokx)]; _ -> []; }
      Single   u  -> case tokx of { tok:tokx | tok==u -> next vars tokx;   _ -> []; }

