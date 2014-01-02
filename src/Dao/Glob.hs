-- "src/Dao/Glob.hs"  functions and data types related to the Glob
-- data type, for matching unix-like glob patterns to strings.
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

module Dao.Glob where

import           Dao.String
import qualified Dao.Tree as T
import           Dao.PPrint
import           Dao.Random

import           Control.DeepSeq
import           Control.Monad

import           Data.Typeable
import           Data.List
import           Data.Maybe
import           Data.Char
import           Data.Word
import           Data.Array.IArray
import qualified Data.Set as S
import qualified Data.Map as M

-- import Debug.Trace

type Tokens = [UStr]

-- | Tokenize a 'Prelude.String'.
tokens :: String -> Tokens
tokens ax = map ustr (loop ax) where
  loop ax =
    case ax of
      [] -> []
      a:ax | elem a "([{}])" -> [a] : loop ax
      a:ax ->
        case msum (map (check a ax) kinds) of
          Nothing -> [a] : loop ax
          Just (got, ax) -> got : loop ax
  check a ax fn = if fn a then let (got, ax') = span fn ax in Just (a:got, ax') else Nothing
  kinds = [isSpace, isAlpha, isNumber, isPunctuation, isAscii, not . isAscii]

-- | Contains information related to how a 'Glob' was matched to input 'Tokens' during the
-- 'runMatchPattern' function evaluation.
data Match
  = Match
    { originalInput :: Tokens
    , matchGaps :: Maybe (Array Word Tokens)
    }
    deriving Show

matchFromList :: Tokens -> Word -> [Tokens] -> Match
matchFromList orig sz stk =
  let arr = if sz==0 then Nothing else Just (listArray (1, sz) stk)
  in  Match{ originalInput = orig, matchGaps = arr }

getMatch :: Word -> Match -> Tokens
getMatch i ma =
  (fromMaybe (error "match retrieved from pattern which contained no wild-card symbols") $
    (matchGaps ma)) ! i

data GlobUnit = Wildcard | AnyOne | Single UStr deriving (Eq, Ord, Show)

-- | Patterns are lists of 'Data.Maybe.Maybe' elements, where constant strings are given by
-- 'Data.Maybe.Just' or wildcards given by 'Data.Maybe.Nothing'. Wildcards runMatchPattern zero or more other
-- Tokens when used as a 'Glob'.
data Glob = Glob { getPatUnits :: [GlobUnit], getGlobLength :: Int } deriving (Eq, Ord, Typeable)
instance Show Glob where
  show pat = show (concatMap fn (getPatUnits pat)) where
    fn a = case a of { Wildcard -> "$*" ; AnyOne -> "$?" ; Single a  -> uchars a }
instance Read Glob where
  readsPrec _ str = [(parsePattern str, "")]

instance NFData Glob     where { rnf (Glob          a b  ) = deepseq a $! deepseq b () }
instance NFData GlobUnit where { rnf a = seq a () }
instance PPrintable Glob where { pPrint = pShow }

instance HasRandGen Glob where
  randO = do
    len <- fmap (+6) (nextInt 6)
    i <- randInt
    let loop len i =
          if len<=1 then [] else let (i', n) = divMod i len 
          in  (n+1) : loop (len-n-1) i'
        cuts = loop len i
    tx <- fmap (([]:) . map (\t -> if t==0 then [AnyOne] else [Wildcard]) . randToBase 2) randInt
    let loop tx cuts ax = case cuts of
          []       -> [ax]
          cut:cuts ->
            let (wx, ax') = splitAt cut ax
                (t,  tx') = splitAt 1 tx
            in  t ++ wx : loop tx' cuts ax'
    patUnits <- fmap (concat . loop tx cuts . intersperse (Single (ustr " "))) $
      replicateM len (fmap (Single . randUStr) randInt)
    return (Glob{getPatUnits=patUnits, getGlobLength=length patUnits})

-- | Create a 'Glob' from its string representation.
parsePattern :: String -> Glob
parsePattern ax = Glob{ getPatUnits = patrn, getGlobLength = foldl (+) 0 lenx } where
  (lenx, patrn) = unzip (loop ax)
  loop ax =
    case ax of
      []         -> []
      '$':'*':ax -> (0, Wildcard) : loop ax
      '$':'?':ax -> (1, AnyOne) : loop ax
      '$':'$':ax -> (1, Single (ustr "$")) : loop ax
      '$':ax     -> next "$" ax
      ax         -> next "" ax
      where
        next pfx ax =
          let (got, ax') = span(/='$') ax
          in  map (\a -> (1, Single a)) (tokens (pfx++got)) ++ loop ax'

-- | 'PatternTree's contain many patterns in an tree structure, which is more efficient when you
-- have many patterns that start with similar sequences of 'GlobUnit's.
type PatternTree a = T.Tree GlobUnit a

-- | Insert an item at multiple points in the 'PatternTree'
insertMultiPattern :: (a -> a -> a) -> [Glob] -> a -> PatternTree a -> PatternTree a
insertMultiPattern plus pats o tree =
  foldl (\tree pat -> T.update (getPatUnits pat) (maybe (Just o) (Just . flip plus o)) tree) tree pats

-- | By converting an ordinary 'Glob' to a pattern tree, you are able to use all of the methods
-- in the "Dao.Tree" module to modify the patterns in it.
toTree :: Glob -> a -> PatternTree a
toTree pat a = T.insert (getPatUnits pat) a T.Void

-- | Intended to be used as the first argument to 'runMatchPattern'. Each segment of the 'Glob' is
-- matched *exactyl* to each input 'Token' exactly, same as 'Prelude.(==)'.
exact :: UStr -> UStr -> Bool
exact = (==)

-- | Intended to be used as the first argument to 'runMatchPattern'. Each segment of a 'Glob' is
-- matched to each input 'Token' *approximately* using a very simple heuristic. This will eliminate
-- some spelling errors, but may cause ambiguity amongst anagrams, for example "crash" will
-- runMatchPattern "chars", "ant" will runMatchPattern "tan", etc. Typing errors where letters are missing
-- or extra letters are insterted into a word from the input string will also runMatchPattern the given
-- runMatchString string, so the input "cany ou?" will runMatchPattern the runMatchString "can you?", and the
-- input "he found" will runMatchPattern the runMatchString "she fond". Use 'approx' in situations when you
-- expect typing errors, and ambiguity can be resolved from context.
approx :: UStr -> UStr -> Bool
approx a b =
  let ax = S.map toLower (S.fromList (uchars a))
      bx = S.map toLower (S.fromList (uchars b))
  in     a == b
      || ax == bx
      || S.size (S.difference (S.union ax bx) (if S.size ax < S.size bx then ax else bx)) <= 1

-- | Takes a 'Dao.Types.UStr' matching function, usually 'exact' or 'approx', and tries to runMatchPattern a
-- string of 'Tokens' to a 'Glob', returning @'Data.Maybe.Just' 'Match'@ on a successful runMatchPattern
-- ('Data.Maybe.Nothing' on no runMatchPattern) with wildcards stored in an array in the 'Match' data
-- structure.
matchPattern :: (UStr -> UStr -> Bool) -> Glob -> Tokens -> [Match]
matchPattern eq pat tokx = matchTree eq (toTree pat ()) tokx >>= (\ (_,m,_) -> [m])
----------------------------------------------------------------------------------------------------
-- This was the oringinal algorithm. It was tested, and it works well. I am keeping it here, just in
-- case it might come in useful some day.
--  matchPattern :: (UStr -> UStr -> Bool) -> Glob -> Tokens -> Maybe Match
--  matchPattern eq runMatchString ax = loop 0 [] pln (getPatUnits runMatchString) aln ax where
--    aln = length ax
--    pln = getGlobLength runMatchString
--    loop sz stk pln px aln ax =
--      case (px, ax) of
--        ([]           , []  )          -> done sz stk
--        ([Wildcard]   , ax  )          -> done (sz+1) (stk++[ax])
--        (Wildcard:px  , ax  )          -> msum (skip [] sz stk pln px aln ax)
--        ((Single p):px, a:ax) | eq p a -> loop sz stk (pln-1) px (aln-1) ax
--        (AnyOne:px    , a:ax)          -> loop (sz+1) (stk++[[a]]) (pln-1) px (aln-1) ax
--        _                              -> Nothing
--    skip gap sz stk pln px aln ax =
--      case ax of
--        a:ax | aln>=pln ->
--            loop (sz+1) (stk++[gap]) pln px aln (a:ax)
--          : skip (gap++[a]) sz stk pln px (aln-1) ax
--        _                         -> []
--    done sz stk = Just (matchFromList ax sz stk)
----------------------------------------------------------------------------------------------------

-- | Takes a tree full of 'GlobUnit' objects, treating each path to a leaf as a list of 'GlobUnit'
-- objects that comopses a single pattern, then matches the input 'Tokens' to each pattern that was
-- composed from every path to every leaf in the given 'Dao.Tree' object. Evaluates every possible
-- match. This is actually the the more general algorithm, and the 'matchPattern' token is a special
-- condition of this algorithm where the 'matchTree' contains only one path to one Leaf.
-- NOTE: Rules that have multiple patterns may execute more than once if the input matches more than
-- one of the patterns associated with the rule. *This is not a bug.* Each pattern may produce a
-- different set of match results, it is up to the programmer of the rule to handle situations where
-- the action may execute many times for a single input.
matchTree :: (UStr -> UStr -> Bool) -> PatternTree a -> Tokens -> [(Glob, Match, a)]
matchTree eq matchTree tokx = loop 0 [] 0 [] matchTree tokx where
  loop sz stk p path bx tokx =
    case (bx, tokx) of
      (T.Leaf       a  , []  ) -> done sz stk p path a
      (T.LeafBranch a b, []  ) -> done sz stk p path a ++ branch sz stk p path b []
      (T.Branch       b, tokx) -> branch sz stk p path b tokx
      (T.LeafBranch a b, tokx) -> done sz stk p path a ++ branch sz stk p path b tokx
      _                        -> []
  branch sz stk p path b tokx = do
    (pat, bx) <- M.assocs b
    case pat of
      Wildcard -> skip [] sz stk (p+1) (path++[pat]) bx tokx
      AnyOne   ->
        case tokx of
          []       -> []
          tok:tokx -> loop (sz+1) (stk++[[tok]]) (p+1) (path++[pat]) bx tokx
      Single u ->
        case tokx of
          []       -> []
          tok:tokx -> if eq u tok then loop sz stk (p+1) (path++[pat]) bx tokx else []
  skip gap sz stk p path bx tokx =
       loop (sz+1) (stk++[gap]) p path bx tokx
    ++ if null tokx then [] else skip (gap++[head tokx]) sz stk p path bx (tail tokx)
  done sz stk p path a =
    [(Glob{ getPatUnits = path, getGlobLength = p }, matchFromList tokx sz stk, a)]

-- | Match a pattern to a simple 'Prelude.String' without tokenizing the string, but returning each
-- part that matched individually.
stringMatch :: Glob -> String -> Maybe [UStr]
stringMatch pat cx = loop [] (getPatUnits pat) cx where
  loop retrn px cx = case cx of
    ""    -> case px of
      []          -> Just retrn
      [Wildcard]  -> Just retrn
      _           -> Nothing
    c:cx  -> case px of
      []               -> Nothing
      AnyOne      : px -> loop (retrn++[ustr [c]]) px cx
      Single ustr : px -> stripPrefix (uchars ustr) (c:cx) >>= loop (retrn++[ustr]) px
      Wildcard    : px -> scan "" (c:cx) where
        scan skipped cx = case cx of
          ""   -> loop [] px cx
          c:cx -> msum $
            [ loop [] px (c:cx) >>= \match -> Just (retrn++[ustr skipped]++match)
            , scan (skipped++[c]) cx
            ]

----------------------------------------------------------------------------------------------------
--import System.IO
--  
--  testPattern = loop where
--    input = "The quick fire fox jumped over the lazy brown dog."
--    outFile = "./testPattern.out"
--    tok = tokens input
--    strpats =
--      [ input
--      , "fox jumped over"
--      , "fire $* over"
--      , "fire $* over $* brown"
--      , "fire $? jumped"
--      , "fire $? jumped $* brown"
--      , "fire $* the $? brown"
--      , "fire time"
--      , "fire $? time"
--      , "fire $* over time"
--      , "fire $? jumped time"
--      , "fire $* over $* time"
--      , "fire $? jumped $* time"
--      , "fire $* the $? time"
--      ]
--    vary pat = map parsePattern $
--      pat ++ map ("$*"++) pat ++ map (++"$*") pat ++ map (\s -> "$*"++s++"$*") pat
--    pats = vary strpats
--    pprin itm = (++"\n") $
--      if null itm
--        then "(no match)"
--        else flip concatMap itm $ \itm ->
--          case matchGaps itm of
--             Nothing  -> "(no wildcards)"
--             Just arr -> concatMap (\ (i,tokx) -> show i++' ':show tokx++"\n") (assocs arr)
--    loop = do
--      h <- openFile outFile WriteMode
--      --let h = stdout
--      forM_ pats $ \pat -> do
--        let ex = matchPattern exact pat tok
--            ap = matchPattern approx pat tok
--        hPutStrLn h (show pat)
--        seq ex $ hPutStrLn h (pprin ex)
--        hPutStrLn h (show pat)
--        seq ap $ hPutStrLn h (pprin ap)
--      hFlush h
--      putStrLn ("Wrote test results to file "++show outFile++".\nDone.")
--      hClose h

