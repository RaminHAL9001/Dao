-- "src/Dao/Ext/WordTree.hs"  an experimental module using Dao modules
-- to build statistical models of language.
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


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dao.Ext.WordTree where

import           Dao.String
import qualified Dao.Tree as T
import           Dao.Predicate
import           Dao.NewParser
import           Dao.Object.Binary

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Concurrent.MVar

import           Data.Char
import           Data.Word
import           Data.Ratio
import qualified Data.Map         as M
import qualified Data.Binary      as B
import qualified Data.Binary.Get  as B
import qualified Data.Binary.Put  as B
import           Data.Maybe
import           Data.Monoid
import           Data.List

import           System.IO
import           System.Random

----------------------------------------------------------------------------------------------------

type Probability = Word32

data Sym = Sym UStr | BEGIN | END | PARA | QUES | ANS deriving (Eq, Ord)
instance Show Sym where
  show sym = case sym of
    Sym s -> show s
    BEGIN -> "<PHRASE>"
    END   -> "</PHRASE>"
    PARA  -> "<PARAGRAPH />"
    QUES  -> "<QUESTION />"
    ANS   -> "<ANSWER />"

newtype SymTree = SymTree { getTree :: T.Tree Sym Probability }
instance Monoid SymTree where
  mempty                          = SymTree T.Void
  mappend (SymTree a) (SymTree b) = SymTree (T.unionWith (+) a b)

updateSymTree :: (T.Tree Sym Probability -> T.Tree Sym Probability) -> SymTree -> SymTree
updateSymTree fn (SymTree tree) = SymTree (fn tree)

instance B.Binary Sym where
  put sym = case sym of
    Sym u -> B.put u
    BEGIN -> B.putWord8 0xBF
    END   -> B.putWord8 0xEF
    PARA  -> B.putWord8 0xFA
    QUES  -> B.putWord8 0xE5
    ANS   -> B.putWord8 0xA5
  get = B.lookAhead B.getWord8 >>= \w -> case w of
    0xBF -> B.getWord8 >> return BEGIN
    0xEF -> B.getWord8 >> return END
    0xFA -> B.getWord8 >> return PARA
    0xE5 -> B.getWord8 >> return QUES
    0xA5 -> B.getWord8 >> return ANS
    _    -> fmap Sym B.get

newtype StorableProbability = StorableProbability { getProbability :: Probability } deriving (Eq, Ord)
instance B.Binary StorableProbability where
  put (StorableProbability p) = B.putWord32be p
  get = fmap StorableProbability B.get

instance B.Binary SymTree where
  put (SymTree tree) = B.put (T.map StorableProbability tree)
  get = fmap (SymTree . T.map getProbability) B.get

-- | Add a list of symbols to the tree given a weighting function. For example, if this list of
-- symbols you would like to add should increase the probability rating of this phrase by a value of
-- one, then the weighting function should simply be @(+1)@. The ordering of the parameters of
-- this function make it easy to use with 'Prelude.foldl'.
updateTree :: (Probability -> Probability) -> T.Tree Sym Probability -> [Sym] -> T.Tree Sym Probability
updateTree upd tree addr = T.alterNode (T.alterData (return . upd . fromMaybe 0)) addr tree

-- | If the list length is less than the given 'Prelude.Int' value, return the list alone.
-- Otherwise, iterate returning every list containing exactly the first 'Prelude.Int' number of
-- elements from the head of the list, and every next list in the iteration is:
-- > rotationsOfLength i (tail items)
-- For example, @rotationsOfLength 5 [1..1]@ will produce a list of lists:
-- > [[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7],[4,5,6,7,8], ...]
rotationsOfLength :: Int -> [a] -> [[a]]
rotationsOfLength i ax = if len<=i then [ax] else loop len ax where
  len = length ax
  loop len ax = if len>i then take i ax : loop (len-1) (tail ax) else [ax]

-- | A type of 'Dao.NewParser.GenLexer' which simply breaks-up english-language strings. The token
-- type is 'Prelude.Char', but the token type is not really used, except to filter out spaces.
type NaturalLexer = GenLexer Char ()

-- | Often people write things like !!!! or .... or -- or ???, these will be treated as single
-- tokens.
commonPunct :: Char -> NaturalLexer
commonPunct c = lexCharP (==c) >> mplus (lexWhile (==c)) (return ()) >> makeToken c

-- | Lex alphabetic language phrases.
alphabeticPhraseLexer :: [NaturalLexer]
alphabeticPhraseLexer =
  [ lexWhile isAlpha   >> makeToken 'A'
  , lexWhile isDigit   >> makeToken '1'
  , lexWhile isSpace   >> makeToken '_'
  ] ++ map commonPunct ".*?!-$" ++
    [ lexCharP (\c -> isSymbol c || isPunctuation c)  >> makeToken 'S'
    , do  lexCharP (const True)
          c <- gets lexBuffer
          fail ("illegal characters: "++concatMap show c)
    ]

-- | Lex alphabetic phrases. Sentences and paragraphs deliminters are not considered.
lexPhrase :: String -> [Sym]
lexPhrase input = case result of
  OK      _ -> toks
  Backtrack -> toks
  PFail err -> error (show err)
  where
    (result, st) = lexicalAnalysis (many $ msum alphabeticPhraseLexer) (newLexerState input)
    nonSpaces t  = if tokType t == '_' then [] else [tokToUStr t]
    toks         = map Sym (concatMap (nonSpaces . getToken) (tokenStream st))

-- | Add a phrse to the symbol tree, given a rotation length, a weight value (1 is a good value if
-- you are not sure) a string, and the tree to update. The ordering of the parameters of this
-- function make easy to use with 'Prelude.foldl'.
addSyms :: Int -> (Probability -> Probability) -> SymTree -> [Sym] -> SymTree
addSyms rotLen weight (SymTree tree) input = SymTree $
  foldl (updateTree weight) tree (rotationsOfLength rotLen input)

-- | Lookup a probability of a path with a given path length. Probability values are not normalized,
-- so you might get values larger than 1. To get the normalized probability value between 0 and 1,
-- you must divide the value produced by this function by the weight of the tree. See 'treeWeight'.
lookupProbability :: Int -> [Sym] -> SymTree -> Ratio Probability
lookupProbability pathlen path (SymTree tree) = sum % count where
  getProb path = fromMaybe 0 (T.lookup path tree)
  (sum, count) = foldl (\ (sum, count) path -> (sum + getProb path, count+1)) (0, 0) $
    rotationsOfLength pathlen path

-- | Computes the sum of all probability values in the given 'SymTree', returning the total weight
-- of the tree.
treeWeight :: SymTree -> Probability
treeWeight (SymTree tree) = foldl (+) 0 (T.elems tree)

-- | Given a path, return every next symbol and the probability of it's occurence.
lookupValues :: [Sym] -> SymTree -> [(Probability, Sym)]
lookupValues path (SymTree tree) = case T.lookupNode path tree of
  Nothing   -> []
  Just tree -> case tree of
    T.Void           -> []
    T.Branch       b -> branch b
    T.Leaf       _   -> []
    T.LeafBranch _ b -> branch b
  where
    branch b = map (\ (sym, tree) -> (getProb tree, sym)) (M.assocs b)
    getProb tree = case tree of
      T.Void           -> 1
      T.Leaf       p   -> p
      T.Branch       _ -> 1
      T.LeafBranch p _ -> p
      -- ^ defaults to 1 because if the path exists up to this point, that sequence of symbols has
      -- occurred at least once.

-- | Sort a probability list from greatest to least.
sortProbList :: [(Probability, a)] -> [(Probability, a)]
sortProbList = sortBy fn where
  fn (a, _) (b, _) = case compare a b of { EQ -> EQ ; LT -> GT ; GT -> LT }

-- | Given a random probability value, select a value from the list. If the random probability value
-- is selected from a flat probability distribution, the resulting lookup will be exactly weighted
-- by the probability associated with it. For example, if the list to lookup is:
-- > [(1, a), (99, b)]
-- then you have a 1% chance of drawing @a@ and a 99% chance of drawing @b@.
selectRandom :: Probability -> [(Probability, a)] -> Maybe a
selectRandom prob items = loop rand (sortProbList items) where
  normal    = foldl (+) 0 (map fst items)
  rand      = mod prob normal
  loop i ax = case ax of
    []          -> mzero
    (p, a) : ax -> let j = i-p in if j<0 then return a else loop j ax

-- | combine symbols into a string.
concatSymsToString :: [Sym] -> String
concatSymsToString = noInitSpace . concatMap fn where
  noInitSpace cx = case cx of
    ""     -> ""
    ' ':cx -> cx
  fn sym = case sym of
    Sym s -> case uchars s of
      c:cx | isSymbol c || isPunctuation c -> c:cx++" "
      cx                                   -> ' ':cx

----------------------------------------------------------------------------------------------------

data RandWalkState
  = RandWalkState
    { generatedWalk :: [Sym]
    , randNumGen    :: StdGen
    , pathLength    :: Int
    , walkingPath   :: [Sym]
    , symbolTree    :: SymTree
    }

type RandWalk a = State RandWalkState a

instance RandomGen RandWalkState where
  next  st = let (i, g) = next  (randNumGen st) in (i               , st{randNumGen=g})
  split st = let (a, b) = split (randNumGen st) in (st{randNumGen=a}, st{randNumGen=b})

instance B.Binary RandWalkState where
  put st = B.put (pathLength st) >> B.put (symbolTree st)
  get = B.get >>= \len -> B.get >>= \tree -> return ((newRandWalkState len 0){symbolTree = tree})

newRandWalkState :: Int -> Int -> RandWalkState
newRandWalkState pathLength randSeed =
  RandWalkState
  { generatedWalk = mempty
  , randNumGen    = mkStdGen randSeed
  , pathLength    = pathLength
  , walkingPath   = mempty
  , symbolTree    = mempty
  }

randProbability :: RandWalk Probability
randProbability = fmap fromIntegral (state next)

randWalkReset :: RandWalk ()
randWalkReset = modify (\st -> st{ generatedWalk = [], walkingPath = [] })

-- | Initialized the 'RandWalkState' with a given input.
randWalkInitWith :: [Sym] -> RandWalk ()
randWalkInitWith syms =
  if null syms
    then  return ()
    else  do
      len <- gets pathLength
      modify (\st -> st{ walkingPath = foldl1 (flip const) (rotationsOfLength len syms) })

-- | Generate the next step in the random walk. If the step could not find any more 'Sym's given
-- it's current position 'Data.Maybe.Nothing' is returned. Otherwise, the next 'Sym' is returned.
randWalkStep :: RandWalk (Maybe Sym)
randWalkStep = do
  path <- gets walkingPath
  let lkup (SymTree tree) = SymTree $ fromMaybe tree (T.lookupNode path tree)
  rand  <- randProbability
  probs <- fmap (selectRandom rand . lookupValues path . lkup) (gets symbolTree)
  case probs of
    Nothing -> return Nothing
    Just  a -> do
      modify $ \st ->
        st{ walkingPath   = take (pathLength st) (walkingPath st ++ [a])
          , generatedWalk = generatedWalk st ++ [a]
          }
      return (Just a)

-- | Execute a random walk, producing symbols until a symbol is produced that causes the given
-- predicate to evaluate to 'Prelude.True'.
randWalkUntil :: (Sym -> Bool) -> RandWalk [Sym]
randWalkUntil predicate = loop >> gets generatedWalk where
  loop = randWalkStep >>= \a -> case a of
    Nothing -> return ()
    Just  a -> if predicate a then return () else loop

addSymsM :: (Probability -> Probability) -> [Sym] -> RandWalk ()
addSymsM fn syms = modify $ \st ->
  st{ symbolTree = addSyms (pathLength st + 1) fn (symbolTree st) syms }

----------------------------------------------------------------------------------------------------

type RandWalkStateIO = MVar RandWalkState

withMVarState :: RandWalkStateIO -> RandWalk a -> IO a
withMVarState mvar fn = modifyMVar mvar (\st -> let (a, st') = runState fn st in return (st', a))

-- | Converts the whole input string to lower case, then evaluates 'lexPhrase'.
lexSimpleText :: String -> [Sym]
lexSimpleText = lexPhrase . map toLower

new :: Int -> Int -> IO RandWalkStateIO
new a b = newMVar (newRandWalkState a b)

addwith :: RandWalkStateIO -> (String -> [Sym]) -> String -> IO ()
addwith mvar mkSyms str = withMVarState mvar (addSymsM (+1) (mkSyms str))

-- | Add a single string using 'lexSimpleText'.
add :: RandWalkStateIO -> String -> IO ()
add mvar = addwith mvar lexSimpleText

readtextwith :: RandWalkStateIO -> (String -> [Sym]) -> FilePath -> IO ()
readtextwith mvar mkSyms fpath = readFile fpath >>= \text ->
  withMVarState mvar (addSymsM (+1) (mkSyms text))

-- | Uses 'lexSimpleText' to update the state with the contents of the given file.
readtext :: RandWalkStateIO -> FilePath -> IO ()
readtext mvar fpath = readtextwith mvar lexSimpleText fpath

-- | Write the state to a file.
save :: RandWalkStateIO -> FilePath -> IO ()
save mvar fpath = readMVar mvar >>= B.encodeFile fpath

load :: FilePath -> IO RandWalkStateIO
load fpath = B.decodeFile fpath >>= newMVar

clear :: RandWalkStateIO -> IO ()
clear mvar = withMVarState mvar randWalkReset

-- | Use with 'randwalk' to stop the random walk when a symbol that matches the given string is
-- generated.
symstr :: String -> Sym -> Bool
symstr str sym = case sym of
  Sym sym -> uchars sym == str
  _       -> False

randwalk :: RandWalkStateIO -> (Sym -> Bool) -> IO [Sym]
randwalk mvar predicate = withMVarState mvar (randWalkUntil predicate)

-- | Pretty-print a list of symbols.
ppsyms :: [Sym] -> IO ()
ppsyms = putStrLn . concatSymsToString

