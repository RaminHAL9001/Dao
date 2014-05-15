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

-- | The 'Glob' expression data type is constructed by parsing a string containing a 'Glob'
-- expression. Reminscent of old-fashioned POSIX glob expressions that you would use in UNIX or
-- Linux systems on the command line (@ls *.hs@).
-- 
-- Also of use is the 'PatternTree' type. This 'Dao.Tree.Tree' data type allows you to associate
-- arbitrary object values with 'Glob' expressions. You can insert 'Glob' expressions into a
-- 'PatternTree' with 'insertMultiPattern' and then use 'matchTree' to match a string expression.
-- Every pattern that matches will return the object value associated with it along with a
-- @'Dao.Tree.Tree' 'Dao.String.Name'@ mapping which substrings matched which wildcards.
-- 
-- The syntax for a glob expression is just an arbitrary string with @'$'@ characters indicating
-- variables. A @'$'@ must be followed by at least one alphabetic or underscore character, and then
-- zero or more alphanumeric characters or underschore characters. These characters may then be
-- followed by a @'?'@. For example:
-- > "some text $wildcard more text"
-- > "some text $wildcard* more text"
-- > "some text $anyone? more text"
-- The first and second forms are identical, you may choose to follow a wildcard with a @'*'@ if you
-- want the 'Wildcard' variable to be followed by text with no space or punctuation in between.
-- 'Wildcard's match arbitrary-length sequences of string constants. For example, the above 'Glob'
-- containing the variable called @wildcard@ will match the following strings:
-- > "some text more text" -> a variable called "wildcard" is assigned an empty list
-- > "some text a more text" -> a variable called "wildcard" is assigned the list [a]
-- > "some text a b more text" -> a variable called "wildcard" is assigned the list [a b]
-- > "some text a b c more text" -> a variable called "wildcard" is assigned the list [a b c]
-- An 'AnyOne' variable matches any string constant, but one and only one. The following strings
-- will match the above example:
-- > "some text then more text" -> a variable called "anyone" is assigned the list [then]
-- > "some text with more text" -> a variable called "anyone" is assigned the list [with]
-- But the above exaple will not match:
-- > "some text more text"
-- > "some text then with more text"
-- Variable matched are stored in @('Dao.Tree.Tree' 'Dao.String.Name')@ structures.
-- 
-- Glob expressions are wrappers around lists of 'GlobUnit's. Each 'GlobUnit' is a 'Wildcard',
-- 'AnyOne' variable, or a string constant called a 'Single'. It is called 'Single' rather than
-- 'Control.Applicative.Const' to avoid conflicting with the data type defined in the
-- "Control.Applicative" module.
-- 
-- The data type used to store 'Single' string constants is polymorphic. So you can construct a
-- 'Glob' containing 'Prelude.String's, 'Dao.String.UStr's, or anything that can be constructed from
-- a 'Prelude.String'.
-- 
-- /NOTE:/ that when a 'Glob' is parsed using 'Prelude.read', the string constant is the substring
-- of all characters between the variables. If there are no variables, the whole string will be
-- stored into a list of just one 'Single' string constant. However this behavior may not be useful.
-- It may be useful to break down string constants into smaller 'Single' string constants. To do
-- this, use the 'parseOverSingles' function.
-- 
-- The following is a simple program you can use from the command line in GHCi to observe how to
-- construct 'Glob' expressions and try matching strings to these 'Glob's to see the result.
-- > import System.IO.Unsafe
-- > import Data.IORef
-- > 
-- > -- Establish a global variable for GHCi.
-- > testref :: IORef (PatternTree String String)
-- > testref = unsafePerformIO (newIORef T.Void)
-- > 
-- > -- A function to break-up a string into clusters of spaces, numbers, or letters.
-- > breakstr :: String -> [String]
-- > breakstr cx = loop cx where
-- >   check cx func = case cx of
-- >     c:cx | func c -> Just $ span func (c:cx)
-- >     _             -> Nothing
-- >   loop cx =
-- >     if null cx
-- >     then  []
-- >     else  maybe ([head cx] : loop (tail cx)) (\ (cx, rem) -> cx : loop rem) $
-- >             foldl (\a -> mplus a . check cx) Nothing [isSpace, isAlpha, isDigit]
-- > 
-- > -- Use 'Prelude.read' to parse a 'Glob' expression with 'Prelude.String's as the constant
-- values. Also, use 'parseOverSignles' to break-down the string constants using breakstr above.
-- > parsepat :: String -> Glob String
-- > parsepat = flip parseOverSingles breakstr . read
-- > 
-- > newpat :: String -> String -> IO ()
-- > newpat pat act = do
-- >   let glob = parsepat pat
-- >   modifyIORef testref (insertMultiPattern (flip const) [glob] act)
-- >   putStrLn $ "added pattern: "++show glob
-- > 
-- > delpat :: String -> IO ()
-- > delpat str = modifyIORef testref (T.delete (getPatUnits $ parsepat str))
-- > 
-- > ls :: IO ()
-- > ls = readIORef testref >>= putStrLn . disp "" where
-- >   disp ind t = case t of
-- >     T.Void           -> "()"
-- >     T.Leaf       o   -> show o
-- >     T.Branch       m -> dispMap ind m
-- >     T.LeafBranch o m -> " = " ++ show o ++ " ..." ++ dispMap ind m
-- >   dispMap ind m = (++(ind++"}")) $ ("{\n"++) $
-- >     if M.null m
-- >     then "(empty map)"
-- >     else  unlines $ do
-- >             (g, tree) <- M.assocs m
-- >             ['\t':ind ++ unwords ['"':show g++"\"", "=", disp ('\t':ind) tree]]
-- > 
-- > trypat :: String -> IO ()
-- > trypat instr = do
-- >   tree <- readIORef testref
-- >   forM_ (matchTree True tree (breakstr instr)) $ \ (glob, vars, o) -> do
-- >     putStrLn $ "pattern: "++show glob
-- >     putStrLn $ "action:  "++show o
-- >     putStrLn $ ("vars assigned:\n"++) $ unlines $ flip map (T.assocs vars) $ \ (nm, o) -> unwords $
-- >       ['\t':show nm, "=", show (unwords o)]
module Dao.Glob where

import           Dao.String
import qualified Dao.Tree as T
import           Dao.PPrint
import           Dao.Random

import           Control.Applicative
import           Control.Monad.Identity
import           Control.DeepSeq

import           Data.Typeable
import           Data.Monoid
import           Data.List
import           Data.Char
import qualified Data.Map as M

----------------------------------------------------------------------------------------------------

-- | Tokenize a 'Prelude.String' grouping together whitespace, numbers, letters, and punctuation
-- makrs, except for brackets and quote markers which will all be tokenized as single character
-- strings.
simpleTokenize :: String -> [UStr]
simpleTokenize ax = map ustr (loop ax) where
  loop ax = case ax of
    [] -> []
    a:ax | elem a "([{}])\"'`" -> [a] : loop ax
    a:ax -> case msum (map (check a ax) kinds) of
      Nothing -> [a] : loop ax
      Just (got, ax) -> got : loop ax
  check a ax fn = if fn a then let (got, ax') = span fn ax in Just (a:got, ax') else Nothing
  kinds = [isSpace, isAlpha, isNumber, isPunctuation, isAscii, not . isAscii]

----------------------------------------------------------------------------------------------------

-- | A general glob type, remeniscent of the good old-fashioned Unix glob expression. Actually, this
-- is only a single unit of a full 'Glob' expression. The unit type need not be a string, but most
-- the instances of 'GlobUnit' into 'Prelude.Show' and 'Prelude.Read' are only defined for
-- 'GlobUnit's of 'Dao.String.UStr's.
data GlobUnit o
  = Wildcard Name (Maybe Name)
  | AnyOne   Name (Maybe Name)
  | Single o
  deriving (Eq, Typeable)

-- Order such that sorting will group 'Wildcards' first, 'AnyOne's second, and 'Single's third.
instance Ord o => Ord (GlobUnit o) where
  compare a b = case a of
    Wildcard a a1 -> case b of
      Wildcard b b1 -> compare a b <> compare a1 b1
      _          -> LT
    AnyOne   a a1 -> case b of
      Wildcard{} -> GT
      AnyOne   b b1 -> compare a b <> compare a1 b1
      Single{}   -> LT
    Single   a -> case b of
      Single   b -> compare a b
      _          -> GT

instance Functor GlobUnit where
  fmap f o = case o of
    Single   o   -> Single (f o)
    Wildcard n t -> Wildcard n t
    AnyOne   n t -> AnyOne   n t

isSingle :: GlobUnit o -> Bool
isSingle o = case o of { Single _ -> True; _ -> False }

isVariable :: GlobUnit o -> Bool
isVariable = not . isSingle

-- not for export -- strips the leadnig and trailing quote @'"'@ characters.
toStringWithoutQuotes :: String -> String
toStringWithoutQuotes cx = loop $ case cx of { '"':cx -> cx ; cx -> cx ; } where
  loop cx = case cx of { '"':"" -> ""; "" -> ""; c:cx -> c : loop cx; }

-- | Use this function to instantiate your version of 'GlobUnit' into the 'Prelude.Show' class. This
-- function assumes your data type is a string-like type where evaluating 'Prelude.show' on your
-- type produces a string of characters with a leading and trailing quote @'"'@ character.
showGlobUnitOfStrings :: (g -> String) -> GlobUnit g -> String
showGlobUnitOfStrings gshow g = let printyp = maybe "" (\n -> "::"++uchars n) in case g of
  Wildcard nm t -> '$':uchars (toUStr nm)++printyp t
  AnyOne   nm t -> '$':uchars (toUStr nm)++printyp t++"?"
  Single   g    -> toStringWithoutQuotes (gshow g)

instance Show (GlobUnit UStr)   where { show = showGlobUnitOfStrings uchars }
instance Show (GlobUnit String) where { show = showGlobUnitOfStrings id }

-- | Use this function to instantiate your version of 'Glob' into the 'Prelude.Show' class. The
-- function you pass to convert the 'Single' type to a string is passed to 'showGlobUnitOfStrings'.
showGlobUnitList :: (g -> String) -> [GlobUnit g] -> String
showGlobUnitList gshow gx = show $ concatMap (showGlobUnitOfStrings gshow) gx

instance Read (GlobUnit String) where
  readsPrec _prec str = let init c = c=='_' || isAlpha c in case str of
    '$':c:str | init c -> do
      (cx, str) <- [span isAlphaNum str]
      (typfunc, str) <- case str of
        ':':':':str -> return $ head $ concat $
          [ case str of
              c:str | init c -> do
                (cx, str) <- [span isAlphaNum str]
                [(Just $ ustr $ c:cx, str)]
              _ -> []
          , [(Nothing, str)]
          ]
        str         -> [(Nothing, str)]
      case str of
        '?':str -> [(AnyOne   (ustr $ c:cx) typfunc, str)]
        _       -> [(Wildcard (ustr $ c:cx) typfunc, str)]
    '$':str -> [span (/='$') str] >>= \ (cx, str) -> [(Single ('$':cx), str)]
    _       -> []

instance Read (GlobUnit UStr) where
  readsPrec prec str = readsPrec prec str >>= \ (g, str) -> return (fmap ustr g, str)

instance UStrType (GlobUnit UStr) where
  maybeFromUStr str = case readsPrec 0 (uchars str) of { [(o, "")] -> Just o; _ -> Nothing; }
  toUStr = ustr . show

instance NFData o => NFData (GlobUnit o) where
  rnf (Wildcard a b) = deepseq a $! deepseq b ()
  rnf (AnyOne   a b) = deepseq a $! deepseq b ()
  rnf (Single   a  ) = deepseq a ()

instance HasRandGen o => HasRandGen (GlobUnit o) where
  randO = countNode $ runRandChoice
  randChoice = randChoiceList $
    [ Single <$> randO
    , return Wildcard <*> randO <*> randO
    , return AnyOne   <*> randO <*> randO
    ]

----------------------------------------------------------------------------------------------------

-- | Patterns are lists of 'Data.Maybe.Maybe' elements, where constant strings are given by
-- 'Data.Maybe.Just' or wildcards given by 'Data.Maybe.Nothing'. Wildcards runMatchPattern zero or more other
-- Tokens when used as a 'Glob'.
data Glob o = Glob { getPatUnits :: [GlobUnit o], getGlobLength :: Int }
  deriving (Eq, Ord, Typeable)

makeGlob :: [GlobUnit o] -> Glob o
makeGlob ox = Glob{ getPatUnits=ox, getGlobLength=length ox }

instance Functor Glob where
  fmap f g = g{ getPatUnits = fmap (fmap f) (getPatUnits g) }

instance Show (Glob UStr)   where { show = showGlobUnitList uchars . getPatUnits }
instance Show (Glob String) where { show = showGlobUnitList id     . getPatUnits }

instance Read (Glob String) where
  readsPrec prec str = if null str then return mempty else do
    (units, str) <- loop [] str
    return (Glob{ getPatUnits=units, getGlobLength=length units }, str)
    where
      loop units str = case break (=='$') str of
        ("", "" ) -> return (units, "")
        ("", str) -> readsPrec prec str >>= \ (unit, str) -> loop (units++[unit]) str
        (cx, str) -> loop (units++[Single cx]) str

instance Read (Glob UStr) where
  readsPrec prec str = readsPrec prec str >>= \ (g, str) -> return (fmap ustr g, str)

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

----------------------------------------------------------------------------------------------------

-- | 'PatternTree's contain many patterns in an tree structure, which is more efficient when you
-- have many patterns that start with similar sequences of 'GlobUnit's.
type PatternTree g o = T.Tree (GlobUnit g) o

-- | When a 'Glob' is constructed with a function of the 'Prelude.Read' class, the 'Single' items
-- produced are all contiguous characters in between 'Wildcard' and 'AnyOne' markers. For example
-- the string:
-- > read "$X will do $Y? too" :: 'Glob' 'Prelude.String'
-- will parse to a 'Glob' where the 'getPatUnits' is the following list of items:
-- > ['Wildcard' "X", 'Single' " will do ", 'AnyOne' "Y", 'Single' " too"]
-- Notice how the 'Single' items contain spaces. This may or may not be desirable.
--
-- In the case that you would like to further parse the 'Single' strings, you can use the
-- 'parseOverSingles' function, breaking a 'Single' down into a list of 'Single's.
parseOverSinglesM :: Monad m => Glob g -> (g -> m [h]) -> m (Glob h)
parseOverSinglesM g convert =
  forM (getPatUnits g)
    (\u -> case u of
        Single   u    -> convert u >>= mapM (return . Single)
        AnyOne   nm t -> return [AnyOne   nm t]
        Wildcard nm t -> return [Wildcard nm t]
    ) >>= return . makeGlob . concat

-- | Like 'parseOverSinglesM' but is a pure function.
parseOverSingles :: Glob g -> (g -> [h]) -> Glob h
parseOverSingles g = runIdentity . parseOverSinglesM g . (return.)

-- | Insert an item at multiple points in the 'PatternTree'
insertMultiPattern :: (Eq g, Ord g) => (o -> o -> o) -> [Glob g] -> o -> PatternTree g o -> PatternTree g o
insertMultiPattern plus pats o tree =
  foldl (\tree pat -> T.update (getPatUnits pat) (maybe (Just o) (Just . flip plus o)) tree) tree pats

-- | By converting an ordinary 'Glob' to a pattern tree, you are able to use all of the methods
-- in the "Dao.Tree" module to modify the patterns in it.
globTree :: (Eq g, Ord g) => Glob g -> o -> PatternTree g o
globTree pat a = T.insert (getPatUnits pat) a T.Void

-- | Calls 'matchTree' with the 'PatternTree' stored within the given 'Glob' object, and returns
-- only the matching results.
matchPattern :: (Eq g, Ord g) => Bool -> Glob g -> [g] -> [M.Map Name (Maybe Name, [g])]
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
--
-- The 'Data.Map.Map' objects returned map which variable names matched to pairs containing in the
-- 'Prelude.fst' slot the type of the token that the variable expects (the type is the part of the
-- pattern variable after the "::" symbol), and in the 'Prelude.snd' slot contains the tokens that
-- matched in that variable position.
matchTree :: (Eq g, Ord g) => Bool -> PatternTree g o -> [g] -> [(Glob g, M.Map Name (Maybe Name, [g]), o)]
matchTree greedy tree tokx = loop M.empty 0 [] tree tokx where
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
  -- partStep takes a list of tokens, like [a,b,c] and returns a list for every possible
  -- 2-way partition: [([],[a,b,c]), ([a],[b,c]), ([a,b],[c]), ([a,b,c],[])]
  -- This forms a list of (bind, tokx) pairs where 'bind' will be assigned to a variable and 'tokx'
  -- is the remaining tokens to be matched. So when a 'Wildcard' variable is matched, it tries every
  -- possible ('bind', 'tokx') pair, binding the 'bind' to a variable and looping on 'tokx'.
  branch vars p path branch tokx = case tokx of
    []       -> []
    tok:tokx -> let next pat vars tree = loop vars (p+1) (path++[pat]) tree in msum $
      [ do  tree <- maybe [] (:[]) $ M.lookup (Single tok) branch
            next (Single tok) vars tree tokx
      , do -- Next we use 'takeWhile' because of how the 'Ord' instance of 'GlobUnit' is defined,
           -- 'Wildcard's and 'AnyOne's are always first in the list of 'assocs'.
           (pat, tree) <- takeWhile (isVariable . fst) (M.assocs branch)
           let defVar nm t mkAssoc = case M.lookup nm vars of
                 Just (_, pfx) -> maybe [] (:[]) (stripPrefix pfx (tok:tokx)) >>= next pat vars tree
                 Nothing       -> do
                   (bind, tokx) <- mkAssoc
                   next pat (M.insert nm (t, bind) vars) tree tokx
           case pat of
             Wildcard nm t -> defVar nm t (partStep [] (tok:tokx))
             AnyOne   nm t -> defVar nm t [([tok], tokx)]
             Single   _    -> error "undefined behavior in Dao.Glob.matchTree:branch: case Single"
             -- 'Single' cases must not occur, they should have been filtered out by the code:
             -- > takeWhile (isVariable . fst)
      ]

