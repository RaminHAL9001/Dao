-- "Dao/Grammar.hs"  a DSL for constructing CFGs
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

-- | "Dao.Grammar' defines a domain-specific language for defining "context-free Grammars" (CFGs).
--
-- This module provides three key data types, 'Regex' and 'Grammar', and the combinators necessary
-- to define CFGs. These data types are actually not parsers, but they can be converted to parsers
-- using 'regexToParser' or 'grammarToParser'.
-- 
-- The 'Regex' is a simple data type that matches strings. The 'Grammar' data type uses 'Regex's to
-- build CFGs.
--
-- Once a 'Grammar' has been defined, the 'Grammar' can be converted to parser functions. The
-- parsing functions must take input text in the form of a "Data.Text.Lazy" 'Data.Text.Lazy.Text'
-- data type. 
--
-- The 'Grammar' data type can be constructed freely, but impossible 'Grammar's, which are
-- 'Grammar's which could never possibly match any string, and ambiguous 'Grammar's, which are
-- 'Grammar's where a single input could match more than one 'Grammar' in a list of concurrent
-- 'Grammar' choices, evaluate to an 'InvalidGrammar' data type. This allows you to build your
-- 'Grammar's without worrying about the implementation details of the actual parser. Parsers
-- constructed by valid 'Grammar's should always work as intended.
--
-- The 'Grammar' and 'Regex' can convert to any monadic parser by making use of the functions
-- provided in the 'Dao.Text.Parser.Class.MonadParser' type class defined in the
-- "Dao.Text.Parser.Class" module. Any parser instantiating this 'Dao.Text.Parser.Class.MonadParser'
-- type class, including the Haskell Platform parsers 'Text.ParserCombinator.ReadP' and
-- 'Text.ParserCombinator.ReadPrec', and the Dao library's own 'Dao.Text.Parser.Parser', instantiate
-- the 'Dao.Text.Parser.Class.MonadParser' class and can be used as the target monadic parser to
-- which 'Grammar's can be converted.
--
-- The "Test.QuickCheck.Arbitrary" module is imported, and a 'Grammar' can be used to generate
-- 'Test.QuickCheck.Arbitrary.arbitrary' input strings that your 'Grammar' can parse.
--
-- There is one major design limitation: 'Grammar's cannot be checked at compile time. An
-- 'InvalidGrammar' is thrown as an 'Control.Exception.Exception' during evaluation of the parser as
-- soon as it is discovered, so "Test.QuickCheck" testing is still necessary to try and discover
-- problem areas in your 'Grammar'. Fortunately, the 'InvalidGrammar' exceptions can be helpful at
-- tracking down invalid grammars and offering suggestions on how to correct them.
module Dao.Grammar
  ( -- * Applying Strings Matched by Regex
    Lexer(Lexer), lexer, lexConst, lexerSpan, lexerBreak, lexerMatch,
    lexerTuple, lexerFunction, lexerRegex,
    -- * Combinators for Building 'Grammar's.
    Grammar, typeOfGrammar, grammarTyped, grammar, lexerTable, anyStringIn, grammarTable,
    elseFail, (<?>), doReadsPrec, grammarBranches,
    -- * Analyzing Grammars
    GrammarView(
      GrammarEmpty, GrammarReturn, GrammarLift, GrammarTable, GrammarRegex,
      GrammarChoice, GrammarTyped, GrammarFail, GrammarRejected
    ),
    grammarView, grammarChoices,
    -- * Converting 'Grammar's to Monadic Parsers ('MonadParser's)
    grammarToParser,
    -- * Working with Source Locations
    backtrack, getPoint, setPoint, location, newline, movePoint, moveCursor,
    -- * Re-Exported Modules
    module Dao.Text.CharSet,
    module Dao.Text.Regex,
    module Dao.Text.Location
  )
  where

import           Dao.Array
import           Dao.Lens
import           Dao.Predicate
import           Dao.TestNull
import           Dao.Text
import           Dao.Text.CharSet
import           Dao.Text.Location
import           Dao.Text.PPrint
import           Dao.Text.Regex

import           Control.Arrow
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans

import           Data.List (nub, sortBy)
import           Data.Monoid
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text      as Strict
import           Data.Typeable

----------------------------------------------------------------------------------------------------

-- | A 'Lexer' is the smallest unit of a 'Grammar'. It takes a 'Regex' and combines it with a
-- function that transforms a 'Dao.Text.LazyText' string to a value.  You can by 'Lexer's into
-- 'Grammar's using the 'grammar' combinator. 'Lexer' instantiates 'Prelude.Functor' and
-- 'Control.Applicative.Applicative'. Be careful not to create infinitely recursive 'Lexer's, for
-- example lexers containing an infinitely 'Prelude.repeat'ing list of 'Dao.Regex.Regex's.
newtype Lexer o = Lexer (LazyText -> o, [Regex])

instance Functor Lexer where { fmap f (Lexer (o, ox)) = Lexer (fmap f o, ox); }

instance Applicative Lexer where
  pure o = Lexer (const o, [])
  (Lexer (f, rxA)) <*> (Lexer (x, rxB)) = Lexer (\str -> f str $ x str, rxA++rxB)

-- | Check to make sure this lexer is valid, that is, make sure 'Dao.Regex.Regex's contained within
-- it shadow each other when checked with 'Dao.Regex.shadowsSeries'. The 'Lexer' returned is an
-- updated 'Lexer' containing the many 'Dao.Regex.Regex's unified into a single 'Dao.Regex.Regex'.
checkLexer :: Lexer o -> Predicate InvalidGrammar (Lexer o)
checkLexer (Lexer (f, rx)) = Lexer . (,) f . return <$> mconcat (return <$> rx)

-- | An 'Prelude.curry'-ed version of the 'Lexer' constructor.
lexer :: (LazyText -> o) -> [Regex] -> Lexer o
lexer = curry Lexer

-- | A shortcut for @'lexer' ('Prelude.const' c) [...]@, creates a 'Lexer' that ignores the text
-- matched by the 'Dao.Regex.Regex' predicate and always returns the value given.
lexConst :: o -> [Regex] -> Lexer o
lexConst = lexer . const

lexerTuple :: Monad m => Lens m (Lexer o) (LazyText -> o, [Regex])
lexerTuple = newLens (\ (Lexer o) -> o) (\o _ -> Lexer o)

lexerFunction :: Monad m => Lens m (Lexer o) (LazyText -> o)
lexerFunction = lexerTuple >>> tuple0

lexerRegex :: Monad m => Lens m (Lexer o) [Regex]
lexerRegex = lexerTuple >>> tuple1

-- | Like 'Dao.Regex.regexSpan', but for 'Lexer's. So it is also like 'Prelude.span' but uses a the
-- 'Regex' within the 'Lexer' to match from the start of the string.
lexerSpan :: Lexer o -> LazyText -> Predicate InvalidGrammar (o, LazyText)
lexerSpan o str = checkLexer o >>= \ (Lexer (f, rx)) -> guard (not $ null rx) >>
  maybe mzero (return . first (f . Lazy.fromStrict)) (regexSpan (head rx) str)

-- | Like 'regexBreak' but for 'Lexer's. Uses 'regexSpan' and a given 'Regex' to break a string into
-- portions that match wrapped in a 'Prelude.Right' constructor, and portions that do not match
-- wrapped into a 'Prelude.Left' constructor. Then the 'lexerFunction' of the 'Lexer' is applied to
-- each 'Prelude.Right' portion.
lexerBreak :: Lexer o -> LazyText -> Predicate InvalidGrammar [Either StrictText o]
lexerBreak o str = checkLexer o >>= \ (Lexer (f, rx)) -> guard (not $ null rx) >>
  return (fmap (f . Lazy.fromStrict) <$> regexBreak (head rx) str)

-- | Like 'regexMatch' but for 'Lexer's. Uses 'lexerSpan' to check whether a given 'Lexer' matches
-- the entire input string or not. Evaluates to 'Dao.Predicate.PFalse' if the whole string is not
-- matched.
lexerMatch :: Lexer o -> LazyText -> Predicate InvalidGrammar o
lexerMatch o str = lexerSpan o str >>= \ (o, str) -> guard (not $ Lazy.null str) >> return o

----------------------------------------------------------------------------------------------------

-- | 'Grammar' is a data structure that can be converted to a monadic parser function of any type
-- that instantiates 'MonadParser', including 'Text.ParserCombinators.ReadP' and
-- 'Dao.Grammar.Text.TextParse'. To do branching 'Grammar' instantiates 'Control.Monad.MonadPlus'
-- and 'Control.Applicative.Alternative'.
data Grammar m o
  = GrEmpty
  | GrReturn o
  | GrLift (m (Grammar m o))
  | GrTable (RegexTable (LazyText -> Grammar m o))
  | GrRegex Regex (LazyText -> Grammar m o)
  | GrChoice (Grammar m o) (Grammar m o)
  | GrTyped TypeRep (Grammar m o)
  | GrFail StrictText
  | GrReject InvalidGrammar
  deriving Typeable

instance MonadFix m => MonadFix (Grammar m) where { mfix f = GrLift $ mfix $ return . (>>= f); }

instance Functor m => Functor (Grammar m) where
  fmap f o = case o of
    GrEmpty            -> GrEmpty
    GrReturn  o        -> GrReturn $ f o
    GrLift    next     -> GrLift $ fmap (fmap f) next
    GrTable   next     -> GrTable $ fmap (fmap (fmap f)) next
    GrRegex   rx  next -> GrRegex rx $ fmap (fmap f) next
    GrChoice  next alt -> GrChoice (fmap f next) (fmap f alt)
    GrTyped   typ next -> GrTyped typ $ fmap f next
    GrFail    err      -> GrFail err
    GrReject  err      -> GrReject err

instance Monad m => Monad (Grammar m) where
  return = GrReturn
  o >>= f = case o of
    GrEmpty            -> GrEmpty
    GrReturn  o        -> f o
    GrLift    next     -> GrLift $ liftM (>>= f) next
    GrTable   next     -> GrTable $ fmap (fmap (>>= f)) next
    GrRegex   rx  next -> GrRegex rx $ fmap (>>= f) next
    GrChoice  next alt -> GrChoice (next >>= f) (alt >>= f)
    GrTyped   typ next -> GrTyped typ $ next >>= f
    GrFail    err      -> GrFail err
    GrReject  err      -> GrReject err
  fail = GrFail . Strict.pack

_cutBranches :: Array Regex -> Grammar m o
_cutBranches = GrReject . InvalidGrammar . (,,) Nothing nullValue . CutSomeBranches

_grPredicate :: Monad m => Predicate InvalidGrammar o -> Grammar m o
_grPredicate o = case o of
  PTrue  o -> GrReturn o
  PFalse   -> GrEmpty
  PError e -> GrReject e

instance Monad m => MonadPlus (Grammar m) where
  mzero = GrEmpty
  mplus a b = case a of
    GrEmpty          -> b
    GrReturn  a      -> case b of
      GrEmpty          -> GrReturn a
      GrTable   tabB   -> _cutBranches $ fst <$> regexTableElements tabB
      GrRegex   rx   _ -> _cutBranches $ array [rx]
      GrReject  err    -> GrReject err
      _                -> GrReturn a
    GrLift    a      -> case b of
      GrEmpty          -> GrLift a
      GrReject  err    -> GrReject err
      _                -> GrLift $ liftM2 mplus a (return b)
    GrTable   tabA   -> case b of
      GrEmpty          -> GrTable tabA
      GrLift    next   -> GrLift $ liftM (mplus $ GrTable tabA) next
      GrTable   tabB   -> case PTrue (fmap Alt <$> tabA) <> PTrue (fmap Alt <$> tabB) of
        PTrue  o -> GrTable $ fmap getAlt <$> o
        PFalse   -> GrEmpty
        PError e -> GrReject e
      GrChoice  b    c -> mplus (mplus (GrTable tabA) b) c
      GrTyped   typ  b -> GrTyped typ $ mplus (GrTable tabA) b
      GrReject  err    -> GrReject err
      b                -> GrChoice (GrTable tabA) b
    GrRegex rxA a    -> case b of
      GrEmpty          -> GrRegex rxA a
      GrChoice  b    c -> mplus (mplus (GrRegex rxA a) b) c
      GrTyped   typ  b -> GrTyped typ $ mplus (GrRegex rxA a) b
      GrReject  err    -> GrReject err
      b                -> GrChoice (GrRegex rxA a) b
    GrChoice  a   a' -> GrChoice a $ mplus a' b
    GrTyped   typ a  ->
      let loop stk b = case b of
            GrTyped t b -> if elem t stk then GrTyped typ a else loop (t:stk) b
            _           -> GrTyped typ $ mplus a b
      in  loop [typ] b
    GrFail    err    -> case b of
      GrTable   tabB   -> _cutBranches $ fst <$> regexTableElements tabB
      GrRegex   rxB  _ -> _cutBranches $ array [rxB]
      _                -> GrFail err
    GrReject  err    -> GrReject err
                             
instance (Applicative m, Monad m) => Applicative (Grammar m) where { pure = return; (<*>) = ap; }

instance (Alternative m, MonadPlus m) => Alternative (Grammar m) where { empty = mzero; (<|>) = mplus; }

instance MonadTrans Grammar where { lift o = GrLift $ liftM GrReturn o; }

instance MonadIO m => MonadIO (Grammar m) where { liftIO = GrLift . liftM GrReturn . liftIO; }

-- Used for debugging. Try to just leave it here, it should be optimized away in the
-- dead-code-removal phase.
_sho :: Int -> Grammar m o -> String
_sho i o = if i<=0 then "..." else case o of
  GrEmpty      -> "GrEmpty"
  GrReturn   _ -> "GrReturn"
  GrLift     _ -> "GrLift"
  GrTable    t -> "GrTable ("++show (const() <$> t)++")"
  GrRegex rx _ -> "GrRegex ("++show rx++")"
  GrChoice a b -> "GrChoice ("++_sho (i-1) a++") ("++_sho (i-1) b++")"
  GrTyped   t _ -> "GrTyped "++show t
  GrFail   msg -> "GrFail "++show msg
  GrReject err -> "GrReject "++show err

-- | Convert a 'Grammar' to a 'MonadParser' that actually parses things.
grammarToParser :: (Monad m, MonadPlus m, MonadParser m) => Grammar m o -> m o
grammarToParser = loop where
  loop o = case o of
    GrEmpty            -> mzero
    GrReturn  o        -> return o
    GrLift    next     -> next >>= loop
    GrTable   next     -> regexTableToParser next >>= loop . (uncurry $ flip ($))
    GrRegex   rx   f   -> regexToParser rx >>= loop . f
    GrChoice  next alt -> mplus (loop next) (loop alt)
    GrTyped   _   next -> loop next
    GrFail    msg      -> fail (Strict.unpack msg)
    GrReject  err      -> throw err

-- | Return the type to which this 'Grammar' will evaluate.
typeOfGrammar :: forall m t . Typeable t => Grammar m t -> TypeRep
typeOfGrammar = typ undefined where
  typ :: t -> Grammar m t -> TypeRep
  typ ~t _ = typeOf t

-- | Construct a 'Grammar' from a list of 'Lexer' choices. This 'Grammar' will try to match each
-- 'Lexer' in turn. If one 'Lexer' backtracks, the next is tried. This can be a source of
-- inefficiency. You may want to consider using 'lexerTable' instead.
grammar :: Monad m => [Lexer o] -> Grammar m o
grammar ox = case make id Nothing ox of
  GrChoice a GrEmpty -> a
  GrChoice GrEmpty a -> a
  a                  -> a
  where
    make g prev ox = case ox of
      []                 -> g GrEmpty
      (Lexer (f, rx)):ox -> do
        rx <- _grPredicate $ mconcat $ PTrue <$> rx
        let next = make (g . GrChoice (GrRegex rx $ return . f)) (Just rx) ox
        case prev of
          Just ry | shadowsSeries ry rx -> GrReject $ InvalidGrammar $
            (Nothing, nullValue, RegexShadowError ParallelRegex ry rx)
          _                             -> next

-- | Like the 'grammar' constructor, but modifies a 'Grammar' so that the 'Data.Typeable.TypeRep'
-- for the return type stored in the 'Grammar's meta-data.
grammarTyped :: Typeable o => Grammar m o -> Grammar m o
grammarTyped o = GrTyped (typeOfGrammar o) o

-- | Similar to the 'grammar' constructor, but will create a 'Dao.Text.Regex.RegexTable' that is
-- used internally to improve the efficiency of parser constructed by the given 'Grammar' node.
-- 'Grammar's constructed with 'lexerTable' has all 'Dao.Text.Regex.Regex's merged into an internal
-- 'Dao.Text.Regex.RegexTable', whereas 'Grammar's constructed 'grammar' or 'Control.Monad.mplus' or
-- @('Control.Applicative.<|>')@ are not merged.  This function allows 'Grammar's constructed with
-- 'grammar', 'Control.Monad.mplus', and @('Control.Applicative.<|>')@ to be merged into an internal
-- 'Dao.Text.Regex.RegexTable'.
--
-- This function may get stuck in an infinite loop if you accidentally construct a 'Grammar' that is
-- biequivalent to @let f = f 'Control.Applicative.<|>' f@
grammarTable :: Monad m => Grammar m o -> Grammar m o
grammarTable o = gather (grammarChoices o) >>= merge mempty [] where
  gather o = case o of
    Left err -> GrReject err
    Right ox -> GrReturn ox
  make = regexsToTable . fmap (fmap (fmap Alt))
  merge tab rxs ox = case ox of
    GrammarRegex rx o : ox -> merge tab (rxs++[(rx, o)]) ox
    GrammarTable    o : ox -> case tab <> make rxs <> PTrue (fmap Alt <$> o) of
      PFalse     -> merge        tab  [] ox
      PTrue  tab -> merge (PTrue tab) [] ox
      PError err -> GrReject err
    [] -> case tab <> make rxs of
      PFalse     -> GrEmpty
      PTrue  tab -> GrTable $ fmap getAlt <$> tab
      PError err -> GrReject err
    ox -> mplus (merge tab rxs []) (merge mempty [] ox)

-- | Construct a 'Grammar' from a list of 'Lexer' choices, and use 'Dao.Regex.regexsToTable' to
-- construct a lexer table.
--
-- A lexer table sacrifices memory usage for speed. It creates an
-- @('Data.Array.IArray.Array' 'Prelude.Char')@ (an array with 'Prelude.Char' indicies) such that a
-- single character look-ahead ('Dao.Text.Regex.look1') can be used to select the appropriate
-- 'Dao.Text.Regex.Regex' to use from the array.
--
-- However it is possible to create arrays that 'Dao.Interval.envelop' the entire range of
-- characters from @'Prelude.minBound'::'Prelude.Char'@ to @'Prelude.maxBound'::'Prelude.Char'@.
-- Please refer to the documentation for the 'Dao.Text.Regex.regexsToTable' function to learn more.
--
-- Also, the given list of choices must be finite, or the process of converting to a 'MonadParser'
-- will loop infinitely. When this 'Grammar' is converted to a 'MonadParser', each 'Regex' will be
-- tried in turn. If any 'Regex' occuring earlier in the list 'Dao.Text.Regex.shadowParallel's any
-- 'Dao.Text.Regex' occuring later in the list (which is checked by 'shadowsParallel'), then this
-- 'Grammar' evaluates to an 'InvalidGramar'. Of course, passing an empty list will create a
-- 'Grammar' equivalent to 'Control.Applicative.empty' which alwasy backtracaks.
lexerTable :: Monad m => [Lexer o] -> Grammar m o
lexerTable ox = case concatenated ox of
  PFalse     -> GrEmpty
  PError err -> GrReject err
  PTrue  tab -> GrTable $ fmap getAlt <$> tab
  where
    concatenated = regexsToTable <=<
      mapM (\ (Lexer (f, ox)) -> flip (,) (Alt . GrReturn . f) <$> mconcat (PTrue <$> ox))

-- | Create a 'Grammar' that matches any one of the given strings. The list of given strings is
-- automatically sorted to make sure shorter strings do not shaddow longer strings.
anyStringIn :: (Monad m, ToText t) => (LazyText -> o) -> [t] -> Grammar m o
anyStringIn f = lexerTable . fmap Lexer . zip (repeat f) . fmap (return . rx) .
  sortBy (\a b -> compare (Strict.length b) $ Strict.length a) . nub . fmap toText

-- | Retrieve every 'Regex' for all branches of the 'Grammar'.
grammarBranches :: Grammar m o -> [Regex]
grammarBranches o = case o of
    GrTable next -> fmap fst (elems $ regexTableElements next)
    GrRegex rx _ -> [rx]
    GrChoice a b -> grammarBranches a ++ grammarBranches b
    _            -> []

-- | When converting this 'Grammar' to a monadic parser, this will convert to the monadic parser's
-- 'Control.Monad.fail' function, that is, it 'Control.Monad.Trans.lift's the 'Control.Monad.fail'
-- function into the 'Grammar'.
--
-- This is different from @('Control.Monad.fail'::'Grammar' m e)@ because
-- @('Control.Monad.fail'::'Grammar' m e)@ represents an invalid 'Grammar' that cannot be converted
-- to a parser at all. The 'elseFail' function is actually a valid 'Grammar' that *can* be converted
-- to a parser: it converts to the parser function that rejects the input string with an error
-- message.
--
-- The name of the function 'elseFail' was chosen to make it more literate when using it as an infix
-- function. For example, if you have a grammar describing an 'integer' where an integer is
-- required, and if there is no integer the input string is invalid, this could be written like so:
--
-- > 'integer' `elseFail` "integer value is required here"
--
elseFail :: (Monad m, MonadPlus m) => Grammar m o -> String -> Grammar m o
elseFail p = mplus p . GrFail . Strict.pack

-- | Inspired by the @Parsec@ package, this is the infix operator of 'elseFail'. In Haskell code, it
-- has a precedence of zero.
(<?>) :: (Monad m, MonadPlus m) => Grammar m o -> String -> Grammar m o
(<?>) = elseFail
infix 0 <?>

-- | A 'Dao.Grammar.Grammar' that takes the result of some other 'Dao.Grammar.Grammar' and uses the
-- instantiation of the 'Prelude.readsPrec' function in the 'Prelude.Read' class to generate a data
-- type from a 'Dao.Text.LazyText' value. Pass an integer precedence value as the first parameter.
-- This parser only succeeds if the whole 'LazyText' is consumed by the 'Prelude.readsPrec'
-- function.
doReadsPrec :: (Monad m, MonadPlus m, Read o) => Int -> LazyText -> Grammar m o
doReadsPrec prec t = case readsPrec prec $ Lazy.unpack t of
  [(o, "")] -> return o
  _         -> mzero

----------------------------------------------------------------------------------------------------

-- | This data type is used to step through a 'Grammar', viewing each branch.
-- are either 'Dao.Text.Regex.Regex's or 'Dao.Text.Regex.RegexTable's.
data GrammarView m o
  = GrammarEmpty
  | GrammarReturn                 o
  | GrammarLift     (m (Grammar m o))
  | GrammarTable    (RegexTable (LazyText -> Grammar m o))
  | GrammarRegex     Regex      (LazyText -> Grammar m o)
  | GrammarChoice      (Grammar m o)        (Grammar m o)
  | GrammarTyped     TypeRep                (Grammar m o)
  | GrammarFail     StrictText
  | GrammarRejected InvalidGrammar
  deriving Typeable

instance Monad m => Functor (GrammarView m) where
  fmap f o = case o of
    GrammarEmpty      -> GrammarEmpty
    GrammarReturn   o -> GrammarReturn     $ f o
    GrammarLift     o -> GrammarLift       $ liftM (liftM f) o
    GrammarTable    o -> GrammarTable      $ fmap (fmap (liftM f)) o
    GrammarRegex  r o -> GrammarRegex    r $ fmap (liftM f) o
    GrammarChoice a b -> GrammarChoice       (liftM f a) (liftM f b)
    GrammarTyped  t o -> GrammarTyped    t $ liftM f o
    GrammarFail     o -> GrammarFail     o
    GrammarRejected o -> GrammarRejected o

instance PPrintable o => PPrintable (GrammarView m o) where
  pPrint o = case o of
    GrammarEmpty         -> [pText "empty"]
    GrammarReturn    o   -> [pText "return (", pIndent (pPrint o), pChar ')']
    GrammarLift      _   -> [pText "lift ..."]
    GrammarTable     o   -> [pText "lexerTable (", pIndent (pPrint (void o)), pChar ')']
    GrammarRegex     o _ -> [pText "grammar (", pIndent (pPrint o), pChar ')']
    GrammarChoice    a b -> do
      let p o = [pNewLine, pIndent $ pChar '(' : pPrint (grammarView o) ++ [pChar ')']]
      [pText "mplus", pIndent $ p a ++ p b]
    GrammarTyped    t o ->
      [ pText "grammarTyped", pSpace, pShow t , pChar '('
      , pIndent $ pPrint $ grammarView o
      , pChar ')'
      ]
    GrammarFail     t   -> [pText "fail", pSpace, pShow t]
    GrammarRejected err -> pPrint err

instance PPrintable o => Show (GrammarView m o) where { show = showPPrint 4 80 . pPrint; }

grammarView :: Grammar m o -> GrammarView m o
grammarView o = case o of
  GrEmpty      -> GrammarEmpty
  GrReturn   o -> GrammarReturn   o
  GrLift     o -> GrammarLift     o
  GrTable    o -> GrammarTable    o
  GrRegex rx o -> GrammarRegex rx o
  GrChoice a b -> GrammarChoice a b
  GrTyped  t o -> GrammarTyped  t o
  GrFail   e   -> GrammarFail     e
  GrReject e   -> GrammarRejected e

-- | This function analyzes a 'Grammar' and produces a list of 'GrammarView' values consisting of
-- all 'Dao.Text.Regex.Regex' and 'Dao.Text.Regex.RegexTable' branches in this immediate grammar
-- node, if any.
--
-- The resulting list may be empty if the 'Grammar' is constructed with one of
-- 'Control.Monad.return', 'Control.Applicative.pure', 'Control.Monad.mzero', 'Control.Monad.fail',
-- 'Control.Applicative.empty', 'Control.Monad.Except.throwError', or 'Control.Monad.Trans.lift'.
--
-- This function may loop infinitely returning no results if the 'Grammar' being analyzed is
-- constructed with an infinite loop of 'grammarTyped's, such as:
--
-- @
-- myGrammar :: 'Control.Monad.Monad' m => 'Grammar' m MyDataType
-- myGrammar = 'grammarTyped' myGrammar
-- @
--
-- The resulting list may be infinitely large if a 'Grammar' is constructed by equations such as:
-- 
-- @
-- iniChoices1 = 'Control.Monad.msum' $ 'Prelude.repeat' myGrammar
-- infChoices2 = myGrammar 'Control.Applicative.<|>' infChoices2
-- @
--
-- or some similarly constructed function. Such functions exist both intentionally, and as a result
-- of programmer error. You may want to wrap this function with @'Prelude.take' lim@ for some
-- reasonable limit @lim@ just in case the grammar is accidentally an infinite choice.
grammarChoices :: Grammar m o -> Either InvalidGrammar [GrammarView m o]
grammarChoices o = case o of
  GrReject err -> Left err
  GrRegex rx o -> Right [GrammarRegex rx o]
  GrTable  tab -> Right [GrammarTable tab]
  GrChoice a b -> (++) <$> grammarChoices a <*> grammarChoices b
  GrTyped  _ o -> grammarChoices o
  o            -> Right [grammarView o]

----------------------------------------------------------------------------------------------------

-- | Some 'Grammar's made up of chains of other 'Grammar's may match a lot of text before
-- discovering that it actually does not match the input text. For example, the grammar:
--
-- > 'Prelude.fmap' 'Data.Monoid.mconcat' $ 'Control.Monad.sequence' $
-- >     [str "hello", 'Dao.Text.toText' 'Control.Applicative.<$>' 'space', str "world"] where
-- >          str s = 'grammar' ['lexer' 'Dao.Text.toText' ['rxString' s]]
--
-- If this 'Grammar' were converted to a parser, it would match the input string "hello world". But
-- if someone input "hello darkness my old friend", the parser would match "hello", then the space
-- after, and then when the grammar expects the string "world" it will see instead the word
-- "darkness" and evaluate to 'Control.Applicative.empty', without putting back the "hello" or
-- space that preceeded the mismatching text.
--
-- Backtracking 'Grammar's let you put strings back, which translate to *very inefficient* but
-- flexible parsers. The 'backtrack' function will translate to a 'MonadBacktrackingParser' that
-- uses 'unstring' to put back all of the text it parsed up to the point where the 'Grammar' no
-- longer matched the input string.
backtrack :: (MonadBacktrackingParser m,Monad m, MonadPlus m) => LazyText -> Grammar m ()
backtrack = GrLift . liftM GrReturn . unstring

getPoint :: (MonadSourceCodeParser m, Monad m, MonadPlus m) => Grammar m TextPoint
getPoint = GrLift $ liftM GrReturn getTextPoint

setPoint :: (MonadSourceCodeParser m, Monad m, MonadPlus m) => TextPoint -> Grammar m ()
setPoint = GrLift . liftM GrReturn . setTextPoint

location :: (MonadSourceCodeParser m, Monad m, MonadPlus m) => Grammar m o -> Grammar m (Location, o)
location o =
  liftM3 (\start o end -> (Location (Just start, Just end), o)) getPoint o getPoint

-- | When converted to a monadic parser, this will increment the line counter and reset the column
-- counter to 1 if the inner 'Grammar' converts to a successful parser. This is designed to be used
-- with the @'lineBreak' :: 'Grammar'@ like so:
--
-- > newline lineBreak
--
newline :: (MonadSourceCodeParser m, Monad m, MonadPlus m) => Grammar m o -> Grammar m o
newline g = getPoint >>= \pt -> g >>= \o -> setPoint (with pt [lineNumber $= (1 +)]) >> return o

-- | If the given 'Grammar' returns a @text@ value, the given function can be used to update the
-- 'TextPoint' of the current 'MonadSourceCodeParser' monad. The function used to update the 'TextPoint'
-- can implement whatever scheme necessary to count lines and columns, but 'asciiLineCounter' is a
-- good default implementation to be passed as the first parameter to this function.
--
-- Use this function with any 'Grammar' that returns a @text@ value. For example:
--
-- > let cursor = 'movePoint' ('asciiLineCounter' 4 'Dao.Text.Parser.unpack')
-- > cursor 'space'
-- > (,) <$> cursor 'integerLiteral' <*>
-- > cursor 'cIdentifier'
--
-- This will update the cursor using 'asciiLineCounter' with a tab width of 4, and the correct text
-- getPoint will be reflected in the parser after parsing some 'space's, and then an 'integerLiteral' and a
-- 'cIdentifier'.
movePoint
  :: (ToText o, MonadSourceCodeParser m, Monad m, MonadPlus m)
  => (StrictText -> TextPoint -> TextPoint) -> Grammar m o -> Grammar m o
movePoint count g = g >>= \o -> getPoint >>= setPoint . count (toText o) >> return o

-- | This 'Grammar' has no effect on the language you are defining. But when it is converted to a
-- 'MonadSourceParser'. Upon conversion, the resulting 'MonadSourceParser' will update it's own the
-- 'TextPoint' contained within it's own internal state. Pass a line-counting function to convert
-- the value produced by the given 'Grammar' into a 'TextPoint', and this function will use
-- 'getPoint' and 'putPoint' to update current 'TextPoint' in the 'MonadSourceParser'.
--
-- For example, if your 'Grammar' returns a 'LazyText' type, you can use 'asciiLineCounter' to
-- convert it to a 'TextPoint' that will count how many lines of text there are, along with how many
-- columns there are in the final line, and this 'TextPoint' will be used to update the 'TextPoint'
-- internal to the 'MonadSourceParser'.
moveCursor
  :: (MonadSourceCodeParser m, Monad m, MonadPlus m)
  => (t -> TextPoint) -> Grammar m t -> Grammar m t
moveCursor toPoint f = do
  before <- getPoint
  o      <- f
  let after = toPoint o
  setPoint $ with after [lineNumber $= ((before~>lineNumber) +)]
  return o

