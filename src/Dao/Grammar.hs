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
    backtrack, getPoint, setPoint, withLocation, newline, movePoint, moveCursor,
    -- * Essential Tokenizers
    -- $Essential_tokenizers
    SpaceToken(SpaceToken), OptSpaceToken(OptSpaceToken), NewLineToken(NewLineToken),
    CInlineComment(CInlineComment), CEndlineComment(CEndlineComment),
    HaskellInlineComment(HaskellInlineComment), HaskellEndlineComment(HaskellEndlineComment),
    inlineCommentGrammar, endlineCommentGrammar,
    CIdentifier(CIdentifier), IntLitTokenLens(intLitToken),
    PosNeg(PosNeg), prependSign, posNegNum, posNegSign,
    ExponentLitToken(ExponentLitToken),
    exponentTokenLens, exponentTokenCapitalE, exponentTokenPosNeg,
    HexIntLitToken(HexIntLitToken),
    OctIntLitToken(OctIntLitToken),
    DecIntLitToken(DecIntLitToken),
    NumberLitToken(DecIntNumberLit, HexIntNumberLit, IntExponentLit, DecimalPointLit),
    decimalPointedNumberToken, exponentLiteralToken,
    CNumberLit(CNumberLit, COctIntNumberLit), cDecimalPoint, cExponent,
    Quoted1Token(Quoted1Token), Quoted2Token(Quoted2Token),
    -- * Re-Exported Modules
    module Dao.Text.CharSet,
    module Dao.Text.Regex,
    module Dao.Text.Location
  )
  where

import           Dao.Array
import           Dao.Class
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
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans

import           Data.Char
import           Data.List (nub, sortBy)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text      as Strict
import           Data.Typeable

import           Numeric

----------------------------------------------------------------------------------------------------

-- | A 'Lexer' is the smallest unit of a 'Grammar'. It takes a 'Regex' and combines it with a
-- function that transforms a 'Dao.Text.LazyText' string to a value.  You can by 'Lexer's into
-- 'Grammar's using the 'grammar' combinator. 'Lexer' instantiates 'Prelude.Functor' and
-- 'Control.Applicative.Applicative'. Be careful not to create infinitely recursive 'Lexer's, for
-- example lexers containing an infinitely 'Prelude.repeat'ing list of 'Dao.Regex.Regex's.
newtype Lexer o = Lexer ([Regex], LazyText -> o)

instance Functor Lexer where { fmap f (Lexer (rx, o)) = Lexer (rx, fmap f o); }

instance Applicative Lexer where
  pure o = Lexer ([], const o)
  (Lexer (rxA, f)) <*> (Lexer (rxB, x)) = Lexer (rxA++rxB, \str -> f str $ x str)

lexerTuple :: Monad m => Lens m (Lexer o) ([Regex], LazyText -> o)
lexerTuple = newLens (\ (Lexer o) -> o) (\o _ -> Lexer o)

-- | Check to make sure this lexer is valid, that is, make sure 'Dao.Regex.Regex's contained within
-- it shadow each other when checked with 'Dao.Regex.shadowsSeries'. The 'Lexer' returned is an
-- updated 'Lexer' containing the many 'Dao.Regex.Regex's unified into a single 'Dao.Regex.Regex'.
checkLexer :: Lexer o -> Predicate InvalidGrammar (Lexer o)
checkLexer (Lexer (rx, f)) = Lexer . flip (,) f . return <$> mconcat (return <$> rx)

-- | A 'Prelude.flip'-ped, 'Prelude.curry'-ed version of the 'Lexer' constructor. This is useful
-- because often the transformation function is 'Control.Category.id' or 'Prelude.const', whereas
-- the 'Dao.Text.Regex.Regex' is a rather more involved expression. For example:
--
-- @
-- myLexer = lexer 'Data.Text.Lazy.toStrict'
--     ['Dao.Text.Regex.rx' ('Dao.Text.CharSet.allOf' "AEIOUYaeiouy", 'Dao.Text.Regex.MIN' 1), 'Dao.Text.Regex.rx' "+++", ('Dao.Text.Regex.rx' "OK", 'Dao.Text.Regex.MAX' 1)]
-- @
lexer :: (LazyText -> o) -> [Regex] -> Lexer o
lexer = flip (curry Lexer)

-- | A shortcut for @'lexer' ('Prelude.const' c) [...]@, creates a 'Lexer' that ignores the text
-- matched by the 'Dao.Regex.Regex' predicate and always returns the value given.
lexConst :: o -> [Regex] -> Lexer o
lexConst = lexer . const

lexerFunction :: Monad m => Lens m (Lexer o) (LazyText -> o)
lexerFunction = lexerTuple >>> tuple1

lexerRegex :: Monad m => Lens m (Lexer o) [Regex]
lexerRegex = lexerTuple >>> tuple0

-- | Like 'Dao.Regex.regexSpan', but for 'Lexer's. So it is also like 'Prelude.span' but uses a the
-- 'Regex' within the 'Lexer' to match from the start of the string.
lexerSpan :: Lexer o -> LazyText -> Predicate InvalidGrammar (o, LazyText)
lexerSpan o str = checkLexer o >>= \ (Lexer (rx, f)) -> guard (not $ null rx) >>
  maybe mzero (return . first (f . Lazy.fromStrict)) (regexSpan (head rx) str)

-- | Like 'regexBreak' but for 'Lexer's. Uses 'regexSpan' and a given 'Regex' to break a string into
-- portions that match wrapped in a 'Prelude.Right' constructor, and portions that do not match
-- wrapped into a 'Prelude.Left' constructor. Then the 'lexerFunction' of the 'Lexer' is applied to
-- each 'Prelude.Right' portion.
lexerBreak :: Lexer o -> LazyText -> Predicate InvalidGrammar [Either StrictText o]
lexerBreak o str = checkLexer o >>= \ (Lexer (rx, f)) -> guard (not $ null rx) >>
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
    GrTyped   typ a  -> GrTyped typ $ mplus a b
    GrFail    err    -> case b of
      GrTable   tabB   -> _cutBranches $ fst <$> regexTableElements tabB
      GrRegex   rxB  _ -> _cutBranches $ array [rxB]
      _                -> GrFail err
    GrReject  err    -> GrReject err
                             
instance (Applicative m, Monad m) => Applicative (Grammar m) where { pure = return; (<*>) = ap; }

instance (Alternative m, MonadPlus m) => Alternative (Grammar m) where { empty = mzero; (<|>) = mplus; }

instance MonadTrans Grammar where { lift o = GrLift $ liftM GrReturn o; }

instance MonadIO m => MonadIO (Grammar m) where { liftIO = GrLift . liftM GrReturn . liftIO; }

-- | Convert a 'Grammar' to a 'MonadParser' that actually parses things.
grammarToParser :: (Monad m, MonadPlus m, MonadParser m) => Grammar m o -> m o
grammarToParser = loop Nothing where
  loop typ o = case o of
    GrEmpty            -> mzero
    GrReturn  o        -> return o
    GrLift    next     -> next >>= loop typ
    GrTable   next     -> regexTableToParser next >>= loop typ . (uncurry $ flip ($))
    GrRegex   rx   f   -> regexToParser rx >>= loop typ . f
    GrChoice  next alt -> mplus (loop typ next) (loop typ alt)
    GrTyped   typ next -> loop (Just typ) next
    GrFail    msg      -> pfail $ InvalidGrammar (typ, nullValue, OtherFailure msg)
    GrReject  err      -> pfail err

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
      (Lexer (rx, f)):ox -> do
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
      mapM (\ (Lexer (rx, f)) -> flip (,) (Alt . GrReturn . f) <$> mconcat (PTrue <$> rx))

-- | Create a 'Grammar' that matches any one of the given strings. The list of given strings is
-- automatically sorted to make sure shorter strings do not shaddow longer strings.
anyStringIn :: (Monad m, ToText t) => (LazyText -> o) -> [t] -> Grammar m o
anyStringIn f = lexerTable . fmap Lexer . flip zip (repeat f) . fmap (return . rx) .
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
elseFail :: Monad m => Grammar m o -> String -> Grammar m o
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

instance (MonadSourceCodeParser m, Monad m, MonadPlus m) => HasCurrentLocation (Grammar m) where
  currentLocation = liftM Location $ liftM2 (,) (liftM Just getPoint) (liftM Just getPoint)

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

withLocation :: (Monad m, MonadSourceCodeParser m) => Grammar m o -> Grammar m (Location, o)
withLocation f = do
  let optional f = mplus (liftM Just f) (return Nothing)
  start <- optional getPoint
  o     <- f
  end   <- optional getPoint
  return (new [locationStart <~ start, locationEnd <~ end], o)

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

----------------------------------------------------------------------------------------------------

-- $Essential_tokenizers
-- It is common practice to use 'Grammar's to generate tokenizers that produce token streams, and
-- then use 'Dao.Predicate.PredicateState' or 'Dao.Logic.LogicT' to define a actual grammar over the
-- token stream.
--
-- Many tokenizers for many languages have a lot in common, especially the tokenizers for the C
-- family of languages. Provided here are some 'Grammar's that could produce a stream of tokens for
-- some of the most fundamental token types. These include newlines, whitespaces (for tab
-- indentation), integers and floating-point number literals, string literals, and character
-- literals.

-- | This token type is produced when a whitespace that is not a newline is picked up during
-- scanning. Carriage return characters @'\r'@ are considered ordinary whitespace.
newtype SpaceToken = SpaceToken StrictText deriving (Eq, Ord, Typeable)
instance ToText SpaceToken where { toText (SpaceToken o) = o; }
instance StringLength SpaceToken where { stringLength = stringLength . toText; }
instance Show SpaceToken where { show = show . toText; }
instance PPrintable SpaceToken where { pPrint = return . pText . toText; }
instance NFData SpaceToken where { rnf (SpaceToken o) = deepseq o (); }
instance Monad m => The (Grammar m SpaceToken) where
  the = grammarTyped $ grammar [lexer (SpaceToken . Lazy.toStrict) [rx (anyOf "\t\v\f ", MIN 1)]]

-- | This is exactly like 'SpaceToken', except the 'Grammar' never fails to parse. If there are no
-- whitespace characters under the current point in the input strean, an empty string token is
-- returned.
newtype OptSpaceToken = OptSpaceToken StrictText deriving (Eq, Ord, Typeable)
instance ToText OptSpaceToken where { toText (OptSpaceToken o) = o; }
instance StringLength OptSpaceToken where { stringLength = stringLength . toText; }
instance Show OptSpaceToken where { show = show . toText; }
instance PPrintable OptSpaceToken where { pPrint = return . pText . toText; }
instance NFData OptSpaceToken where { rnf (OptSpaceToken o) = deepseq o (); }
instance TestNull OptSpaceToken where
  nullValue = OptSpaceToken nullValue
  testNull (OptSpaceToken o) = testNull o
instance Monad m => The (Grammar m OptSpaceToken) where
  the = grammarTyped $ grammar [lexer (OptSpaceToken . Lazy.toStrict) [rx (anyOf "\t\v\f ", MIN 0)]]

-- | This token type is produced when any newline characters @'\n'@, @'\r'@, @"\n\r"@, or @"\r\n"@
-- are picked up during scanning.
newtype NewLineToken = NewLineToken StrictText deriving (Eq, Ord, Typeable)
instance ToText NewLineToken where { toText (NewLineToken o) = o; }
instance StringLength NewLineToken where { stringLength = stringLength . toText; }
instance Show NewLineToken where { show = show . toText; }
instance PPrintable NewLineToken where { pPrint _ = [pNewLine]; }
instance NFData NewLineToken where { rnf (NewLineToken o) = deepseq o (); }
instance Monad m => The (Grammar m NewLineToken) where
  the = grammarTyped $ grammar
    [lexer (NewLineToken . Lazy.toStrict) [rx "\n\r", rx "\r\n", rx $ anyOf "\n\r"]]

inlineCommentGrammar :: Monad m => Char -> Char -> Char -> Grammar m StrictText
inlineCommentGrammar start interm end = liftM Lazy.toStrict $ do
  let mid    = rx (interm, MIN 1)
  let open   = rx  start
  let close  = rx  end
  let notMid = rx (noneOf [interm], MIN 0)
  let loop     str = mplus (liftM (mappend str) $ grammar [lexer id [close]]) $
        liftM (mappend str) (grammar [lexer id [notMid, mid]]) >>= loop
  grammar [lexer id [open, mid]] >>= loop

-- | This is a C programming language styled inline comment: @/* comment text */@.
newtype CInlineComment = CInlineComment StrictText deriving (Eq, Ord, Typeable)
instance ToText CInlineComment where { toText (CInlineComment o) = o; }
instance StringLength CInlineComment where { stringLength = stringLength . toText; }
instance Show CInlineComment where { show = show . toText; }
instance PPrintable CInlineComment where { pPrint = return . pText . toText; }
instance NFData CInlineComment where { rnf (CInlineComment o) = deepseq o (); }
instance Monad m => The (Grammar m CInlineComment) where
  the = grammarTyped $ liftM CInlineComment $ inlineCommentGrammar '/' '*' '/'

-- | This is a C programming language styled inline comment: @/* comment text */@.
newtype HaskellInlineComment = HaskellInlineComment StrictText deriving (Eq, Ord, Typeable)
instance ToText HaskellInlineComment where { toText (HaskellInlineComment o) = o; }
instance StringLength HaskellInlineComment where { stringLength = stringLength . toText; }
instance Show HaskellInlineComment where { show = show . toText; }
instance PPrintable HaskellInlineComment where { pPrint = return . pText . toText; }
instance NFData HaskellInlineComment where { rnf (HaskellInlineComment o) = deepseq o (); }
instance Monad m => The (Grammar m HaskellInlineComment) where
  the = grammarTyped $ liftM HaskellInlineComment $ inlineCommentGrammar '{' '-' '}'

-- | The 'Grammar' used to parse the 'CEndlineComment' and 'HaskellEndlineComment' tokens.
endlineCommentGrammar :: (Monad m, ToRegex t) => t -> Grammar m StrictText
endlineCommentGrammar t = grammar [lexer Lazy.toStrict [rx t, rx (noneOf "\n", MIN 0)]]

-- | This is a C programming language styled end-of-line comment: @// comment text@. The terminating
-- newline character is not included.
newtype CEndlineComment = CEndlineComment StrictText deriving (Eq, Ord, Typeable)
instance ToText CEndlineComment where { toText (CEndlineComment o) = o; }
instance StringLength CEndlineComment where { stringLength = stringLength . toText; }
instance Show CEndlineComment where { show = show . toText; }
instance PPrintable CEndlineComment where { pPrint = return . pText . toText; }
instance NFData CEndlineComment where { rnf (CEndlineComment o) = deepseq o (); }
instance Monad m => The (Grammar m CEndlineComment) where
  the = grammarTyped $ liftM CEndlineComment $ endlineCommentGrammar "//"

-- | This is a C programming language styled end-of-line comment: @// comment text@. The terminating
-- newline character is not included.
newtype HaskellEndlineComment = HaskellEndlineComment StrictText deriving (Eq, Ord, Typeable)
instance ToText HaskellEndlineComment where { toText (HaskellEndlineComment o) = o; }
instance StringLength HaskellEndlineComment where { stringLength = stringLength . toText; }
instance Show HaskellEndlineComment where { show = show . toText; }
instance PPrintable HaskellEndlineComment where { pPrint = return . pText . toText; }
instance NFData HaskellEndlineComment where { rnf (HaskellEndlineComment o) = deepseq o (); }
instance Monad m => The (Grammar m HaskellEndlineComment) where
  the = grammarTyped $ liftM HaskellEndlineComment $ endlineCommentGrammar "--"

-- | This is a C programming language style identifier, these are names starting with any
-- alphabetic or underscore characters, and containing any number of alphanumeric or underscore
-- characters.
newtype CIdentifier = CIdentifier StrictText deriving (Eq, Ord, Typeable)
instance ToText CIdentifier where { toText (CIdentifier o) = o; }
instance StringLength CIdentifier where { stringLength = stringLength . toText; }
instance Show CIdentifier where { show = show . toText; }
instance PPrintable CIdentifier where { pPrint = return . pText . toText; }
instance NFData CIdentifier where { rnf (CIdentifier o) = deepseq o (); }
instance Monad m => The (Grammar m CIdentifier) where
  the = let underscore = mappend (anyOf "_") in grammarTyped $ grammarTable $ grammar $
    [ lexer (CIdentifier . Lazy.toStrict) $
        [ rx (underscore alphabetical, MIN 1)
        , rx (underscore alphanumerical, MIN 0)
        ]
    ]

----------------------------------------------------------------------------------------------------

-- | This is a class providing the 'intLitToken' method, which is a 'Dao.Lens.Lens' method for accessing
-- and modifying an integer value token string.
class IntLitTokenLens o where { intLitToken :: Monad m => Lens m o Integer; }

-- | This is a token for an optional positive or negative sign prefix.
newtype PosNeg = PosNeg Ordering deriving (Eq, Ord, Typeable)
instance TestNull PosNeg where { nullValue = PosNeg EQ; testNull (PosNeg o) = o==EQ; }
instance ToText PosNeg where { toText = flip (prependSign Strict.cons) mempty; }
instance StringLength PosNeg where { stringLength = stringLength . toText; }
instance PPrintable PosNeg where { pPrint = return . pText . toText; }
instance NFData PosNeg where { rnf (PosNeg o) = seq o (); }
instance Show PosNeg where { show = flip (prependSign (:)) ""; }
instance Monad m => The (Grammar m PosNeg) where
  the =
    let f c = PosNeg $
          if c == Lazy.singleton '+' then GT else
            if c == Lazy.singleton '-' then LT else EQ
    in  grammarTyped $ grammar [lexer f [rx (anyOf "+-", MAX 1)]]

-- | This function prepends a sign character (@'+'@, @'-'@, or nothing) to a string-like value based
-- on the contents of the 'PosNeg' value given. For example, if you have a 'PosNeg' value
-- @'PosNeg' 'Prelude.GT'@
-- and would like to update a 'Prelude.String' to include a sign character based on this 'PosNeg'
-- value, you could call this function like so:
--
-- @
-- 'prependSign' (:) ('PosNeg' 'Prelude.GT') "12345"
-- @
prependSign :: (Char -> o -> o) -> PosNeg -> o -> o
prependSign cons (PosNeg o) = case o of { EQ -> id; GT -> cons '+'; LT -> cons '-'; }

-- | This function will negate a 'Prelude.Num' number @n@ if the given 'PosNeg' is
-- @('PosNeg' 'Prelude.LT')@.
posNegNum :: Num n => PosNeg -> n -> n
posNegNum (PosNeg o) = if o==LT then negate else id

-- | This function will create a 'PosNeg' data type based on the sign of the given
-- 'Prelude.Num' number n.
posNegSign :: (Num n, Ord n) => n -> PosNeg
posNegSign = PosNeg . compare 0

-- | Part of a 'NumberLitToken' after a decimal point, for example after a @1.0@ there may be an
-- exponent like @1.0e-12@, or @1.0E9@. This contains the @'e'@ and the sign, and the exponent
-- characters.
newtype ExponentLitToken = ExponentLitToken (Bool, PosNeg, StrictText) deriving (Eq, Ord, Typeable)
instance Show ExponentLitToken where { show = show . toText; }
instance PPrintable ExponentLitToken where { pPrint = return . pText . toText; }
instance NFData ExponentLitToken where { rnf (ExponentLitToken o) = deepseq o (); }
instance StringLength ExponentLitToken where { stringLength = stringLength . toText; }

instance ToText ExponentLitToken where
  toText (ExponentLitToken (e, sign, txt)) =
    Strict.cons (if e then 'E' else 'e') . prependSign Strict.cons sign $ txt

instance TestNull ExponentLitToken where
  testNull (ExponentLitToken (_, _, txt)) = Strict.null txt || txt == Strict.singleton '0'
  nullValue = ExponentLitToken (False, nullValue, Strict.singleton '0')

instance IntLitTokenLens ExponentLitToken where
  intLitToken = newLens
    (\  (ExponentLitToken (_, pos, txt)) -> posNegNum pos $ read (Strict.unpack txt))
    (\i (ExponentLitToken (e, _  , _  )) -> ExponentLitToken (e, posNegSign i, Strict.pack $ show $ abs i))

instance Monad m => The (Grammar m ExponentLitToken) where
  the = let mk e s d = ExponentLitToken (e == Lazy.singleton 'E', s, d) in grammarTyped $
    (grammar [lexer mk [rx $ anyOf "Ee"]] `ap` (the :: Grammar m PosNeg)) `ap`
      grammar [lexer Lazy.toStrict [rx (within [('0', '9')], MIN 1)]]

-- | A 'Dao.Lens.Lens' for the contents of the 'ExponentLitToken'.
exponentTokenLens :: Monad m => Lens m ExponentLitToken (Bool, PosNeg, StrictText)
exponentTokenLens = newLens (\ (ExponentLitToken o) -> o) (\o _ -> ExponentLitToken o)

-- | A 'Dao.Lens.Lens' for the 'Prelude.Bool' value indicating whether the exponent was a positive
-- letter @E@. For example in the number @6.02213665168E23@ has a capital letter @E@ indicating the
-- exponent. Conversely, the number @6.02213665168e23@ has a lowercase letter @e@ indicating the
-- exponent.
exponentTokenCapitalE :: Monad m => Lens m ExponentLitToken Bool
exponentTokenCapitalE = exponentTokenLens >>> tuple0

-- | A 'Dao.Lens.Lens' indicating the sign of the exponent, for example the sign of the exponent
-- @23@ in the number @6.02213665168e+23@, or the sign of the number @-15@ in the
-- exponent @4.135667516e-15@.
exponentTokenPosNeg :: Monad m => Lens m ExponentLitToken PosNeg
exponentTokenPosNeg = exponentTokenLens >>> tuple1

-- | A hexadecimal integer literal is a base-sixteen number with the prefix @0x@ or @0X@.
newtype HexIntLitToken = HexIntLitToken Strict.Text deriving (Eq, Ord, Typeable)
instance ToText HexIntLitToken where { toText (HexIntLitToken o) = o; }
instance StringLength HexIntLitToken where { stringLength = stringLength . toText; }
instance PPrintable HexIntLitToken where { pPrint = return . pText . toText; }
instance NFData HexIntLitToken where { rnf (HexIntLitToken o) = deepseq o (); }
instance Show HexIntLitToken where { show = show . toText; }

instance TestNull HexIntLitToken where
  testNull (HexIntLitToken o) = and $ ('0' ==) <$> Strict.unpack o
  nullValue = HexIntLitToken $ Strict.pack "0"

instance IntLitTokenLens HexIntLitToken where
  intLitToken = newLens
    (\  (HexIntLitToken o) -> case readHex $ Strict.unpack o of
          [(o, "")] -> o
          _ -> error $ "could not parse hexadecimal integer literal "++show o
    )
    (\i _ -> HexIntLitToken $ Strict.pack $ "0x" ++ fmap toUpper (showHex i ""))

instance Monad m => The (Grammar m HexIntLitToken) where
  the = let rxhex = rx (csetBase16, MIN 1) in liftM (HexIntLitToken . Lazy.toStrict . Lazy.drop 2) $
    grammarTyped $ grammar [lexer id [rx "0x", rxhex], lexer id [rx "0X", rxhex]]

----------------------------------------------------------------------------------------------------

-- | An octal integer literal is a base-eight number that always begins with the digit @'0'@. Note that
-- the Haskell does not support octal integer literals, but the C family of programming languages do.
newtype OctIntLitToken = OctIntLitToken Strict.Text deriving (Eq, Ord, Typeable)
instance ToText OctIntLitToken where { toText (OctIntLitToken o) = o; }
instance StringLength OctIntLitToken where { stringLength = stringLength . toText; }
instance Show OctIntLitToken where { show = show . toText; }
instance PPrintable OctIntLitToken where { pPrint = return . pText . toText; }
instance NFData OctIntLitToken where { rnf (OctIntLitToken o) = deepseq o (); }

instance TestNull OctIntLitToken where
  testNull (OctIntLitToken o) = and $ ('0' ==) <$> Strict.unpack o
  nullValue = OctIntLitToken $ Strict.singleton '0'

instance IntLitTokenLens OctIntLitToken where
  intLitToken = newLens
    (\  (OctIntLitToken o) -> case readOct $ Strict.unpack o of
          [(o, "")] -> o
          _ -> error $ "could not parse octal integer literal "++show o
    )
    (\i _ -> OctIntLitToken $ Strict.pack $ '0' : showOct i "")

instance Monad m => The (Grammar m OctIntLitToken) where
  the = grammarTyped $ grammar [lexConst () [rx '0']] >>
    grammar [lexer (OctIntLitToken . Lazy.toStrict) [rx (csetBase8, MIN 1)]]

----------------------------------------------------------------------------------------------------

-- | A decimal integer literal is a base-ten number consisting of the digits 0-9, but not starting
-- with a leading @'0'@ digit unless the number is zero. If it starts with a leading @'0'@ digit it
-- could (in the C programming language family) indicate a base-8 integer literal. In the Haskell
-- language family, any string of base-10 digits is a decimal integer literal, regardless of whether
-- it has leading @'0'@ digits.
newtype DecIntLitToken = DecIntLitToken Strict.Text deriving (Eq, Ord, Typeable)
instance ToText DecIntLitToken where { toText (DecIntLitToken o) = o; }
instance StringLength DecIntLitToken where { stringLength = stringLength . toText; }
instance Show DecIntLitToken where { show = show . toText; }
instance PPrintable DecIntLitToken where { pPrint = return . pText . toText; }
instance NFData DecIntLitToken where { rnf (DecIntLitToken o) = deepseq o (); }
instance TestNull DecIntLitToken where
  testNull (DecIntLitToken o) = and $ ('0' ==) <$> Strict.unpack o
  nullValue = DecIntLitToken $ Strict.pack "0"

instance IntLitTokenLens DecIntLitToken where
  intLitToken = newLens
    (\  (DecIntLitToken o) -> case reads $ Strict.unpack o of
          [(o, "")] -> o
          _ -> error $ "could not parse integer literal "++show o
    )
    (\i _ -> DecIntLitToken $ Strict.pack $ show i)

instance Monad m => The (Grammar m DecIntLitToken) where
  the = grammarTyped $ grammar [lexer (DecIntLitToken . Lazy.toStrict) [rx (csetBase10, MIN 1)]]

----------------------------------------------------------------------------------------------------

-- | A non-C programming language family numerical literal, including ordinary integers, floating
-- point numbers, hexadecimal integers, but not octal integers. The 'CNumberLit' data type includes
-- a union of this data type along with 'OctIntLitToken's.
data NumberLitToken
  = DecIntNumberLit DecIntLitToken
  | HexIntNumberLit HexIntLitToken
  | IntExponentLit  DecIntLitToken ExponentLitToken
  | DecimalPointLit (Maybe DecIntLitToken) DecIntLitToken (Maybe ExponentLitToken)
  deriving (Eq, Ord, Typeable)

-- | Get or set the integer portion of the 'NumberLitToken'. In the case of a decimal point, only the
-- part before the decimal point is modified.
instance IntLitTokenLens NumberLitToken where
  intLitToken =
    newLens
      (\  o -> case o of
          DecIntNumberLit o     -> o~>intLitToken
          HexIntNumberLit o     -> o~>intLitToken
          IntExponentLit  o   _ -> o~>intLitToken
          DecimalPointLit o _ _ -> maybe 0 (~> intLitToken) o
      )
      (\i o -> case o of
          DecIntNumberLit o     -> DecIntNumberLit $ with o [intLitToken <~ i]
          HexIntNumberLit o     -> HexIntNumberLit $ with o [intLitToken <~ i]
          IntExponentLit  o   e -> IntExponentLit (with o [intLitToken <~ i]) e
          DecimalPointLit o d e ->
            DecimalPointLit (Just $ by [intLitToken <~ i] $ fromMaybe nullValue o) d e
      )

instance ToText NumberLitToken where
  toText o = Lazy.toStrict $ Lazy.fromChunks $ case o of
    DecIntNumberLit o     -> [toText o]
    HexIntNumberLit o     -> [Strict.pack "0x", toText o]
    IntExponentLit  o e   -> [toText o, toText e]
    DecimalPointLit o d e ->
      [ maybe (Strict.singleton '0') toText o
      , Strict.singleton '.', toText d
      , maybe mempty toText e
      ]

instance StringLength NumberLitToken where { stringLength = stringLength . toText; }

instance PPrintable NumberLitToken where { pPrint = return . pText . toText; }

instance Show NumberLitToken where { show = show . toText; }

instance NFData NumberLitToken where
  rnf o = case o of
    DecIntNumberLit o     -> deepseq o ()
    HexIntNumberLit o     -> deepseq o ()
    IntExponentLit  a b   -> deepseq a $! deepseq b ()
    DecimalPointLit a b c -> deepseq a $! deepseq b $! deepseq c ()

instance Monad m => The (Grammar m NumberLitToken) where
  the = grammarTyped $ grammarTable $ msum $
    [ the >>= \ (DecIntLitToken o) -> msum
        [ liftM (IntExponentLit $ DecIntLitToken o) the
        , liftM2 (DecimalPointLit $ Just $ DecIntLitToken o)
                 (grammar [lexConst () [rx '.']] >> the)
                 (mplus (liftM Just the) $ return Nothing)
        ]
    , liftM HexIntNumberLit the
    , liftM2 (DecimalPointLit Nothing)
             (grammar [lexConst () [rx '.']] >> the)
             (mplus (liftM Just the) $ return Nothing)
    ]

-- | Retrieve or update a 'DecIntLitToken' within a 'NumberLitToken' if it exists.
decimalPointedNumberToken :: Monad m => Lens m NumberLitToken (Maybe DecIntLitToken)
decimalPointedNumberToken = newLens
  (\  o -> case o of
      DecimalPointLit _ o _ -> Just o
      _                      -> Nothing
  )
  (\d o -> case d of
      Nothing -> case o of
        DecimalPointLit o' _ e ->
          let o = fromMaybe nullValue o' in maybe (DecIntNumberLit o) (IntExponentLit o) e
        _                       -> o
      Just  d -> case o of
        DecIntNumberLit o     -> DecimalPointLit (Just o) d Nothing
        HexIntNumberLit o     -> DecimalPointLit (Just $ new [intLitToken <~ o~>intLitToken]) d Nothing
        IntExponentLit  o   e -> DecimalPointLit (Just o) d (Just e)
        DecimalPointLit o _ e -> DecimalPointLit o d e
  )

-- | Retrieve or update a 'ExponentLitToken' within a 'NumberLitToken' if it exists.
exponentLiteralToken :: Monad m => Lens m NumberLitToken (Maybe ExponentLitToken)
exponentLiteralToken = newLens
  (\  o -> case o of
      IntExponentLit  _   e -> Just e
      DecimalPointLit _ _ e -> e
      _                      -> Nothing
  )
  (\e o -> case e of
      Nothing -> case o of
        DecimalPointLit o d _ -> DecimalPointLit o d Nothing
        IntExponentLit  o   _ -> DecIntNumberLit o
        _                      -> o
      Just  e -> case o of
        DecIntNumberLit o     -> IntExponentLit  o          e
        HexIntNumberLit o     -> IntExponentLit (new [intLitToken <~ o~>intLitToken]) e
        DecimalPointLit o d _ -> DecimalPointLit o  d (Just e)
        IntExponentLit  o   _ -> IntExponentLit  o          e
  )

----------------------------------------------------------------------------------------------------

-- | A C programming language family numerical literal, including ordinary integers, floating point
-- numbers, hexadecimal integers, and octal integers. The 'CNumberLit' data type is a union of this
-- data type along with 'OctIntLitToken's.
data CNumberLit
  = CNumberLit       NumberLitToken
  | COctIntNumberLit OctIntLitToken
  deriving (Eq, Ord, Typeable)

-- | Get or set the integer portion of the 'CNumberLit'. In the case of a decimal point, only the
-- part before the decimal point is modified.
instance IntLitTokenLens CNumberLit where
  intLitToken =
    newLens
      (\  o -> case o of
          CNumberLit       o -> o~>intLitToken
          COctIntNumberLit o -> o~>intLitToken
      )
      (\i o -> case o of
          CNumberLit       o -> CNumberLit       $ with o [intLitToken <~ i]
          COctIntNumberLit o -> COctIntNumberLit $ with o [intLitToken <~ i]
      )

instance ToText CNumberLit where
  toText o = Lazy.toStrict $ Lazy.fromChunks $ case o of
    CNumberLit       o -> [toText o]
    COctIntNumberLit o -> [Strict.singleton '0', toText o]

instance StringLength CNumberLit where { stringLength = stringLength . toText; }

instance PPrintable CNumberLit where { pPrint = return . pText . toText; }

instance Show CNumberLit where { show = show . toText; }

instance NFData CNumberLit where
  rnf o = case o of
    CNumberLit       o -> deepseq o ()
    COctIntNumberLit o -> deepseq o ()

instance Monad m => The (Grammar m CNumberLit) where
  the = grammarTyped $ grammarTable $ msum $
    [ the >>= \ (DecIntLitToken o) -> msum
        [ liftM (CNumberLit . IntExponentLit (DecIntLitToken o)) the
        , liftM CNumberLit $
            liftM2 (DecimalPointLit $ Just $ DecIntLitToken o)
              (grammar [lexConst () [rx '.']] >> the)
              (mplus (liftM Just the) $ return Nothing)
        , return $ case Strict.unpack o of
            '0':c:cx | and (csetMember csetBase8 <$> c:cx) -> COctIntNumberLit $ OctIntLitToken o
            _ -> CNumberLit $ DecIntNumberLit $ DecIntLitToken o
        ]
    , liftM (CNumberLit . HexIntNumberLit) the
    , liftM CNumberLit $
        liftM2 (DecimalPointLit Nothing)
               (grammar [lexConst () [rx '.']] >> the)
               (mplus (liftM Just the) $ return Nothing)
    ]

-- | Retrieve or update a 'DecIntLitToken' within a 'CNumberLit' if it exists.
cDecimalPoint :: Monad m => Lens m CNumberLit (Maybe DecIntLitToken)
cDecimalPoint = newLens
  (\  o -> case o of
      CNumberLit o -> o~>decimalPointedNumberToken
      _            -> Nothing
  )
  (\d o -> case o of
      CNumberLit o -> CNumberLit $ with o [decimalPointedNumberToken <~ d]
      _            -> o
  )

-- | Retrieve or update a 'ExponentLitToken' within a 'CNumberLit' if it exists.
cExponent :: Monad m => Lens m CNumberLit (Maybe ExponentLitToken)
cExponent = newLens
  (\  o -> case o of
      CNumberLit o -> o~>exponentLiteralToken
      _            -> Nothing
  )
  (\d o -> case o of
      CNumberLit o -> CNumberLit $ with o [exponentLiteralToken <~ d]
      _            -> o
  )

----------------------------------------------------------------------------------------------------

-- | Used for parsing things like string literal, where you have a single character delimiting both
-- the start and end of the token, and a single escape character to allow for accepting a delimiter
-- character into the token. The first parameter is an error message to use to report when the token
-- exceeds the end of the parser's input string without finding an end delimiter.
escapedDelimited :: Monad m => String -> Char -> Char -> Grammar m Lazy.Text
escapedDelimited errmsg delim esc = grammar [lexer id [rx delim]] >>= loop where
  loop str = grammar [lexer id [rx (noneOf [delim, esc], MIN 0)]] >>= \nondelim ->
    flip elseFail (errmsg++" not delimited by closing "++show delim) $ do
      escDelim <- grammar [lexer id [rx $ anyOf [delim, esc]]]
      str <- return $ str <> nondelim <> escDelim
      if Lazy.unpack escDelim == [esc]
      then grammar [lexer id [rx anyChar]] >>= loop . (str <>)
      else return (str <> escDelim)

-- | This is a double-quoted string literal, that is a string literal delimited by an opening and
-- closing double-quote character: @(")@. Tokens of this kind typically represent string literals in
-- both Haskell and the C family of programming languages.
newtype Quoted2Token = Quoted2Token StrictText deriving (Eq, Ord, Typeable)
instance ToText Quoted2Token where { toText (Quoted2Token o) = o; }
instance StringLength Quoted2Token where { stringLength = stringLength . toText; }
instance PPrintable Quoted2Token where { pPrint = return . pText . toText; }
instance Show Quoted2Token where { show = show . toText; }
instance NFData Quoted2Token where { rnf (Quoted2Token o) = deepseq o (); }
instance Monad m => The (Grammar m Quoted2Token) where
  the = liftM (Quoted2Token . Lazy.toStrict) (escapedDelimited "string literal" '"' '\\')

-- | This is a single-quoted string literal, that is a string literal delimited by an opening and
-- closing single-quote character: @(')@. Tokens of this kind typically represent character literals
-- in both Haskell and the C family of programming languages, however in other languages, like Bash
-- or Python these string literals also represent string data types.
newtype Quoted1Token = Quoted1Token StrictText deriving (Eq, Ord, Typeable)
instance ToText Quoted1Token where { toText (Quoted1Token o) = o; }
instance StringLength Quoted1Token where { stringLength = stringLength . toText; }
instance PPrintable Quoted1Token where { pPrint = return . pText . toText; }
instance Show Quoted1Token where { show = show . toText; }
instance NFData Quoted1Token where { rnf (Quoted1Token o) = deepseq o (); }
instance Monad m => The (Grammar m Quoted1Token) where
  the = grammarTyped $
    liftM (Quoted1Token . Lazy.toStrict) $ escapedDelimited "string literal" '\'' '\\'

