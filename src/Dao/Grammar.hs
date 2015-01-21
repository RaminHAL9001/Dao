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
-- This module provides three key data types, 'Regex', 'Lexer', and 'Grammar', and the combinators
-- necessary to define CFGs. These data types are actually not parsers, but they can be converted
-- to parsers using 'regexToParser', 'lexerToParser', 'grammarToParser', 'grammarToVerboseParser'.
-- 
-- The 'Regex' is a simple data type that matches strings. 'Lexer's take 'Regex's and use the
-- matched strings to construct data types in 'Control.Monad.Monad'-ic and
-- 'Control.Applicative.Applicative' ways. The 'Grammar' data type uses 'Lexer's to build CFGs.
--
-- Once a 'Grammar' has been defined, the 'Grammar' can be converted to parser functions. The
-- parsing functions are polymorphic over any type of input string that instantiates the
-- 'Dao.Text.Builder.TextBuilder' class.
--
-- The 'Grammar' and 'Lexer' data types can be constructed freely, but impossible 'Grammar's, which
-- are 'Grammar's which could never possibly match any string, and ambiguous 'Grammar's, which are
-- 'Grammar's where a single input could match more than one 'Grammar' in a list of concurrent
-- 'Grammar' choices, evaluate to an 'InvalidGrammar' data type. This allows you to build your
-- 'Grammar's without worrying about the implementation details of the actual parser. Parsers
-- constructed by valid 'Grammar's should always work as intended.
--
-- The 'Grammar', 'Lexer', and 'Regex' can convert to any monadic parser. A type class 'MonadParser'
-- is defined to classify the various monadic parsers that a 'Grammar', 'Lexer' or 'Regex' could be
-- converted to. The Haskell Platform parsers 'Text.ParserCombinator.ReadP' and
-- 'Text.ParserCombinator.ReadPrec' are instantiated into the 'MonadParser' class so 'Grammar's can
-- be used to instantiate 'Prelude.Read.readPrec'.
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
  ( StrictText, LazyText,
    -- * Combinators for Building 'Regex's
    Regex, rxString, rxChar, rxAnyOf, rxNoneOf, rxRanges, rxCharSet,
    rxAny1, rxNotRanges,
    RegexRepeater, ToRegexRepeater(toRegexRepeater), RepeatMinMax(MIN, MAX), lexRepeaterToPair,
    probRepeater,
    -- * Combinators for Building 'Grammar's and 'Lexer's
    Lexer, typeOfLexer, regexOfLexer, lexer, lexConst,
    Grammar, typeOfGrammar, choice, elseFail, (<?>),
    HasLexer(theLexer), HasGrammar(theGrammar),
    TypeableGrammar(informType),
    -- * Working with source locations.
    Location(NoLocation, StartLocation, EndLocation, Location),
    LocationFunctor(fmapLocation),
    getPoint, setPoint, location, unwrapLocation, locationRegion, startLocation, endLocation,
    newline, movePoint, asciiLineCounter, moveCursor, backtrack,
    DeleteContents(deleteContents),
    -- * Inspeting 'Lexer's and 'Grammar's.
    LexerSignature(LexerReturn, LexerRegex, LexerRead, LexerFail, LexerReject),
    lexerSignature, lexerSignatureEq,
    GrammarSignature(
      GrammarEmpty, GrammarReturn, GrammarLift, GrammarTable, GrammarChoice, GrammarType,
      GrammarComment, GrammarFail, GrammarReject
    ), grammarSignature, grammarSignatureEq, pprintGrammarSignature,
    -- * Converting 'Grammar's to Monadic Parsers ('MonadParser's)
    grammarToParser, grammarToVerboseParser, lexerToParser, regexToParser, regexUnitToParser,
    -- * The Error Type Used By All Monadic Parsers ('MonadParser's)
    LexerError(LexerError),
    -- * Making Parsers from 'Grammar's
    MonadParser(
      look, look1, get1, string, count, eof, munch, munch1,
      noMoreThan, satisfy, char, regex, pfail
    ),
    -- * Spices for Home-Baked Monadic Parsers
    MonadSourceCodeParser(getTextPoint, setTextPoint),
    MonadPrecedenceParser(prec, step, reset),
    MonadBacktrackingParser(unstring),
    -- * Low-Level Data Structures
    lexError, lexErrorUnit, lexErrorType, lexErrorText, lexErrorLocation,
    CharSet, CharTable, CharCount, ColumnNumber, LineNumber, pPrintCharSet,
    TextPoint(TextPoint), lineNumber, columnNumber,
    TextRegion, textRegion, textRegionToPair,
    Probability, computeLogProb, computeProb, probChar, probSet, probText,
    ShadowsSeries(shadowsSeries), ShadowsParallel(shadowsParallel),
    InitialCharSetOf(initialCharSetOf),
    RxMinLength(rxMinLength), RxMaxLength(rxMaxLength),
    RegexUnit, rxGetString, rxGetCharSet, rxGetRepeater,
    probRegexUnit,
    InvalidGrammar(InvalidRegex, CutSomeBranches, InvalidGrammar),
    cutBranches, invalidGrammarMessage,
    regexTypeA, regexTypeB, regexUnitA, regexUnitB, regexSeqA,
    regexSeqB, regexBadSequence, invalidGrammar,
    probRegex, lexRegexUnits,
    grammarBranches, makeCharTable
  )
  where

import           Dao.Array
import           Dao.Count
import qualified Dao.Interval    as Iv
import           Dao.PPrint
import qualified Dao.SparseArray as Sa
import           Dao.TestNull
import           Dao.Text
import           Dao.Text.Builder

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans

import qualified Data.Array.IArray as A
import           Data.Char hiding (Space)
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text      as Strict
import           Data.Typeable

import           Numeric (showHex)

import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

----------------------------------------------------------------------------------------------------

-- | This class generalizes over the functions for parsers like 'Text.ParserCombinators.ReadP.ReadP',
-- 'Text.GrammarCombinator.ReadPrec.ReadPrec', and our own 'Dao.Grammar.Text.TextParse'.
--
-- When programming your parsers, you actually will not *NOT* be using the functions in this class.
-- Instead, you construct your parsers using the 'Grammar' or 'Lexer' data type, then use the
-- 'makeParser' function to convert the 'Grammar' to a monadic parser function (one which
-- instantiates this class). This class only exists to provide a consistent interface to the various
-- parser monads available.
--
-- This class merely provides the necessary functions that 'makeParser' can use to convert 'Lexer'
-- and 'Grammar' data types to some concrete monadic parser -- a monadic parser that instantiates
-- this class. You do not need to concern yourself with the details of how the 'Lexer's or
-- 'Grammar's are converted, just know that the conversion process calls these functions.
--
-- The minimal complete definition includes 'look', 'look1', 'get1', 'string', and 'count', but is
-- highly recommended you also instantiate 'eof', 'munch', 'munch1', and 'noMoreThan' as the default
-- instantiations which rely on the minimal complete definition are a bit inefficient.
class (Monad m, MonadPlus m, TextBuilder text) => MonadParser text m | m -> text where
  -- | Look ahead without consuming any text. (required)
  look  :: m text
  -- | Look ahead 1 character without consuming it. Evaluate to 'Control.Monad.mzero' if at end of
  -- input text. (required)
  look1 :: m Char
  -- | Consume a single character from the input text. (required)
  get1 :: m Char
  -- | If the given text matches the current head of the parser input text, consume that text and
  -- succeed, otherwise evaluate to 'Control.Applicative.empty'. (required)
  string :: text -> m text
  -- | Consume a given number of characters or fail if there aren't enough, and never parse more
  -- than the given number of characters. This requires a look ahead, so it is left to the
  -- instatiator of this class to provide the most efficient implementation. (required)
  count :: Count -> Iv.Set Char -> m text
  -- | Succeed only if the end of the input text has been reached.
  eof :: m ()
  eof = mplus (look1 >> return False) (return True) >>= guard
  -- | Consume and return all characters from the head of the input text that satisfy the predicate.
  -- Never backtracks, returns an empty string if the head of the input text does not satisfy the
  -- predicate at all.
  munch :: Iv.Set Char -> m text
  munch p =
    let loop cx = mplus (liftM Just $ satisfy p) (return Nothing) >>=
          maybe (return cx) (loop . snocChar cx) in loop mempty
  -- | Consume and return all characters from the head of the input text that match the given
  -- predicate, but backtracks if no characters from the head of the input text match.
  munch1 :: Iv.Set Char -> m text
  munch1 p = satisfy p >>= \c -> munch p >>= \cx -> return (consChar c cx)
  -- | Never fail, but consume as many characters satisfying the predicate as possible, and never
  -- consuming more than the specified amount.
  noMoreThan :: Count -> Iv.Set Char -> m text
  noMoreThan lim p =
    let loop lim cx = if lim<=0 then return cx else
          mplus (liftM Just $ satisfy p) (return Nothing) >>=
            maybe (return cx) (loop (pred lim) . snocChar cx)
    in  loop lim mempty
  -- | Consumes the next character only if it satisfies the given predicate.
  satisfy :: Iv.Set Char -> m Char
  satisfy p = look1 >>= \c -> if Iv.member p c then get1 else mzero
  -- | Like 'satisfy' but matches a single character.
  char :: Char -> m Char
  char = satisfy . Iv.singleton
  -- | This optional function should attempt to match a 'Regex' to the input string by using
  -- 'regexToParser'.
  regex :: Regex -> m text
  regex = regexToParser
  -- | When the input text does not match a 'Grammar' or 'Lexer', an error of type 'LexerError' must
  -- be handed over to this monad. What happens next is up to the instance provided here. It may
  -- throw an error, or it may evaluate to 'Control.Applicative.empty'.
  pfail :: LexerError -> m ()
  pfail err = fail ("Parser error: "++show err)

instance MonadParser String ReadP.ReadP where
  look    = ReadP.look
  look1   = look >>= \cx -> case cx of { [] -> mzero; c:_ -> return c; }
  get1    = ReadP.get
  string  = ReadP.string
  count i = ReadP.count (fromIntegral i) . ReadP.satisfy . Iv.member
  eof     = ReadP.eof
  munch   = ReadP.munch . Iv.member
  munch1  = ReadP.munch1 . Iv.member
  satisfy = ReadP.satisfy . Iv.member
  char    = ReadP.char
  pfail   = fail . show

instance MonadParser String ReadPrec.ReadPrec where
  look    = ReadPrec.look
  look1   = look >>= \cx -> case cx of { [] -> mzero; c:_ -> return c; }
  get1    = ReadPrec.get
  string  = ReadPrec.lift . ReadP.string
  count i = ReadPrec.lift . ReadP.count (fromIntegral i) . ReadP.satisfy . Iv.member
  eof     = ReadPrec.lift ReadP.eof
  munch   = ReadPrec.lift . ReadP.munch . Iv.member
  munch1  = ReadPrec.lift . ReadP.munch1 . Iv.member
  satisfy = ReadPrec.lift . ReadP.satisfy . Iv.member
  char    = ReadPrec.lift . ReadP.char
  pfail   = fail . show

----------------------------------------------------------------------------------------------------

-- | This class extends the 'MonadParser' class with functions for precedence parsing, useful for
-- parsing languages that have infix operators.
class (Monad m, MonadPlus m, TextBuilder text, MonadParser text m) =>
  MonadPrecedenceParser text m | m -> text where
    -- | This combinator should evaluate to 'Control.Monad.mzero' if the given 'Count' value is less
    -- than the current precedence state of the parser monad, or else it should set the current
    -- precedence value to the given 'Prelude.Int' value and then evaluate the given parser. This
    -- allows the lower-precedence parsers that called this parser to have a chance at parsing an
    -- infix operator of lower precedence than the operator that this parser would have parsed.
    -- Otherwise if the current precedence state of the parser monad is greater than or equal to the
    -- given count value, the provided parser should go ahead and parse right away. (required)
    prec :: Int -> m a -> m a
    -- | Indicate that this parser is parsing an infix operator of higher precedence than the parser
    -- that called it. (required)
    step :: m a -> m a
    -- | Ask the current precedence of the parser state to be reset to it's minimal value. This is
    -- usually done after an openening-bracket token has been parsed. The current precedence of the
    -- parser state should be "saved" so that when the given parser is done (and a closing-bracket
    -- token is parsed), the precedence value from outside of the brackets is restored.
    reset :: m a -> m a

instance MonadPrecedenceParser String ReadPrec.ReadPrec where
  prec  = ReadPrec.prec . fromIntegral
  step  = ReadPrec.step
  reset = ReadPrec.reset

----------------------------------------------------------------------------------------------------

-- | This class extends the 'MonadParser' class with functions for manipulating line and column
-- numbers, which is what parsers of computer language source code often need to do for producing
-- useful error messages. If your monadic parser can track line and or column numbers, you should
-- consider instantiating it into this class.
class (Monad m, MonadPlus m, TextBuilder text, MonadParser text m) =>
  MonadSourceCodeParser text m | m -> text where
    getTextPoint :: m TextPoint
    setTextPoint :: TextPoint -> m ()

----------------------------------------------------------------------------------------------------

-- | Backtracking parsers should be avoided because they are inefficient. However, this class is
-- provided if the functionality is required.
class (Monad m, MonadPlus m, TextBuilder text, MonadParser text m) =>
  MonadBacktrackingParser text m | m -> text where
    -- | This function should prepend arbitrary text to the front of the input string in the state
    -- of the parser. It is called 'unstring' because it has the opposite effect of the 'string'
    -- function.
    unstring :: text -> m ()

----------------------------------------------------------------------------------------------------

-- | A 'LexerError' can occur during the parser evaluation. This is different from
-- 'InvalidGrammar' which occurs when constructing a 'Lexer' or 'Grammar', and happens if there are
-- ambiguities in the specified Context Free Grammar. A 'LexerError', on the other hand, is
-- provided to the lower-level parser monad (the monadic parser that instantiates 'MonadParser') by
-- way of the 'pfail' function.
data LexerError
  = LexerError
    { lexError          :: Maybe Regex
    , lexErrorUnit      :: Maybe RegexUnit
      -- ^ the regex unit that caused the failure.
    , lexErrorType      :: Maybe TypeRep
      -- ^ the data type that would have been constructed had the parse succeeded.
    , lexErrorText      :: Maybe StrictText
      -- ^ the text parsed just before the unexpected characters were found.
    , lexErrorLocation  :: Maybe TextPoint
      -- ^ optionally set the location of the error
    , lexErrorMessage   :: Maybe StrictText
      -- ^ this value is set with the string parameter passed to
      -- @('Control.Monad.fail' :: 'Lexer' t)@.
    }
  deriving (Eq, Ord, Show, Typeable)

instance PPrintable LexerError where
  pPrint p = let ml o f = maybe [] f o in return $ pSpan (toText "LexerError") $ concat
    [ [pText "lexer error:"]
    , ml (lexErrorText p) $ \o -> let t = Strict.take 10 o in
        pShow t : (guard (Strict.length t < Strict.length o) >> [pShow t, pText "...", PSpace])
    , ml (lexErrorMessage p) $ \o -> [pText o, PNewLine]
    , return $ pIndent $ concat
        [ ml (lexErrorType p) $ \o ->
            [pText "while lexing data of type", pSpace, pShow o]
        , ml (lexError     p) $ \o -> concat
            [ [pText "should have matched regular expression:", pSpace], pPrint o,
              [PNewLine]
            ]
        , ml (lexErrorUnit p) $ \o -> return $ pIndent $
            [pText "(on this part:", pSpace] ++ pPrint o ++ [pText ")", PNewLine]
        ]
    ]

instance NFData LexerError where
  rnf (LexerError a b _ c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()

lexerError :: LexerError
lexerError = LexerError Nothing Nothing Nothing Nothing Nothing Nothing

----------------------------------------------------------------------------------------------------

type CharSet = Iv.Set Char

-- | A 'CharTable' is a 'CharArray' containing lex functions, which are functions of the
-- type:
--
-- > 'Prelude.String' -> [(a, 'Prelude.String')]
--
-- A 'CharTable' is useful for constructing lex functions using 'makeLex'.
type CharTable a  = Sa.SparseArray Char a

-- | A count of how many characters have been read during parsing.
type CharCount    = Count

-- | A count of how many columns have been parsed since the start of the last line.
type ColumnNumber = Count

-- | A count of how many lines have been encounted since the start of parsing.
type LineNumber   = Count

pPrintCharSet :: CharSet -> [PPrint]
pPrintCharSet cs =
  let ch c = pText $ case c of
          '-'  -> "\\-"
          '~'  -> "\\~"
          '\\' -> "\\\\"
          '['  -> "\\["
          ']'  -> "\\]"
          '('  -> "\\("
          ')'  -> "\\)"
          c    -> [c]
      prin o = case o of
        (Iv.Finite a, Iv.Finite b) | a==b -> [ch a]
        (Iv.Finite a, Iv.Finite b) -> [pText "[", ch a, pText "-", ch b, pText "]"]
        (_          , Iv.Finite b) -> [pText "[-", pText [b,']']]
        (Iv.Finite a, _          ) -> [pText ['[',a], pText "-]"]
        (_          , _          ) -> [pText "."]
  in  case Iv.toPair <$> Iv.toList cs of
        []  -> [pText "[]"]
        [o] -> prin o
        ox  -> [pText "[", pInline (ox>>=prin), pText "]"]

----------------------------------------------------------------------------------------------------

-- | A 'LineNumber' and 'ColumnNumber' useful for error reporting.
data TextPoint = TextPoint{ lineNumber :: LineNumber, columnNumber :: ColumnNumber }
  deriving (Eq, Ord, Typeable)

instance PPrintable TextPoint where
  pPrint p = [pShow $ lineNumber p, pText ":", pShow $ columnNumber p]

instance Show TextPoint where { show (TextPoint num col) = show num ++ ':' : show col }

instance NFData TextPoint where
  rnf (TextPoint a b) = deepseq a $! deepseq b ()

----------------------------------------------------------------------------------------------------

-- | A class defining a single function for data types which store tokens or source location
-- information where it is possible to clear the information to save space, without changing the
-- meaning of the data. This is true of the 'Space' and 'LineSpace' data types which keep the space
-- characters until you clear them, and for the 'TextRegion' data type which stores information
-- about locations of tokens. Although a text region is meaningless without the location
-- information, it is convenient in cases where the data type storing the location information is
-- still useful after the location information has been dicarded.
class DeleteContents o where { deleteContents :: o -> o }

----------------------------------------------------------------------------------------------------

-- | Two 'TextPoints' demarcing the start and end of some substring in the parsed text.
newtype TextRegion = TextRegion{ textRegionToPair :: Maybe (TextPoint, TextPoint) }
  deriving (Eq, Ord, Typeable)

instance Monoid TextRegion where
  mempty                                = TextRegion Nothing
  mappend (TextRegion a) (TextRegion b) = case a of
    Nothing               -> TextRegion b
    (Just (startA, endA)) -> case b of
      Nothing               -> TextRegion a
      (Just (startB, endB)) -> TextRegion $ Just (min startA startB, max endA endB)

instance Show TextRegion where
  show (TextRegion o) = case o of
    Nothing     -> ""
    Just (a, b) -> show a ++ '-' : show b

instance NFData TextRegion where
  rnf (TextRegion  Nothing     ) = ()
  rnf (TextRegion (Just (a, b))) = deepseq a $! deepseq b ()

instance DeleteContents TextRegion where { deleteContents _ = TextRegion Nothing; }

textRegion :: TextPoint -> TextPoint -> TextRegion
textRegion a b = TextRegion $ Just (min a b, max a b)

----------------------------------------------------------------------------------------------------

-- | This data type models an equation for computing the probability that a regular expression will
-- match a completely random input string. It is useful for ordering regular expressions.
data Probability
  = P_Coeff  Integer -- an integer value not taken to the power -1
  | P_Prob   Integer -- an integer value taken to the power -1
  | P_Fac    Integer -- an integer factorial
  | P_DivFac Integer -- an integer factorial taken to the power -1
  | P_Mult Probability Probability -- product of probabilities
  | P_Pow  Probability Integer -- a probability taken to an integer power.
  deriving Eq

instance Ord Probability where { compare a b = compare (computeLogProb a) (computeLogProb b) }

instance NFData Probability where
  rnf (P_Coeff  a  ) = deepseq a ()
  rnf (P_Prob   a  ) = deepseq a ()
  rnf (P_Fac    a  ) = deepseq a ()
  rnf (P_DivFac a  ) = deepseq a ()
  rnf (P_Mult   a b) = deepseq a $! deepseq b ()
  rnf (P_Pow    a b) = deepseq a $! deepseq b ()

-- | Compute the natural logarithm of a probability.
computeLogProb :: Probability -> Double
computeLogProb o = let fac o = if o<=1 then 0 else (o+1) * div o 2 in case o of
  P_Coeff  o -> log (fromRational (o % 1))
  P_Prob   o -> negate (log (fromRational (o % 1)))
  P_Fac    o -> fromRational (fac o % 1)
  P_DivFac o -> fromRational (1 % fac o)
  P_Mult a b -> computeLogProb a + computeLogProb b
  P_Pow  a b -> computeLogProb a * fromRational (b % 1)

-- | Compute a probability. *WARNING:* for most data types in this module, computing this function
-- produces *enormous* numbers. Use 'computeLogProb' instead.
computeProb :: Probability -> Rational
computeProb o = let fac o = if o<=1 then 1 else product [2..o] in case o of
  P_Coeff  o -> o % 1
  P_Prob   o -> 1 % o
  P_Fac    o -> fac o % 1
  P_DivFac o -> 1 % fac o
  P_Mult a b -> computeProb a * computeProb b
  P_Pow  a b -> computeProb a ^ b

-- | The probability that a single character will match 
probChar :: Probability
probChar = P_Prob $ toInteger (ord maxBound) - toInteger (ord minBound)

-- | The probability that a 'CharSet' will match the beginning of a completely random string. It is
-- the value @'Prelude.log' s + 'invP_Char'@, where @s@ is the size of the set.
probSet :: CharSet -> Probability
probSet s = P_Mult (P_Coeff $ toInteger $ Iv.size $ ord <$> s) probChar

-- | The probability that a string will match the beginning of a completely random string. It is the
-- probability of 'logP_Char' taken to the @n@th power where @n@ is the length of the string.
probText :: StrictText -> Probability
probText t = P_Pow probChar (toInteger $ Strict.length t)

----------------------------------------------------------------------------------------------------

-- | This class defines a predicate over 'RegexUnit's, 'Regex's, and 'Lexer's which asks, if the
-- left and then the right regular expressiosn were applied in series, and both would the combined
-- regular expression always fail? That is if you applied the left-hand regular expression and it
-- succeds but consumes all characters that the right-hand regular expression would have matched,
-- the combined regular expression will always fail.
--
-- For example, the regulare expression @/(abc){1}/@ matches the string @"abc"@ one or more
-- (possibly infinite) number of times. If it were followed by the right-hand regulare expression
-- @/(abc){2}/@ (matching @"abc"@ at least two times, and possibly infinite many times), then the
-- right-hand will never succeed because the left-hand regulare expression has certainly consumed
-- all characters that may have matched the right-hand. So the @/(abc){1}/@ shadows @/(abc){2}/@
-- when they are applied in series.
-- 
-- But what about if left and right were reversed? If @/(abc){2}/@ were on the left and @/(abc){1}/@
-- were on the right, well @/(abc){2}/@ will always fail for the input @"abc"@, and we know that
-- @/(abc){1}/@ would have matched but will never have a chance to because the @/(abc){2}/@ will
-- have failed before having tried it. This is an example of mutually shadowing regulare
-- expressiones: no matter which order the two are tried, applying both regulare expressiones in
-- series is guaranteed to never allow the second to be tried before failure occurs.
--
-- Another example is when the left regulare expression is the character set @/[a-z]+/@ (matches or
-- more lower-case letters), and the right regulare expression is the string @/(abcdef)/@. In this
-- case, the left shadows the right and this predicate will evaluate to 'Prelude.True'. However if
-- they were flipped this predicate evaluates to 'Prelude.False' because, @/(abcdef)/@ could match
-- and then be followed by any number of lower-case letters which would be matched by @/[a-z]+/@.
class ShadowsSeries o where { shadowsSeries :: o -> o -> Bool }

-- Only in the case of 'CharSet's is 'shadowsSeries' completely equivalent to 'shadowsParallel'
instance ShadowsSeries CharSet where { shadowsSeries = shadowsParallel }

-- | This class defines a predicate over 'RegexUnit's, 'Regex's, and 'Lexer's which asks, if
-- first the left regulare expression and then the right regex were both applied to the same input
-- string (in parallel, so they both get the same input), then is it true that the the left regulare
-- expression will match any input string that the right would match? If the two regulare
-- expressions were used to construct a grammar where the right-hand regular expression is only
-- tried in the case that the left-hand regex does not match, would the left always succeed and the
-- right would never have a chance of parsing? In other words, does the left regulare expression
-- shadow the right regex when they are applied in parallel?
-- 
-- For example, when a shorter string regulare expression is a prefix string of a longer string
-- regex, @/(abc)/@ and @/(abcd)@/. If the left were tried first, it will succeed for any string
-- that would have matched @/(abcd)/@, so the regulare expression @/(abcd)/@ would never have a
-- chance of succeeding, so the left shadows the right. If the regulare expressiones were reversed,
-- the input string @"abce"@ would not match @/(abcd)/@ but it would match @/(abc)/@, so if
-- @/(abcd)/@ were on the left, it would not be shadowing the regulare expression @/(abc)/@.
--
-- In another example, the regulare expression @/[abc]+/@ (matches one or more of the characters
-- @'a'@, @'b'@, or @'c'@) and @/[cde]+/@, these regulare expressions are mutually non-shadowing.
-- Although they both have in common the character @'c'@, and a string like @"ccccccc"@ will match
-- both (thus in the event we receive that input string, the left regulare expression will shadow
-- the right regex), this is not a case of shadowing because shadowing does not occur for all
-- possible input strings that could match the left regulare expression. A regex only shadows in
-- parallel if there is a 100% chance any string that matches the left regulare expression will do
-- so such that the right regex could never have a chance of being matched when converted to a
-- parser.
class ShadowsParallel o where { shadowsParallel :: o -> o -> Bool }

instance ShadowsParallel CharSet where { shadowsParallel a b = Iv.null $ Iv.delete b a }

-- | This class defines a function that returns the interval 'Dao.Interval.Set' of characters for a
-- regular expression that an input string could start with that might match the whole regular
-- expression. For string regular expressions like @/(abc)/@, this function will evaluate to a
-- 'Dao.Interval.Set' containing a single character @'a'@. For character set regular expressions
-- like @/[abc]/@, this function will evaluate to a 'Dao.Interval.Set' containing all of the
-- characters @'a'@, @'b'@, and @'c'@.
class InitialCharSetOf o where { initialCharSetOf :: o -> CharSet }

instance InitialCharSetOf CharSet where { initialCharSetOf = id }

-- | The minimum length string that could possbly match the 'RegexUnit'. Strings shorter than
-- this value will definitely never match the 'RegexUnit'.
class RxMinLength o where { rxMinLength :: o -> Integer }

-- | The maximum length string that could possbly match the 'RegexUnit'. A 'RegexUnit' will never
-- match more characters than this value, but could actually match fewer than this value.
-- 'Prelude.Nothing' indicates that it could match an infinite length string.
class RxMaxLength o where { rxMaxLength :: o -> Maybe Integer }

----------------------------------------------------------------------------------------------------

-- | 'RegexRepeater's define whether a regex is applied just once, or applied many times. An
-- 'Dao.Interval.Interval' which may or may not be bounded. 'RegexRepeater's are ordered first by
-- the minimum number of characters that it could match, and then by the maximum number of
-- characters that it could match.
--
-- 'Lexer' constructors like 'rxString', 'rxAnyOf', and 'rxChar' all require a 'RegexRepeater'
-- parameter, and this parameter can be satisfied by any one of the data types that instantiate
-- 'ToRegexRepeater', please refer to the documentation for 'ToRegexRepeater' for more about how to
-- construct this data type.
data RegexRepeater = RxSingle | RxRepeat Count (Maybe Count) deriving (Eq, Ord, Show, Typeable)

instance PPrintable RegexRepeater where
  pPrint o = case o of
    RxSingle            -> []
    RxRepeat 0 Nothing  -> [pInline [pText "*"]]
    RxRepeat 1 Nothing  -> [pInline [pText "+"]]
    RxRepeat n Nothing  -> [pInline [pText "{", pShow n, pText "}"]]
    RxRepeat n (Just o) -> [pInline [pText "{", pShow n, pText ",", pShow o, pText "}"]]

instance TestNull RegexRepeater where
  testNull (RxRepeat 0 (Just 0)) = True
  testNull _                     = False
  nullValue = RxRepeat 0 (Just 0)

instance NFData RegexRepeater where
  rnf  RxSingle      = ()
  rnf (RxRepeat a b) = deepseq a $! deepseq b ()

-- | This class defines the various data types that can be used to construct a 'RegexRepeater'. The
-- following instances are available:
--
-- @'Dao.Count.Count' :: @ will construct a repeater that only succeeds if the exactly the number of
-- repetitions are matched.
--
-- @('Dao.Count.Count', 'Dao.Count.Count') :: @ will construct a repeater that succeeds only if the
-- mimimum number of repetitions given by the 'Prelude.fst' paremeter can be matched, but will match
-- as many as the maximum number of repetitions given by the 'Prelude.snd' parameter.
-- 
-- @'RepeatMinMax' :: @ Please refer to the documentation for 'RepeatMinMax' below for more on how
-- this works.
--
-- @('Dao.Count.Count', 'Prelude.Maybe' 'Dao.Count.Count) :: @ -- this is the inverse of
-- 'lexRepeaterToPair'. Please refer to the documentation 'lexRepeaterToPair' for more on how this
-- works.
class ToRegexRepeater o where { toRegexRepeater :: o -> RegexRepeater }

-- | This data type instantiates 'ToRegexRepeater', and lets you construct either of two
-- 'RegexRepeater's:
data RepeatMinMax
  = MIN Count
    -- ^ Construct a 'RegexRepeater' that will only succeed if the minimum number of repetitions is
    -- matched, but once that minimum number is matched, it matches input greedily, taking as mutch
    -- matching input as it can.
  | MAX Count
    -- ^ Construct a 'RegexRepeater' that has no mimum number of matches (and therefore never
    -- fails), but will never match more than the maximum number of repetitions.

instance ToRegexRepeater Count where
  toRegexRepeater o = if o==1 then RxSingle else RxRepeat (max 0 o) (Just $ max 0 o)

instance ToRegexRepeater (Count, Count) where
  toRegexRepeater (lo, hi) = if lo==1 && hi==1 then RxSingle else RxRepeat lo (Just hi)

instance ToRegexRepeater RepeatMinMax where
  toRegexRepeater o = case o of
    MAX o -> RxRepeat 0 (Just $ max 0 o)
    MIN o -> RxRepeat o Nothing

instance ToRegexRepeater (Count, Maybe Count) where
  toRegexRepeater (lo', hi) = let lo = min 0 lo' in
    if lo==1 && hi==Just 1 then RxSingle else RxRepeat lo (min lo <$> hi)
 
-- | Convert the 'RegexRepeater' to a pair of values: the 'Prelude.fst' value is the minimum number
-- of times a regular expression must match the input string in order for the match to succeed. The
-- 'Prelude.snd' value is the maximum number of times the regular expression may match the input
-- string, or 'Prelude.Nothing' if there is no maximum. Once the minimum number of matches has been
-- satisfied, the regular expression will never fail to match, therefore even if the input string
-- matches more than the given maximum number of repetitions, only the maximum number of repetitions
-- will be matched and the remainder of the input string will be left untouched.
lexRepeaterToPair :: RegexRepeater -> (Count, Maybe Count)
lexRepeaterToPair o = case o of
  RxSingle       -> (1, Just 1)
  RxRepeat lo hi -> (lo, hi)

-- | The probability of a 'RegexRepeater' is a function on a probability value, where the function is:
--
-- > \r p -> 'P_Pow' p (('Prelude.fst' . 'lexRepeaterToPair') r)
--
probRepeater :: RegexRepeater -> Probability -> Probability
probRepeater r p = P_Pow p $ toInteger $ (fst . lexRepeaterToPair) r

----------------------------------------------------------------------------------------------------

-- | 'RegexUnit's define the simplest regular expression from which more complex regular expression
-- can be built. the 'Regex' is a more complex regular expression composed of 'RegexUnit's. There
-- are only two types of unit regex: a string and a character set.
--
-- A string 'RegexUnit' defines a string constant that may be a substring of the start of the input.
-- For example, the 'RegexUnit' string @/(abc)/@ matches the inputs @"abcdef"@ and @"abc"@, but does
-- not match @"ab"@, @"abddef"@, or @"zabc"@.
--
-- A character set 'RegexUnit' defines a set of characters where the start of the input string may
-- be elements of the set. For example, the 'RegexUnit' @/[AEIOUaeiou]/@ is the set of vowels and
-- will match any input string that starts with a vowel.
--
-- Both string and character set 'RegexUnit's can specify a minimum and maxiumum number of
-- repetitions. For example the string 'RegexUnit' @/(abc){2,4}/@ will match the string "abc"
-- between 2 and 4 times. So it will match the strings @"abcxyz"@ (2 repeitions), @"abcabcxyz"@ (3
-- repetitions), and @"abcabcabcabcxyz"@ (4 repetitions). It will ALSO match @"abcabcabcabcabc"@ (5
-- repetitions), however only the first 4 repetitions will match, the fifth repetition will be
-- available for other regular expressions to possibly match.  It is also possible to construct the
-- regex @/(abc){2}/@ which specifies an infinite maximum bound -- at least two repetitions must
-- match, and any number of repetitions after that.
--
-- A null regular expression always matches the start of the input string as every string begins
-- with an implicit null string.
data RegexUnit
  = RxString{ rxGetString  :: StrictText, rxGetRepeater :: RegexRepeater }
  | RxChar  { rxGetCharSet :: CharSet,    rxGetRepeater :: RegexRepeater }
  deriving (Eq, Show, Typeable)

instance TestNull RegexUnit where
  nullValue = RxString nullValue nullValue
  testNull o = case o of
    -- If either the repeater OR the string/charsets are null the whole RegexUnit is null.
    RxString t o -> testNull o || testNull t
    RxChar   t o -> testNull o || testNull t

instance Ord RegexUnit where
  compare a b = compare (computeLogProb $ probRegexUnit a) (computeLogProb $ probRegexUnit b)

instance NFData RegexUnit where
  rnf (RxString a b) = deepseq a $! deepseq b ()
  rnf (RxChar   a b) = deepseq a $! deepseq b ()

instance InitialCharSetOf RegexUnit where
  initialCharSetOf o = case o of
    RxString o _ -> if Strict.null o then Iv.empty else Iv.singleton $ Strict.head o
    RxChar   o _ -> o

instance RxMinLength RegexUnit where
  rxMinLength o = toInteger ((fst . lexRepeaterToPair) $ rxGetRepeater o) * case o of
    RxString o _ -> toInteger (Strict.length o)
    RxChar   _ _ -> 1

instance RxMaxLength RegexUnit where
  rxMaxLength o = (snd . lexRepeaterToPair) (rxGetRepeater o) >>= \m -> Just $ toInteger m * case o of
    RxString o _ -> toInteger (Strict.length o)
    RxChar   _ _ -> 1

instance ShadowsSeries RegexUnit where
  shadowsSeries a b =
    let bnds = lexRepeaterToPair . rxGetRepeater
        (_, hiA) = bnds a
    in  case a of
          RxString tA _ -> case b of
            RxString tB _ -> isNothing hiA && tA==tB
            RxChar   _  _ -> False
          RxChar   tA _ -> isNothing hiA && case b of
            RxString tB _ -> Iv.null $ Iv.delete (Iv.fromPoints $ Strict.unpack tB) tA
            RxChar   tB _ -> shadowsSeries tA tB

instance ShadowsParallel RegexUnit where { shadowsParallel = _rxUnit_shadowsParallel True }

instance PPrintable RegexUnit where
  pPrint o =
    let sanitize cx = cx >>= \c -> case c of
          '\\'          -> "[\\\\]"
          '"'           -> "[\\\"]"
          '.'           -> "[.]"
          '|'           -> "[|]"
          '^'           -> "[^]"
          '$'           -> "[$]"
          '['           -> "\\["
          ']'           -> "\\]"
          '('           -> "\\("
          ')'           -> "\\)"
          '{'           -> "\\{"
          '}'           -> "\\}"
          c | isPrint c -> [c]
          c             -> "\\x" ++ showHex (ord c) ""
    in  case o of
          RxString t r -> pText (sanitize $ fromText t) : pPrint r
          RxChar   c r -> pPrintCharSet c ++ pPrint r

-- Used by the instantiation of 'ShadowsParallel' for both 'RegexUnit' and 'Regex'.
_rxUnit_shadowsParallel :: Bool -> RegexUnit -> RegexUnit -> Bool
_rxUnit_shadowsParallel isFinalItem a b = case a of
  RxString tA opA -> case b of
    RxString tB opB -> compLo opA opB && if not isFinalItem then tA==tB else case opA of
      RxSingle -> case opB of
        RxSingle -> Strict.isPrefixOf tA tB
        _        -> False
      _        -> False
    _               -> False
  RxChar   tA opA -> case b of
    RxChar   tB opB -> Iv.null (Iv.delete tB tA) && compLo opA opB
    RxString tB _   -> rxMinLength a <= rxMinLength b &&
      Iv.null (Iv.delete (Iv.fromPoints (Strict.unpack tB)) tA)
  where
    compLo opA opB = 
      let (loA, _) = lexRepeaterToPair opA
          (loB, _) = lexRepeaterToPair opB
      in  loA<=loB

-- | The 'Probability' value that a given 'RegexUnit' will match a completely random input string.
probRegexUnit :: RegexUnit -> Probability
probRegexUnit o = if testNull o then P_Prob 1 else case o of
  RxString t op -> probRepeater op (probText t)
  RxChar   t op -> probRepeater op (probSet  t)

regexUnitToParser
  :: (MonadParser text m, MonadPlus m, Monoid text, TextBuilder text)
  => RegexUnit -> m text
regexUnitToParser o = do
  let nope :: (MonadParser text m, MonadPlus m, Monoid text, TextBuilder text) => text -> m x
      nope got = do
        pfail $
          lexerError
          { lexErrorUnit = Just o
          , lexErrorText = Just $ toText $ toLazyText got
          }
        fail $ concat
          [ "failed after text: ", show (toLazyText got)
          , "\nexpecting text matching regular expression: ", show o
          ]
  let optional f = mplus (liftM Just f) (return Nothing) -- redefined to avoid using Alternative
  let run get add op = do
        let (lo, hi) = lexRepeaterToPair op
            req i got = if i>=lo then return (i, got) else
              optional get >>= maybe (nope got) (req (succ i) . add got)
            opt hi i got = optional (guard (i<hi) >> get) >>=
              maybe (return got) (opt hi (succ i) . add got)
            inf got = optional get >>= maybe (return got) (inf . add got)
        (i, got) <- req 0 mempty
        maybe (inf got) (\hi -> opt hi i got) hi
  case o of
    RxString t rep -> run (string $ fromLazyText $ toLazyText t) mappend rep
    RxChar   t rep -> run (satisfy t) snocChar rep

----------------------------------------------------------------------------------------------------

-- | This error occurs when constructing a 'Lexer' or 'Grammar', and happens if there are ambiguities
-- in the specified Context Free Grammar. Whether or not a 'Lexer's is correct is checked
-- automatically. A 'Lexer' is correct only if two 'RegexUnit's constructing a larger 'RegexUnit'
-- part do not shadow each other. For example, if a regex parsing an infinite number of lower case
-- numbers preceeds a string containing only lower case characters, the first grammar will always
-- consume all input and the second will always fail.  During construction of the 'Grammar', these
-- cases are caught and reported as an error of this data type.
data InvalidGrammar
  = InvalidRegex
    { regexTypeA :: Maybe TypeRep  , regexTypeB :: Maybe TypeRep
    , regexUnitA :: Maybe RegexUnit, regexUnitB :: Maybe RegexUnit
    , regexSeqA  :: Maybe Regex    , regexSeqB  :: Maybe Regex
    , regexBadSequence :: Bool
      -- ^ 'True' means A is in sequence with B, 'False' means A was in parallel with B
    } -- ^ A Lexer is only ever invalid when one regex is shadowing another.
  | CutSomeBranches
    { regexTypeA  :: Maybe TypeRep
    , cutBranches :: [Regex]
    }
  | InvalidGrammar
    { regexTypeA            :: Maybe TypeRep
    , invalidGrammarMessage :: Strict.Text
    }
  deriving (Eq, Ord, Show, Typeable)

instance PPrintable InvalidGrammar where
  pPrint o = case o of
    InvalidRegex
      { regexTypeA=typA , regexTypeB=typB
      , regexUnitA=unitA, regexUnitB=unitB
      , regexBadSequence=isSeq
      } ->
        let prin defltMsg typ unit = pSpace : case (unit, typ) of
              (Nothing, Nothing) -> pSentence $ defltMsg++" regular expressions"
              _                  ->
                pSentence "the regular expression" ++ pSpace : maybe [] pPrint unit ++
                maybe [] (\typ -> pSpace : pSentence "for type" ++ [pSpace, pShow typ]) typ
        in  pText (if isSeq then "Sequencing" else "Parallelizing") : prin "earlier" typA unitA ++
            pText "guarantees" : pSpace : prin "later" typB unitB ++
              pSpace : pSentence "will never have a chance to be matched."
    CutSomeBranches
      { regexTypeA=typA
      , cutBranches=units
      } -> concat
        [ maybe []
            (\t -> pSentence "the regular expression for the type" ++
                    [pSpace, pShow t, pText "guarantees"])
            typA
        , pSentence "the following regular expressions will never match:"
        , [pIndent $ units >>= \rx -> pPrint rx ++ [pNewLine]]
        ]
    InvalidGrammar
      { regexTypeA=typA
      , invalidGrammarMessage=msg
      } ->
        maybe
          (pSentence "Some grammars were invalid:")
          (\t -> pSentence "The grammar for the type" ++
                  pSpace : pShow t : pSpace : pSentence "is invalid:")
          typA
        ++ [pNewLine, pText msg]

instance Exception InvalidGrammar where {}

instance NFData InvalidGrammar where
  rnf (InvalidRegex a b c d e f g) =
    let dseq a () = seq a $! maybe () (`seq` ()) a
    in  dseq a $! dseq b $! deepseq c $! deepseq d $! deepseq e $! deepseq f $! deepseq g ()
  rnf (CutSomeBranches _ _) = ()
  rnf (InvalidGrammar  _ _) = ()

invalidGrammar :: InvalidGrammar
invalidGrammar = InvalidRegex Nothing Nothing Nothing Nothing Nothing Nothing False

_invalidSetType :: TypeRep -> InvalidGrammar -> InvalidGrammar
_invalidSetType typ err = 
    let goodType typ = and $ (typ /=) <$> -- we like types more specific than () and String
          [typeOf (), typeOf (mempty::LazyText), typeOf (mempty::StrictText), typeOf ""]
        setType getFrom err t = maybe (return t) return $
          getFrom err >>= \t -> guard (goodType t) >> return t
    in  err{ regexTypeA=setType regexTypeA err typ, regexTypeB=setType regexTypeB err typ }

-- | This function will check if the @thing@ (the 'Grammar' or 'Lexer') is determined to be an
-- 'InvalidGrammar', and will update the type information in the error message.
class TypeableGrammar thing where { informType :: Typeable o => thing o -> thing o; }

----------------------------------------------------------------------------------------------------

-- | A sequence of 'RegexUnit's for building more complex regular expressions patterns that can be
-- used to match strings. This data type instantiates 'Prelude.Ord' in the same way that 'RegexUnit'
-- instantiates 'Prelude.Ord', that is, if both 'Regex' values are applied to the same input string,
-- the first 'Regex' is greater than the second if the first consumes all or more of the input
-- string that the second will consume.
--
-- A 'Regex' is the building block of a 'Lexer'.
newtype Regex = Regex (Array RegexUnit) deriving (Eq, Show, Typeable)

instance PPrintable Regex where
  pPrint (Regex o) = case elems o of
    [] -> [pText "\"\""]
    ox -> [pInline $ [pText "\""] ++ (ox>>=pPrint) ++ [pText "\""]]

instance Ord Regex where
  compare a b = compare (computeLogProb $ probRegex a) (computeLogProb $ probRegex b)

instance Monoid (Either InvalidGrammar Regex) where
  mempty = Right $ Regex mempty
  mappend a@(Left _) _ = a
  mappend _ b@(Left _) = b
  mappend (Right seqA@(Regex a)) (Right seqB@(Regex b)) = fromMaybe (Right _emptyRegexSeq) $ msum
    [do unitA <- lastElem a -- If a is empty, we stop here.
        unitB <- b!0 -- If b is empty, we stop here.
        -- a and b are not empty, we can safely assume at least one element can be indexed
        let items arr = concat . fmap (maybe [] return . (arr !))
        Just $ case _rxSeq unitA unitB of
          Right rxAB -> Right $ Regex $ array $ concat
            [items a [0 .. size a - 1], maybe [unitA, unitB] return rxAB, items b [1 .. size b]]
          Left  err  -> Left $ case err of
            InvalidRegex{} -> err{ regexSeqA=Just seqA, regexSeqB=Just seqB }
            err            -> err
    , lastElem a >> Just (Right $ Regex a)
    , b ! 0 >> Just (Right $ Regex b)
    ]

instance NFData Regex where { rnf (Regex a) = deepseq a (); }

instance ShadowsParallel Regex where
  shadowsParallel (Regex a) (Regex b) = loop (elems a) (elems b) where
    loop ax bx = case ax of
      []   -> True
      a:ax -> case bx of
        []   -> False
        b:bx -> _rxUnit_shadowsParallel (null ax) a b && loop ax bx

instance ShadowsSeries Regex where
  shadowsSeries (Regex a) (Regex b) = fromMaybe False $
   lastElem a >>= \rxA -> b ! 0 >>= \rxB -> Just $ shadowsSeries rxA rxB

instance TestNull Regex where
  nullValue = Regex nullValue
  testNull (Regex a) = testNull a

instance RxMinLength Regex where
  rxMinLength (Regex o) = sum $ rxMinLength <$> elems o

instance RxMaxLength Regex where
  rxMaxLength (Regex o) = fmap sum $ sequence $ rxMaxLength <$> elems o

instance InitialCharSetOf Regex where
  initialCharSetOf (Regex o) = maybe Iv.empty initialCharSetOf (o ! 0)

_emptyRegexSeq :: Regex
_emptyRegexSeq = Regex mempty

-- | Returns the 'Probability' value a 'Regex' will match a completely random input string.
probRegex :: Regex -> Probability
probRegex (Regex o) = foldl (\p -> P_Mult p . probRegexUnit) (P_Prob 1) (elems o)

-- Tries to merge two 'RegexUnit's together on the assumption that each 'RegexUnit' will be appended
-- in series. This merges non-repeating strings, identical strings that repeat, and any two
-- character sets as long as the latter character set is a subset of the former and there are no
-- maximum repeition limitations.
_rxSeq :: RegexUnit -> RegexUnit -> Either InvalidGrammar (Maybe RegexUnit)
_rxSeq a b = case a of
  RxString tA RxSingle -> case b of
    RxString tB RxSingle -> return $ Just $ RxString (tA<>tB) RxSingle
    _                    -> if not (shadowsSeries a b) then return Nothing else
      Left $ invalidGrammar{ regexUnitA=Just a, regexUnitB=Just b, regexBadSequence=True }
  _                      -> return Nothing

-- | Construct a 'Regex' from a list of 'RegexUnit'.
lexRegexUnits :: [RegexUnit] -> Either InvalidGrammar Regex
lexRegexUnits = loop [] where
  loop stk ox = case ox of
    []     -> Right $ Regex $ array stk
    [o]    -> Right $ Regex $ array (stk++[o])
    a:b:ox -> _rxSeq a b >>= \ab -> case ab of
      Just ab -> loop stk (ab:ox)
      Nothing -> loop (stk++[a]) (b:ox)

-- | This function converts a 'Regex' to a 'MonadParser' using the various methods of the
-- 'MonadParser' class, for example 'munch', 'string', and 'noMoreThan'.
--
-- If you are rolling your own 'MonadParser' rather than using one of the many excellent monadic
-- parsers available to you, it is safe to use this function to instantiate the 'regex' method; this
-- function does not make use of the 'regex' function. In fact, the default instantiation for
-- 'regex' uses this function, so you do not need to instantiate 'regex' at all.
regexToParser
  :: (MonadPlus m, MonadParser text m, Monoid text, TextBuilder text)
  => Regex -> m text
regexToParser rx@(Regex sq) = loop (elems sq) mempty where
  loop ox t = case ox of
    []   -> return t
    o:ox -> mplus (regexUnitToParser o) $
      (do pfail $ lexerError{ lexError=Just rx, lexErrorUnit=Just o, lexErrorText=Just $ toText $ toLazyText t }
          fail $ concat
            ["failed after text: ", show (toLazyText t)
            , "\nwas expecting text matching: ", show o
            ]
      ) >>= loop ox . mappend t

_rxPrim :: ToRegexRepeater rep => (o -> RegexRepeater -> RegexUnit) -> rep -> o -> Regex
_rxPrim f rep o = let unit = f o (toRegexRepeater rep) in
  if testNull unit then Regex mempty else Regex $ array [unit]

-- | This primitive 'Regex' matches a single character.
rxChar :: ToRegexRepeater rep => rep -> Char -> Regex
rxChar = _rxPrim $ RxChar . Iv.singleton

-- | This primitive 'Regex' matches any characters in a 'Dao.Interval.Set' of 'Prelude.Char's taken
-- from the string object containing them. The type of the string object can be of anything
-- instantiates 'Dao.Text.Builder.TextBuilder', including 'Prelude.String'.
rxAnyOf :: (ToRegexRepeater rep, TextBuilder str) => rep -> str -> Regex
rxAnyOf = _rxPrim $ RxChar . Iv.fromPoints . unpack

-- | This is the inverse of the 'rxAnyOf' 'Regex'. Any characters not included in the list of
-- provided characters will match this 'Regex'.
rxNoneOf :: (ToRegexRepeater rep, TextBuilder str) => rep -> str -> Regex
rxNoneOf = _rxPrim $ RxChar . Iv.invert . Iv.fromPoints . unpack

-- | Creates a character set from a list of intervals, each interval expressed as a pair of
-- characters. For example, the set of alphanumeric characters could be expressed as:
--
-- > 'rxRanges' ('MIN' 1) [('0', '9'), ('A', 'Z'), ('a', 'z')]
--
rxRanges :: ToRegexRepeater rep => rep -> [(Char, Char)] -> Regex
rxRanges = _rxPrim $ RxChar . Iv.fromPairs

-- | This is the inverse of the 'rxRanges' 'Regex'. Any characters not included any of the
-- list of character intervals will match this 'Regex'.
rxNotRanges :: ToRegexRepeater rep => rep -> [(Char, Char)] -> Regex
rxNotRanges = _rxPrim $ RxChar . Iv.invert . Iv.fromPairs

-- | This is a 'Regex' that matches any and all characters.
rxAny1 :: ToRegexRepeater rep => rep -> Regex
rxAny1 = Regex . array . return . RxChar Iv.whole . toRegexRepeater

-- | This primitive 'Regex' matches a string of characters. The type of the string object can be of
-- anything instantiates 'Dao.Text.Builder.TextBuilder', including 'Prelude.String'.
rxString :: (ToRegexRepeater rep, TextBuilder str) => rep -> str -> Regex
rxString = _rxPrim $ RxString . toText . toLazyText

-- | Create a 'Regex' from any 'Dao.Interval.Set' of 'Prelude.Char's.
rxCharSet :: ToRegexRepeater rep => rep -> CharSet -> Regex
rxCharSet = _rxPrim RxChar

----------------------------------------------------------------------------------------------------

-- | A 'Lexer' is the fundamental building block of a 'Grammar'.
--
-- It is a monadic data type that you can use to construct lexical analyzers. There are only two
-- constructors: 'lexer' and 'lexConst', both of which take zero or more 'Regex's that can convert
-- the string matching the 'Regex' to a value. More complicated 'Lexer's can be constructed by
-- function composition using 'Control.Monad.Monad', 'Control.Monad.MonadPlus',
-- 'Control.Applicative.Applicative', 'Control.Applicative.Alternative', and 'Data.Monoid.Monoid'.
--
-- 'Regex's that create ambiguous patterns will result in invalid 'Lexer's that throw
-- 'InvalidGrammar' when evaluated.
data Lexer o
  = LxIden                       o  -- The identity monad.
  | LxStep     Regex      (Lexer o) -- Match a regex, buffer matching text.
  | LxTake    (LazyText -> Lexer o) -- Empty the buffer into a function.
  | LxFail   StrictText             -- Throw a parser error.
  | LxReject InvalidGrammar         -- The constructed Lexer is invalid.
  deriving Typeable

instance Monoid o => Monoid (Lexer o) where
  mempty      = return mempty
  mappend a b = mappend <$> a <*> b

instance NFData o => NFData (Lexer o) where
  rnf (LxIden   a  ) = deepseq a ()
  rnf (LxTake     _) = ()
  rnf (LxStep   a _) = deepseq a ()
  rnf (LxFail   a  ) = deepseq a ()
  rnf (LxReject a  ) = deepseq a ()

instance Functor Lexer where
  fmap f o = case o of
    LxIden    o  -> LxIden    $ f o
    LxStep rx o  -> LxStep rx $ fmap f o
    LxTake    o  -> LxTake    $ fmap (fmap f) o
    LxFail   msg -> LxFail   msg
    LxReject err -> LxReject err

instance Monad Lexer where
  return      = LxIden
  fail        = LxFail . Strict.pack
  oA >>= bind = case oA of
    LxIden     oA -> bind oA
    LxStep rxA oA -> step bind rxA oA
    LxTake     oA -> LxTake $ oA >=> bind
    LxFail   msg  -> LxFail   msg
    LxReject err  -> LxReject err
    where 
      -- plus: evaluate to the @next@ Lexer only if the two 'Regex's do not mask each other.
      plus :: Regex -> Regex -> (Regex -> Lexer o) -> Lexer o
      plus rxA rxB next = case Right rxA <> Right rxB of
        Right rxC -> next rxC
        Left  err -> LxReject err
      -- check: checks that 'LxTake's do not mask each other or other 'LxStep's.
      check :: Regex -> (LazyText -> Lexer o) -> Lexer o
      check rxA oB = LxTake $ \txt -> case oB txt of
        LxIden     oB -> LxIden oB
        LxStep rxB oB -> plus rxA rxB $ \ _ -> LxStep rxB oB
        LxTake     oB -> check rxA oB
        LxFail    msg -> LxFail   msg
        LxReject  err -> LxReject err
      -- reduce: makes sure consecutive 'LxStep's are reduced to a single 'LxStep'
      reduce :: Lexer o -> Regex -> Lexer o
      reduce oB rxA = case oB of 
        LxIden     oB -> LxStep rxA $ LxIden oB
        LxStep rxB oB -> plus rxA rxB $ reduce oB
        LxTake     oB -> check rxA oB
        LxFail    msg -> LxFail   msg
        LxReject  err -> LxReject err
      -- step: if the current 'Lexer' is a 'LxStep', reduce it and then bind it.
      step :: (a -> Lexer b) -> Regex -> Lexer a -> Lexer b
      step bind rxA oA = case oA of
        LxIden     oA -> case bind oA of
          LxIden     oB -> LxStep rxA $ LxIden oB
          LxStep rxB oB -> plus rxA rxB $ reduce oB
          LxTake     oB -> LxStep rxA $ check rxA oB
          LxFail    msg -> LxFail   msg
          LxReject  err -> LxReject err
        LxStep rxB oA -> plus rxA rxB $ \rxC -> step bind rxC oA
        LxTake     oA -> LxStep rxA $ check rxA $ oA >=> bind
        LxFail    msg -> LxFail   msg
        LxReject  err -> LxReject err

instance Applicative Lexer where { pure = return; (<*>) = ap; }

instance ShadowsParallel (Lexer o) where
  shadowsParallel a b = case a of
    LxIden     _ -> False
    LxStep rxA _ -> case b of
      LxIden     _ -> False
      LxStep rxB _ -> shadowsParallel rxA rxB
      LxTake     _ -> False
      LxFail     _ -> False
      LxReject   _ -> False
    LxTake     _ -> False
    LxFail     _ -> False
    LxReject   _ -> False

instance TypeableGrammar Lexer where
  informType o = case o of
    LxReject err -> LxReject $ _invalidSetType (typeOfLexer o) err
    _               -> o

-- | Defines a class of types that have 'Lexer's defined for them, similar to how many data types
-- may instantiate 'Prelude.Read'.
class HasLexer o where { theLexer :: Lexer o }

-- | Convert a 'Lexer' to a 'MonadParser' that actually parses things.
lexerToParser :: (Monad m, MonadParser text m, TextBuilder text) => Lexer o -> m o
lexerToParser o =
  let nope buf rx msg = do
        let err = lexerError{lexErrorMessage=Just msg, lexError=rx, lexErrorText=buf}
        pfail err >> fail ("Lexer error: "++show err)
  in  case o of
        LxIden    o -> return o
        LxStep rx o -> loop mempty rx o where
          loop buf rx o = liftM ((buf <>) . toLazyText) (regex rx) >>= \buf -> case o of
            LxIden    o  -> return o
            LxStep rx o  -> loop buf rx o
            LxTake    o  -> lexerToParser $ o buf
            LxFail   msg -> nope (Just $ toText $ toLazyText buf) (Just rx) msg
            LxReject err -> throw err
        LxTake    o  -> lexerToParser $ o mempty
        LxFail   msg -> nope Nothing Nothing msg
        LxReject err -> throw err

-- | Get the 'Regex' that defines this 'Lexer'. Note that if your 'Lexer's was constructed by
-- 'Data.Monoid.mappend'-ing a two 'Regex's, then only the 'Regex' from the left-hand argument can
-- be retrieved. The right-hand regex cannot be retrieved without first evaluating the left-hand
-- 'Regex' against a string.
regexOfLexer :: Lexer t -> Regex
regexOfLexer o = case o of
  LxIden    _ -> Regex mempty
  LxStep rx _ -> rx
  LxTake    _ -> Regex mempty
  LxFail    _ -> Regex mempty
  LxReject  _ -> Regex mempty

-- | Return the type to which this 'Lexer' will evaluate.
typeOfLexer :: Typeable o => Lexer o -> TypeRep
typeOfLexer = head . snd . splitTyConApp . typeOf

-- | Construct a 'Lexer' that uses a 'Regex' to match text, and then uses that text to construct a
-- data type. This function takes a data type constructor and a regular expression.
lexer :: Typeable t => (LazyText -> t) -> [Regex] -> Lexer t
lexer constr rx = case mconcat $ fmap Right rx of
  Right rx -> LxStep rx $ LxTake $ return . constr
  Left err -> LxReject $ _invalidSetType (typeOfLexer $ lexer constr rx) err

-- | This is the null token, or a token that discards the characters matched by the 'Lexer' and
-- simply returns a constant value. This 'Lexer' is defined as the equation:
--
-- > \o -> 'lexer' ('Prelude.const' o)
--
lexConst :: Typeable t => t -> [Regex] -> Lexer t
lexConst t = lexer (const t)

----------------------------------------------------------------------------------------------------

-- | This data type is a version of 'Lexer' that can be inspected with @case@ statements. A 'Lexer'
-- can be converted to a 'LexerSignature' but not vice-versa. 'LexerSignature' instantiates
-- 'Prelude.Eq', so you can test if two 'Lexer's are somewhat equivalent by comparing their
-- 'LexerSignature's. 'LexerSignature' also instantiates 'Prelude.Show' so you can visualize it.
data LexerSignature o
  = LexerReturn o
    -- ^ The lexer that does nothing and returns a constant value.
  | LexerRegex  Regex (LexerSignature o)
    -- ^ The lexer that tries to match a 'Regex' to an input string, buffering the matched text,
    -- then behaves as another lexer.
  | LexerRead   (LazyText -> LexerSignature o)
    -- ^ The lexer that empties the buffer of matched text and uses it to either construct a value,
    -- or to match more text. This lexer takes a constructor that converts 'LazyText' to either a
    -- 'LexerReturn' value or another 'LexerRegex' which may use the buffered text before doing more
    -- matching.
  | LexerFail   StrictText
    -- ^ The valid lexer that is used when you know that text has been matched that should not have
    -- been input into this lexer.
  | LexerReject InvalidGrammar
    -- ^ The invalid lexer that is only constructed when two 'LexerRegex's are chained together
    -- where the first 'Regex' 'shadowsSeries's the second, i.e. the first 'Regex' consumes all
    -- characters making it impossible for the second 'Regex' to ever match.
  deriving Typeable

instance Functor LexerSignature where
  fmap f o = case o of
    LexerReturn   o -> LexerReturn  $ f o
    LexerRegex rx o -> LexerRegex rx $ fmap f o
    LexerRead     o -> LexerRead    $ fmap (fmap f) o
    LexerFail   err -> LexerFail   err
    LexerReject err -> LexerReject err

instance Eq o => Eq (LexerSignature o) where { (==) = lexerSignatureEq (==); }

instance PPrintable o => PPrintable (LexerSignature o) where
  pPrint = pprintLexerSignature pPrint

instance PPrintable o => Show (LexerSignature o) where
  show = Lazy.unpack . runTextPPrinter 2 80 . pPrint

-- | Construct a 'LexerSignature' from a 'Lexer'.
lexerSignature :: Lexer o -> LexerSignature o
lexerSignature o = let loop = lexerSignature in case o of
  LxIden      o -> LexerReturn o
  LxStep   rx o -> LexerRegex rx (loop o)
  LxTake      o -> LexerRead $ fmap loop o
  LxFail   err  -> LexerFail err
  LxReject err  -> LexerReject err

-- | Compare two 'LexerSignature's using an equality function provided for testing equality of the
-- polymorphic type @o@.
lexerSignatureEq :: (o -> o -> Bool) -> LexerSignature o -> LexerSignature o -> Bool
lexerSignatureEq eq a b = let loop = lexerSignatureEq eq in case (a, b) of
  (LexerReturn    a, LexerReturn    b) -> eq a b
  (LexerRegex rxA a, LexerRegex rxB b) -> rxA==rxB && loop a b
  (LexerRead      _, LexerRead      _) -> True -- Cannot inspect the return value of a function.
  (LexerFail      a, LexerFail      b) -> a==b -- Yes, fail messages are compared as well.
  _                                    -> False -- Rejected lexers are never equivalent.

-- | Convert a 'LexerSignature' to a list of 'StrictText' strings using a @show@ function provided
-- for the polymorphic type @o@. The resulting list of 'StrictText's are intended to be concatenated
-- the with 'Data.Text.Lazy.unlines' function.
pprintLexerSignature :: (o -> [PPrint]) -> LexerSignature o -> [PPrint]
pprintLexerSignature txt o =
  let section hdr body = [pInline $ hdr ++ [PIndent body]]
      loop = pprintLexerSignature txt
  in  case o of
        LexerReturn   o -> section [pText "LexerReturn", pSpace] (txt o)
        LexerRegex rx o -> section (pText "LexerRegex" : pPrint rx ++ [pSpace, pText "$", pSpace]) (loop o)
        LexerRead     _ -> [pText "LexerRead ..."]
        LexerFail   err -> pText "LexerFail" : pPrint err
        LexerReject err -> pPrint err

----------------------------------------------------------------------------------------------------

-- | 'Grammar' is a data structure that can be converted to a monadic parser function of any type
-- that instantiates 'MonadParser', including 'Text.ParserCombinators.ReadP' and
-- 'Dao.Grammar.Text.TextParse'. It extends 'Lexer' with the ability to branch, that is to try
-- several 'Lexer's until one that matches the input string is found. To do branching 'Grammar'
-- instantiates 'Control.Monad.MonadPlus' and 'Control.Applicative.Alternative'.
data Grammar m o
  = GrEmpty
    -- ^ This is a valid grammr indicating that the input string does not match. It is used to
    -- instantiate 'Control.Monad.mzero' and 'Control.Applicative.empty'.
  | GrReturn                               o
    -- ^ This is a valid 'Grammar' indicating that the input string is valid. This constructor is
    -- used to instantiate 'Control.Monad.return' and 'Control.Applicative.pure'.
  | GrLift            (m    (Grammar m o))
  | GrTable   (Array (Lexer (Grammar m o)))
  | GrChoice  (Grammar m o) (Grammar m o)
  | GrType    TypeRep       (Grammar m o)
  | GrComment StrictText    (Grammar m o)
  | GrFail    StrictText
  | GrReject  InvalidGrammar
  deriving Typeable

instance MonadFix m => MonadFix (Grammar m) where { mfix f = GrLift $ mfix $ return . (>>= f); }

instance Monad m => Functor (Grammar m) where
  fmap f o = case o of
    GrEmpty            -> GrEmpty
    GrReturn  o        -> GrReturn  $ f o
    GrLift    next     -> GrLift  $       liftM (liftM f) next
    GrTable   next     -> GrTable  (liftM (liftM (liftM f)) next)
    GrChoice  next alt -> GrChoice (liftM f next) (liftM f alt)
    GrType    typ next -> GrType  typ $ liftM f next
    GrComment lbl next -> GrComment lbl $ liftM f next
    GrFail    err      -> GrFail   err
    GrReject  err      -> GrReject err

instance Monad m => Monad (Grammar m) where
  return = GrReturn
  o >>= f = case o of
    GrEmpty            -> GrEmpty
    GrReturn  o        -> f o
    GrLift    next     -> GrLift  $ liftM (>>= f) next
    GrTable   next     -> GrTable $ liftM (liftM (>>= f)) next
    GrChoice  next alt -> GrChoice (next >>= f) (alt >>= f)
    GrType    typ next -> GrType  typ $ next >>= f
    GrComment lbl next -> GrComment lbl $ next >>= f
    GrFail    err      -> GrFail   err
    GrReject  err      -> GrReject err
  fail = GrFail . Strict.pack

instance MonadPlus m => MonadPlus (Grammar m) where
  mzero = GrEmpty
  mplus a b = case a of
    GrEmpty            -> b
    GrReturn  a        -> case b of
      GrEmpty            -> GrReturn a
      GrReturn  _        -> GrReturn a
      GrLift    b        -> GrLift   $ liftM (mplus $ GrReturn a) b
      GrTable   tabB     -> GrReject $ CutSomeBranches Nothing $ regexOfLexer <$> elems tabB
      GrChoice  next alt -> mplus (mplus (GrReturn a) next) alt
      GrType    _    _   -> GrReturn a
      GrComment _    _   -> GrReturn a
      GrFail    _        -> GrReturn a
      GrReject  err      -> GrReject err
    GrLift    a        -> case b of
      GrEmpty            -> GrLift a
      GrReturn  next     -> GrLift  $ liftM (flip mplus $ GrReturn next) a
      GrLift    next     -> GrLift  $ mplus a next
      GrTable   next     -> GrLift  $ liftM (flip mplus $ GrTable next) a
      GrChoice  next alt -> mplus (mplus (GrLift a) next) alt
      GrType    typ next -> GrChoice (GrLift a) (GrType  typ next)
      GrComment lbl next -> GrChoice (GrLift a) (GrComment lbl next)
      GrFail    err      -> GrChoice (GrLift a) (GrFail err)
      GrReject  err      -> GrReject err
    GrTable   tabA     -> case b of
      GrEmpty            -> GrTable tabA
      GrReturn  b        -> GrChoice (GrTable tabA) (GrReturn b)
      GrLift    next     -> GrLift  $ liftM (mplus $ GrTable tabA) next
      GrTable   tabB     -> case tabB ! 0 of
        Nothing   -> GrTable tabA
        Just rxB  -> case lastElem tabA of
          Nothing   -> GrTable tabB
          Just rxA  -> if not $ shadowsParallel rxA rxB then GrTable $ tabA<>tabB else GrReject
            invalidGrammar
            { regexSeqA = Just $ regexOfLexer rxA
            , regexSeqB = Just $ regexOfLexer rxB
            , regexBadSequence = False
            }
      GrChoice  next alt -> mplus (mplus (GrTable tabA) next) alt
      GrType    typ next -> GrChoice (GrTable tabA) (GrType  typ next)
      GrComment lbl next -> GrChoice (GrTable tabA) (GrComment lbl next)
      GrFail    err      -> GrChoice (GrTable tabA) (GrFail err)
      GrReject  err      -> GrReject err
    GrChoice  next alt -> GrChoice next (mplus alt b)
    GrType    typ next -> GrType   typ $ mplus next b
    GrComment lbl next -> GrComment  lbl $ mplus next b
    GrFail    err      -> case b of
      GrTable   next     -> GrReject $ CutSomeBranches Nothing $ regexOfLexer <$> elems next
      _                    -> GrFail   err
    GrReject  err      -> GrReject err
                             
instance (Applicative m, Monad m) => Applicative (Grammar m) where { pure = return; (<*>) = ap; }

instance (Alternative m, MonadPlus m) => Alternative (Grammar m) where { empty = mzero; (<|>) = mplus; }

instance MonadTrans Grammar where { lift o = GrLift $ liftM GrReturn o; }

instance MonadIO (Grammar IO) where { liftIO = lift; }

-- | This defines a class of data types which have 'Grammar's defined for them, similar to how many
-- data types may instantiate 'Prelude.Read'.
class HasGrammar o m where { theGrammar :: Grammar m o }

_grammarToParser
  :: (Functor m, Monad m, MonadPlus m, MonadParser text m, TextBuilder text)
  => (Grammar m o -> m ()) -> Grammar m o -> m o
_grammarToParser f = loop where
  loop o = f o >> case o of
    GrEmpty            -> mzero
    GrReturn  o        -> return o
    GrLift    next     -> next >>= loop
    GrTable   next     -> case _makeCharTable $ elems next of
      Left  err     -> throw err
      Right chartab -> look1 >>= maybe mzero (msum . fmap lexerToParser >=> loop) . Sa.lookup chartab
    GrChoice  next alt -> mplus (loop next) (loop alt)
    GrType    _   next -> loop next
    GrComment _   next -> loop next
    GrFail    msg      -> fail (Strict.unpack msg)
    GrReject  err      -> throw err

-- | Convert a 'Grammar' to a 'MonadParser' that actually parses things.
grammarToParser
  :: (Functor m, Monad m, MonadPlus m, MonadParser text m, TextBuilder text)
  => Grammar m o -> m o
grammarToParser = _grammarToParser (const $ return ())

-- | Like 'grammarToParser', but takes an additional 'MonadParser' function which takes a
-- 'GrammarSignature' value, and can be used to display information about each node of the 'Grammar'
-- before it is converted to 'MonadParser' functions.
grammarToVerboseParser
  :: (Functor m, Monad m, MonadPlus m, MonadParser text m, TextBuilder text)
  => (GrammarSignature m o -> m ()) -> Grammar m o -> m o
grammarToVerboseParser f = _grammarToParser $ f . grammarSignature

-- Construct a 'Grammar' from a 'Lexer'.
_rxGrammar :: Lexer a -> Grammar m a
_rxGrammar o = case o of
  LxReject err -> GrReject err
  LxIden     o -> GrReturn o
  lexer           -> GrTable $ array [fmap GrReturn lexer]

-- | Return the type to which this 'Grammar' will evaluate.
typeOfGrammar :: forall m t . Typeable t => Grammar m t -> TypeRep
typeOfGrammar = typ undefined where
  typ :: t -> Grammar m t -> TypeRep
  typ ~t _ = typeOf t

-- | Construct a 'Grammar' from a finite list of 'Lexer' choices. When this 'Grammar' is converted
-- to a 'MonadParser', each 'Lexer' will be tried in turn. If any 'Lexer' occuring earlier in the
-- list 'shadowParallel's any 'Lexer' occuring later in the list (which is checked by
-- 'shadowsParallel'), then this 'Grammar' evaluates to an 'InvalidGramar'.
choice :: (Functor m, Monad m) => [Lexer o] -> Grammar m o
choice lexers = if null lexers then GrEmpty else loop lexers where
  loop ox = case ox of
    (a:b:ox) ->
      if not $ shadowsParallel a b
      then  loop $ b:ox
      else GrReject $
        invalidGrammar
        { regexSeqA = Just $ regexOfLexer a
        , regexSeqB = Just $ regexOfLexer b
        , regexBadSequence = False
        }
    _ -> GrTable $ array $ fmap return <$> lexers

-- | Retrieve every 'Regex' for all branches of the 'Grammar'.
grammarBranches :: Grammar m o -> [Regex]
grammarBranches o = case o of
  GrTable next -> regexOfLexer <$> elems next
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
elseFail :: (Functor m, Monad m, MonadPlus m) => Grammar m o -> String -> Grammar m o
elseFail p = mplus p . GrFail . Strict.pack

-- | Inspired by the @Parsec@ package, this is the infix operator of 'elseFail'. In Haskell code, it
-- has a precedence of zero.
(<?>) :: (Functor m, Monad m, MonadPlus m) => Grammar m o -> String -> Grammar m o
(<?>) = elseFail
infix 0 <?>

instance TypeableGrammar (Grammar m) where
  informType o = let typ = typeOfGrammar o in case o of
    GrReject err -> GrReject $ _invalidSetType typ err
    _            -> GrType typ o

----------------------------------------------------------------------------------------------------

-- | This data type is a version of 'Grammar' that can be inspected with @case@ statements. A
-- 'Grammar' can be converted to a 'GrammarSignature' but not vice-versa. 'GrammarSignature'
-- instantiates 'Prelude.Eq', so you can test if two 'Lexer's are somewhat equivalent by comparing
-- their 'LexerSignature's. 'LexerSignature' also instantiates 'Prelude.Show' so you can visualize
-- it.
data GrammarSignature m o
  = GrammarEmpty
    -- ^ This is the 'Grammar' that indicates the current input text is not grammatically correect.
  | GrammarReturn o
  | GrammarLift    (m (GrammarSignature m o))
    -- ^ This 'GrammarSignature' marks a point in the 'Grammar' where a 'MonadParser' has been
    -- lifted directly into the 'Grammar'.
  | GrammarTable   [LexerSignature (GrammarSignature m o)]
    -- ^ This is a 'Grammar' constructed where there is a choice of many 'Lexer's that could define
    -- the 'Grammar' for the input text. Any of the 'Lexer's may match the input text, the first to
    -- match defines the 'Grammar' of that position in the text.
  | GrammarChoice  [GrammarSignature m o]
    -- ^ This defines a 'Grammar' where any of the 'Grammar's in the list could be grammatically
    -- correct for the given input text. The first 'Grammar' to match the input text defines the
    -- 'Grammar' of the text.
  | GrammarType    TypeRep    (GrammarSignature m o)
    -- ^ This is a 'Grammar' label, indicating the data type that should be constructed from the
    -- input text if the 'Grammar' should match.
  | GrammarComment StrictText (GrammarSignature m o)
    -- ^ This is a 'Grammar' comment label, a programmer-defined label indicating what the current
    -- 'Grammar' structure is.
  | GrammarFail    StrictText
    -- ^ This is the 'Grammar that is defined to be incorrect. It is similar to 'GrammarEmpty' but
    -- contains information about why the input text is not grammatical. It is different from
    -- 'GrammarEmpty' in that when translated to a 'MonadParser', a 'GrammarEmpty' will translate to
    -- 'Control.Monad.mzero' (equivalently 'Control.Applicative.empty') whereas a 'GrammarFail' will
    -- translated to 'Control.Monad.fail'.
  | GrammarReject InvalidGrammar
    -- ^ This is an invalid 'Grammar'. Unlike 'GrammarFail', this constructor indicates that the
    -- 'Grammar' is itself is invalid or ambiguous and cannot be converted to any meaningful parser
    -- at all, thus we reject the 'Grammar'.

instance Functor m => Functor (GrammarSignature m) where
  fmap f o = case o of
    GrammarEmpty       -> GrammarEmpty
    GrammarReturn    o -> GrammarReturn $ f o
    GrammarLift      o -> GrammarLift $ fmap (fmap f) o
    GrammarTable     o -> GrammarTable $ fmap (fmap (fmap f)) o
    GrammarChoice    o -> GrammarChoice $ fmap (fmap f) o
    GrammarType    t o -> GrammarType t $ fmap f o
    GrammarComment c o -> GrammarComment c $ fmap f o
    GrammarFail    err -> GrammarFail   err
    GrammarReject  err -> GrammarReject err

instance Eq o => Eq (GrammarSignature m o) where
  (==) = grammarSignatureEq (==) (\ _ _ -> True)

instance PPrintable o => Show (GrammarSignature m o) where
  show = Lazy.unpack . runTextPPrinter 2 80 .
    pprintGrammarSignature pPrint (const [pText "(do{...})"]) 1

grammarSignature :: Functor m => Grammar m o -> GrammarSignature m o
grammarSignature o = case o of
  GrEmpty       -> GrammarEmpty
  GrReturn    o -> GrammarReturn o
  GrLift      o -> GrammarLift (fmap grammarSignature o)
  GrTable     o -> GrammarTable $ lexerSignature . fmap grammarSignature <$> elems o
  GrChoice  a b ->
    let loop o = case o of
          GrChoice a b -> loop a ++ loop b
          o            -> [grammarSignature o]
    in  GrammarChoice $ loop a ++ loop b
  GrType    t o -> GrammarType    t $ grammarSignature o
  GrComment c o -> GrammarComment c $ grammarSignature o
  GrFail    err -> GrammarFail   err
  GrReject  err -> GrammarReject err

-- | Compares two 'GrammarSignature's for similarity, it is a kind of equality function, but
-- equality testing operators for @o@ and @(m o)@ must be provided. This function is used to define
-- 'Prelude.Eq' like so:
--
-- > instance 'Prelude.Eq' o => 'Prelude.Eq' ('GrammarSignature' m o) where
-- >     ('Prelude.==') a b = 'grammarSignatureEq' ('Prelude.==') ('Prelude.const' 'Prelude.True')
--
grammarSignatureEq
  :: (o -> o -> Bool)
  -> (forall o . m o -> m o -> Bool)
  -> GrammarSignature m o -> GrammarSignature m o -> Bool
grammarSignatureEq eq eqm a b =
  let loop = grammarSignatureEq eq eqm
      comp :: (o -> o -> Bool) -> [o] -> [o] -> Bool
      comp eq a b = case (a, b) of
        ([]  , []  ) -> True
        (a:ax, b:bx) -> comp eq ax bx && eq a b -- breadth-first comparison
        _            -> False 
  in  case (a, b) of
        (GrammarEmpty       , GrammarEmpty       ) -> True
        (GrammarReturn  a   , GrammarReturn  b   ) -> eq a b
        (GrammarLift    a   , GrammarLift    b   ) -> eqm a b
        (GrammarTable   a   , GrammarTable   b   ) -> comp (lexerSignatureEq loop) a b
        (GrammarChoice  a   , GrammarChoice  b   ) -> comp loop a b
        (GrammarType    ta a, GrammarType    tb b) -> ta==tb && loop a b
        (GrammarComment ca a, GrammarComment cb b) -> ca==cb && loop a b
        (GrammarFail    a   , GrammarFail    b   ) -> a==b
        _                                    -> False

-- | Convert a 'GrammarSignature' to a list of 'StrictText's given two functions for converting the
-- @o@ and @(m o)@ data types. Also provide an integer maximum-depth value, as 'Grammar's are
-- recursive structures that may loop indefinitely. Usually a depth of 1 is good enough. The
-- resulting list of 'StrictText's are intended to be concatenated with the 'Data.Text.Lazy.unlines'
-- function.
pprintGrammarSignature
  :: (o -> [PPrint])
  -> (m (GrammarSignature m o) -> [PPrint])
  -> Int -> GrammarSignature m o -> [PPrint]
pprintGrammarSignature txt txtm depth o =
  let loop  = pprintGrammarSignature txt txtm (depth-1)
      section hdr body = [pInline $ hdr ++ [pIndent body]]
      closure hdr body = section (hdr ++ [pText "("]) [pIndent body, pNewLine, pText ")"]
      list :: (o -> [PPrint]) -> String -> [o] -> [PPrint]
      list prin title ox = section [pText title, pSpace] $ case ox of
        []   -> [pText "[]"]
        o:ox -> return $
          if depth<=0 then pText "[ .... ]" else pInline $ concat
            [ [pText "[", pSpace], prin o, [pNewLine]
            , ox >>= \o -> [pText ",", pSpace] ++ prin o ++ [pNewLine]
            , [pText "]"]
            ]
  in  case o of
        GrammarEmpty        -> [pText "GrammarEmpty"]
        GrammarReturn    o  -> section [pText "GrammarReturn", pSpace] (txt o)
        GrammarLift      o  -> section [pText "GrammarLift", pSpace] (txtm o)
        GrammarTable     ox -> list (pprintLexerSignature loop) "GrammarTable" ox
        GrammarChoice    ox -> list loop "GrammarChoice" ox
        GrammarType    t o  ->
          closure [pText "GrammarType(", pText (show t), pText ")", pSpace] (loop o)
        GrammarComment c o  -> 
          closure [pText "GrammarType(", pText (show c), pText ")", pSpace] (loop o)
        GrammarFail    err  -> pPrint err
        GrammarReject  err  -> pPrint err

----------------------------------------------------------------------------------------------------

_makeCharTable :: [Lexer (Grammar m o)] -> Either InvalidGrammar (CharTable [Lexer (Grammar m o)])
_makeCharTable = loop [] where
  errCut = Left . CutSomeBranches Nothing . fmap regexOfLexer
  loop rev ox = case ox of
    []                         -> return $ Sa.fromListWith (++) rev
    LxReject         err  : _  -> Left err
    LxFail            _   : _  -> errCut ox
    LxIden            _   : _  -> errCut ox
    LxTake            _   : _  -> errCut ox
    o@(LxStep charset _ ) : ox -> flip loop ox $ (rev++) $
      flip fmap (Iv.toList $ initialCharSetOf charset) $ \interval ->
        A.accumArray const [o] (Iv.toBoundedPair interval) []

makeCharTable :: [Lexer o] -> Either InvalidGrammar (CharTable [Lexer (Grammar m o)])
makeCharTable tab' = loop tab >>= _makeCharTable where
  tab = fmap GrReturn <$> tab'
  loop ox = case ox of
    [ ]    -> Right tab
    [_]    -> Right tab
    a:b:ox -> if not (shadowsParallel a b) then loop (b:ox) else Left $
      invalidGrammar
      { regexSeqA = Just $ regexOfLexer a
      , regexSeqB = Just $ regexOfLexer b
      , regexBadSequence = False
      }

----------------------------------------------------------------------------------------------------

-- | This data type contains an inner type that can be described with a 'Grammar'. When the grammar
-- for this type is converted to a parser, the source location before and after the parse are
-- recorded and stored with the inner type so analyzing the 'Location' will tell you from where in
-- the source file the inner type was parsed.
data Location o
  = NoLocation              o
  | StartLocation TextPoint o
  | EndLocation             o TextPoint
  | Location      TextPoint o TextPoint
  deriving (Eq, Ord, Show, Typeable)

instance Functor Location where
  fmap f o = case o of
    NoLocation       o    -> NoLocation       (f o)
    StartLocation lo o    -> StartLocation lo (f o)
    EndLocation      o hi -> EndLocation      (f o) hi
    Location      lo o hi -> Location      lo (f o) hi

instance DeleteContents (Location o) where
  deleteContents o = case o of
    NoLocation      o   -> NoLocation o
    StartLocation _ o   -> NoLocation o
    EndLocation     o _ -> NoLocation o
    Location      _ o _ -> NoLocation o

instance Monoid o => Monoid (Location o) where
  mempty = NoLocation mempty
  mappend a b = case a of
    NoLocation        a     -> case b of
      NoLocation        b     -> NoLocation                  (a<>b)
      StartLocation loB b     -> StartLocation      loB      (a<>b)
      EndLocation       b hiB -> EndLocation                 (a<>b)      hiB
      Location      loB b hiB -> Location           loB      (a<>b)      hiB
    StartLocation loA a     -> case b of
      NoLocation        b     -> StartLocation      loA      (a<>b)
      StartLocation loB b     -> StartLocation (min loA loB) (a<>b)
      EndLocation       b hiB -> Location           loA      (a<>b)          hiB
      Location      loB b hiB -> Location      (min loA loB) (a<>b)          hiB
    EndLocation       a hiA -> case b of
      NoLocation        b     -> EndLocation                 (a<>b)      hiA
      StartLocation loB b     -> Location               loB  (a<>b)      hiA
      EndLocation       b hiB -> Location               hiB  (a<>b) (max hiA hiB)
      Location      loB b hiB -> Location               loB  (a<>b) (max hiA hiB)
    Location      loA a hiA -> case b of
      NoLocation        b     -> Location           loA      (a<>b)      hiA
      StartLocation loB b     -> Location      (min loA loB) (a<>b)      hiA
      EndLocation       b hiB -> Location           loA      (a<>b) (max hiA hiB)
      Location      loB b hiB -> Location      (min loA loB) (a<>b) (max hiA hiB)

----------------------------------------------------------------------------------------------------

class LocationFunctor dat o where { fmapLocation :: (Location o -> Location o) -> dat -> dat; }

instance LocationFunctor (Location o) o where { fmapLocation = ($); }

unwrapLocation :: Location o -> o
unwrapLocation o = case o of
  NoLocation      o   -> o
  StartLocation _ o   -> o
  EndLocation     o _ -> o
  Location      _ o _ -> o

-- | Convert a 'Location' to a 'TextRegion', if possible.
locationRegion :: Location o -> TextRegion
locationRegion o = case o of
  Location      lo _ hi -> TextRegion $ Just (lo, hi)
  _                     -> TextRegion Nothing

startLocation :: Location o -> Maybe TextPoint
startLocation o = case o of
  StartLocation o _   -> Just  o
  Location      o _ _ -> Just  o
  _                   -> Nothing

endLocation :: Location o -> Maybe TextPoint
endLocation o = case o of
  EndLocation   _ o -> Just  o
  Location    _ _ o -> Just  o
  _                 -> Nothing

getPoint
  :: (MonadSourceCodeParser text m, Functor m, Applicative m, Alternative m, Monad m, MonadPlus m)
  => Grammar m TextPoint
getPoint = GrLift $ fmap GrReturn getTextPoint

setPoint
  :: (MonadSourceCodeParser text m, Functor m, Applicative m, Alternative m, Monad m, MonadPlus m)
  => TextPoint -> Grammar m ()
setPoint = GrLift . fmap GrReturn . setTextPoint

location
  :: (MonadSourceCodeParser text m, Functor m, Applicative m, Alternative m, Monad m, MonadPlus m)
  => Grammar m o -> Grammar m (Location o)
location o = Location <$> getPoint <*> o <*> getPoint

-- | When converted to a monadic parser, this will increment the line counter and reset the column
-- counter to 1 if the inner 'Grammar' converts to a successful parser. This is designed to be used
-- with the @'lineBreak' :: 'Grammar'@ like so:
--
-- > newline lineBreak
--
newline
  :: (MonadSourceCodeParser text m, Functor m, Applicative m, Alternative m, Monad m, MonadPlus m)
  => Grammar m o -> Grammar m o
newline g = getPoint >>= \pt -> g >>= \o ->
  setPoint (pt{ lineNumber = 1 + lineNumber pt }) >> return o

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
  :: (ToText o, MonadSourceCodeParser text m, Functor m, Applicative m, Alternative m, Monad m, MonadPlus m)
  => (StrictText -> TextPoint -> TextPoint) -> Grammar m o -> Grammar m o
movePoint count g = g >>= \o -> getPoint >>= setPoint . count (toText o) >> return o

-- | The 'asciiLineCounter' is a nice default line counter that can be used with the 'movePoint'
-- function. 
asciiLineCounter :: Count -> StrictText -> TextPoint -> TextPoint
asciiLineCounter tab t =
  let column n (TextPoint ln col) = TextPoint ln $ col+n
      line n col (TextPoint ln _) = TextPoint (ln+n) col
      loop cx st = case cx of
        []   -> st
        c:cx -> case c of
          '\t' -> loop cx $ column tab st
          '\n' -> loop cx $ line 1 1 st
          c | ' '<=c -> loop cx $ column 1 st
          _          -> loop cx st
  in  loop (Strict.unpack t)

-- | This 'Grammar' has no effect on the language you are defining. But when it is converted to a
-- 'MonadSourceParser'. Upon conversion, the resulting 'MonadSourceParser' will update it's own the
-- 'TextPoint' contained within it's own internal state. Pass a line-counting function to convert
-- the value produced by the given 'Grammar' into a 'TextPoint', and this function will use
-- 'getPoint' and 'putPoint' to update current 'TextPoint' in the 'MonadSourceParser'.
--
-- For example, if your 'Grammar' returns a 'StrictText' type, you can use 'asciiLineCounter' to
-- convert it to a 'TextPoint' that will count how many lines of text there are, along with how many
-- columns there are in the final line, and this 'TextPoint' will be used to update the 'TextPoint'
-- internal to the 'MonadSourceParser'.
moveCursor
  :: (MonadSourceCodeParser text m, Functor m,
      Applicative m, Alternative m, Monad m, MonadPlus m)
  => (t -> TextPoint) -> Grammar m t -> Grammar m t
moveCursor toPoint f = do
  before <- getPoint
  o      <- f
  let after = toPoint o
  setPoint $ after{ lineNumber = lineNumber before + lineNumber after }
  return o

----------------------------------------------------------------------------------------------------

-- | Some 'Grammar's made up of chains of other 'Grammar's may match a lot of text before
-- discovering that it actually does not match the input text. For example, the grammar:
--
-- > 'Prelude.fmap' 'Data.Monoid.mconcat' $ 'Control.Monad.sequence' $
-- >     [str "hello", 'Dao.Text.toText' 'Control.Applicative.<$>' 'space', str "world"] where
-- >          str s = 'choice' ['lexer' 'Dao.Text.toText' ['rxString' s]]
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
backtrack
  :: (MonadBacktrackingParser text m, Functor m, Applicative m, Alternative m, Monad m, MonadPlus m)
  => text -> Grammar m ()
backtrack = GrLift . fmap GrReturn . unstring

