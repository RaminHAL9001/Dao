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

module Dao.Text.Regex
  ( -- * Combinators for Building 'Regex's
    ToRegex(rx), Regex, regexUnits,
    RegexUnit, RegexRepeater, rxGetString, rxGetCharSet, rxGetRepeater,
    RepeatMinMax(MIN, MAX),
    ToRegexRepeater(toRegexRepeater), lexRepeaterToPair,
    regexToParser, regexUnitToParser, regexSpan, regexBreak, regexMatch, concatRegexUnits,
    -- * Creating a Regex Table:
    RegexTable, regexTableElements, regexTable, regexsToTable, regexTableToParser,
    -- * The Error Type Used By All Monadic Parsers ('MonadParser's)
    InvalidGrammar(InvalidGrammar),
    InvalidGrammarDetail(OtherFailure, RegexShadowError, UnexpectedInput, CutSomeBranches),
    RegexShadowErrorType(ParallelRegex, SequenceRegex),
    otherFailure, regexShadowErrorLens, regexShadowErrorType, regexShadower, regexShadowee,
    unexpectedInputLens, unexpectedInput, unexpectedInputMessage, cutSomeBranches,
    invalidGrammarLens, invalidGrammarTypeRep, invalidGrammarLocation, invalidGrammarDetail,
    -- * High-Level Properties of 'Regex's.
    InitialCharSetOf(initialCharSetOf),
    RxMinLength(rxMinLength), RxMaxLength(rxMaxLength),
    Probability, computeLogProb, computeProb, probChar, probSet, probText,
    probRegexUnit, probRepeater, probRegex,
    ShadowsSeries(shadowsSeries), ShadowsParallel(shadowsParallel),
    -- * An Abstract Interface for Parser Monads
    MonadParser(
      look, look1, get1, string, count, eof, munch, munch1,
      noMoreThan, satisfy, char, regex, pfail,
      incrementPushBackCounter, incrementCharCounter
    ),
    MonadPrecedenceParser(prec, step, reset),
    MonadSourceCodeParser(getTextPoint, setTextPoint),
    MonadBacktrackingParser(unstring),
    -- * A General Purpose Lazy 'Data.Text.Lazy.Text' Based 'Parser' Monad
    ParseInput, Parser, runParser, getCharCount,
    ParserState(ParserState), parserState, inputString, userState, textPoint, charCount,
    minPrec, precedence, efficiency, parser_to_S, lookAheadWithParser
  ) where

import           Prelude hiding (id, (.))

import           Dao.Array
import           Dao.Count
import           Dao.Lens
import           Dao.Predicate
import           Dao.Text
import           Dao.Text.Location
import           Dao.Text.CharSet

import qualified Dao.Interval as Iv
import           Dao.PPrint
import           Dao.TestNull

import           Control.Applicative
import           Control.Category
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import qualified Data.Array.IArray as A
import           Data.Char hiding (Space)
import           Data.Ix
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text      as Strict
import           Data.Typeable

import qualified Text.ParserCombinators.ReadP    as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

import           Numeric (showHex)

----------------------------------------------------------------------------------------------------

-- | This class generalizes over the functions for parsers like 'Text.ParserCombinators.ReadP.ReadP',
-- 'Text.GrammarCombinator.ReadPrec.ReadPrec', and our own 'Dao.Grammar.Text.TextParse'.
--
-- When programming your parsers, you actually will not *NOT* be using the functions in this class.
-- Instead, you construct your parsers using the 'Grammar' data type, then use the 'makeParser'
-- function to transform the 'Grammar' into a monadic parser function (one which instantiates this
-- class). This class only exists to provide a consistent interface to the various parser monads
-- available.
--
-- This class merely provides the necessary functions that 'makeParser' can use to transform 'Grammar'
-- data types into some concrete monadic parser -- a monadic parser that instantiates this class. You
-- do not need to concern yourself with the details of how the 'Grammar's are converted, just know
-- that the conversion process calls these functions.
--
-- The minimal complete definition includes 'look', 'look1', 'unstring', 'get1', 'string', and
-- 'count', but is highly recommended you also instantiate 'eof', 'munch', 'munch1', and
-- 'noMoreThan' as the default instantiations which rely on the minimal complete definition are a
-- bit inefficient.
class (Monad m, MonadPlus m) => MonadParser m where
  -- | Look ahead some number of characters without consuming any text,
  -- evaluate to 'Control.Applicative.empty' if the requested number of
  -- characters is not available. (required)
  look  :: m LazyText
  -- | Look ahead 1 character without consuming it. Evaluate to 'Control.Monad.mzero' if at end of
  -- input text. (required)
  look1 :: m Char
  -- | Consume a single character from the input text. (required)
  get1 :: m Char
  -- | If the given text matches the current head of the parser input text, consume that text and
  -- succeed, otherwise evaluate to 'Control.Applicative.empty'. (required)
  string :: LazyText -> m LazyText
  -- | Consume a given number of characters or fail if there aren't enough, and never parse more
  -- than the given number of characters. This requires a look ahead, so it is left to the
  -- instatiator of this class to provide the most efficient implementation. (required)
  count :: Count -> CharSet -> m LazyText
  -- | Succeed only if the end of the input text has been reached.
  eof :: m ()
  eof = mplus (look1 >> return False) (return True) >>= guard
  -- | Consume and return all characters from the head of the input text that satisfy the predicate.
  -- Never backtracks, returns an empty string if the head of the input text does not satisfy the
  -- predicate at all.
  munch :: CharSet -> m LazyText
  munch p =
    let loop cx = mplus (liftM Just $ satisfy p) (return Nothing) >>=
          maybe (return cx) (loop . Lazy.snoc cx) in loop mempty
  -- | Consume and return all characters from the head of the input text that match the given
  -- predicate, but backtracks if no characters from the head of the input text match.
  munch1 :: CharSet -> m LazyText
  munch1 p = liftM2 Lazy.cons (satisfy p) (munch p)
  -- | Never fail, but consume as many characters satisfying the predicate as possible, and never
  -- consuming more than the specified amount.
  noMoreThan :: Count -> CharSet -> m LazyText
  noMoreThan lim p =
    let loop lim cx = if lim<=0 then return cx else
          mplus (liftM Just $ satisfy p) (return Nothing) >>=
            maybe (return cx) (loop (pred lim) . Lazy.snoc cx)
    in  loop lim mempty
  -- | Consumes the next character only if it satisfies the given predicate.
  satisfy :: CharSet -> m Char
  satisfy p = look1 >>= guard . csetMember p >> get1
  -- | Like 'satisfy' but matches a single character.
  char :: Char -> m Char
  char = satisfy . anyOf . return
  -- | This optional function should attempt to match a 'Regex' to the input string by using
  -- 'regexToParser'.
  regex :: Regex -> m LazyText
  regex = regexToParser
  -- | When the input text does not match a 'Grammar', an error of type 'InvalidGrammar' must be
  -- handed over to this monad. What happens next is up to the instance provided here. It may throw
  -- an error, or it may evaluate to 'Control.Applicative.empty'.
  pfail :: InvalidGrammar -> (forall ig . m ig)
  pfail err = fail ("Parser error: "++show err)
  -- | An optional function to record efficiency of your parser. Every time characters are removed
  -- from the input stream, the number of characters removed are reported here so you can increment
  -- a character counter in your parser's state, if there is one.
  incrementCharCounter :: Count -> m ()
  incrementCharCounter = const $ return ()
  -- | An optional function to record efficiency of your parser. The number of characters
  -- looked-ahead by 'look' are reported here so you can increment a look-ahead couner in your
  -- parser's state, if there is one.
  incrementPushBackCounter :: Count -> m ()
  incrementPushBackCounter = const $ return ()

instance MonadParser ReadP.ReadP where
  look    = liftM Lazy.pack ReadP.look
  look1   = ReadP.look >>= \cx -> case cx of { [] -> mzero; c:_ -> return c; }
  get1    = ReadP.get
  string  = liftM Lazy.pack . ReadP.string . Lazy.unpack
  count i = liftM Lazy.pack . ReadP.count (fromIntegral i) . ReadP.satisfy . csetMember
  eof     = ReadP.eof
  munch   = liftM Lazy.pack . ReadP.munch . csetMember
  munch1  = liftM Lazy.pack . ReadP.munch1 . csetMember
  satisfy = ReadP.satisfy . csetMember
  char    = ReadP.char
  pfail   = fail . show

instance MonadParser ReadPrec.ReadPrec where
  look    = liftM Lazy.pack ReadPrec.look
  look1   = ReadPrec.look >>= \cx -> case cx of { [] -> mzero; c:_ -> return c; }
  get1    = ReadPrec.get
  string  = liftM Lazy.pack . ReadPrec.lift . ReadP.string . Lazy.unpack
  count i = liftM Lazy.pack . ReadPrec.lift . ReadP.count (fromIntegral i) . ReadP.satisfy . csetMember
  eof     = ReadPrec.lift ReadP.eof
  munch   = liftM Lazy.pack . ReadPrec.lift . ReadP.munch . csetMember
  munch1  = liftM Lazy.pack . ReadPrec.lift . ReadP.munch1 . csetMember
  satisfy = ReadPrec.lift . ReadP.satisfy . csetMember
  char    = ReadPrec.lift . ReadP.char
  pfail   = fail . show

----------------------------------------------------------------------------------------------------

-- | This class extends the 'MonadParser' class with functions for precedence parsing, useful for
-- parsing languages that have infix operators.
class (Monad m, MonadPlus m, MonadParser m) => MonadPrecedenceParser m  where
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

instance MonadPrecedenceParser ReadPrec.ReadPrec where
  prec  = ReadPrec.prec . fromIntegral
  step  = ReadPrec.step
  reset = ReadPrec.reset

----------------------------------------------------------------------------------------------------

-- | This class extends the 'MonadParser' class with functions for manipulating line and column
-- numbers, which is what parsers of computer language source code often need to do for producing
-- useful error messages. If your monadic parser can track line and or column numbers, you should
-- consider instantiating it into this class.
class (Monad m, MonadPlus m, MonadParser m) => MonadSourceCodeParser m where
  getTextPoint :: m TextPoint
  setTextPoint :: TextPoint -> m ()

-----------------------------------------------------------------------------------------------------

-- | Backtracking parsers should be avoided because they are inefficient. However, this class is
-- provided if the functionality is required.
class (Monad m, MonadPlus m, MonadParser m) => MonadBacktrackingParser m where
  -- | This function should prepend arbitrary text to the front of the input string in the state
  -- of the parser. It is called 'unstring' because it has the opposite effect of the 'string'
  -- function.
  unstring :: LazyText -> m ()

----------------------------------------------------------------------------------------------------

-- Transform a 'Regex' into a 'Parser'. This is used to implement the 'regex' function for the
-- instantiation of the 'Parser' data type into the 'MonadParser' class.
_regexToParser :: Regex -> Parser st LazyText
_regexToParser (Regex sq) = mconcat <$> (sequence $ _regexUnitToParser <$> elems sq)

-- | This function converts a 'Regex' to a 'MonadParser' using the various methods of the
-- 'MonadParser' class, for example 'munch', 'string', and 'noMoreThan'.
--
-- If you are rolling your own 'MonadParser' rather than using one of the many excellent monadic
-- parsers available to you, it is safe to use this function to instantiate the 'regex' method; this
-- function does not make use of the 'regex' function. In fact, the default instantiation for
-- 'regex' uses this function, so you do not need to instantiate 'regex' at all.
regexToParser :: (MonadPlus m, MonadParser m) => Regex -> m LazyText
regexToParser = liftM fst . flip lookAheadWithParser () . _regexToParser

regexUnitToParser :: (Monad m, MonadPlus m, MonadParser m) => RegexUnit -> m LazyText
regexUnitToParser = liftM fst . flip lookAheadWithParser () . _regexUnitToParser

-- | Like 'Prelude.span' but uses a 'Regex' to match from the start of the string.
regexSpan :: Regex -> LazyText -> Maybe (StrictText, LazyText)
regexSpan regex str =
  let (result, st) = runParser (regexToParser regex) $ on (parserState ()) [inputString <~ str] in
  case result of
    OK o -> Just (Lazy.toStrict o, st & inputString)
    _    -> Nothing

-- | Uses 'regexSpan' and a given 'Regex' to break a string into portions that match wrapped in a
-- 'Prelude.Right' constructor, and portions that do not match wrapped into a 'Prelude.Left'
-- constructor.
regexBreak :: Regex -> LazyText -> [Either StrictText StrictText]
regexBreak regex str = loop Lazy.empty str where
  loop keep str = case regexSpan regex str of
    Nothing       -> loop (Lazy.snoc keep $ Lazy.head str) (Lazy.tail str)
    Just (o, str) -> (guard (not $ Lazy.null keep) >> [Left $ Lazy.toStrict keep]) ++
      Right o : loop (Lazy.empty) str

-- | Uses 'regexSpan' to check whether a given 'Regex' matches the entire input string or not.
regexMatch :: Regex -> LazyText -> Bool
regexMatch regex str = maybe False (Lazy.null . snd) $ regexSpan regex str

----------------------------------------------------------------------------------------------------

data RegexShadowErrorType = ParallelRegex | SequenceRegex
  deriving (Eq, Ord, Show, Typeable, Ix, Enum, Bounded)

instance NFData RegexShadowErrorType where
  rnf o = case o of { ParallelRegex -> (); SequenceRegex -> (); }

----------------------------------------------------------------------------------------------------

-- | A 'InvalidGrammar' can occur during the parser evaluation. This is different from
-- 'InvalidGrammar' which occurs when constructing a 'Grammar', and happens if there are ambiguities
-- in the specified Context Free Grammar. A 'InvalidGrammar', on the other hand, is provided to the
-- lower-level parser monad (the monadic parser that instantiates 'MonadParser') by way of the
-- 'pfail' function.
data InvalidGrammarDetail
  = OtherFailure     StrictText
  | RegexShadowError RegexShadowErrorType Regex  Regex
  | UnexpectedInput  StrictText StrictText
  | CutSomeBranches  (Array Regex)
  deriving (Eq, Ord, Typeable)

instance PPrintable InvalidGrammarDetail where
  pPrint o = case o of
    OtherFailure msg -> [pText msg]
    RegexShadowError typ a b -> concat
      [ [pText "the regular expression", pSpace], pPrint a
      , [pText "consumes all of the characters that could possibly be consumed by", pSpace], pPrint b
      , [pText "when each regular expression is matched in", pSpace]
      , [case typ of { ParallelRegex -> pText "parallel,"; SequenceRegex -> pText "sequence,"; }]
      ]
    UnexpectedInput msg str -> [pText msg, pText ":", pSpace, pText $ show str]
    CutSomeBranches rxs     -> concat
      [ [pText "regular expressions to be tried in parallel, some have been cut off:", pNewLine]
      , elems rxs >>= \rx -> pPrint rx ++ [pNewLine]
      ]

instance Show InvalidGrammarDetail where { show = showPPrint 4 4 . pPrint; }

instance TestNull InvalidGrammarDetail where
  nullValue = OtherFailure nullValue
  testNull o = case o of { OtherFailure o -> Strict.null o; _ -> False; }

instance NFData InvalidGrammarDetail where
  rnf a = case a of
    OtherFailure     a     -> deepseq a ()
    RegexShadowError a b c -> deepseq a $! deepseq b $! deepseq c ()
    UnexpectedInput  a b   -> deepseq a $! deepseq b ()
    CutSomeBranches  a     -> deepseq a ()

otherFailure :: (Monad m, MonadPlus m) => Lens m InvalidGrammarDetail StrictText
otherFailure =
  newLensM
    (\  o -> case o of { OtherFailure o -> return o; _ -> mzero; })
    (\p o -> case o of { OtherFailure _ -> return (OtherFailure p); _ -> mzero; })

regexShadowErrorLens
  :: (Monad m, MonadPlus m)
  => Lens m InvalidGrammarDetail (RegexShadowErrorType, Regex, Regex)
regexShadowErrorLens =
  newLensM
    (\         o -> case o of { RegexShadowError a b c -> return (a, b, c); _ -> mzero; })
    (\ (a,b,c) o -> case o of { RegexShadowError _ _ _ -> return (RegexShadowError a b c); _ -> mzero; })

regexShadowErrorType :: (Monad m, MonadPlus m) => Lens m InvalidGrammarDetail RegexShadowErrorType
regexShadowErrorType = regexShadowErrorLens >>> tuple0

regexShadower :: (Monad m, MonadPlus m) => Lens m InvalidGrammarDetail Regex
regexShadower = regexShadowErrorLens >>> tuple1

regexShadowee :: (Monad m, MonadPlus m) => Lens m InvalidGrammarDetail Regex
regexShadowee = regexShadowErrorLens >>> tuple2

unexpectedInputLens
  :: (Monad m, MonadPlus m)
  => Lens m InvalidGrammarDetail (StrictText, StrictText)
unexpectedInputLens =
  newLensM
    (\       o -> case o of { UnexpectedInput a b -> return (a, b); _ -> mzero; })
    (\ (a,b) o -> case o of { UnexpectedInput _ _ -> return (UnexpectedInput a b); _ -> mzero; })

unexpectedInput :: (Monad m, MonadPlus m) => Lens m InvalidGrammarDetail StrictText
unexpectedInput = unexpectedInputLens >>> tuple0

unexpectedInputMessage :: (Monad m, MonadPlus m) => Lens m InvalidGrammarDetail StrictText
unexpectedInputMessage = unexpectedInputLens >>> tuple1

cutSomeBranches :: (Monad m, MonadPlus m) => Lens m InvalidGrammarDetail (Array Regex)
cutSomeBranches =
  newLensM
    (\  o -> case o of { CutSomeBranches o -> return o; _ -> mzero; })
    (\a o -> case o of { CutSomeBranches _ -> return $ CutSomeBranches a; _ -> mzero; })

----------------------------------------------------------------------------------------------------

newtype InvalidGrammar
  = InvalidGrammar (Maybe TypeRep, Location (), InvalidGrammarDetail)
  deriving (Eq, Ord, Typeable)

instance Exception InvalidGrammar

instance TestNull InvalidGrammar where
  nullValue = InvalidGrammar (Nothing, NoLocation (), OtherFailure (toText ""))
  testNull (InvalidGrammar (a, b, c)) = case b of
    NoLocation () -> isNothing a && testNull c
    _             -> False

instance Show InvalidGrammar where { show = showPPrint 4 4 . pPrint; }

instance PPrintable InvalidGrammar where
  pPrint (InvalidGrammar (t, loc, detail)) = concat
    [ maybe [] pPrint (endLocation loc) ++ [pSpace]
    , maybe [] (\t -> [pText "(in grammar for data type", pSpace, pShow t, pText ")", pSpace]) t
    , pPrint detail
    ]

instance NFData InvalidGrammar where
  rnf (InvalidGrammar (a, b, c)) = deepseq b $! deepseq c $! maybe () (const ()) a

invalidGrammarLens
  :: Monad m
  => Lens m InvalidGrammar (Maybe TypeRep, Location (), InvalidGrammarDetail)
invalidGrammarLens = newLens (\ (InvalidGrammar o) -> o) (\p _ -> InvalidGrammar p)

invalidGrammarTypeRep :: Monad m => Lens m InvalidGrammar (Maybe TypeRep)
invalidGrammarTypeRep = invalidGrammarLens >>> tuple0

invalidGrammarLocation :: Monad m => Lens m InvalidGrammar (Location ())
invalidGrammarLocation = invalidGrammarLens >>> tuple1

invalidGrammarDetail :: Monad m => Lens m InvalidGrammar InvalidGrammarDetail
invalidGrammarDetail = invalidGrammarLens >>> tuple2

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

instance Ord Probability where { compare a b = compare (computeLogProb a) (computeLogProb b); }

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
probSet cs = P_Mult (P_Coeff $ toInteger $ csetSize cs) probChar

-- | The probability that a string will match the beginning of a completely random string. It is the
-- probability of 'logP_Char' taken to the @n@th power where @n@ is the length of the string.
probText :: StrictText -> Probability
probText t = P_Pow probChar (toInteger $ Strict.length t)

----------------------------------------------------------------------------------------------------

-- | This class defines a predicate over 'RegexUnit's, 'Regex's, which asks, if the left and then
-- the right regular expressiosn were applied in series, and both would the combined regular
-- expression always fail? That is if you applied the left-hand regular expression and it succeds
-- but consumes all characters that the right-hand regular expression would have matched, the
-- combined regular expression will always fail.
--
-- For example, the regular expression @/(abc){1}/@ matches the string @"abc"@ one or more
-- (possibly infinite) number of times. If it were followed by the right-hand regular expression
-- @/(abc){2}/@ (matching @"abc"@ at least two times, and possibly infinite many times), then the
-- right-hand will never succeed because the left-hand regular expression has certainly consumed
-- all characters that may have matched the right-hand. So the @/(abc){1}/@ shadows @/(abc){2}/@
-- when they are applied in series.
-- 
-- But what about if left and right were reversed? If @/(abc){2}/@ were on the left and @/(abc){1}/@
-- were on the right, well @/(abc){2}/@ will always fail for the input @"abc"@, and we know that
-- @/(abc){1}/@ would have matched but will never have a chance to because the @/(abc){2}/@ will
-- have failed before having tried it. This is an example of mutually shadowing regular
-- expressions: no matter which order the two are tried, applying both regular expressions in
-- series is guaranteed to never allow the second to be tried before failure occurs.
--
-- Another example is when the left regular expression is the character set @/[a-z]+/@ (matches or
-- more lower-case letters), and the right regular expression is the string @/(abcdef)/@. In this
-- case, the left shadows the right and this predicate will evaluate to 'Prelude.True'. However if
-- they were flipped this predicate evaluates to 'Prelude.False' because, @/(abcdef)/@ could match
-- and then be followed by any number of lower-case letters which would be matched by @/[a-z]+/@.
class ShadowsSeries o where { shadowsSeries :: o -> o -> Bool; }

-- Only in the case of 'CharSet's is 'shadowsSeries' completely equivalent to 'shadowsParallel'
instance ShadowsSeries CharSet where { shadowsSeries = shadowsParallel; }

-- | This class defines a predicate over 'RegexUnit's and 'Regex's which asks, if first the left
-- regular expression and then the right regex were both applied to the same input string (in
-- parallel, so they both get the same input), then is it true that the the left regular expression
-- will match any input string that the right would match? If the two regular expressions were used
-- to construct a grammar where the right-hand regular expression is only tried in the case that the
-- left-hand regex does not match, would the left always succeed and the right would never have a
-- chance of parsing? In other words, does the left regular expression shadow the right regex when
-- they are applied in parallel?
-- 
-- For example, when a shorter string regular expression is a prefix string of a longer string
-- regex, @/(abc)/@ and @/(abcd)@/. If the left were tried first, it will succeed for any string
-- that would have matched @/(abcd)/@, so the regular expression @/(abcd)/@ would never have a
-- chance of succeeding, so the left shadows the right. If the regular expressions were reversed,
-- the input string @"abce"@ would not match @/(abcd)/@ but it would match @/(abc)/@, so if
-- @/(abcd)/@ were on the left, it would not be shadowing the regular expression @/(abc)/@.
--
-- In another example, the regular expression @/[abc]+/@ (matches one or more of the characters
-- @'a'@, @'b'@, or @'c'@) and @/[cde]+/@, these regular expressions are mutually non-shadowing.
-- Although they both have in common the character @'c'@, and a string like @"ccccccc"@ will match
-- both (thus in the event we receive that input string, the left regular expression will shadow
-- the right regex), this is not a case of shadowing because shadowing does not occur for all
-- possible input strings that could match the left regular expression. A regex only shadows in
-- parallel if there is a 100% chance any string that matches the left regular expression will do
-- so such that the right regex could never have a chance of being matched when converted to a
-- parser.
class ShadowsParallel o where { shadowsParallel :: o -> o -> Bool; }

instance ShadowsParallel CharSet where
  shadowsParallel (CharSet a) (CharSet b) = Iv.null $ Iv.delete b a

-- | This class defines a function that returns the interval 'Dao.Interval.Set' of characters for a
-- regular expression that an input string could start with that might match the whole regular
-- expression. For string regular expressions like @/(abc)/@, this function will evaluate to a
-- 'Dao.Interval.Set' containing a single character @'a'@. For character set regular expressions
-- like @/[abc]/@, this function will evaluate to a 'Dao.Interval.Set' containing all of the
-- characters @'a'@, @'b'@, and @'c'@.
class InitialCharSetOf o where { initialCharSetOf :: o -> CharSet; }

instance InitialCharSetOf CharSet where { initialCharSetOf = id; }

-- | The minimum length string that could possbly match the 'RegexUnit'. Strings shorter than
-- this value will definitely never match the 'RegexUnit'.
class RxMinLength o where { rxMinLength :: o -> Integer; }

-- | The maximum length string that could possbly match the 'RegexUnit'. A 'RegexUnit' will never
-- match more characters than this value, but could actually match fewer than this value.
-- 'Prelude.Nothing' indicates that it could match an infinite length string.
class RxMaxLength o where { rxMaxLength :: o -> Maybe Integer; }

----------------------------------------------------------------------------------------------------

-- | 'RegexRepeater's define whether a regex is applied just once, or applied many times. An
-- 'Dao.Interval.Interval' which may or may not be bounded. 'RegexRepeater's are ordered first by
-- the minimum number of characters that it could match, and then by the maximum number of
-- characters that it could match.
--
-- 'Regex' constructors like 'rxString', 'rxAnyOf', and 'rxChar' all require a 'RegexRepeater'
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
  toRegexRepeater (lo, hi) =
    if lo==1 && hi==1 then RxSingle else RxRepeat (min lo hi) (Just $ max lo hi)

instance ToRegexRepeater RepeatMinMax where
  toRegexRepeater o = case o of
    MAX o -> RxRepeat 0 (Just $ max 0 o)
    MIN o -> RxRepeat o Nothing

instance ToRegexRepeater (Count, Maybe Count) where
  toRegexRepeater (lo'', hi) = let lo' = max 0 lo'' in case hi of
    Nothing  -> RxRepeat lo' Nothing
    Just hi' -> let { hi = max lo' hi'; lo = min lo' hi; } in
      if lo==1 && hi==1 then RxSingle else RxRepeat lo (Just hi)
 
-- | Transform the 'RegexRepeater' into a pair of values: the 'Prelude.fst' value is the minimum
-- number of times a regular expression must match the input string in order for the match to
-- succeed. The 'Prelude.snd' value is the maximum number of times the regular expression may match
-- the input string, or 'Prelude.Nothing' if there is no maximum. Once the minimum number of matches
-- has been satisfied, the regular expression will never fail to match, therefore even if the input
-- string matches more than the given maximum number of repetitions, only the maximum number of
-- repetitions will be matched and the remainder of the input string will be left untouched.
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
  | RxChar  { rxGetCharSet :: CharSet   , rxGetRepeater :: RegexRepeater }
  deriving (Eq, Typeable)

_regexUnitToParser :: RegexUnit -> Parser st LazyText
_regexUnitToParser o = case o of
  RxString o rep -> do
    let zo  = Lazy.fromStrict o
    let check = string zo *> pure True <|> pure False
    let (lo, hi) = lexRepeaterToPair rep
    let init i keep = if i>=lo then return (i, keep) else check >>= \ok ->
          if ok then init (i+1) (keep<>zo) else unstring keep >> mzero
    let loop i keep = if not $ maybe True (i <) hi then return keep else check >>= \ok ->
          if ok then loop (i+1) (keep<>zo) else return keep
    init 0 mempty >>= uncurry loop
  RxChar cset rep -> do
    let (lo, hi) = lexRepeaterToPair rep
    init <- if lo==0 then pure mempty else count lo cset
    mappend init <$> maybe (munch cset) (flip noMoreThan cset) hi

instance Show RegexUnit where { show = showPPrint 4 4 . pPrint; }

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
    RxString o _ -> CharSet $ if Strict.null o then Iv.empty else Iv.singleton $ Strict.head o
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
          RxString  tA _ -> case b of
            RxString  tB _ -> isNothing hiA && tA==tB
            RxChar    _  _ -> False
          RxChar    tA _ -> isNothing hiA && case b of
            RxString  tB _ -> testNull $ csetDelete (anyOf $ Strict.unpack tB) tA
            RxChar    tB _ -> shadowsSeries tA tB

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
          RxChar   c r -> pPrint c ++ pPrint r

-- Used by the instantiation of 'ShadowsParallel' for both 'RegexUnit' and 'Regex'.
_rxUnit_shadowsParallel :: Bool -> RegexUnit -> RegexUnit -> Bool
_rxUnit_shadowsParallel isFinalItem a b = case a of
  RxString  tA opA -> case b of
    RxString  tB opB -> compLo opA opB && if not isFinalItem then tA==tB else case opA of
      RxSingle  -> case opB of
        RxSingle  -> Strict.isPrefixOf tA tB
        _         -> False
      _         -> False
    _                -> False
  RxChar    tA opA -> case b of
    RxChar    tB opB -> testNull (csetDelete tB tA) && compLo opA opB
    RxString  tB _   -> rxMinLength a <= rxMinLength b &&
      testNull (csetDelete (anyOf $ Strict.unpack tB) tA)
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

----------------------------------------------------------------------------------------------------

-- | This class provides a single function 'rx', which is used to construct a 'Regex' from either a
-- string-like value or from a 'CharSet'. You pass a list of 'Regex's constructed with 'rx' to the
-- 'Dao.Grammar.lexer' or 'Dao.Grammar.lexConst' functions to create 'Grammar's, which can further
-- be combined into 'Grammar's.
--
-- Also notice in the list of known instances to this class, each type, such as 'CharSet' or
-- 'Data.Text.Text', which instantiates this class also is instantiated paired with a
-- 'ToRegexRepeater' data type. This allows you to create 'Regex's that can match multiple times
-- using by pairing it with a repeater.
--
-- Matches the input "hello...":
--
-- @
-- 'rx' "hello"
-- @
--
-- Match "hellohellohello...":
--
-- @
-- 'rx'("hello", 3) -- 
-- @
--
-- Matches exactly one vowel character:
--
-- @
-- 'rx'('anyOf' "aeiou")
-- @
--
-- Matches 3 vowels in a row, but only if there are no more than 3.
--
-- @
-- 'rx'('anyOf' "aeiou", 'MAX' 3)
-- @
--
-- Matches exactly one character, no mattet what that character is:
--
-- @
-- 'rx' 'anyChar'
-- @
--
-- Matches one or more non-whitespace character:
--
-- @
-- 'rx'('noneOf' " \t\n\r\f\v", 'MIN' 1)
-- @
--
-- Matches a set of hexadecimal characters a minimum of 4 times and a maximum of 8 times:
--
-- @
-- 'rx'('within' [('0','9'),('A','F'),('a','f')], (4,8))
-- @
class ToRegex o where { rx :: o -> Regex; }

instance ToRegex RegexUnit where { rx = Regex . array . return; }

instance ToRegex CharSet where
  rx = rx . flip RxChar (toRegexRepeater $ Count 1)

instance ToRegexRepeater rep => ToRegex (CharSet, rep) where
  rx (o, rep) = rx $ RxChar o (toRegexRepeater rep)

instance ToRegex Strict.Text where
  rx = rx . flip RxString (toRegexRepeater $ Count 1)

instance ToRegexRepeater rep => ToRegex (Strict.Text, rep) where
  rx (o, rep) = rx $ RxString o (toRegexRepeater rep)

instance ToRegex Lazy.Text where
  rx = rx . Lazy.toStrict

instance ToRegexRepeater rep => ToRegex (Lazy.Text, rep) where
  rx (o, rep) = rx $ RxString (Lazy.toStrict o) (toRegexRepeater rep)

instance ToRegex String where { rx = rx . toText; }

instance ToRegexRepeater rep => ToRegex (String, rep) where
  rx (o, rep) = rx (toText o, rep)

instance ToRegex Char where { rx = rx . Strict.singleton; }

instance ToRegexRepeater rep => ToRegex (Char, rep) where
  rx (o, rep) = rx (Strict.singleton o, rep)

-- | A sequence of 'RegexUnit's for building more complex regular expressions patterns that can be
-- used to match strings. This data type instantiates 'Prelude.Ord' in the same way that 'RegexUnit'
-- instantiates 'Prelude.Ord', that is, if both 'Regex' values are applied to the same input string,
-- the first 'Regex' is greater than the second if the first consumes all or more of the input
-- string that the second will consume.
newtype Regex = Regex { regexUnits :: Array RegexUnit } deriving (Eq, Show, Typeable)

instance PPrintable Regex where
  pPrint (Regex o) = case elems o of
    [] -> [pText "\"\""]
    ox -> [pInline $ [pText "\""] ++ (ox>>=pPrint) ++ [pText "\""]]

instance Ord Regex where
  compare a b = compare (computeLogProb $ probRegex a) (computeLogProb $ probRegex b)

instance Monoid (Predicate InvalidGrammar Regex) where
  mempty = Backtrack
  mappend a b = case a of
    Backtrack    -> b
    PFail     _  -> a
    OK (Regex a) -> case b of
      Backtrack    -> OK (Regex a)
      PFail     _  -> b
      OK (Regex b) -> fromMaybe Backtrack $ msum
        [do unitA <- lastElem a -- If a is empty, we stop here.
            unitB <- b!0 -- If b is empty, we stop here.
            -- a and b are not empty, we can safely assume at least one element can be indexed
            let items arr rng = catMaybes $ (arr !) <$> range rng
            if shadowsSeries unitA unitB
            then Just $ PFail $ InvalidGrammar $
              (Nothing, NoLocation (), RegexShadowError SequenceRegex (Regex a) (Regex b))
            else Just $ do
              rxAB <- _rxSeq unitA unitB
              OK $ Regex $ array $ concat
                [items a (0, size a - 2), maybe [unitA, unitB] return rxAB, items b (1, size b - 1)]
        , lastElem a >> Just (OK $ Regex a)
        , b!0 >> Just (OK $ Regex b)
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
   lastElem a >>= \rxA -> b!0 >>= \rxB -> Just $ shadowsSeries rxA rxB

instance TestNull Regex where
  nullValue = Regex nullValue
  testNull (Regex a) = testNull a

instance RxMinLength Regex where
  rxMinLength (Regex o) = sum $ rxMinLength <$> elems o

instance RxMaxLength Regex where
  rxMaxLength (Regex o) = fmap sum $ sequence $ rxMaxLength <$> elems o

instance InitialCharSetOf Regex where
  initialCharSetOf (Regex o) = maybe mempty initialCharSetOf (o ! 0)

_emptyRegexSeq :: Regex
_emptyRegexSeq = Regex mempty

-- | Returns the 'Probability' value a 'Regex' will match a completely random input string.
probRegex :: Regex -> Probability
probRegex (Regex o) = foldl (\p -> P_Mult p . probRegexUnit) (P_Prob 1) (elems o)

-- Tries to merge two 'RegexUnit's together on the assumption that each 'RegexUnit' will be appended
-- in series. This merges non-repeating strings, identical strings that repeat, and any two
-- character sets as long as the latter character set is a subset of the former and there are no
-- maximum repeition limitations.
_rxSeq :: RegexUnit -> RegexUnit -> Predicate InvalidGrammar (Maybe RegexUnit)
_rxSeq a b = case a of
  RxString tA RxSingle -> case b of
    RxString tB RxSingle -> return $ Just $ RxString (tA<>tB) RxSingle
    _                    -> let mkrx = Regex . array . return in
      if not $ shadowsSeries a b then return Nothing else throwError $ InvalidGrammar
        (Nothing, NoLocation (), RegexShadowError ParallelRegex (mkrx a) (mkrx b))
  _                      -> return Nothing

-- | Construct a 'Regex' from a list of 'RegexUnit'.
concatRegexUnits :: [RegexUnit] -> Predicate InvalidGrammar Regex
concatRegexUnits = loop [] where
  loop stk ox = case ox of
    []     -> return $ Regex $ array stk
    [o]    -> return $ Regex $ array (stk++[o])
    a:b:ox -> _rxSeq a b >>= \ab -> case ab of
      Just ab -> loop stk (ab:ox)
      Nothing -> loop (stk++[a]) (b:ox)

----------------------------------------------------------------------------------------------------

data RegexTable o
  = RegexTable
    { regexTableElements :: Array (Regex, o)
    , regexTable         :: A.Array Char (Array (Regex, o))
    }

instance Functor RegexTable where
  fmap f (RegexTable a b) = RegexTable (fmap (fmap f) a) (fmap (fmap (fmap f)) b)

instance Monoid (Predicate InvalidGrammar (RegexTable o)) where
  mempty = mzero
  mappend a b = do
    (RegexTable rxA tabA) <- a
    (RegexTable rxB tabB) <- b
    if arrayIsNull rxA then b else if arrayIsNull rxB then a else do
      let alast = lastElem rxA
      let bfrst = rxB!0
      checkRegexChoices $ maybe [] (fmap fst) $ msum
        [(\a b -> [a,b]) <$> alast <*> bfrst, return <$> alast, return <$> bfrst]
      let interv  = uncurry Iv.interval . A.bounds
      let newbnds = Iv.toBoundedPair $ Iv.envelop (interv tabA) (interv tabB)
      return $ RegexTable (rxA<>rxB) $
        A.accumArray mappend mempty newbnds $ A.assocs tabA ++ A.assocs tabB

-- | Given a list of 'Regex's that may be evaluated in parallel, make sure no one 'Regex' shadows
-- any other 'Regex' that occurs after it.
checkRegexChoices :: [Regex] -> Predicate InvalidGrammar ()
checkRegexChoices = loop Nothing where
  loop keep ox = case ox of
    []    -> return ()
    r2:ox -> case keep of
      Nothing -> loop (Just r2) ox
      Just r1 ->
        if shadowsParallel r1 r2
        then PFail $ InvalidGrammar (Nothing, NoLocation (), RegexShadowError ParallelRegex r1 r2)
        else loop (Just r2) ox

-- | Every 'Regex' contains a 'Regex', and every 'Regex' can match some range of characters. This
-- function creates an 'Data.Array.IArray.Array' mapping every character that could possibly match a
-- list of 'Regex's. The result is that with a single character look-ahead, you can do a O(1)
-- (constant time) lookup of the 'Regex's that will be able to evaluate successfully, minimizing
-- backtracking.
regexsToTable :: [(Regex, o)] -> Predicate InvalidGrammar (RegexTable o)
regexsToTable ox = do
  checkRegexChoices $ fst <$> ox
  guard $ not $ null ox
  let cset = mconcat $ initialCharSetOf . fst <$> ox
  bnds@(lo, hi) <- maybe mzero return $ charSetBounds cset
  guard $ minBound/=lo || hi/=maxBound
  return $ RegexTable (array ox) $ fmap array $ A.accumArray (++) [] bnds $ do
    regex <- ox
    c <- charSetRange $ initialCharSetOf $ fst regex
    [(c, [regex])]

regexTableToParser :: (Monad m, MonadParser m) => RegexTable o -> m (LazyText, o)
regexTableToParser (RegexTable _ arr) = do
  c <- look1
  guard $ inRange (A.bounds arr) c
  msum $ do
    (regex, o) <- elems $ arr A.! c
    [liftM2 (,) (regexToParser regex) $ return o]

----------------------------------------------------------------------------------------------------

-- | A convenient type synonym for the lazy 'Data.Text.Lazy.Text' data type.
type ParseInput = LazyText

-- | Construct this type with 'parserState' and pass the value to 'runParser'.
data ParserState st
  = ParserState LazyText st TextPoint CharCount CharCount Int

instance Functor ParserState where
  fmap z (ParserState a b c d e f) = ParserState a (z b) c d e f

instance TestNull st => TestNull (ParserState st) where
  nullValue = parserState nullValue
  testNull o = Lazy.null (o & inputString) && (testNull $ o & userState) &&
    TextPoint 1 1 == (o & textPoint) && 0 == (o & charCount) && 0 == (o & precedence)

parserStateLens :: Monad m => Lens m (ParserState st) (LazyText, st, TextPoint, CharCount, CharCount, Int)
parserStateLens =
  newLens (\ (ParserState a b c d e f) -> (a, b, c, d, e, f))
          (\ (a, b, c, d, e, f) _ -> ParserState a b c d e f)

inputString :: Monad m => Lens m (ParserState st) LazyText
inputString = parserStateLens >>> tuple0

userState :: Monad m => Lens m (ParserState st) st
userState = parserStateLens >>> tuple1

textPoint :: Monad m => Lens m (ParserState st) TextPoint
textPoint = parserStateLens >>> tuple2

charCount :: Monad m => Lens m (ParserState st) CharCount
charCount = parserStateLens >>> tuple3

pushBackCount :: Monad m => Lens m (ParserState st) CharCount
pushBackCount = parserStateLens >>> tuple4

precedence :: Monad m => Lens m (ParserState st) Int
precedence = parserStateLens >>> tuple5

-- | Define a 'ParserState' that can be used to evaluate a 'Parser' monad. The state value can be
-- anything you choose. 'Parser' instantiates the 'Control.Monad.State.MonadState' class so you can
-- keep track of a pure state for whatever parsing you intend to do.
parserState :: st -> ParserState st
parserState st = ParserState nullValue st (TextPoint 1 1) 0 0 minPrec

-- | Report how efficiently this parser ran. Efficiency is measured by the number of characters
-- taken from the input divided by the number of characters that had to be pushed back onto the
-- input due to backtracking.
efficiency :: ParserState st -> Double
efficiency st = fromRational $
  (fromIntegral $ st & charCount) % (max 1 $ fromIntegral $ st & pushBackCount)

----------------------------------------------------------------------------------------------------

-- | A parser that operates on an input string of the lazy 'Data.Text.Lazy.Text' data type.
newtype Parser st a = Parser { _runParse :: PredicateT InvalidGrammar (State (ParserState st)) a }

instance Functor (Parser st) where
  fmap f (Parser o) = Parser $ fmap f o

instance Monad (Parser st) where
  return = Parser . return
  (Parser a) >>= b = Parser $ a >>= _runParse . b
  fail msg = getTextPoint >>= \loc ->
    throwError $ InvalidGrammar (Nothing, EndLocation () loc, OtherFailure $ Strict.pack msg)

instance MonadPlus (Parser st) where
  mzero = Parser mzero
  mplus (Parser a) (Parser b) = Parser $ mplus a b

instance Applicative (Parser st) where { pure = return; (<*>) = ap; }

instance Alternative (Parser st) where { empty = mzero; (<|>) = mplus; }

instance MonadError InvalidGrammar (Parser st) where
  throwError = Parser . throwError
  catchError (Parser p) catch = Parser $ catchError p (_runParse . catch)

instance PredicateClass InvalidGrammar (Parser st) where
  predicate = Parser . predicate
  returnPredicate (Parser try) = Parser $ try >>= return . OK

instance MonadState st (Parser st) where
  state f = Parser $ lift $ state $ \st ->
    let (a, ust) = f (st & userState) in (a, on st [userState <~ ust])

instance MonadParser (Parser st) where
  look      = Parser $ lift $ gets (& inputString)
  look1     = _take $ \t -> if Lazy.null t then (0, Nothing) else (1, return (Lazy.head t, 1, t))
  get1      = _take $ \t -> (1, guard (not $ Lazy.null t) >> return (Lazy.head t, 1, Lazy.tail t))
  string  s = _take $ \t -> let len = stringLength s in (len, (,,) s len <$> Lazy.stripPrefix s t)
  munch   f = _take $ \t ->
    let (keep, rem) = Lazy.span (csetMember f) t
        len = stringLength rem
    in  (min len $ if Lazy.null t then 0 else 1, return (keep, len, rem))
  eof       = look >>= guard . Lazy.null
  pfail err = do
    loc <- getTextPoint
    throwError $ on err [invalidGrammarLocation $= mappend (EndLocation () loc)]
  count c p = _take $ \t -> (,) c $ do
    (keep, t) <- pure $ Lazy.splitAt (countToInt c) t
    let len = Lazy.length keep
    keep <- pure $ Lazy.takeWhile (csetMember p) keep
    guard $ len == Lazy.length keep
    return (keep, c, t)
  regex = _regexToParser
  noMoreThan lim p = _take $ \t ->
    let loop i keep t = let c = Lazy.head t in
          if Lazy.null t || i>=lim || not (csetMember p c)
          then (i, Just (keep, i, t))
          else loop (seq i $! i+1) (Lazy.snoc keep c) (Lazy.tail t)
    in  loop 0 Lazy.empty t
  incrementPushBackCounter c = Parser $ lift $ modify $ by [pushBackCount $= (+ c)]
  incrementCharCounter     c = Parser $ lift $ modify $ by [charCount     $= (+ c)]

instance MonadPrecedenceParser (Parser st) where
  prec c p = Parser (lift $ gets (& precedence)) >>= guard . (c >=) >> _modPrecedence (const c) p
  step   p = _modPrecedence succ p
  reset  p = _modPrecedence (const 0) p

instance MonadSourceCodeParser (Parser st) where
  getTextPoint = Parser $ lift $ gets (& textPoint)
  setTextPoint = const $ return ()

instance MonadBacktrackingParser (Parser st) where
  unstring t = _take $ \instr -> let len = stringLength t in (len, Just ((), negate len, t<>instr))

-- | Get the number of characters parsed so far.
getCharCount :: Parser st CharCount
getCharCount = Parser $ lift $ gets (& charCount)

minPrec :: Int
minPrec = 0

_take :: (Lazy.Text -> (Count, Maybe (a, Count, Lazy.Text))) -> Parser st a
_take f =
  ( Parser $ lift $ state $ \st ->
      let (pb, o) = f $ st & inputString
          updPBC = pushBackCount $= (+ pb)
      in  case o of
            Nothing           -> (Nothing, on st [updPBC])
            Just (o, cc, rem) -> (Just  o, on st [inputString <~ rem, charCount $= (+ cc), updPBC])
  ) >>= maybe mzero return

-- | Run the parser with a given 'ParserState' containing the input to be parsed and the
-- initializing state. The whole 'ParserState' is returned with the parsing result
-- 'Dao.Predicate.Predicate'.
runParser :: Parser st a -> ParserState st -> (Predicate InvalidGrammar a, ParserState st)
runParser (Parser p) = runState (runPredicateT p)

_modPrecedence :: (Int -> Int) -> Parser st o -> Parser st o
_modPrecedence f p = do
  c <- Parser $ lift $ gets (& precedence)
  Parser $ lift $ modify $ by [precedence <~ f c]
  let reset = Parser $ lift $ modify $ by [precedence <~ c]
  o <- catchError (optional p) (\e -> reset >> throwError e)
  reset >> maybe mzero return o

-- | Transform a 'Parser' monad into some other monadic function. This is useful for converting
-- 'Parser' monad to a 'Prelude.ReadS' function that takes a precedence value. This is ideal for
-- convering a 'Parser' directly to a function that can be used to instantiate 'Prelude.readsPrec'
-- for the 'Prelude.Read' class. It can also be converted to a 'Text.ParserCombinators.ReadP.ReadP'
-- or 'Text.ParserCombinators.ReadPrec.ReadPrec' parser.
--
-- For example, if you have a 'Parser' function called @myParse@ of type:
--
-- > myParse :: 'Parser' () MyValue
--
-- you could use it to instantiate the 'Prelude.readsPrec' function for @MyValue@ like so:
--
-- > instance 'Prelude.Read' MyValue where { 'Prelude.readsPrec' = myParse () }
parser_to_S :: MonadPlus m => Parser st a -> st -> Int -> String -> m (a, String)
parser_to_S (Parser p) init prec instr = do
  let (result, st) = runState (runPredicateT p) $
        on (parserState init) [precedence <~ prec, inputString <~ Lazy.pack instr]
  case result of
    PFail err -> fail (show err)
    Backtrack -> mzero
    OK     a  -> return (a, Lazy.unpack $ st & inputString)

-- | This function lets you use the 'Parser' data type provided in this module within any abstract
-- 'MonadParser', even parsers like 'Text.ParserCombinators.ReadP' and
-- 'Text.ParserCombinators.ReadPrec'. It essentially takes a concrete 'Parser' type and transforms
-- it into any other polymorphic parser type that instantiates 'MonadParser'.
--
-- The 'MonadParser' class cannot perform "backtracking" as in pushing an arbitrary string back into
-- the input stream. The 'MonadParser' class was designed this way because it was necessary to
-- include 'Text.ParserCombinator.ReadP' and 'Text.ParserCombinator.ReadPrec' as members of the
-- 'MonadParser' class.
--
-- In a lazy langauage like Haskell, backtracking is done not by pushing back, but by simply looking
-- ahead with 'look'. Branching using 'Control.Monad.mplus' causes the current input string to be
-- copied lazily -- the lazy copy only contains the observed characters and not the entire input.
-- If the branch fails, the lazily copied string is disgarded while the original string has been
-- left untouched, and this is called backtracking.
--
-- But some parsing algorithms, for example the instance of 'regex' for the 'Parser' type defined in
-- this module, may want to do push-back backtracking by using 'Parser's own specific instantiation
-- of 'unstring', even if the abstract parser implementation does not instantiate 'unstring'.
-- Wouldn't it be nice if 'unstring' could be used for all abstract 'MonadParser's, not just the
-- ones that bother to instantiate 'MonadBacktrackingParser'?
--
-- That is what 'lookAheadWithParser' does. To perform push-back backtracking, the 'look' function
-- is used to look-ahead, taking the entire input string (lazily) and placing it into a
-- "Data.Text.Lazy" 'Data.Text.Lazy.Text' data structure, and then using the lazy text data as the
-- input to a "Dao.Regex" 'Parser'. The 'lookAheadWithParser' function can then be reused to perform
-- the parsing on the string retrieved from 'look'. If the 'Parser' succeeds, the 'count' function
-- is used to remove the length of parsed string from the polymorphic 'MonadParser'. If it fails,
-- "backtracking" is performed by simply not doing anything -- since the parse occurred on the
-- 'look'-ahead lazily copied string, nothing was actually removed from the 'MonadParser's input.
--
-- Be warned that though it is possible to transform a 'Parser' into another identical 'Parser' with
-- this function there is absolutely no reason to ever do this, and if you do you will experience a
-- performance loss. So do not ever do something like this:
--
-- @
-- myParser :: Parser MyState MyData
-- myParser = 'Dao.Grammar.grammarToParser' $ ...
--
-- myAbstractParser :: MonadParser m => m MyData
-- myAbstractParser = lookAheadWithParser myParser $ initMyState{ ... }
--
-- main :: IO () -- use the abstract parser with 'runParser'
-- main = case 'runParser' myAbstractParser $ initMyState{ ... } of
--     'Dao.Predicate.OK' myData -> 'Prelude.print' myData
--     _ -> 'Control.Monad.fail' "could not parse"
-- @
--
-- The @myAbstractParser@ function is an abstract 'MonadParser' constructed with
-- 'lookAheadMonadParser'. But using @myAbstractParser@ with 'runParser' tells Haskell to transform
-- the abstrat parser into a concrete 'Parser', so you have essentially transformed a 'Parser' into
-- an abstract 'MonadParser' and back into a 'Parser' again. This is what you should avoid. The
-- above @main@ function should be this instead:
--
-- @
-- main :: IO () -- use myParser, not myAbstractParser, with 'runParser'.
-- main = case 'runParser' myParser $ initMyState{ ... } of
--     'Dao.Predicate.OK' myData -> 'Prelude.print' myData
--     _ -> 'Control.Monad.fail' "could not parse"
-- @
lookAheadWithParser
  :: (Monad m, MonadPlus m, MonadParser m)
  => Parser st LazyText -> st -> m (LazyText, st)
lookAheadWithParser p init = do
  str <- look
  let (result, st) = runParser p $ on (parserState init) [inputString <~ str]
  let pushback = incrementPushBackCounter $ (st & pushBackCount) + (st & charCount)
  case result of
    Backtrack -> pushback >> mzero
    PFail err -> pushback >> pfail err >> fail (showPPrint 4 4 $ pPrint err)
    OK      o -> do
      incrementCharCounter (st & charCount)
      noMoreThan (st & charCount) anyChar
      return (o, st & userState)

