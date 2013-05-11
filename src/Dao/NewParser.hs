-- "src/Dao/NewParser.hs"  a parser for defining general context-free
-- grammars that are parsed in two phases: the lexical and the
-- syntactic analysis phases.
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

module Dao.NewParser where

import           Dao.String
import qualified Dao.Token as L
import           Dao.Predicate
import           Dao.Object  hiding (Tokenizer)

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error

import           Data.Monoid
import           Data.Maybe
import           Data.Word
import           Data.Char  hiding (Space)
import           Data.List

import           System.IO

----------------------------------------------------------------------------------------------------
-- Important data types for lexical analysis.

-- | This is a basic token typing system suitable for most parsers. Your lexer does not need to
-- use all of these types. These types are semi-arbitrary labels you can use however you please.
-- Keep in mind, however, that functions in this module might make assumptions as to the kind of
-- characters stored along side these token types. For example: 'mightHaveNLorTabsTT' assumes that
-- 'Space' and 'StrLit' may contain newline characters and 'Keyword and 'Identifier' will not.
data TT
  = Space
  | Newline
      -- ^ your lexer might return '\n' characters as 'Space's, but this token type is here in
      -- case you need to treat newlines as special characters.
  | Indent
      -- ^ your lexer might return '\t' characters as 'Space's, but this token type is here in
      -- case you need to treat indentation as special characters.
  | Alphabetic
  | Alnum -- ^ alphabetical or numerical (digits)
  | Keyword
  | Identifier  -- ^ same as 'Label', unless you treat it differently
  | Label       -- ^ same as 'Identifier', unless you treat it differently
  | Digits
  | Number      -- ^ a whole number including floating points, not just digits.
  | PointDigits -- ^ digits with a decimal point
  | OctDigits
  | HexDigits
  | Punct       -- ^ punctuation mark, not including mathematical symbols
  | Symbol      -- ^ a symbol (like punctuation)
  | Operator
  | CharLit     -- ^ a character literal treated as a single token
  | StrLit      -- ^ a string literal treated as a single token
  | ComEndl     -- ^ a comment at the end of the line
  | ComInln     -- ^ the start of a multi-line comment
  | Opener      -- ^ an open-paren or brace or anything that opens a closure
  | Closer      -- ^ a close-paren or brace or anything that closes a closure
  | CtrlChar    -- ^ ascii control characters
  | Arbitrary   -- ^ a type used to mark arbitrary data
  | Ignored     -- ^ characters that can be ignored, like in multi-line comments.
  | Unknown
      -- ^ it is possible to construct tokenizers that never fail by creating a 'Tokenizer' that
      -- returns unknown tokens paired with this type. This 'Unknown' 'Tokenizer' should always be
      -- the final 'Tokenizer' in your list of 'Tokenizers' passed to 'lex'. Then your 'Parser' can
      -- decide how to handle these.
  deriving (Eq, Ord, Enum, Show)

data GenToken tok = GenToken { tokType :: tok, tokToUStr :: UStr }
type Token = GenToken TT
instance Show tok => Show (GenToken tok) where
  show tok = show (tokType tok) ++ " " ++ show (tokToUStr tok)
instance (Eq tok, Enum tok) => CanMapTokens tok GenToken where
  fmapTokens conv tok = tok{tokType = conv (tokType tok)}

class HasLineNumber   a where { lineNumber   :: a -> Word }
class HasColumnNumber a where { columnNumber :: a -> Word }

class (Eq tok, Enum tok) => CanMapTokens tok m where
  fmapTokens :: (Eq tok', Enum tok') => (tok -> tok') -> m tok -> m tok'

data GenTokenAt tok =
  GenTokenAt{ tokenAtLineNumber :: Word, tokenAtColumnNumber :: Word, getToken :: GenToken tok }
type TokenAt = GenTokenAt TT
instance HasLineNumber   TokenAt where { lineNumber   = tokenAtLineNumber   }
instance HasColumnNumber TokenAt where { columnNumber = tokenAtColumnNumber }

data GenLine tok
  = GenLine
    { lineLineNumber :: Word
    , lineTokens     :: [(Word, GenToken tok)]
      -- ^ a list of tokens, each with an associated column number.
    }

instance (Eq tok, Enum tok) => CanMapTokens tok GenLine where
  fmapTokens conv (GenLine a list) = GenLine a (map (\ (b, tok) -> (b, fmapTokens conv tok)) list)

type Line = GenLine TT

instance HasLineNumber (GenLine tok) where { lineNumber = lineLineNumber }

instance Show tok => Show (GenLine tok) where
  show line = show (lineLineNumber line) ++ ": " ++ show (lineTokens line)

----------------------------------------------------------------------------------------------------

-- | This is the state used by every 'GenLexer'. There is no @GenLexerState@, there is only this
-- data type because the 'LexerState' does not store any 'GenToken's, only strings.
data LexerState
  = LexerState
    { lexBuffer :: String
      -- ^ stores the characters consumed by 'GenLexer's. This buffer is never cleared until
      -- 'makeToken' is evaluated. Retrieve this string using:
      -- > 'Control.Monad.State.gets' 'lexBuffer'
    , lexInput  :: String
      -- ^ contains the remainder of the input string to be analyzed. Retrieve this string using:
      -- > 'Control.Monad.State.gets' 'lexInput'
    }

-- | Create a new lexer state using the given input 'Prelude.String'. This is only realy useful if
-- you must evaluate 'runLexerState'.
newLexerState :: String -> LexerState
newLexerState input = LexerState{ lexBuffer = "", lexInput = input }

-- | The 'GenLexer' is very similar in many ways to regular expressions, however 'GenLexer's always
-- begin evaluating at the beginning of the input string. The 'lexicalAnalysis' phase of parsing
-- must generate 'GenToken's from the input string. 'GenLexer's provide you the means to do with
-- primitive functions like 'lexString', 'lexChar', and 'lexUntil', and combinators like 'lexMany',
-- 'lexMany1', and 'lexUntilTermChar'. These primitive functions collect characters into a buffer,
-- and you can then empty the buffer and use the buffered characters to create a 'GenToken' using
-- the 'makeToken' function.
-- 
-- 'GenLexer' instantiates 'Control.Monad.State.MonadState', 'Control.Monad.Error.MonadError',
-- 'Control.Monad.MonadPlus', and of course 'Control.Monad.Monad' and 'Data.Functor.Functor'. The
-- 'Control.Monad.fail' function is overloaded such that it does not evaluate to an exception, such
-- that it can halt 'lexecialAnalysis' and also provide useful information about the failure.
-- 'Control.Monad.Error.throwError' can also be used, and 'Control.Monad.Error.catchError' will
-- catch errors thrown by 'Control.Monad.Error.throwError' and 'Control.Monad.fail'.
-- 'Control.Monad.mzero' causes backtracking. Be careful when recovering from backtracking using
-- 'Control.Monad.mplus' because the 'lexBuffer' is not cleared. It is usually better to backtrack
-- using 'lexBacktrack' (or don't backtrack at all, because it is inefficient). However you don't
-- need to worry too much; if a 'GenLexer' backtracks while being evaluated in 'lexicalAnalysis' the
-- 'lexInput' will not be affected at all and the 'lexBuffer' is ingored entirely.
newtype GenLexer tok a = GenLexer { runLexer :: PTrans (GenParserErr tok) (State LexerState) a }
instance (Eq tok, Enum tok) => Functor (GenLexer tok) where { fmap fn (GenLexer lex) = GenLexer (fmap fn lex) }
instance (Eq tok, Enum tok) => Monad (GenLexer tok) where
  (GenLexer fn) >>= mfn = GenLexer (fn >>= runLexer . mfn)
  return                = GenLexer . return
  fail msg              = GenLexer (throwError ((parserErr 0 0){parserErrMsg = Just (ustr msg)}))
instance (Eq tok, Enum tok) => MonadPlus (GenLexer tok) where
  mplus (GenLexer a) (GenLexer b) = GenLexer (mplus a b)
  mzero                           = GenLexer mzero
instance (Eq tok, Enum tok) => MonadState LexerState (GenLexer tok) where
  get = GenLexer (lift get)
  put = GenLexer . lift . put
instance (Eq tok, Enum tok) => MonadError (GenParserErr tok) (GenLexer tok) where
  throwError                        = GenLexer . throwError
  catchError (GenLexer try) catcher = GenLexer (catchError try (runLexer . catcher))
instance (Eq tok, Enum tok) => ErrorMonadPlus (GenParserErr tok) (GenLexer tok) where
  catchPValue (GenLexer try) = GenLexer (catchPValue try)
  assumePValue               = GenLexer . assumePValue

-- | Throughout this module, there are @Gen@/Thing/s (general /Thing/s) and regular /Thing/s, for
-- example 'GenLexer' and 'Lexer'. 'GenLexer's allow you to specify your own type of tokens as long
-- as the token type instantiates 'Prelude.Eq' and 'Prelude.Enum' (which can be derived using the
-- @deriving@ keyword). Regular 'Lexer' uses the 'TT' token type provided in this module, which
-- should be enough for most purposes; rarely should you ever need your own token type.
type Lexer a = GenLexer TT a

-- | A 'GenTokenizer' or 'Tokenizer' is a special form of 'GenLexer' which is essential to
-- 'lexicalAnalysis'. There are three "laws" that a 'GenTokenizer' must satisfy:
-- 1. 'GenTokenizers' are monads, but the final monadic binding must be the 'makeToken' function, or
--     some function that returns 'makeToken'. Returning your own 'GenToken' using monadic
--     'Control.Monad.return' is possible but should be considered bad style.
-- 2. 'GenTokenizers' *must always* consume at least one character if it does not backtrack. This
--    law cannot be enforced in the type system, although functions like 'makeToken', 'runTokenizer'
--    and the 'lexicalAnalysis' function both check that a tokenizer always consumes one token and
--    forces backtracking if it does not. However in places where 'makeToken' or 'lexicalAnalysis'
--    are not evaluated, tokenizers that return empty tokens might result in infinite loops.
-- 3. The line positions of tokens is computed by the 'lexicalAnalysis' phase, which assumes that
--    *every token* generated by a 'GenTokenizer' will return a token containing exactly the
--    characters lexed-off of the input string, nothing more and nothing less. It is therefore
--    important that space characters also be treated as tokens, and that no characters are
--    thrown-away during lexing.  Lexers can return more than one token, so if necessary, return an
--    'Ignored' token with tokens you don't need along with a second token containing the important
--    characters, then drop all 'Ignored' tokens during 'syntacticAnalysis'. 
type GenTokenizer tok = GenLexer tok [GenToken tok]

type Tokenizer = GenTokenizer TT

-- Evaluates to 'Prelude.True' if the 'TT' token might have newline or tab characters and thus
-- require it be scanned for updating the line or column counts more scrupulously. This is an
-- optimization. If the 'lex'er needs to check every token for newlines, even if tokens that are
-- /defined/ to never lexSimple newlines, checking every character within these tokens is a waste of
-- time. This function is useful to tell the lexer which tokens it need not worry about when
-- updating the newline table. However, if you mistakenly tell the lexer that a token does not have
-- newlines when it actually does, the lexer will not update the line information correctly. If your
-- parser is showing line numbers that are always less than the actual lines numbers, this is a good
-- place to check for debugging.
mightHaveNLorTabsTT :: Token -> Bool
mightHaveNLorTabsTT tok = case tokType tok of
  Alphabetic  -> False
  Keyword     -> False
  Alnum       -> False
  Digits      -> False
  PointDigits -> False
  OctDigits   -> False
  HexDigits   -> False
  Symbol      -> False
  Punct       -> False
  Operator    -> False
  Identifier  -> False
  Opener      -> False
  Closer      -> False
  _           -> True

-- | Append the first string parameter to the 'lexBuffer', and set the 'lexInput' to the value of
-- the second string parameter. Most lexers simply takes the input, breaks it, then places the two
-- halves back into the 'LexerState', which is what this function does. *Be careful* you don't pass
-- the wrong string as the second parameter. Or better yet, don't use this function.
lexSetState :: (Eq tok, Enum tok) => String -> String -> GenLexer tok ()
lexSetState got remainder = modify (\st -> st{lexBuffer = lexBuffer st ++ got, lexInput = remainder})

-- | Unlike simply evaluating 'Control.Monad.mzero', 'lexBacktrack' will push the contents of the
-- 'lexBuffer' back onto the 'lexInput'. This is inefficient, so if you rely on this too often you
-- should probably re-think the design of your lexer.
lexBacktrack :: (Eq tok, Enum tok) => GenLexer tok ig
lexBacktrack = modify (\st -> st{lexBuffer = "", lexInput = lexBuffer st ++ lexInput st}) >> mzero

-- | A fundamental 'Lexer', uses 'Data.List.break' to break-off characters from the input string
-- until the given predicate evaluates to 'Prelude.True'. Backtracks if no characters are lexed.
-- See also: 'charSet' and 'unionCharP'.
lexWhile :: (Eq tok, Enum tok) => (Char -> Bool) -> GenLexer tok ()
lexWhile predicate = do
  (got, remainder) <- fmap (span predicate) (gets lexInput)
  if null got then mzero else lexSetState got remainder

-- | Like 'lexUnit' but inverts the predicate, lexing until the predicate does not match. This
-- function is defined as:
-- > \predicate -> 'lexUntil' ('Prelude.not' . predicate)
-- See also: 'charSet' and 'unionCharP'.
lexUntil :: (Eq tok, Enum tok) => (Char -> Bool) -> GenLexer tok ()
lexUntil predicate = lexWhile (not . predicate)

-- | Create a 'GenToken' using the contents of the 'lexBuffer', then clear the 'lexBuffer'. This
-- function backtracks if the 'lexBuffer' is empty -- you must never create null tokens.
makeToken  :: (Eq tok, Enum tok) => tok -> GenLexer tok [GenToken tok]
makeToken tok = do
  str <- gets lexBuffer
  if null str then mzero else modify (\st -> st{lexBuffer=""}) >> return [GenToken tok (ustr str)]

-- | A fundamental lexer using 'Data.List.stripPrefix' to check whether the given string is at the
-- very beginning of the input.
lexString :: (Eq tok, Enum tok) => String -> GenLexer tok ()
lexString str =
  gets lexInput >>= assumePValue . maybeToBacktrack . stripPrefix str >>= lexSetState str

-- | A fundamental lexer succeeding if the next 'Prelude.Char' in the 'lexInput' matches the
-- given predicate. See also: 'charSet' and 'unionCharP'.
lexCharP ::  (Eq tok, Enum tok) => (Char -> Bool) -> GenLexer tok ()
lexCharP predicate = gets lexInput >>= \input -> case input of
  c:input | predicate c -> lexSetState [c] input
  _                     -> mzero

-- | Succeeds if the next 'Prelude.Char' on the 'lexInput' matches the given 'Prelude.Char'
lexChar :: (Eq tok, Enum tok) => Char -> GenLexer tok ()
lexChar c = lexCharP (==c)

-- | *Not a 'GenLexer'* but useful when passed as the first parameter to 'lexCharP', 'lexWhile' or
-- 'lexUntil'. This function creates a predicate over 'Prelude.Chars's that evaluates to
-- 'Prelude.True' if the 'Prelude.Char' is equal to any of the 'Prelude.Char's in the given
-- 'Prelude.String'. This is similar to the behavior of character sets in POSIX regular expressions:
-- the Regex @[abcd]@ matches the same characters as the predicate @('charSet' "abcd")@
charSet :: String -> Char -> Bool
charSet charset c = or $ map (c==) $ nub charset

-- | *'Not a 'GenLexer'* but useful when passed as the first parameter to 'lexCharP', 'lexWhile' or
-- 'lexUntil'.This function creates a simple set-union of predicates to create a new predicate on
-- 'Prelude.Char's. The predicate evalautes to 'Prelude.True' if the 'Prelude.Char' applied to any
-- of the predicates evaluate to 'Prelude.True'. This is similar to unions of character ranges in
-- POSIX regular expressions: the Regex @[[:xdigit:]xyzXYZ]@ matches the same characters as the
-- predicate:
-- > ('unionCharP' [isHexDigit, charSet "xyzXYZ"])
unionCharP :: [Char -> Bool] -> Char -> Bool
unionCharP px c = or (map ($c) px)

-- | Unions the 'Data.Char.isSymbol' and 'Data.Char.isPunctuation' predicates.
isSymPunct :: Char -> Bool
isSymPunct c = isSymbol c || isPunctuation c

-- | This is a character predicate I find very useful and I believe it should be included in the
-- standard Haskell "Data.Char" module, it is succeeds for alpha-numeric or underscore character.
isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = isAlphaNum c || c=='_'

-- | This is a character predicate I find very useful and I believe it should be included in the
-- standard Haskell "Data.Char" module, it is succeeds for alpha-numeric or underscore character.
isAlpha_ :: Char -> Bool
isAlpha_ c = isAlpha c || c=='_'

-- | Repeat the lexer until it fails. This 'GenLexer' never backtracks, so beware: it may cause
-- infinite loops.
lexMany :: (Eq tok, Enum tok) => GenLexer tok ig -> GenLexer tok ()
lexMany lexer = let loop = mplus (lexer >> loop) (return ()) in loop

-- | Repeat the lexer until it fails, backtrack if it fails the first time.
lexMany1 :: (Eq tok, Enum tok) => GenLexer tok ig -> GenLexer tok ()
lexMany1 lexer = lexer >> lexMany lexer

lexOptional :: (Eq tok, Enum tok) => GenLexer tok () -> GenLexer tok ()
lexOptional lexer = mplus lexer (return ())

-- | Backtracks if there are still characters in the input.
lexEOF :: (Eq tok, Enum tok) => GenLexer tok ()
lexEOF = fmap (=="") (gets lexInput) >>= guard

-- | Create a 'GenLexer' that will continue scanning until it sees an unescaped terminating
-- sequence. You must provide three lexers: the scanning lexer, the escape sequence 'GenLexer' and
-- the terminating sequence 'GenLexer'. Evaluates to 'Prelude.True' if the termChar was found,
-- returns 'Prelude.False' if this tokenizer went to the end of the input without seenig an
-- un-escaped terminating character.
lexUntilTerm :: (Eq tok, Enum tok) => GenLexer tok () -> GenLexer tok () -> GenLexer tok () -> GenLexer tok Bool
lexUntilTerm scanLexer escLexer termLexer = loop where
  skipOne = lexCharP (const True)
  loop = do
    (shouldContinue, wasTerminated) <- msum $
      [ lexEOF    >> return (False, False)
      , termLexer >> return (False, True )
      , escLexer  >>
          mplus (lexEOF                              >> return (False, False))
                (msum [termLexer, escLexer, skipOne] >> return (True , False))
      , scanLexer >> return (True , False)
      , skipOne   >> return (True , False)
      ]
    if shouldContinue then loop else return wasTerminated

-- | A special case of 'lexUntilTerm', lexes until it finds an un-escaped terminating 'Prelude.Char'. You
-- must only provide the escape 'Prelude.Char' and the terminating 'Prelude.Char'.
lexUntilTermChar :: (Eq tok, Enum tok) => Char -> Char -> GenLexer tok Bool
lexUntilTermChar escChar termChar =
  lexUntilTerm (lexUntil (\c -> c==escChar || c==termChar)) (lexChar escChar) (lexChar termChar)

-- | A special case of 'lexUntilTerm', lexes until finds an un-escaped terminating 'Prelude.String'.
-- You must provide only the escpae 'Prelude.String' and the terminating 'Prelude.String'. You can
-- pass a null string for either escape or terminating strings (passing null for both evaluates to
-- an always-backtracking lexer). The most escape and terminating strings are analyzed and the most
-- efficient method of lexing is decided, so this lexer is guaranteed to be as efficient as
-- possible.
lexUntilTermStr :: (Eq tok, Enum tok) => String -> String -> GenLexer tok Bool
lexUntilTermStr escStr termStr = case (escStr, termStr) of
  (""    , ""     ) -> mzero
  (""    , termStr) -> hasOnlyTerm termStr
  (escStr, ""     ) -> hasOnlyTerm escStr
  _                 -> do
    let e = head escStr
        t = head termStr
        predicate = if e==t then (==t) else (\c -> c==e || c==t)
    lexUntilTerm (lexUntil predicate) (lexString escStr) (lexString termStr)
  where
    hasOnlyTerm str = do
      let (scan, term) = case str of
            [a]          -> (lexUntil (==a), lexChar a)
            [a,b] | a/=b -> (lexUntil (==a), lexWhile (==a) >> lexChar b)
            a:ax         -> (lexUntil (==a), let loop = mplus (lexString str) (lexChar a >> loop) in loop)
      lexUntilTerm scan term term

testTokenizer :: (Eq tok, Enum tok, Show tok) => GenTokenizer tok -> String -> IO String
testTokenizer tok str = case runTokenizer tok str of
  Backtrack      -> putStrLn "Backtrack" >> return str
  PFail     err  -> putStrLn (show err)  >> return str
  OK (toks, str) -> mapM print toks >> return str

testLexer :: (Eq tok, Enum tok, Show tok) => GenLexer tok a -> String -> PValue (GenParserErr tok) a
testLexer lexer str = fst $ runLexerState lexer (newLexerState str)

----------------------------------------------------------------------------------------------------
-- Functions that facilitate lexical analysis.

-- | The fundamental lexer: takes a predicate over characters, if one or more characters
-- matches, a token is constructed and it is paired with the remaining string and wrapped into a
-- 'Data.Maybe.Just' value. Otherwise 'Data.Maybe.Nothing' is returned. The 'Data.Maybe.Maybe' type
-- is used so you can combine fundamental tokenizers using 'Control.Monad.mplus'.
lexSimple :: (Eq tok, Enum tok) => tok -> (Char -> Bool) -> GenTokenizer tok
lexSimple tok predicate = lexWhile predicate >> makeToken tok

-- | A fundamental lexer using 'Data.Char.isSpace' and evaluating to a 'Space' token.
lexSpace :: Tokenizer
lexSpace = lexSimple Space isSpace

-- | A fundamental lexer using 'Data.Char.isAlpha' and evaluating to a 'Alphabetic' token.
lexAlpha :: Tokenizer
lexAlpha = lexSimple Alphabetic isAlpha

-- | A fundamental lexer using 'Data.Char.isDigit' and evaluating to a 'Digits' token.
lexDigits :: Tokenizer
lexDigits = lexSimple Digits isDigit

-- | A fundamental lexer using 'Data.Char.isHexDigit' and evaluating to a 'HexDigit' token.
lexHexDigits :: Tokenizer
lexHexDigits = lexSimple HexDigits isHexDigit

-- | Constructs an operator 'Tokenizer' from a string of operators separated by spaces. For example,
-- pass @"+ += - -= * *= ** / /= % %= = == ! !="@ to create 'Lexer' that will properly parse all of
-- those operators. The order of the operators is *NOT* important, repeat symbols are tried only
-- once, the characters @+=@ are guaranteed to be parsed as a single operator @["+="]@ and not as
-- @["+", "="]@.
lexOperator :: String -> Tokenizer
lexOperator ops =
  msum (map (\op -> lexString op >> makeToken Operator) $ reverse $ nub $ sortBy len $ words ops)
  where
    len a b = case compare (length a) (length b) of
      EQ -> compare a b
      GT -> GT
      LT -> LT

lexToEndline :: (Eq tok, Enum tok) => GenLexer tok ()
lexToEndline = lexUntil (=='\n')

lexInlineComment :: String -> String -> Tokenizer
lexInlineComment startStr endStr = do
  lexString startStr
  completed <- lexUntilTermStr "" endStr
  if completed
    then  makeToken ComInln
    else  fail "comment runs past end of input"

lexInlineC_Comment :: Tokenizer
lexInlineC_Comment = lexInlineComment "/*" "*/"

lexEndlineC_Comment :: Tokenizer
lexEndlineC_Comment = lexString "//" >> lexUntil (=='\n') >> makeToken ComEndl

lexInlineHaskellComment :: Tokenizer
lexInlineHaskellComment = lexInlineComment "{-" "-}"

lexEndlineHaskellComment :: Tokenizer
lexEndlineHaskellComment = lexString "--" >> lexToEndline >> makeToken ComEndl

-- | A lot of programming languages provide only end-line comments beginning with a (@#@) character.
lexEndlineCommentHash :: Tokenizer
lexEndlineCommentHash = lexChar '#' >> lexToEndline >> makeToken ComEndl

lexStringLiteral :: Tokenizer
lexStringLiteral = do
  lexChar '"'
  completed <- lexUntilTermChar '\\' '"'
  if completed
    then  makeToken StrLit
    else  fail "string literal expression runs past end of input"

lexCharLiteral :: Tokenizer
lexCharLiteral = lexChar '\'' >> lexUntilTermChar '\\' '\'' >> makeToken CharLit

-- | This actually tokenizes a general label: alpha-numeric and underscore characters starting with
-- an alphabetic or underscore character. This is useful for several programming languages.
-- Evaluates to a 'Keyword' token type, it is up to the 'GenParser's in the syntacticAnalysis phase
-- to sort out which 'Keyword's are actually keywords and which are labels for things like variable
-- names.
lexKeyword :: Tokenizer
lexKeyword = do
  lexWhile (\c -> isAlpha    c || c=='_')
  lexOptional (lexWhile (\c -> isAlphaNum c || c=='_'))
  makeToken Keyword

-- | Create a 'Tokenizer' that lexes the various forms of numbers. Hexadecimal and binary number
-- expressions are also tokenized, the characters @'x'@, @'X'@, @'b'@, and @'B'@ are all prefixes to
-- a string of hexadecimal digits (tokenized with 'Data.Char.isHexDigit'). So the following
-- expression are all parsed as 'Number' tokens:
-- > 0xO123456789ABCDEfabcdef, 0XO123456789ABCDEfabcdef, 0bO123456789ABCDEfabcdef, 0BO123456789ABCDEfabcdef
-- these could all be valid hexadecimal or binary numbers. Of course @0bO123456789ABCDEfabcdef@ is
-- not a valid binary number, but this lexer does not care about the actual value, it is expected
-- that the 'GenParser' report it as an error during the 'syntacticAnalysis' phase. Floating-point
-- decimal numbers are also lexed appropriately, and this includes floating-point numbers expressed
-- in hexadecimal. Again, if your language must disallow hexadecimal floating-point numbers, throw
-- an error in the 'syntacticAnalysis' phase.
lexNumber :: Tokenizer
lexNumber = do
  (getDot, isHex) <- msum $
    [ do  lexChar '0' -- lex a leading zero
          msum $
            [ lexCharP (charSet "xX") >> lexWhile isHexDigit >> return (True , True )
            , lexCharP (charSet "Bb") >> lexWhile isDigit    >> return (True , False)
            , return (True , False) -- a zero not followed by an 'x' or 'b' is also valid
            ]
    , lexChar '.' >> lexWhile isDigit >> return (False, False) -- lex a leading decimal point
    , lexWhile isDigit                >> return (True , False) -- lex an ordinary number
    ]
  lexOptional $ do
    if getDot
      then  do
        lexChar '.'
        mplus (lexWhile (if isHex then isHexDigit else isDigit))
              (fail "no digits after decimal point")
      else  return ()
    if isHex
      then  return ()
      else  lexOptional $ do
              lexCharP (charSet "Ee")
              lexOptional (lexCharP (charSet "-+"))
              mplus (lexWhile isDigit)
                    (fail "no digits after exponent mark in decimal-point number")
  makeToken Number

-- | Creates a 'Label' token for haskell data type names, type names, class names, or constructors,
-- i.e. one or more labels (alpha-numeric and underscore characters) separated by dots (with no
-- spaces between the dots) where every first label is a capital alphabetic character, and the final
-- label may start with a lower-case letter or underscore, or the final label may also be
-- punctuation surrounded by parens. Examples are: @Aaa.Bbb@, @D.Ee_ee.ccc123@, @Aaa.Bbb.Ccc.___d1@,
-- @A.B.C.(-->)@.
lexHaskellLabel :: Tokenizer
lexHaskellLabel = loop 0 where
  label    = do
    lexCharP (\c -> isUpper c && isAlpha c)
    lexOptional (lexWhile isAlphaNum_)
  loop   i = mplus (label >> mplus (tryDot i) done) (if i>0 then final else mzero)
  tryDot i = lexChar '.' >> loop (i+1)
  final    = mplus (lexCharP isAlpha_ >> lexOptional (lexWhile isAlphaNum_)) oper >> done
  oper     = do lexChar '('
                mplus (lexWhile (\c -> c/=')' && isSymPunct c) >> lexChar ')')
                      (fail "bad operator token after final dot of label")
  done     = makeToken Label

-- | Creates a list of 'Tokenizer's for a C-like programming language. Since the operators are the
-- biggest lexical difference between languages, you must only provide a string of space-separated
-- operator symbols, for example:
-- > "= == ! != < <= > >= + ++ += - -- -> -= * *= / /= % %= ^ ^= << <<= >> >>= . & ~ ? : ;"
-- This string is passed directly to 'lexOperator' to construct the final 'Tokenizer' in the list of
-- 'Tokenizers' to which the function evaluates. This is a very simple tokenizer, so all tokens
-- starting with an underscore or alphabetic character are tokenized as 'Dao.Parser.Keyword's. Your
-- 'Dao.Parser.GenParser' can then use the 'Dao.Parser.keyword' function to sort out which
-- 'Dao.Parser.Keyword' tokens are actually keywords and which are labels.
-- 
-- This simplified tokenizing strategy is applied to punctuation as well. It is best to treat all
-- punctuation marks as 'Dao.Parser.Operator's, and leave it to the 'GenParser's in the
-- syntacticAnalysis phase to sort out which operators are actually operators. Therefore, if you
-- leave out (for example) the semicolon because it is not technically an operator, your C-like
-- language will fail to tokenize when it comes to a semicolon. If your parser must have semicolons
-- treated differently from operators, you can omit the semicolon from the string parameter passed
-- to this function and then append your own semicolon 'Dao.Parser.Tokenizer' to the list produced
-- by this function.
tokenizeC_Like :: String -> [Tokenizer]
tokenizeC_Like operators =
  [ lexStringLiteral
  , lexCharLiteral
  , lexInlineC_Comment
  , lexEndlineC_Comment
  , lexSpace
  , lexKeyword
  , lexNumber
  , lexOperator operators
  , lexCharP (charSet "([{") >> makeToken Opener
  , lexCharP (charSet "}])") >> makeToken Closer
  ]

-- | If you like 'Tokenizer's provided in this module, like 'lexOperator' or 'lexDigits', but you
-- need a 'GenTokenizer' version of them, you can provide a conversion function to convert from a
-- 'TT' token type to your own token type:
-- > data MyToken = MyAlpha | MyDigits deriving ('Prelude.Eq', 'Prelude.Enum')
-- > myLexAlpha  = 'lexConvertTT' ('Prelude.const' MyAlpha)  'lexAlpha'
-- > myLexDigits = 'lexConvertTT' ('Prelude.const' MyDigits) 'lexDigits'
lexConvertTT :: (Eq tok, Enum tok) => (TT -> tok) -> Tokenizer -> GenTokenizer tok
lexConvertTT conv tokenizer = do
  st <- get
  let (result, st) = runLexerState tokenizer st
  result <- assumePValue $ case result of
    OK   toks -> return (map (fmapTokens conv) toks)
    Backtrack -> Backtrack
    PFail err -> PFail (fmapTokens conv err)
  put st >> return result

-- | The 'GenLexer's analogue of 'Control.Monad.State.runState', runs the lexer using an existing
-- 'LexerState'.
runLexerState :: (Eq tok, Enum tok) => GenLexer tok a -> LexerState -> (PValue (GenParserErr tok) a, LexerState)
runLexerState (GenLexer lexer) = runState (runPTrans lexer)

-- | Runs a 'GenLexer' (specifically a 'GenTokenizer' that returns a 'Prelude.String') with an input
-- string and evaluates it using an input string, returning the 'Dao.Prelude.PValue' result of the
-- 'GenTokenizer' evaluation with portion of the input string that was not lexed. FYI: this function
-- also enforces the rule that 'GenToken's must never be null strings, all tokenized strings are
-- checked. If a 'GenToken' containing a 'Dao.String.nil' is returned, this function evaluates to
-- 'Control.Monad.mzero'.
runTokenizer :: (Eq tok, Enum tok) => GenTokenizer tok -> String -> (PValue (GenParserErr tok) ([GenToken tok], String))
runTokenizer lexer input = fst $ flip runLexerState (newLexerState input) $ do
  toks <- fmap (filter (not . (==nil) . tokToUStr)) lexer
  guard (not (null toks))
  gets lexInput >>= \remainder -> return (toks, remainder)

-- | This function performs the lexical analysis pass over an entire input text. It iterates over an
-- input string. Each iteration takes a list of 'Tokenizer's and tries each one in order. If one of
-- the 'Tokenizers' succeeds, the characters tokenized are broken off of the input string and stored
-- with their line and column information, and the next iteration begins with the remaining input
-- string. If all of the given 'Tokenizer's fail, this function evaluates to 'Dao.Predicate.PFail'
-- and returns the location of the failure. This iteration continues until the entire input string
-- is consumed. In this way, the lexer breaks an arbitrary input string into lines, and breaks lines
-- into 'Token's. 'Token's like string literals and comments might take up several lines, so the
-- 'Token' data type is defined to be able to still work even though the input is broken into lines.
-- The first parameter is an integer value (actually a 'Data.Word.Word') for how many columns a tab
-- takes to give more accurate column counts.
--
-- Every 'GenTokenizer' passed to this function is treated as an atomic lexer: it either succeeds or
-- backtracks. If it succeeds, the token produced is used, if it backtracks, the 'lexInput' is
-- restored to exactly the state it was before evaluating the 'GenTokenizer' as if nothing happened,
-- The characters in 'lexInput' or 'lexBuffer' are ignored after backtracking.
-- 
-- Line positions of tokens assumes that *every token* generated by the @['GenTokenizer' tok]@ list
-- of tokenizers will return a token containing exactly the characters lexed-off of the input
-- string, nothing more and nothing less. It is therefore important that space characters also be
-- treated as tokens, and that no characters are thrown-away during lexing. Lexers can return more
-- than one token, so if necessary, return an 'Ignored' token along with a token containing the
-- important characters.
lexicalAnalysis :: (Eq tok, Enum tok) => [GenTokenizer tok] -> Word -> String -> PValue (GenParserErr tok) [GenLine tok]
lexicalAnalysis tokenizers tablen input = fmap (cluster 1 []) (lexLoop 1 1 [] input) where
  lexLoop lineNum colNum toks str = case nextLex str of
    OK (newToks, str) ->
      let (newLineNum, newColNum, positionedToks) = positionTok [] lineNum colNum newToks
          toks' = toks ++ positionedToks
      in  if null newToks
            then  PFail $ 
                    (parserErr lineNum colNum){
                      parserErrMsg = Just $ ustr $ "tokenizer evaluated to null list of tokens"
                    }
            else  if null str
                    then  OK toks'
                    else  lexLoop newLineNum newColNum toks' str
    PFail err -> PFail err
    Backtrack -> PFail $
      (parserErr lineNum colNum)
      { parserErrMsg = Just $ ustr $
          "bad symbol"++(if null str then "<END>" else ' ':show (head str))++" in character stream"
      }
  nextLex str = msum $ map (flip runTokenizer str) $ tokenizers
  cluster curLineNum toks tx = case tx of
    []                          ->
      if null toks then [] else [GenLine{lineLineNumber=curLineNum, lineTokens=toks}]
    (lineNum, colNum, tok) : tx ->
      if lineNum==curLineNum || null toks -- if we are on the same line, or if the line is empty
        then  cluster lineNum (toks++[(colNum, tok)]) tx -- then get the next token
        else  GenLine{lineLineNumber=curLineNum, lineTokens=toks} : cluster lineNum [(colNum, tok)] tx
  countNLs lns cols input = case break (=='\n') input of
    (""    , ""        ) -> (lns, cols)
    (_     , '\n':after) -> countNLs (lns+1) 0 after
    (before, after     ) -> (lns, cols + foldl (+) 0 (map charPrintWidth (before++after)))
  charPrintWidth c = case c of
    c | c=='\t'   -> tablen
    c | isPrint c -> 1
    c             -> 0
  positionTok rx lineNum colNum toks = case toks of
    []       -> (lineNum, colNum, rx)
    tok:toks -> 
      let (lns, cols) = countNLs 0 0 $ uchars $ tokToUStr tok
          newLineNum  = lns + lineNum
          newColNum   = cols + (if lns>0 then 0 else colNum)
      in  positionTok (rx++[(lineNum, colNum, tok)]) newLineNum newColNum toks

testLexicalAnalysis_withFilePath :: (Eq tok, Enum tok, Show tok) => [GenTokenizer tok] -> Word -> FilePath -> String -> IO ()
testLexicalAnalysis_withFilePath tokenizers tablen filepath input = putStrLn result where
  result = case lexicalAnalysis tokenizers tablen input of
    OK  lines -> concatMap showLine lines
    Backtrack -> reportFilepath++": lexical analysis evalauted to \"Backtrack\""
    PFail err -> reportFilepath++show err
  showLine line = concat $
    [ "\n----------\nline "
    , show (lineNumber line), "\n"
    , intercalate ", " $ map showTok (lineTokens line)
    ]
  showTok (col, tok) = concat [show col, " ", show (tokType tok), " ", show (tokToUStr tok)]
  reportFilepath = if null filepath then "" else filepath++":"

-- | Run the 'lexicalAnalysis' with the 'GenTokenizers' on the given 'Prelude.String' and print out
-- every token created.
testLexicalAnalysis :: (Eq tok, Enum tok, Show tok) => [GenTokenizer tok] -> Word -> String -> IO ()
testLexicalAnalysis a b c = testLexicalAnalysis_withFilePath a b "" c

-- | Run the 'lexicalAnalysis' with the 'GenTokenizers' on the contents of the file at the the given
-- 'System.IO.FilePath' 'Prelude.String' and print out every token created.
testLexicalAnalysisOnFile :: (Eq tok, Enum tok, Show tok) => [GenTokenizer tok] -> Word -> FilePath -> IO ()
testLexicalAnalysisOnFile a b c = readFile c >>= testLexicalAnalysis_withFilePath a b c

----------------------------------------------------------------------------------------------------
-- The parser data type

-- | This data structure is used by both the 'GenLexer' and 'GenParser' monads.
data GenParserErr tok
  = GenParserErr
    { parserErrLoc :: L.Location
    , parserErrMsg :: Maybe UStr
    , parserErrTok :: Maybe (GenToken tok)
    }

type ParserErr = GenParserErr TT

instance Show tok => Show (GenParserErr tok) where
  show err = concat $
    [ show (parserErrLoc err)
    , fromMaybe "" (fmap ((": on token "++) . show) (parserErrTok err))
    , fromMaybe "" (fmap ((": "++) . uchars) (parserErrMsg err))
    ]

instance (Eq tok, Enum tok) => CanMapTokens tok GenParserErr where
  fmapTokens conv err =
    err{ parserErrTok = fmap (fmapTokens conv) (parserErrTok err) }

-- | An initial blank parser error.
parserErr :: (Eq tok, Enum tok) => Word -> Word -> GenParserErr tok
parserErr lineNum colNum =
  GenParserErr
  { parserErrLoc =
      L.LineColumn
      { L.startingLine   = fromIntegral lineNum
      , L.startingColumn = colNum
      , L.endingLine     = fromIntegral lineNum
      , L.endingColumn   = colNum
      }
  , parserErrMsg   = Nothing
  , parserErrTok   = Nothing
  }

-- | The 'GenParserState' contains a stream of all tokens created by the 'lexicalAnalysis' phase.
-- This is the state associated with a 'GenParser' in the instantiation of
-- 'Control.Monad.State.MonadState', so 'Control.Monad.State.get' returns a value of this data type.
newtype GenParserState tok = GenParserState { getLines :: [GenLine tok] }
type ParserState = GenParserState TT

instance (Eq tok, Enum tok) => CanMapTokens tok GenParserState where
  fmapTokens conv (GenParserState lines) = GenParserState (map (fmapTokens conv) lines)

newParserState :: (Eq tok, Enum tok) => [GenLine tok] -> GenParserState tok
newParserState = GenParserState

-- | The task of the 'GenParser' monad is to look at every token in order and construct syntax trees
-- in the 'syntacticAnalysis' phase.
--
-- This function instantiates all the useful monad transformers, including 'Data.Functor.Functor',
-- 'Control.Monad.Monad', 'Control.MonadPlus', 'Control.Monad.State.MonadState',
-- 'Control.Monad.Error.MonadError' and 'Dao.Predicate.ErrorMonadPlus'. Backtracking can be done
-- with 'Control.Monad.mzero' and "caught" with 'Control.Monad.mplus'. 'Control.Monad.fail' and
-- 'Control.Monad.Error.throwError' evaluate to a control value containing a 'GenParserErr' value
-- which can be caught by 'Control.Monad.Error.catchError', and which automatically contain
-- information about the location of the failure and the current token in the stream that caused the
-- failure.
newtype GenParser tok a = GenParser { parserToPTrans :: PTrans (GenParserErr tok) (State (GenParserState tok)) a}
type Parser a = GenParser TT a
instance (Eq tok, Enum tok) => Functor   (GenParser tok) where { fmap f (GenParser a) = GenParser (fmap f a) }
instance (Eq tok, Enum tok) => Monad     (GenParser tok) where
  (GenParser ma) >>= mfa = GenParser (ma >>= parserToPTrans . mfa)
  return a               = GenParser (return a)
  fail msg = do
    (a, b) <- getCursor
    throwError ((parserErr a b){parserErrMsg = Just (ustr msg)})
instance (Eq tok, Enum tok) => MonadPlus (GenParser tok) where
  mzero                             = GenParser mzero
  mplus (GenParser a) (GenParser b) = GenParser (mplus a b)
instance (Eq tok, Enum tok) => MonadState (GenParserState tok) (GenParser tok) where
  get = GenParser (PTrans (fmap OK get))
  put = GenParser . PTrans . fmap OK . put
instance (Eq tok, Enum tok) => MonadError (GenParserErr tok) (GenParser tok) where
  throwError err                        = assumePValue (PFail err)
  catchError (GenParser ptrans) catcher = GenParser $ do
    pval <- catchPValue ptrans
    case pval of
      OK      a -> return a
      Backtrack -> mzero
      PFail err -> parserToPTrans (catcher err)
instance (Eq tok, Enum tok) => ErrorMonadPlus (GenParserErr tok) (GenParser tok) where
  catchPValue (GenParser ptrans) = GenParser (catchPValue ptrans)
  assumePValue                   = GenParser . assumePValue

-- | Return the next token in the state. If the boolean parameter is true, the current token will
-- also be removed from the state.
nextToken :: (Eq tok, Enum tok) => Bool -> GenParser tok (Word, Word, GenToken tok)
nextToken doRemove = get >>= \lines -> case getLines lines of
  []         -> mzero
  line:lines -> case lineTokens line of
    []                 -> put (GenParserState{getLines=lines}) >> nextToken doRemove
    (colNum, tok):toks -> do
      if doRemove then put (GenParserState{getLines = line{lineTokens=toks}:lines}) else return ()
      return (lineNumber line, colNum, tok)

-- | Return the next token in the state if it is of the type specified, removing it from the state.
token :: (Eq tok, Enum tok) => (tok -> Bool) -> GenParser tok UStr
token requestedType = do
  (_, _, tok) <- nextToken False
  if requestedType (tokType tok) then nextToken True >> return (tokToUStr tok) else mzero

-- | Return the next token in the state if it is of the type specified and also if the string value
-- evaluated by the given predicate returns true, otherwise backtrack.
tokenP :: (Eq tok, Enum tok) => (tok -> Bool) -> (String -> Bool) -> GenParser tok UStr
tokenP requestedType predicate = token requestedType >>= \tokenString ->
  if predicate (uchars tokenString) then return tokenString else mzero

tokenType :: (Eq tok, Enum tok) => tok -> GenParser tok UStr
tokenType requestedType = token (==requestedType)

-- | Return the next token in the state if the string value of the token is exactly equal to the
-- given string, and if the token type is any one of the given token types.
tokenTypes :: (Eq tok, Enum tok) => [tok] -> String -> GenParser tok UStr
tokenTypes requestedType compareString = tokenP (\b -> or (map (==b) requestedType)) (==compareString)

-- | Succeeds if the next 'Token' is a 'Alphabetic' 'TT' containing the exact string provided.
-- This function is defined simply as @'tokenStr' ['Alnum', 'Alphabetic']@
keyword :: String -> Parser UStr
keyword = tokenTypes [Keyword]

-- | Succeeds if the next 'Token' is a 'Symbol' 'TT' containing the exact string provided.
-- This function is defined simply as @'tokenStr' 'Symbol'@
operator :: String -> Parser UStr
operator = tokenTypes [Operator]

-- | Push an arbitrary token into the state, but you really don't want to use this function. It is
-- used to implement backtracking by the 'withToken' function, so use 'withToken' instead.
pushToken :: (Eq tok, Enum tok) => (Word, Word, GenToken tok) -> GenParser tok ()
pushToken (lineNum, colNum, tok) = modify $ \st -> case getLines st of
  []         -> st{getLines = [GenLine{lineLineNumber=lineNum, lineTokens=[(colNum,tok)]}]}
  line:lines ->
    if lineLineNumber line == lineNum
      then  st{getLines = line{lineTokens = (colNum, tok) : lineTokens line} : lines}
      else  st{getLines = GenLine{lineLineNumber=lineNum, lineTokens=[(colNum,tok)]} : line : lines}

-- | Single token look-ahead: takes the next token, removing it from the state, and uses it to
-- evaluate the given 'Parser'. If the backtracks, the token is replaced into the state. Failures are
-- not caught, make sure the 'Parser' you pass to this function makes use of
-- 'Control.Monad.Error.catchError' to catch failures if that is what you need it to do. This
-- function does not keep a copy of the state, it removes a token from the stream, then places it
-- back if backtracking occurs. This is supposed to be more efficient.
withToken :: (Eq tok, Enum tok) => ((Word, Word, GenToken tok) -> GenParser tok a) -> GenParser tok a
withToken parser = do
  token <- nextToken True
  mplus (parser token) (pushToken token >> mzero)

withTokenType :: (Eq tok, Enum tok) => tok -> (UStr -> GenParser tok a) -> GenParser tok a
withTokenType requestedType parser = withToken $ \ (_, _, tok) ->
  if tokType tok == requestedType then parser (tokToUStr tok) else mzero

-- | Return the current line and column of the current token without modifying the state in any way.
getCursor :: (Eq tok, Enum tok) => GenParser tok (Word, Word)
getCursor = nextToken False >>= \ (a,b, _) -> return (a,b)

-- | Evaluates to @()@ if we are at the end of the input text, otherwise backtracks.
getEOF :: (Eq tok, Enum tok) => GenParser tok ()
getEOF = get >>= \st -> case getLines st of
  []   -> return ()
  [st] -> if null (lineTokens st) then return () else mzero
  _    -> mzero

-- | Given two parameters: 1. an error message and 2. a 'Parser', will succeed normally if
-- evaluating the given 'Parser' succeeds. But if the given 'Parser' backtracks, this this function
-- will evaluate to a 'Parser' failure with the given error message. If the given 'Parser' fails,
-- it's error message is used instead of the error message given to this function. The string
-- "expecting " is automatically prepended to the given error message so it is a good idea for your
-- error message to simple state what you were expecting, like "a string" or "an integer". I
-- typically write 'expect' statements like so:
-- > fracWithExp = do
-- >     fractionalPart <- parseFractional
-- >     'tokenP' 'Alphabetic' (\tok -> tok=="E" || tok=="e")
-- >     'expect' "an integer expression after the 'e'" $ do
-- >         exponentPart <- parseSignedInteger
-- >         return (makeFracWithExp fractionalPart exponentPart :: 'Prelude.Double')
expect :: (Eq tok, Enum tok) => String -> GenParser tok a -> GenParser tok a
expect errMsg parser = do
  (a, b) <- getCursor
  let expectMsg = "expecting "++errMsg
  mplus parser (throwError ((parserErr a b){parserErrMsg = Just (ustr expectMsg)}))

-- | If the given 'Parser' backtracks then evaluate to @return ()@, otherwise ignore the result of the
-- 'Parser' and evaluate to @return ()@.
ignore :: (Eq tok, Enum tok) => GenParser tok ig -> GenParser tok ()
ignore = flip mplus (return ()) . void

-- | Shorthand for @'ignore' ('token' 'Space')@
skipSpaces :: Parser ()
skipSpaces = ignore (token (==Space))

-- | A 'marker' immediately stores the cursor onto the stack. It then evaluates the given 'Parser'.
-- If the given 'Parser' fails, the position of the failure (stored in a 'Dao.Token.Location') is
-- updated such that the starting point of the failure points to the cursor stored on the stack by
-- this 'marker'. When used correctly, this function makes error reporting a bit more helpful.
marker :: (Eq tok, Enum tok) => GenParser tok a -> GenParser tok a
marker parser = do
  (a, b) <- getCursor
  flip mapPFail parser $ \parsErr ->
    parsErr
    { parserErrLoc =
        (parserErrLoc parsErr)
        { L.startingLine   = fromIntegral a
        , L.startingColumn = b
        }
    }

-- | If you like 'Parser's provided in this module, like 'keyword' or 'operator', but you
-- need a 'GenParser' version of them, you can provide a conversion function to convert from a
-- 'TT' token type to your own token type:
-- > data MyToken = MyKeyword | MyOperator deriving ('Prelude.Eq', 'Prelude.Enum')
-- > myTokenToTT myTok = if myTok==MyKeyword then Keyword else Operator
-- > myKeyword  = 'parseConvertTT' ('Prelude.const' MyKeyword)  myTokenToTT 'keyword'
-- > myOperator = 'parseConvertTT' ('Prelude.const' MyOperator) myTokenToTT 'operator'
parseConvertTT :: (Eq tok, Enum tok) => (TT -> tok) -> (tok -> TT) -> Parser a -> GenParser tok a
parseConvertTT conv unconv parser = do
  st <- get
  let (result, st') = runParserState parser (fmapTokens unconv st)
  result <- assumePValue $ case result of
    OK      a -> OK      a
    Backtrack -> Backtrack
    PFail err -> PFail (fmapTokens conv err)
  put (fmapTokens conv st') >> return result

-- | The 'GenParser's analogue of 'Control.Monad.State.runState', runs the parser using an existing
-- 'GenParserState'.
runParserState :: (Eq tok, Enum tok) => GenParser tok a -> GenParserState tok -> (PValue (GenParserErr tok) a, GenParserState tok)
runParserState (GenParser parser) = runState (runPTrans parser)

-- | This is the second phase of parsing, takes a stream of tokens created by the 'lexicalAnalysis'
-- phase (the @['GenLine' tok]@ parameter) and constructs a syntax tree data structure using the
-- 'GenParser' monad provided.
syntacticAnalysis :: (Eq tok, Enum tok) => GenParser tok synTree -> [GenLine tok] -> PValue (GenParserErr tok) synTree
syntacticAnalysis parser = fst . runParserState parser . newParserState

----------------------------------------------------------------------------------------------------

-- | A *General Context-Free Grammar* is a data structure that allows you to easily define a
-- two-phase parser (a parser with a 'lexicalAnalysis' phase, and a 'syntacticAnalysis' phase). The
-- fields supplied to this data type are define the grammar, and the 'parse' function can be used to
-- parse an input string using the context-free grammar defined in this data structure. *Note* that
-- the parser might have two phases, but because Haskell is a lazy language and 'parse' is a pure
-- function, both phases happen at the same time, so the resulting parser does not need to parse the
-- entire input in the first phase before beginning the second phase.
data GenCFGrammar tok synTree
  = GenCFGrammar
    { columnWidthOfTab :: Word
      -- ^ specify how many columns a @'\t'@ character takes up. This number is important to get
      -- accurate line:column information in error messages.
    , tokenizers       :: [GenTokenizer tok]
      -- ^ *the order of these tokenizers is important,* these are the tokenizers passed to the
      -- 'lexicalAnalysis' phase to generate the stream of tokens for the 'syntacticAnalysis' phase.
    , mainParser       :: GenParser tok synTree
      -- ^ this is the parser entry-point which is used to evaluate the 'syntacticAnalysis' phase.
    }

type CFGrammar synTree = GenCFGrammar TT synTree

-- | This is *the function that parses* an input string according to a given 'GenCFGrammar'.
parse :: (Eq tok, Enum tok) => GenCFGrammar tok synTree -> String -> PValue (GenParserErr tok) synTree
parse cfg input =
  lexicalAnalysis (tokenizers cfg) (columnWidthOfTab cfg) input >>= syntacticAnalysis (mainParser cfg)

