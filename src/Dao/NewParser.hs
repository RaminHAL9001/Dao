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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dao.NewParser where

import           Dao.String
import           Dao.Predicate

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error

import           Data.Monoid
import           Data.Typeable
import           Data.Maybe
import           Data.Word
import           Data.Char  hiding (Space)
import           Data.List
import           Data.Array.IArray
import qualified Data.Map    as M
import qualified Data.IntMap as IM

import           System.IO

import Debug.Trace

type LineNum   = Word
type ColumnNum = Word
type TabWidth  = Word

----------------------------------------------------------------------------------------------------

-- | Used mostly by 'Dao.Parser' and 'Dao.Object.Parser' but is also used to report location of
-- errors and are stored in the abstract syntax tree, 'ObjectExpr', 'ScriptExpr'.
data Location
  = LocationUnknown
  | Location -- ^ the 'Location' but without the starting/ending character count
    { startingLine   :: LineNum
    , startingColumn :: ColumnNum
    , endingLine     :: LineNum
    , endingColumn   :: ColumnNum
    }
  deriving (Eq, Typeable)

atPoint :: LineNum -> ColumnNum -> Location
atPoint a b =
  Location
  { startingLine   = a
  , endingLine     = a
  , startingColumn = b
  , endingColumn   = b
  }

instance Ord Location where
  compare a b = case (a,b) of
    (LocationUnknown, LocationUnknown) -> EQ
    (_              , LocationUnknown) -> LT
    (LocationUnknown, _              ) -> GT
    (a              , b              ) ->
      compare (abs(ela-sla), abs(eca-sca), sla, sca) (abs(elb-slb), abs(ecb-scb), slb, scb)
    where
      sla = startingLine   a
      ela = endingLine     a
      slb = startingLine   b
      elb = endingLine     b
      sca = startingColumn a
      eca = endingColumn   a
      scb = startingColumn b
      ecb = endingColumn   b
  -- ^ Greater-than is determined by a heuristic value of how large and uncertain the position of
  -- the error is. If the exact location is known, it has the lowest uncertainty and is therefore
  -- less than a location that might occur across two lines. The 'LocationUnknown' value is the most
  -- uncertain and is greater than everything except itself. Using this comparison function, you can
  -- sort lists of locations from least to greatest and hopefully get the most helpful, most
  -- specific location at the top of the list.

-- | The the coordinates from a 'Location':
-- @(('startingLine', 'startingColumn'), ('endingLine', 'endingColumn'))@
locationCoords :: Location -> Maybe ((LineNum, ColumnNum), (LineNum, ColumnNum))
locationCoords loc = case loc of
  LocationUnknown -> Nothing
  _ -> Just ((startingLine loc, startingColumn loc), (endingLine loc, endingColumn loc))

class HasLocation a where
  getLocation :: a -> Location
  setLocation :: a -> Location -> a

instance Show Location where
  show t = case t of
    LocationUnknown -> ""
    _ -> show (startingLine t) ++ ':' : show (startingColumn t)

instance Monoid Location where
  mempty =
    Location
    { startingLine   = 0
    , startingColumn = 0
    , endingLine     = 0
    , endingColumn   = 0
    }
  mappend loc a = case loc of
    LocationUnknown -> a
    _ -> case a of
      LocationUnknown -> loc
      _ ->
        loc
        { startingLine   = min (startingLine   loc) (startingLine   a)
        , startingColumn = min (startingColumn loc) (startingColumn a)
        , endingLine     = max (endingLine     loc) (endingLine     a)
        , endingColumn   = max (endingColumn   loc) (endingColumn   a)
        }

data GenToken tok
  = GenEmptyToken { tokType :: tok }
  | GenCharToken  { tokType :: tok, tokChar :: !Char }
  | GenToken      { tokType :: tok, tokUStr :: UStr }
instance Show tok => Show (GenToken tok) where
  show tok = show (tokType tok) ++ " " ++ show (tokToUStr tok)

class HasLineNumber   a where { lineNumber   :: a -> LineNum }
class HasColumnNumber a where { columnNumber :: a -> ColumnNum }

tokToUStr :: GenToken tok -> UStr
tokToUStr tok = case tok of
  GenEmptyToken _   -> nil
  GenCharToken  _ c -> ustr [c]
  GenToken      _ u -> u

tokToStr :: GenToken tok -> String
tokToStr tok = case tok of
  GenEmptyToken _   -> ""
  GenCharToken  _ c -> [c]
  GenToken      _ u -> uchars u

data GenTokenAt tok =
  GenTokenAt
  { tokenAtLineNumber   :: LineNum
  , tokenAtColumnNumber :: ColumnNum
  , getToken            :: GenToken tok
  }
instance HasLineNumber   (GenTokenAt tok) where { lineNumber   = tokenAtLineNumber   }
instance HasColumnNumber (GenTokenAt tok) where { columnNumber = tokenAtColumnNumber }

data GenLine tok
  = GenLine
    { lineLineNumber :: LineNum
    , lineTokens     :: [(ColumnNum, GenToken tok)]
      -- ^ a list of tokens, each with an associated column number.
    }

instance HasLineNumber (GenLine tok) where { lineNumber = lineLineNumber }

instance Show tok => Show (GenLine tok) where
  show line = show (lineLineNumber line) ++ ": " ++ show (lineTokens line)

----------------------------------------------------------------------------------------------------
-- The parser error data type.

-- | This data structure is used by both the 'GenLexer' and 'TokStream' monads.
data GenError st tok
  = GenError
    { parseErrLoc     :: Maybe Location
    , parseErrMsg     :: Maybe UStr
    , parseErrTok     :: Maybe (GenToken tok)
    , parseStateAtErr :: Maybe st
    }

instance Show tok =>
  Show (GenError st tok) where
    show err =
      let msg = concat $ map (fromMaybe "") $
            [ fmap (("(on token "++) . (++")") . show) (parseErrTok err)
            , fmap ((": "++) . uchars) (parseErrMsg err)
            ]
      in  if null msg then "Unknown parser error" else msg

-- | An initial blank parser error.
parserErr :: (Eq tok, Enum tok) => LineNum -> ColumnNum -> GenError st tok
parserErr lineNum colNum =
  GenError
  { parseErrLoc = Just $
      Location
      { startingLine   = lineNum
      , startingColumn = colNum
      , endingLine     = lineNum
      , endingColumn   = colNum
      }
  , parseErrMsg = Nothing
  , parseErrTok = Nothing
  , parseStateAtErr = Nothing
  }

----------------------------------------------------------------------------------------------------

-- | This is the state used by every 'GenLexer'.
data GenLexerState tok
  = GenLexerState
    { lexTabWidth      :: TabWidth
    , lexCurrentLine   :: LineNum
    , lexCurrentColumn :: ColumnNum
    , lexTokenCounter  :: Word
      -- ^ some algorithms would like to know if you lexed any tokens at all, and will fail if you
      -- did not. There needs to be some way of knowing how many tokens your 'GenLexer' created.
    , tokenStream      :: [GenTokenAt tok]
    , lexBuffer        :: String
      -- ^ stores the characters consumed by 'GenLexer's. This buffer is never cleared until
      -- 'makeToken' is evaluated. Retrieve this string using:
      -- > 'Control.Monad.State.gets' 'lexBuffer'
    , lexInput         :: String
      -- ^ contains the remainder of the input string to be analyzed. Retrieve this string using:
      -- > 'Control.Monad.State.gets' 'lexInput'
    }

-- | Create a new lexer state using the given input 'Prelude.String'. This is only realy useful if
-- you must evaluate 'runLexerState'.
newLexerState :: (Eq tok, Enum tok) => String -> GenLexerState tok
newLexerState input =
  GenLexerState
  { lexTabWidth      = 4
  , lexTokenCounter  = 0
  , lexCurrentLine   = 1
  , lexCurrentColumn = 1
  , tokenStream      = []
  , lexBuffer        = ""
  , lexInput         = input
  }

-- | 'parse' will evaluate the 'GenLexer' over the input string first. If the 'GenLexer' fails, it
-- will evaluate to a 'Dao.Prelude.PFail' value containing a 'GenError' value of type:
-- > ('Prelude.Eq' tok, 'Prelude.Enum' tok) => 'GenError' ('GenLexerState' tok)
-- However the 'TokStream's evaluate to 'GenError's containing type:
-- > ('Prelude.Eq' tok, 'Prelude.Enum' tok) => 'GenError' ('TokStreamState' st tok)
-- This function provides an easy way to convert between the two 'GenError' types, however since
-- the state value @st@ is polymorphic, you will need to insert your parser state into the error
-- value after evaluating this function. For example:
-- > case tokenizerResult of
-- >    'Dao.Predicate.PFail' lexErr -> 'Dao.Predicate.PFail' (('lexErrToParseErr' lexErr){'parseStateAtErr' = Nothing})
-- >    ....
lexErrToParseErr
  :: (Eq tok, Enum tok)
  => GenError (GenLexerState tok) tok
  -> GenError (TokStreamState st tok) tok
lexErrToParseErr lexErr =
  lexErr
  { parseStateAtErr = Nothing
  , parseErrLoc = st >>= \st -> return (atPoint (lexCurrentLine st) (lexCurrentColumn st))
  }
  where { st = parseStateAtErr lexErr }


-- | The 'GenLexer' is very similar in many ways to regular expressions, however 'GenLexer's always
-- begin evaluating at the beginning of the input string. The 'lexicalAnalysis' phase of parsing
-- must generate 'GenToken's from the input string. 'GenLexer's provide you the means to do with
-- primitive functions like 'lexString', 'lexChar', and 'lexUntil', and combinators like 'defaultTo'
-- and 'lexUntilTermChar'. These primitive functions collect characters into a buffer, and you can
-- then empty the buffer and use the buffered characters to create a 'GenToken' using the
-- 'makeToken' function.
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
newtype GenLexer tok a = GenLexer{
    runLexer :: PTrans (GenError (GenLexerState tok) tok) (State (GenLexerState tok)) a
  }
instance (Eq tok, Enum tok) =>
  Functor (GenLexer tok) where { fmap fn (GenLexer lex) = GenLexer (fmap fn lex) }
instance (Eq tok, Enum tok) =>
  Monad (GenLexer tok) where
    (GenLexer fn) >>= mfn          = GenLexer (fn >>= runLexer . mfn)
    return                         = GenLexer . return
    fail msg                       = do
      st <- get
      throwError $
        (parserErr (lexCurrentLine st) (lexCurrentColumn st)){parseErrMsg = Just (ustr msg)}
instance (Eq tok, Enum tok) =>
  MonadPlus (GenLexer tok) where
    mplus (GenLexer a) (GenLexer b) = GenLexer (mplus a b)
    mzero                           = GenLexer mzero
instance (Eq tok, Enum tok) =>
  Applicative (GenLexer tok) where { pure = return; (<*>) = ap; }
instance (Eq tok, Enum tok) =>
  Alternative (GenLexer tok) where { empty = mzero; (<|>) = mplus; }
instance (Eq tok, Enum tok) =>
  MonadState (GenLexerState tok) (GenLexer tok) where
    get = GenLexer (lift get)
    put = GenLexer . lift . put
instance (Eq tok, Enum tok) =>
  MonadError (GenError (GenLexerState tok) tok) (GenLexer tok) where
    throwError                        = GenLexer . throwError
    catchError (GenLexer try) catcher = GenLexer (catchError try (runLexer . catcher))
instance (Eq tok, Enum tok) =>
  MonadPlusError (GenError (GenLexerState tok) tok) (GenLexer tok) where
    catchPValue (GenLexer try) = GenLexer (catchPValue try)
    assumePValue               = GenLexer . assumePValue
instance (Eq tok, Enum tok, Monoid a) =>
  Monoid (GenLexer tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }

-- | Append the first string parameter to the 'lexBuffer', and set the 'lexInput' to the value of
-- the second string parameter. Most lexers simply takes the input, breaks it, then places the two
-- halves back into the 'LexerState', which is what this function does. *Be careful* you don't pass
-- the wrong string as the second parameter. Or better yet, don't use this function.
lexSetState :: (Eq tok, Enum tok) => String -> String -> GenLexer tok ()
lexSetState got remainder = modify $ \st ->
  st{lexBuffer = lexBuffer st ++ got, lexInput = remainder}

-- | Unlike simply evaluating 'Control.Monad.mzero', 'lexBacktrack' will push the contents of the
-- 'lexBuffer' back onto the 'lexInput'. This is inefficient, so if you rely on this too often you
-- should probably re-think the design of your lexer.
lexBacktrack :: (Eq tok, Enum tok) => GenLexer tok ig
lexBacktrack = modify (\st -> st{lexBuffer = "", lexInput = lexBuffer st ++ lexInput st}) >> mzero

-- | Single character look-ahead, never consumes any tokens, never backtracks unless we are at the
-- end of input.
lexLook1 :: (Eq tok, Enum tok) => GenLexer tok Char
lexLook1 = gets lexInput >>= \input -> case input of { "" -> mzero ; c:_ -> return c }

-- | Arbitrary look-ahead, creates a and returns copy of the portion of the input string that
-- matches the predicate. This function never backtracks, and it might be quite inefficient because
-- it must force strict evaluation of all characters that match the predicate.
lexCopyWhile :: (Eq tok, Enum tok) => (Char -> Bool) -> GenLexer tok String
lexCopyWhile predicate = fmap (takeWhile predicate) (gets lexInput)

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

-- lexer: update line/column with string
lexUpdLineColWithStr :: (Eq tok, Enum tok) => String -> GenLexer tok ()
lexUpdLineColWithStr input = do
  st <- get
  let tablen = lexTabWidth st
      countNLs lns cols input = case break (=='\n') input of
        (""    , ""        ) -> (lns, cols)
        (_     , '\n':after) -> countNLs (lns+1) 0 after
        (before, after     ) -> (lns, cols + foldl (+) 0 (map charPrintWidth (before++after)))
      charPrintWidth c = case c of
        c | c=='\t'   -> tablen
        c | isPrint c -> 1
        c             -> 0
      (newLine, newCol) = countNLs (lexCurrentLine st) (lexCurrentColumn st) input
  put (st{lexCurrentLine=newLine, lexCurrentColumn=newCol})

-- | Create a 'GenToken' using the contents of the 'lexBuffer', then clear the 'lexBuffer'. This
-- function backtracks if the 'lexBuffer' is empty. If you pass "Prelude.False' as the first
-- parameter the tokens in the 'lexBuffer' are not stored with the token, the token will only
-- contain the type.
makeGetToken  :: (Eq tok, Enum tok) => Bool -> tok -> GenLexer tok (GenToken tok)
makeGetToken storeChars typ = do
  st <- get
  let str = lexBuffer st
  token <- case str of
    []               -> mzero
    [c] | storeChars -> return $ GenCharToken{tokType=typ, tokChar=c}
    cx  | storeChars -> return $ GenToken{tokType=typ, tokUStr=ustr str}
    _                -> return $ GenEmptyToken{tokType=typ}
  put $
    st{ lexBuffer   = ""
      , tokenStream = tokenStream st ++
          [ GenTokenAt
            { tokenAtLineNumber   = lexCurrentLine   st
            , tokenAtColumnNumber = lexCurrentColumn st
            , getToken            = token
            } ]
      , lexTokenCounter = lexTokenCounter st + 1
      }
  lexUpdLineColWithStr str
  return token

-- | Create a token in the stream without returning it (you usually don't need the token anyway). If
-- you do need the token, use 'makeGetToken'.
makeToken :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
makeToken = void . makeGetToken True

-- | Create a token in the stream without returning it (you usually don't need the token anyway). If
-- you do need the token, use 'makeGetToken'. The token created will not store any characters, only
-- the type of the token. This can save a lot of memory, but it requires you have very descriptive
-- token types.
makeEmptyToken :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
makeEmptyToken = void . makeGetToken False

-- | Clear the 'lexBuffer' without creating a token.
clearBuffer :: (Eq tok, Enum tok) => GenLexer tok ()
clearBuffer = get >>= \st -> lexUpdLineColWithStr (lexBuffer st) >> put (st{lexBuffer=""})

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
lexUntilTerm
  :: (Eq tok, Enum tok)
  => GenLexer tok () -> GenLexer tok () -> GenLexer tok () -> GenLexer tok Bool
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

-- | A special case of 'lexUntilTerm', lexes until it finds an un-escaped terminating
-- 'Prelude.Char'. You must only provide the escape 'Prelude.Char' and the terminating
-- 'Prelude.Char'.
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
            a:ax         ->
              (lexUntil (==a), let loop = mplus (lexString str) (lexChar a >> loop) in loop)
      lexUntilTerm scan term term

-- | Takes three parameters. (1) A label for this set of 'GenLexer's used in error reporting.
-- (2) A halting predicate which backtracks if there is more tokenizing to be done, and succeeds
-- when this tokenizer is done. (3) A list of 'GenLexer's that will each be tried in turn. This
-- function loops, first trying the halting predicate, and then trying the 'GenLexer' list, and
-- looping continues until the halting predicate succeeds or fails if the 'GenLexer' list
-- backtracks. If the 'GenLexer' list backtracks, the error message is
-- > "unknown characters scanned by $LABEL tokenizer"
-- where @$LABEL@ is the string passed as parameter 1.
runLexerLoop
  :: (Eq tok, Enum tok)
  => String -> GenLexer tok () -> [GenLexer tok ()] -> GenLexer tok ()
runLexerLoop msg predicate lexers = loop 0 where
  loop i = do
    s <- gets lexInput
    shouldContinue <- msum $
      [ predicate >> return False
      , msum lexers >> return True
      , fail ("unknown characters scanned by "++msg++" tokenizer")
      ]
    seq shouldContinue $! if shouldContinue then loop (i+1) else return ()

----------------------------------------------------------------------------------------------------
-- Functions that facilitate lexical analysis.

-- | The fundamental lexer: takes a predicate over characters, if one or more characters
-- matches, a token is constructed and it is paired with the remaining string and wrapped into a
-- 'Data.Maybe.Just' value. Otherwise 'Data.Maybe.Nothing' is returned. The 'Data.Maybe.Maybe' type
-- is used so you can combine fundamental tokenizers using 'Control.Monad.mplus'.
lexSimple :: (Eq tok, Enum tok) => tok -> (Char -> Bool) -> GenLexer tok ()
lexSimple tok predicate = lexWhile predicate >> makeToken tok

-- | A fundamental lexer using 'Data.Char.isSpace' and evaluating to a 'Space' token.
lexSpace :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexSpace tok = lexSimple tok isSpace

-- | A fundamental lexer using 'Data.Char.isAlpha' and evaluating to a 'Alphabetic' token.
lexAlpha :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexAlpha tok = lexSimple tok isAlpha

-- | A fundamental lexer using 'Data.Char.isDigit' and evaluating to a 'Digits' token.
lexDigits :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexDigits tok = lexSimple tok isDigit

-- | A fundamental lexer using 'Data.Char.isHexDigit' and evaluating to a 'HexDigit' token.
lexHexDigits :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexHexDigits tok = lexSimple tok isHexDigit

-- | Constructs an operator 'GenLexer' from a string of operators separated by spaces. For example,
-- pass @"+ += - -= * *= ** / /= % %= = == ! !="@ to create 'Lexer' that will properly parse all of
-- those operators. The order of the operators is *NOT* important, repeat symbols are tried only
-- once, the characters @+=@ are guaranteed to be parsed as a single operator @["+="]@ and not as
-- @["+", "="]@. *No token is created,* you must create your token using 'makeToken' or
-- 'makeEmptyToken' immediately after evaluating this tokenizer.
lexOperator :: (Eq tok, Enum tok) => String -> GenLexer tok ()
lexOperator ops =
  msum (map (\op -> lexString op) $ reverse $ nub $ sortBy len $ words ops)
  where
    len a b = case compare (length a) (length b) of
      EQ -> compare a b
      GT -> GT
      LT -> LT

lexToEndline :: (Eq tok, Enum tok) => GenLexer tok ()
lexToEndline = lexUntil (=='\n')

lexInlineComment :: (Eq tok, Enum tok) => tok -> String -> String -> GenLexer tok ()
lexInlineComment tok startStr endStr = do
  lexString startStr
  completed <- lexUntilTermStr "" endStr
  if completed
    then  makeToken tok
    else  fail "comment runs past end of input"

lexInlineC_Comment :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexInlineC_Comment tok = lexInlineComment tok "/*" "*/"

lexEndlineC_Comment :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexEndlineC_Comment tok = lexString "//" >> lexUntil (=='\n') >> makeToken tok

lexInlineHaskellComment :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexInlineHaskellComment tok = lexInlineComment tok "{-" "-}"

lexEndlineHaskellComment :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexEndlineHaskellComment tok = lexString "--" >> lexToEndline >> makeToken tok

-- | A lot of programming languages provide only end-line comments beginning with a (@#@) character.
lexEndlineCommentHash :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexEndlineCommentHash tok = lexChar '#' >> lexToEndline >> makeToken tok

lexStringLiteral :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexStringLiteral tok = do
  lexChar '"'
  completed <- lexUntilTermChar '\\' '"'
  if completed
    then  makeToken tok
    else  fail "string literal expression runs past end of input"

lexCharLiteral :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexCharLiteral tok = lexChar '\'' >> lexUntilTermChar '\\' '\'' >> makeToken tok

-- | This actually tokenizes a general label: alpha-numeric and underscore characters starting with
-- an alphabetic or underscore character. This is useful for several programming languages.
-- Evaluates to a 'Keyword' token type, it is up to the 'TokStream's in the syntacticAnalysis phase
-- to sort out which 'Keyword's are actually keywords and which are labels for things like variable
-- names.
lexKeyword :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexKeyword tok = do
  lexWhile (\c -> isAlpha c || c=='_')
  lexOptional (lexWhile (\c -> isAlphaNum c || c=='_'))
  makeToken tok

-- | Create a 'GenLexer' that lexes the various forms of numbers. If the number contains no special
-- modification, i.e. no hexadecimal digits, no decimal points, and no exponents, a 'Digits' token
-- is returned. Anything more exotic than simple base-10 digits and a 'Number' token is returned. If
-- the 'Number' is expressed in base-10 and also has an exponent, like @6.022e23@ where @e23@ is the
-- exponent or @1.0e-10@ where @e-10@ is the exponent, then 'NumberExp' is returned.
-- 
-- Hexadecimal and binary number expressions are also tokenized, the characters @'x'@, @'X'@, @'b'@,
-- and @'B'@ are all prefixes to a string of hexadecimal digits (tokenized with
-- 'Data.Char.isHexDigit'). So the following expression are all parsed as 'Number' tokens:
-- > 0xO123456789ABCDEfabcdef, 0XO123456789ABCDEfabcdef, 0bO123456789, 0BO123456789
-- these could all be valid hexadecimal or binary numbers. Of course @0bO123456789@ is
-- not a valid binary number, but this lexer does not care about the actual value, it is expected
-- that the 'TokStream' report it as an error during the 'syntacticAnalysis' phase. Floating-point
-- decimal numbers are also lexed appropriately, and this includes floating-point numbers expressed
-- in hexadecimal. Again, if your language must disallow hexadecimal floating-point numbers, throw
-- an error in the 'syntacticAnalysis' phase.
lexNumber :: (Eq tok, Enum tok) => tok -> tok -> tok -> tok -> GenLexer tok ()
lexNumber digits hexDigits number numberExp = do
  let altBase typ xb@(u:l:_) pred = do
        lexCharP (charSet xb)
        mplus (lexWhile pred) (fail ("no digits after "++typ++" 0"++l:" token"))
  (getDot, isHex) <- msum $
    [ do  lexChar '0' -- lex a leading zero
          msum $
            [ altBase "hexadecimal" "Xx" isHexDigit >> return (True , True )
            , altBase "binary"      "Bb" isDigit    >> return (True , False)
            , lexWhile isDigit                      >> return (True , False)
            , return (True , False)
              -- ^ a zero not followed by an 'x', 'b', or any other digits is also valid
            ]
    , lexChar '.' >> lexWhile isDigit >> return (False, False) -- lex a leading decimal point
    , lexWhile isDigit                >> return (True , False) -- lex an ordinary number
    , lexBacktrack
    ]
  (gotDot, gotExp) <- flip mplus (return (False, False)) $ do
    gotDot <-
      if getDot -- we do not have the dot?
        then  flip mplus (return False) $ do
                lexChar '.'
                mplus (lexWhile (if isHex then isHexDigit else isDigit))
                      (fail "no digits after decimal point")
                return True
        else  return True -- we already had the dot
    if isHex
      then  return (gotDot, False) -- don't look for an exponent
      else  flip mplus (return (gotDot, False)) $ do
              lexCharP (charSet "Ee")
              lexOptional (lexCharP (charSet "-+"))
              mplus (lexWhile isDigit)
                    (fail "no digits after exponent mark in decimal-point number")
              return (gotDot, True )
  makeToken $
    if gotDot || gotExp
      then  if gotExp then numberExp else number
      else  if isHex  then hexDigits else digits

-- | Creates a 'Label' token for haskell data type names, type names, class names, or constructors,
-- i.e. one or more labels (alpha-numeric and underscore characters) separated by dots (with no
-- spaces between the dots) where every first label is a capital alphabetic character, and the final
-- label may start with a lower-case letter or underscore, or the final label may also be
-- punctuation surrounded by parens. Examples are: @Aaa.Bbb@, @D.Ee_ee.ccc123@, @Aaa.Bbb.Ccc.___d1@,
-- @A.B.C.(-->)@.
lexHaskellLabel :: (Eq tok, Enum tok) => tok -> GenLexer tok ()
lexHaskellLabel tok = loop 0 where
  label    = do
    lexCharP (\c -> isUpper c && isAlpha c)
    lexOptional (lexWhile isAlphaNum_)
  loop   i = mplus (label >> mplus (tryDot i) done) (if i>0 then final else mzero)
  tryDot i = lexChar '.' >> loop (i+1)
  final    = mplus (lexCharP isAlpha_ >> lexOptional (lexWhile isAlphaNum_)) oper >> done
  oper     = do lexChar '('
                mplus (lexWhile (\c -> c/=')' && isSymPunct c) >> lexChar ')')
                      (fail "bad operator token after final dot of label")
  done     = makeToken tok

-- | Takes a 'tokenStream' resulting from the evaulation of 'lexicalAnalysis' and breaks it into
-- 'GenLine's. This makes things a bit more efficient because it is not necessary to store a line
-- number with every single token. It is necessary for initializing a 'TokStream'.
tokenStreamToLines :: (Eq tok, Enum tok) => [GenTokenAt tok] -> [GenLine tok]
tokenStreamToLines toks = loop toks where
  makeLine num toks =
    GenLine
    { lineLineNumber = num
    , lineTokens     = map (\t -> (tokenAtColumnNumber t, getToken t)) toks
    }
  loop toks = case toks of
    []     -> []
    t:toks ->
      let num           = tokenAtLineNumber t
          (line, toks') = span ((==num) . tokenAtLineNumber) (t:toks)
      in  makeLine num line : loop toks'

-- | The 'GenLexer's analogue of 'Control.Monad.State.runState', runs the lexer using an existing
-- 'LexerState'.
lexicalAnalysis
  :: (Eq tok, Enum tok)
  => GenLexer tok a
  -> GenLexerState tok
  -> (PValue (GenError (GenLexerState tok) tok) a, GenLexerState tok)
lexicalAnalysis lexer st = runState (runPTrans (runLexer lexer)) st

testLexicalAnalysis_withFilePath
  :: (Eq tok, Enum tok, Show tok)
  => GenLexer tok () -> FilePath -> TabWidth -> String -> IO ()
testLexicalAnalysis_withFilePath tokenizer filepath tablen input = putStrLn report where
  (result, st) = lexicalAnalysis tokenizer ((newLexerState input){lexTabWidth=tablen})
  lines  = tokenStreamToLines (tokenStream st)
  more   = take 21 (lexInput st)
  remain = "\nremaining: "++(if length more > 20 then show (take 20 more)++"..." else show more)
  loc    = show (lexCurrentLine st) ++ ":" ++ show (lexCurrentColumn st)
  report = (++remain) $ intercalate "\n" (map showLine lines) ++ '\n' : case result of
    OK      _ -> "Success!"
    Backtrack -> reportFilepath ++ ": lexical analysis evalauted to \"Backtrack\""
    PFail err -> reportFilepath ++ show err
  showLine line = concat $
    [ "----------\nline "
    , show (lineNumber line), "\n"
    , intercalate ", " $ map showTok (lineTokens line)
    ]
  showTok (col, tok) = concat [show col, " ", show (tokType tok), " ", show (tokToUStr tok)]
  reportFilepath = (if null filepath then "" else filepath)++":"++loc

-- | Run the 'lexicalAnalysis' with the 'GenLexer' on the given 'Prelude.String' and print out
-- every token created.
testLexicalAnalysis
  :: (Eq tok, Enum tok, Show tok)
  => GenLexer tok () -> TabWidth -> String -> IO ()
testLexicalAnalysis a b c = testLexicalAnalysis_withFilePath a "" b c

-- | Run the 'lexicalAnalysis' with the 'GenLexer' on the contents of the file at the the given
-- 'System.IO.FilePath' 'Prelude.String' and print out every token created.
testLexicalAnalysisOnFile
  :: (Eq tok, Enum tok, Show tok)
  => GenLexer tok () -> TabWidth -> FilePath -> IO ()
testLexicalAnalysisOnFile a b c = readFile c >>= testLexicalAnalysis_withFilePath a c b

----------------------------------------------------------------------------------------------------
-- $The_parser_class
-- This module provides the 'TokStream' and 'GenParser' monads, both of which can be used to
-- construct parsers, and so there is a set of functions which are common to both of these monads.
-- You can also extend either of these monads with your own data type and instantiate this class to
-- make use of the same functions.
-- 
-- Your might parser your own custom parsers that behave slightly differently than the fundamental
-- 'TokStream' parser. For example, a 'TokStream' parser doesn't care about the kind of tokens you
-- use, so the 'token' function might simply take the token, compares it to the token in the stream,
-- and then decides whether or not to shift it off the stream and use it, or put it back and fail.
-- On the other hand, the 'GenParser' monad requires your tokens to instantiate the 'Data.Ix.Ix'
-- class so that your tokens can be used to build efficient lookup tables. The instantiation of the
-- 'token' function for the 'GenParser' monad works very differently (more efficiently) under the
-- hood as compared to the ordinary 'TokStream' monad which does not build lookup tables.

-- | This class declares a set of functions that are expected to be common to all parsers dervied
-- from the parsers in this module. *The minimal complete definition is:*
-- > 'guardEOF'
-- > 'shiftPos'
-- > 'unshift'
-- but these default definitions are very inefficient. Hopefully your parser will extend a more
-- efficient instantiation of 'MonadParser' such as 'TokStream' or better yet 'GenParser'.
class (Eq tok, Enum tok, Monad (parser tok), MonadPlus (parser tok)) =>
  MonadParser parser tok where
    -- | A 'Control.Monad.guard'-like function that backtracks if there are still tokens in the
    -- token stream, or succeeds if there are no tokens left.
    guardEOF :: (Eq tok, Enum tok) => parser tok ()
    -- | Put a token and it's positional information into the front of the token stream, so the very
    -- next call to 'shiftPos' or 'look1Pos' will retrieve the information you passed to this
    -- function. This is used to implement backtracking in a function like 'tokenP'. Use of this
    -- function is an anti-pattern, meaning if you find yourself using this function often, say
    -- *more than once* in your entire module (and that one usage is not within the instantiation of
    -- 'tokenP'), you have probably done a terrible job of designing your parser.
    unshift :: (LineNum, ColumnNum, GenToken tok) -> parser tok ()
    -- | Shift the token off of the token stream along with it's positional information and succeed.
    -- Backtrack if there are no more tokens.
    shiftPos :: parser tok (LineNum, ColumnNum, GenToken tok)
    -- | Look-ahead 1 token, i.e. copy the token off of the token stream without shifting it out of
    -- the stream. The token is copied along with it's positional information. Succeed if there was
    -- a token that could be copied, backtrack if the token stream is empty.
    look1Pos :: parser tok (LineNum, ColumnNum, GenToken tok)
    look1Pos = shiftPos >>= \t -> unshift t >> return t
    -- | Like 'tokenP' but is only concerned with the type of the token. The 'GenParser'
    -- instantiation of this function uses the token type as an array index and stores the
    -- sub-parser at that index. The 'TokStream' instantiation of this function is merely a special
    -- case of 'tokenP'.
    token :: tok -> parser tok a -> parser tok a
    token tok parser = do
      kept@(_, _, nxt) <- shiftPos
      mplus (if tok == tokType nxt then parser else mzero) (unshift kept >> mzero)
    -- | Like 'tokenP' but is only concerned with the string-value of the token. The 'GenParser'
    -- instantiation of this function uses the token string as a 'Data.Map.Map' key and stores the
    -- sub-parser at that key. The default instantiation of this function is merely a special
    -- case of 'tokenP'.
    tokenUStr :: UStr -> parser tok a -> parser tok a
    tokenUStr u parser = do
      kept@(_, _, nxt) <- shiftPos
      mplus (if u == tokToUStr nxt then parser else mzero) (unshift kept >> mzero)
    -- | Exactly like 'tokenUStr' but uses a 'Prelude.String' as the input to be checked.
    tokenStr :: String -> parser tok a -> parser tok a
    tokenStr s = tokenUStr (ustr s)
    -- | Like 'tokenPosP' but does not care about the 'LineNum' or 'ColumnNum'.
    tokenP :: GenToken tok -> parser tok a -> parser tok a
    tokenP tok parser = do
      kept@(_, _, nxt) <- shiftPos
      mplus (if tokType tok == tokType nxt && tokToUStr tok == tokToUStr nxt then parser else mzero)
            (unshift kept >> mzero)
    -- | A token predicate, the behavior of this function is to be a predicate on the next token in
    -- the token stream and shift the token off of the stream and use it to evaluate the predicate.
    -- If the predicate evaluates successfully, the sub-parser provided as the second parameter is
    -- evaluated. If either the sub-parser or the predicate backtrack, the token is placed back onto
    -- the token stream. Your own instance does not need to do this exact same thing, but it is less
    -- confusing if you do it this way.
    tokenPosP :: (LineNum, ColumnNum, GenToken tok) -> parser tok a -> parser tok a
    tokenPosP (_, _, tok) parser = tokenP tok parser

-- | Like 'shiftPos' but conveniently removes the 'LineNum' and 'ColumnNum', since usually you
-- don't need that information.
shift :: MonadParser parser tok => parser tok (GenToken tok)
shift = shiftPos >>= \ (_, _, tok) -> return tok

-- | Like 'look1Pos' but conveniently removes the 'LineNum' and 'ColumnNum', since usually you
-- don't need that information.
look1 :: MonadParser parser tok => parser tok (GenToken tok)
look1 = look1Pos >>= \ (_, _, tok) -> return tok

-- | Return the current line and column of the current token without modifying shifting the token
-- stream.
getCursor :: MonadParser parser tok => parser tok (LineNum, ColumnNum)
getCursor = look1Pos >>= \ (a,b, _) -> return (a,b)

-- | If the given 'Parser' backtracks then evaluate to @return ()@, otherwise ignore the result of
-- the 'Parser' and evaluate to @return ()@.
ignore :: MonadParser parser tok => parser tok ig -> parser tok ()
ignore parser = mplus (parser >> return ()) (return ()) 

-- | Return the default value provided in the case that the given 'TokStream' fails, otherwise
-- return the value returned by the 'TokStream'.
defaultTo :: MonadParser parser tok => a -> parser tok a -> parser tok a
defaultTo defaultValue parser = mplus parser (return defaultValue)

----------------------------------------------------------------------------------------------------
-- $The_parser_data_types
-- A parser is defined as a stateful monad for analyzing a stream of tokens. A token stream is
-- represented by a list of 'GenLine' structures, and the parser monad's jobs is to look at the
-- current line, and extract the current token in the current line in the state, and use the tokens
-- to construct data. 'TokStream' is the fundamental parser, but it might be very tedious to use. It
-- is better to construct parsers using 'GenParser' which is a higher-level, easier to use data type
-- that is converted into the lower-level 'TokStream' type.

-- | The 'TokStreamState' contains a stream of all tokens created by the 'lexicalAnalysis' phase.
-- This is the state associated with a 'TokStream' in the instantiation of 'Control.Mimport
-- Debug.Traceonad.State.MonadState', so 'Control.Monad.State.get' returns a value of this data
-- type.
data TokStreamState st tok
  = TokStreamState
    { userState   :: st
    , getLines    :: [GenLine tok]
    , recentTokens :: [(LineNum, ColumnNum, GenToken tok)]
      -- ^ single look-ahead is common, but the next token exists within the 'Prelude.snd' value
      -- within a pair within a list within the 'lineTokens' field of a 'GenLine' data structure.
      -- Rather than traverse that same path every time 'nextToken' or 'withToken' is called, the
      -- next token is cached here.
    }

newParserState :: (Eq tok, Enum tok) => ust -> [GenLine tok] -> TokStreamState ust tok
newParserState userState lines =
  TokStreamState{userState = userState, getLines = lines, recentTokens = []}

modifyUserState :: (Eq tok, Enum tok) => (st -> st) -> TokStream st tok ()
modifyUserState fn = modify (\st -> st{userState = fn (userState st)})

-- | The task of the 'TokStream' monad is to look at every token in order and construct syntax trees
-- in the 'syntacticAnalysis' phase.
--
-- This function instantiates all the useful monad transformers, including 'Data.Functor.Functor',
-- 'Control.Monad.Monad', 'Control.MonadPlus', 'Control.Monad.State.MonadState',
-- 'Control.Monad.Error.MonadError' and 'Dao.Predicate.MonadPlusError'. Backtracking can be done
-- with 'Control.Monad.mzero' and "caught" with 'Control.Monad.mplus'. 'Control.Monad.fail' and
-- 'Control.Monad.Error.throwError' evaluate to a control value containing a 'GenError' value
-- which can be caught by 'Control.Monad.Error.catchError', and which automatically contain
-- information about the location of the failure and the current token in the stream that caused the
-- failure.
newtype TokStream ust tok a
  = TokStream{
      parserToPTrans ::
        PTrans (GenError (TokStreamState ust tok) tok) (State (TokStreamState ust tok)) a
    }
instance (Eq tok, Enum tok) =>
  Functor   (TokStream st tok) where { fmap f (TokStream a) = TokStream (fmap f a) }
instance (Eq tok, Enum tok) =>
  Monad (TokStream ust tok) where
    (TokStream ma) >>= mfa = TokStream (ma >>= parserToPTrans . mfa)
    return a               = TokStream (return a)
    fail msg = do
      ab  <- optional getCursor
      tok <- optional look1
      st  <- get
      throwError $
        GenError
        { parseErrLoc = fmap (uncurry atPoint) ab
        , parseErrMsg = Just (ustr msg)
        , parseErrTok = tok
        , parseStateAtErr = Just st
        }
instance (Eq tok, Enum tok) =>
  MonadPlus (TokStream ust tok) where
    mzero                             = TokStream mzero
    mplus (TokStream a) (TokStream b) = TokStream (mplus a b)
instance (Eq tok, Enum tok) =>
  Applicative (TokStream ust tok) where { pure = return ; (<*>) = ap; }
instance (Eq tok, Enum tok) =>
  Alternative (TokStream ust tok) where { empty = mzero; (<|>) = mplus; }
instance (Eq tok, Enum tok) =>
  MonadState (TokStreamState ust tok) (TokStream ust tok) where
    get = TokStream (PTrans (fmap OK get))
    put = TokStream . PTrans . fmap OK . put
instance (Eq tok, Enum tok) =>
  MonadError (GenError (TokStreamState ust tok) tok) (TokStream ust tok) where
    throwError err = do
      st <- get
      assumePValue (PFail (err{parseStateAtErr=Just st}))
    catchError (TokStream ptrans) catcher = TokStream $ do
      pval <- catchPValue ptrans
      case pval of
        OK      a -> return a
        Backtrack -> mzero
        PFail err -> parserToPTrans (catcher err)
instance (Eq tok, Enum tok) =>
  MonadPlusError (GenError (TokStreamState ust tok) tok) (TokStream ust tok) where
    catchPValue (TokStream ptrans) = TokStream (catchPValue ptrans)
    assumePValue                   = TokStream . assumePValue
instance (Eq tok, Enum tok, Monoid a) =>
  Monoid (TokStream ust tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }
instance (Eq tok, Enum tok) =>
  MonadParser (TokStream ust) tok where
    guardEOF = mplus (look1Pos >> return False) (return True) >>= guard
    unshift postok = modify (\st -> st{recentTokens = postok : recentTokens st})
    shiftPos = nextTokenPos True
    look1Pos = nextTokenPos False

-- | Return the next token in the state along with it's line and column position. If the boolean
-- parameter is true, the current token will also be removed from the state.
nextTokenPos :: (Eq tok, Enum tok) => Bool -> TokStream st tok (LineNum, ColumnNum, GenToken tok)
nextTokenPos doRemove = do
  st <- get
  case recentTokens st of
    [] -> case getLines st of
      []         -> mzero
      line:lines -> case lineTokens line of
        []                 -> put (st{getLines=lines}) >> nextTokenPos doRemove
        (colNum, tok):toks -> do
          let postok = (lineNumber line, colNum, tok)
              upd st = st{ getLines = line{ lineTokens = toks } : lines }
          if doRemove -- the 'recentTokens' buffer is cleared here regardless.
            then  put (upd st)
            else  put ((upd st){ recentTokens = [postok] })
          return postok
    tok:tokx | doRemove -> put (st{recentTokens=tokx}) >> return tok
    tok:tokx            -> return tok
      -- ^ if we remove a token, the 'recentTokens' cache must be cleared because we don't know what
      -- the next token will be. I use 'mzero' to clear the cache, it has nothing to do with the
      -- parser backtracking.

-- | A 'marker' immediately stores the cursor onto the stack. It then evaluates the given 'Parser'.
-- If the given 'Parser' fails, the position of the failure (stored in a 'Dao.Token.Location') is
-- updated such that the starting point of the failure points to the cursor stored on the stack by
-- this 'marker'. When used correctly, this function makes error reporting a bit more helpful.
marker :: MonadParser (TokStream ust) tok => TokStream ust tok a -> TokStream ust tok a
marker parser = do
  ab <- mplus (fmap Just getCursor) (return Nothing)
  flip mapPFail parser $ \parsErr ->
    parsErr
    { parseErrLoc =
        let p = parseErrLoc parsErr
        in  mplus (p >>= \loc -> ab >>= \ (a, b) -> return (mappend loc (atPoint a b))) p
    }

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
expect :: MonadParser (TokStream ust) tok => String -> TokStream ust tok a -> TokStream ust tok a
expect errMsg parser = do
  (a, b) <- getCursor
  let expectMsg = "expecting "++errMsg
  mplus parser (throwError ((parserErr a b){parseErrMsg = Just (ustr expectMsg)}))

-- | The 'TokStream's analogue of 'Control.Monad.State.runState', runs the parser using an existing
-- 'TokStreamState'.
runParserState
  :: MonadParser (TokStream ust) tok
  => TokStream ust tok a
  -> TokStreamState ust tok
  -> (PValue (GenError (TokStreamState ust tok) tok) a, TokStreamState ust tok)
runParserState (TokStream parser) = runState (runPTrans parser)

-- | This is the second phase of parsing, takes a stream of tokens created by the 'lexicalAnalysis'
-- phase (the @['GenLine' tok]@ parameter) and constructs a syntax tree data structure using the
-- 'TokStream' monad provided.
syntacticAnalysis
  :: MonadParser (TokStream ust) tok
  => TokStream ust tok synTree
  -> ust
  -> [GenLine tok]
  -> (PValue (GenError (TokStreamState ust tok) tok) synTree, TokStreamState ust tok)
syntacticAnalysis parser userState lines = runParserState parser $ newParserState userState lines

----------------------------------------------------------------------------------------------------
-- $ParseTable
-- ParseTable is a data type (e.g. it is a 'Data.Monoid.Monoid') and also a control structure (e.g.
-- it is a 'Control.Monad.Monad' and a 'Control.Applicative.Alternative'). As a control structure,
-- it allows you to construct parser functions that generate objects from token streams. As a data
-- structure (internally) a sparse matrix parsing table is constructed to do this.
-- 
-- When you make use of 'Control.Monad.Monad', 'Control.Monad.MonadPlus',
-- 'Control.Applicative.Applicative', 'Control.Applicative.Alternative', and other classes to
-- construct your 'ParseTable', behind the scenes the combinators are actually defining a sparse
-- matrix with a default parser action of 'Control.Monad.mzero' which are filled-in with values
-- supplied to the the 'token' function. If your 'ParseTable' refers to an enumerated data type which
-- instantiates this class, you will also be able to fill-in the parser table with pointers to other
-- columns in the table.
--
-- You are presumably using GHC, so you can use the @{-#INLINE ... #-}@ pragma to improve efficency.
-- To create a row in the parser table, you would use the function 'token' or 'tokens' and combine
-- them with from 'Control.Monad.mplus', 'Control.Monad.msum', and ('Control.Applicative.<|>').
-- Behind the scenes, this module will construct an 'Data.Array.Array' so that it may very quickly
-- lookup the next parser. By making these functions top-level functions and using
-- @{-#INLINE ... #-}@ pragma, GHC will try harder to make sure these arrays are only allocated
-- once. Although inlining tends to occur without @{-#INLINE ... #-}@, it is not a guarantee.

data ParseTable st tok a
  = ParseTableArray { parseTableArray :: Array tok (ParseTable st tok a) }
    -- ^ a parser table constructed from 'ParseTableArrayItem's. This is the most efficient method to
    -- parse, so it is best to try and construct your parser from 'ParseTableArrayItem's rather than
    -- simply using monadic notation to stick together a bunch of parsing functions.
  | ParseTableMap { parserMap :: M.Map UStr (ParseTable st tok a) }
  | ParseTable { tokStreamParser :: TokStream st tok a }
    -- ^ a plain parser function, used for lifting 'TokStream' into the 'ParseTable' monad,
    -- specificaly for the 'Control.Monad.return' function.
instance (Ix tok, Enum tok) => Monad (ParseTable st tok) where
  return a = ParseTable { tokStreamParser = return a }
  fail     = ParseTable . fail
  parser >>= bindTo =
    ParseTable{
      tokStreamParser = evalTableToTokStream parser >>= \a -> evalTableToTokStream (bindTo a)
    }
instance (Ix tok, Enum tok) => Functor (ParseTable ust tok) where
  fmap f parser = case parser of
    ParseTableArray     arr        ->
      ParseTableArray { parseTableArray = amap (\origFunc -> fmap f origFunc) arr }
    ParseTable      parser ->
      ParseTable{ tokStreamParser = fmap f parser}
instance (Ix tok, Enum tok) =>
  MonadPlus (ParseTable ust tok) where
    mzero     = ParseTable{ tokStreamParser = mzero }
    mplus a b = case a of
      ParseTableArray arrA -> case b of
        ParseTableArray arrB -> merge (assocs arrA ++ assocs arrB)
        ParseTable      fb   -> ParseTable{tokStreamParser = mplus (evalParseArray arrA) fb}
      ParseTable      fa   -> ParseTable $ mplus fa $ case b of
        ParseTableArray arrB -> evalParseArray arrB
        ParseTable      fb   -> fb
      where
        minmax a = foldl (\ (n, x) a -> (min n a, max x a)) (a, a) . map fst
        merge ax = case ax of
          []                 -> mzero
          ax@((tok, func):_) -> ParseTableArray{
              parseTableArray = accumArray mplus mzero (minmax tok ax) ax
            }
instance (Ix tok, Enum tok) =>
  Applicative (ParseTable ust tok) where { pure = return; (<*>) = ap; }
instance (Ix tok, Enum tok) =>
  Alternative (ParseTable ust tok) where { empty = mzero; (<|>) = mplus; }
instance (Ix tok, Enum tok, Monoid a) =>
  Monoid (ParseTable ust tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }
instance (Ix tok, Enum tok) =>
  MonadState ust (ParseTable ust tok) where
    get = ParseTable (gets userState)
    put st = ParseTable $ modify $ \parserState -> parserState{userState=st}
instance (Ix tok, Enum tok) =>
  MonadError (GenError (TokStreamState ust tok) tok) (ParseTable ust tok) where
    throwError = ParseTable . throwError
    catchError trial catcher = ParseTable $
      catchError (evalTableToTokStream trial) (\err -> evalTableToTokStream (catcher err))
instance (Ix tok, Enum tok) =>
  MonadPlusError (GenError (TokStreamState ust tok) tok) (ParseTable ust tok) where
    catchPValue ptrans = ParseTable (catchPValue (evalTableToTokStream ptrans))
    assumePValue       = ParseTable . assumePValue
instance (Ix tok, Enum tok) =>
  MonadParser (ParseTable ust) tok where
    guardEOF = ParseTable guardEOF
    unshift  = ParseTable . unshift
    shiftPos = ParseTable shiftPos
    look1Pos = ParseTable look1Pos

-- | Evaluate a 'ParseTable' to a 'TokStream'.
evalTableToTokStream :: (Ix tok, Enum tok) => ParseTable st tok a -> TokStream st tok a
evalTableToTokStream table = case table of
  ParseTable      parser -> parser
  ParseTableArray parser -> evalParseArray parser
  ParseTableMap   parser -> evalParseMap   parser

-- Efficiently evaluates a array from a 'ParseTableArray'. One token is shifted from the stream, and
-- the 'tokType' is used to select and evaluate the 'TokStream' in the 'TokStream'. The 'tokToStr'
-- value of the token is passed to the selected 'TokStream'. If the 'tokType' does not exist in the
-- table, or if the selected parser backtracks, this parser backtracks and the selecting token is
-- shifted back onto the stream.
evalParseArray :: (Ix tok, Enum tok) => Array tok (ParseTable st tok a) -> TokStream st tok a
evalParseArray arr = do
  tok <- fmap tokType look1
  if inRange (bounds arr) tok then evalTableToTokStream (arr!tok) else mzero

evalParseMap :: (Ix tok, Enum tok) => M.Map UStr (ParseTable st tok a) -> TokStream st tok a
evalParseMap m = do
  tok <- fmap tokToUStr look1
  join $ fmap evalTableToTokStream $ assumePValue $ maybeToBacktrack $ M.lookup tok m

----------------------------------------------------------------------------------------------------
-- $Generalized_state_transition_parser
-- This parser is a high-level representation of parsers. It is essentially a data structure that
-- behaves like a 'MonadParser', but this data type is really a meta-structure for building
-- 'TokStream' parsers. When you convert a 'GenParser' to a 'ParseTable' using
-- 'evalGenParserToParseTable', the high-level structure of the 'GenParser' is stored into a sparse
-- matrix with every column of the matrix being an array of token types associated with an action
-- function that can be translated into a 'TokStream' function. As long as the 'ParseTable' does not
-- go out of scope, the matrix will not be garbage collected. Furthermore, using arrays to select
-- the next 'TokStream' parser to evaluated is extremely fast.
--
-- 'GenParser' of course instantiates 'MonadParser' so you can use all the ordinary functions like
-- 'token' and 'shift' to build up a parser. However you should never need to use 'shift' or
-- 'unshift'. 'GenParser' is specially designed to construct an efficient 'TokStream' parser that
-- takes care of the 'shift'ing and 'unshift'ing so that 'unshift'ing is used as little as possible.
--
-- All you need to do is specify how to convert tokens into data structures using 'token',
-- 'tokenUStr', and 'tokenStr', and use 'Control.Monad.Monad' and 'Control.Applicative.Applicative'
-- to construct objects from simpler parsers.

-- | The *Generalized State Transition Parser*. Use this to build parsers.
data GenParser ust tok a
  = GenParserBacktrack
  | GenParserConst  { constValue  :: a }
  | GenParserFail   { failMessage :: String }
  | GenParserUpdate { updateInner :: ParseTable ust tok a }
  | GenParserTable
    { checkTokenAndString :: M.Map tok  (M.Map UStr (GenParser ust tok a))
    , checkToken          :: M.Map tok  (GenParser ust tok a)
    , checkString         :: M.Map UStr (GenParser ust tok a)
    }
instance (Ix tok, Enum tok) =>
  Monad (GenParser ust tok) where
    return     = GenParserConst
    fail       = GenParserFail
    p >>= bind = case p of
      GenParserBacktrack            -> GenParserBacktrack
      GenParserConst a              -> bind a
      GenParserFail msg             -> GenParserFail msg
      GenParserUpdate p -> GenParserUpdate{
          updateInner = do
            a <- p >>= evalGenParserToParseTable . bind
            ParseTable shift >> return a
        }
      GenParserTable tokstr tok str ->
        GenParserTable
        { checkTokenAndString = fmap (fmap (>>=bind)) tokstr
        , checkToken          = fmap (>>=bind) tok
        , checkString         = fmap (>>=bind) str
        }
instance (Ix tok, Enum tok) =>
  Functor (GenParser ust tok) where
    fmap f p = case p of
      GenParserBacktrack -> GenParserBacktrack
      GenParserConst  a -> GenParserConst (f a)
      GenParserFail msg -> GenParserFail  msg
      GenParserUpdate p -> GenParserUpdate (fmap f p)
      GenParserTable tokstr tok str ->
        GenParserTable
        { checkTokenAndString = fmap (fmap (fmap f)) tokstr
        , checkToken          = fmap (fmap f) tok
        , checkString         = fmap (fmap f) str
        }
instance (Ix tok, Enum tok) =>
  MonadPlus (GenParser ust tok) where
    mzero     = GenParserBacktrack
    mplus a b = case a of
      GenParserBacktrack -> b
      GenParserConst   a -> GenParserConst a
      GenParserFail  msg -> GenParserFail msg
      GenParserUpdate  a -> case b of
        GenParserBacktrack -> GenParserUpdate a
        GenParserFail  msg -> GenParserUpdate a
        GenParserConst   b -> GenParserUpdate (mplus a (return b))
        GenParserUpdate  b -> GenParserUpdate (mplus a b)
        b             -> GenParserUpdate (mplus a (evalGenParserToParseTable b))
      GenParserTable tokstrA tokA strA -> case b of
        GenParserBacktrack -> GenParserTable tokstrA tokA strA
        GenParserConst   b ->
          GenParserTable
          { checkTokenAndString = fmap (fmap (flip mplus (GenParserConst b))) tokstrA
          , checkToken          = fmap (flip mplus (GenParserConst b)) tokA
          , checkString         = fmap (flip mplus (GenParserConst b)) strA
          }
        GenParserFail  msg -> GenParserTable tokstrA tokA strA
        GenParserUpdate  b ->
          GenParserTable
          { checkTokenAndString = fmap (fmap (mplus (GenParserUpdate b))) tokstrA
          , checkToken          = fmap (mplus (GenParserUpdate b)) tokA
          , checkString         = fmap (mplus (GenParserUpdate b)) strA
          }
        GenParserTable tokstrB tokB strB ->
          GenParserTable
          { checkTokenAndString = M.unionWith (M.unionWith mplus) tokstrA tokstrB
          , checkToken          = M.unionWith              mplus  tokA    tokB
          , checkString         = M.unionWith              mplus strA    strB
          }
instance (Ix tok, Enum tok) => Applicative (GenParser ust tok) where { pure = return; (<*>) = ap; }
instance (Ix tok, Enum tok) => Alternative (GenParser ust tok) where { empty = mzero; (<|>) = mplus; }
instance (Ix tok, Enum tok, Monoid a) =>
  Monoid (GenParser ust tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }
instance (Ix tok, Enum tok) =>
  MonadState ust (GenParser ust tok) where
    get = GenParserUpdate get
    put = GenParserUpdate . put
instance (Ix tok, Enum tok) =>
  MonadError (GenError (TokStreamState ust tok) tok) (GenParser ust tok) where
    throwError = GenParserUpdate . throwError
    catchError trial catcher = GenParserUpdate $
      catchError (evalGenParserToParseTable trial) (\err -> evalGenParserToParseTable (catcher err))
instance (Ix tok, Enum tok) =>
  MonadPlusError (GenError (TokStreamState ust tok) tok) (GenParser ust tok) where
    catchPValue ptrans = GenParserUpdate (catchPValue (evalGenParserToParseTable ptrans))
    assumePValue       = GenParserUpdate . assumePValue
instance (Ix tok, Enum tok) =>
  MonadParser (GenParser ust) tok where
    guardEOF = GenParserUpdate guardEOF
    unshift  = GenParserUpdate . unshift
    shiftPos = GenParserUpdate shiftPos
    look1Pos = GenParserUpdate look1Pos
    token     t p = emptyTable{checkToken  = M.singleton t p}
    tokenUStr u p = emptyTable{checkString = M.singleton u p}
    tokenP    tok p = do
      let t = tokType tok
          u = tokToUStr tok
      if u==nil
        then  token t p
        else  emptyTable{checkTokenAndString = M.singleton t (M.singleton u p)}

-- | Allows you to build your own parser table from scratch by directly mapping tokens and strings
-- to 'GenParser's using functions provided in "Data.Map".
emptyTable :: GenParser ust tok a
emptyTable =
  GenParserTable
  { checkTokenAndString = M.empty
  , checkToken          = M.empty
  , checkString         = M.empty
  }

tokenTypeUStr :: (Ix tok, Enum tok) => tok -> UStr -> GenParser ust tok a -> GenParser ust tok a
tokenTypeUStr t u = tokenP (GenToken{tokType=t, tokUStr=u})

tokenTypeStr :: (Ix tok, Enum tok) => tok -> String -> GenParser ust tok a -> GenParser ust tok a
tokenTypeStr t s = tokenTypeUStr t (ustr s)

-- | Convert a 'GenParser' to a 'ParseTable'. Doing this will lazily construct a sparse matrix which
-- becomes the state transition table for this parser, hence the token type must instantiate
-- 'Data.Ix.Ix'. Try to keep the resulting 'ParseTable' in scope for as long as there is a
-- possibility that you will use it. Every time this function is evaluated, a new set of
-- 'Data.Array.IArray.Array's are constructed to build the sparse matrix.
evalGenParserToParseTable :: (Ix tok, Enum tok) => GenParser st tok a -> ParseTable st tok a
evalGenParserToParseTable p = case p of
  GenParserBacktrack -> mzero
  GenParserConst   a -> return a
  GenParserFail  msg -> fail msg
  GenParserUpdate fn -> fn
  GenParserTable tokstr tok str -> msum $ concat $
    [ mkMapArray tokstr
    , mkArray tok
    , mkmap str
    ]
  where
    findBounds tok = foldl (\ (min0, max0) (tok, _) -> (min min0 tok, max max0 tok)) (tok, tok)
    mkMapArray :: (Ix tok, Enum tok) => M.Map tok (M.Map UStr (GenParser st tok a)) -> [ParseTable st tok a]
    mkMapArray m = do
      let ax = M.assocs m
      case ax of
        []          -> mzero
        [(tok, m)]  -> return $ do
          t <- fmap tokType (ParseTable look1)
          guard (t==tok)
          ParseTableMap{parserMap = M.map evalGenParserToParseTable m}
        (tok, _):ax' -> do
          let minmax = findBounds tok ax'
              bx     = concatMap (\ (tok, par) -> map ((,)tok) (mkmap par)) ax
          return (ParseTableArray{parseTableArray = accumArray (\_ a -> a) mzero minmax bx})
    mkArray :: (Ix tok, Enum tok) => M.Map tok (GenParser st tok a) -> [ParseTable st tok a]
    mkArray m = do
      let ax = M.assocs m
      case ax of
        []            -> mzero
        [(tok, gstp)] -> return $ do
          t <- fmap tokType (ParseTable look1)
          guard (t==tok)
          evalGenParserToParseTable gstp
        (tok, _):ax'  -> do
          let minmax = findBounds tok ax'
              bx = map (\ (tok, par) -> (tok, evalGenParserToParseTable par)) ax
          return (ParseTableArray{parseTableArray = accumArray (\_ a -> a) mzero minmax bx})
    mkmap :: (Ix tok, Enum tok) => M.Map UStr (GenParser st tok a) -> [ParseTable st tok a]
    mkmap m = case M.assocs m of
      []            -> mzero
      [(str, gstp)] -> return $ do
        t <- fmap tokToUStr (ParseTable look1)
        guard (t==str)
        evalGenParserToParseTable gstp
      _             -> return (ParseTableMap{parserMap = M.map evalGenParserToParseTable m})

----------------------------------------------------------------------------------------------------

-- | A *General Context-Free Grammar* is a data structure that allows you to easily define a
-- two-phase parser (a parser with a 'lexicalAnalysis' phase, and a 'syntacticAnalysis' phase). The
-- fields supplied to this data type are define the grammar, and the 'parse' function can be used to
-- parse an input string using the context-free grammar defined in this data structure. *Note* that
-- the parser might have two phases, but because Haskell is a lazy language and 'parse' is a pure
-- function, both phases happen at the same time, so the resulting parser does not need to parse the
-- entire input in the first phase before beginning the second phase.
data GenCFGrammar st tok synTree
  = GenCFGrammar
    { columnWidthOfTab :: TabWidth
      -- ^ specify how many columns a @'\t'@ character takes up. This number is important to get
      -- accurate line:column information in error messages.
    , mainLexer        :: GenLexer tok ()
      -- ^ *the order of these tokenizers is important,* these are the tokenizers passed to the
      -- 'lexicalAnalysis' phase to generate the stream of tokens for the 'syntacticAnalysis' phase.
    , mainParser       :: TokStream st tok synTree
      -- ^ this is the parser entry-point which is used to evaluate the 'syntacticAnalysis' phase.
    }

-- | This is *the function that parses* an input string according to a given 'GenCFGrammar'.
parse
  :: (Eq tok, Enum tok)
  => GenCFGrammar st tok synTree
  -> st -> String -> PValue (GenError st tok) synTree
parse cfg st input = case lexicalResult of
  OK      _ -> case parserResult of
    OK     a  -> OK a
    Backtrack -> Backtrack
    PFail err -> PFail $ err{parseStateAtErr=Just (userState parserState)}
  Backtrack -> Backtrack
  PFail err -> PFail $ (lexErrToParseErr err){parseStateAtErr = Nothing}
  where
    initState = (newLexerState input){lexTabWidth = columnWidthOfTab cfg}
    (lexicalResult, lexicalState) = lexicalAnalysis (mainLexer cfg) initState
    (parserResult , parserState ) =
      syntacticAnalysis (mainParser cfg) st (tokenStreamToLines (tokenStream lexicalState))

