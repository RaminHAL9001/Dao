-- "src/Dao/Parser.hs"  Construct tokenizeres from
-- 'Dao.EnumSet.EnumSet's, and parsers from tokenizers. 
-- 
-- Copyright (C) 2008-2012  Ramin Honary.
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

module Dao.Parser
  ( -- * The 'Regex' Type
    Regex, matchRegex, matchRepeat
    -- * Primitive 'Regex's
    -- $Primitive_regexs
  , rxNull, rxEmpty, rxTrue, rxFalse, rxSequence, rxChoice
  , rxChar, rxString, rxUStr, rxCharSet, rxNotCharSet, rxUnion
  , rxCharSetFromStr, rxNotCharSetFromStr
    -- * Essential 'Regex's
    -- $Essential_Regexs
  , space, hspace, upper, lower, alpha, alpha_, digit
  , alnum, alnum_, xdigit, spaceCtrl, cntrl, punct, printable, ascii
    -- * The 'Parser' Monad
  , Parser, runParser, ParseValue, ParseState, tokenStack
    -- * Working With 'Tokens'
    -- $Working_With_Tokens
  , Token, startingLine, startingChar, startingColumn
  , endingLine, endingChar, endingColumn, tokenChars, appendTokens
  , backtrack, getToken, getCombinedTokens, token
    -- * 'Parser' Combinators
    -- $Parser_Combinators
  , endOfInput, regex, readsAll
  , lookAhead, char, string, ustring, charSet, notCharSet, repeating
  , repeatRegex, zeroOrOne, regexMany, regexMany1, many, many1
    -- * Miscelaneous
  , fromReadS, choice, spanAtWord
  )
  where

import           Dao.String
import           Dao.EnumSet
import qualified Dao.Tree as T
import           Dao.Predicate

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error

import           Data.Maybe
import           Data.Word
import           Data.Char
import           Data.List

----------------------------------------------------------------------------------------------------

-- | Combines the functionality of 'Data.List.break' and 'Prelude.splitAt', and takes a 'Data.Word'
-- as the splitting point, unlike 'Data.List.splitAt' which takes an 'Data.Int.Int'.
spanAtWord :: (a -> Bool) -> Word -> [a] -> ([a], [a])
spanAtWord chk w ax = loop w [] ax where
  loop w rx ax = case ax of
    a:ax | w>0 && chk a -> loop (w-1) (rx++[a]) ax
    ax                  -> (rx, ax)

-- not for export
iLength :: Num len => [a] -> len
iLength = foldl (+) 0 . map (const 1)

-- not for export
wordReplicateM :: (Ord w, Num w, Monad m) => w -> m a -> m [a]
wordReplicateM w fn = loop w [] where
  loop w ax = if w >= 0 then fn >>= \a -> loop (w-1) (ax++[a]) else return ax

----------------------------------------------------------------------------------------------------

-- | A string predicate: predicates on characters for breaking off the head of a string. Unlike
-- POSIX regulare expressions, grouping is not supported -- instead of grouping, make use of the
-- 'Parser' combinators provided in this module.
data Regex
  = RNull
  | RFalse
  | RTrue
  | REmpty
  | RChar     Char
  | RCharSet  { rxToEnumSet :: EnumSet Char }
  | RString   UStr
  | RSequence [Regex]
  | RChoice   [Regex]
  deriving (Eq, Show)

max_regex_recurse_limit = 2^15
recursive_regex_err = error "regular expression matching surpassd maximum recursion limit"

-- | Greedy matching of strings to a string predicates ('RString's). This function never backtracks,
-- and only matches at the beginning of the state string (input string). More complex regular
-- expressions are constructed with the 'Parser' combinators in this module.
--     The types for 'matchRegex' is such that it can easily construct a monadic type, where the
-- monad is the 'Data.Maybe.Maybe' monad lifted into a 'Control.Monad.State.StateT' monad. To make
-- this function monadic, simply write: @'Control.Monad.State.Lazy.StateT' ('matchPString' parseRegex)@
-- where @parseRegex@ is a value of type 'Regex'. If the 'Regex' matches, the state will be the string
-- you are parsing, the return value will be the the string of characters that the 'Regex' matches,
-- however the parseRegex does not match, this function evaluates to an empty list.
matchRegex :: Regex -> String -> Maybe (String, String)
matchRegex reg ax = loop 0 reg ax where
  loop i reg ax 
    | i >= max_regex_recurse_limit = recursive_regex_err
    | otherwise = case reg of
        RNull        -> return ("", ax)
        RTrue        ->
          case ax of
            a:ax -> return ([a], ax)
            []   -> mzero
        RFalse       -> mzero
        REmpty       ->
          case ax of
            "" -> return ("", "")
            _  -> mzero
        RChar      c ->
          case ax of
            a:ax  | a == c -> return ([a], ax)
            _              -> mzero
        RString    r ->
          let s = uchars r in stripPrefix s ax >>= \ax -> Just (s, ax)
        RCharSet   r ->
          case ax of
            a:ax  | setMember r a -> return ([a], ax)
            _                     -> mzero
        RSequence px -> runStateT (fmap concat $ sequence $ map (StateT . loop (i+1)) px) ax
        RChoice   rx -> choiceLoop rx where
          choiceLoop rx = case rx of
            []   -> mzero
            r:rx -> let match = loop (i+1) r ax in if mzero==match then choiceLoop rx else match

-- | Match a 'Regex' repeatedly, but only succeeds if it matches more than a lower-bound number of
-- times given by the first parameter (@lo@). Once the lower limit minimum as been parsed, match as
-- many times as possible without exceeding an upper-bound number of times given by the second
-- parameter (@hi@). less than a maximum digit of times. Be careful when creating 'rxRepeat'
-- 'Regex's that match a minimum of zero characters, especially if these repeating 'Regex's are used
-- in recursive parsers. The maximum number of matches is @lo + hi@ times.
-- 
-- *WARNING:* Matching no characters will still succeed and recurse without removing any characters
-- from the input string, and thus loop indefinitely.
matchRepeat :: Word -> Word -> Regex -> String -> Maybe (String, String)
matchRepeat lo hi reg ax = case reg of -- TODO: resolve infinite loop
  RNull        -> return ("", ax)
  RFalse       -> mzero
  RTrue        -> return (spanAtWord (const True) hi ax)
  REmpty       -> matchRegex REmpty ax
  RChar    c   -> simple (==c)
  RCharSet set -> simple (setMember set)
  r            -> flip runStateT ax $
    fmap concat (wordReplicateM lo (StateT (matchRegex r))) >>= limLoop
  where
    simple fn = do
      let (smallestMatch, afterSmallestMatch) = spanAtWord (const True) lo ax
      if and (map fn smallestMatch)
        then  let (spanned, remainder) = spanAtWord fn hi afterSmallestMatch
              in  return (smallestMatch++spanned, remainder)
        else mzero
    limLoop collected = do
      ax <- get
      let next = runStateT (StateT (matchRegex reg)) ax
      if mzero==next
        then return collected
        else StateT (const next) >>= \more -> limLoop (collected++more)

----------------------------------------------------------------------------------------------------

-- $Primitive_regexs
-- To construct 'Regex's, use these primitive functions.

-- | This 'Regex' succeeds without taking any characters. *CAUTION* it can match infinitely many
-- times.
rxNull :: Regex
rxNull = RNull

-- | This 'Regex' fails to match any string.
rxFalse :: Regex
rxFalse = RFalse

-- | This 'Regex' matches every character of any string.
rxTrue :: Regex
rxTrue = RTrue

-- | This 'Regex' matches a single character.
rxChar :: Char -> Regex
rxChar = RChar

-- | This 'Regex' matches only if the input string is empty.
rxEmpty :: Regex
rxEmpty = REmpty

-- | This 'Regex' matches a 'Prelude.String'
rxString :: String -> Regex
rxString s = case s of
  ""  -> RNull
  [c] -> RChar c
  s   -> RString (ustr s)

-- | This 'Regex' matches a 'Dao.String.UStr'.
rxUStr :: UStr -> Regex
rxUStr u = case uchars u of
  ""  -> RNull
  [c] -> RChar c
  _   -> RString u

-- | A regular expression matching an @'Dao.EnumSet.EnumSet' 'Data.Char.Char'@
rxCharSet :: EnumSet Char -> Regex
rxCharSet set = case set of
  set | setIsNull set    -> RFalse
  set | set==infiniteSet -> RTrue
  set                    ->
    fromMaybe (RCharSet set) (setIsSingleton set >>= Just . RChar)

-- | Like 'rxCharSet' but creates a set from a given string of characters.
rxCharSetFromStr :: String -> Regex
rxCharSetFromStr str = rxCharSet (foldl setUnion emptySet (map point str))

-- | Like 'rxCharSet' but uses 'Dao.EnumSet.setInvert' to invert the set of characters.
rxNotCharSet :: EnumSet Char -> Regex
rxNotCharSet = rxCharSet . setInvert

-- | Like 'rxNotCharSet' but creates an inveted set from a given string of characters.
rxNotCharSetFromStr :: String -> Regex
rxNotCharSetFromStr str = rxNotCharSet (foldl setUnion emptySet (map point str))

-- | This 'Regex' matches a string if every 'Regex' in the given list matches in order.
rxSequence :: [Regex] -> Regex
rxSequence rx = case join [] (loop [] rx) of
  []  -> RNull
  [r] -> r
  rx  -> RSequence rx
  where
    loop zx rx = case rx of
      []          -> zx
      RFalse : _  -> [RFalse]
      RNull  : rx -> loop zx rx
      r      : rx -> loop (zx++[r]) rx
    join zx ax = case ax of
      []                -> []
      RSequence bx : ax -> join zx (bx++ax)
      RString u    : ax -> join (zx++[u]) ax
      a            : ax -> case zx of
        []  -> a : join [] ax
        [z] -> RString z : join [] ax
        zx  -> RString (ustr $ concatMap uchars zx) : join [] ax

-- | Given a list of 'Regex's, evaluates each in turn, and yields the the first 'Regex' in the list
-- to match a string.
rxChoice :: [Regex] -> Regex
rxChoice rx = case rx of
  []  -> RFalse
  [r] -> r
  rx  -> RChoice rx

----------------------------------------------------------------------------------------------------

-- $Essential_Regexs
-- These 'Regex's are not primitive, but they are essential to most applications. The names of these
-- 'Regex's are similar or identical to the "character classes" provided by POSIX regular
-- expressions. Please be careful not to confuse these with 'Parser' combinators provided below.
-- Remember that a 'Parser' is a monadic type that uses 'Regex's to gather characters, 'Regex's
-- alone do nothing. These are the only 'Regex's that do not start with @rx@, so do not confuse
-- these with parser combinators, for example:
-- @myParser = space >>= regexMany1 (rxChar '.') -- *TYPE ERROR!* @
-- @myParser = regexMany space >> regexMany1 (rxChar '.') -- correct@

-- | ASCII space, tab, carriage return, newline, and vertical tab.
space :: Regex
space = RCharSet $ enumSet (map single "\t\r\n\v ")

-- | ASCII space or tab only, "horizontal" space.
hspace :: Regex
hspace = RCharSet $ enumSet (map single "\t ")

-- | ASCII upper case alphabet characters.
upper :: Regex
upper = RCharSet $ range 'A' 'Z'

-- | ASCII lower case alphabet characters.
lower :: Regex
lower = RCharSet $ range 'a' 'z'

-- | ASCII alphabet characters, upper and lower case.
alpha :: Regex
alpha = RCharSet $ setUnion (rxToEnumSet upper) (rxToEnumSet lower)

-- | ASCII alphabet characters, upper and lower case, or the underscore @'_'@ character.
alpha_ :: Regex
alpha_ = RCharSet $ setUnion (rxToEnumSet alpha) (point '_')

-- | ASCII digit characters.
digit :: Regex
digit = RCharSet $ range '0' '9'

-- | ASCII digit and alphabet characters.
alnum :: Regex
alnum = RCharSet $ setUnion (rxToEnumSet digit) (rxToEnumSet alpha)

-- | ASCII digit and alphabet characters, and the underscore @'_'@ character.
alnum_ :: Regex
alnum_ = RCharSet $ setUnion (rxToEnumSet alnum) (point '_')

-- | ASCII digit characters and the alphabet characters @'A'@ through @'F'@ and @'a'@ through @'f'@
-- for hexadecimal digit parsing.
xdigit :: Regex
xdigit = RCharSet $ setUnion (rxToEnumSet digit) (enumSet [segment 'A' 'F', segment 'a' 'f'])

-- | ASCII control characters, including 'whitespace's.
spaceCtrl :: Regex
spaceCtrl = RCharSet $ enumSet [segment '\x00' '\x1F', single '\x7F']

-- | ASCII control characters, *excluding* 'whitespace's.
cntrl :: Regex
cntrl = RCharSet $ setDelete (rxToEnumSet cntrl) (rxToEnumSet space)

-- | ASCII punctuation marks.
punct :: Regex
punct = RCharSet $ enumSet [segment '!' '/', segment ':' '@', segment '[' '`', segment '{' '~']

-- | ASCII printable characters, all spaces, 'punct'uation, alphabet and digit characters.
printable :: Regex
printable = RCharSet $ setUnion (rxToEnumSet space) (range '!' '~')

-- | Any ASCII character.
ascii :: Regex
ascii = RCharSet $ range '\x00' '\x7F'

-- | Union of two 'Regex's.
rxUnion :: Regex -> Regex -> Regex
rxUnion a b  = loop 0 a b where
  errmsg = error "union operation over regexes of these particular values is undefined"
  usimple i a b = case b of
    RSequence rx -> Just $ case rx of
      []   -> a
      r:rx -> RSequence (loop (i+1) a r : rx)
    RChoice rx       -> Just $ RChoice (map (loop (i+1) RTrue) rx)
    _      -> Nothing
  seqzip i ax bx = case (ax, bx) of
    ([]  , []  ) -> []
    ([]  , bx  ) -> bx
    (ax  , []  ) -> ax
    (a:ax, b:bx) -> loop (i+1) a b : seqzip i ax bx
  mult i a ax bx = rxChoice (map (\b -> rxSequence (loop (i+1) a b : ax)) bx)
  loop i a b
    | i==max_regex_recurse_limit =
        error "depth of regular expression structure exceeds built-in limit"
    | otherwise = fromMaybe errmsg $ case a of
        RNull  -> Just $ case b of
          RFalse -> RNull
          _      -> b
        RFalse -> Just b
        RTrue  -> mplus (usimple (i+1) RTrue b) $ Just $ case b of
          RString   s  -> rxSequence [RTrue, rxUStr (ustr $ drop 1 $ uchars s)]
          _            -> RTrue
        RChar a -> mplus (usimple (i+1) (RChar a) b) $ Just $ case b of
          RTrue -> RTrue
          RString   s  -> case uchars s of
            c:cx | c==a -> RString s
            c:cx        ->
              let u = RCharSet (enumSet [single a, single c])
              in  if null cx then u else RSequence [u, RString (ustr cx)]
            _           -> RChar a
          RChar b | a/=b -> RCharSet (enumSet [single a, single b])
          RCharSet s     -> RCharSet (setUnion (point a) s)
          _     -> RChar a
        RCharSet a -> mplus (usimple (i+1) (RCharSet a) b) $ Just $ case b of
          RTrue      -> RTrue
          RChar    b -> rxCharSet (setUnion a $ point b)
          RCharSet b -> RCharSet (setUnion a b)
          RString  b -> case uchars b of
            []   -> RCharSet a
            [b]  -> RCharSet (setUnion a $ point b)
            b:bx -> rxSequence [RCharSet (setUnion a $ point b), rxString bx]
          _          -> RCharSet a
        RString  a -> mplus (usimple (i+1) (RString a) b) $ Just $ case b of
          RString  b
            | a==b         -> RString a
            | lenA <= lenB -> fn a (uchars a) (uchars b)
            | otherwise    -> fn b (uchars b) (uchars a)
            where
              lenA = ulength a
              lenB = ulength b
              fn a ax bx = case stripPrefix ax bx of
                Just post -> RSequence [RString a, rxString post]
                Nothing   -> RChoice [RString a, RString b]
          _ -> loop i b (RString a)
        RSequence ax       -> Just $ case ax of
          []   -> b
          a:ax -> case b of
            RSequence bx       -> rxSequence (seqzip i ax bx)
            RChoice   bx       -> mult i a ax bx
            b                  -> rxSequence (loop (i+1) b a : ax)
        RChoice   ax       -> Just $ case ax of
          [] -> b
          ax -> case b of
            RSequence bx -> case bx of
              []   -> RChoice ax
              b:bx -> mult i b bx ax
            RChoice   bx -> rxChoice (ax++bx)

----------------------------------------------------------------------------------------------------

-- | Used internally by 'Parser's 'Control.Monad.State.State' monad.
data ParseState
  = ParseState
    { parsedCharCount :: Word64
    , lineNumber      :: Word64
    , charColumn      :: Word
    , parseString     :: String
    , tokenStack      :: [Token]
    }

-- | 'Parser' instantiates 'Control.Monad.Monad', 'Control.Monad.MonadPlus', 'Data.Functor.Functor',
-- 'Control.Monad.State.Class.MonadState', and 'Control.Monad.Error.MonadError'. Use
-- 'Control.Monad.msum' to takes a list of parsers and try each one in turn, evaluating to the first
-- parser that succeeds. Use ordinary monadic bind (@>>=@ and @>>@) to parse sequences. Use
-- 'Control.Monad.mzero' to indicate non-matching, which will result in evaluating the next
-- alternative parsers in a list of 'Control.Monad.msum' parsers, if any. Use 'pfail' to halt
-- parsing with a descriptive error message. Use 'pcatch' to catch failed parsers, mark the error,
-- and continue parsing.
newtype Parser a = Parser{ parserPTransState :: PTrans Token (State ParseState) a }

type ParseValue a = PValue Token a

-- | Evaluate a 'Parser' with an input string. The evaluation yields a pair from the internal
-- 'Control.Monad.State.Lazy.State' monad: the 'Prelude.fst' value of the pair is the result of the
-- parse, or 'Data.Maybe.Nothing' if it failed, the 'Prelude.snd' value of the pair contains
-- information about where a parse failed.
runParser :: Parser a -> String -> (ParseValue a, String)
runParser parser str = (result, remainder) where
  init =
    ParseState
    { parsedCharCount = 0
    , lineNumber      = 1
    , charColumn      = 0
    , parseString     = str
    , tokenStack      = []
    }
  (result, st) = runState (runPTrans (parserPTransState parser)) init
  remainder = parseString st

instance Monad Parser where
  return a = Parser (return a)
  Parser ma >>= fma = Parser $ ma >>= parserPTransState . fma
  Parser ma >> Parser mb = Parser $ ma >> mb
  fail msg = do
    token <- getToken
    Parser (tokenFail token msg)

instance Functor Parser where
  fmap f (Parser ma) = Parser $ fmap f ma

-- | 'mzero' introduces backtracking, 'mplus' introduces a choice.
instance MonadPlus Parser where
  mzero = Parser mzero
  mplus (Parser a) (Parser b) = Parser $ mplus a b

-- | For the most inefficient but most correct form of backtracking, you can
-- 'Control.Monad.State.get' a parser state (which is an opaque type), then try a branch, then
-- restore the parser state with 'Control.Monad.State.put'. Refer to the 'choice' function for more
-- information about using the 'ParseState'.
instance MonadState ParseState Parser where
  get = Parser $ lift get
  put = Parser . lift . put
  state = Parser . lift . state

-- | 'throwError' is identical to 'Parser's instantiation of 'Control.Monad.fail'.
--
-- It is a good idea to evaluate 'clearTokenStack' before trying an operation which might throw an
-- error, after an error is caught, and after evaluating 'catchError'. Remember, it is convienient
-- to use 'ok' instead of 'Control.Monad.return' because 'ok' automatically calls
-- 'clearTokenStack'.
instance MonadError UStr Parser where
  throwError msg = getToken >>= \tok -> Parser (tokenThrowError tok msg)
  catchError (Parser parser) catcher = Parser (catchError parser (parserPTransState . catcher))

----------------------------------------------------------------------------------------------------

-- $Working_With_Tokens
-- The 'Parser' state has a place to save characters that have been parsed called the 'tokenStack'.
-- It is modeled on the idea of a highlighting pen. As you scan through text, you can place the pen
-- down, collecting characters to form a token. The 'token' function starts collecting. You can
-- call 'token' multiple times in a row, each time a new token is created, like lifting the pen
-- and then putting it back down at the same spot to create a visible break in the highlight. You
-- can retrieve a copy of all characters highlighted by the current token with 'getToken'.
--
-- Tokens are also used internally by the 'Parser' monad. The 'Control.Monad.fail' function will
-- cause the token stack to be cleared, and an error message is created using the cleared tokens.
--
-- The token stack can also provide slightly more efficient 'backtrack'ing, although there is still
-- a performance penalty.  You can 'backtrack' to the most recent 'token' break, or you can
-- 'backtrackAll' back to the very first 'token'. 

-- | 'Token's are created by 'Parser's like 'regex' or 'regexMany1'. You can combine tokens using
-- 'append', 'appendTokens' and 'parseAppend'. There is no restriction on the order in which you
-- combine tokens, but it is less confusing if you append tokens in the order in which they were
-- parsed.  You can then use the 'tokenChars' function to retrieve the characters stored in the
-- 'Token' to create data structures that can be returned by your 'Parser' function. You can also
-- "undo" a parse by passing a 'Token' to the 'backtrack' function, which pushes the 'Token's
-- characters back onto the head of the input string, however this is inefficient and should be
-- avoided.
data Token
  = Token
    { startingLine   :: Word64
    , startingChar   :: Word64
    , startingColumn :: Word
    , endingLine     :: Word64
    , endingChar     :: Word64
    , endingColumn   :: Word
    , tokenChars     :: String
    }
  deriving (Eq, Ord)

instance Show Token where
  show t = show (startingLine t) ++ ':' : show (startingColumn t) ++ ' ' : show (tokenChars t)

-- | Append two 'Token's together. You can append tokens in the order in which they were parsed and
-- pass them to 'backtrack' to "undo" a parse.
appendTokens :: Token -> Token -> Token
appendTokens token a =
  token
  { startingLine   = min (startingLine   token) (startingLine   a)
  , startingChar   = min (startingChar   token) (startingChar   a)
  , startingColumn = min (startingColumn token) (startingColumn a)
  , endingLine     = max (endingLine     token) (endingLine     a)
  , endingChar     = max (endingChar     token) (endingChar     a)
  , endingColumn   = max (endingColumn   token) (endingColumn   a)
  , tokenChars     = tokenChars token ++ tokenChars a
  } 

-- not for export --
new_token :: Parser Token
new_token = get >>= \st -> return $
  Token
  { startingLine = lineNumber st
  , startingChar = parsedCharCount st
  , startingColumn = charColumn st
  , endingLine = lineNumber st
  , endingChar = parsedCharCount st
  , endingColumn = charColumn st
  , tokenChars = ""
  }

-- not for export --
begin_token :: Parser ()
begin_token = new_token >>= \t -> modify $ \st -> st{tokenStack = t : tokenStack st}

-- not for export --
put_back :: Token -> [Token] -> Parser ()
put_back t tx = modify $ \st ->
  st{ parsedCharCount = startingChar t
    , lineNumber = startingLine t
    , charColumn = startingColumn t
    , parseString = tokenChars t ++ parseString st
    , tokenStack = tx
    }

-- not for export --
end_token :: Parser ()
end_token = get >>= \st -> case tokenStack st of
  []       -> return ()
  [t]      -> put (st{tokenStack = []})
  t1:t2:tx -> put (st{tokenStack = appendTokens t2 t1 : tx})

-- not for export --
token_stack :: Parser a -> ([Token] -> ParseState -> Parser a) -> Parser a
token_stack onEmpty onFull = get >>= \st -> case tokenStack st of
  [] -> onEmpty
  tx -> onFull tx st

-- | Run a given parser, collecting every character it matches. You can use 'getToken' to get a
-- 'Token' containing these characters. When the given parser has been evaluated, regardless of
-- whether it evaluates to a 'Backtrack' or 'OK', the token is deleted. If the given parser
-- evaluates to 'PFail', the token is used for error reporting.
token :: Parser a -> Parser a
token parser = begin_token >> mplus (parser >>= \a -> end_token >> return a) (end_token >> mzero)

-- | This will "undo" all parsing to the most recent 'token' call, taking the top 'Token' off of the
-- 'tokenStack'. Once 'backtrack' modifies the internal 'Control.Monad.State.State', it evaluates to
-- 'Control.Monad.mzero' which will trigger 'Backtrack'ing. If the 'tokenStack' is empty, the
-- internal 'Control.Monad.State.State' is not modified, but it still evaluates to
-- 'Control.Monad.mzero'.
backtrack :: Parser ig
backtrack = token_stack mzero $ \ (t:tx) _ -> put_back t tx >> mzero

-- | Get a copy of the 'Token' at the top of the 'tokenStack', which was created by the most recent
-- evaluation of the 'token' function. Returns an empty 'Token' at the current position in the
-- input string if the 'tokenStack' is empty.
getToken :: Parser Token
getToken = token_stack new_token (\ (t:tx) _ -> return t)

-- not for export --
concat_tokens :: [Token] -> Token
concat_tokens = foldl1 appendTokens . reverse

-- | Get a copy of the whole 'tokenStack', but with every 'Token' combined together into a single
-- 'Token'. Returns an empty 'Token' at the current position in the input string if the 'tokenStack'
-- is empty.
getCombinedTokens :: Parser Token
getCombinedTokens = token_stack new_token $ \tx _ -> return (concat_tokens tx)

----------------------------------------------------------------------------------------------------

-- $Parser_Combinators
-- Parsers create 'Token's by matching 'Regex's to the input string. Since 'Parser' is a monadic
-- type, you can build very complex parsers from these basic combinators using the comfortable
-- monadic notation.

-- | Returns 'Prelude.True' if the parser is at the end of the input string (end-of-file).
endOfInput :: Parser Bool
endOfInput = get >>= \st -> return (null (parseString st))

-- | Match the head of the input string to a 'Regex'. If it matches, evaluate to the resulting
-- 'Prelude.String' that matched, or fail if the 'Regex' does not match.
parseRegex :: (Regex -> String -> Maybe (String, String)) -> Regex -> Parser String
parseRegex matchFunc r = do
  match <- fmap (matchFunc r) (gets parseString)
  case match of
    Nothing              -> mzero
    Just (result, instr) -> do
      let ax = lines result
      modify $ \st -> 
        st{ parsedCharCount = parsedCharCount st + iLength result
          , lineNumber      = lineNumber st + iLength (filter ('\n'==) result)
          , charColumn      = case ax of
              []  -> charColumn st
              [a] -> charColumn st + iLength a
              ax  -> iLength (head (reverse ax))
          , parseString     = instr
          , tokenStack      = case tokenStack st of
              []   -> []
              t:tx -> (:tx) $
                t { endingLine   = lineNumber st
                  , endingChar   = parsedCharCount st
                  , endingColumn = charColumn st
                  , tokenChars   = tokenChars t ++ result
                  }
          }
      return result

-- | Parse a 'Regex' exactly one time, or else fail, shorthand for @'parseRegex' 'matchRegex'@.
regex :: Regex -> Parser String
regex = parseRegex matchRegex

-- | Often you may want to use a 'Text.Read.ReadS' parser that is already available for a data type
-- that is an instance of the 'Text.Read.Read' class, or in the case of number parsing functions
-- like 'Numeric.readHex', a function that has the same type as a 'Text.Read.ReadS'. This function
-- takes the 'Text.Read.ReadS' function and parses a string and returns the parsed result. *HOWEVER*
-- unlike the 'fromReadS' function, this function evaluates to 'Control.Monad.fail' if the whole
-- string is not used. This is useful in situations where your parseRegex scans too many characters in
-- order to catch mistakes. For example say, if your language allows for variables starting with
-- digits, like "1x, 2x" but you also can parse integer constants "1, 2". In this situation you
-- could write an equation like this:
-- @do  selected <- 'regexMany1' 'alnum' -- parse a string of numbers and letters@
-- @    Control.Monad.msum $@
-- @        [ fmap Left ('readsAll' 'Text.Read.reads' selected)@ -- backtracks if any letters were selected
-- @        , return (Right selected)
-- @        ]@
-- If 'readsAll' backtracks, the string is probably "1x" because the 'Prelude.Integer' parser will not
-- parse the whole string, rather than just "1" because "x" will be ignored. This will cause
-- 'readsAll' to 'Backtrack', and instead evaluate to the equation
-- @'Control.Monad.return' ('Data.Either.Right' selected)@ which returns the parsed string as a
-- string rather than an integer.
readsAll :: String -> ReadS a -> String -> Parser a
readsAll errmsg reads str = case reads str of
  (success, ""):_ -> return success
  _               -> fail errmsg

-- | Return some of the yet-unparsed characters without consuming them. Pass a positive integer
-- value of how many characters you want to look ahead.
lookAhead :: Int -> Parser String
lookAhead i = get >>= \st -> return (take i (parseString st))

-- | Parse a single character, shorthand for @'parseRegex' . 'rxChar'@.
char :: Char -> Parser Char
char = parseRegex matchRegex . rxChar >=> \ [c] -> return c

-- | Parse a 'Dao.String.UStr', shorthand for @'parseRegex' . 'rxUStr'@.
ustring :: UStr -> Parser UStr
ustring = parseRegex matchRegex . rxUStr >=> return . ustr

-- | Parse a 'Prelude.String', shorthand for @'parseRegex' . 'rxString'@.
string :: String -> Parser String
string = parseRegex matchRegex . rxString

-- | Parse a @'Dao.EnumSet.EnumSet' 'Data.Char.Char'@, shorthand for @'parseRegex' . 'rxCharSet'@.
charSet :: EnumSet Char -> Parser String
charSet = parseRegex matchRegex . rxCharSet

-- | Parse a @'Dao.EnumSet.EnumSet' 'Data.Char.Char'@ but invert the set.
notCharSet :: EnumSet Char -> Parser String
notCharSet = parseRegex matchRegex . rxCharSet . setInvert

-- | Parse a 'Regex' repeatedly, but failing if the lower-limit number of matches does not match,
-- and then succeding only an upper-limit number of times. The maximum number of times that a parseRegex
-- will be matched is @lo + hi@ times, where @lo@ is the lower limit and @hi@ is the upper limit.
repeatRegex :: Word -> Word -> Regex -> Parser String
repeatRegex lo hi reg = parseRegex (matchRepeat lo hi) reg

-- | Parse one or zero 'Regex's. Since this parser never fails, be careful when using it in
-- recursive parsers.
zeroOrOne :: Regex -> Parser String
zeroOrOne reg = repeatRegex 0 1 reg

-- | Parse a regular expression as many times as possible, possibly zero times, and returning what
-- was parsed. This parser never fails, so be careful when using it in recursive parsers.
regexMany :: Regex -> Parser String
regexMany reg = mplus (regexMany1 reg) (return "")

-- | Like 'regexMany' but fails if the parseRegex does not match at least one time.
regexMany1 :: Regex -> Parser String
regexMany1 reg = parseRegex (\reg str -> run str >>= loop) reg where
  run str = matchRepeat 1 maxBound reg str
  loop (a, str) =
    mplus (run str >>= \ (b, str) -> if null b then return (a, str) else loop (a++b, str)) $
      return (a, str)

-- | Match a parser a minimum number of of times.
atLeast :: Word -> Parser a -> Parser [a]
atLeast = wordReplicateM

-- | Match a parser as many times as possible, possibly zero times, but not more than an upper-limit
-- number of times. This parser never fails, so be careful when using it in recursive parsers.
atMost :: Word -> Parser a -> Parser [a]
atMost hi par = loop 0 [] where
  loop i ax = mplus (par >>= \a -> if i>=hi then return (a:ax) else loop (i+1) (ax++[a])) (return ax)

-- | Evaluate 'atLeast' and 'atMost' in turn with the same parser. The maximum number of times that
-- a parseRegex will be matched is @lo + hi@ times, where @lo@ is the lower limit and @hi@ is the upper
-- limit.
repeating :: Word -> Word -> Parser a -> Parser [a]
repeating lo hi par = do
  a <- atLeast lo par
  b <- atMost  hi par
  return (a++b)

-- | Evaluate a parser as many times as possible, does not fail, only returns what it could parse
-- (perhaps an empty string).
many :: Parser a -> Parser [a]
many par = loop [] where
  loop ax = mplus (par >>= \a -> loop (ax++[a])) (return ax)

-- | Evaluate a parser as many times as possible, but fails if it cannot match at least one time.
many1 :: Parser a -> Parser [a]
many1 par = par >>= \a -> many par >>= \ax -> return (a:ax)

----------------------------------------------------------------------------------------------------

-- | Parse some object of a type that is an instance of 'Text.Read.Read' class using the
-- 'Text.Read.readsPrec' function. *WARNING:* there is no way to retrieve the exact characters
-- parsed by the 'Text.Read.ReadS' parser, so the line and column numbers, and the character count,
-- is not updated, which will really throw off these numbers. Try to avoid using this function,
-- unless you do not care about reporting line/column numbers of where parsing failures occur. It is
-- better to parse a token @t@ first, then use @'readsAll' ('tokenChars' t)@ instead.
fromReadS :: Read a => String -> Int -> Parser a
fromReadS errmsg prec = do
  str <- gets parseString
  case readsPrec prec str of
    (a, str):_ -> modify (\st -> st{parseString = str}) >> return a
    []         -> fail errmsg

-- | *WARNING:* never ever use the 'choice' function. If you need to use this function, it means
-- your parser is very poorly designed. It is only here to educate programmers.
--
-- This function creates a copy of the entire parse state (including the input string), then tries
-- each choice in the list of 'Parser's provided. If the first 'Parser' 'Backtrack's, the state is
-- destroyed, and the copy of the state taken before the first 'Parser' was evaluated is restored.
-- Then the next 'Parser' in the list is tried, as if the first parser never happened. Since Haskell
-- uses lazy evaluation, the entire input string may not be copied when the state is, but then again
-- it might be, incurring a tremendous performance penalty.
--
-- Efficient choice is provided by 'Control.Monad.msum' and 'Control.Monad.mplus', because
-- 'Backtrack'ing does not require the entire state be copied.
--
-- The equation for this function is as follows:
-- @\parsers -> get >>= \copyOfState -> msum (map (\nextParser -> put copyOfState >> nextParser) parsers)@
choice :: [Parser a] -> Parser a
choice parsers = get >>= \copyOfState ->
  msum (map (\nextParser -> put copyOfState >> nextParser) parsers)

