-- "src/Dao/MatchString.hs"  Construct a tokenizer from
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

module Dao.Regex
  ( -- * The 'Regex' Type
    Regex, matchRegex, matchRepeat
    -- * Primitive 'Regex's
    -- $Primitive_regexs
  , rxNull, rxEmpty, rxTrue, rxFalse, rxSequence, rxChoice
  , rxChar, rxString, rxUStr, rxCharSet, rxUnion
    -- * Essential 'Regex's
    -- $Essential_regexs
  , space, hspace, upper, lower, alpha, digit
  , alnum, alnum_, xdigit, spaceCtrl, ctrl, punct, printable, ascii
    -- * 'Parser' Combinators
    -- $Parser_combinators
  , Parser, runParser, label, pfail, parseRegex, regex, fromReadS, readsAll
  , char, string, ustring, charSet, notCharSet, repeating
  , repeatRegex, zeroOrOne, regexMany, regexMany1, many, many1
  , -- * Handling Undefined Values
    PValue(OK, Wrong), ok, wrong
    -- * Miscelaneous
  , spanAtWord
  )
  where

import           Dao.String
import           Dao.EnumSet
import qualified Dao.Tree as T

import           Control.Monad
import           Control.Monad.State

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

-- $Essential_regexs
-- These 'Regex's are not primitive, but they are essential to most applications.

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
ctrl :: Regex
ctrl = RCharSet $ setDelete (rxToEnumSet ctrl) (rxToEnumSet space)

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

-- not for export
data ParseState
  = ParseState
    { parsedCharCount :: Word64
    , lineNumber      :: Word64
    , charColumn      :: Word
    , parseInfoStack  :: [ParseInfo]
    , parseString     :: String
    }

-- | Error messages are reported by way of this data structure.
data ParseInfo
  = ParseInfo
    { onLine   :: Word64
    , onColumn :: Word
    , message :: UStr
    }

-- | Evaluate a 'Parser' with an input string. The evaluation yields a pair from the internal
-- 'Control.Monad.State.Lazy.State' monad: the 'Prelude.fst' value of the pair is the result of the
-- parse, or 'Data.Maybe.Nothing' if it failed, the 'Prelude.snd' value of the pair contains
-- information about where a parse failed.
runParser :: Parser a -> String -> (Maybe a, [ParseInfo])
runParser parser str = (result, info) where
  init =
    ParseState
    { parsedCharCount = 0
    , lineNumber      = 1
    , charColumn      = 0
    , parseInfoStack  = []
    , parseString     = str
    }
  (result, st) = runState (parserStateMonad parser) init
  info = parseInfoStack st

-- | Instantiates 'Control.Monad.Monad', 'Control.Monad.MonadPlus', 'Data.Functor.Functor', and
-- 'Control.Monad.State.Class.MonadState'. Use 'Control.Monad.msum' to takes a list of parsers and
-- try each one in turn, evaluating to the first parser that succeeds. Use ordinary monadic bind
-- (@>>=@ and @>>@) to parse sequences.
newtype Parser a = Parser{ parserStateMonad :: State ParseState (Maybe a) }

instance Monad Parser where
  return a = Parser (return (Just a))
  Parser ma >>= fma = Parser $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      Just a  -> parserStateMonad (fma a)
  Parser ma >> Parser mb = Parser $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      Just _  -> mb
  fail msg = label_ msg >> pfail

instance Functor Parser where
  fmap f (Parser ma) = Parser (fmap (fmap f) ma)

instance MonadPlus Parser where
  mzero = Parser (return Nothing)
  mplus (Parser a) (Parser b) = Parser $ do
    result <- a
    case result of
      Nothing -> b
      Just a  -> return (Just a)

instance MonadState ParseState Parser where
  get = Parser (get >>= \st -> return (Just st))
  put st = Parser (put st >> return (Just ()))
  state f = Parser (state f >>= \a -> return (Just a))

-- | Evaluates to a 'Parser' that does not match the head of the input string. The return type is
-- ignored.
pfail :: Parser ig
pfail = Parser (return Nothing)

-- not for export
-- pushes information to the 'parseInfoStack'
label_ :: String -> Parser ()
label_ msg = do
  line <- gets lineNumber
  col  <- gets charColumn
  let info = ParseInfo{onLine = line, onColumn = col, message = ustr msg}
  modify (\st -> st{parseInfoStack = info : parseInfoStack st})

-- | Label a 'Parser' so that if it fails, the label for this parser can be reported in the
-- 'ParseInfo'.
label :: String -> Parser a -> Parser a
label msg parse = do
  label_ msg
  result <- parse
  modify (\st -> st{parseInfoStack = tail (parseInfoStack st)})
  return result

-- | Match the head of the input string to a 'Regex'. If it matches, evaluate to the resulting
-- 'Prelude.String' that matched, or fail if the 'Regex' does not match.
parseRegex :: (Regex -> String -> Maybe (String, String)) -> Regex -> Parser String
parseRegex matchFunc r = do
  match <- fmap (matchFunc r) (gets parseString)
  case match of
    Nothing              -> pfail
    Just (result, instr) -> do
      let ax = lines result
      modify $ \st ->
        st{ parseString = instr
          , parsedCharCount = parsedCharCount st + iLength result
          , lineNumber = lineNumber st + iLength ax
          , charColumn = case ax of
              []  -> charColumn st
              [a] -> charColumn st + iLength a
              ax  -> iLength (head (reverse ax))
          }
      return result

-- | Parse a 'Regex' exactly one time, or else fail, shorthand for @'parseRegex' 'matchRegex'@.
regex :: Regex -> Parser String
regex = parseRegex matchRegex

-- | Parse some object of a type that is an instance of 'Text.Read.Read' class using the
-- 'Text.Read.readsPrec' function.
fromReadS :: Read a => Int -> Parser a
fromReadS prec = do
  str <- gets parseString
  case readsPrec prec str of
    (a, str):_ -> modify (\st -> st{parseString = str}) >> return a
    []         -> pfail

-- | Often you may want to use a 'Text.Read.ReadS' parser that is already available for a data type
-- that is an instance of the 'Text.Read.Read' class, or in the case of number parsing functions
-- like 'Numeric.readHex', a function that has the same type as a 'Text.Read.ReadS'. This function
-- takes the 'Text.Read.ReadS' function and parses a string and returns the parsed result. *HOWEVER*
-- unlike the 'fromReadS' function, this function evaluates to 'Control.Monad.fail' if the whole
-- string is not used. This is useful in situations where your parseRegex scans too many characters in
-- order to catch mistakes. For example say, if your language allows for variables starting with
-- digits, like "1x, 2x" but you also can parse integer constants "1, 2". In this situation you
-- could write an equation like this:
-- @do  selected <- 'many1' 'alnum'@
-- @    Control.Monad.msum $@
-- @        [ fmap Left ('readsAll' 'Text.Read.reads' selected)@
-- @        , return (Right selected)
-- @        ]@
-- If 'readsAll' fails, the string is probably "1x" because the 'Prelude.Integer' parser will not
-- parse the whole string, rather than just "1" because "x" will be ignored. This will cause
-- 'readsAll' to fail, and instead evaluate to the equation
-- @'Control.Monad.return' ('Data.Either.Right' selected)@ which returns the parsed string as a
-- string rather than an integer.
readsAll :: ReadS a -> String -> Parser a
readsAll reads str = case reads str of
  (success, ""):_ -> return success
  _               -> pfail

-- | Parse a single character, shorthand for @'parseRegex' . 'rxChar'@.
char :: Char -> Parser Char
char = parseRegex matchRegex . rxChar >=> \ [c] -> return c

-- | Parse a 'Dao.String.UStr', shorthand for @'parseRegex' . 'rxUStr'@.
ustring :: UStr -> Parser String
ustring = parseRegex matchRegex . rxUStr

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

-- | A "parsed value" or "predicate value" data type allows a parser to fail without causing
-- backtracking. How it works is simple: it allows a parse to succeede but the string it parsed to
-- be "undefined" in a descriptive way using the 'Wrong' constructor. Values of this data type can
-- be 'Control.Monad.return'ed normally in a parser but returning an error message instead of a
-- value, it only returns the string that caused the failure and a decription of why. Parsing can
-- then continue normally, allowing for more errors to be caught.
data PValue a
  = OK a
  | Wrong { failedToken :: UStr, failedBecause :: UStr }
  deriving (Eq, Ord, Show)

wrong :: String -> String -> Parser (PValue ig)
wrong tok msg = return (Wrong{failedToken = ustr tok, failedBecause = ustr msg})

ok :: a -> Parser (PValue a)
ok = return . OK

