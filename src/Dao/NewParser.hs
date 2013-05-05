-- "src/Dao/NewParser.hs"  a parser specifically designed for the
-- the Dao programming language.
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

----------------------------------------------------------------------------------------------------
-- Most useful functions

-- | The lexer breaks an arbitrary input string into lines, and breaks lines into 'Token's. 'Token's
-- like string literals and comments can take up several lines, so the 'Token' data type is defined
-- to be able to still work even though the input is broken into lines. The first parameter is an
-- integer value (actually a 'Data.Word.Word') for how many columns a tab takes to give more
-- accurate column counts.
lex :: Word -> String -> PValue L.Location [Line]
lex tablen input = fmap (cluster 1 []) (lexLoop 1 1 [] input) where
  lexLoop lineNum colNum toks str = case nextLex str of
    OK (tok, str) ->
      let (lns, cols) = countNLs 0 0 $ uchars $ tokToUStr tok
          newLineNum = lns + lineNum
          newColNum  = cols + (if lns>0 then 0 else colNum)
          toks' = toks ++ [(lineNum, colNum, tok)]
      in  if null str then OK toks' else lexLoop newLineNum newColNum toks' str
    PFail u v -> PFail u v
    Backtrack -> PFail (L.LineColumn (fromIntegral lineNum) colNum (fromIntegral lineNum) colNum) $
      ustr ("bad symbol"++(if null str then "<END>" else show (head str))++" in character stream")
  nextLex str = msum $ map ($str) $ -- the ordering of these lexers is important
    [getSpace, getComInln, getComEndl, getStrLit, getLetters, getNumbers, getPunct]
  cluster curLineNum toks tx = case tx of
    []                          ->
      if null toks then [] else [Line{lineLineNumber=curLineNum, lineTokens=toks}]
    (lineNum, colNum, tok) : tx ->
      if lineNum==curLineNum || null toks -- if we are on the same line, or if the line is empty
        then  cluster lineNum (toks++[(colNum, tok)]) tx -- then get the next token
        else  Line{lineLineNumber=curLineNum, lineTokens=toks} : cluster lineNum [(colNum, tok)] tx
  countNLs lns cols input = case break (=='\n') input of
    (""    , ""        ) -> (lns, cols)
    (_     , '\n':after) -> countNLs (lns+1) 0 after
    (before, after     ) -> (lns, cols + foldl (+) 0 (map charPrintWidth (before++after)))
  charPrintWidth c = case c of
    c | c=='\t'   -> tablen
    c | isPrint c -> 1
    c             -> 0

----------------------------------------------------------------------------------------------------
-- Important data types for lexical analysis.

data TokenType
  = Space
  | Letters
  | Numbers
  | Punct
  | StrLit  -- ^ a string literal is treated as a single token
  | ComEndl -- ^ a comment at the end of the line
  | ComInln -- ^ the start of a multi-line comment
  | Ignored -- ^ characters that can be ignored, like in multi-line comments.
  deriving (Eq, Ord, Enum, Show)

type Token = (TokenType, UStr)
tokToUStr = snd
tokType   = fst

class HasLineNumber   a where { lineNumber   :: a -> Word }
class HasColumnNumber a where { columnNumber :: a -> Word }

data TokenAt = TokenAt{tokenAtLineNumber::Word, tokenAtColumnNumber::Word, getToken::Token}
instance HasLineNumber   TokenAt where { lineNumber   = tokenAtLineNumber   }
instance HasColumnNumber TokenAt where { columnNumber = tokenAtColumnNumber }

-- | The punctuation marks used in the Dao language, sorted from longest to shortest strings.
daoPuncts :: [String]
daoPuncts = reverse $ nub $ sortBy len $ words $ concat $
  [ allArithOp2Strs, " ", allArithOp1Strs, " ", allUpdateOpStrs
  , " : ; " -- legal tokens that are not infix or suffix operators
  , " ( ) { } [ ] #{ }# " -- parenthetical tokens are also included here.
  ]
  where
    len a b = case compare (length a) (length b) of
      EQ -> compare a b
      GT -> GT
      LT -> LT

data Line
  = Line
    { lineLineNumber :: Word
    , lineTokens     :: [(Word, Token)] -- ^ a list of tokens, each with an associated column number.
    }

instance HasLineNumber Line where { lineNumber = lineLineNumber }

instance Show Line where
  show line = show (lineLineNumber line) ++ ": " ++ show (lineTokens line)

type Tokenizer = String -> PValue L.Location (Token, String)

-- returns 'Prelude.True' if the token might have newline or tab characters and thus require it be
-- scanned for updating the line or column counts more scrupulously.
mightHaveNLorTabs :: Token -> Bool
mightHaveNLorTabs tok = case tokType tok of
    Letters -> False
    Numbers -> False
    Punct   -> False
    _       -> True

----------------------------------------------------------------------------------------------------
-- The parser data type

data ParserErr
  = ParseErrWithToken { parserErrLoc :: L.Location , parseErrToken :: Token }
  | ParseErr          { parserErrLoc :: L.Location }

newtype Parser a = Parser { parserToPTrans :: PTrans (L.Location, Token) (State [Line]) a}
instance Functor   Parser where { fmap f (Parser a) = Parser (fmap f a) }
instance Monad     Parser where
  (Parser ma) >>= mfa = Parser (ma >>= parserToPTrans . mfa)
  return a            = Parser (return a)
instance MonadPlus Parser where
  mzero                       = Parser mzero
  mplus (Parser a) (Parser b) = Parser (mplus a b)
instance MonadState [Line] Parser where
  get = Parser (PTrans (fmap OK get))
  put = Parser . PTrans . fmap OK . put

----------------------------------------------------------------------------------------------------

-- | Return the next token in the state. If the boolean parameter is true, the current token will
-- also be removed from the state.
nextToken :: Bool -> Parser (Word, Word, Token)
nextToken doRemove = get >>= \lines -> case lines of
  []         -> mzero
  line:lines -> case lineTokens line of
    []                 -> put lines >> nextToken doRemove
    (colNum, tok):toks -> do
      if doRemove then put (line{lineTokens=toks}:lines) else return ()
      return (lineNumber line, colNum, tok)

-- | Return the next token in the state if it is of the type specified, removing it from the state.
token :: TokenType -> Parser UStr
token requestedType = do
  tok@(_, _, (currentTokenType, str)) <- nextToken False
  if currentTokenType==requestedType then nextToken True >> return str else mzero

-- | Push an arbitrary token into the state, but you really don't want to use this function. It is
-- used to implement backtracking by the 'withToken' function, so use 'withToken' instead.
pushToken :: (Word, Word, Token) -> Parser ()
pushToken (lineNum, colNum, tok) = modify $ \lines -> case lines of
  []         -> [Line{lineLineNumber=lineNum, lineTokens=[(colNum,tok)]}] :: [Line]
  line:lines -> (line{lineTokens = (colNum, tok) : lineTokens line} : lines) :: [Line]

-- | Single token look-ahead: takes the next token, removing it from the state, and uses it to
-- evaluate the given parser. If the backtracks, the token is replaced into the state. Failures are
-- not caught, make sure the parser you pass to this function makes use of
-- 'Control.Monad.Error.catchError' to catch failures if that is what you need it to do.
withToken :: ((Word, Word, Token) -> Parser a) -> Parser a
withToken parser = do
  token <- nextToken True
  mplus (parser token) (pushToken token >> mzero)

withTokenType :: TokenType -> (UStr -> Parser a) -> Parser a
withTokenType requestedType parser = withToken $ \ (_, _, (currentTokenType, str)) ->
  if currentTokenType==requestedType then parser str else mzero

-- | Return the current line and column of the current token without modifying the state in any way.
getCursor :: Parser (Word, Word)
getCursor = nextToken False >>= \ (a,b, _) -> return (a,b)

----------------------------------------------------------------------------------------------------
-- Functions that facilitate lexical analysis.

-- | A fundamental tokenizer using 'Data.List.stripPrefix' to check whether the given string is at the
-- very beginning of the input.
getString :: (UStr -> Token) -> String -> Tokenizer
getString constructor str input = maybeToBacktrack $
  stripPrefix str input >>= \rem -> return (constructor (ustr str), rem)

-- | The fundamental tokenizer: takes a predicate over characters, if one or more characters
-- matches, a token is constructed and it is paired with the remaining string and wrapped into a
-- 'Data.Maybe.Just' value. Otherwise 'Data.Maybe.Nothing' is returned. The 'Data.Maybe.Maybe' type
-- is used so you can combine fundamental tokenizers using 'Control.Monad.mplus'.
tokenize :: (UStr -> Token) -> (Char -> Bool) -> Tokenizer
tokenize constructor predicate input = case span predicate input of
  ("" , _   ) -> mzero
  (str, more) -> return (constructor (ustr str), more)

-- | A fundamental parser using 'Data.Char.isSpace' and evaluating to a 'Space' token.
getSpace :: Tokenizer
getSpace = tokenize ((,)Space) isSpace

-- | A fundamental tokenizer using 'Data.Char.isAlpha' and evaluating to a 'Letters' token.
getLetters :: Tokenizer
getLetters = tokenize ((,)Letters) isAlpha

-- | A fundamental tokenizer using 'Data.Char.isDigit' and evaluating to a 'Numbers' token.
getNumbers :: Tokenizer
getNumbers = tokenize ((,)Letters) isDigit

-- | Parse a Dao-language operator, which is alway a punctuation mark of some kind. The punctuation
-- marks are listed in the 'daoPuncts' constant in this module.
getPunct :: Tokenizer
getPunct input = msum (map (\daoPunct -> getString ((,)Punct) daoPunct input) daoPuncts)

-- once you have tokenized your initial quote character, this tokenizer lexes-out the rest of the
-- input string.
getInnerStrLit :: String -> Tokenizer
getInnerStrLit str input = case break (\c -> c=='"' || c=='\\') input of
  (got, ""        ) -> return ((StrLit, ustr (str++got)), "")
  (got, '"' :input) -> return ((StrLit, ustr (str++got++"\"")), input)
  (got, '\\':input) -> getInnerStrLit (str++got++"\\") input
  (got,      input) -> getInnerStrLit (str++got) input

-- | Tokenizes a string literal expression between two quote characters, ignoring quote characters
-- preceeded by a backslash, and ignoring newlines. If the literal expression runs past the end of
-- the input, this function evaluates to a @'Data.Maybe.Just' ('StrOpen' token, "")@.
getStrLit :: Tokenizer
getStrLit input = case input of
  '"':input -> getInnerStrLit "\"" input
  _         -> mzero

-- | Tokenize a comment starting with a @//@ symbol and runs to the first newline character it sees.
getComEndl :: Tokenizer
getComEndl input = case input of
  '/':'/':input -> loop "//" input where
    loop str input = case break (=='\n') input of
      (got, '\n':input) -> return ((ComEndl, ustr (str++got++"\n")), input)
      (got,      input) -> return ((ComEndl, ustr (str++got))      , input)
  _            -> mzero

-- once you have tokenized your initial open-comment characters, this tokenizer lexes-out the rest
-- of the comment.
getInnerComment :: String -> Tokenizer
getInnerComment str input = case break (=='*') input of
  (got, '*':'/':input) -> return ((ComInln, ustr (str++got++"*/")), input)
  (got,            "") -> return ((ComInln, ustr (str++got))      , ""   )
  (got,         input) ->
    let (astrs, more) = span (=='*') input in getInnerComment (str++got++astrs) more

getComInln :: Tokenizer
getComInln input = case input of
  '/':'*':input -> getInnerComment "/*" input
  _             -> mzero

