-- "src/Dao/Parser.hs"  a simple, terribly inefficient parser that is
-- very easy to change and add new language constructs to.
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


module Dao.Parser where

import           Dao.Types
import           Dao.Combination
import           Dao.Object
import           Dao.Object.Show -- for printing parse errors

import           Control.Monad
import           Control.Monad.State.Class

import           Data.List
import           Data.Word
import           Data.Array.IArray

import           Text.Read (readsPrec)

type RowNumber = Word64
type ColNumber = Word
type Location  = (RowNumber, ColNumber)

data ParserState
  = ParserState
    { rowNumber   :: RowNumber -- ^ how many @'\n'@ characters have traversed.
    , colNumber   :: ColNumber -- ^ how many characters have traversed.
    , inputString :: String
    }
    deriving (Eq, Ord, Show)

parserState = ParserState{ rowNumber = 1, colNumber = 0, inputString = "" }

type Parser a = Combination ParserState a

parser_to_ReadS :: Parser a -> String -> [(a, String)]
parser_to_ReadS fn str = concatMap rs (runCombination fn init) where
  init = parserState{ inputString = str }
  rs (Right a, st) = [(a, inputString st)]
  rs _             = []

-- not exported
new_line = \st -> st{ colNumber = 0, rowNumber = 1+rowNumber st }

getLocation :: Parser Location
getLocation = get >>= \st -> return (rowNumber st, colNumber st)

no_more_input = "no more input"
no_match = "no match"

-- | Label a parse. If a parse failure occurs, the label will discribe where and why the parse
-- failed.
labelParse :: String -> Parser a -> Parser a
labelParse msg fn = get >>= \st -> flip failMsg fn $ OArray $ listArray (0, 3) $
  [ OString (ustr msg)
  , OWord (rowNumber st)
  , OWord (fromIntegral (colNumber st))
  , OString (ustr (takeWhile (/='\n') (inputString st)))
  ]

-- | The 'labelParse' function produces reports on parse failures, this function can convert these
-- reports into readable strings. Pass a string filename, or some relevant information for reporting
-- what produced the string being parsed that cause the parse failure. Then pass the
-- 'Dao.Types.Object' that was stored in the 'Data.Either.Left' value evaluated from the
-- 'Dao.Combination.runCombination' function.
printParseError :: String -> Object -> String
printParseError source obj = loop 0 obj where
  loop idnc obj = case obj of
    OArray arr           -> case arr!0 of
      OString label      -> case arr!1 of
        OWord line       -> case arr!2 of
          OWord column   -> case arr!3 of
            OString text -> source++':':show line
              ++',':show column++' ':uchars label++"\n\t"++uchars text
            _ -> showObj idnc obj
          _ -> showObj idnc obj
        _ -> showObj idnc obj
      _ -> showObj idnc obj
    OPair (obj, ox) -> loop idnc obj ++ case ox of
      OList ox -> concatMap (('\n':) . loop (idnc+1)) ox
      _ -> showObj idnc ox
    _ -> showObj idnc obj

-- | Parse any string of characters.
string :: String -> Parser String
string str = do
  st <- get
  let loop row col ax bx =
        case (ax, bx) of
          (ax, []) -> do
            modify (\st -> st{ rowNumber = row, colNumber = col, inputString = ax })
            return str
          (a:ax, b:bx) | a==b -> if a=='\n' then loop (row+1) 0 ax bx else loop row (col+1) ax bx
          _ -> fail str
  loop (rowNumber st) (colNumber st) (inputString st) str

ifEOF :: Parser a -> (String -> Parser a) -> Parser a
ifEOF yes no = fmap inputString get >>= \ax -> if null ax then yes else no ax

look :: Parser String
look = fmap inputString get

next :: Parser Char
next = ifEOF (fail no_more_input) $ \ax -> do
  let c = head ax
  modify (\st -> (if c=='\n' then new_line else id) (st{inputString = tail ax}))
  return c

eof :: Parser ()
eof = ifEOF (return ()) (\_ -> fail "not end of input")

satisfy :: (Char -> Bool) -> Parser Char
satisfy c = next >>= \a -> if c a then return a else fail no_match

char :: Char -> Parser Char
char b = next >>= \a -> if a==b then return b else fail (show b)

withSplit :: (String -> (String, String)) -> Parser String
withSplit splitter = do
  (bx, ax) <- fmap (splitter . inputString) get
  let rem st     = st{ inputString = ax }
      count st b = if b=='\n' then new_line st else st{ colNumber = 1+colNumber st }
      nl st      = foldl count st bx
  modify (rem . nl)
  return bx

munch :: (Char -> Bool) -> Parser String
munch c = withSplit (span c)

munch1 :: (Char -> Bool) -> Parser String
munch1 c = next >>= \a -> if c a then fmap (a:) (munch c) else fail no_match

reusedParser :: Read a => String -> Parser a
reusedParser msg = do
  str <- fmap inputString get
  case readsPrec 0 str of
    [] -> fail msg
    ax -> combination (\st -> map (\ (a, rem) -> (Right a, st{ inputString = rem })) ax)

-- | The first parameter to this function is a parser determines takes the next value token, like a
-- number or variable in a math equation. As long as this parser succeeds at least once, the entire
-- 'precInfix' parser will also succeed, regardless of whether or not it is followed by an operator
-- symbol. The second parameter to this function is a list of infix operator symbol parsers, every
-- parser is tried in turn, the first successful parse is used. Each parser in the list must parse
-- an operator symbol and return the operator prescedence and an operating function. The operating
-- function can be a constructor to a recursive datatype or a function that immediately computes the
-- value of the two symbols applied to the operator. The constructed or computed value is returned.
-- A higher prescedence means it binds more tightly. For example (+) may have a prescedence of 1 and
-- (*) may have a prescedence of 2, so the equation @1+2*3@ is equal to @1+(2*3)@ and NOT @(1+2)*3@.
-- Higher prescedence values bind more tightly, so the meaning of the prescedence value is defined
-- as "an operator which takes prescedence over others depending on how large it's prescedence value
-- is". Prescedence is completely relative, so you can define your lowest prescedence as
-- @(maxBound::Int) - 1@ and your highest prescedence as @(maxBound::Int)@ if you wish.
precInfix :: Parser a -> [Parser (Int, a -> a -> a)] -> Parser a
precInfix value operator = fmap init (sepByEither1 value (first operator)) where
  init tokx = case tokx of
    [Right x] -> x
    Right x : Left (p, op) : tokx -> loop [] x p op tokx
  loop stack x p op tokx = case tokx of
    [Right y] ->
      let (stack', x') = unstack stack x p
      in  foldl (\x (_, y, op) -> op y x) (op x' y) stack'
    Right y : Left (p', op') : tokx -> case compare p p' of
      GT -> let (stack', x') = unstack stack x p in loop stack' (op x' y) p' op' tokx
      EQ -> loop stack (op x y) p' op' tokx
      LT -> loop ((p, x, op):stack) y p' op' tokx
  unstack stack x lo = case stack of
    [] -> ([], x)
    (p, y, op):stack' -> if p>lo then unstack stack' (op y x) lo else (stack, x)

