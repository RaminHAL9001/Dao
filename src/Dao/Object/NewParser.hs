-- "src/Dao/Object/NewParser.hs" makes use of "Dao.NewParser" to parse
-- parse 'Dao.Object.AST_Object' expressions.
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

module Dao.Object.NewParser where

import           Dao.String
import qualified Dao.Token as L
import           Dao.Object hiding (Tokenizer)
import           Dao.Object.AST
import           Dao.EnumSet
import           Dao.NewParser
import qualified Dao.Tree as T

import           Control.Monad
import           Control.Monad.Error

import           Data.List
import           Data.Char hiding (Space)
import           Data.Word
import           Data.Ratio
import           Data.Complex
import           Data.Time.Clock
import           Numeric

----------------------------------------------------------------------------------------------------

-- | This is the list of tokenizers used by 'lex' to break-up input string into parsable 'Token's.
daoTokenizers :: [Tokenizer]
daoTokenizers = 
  [ lexStringLiteral
  , lexCharLiteral
  , lexInlineC_Comment
  , lexEndlineC_Comment
  , lexSpace
  , lexKeyword
  , lexNumber
  , lexOperator $ concat $
      [allArithOp2Strs, " ", allArithOp1Strs, " ", allUpdateOpStrs , " , : ; "]
  , lexString "#{" >> makeToken Opener
  , lexString "}#" >> makeToken Closer
  , lexCharP (charSet "([{") >> makeToken Opener
  , lexCharP (charSet "}])") >> makeToken Closer
  ]

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

-- copied from the Dao.Parser module
rationalFromString :: Int -> Rational -> String -> Maybe Rational
rationalFromString maxValue base str =
  if b<1 then fmap (b*) (fol (reverse str)) else fol str where
    b = abs base
    maxVal = abs maxValue
    fol = foldl shiftAdd (return 0)
    shiftAdd result nextChar = do
      x <- result
      y <- convertChar nextChar
      if y>=maxVal then mzero else return (x*b + (toInteger y % 1))
    convertChar a = case a of
      a | isDigit a -> return (ord a - ord '0')
      a | isLower a -> return (ord a - ord 'a' + 10)
      a | isUpper a -> return (ord a - ord 'A' + 10)
      _             -> mzero

parseNumber :: Parser Object
parseNumber = do
  num <- fmap uchars (tokenType Number)

-- | Parses a numeric object without the leading positive or negative sign. The sign must be part of
-- an equation expression. If the negative sign were parsed in this parser, an expression like @1-1@
-- might parse to 
old_parseNumber :: Parser Object
old_parseNumber = marker $ msum [parseDot 10 nil, hexadecimal, octOrBin, decimal] where
  dot = operator "."
  octOrBin = do
    int <- tokenP Digits (\str -> head str == '0')
    let uint = uchars int
    msum $
      [ do guard (uint=="0") -- octal or binary integer
           typ  <- fmap (head . uchars) (tokenP Alphabetic (\tok -> tok=="o" || tok=="b"))
           int  <- mplus (token Digits) (return nil)
           -- ^ ignore the previous "int" value it was the 0 before the "o" or "b" character
           frac <- mplus (dot >> token Digits) (return nil)
           endWith (if typ=='o' then 8 else 2) int frac False nil
      , guard (uint=="0") >> (parseDot 10 int) -- 0.XXX is a base-10 number
      , do guard (uint=="00") -- 00.XXX is a base-8 number
           frac <- dot >> token Digits
           endWith 8 int frac False nil
      , endWith 8 int nil False nil
      ]
  hexadecimal = do
    tokenStr Digits "0"
    tokenP Alphabetic (\str -> str=="x" || str=="X")
    int  <- token HexDigits
    frac <- mplus (dot >> token HexDigits) (return nil)
    endWith 16 int frac False nil
  decimal = do
    int <- token Digits
    msum [parseDot 10 int, parseExp 10 int nil, endWith 10 int nil False nil]
  parseDot base int = do
    frac <- dot >> token Digits
    mplus (parseExp base int frac) (endWith base int frac False nil)
  parseExp base int frac = do
    tok <- tokenP Alphabetic (\tok -> tok=="E" || tok=="e")
    expect ("an integer exponent after the "++show tok) $ do
      hasMinusSign <- fmap ((=="-") . uchars) $
        mplus (tokenP Punct (\tok -> tok=="-" || tok=="+")) (return nil)
      exp <- token Digits
      endWith 10 int frac hasMinusSign exp
  endWith base int frac hasMinusSign exp = do
    int  <- return (uchars int)
    frac <- return (uchars frac)
    exp  <- return (uchars exp)
    let b = toInteger base % 1
        rational = do
          x   <- rationalFromString base b int
          y   <- rationalFromString base (recip b) frac
          exp <- fmap (round . abs) (rationalFromString base b exp)
          let ibase  = if hasMinusSign then recip b else b
              result = (x+y)*(ibase^^exp)
          return (round result % 1 == result, result)
    (r_is_an_integer, r) <- case rational of
      Nothing -> fail ("incorrect digits used to form a base-"++show base++" number")
      Just  r -> return r
    typ <- fmap uchars (mplus (token Alphabetic) (return nil))
    case typ of
      "U" -> return $ OWord (fromIntegral (round r))
      "I" -> return $ OInt  (round r)
      "L" -> return $ OLong (round r)
      "R" -> return $ ORatio r
      "F" -> return $ OFloat (fromRational r)
      "f" -> return $ OFloat (fromRational r)
      "i" -> return $ OComplex (0 :+ fromRational r)
      "j" -> return $ OComplex (0 :+ fromRational r)
      "s" -> return $ ODiffTime (fromRational r)
      ""  ->
        if r_is_an_integer && null frac
          then
            let i = round r
            in  if fromIntegral (minBound::T_int) <= i && i <= fromIntegral (maxBound::T_int)
                  then  return $ OInt $ fromIntegral i
                  else  return $ OLong i
          else return (ORatio r)
      typ -> fail ("unknown numeric type "++show typ)

parseDiffTime :: Parser T_diffTime
parseDiffTime = marker $ do
  let colon = operator ":"
  hours   <- token Digits
  minutes <- colon >> token Digits
  seconds <- colon >> token Digits
  if ulength minutes > 2 || ulength seconds > 2
    then fail ("invalid time literal expression")
    else do
      milisec <- mplus (fmap uchars (operator "." >> token Digits)) (return "")
      hours   <- return (uchars hours)
      minutes <- return (uchars minutes)
      seconds <- return (uchars seconds)
      let rint = read :: String -> Integer
          rr s = rint s % 1
      return $ fromRational $ toRational $
        60*60 * rr hours + 60 * rr minutes + rr seconds +
          if null milisec then 0 else rr milisec / 10 ^ length milisec

parseDate :: Parser T_time
parseDate = marker $ do
  let dash = operator "-"
  year  <- fmap uchars (token Digits)
  month <- fmap uchars (dash >> token Digits)
  day   <- fmap uchars (dash >> token Digits)
  let commaSpace = -- a comma followed by an optional space OR no comma but a required space
        mplus (operator "," >> skipSpaces) (void $ token Space)
  diffTime <- mplus (commaSpace >> parseDiffTime) (return (fromRational 0))
  zone <- mplus (commaSpace >> fmap uchars (token Alphabetic)) (return "")
  return (addUTCTime diffTime (read (year ++ '-':month ++ '-':day ++ " 00:00:00" ++ zone)))

