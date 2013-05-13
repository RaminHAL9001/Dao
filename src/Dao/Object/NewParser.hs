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

maxYears :: Integer
maxYears = 9999

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
  , lexOperator daoOperators
  , lexString "#{" >> makeToken Opener
  , lexString "}#" >> makeToken Closer
  , lexCharP (charSet "([{") >> makeToken Opener
  , lexCharP (charSet "}])") >> makeToken Closer
  ]

daoOperators :: String
daoOperators = concat [allArithOp2Strs, " ", allArithOp1Strs, " ", allUpdateOpStrs , " , : ; "]

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

-- | Parses a numeric object without the leading positive or negative sign. The sign must be part of
-- an equation expression. If the negative sign were parsed in this parser, an expression like @1-1@
-- might parse to 
parseNumber :: Parser Object
parseNumber = do
  let getTyp = optional "" (fmap uchars (tokenType Keyword))
  mplus (fmap uchars (tokenType Digits) >>= \num -> getTyp >>= numberFromStrs 10 num "" "") $ do
    num <- fmap uchars (tokenTypes [Number, NumberExp])
    typ <- getTyp
    -- ^ 'typ' is the optional terminating type modifier, for example numbers that end with "f" to
    -- indicate the floating type or "L" to indicate the long-int type
    let mk base int frac exp = numberFromStrs base int frac exp typ
        getDot = break (=='.')
        getExp = break (\c -> c=='e' || c=='E')
        altBaseDot base num = case getDot num of
          (num, "" ) -> mk base num ""  ""
          (num, dec) -> mk base num dec ""
    case num of
      '0':x:num | x=='x' || x=='X' -> altBaseDot 16 num
      '0':b:num | b=='b' || b=='B' -> altBaseDot 2  num
      '0':  num                    -> altBaseDot 8  num
      num -> case getDot num of
        (num, ""   ) -> case getExp num of
          (num, ""   ) -> mk 10 num ""  ""
          (num, _:exp) -> mk 10 num ""  exp -- _:exp skips the 'E' or 'e'
        (num, _:dec) -> case getExp dec of  -- _:dec skips the decimal point
          (dec, ""   ) -> mk 10 num dec ""
          (dec, _:exp) -> mk 10 num dec exp -- _:exp skips the 'E' or 'e'

-- copied from the Dao.Parser module
numberFromStrs :: Int -> String -> String -> String -> String -> Parser Object
numberFromStrs base int frac plusMinusExp typ = do
  let (exp, hasMinusSign) = case plusMinusExp of
        ""             -> ("" , False)
        p:exp | p=='+' -> (exp, False)
              | p=='-' -> (exp, True )
        exp            -> (exp, False)
      b = toInteger base % 1
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

-- | Parses optional parenthesis around an inner parser, but does not parse white spaces or
-- comments before or after the parser. Provide a message which will be used to build the error
-- message @"expecting close parenthesis after "++errMsg++" expression."@.
parseOptionalParens :: String -> Parser a -> Parser a
parseOptionalParens errMsg parser = flip mplus parser $ do
  token (==Opener) (=="(")
  a <- parser
  expect ("close parenthesis after "++errMsg++" expression") (token (==Closer) (==")") >> return a)

-- | Parses a different form of 'ODiffTime', with days, hours, minutes, and seconds separated by
-- colons. This parser does not care about context, colons are used as @hour:minute:second@ separator
-- tokens, regardless of their meaning elsewhere. Spaces are not allowed between
-- @hour:minute:second@ tokens.
parseDiffTime :: Parser T_diffTime
parseDiffTime = marker $ withToken (==Digits) $ \d -> expect "diff-time constant expression" $ do
  dx <- loop [d] 3
  case map uchars dx of
    [days, hours, minutes, seconds] -> mk days hours minutes seconds
    [      hours, minutes, seconds] -> mk ""   hours minutes seconds
    [             minutes, seconds] -> mk ""   ""    minutes seconds
  where
    mk          = diffTimeFromStrs
    colon       = operator ":"
    nocolonfail = fail "incorrect time expression, no digits after colon"
    loop got i  = case i of
      i | i>0       -> do
        d <- nextToken True
        let td = tokType d
        if td/=Number && td/=Digits
          then  fail "expecting digits for time value"
          else  case uchars (tokToUStr d) of
                  '0':xb:_ | isAlpha xb -> fail "time values must be expressed in base-10"
                  _                     -> do
                    colon
                    let got' = got++[tokToUStr d]
                    flip mplus nocolonfail $
                      if td==Number then return got' else loop got' (i-1)
        | otherwise -> return got

-- | Compute diff times from strings representing days, hours, minutes, and seconds. The seconds
-- value may have a decimal point.
diffTimeFromStrs :: String -> String -> String -> String -> Parser T_diffTime
diffTimeFromStrs days hours minutes seconds = do
  days    <- check "days"    (maxYears*365) days
  hours   <- check "hours"   24             hours
  minutes <- check "minutes" 60             minutes
  let sec = check "seconds" 60
  seconds <- case break (=='.') seconds of
    (_      , "."            ) ->
      fail "no digits after decimal point in seconds-value of time expression"
    (seconds, ""             ) -> sec seconds
    (seconds, '.':miliseconds) -> do
      seconds <- sec seconds
      return (seconds + rint miliseconds % (10 ^ length miliseconds))
  return $ fromRational (60*60*24*days + 60*60*hours + 60*minutes + seconds)
  where
    rint str = if null str then 0 else (read str) :: Integer
    integerToRational s = s % 1 :: Rational
    check :: String -> Integer -> String -> Parser Rational
    check typ maxVal s =
      let i = rint s
          zero = return (0%1)
          ok = return (integerToRational i)
          err = fail $ concat ["time value expression with ", s, " ", typ, " is invalid"]
      in  if null s then zero else if i<maxVal then ok else err

-- | Parses a different form of 'OTime', with the standard dash-separated @YYYY-MM-DD@ format, along
-- with an optional time value as parsed by 'parseDiffTime'. This parser does not care about
-- context, dashes are used as @year-month-date@ separator tokens, regardless of their meaning
-- elsewhere. Spaces are not allowed between @year-month-date@ tokens.
parseDate :: Parser T_time
parseDate = marker $ withToken (==Digits) $ \yyyy -> expect "absolute date constant expression" $ do
  let dash = operator "-"
      digits = fmap uchars (tokenType Digits)
      year = uchars yyyy
  month <- dash >> digits
  day   <- dash >> digits
  let commaSpace = -- a comma followed by an optional space OR no comma but a required space
        mplus (operator "," >> skipSpaces) (void $ tokenType Space)
  diffTime <- mplus (commaSpace >> parseDiffTime) (return (fromRational 0))
  zone <- msum $
    [  do commaSpace
          msum $
            [ mplus (fmap uchars (tokenType Keyword)) (return "")
            , do  plus <- optional "" (fmap uchars (mplus (operator "+") (operator "-")))
                  zone <- fmap uchars (tokenType Digits)
                  let withColon h1 h2 = expect "valid time-zone offset" $ msum $
                        [ do  operator ":"
                              mm <- tokenType Digits
                              case uchars mm of
                                [m1,m2] | (read [m1,m2] :: Int) < 60 -> return (plus++[h1,h2,m1,m2])
                                _                                    -> mzero
                        , if (read [h1,h2] :: Int) > 12 then return [h1,h2] else mzero
                        ]
                      withoutColon h1 h2 m1 m2 = do
                        if (read [h1,h2] :: Int) < 12 && (read [m1,m2] :: Int) < 60
                          then  return (plus++[h1,h2,m1,m2])
                          else  fail ("timezone offset value "++zone++[h1,h2,m1,m2]++" is out-of-bounds")
                  case zone of
                    [h1,h2,m1,m2] -> withoutColon h1  h2 m1 m2
                    [   h2,m1,m2] -> withoutColon '0' h2 m1 m2
                    [h1,h2      ] -> withColon h1  h2
                    [   h2      ] -> withColon '0' h2
            ]
    , return ""
    ]
  return (addUTCTime diffTime (read (year ++ '-':month ++ '-':day ++ " 00:00:00" ++ zone)))

----------------------------------------------------------------------------------------------------

daoCFGrammar :: CFGrammar a
daoCFGrammar =
  GenCFGrammar
  { columnWidthOfTab = 4
  , tokenizers       = daoTokenizers
  , mainParser       = error "daoCFGrammar mainParser is not defined"
  }

testDaoGrammar :: Show a => Parser a -> String -> IO ()
testDaoGrammar parser input = print (parse (daoCFGrammar{mainParser=parser}) input)

