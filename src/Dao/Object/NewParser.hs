-- "src/Dao/Object/NewParser.hs" makes use of 'Dao.Regex' to parse
-- 'Dao.Object.Object's and 'Dao.Object.ObjectExpr'essions.
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

module Dao.Object.NewParser where

import           Dao.String
import           Dao.Object
import           Dao.EnumSet
import           Dao.Regex

import           Control.Monad

import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Complex
import           Numeric

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

readBinaryInt :: String -> [(Integer, String)]
readBinaryInt str = if null str then [] else [loop 0 str] where
  loop i cx = case cx of
    ""     -> (i, "")
    '0':cx -> loop (i*2) cx
    '1':cx -> loop (i*2 + 1) cx
    cx     -> (i, cx)

-- | Parses 'Prelude.Integer's, 'Prelude.Double's, 'Complex.Complex's, and 'Data.Time.DiffTime's,
-- and returns them as 'Dao.Object.Object's.
numericObj :: Parser (PValue Object)
numericObj = do
  sign <- zeroOrOne plusMinus
  dgt  <- regexMany1 digit
  let got = sign++dgt
      errmsg got = wrong got "invalid integer expression"
  case dgt of
    "0"     -> do
      base <- zeroOrOne alpha
      got  <- return (got++base)
      let alt take base = regexMany1 take >>= parsePoint base take got sign
      flip mplus (errmsg got) $ case base of
        "b" -> alt digit  2
        "x" -> alt xdigit 16
        ""  -> parsePoint 10 digit got sign ""
        _   -> errmsg got
    '0':dgt -> parsePoint 8  digit got sign dgt
    dgt     -> parsePoint 10 digit got sign dgt
  where
    plusMinus = rxUnion (rxChar '+') (rxChar '-')
    parsePoint base take got sign dgt = flip mplus (makeRational base got sign dgt "" "" "") $ do
      char '.'
      rdx <- regexMany take
      got <- return (got++'.':rdx)
      let done = makeRational base got sign dgt rdx "" ""
      if null rdx
        then wrong got "expecting digits after point"
        else if base==10 then mplus (parseExponent got sign dgt rdx) done else done
    parseExponent got sign dgt rdx = do
      e       <- regex (rxUnion (rxChar 'e') (rxChar 'E'))
      expSign <- zeroOrOne plusMinus
      expDgt  <- regexMany digit
      got     <- return (got++e++expSign++expDgt)
      if null expDgt
        then  wrong got ("expecting numerical exponent after \""++e++expSign++"\" symbol")
        else  if null (drop 5 expDgt) -- make sure the exponent is 5 digits or less.
                then  makeRational 10 got sign dgt rdx expSign expDgt
                else  wrong got "exponent is too large"
    makeRational base got sign dgt rdx expSign expDgt = do
      let b = toInteger base % 1
          r = do
            x   <- rationalFromString base b dgt
            y   <- rationalFromString base (recip b) rdx
            exp <- fmap (round . abs) (rationalFromString base b expDgt)
            let ibase  = if expSign=="-" then recip b else b
                result = (if sign=="-" then negate else id) ((x+y)*(ibase^^exp))
            return (round result % 1 == result, result)
      case r of
        Nothing -> wrong got ("incorrect digits used to form a base-"++show base++" number")
        Just (r_is_an_integer, r) -> do
          typ <- zeroOrOne alpha
          case typ of
            "U" -> ok $ OWord $ fromIntegral $ round r
            "I" -> ok $ OInt $ round r
            "L" -> ok $ OLong $ round r
            "R" -> ok $ ORatio r
            "F" -> ok $ OFloat (fromRational r)
            "f" -> ok $ OFloat (fromRational r)
            "i" -> ok $ OComplex (0 :+ fromRational r)
            "j" -> ok $ OComplex (0 :+ fromRational r)
            "s" -> ok $ ODiffTime $ fromRational r
            ""  ->
              if r_is_an_integer
                then
                  let i = round r
                  in  if fromIntegral (minBound::T_int) <= i && i <= fromIntegral (maxBound::T_int)
                        then  ok $ OInt $ fromIntegral i
                        else  ok $ OLong i
                else ok (ORatio r)
            typ -> wrong (got++typ) ("unknown numeric type "++show typ)

parseString :: Parser (PValue String)
parseString = do
  char '"'
  loop "\"" where
    stops = foldl1 setUnion $ map point "\"\\\n"
    loop zx = do
      str <- regexMany (rxCharSet (setInvert stops))
      let errmsg got = wrong got "string literal runs past end of input"
          got = zx++str
      flip mplus (errmsg got) $ do
        stop <- charSet stops
        got  <- return (got++stop)
        case stop of
          "\n" -> wrong got "string runs past end of line"
          "\\" -> mplus (regex rxTrue >>= \c -> loop (got++c)) $ do
            regexMany (rxUnion hspace (rxChar '\r')) >> regex (rxChar '\n')
            wrong got "cannot use '\\' token to continue string to next line"
          "\"" -> mplus (readsAll reads got >>= ok) (wrong got "invalid string constant")
          _    -> errmsg got

object :: Parser Object
object = do
  return ONull

