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
rationalFromString maxVal base ax = foldl fn (return $ if base<1 then 1 else 0) ax where
  fn a b = do
    let c = conv b
    c <- if c>=maxVal then Nothing else Just (fromIntegral c)
    a >>= \a -> return (a*base + c)
  conv a = case a of
    a | isDigit a -> ord a - ord '0'
    a | isLower a -> ord a - ord 'a' + 10
    a | isUpper a -> ord a - ord 'A' + 10

readBinaryInt :: String -> [(Integer, String)]
readBinaryInt str = if null str then [] else [loop 0 str] where
  loop i cx = case cx of
    ""     -> (i, "")
    '0':cx -> loop (i*2) cx
    '1':cx -> loop (i*2 + 1) cx
    cx     -> (i, cx)

nonDecimalInteger :: Parser (PValue Integer)
nonDecimalInteger = do
  sign <- zeroOrOne (rxChar '-')
  char '0'
  base <- regex $ foldl1 rxUnion [rxChar 'x', rxChar 'b', digit]
  let done x = if null sign then ok x else ok (0-x)
  case base of
    "x"  -> do
      dx <- regexMany1 xdigit
      mplus (readsAll readHex dx >>= ok) (wrong dx "bad hexadecimal integer constant")
    base -> do
      dx <- regexMany1 digit
      let bdx = base++dx
      case base of
        "b"  -> mplus (readsAll readBinaryInt dx >>= done) (wrong  dx "bad binary integer constant")
        base -> mplus (readsAll readOct      bdx >>= done) (wrong bdx "bad ocatal integer constant")

-- | Parses 'Prelude.Integer's, 'Prelude.Double's, 'Complex.Complex's, and 'Data.Time.DiffTime's, 
numeric :: Parser (PValue Object)
numeric = do
  sign <- zeroOrOne plusMinus
  mplus (char '.' >> regexMany1 digit >>= \dgt -> parseExponent digit 10 (sign++".") sign dgt "") $ do
    dgt <- regexMany1 digit
    let got = sign++dgt
    case dgt of
      "0"      -> do
        base <- zeroOrOne alpha
        let errmsg = wrong ('0':base) "invalid integer expression"
        flip mplus errmsg $ case base of
          "b"  -> regexMany1 digit  >>= parseDecimal digit  2  sign "0b"
          "x"  -> regexMany1 xdigit >>= parseDecimal xdigit 16 sign "0x"
          ""   -> ok (OInt 0)
          _    -> errmsg
      '0':dgt  -> parseDecimal digit 8  got sign dgt
      dgt      -> parseDecimal digit 10 got sign dgt
  where
    plusMinus = rxUnion (rxChar '+') (rxChar '-')
    parseDecimal take base got sign dgt = flip mplus (parseExponent take base got sign dgt "") $ do
      typ <- zeroOrOne (rxChar '.')
      case typ of
        ""  -> makeRational base sign got dgt "" "" ""
        "." -> do
          rdx <- regexMany take
          if null rdx
            then wrong got "expecting digits after point"
            else parseExponent take base (got++typ++rdx) sign dgt rdx
    parseExponent take base got sign dgt rdx = do
      expSign <- zeroOrOne plusMinus
      regex (rxUnion (rxChar 'e') (rxChar 'E'))
      expDgt <- regexMany1 take
      if null (drop 5 expDgt) -- make sure the exponent is 5 digits or less.
        then  makeRational base sign got dgt rdx expSign expDgt
        else  wrong (got++expSign++expDgt) "exponent is too large"
    makeRational base sign got dgt rdx expSign expDgt = case r of
      Nothing -> wrong got ("incorrect digits used to form a base-"++show base++" number")
      Just (is_an_integer, r) -> getType got is_an_integer r
      where
        r = do
          let b = toInteger base
          x   <- fmap (if null sign then id else negate) $ rationalFromString base (b%1) dgt
          y   <- rationalFromString base (1%b) rdx
          exp <- fmap round (rationalFromString base (b%1) expDgt)
          let ibase = if expSign=="-" then 1%b else b%1
          return (b==1 && exp>=1, x*y*ibase^^exp)
    getType got r_is_an_integer r = do
      typ <- zeroOrOne alpha
      case typ of
        "U" -> ok $ OWord $ fromIntegral $ round r
        "I" -> ok $ OLong $ round r
        "L" -> ok $ OInt $ round r
        "R" -> ok $ ORatio r
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

parseString :: Parser (PValue UStr)
parseString = do
  char '"'
  loop "\"" where
    stops = foldl1 setUnion $ map point "\"\\\n"
    loop zx = do
      str <- regexMany (rxCharSet (setInvert stops))
      if null str
        then mplus (char '"' >> ok (ustr zx)) (wrong zx "unterminated string constant")
        else do
          stop <- charSet stops
          let got = zx++str++stop
          case stop of
            "\n" -> wrong got "string runs past end of line"
            "\\" -> msum $
              [ do  regexMany (rxUnion hspace (rxChar '\r')) >> regex (rxChar '\n')
                    wrong got "cannot use '\\' token to continue string to next line"
              , regex rxTrue >>= \c -> loop (got++c)
              ]
            "\"" -> msum $
              [ readsAll reads got >>= ok
              , wrong got "invalid string constant"
              ]

object :: Parser Object
object = do
  return ONull

