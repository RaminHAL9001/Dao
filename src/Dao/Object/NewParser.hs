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
numericObj :: Parser Object
numericObj = do
  beginToken
  sign <- plusMinus
  dgt  <- regexMany1 digit
  let errmsg = fail "invalid integer expression"
  case dgt of
    "0"     -> do
      base <- zeroOrOne alpha
      let alt take base = regexMany1 take >>= parsePoint base take sign
      flip mplus errmsg $ case base of
        "b" -> alt digit  2
        "x" -> alt xdigit 16
        ""  -> parsePoint 10 digit sign ""
        _   -> errmsg
    '0':dgt -> parsePoint 8  digit sign dgt
    dgt     -> parsePoint 10 digit sign dgt
  where
    plusMinus = zeroOrOne (rxUnion (rxChar '+') (rxChar '-'))
    parsePoint base take sign dgt = flip mplus (makeRational base sign dgt "" "" "") $ do
      char '.'
      rdx   <- regexMany take
      let done = makeRational base sign dgt rdx "" ""
      if null rdx
        then fail "expecting digits after point"
        else if base==10 then mplus (parseExponent sign dgt rdx) done else done
    parseExponent sign dgt rdx = do
      e       <- regex (rxUnion (rxChar 'e') (rxChar 'E'))
      expSign <- plusMinus
      expDgt  <- regexMany digit
      if null expDgt
        then  fail ("expecting numerical exponent after \""++e++expSign++"\" symbol")
        else
          if null (drop 5 expDgt) -- make sure the exponent is 5 digits or less.
            then  makeRational 10 sign dgt rdx expSign expDgt
            else  fail "exponent is too large"
    makeRational base sign dgt rdx expSign expDgt = do
      let b = toInteger base % 1
          r = do
            x   <- rationalFromString base b dgt
            y   <- rationalFromString base (recip b) rdx
            exp <- fmap (round . abs) (rationalFromString base b expDgt)
            let ibase  = if expSign=="-" then recip b else b
                result = (if sign=="-" then negate else id) ((x+y)*(ibase^^exp))
            return (round result % 1 == result, result)
      case r of
        Nothing -> fail ("incorrect digits used to form a base-"++show base++" number")
        Just (r_is_an_integer, r) -> do
          typ <- zeroOrOne alpha
          case typ of
            "U" -> ok $ OWord (fromIntegral (round r))
            "I" -> ok $ OInt  (round r)
            "L" -> ok $ OLong (round r)
            "R" -> ok $ ORatio r
            "F" -> ok $ OFloat (fromRational r)
            "f" -> ok $ OFloat (fromRational r)
            "i" -> ok $ OComplex (0 :+ fromRational r)
            "j" -> ok $ OComplex (0 :+ fromRational r)
            "s" -> ok $ ODiffTime (fromRational r)
            ""  ->
              if r_is_an_integer
                then
                  let i = round r
                  in  if fromIntegral (minBound::T_int) <= i && i <= fromIntegral (maxBound::T_int)
                        then  return $ OInt $ fromIntegral i
                        else  return $ OLong i
                else return (ORatio r)
            typ -> fail ("unknown numeric type "++show typ)

parseString :: Parser String
parseString = beginTokenIf (char '"') >> loop where
  stops = foldl1 setUnion $ map point "\"\\\n"
  loop = do
    regexMany (rxCharSet (setInvert stops))
    let errmsg = fail "string literal runs past end of input"
    flip mplus errmsg $ do
      stop <- charSet stops
      case stop of
        "\n" -> fail "string runs past end of line"
        "\\" -> mplus (regex rxTrue >> loop) $ do
          regexMany (rxUnion hspace (rxChar '\r')) >> regex (rxChar '\n')
          fail "cannot use '\\' token to continue string to next line"
        "\"" -> getToken >>= readsAll "invalid string constant" reads . tokenChars >>= ok
        _    -> errmsg

parseComment :: Parser [Comment]
parseComment = many comment where
  comment = do
    regexMany space -- ignore whitespace
    beginTokenIf (char '/')
    msum [endline, inline, backtrack]
  endline = do
    char '/'
    ax <- regexMany (rxNotCharSet (point '\n'))
    zeroOrOne (rxChar '\n')
    t <- endToken
    return (EndlineComment (ustr (tokenChars t)))
  inline = do
    regexMany1 (rxChar '*')
    mplus (char '/' >> endToken >>= \t -> return (InlineComment (ustr (tokenChars t)))) $ do
      regexMany (rxCharSet (setInvert (point '*')))
      we_hit_the_end <- endOfInput
      if we_hit_the_end
        then fail "comment runs past end of input"
        else inline

parseListable :: Char -> Char -> Char -> Parser a -> Parser [Com a]
parseListable open delim close getValue = char open >> loop [] where
  loop zx = do
    before <- parseComment
    value  <- getValue
    after  <- parseComment
    let next = zx++[com before value after]
    mplus (char delim >> loop next) (char close >> return next)

parseKeywordOrName :: Parser String
parseKeywordOrName = liftM2 (++) (regex alpha_) (regexMany alnum_)

isKeyword :: String -> Bool
isKeyword str = elem str $ words $ concat $
  [ " if else try catch for in break continue return throw call function"
  , " pattern regex import require"
  , " static qtime const global"
  ]

isTypeword ::String -> Bool
isTypeword str = elem str $ words $ concat $
  [ " null true false type ref int word float long ratio complex"
  , " string list set map tree intmap difftime date"
  , " range regex pattern parser script"
  ]

isReservedWord :: String -> Bool
isReservedWord str = isTypeword str || isKeyword str

parseName :: String -> Parser Name
parseName msg = do
  beginToken
  name <- parseKeywordOrName
  if isReservedWord name
    then fail msg
    else ok (ustr name)

expect :: String -> ([Comment] -> Parser a) -> Parser a
expect msg withComment =
  parseComment >>= \com -> regexMany space >> mplus (withComment com) (fail msg)

----------------------------------------------------------------------------------------------------

parseBracketedScript :: Parser [Com ScriptExpr]
parseBracketedScript = beginTokenIf (char '{') >>= \com1 -> loop [] where
  loop zx = mplus (regexMany space >> char '}' >> endToken >> return zx) $ do
    com1 <- parseComment
    expr <- regexMany space >> parseScriptExpr
    com2 <- parseComment
    loop (zx++[com com1 expr com2])

parseScriptExpr :: Parser ScriptExpr
parseScriptExpr = do
  key <- parseKeywordOrName
  msum $ map ($key) $ undefined -- TODO
    -- [ ifStatement, tryStatement, forStatement, withStatement, continueStatement, returnStatement
    -- , elseStatement, catchStatement, objectExprStatement
    -- ]

guardKeyword :: String -> Parser a -> String -> Parser a
guardKeyword str par key = guard (key==str) >> par

