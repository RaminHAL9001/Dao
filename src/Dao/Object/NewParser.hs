-- "src/DaoTT/Object/NewParser.hs" makes use of "DaoTT.NewParser" to parse
-- parse 'DaoTT.Object.AST_Object' expressions.
-- 
-- Copyright (C) 2008-2013  Ramin Honary.
-- This file is part of the DaoTT System.
--
-- The DaoTT System is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
-- 
-- The DaoTT System is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

{-# LANGUAGE MultiParamTypeClasses #-}

module Dao.Object.NewParser where

import           Dao.String
import           Dao.Object hiding (Tokenizer)
import           Dao.Object.AST
import           Dao.Predicate
import           Dao.NewParser
import qualified Dao.Tree as T
import qualified Dao.EnumSet as Es

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error hiding (Error)
import           Control.Monad.State

import           Data.Monoid
import           Data.Maybe
import           Data.List
import           Data.Char hiding (Spaces)
import           Data.Word
import qualified Data.Set  as S
import qualified Data.Map  as M
import           Data.Ratio
import           Data.Complex
import           Data.Time.Clock
import           Data.Array.IArray
import           Numeric

import Debug.Trace

maxYears :: Integer
maxYears = 99999

----------------------------------------------------------------------------------------------------

newtype DaoTT = DaoTT{ daoUnwrapTT :: TT } deriving (Eq, Ord, Ix)
instance TokenType  DaoTT where { unwrapTT = daoUnwrapTT; wrapTT = DaoTT }
instance HasTokenDB DaoTT where { tokenDB  = daoTokenDB }
instance MetaToken DaoTokenLabel DaoTT where { tokenDBFromMetaValue _ = tokenDB }
instance Show DaoTT where { show = deriveShowFromTokenDB daoTokenDB }

type DaoLexer    = Lexer DaoTT ()
type DaoParser a = Parser DaoParState DaoTT a
type DaoParseErr = Error DaoParState DaoTT

daoTokenDB :: TokenDB DaoTT
daoTokenDB = makeTokenDB daoTokenDef

data DaoTokenLabel
  = SPACE | INLINECOM | ENDLINECOM
  | BASE64DATA | BASE16 | BASE2 | BASE10 | DOTBASE10 | NUMTYPE | EXPONENT | INTREF
  | DATE | TIME | STRINGLIT | LABEL
  deriving (Eq, Enum, Show, Read)
instance UStrType DaoTokenLabel where { ustr = derive_ustr; fromUStr = derive_fromUStr; }

daoTokenDef :: LexBuilder ()
daoTokenDef = do
  ------------------------------------------ WHITESPACE -------------------------------------------
  space        <- emptyToken SPACE      $ rxRepeat1(map ch "\t\n\r\f\v ")
  
  ------------------------------------------- COMMENTS --------------------------------------------
  closeInliner <- fullToken  INLINECOM  $ rxRepeat1(ch '*') . rx '/'
  inlineCom    <- fullToken  INLINECOM  $ rx "/*" .
    fix (\loop -> closeInliner <> rxRepeat1(invert[ch '*']) . loop)
  endlineCom   <- fullToken  ENDLINECOM $ rx "//" . rxRepeat(invert[ch '\n'])
  multiComs    <- pure $ opt $ fix((mconcat[space, inlineCom, endlineCom]).)
  
  ------------------------------------------- KEYWORDS --------------------------------------------
  keywords     <- keyStringTable $ words $ unwords $
    [ "if else for in while with try catch continue break return throw"
    , "data struct list set intmap dict array date time"
    , "global local qtime static function func pattern rule"
    , "import imports require requires BEGIN END EXIT"
    ]
  let alpha = [from 'A' to 'Z', from 'a' to 'z', ch '_']
  label        <- fullToken  LABEL      $ rxRepeat1 alpha . rxRepeat(from '0' to '9' : alpha)
  
  ------------------------------------------ PUNCTUATION ------------------------------------------
  operators    <- keyStringTable $ words $ unwords $
    [allUpdateOpStrs, allArithOp1Strs, allArithOp2Strs]
  groups@[openBracket, closeBracket, openBrace, closeBrace, openParen, closeParen, comma] <-
    mapM (keyString . (:[])) "[]{}(),"
  
  -------------------------------------- DATA SPECIAL SYNTAX --------------------------------------
  dataRX       <- keyString "data"
  base64Data   <- fullToken  BASE64DATA $
    rxRepeat1[from 'A' to 'Z', from 'a' to 'z', from '0' to '9', ch '+', ch '/']
  dataSyntax   <- pure $ dataRX . multiComs . openBrace .
    fix(\loop -> (base64Data<>space) . loop <> closeBrace <> rxErr "unknown token in base-64 data")
  
  ---------------------------------------- STRING  LITERAL ----------------------------------------
  stringLit    <- fullToken  STRINGLIT  $ rx '"' .
    (fix $ \loop ->
      rxRepeat(invert [ch '"', ch '\\']) . (rx "\\" . rx anyChar . loop <> rx '"'))
  
  ---------------------------------------- NUMERICAL TYPES ----------------------------------------
  let from0to9  = from '0' to '9'
      plusMinus = rx[ch '+', ch '-']
      dot       = rx '.'
  base10       <- fullToken  BASE10     $ rxRepeat1 from0to9
  dotBase10    <- fullToken  DOTBASE10  $ dot . base10
  exponent     <- fullToken  EXPONENT   $ rx[ch 'e', ch 'E'] .
    cantFail "expecting exponent value after 'e' or 'E' character" . opt plusMinus . base10
  base2        <- fullToken  BASE2      $ (rx "0b" <> rx "0B") . base10
  base16       <- fullToken  BASE16     $ (rx "0x" <> rx "0X") .
    rxRepeat[from0to9, from 'A' to 'F', from 'a' to 'f']
  numType      <- fullToken  NUMTYPE    $ rx (map ch "UILRFfijs")
  intRefSyntax <- fullToken  INTREF     $ rx '$' . rxRepeat1 from0to9
  let base10Syntax = dotBase10 . opt exponent . opt numType <>
        base10 . opt (dotBase10 <> dot) . opt exponent . opt numType
  
  ----------------------------------- DATE/TIME SPECIAL SYNTAX ------------------------------------
  let dd           = rxLimitMinMax 1 1 from0to9
      dash         = rx '-'
      colon        = rx ':'
      getTime   i  = dd . (let loop i = if i>0 then colon . opt (dd . loop(i-1)) else id in loop i)
  time         <- fullToken  TIME       $ getTime 3
  time24       <- fullToken  TIME       $ getTime 2
  date         <- fullToken  DATE       $ rxLimitMinMax 4 1 from0to9 . dash . dd . dash . dd
  let getMills     = dotBase10 <>
        dot . rxErr "expecting decimal value after dot in time expression"
      optParens re = (space<>multiComs) . re <>
        multiComs . openParen . multiComs . re . multiComs .
          cantFail "expecting closing parenthesis" . closeParen
      optComma    = opt space . opt comma . opt space
      dateTime    = date .
        opt (optComma . opt plusMinus .  cantFail "expecting time value" . time24 .
          opt getMills . optComma . label)
  timeKeyword  <- keyString "time"
  dateKeyword  <- keyString "date"
  timeSyntax   <- pure $ timeKeyword . optParens (time . opt getMills)
  dateSyntax   <- pure $ dateKeyword . optParens dateTime
  
  ------------------------------------------- ACTIVATE --------------------------------------------
  activate $
    [ space, inlineCom, endlineCom
    , dataSyntax, dateSyntax, timeSyntax
    , stringLit, base16, base2, base10Syntax, intRefSyntax
    , keywords, mconcat groups, operators
    ]

----------------------------------------------------------------------------------------------------

data DaoParState
  = DaoParState
    { bufferedComments :: Maybe [Comment]
    , nonHaltingErrors :: [DaoParseErr]
    }
instance Monoid DaoParState where
  mappend a b =
     b{ bufferedComments = bufferedComments a >>= \a -> bufferedComments b >>= \b -> return (a++b)
      , nonHaltingErrors = nonHaltingErrors a ++ nonHaltingErrors b
      }
  mempty = DaoParState{ bufferedComments = Nothing, nonHaltingErrors = [] }

setCommentBuffer :: [Comment] -> DaoParser ()
setCommentBuffer coms = modify $ \st ->
  st{ bufferedComments = if null coms then mzero else return coms }

failLater :: String -> DaoParser ()
failLater msg = catchError (fail msg) $ \err ->
  modify $ \st -> st{nonHaltingErrors = nonHaltingErrors st ++ [err{parseStateAtErr=Nothing}]}

----------------------------------------------------------------------------------------------------

parseNumber :: DaoParser Object
parseNumber = msum $
  [ base 16 BASE16
  , base  2 BASE2
  , join $ pure (numberFromStrs 10)
      <*> token BASE10
      <*> optional (token DOTBASE10)
      <*> optional (token EXPONENT)
      <*> numType
  ]
  where
    numType = optional (token NUMTYPE)
    ignore  = pure Nothing
    base b t = join $ pure (numberFromStrs b) <*>
      fmap (drop 2) (token t) <*> ignore <*> ignore <*> numType

-- copied from the Dao.DaoParser module
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

-- copied from the Dao.Parser module
numberFromStrs :: Int -> String -> Maybe String -> Maybe String -> Maybe String -> DaoParser Object
numberFromStrs base int maybFrac maybPlusMinusExp maybTyp = do
  let frac         = fromMaybe "" maybFrac
      plusMinusExp = fromMaybe "" maybPlusMinusExp
      typ          = fromMaybe "" maybTyp
      (exp, hasMinusSign) = case plusMinusExp of
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

-- | Compute diff times from strings representing days, hours, minutes, and seconds. The seconds
-- value may have a decimal point.
diffTimeFromStrs :: String -> String -> String -> String -> String -> DaoParser T_diffTime
diffTimeFromStrs days hours minutes seconds miliseconds = do
  days    <- check "days"    (maxYears*365) days
  hours   <- check "hours"             24   hours
  minutes <- check "minutes"           60   minutes
  let sec =  check "seconds"           60
  seconds <-
    if null miliseconds
      then  sec seconds
      else  do
        seconds <- sec seconds
        return (seconds + rint miliseconds % (10 ^ length miliseconds))
  return $ fromRational (60*60*24*days + 60*60*hours + 60*minutes + seconds)
  where
    rint str            = if null str then 0 else (read str :: Integer)
    integerToRational s = s % 1 :: Rational
    check :: String -> Integer -> String -> DaoParser Rational
    check typ maxVal  s = do
      let i    = rint s
          zero = return (0%1)
          ok   = return (integerToRational i)
          err  = fail $ concat ["time value expression with ", s, " ", typ, " is invalid"]
      if null s then zero else if i<maxVal then ok else err

----------------------------------------------------------------------------------------------------

testDaoLexer :: String -> IO ()
testDaoLexer = testLexicalAnalysis (tokenDBLexer daoTokenDB) 4

daoGrammar :: CFGrammar DaoParState DaoTT Object
daoGrammar = newCFGrammar 4 parseNumber

testDaoParser :: String -> IO ()
testDaoParser input = case parse daoGrammar mempty input of
  OK      a -> putStrLn ("Parser succeeded:\n"++show a)
  Backtrack -> testDaoLexer input >> putStrLn "---- PARSER BACKTRACKED ----"
  PFail err -> testDaoLexer input >> putStrLn ("---- PARSER FAILED ----" ++ show err)

