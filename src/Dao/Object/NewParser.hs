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

newtype DaoTT = DaoTT{ unwrapDaoTT :: TT } deriving (Eq, Ord, Ix)
instance TokenType  DaoTT where { unwrapTT = unwrapDaoTT; wrapTT = DaoTT }
instance HasTokenDB DaoTT where { tokenDB  = daoTokenDB }
instance MetaToken DaoTokenLabel DaoTT where { tokenDBFromMetaValue _ = tokenDB }
instance Show DaoTT where { show = deriveShowFromTokenDB daoTokenDB }

type DaoLexer    = Lexer DaoTT ()
type DaoParser a = Parser DaoParState DaoTT a
type DaoParseErr = Error DaoParState DaoTT

daoTokenDB :: TokenDB DaoTT
daoTokenDB = makeTokenDB daoTokenDef

data DaoTokenLabel
  = SPACE | INLINECOM | ENDLINECOM | COMMA
  | BASE64DATA | BASE16 | BASE2 | BASE10 | DOTBASE10 | NUMTYPE | EXPONENT | INTREF
  | STRINGLIT | LABEL
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
    [allUpdateOpStrs, allArithOp1Strs, allArithOp2Strs, ":"]
  groups@[openParen, closeParen, openBrace, closeBrace, _, _, _, _] <-
    mapM keyString $ words "( ) { } [ ] #{ }#"
  semicolon    <- keyString                   ";"
  comma        <- emptyToken COMMA       $ rx ','
  
  -------------------------------------- DATA SPECIAL SYNTAX --------------------------------------
  dataRX       <- keyString "data"
  base64Data   <- fullToken  BASE64DATA $
    rxRepeat1[from 'A' to 'Z', from 'a' to 'z', from '0' to '9', ch '+', ch '/']
  dataParser   <- pure $ dataRX . multiComs . openBrace .
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
  intRefParser <- fullToken  INTREF     $ rx '$' . rxRepeat1 from0to9
  let base10Parser = dotBase10 . opt exponent . opt numType <>
        base10 . opt (dotBase10 <> dot) . opt exponent . opt numType
  
  ------------------------------------------- ACTIVATE --------------------------------------------
  activate $
    [ space, inlineCom, endlineCom, comma
    , stringLit, base16, base2, base10Parser, intRefParser, dataParser
    , keywords, label, mconcat groups, operators
    , semicolon
    ]

----------------------------------------------------------------------------------------------------

data DaoParState
  = DaoParState
    { bufferedComments :: Maybe [Comment]
    , nonHaltingErrors :: [DaoParseErr]
    , internalState    :: Maybe (TokStreamState DaoParState DaoTT)
    }
instance Monoid DaoParState where
  mappend a b =
     b{ bufferedComments = bufferedComments a >>= \a -> bufferedComments b >>= \b -> return (a++b)
      , nonHaltingErrors = nonHaltingErrors a ++ nonHaltingErrors b
      , internalState    = internalState b
      }
  mempty =
    DaoParState
    { bufferedComments = Nothing
    , nonHaltingErrors = []
    , internalState = Nothing
    }

setCommentBuffer :: [Comment] -> DaoParser ()
setCommentBuffer coms = modify $ \st ->
  st{ bufferedComments = if null coms then mzero else return coms }

failLater :: String -> DaoParser ()
failLater msg = catchError (fail msg) $ \err ->
  modify $ \st -> st{nonHaltingErrors = nonHaltingErrors st ++ [err{parseStateAtErr=Nothing}]}

----------------------------------------------------------------------------------------------------

-- | Parses an arbitrary number of space and comment tokens, comments are returned.
space :: DaoParser [Comment]
space = label "space" $ do
  st <- get
  case bufferedComments st of
    Just coms -> put (st{bufferedComments=mempty}) >> return coms
    Nothing   -> fmap concat $ many $ table $
      [ token SPACE (const [] . as0)
      , token INLINECOM  (\c -> [InlineComment  $ asUStr c])
      , token ENDLINECOM (\c -> [EndlineComment $ asUStr c])
      ]

-- | Evaluates a 'DaoParser' within a cluster of optional spaces and comments, returning the result
-- of the parser wrapped in a 'Dao.Object.Com' constructor. If the given 'DaoParser' backtracks, the
-- comments that were parsed before the 'DaoParser' was evaluated are buffered so a second call two
-- successive calls to this function return immediately. For example in an expression like:
-- > 'Control.Monad.msum' ['commented' p1, 'commented' p2, ... , 'commented' pN]
-- The spaces and comments occurring before the parsers @p1@, @p2@, ... , @pN@ are only being parsed
-- once, no matter how many parser are tried.
commented :: DaoParser a -> DaoParser (Com a)
commented parser = do
  before <- space
  msum $
    [ parser >>= \result -> space >>= \after -> return (com before result after)
    , modify(\st -> st{bufferedComments = Just before}) >> mzero
    ]

-- | Parses an expression which may optionally occur within a parenthetical closure. Provide a
-- message that will be used to produce an error message if there is no matching close parenthesis
-- token.
optParens :: String -> DaoParser a -> DaoParser a
optParens errmsg parser = msum $
  [ tokenBy "(" as0 >> parser >>= \a -> expect errmsg (tokenBy ")" as0) >> return a
  , parser
  ]

----------------------------------------------------------------------------------------------------

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
  let frac         = fromMaybe "" (fmap tail maybFrac)
      strprfx      = foldl (\f s t -> f (fromMaybe t (stripPrefix s t))) id . words
      plusMinusExp = fromMaybe "" (fmap (strprfx ".e .E e E") maybPlusMinusExp)
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

----------------------------------------------------------------------------------------------------

-- | Compute diff times from strings representing days, hours, minutes, and seconds. The seconds
-- value may have a decimal point.
diffTimeFromStrs :: String -> Maybe String -> DaoParser T_diffTime
diffTimeFromStrs time optMils = do
  let miliseconds = noDot optMils
  let [days,hours,minutes,seconds] = split [] time
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
    noDot     str       = case str of { Nothing -> ""; Just('.':str) -> str; Just str -> str; }
    noPlus    str       = case str of { '+':str -> str; _ -> str; }
    split buf str       = case break (==':') str of
      (t, ""     ) -> reverse $ take 4 $ (t:buf) ++ repeat ""
      (t, ':':str) -> split (t:buf) str
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

-- | Parsing numerical literals
number :: DaoParser Object
number = label "number" $ table $
  [ base 16 BASE16
  , base  2 BASE2
  , join $ pure (numberFromStrs 10)
      <*> token BASE10 asString
      <*> optional (token DOTBASE10 asString)
      <*> optional (token EXPONENT  asString)
      <*> optional (token NUMTYPE   asString)
  , join $ pure (numberFromStrs 10 "")
      <*> token DOTBASE10 (Just . asString)
      <*> optional (token EXPONENT asString)
      <*> optional (token NUMTYPE  asString)
  ]
  where
    ignore   = pure Nothing
    base b t = join $ pure (numberFromStrs b) <*>
      fmap (drop 2) (token t asString) <*> ignore <*> ignore <*> optional (token NUMTYPE asString)

-- Objects that are parsed as a single value, which includes all literal expressions and equtions in
-- parentheses.
singleton :: DaoParser AST_Object
singleton = label "singleton" $ mplus (inParens object) $ fmap (\o -> AST_Literal o LocationUnknown) $ table $
  [ number
  , ORef    . IntRef   . read . tail <$> token INTREF    asString
  , ORef    . LocalRef               <$> token LABEL     asUStr
  , OString . read                   <$> token STRINGLIT asString
  ]

inParens :: DaoParser AST_Object -> DaoParser AST_Object
inParens parser = label "inParens" $ do
  tokenBy "(" as0
  a <- pure (\o -> AST_Paren True o LocationUnknown) <*> commented object
  expect "close-parentheses" $ tokenBy ")" (const a)

commaSepd :: (UStrType str, UStrType errmsg) =>
  errmsg -> str -> str -> DaoParser AST_Object -> DaoParser [Com AST_Object]
commaSepd errMsg open close parser = label "commaSepd" $ do
  tokenBy open as0
  objs <- (space >>= \c -> tokenBy close as0 >> return [com c AST_Void []])
    <|> many (commented object)
  expect errMsg $ tokenBy close as0 >> return objs

reference :: DaoParser AST_Object
reference = label "reference" $ infixed msg rightAssoc AST_Equation prefixedTerm (opsParser ". ->") where
  msg = "label after dot or arrow operator"
  prefixedTerm = label "reference.prefixedTerm" $ withLoc $
    pure AST_Prefix <*> (read <$> (tokenBy "$" <> tokenBy "@") asString) <*> commented singleton

funcCall :: DaoParser AST_Object
funcCall = label "funcCall" $ withLoc $ pure AST_FuncCall
  <*> token LABEL asUStr
  <*> space
  <*> commaSepd "close-parentheses after function call" "(" ")" object

arraySub :: DaoParser AST_Object
arraySub = label "arraySub" $ withLoc $ pure AST_ArraySub
  <*> reference
  <*> space
  <*> (tokenBy "[" as0 >> commented object >>= \o -> tokenBy "]" as0 >> return o)

type InfixConstr   op obj = obj -> op -> obj -> Location -> obj
type Associativity op obj = InfixConstr op obj -> obj -> [(Location, obj, op)] -> obj

rightAssoc :: Associativity op obj
rightAssoc constr = foldr (\ (loc, lhs, op) rhs -> constr lhs op rhs loc)

leftAssoc :: Associativity op obj
leftAssoc constr = foldl (\ lhs (loc, rhs, op) -> constr lhs op rhs loc)

initInfixed
  :: String
  -> Associativity op obj
  -> InfixConstr op obj
  -> DaoParser obj
  -> DaoParser op
  -> obj -> DaoParser obj
initInfixed msg assoc constr parser opsParser initObj = loop [] initObj where
  loop stack first = flip mplus (return (assoc constr first stack)) $ do
    tok <- look1 id
    let loc = asLocation tok
    op  <- opsParser
    join $ pure (\loc rhs -> loop (stack++[(loc, rhs, op)]) first)
      <*> look1 asLocation
      <*> expect (msg++" after "++show tok++" token") parser

infixed
  :: String
  -> Associativity op obj
  -> InfixConstr op obj
  -> DaoParser obj
  -> DaoParser op
  -> DaoParser obj
infixed msg assoc constr parser opsParser = trace "infixed" $
  trace "initial parser for infix" parser >>= trace "bind to initInfixed" . initInfixed msg assoc constr parser opsParser

initInfixTable
  :: String
  -> DaoParser obj
  -> [(Associativity op obj, InfixConstr op obj, DaoParser op)]
  -> obj -> DaoParser obj
initInfixTable msg parser table first = trace "initInfixTable" $ loop table first where
  loop upper first = trace "loop infixTable" $ case upper of
    []                            -> trace "infixTable backtracked" $ mzero
    (assoc, constr, opstrs):lower -> msum $
      [ trace "initInfixTable: parse then loop at current associativity" $ initInfixed msg assoc constr parser opstrs first >>= loop table
      , trace "initInfixTable: loop to lower associativity" loop lower first
      , return first
      ]

infixTable
  :: String
  -> DaoParser obj
  -> [(Associativity op obj, InfixConstr op obj, DaoParser op)]
  -> DaoParser obj
infixTable msg parser table = trace "init parser for infixTable" parser >>= trace "bind to initInfixTable" . initInfixTable msg parser table

opsParser :: Read op => String -> DaoParser (Com op)
opsParser = commented . table . fmap (flip tokenBy (read . asString)) . words

object :: DaoParser AST_Object
object = trace "object" $ label "object" $ flip mplus obj $ withLoc $
  pure AST_Prefix <*> read <$> mconcat (map tokenBy (words "- ~ !")) asString <*> commented obj
  where { obj = table [funcCall, arraySub, reference, singleton] }

arithmetic :: DaoParser AST_Object
arithmetic = trace "evaluate arithmetic" $ label "arithmetic" $ infixTable msg object table where
  msg   = "object expression after arithmetic operator"
  table = (rightAssoc, AST_Equation, commented $ tokenBy "**" (read . asString)) :
    map (\str -> (leftAssoc, AST_Equation, label "arithmetic.operator" $ opsParser str))
      ["* / %", "+ -", "<< >>", "&", "^", "|", "&&", "||", "< <= >= >", "== !="]

equation :: DaoParser AST_Object
equation = label "equation" $
  infixed msg rightAssoc AST_Assign (trace "arithmetic parser in \"equation\"" arithmetic) (opsParser allUpdateOpStrs) where
    msg = "object expression after assignment operator"

----------------------------------------------------------------------------------------------------

daoParser :: DaoParser AST_Object
daoParser = equation

daoGrammar :: Language DaoParState DaoTT AST_Object
daoGrammar = newLanguage 4 $ mplus daoParser $ do
  tokSt <- parserLiftTokStream get
  modify (\st -> st{internalState = Just tokSt})
  fail "Parser backtracked without taking all input."

----------------------------------------------------------------------------------------------------

testDaoLexer :: String -> IO ()
testDaoLexer = testLexicalAnalysis (tokenDBLexer daoTokenDB) 4

testDaoParser :: String -> IO ()
testDaoParser input = case parse daoGrammar mempty input of
  OK      a -> putStrLn ("Parser succeeded:\n"++show a)
  Backtrack -> testDaoLexer input >> putStrLn "---- PARSER BACKTRACKED ----\n"
  PFail err -> do
    testDaoLexer input
    putStrLn ("---- PARSER FAILED ----\n" ++ show err)
    let st = parseStateAtErr err >>= internalState
    putStrLn ("recentTokens = "++show (tokenQueue <$> st))
    putStrLn ("getLines     = "++show (getLines   <$> st))

