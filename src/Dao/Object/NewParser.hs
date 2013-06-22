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

--  dbg :: String -> Parser Parstate Dao a -> Parser Parstate Dao a
--  dbg msg parser = do
--    t <- optional currentTok
--    trace ("parsing "++msg++", next token is "++show t) (return ())
--    v <- catchPValue parser
--    flip trace (return ()) $ case v of
--      PFail err -> msg++" failed: "++show err
--      Backtrack -> msg++" backtracked"
--      OK      _ -> msg++" OK"
--    assumePValue v

----------------------------------------------------------------------------------------------------

type DaoParser a = Parser Parstate DaoTT a
type DaoLexer  a = Lexer DaoTT a
type DaoParseErr = Error Parstate DaoTT

newtype Dao = Dao{ daoUnwrapTT :: TT } deriving (Eq, Ord, Ix)
instance TokenType Dao where { unwrapTT = daoUnwrapTT; wrapTT = Dao; }
instance Show Dao where { show = deriveShowFromTokenDB daoTokenDB }

daoTokenDB :: TokenDB Dao
daoTokenDB = makeTokenDB daoTokenDef

spaceLabel      = "SPACE"
base16intLabel  = "BASE16"
base2intLabel   = "BASE2"
litStrLabel     = "STRING"
inlineComLabel  = "INLINECOM"
endlineComLabel = "ENDLINECOM"

daoTokenDef :: LexBuilder Dao ()
daoTokenDef = do
  let spaceRegex = rxRepeat(map ch " \t\n\r\f\v")
  space <- regexToken spaceLabel spaceRegex
  let inlineComRegex =
        rx "/*" . rxRepeat(invert[ch '*']) . rxRepeat(ch '*') . (rx '/' <> inlineComRegex)
  inlineCom  <- regexToken inlineComLabel inlineComRegex
  let endlineComRegex = rx "//" . rxRepeat(invert[ch '\n'])
  endlineCom <- regexToken endlineComLabel endlineComRegex
  dataToken  <- stringToken "data"
  openBrace  <- stringToken "{"
  closeBrace <- stringToken "}"
  base64Data <- newTokenType "BASE64DATA"
  let multiComments =
        (mconcat $
            [ spaceRegex      . rxEmptyToken space
            , inlineComRegex  . rxToken inlineCom
            , endlineComRegex . rxToken endlineCom
            ]) . multiComments
  let base64DataRegex =
        (spaceRegex . rxToken space <>
          rxRepeat [from 'A' to 'Z', from 'a' to 'z', from '0' to '9', ch '+', ch '/'] .
            rxToken base64Data) . base64DataRegex
  regex $ rx "data" . rxEmptyToken dataToken . (multiComments <>
    rx "{" . rxEmptyToken openBrace . (base64DataRegex <> rx "}" . rxEmptyToken closeBrace))
  stringTable $ words $ unwords $
    [ allUpdateOpStrs, allArithOp1Strs, allArithOp2Strs
    , "{ } ( ) [ ]"
    ]
  stringTable $ words $ unwords $
    [ "if else for in while with try catch"
    , "continue break return throw"
    , "data struct list set intmap dict array date time"
    , "global local qtime static"
    , "function func pattern rule"
    , "import imports require requires"
    , "BEGIN END EXIT"
    ]
  let strlit = rxRepeat (invert [ch '"', ch '\\']) . (rx "\\" . rx anyChar . strlit <> rx '"')
  regexToken litStrLabel $ rx '"' . strlit
  let from0to9 = from '0' to '9'
      hexdigits = rxRepeat[from0to9, from 'A' to 'F', from 'a' to 'f']
      decdigits = rxRepeat from0to9
  regexToken base16intLabel $ (rx "0x" <> rx "0X") . hexdigits
  regexToken base2intLabel  $ (rx "0b" <> rx "0B") . decdigits
  return ()

-- | The token types.
data DaoTT
  = Spaces      | ComEndl     | ComInln     | Label
  | KeyIF       | KeyELSE     | KeyFOR      | KeyIN       | KeyWHILE  | KeyWITH
  | KeyTRY      | KeyCATCH    | KeyCONTINUE | KeyBREAK    | KeyTHROW  | KeyRETURN
  | KeyDATA     | KeySTRUCT   | KeyLIST     | KeySET     
  | KeyINTMAP   | KeyDICT     | KeyARRAY    | KeyDATE     | KeyTIME
  | KeyGLOBAL   | KeyLOCAL    | KeyQTIME    | KeySTATIC
  | KeyFUNCTION | KeyFUNC     | KeyPAT      | KeyRULE
  | KeyBEGIN    | KeyEND      | KeyEXIT    
  | KeyIMPORT   | KeyREQUIRE               
  | DateTok     | TimeTok                  
  | CharLit     | StrLit                   
  | Digits10    | DotDigits10 | Exponent    | Digits16    | Digits8   | Digits2    | NumSuffix
  | OpenMeta    | CloseMeta
  | OpenParen   | CloseParen  | OpenSquare  | CloseSquare | OpenBrace | CloseBrace
  | InfixOp     | AssignOp    | Comma       | Semicolon   | Colon     | IntRefTok
  | LogicNotOp  | ArithNegOp  | BinInvertOp | DerefOp     | RefOp     | DotOp      | ArrowOp
  | Arbitrary   | Unknown
  deriving (Eq, Ord, Ix, Show)

--  operator :: String -> DaoParser String
--  operator a = token InfixOp $
--    currentTok >>= \tok -> let b = tokToStr tok in guard (a==b) >> return a

-- | Shorthand for @'ignore' ('token' 'Spaces')@
--  skipSpaces :: DaoParser ()
--  skipSpaces = ignore (takeToken Spaces)

--  optionStringType :: DaoTT -> DaoParser String
--  optionStringType tok = fmap (fromMaybe "") (optional $ fmap tokToStr $ takeToken tok)

----------------------------------------------------------------------------------------------------

data Parstate
  = Parstate
    { bufferedComments :: Maybe [Comment]
    , nonHaltingErrors :: [DaoParseErr]
    }
instance Monoid Parstate where
  mappend a b =
     b{ bufferedComments = bufferedComments a >>= \a -> bufferedComments b >>= \b -> return (a++b)
      , nonHaltingErrors = nonHaltingErrors a ++ nonHaltingErrors b
      }
  mempty = Parstate{ bufferedComments = Nothing, nonHaltingErrors = [] }

setCommentBuffer :: [Comment] -> DaoParser ()
setCommentBuffer coms = modify $ \st ->
  st{ bufferedComments = if null coms then mzero else return coms }

failLater :: String -> DaoParser ()
failLater msg = catchError (fail msg) $ \err ->
  modify $ \st -> st{nonHaltingErrors = nonHaltingErrors st ++ [err{parseStateAtErr=Nothing}]}

----------------------------------------------------------------------------------------------------

maxYears :: Integer
maxYears = 9999

lexDaoNumber :: DaoLexer ()
lexDaoNumber = msum $
  [ otherBase "0x" 'x' 'X' isHexDigit Digits16
  , otherBase "0b" 'b' 'B' isDigit    Digits2
  , leadingZero
  , lexWhile isDigit >> makeToken Digits10 >> getDotExp
  , getDot >> getE >> suffix
  ]
  where
    otherBase typmsg c1 c2 predicate tok = do
      mplus (lexString ('0':c1:[])) (lexString ('0':c2:[]))
      flip mplus (fail ("expecting digits after "++typmsg++" token")) $
        lexWhile predicate >> makeToken tok
      suffix
    leadingZero = do
      lexCharP (=='0')
      msum $
        [ do  lexWhile isDigit
              c <- optional lexLook1
              if c == Just '.'
                then  makeToken Digits10 >> getDotExp
                else  makeToken Digits8
        , makeToken Digits10 >> getDotExp
        , suffix -- if the above 'getDotExp' fails, we have still evalauted makeToken Digits10
        ]
    getDot = lexCharP (=='.') >> mplus (lexWhile isDigit >> makeToken DotDigits10) lexBacktrack
    badExp = fail "expecting digits after decimal point"
    getExp = msum $
      [ do  msum (map lexString $ words ".E+ .E- .E .e+ .e- .e")
            mplus (lexWhile isDigit) (fail "expecting digits after exponent mark")
            makeToken Exponent
      , do  lexCharP (=='.')
            mplus (lexWhile isDigit) badExp
            makeToken DotDigits10
            void (optional getE)
      , getE
      ]
    getE = do
      msum (map lexString $ words "E+ E- E e+ e- e")
      mplus (lexWhile isDigit) badExp
      makeToken Exponent
    getDotExp = msum [getDot >> getExp, getExp, return ()] >> suffix
    suffix = void $ optional $
      lexCharP (charSet "jisfFURLI") >> makeToken NumSuffix

daoKeywords :: [String]
daoKeywords = words $ concat $
  [ " if else for in while with try catch "
  , " continue break return throw "
  , " data struct list set intmap dict array date time "
  , " global local qtime static "
  , " function func pattern rule "
  , " import imports require requires"
  , " BEGIN END EXIT "
  ]

daoKeywordTokens :: [DaoTT]
daoKeywordTokens =
  [ KeyIF, KeyELSE, KeyFOR, KeyIN, KeyWHILE, KeyWITH, KeyTRY, KeyCATCH
  , KeyCONTINUE, KeyBREAK, KeyRETURN, KeyTHROW
  , KeyDATA, KeySTRUCT, KeyLIST, KeySET, KeyINTMAP, KeyDICT, KeyARRAY, KeyDATE, KeyTIME
  , KeyGLOBAL, KeyLOCAL, KeyQTIME, KeySTATIC
  , KeyFUNCTION, KeyFUNC, KeyPAT, KeyRULE
  , KeyIMPORT, KeyIMPORT, KeyREQUIRE, KeyREQUIRE
  , KeyBEGIN, KeyEND, KeyEXIT
  ]

daoStringToKeywordMap :: M.Map String DaoTT
daoStringToKeywordMap = M.fromList (zip daoKeywords daoKeywordTokens)

-- assumes keyword tokesn are consecutively enumerated
daoKeywordToStringArray :: Array DaoTT String
daoKeywordToStringArray = array (KeyIF, KeyRULE) (zip daoKeywordTokens daoKeywords)

daoKeywordToString :: DaoTT -> String
daoKeywordToString tok =
  if inRange (bounds daoKeywordToStringArray) tok then daoKeywordToStringArray!tok else ""

lexDaoKeyword :: DaoLexer ()
lexDaoKeyword = do
  let a = (=='_')
  lexWhile (unionCharP (a:[isAlpha]))
  optional (lexWhile (unionCharP (a:[isAlphaNum])))
  str <- gets lexBuffer
  case M.lookup str daoStringToKeywordMap of
    Just key -> makeEmptyToken key
    Nothing  -> makeToken Label

-- This function also lexes integer references like:
-- > $1 $2 $3 ...
lexDaoOperator :: DaoLexer ()
lexDaoOperator = do
  lexOperator daoInfixOperators
  op <- gets lexBuffer
  case op of
    ":"  -> makeEmptyToken Colon
    ";"  -> makeEmptyToken Semicolon
    ","  -> makeEmptyToken Comma
    "->" -> makeEmptyToken ArrowOp
    "@"  -> makeEmptyToken DerefOp
    "!"  -> makeEmptyToken LogicNotOp
    "~"  -> makeEmptyToken BinInvertOp
    "-"  -> makeEmptyToken ArithNegOp
    op  -> case (readsPrec 0 op :: [(ArithOp1, String)]) of
      []          -> makeToken InfixOp
      [(REF, "")] -> mplus (lexWhile isDigit >> makeToken IntRefTok) (makeToken RefOp)

lexDaoParens :: DaoLexer ()
lexDaoParens = msum $
  [ lexString "#{" >> makeEmptyToken OpenMeta
  , lexString "}#" >> makeEmptyToken CloseMeta
  , mk '(' OpenParen , mk ')' CloseParen
  , mk '[' OpenSquare, mk ']' CloseSquare
  , mk '{' OpenBrace , mk '}' CloseBrace
  ]
  where { mk c tok = lexChar c >> makeEmptyToken tok }

-- | One of the design goals of DaoTT is for its language to be able to express any of it's built-in
-- objects. Arbitrary data stored in 'DaoTT.Object.OBinary' objects are constructed from base-64
-- encoded tokens directly from the source file. This tokenizer accomodates for base-64 tokens by
-- looking for a "data" keyword and @{@ open-brace is seen, then switching to a special inner
-- tokenizer, producing a stream of tokens until a @}@ closing brace is seen, then control is
-- returned to the calling context.
dataSpecialLexer :: DaoLexer ()
dataSpecialLexer = do
  k <- lexWhile isAlpha >> gets lexBuffer
  case k of
    "data" -> do
      makeToken Label
      many $ msum [lexSpace Spaces, lexInlineC_Comment ComInln, lexEndlineC_Comment ComEndl]
      flip mplus (return ()) $ do
        lexChar '{' >> makeToken OpenBrace
        let b64chars = unionCharP [isAlphaNum, (=='+'), (=='/'), (=='=')]
            ender    = lexChar '}' >> makeToken CloseBrace
        runLexerLoop "base-64 data expression" ender $
          [ lexSpace Spaces
          , lexWhile b64chars >> makeToken Arbitrary
          , lexUntil (unionCharP [b64chars, isSpace, (=='}')]) >> makeToken Unknown
          , lexEOF >> fail "base-64 data expression runs past end of input"
          ]
    _ -> lexBacktrack

-- | This is the list of tokenizers used by 'lex' to break-up input string into parsable 'Token's.
daoLexers :: [DaoLexer ()]
daoLexers = 
  [ lexStringLiteral     StrLit
  , lexCharLiteral       CharLit
  , lexInlineC_Comment   ComInln
  , lexEndlineC_Comment  ComEndl
  , lexSpace             Spaces
  , dataSpecialLexer
  , lexDaoNumber
  , lexDaoKeyword
  , lexOperator allUpdateOpStrs >> makeToken AssignOp
  , lexDaoOperator
  , lexDaoParens
  ]

daoMainLexer :: DaoLexer ()
daoMainLexer = runLexerLoop "DaoTT script" lexEOF daoLexers

daoInfixOperators :: String
daoInfixOperators = concat [allArithOp2Strs, " ", allArithOp1Strs, " , : ; "]

----------------------------------------------------------------------------------------------------

-- copied from the DaoTT.DaoParser module
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
--  parseNumber :: DaoParser Object
--  parseNumber = msum $
--    [ token Digits16 $ shift >>= \str -> case tokToStr str of {'0':x:dx | charSet "Xx" x -> mk 16 dx}
--    , token Digits2  $ shift >>= \str -> case tokToStr str of {'0':b:dx | charSet "Bb" b -> mk 2  dx}
--    , token Digits8  $ shift >>= \str -> case tokToStr str of {'0':  dx                  -> mk 8  dx}
--    , token Digits10 $ shift >>= \str -> case tokToStr str of {dx                        -> mk 10 dx}
--    ]
--    where
--      dots = fmap stripDots (optionStringType DotDigits10)
--      exps = fmap stripEs   (optionStringType Exponent)
--      optexps frac = mplus exps (return "") >>= \e -> return (frac, e)
--      stripDots str = case str of {'.':str -> str; _ -> str}
--      stripEs   str = case str of
--        e:str     | e=='e' || e=='E' -> str
--        '.':e:str | e=='e' || e=='E' -> str
--        str                          -> str
--      mk base int = do
--        (frac, plusMinusExp) <-
--          if base==10 then mplus (dots >>= optexps) (optexps "") else return ("", "")
--        typ <- optionStringType NumSuffix
--        numberFromStrs base int frac plusMinusExp typ

-- copied from the DaoTT.Parser module
numberFromStrs :: Int -> String -> String -> String -> String -> DaoParser Object
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
--  parseOptionalParens :: String -> DaoParser a -> DaoParser a
--  parseOptionalParens errMsg parser = flip mplus (skipSpaces >> parser) $ do
--    takeToken OpenParen
--    skipSpaces
--    a <- parser
--    skipSpaces
--    expect ("close parenthesis after "++errMsg++" expression") (takeToken CloseParen >> return a)

-- | Parses a different form of 'ODiffTime', with days, hours, minutes, and seconds separated by
-- colons. This parser does not care about context, colons are used as @hour:minute:second@ separator
-- tokens, regardless of their meaning elsewhere. Spaces are not allowed between
-- @hour:minute:second@ tokens.
--  parseDiffTime :: DaoParser T_diffTime
--  parseDiffTime = do
--    d <- takeTokens [Digits10, Digits8]
--    takeToken Colon
--    flip mplus (fail "expecting diff-time expression") $ do
--      (dx, miliseconds) <- loop [tokToStr d] 3
--      case dx of
--        [days, hours, minutes, seconds] -> mk days hours minutes seconds miliseconds
--        [      hours, minutes, seconds] -> mk ""   hours minutes seconds miliseconds
--        [             minutes, seconds] -> mk ""   ""    minutes seconds miliseconds
--        -- ax -> trace (show ax) undefined
--    where
--      mk           = diffTimeFromStrs
--      colon        = takeToken Colon
--      stripDot str = case str of {'.':str -> str; _ -> str}
--      loop  got i  = case i of
--        i | i>0       -> flip mplus (return (got, "")) $
--          expect "expecting digits for time value" $ do
--            d <- takeTokens [Digits10, Digits8]
--            let got' = got++[tokToStr d]
--            flip mplus (return (got', "")) $ do
--              s <- takeTokens [DotDigits10, Colon]
--              case tokType s of
--                DotDigits10 -> return (got', tail (tokToStr s))
--                Colon       -> loop got' (i-1)
--                _           -> return (got', "")
--          | otherwise -> return (got, "")

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

-- | Parses a different form of 'OTime', with the standard dash-separated @YYYY-MM-DD@ format, along
-- with an optional time value as parsed by 'parseDiffTime'. This parser does not care about
-- context, dashes are used as @year-month-date@ separator tokens, regardless of their meaning
-- elsewhere. Spaces are not allowed between @year-month-date@ tokens.
--  parseTime :: DaoParser T_time
--  parseTime = do
--    yyyy <- takeTokens [Digits10, Digits8]
--    expect "absolute time constant expression" $ do
--      let dash = takeToken ArithNegOp
--          digits = fmap tokToStr (takeTokens [Digits10, Digits8])
--          year = tokToStr yyyy
--      month <- dash >> digits
--      day   <- dash >> digits
--      let commaSpace = -- a comma followed by an optional space OR no comma but a required space
--            mplus (takeToken Comma >> skipSpaces) (void $ takeToken Spaces)
--      diffTime <- mplus (commaSpace >> parseDiffTime) (return (fromRational 0))
--      zone <- msum $
--        [  do commaSpace
--              msum $
--                [ fmap tokToStr $ takeToken Label
--                , do  plus <- defaultTo "" (mplus (operator "+") (operator "-"))
--                      zone <- fmap tokToStr $ takeTokens [Digits10, Digits8]
--                      let withColon h1 h2 = expect "valid time-zone offset" $ msum $
--                            [ do  takeToken Colon
--                                  mm <- fmap tokToStr $ takeTokens [Digits10, Digits8]
--                                  case mm of
--                                    [m1,m2] | (read [m1,m2] :: Int) < 60 -> return (plus++[h1,h2,m1,m2])
--                                    _                                    -> mzero
--                            , if (read [h1,h2] :: Int) > 24 then return [h1,h2] else mzero
--                            ]
--                          withoutColon h1 h2 m1 m2 = do
--                            if (read [h1,h2] :: Int) < 24 && (read [m1,m2] :: Int) < 60
--                              then  return (plus++[h1,h2,m1,m2])
--                              else  fail $ concat $
--                                      [ "timezone offset value "
--                                      , zone, [h1,h2,m1,m2]
--                                      , " is out-of-bounds"
--                                      ]
--                      case zone of
--                        [h1,h2,m1,m2] -> withoutColon  h1 h2 m1 m2
--                        [   h2,m1,m2] -> withoutColon '0' h2 m1 m2
--                        [h1,h2      ] -> withColon  h1 h2
--                        [   h2      ] -> withColon '0' h2
--                ]
--        , return ""
--        ]
--      return (addUTCTime diffTime (read (year ++ '-':month ++ '-':day ++ " 00:00:00" ++ zone)))

----------------------------------------------------------------------------------------------------

parseComments :: DaoParser [Comment]
parseComments = msum $
  [ token ComInln $ shift >>= \com -> return [InlineComment  $ tokToUStr com]
  , token ComEndl $ shift >>= \com -> return [EndlineComment $ tokToUStr com]
  , token Spaces  $ shift >>= \ _  -> return []
  ]

-- | Use this to prevent constant re-parsing of comments. If you need just one set of comments and would like
-- to put it back if your parser fails, use this function. If you would like to try parsing an
-- expression with comments before and after it, use 'parseWithComments', which also does caching.
cachedComments :: ([Comment] -> DaoParser a) -> DaoParser a
cachedComments parser = do
  st   <- get
  let notCached = parseComments >>= \coms -> setCommentBuffer coms >> return coms
  coms <- case bufferedComments st of
    Nothing   -> notCached
    Just []   -> notCached
    Just coms -> return coms
  a <- parser coms
  setCommentBuffer []
  return a

-- | Uses already-buffered comments, or parses more comments into the buffer in the 'Parstate',
-- then evaluates a sub-parser. If the sub-parser succeeds, the buffer is cleared and the comments
-- are used to create a wrapper around the result value of the sub-parser. If the sub-parser
-- backtracks, the comments are left in the buffer for another parser to have a try. See also
-- 'cachedComments' if you need only comments before an expression, not before and after.
parseWithComments :: DaoParser a -> DaoParser (Com a)
parseWithComments parser = do
  st   <- get
  com1 <- case bufferedComments st of
    Nothing   -> parseComments >>= \com1 -> setCommentBuffer com1 >> return com1
    Just com1 -> return com1
  a    <- parser
  setCommentBuffer []
  com2 <- parseComments
  return (com com1 a com2)

parseTopLevelComments :: ([Comment] -> a) -> DaoParser a
parseTopLevelComments construct = cachedComments $ \coms ->
  if null coms then mzero else return (construct coms)

-- | Most nodes in the DaoTT abstract syntax tree take a 'DaoTT.Token.Location' as the final parameter.
-- This parser will take a sub-parser, store the cursor before and after running the sube parser,
-- constructing a 'DaoTT.Token.Location' from the before and after cursor positions. The sub-parser
-- must return a function that constructs some value (e.g. a node of the dao abstract syntax tree
-- with every sub-node filled in except for the 'DaoTT.Token.Location'). Then the location constructed
-- by this function passed as a parameter to the the node constructor returned by the sub-parser.
parseWithLocation :: DaoParser (Location -> a) -> DaoParser a
parseWithLocation parser = do
  (line1, col1) <- getCursor
  construct <- parser
  loc <- mplus (getCursor >>= \ (line2, col2) -> return (Location (fromIntegral line1) col1 (fromIntegral line2) col2))
               (return (atPoint line1 col1))
  return (construct loc)

--  parseFuncParams :: DaoParser AST_Object -> DaoParser [Com AST_Object]
--  parseFuncParams objParser = do
--    takeToken OpenParen
--    com1 <- parseComments
--    mplus (close >> return [com com1 AST_Void []]) (loop com1 [])
--    where
--      close = takeToken CloseParen
--      loop com1 got = expect "object expression for function parameter" $ do
--        obj  <- objParser
--        com2 <- parseComments
--        let got' = got++[com com1 obj com2]
--            msg = concat $
--              [ "either a closing parenthesis, "
--              , "or a comma follwed by the next item in function parameters list"
--              ]
--        expect msg $
--          mplus (close >> return got')
--                (takeToken Comma >> parseComments >>= \com1 -> loop com1 got')

-- | The @date@ and @time@ functions work on objects expressed with a special syntax that
-- needs to be handled before trying any other parser. This function is intended to be used with
-- 'DaoTT.NewParser.eachWithKeyword' or 'DaoTT.NewParser.withToken', so the final parameter is not
-- necessary unless your function is already holding a 'DaoTT.NewParser.Label'.
--  parseSpecialFuncCall :: String -> DaoParser Object -> UStr -> DaoParser AST_Object
--  parseSpecialFuncCall key objParser nextKeyword = do
--    guard (uchars nextKeyword == key)
--    com1 <- parseComments
--    let mk ox = return (AST_FuncCall (ustr key) com1 ox)
--        altParseObject = flip mplus parseObject $
--          parseWithLocation (fmap AST_Literal objParser)
--    parseWithLocation $ msum $
--      [ parseWithComments altParseObject >>= mk . (:[])
--      , parseFuncParams   altParseObject >>= mk
--      ]

----------------------------------------------------------------------------------------------------
-- Functions used for creating equations constructed from object expressions interleaved with infix
-- operators.

makeEquation :: [Either (Com Name) AST_Object] -> DaoParser AST_Object
makeEquation objx = case applyPrescedence objx of
  [Right obj] -> return obj
  _ ->  error $ ("unknown prescedence for operators:"++) $ concat $
          flip concatMap objx $ \obj -> case obj of
            Left obj -> [' ':uchars (unComment obj)]
            _        -> []

-- (Copied from the old parser "src/DaoTT/Object/Parser.hs")
-- Operator prescedence mimics the C and C++ family of languages.
-- 'applyPrescedence' scans from highest to lowest prescedence, essentially creating a function
-- that looks like this: (scanBind (words p3) . scanBind (words p2) . scanBind (words p1) . ...)
-- The apply (.) operator has the right-hand function evaluated first before the left-hand
-- function, so the right-most operators have the highest prescedence because they get bound by
-- 'scanBind' first. Therefore, listing the operators by prescedence (from left to right) means
-- listing them from lowest to highest prescedence.
applyPrescedence :: [Either (Com Name) AST_Object] -> [Either (Com Name) AST_Object]
applyPrescedence = foldl (.) assignOp $ map (scanBind AST_Equation . words) $ opPrecTable where
  assignOp = scanBind AST_Assign $ -- lowest prescedence, used as initial value to fold
    words "= += -= *= /= %= &= |= <<= >>= ^="
  opPrecTable = -- operators listed from lowest to highest prescedence
    [ "||", "&&", "|", "^", "&", "!= =="
    , "<= >= < >", "<< >>", "+ -", "* / %", "**", ". ->"
    ]

-- (Copied from the old parser "src/DaoTT/Object/Parser.hs")
-- Given a list of operators, scans through an equation of the form
-- (Right exprA : Left op : Right exprB : ...) 
-- and if the 'op' is in the list of operators, the 'exprA' and 'exprB' are bound together into an
-- 'DaoTT.Object.AST_Equation' data structure. If 'op' is not in the list of operators, it is passed over.
scanBind
  :: Read a
  => (AST_Object -> Com a -> AST_Object -> Location -> AST_Object)
  -> [String]
  -> [Either (Com Name) AST_Object]
  -> [Either (Com Name) AST_Object]
scanBind constructor ops objx = case objx of
  [Right o] -> [Right o]
  Right a : Left op : Right b : objx ->
    if elem (uchars (unComment op)) ops -- if this operator is of the prescedence we are looking for
      then  scanBind constructor ops $ -- "bind" the operands to it
             (Right (constructor a (fmap (read . uchars) op) b LocationUnknown) : objx)
      else  Right a : Left op : -- otherwise ignore this operator
              scanBind constructor ops (Right b : objx)
  objx -> error ("scanBind failed:\n"++show objx)

----------------------------------------------------------------------------------------------------
-- Prescedence parsing: the following parsers define a prescedence for 'DaoTT.Object.AST.AST_Object'
-- expressions.

-- A function to create parser table elements for 'OpenParen' and 'OpenMeta'.
--  parenOrMeta
--    :: DaoTT -> DaoTT
--    -> (Com AST_Object -> Location -> AST_Object)
--    -> DaoParser (Location -> AST_Object)
--  parenOrMeta open close construct = token open $ do
--    shift
--    expect "object expression after open-parnethesis" $ do
--      o <- parseWithComments parseObject
--      expect "close-parenthesis" (takeToken close >> return (construct o))

-- Highest prescedence object expression parsers, parses string literals, integer literals, and
-- expressions enclosed in parentheses or "meta-eval" braces.
--  objectExprPrec9 :: DaoParser (Location -> AST_Object)
--  objectExprPrec9 = msum $
--    [ parenOrMeta OpenParen CloseParen (AST_Paren True)
--    , parenOrMeta OpenMeta  CloseMeta   AST_MetaEval
--    , token StrLit  $ shift >>= return . AST_Literal . ostr  . read . tokToStr
--    , token CharLit $ shift >>= return . AST_Literal . OChar . read . tokToStr
--    , token Label   $ shift >>= return . AST_Literal . ORef . LocalRef . tokToUStr
--    ]

-- Extends 'objextTabElemsPrec8' to also parse reference and dereference prefix opreators.
--  objectExprPrec8 :: DaoParser (Location -> AST_Object)
--  objectExprPrec8 = mplus objectExprPrec9 $ msum [token DerefOp (mk REF), token RefOp (mk DEREF)] where
--    mk typ = do
--      obj <- parseWithComments (parseWithLocation objectExprPrec9)
--      return (AST_Prefix typ obj)

-- Parses objects (using 'objectExprPrec8') interleaved with the 'DaoTT.Object.DOT' (@.@) and
-- 'DaoTT.Object.POINT' (@->@) operators, prefix operators like 'DaoTT.Object.REF' (@@@) and
-- 'DaoTT.Object.DEREF' (@$@). This makes it suitable for parsing the parameter object expression for
-- functions like @global@ or @struct@ without requiring parentheses. Therefore expressions like the
-- following:
-- > global a + global b
-- will be parsed equivalently to the expression:
-- > (global a) + (global b)
--  parseRefEquation :: DaoParser AST_Object
--  parseRefEquation = init where
--    init = do -- get one object and then try the loop
--      obj <- parseWithLocation objectExprPrec8 -- if the loop backtracks on it's first item, return the object alone.
--      mplus (parseWithLocation (loop [Right obj])) (return obj)
--    loop got = flip mplus (makeEquation got >>= \obj -> return (setLocation obj)) $ do
--      comOp <- parseWithComments $ fmap tokType $ takeTokens [DotOp, ArrowOp]
--      let uncomOp = unComment comOp
--          (opStr, opUStr) =
--            if uncomOp==DotOp
--              then  ("dot (.)"   , ustr "." )
--              else  ("arrow (->)", ustr "->")
--          op = fmap (const opUStr) comOp
--      expect ("object expression after "++opStr++" operator") $
--        parseWithLocation objectExprPrec8 >>= \obj -> loop (got++[Left op, Right obj])

-- Extents objectTabElemsPrec8 with parsers of labels qualified with the keywords @global@, @local@,
-- @qtime@, or @static@.
--  objectExprPrec7 :: DaoParser (Location -> AST_Object)
--  objectExprPrec7 = mplus objectExprPrec8 $ msum $
--    [ token KeyGLOBAL qualifier
--    , token KeyQTIME  qualifier
--    , token KeyLOCAL  qualifier
--    , token KeySTATIC qualifier
--    ]
--    where
--      qualifier = shift >>= \kw -> parseRefEquation >>= \obj ->
--        cachedComments $ \coms -> return (AST_FuncCall (tokToUStr kw) coms [Com obj])

-- Parse object expressions indexed with a square-braced indexing epxression suffix.
--  objectSuffixed :: DaoParser (Location -> AST_Object)
--  objectSuffixed = do
--    obj <- parseWithLocation objectExprPrec7
--    flip mplus (return (const obj)) $ cachedComments $ \coms -> msum
--      [ do  takeToken OpenSquare
--            expect "object expression as an index value within square-brackets" $ do
--              idx <- parseWithComments parseObject
--              expect "expecting closing square-brackets" $ do
--                takeToken CloseSquare
--                return (AST_ArraySub obj coms idx)
--      , case obj of
--          AST_Literal (ORef ref) _ -> case ref of
--            LocalRef funcName -> flip mplus (return (const obj)) $
--              cachedComments $ \coms -> do
--                params <- parseFuncParams parseObject
--                return (AST_FuncCall funcName coms params)
--            _ -> fail "function call to unsupported reference type"
--                 -- TODO: make function calls to object expressions rather than plain names.
--          _ -> return (const obj)
--      ]

-- Extends the 'objectTabElemsPrec8' table with parsers that create object expressions from
-- keywords, for example @date@, @time@, @list@, @set@, @dict@, @intmap@, @struct@, and @array@.
-- Also parses arithmetic negation and bitwise inversion prefix operators.
--  objectExprPrec6 :: DaoParser (Location -> AST_Object)
--  objectExprPrec6 = msum $
--    [ objectSuffixed
--    , objectExprPrec7 
--    , token KeyDICT   dictIntmap
--    , token KeyINTMAP dictIntmap
--    , token KeyLIST   listSet
--    , token KeySET    listSet
--    , token KeyDATA   $ cachedComments $ \coms ->
--        expect "open brace containing base-64 data after \"data\" statement" $ do
--          takeToken OpenBrace
--          let loop got = do
--                next <- fmap (Com . tokToUStr) (takeToken Arbitrary)
--                skipSpaces
--                let got' = got++[next]
--                mplus (takeToken CloseBrace >> return got') (loop got')
--          got <- expect "base-64 data for \"data\" statement, or closnig brace" (loop [])
--          skipSpaces >> return (AST_Data coms got)
--    , token KeySTRUCT $ do
--        tok <- shift
--        let key = tokToStr tok
--            nobrace = takeToken OpenBrace
--        expect "optional item and required braced list of items to initialize struct" $ do
--          init  <- parseWithComments (mplus (nobrace >> parseObject) (return AST_Void))
--          items <- parseCommaSeparated key (assertAssignExpr key)
--          return (AST_Struct init items)
--    , token KeyARRAY  $ do
--        bounds <- parseWithComments (parseFuncParams parseObject)
--        expect "braced list of items to initialized array" $ do
--          items  <- parseCommaSeparated "array" (\_ -> return ())
--          return (AST_Array bounds items)
--    , token KeyFUNC     $ parseLambdaExpr FuncExprType
--    , token KeyFUNCTION $ parseLambdaExpr FuncExprType
--    , token KeyPAT      $ parseLambdaExpr PatExprType
--    , token KeyRULE     $ parseLambdaExpr RuleExprType
--    , objLit KeyDATE ODiffTime parseDiffTime, objLit KeyTIME OTime parseTime
--    , token ArithNegOp  $ prefixOp objectSuffixed NEG
--    , token BinInvertOp $ prefixOp objectSuffixed INVB
--    ]
--    where
--      objLit :: DaoTT -> (a -> Object) -> DaoParser a -> DaoParser (Location -> AST_Object)
--      objLit key constructor parser = token key (fmap (AST_Literal . constructor) parser)
--      dictIntmap = shift >>= \tok -> cachedComments $ \coms -> do
--        let k = tokToStr tok
--        fmap (AST_Dict (tokToUStr tok) coms) (parseCommaSeparated k (assertAssignExpr k))
--      listSet    = shift >>= \tok -> cachedComments $ \coms -> do
--        let k = tokToStr tok
--        fmap (AST_Dict (tokToUStr tok) coms) (parseCommaSeparated k (\_ -> return ()))
--      prefixOp parseObj typ =
--        fmap (AST_Prefix typ) (parseWithComments (parseWithLocation parseObj))

-- Used in 'objectExprPrec6', lambda expressions are expressions that begin with a keyword like
-- "func" or "rule".
--  parseLambdaExpr :: LambdaExprType -> DaoParser (Location -> AST_Object)
--  parseLambdaExpr ftyp = do
--    tok <- shift
--    params <- parseWithComments (parseFuncParams parseObject)
--    expect ("braced script after \""++tokToStr tok++"\" statement") $
--      fmap (AST_Lambda ftyp params) parseBracedScript

-- A general equation used to parse object expressions interleaved with infix operators. Used by
-- 'parseEqation' (indirectly via 'objectExprPrec5') and also by 'parseObject' directly.
--  parseInterleavedInfixOps :: DaoTT -> DaoParser (Location -> AST_Object) -> DaoParser (Location -> AST_Object)
--  parseInterleavedInfixOps opType objParser = do
--    let getObj = parseWithLocation objParser
--    obj <- getObj
--    let getOp = parseWithComments (fmap tokToUStr (takeToken opType))
--        loop lastOp got = expect ("object expression after ("++uchars (unComment lastOp)++") operator") $ do
--          obj <- getObj
--          let got' = got++[Right obj]
--          mplus (getOp >>= \op -> loop op (got'++[Left op])) (fmap const (makeEquation got'))
--    mplus (getOp >>= \op -> loop op [Right obj, Left op]) (return (const obj))

-- Parses an equation with arithmetic and logic operators, excluding prefix logical NOT. Assignment
-- operators are not parsed with this parser, they are lower presedence.
--  objectExprPrec5 :: DaoParser (Location -> AST_Object)
--  objectExprPrec5 = parseInterleavedInfixOps InfixOp objectExprPrec6

-- Parses either a logical-NOT operator followed by an infix-operator expression, or just the
-- infix-operator expression alone. This is the lowest-prescedence non-assignment equation
-- expression. It does not need a parse table.
--  parseEquation :: DaoParser (Location -> AST_Object)
--  parseEquation = flip mplus objectExprPrec5 $ do
--    takeToken LogicNotOp
--    fmap (AST_Prefix NOT) (parseWithComments (parseWithLocation objectExprPrec5))

-- | This is the entry-point parser for 'DaoTT.Object.AST.AST_Object'. It parses the
-- lowest-prescedence object equation expression, which are assignment expressions.
--  parseObject :: DaoParser AST_Object
--  parseObject = parseWithLocation $ parseInterleavedInfixOps AssignOp parseEquation

-- used by parseDictObject and parseListObject
--  parseCommaSeparated :: String -> (AST_Object -> DaoParser ()) -> DaoParser [Com AST_Object]
--  parseCommaSeparated key check = expect ("opening-brace for "++key++" expression") $ do
--    takeToken OpenBrace
--    let done got = takeToken CloseBrace >> return got
--        loop got = mplus (done got) $ do
--          obj <- parseWithComments $ parseObject >>= \obj -> check obj >> return obj
--          let got' = got++[obj]
--          expect ("comma or closing-brace to denote elements of "++key++" expression") $
--            mplus (done got') (takeToken Comma >> loop got')
--    loop []

-- Passed to 'parseCommaSeparated' by "dict" and "intmap" expressions.
assertAssignExpr :: String -> AST_Object -> DaoParser ()
assertAssignExpr key o = case o of
  AST_Assign _ _ _ _ -> return ()
  _                  -> failLater $
    "element of "++key++" expression is not an assignment expression"

----------------------------------------------------------------------------------------------------

-- Parse any script expression starting with a keyword.
--  parseScriptTable :: DaoParser (Location -> AST_Script)
--  parseScriptTable = msum $
--    [ token KeyIF $ cachedComments $ \coms ->
--        expect "conditional expression after \"if\" statement" $ do
--          obj <- parseObject
--          expect "braced script after \"if\" statement" $ do
--            thn <- parseWithComments parseBracedScript
--            let done = AST_IfThenElse coms obj thn
--            msum $
--              [ do  takeToken KeyELSE
--                    fmap done $ parseWithComments $
--                      expect "braced subscript or if statement after else statement" $ msum
--                        [ takeToken KeyIF >> parseScript >>= \s -> return [Com s]
--                        , parseBracedScript
--                        ]
--              , return $ done (Com [])
--              ]
--    , token KeyTRY $ do
--        tryScript <- parseWithComments parseBracedScript
--        msum $
--          [ do  takeToken KeyCATCH
--                expect "variable name after \"catch\" statement" $ do
--                  nm <- parseWithComments (fmap tokToUStr $ takeToken Label)
--                  expect "braced script after \"catch\" statement" $
--                    fmap (AST_TryCatch tryScript nm) parseBracedScript
--          , return (AST_TryCatch tryScript (Com nil) [])
--          ]
--    , token KeyELSE  $ fail "\"else\" statement not following an \"if\" statement"
--    , token KeyCATCH $ fail "\"catch\" statement not following a \"try\" statement"
--    , token KeyFOR   $ expect "iterator variable name after \"for\" statement" $ do
--        nm  <- parseWithComments (fmap tokToUStr $ takeToken Label)
--        expect "\"in\" after \"for\" statement" $ do
--          takeToken KeyIN
--          obj <- parseWithComments parseObject
--          expect "braced script after \"for\" statement" $
--            fmap (AST_ForLoop nm obj) parseBracedScript
--    , token KeyWHILE $ expect "iterator expression after \"while\" statement" $ do
--        obj <- parseWithComments parseObject
--        expect "braced script after \"while\" statement" $
--          fmap (AST_WhileLoop obj) parseBracedScript
--    , token KeyCONTINUE $ continueBreak True
--    , token KeyBREAK    $ continueBreak False
--    , token KeyRETURN   $ throwReturn   True
--    , token KeyTHROW    $ throwReturn   False
--    , token KeyWITH     $ do
--        obj <- parseWithComments parseObject
--        expect "braced script after \"with\" statement" $
--          fmap (AST_WithDoc obj) parseBracedScript
--    ]
--    where
--      continueBreak isContinue = do
--        tok <- shift
--        msum $
--          [ do  cachedComments $ \coms -> do
--                takeToken KeyIF
--                expect ("conditional expression after \""++tokToStr tok++" if\" statement") $ do
--                  obj <- parseWithComments parseObject
--                  expect ("semicolon after \""++tokToStr tok++"\" statement") $
--                    takeToken Semicolon >> return (AST_ContinueExpr isContinue coms obj)
--          , expect ("semicolon after \""++tokToStr tok++"\" statement") $ cachedComments $ \com -> do
--              takeToken Semicolon
--              return (AST_ContinueExpr isContinue com (Com AST_Void))
--          ]
--      throwReturn isReturn = do
--        tok <- shift
--        let done obj = expect ("colon after \""++tokToStr tok++"\" statement") $ 
--              takeToken Semicolon >> return (AST_ReturnExpr isReturn obj)
--        msum $
--          [ parseWithComments parseObject >>= done
--          , cachedComments $ \coms -> done (com coms AST_Void [])
--          ]

-- | This is the entry-point parser for 'DaoTT.Object.AST.AST_Script'.
--  parseScript :: DaoParser AST_Script
--  parseScript = dbg "parseScript" $ msum $
--    [ parseTopLevelComments AST_Comment
--    , parseWithLocation parseScriptTable
--    , parseWithLocation $ do
--        obj <- parseObject
--        cachedComments $ \coms -> expect "semicolon terminating object expression" $ do
--          takeToken Semicolon
--          return (AST_EvalObject obj coms)
--    ]

--  parseBracedScript :: DaoParser [Com AST_Script]
--  parseBracedScript = dbg "parseBracedScript" $ do
--    dbg "takeToken OpenBrace" $ takeToken OpenBrace
--    exprs <- fmap (map Com) (many parseScript)
--    dbg "takeToken CloseBrace" $ takeToken CloseBrace
--    return exprs

----------------------------------------------------------------------------------------------------

--  parseAttribute :: DaoParser AST_TopLevel
--  parseAttribute = msum $
--    [ parseTopLevelComments AST_TopComment
--    , parseWithLocation $ tokens [KeyREQUIRE, KeyIMPORT] $ do
--        tok <- shift
--        let typ = tokType tok
--            k = if typ==KeyREQUIRE then "require" else "imoprt"
--        expect ("string constant expression after "++k++" statement") $ do
--          str <- parseWithComments (fmap tokToUStr $ takeToken StrLit)
--          expect ("semicolon after "++k++" statement") $
--            takeToken Semicolon >> return (AST_Attribute (Com (ustr k)) str)
--    ]

--  parseTopLevel :: DaoParser AST_TopLevel
--  parseTopLevel = msum $
--    [ parseTopLevelComments AST_TopComment
--    , parseWithLocation (fmap AST_TopScript parseScript)
--    ]

--  topLevelKeywords :: DaoParser (Location -> AST_TopLevel)
--  topLevelKeywords = msum $
--    [ token KeyBEGIN    $ evt  "BEGIN"    BeginExprType
--    , token KeyEND      $ evt  "END"      EndExprType
--    , token KeyEXIT     $ evt  "EXIT"     ExitExprType
--    , token KeyFUNC     $ func "func"     FuncExprType
--    , token KeyFUNCTION $ func "function" FuncExprType
--    , token KeyPAT      $ rule "pattern"  PatExprType
--    , token KeyRULE     $ rule "rule"     RuleExprType
--    ]
--    where
--      evt str typ = expect ("script expression after "++str++" statement") $ do
--        script <- cachedComments (\coms -> fmap (\o -> com coms o []) parseBracedScript)
--        return (AST_Event typ script)
--      func str typ = msum $
--        [ do  args <- parseWithComments $ parseFuncParams parseObject
--              expect "braced script expression after function statement" $ do
--                fmap (AST_TopLambda typ args) parseBracedScript
--          -- ^ The keyword "function" followed not by a function name but by an argument list in
--          -- parenthesis is another way of declaring a top-level "pattern" expression.
--        , expect "name for function" $ do
--            name <- parseWithComments (fmap tokToUStr $ takeToken Label)
--            expect "argument list for function statement" $ do
--              args <- parseFuncParams $ parseWithLocation $
--                fmap (AST_Literal . ORef . LocalRef) (fmap tokToUStr $ takeToken Label)
--              expect "script expression for function statement" $ do
--                fmap (AST_TopFunc name args) (parseWithComments parseBracedScript)
--        ]
--      rule str typ = expect ("arguments list after "++str++" statement") $ do
--        args <- parseWithComments $ mplus (parseFuncParams parseObject) $
--          if typ==RuleExprType
--            then  fmap (:[]) $ parseWithLocation $ takeToken StrLit >>= \nm ->
--                    return (Com . AST_Literal (OString $ tokToUStr nm))
--            else  mzero
--        expect ("braced script expression after "++str++" statement") $
--          fmap (AST_TopLambda typ args) parseBracedScript

----------------------------------------------------------------------------------------------------

--  parseDaoScript :: DaoParser AST_SourceCode
--  parseDaoScript = do
--    attribs <- loopAttribs []
--    scripts <- loop []
--    return $
--      AST_SourceCode
--      { sourceModified = 0
--      , sourceFullPath = nil
--      , directives = attribs ++ scripts
--      }
--    where
--      loopAttribs got = mplus (parseAttribute >>= \a -> loopAttribs (got++[a])) (return got)
--      loop        got = do
--        skipSpaces
--        lines <- DaoParser (gets getLines)
--        (ax, continue) <- msum $
--          [ guardEOF >> return (got, False)
--          , parseTopLevel >>= \a -> return ([a], True)
--          , fail ("bad token at top level: "++show lines)
--          ]
--        let got' = got++ax
--        if continue then loop got' else return got'

----------------------------------------------------------------------------------------------------

--  daoCFGrammar :: CFGrammar AST_SourceCode
--  daoCFGrammar =
--    CFGrammar
--    { columnWidthOfTab = 4
--    , mainLexer        = daoMainLexer
--    , mainParser       = evalGenToSimParser parseDaoScript
--    }

testDaoLexer :: String -> IO ()
testDaoLexer = testLexicalAnalysis (tokenDBLexer daoTokenDB) 4

--  testDaoGrammar :: Show a => DaoParser a -> String -> IO ()
--  testDaoGrammar parser input =
--    case parse (daoCFGrammar{mainParser = evalGenToSimParser parser}) mempty input of
--      Backtrack -> putStrLn "Backtrack"
--      PFail err -> print err
--      OK    val -> print val

