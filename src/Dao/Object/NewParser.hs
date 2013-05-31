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
import           Dao.EnumSet
import           Dao.Predicate
import           Dao.NewParser
import qualified Dao.Tree as T

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
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

dbg :: String -> Parser a -> Parser a
dbg msg parser = do
  t <- optional (nextToken False)
  trace ("parse "++msg++", nextToken = "++show t) (return ())
  v <- catchPValue parser
  flip trace (return ()) $ case v of
    PFail err -> msg++" failed: "++show err
    Backtrack -> msg++" backtracked"
    OK      _ -> msg++" OK"
  assumePValue v

----------------------------------------------------------------------------------------------------

-- | The token types.
data TT
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
  deriving (Eq, Ord, Enum, Ix, Show)

lexNumber :: Lexer ()
lexNumber = error "TODO: define lexNumber"

-- | Shorthand for @'ignore' ('token' 'Spaces')@
skipSpaces :: Parser ()
skipSpaces = ignore (tokenType Spaces)

operator :: String -> Parser UStr
operator k = token (==InfixOp) (==k)

withKeyword :: (UStr -> Parser a) -> Parser a
withKeyword parser = withToken (==Label) parser

keyword :: String -> Parser UStr
keyword k = token (==Label) (==k)

optionStringType :: TT -> Parser String
optionStringType tok = fmap (fromMaybe "") (optional (fmap uchars (tokenType tok)))

----------------------------------------------------------------------------------------------------

data ParserState
  = ParserState
    { bufferedComments :: Maybe [Comment]
    , nonHaltingErrors :: [ParserErr]
    }
instance Monoid ParserState where
  mappend a b =
     b{ bufferedComments = bufferedComments a >>= \a -> bufferedComments b >>= \b -> return (a++b)
      , nonHaltingErrors = nonHaltingErrors a ++ nonHaltingErrors b
      }
  mempty = ParserState{ bufferedComments = Nothing, nonHaltingErrors = [] }

type Lexer          a = GenLexer                      TT a
type Parser         a = GenParser         ParserState TT a
type CFGrammar      a = GenCFGrammar      ParserState TT a
type ParserErr        = GenParserErr      ParserState TT
type ParseTable     a = GenParseTable     ParserState TT a
type ParseTableElem a = GenParseTableElem ParserState TT a

setCommentBuffer :: [Comment] -> Parser ()
setCommentBuffer coms = modifyUserState $ \st ->
  st{ bufferedComments = if null coms then mzero else return coms }

failLater :: String -> Parser ()
failLater msg = catchError (fail msg) $ \err ->
  modifyUserState $ \st -> st{nonHaltingErrors = nonHaltingErrors st ++ [err]}

----------------------------------------------------------------------------------------------------

maxYears :: Integer
maxYears = 9999

-- | This is the list of tokenizers used by 'lex' to break-up input string into parsable 'Token's.
daoLexers :: [Lexer ()]
daoLexers = 
  [ lexStringLiteral     StrLit
  , lexCharLiteral       CharLit
  , lexInlineC_Comment   ComInln
  , lexEndlineC_Comment  ComEndl
  , lexSpace             Spaces
  , dataSpecialLexer
  , lexDaoNumber
  , lexDaoKeyword
  , lexDaoOperator
  , lexOperator allUpdateOpStrs >> makeToken AssignOp
  , lexDaoParens
  ]

daoMainLexer :: Lexer ()
daoMainLexer = runLexerLoop "Dao script" lexEOF daoLexers

daoInfixOperators :: String
daoInfixOperators = concat [allArithOp2Strs, " ", allArithOp1Strs, " , : ; "]

lexDaoNumber :: Lexer ()
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

daoKeywordTokens :: [TT]
daoKeywordTokens =
  [ KeyIF, KeyELSE, KeyFOR, KeyIN, KeyWHILE, KeyWITH, KeyTRY, KeyCATCH
  , KeyCONTINUE, KeyBREAK, KeyRETURN, KeyTHROW
  , KeyDATA, KeySTRUCT, KeyLIST, KeySET, KeyINTMAP, KeyDICT, KeyARRAY, KeyDATE, KeyTIME
  , KeyGLOBAL, KeyLOCAL, KeyQTIME, KeySTATIC
  , KeyFUNCTION, KeyFUNC, KeyPAT, KeyRULE
  , KeyIMPORT, KeyIMPORT, KeyREQUIRE, KeyREQUIRE
  , KeyBEGIN, KeyEND, KeyEXIT
  ]

daoStringToKeywordMap :: M.Map String TT
daoStringToKeywordMap = M.fromList (zip daoKeywords daoKeywordTokens)

-- assumes keyword tokesn are consecutively enumerated
daoKeywordToStringArray :: Array TT String
daoKeywordToStringArray = array (KeyIF, KeyRULE) (zip daoKeywordTokens daoKeywords)

daoKeywordToString :: TT -> String
daoKeywordToString tok =
  if inRange (bounds daoKeywordToStringArray) tok then daoKeywordToStringArray!tok else ""

lexDaoKeyword :: Lexer ()
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
lexDaoOperator :: Lexer ()
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

lexDaoParens :: Lexer ()
lexDaoParens = msum $
  [ lexString "#{" >> makeEmptyToken OpenMeta
  , lexString "}#" >> makeEmptyToken CloseMeta
  , mk '(' OpenParen , mk ')' CloseParen
  , mk '[' OpenSquare, mk ']' CloseSquare
  , mk '{' OpenBrace , mk '}' CloseBrace
  ]
  where { mk c tok = lexChar c >> makeEmptyToken tok }

-- | One of the design goals of Dao is for its language to be able to express any of it's built-in
-- objects. Arbitrary data stored in 'Dao.Object.OBinary' objects are constructed from base-64
-- encoded tokens directly from the source file. This tokenizer accomodates for base-64 tokens by
-- looking for a "data" keyword and @{@ open-brace is seen, then switching to a special inner
-- tokenizer, producing a stream of tokens until a @}@ closing brace is seen, then control is
-- returned to the calling context.
dataSpecialLexer :: Lexer ()
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

----------------------------------------------------------------------------------------------------

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
parseNumberTabElems :: [ParseTableElem Object]
parseNumberTabElems =
  [ ptab Digits16 $ \str -> case uchars str of {'0':x:dx | charSet "Xx" x -> mk 16 dx}
  , ptab Digits2  $ \str -> case uchars str of {'0':b:dx | charSet "Bb" b -> mk 2  dx}
  , ptab Digits8  $ \str -> case uchars str of {'0':  dx                  -> mk 8  dx}
  , ptab Digits10 $ \str -> case uchars str of {dx                        -> mk 10 dx}
  ]
  where
    dots = fmap stripDots (optionStringType DotDigits10)
    exps = fmap stripEs   (optionStringType Exponent)
    optexps frac = mplus exps (return "") >>= \e -> return (frac, e)
    stripDots str = case str of {'.':str -> str; _ -> str}
    stripEs   str = case str of
      e:str     | e=='e' || e=='E' -> str
      '.':e:str | e=='e' || e=='E' -> str
      str                          -> str
    mk base int = do
      (frac, plusMinusExp) <-
        if base==10 then mplus (dots >>= optexps) (optexps "") else return ("", "")
      typ <- optionStringType NumSuffix
      numberFromStrs base int frac plusMinusExp typ

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
parseOptionalParens errMsg parser = flip mplus (skipSpaces >> parser) $ do
  tokenType OpenParen
  skipSpaces
  a <- parser
  skipSpaces
  expect ("close parenthesis after "++errMsg++" expression") (tokenType CloseParen >> return a)

-- | Parses a different form of 'ODiffTime', with days, hours, minutes, and seconds separated by
-- colons. This parser does not care about context, colons are used as @hour:minute:second@ separator
-- tokens, regardless of their meaning elsewhere. Spaces are not allowed between
-- @hour:minute:second@ tokens.
parseDiffTime :: Parser T_diffTime
parseDiffTime = do
  d <- tokenTypes [Digits10, Digits8]
  tokenType Colon
  flip mplus (fail "expecting diff-time expression") $ do
    (dx, miliseconds) <- loop [uchars d] 3
    case dx of
      [days, hours, minutes, seconds] -> mk days hours minutes seconds miliseconds
      [      hours, minutes, seconds] -> mk ""   hours minutes seconds miliseconds
      [             minutes, seconds] -> mk ""   ""    minutes seconds miliseconds
  where
    mk           = diffTimeFromStrs
    colon        = tokenType Colon
    stripDot str = case str of {'.':str -> str; _ -> str}
    loop  got i  = case i of
      i | i>0       ->
        flip mplus (return (got, "")) $ do
          d <- nextToken True
          let got' = got++[tokToStr d]
          case tokType d of
            tt | tt==Digits10 || tt==Digits8 -> flip mplus (return (got', "")) $ do
              s <- nextToken False
              case tokType s of
                DotDigits10 -> nextToken True >> return (got', tail (tokToStr s))
                Colon       -> nextToken True >> loop got' (i-1)
                _           -> return (got', "")
            td -> fail ("expecting digits for time value, got "++show td++" instead")
        | otherwise -> return (got, "")

-- | Compute diff times from strings representing days, hours, minutes, and seconds. The seconds
-- value may have a decimal point.
diffTimeFromStrs :: String -> String -> String -> String -> String -> Parser T_diffTime
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
    check :: String -> Integer -> String -> Parser Rational
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
parseTime :: Parser T_time
parseTime = do
  yyyy <- tokenTypes [Digits10, Digits8]
  expect "absolute time constant expression" $ do
    let dash = tokenType ArithNegOp
        digits = fmap uchars (tokenTypes [Digits10, Digits8])
        year = uchars yyyy
    month <- dash >> digits
    day   <- dash >> digits
    let commaSpace = -- a comma followed by an optional space OR no comma but a required space
          mplus (operator "," >> skipSpaces) (void $ tokenType Spaces)
    diffTime <- mplus (commaSpace >> parseDiffTime) (return (fromRational 0))
    zone <- msum $
      [  do commaSpace
            msum $
              [ fmap uchars (tokenType Label)
              , do  plus <- defaultTo "" (fmap uchars (mplus (operator "+") (operator "-")))
                    zone <- fmap uchars (tokenTypes [Digits10, Digits8])
                    let withColon h1 h2 = expect "valid time-zone offset" $ msum $
                          [ do  tokenType Colon
                                mm <- tokenTypes [Digits10, Digits8]
                                case uchars mm of
                                  [m1,m2] | (read [m1,m2] :: Int) < 60 -> return (plus++[h1,h2,m1,m2])
                                  _                                    -> mzero
                          , if (read [h1,h2] :: Int) > 24 then return [h1,h2] else mzero
                          ]
                        withoutColon h1 h2 m1 m2 = do
                          if (read [h1,h2] :: Int) < 24 && (read [m1,m2] :: Int) < 60
                            then  return (plus++[h1,h2,m1,m2])
                            else  fail ("timezone offset value "++zone++[h1,h2,m1,m2]++" is out-of-bounds")
                    case zone of
                      [h1,h2,m1,m2] -> withoutColon  h1 h2 m1 m2
                      [   h2,m1,m2] -> withoutColon '0' h2 m1 m2
                      [h1,h2      ] -> withColon  h1 h2
                      [   h2      ] -> withColon '0' h2
              ]
      , return ""
      ]
    return (addUTCTime diffTime (read (year ++ '-':month ++ '-':day ++ " 00:00:00" ++ zone)))

----------------------------------------------------------------------------------------------------

parseCommentsTable :: ParseTable [Comment]
parseCommentsTable = newParseTable $
  [ ptab ComInln (\com -> return [InlineComment  com])
  , ptab ComEndl (\com -> return [EndlineComment com])
  , ptab Spaces  (\ _  -> return [])
  ]

parseComments :: Parser [Comment]
parseComments = fmap concat $ many $ evalParseTable $ parseCommentsTable

-- | Use this to prevent constant re-parsing of comments. If you need just one set of comments and would like
-- to put it back if your parser fails, use this function. If you would like to try parsing an
-- expression with comments before and after it, use 'parseWithComments', which also does caching.
cachedComments :: ([Comment] -> Parser a) -> Parser a
cachedComments parser = do
  st   <- gets userState
  let notCached = parseComments >>= \coms -> setCommentBuffer coms >> return coms
  coms <- case bufferedComments st of
    Nothing   -> notCached
    Just []   -> notCached
    Just coms -> return coms
  a <- parser coms
  setCommentBuffer []
  return a

-- | Uses already-buffered comments, or parses more comments into the buffer in the 'ParserState',
-- then evaluates a sub-parser. If the sub-parser succeeds, the buffer is cleared and the comments
-- are used to create a wrapper around the result value of the sub-parser. If the sub-parser
-- backtracks, the comments are left in the buffer for another parser to have a try. See also
-- 'cachedComments' if you need only comments before an expression, not before and after.
parseWithComments :: Parser a -> Parser (Com a)
parseWithComments parser = do
  st   <- gets userState
  com1 <- case bufferedComments st of
    Nothing   -> parseComments >>= \com1 -> setCommentBuffer com1 >> return com1
    Just com1 -> return com1
  a    <- parser
  setCommentBuffer []
  com2 <- parseComments
  return (com com1 a com2)

parseTopLevelComments :: ([Comment] -> a) -> Parser a
parseTopLevelComments construct = dbg "parseTopLevelComments" $ cachedComments $ \coms ->
  if null coms then mzero else return (construct coms)

-- | Most nodes in the Dao abstract syntax tree take a 'Dao.Token.Location' as the final parameter.
-- This parser will take a sub-parser, store the cursor before and after running the sube parser,
-- constructing a 'Dao.Token.Location' from the before and after cursor positions. The sub-parser
-- must return a function that constructs some value (e.g. a node of the dao abstract syntax tree
-- with every sub-node filled in except for the 'Dao.Token.Location'). Then the location constructed
-- by this function passed as a parameter to the the node constructor returned by the sub-parser.
parseWithLocation :: Parser (Location -> a) -> Parser a
parseWithLocation parser = do
  (line1, col1) <- getCursor
  construct <- parser
  loc <- mplus (getCursor >>= \ (line2, col2) -> return (Location (fromIntegral line1) col1 (fromIntegral line2) col2))
               (return (atPoint line1 col1))
  return (construct loc)

parseFuncParams :: Parser AST_Object -> Parser [Com AST_Object]
parseFuncParams objParser = do
  tokenType OpenParen
  com1 <- parseComments
  mplus (close >> return [com com1 AST_Void []]) (loop com1 [])
  where
    close = tokenType CloseParen
    loop com1 got = expect "object expression for function parameter" $ do
      obj  <- objParser
      com2 <- parseComments
      let got' = got++[com com1 obj com2]
      expect "comma and next item in function parameters list, or closing parenthesis" $
        mplus (close >> return got')
              (operator "," >> parseComments >>= \com1 -> loop com1 got')

-- | The @date@ and @time@ functions work on objects expressed with a special syntax that
-- needs to be handled before trying any other parser. This function is intended to be used with
-- 'Dao.NewParser.eachWithKeyword' or 'Dao.NewParser.withToken', so the final parameter is not
-- necessary unless your function is already holding a 'Dao.NewParser.Label'.
parseSpecialFuncCall :: String -> Parser Object -> UStr -> Parser AST_Object
parseSpecialFuncCall key objParser nextKeyword = do
  guard (uchars nextKeyword == key)
  com1 <- parseComments
  let mk ox = return (AST_FuncCall (ustr key) com1 ox)
      altParseObject = flip mplus parseObject $
        parseWithLocation (fmap AST_Literal objParser)
  parseWithLocation $ msum $
    [ parseWithComments altParseObject >>= mk . (:[])
    , parseFuncParams   altParseObject >>= mk
    ]

----------------------------------------------------------------------------------------------------
-- Functions used for creating equations constructed from object expressions interleaved with infix
-- operators.

makeEquation :: [Either (Com Name) AST_Object] -> Parser AST_Object
makeEquation objx = case applyPrescedence objx of
  [Right obj] -> return obj
  _ ->  error $ ("unknown prescedence for operators:"++) $ concat $
          flip concatMap objx $ \obj -> case obj of
            Left obj -> [' ':uchars (unComment obj)]
            _        -> []

-- (Copied from the old parser "src/Dao/Object/Parser.hs")
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

-- (Copied from the old parser "src/Dao/Object/Parser.hs")
-- Given a list of operators, scans through an equation of the form
-- (Right exprA : Left op : Right exprB : ...) 
-- and if the 'op' is in the list of operators, the 'exprA' and 'exprB' are bound together into an
-- 'Dao.Object.AST_Equation' data structure. If 'op' is not in the list of operators, it is passed over.
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
-- Prescedence parsing: the following parsers define a prescedence for 'Dao.Object.AST.AST_Object'
-- expressions.

-- A function to create parser table elements for 'OpenParen' and 'OpenMeta'.
parenOrMeta
  :: TT -> TT
  -> (Com AST_Object -> Location -> AST_Object)
  -> ParseTableElem (Location -> AST_Object)
parenOrMeta open close construct = ptab open $ \ustr ->
  expect "object expression after open-parnethesis" $ do
    o <- parseWithComments parseObject
    expect "close-parenthesis" (tokenType close >> return (construct o))

-- Highest prescedence object expression parsers, parses string literals, integer literals, and
-- expressions enclosed in parentheses or "meta-eval" braces.
objectTabElemsPrec9 :: [ParseTableElem (Location -> AST_Object)]
objectTabElemsPrec9 = map (bindPTabElem (return . AST_Literal)) parseNumberTabElems ++
  [ parenOrMeta OpenParen CloseParen (AST_Paren True)
  , parenOrMeta OpenMeta  CloseMeta   AST_MetaEval
  , ptab StrLit  $ return . AST_Literal . ostr  . read . uchars
  , ptab CharLit $ return . AST_Literal . OChar . read . uchars
  , ptab Label   $ return . AST_Literal . ORef . LocalRef
  ]

objectExprPrec9 :: Parser (Location -> AST_Object)
objectExprPrec9 = evalParseTable $ newParseTable objectTabElemsPrec9

-- Extends 'objextTabElemsPrec8' to also parse reference and dereference prefix opreators.
objectTabElemsPrec8 :: [ParseTableElem (Location -> AST_Object)]
objectTabElemsPrec8 = objectTabElemsPrec9 ++ [ptab DerefOp (mk REF), ptab RefOp (mk DEREF)] where
  mk typ str = do
    obj <- parseWithComments (parseWithLocation objectExprPrec9)
    return (AST_Prefix typ obj)

objectExprPrec8 :: Parser (Location -> AST_Object)
objectExprPrec8 = evalParseTable $ newParseTable objectTabElemsPrec8

-- Parses objects (using 'objectExprPrec8') interleaved with the 'Dao.Object.DOT' (@.@) and
-- 'Dao.Object.POINT' (@->@) operators, prefix operators like 'Dao.Object.REF' (@@@) and
-- 'Dao.Object.DEREF' (@$@). This makes it suitable for parsing the parameter object expression for
-- functions like @global@ or @struct@ without requiring parentheses. Therefore expressions like the
-- following:
-- > global a + global b
-- will be parsed equivalently to the expression:
-- > (global a) + (global b)
parseRefEquation :: Parser AST_Object
parseRefEquation = init where
  init = do -- get one object and then try the loop
    obj <- parseWithLocation objectExprPrec8 -- if the loop backtracks on it's first item, return the object alone.
    mplus (parseWithLocation (loop [Right obj])) (return obj)
  loop got = flip mplus (makeEquation got >>= \obj -> return (setLocation obj)) $ do
    op <- parseWithComments $ withToken (==InfixOp) $ \op -> case uchars op of
      "."  -> return op
      "->" -> return op
      _    -> mzero
    expect ("object expression after "++uchars (unComment op)++" operator") $
      parseWithLocation objectExprPrec8 >>= \obj -> loop (got++[Left op, Right obj])

-- Extents objectTabElemsPrec8 with parsers of labels qualified with the keywords @global@, @local@,
-- @qtime@, or @static@.
objectTabElemsPrec7 :: [ParseTableElem (Location -> AST_Object)]
objectTabElemsPrec7 = objectTabElemsPrec8 ++
  [ ptab KeyGLOBAL qualifier
  , ptab KeyQTIME  qualifier
  , ptab KeyLOCAL  qualifier
  , ptab KeySTATIC qualifier
  ]
  where
    qualifier kw = parseRefEquation >>= \obj ->
      cachedComments $ \coms -> return (AST_FuncCall kw coms [Com obj])

objectExprPrec7 :: Parser (Location -> AST_Object)
objectExprPrec7 = evalParseTable $ newParseTable objectTabElemsPrec7

-- Parse object expressions indexed with a square-braced indexing epxression suffix.
objectSuffixed :: Parser (Location -> AST_Object)
objectSuffixed = do
  obj <- parseWithLocation objectExprPrec7
  flip mplus (return (const obj)) $ cachedComments $ \coms -> msum
    [ do  tokenType OpenSquare
          expect "object expression as an index value within square-brackets" $ do
            idx <- parseWithComments parseObject
            expect "expecting closing square-brackets" $ do
              tokenType CloseSquare
              return (AST_ArraySub obj coms idx)
    , do  tokenType OpenParen
          case obj of
            AST_Literal (ORef ref) _ -> case ref of
              LocalRef funcName -> expect "arguments for function call" $
                cachedComments $ \coms -> do
                  params <- parseFuncParams parseObject
                  return (AST_FuncCall funcName coms params)
              _ -> fail "function call to unsupported reference type"
                   -- TODO: make function calls to object expressions rather than plain names.
            _ -> fail "parenthetical expression after non-label"
    ]

-- Extends the 'objectTabElemsPrec8' table with parsers that create object expressions from
-- keywords, for example @date@, @time@, @list@, @set@, @dict@, @intmap@, @struct@, and @array@.
-- Also parses arithmetic negation and bitwise inversion prefix operators.
objectTabElemsPrec6 :: [ParseTableElem (Location -> AST_Object)]
objectTabElemsPrec6 = objectTabElemsPrec7 ++ 
  [ ptab KeyDICT   dictIntmap
  , ptab KeyINTMAP dictIntmap
  , ptab KeyLIST   listSet
  , ptab KeySET    listSet
  , ptab KeyDATA   $ \ _ -> cachedComments $ \coms ->
      expect "open brace containing base-64 data after \"data\" statement" $ do
        tokenType OpenBrace
        let loop got = do
              next <- fmap Com (tokenType Arbitrary)
              skipSpaces
              let got' = got++[next]
              mplus (tokenType CloseBrace >> return got') (loop got')
        got <- expect "base-64 data for \"data\" statement, or closnig brace" (loop [])
        skipSpaces >> return (AST_Data coms got)
  , ptab KeySTRUCT $ \ukey -> do
      let key = uchars ukey
          nobrace = do
            t <- nextToken False
            if tokType t == OpenBrace then mzero else return ()
      expect "optional item and required braced list of items to initialize struct" $ do
        init  <- parseWithComments (mplus (nobrace >> parseObject) (return AST_Void))
        items <- parseCommaSeparated key (assertAssignExpr key)
        return (AST_Struct init items)
  , ptab KeyARRAY  $ \ _ -> do
      bounds <- parseWithComments (parseFuncParams parseObject)
      expect "braced list of items to initialized array" $ do
        items  <- parseCommaSeparated "array" (\_ -> return ())
        return (AST_Array bounds items)
  , ptab KeyFUNC     $ parseLambdaExpr FuncExprType
  , ptab KeyFUNCTION $ parseLambdaExpr FuncExprType
  , ptab KeyPAT      $ parseLambdaExpr PatExprType
  , ptab KeyRULE     $ parseLambdaExpr RuleExprType
  , objLit KeyDATE ODiffTime parseDiffTime, objLit KeyTIME OTime parseTime
  , ptab ArithNegOp  $ prefixOp objectSuffixed NEG
  , ptab BinInvertOp $ prefixOp objectSuffixed INVB
  ]
  where
    objLit :: TT -> (a -> Object) -> Parser a -> ParseTableElem (Location -> AST_Object)
    objLit key constructor parser = ptab key (\ _ -> fmap (AST_Literal . constructor) parser)
    dictIntmap ukey = cachedComments $ \coms -> do
      let k = uchars ukey
      fmap (AST_Dict ukey coms) (parseCommaSeparated k (assertAssignExpr k))
    listSet    ukey = cachedComments $ \coms -> do
      let k = uchars ukey
      fmap (AST_Dict ukey coms) (parseCommaSeparated k (\_ -> return ()))
    prefixOp parseObj typ _ = fmap (AST_Prefix typ) (parseWithComments (parseWithLocation parseObj))

-- Used in 'objectExprPrec6', lambda expressions are expressions that begin with a keyword like
-- "func" or "rule".
parseLambdaExpr :: LambdaExprType -> UStr -> Parser (Location -> AST_Object)
parseLambdaExpr ftyp key = do
  params <- parseWithComments (parseFuncParams parseObject)
  expect ("braced script after \""++uchars key++"\" statement") $
    fmap (AST_Lambda ftyp params) parseBracedScript

objectExprPrec6 :: Parser (Location -> AST_Object)
objectExprPrec6 = evalParseTable $ newParseTable objectTabElemsPrec6

-- A general equation used to parse object expressions interleaved with infix operators. Used by
-- 'parseEqation' (indirectly via 'objectExprPrec5') and also by 'parseObject' directly.
parseInterleavedInfixOps :: TT -> Parser (Location -> AST_Object) -> Parser (Location -> AST_Object)
parseInterleavedInfixOps opType objParser = do
  let getObj = parseWithLocation objParser
  obj <- getObj
  let getOp = parseWithComments (tokenType opType)
      loop lastOp got = expect ("object expression after ("++uchars (unComment lastOp)++") operator") $ do
        obj <- getObj
        let got' = got++[Right obj]
        mplus (getOp >>= \op -> loop op (got'++[Left op])) (fmap const (makeEquation got'))
  mplus (getOp >>= \op -> loop op [Right obj, Left op]) (return (const obj))

-- Parses an equation with arithmetic and logic operators, excluding prefix logical NOT. Assignment
-- operators are not parsed with this parser, they are lower presedence.
objectExprPrec5 :: Parser (Location -> AST_Object)
objectExprPrec5 = parseInterleavedInfixOps InfixOp objectExprPrec6

-- Parses either a logical-NOT operator followed by an infix-operator expression, or just the
-- infix-operator expression alone. This is the lowest-prescedence non-assignment equation
-- expression. It does not need a parse table.
parseEquation :: Parser (Location -> AST_Object)
parseEquation = flip mplus objectExprPrec5 $ do
  tokenType LogicNotOp
  fmap (AST_Prefix NOT) (parseWithComments (parseWithLocation objectExprPrec5))

-- | This is the entry-point parser for 'Dao.Object.AST.AST_Object'. It parses the
-- lowest-prescedence object equation expression, which are assignment expressions.
parseObject :: Parser AST_Object
parseObject = parseWithLocation $ parseInterleavedInfixOps AssignOp parseEquation

-- used by parseDictObject and parseListObject
parseCommaSeparated :: String -> (AST_Object -> Parser ()) -> Parser [Com AST_Object]
parseCommaSeparated key check = expect ("opening-brace for "++key++" expression") $ do
  tokenType OpenBrace
  let done got = tokenType CloseBrace >> return got
      loop got = mplus (done got) $ do
        obj <- parseWithComments $ parseObject >>= \obj -> check obj >> return obj
        let got' = got++[obj]
        expect ("comma or closing-brace to denote elements of "++key++" expression") $
          mplus (done got') (tokenType Comma >> loop got')
  loop []

-- Passed to 'parseCommaSeparated' by "dict" and "intmap" expressions.
assertAssignExpr :: String -> AST_Object -> Parser ()
assertAssignExpr key o = case o of
  AST_Assign _ _ _ _ -> return ()
  _                  -> failLater $
    "element of "++key++" expression is not an assignment expression"

----------------------------------------------------------------------------------------------------

-- Parse any script expression starting with a keyword.
scriptTabElems :: [ParseTableElem (Location -> AST_Script)]
scriptTabElems =
  [ ptab KeyIF $ \ _ -> cachedComments $ \coms ->
      expect "conditional expression after \"if\" statement" $ do
        obj <- parseObject
        expect "braced script after \"if\" statement" $ do
          thn <- parseWithComments parseBracedScript
          let done = AST_IfThenElse coms obj thn
          msum $
            [ do  tokenType KeyELSE
                  fmap done $ parseWithComments $
                    expect "braced subscript or if statement after else statement" $ msum
                      [ do  t <- nextToken False -- look ahead for "if"
                            let next_is_if = tokType t == Label && uchars (tokToUStr t) == "if"
                            if next_is_if then parseScript >>= \s -> return [Com s] else mzero
                      , parseBracedScript
                      ]
            , return $ done (Com [])
            ]
  , ptab KeyTRY $ \ _ -> do
      tryScript <- parseWithComments parseBracedScript
      msum $
        [ do  tokenType KeyCATCH
              expect "variable name after \"catch\" statement" $ do
                nm <- parseWithComments (tokenType Label)
                expect "braced script after \"catch\" statement" $
                  fmap (AST_TryCatch tryScript nm) parseBracedScript
        , return (AST_TryCatch tryScript (Com nil) [])
        ]
  , ptab KeyELSE  $ \ _ -> fail "\"else\" statement not following an \"if\" statement"
  , ptab KeyCATCH $ \ _ -> fail "\"catch\" statement not following a \"try\" statement"
  , ptab KeyFOR   $ \ _ -> expect "iterator variable name after \"for\" statement" $ do
      nm  <- parseWithComments (tokenType Label)
      expect "\"in\" after \"for\" statement" $ do
        keyword "in"
        obj <- parseWithComments parseObject
        expect "braced script after \"for\" statement" $
          fmap (AST_ForLoop nm obj) parseBracedScript
  , ptab KeyWHILE $ \ _ -> expect "iterator expression after \"while\" statement" $ do
      obj <- parseWithComments parseObject
      expect "braced script after \"while\" statement" $
        fmap (AST_WhileLoop obj) parseBracedScript
  , ptab KeyCONTINUE $ continueBreak True
  , ptab KeyBREAK    $ continueBreak False
  , ptab KeyRETURN   $ throwReturn   True
  , ptab KeyTHROW    $ throwReturn   False
  , ptab KeyWITH     $ \ _ -> do
      obj <- parseWithComments parseObject
      expect "braced script after \"with\" statement" $
        fmap (AST_WithDoc obj) parseBracedScript
  ]
  where
    continueBreak isContinue key = msum $
      [ do  cachedComments $ \coms -> do
            tokenType KeyIF
            expect ("conditional expression after \""++uchars key++" if\" statement") $ do
              obj <- parseWithComments parseObject
              expect ("semicolon after \""++uchars key++"\" statement") $
                tokenType Semicolon >> return (AST_ContinueExpr isContinue coms obj)
      , expect ("semicolon after \""++uchars key++"\" statement") $ cachedComments $ \com -> do
          tokenType Semicolon
          return (AST_ContinueExpr isContinue com (Com AST_Void))
      ]
    throwReturn isReturn key = do
      let done obj = expect ("colon after \""++uchars key++"\" statement") $ 
            tokenType Semicolon >> return (AST_ReturnExpr isReturn obj)
      msum $
        [ parseWithComments parseObject >>= done
        , cachedComments $ \coms -> done (com coms AST_Void [])
        ]

parseScriptTable :: ParseTable (Location -> AST_Script)
parseScriptTable = newParseTable scriptTabElems

-- | This is the entry-point parser for 'Dao.Object.AST.AST_Script'.
parseScript :: Parser AST_Script
parseScript = dbg "parseScript" $ msum $
  [ parseTopLevelComments AST_Comment
  , parseWithLocation (evalParseTable parseScriptTable)
  , parseWithLocation $ do
      obj <- parseObject
      cachedComments $ \coms -> expect "semicolon terminating object expression" $ do
        tokenType Semicolon
        return (AST_EvalObject obj coms)
  ]

parseBracedScript :: Parser [Com AST_Script]
parseBracedScript = dbg "parseBracedScript" $ do
  dbg "tokenType OpenBrace" $ tokenType OpenBrace
  exprs <- fmap (map Com) (many parseScript)
  dbg "tokenType CloseBrace" $ tokenType CloseBrace
  return exprs

----------------------------------------------------------------------------------------------------

parseAttribute :: Parser AST_TopLevel
parseAttribute = msum $
  [ parseTopLevelComments AST_TopComment
  , parseWithLocation $ withTokenP $ \tok _ -> do
      guard (tok==KeyREQUIRE || tok==KeyIMPORT)
      let k = if tok==KeyREQUIRE then "require" else "imoprt"
      expect ("string constant expression after "++k++" statement") $ do
        str <- parseWithComments (tokenType StrLit)
        expect ("semicolon after "++k++" statement") $
          tokenType Semicolon >> return (AST_Attribute (Com (ustr k)) str)
  ]

parseTopLevel :: Parser AST_TopLevel
parseTopLevel = msum $
  [ parseTopLevelComments AST_TopComment
  , parseWithLocation topLevelTable
  , parseWithLocation (fmap AST_TopScript parseScript)
  ]

topLevelTabElems :: [ParseTableElem (Location -> AST_TopLevel)]
topLevelTabElems = 
  [ ptab KeyBEGIN    $ evt  "BEGIN"    BeginExprType
  , ptab KeyEND      $ evt  "END"      EndExprType
  , ptab KeyEXIT     $ evt  "EXIT"     ExitExprType
  , ptab KeyFUNC     $ func "func"     FuncExprType
  , ptab KeyFUNCTION $ func "function" FuncExprType
  , ptab KeyPAT      $ rule "pattern"  PatExprType
  , ptab KeyRULE     $ rule "rule"     RuleExprType
  ]
  where
  evt str typ _ = expect ("script expression after "++str++" statement") $ do
    script <- cachedComments (\coms -> fmap (\o -> com coms o []) parseBracedScript)
    return (AST_Event typ script)
  func str typ _ = msum $
    [ do  args <- parseWithComments $ parseFuncParams parseObject
          expect "braced script expression after function statement" $ do
            fmap (AST_TopLambda typ args) parseBracedScript
      -- ^ The keyword "function" followed not by a function name but by an argument list in
      -- parenthesis is another way of declaring a top-level "pattern" expression.
    , expect "name for function" $ do
        name <- parseWithComments (tokenType Label)
        expect "argument list for function statement" $ do
          args <- parseFuncParams $ parseWithLocation $
            fmap (AST_Literal . ORef . LocalRef) (tokenType Label)
          expect "script expression for function statement" $ do
            fmap (AST_TopFunc name args) (parseWithComments parseBracedScript)
    ]
  rule str typ _ = expect ("arguments list after "++str++" statement") $ do
    args <- parseWithComments $ mplus (parseFuncParams parseObject) $
      if typ==RuleExprType
        then  fmap (:[]) (parseWithLocation (tokenType StrLit >>= \nm -> return (Com . AST_Literal (OString nm))))
        else  mzero
    expect ("braced script expression after "++str++" statement") $
      fmap (AST_TopLambda typ args) parseBracedScript

topLevelTable :: Parser (Location -> AST_TopLevel)
topLevelTable = evalParseTable $ newParseTable $
  map (bindPTabElem (\a -> return (\loc -> AST_TopScript (a loc) loc))) scriptTabElems

----------------------------------------------------------------------------------------------------

parseDaoScript :: Parser AST_SourceCode
parseDaoScript = do
  attribs <- loopAttribs []
  scripts <- loop []
  return $
    AST_SourceCode
    { sourceModified = 0
    , sourceFullPath = nil
    , directives = attribs ++ scripts
    }
  where
    loopAttribs got = mplus (parseAttribute >>= \a -> loopAttribs (got++[a])) (return got)
    loop        got = do
      skipSpaces
      lines <- gets getLines
      (ax, continue) <- msum $
        [ parseEOF >> return (got, False)
        , parseTopLevel >>= \a -> return ([a], True)
        , fail ("bad token at top level: "++show lines)
        ]
      let got' = got++ax
      if continue then loop got' else return got'

----------------------------------------------------------------------------------------------------

daoCFGrammar :: CFGrammar AST_SourceCode
daoCFGrammar =
  GenCFGrammar
  { columnWidthOfTab = 4
  , mainLexer        = daoMainLexer
  , mainParser       = parseDaoScript
  }

testDaoLexer :: String -> IO ()
testDaoLexer = testLexicalAnalysis daoMainLexer 4

testDaoGrammar :: Show a => Parser a -> String -> IO ()
testDaoGrammar parser input = case parse (daoCFGrammar{mainParser=parser}) mempty input of
  Backtrack -> putStrLn "Backtrack"
  PFail err -> print err
  OK    val -> print val

