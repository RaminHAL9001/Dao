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

import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State

import           Data.Monoid
import           Data.List
import           Data.Char hiding (Space)
import           Data.Word
import qualified Data.Set  as S
import           Data.Ratio
import           Data.Complex
import           Data.Time.Clock
import           Numeric

import Debug.Trace

dbg :: String -> Parser a -> Parser a
dbg msg parser = do
  t <- nextToken False
  trace ("parse "++msg++", nextToken = "++show t) (return ())
  v <- catchPValue parser
  flip trace (return ()) $ case v of
    PFail err -> msg++" failed: "++show err
    Backtrack -> msg++" backtracked"
    OK      _ -> msg++" OK"
  assumePValue v

----------------------------------------------------------------------------------------------------

data ParserState
  = ParserState
    { bufferedComments :: Maybe [Comment]
    , nonHaltingErrors :: [StParserErr ParserState]
    }
instance Monoid ParserState where
  mappend a b =
     b{ bufferedComments = bufferedComments a >>= \a -> bufferedComments b >>= \b -> return (a++b)
      , nonHaltingErrors = nonHaltingErrors a ++ nonHaltingErrors b
      }
  mempty = ParserState{ bufferedComments = Nothing, nonHaltingErrors = [] }
type Parser a = StParser ParserState a
type CFGrammar a = StCFGrammar ParserState a

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
daoTokenizers :: [Tokenizer]
daoTokenizers = 
  [ lexStringLiteral
  , lexCharLiteral
  , lexInlineC_Comment
  , lexEndlineC_Comment
  , lexSpace
  , dataSpecialTokenizer
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

-- | One of the design goals of Dao is for its language to be able to express any of it's built-in
-- objects. Arbitrary data stored in 'Dao.Object.OBinary' objects are constructed from base-64
-- encoded tokens directly from the source file. This tokenizer accomodates for base-64 tokens by
-- looking for a "data" keyword and @{@ open-brace is seen, then switching to a special inner
-- tokenizer, producing a stream of tokens until a @}@ closing brace is seen, then control is
-- returned to the calling context.
dataSpecialTokenizer :: Tokenizer
dataSpecialTokenizer = do
  k <- lexKeyword
  case k of
    [tok] | uchars (tokToUStr tok) == "data" -> do
      got <- fmap ((k++) . concat) $ lexMany $ msum $
        [lexSpace, lexInlineC_Comment, lexEndlineC_Comment]
      flip mplus (return got) $ do
        got <- fmap (got++) (lexChar '{' >> makeToken Opener)
        let b64chars = unionCharP [isAlphaNum, (=='+'), (=='/'), (=='=')]
            isDone yesNo tok = makeToken tok >>= \t -> return (t, yesNo)
            loop got = do
              (tok, continue) <- msum $
                [ lexChar '}' >> isDone True Closer
                , lexSpace >>= \t -> return (t, True)
                , lexWhile b64chars >> isDone False Arbitrary
                , lexUntil (unionCharP [b64chars, isSpace, (=='}')]) >> isDone False Unknown
                , lexEOF >> return ([], False)
                ]
              let got' = got++tok
              if continue then loop got' else return got'
        loop got
    _ -> return k

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
parseOptionalParens errMsg parser = flip mplus (skipSpaces >> parser) $ do
  token (==Opener) (=="(")
  skipSpaces
  a <- parser
  skipSpaces
  expect ("close parenthesis after "++errMsg++" expression") (token (==Closer) (==")") >> return a)

-- | Parses a different form of 'ODiffTime', with days, hours, minutes, and seconds separated by
-- colons. This parser does not care about context, colons are used as @hour:minute:second@ separator
-- tokens, regardless of their meaning elsewhere. Spaces are not allowed between
-- @hour:minute:second@ tokens.
parseDiffTime :: Parser T_diffTime
parseDiffTime = marker $ withToken (==Digits) $ \d -> withToken (==Operator) $ \co -> do
  guard (uchars co == ":")
  flip mplus (fail "expecting diff-time expression") $ do
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
      i | i>0       ->
        flip mplus (return got) $ do
          d <- nextToken True
          let td = tokType d
          if td/=Number && td/=Digits
            then  fail ("expecting digits for time value, got "++show td++" instead")
            else  case uchars (tokToUStr d) of
                    '0':xb:_ | isAlpha xb -> fail "time values must be expressed in base-10"
                    _                     -> do
                      let got' = got++[tokToUStr d]
                      if td==Number
                        then  return got'
                        else  mplus (colon >> mplus (loop got' (i-1)) nocolonfail) (return got')
        | otherwise -> return got

-- | Compute diff times from strings representing days, hours, minutes, and seconds. The seconds
-- value may have a decimal point.
diffTimeFromStrs :: String -> String -> String -> String -> Parser T_diffTime
diffTimeFromStrs days hours minutes seconds = do
  days    <- check "days"    (maxYears*365) days
  hours   <- check "hours"             24   hours
  minutes <- check "minutes"           60   minutes
  let sec =  check "seconds"           60
  seconds <- case break (=='.') seconds of
    (_      , "."            ) ->
      fail "no digits after decimal point in seconds-value of time expression"
    (seconds, ""             ) -> sec seconds
    (seconds, '.':miliseconds) -> do
      seconds <- sec seconds
      return (seconds + rint miliseconds % (10 ^ length miliseconds))
    err -> fail "unexpeced WHAT"
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
            [ fmap uchars (tokenType Keyword)
            , do  plus <- optional "" (fmap uchars (mplus (operator "+") (operator "-")))
                  zone <- fmap uchars (tokenType Digits)
                  let withColon h1 h2 = expect "valid time-zone offset" $ msum $
                        [ do  operator ":"
                              mm <- tokenType Digits
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

parseComments :: Parser [Comment]
parseComments = loop [] where
  loop got = do
    let next c = loop (got++[c])
    msum $
      [ fmap InlineComment  (tokenType ComInln) >>= next
      , fmap EndlineComment (tokenType ComEndl) >>= next
      , tokenType Space >> loop got
      , return got
      ]

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
parseTopLevelComments construct = cachedComments $ \coms ->
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

-- | Parse any object that can be constructed from the token stream directly and does not require an
-- 'Dao.Object.ObjectExpr' in order to be evaluated.
parseSimpleObject :: Parser Object
parseSimpleObject = msum $
  [ parseNumber
  , tokenType StrLit  >>= \strLit  -> return (ostr  $ read $ uchars strLit )
  , tokenType CharLit >>= \charLit -> return (OChar $ read $ uchars charLit)
  ]

parseFuncParams :: Parser AST_Object -> Parser [Com AST_Object]
parseFuncParams objParser = do
  token (==Opener) (=="(")
  com1 <- parseComments
  mplus (close >> return [com com1 AST_Void []]) (loop com1 [])
  where
    close = token (==Closer) (==")")
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
-- necessary unless your function is already holding a 'Dao.NewParser.Keyword'.
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

-- | Given the name of the function uses it to parse an 'Dao.Object.AST.AST_FuncCall'. Also calls
-- 'parseSpecialFuncCall' so functions with special syntax are parsed correctly.
parseFuncCall :: UStr -> Parser AST_Object
parseFuncCall nextKeyword = msum $
  [ parseSpecialFuncCall "date" (fmap OTime     parseDate    ) nextKeyword
  , parseSpecialFuncCall "time" (fmap ODiffTime parseDiffTime) nextKeyword
  , parseWithLocation $ cachedComments $ \coms -> do
      params <- parseFuncParams parseObject
      return (AST_FuncCall nextKeyword coms params)
  ]

----------------------------------------------------------------------------------------------------

-- Prescedence parsing: the following parsers define a prescedence for 'Dao.Object.AST.AST_Object'
-- expressions.

daoKeywords :: S.Set Name
daoKeywords = S.fromList $ map ustr $ words $ concat $
  [ " func function rule pattern pat "
  , " if else try catch for in while continue break return throw "
  , " list set dict intmap struct data "
  ]

parseNonKeyword :: Parser UStr
parseNonKeyword = token (==Keyword) (\e -> not $ S.member (ustr e) daoKeywords)

-- | A unit object is any object expression that could be used as a part of a multi-part object
-- expression, e.g. an equation contains multiple object units. This is important for deciding
-- prescedence in equations. Consider unit objects of the highest prescedence. It is safe to call
-- 'parseUnitObject' from any lower-prescedence parser without causing a backtracking loop.
parseUnitObject :: Parser AST_Object
parseUnitObject = parseWithLocation $ msum $
  [ do  open <- fmap uchars (tokenType Opener)
        let (construct, close) = case open of
              "("  -> (AST_Paren True, ")")
              "#{" -> (AST_MetaEval, "}#")
        expect "object expression after open-parnethesis" $ do
          o <- parseWithComments parseObject
          expect "close-parenthesis" (token (==Closer) (==close) >> return (construct o))
  , fmap AST_Literal parseSimpleObject
  , withKeyword $ \ukey -> let k = uchars ukey in case k of
      k | k=="dict" || k=="intmap" -> cachedComments $ \coms ->
        fmap (AST_Dict ukey coms) (parseCommaSeparated k (assertAssignExpr k))
      k | k=="list" || k=="set"    -> cachedComments $ \coms ->
        fmap (AST_Dict ukey coms) (parseCommaSeparated k (\_ -> return ()))
      "data"    ->
        cachedComments (\coms -> open >> loop [] >>= \items -> return (AST_Data coms items)) where
          open     = token (==Opener) (=="{")
          close    = skipSpaces >> token (==Closer) (=="}")
          loop got = do
            skipSpaces
            next <- fmap Com (tokenType Arbitrary)
            let got' = got++[next]
            mplus (close >> return got') (loop got')
      "struct" -> do
        let key = uchars ukey
            nobracket = do
              t <- nextToken False
              if tokType t == Opener && uchars (tokToUStr t) == "{" then mzero else return ()
        guard (key == "struct")
        expect "optional item and required bracketed list of items to initialize struct" $ do
          init  <- parseWithComments (mplus (nobracket >> parseObject) (return AST_Void))
          items <- parseCommaSeparated key (assertAssignExpr key)
          return (AST_Struct init items)
      "array"  -> do
        bounds <- parseWithComments (parseFuncParams parseObject)
        expect "bracketed list of items to initialized array" $ do
          items  <- parseCommaSeparated "array" (\_ -> return ())
          return (AST_Array bounds items)
      "func"     -> parseLambdaExpr k FuncExprType
      "function" -> parseLambdaExpr k FuncExprType
      "rule"     -> parseLambdaExpr k RuleExprType
      "pattern"  -> parseLambdaExpr k PatExprType
      k | k=="global" || k=="local" || k=="static" || k=="qtime" -> cachedComments $ \coms -> do
        obj <- parseRefEquation
        return (AST_FuncCall ukey coms [Com obj])
      k -> msum $
        [ fmap const (parseFuncCall ukey)
        , return (AST_Literal (ORef (LocalRef ukey)))
        ]
  ]

-- | This is a parser that parses either a plain unit object using 'parseUnitObject', or
-- alternatively, a prefix operator followed by a plain unit object. Supply a predicate that returns
-- true for every prefix operator you need.
parsePrefixedObject :: Parser AST_Object
parsePrefixedObject = withToken (==Operator) $ \ustr -> case readsPrec 0 (uchars ustr) of
  [(op, "")] -> parseWithLocation $
    parseWithComments parseUnitObject >>= \obj -> return (AST_Prefix op obj)
  _                         -> parseUnitObject

-- | This is an alternative to 'parseObject'. It parses expressions of lower prescedence than
-- 'parseUnitObject' but of higher than a typical equation parsed with 'parseObject'. Specifically,
-- it parses unit objects (using 'parseUnitObject') interleaved with the 'Dao.Object.DOT' (@.@) and
-- 'Dao.Object.POINT' (@->@) operators, and also prefix operators 'Dao.Object.REF' (@@@) and
-- 'Dao.Object.DEREF' (@$@). This makes it suitable for parsing the parameter object expression for
-- functions like @global@ or @struct@ without requiring parentheses in situations like the
-- following:
-- > global a + global b
-- will be parsed as:
-- > (global a) + (global b)
parseRefEquation :: Parser AST_Object
parseRefEquation = parsePrefixedObject >>= \obj -> parseWithLocation (loop [Right obj]) where
  loop got = flip mplus (makeEquation got >>= \obj -> return (setLocation obj)) $ do
    op <- parseWithComments $ withToken (==Operator) $ \op -> case uchars op of
      "."  -> return op
      "->" -> return op
      _    -> mzero
    expect ("object expression after "++uchars (unComment op)++" operator") $
      parsePrefixedObject >>= \obj -> loop (got++[Left op, Right obj])

-- | This is the entry-point parser for 'Dao.Object.AST.AST_Object'.
parseObject :: Parser AST_Object
parseObject = parseWithLocation $ do
  obj <- parsePrefixedObject
  msum $
    [ cachedComments $ \com1 -> do
        token (==Opener) (=="[")
        expect "object expression for array subscript index" $ do
          idx <- parseWithComments parseObject
          expect "closing bracket for array subscript expression" $ do
            token (==Closer) (=="]")
            return (AST_ArraySub obj com1 idx)
    , do  let loop got = flip mplus (return got) $ do
                op <- parseWithComments $ withToken (==Operator) $ \op -> case uchars op of
                  ";" -> mzero
                  "," -> mzero
                  _   -> return op
                expect ("object after infix operator ("++show (unComment op)++")") $
                  parseObject >>= \obj -> loop (got++[Left op, Right obj])
          obj <- loop [Right obj] >>= makeEquation
          return (setLocation obj)
    ]

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
      then  scanBind constructor ops $
             (Right (constructor a (fmap (read . uchars) op) b LocationUnknown) : objx) -- "bind" the operands to it
      else  Right a : Left op : -- otherwise ignore this operator
              scanBind constructor ops (Right b : objx)
  objx -> error ("scanBind failed:\n"++show objx)

-- used by parseDictObject and parseListObject
parseCommaSeparated :: String -> (AST_Object -> Parser ()) -> Parser [Com AST_Object]
parseCommaSeparated key check = expect ("opening-brace for "++key++" expression") $ do
  token (==Opener) (=="{")
  let done got = token (==Closer) (=="}") >> return got
      loop got = mplus (done got) $ do
        obj <- parseWithComments (marker (parseObject >>= \obj -> check obj >> return obj))
        let got' = got++[obj]
        expect ("comma or closing-brace to denote elements of "++key++" expression") $
          mplus (done got') (token (==Operator) (==",") >> loop got')
  loop []

assertAssignExpr :: String -> AST_Object -> Parser ()
assertAssignExpr key o = case o of
  AST_Assign _ _ _ _ -> return ()
  _                  -> failLater $
    "element of "++key++" expression is not an assignment expression"

parseLambdaExpr :: String -> LambdaExprType -> Parser (Location -> AST_Object)
parseLambdaExpr key ftyp = do
  params <- parseWithComments (parseFuncParams parseObject)
  expect ("bracketed script after \""++key++"\" statement") $
    fmap (AST_Lambda ftyp params) parseBracketedScript

----------------------------------------------------------------------------------------------------

parseScript :: Parser AST_Script
parseScript = parseWithLocation $ msum $
  [ fmap const (parseTopLevelComments AST_Comment)
  , withKeyword $ \ukey -> case uchars ukey of
      "if"       -> cachedComments $ \coms -> expect "conditional expression after \"if\" statement" $ do
        obj <- parseObject
        expect "bracketed script after \"if\" statement" $ do
          thn <- parseWithComments parseBracketedScript
          let done = AST_IfThenElse coms obj thn
          msum $
            [ do  keyword "else"
                  fmap done $ parseWithComments $
                    expect "bracketed subscript or if statement after else statement" $ msum
                      [ do  t <- nextToken False -- look ahead for "if"
                            let next_is_if = tokType t == Keyword && uchars (tokToUStr t) == "if"
                            if next_is_if then parseScript >>= \s -> return [Com s] else mzero
                      , parseBracketedScript
                      ]
            , return $ done (Com [])
            ]
      "try"      -> do
        tryScript <- parseWithComments parseBracketedScript
        msum $
          [ do  keyword "catch"
                expect "variable name after \"catch\" statement" $ do
                  nm <- parseWithComments parseNonKeyword
                  expect "bracketed script after \"catch\" statement" $
                    fmap (AST_TryCatch tryScript nm) parseBracketedScript
          , return (AST_TryCatch tryScript (Com nil) [])
          ]
      "else"     -> fail "\"else\" statement not following an \"if\" statement"
      "catch"    -> fail "\"catch\" statement not following a \"try\" statement"
      "for"      -> expect "iterator variable name after \"for\" statement" $ do
        nm  <- parseWithComments parseNonKeyword
        expect "\"in\" after \"for\" statement" $ do
          keyword "in"
          obj <- parseWithComments parseObject
          expect "bracketed script after \"for\" statement" $
            fmap (AST_ForLoop nm obj) parseBracketedScript
      "while"    -> expect "iterator expression after \"while\" statement" $ do
        obj <- parseWithComments parseObject
        expect "bracketed script after \"while\" statement" $
          fmap (AST_WhileLoop obj) parseBracketedScript
      key | key=="continue" || key=="break" -> msum $
        [ do  cachedComments $ \coms -> do
              keyword "if"
              expect ("conditional expression after \""++key++" if\" statement") $ do
                obj <- parseWithComments parseObject
                expect ("semicolon after \""++key++"\" statement") $
                  operator ";" >> return (AST_ContinueExpr (key=="continue") coms obj)
        , expect ("semicolon after \""++key++"\" statement") $ cachedComments $ \com -> do
            operator ";"
            return (AST_ContinueExpr (key=="continue") com (Com AST_Void))
        ]
      key | key=="return"   || key=="throw" -> do
        let done obj = expect ("colon after \""++key++"\" statement") $ 
              operator ";" >> return (AST_ReturnExpr (key=="return") obj)
        msum $
          [ parseWithComments parseObject >>= done
          , cachedComments $ \coms -> done (com coms AST_Void [])
          ]
      "with"     -> do
        obj <- parseWithComments parseObject
        expect "bracketed script after \"with\" statement" $
          fmap (AST_WithDoc obj) parseBracketedScript
      _          -> mzero
  , do  obj  <- parseObject
        cachedComments $ \coms ->
          expect "semicolon" (operator ";" >> return (AST_EvalObject obj coms))
  ]

parseBracketedScript :: Parser [Com AST_Script]
parseBracketedScript = token (==Opener) (=="{") >> loop [] where
  loop got = msum $
    [ parseTopLevelComments (Com . AST_Comment) >>= loop . (\c -> got++[c])
    , token (==Closer) (=="}") >> return got
    , expect "script expression" (parseScript >>= \script -> loop (got++[Com script]))
    ]

----------------------------------------------------------------------------------------------------

parseAttribute :: Parser AST_TopLevel
parseAttribute = parseWithLocation $ msum $
  [ fmap const (parseTopLevelComments AST_TopComment)
  , withKeyword $ \ukey -> do
      let k = uchars ukey
      guard (k=="require" || k=="requires" || k=="import" || k=="imports")
      expect ("string constant expression after "++k++" statement") $ do
        str <- parseWithComments (fmap (ustr . concatMap uchars) (loop []))
        expect ("semicolon after "++k++" statement") $
          operator ";" >> return (AST_Attribute (Com ukey) str)
  ]
  where
    loop got = do
      next <- tokenTypes [StrLit, Keyword, Operator]
      let got' = got++[next]
      flip mplus (return got') $ withToken (==Operator) $ \op ->
        if uchars op == ";" then mzero else loop (got' ++[op])

parseTopLevel :: Parser AST_TopLevel
parseTopLevel = parseWithLocation $ msum $
  [ fmap const (parseTopLevelComments AST_TopComment)
  , withKeyword $ \ukey -> case uchars ukey of
      k | k=="BEGIN" || k=="END" || k=="EXIT" -> case readsPrec 0 k of
        [(typ, "")] -> expect ("script expression after "++k++" statement") $ do
          script <- cachedComments (\coms -> fmap (\o -> com coms o []) parseBracketedScript)
          return (AST_Event typ script)
        _ -> mzero
      k | k=="func" || k=="function" -> expect "name for function" $ do
        name <- parseWithComments (tokenType Keyword)
        expect "argument list for function statement" $ do
          args <- parseFuncParams $ parseWithLocation $
            fmap (AST_Literal . ORef . LocalRef) (tokenType Keyword)
          expect "script expression for function statement" $ do
            fmap (AST_TopFunc name args) (parseWithComments parseBracketedScript)
      k | k=="rule" || k=="pattern"  -> case readsPrec 0 k of
        [(typ, "")] -> expect ("arguments list after "++k++" statement") $ do
          args <- parseWithComments $ mplus (parseFuncParams parseObject) $
            if k=="rule"
              then  fmap (:[]) (parseWithLocation (tokenType StrLit >>= \nm -> return (Com . AST_Literal (OString nm))))
              else  mzero
          expect ("bracketed script expression after "++k++" statement") $
            fmap (AST_TopLambda typ args) parseBracketedScript
        _ -> mzero
      _ -> mzero
  , fmap AST_TopScript parseScript
  ]

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
    loop        got = msum $
      [ parseEOF >> return got
      , parseTopLevel >>= \a -> loop (got++[a])
      , fail "bad token at top level"
      ]

----------------------------------------------------------------------------------------------------

daoCFGrammar :: CFGrammar a
daoCFGrammar =
  GenCFGrammar
  { columnWidthOfTab = 4
  , tokenizers       = daoTokenizers
  , mainParser       = error "daoCFGrammar mainParser is not defined"
  }

testDaoGrammar :: Show a => Parser a -> String -> IO ()
testDaoGrammar parser input = case parse (daoCFGrammar{mainParser=parser}) mempty input of
  Backtrack -> putStrLn "Backtrack"
  PFail err -> print err
  OK    val -> print val

