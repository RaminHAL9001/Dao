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

parseListable :: String -> Char -> Char -> Char -> Parser a -> Parser [Com a]
parseListable msg open delim close getValue = char open >> loop [] where
  loop zx = mplus (char close >> return zx) $ expect msg $ \before -> do
    value <- getValue
    after <- parseComment
    regexMany space
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
    then fail ("cannot use keyword as "++msg)
    else ok (ustr name)

parseDotName :: Parser [Name]
parseDotName = loop [] where
  loop zx = flip mplus (return zx) $ do
    beginTokenIf (char '.')
    mplus (endToken >> parseName "global reference" >>= \z -> loop (zx++[z])) backtrack

parseLocalGlobal :: Parser Reference
parseLocalGlobal = mplus (fmap GlobalRef parseDotName) (fmap LocalRef (parseName "variable name"))

----------------------------------------------------------------------------------------------------

-- | A 'KeywordComment' parser is a parser that starts by looking for a keyword, then parses an
-- expression based on that keyword. It is left-factored, so before calling a function of this type,
-- the keyword and first comment after the keyword must both be parsed by the calling context and
-- passed to this function. The result is, many parsers of this type can be placed together in a
-- single 'Control.Monad.msum' list, and each parser will be tried in turn but will not need to
-- backtrack to the initial keyword, because the initial keyword and comment was parsed for it by
-- the calling context.
type NameComParser a = String -> [Comment] -> Parser a

-- | Like NameComParser but takes an initial 'Dao.Object.ObjectExpr' and comment as it's parameters.
type ObjComParser a = ObjectExpr -> [Comment] -> Parser a

-- | Construct a 'NameComParser' by passing it a keyword and the function used to act on the
-- keyword. For example:
-- @'endStatement = 'guardKeyword' "end" (\comment -> return (EndStmt comment))@
-- The 'NameComParser' takes a second 'Prelude.String' and @['Dao.Object.Comment']@ which are
-- expected to be passed by the calling context. The string given will be checked against the
-- keyword, if it matches, the comment received by the calling context is passed to the given
-- parser, otherwise, it evaluates to 'Control.Monad.mzero', hence it is a guard function.
guardKeyword :: String -> ([Comment] -> Parser a) -> NameComParser a
guardKeyword requireKey par key com = guard (key==requireKey) >> par com

-- | Create a new 'NameComParser' by applying it's own input arguments to a list of other
-- 'NameComParser's and then passing this list to 'Control.Monad.msum', which will evaluate each
-- parser in turn and evaluate to the first parser to succeed, hence the name "choice".
nameComParserChoice :: [NameComParser a] -> NameComParser a
nameComParserChoice choices initString comment = msum $
  map (\choice -> choice initString comment) choices

-- | A parser that fails completely if it does not meet the given "expectation" parser. This
-- function takes an error message, and a parser function which takes a [Comment] as it's parameter.
-- This function parses the comment, ignores trailing whitespace, then evaluates the "expectation"
-- parser with the comment. If the "expectation" parser fails, the error message is used to
-- 'Control.Monad.fail' the whole parser. This parsing pattern is so common that it warrants it's
-- own function.
expect :: String -> ([Comment] -> Parser a) -> Parser a
expect msg expectation =
  parseComment >>= \com -> regexMany space >> mplus (expectation com) (fail msg)

----------------------------------------------------------------------------------------------------

-- | This is the "entry point" for parsing a 'Dao.Object.ScriptExpr'.
parseScriptExpr :: Parser ScriptExpr
parseScriptExpr = do
  key  <- parseKeywordOrName
  com1 <- parseComment
  mplus (nameComParserChoice choices key com1) $ do
    beginToken
    objExpr <- keywordObjectExpr key com1
    expect "does not evaluate to a valid command" $ \com2 -> 
      endToken >> objectExprStatement objExpr com2
  where
    choices =
      [ ifStatement, tryStatement, forStatement, withStatement
      , continueStatement, returnStatement
      , elseStatement, catchStatement
      ]

parseBracketedScript :: Parser [Com ScriptExpr]
parseBracketedScript = beginTokenIf (char '{') >>= \com1 -> loop [] where
  loop zx = mplus (regexMany space >> char '}' >> endToken >> return zx) $ do
    com1 <- parseComment
    expr <- regexMany space >> parseScriptExpr
    com2 <- parseComment
    loop (zx++[com com1 expr com2])

expected_sub_for msg = fail $
  "expecting bracketed sub-script expression for body of \""++msg++"\" statement"

ifStatement :: NameComParser ScriptExpr
ifStatement = guardKeyword "if" loop where
  loop com1 = do
    beginToken
    objExpr <- mplus parseObjectExpr $
      fail "expecting object expression for condition of \"if\" statement"
    case objExpr of
      ParenExpr objExpr -> do
        endToken
        expect (expected_sub_for "if") $ \com2 -> do
          thenStmt <- parseBracketedScript
          let done com3 com4 elseStmt = return $
                IfThenElse (com com1 (unComment objExpr) com2) (com [] thenStmt com3) (com com4 elseStmt [])
          com3 <- parseComment
          flip mplus (done com3 [] []) $ do
            string "else"
            com4 <- parseComment
            regexMany space 
            msum $
              [ string "if" >> parseComment >>= loop >>= done com3 com4 . (:[]) . Com
              , parseBracketedScript >>= done com3 com4
              , fail (expected_sub_for "else")
              ]
      _ -> fail "conditional expression must be in parentheses"

tryStatement ::NameComParser ScriptExpr
tryStatement = guardKeyword "try" $ \com1 -> do
  tryStmt <- mplus parseBracketedScript (fail (expected_sub_for "try"))
  let done com2 com3 name com4 catchStmt = return $
        TryCatch (com com1 tryStmt []) (com com2 name com3) (com com4 catchStmt [])
  mplus (done [] [] nil [] []) $ do
    com2 <- parseComment
    string "catch"
    regexMany space
    expect "\"catch\" statement must be followed by a variable name" $ \com3 -> do
      name <- parseName "the name of the \"catch\" variable"
      expect (expected_sub_for "catch") $ \com4 -> do
        catchStmt <- parseBracketedScript
        done com2 com3 name com4 catchStmt

forStatement :: NameComParser ScriptExpr
forStatement = guardKeyword "for" $ \com1 -> do
  name <- mplus (parseName "the name of the \"for\" variable") $
    fail "\"for\" statement must be followed by a variable name"
  expect "expecting \"in\" statement" $ \com2 -> do
    string "in"
    expect "expecting object expression over which to iterate in \"for\" statement" $ \com3 -> do
      iterExpr <- parseObjectExpr
      expect "expecting bracketed sub-script expression for body of \"for\" statement" $ \com4 -> do
        forStmt <- parseBracketedScript
        return (ForLoop (com com1 name com2) (com com3 iterExpr []) (com com4 forStmt []))

withStatement :: NameComParser ScriptExpr
withStatement = guardKeyword "with" $ \com1 -> do
  withObjExpr <- mplus parseObjectExpr $
    fail "expecting object expression after \"with\" statement"
  expect (expected_sub_for "with") $ \com2 -> do
    with <- parseBracketedScript
    return (WithDoc (com com1 withObjExpr []) (com com2 with []))

returnStatement :: NameComParser ScriptExpr
returnStatement key com1 = mplus retur thro where
  retur = fn "return" key com1
  thro  = fn "throw"  key com1
  fn key = guardKeyword key $ \com1 -> do
    objExpr <- mplus parseObjectExpr $
      fail ("expecting object expression for \""++key++"\" statement")
    expect ("\""++key++"\" statement must be terminated with a simicolon \";\" character") $ \com2 ->
      char ';' >> return (ReturnExpr (Com (key=="return")) (com com1 objExpr com2) (Com ()))

continueStatement :: NameComParser ScriptExpr
continueStatement key com1 = mplus break continue where
  break    = fn "break"    key com1
  continue = fn "continue" key com1
  fn str = guardKeyword str $ \com1 -> do
    expect ("expecting \";\" or \"if(expression)\" after \""++str++"\" statement") $ \com1 -> do
      let done com2 expr com3 =
            return (ContinueExpr (com [] (str=="continue") com1) (com com2 expr com3) (Com ()))
      mplus (char ';' >> done [] (Literal (Com ONull)) []) $ do
        string "if"
        expect ("expecting object expression after \""++str++"\" statement") $ \com2 -> do
          objExpr <- parseObjectExpr
          expect ("\""++str++"\" statement must be terminated with a simicolon \";\" character") $ \com3 ->
            char ';' >> done com2 objExpr com3

-- fails immediately, else and catch statements are parsed only after if and try statements.
badStatement :: String -> String -> NameComParser ScriptExpr
badStatement str msg key = flip (guardKeyword str) key $ do
  fail ("\""++str++"\" statement must be preceeded by a valid \""++msg++"\" statement")

elseStatement  = badStatement "else" "if"
catchStatement = badStatement "catch" "try"

-- | This function takes an 'Dao.Object.ObjectExpr' and checks if it can be used as a statement. It
-- then checks for a trailing simicolon or newline. Function calls with side-effects and assignment
-- expressions can be used as stand-alone script expressions and will evaluate to a
-- 'Dao.Object.ScriptExpr', whereas equations and literal expressions cannot, and will evaluate to a
-- syntax error.
objectExprStatement :: ObjComParser ScriptExpr
objectExprStatement initExpr com1 = loop (Com initExpr) where
  done = do -- parse required terminating semicolon.
    regexMany space
    endline <- zeroOrOne (rxChar ';')
    if null endline
      then parseEquation initExpr com1 >>= flip objectExprStatement []
      else return (EvalObject (com [] initExpr com1))
  loop expr = case unComment expr of
    AssignExpr _ _   -> done
    FuncCall   _ _   -> done
    LambdaCall _ _ _ -> done
    ParenExpr  expr  -> loop expr
    _ -> fail "cannot use an object expression as a statement"

----------------------------------------------------------------------------------------------------

-- This is the "entry point" for parsing 'Dao.Object.ObjectExpr's.
parseObjectExpr :: Parser ObjectExpr
parseObjectExpr = msum $
  [ do -- parse an expression enclosed in parentheses
        char '('
        com1 <- parseComment
        expr <- regexMany space >> parseObjectExpr
        expect "missing close parenthases" $ \com2 ->
          char ')' >> return (ParenExpr (com com1 expr com2))
  , do -- low-prescedence unary operators, these are interpreted as 'FuncCall's.
        op   <- regex (rxCharSetFromStr "@$")
        expect ("\""++op++"\" operator must be followed by an object expression") $ \com1 -> do
          expr <- parseObjectExpr
          return (FuncCall (Com (ustr op)) (com com1 [Com expr] []))
  , do -- parse an object expression that starts with a keyword or name.
        key  <- parseKeywordOrName
        com1 <- parseComment
        regexMany space 
        mplus (keywordObjectExpr key com1) $ -- if "key" isn't a keyword, treat it as a LocalRef
          parseEquation (Literal (com com1 (ORef (LocalRef (ustr key))) [])) []
  ]

-- Here we collect all of the ObjectExpr parsers that start by looking for a keyword
keywordObjectExpr :: NameComParser ObjectExpr
keywordObjectExpr key com1 = msum $ map (\parser -> parser key com1) choices where
  choices = [parseReference, parseLambdaCall, parseContainerObject]

-- This need to be the last parser in a list of choices that parse an initial keyword because 
parseReference :: NameComParser ObjectExpr
parseReference key com1 = case key of
  "local"  -> makeref True  LocalRef  e "local variable name"
  "static" -> makeref True  StaticRef e "local variable name for static reference"
  "global" -> makeref False e GlobalRef "global variable name"
  "qtime"  -> makeref False e QTimeRef  "global variable name for query-time reference"
  _        -> mzero
  where
    e = undefined
    makeref typ nameCo namexCo msg = do -- 'typ'=True for LocalRef, 'typ'=False for GlobalRef
      beginToken
      ref <- parseLocalGlobal
      let done co r = endToken >> parseEquation (Literal (com com1 (ORef (co r)) [])) []
      case ref of
        LocalRef  r | typ     -> done nameCo r
        GlobalRef r | not typ -> done namexCo r
        _                     -> fail ("expecting "++msg)

parseLambdaCall :: NameComParser ObjectExpr
parseLambdaCall = guardKeyword "call" $ \com1 ->
  expect "expecting an expression after \"call\" statement" $ \com2 -> do
    objExpr <- parseObjectExpr
    expect "expecting a tuple containing arguments to be passed to the \"call\" statement" $ \com3 -> do
      argv <- parseListable "argument for function call" '(' ',' ')' parseObjectExpr
      return (LambdaCall (Com ()) (com com1 objExpr com2) (com com3 argv []))

-- Parses objects that are complex expressions, like sets, lists, dicts, intmaps, and arrays.
parseContainerObject :: NameComParser ObjectExpr
parseContainerObject = nameComParserChoice [parseLambdaDef]

parseLambdaDef :: NameComParser ObjectExpr
parseLambdaDef key com1 = do
  guard (key=="func" || key=="function")
  expect "list of parameter variables after \"function\" statement" $ \com2 -> do
    let msg = "function parameter variable"
    params <- parseListable msg '(' ',' ')' (parseName msg)
    expect (expected_sub_for "lambda function definition") $ \com3 -> do
      script <- parseBracketedScript
      return (LambdaExpr (Com ()) (com com1 params com2) (com com3 script []))

parseArrayDef :: NameComParser ObjectExpr
parseArrayDef = guardKeyword "array" $ \com1 -> do
  beginToken
  expect "(first,last) index bounds for array definition" $ \com2 -> do
    bounds <- parseListable "array definition index bound value" '(' ',' ')' parseObjectExpr
    let bad_bounds = fail "array defintion must have exactly two index bounds given"
    case bounds of
      [lo, hi] -> do
        endToken
        expect "initializing values for array defition" $ \com3 -> do
          initValues <- parseListable "array initialing element" '{' ',' '}' parseObjectExpr
          com4 <- parseComment
          parseEquation (ArrayExpr (Com ()) (com com1 bounds com2) (com com3 initValues [])) com4
      _       -> bad_bounds

parseDictDef :: NameComParser ObjectExpr
parseDictDef key com1 = do
  guard (key=="dict" || key=="intmap")
  let getItem = beginToken >> parseObjectExpr >>= \item -> case item of
        AssignExpr _ _ -> endToken >> return item
        _ -> fail ("each entry to "++key++" definition must assign a value to a key")
  items <- parseListable ("items for "++key++" definition") '{' ',' '}' getItem
  parseComment >>= parseEquation (DictExpr (Com (ustr key)) (com com1 items []))

-- Parses equations and assignment operations (which are similar). It takes an initial object, then
-- begins scanning for operators.
parseEquation :: ObjComParser ObjectExpr
parseEquation obj com1 = undefined -- TODO

