-- "src/Dao/Object/Parser.hs" makes use of 'Dao.Parser' to parse
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

module Dao.Object.Parser where

import           Dao.String
import           Dao.Token
import           Dao.Object
import           Dao.EnumSet
import           Dao.Parser
import qualified Dao.Tree as T

import           Control.Monad

import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Complex
import           Data.Time.Clock
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
numericObj = token $ do
  sign <- plusMinus
  dgt  <- mplus (regexMany1 digit) backtrack
  let errmsg = fail "invalid integer expression"
  case dgt of
    "0"     -> do
      base <- zeroOrOne alpha
      let alt take base = regexMany1 take >>= parsePoint base take sign
      flip mplus errmsg $ case base of
        "b" -> alt digit  2
        "x" -> alt xdigit 16
        ""  -> parsePoint 10 digit sign ""
        _   -> typeSuffix True 0 "" base
    '0':dgt -> parsePoint 8  digit sign dgt
    dgt     -> parsePoint 10 digit sign dgt
  where
    plusMinus = zeroOrOne (rxUnion (rxChar '+') (rxChar '-'))
    parsePoint base take sign dgt = flip mplus (makeRational base sign dgt "" "" "") $ do
      char '.'
      rdx <- regexMany take
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
        Just (r_is_an_integer, r) -> zeroOrOne alpha >>= typeSuffix r_is_an_integer r rdx
    typeSuffix r_is_an_integer r rdx typ = case typ of
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
        if r_is_an_integer && null rdx
          then
            let i = round r
            in  if fromIntegral (minBound::T_int) <= i && i <= fromIntegral (maxBound::T_int)
                  then  return $ OInt $ fromIntegral i
                  else  return $ OLong i
          else return (ORatio r)
      typ -> fail ("unknown numeric type "++show typ)

parseString :: Parser String
parseString = token (char '"' >> loop) where
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
        "\"" -> getToken >>= readsAll "invalid string constant" reads . tokenChars
        _    -> errmsg

parseDiffTime :: Parser T_diffTime
parseDiffTime = token $ do
  hours   <- regexMany1 digit
  minutes <- char ':' >> regexMany1 digit
  seconds <- char ':' >> regexMany1 digit
  if length minutes > 2 || length seconds > 2
    then fail ("invalid time literal expression")
    else do
      milisec <- mplus (char '.' >> regexMany1 digit) (return "")
      let rint = read :: String -> Integer
          rr s = rint s % 1
      return $ fromRational $ toRational $
        60*60 * rr hours + 60 * rr minutes + rr seconds +
          if null milisec then 0 else rr milisec / 10 ^ length milisec

parseDate :: Parser T_time
parseDate = token $ do
  year  <- regexMany1 digit
  month <- char '-' >> regexMany1 digit
  day   <- char '-' >> regexMany1 digit
  diffTime <- flip mplus (return (fromRational 0)) $ do
    comma <- mplus (char ',' >> return True) (return False)
    (if comma then regexMany else regexMany1) space
    parseDiffTime
  zone <- flip mplus (return "") $
    mplus (char ',') (return ',') >> regexMany space >> fmap (' ':) (regexMany1 alpha)
  return (addUTCTime diffTime (read (year ++ '-':month ++ '-':day ++ " 00:00:00" ++ zone)))

parseCharLiteral :: Parser Char
parseCharLiteral = token $ do
  char '\''
  flip mplus (fail "expecting character literal") $ do
    let loop cx = do
          cx1 <- many (notCharSet (setUnion (point '\\') (point '\'')))
          (more, cx2) <- msum $
            [ char '\'' >> return (False, "'")
            , char '\\' >> return (True , "\\")
            , fail "end of input in the middle of a character literal expression"
            ]
          let cx' = cx ++ concat cx1 ++ cx2
          if more then loop cx' else return cx'
    cx <- loop "'"
    case readsPrec 0 cx of
      [(c, "")] -> return c
      _         -> fail ("cannot create char literal from expression "++cx)

parseComment :: Parser [Comment]
parseComment = many comment where
  comment = do
    regexMany space -- ignore whitespace
    token $ do
      char '/'
      msum [endline, inline, backtrack]
  endline = do
    char '/'
    ax <- regexMany (rxNotCharSet (point '\n'))
    zeroOrOne (rxChar '\n')
    t <- getToken
    return (EndlineComment (ustr (tokenChars t)))
  inline = do
    regexMany1 (rxChar '*')
    mplus (char '/' >> getToken >>= \t -> return (InlineComment (ustr (tokenChars t)))) $ do
      regexMany (rxCharSet (setInvert (point '*')))
      we_hit_the_end <- endOfInput
      if we_hit_the_end
        then fail "comment runs past end of input"
        else inline

parseListable
  :: String
  -> Char -> Char -> Char
  -> Parser (ObjectExpr, [Comment])
  -> Parser [Com (ObjectExpr, [Comment])]
parseListable msg open delim close getValue = begin where
  begin = do
    char open
    com1 <- parseComment
    regexMany space
    mplus (char close >> return [Com (VoidExpr, com1)]) (loop com1 [])
  loop com1 zx = do
    (value, com2) <- getValue
    regexMany space
    mplus (char close >> return (zx++[Com (value, com2)])) $ do
      char delim
      com3 <- parseComment
      regexMany space
      loop com3 (zx++[com com1 (value, []) com2])

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
parseName msg = token $ do
  name <- parseKeywordOrName
  if isReservedWord name
    then fail ("cannot use keyword as "++msg)
    else return (ustr name)

parseDotName :: Parser (Bool, [Name])
parseDotName = mplus (parname >>= loop False . (:[])) (loop True []) where
  parname = fmap ustr parseKeywordOrName
  loop leadingDot zx = flip mplus (if null zx then mzero else done leadingDot zx) $ token $ do
    char '.'
    mplus (parname >>= \z -> loop leadingDot (zx++[z])) backtrack
  done leadingDot zx = case zx of
    [z] | not leadingDot && isReservedWord (uchars z) -> fail "cannot use keyword as variable name"
    _ -> return (leadingDot, zx)

parseIntRef :: Parser Reference
parseIntRef = token $ do
  char '$'
  flip mplus backtrack $ do
    int <- regexMany1 digit
    fmap IntRef (readsAll "integer reference" reads int)

parseLocalGlobal :: Parser Reference
parseLocalGlobal = msum $
  [ parseDotName >>= \ (leadingDot, nx) -> case nx of
      []  -> mzero
      [n] -> return (if leadingDot then GlobalRef [n] else LocalRef n)
      nx  -> return (GlobalRef nx)
  , token $ do -- a string reference
      char '$'
      flip mplus backtrack $ do
        str <- parseString
        if null str
          then fail "cannot use null string as reference"
          else return (GlobalRef $ map ustr $ words str)
  ]

----------------------------------------------------------------------------------------------------

-- | A 'NameComParser' is a parser that starts by looking for a keyword, then parses an expression
-- based on that keyword. It is left-factored, so before calling a function of this type, the
-- keyword and first comment after the keyword must both be parsed by the calling context and passed
-- to this function. The result is, many parsers of this type can be placed together in a single
-- 'Control.Monad.msum' list, and each parser will be tried in turn but will not need to backtrack
-- to the initial keyword, because the initial keyword and comment was parsed for it by the calling
-- context.
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
  parseComment >>= \com -> regexMany space >> mplus (expectation com) (fail ("expecting "++msg))

----------------------------------------------------------------------------------------------------

unloc = LocationUnknown

-- | Parses a sequence of 'Dao.Object.ScriptExpr's which can be used for interactive evaluation.
parseInteractiveScript :: Parser [Com ScriptExpr]
parseInteractiveScript = many commented where
  commented = do
    com1 <- parseComment
    expr <- parseScriptExpr
    com2 <- parseComment
    return (com com1 expr com2)

-- | This is the "entry point" for parsing a 'Dao.Object.ScriptExpr'.
parseScriptExpr :: Parser ScriptExpr
parseScriptExpr = applyLocation $ mplus keywordExpr objectExpr where
  objectExpr = parseObjectExpr >>= uncurry objectExprStatement
  keywordExpr = do -- parse an expression that starts with a keyword
    key  <- parseKeywordOrName
    com1 <- parseComment
    nameComParserChoice choices key com1
  choices = -- these are all the kinds of expressions that start with a keyword
    [ ifStatement, tryStatement, forStatement, withStatement
    , continueStatement, returnStatement
    , elseStatement, catchStatement
    , \objExpr com1 -> do
          objExpr <- keywordObjectExpr objExpr com1
          com2 <- parseComment
          regexMany space
          (objExpr, com2) <- parseEquation objExpr com2
          objectExprStatement objExpr com2
    ]

parseBracketedScript :: Parser [Com ScriptExpr]
parseBracketedScript = token (char '{' >>= \com1 -> loop []) where
  loop zx = mplus (regexMany space >> char '}' >> return zx) $
    expect "script expression" $ \com1 -> do
      expr <- parseScriptExpr
      loop (zx++[com com1 expr []])

expected_sub_for msg = fail $
  "expecting bracketed sub-script expression for body of \""++msg++"\" statement"

ifStatement :: NameComParser ScriptExpr
ifStatement = guardKeyword "if" loop where
  loop com1 = token $ do
    (objExpr, com2) <- mplus parseObjectExpr $
      fail "expecting object expression for condition of \"if\" statement"
    case objExpr of
      ParenExpr _ _ _ -> token $
        expect (expected_sub_for "if") $ \com3 -> do
          thenStmt <- parseBracketedScript
          let done com4 com5 elseStmt = return $
                IfThenElse com1 objExpr (com com2 thenStmt com3) (com com4 elseStmt com5) unloc
          com4 <- parseComment
          flip mplus (done com4 [] []) $ do
            string "else"
            com5 <- parseComment
            regexMany space 
            msum $
              [ string "if" >> parseComment >>= loop >>= done com4 com5 . (:[]) . Com
              , parseBracketedScript >>= done com4 com5
              , fail (expected_sub_for "else")
              ]
      _ -> fail "conditional expression must be in parentheses"

tryStatement ::NameComParser ScriptExpr
tryStatement = guardKeyword "try" $ \com1 -> do
  tryStmt <- mplus parseBracketedScript (fail (expected_sub_for "try"))
  com2 <- parseComment
  let done com3 name com4 catchStmt = return $
        TryCatch (com com1 tryStmt com2) (com com3 name com4) catchStmt unloc
  flip mplus (done [] nil [] []) $ do
    string "catch"
    regexMany space
    expect "\"catch\" statement must be followed by a variable name" $ \com3 -> do
      name <- parseName "the name of the \"catch\" variable"
      expect (expected_sub_for "catch") $ \com4 -> do
        catchStmt <- parseBracketedScript
        done com3 name com4 catchStmt

forStatement :: NameComParser ScriptExpr
forStatement = guardKeyword "for" $ \com1 -> do
  name <- mplus (parseName "the name of the \"for\" variable") $
    fail "\"for\" statement must be followed by a variable name"
  expect "expecting \"in\" statement" $ \com2 -> do
    string "in"
    expect "expecting object expression over which to iterate in \"for\" statement" $ \com3 -> do
      (iterExpr, com4) <- parseObjectExpr
      forStmt <- mplus parseBracketedScript $ fail $
        "expecting bracketed sub-script expression for body of \"for\" statement"
      return (ForLoop (com com1 name com2) (com com3 iterExpr com4) forStmt unloc)

withStatement :: NameComParser ScriptExpr
withStatement = guardKeyword "with" $ \com1 -> do
  (withObjExpr, com2) <- mplus parseObjectExpr $
    fail "expecting object expression after \"with\" statement"
  with <- mplus parseBracketedScript (fail (expected_sub_for "with"))
  return (WithDoc (com com1 withObjExpr com2) with unloc)

returnStatement :: NameComParser ScriptExpr
returnStatement key com1 = do
  guard (key=="return" || key=="throw")
  regexMany space
  let semicolon = "expecting terminating semicolon \";\" after return statement"
      done objExpr com2 = return (ReturnExpr (key=="return") (com com1 objExpr com2) unloc)
  msum $
    [ do  (objExpr, com2) <- parseObjectExpr
          mplus (char ';' >> done objExpr com2) $
            fail "return statement must be terminated with a semicolon \";\""
    , char ';' >> done VoidExpr []
    , fail semicolon
    ]

continueStatement :: NameComParser ScriptExpr
continueStatement key com1 = do
  guard (key=="break" || key=="continue")
  let done com2 expr com3 =
        return (ContinueExpr (key=="continue") com1 (com com2 expr com3) unloc)
  mplus (char ';' >> done [] VoidExpr []) $ do
    string "if"
    expect ("expecting object expression after "++key++"-if statement") $ \com2 -> do
      (objExpr, com3) <- parseObjectExpr
      regexMany space
      mplus (char ';' >> done com2 objExpr com3) $
        fail ("\""++key++"\" statement must be terminated with a simicolon \";\" character")

-- fails immediately, else and catch statements are parsed only after if and try statements.
badStatement :: String -> String -> NameComParser ScriptExpr
badStatement str msg key com1 = do
  guard (str==key)
  fail ("\""++str++"\" statement must be preceeded by a valid \""++msg++"\" statement")

elseStatement  = badStatement "else" "if"
catchStatement = badStatement "catch" "try"

-- | This function takes an 'Dao.Object.ObjectExpr' and checks if it can be used as a statement. It
-- then checks for a trailing simicolon or newline. Function calls with side-effects and assignment
-- expressions can be used as stand-alone script expressions and will evaluate to a
-- 'Dao.Object.ScriptExpr', whereas equations and literal expressions cannot, and will evaluate to a
-- syntax error.
objectExprStatement :: ObjComParser ScriptExpr
objectExprStatement initExpr com1 = loop initExpr where
  loop expr = case expr of
    AssignExpr _ _ _  _ -> done
    FuncCall   _ _ _  _ -> done
    ParenExpr  _ expr _ -> loop (unComment expr)
    _ -> fail ("cannot use an object expression as a statement\n" ++ show expr)
  done = mplus (char ';' >> return (EvalObject initExpr com1 unloc)) $
    fail "expecting terminating semicolon \";\" after statement"

----------------------------------------------------------------------------------------------------

-- | This is the "entry point" for parsing 'Dao.Object.ObjectExpr's.
parseObjectExpr :: Parser (ObjectExpr, [Comment])
parseObjectExpr = do
  obj  <- applyLocation parseNonEquation
  com1 <- parseComment
  regexMany space
  mplus (parseEquation obj com1) (return (obj, com1))

parseEquation :: ObjectExpr -> [Comment] -> Parser (ObjectExpr, [Comment])
parseEquation obj com1 = loop [] obj com1 where
  loop objx obj com1 = msum $
    [ do -- Parse an indexing expression in square brackets.
          -- ** As long as this is parsed before the 'nonKeywordObjectExpr's, the indexing
          -- operator, square-brackets (for example, "[i]") will have a higher prescedence than
          -- the unary referencing and dereferencing operators "$" and "@".
          char '['
          expect "object expression inside of square brackets" $ \com2 -> do
            (idx, com3) <- parseObjectExpr
            flip mplus (fail "expecting closing square-bracket \"]\"") $ do
              char ']'
              com1 <- parseComment
              regexMany space
              loop objx (ArraySubExpr obj com1 (com com2 idx com3) unloc) com1
    , do -- Parse an infix operator.
          op <- parseInfixOp
          expect ("expecting object expression after infix operator \""++uchars op++"\"") $ \com2 -> do
            nonEqn <- parseNonEquation
            com3 <- parseComment
            regexMany space
            loop (objx++[Right obj, Left (com com1 op com2)]) nonEqn com3
    , return $ case applyPrescedence (objx++[Right obj]) of
        [Right obj] -> (obj, com1)
        _ ->  error $ ("unknown prescedence for operators:"++) $ concat $
                flip concatMap objx $ \obj -> case obj of
                  Left obj -> [' ':uchars (unComment obj)]
                  _        -> []
    ]

-- Parses anything that is not an equation. This is mostly for prescedence, as this function calls
-- 'nonKeywordObjectExpr' which calls 'parseUnaryOperatorExpr'. The unary operator parser calls this
-- function to get its operand, rather than 'parseObjectExpr', which ensures that the unary
-- operators have a prescedence higher than any other operator.
parseNonEquation :: Parser ObjectExpr
parseNonEquation = mplus nonKeywordObjectExpr $ do
  name <- parseKeywordOrName
  expect "some object expression" (\com1 -> keywordObjectExpr name com1)

parseParenObjectExpr :: Parser ObjectExpr
parseParenObjectExpr = do
  char '('
  expect "object expression in parentheses" $ \com1 -> do
    (expr, com2) <- parseObjectExpr
    flip mplus (fail "expecting close parethases") $
      char ')' >> return (ParenExpr True (com com1 expr com2) unloc)

-- Object expressions that don't begin with a keyword.
nonKeywordObjectExpr :: Parser ObjectExpr
nonKeywordObjectExpr = msum $
  [ parseParenObjectExpr
    -- ^ anything with an open-parenthasis.
  , fmap (flip Literal unloc . ORef) parseIntRef
    -- ^ literal integer references, parsed out as a single value, rather than the reference unary
    -- operator operating on an integer value.
  , fmap (flip Literal unloc . OString . ustr) parseString 
    -- ^ literal string
  , fmap (flip Literal unloc . OChar) parseCharLiteral
    -- ^ literal character
  , fmap (flip Literal unloc) numericObj
    -- ^ if a numeric object starts with an minus-sign, it will parse to a single negative value
    -- rather than parsing to a unary negation operator that operates on a positive integer value.
  , parseUnaryOperatorExpr
    -- ^ unary operators, like dereference (@name) or reference ($name)
  ]

-- Parses an equation starting with a unary operator.
parseUnaryOperatorExpr :: Parser ObjectExpr
parseUnaryOperatorExpr = do -- high-prescedence unary operators, these are interpreted as 'FuncCall's.
  op <- regex (rxCharSetFromStr "-@$!~")
  expect ("\""++op++"\" operator must be followed by an object expression") $ \com1 -> do
    expr <- mplus parseParenObjectExpr parseNonEquation
    return (PrefixExpr (read op) (com com1 expr []) unloc)

parseInfixOp :: Parser Name
parseInfixOp = fmap ustr $ regex $ rxChoice $ map rxString $ words $ concat $
  [ " <<= >>= += -= *= /= %= &= |= ^= .= "
  , " != == <= >= && || << >> ** -> "
  , " . = + - * / % & | < > ^ "
  ]

-- Operator prescedence mimics the C and C++ family of languages.
-- 'applyPrescedence' scans from highest to lowest prescedence, essentially creating a function
-- that looks like this: (scanBind (words p3) . scanBind (words p2) . scanBind (words p1) . ...)
-- The apply (.) operator has the right-hand function evaluated first before the left-hand
-- function, so the right-most operators have the highest prescedence because they get bound by
-- 'scanBind' first. Therefore, listing the operators by prescedence (from left to right) means
-- listing them from lowest to highest prescedence.
applyPrescedence :: [Either (Com Name) ObjectExpr] -> [Either (Com Name) ObjectExpr]
applyPrescedence = foldl (.) assignOp $ map (scanBind Equation . words) $ opPrecTable where
  assignOp = scanBind AssignExpr $ -- lowest prescedence, used as initial value to fold
    words "= += -= *= /= %= &= |= <<= >>= ^="
  opPrecTable = -- operators listed from lowest to highest prescedence
    [ "||", "&&", "|", "^", "&", "!= =="
    , "<= >= < >", "<< >>", "+ -", "* / %", "**", ". ->"
    ]

-- Given a list of operators, scans through an equation of the form
-- (Right exprA : Left op : Right exprB : ...) 
-- and if the 'op' is in the list of operators, the 'exprA' and 'exprB' are bound together into an
-- 'Dao.Object.Equation' data structure. If 'op' is not in the list of operators, it is passed over.
scanBind
  :: Read a
  => (ObjectExpr -> Com a -> ObjectExpr -> Location -> ObjectExpr)
  -> [String]
  -> [Either (Com Name) ObjectExpr]
  -> [Either (Com Name) ObjectExpr]
scanBind constructor ops objx = case objx of
  [Right o] -> [Right o]
  Right a : Left op : Right b : objx ->
    if elem (uchars (unComment op)) ops -- if this operator is of the prescedence we are looking for
      then scanBind constructor ops (Right (constructor a (fmap (read . uchars) op) b unloc) : objx) -- "bind" the operands to it
      else Right a : Left op : -- otherwise ignore this operator
             scanBind constructor ops (Right b : objx)
  objx -> error ("scanBind failed:\n"++show objx)

-- Here we collect all of the ObjectExpr parsers that start by looking for a keyword
keywordObjectExpr :: NameComParser ObjectExpr
keywordObjectExpr key com1 = msum $ map (\parser -> parser key com1) $
  [ parseDateTime, parseArrayDef, parseListSetDictIntmap
  , parseHexData, parseStruct, parseLambdaDef, constructWithNonKeyword
  ]

-- Until I fix how the comments are stored in object and script expressions, I will have to make do
-- with hacks like this one.
adjustComListable :: Parser [Com (a, [Comment])] -> Parser [Com a]
adjustComListable =
  fmap (map (\a -> let (o, c) = unComment a in appendComments (fmap (const o) a) c))

parseFunctionParameters :: String -> Parser [Com ObjectExpr]
parseFunctionParameters msg = adjustComListable $
  parseListable msg '(' ',' ')' parseObjectExpr

-- | Function calls with a single argument that are so common that, as convenience, you can call
-- them without putting the argument in parenthases.
parseBuiltinFuncCall :: NameComParser ObjectExpr
parseBuiltinFuncCall key com1 = do
  guard $ elem key $ words $ concat $
    [ " sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh exp log log10 "
    , " throw static qtime global local do query print file open save close edit test "
    ]
  (arg, com2) <- parseObjectExpr
  return (FuncCall (ustr key) com1 [com [] arg com2] unloc)

func_param_var = "function parameter variable"

parseLambdaDef :: NameComParser ObjectExpr
parseLambdaDef key com1 = do
  guard (key=="func" || key=="function" || key=="rule" || key=="pat" || key=="pattern")
  expect "list of parameter variables after \"function\" statement" $ \com2 -> msum $
    [ do  scrpt <- parseBracketedScript
          return (LambdaExpr (read key) (ComAfter [] (com1++com2)) scrpt unloc)
    , do  params <- parseFunctionParameters ("parameters to \""++key++"\" expression")
          expect (expected_sub_for "lambda function definition") $ \com2 -> do
            script <- parseBracketedScript
            return (LambdaExpr (read key) (com com1 params com2) script unloc)
    ]

parseArrayDef :: NameComParser ObjectExpr
parseArrayDef = guardKeyword "array" $ \com1 -> token $ do
  bounds <- parseFunctionParameters "upper/lower bound for array declaration"
  expect "(first,last) index bounds for array definition" $ \com2 -> do
    let bad_bounds = fail "array defintion must have exactly two index bounds given"
    case bounds of
      [lo, hi] -> token $ do
        flip mplus (fail "expecting initializing values for array defition") $ do
          initValues <- adjustComListable $
            parseListable "array initializing element" '{' ',' '}' parseObjectExpr
          return (ArrayExpr (com com1 bounds com2) initValues unloc)
      _       -> bad_bounds

parseListItems :: String -> Bool -> Parser [Com ObjectExpr]
parseListItems key requireAssign = do
  let getDictItem = token $ parseObjectExpr >>= \ (item, com2) -> case item of
        AssignExpr _ _ _ _ -> return (item, com2)
        _ -> fail ("each entry to "++key++" definition must assign a value to a key")
      getItem = if requireAssign then getDictItem else parseObjectExpr
  adjustComListable (parseListable ("items for "++key++" definition") '{' ',' '}' getItem)

parseListSetDictIntmap :: NameComParser ObjectExpr
parseListSetDictIntmap key com1 = do
  guard (key=="dict" || key=="intmap" || key=="set" || key=="list")
  items <- parseListItems key (key=="dict" || key=="intmap")
  return (DictExpr (ustr key) com1 items unloc)

parseDateTime :: NameComParser ObjectExpr
parseDateTime = guardKeyword "time" $ \com1 -> do
  needsParen <- mplus (char '(' >> return True) (return False)
  expect "date-time literal expression" $ \com2 -> do
    date <- parseDate
    let msg       = "require close-parenthesis after date literal expression"
        done com3 = return $
          FuncCall (ustr "time") com1 [com com2 (Literal (OTime date) unloc) com3] unloc
    if needsParen then expect msg (\com3 -> char ')' >> done com3) else done []

parseStruct :: NameComParser ObjectExpr
parseStruct = guardKeyword "struct" $ \com1 ->
  msum $
    [ do  (obj, com2) <- parseObjectExpr
          mplus (objData com1 obj com2) (return (StructExpr (com com1 obj com2) [] unloc))
    , objData com1 VoidExpr []
    , fail "expecting data structure definition after keyword \"struct\""
    ]
  where
    objData com1 obj com2 = do
      items <- parseListItems "struct" True
      return (StructExpr (com com1 obj com2) items unloc)

parseHexData :: NameComParser ObjectExpr
parseHexData key _ = do
  guard (key=="data")
  expect "bracketed base-64-encoded data expression" $ \com1 -> do
    char '{'
    ax <- loop []
    return (DataExpr com1 ax unloc)
  where
    b64ch = enumSet [segment 'A' 'Z', segment 'a' 'z', segment '0' '9', single '/', single '+']
    loop zx = do
      com1 <- parseComment
      regexMany space
      mplus (char '}' >> return (zx++[com com1 nil []])) $ do
        hex  <- fmap concat $ many $ charSet $ b64ch
        com2 <- parseComment
        sp   <- regexMany space
        eq   <- regexMany (rxChar '=')
        com3 <- parseComment
        regexMany space
        case eq of
          ('=':'=':'=':_) -> fail "too many equals signs at in the data literal expression"
          ""              -> loop (zx++[com com1 (ustr hex) (com2++com3)])
          _               -> do
            mplus (char '}') $
              fail "expecting closing brace character after equals signs in base-64 expression"
            if null sp && null com2
              then  return (zx++[com com1 (ustr (hex++eq)) (com2++com3)])
              else  return (zx++[com com1 (ustr hex) com2, com [] (ustr eq) com3])

-- If 'parseKeywordOrName' was used to parse a symbol, and all of the above keyword parsers
-- backtrack, this function takes the symbol and treats it as a name, which could be a local
-- variable name, a simple function call, or a part of a global variable name.
constructWithNonKeyword :: NameComParser ObjectExpr
constructWithNonKeyword key com1 = msum [funcCall, parseBuiltinFuncCall key com1, localRef] where
  localRef = return (Literal (ORef $ LocalRef $ ustr key) unloc)
  funcCall = do
    argv <- adjustComListable $
      parseListable ("arguments to function \""++key++"\"") '(' ',' ')' parseObjectExpr
    return (FuncCall (ustr key) com1 argv unloc)

----------------------------------------------------------------------------------------------------

-- | Parse a source file. Takes a list of options, which are treated as directives, for example
-- "string.tokenizer" or "string.compare", which allows global options to be set for this module.
-- Pass an empty list to have all the defaults.
parseSourceFile :: Parser SourceCode
parseSourceFile = do
  parseComment >> string "module" >> regexMany space
  flip mplus (fail "keyword \"module\" must be followed by a module name string") $ do
    handl <- fmap ustr parseString
    zeroOrOne (rxChar ';')
    drcvs <- many $ do
      com1 <- parseComment
      regexMany space
      directive <- parseDirective
      return (com com1 directive [])
    regexMany space >> parseComment
    return $
      SourceCode
      { sourceModified = 0
      , sourceFullPath = nil
      , sourceModuleName = Com handl
      , directives = Com drcvs
      }

parseDirective :: Parser TopLevelExpr
parseDirective = parseKeywordOrName >>= \key -> msum $
  [ do -- Parse an "import" or "require" directive.
        guard (key=="requires" || key=="require" || key=="import")
        req <- regexMany space >> fmap ustr parseString
        regexMany space >> zeroOrOne (rxChar ';')
        return (Attribute (Com (ustr key)) (Com req) unloc)
  , do -- Parse an event action.
        guard (key=="TAKEDOWN" || key=="SETUP" || key=="BEGIN" || key=="END")
        expect ("bracketed list of commands after "++key++" statement") $ \com1 -> do
          scrpt <- parseBracketedScript
          let block = com com1 scrpt []
          return $ case key of
            "TAKEDOWN" -> TakedownExpr block unloc
            "SETUP"    -> SetupExpr    block unloc
            "BEGIN"    -> BeginExpr    block unloc
            "END"      -> EndExpr      block unloc
  , do -- Parse a top-level rule.
        guard (key=="rule" || key=="pat" || key=="pattern")
        let msg = "script expression for rule definition"
        expect "rule pattern string expression" $ \com1 -> msum $
          [ do  pattern <- parseFunctionParameters ("patterns to \""++key++"\" expression")
                expect msg $ \com2 -> do
                  scrpt <- parseBracketedScript
                  return (TopLambdaExpr (read key) (com com1 pattern com2) scrpt unloc)
           , do (pattern, com2) <- parseObjectExpr
                expect msg $ \com3 -> do
                  scrpt <- parseBracketedScript
                  return (TopLambdaExpr (read key) (Com [com com1 pattern (com2++com3)]) scrpt unloc)
           ]
  , do -- Parse a top-level function declaration, which has it's name stated before the arguments.
        guard (key=="function" || key=="func")
        let msg = "top-level function declaration"
        expect ("expecting name for "++msg) $ \com1 -> do
          name <- parseName "name of function"
          expect ("list of parameter variables for "++msg) $ \com2 -> do
            params <- parseFunctionParameters "function parameters"
            expect "top-level function declaration, function body" $ \com3 -> do
              scrpt <- parseBracketedScript
              return (ToplevelFunc (com com1 name com2) params (com com2 scrpt com3) unloc)
  , do -- Parse a top-level global variable declaration.
        (_, name_) <- parseDotName
        let msg = "top-level global variable declaration"
            name = ustr key : name_
        expect ("expecting equals-sign \"=\" for "++msg) $ \com1 -> do
          char '='
          expect ("expecting object expression for "++msg) $ \com2 -> do
            (objExpr, com3) <- parseObjectExpr
            flip mplus (fail (msg++" must be terminated with a semicolon \";\"")) $
              char ';' >> return (ToplevelDefine (com [] name com1) (com com2 objExpr com3) unloc)
  ]

