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
import           Dao.Object
import           Dao.EnumSet
import           Dao.Parser
import           Dao.Types

import           Control.Monad

import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Complex
import           Numeric

import Debug.Trace

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

parseDotName :: Parser (Bool, [Name])
parseDotName = mplus (parname >>= loop False . (:[])) (loop True []) where
  parname = fmap ustr parseKeywordOrName
  loop leadingDot zx = flip mplus (if null zx then mzero else done leadingDot zx) $ do
    beginTokenIf (char '.')
    mplus (endToken >> parname >>= \z -> loop leadingDot (zx++[z])) backtrack
  done leadingDot zx = case zx of
    [z] | not leadingDot && isReservedWord (uchars z) -> fail "cannot use keyword as variable name"
    _ -> return (leadingDot, zx)

parseIntRef :: Parser Reference
parseIntRef = do
  beginToken
  char '$'
  flip mplus backtrack $ do
    int <- regexMany1 digit
    fmap IntRef (readsAll "integer reference" reads int) >>= ok

parseLocalGlobal :: Parser Reference
parseLocalGlobal = msum $
  [ parseDotName >>= \ (leadingDot, nx) -> case nx of
      []  -> mzero
      [n] -> return (if leadingDot then GlobalRef [n] else LocalRef n)
      nx  -> return (GlobalRef nx)
  , do  beginToken -- a string reference
        char '$'
        flip mplus backtrack $ do
          str <- parseString
          if null str
            then fail "cannot use null string as reference"
            else ok (GlobalRef $ map ustr $ words str)
  ]

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
  parseComment >>= \com -> regexMany space >> mplus (expectation com) (fail ("expecting "++msg))

----------------------------------------------------------------------------------------------------

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
parseScriptExpr = mplus keywordExpr objectExpr where
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
          (objExpr, com2) <- parseEquation objExpr
          objectExprStatement objExpr com2
    ]

parseBracketedScript :: Parser [Com ScriptExpr]
parseBracketedScript = beginTokenIf (char '{') >>= \com1 -> loop [] where
  loop zx = mplus (regexMany space >> char '}' >> ok zx) $
    expect "script expression" $ \com1 -> do
      expr <- parseScriptExpr
      loop (zx++[com com1 expr []])

expected_sub_for msg = fail $
  "expecting bracketed sub-script expression for body of \""++msg++"\" statement"

ifStatement :: NameComParser ScriptExpr
ifStatement = guardKeyword "if" loop where
  loop com1 = do
    beginToken
    (objExpr, com2) <- mplus parseObjectExpr $
      fail "expecting object expression for condition of \"if\" statement"
    case objExpr of
      ParenExpr objExpr -> do
        endToken
        expect (expected_sub_for "if") $ \com3 -> do
          thenStmt <- parseBracketedScript
          let done com4 com5 elseStmt = return $
                IfThenElse (com com1 (unComment objExpr) com2) (com [] thenStmt com3) (com com4 elseStmt [])
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
        TryCatch (com com1 tryStmt []) (com com2 name com3) (com com4 catchStmt [])
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
      expect "expecting bracketed sub-script expression for body of \"for\" statement" $ \com5 -> do
        forStmt <- parseBracketedScript
        return (ForLoop (com com1 name com2) (com com3 iterExpr com4) (com com5 forStmt []))

withStatement :: NameComParser ScriptExpr
withStatement = guardKeyword "with" $ \com1 -> do
  (withObjExpr, com2) <- mplus parseObjectExpr $
    fail "expecting object expression after \"with\" statement"
  expect (expected_sub_for "with") $ \com3 -> do
    with <- parseBracketedScript
    return (WithDoc (com com1 withObjExpr com2) (com com3 with []))

returnStatement :: NameComParser ScriptExpr
returnStatement key com1 = do
  guard (key=="return" || key=="throw")
  regexMany space
  let nullExpr = com com1 (Literal (Com ONull)) []
      semicolon = "expecting terminating semicolon \";\" after return statement"
      done comObjExpr = return (ReturnExpr (Com (key=="return")) comObjExpr (Com ()))
  msum $
    [ do  (objExpr, com2) <- parseObjectExpr
          mplus (char ';' >> done (com com1 objExpr com2)) $
            fail "return statement must be terminated with a semicolon \";\""
    , char ';' >> done nullExpr
    , fail semicolon
    ]

continueStatement :: NameComParser ScriptExpr
continueStatement key com1 = do
  guard (key=="break" || key=="continue")
  let done com2 expr com3 =
        return (ContinueExpr (com [] (key=="continue") com1) (com com2 expr com3) (Com ()))
  mplus (char ';' >> done [] (Literal (Com ONull)) []) $ do
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
    AssignExpr _ _ _ -> done
    FuncCall   _ _   -> done
    LambdaCall _ _ _ -> done
    ParenExpr  expr  -> loop (unComment expr)
    _ -> fail $ "cannot use an object expression as a statement\n" ++ show expr
  done = expect "terminating semicolon \";\" after statement" $ \com2 -> 
    char ';' >> return (EvalObject (appendComments (com [] initExpr com1) com2))

----------------------------------------------------------------------------------------------------

-- | This is the "entry point" for parsing 'Dao.Object.ObjectExpr's.
parseObjectExpr :: Parser (ObjectExpr, [Comment])
parseObjectExpr = parseNonEquation >>= parseEquation where

parseEquation :: ObjectExpr -> Parser (ObjectExpr, [Comment])
parseEquation obj = parseComment >>= \com1 -> regexMany space >> loop [] obj com1 where
  loop objx obj com1 = do
    let comObj = com [] obj com1
    regexMany space
    msum $
      [ do -- Parse an indexing expression in square brackets.
            -- ** As long as this is parsed before the 'nonKeywordObjectExpr's, the indexing
            -- operator, square-brackets (for example, "[i]") will have a higher prescedence than
            -- the unary referencing and dereferencing operators "$" and "@".
            char '['
            expect "object expression inside of square brackets" $ \com2 -> do
              (idx, com3) <- parseObjectExpr
              expect "expecting closing square-bracket \"]\"" $ \com4 ->
                char ']' >> loop objx (ArraySubExpr comObj (com com2 idx com3)) com4
      , do -- Parse an infix operator.
            op <- parseInfixOp
            expect ("expecting object expression after infix operator \""++uchars op++"\"") $ \com2 ->
              parseNonEquation >>= \nonEqn -> loop (objx++[Right comObj, Left op]) nonEqn com2
      , return $ case applyPrescedence (objx++[Right comObj]) of
          [Right obj] -> (unComment obj, com1)
          _ ->  error $ ("unknown prescedence for operators:"++) $ concat $
                  flip concatMap objx $ \obj -> case obj of
                    Left obj -> [' ':uchars obj]
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

-- Object expressions that don't begin with a keyword.
nonKeywordObjectExpr :: Parser ObjectExpr
nonKeywordObjectExpr = msum $
  [ do -- parse an expression enclosed in parentheses
        char '('
        expect "object expression in parentheses" $ \com1 -> do
          (expr, com2) <- parseObjectExpr
          flip mplus (fail "expecting close parethases") $
            char ')' >> return (ParenExpr (com com1 expr com2))
  -- literal strings or integers
  , fmap (Literal . Com . ORef) parseIntRef
  , fmap (Literal . Com . OString . ustr) parseString 
  , fmap (Literal . Com) numericObj
  -- unary operators, like dereference (@name) or reference ($name)
  , parseUnaryOperatorExpr
  ]

-- Parses an equation starting with a unary operator.
parseUnaryOperatorExpr :: Parser ObjectExpr
parseUnaryOperatorExpr = do -- high-prescedence unary operators, these are interpreted as 'FuncCall's.
  op <- regex (rxCharSetFromStr "@$!~.")
  expect ("\""++op++"\" operator must be followed by an object expression") $ \com1 -> do
    expr <- parseNonEquation
    return (FuncCall (Com (ustr op)) (com com1 [Com expr] []))

parseInfixOp :: Parser Name
parseInfixOp = fmap ustr $ msum $ map string $ words $ concat $
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
applyPrescedence :: [Either Name (Com ObjectExpr)] -> [Either Name (Com ObjectExpr)]
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
  :: (Com ObjectExpr -> Com Name -> Com ObjectExpr -> ObjectExpr)
  -> [String]
  -> [Either Name (Com ObjectExpr)]
  -> [Either Name (Com ObjectExpr)]
scanBind constructor ops objx = case objx of
  [Right o] -> [Right o]
  Right a : Left op : Right b : objx ->
    if elem (uchars op) ops -- if this operator is of the prescedence we are looking for
      then scanBind constructor ops $ -- "bind" the operands to it
              (Right (Com $ constructor a (Com op) b) : objx)
      else Right a : Left op : -- otherwise ignore this operator
             scanBind constructor ops (Right b : objx)
  objx -> error ("scanBind failed:\n"++show objx)

-- Here we collect all of the ObjectExpr parsers that start by looking for a keyword
keywordObjectExpr :: NameComParser ObjectExpr
keywordObjectExpr key com1 = msum $ map (\parser -> parser key com1) $
  [ parseLambdaCall, parseArrayDef, parseListSetDictIntmap
  , parseClassedRef, parseStruct, constructWithNonKeyword
  ]

-- Until I fix how the comments are stored in object and script expressions, I will have to make do
-- with hacks like this one.
adjustComListableObjExpr :: Parser [Com (ObjectExpr, [Comment])] -> Parser [Com ObjectExpr]
adjustComListableObjExpr =
  fmap (map (\a -> let (o, c) = unComment a in appendComments (fmap (const o) a) c))

parseFunctionParameters :: String -> Parser [Com ObjectExpr]
parseFunctionParameters msg = adjustComListableObjExpr $
  parseListable msg '(' ',' ')' parseObjectExpr

parseLambdaCall :: NameComParser ObjectExpr
parseLambdaCall = guardKeyword "call" $ \com1 ->
  expect "expecting an expression after \"call\" statement" $ \com2 -> do
    (objExpr, com3) <- parseObjectExpr
    flip mplus (fail "expecting a tuple containing arguments to be passed to the \"call\" statement") $ do
      argv <- parseFunctionParameters "function parameter variable"
      return (LambdaCall (Com ()) (com com1 objExpr com2) (com com3 argv []))

parseClassedRef :: NameComParser ObjectExpr
parseClassedRef key com1 = case key of
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
      let done co r = endToken >> return (Literal (com com1 (ORef (co r)) []))
      case ref of
        LocalRef  r | typ     -> done nameCo r
        GlobalRef r | not typ -> done namexCo r
        _                     -> fail ("expecting "++msg)

parseFuncParamVars :: Parser [Com Name]
parseFuncParamVars =
  let msg = "function parameter variable"
  in  parseListable msg '(' ',' ')' (parseName msg)

parseLambdaDef :: NameComParser ObjectExpr
parseLambdaDef key com1 = do
  guard (key=="func" || key=="function")
  expect "list of parameter variables after \"function\" statement" $ \com2 -> do
    params <- parseFuncParamVars
    expect (expected_sub_for "lambda function definition") $ \com3 -> do
      script <- parseBracketedScript
      return (LambdaExpr (Com ()) (com com1 params com2) (com com3 script []))

parseArrayDef :: NameComParser ObjectExpr
parseArrayDef = guardKeyword "array" $ \com1 -> do
  beginToken
  expect "(first,last) index bounds for array definition" $ \com2 -> do
    bounds <- parseFunctionParameters "upper/lower bound for array declaration"
    let bad_bounds = fail "array defintion must have exactly two index bounds given"
    case bounds of
      [lo, hi] -> do
        endToken
        flip mplus (fail "expecting initializing values for array defition") $ do
          initValues <- adjustComListableObjExpr $
            parseListable "array initialing element" '{' ',' '}' parseObjectExpr
          return (ArrayExpr (Com ()) (com com1 bounds []) (com com2 initValues []))
      _       -> bad_bounds

parseListSetDictIntmap :: NameComParser ObjectExpr
parseListSetDictIntmap key com1 = do
  guard (key=="dict" || key=="intmap" || key=="set" || key=="list")
  let getDictItem = beginToken >> parseObjectExpr >>= \ (item, com2) -> case item of
        AssignExpr _ _ _ -> endToken >> return (item, com2)
        _ -> fail ("each entry to "++key++" definition must assign a value to a key")
      getItem = if key=="dict" || key=="intmap" then getDictItem else parseObjectExpr
  items <- adjustComListableObjExpr $
    parseListable ("items for "++key++" definition") '{' ',' '}' getItem
  return (DictExpr (Com (ustr key)) (com com1 items []))

parseStruct :: NameComParser ObjectExpr
parseStruct = guardKeyword "struct" $ \com1 ->
  msum $
    [ parseObjectExpr >>= objData com1
    , objData com1 (Literal (Com ONull), [])
    , fail "expecting data structure definition after keyword \"struct\""
    ]
  where
    objData com1 (obj, obj2) =
      expect "data structure needs bracketed list of field declarations" $ \com2 -> do
        items <- parseListable "field declaration for data structure" '{' ',' '}' parseItem
        return (StructExpr (Com ()) (com com1 obj com2) (Com items))
    msg = "field label"
    parseItem = expect msg $ \com1 -> do
      name <- parseName msg
      expect ("equals-sign \"=\" after "++msg) $ \com2 -> do
        char '='
        expect "object experssion to assign to field" $ \com3 -> do
          (obj, com4) <- parseObjectExpr
          return $
            AssignExpr (com com1 (Literal (Com $ ORef $ LocalRef $ name)) com2) (Com nil) (com com3 obj com4)

-- If 'parseKeywordOrName' was used to parse a symbol, and all of the above keyword parsers
-- backtrack, this function takes the symbol and treats it as a name, which could be a local
-- variable name, a simple function call, or a part of a global variable name.
constructWithNonKeyword :: NameComParser ObjectExpr
constructWithNonKeyword key com1 = mplus funcCall localRef where
  localRef = return (Literal (com [] (ORef $ LocalRef $ ustr key) com1))
  funcCall = do
    argv <- adjustComListableObjExpr $
      parseListable ("arguments to function \""++key++"\"") '(' ',' ')' parseObjectExpr
    return (FuncCall (com [] (ustr key) com1) (Com argv))

----------------------------------------------------------------------------------------------------

-- | Parse a source file. Takes a list of options, which are treated as directives, for example
-- "string.tokenizer" or "string.compare", which allows global options to be set for this module.
-- Pass an empty list to have all the defaults.
parseSourceFile :: Parser SourceCode
parseSourceFile = do
  clearTokenStack
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

parseDirective :: Parser Directive
parseDirective = msum $
  [ do -- Parse an "import" or "require" directive.
        opt <- fmap ustr (mplus (string "require" >> zeroOrOne (rxChar 's')) (string "import"))
        req <- regexMany space >> fmap ustr parseString
        regexMany space >> zeroOrOne (rxChar ';')
        return (Attribute (Com opt) (Com req))
  , do -- Parse an event action.
        event <- msum $ map string ["BEGIN", "END", "SETUP", "TAKEDOWN"]
        expect ("bracketed list of commands after "++event++" statement") $ \com1 -> do
          scrpt <- parseBracketedScript
          let block = com com1 scrpt []
          return $ case event of
            "TAKEDOWN" -> TakedownExpr block
            "SETUP"    -> SetupExpr    block
            "BEGIN"    -> BeginExpr    block
            "END"      -> EndExpr      block
  , do -- Parse a top-level rule.
        string "rule"
        let msg = "rule pattern string expression"
            pat = parseString >>= readsAll msg reads
        expect msg $ \com1 -> do
          pattern <- mplus (parseListable msg '(' ',' ')' pat) $
            (pat >>= \pattern -> return [com com1 pattern []])
          expect "script expression for rule definition" $ \com2 -> do
            scrpt <- parseBracketedScript
            return $ RuleExpr $ Com $
              Rule{ rulePattern = com com1 pattern [], ruleAction = com com2 scrpt [] }
  , do -- Parse a top-level function declaration.
        mplus (string "function") (string "func")
        let msg = "top-level function declaration"
        expect ("expecting name for "++msg) $ \com1 -> do
          (_, name) <- parseDotName
          expect ("list of parameter variables for "++msg) $ \com2 -> do
            params <- parseFuncParamVars
            expect "top-level function declaration, function body" $ \com3 -> do
              scrpt <- parseBracketedScript
              return $ ToplevelDefine (com [] name com1) $
                com [] (LambdaExpr (Com ()) (com [] params com2) (Com scrpt)) com2
  , do -- Parse a top-level global variable declaration.
        let msg = "top-level global variable declaration"
        (_, name) <- parseDotName
        expect ("expecting equals-sign \"=\" for "++msg) $ \com1 -> do
          char '='
          expect ("expecting object expression for "++msg) $ \com2 -> do
            (objExpr, com3) <- parseObjectExpr
            flip mplus (fail (msg++" must be terminated with a semicolon \";\"")) $
              char ';' >> return (ToplevelDefine (com [] name com1) (com com2 objExpr com3))
  ]

