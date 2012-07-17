-- "src/Dao/Object/Parsers.hs"  provides the parsers that parse Dao
-- objects, Dao scripts, and whole Dao programs.
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


module Dao.Object.Parsers where

import           Dao.Types
import           Dao.Pattern
import           Dao.Combination
import           Dao.Parser
import           Dao.Object
import           Dao.Object.Show

import           Data.Char
import           Data.List

import qualified Data.ByteString.Lazy as B

import Debug.Trace

-- | Used to delimit individual 'Script' expressions in blocks.
endLine :: Parser Char
endLine = munch nonNLSpace >> satisfy (\c -> c==';' || c=='\n')

underbarAlphaNum :: Char -> Bool
underbarAlphaNum c = c=='_' || isAlphaNum c

whitespace :: Char -> Bool
whitespace c = c==' ' || isControl c

nonNLSpace :: Char -> Bool
nonNLSpace c = whitespace c && c/='\n'

wordToken :: Parser String
wordToken = do
  ubar <- munch (=='_')
  alph <- munch1 isAlpha
  txt  <- munch underbarAlphaNum
  return (ubar++alph++txt)

wordTokenUStr :: Parser UStr
wordTokenUStr = fmap ustr wordToken

numeralToken :: Parser UStr
numeralToken = do
  a <- satisfy isNumber
  ax <- munch1 (\c -> isAlphaNum c || c=='.')
  return (ustr (a:ax))

idenToken :: Parser [UStr]
idenToken = do
  char '$'
  between (char '{') (char '}') $
    flip sepBy (char '.') $ first $
      [ char '$' >> reusedParser "string literal to be used as part of a Reference literal"
      , wordTokenUStr
      ]

------------------------------------------------------------------------------------------------------

commentString :: Comment -> UStr
commentString com = case com of
  InlineComment  com -> com
  EndlineComment com -> com

comment :: Parser Comment
comment = munch whitespace >> first [inline, endline] where
  endline = do
    string "//"
    txt <- munch (/='\n')
    return (EndlineComment (ustr txt))
  inline = do
    string "/*"
    let loop ax = first $
          [eof >> fail "comment runs past end of input"
          ,do bx <- munch  (/='*')
              (_:cx) <- munch1 (=='*')
              let ax' = ax++bx++cx
              first [char '/' >> return (InlineComment (ustr ax')), loop ax']
          ]
    loop ""

-- | Use a parser to construct an item. The whitespace and comments before and after this item will
-- be parsed and attached to the constructed item.
withComments :: Parser a -> Parser (Com a)
withComments fn = do
  bef <- many comment
  a   <- munch whitespace >> fn
  aft <- munch whitespace >> many comment
  return $ case (bef, aft) of
    ([] , [] ) -> Com           a
    (bef, [] ) -> ComBefore bef a
    ([] , aft) -> ComAfter      a aft
    (bef, aft) -> ComAround bef a aft

----------------------------------------------------------------------------------------------------
-- Parsing literal object expressions

parseNull = first [string "null", string "false"] >> return ONull
parseTrue = string "true" >> return OTrue
parseType = string "type" >> char '.' >> munch1 isAlpha >>= toType where
  a = return . OType
  toType str = case str of
    "null"     -> a NullType
    "false"    -> a NullType
    "true"     -> a TrueType
    "Type"     -> a TypeType
    "Int"      -> a IntType
    "Word"     -> a WordType
    "Long"     -> a LongType
    "Float"    -> a FloatType
    "Ratio"    -> a RatioType
    "Complex"  -> a ComplexType
    "DiffTime" -> a DiffTimeType
    "Time"     -> a TimeType
    "Char"     -> a CharType
    "String"   -> a StringType
    "Ref"      -> a RefType
    "Pair"     -> a PairType
    "List"     -> a ListType
    "Set"      -> a SetType
    "Array"    -> a ArrayType
    "Dict"     -> a DictType
    "Intmap"   -> a IntMapType
    "Tree"     -> a TreeType
    "Pattern"  -> a PatternType
    "Script"   -> a ScriptType
    "Rule"     -> a RuleType
    "Bytes"    -> a BytesType
    _          -> fail ("unknown type: "++show str)

litInt = do
  o  <- reusedParser "Int"
  ax <- look
  case ax of
    a:_ | a=='.' || underbarAlphaNum a -> fail "imprpoer int token"
    _ -> return (OInt o)
litFloat    = fmap OFloat (reusedParser "Float")
litRatio    = fmap ORatio (reusedParser "Ratio")
litComplex  = fmap OComplex (reusedParser "Complex")
litTime     = fmap OTime (reusedParser "Time")
litDiffTime = do
  d <- reusedParser "Float" :: Parser Double
  char 's'
  ax <- look
  case ax of
    a:ax | isAlpha a -> fail "improper difftime token"
    _ -> return (ODiffTime (fromRational (toRational d)))
litRef      = fmap ORef idenToken
litString   = fmap OString (reusedParser "String")

litBytes = string "Bytes" >> munch whitespace >> char '\'' >> base where
  base = string "16:" >> loop []
  loop ax = do
    munch whitespace
    first $
      [ char '\'' >> return (OBytes (B.pack ax))
      ,do c <- satisfy (not . isHexDigit)
          fail ("invalid character "++show c++" in "++show BytesType++" literal")
      ,do cx <- munch isHexDigit
          let ln = length cx
              conv c = fromIntegral $ case c of
                c | '0'<=c && c<='9' -> ord c - ord '0'
                c | 'A'<=c && c<='F' -> ord c - ord 'A' + 10
                c | 'a'<=c && c<='f' -> ord c - ord 'a' + 10
                _ -> error $ "internal error: "++
                      "invalid character received by hexadecimal decoder for Bytes literal"
              pairs cx = case cx of
                []     -> []
                [a]    -> [a]
                a:b:cx -> (a*0x10 + b) : pairs cx
          loop (ax ++ pairs (map conv (if mod ln 2 == 1 then '0':cx else cx)))
      ]

keyword :: String -> Parser String
keyword k = do
  w <- munch whitespace >> wordTokenUStr
  if w == ustr k then return k else fail ("expecting keyword "++k)

-- | Objects that can be parsed without needing more complex constructs, like comma-separated items
-- enclosed with brackets. Another condition for an 'Object' to be atomic is that it must be
-- unambiguously parsed in parallel with all other atomic objects. Currently, the list of atomic
-- objects includes: 'OType', 'ONull', 'OTrue' 'ORef', 'OInt', 'OFloat' (as long as the float
-- expression contains a decimal point or an exponent), 'ORatio', 'OString', 'OWord',
-- 'OLong', 'OTime', 'ODiffTime' (must be followed by a 's' character to denote "seconds").
atomicObj :: Parser Object
atomicObj = labelParse "simple constant" $ first $
  [ parseNull, parseTrue, parseType, litInt, litFloat
  , litTime, litDiffTime, litRef, litString, p "word" OWord, p "long" OLong
  ] where { p str con = keyword str >> munch1 whitespace >> fmap con (reusedParser str) }

----------------------------------------------------------------------------------------------------

listingExpr :: String -> Char -> Char -> Char -> Parser (Com a) -> Parser [Com a]
listingExpr msg open comma close pars =  labelParse msg $ do
  munch whitespace >> char open
  objs <- sepBy pars (munch whitespace >> char comma)
  munch whitespace >> char close
  return objs

listExpr :: Parser [Com ObjectExpr]
listExpr = listingExpr "list expression" '[' ',' ']' (withComments objectExpr)

-- | Used to build various constructors expressions, including @array@, @dict@, @intmap@, and @func@.
constructorExpr :: String -> Parser [Com ObjectExpr] -> Parser ObjectExpr
constructorExpr cons pars = do
  cons <- withComments (fmap ustr (string cons))
  fmap (DictExpr cons) (withComments pars)

listConsExpr :: Parser ObjectExpr
listConsExpr = fmap (DictExpr (Com (ustr "list"))) (withComments listExpr)

setExpr :: Parser ObjectExpr
setExpr = constructorExpr "set" $
  listingExpr "set expression" '[' ',' ']' (withComments objectExpr)

defExpr :: Char -> Parser a -> (Com a -> Com ObjectExpr -> b) -> Parser term -> Parser b
defExpr c iden cons term = do
  nm <- withComments iden
  char c >> munch whitespace
  obj <- withComments objectExpr
  term >> return (cons nm obj)

-- | Used to construct @dict@ and @intmap@ types.
objAssignExpr :: Parser ObjectExpr
objAssignExpr = defExpr ':' objectExpr AssignExpr (return ())

dictExpr :: Parser ObjectExpr
dictExpr = first [fn "dict" "dictionary expression", fn "intmap" "intmap expression"] where
  fn short long = constructorExpr short $
    listingExpr long '{' ',' '}' (withComments objAssignExpr)

lambdaExpr :: Parser ObjectExpr
lambdaExpr = do
  con   <- withComments (void (string "func"))
  labelParse "anonymous function" $ do
    args  <- withComments (listingExpr "argument list for function" '(' ',' ')' (withComments wordTokenUStr))
    block <- withComments (blockExpr "anonymous function expression block")
    return (LambdaExpr con args block)

arrayExpr :: Parser ObjectExpr
arrayExpr = do
  con <- withComments (void (string "array"))
  labelParse "array" $ first $
    [do range <- withComments $
          listingExpr "array range expression" '(' ',' ')' (withComments objectExpr)
        items <- withComments listExpr
        return (ArrayExpr con range items)
    ,do items <- withComments listExpr
        return (ArrayExpr con (Com []) items)
    ]

funcArgsList :: Parser [Com ObjectExpr]
funcArgsList =
  listingExpr "arguments list to function call" '(' ',' ')' (withComments objectExpr)

funcCallExpr :: Parser ObjectExpr
funcCallExpr = first $
  [do name <- withComments wordTokenUStr
      argv <- withComments funcArgsList
      return (FuncCall name argv)
  ,do name <- withComments wordTokenUStr
      arg1 <- withComments unitObjectExpr
      return (FuncCall name (Com [arg1]))
  ]

lambdaCallExpr :: Parser ObjectExpr
lambdaCallExpr = do
  con   <- withComments (void (string "call"))
  names <- withComments objectExpr
  argv  <- withComments funcArgsList
  return (LambdaCall con names argv)

localRefExpr :: Parser ObjectExpr
localRefExpr = fmap LocalRef (withComments wordTokenUStr)

intRefExpr :: Parser ObjectExpr
intRefExpr = fmap IntRef (withComments (char '$' >> reusedParser "match index reference"))

literalExpr :: Parser ObjectExpr
literalExpr = fmap Literal (withComments atomicObj)

parenExpr :: Char -> Char -> Parser ObjectExpr
parenExpr open clos = do
  a <- char open >> fmap ParenExpr (withComments objectExpr)
  munch whitespace
  labelParse "close parenthases" (char clos >> return a)

-- | These are object expressions of the 1st highest prescedence, and are NOT equation expressions
-- composed of multiple symbols separated by binary infix operators. These are the "units" that an
-- make an equation, i.e. are the unit expressions between infix expressions.
unitObjectExpr :: Parser ObjectExpr
unitObjectExpr = do
  a <- first $
    [ parenExpr '(' ')', arrayExpr, intRefExpr, dictExpr, setExpr, lambdaExpr
    , listConsExpr, literalExpr, lambdaCallExpr, funcCallExpr, localRefExpr
    ]
  option a (withComments (parenExpr '[' ']') >>= \subscript ->
    return (ArraySubExpr (Com a) subscript))

-- | These are object expressions of the 3rd highest prescedence, the 2nd highest is the
-- square-brackets subscript operation, but to improve efficiency, parsing of square-brackets is
-- factored into the 'unitObjectExpr' parser.
unaryOperator :: Parser ObjectExpr
unaryOperator = do
  op <- withComments (fmap (ustr . (:"")) (first (map char "$@~!-+")))
  labelParse ("unary operator "++uchars (unComment op)) $ do
    ex <- withComments unitObjectExpr
    return (FuncCall op (Com [ex]))

-- | Parsing 'Dao.Types.ObjectExpr's that are composed of built-in binary (2-parameter) infixed
-- operators, particularly 'Dao.Types.Equation' expressions.
equationExpr :: Parser ObjectExpr
equationExpr = labelParse "equation" $ precInfix (first [unitObjectExpr, unaryOperator]) table where
  prec p opx = flip map opx $ \op -> 
    withComments (fmap ustr $ string op) >>= \op ->
      return (0-p, \x y -> Equation (Com x) op (Com y))
  table = concat $
    [ prec  4 ["**"]
    , prec  7 ["<<", ">>"] -- bitwise shift left, right
    , prec  8 ["<", "<=", ">", ">="] -- logical comparator, less than (or equal to), greater-than (or equal to)
    , prec  9 ["==", "!="] -- logical equivalence, logical un-equivalence
    , prec 13 ["&&"] -- logical and
    , prec 14 ["||"] -- logical or
    , prec 15 ["->"] -- function composition
    , prec  2 ["."] -- concatenate references
    , prec  5 ["*", "/", "%"] -- arithmetic multiply, divide, modulus
    , prec  6 ["+", "-", ":+"] -- arithmetic plus, minus, compose complex from real/imaginary parts
    , prec  8 ["<", ">"] -- logical comparator, less than (or equal to), greater-than (or equal to)
    , prec 10 ["&"] -- bitwise and
    , prec 11 ["^"] -- bitwise xor
    , prec 12 ["|"] -- bitwise or
    ]
  -- NOTE: prescedence for parenthases, brackets, and commas are implied in the parser itself.
  -- The order of this table is important, @&&@ *MUST* be parsed before @&@. Prescedence is not
  -- determined by order, but by the numerical prescedence value.

objectExpr :: Parser ObjectExpr
objectExpr = first $ -- first check for an assignment expression has the lowest prescedence.
  [ do  left <- withComments equationExpr
        munch whitespace >> string "=" >> munch whitespace
        right <- withComments objectExpr
        return (AssignExpr left right)
  , equationExpr
  ]

----------------------------------------------------------------------------------------------------

objectEvalExpr :: Parser ScriptExpr
objectEvalExpr = do
  obj <- withComments objectExpr
  void (munch whitespace >> char ';' >> munch whitespace)
  return (EvalObject obj)

ifThenExpr :: Parser ScriptExpr
ifThenExpr = do
  keyword "if"
  labelParse "if" $ do
    cndn <- withComments objectExpr
    thn  <- withComments (blockExpr "conditional expression of an if")
    first $
      [ do  keyword "else"
            labelParse "else" $ first $
              [ do  els <- withComments ifThenExpr
                    return (IfThenElse cndn thn (Com [els]))
              , do  els <- withComments (blockExpr "else")
                    return (IfThenElse cndn thn els)
              ]
      , return (IfThenElse cndn thn (Com []))
      ]

tryCatchExpr :: Parser ScriptExpr
tryCatchExpr = do
  keyword "try"
  labelParse "try" $ do
    try <- withComments (blockExpr "try")
    keyword "catch"
    labelParse "catch" $ do
      var <- withComments wordTokenUStr
      catch <- withComments (blockExpr "catch")
      return (TryCatch try var catch)

forLoopExpr :: Parser ScriptExpr
forLoopExpr = do
  keyword "foreach"
  labelParse "foreach loop" $ do
    var <- withComments wordTokenUStr
    keyword "in"
    labelParse "at the \"in\" portion of a foreach loop" $ do
      obj <- withComments objectExpr
      blk <- withComments (blockExpr "foreach")
      return (ForLoop var obj blk)

continueExpr :: Parser ScriptExpr
continueExpr = do
  fn' <- withComments $ do
    fn <- (string "continue" <++ string "break")
    labelParse ("\"if\" portion of "++fn) $
      munch whitespace >> string "if" >> return fn
  labelParse (unComment fn') $ do
    let fn = fmap (=="continue") fn'
    obj <- withComments objectExpr
    com <- withComments (char ';')
    return (ContinueExpr fn obj (fmap (const ()) com))

-- This is like 'objectEvalExpr', but has a higher prescedence so "return" and "throw" statements
-- can take values without parenthasis.
throwReturn :: Parser ScriptExpr
throwReturn = do
  fn' <- withComments (string "return" <++ string "throw")
  labelParse (unComment fn') $ do
    let fn = fmap (=="return") fn'
    obj <- withComments objectExpr
    com <- withComments (char ';')
    return (ReturnExpr fn obj (fmap (const ()) com))

withDocExpr :: Parser ScriptExpr
withDocExpr = do
  keyword "with"
  labelParse "with" $ do
    doc <- withComments objectExpr
    blk <- withComments (blockExpr "\"with\" expression")
    return (WithDoc doc blk)

scriptExpr :: Parser ScriptExpr
scriptExpr = labelParse "script expression" $ first $
  [ ifThenExpr,   tryCatchExpr, forLoopExpr, withDocExpr
  , continueExpr,  throwReturn, objectEvalExpr
  ]

-- | Curly-braces surrounding a sequence of 'ScriptExpr's.
blockExpr :: String -> Parser [Com ScriptExpr]
blockExpr msg = labelParse msg $ do
  let m c = munch whitespace >> char c
  between (m '{') (m '}') interactiveScript

-- | Script expressions parsed at an intercative prompt are basically 'blockExpr's, but wihtout the
-- encosing open/close curly-braces.
interactiveScript :: Parser [Com ScriptExpr]
interactiveScript = many (withComments scriptExpr)

----------------------------------------------------------------------------------------------------

rulePatternParser :: Parser [Com Pattern]
rulePatternParser = do
  let par = withComments (fmap parsePattern (reusedParser "rule pattern"))
  first [listingExpr "rule pattern expression" '(' ',' ')' par, fmap (:[]) par]

ruleDirectiveParser :: Parser Directive
ruleDirectiveParser = fmap RuleExpr $ withComments $ do
  string "rule"
  labelParse "rule pattern" $ do
    pats <- withComments rulePatternParser
    block <- withComments (blockExpr "rule action commands")
    return (Rule pats block)

-- | A parser that parses @SETUP@, @TAKEDOWN@, @BEGIN@, @END@, directives.
eventDirective :: String -> (Com [Com ScriptExpr] -> Directive) -> Parser Directive
eventDirective str con = string str >> fmap con (withComments (blockExpr (str++" directive")))

-- | A parser that parses "requires", "string.tokenizer = ...;" and "string.equality = ...;"
-- directives.
optionDirective :: String -> Parser Directive
optionDirective req = do
  req   <- withComments (fmap ustr (string req))
  label <- withComments (fmap (ustr . intercalate ".") (sepBy wordToken (char '.')))
  char ';' >> return (Requires req label)

-- | A 'source' program is composed of concatenated directives.
directive :: Parser Directive
directive = first $
  [ defExpr '=' idenToken ToplevelDefine endLine
  , ruleDirectiveParser
  , do  munch whitespace >> string "import" >> munch whitespace
        im <- fmap ImportExpr (withComments (reusedParser "quoted string for import file path"))
        endLine >> return im
  , eventDirective "SETUP"    SetupExpr
  , eventDirective "BEGIN"    BeginExpr
  , eventDirective "END"      EndExpr
  , eventDirective "TAKEDOWN" TakedownExpr
  , optionDirective "string.tokenizer"
  , optionDirective "string.equality"
  , optionDirective "requires"
  ]

-- | Parses a program string from source code, but does not construct a
-- 'Dao.Types.Program' object. Pass the result of this parser to
-- 'Dao.Object.Data.programFromSource'.
source :: Parser SourceCode
source = do
  handl <- withComments (string "module" >> munch whitespace >> reusedParser "module name string")
  drcvs <- withComments (many (withComments directive))
  munch whitespace >> eof
  return (SourceCode{sourceFullPath = nil, sourceModuleName = handl, directives = drcvs})

----------------------------------------------------------------------------------------------------
-- Testing parsers.

testParse :: Show a => Parser a -> String -> IO ()
testParse fn str = forM_ (runCombination fn (parserState{inputString = str})) $ \ (a, st) -> do
  rem <- case a of
    Right s -> putStrLn "success:" >> print s >> return st
    Left  e -> putStrLn ("failed: "++showObj 0 e) >> return st
  putStrLn ("remainder:\n"++show rem)

