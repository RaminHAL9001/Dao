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
import           Data.Time.LocalTime
import           Numeric

import Debug.Trace

maxYears :: Integer
maxYears = 99999

----------------------------------------------------------------------------------------------------

newtype DaoTT = DaoTT{ unwrapDaoTT :: TT } deriving (Eq, Ord, Ix)
instance TokenType  DaoTT where { unwrapTT = unwrapDaoTT; wrapTT = DaoTT }
instance HasTokenDB DaoTT where { tokenDB  = daoTokenDB }
instance MetaToken DaoTokenLabel DaoTT  where { tokenDBFromMetaValue _ = tokenDB }
instance Show DaoTT where { show = deriveShowFromTokenDB daoTokenDB }

type DaoLexer       = Lexer DaoTT ()
type DaoParser    a = Parser    DaoParState DaoTT a
type DaoTableItem a = TableItem DaoParState DaoTT a 
type DaoPTable    a = PTable    DaoParState DaoTT a
type DaoParseErr    = Error     DaoParState DaoTT

daoTokenDB :: TokenDB DaoTT
daoTokenDB = makeTokenDB daoTokenDef

data DaoTokenLabel
  = SPACE | INLINECOM | ENDLINECOM | COMMA | LABEL | STRINGLIT | DATE | TIME
  | BASE10 | DOTBASE10 | NUMTYPE | EXPONENT | INTREF | BASE64DATA | BASE16 | BASE2
  deriving (Eq, Enum, Show, Read)
instance UStrType DaoTokenLabel where { ustr = derive_ustr; fromUStr = derive_fromUStr; }

daoTokenDef :: LexBuilder ()
daoTokenDef = do
  ------------------------------------------ WHITESPACE -------------------------------------------
  let spaceRX = rxRepeat1(map ch "\t\n\r\f\v ")
  space        <- emptyToken SPACE      $ spaceRX
  
  ------------------------------------------- COMMENTS --------------------------------------------
  closeInliner <- fullToken  INLINECOM  $ rxRepeat1(ch '*') . rx '/'
  inlineCom    <- fullToken  INLINECOM  $ rx "/*" .
    fix (\loop -> closeInliner <> rxRepeat1(invert[ch '*']) . loop)
  endlineCom   <- fullToken  ENDLINECOM $ rx "//" . rxRepeat(invert[ch '\n'])
  multiComs    <- pure $ opt $ fix((space <> inlineCom <> endlineCom).)
  
  -------------------------------------------- LABELS ---------------------------------------------
  let alpha = [from 'A' to 'Z', from 'a' to 'z', ch '_']
      labelRX = rxRepeat1 alpha
  label        <- fullToken  LABEL      $ labelRX . rxRepeat(from '0' to '9' : alpha)
  
  ---------------------------------------- NUMERICAL TYPES ----------------------------------------
  let from0to9  = from '0' to '9'
      plusMinus = rx[ch '+', ch '-']
      dot       = rx '.'
      number    = rxRepeat1 from0to9
  base10       <- fullToken  BASE10     $ number
  dotBase10    <- fullToken  DOTBASE10  $ dot . base10
  exponent     <- fullToken  EXPONENT   $ rx[ch 'e', ch 'E'] .
    cantFail "expecting exponent value after 'e' or 'E' character" . opt plusMinus . base10
  base2        <- fullToken  BASE2      $ (rx "0b" <> rx "0B") . base10
  base16       <- fullToken  BASE16     $ (rx "0x" <> rx "0X") .
    rxRepeat[from0to9, from 'A' to 'F', from 'a' to 'f']
  numType      <- fullToken  NUMTYPE    $ rx (map ch "UILRFfijs")
  intRefParser <- fullToken  INTREF     $ rx '$' . number
  let base10Parser = dotBase10 . opt exponent . opt numType <>
        base10 . opt (dotBase10 <> dot) . opt exponent . opt numType
  
  ---------------------------------------- STRING  LITERAL ----------------------------------------
  stringLit    <- fullToken  STRINGLIT  $ rx '"' .
    (fix $ \loop ->
      rxRepeat(invert [ch '"', ch '\\']) . (rx "\\" . rx anyChar . loop <> rx '"'))
  
  -------------------------------------- KEYWORDS AND GROUPING ------------------------------------
  keyStringTable $ words "( [ ] )"
  let com = rx ','
  comma           <- emptyToken COMMA       $ com
  keyStringTable $ words "$ @ -> . global local qtime static"
  keyStringTable (words "! - ~")
  keywords_groups <- keyStringTable $ words $ unwords $
    [ "{ data struct list set intmap dict array date time }"
    , "if else for in while with try catch continue break return throw"
    , "#{ }# [ ] ( )"
    , "global local qtime static"
    , "function func pattern pat rule BEGIN END EXIT"
    , "import imports require requires"
    ]
  [openBrace, closeBrace, openParen, closeParen] <- mapM keyString $ words "{ } ( )"
  
  -------------------------------------- DATA SPECIAL SYNTAX --------------------------------------
  dataRX       <- keyString "data"
  base64Data   <- fullToken  BASE64DATA $
    rxRepeat1[from 'A' to 'Z', from 'a' to 'z', from '0' to '9', ch '+', ch '/']
  dataLexer   <- pure $ dataRX . multiComs . openBrace .
    fix(\loop -> (base64Data<>space) . loop <> closeBrace <> rxErr "unknown token in base-64 data")
  
  ------------------------------------------- OPERATORS -------------------------------------------
  operators    <- keyStringTable $ words $ unwords $
    [allUpdateOpStrs, allArithOp1Strs, allArithOp2Strs, ": ;"]
  
  ------------------------------------ DATE/TIME SPECIAL SYNTAX -----------------------------------
  -- makes use of token types that have already been created above
  let year = rxLimitMinMax 4 5 from0to9
      dd   = rxLimitMinMax 1 2 from0to9
      col  = rx ':'
      hy   = rx '-'
      timeRX = dd . col . dd . col . dd . opt(dot . number)
  dateExpr <- fullToken DATE $
    year . hy . dd . hy . dd . opt(spaceRX . timeRX) . opt(spaceRX . labelRX)
  dateTag  <- keyString "date"
  date     <- pure $ dateTag . space . dateExpr
  timeExpr <- fullToken TIME timeRX
  timeTag  <- keyString "time"
  time     <- pure $ timeTag . space . timeExpr
  
  ------------------------------------------- ACTIVATE --------------------------------------------
  activate $
    [ space, inlineCom, endlineCom, comma, intRefParser, operators
    , stringLit, base16, base2, base10Parser
    , date, time, dataLexer
    , keywords_groups, label
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
  st{ bufferedComments = (if null coms then mzero else return coms) <> bufferedComments st }

failLater :: String -> Location -> DaoParser ()
failLater msg loc = catchError (fail msg) $ \err -> modify $ \st ->
  st{nonHaltingErrors =
      nonHaltingErrors st ++ [err{parseStateAtErr=Nothing, parseErrLoc = Just loc}]}

----------------------------------------------------------------------------------------------------

spaceComPTab :: DaoPTable [Comment]
spaceComPTab = table $
  [ tableItem SPACE      (return . const [] . as0)
  , tableItem INLINECOM  (\c -> return [InlineComment  $ asUStr c])
  , tableItem ENDLINECOM (\c -> return [EndlineComment $ asUStr c])
  ]

-- | Parses an arbitrary number of space and comment tokens, comments are returned. Backtracks if
-- there are no comments.
space :: DaoParser [Comment]
space = do
  st <- get
  case bufferedComments st of
    Just coms -> put (st{bufferedComments=mempty}) >> return coms
    Nothing   -> fmap concat $ many (evalPTable spaceComPTab)

-- The 'space' parser backtracks if there are no spaces, which is important to prevent infinite
-- recursion in some situations. The 'optSpace' evalautes 'space' but returns an empty list of
-- 'space' backtracks, so 'optSpace' never backtracks.
optSpace :: DaoParser [Comment]
optSpace = mplus space (return [])

-- | Evaluates a 'DaoParser' within a cluster of optional spaces and comments, returning the result
-- of the parser wrapped in a 'Dao.Object.Com' constructor. If the given 'DaoParser' backtracks, the
-- comments that were parsed before the 'DaoParser' was evaluated are buffered so a second call two
-- successive calls to this function return immediately. For example in an expression like:
-- > 'Control.Monad.msum' ['commented' p1, 'commented' p2, ... , 'commented' pN]
-- The spaces and comments occurring before the parsers @p1@, @p2@, ... , @pN@ are only being parsed
-- once, no matter how many parser are tried.
commented :: DaoParser a -> DaoParser (Com a)
commented parser = do
  before <- mplus space (return [])
  flip mplus (setCommentBuffer before >> mzero) $ do
    result <- parser
    after  <- mplus space (return [])
    return (com before result after)

-- Take comments of the stream, but do not return them, instead just buffer them. This is a good way
-- to do a look-ahead past comments without deleting comments. If the next parser evaluated
-- immediately after this one is 'commented', the comments buffered by this function will be
-- returned with the object parsed by 'commented'. This is necessary in parse tables where the table
-- needs an operator token to select the next parser in the table, but the returned operator token
-- must be preceeded by possible comments.
bufferComments :: DaoParser ()
bufferComments = mplus (space >>= setCommentBuffer) (return ())

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
  let frac         = maybe "" id (maybFrac >>= stripPrefix ".")
      strprfx      = foldl (\f s t -> f (maybe t id (stripPrefix s t))) id . words
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
diffTimeFromStrs :: String -> DaoParser T_diffTime
diffTimeFromStrs time = do
  let [days,hours,minutes,secMils] = split [] time
      (seconds, miliseconds) = break (=='.') secMils
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

numberPTab :: DaoPTable AST_Object
numberPTab = table $
  [ base 16 BASE16
  , base  2 BASE2
  , tableItem BASE10    $ \tok -> do
      frac <- optional (token DOTBASE10 id)
      exp  <- optional (token EXPONENT  id)
      typ  <- optional (token NUMTYPE   id)
      done tok 10 (asString tok) (mstr frac) (mstr exp) (mstr typ) (return (asLocation tok) <> mloc frac <> mloc exp <> mloc typ)
  , tableItem DOTBASE10 $ \tok -> do
      exp  <- optional (token EXPONENT id)
      typ  <- optional (token NUMTYPE  id)
      done tok 10 "" (Just (asString tok)) (mstr exp) (mstr typ) (return (asLocation tok) <> mloc exp <> mloc typ)
  ]
  where
    mloc = fmap asLocation
    mstr = fmap asString
    ignore :: DaoParser (Maybe String)
    ignore   = return Nothing
    base b t = tableItem t $ \tok -> do
      typ <- optional (token NUMTYPE id)
      done tok b (drop 2 (asString tok)) Nothing Nothing (mstr typ) (return (asLocation tok) <> mloc typ)
    done tok base int frac exp typ loc = do
      num <- numberFromStrs base int frac exp typ
      let endLoc = asLocation tok
      return (AST_Literal num (maybe endLoc id loc))

-- | Parsing numerical literals
number :: DaoParser AST_Object
number = evalPTable numberPTab

singletonPTab :: DaoPTable AST_Object
singletonPTab = numberPTab <> singlPTab where
  literal constr tok = return (AST_Literal (constr tok) (asLocation tok))
  singlPTab = table $
    [ tableItem INTREF    (literal $ ORef . IntRef   . read . tail . asString)
    , tableItem STRINGLIT (literal $ OString         . read        . asString)
    , tableItemBy "("    $ \tok -> do
        obj <- commented equation
        expect "closing parentheses" $ do
          endloc <- tokenBy ")" asLocation
          return (AST_Paren obj (asLocation tok <> endloc))
    , tableItemBy "date" $ \startTok ->
        expect "date/time value expression after \"date\" statement" $ do
          token SPACE as0
          tok <- token DATE id
          case readsPrec 0 (asString tok) of
            [(obj, "")] -> shift as0 >>
              return (AST_Literal (OTime obj) (asLocation startTok <> asLocation tok))
            _     -> unshift tok >> fail "invalid UTC-time expression"
    , tableItemBy "time" $ \startTok ->
        expect "UTC-time value after \"time\" statement" $ do
          token SPACE as0
          tok  <- token TIME id
          time <- diffTimeFromStrs (asString tok)
          return (AST_Literal (ODiffTime time) (asLocation startTok <> asLocation tok))
    , container "list" return
    , container "set"  return
    , (\lbl -> container lbl (checkAssign lbl)) "dict"
    , (\lbl -> container lbl (checkAssign lbl)) "intmap"
    , tableItemBy "array" $ \startTok -> do
        let arrBounds = expect "array bounds expression" $ do
              startLoc <- tokenBy "(" asLocation
              (bnds, endLoc) <- commaSepd "bound value for \"array\" statement" ")" equation
              case bnds of
                [a, b] -> return ()
                _      ->
                  failLater "array boundaries declratation should have only two expressions" $
                    startLoc <> endLoc
              return bnds
        bnds <- commented arrBounds
        let initMsg = "list of items to initialize array declaration"
        expect initMsg $ do
          tokenBy "{" as0
          (items, endLoc) <- commaSepd initMsg "}" equation
          return (AST_Array bnds items (asLocation startTok <> endLoc))
    , tableItemBy "struct" $ \startTok -> do
        initObj <- commented equation
        let noBracedItems =  return $
              AST_Struct initObj [] (asLocation startTok <> getLocation (unComment initObj))
        flip mplus noBracedItems $ do
          tokenBy "{" as0
          (items, endLoc) <- commaSepd "list of items to initialize struct declaration" "}" equation
          return (AST_Struct initObj items (asLocation startTok <> endLoc))
    ]
  checkAssign lbl obj = do
    case obj of
      AST_Assign _ _ _ _ -> return ()
      obj                -> do
        failLater ("non-assignment expression in \""++lbl++"\" statement") (getLocation obj)
    return obj

container :: UStrType lbl =>
  lbl -> (AST_Object -> DaoParser AST_Object) -> DaoTableItem AST_Object
container lbl constr = tableItemBy lbl $ \startTok -> do
  let typeStr = uchars (ustr lbl)
      msg = "bracketed list of items for \""++typeStr++"\" statement"
  com1 <- space
  (items, endLoc) <- expect msg $ do
    startLoc <- tokenBy "{" as0
    commaSepd msg "}" (equation >>= constr)
  return (AST_Dict (ustr lbl) com1 items (asLocation startTok <> endLoc))

-- Objects that are parsed as a single value, which includes all literal expressions and equtions in
-- parentheses.
singleton :: DaoParser AST_Object
singleton = evalPTable singletonPTab

equationConstructor :: AST_Object -> (Location, Com ArithOp2) -> AST_Object -> AST_Object
equationConstructor left (loc, op) right = AST_Equation left op right loc

referencePTab :: DaoPTable AST_Object
referencePTab = table $ map mkPar ["$", "@"] ++ labeled where
  mkPar opStr = tableItemBy opStr $ \tok -> do
    let as = read . tokTypeToString . asTokType
    obj <- commented reference
    return (AST_Prefix (as tok) obj ((getLocation (unComment obj)) <> asLocation tok))
  labeled = (:[]) $ tableItem LABEL $ \tok -> do
    let initObj = AST_Literal (ORef (LocalRef (asUStr tok))) (asLocation tok)
    flip mplus (return initObj) $
      simpleInfixedWithInit "label after dot or arrow operator" rightAssoc
        equationConstructor
        (return initObj)
        (flip mplus singleton (reference >>= \o -> bufferComments >> return o))
        (liftM2 (,) (look1 asLocation) (commented (evalPTable infixPTab)))
  infixPTab :: DaoPTable ArithOp2
  infixPTab = table $ flip fmap [".", "->"] $
    flip tableItemBy (return . read . tokTypeToString . asTokType)

reference :: DaoParser AST_Object
reference = evalPTable referencePTab

-- A 'reference' statement qualified by a prefix like "global", "local", "qtime", or "static".
qualReferencePTab :: DaoPTable AST_Object
qualReferencePTab = table $ flip fmap (words "global local qtime static .") $ \pfx ->
  tableItemBy pfx $ \tok -> do
    obj <- commented reference
    return (AST_Prefix (read (tokTypeToString (asTokType tok))) obj (asLocation tok <> (getLocation (unComment obj))))

----------------------------------------------------------------------------------------------------

commaSepd :: (UStrType str, UStrType errmsg) =>
  errmsg -> str -> DaoParser AST_Object -> DaoParser ([Com AST_Object], Location)
commaSepd errMsg close parser =
  msum [commented parser >>= loop . (:[]), parseComEmpty, parseClose [] [], err] where
    parseComEmpty = space >>= parseClose []
    parseClose stack c = do
      loc <- tokenBy close asLocation
      return (if null c then stack else stack++[com c AST_Void []], loc)
    loop stack = flip mplus (parseClose stack []) $ do
      token COMMA as0
      expect errMsg (commented parser >>= \o -> loop (stack++[o]))
    err = fail $ "unknown token while parsing list of items for "++uchars (ustr errMsg)

-- This is 'reference' expression possibly followed by square brackets or parentheses. If there is
-- no juxtapose, just return the 'reference' expression. This covers function call expressions and
-- array subscript expressions. Because there is no delimiting token, this table is a combination of
-- all the tables of higher prescedence than it, which are 'singletonPTab', 'referencePTab' and
-- 'qualReferencePTab'. Tables of lower prescedence can include this table, but should not include
-- those tables (it would be redundant).
funcCallArraySubPTab :: DaoPTable AST_Object
funcCallArraySubPTab =
  bindPTable (mconcat [singletonPTab, refPTab, qualReferencePTab]) $ \obj ->
    flip mplus (return obj) $ bufferComments >> evalPTable (juxtaposedPTab obj)
  where
    refPTab = bindPTable referencePTab (\obj -> return obj)
    bracket obj (open:close:[], constr) = tableItemBy [open] $ \tok -> do
      coms <- space
      (args, endloc) <- commaSepd "argument to function" [close] equation
      return (constr obj coms args (asLocation tok <> endloc))
    juxtaposedPTab obj = table $ fmap (bracket obj) [("()", AST_FuncCall), ("[]", AST_ArraySub)]

funcCallArraySub :: DaoParser AST_Object
funcCallArraySub = evalPTable funcCallArraySubPTab

arithPrefixPTab :: DaoPTable AST_Object
arithPrefixPTab = table $ flip fmap ["~", "-"] $ \pfxOp -> tableItemBy pfxOp $ \tok -> do
  obj <- commented reference
  return (AST_Prefix (read (tokTypeToString (asTokType tok))) obj (asLocation tok))

arithPrefix :: DaoParser AST_Object
arithPrefix = evalPTable arithPrefixPTab

-- This table extends the 'funcCallArraySubPTab' table with the 'arithPrefixPTab' table. It is the
-- most complicated (and therefore lowest prescedence) object expression that can be formed without
-- making use of infix operators.
objectPTab :: DaoPTable AST_Object
objectPTab = mappend funcCallArraySubPTab arithPrefixPTab

-- Evaluates 'objectPTab' to a 'DaoParser'.
object :: DaoParser AST_Object
object = evalPTable objectPTab

-- Parses a sequence of 'object' expressions interspersed with arithmetic infix opreators.
-- All infixed logical operators are included, assignment operators are not. The only prefix logical
-- operator, Logical NOT @(!)@ is not parsed here but in the 'logical' function.
arithOpTable :: OpTableParser DaoParState DaoTT (Location, Com ArithOp2) AST_Object
arithOpTable =
  newOpTableParser "arithmetic expression" False
    (\tok -> do
        op <- commented (shift (read . tokTypeToString . asTokType))
        return (asLocation tok, op)
    )
    (object >>= \o -> bufferComments >> return o)
    (\left (loc, op) right -> AST_Equation left op right loc)
    ( opRight ["**"] equationConstructor
    : fmap (\ops -> opLeft (words ops) equationConstructor)
        ["* / %", "+ -", "<< >>", "&", "^", "|", "< <= >= >", "== !=", "&&", "||"]
    )

arithmeticPTab :: DaoPTable AST_Object
arithmeticPTab = ordinary <> table [prefixedWithLogicalNOT] where
  ordinary = bindPTable objectPTab $ \obj ->
    evalOpTableParserWithInit (bufferComments >> return obj) arithOpTable
  prefixedWithLogicalNOT = tableItemBy "!" $ \op -> do
    obj <- commented arithmetic
    return (AST_Prefix (read $ tokTypeToString $ asTokType op) obj (asLocation op))

-- Evalautes the 'arithOpTable' to a 'DaoParser'.
arithmetic :: DaoParser AST_Object
arithmetic = evalPTable arithmeticPTab

equationPTab :: DaoPTable AST_Object
equationPTab = bindPTable arithmeticPTab $ \obj ->
  simpleInfixedWithInit "object expression for assignment operator" rightAssoc
    (\left (loc, op) right -> AST_Assign left op right loc)
    (bufferComments >> return obj)
    arithmetic
    (liftM2 (,) (look1 asLocation) (commented (evalPTable opTab)))
  where
    opTab :: DaoPTable UpdateOp
    opTab = table $
      fmap (flip tableItemBy (return . read . tokTypeToString . asTokType)) (words allUpdateOpStrs)

-- | Evaluates a sequence arithmetic expressions interspersed with assignment operators.
equation :: DaoParser AST_Object
equation = evalPTable equationPTab

----------------------------------------------------------------------------------------------------

bracketed :: String -> DaoParser ([AST_Script], Location)
bracketed msg = do
  startLoc <- tokenBy "{" asLocation
  scrps <- mplus (many script) (return [])
  expect ("curly-bracket to close "++msg++" statement") $ do
    endLoc <- tokenBy "}" asLocation
    return (scrps, startLoc<>endLoc)

script :: DaoParser AST_Script
script = evalPTable scriptPTab

scriptPTab :: DaoPTable AST_Script
scriptPTab = comments <> objExpr <> table exprs where
  objExpr = bindPTable equationPTab $ \obj -> do
    coms <- optSpace
    expect "semicolon after object expression" $ do
      endLoc <- tokenBy ";" asLocation
      return (AST_EvalObject obj coms (getLocation obj <> endLoc))
  comments = bindPTable spaceComPTab $ \c1 -> optSpace >>= \c2 -> return (AST_Comment (c1++c2))
  exprs =
    [ if_tableItem
    , tableItemBy "return"   (returnExpr True)
    , tableItemBy "throw"    (returnExpr False)
    , tableItemBy "continue" (continExpr True)
    , tableItemBy "break"    (continExpr False)
    , tableItemBy "try"   $ \tok -> expect "bracketed script after \"try\" statement" $ do
        tryCom <- commented (bracketed "\"try\" statement")
        let (try, endLoc) = unComment tryCom
        tryCom <- return (fmap (const try) tryCom)
        let done comName catch endLoc = return $
              AST_TryCatch (fmap (fmap Com) tryCom) comName catch (asLocation tok <> endLoc)
        flip mplus (done (Com nil) [] endLoc) $ do
          tokenBy "catch" as0
          expect "varaible name after \"catch\" statement" $ do
            comName <- commented (token LABEL asUStr)
            (catch, endLoc) <- bracketed "\"catch\" statement"
            done comName (fmap Com catch) endLoc
    , tableItemBy "for"   $ \tok -> expect "iterator label after \"for statement\"" $ do
        comName <- commented (token LABEL asUStr)
        expect "\"in\" statement after \"for\" statement" $ do
          tokenBy "in" as0
          expect "object expression over which to iterate of \"for-in\" statement" $ do
            obj <- commented equation
            expect "bracketed script after \"for-in\" statement" $ do
              (for, endLoc) <- bracketed "\"for\" statement"
              return (AST_ForLoop comName obj (fmap Com for) (asLocation tok <> endLoc))
    , tableItemBy "while" $ \tok -> expect "conditional expression after \"while\" statement" $ do
        obj <- commented equation
        expect "bracketed script after \"while\" statement" $ do
          (while, endLoc) <- bracketed "\"while\" statement"
          return (AST_WhileLoop obj (fmap Com while) (asLocation tok <> endLoc))
    , tableItemBy "with"  $ \tok -> expect "reference expression after \"with\" statement" $ do
        obj <- commented equation
        expect "bracketed script after \"with\" statement" $ do
          (with, endLoc) <- bracketed "\"with\" statement"
          return (AST_WithDoc obj (fmap Com with) (asLocation tok <> endLoc))
    ]
  semicolon = tokenBy ";" asLocation
  returnExpr isReturn tok = do
    obj    <- commented equation
    endLoc <- semicolon
    return (AST_ReturnExpr isReturn obj (asLocation tok <> endLoc))
  continExpr isContin tok = do
    let startLoc = asLocation tok
    let msg e = concat $
          [e, " after ", if isContin then "\"continue" else "\"break", "-if\" statement"]
    coms <- optSpace
    msum $
      [do endLoc <- semicolon
          return (AST_ContinueExpr isContin coms (Com AST_Void) (startLoc<>endLoc))
      ,do tokenBy "if" as0
          expect (msg "conditional expression") $ do
            obj <- commented equation
            expect (msg "semicolon") $ do
              endLoc <- semicolon
              return (AST_ContinueExpr isContin coms obj (startLoc<>endLoc))
      , fail (msg "expecting optional object expression followed by a semicolon")
      ]

if_tableItem :: DaoTableItem AST_Script
if_tableItem = tableItemBy "if" $ \tok -> expect "conditional expression after \"if\" statement" $ do
  coms <- optSpace
  expect "object expression after \"if\" statement" $ do
    coms <- optSpace
    obj  <- equation
    expect "bracketed script expressions after \"if\" statement" $ do
      (comThen, thenLoc) <- fmap (\ (thn, loc) -> (com coms thn [], loc)) $ bracketed "if statement"
      (comElse, elseLoc) <- elseStatement
      return (AST_IfThenElse coms obj (fmap (fmap Com) comThen) (fmap (fmap Com) comElse) (asLocation tok <> thenLoc <> elseLoc))

elseStatement :: DaoParser (Com [AST_Script], Location)
elseStatement = do
  com1 <- optSpace
  flip mplus (return (com com1 [] [], LocationUnknown)) $ do
    startLoc <- tokenBy "else" asLocation
    com2     <- optSpace
    let done (els, endLoc) = return (com com1 els com2, startLoc<>endLoc)
    -- The comments for the second bracketed script of AST_IfThenElse aren't surrounding the whole
    -- bracketed expression, only the "else" statement token. This needs to be considered when
    -- creating syntax trees with quickcheck, and when instantiating the pretty printer.
    expect "curly-bracketed script or another \"if\" statement after \"else\" statement" $
      mplus (bracketed "\"else\" statement" >>= done) $ do
        elsif <- evalPTableItem if_tableItem
        return (com com1 [elsif] com2, startLoc <> getLocation elsif)

----------------------------------------------------------------------------------------------------

toplevelPTab :: DaoPTable AST_TopLevel
toplevelPTab = comments <> scriptExpr <> table expr where
  comments = bindPTable spaceComPTab (\c1 -> optSpace >>= \c2 -> return (AST_TopComment (c1++c2)))
  scriptExpr = bindPTable scriptPTab $ \obj -> return (AST_TopScript obj (getLocation obj))
  expr =
    [ tableItemBy "func"    function, tableItemBy "function" function
    , tableItemBy "BEGIN"   event   , tableItemBy "END"      event   , tableItemBy "EXIT" event
    , tableItemBy "pattern" pattern , tableItemBy "pat"      pattern , tableItemBy "rule" pattern
    ]
  singlePattern = commented equation >>= \eqn -> return ([eqn], getLocation (unComment eqn))
  function tok = do
    let exprType = '"':asString tok++"\""
    comName <- commented (token LABEL asUStr)
    expect ("list of patterns for \""++asString tok++"\" statement") $ do
      startLoc <- tokenBy "(" asLocation
      (pats, endLoc) <- commaSepd ("pattern for "++exprType++" statement") ")" equation
      expect ("bracketed script expression for "++exprType++" statement") $ do
        (scrp, endLoc) <- bracketed ('"':asString tok++"\" statement")
        return (AST_TopFunc comName pats (Com (fmap Com scrp)) (asLocation tok <> endLoc))
  event   tok = do
    let exprType = show (asTokType tok)
    coms <- optSpace
    expect ("bracketed script after \""++exprType++"\" statement") $ do
      (event, endLoc) <- bracketed ('"':exprType++"\" statement")
      return (AST_Event (read exprType) (Com (fmap Com event)) (asLocation tok <> endLoc))
  pattern tok = do
    let exprType = show (asTokType tok)
    expect ("pattern expression for \""++exprType++"\"") $ do
      comPat <- commented $ flip mplus singlePattern $ do
        startLoc <- tokenBy "(" asLocation
        commaSepd ("pattern for \""++exprType++"\" expression") ")" equation
      let (pats, endLoc) = unComment comPat
          rule = fmap (const pats) comPat
      expect ("bracketed script after \""++exprType++"\" statement") $ do
        (action, endLoc) <- bracketed ('"':exprType++"\" statement")
        return (AST_TopLambda (read exprType) rule (fmap Com action) (asLocation tok <> endLoc))

toplevel :: DaoParser AST_TopLevel
toplevel = evalPTable toplevelPTab

----------------------------------------------------------------------------------------------------

daoParser :: DaoParser AST_SourceCode
daoParser = do
  let loop dx = msum
        [ isEOF >>= guard >> return dx
        , toplevel >>= \d -> loop (dx++[d])
        , fail "syntax error on token"
        ]
  src <- loop []
  return (AST_SourceCode{sourceModified=0, sourceFullPath=nil, directives=src})

daoGrammar :: Language DaoParState DaoTT AST_SourceCode
daoGrammar = newLanguage 4 $ mplus daoParser $ fail "Parser backtracked without taking all input."

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

