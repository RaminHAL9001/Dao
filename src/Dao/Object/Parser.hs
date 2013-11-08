-- "src/Object/Parser.hs" makes use of "Dao.Parser" to parse
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

{-# LANGUAGE MultiParamTypeClasses #-}

module Dao.Object.Parser where

import           Dao.String
import           Dao.Token
import           Dao.PPrint
import           Dao.Object.PPrint
import           Dao.Object hiding (Tokenizer, get, put)
import           Dao.Object.AST
import           Dao.Predicate
import           Dao.Parser
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
type DaoTableItem a = TableItem DaoTT (DaoParser a)
type DaoPTable    a = PTable    DaoTT (DaoParser a)
type DaoParseErr    = Error     DaoParState DaoTT

daoTokenDB :: TokenDB DaoTT
daoTokenDB = makeTokenDB daoTokenDef

data DaoTokenLabel
  = SPACE | INLINECOM | ENDLINECOM | COMMA | LABEL | STRINGLIT | CHARLIT | DATE | TIME
  | BASE10 | DOTBASE10 | NUMTYPE | EXPONENT | BASE16 | BASE2
  deriving (Eq, Enum, Show, Read)
instance UStrType DaoTokenLabel where { toUStr = derive_ustr; fromUStr = derive_fromUStr; }

daoTokenDef :: LexBuilder DaoTT
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
      labelRX = rxRepeat1 alpha . rxRepeat(from '0' to '9' : alpha)
  label        <- fullToken  LABEL      $ labelRX
  
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
  point        <- operator "."
  let base10Parser = mconcat $
        [ dotBase10 . opt exponent . opt numType
        , base10 . opt dotBase10 . opt exponent . opt numType
        , base10 . dot . exponent . opt numType
        , base10 . opt exponent . opt numType
        ]
  ---------------------------------------- STRING  LITERAL ----------------------------------------
  let litExpr op =  rx op .  (fix $ \loop ->
        rxRepeat(invert [ch op, ch '\\']) . (rx "\\" . rx anyChar . loop <> rx op))
  stringLit    <- fullToken  STRINGLIT $ litExpr '"'
  charLit      <- fullToken  CHARLIT   $ litExpr '\''
  -------------------------------------- KEYWORDS AND GROUPING ------------------------------------
  openers <- operatorTable $ words "( [ { {#"
  -- trace ("opener tokens ( [ { {#   ---> "++show openers) $ return ()
  comma           <- emptyToken COMMA (rx ',')
  operatorTable (words "$ @ -> . ! - ~")
  daoKeywords <- keywordTable LABEL labelRX $ words $ unwords $
    [ "global local qtime static"
    , "null false true tree date time function func rule"
    , "if else for in while with try catch continue break return throw"
    , "global local qtime static"
    , "BEGIN END EXIT import require"
    , "struct union operator public private new" -- other reserved keywords, but they don't do anything yet.
    ]
  let withKeyword key func = do
        tok <- getTokID key :: LexBuilderM DaoTT
        return (rx key . (label <> rxEmptyToken tok . func))
  closers <- operatorTable $ words "#} } ] )"
  [openBrace, closeBrace, openParen, closeParen] <- mapM operator (words "{ } ( )")
  -- trace ("openBrace = "++show openBrace) $ return ()
  
  ------------------------------------------- OPERATORS -------------------------------------------
  operators    <- operatorTable $ words $ unwords $
    [allUpdateOpStrs, allPrefixOpStrs, allInfixOpStrs, ": ;"]
  -- trace ("operators: "++show operators) $ return ()
  
  ------------------------------------ DATE/TIME SPECIAL SYNTAX -----------------------------------
  -- makes use of token types that have already been created above
  let year = rxLimitMinMax 4 5 from0to9
      dd   = rxLimitMinMax 1 2 from0to9
      col  = rx ':'
      hy   = rx '-'
      timeRX = dd . col . dd . col . dd . opt(dot . number)
  timeExpr <- fullToken TIME timeRX
  time <- withKeyword "time" $ cantFail "time expression" . space . timeExpr
  dateExpr <- fullToken DATE $ year . hy . dd . hy . dd
  date <- withKeyword "date" $ cantFail "date expression" .
    space . dateExpr . opt(space . timeExpr) . opt(space . label)
  
  ------------------------------------------- ACTIVATE --------------------------------------------
  -- activate $
  return $ regexToTableLexer $
    [ space, inlineCom, endlineCom, comma
    , stringLit, charLit, base16, base2, base10Parser
    , operators, openers, closers
    , date, time, daoKeywords
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
  st <- Control.Monad.State.get
  case bufferedComments st of
    Just coms -> put (st{bufferedComments=mempty}) >> return coms
    Nothing   -> fmap concat $ many (joinEvalPTable spaceComPTab)

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

-- copied from the Dao.OldParser module
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
    "s" -> return $ ORelTime (fromRational r)
    ""  ->
      return (OInt (fromIntegral (round r)))
--    if r_is_an_integer && null frac
--      then
--        let i = round r
--        in  if fromIntegral (minBound::T_int) <= i && i <= fromIntegral (maxBound::T_int)
--              then  return $ OInt $ fromIntegral i
--              else  return $ OLong i
--      else return (ORatio r)
    typ -> fail ("unknown numeric type "++show typ)

----------------------------------------------------------------------------------------------------

-- | Compute diff times from strings representing days, hours, minutes, and seconds. The seconds
-- value may have a decimal point.
diffTimeFromStrs :: String -> DaoParser T_diffTime
diffTimeFromStrs time = do
  let [hours,minutes,secMils] = split [] time
      (seconds, dot_mils) = break (=='.') secMils
      miliseconds = dropWhile (=='.') dot_mils
  hours   <- check "hours"             24   hours
  minutes <- check "minutes"           60   minutes
  let sec =  check "seconds"           60
  seconds <-
    if null miliseconds
      then  sec seconds
      else  do
        seconds <- sec seconds
        return (seconds + rint miliseconds % (10 ^ length miliseconds))
  return $ fromRational (60*60*hours + 60*minutes + seconds)
  where
    split buf str       = case break (==':') str of
      (t, ""     ) -> reverse $ take 3 $ (t:buf) ++ repeat ""
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

numberPTabItems :: [DaoTableItem AST_Object]
numberPTabItems = 
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

numberPTab :: DaoPTable AST_Object
numberPTab = table numberPTabItems

-- | Parsing numerical literals
number :: DaoParser AST_Object
number = joinEvalPTable numberPTab

singletonPTab :: DaoPTable AST_Object
singletonPTab = table singletonPTabItems

parenPTabItem :: DaoTableItem AST_Paren
parenPTabItem = tableItemBy "("    $ \tok -> do
  obj <- commented equation
  expect "closing parentheses" $ do
    endloc <- tokenBy ")" asLocation
    return (AST_Paren obj (asLocation tok <> endloc))

paren :: DaoParser AST_Paren
paren = joinEvalPTableItem parenPTabItem

singletonPTabItems :: [DaoTableItem AST_Object]
singletonPTabItems = numberPTabItems ++ fmap (fmap (fmap AST_ObjQualRef)) qualReferencePTabItems ++
  [ tableItem STRINGLIT (literal $ OString . read     . asString)
  , tableItem CHARLIT   (literal $ OChar   . read     . asString)
  , fmap (fmap AST_ObjParen) parenPTabItem
  , tableItemBy "{#" $ \startTok -> expect "object expression after open {# meta-eval brace" $ do
      scrp <- fmap AST_CodeBlock (many script)
      expect "closing #} meta-eval brace" $ do
        endLoc <- tokenBy "#}" asLocation
        return (AST_MetaEval scrp (asLocation startTok <> endLoc))
  , trueFalse "null" ONull, trueFalse "false" ONull, trueFalse "true" OTrue
  , reserved "operator", reserved "public", reserved "private", reserved "new"
  ]
  where
    literal constr tok = return (AST_Literal (constr tok) (asLocation tok))
    trueFalse lbl obj = tableItemBy lbl $ \tok -> return (AST_Literal obj (asLocation tok))
    reserved key = tableItemBy key $ fail $
      "keyword "++show key++" is reserved for future use, not implemented in this version of Dao"

-- Objects that are parsed as a single value, which includes all literal expressions and equtions in
-- parentheses.
singleton :: DaoParser AST_Object
singleton = joinEvalPTable singletonPTab

prefixedSingletonPTabItems :: [DaoTableItem AST_Object]
prefixedSingletonPTabItems = [prefixOp "$", prefixOp "@"] where
  prefixOp op = tableItemBy op $ \tok -> case maybeFromUStr (asUStr tok) of
    Nothing -> error ("token "++show tok++" used as prefix operator")
    Just op -> do
      ref <- commented qualReference
      let loc = asLocation tok <> getLocation (unComment ref)
      return (AST_Prefix op (fmap AST_ObjQualRef ref) loc)

prefixedSingletonPTab :: DaoPTable AST_Object
prefixedSingletonPTab = table prefixedSingletonPTabItems

prefixedSingleton :: DaoParser AST_Object
prefixedSingleton = joinEvalPTable prefixedSingletonPTab

-- Objects that are parsed as a single value but which are constructed from other object
-- expressions. This table excludes 'singletonPTab'.
containerPTab :: DaoPTable AST_Object
containerPTab = table $
  [ tableItemBy "date" $ \startTok ->
      expect "date/time value expression after \"date\" statement" $ do
        token SPACE as0
        date <- token DATE id
        let optsp tok = optional $
              token SPACE id >>= \s -> mplus (token tok id) (unshift s >> mzero)
        time <- optsp TIME
        zone <- optsp LABEL
        let loc = asLocation startTok <>
                maybe LocationUnknown id (fmap asLocation time <> fmap asLocation zone)
            astr = (' ':) . asString
            timeAndZone = maybe " 00:00:00" astr time ++ maybe "" astr zone
        case readsPrec 0 (asString date ++ timeAndZone) of
          [(obj, "")] -> return (AST_Literal (OAbsTime obj) loc)
          _           -> fail "invalid UTC-time expression"
  , tableItemBy "time" $ \startTok -> expect "UTC-time value after \"time\" statement" $ do
      token SPACE as0
      tok  <- token TIME id
      time <- diffTimeFromStrs (asString tok)
      return (AST_Literal (ORelTime time) (asLocation startTok <> asLocation tok))
  , tableItemBy "tree" $ \startTok -> do
      initObj <- commented (equation <|> return AST_Void)
      let endLoc = case unComment initObj of
            AST_Void -> asLocation startTok
            obj      -> getLocation obj
      let noBracedItems =  return (AST_Struct initObj mempty (asLocation startTok <> endLoc))
      flip mplus noBracedItems $ do
        tokenBy "{" as0
        (items, endLoc) <- commaSepd "item in list for initializing struct declaration" "}" (return . com [] AST_Void) equation id
        return (AST_Struct initObj (mkObjList items) (asLocation startTok <> endLoc))
  , tableItemBy "rule" $ \startTok -> expect "list of strings after \"rule\" statement" $ do
      lst <- commented $ joinEvalPTable $ table $
        [ tableItem STRINGLIT $ \str -> return $ AST_StringList [Com (asUStr str)] (asLocation str)
        , tableItemBy "(" $ \openTok -> do
            let lu = LocationUnknown
            (lst, loc) <- commaSepd "strings for rule header" ")" (flip AST_NoStrings lu) (token STRINGLIT asUStr) (flip AST_StringList lu)
            return (setLocation lst (asLocation openTok <> loc))
        ]
      expect "bracketed expression after rule header" $ do
        (scrpt, endLoc) <- bracketed ("script expression for \"rule\" statement")
        return $ AST_Rule lst scrpt (asLocation startTok <> endLoc)
  , lambdaFunc "func", lambdaFunc "function"
  ]
  where
    checkAssign lbl obj = do
      case obj of
        AST_Assign{} -> return ()
        obj          ->
          failLater ("non-assignment expression in \""++lbl++"\" statement") (getLocation obj)
      return obj
    lambdaFunc lbl = tableItemBy lbl $ \startTok ->
      expect ("parameters and bracketed script after \""++lbl++"\" statement") $ do
        constr <- mplus (pure AST_Func <*> optSpace <*> token LABEL asName) (return AST_Lambda)
        params <- commented paramList
        (scrpt, endLoc) <- bracketed ("script expression after \""++lbl++"\" statement")
        return $ constr params scrpt (asLocation startTok <> endLoc)

-- None of the functions related to parameters and type checking parse with tables because there is
-- simply no need for it according to the Dao language syntax.
typeCheckParser :: a -> DaoParser (AST_TyChk a)
typeCheckParser a = flip mplus (return (AST_NotChecked a)) $ do
  com1 <- commented (tokenBy ":" id)
  let startLoc = asLocation (unComment com1)
  expect "type expression after colon operator" $ equation >>= \obj -> return $
    AST_Checked a (fmap as0 com1) obj (startLoc <> getLocation obj)

typeCheckedName :: DaoParser (AST_TyChk Name)
typeCheckedName = token LABEL id >>= \tok ->
  fmap (\tychk -> setLocation tychk (asLocation tok <> getLocation tychk)) $
    typeCheckParser (asName tok)

parameter :: DaoParser AST_Param
parameter = msum $
  [ do  startLoc <- tokenBy "$" asLocation
        coms     <- optional space
        item     <- typeCheckedName
        return $ AST_Param coms item (startLoc <> getLocation item)
  , typeCheckedName >>= \nm -> return $ AST_Param Nothing nm (getLocation nm)
  ]

paramList :: DaoParser AST_ParamList
paramList = do
  startLoc   <- tokenBy "(" asLocation
  (lst, loc) <- commaSepd "parameter value" ")" (return . com [] AST_NoParams) parameter id
  lst        <- typeCheckParser lst
  return (AST_ParamList lst (startLoc <> loc))

singletonOrContainerPTab :: DaoPTable AST_Object
singletonOrContainerPTab = singletonPTab <> containerPTab

singletonOrContainer :: DaoParser AST_Object
singletonOrContainer = joinEvalPTable singletonOrContainerPTab

--referencePTab :: DaoPTable AST_Ref
--referencePTab = table (map mkPar ["$", "@"]) where
--  mkPar opStr = tableItemBy opStr $ \tok -> do
--    let as = fromUStr . tokTypeToUStr . asTokType
--    coms <- optSpace
--    obj  <- commented reference
--    return (AST_RefOp (as tok) coms obj)

referencePTabItem :: DaoTableItem AST_Ref
referencePTabItem = tableItem LABEL $ \init -> fmap constr $
  simpleInfixedWithInit "reference expression" rightAssoc chain
    (asName init)
    (token LABEL id >>= asName)
    (commented (tokenBy "." as0))
  where
    constr :: ([Com Name], Location) -> AST_Ref
    constr (nx, loc) = case nx of
      []   -> AST_RefNull
      n:nx -> AST_Ref (unComment n) nx loc
    chain (l, lloc) op (r, rloc) = (l++r, lloc<>rloc)
    asName :: TokenAt DaoTT -> DaoParser ([Com Name], Location)
    asName tok = case maybeFromUStr (asUStr tok) of
      Just  n -> return ([Com n], asLocation tok)
      Nothing -> fail ("expecting label, but token "++show (asString tok)++" is not a valid label")

referencePTab :: DaoPTable AST_Ref
referencePTab = table [referencePTabItem]

reference :: DaoParser AST_Ref
reference = joinEvalPTableItem referencePTabItem

-- A 'reference' statement, or a 'reference' statement qualified by a prefix like "global", "local",
-- "qtime", or "static".
qualReferencePTabItems :: [DaoTableItem AST_QualRef]
qualReferencePTabItems = (fmap (fmap AST_Unqualified) referencePTabItem :) $
  flip fmap (words "global local qtime static .") $ \pfx ->
    tableItemBy pfx $ \tok -> do
      coms <- optSpace
      obj  <- reference
      let loc  = asLocation tok <> getLocation obj
      let qual = fromUStr (tokTypeToUStr (asTokType tok))
      return (AST_Qualified qual coms obj loc)

qualReferencePTab :: DaoPTable AST_QualRef
qualReferencePTab = table qualReferencePTabItems

qualReference :: DaoParser AST_QualRef
qualReference = joinEvalPTable qualReferencePTab

----------------------------------------------------------------------------------------------------

commaSepd :: (UStrType str, UStrType errmsg) =>
  errmsg -> str -> ([Comment] -> b) -> DaoParser a -> ([Com a] -> b) -> DaoParser (b, Location)
commaSepd errMsg close voidVal parser constr =
  msum [commented parser >>= loop . (:[]), parseComEmpty, parseClose [] [], err] where
    parseComEmpty = space >>= parseClose []
    parseClose stack c = do
      loc <- tokenBy close asLocation
      return (if null stack && null c then constr stack else if null c then constr stack else voidVal c, loc)
    loop stack = flip mplus (parseClose stack []) $ do
      token COMMA as0
      o <- commented (expect errMsg parser)
      loop (stack++[o])
    err = fail $ "unknown token while parsing list of items for "++uchars errMsg

-- More than one parser has need of 'commaSepd' as a parameter to 'commented', but passing
-- 'commaSped' to 'commented' will return a value of type:
-- > 'Dao.Object.Com' (['Dao.Object.Com'], 'Dao.Parser.Location')
-- which is not useful for constructors of the abstract syntax tree. This function takes the
-- comments around the pair and maps the first item of the pair to the comments, returning an
-- uncommented pair.
commentedInPair :: DaoParser (a, Location) -> DaoParser (Com a, Location)
commentedInPair parser = do
  comntd <- commented parser
  let (a, loc) = unComment comntd
  return (fmap (const a) comntd, loc)

commaSepdObjList :: String -> String -> DaoParser AST_ObjList
commaSepdObjList open close = do
  coms          <- optSpace
  startLoc      <- tokenBy open asLocation
  (lst, endLoc) <- commaSepd "arguments to function call" close (return . com [] AST_Void) equation id
  return (AST_ObjList coms lst (startLoc<>endLoc))
    

funcCallParamList :: DaoParser AST_ObjList
funcCallParamList = commaSepdObjList "(" ")"

initializerList :: DaoParser AST_ObjList
initializerList = commaSepdObjList "{" "}"

-- The syntax for 'Dao.Object.AST.AST_InitExpr', 'Dao.Object.AST.AST_FuncCall', and
-- 'Dao.Object.AST_ObjQualRef' expressions are similar enough that 'AST_InitExpr' that the semantics
-- need to be hand-merged into a single function here. Basically, if there is a reference expression
-- alone, it is an 'Dao.Object.AST_ObjQualRef' expression. If the reference expression is followed
-- immediately by a curly-bracketed list of object expressions and then a semicolon, it is an
-- 'AST_Init' expression. If the reference expression is followed by round brackets with a
-- comma-separated list of object expressions and then a semi-colon, it is a
-- 'Dao.Object.AST_FuncCall' expression. If the reference expression is followed by a
-- comma-separated list of object expressions in round brackets, AND THEN followed by a
-- comma-separated list of object expressions in curly brackets, and then a semi-colon, it is once
-- again a 'Dao.Object.AST_Init' expression.
funcCallOrInitPTabItem :: DaoTableItem AST_Object
funcCallOrInitPTabItem = bindPTableItem referencePTabItem $ \ref -> do
  let objRef = AST_ObjQualRef (AST_Unqualified ref)
  let refLoc = getLocation ref
  let curlyBrackets params = do
        initList <- initializerList
        return (AST_Init ref params initList (refLoc <> getLocation initList))
  msum $
    [ do  paramList <- funcCallParamList
          coms      <- optSpace
          msum $
            [ curlyBrackets (AST_OptObjList paramList coms)
            , return (AST_FuncCall objRef paramList (refLoc <> getLocation paramList))
            ]
    , optSpace >>= curlyBrackets . AST_NoObjList
    , return objRef
    ]

funcCallArraySub :: DaoParser AST_Object
funcCallArraySub = joinEvalPTableItem funcCallOrInitPTabItem

arithPrefixPTab :: DaoPTable AST_Object
arithPrefixPTab = table $ (logicalNOT:) $ flip fmap ["~", "-", "+"] $ \pfxOp ->
  tableItemBy pfxOp $ \tok -> do
    obj <- commented object
    return (AST_Prefix (fromUStr (tokTypeToUStr (asTokType tok))) obj (asLocation tok))
  where
    logicalNOT = tableItemBy "!" $ \op -> do
      obj <- commented arithmetic
      return (AST_Prefix (fromUStr $ tokTypeToUStr $ asTokType op) obj (asLocation op))

arithPrefix :: DaoParser AST_Object
arithPrefix = joinEvalPTable arithPrefixPTab

-- This table extends the 'funcCallArraySubPTab' table with the 'arithPrefixPTab' table. The
-- 'containerPTab' is also included at this level. It is the most complicated (and therefore lowest
-- prescedence) object expression that can be formed without making use of infix operators.
objectPTab :: DaoPTable AST_Object
objectPTab = mconcat [table [funcCallOrInitPTabItem], arithPrefixPTab, singletonOrContainerPTab]

-- Evaluates 'objectPTab' to a 'DaoParser'.
object :: DaoParser AST_Object
object = joinEvalPTable objectPTab

-- A constructor that basically re-arranges the arguments to the 'Dao.Object.AST.AST_Equation'
-- constructor such that this function can be used as an argument to 'Dao.Parser.sinpleInfixed'
-- or 'Dao.Parser.newOpTableParser'.
equationConstructor :: AST_Object -> (Location, Com InfixOp) -> AST_Object -> AST_Object
equationConstructor left (loc, op) right = AST_Equation left op right loc

-- Parses a sequence of 'object' expressions interspersed with arithmetic infix opreators.
-- All infixed logical operators are included, assignment operators are not. The only prefix logical
-- operator. Logical NOT @(!)@ is not parsed here but in the 'arithmetic' function.
arithOpTable :: OpTableParser DaoParState DaoTT (Location, Com InfixOp) AST_Object
arithOpTable =
  newOpTableParser "arithmetic expression" False
    (\tok -> do
        op <- commented (shift (fromUStr . tokTypeToUStr . asTokType))
        return (asLocation tok, op)
    )
    (object >>= \o -> bufferComments >> return o)
    (\left (loc, op) right -> AST_Equation left op right loc)
    ( opRight ["->", "**"] equationConstructor
    : fmap (\ops -> opLeft (words ops) equationConstructor)
        ["* / %", "+ -", "<< >>", "&", "^", "|", "< <= >= >", "== !=", "&&", "||"]
    )

arithmeticPTab :: DaoPTable AST_Object
arithmeticPTab = bindPTable objectPTab $ \obj ->
  evalOpTableParserWithInit (bufferComments >> return obj) arithOpTable

-- Evalautes the 'arithOpTable' to a 'DaoParser'.
arithmetic :: DaoParser AST_Object
arithmetic = joinEvalPTable arithmeticPTab

equationPTab :: DaoPTable AST_Object
equationPTab = bindPTable arithmeticPTab $ \obj ->
  simpleInfixedWithInit "object expression for assignment operator" rightAssoc
    (\left (loc, op) right -> AST_Assign (AST_LValue left) op right loc)
    (bufferComments >> return obj)
    arithmetic
    (liftM2 (,) (look1 asLocation) (commented (joinEvalPTable opTab)))
  where
    opTab :: DaoPTable UpdateOp
    opTab = table $
      fmap (flip tableItemBy (return . fromUStr . tokTypeToUStr . asTokType)) (words allUpdateOpStrs)

-- | Evaluates a sequence arithmetic expressions interspersed with assignment operators.
equation :: DaoParser AST_Object
equation = joinEvalPTable equationPTab

----------------------------------------------------------------------------------------------------

bracketed :: String -> DaoParser (AST_CodeBlock, Location)
bracketed msg = do
  startLoc <- tokenBy "{" asLocation
  scrps    <- mplus (many script) (return [])
  let filtered = scrps >>= \scrp -> case scrp of
        AST_Comment [] -> []
        scrp           -> [scrp]
  expect ("curly-bracket to close "++msg++" statement") $ do
    a <- look1 id
    endLoc <- tokenBy "}" asLocation
    return (AST_CodeBlock filtered, startLoc<>endLoc)

script :: DaoParser AST_Script
script = joinEvalPTable scriptPTab

ifWhilePTabItem :: String -> (AST_If -> a) -> DaoTableItem a
ifWhilePTabItem keyword constr = tableItemBy keyword $ \tok -> do
  obj <- commented paren
  (thn, loc) <- bracketed keyword
  return $ constr $ AST_If obj thn (asLocation tok <> loc)

whilePTabItem :: DaoTableItem AST_While
whilePTabItem = ifWhilePTabItem "while" AST_While

ifPTabItem :: DaoTableItem AST_If
ifPTabItem = ifWhilePTabItem "if" id

ifStatement :: DaoParser AST_If
ifStatement = joinEvalPTableItem ifPTabItem

ifElsePTabItem :: DaoTableItem AST_IfElse
ifElsePTabItem = bindPTableItem ifPTabItem (loop []) where
  loop elsx ifExpr = do
    msum $
      [ do  com1   <- optSpace
            elsLoc <- tokenBy "else" asLocation
            expect "bracketed expression, or another \"if\" expression after \"else\" statement" $ do
              com2 <- optSpace
              msum $
                [ do  nextIf <- ifStatement
                      loop (elsx++[AST_Else (com com1 () com2) nextIf (elsLoc <> getLocation nextIf)]) ifExpr
                , do  (els, endLoc) <- bracketed "else statement"
                      return (AST_IfElse ifExpr elsx (com com1 () com2) (Just els) (getLocation ifExpr <> endLoc))
                ]
      , return (AST_IfElse ifExpr elsx (Com ()) Nothing (getLocation ifExpr))
      ]

--ifStatementTabItem :: DaoTableItem (Com AST_Object, AST_CodeBlock, AST_ElseIf, Location)
--ifStatementTabItem = tableItemBy "if" $ \tok -> expect "conditional expression after \"if\" statement" $ do
--  coms <- optSpace
--  expect "object expression after \"if\" statement" $ do
--    coms <- optSpace
--    obj  <- commented equation
--    expect "bracketed script expressions after \"if\" statement" $ do
--      (thenStmt, thenLoc) <- bracketed "if statement"
--      elseStmt <- elseStatement
--      return (obj, thenStmt, elseStmt, asLocation tok <> thenLoc <> getLocation elseStmt)
--
--ifStatement :: DaoParser (Com AST_Object, AST_CodeBlock, AST_ElseIf, Location)
--ifStatement = joinEvalPTableItem ifStatementTabItem
--
--elseStatement :: DaoParser AST_ElseIf
--elseStatement = do
--  com1 <- optSpace
--  flip mplus (return AST_NullElseIf) $ do
--    startLoc <- tokenBy "else" asLocation
--    com2     <- optSpace
--    let done (els, endLoc) = return (AST_Else com1 els endLoc)
--    -- The comments for the second bracketed script of AST_IfThenElse aren't surrounding the whole
--    -- bracketed expression, only the "else" statement token. This needs to be considered when
--    -- creating syntax trees with quickcheck, and when instantiating the pretty printer.
--    expect "curly-bracketed script or another \"if\" statement after \"else\" statement" $
--      mplus (bracketed "\"else\" statement" >>= done)
--            (fmap (\ (a,b,c,d) -> AST_ElseIf a b c d) ifStatement)

scriptPTab :: DaoPTable AST_Script
scriptPTab = comments <> objExpr <> table exprs where
  objExpr = bindPTable equationPTab $ \obj -> do
    coms <- optSpace
    expect "semicolon after object expression" $ do
      endLoc <- tokenBy ";" asLocation
      return (AST_EvalObject obj coms (getLocation obj <> endLoc))
  comments = bindPTable spaceComPTab $ \c1 -> optSpace >>= \c2 -> return (AST_Comment (c1++c2))
  exprs = fmap (fmap AST_WhileLoop) whilePTabItem : fmap (fmap AST_IfThenElse) ifElsePTabItem :
    [ tableItemBy "return"   (returnExpr True)
    , tableItemBy "throw"    (returnExpr False)
    , tableItemBy "continue" (continExpr True)
    , tableItemBy "break"    (continExpr False)
    , tableItemBy "try"   $ \tok -> expect "bracketed script after \"try\" statement" $ do
        tryCom <- commented (bracketed "\"try\" statement")
        let (try, endLoc) = unComment tryCom
        tryCom <- return (fmap (const try) tryCom)
        let done comName catch endLoc = return $
              AST_TryCatch tryCom comName catch (asLocation tok <> endLoc)
        flip mplus (done Nothing mempty endLoc) $ do
          endLoc  <- tokenBy "catch" asLocation
          comName <- optional $ commented $ token LABEL asName
          scrpt   <- optional $ bracketed "\"catch\" statement"
          case scrpt of
            Nothing              -> done comName Nothing      endLoc
            Just (catch, endLoc) -> done comName (Just catch) endLoc
    , tableItemBy "for"   $ \tok -> expect "iterator label after \"for statement\"" $ do
        comName <- commented (token LABEL asName)
        expect "\"in\" statement after \"for\" statement" $ do
          tokenBy "in" as0
          expect "object expression over which to iterate of \"for-in\" statement" $ do
            obj <- commented paren
            expect "bracketed script after \"for-in\" statement" $ do
              (for, endLoc) <- bracketed "\"for\" statement"
              return (AST_ForLoop comName obj for (asLocation tok <> endLoc))
--    , tableItemBy "while" $ \tok -> expect "conditional expression after \"while\" statement" $ do
--        obj <- commented equation
--        expect "bracketed script after \"while\" statement" $ do
--          (while, endLoc) <- bracketed "\"while\" statement"
--          return (AST_WhileLoop obj while (asLocation tok <> endLoc))
    , tableItemBy "with"  $ \tok -> expect "reference expression after \"with\" statement" $ do
        obj <- commented paren
        expect "bracketed script after \"with\" statement" $ do
          (with, endLoc) <- bracketed "\"with\" statement"
          return (AST_WithDoc obj with (asLocation tok <> endLoc))
    ]
  semicolon = tokenBy ";" asLocation
  returnExpr isReturn tok = do
    obj    <- commented (equation <|> return AST_Void)
    expect ("semicolon after \""++asString tok++"\" statement") $ do
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

----------------------------------------------------------------------------------------------------

toplevelPTab :: DaoPTable AST_TopLevel
toplevelPTab = table expr <> comments <> scriptExpr where
  comments = bindPTable spaceComPTab (\c1 -> optSpace >>= \c2 -> return (AST_TopComment (c1++c2)))
  scriptExpr = bindPTable scriptPTab $ \obj -> return (AST_TopScript obj (getLocation obj))
  expr =
    [ event    "BEGIN"  , event    "END"     , event   "EXIT"
    , header   "require", header   "import"
    ]
  singlePattern = commented equation >>= \eqn -> return ([eqn], getLocation (unComment eqn))
  event   lbl = tableItemBy lbl $ \tok -> do
    let exprType = show (asTokType tok)
    coms <- optSpace
    expect ("bracketed script after \""++exprType++"\" statement") $ do
      (event, endLoc) <- bracketed ('"':exprType++"\" statement")
      return (AST_Event (read lbl) coms event (asLocation tok <> endLoc))
  header lbl = tableItemBy lbl $ \startTok ->
    expect ("string literal or reference for \""++lbl++"\" statement") $ do
      let strlit = do
            tok <- token STRINGLIT id
            case readsPrec 0 (asString tok) of
              [(sym, "")] -> return (AST_Literal (OString (ustr (sym::String))) (asLocation tok))
              _           -> fail ("invalid string expression: "++show (asUStr tok))
      expr <- commented equation
      expect ("semicolon after \""++lbl++"\" statement") $ do
        endLoc <- tokenBy ";" asLocation
        return $ AST_Attribute (ustr lbl) expr $
          (asLocation startTok <> (getLocation (unComment expr)))

toplevel :: DaoParser AST_TopLevel
toplevel = joinEvalPTable toplevelPTab

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
  OK      a -> putStrLn ("Parser succeeded:\n"++prettyShow a)
  Backtrack -> testDaoLexer input >> putStrLn "---- PARSER BACKTRACKED ----\n"
  PFail err -> do
    testDaoLexer input
    putStrLn ("---- PARSER FAILED ----\n" ++ show err)
    let st = parseStateAtErr err >>= internalState
    putStrLn ("recentTokens = "++show (tokenQueue <$> st))
    putStrLn ("getLines     = "++show (getLines   <$> st))

