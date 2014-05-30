-- "src/Dao/Interpreter/Parser.hs" makes use of "Dao.Parser" to parse
-- parse 'Dao.Interpreter.AST' expressions.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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

module Dao.Interpreter.Parser where

import           Dao.String
import           Dao.Token
import           Dao.PPrint
import           Dao.Interpreter     hiding (opt)
import           Dao.Interpreter.AST
import           Dao.Interpreter.Tokenizer
import           Dao.Predicate
import           Dao.Parser

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error hiding (Error)
import           Control.Monad.State

import           Data.Monoid
import           Data.Maybe
import           Data.List
import           Data.Char
import           Data.Ratio
import           Data.Complex

maxYears :: Integer
maxYears = 99999

type DaoParser    a = Parser    DaoParState DaoTT a
type DaoTableItem a = TableItem DaoTT (DaoParser a)
type DaoPTable    a = PTable    DaoTT (DaoParser a)
type DaoParseErr    = Error     DaoParState DaoTT

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
-- of the parser wrapped in a 'Dao.Interpreter.Com' constructor. If the given 'DaoParser' backtracks, the
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
        exp <- fmap (round . abs) (rationalFromString base b exp) :: Maybe Integer
        let ibase  = if hasMinusSign then recip b else b
            result = (x+y)*(ibase^^exp)
        return (round result % 1 == result, result)
  (_r_is_an_integer, r) <- case rational of
    Nothing -> fail ("incorrect digits used to form a base-"++show base++" number")
    Just  r -> return r
  case typ of
    "U" -> return $ OWord (round r)
    "I" -> return $ OInt  (round r)
    "L" -> return $ OLong (round r)
    "R" -> return $ ORatio r
    "F" -> return $ OFloat (fromRational r)
    "f" -> return $ OFloat (fromRational r)
    "i" -> return $ OComplex $ Complex $ 0 :+ fromRational r
    "j" -> return $ OComplex $ Complex $ 0 :+ fromRational r
    "s" -> return $ ORelTime (fromRational r)
    ""  ->
      return (OInt (round r))
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
      (_, _      ) -> error "unexpected character while parsing time-literal expression"
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

numberPTabItems :: [DaoTableItem (AST_Literal Object)]
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
  , tableItemBy "date" $ \startTok ->
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
          [(o, "")] -> return (AST_Literal (OAbsTime o) loc)
          _         -> fail "invalid UTC-time expression"
  , tableItemBy "time" $ \startTok -> expect "UTC-time value after \"time\" statement" $ do
      token SPACE as0
      tok  <- token TIME id
      time <- diffTimeFromStrs (asString tok)
      return (AST_Literal (ORelTime time) (asLocation startTok <> asLocation tok))
  ]
  where
    mloc = fmap asLocation
    mstr = fmap asString
    base b t = tableItem t $ \tok -> do
      typ <- optional (token NUMTYPE id)
      done tok b (drop 2 (asString tok)) Nothing Nothing (mstr typ) (return (asLocation tok) <> mloc typ)
    done tok base int frac exp typ loc = do
      num <- numberFromStrs base int frac exp typ
      let endLoc = asLocation tok
      return (AST_Literal num (maybe endLoc id loc))

numberPTab :: DaoPTable (AST_Literal Object)
numberPTab = table numberPTabItems

-- | Parsing numerical literals
number :: DaoParser (AST_Literal Object)
number = joinEvalPTable numberPTab

singletonPTab :: DaoPTable (AST_Literal Object)
singletonPTab = table singletonPTabItems

parenPTabItem :: DaoTableItem (AST_Paren Object)
parenPTabItem = tableItemBy "(" $ \tok -> do
  o <- commented assignment
  expect "closing parentheses" $ do
    endloc <- tokenBy ")" asLocation
    return (AST_Paren o (asLocation tok <> endloc))

paren :: DaoParser (AST_Paren Object)
paren = joinEvalPTableItem parenPTabItem

metaEvalPTabItem :: DaoTableItem (AST_Object Object)
metaEvalPTabItem = tableItemBy "${" $ \startTok -> expect "object expression after open ${ meta-eval brace" $ do
  scrp <- fmap (AST_CodeBlock . concat) (many script)
  expect "closing } for meta-eval brace" $ do
    endLoc <- tokenBy "}" asLocation
    return (AST_MetaEval scrp (asLocation startTok <> endLoc))

singletonPTabItems :: [DaoTableItem (AST_Literal Object)]
singletonPTabItems = numberPTabItems ++
  [ tableItem STRINGLIT (literal $ OString . read     . asString)
  , tableItem CHARLIT   (literal $ OChar   . read     . asString)
  , trueFalse "null" ONull, trueFalse "false" ONull, trueFalse "true" OTrue
  , reserved "operator", reserved "public", reserved "private"
  ]
  where
    literal constr tok = return (AST_Literal (constr tok) (asLocation tok))
    trueFalse lbl obj = tableItemBy lbl $ \tok -> return (AST_Literal obj (asLocation tok))
    reserved key = tableItemBy key $ fail $
      "keyword "++show key++" is reserved for future use, not implemented in this version of Dao"

-- Objects that are parsed as a single value, which includes all literal expressions and equtions in
-- parentheses.
singleton :: DaoParser (AST_Literal Object)
singleton = joinEvalPTable singletonPTab

-- Returns an AST_ObjList, which is a constructor that contains leading whitespace/comments. However
-- this function is a 'DaoTableItem' parser, which means the first token parsed must be the opening
-- bracket. In order to correctly parse the leading whitespace/comments while also correctly
-- identifying the opening bracket token, it is expected that you have called 'bufferComments'
-- immediately before this function is evaluated.
commaSepdObjList :: String -> String -> String -> DaoTableItem (AST_ObjList Object)
commaSepdObjList msg open close = tableItemBy open $ \startTok -> do
  let startLoc = asLocation startTok
  coms <- optSpace -- the comments must have been buffered by this point, otherwise the parser behaves strangely.
  (lst, endLoc) <- commaSepd ("arguments to "++msg) close (return . com [] nullValue) assignment id
  return (AST_ObjList coms lst (startLoc<>endLoc))

ruleFuncPTab :: DaoPTable (AST_RuleFunc Object)
ruleFuncPTab = table $
  [ tableItemBy "rule" $ \startTok ->
      expect "list of strings after \"rule\" statement" $ do
        lst <- commented $ joinEvalPTable $ table $
          [ tableItem STRINGLIT $ \str -> return $ AST_RuleString (Com $ read $ uchars $ asUStr str) (asLocation str)
          , fmap (\ (AST_ObjList coms lst loc) ->
                  if null lst then AST_NullRules coms loc else AST_RuleHeader lst loc
              ) <$> commaSepdObjList "rule header" "(" ")"
          ]
        expect "bracketed expression after rule header" $ do
          (scrpt, endLoc) <- bracketed ("script expression for \"rule\" statement")
          return $ AST_Rule lst scrpt (asLocation startTok <> endLoc)
  , lambdaFunc "func", lambdaFunc "function"
  ]
  where
    lambdaFunc lbl = tableItemBy lbl $ \startTok ->
      expect ("parameters and bracketed script after \""++lbl++"\" statement") $ do
        constr <- mplus (pure AST_Func <*> optSpace <*> token LABEL asName) (return AST_Lambda)
        params <- commented paramList
        (scrpt, endLoc) <- bracketed ("script expression after \""++lbl++"\" statement")
        return $ constr params scrpt (asLocation startTok <> endLoc)

ruleFunc :: DaoParser (AST_RuleFunc Object)
ruleFunc = joinEvalPTable ruleFuncPTab

-- Objects that are parsed as a single value but which are constructed from other object
-- expressions. This table excludes 'singletonPTab'.
containerPTab :: DaoPTable (AST_Object Object)
containerPTab = table [metaEvalPTabItem]

-- None of the functions related to parameters and type checking parse with tables because there is
-- simply no need for it according to the Dao language syntax.
typeCheckParser :: a -> DaoParser (AST_TyChk a Object)
typeCheckParser a = flip mplus (return (AST_NotChecked a)) $ do
  com1 <- commented (tokenBy "::" id)
  let startLoc = asLocation (unComment com1)
  expect "type expression after colon operator" $ arithmetic >>= \obj -> return $
    AST_Checked a (fmap as0 com1) obj (startLoc <> getLocation obj)

typeCheckedName :: DaoParser (AST_TyChk Name Object)
typeCheckedName = token LABEL id >>= \tok ->
  fmap (\tychk -> setLocation tychk (asLocation tok <> getLocation tychk)) $
    typeCheckParser (asName tok)

parameter :: DaoParser (AST_Param Object)
parameter = msum $
  [ do  startLoc <- tokenBy "$" asLocation
        coms     <- optional space
        item     <- typeCheckedName
        return $ AST_Param coms item (startLoc <> getLocation item)
  , typeCheckedName >>= \nm -> return $ AST_Param Nothing nm (getLocation nm)
  ]

paramList :: DaoParser (AST_ParamList Object)
paramList = do
  startLoc   <- tokenBy "(" asLocation
  (lst, loc) <- commaSepd "parameter value" ")" (return . com [] AST_NoParams) parameter id
  lst        <- typeCheckParser lst
  return (AST_ParamList lst (startLoc <> loc))

singletonOrContainerPTab :: DaoPTable (AST_Object Object)
singletonOrContainerPTab = fmap (fmap AST_ObjLiteral) singletonPTab <> containerPTab

singletonOrContainer :: DaoParser (AST_Object Object)
singletonOrContainer = joinEvalPTable singletonOrContainerPTab

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
-- > 'Dao.Interpreter.Com' (['Dao.Interpreter.Com'], 'Dao.Parser.Location')
-- which is not useful for constructors of the abstract syntax tree. This function takes the
-- comments around the pair and maps the first item of the pair to the comments, returning an
-- uncommented pair.
commentedInPair :: DaoParser (a, Location) -> DaoParser (Com a, Location)
commentedInPair parser = do
  comntd <- commented parser
  let (a, loc) = unComment comntd
  return (fmap (const a) comntd, loc)

-- You MUST have evaluated 'bufferComments' before evaluating any of the parsers in this table.
-- 'refSuffix' does this.
refSuffixPTabItems :: [DaoTableItem (AST_RefSuffix Object)]
refSuffixPTabItems =
  [ tableItemBy "." $ \tok -> do
      comBefore <- optSpace -- get comments before dot that were buffered by 'bufferComments'
      comAfter  <- optSpace -- get comments after the dot
      expect "valid identifier after dot token" $ do
        name      <- token LABEL id
        suf       <- refSuffix
        let loc = asLocation tok <> getLocation suf
        return $ AST_DotRef (com comBefore () comAfter) (asName name) suf loc
  , p "subscript expression"     "[" "]" AST_Subscript
  , p "function call expression" "(" ")" AST_FuncCall
  ]
  where
    p msg open close constr = bindPTableItem (commaSepdObjList msg open close) $ \olst ->
      refSuffix >>= \suf -> return $ constr olst suf

refSuffixPTab :: DaoPTable (AST_RefSuffix Object)
refSuffixPTab = table refSuffixPTabItems

refSuffix :: DaoParser (AST_RefSuffix Object)
refSuffix = bufferComments >> joinEvalPTable refSuffixPTab <|> return AST_RefNull

referencePTabItems :: [DaoTableItem (AST_Reference Object)]
referencePTabItems =
  [ p "local" LOCAL, p "const" CONST, p "static" STATIC, p "global" GLOBAL, p "." GLODOT
  , tableItem LABEL $ \name -> do
      suf <- refSuffix
      return $ AST_Reference UNQUAL [] (asName name) suf (asLocation name)
  , bindPTableItem parenPTabItem $ \o -> refSuffix >>= \suf -> return $
      AST_RefObject o suf (getLocation o <> getLocation suf)
  ]
  where
    p opstr op = tableItemBy opstr $ \tok ->
      expect ("reference expression after "++show opstr++" qualifier") $ do
        coms <- optSpace
        name <- token LABEL id
        suf  <- refSuffix
        return $ AST_Reference op coms (asName name) suf (asLocation tok <> asLocation name)

referencePTab :: DaoPTable (AST_Reference Object)
referencePTab = table referencePTabItems

referenceParser :: DaoParser (AST_Reference Object)
referenceParser = joinEvalPTable referencePTab

refPrefixPTabItems :: [DaoTableItem (AST_RefPrefix Object)]
refPrefixPTabItems = [p "$" REF, p "@" DEREF] where
  p opstr op = tableItemBy opstr $ \tok ->
    expect ("reference expression after"++show opstr++" token") $ do
      coms <- optSpace
      ref  <- refPrefixParser
      return $ AST_RefPrefix op coms ref (asLocation tok <> getLocation ref)

refPrefixPTab :: DaoPTable (AST_RefPrefix Object)
refPrefixPTab = table refPrefixPTabItems <> (fmap (\o -> AST_PlainRef o) <$> referencePTab)

refPrefixParser :: DaoParser (AST_RefPrefix Object)
refPrefixParser = joinEvalPTable refPrefixPTab

initPTab :: DaoPTable (AST_Object Object)
initPTab = bindPTable refPrefixPTab $ \o -> let single = return (AST_ObjSingle o) in case o of
  AST_PlainRef ref -> case refToDotLabelAST ref of
    Just (ref, inits) -> (bufferComments>>) $ flip mplus single $ do
      olst <- joinEvalPTableItem $ commaSepdObjList "initializing expression" "{" "}"
      coms <- optSpace
      return $ AST_Init ref (AST_OptObjList coms inits) olst (getLocation o <> getLocation olst)
    Nothing           -> single
  AST_RefPrefix{}  -> single

structPTabItems :: [DaoTableItem (AST_Object Object)]
structPTabItems = (:[]) $ tableItem HASHLABEL $ \nameTok -> do
  bufferComments
  let name = fromUStr $ ustr $ tail $ asString nameTok
  let startLoc = asLocation nameTok
  flip mplus (return $ AST_Struct name nullValue startLoc) $ do
    objList <- joinEvalPTableItem $ commaSepdObjList "data structure initializer" "{" "}"
    let objListLoc = getLocation objList
    return $ AST_Struct name (AST_OptObjList [] (Just objList)) (startLoc<>objListLoc)

structPTab :: DaoPTable (AST_Object Object)
structPTab = table structPTabItems

arithPrefixPTab :: DaoPTable (AST_Object Object)
arithPrefixPTab = table $ (logicalNOT:) $ flip fmap ["~", "-", "+"] $ \pfxOp ->
  tableItemBy pfxOp $ \tok -> optSpace >>= \coms -> object >>= \o ->
    return (AST_ArithPfx (fromUStr (tokTypeToUStr (asTokType tok))) coms o (asLocation tok))
  where
    logicalNOT = tableItemBy "!" $ \op -> do
      coms <- optSpace
      o <- object
      return $ AST_ArithPfx (fromUStr $ tokTypeToUStr $ asTokType op) coms o (asLocation op)

-- This table extends the 'funcCallArraySubPTab' table with the 'arithPrefixPTab' table. The
-- 'containerPTab' is also included at this level. It is the most complicated (and therefore lowest
-- prescedence) object expression that can be formed without making use of infix operators.
objectPTab :: DaoPTable (AST_Object Object)
objectPTab = mconcat [singletonOrContainerPTab, arithPrefixPTab, structPTab, initPTab]

-- Evaluates 'objectPTab' to a 'DaoParser'.
object :: DaoParser (AST_Object Object)
object = joinEvalPTable objectPTab

-- A constructor that basically re-arranges the arguments to the 'Dao.Interpreter.AST.AST_Eval'
-- constructor such that this function can be used as an argument to 'Dao.Parser.sinpleInfixed'
-- or 'Dao.Parser.newOpTableParser'.
arithConstr :: AST_Arith Object -> (Location, Com InfixOp) -> AST_Arith Object -> DaoParser (AST_Arith Object)
arithConstr left (loc, op) right = return $ AST_Arith left op right loc

-- Parses a sequence of 'object' expressions interspersed with arithmetic infix opreators.
-- All infixed logical operators are included, assignment operators are not. The only prefix logical
-- operator. Logical NOT @(!)@ is not parsed here but in the 'arithmetic' function.
arithOpTable :: OpTableParser DaoParState DaoTT (Location, Com InfixOp) (AST_Arith Object)
arithOpTable =
  newOpTableParser "arithmetic expression" False
    (\tok -> do
        op <- commented (shift (fromUStr . tokTypeToUStr . asTokType))
        return (asLocation tok, op)
    )
    (object >>= \o -> bufferComments >> return (AST_Object o))
    arithConstr
    ( opRight ["->"] arithConstr
    : opRight ["**"] arithConstr
    : fmap (\ops -> opLeft (words ops) arithConstr)
        ["* / %", "+ -", "<< >>", "&", "^", "|", "< <= >= >", "== !="]
    ++ [opRight ["&&"] arithConstr, opRight [ "||"] arithConstr]
    )

arithmeticPTab :: DaoPTable (AST_Arith Object)
arithmeticPTab = bindPTable objectPTab $ \o ->
  evalOpTableParserWithInit (bufferComments >> return (AST_Object o)) arithOpTable

-- Evalautes the 'arithOpTable' to a 'DaoParser'.
arithmetic :: DaoParser (AST_Arith Object)
arithmetic = joinEvalPTable arithmeticPTab

objTestPTab :: DaoPTable (AST_ObjTest Object)
objTestPTab = mappend (fmap AST_ObjRuleFunc <$> ruleFuncPTab) $ bindPTable arithmeticPTab $ \a -> do
  bufferComments
  flip mplus (return $ AST_ObjArith a) $ do
    qmark <- commented (tokenBy "?" as0)
    expect "arithmetic expression after (?) operator" $ do
      b <- arithmetic
      expect "(:) operator and arithmetic expression after (?) operator" $ do
        coln <- commented (tokenBy ":" as0)
        expect "arithmetic expression after (:) operator" $ do
          c <- arithmetic
          return $ AST_ObjTest a qmark b coln c (getLocation a <> getLocation c)

objTest :: DaoParser (AST_ObjTest Object)
objTest = joinEvalPTable objTestPTab

assignmentWithInit :: DaoParser (AST_Assign Object) -> DaoParser (AST_Assign Object)
assignmentWithInit init = 
  simpleInfixedWithInit "object expression for assignment operator" rightAssoc
    (\left (loc, op) right -> return $ case left of
        AST_Eval left -> AST_Assign left op right loc
        left          -> left
    )
    (bufferComments >> init)
    (fmap AST_Eval objTest)
    (pure (,) <*> look1 asLocation <*> commented (joinEvalPTable opTab))
  where
    opTab :: DaoPTable UpdateOp
    opTab = table $
      fmap (flip tableItemBy (return . fromUStr . tokTypeToUStr . asTokType)) (words allUpdateOpStrs)

assignmentPTab :: DaoPTable (AST_Assign Object)
assignmentPTab = bindPTable objTestPTab (assignmentWithInit . return . AST_Eval)

-- | Evaluates a sequence arithmetic expressions interspersed with assignment operators.
assignment :: DaoParser (AST_Assign Object)
assignment = joinEvalPTable assignmentPTab

----------------------------------------------------------------------------------------------------

bracketed :: String -> DaoParser (AST_CodeBlock Object, Location)
bracketed msg = do
  startLoc <- tokenBy "{" asLocation
  scrps    <- concat <$> (many script <|> return [])
  expect ("curly-bracket to close "++msg++" statement") $ do
    _ <- look1 id
    endLoc <- tokenBy "}" asLocation
    return (AST_CodeBlock scrps, startLoc<>endLoc)

script :: DaoParser [AST_Script Object]
script = joinEvalPTable scriptPTab

ifWhilePTabItem :: String -> (AST_If Object -> a) -> DaoTableItem a
ifWhilePTabItem keyword constr = tableItemBy keyword $ \tok -> do
  o <- commented paren
  (thn, loc) <- bracketed keyword
  return $ constr $ AST_If o thn (asLocation tok <> loc)

whilePTabItem :: DaoTableItem (AST_While Object)
whilePTabItem = ifWhilePTabItem "while" AST_While

ifPTabItem :: DaoTableItem (AST_If Object)
ifPTabItem = ifWhilePTabItem "if" id

ifStatement :: DaoParser (AST_If Object)
ifStatement = joinEvalPTableItem ifPTabItem

lastElseParser :: DaoParser (AST_LastElse Object)
lastElseParser = do
  comTok <- commented $ tokenBy "else" asLocation
  (els, endLoc) <- bracketed "else statement"
  return $ AST_LastElse (fmap (const ()) comTok) els (unComment comTok <> endLoc)

catchExprParser :: DaoParser (AST_Catch Object)
catchExprParser = do
  bufferComments
  startLoc <- tokenBy "catch" asLocation
  expect "parameter variable name/type after \"catch\" statement" $ do
    coms  <- optSpace
    param <- commented parameter
    (scrpt, endLoc) <- bracketed "\"catch\" statement"
    return $ AST_Catch coms param scrpt (startLoc <> endLoc)

ifElsePTabItem :: DaoTableItem (AST_IfElse Object)
ifElsePTabItem = bindPTableItem ifPTabItem (loop []) where
  loop elsx ifExpr = msum $
    [ do  com1   <- optSpace
          elsLoc <- tokenBy "else" asLocation
          expect "bracketed expression, or another \"if\" expression after \"else\" statement" $ do
            com2 <- optSpace
            msum $
              [ do  nextIf <- ifStatement
                    loop (elsx++[AST_Else (com com1 () com2) nextIf (elsLoc <> getLocation nextIf)]
                          ) ifExpr
              , do  (els, endLoc) <- bracketed "else statement"
                    return $
                      AST_IfElse ifExpr elsx
                        (Just $ AST_LastElse (com com1 () com2) els (getLocation els <> endLoc))
                        (getLocation ifExpr <> endLoc)
              ]
    , return (AST_IfElse ifExpr elsx Nothing (getLocation ifExpr))
    ]

scriptPTab :: DaoPTable [AST_Script Object]
scriptPTab = comments <> objExpr <> table exprs where
  -- Object expressions should end with a semi-colon. An exception to this rule is made for rule and
  -- function constant expressions.
  objExpr = bindPTable assignmentPTab $ \o -> case o of
    AST_Eval (AST_ObjRuleFunc o) ->
      flip mplus (return [AST_RuleFunc o]) $ do
        coms <- optSpace
        loc  <- mappend (getLocation o) <$> tokenBy ";" asLocation
        return [AST_EvalObject (AST_Eval $ AST_ObjRuleFunc o) coms loc]
    o -> do
      coms <- optSpace
      expect "semicolon after object expression" $ do
        endLoc <- tokenBy ";" asLocation
        return [AST_EvalObject o coms (getLocation o <> endLoc)]
  comments = bindPTable spaceComPTab $ \c1 -> optSpace >>= \c2 ->
    let coms = c1++c2 in if null coms then return [] else return [AST_Comment coms]
  exprs =
    [ fmap (fmap (return . AST_WhileLoop)) whilePTabItem 
    , fmap (fmap (return . AST_IfThenElse)) ifElsePTabItem
    , returnExpr "return"   True
    , returnExpr "throw"    False
    , continExpr "continue" True
    , continExpr "break"    False
    , tableItemBy "try" $ \tok -> expect "bracketed script after \"try\" statement" $ do
        coms <- optSpace
        (try, endLoc) <- bracketed "\"try\" statement"
        elsExprs   <- many lastElseParser
        catchExprs <- many catchExprParser
        let finalLoc :: forall a . HasLocation a => [a] -> Location
            finalLoc = foldl (\_ a -> getLocation a) endLoc
        let loc = asLocation tok <> finalLoc elsExprs <> finalLoc catchExprs
        return [AST_TryCatch coms try elsExprs catchExprs loc]
    , tableItemBy "for"   $ \tok -> expect "iterator label after \"for statement\"" $ do
        comName <- commented (token LABEL asName)
        expect "\"in\" statement after \"for\" statement" $ do
          tokenBy "in" as0
          expect "object expression over which to iterate of \"for-in\" statement" $ do
            o <- commented refPrefixParser
            expect "bracketed script after \"for-in\" statement" $ do
              (for, endLoc) <- bracketed "\"for\" statement"
              return [AST_ForLoop comName o for (asLocation tok <> endLoc)]
    , tableItemBy "with"  $ \tok -> expect "reference expression after \"with\" statement" $ do
        o <- commented paren
        expect "bracketed script after \"with\" statement" $ do
          (with, endLoc) <- bracketed "\"with\" statement"
          return [AST_WithDoc o with (asLocation tok <> endLoc)]
    ]
  semicolon = tokenBy ";" asLocation
  returnExpr key isReturn = tableItemBy key $ \tok -> do
    o <- commented (assignment <|> return nullValue)
    expect ("semicolon after \""++key++"\" statement") $ do
      endLoc <- semicolon
      return [AST_ReturnExpr isReturn o (asLocation tok <> endLoc)]
  continExpr key isContin = tableItemBy key $ \tok -> do
    let startLoc = asLocation tok
    let msg e = concat [e, " after \"", key, "-if\" statement"]
    coms <- optSpace
    msum $
      [do endLoc <- semicolon
          return [AST_ContinueExpr isContin coms (Com nullValue) (startLoc<>endLoc)]
      ,do tokenBy "if" as0
          expect (msg "conditional expression") $ do
            o <- commented assignment
            expect (msg "semicolon") $ do
              endLoc <- semicolon
              return [AST_ContinueExpr isContin coms o (startLoc<>endLoc)]
      , fail (msg "expecting optional object expression followed by a semicolon")
      ]

----------------------------------------------------------------------------------------------------

dotName :: DaoParser AST_DotName
dotName = return AST_DotName <*> commented (tokenBy "." as0) <*> token LABEL asName

dotLabelTableItem :: DaoTableItem AST_DotLabel
dotLabelTableItem = tableItem LABEL $ \tok ->
  return (AST_DotLabel (asName tok)) <*> many dotName <*> pure (asLocation tok)

attributePTab :: DaoPTable AST_Attribute
attributePTab = table $
  [ fmap AST_AttribDotName <$> dotLabelTableItem
  , tableItem STRINGLIT $ \tok -> return $ AST_AttribString (asUStr tok) (asLocation tok)
  ]

attribute :: DaoParser AST_Attribute
attribute = joinEvalPTable attributePTab

namespace :: DaoParser AST_Namespace
namespace = do
  tokenBy "as" as0
  nm <- commented $ token LABEL id
  return $ AST_Namespace (fmap asName nm) (unComment $ fmap asLocation nm)

----------------------------------------------------------------------------------------------------

toplevelPTab :: DaoPTable [AST_TopLevel Object]
toplevelPTab = table expr <> comments <> scriptExpr where
  comments = bindPTable spaceComPTab $ \c1 -> optSpace >>= \c2 -> return $
    let coms = c1++c2 in if null coms then [] else [AST_TopComment (c1++c2)]
  scriptExpr = bindPTable scriptPTab $ return . map (\o -> AST_TopScript o (getLocation o))
  expr =
    [ event  "BEGIN"  , event  "END"   , event  "EXIT"
    , tableItemBy "require" $ \startTok -> needAttrib "require" $ do
        attrib <- commented attribute
        endTok <- needSemicolon "require"
        return [AST_Require attrib $ asLocation startTok <> endTok]
    , tableItemBy "import" $ \startTok -> needAttrib "import" $ do
        attrib <- commented attribute
        mplus
          (do namesp <- namespace
              endTok <- needSemicolon "import"
              return [AST_Import attrib namesp $ asLocation startTok <> endTok]
          )
          (return . AST_Import attrib nullValue .
            mappend (asLocation startTok) <$> needSemicolon "import")
    ]
  needSemicolon msg =
    expect ("expecting semicolon after \""++msg++"\" statement") $ tokenBy ";" asLocation
  needAttrib msg =
    expect ("expect string literal or logical module name after \""++msg++"\" statement")
  event   lbl = tableItemBy lbl $ \tok -> do
    let exprType = show (asTokType tok)
    coms <- optSpace
    expect ("bracketed script after \""++exprType++"\" statement") $ do
      (event, endLoc) <- bracketed ('"':exprType++"\" statement")
      return [AST_Event (read lbl) coms event (asLocation tok <> endLoc)]

toplevel :: DaoParser [AST_TopLevel Object]
toplevel = joinEvalPTable toplevelPTab

----------------------------------------------------------------------------------------------------

daoParser :: DaoParser (AST_SourceCode Object)
daoParser = do
  let loop dx = msum
        [ isEOF >>= guard >> return dx
        , toplevel >>= \d -> loop (dx++d)
        , fail "syntax error on token"
        ]
  src <- loop []
  return (AST_SourceCode{sourceModified=0, sourceFullPath=nil, directives=src})

daoGrammar :: Language DaoParState DaoTT (AST_SourceCode Object)
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

