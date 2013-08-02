-- "src/Dao/NewParser.hs"  a parser for defining general context-free
-- grammars that are parsed in two phases: the lexical and the
-- syntactic analysis phases.
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Dao.NewParser where

import           Dao.String
import           Dao.Predicate
import qualified Dao.EnumSet  as Es

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.State
import           Control.Monad.Error hiding (Error)

import           Data.Data
import           Data.Tuple
import           Data.Monoid
import           Data.Typeable
import           Data.Maybe
import           Data.Word
import           Data.Char  hiding (Space)
import           Data.List
import           Data.Array.IArray
import qualified Data.Map    as M
import qualified Data.IntMap as IM

import           System.IO

import Debug.Trace

----------------------------------------------------------------------------------------------------

type LineNum   = Word
type ColumnNum = Word
type TabWidth  = Word

----------------------------------------------------------------------------------------------------

-- | If an object contains a location, it can instantiate this class to allow locations to be
-- updated or deleted (deleted by converting it to 'LocationUnknown'. Only three types in this
-- module instantiate this class, but any data type that makes up an Abstract Syntax Tree, for
-- example 'Dao.Object.ObjectExpr' or 'Dao.Object.AST.ObjectExrpr' also instantiate this class.
class HasLocation a where
  getLocation :: a -> Location
  setLocation :: a -> Location -> a

-- | Contains two points, a starting and and ending point, where each point consists of a row (line
-- number) and column (character count from the beginning of a line) for locating entities in a
-- parsable text. This type does not contain information regarding the source of the text, or
-- whether or not the input text is a file or stream.
data Location
  = LocationUnknown
  | Location
    { startingLine   :: LineNum
      -- ^ the 'Location' but without the starting/ending character count
    , startingColumn :: ColumnNum
    , endingLine     :: LineNum
    , endingColumn   :: ColumnNum
    }
  deriving (Eq, Typeable, Data)
instance HasLocation Location where { getLocation = id; setLocation = flip const; }
instance Show Location where
  show t = case t of
    LocationUnknown -> ""
    _ -> show (startingLine t) ++ ':' : show (startingColumn t)
instance Monoid Location where
  mempty =
    Location
    { startingLine   = 0
    , startingColumn = 0
    , endingLine     = 0
    , endingColumn   = 0
    }
  mappend loc a = case loc of
    LocationUnknown -> a
    _ -> case a of
      LocationUnknown -> loc
      _ ->
        loc
        { startingLine   = min (startingLine   loc) (startingLine   a)
        , startingColumn = min (startingColumn loc) (startingColumn a)
        , endingLine     = max (endingLine     loc) (endingLine     a)
        , endingColumn   = max (endingColumn   loc) (endingColumn   a)
        }
instance Ord Location where
  compare a b = case (a,b) of
    (LocationUnknown, LocationUnknown) -> EQ
    (_              , LocationUnknown) -> LT
    (LocationUnknown, _              ) -> GT
    (a              , b              ) ->
      compare (abs(ela-sla), abs(eca-sca), sla, sca) (abs(elb-slb), abs(ecb-scb), slb, scb)
    where
      sla = startingLine   a
      ela = endingLine     a
      slb = startingLine   b
      elb = endingLine     b
      sca = startingColumn a
      eca = endingColumn   a
      scb = startingColumn b
      ecb = endingColumn   b
  -- ^ Greater-than is determined by a heuristic value of how large and uncertain the position of
  -- the error is. If the exact location is known, it has the lowest uncertainty and is therefore
  -- less than a location that might occur across two lines. The 'LocationUnknown' value is the most
  -- uncertain and is greater than everything except itself. Using this comparison function, you can
  -- sort lists of locations from least to greatest and hopefully get the most helpful, most
  -- specific location at the top of the list.

-- | Create a location where the starting and ending point is the same row and column.
atPoint :: LineNum -> ColumnNum -> Location
atPoint a b =
  Location
  { startingLine   = a
  , endingLine     = a
  , startingColumn = b
  , endingColumn   = b
  }

-- | The the coordinates from a 'Location':
-- @(('startingLine', 'startingColumn'), ('endingLine', 'endingColumn'))@
locationCoords :: Location -> Maybe ((LineNum, ColumnNum), (LineNum, ColumnNum))
locationCoords loc = case loc of
  LocationUnknown -> Nothing
  _ -> Just ((startingLine loc, startingColumn loc), (endingLine loc, endingColumn loc))

----------------------------------------------------------------------------------------------------
-- $All_about_tokens
-- This module was designed to create parsers which operate in two phases: a lexical analysis phase
-- (see 'lexicalAnalysis') where input text is split up into tokens, and a syntactic analysis phase
-- where a stream of tokens is converted into data. 'Token' is the data type that makes this
-- possible.

class HasLineNumber   a where { lineNumber   :: a -> LineNum }
class HasColumnNumber a where { columnNumber :: a -> ColumnNum }
class HasToken        a where { getToken     :: a tok -> Token tok }

-- | Every token emitted by a lexical analyzer must have at least a type. 'Token' is polymorphic
-- over the type of token.
data Token tok
  = EmptyToken { tokType :: tok }
    -- ^ Often times, tokens may not need to contain any text. This is often true of opreator
    -- symbols and keywords. This constructor constructs a token with just a type and no text. The
    -- more descriptive your token types are, the less you need you will have for storing the text
    -- along with the token type, and the more memory you will save.
  | CharToken  { tokType :: tok, tokChar :: !Char }
    -- ^ Constructs tokens along with the text. If the text is only a single character, this
    -- constructor is used, which can save a little memory as compared to storing a
    -- 'Dao.String.UStr'.
  | Token      { tokType :: tok, tokUStr :: UStr }
    -- ^ Constructs tokens that contain a copy of the text extracted by the lexical analyzer to
    -- create the token.
instance Show tok => Show (Token tok) where
  show tok =
    let cont = tokToStr tok
    in  show (tokType tok) ++ (if null cont then "" else ' ':show cont)
instance Functor Token where
  fmap f t = case t of
    EmptyToken t   -> EmptyToken (f t)
    CharToken  t c -> CharToken  (f t) c
    Token      t u -> Token      (f t) u

-- | If the lexical analyzer emitted a token with a copy of the text used to create it, this
-- function can retrieve that text. Returns 'Dao.String.nil' if there is no text.
tokToUStr :: Token tok -> UStr
tokToUStr tok = case tok of
  EmptyToken _   -> nil
  CharToken  _ c -> ustr [c]
  Token      _ u -> u

-- | Like 'tokToUStr' but returns a 'Prelude.String' or @""@ instead.
tokToStr :: Token tok -> String
tokToStr tok = case tok of
  EmptyToken _   -> ""
  CharToken  _ c -> [c]
  Token      _ u -> uchars u

-- | This data type stores the starting point (the line number and column number) in the
-- source file of where the token was emitted along with the 'Token' itself.
data TokenAt tok
  = TokenAt
    { tokenAtLineNumber   :: LineNum
    , tokenAtColumnNumber :: ColumnNum
    , getTokenValue       :: Token tok
    }
instance Show tok =>
  Show (TokenAt tok) where
    show tok = let (a,b,c) = asTriple tok in show a++':':show b++' ':show c
instance HasLineNumber   (TokenAt tok) where { lineNumber   = tokenAtLineNumber }
instance HasColumnNumber (TokenAt tok) where { columnNumber = tokenAtColumnNumber }
instance HasToken TokenAt where { getToken = getTokenValue }
instance Functor  TokenAt where
  fmap f t =
    TokenAt
    { tokenAtLineNumber   = tokenAtLineNumber t
    , tokenAtColumnNumber = tokenAtColumnNumber t
    , getTokenValue       = fmap f (getToken t)
    }

-- | See 'token' and 'tokenBy'.
asTokType :: TokenAt tok -> tok
asTokType = tokType . getToken

-- | See 'token' and 'tokenBy'.
asString :: TokenAt tok -> String
asString = tokToStr . getToken

-- | See 'token' and 'tokenBy'.
asUStr :: TokenAt tok -> UStr
asUStr = tokToUStr . getToken

-- | That is as-zero, because "0" looks kind of like "()".
-- See 'token' and 'tokenBy'.
as0 :: TokenAt tok -> ()
as0 = const ()

-- | See 'token' and 'tokenBy'.
asToken :: TokenAt tok -> Token tok
asToken = getToken

-- | See 'token' and 'tokenBy'.
asTokenAt :: TokenAt tok -> TokenAt tok
asTokenAt = id

-- | See 'token' and 'tokenBy'.
asTriple :: TokenAt tok -> (LineNum, ColumnNum, Token tok)
asTriple tok = (lineNumber tok, columnNumber tok, getToken tok)

-- | See 'token' and 'tokenBy'.
asLineColumn :: TokenAt tok -> (LineNum, ColumnNum)
asLineColumn tok = (lineNumber tok, columnNumber tok)

-- | See 'token' and 'tokenBy'.
asLocation :: TokenAt tok  -> Location
asLocation = uncurry atPoint . asLineColumn

-- | The lexical analysis phase emits a stream of 'TokenAt' objects, but it is not memory
-- efficient to store the line and column number with every single token. To save space, the token
-- stream is "compressed" into 'Lines', where 'TokenAt' that has the same 'lineNumber' is
-- placed into the same 'Line' object. The 'Line' stores the 'lineNumber', and the
-- 'lineNumber's are deleted from every 'TokenAt', leaving only the 'columnNumber' and 'Token'
-- in each line.
data Line tok
  = Line
    { lineLineNumber :: LineNum
    , lineTokens     :: [(ColumnNum, Token tok)]
      -- ^ a list of tokens, each with an associated column number.
    }
instance HasLineNumber (Line tok) where { lineNumber = lineLineNumber }
instance Show tok => Show (Line tok) where
  show line = concat $
    [ "Line ", show (lineLineNumber line), ":\n"
    , intercalate ", " $
        map (\ (col, tok) -> show col++" "++show tok) (lineTokens line)
    ]
instance Functor Line where
  fmap f t =
    Line
    { lineLineNumber = lineLineNumber t
    , lineTokens     = fmap (fmap (fmap f)) (lineTokens t)
    }

lineToTokensAt :: Line tok -> [TokenAt tok]
lineToTokensAt line = map f (lineTokens line) where
  lineNum         = lineNumber line
  f (colNum, tok) =
    TokenAt
    { tokenAtLineNumber   = lineNum
    , tokenAtColumnNumber = colNum
    , getTokenValue       = tok
    }

----------------------------------------------------------------------------------------------------
-- $Error_handling
-- The lexical analyzer and syntactic analysis monads all instantiate
-- 'Control.Monad.Error.Class.MonadError' in the Monad Transformer Library ("mtl" package). This is
-- the type used for 'Control.Monad.Error.Class.throwError' and
-- 'Control.Monad.Error.Class.catchError'.

-- | This data structure is used by both the lexical analysis and the syntactic analysis phase.
data Error st tok
  = Error
    { parseErrLoc     :: Maybe Location
    , parseErrMsg     :: Maybe UStr
    , parseErrTok     :: Maybe (Token tok)
    , parseStateAtErr :: Maybe st
    }
instance Show tok => Show (Error st tok) where
  show err =
    let msg = concat $ map (maybe "" id) $
          [ fmap ((':':) . (++":") . show) (parseErrLoc err)
          , fmap ((" (on token "++) . (++")") . show) (parseErrTok err)
          , fmap ((" "++) . uchars) (parseErrMsg err)
          ]
    in  if null msg then "Unknown parser error" else msg
instance Functor (Error st) where
  fmap f e =
    Error
    { parseErrLoc     = parseErrLoc e
    , parseErrMsg     = parseErrMsg e
    , parseErrTok     = fmap (fmap f) (parseErrTok e)
    , parseStateAtErr = parseStateAtErr e
    }

-- | An initial blank parser error you can use to construct more detailed error messages.
parserErr :: Location -> Error st tok
parserErr loc =
  Error
  { parseErrLoc = Just loc
  , parseErrMsg = Nothing
  , parseErrTok = Nothing
  , parseStateAtErr = Nothing
  }

newError :: Error st tok
newError =
  Error
  { parseErrLoc=Nothing
  , parseErrMsg=Nothing
  , parseErrTok=Nothing
  , parseStateAtErr=Nothing
  }

----------------------------------------------------------------------------------------------------
-- $Lexer_builder
-- When defining a computer language, one essential step will be to define your keywords and
-- operators, and define tokens for these keywords and operators.
--
-- However, it might be more convenient if there was a way to simply declare to your program "here
-- are my keywords, here are my operators, here is how you lex comments, here is how you lex white
-- spaces", stated simply using Haskell functions, and then let the token types be derived from
-- these declarations. The functions in this section intend to provide you with this ability.

-- | Here is class that allows you to create your own token type from a Haskell newtype, like so:
-- > newtype MyToken = MyToken{ unwrapMyToken :: TT }
-- > instance TokenType MyToken where
-- >     'wrapTT' = MyToken
-- >     'unwrapTT' = unwrapMyToken
-- > 
-- > myTokenDB = 'makeTokenDB' $ do
-- >     ....
class (Ix a, Show a) => TokenType a where { wrapTT :: TT -> a; unwrapTT :: a -> TT; }
instance TokenType TT where { wrapTT = id; unwrapTT = id; }

-- | An actual value used to symbolize a type of token is a 'TT'. For example, an integer token
-- might be assigned a value of @('TT' 0)@ a keyword might be @('TT' 1)@, an operator might be
-- @('TT' 2)@, and so on. You do not define the numbers representing these token types, these
-- numbers are defined automatically when you construct a 'LexBuilder'.
--
-- A 'TT' value is just an integer wrapped in an opaque newtype and deriving 'Prelude.Eq',
-- 'Prelude.Ord', 'Prelude.Show', and 'Data.Ix.Ix'. The constructor for 'TT' is not exported, so you
-- can rest assured any 'TT' objects in your program can only be generated during construction of a
-- 'LexBuilder'.
-- 
-- It is also a good idea to wrap this 'TT' type in your own newtype and define your parser over
-- your newtype, which will prevent you from confusing the same 'TT' type in two different parsers.
-- For example:
-- > newtype MyToken { myTokenTT :: TT }
-- > myLexer :: 'Lexer' MyToken ()
-- > myLexer = ...
-- If you instantiate your newtype into the 'TokenType' class, you can also very easily instantiate
-- 'Prelude.Read' and 'Prelude.Show' for your tokens.
newtype TT = MkTT{ intTT :: Int } deriving (Eq, Ord, Ix, Typeable)
instance Show TT where { show (MkTT tt) = "TT"++show tt }

-- Not for export, wouldn't want people making arbitrary 'TT's now, would we?
enumTTFrom :: TT -> TT -> [TT]
enumTTFrom (MkTT a) (MkTT b) = map MkTT [a..b]

-- | The data type constructed from the 'LexBuilder' monad, used to build a 'Lexer' for your
-- programming language, and also can be used to define the 'Prelude.Show' instance for your token
-- type using 'deriveShowFromTokenDB'.
data TokenDB tok =
  TokenDB
  { tableTTtoUStr :: Array TT UStr
  , tableUStrToTT :: M.Map UStr TT
  , tokenDBLexer  :: Lexer tok ()
  }

-- | A state for the 'LexBuilder' monad, used to declaring the regular expressions for a lexer. This
-- state is converted to a 'TokenDB' which is used by the parser to identify tokens.
-- > myTokens :: 'LexBuilder'
-- > myTokens = do
-- >     let keyword = 'stringTable' . 'Prelude.unwords'
-- >     keyword "if then else case of let in where"
-- >     key "() == /= -> \\ : :: ~ @"
-- >     lexer "string.literal"  'lexStringLiteral'
-- >     lexer "comment.endline" 'lexEndlineC_Comment'
-- >     lexer "comment.inline"  'lexInlineC_Comment'
-- >     -- (The dots in the token type name do not mean anything, it just looks nicer.)
data LexBuilderState
  = LexBuilderState
    { regexItemCounter :: TT
    , stringToIDTable  :: M.Map UStr TT
      -- ^ contains all simple lexers which do not establish loops. This would be any lexer that
      -- takes a keyword or operator, or a lexer constructed from a 'Regex' (which might or might
      -- not loop) but produces only one kind of token.
    , buildingLexer    :: Regex
    }
newtype LexBuilder a = LexBuilder{ runLexBuilder :: State LexBuilderState a }
instance Monad LexBuilder where
  return = LexBuilder . return
  (LexBuilder a) >>= b = LexBuilder (a >>= runLexBuilder . b)
instance Functor     LexBuilder where { fmap f (LexBuilder m) = LexBuilder (fmap f m) }
instance Applicative LexBuilder where { pure = return; (<*>) = ap; }
instance Monoid a =>
  Monoid (LexBuilder a) where { mempty = return mempty; mappend = liftM2 mappend; }

-- | This class exists to make 'emptyToken', 'fullToken', and 'activate' functions polymorphic over
-- two different types: the 'RegexBaseType's and 'Regex' and @['Regex']@ types.
class RegexType rx where { toRegex :: rx -> Regex }
instance RegexType  Char         where { toRegex = rx }
instance RegexType  String       where { toRegex = rx }
instance RegexType  UStr         where { toRegex = rx }
instance RegexType (Es.Set Char) where { toRegex = rx }
instance RegexType [Es.Set Char] where { toRegex = rx }
instance RegexType  Regex        where { toRegex = id }
instance RegexType [Regex]       where { toRegex = mconcat }

-- not for export
initLexBuilder :: LexBuilderState
initLexBuilder = LexBuilderState (MkTT 1) mempty (const RxBacktrack)

-- not for export
newTokID :: UStr -> State LexBuilderState TT
newTokID u = do
  tok <- fmap (M.lookup u) (gets stringToIDTable)
  case tok of
    Nothing  -> do
      tok <- gets regexItemCounter
      modify $ \st ->
        st{ regexItemCounter = MkTT (intTT tok + 1)
          , stringToIDTable = M.insert u tok (stringToIDTable st)
          }
      return tok
    Just tok -> return tok

-- not for export
makeRegex :: RegexType rx => Bool -> UStr -> rx -> LexBuilder Regex
makeRegex keep u re = LexBuilder $ newTokID u >>= \tok ->
  return (toRegex re . (if keep then rxToken else rxEmptyToken) tok)

-- | The 'tokenHold' function creates a token ID and associates it with a type that is used to
-- construct a 'Regex', and returns the 'Regex' to the 'LexBuilder' monad. However this does not
-- actually 'activate' the 'Regex', it simply allows you to use the returned 'Regex' in more
-- complex expressions that might construct multiple tokens. You can 'tokenHold' expressions in any
-- order, ordering is not important.
fullToken :: (UStrType str, RegexType rx) => str -> rx -> LexBuilder Regex
fullToken s = makeRegex True (ustr s)

-- | 'token' has identical behavior as 'tokenHold', except the 'Regex' created will produce an empty
-- token, that is, a token that only indicates it's type and contains none of the string that
-- matched the regular expression. This can be done when it is clear (or you do not care) what
-- string was lexed when the token was created just by knowing the type of the token, for example
-- whitespace tokens, or keyword tokens which have the text used to create the token easily
-- retrievable by converting token to a string. Making use of 'token' can greatly improveme
-- performance as compared to using 'tokenHold' exclusively.
emptyToken :: (UStrType str, RegexType rx) => str -> rx -> LexBuilder Regex
emptyToken s = makeRegex False (ustr s)

-- | 'activating' a regular expression actually converts the regular expression to a 'Lexer'. The
-- /order of activation is important/, expressions that are defined first will have the first try at
-- lexing input strings, followed by every expression activated after it in order. It is also NOT
-- necessary to define an expression with 'regex' or 'regexHold' before activating it, however
-- expressions that have not been associated with a token type will simply consume input without
-- producing any tokens. This might be desireable for things like whitespace, but it is usually not
-- what you want to do.
activate :: RegexType rx => rx -> LexBuilder ()
activate re = LexBuilder{
    runLexBuilder = modify (\st -> st{buildingLexer = buildingLexer st <> toRegex re})
  }

-- | Once you have defined your 'LexBuilder' function using monadic notation, convert the value of
-- this function to a 'TokenDB' value. The 'TokenDB' is mostly handy for retreiving the string
-- values associated with each token ID created by the 'newTokenType' function, for example when
-- using 'dervieShowFromTokenDB' which uses a 'TokenDB' to produce a function suitable for
-- instantiating your @'TokenType'@ into the 'Prelude.Show' class.
makeTokenDB :: TokenType tok => LexBuilder a -> TokenDB tok
makeTokenDB builder =
  TokenDB
  { tableTTtoUStr = array (MkTT 1, regexItemCounter st) $
      fmap swap $ fmap (fmap unwrapTT) $ M.assocs tabmap
  , tokenDBLexer  = void $ many $ regexToLexer $ buildingLexer st
  , tableUStrToTT = tabmap
  }
  where
    tabmap = stringToIDTable st
    st = execState (runLexBuilder builder) $
      LexBuilderState{regexItemCounter=MkTT 1, stringToIDTable=mempty, buildingLexer=mempty}

-- | This function lets you easily "derive" the instance for 'Prelude.Show' for a given 'TokenType'
-- associated with a 'TokenDB'. It should be used like so:
-- > newtype MyToken = MyToken{ unwrapMyToken :: TT }
-- > instance 'TokenType' MyToken where{ wrapTT = MyToken; unwrapTT = unwrapMyToken; }
-- > instance 'Prelude.Show' MyToken where{ show = 'deriveShowFromTokenDB' myTokenDB }
-- > myTokenDB :: 'TokenDB' MyToken
-- > myTokenDB = 'makeTokenDB' $ ....
deriveShowFromTokenDB :: TokenType tok => TokenDB tok -> tok -> String
deriveShowFromTokenDB tokenDB tok =
  let str = uchars (tableTTtoUStr tokenDB ! unwrapTT tok)
  in  if or (map (not . isAlphaNum) str) then show str else str

tokTypeToUStr :: (TokenType tok, HasTokenDB tok) => tok -> UStr
tokTypeToUStr tok =
  let arr = (tableTTtoUStr (tokenDBFromToken tok))
      tt  = unwrapTT tok
  in  if inRange (bounds arr) tt
        then  arr!tt
        else  error ("no registered identifier "++show tt++" in tokenDB")  where

tokTypeToString :: (TokenType tok, HasTokenDB tok) => tok -> String
tokTypeToString = uchars . tokTypeToUStr

-- | Get token from a 'TokenDB' that was associated with the 'Dao.String.UStrType'.
maybeLookupToken :: (UStrType str, TokenType tok) => TokenDB tok -> str -> Maybe tok
maybeLookupToken tokenDB = fmap wrapTT . flip M.lookup (tableUStrToTT tokenDB) . ustr

-- | Like 'maybeLookupToken' but evaluates to 'Prelude.error' if no such token was defined.
lookupToken :: (UStrType str, TokenType tok) => TokenDB tok -> str -> tok
lookupToken tokenDB str =
  fromMaybe (error $ "internal: token "++show (ustr str)++" was never defined") $
    maybeLookupToken tokenDB str

mk_keyword :: UStrType str => TT -> Regex -> str -> LexBuilder (UStr, TT)
mk_keyword deflt regex key = do
  let ukey = ustr key
      keyChars = uchars ukey
  if fst (runRegex regex keyChars :: (Bool, ([TokenAt TT], String)))
    then  LexBuilder (newTokID ukey) >>= return . (,) ukey
    else  error ("keyword token "++show keyChars++"does not match it's own keyword Regex")

-- | Create a single keyword 'Regex'. 'Control.Monad.mapM'-ing over this function is not the same as
-- using 'keywordTable', 'keywordTable' creates an actual table when evalauting to a 'Lexer'. This
-- function creates no table, it will simply evaluate to a lexer that returns a token of the keyword
-- type if the keyword matches the input, or else it returns the default token type.
keyword :: (UStrType str, UStrType key) => str -> Regex -> key -> LexBuilder Regex
keyword deflt regex key = do
  deflt      <- LexBuilder (newTokID (ustr deflt))
  (ukey, tt) <- mk_keyword deflt regex key
  return (regex . rxMakeToken (\str -> if ustr str==ukey then (False, tt) else (True, deflt)))

-- | To construct a keyword table, you must provide three parameters: the first two are a default
-- 'TokenType' and a 'Regex' used to split non-keyword words out of the character stream. The third
-- parameter is a list of keywords strings. Every keyword string will become it's own token type.
-- The resultant 'Regex' will, when evaluated as a 'Lexer', first try to split a non-keyword string
-- off of the stream. But if that string matches a keyword, a keyword token of it's own type is
-- emitted. Otherwise, it will emit a non-keyword token of the 'TokenType' given here. Keyword
-- tokens are empty, non-keyword contain the characters that matched the regex.
--
-- A common way to use this function is with the 'Data.List.words' function:
-- > 'keywordTable' MyVarNameType ('rx' ('from' @'@a@'@ 'to' @'@z@'@)) $
-- >     'Data.List.words' $ 'Data.List.unwords' $ 
-- >         [ "while for if then else goto return break continue"
-- >           "class instance new delete super typeof" ]
keywordTable :: (UStrType str, UStrType key) => str -> Regex -> [key] -> LexBuilder Regex
keywordTable deflt regex keys = do
  deflt   <- LexBuilder (newTokID (ustr deflt))
  keyDict <- fmap M.fromList (forM keys (mk_keyword deflt regex))
  return (regex . rxMakeToken (maybe (True, deflt) ((,) False) . flip M.lookup keyDict . ustr))

-- | Creates a token type with 'regex' where the text lexed from the input is identical to name of
-- the token. The difference between an operator and a keyword is that operators will be lexed
-- regardless of the characters following it in the stream, which means if you have an operator "op"
-- and the next characters in the stream are @"open()"@, then the lexical analysis will split this
-- into @["op", "en()"]@, the remainder @"en()"@ characters must be lexed by some other lexer,
-- and @"op"@ will be treated as a single operator token. Keywords do not work this way.
operator :: UStrType str => str -> LexBuilder Regex
operator str = do
  let u = ustr str
  case ulength u of
    0 -> return id
    1 -> makeRegex False u (head (uchars u))
    _ -> makeRegex False u u

-- | Creates a 'TokenTable' using a list of keywords or operators you provide to it. Use this
-- function to ensure operators do not interfear with each other. For example, if you have two
-- operators @"=="@ and @"="@, the lexer must try to split the @"=="@ operator from the stream
-- before it tries splitting @"="@. This function ensures that the order in which operators are
-- tried is the correct order.
-- 
-- Every string provided becomes it's own token type. For example:
-- > myKeywords = 'tokenTable' $ 'Data.List.words' $ 'Data.List.unwords' $
-- >     [ "** * / % + - & | ^",
-- >       "= *= /= %= += -= &= |= ^=",
-- >       "== != <= >= < >" ]
operatorTable :: UStrType str => [str] -> LexBuilder Regex
operatorTable = fmap mconcat . mapM operator . sortBy (flip compare) . map ustr

-- | Retrieve a 'TokenType' from a 'UStrType' (or a subclass of 'UStrType' like 'MetaToken') value.
-- THis is necesary for building tokenizing regular expressions that are more complicated than a
-- typeical keyword or operator. You must declare in the 'Regex' when to create a token from the
-- given token types returned to the 'LexBuilder' monad by this function.
getTokID :: (UStrType tokID, TokenType tok) => tokID -> LexBuilder tok
getTokID tokID = fmap wrapTT (LexBuilder (newTokID (ustr tokID)))

----------------------------------------------------------------------------------------------------
-- $Regular_expressions
-- Provides a very simple data type for constructing expressions that can match strings. This type
-- is used in a 'LexBuilder' to and are associated with token types such than when the 'LexBuilder'
-- is converted to a 'Lexer', a token of the associated type is generated when a regex matches
-- the beginning of the input string that is being lexically analyzed.

-- | Any function with a type @'RegexUnit' -> 'RegexUnit'@ or 'Regex' can be used in a
-- dot-operator-separated sequence of expressions. But the 'rx' function provided by this class
-- introduces a kind of constant expression that can be used in sequences of 'RegexUnit's. For
-- example, suppose you have defined a regular expression @digit@ of type
-- @'RegexUnit' -> 'RegexUnit'@ to match an arbitrarily long sequence of numeric characters, and a
-- regular expression @space@ of type 'Regex' to match an arbitrarily long sequence of whitespace
-- characters. You could then define a regular expression with a sequence like so:
-- > myRegex :: Regex
-- > myRegex = 'rx' "first" . space . digit . space . 'rx' "second" . space . digit
-- Here, we take advantage of the fact that 'Prelude.String' is instnatiated into the
-- 'RegexBaseType' class, because this allows us to write @'rx' "first"@ and @'rx' "second"@, which
-- will match the strings "first" and "second" if they occur in the input string to be matched. This
-- expression is equivalent to the POSIX regular expression
-- @first[[:space:]]+[[:digit:]]+[[:space:]]+second[[:space:]]+[[:digit:]]+@
-- which would match strings like the following:
-- > "first 1231 second 99"
-- > "first    0          second      1233491202"
-- 
-- /To create a choice/ you can use 'Data.Monoid.mappend', 'Data.Monoid.mconcat', or the infix
-- operator equivalent of 'Data.Monoid.mappend', which is 'Data.Monoid.<>'. The reason is,
-- 'Data.Monoid.Monoid' instantiates functions of type @a -> b@, and this provides a default
-- instnatiation for functions of type 'Regex'. For example,
-- suppose you have two regular expressions, @regexA@ and @regexB@. If you want to construct a
-- 'Regex' that tries matching @regexA@, and if it fails, then tries @regexB@, you would write:
-- > matchAorB = regexA 'Data.Monoid.<>' regexB
-- You can use 'Data.Monoid.concat' to create larger choices:
-- > 'Data.Monoid.mconcat' ['rx' "tryThis", 'rx' "thenThis", regexA, regexB]
-- or equivalently:
-- > 'rx' 'Data.Monoid.<>' "tryThis" 'Data.Monoid.<>' 'rx' "thenThis" 'Data.Monoid.<>' regexA 'Data.Monoid.<>' regexB
-- 
-- Another advantage of 'Regex' being a function of type @'RegexUnit' -> 'RegexUnit'@ is that any
-- function of the type @a -> a@ can easily be used with the 'Prelude.fix' function to create loops,
-- for example, to lex an arbitrarily long sequence of numbers separated by spacecs:
-- > do space  <- 'newTokenType' "SPACE"
-- >    number <- 'newTokenType' "NUMBER"
-- >    'regex' $ 'Prelude.fix' $ \loop ->
-- >        ('rxRepeat1'('ch' ' ') . 'rxEmptyToken' space 'Data.Monoid.<>' 'rxRepeat1'('from' '0' 'to' '9') . 'rxToken' number) . loop
type Regex = RegexUnit -> RegexUnit

-- Not for export, this is a type used in the 'RxMakeToken' constructor, it is a basically a data
-- representing a function that can convert a string to a 'TT', but could be a constant function.
data MakeToken = MakeToken (String -> (Bool, TT)) | ConstToken Bool TT
instance Eq MakeToken where
  ConstToken kA ttA == ConstToken kB ttB = kA==kB && ttA==ttB
  _                 == _                 = False
instance Show MakeToken where
  show (ConstToken kA ttA) = (if kA then "full " else "empty ")++show ttA
  show _                   = "MakeToken"

evalMakeToken :: TokenType tok => MakeToken -> String -> Lexer tok ()
evalMakeToken mkt withStr = (if keep then makeToken else makeEmptyToken) (wrapTT tt) where
  (keep, tt) = case mkt of
    ConstToken keep tt -> (keep, tt)
    MakeToken  make    -> make withStr

-- | This is an intermediate type for constructing 'Regex's. You will not be using it directly. You
-- will instead use any function that evaluates to a 'Regex', which is a function of this data type.
data RegexUnit
  = RxBacktrack
  | RxSuccess
  | RxChoice   { getSubRegexes :: [RegexUnit] }
  | RxStep     { rxStepUnit    :: RxPrimitive , subRegex :: RegexUnit }
  | RxExpect   { rxErrMsg      :: UStr        , subRegex :: RegexUnit }
  | RxDontMatch{                                subRegex :: RegexUnit }
  | RxMakeToken{ rxMakeFunc    :: MakeToken   , subRegex :: RegexUnit }
  deriving Eq
instance Show RegexUnit where
  show rx = loop 0 rx where
    loop i rx = if i>2 then "..." else case rx of
      RxBacktrack     -> "RxBacktrack"
      RxSuccess       -> "RxSuccess"
      RxChoice      c -> "RxChoice "++show c
      RxStep      p s -> "RxStep ("++showRegexPrim p++") . "++loop (i+1) s
      RxExpect    e s -> "RxExpect "++show e++" . "++loop (i+1) s
      RxDontMatch   s -> "RxDontMatch ("++loop (i+1) s++")"
      RxMakeToken t s -> "RxMakeToken ("++show t++") . "++loop (i+1) s
instance Show (RegexUnit -> RegexUnit) where { show rx = show (rx RxSuccess) }
instance Monoid RegexUnit where
  mempty = RxBacktrack
  mappend a b = case a of
    RxBacktrack        -> b
    RxSuccess          -> a
    RxExpect    _   _  -> a
    RxStep      a'  ax -> case b of
      RxStep      b'  bx -> case a' of
        RxRepeat lo _ _ | lo==Es.NegInf || lo==Es.Point 0 -> RxStep a' ax
        a'              | a'==b'                          -> RxStep a' (ax<>bx)
        a'                                                -> RxChoice [a,b]
      b                  -> RxChoice [a,b]
    RxDontMatch    ax  -> case b of
      RxDontMatch    bx  -> RxDontMatch (ax<>bx)
      b                  -> RxChoice [a,b]
    RxChoice       ax  -> case b of
      RxChoice       bx  ->
        let list = loop ax bx
        in  if null list
              then RxBacktrack
              else if null (tail list) then head list else RxChoice list
      b                    ->
        let list = loop ax [b]
        in  if null (tail list) then head list else RxChoice list
    RxMakeToken ta ax -> case b of
      RxMakeToken tb bx | ta==tb -> RxMakeToken ta (ax<>bx)
      b                          -> RxChoice [a,b]
    where
      loop ax bx = case ax of
        []   -> bx
        [a]  -> case bx of
          []   -> [a]
          b:bx -> case a<>b of
            RxChoice ax -> ax++bx
            b           -> a:b:bx
        a:ax -> a : loop ax bx

-- | Convert a 'Regex' function to a 'Lexer'. The resulting 'Lexer' will not call 'makeToken' or
-- 'makeEmptyToken', it will only match the beginning of the input string according to the 'Regex',
-- leaving the matched characters in the 'lexBuffer'.
regexToLexer :: TokenType tok => Regex -> Lexer tok ()
regexToLexer re = loop (re RxSuccess) where
  loop re = case re of
    RxBacktrack       -> lexBacktrack
    RxSuccess         -> return ()
    RxMakeToken tt re -> case re of
      RxMakeToken tt re -> loop (RxMakeToken tt re)
      _                 -> gets lexBuffer >>= evalMakeToken tt >> loop re
        -- if there are two 'RxMakeToken's in a row, use the later one. This makes for more
        -- intuitive regular expressions.
    RxChoice       re -> msum $ map loop re
    RxStep    r    re -> do
      keep <- gets lexBuffer
      clearBuffer
      -- fmap (take 4) (gets lexInput) >>= \i -> trace ("lexInput = "++show i++"...") (return ())
      mplus (regexPrimToLexer r >> modify (\st -> st{lexBuffer = keep ++ lexBuffer st}))
            (modify (\st -> st{lexBuffer=keep, lexInput = lexBuffer st ++ lexInput st}) >> mzero)
      loop re
    RxExpect  err  re -> mplus (loop re) (fail (uchars err))
    RxDontMatch    re -> case re of
      RxDontMatch    re -> loop re -- double negative means succeed on match
      re                -> do
        keep <- gets lexBuffer
        matched <- clearBuffer >> mplus (loop re >> return True) (return False)
        if matched
          then  do -- fail if matched
            modify $ \st ->
              st{ lexBuffer = ""
                , lexInput  = keep ++ lexBuffer st ++ lexInput st
                }
            mzero
          else  do -- success if not matched
            modify $ \st ->
              st{ lexBuffer = keep
                , lexInput  = lexBuffer st ++ lexInput st
                }
            return ()

-- Not for export
data RxPrimitive
  = RxDelete
  | RxString   { rxString   :: UStr }
  | RxCharSet  { rxCharSet  :: Es.Set Char }
  | RxRepeat   { rxLowerLim :: Es.Inf Int, rxUpperLim :: Es.Inf Int, subRegexUnit :: RxPrimitive }
  deriving Eq

showRegexPrim :: RxPrimitive -> String
showRegexPrim re = case re of
  RxDelete           -> "(delete)"
  RxString       str -> show str
  RxCharSet      ch  -> show ch
  RxRepeat lo hi re  -> "repeat("++show lo++".."++show hi++", "++showRegexPrim re++")"

regexPrimToLexer :: TokenType tok => RxPrimitive -> Lexer tok ()
regexPrimToLexer re = case re of
  RxDelete          -> clearBuffer
  RxString  str     -> lexString (uchars str)
  RxCharSet set     -> lexCharP (Es.member set)
  -- do i <- gets lexInput
  --    trace ("lexInput: "++show (take 4 i)++" match against "++show set) (lexCharP (Es.member set))
  --    trace "OK" (return ())
  RxRepeat lo hi re -> rept lo hi re
  where
    rept lo hi re = fromMaybe (seq (error "internal error") $! return ()) $ do
      getLo <- mplus (Es.toPoint lo >>= return . lowerLim re) (return (return ()))
      getHi <- mplus (Es.toPoint hi >>= return . upperLim re) (return (noLimit re))
      return (getLo >> mplus getHi (return ()))
    lowerLim re lo = case re of
      RxString  str       -> lowerLimLex lo (lexString (uchars str))
      RxCharSet set       -> do
        keep <- gets lexBuffer
        clearBuffer >> lexWhile (Es.member set)
        got <- gets lexBuffer
        if length got < lo
          then  do
            modify (\st -> st{lexInput = keep ++ got ++ lexInput st, lexBuffer = ""})
            mzero
          else  modify (\st -> st{lexBuffer = keep ++ got})
      RxRepeat lo' hi' re -> lowerLimLex lo (rept lo' hi' re)
    lowerLimLex lo lex = replicateM_ lo lex
    upperLim re hi = case re of
      RxString  str       -> replicateM_ hi (lexString (uchars str))
      RxCharSet set       -> lexWhile       (Es.member set)
      RxRepeat lo' hi' re -> replicateM_ hi (rept lo' hi' re)
    noLimit re = case re of
      RxString  str       -> forever  (lexString (uchars str))
      RxCharSet set       -> lexWhile (Es.member set)
      RxRepeat lo  hi  re -> forever  (rept lo hi re)

-- | Any type which instantiates the 'RegexBaseType' class can be used to with the 'rx' function to
-- construct a part of a 'Regex' which can be used in a sequence of 'Regex's.
--
-- Probably the most useful instance of this class apart from that of 'Prelude.String' is the
-- instance for the type @['Dao.EnumSet.Set' 'Data.Char.Char']@, which allows you to union ranges of
-- character sets like so:
-- > 'rx'['from' '0' 'to' '9', from @'A'@ 'to' @'Z'@, 'from' '@a@' 'to' '@z@', 'ch' '@_@']
-- which would be equivalent to the POSIX regular expression @[0-9A-Za-z_]@, a regular expression
-- that matches any single alphabetic, numeric, of underscore character.
class RegexBaseType t where
  rxPrim :: t -> RxPrimitive
  rx :: t -> Regex
  rx = RxStep . rxPrim
instance RegexBaseType  Char         where { rxPrim = RxCharSet . Es.point }
instance RegexBaseType  String       where { rxPrim = RxString  . ustr     }
instance RegexBaseType  UStr         where { rxPrim = RxString  }
instance RegexBaseType (Es.Set Char) where { rxPrim = RxCharSet }
instance RegexBaseType [Es.Set Char] where { rxPrim = RxCharSet . foldl Es.union mempty }

-- | The type of the 'from' and 'to' functions are specially defined so that you can write ranges of
-- characters. For example, if you want to match upper-case characters, you would simply write:
-- > from 'A' to 'Z'
-- or equivalently:
-- > 'A' `to` 'Z'
-- but I prefer to use 'from' because the use of single quotes and back-quotes together in the same
-- expression is tedius. This would be equivalent to the POSIX regular expressions: @[A-Z]@
from :: Char -> (Char -> Char -> Es.Set Char) -> Char -> Es.Set Char
from a to b = to a b

-- | The type of the 'from' and 'to' functions are specially defined so that you can write ranges of
-- characters. For example, if you want to match upper-case characters, you would simply write:
-- > from 'A' to 'Z'
-- or equivalently:
-- > 'A' `to` 'Z'
-- but I prefer to use 'from' because the use of single quotes and back-quotes together in the same
-- expression is tedius. This would be equivalent to the POSIX regular expressions: @[A-Z]@
to :: Char -> Char -> Es.Set Char
to toch frch = Es.range frch toch

-- | Create a character set that matches only a single character. For example if you want to match
-- just a single lowercase letter-A character:
-- > ch 'a'
-- This would b equivalent to the POSIX regular expression @[a]@
-- Be careful not to confuse this function with the 'rx' function, which is instantiated to create
-- 'Regex' functions from 'Prelude.Char's. The expression @'rx' @'@@a@@'@ cannot be used in a
-- set of other @'Dao.EnumSet.Set' 'Prelude.Char'@ types to create a larger set:
-- > badRegex = 'repeat' ['from' '0' 'to' '9', 'rx' '.'] -- /COMPILE-TIME ERROR!!!/
-- >
-- > -- This matches strings ending in dots, like "98765.", "123.", and "0."
-- > -- but does not match "0.0" or ".123"
-- > numberEndingWithDot 'repeat' ['from' '0' 'to' '9'] . 'rx' '.' 
-- >
-- > -- This matches strings like "0.0.0.0", "123.", ".99", "...", "0.0" and "1..20"
-- > dotsOrDigits = 'repeat' ['from' '0' 'to' '9', 'ch' '.']
-- This function is also useful with the 'Prelude.map' function to create a set of characters from a
-- 'Prelude.String':
-- > stringOfVowels = 'rxRepeat' ('Prelude.map' 'ch' "AEIOUaeiou")
ch :: Char -> Es.Set Char
ch = Es.point

-- | Produces a character set that matches any character, like the POSIX regular expression dot
-- (@.@). /NEVER use this in a 'rxRepeat' function/ unless you really want to dump the entire
-- remainder of the input string into the 'lexBuffer'.
anyChar :: Es.Set Char
anyChar = Es.infinite

-- | Invert a character set: this 'Regex' will match any characters not in the union of the sets
-- provided.
invert :: [Es.Set Char] -> Es.Set Char
invert = Es.invert . foldl Es.union mempty

-- | An optional regex, tries to match, but succeeds regardless of whether or not the given
-- actually matches. In fact, this 'Regex' is exactly identical to the equation:
-- > \regex -> regex 'Data.Monoid.<>' 'Prelude.id'
opt :: Regex -> Regex
opt = (<>id)

-- | This 'Regex' matches nothing and succeeds, and deletes any 'Regex' appended to it with the dot
-- operator. Any 'Regex' occurring after a 'halt' will not be evaluated.
halt :: Regex
halt = const RxSuccess

-- | Marks a point in a 'Regex' sequence where the matching must not fail, and if it does fail, the
-- resulting 'Lexer' to which this 'Regex' evaluates will evaluate to 'Control.Monad.fail' with an
-- error message provided as a paramater to this function. For example:
-- > decimalPoint = digits . 'rx' '.' . 'cantFail' "must have digits after a decimal point" . digits
cantFail :: String -> Regex
cantFail msg = RxExpect (ustr msg)

-- | This is a look-ahead 'Regex' that matches if the 'Regex' parameter provided does not match. An
-- extremely inefficient function, you should avoid using it and consider re-designing your
-- 'LexBuilder' if you rely on this function too often. This function is designed to occur only at
-- the end of your 'Regex', that is, every 'Regex' that occurs after 'rxDont' is part of the 'Regex'
-- to not match. For example:
-- > myRegex = 'rx' "else" . spaces . 'rxDont' . 'rx' "if" . spaces . rx "end"
-- will succeed only if the string else is not followed by a string @"if end"@. There is no way to
-- make the 'Regex' first check if the next string is not @"if"@ and if it is not then continue
-- matching with @spaces@ and @'rx' "end"@ after that.
dont :: Regex
dont = RxDontMatch

-- | Clear the 'lexBuffer' without creating a token, effectively deleting the characters from the
-- input stream, ignoring those characters.
rxClear :: Regex
rxClear = RxStep RxDelete

-- | Force an error to occur.
rxErr :: String -> Regex
rxErr msg = cantFail msg . mempty

-- | Repeat a regular 'RegexBaseType' regular expression a number of times, with the number of times
-- repeated being limited by an upper and lower bound. Fails to match if the minimum number of
-- occurences cannot be matched, otherwise continues to repeat as many times as possible (greedily)
-- but not exceeding the upper bound given.
rxLimitMinMax :: RegexBaseType rx => Int -> Int -> rx -> Regex
rxLimitMinMax lo hi = RxStep . RxRepeat (Es.Point lo) (Es.Point hi) . rxPrim

-- | Repeats greedily, matching the 'RegexBaseType' regular expression as many times as possible,
-- but backtracks if the regex cannot be matched a minimum of the given number of times.
rxLimitMin :: RegexBaseType rx => Int -> rx -> Regex
rxLimitMin lo = RxStep . RxRepeat (Es.Point lo) Es.PosInf . rxPrim

-- | Match a 'RegexBaseType' regex as many times as possible (greedily) but never exceeding the
-- maximum number of times given. Must match at least one character, or else backtracks.
rxLimitMax :: RegexBaseType rx => Int -> rx -> Regex
rxLimitMax hi = RxStep . RxRepeat Es.NegInf (Es.Point hi) . rxPrim

-- | Like 'rx' but repeats, but must match at least one character. It is similar to the @*@ operator
-- in POSIX regular expressions. /WARNING:/ do not create regex loops using only regexs of this
-- type, your regex will loop indefinitely.
rxRepeat :: RegexBaseType rx => rx -> Regex
rxRepeat = RxStep . RxRepeat Es.NegInf Es.PosInf . rxPrim

-- | Defined as @'rxLimitMin' 1@, matches a primitive 'RegexBaseType' one or more times, rather than
-- zero or more times. It is imilar to the @+@ operator in POSIX regular expressions.
rxRepeat1 :: RegexBaseType rx => rx -> Regex
rxRepeat1 = rxLimitMin 1

-- | Create a token, keep the portion of the string that has matched the regex up to this point, but
-- clear the match buffer once the token has been created.
rxToken :: TokenType tok => tok -> Regex
rxToken tok = RxMakeToken (ConstToken True $ unwrapTT tok)

-- | Create a token, disgarding the portion of the string that has matched the regex up to this point.
rxEmptyToken :: TokenType tok => tok -> Regex
rxEmptyToken tok = RxMakeToken (ConstToken False $ unwrapTT tok)

rxMakeToken :: TokenType tok => (String -> (Bool, tok)) -> Regex
rxMakeToken make = RxMakeToken (MakeToken (fmap (fmap unwrapTT) make))

----------------------------------------------------------------------------------------------------
-- $Lexical_Analysis
-- There is only one type used for lexical analysis: the 'Lexer'. This monad is used to analyze
-- text in a 'Prelude.String', and to emit 'Token's. Internally, the 'Token's
-- emitted are automatically stored with their line and column number information in a 'TokenAt'
-- object.
--
-- Although lexical analysis and syntactic analysis are both separate stages, keep in mind that
-- Haskell is a lazy language. So when each phase is composed into a single function, syntactic
-- analysis will occur as tokens become available as they are emitted the lexical analyzer. So what
-- tends to happen is that lexical and syntactic analysis will occur in parallel.
--
-- Although if your syntactic analysis does something like apply 'Data.List.reverse' to the entire
-- token stream and then begin parsing the 'Data.List.reverse'd stream, this will force the entire
-- lexical analysis phase to complete and store the entire token stream into memory before the
-- syntactic analyse can begin. Any parser that scans forward over tokens will consume a lot of
-- memory. But through use of 'Parser' it is difficult to make this mistake.

-- | This is the state used by every 'Lexer'. It keeps track of the line number and column
-- number, the current input string, and the list of emitted 'Token's.
data LexerState tok
  = LexerState
    { lexTabWidth      :: TabWidth
      -- ^ When computing the column number of tokens, the number of spaces a @'\TAB'@ character
      -- counts for should be configured. The default set in 'newLexerState' is 4.
    , lexCurrentLine   :: LineNum
    , lexCurrentColumn :: ColumnNum
    , lexTokenCounter  :: Word
      -- ^ some algorithms would like to know if you lexed any tokens at all, and will fail if you
      -- did not. There needs to be some way of knowing how many tokens your 'Lexer' created.
    , tokenStream      :: [TokenAt tok]
    , lexBuffer        :: String
      -- ^ stores the characters consumed by 'Lexer's. This buffer is never cleared until
      -- 'makeToken' is evaluated. Retrieve this string using:
      -- > 'Control.Monad.State.gets' 'lexBuffer'
    , lexInput         :: String
      -- ^ contains the remainder of the input string to be analyzed. Retrieve this string using:
      -- > 'Control.Monad.State.gets' 'lexInput'
    }
instance HasLineNumber   (LexerState tok) where { lineNumber   = lexCurrentLine }
instance HasColumnNumber (LexerState tok) where { columnNumber = lexCurrentColumn }

-- | Create a new lexer state using the given input 'Prelude.String'. This is only realy useful if
-- you must evaluate 'runLexerState'.
newLexerState :: String -> LexerState tok
newLexerState input =
  LexerState
  { lexTabWidth      = 4
  , lexTokenCounter  = 0
  , lexCurrentLine   = 1
  , lexCurrentColumn = 1
  , tokenStream      = []
  , lexBuffer        = ""
  , lexInput         = input
  }

-- | 'parse' will evaluate the 'Lexer' over the input string first. If the 'Lexer' fails, it
-- will evaluate to a 'Dao.Prelude.PFail' value containing a 'Error' value of type:
-- > 'Error' ('LexerState')
-- However the 'Parser's evaluate to 'Error's containing type:
-- > 'Error' ('TokStreamState' st)
-- This function provides an easy way to convert between the two 'Error' types, however since
-- the state value @st@ is polymorphic, you will need to insert your parser state into the error
-- value after evaluating this function. For example:
-- > case tokenizerResult of
-- >    'Dao.Predicate.PFail' lexErr -> 'Dao.Predicate.PFail' (('lexErrToParseErr' lexErr){'parseStateAtErr' = Nothing})
-- >    ....
lexErrToParseErr :: TokenType tok => Error (LexerState tok) tok -> Error (TokStreamState st tok) tok
lexErrToParseErr lexErr =
  lexErr
  { parseStateAtErr = Nothing
  , parseErrLoc = st >>= \st -> return (atPoint (lexCurrentLine st) (lexCurrentColumn st))
  }
  where { st = parseStateAtErr lexErr }

lexCurrentLocation :: LexerState tok -> Location
lexCurrentLocation st = atPoint (lineNumber st) (columnNumber st)

-- | The 'Lexer' is very similar in many ways to regular expressions, however 'Lexer's always
-- begin evaluating at the beginning of the input string. The lexical analysis phase of parsing
-- must generate 'Token's from the input string. 'Lexer's provide you the means to do with
-- primitive functions like 'lexString', 'lexChar', and 'lexUntil', and combinators like 'defaultTo'
-- and 'lexUntilTermChar'. These primitive functions collect characters into a buffer, and you can
-- then empty the buffer and use the buffered characters to create a 'Token' using the
-- 'makeToken' function.
-- 
-- The 'Control.Monad.fail' function is overloaded such that it halts 'lexecialAnalysis' with a
-- useful error message about the location of the failure. 'Control.Monad.Error.throwError' can
-- also be used, and 'Control.Monad.Error.catchError' will catch errors thrown by
-- 'Control.Monad.Error.throwError' and 'Control.Monad.fail'.  'Control.Monad.mzero' causes
-- backtracking. Be careful when recovering from backtracking using 'Control.Monad.mplus' because
-- the 'lexBuffer' is not cleared. It is usually better to backtrack using 'lexBacktrack' (or don't
-- backtrack at all, because it is inefficient). However you don't need to worry too much; if a
-- 'Lexer' backtracks while being evaluated in lexical analysis the 'lexInput' will not be
-- affected at all and the 'lexBuffer' is ingored entirely.
newtype Lexer tok a = Lexer{
    lexerToPTrans :: PTrans (Error (LexerState tok) tok) (State (LexerState tok)) a
  }
instance Functor (Lexer tok) where { fmap fn (Lexer lex) = Lexer (fmap fn lex) }
instance TokenType tok =>
  Monad (Lexer tok) where
    (Lexer fn) >>= mfn = Lexer (fn >>= lexerToPTrans . mfn)
    return             = Lexer . return
    fail msg           = do
      st <- get
      throwError $
        (parserErr (lexCurrentLocation st)){parseErrMsg = Just (ustr msg)}
instance TokenType tok =>
  MonadPlus (Lexer tok) where
    mplus (Lexer a) (Lexer b) = Lexer (mplus a b)
    mzero                           = Lexer mzero
instance TokenType tok =>
  Applicative (Lexer tok) where { pure = return; (<*>) = ap; }
instance TokenType tok =>
  Alternative (Lexer tok) where { empty = mzero; (<|>) = mplus; }
instance TokenType tok =>
  MonadState (LexerState tok) (Lexer tok) where
    get = Lexer (lift get)
    put = Lexer . lift . put
instance TokenType tok =>
  MonadError (Error (LexerState tok) tok) (Lexer tok) where
    throwError                     = Lexer . throwError
    catchError (Lexer try) catcher = Lexer (catchError try (lexerToPTrans . catcher))
instance TokenType tok =>
  MonadPlusError (Error (LexerState tok) tok) (Lexer tok) where
    catchPValue (Lexer try) = Lexer (catchPValue try)
    assumePValue            = Lexer . assumePValue
instance (TokenType tok, Monoid a) =>
  Monoid (Lexer tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }

-- | Append the first string parameter to the 'lexBuffer', and set the 'lexInput' to the value of
-- the second string parameter. Most lexers simply takes the input, breaks it, then places the two
-- halves back into the 'LexerState', which is what this function does. *Be careful* you don't pass
-- the wrong string as the second parameter. Or better yet, don't use this function.
lexSetState :: TokenType tok => String -> String -> Lexer tok ()
lexSetState got remainder = modify $ \st ->
  st{lexBuffer = lexBuffer st ++ got, lexInput = remainder}

-- | Unlike simply evaluating 'Control.Monad.mzero', 'lexBacktrack' will push the contents of the
-- 'lexBuffer' back onto the 'lexInput'. This is inefficient, so if you rely on this too often you
-- should probably re-think the design of your lexer.
lexBacktrack :: TokenType tok => Lexer tok ig
lexBacktrack = modify (\st -> st{lexBuffer = "", lexInput = lexBuffer st ++ lexInput st}) >> mzero

-- | Single character look-ahead, never consumes any tokens, never backtracks unless we are at the
-- end of input.
lexLook1 :: TokenType tok => Lexer tok Char
lexLook1 = gets lexInput >>= \input -> case input of { "" -> mzero ; c:_ -> return c }

-- | Arbitrary look-ahead, creates a and returns copy of the portion of the input string that
-- matches the predicate. This function never backtracks, and it might be quite inefficient because
-- it must force strict evaluation of all characters that match the predicate.
lexCopyWhile :: TokenType tok => (Char -> Bool) -> Lexer tok String
lexCopyWhile predicate = fmap (takeWhile predicate) (gets lexInput)

-- | A fundamental 'Lexer', uses 'Data.List.break' to break-off characters from the input string
-- until the given predicate evaluates to 'Prelude.True'. Backtracks if no characters are lexed.
lexWhile :: TokenType tok => (Char -> Bool) -> Lexer tok ()
lexWhile predicate = do
  (got, remainder) <- fmap (span predicate) (gets lexInput)
  if null got then mzero else lexSetState got remainder

-- | Like 'lexUnit' but inverts the predicate, lexing until the predicate does not match. This
-- function is defined as:
-- > \predicate -> 'lexUntil' ('Prelude.not' . predicate)
lexUntil :: TokenType tok => (Char -> Bool) -> Lexer tok ()
lexUntil predicate = lexWhile (not . predicate)

-- lexer: update line/column with string
lexUpdLineColWithStr :: TokenType tok => String -> Lexer tok ()
lexUpdLineColWithStr input = do
  st <- get
  let tablen = lexTabWidth st
      countNLs lns cols input = case break (=='\n') input of
        (""    , ""        ) -> (lns, cols)
        (_     , '\n':after) -> countNLs (lns+1) 0 after
        (before, after     ) -> (lns, cols + foldl (+) 0 (map charPrintWidth (before++after)))
      charPrintWidth c = case c of
        c | c=='\t'   -> tablen
        c | isPrint c -> 1
        c             -> 0
      (newLine, newCol) = countNLs (lineNumber st) (columnNumber st) input
  put (st{lexCurrentLine=newLine, lexCurrentColumn=newCol})

-- | Create a 'Token' using the contents of the 'lexBuffer', then clear the 'lexBuffer'. This
-- function backtracks if the 'lexBuffer' is empty. If you pass "Prelude.False' as the first
-- parameter the tokens in the 'lexBuffer' are not stored with the token, the token will only
-- contain the type.
makeGetToken :: TokenType tok => Bool -> tok -> Lexer tok (Token tok)
makeGetToken storeChars typ = do
  st <- get
  let str = lexBuffer st
  token <- case str of
    []               -> mzero
    [c] | storeChars -> return $ CharToken{tokType=typ, tokChar=c}
    cx  | storeChars -> return $ Token{tokType=typ, tokUStr=ustr str}
    _                -> return $ EmptyToken{tokType=typ}
  put $
    st{ lexBuffer   = ""
      , tokenStream = tokenStream st ++
          [ TokenAt
            { tokenAtLineNumber   = lineNumber   st
            , tokenAtColumnNumber = columnNumber st
            , getTokenValue       = token
            } ]
      , lexTokenCounter = lexTokenCounter st + 1
      }
  lexUpdLineColWithStr str
  return token

-- | Create a token in the stream without returning it (you usually don't need the token anyway). If
-- you do need the token, use 'makeGetToken'.
makeToken :: TokenType tok => tok -> Lexer tok ()
makeToken = void . makeGetToken True

-- | Create a token in the stream without returning it (you usually don't need the token anyway). If
-- you do need the token, use 'makeGetToken'. The token created will not store any characters, only
-- the type of the token. This can save a lot of memory, but it requires you have very descriptive
-- token types.
makeEmptyToken :: TokenType tok => tok -> Lexer tok ()
makeEmptyToken = void . makeGetToken False

-- | Clear the 'lexBuffer' without creating a token.
clearBuffer :: TokenType tok => Lexer tok ()
clearBuffer = get >>= \st -> lexUpdLineColWithStr (lexBuffer st) >> put (st{lexBuffer=""})

-- | A fundamental lexer using 'Data.List.stripPrefix' to check whether the given string is at the
-- very beginning of the input.
lexString :: TokenType tok => String -> Lexer tok ()
lexString str =
  gets lexInput >>= assumePValue . maybeToBacktrack . stripPrefix str >>= lexSetState str

-- | A fundamental lexer succeeding if the next 'Prelude.Char' in the 'lexInput' matches the
-- given predicate. See also: 'charSet' and 'unionCharP'.
lexCharP :: TokenType tok => (Char -> Bool) -> Lexer tok ()
lexCharP predicate = gets lexInput >>= \input -> case input of
  c:input | predicate c -> lexSetState [c] input
  _                     -> mzero

-- | Succeeds if the next 'Prelude.Char' on the 'lexInput' matches the given 'Prelude.Char'
lexChar :: TokenType tok => Char -> Lexer tok ()
lexChar c = lexCharP (==c)

-- | Backtracks if there are still characters in the input.
lexEOF :: TokenType tok => Lexer tok ()
lexEOF = fmap (=="") (gets lexInput) >>= guard

-- | Takes a 'tokenStream' resulting from the evaulation of lexical analysis and breaks it into
-- 'Line's. This makes things a bit more efficient because it is not necessary to store a line
-- number with every single token. It is necessary for initializing a 'Parser'.
tokenStreamToLines :: [TokenAt tok] -> [Line tok]
tokenStreamToLines toks = loop toks where
  makeLine num toks =
    Line
    { lineLineNumber = num
    , lineTokens     = map (\t -> (tokenAtColumnNumber t, getToken t)) toks
    }
  loop toks = case toks of
    []     -> []
    t:toks ->
      let num           = tokenAtLineNumber t
          (line, toks') = span ((==num) . tokenAtLineNumber) (t:toks)
      in  makeLine num line : loop toks'

-- | The 'Lexer's analogue of 'Control.Monad.State.runState', runs the lexer using an existing
-- 'LexerState'.
lexicalAnalysis
  :: TokenType tok
  => Lexer tok a -> LexerState tok -> (PValue (Error (LexerState tok) tok) a, LexerState tok)
lexicalAnalysis lexer st = runState (runPTrans (lexerToPTrans lexer)) st

-- | Perform a simple evaluation of a 'Lexer' against the beginning of a string, returning the
-- tokens and the remaining string. Evaluates to @('Prelude.True', (tokens, remainder))@ if the
-- lexer succeeded, evaluates to @('Prelude.False', (tokens, remainder))@ if the lexer failed or
-- backtracked, where @tokens@ are the tokens produced and @remainder@ is the portion of the string
-- that was not tokenized.
runLexer :: TokenType tok => Lexer tok a -> String -> (Bool, ([TokenAt tok], String))
runLexer lexer inputStr =
  let (lexResult, st) = lexicalAnalysis lexer (newLexerState inputStr)
  in  (case lexResult of { OK _ -> True; _ -> False; }, (tokenStream st, lexInput st))

-- | Convert a 'Regex' to a 'Lexer' and match a string against it using 'runLexer', so it only
-- matches at the beginning of a string, not at any arbitrary point in the middle of the string.
runRegex :: TokenType tok => Regex -> String -> (Bool, ([TokenAt tok], String))
runRegex lexer inputStr =
  let (a, (b, c)) = runLexer (regexToLexer lexer) inputStr in (a, (fmap (fmap wrapTT) b, c))

testLexicalAnalysis_withFilePath
  :: (Show tok, TokenType tok)
  => Lexer tok () -> FilePath -> TabWidth -> String -> IO ()
testLexicalAnalysis_withFilePath tokenizer filepath tablen input = putStrLn report where
  (result, st) = lexicalAnalysis tokenizer ((newLexerState input){lexTabWidth=tablen})
  lines  = tokenStreamToLines (tokenStream st)
  more   = take 21 (lexInput st)
  remain = "\nremaining: "++(if length more > 20 then show (take 20 more)++"..." else show more)
  loc    = show (lineNumber st) ++ ":" ++ show (lexCurrentColumn st)
  report = (++remain) $ intercalate "\n" (map showLine lines) ++ '\n' : case result of
    OK      _ -> "No failures during lexical analysis."
    Backtrack -> reportFilepath ++ ": lexical analysis evalauted to \"Backtrack\""
    PFail err -> reportFilepath ++ show err
  showLine line = unlines $ ["----------", show line]
  reportFilepath = (if null filepath then "" else filepath)++":"++loc

-- | Run the 'lexicalAnalysis' with the 'Lexer' on the given 'Prelude.String' and print out
-- every token created.
testLexicalAnalysis
  :: (Show tok, TokenType tok)
  => Lexer tok () -> TabWidth -> String -> IO ()
testLexicalAnalysis a b c = testLexicalAnalysis_withFilePath a "" b c

-- | Run the 'lexicalAnalysis' with the 'Lexer' on the contents of the file at the the given
-- 'System.IO.FilePath' 'Prelude.String' and print out every token created.
testLexicalAnalysisOnFile
  ::(Show tok, TokenType tok) 
  => Lexer tok () -> TabWidth -> FilePath -> IO ()
testLexicalAnalysisOnFile a b c = readFile c >>= testLexicalAnalysis_withFilePath a c b

----------------------------------------------------------------------------------------------------
-- $Fundamental_parser_data_types
-- A parser is defined as a stateful monad for analyzing a stream of tokens. A token stream is
-- represented by a list of 'Line' structures, and the parser monad's jobs is to look at the
-- current line, and extract the current token in the current line in the state, and use the tokens
-- to construct data. 'TokStream' is the fundamental parser, but it might be very tedious to use. It
-- is better to construct parsers using 'TokStream' which is a higher-level, easier to use data type
-- that is converted into the lower-level 'TokStream' type.

-- | The 'TokStreamState' contains a stream of all tokens created by the 'lexicalAnalysis' phase.
-- This is the state associated with a 'TokStream' in the instantiation of
-- 'Control.Monad.State.MonadState', so 'Control.Monad.State.get' returns a value of this data type.
data TokStreamState st tok
  = TokStreamState
    { userState     :: st
    , getLines      :: [Line tok]
    , tokenQueue    :: [TokenAt tok]
      -- ^ single look-ahead is common, but the next token exists within the 'Prelude.snd' value
      -- within a pair within a list within the 'lineTokens' field of a 'Line' data structure.
      -- Rather than traverse that same path every time 'nextToken' or 'withToken' is called, the
      -- next token is cached here.
    , finalLocation :: Location -- ^ the line and column number marking the end of the file.
    }
instance Functor (TokStreamState st) where
  fmap f s =
    TokStreamState
    { userState     = userState s
    , getLines      = fmap (fmap f) (getLines s)
    , tokenQueue    = fmap (fmap f) (tokenQueue s)
    , finalLocation = finalLocation s
    }

newTokStream :: TokenType tok => st -> [Line tok] -> TokStreamState st tok
newTokStream userState lines =
  TokStreamState
  { userState     = userState
  , getLines      = lines
  , tokenQueue    = []
  , finalLocation = LocationUnknown
  }

newTokStreamFromLexer :: TokenType tok => st -> LexerState tok -> TokStreamState st tok
newTokStreamFromLexer userState lexerState =
  (newTokStream userState $ tokenStreamToLines $ tokenStream lexerState)
  { finalLocation =
      atPoint (lexCurrentColumn lexerState) (lexCurrentColumn lexerState)
  }

-- The 'TokStreamState' data structure has a field of a polymorphic type reserved for containing
-- arbitrary stateful information. 'TokStream' instantiates 'Control.Monad.State.Class.MonadState'
-- usch that 'Control.Monad.State.Class.MonadState.get' and
-- 'Control.Monad.State.Class.MonadState.put' return the 'TokStreamState' type, however if you wish
-- to modify the arbitrary state value using a function similar to how the
-- 'Control.Monad.State.Class.MonadState.modify' would do, you can use this function.
--modifyUserState :: TokenType tok => (st -> st) -> TokStream st tok ()
--modifyUserState fn = modify (\st -> st{userState = fn (userState st)})

-- | The task of the 'TokStream' monad is to look at every token in order and construct syntax trees
-- in the 'syntacticAnalysis' phase.
--
-- This function instantiates all the useful monad transformers, including 'Data.Functor.Functor',
-- 'Control.Monad.Monad', 'Control.MonadPlus', 'Control.Monad.State.MonadState',
-- 'Control.Monad.Error.MonadError' and 'Dao.Predicate.MonadPlusError'. Backtracking can be done
-- with 'Control.Monad.mzero' and "caught" with 'Control.Monad.mplus'. 'Control.Monad.fail' and
-- 'Control.Monad.Error.throwError' evaluate to a control value containing a 'Error' value
-- which can be caught by 'Control.Monad.Error.catchError', and which automatically contain
-- information about the location of the failure and the current token in the stream that caused the
-- failure.
newtype TokStream st tok a
  = TokStream{
      parserToPTrans ::
        PTrans (Error (TokStreamState st tok) tok) (State (TokStreamState st tok)) a
    }
instance Functor (TokStream st tok) where { fmap f (TokStream a) = TokStream (fmap f a) }
instance TokenType tok =>
  Monad (TokStream st tok) where
    (TokStream ma) >>= mfa = TokStream (ma >>= parserToPTrans . mfa)
    return a               = TokStream (return a)
    fail msg = do
      tok <- optional (nextToken False)
      st  <- get
      throwError $
        Error
        { parseErrLoc     = fmap asLocation tok
        , parseErrMsg     = Just (ustr msg)
        , parseErrTok     = fmap getToken tok
        , parseStateAtErr = Just st
        }
instance TokenType tok =>
  MonadPlus (TokStream st tok) where
    mzero                             = TokStream mzero
    mplus (TokStream a) (TokStream b) = TokStream (mplus a b)
instance TokenType tok =>
  Applicative (TokStream st tok) where { pure = return ; (<*>) = ap; }
instance TokenType tok =>
  Alternative (TokStream st tok) where { empty = mzero; (<|>) = mplus; }
instance TokenType tok =>
  MonadState (TokStreamState st tok) (TokStream st tok) where
    get = TokStream (PTrans (fmap OK get))
    put = TokStream . PTrans . fmap OK . put
instance TokenType tok =>
  MonadError (Error (TokStreamState st tok) tok) (TokStream st tok) where
    throwError err = do
      st <- get
      assumePValue (PFail (err{parseStateAtErr=Just st}))
    catchError (TokStream ptrans) catcher = TokStream $ do
      pval <- catchPValue ptrans
      case pval of
        OK      a -> return a
        Backtrack -> mzero
        PFail err -> parserToPTrans (catcher err)
instance TokenType tok =>
  MonadPlusError (Error (TokStreamState st tok) tok) (TokStream st tok) where
    catchPValue (TokStream ptrans) = TokStream (catchPValue ptrans)
    assumePValue                   = TokStream . assumePValue
instance (TokenType tok, Monoid a) =>
  Monoid (TokStream st tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }

-- Return the next token in the state along with it's line and column position. If the boolean
-- parameter is true, the current token will also be removed from the state.
nextToken :: TokenType tok => Bool -> TokStream st tok (TokenAt tok)
nextToken doRemove = do
  st <- get
  case tokenQueue st of
    [] -> case getLines st of
      []         -> mzero
      line:lines -> do
        modify (\st -> st{tokenQueue=lineToTokensAt line, getLines=lines})
        nextToken doRemove
    tok:tokx | doRemove -> do
      put (st{tokenQueue=tokx})
      -- trace ("shift: "++show tok++"\nremaining: "++show tokx) (return tok)
      return tok
    tok:tokx            -> do
      -- trace ("look1, current = "++show tok++"\nlook1, next = "++show tokx) $ return ()
      return tok

----------------------------------------------------------------------------------------------------

-- | The public API for the 'TokStream' monad wraps 'TokStream' in a newtype called 'Parser.
data Parser st tok a
  = ParserNull
  | Parser{ parserToTokStream :: TokStream st tok a }
  -- 'Parser' has it's own 'ParserNull' constructor because of lookup tables. When a lookup table is
  -- evaluated, it selects the next parser from the table based on the current token in the stream.
  -- But before it evaluates the next parser, it must shift the current token from the stream. If
  -- the next parser is 'mzero', then the token will need to be unshifed immediately. To prevent
  -- this unnecessary shift-unshift, the looked-up parser is checked: if the next parser is
  -- 'ParserNull' (which could happen often because it is the default value used when constructing
  -- the table) then the current token is not shifted at all.
instance Show (Parser st tok a) where { show p = case p of { ParserNull -> "ParserNull"; Parser _ -> "Parser ..."; }}
instance TokenType tok => Functor (Parser st tok) where
  fmap f p = case p of { ParserNull -> ParserNull; Parser p -> Parser (fmap f p); }
instance TokenType tok =>
  Monad (Parser st tok) where
    return = Parser . return
    parser >>= bind = case parser of
      ParserNull -> ParserNull
      Parser   p -> Parser $ p >>= \a -> case bind a of { ParserNull -> mzero; Parser p -> p; }
    --parserA >> parserB = case parserA of
    --  ParserNull     -> ParserNull
    --  Parser parserA -> case parserB of
    --    ParserNull     -> Parser (parserA >> mzero)
    --    Parser parserB -> Parser (parserA >> parserB)
    fail   = Parser . fail
instance TokenType tok =>
  MonadPlus (Parser st tok) where
    mzero = ParserNull
    mplus a b = case a of
      ParserNull -> b
      Parser   a -> Parser $ case b of
        ParserNull -> a
        Parser   b -> mplus a b
instance TokenType tok => Applicative (Parser st tok) where { pure = return; (<*>) = ap;    }
instance TokenType tok => Alternative (Parser st tok) where { empty = mzero; (<|>) = mplus; }
instance TokenType tok => MonadState st (Parser st tok) where
  get     = Parser (gets userState)
  put ust = Parser (modify (\st -> st{userState=ust}))
instance TokenType tok =>
  MonadError (Error (TokStreamState st tok) tok) (Parser st tok) where
    throwError = Parser . throwError
    catchError trial catcher = Parser $
      catchError (parserToTokStream trial) (\err -> parserToTokStream (catcher err))
instance TokenType tok =>
  MonadPlusError (Error (TokStreamState st tok) tok) (Parser st tok) where
    catchPValue ptrans = Parser (catchPValue (parserToTokStream ptrans))
    assumePValue       = Parser . assumePValue
instance TokenType tok => Monoid (Parser st tok a) where {mempty=mzero; mappend=mplus; }

----------------------------------------------------------------------------------------------------

-- | This class exists to provide the 'token'. Ordinarily you would want to use the 'tokenType'
-- function to construct a parser that succeeds when the 'TokenType' value you provide to it matches
-- the next 'Token' in the 'TokStream'. However you need to have this 'TokenType' value first to
-- construct the parser, which means retrieving it by name from the 'TokenDB'. But things are more
-- convenient if you have a 'MetaType' that you used to construct your 'Lexer' using the 'fullToken'
-- or 'emptyToken' functions, which is possible because these functions construct 'Lexer's from
-- 'UStrType's, and your 'MetaToken' type also instantiates 'UStrType'. If you have created a data
-- type to be used as a 'MetaType' and used it to construct tokens, the 'Dao.String.UStr' value of
-- the 'MetaToken' type is associated with the 'TT' value of your 'TokenType' in the 'TokenDB'. This
-- function conveniently sees your 'MetaToken' type, retrieves it from the 'TokenDB', and uses the
-- 'TT' value retrieved to construct a 'TokStream' using the 'tokenType' function.
-- > newtype MyToken = MyToken{ unwrapMyToken :: TT }
-- > instance 'TokenType' MyToken where { 'unwrapTT' = unwrapMyToken; 'wrapTT' = MyToken; }
-- > data MyMetaTT WHITESPACE | NUMBER | LABEL
-- > 
-- > instance 'MetaToken' MyMetaTT MyToken where { 'tokenDBFromMetaValue' _ = getTokenDB }
-- > instance 'HasTokenDB' MyToken where
-- >     getTokenDB = 'makeTokenDB' $ do
-- >         mySpace  <- emptyToken WHITESPACE $ 'rxRepeat1'('Prelude.map' 'ch' "\n\t ")
-- >         let number = 'from' '0' 'to' '9'
-- >         myNumber <- fullToken  NUMBER     $ 'rxRepeat1' number
-- >         let alpha_ = [ch '_', 'from' '@A@' 'to' '@Z@', 'from' '@a@' 'to' '@z@']
-- >         myLabel  <- fullToken  LABLE      $ 'rxRepeat1' alpha_ . rxRepeat(number : alpha_)
-- >         -- now the 'TT' values have been associated with the 'Dao.String.UStr' values
-- >         -- of your 'MetaToken' type.
-- >         'activate' [mySpace, myNumber, myLabel]
-- > 
-- > data MyAST = NullAST | Assign{ label :: 'Prelude.String', intValue :: 'Prelude.Int' }
-- > mkAssign :: String -> String -> MyAST
-- > mkAssign lbl val = Assign{ label = lbl, intValue = 'Prelude.read' val }
-- > 
-- > -- make a 'TokStream' that parses a label, followed by a space, followed by a number
-- > myTokStream :: TokStream () MyToken MyAST
-- > myTokStream = 'Control.Applicative.pure' mkAssign 'Control.Applicative.<*>' ('token' LABEL) 'Control.Applicative.<*>' ('token' NUMBER)
class (Enum meta, UStrType meta) =>
  MetaToken meta tok | meta -> tok where { tokenDBFromMetaValue :: meta -> TokenDB tok }

-- | This class exists to to provide the 'tokenBy' function.
class TokenType tok => HasTokenDB tok where { tokenDB :: TokenDB tok }
getTokenDB :: HasTokenDB tok => Parser st tok (TokenDB tok)
getTokenDB = return tokenDB

tokenDBFromToken :: HasTokenDB tok => tok -> TokenDB tok
tokenDBFromToken _ = tokenDB

tokenDBFromParser :: HasTokenDB tok => Parser st tok a -> TokenDB tok
tokenDBFromParser _ = tokenDB

-- | If the given 'Parser' backtracks then evaluate to @return ()@, otherwise ignore the result of
-- the 'Parser' and evaluate to @return ()@.
ignore :: TokenType tok => Parser st tok ig -> Parser st tok ()
ignore parser = mplus (parser >> return ()) (return ()) 

-- | Return the default value provided in the case that the given 'Parser' fails, otherwise
-- return the value returned by the 'Parser'.
defaultTo :: TokenType tok => a -> Parser st tok a -> Parser st tok a
defaultTo defaultValue parser = mplus parser (return defaultValue)

-- | Given two parameters: 1. an error message and 2. a 'Parser', will succeed normally if
-- evaluating the given 'Parser' succeeds. But if the given 'Parser' backtracks, this this function
-- will evaluate to a 'Parser' failure with the given error message. If the given 'Parser' fails,
-- it's error message is used instead of the error message given to this function. The string
-- "expecting " is automatically prepended to the given error message so it is a good idea for your
-- error message to simple state what you were expecting, like "a string" or "an integer". I
-- typically write 'expect' statements like so:
-- > fracWithExp = do
-- >     fractionalPart <- parseFractional
-- >     'tokenStrType' 'Alphabetic' (\tok -> tok=="E" || tok=="e")
-- >     'expect' "an integer expression after the 'e'" $ do
-- >         exponentPart <- parseSignedInteger
-- >         return (makeFracWithExp fractionalPart exponentPart :: 'Prelude.Double')
expect
  :: (TokenType tok, UStrType str, MonadError (Error (TokStreamState st tok) tok) (Parser st tok))
  => str -> Parser st tok a -> Parser st tok a
expect errMsg parser = do
  loc <- mplus (look1 asLocation) (Parser (gets finalLocation))
  let expectMsg = "expecting " ++ uchars (ustr errMsg)
  mplus parser (throwError ((parserErr loc){parseErrMsg = Just (ustr expectMsg)}))

-- | Given a constructor that takes an arbitray value and a 'Dao.NewTokStream.Location' value, and a
-- 'Dao.NewTokStream.Parser' that evaluates to the same type of arbitrary value, this function
-- automatically notes the location of the current token, then evaluates the parser, then notes the
-- location of the next token to create a 'Dao.NewTokStream.Location' value and apply it to the
-- constructor.
withLoc :: TokenType tok => Parser st tok (Location -> a) -> Parser st tok a
withLoc parser = do
  before <- look1 asLocation
  cons   <- parser
  after  <- mplus (look1 asLocation) (Parser (gets finalLocation))
  return (cons (before<>after))

shift :: TokenType tok => (TokenAt tok -> a) -> Parser st tok a
shift as = Parser (fmap as (nextToken True))

look1 :: TokenType tok => (TokenAt tok -> a) -> Parser st tok a
look1 as = Parser (fmap as (nextToken False))

unshift :: TokenType tok => TokenAt tok -> Parser st tok ()
unshift tok = Parser $ modify (\st -> st{tokenQueue = tok : tokenQueue st})

metaTypeToTokenType :: (TokenType tok, MetaToken meta tok) => meta -> tok
metaTypeToTokenType meta =
  case M.lookup (ustr meta) (tableUStrToTT (tokenDBFromMetaValue meta)) of
    Nothing  -> error $ "internal: parser defined to use meta token "++show (ustr meta)++
      " without having activated any tokenizer that constructs a token of that meta type"
    Just tt -> wrapTT tt

-- | This function takes two parameters, the first is a polymorphic function we can call
-- @getter@ that takes some of the contents of the current token in the stream. The first value
-- is a 'MetaToken' value we can call @meta@. This function will check the whether current token
-- in the stream has an identifier value that matches the given @meta@ value. If so, the current
-- token is shifted off of the stream and passed to the @getter@ function to extract the
-- necessary information from the token.
-- 
-- Valid @getter@ functions are 'asTokType', 'asString', 'asUStr', 'as0', 'asToken',
-- 'asTokenAt', 'asTriple', 'asLineColumn', 'asLocation', or any composition of functions with
-- any of the above as the right-most function.
token :: (TokenType tok, MetaToken meta tok) => meta -> (TokenAt tok -> a) -> Parser st tok a
token meta as = look1 asTokType >>= \tok ->
  if tok == metaTypeToTokenType meta then shift as else mzero

-- | Useful for keywords or operators, this function is used to check if the next 'Token' value
-- in the 'Parser' is of a 'TokenType' labeled by the given constant string. This function
-- has similar behavior to @('tokenString' 'shift')@, /HOWEVER/ unlike 'tokenString', /this
-- function is much more efficient/ because the 'Token' identifier is looked up in the 'TokenDB'
-- only once and then used to add this parser to a parse table instead of merely comparing the
-- string value of the token.
-- 
-- Valid @getter@ functions are 'asTokType', 'asString', 'asUStr', 'as0', 'asToken',
-- 'asTokenAt', 'asTriple', 'asLineColumn', 'asLocation', or any composition of functions with
-- any of the above as the right-most function.
tokenBy :: (UStrType name, HasTokenDB tok) => name -> (TokenAt tok -> a) -> Parser st tok a
tokenBy name as = do
  db <- getTokenDB
  let uname = ustr name 
  tok <- look1 id
  case M.lookup uname (tableUStrToTT db) of
    Nothing -> if asUStr tok == uname then shift as else mzero
    Just tt -> if unwrapTT (asTokType tok) == tt then shift as else mzero

-- | A 'marker' immediately stores the cursor onto the runtime call stack. It then evaluates the
-- given 'Parser'. If the given 'Parser' fails, the position of the failure (stored in a
-- 'Dao.Token.Location') is updated such that the starting point of the failure points to the cursor
-- stored on the stack by this 'marker'. When used correctly, this function makes error reporting a
-- bit more helpful.
marker :: (TokenType tok, MonadPlusError (Error (TokStreamState st tok) tok) (Parser st tok)) =>
  Parser st tok a -> Parser st tok a
marker parser = do
  before <- mplus (look1 asLocation) (Parser (gets finalLocation))
  flip mapPFail parser $ \parsErr ->
    parsErr{parseErrLoc = parseErrLoc parsErr >>= \after -> return(before<>after)}

-- | The 'Parser's analogue of 'Control.Monad.State.runState', runs the parser using an existing
-- 'TokStreamState'.
runParserState
  :: TokenType tok
  => Parser st tok a
  -> TokStreamState st tok
  -> (PValue (Error (TokStreamState st tok) tok) a, TokStreamState st tok)
runParserState (Parser (TokStream parser)) = runState (runPTrans parser)

-- | This is the second phase of parsing, takes a stream of tokens created by the 'lexicalAnalysis'
-- phase (the @['Line' tok]@ parameter) and constructs a syntax tree data structure using the
-- 'Parser' monad provided.
syntacticAnalysis
  :: TokenType tok
  => Parser st tok synTree
  -> TokStreamState st tok
  -> (PValue (Error (TokStreamState st tok) tok) synTree, TokStreamState st tok)
syntacticAnalysis = runParserState

getNullToken :: TokenType tok => Parser st tok tok
getNullToken = return (wrapTT (MkTT 0))

isEOF :: TokenType tok => Parser st tok Bool
isEOF = Parser (get >>= \st -> return (null (getLines st) && null (tokenQueue st)))

----------------------------------------------------------------------------------------------------

-- | Parse table, used to create arrays of tokens that can be used to efficiently select the next
-- parse action to take based on the value of the current token in the token stream.
data PTable st tok a
  = PTableNull
  | PTable { pTableArray :: Array TT (TokenAt tok -> Parser st tok a) }
instance TokenType tok => Functor (PTable st tok) where
  fmap f ptab = case ptab of
    PTableNull -> PTableNull
    PTable arr -> PTable (fmap (fmap (fmap f)) arr)
instance TokenType tok => Monoid (PTable st tok a) where
  mempty      = PTableNull
  mappend a b = case a of
    PTableNull -> b
    PTable arr -> case b of
      PTableNull  -> PTable arr
      PTable arr' -> PTable (mergeArrays mappend (\_ -> mzero) arr arr' )

newtype TableItem st tok a = TableItem{ tableItemToPair :: (tok, TokenAt tok -> Parser st tok a) }
instance TokenType tok => Functor (TableItem st tok) where
  fmap f (TableItem ti) = TableItem (fmap (fmap (fmap f)) ti)

tableItem :: (TokenType tok, MetaToken meta tok) =>
  meta -> (TokenAt tok -> Parser st tok a) -> TableItem st tok a
tableItem meta parser = TableItem (metaTypeToTokenType meta, parser)

tableItemBy :: (TokenType tok, HasTokenDB tok, UStrType name) =>
  name -> (TokenAt tok -> Parser st tok a) -> TableItem st tok a
tableItemBy name parser =
  case M.lookup (ustr name) (tableUStrToTT (tokenDBFromParser (parser undefined))) of
    Nothing  -> error ("tableItemBy "++show (ustr name)++" not activated in TokenDB")
    Just tok -> TableItem (wrapTT tok, parser)

evalPTableItem :: TokenType tok => TableItem st tok a -> Parser st tok a
evalPTableItem (TableItem (tokType, parser)) =
  look1 asTokType >>= \t -> if tokType==t then shift id >>= parser else mzero

table :: TokenType tok => [TableItem st tok a] -> PTable st tok a
table tokParserAssocs = case asocs of
  []          -> PTableNull
  (tok1, _):_ -> PTable (accumArray mappend (\_ -> mzero) bnds ttParserAssocs) where
    asocs = fmap tableItemToPair tokParserAssocs
    ttParserAssocs = fmap (\ (tok, parser) -> (unwrapTT tok, parser)) asocs
    tt = unwrapTT tok1
    bnds = foldl (\ (lo, hi) (tt, _) -> (min lo tt, max hi tt)) (tt, tt) ttParserAssocs
  where
    asocs = fmap tableItemToPair tokParserAssocs

bindPTableItem :: TokenType tok =>
  TableItem st tok a -> (a -> Parser st tok b) -> TableItem st tok b
bindPTableItem (TableItem item) bind = TableItem (fmap (fmap (>>=bind)) item)

bindPTableItemList :: TokenType tok =>
  [TableItem st tok a] -> (a -> Parser st tok b) -> [TableItem st tok b]
bindPTableItemList list bind = fmap (flip bindPTableItem bind) list

bindPTable :: TokenType tok =>
  PTable st tok a -> (a -> Parser st tok b) -> PTable st tok b
bindPTable table bind = case table of
  PTableNull -> PTableNull
  PTable arr -> PTable (fmap (fmap (>>=bind)) arr)

evalPTable :: TokenType tok => PTable st tok a -> Parser st tok a
evalPTable ptable = case ptable of
  PTableNull -> mzero
  PTable arr -> do
    tok <- look1 id
    let tt = unwrapTT (asTokType tok)
    -- trace ("evalPTable("++show (asTokType tok)++") "++show (map (\t -> wrapTT t `asTypeOf` (asTokType tok)) $ uncurry enumTTFrom (bounds arr))) $ return ()
    if inRange (bounds arr) tt
      then  case (arr!tt) tok of
              ParserNull -> mzero
              next       -> shift as0 >> next
      else  mzero

----------------------------------------------------------------------------------------------------
-- $Infix_operator_table

-- | An infix constructor is a function of this form. It takes a 'Location' as it's final parameter,
-- which will denote the location of the @op@ token. The 'Location' can just be ignored if you want.
type InfixConstr op obj = obj -> op -> obj -> obj

-- | Used to define right or left associativity for infix operators.
newtype Associativity = Associativity{ associatesLeft :: Bool } deriving (Eq, Ord)
instance Show Associativity where
  show (Associativity left) = if left then "leftAssoc" else "rightAssoc"

associatesRight :: Associativity -> Bool
associatesRight = not . associatesLeft

rightAssoc :: Associativity
rightAssoc = Associativity False

leftAssoc :: Associativity
leftAssoc  = Associativity True

runAssociativity :: Associativity -> InfixConstr op obj -> obj -> [(obj, op)] -> obj
runAssociativity assoc constr obj stack =
  if associatesLeft assoc
    then  foldl (\ lhs (rhs, op) -> constr lhs op rhs) obj stack
    else  (foldr (\ (rhs, op) next initObj -> constr initObj op (next rhs)) id stack) obj

-- | This data type is used to relate an 'Associativity' and an 'InfixConstr' to some operators,
-- the operators being given as strings.
data OpPrec op obj = OpPrec{ opPrecTo3Tuple :: (Associativity, [UStr], InfixConstr op obj) }

opPrec :: UStrType str => Associativity -> [str] -> InfixConstr op obj -> OpPrec op obj
opPrec a b c = OpPrec (a, map ustr b, c)

opLeft :: UStrType str => [str] -> InfixConstr op obj -> OpPrec op obj
opLeft = opPrec leftAssoc

opRight :: UStrType str => [str] -> InfixConstr op obj -> OpPrec op obj
opRight = opPrec rightAssoc

-- Stored in the 'OpTableParser', used to make choices on when to stack a token and when backtrack.
type OpTablePrec op obj = Maybe (Associativity, Int, InfixConstr op obj)

-- | Use this data type to setup an operator table parser which parses a sequence of @obj@ type data
-- separated by @op@ type operator tokens, where the @op@ tokens have been assigned the properties
-- of fixity and right or left associativity.
data OpTableParser st tok op obj
  = OpTableParser
    { opTableErrMsg      :: UStr
    , opTableAutoShift   :: Bool
      -- ^ instructs the 'evalOpTableParser' whether or not to automatically shift the operator
      -- token from the token stream. Usually you should set this tor 'Prelude.True'. However if
      -- your @op@ parser is more complicated than just converting a token, and actually needs to
      -- parse tokens in between terms, set this to 'Prelude.False' and make the 'opTableOpAs'
      -- function perform parsing and shifting of the operator. If you do set this to
      -- 'Prelude.False', make sure your 'opTableOpAs' function evaluates 'shift' at least once.
      -- The original reason for providing this option is to more easily build parsers that keep
      -- comments in the abstract syntax tree. Parsers that keep comments will usually parse all
      -- comments at the beginning of the file, then proceed with parsing, and every token parsed
      -- will have comments immediately after it parsed and paired with it. But it would be
      -- impossible to create such a parser without the ability to specify exactly when to shift the
      -- operator token and parse the comments.
    , opTableOpAs        :: TokenAt tok -> Parser st tok op
      -- ^ 'TokenAt' values are automatically retrieved from the token stream by the
      -- 'evalOpTableParser' function, and this function is used to convert those 'TokenAt' values
      -- to values of data type @op@. During evaluation of 'evalOpTableParser' this function is
      -- evaluated before the operator token is shifted from the token stream.
    , opTableObjParser   :: Parser st tok obj
      -- ^ This function will parse the non-opreator values of the equation.
    , opTableConstr      :: InfixConstr op obj
      -- ^ This function is used to construct an @obj@ from a stack of @obj@ and @op@ values, it is
      -- passed to 'runAssociativity'. In an arithmetic parser, for example, this function might be
      -- of the form:
      -- > constr :: Int -> String -> Int -> Int
      -- > constr a op b = if op=="+" then a+b else if op=="*" then a*b
    , opTableArray  :: Maybe (Array TT (OpTablePrec op obj))
    }

-- | Evaluate an 'OpTableParser' to a 'Parser'.
evalOpTableParser :: (HasTokenDB tok, TokenType tok, Show obj) => OpTableParser st tok op obj -> Parser st tok obj
evalOpTableParser optab = evalOpTableParserWithInit (opTableObjParser optab) optab

-- | Same as 'evalOpTableParser', but lets you provide a different parser for parsing the first
-- object term in the expression. This is useful if the function for parsing the initial term of the
-- expression is a prefix expression (but necessarily returns the same data type) of the function
-- that parses object terms in the expression. After the initial parser function is evaluated, the
-- 'opTableObjParser' function is used for every other term after it.
evalOpTableParserWithInit
  :: (HasTokenDB tok, TokenType tok, Show obj)
  => Parser st tok obj
  -> OpTableParser st tok op obj
  -> Parser st tok obj
evalOpTableParserWithInit initParser optab = do
  initObj <- initParser
  maybe (return initObj) (begin initObj) (opTableArray optab)
  where
    begin obj table = mplus (lookGetPrec table >>= bindLeft table obj) (return obj)
    lookGetPrec table = do
      tok <- look1 id
      let tt = unwrapTT (asTokType tok)
      if inRange (bounds table) tt
        then  case table!tt of
                Nothing      -> mzero
                Just (a,b,c) -> return (tok, a, b, c)
        else  mzero
    bindLeft table leftObj (tok, assoc, prec, constr) = flip mplus (return leftObj) $ do
      op <- opTableOpAs optab tok
      if opTableAutoShift optab then shift as0 else return ()
      (rightObj, opInfo) <- bindRight table tok assoc prec
      let obj = constr leftObj op rightObj
      case opInfo of
        Nothing     -> return obj
        Just opInfo -> bindLeft table obj opInfo
    bindRight table tok assoc prec = do
      expect (uchars (opTableErrMsg optab)++" after "++show tok++" token") $ do
        leftObj <- opTableObjParser optab
        flip mplus (return (leftObj, Nothing)) $ do
          opInfo@(nextTok, nextAssoc, nextPrec, constr) <- lookGetPrec table
          let mergeStack = return (leftObj, Just opInfo)
          let msg assoc = "associates to the "++(if associatesLeft assoc then "left" else "right")
          case () of
            () | assoc/=nextAssoc      && prec==nextPrec -> fail $ unwords $
              [ "ambiguous experession, infix operators"
              , show tok, "and", show nextTok, "are of the same prescedence, but"
              , show tok, msg assoc, "whereas", show nextTok, msg nextAssoc
              ]
            () | associatesLeft  assoc && prec>=nextPrec -> mergeStack
            () | associatesRight assoc && prec> nextPrec -> mergeStack
            ()                                           -> do
              op <- opTableOpAs optab nextTok
              if opTableAutoShift optab then shift as0 else return ()
              (rightObj, opInfo) <- bindRight table nextTok nextAssoc nextPrec
              return (constr leftObj op rightObj, opInfo)

-- | Sets up the 'OpTableParser' data structure.
--
-- The first parameter is a function used to parse the right and left hand sides of each infix
-- operator. This function may safely recurse to itself via the 'Parser' created by the evaluation
-- of 'evalOpTableParser' provided that there is at least one other parser that does not recurse
-- which is tried before it. For example:
-- > myOperatorTable :: OpPrecTable MyOperator MySymbol
-- > myOperatorFromToken :: 'TokenAt' MyTokType -> 'Parser' () MyTokType MyOperator
-- > nonRecursive :: 'Parser' () MyTokType MySymbol -- This parser never recurses to 'myExpression'.
-- >
-- > -- This parser does recurse to 'myExpression'
-- > recursive :: 'Parser' () MyTokType MySymbol
-- > recursive = 'evalOpTableParser' myExpression
-- >
-- > -- This expression parser is OK, it will not loop infinitely as long as nonRecursive takes at
-- > least one token from the token stream.
-- > myExpression :: 'OpTableParser' () MyTokType MyOperator MySymbol
-- > myExpression = 'newOpTableParser' (nonRecursive >> recursive) myOperatorFromToken myOperatorTable
-- >
-- > -- These expression parsers will loop infinitely doing nothing:
-- > badExpression1 = 'newOpTableParser' recursive myOperatorFromToken myOperatorTable
-- > badExpression2 = 'newOpTableParser' (nonRecursive<|>recursive) myOperatorFromToken myOperatorTable
-- > 
-- 
-- The second parameter is a function which produces an @op@ data type from a 'TokenAt' value.
-- Operators are taken from the token stream by the table evaluator, and this function will take the
-- 'TokenAt' value provided by the table evaluator and convert it to an operator data type. The @op@
-- typed data evaluated from this function will be used to construct the final @obj@ value.
-- 
-- The final parameters is an 'OpPrecTable' which you construct with the 'opPrecTable' or 'opTable'
-- functions, where you will assign prescedence and associativity properties to every operator
-- token.
newOpTableParser
  :: (HasTokenDB tok, TokenType tok, UStrType errMsg)
  => errMsg
  -> Bool
  -> (TokenAt tok -> Parser st tok op)
  -> Parser st tok obj
  -> InfixConstr op obj
  -> [OpPrec op obj]
  -> OpTableParser st tok op obj
newOpTableParser errMsg autoShift asOp objParser constr optab =
  OpTableParser
  { opTableErrMsg    = ustr errMsg
  , opTableAutoShift = autoShift
  , opTableObjParser = objParser
  , opTableOpAs      = asOp
  , opTableConstr    = constr
  , opTableArray     = inferTypes objParser tokenDB optab
  }
  where
    ttBounds (tt, _) = foldl (\ (lo, hi) (tt, _) -> (min lo tt, max hi tt)) (tt, tt) . tail
    maxPrec = length optab
    precList :: (TokenType tok, HasTokenDB tok) => Parser st tok obj -> TokenDB tok -> [OpPrec op obj] -> [(TT, OpTablePrec op obj)]
    precList parser db optab = do
      (n, (assoc, operatorUStrs, constr)) <- zip [0..] (fmap opPrecTo3Tuple optab)
      op <- operatorUStrs
      case M.lookup op (tableUStrToTT db) of
        Nothing -> error ("internal, token "++show op++" has not been activated in the TokenDB")
        Just op -> return (op, Just (assoc, maxPrec-n, constr))
    inferTypes :: (TokenType tok, HasTokenDB tok) => Parser st tok obj -> TokenDB tok -> [OpPrec op obj] -> Maybe (Array TT (OpTablePrec op obj))
    inferTypes parser db optab = case precList parser db optab of
      [] -> Nothing
      ax -> Just (accumArray (flip const) Nothing (ttBounds (head ax) ax) ax)

-- | Evaluate a parser with infix operators in between each term. Provide an initial parser to be
-- evaluated for the first term only, then the function for parsing terms of the expression, then
-- the infix operator parser. All infix operators will be of the same prescedence and the given
-- associativity. No tables are created by this function, so it is a good idea for your infix
-- operator parser and or the parser for evaluating terms to be functions of the 'table' function.
simpleInfixedWithInit
  :: (TokenType tok, UStrType errMsg)
  => errMsg
  -> Associativity
  -> InfixConstr op obj
  -> Parser st tok obj
  -> Parser st tok obj
  -> Parser st tok op
  -> Parser st tok obj
simpleInfixedWithInit errMsg assoc constr initPar objPar opPar = initPar >>= loop [] where
  loop stack initObj = flip mplus (return (runAssociativity assoc constr initObj stack)) $
    opPar >>= \op -> expect (ustr errMsg) (objPar >>= \o -> loop (stack++[(o, op)]) initObj)

-- | Same as 'simpleInfixedWithInit' except the fourth parameter (the function for parsing the terms
-- of the expression) is used as both the initial term parser and the function for parsing terms of
-- the expression.
simpleInfixed
  :: (TokenType tok, UStrType errMsg)
  => errMsg
  -> Associativity
  -> InfixConstr op obj
  -> Parser st tok obj
  -> Parser st tok op
  -> Parser st tok obj
simpleInfixed errMsg assoc constr objPar opPar = objPar >>= loop [] where
  loop stack initObj = flip mplus (return (runAssociativity assoc constr initObj stack)) $
    opPar >>= \op -> expect (ustr errMsg) (objPar >>= \o -> loop (stack++[(o, op)]) initObj)

----------------------------------------------------------------------------------------------------
-- | A 'Language' is a data structure that allows you to easily define a
-- two-phase parser (a parser with a 'lexicalAnalysis' phase, and a 'syntacticAnalysis' phase). The
-- fields supplied to this data type define the grammar, and the 'parse' function can be used to
-- parse an input string using the context-free grammar defined in this data structure. *Note* that
-- the parser might have two phases, but because Haskell is a lazy language and 'parse' is a pure
-- function, both phases happen at the same time, so the resulting parser does not need to parse the
-- entire input in the first phase before beginning the second phase.
-- 
-- This data type can be constructed from a 'Parser' in such a way that the resulting
-- 'Parser' is stored in this object permenantly. It might then be possible to reduce
-- initialization time by using an *INLINE* pragma, which will hopefully cause the compiler to
-- define as much of the 'Parser'@'@s sparse matrix as it possibly can at compile time. But this
-- is not a guarantee, of course, you never really know how much an optimization helps until you do
-- proper profiling.
data Language st tok synTree
  = Language
    { columnWidthOfTab :: TabWidth
      -- ^ specify how many columns a @'\t'@ character takes up. This number is important to get
      -- accurate line:column information in error messages.
    , mainLexer        :: Lexer tok ()
      -- ^ *the order of these tokenizers is important,* these are the tokenizers passed to the
      -- 'lexicalAnalysis' phase to generate the stream of tokens for the 'syntacticAnalysis' phase.
    , mainParser    :: Parser st tok synTree
      -- ^ this is the parser entry-point which is used to evaluate the 'syntacticAnalysis' phase.
    }

-- | Construct a 'Language' from a 'Parser'. This defines a complete parser that can be used
-- by the 'parse' function. In constructing this 'Language', the 'Parser' will be converted
-- to a 'Parser' which can be referenced directly from this object. This encourages the runtime
-- to cache the 'Parser' which can lead to better performance. Using an INLINE pragma on this
-- value could possibly improve performance even further.
newLanguage :: (HasTokenDB tok, TokenType tok) =>
  TabWidth -> Parser st tok synTree -> Language st tok synTree
newLanguage tabw parser =
  Language
  { columnWidthOfTab = tabw
  , mainLexer        = tokenDBLexer tokenDB
  , mainParser       = parser
  }

-- | This is /the function that parses/ an input string according to a given 'Language'.
parse :: TokenType tok =>
  Language st tok synTree -> st -> String -> PValue (Error st tok) synTree
parse lang st input = case lexicalResult of
  OK      _ -> case parserResult of
    OK     a  -> OK a
    Backtrack -> Backtrack
    PFail err -> PFail $ err{parseStateAtErr = Just (userState parserState)}
  Backtrack -> Backtrack
  PFail err -> PFail $ (lexErrToParseErr err){parseStateAtErr = Nothing}
  where
    initState = (newLexerState input){lexTabWidth = columnWidthOfTab lang}
    (lexicalResult, lexicalState) = lexicalAnalysis (mainLexer lang) initState
    (parserResult , parserState ) =
      syntacticAnalysis (mainParser lang) (newTokStreamFromLexer st lexicalState)

----------------------------------------------------------------------------------------------------

mergeArrays :: Ix i => (e -> e -> e) -> e -> Array i e -> Array i e -> Array i e
mergeArrays plus zero a b =
  accumArray plus zero (boun (bounds a) (bounds b)) (assocs a ++ assocs b) where
    boun (loA, hiA) (loB, hiB) = (min loA loB, max hiA hiB)

