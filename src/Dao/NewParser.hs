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
import           Control.Monad.State
import           Control.Monad.Error hiding (Error)

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
  deriving (Eq, Typeable)
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
-- $All about tokens
-- This module was designed to create parsers which operate in two phases: a lexical analysis phase
-- (see 'lexicalAnalysis') where input text is split up into tokens, and a syntactic analysis phase
-- where a stream of tokens is converted into data. 'Token' is the data type that makes this
-- possible.

-- | Used by 'TokenAt' and 'Line'. This class is probably not useful outside of this module.
class HasLineNumber   a where
  lineNumber    :: a -> LineNum
  setLineNumber :: a -> LineNum -> a

-- | Used by 'TokenAt' and 'Line'. This class is probably not useful outside of this module.
class HasColumnNumber a where
  columnNumber    :: a -> ColumnNum
  setColumnNumber :: a -> ColumnNum -> a

-- | Every token emitted by a lexical analyzer must have at least a type. 'Token' is polymorphic
-- over the type of token. The 'MonadParser' class only requires tokens to instantiate 'Prelude.Eq',
-- but you will find that the useful parser defined in this module, the 'Parser', requires tokens
-- to instantiate 'Data.Ix.Ix' so that tokens can be used as indecies to 'Data.Array.IArray.Array's
-- in order to implement fast lookup tables.
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
  show tok = show (tokType tok) ++ " " ++ show (tokToUStr tok)
instance TokenType tok => CFG Token tok where
  castTT t = case t of
    EmptyToken t   -> EmptyToken (wrapTT t)
    CharToken  t c -> CharToken  (wrapTT t) c
    Token      t u -> Token      (wrapTT t) u

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
data TokenAt tok =
  TokenAt
  { tokenAtLineNumber   :: LineNum
  , tokenAtColumnNumber :: ColumnNum
  , getToken            :: Token tok
  }
instance HasLineNumber   (TokenAt tok) where
  lineNumber        = tokenAtLineNumber
  setLineNumber a n = a{tokenAtLineNumber=n}
instance HasColumnNumber (TokenAt tok) where
  columnNumber        = tokenAtColumnNumber
  setColumnNumber a n = a{tokenAtColumnNumber=n}
instance TokenType tok =>
  CFG TokenAt tok where
    castTT t = case t of
      TokenAt line col t ->
        TokenAt
        { tokenAtLineNumber   = line
        , tokenAtColumnNumber = col
        , getToken            = castTT t
        }

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
instance HasLineNumber (Line tok) where
  lineNumber        = lineLineNumber
  setLineNumber a n = a{lineLineNumber=n}
instance Show tok => Show (Line tok) where
  show line = show (lineLineNumber line) ++ ": " ++ show (lineTokens line)
instance TokenType tok =>
  CFG Line tok where
    castTT t =
      Line
      { lineLineNumber = lineLineNumber t
      , lineTokens     = fmap (fmap castTT) (lineTokens t)
        -- taking advantage of the fact that a 2-tuple instantiates 'Data.Functor.Functor' over the
        -- second item.
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
instance Show tok =>
  Show (Error st tok) where
    show err =
      let msg = concat $ map (fromMaybe "") $
            [ fmap (("(on token "++) . (++")") . show) (parseErrTok err)
            , fmap ((": "++) . uchars) (parseErrMsg err)
            ]
      in  if null msg then "Unknown parser error" else msg
instance TokenType tok =>
  CFG (Error st) tok where
    castTT t =
      Error
      { parseErrLoc = parseErrLoc t
      , parseErrMsg = parseErrMsg t
      , parseErrTok = fmap castTT (parseErrTok t)
      , parseStateAtErr = parseStateAtErr t
      }

-- | An initial blank parser error you can use to construct more detailed error messages.
parserErr :: Eq tok => LineNum -> ColumnNum -> Error st tok
parserErr lineNum colNum =
  Error
  { parseErrLoc = Just $
      Location
      { startingLine   = lineNum
      , startingColumn = colNum
      , endingLine     = lineNum
      , endingColumn   = colNum
      }
  , parseErrMsg = Nothing
  , parseErrTok = Nothing
  , parseStateAtErr = Nothing
  }

----------------------------------------------------------------------------------------------------
-- $Lexer_builder
-- When defining a computer language, one essential step will be to define your keywords and
-- operators, and define tokens for these keywords and operators. Since the 'MonadParser's defined
-- in this module are polymorphic over token types, you could do this yourself with an ordinary
-- Haskell data structure deriving the 'Prelude.Eq', 'Data.Ix.Ix', 'Prelude.Show', and
-- 'Prelude.Read' instantiations with the deriving keyword. You could then use this data type to
-- represent all possible tokens in your language.
--
-- However, it might be more convenient if there was a way to simply declare to your program "here
-- are my keywords, here are my operators, here is how you lex comments, here is how you lex white
-- spaces", stated simply using Haskell functions, and then let the token types be derived from
-- these declarations. The functions in this section intend to provide you with this ability.

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
newtype TT = MkTT{ intTT :: Int } deriving (Eq, Ord, Show, Ix)

-- Used internally to the 'LexBuilderState', particularly the 'labeledLexers' field.
-- data LexBuilderUnit tt
--   = KeyStrRef { builderUnitKey :: tt }
--   | RegexRef  { builderUnitKey :: tt, builderUnitRegex :: Regex }
-- instance Functor LexBuilderUnit where
--   fmap f a = case a of { KeyStrRef tt -> KeyStrRef (f tt); RegexRef tt r -> RegexRef (f tt) r; }
-- isKeyStrRef :: LexBuilderUnit tt -> Bool
-- isKeyStrRef a = case a of { KeyStrRef _ -> True; _ -> False; }

-- | Example:
-- > myTokens :: 'LexBuilder'
-- > myTokens = do
-- >     let key = 'stringTable' . 'Prelude.unwords'
-- >     key "if then else case of let in where"
-- >     key "() == /= -> \\ : :: ~ @"
-- >     lexer "string.literal"  'lexStringLiteral'
-- >     lexer "comment.endline" 'lexEndlineC_Comment'
-- >     lexer "comment.inline"  'lexInlineC_Comment'
-- >     -- (The dots in the token type name do not mean anything, it just looks nicer.)
data LexBuilderState tok
  = LexBuilderState
    { regexItemCounter :: Int
    , labeledLexers    :: M.Map UStr tok
      -- ^ contains all simple lexers which do not establish loops. This would be any lexer that
      -- takes a keyword or operator, or a lexer constructed from a 'Regex' (which might or might
      -- not loop) but produces only one kind of token.
    , buildingLexer    :: Regex
    }
newtype LexBuilder tok a = LexBuilder{ runLexBuilder :: State (LexBuilderState tok) a }
instance (Ix tok, TokenType tok) =>
  Monad (LexBuilder tok) where
    return = LexBuilder . return
    (LexBuilder a) >>= b = LexBuilder (a >>= runLexBuilder . b)
initLexBuilder = LexBuilderState 1 mempty (const RxBacktrack)

-- not for export
newTokID :: TokenType tok => State (LexBuilderState tok) tok
newTokID = do
  i <- gets regexItemCounter
  modify (\st -> st{regexItemCounter = i+1})
  return (wrapTT (MkTT i))

-- | The data type constructed from the 'LexBuilder' monad, used to build a 'Lexer' for your
-- programming language, and also can be used to define the 'Prelude.Show' instance for your token
-- type using 'deriveShowFromTokenDB'.
data TokenDB tok =
  TokenDB
  { tableTTtoUStr :: Array TT UStr
  , tableUStrToTT :: M.Map UStr tok
  , tokenDBLexer  :: Lexer tok ()
  }

deriveShowFromTokenDB :: TokenType tok => TokenDB tok -> tok -> String
deriveShowFromTokenDB tokenDB tok = uchars (tableTTtoUStr tokenDB ! unwrapTT tok)

getTTfromUStr :: TokenType tok => TokenDB tok -> UStr -> Maybe tok
getTTfromUStr tokenDB = flip M.lookup (tableUStrToTT tokenDB)

makeTokenDB :: (Eq tok, TokenType tok) => LexBuilder tok a -> TokenDB tok
makeTokenDB builder =
  TokenDB
  { tableTTtoUStr = array (MkTT 1, MkTT (regexItemCounter st)) $
      fmap swap $ fmap (fmap unwrapTT) $ M.assocs tabmap
  , tokenDBLexer  = void $ many $ regexToLexer $ buildingLexer st
  , tableUStrToTT = tabmap
  }
  where
    tabmap = labeledLexers st
    st = execState (runLexBuilder builder) $
      LexBuilderState{regexItemCounter=1, labeledLexers=mempty, buildingLexer=mempty}

-- | Creates a single keyword or operator token, with the name of the token being the string that is
-- lexed-out of the input. Its like 'stringTable' but creates only a single token which stores no
-- data and returns the newly created token value.
ustrToken :: TokenType tok => UStr -> LexBuilder tok tok
ustrToken u = LexBuilder $ do
  tok <- fmap (M.lookup u) (gets labeledLexers)
  case tok of
    Just tok -> return tok
    Nothing  -> do
      tok <- newTokID
      modify $ \st ->
        st{ labeledLexers = M.insert u tok (labeledLexers st)
          , buildingLexer = buildingLexer st <> rx u . rxEmptyToken tok
          }
      return tok

-- | Like 'ustrToken', except takes a 'Prelude.String' input.
stringToken :: TokenType tok => String -> LexBuilder tok tok
stringToken = ustrToken . ustr

-- | Creates a 'TokenTable' using a list of keywords or operators you provide to it.
-- Every string provided becomes it's own token type. For example:
-- > myKeywords = 'tokenTable' $ 'Data.List.words' $ 'Data.List.unwords' $
-- >     [ "data newtype class instance"
-- >     , "if then else case of let in where"
-- >     , "import module qualified as hiding"
-- >     ]
stringTable :: TokenType tok => [String] -> LexBuilder tok ()
stringTable = ustrTable . map ustr

-- | Like 'stringTable' except takes a list of 'Dao.String.UStr's.
ustrTable :: TokenType tok => [UStr] -> LexBuilder tok ()
ustrTable = mapM_ ustrToken . reverse . sort

-- | Create a token type that is defined by a 'Lexer' instead of a keyword or operator string.
-- The lexer must be labeled so it can be uniquely identified, and also for producing more
-- desriptive error messages.
regexToken :: TokenType tok => String -> Regex -> LexBuilder tok tok
regexToken label re = LexBuilder $ do
  tok <- newTokID
  modify $ \st ->
    st{ labeledLexers = M.insert (ustr label) tok (labeledLexers st)
      , buildingLexer = buildingLexer st <> re . rxToken tok
      }
  return tok

-- | Create a new token type without assigning any regular expression to it. The type ID is
-- registered but not used until you explicitly make use of the token type within a regex with a
-- function like 'rxToken' or 'rxEmptyToken'.
newTokenType :: TokenType tok => String -> LexBuilder tok tok
newTokenType t = LexBuilder newTokID

-- | Simply append a 'Regex' to the 'LexBuilder', it will function within the larger lexer but it
-- won't be assigned to any token type. The use of this function implies that you have created a
-- token type for it using 'newTokenType' or 'regexToken' and will use that type within this lexer
-- via the 'rxToken' or 'rxEmptyToken' functions.
regex :: Regex -> LexBuilder tok ()
regex re = LexBuilder (modify $ \st -> st{buildingLexer = buildingLexer st <> re})

-- | Here is class that allows you to create your own token type from a Haskell newtype. It is
-- usually a good idea do this to keep parsers isolated from one another. It should be considered a
-- best practice to do this for every language. For example, if you have a Python parser, make a new
-- type for Python tokens:
-- > newtype Python = Python 'TT' deriving 'Data.Ix.Ix'
-- > instance TokenType Python where { 'wrapTT' = Python }
-- Only two lines of code, so there is no excuse not to do it. It /MUST/ derive 'Data.Ix.Ix'.
--
-- The reason for this is to prevent confusing tokens produced by different tokenizers for different
-- languages. For example: if you have a large project that compiles two different
-- languages, say Python and Ruby, into the same Abstract Syntax Tree, you don't want both parsers
-- using 'TT' as their token types because someone might accidentally feed 'TT' tokens from the Ruby
-- tokenizer into the Python parser or vice-versa. Using a wrapper type lets you catch this error at
-- compile time.
-- 
-- Now your parsers may peacfully coexist, even in the same module:
-- > parsePython :: 'Prelude.String' -> 'Parser' () Python MySyntaxTree
-- > parsePython = 'parse' myPythonGrammar mempty
-- > parseRuby   :: 'Prelude.String' -> 'Parser' () Ruby MySyntaxTree
-- > parseRuby   = 'parse' myRubyGrammar   mempty
class Ix a => TokenType a where { wrapTT :: TT -> a; unwrapTT :: a -> TT; }
instance TokenType TT where { wrapTT = id; unwrapTT = id; }

-- | The class of Context Free Grammars ('CFG'). A 'CFG' is defined by its functions of lexical
-- analysis and syntactic analysis. Central to these functions are the type of token which defines
-- the language. So nearly every data type in this program is polymorphic over the token type.  Many
-- of these functions begin as tokenizers or lexers over the generic token type 'TT', so there needs
-- to be a way of converting these functions to ones over a polymorphic type.
-- 
-- This class provides a function 'castTT' function to allow any data type that operates on a
-- generic 'TT' token stream to be converted to the correct token type (a type which must be an
-- instance of 'TokenType'). There is no way to convert back from this token type to 'TT', as I
-- currently see no reason to allow the same parser to have access to two diffent token types.
class TokenType tok =>
  CFG p tok where { castTT :: p TT -> p tok }

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
type Regex = RegexUnit -> RegexUnit

-- | This is an intermediate type for constructing 'Regex's. You will not be using it directly. You
-- will instead use any function that evaluates to a 'Regex', which is a function of this data type.
data RegexUnit
  = RxBacktrack
  | RxSuccess
  | RxChoice   { getSubRegexes :: [RegexUnit] }
  | RxStep     { rxStepUnit    :: RxPrimitive , subRegex :: RegexUnit }
  | RxExpect   { rxErrMsg      :: UStr        , subRegex :: RegexUnit }
  | RxDontMatch{ subRegex      :: RegexUnit }
  | RxMakeToken{ rxMakeToken   :: TT, rxKeepContent :: Bool, subRegex :: RegexUnit }
  deriving Eq
instance Monoid RegexUnit where
  mempty      = RxBacktrack
  mappend a b = case a of
    RxBacktrack    -> b
    RxSuccess      -> RxSuccess
    RxChoice    [] -> b
    RxChoice    ax -> case b of
      RxBacktrack    -> RxChoice ax
      RxSuccess      -> RxChoice (cut $ ax++[RxSuccess])
      RxChoice    bx -> RxChoice (cut $ ax++bx)
      b              -> foldr mappend b ax
      where
        cut ax = case ax of
          []                -> []
          RxSuccess:ax      -> [RxSuccess]
          RxExpect b bx : _ -> [RxExpect b bx]
          a:ax              -> a : cut ax
    RxStep    a ax      -> case b of
      RxBacktrack       -> RxStep a ax
      RxSuccess         -> RxChoice [RxStep a ax, RxSuccess]
      RxChoice      []  -> RxStep a ax
      RxChoice   (b:bx) -> case mappend (RxStep a ax) b of
        RxChoice      ax  -> RxChoice (ax++bx)
        b                 -> RxChoice (b:bx)
      RxStep      b bx  -> case (a, b) of
        (a, b) | a==b -> RxStep a (mappend ax bx)
        (a, b)        -> case (a, b) of
          (RxCharSet a, RxCharSet b) -> RxStep (RxCharSet $ Es.union a b) (mappend ax bx)
          (RxString aa, RxString bb) -> RxChoice $ -- sort strings from longest to shortest
            (if aa<bb then (++[RxStep a ax]) else ([RxStep a ax]++)) [RxStep b bx]
          _                          -> RxChoice [RxStep a ax, RxStep b bx]
      b                 -> RxChoice [RxStep a ax, b]
    RxExpect  err ax -> RxExpect err ax
    RxDontMatch       a -> case b of
      RxDontMatch       b -> RxDontMatch (mappend a b)
      b                   -> RxChoice [b, RxDontMatch a]
    RxMakeToken tt keep ax -> case b of
      RxMakeToken _  _    bx -> RxMakeToken tt keep (mappend ax bx)
      bx                     -> RxMakeToken tt keep (mappend ax bx)

showRegex :: TokenDB tok -> Regex -> String
showRegex tokDB re = evalState (loop (re RxSuccess)) ("", mempty) where
  loop re = case re of
    RxBacktrack   -> return "backtrack"
    RxSuccess     -> return "success"
    RxChoice   re -> do
      (indent, _) <- get
      withState (\ (indent, vars) -> ("  "++indent, vars)) $
        mapM loop re >>= return . foldl (\str next -> indent++str++"\n"++next) ""
    RxStep            r re -> loop re >>= return . ((showRegexPrim r ++ " ")++)
    RxExpect          e re -> loop re >>= return . (("must(else fail "++show e++") do-> ")++)
    RxDontMatch         re -> loop re >>= return . (\s -> "not("++s++")")
    RxMakeToken tt keep re -> do
      vars <- fmap snd get
      let cons = if keep then "token" else "emptok"
      case M.lookup tt vars of
        Nothing -> do
          let str = tableTTtoUStr tokDB ! tt
          modify (\ (indent, vars) -> (indent, M.insert tt str vars))
          after <- loop re
          return (cons++' ':uchars str++' ':after)
        Just str -> return (cons++' ':uchars str++" ...")

-- | Convert a 'Regex' function to a 'Lexer'. The resulting 'Lexer' will not call 'makeToken' or
-- 'makeEmptyToken', it will only match the beginning of the input string according to the 'Regex',
-- leaving the matched characters in the 'lexBuffer'.
regexToLexer :: (Eq tok, TokenType tok) => Regex -> Lexer tok ()
regexToLexer re = loop (re RxSuccess) where
  loop re = case re of
    RxBacktrack            -> lexBacktrack
    RxSuccess              -> return ()
    RxMakeToken tt keep re -> (if keep then makeToken else makeEmptyToken) (wrapTT tt) >> loop re
    RxChoice            re -> msum $ map loop re where
    RxStep         r    re -> do
      keep <- gets lexBuffer
      clearBuffer
      continue <- msum $
        [ regexPrimToLexer r >> modify (\st -> st{lexBuffer = keep ++ lexBuffer st}) >> return True
        , do  modify (\st -> st{ lexBuffer = keep, lexInput = lexBuffer st ++ lexInput st})
              return False
        ]
      if continue then loop re else mzero
    RxExpect       err  re -> mplus (loop re) (fail (uchars err))
    RxDontMatch         re -> case re of
      RxDontMatch re -> loop re -- double negative means succeed on match
      re             -> do
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
  = RxString   { rxString  :: UStr }
  | RxCharSet  { rxCharSet :: Es.Set Char }
  | RxRepeat   { rxLowerLim :: Es.Inf Int, rxUpperLim :: Es.Inf Int, subRegexUnit :: RxPrimitive }
  deriving Eq

showRegexPrim :: RxPrimitive -> String
showRegexPrim re = case re of
  RxString       str -> show str
  RxCharSet      ch  -> show ch
  RxRepeat lo hi re  -> "repeat("++show lo++".."++show hi++", "++showRegexPrim re++")"

regexPrimToLexer :: Eq tok => RxPrimitive -> Lexer tok ()
regexPrimToLexer re = case re of
  RxString  str     -> lexString (uchars str)
  RxCharSet set     -> lexCharP (Es.member set)
  RxRepeat lo hi re -> rept lo hi re
  where
    rept lo hi re = fromMaybe (seq "internal error" $! return ()) $ do
      getLo <- mplus (Es.toPoint lo >>= return . lowerLim re) (return (return ()))
      getHi <- mplus (Es.toPoint hi >>= return . upperLim re) (return (noLimit re))
      return (getLo >> getHi)
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
      RxCharSet set       -> lexWhile (Es.member set)
      RxRepeat lo' hi' re -> replicateM_ hi (rept lo' hi' re)
    noLimit re = case re of
      RxString  str       -> loop (lexString (uchars str))
      RxCharSet set       -> mplus (lexWhile (Es.member set)) (return ())
      RxRepeat lo  hi  re -> loop (rept lo hi re)
    loop lex = do
      continue <- mplus (lex >> return True) (return False)
      if continue then loop lex else return ()

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

-- | Marks a point in a 'Regex' sequence where the matching must not fail, and if it does fail, the
-- resulting 'Lexer' to which this 'Regex' evaluates will evaluate to 'Control.Monad.fail' with an
-- error message provided as a paramater to this function. For example:
-- > decimalPoint = digits . 'rx' '.' . 'cantFail' "must have digits after a decimal point" . digits
cantFail :: String -> Regex
cantFail msg = RxExpect (ustr msg)

-- | This is a look-ahead 'Regex' that matches if the 'Regex' parameter provided does not match. An
-- extremely inefficient function, you should avoid using it and consider re-designing your
-- 'LexBuilder' if you rely on this function too often. This function is designed to occur only at
-- the end of your 'Regex', that is, every 'Regex' that occurs after 'doNot' is part of the 'Regex'
-- to not match. For example:
-- > myRegex = 'rx' "else" . spaces . 'doNot' . 'rx' "if" . spaces . rx "end"
-- will succeed only if the string else is not followed by a string @"if end"@. There is no way to
-- make the 'Regex' first check if the next string is not @"if"@ and if it is not then continue
-- matching with @spaces@ and @'rx' "end"@ after that.
doNot :: Regex
doNot = RxDontMatch

-- | Create a token, keep the portion of the string that has matched the regex up to this point, but
-- clear the match buffer once the token has been created.
rxToken :: TokenType tt => tt -> Regex
rxToken tt = RxMakeToken (unwrapTT tt) True

-- | Create a token, disgarding the portion of the string that has matched the regex up to this point.
rxEmptyToken :: TokenType tt => tt -> Regex
rxEmptyToken tt = RxMakeToken (unwrapTT tt) False

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
instance TokenType tok =>
  CFG LexerState tok where
    castTT t =
      LexerState
      { lexTabWidth      = lexTabWidth t
      , lexCurrentLine   = lexCurrentLine t
      , lexCurrentColumn = lexCurrentColumn t
      , lexTokenCounter  = lexTokenCounter t
      , tokenStream      = fmap castTT (tokenStream t)
      , lexBuffer        = lexBuffer t
      , lexInput         = lexInput t
      }

-- | Create a new lexer state using the given input 'Prelude.String'. This is only realy useful if
-- you must evaluate 'runLexerState'.
newLexerState :: Eq tok => String -> LexerState tok
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
-- > 'Prelude.Eq' tok => 'Error' ('LexerState' tok)
-- However the 'TokStream's evaluate to 'Error's containing type:
-- > 'Prelude.Eq' tok => 'Error' ('TokStreamState' st tok)
-- This function provides an easy way to convert between the two 'Error' types, however since
-- the state value @st@ is polymorphic, you will need to insert your parser state into the error
-- value after evaluating this function. For example:
-- > case tokenizerResult of
-- >    'Dao.Predicate.PFail' lexErr -> 'Dao.Predicate.PFail' (('lexErrToParseErr' lexErr){'parseStateAtErr' = Nothing})
-- >    ....
lexErrToParseErr
  :: Eq tok
  => Error (LexerState tok) tok
  -> Error (TokStreamState st tok) tok
lexErrToParseErr lexErr =
  lexErr
  { parseStateAtErr = Nothing
  , parseErrLoc = st >>= \st -> return (atPoint (lexCurrentLine st) (lexCurrentColumn st))
  }
  where { st = parseStateAtErr lexErr }


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
    runLexer :: PTrans (Error (LexerState tok) tok) (State (LexerState tok)) a
  }
instance Eq tok =>
  Functor (Lexer tok) where { fmap fn (Lexer lex) = Lexer (fmap fn lex) }
instance Eq tok =>
  Monad (Lexer tok) where
    (Lexer fn) >>= mfn          = Lexer (fn >>= runLexer . mfn)
    return                         = Lexer . return
    fail msg                       = do
      st <- get
      throwError $
        (parserErr (lexCurrentLine st) (lexCurrentColumn st)){parseErrMsg = Just (ustr msg)}
instance Eq tok =>
  MonadPlus (Lexer tok) where
    mplus (Lexer a) (Lexer b) = Lexer (mplus a b)
    mzero                           = Lexer mzero
instance Eq tok =>
  Applicative (Lexer tok) where { pure = return; (<*>) = ap; }
instance Eq tok =>
  Alternative (Lexer tok) where { empty = mzero; (<|>) = mplus; }
instance Eq tok =>
  MonadState (LexerState tok) (Lexer tok) where
    get = Lexer (lift get)
    put = Lexer . lift . put
instance Eq tok =>
  MonadError (Error (LexerState tok) tok) (Lexer tok) where
    throwError                        = Lexer . throwError
    catchError (Lexer try) catcher = Lexer (catchError try (runLexer . catcher))
instance Eq tok =>
  MonadPlusError (Error (LexerState tok) tok) (Lexer tok) where
    catchPValue (Lexer try) = Lexer (catchPValue try)
    assumePValue               = Lexer . assumePValue
instance (Eq tok, Monoid a) =>
  Monoid (Lexer tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }

-- | Append the first string parameter to the 'lexBuffer', and set the 'lexInput' to the value of
-- the second string parameter. Most lexers simply takes the input, breaks it, then places the two
-- halves back into the 'LexerState', which is what this function does. *Be careful* you don't pass
-- the wrong string as the second parameter. Or better yet, don't use this function.
lexSetState :: Eq tok => String -> String -> Lexer tok ()
lexSetState got remainder = modify $ \st ->
  st{lexBuffer = lexBuffer st ++ got, lexInput = remainder}

-- | Unlike simply evaluating 'Control.Monad.mzero', 'lexBacktrack' will push the contents of the
-- 'lexBuffer' back onto the 'lexInput'. This is inefficient, so if you rely on this too often you
-- should probably re-think the design of your lexer.
lexBacktrack :: Eq tok => Lexer tok ig
lexBacktrack = modify (\st -> st{lexBuffer = "", lexInput = lexBuffer st ++ lexInput st}) >> mzero

-- | Single character look-ahead, never consumes any tokens, never backtracks unless we are at the
-- end of input.
lexLook1 :: Eq tok => Lexer tok Char
lexLook1 = gets lexInput >>= \input -> case input of { "" -> mzero ; c:_ -> return c }

-- | Arbitrary look-ahead, creates a and returns copy of the portion of the input string that
-- matches the predicate. This function never backtracks, and it might be quite inefficient because
-- it must force strict evaluation of all characters that match the predicate.
lexCopyWhile :: Eq tok => (Char -> Bool) -> Lexer tok String
lexCopyWhile predicate = fmap (takeWhile predicate) (gets lexInput)

-- | A fundamental 'Lexer', uses 'Data.List.break' to break-off characters from the input string
-- until the given predicate evaluates to 'Prelude.True'. Backtracks if no characters are lexed.
-- See also: 'charSet' and 'unionCharP'.
lexWhile :: Eq tok => (Char -> Bool) -> Lexer tok ()
lexWhile predicate = do
  (got, remainder) <- fmap (span predicate) (gets lexInput)
  if null got then mzero else lexSetState got remainder

-- | Like 'lexUnit' but inverts the predicate, lexing until the predicate does not match. This
-- function is defined as:
-- > \predicate -> 'lexUntil' ('Prelude.not' . predicate)
-- See also: 'charSet' and 'unionCharP'.
lexUntil :: Eq tok => (Char -> Bool) -> Lexer tok ()
lexUntil predicate = lexWhile (not . predicate)

-- lexer: update line/column with string
lexUpdLineColWithStr :: Eq tok => String -> Lexer tok ()
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
      (newLine, newCol) = countNLs (lexCurrentLine st) (lexCurrentColumn st) input
  put (st{lexCurrentLine=newLine, lexCurrentColumn=newCol})

-- | Create a 'Token' using the contents of the 'lexBuffer', then clear the 'lexBuffer'. This
-- function backtracks if the 'lexBuffer' is empty. If you pass "Prelude.False' as the first
-- parameter the tokens in the 'lexBuffer' are not stored with the token, the token will only
-- contain the type.
makeGetToken  :: Eq tok => Bool -> tok -> Lexer tok (Token tok)
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
            { tokenAtLineNumber   = lexCurrentLine   st
            , tokenAtColumnNumber = lexCurrentColumn st
            , getToken            = token
            } ]
      , lexTokenCounter = lexTokenCounter st + 1
      }
  lexUpdLineColWithStr str
  return token

-- | Create a token in the stream without returning it (you usually don't need the token anyway). If
-- you do need the token, use 'makeGetToken'.
makeToken :: Eq tok => tok -> Lexer tok ()
makeToken = void . makeGetToken True

-- | Create a token in the stream without returning it (you usually don't need the token anyway). If
-- you do need the token, use 'makeGetToken'. The token created will not store any characters, only
-- the type of the token. This can save a lot of memory, but it requires you have very descriptive
-- token types.
makeEmptyToken :: Eq tok => tok -> Lexer tok ()
makeEmptyToken = void . makeGetToken False

-- | Clear the 'lexBuffer' without creating a token.
clearBuffer :: Eq tok => Lexer tok ()
clearBuffer = get >>= \st -> lexUpdLineColWithStr (lexBuffer st) >> put (st{lexBuffer=""})

-- | A fundamental lexer using 'Data.List.stripPrefix' to check whether the given string is at the
-- very beginning of the input.
lexString :: Eq tok => String -> Lexer tok ()
lexString str =
  gets lexInput >>= assumePValue . maybeToBacktrack . stripPrefix str >>= lexSetState str

-- | A fundamental lexer succeeding if the next 'Prelude.Char' in the 'lexInput' matches the
-- given predicate. See also: 'charSet' and 'unionCharP'.
lexCharP ::  Eq tok => (Char -> Bool) -> Lexer tok ()
lexCharP predicate = gets lexInput >>= \input -> case input of
  c:input | predicate c -> lexSetState [c] input
  _                     -> mzero

-- | Succeeds if the next 'Prelude.Char' on the 'lexInput' matches the given 'Prelude.Char'
lexChar :: Eq tok => Char -> Lexer tok ()
lexChar c = lexCharP (==c)

-- | *Not a 'Lexer'* but useful when passed as the first parameter to 'lexCharP', 'lexWhile' or
-- 'lexUntil'. This function creates a predicate over 'Prelude.Chars's that evaluates to
-- 'Prelude.True' if the 'Prelude.Char' is equal to any of the 'Prelude.Char's in the given
-- 'Prelude.String'. This is similar to the behavior of character sets in POSIX regular expressions:
-- the RegexUnit @[abcd]@ matches the same characters as the predicate @('charSet' "abcd")@
charSet :: String -> Char -> Bool
charSet charset c = or $ map (c==) $ nub charset

-- | *'Not a 'Lexer'* but useful when passed as the first parameter to 'lexCharP', 'lexWhile' or
-- 'lexUntil'.This function creates a simple set-union of predicates to create a new predicate on
-- 'Prelude.Char's. The predicate evalautes to 'Prelude.True' if the 'Prelude.Char' applied to any
-- of the predicates evaluate to 'Prelude.True'. This is similar to unions of character ranges in
-- POSIX regular expressions: the RegexUnit @[[:xdigit:]xyzXYZ]@ matches the same characters as the
-- predicate:
-- > ('unionCharP' [isHexDigit, charSet "xyzXYZ"])
unionCharP :: [Char -> Bool] -> Char -> Bool
unionCharP px c = or (map ($c) px)

-- | Unions the 'Data.Char.isSymbol' and 'Data.Char.isPunctuation' predicates.
isSymPunct :: Char -> Bool
isSymPunct c = isSymbol c || isPunctuation c

-- | This is a character predicate I find very useful and I believe it should be included in the
-- standard Haskell "Data.Char" module, it is succeeds for alpha-numeric or underscore character.
isAlphaNum_ :: Char -> Bool
isAlphaNum_ c = isAlphaNum c || c=='_'

-- | This is a character predicate I find very useful and I believe it should be included in the
-- standard Haskell "Data.Char" module, it is succeeds for alpha-numeric or underscore character.
isAlpha_ :: Char -> Bool
isAlpha_ c = isAlpha c || c=='_'

lexOptional :: Eq tok => Lexer tok () -> Lexer tok ()
lexOptional lexer = mplus lexer (return ())

-- | Backtracks if there are still characters in the input.
lexEOF :: Eq tok => Lexer tok ()
lexEOF = fmap (=="") (gets lexInput) >>= guard

-- | Create a 'Lexer' that will continue scanning until it sees an unescaped terminating
-- sequence. You must provide three lexers: the scanning lexer, the escape sequence 'Lexer' and
-- the terminating sequence 'Lexer'. Evaluates to 'Prelude.True' if the termChar was found,
-- returns 'Prelude.False' if this tokenizer went to the end of the input without seenig an
-- un-escaped terminating character.
lexUntilTerm
  :: Eq tok
  => Lexer tok () -> Lexer tok () -> Lexer tok () -> Lexer tok Bool
lexUntilTerm scanLexer escLexer termLexer = loop where
  skipOne = lexCharP (const True)
  loop = do
    (shouldContinue, wasTerminated) <- msum $
      [ lexEOF    >> return (False, False)
      , termLexer >> return (False, True )
      , escLexer  >>
          mplus (lexEOF                              >> return (False, False))
                (msum [termLexer, escLexer, skipOne] >> return (True , False))
      , scanLexer >> return (True , False)
      , skipOne   >> return (True , False)
      ]
    if shouldContinue then loop else return wasTerminated

-- | A special case of 'lexUntilTerm', lexes until it finds an un-escaped terminating
-- 'Prelude.Char'. You must only provide the escape 'Prelude.Char' and the terminating
-- 'Prelude.Char'.
lexUntilTermChar :: Eq tok => Char -> Char -> Lexer tok Bool
lexUntilTermChar escChar termChar =
  lexUntilTerm (lexUntil (\c -> c==escChar || c==termChar)) (lexChar escChar) (lexChar termChar)

-- | A special case of 'lexUntilTerm', lexes until finds an un-escaped terminating 'Prelude.String'.
-- You must provide only the escpae 'Prelude.String' and the terminating 'Prelude.String'. You can
-- pass a null string for either escape or terminating strings (passing null for both evaluates to
-- an always-backtracking lexer). The most escape and terminating strings are analyzed and the most
-- efficient method of lexing is decided, so this lexer is guaranteed to be as efficient as
-- possible.
lexUntilTermStr :: Eq tok => String -> String -> Lexer tok Bool
lexUntilTermStr escStr termStr = case (escStr, termStr) of
  (""    , ""     ) -> mzero
  (""    , termStr) -> hasOnlyTerm termStr
  (escStr, ""     ) -> hasOnlyTerm escStr
  _                 -> do
    let e = head escStr
        t = head termStr
        predicate = if e==t then (==t) else (\c -> c==e || c==t)
    lexUntilTerm (lexUntil predicate) (lexString escStr) (lexString termStr)
  where
    hasOnlyTerm str = do
      let (scan, term) = case str of
            [a]          -> (lexUntil (==a), lexChar a)
            [a,b] | a/=b -> (lexUntil (==a), lexWhile (==a) >> lexChar b)
            a:ax         ->
              (lexUntil (==a), let loop = mplus (lexString str) (lexChar a >> loop) in loop)
      lexUntilTerm scan term term

-- | Takes three parameters. (1) A label for this set of 'Lexer's used in error reporting.
-- (2) A halting predicate which backtracks if there is more tokenizing to be done, and succeeds
-- when this tokenizer is done. (3) A list of 'Lexer's that will each be tried in turn. This
-- function loops, first trying the halting predicate, and then trying the 'Lexer' list, and
-- looping continues until the halting predicate succeeds or fails if the 'Lexer' list
-- backtracks. If the 'Lexer' list backtracks, the error message is
-- > "unknown characters scanned by $LABEL tokenizer"
-- where @$LABEL@ is the string passed as parameter 1.
runLexerLoop
  :: Eq tok
  => String -> Lexer tok () -> [Lexer tok ()] -> Lexer tok ()
runLexerLoop msg predicate lexers = loop 0 where
  loop i = do
    s <- gets lexInput
    shouldContinue <- msum $
      [ predicate >> return False
      , msum lexers >> return True
      , fail ("unknown characters scanned by "++msg++" tokenizer")
      ]
    seq shouldContinue $! if shouldContinue then loop (i+1) else return ()

----------------------------------------------------------------------------------------------------
-- $Handy_lexers

-- | The fundamental lexer: takes a predicate over characters, if one or more characters
-- matches, a token is constructed and it is paired with the remaining string and wrapped into a
-- 'Data.Maybe.Just' value. Otherwise 'Data.Maybe.Nothing' is returned. The 'Data.Maybe.Maybe' type
-- is used so you can combine fundamental tokenizers using 'Control.Monad.mplus'.
lexSimple :: Eq tok => tok -> (Char -> Bool) -> Lexer tok ()
lexSimple tok predicate = lexWhile predicate >> makeToken tok

-- | A fundamental lexer using 'Data.Char.isSpace' and evaluating to a 'Space' token.
lexSpace :: Eq tok => tok -> Lexer tok ()
lexSpace tok = lexSimple tok isSpace

-- | A fundamental lexer using 'Data.Char.isAlpha' and evaluating to a 'Alphabetic' token.
lexAlpha :: Eq tok => tok -> Lexer tok ()
lexAlpha tok = lexSimple tok isAlpha

-- | A fundamental lexer using 'Data.Char.isDigit' and evaluating to a 'Digits' token.
lexDigits :: Eq tok => tok -> Lexer tok ()
lexDigits tok = lexSimple tok isDigit

-- | A fundamental lexer using 'Data.Char.isHexDigit' and evaluating to a 'HexDigit' token.
lexHexDigits :: Eq tok => tok -> Lexer tok ()
lexHexDigits tok = lexSimple tok isHexDigit

-- | Constructs an operator 'Lexer' from a string of operators separated by spaces. For example,
-- pass @"+ += - -= * *= ** / /= % %= = == ! !="@ to create 'Lexer' that will properly parse all of
-- those operators. The order of the operators is *NOT* important, repeat symbols are tried only
-- once, the characters @+=@ are guaranteed to be parsed as a single operator @["+="]@ and not as
-- @["+", "="]@. *No token is created,* you must create your token using 'makeToken' or
-- 'makeEmptyToken' immediately after evaluating this tokenizer.
lexOperator :: Eq tok => String -> Lexer tok ()
lexOperator ops =
  msum (map (\op -> lexString op) $ reverse $ nub $ sortBy len $ words ops)
  where
    len a b = case compare (length a) (length b) of
      EQ -> compare a b
      GT -> GT
      LT -> LT

-- | Gather up all the characters until a newline character is reached.
lexToEndline :: Eq tok => Lexer tok ()
lexToEndline = lexUntil (=='\n')

lexInlineComment :: Eq tok => tok -> String -> String -> Lexer tok ()
lexInlineComment tok startStr endStr = do
  lexString startStr
  completed <- lexUntilTermStr "" endStr
  if completed
    then  makeToken tok
    else  fail "comment runs past end of input"

lexInlineC_Comment :: Eq tok => tok -> Lexer tok ()
lexInlineC_Comment tok = lexInlineComment tok "/*" "*/"

lexEndlineC_Comment :: Eq tok => tok -> Lexer tok ()
lexEndlineC_Comment tok = lexString "//" >> lexUntil (=='\n') >> makeToken tok

lexInlineHaskellComment :: Eq tok => tok -> Lexer tok ()
lexInlineHaskellComment tok = lexInlineComment tok "{-" "-}"

lexEndlineHaskellComment :: Eq tok => tok -> Lexer tok ()
lexEndlineHaskellComment tok = lexString "--" >> lexToEndline >> makeToken tok

-- | A lot of programming languages provide only end-line comments beginning with a (@#@) character.
lexEndlineCommentHash :: Eq tok => tok -> Lexer tok ()
lexEndlineCommentHash tok = lexChar '#' >> lexToEndline >> makeToken tok

lexStringLiteral :: Eq tok => tok -> Lexer tok ()
lexStringLiteral tok = do
  lexChar '"'
  completed <- lexUntilTermChar '\\' '"'
  if completed
    then  makeToken tok
    else  fail "string literal expression runs past end of input"

lexCharLiteral :: Eq tok => tok -> Lexer tok ()
lexCharLiteral tok = lexChar '\'' >> lexUntilTermChar '\\' '\'' >> makeToken tok

-- | This actually tokenizes a general label: alpha-numeric and underscore characters starting with
-- an alphabetic or underscore character. This is useful for several programming languages.
-- Evaluates to a 'Keyword' token type, it is up to the 'TokStream's in the syntacticAnalysis phase
-- to sort out which 'Keyword's are actually keywords and which are labels for things like variable
-- names.
lexKeyword :: Eq tok => tok -> Lexer tok ()
lexKeyword tok = do
  lexWhile (\c -> isAlpha c || c=='_')
  lexOptional (lexWhile (\c -> isAlphaNum c || c=='_'))
  makeToken tok

-- | Create a 'Lexer' that lexes the various forms of numbers. If the number contains no special
-- modification, i.e. no hexadecimal digits, no decimal points, and no exponents, a 'Digits' token
-- is returned. Anything more exotic than simple base-10 digits and a 'Number' token is returned. If
-- the 'Number' is expressed in base-10 and also has an exponent, like @6.022e23@ where @e23@ is the
-- exponent or @1.0e-10@ where @e-10@ is the exponent, then 'NumberExp' is returned.
-- 
-- Hexadecimal and binary number expressions are also tokenized, the characters @'x'@, @'X'@, @'b'@,
-- and @'B'@ are all prefixes to a string of hexadecimal digits (tokenized with
-- 'Data.Char.isHexDigit'). So the following expression are all parsed as 'Number' tokens:
-- > 0xO123456789ABCDEfabcdef, 0XO123456789ABCDEfabcdef, 0bO123456789, 0BO123456789
-- these could all be valid hexadecimal or binary numbers. Of course @0bO123456789@ is
-- not a valid binary number, but this lexer does not care about the actual value, it is expected
-- that the 'TokStream' report it as an error during the 'syntacticAnalysis' phase. Floating-point
-- decimal numbers are also lexed appropriately, and this includes floating-point numbers expressed
-- in hexadecimal. Again, if your language must disallow hexadecimal floating-point numbers, throw
-- an error in the 'syntacticAnalysis' phase.
lexNumber :: Eq tok => tok -> tok -> tok -> tok -> Lexer tok ()
lexNumber digits hexDigits number numberExp = do
  let altBase typ xb@(u:l:_) pred = do
        lexCharP (charSet xb)
        mplus (lexWhile pred) (fail ("no digits after "++typ++" 0"++l:" token"))
  (getDot, isHex) <- msum $
    [ do  lexChar '0' -- lex a leading zero
          msum $
            [ altBase "hexadecimal" "Xx" isHexDigit >> return (True , True )
            , altBase "binary"      "Bb" isDigit    >> return (True , False)
            , lexWhile isDigit                      >> return (True , False)
            , return (True , False)
              -- a zero not followed by an 'x', 'b', or any other digits is also valid
            ]
    , lexChar '.' >> lexWhile isDigit >> return (False, False) -- lex a leading decimal point
    , lexWhile isDigit                >> return (True , False) -- lex an ordinary number
    , lexBacktrack
    ]
  (gotDot, gotExp) <- flip mplus (return (False, False)) $ do
    gotDot <-
      if getDot -- we do not have the dot?
        then  flip mplus (return False) $ do
                lexChar '.'
                mplus (lexWhile (if isHex then isHexDigit else isDigit))
                      (fail "no digits after decimal point")
                return True
        else  return True -- we already had the dot
    if isHex
      then  return (gotDot, False) -- don't look for an exponent
      else  flip mplus (return (gotDot, False)) $ do
              lexCharP (charSet "Ee")
              lexOptional (lexCharP (charSet "-+"))
              mplus (lexWhile isDigit)
                    (fail "no digits after exponent mark in decimal-point number")
              return (gotDot, True )
  makeToken $
    if gotDot || gotExp
      then  if gotExp then numberExp else number
      else  if isHex  then hexDigits else digits

-- | Creates a 'Label' token for haskell data type names, type names, class names, or constructors,
-- i.e. one or more labels (alpha-numeric and underscore characters) separated by dots (with no
-- spaces between the dots) where every first label is a capital alphabetic character, and the final
-- label may start with a lower-case letter or underscore, or the final label may also be
-- punctuation surrounded by parens. Examples are: @Aaa.Bbb@, @D.Ee_ee.ccc123@, @Aaa.Bbb.Ccc.___d1@,
-- @A.B.C.(-->)@.
lexHaskellLabel :: Eq tok => tok -> Lexer tok ()
lexHaskellLabel tok = loop 0 where
  label    = do
    lexCharP (\c -> isUpper c && isAlpha c)
    lexOptional (lexWhile isAlphaNum_)
  loop   i = mplus (label >> mplus (tryDot i) done) (if i>0 then final else mzero)
  tryDot i = lexChar '.' >> loop (i+1)
  final    = mplus (lexCharP isAlpha_ >> lexOptional (lexWhile isAlphaNum_)) oper >> done
  oper     = do lexChar '('
                mplus (lexWhile (\c -> c/=')' && isSymPunct c) >> lexChar ')')
                      (fail "bad operator token after final dot of label")
  done     = makeToken tok

-- | Takes a 'tokenStream' resulting from the evaulation of lexical analysis and breaks it into
-- 'Line's. This makes things a bit more efficient because it is not necessary to store a line
-- number with every single token. It is necessary for initializing a 'TokStream'.
tokenStreamToLines :: Eq tok => [TokenAt tok] -> [Line tok]
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
  :: Eq tok
  => Lexer tok a
  -> LexerState tok
  -> (PValue (Error (LexerState tok) tok) a, LexerState tok)
lexicalAnalysis lexer st = runState (runPTrans (runLexer lexer)) st

testLexicalAnalysis_withFilePath
  :: (Eq tok, Show tok)
  => Lexer tok () -> FilePath -> TabWidth -> String -> IO ()
testLexicalAnalysis_withFilePath tokenizer filepath tablen input = putStrLn report where
  (result, st) = lexicalAnalysis tokenizer ((newLexerState input){lexTabWidth=tablen})
  lines  = tokenStreamToLines (tokenStream st)
  more   = take 21 (lexInput st)
  remain = "\nremaining: "++(if length more > 20 then show (take 20 more)++"..." else show more)
  loc    = show (lexCurrentLine st) ++ ":" ++ show (lexCurrentColumn st)
  report = (++remain) $ intercalate "\n" (map showLine lines) ++ '\n' : case result of
    OK      _ -> "Success!"
    Backtrack -> reportFilepath ++ ": lexical analysis evalauted to \"Backtrack\""
    PFail err -> reportFilepath ++ show err
  showLine line = concat $
    [ "----------\nline "
    , show (lineNumber line), "\n"
    , intercalate ", " $ map showTok (lineTokens line)
    ]
  showTok (col, tok) = concat [show col, " ", show (tokType tok), " ", show (tokToUStr tok)]
  reportFilepath = (if null filepath then "" else filepath)++":"++loc

-- | Run the 'lexicalAnalysis' with the 'Lexer' on the given 'Prelude.String' and print out
-- every token created.
testLexicalAnalysis
  :: (Eq tok, Show tok)
  => Lexer tok () -> TabWidth -> String -> IO ()
testLexicalAnalysis a b c = testLexicalAnalysis_withFilePath a "" b c

-- | Run the 'lexicalAnalysis' with the 'Lexer' on the contents of the file at the the given
-- 'System.IO.FilePath' 'Prelude.String' and print out every token created.
testLexicalAnalysisOnFile
  :: (Eq tok, Show tok)
  => Lexer tok () -> TabWidth -> FilePath -> IO ()
testLexicalAnalysisOnFile a b c = readFile c >>= testLexicalAnalysis_withFilePath a c b

----------------------------------------------------------------------------------------------------
-- $The_parser_class
-- This module provides the 'TokStream' and 'Parser' monads, both of which can be used to
-- construct parsers, and so there is a set of functions which are common to both of these monads.
-- You can also extend either of these monads with your own data type and instantiate this class to
-- make use of the same functions.
-- 
-- Your might parser your own custom parsers that behave slightly differently than the fundamental
-- 'TokStream' parser. For example, a 'TokStream' parser doesn't care about the kind of tokens you
-- use, so the 'token' function might simply take the token, compares it to the token in the stream,
-- and then decides whether or not to shift it off the stream and use it, or put it back and fail.
-- On the other hand, the 'Parser' monad requires your tokens to instantiate the 'Data.Ix.Ix'
-- class so that your tokens can be used to build efficient lookup tables. The instantiation of the
-- 'token' function for the 'Parser' monad works very differently (more efficiently) under the
-- hood as compared to the ordinary 'TokStream' monad which does not build lookup tables.

-- | This class declares a set of functions that are expected to be common to all parsers dervied
-- from the parsers in this module. *The minimal complete definition is:*
-- > 'guardEOF'
-- > 'shiftPos'
-- > 'unshift'
-- but these default definitions alone would be very inefficient. Hopefully your parser will
-- instantiate this class with a more efficient 'MonadParser' such as one that extends 'TokStream'
-- or better yet 'Parser'.
class (Eq tok, Monad (parser tok), MonadPlus (parser tok)) =>
  MonadParser parser tok where
    -- | A 'Control.Monad.guard'-like function that backtracks if there are still tokens in the
    -- token stream, or succeeds if there are no tokens left.
    guardEOF :: Eq tok => parser tok ()
    -- | Put a token and it's positional information into the front of the token stream, so the very
    -- next call to 'shiftPos' or 'look1Pos' will retrieve the information you passed to this
    -- function. This is used to implement backtracking in a function like 'tokenP'. Use of this
    -- function is an anti-pattern, meaning if you find yourself using this function often, say
    -- *more than once* in your entire module (and that one usage is not within the instantiation of
    -- 'tokenP'), you have probably done a terrible job of designing your parser.
    unshift :: (LineNum, ColumnNum, Token tok) -> parser tok ()
    -- | Shift the token off of the token stream along with it's positional information and succeed.
    -- Backtrack if there are no more tokens.
    shiftPos :: parser tok (LineNum, ColumnNum, Token tok)
    -- | Look-ahead 1 token, i.e. copy the token off of the token stream without shifting it out of
    -- the stream. The token is copied along with it's positional information. Succeed if there was
    -- a token that could be copied, backtrack if the token stream is empty.
    look1Pos :: parser tok (LineNum, ColumnNum, Token tok)
    look1Pos = shiftPos >>= \t -> unshift t >> return t
    -- | Like 'tokenP' but is only concerned with the type of the token. The 'Parser'
    -- instantiation of this function uses the token type as an array index and stores the
    -- sub-parser at that index. The 'TokStream' instantiation of this function is merely a special
    -- case of 'tokenP'.
    token :: tok -> parser tok a -> parser tok a
    token tok parser = do
      kept@(_, _, nxt) <- shiftPos
      mplus (if tok == tokType nxt then parser else mzero) (unshift kept >> mzero)
    -- | Like 'tokenP' but is only concerned with the string-value of the token. The 'Parser'
    -- instantiation of this function uses the token string as a 'Data.Map.Map' key and stores the
    -- sub-parser at that key. The default instantiation of this function is merely a special
    -- case of 'tokenP'.
    tokenUStr :: UStr -> parser tok a -> parser tok a
    tokenUStr u parser = do
      kept@(_, _, nxt) <- shiftPos
      mplus (if u == tokToUStr nxt then parser else mzero) (unshift kept >> mzero)
    -- | Exactly like 'tokenUStr' but uses a 'Prelude.String' as the input to be checked.
    tokenStr :: String -> parser tok a -> parser tok a
    tokenStr s = tokenUStr (ustr s)
    -- | Like 'tokenPosP' but does not care about the 'LineNum' or 'ColumnNum'.
    tokenP :: Token tok -> parser tok a -> parser tok a
    tokenP tok parser = do
      kept@(_, _, nxt) <- shiftPos
      mplus (if tokType tok == tokType nxt && tokToUStr tok == tokToUStr nxt then parser else mzero)
            (unshift kept >> mzero)
    -- | A token predicate, the behavior of this function is to be a predicate on the next token in
    -- the token stream and shift the token off of the stream and use it to evaluate the predicate.
    -- If the predicate evaluates successfully, the sub-parser provided as the second parameter is
    -- evaluated. If either the sub-parser or the predicate backtrack, the token is placed back onto
    -- the token stream. Your own instance does not need to do this exact same thing, but it is less
    -- confusing if you do it this way.
    tokenPosP :: (LineNum, ColumnNum, Token tok) -> parser tok a -> parser tok a
    tokenPosP (_, _, tok) parser = tokenP tok parser

-- | Like 'shiftPos' but conveniently removes the 'LineNum' and 'ColumnNum', since usually you
-- don't need that information.
shift :: MonadParser parser tok => parser tok (Token tok)
shift = shiftPos >>= \ (_, _, tok) -> return tok

-- | Like 'look1Pos' but conveniently removes the 'LineNum' and 'ColumnNum', since usually you
-- don't need that information.
look1 :: MonadParser parser tok => parser tok (Token tok)
look1 = look1Pos >>= \ (_, _, tok) -> return tok

-- | Return the current line and column of the current token without modifying shifting the token
-- stream.
getCursor :: MonadParser parser tok => parser tok (LineNum, ColumnNum)
getCursor = look1Pos >>= \ (a,b, _) -> return (a,b)

-- | If the given 'Parser' backtracks then evaluate to @return ()@, otherwise ignore the result of
-- the 'Parser' and evaluate to @return ()@.
ignore :: MonadParser parser tok => parser tok ig -> parser tok ()
ignore parser = mplus (parser >> return ()) (return ()) 

-- | Return the default value provided in the case that the given 'TokStream' fails, otherwise
-- return the value returned by the 'TokStream'.
defaultTo :: MonadParser parser tok => a -> parser tok a -> parser tok a
defaultTo defaultValue parser = mplus parser (return defaultValue)

----------------------------------------------------------------------------------------------------
-- $Fundamental_parser_data_types
-- A parser is defined as a stateful monad for analyzing a stream of tokens. A token stream is
-- represented by a list of 'Line' structures, and the parser monad's jobs is to look at the
-- current line, and extract the current token in the current line in the state, and use the tokens
-- to construct data. 'TokStream' is the fundamental parser, but it might be very tedious to use. It
-- is better to construct parsers using 'Parser' which is a higher-level, easier to use data type
-- that is converted into the lower-level 'TokStream' type.

-- | The 'TokStreamState' contains a stream of all tokens created by the 'lexicalAnalysis' phase.
-- This is the state associated with a 'TokStream' in the instantiation of 'Control.Mimport
-- Debug.Traceonad.State.MonadState', so 'Control.Monad.State.get' returns a value of this data
-- type.
data TokStreamState st tok
  = TokStreamState
    { userState   :: st
    , getLines    :: [Line tok]
    , recentTokens :: [(LineNum, ColumnNum, Token tok)]
      -- ^ single look-ahead is common, but the next token exists within the 'Prelude.snd' value
      -- within a pair within a list within the 'lineTokens' field of a 'Line' data structure.
      -- Rather than traverse that same path every time 'nextToken' or 'withToken' is called, the
      -- next token is cached here.
    }

newParserState :: Eq tok => ust -> [Line tok] -> TokStreamState ust tok
newParserState userState lines =
  TokStreamState{userState = userState, getLines = lines, recentTokens = []}

-- | The 'TokStreamState' data structure has a field of a polymorphic type reserved for containing
-- arbitrary stateful information. 'TokStream' instantiates 'Control.Monad.State.Class.MonadState'
-- usch that 'Control.Monad.State.Class.MonadState.get' and
-- 'Control.Monad.State.Class.MonadState.put' return the 'TokStreamState' type, however if you wish
-- to modify the arbitrary state value using a function similar to how the
-- 'Control.Monad.State.Class.MonadState.modify' would do, you can use this function.
modifyUserState :: Eq tok => (st -> st) -> TokStream st tok ()
modifyUserState fn = modify (\st -> st{userState = fn (userState st)})

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
newtype TokStream ust tok a
  = TokStream{
      parserToPTrans ::
        PTrans (Error (TokStreamState ust tok) tok) (State (TokStreamState ust tok)) a
    }
instance Eq tok =>
  Functor   (TokStream st tok) where { fmap f (TokStream a) = TokStream (fmap f a) }
instance Eq tok =>
  Monad (TokStream ust tok) where
    (TokStream ma) >>= mfa = TokStream (ma >>= parserToPTrans . mfa)
    return a               = TokStream (return a)
    fail msg = do
      ab  <- optional getCursor
      tok <- optional look1
      st  <- get
      throwError $
        Error
        { parseErrLoc = fmap (uncurry atPoint) ab
        , parseErrMsg = Just (ustr msg)
        , parseErrTok = tok
        , parseStateAtErr = Just st
        }
instance Eq tok =>
  MonadPlus (TokStream ust tok) where
    mzero                             = TokStream mzero
    mplus (TokStream a) (TokStream b) = TokStream (mplus a b)
instance Eq tok =>
  Applicative (TokStream ust tok) where { pure = return ; (<*>) = ap; }
instance Eq tok =>
  Alternative (TokStream ust tok) where { empty = mzero; (<|>) = mplus; }
instance Eq tok =>
  MonadState (TokStreamState ust tok) (TokStream ust tok) where
    get = TokStream (PTrans (fmap OK get))
    put = TokStream . PTrans . fmap OK . put
instance Eq tok =>
  MonadError (Error (TokStreamState ust tok) tok) (TokStream ust tok) where
    throwError err = do
      st <- get
      assumePValue (PFail (err{parseStateAtErr=Just st}))
    catchError (TokStream ptrans) catcher = TokStream $ do
      pval <- catchPValue ptrans
      case pval of
        OK      a -> return a
        Backtrack -> mzero
        PFail err -> parserToPTrans (catcher err)
instance Eq tok =>
  MonadPlusError (Error (TokStreamState ust tok) tok) (TokStream ust tok) where
    catchPValue (TokStream ptrans) = TokStream (catchPValue ptrans)
    assumePValue                   = TokStream . assumePValue
instance (Eq tok, Monoid a) =>
  Monoid (TokStream ust tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }
instance Eq tok =>
  MonadParser (TokStream ust) tok where
    guardEOF = mplus (look1Pos >> return False) (return True) >>= guard
    unshift postok = modify (\st -> st{recentTokens = postok : recentTokens st})
    shiftPos = nextTokenPos True
    look1Pos = nextTokenPos False

-- | Return the next token in the state along with it's line and column position. If the boolean
-- parameter is true, the current token will also be removed from the state.
nextTokenPos :: Eq tok => Bool -> TokStream st tok (LineNum, ColumnNum, Token tok)
nextTokenPos doRemove = do
  st <- get
  case recentTokens st of
    [] -> case getLines st of
      []         -> mzero
      line:lines -> case lineTokens line of
        []                 -> put (st{getLines=lines}) >> nextTokenPos doRemove
        (colNum, tok):toks -> do
          let postok = (lineNumber line, colNum, tok)
              upd st = st{ getLines = line{ lineTokens = toks } : lines }
          if doRemove -- the 'recentTokens' buffer is cleared here regardless.
            then  put (upd st)
            else  put ((upd st){ recentTokens = [postok] })
          return postok
    tok:tokx | doRemove -> put (st{recentTokens=tokx}) >> return tok
    tok:tokx            -> return tok
      -- If we remove a token, the 'recentTokens' cache must be cleared because we don't know what
      -- the next token will be. I use 'mzero' to clear the cache, it has nothing to do with the
      -- parser backtracking.

-- | A 'marker' immediately stores the cursor onto the stack. It then evaluates the given 'Parser'.
-- If the given 'Parser' fails, the position of the failure (stored in a 'Dao.Token.Location') is
-- updated such that the starting point of the failure points to the cursor stored on the stack by
-- this 'marker'. When used correctly, this function makes error reporting a bit more helpful.
marker :: MonadParser (TokStream ust) tok => TokStream ust tok a -> TokStream ust tok a
marker parser = do
  ab <- mplus (fmap Just getCursor) (return Nothing)
  flip mapPFail parser $ \parsErr ->
    parsErr
    { parseErrLoc =
        let p = parseErrLoc parsErr
        in  mplus (p >>= \loc -> ab >>= \ (a, b) -> return (mappend loc (atPoint a b))) p
    }

-- | Given two parameters: 1. an error message and 2. a 'Parser', will succeed normally if
-- evaluating the given 'Parser' succeeds. But if the given 'Parser' backtracks, this this function
-- will evaluate to a 'Parser' failure with the given error message. If the given 'Parser' fails,
-- it's error message is used instead of the error message given to this function. The string
-- "expecting " is automatically prepended to the given error message so it is a good idea for your
-- error message to simple state what you were expecting, like "a string" or "an integer". I
-- typically write 'expect' statements like so:
-- > fracWithExp = do
-- >     fractionalPart <- parseFractional
-- >     'tokenP' 'Alphabetic' (\tok -> tok=="E" || tok=="e")
-- >     'expect' "an integer expression after the 'e'" $ do
-- >         exponentPart <- parseSignedInteger
-- >         return (makeFracWithExp fractionalPart exponentPart :: 'Prelude.Double')
expect :: MonadParser (TokStream ust) tok => String -> TokStream ust tok a -> TokStream ust tok a
expect errMsg parser = do
  (a, b) <- getCursor
  let expectMsg = "expecting "++errMsg
  mplus parser (throwError ((parserErr a b){parseErrMsg = Just (ustr expectMsg)}))

-- | The 'TokStream's analogue of 'Control.Monad.State.runState', runs the parser using an existing
-- 'TokStreamState'.
runParserState
  :: MonadParser (TokStream ust) tok
  => TokStream ust tok a
  -> TokStreamState ust tok
  -> (PValue (Error (TokStreamState ust tok) tok) a, TokStreamState ust tok)
runParserState (TokStream parser) = runState (runPTrans parser)

-- | This is the second phase of parsing, takes a stream of tokens created by the 'lexicalAnalysis'
-- phase (the @['Line' tok]@ parameter) and constructs a syntax tree data structure using the
-- 'TokStream' monad provided.
syntacticAnalysis
  :: MonadParser (TokStream ust) tok
  => TokStream ust tok synTree
  -> ust
  -> [Line tok]
  -> (PValue (Error (TokStreamState ust tok) tok) synTree, TokStreamState ust tok)
syntacticAnalysis parser userState lines = runParserState parser $ newParserState userState lines

----------------------------------------------------------------------------------------------------
-- $ParseTable
-- A parse table is not something you should construct by hand in your own code. The 'ParseTable' is
-- really an intermediate data structure between the very high-level 'Parser' and the rather
-- low-level 'TokStream'.
--
-- At the 'TokStream' level, you must always worry about the tokens in the stream, whether or not it
-- is necessary to 'shift' the next token or 'unshift' it if you have backtracked, or if it is
-- necessary to fail with an error message, making sure you don't 'unshift' the same token twice,
-- but also making sure you don't forget to 'unshift' a token after backtracking. Creating parsers
-- can become very tedius and error-prone.
-- 
-- At the 'Parser' level, you don't concern yourself with the token stream, you only worry about
-- what token type and string value you need in order to construct your data type, and you can trust
-- that token stream will be shifted and checked accordingly without you ever having to actually
-- call 'shift' or 'unshift'. But how is this possible? All parsers that operate on token streams
-- need some mechanism to determine when to 'shift' or 'unshift' a token, right?
-- 
-- That is where 'ParseTable' comes in. The 'Parser' is actually a meta-parser that does not
-- operate on the token stream directly. Instead, the 'Parser' monad is used to construct a large
-- object that can be converted to a 'ParseTable' with the 'evalGenParserToParseTable' function. The
-- parse table object contains a sparse matrix that maps tokens to state transitions. The matrix is
-- constructed of variously sized 'Data.Array.IArray.Array's with the token type value used as and
-- index (hence the polymorphic token type @tok@ must instantiate 'Data.Ix.Ix').
-- 
-- The 'ParseTable' can then be evaluated to a 'TokStream' monad which does all the tedius work of
-- keeping track of the tokens in the stream. However generating the 'ParseTable' with
-- 'evalGenParserToParseTable' is not a trivial operation, the mappings between token indecies and
-- 'TokStream' combinators must be computed and arrays must be allocated. So it is better to hang on
-- to your 'ParseTable' throughout the duration of your parsing task. As long as you have a
-- reference to the same 'ParseTable' constructed by your one call to the
-- 'evalGenParserToParseTable' function, neither the 'ParseTable' nor the arrays within it be
-- garbage collected.

-- | This data type instantiates 'Control.Monad.Monad', 'Control.Applicative.Alternative', and
-- others, but you really should not compose your own parse tables using these functions. Define
-- your parser using 'Parser' and let 'evalGenParserToParseTable' compose the 'ParseTable' for
-- you.
data ParseTable st tok a
  = ParseTableArray { parseTableArray :: Array tok (ParseTable st tok a) }
    -- ^ stores references to 'ParseTable' functions into an array for fast retrieval by the type of
    -- the current token.
  | ParseTableMap { parserMap :: M.Map UStr (ParseTable st tok a) }
  | ParseTable { tokStreamParser :: TokStream st tok a }
    -- ^ this constructor stores a plain 'TokStream' function, so this constructor can be used to
    -- lift 'TokStream' functions into the 'ParseTable' monad. This constructor is not composable
    -- like the above two constructors are, so the 'evalGenParserToParseTable' function tries to
    -- avoid using this unless it is absolutely necessary.
instance Ix tok => Monad (ParseTable st tok) where
  return a = ParseTable { tokStreamParser = return a }
  fail     = ParseTable . fail
  parser >>= bindTo =
    ParseTable{
      tokStreamParser = evalTableToTokStream parser >>= \a -> evalTableToTokStream (bindTo a)
    }
instance Ix tok => Functor (ParseTable ust tok) where
  fmap f parser = case parser of
    ParseTableArray     arr        ->
      ParseTableArray { parseTableArray = amap (\origFunc -> fmap f origFunc) arr }
    ParseTable      parser ->
      ParseTable{ tokStreamParser = fmap f parser}
instance Ix tok =>
  MonadPlus (ParseTable ust tok) where
    mzero     = ParseTable{ tokStreamParser = mzero }
    mplus a b = case a of
      ParseTableArray arrA -> case b of
        ParseTableArray arrB -> merge (assocs arrA ++ assocs arrB)
        ParseTable      fb   -> ParseTable{tokStreamParser = mplus (evalParseArray arrA) fb}
      ParseTable      fa   -> ParseTable $ mplus fa $ case b of
        ParseTableArray arrB -> evalParseArray arrB
        ParseTable      fb   -> fb
      where
        minmax a = foldl (\ (n, x) a -> (min n a, max x a)) (a, a) . map fst
        merge ax = case ax of
          []                 -> mzero
          ax@((tok, func):_) -> ParseTableArray{
              parseTableArray = accumArray mplus mzero (minmax tok ax) ax
            }
instance Ix tok =>
  Applicative (ParseTable ust tok) where { pure = return; (<*>) = ap; }
instance Ix tok =>
  Alternative (ParseTable ust tok) where { empty = mzero; (<|>) = mplus; }
instance (Ix tok, Monoid a) =>
  Monoid (ParseTable ust tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }
instance Ix tok =>
  MonadState ust (ParseTable ust tok) where
    get = ParseTable (gets userState)
    put st = ParseTable $ modify $ \parserState -> parserState{userState=st}
instance Ix tok =>
  MonadError (Error (TokStreamState ust tok) tok) (ParseTable ust tok) where
    throwError = ParseTable . throwError
    catchError trial catcher = ParseTable $
      catchError (evalTableToTokStream trial) (\err -> evalTableToTokStream (catcher err))
instance Ix tok =>
  MonadPlusError (Error (TokStreamState ust tok) tok) (ParseTable ust tok) where
    catchPValue ptrans = ParseTable (catchPValue (evalTableToTokStream ptrans))
    assumePValue       = ParseTable . assumePValue
instance Ix tok =>
  MonadParser (ParseTable ust) tok where
    guardEOF = ParseTable guardEOF
    unshift  = ParseTable . unshift
    shiftPos = ParseTable shiftPos
    look1Pos = ParseTable look1Pos

-- | Evaluate a 'ParseTable' to a 'TokStream'.
evalTableToTokStream :: Ix tok => ParseTable st tok a -> TokStream st tok a
evalTableToTokStream table = case table of
  ParseTable      parser -> parser
  ParseTableArray parser -> evalParseArray parser
  ParseTableMap   parser -> evalParseMap   parser

-- | Efficiently evaluates an array stored in the 'ParseTableArray' constructor. Evaluation will
-- shift on token from the stream and the 'tokType' is used to select the next 'ParseTable' stored
-- in the array at that 'tokType' address. If the 'tokType' is not a valid index of the
-- 'Data.Array.IArray.Array', or if the selected 'ParseTable' backtracks when evaluated, this parser
-- backtracks and the selecting token is shifted back onto the stream.
evalParseArray :: Ix tok => Array tok (ParseTable st tok a) -> TokStream st tok a
evalParseArray arr = do
  tok <- fmap tokType look1
  if inRange (bounds arr) tok then evalTableToTokStream (arr!tok) else mzero

-- | Efficiently evaluates a 'Data.Map.Map' stored in a 'ParseTableMap' constructor. Evaluation will
-- shift one token from the stream, and the 'tokToUStr' value is used as a key to select and
-- evaluate the 'ParseTable' stored in the 'Data.Map.Map'. If the 'tokToUStr' value is not a key in
-- the 'Data.Map.Map', or if the selected 'ParseTable' backtracks when evaluated, this parser
-- backtracks and the selecting token is shifted back onto the stream.
evalParseMap :: Ix tok => M.Map UStr (ParseTable st tok a) -> TokStream st tok a
evalParseMap m = do
  tok <- fmap tokToUStr look1
  join $ fmap evalTableToTokStream $ assumePValue $ maybeToBacktrack $ M.lookup tok m

----------------------------------------------------------------------------------------------------
-- $State_transition_parser
-- This data type is a high-level representation of parsers. To understand how it differs from
-- a 'ParseTable', please read the section above.
--
-- A 'Parser' can be used to build arbitrarily complex Abstract Syntax Trees (ASTs), and the
-- 'evalGenParserToParseTable' function will do its best to find the most efficient 'ParseTable'
-- representation of the parser for any given AST.
-- 
-- Here is a quick example of how to build your own parser using 'Parser':
-- > data TOKEN = NUMBER | PLUS | MINUS | TIMES | OPENPAREN | CLOSEPAREN deriving Ix
-- > data AST = Value Int | Add AST AST | Sub AST AST | Mult AST AST | Parens AST
-- > 
-- > -- The tokenizer, associates a 'Lexer' with every TOKEN.
-- > myTokenizer :: Lexer TOKEN ()
-- > myTokenizer = 'Control.Applicative.many' $ 'Control.Monad.msum' $
-- >     [ 'lexUntil' 'Data.Char.isNumber' >> 'makeToken' NUMBER
-- >     , 'lexChar' @'@(@'@ >> 'makeEmptyToken' OPENPAREN
-- >     , 'lexChar' @'@)@'@ >> 'makeEmptyToken' CLOSEPAREN
-- >     , 'lexChar' @'@+@'@ >> 'makeEmptyToken' PLUS
-- >     , 'lexChar' @'@-@'@ >> 'makeEmptyToken' MINUS
-- >     , 'lexChar' @'@*@'@ >> 'makeEmptyToken' TIMES
-- >     , 'lexWhile' 'Data.Char.isSpace' >> 'clearBuffer' -- ignore spaces
-- >     , 'Control.Monad.fail' "unknown character"
-- >     ]
-- > 
-- > -- The Plus, Minus, and Times constructors in the AST data type all have the same parser, just
-- > -- with different token types and constructors.
-- > operator :: TOKEN -> (AST -> AST -> AST) -> 'Parser' () TOKEN AST -> 'Parser' () TOKEN AST
-- > operator tokTyp constructor parse = 'Control.Applicatie.pure' constructor 'Control.Applicative.<*>' (parse >>= 'token' tokTyp . 'Control.Monad.return') 'Control.Applicative.<*>) parse
-- > 
-- > -- Parse an expression in parentheses.
-- > parens :: 'Parser' () TOKEN AST
-- > parens = token OPENPARN >> mainParser >>= token CLOSEPAREN . return
-- > 
-- > -- Multiplication has a higher prescedence than addition or subtraction, so we make a separate table for multiplication
-- > mult :: 'Parser' () TOKEN AST
-- > mult = operator TIMES Mult (parens 'Control.Applicative.<|>' mult)
-- > 
-- > -- Addition and subtraction have the same prescedence, they call the above "mult" function which has a higher prescedence.
-- > add :: 'Parser' () TOKEN AST
-- > add = let getHigherPrec = mult 'Control.Applicative.<|>' add
-- >       in  operator PLUS Add getHigherPrec 'Control.Applicative.<|>' operator MINUS Sub getHigherPrec
-- > 
-- > mainParser :: 'CFGrammar' () TOKEN [AST]
-- > mainParser = 'newCFGrammar' 4 myTokenizer $
-- >     'Control.Applicative.many' $ 'Control.Monad.msum' $
-- >         [ 'Control.Applicatie.pure' Value 'Control.Applicative.<*>' ('token' NUMBER $ 'fromString' 'Prelude.read')
-- >         , parens, mult, add
-- >         ]
-- > 
-- > evalAST :: AST -> Int
-- > evalAST a = case a of
-- >     Value i   -> i
-- >     Plus  a b -> evalAST a + evalAST b
-- >     Minus a b -> evalAST a - evalAST b
-- >     Times a b -> evalAST a * evalAST b
-- >     Parens i  -> evalAST i
-- >
-- > main = getContents >>= \inputString -> case 'parse' mainParser () inputString of
-- >     'Dao.Predicate.OK' ast    -> 'Control.Monad.mapM_' ('System.IO.print' . evalAST) ast
-- >     'Dao.Predicate.Backtrack' -> 'System.IO.hPutStrLn' 'System.IO.stderr' "error: parse backtracked"
-- >     'Dao.Predicate.PFail' err -> 'System.IO.hPrint' 'System.IO.stderr' err

-- | This data type is used to express /State Transition Parsers/. Use this to build
-- your parsers.
data Parser ust tok a
  = ParserBacktrack
  | ParserConst  { constValue  :: a }
  | ParserFail   { failMessage :: String }
  | ParserUpdate { updateInner :: ParseTable ust tok a }
  | ParserTable
    { checkTokenAndString :: M.Map tok  (M.Map UStr (Parser ust tok a))
    , checkToken          :: M.Map tok  (Parser ust tok a)
    , checkString         :: M.Map UStr (Parser ust tok a)
    }
instance Ix tok =>
  Monad (Parser ust tok) where
    return     = ParserConst
    fail       = ParserFail
    p >>= bind = case p of
      ParserBacktrack            -> ParserBacktrack
      ParserConst a              -> bind a
      ParserFail msg             -> ParserFail msg
      ParserUpdate p -> ParserUpdate{
          updateInner = do
            a <- p >>= evalGenParserToParseTable . bind
            ParseTable shift >> return a
        }
      ParserTable tokstr tok str ->
        ParserTable
        { checkTokenAndString = fmap (fmap (>>=bind)) tokstr
        , checkToken          = fmap (>>=bind) tok
        , checkString         = fmap (>>=bind) str
        }
instance Ix tok =>
  Functor (Parser ust tok) where
    fmap f p = case p of
      ParserBacktrack -> ParserBacktrack
      ParserConst  a -> ParserConst (f a)
      ParserFail msg -> ParserFail  msg
      ParserUpdate p -> ParserUpdate (fmap f p)
      ParserTable tokstr tok str ->
        ParserTable
        { checkTokenAndString = fmap (fmap (fmap f)) tokstr
        , checkToken          = fmap (fmap f) tok
        , checkString         = fmap (fmap f) str
        }
instance Ix tok =>
  MonadPlus (Parser ust tok) where
    mzero     = ParserBacktrack
    mplus a b = case a of
      ParserBacktrack -> b
      ParserConst   a -> ParserConst a
      ParserFail  msg -> ParserFail msg
      ParserUpdate  a -> case b of
        ParserBacktrack -> ParserUpdate a
        ParserFail  msg -> ParserUpdate a
        ParserConst   b -> ParserUpdate (mplus a (return b))
        ParserUpdate  b -> ParserUpdate (mplus a b)
        b             -> ParserUpdate (mplus a (evalGenParserToParseTable b))
      ParserTable tokstrA tokA strA -> case b of
        ParserBacktrack -> ParserTable tokstrA tokA strA
        ParserConst   b ->
          ParserTable
          { checkTokenAndString = fmap (fmap (flip mplus (ParserConst b))) tokstrA
          , checkToken          = fmap (flip mplus (ParserConst b)) tokA
          , checkString         = fmap (flip mplus (ParserConst b)) strA
          }
        ParserFail  msg -> ParserTable tokstrA tokA strA
        ParserUpdate  b ->
          ParserTable
          { checkTokenAndString = fmap (fmap (mplus (ParserUpdate b))) tokstrA
          , checkToken          = fmap (mplus (ParserUpdate b)) tokA
          , checkString         = fmap (mplus (ParserUpdate b)) strA
          }
        ParserTable tokstrB tokB strB ->
          ParserTable
          { checkTokenAndString = M.unionWith (M.unionWith mplus) tokstrA tokstrB
          , checkToken          = M.unionWith              mplus  tokA    tokB
          , checkString         = M.unionWith              mplus strA    strB
          }
instance Ix tok => Applicative (Parser ust tok) where { pure = return; (<*>) = ap; }
instance Ix tok => Alternative (Parser ust tok) where { empty = mzero; (<|>) = mplus; }
instance (Ix tok, Monoid a) =>
  Monoid (Parser ust tok a) where { mempty = return mempty; mappend a b = liftM2 mappend a b; }
instance Ix tok =>
  MonadState ust (Parser ust tok) where
    get = ParserUpdate get
    put = ParserUpdate . put
instance Ix tok =>
  MonadError (Error (TokStreamState ust tok) tok) (Parser ust tok) where
    throwError = ParserUpdate . throwError
    catchError trial catcher = ParserUpdate $
      catchError (evalGenParserToParseTable trial) (\err -> evalGenParserToParseTable (catcher err))
instance Ix tok =>
  MonadPlusError (Error (TokStreamState ust tok) tok) (Parser ust tok) where
    catchPValue ptrans = ParserUpdate (catchPValue (evalGenParserToParseTable ptrans))
    assumePValue       = ParserUpdate . assumePValue
instance Ix tok =>
  MonadParser (Parser ust) tok where
    guardEOF = ParserUpdate guardEOF
    unshift  = ParserUpdate . unshift
    shiftPos = ParserUpdate shiftPos
    look1Pos = ParserUpdate look1Pos
    token     t p = emptyTable{checkToken  = M.singleton t p}
    tokenUStr u p = emptyTable{checkString = M.singleton u p}
    tokenP    tok p = do
      let t = tokType tok
          u = tokToUStr tok
      if u==nil
        then  token t p
        else  emptyTable{checkTokenAndString = M.singleton t (M.singleton u p)}

-- | Allows you to build your own parser table from scratch by directly mapping tokens and strings
-- to 'Parser's using functions provided in "Data.Map".
emptyTable :: Parser ust tok a
emptyTable =
  ParserTable
  { checkTokenAndString = M.empty
  , checkToken          = M.empty
  , checkString         = M.empty
  }

tokenTypeUStr :: Ix tok => tok -> UStr -> Parser ust tok a -> Parser ust tok a
tokenTypeUStr t u = tokenP (Token{tokType=t, tokUStr=u})

tokenTypeStr :: Ix tok => tok -> String -> Parser ust tok a -> Parser ust tok a
tokenTypeStr t s = tokenTypeUStr t (ustr s)

-- | Convert a 'Parser' to a 'ParseTable'. Doing this will lazily construct a sparse matrix which
-- becomes the state transition table for this parser, hence the token type must instantiate
-- 'Data.Ix.Ix'. Try to keep the resulting 'ParseTable' in scope for as long as there is a
-- possibility that you will use it. Every time this function is evaluated, a new set of
-- 'Data.Array.IArray.Array's are constructed to build the sparse matrix.
evalGenParserToParseTable :: Ix tok => Parser st tok a -> ParseTable st tok a
evalGenParserToParseTable p = case p of
  ParserBacktrack -> mzero
  ParserConst   a -> return a
  ParserFail  msg -> fail msg
  ParserUpdate fn -> fn
  ParserTable tokstr tok str -> msum $ concat $
    [ mkMapArray tokstr
    , mkArray tok
    , mkmap str
    ]
  where
    findBounds tok = foldl (\ (min0, max0) (tok, _) -> (min min0 tok, max max0 tok)) (tok, tok)
    mkMapArray :: Ix tok => M.Map tok (M.Map UStr (Parser st tok a)) -> [ParseTable st tok a]
    mkMapArray m = do
      let ax = M.assocs m
      case ax of
        []          -> mzero
        [(tok, m)]  -> return $ do
          t <- fmap tokType (ParseTable look1)
          guard (t==tok)
          ParseTableMap{parserMap = M.map evalGenParserToParseTable m}
        (tok, _):ax' -> do
          let minmax = findBounds tok ax'
              bx     = concatMap (\ (tok, par) -> map ((,)tok) (mkmap par)) ax
          return (ParseTableArray{parseTableArray = accumArray (\_ a -> a) mzero minmax bx})
    mkArray :: Ix tok => M.Map tok (Parser st tok a) -> [ParseTable st tok a]
    mkArray m = do
      let ax = M.assocs m
      case ax of
        []            -> mzero
        [(tok, gstp)] -> return $ do
          t <- fmap tokType (ParseTable look1)
          guard (t==tok)
          evalGenParserToParseTable gstp
        (tok, _):ax'  -> do
          let minmax = findBounds tok ax'
              bx = map (\ (tok, par) -> (tok, evalGenParserToParseTable par)) ax
          return (ParseTableArray{parseTableArray = accumArray (\_ a -> a) mzero minmax bx})
    mkmap :: Ix tok => M.Map UStr (Parser st tok a) -> [ParseTable st tok a]
    mkmap m = case M.assocs m of
      []            -> mzero
      [(str, gstp)] -> return $ do
        t <- fmap tokToUStr (ParseTable look1)
        guard (t==str)
        evalGenParserToParseTable gstp
      _             -> return (ParseTableMap{parserMap = M.map evalGenParserToParseTable m})

-- | Your 'Parser' will needs to convert 'Token's to other data types. This function takes a
-- pure function that performs a conversion from a single 'Prelude.String', and uses the string
-- value of the current function to evaluate it.
fromString :: Ix tok => (String -> a) -> Parser st tok a
fromString f = pure (f . tokToStr) <*> look1

-- | Your 'Parser' will needs to convert 'Token's to other data types. This function takes a
-- pure function that performs a conversion from a single 'Dao.String.UStr', and uses the string
-- value of the current function to evaluate it.
fromUStr :: Ix tok => (UStr -> a) -> Parser st tok a
fromUStr f = pure (f . tokToUStr) <*> look1

----------------------------------------------------------------------------------------------------

-- | A /Context Free Grammar/ is a data structure that allows you to easily define a
-- two-phase parser (a parser with a 'lexicalAnalysis' phase, and a 'syntacticAnalysis' phase). The
-- fields supplied to this data type define the grammar, and the 'parse' function can be used to
-- parse an input string using the context-free grammar defined in this data structure. *Note* that
-- the parser might have two phases, but because Haskell is a lazy language and 'parse' is a pure
-- function, both phases happen at the same time, so the resulting parser does not need to parse the
-- entire input in the first phase before beginning the second phase.
-- 
-- This data type can be constructed from a 'Parser' in such a way that the resulting
-- 'ParseTable' is stored in this object permenantly. It might then be possible to reduce
-- initialization time by using an *INLINE* pragma, which will hopefully cause the compiler to
-- define as much of the 'ParseTable'@'@s sparse matrix as it possibly can at compile time. But this
-- is not a guarantee, of course, you never really know how much an optimization helps until you do
-- proper profiling.
data CFGrammar st tok synTree
  = CFGrammar
    { columnWidthOfTab :: TabWidth
      -- ^ specify how many columns a @'\t'@ character takes up. This number is important to get
      -- accurate line:column information in error messages.
    , mainLexer        :: Lexer tok ()
      -- ^ *the order of these tokenizers is important,* these are the tokenizers passed to the
      -- 'lexicalAnalysis' phase to generate the stream of tokens for the 'syntacticAnalysis' phase.
    , mainParser       :: ParseTable st tok synTree
      -- ^ this is the parser entry-point which is used to evaluate the 'syntacticAnalysis' phase.
    }

-- | Construct a 'CFGrammar' from a 'Parser'. This defines a complete parser that can be used
-- by the 'parse' function. In constructing this 'CFGrammar', the 'Parser' will be converted
-- to a 'ParseTable' which can be referenced directly from this object. This encourages the runtime
-- to cache the 'ParseTable' which can lead to better performance. Using an INLINE pragma on this
-- value could possibly improve performance even further.
newCFGrammar :: Ix tok =>
  TabWidth -> Lexer tok () -> Parser st tok synTree -> CFGrammar st tok synTree
newCFGrammar tabw lexer parser =
  CFGrammar
  { columnWidthOfTab = tabw
  , mainLexer        = lexer
  , mainParser       = evalGenParserToParseTable parser
  }

-- | This is /the function that parses/ an input string according to a given 'CFGrammar'.
parse
  :: Ix tok
  => CFGrammar st tok synTree
  -> st -> String -> PValue (Error st tok) synTree
parse cfg st input = case lexicalResult of
  OK      _ -> case parserResult of
    OK     a  -> OK a
    Backtrack -> Backtrack
    PFail err -> PFail $ err{parseStateAtErr=Just (userState parserState)}
  Backtrack -> Backtrack
  PFail err -> PFail $ (lexErrToParseErr err){parseStateAtErr = Nothing}
  where
    initState = (newLexerState input){lexTabWidth = columnWidthOfTab cfg}
    (lexicalResult, lexicalState) = lexicalAnalysis (mainLexer cfg) initState
    (parserResult , parserState ) =
      syntacticAnalysis (evalTableToTokStream (mainParser cfg)) st $
        tokenStreamToLines (tokenStream lexicalState)

