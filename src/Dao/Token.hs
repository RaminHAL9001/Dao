-- "src/Dao/Token.hs"  Defines the 'Token' and 'Location' types
-- used by "src/Dao/Object.hs" and "src/Dao/Parser.hs".
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

{-# LANGUAGE DeriveDataTypeable #-}

module Dao.Token where

import Dao.String

import           Data.Monoid
import           Data.Word
import           Data.List (intercalate)
import           Data.Typeable
import           Data.Data

import           Control.DeepSeq

----------------------------------------------------------------------------------------------------

type LineNum   = Word
type ColumnNum = Word
type TabWidth  = Word

-- | If an object contains a location, it can instantiate this class to allow locations to be
-- updated or deleted (deleted by converting it to 'LocationUnknown'. Only three types in this
-- module instantiate this class, but any data type that makes up an Abstract Syntax Tree, for
-- example 'Dao.Object.ObjectExpr' or 'Dao.Object.AST.ObjectExrpr' also instantiate this class.
class HasLocation a where
  getLocation :: a -> Location
  setLocation :: a -> Location -> a
  delLocation :: a -> a

instance HasLocation () where
  { getLocation _ = LocationUnknown; setLocation a _ = a; delLocation a = a; }

instance HasLocation UStr where
  { getLocation _ = LocationUnknown; setLocation a _ = a; delLocation a = a; }

instance HasLocation Name where
  { getLocation _ = LocationUnknown; setLocation a _ = a; delLocation a = a; }

instance HasLocation a => HasLocation (Maybe a) where
  getLocation       = maybe LocationUnknown getLocation
  setLocation o loc = fmap (flip setLocation loc) o
  delLocation o     = fmap       delLocation      o

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
instance HasLocation Location where
  getLocation = id
  setLocation = flip const
  delLocation = const LocationUnknown
instance Show Location where
  show t = case t of
    LocationUnknown  -> ""
    Location a b c d -> show a ++ ':' : show b ++
      if a==c && b==d then "" else " to " ++ show c ++ ':' : show d
instance Monoid Location where
  mempty = LocationUnknown
--  Location
--  { startingLine   = 0
--  , startingColumn = 0
--  , endingLine     = 0
--  , endingColumn   = 0
--  }
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
    _ -> compare (abs(ela-sla), abs(eca-sca), sla, sca) (abs(elb-slb), abs(ecb-scb), slb, scb)
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

instance HasNullValue Location where
  nullValue = LocationUnknown
  testNull LocationUnknown = True
  testNull _ = False

instance NFData Location where
  rnf LocationUnknown = ()
  rnf (Location a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

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

-- | Extract the type of the token.
asTokType :: TokenAt tok -> tok
asTokType = tokType . getToken

-- | Extract the string value stored in this token. /WARNING:/ keyword and operator tokens contain
-- no strings to save memory, so evaluating this function on any token type defind by
-- 'Dao.Parser.operator', 'Dao.Parser.operatorTable', or 'Dao.Parser.keyword' will result in a null
-- sring.
asString :: TokenAt tok -> String
asString = tokToStr . getToken

-- | Extract the string value stored in this token. /WARNING:/ keyword and operator tokens contain
-- no strings to save memory, so evaluating this function on any token type defind by
-- 'Dao.Parser.operator', 'Dao.Parser.operatorTable', or 'Dao.Parser.keyword' will result in a null
-- sring.
asUStr :: TokenAt tok -> UStr
asUStr = tokToUStr . getToken

-- | Extract the string value stored in this token. /WARNING:/ keyword and operator tokens contain
-- no strings to save memory, so evaluating this function on any token type defind by
-- 'Dao.Parser.operator', 'Dao.Parser.operatorTable', or 'Dao.Parser.keyword' will result in a null
-- sring.
asName :: TokenAt tok -> Name
asName = fromUStr . asUStr

-- | That is as-zero, because "0" looks kind of like "()".
-- This function is useful when it is necessary to pass a function argument to functions like
-- 'Dao.Parser.token' and 'Dao.Parser.tokenBy' but you want to ignore the token returned.
as0 :: TokenAt tok -> ()
as0 = const ()

-- | Retrieve the token part of a 'TokenAt' object. 
asToken :: TokenAt tok -> Token tok
asToken = getToken

-- | Synonym for 'Prelude.id', used when it is necessary to pass a function argument to functions like
-- 'Dao.Parser.token' and 'Dao.Parser.tokenBy' but you just want the whole 'TokenAt' object.
asTokenAt :: TokenAt tok -> TokenAt tok
asTokenAt = id

-- | Convert the contents of a 'TokenAt' object to a tripple containg it's component parts.
asTriple :: TokenAt tok -> (LineNum, ColumnNum, Token tok)
asTriple tok = (lineNumber tok, columnNumber tok, getToken tok)

-- | Convert the contents of a 'TokenAt' object to a pair containg it's component parts, but not the
-- 'Token' itself.
asLineColumn :: TokenAt tok -> (LineNum, ColumnNum)
asLineColumn tok = (lineNumber tok, columnNumber tok)

-- | Convert the contents of the 'TokenAt' object's 'lineNumber' and 'columnNumber' to a 'Location'
-- object.
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

