-- "Dao/Text/Parser.hs"  provides a parser that uses 'Data.Text.Lazy.Text'
-- internally.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see the URL:
-- <http://www.gnu.org/licenses/agpl.html>.

-- | A parser monad that operates on lazy 'Data.Text.Lazy.Text' input internally.
module Dao.Text.Parser
  ( ParseInput, Parser, runParser, getCharCount, parser_to_S,
    ParserError(ParserError), parserError, parserErrorAt, parserErrorMsg, minPrec,
    ParserState(ParserState), parserState, inputString, userState, textPoint, charCount,
    precedence
  )
  where

import           Dao.Count
import qualified Dao.Interval as Iv
import           Dao.Grammar
import           Dao.Predicate
import           Dao.PPrint
import           Dao.Text
import           Dao.Text.Builder

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Monoid
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text      as Strict
import           Data.Typeable

----------------------------------------------------------------------------------------------------

-- | A convenient type synonym for the lazy 'Data.Text.Lazy.Text' data type.
type ParseInput = LazyText

data ParserError
  = ParserError
    { parserErrorMsg   :: Strict.Text
    , parserErrorAt    :: Maybe TextPoint
    , parserLexerError :: Maybe LexerError
    }
  deriving (Eq, Ord, Typeable)

instance Show ParserError where
  show p = maybe "" show (parserErrorAt p) ++ ": " ++ Strict.unpack (parserErrorMsg p) ++
    maybe "" show (parserLexerError p)

instance PPrintable ParserError where
  pPrint p = return $ pSpan (toText "ParserError") $ concat $ let msg = [pText $ parserErrorMsg p] in
    [ maybe msg (\o -> [pInline $ pPrint o ++ [pText ":", PSpace] ++ msg]) $
        (parserLexerError p >>= lexErrorLocation) <|> parserErrorAt p
    , maybe [] (\o -> [pText ",", PSpace, pIndent $ pPrint o]) (parserLexerError p)
    ]

parserError :: ParserError
parserError = ParserError{ parserErrorMsg=mempty, parserErrorAt=Nothing, parserLexerError=Nothing }

----------------------------------------------------------------------------------------------------

-- | Construct this type with 'parserState' and pass the value to 'runParser'.
data ParserState st
  = ParserState
    { inputString :: LazyText
    , userState   :: st
    , textPoint   :: TextPoint
    , charCount   :: CharCount
    , precedence  :: Int
    }

instance Functor ParserState where
  fmap f st =
    ParserState
    { inputString = inputString st
    , userState   = f $ userState st
    , textPoint   = textPoint st
    , charCount   = charCount st
    , precedence  = precedence st
    }

-- | Define a 'ParserState' that can be used to evaluate a 'Parser' monad. The state value can be
-- anything you choose. 'Parser' instantiates the 'Control.Monad.State.MonadState' class so you can
-- keep track of a pure state for whatever parsing you intend to do.
parserState :: st -> ParserState st
parserState st =
  ParserState
  { inputString = mempty
  , userState   = st
  , textPoint   = TextPoint 1 1
  , charCount   = 0
  , precedence  = 0
  }

----------------------------------------------------------------------------------------------------

-- | A parser that operates on an input string of the lazy 'Data.Text.Lazy.Text' data type.
newtype Parser st a = Parser { _runParse :: PredicateT ParserError (State (ParserState st)) a }

instance Functor (Parser st) where
  fmap f (Parser o) = Parser $ fmap f o

instance Monad (Parser st) where
  return = Parser . return
  (Parser a) >>= b = Parser $ a >>= _runParse . b
  fail msg = getTextPoint >>= \loc ->
    Parser $ throwError $ parserError{ parserErrorAt=Just loc, parserErrorMsg=Strict.pack msg }

instance MonadPlus (Parser st) where
  mzero = Parser mzero
  mplus (Parser a) (Parser b) = Parser $ mplus a b

instance Applicative (Parser st) where { pure = return; (<*>) = ap; }

instance Alternative (Parser st) where { empty = mzero; (<|>) = mplus; }

instance MonadError ParserError (Parser st) where
  throwError = Parser . throwError
  catchError (Parser p) catch = Parser $ catchError p (_runParse . catch)

instance PredicateClass ParserError (Parser st) where
  predicate = Parser . predicate
  returnPredicate (Parser try) = Parser $ try >>= return . OK

instance MonadState st (Parser st) where
  state f = Parser $ lift $ state $ \st ->
    let (a, ust) = f (userState st) in (a, st{ userState=ust })

instance MonadParser Lazy.Text (Parser st) where
  look      = Parser $ lift $ gets inputString
  look1     = look >>= \t -> guard (not $ Lazy.null t) >> return (Lazy.head t)
  get1      = _take $ \t -> guard (not $ Lazy.null t) >> return (Lazy.head t, Lazy.tail t)
  string  s = pure (toLazyText s) >>= \s ->
    _take (\t -> guard (Lazy.isPrefixOf s t) >> return (Lazy.splitAt (Lazy.length s) t))
  munch   f = _take $ \t -> return $ Lazy.span (Iv.member f) t
  munch1  f = Lazy.cons <$> satisfy f <*> munch f
  satisfy f = look1 >>= guard . Iv.member f >> get1
  char      = satisfy . Iv.singleton
  eof       = look >>= guard . Lazy.null
  pfail err = getTextPoint >>= \loc ->
    throwError $ parserError{ parserErrorAt=Just loc, parserLexerError=Just err }
  count c p = _take $ \t -> do
    c <- pure $ countToInt c
    let loop c tx = if c<=0 then return () else case tx of -- lazy string length
          []   -> mzero
          t:tx -> loop (c - fromIntegral (Strict.length t)) tx
    loop c $ Lazy.toChunks t
    (keep, rem    ) <- pure $ Lazy.splitAt c t
    (keep, putBack) <- pure $ Lazy.span (Iv.member p) keep
    guard (Lazy.null putBack) >> return (keep, rem)
  noMoreThan c p = _take $ \t -> do
    lim <- pure $ countToInt c
    let loop got c tx = case tx of
          []   -> return (Lazy.fromChunks got, mempty)
          t:tx -> let c' = c + fromIntegral (Strict.length t) in case c' > lim of
            True  -> do
              (keep, rem    ) <- pure $ Strict.splitAt (fromIntegral $ lim - c) t
              (keep, putBack) <- pure $ Strict.span (Iv.member p) keep
              return (Lazy.fromChunks $ got++[keep], Lazy.fromChunks $ putBack : rem : tx)
            False -> do
              (keep, rem    ) <- pure $ Strict.span (Iv.member p) t
              case Strict.null rem of
                True  -> loop (got++[keep]) c' tx
                False -> return (Lazy.fromChunks $ got++[keep], Lazy.fromChunks $ rem : tx)
    loop [] 0 (Lazy.toChunks t)

instance MonadPrecedenceParser Lazy.Text (Parser st) where
  prec c p = Parser (lift $ gets precedence) >>= guard . (c >=) >> _modPrecedence (const c) p
  step   p = _modPrecedence succ p
  reset  p = _modPrecedence (const 0) p

instance MonadSourceCodeParser Lazy.Text (Parser st) where
  getTextPoint = Parser $ lift $ gets textPoint
  setTextPoint = const $ return ()

instance MonadBacktrackingParser Lazy.Text (Parser st) where
  unstring t = _take $ \instr -> Just ((), t<>instr)

-- | Get the number of characters parsed so far.
getCharCount :: Parser st CharCount
getCharCount = Parser $ lift $ gets charCount

minPrec :: Int
minPrec = 0

_take :: (Lazy.Text -> Maybe (a, Lazy.Text)) -> Parser st a
_take f =
  (Parser $ lift $ state $ \st ->
    maybe (Nothing, st) (\ (a, rem) -> (Just a, st{ inputString=rem })) $ f $ inputString st
  ) >>= maybe mzero return

-- | Run the parser with a given 'ParserState' containing the input to be parsed and the
-- initializing state. The whole 'ParserState' is returned with the parsing result
-- 'Dao.Predicate.Predicate'.
runParser :: Parser st a -> ParserState st -> (Predicate ParserError a, ParserState st)
runParser (Parser p) = runState (runPredicateT p)

_modPrecedence :: (Int -> Int) -> Parser st o -> Parser st o
_modPrecedence f p = do
  c <- Parser (lift $ gets precedence)
  Parser (lift $ modify $ \st -> st{precedence=f c})
  let reset = Parser (lift $ modify $ \st -> st{precedence=c})
  o <- catchError (optional p) (\e -> reset >> throwError e)
  reset >> maybe mzero return o

-- | Convert a 'Parser' monad to some other monadic function. This is useful for converting 'Parser'
-- monad to a 'Prelude.ReadS' function that takes a precedence value. This is ideal for convering a
-- 'Parser' directly to a function that can be used to instantiate 'Prelude.readsPrec' for the
-- 'Prelude.Read' class. It can also be converted to a 'Text.ParserCombinators.ReadP.ReadP' or
-- 'Text.ParserCombinators.ReadPrec.ReadPrec' parser.
--
-- For example, if you have a 'Parser' function called @myParse@ of type:
--
-- > myParse :: 'Parser' () MyValue
--
-- you could use it to instantiate the 'Prelude.readsPrec' function for @MyValue@ like so:
--
-- > instance 'Prelude.Read' MyValue where { 'Prelude.readsPrec' = myParse () }
parser_to_S :: MonadPlus m => Parser st a -> st -> Int -> String -> m (a, String)
parser_to_S (Parser p) init prec instr = do
  let (result, st) = runState (runPredicateT p) $
        (parserState init){ precedence=prec, inputString=toLazyText instr }
  case result of
    PFail err -> fail (show err)
    Backtrack -> mzero
    OK     a  -> return (a, Lazy.unpack $ inputString st)

