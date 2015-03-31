-- "Dao/PPrint.hs"  functions for pretty-printing with "Data.Text.Lazy"
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

-- | "Dao.Grammar' defines a domain-specific language for defining "context-free Grammars" (CFGs).
--
-- This module provides three key data types, 'Regex', 'Lexer', and 'Grammar', and the combinators
-- necessary to define CFGs. These data types are actually not parsers, but they can be converted
-- to parsers using 'regexToParser', 'lexerToParser', and 'grammarToParser'.
-- 
-- The 'Regex' is a simple data type that matches strings. 'Lexer's take 'Regex's and use the
-- matched strings to construct data types in 'Control.Monad.Monad'-ic and
-- 'Control.Applicative.Applicative' ways. The 'Grammar' data type uses 'Lexer's to build CFGs.
--
-- Once a 'Grammar' has been defined, the 'Grammar' can be converted to parser functions. The
-- parsing functions are polymorphic over any type of input string that instantiates the
-- 'Dao.Text.Builder.TextBuilder' class.
--
-- The 'Grammar' and 'Lexer' data types can be constructed freely, but impossible 'Grammar's, which
-- are 'Grammar's which could never possibly match any string, and ambiguous 'Grammar's, which are
-- 'Grammar's where a single input could match more than one 'Grammar' in a list of concurrent
-- 'Grammar' choices, evaluate to an 'InvalidGrammar' data type. This allows you to build your
-- 'Grammar's without worrying about the implementation details of the actual parser. Parsers
-- constructed by valid 'Grammar's should always work as intended.
--
-- The 'Grammar', 'Lexer', and 'Regex' can convert to any monadic parser. A type class 'MonadParser'
-- is defined to classify the various monadic parsers that a 'Grammar', 'Lexer' or 'Regex' could be
-- converted to. The Haskell Platform parsers 'Text.ParserCombinator.ReadP' and
-- 'Text.ParserCombinator.ReadPrec' are instantiated into the 'MonadParser' class so 'Grammar's can
-- be used to instantiate 'Prelude.Read.readPrec'.
--
-- The "Test.QuickCheck.Arbitrary" module is imported, and a 'Grammar' can be used to generate
-- 'Test.QuickCheck.Arbitrary.arbitrary' input strings that your 'Grammar' can parse.
--
-- There is one major design limitation: 'Grammar's cannot be checked at compile time. An
-- 'InvalidGrammar' is thrown as an 'Control.Exception.Exception' during evaluation of the parser as
-- soon as it is discovered, so "Test.QuickCheck" testing is still necessary to try and discover
-- problem areas in your 'Grammar'. Fortunately, the 'InvalidGrammar' exceptions can be helpful at
-- tracking down invalid grammars and offering suggestions on how to correct them.
module Dao.PPrint where

import           Dao.Count
import           Dao.Text

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Data.List (intersperse)
import           Data.Monoid
import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as Lazy
import           Data.Typeable

sp :: StrictText
sp = Strict.singleton ' '

-- | Calls 'textPPrinter' with a new empty 'PPrintState' and 'tabWidth' and 'wrapWidth'
-- (respectively) each set with the first two given integer parameters.
runTextPPrinter :: Int -> Int -> [PPrint] -> LazyText
runTextPPrinter tabs wraps tx =
  evalState (mapM_ textPPrinter tx >> gets outputText) (mempty{ tabWidth=tabs, wrapWidth=wraps })

-- | Calls 'runTextPPrinter' and converts the result to a 'Prelude.String', convenient for
-- instantiating 'PPRintable' objects into the 'Prelude.Show' class.
showPPrint :: Int -> Int -> [PPrint] -> String
showPPrint t w = Lazy.unpack . runTextPPrinter t w

----------------------------------------------------------------------------------------------------

type HtmlClass = StrictText

type HtmlWidth = Int

data HtmlRow
  = HtmlTableHeader HtmlClass [HtmlCell]
  | HtmlTableRow    HtmlClass [HtmlCell]
  deriving (Eq, Ord, Show, Typeable)

data HtmlCell = HtmlCell HtmlClass [PPrint]
  deriving (Eq, Ord, Show, Typeable)

-- | A 'PPrint' is an instruction for outputting pretty text. There are very simple HTML
-- constructing functions as well. Convert a list of 'PPrint' tokens to text using 'runTextPPrinter' or
-- 'pprintHtml'. The 'runTextPPrinter' function will generate plain text even if HTML structures are
-- printed.
data PPrint
  = PNewLine        -- ^ recommend a place where a new line can be placed
  | PForceLine  Int -- ^ force a new line
  | PSpace          -- ^ a single space, multiple consecutive 'PSpace's are compressed into a single space.
  | PForceSpace Int -- ^ place a number of spaces, even if we are at the end or beginning of a line.
  | PText       StrictText -- ^ insert text
  | PSetTabStop -- ^ set the tab stop at the current column
  | PTabToStop  -- ^ place spaces until the next tab stop
  | PClearTabStops -- ^ clear all current tab stops.
  | PInline [PPrint]
    -- ^ replace all 'PNewLine's with spaces and try to print into a single line. If the text
    -- can fit into the current line without going passed the wrap limit (if the resulting
    -- text length is less than the current wrap width minus the current column count) then print
    -- everything onto the current line.
  | PIndent [PPrint]
    -- ^ recommends to the printer that the following tokens be tabbed if they are to start on a new
    -- line, and wrapped tokens will also be indented.
  | PSpan       HtmlClass [PPrint] -- ^ when outputting HTML, set the SPAN class.
  | PQuote      HtmlClass [PPrint] -- ^ output an HTML quoted span.
  | PMaxColumns Int [PPrint] [PPrint]
    -- ^ truncate output of the first 'PPrint' to the given maximum number of columns, append the
    -- second 'PPrint' if the output is truncated 
  | PMaxRows    Int [PPrint] [PPrint]
    -- ^ truncate output of the first 'PPrint' to the given maximum number of rows, append the
    -- second 'PPrint' if the output is truncated .
  | PParagraph  HtmlClass [PPrint]
  | PDiv        HtmlClass [PPrint]
  | PTable      HtmlClass (Maybe HtmlWidth) [HtmlRow]
  | PNumbered   HtmlClass [HtmlCell]
  | PBulleted   HtmlClass [HtmlCell]
  | PHeader1    HtmlClass [PPrint]
  | PHeader2    HtmlClass [PPrint]
  | PHeader3    HtmlClass [PPrint]
  | PHeader4    HtmlClass [PPrint]
  deriving (Eq, Ord, Show, Typeable)

-- | Use the instance of 'Prelude.Show' for a given data type, convert the data type to a string,
-- then pack the entire string into a 'PText' constructor.
pShow :: Show t => t -> PPrint
pShow = pText . show

-- | Like 'PText' but more convenient because it takes as it's parameter any data type that
-- instantiates 'Dao.Text.ToText'.
pText :: ToText t => t -> PPrint
pText = PText . toText

-- | Like 'PInline', but automatically calls 'cleanSpaces' on the input parameter.
pInline :: [PPrint] -> PPrint
pInline = PInline . cleanSpaces

-- | Like 'PIndent', but automatically calls 'cleanSpaces' on the input parameter.
pIndent :: [PPrint] -> PPrint
pIndent = PIndent . cleanSpaces

-- | Like 'PSpan', but automatically calls 'cleanSpaces' on the input 'PPrint' list.
pSpan :: HtmlClass -> [PPrint] -> PPrint
pSpan c = PSpan c . cleanSpaces

-- | Like 'PSpan', but automatically calls 'cleanSpaces' on the input 'PPrint' list.
pDiv :: HtmlClass -> [PPrint] -> PPrint
pDiv c = PDiv c . cleanSpaces

-- | Like 'PSpan', but automatically calls 'cleanSpaces' on the input 'PPrint' list.
pQuote :: HtmlClass -> [PPrint] -> PPrint
pQuote c = PQuote c . cleanSpaces

-- | Synonym for 'PNewLine', but with a lowercase leading @p@ so you don't get confused.
pNewLine :: PPrint
pNewLine = PNewLine

-- | Synonym for 'PForceLine', but with a lowercase leading @p@ so you don't get confused.
pForceLine :: Int -> PPrint
pForceLine = PForceLine

-- | Synonym for 'PSpace', but with a lowercase leading @p@ so you don't get confused.
pSpace :: PPrint
pSpace = PSpace

-- | Synonym for 'PForceSpan', but with a lowercase leading @p@ so you don't get confused.
pForceSpace :: Int -> PPrint
pForceSpace = PForceSpace

-- | Synonym for 'PSetTabStop', but with a lowercase leading @p@ so you don't get confused.
pSetTabStop :: PPrint
pSetTabStop = PSetTabStop

-- | Synonym for 'PTabToStop', but with a lowercase leading @p@ so you don't get confused.
pTabToStop :: PPrint
pTabToStop = PTabToStop

-- | Synonym for 'PClearTabStops', but with a lowercase leading @p@ so you don't get confused.
pClearTabStops :: PPrint
pClearTabStops = PClearTabStops

-- | Using 'Prelude.words', break up a string into 'PPrint' units constructed with 'pText' and
-- 'Data.List.intersperse'd with 'pSpace's.
pSentence :: String -> [PPrint]
pSentence = intersperse PSpace . fmap pText . words

class PPrintable o where { pPrint :: o -> [PPrint] }

instance PPrintable ()           where { pPrint = return . pShow }
instance PPrintable Char         where { pPrint = return . pShow }
instance PPrintable Int          where { pPrint = return . pShow }
instance PPrintable Count        where { pPrint = return . pShow }
instance PPrintable Integer      where { pPrint = return . pShow }
instance PPrintable Double       where { pPrint = return . pShow }
instance PPrintable String       where { pPrint = return . pShow }
instance PPrintable Strict.Text  where { pPrint = return . pShow }
instance PPrintable Lazy.Text    where { pPrint = return . pShow }

----------------------------------------------------------------------------------------------------

data PPrintState
  = PPrintState
    { wrapWidth      :: Int
    , newlineWithCR  :: Bool
    , tabWidth       :: Int
    , tabStops       :: [Int]
    , currentIndent  :: Int
    , currentColumn  :: Int
    , currentLine    :: Int
    , outputText     :: LazyText
    }

instance Monoid PPrintState where
  mempty =
    PPrintState
    { wrapWidth     = 80
    , tabWidth      = 4
    , newlineWithCR = False
    , currentIndent = 0
    , currentColumn = 0
    , currentLine   = 1
    , tabStops      = []
    , outputText    = mempty
    }
  mappend a b = b{ outputText = mappend (outputText a) (outputText b) }

data TextStatistics
  = TextStatistics
    { maximumWidth      :: Int -- ^ number of columns
    , maximumHeight     :: Int -- ^ number of rows
    , sizeOfContent     :: Int -- ^ sum total of every 'Strict.length' of every 'Strict.Text' item.
    , statColumnCounter :: Int -- ^ length of the final
    }

instance Monoid TextStatistics where
  mempty =
    TextStatistics
    { maximumWidth      = 0
    , maximumHeight     = 0
    , sizeOfContent     = 0
    , statColumnCounter = 0
    }
  mappend a b =
    b { maximumWidth      = max (maximumWidth  a) (maximumWidth  b)
      , maximumHeight     = max (maximumHeight a) (maximumHeight b)
      , sizeOfContent     = sizeOfContent     a + sizeOfContent b
      , statColumnCounter = statColumnCounter a +
          (if maximumHeight b > 0 then 0 else statColumnCounter b)
      }

-- | Gather statistics on the maximum width and height, and the total number of 'Data.Text.Text'
-- characters contained in the list of 'PText's. 'Prelude.Nothing' can be returned under the
-- following conditions:
-- 1. an integer limit greater than zero is provided as the first parameter to this function
-- 2. either the total number of characters exceeds this limit, or if there are any formatters that
--    necessetate multiple lines, for example 'PTable', 'PParagraph'.
--
-- One purpose of this function is to determine if the text in a 'PPrint' list can be inlined. The
-- limit provided can halt calculation of the statistics early so the entire 'PPrint' list need not
-- be evaluated once it is obvious that the text cannot be inlined.
-- 
-- 'PNewLine's are counted as a single unit of content and increment the 'sizeOfContent' value, it
-- does not halt calculation. 'PNewLine's are in fact only recommendations to the interpreter as to
-- where to place newlines.
approxWidth :: Int -> [PPrint] -> Maybe TextStatistics
approxWidth limit tx = evalState (runMaybeT $ loop 0 tx >> get) mempty where
  newline   c = lift $ state  $ \st -> let n = maximumHeight st + c in (n, st{ maximumHeight=n })
  increment c = lift $ modify $ \st ->
    let n = sizeOfContent st + c
    in st{ sizeOfContent=n, maximumWidth=max n $ maximumWidth st }
  loop rowLimit tx = do
    lift (gets sizeOfContent) >>= guard . \i -> limit>0 && i<limit
    case tx of
      []   -> return ()
      t:tx -> let next = loop rowLimit in case t of
        PNewLine               -> newline 1 >>= \n -> unless (rowLimit>0 && n>=rowLimit) (next tx)
        PForceLine  i          -> guard (limit<=0) >> newline i >> next tx
        PSpace                 -> increment 1 >> next tx
        PForceSpace i          -> increment i >> next tx
        PText       t          -> increment (Strict.length t) >> next tx
        PSetTabStop            -> next tx
        PTabToStop             -> next tx
        PClearTabStops         -> next tx
        PInline           ux   -> next ux >> next tx
        PIndent           ux   -> next ux >> next tx
        PSpan       _     ux   -> next ux >> next tx
        PQuote      _     ux   -> increment 2 >> next ux >> next tx
        PMaxColumns _     ux _ -> next ux >> next tx
        PMaxRows rowLimit ux _ -> do
          n <- lift $ gets maximumHeight
          loop (if rowLimit>0 then n+rowLimit else 0) ux
          next tx
        _                      -> mzero -- TODO: computations on the remainder of the HTML elements.

textPPrintBreakLine :: Int -> State PPrintState ()
textPPrintBreakLine i =
  modify $ \st ->
    st{ currentLine   = currentLine st + i
      , currentColumn = 0
      , outputText    = mappend (outputText st) $ Lazy.fromChunks $ return $ Strict.replicate i $
          if newlineWithCR st then Strict.pack "\r\n" else Strict.singleton '\n'
      }

-- | Scans through a list of 'PPrint' items, if there are multiple 'PSpace's they are joined into
-- one, all 'PSapace's following a 'PForceSpace' are removed. If there are multiple 'PNewLine's they
-- are joined into one, 'PNewLine's following 'PForceNewLine's are removed. All spaces before any
-- 'PNewLine's or 'PForceLine's are removed. Pass a 'Prelude.Bool' value indicating whether you want
-- the space cleaning to recurse into all 'PPrint' instructions, you should almost never need to
-- pass 'Prelude.True'.
cleanSpaces :: [PPrint] -> [PPrint]
cleanSpaces = loop where
  loop tx = case tx of
    []                         -> []
    PSpace                : _  -> joinSpaces False 0 tx
    PForceSpace _         : _  -> joinSpaces True  0 tx
    PNewLine              : _  -> joinLines        0 tx
    PForceLine  _         : _  -> joinLines        0 tx
    PText       t         : tx -> PText        t : loop tx
    PSetTabStop           : tx -> PSetTabStop    : loop tx
    PTabToStop            : tx -> PTabToStop     : loop tx
    PClearTabStops        : tx -> PClearTabStops : loop tx
    PInline         ux    : tx -> PInline  ux : loop tx
    PIndent         ux    : tx -> PIndent  ux : loop tx
    PMaxColumns c   ux vx : tx -> PMaxColumns c ux vx : loop tx
    PMaxRows    c   ux vx : tx -> PMaxRows    c ux vx : loop tx
    PSpan       c   ux    : tx -> PSpan  c ux : loop tx
    PDiv        c   ux    : tx -> PDiv   c ux : loop tx
    PQuote      c   ux    : tx -> PQuote c ux : loop tx
    PTable      c w ux    : tx -> PTable c w ux : loop tx
    PParagraph  c   ux    : tx -> PParagraph c ux : loop tx
    PNumbered   c   ux    : tx -> PNumbered  c (fmap cells ux) : loop tx
    PBulleted   c   ux    : tx -> PBulleted  c (fmap cells ux) : loop tx
    PHeader1    c   ux    : tx -> PHeader1   c ux : loop tx
    PHeader2    c   ux    : tx -> PHeader2   c ux : loop tx
    PHeader3    c   ux    : tx -> PHeader3   c ux : loop tx
    PHeader4    c   ux    : tx -> PHeader3   c ux : loop tx
  cells (HtmlCell c tx) = HtmlCell c tx
  make forcedConstr constr len tx =
    if len==1 then constr : loop tx else if len>0 then forcedConstr len : loop tx else loop tx
  makeSpace = make PForceSpace PSpace
  makeLine  = make PForceLine  PNewLine
  joinSpaces forced len tx = case tx of
    []                 -> if forced then makeSpace len [] else []
    PSpace        : tx -> joinSpaces forced (if len<=0 then 1 else len) $ loop tx
    PForceSpace i : tx -> if i>0 then joinSpaces True (len+i) tx else joinSpaces True len tx
    PNewLine      : _  -> joinLines 0 $ if forced then makeSpace len tx else joinLines 0 tx
    PForceLine  _ : _  -> joinLines 0 $ if forced then makeSpace len tx else joinLines 0 tx
    tx                 -> makeSpace len $ loop tx
  joinLines len tx = case tx of
    []                 -> makeLine len []
    PNewLine      : tx -> joinLines (if len==0 then 1 else len) $ loop tx
    PForceLine i  : tx -> joinLines (len+i) $ loop tx
    tx                 -> makeLine len $ loop tx

-- | Remove formatting and return a simplified list of 'PPrint's that will just print everything on
-- a single line on the current line, ignore all formatting commands, do not wrap. 'PForceLine' will
-- still create newlines. The first 'Prelude.Bool' parameter indicates whether you would like to
-- honor 'PIndent' formatters. The second 'Prelude.Bool' parameter indicates whether you would like
-- to honor 'PNewLine' formatters; passing 'Prelude.False' for the second parameter will replace
-- 'PNewLine's with 'PSpace's.
textPPrintInline :: Bool -> Bool -> [PPrint] -> [PPrint]
textPPrintInline keepIndents keepNewLines tx = do
  let loop = textPPrintInline keepIndents keepNewLines
  let cell (HtmlCell _ tx) = loop tx
  let q = [PText $ Strict.singleton '"']
  t <- cleanSpaces tx
  case t of
    PNewLine           -> guard keepNewLines >> return PNewLine
    PForceLine  i      -> return $ PForceLine i
    PSpace             -> return PSpace
    PForceSpace i      -> return $ PForceSpace i
    PText       t      -> return $ PText t
    PSpan       _ tx   -> loop tx
    PQuote      _ tx   -> concat [q, tx, q]
    PMaxColumns _ tx _ -> loop tx
    PMaxRows    _ tx _ -> loop tx
    PTable      _ _ tx -> tx >>= \t -> case t of
      HtmlTableHeader _ tx -> tx >>= cell
      HtmlTableRow    _ tx -> tx >>= cell
    PParagraph  _ tx   -> loop tx
    PNumbered   _ tx   -> tx >>= cell
    PBulleted   _ tx   -> tx >>= cell
    PHeader1    _ tx   -> loop tx
    PHeader2    _ tx   -> loop tx
    PHeader3    _ tx   -> loop tx
    PHeader4    _ tx   -> loop tx
    _                  -> mzero

-- | This is the 'Control.Monad.State.State'-ful text printer, converting a string of 'PPrint's to
-- plain text.
textPPrinter :: PPrint -> State PPrintState ()
textPPrinter t = case t of
  PNewLine       -> textPPrintBreakLine 1
  PForceLine i   -> textPPrintBreakLine i
  PSpace         -> modify $ \st ->
    st{ outputText = mappend (outputText st) $
          if currentColumn st == 0 then mempty else Lazy.fromChunks [sp]
      }
  PForceSpace i  -> modify $ \st ->
    st{ outputText    = outputText st <> Lazy.fromChunks [Strict.replicate i sp]
      , currentColumn = currentColumn st + i
      }
  PSetTabStop    -> modify $ \st ->
    st{ tabStops =
          let col = currentColumn st
              loop zx tx = case tx of
                []            -> zx++[col]
                t:tx | t==col -> zx++tx
                t:tx | t >col -> zx++col:t:tx
                t:tx          -> loop (zx++[t]) tx
          in  loop [] $ tabStops st
      }
  PClearTabStops -> modify $ \st -> st{ tabStops=[] }
  PTabToStop     -> modify $ \st ->
    let col = currentColumn st
        tab = tabWidth st
        tx  = dropWhile (< col) (tabStops st)
    in  st{ currentColumn = if null tx then tab * (1 + div col tab) else head tx } 
  PText   t      -> modify $ \st ->
    let col = currentColumn st
        tab = if col==0 then Strict.replicate (currentIndent st) sp else mempty
        txt = tab <> t
    in  st{ outputText    = mappend (outputText st) $ Lazy.fromChunks [txt]
          , currentColumn = currentColumn st + Strict.length txt
          }
  PInline tx     -> do
    width <- gets wrapWidth
    col   <- gets currentColumn
    case approxWidth (width-col) tx of
      Nothing -> mapM_ textPPrinter tx
      Just  _ -> mapM_ textPPrinter $ textPPrintInline False False tx
  PIndent tx     -> do
    tab <- gets currentIndent
    modify $ \st -> st{ currentIndent = tab + tabWidth st }
    mapM_ textPPrinter tx
    modify $ \st -> st{ currentIndent = tab }
  -- o ^ Recommends to the printer that the following tokens be tabbed if they are to start on a new
  -- line, and wrapping tokens will also be indented.
  PSpan  _ tx    -> mapM_ textPPrinter tx
  PQuote _ tx    -> let q = [PText $ Strict.singleton '"'] in mapM_ textPPrinter $ concat [q, tx, q]
  _              -> return () -- TODO: a proper textual representation of the other HTML elements.

