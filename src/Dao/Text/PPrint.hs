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

-- | Using the editor is a little tedious. This module provides a pretty-printing data type 'PPrint'
-- data type which you can use to define blocks of code that can be indented and wrapped, and this
-- 'PPrint' type can be transformed into an 'Dao.Text.Editor.EditorT' monadic function.
module Dao.Text.PPrint where

import           Prelude hiding (id, (.))

import           Dao.Count
import           Dao.Lens
import           Dao.Predicate
import           Dao.Text
import           Dao.Text.Editor

import           Control.Category
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer

import           Data.List (intersperse, intercalate)
import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as Lazy
import           Data.Typeable

----------------------------------------------------------------------------------------------------

sp :: StrictText
sp = Strict.singleton ' '

-- | Calls 'textPPrinter' with a new empty 'PPrintState' and 'tabWidth' and 'wrapWidth'
-- (respectively) each set with the first two given integer parameters.
runTextPPrinter :: Monad m => Int -> Int -> [PPrint] -> m LazyText
runTextPPrinter tabs wraps tx =
  evalStateT (runEditorT $ mapM_ textPPrinter tx >> gets (~> outputText)) $
    new[tabWidth <~ tabs, wrapWidth <~ wraps]

runPureTextPPrinter :: Int -> Int -> [PPrint] -> LazyText
runPureTextPPrinter tabs wraps = runIdentity . runTextPPrinter tabs wraps

-- | Calls 'runTextPPrinter' and converts the result to a 'Prelude.String', convenient for
-- instantiating 'PPRintable' objects into the 'Prelude.Show' class.
showPPrint :: Int -> Int -> [PPrint] -> String
showPPrint t w = Lazy.unpack . runPureTextPPrinter t w

----------------------------------------------------------------------------------------------------

-- | A 'PPrint' is an instruction for outputting pretty text.  constructing functions as well.
-- Convert a list of 'PPrint' tokens to text using 'runTextPPrinter'.
data PPrint
  = PNewLine        -- ^ recommend a place where a new line can be placed
  | PForceLine  Int -- ^ force a new line
  | PSpace          -- ^ a single space, multiple consecutive 'PSpace's are compressed into a single space.
  | PForceSpace Int -- ^ place a number of spaces, even if we are at the end or beginning of a line.
  | PChar       Char -- ^ place a single character.
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
  | PMaxColumns Int [PPrint] [PPrint]
    -- ^ truncate output of the first 'PPrint' to the given maximum number of columns, append the
    -- second 'PPrint' if the output is truncated 
  | PMaxRows    Int [PPrint] [PPrint]
    -- ^ truncate output of the first 'PPrint' to the given maximum number of rows, append the
    -- second 'PPrint' if the output is truncated .
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

-- | Place a single character.
pChar :: Char -> PPrint
pChar = PChar

-- | Another name for 'Data.List.intercalate'.
pSepBy :: [PPrint] -> [[PPrint]] -> [PPrint]
pSepBy = intercalate

class PPrintable o where { pPrint :: o -> [PPrint] }

instance PPrintable ()           where { pPrint = return . pShow }
instance PPrintable Bool         where { pPrint = return . pShow }
instance PPrintable Char         where { pPrint = return . pShow }
instance PPrintable Int          where { pPrint = return . pShow }
instance PPrintable Count        where { pPrint = return . pShow }
instance PPrintable Integer      where { pPrint = return . pShow }
instance PPrintable Double       where { pPrint = return . pShow }
instance PPrintable String       where { pPrint = return . pShow }
instance PPrintable Strict.Text  where { pPrint = return . pShow }
instance PPrintable Lazy.Text    where { pPrint = return . pShow }

instance (PPrintable err, PPrintable o) => PPrintable (Predicate err o) where
  pPrint o = do
    let f :: PPrintable p => String -> p -> [PPrint]
        f c o = [pText c, pSpace, pChar '(', pNewLine, pInline (pPrint o), pNewLine, pChar ')']
    case o of
      PFalse   -> [pText "PFalse"]
      PTrue  o -> f "PTrue"  o
      PError o -> f "PError" o

----------------------------------------------------------------------------------------------------

-- | This is the data produced by the 'approxWidth' function. It is used to try and determine how
-- large (width and height) the text will be when printing a a list of 'PPrint' structures.
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
        PChar       _          -> increment 1 >> next tx
        PText       t          -> increment (Strict.length t) >> next tx
        PSetTabStop            -> next tx
        PTabToStop             -> next tx
        PClearTabStops         -> next tx
        PInline           ux   -> next ux >> next tx
        PIndent           ux   -> next ux >> next tx
        PMaxColumns _     ux _ -> next ux >> next tx
        PMaxRows rowLimit ux _ -> do
          n <- lift $ gets maximumHeight
          loop (if rowLimit>0 then n+rowLimit else 0) ux
          next tx

-- | Scans through a list of 'PPrint' items, if there are multiple 'PSpace's they are joined into
-- one, all 'PSapace's following a 'PForceSpace' are removed. If there are multiple 'PNewLine's they
-- are joined into one, 'PNewLine's following 'PForceNewLine's are removed. All spaces before any
-- 'PNewLine's or 'PForceLine's are removed. Pass a 'Prelude.Bool' value indicating whether you want
-- the space cleaning to recurse into all 'PPrint' instructions, you should almost never need to
-- pass 'Prelude.True'.
cleanSpaces :: [PPrint] -> [PPrint]
cleanSpaces = loop where
  loop tx = case tx of
    [] -> []
    ------------------------------------
    PForceSpace i : tx | i<=0 -> loop tx
    PForceLine  i : tx | i<=0 -> loop tx
    ----------------------------------------------------------------------------
    PNewLine      : PNewLine      : tx -> loop $ PNewLine                   : tx
    PNewLine      : PForceLine  i : tx -> loop $ PForceLine    i : PNewLine : tx
    PForceLine  i : PNewLine      : tx -> loop $ PForceLine    i            : tx
    PForceLine  a : PForceLine  b : tx -> loop $ PForceLine  (a+b)          : tx
    -----------------------------------------------------------------------------
    PSpace        : PSpace        : tx -> loop $ PSpace                     : tx
    PSpace        : PForceSpace i : tx -> loop $ PForceSpace   i : PSpace   : tx
    PForceSpace i : PSpace        : tx -> loop $ PForceSpace   i            : tx
    PForceSpace a : PForceSpace b : tx -> loop $ PForceSpace (a+b)          : tx
    ----------------------------------------------------------------------------
    PSpace        : PNewLine      : tx -> loop $ PNewLine                   : tx
    PSpace        : PForceLine  i : tx -> loop $ PForceLine    i            : tx
    PNewLine      : PSpace        : tx -> loop $ PNewLine                   : tx
    PForceLine  i : PSpace        : tx -> loop $ PForceLine    i            : tx
    ----------------------------------------------------------------------------
    t:tx -> t : loop tx

-- | Remove formatting and return a simplified list of 'PPrint's that will just print everything on
-- a single line on the current line, ignore all formatting commands, do not wrap. 'PForceLine' will
-- still create newlines. The first 'Prelude.Bool' parameter indicates whether you would like to
-- honor 'PIndent' formatters. The second 'Prelude.Bool' parameter indicates whether you would like
-- to honor 'PNewLine' formatters; passing 'Prelude.False' for the second parameter will replace
-- 'PNewLine's with 'PSpace's.
textPPrintInline :: Bool -> Bool -> [PPrint] -> [PPrint]
textPPrintInline keepIndents keepNewLines tx = do
  let loop = textPPrintInline keepIndents keepNewLines
  t <- cleanSpaces tx
  case t of
    PNewLine           -> guard keepNewLines >> return PNewLine
    PForceLine  i      -> return $ PForceLine i
    PSpace             -> return PSpace
    PForceSpace i      -> return $ PForceSpace i
    PText       t      -> return $ PText t
    PMaxColumns _ tx _ -> loop tx
    PMaxRows    _ tx _ -> loop tx
    _                  -> mzero

-- | This is the 'Control.Monad.State.State'-ful text printer, converting a string of 'PPrint's to
-- plain text.
textPPrinter :: Monad m => PPrint -> EditorT m ()
textPPrinter t = case t of
  PNewLine       -> lineBreak 1
  PForceLine i   -> lineBreak i
  PSpace         -> modify $ \st -> with st
    [outputText $= if st~>currentColumn == 0 then id else flip mappend $ Lazy.fromStrict sp]
  PForceSpace i  -> modify $ \st -> with st
    [ outputText    $= flip mappend $ Lazy.fromChunks [Strict.replicate i sp]
    , currentColumn $= (+ i)
    ]
  PSetTabStop    -> modify $ \st -> with st
    [ tabStops $=
        let col = st~>currentColumn
            loop zx tx = case tx of
              []            -> zx++[col]
              t:tx | t==col -> zx++tx
              t:tx | t >col -> zx++col:t:tx
              t:tx          -> loop (zx++[t]) tx
        in  loop []
    ]
  PClearTabStops -> modify $ by [tabStops <~ []]
  PTabToStop     -> modify $ \st -> with st $
    let col = st~>currentColumn
        tab = st~>tabWidth
        tx  = dropWhile (< col) (st~>tabStops)
    in  [currentColumn <~ if null tx then tab * (1 + div col tab) else head tx]
  PChar   c      -> modify $ \st -> with st $ [outputText $= flip Lazy.snoc c, currentColumn $= (+ 1)]
  PText   t      -> modify $ \st -> with st $
    let col = st~>currentColumn
        tab = if col==0 then Strict.replicate (st~>currentIndent) sp else mempty
        txt = tab <> t
    in  [ outputText    $= flip mappend $ Lazy.fromChunks [txt]
        , currentColumn $= (+ (Strict.length txt))
        ]
  PInline tx     -> do
    width <- gets (~> wrapWidth)
    col   <- gets (~> currentColumn)
    case approxWidth (width-col) tx of
      Nothing -> mapM_ textPPrinter tx
      Just  _ -> mapM_ textPPrinter $ textPPrintInline False False tx
  PIndent tx     -> do
    tab <- gets (~> currentIndent)
    modify $ \st -> with st [currentIndent <~ tab + st~>tabWidth]
    mapM_ textPPrinter tx
    modify $ by [currentIndent <~ tab]
  -- o ^ Recommends to the printer that the following tokens be tabbed if they are to start on a new
  -- line, and wrapping tokens will also be indented.
  _              -> return ()

