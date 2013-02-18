-- "src/Dao/PPrint.hs"  a pretty-printer designed especially for
-- printing Dao script code.
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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.PPrint where

import           Dao.String

import           Control.Monad
import           Control.Monad.State

import           Data.List
import           Data.Char
import           Data.Monoid

----------------------------------------------------------------------------------------------------

-- | Remove trailing whitespace, I stole the idea from the Perl language.
chomp :: String -> String
chomp = foldl (\ out (spc, str) -> if null str then "" else out++spc++str) "" . spcstr where
  spcstr cx
    | null   cx = []
    | otherwise = 
         let (spc, more) = span  isSpace cx
             (str, cx  ) = break isSpace more
         in  (spc, str ) : spcstr cx

----------------------------------------------------------------------------------------------------

-- | The intermediate printer structure, the 'PPrint' monad generates a structure using this data
-- type, the pretty printer uses these types to make decisions on how to output information. This
-- structure contains the basic printing strategies involving how to wrap lines, indent, and place
-- lines of code in a block.
data PPrintItem
  = PNewLine -- ^ force a new line
  | PPrintString { pPrintString :: UStr } -- ^ a single string of output
  | PPrintInline { pPrintItems :: [PPrintItem] }
    -- ^ print items all on a single line of output, wrap if necessary, wrapping forces indentation.
    -- This should be used for outputting equations.
  | PPrintList
    { pPrintHeader :: [PPrintItem]
    , pPrintOpen   :: UStr
    , pPrintSep    :: UStr
    , pPrintClose  :: UStr
    , pPrintItems  :: [PPrintItem]
    }
    -- ^ a list of items separated by a separator string, as many items are written per line as
    -- possible, items wrap on the next line if they exceed the line limit, if a wrap occurs, the
    -- header is written on its own line, the items are written
  | PPrintClosure
    { pPrintHeader :: [PPrintItem]
    , pPrintOpen   :: UStr
    , pPrintClose  :: UStr
    , pPrintItems  :: [PPrintItem]
    }
    -- ^ used for things like functions and if statements. If possible, the header and closure are
    -- all written on one line, unless there are more than three items. If they cannot be written
    -- all on one line, or there are more than three items, then the items are printed one on each
    -- line and indented within the open and close strings.

-- | The state used by the 'PPrint' monad.
newtype PPrintState
  = PPrintState
    { pPrintStack :: [PPrintItem]
      -- ^ retrieves the stack of 'PPrintItem's generated during evaluation of 'PPrint' monadic
      -- functions.
    }

-- | Initialize the pretty printer monad, can be passed as the second argument to
-- 'Control.Monad.State.runState' and 'Control.Monad.State.execState'.
initPPrintState :: PPrintState
initPPrintState = PPrintState{ pPrintStack = [] }

instance Monoid PPrintState where
  mempty      = initPPrintState
  mappend a b = PPrintState{ pPrintStack = mappend (pPrintStack a) (pPrintStack b) }

-- | Evaluate a 'PPrint' monadic function to it's intermediate 'PPrintState'.
pEvalState :: PPrint () -> PPrintState
pEvalState fn = execState fn initPPrintState

instance Show PPrintState where { show = showPPrintState 80 "    " }
instance Show (PPrint ()) where { show p = show (execState p initPPrintState) }

----------------------------------------------------------------------------------------------------

class PPrintable a where { pPrint :: a -> PPrint () }

-- | A 'Control.Monad.State.State'ful pretty printer monad.
type PPrint a = State PPrintState a

-- not for export. Places a 'PPrintItem' onto the stack within the 'PPrintState'.
pPush :: PPrintItem -> PPrint ()
pPush item = modify (\p -> p{ pPrintStack = pPrintStack p ++ [item] })

-- not for export. Evaluates a 'PPrint' monad and returns the stack of 'PPrintItem's that were
-- created during evaluation.
pEval :: PPrint () -> [PPrintItem]
pEval fn = pPrintStack (execState fn initPPrintState)

-- | Force a new line
pNewLine :: PPrint ()
pNewLine = pPush PNewLine

instance PPrintable UStr where { pPrint = pPush . PPrintString }

-- | Print a 'Dao.String.UStr' as a single line.
pUStr :: UStr -> PPrint ()
pUStr u = if nil==u then return () else pPush (PPrintString u)

-- | Print a 'Prelude.String' as a single line.
pString :: String -> PPrint ()
pString = pPrint . ustr

-- | Print any value that instantiates 'Prelude.Show'.
pShow :: Show a => a -> PPrint ()
pShow = pString . show

-- | Shortcut for @('pPrint' . 'Data.List.concat')@
pConcat :: [String] -> PPrint ()
pConcat = pString . concat

-- | Evaluate the 'PPrint' printer, and every line of output will be used as an item in a list, and
-- every item is separated by a given separator string.
pInline :: PPrint () -> PPrint ()
pInline fn = pPush (PPrintInline (pEval fn))

-- | Like 'pInline' but places a separator string between each item.
pList :: PPrint () -> String -> String -> String -> PPrint () -> PPrint ()
pList header open sep close items = pPush $
  PPrintList
  { pPrintHeader = pEval header
  , pPrintOpen   = ustr  open
  , pPrintSep    = ustr  sep
  , pPrintClose  = ustr  close
  , pPrintItems  = pEval items
  }

pClosure :: PPrint () -> String -> String -> PPrint () -> PPrint ()
pClosure header open close items = pPush $
  PPrintClosure
  { pPrintHeader = pEval header
  , pPrintOpen   = ustr  open
  , pPrintClose  = ustr  close
  , pPrintItems  = pEval items
  }

----------------------------------------------------------------------------------------------------

-- not for export
data Printer
  = Printer
    { printerTab :: Int -- how many indentation marks should preceed this line
    , printerCol :: Int -- how many non-indentation characters are in the buffer
    , printerOut :: [(Int, Int, String)] -- all lines before the current line in the buffer
    , printerBuf :: String -- buffers the current line
    }

initPrinter :: Printer
initPrinter =
  Printer
  { printerTab = 0
  , printerCol = 0
  , printerOut = []
  , printerBuf = ""
  }

printerOutputTripple :: Printer -> (Int, Int, String)
printerOutputTripple st = (printerTab st, printerCol st, printerBuf st)

instance Monoid Printer where
  mempty = initPrinter
  mappend origSt st = case printerOut st of
    []                -> 
      origSt
      { printerBuf = printerBuf origSt ++ printerBuf st
      , printerCol = printerCol origSt +  printerCol st
      }
    (_, col, buf):out ->
      Printer
      { printerBuf = printerBuf st
      , printerCol = printerCol st
      , printerTab = printerTab st
      , printerOut = printerOut origSt ++
          (printerTab origSt, printerCol origSt + col, printerBuf origSt ++ buf) : out
      }

-- | A kind of pre-conversion, the 'PPrintState' is broken into a list of strings, each string
-- preceeded by it's indentation factor.
linesFromPPrintState :: Int -> PPrintState -> [(Int, String)]
linesFromPPrintState maxWidth ps = end (execState (mapM_ prin (pPrintStack ps)) mempty) where
  prin :: PPrintItem -> State Printer ()
  prin p = case p of
    PNewLine        -> newline
    PPrintString p  -> ustring p
    PPrintList   hdr opn sep clo px -> do
      let contents = sequence_ (intercalate [ustring sep] (map (return . prin) px))
      tryEach $
        [ tryInline (mapM_ prin hdr >> ustring opn >> contents >> ustring clo)
        , do  noDupNewline
              ok <- tryInline (mapM_ prin hdr >> ustring opn)
              if not ok
                then return False
                else do
                  newline >> setIndent (+1) >> contents
                  setIndent (+(0-1)) >> newline >> ustring clo
                  return True
        , do  printAcross (hdr++[PPrintString opn]) >> newline
              setIndent (+1)
              printAcross (intercalate [PPrintString sep] (map return px))
              setIndent (+(0-1))
              ustring clo
              return True
        ]
    PPrintInline px -> do
      let loop px = case px of
            []   -> return True
            p:px -> do
              origSt <- get
              let st = execState (prin p) mempty
                  newSt = mappend origSt st
              if null (printerOut st) && printerCol newSt <= maxWidth
                then put newSt >> loop px
                else do
                  newline >> setIndent (+1)
                  printAcross (p:px) >> setIndent (+(0-1))
                  newline >> return True
      tryEach [tryInline (mapM_ prin px), newline >> tryInline (mapM_ prin px), newline >> loop px]
    PPrintClosure hdr opn clo px -> do
      let gt3 = case px of
            (_:_:_:_:_) -> True
            _           -> False
          content = do
            printAcross (hdr++[PPrintString opn])
            newline >> setIndent (+1)
            mapM_ (\p -> prin p >> newline) px
            setIndent (+(0-1)) >> ustring clo
      if gt3
        then  content
        else  tryEach $
                [ tryInline (mapM_ prin hdr >> ustring opn >> mapM_ prin px >> ustring clo)
                , content >> return True
                ]
  ustring p = modify $ \st ->
    st{ printerCol = printerCol st + ulength p, printerBuf = printerBuf st ++ uchars p }
  setIndent f = modify $ \st -> st{ printerTab = f (printerTab st) }
  printAcross px = case px of
    []   -> return ()
    p:px -> do
      let trySt = execState (prin p) mempty
      st <- get
      if sumRunsOver st trySt
        then put (mappend st trySt)
        else newline >> modify (\st -> mappend st trySt)
      printAcross px
  tryInline fn = do
    let trySt = execState fn mempty
    st <- get
    if sumRunsOver st trySt then return False else put (mappend st trySt) >> return True
  tryEach fnx = case fnx of
    []     -> return ()
    fn:fnx -> get >>= \st -> fn >>= \ok -> if ok then return () else put st >> tryEach fnx
  sumRunsOver st trySt = null (printerOut trySt) && printerCol st + printerCol trySt <= maxWidth
  noDupNewline = gets printerBuf >>= \buf -> if null buf then return () else newline
  newline = modify $ \st ->
    st{ printerCol = 0
      , printerBuf = ""
      , printerOut = printerOut st ++ [(printerTab st, printerCol st, printerBuf st)]
      }
  end = map (\ (a, _, b) -> (a, chomp b)) . printerOut

-- | Given a list of strings, each prefixed with an indentation count, and an indentation string,
-- concatenate all strings into a one big string, with each string being indented and on it's own
-- line.
linesToString :: String -> [(Int, String)] -> String
linesToString indentStr = concatMap $ \ (indentCount, string) ->
  concat (replicate indentCount indentStr) ++ string ++ "\n"

-- Given an indentation string and a maximum width value, construct a string from the 'PPrintState'.
-- The maximum width value is used to call 'linesFromPPrintState', and the indentation string is
-- used to call 'linesToString'.
showPPrintState :: Int -> String -> PPrintState -> String
showPPrintState maxWidth indentStr ps = linesToString indentStr (linesFromPPrintState maxWidth ps)

