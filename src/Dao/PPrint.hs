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
import           Data.Word
import           Data.Monoid

import Debug.Trace

----------------------------------------------------------------------------------------------------

-- | This is the function you will probably care about most: take a value of any data type that
-- instantiates 'PPrintable', and a maximum text-wrapping width value, and a tab string, and will
-- convert that value to a 'Prelude.String'.
prettyPrint :: PPrintable a => Int -> String -> a -> String
prettyPrint maxWidth tab = showPPrint maxWidth tab . pPrint

-- | Calls 'prettyPrint' with the default values @80@ for the text-wrapping width, and a tab string
-- consisting of four single-space characters (four ASCII @0x20@ characters).
prettyShow :: PPrintable a => a -> String
prettyShow = prettyPrint 80 "    "

----------------------------------------------------------------------------------------------------

-- | Remove trailing whitespace, I stole the idea from the Perl language.
chomp :: String -> String
chomp = foldl (\ out (spc, str) -> if null str then out else out++spc++str) "" . spcstr where
  spcstr cx = case cx of
    "" -> []
    cx -> (spc, str) : spcstr cx' where
      (spc, more) = span  isSpace cx
      (str, cx' ) = break isSpace more

-- | like 'Prelude.map', but doesn't touch the last item in the list.
mapAlmost :: (a -> a) -> [a] -> [a]
mapAlmost fn ax = case ax of
  [] -> []
  [a] -> [a]
  a:ax -> fn a : mapAlmost fn ax

-- | like 'Prelude.map', but doesn't touch the first item in the list.
mapTail :: (a -> a) -> [a] -> [a]
mapTail fn ax = case ax of
  []   -> []
  a:ax -> a : map fn ax

----------------------------------------------------------------------------------------------------

class PPrintable a where { pPrint :: a -> PPrint () }

-- | Put a new line regardless of whether or not we are aleady on a new line.
pNewLine :: PPrint ()
pNewLine = modify $ \st ->
  st{ printerCol = 0
    , printerBuf = ""
    , printerOut = printerOut st ++ [printerOutputTripple st]
    , lineCount  = lineCount st + 1
    , printerTab = nextTab st
    }

-- | Like 'pNewLine' but also indicates that there *must* be a new line here (like after a comment)
-- to prevent lines from being joined.
pForceNewLine :: PPrint ()
pForceNewLine = modify (\st -> st{forcedNewLine=True})

-- | Place a new line unless we are already on a new line.
pEndLine :: PPrint ()
pEndLine = gets printerCol >>= \col ->
  if col==0 then modify (\st -> st{printerTab=nextTab st}) else pNewLine
    

pIndent :: PPrint () -> PPrint ()
pIndent indentedPrinter = do
  tab <- gets nextTab
  modify (\st -> st{nextTab=tab+1})
  indentedPrinter
  modify (\st -> st{nextTab=tab})

instance PPrintable UStr where { pPrint = pUStr }

-- not for export
appendString :: Int -> String -> PPrint ()
appendString len str = modify $ \st ->
  st{ printerCol = printerCol st + len
    , printerBuf = printerBuf st ++ str
    , charCount  = charCount  st + len
    }

-- | Print a 'Dao.String.UStr' as a single line.
pUStr :: UStr -> PPrint ()
pUStr u = if nil==u then return () else appendString (ulength u) (uchars u)

-- | Print a 'Prelude.String' as a single line.
pString :: String -> PPrint ()
pString s = if null s then return () else appendString (length s) s

-- | Print any value that instantiates 'Prelude.Show'.
pShow :: Show a => a -> PPrint ()
pShow = pString . show

-- | Shortcut for @('pPrint' . 'Data.List.concat')@
pConcat :: [String] -> PPrint ()
pConcat = pString . concat

-- | Just keep printing items along the line without wrapping until a 'pNewLine' or 'pEndLine'
-- occurs. Actually, this function simply a synonym for 'Control.Monad.sequence_'.
pNoWrap :: [PPrint ()] -> PPrint ()
pNoWrap = sequence_

-- | Try to print with the given function, but if the printed text runs past the 'maxWidth', or if
-- the printed text results in multiple lines of output, end the current line of text before
-- placing the text from the given function.
pWrap :: PPrint () -> PPrint ()
pWrap fn = do
  st <- get
  let trySt = execState fn (subprint st)
  if printerCol st + charCount trySt > maxWidth st then pEndLine else return ()
  appendState trySt

-- | Evaluate the 'PPrint' printer, and every line of output will be used as an item in a list and
-- printed across a line, wrapping on to the next line if the line goes past the width limit.
pInline :: [PPrint ()] -> PPrint ()
pInline = sequence_ . map pWrap

-- | Like 'pInline' but if the line wraps, every line after the first will be indented.
pWrapIndent :: [PPrint ()] -> PPrint ()
pWrapIndent px = do
  st <- get
  let trySt = execState (pInline px) (subprint st)
  case printerOut trySt of
    []   -> appendState trySt
    p:px ->
      let ind (tab, len, str) = (tab+1, len, str)
      in  appendState (trySt{printerOut = p : map ind px, printerTab = printerTab trySt + 1})

-- | Will evaluate a 'PPrint' function to create a block of text, and if the block of text can be
-- fit into a single line, it will be placed inline with the text precceding and succeeding it.
-- If it cannot be placed into a single line, it will be preceeded and succeeded by a 'pEndLine'.
-- Passing 'Prelude.False' as the first parameter means 'pEndLine' will not succeed the block of
-- text, which can come in handy (for example) when you need to follow an item with a closing
-- punctuation mark like a comma or semicolon, and you don't want that closing punctuation on the
-- next line.
pGroup :: Bool -> PPrint () -> PPrint ()
pGroup after fn = do
  st <- get
  let trySt = execState (pEndLine >> fn) (subprint st)
  if charCount trySt > maxWidth st || forcedNewLine trySt
    then  pEndLine >> appendState trySt >> (if after then pEndLine else return ())
    else  appendState (stateJoinLines trySt)

pList :: PPrint () -> String -> String -> String -> [PPrint ()] -> PPrint ()
pList header open separator close px = do
  st <- get
  let mw  = maxWidth st
      cur = printerCol st
      sep = ustr separator
  pGroup False $ do
    header >> pString open >> pEndLine
    pIndent $ pInline $ map (pGroup True) $ mapAlmost (>>(pUStr sep)) px
    pEndLine >> pString close

-- | Like 'pList' but there is no need to pass the first @'PPrint' ()@ header parameter, this
-- parameter is set to @'Prelude.return' ()@.
pList_ :: String -> String -> String -> [PPrint ()] -> PPrint ()
pList_ = pList (return ())

pClosure :: PPrint () -> String -> String -> [PPrint ()] -> PPrint ()
pClosure header open close px = do
  st <- get
  let content = do
        header >> pString open >> pEndLine
        pIndent (sequence_ $ mapAlmost (>>pEndLine) px)
        pEndLine >> pString close
      trySt = execState content (subprint st)
  if charCount trySt + printerCol st > maxWidth st then pEndLine else return ()
  appendState trySt

----------------------------------------------------------------------------------------------------

type PPrint a = State Printer a
type POutput = (Int, Int, UStr)

-- not for export
data Printer
  = Printer
    { printerTab    :: Int -- how many indentation marks should preceed this line
    , printerCol    :: Int -- how many non-indentation characters are in the buffer
    , printerOut    :: [POutput] -- all lines before the current line in the buffer
    , printerBuf    :: String -- buffers the current line
    , nextTab       :: Int
    , lineCount     :: Int -- how many lines have been printed
    , charCount     :: Int -- how many characters have been printed
    , maxWidth      :: Int
    , forcedNewLine :: Bool
    }

initPrinter :: Int -> Printer
initPrinter width =
  Printer
  { printerTab    = 0
  , printerCol    = 0
  , printerOut    = []
  , printerBuf    = ""
  , maxWidth      = width
  , lineCount     = 0
  , charCount     = 0
  , nextTab       = 0
  , forcedNewLine = False
  }

printerOutputTripple :: Printer -> (Int, Int, UStr)
printerOutputTripple st = (printerTab st, printerCol st, ustr (printerBuf st))

instance Monoid Printer where
  mempty = initPrinter 80
  mappend origSt st = case printerOut st of
    []                ->
      (combine origSt st)
      { printerBuf = printerBuf origSt ++ printerBuf st
      , printerCol = printerCol origSt +  printerCol st
      }
    (_, col, buf):out ->
      (combine origSt st)
      { printerOut = printerOut origSt ++
          (printerTab origSt, printerCol origSt + col, ustr (printerBuf origSt ++ uchars buf)) : out
      , printerBuf = printerBuf st
      , printerCol = printerCol st
      }
    where
      combine origSt st = 
        origSt
        { charCount     = charCount origSt + charCount st
        , lineCount     = lineCount origSt + lineCount st
        , maxWidth      = maxWidth  origSt
        , printerTab    = printerTab st
        , nextTab       = nextTab st
        , forcedNewLine = forcedNewLine origSt || forcedNewLine st
        }

-- | Force a string into the 'printerBuf' buffer without modifying anything else. This should allow
-- you to put markers into the output without effecting any of the metrics used to control how the
-- output is indented or wrapped.
pDebug :: (Printer -> String) -> PPrint ()
pDebug fn = do
  st <- get
  let msg = "["++fn st++"]"
  put (st{printerBuf=printerBuf st ++ seq msg msg})

stateJoinLines :: Printer -> Printer
stateJoinLines st =
  st{printerBuf = str ++ printerBuf st, printerCol = len + printerCol st, printerOut=[]} where
    (len, str) = foldl joinln (0, "") (printerOut st)
    joinln (len0, str0) (_, len1, str1) = (len0+len1, str0 ++ uchars str1)

appendState :: Printer -> PPrint ()
appendState = modify . flip mappend

-- | A kind of pre-conversion, the 'PPrintState' is broken into a list of strings, each string
-- preceeded by it's indentation factor.
linesFromPPrintState :: Int -> PPrint () -> [(Int, String)]
linesFromPPrintState maxWidth ps = end (execState ps mempty) where
  end st = flip map (printerOut st ++ [printerOutputTripple st]) $ \ (a, _, b) ->
    (a, dropWhile isSpace (chomp (uchars b)))

printAcross :: [PPrint ()] -> PPrint ()
printAcross px = case px of
  []   -> return ()
  p:px -> do
    st <- get
    st <- return (st{printerBuf = printerBuf st})
    let trySt = execState p (subprint st)
    if withinMaxWidth st trySt
      then put (mappend st trySt)
      else pEndLine >> modify (\st -> mappend st trySt)
    printAcross px

withinMaxWidth :: Printer -> Printer -> Bool
withinMaxWidth st trySt = null (printerOut trySt) && printerCol st + printerCol trySt <= maxWidth st

subprint :: Printer -> Printer
subprint st = st{printerBuf="", printerCol=0, printerOut=[], charCount=0, lineCount=0}

tabAll :: Bool -> [POutput] -> [POutput]
tabAll alsoTabFinalLine ax = case ax of
  []                 -> []
  [(tab, len, str)]  -> if alsoTabFinalLine then [(tab+1, len, str)] else [(tab, len, str)]
  (tab, len, str):ax -> (tab+1, len, str) : tabAll alsoTabFinalLine ax

-- | Given a list of strings, each prefixed with an indentation count, and an indentation string,
-- concatenate all strings into a one big string, with each string being indented and on it's own
-- line.
linesToString :: String -> [(Int, String)] -> String
linesToString indentStr = intercalate "\n" .
  map (\ (indentCount, content) -> concat (replicate indentCount indentStr) ++ content)

-- Given an indentation string and a maximum width value, construct a string from the 'PPrintState'.
-- The maximum width value is used to call 'linesFromPPrintState', and the indentation string is
-- used to call 'linesToString'.
showPPrint :: Int -> String -> PPrint () -> String
showPPrint maxWidth indentStr ps = linesToString indentStr (linesFromPPrintState maxWidth ps)

