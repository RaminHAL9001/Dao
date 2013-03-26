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
-- instantiates 'PPrintable', and a maximum width value, and a tab string, and will convert that
-- value to a 'Prelude.String'.
prettyPrint :: PPrintable a => Int -> String -> a -> String
prettyPrint maxWidth tab = showPPrintState maxWidth tab . pPrint

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

-- | Force a new line
pNewLine :: PPrint ()
pNewLine = modify $ \st ->
  st{ printerCol = 0
    , printerBuf = ""
    , printerOut = printerOut st ++ [printerOutputTripple st]
    , forcedNewLine = True
    , lineCount = lineCount st + 1
    }

-- | Place a new line unless we are already on a new line.
pEndLine :: PPrint ()
pEndLine = do
  gets printerCol >>= \col -> if col==0 then return () else pNewLine
  modify (\st -> st{forcedNewLine=True})

pIndent :: PPrint () -> PPrint ()
pIndent indentedPrinter = do
  tab <- gets printerTab
  modify (\st -> st{printerTab = tab+1})
  indentedPrinter
  modify (\st -> st{printerTab = tab})

instance PPrintable UStr where { pPrint = pUStr }

-- not for export
appendString :: Int -> String -> PPrint ()
appendString len str = modify $ \st ->
  st{ printerCol = printerCol st + len
    , printerBuf = printerBuf st ++ str
    , charCount  = charCount  st + fromIntegral len
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

-- not for export
printInline :: Bool -> [PPrint ()] -> PPrint ()
printInline doIndent px = do
  st <- get
  let nonewline = inliningNow st
      trySt = execState (printAcross px) ((subprint st){inliningNow=True})
      out = filter (\ (_, _, str) -> not (null str)) (printerOut trySt)
  put (st{forcedNewLine = forcedNewLine st || forcedNewLine trySt})
  st <- get
  if null out
    then  if printerCol trySt + printerCol st <= maxWidth st
            then  put (mappend st trySt)
            else  pEndLine >> modify (flip mappend trySt)
    else do
      let line@(ind, strlen, str) = head out
          combined = (printerTab st, printerCol st + strlen, printerBuf st ++ str)
          pIndent = if doIndent then tabAll True else id
      if strlen + printerCol st < maxWidth st
        then  put $ st{ printerOut = printerOut st ++
                          combined : pIndent (tail out ++ [printerOutputTripple trySt])
                      }
        else  put $ st{ printerOut = printerOut st ++ line : pIndent (tail out)
                      , printerBuf = printerBuf trySt
                      }

-- | Evaluate the 'PPrint' printer, and every line of output will be used as an item in a list and
-- printed across a line, wrapping on to the next line if the line goes past the width limit.
pInline :: [PPrint ()] -> PPrint ()
pInline = printInline False

-- | Like 'pInline' but if the line wraps, every line after the first will be indented.
pWrapIndent :: [PPrint ()] -> PPrint ()
pWrapIndent = printInline True

-- | Inlines items without wrapping, newlines are always ignored. Only to be used if you must NOT
-- have a line break between multiple items.
pNoWrap :: [PPrint ()] -> PPrint ()
pNoWrap px = do
  modify (\st -> st{inliningNow=True})
  sequence_ px
  modify (\st -> st{inliningNow=False})

-- | Like 'pInline' but places a separator string between each item.
pList :: PPrint () -> String -> String -> String -> [PPrint ()] -> PPrint ()
pList hdr open separator close px = do
  st <- get
  let opn      = ustr open
      clo      = ustr close
      sep      = ustr separator
      noNewline = inliningNow st
      curLen  = printerCol st
      curBuf  = printerBuf st
      hdrSt   = execState (hdr >> pUStr opn) ((subprint st){inliningNow=True})
      hdrLen  = printerCol hdrSt
      hdrBuf  = printerBuf hdrSt
      content = printAcross (mapAlmost (\p -> p >> pUStr sep) px)
      lstSt   = execState (content >> pUStr clo) (subprint st)
      lstLen  = printerCol lstSt
      lstBuf  = printerBuf lstSt
      toolong = pEndLine >> hdr >> pUStr opn >>
        pEndLine >> pIndent (content >> pEndLine) >> pUStr clo
  if null (printerOut hdrSt)
    then  let hdrLen = printerCol st
          in  if null (printerOut lstSt)
                then  if curLen + hdrLen + lstLen < (maxWidth st)
                        then  put $ st{ printerBuf = curBuf ++ hdrBuf ++ lstBuf
                                      , printerCol = curLen + hdrLen + lstLen
                                      }
                        else  if hdrLen + lstLen < (maxWidth st)
                                then do
                                  pEndLine
                                  put $ st{ printerBuf = hdrBuf ++ lstBuf
                                          , printerCol = hdrLen + lstLen
                                          }
                                else  toolong
                else  toolong
    else  toolong
  if noNewline then return () else pEndLine

-- | Like 'pList' but there is no need to pass the first @'PPrint' ()@ header parameter, this
-- parameter is set to @'Prelude.return' ()@.
pList_ :: String -> String -> String -> [PPrint ()] -> PPrint ()
pList_ = pList (return ())

pClosure :: PPrint () -> String -> String -> [PPrint ()] -> PPrint ()
pClosure hdr open close px = do
  st <- get
  let opn = ustr open
      clo = ustr close
      nonewline = inliningNow st
      content = do
        printAcross [hdr, pUStr opn]
        pEndLine
        pIndent (mapM_ (\p -> p >> pEndLine) px)
        pUStr clo
      trySt = execState content ((subprint st){inliningNow=False})
      loop col buf ax = case ax of
        _  | col>maxWidth st -> content
        (_, strlen, str):ax  -> loop (col+strlen) (buf++str) ax
        []                   -> do
          len <- gets printerCol
          if nonewline && len + ulength clo < maxWidth st
            then  modify $ \st ->
                    st{ printerBuf = buf ++ printerBuf trySt
                      , printerCol = col +  printerCol trySt
                      }
            else  modify $ \st ->
                    st{ printerOut = printerOut st ++ [(printerTab st, col, buf)]
                      , printerBuf = printerBuf trySt
                      , printerCol = printerCol trySt
                      }
  if forcedNewLine trySt
    then modify (flip mappend trySt)
    else loop (printerCol st) (printerBuf st) (printerOut trySt)
  if nonewline then return () else pEndLine

----------------------------------------------------------------------------------------------------

type PPrint a = State Printer a
type POutput = (Int, Int, String)

-- not for export
data Printer
  = Printer
    { printerTab    :: Int -- how many indentation marks should preceed this line
    , printerCol    :: Int -- how many non-indentation characters are in the buffer
    , printerOut    :: [POutput] -- all lines before the current line in the buffer
    , maxWidth      :: Int
    , printerBuf    :: String -- buffers the current line
    , forcedNewLine :: Bool -- set if a pNewLine is forced, like if an end-line comment was printed.
    , inliningNow   :: Bool -- set if we are inside of a 'PPrintInline' block.
    , lineCount     :: Word -- how many lines have been printed
    , charCount     :: Word64 -- how many characters have been printed
    }

instance Show Printer where
  show p = concat $
    [ "Printer:\n  tab=", show (printerTab p)
    , ", col=", show (printerCol p)
    , ", buf=", show (printerBuf p)
    , ", flags=", intercalate "|" $
        concat [if forcedNewLine p then ["forcenl"] else [], if inliningNow p then ["inline"] else []]
    , "\n"
    ] ++ (map (\ (num, (tab, _, str)) -> "    "++show num++':':show tab++": "++show str++"\n") $
            take 10 $ reverse $ zip (iterate (+1) 0) (printerOut p))

initPrinter :: Int -> Printer
initPrinter width =
  Printer
  { printerTab    = 0
  , printerCol    = 0
  , printerOut    = []
  , printerBuf    = ""
  , maxWidth      = width
  , inliningNow   = False
  , forcedNewLine = False
  , lineCount     = 0
  , charCount     = 0
  }

printerOutputTripple :: Printer -> (Int, Int, String)
printerOutputTripple st = (printerTab st, printerCol st, printerBuf st)

instance Monoid Printer where
  mempty = initPrinter 80
  mappend origSt st = case printerOut st of
    []                -> (combine origSt st){printerBuf = printerBuf origSt ++ printerBuf st}
    (_, col, buf):out ->
      (combine origSt st)
      { printerOut = printerOut origSt ++
          (printerTab origSt, printerCol origSt + col, printerBuf origSt ++ buf) : out
      , printerBuf    = printerBuf st
      }
    where
      combine origSt st = 
        origSt
        { printerCol    = printerCol origSt +  printerCol st
        , inliningNow   = inliningNow   origSt || inliningNow st
        , forcedNewLine = forcedNewLine origSt || forcedNewLine st
        , charCount     = charCount  origSt +  charCount st
        , lineCount     = lineCount  origSt +  lineCount st
        , maxWidth      = maxWidth origSt
        }

-- | A kind of pre-conversion, the 'PPrintState' is broken into a list of strings, each string
-- preceeded by it's indentation factor.
linesFromPPrintState :: Int -> PPrint () -> [(Int, String)]
linesFromPPrintState maxWidth ps = end (execState ps mempty) where
  end st = flip map (printerOut st ++ [printerOutputTripple st]) $ \ (a, _, b) ->
    (a, dropWhile isSpace (chomp b))

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
linesToString indentStr = concatMap $ \ (indentCount, string) ->
  concat (replicate indentCount indentStr) ++ string ++ "\n"

-- Given an indentation string and a maximum width value, construct a string from the 'PPrintState'.
-- The maximum width value is used to call 'linesFromPPrintState', and the indentation string is
-- used to call 'linesToString'.
showPPrintState :: Int -> String -> PPrint () -> String
showPPrintState maxWidth indentStr ps = linesToString indentStr (linesFromPPrintState maxWidth ps)

