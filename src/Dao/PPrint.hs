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

-- | The intermediate printer structure, the 'PPrint' monad generates a structure using this data
-- type, the pretty printer uses these types to make decisions on how to output information. This
-- structure contains the basic printing strategies involving how to wrap lines, indent, and place
-- lines of code in a block.
data PPrintItem
  = PNewLine -- ^ force a new line
  | PEndLine -- ^ place a new line unless we are already on a new line.
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

instance Show PPrintItem where
  show p = case p of
    PNewLine                         -> "newline"
    PPrintString  p                  -> show p
    PPrintInline  px                 -> "inline "++show px
    PPrintClosure hdr opn     clo px -> concat ["closure ", show opn, show clo, " ", show hdr]
    PPrintList    hdr opn sep clo px -> concat $
      ["list ", show opn, " ", show sep, " ", show clo, " ", show hdr, " ", show px]

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
  mappend a b = PPrintState{ pPrintStack = pPrintStack a ++ pPrintStack b }

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

-- | Place a new line unless we are already on a new line.
pEndLine :: PPrint ()
pEndLine = pPush PEndLine

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

-- | Like 'pList' but there is no need to pass the first @'PPrint' ()@ header parameter, this
-- parameter is set to @'Prelude.return' ()@.
pList_ :: String -> String -> String -> PPrint () -> PPrint ()
pList_ = pList (return ())

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
    , forcedNewLine :: Bool -- set if a newline is forced, like if an end-line comment was printed.
    , inliningNow :: Bool -- set if we are inside of a 'PPrintInline' block.
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

initPrinter :: Printer
initPrinter =
  Printer
  { printerTab = 0
  , printerCol = 0
  , printerOut = []
  , printerBuf = ""
  , inliningNow = False
  , forcedNewLine = False
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
      , inliningNow = inliningNow origSt || inliningNow st
      , forcedNewLine = forcedNewLine origSt || forcedNewLine st
      }
    (_, col, buf):out ->
      st{ printerOut = printerOut origSt ++
            (printerTab origSt, printerCol origSt + col, printerBuf origSt ++ buf) : out
        , inliningNow = inliningNow origSt || inliningNow st
        , forcedNewLine = forcedNewLine origSt || forcedNewLine st
        }

-- | A kind of pre-conversion, the 'PPrintState' is broken into a list of strings, each string
-- preceeded by it's indentation factor.
linesFromPPrintState :: Int -> PPrintState -> [(Int, String)]
linesFromPPrintState maxWidth ps = end (execState (mapM_ prin (pPrintStack ps)) mempty) where
  prin :: PPrintItem -> State Printer ()
  prin p = case p of
    PNewLine        -> newline >> modify (\st -> st{forcedNewLine=True})
    PEndLine        -> noDupNewLine >> modify (\st -> st{forcedNewLine=True})
    PPrintString p  -> ustring p
    PPrintList   hdr opn sep clo px -> do
      st <- get
      let curLen  = printerCol st
          curBuf  = printerBuf st
          hdrSt   = execState (mapM_ prin (hdr++[PPrintString opn])) $
            st{printerBuf="", printerCol=0, printerOut=[]}
          hdrLen  = printerCol hdrSt
          hdrBuf  = printerBuf hdrSt
          content = printAcross (mapAlmost (PPrintInline . (:[PPrintString sep])) px)
          lstSt   = execState (content >> ustring clo) (subprint st)
          lstLen  = printerCol lstSt
          lstBuf  = printerBuf lstSt
          toolong = noDupNewLine >> mapM_ prin (hdr++[PPrintString opn]) >>
            noDupNewLine >> indent (content >> noDupNewLine) >> ustring clo
      if null (printerOut hdrSt)
        then  let hdrLen = printerCol st
              in  if null (printerOut lstSt)
                    then  if curLen + hdrLen + lstLen < maxWidth
                            then  put $ st{ printerBuf = curBuf ++ hdrBuf ++ lstBuf
                                          , printerCol = curLen + hdrLen + lstLen
                                          }
                            else  if hdrLen + lstLen < maxWidth
                                    then do
                                      noDupNewLine
                                      put $ st{ printerBuf = hdrBuf ++ lstBuf
                                              , printerCol = hdrLen + lstLen
                                              }
                                    else  toolong
                    else  toolong
        else  toolong
      st <- get
      if inliningNow st then return () else noDupNewLine
    PPrintInline px -> do
      st <- get
      let trySt = execState (printAcross px) ((subprint st){inliningNow = True})
          out = filter (\ (_, _, str) -> not (null str)) (printerOut trySt)
      put (st{ forcedNewLine = forcedNewLine st || forcedNewLine trySt})
      st <- get
      if null out
        then  if printerCol trySt + printerCol st <= maxWidth
                then  put $ st{ printerCol = printerCol st + printerCol trySt
                              , printerBuf = printerBuf st ++ printerBuf trySt
                              }
                else  put $ st{ printerOut = printerOut st ++ [printerOutputTripple st]
                              , printerCol = printerCol trySt
                              , printerBuf = printerBuf trySt
                              }
        else do
          let line@(ind, strlen, str) = head out
              combined = (printerTab st, printerCol st + strlen, printerBuf st ++ str)
          if strlen + printerCol st < maxWidth
            then  put $ st{ printerOut = printerOut st
                              ++ combined : tabAll True (tail out ++ [printerOutputTripple trySt])
                          }
            else  put $ st{ printerOut = printerOut st ++ line : tabAll True out }
          noDupNewLine
    PPrintClosure hdr opn clo px -> do
      st <- get
      noDupNewLine
      let content = do
            printAcross (hdr++[PPrintString opn])
            noDupNewLine
            indent (mapM_ (\p -> prin p >> noDupNewLine) px)
            ustring clo
            noDupNewLine
          trySt = execState content (subprint st)
          loop col buf ax = case ax of
            _  | col>maxWidth   -> content
            (_, strlen, str):ax -> loop (col+strlen) (buf++str) ax
            []                  -> do
              modify (\st -> st{ printerOut = printerOut st ++ [(printerTab st, col, buf)] })
              noDupNewLine
      if forcedNewLine trySt then modify (flip mappend trySt) else loop 0 "" (printerOut trySt)
  ustring p = modify $ \st ->
    st{ printerCol = printerCol st + ulength p
      , printerBuf = printerBuf st ++ uchars p
      }
  newline = modify $ \st ->
    st{ printerCol = 0
      , printerBuf = ""
      , printerOut = printerOut st ++ [printerOutputTripple st]
      }
  indent indentedPrinter = do
    tab <- gets printerTab
    modify (\st -> st{printerTab = tab+1})
    indentedPrinter
    modify (\st -> st{printerTab = tab})
  subprint st = st{printerBuf="", printerCol=0, printerOut=[]}
  printAcross px = case px of
    []   -> return ()
    p:px -> do
      st <- get
      let trySt = execState (prin p) (subprint st)
      if sumRunsOver st trySt
        then put (mappend st trySt)
        else noDupNewLine >> modify (\st -> mappend st trySt)
      printAcross px
  tryInline fn = do
    st <- get
    let trySt = execState fn (st{printerOut=[]})
        done = put $
          st{ printerBuf = ""
            , printerCol = 0
            , printerOut = printerOut st ++
                printerOutputTripple st : printerOut trySt ++
                  [printerOutputTripple trySt]
            }
        loop len buf ax = case ax of
          _  | len > maxWidth -> done
          []                  -> do
            put $ st{ printerBuf = ""
                    , printerCol = 0
                    , printerOut = printerOut st ++
                        [(printerTab st, printerCol st + len, printerBuf st ++ buf)]
                    }
          (i, strlen, str):ax -> loop (len+strlen) (buf++str) ax
    if forcedNewLine st then done else loop (printerCol st) (printerBuf st) (printerOut trySt)
  tabAll alsoTabFinalLine ax = case ax of
    []                 -> []
    [(tab, len, str)]  -> if alsoTabFinalLine then [(tab+1, len, str)] else [(tab, len, str)]
    (tab, len, str):ax -> (tab+1, len, str) : tabAll alsoTabFinalLine ax
  sumRunsOver st trySt = null (printerOut trySt) && printerCol st + printerCol trySt <= maxWidth
  noDupNewLine = gets printerBuf >>= \buf -> if and (map isSpace buf) then return () else newline
  end st = flip map (printerOut st ++ [printerOutputTripple st]) $ \ (a, _, b) ->
    (a, dropWhile isSpace (chomp b))

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

