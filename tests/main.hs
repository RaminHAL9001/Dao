-- "tests/pprint.hs"  tests the 'Dao.PPrint' module.
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

module Main where

import           RandObj

import           Dao.String
import           Dao.Predicate
import           Dao.PPrint
import           Dao.Token
import           Dao.Parser
import           Dao.Object
import           Dao.Object.Parser
import           Dao.Object.Binary
import           Dao.Object.Show

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.State

import           Data.List
import           Data.Monoid
import           Data.Binary
import qualified Data.Binary          as B
import qualified Data.ByteString.Lazy as B

import           System.IO

----------------------------------------------------------------------------------------------------

el = EndlineComment . ustr
il = InlineComment  . ustr
lu = LocationUnknown
a  = Literal (ORef (LocalRef (ustr "a"))) lu
i0 = Literal (OInt 0) lu
i1 = Literal (OInt 1) lu
i2 = Literal (OInt 2) lu
i3 = Literal (OInt 3) lu
add   = Com ADD
eqeq  = Com EQUL
modu  = Com MOD
mult  = Com MULT
diveq = Com UDIV
eq    = Com UCONST
evalObj expr = EvalObject expr [] lu

ifExpr :: Int -> ScriptExpr
ifExpr i =
  IfThenElse
    []
    (ParenExpr True (Com (Equation (Equation a modu i2 lu) eqeq i0 lu)) lu)
    (Com $
      [ ComBefore [el " if the number is even, divide by two"] $
          evalObj (AssignExpr a diveq i2 lu)
      ])
    (Com $
      [ ComBefore [el " if the number is odd, multiply by three and add one"] $
          evalObj (AssignExpr a eq (Equation (Equation a mult i3 lu) add i1 lu) lu)
      ] ++ if i<=0 then [] else [ComBefore [el " then test it again"] (ifExpr (i-1))]
    )
    LocationUnknown

mainIfExpr = showPPrint 80 "    " (pPrint (ifExpr 3))

testPrinter item = mapM_ fn [20, 80, 120] where
  fn maxWidth = putStrLn $ showPPrint maxWidth "    " item

samples = [pString "Hello, world! ", pString "Spitting out the daemons. ", pString "Good times. "]

testClosure = testPrinter $ pClosure (pString "begin ") "{ " "}" $ samples

testList = testPrinter $ pList (pString "list ") "{ " ", " " }" $
  map pString (words "this is something to test the pList function each word in this list is treated as a list item")

testInline = testPrinter $ pInline $ intercalate [pString " + "] $ map return $ concat $
  [ map pString (words "testing the pInline function")
  , [pClosure (pString "innerClosure() ") "{ " "}" samples]
  , map pString (words "after the closure more words exist")
  ]

-- test the pretty printer
simpleTest = testList >> testInline >> testClosure >> putStrLn mainIfExpr

----------------------------------------------------------------------------------------------------

maxRecurseDepth = 6

randObj :: Int -> Object
randObj = genRand maxRecurseDepth

-- | Generate a single random item from a seed value and print this item to stdout. Also returns the
-- item generated so if you run this from GHCI, you can make use of the derived "Show" function to
-- see what the object looks like as a data Haskell structure.
runItem i = do
  putStrLn ("//"++show i)
  let obj = randObj i
  putStr (prettyPrint 80 "    " obj)
  return obj

specify = Nothing
testAllItems = [0..1000]

-- | Simply generate several random objects using 'randObj'
randTest = case specify of
  Nothing -> mapM_ runItem testAllItems
  Just  i -> void $ runItem i

----------------------------------------------------------------------------------------------------

pPrintComScriptExpr :: [Com ScriptExpr] -> PPrint ()
pPrintComScriptExpr = pPrintSubBlock (return ())

-- | Test the pretty printer and the parser. If a randomly generated object can be pretty printed,
-- and the parser can parse the pretty-printed string and create the exact same object, then the
-- test pases.
testEveryParsePPrint :: MVar Handle -> MVar Int -> Chan (Maybe Int) -> IO ()
testEveryParsePPrint hlock counter ch = handle h loop where
  h (SomeException e) = do
    putMVar counter 1 >>= evaluate
    print e
  loop = do
    i <- readChan ch
    case i of
      Nothing -> return ()
      Just  i -> do
        let obexp = genRandWith randO maxRecurseDepth i :: ObjectExpr
            -- bytes = B.encode obexp
            -- obj   = B.decode bytes
            str   = seq obexp $! showPPrint 80 "    " (pPrint obexp)
            (par, msg) = seq str $! runParser (fmap fst parseObjectExpr) str 
            err reason = do
              modifyMVar_ hlock $ \h -> do
                hPutStrLn h $! concat $!
                  [ "ITEM #", show i, " ", reason
                  , if null msg then "." else '\n':msg
                  , "\n", str
                  , "\n", show obexp
                  , "\n--------------------------------------------------------------------------\n"
                  ]
                return h
              putMVar counter 1 >>= evaluate
        -- if seq obexp $! seq bytes $! obj/=obexp
          -- then  err "Binary deserialization does not match source object >>= evaluate"
          -- else
        case seq par $! seq msg $! par of
                  OK      _ -> loop
                  Backtrack -> err "Ambiguous parse" >>= evaluate
                  PFail _ b -> err ("Parse failed, "++uchars b) >>= evaluate

----------------------------------------------------------------------------------------------------

threadCount = 8

main = do
  ch      <- newChan
  counter <- newEmptyMVar
  h       <- openFile "./debug.log" ReadWriteMode
  hlock   <- newMVar h
  hwait   <- newMVar True
  let ctrlLoop count = do
        c <- takeMVar counter
        let nextCount = count-c
        if nextCount>0 then ctrlLoop nextCount else modifyMVar_ hwait (return . const False)
      iterLoop i = do
        continue <- readMVar hwait
        if continue then writeChan ch (Just i) else writeChan ch Nothing
        iterLoop (i+1)
  workThreads <- replicateM threadCount $ forkOS $ do
    testEveryParsePPrint hlock counter ch
  iterThread <- forkIO (iterLoop 0)
  ctrlLoop threadCount
  mapM_ killThread (iterThread:workThreads)
  hClose h

