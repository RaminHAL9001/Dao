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

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Dao.String
import           Dao.Predicate
import           Dao.PPrint
import           Dao.Token
import           Dao.Parser
import           Dao.Struct
import           Dao.Random
import           Dao.Object
import           Dao.Object.AST
import           Dao.Object.Parser
import           Dao.Object.Binary
import           Dao.Object.PPrint
import           Dao.Object.Struct
import           Dao.Object.Random
import           Dao.Object.DeepSeq

import           Control.Concurrent
import           Control.Exception
import           Control.DeepSeq
import           Control.Monad.State

import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.IORef
import           Data.Binary
import qualified Data.Binary          as B
import qualified Data.ByteString.Lazy as B

import           Text.Printf

import           Numeric

import           System.IO
import           System.Environment

----------------------------------------------------------------------------------------------------

el = EndlineComment . ustr
il = InlineComment  . ustr
lu = LocationUnknown
a  = AST_Literal (ORef (LocalRef (ustr "a"))) lu
i0 = AST_Literal (OInt 0) lu
i1 = AST_Literal (OInt 1) lu
i2 = AST_Literal (OInt 2) lu
i3 = AST_Literal (OInt 3) lu
add   = Com ADD
eqeq  = Com EQUL
modu  = Com MOD
mult  = Com MULT
diveq = Com UDIV
eq    = Com UCONST
evalObj expr = AST_EvalObject expr [] lu

ifExpr :: Int -> AST_Script
ifExpr i =
  AST_IfThenElse
    []
    (AST_Paren True (Com (AST_Equation (AST_Equation a modu i2 lu) eqeq i0 lu)) lu)
    (Com $
      [ ComBefore [el " if the number is even, divide by two"] $
          evalObj (AST_Assign a diveq i2 lu)
      ])
    (Com $
      [ ComBefore [el " if the number is odd, multiply by three and add one"] $
          evalObj (AST_Assign a eq (AST_Equation (AST_Equation a mult i3 lu) add i1 lu) lu)
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

showBinary :: B.ByteString -> String
showBinary b =
  intercalate " " $ map (\b -> (if b<0x10 then ('0':) else id) (flip showHex "" b)) $ B.unpack b

maxRecurseDepth = 5

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

errHandler :: String -> [Handler ()]
errHandler msg = 
  [ Handler (\ (e::IOException) -> hPutStrLn stderr (msg++": "++show e))
  , Handler (\ (e::ErrorCall) -> hPutStrLn stderr (msg++": "++show e))
  ]

pPrintComScriptExpr :: [Com AST_Script] -> PPrint ()
pPrintComScriptExpr = pPrintSubBlock (return ())

-- | Test the pretty printer and the parser. If a randomly generated object can be pretty printed,
-- and the parser can parse the pretty-printed string and create the exact same object, then the
-- test pases.
testEveryParsePPrint :: MVar Bool -> MVar (Handle, HandlePosn) -> MVar Bool -> MVar (Maybe Int) -> IO ()
testEveryParsePPrint hwait hlock notify ch = newIORef (0-1, undefined) >>= topLoop where
  topLoop ref = do
    handle (h ref) (loop ref)
    contin <- readMVar hwait
    if contin then topLoop ref else return ()
  h ref (SomeException e) = do
    putMVar notify False >>= evaluate
    (i, obj) <- readIORef ref
    let fileName = "uncaught.log"
    catches
      (do logFile <- openFile fileName AppendMode
          catches
            (do hPutStrLn logFile ("ITEM #"++show i++"\n"++show e)
                hPutStrLn logFile (concat [prettyPrint 80 "    " obj, "\n", sep]) >>= evaluate
                hFlush logFile
            )
            (errHandler "uncaught exception")
          hClose logFile
      )
      (errHandler ("while writing "++show fileName))
  loop ref = do
    i <- takeMVar ch
    case i of
      Nothing -> return ()
      Just  i -> do
        modifyMVar_ hlock $ \ (handl, pos) -> do -- using an mvar to prevent race conditions on output to this file
          handle (\ (e::IOException) -> hPrint stderr e) $ do
            hSetPosn pos >>= evaluate
            hPutStrLn handl (show i++"               ") >>= evaluate
          return (handl, pos)
        let obexp  = {-# SCC obexp  #-} genRandWith randO maxRecurseDepth i :: AST_TopLevel
            binexp = {-# SCC binexp #-} toInterm obexp :: [TopLevelExpr]
            tree   = {-# SCC tree   #-} dataToStruct obexp
        deepseq obexp (return ())
        writeIORef ref (i, obexp)
        let bytes = {-# SCC bytes #-} B.encode binexp
            bin   = {-# SCC bin   #-} B.decode bytes
            str   = {-# SCC str   #-} showPPrint 80 "    " (pPrint obexp)
            untre = {-# SCC untre #-} structToData tree :: PValue UpdateErr AST_TopLevel
            (par, msg) = {-# SCC par #-} runParser (regexMany space >> parseDirective) str 
            err reason = do
              catches
                (do logFile <- openFile (show i++".log") AppendMode
                    catches
                      (do (hPutStrLn logFile $! concat $!
                              [ "ITEM #", show i, " ", reason
                              , if null msg then "." else '\n':msg
                              , "\n", str, "\n", show obexp, "\n"
                              , showBinary bytes, "\n", sep
                              ]) >>= evaluate
                          hFlush logFile
                      )
                      (errHandler "while writing report")
                    hClose logFile
                )
                (errHandler "exception during test")
        status1 <- handle (\ (ErrorCall e) -> err ("Construction failed: "++show e) >> return False) $ do
          case untre of
            OK      o ->
              if seq o $! o/=obexp
                then  err "Construction does not match source object" >> return False
                else  return True
            Backtrack -> err "Ambiguous construction" >> return False
            PFail _ b -> err ("Construction failed, "++uchars b) >> return False
        status2 <- handle (\ (ErrorCall e) -> err ("Binary decoding failed: "++show e) >> return False) $ do
          if seq obexp $! seq bytes $! bin/=binexp
            then  err "Binary deserialization does not match source object" >> return False
            else  return True
        status3 <- handle (\ (ErrorCall e) -> err ("Parsing failed: "++show e) >> return False) $ do
          case par of
            OK      o -> seq o $! return True
            Backtrack -> err "Ambiguous parse" >> return False
            PFail _ b -> err ("Parse failed, "++uchars b) >> return False
        putMVar notify (status1&&status2&&status3)
        yield >> loop ref
  sep = "--------------------------------------------------------------------------"

----------------------------------------------------------------------------------------------------

threadCount :: Int
threadCount = 3

maxErrors :: Int
maxErrors = 200

displayInterval :: Int
displayInterval = 4000000

main = do
  args    <- getArgs
  i <- return $ case args of
    i:_ -> read i
    []  -> 0
  ch      <- newEmptyMVar
  notify  <- newEmptyMVar
  counter <- newMVar (0, 0)
  h       <- openFile "./testid" ReadWriteMode
  pos     <- hGetPosn h
  hlock   <- newMVar (h, pos)
  hwait   <- newMVar True
  let ctrlLoop count threads = do
        passed <- takeMVar notify
        let nextCount = if not passed then count-1 else count
        modifyMVar_ counter (\ (total, count) -> return (total+1, count+1))
        if nextCount>0
          then  ctrlLoop nextCount threads
          else do
            modifyMVar_ hwait (return . const False)
            mapM_ killThread threads
      iterLoop i = do
        continue <- readMVar hwait
        putMVar ch (if continue then Just i else Nothing)
        iterLoop (i+1)
      displayInfo = do
        (total, count) <- modifyMVar counter $ \ (total, count) -> return ((total,0), (total,count))
        putStrLn $ concat $
          [ show total, " tests completed at the rate of "
          , printf "%.2f tests per second"
              (fromRational (toRational count / (toRational displayInterval / 1000000)) :: Float)
          ]
      infoLoop = threadDelay displayInterval >> displayInfo >> infoLoop
  workThreads <- replicateM threadCount $ forkIO $ do
    testEveryParsePPrint hwait hlock notify ch
  iterThread <- forkOS (iterLoop i)
  dispThread <- forkIO infoLoop
  ctrlLoop maxErrors (iterThread:dispThread:workThreads)
  hClose h
  displayInfo

