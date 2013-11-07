-- "tests/Dao/SingleTest.hs" a simple program that runs only the random
-- tests generated by the integer seeds specified on the command line.
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

module Dao.SingleTest where

import           Dao.Prelude
import           Dao.Binary
import           Dao.Test
import           Dao.Object
import           Dao.Object.Parser
import           System.Environment
import qualified Data.ByteString.Lazy as Z

type RandGen   a = Int -> IO (Dao a)
type BinUnpack a = Dao Z.ByteString -> IO (Dao a)
type ParsePP   a = String -> IO (Dao a)

whichTests :: [Int -> IO ()]
whichTests = [testObjExpr]

singleTestMain = do
  i <- getArgs >>=
    mapM (\i -> case readsPrec 0 i of
        [(i, "")] -> mapM_ ($ i) whichTests
        []        -> fail $ "Unknown command-line argument: "++show i++'\n':errmsg
      )
  case i of
    [] -> fail errmsg
    _  -> return ()

objectExpr :: Int -> IO ()
objectExpr = fromRand (ioRandO :: RandGen ObjectExpr) (fromBinary :: BinUnpack ObjectExpr) (fmap interm equation) where
  interm a = case toInterm a of
    [o] -> o
    _   -> error ("could not convert AST to intermediate:\n"++prettyShow a)

testObjExpr :: Int -> IO ()
testObjExpr = fromRand (ioRandO :: RandGen RandObj) (fromBinary :: BinUnpack RandObj) (fmap RandTopLevel toplevel)

fromRand :: (Eq a, Binary a MethodTable, PPrintable a, HasLocation a, Show a) => RandGen a -> BinUnpack a -> DaoParser a -> Int -> IO ()
fromRand gen unpack pars i = do
  a <- gen i
  putStrLn ("Generated test expr #"++show i)
  let ppa = prettyShow (daoUnwrap a)
  putStrLn ppa
  print (daoUnwrap a)
  b <- toBinary a
  putStrLn ("As binary string:")
  cast Base16String b >>= pp
  putStrLn ("Decoded from binary string:")
  c <- unpack b
  pp c
  putStrLn $
    if c==a
      then "SUCCESS: decoded object matches original test object"
      else "FAIL: decoded object does not match original test object"
  putStr "Parsing pretty-printed representation..."
  d <- fmap delLocation <$> parseWith pars (Dao ppa)
  putStrLn "OK. Parsed object is:"
  pp d
  print (daoUnwrap d)
  putStrLn $
    if a==d
      then "SUCCESS: parsed object matches original test object"
      else "FAIL: parsed object does not match original test object"

errmsg = "please specify the test IDs you want to run as command-line arguments to this program"
