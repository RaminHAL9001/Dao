-- "src/Dao/Builtins.hs"  the built-in functions that provide the
-- most basic functionality to the Dao scripting language.
-- 
-- Copyright (C) 2008-2012  Ramin Honary.
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


-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Builtins where

-- | This module contains the most fundamental functions that are built-into the Dao language,
-- particularly the unary and binary operators, like arithmetic @(+) (-) (*) (/) (%)@, and other
-- essentials like @print@, @join@, and @do@.

import           Dao.Debug.OFF
import           Dao.Types
import qualified Dao.Tree as T
import           Dao.Pattern
import           Dao.Predicate
import           Dao.Document
import           Dao.Files
import           Dao.Combination
import           Dao.Parser
import           Dao.Evaluator
import           Dao.Tasks

import           Dao.Object.Monad
import           Dao.Object.Data
import           Dao.Object.Show
import           Dao.Object.Binary
import           Dao.Object.Parsers

import           Control.Exception
import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Control.Monad.Reader

import           Data.Maybe (fromMaybe)
import           Data.Either
import           Data.Array.IArray
import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.List
import           Data.Complex
import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.IntMap as I
import qualified Data.ByteString.Lazy.UTF8 as U

--debug: for use with "trace"
import Debug.Trace

----------------------------------------------------------------------------------------------------

listBreakup :: Eq a => [a] -> [a] -> [Either [a] [a]]
listBreakup str substr = if null substr then [Left str] else loop [] [] (length str) str where
  min = length substr
  loop got rx remlen str =
    if remlen < min
      then got++[Left str]
      else
        case stripPrefix substr str of
          Nothing  -> loop got (rx++[head str]) (remlen-1) (tail str)
          Just str -> loop (got++[Left rx, Right substr]) [] (remlen-min) str

-- | Is simply @diff (union a b) b@ is a set/dict/intmap, pass the relevant @diff@ and @union@
-- function.
setwiseXOR :: (a -> a -> a) -> (a -> a -> a) -> a -> a -> a
setwiseXOR diff union a b = diff (union a b) b

----------------------------------------------------------------------------------------------------

-- | When constructing 'CheckFunc' built-in functions, arguments are passed by reference. To
-- de-reference arugments while simply returning constants, evaluate this equation.
derefRefLiteral :: Object -> Check Object
derefRefLiteral obj = case obj of
  ORef ref -> do
    obj <- execScriptToCheck (const Nothing) (execGlobalLookup ref)
    case obj of
      Nothing  -> checkFail "undefined reference" (ORef ref)
      Just obj -> return obj
  obj      -> return obj

----------------------------------------------------------------------------------------------------

-- | These are the fundamental built-in functions, including arithmetic operators.
basicScriptOperations :: M.Map Name CheckFunc
basicScriptOperations = M.fromList funcList where
  func nm a = (ustr nm, CheckFunc{checkFunc = \argv -> return argv >>= a})
  funcList =
    [ func "+" $ \ [a, b] -> do
        let stringOK = checkOK . OString . ustr
        case (a, b) of
          (OString a, OString b) -> stringOK (uchars a++uchars b)
          (OString a, b        ) -> stringOK (uchars a++showObj 0 b)
          (a        , OString b) -> stringOK (showObj 0 a++uchars b)
          (OList   a, OList   b) -> checkOK (OList (a++b))
          (OList   a, OArray  b) -> checkOK (OList (a++elems b))
          (OArray  a, OList   b) -> checkOK (OList (elems a++b))
          (a        , b        ) -> checkNumericOp "+" (+) a b
    , func "-" $ \ [a, b] -> do
        case (a, b) of
          (OSet    a, OSet    b) -> checkOK (OSet (S.difference a b))
          (ODict   a, ODict   b) -> checkOK (ODict (M.difference a b))
          (OIntMap a, OIntMap b) -> checkOK (OIntMap (I.difference a b))
          (a        , b        ) -> checkNumericOp "-" (-) a b
    , func "*" $ \ [a, b] -> do
        (a, b) <- promoteNum "/" a b
        let listInt    lst x = concat  (take (fromIntegral x) (repeat lst))
            oListInt   lst x = checkOK (OList (listInt lst x))
            oStringInt lst x = checkOK (OString (ustr (listInt (uchars lst) x)))
        case (a, b) of
          (OList   a, OInt    b) -> oListInt a b
          (OList   a, OWord   b) -> oListInt a b
          (OInt    a, OList   b) -> oListInt b a
          (OWord   a, OList   b) -> oListInt b a
          (OString a, OInt    b) -> oStringInt a b
          (OString a, OWord   b) -> oStringInt a b
          (OInt    a, OString b) -> oStringInt b a
          (OWord   a, OString b) -> oStringInt b a
          _ -> checkNumericOp "*" (*) a b
    , func "/" $ \ [a, b] -> do
        case (a, b) of
          (OString a, OString b) -> checkOK $ OList $ map OString $
            map (either ustr ustr) $ listBreakup (uchars a) (uchars b)
          (OList   a, OList   b) -> checkOK $ OList $ map (either OList OList) $ listBreakup a b
          _ -> checkBinumericOp "/" div (/) a b
    , func "%" $ \ [a, b] -> do
        let done fn = checkOK . OList . concatMap (either fn (const []))
        case (a, b) of
          (OString a, OString b) ->
            done (\a -> [OString (ustr a)]) (listBreakup (uchars a) (uchars b))
          _ -> do
            checkOK $ case (a, b) of
              (OWord a, OWord b) -> OWord (mod a b)
              (OInt  a, OInt  b) -> OInt  (mod a b)
    , func "**" $ \ [a, b] -> checkBinumericOp "^" (^) (**) a b
    , func "|"  $ \ [a, b] -> checkBitwiseOp "|" (.|.) (S.union) (M.union) (I.union) a b
    , func "&"  $ \ [a, b] -> checkBitwiseOp "&" (.&.) (S.intersection) (M.intersection) (I.intersection) a b
    , func "^"  $ \ [a, b] ->
        checkBitwiseOp "^" (xor)
          (setwiseXOR S.difference S.union)
          (setwiseXOR M.difference M.union)
          (setwiseXOR I.difference I.union) a b
    , func "~"  $ \ [a] -> checkOK $ case a of
        (OInt  a) -> OInt  (complement a)
        (OWord a) -> OWord (complement a)
    , func "!" $ \ [a] -> checkOK $ boolToObj $ not $ objToBool a
    , func "&&" $ \ [a, b] -> checkLogicalOp (&&) a b
    , func "||" $ \ [a, b] -> checkLogicalOp (||) a b
    , func "==" $ \ [a, b] -> checkCompareOp "==" (==) a b
    , func "!=" $ \ [a, b] -> checkCompareOp "!=" (/=) a b
    , func "<=" $ \ [a, b] -> checkCompareOp "<=" (<=) a b
    , func ">=" $ \ [a, b] -> checkCompareOp ">=" (>=) a b
    , func ">"  $ \ [a, b] -> checkCompareOp ">"  (>=) a b
    , func "<"  $ \ [a, b] -> checkCompareOp "<"  (>=) a b
    , func "<<" $ \ [a, b] -> do
        let shift a b = shiftL a (fromIntegral b)
        checkOK $ case (a, b) of
          (OWord   a, OWord   b) -> OWord (shift a b)
          (OInt    a, OInt    b) -> OInt  (shift a b)
          (OWord   a, OInt    b) -> OWord (shift a b)
          (OInt    a, OWord   b) -> OInt  (shift a b)
          (OList   a, b        ) -> OList (a++[b])
          (OSet    a, b        ) -> OSet (S.insert b a)
          (ODict   a, OPair (OString nm, b)) -> ODict (M.insert nm b a)
          (OIntMap a, OPair (OInt    i , b)) -> OIntMap (I.insert (fromIntegral i) b a)
          (OIntMap a, OPair (OWord   i , b)) -> OIntMap (I.insert (fromIntegral i) b a)
          (OTree   a, OPair (ORef   ref, b)) -> OTree (T.insert ref b a)
    , func ">>" $ \ [a, b] -> do
        let shift a b = shiftR a (fromIntegral b)
        checkOK $ case (a, b) of
          (OWord   a, OWord   b) -> OWord (shift a b)
          (OInt    a, OInt    b) -> OInt  (shift a b)
          (OWord   a, OInt    b) -> OWord (shift a b)
          (OInt    a, OWord   b) -> OInt  (shift a b)
          (OList   a, b        ) -> OList (delete b a)
          (OSet    a, b        ) -> OSet  (S.delete b a)
          (ODict   a, OString b) -> ODict (M.delete b a)
          (OIntMap a, OInt    b) -> OIntMap (I.delete (fromIntegral b) a)
          (OIntMap a, OWord   b) -> OIntMap (I.delete (fromIntegral b) a)
          (OTree   a, ORef    b) -> OTree (T.delete b a)
    , func "@"  $ \ [a] -> derefRefLiteral a >>= checkOK
    , func "."  $ \ [ORef a, ORef b] -> checkOK (ORef (a++b))
    , func "ref" $ \ ax -> case ax of
        [ORef a] -> checkOK (ORef a)
        ax -> fmap (CENext . ORef . concat) $ forM ax $ \o -> case o of
          (OString o) -> return [o]
          (ORef    o) -> return o
    , func "delete" $ \ ax -> do
        forM_ ax $ \ (ORef o) -> execScriptToCheck id (deleteGlobalVar o >> return OTrue)
        checkOK OTrue
    , func "fst" $ \ [OPair (a, _)] -> checkOK a
    , func "snd" $ \ [OPair (_, a)] -> checkOK a
    , func "join" $ \ax -> case ax of
        [a, b] -> do
          checkOK $ case (a, b) of
            (OString a, OList b) -> OString $ ustr $
              intercalate (uchars a) $ flip map b $ \b -> case b of
                OString b -> uchars b
                b         -> showObj 0 b
            (OList   a, OList b) -> OList $ intercalate a $ flip map b $ \b -> case b of
              (OList b) -> b
              b         -> [b]
        [a] -> checkOK $ case a of
          OList (ax@(OString _:_)) -> OString $ ustr $ flip concatMap ax $ \a -> case a of
            OString a -> uchars a
            a         -> showObj 0 a
          OList (ax@(OList   _:_)) -> OList $ flip concatMap ax $ \a -> case a of
            OList a -> a
            a       -> [a]
    , func "listArray" $ \ [OList a] -> checkOK (OArray (listArray (0, fromIntegral (length a)) a))
    , func "print"  $ \ ax -> do
        forM ax $ \a -> liftIO $ case a of
          OString a -> putStrLn (uchars a)
          a         -> putStrLn (showObj 0 a)
        checkOK ONull
    , func "readDB" $ \ [OString path] -> execScriptToCheck (error "file not returned") $ do
        file <- execReadDB path ideaLoadHandle
        xunit <- ask
        let fp = filePath file
        execRun (dModifyMVar_ xloc (execOpenFiles xunit) (return . M.insert fp file))
        return (CENext $ OString fp)
    , func "writeDB" $ \ ax -> case ax of
        [OString path] ->
          (execScriptToCheck (error "file not returned") $
            (fmap (OString . filePath) (execWriteDB path))
          ) >>= checkOK
        [OString path, OString newPath] -> error "(write to new path is not yet defined)" -- TODO: define it
    , func "closeDB" $ \ [OString path] -> execScriptToCheck CENext $ do
        xunit <- ask
        execRun $ dModifyMVar xloc (execOpenFiles xunit) $ \ftab -> do
          let file = M.lookup path ftab
          case file of
            Nothing   -> return (ftab, CENext ONull)
            Just file -> return (M.delete path ftab, CENext (OString (filePath file)))
    , func "listDBs" $ \ [] -> execScriptToCheck CENext $ ask >>= \xunit -> execRun $
        fmap (CENext . OList . map OString . M.keys) (dReadMVar xloc (execOpenFiles xunit))
    , func "listMods" $ \ [] -> runToCheck $ do
        fmap (OList . map (\ (a, b) -> OPair (OString a, OString (filePath b))) . M.assocs) $
          ask >>= dReadMVar xloc . logicalNameIndex
    , func "do" $ \ ax -> do
        let run sel ax = do
              let strs = concatMap extractStringElems ax
              xunit <- execScriptToCheck undefined ask :: Check ExecUnit
              runToCheck_ $ do
                files <- selectModules (Just xunit) sel
                let job = currentExecJob xunit
                case job of
                  Nothing  -> forM_ strs (flip (execInputString False) files)
                  Just job -> do
                    xunits <- mapM (dReadMVar xloc . execUnit) files
                    forM strs (makeTasksForInput xunits) >>= startTasksForJob job . concat
              checkOK OTrue
        case ax of
          (OString sel:ax) -> run [sel] ax
          (OList   sel:ax)
            | not (null ax) -> forM sel (\ (OString o) -> return o) >>= \sel -> run sel ax
          ax -> run [] ax
    ]

