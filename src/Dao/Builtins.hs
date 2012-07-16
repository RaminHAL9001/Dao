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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Dao.Builtins where

-- | This module contains the most fundamental functions that are built-into the Dao language,
-- particularly the unary and binary operators, like arithmetic @(+) (-) (*) (/) (%)@, and other
-- essentials like @print@, @join@, and @do@.

import           Dao.Debug.ON
import           Dao.Types
import qualified Dao.Tree as T
import           Dao.Pattern
import           Dao.Predicate
import           Dao.Document
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

-- | When constructing 'CheckFunc' built-in functions, arguments are passed by reference. To
-- de-reference arugments while simply returning constants, evaluate this equation.
objectValue :: Object -> Check Object
objectValue obj = case obj of
  ORef ref -> do
    obj <- execScriptToCheck (const Nothing) (execGlobalLookup ref)
    case obj of
      Nothing  -> checkFail "undefined reference" (ORef ref)
      Just obj -> return obj
  obj      -> return obj

checkNumericOp :: String -> (forall a . Num a => a -> a -> a) -> Object -> Object -> Check (ContErr Object)
checkNumericOp op fn a b =
  execScriptToCheck id (promoteNum op a b >>= uncurry (withNum fn)) >>= checkOK

-- | Performs 'objectValue' on a pair of objects; arithmetic operators are built-in functions, most
-- built-in functions take two arguments.
objectValue2 :: Object -> Object -> Check (Object, Object)
objectValue2 a b = objectValue a >>= \a -> objectValue b >>= \b -> return (a, b)

-- | Used to create a built-in operator (like divide @(/)@ or modulus @(%)@) where, these operators
-- have two separate functions in Haskell. Provide the operator, the two Haskell functions, and two
-- Objects which will be "promoted" using 'Dao.Object.Data.promoteNum' and then the correct Haskell
-- function will be selected and applied.
checkBinumericOp
  :: String
  -> (forall a . Integral a => a -> a -> a)
  -> (forall b . Floating b => b -> b -> b)
  -> Object
  -> Object
  -> Check (ContErr Object)
checkBinumericOp op fnInt fnFrac a b = do
  (OPair (a, b)) <- execScriptToCheck id (fmap OPair (promoteNum op a b))
  checkOK $ case (a, b) of
    (OInt     a, OInt     b) -> OInt      (fnInt  a b)
    (OWord    a, OWord    b) -> OWord     (fnInt  a b)
    (OLong    a, OLong    b) -> OLong     (fnInt  a b)
    (OFloat   a, OFloat   b) -> OFloat    (fnFrac a b)
    (OComplex a, OComplex b) -> OComplex  (fnFrac a b)

-- | Bitwise logical operations which work on 'Dao.Types.OInt's and 'Dao.Object.OWord's, which also
-- work on 'Dao.Types.OSet's, 'Dao.Object.OIntMap's, and 'Dao.Types.ODict's.
checkBitwiseOp
  :: String
  -> (forall a . Bits a => a -> a -> a)
  -> (T_set  -> T_set  -> T_set)
  -> (T_dict -> T_dict -> T_dict)
  -> (T_intMap -> T_intMap -> T_intMap)
  -> Object
  -> Object
  -> Check (ContErr Object)
checkBitwiseOp op fnBit fnSet fnDict fnIntMap a b = do
  (OPair (a, b)) <- execScriptToCheck id (fmap OPair (promoteNum op a b))
  checkOK $ case (a, b) of
    (OWord   a, OWord   b) -> OWord   (fnBit    a b)
    (OInt    a, OInt    b) -> OInt    (fnBit    a b)
    (OSet    a, OSet    b) -> OSet    (fnSet    a b)
    (ODict   a, ODict   b) -> ODict   (fnDict   a b)
    (OIntMap a, OIntMap b) -> OIntMap (fnIntMap a b)

-- | Is simply @diff (union a b) b@ is a set/dict/intmap, pass the relevant @diff@ and @union@
-- function.
setwiseXOR :: (a -> a -> a) -> (a -> a -> a) -> a -> a -> a
setwiseXOR diff union a b = diff (union a b) b

-- | Used for implementing the @&&@ and @||@ operators, evaluate a pair of items, each as a boolean
-- value.
objectBoolValue2 :: Object -> Object -> Check (Bool, Bool)
objectBoolValue2 a b = let fn = fmap objToBool . objectValue in liftM2 (,) (fn a) (fn b)

----------------------------------------------------------------------------------------------------

-- | These are the fundamental built-in functions, including arithmetic operators.
basicScriptOperations :: M.Map Name CheckFunc
basicScriptOperations = M.fromList funcList where
  func nm a = (ustr nm, CheckFunc{checkFunc = \argv -> return argv >>= a})
  funcList =
    [ func "+" $ \ [a, b] -> do
        (a, b) <- objectValue2 a b
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
        (a, b) <- objectValue2 a b
        case (a, b) of
          (OSet    a, OSet    b) -> checkOK (OSet (S.difference a b))
          (ODict   a, ODict   b) -> checkOK (ODict (M.difference a b))
          (OIntMap a, OIntMap b) -> checkOK (OIntMap (I.difference a b))
          (a        , b        ) -> checkNumericOp "-" (-) a b
    , func "*" $ \ [a, b] -> do
        (a, b) <- objectValue2 a b
        (OPair (a, b)) <- execScriptToCheck id (fmap OPair (promoteNum "/" a b))
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
        (a, b) <- objectValue2 a b
        case (a, b) of
          (OString a, OString b) -> checkOK $ OList $ map OString $
            map (either ustr ustr) $ listBreakup (uchars a) (uchars b)
          (OList   a, OList   b) -> checkOK $ OList $ map (either OList OList) $ listBreakup a b
          _ -> checkBinumericOp "/" div (/) a b
    , func "%" $ \ [a, b] -> do
        (a, b) <- objectValue2 a b
        let done fn = checkOK . OList . concatMap (either fn (const []))
        case (a, b) of
          (OString a, OString b) ->
            done (\a -> [OString (ustr a)]) (listBreakup (uchars a) (uchars b))
          _ -> do
            (a, b) <- objectValue2 a b
            checkOK $ case (a, b) of
              (OWord a, OWord b) -> OWord (mod a b)
              (OInt  a, OInt  b) -> OInt  (mod a b)
    , func "**" $ \ [a, b] -> objectValue2 a b >>= uncurry (checkBinumericOp "^" (^) (**))
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
    , func "!" $ \ [a] -> objectValue a >>= checkOK . boolToObj . not . objToBool
    , func "&&" $ \ [a, b] -> objectBoolValue2 a b >>= checkOK . boolToObj . uncurry (&&)
    , func "||" $ \ [a, b] -> objectBoolValue2 a b >>= checkOK . boolToObj . uncurry (||)
    , func "==" $ \ [a, b] -> objectValue2 a b >>= checkOK . boolToObj . uncurry (==)
    , func "!=" $ \ [a, b] -> objectValue2 a b >>= checkOK . boolToObj . uncurry (/=)
    , func "<<" $ \ [a, b] -> do
        (a, b) <- objectValue2 a b
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
        (a, b) <- objectValue2 a b
        let shift a b = shiftR a (fromIntegral b)
        checkOK $ case (a, b) of
          (OWord   a, OWord b) -> OWord (shift a b)
          (OInt    a, OInt  b) -> OInt  (shift a b)
          (OWord   a, OInt  b) -> OWord (shift a b)
          (OInt    a, OWord b) -> OInt  (shift a b)
          (OList   a, b      ) -> OList (delete b a)
          (OSet    a, b        ) -> OSet  (S.delete b a)
          (ODict   a, OString b) -> ODict (M.delete b a)
          (OIntMap a, OInt    b) -> OIntMap (I.delete (fromIntegral b) a)
          (OIntMap a, OWord   b) -> OIntMap (I.delete (fromIntegral b) a)
          (OTree   a, ORef    b) -> OTree (T.delete b a)
    , func "@"  $ \ [a] -> objectValue a >>= objectValue >>= checkOK
    , func "$"  $ \ [a] -> checkOK a
    , func "."  $ \ [ORef a, ORef b] -> checkOK (ORef (a++b))
    , func "ref" $ \ ax ->
        fmap (CENext . ORef) (mapM (objectValue >=> \ (OString o) -> return o) ax)
    , func "delete" $ \ ax -> do
        forM_ ax $ \ (ORef o) -> execScriptToCheck id (deleteGlobalVar o >> return OTrue)
        checkOK OTrue
    , func "fst" $ \ [OPair (a, _)] -> checkOK a
    , func "snd" $ \ [OPair (_, a)] -> checkOK a
    , func "join" $ \ax -> case ax of
        [a, b] -> do
          (a, b) <- objectValue2 a b
          checkOK $ case (a, b) of
            (OString a, OList b) -> OString $ ustr $
              intercalate (uchars a) $ flip map b $ \b -> case b of
                OString b -> uchars b
                b         -> showObj 0 b
            (OList   a, OList b) -> OList $ intercalate a $ flip map b $ \b -> case b of
              (OList b) -> b
              b         -> [b]
        [a] -> do
          a <- objectValue a
          checkOK $ case a of
            OList (ax@(OString _:_)) -> OString $ ustr $ flip concatMap ax $ \a -> case a of
              OString a -> uchars a
              a         -> showObj 0 a
            OList (ax@(OList   _:_)) -> OList $ flip concatMap ax $ \a -> case a of
              OList a -> a
              a       -> [a]
    , func "listArray" $ \ [a] -> do
        (OList a) <- objectValue a
        checkOK (OArray (listArray (0, fromIntegral (length a)) a))
    , func "print"  $ \ ax -> do
        forM ax $ \a -> objectValue a >>= \a -> liftIO $ case a of
          OString a -> putStrLn (uchars a)
          a         -> putStrLn (showObj 0 a)
        checkOK ONull
    , func "newDB"  $ \ ax -> case ax of
        [OString path] -> runToCheck_ $ newDoc path (initDoc T.Void)
        [OString path, OTree init] -> runToCheck_ $ newDoc path (initDoc init)
    , func "openDB" $ \ [OString path] -> runToCheck_ (openDoc path)
    , func "saveDB" $ \ ax -> case ax of
        [] -> runToCheck_ saveAll
        [OString path] -> runToCheck_ (saveDoc path)
        [OString path, OString newPath] -> runToCheck_ (saveDocAs path newPath)
    , func "closeDB" $ \ [a] ->
        objectValue a >>= \ (OString path) -> runToCheck_ (closeDoc path)
    , func "listDBs" $ \ [] -> runToCheck $
        fmap (OList . map OString . M.keys) (ask >>= dReadMVar xloc . documentList)
    , func "listMods" $ \ [] -> runToCheck $ do
        fmap (OList . map (\ (a, b) -> OPair (OString a, OString (filePath b))) . M.assocs) $
          ask >>= dReadMVar $loc . logicalNameIndex
    , func "do" $ \ ax -> do
        ax <- mapM objectValue ax >>= (\t -> return $ trace "do: got args" t)
        let run sel ax = do
              strs  <- fmap (concatMap extractStringElems) (mapM objectValue ax)
              xunit <- execScriptToCheck undefined ask :: Check ExecUnit
              runToCheck_ $ do
                files <- selectModules (Just xunit) sel
                let job = currentExecJob xunit
                case job of
                  Nothing  -> forM_ strs (flip (execInputString False) files)
                  Just job -> do
                    xunits <- mapM (dReadMVar $loc . execUnit) files
                    forM strs (makeTasksForInput xunits) >>= startTasksForJob job . concat
              checkOK OTrue
        case ax of
          (OString sel:ax) -> run [sel] ax
          (OList   sel:ax)
            | not (null ax) -> forM sel (\ (OString o) -> return o) >>= \sel -> run sel ax
          ax -> run [] ax
    ]

