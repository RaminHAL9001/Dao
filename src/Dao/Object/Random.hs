-- "src/Dao/Object/Random.hs"  instantiates Objects into the RandO class.
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
{-# LANGUAGE FlexibleInstances #-}

module Dao.Random where

import           Dao.Token
import           Dao.Glob
import           Dao.Object
import           Dao.Random
import           Dao.Object.AST
import qualified Dao.Tree              as T

import           Control.Monad.State

import           Data.List
import           Data.Char
import           Data.Bits
import           Data.Word
import           Data.Ratio
import           Data.Complex
import           Data.Time
import           Data.Array.IArray
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as Bz
import qualified Data.Set              as S
import qualified Data.Map              as M
import qualified Data.IntMap           as I

import           System.Random

----------------------------------------------------------------------------------------------------

randCom :: a -> RandO (Com a)
randCom = return . Com
--  randCom :: a -> RandO (Com a)
--  randCom a = do
--    typ <- fmap (flip mod 24 . unsign) randInt
--    case typ of
--      0 -> do
--        before <- randComments
--        after  <- randComments
--        return (ComAround before a after)
--      1 -> do
--        before <- randComments
--        return (ComBefore before a)
--      2 -> do
--        after <- randComments
--        return (ComAfter a after)
--      _ -> return (Com a)

randComments :: RandO [Comment]
randComments = return []
--  randComments :: RandO [Comment]
--  randComments = do
--    i0 <- randInt
--    let (i1, many) = divMod i0 4
--        (i2, typn) = divMod i1 16
--        typx = take many (randToBase 2 typn ++ replicate 4 0)
--        lenx = map (+1) (randToBase 29 i2)
--        com typ = if typ==0 then EndlineComment else InlineComment
--    forM (zip typx lenx) $ \ (typ, len) ->
--      fmap (com typ . ustr . unwords . map (B.unpack . getRandomWord)) (replicateM len randInt)

getRandomWord :: Int -> B.ByteString
getRandomWord i = randomWords ! (mod i (rangeSize (bounds randomWords) - 1))

----------------------------------------------------------------------------------------------------

instance HasRandGen Object where
  randO = randOFromList $
    [ return ONull
    , return OTrue
    -- , fmap OType randO
    , randInteger (OInt  0) $ \i -> randInt >>= \j -> return (OInt$fromIntegral$ i*j)
    , randInteger (OWord 0) $ \i -> randInt >>= \j -> return (OWord$fromIntegral$abs$ i*j)
    , randInteger (OLong 0) $ \i ->
        replicateM (mod i 4 + 1) randInt >>= return . OLong . longFromInts
    , randInteger (OFloat 0) (fmap (OFloat . fromRational) . randRational)
    , randInteger (ORatio 0) (fmap ORatio . randRational)
    , randInteger (OComplex 0) $ \i0 -> do
        let (i1, rem) = divMod i0 4
        real <- fmap fromRational (randRational i1)
        cplx <- fmap fromRational (randRational rem)
        return (OComplex (real:+cplx))
    , do  day <- fmap (ModifiedJulianDay . unsign . flip mod 73000) randInt
          sec <- fmap (fromRational . toRational . flip mod 86400) randInt
          return (OTime (UTCTime{utctDay=day, utctDayTime=sec}))
    , randInteger (ODiffTime 0) $ \i -> do
        div <- randInt
        fmap (ODiffTime . fromRational . (% fromIntegral div) . longFromInts) $
          replicateM (mod i 2 + 1) randInt
    , randInteger (OChar '\n') (\i -> return (OChar $ chr $ mod i $ ord (maxBound::Char)))
    , randInteger (OString nil) $ \i -> do
        len <- nextInt 4
        flip fmap (replicateM (len+1) randInt) $
          OString . ustr . unwords . map (B.unpack . getRandomWord)
    , fmap ORef randO
    , fmap OPair (liftM2 (,) randO randO)
    , fmap OList (randList 0 40)
    , fmap (OSet . S.fromList) (randList 0 40)
    , do -- OArray
          hi <- nextInt 12
          lo <- nextInt 8
          fmap (OArray . listArray (fromIntegral lo, fromIntegral (lo+hi))) $
            replicateM (hi+1) (limSubRandO ONull)
    , randObjMap ODict   M.fromList (fmap randUStr randInt)
    , randObjMap OIntMap I.fromList randInt
    , fmap OTree randO
    , fmap OGlob randO
    , fmap OScript randO
    , do  i <- nextInt 10
          fmap (OBytes . Bz.pack . map fromIntegral . concat) $
            replicateM i (fmap (randToBase 256) randInt)
    ]

instance HasRandGen TypeID where
  randO = fmap toEnum (nextInt (fromEnum (maxBound::TypeID)))

instance HasRandGen Reference where
  randO = randOFromList $
    [ fmap (IntRef . fromIntegral . abs) randInt
    , fmap LocalRef  single
    , fmap StaticRef single
    , fmap QTimeRef  multi
    , fmap GlobalRef multi
    , liftM2 ProgramRef (fmap (ustr . (++".dao") . uchars) single) subRandO 
    , liftM2 FileRef (fmap (ustr . (++".idea") . uchars) single) multi
    , liftM2 Subscript subRandO (limSubRandO (OList []))
    , fmap MetaRef subRandO
    ]
    where
      single = fmap randUStr randInt
      multi = do
        i0 <- randInt
        let (i1, len) = divMod i0 4
        fmap ((randUStr i1 :) . map randUStr) (replicateM len randInt)

instance HasRandGen Glob where
  randO = do
    len <- fmap (+6) (nextInt 6)
    i <- randInt
    let loop len i =
          if len<=1 then [] else let (i', n) = divMod i len 
          in  (n+1) : loop (len-n-1) i'
        cuts = loop len i
    tx <- fmap (([]:) . map (\t -> if t==0 then [AnyOne] else [Wildcard]) . randToBase 2) randInt
    let loop tx cuts ax = case cuts of
          []       -> [ax]
          cut:cuts ->
            let (wx, ax') = splitAt cut ax
                (t,  tx') = splitAt 1 tx
            in  t ++ wx : loop tx' cuts ax'
    patUnits <- fmap (concat . loop tx cuts . intersperse (Single (ustr " "))) $
      replicateM len (fmap (Single . randUStr) randInt)
    return (Glob{getPatUnits=patUnits, getGlobLength=length patUnits})

instance HasRandGen (T.Tree Name Object) where
  randO = do
    branchCount <- nextInt 4
    cuts <- fmap (map (+1) . randToBase 6) randInt
    fmap (T.fromList . concat) $ replicateM (branchCount+1) $ do
      wx <- replicateM 6 (fmap randUStr randInt)
      forM cuts $ \cut -> do
        obj <- limSubRandO ONull
        return (take cut wx, obj)

instance HasRandGen Subroutine where
  randO = do
    pats <- randList 0 30
    scrp <- randScriptExpr
    let msg = "Executable generated by \"randO\" is not intended to be executed."
    return $
      Subroutine
      { argsPattern      = pats
      , getSubExecutable =
          Executable
          { origSourceCode = concatMap (toInterm . unComment) scrp
          , staticVars     = error msg
          , executable     = error msg
          }
      }

----------------------------------------------------------------------------------------------------

no :: RandO Location
no = return LocationUnknown

lit :: Object -> AST_Object
lit = flip AST_Literal LocationUnknown

randScriptExpr :: RandO [Com AST_Script]
randScriptExpr = randList 0 30 >>= mapM randCom

comRandScriptExpr :: RandO (Com [Com AST_Script])
comRandScriptExpr = randScriptExpr >>= randCom

comRandName :: RandO (Com Name)
comRandName = fmap randUStr randInt >>= randCom

comRandObjExpr :: RandO (Com AST_Object)
comRandObjExpr = randO >>= randCom

comRandObjExprList :: RandO [Com AST_Object]
comRandObjExprList = randList 1 20 >>= mapM randCom

comAssignExprList :: [AST_Object] -> RandO [AST_Object]
comAssignExprList = mapM $ \item ->
  liftM3 (AST_Assign item) (subRandO >>= randCom) randO no

mostlyRefExprs :: RandO AST_Object
mostlyRefExprs = do
  nonRef <- randBool
  if nonRef
    then  fmap (flip (AST_Paren True) LocationUnknown . Com) $
            limSubRandO (lit $ ORef $ LocalRef $ ustr "arr")
    else  fmap (lit . ORef . LocalRef . randUStr) randInt

limRandObj :: RandO Object
limRandObj = limSubRandO (OInt 0)

instance HasRandGen UpdateOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::UpdateOp)))

instance HasRandGen ArithOp1 where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::ArithOp1)))

instance HasRandGen ArithOp2 where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::ArithOp2)))

instance HasRandGen AST_Script where
  randO = randOFromList $
    [ liftM3 AST_EvalObject   randAssignExpr randComments no
    , liftM5 AST_IfThenElse   randComments randO comRandScriptExpr comRandScriptExpr no
    , liftM4 AST_TryCatch     comRandScriptExpr comRandName randScriptExpr no
    , liftM4 AST_ForLoop      comRandName comRandObjExpr randScriptExpr no
    , liftM4 AST_ContinueExpr randBool randComments comRandObjExpr no
    , liftM3 AST_ReturnExpr   randBool comRandObjExpr no
    , liftM3 AST_WithDoc      comRandObjExpr randScriptExpr no
    ]

instance HasRandGen LambdaExprType where
  randO = fmap toEnum (nextInt 3)

-- | Will create a random 'Dao.Object.AST_Object' of a type suitable for use as a stand-alone script
-- expression, which is only 'AST_Assign'.
randAssignExpr :: RandO AST_Object
randAssignExpr = liftM4 AST_Assign   randO (randO >>= randCom) randO no

instance HasRandGen AST_Object where
  -- | Differs from 'randAssignExpr' in that this 'randO' can generate 'Dao.Object.AST_Literal' expressions
  -- whereas 'randAssignExpr' will not so it does not generate stand-alone constant expressions within
  -- 'Dao.Object.AST_Script's.
  randO = randOFromList $
    [ liftM2 AST_Literal      limRandObj no
    , randAssignExpr
    , let check a = case a of
            AST_Equation a op b no | DOT == unComment op  -> AST_Equation (check a) op (check b) no
            AST_Literal (ORef (LocalRef _))           _   -> a
            AST_Literal (OString _)                   _   -> a
            AST_FuncCall _ _ _                        _   -> a
            AST_Paren _ a                         loc -> AST_Paren True a loc
            a                                         -> AST_Paren True (Com a) LocationUnknown
      in  fmap check (liftM4 AST_Equation randO (randO >>= randCom) randO no)
    , do  o@(AST_Prefix fn cObjXp no)  <- liftM3 AST_Prefix randO comRandObjExpr no
          return $ case fn of
            REF -> case unComment cObjXp of
              AST_Literal (ORef (LocalRef _)) _ -> o
              _ -> AST_Prefix fn (fmap (flip ((AST_Paren True) . Com) no) cObjXp) no
            _ -> o
    , liftM3 AST_Paren    randBool comRandObjExpr no
    , liftM4 AST_ArraySub mostlyRefExprs randComments comRandObjExpr no
    , liftM4 AST_FuncCall     (fmap randUStr randInt) randComments comRandObjExprList no
    , do -- AST_Dict
          typ   <- nextInt 4
          dict  <- return $ ustr $ case typ of
            0 -> "dict"
            1 -> "intmap"
            2 -> "list"
            3 -> "set"
          let rndlist = randListOf 0 30 (fmap lit limRandObj)
          exprs <- mapM randCom =<< case typ of
            0 -> randListOf 0 30 (fmap (lit . OString . randUStr ) randInt) >>= comAssignExprList
            1 -> randListOf 0 30 (fmap (lit . OInt . fromIntegral) randInt) >>= comAssignExprList
            2 -> rndlist
            3 -> rndlist
          coms  <- randComments
          return (AST_Dict dict coms exprs LocationUnknown)
    , do -- AST_Array
          i <- nextInt 4
          let int = fmap (OInt . fromIntegral) randInt
              ref = fmap ORef subRandO
              f x = liftM2 x int ref
          idxExpr <- randCom =<< mapM (randCom . lit) =<< case i of
            0 -> replicateM 2 int
            1 -> f (\a b -> [a,b]) 
            2 -> replicateM 2 ref
            3 -> f (\a b -> [b,a])
          items <- randList 0 30 >>= mapM randCom
          return (AST_Array idxExpr items LocationUnknown)
    , liftM3 AST_Struct comRandObjExpr (randList 0 30 >>= comAssignExprList >>= mapM randCom) no
    , liftM4 AST_Lambda randO (randArgsDef >>= randCom) randScriptExpr no
    , liftM2 AST_MetaEval comRandObjExpr no
    ]

randArgsDef :: RandO [Com AST_Object]
randArgsDef = randList 0 7 >>= mapM randCom

instance HasRandGen TopLevelEventType where
  randO = fmap toEnum (nextInt 3)

instance HasRandGen AST_TopLevel where
  randO = randOFromList $
    [ do  req_ <- nextInt 2
          let req = Com $ ustr $ if req_ == 0 then "require" else "import"
          typ <- nextInt 2
          words <- fmap (map uchars) (randListOf 1 6 (fmap randUStr randInt))
          str <- randCom $ case typ of
            0 -> ustr $ show $ intercalate " " $ words
            1 -> ustr $ intercalate "." $ words
          return (AST_Attribute req str LocationUnknown)
    , liftM2 AST_TopScript randO no
    , do  name <- comRandName
          args <- randList 0 7 >>= mapM randCom
          scrp <- comRandScriptExpr
          return (AST_TopFunc name args scrp LocationUnknown)
    , liftM2 AST_TopScript      randO no
    , liftM4 AST_TopLambda  randO (randArgsDef >>= randCom) randScriptExpr no
    , liftM3 AST_Event      randO comRandScriptExpr no
    ]

----------------------------------------------------------------------------------------------------

instance HasRandGen Pattern where
  randO = return ObjAny1

