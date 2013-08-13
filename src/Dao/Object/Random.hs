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

module Dao.Object.Random where

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
import qualified Data.Binary           as Db
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as Bz
import qualified Data.Set              as S
import qualified Data.Map              as M
import qualified Data.IntMap           as I

import           System.Random

-- import           Debug.Trace

----------------------------------------------------------------------------------------------------

randObjMap :: (map Object -> Object) -> ([(key, Object)] -> map Object) -> RandO key -> RandO Object
randObjMap objConstruct mapConstruct keygen = (randList 0 7) >>= \ox ->
  fmap (objConstruct . mapConstruct) (forM ox (\obj -> keygen >>= \key -> return (key, obj)))

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

----------------------------------------------------------------------------------------------------

randInteger :: Object -> (Int -> RandO Object) -> RandO Object
randInteger zero mkOther = do
  i <- randInt
  let (x, r) = divMod i 2
  if r==0 then return zero else mkOther x

randSingleton :: RandO Object
randSingleton = randOFromList randSingletonList

randSingletonList :: [RandO Object]
randSingletonList =
  [ return ONull
  , return OTrue
  -- , fmap OType randO
  , randInteger (OInt  0) $ \i -> randInt >>= \j -> return (OInt$fromIntegral$ i*j)
  , randInteger (OWord 0) $ \i -> randInt >>= \j -> return (OWord$fromIntegral$abs$ i*j)
  , randInteger (OLong 0) $ \i -> replicateM (mod i 4 + 1) randInt >>= return . OLong . longFromInts
  , randInteger (ORatio 0) $ \i -> return (ORatio (toInteger i % 1))
  , randInteger (OComplex (0:+0)) $ \i -> return (OComplex (0 :+ (fromRational (toInteger i % 1))))
  , randInteger (OFloat 0) (fmap (OFloat . fromRational) . randRational)
  , randInteger (OChar '\n') (\i -> return (OChar $ chr $ mod i $ ord (maxBound::Char)))
  , randInteger (OString nil) $ \i -> do
      len <- nextInt 4
      flip fmap (replicateM (len+1) randInt) $
        OString . ustr . unwords . map (B.unpack . getRandomWord)
  , fmap (ORef . LocalRef) randName
  ]

instance HasRandGen Object where
  randO = randOFromList $ randSingletonList ++
    [ randInteger (ORatio 0) (fmap ORatio . randRational)
    , randInteger (OComplex 0) $ \i0 -> do
        let (i1, rem) = divMod i0 4
        real <- fmap fromRational (randRational i1)
        cplx <- fmap fromRational (randRational rem)
        return (OComplex (real:+cplx))
    , fmap ORef randO
    , fmap OPair (liftM2 (,) randO randO)
    , fmap OList (randList 0 40)
    , fmap (OSet . S.fromList) (randList 0 40)
      -- OTime
    , do  day <- fmap (ModifiedJulianDay . unsign . flip mod 73000) randInt
          sec <- fmap (fromRational . toRational . flip mod 86400) randInt
          return (OTime (UTCTime{utctDay=day, utctDayTime=sec}))
      -- ODiffTime
    , randInteger (ODiffTime 0) $ \i -> do
        div <- randInt
        fmap (ODiffTime . fromRational . (% fromIntegral div) . longFromInts) $
          replicateM (mod i 2 + 1) randInt
    , do -- OArray
          hi <- nextInt 12
          lo <- nextInt 8
          fmap (OArray . listArray (fromIntegral lo, fromIntegral (lo+hi))) $
            replicateM (hi+1) (limSubRandO ONull)
    , randObjMap ODict   M.fromList (randName)
    , randObjMap OIntMap I.fromList randInt
    , fmap OTree randO
    , fmap OGlob randO
    , fmap OScript randO
      -- OBytes
    , do  i <- nextInt 10
          fmap (OBytes . Bz.pack . map fromIntegral . concat) $
            replicateM i (fmap (randToBase 256) randInt)
    ]

instance HasRandGen TypeID where
  randO = fmap toEnum (nextInt (fromEnum (maxBound::TypeID)))

randMultiName :: RandO [UStr]
randMultiName = do
  i0 <- randInt
  let (i1, len) = divMod i0 4
  fmap ((randUStr i1 :) . map randUStr) (replicateM len randInt)

randSimpleRefList :: [RandO Reference]
randSimpleRefList =
  [ fmap (IntRef . fromIntegral . abs) randInt
  , fmap LocalRef  randName
  ]

randSimpleRef :: RandO Reference
randSimpleRef = randOFromList randSimpleRefList

instance HasRandGen Reference where
  randO = randOFromList $ randSimpleRefList ++ 
    [ fmap StaticRef randName
    , fmap QTimeRef  randMultiName
    , fmap GlobalRef randMultiName
    , liftM2 ProgramRef (fmap (ustr . (++".dao") . uchars) randName) subRandO 
    , liftM2 FileRef (fmap (ustr . (++".idea") . uchars) randName) randMultiName
    , liftM2 Subscript subRandO (limSubRandO (OList []))
    , fmap MetaRef subRandO
    ]

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
      wx <- replicateM 6 (randName)
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
          { origSourceCode = concatMap toInterm scrp
          , staticVars     = error msg
          , executable     = error msg
          }
      }

----------------------------------------------------------------------------------------------------

no :: RandO Location
no = return LocationUnknown

lit :: Object -> AST_Object
lit = flip AST_Literal LocationUnknown

randScriptExpr :: RandO [AST_Script]
randScriptExpr = randList 0 30

comRandScriptExpr :: RandO (Com [AST_Script])
comRandScriptExpr = randScriptExpr >>= randCom

randName :: RandO Name
randName = fmap randUStr randInt

comRandObjExpr :: RandO (Com AST_Object)
comRandObjExpr = randO >>= randCom

comRandObjExprList :: RandO [Com AST_Object]
comRandObjExprList = randListOf 1 20 randObjectAST >>= mapM randCom

comAssignExprList :: [AST_Object] -> RandO [AST_Object]
comAssignExprList = mapM $ \item -> liftM3 (AST_Assign item) (randO >>= randCom) randAssignExpr no

mostlyRefExprs :: RandO AST_Object
mostlyRefExprs = do
  nonRef <- randBool
  if nonRef
    then  fmap (flip AST_Paren LocationUnknown . Com) $
            limSubRandO (lit $ ORef $ LocalRef $ ustr "arr")
    else  fmap (lit . ORef . LocalRef . randUStr) randInt

limRandObj :: RandO Object
limRandObj = limSubRandO (OInt 0)

instance HasRandGen UpdateOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::UpdateOp)))

instance HasRandGen PrefixOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::PrefixOp)))

instance HasRandGen InfixOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::InfixOp)))

instance HasRandGen LambdaExprType where
  randO = fmap toEnum (nextInt 3)

randScriptList :: [RandO AST_Script]
randScriptList =
  [ liftM3 AST_EvalObject   randAssignExpr randComments no
  , liftM5 AST_IfThenElse   randComments randO comRandScriptExpr comRandScriptExpr no
  , do  try   <- comRandScriptExpr
        catch <- nextInt 2
        (name, catch) <-
          if catch==0
            then  return (Com nil, [])
            else  liftM2 (,) (randName >>= randCom) randScriptExpr
        return (AST_TryCatch try name catch LocationUnknown)
  , liftM4 AST_ForLoop      (randName>>=randCom) comRandObjExpr randScriptExpr no
  , liftM4 AST_ContinueExpr randBool randComments (randObjectASTVoid >>= randCom) no
  , liftM3 AST_ReturnExpr   randBool (randObjectASTVoid >>= randCom) no
  , liftM3 AST_WithDoc      comRandObjExpr randScriptExpr no
  ]

randScript :: RandO AST_Script
randScript = randOFromList randScriptList

instance HasRandGen AST_Script where
  randO = randOFromList $ randScriptList ++ [liftM AST_Comment randComments]

-- | Will create a random 'Dao.Object.AST_Object' of a type suitable for use as a stand-alone script
-- expression, which is only 'AST_Assign'.
randAssignExpr :: RandO AST_Object
randAssignExpr = do
  ox <- randListOf 0 3 (liftM2 (,) randFuncHeader (randO>>=randCom))
  o  <- randFuncHeader
  return (foldr (\(left, op) right -> AST_Assign left op right LocationUnknown) o ox)

randSingletonASTList :: [RandO AST_Object]
randSingletonASTList = fmap (fmap (flip AST_Literal LocationUnknown)) randSingletonList

randSingletonAST :: RandO AST_Object
randSingletonAST = randOFromList randSingletonASTList

randPrefixWith :: RandO AST_Object -> [PrefixOp] -> RandO AST_Object
randPrefixWith randGen ops = randOFromList $ randGen : fmap randOps ops where
  randOps op = do
    obj <- randGen >>= randCom
    return (AST_Prefix op obj LocationUnknown)

randRefDeref :: RandO AST_Object
randRefDeref = randPrefixWith randReference [REF, DEREF]

randRefQualified :: RandO AST_Object
randRefQualified = randPrefixWith randRefDeref $
  [GLDOT, GLOBALPFX, LOCALPFX, QTIMEPFX, STATICPFX]

randReference :: RandO AST_Object
randReference = do
  let ref = fmap (\a -> AST_Literal (ORef (LocalRef a)) LocationUnknown) randName
      comOp = nextInt 2 >>= \op -> randCom (if op==0 then DOT else POINT)
  ax <- randListOf 1 3 (liftM2 (,) ref comOp)
  a  <- ref
  return $ foldr (\ (left, op) right -> (AST_Equation left op right LocationUnknown)) a ax

randFuncHeaderList :: [RandO AST_Object]
randFuncHeaderList = fmap loop $
  [ randRefQualified
  , liftM2 AST_Paren    comRandObjExpr no
  , liftM2 AST_MetaEval comRandObjExpr no
  ]
  where
    loop rand = rand >>= \o -> nextInt 2 >>= \i ->
      if i==0
        then return o
        else nextInt 2 >>= \c -> 
          liftM4 (if c==0 then AST_FuncCall else AST_ArraySub)
            randFuncHeader randComments comRandObjExprList no

randFuncHeader :: RandO AST_Object
randFuncHeader = randOFromList randFuncHeaderList

randContainerList :: [RandO AST_Object]
randContainerList =
  [ do -- AST_Dict
        typ   <- nextInt 4
        dict  <- return $ ustr $ case typ of
          0 -> "dict"
          1 -> "intmap"
          2 -> "list"
          3 -> "set"
        let rndlist = randListOf 0 30 randAssignExpr
        let idx fn = fmap (lit . fn) randInt
        exprs <- mapM randCom =<< case typ of
          0 -> randListOf 0 30 (idx (OString . randUStr)) >>= comAssignExprList
          1 -> randListOf 0 30 (idx (OInt . fromIntegral)) >>= comAssignExprList
          2 -> rndlist
          3 -> rndlist
        coms  <- randComments
        return (AST_Dict dict coms exprs LocationUnknown)
  , do -- AST_Array
        i <- nextInt 4
        let int = randSingletonAST
            ref = randReference
            f x = liftM2 x int ref
        idxExpr <- randCom =<< mapM randCom =<< case i of
          0 -> replicateM 2 int
          1 -> f (\a b -> [a,b]) 
          2 -> replicateM 2 ref
          3 -> f (\a b -> [b,a])
        items <- randList 0 30 >>= mapM randCom
        return (AST_Array idxExpr items LocationUnknown)
  , liftM3 AST_Struct (randObjectASTVoid >>= randCom) (randList 0 30 >>= comAssignExprList >>= mapM randCom) no
  , liftM4 AST_Lambda randO (randArgsDef >>= randCom) randScriptExpr no
  , liftM2 (\coms dat -> AST_Data coms dat LocationUnknown) randComments $ do
      spcs <- fmap ((+1) . (flip mod 8)) randInt
      fmap concat $ replicateM spcs $ do
        len <- fmap ((+1) . (flip mod 12)) randInt
        dat <- fmap Bz.concat (replicateM len (fmap Db.encode randInt))
        return (map (Com . ustr) $ b64Encode dat)
  ]

randContainer :: RandO AST_Object
randContainer = randOFromList randContainerList

randObjectASTList :: [RandO AST_Object]
randObjectASTList =  randAssignExpr : randContainerList ++ randSingletonASTList

randObjectAST :: RandO AST_Object
randObjectAST = randPrefixWith (randOFromList randObjectASTList) [INVB, NEGTIV]

randInfixOp :: RandO (Com InfixOp, (Int, Bool))
randInfixOp = do
  (op, prec) <- randOFromList opGroups
  op         <- randCom op
  return (op, prec)
  where
    left  op = (op, True )
    right op = (op, False)
    opGroups = fmap return $ do
      (precedence, operators) <- zip [1..] $ (++[[right POW]]) $ fmap (fmap left) $
        [ [EQUL, NEQUL]
        , [GTN, LTN, GTEQ, LTEQ], [SHL, SHR]
        , [OR], [AND], [ORB], [XORB], [ANDB]
        , [ADD, SUB], [DIV, MOD], [MULT]
        ]
      (operator, associativity) <- operators
      return (operator, (precedence, associativity))

randArithmetic :: RandO AST_Object
randArithmetic = do
  o  <- randObjectAST
  ox <- randListOf 0 4 (liftM2 (,) randObjectAST randInfixOp)
  return (fst $ loop 0 o ox)
  where
    bind right op left = AST_Equation right op left LocationUnknown
    loop prevPrec left opx = case opx of
      []                                    -> (left, [])
      -- (right, (op, _                )):[]   -> bind right op left
      (right, (op, (prec, leftAssoc))):next ->
        if prevPrec<prec || (prevPrec==prec && not leftAssoc) -- ? If so, we should bind right
          then  let (right, next) = loop prec left opx in loop prevPrec right next
          else  loop prec (bind right op left) next

-- Can also produce void expressions.
randObjectASTVoidList :: [RandO AST_Object]
randObjectASTVoidList = return AST_Void : randObjectASTList

-- Can also produce void expressions.
randObjectASTVoid :: RandO AST_Object
randObjectASTVoid = randOFromList randObjectASTVoidList

randEquationASTList :: [RandO AST_Object]
randEquationASTList = randFuncHeaderList ++
  [ let check a = case a of
          AST_Equation a op b no | DOT == unComment op  -> AST_Equation (check a) op (check b) no
          AST_Literal (ORef (LocalRef _)) _   -> a
          AST_Literal (OString _)         _   -> a
          AST_FuncCall _       _      _   _   -> a
          AST_Paren    a                  loc -> AST_Paren a loc
          a                                   -> AST_Paren (Com a) LocationUnknown
    in  fmap check (liftM4 AST_Equation randO (randO>>=randCom) randO no)
  ]

instance HasRandGen AST_Object where
  -- | Differs from 'randAssignExpr' in that this 'randO' can generate 'Dao.Object.AST_Literal' expressions
  -- whereas 'randAssignExpr' will not so it does not generate stand-alone constant expressions within
  -- 'Dao.Object.AST_Script's.
  randO = randOFromList randObjectASTList

randArgsDef :: RandO [Com AST_Object]
randArgsDef = randList 0 7 >>= mapM randCom

instance HasRandGen TopLevelEventType where
  randO = fmap toEnum (nextInt 3)

instance HasRandGen AST_TopLevel where
  randO = randOFromList $
    [ do  req_ <- nextInt 2
          let req = ustr $ if req_ == 0 then "require" else "import"
          typ <- nextInt 2
          words <- fmap (map uchars) (randListOf 1 6 (randName))
          (rnd, quoted) <- fmap (flip divMod 2) randInt
          (rnd, len)    <- return (divMod rnd 4)
          item <- sequence $ replicate (len+1) $ fmap (ustr . B.unpack . getRandomWord) randInt
          item <- return $
            if len==0
              then  AST_Literal (OString (ustr (head words))) LocationUnknown
              else
                foldr
                  (\a b ->
                      AST_Equation
                        (AST_Literal (ORef (LocalRef a)) LocationUnknown)
                        (Com DOT)
                        b
                        LocationUnknown
                  )
                  (AST_Literal (ORef (LocalRef (head item))) LocationUnknown)
                  (tail item)
          item <- randCom item
          return (AST_Attribute req item LocationUnknown)
    , liftM2 AST_TopScript randScript no
    , do  coms <- randComments
          name <- randName
          args <- randList 0 7 >>= mapM randCom >>= randCom
          scrp <- randScriptExpr
          return (AST_TopFunc coms name args scrp LocationUnknown)
    , liftM4 AST_TopLambda  randO (randArgsDef >>= randCom) randScriptExpr no
    , liftM4 AST_Event      randO randComments randScriptExpr no
    ]

----------------------------------------------------------------------------------------------------

instance HasRandGen Pattern where
  randO = return ObjAny1

