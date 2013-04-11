-- "src/Dao/Object/Data.hs"  provides many functions and class
-- instantiations for working with Dao's "Object" data type.
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


{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.Data where

import           Dao.Object
import qualified Dao.Tree as T
import           Dao.Glob
import           Dao.Predicate
import           Dao.Object.Monad

import           Control.Monad

import           Data.Typeable
import           Data.Dynamic
import           Data.Maybe
import           Data.Either
import           Data.List
import           Data.Bits
import           Data.Complex
import qualified Data.Map as M
import qualified Data.IntMap as I
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as U
import           Data.Int
import           Data.Char
import           Data.Word
import           Data.Ratio
import           Data.Array.IArray
import           Data.Time hiding (parseTime)

class Objectify a where
  toObject   :: a -> Object
  fromObject :: Object -> PredicateIO st a

instance Objectify ()      where
  toObject () = ONull
  fromObject ONull = return ()

instance Objectify Bool    where
  toObject tf = if tf then OTrue else ONull
  fromObject o = return $ case o of
    OTrue -> True
    ONull -> False

instance Objectify TypeID  where
  toObject = OType
  fromObject (OType o) = return o

instance Objectify Int64   where
  toObject = OInt  
  fromObject (OInt o) = return o

instance Objectify Word64  where
  toObject = OWord 
  fromObject (OWord o) = return o

instance Objectify Integer where
  toObject = OLong 
  fromObject (OLong o) = return o

instance Objectify (Ratio Integer) where
  toObject = ORatio
  fromObject (ORatio o) = return o

instance Objectify (Complex Double) where
  toObject = OComplex
  fromObject (OComplex o) = return o

instance Objectify Double  where
  toObject = OFloat
  fromObject (OFloat o) = return o

instance Objectify UTCTime where
  toObject = OTime  
  fromObject (OTime o) = return o

instance Objectify NominalDiffTime where
  toObject = ODiffTime
  fromObject (ODiffTime o) = return o

instance Objectify Char where
  toObject = OChar
  fromObject (OChar o) = return o

instance Objectify UStr where
  toObject = OString
  fromObject (OString o) = return o

instance Objectify [Char] where
  toObject = OString . ustr
  fromObject (OString o) = return (uchars o)

instance Objectify Reference where
  toObject = ORef
  fromObject (ORef o) = return o

instance Objectify (Object, Object) where
  toObject = OPair
  fromObject (OPair o) = return o

instance Objectify [Object] where
  toObject = OList
  fromObject (OList o) = return o

instance Objectify (S.Set Object) where
  toObject = OSet
  fromObject (OSet o) = return o

instance Objectify (M.Map Name Object) where
  toObject = ODict
  fromObject (ODict o) = return o

instance Objectify (I.IntMap Object) where
  toObject = OIntMap
  fromObject (OIntMap o) = return o

instance Objectify (T.Tree Name Object) where
  toObject = OTree
  fromObject (OTree o) = return o

instance Objectify Glob where
  toObject = OGlob
  fromObject (OGlob o) = return o

instance Objectify Rule where
  toObject = ORule
  fromObject (ORule o) = return o

instance Objectify B.ByteString where
  toObject = OBytes
  fromObject (OBytes o) = return o

instance Objectify FuncExpr where
  toObject = OScript
  fromObject (OScript o) = return o

instance Objectify Object where { toObject = id ; fromObject = return }

----------------------------------------------------------------------------------------------------

class HasNull t where
  nullValue :: t
  testNull :: t -> Bool

instance HasNull Object where { nullValue = ONull; testNull = (nullValue==) }
instance HasNull () where { nullValue = (); testNull _ = True }
instance HasNull TypeID where { nullValue = NullType; testNull = (nullValue==) }
instance HasNull Char where { nullValue = '\0'; testNull = (nullValue==) }
instance HasNull Int64 where { nullValue = 0; testNull = (nullValue==) }
instance HasNull Word64 where { nullValue = 0; testNull = (nullValue==) }
instance HasNull Integer where { nullValue = 0; testNull = (nullValue==) }
instance HasNull Double where { nullValue = 0; testNull = (nullValue==) }
instance HasNull (Ratio Integer) where { nullValue = 0; testNull = (nullValue==) }
instance HasNull (Complex Double) where { nullValue = 0:+0; testNull = (nullValue==) }
instance HasNull UTCTime where
  nullValue = UTCTime{utctDay=ModifiedJulianDay 0, utctDayTime=0}
  testNull = (nullValue==)
instance HasNull NominalDiffTime where { nullValue = 0; testNull = (nullValue==) }
instance HasNull [UStr] where { nullValue = []; testNull = (nullValue==) }
instance HasNull UStr where { nullValue = nil; testNull = (nullValue==) }
instance HasNull [Object] where { nullValue = []; testNull = (nullValue==) }
instance HasNull (Array Int Object) where
  nullValue = listArray (0,1) [ONull]
  testNull = (nullValue==)
instance HasNull (S.Set Object) where { nullValue = S.empty ; testNull = S.null }
instance HasNull (M.Map Name Object) where { nullValue = M.empty ; testNull = M.null }
instance HasNull (I.IntMap Object) where { nullValue = I.empty ; testNull = I.null }
instance HasNull (T.Tree Name Object) where { nullValue = T.Void ; testNull = (nullValue==) }
instance HasNull (B.ByteString) where { nullValue = B.empty ; testNull = B.null }
instance HasNull Glob where
  nullValue = Glob{getPatUnits = [], getGlobLength = 0}
  testNull o = getPatUnits o == []
instance HasNull FuncExpr where
  nullValue = FuncExpr{scriptArgv = Com [], scriptCode = Com []}
  testNull o = unComment (scriptCode o) == []

----------------------------------------------------------------------------------------------------

boolToObj :: Bool -> Object
boolToObj b = if b then OTrue else ONull

objToBool :: Object -> Bool
objToBool obj = case obj of
  ONull        -> False
  OTrue        -> True
  OType      o -> testNull o
  OInt       o -> testNull o
  OWord      o -> testNull o
  OLong      o -> testNull o
  OFloat     o -> testNull o
  ORatio     o -> testNull o
  OComplex   o -> testNull o
  OTime      o -> testNull o
  ODiffTime  o -> testNull o
  OChar      o -> testNull o
  OString    o -> testNull o
  ORef       o -> True
  OPair (a, b) -> objToBool a && objToBool b
  OList      o -> testNull o
  OSet       o -> testNull o
  OArray     o -> True
  ODict      o -> testNull o
  OIntMap    o -> testNull o
  OTree      o -> testNull o
  OGlob      o -> testNull o
  OScript    o -> testNull o
  ORule      o -> True
  OBytes     o -> testNull o

----------------------------------------------------------------------------------------------------

okInt :: Integral i => i -> PValue tok T_word
okInt = return . fromIntegral

objSize :: Object -> PValue tok T_word
objSize o = case o of
  OString   o -> return $ fromIntegral $ U.length (toUTF8ByteString o)
  ORef      o -> return $ loop 0 o where
    loop i o = case o of
      MetaRef o -> loop (i+1) o
      _         -> i
  OPair     _ -> okInt $ 2
  OList     o -> okInt $ length o
  OSet      o -> okInt $ S.size o
  OArray    o -> okInt $ let (lo, hi) = bounds o in (lo - hi)
  OIntMap   o -> okInt $ I.size o
  ODict     o -> okInt $ M.size o
  OTree     o -> okInt $ T.size o
  OGlob     o -> okInt $ length (getPatUnits o)
  ORule     o -> okInt $ length (unComment (ruleAction  o))
  OBytes    o -> okInt $ B.length o
  _           -> mzero

objToList :: Object -> PValue tok [Object]
objToList o = case o of
  OPair (a, b) -> return $ [a, b]
  OString  o   -> return $ map OChar (uchars o)
  OList    o   -> return o
  OSet     o   -> return $ S.elems o
  OArray   o   -> return $ elems o
  ODict    o   -> return $ map (\ (a, b) -> OPair (OString a, b))             (M.assocs o)
  OIntMap  o   -> return $ map (\ (a, b) -> OPair (OInt (fromIntegral a), b)) (I.assocs o)
  OTree    o   -> return $ map (\ (a, b) -> OPair (OList (map OString a), b)) (T.assocs o)
  OGlob    o   -> return $ patternComponents o
  _            -> mzero

patUnitToObj :: GlobUnit -> Object
patUnitToObj p = case p of
  Wildcard -> OType ListType
  AnyOne   -> OType StringType
  Single o -> OString o

objToPatUnit :: Object -> PValue tok GlobUnit
objToPatUnit o = case o of
  OType StringType -> return Wildcard
  OType ListType   -> return AnyOne
  OString str      -> return $ Single str
  _                -> mzero

objListToPattern :: [Object] -> PValue tok Glob
objListToPattern ox = loop 0 ox [] where
  loop i ox px = case ox of
    []   -> return $ Glob{getGlobLength = i, getPatUnits = px}
    o:ox -> objToPatUnit o >>= \o -> loop (i+1) ox (px++[o])

-- | Break a pattern into a list of it's component parts. 'Dao.Glob.Wildcard's (the Kleene star
-- operation) translates to a 'ListType' object because wildcards may match a whole list of
-- strings and 'Dao.Glob.AnyOne's translate to a StringType object because they can only match
-- one single string. Everything else translates to an 'OString' object.
patternComponents :: Glob -> [Object]
patternComponents p = map patUnitToObj (getPatUnits p)

ruleComponents :: Rule -> Object
ruleComponents r =
  OPair ( OList (map (OList . patternComponents) (map unComment (unComment (rulePattern r))))
        , OScript (FuncExpr (Com []) (ruleAction r))
        )

----------------------------------------------------------------------------------------------------

in_word_range :: Integral a => a -> Bool
in_word_range a =
  let a' = toInteger a
  in  toInteger (minBound::T_word) <= a' && a' <= toInteger (maxBound::T_word)

in_int_range :: Integral a => a -> Bool
in_int_range a =
  let a' = toInteger a
  in  toInteger (minBound::T_int) <= a' && a' <= toInteger (maxBound::T_int)

checkBounds
  :: (Integral a, Integral b, Objectify a)
  => TypeID
  -> (a -> Bool)
  -> a
  -> PValue Object b
checkBounds t check a =
  if check a
    then return (fromIntegral a)
    else tokenFail (OPair (toObject a, OType t)) $
            ("value out of range, cannot convert to "++show t++" type")

class ToWord a where { toWord :: a -> PredicateIO st T_word }
instance ToWord Word64  where { toWord = return }
instance ToWord Int64   where { toWord = checkBounds LongType in_word_range }
instance ToWord Integer where { toWord = checkBounds LongType in_word_range }
instance ToWord Object where
  toWord o = case o of
    OWord o -> return o
    OInt  o -> toWord o
    OLong o -> toWord o

class ToInt  a where { toInt :: a -> PredicateIO st T_int }
instance ToInt Word64  where { toInt = checkBounds IntType in_int_range }
instance ToInt Int64   where { toInt = return }
instance ToInt Integer where { toInt = checkBounds IntType in_int_range }
instance ToInt Object where
  toInt o = case o of
    OWord o -> toInt  o
    OInt  o -> return o
    OLong o -> toInt  o

class ToLong a where { toLong :: a -> PredicateIO st T_long }
instance ToLong Word64  where { toLong = return . fromIntegral }
instance ToLong Int64   where { toLong = return . fromIntegral }
instance ToLong Integer where { toLong = return . fromIntegral }
instance ToLong Object  where
  toLong o = case o of
    OWord o -> toLong o
    OInt  o -> toLong o
    OLong o -> return o

class ToFloat a where { toFloat :: a -> PredicateIO st T_float }
instance ToFloat Word64  where { toFloat = return . fromIntegral }
instance ToFloat Int64   where { toFloat = return . fromIntegral }
instance ToFloat Integer where { toFloat = return . fromIntegral }
instance ToFloat Double  where { toFloat = return }
instance ToFloat (Ratio Integer) where { toFloat = return . fromRational }
instance ToFloat NominalDiffTime where { toFloat = return . fromRational . toRational }
instance ToFloat Object where
  toFloat o = case o of
    OWord     o -> toFloat o
    OInt      o -> toFloat o
    OLong     o -> toFloat o
    OFloat    o -> toFloat o
    ORatio    o -> toFloat o
    ODiffTime o -> toFloat o

class ToRatio a where { toRatio :: a -> PredicateIO st T_ratio }
instance ToRatio Word64  where { toRatio = return . fromIntegral }
instance ToRatio Int64   where { toRatio = return . fromIntegral }
instance ToRatio Integer where { toRatio = return . fromIntegral }
instance ToRatio Double  where { toRatio = return . toRational }
instance ToRatio (Ratio Integer) where { toRatio = return }
instance ToRatio NominalDiffTime where { toRatio = return . toRational }
instance ToRatio Object where
  toRatio o = case o of
    OWord     o -> toRatio o
    OInt      o -> toRatio o
    OLong     o -> toRatio o
    OFloat    o -> toRatio o
    ORatio    o -> toRatio o
    ODiffTime o -> toRatio o

class ToDiffTime a where { toDiffTime :: a -> PredicateIO st T_diffTime }
instance ToDiffTime Word64  where { toDiffTime = return . fromIntegral }
instance ToDiffTime Int64   where { toDiffTime = return . fromIntegral }
instance ToDiffTime Integer where { toDiffTime = return . fromIntegral }
instance ToDiffTime Double  where { toDiffTime = return . fromRational . toRational }
instance ToDiffTime (Ratio Integer) where { toDiffTime = return . fromRational }
instance ToDiffTime NominalDiffTime where { toDiffTime = return }
instance ToDiffTime Object where
  toDiffTime o = case o of
    OWord     o -> toDiffTime o
    OInt      o -> toDiffTime o
    OLong     o -> toDiffTime o
    OFloat    o -> toDiffTime o
    ORatio    o -> toDiffTime o
    ODiffTime o -> toDiffTime o

class ToComplex a where { toComplex :: a -> PredicateIO st T_complex }
instance ToComplex Word64  where { toComplex i = return (fromIntegral i :+ 0) }
instance ToComplex Int64   where { toComplex i = return (fromIntegral i :+ 0) }
instance ToComplex Integer where { toComplex i = return (fromIntegral i :+ 0) }
instance ToComplex Double  where { toComplex i = return (i :+ 0) }
instance ToComplex (Ratio Integer) where { toComplex i = return (fromRational i :+ 0) }
instance ToComplex NominalDiffTime where
  toComplex i = return (fromRational (toRational i) :+ 0)
instance ToComplex (Complex Double) where { toComplex i = return i }
instance ToComplex Object where
  toComplex i = case i of
    OWord     o -> toComplex o
    OInt      o -> toComplex o
    OLong     o -> toComplex o
    OFloat    o -> toComplex o
    ORatio    o -> toComplex o
    ODiffTime o -> toComplex o
    OComplex  o -> return o

----------------------------------------------------------------------------------------------------

-- | A table lookup that takes two numerical objects, and decides which object contains less
-- information, then converts the lesser object to the same type of object as the greater object.
-- For example, given an 'Object.OInt' and an 'Object.OFloat', the int will be converted to a float.
-- The types that can be converted are as follows in lesser-to-greater order:
--     'Object.OWord', 'Object.OInt', 'Object.OLong', 'Object.OFloat',
--     'Object.ORatio', 'Object.ODiffTime', 'Object.OComplex'
promoteNum :: String -> Object -> Object -> PredicateIO st (Object, Object)
promoteNum msg a b = case a of
    OWord      _ -> case b of
      OWord     _ -> return (           a,            b)
      OInt      _ -> rFst   (toInt      a)            b
      OLong     _ -> rFst   (toLong     a)            b
      OFloat    _ -> rFst   (toFloat    a)            b
      ORatio    _ -> rFst   (toRatio    a)            b
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
    OInt       _ -> case b of
      OWord     _ -> rSnd               a (toInt      b)
      OInt      _ -> return (           a,            b)
      OLong     _ -> rFst   (toLong     a)            b
      OFloat    _ -> rFst   (toFloat    a)            b
      ORatio    _ -> rFst   (toRatio    a)            b
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
    OLong      _ -> case b of
      OWord     _ -> rSnd               a (toLong     b)
      OInt      _ -> rSnd               a (toLong     b)
      OLong     _ -> return (           a,            b)
      OFloat    _ -> rFst   (toFloat    a)            b
      ORatio    _ -> rFst   (toRatio    a)            b
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
    OFloat     _ -> case b of
      OWord     _ -> rSnd               a (toFloat    b)
      OInt      _ -> rSnd               a (toFloat    b)
      OLong     _ -> rSnd               a (toFloat    b)
      OFloat    _ -> return (           a,            b)
      ORatio    _ -> rFst   (toRatio    a)            b
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
    ORatio     _ -> case b of
      OWord     _ -> rSnd               a (toRatio    b)
      OInt      _ -> rSnd               a (toRatio    b)
      OLong     _ -> rSnd               a (toRatio    b)
      OFloat    _ -> rSnd               a (toRatio    b)
      ORatio    _ -> return (           a,            b)
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
    ODiffTime  _ -> case b of
      OWord     _ -> rSnd               a (toDiffTime b)
      OInt      _ -> rSnd               a (toDiffTime b)
      OLong     _ -> rSnd               a (toDiffTime b)
      OFloat    _ -> rSnd               a (toDiffTime b)
      ORatio    _ -> rSnd               a (toDiffTime b)
      ODiffTime _ -> return (           a,            b)
      OComplex  _ -> rFst   (toComplex  a)            b
    OComplex   _ -> case b of
      OWord     _ -> rSnd               a (toComplex  b)
      OInt      _ -> rSnd               a (toComplex  b)
      OLong     _ -> rSnd               a (toComplex  b)
      OFloat    _ -> rSnd               a (toComplex  b)
      ORatio    _ -> rSnd               a (toComplex  b)
      ODiffTime _ -> rSnd               a (toComplex  b)
      OComplex  _ -> return (           a,            b)
  where
    rFst fn b = fmap (\a -> (toObject a, b)) fn
    rSnd a fn = fmap (\b -> (a, toObject b)) fn

-- | Execute a function that is a member of the class 'Prelude.Num' over two objects. These
-- functions include @Prelude.(+)@, @Prelude.(-)@, and @Prelude.(*)@.
withNum
  :: (forall a . (Num a, Objectify a) => a -> a -> a)
  -> Object
  -> Object
  -> PredicateIO st Object
withNum fn a b = return $ case (a, b) of
  (OWord     a, OWord     b) -> toObject (fn a b)
  (OInt      a, OInt      b) -> toObject (fn a b)
  (OLong     a, OLong     b) -> toObject (fn a b)
  (OFloat    a, OFloat    b) -> toObject (fn a b)
  (ORatio    a, ORatio    b) -> toObject (fn a b)
  (ODiffTime a, ODiffTime b) -> toObject (fn a b)
  (OComplex  a, OComplex  b) -> toObject (fn a b)

-- | Execute a function that is a member of the class 'Prelude.Integral' over two objects. These
-- functions include 'Prelude.div', 'Prelude.mod', and 'Prelude.(^)'.
withIntegral
  :: (forall a . (Objectify a, Integral a) => a -> a -> a)
  -> Object
  -> Object
  -> PredicateIO st Object
withIntegral fn a b = return $ case (a, b) of
  (OWord a, OWord b) -> toObject (fn a b)
  (OInt  a, OInt  b) -> toObject (fn a b)
  (OLong a, OLong b) -> toObject (fn a b)

-- | Execute a function that is a member of the class 'Prelude.Fractional' over two objects. These
-- functions include @Prelude.(/)@, @Prelude.(**)@, and 'Prelude.logBase'.
withFractional
  :: (forall a . (Fractional a, Objectify a) => a -> a -> a)
  -> Object
  -> Object
  -> PredicateIO st Object
withFractional fn a b = return $ case (a, b) of
  (OFloat    a, OFloat    b) -> toObject (fn a b)
  (ORatio    a, ORatio    b) -> toObject (fn a b)
  (ODiffTime a, ODiffTime b) -> toObject (fn a b)
  (OComplex  a, OComplex  b) -> toObject (fn a b)

checkNumericOp
  :: String
  -> (forall a . Num a => a -> a -> a)
  -> Object
  -> Object
  -> PredicateIO st (FlowCtrl Object)
checkNumericOp op fn a b = promoteNum op a b >>= uncurry (withNum fn) >>= checkOK

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
  -> PredicateIO st (FlowCtrl Object)
checkBinumericOp op fnInt fnFrac a b = do
  (a, b) <- promoteNum op a b
  checkOK $ case (a, b) of
    (OInt     a, OInt     b) -> OInt      (fnInt  a b)
    (OWord    a, OWord    b) -> OWord     (fnInt  a b)
    (OLong    a, OLong    b) -> OLong     (fnInt  a b)
    (OFloat   a, OFloat   b) -> OFloat    (fnFrac a b)
    (OComplex a, OComplex b) -> OComplex  (fnFrac a b)

-- | Bitwise logical operations which work on 'Dao.Object.OInt's and 'Dao.Object.OWord's, which also
-- work on 'Dao.Object.OSet's, 'Dao.Object.OIntMap's, and 'Dao.Object.ODict's.
checkBitwiseOp
  :: String
  -> (forall a . Bits a => a -> a -> a)
  -> (T_set  -> T_set  -> T_set)
  -> (T_dict -> T_dict -> T_dict)
  -> (T_intMap -> T_intMap -> T_intMap)
  -> Object
  -> Object
  -> Check (FlowCtrl Object)
checkBitwiseOp op fnBit fnSet fnDict fnIntMap a b = do
  (a, b) <- promoteNum op a b
  checkOK $ case (a, b) of
    (OWord   a, OWord   b) -> OWord   (fnBit    a b)
    (OInt    a, OInt    b) -> OInt    (fnBit    a b)
    (OSet    a, OSet    b) -> OSet    (fnSet    a b)
    (ODict   a, ODict   b) -> ODict   (fnDict   a b)
    (OIntMap a, OIntMap b) -> OIntMap (fnIntMap a b)

-- | If the object is not null (as determined by the 'objToBool' function), then this object is
-- returned wrapped in the 'Data.Maybe.Just' data type. Otherwise, 'Data.Maybe.Nothing' is returned.
objToMaybe :: Object -> Maybe Object
objToMaybe o = if objToBool o then Just o else Nothing

-- | Comparator operators include @<@, @>@ @<=@, and @>=@.
checkCompareOp
  :: String
  -> (forall a . Ord a => a -> a -> Bool)
  -> Object
  -> Object
  -> Check (FlowCtrl Object)
checkCompareOp op fn a b = case (a, b) of
  (ONull      , ONull      ) -> done $ fn False False
  (OTrue      , OTrue      ) -> done $ fn True  True
  (ONull      , OTrue      ) -> done $ fn False True
  (OTrue      , ONull      ) -> done $ fn True  False
  (OString   a, OString   b) -> done $ fn a b
  (OTime     a, OTime     b) -> done $ fn a b
  (OChar     a, OChar     b) -> done $ fn a b
  (OPair     a, OPair     b) -> done $ fn a b
  (OList     a, OList     b) -> done $ fn a b
  (OSet      a, OSet      b) -> done $ fn a b
  (ODict     a, ODict     b) -> done $ fn a b
  (OIntMap   a, OIntMap   b) -> done $ fn a b
  (OTree     a, OTree     b) -> done $ fn a b
  (OGlob     a, OGlob     b) -> done $ fn a b
  (OScript   a, OScript   b) -> done $ fn a b
  (ORule     a, ORule     b) -> done $ fn a b
  (OBytes    a, OBytes    b) -> done $ fn a b
  _ -> promoteNum op a b >>= \ (a, b) -> done $ case (a, b) of
    (OInt      a, OInt      b) -> fn a b
    (OWord     a, OWord     b) -> fn a b
    (OLong     a, OLong     b) -> fn a b
    (OFloat    a, OFloat    b) -> fn a b
    (ORatio    a, ORatio    b) -> fn a b
    (OComplex  a, OComplex  b) -> fn a b
    (ODiffTime a, ODiffTime b) -> fn a b
  where { done = checkOK . boolToObj }

