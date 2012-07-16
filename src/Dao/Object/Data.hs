-- "src/Dao/Object/Data.hs"  provides many functions and class
-- instantiations for working with Dao's "Object" data type.
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


{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.Data where

import           Dao.Types
import qualified Dao.Tree as T
import           Dao.Pattern
import           Dao.Object
import           Dao.Object.Monad

import           Control.Monad

import           Data.Typeable
import           Data.Dynamic
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Either
import           Data.List
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
  fromObject :: Monad m => Object -> ContErrT m a

-- | Pair an error message with an object that can help to describe what went wrong.
objectError :: Monad m => Object -> String -> ContErrT m err
objectError o msg = ceError (OPair (OString (ustr msg), o))

typeCheck :: Monad m => TypeID -> Object -> (Object -> ContErrT m a) -> ContErrT m a
typeCheck t o fn =
  let ot = objType o
      er = ustr ("expecting evaluation to objet of type "++show t++", actual type is "++show ot)
  in  if ot==t then fn o else ceError (OPair (OString er, OType ot))

instance Objectify ()      where
  toObject () = ONull
  fromObject o = typeCheck NullType o (\ ONull -> return ())

instance Objectify Bool    where
  toObject tf = if tf then OTrue else ONull
  fromObject o = case o of
    OTrue -> return True
    ONull -> return False
    o ->  let ot = objType o
              er = ustr ("expecting a true or false evaluation, actual type is "++show ot)
          in  ceError (OPair (OString er, OType ot))

instance Objectify TypeID  where
  toObject = OType
  fromObject o = typeCheck TypeType o (\ (OType o) -> return o)

instance Objectify Int64   where
  toObject = OInt  
  fromObject o = typeCheck IntType o (\ (OInt o) -> return o)

instance Objectify Word64  where
  toObject = OWord 
  fromObject o = typeCheck WordType o (\ (OWord  o) -> return o)

instance Objectify Integer where
  toObject = OLong 
  fromObject o = typeCheck LongType o (\ (OLong  o) -> return o)

instance Objectify (Ratio Integer) where
  toObject = ORatio
  fromObject o = typeCheck RatioType o (\ (ORatio o) -> return o)

instance Objectify (Complex Double) where
  toObject = OComplex
  fromObject o = typeCheck ComplexType o (\ (OComplex o) -> return o)

instance Objectify Double  where
  toObject = OFloat
  fromObject o = typeCheck FloatType o (\ (OFloat o) -> return o)

instance Objectify UTCTime where
  toObject = OTime  
  fromObject o = typeCheck TimeType o (\ (OTime   o) -> return o)

instance Objectify NominalDiffTime where
  toObject = ODiffTime
  fromObject o = typeCheck DiffTimeType o (\ (ODiffTime o) -> return o)

instance Objectify Char where
  toObject = OChar
  fromObject o = typeCheck CharType o (\ (OChar o) -> return o)

instance Objectify UStr where
  toObject = OString
  fromObject o = typeCheck StringType o (\ (OString o) -> return o)

instance Objectify [Char] where
  toObject = OString . ustr
  fromObject o = typeCheck StringType o (\ (OString o) -> return (uchars o))

instance Objectify [UStr] where
  toObject = ORef
  fromObject o = typeCheck StringType o (\ (ORef  o) -> return o)

instance Objectify (Object, Object) where
  toObject = OPair
  fromObject o = typeCheck PairType o (\ (OPair o) -> return o)

instance Objectify [Object] where
  toObject = OList
  fromObject o = typeCheck ListType o (\ (OList o) -> return o)

instance Objectify (S.Set Object) where
  toObject = OSet
  fromObject o = typeCheck SetType o (\ (OSet o) -> return o)

instance Objectify (M.Map Name Object) where
  toObject = ODict
  fromObject o = typeCheck DictType o (\ (ODict o) -> return o)

instance Objectify (I.IntMap Object) where
  toObject = OIntMap
  fromObject o = typeCheck IntMapType o (\ (OIntMap o) -> return o)

instance Objectify (T.Tree Name Object) where
  toObject = OTree
  fromObject o = typeCheck TreeType o (\ (OTree o) -> return o)

instance Objectify Pattern where
  toObject = OPattern
  fromObject o = typeCheck PatternType o (\ (OPattern o) -> return o)

instance Objectify Rule where
  toObject = ORule
  fromObject o = typeCheck RuleType o (\ (ORule o) -> return o)

instance Objectify B.ByteString where
  toObject = OBytes
  fromObject o = typeCheck BytesType o (\ (OBytes o) -> return o)

instance Objectify Script where
  toObject = OScript
  fromObject o = typeCheck ScriptType o (\ (OScript o) -> return o)

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
instance HasNull Pattern where
  nullValue = Pattern{getPatUnits = [], getPatternLength = 0}
  testNull o = getPatUnits o == []
instance HasNull ScriptExpr where { nullValue = NO_OP; testNull = (nullValue==) }
instance HasNull Script where
  nullValue = Script{scriptArgv = Com [], scriptCode = Com []}
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
  ORef       o -> testNull o
  OPair      _ -> True
  OList      o -> testNull o
  OSet       o -> testNull o
  OArray     o -> True
  ODict      o -> testNull o
  OIntMap    o -> testNull o
  OTree      o -> testNull o
  OPattern   o -> testNull o
  OScript    o -> testNull o
  ORule      o -> True
  OBytes     o -> testNull o

----------------------------------------------------------------------------------------------------

objSize :: Monad m => Object -> ContErrT m T_word
objSize o = case o of
  OString   o -> return (fromIntegral (U.length (toUTF8ByteString o)))
  ORef      o -> return (fromIntegral (length o))
  OPair     _ -> return 2
  OList     o -> return (fromIntegral (length o))
  OSet      o -> return (fromIntegral (S.size o))
  OArray    o -> return (fromIntegral (let (lo, hi) = bounds o in lo - hi))
  OIntMap   o -> return (fromIntegral (I.size o))
  ODict     o -> return (fromIntegral (M.size o))
  OTree     o -> return (T.size o)
  OPattern  o -> return (fromIntegral (length (getPatUnits o)))
  ORule     o -> return (fromIntegral (length (unComment (ruleAction  o))))
  OBytes    o -> return (fromIntegral (B.length o))
  _           ->
    let ot = objType o
    in  objectError (OType ot) (show ot++" type does not have \"size\" attribute")

objToList :: Monad m => Object -> ContErrT m [Object]
objToList o = case o of
  OPair (a, b) -> return [a, b]
  OString  o   -> return (map OChar (uchars o))
  OList    o   -> return o
  OSet     o   -> return (S.elems o)
  OArray   o   -> return (elems o)
  ODict    o   -> return (map (\ (a, b) -> OPair (OString a, b))             (M.assocs o))
  OIntMap  o   -> return (map (\ (a, b) -> OPair (OInt (fromIntegral a), b)) (I.assocs o))
  OTree    o   -> return (map (\ (a, b) -> OPair (OList (map OString a), b)) (T.assocs o))
  OPattern o   -> return (patternComponents o)
  _ ->
    let ot = objType o
    in  objectError (OType ot) (show ot++" type does not have \"asList\" attribute")

-- | Traverse the entire object, returning a list of all 'Dao.Object.OString' elements.
extractStringElems :: Object -> [UStr]
extractStringElems o = case o of
  OString  o   -> [o]
  OList    o   -> concatMap extractStringElems o
  OSet     o   -> concatMap extractStringElems (S.elems o)
  OArray   o   -> concatMap extractStringElems (elems o)
  ODict    o   -> concatMap extractStringElems (M.elems o)
  OIntMap  o   -> concatMap extractStringElems (I.elems o)
  OTree    o   -> concatMap extractStringElems (T.elems o)
  OPair (a, b) -> concatMap extractStringElems [a, b]
  _            -> []

patUnitToObj :: PatUnit -> Object
patUnitToObj p = case p of
  Wildcard -> OType ListType
  AnyOne   -> OType StringType
  Single o -> OString o

objToPatUnit :: Object -> PatUnit
objToPatUnit o = case o of
  OType StringType -> Wildcard
  OType ListType   -> AnyOne
  OString str      -> Single str
  _                -> error ("pattern contains object of invalid type: "++show (objType o))

objListToPattern :: [Object] -> Pattern
objListToPattern ox = loop 0 ox [] where
  loop i ox px = case ox of
    []   -> Pattern{getPatternLength = i, getPatUnits = px}
    o:ox -> loop (i+1) ox (px++[objToPatUnit o])

-- | Break a pattern into a list of it's component parts. 'Dao.Pattern.Wildcard's (the Kleene star
-- operation) translates to a 'ListType' object because wildcards may match a whole list of
-- strings and 'Dao.Pattern.AnyOne's translate to a StringType object because they can only match
-- one single string. Everything else translates to an 'OString' object.
patternComponents :: Pattern -> [Object]
patternComponents p = map patUnitToObj (getPatUnits p)

ruleComponents :: Rule -> Object
ruleComponents r =
  OPair ( OList (map (OList . patternComponents) (map unComment (unComment (rulePattern r))))
        , OScript (Script (Com []) (ruleAction r))
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

checkBounds :: (Monad m, Integral a, Integral b, Objectify a) => TypeID -> (a -> Bool) -> a -> ContErrT m b
checkBounds t check a =
  if check a
    then return (fromIntegral a)
    else ceError $ OPair $
           ( OPair (toObject a, OType t)
           , OString (ustr ("value out of range, cannot convert to "++show t++" type"))
           )

convertError :: Monad m => Object -> TypeID -> ContErrT m err
convertError o t = 
  let ot = objType o
  in  ceError $ OPair $
        ( OPair (OType ot, OType t)
        , OString (ustr (show ot++" types cannot be implicitly converted to "++show t++" types"))
        )

class ToWord a where { toWord :: Monad m => a -> ContErrT m T_word }
instance ToWord Word64  where { toWord = return }
instance ToWord Int64   where { toWord = checkBounds LongType in_word_range }
instance ToWord Integer where { toWord = checkBounds LongType in_word_range }
instance ToWord Object where
  toWord o = case o of
    OWord o -> return o
    OInt  o -> toWord o
    OLong o -> toWord o
    o -> convertError o WordType

class ToInt  a where { toInt :: Monad m => a -> ContErrT m T_int }
instance ToInt Word64  where { toInt = checkBounds IntType in_int_range }
instance ToInt Int64   where { toInt = return }
instance ToInt Integer where { toInt = checkBounds IntType in_int_range }
instance ToInt Object where
  toInt o = case o of
    OWord o -> toInt o
    OInt  o -> return o
    OLong o -> toInt o
    o -> convertError o IntType

class ToLong a where { toLong :: Monad m => a -> ContErrT m T_long }
instance ToLong Word64  where { toLong = return . fromIntegral }
instance ToLong Int64   where { toLong = return . fromIntegral }
instance ToLong Integer where { toLong = return . fromIntegral }
instance ToLong Object  where
  toLong o = case o of
    OWord o -> toLong o
    OInt  o -> toLong o
    OLong o -> return o
    o -> convertError o LongType

class ToFloat a where { toFloat :: Monad m => a -> ContErrT m T_float }
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
    o -> convertError o FloatType

class ToRatio a where { toRatio :: Monad m => a -> ContErrT m T_ratio }
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
    o -> convertError o RatioType

class ToDiffTime a where { toDiffTime :: Monad m => a -> ContErrT m T_diffTime }
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
    o -> convertError o DiffTimeType

class ToComplex a where { toComplex :: Monad m => a -> ContErrT m T_complex }
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
    o -> convertError o ComplexType

dissimilarTypes :: Monad m => String -> Object -> Object -> ContErrT m err
dissimilarTypes msg a b =
  let at = objType a
      bt = objType b
  in  objectError (OPair (OType at, OType bt)) $
          (if null msg then "" else show msg++" operator ")
        ++("cannot operate on dissimilar types "++show at++" and "++show bt)

-- | A table lookup that takes two numerical objects, and decides which object contains less
-- information, then converts the lesser object to the same type of object as the greater object.
-- For example, given an 'Object.OInt' and an 'Object.OFloat', the int will be converted to a float.
-- The types that can be converted are as follows in lesser-to-greater order:
--     'Object.OWord', 'Object.OInt', 'Object.OLong', 'Object.OFloat',
--     'Object.ORatio', 'Object.ODiffTime', 'Object.OComplex'
promoteNum :: Monad m => String -> Object -> Object -> ContErrT m (Object, Object)
promoteNum msg a b = msum [conv a b, dissimilarTypes msg a b] where
  rFst fn b = fmap (\a -> (toObject a, b)) fn
  rSnd a fn = fmap (\b -> (a, toObject b)) fn
  conv a b = case a of
    OWord      _ -> case b of
      OWord     _ -> return (           a,            b)
      OInt      _ -> rFst   (toInt      a)            b
      OLong     _ -> rFst   (toLong     a)            b
      OFloat    _ -> rFst   (toFloat    a)            b
      ORatio    _ -> rFst   (toRatio    a)            b
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
      _           -> ceError ONull
    OInt       _ -> case b of
      OWord     _ -> rSnd               a (toInt      b)
      OInt      _ -> return (           a,            b)
      OLong     _ -> rFst   (toLong     a)            b
      OFloat    _ -> rFst   (toFloat    a)            b
      ORatio    _ -> rFst   (toRatio    a)            b
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
      _           -> ceError ONull
    OLong      _ -> case b of
      OWord     _ -> rSnd               a (toLong     b)
      OInt      _ -> rSnd               a (toLong     b)
      OLong     _ -> return (           a,            b)
      OFloat    _ -> rFst   (toFloat    a)            b
      ORatio    _ -> rFst   (toRatio    a)            b
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
      _           -> ceError ONull
    OFloat     _ -> case b of
      OWord     _ -> rSnd               a (toFloat    b)
      OInt      _ -> rSnd               a (toFloat    b)
      OLong     _ -> rSnd               a (toFloat    b)
      OFloat    _ -> return (           a,            b)
      ORatio    _ -> rFst   (toRatio    a)            b
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
      _           -> ceError ONull
    ORatio     _ -> case b of
      OWord     _ -> rSnd               a (toRatio    b)
      OInt      _ -> rSnd               a (toRatio    b)
      OLong     _ -> rSnd               a (toRatio    b)
      OFloat    _ -> rSnd               a (toRatio    b)
      ORatio    _ -> return (           a,            b)
      ODiffTime _ -> rFst   (toDiffTime a)            b
      OComplex  _ -> rFst   (toComplex  a)            b
      _           -> ceError ONull
    ODiffTime  _ -> case b of
      OWord     _ -> rSnd               a (toDiffTime b)
      OInt      _ -> rSnd               a (toDiffTime b)
      OLong     _ -> rSnd               a (toDiffTime b)
      OFloat    _ -> rSnd               a (toDiffTime b)
      ORatio    _ -> rSnd               a (toDiffTime b)
      ODiffTime _ -> return (           a,            b)
      OComplex  _ -> rFst   (toComplex  a)            b
      _           -> ceError ONull
    OComplex   _ -> case b of
      OWord     _ -> rSnd               a (toComplex  b)
      OInt      _ -> rSnd               a (toComplex  b)
      OLong     _ -> rSnd               a (toComplex  b)
      OFloat    _ -> rSnd               a (toComplex  b)
      ORatio    _ -> rSnd               a (toComplex  b)
      ODiffTime _ -> rSnd               a (toComplex  b)
      OComplex  _ -> return (           a,            b)
      _           -> ceError ONull
    _            -> ceError ONull

-- | Execute a function that is a member of the class 'Prelude.Num' over two objects. These
-- functions include @Prelude.(+)@, @Prelude.(-)@, and @Prelude.(*)@.
withNum
  :: Monad m
  => (forall a . (Num a, Objectify a) => a -> a -> a)
  -> Object -> Object -> ContErrT m Object
withNum fn a b = case (a, b) of
  (OWord     a, OWord     b) -> return (toObject (fn a b))
  (OInt      a, OInt      b) -> return (toObject (fn a b))
  (OLong     a, OLong     b) -> return (toObject (fn a b))
  (OFloat    a, OFloat    b) -> return (toObject (fn a b))
  (ORatio    a, ORatio    b) -> return (toObject (fn a b))
  (ODiffTime a, ODiffTime b) -> return (toObject (fn a b))
  (OComplex  a, OComplex  b) -> return (toObject (fn a b))

-- | Execute a function that is a member of the class 'Prelude.Integral' over two objects. These
-- functions include 'Prelude.div', 'Prelude.mod', and 'Prelude.(^)'.
withIntegral
  :: Monad m
  => (forall a . (Objectify a, Integral a) => a -> a -> a)
  -> Object -> Object -> ContErrT m Object
withIntegral fn a b = case (a, b) of
  (OWord a, OWord b) -> return (toObject (fn a b))
  (OInt  a, OInt  b) -> return (toObject (fn a b))
  (OLong a, OLong b) -> return (toObject (fn a b))

-- | Execute a function that is a member of the class 'Prelude.Fractional' over two objects. These
-- functions include @Prelude.(/)@, @Prelude.(**)@, and 'Prelude.logBase'.
withFractional
  :: Monad m
  => (forall a . (Fractional a, Objectify a) => a -> a -> a)
  -> Object -> Object -> ContErrT m Object
withFractional fn a b = case (a, b) of
  (OFloat    a, OFloat    b) -> return (toObject (fn a b))
  (ORatio    a, ORatio    b) -> return (toObject (fn a b))
  (ODiffTime a, ODiffTime b) -> return (toObject (fn a b))
  (OComplex  a, OComplex  b) -> return (toObject (fn a b))

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

