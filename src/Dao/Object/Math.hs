-- "src/Dao/Object/Math.hs" instantiates the "Object" data into several
-- numeric classes, including Num, Floating, Real, Fractional, RealFrac,
-- Integral, and Bits.
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
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.Math where

import           Dao.Object
import           Dao.Glob
import           Dao.Predicate
import qualified Dao.Tree as T

import           Numeric

import           Data.Maybe
import           Data.Int
import           Data.Word
import           Data.Char
import           Data.Bits
import           Data.Ratio
import           Data.Complex
import           Data.Array.IArray
import           Data.Time hiding (parseTime)
import qualified Data.Set                  as S
import qualified Data.Map                  as M
import qualified Data.IntMap               as IM
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy      as B
import qualified Codec.Binary.UTF8.String  as UTF8

import           Control.Monad

----------------------------------------------------------------------------------------------------

boolToObj :: Bool -> Object
boolToObj b = if b then OTrue else ONull

objToBool :: Object -> Bool
objToBool obj = not (testNull obj)

testNull :: Object -> Bool
testNull o = case o of
  ONull             -> True
  OInt      0       -> True
  OWord     0       -> True
  OLong     0       -> True
  OFloat    0.0     -> True
  ORatio    o | o==(0%1)      -> True
  OComplex  o | o==(0:+0)     -> True
  ODiffTime o | toRational o == 0%1 -> True
  OChar     o | ord o == 0          -> True
  OString   o | o==nil              -> True
  ORef (GlobalRef [])          -> True
  ORef (QTimeRef  [])          -> True
  ORef (LocalRef  s)  | s==nil -> True
  ORef (StaticRef s)  | s==nil -> True
  OList        []        -> True
  OSet     o | S.null  o -> True
  ODict    o | M.null  o -> True
  OIntMap  o | IM.null o -> True
  OTree    o | o==T.Void -> True
  OGlob    o | null (getPatUnits o) -> True
  ORule    o | null (rulePattern o) && null (ruleAction o) -> True
  OBytes   o | B.null o  -> True
  _ -> False

objToList :: Object -> PValue tok [Object]
objToList o = case o of
  OPair (a, b) -> return $ [a, b]
  OString  o   -> return $ map OChar (uchars o)
  OList    o   -> return o
  OSet     o   -> return $ S.elems o
  OArray   o   -> return $ elems o
  ODict    o   -> return $ map (\ (a, b) -> OPair (OString a, b))             (M.assocs o)
  OIntMap  o   -> return $ map (\ (a, b) -> OPair (OInt (fromIntegral a), b)) (IM.assocs o)
  OTree    o   -> return $ map (\ (a, b) -> OPair (OList (map OString a), b)) (T.assocs o)
  OGlob    o   -> return $ patternComponents o
  _            -> mzero

-- | Break a pattern into a list of it's component parts. 'Dao.Glob.Wildcard's (the Kleene star
-- operation) translates to a 'ListType' object because wildcards may match a whole list of
-- strings and 'Dao.Glob.AnyOne's translate to a StringType object because they can only match
-- one single string. Everything else translates to an 'OString' object.
patternComponents :: Glob -> [Object]
patternComponents p = map patUnitToObj (getPatUnits p)

patUnitToObj :: GlobUnit -> Object
patUnitToObj p = case p of
  Wildcard -> OType ListType
  AnyOne   -> OType StringType
  Single o -> OString o

----------------------------------------------------------------------------------------------------

isNumeric :: Object -> Bool
isNumeric o = case o of
  OWord     _ -> True
  OInt      _ -> True
  OLong     _ -> True
  ODiffTime _ -> True
  OFloat    _ -> True
  ORatio    _ -> True
  OComplex  _ -> True
  _           -> False

isIntegral :: Object -> Bool
isIntegral o = case o of
  OWord _ -> True
  OInt  _ -> True
  OLong _ -> True
  _       -> False

isRational :: Object -> Bool
isRational o = case o of
  OWord     _ -> True
  OInt      _ -> True
  OLong     _ -> True
  ODiffTime _ -> True
  OFloat    _ -> True
  ORatio    _ -> True
  _           -> False

isFloating :: Object -> Bool
isFloating o = case o of
  OFloat   _ -> True
  OComplex _ -> True
  _          -> False

objToIntegral :: Object -> PValue tok T_long
objToIntegral o = case o of
  OWord o -> return $ toInteger o
  OInt  o -> return $ toInteger o
  OLong o -> return o
  _       -> mzero

objToRational :: Object -> PValue tok T_ratio
objToRational o = case o of
  OWord     o -> return $ toRational o
  OInt      o -> return $ toRational o
  ODiffTime o -> return $ toRational o
  OFloat    o -> return $ toRational o
  OLong     o -> return $ toRational o
  ORatio    o -> return o
  _           -> mzero

instance Real Object where
  toRational o = fromPValue (error "Object value is not a rational number") (objToRational o)

objToComplex :: Object -> PValue tok T_complex
objToComplex o = case o of
  OComplex o -> return o
  o          -> objToRational o >>= \o -> return (fromRational o :+ 0)

fitIntToBounds :: (Integral a, Bounded a) => a -> a -> (a -> Object) -> T_long -> PValue tok Object
fitIntToBounds minb maxb construct a =
  if fromIntegral minb <= a && a <= fromIntegral maxb
    then return (construct (fromIntegral a))
    else mzero

smallestIntContainer :: T_long -> PValue tok Object
smallestIntContainer a = msum $
  [ fitIntToBounds minBound maxBound OWord a
  , fitIntToBounds minBound maxBound OInt  a
  , return (OLong a)
  ]

objToFloat :: Object -> PValue tok T_float
objToFloat o = case o of
  OFloat    f -> return f
  _           -> mzero

objToDiffTime :: Object -> PValue tok T_diffTime
objToDiffTime o = case o of
  ODiffTime f -> return f
  _           -> mzero

type_mismatch f a = error $
    "type mismatch for \""++f++"\" operation, operands are of type "++show (objType a)

type_mismatch2 f a b = error $
    "type mismatch for \""++f++"\" operation, operands are of type "
  ++show (objType a)++" and "++show (objType b)

instance Num Object where
  a + b = fromPValue (type_mismatch2 "+" a b) $ msum $
    [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> smallestIntContainer (a+b)
    , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a+b)
    , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ODiffTime (a+b)
    , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a+b)
    , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a+b)
    ]
  a - b = fromPValue (type_mismatch2 "-" a b) $  msum $
    [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> smallestIntContainer (a-b)
    , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a-b)
    , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ODiffTime (a-b)
    , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a-b)
    , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a-b)
    ]
  a * b = fromPValue (type_mismatch2 "*" a b) $ msum $
    [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> smallestIntContainer (a*b)
    , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a*b)
    , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ODiffTime (a*b)
    , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a*b)
    , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a*b)
    ]
  signum a = fromPValue (type_mismatch "signum" a) $
    objToRational a >>= \a ->
      return (if a==0 then OInt 0 else if a>0 then OInt 1 else OInt (0-1))
  abs a = fromPValue (type_mismatch "abs" a) $ case a of
    OWord     a -> return $ OWord a
    OInt      a -> return $ OInt (abs a)
    OLong     a -> return $ OLong (abs a)
    ODiffTime a -> return $ ODiffTime (abs a)
    OFloat    a -> return $ OFloat (abs a)
    ORatio    a -> return $ ORatio (abs a)
    OComplex  a -> return $ OFloat (magnitude a)
    _           -> mzero
  fromInteger = OLong
  negate a = fromPValue (type_mismatch "negate" a) $ case a of
    OInt      a -> return $ OInt      (negate a)
    OLong     a -> return $ OLong     (negate a)
    ODiffTime a -> return $ ODiffTime (negate a)
    OFloat    a -> return $ OFloat    (negate a)
    ORatio    a -> return $ ORatio    (negate a)
    OComplex  a -> return $ OComplex  (negate a)
    _           -> mzero

instance Fractional Object where
  a / b = fromPValue (type_mismatch2 "/" a b) $ msum $
    [ case (a, b) of
        (OFloat a, OFloat b) -> return $ OFloat (a/b)
        _                    -> mzero
    , objToRational a >>= \a -> objToRational b >>= \b -> return (ORatio (a/b))
    , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return (OComplex (a/b))
      -- TODO: fail if types are wrong
    ]
  recip a = fromPValue (type_mismatch "recip" a) $ msum $
    [ case a of
        OFloat   a -> return (OFloat   (recip a))
        OComplex a -> return (OComplex (recip a))
        _          -> mzero
    , fmap (ORatio . recip) (objToRational a)
      -- TODO: fail if types are wrong
    ]
  fromRational = ORatio

instance Floating Object where
  pi = OFloat pi
  exp   a = fromPValue (type_mismatch "exp"   a) (mplus (fmap (OFloat . exp  ) (objToFloat a)) (fmap (OComplex . exp  ) (objToComplex a)))
  sqrt  a = fromPValue (type_mismatch "sqrt"  a) (mplus (fmap (OFloat . sqrt ) (objToFloat a)) (fmap (OComplex . sqrt ) (objToComplex a)))
  log   a = fromPValue (type_mismatch "log"   a) (mplus (fmap (OFloat . log  ) (objToFloat a)) (fmap (OComplex . log  ) (objToComplex a)))
  sin   a = fromPValue (type_mismatch "sin"   a) (mplus (fmap (OFloat . sin  ) (objToFloat a)) (fmap (OComplex . sin  ) (objToComplex a)))
  cos   a = fromPValue (type_mismatch "cos"   a) (mplus (fmap (OFloat . cos  ) (objToFloat a)) (fmap (OComplex . cos  ) (objToComplex a)))
  tan   a = fromPValue (type_mismatch "tan"   a) (mplus (fmap (OFloat . tan  ) (objToFloat a)) (fmap (OComplex . tan  ) (objToComplex a)))
  asin  a = fromPValue (type_mismatch "asin"  a) (mplus (fmap (OFloat . asin ) (objToFloat a)) (fmap (OComplex . asin ) (objToComplex a)))
  acos  a = fromPValue (type_mismatch "acos"  a) (mplus (fmap (OFloat . acos ) (objToFloat a)) (fmap (OComplex . acos ) (objToComplex a)))
  atan  a = fromPValue (type_mismatch "atan"  a) (mplus (fmap (OFloat . atan ) (objToFloat a)) (fmap (OComplex . atan ) (objToComplex a)))
  sinh  a = fromPValue (type_mismatch "sinh"  a) (mplus (fmap (OFloat . sinh ) (objToFloat a)) (fmap (OComplex . sinh ) (objToComplex a)))
  cosh  a = fromPValue (type_mismatch "cosh"  a) (mplus (fmap (OFloat . cosh ) (objToFloat a)) (fmap (OComplex . cosh ) (objToComplex a)))
  tanh  a = fromPValue (type_mismatch "tanh"  a) (mplus (fmap (OFloat . tanh ) (objToFloat a)) (fmap (OComplex . tanh ) (objToComplex a)))
  asinh a = fromPValue (type_mismatch "asinh" a) (mplus (fmap (OFloat . asinh) (objToFloat a)) (fmap (OComplex . asinh) (objToComplex a)))
  acosh a = fromPValue (type_mismatch "acosh" a) (mplus (fmap (OFloat . acosh) (objToFloat a)) (fmap (OComplex . acosh) (objToComplex a)))
  atanh a = fromPValue (type_mismatch "atanh" a) (mplus (fmap (OFloat . atanh) (objToFloat a)) (fmap (OComplex . atanh) (objToComplex a)))
  a ** b = fromPValue (type_mismatch2 "**" a b) $ msum $
    [ objToFloat a >>= \a -> objToFloat b >>= \b -> return (OFloat (a**b))
    , objToComplex a >>= \a -> objToComplex b >>= \b -> return (OComplex (a**b))
    ]
  logBase a b = fromPValue (type_mismatch2 "logBase" a b) $ msum $
    [ objToFloat a >>= \a -> objToFloat b >>= \b -> return (OFloat (logBase a b))
    , objToComplex a >>= \a -> objToComplex b >>= \b -> return (OComplex (logBase a b))
    ]

instance Integral Object where
  toInteger o = fromPValue (type_mismatch "toInteger" o) $ objToIntegral o
  quotRem a b = fromPValue (type_mismatch2 "quotRem" a b) $ do
    a <- objToIntegral a
    b <- objToIntegral b
    let (x,y) = quotRem a b
    a <- smallestIntContainer x
    b <- smallestIntContainer y
    return (a, b)

instance RealFrac Object where
  properFraction a = fromPValue (type_mismatch "properFraction" a) $ msum $
    [ objToIntegral a >>= \a -> return (fromIntegral a, OWord 0)
    , objToRational a >>= \a -> let (x, y) = properFraction a in return (fromIntegral x, ORatio y)
    ]

objToInt :: Object -> PValue tok Int
objToInt a = objToIntegral a >>= \a ->
  if minInt <= a && a <= maxInt then return (fromIntegral a) else mzero
  where
    minInt = fromIntegral (minBound::Int)
    maxInt = fromIntegral (maxBound::Int)

-- | Used to implement 'Data.Bits.(.&.)', 'Data.Bits.(.|.)', and 'Data.Bits.xor'.
bitsOp2 :: (forall a . Bits a => a -> a -> a) -> Object -> Object -> PValue tok Object
bitsOp2 fn a b = case (a, b) of
  (OInt  a, OInt  b) -> return (OInt  (fn a b))
  (OWord a, OWord b) -> return (OWord (fn a b))
  (OLong a, b      ) -> objToIntegral b >>= \b -> return (OLong (fn a b))
  (a      , OLong b) -> objToIntegral a >>= \a -> return (OLong (fn a b))
  _                  -> mzero

-- | Used to implement a version of 'Data.Bits.shift' and 'Data.Bits.rotate', but with an object as
-- the second parameter to these functions.
bitsMove :: (forall a . Bits a => a -> Int -> a) -> Object -> Object -> PValue tok Object
bitsMove fn a b = objToInt b >>= \b -> bitsMoveInt fn a b

bitsMoveInt :: (forall a . Bits a => a -> Int -> a) -> Object -> Int -> PValue tok Object
bitsMoveInt fn a b = case a of
  OInt  a -> return (OInt  (fn a b))
  OWord a -> return (OWord (fn a b))
  OLong a -> return (OLong (fn a b))
  _       -> mzero

-- | Used to implement a version of 'Data.Bits.testBit' but with an object as the second parameter
-- to these functions.
objTestBit :: Object -> Object -> PValue tok Object
objTestBit a i = objToInt i >>= \i -> case a of
  OInt  a -> return (oBool (testBit a i))
  OWord a -> return (oBool (testBit a i))
  OLong a -> return (oBool (testBit a i))
  _       -> mzero

instance Bits Object where
  a .&. b = fromPValue (type_mismatch "&" a b) $ bitsOp2 (.&.) a b
  a .|. b = fromPValue (type_mismatch "|" a b) $ bitsOp2 (.|.) a b
  xor a b = fromPValue (type_mismatch "^" a b) $ bitsOp2  xor  a b
  complement a = fromPValue (type_mismatch "complement" a) $ case a of
    OInt  a -> return $ OInt  (complement a)
    OWord a -> return $ OWord (complement a)
    OLong a -> return $ OLong (complement a)
    _       -> mzero
  shift  a i = fromPValue (type_mismatch "shift"  a) $ bitsMoveInt shift  a i
  rotate a i = fromPValue (type_mismatch "rotate" a) $ bitsMoveInt rotate a i
  bitSize a = case a of
    OInt  a -> bitSize a
    OWord a -> bitSize a
    OLong a -> bitSize a
    _       -> 0-1
  testBit a i = case a of
    OInt  a -> testBit a i
    OWord a -> testBit a i
    OLong a -> testBit a i
    _       -> False
  isSigned a = case a of
    OInt  _ -> True
    OLong _ -> True
    _       -> False
  bit i = OLong (bit i)
  popCount a = case a of
    OInt  a -> popCount a
    OWord a -> popCount a
    OLong a -> popCount a
    _       -> 0-1

