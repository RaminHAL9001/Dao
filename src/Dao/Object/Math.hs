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

module Dao.Object.Math where

import           Dao.Object

import           Numeric

import           Data.Maybe
import           Data.Int
import           Data.Word
import           Data.Bits
import           Data.Ratio
import           Data.Complex
import           Data.Time hiding (parseTime)

import           Control.Monad

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

objToIntegral :: Object -> Maybe T_long
objToIntegral o = case o of
  OWord o -> return $ toInteger o
  OInt  o -> return $ toInteger o
  OLong o -> return o
  _       -> mzero

objToRational :: Object -> Maybe T_ratio
objToRational o = case o of
  OWord     o -> return $ toRational o
  OInt      o -> return $ toRational o
  ODiffTime o -> return $ toRational o
  OFloat    o -> return $ toRational o
  OLong     o -> return $ toRational o
  ORatio    o -> return o
  _           -> mzero

instance Real Object where
  toRational o = fromMaybe (error "Object value is not a rational number") (objToRational o)

objToComplex :: Object -> Maybe T_complex
objToComplex o = case o of
  OComplex o -> return o
  o          -> objToRational o >>= \o -> return (fromRational o :+ 0)

fitIntToBounds :: (Integral a, Bounded a) => a -> a -> (a -> Object) -> T_long -> Maybe Object
fitIntToBounds minb maxb construct a =
  if fromIntegral minb <= a && a <= fromIntegral maxb
    then return (construct (fromIntegral a))
    else mzero

smallestIntContainer :: T_long -> Object
smallestIntContainer a = fromMaybe ONull $ msum $
  [ fitIntToBounds minBound maxBound OWord a
  , fitIntToBounds minBound maxBound OInt  a
  , return (OLong a)
  ]

objToFloat :: Object -> Maybe T_float
objToFloat o = case o of
  OFloat    f -> return f
  _           -> mzero

objToDiffTime :: Object -> Maybe T_diffTime
objToDiffTime o = case o of
  ODiffTime f -> return f
  _           -> mzero

instance Num Object where
  a + b = fromMaybe ONull $ msum $
    [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> return $ smallestIntContainer (a+b)
    , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a+b)
    , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ODiffTime (a+b)
    , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a+b)
    , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a+b)
    ]
  a - b = fromMaybe ONull $ msum $
    [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> return $ smallestIntContainer (a-b)
    , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a-b)
    , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ODiffTime (a-b)
    , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a-b)
    , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a-b)
    ]
  a * b = fromMaybe ONull $ msum $
    [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> return $ smallestIntContainer (a*b)
    , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a*b)
    , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ODiffTime (a*b)
    , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a*b)
    , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a*b)
    ]
  signum a = fromMaybe ONull $
    objToRational a >>= \a ->
      return (if a==0 then OInt 0 else if a>0 then OInt 1 else OInt (0-1))
  abs a = case a of
    OWord     a -> OWord a
    OInt      a -> OInt (abs a)
    OLong     a -> OLong (abs a)
    ODiffTime a -> ODiffTime (abs a)
    OFloat    a -> OFloat (abs a)
    ORatio    a -> ORatio (abs a)
    OComplex  a -> OFloat (magnitude a)
    _           -> ONull
  fromInteger = OLong
  negate a = case a of
    OInt      a -> OInt      (negate a)
    OLong     a -> OLong     (negate a)
    ODiffTime a -> ODiffTime (negate a)
    OFloat    a -> OFloat    (negate a)
    ORatio    a -> ORatio    (negate a)
    OComplex  a -> OComplex  (negate a)
    _           -> ONull

instance Fractional Object where
  a / b = fromMaybe ONull $ msum $
    [ case (a, b) of
        (OFloat a, OFloat b) -> return $ OFloat (a/b)
        _                    -> mzero
    , objToRational a >>= \a -> objToRational b >>= \b -> return (ORatio (a/b))
    , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return (OComplex (a/b))
    ]
  recip a = fromMaybe ONull $ msum $
    [ case a of
        OFloat   a -> return (OFloat   (recip a))
        OComplex a -> return (OComplex (recip a))
        _          -> mzero
    , fmap (ORatio . recip) (objToRational a)
    ]
  fromRational = ORatio

instance Floating Object where
  pi = OFloat pi
  exp a = fromMaybe ONull (mplus (fmap (OFloat . exp) (objToFloat a)) (fmap (OComplex . exp) (objToComplex a)))
  sqrt a = fromMaybe ONull (mplus (fmap (OFloat . sqrt) (objToFloat a)) (fmap (OComplex . sqrt) (objToComplex a)))
  log a = fromMaybe ONull (mplus (fmap (OFloat . log) (objToFloat a)) (fmap (OComplex . log) (objToComplex a)))
  sin a = fromMaybe ONull (mplus (fmap (OFloat . sin) (objToFloat a)) (fmap (OComplex . sin) (objToComplex a)))
  cos a = fromMaybe ONull (mplus (fmap (OFloat . cos) (objToFloat a)) (fmap (OComplex . cos) (objToComplex a)))
  tan a = fromMaybe ONull (mplus (fmap (OFloat . tan) (objToFloat a)) (fmap (OComplex . tan) (objToComplex a)))
  asin a = fromMaybe ONull (mplus (fmap (OFloat . asin) (objToFloat a)) (fmap (OComplex . asin) (objToComplex a)))
  acos a = fromMaybe ONull (mplus (fmap (OFloat . acos) (objToFloat a)) (fmap (OComplex . acos) (objToComplex a)))
  atan a = fromMaybe ONull (mplus (fmap (OFloat . atan) (objToFloat a)) (fmap (OComplex . atan) (objToComplex a)))
  sinh a = fromMaybe ONull (mplus (fmap (OFloat . sinh) (objToFloat a)) (fmap (OComplex . sinh) (objToComplex a)))
  cosh a = fromMaybe ONull (mplus (fmap (OFloat . cosh) (objToFloat a)) (fmap (OComplex . cosh) (objToComplex a)))
  tanh a = fromMaybe ONull (mplus (fmap (OFloat . tanh) (objToFloat a)) (fmap (OComplex . tanh) (objToComplex a)))
  asinh a = fromMaybe ONull (mplus (fmap (OFloat . asinh) (objToFloat a)) (fmap (OComplex . asinh) (objToComplex a)))
  acosh a = fromMaybe ONull (mplus (fmap (OFloat . acosh) (objToFloat a)) (fmap (OComplex . acosh) (objToComplex a)))
  atanh a = fromMaybe ONull (mplus (fmap (OFloat . atanh) (objToFloat a)) (fmap (OComplex . atanh) (objToComplex a)))
  a ** b = fromMaybe ONull $ msum $
    [ objToFloat a >>= \a -> objToFloat b >>= \b -> return (OFloat (a**b))
    , objToComplex a >>= \a -> objToComplex b >>= \b -> return (OComplex (a**b))
    ]
  logBase a b = fromMaybe ONull $ msum $
    [ objToFloat a >>= \a -> objToFloat b >>= \b -> return (OFloat (logBase a b))
    , objToComplex a >>= \a -> objToComplex b >>= \b -> return (OComplex (logBase a b))
    ]

non_int_value = error "Object value is not an integer"

instance Integral Object where
  toInteger o = fromMaybe non_int_value (objToIntegral o)
  quotRem a b = fromMaybe non_int_value $
    objToIntegral a >>= \a -> objToIntegral b >>= \b ->
      return (let (x,y) = quotRem a b in (smallestIntContainer x, smallestIntContainer y))

instance RealFrac Object where
  properFraction a = fromMaybe (error "Object value is not a real number") $ msum $
    [ objToIntegral a >>= \a -> return (fromIntegral a, OWord 0)
    , objToRational a >>= \a -> let (x, y) = properFraction a in return (fromIntegral x, ORatio y)
    ]

objToInt :: Object -> Maybe Int
objToInt a = objToIntegral a >>= \a ->
  if minInt <= a && a <= maxInt then return (fromIntegral a) else mzero
  where
    minInt = fromIntegral (minBound::Int)
    maxInt = fromIntegral (maxBound::Int)

-- | Used to implement 'Data.Bits.(.&.)', 'Data.Bits.(.|.)', and 'Data.Bits.xor'.
bitsOp2 :: (forall a . Bits a => a -> a -> a) -> Object -> Object -> Maybe Object
bitsOp2 fn a b = case (a, b) of
  (OInt  a, OInt  b) -> return (OInt  (fn a b))
  (OWord a, OWord b) -> return (OWord (fn a b))
  (OLong a, b      ) -> objToIntegral b >>= \b -> return (OLong (fn a b))
  (a      , OLong b) -> objToIntegral a >>= \a -> return (OLong (fn a b))
  _                  -> mzero

-- | Used to implement a version of 'Data.Bits.shift' and 'Data.Bits.rotate', but with an object as
-- the second parameter to these functions.
bitsMove :: (forall a . Bits a => a -> Int -> a) -> Object -> Object -> Maybe Object
bitsMove fn a b = objToInt b >>= \b -> bitsMoveInt fn a b

bitsMoveInt :: (forall a . Bits a => a -> Int -> a) -> Object -> Int -> Maybe Object
bitsMoveInt fn a b = case a of
  OInt  a -> return (OInt  (fn a b))
  OWord a -> return (OWord (fn a b))
  OLong a -> return (OLong (fn a b))
  _       -> mzero

-- | Used to implement a version of 'Data.Bits.testBit' but with an object as the second parameter
-- to these functions.
objTestBit :: Object -> Object -> Maybe Object
objTestBit a i = objToInt i >>= \i -> case a of
  OInt  a -> return (oBool (testBit a i))
  OWord a -> return (oBool (testBit a i))
  OLong a -> return (oBool (testBit a i))
  _       -> mzero

instance Bits Object where
  a .&. b = fromMaybe ONull (bitsOp2 (.&.) a b)
  a .|. b = fromMaybe ONull (bitsOp2 (.|.) a b)
  xor a b = fromMaybe ONull (bitsOp2  xor  a b)
  complement a = case a of
    OInt  a -> OInt  (complement a)
    OWord a -> OWord (complement a)
    OLong a -> OLong (complement a)
    _       -> ONull
  shift  a i = fromMaybe ONull (bitsMoveInt shift  a i)
  rotate a i = fromMaybe ONull (bitsMoveInt rotate a i)
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

