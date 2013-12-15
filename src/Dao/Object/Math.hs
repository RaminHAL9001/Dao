-- "src/Dao/Object/Math.hs" instantiates the "Object" data into several
-- numeric classes, including Num, Floating, Real, Fractional, RealFrac,
-- Integral, and Bits.
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
{-# LANGUAGE FlexibleInstances #-}

module Dao.Object.Math where

import           Dao.Object

type_mismatch :: String -> Value o -> error
type_mismatch f a = error $
    "type mismatch for \""++f++"\" operation, operands are of type "++show (objType a)

type_mismatch2 :: String -> Value o -> Value o -> error
type_mismatch2 f a b = error $
    "type mismatch for \""++f++"\" operation, operands are of type "
  ++show (objType a)++" and "++show (objType b)

--instance Num Object where
--a + b = fromPredicate (type_mismatch2 "+" a b) $ msum $
--  [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> smallestIntContainer (a+b)
--  , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a+b)
--  , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ORelTime (a+b)
--  , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a+b)
--  , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a+b)
--  ]
--a - b = fromPredicate (type_mismatch2 "-" a b) $  msum $
--  [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> smallestIntContainer (a-b)
--  , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a-b)
--  , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ORelTime (a-b)
--  , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a-b)
--  , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a-b)
--  ]
--a * b = fromPredicate (type_mismatch2 "*" a b) $ msum $
--  [ objToIntegral a >>= \a -> objToIntegral b >>= \b -> smallestIntContainer (a*b)
--  , objToFloat    a >>= \a -> objToFloat    b >>= \b -> return $ OFloat (a*b)
--  , objToDiffTime a >>= \a -> objToDiffTime b >>= \b -> return $ ORelTime (a*b)
--  , objToRational a >>= \a -> objToRational b >>= \b -> return $ ORatio (a*b)
--  , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return $ OComplex (a*b)
--  ]
--signum a = fromPredicate (type_mismatch "signum" a) $
--  objToRational a >>= \a ->
--    return (if a==0 then OInt 0 else if a>0 then OInt 1 else OInt (0-1))
--abs a = fromPredicate (type_mismatch "abs" a) $ case a of
--  OWord     a -> return $ OWord a
--  OInt      a -> return $ OInt (abs a)
--  OLong     a -> return $ OLong (abs a)
--  ORelTime a -> return $ ORelTime (abs a)
--  OFloat    a -> return $ OFloat (abs a)
--  ORatio    a -> return $ ORatio (abs a)
--  OComplex  a -> return $ OFloat (magnitude a)
--  _           -> mzero
--fromInteger = OLong
--negate a = fromPredicate (type_mismatch "negate" a) $ case a of
--  OInt      a -> return $ OInt      (negate a)
--  OLong     a -> return $ OLong     (negate a)
--  ORelTime a -> return $ ORelTime (negate a)
--  OFloat    a -> return $ OFloat    (negate a)
--  ORatio    a -> return $ ORatio    (negate a)
--  OComplex  a -> return $ OComplex  (negate a)
--  _           -> mzero

--instance Fractional Object where
--a / b = fromPredicate (type_mismatch2 "/" a b) $ msum $
--  [ case (a, b) of
--      (OFloat a, OFloat b) -> return $ OFloat (a/b)
--      _                    -> mzero
--  , objToRational a >>= \a -> objToRational b >>= \b -> return (ORatio (a/b))
--  , objToComplex  a >>= \a -> objToComplex  b >>= \b -> return (OComplex (a/b))
--    -- TODO: fail if types are wrong
--  ]
--recip a = fromPredicate (type_mismatch "recip" a) $ msum $
--  [ case a of
--      OFloat   a -> return (OFloat   (recip a))
--      OComplex a -> return (OComplex (recip a))
--      _          -> mzero
--  , fmap (ORatio . recip) (objToRational a)
--    -- TODO: fail if types are wrong
--  ]
--fromRational = ORatio

--instance Floating Object where
--pi = OFloat pi
--exp   a = fromPredicate (type_mismatch "exp"   a) (mplus (fmap (OFloat . exp  ) (objToFloat a)) (fmap (OComplex . exp  ) (objToComplex a)))
--sqrt  a = fromPredicate (type_mismatch "sqrt"  a) (mplus (fmap (OFloat . sqrt ) (objToFloat a)) (fmap (OComplex . sqrt ) (objToComplex a)))
--log   a = fromPredicate (type_mismatch "log"   a) (mplus (fmap (OFloat . log  ) (objToFloat a)) (fmap (OComplex . log  ) (objToComplex a)))
--sin   a = fromPredicate (type_mismatch "sin"   a) (mplus (fmap (OFloat . sin  ) (objToFloat a)) (fmap (OComplex . sin  ) (objToComplex a)))
--cos   a = fromPredicate (type_mismatch "cos"   a) (mplus (fmap (OFloat . cos  ) (objToFloat a)) (fmap (OComplex . cos  ) (objToComplex a)))
--tan   a = fromPredicate (type_mismatch "tan"   a) (mplus (fmap (OFloat . tan  ) (objToFloat a)) (fmap (OComplex . tan  ) (objToComplex a)))
--asin  a = fromPredicate (type_mismatch "asin"  a) (mplus (fmap (OFloat . asin ) (objToFloat a)) (fmap (OComplex . asin ) (objToComplex a)))
--acos  a = fromPredicate (type_mismatch "acos"  a) (mplus (fmap (OFloat . acos ) (objToFloat a)) (fmap (OComplex . acos ) (objToComplex a)))
--atan  a = fromPredicate (type_mismatch "atan"  a) (mplus (fmap (OFloat . atan ) (objToFloat a)) (fmap (OComplex . atan ) (objToComplex a)))
--sinh  a = fromPredicate (type_mismatch "sinh"  a) (mplus (fmap (OFloat . sinh ) (objToFloat a)) (fmap (OComplex . sinh ) (objToComplex a)))
--cosh  a = fromPredicate (type_mismatch "cosh"  a) (mplus (fmap (OFloat . cosh ) (objToFloat a)) (fmap (OComplex . cosh ) (objToComplex a)))
--tanh  a = fromPredicate (type_mismatch "tanh"  a) (mplus (fmap (OFloat . tanh ) (objToFloat a)) (fmap (OComplex . tanh ) (objToComplex a)))
--asinh a = fromPredicate (type_mismatch "asinh" a) (mplus (fmap (OFloat . asinh) (objToFloat a)) (fmap (OComplex . asinh) (objToComplex a)))
--acosh a = fromPredicate (type_mismatch "acosh" a) (mplus (fmap (OFloat . acosh) (objToFloat a)) (fmap (OComplex . acosh) (objToComplex a)))
--atanh a = fromPredicate (type_mismatch "atanh" a) (mplus (fmap (OFloat . atanh) (objToFloat a)) (fmap (OComplex . atanh) (objToComplex a)))
--a ** b = fromPredicate (type_mismatch2 "**" a b) $ msum $
--  [ objToFloat a >>= \a -> objToFloat b >>= \b -> return (OFloat (a**b))
--  , objToComplex a >>= \a -> objToComplex b >>= \b -> return (OComplex (a**b))
--  ]
--logBase a b = fromPredicate (type_mismatch2 "logBase" a b) $ msum $
--  [ objToFloat a >>= \a -> objToFloat b >>= \b -> return (OFloat (logBase a b))
--  , objToComplex a >>= \a -> objToComplex b >>= \b -> return (OComplex (logBase a b))
--  ]

-- | Used to implement 'Data.Bits.(.&.)', 'Data.Bits.(.|.)', and 'Data.Bits.xor'.
--bitsOp2 :: (forall a . Bits a => a -> a -> a) -> Object -> Object -> Predicate tok Object
--bitsOp2 fn a b = case (a, b) of
--(OInt  a, OInt  b) -> return (OInt  (fn a b))
--(OWord a, OWord b) -> return (OWord (fn a b))
--(OLong a, b      ) -> objToIntegral b >>= \b -> return (OLong (fn a b))
--(a      , OLong b) -> objToIntegral a >>= \a -> return (OLong (fn a b))
--_                  -> mzero
