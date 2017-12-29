-- | This module provides a large number of useful default Built-In Functions (BIFs) which you can
-- use to declare the 'Language.Interpreter.Dao.Kernel.builtins' field of the Dao Lisp execution
-- 'Language.Interpreter.Dao.Kernel.Environment', when evaluating the the 'evalDaoIO' function. The
-- function 'defaultBIFs' provided at the top of this module could be considered the Dao Lisp
-- "standard library" of functions.
--
-- Each one of these so-called "standard library" functions are provided as a stand-alone Haskell
-- function of type
-- @('Language.Interpreter.Dao.Kernel.Atom', 'Language.Interpreter.Dao.Kernel.Outliner' ('Language.Interpreter.Dao.Kernel.DaoEval' 'Language.Interpreter.Dao.Kernel.DaoExpr'))@
-- so that you may evaluate it directly with the 'evalBIF' function, or that you may use it in your
-- own customized dictionary of 'Language.Interpreter.Dao.Kernel.builtins' with a name of your
-- choosing.
module Language.Interpreter.Dao.DefaultBIFs
  ( DefaultBIF, defaultBIFs, rename,
  )
  where

import           Language.Interpreter.Dao.Kernel

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.Map     as Map
import qualified Data.Text    as Strict
import qualified Data.Text.IO as Strict

----------------------------------------------------------------------------------------------------

type DefaultBIF = (Atom, Outliner (DaoEval DaoExpr))

defaultBIFs :: Map.Map Atom DaoLispBuiltin
defaultBIFs = bifList $ fmap (uncurry bif)
  [ daoSum
  , daoIncreasing
  , daoDecreasing
  , daoInterpolate
  , daoPrint
  , daoThrow
  , daoPredicate
  ]

-- | A convenient function for renaming a 'DefaulBIF' before adding it to your own dictionary of
-- BIFs.
rename :: DefaultBIF -> Atom -> DefaultBIF
rename (_ , f) newname = (newname, f)

-- | A convenient function for evaluating a 'DefaultBIF' to a
-- 'Language.Interpreter.Dao.Kernel.DaoEval' function.
evalBIF :: DefaultBIF -> [DaoExpr] -> DaoEval DaoExpr
evalBIF = snd . uncurry bif

----------------------------------------------------------------------------------------------------

daoSum :: DefaultBIF
daoSum = (,) "+" $
  monotypeArgList (pure . DaoInt . sum) <|>
  monotypeArgList (pure . DaoFloat . sum) <|>
  monotypeArgList (pure . DaoString . mconcat)

daoInterpolate :: DefaultBIF
daoInterpolate = (,) "interpolate" $
  dumpArgs $ pure . liftM (DaoString . strInterpolate) . filterEvalForms

daoPrint :: DefaultBIF
daoPrint = (,) "print" $ dumpArgs $ pure . daoVoid .
  (filterEvalForms >=> liftIO . Strict.putStrLn . strInterpolate)

daoThrow :: DefaultBIF
daoThrow = (,) "throw" $
  (daoFail <$> (plainError <$> outlineDaoDecoder <*> (dictAssocs <$> outlineDaoDecoder))) <|>
  (dumpArgs $ return . daoFail . plainError "user-error" . pure . (,) "args" . daoList)

daoPredicate :: DefaultBIF
daoPredicate = (,) "?" $ do
  fn <- outlineDaoDecoder
  dumpArgs $ \ args -> return $ do
    result <- evalAtomWith fn args
    case result of
      DaoTrue -> return $ daoList args
      _       -> daoFail $ plainError "predicate" $
        [("reason", DaoString "match did not evaluate to true")]

----------------------------------------------------------------------------------------------------

creasing :: (a -> a -> Bool) -> [a] -> Bool
creasing ordered = \ case { [] -> False; a:ax -> loop a ax; } where
  loop a ax = case ax of
    []   -> True
    b:ax -> if a `ordered` b then loop b ax else False

daoCreasing :: (forall a . (Ord a, DaoDecode a) => a -> a -> Bool) -> Outliner (DaoEval DaoExpr)
daoCreasing f =
  monotypeArgList (pure . dao . (creasing f :: [Int] -> Bool)) <|>
  monotypeArgList (pure . dao . (creasing f :: [Double] -> Bool)) <|>
  monotypeArgList (pure . dao . (creasing f :: [Strict.Text] -> Bool))

daoIncreasing :: DefaultBIF
daoIncreasing = (,) "<=" $ daoCreasing (<=)

daoDecreasing :: DefaultBIF
daoDecreasing = (,) ">=" $ daoCreasing (>=)

