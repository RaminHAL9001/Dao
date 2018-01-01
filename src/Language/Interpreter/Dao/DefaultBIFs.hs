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
  ( DefaultBIF, defaultBIFs, rename, evalBIF,
    -- * Stand-Alone BIFs
    daoSum, daoSubtract, daoProduct, daoDivide, daoModulus, daoIncreasing, daoDecreasing,
    daoInterpolate, daoPrint, daoThrow, daoPredicate, daoGet
  )
  where

import           Language.Interpreter.Dao.Kernel

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.List.NonEmpty (toList)
import qualified Data.Map     as Map
import qualified Data.Text    as Strict
import qualified Data.Text.IO as Strict

----------------------------------------------------------------------------------------------------

type DefaultBIF = (Atom, Outliner (DaoEval DaoExpr))

defaultBIFs :: Map.Map Atom DaoLispBuiltin
defaultBIFs = bifList $ fmap (uncurry bif)
  [ daoSum
  , daoSubtract
  , daoProduct
  , daoDivide
  , daoModulus
  , daoIncreasing
  , daoDecreasing
  , daoInterpolate
  , daoPrint
  , daoThrow
  , daoPredicate
  , daoGet
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

daoProduct :: DefaultBIF
daoProduct = (,) "*" $
  monotypeArgList (pure . DaoInt . product) <|>
  monotypeArgList (pure . DaoFloat . product)

_nonassoc :: Num a => a -> (a -> a) -> (a -> a -> a) -> ([a] -> a) -> [a] -> a
_nonassoc id neg bin join = \ case { [] -> id; [a] -> neg a; a:ax -> a `bin` join ax; }

daoSubtract :: DefaultBIF
daoSubtract = (,) "-" $
  monotypeArgList (pure . DaoInt . _nonassoc 0 negate (-) sum) <|>
  monotypeArgList (pure . DaoFloat . _nonassoc 0 negate (-) sum)

daoDivide :: DefaultBIF
daoDivide = (,) "/" $
  monotypeArgList (pure . DaoInt . _nonassoc 1 (\ x -> if x == 1 then 1 else 0) div product) <|>
  monotypeArgList (pure . DaoFloat . _nonassoc 1 recip (/) product)

daoModulus :: DefaultBIF
daoModulus = (,) "mod" $ dumpArgs $ \ case
  []   -> return $ pure $ DaoInt 1
  a:ax -> do
    let err param = plainError "matching"
          [ ("reason", DaoString "modulus require integer parameters")
          , ("offender", param)
          ]
    a  <- case a of { DaoInt a -> return a; param -> matchQuit $ err param; }
    ax <- forM ax $ \ case
      DaoInt 0 -> matchQuit $ plainError "matching"
        [ ("reason", DaoString "divide by zero")
        , ("function", DaoAtom "mod")
        ]
      DaoInt a -> return a
      param    -> matchQuit $ err param
    return $ pure $ case ax of
      []   -> DaoInt a
      a:ax -> DaoInt $ foldl mod a ax

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

-- | Intended to only be used in pattern matching. This function takes a partially applied form as a
-- parameter, applies the remaining arguments to evaluate the form as a function, and returns all
-- arguments if the form evaluation results in a 'Language.Interpreter.Dao.Kernel.DaoTrue' value.
-- Anything other value throws an exception which indicates that the pattern does not match.
--
-- The result is, a partial function call preceeded by a @?@ will test if arguments passed to the
-- function result in true, and if so the arguments may be assigned to a pattern match variable,
-- otherwise the pattern simply does not match.
daoPredicate :: DefaultBIF
daoPredicate = (,) "?" $ do
  fn <- outlineDaoDecoder
  dumpArgs $ \ args -> return $ do
    result <- evalAtomWith fn args
    case result of
      DaoTrue -> return $ daoList args
      _       -> daoFail $ plainError "predicate"
        [ ("reason", DaoString "predicate did not evaluate to true")
        , ("function", DaoAtom fn)
        ]

_daoget
  :: (DaoDecode dict, DaoDecode key)
  => (dict -> key -> Maybe DaoExpr)
  -> Outliner (DaoEval DaoExpr)
_daoget lookup = do
  dict  <- outlineDaoDecoder
  let getatom = DaoAtom "get"
  (listLen, keys)  <- (((,) Nothing) . pure <$> outlineDaoDecoder) <|>
    (outlineExprEq DaoNull $ const $ return (Just 0, [])) <|>
    (do keysList <- outlineDaoDecoder
        liftM ((,) $ Just $ length keysList) $
          forM (toList $ unwrapList keysList) $ \ key -> case daoDecode key of
            Right key -> return key
            Left  err -> matchQuit $ let info = errorAppendInfo in
              info "reason" (DaoString "incorrect key type for lookup target") .
              info "function" getatom .
              info "offender" key $ err
    )
  deflt <- matchStep [] return <|> return DaoVoid
  flip returnIfEnd [("function", getatom)] $ pure $ case listLen of
    Just len -> maybe DaoNull DaoList $
      sizedMaybeList len (maybe deflt id . lookup dict <$> keys) deflt
    Nothing  -> case keys of
      []       -> deflt
      key : _  -> maybe deflt id $ lookup dict key

daoGet :: DefaultBIF
daoGet = (,) "get" $ _daoget lookupDict <|> _daoget indexList

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

