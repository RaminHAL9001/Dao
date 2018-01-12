-- | This module provides a large number of useful default __Built-In Functions__ (BIFs) which you
-- can use to declare the 'Language.Interpreter.Dao.Kernel.builtins' field of the Dao Lisp execution
-- 'Language.Interpreter.Dao.Kernel.Environment', when evaluating the the 'evalDaoIO' function. The
-- function 'defaultBIFs' provided at the top of this module could be considered the Dao Lisp
-- "standard library" of functions.
--
-- Each one of these so-called "standard library" functions are provided as a stand-alone Haskell
-- function of type
-- @('Language.Interpreter.Dao.Kernel.Atom', 'Language.Interpreter.Dao.Kernel.Outliner' ('Language.Interpreter.Dao.Kernel.DaoEval' 'Language.Interpreter.Dao.Kernel.DaoExpr'))@
-- so that you may evaluate it directly with the 'evalDefaultBIF' function, or that you may use it in your
-- own customized dictionary of 'Language.Interpreter.Dao.Kernel.builtins' with a name of your
-- choosing.
module Language.Interpreter.Dao.DefaultBIFs
  ( -- * Setting-up the Environment of a Dao Interpreter
    newEnvironment, extendBuiltins, setupTraceBIF, setupTraceAtom, setupTraceForm,
    -- * Defining BIFs
    DefaultBIF, defaultBIFs, rename, evalDefaultBIF,
    -- * Arithmetic
    daoSum, daoSubtract, daoProduct, daoDivide, daoModulus,
    -- * Comparison
    daoEqual, daoNotEqual, daoLessEq, daoLess, daoGreaterEq, daoGreater,
    -- * Strings
    daoInterpolate,
    -- * Dictionaries
    daoGet, daoPut,
    -- * IO
    daoPrint,
    -- * Checks on Matched Pattern
    daoPredicate,
    -- * Exceptions
    daoThrow,
  )
  where

import           Language.Interpreter.Dao.Kernel

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans

import           Data.Foldable
--import           Data.List.NonEmpty (toList)
import qualified Data.Map     as Map
import qualified Data.Text    as Strict
import qualified Data.Text.IO as Strict

--import Debug.Trace

----------------------------------------------------------------------------------------------------

-- | Create a new default 'Environment' to be used to evaluate some Dao Lisp expressions. Pass an
-- update function composed (that is composed with the 'Prelude..' operator) of several setup
-- functions, like 'setupBuiltins', 'setupTraceBIF', 'setupTraceAtom', and 'setupTraceForm'.
--
-- Note that if you simply want to use the default 'Language.Interpreter.Dao.Kernel.Environment',
-- construct it by passing 'Prelude.id' to this function. That is to say
-- @('newEnvironment' 'Prelude.id')@ is the default environment.
newEnvironment :: (Environment -> Environment) -> Environment
newEnvironment = flip ($) emptyEnvironment{ builtins = defaultBIFs }

-- | Append builtin functions to a 'newEnvironment'. A 'newEnvironment' function provides an
-- environment that installs the 'defaultBIFs', but if any function you provide to 'extendBuiltins'
-- has the same name as any of the 'defaultBIFs', each of these identically named 'defaultBIFs' will
-- be replaced with your own version of the BIF.
-- 
-- If the list of BIFs passed to this function contains any duplicate named entries, a Haskell
-- exception is thrown. This is to notify you of a potential name conflict as quickly as possible,
-- hopefully as soon as your program begins execution and before anything expensive or destructive
-- happens.
--
-- Exceptions are not thrown from multiple calls to 'extendBuiltins' when name conflicts occur,
-- BIFs defined from prior calls to 'extendBuiltins' are silently overwritten.
extendBuiltins :: [(Atom, DaoBuiltinFunction)] -> (Environment -> Environment)
extendBuiltins elems env =
  env{ builtins = Map.union (Map.fromListWithKey noDups elems) (builtins env) } where
    noDups name _ _ = error $
      "Cannot init Dao Lisp interpreter, " ++
      "extendBuiltins called with multiple built-in functions named \"" ++
      show name ++ "\""

-- | Define a trace function in the 'newEnvironment' to be called when a Built-In Function (BIF) is
-- evaluated, for debugging purposes.
--
-- If this function is called multiple times, the last function given is used -- be careful, "last
-- function" depends on whether you use the dot @('Prelude..')@ function or the forward arrow
-- @('Control.Category.>>>')@ function. When using the forward arrow, the last line of Haskell code
-- you write is the "last function," when using the dot operator, the first line of Haskell code you
-- write is the "last function."
setupTraceBIF :: (Atom -> [DaoExpr] -> DaoEval ()) -> (Environment -> Environment)
setupTraceBIF f env = env{ traceBIF = Just f }

-- | Similar to 'setupTraceBIF', except this trace function is called whenever a non-Built-In
-- Function is called, with the 'Atom' that labels the function, and the list of 'DaoExpr' arguments
-- passed to it, usually a function defined in a Dao Lisp database.
setupTraceAtom :: (Atom -> [DaoExpr] -> DaoEval ()) -> (Environment -> Environment)
setupTraceAtom f env = env{ traceAtom = Just f }

-- | Very similar to 'setupTraceAtom': this trace function is called whenever a non-Built-In
-- Function is called, usually a function defined in a Dao Lisp database, the difference between
-- this and the function you define with 'setupTraceAtom' is that the entire 'Form' is given to this
-- function, not just the 'Atom' and it's arguments.
setupTraceForm :: (Form -> DaoEval ()) -> (Environment -> Environment)
setupTraceForm f env = env{ traceForm = Just f }

----------------------------------------------------------------------------------------------------

type DefaultBIF = (Atom, DaoBuiltinFunction)

-- | This is the statica global data structure which contains all of the BIFs defined in this
-- module. This table of BIFs is automatically included into your
-- 'Language.Interpreter.Dao.Kernel.Environment' when you construct one with the 'newEnvironment'
-- function.
defaultBIFs :: Map.Map Atom DaoBuiltinFunction
defaultBIFs = bifList $ fmap (uncurry bif)
  [ daoSum, daoSubtract, daoProduct, daoDivide , daoModulus
  , daoEqual, daoNotEqual, daoLess, daoLessEq, daoGreater, daoGreaterEq
  , daoInterpolate
  , daoPrint
  , daoThrow
  , daoPredicate
  , daoGet, daoPut
  ]

-- | A convenient function for renaming a 'DefaulBIF' before adding it to your own dictionary of
-- BIFs.
rename :: DefaultBIF -> Atom -> DefaultBIF
rename (_ , f) newname = (newname, f)

-- | A convenient function for evaluating a 'DefaultBIF' to a
-- 'Language.Interpreter.Dao.Kernel.DaoEval' function.
evalDefaultBIF :: DefaultBIF -> [DaoExpr] -> DaoEval DaoExpr
evalDefaultBIF = uncurry evalBIF

----------------------------------------------------------------------------------------------------

daoSum :: DefaultBIF
daoSum = bif "+" $ DaoStrict $
  argsDecodeAll (pure . DaoInt . sum) <|>
  argsDecodeAll (pure . DaoFloat . sum) <|>
  argsDecodeAll (pure . DaoString . mconcat) <|>
  argsDecodeAll (pure . dao . or) <|>
  argsDecodeAll (pure . DaoDict . unionDicts (\ _ _ a -> a))

daoProduct :: DefaultBIF
daoProduct = bif "*" $ DaoStrict $
  argsDecodeAll (pure . DaoInt . product) <|>
  argsDecodeAll (pure . DaoFloat . product) <|>
  argsDecodeAll (pure . dao . and) <|>
  argsDecodeAll (pure . DaoDict . intersectDicts (\ _ _ a -> a))

_nonassoc :: Num a => a -> (a -> a) -> (a -> a -> a) -> ([a] -> a) -> [a] -> a
_nonassoc id neg bin join = \ case { [] -> id; [a] -> neg a; a:ax -> a `bin` join ax; }

daoSubtract :: DefaultBIF
daoSubtract = bif "-" $ DaoStrict $
  argsDecodeAll (pure . DaoInt . _nonassoc 0 negate (-) sum) <|>
  argsDecodeAll (pure . DaoFloat . _nonassoc 0 negate (-) sum)

daoDivide :: DefaultBIF
daoDivide = bif "/" $ DaoStrict $
  argsDecodeAll (pure . DaoInt . _nonassoc 1 (\ x -> if x == 1 then 1 else 0) div product) <|>
  argsDecodeAll (pure . DaoFloat . _nonassoc 1 recip (/) product)

daoModulus :: DefaultBIF
daoModulus = bif "mod" $ DaoStrict $ matchAll $ \ case
  []   -> return $ DaoInt 1
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
    return $ case ax of
      []   -> DaoInt a
      a:ax -> DaoInt $ foldl mod a ax

-- | Join many strings and characters into a single string. Non-strings and non-characters passed to
-- this function are converted to a string value. Example:
--
-- > (interpolate "Hello" ',' " world! " One Two Three.)
-- 
-- The above will create a string @"Hello, world! OneTwoThree."@
daoInterpolate :: DefaultBIF
daoInterpolate = bif "interpolate" $ DaoStrict $ matchAll $
  pure . DaoString . strInterpolate

-- | Call 'daoInterpolate' and push the result to the system console's standard output stream.
daoPrint :: DefaultBIF
daoPrint = bif "print" $ DaoStrict $ matchAll $
  lift . daoVoid . liftIO . Strict.putStrLn . strInterpolate

-- | This function throws an uncatchable exception which immediately halts evaluation of a
-- 'Language.Interpreter.Dao.Kernel.DaoEval' function. Exceptions are, however, caught during
-- pattern matching when a pattern match evaluates a predicate function that evaluates this very
-- @throw@ function. If an exception is @throw@n during pattern matching, the pattern simply fails
-- and another pattern is tried.
daoThrow :: DefaultBIF
daoThrow = bif "throw" $ DaoNonStrict $
  (plainError <$> argDecode info <*> (dictAssocs <$> argDecode info) >>= lift . daoFail) <|>
  (matchAll $ lift . daoFail . plainError "user-error" . pure . (,) "args" . daoList)
  where
    info = [("function", DaoAtom "throw")]

-- | Intended to only be used in pattern matching. This function takes a partially applied form as a
-- parameter, applies the remaining arguments to evaluate the form as a function, and returns all
-- arguments if the form evaluation results in a 'Language.Interpreter.Dao.Kernel.DaoTrue' value.
-- Anything other value throws an exception which indicates that the pattern does not match.
--
-- The result is, a partial function call preceeded by a @?@ will test if arguments passed to the
-- function result in true, and if so the arguments may be assigned to a pattern match variable,
-- otherwise the pattern simply does not match.
--
-- Example
-- > (defn bigEnough (x) (<= 5 x))
-- > rule (type== Int * 1 (:numbers (? bigEnough))) (for x in numbers (print x " is big enough"))
--
-- Note the predicate creates a pattern that matches one or more integer values and assignes all
-- matching integers to a list called @numbers@. The list of numbers is also typed with the
-- @(? bigEnough)@ predicate, which is a function call to this very @?@ function that calls
-- @bigEnough@ with all values that match the pattern in that position.
daoPredicate :: DefaultBIF
daoPredicate = bif "?" $ DaoNonStrict $ do
  fn <- argDecode [("function", DaoAtom "?")]
  matchAll $ \ args -> do
    result <- lift $ evalNamedMacro fn args
    case result of
      DaoTrue -> return $ daoList args
      _       -> lift $ daoFail $ plainError "predicate"
        [ ("reason", DaoString "predicate did not evaluate to true")
        , ("function", DaoAtom fn)
        ]

_daoget
  :: (DaoDecode dict, DaoDecode key, Show key) -- TODO remove Show
  => (key -> dict -> Maybe DaoExpr)
  -> DaoMacro DaoExpr
_daoget lookup = do
  let getatom = DaoAtom "get"
  let badkey = DaoString "incorrect key type for lookup target"
  let info = [("reason", badkey), ("function", getatom)]
  (listLen, keys)  <- mplus
    (((,) Nothing) . pure <$> argDecode info)
    (do keysList <- argDecode info
        liftM ((,) $ Just $ length keysList) $
          forM (toList $ unwrapList keysList) $ \ key -> case daoDecode key of
            Right key -> return key
            Left  err -> matchQuit $ let info = errorAppendInfo in
              info "reason" badkey .
              info "function" getatom .
              info "bad-key" key $ err
    )
  dict  <- argEvalDeepDecode info
  deflt <- argEvalDeepDecode [] <|> return DaoVoid
  flip returnIfEnd [("function", getatom)] $ case listLen of
    Just len -> maybe DaoNull DaoList $
      sizedMaybeList len (maybe deflt id . flip lookup dict <$> keys) deflt
    Nothing  -> case keys of
      []       -> deflt
      key : _  -> maybe deflt id $ lookup key dict

-- | Perform a lookup on a dictionary or list, returning 'DaoVoid' or an optional @default-value@ if
-- the key does not exist. This is a 'DaoNonStrict' function which does not evaluate the key if it
-- is an 'Language.Interpreter.Dao.Kernel.Atom'.
--
-- > (get key dictionary default-value)
-- > (get index list default-value)
--
-- The dictionary and default values are evaluated before performing the lookup, but the key is not
-- evaluated unless it is a form. This allows you to specify an atom without it being looked up in
-- the local variable stack.
--
-- First parameter format: dictionary and a single key, with an optional default value to be
-- returned if the key is undefined. Returns the value associated with the key in the dictionary or
-- list.
--
-- Examples:
--
-- 1. Get's the value associated with the key @aaa@.
--
-- > (get aaa {:aaa 1})    ---> returns 1
--
-- 2. The key @bbb@ does not exist, so it returns the given default value zero @0@:
--
-- > (get bbb {:aaa 1} 0)  ---> returns 0
--
-- 3. Get the first element in the list, returns the 'Language.Interpreter.Dao.Kernel.Atom' @a@:
--
-- > (get 0 [a b c d])
--
-- 4. Get an undefined index:
-- 
-- > (get (+ 3 1) [a b c d] (atom nothing))    ---> returns ('Language.Interpreter.Dao.Kernel.DaoAtom' "nothing")
-- 
-- 5. Get an integer index, where the integer is assigned to a variable name, returns
--    @'Language.Intgerpreter.Dao.Kernel.DaoString' "one"@:
--
-- > (do let key = 1; get (@ key) ["zero" "one" "two" "three"];)
-- 
daoGet :: DefaultBIF
daoGet = bif "get" $ DaoNonStrict $
  _daoget lookupDict <|> _daoget indexList

_daoput
  :: (DaoDecode dict, DaoDecode key, DaoEncode dict, Show dict, Show key)
  => (dict -> [(key, DaoExpr)] -> dict)
  -> DaoMacro DaoExpr
_daoput update = do
  let info = [("function", DaoAtom "put")]
  pairs <-
    (argOutlineListWith info $ outlinePairsWith outlineDaoDecoder $ matchStep [] return) <|>
    (liftM pure $ (,) <$> argDecode info <*> argEvalDeepDecode info) <|>
    ( matchQuit $ plainError "arg-matching" $ info ++
        [("reason", DaoString "argument index+value pair must be single pair, or list of pairs")]
    )
  pairs <- forM pairs $ \ (atom, expr) -> ((,) atom) <$> lift (evalDeep expr)
  dict  <- argEvalDeepDecode info
  returnIfEnd (dao $ update dict pairs) info

-- | Perform an insertion on a dictionary or list. If the target is a list, the keys must be integer
-- values.
--
-- > (put key value dict)
-- > (put [:key1 value1 :key2 value2 :key3 value3 ...] dict)
--
-- Notice that when inserting multiple elements, you must specify a __square-bracketed__ list of
-- pairs, not a curly-bracketed dictionary.
daoPut :: DefaultBIF
daoPut = bif "put" $ DaoNonStrict $ _daoput insertDict <|> _daoput insertList

----------------------------------------------------------------------------------------------------

type CreasingFunction a = [a] -> DaoEval DaoExpr

comparing :: Atom -> (a -> a -> Bool) -> CreasingFunction a
comparing atom ord = \ case { [] -> err; a:ax -> return $ loop a ax; } where
  err = daoFail $ plainError "matching"
    [ ("function", DaoAtom atom)
    , ("reason", DaoString "no arguments to function")
    ]
  loop a ax = case ax of
    []   -> DaoTrue
    b:ax -> if a `ord` b then loop b ax else DaoNull

daoCompare
  :: Atom
  -> (forall a . (Ord a, DaoDecode a) => a -> a -> Bool)
  -> DefaultBIF
daoCompare atom f = (,) atom $ DaoStrict $
  argsDecodeAll (comparing atom f :: CreasingFunction Int) <|>
  argsDecodeAll (comparing atom f :: CreasingFunction Double) <|>
  argsDecodeAll (comparing atom f :: CreasingFunction Strict.Text)

daoEqual :: DefaultBIF
daoEqual = daoCompare "==" (==)

daoNotEqual :: DefaultBIF
daoNotEqual = daoCompare "/=" (/=)

daoLess :: DefaultBIF
daoLess = daoCompare "<" (<)

daoLessEq :: DefaultBIF
daoLessEq = daoCompare "<=" (<=)

daoGreater :: DefaultBIF
daoGreater = daoCompare ">" (>)

daoGreaterEq :: DefaultBIF
daoGreaterEq = daoCompare ">=" (>=)

