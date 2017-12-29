-- | This module contains the kernel of the Dao Lisp language engine: 
--
-- 1. the Abstract Syntax Tree (AST) but not the database file format,
--
-- 2. the primitive data types,
--
-- 3. type classes for converting Haskell data types to and from a Dao Lisp AST,
--
-- 4. the interpreter: functions like 'evalDaoExprIO' for interpreting the syntax tree in a running
--    program,
--
-- 5. functions for defining your own Built-In Functions (BIFs) in Haskell to be called from within
--    a Dao Lisp interpreter thread.
--
-- Dao Lisp might be simple enough to understand that you could write it by hand into a text file,
-- but it was not designed to be hand-written by programmers. Dao Lisp is more of a protocol used to
-- store production rules into databases. Express these production rules using functions like
-- 'daoRule', and then use 'Prelude.print' or 'Prelude.show' to generate the Dao Lisp code to be
-- stored to a file or database.
module Language.Interpreter.Dao.Kernel
  ( -- * The Dao Lisp Interpreter
    DaoEval, Environment(..), DaoLispBuiltin, environment, bif, bifList, monotypeArgList,
    newEnvironment, setupBuiltins, setupTraceBIF, setupTraceAtom, setupTraceForm, DaoFunction(..),
    daoFail, daoCatch, daoVoid, evalDaoExprIO, evalDaoIO, evalDaoExprWith, evalPartial,
    evalAtomWith, evalFunction, filterEvalForms, evalProcedure, evalPipeline, evalDaoRule,
    -- * Production Rule Database Entry
    DBEntry(..),
    -- * Dao Lisp's Fundamental Data Type
    DaoExpr(..), primitiveType, typeToAtom, basicType, DaoExprType(..), concatDaoExprs, plainFnCall,
    daoFnCall, strInterpolate, filterVoids, parseDaoUnit,
    -- * Converting Haskell to Dao Lisp Data Types
    DaoEncode(..), Inlining(..), Inliner(..), runInliner, inline1, inline1Dao, inlining,
    inlineToForm, inlineToFormWith,
    -- * Decoding 'DaoExpr's to Haskell Types
    Outliner, SubOutliner, runOutliner, runOutlining, outlineDaoDecoder, outlineAnyForm, subOutline,
    subOutlineWith, outlineExprType, outlineExprTypeOf, outlineExprEq, outlineExpr, outlineExprWith,
    -- * Pattern Matching Functions
    DaoDecode(..), Outlining(..), PatternMatcher, PatternMatcherState, MatchResult(..),
    runPatternMatch, resumeMatching, dumpArgs, returnIfEnd, setMatchType, matchStep, subMatch,
    maybeMatch, matchError, matchFail, matchQuit,
    -- * Primitive Dao Lisp Data Types
    DaoSize(..),
    Atom, parseDaoAtom, plainAtom,
    Dict(..), daoDict, plainDict, dictNull, unionDict, unionDictWith, emptyDict, lookupDict,
    dictAssocs,
    List, unwrapList, reverseUnwrapList, daoList, daoArray, plainList, maybeList, daoReverseList,
    showMaybeList, readMaybeList, listToForm, listToArray,
    Form(..), daoForm, daoForm1, plainForm, formSize, unwrapForm, inspectFormElems,
    parseTopLevelForm,
    Rule, Likelihood, Depends(..), Provides(..), daoRule,
    Error, ErrorClass, getErrorClass, getErrorInfo, plainError, daoError, errorAppendInfo,
    -- * Pattern Matching
    RulePatternChoice, matchRulePattern, RulePatternType, RulePattern, RulePatternNT,
    RulePatternUnit, patConst, patNamed, patTyped, patNamedTyped, pattern, choice, pattern1,
    RuleMatcher(..), runRuleMatcher, ruleMatchLift, noMatch, matchExprPatUnit,
    matchGenPatternUnit, matchGenPatternUnit', matchTypedPattern, matchNamedPattern,
    matchGenPatternSequence, matchGenPatternChoice, 
    -- * Pattern Matching Data Types
    ExprPatUnit(..), GenPatternUnit(..), TypedPattern(..), KleeneType(..), NamedPattern(..),
    GenPatternSequence(..), GenPatternChoice(..),
  )
  where

import           Control.Arrow
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Array.IArray
import           Data.Char
import           Data.Either           (partitionEithers)
import           Data.Foldable
import           Data.Functor.Identity
import           Data.List.NonEmpty    (NonEmpty(..), unfold)
import qualified Data.Map               as Map
import           Data.Semigroup
import           Data.String
import qualified Data.Text              as Strict
import qualified Data.Text.Lazy         as Lazy
import qualified Data.Text.Lazy.Builder as Build
--import           Data.TokenParser
import           Data.Typeable

import           Numeric

--import Debug.Trace
--import Unsafe.Coerce

----------------------------------------------------------------------------------------------------

_sp :: String -> String
_sp = dropWhile isSpace

_parmap :: Read a => (a -> b) -> (Int -> ReadS a) -> Int -> ReadS b
_parmap f reads p = fmap (f *** id) . reads p

_parseBracketsWith
  :: Char -> Char
  -> (Int -> ReadS a)
  -> Int -> ReadS a
_parseBracketsWith open clos readsPrec p str = case str of
  c:str | c == open -> do
    (a, str) <- readsPrec p $ _sp str
    case _sp str of
      c:str | c == clos -> [(a, _sp str)]
      _                 -> throw $ _daoError "parsing"
        [ ("reason", DaoString $ Strict.pack $ "expecting close " ++ open : clos : " bracket")
        , ("on-input", DaoString $ Strict.pack $ take 64 str)
        ]
  _                 -> []

_parseBrackets :: Read a => Char -> Char -> Int -> ReadS a
_parseBrackets open clos = _parseBracketsWith open clos readsPrec

_parseManyWith
  :: b -> (Int -> [a] -> b)
  -> (Int -> ReadS a)
  -> Int -> ReadS b
_parseManyWith empty constr readsPrec p str = loop (negate 1) [] str where
  loop i stack str = seq i $! case readsPrec p str of
    [(a, str)] -> loop (i + 1) (a : stack) (_sp str)
    _          -> [if null stack then (empty, str) else (constr i stack, str)]

_parseMany :: Read a => b -> (Int -> [a] -> b) -> Int -> ReadS b
_parseMany empty constr = _parseManyWith empty constr readsPrec

_parseSepWith :: Char -> b -> (Int -> [a] -> b) -> (Int -> ReadS a) -> Int -> ReadS b
_parseSepWith sep empty constr readsPrec p = loop (negate 1) [] where
  loop i stack str = seq i $!
    let end a str = [if null stack then (empty, str) else (constr i (a : stack), str)] in
    case readsPrec p str of
      []          -> []
      [(a, str)]  -> case str of
        c:str | c == sep -> loop (i + 1) (a : stack) (_sp str)
        str              -> end a (_sp str)
      (a, str):_  -> end a str

_parseSep :: Read a => Char -> b -> (Int -> [a] -> b) -> Int -> ReadS b
_parseSep sep empty constr = _parseSepWith sep empty constr readsPrec

_ambig :: Show a => [(a, String)] -> String
_ambig = ("ambiguous parse:\n" ++) <<<
  ( zip [1::Int ..] >=> \ (i, (a, rem)) ->
    "    " ++ show i ++ ": " ++ show a ++ "(remainder = " ++
    (let (a,b) = splitAt 10 rem in show a ++ if null b then "" else "...") ++ ")"
  )

_showNonEmpty :: Show a => NonEmpty a -> ShowS
_showNonEmpty = showList . toList

_daoShowSp :: ShowS -> ShowS
_daoShowSp f = f . \ case { "" -> ""; more -> ' ' : more; }

_daoShowBrackets :: Char -> Char -> ShowS -> ShowS
_daoShowBrackets open close shows = (open :) . ((shows "") ++) . (close :)

----------------------------------------------------------------------------------------------------

-- | This is the fundamental expression data type for Dao Lisp. The constructors are exposed here
-- in order to allow you to use pattern matching, and for disambiguating what type you wish to
-- construct when the 'dao' function is causing your problems during type checking.
data DaoExpr
  = DaoVoid  -- ^ These values are ignored.
  | DaoNull  -- ^ This is the 'False' value.
  | DaoTrue  -- ^ This is the 'True' value.
  | DaoComma -- ^ This is a special 'Atom' that is the single "comma" character @','@.
  | DaoColon -- ^ This is a special 'Atom' that is the single "colon" character @':'@.
  | DaoSemi  -- ^ This is a special 'Atom' that is the single "semicolon" character @';'@.
  | DaoAtom   !Atom
  | DaoChar   !Char
  | DaoInt    !Int
  | DaoFloat  !Double
  | DaoString !Strict.Text
  | DaoDict   !Dict
  | DaoList   !(List DaoExpr)
  | DaoForm   !Form
  | DaoRule   !Rule
  | DaoError  !Error
  deriving (Eq, Ord)

-- | Each of the above primitive type constructors can be mapped to one of these primitive types.
-- You can use 'primitiveType' to extract one of these values from a 'DaoExpr'. You can use
-- 'basicType' to construct one of these types from an 'Atom'. You can use 'typeToAtom' to convert
-- one of these types to an 'Atom'.
data DaoExprType
  = DaoVoidType
  | DaoNullType
  | DaoTrueType
  | DaoAtomType
  | DaoCharType
  | DaoIntType
  | DaoFloatType
  | DaoStringType
  | DaoDictType
  | DaoListType
  | DaoFormType
  | DaoRuleType
  | DaoErrorType
  deriving (Eq, Ord)

instance Show DaoExprType where { show = show . typeToAtom; } 

instance Show DaoExpr where
  showList = \ case
    []                       -> id
    [a]                      -> showsPrec 0 a
    DaoColon : DaoComma : ax -> showsPrec 0 DaoColon . (' ' :) .
                                showsPrec 0 DaoComma . (' ' :) . showList ax
    DaoColon : a        : ax -> showsPrec 0 DaoColon . showList (a:ax)
    a        : DaoComma : ax -> showsPrec 0 a . showsPrec 0 DaoComma . (' ' :) . showList ax
    a                   : ax -> showsPrec 0 a . (' ' :) . showList ax
  showsPrec p = \ case
    DaoVoid     -> (++) "()"
    DaoNull     -> (++) "[]"
    DaoTrue     -> (++) "true"
    DaoComma    -> (',' :)
    DaoColon    -> (':' :)
    DaoSemi     -> (';' :)
    DaoChar   a -> showsPrec p a
    DaoAtom   a -> showsPrec p a
    DaoFloat  a -> showsPrec p a . ('f' :)
    DaoInt    a -> showsPrec p a
    DaoString a -> showsPrec p a
    DaoList   a -> showsPrec p a
    DaoForm   a -> showsPrec p a
    DaoDict   a -> showsPrec p a
    DaoError  a -> showsPrec p a
    DaoRule   a -> showsPrec p a

instance Read DaoExpr where
  readsPrec p = fmap (fmap (take 1))
    ( _parmap DaoRule _parseRule <>
      _parmap DaoError _parseError <>
      _parse1Expr
    ) p . _sp

instance Semigroup DaoExpr where { a <> b = concatDaoExprs [a, b]; }
instance Monoid    DaoExpr where { mempty = DaoVoid; mappend = (<>); }

instance DaoDecode DaoExprType where
  daoDecode = \ case
    DaoAtom t -> case basicType t of
      Nothing -> matchErrorL "could not decode DaoExprType" [("offender", DaoAtom t)]
      Just  t -> return t
    expr -> matchErrorL "could not decode DaoExprType from non-Atom value" [("offender", expr)]

_parse1Expr :: Int -> ReadS DaoExpr
_parse1Expr p str = case
  ( _parseList <>
    (\ _ -> \ case { '(':')':str -> [(DaoVoid, _sp str)]; _ -> []; }) <>
    ( _parseBracketsWith '(' ')' $ fmap (fmap (take 1)) $
        _parmap DaoRule  _parseRule <>
        _parmap DaoError _parseError <>
        _parmap DaoForm  _parseForm
    ) <>
    _parmap DaoDict _parseDict <>
    parseDaoUnit
  ) p str of { [] -> []; a:_ -> [a]; }

-- | Return a string indicating a unique primitive type for a 'DaoExpr', without evaluating
-- 'Form's.
primitiveType :: DaoExpr -> DaoExprType
primitiveType = \ case
  DaoVoid      -> DaoVoidType
  DaoNull      -> DaoNullType
  DaoTrue      -> DaoTrueType
  DaoComma     -> DaoAtomType
  DaoColon     -> DaoAtomType
  DaoSemi      -> DaoAtomType
  DaoChar   {} -> DaoCharType
  DaoAtom   {} -> DaoAtomType
  DaoInt    {} -> DaoIntType
  DaoFloat  {} -> DaoFloatType
  DaoString {} -> DaoStringType
  DaoDict   {} -> DaoDictType
  DaoList   {} -> DaoListType
  DaoForm   {} -> DaoFormType
  DaoRule   {} -> DaoRuleType
  DaoError  {} -> DaoErrorType

-- | Convert a 'DaoExprType' to an 'Atom'. Note that type atoms are all capitalized. All possible
-- values returned by this function are: Null, True, Atom, Integer, Float, String, Dictionary, List,
-- Form, Rule, and Error.
typeToAtom :: DaoExprType -> Atom
typeToAtom = Atom . \ case
  DaoVoidType   -> "Void"
  DaoNullType   -> "Null"
  DaoTrueType   -> "True"
  DaoAtomType   -> "Atom"
  DaoIntType    -> "Int"
  DaoCharType   -> "Char"
  DaoFloatType  -> "Flo"
  DaoStringType -> "Str"
  DaoDictType   -> "Dict"
  DaoListType   -> "List"
  DaoFormType   -> "Form"
  DaoRuleType   -> "Rule"
  DaoErrorType  -> "Error"

_indentType :: DaoExprType -> ShowS
_indentType = (++) . \ case
  DaoVoidType   -> " Void"
  DaoNullType   -> " Null"
  DaoTrueType   -> " True"
  DaoAtomType   -> " Atom"
  DaoIntType    -> "  Int"
  DaoCharType   -> " Char"
  DaoFloatType  -> "  Flo"
  DaoStringType -> "  Str"
  DaoDictType   -> " Dict"
  DaoListType   -> " List"
  DaoFormType   -> " Form"
  DaoRuleType   -> " Rule"
  DaoErrorType  -> "Error"

-- | Construct a primitive type from an 'Atom'. The primitive types that can be constructed must
-- match the 'Atom' values returned by the 'typeToAtom' function, with the following exceptions:
--
-- 1. 'DaoIntType' may be constructed with the Atom "Integer" or "Int"
-- 2. 'DaoNullType' may be constructed with the Atom "Null" or "False"
-- 3. 'DaoDictType' may be constructed with the Atom "Dictionary" or "Dict"
-- 4. 'DaoCharType' may be constructed with the Atom "Character" or "Char"
-- 5. 'DaoFloatType' may be constructed with the Atom "Float" or "Flo"
-- 6. 'DaoStringType' may be constructed with the Atom "String" or "Str"
basicType :: Atom -> Maybe DaoExprType
basicType (Atom a) = case a of
  "Void"       -> Just DaoVoidType
  "Null"       -> Just DaoNullType
  "False"      -> Just DaoNullType
  "Char"       -> Just DaoCharType
  "Character"  -> Just DaoCharType
  "Atom"       -> Just DaoAtomType
  "Int"        -> Just DaoIntType
  "Integer"    -> Just DaoIntType
  "Flo"        -> Just DaoFloatType
  "Float"      -> Just DaoFloatType
  "Str"        -> Just DaoStringType
  "String"     -> Just DaoStringType
  "Dict"       -> Just DaoDictType
  "Dictionary" -> Just DaoDictType
  "List"       -> Just DaoListType
  "Form"       -> Just DaoFormType
  "Rule"       -> Just DaoRuleType
  "Error"      -> Just DaoErrorType
  _            -> Nothing

_parseIntFloatLiteral :: Int -> ReadS DaoExpr
_parseIntFloatLiteral p str = case str of
  ""  -> []
  str -> case (_parmap DaoFloat readsPrec <> _parmap DaoInt readsPrec) p str of
    [(DaoFloat fn, fstr), (DaoInt n, str)] -> case fstr of -- NOT (_sp fstr)
      'f':fstr -> [(DaoFloat fn, _sp fstr)]
      _        -> [(DaoInt    n, _sp str )]
    [(DaoInt    n,  str)] -> case str of
      'f':str  -> [(DaoFloat $ realToFrac n, str)]
      c:str | not (isAlpha c) -> [(DaoInt n, c:str)]
      _        -> []
    a -> a

-- | Parses any 'DaoExpr' that is not constructed from brackets, so lists, dictionaries, and forms
-- are not parsed. Sometimes this can be more useful than the ordinary 'Prelude.readsPrec'
-- instantiation for the 'DaoExpr' data type.
parseDaoUnit :: Int -> ReadS DaoExpr
parseDaoUnit p = span atomChar >>> \ (buf, continue) -> do
  let atom = case buf of
        "true"  -> [(DaoTrue, _sp continue)]
        "false" -> [(DaoNull, _sp continue)]
        "null"  -> [(DaoNull, _sp continue)]
        _       -> [(DaoAtom $ Atom $ Strict.pack buf, _sp continue)]
  let nopoint constr num = case readsPrec p num of
        [(num, "")] -> [(constr num, _sp continue)]
        _           -> atom
  case buf of
    ""  -> case continue of
      ""              -> []
      ':'  : continue -> [(DaoColon, _sp continue)]
      ';'  : continue -> [(DaoSemi , _sp continue)]
      ','  : continue -> [(DaoComma, _sp continue)]
      '"'  : _        -> case readsPrec p continue of
        [(quot, continue)] -> [(DaoString $ Strict.pack quot, _sp continue)]
        _                  -> throw $ _daoError "parsing"
          [ ("reason", DaoString "failed to parse string literal")
          , ("on-input", DaoString $ Strict.pack$ take 16 continue)
          ]
      '\'' : _        -> case readsPrec p continue of
        [(char, continue)] -> [(DaoChar char, _sp continue)]
        _                  -> throw $ _daoError "parsing"
          [ ("reason", DaoString "failed to parse character literal")
          , ("on-input", DaoString $ Strict.pack $ take 8 continue)
          ]
      _               -> []
        --throw $ _daoError "parsing" 
        --  [ ("reason", DaoString "unknown character")
        --  , ("offender", DaoChar c)
        --  , ("on-input", DaoString $ Strict.pack $ take 8 continue)
        --  ]
    buf -> do
      (neg, buf) <- case buf of
        '-':buf -> [(True, buf)]
        '+':buf -> [(False, buf)]
        buf     -> [(False, buf)]
      let negf = if neg then negate else id
      let negi = if neg then negate else id
      case buf of
        '.' : buf -> case readsPrec p $ '0':'.':buf of
          [(num, "")] -> [(DaoFloat $ negf num, _sp continue)]
          _           -> atom
        buf       -> case span isDigit buf of
          ("", _)                                 -> atom
          ('0':x:num, str) | x == 'x' || x == 'X' -> case readHex $ num ++ str of
            [(num, "")]                             -> [(DaoInt num, _sp continue)]
            _                                       -> atom
          (num, "")                               -> nopoint (DaoInt . negi) num
          (num, "f")                              -> nopoint (DaoFloat . realToFrac . negi) num
          (_  , e:_) | e `elem` (".eE" :: String) -> case readsPrec p $ buf ++ continue of
            [(num, continue)]                       ->
              let d = DaoFloat $ negf num in case continue of
                ""                                      -> [(d, "")]
                'f':continue                            -> [(d, _sp continue)]
                c:continue | not (atomChar c)           -> [(d, _sp $ c:continue)]
                _                                       -> atom
            _                                       -> atom
          _                                       -> atom

concatDaoExprs :: [DaoExpr] -> DaoExpr
concatDaoExprs = loop (negate 1) [] where
  unlist stack = \ case { [] -> stack; a:ax -> unlist (a:stack) ax; }
  loop len stack ax = seq len $! case ax of
    []                          -> if len < 0 then DaoVoid else DaoList $ _list len stack
    DaoVoid                : ax -> loop len stack ax
    DaoList (v@(List arr)) : ax -> loop (len + length v) (unlist stack $ elems arr) ax
  --DaoForm f              : ax -> loop (len + formSize f) (_revUnForm f ++ stack) ax
    a                      : ax -> loop (succ len) (a : stack) ax

-- | Construct a function call 'Form' data structure that can be evaluated by 'evalDaoIO', given an
-- 'Atom' as the name of the function to call, and the list of arguments to be passed to the
-- function.
plainFnCall :: Atom -> [DaoExpr] -> Form
plainFnCall nm = plainForm . (:|) (DaoAtom nm)

-- | Like 'plainFnCall' but conveniently wraps the 'Form' into a 'DaoForm' constructor to return a
-- value of type 'DaoExpr'.
daoFnCall :: Atom -> [DaoExpr] -> DaoExpr
daoFnCall nm = DaoForm . plainFnCall nm

-- | This is a simple, and pure, string interpolation function in which 'DaoString's are unpacked
-- and without being "sanitized" and surrounded in quotation marks, and non-strings are converted to
-- strings using the ordinary Haskell 'Prelude.show' function.
--
-- Note that this function can also be used with 'filterEvalForms', with this equation:
--
-- @
-- 'Data.Functor.fmap' 'strInterpolate' . 'filterEvalForms'
-- @
--
-- which will perform an evaluation of all 'Form's in the 'DaoExpr' list before evaluating
-- 'strInterpolate' on all returned strings.
strInterpolate :: [DaoExpr] -> Strict.Text
strInterpolate = Lazy.toStrict . Build.toLazyText . loop where
  loop = \ case
    []                 -> mempty
    DaoString txt : ax -> Build.fromText txt <> loop ax
    ax                 -> shloop id ax
  join elems = Build.fromString $ showList (elems []) ""
  shloop elems = \ case
    []                   -> join elems
    ax@(DaoString{} : _) -> join elems <> loop ax
    a               : ax -> shloop (elems . (a :)) ax

-- | This function filters out all 'DaoVoid' expressions from the given list.
filterVoids :: [DaoExpr] -> [DaoExpr]
filterVoids = (>>= (\ case { DaoVoid -> []; a -> [a]; }))

----------------------------------------------------------------------------------------------------

-- | A 'DaoDecode' classifies functions used to produce a Haskell values of type @a@ from a single
-- primitive 'DaoExpr' value.
class DaoDecode a where
  daoDecode :: DaoExpr -> Either Error a

-- | This is a class of Haskell types @a@ which can be constructed from a list of 'DaoExpr's.
class Outlining a where
  outline :: Outliner a

instance DaoDecode DaoExpr       where { daoDecode = return; }
instance DaoDecode ()            where
  daoDecode = \ case
    DaoVoid -> return ()
    expr    -> _outlineTypeOfErrL expr DaoVoid
instance DaoDecode Int           where
  daoDecode = \ case
    DaoInt a -> return a
    expr     -> _outlineTypeOfErrL expr (DaoInt 0)
instance DaoDecode Double        where
  daoDecode = \ case
    DaoFloat a -> return a
    expr       -> _outlineTypeOfErrL expr (DaoFloat 0.0)
instance DaoDecode Float         where
  daoDecode = \ case
    DaoFloat a -> return $ realToFrac a
    expr       -> _outlineTypeOfErrL expr (DaoFloat 0.0)
instance DaoDecode String        where
  daoDecode = \ case
    DaoString a -> return $ Strict.unpack a
    expr        -> _outlineTypeOfErrL expr (DaoString "")
instance DaoDecode Strict.Text   where
  daoDecode = \ case
    DaoString a -> return a
    expr        -> _outlineTypeOfErrL expr (DaoString "")
instance DaoDecode Build.Builder where
  daoDecode = \ case
    DaoString a -> return $ Build.fromText a
    expr        -> _outlineTypeOfErrL expr (DaoString "")
instance DaoDecode (Map.Map Atom DaoExpr) where
  daoDecode = \ case
    DaoDict (Dict a) -> return a
    expr             -> _outlineTypeOfErrL expr (DaoDict $ Dict $ Map.empty)
instance DaoDecode (Array Int DaoExpr) where
  daoDecode = \ case
    DaoList (List a) -> return a
    expr            -> _outlineTypeOfErrL expr (DaoList $ _list 0 $ [DaoInt 0])

----------------------------------------------------------------------------------------------------

-- | The class of data types which have a "size" value. This includes 'List's, 'Form's,
-- 'Strict.Text', and 'Atom's.
class DaoSize a where { daoSize :: a -> Int; }

instance DaoSize (List DaoExpr) where { daoSize (List arr) = 1 + uncurry subtract (bounds arr); }
instance DaoSize Dict where { daoSize (Dict map) = Map.size map; }
instance DaoSize Strict.Text where { daoSize = Strict.length; }
instance DaoSize Atom where { daoSize (Atom str) = Strict.length str; }
instance DaoSize Form where { daoSize (Form list) = daoSize list; }
instance DaoSize DaoExpr where
  daoSize = \ case
    DaoTrue     -> 1
    DaoAtom   a -> daoSize a
    DaoList   a -> daoSize a
    DaoDict   a -> daoSize a
    DaoForm   a -> daoSize a
    DaoString a -> daoSize a
    _           -> 0

----------------------------------------------------------------------------------------------------

-- | This is a Dao Atom data type, for unquoted string expressions in Dao Lisp 'Form's. This data
-- type instantiates 'Data.String.IsString' so you can express Dao 'Atom's in Haskell code as string
-- literals (if the @OverloadedStrings@ compiler option is enabled). But the string used to
-- construct 'Atom' must follow the same parsing rules, so the atom may not contain spaces, integer
-- literals, bracketed expressions, quotation marks, or the reserved words @rule@, @error@,
-- @true@, @false@, or @null.
newtype Atom = Atom Strict.Text deriving (Eq, Ord, Typeable)

instance IsString Atom where { fromString str = _plainAtom (Strict.pack str) str; }

instance Show Atom where { show (Atom a) = Strict.unpack a; }
instance Read Atom where
  readsPrec p str = do
    (a, str) <- parseDaoAtom p str
    case a of
      Left err -> throw err
      Right  a -> [(a, _sp str)]

instance Inlining  Atom where { inline = (<>) . pure . dao; }
instance Outlining Atom where { outline = outlineDaoDecoder; }

instance DaoEncode Atom where
  dao (Atom a) = case a of
    "," -> DaoComma
    ":" -> DaoColon
    ";" -> DaoSemi
    a   -> DaoAtom (Atom a)

instance DaoDecode Atom where
  daoDecode = \ case
    DaoAtom a -> return a
    expr      -> _outlineTypeOfErrL expr (DaoAtom $ Atom "atom")

-- | Construct a plain 'Atom' from a string. The string must be a valid 'Atom' or a Haskell runtime
-- exception is thrown. This function is used to instantiate the 'Data.String.fromString' method of
-- the 'Data.String.IsString' type class.
plainAtom :: Strict.Text -> Atom
plainAtom = id &&& Strict.unpack >>> uncurry _plainAtom 

_plainAtom :: Strict.Text -> String -> Atom
_plainAtom txt = let onInput = ("on-input", DaoString txt) in parseDaoAtom 0 >>> \ case
  [(_ , rem)] | not (null rem) -> throw $ _daoError "parsing"
    [ ("reason", DaoString "converted from a Haskell string that expresses more than one atom")
    , onInput
    ]
  [(atom, "")] -> case atom of
    Left  err     -> throw err
    Right atom    -> atom
  [] -> throw $ _daoError "parsing"
    [ ("reason", DaoString "converted from Haskell string that expresses no atoms")
    , onInput
    ]
  parses -> case partitionEithers $ fst <$> parses of
    ([], possible) -> throw $ _daoError "parsing"
      [ ("reason", DaoString "ambiguous parse")
      , ("possible-values", DaoList $ _list (length possible - 1) (DaoAtom <$> possible))
      , onInput
      ]
    (err : _ , _) -> throw err

atomChar :: Char -> Bool
atomChar c = isAlphaNum c || not (c `elem` ("'\",:;(){}[] " :: String) || isControl c)

parseDaoAtom :: Int -> ReadS (Either Error Atom)
parseDaoAtom p = parseDaoUnit p >=> \ (unit, str) -> case unit of
  DaoComma      -> [(Right (Atom $ Strict.singleton ','), _sp str)]
  DaoColon      -> [(Right (Atom $ Strict.singleton ':'), _sp str)]
  DaoSemi       -> [(Right (Atom $ Strict.singleton ';'), _sp str)]
  DaoAtom  atom -> [(Right atom, _sp str)]
  _            -> return $ flip (,) (_sp str) $ Left $ _daoError "parsing"
    [ ("reason", DaoString "expected a literal expression of an atomic value, instead parsed")
    , ("offender", unit)
    , ("offender-type", DaoAtom $ typeToAtom $ primitiveType unit)
    ]

_resWord :: Atom -> ReadS (Either Error Atom)
_resWord atom@(Atom word) str = do
  let fail = return $ flip (,) str $ Left $ _daoError "cannot-parse-atom"
        [ ("reason", DaoString "could not parse reserved word")
        , ("offender", DaoString word)
        , ("on-input", DaoString $ Strict.pack $ take 16 str)
        ]
  case word of
    "null"  -> fail
    "true"  -> fail
    "false" -> fail
    "error" -> fail
    "rule"  -> fail
    _       -> [(Right atom, str)]

----------------------------------------------------------------------------------------------------

-- | This is the Dictionary data type, which maps 'Atom's to arbitrary 'DaoExpr's. The syntax for a
-- dictionary contains zero or more relations in curly brackets, where a relation is expressed as:
--
-- > ':'  'Atom'   'DaoExpr'
-- 
-- So for example:
--
-- > { :three 3   :two 2   :one 1 }
-- 
-- Keep in mind that the colon @':'@ character is a special 'Atom' which always stands alone.
-- So the 'Dict' @{::::}@ will actually parse to a value equivalent to the value constructed by
--
-- @
-- 'plainDict' [(":", 'DaoAtom' ":"), (":", 'DaoAtom' ":")]
-- @
--
-- Some other examples of valid dictionary expressions:
--
-- > { ::    (the key atom will be ":")
-- >   :;    (the key atom will be ";")
-- >   :,    (the key atom will be ",")
-- >   :-->  (the key atom will be "-->")
-- > }
-- 
-- The comma @','@ and semicolon @';'@ are also special 'Atom's which always stand alone. So the
-- following expressions will fail to parse:
--
-- > { :,,
-- >   :,,,
-- > }
--
-- Above, the first line will parse to @'plainDict' [("," , 'daoAtom' ",")]@ but the second line
-- will fail to parse because there are two commas after the key atom ":,". However this would be
-- OK:
--
-- > { :,,
-- >   :,(,,)
-- > }
--
-- Which parses to:
--
-- @
-- 'plainDict' [ ("," , 'daoForm' ['daoAtom' "," , 'daoAtom' ","]) ]
-- @
--
newtype Dict = Dict { dictToMap :: Map.Map Atom DaoExpr }
  deriving (Eq, Ord, Typeable)

instance Show Dict where
  showsPrec _ (Dict map) = _daoShowBrackets '{' '}' $ showList $ do
    (atom, expr) <- Map.assocs map
    [DaoColon, DaoAtom atom, expr]

instance Read Dict where { readsPrec = _parseDict; }

instance Inlining Dict where { inline = (<>) . pure . dao; }
instance Outlining Dict where { outline = outlineDaoDecoder; }
instance DaoEncode Dict where { dao = DaoDict; }
instance DaoDecode Dict where
  daoDecode = \ case
    DaoDict a -> return a
    expr      -> _outlineTypeOfErrL expr (DaoDict $ Dict $ Map.empty)

_parseDict :: Int -> ReadS Dict
_parseDict p = \ case
  '{':str -> do
    let loop stack str = case _sp str of
          '}' : str -> [(Dict $ Map.fromList stack, _sp str)]
          ':' : str -> do
            (atom, str) <- parseDaoAtom p (_sp str)
            case atom of
              Left   err -> throw err
              Right atom -> do
                (p, str) <- _parse1Expr p (_sp str)
                loop ((atom, p) : stack) (_sp str)
          _         -> []
    loop [] (_sp str)
  _ -> []

-- | Check if a 'Dict' dictionary is empty.
dictNull :: Dict -> Bool
dictNull (Dict d) = Map.null d

-- | Construct a 'Dict' dictionary not wrapped in a 'DaoExpr' .
plainDict :: [(Atom, DaoExpr)] -> Dict
plainDict = Dict . Map.fromList

-- | Construct a 'Dict' dictionary as a 'DaoExpr' .
daoDict :: [(Atom, DaoExpr)] -> DaoExpr
daoDict = DaoDict . plainDict

-- | Perform a 'Data.Map.unionWith' operation on two 'Dict'ionary values.
unionDictWith :: (DaoExpr -> DaoExpr -> DaoExpr) -> Dict -> Dict -> Dict
unionDictWith f (Dict a) (Dict b) = Dict $ Map.unionWith f a b

-- | Calls 'unionDictWith' with the 'concatDaoExprs' function
unionDict :: Dict -> Dict -> Dict
unionDict = unionDictWith (\ a b -> concatDaoExprs [a, b])

-- | A 'Dict' that contains no elements.
emptyDict :: Dict
emptyDict = Dict Map.empty

-- | Lookup an element in a 'Dict'.
lookupDict :: Atom -> Dict -> Maybe DaoExpr
lookupDict atom = Map.lookup atom . dictToMap

-- | Insert an ielement into a 'Dict', given a function used to combine elements if the 'Atom' key
-- already exists.
insertDict :: (DaoExpr -> DaoExpr -> DaoExpr) -> Atom -> DaoExpr -> Dict -> Dict
insertDict f key val (Dict map) = Dict $ Map.insertWith f key val map

-- | Return the list of 'Atom' 'DaoExpr' pairs which define this 'Dict'.
dictAssocs :: Dict -> [(Atom, DaoExpr)]
dictAssocs (Dict map) = Map.assocs map

----------------------------------------------------------------------------------------------------

-- | This is the internal list type used by Dao Lisp. It is actually a @newtype@ wrapper around a
-- Haskell 'Data.Array.IArray.Array' data type from from the "Data.Array.IArray" module. Therefore
-- this data type may not be empty. When constructing a 'DaoExpr' from a list of elements, if the
-- list is null then the 'DaoExpr' constructed is 'DaoNull'. Constructing a plain 'List' from a null
-- Haskell list will throw a Haskell runtime exception.
newtype List a = List (Array Int a)
  deriving (Eq, Ord, Typeable)

instance Functor List where { fmap f (List arr) = List $ fmap f arr; }

instance Show a => Show (List a) where
  showsPrec _ = _daoShowBrackets '[' ']' . _showNonEmpty . unwrapList

instance Read a => Read (List a) where
  readsPrec = _parseBracketsWith '[' ']' $ _parseMany (error "empty list") (\ i -> _list i)

instance Traversable List where
  --traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (List arr) = List <$> traverse f arr
  --sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA (List arr) = List <$> sequenceA arr
  --mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  mapM f (List arr) = liftM List $ mapM f arr
  --sequence :: Monad m => t (m a) -> m (t a)
  sequence (List arr) = liftM List $ sequence arr

instance Foldable List where
  -- fold :: Monoid m => t m -> m
  fold (List arr) = fold arr
  -- foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f (List arr) = foldMap f arr
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f b (List arr) = foldr f b arr
  -- foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f b (List arr) = foldl f b arr
  -- toList :: t a -> [a]
  toList (List arr) = toList arr
  -- null :: t a -> Bool
  null (List arr) = null arr
  -- length :: t a -> Int
  length (List arr) = length arr
  -- elem :: Eq a => a -> t a -> Bool
  elem a (List arr) = elem a arr
  -- maximum :: Ord a => t a -> a
  maximum (List arr) = maximum arr
  -- minimum :: Ord a => t a -> a
  minimum (List arr) = minimum arr
  -- sum :: Num a => t a -> a
  sum (List arr) = sum arr
  -- product :: Num a => t a -> a
  product (List arr) = product arr

instance Inlining (List DaoExpr) where { inline = (<>) . pure . dao; }
instance Outlining (List DaoExpr) where { outline = outlineDaoDecoder; }
instance DaoEncode (List DaoExpr) where { dao = DaoList; }
instance DaoDecode (List DaoExpr) where
  daoDecode = \ case
    DaoList a -> return a
    expr      -> _outlineTypeOfErrL expr (DaoList $ _list 0 $ [DaoInt 0])

_parseList :: Int -> ReadS DaoExpr
_parseList = _parseBracketsWith '[' ']' $
  _parseManyWith DaoNull (\ i -> DaoList . _list i) _parse1Expr

-- | Like 'Prelude.show' but allows for an empty list.
showMaybeList :: Show a => Maybe (List a) -> ShowS
showMaybeList = maybe (('[' :) . (']' :)) (showsPrec 0)

-- | Like 'Prelude.readsPrec' but may parse an empty list.
readMaybeList :: Read a => Int -> ReadS (Maybe (List a))
readMaybeList = _parseBracketsWith '[' ']' $ _parseMany Nothing (\ i -> Just . _list i)

-- Construct a listtor with a known upper bound value. Elements in the given list are reversed.
_list :: Int -> [a] -> List a
_list i = List . array (0, i) . zip (takeWhile (>= 0) $ subtract 1 `iterate` i)

_listSnoc :: List a -> a -> List a
_listSnoc list a = let (b :| elems) = reverseUnwrapList list in
  _list (length list) $ a : b : elems

-- | Convert a 'List' of 'DaoExpr's to a 'Form', in O(1) time.
listToForm :: List DaoExpr -> Form
listToForm = Form

-- | Convert a 'List' to a Haskell 'Data.Array.IArray.Array'.
listToArray :: List a -> Array Int a
listToArray (List arr) = arr

-- | Construct a 'List' from a 'Data.List.NonEmpty.NonEmpty' Haskell list.
plainList :: NonEmpty a -> List a
plainList (b :| bx) = List $ uncurry array $ loop [] 0 b bx where
  loop stack i a bx = seq i $! case bx of
    []   -> ((0, i), (i, a) : stack)
    b:bx -> loop ((i, a) : stack) (i + 1) b bx

-- | Construct a 'List' from a Haskell list, returning 'Prelude.Nothing' if the list is empty.
maybeList :: [a] -> Maybe (List a)
maybeList = \ case { [] -> Nothing; a:ax -> Just $ plainList $ a :| ax; }

-- | Construct a Haskell 'Data.Array.IArray.Array' of 'DaoExpr's.
daoArray :: [DaoExpr] -> Maybe (Array Int DaoExpr)
daoArray = filter (\ case { DaoVoid -> False; _ -> True; }) >>> \ case
  []    -> Nothing
  exprs -> Just $ uncurry array $
    ((,) 0 . foldl (\ _ i -> i) 0 . fmap fst &&& id) $ zip [0::Int ..] exprs

-- | Retrieve the contents of a 'List' in reverse as a 'Data.List.NonEmpty.NonEmpty' list.
reverseUnwrapList :: List a -> NonEmpty a
reverseUnwrapList (List arr) = let (lo, hi) = bounds arr in
  (arr !) <$> unfold (\ i -> (i, if i > lo then Just $ i - 1 else Nothing)) hi

-- | Retrieve the contents of a 'List' as a 'Data.List.NonEmpty.NonEmpty' list.
unwrapList :: List a -> NonEmpty a
unwrapList (List arr) = let (lo, hi) = bounds arr in
  (arr !) <$> (unfold (\ i -> (i, if i < hi then Just $ i + 1 else Nothing)) lo)

_indexList :: List a -> Int -> a
_indexList (List a) = (a !)

-- | Construct a 'DaoExpr' of primitive type 'DaoList'. If the given Haskell list is empty,
-- this function evaluates to 'DaoNull'. A 'DaoList' always created, even if there is only one
-- element, which is different from 'daoForm' which does not create forms unless there is only one
-- element. This function is total, so will never fail unless the list itself contains undefined
-- Haskell elements.
daoList :: [DaoExpr] -> DaoExpr
daoList = maybe DaoNull (DaoList . List) . daoArray

-- | Construct a 'DaoExpr' of primitive type 'DaoList' with the elements reversed. The
-- 'Data.List.reverse' function is not used, the given list elements are not re-ordered in any way,
-- so this function is very efficient, and especially good for freezing a list of elements that have
-- been built by stacking them with a recursive function.
daoReverseList :: [DaoExpr] -> DaoExpr
daoReverseList = maybe DaoNull (DaoList . List) . daoArray

----------------------------------------------------------------------------------------------------

-- | This is a data structure known to Lisp programmers as an "S-Expression". It is a sequence of
-- primitive 'DaoExpr's that can represent a more complex data structure. Forms are evaluated as
-- commands, and are different from lists in that they can be evaluated directly, whereas a list
-- must first be converted to a form before it can be evaluated.
newtype Form = Form { formToList :: List DaoExpr } deriving (Eq, Ord, Typeable)

instance Show Form where { showsPrec _ = _daoShowBrackets '(' ')' . _showNonEmpty . unwrapForm; }
instance Read Form where { readsPrec p = _parseForm p . _sp; }
instance Inlining  Form where { inline = (<>) . pure . DaoForm; }
instance DaoEncode Form where { dao = DaoForm; }
instance Outlining Form where { outline = outlineDaoDecoder ; }
instance DaoDecode Form where
  daoDecode = \ case
    DaoForm a -> return a
    expr      -> _outlineTypeOfErrL expr (daoForm [DaoNull])

_parseForm :: Int -> ReadS Form
_parseForm p = \ case
  ""  -> []
  str -> case _parseManyWith Nothing (\ i -> Just . _form i) _parse1Expr p str of
    [(Nothing  , _  )] -> []
    [(Just form, str)] -> [(form, _sp str)]
    _ -> throw $ _daoError "parsing" [("reason", DaoString "failed to parse Form") ]

-- | Pretty-prints each element of the given 'Form' on it's own line along with the index number and
-- type.
inspectFormElems :: Form -> ShowS
inspectFormElems form = (++) "Form with " . showsPrec 0 (formSize form) .
  (++) " elements:\n" . fold (unwrapForm form) where
    indent = (++) . \ case
      i | i <= 9 -> "    "
      i | i <= 99 -> "   "
      i | i <= 999 -> "  "
      i | i <= 9999 -> " "
      _              -> ""
    elem i a = indent i . showsPrec 0 (i :: Int) .
      _indentType (primitiveType a) . (' ' :) . showsPrec 0 a . ('\n' :)
    fold (a :| ax) = foldl (\ f (i, a) -> f . elem i a) (elem 0 a) (zip [1 ..] ax)

parseTopLevelForm :: Int -> ReadS DaoExpr
parseTopLevelForm = _parseSep ';' DaoVoid (\ i -> DaoForm . _form i)

-- | Construct a 'DaoForm' from a Haskell list of expressions. If the Haskell list is empty,
-- this function evaluates to 'DaoVoid' (not 'DaoNull', which is different from how 'daoList'
-- behaves). If the list contains only 1 element, the element is returned as it is, without wrapping
-- it in a form. To force the creation of a form with only one element, use 'daoForm1'.
daoForm :: [DaoExpr] -> DaoExpr
daoForm = \ case
  []   -> DaoVoid
  [a]  -> a
  a:ax -> DaoForm $ Form $ plainList $ a :| ax

-- | Similar to 'daoForm', but if the given list of 'DaoExpr's contains only one element, a
-- 'DaoForm' with exactly one element is created. Otherwise it behaves just like 'daoForm'.
daoForm1 :: [DaoExpr] -> DaoExpr
daoForm1 = \ case
  [a] -> DaoForm $ _form 0 [a]
  ax  -> daoForm ax

-- | Construct a 'Form' from a 'Data.List.NonEmpty.NonEmpty' Haskell list. This function is a
-- shorthand for the equation @('listToForm' . 'plainForm')@.
plainForm :: NonEmpty DaoExpr -> Form
plainForm = Form . plainList

-- | Return the number of elements in a 'Form'.
formSize :: Form -> Int
formSize (Form a) = length a

_revUnForm :: Form -> NonEmpty DaoExpr
_revUnForm (Form a) = reverseUnwrapList a

_form :: Int -> [DaoExpr] -> Form
_form i = Form . _list i

unwrapForm :: Form -> NonEmpty DaoExpr
unwrapForm (Form a) = unwrapList a

_indexForm :: Form -> Int -> DaoExpr
_indexForm (Form list) = _indexList list

----------------------------------------------------------------------------------------------------

-- | The likelihood that a 'Rule' will match some input. This is a simple optimization heuristic
-- based on how many times certain rules have successfully matched input queries, allowing the
-- 'Rule's in the database to be sorted such that 'Rule's is closer to the top and therefore will be
-- matched to a query sooner.
type Likelihood = Double

-- | The Production Rule data type. This is a primitive data type with it's own syntax. In Dao Lisp,
-- the syntax for a 'Rule' begins with the @rule@ keyword:
--
-- > rule [dep1 dep2 ...] [provd1 provd2 ...] (some pattern) (some action)
--
-- The pattern and action are not optional and the parser will fail if they do not exist. The
-- pattern and action are always specified in that order.
--
-- There are two optional square-bracketed lists of atoms, the first square list will be the rule
-- dependency tags, the second will be the rule provider tags, always in that order regardless of
-- where or whether or not they are specified. So the following rules are also valid:
--
-- > rule [dep1 dep2 ...] (some pattern) (some action)
-- > rule (some pattern) (some action)
-- > rule [] [provd1 provd2 ...] (some pattern) (some action)
-- > rule [dep1 dep2 ...] (some pattern) (some action) [provd1 provd2 ...]
-- > rule (some pattern) [dep1 dep2 ...] (some action) [provd1 provd2 ...]
-- > rule (some pattern) [dep1 dep2 ...] (some action)
-- > rule (some pattern) (some action) [dep1 dep2 ...]
--
-- However it is not possible to define a rule like this:
--
-- > rule (some pattern) (some action) [provd1 provd2 ...]  -- impossible
--
-- because the first square bracket seen will be used as the dependencies and not the providers.
data Rule
  = Rule
    { ruleDepends    :: Depends
    , ruleProvides   :: Provides
    , ruleLikelihood :: Likelihood
    , rulePattern    :: RulePatternChoice
    , ruleAction     :: Form
    }
  deriving (Eq, Ord, Typeable)

newtype Depends  = Depends  { dependsList  :: Maybe (List DaoExpr) } deriving (Eq, Ord, Typeable)
newtype Provides = Provides { providesList :: Maybe (List DaoExpr) } deriving (Eq, Ord, Typeable)

instance Show     Depends  where { showsPrec p = maybe id (showsPrec p) . dependsList; }
instance Show     Provides where { showsPrec p = maybe id (showsPrec p) . providesList; }
--instance IsString Depends  where { fromString = Depends  . _depProvs "depends" ; }
--instance IsString Provides where { fromString = Provides . _depProvs "provides"; }

instance Show Rule where
  showsPrec p
    ( Rule
      { ruleDepends=deps
      , ruleProvides=provs
      , ruleLikelihood=like
      , rulePattern=pat
      , ruleAction=act
      })
    = (++) "rule" .
      showsPrec p deps .
      showsPrec p provs . (' ' :) .
      showsPrec p like . (' ' :) .
      ('(' :) . showsPrec p pat . (')' :) . 
      showsPrec p act

instance Read Rule where { readsPrec = _parseRule; }

_parseRule :: Int -> ReadS Rule
_parseRule p = \ case
  'r':'u':'l':'e':x:str | not (isAlphaNum x) -> do
    let balanced _ = id -- makes the code below look nicer
    let failParse add msg = throw $ _daoError "parsing" $ add
          [ ("reason", DaoString msg)
          , ("primitive-type", DaoAtom "rule")
          ]
    let failExpect tag = failParse (("required", DaoAtom $ Atom tag) :) "missing field"
    let mkRule (like, deps, provs, pat, act) = Rule
          { ruleLikelihood = maybe 0.0 id like
          , ruleDepends    = maybe (Depends Nothing) id deps
          , ruleProvides   = maybe (Provides Nothing) id provs
          , rulePattern    = maybe
              (failExpect "pattern")
              (\ form -> 
                  let info = _errorAppendInfo "input" (DaoForm form) .
                             _errorAppendInfo "expecting" (DaoAtom $ Atom "rule-pattern")
                  in  case runOutlining $ toList $ unwrapForm form of
                        (MatchOK pat, []  ) -> pat
                        (MatchOK pat, more) -> throw $ _daoError "parsing"
                          [ ("reason", DaoString "ambiguous pattern")
                          , ("parsed-pattern", inlineToForm pat)
                          , ("whole-pattern", DaoForm form)
                          , ("remainder", daoForm1 more)
                          ]
                        (MatchFail err, _ ) -> throw $ info err
                        (MatchQuit err, _ ) -> throw $ info err
              ) pat
          , ruleAction     = maybe (failExpect "action") id act
          }
    let loop str parsed = case parsed of
          (Just{}, Just{}, Just{}, Just{}, Just{}) -> [(mkRule parsed, _sp str)]
          parsed -> case str of
            '[':_   -> balanced ']' $ case parsed of
              (like, Just deps, Nothing, pat, act) -> do
                (atoms, str) <- readMaybeList p str
                loop (_sp str) (like, Just deps, Just (Provides atoms), pat, act)
              (like, Nothing  , provs  , pat, act) -> do
                (atoms, str) <- readMaybeList p str
                loop (_sp str) (like, Just (Depends atoms), provs, pat, act)
              (_ , Just{}   , Just{} , _  , _  ) -> failParse id
                "rule depends and provides have already both been defined"
            '(':_ -> balanced ')' $ case parsed of
              (like, deps, provs, Just pat, Nothing) -> do
                (form, str) <- _parseBracketsWith '(' ')' _parseForm p str
                loop (_sp str) (like, deps, provs, Just pat, Just form)
              (like, deps, provs, Nothing , act    ) -> do
                (form, str) <- _parseBracketsWith '(' ')' _parseForm p str
                loop (_sp str) (like, deps, provs, Just form, act)
              (_ , _ , _ , Just{}, Just{}) -> failParse id
                "rule pattern and action have already both been defined"
            str   -> case readsPrec 0 str of
              [(like, str)] -> case parsed of
                (Just oldlike, _ , _ , _ , _ ) -> failParse
                  ((("first", DaoFloat oldlike) :) . (("second", DaoFloat like) :))
                  "rule likelihood specified more than once"
                (Nothing, deps, provs, pat, act) ->
                  loop (_sp str) (Just like, deps, provs, pat, act)
              _             -> (\ rule -> [(rule, _sp str)]) $! mkRule parsed 
    loop (_sp $ x:str) (Nothing, Nothing, Nothing, Nothing, Nothing)
  _ -> []

daoRule :: [DaoExpr] -> [DaoExpr] -> Likelihood -> RulePatternChoice -> Form -> Rule
daoRule deps provs like pat act = Rule
  { ruleDepends    = Depends  $ maybeList deps
  , ruleProvides   = Provides $ maybeList provs
  , ruleLikelihood = like
  , rulePattern    = pat
  , ruleAction     = act
  }

_depProvs :: Strict.Text -> String -> Maybe (List Atom)
_depProvs what txt =
  case _parseMany Nothing (\ i -> Just . _list i) 0 txt of
    [(a, str)] -> if null str then a else throw $ _daoError "constructing-atoms"
      [ ("reason", DaoString "extraneous literal input")
      , ("type", DaoAtom $ Atom what)
      , ("on-input", DaoString $ Strict.pack $ take 32 str)
      ]
    _  -> throw $ _daoError "constructing-atoms" $
      [ ("reason", DaoString "could not construct rule")
      , ("type", DaoAtom $ Atom what)
      ]

----------------------------------------------------------------------------------------------------

type ErrorClass = Atom

data Error = Error { getErrorClass :: !ErrorClass, getErrorInfo :: !Dict }
  deriving (Eq, Ord)

instance Exception Error where {}

instance Show Error where
  showsPrec p (Error clas dict) = _daoShowBrackets '(' ')' $ (++) "error " .
    showsPrec p clas . (if dictNull dict then id else (' ' :) . showsPrec p dict)

instance Read Error where { readsPrec = _parseError; }

instance DaoEncode Error where { dao = DaoError; }
instance DaoDecode Error where
  daoDecode = \ case
    DaoError err -> return err
    expr         -> _outlineTypeOfErrL expr DaoVoid

_parseError :: Int -> ReadS Error
_parseError p = \ case
  'e':'r':'r':'o':'r':x:str | not (isAlphaNum x) -> do
    (clas, str) <- readsPrec p $ _sp str
    (dict, str) <- readsPrec p $ _sp str
    [(Error clas dict, _sp str)]
  _ -> []

-- | Construct a plain 'Error', rather than an 'Error' wrapped up in a 'DaoExpr'.
plainError :: ErrorClass -> [(Atom, DaoExpr)] -> Error
plainError clas = Error clas . Dict . Map.fromList

-- | Construct an 'Error'.
daoError :: ErrorClass -> [(Atom, DaoExpr)] -> DaoExpr
daoError clas = DaoError . plainError clas

_daoError :: String -> [(String, DaoExpr)] -> Error
_daoError clas = Error (Atom $ Strict.pack clas) . Dict .
  Map.fromList . fmap (Atom . Strict.pack *** id)

-- | Update an 'Error' type by appending additional information to it's inner dictionary. If the
-- 'Atom' already exists in this 'Error' message, the existing information assigned to that 'Atom'
-- is paired with the new information in a list structure.
errorAppendInfo :: Atom -> DaoExpr -> Error -> Error
errorAppendInfo = _errAppend
  -- This function is only different from the '_errorAppendInfo' function in that the 'fromString'
  -- function is used to construct the atom, which is the same as the 'read' function.

_errorAppendInfo :: String -> DaoExpr -> Error -> Error
_errorAppendInfo atom = _errAppend (Atom $ Strict.pack atom)
  -- Calling '_errorAppendInfo' assumes it has been called from within this module, and so no
  -- parsing is performed of the string to check that iti is a valid atom.

-- Attach more information to the dictionary contained within the 'Error'.
_errAppend :: Atom -> DaoExpr -> Error -> Error
_errAppend atom newInfo (Error clas (Dict map)) =
  let upd newInfo = \ case
        DaoList list -> DaoList $ _listSnoc list newInfo
        oldInfo      -> DaoList $ _list 1 [newInfo, oldInfo]
  in  Error clas $ Dict $ Map.insertWith upd atom newInfo map

----------------------------------------------------------------------------------------------------

-- | This is the function type used for __all__ pattern matching in the Dao Lisp kernel. More
-- specifically, this function type is an abstraction for two different functions:
--
-- 1. The 'evalDaoRule' and 'Language.Interpreter.Dao.Database.dbQuery' functions, which takes a
--    'Form' input and matches it against a database of 'Rule's.
--
-- 2. Any function of type 'Outliner', which decodes the information stored in Dao 'Form's into
--    arbitrary Haskell data types by pattern matching against the elements of a 'Form'.
--
-- The difference between a "match" function and a "outliner" function is that "outliners" are
-- specific to constructing (matching) Haskell data types from a list of 'DaoExpr's in a 'Form'.
-- Outlining (the opposite of inlining) involves matching a line of 'DaoExpr's in a 'Form' whereas
-- "matching" may convert any pattern type from anywhere.
newtype PatternMatcher unit t m a
  = PatternMatcher
    { unwrapPatternMatcher ::
        ContT (MatchResult t) (StateT (PatternMatcherState unit) m) (MatchResult a)
    }

instance Functor m => Functor (PatternMatcher unit t m) where
  fmap f (PatternMatcher a) = PatternMatcher $ fmap (fmap f) a

instance Monad m => Applicative (PatternMatcher unit t m) where { pure = return; (<*>) = ap; }

instance Monad m => Monad (PatternMatcher unit t m) where
  return = PatternMatcher . return . MatchOK
  (PatternMatcher a) >>= f = PatternMatcher $ a >>= \ case
    MatchOK   a   -> unwrapPatternMatcher $ f a
    MatchQuit err -> return $ MatchQuit err
    MatchFail err -> return $ MatchFail err

instance Monad m => MonadFail (PatternMatcher unit t m) where
  fail msg = PatternMatcher $ ContT $ const $ return $ MatchQuit $
    _daoError "matching" [("reason", DaoString $ Strict.pack msg)]

instance Monad m => MonadPlus (PatternMatcher unit t m) where
  mzero = PatternMatcher $ ContT $ flip ($) $ MatchQuit $ _daoError "matching"
    [("reason", DaoString "Dao Lisp expression didn't match any of the given decoder patterns")]
  mplus (PatternMatcher a) (PatternMatcher b) = PatternMatcher $ do
    st <- lift get
    result <- a
    ContT $ \ next -> case result of
      MatchQuit {}  -> put st >> runContT b next
      MatchOK   {}  -> next result
      MatchFail err -> return $ MatchFail err

instance Monad m => Alternative (PatternMatcher unit t m) where { empty = mzero; (<|>) = mplus; }

instance Monad m => MonadCont (PatternMatcher unit t m) where
  callCC f = PatternMatcher $ callCC $ \ halt ->
    unwrapPatternMatcher $ f (PatternMatcher <$> halt . MatchOK)

instance Monad m => MonadError Error (PatternMatcher unit t m) where
  throwError = PatternMatcher . return . MatchFail
  catchError (PatternMatcher try) catch = PatternMatcher $ do
    result <- try
    case result of
      MatchFail err -> unwrapPatternMatcher $ catch err
      MatchQuit err -> return $ MatchQuit err
      MatchOK   a   -> return $ MatchOK a

instance MonadTrans (PatternMatcher unit t) where
  lift = PatternMatcher . lift . lift . fmap MatchOK

----------------------------------------------------------------------------------------------------

-- | After running a 'PatternMatcher', you will receive a decoded Haskell data structure of type @a@
-- wrapped in a 'MatchOK' constructor, or you can get one of two errors.
--
-- 1. 'MatchQuit' means the 'PatternMatcher' could not match any of the possible pattern against the input
--    expression and backtracked (backtracking happens upon evaluting 'Control.Monad.mzero',
--    'Control.Applicative.empty', or 'matchQuit').
--
-- 2. 'MatchFail' means the 'PatternMatcher' detected a fault somewhere in the input expression and
--    immediately halted.
data MatchResult a = MatchOK a | MatchQuit Error | MatchFail Error
  deriving (Eq, Ord, Functor)

instance Applicative MatchResult where { pure = return; (<*>) = ap; }

instance Monad MatchResult where
  return = MatchOK
  a >>= f = case a of
    MatchOK     a -> f a
    MatchFail err -> MatchFail err
    MatchQuit err -> MatchQuit err

instance MonadPlus MatchResult where
  mzero = MatchQuit $ _matchErr "evaluated to empty" []
  mplus a b = case a of { MatchQuit{} -> b; _ -> a; }

instance Alternative MatchResult where { empty = mzero; (<|>) = mplus; }

instance MonadFail MatchResult where
  fail = MatchFail . flip _matchErr [] . Strict.pack

instance MonadError Error MatchResult where
  throwError = MatchFail
  catchError try catch = case try of { MatchFail err -> catch err; _ -> try; }

instance DaoEncode a => DaoEncode (MatchResult a) where { dao = inlineToForm; }
instance DaoDecode a => DaoDecode (MatchResult a) where { daoDecode = outlineExpr; }

instance DaoEncode a => Inlining  (MatchResult a) where
  inline = \ case
    MatchOK   a   -> inline1Dao (Atom "MatchOK"  ) . inline1Dao a
    MatchQuit err -> inline1Dao (Atom "MatchQuit") . inline1Dao err
    MatchFail err -> inline1Dao (Atom "MatchFail") . inline1Dao err

instance DaoDecode a => Outlining (MatchResult a) where
  outline =
    outlineAtom "MatchOK"   (MatchOK   <$> outlineDaoDecoder) <|>
    outlineAtom "MatchQuit" (MatchQuit <$> outlineDaoDecoder) <|>
    outlineAtom "MatchFail" (MatchFail <$> outlineDaoDecoder)

instance DaoEncode a => Show (MatchResult a) where { showsPrec _ = showList . inlining; }

_mapFailed :: (Error -> Error) -> MatchResult a -> MatchResult a
_mapFailed f = \ case { MatchFail err -> MatchFail $ f err; result -> result; }

----------------------------------------------------------------------------------------------------

data PatternMatcherState unit
  = PatternMatcherState
    { patMatcherTypeRep :: Maybe TypeRep
    , patMatcherIndex   :: !Int
    , patMatcherSource  :: [unit]
    }

_matchQuit :: State (PatternMatcherState unit) Error
_matchQuit = return $ _daoError "matching"
  [("reason", DaoString "evaluation was reduced to an empty decoder")]

_matchErr :: Strict.Text -> [(Strict.Text, DaoExpr)] -> Error
_matchErr msg = matchError msg . fmap (Atom *** id)

-- | Useful for wrapping a 'matchError' in a 'Prelude.Left', which is useful for defining
-- instances of 'DaoDecode'.
_matchErrL :: Strict.Text -> [(Strict.Text, DaoExpr)] -> Either Error void
_matchErrL msg = Left . _matchErr msg

-- | Construct an 'Error' in the 'ErrorClass' of "matching" to indicate that your 'Outliner'
-- function has encountered a condition in which it cannot continue matching, which is either
-- 'matchQuit' (backtracking, allows for backing up and trying again) or 'matchFail' which throws
-- the 'Error' as an exception from which the operation cannot recover and that immediately halts
-- matching.
matchError :: Strict.Text -> [(Atom, DaoExpr)] -> Error
matchError msg = plainError "matching" . ((Atom "reason", DaoString msg) :)

-- | Useful for wrapping a 'matchError' in a 'Prelude.Left', which is useful for defining
-- instances of 'DaoDecode'.
matchErrorL :: Strict.Text -> [(Atom, DaoExpr)] -> Either Error void
matchErrorL msg = Left . matchError msg

-- | This is the function used to backtrack during matching of a list of arguments. The first text
-- parameter to this function should provide an explanation as to why the decoder failed, just in
-- case the backtracking is not caught and the entire decoder fails.
matchQuit :: Monad m => Error -> PatternMatcher unit t m void
matchQuit = PatternMatcher . return . MatchQuit

-- | Throw a decoder exception. This should be used if, during matching, you detect a condition that
-- is absolutely wrong that should flag an immediate and uncatchable halting error.
matchFail :: Monad m => Error -> PatternMatcher unit t m void
matchFail = PatternMatcher . return . MatchFail

resumeMatching
  :: Monad m
  => PatternMatcher unit t m t -> PatternMatcherState unit -> m (MatchResult t, PatternMatcherState unit)
resumeMatching (PatternMatcher f) = runStateT $ runContT f return

runPatternMatch :: Monad m => PatternMatcher unit t m t -> [unit] -> m (MatchResult t, PatternMatcherState unit)
runPatternMatch f src = resumeMatching f $ PatternMatcherState
  { patMatcherTypeRep = Nothing
  , patMatcherIndex   = 0
  , patMatcherSource  = src
  }

-- | Wherever the 'PatternMatcher' happens to be in scanning the current argument list, stop and
-- take whatever arguments remain and pass them all to the given continuation.
dumpArgs :: Monad m => ([unit] -> PatternMatcher unit t m a) -> PatternMatcher unit t m a
dumpArgs = (>>=) $ PatternMatcher $ lift $ state $
  MatchOK . patMatcherSource &&& \ st -> st{ patMatcherSource = [] }

-- | Return a value of type @a@ if and only if all arguments given to the 'PatternMatcher' have been
-- analyzed and there are no more remaining, otherwise backtrack with a call to 'matchQuit'.
returnIfEnd :: Monad m => a -> PatternMatcher unit t m a
returnIfEnd a = PatternMatcher $ ContT $ \ next -> gets patMatcherSource >>= \ case
  [] -> next $ MatchOK a
  _  -> return $ MatchQuit $ _daoError "matching"
    [("reason", DaoString "pattern requires there be no further arguments in order to match")]

_setType :: (Monad m, Typeable a) => Proxy a -> PatternMatcher unit t m a -> PatternMatcher unit t m a
_setType prox (PatternMatcher f) = PatternMatcher $ ContT $ \ next -> case typeRepArgs $ typeOf prox of
  [typ] -> flip runContT next $ do
    oldtyp <- lift $ state $ \ st -> (patMatcherTypeRep st, st{ patMatcherTypeRep = Just typ })
    f <* lift (modify $ \ st -> st{ patMatcherTypeRep = oldtyp })
  typs  -> error $ "in setMatchType, typeRepArgs Proxy returned " ++ show typs

-- | This funcion can help make your programs easier to debug. Evaluating a decoder within this
-- function marks it with a Haskell 'Data.Typeable.TypeRep' (type representation) information that
-- can be obtained from the instance of the 'Data.Typeable.Typeable' class for the data type @a@. If
-- the decoder fails, the 'Data.Typeable.TypeRep' information is attached to the error message,
-- which can be very helpful for debugging.
setMatchType :: (Monad m, Typeable a) => PatternMatcher unit t m a -> PatternMatcher unit t m a
setMatchType = _setType Proxy

-- | Evaluate an 'Outliner' of a different "inner" type, returning the data decoded by the inner
-- type decoder.
subMatch :: Monad m => PatternMatcher unit inner m inner -> PatternMatcher unit outer m inner
subMatch (PatternMatcher f) = PatternMatcher $ ContT $ \ next -> get >>=
  lift . runStateT (runContT f return) >>= \ (result, st) -> case result of
    MatchFail err -> put st >> return (MatchFail err)
    MatchOK   {}  -> put st >> next result
    MatchQuit err -> return $ MatchFail err

-- | Pull the next argument from the argument stack and tries to evaluate a function on it.  If the
-- function evaluation succeeds, the element is shifted off of the argument stack. Provide 'Error'
-- information to be attached to the 'Error' message that might occur if the end of the list of
-- @unit@ arguments has been reached.
matchStep
  :: Monad m
  => [(Atom, DaoExpr)]
  -> (unit -> PatternMatcher unit t m a)
  -> PatternMatcher unit t m a
matchStep info f = do
  st <- PatternMatcher $ lift $ gets MatchOK
  case patMatcherSource st of
    expr:more -> do
      PatternMatcher $ lift $ state $ \ st -> (MatchOK (), st{ patMatcherSource = more })
      f expr
    []        -> matchQuit $
      matchError "end of form reached before required elements decoded" info

-- | Attempts to evaluate an 'Outliner'. If it backtracks, 'Prelude.Nothing' is returned.
maybeMatch :: Monad m => PatternMatcher unit t m t -> PatternMatcher unit t m (Maybe t)
maybeMatch (PatternMatcher f) = PatternMatcher $ ContT $ \ next -> get >>=
  lift . runStateT (runContT f return) >>= \ (result, st) -> case result of
    MatchFail err -> return $ MatchFail err
    MatchOK   t   -> put st >> next (MatchOK $ Just t)
    MatchQuit {}  -> put st >> next (MatchOK Nothing)

----------------------------------------------------------------------------------------------------

-- | The inverse (isomorphism) of the 'Inliner' type is the 'Outliner'; it could also be called a
-- "De-Inliner". The name 'Outliner' was chosen over @DeInliner@ because 'Outliner' is a name that
-- is easier to speak and type.
--
-- An 'Outliner' performs a pattern match against a line of 'DaoExpr' values that have been stored
-- into a 'Form' data structure by the 'inlineToForm' function. If the pattern match is successful,
-- the exact same Haskell data type that was passed to the 'inlineToForm' function SHOULD be
-- returned, assuming you, the programmer, have implemented you 'Inliner' and 'Outliner' functions
-- correctly.
--
-- 'Outliner' is a type for constructing Haskell data type from 'DaoExpr'essions. Since this is a
-- synonym for 'SubOutliner' which is a synonym for 'PatternMatcher', pattern match functions such
-- as 'maybeMatch', 'matchStep', 'subMatch', and 'setMatchType' can all be used to define functions
-- of type 'Outliner' and type 'SubOutliner'.
type Outliner t = SubOutliner t t

-- | This is a type for constructing Haskell data type from 'DaoExpr'essions of type @t@ where an
-- intermediate type @a@ is required to construct the type @t@. This is especially useful for
-- 'Control.Applicative.Applicative' functions which are contsructing a type @t@ from it's component
-- types @a@, @b@, @c@ like so:
--
-- > data T = Mk_T Int String
-- > 
-- > outlineInt :: 'SubOutliner' Mk_T Int
-- > outlineInt = 'outline'
-- > 
-- > outlineString :: 'SubOutliner' Mk_T String
-- > outlineString = 'outline'
-- > 
-- > outline_T :: 'Outliner' T
-- > outline_T = Mk_T <$> outlineInt <*> outlineString
--
type SubOutliner t a = PatternMatcher DaoExpr t Identity a

-- | Run an 'Outliner' function against a list of 'DaoExpr' values.
runOutliner :: Outliner t -> [DaoExpr] -> (MatchResult t, [DaoExpr])
runOutliner f = fmap patMatcherSource . runIdentity . runPatternMatch f

-- | Uses the instance of 'Outlining' to evaluate the 'runOutliner' function.
runOutlining :: Outlining t => [DaoExpr] -> (MatchResult t, [DaoExpr])
runOutlining = runOutliner outline

-- | Use this to decode a 'DaoForm' using an intance of 'Outlining'. This function is useful for
-- instantiating 'daoDecode' from a 'Outliner'.
outlineExpr :: Outlining a => DaoExpr -> Either Error a
outlineExpr = outlineExprWith outline

-- | Similar to 'outlineExpr', but makes use of an intance of 'Outlining'. This function is
-- useful for instantiating 'daoDecode' from a 'Outliner'.
outlineExprWith :: Outliner a -> DaoExpr -> Either Error a
outlineExprWith decoder = \ case
  DaoNull      -> returnWith $ runOutliner decoder []
  DaoForm form -> returnWith $ runOutliner decoder $ toList $ unwrapForm form
  expr         -> _matchErrL "expecting form expression" [("offender", expr)]
  where
    returnWith = fst >>> \ case
      MatchOK   a   -> Right a
      MatchFail err -> Left err
      MatchQuit err -> Left err

-- | This function succeeds only if the 'matchStep' is an element exactly equal to the given
-- 'DaoExpr'. The matched element is passed as a parameter to the given continuation.
outlineExprEq :: DaoExpr -> (DaoExpr -> SubOutliner t a) -> SubOutliner t a
outlineExprEq expr f = matchStep [("required", expr)] $ \ next ->
  if next == expr then f next else matchQuit $
    matchError "expecting exact data" [("required", expr), ("offender", next)]

-- | This function will shift the next argument if the 'primitiveType' of the argument is the given
-- 'DaoExprType'.
outlineExprType :: DaoExprType -> (DaoExpr -> SubOutliner t a) -> SubOutliner t a
outlineExprType typ0 f = matchStep [("required-type", DaoAtom $ typeToAtom typ0)] $ \ next -> do
  let typ1 = primitiveType next
  if typ1 == typ0 then f next else matchQuit $ _outlineTypeErr next typ0

-- | Similar to 'outlineExprEq', but the 'primitiveType' of the given 'DaoExpr' is used to match the
-- 'primitiveType' of the next argment.
outlineExprTypeOf :: DaoExpr -> (DaoExpr -> SubOutliner t a) -> SubOutliner t a
outlineExprTypeOf = outlineExprType . primitiveType

-- | This function combines 'matchStep' with 'daoDecode', taking the next argument from the stack
-- and passing it to the instance of the 'DaoDecode' type class for the type @a@. Failure to decode
-- results in 'matchQuit'.
outlineDaoDecoder :: DaoDecode a => SubOutliner t a
outlineDaoDecoder = matchStep [] $ daoDecode >>> matchQuit ||| pure

-- | If the next argument is a 'Form', pass this form to the given continuation, otherwise
-- backtrack.
outlineAnyForm :: (Form -> Outliner a) -> Outliner a
outlineAnyForm f = let reqtyp = ("required-type", DaoAtom $ typeToAtom DaoFormType) in
  matchStep [Atom *** id $ reqtyp] $ \ case
    DaoForm form -> subMatch $ f form
    expr         -> matchQuit $ _matchErr "expecting any Dao Form"
      [ reqtyp
      , ("offender-type", DaoAtom $ typeToAtom $ primitiveType expr)
      , ("offender", expr)
      ]

-- | If the next argument is a 'Form', pause the current decoder and start a new decoder
-- evaluating the entire 'Form' before returning to and resuming the current decoder.
subOutlineWith :: Outliner inner -> SubOutliner outer inner
subOutlineWith f = matchStep [] $ \ case
  DaoForm form -> do
    old <- PatternMatcher $ lift $ state $ MatchOK . patMatcherSource &&& \ st ->
      st{ patMatcherSource = toList $ unwrapForm form }
    result <- subMatch f
    PatternMatcher $ lift $ state $ MatchOK . const result &&& \ st -> st{ patMatcherSource = old }
  _ -> matchQuit $ _matchErr "expecting Dao form"
          [("required-type", DaoAtom $ typeToAtom DaoFormType)]

-- | Like 'subOutlineWith', but makes use of the 'outline' instance of the returned type @a@.
subOutline :: Outlining a => SubOutliner t a
subOutline = subOutlineWith outline

-- | If the next argument is an 'Atom' that matches the given 'Atom', the next 'Atom' is shifted and
-- the given continuation is evaluated.
outlineAtom :: Atom -> SubOutliner t a -> SubOutliner t a
outlineAtom required f = matchStep [("required-type", DaoAtom $ typeToAtom DaoFormType)] $ \ case
  DaoAtom next | next == required -> f
  _ -> matchQuit $ _matchErr "expecting Atom" [("required-atom", DaoAtom required)]

_outlineTypeOfErrL :: DaoExpr -> DaoExpr -> Either Error void
_outlineTypeOfErrL expr = Left . _outlineTypeOfErr expr

_outlineTypeOfErr :: DaoExpr -> DaoExpr -> Error
_outlineTypeOfErr next = _outlineTypeErr next . primitiveType

_outlineTypeErrL :: DaoExpr -> DaoExprType -> Either Error void
_outlineTypeErrL expr = Left . _outlineTypeErr expr

_outlineTypeErr :: DaoExpr -> DaoExprType -> Error
_outlineTypeErr next t = _daoError "matching"
  [ ("required-type", DaoAtom $ typeToAtom t)
  , ("offender-type", DaoAtom $ typeToAtom $ primitiveType next)
  , ("offender", next)
  ]

----------------------------------------------------------------------------------------------------

-- | This type defines a function type that is isomorphic to the 'Outliner' function type. It is a
-- thin wrapper around a Haskell lazy list data type that really only exists to provide some
-- distinction between plain Haskell lazy-lists and the type used to denote an inverse (isomorphism)
-- for the 'PatternMatcher' function.
--
-- Almost all combinators in this module that take 'Inliner' function types as parameters are
-- defined in continuation passing style. That is to say, you never work with just a single
-- 'Inliner' function type, rather you work with a @('Inliner' a -> 'Inliner' a)@ function type, and
-- sequence these functions using the dot @('Control.Category..')@ operator.
newtype Inliner a = Inliner { inlinerToList :: [a] }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Semigroup (Inliner a) where
  (Inliner a) <> (Inliner b) = Inliner (a <> b)

instance Monoid (Inliner a) where
  mempty = Inliner []
  mappend = (<>)

-- | This function takes an 'Inliner' continuation and evaluates it to a list of elements.
runInliner :: (Inliner a -> Inliner a) -> [a]
runInliner = inlinerToList . ($ empty)

-- | This function evaluates the 'runInliner' on the continuation function produced by applying the
-- type @a@ to the 'inline' function.
inlining :: Inlining a => a -> [DaoExpr]
inlining = runInliner . inline

-- | Evaluate an 'Inlining' value @a@ with the 'inlining' function, then wrap the result into
-- a 'Form' into a 'DaoExpr' using the 'daoForm1' function.
inlineToForm :: Inlining a => a -> DaoExpr
inlineToForm = daoForm1 . inlining

-- | Evaluate an 'Inliner' continuation with the 'runInliner' function, then wrap the result into a
-- 'Form' into a 'DaoExpr' using the 'daoForm1' function.
inlineToFormWith :: (Inliner DaoExpr -> Inliner DaoExpr) -> DaoExpr
inlineToFormWith = daoForm1 . runInliner

-- | A way to wrap a single element @a@ into a continuation passing style 'Inliner' function. This
-- allows you to, for example include a single element @a@ in a list of elements produced by a
-- sequence of 'Inliner' continuations.
inline1 :: a -> (Inliner a -> Inliner a)
inline1 a (Inliner ax) = Inliner (a : ax)

-- | A way to wrap a single element @a@ into a a continuation passing style 'Inliner' function while
-- automatically converting it to a 'DaoExpr' using the 'dao' function. This function is simply
-- defined as @'inline1' . 'dao'@, but this function is used often enough that this shortcut is
-- helpful.
inline1Dao :: DaoEncode a => a -> (Inliner DaoExpr -> Inliner DaoExpr)
inline1Dao = inline1 . dao

-- | Convert a pure Haskell value to a 'DaoExpr' data type. This module provides mappings for the
-- following Haskell types to the following Dao primitive data types:
--
-- * @()@ -> 'DaoVoid'
-- * @[]@ -> 'DaoNull' -- only for empty lists
-- * @forall expr . DaoEncode expr => [expr]@ -> 'DaoList' -- only for non-empty lists
-- * @forall expr . DaoEncode expr => "Data.Map".'Data.Map.Map' 'Atom' expr@ -> 'DaoDict'
-- * @'List' 'DaoExpr'@ -> 'DaoList'
-- * @'Prelude.True' :: 'Prelude.Bool'@ -> 'DaoTrue'
-- * @'Prelude.False' :: 'Prelude.Bool'@ -> 'DaoFalse'
-- * @'Prelude.Int'@ -> 'DaoInt'
-- * @'Prelude.Double'@ -> 'DaoFloat'
-- * @'Prelude.Float'@ -> 'DaoFloat' (converted from 'Prelude.Double')
-- * @'Prelude.String'@ -> 'DaoString'
-- * @"Data.Typeable".'Data.Typeable.TypeRep'@ -> 'DaoString'
-- * @"Data.Text".'Data.Text.Text'@ -> 'DaoString'
-- * @"Data.Text.Lazy".'Data.Text.Lazy.Text'@ -> 'DaoString'
-- * @"Data.Text.Lazy.Builder".'Data.Text.Lazy.Builder.Builder'@ -> 'DaoString'
-- 
-- Also there are a few primitive types defined in this module, namely 'Atom', 'List', 'Form',
-- 'Dict', and 'Error'. These types (as Haskell data types) can also be encoded to 'DaoExpr's.
--
-- * @'Atom'@ -> 'DaoAtom'
-- * @'List' 'DaoExpr'@ -> 'DaoList'
-- * @'Form'@ -> 'DaoForm'
-- * @'Rule'@ -> 'DaoRule'
-- * @'Error'@ -> 'DaoError'
class DaoEncode a where
  dao :: a -> DaoExpr

-- | This a class of functions isomorphic to the class of 'DaoEncode' functions: where 'DaoEncode'
-- converts a simple Haskell data type into single 'DaoExpr' with a 'primitiveType', this function
-- converts a complex Haskell data type with multiple parts into a list of 'DaoExpr's that can be
-- stored in a 'Form'. The 'Outlining' function should also be instantiated with an isomorphic
-- function to the instantiation of this 'inline' function -- i.e. a function that covnerts back to
-- a Haskell data type from a 'Form' without losing information.
--
-- __Note__ that there is an instance of @['DaoExpr']@ for this class, so one easy way to inline a
-- list of 'DaoExpr's is simply to evaluate the 'inline' function on a list of @['DaoExpr']@. For
-- example:
--
-- @
-- 'inline1' ('DaoInt' 5) 'Control.Category..' 'inline' ['DaoInt' 6, 'DaoInt' 7, 'DaoInt' 8]
-- @
--
-- is the same as the equation:
--
-- @
-- \\ next -> 'Inliner' ('Data.Functor.fmap' 'DaoInt' [5,6,7,8]) 'Data.Semigroup.<>' next
-- @
class Inlining a where
  -- | The 'inline' function is defined in continuation passing style (CPS). To evaluate an 'inline'
  -- continuation function, pass 'Control.Applicative.empty' as the second argument.
  --
  -- Often you may want to define an 'inline' instance for a @newtype@. For example:
  --
  -- > newtype MyNewInt = MyNewInt { unwrapMyNewInt :: Int } deriving Num
  -- 
  -- Since 'Int' instantiates 'inline', an easy way to define an 'inline' function for your
  -- 'MyNewInt' type would be to use this equation:
  -- 
  -- @
  -- instance Inliner MyNewInt where
  --     'inline' = ('Data.Semigroup.<>') 'Control.Category..' 'inline' 'Control.Category..' unwrapMyNewInt
  -- @
  --
  -- Which is the point-free style way of expressing an equation like this:
  --
  -- @
  -- instance Inliner MyNewInt where
  --     'inline' (MyNewInt i) next = 'inline' i 'Data.Semigroup.<>' next
  -- @
  inline :: a -> Inliner DaoExpr -> Inliner DaoExpr

instance Inlining [DaoExpr] where { inline = (<>) . Inliner; }

instance DaoEncode DaoExpr       where { dao = id; }
instance DaoEncode ()            where { dao () = DaoVoid; }
instance DaoEncode Int           where { dao = DaoInt; }
instance DaoEncode Double        where { dao = DaoFloat; }
instance DaoEncode Float         where { dao = DaoFloat . realToFrac; }
instance DaoEncode TypeRep       where { dao = DaoString . Strict.pack . show; }
instance DaoEncode String        where { dao = DaoString . Strict.pack; }
instance DaoEncode Strict.Text   where { dao = DaoString; }
instance DaoEncode Lazy.Text     where { dao = DaoString . Lazy.toStrict; }
instance DaoEncode Build.Builder where { dao = DaoString . Lazy.toStrict . Build.toLazyText; }
instance DaoEncode Bool          where { dao = \ case { True -> DaoTrue; False -> DaoNull; }; }

instance DaoEncode expr => DaoEncode [expr] where { dao = daoList . fmap dao; }
instance DaoEncode expr => DaoEncode (Map.Map Atom expr) where { dao = DaoDict . Dict . fmap dao; }

----------------------------------------------------------------------------------------------------

newtype EnvStack = EnvStack (NonEmpty Dict)

_pushEnv :: Dict -> EnvStack -> EnvStack
_pushEnv b (EnvStack (a :| ax)) = EnvStack $ b :| (a : ax)

_popEnv :: EnvStack -> (Dict, EnvStack)
_popEnv (EnvStack (_ :| stack)) = case stack of
  []      -> throw $ _daoError "dao-stack-underflow" []
  a:stack -> (a, EnvStack $ a :| stack)

_lookupEnv :: Atom -> EnvStack -> Maybe DaoExpr
_lookupEnv atom (EnvStack stack) =
  foldl (\ a (Dict d) -> mplus a $ Map.lookup atom d) Nothing (toList stack)

----------------------------------------------------------------------------------------------------

-- | This function type is pretty straight-forward: when you define a Haskell function to be called
-- from within the Dao Lisp interpreter, this is the type of function to define. This function takes
-- a lazy list of arguments which you can deconstruct however you like (using case statements or
-- whatever), but it is recommended you define an 'Outliner' function and deconstruct arguments
-- using 'runOutliner'. Once arguments are deconstructed, perform your computation as a 'DaoEval'
-- function type to produce a result. Then convert the result to a 'DaoExpr' using the 'dao'
-- function, then 'Control.Monad.return' the converted result.
type DaoLispBuiltin = [DaoExpr] -> DaoEval DaoExpr

-- | This is the environment you must initialize before evaluating a 'DaoExpr' using 'DaoEval'.
--
-- To construct a new 'Environment', you can just use ordinary Haskell record syntax:
--
-- @
-- import qualified "Data.Map" as Map
-- import           "Data.Monoid" ('mappend')
-- 
-- myEnv :: 'Environment'
-- myEnv = 'environment'
--     { 'builtins' = 'bifList'
--         [ 'bif' "sum" $ 'monotypeArgList' $
--               'Control.Monad.return' . 'DaoInt' . 'Prelude.sum'
--         , 'bif' "print" $ 'Control.Applicative.pure' $
--               'daoVoid' . ('Data.Functor.fmap' 'strInterpolate' . 'filterEvalForms' 'Control.Monad.>=>' 'Control.Monad.IO.Class.liftIO' . 'Prelude.putStrLn')
--         ]
--     }
-- @
-- 
-- However, there are functions in this module for defining an 'Environment' using continuation
-- passing style:
--
-- @
-- import qualified "Data.Map" as Map
-- import           "Data.Monoid" ('mappend')
-- 
-- myEnv :: 'Environment'
-- myEnv = 'newEnvironment' 'Prelude.$' 'setupBultins'
--     [ 'bif' "sum" 'monotypeArgList' $ 'Control.Monad.return' . 'DaoInt' . 'Prelude.sum'
--     , 'bif' "print" 'Control.Applicative.pure' $
--            'Control.Monad.liftM' ('Prelude.const' 'DaoVoid') . 'Control.Monad.IO.Class.liftIO' . 'Prelude.putStrLn' . 'strInterpolate')
--     ]
-- @     
data Environment
  = Environment
    { builtins  :: Map.Map Atom DaoLispBuiltin  -- ^ Built-In Functions (BIFs)
    , locals    :: Map.Map Atom DaoExpr  -- ^ Functions defined locally, within a single database.
    , envStack  :: EnvStack
    , traceForm :: Maybe (Form -> DaoEval ())
    , traceAtom :: Maybe (Atom -> [DaoExpr] -> DaoEval ())
    , traceBIF  :: Maybe (Atom -> [DaoExpr] -> DaoEval ())
    }

-- | Construct a 'Data.Map.Map' of 'DaoLispBuiltin's from a list of pairs. You can define each pair
-- of the list as a literal tuple, or you can use the 'bif' function to define each pair. To prevent
-- you from accidentally defining the same function more than once, this whole function evaluates to
-- an 'Prelude.error' if you names should happen to collide, but of course this problem will not be
-- caught until the 'bifList' is evaluated at runtime, so it is not an ideal "if it compiles it
-- runs" solution.
bifList :: [(Atom, DaoLispBuiltin)] -> Map.Map Atom DaoLispBuiltin
bifList = Map.fromListWithKey $ \ nm _ _ -> throw $ _daoError "initializing-interpreter"
  [ ("kernel-function", DaoAtom $ Atom "bifList")
  , ("reason", DaoString "duplicate BIF name")
  , ("duplicate-BIF-name", DaoAtom nm)
  ]

-- | An empty 'Environment' type used to construct a new one from scratch. Use this with Haskell
-- record syntax, or else use 'newEnvironment' to construct an 'Environment' to pass to the
-- 'evalDaoIO' function to evaluate a Dao Lisp program.
environment :: Environment
environment = Environment
  { builtins  = Map.empty
  , locals    = Map.empty
  , envStack  = EnvStack $ Dict Map.empty :| []
  , traceForm = Nothing
  , traceAtom = Nothing
  , traceBIF  = Nothing
  }

-- | This function is intended to define elements of a list passed to the 'bifList' function which
-- is then used to initialize the 'builtins' field of the 'Environment' data structure that you pass
-- to 'evalDaoIO'.
--
-- Define a Built-In Function (BIF) to be used by your own personal Dao Lisp interpreter. Pass a
-- name as an 'Atom' (use 'plainAtom', or else compile your program with the @-XOverloadedStrings@
-- setting and use a string literal).
--
-- Then pass an 'Outliner' function which should convert the @['DaoExpr']@ list of input arguments
-- to a Haskell type your function can use. The 'Outliner' function is a pattern matching monad with
-- combinators that allow you to analyze a list of arguments passed to your BIF. Your 'Outliner'
-- will construct a data type, a tuple, or a list of arguments that your BIF takes as input, and
-- after evaluating the BIF it must return a value of type 'DaoEval'.
--
-- For example:
--
-- @
-- 'bifList'
--     [ 'bif' "sum" $
--           'monotypeArgList' $ return . 'DaoInt' . 'Prelude.sum'
--     , 'bif' "square" $
--           ('Data.Functor.fmap' (\\ a -> a * a :: 'Prelude.Int') 'subOutline') 'Control.Applicative.<|>'
--           ('Data.Functor.fmap' (\\ a -> a * a :: 'Prelude.Double') 'subOutline')
--     , 'bif' "lookup" $ do -- Looks up a dictionary, throws an exception if key doesn't exist.
--           dict <- 'subOutline'
--           key  <- 'subOutline'
--           'Prelude.maybe' ('daoFail' $ 'daoError' "dict-lookup" ["key", 'DaoAtom' key])
--               'Control.Applicative.pure' ('lookupDict' key dict)
--     , 'bif' "print" $
--           'dumpArgs' $ \\ args -> do
--               evaldArgs <- 'filterEvalForms' args
--               'Control.Monad.IO.liftIO' $ 'Prelude.putStrLn' $ 'strInterpolate' evaldArgs
--               return 'DaoVoid'
--     ]
-- @
bif :: Atom -> Outliner (DaoEval DaoExpr) -> (Atom, DaoLispBuiltin)
bif name getargs = (,) name $ runOutliner getargs >>> \ case
  (MatchOK  eval, []  ) -> eval
  (MatchOK   {} , args) -> daoFail $ _daoError "calling-builtin"
    [ ("reason", DaoString "extraneous arguments")
    , ("function-name", DaoAtom name)
    , ("arguments", daoList args)
    ]
  (MatchQuit err, _   ) -> daoFail err
  (MatchFail err, _   ) -> daoFail err

-- | This function can be used to define a Built-In Function (@'bif'@) where all arguments must be
-- of the same type, given that this type instantiates the 'daoDecode' function, for example all
-- 'DaoString's or all 'DaoInt's.
monotypeArgList :: DaoDecode arg => ([arg] -> a) -> SubOutliner t a
monotypeArgList f = loop id where
  loop args = (outlineDaoDecoder >>= loop . (args .) . (:)) <|> returnIfEnd (f $ args [])

-- | Create a new 'Environment' to be used to evaluate some Dao Lisp expressions. Pass an update
-- function composed (that is composed with the 'Prelude..' operator) of several setup functions,
-- like 'setupBuiltins', 'setupTraceBIF', 'setupTraceAtom', and 'setupTraceForm'.
newEnvironment :: (Environment -> Environment) -> Environment
newEnvironment = ($ environment)

-- | Append builtin functions to a 'newEnvironment'. A runtime error is reported if more than one
-- function with the same name are given. Pass a list of 'bif' functions as the argument to this
-- function, which should then be passed to 'newEnvironment'.
setupBuiltins :: [(Atom, DaoLispBuiltin)] -> (Environment -> Environment)
setupBuiltins elems env =
  let noDups name _ _ = error $
        "Cannot init Dao Lisp interpreter, " ++
        "setupBuiltins called with multiple built-in functions named \"" ++
        show name ++ "\""
  in  env{ builtins = Map.unionWithKey noDups (builtins env) (Map.fromListWithKey noDups elems) }

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

class DaoFunction expr where { evalDao :: expr -> DaoEval DaoExpr; }

instance DaoFunction DaoExpr where
  evalDao expr = case expr of
    DaoForm form -> evalDao form
    expr         -> return expr

instance DaoFunction Form where
  evalDao form = do
    env <- get
    maybe (return ()) ($ form) (traceForm env)
    let (head :| args) = unwrapForm form
    evalDaoExprWith head args

-- | A monadic function type for evaluating 'DaoExpr' values.
newtype DaoEval a
  = DaoEval
    { unwrapDao :: ReaderT HaltMethod (ContT DaoExpr (StateT Environment IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFail DaoEval where
  fail msg = DaoEval $ ReaderT $ \ halt -> ContT $ const $ get >>= \ env -> 
    liftIO $ evalDaoExprIO env $ do
      halt $ DaoError $ _daoError "builtin-failure" [("reason", DaoString $ Strict.pack msg)]
      return $ error "Dao internal exception: monadic fail did not halt after evaluating halt"

type HaltMethod = DaoExpr -> DaoEval ()

instance MonadState Environment DaoEval where
  state = DaoEval . lift . lift . state

instance MonadCont DaoEval where
  callCC f = DaoEval $ ReaderT $ \ halt -> do
    callCC $ flip runReaderT halt . unwrapDao . f . fmap (DaoEval . ReaderT . const)

evalDaoExprIO :: Environment -> DaoEval DaoExpr -> IO DaoExpr
evalDaoExprIO env (DaoEval f) =
  callCC (runReaderT f . fmap (DaoEval . lift)) `runContT` return `evalStateT` env

evalDaoIO :: DaoFunction f => Environment -> f -> IO DaoExpr
evalDaoIO env = evalDaoExprIO env . evalDao

-- | Throw an exception within the Dao Lisp interpreter.
daoFail :: Error -> DaoEval void
daoFail expr = DaoEval ask >>= \ halt -> halt (DaoError expr) >>
  return (error "Dao internal: daoFail did not halt after evaluating halt")

-- | Catch an exception that was thrown within the Dao Lisp interpreter.
daoCatch :: DaoEval DaoExpr -> (Error -> DaoEval DaoExpr) -> DaoEval DaoExpr
daoCatch (DaoEval try) catch = do
  expr <- callCC $ \ halt -> DaoEval (local (const halt) try)
  case expr of
    DaoError err -> catch err
    expr         -> return expr

-- | This function is similar to the 'Data.Functor.void' function but for the Dao Lisp interpreter,
-- wherein the @'DaoEval'@ function is evaluated but the returned value is disgarded and 'DaoVoid'
-- is returned instead.
daoVoid :: DaoEval void -> DaoEval DaoExpr
daoVoid = fmap $ const DaoVoid

-- | Evaluate a 'DaoExpr' to a form, then apply the arguments.
evalDaoExprWith :: DaoExpr -> [DaoExpr] -> DaoEval DaoExpr
evalDaoExprWith expr args = case expr of
  DaoForm form -> evalDao form >>= flip evalDaoExprWith args
  DaoAtom atom -> evalAtomWith atom args
  DaoError err -> daoFail err
  expr         -> daoFail $ _daoError "form-eval"
    [ ("form-head-wrong-type", DaoAtom $ typeToAtom $ primitiveType expr)
    , ("form-head-value", expr)
    ]

-- | Evaluate a partially applied function, which must be given as a 'Form' expression. The
-- remaining arguments to be applied must be given as a list of 'DaoExpr's.
evalPartial :: Form -> [DaoExpr] -> DaoEval DaoExpr
evalPartial form more = do
  let (head :| args) = unwrapForm form
  evalDaoExprWith head $ args ++ more

-- | Evaluate an 'Atom' with a list of arguments. This function will not call into the 'tracer'.
-- This function also recursively calls 'evalDao' on each argument using the 'filterEvalForms'
-- function.
evalAtomWith :: Atom -> [DaoExpr] -> DaoEval DaoExpr
evalAtomWith cmd = filterEvalForms >=> \ args -> gets (_lookupEnv cmd . envStack) >>= \ case
  Nothing   -> _evalFunction cmd args
  Just expr -> do
    get >>= maybe (return ()) (\ tr -> tr cmd args) . traceAtom
    evalDaoExprWith expr args

_evalFunction :: Atom -> [DaoExpr] -> DaoEval DaoExpr
_evalFunction cmd args = get >>= \ env -> case Map.lookup cmd $ locals env of
  Just expr -> case expr of
    DaoRule rule -> evalDaoRule rule args >>= \ case
      MatchOK  expr -> return expr
      MatchQuit err -> daoFail err
      MatchFail err -> daoFail err
    DaoForm form -> evalPartial form args
    expr         -> case args of
      []           -> return expr
      _            -> daoFail $ _daoError "applied-non-function"
        [ ("reason", DaoString "is not a function or rule type")
        , ("name", DaoAtom cmd)
        , ("primitive-type", DaoAtom $ typeToAtom $ primitiveType expr)
        ]
  Nothing   -> case Map.lookup cmd $ builtins env of
    Nothing   -> daoFail $ _daoError "undefined-function" [("builtin-name", DaoAtom cmd)]
    Just bif  -> maybe (return ()) (\ tr -> tr cmd args) (traceBIF env) >> bif args

-- | Evaluate an 'Atom' by using the 'Atom' to select only a built-in function. Functions defined
-- locally (on the stack) are not considered.
evalFunction :: Atom -> [DaoExpr] -> DaoEval DaoExpr
evalFunction cmd = filterEvalForms >=> _evalFunction cmd

-- | This function will collapse a list of 'DaoExpr's by evaluating all 'DaoForm's within it. It
-- works by filtering a list of 'DaoExpr's such that only 'DaoForm' are extracted and evaluating
-- them with the 'evalDao' function and replaced with the 'DaoExpr' value returned by 'evalDao',
-- while all non 'DaoForm' values are ignored and left in place. Also, all 'DaoVoid' values are
-- removed.
filterEvalForms :: [DaoExpr] -> DaoEval [DaoExpr]
filterEvalForms = liftM filterVoids . mapM evalDao

-- | Evaluate a procedure, taking care to perform a proper tail recursion on the last form of the
-- procedure.
evalProcedure :: [DaoExpr] -> DaoEval DaoExpr
evalProcedure = \ case
  []   -> return DaoVoid
  [a]  -> evalDao a
  a:ax -> evalDao a >> evalProcedure ax

-- | Similar to 'evalProcedure' except each element in the pipeline must be a 'Form' the return
-- value of each 'Form', and the value returned by evaluating 'evalDao' on the form is used as an
-- argument to the 'Form' immediately following it in the list.  Pass the argument to feed into the
-- first arguemnt as the first 'DaoExpr' parameter to this function. Clojure programmers may
-- recognizes this function as being somewhat similar to the @->>@ command.
evalPipeline :: [Form] -> DaoExpr -> DaoEval DaoExpr
evalPipeline = loop where
  run1 = unwrapForm >>> \ (head :| args) -> evalDaoExprWith head . (args ++) . pure
  loop ax = case ax of
    []   -> return
    [a]  -> run1 a
    a:ax -> run1 a >=> loop ax

-- | Evaluate a 'Rule' by matching a list of arguments against it.
evalDaoRule :: Rule -> [DaoExpr] -> DaoEval (MatchResult DaoExpr)
evalDaoRule rule query = flip runRuleMatcher query $ matchRulePattern exec DaoVoid rule where
  exec _ dict = ruleMatchLift $ do
    modify $ \ st -> st{ envStack = _pushEnv dict $ envStack st }
    result <- evalPartial (ruleAction rule) query
    modify $ \ st -> st{ envStack = snd $ _popEnv $ envStack st }
    return result

----------------------------------------------------------------------------------------------------

-- | This is the top-level of the abstract syntax tree for Dao production rule databases. The actual
-- 'Language.Interpreter.Dao.Database' data type is defined in the "Language.Interpreter.Dao"
-- module.
data DBEntry
  = NamedEntry Atom DaoExpr
  | UnnamedEntry Rule
  deriving (Eq, Ord, Typeable)

instance Read DBEntry where
  readsPrec p str = let fail msg = throw $ _daoError "parsing" [("reason", DaoString msg)] in
    case readsPrec p str of
      [(rule, str)] -> case str of
        ';' : str     -> [(UnnamedEntry rule, _sp str)]
        _             -> fail "expecting semicolon after rule for 'UnnamedEntry' expression"
      _             -> case readsPrec p str of
        [(atom, str)] -> case str of
          '=' : str     -> case readsPrec p $ _sp str of
            [(form, str)] -> case str of
              ';' : str     -> [(NamedEntry atom form, _sp str)]
              _             -> fail "expecting semicolon after 'NamedEntry' expression"
            _             -> fail "expecting parenthetical 'Form' expression after '=' character"
          _             -> fail "expecting '=' character after non-'Rule' top-level expression"
        _             -> fail "top-level expression neither 'NamedEntry' nor 'Rule'"

instance Show DBEntry where
  showsPrec p = \ case
    NamedEntry nm form -> showsPrec p nm . (" = " ++) . showsPrec p form . (';' :)
    UnnamedEntry  rule -> showsPrec p rule . (';' :)

----------------------------------------------------------------------------------------------------

-- | A unit pattern used in 'Rule's to match against 'DaoExpr'essions.
data ExprPatUnit
  = AnyExpr
    -- ^ Matches anything at all.
  | TestExpr      !Form
    -- ^ Matches a single element and calls a function on it. If the function call throws an error,
    -- the pattern match fails. If the function call returns anything at all, the matching
    -- expression is assinged the return value.
  | EqExpr        !DaoExpr
    -- ^ Match the 'DaoExpr' using @('Prelude.==')@
  | NotEqExpr     !DaoExpr
    -- ^ Match the 'DaoExpr' using @('Prelude./=')@
  | IsPrimType    !DaoExprType
    -- ^ Match any 'DaoExpr' for which the 'primitiveType' matches this given value.
  | IsNotPrimType !DaoExprType
    -- ^ Match any 'DaoExpr' for which the 'primitiveType' does not matche this given value.
  | AnyPrimType   !(List DaoExprType)
    -- ^ Match any 'DaoExpr' for which the any of the given 'primitiveType' matches any of these
    -- given values.
  | NoPrimType    !(List DaoExprType)
    -- ^ Match any 'DaoExpr' for which the any of the given 'primitiveType' does not match any of
    -- these given values.
  deriving (Eq, Ord, Typeable)

instance Show ExprPatUnit where
  showsPrec _ = showList . inlining
  showList = showList . (>>= inlining)

instance Inlining ExprPatUnit where
  inline = (<>) . Inliner . \ case
    AnyExpr         -> [DaoAtom "any"]
    TestExpr      a -> [DaoAtom "$", DaoForm a]
    EqExpr        a -> [DaoAtom "==", a]
    NotEqExpr     a -> [DaoAtom "/=", a]
    IsPrimType    a -> [DaoAtom "type==", DaoAtom $ typeToAtom a]
    IsNotPrimType a -> [DaoAtom "type/=", DaoAtom $ typeToAtom a]
    AnyPrimType   a -> [DaoAtom "type==", DaoList $ DaoAtom . typeToAtom <$> a]
    NoPrimType    a -> [DaoAtom "type/=", DaoList $ DaoAtom . typeToAtom <$> a]

instance Outlining ExprPatUnit where
  outline = msum
    [ outlineAtom "any" $ return AnyExpr
    , outlineAtom "$"   $ outlineAnyForm $ return . TestExpr
    , outlineAtom "=="  $ matchStep [] $ return . EqExpr
    , outlineAtom "/="  $ matchStep [] $ return . NotEqExpr
    , _outlineTypeList "type==" IsPrimType   AnyPrimType
    , _outlineTypeList "type/=" IsNotPrimType NoPrimType
    ]

_outlineTypeList
  :: Strict.Text -> (DaoExprType -> ExprPatUnit)
  -> (List DaoExprType -> ExprPatUnit)
  -> SubOutliner t ExprPatUnit 
_outlineTypeList which atomConstr listConstr = let sym = Atom which in outlineAtom sym $
  mplus (atomConstr <$> outlineDaoDecoder) $ matchStep [] $ \ case
    DaoList t -> listConstr . plainList <$> forM (unwrapList t) (daoDecode >>> matchFail ||| pure)
    expr      -> matchFail $ matchError "expecting type primitive, or list of type primitives"
      [ ("constructor", DaoAtom sym)
      , ("on-atom", DaoAtom sym)
      , ("offender-type", DaoAtom $ typeToAtom $ primitiveType expr)
      , ("offender", expr)
      ]

----------------------------------------------------------------------------------------------------

-- | A list pattern unit is a type that can matches zero or more elements in a list, and is usually
-- matched against the start of the list. When encoding these expressions to a 'Form' the pattern
-- @pat@ is encoded first, followed by an optional atom with the following rules:
--
-- 1. If there is no atom, it is a 'Single' pattern.
-- 2. If the atom is @"?"@, it is a 'ZeroOrOne' pattern.
-- 3. If the atom is @*NNN@ where "NNN" is an integer literal (e.g. @*15@) it is an 'AtLeast'
--     pattern.
-- 4. If the atom is @*MMM<NNN@ where "NNN" and "MMM" are integer literals (e.g. @*15<25@) it is an
--     'AtMost' pattern.
data GenPatternUnit pat
  = Single              !pat
    -- ^ A 'Single' pattern is simply coded as an ordinary pattern of type @pat@, for example
    -- @"Hello, world!"@ will parse to a value @'Single' ('DaoString' "Hello, world!")@. When
    -- evaluated by the Dao Lisp interpreter, it will match a single pattern of type @pat@ to the
    -- head of the stack, shifting the stack if it succeeds, or else failing without shifting.
  | ZeroOrOne           !KleeneType !pat
    -- ^ A 'ZeroOrOne' pattern is coded as an ordinary pattern of type @pat@ followed by a single
    -- question-mark atom, for example @"Hello, world!" ?@ will parse to a value
    -- @'ZeroOrOne' ('DaoString' "Hello, world!")@. When evaluated by the Dao Lisp interpreter, it
    -- will try to match a single pattern of type @pat@ to the head of the stack and shift the
    -- stack, but will succeed without shifting the stack if the pattern is not matched.
  | AtLeast   !Int      !KleeneType !pat
    -- ^ An 'AtLeast' pattern will match the given pattern of type @pat@ starting from the head of
    -- the stack, and will continue matching matching elements on the stack until cannot continue.
    -- Once matching cannot continue, if the number of elements matched is less than the given
    -- 'Prelude.Int' number of elements, the match fails and no elements are shifted. Otherwise, the
    -- number of elements are shifted off of the stack and the match succeeds.
  | AtMost    !Int !Int !KleeneType !pat
    -- ^ Similar to 'AtLeast' (must specify the lower bound), except there is also an upper bound.
    -- After matching cannot continue, if the number of elements matched exceeds the given
    -- 'Prelude.Int' upper bound, matching fails and no elements are shifted off of the stack.
  | FewerThan !Int !Int !KleeneType !pat
    -- ^ Similar to 'AtMost', except matching does not fail if more than the given upper-bound
    -- number of elements match, rather the maximum number of elements are shifted and matching
    -- succeeds.
  deriving (Eq, Ord, Typeable)

data KleeneType
  = CheckOnce   -- ^ symbolized by an absence of a tag
  | CheckLazy   -- ^ symbolized by the @*?@ tag
  | CheckGreedy -- ^ symbolized by the @*!@ tag
  deriving (Eq, Ord, Show, Typeable, Enum)

instance Inlining KleeneType where
  inline = (<>) . Inliner . \ case
    CheckOnce   -> [DaoAtom "*" ]
    CheckLazy   -> [DaoAtom "*?"]
    CheckGreedy -> [DaoAtom "*!"]

instance Outlining KleeneType where
  outline = flip mplus (return CheckOnce) $ matchStep [] $ \ case
    DaoAtom (Atom a) -> case a of
      "*?" -> return CheckLazy
      "*!" -> return CheckGreedy
      "*"  -> return CheckOnce
      _    -> empty
    _    -> empty

instance Inlining pat => Show (GenPatternUnit pat) where
  showsPrec _ = showList . inlining
  showList = showList . (>>= inlining)

instance Inlining pat => Inlining (GenPatternUnit pat) where
  inline = \ case
    Single          pat -> inline pat
    ZeroOrOne     k pat -> inline pat . inline1
      (DaoAtom $ Atom $ case k of { CheckOnce -> "?"; CheckGreedy -> "?!"; CheckLazy -> "??"; })
    AtLeast   i   k pat -> inline pat . inline k . inline1 (DaoInt i)
    AtMost    i j k pat -> inline pat . inline k . inline [DaoInt i, DaoInt j, DaoAtom "!"]
    FewerThan i j k pat -> inline pat . inline k . inline [DaoInt i, DaoInt j]

instance Outlining pat => Outlining (GenPatternUnit pat) where
  outline = _deGenPatUnit outline

_deGenPatUnit :: Outliner pat -> Outliner (GenPatternUnit pat)
_deGenPatUnit decodepat = subMatch decodepat >>= \ pat ->
  ( subMatch outline >>= \ k ->
      ( matchStep [] $ \ case
          DaoInt i -> mplus
            ( matchStep [] $ \ case
                DaoInt j -> mplus
                  ( matchStep [] $ \ case
                      DaoAtom (Atom "!") -> return $ AtMost i j k pat
                      _                  -> mzero
                  )
                  (return $ FewerThan i j k pat)
                _        -> mzero
            )
            (return $ AtLeast i k pat)
          _        -> mzero
      ) -- <|>
        -- (matchFail $ _matchErr "expecting integer after token" [("on-input", inlineToForm k)])
  ) <|>
  ( matchStep [] $ \ case
      DaoAtom (Atom a) -> let ok = return . flip ZeroOrOne pat in case a of
        "?"  -> ok CheckOnce
        "?!" -> ok CheckGreedy
        "??" -> ok CheckLazy
        _    -> mzero
      _                -> mzero
  ) <|>
  (return $ Single pat)

----------------------------------------------------------------------------------------------------

-- | This data type optionally attaches some sort of type information (given by the type variable
-- @typ@) to a pattern of type @pat@.
--
-- In an 'RulePatternChoice', a type is just an atom referring to a type function. Once the pattern is
-- matched, the elements of the pattern are passed as arguments to this function, and if the
-- function returns any non-'Error' value, the pattern succeeds. If the function returns an 'Error',
-- the pattern match fails.
--
-- The encoded Dao Lisp syntax for a 'TypedPattern' is the ordinary syntax for @pat@ followed by a
-- colon, followed by an atom referencing the type function to be called. For example:
--
-- > type== String *2:(isPersonsName)
-- > type== String *2:(isPersonsName)*?
-- > type== String *2:(isPersonsName)*!
--
-- This pattern will match two or more strings, then pass all matching strings as arguments to a
-- function called @isPersonsName@. What happens next depends on whether the type is followed by a
-- @*?@ or @*!@ tag, or no tag. No tag means the pattern matches as many strings as possible until
-- it can match no more, and if the match succeeds (in this case the @*2@ indicates, there needs to
-- be at least 2 matches), then all of the arguments are passed to the @isPersonsName@ function
-- once, and the return value of this function (error or success) indicates whether the pattern
-- matches.
--
-- But if the @*?@ tag is provided, then as soon as 2 strings have been matched, @isPersonsName@ is
-- called. If @isPersonsName@ returns an error, then the pattern tries to match another string, and
-- if it can match 3 strings then it calls @isPersonsName@ with 3 strings. It keeps trying with
-- successivly more strings until @isPersonsName@ returns a successful result, or until the
-- @type== String@ pattern can match no further strings.
--
-- The @*!@ tag is similar, but "greedy" in that it first tries to match as many strings as
-- possible, then calls @isPersonsName@ with the maximum number of strings it found. It keeps
-- calling @isPersonsName@ with successively fewer strings until @isPersonsName@ returns a
-- non-error, or until fewer than 2 strings remain (again as indicated by the @*2@ pattern).
data TypedPattern pat typ
  = UntypedPattern
    { typeablePattern :: !(GenPatternUnit pat)
    }
  | TypedPattern
    { typeablePattern     :: !(GenPatternUnit pat)
    , typeablePatternType :: !typ
    }
  deriving (Eq, Ord, Typeable)

instance (Inlining pat, Inlining typ) => Show (TypedPattern pat typ) where
  showsPrec _ = showList . inlining
  showList = showList . (>>= inlining)

instance (Inlining pat, Inlining typ) => DaoEncode (TypedPattern pat typ) where
  dao = inlineToForm

instance (Outlining pat, Outlining typ) => DaoDecode (TypedPattern pat typ) where
  daoDecode = outlineExpr

instance (Inlining pat, Inlining typ) => Inlining (TypedPattern pat typ) where
  inline = \ case
    UntypedPattern pat     -> inline pat
    TypedPattern   pat typ -> inline pat . inline1 DaoColon . inline typ

instance (Outlining pat, Outlining typ) => Outlining (TypedPattern pat typ) where
  outline = _deTypedPattern outline outline

_deTypedPattern :: Outliner pat -> Outliner typ -> Outliner (TypedPattern pat typ)
_deTypedPattern decodePat decodeTyp = do
  pat <- subMatch $ _deGenPatUnit decodePat
  flip mplus (return $ UntypedPattern pat) $ matchStep [] $ \ case
    DaoColon -> TypedPattern pat <$>
      ( mplus (subMatch decodeTyp) $ matchFail $
          _matchErr "expecting type expression after colon in pattern expression" []
      )
    _        -> empty

----------------------------------------------------------------------------------------------------

-- | A data type which extends a unit pattern type such as 'ExprPatUnit's or 'GlobUnit's with a
-- feature for binding a matched pattern to an 'Atom' in a local evaluation context. These elements
-- do not stand alone in a larger pattern, however, these are single elements in a list of mutually
-- exclusive patterns.
--
-- The encoded syntax of a 'NamedPattern' is:
--
-- * @pattern@               yields a 'PatConst'
-- * @:name pattern@         yields a 'NamedPattern' with an 'UntypedPattern'
-- * @:name (pattern :type)@ yields a 'NamedPattern' with a 'TypedPatterN'
--
-- That is a colon, followed by a variable name (which must be an atom), followed by a pattern of
-- type @pat@.
data NamedPattern pat typ
  = PatConst     !(TypedPattern pat typ)
    -- ^ This pattern must match, but the match is not bound to a variable.
  | NamedPattern !Atom !(TypedPattern pat typ)
    -- ^ This pattern will bind bind matching elements to a given 'Atom' if it matches.
  deriving (Eq, Ord, Typeable)

instance (Inlining pat, Inlining typ) => Show (NamedPattern pat typ) where
  showsPrec _ = showList . inlining
  showList = showList . (>>= inlining)

instance (Inlining pat, Inlining typ) => Inlining (NamedPattern pat typ) where
  inline = \ case
    PatConst        pat -> inline pat
    NamedPattern nm pat -> inline1 DaoColon . inline1 (DaoAtom nm) . case pat of
      UntypedPattern  pat -> inline pat
      pat                 -> case inlining pat of
        []   -> error "encoded TypedPattern should never produced empty form"
        a:ax -> inline [DaoForm $ Form $ plainList $ a :| ax]

instance (Outlining pat, Outlining typ) => Outlining (NamedPattern pat typ) where
  outline = flip mplus (PatConst <$> subMatch outline) $
    outlineExprEq DaoColon $ const $ matchStep [] $ \ case 
      DaoAtom nm -> NamedPattern nm <$> subMatch (subOutline <|> outline)
      _          -> matchFail $
        _matchErr "expecting name after ':' for named pattern expression" []

----------------------------------------------------------------------------------------------------

-- | A pattern to match a list of pattern units against a list of arguments.
newtype GenPatternSequence typ pat = GenPatternSequence (List (NamedPattern typ pat))
  deriving (Eq, Ord, Typeable)

instance (Inlining typ, Inlining pat) => Show (GenPatternSequence typ pat) where
  showsPrec _ = showList . inlining
  showList = showList . (>>= inlining)

instance (Inlining typ, Inlining pat) => Inlining (GenPatternSequence typ pat) where
  inline (GenPatternSequence list) = _inlineList DaoComma list

instance (Outlining typ, Outlining pat) => Outlining (GenPatternSequence typ pat) where
  outline = _outlineList GenPatternSequence DaoComma

_inlineList :: Inlining a => DaoExpr -> List a -> Inliner DaoExpr -> Inliner DaoExpr
_inlineList sep = unwrapList >>> \ (a :| ax) ->
  foldl (\ f a -> f . inline1 sep . inline a) (inline a) ax

_outlineList :: Outlining a => (List a -> b) -> DaoExpr -> Outliner b
_outlineList constr sep = do
  let patvar = subOutline <|> subMatch outline
  let loop i stack = seq i $! mplus
        (outlineExprEq sep $ const $ patvar >>= loop (i + 1) . (: stack))
        (return $ constr $ _list i stack)
  patvar >>= loop 0 . pure

----------------------------------------------------------------------------------------------------

-- | A second dimension of pattern matching, each pattern in the choice is matched, all successful
-- matches are returned.
newtype GenPatternChoice typ pat = GenPatternChoice (List (GenPatternSequence typ pat))
  deriving (Eq, Ord, Typeable)

instance (Inlining typ, Inlining pat) => Show (GenPatternChoice typ pat) where
  showsPrec _ = showList . inlining
  showList = showList . (>>= inlining)

instance (Inlining typ, Inlining pat) => Inlining (GenPatternChoice typ pat) where
  inline (GenPatternChoice list) = _inlineList DaoSemi list

instance (Outlining typ, Outlining pat) => Outlining (GenPatternChoice typ pat) where
  outline = _outlineList GenPatternChoice DaoSemi

----------------------------------------------------------------------------------------------------

-- | This is the data type expressed in a 'Rule' in a Dao Lisp rule database. It is a sequence of
-- 'RulePattern's which will all be matched against a query, often matching in parallel. If any one
-- of the combined patterns in this choice structure results in a successful pattern match, the
-- whole pattern matches and triggers the action.
--
-- A 'RulePatternChoice' is the data type used to define 'Rule's. To define the 'RulePatternChoice'
-- for a 'Rule', specify a sequence of @'Data.List.NonEmpty.NonEmpty' 'RulePatternNT'@s joined
-- together by the @('Data.Semigroup.<>')@ operator and then evaluate the entire expression using
-- the 'pattern1' constructor to construct a 'RulePatternChoice'. For example:
--
-- @
-- 'pattern1'
--    ( ('patConst'              $ 'Single'    $ 'EqExpr' $ 'DaoString' "My name is") 'Data.Semigroup.<>'
--      ('patNamed' "firstName"  $ 'Single'    $ 'IsPrimType' 'DaoStringType') 'Data.Semigroup.<>'
--      ('patNamed' "secondName" $ 'ZeroOrOne' $ 'IsPrimType' 'DaoStringType')
--    )
-- @
-- 
-- You could also construct multiple @'Data.List.NonEmpty.NonEmpty' 'RulePattern'@s and again join
-- them together, again using the @('Data.Semigroup.<>')@ operator, then evaluate the
-- 'Data.List.NonEmpty.NonEmpty' list to a 'RulePatternChoice' using the 'choice' constructor. For
-- example:
--
-- @
-- 'choice'
--    ( 'pattern' ('patConst'  $ 'Single'    $ 'EqExpr' $ 'DaoAtom' "hello") 'Data.Semigroup.<>'
--      'pattern' ('patTyped'  $ 'plainForm' $ 'DaoAtom' "sum" 'Data.List.NonEmpty.:|' [])
--                ('AtLeast' 2 $ 'IsPrimType' 'DaoIntType')
--    )
-- @
type RulePatternChoice = GenPatternChoice ExprPatUnit RulePatternType

-- | This is a sequence of 'RulePatternNT's will be matched one after another until all units are
-- matched, at which point the whole pattern match succeeds.
type RulePattern = GenPatternSequence ExprPatUnit RulePatternType

-- | This is a __N__ Named and __T__ Typed element of a 'RulePattern'. A unit is a 'RulePatternUnit'
-- that may be a 'NamedPattern' or an unnamed 'PatConst', and it may be a 'TypedPattern' or an
-- 'UntypedPattern'. The 'patConst', 'patNamed', 'patTyped', and 'patNamedTyped' constructors below
-- are given to create a 'RulePatternNT'.
type RulePatternNT   = NamedPattern ExprPatUnit RulePatternType

-- | A 'RulePatternUnit' is an untyped and unnamed unit pattern. See the documentation for
-- 'GenPatternUnit' to learn more about constructing pattern units.
type RulePatternUnit  = GenPatternUnit ExprPatUnit

-- | When typing the elements matched by patterns, the type check is simply a function call
-- expressed as a 'Form' evaluated by 'evalPartial'.
type RulePatternType   = Form

-- | Create an unnamed untyped 'RulePattern' wrapped in a 'Data.List.NonEmpty.NonEmpty' singleton so
-- that it can be combined with other singltons into a structure evaluated by the 'pattern' or
-- 'pattern1' data constructors.
patConst :: RulePatternUnit -> NonEmpty RulePatternNT
patConst expr = flip (:|) [] $ PatConst $ UntypedPattern{ typeablePattern = expr }

-- | Create an named untyped 'RulePattern' wrapped in a 'Data.List.NonEmpty.NonEmpty' singleton so
-- that it can be combined with other singltons into a structure evaluated by the 'pattern' or
-- 'pattern1' data constructors.
patNamed :: Atom -> RulePatternUnit -> NonEmpty RulePatternNT
patNamed nm expr = flip (:|) [] $ NamedPattern nm $ UntypedPattern{ typeablePattern = expr }

-- | Create an unnamed typed 'RulePattern' wrapped in a 'Data.List.NonEmpty.NonEmpty' singleton so
-- that it can be combined with other singltons into a structure evaluated by the 'pattern' or
-- 'pattern1' data constructors.
patTyped :: RulePatternType -> RulePatternUnit -> NonEmpty RulePatternNT
patTyped typ expr = flip (:|) [] $ PatConst $
  TypedPattern{ typeablePattern = expr, typeablePatternType = typ }

-- | Create an named and typed 'RulePattern' wrapped in a 'Data.List.NonEmpty.NonEmpty' singleton so
-- that it can be combined with other singltons into a structure evaluated by the 'pattern' or
-- 'pattern1' data constructors.
patNamedTyped :: Atom -> RulePatternType -> RulePatternUnit -> NonEmpty RulePatternNT
patNamedTyped nm typ expr = flip (:|) [] $ NamedPattern nm $
  TypedPattern{ typeablePattern = expr, typeablePatternType = typ }

-- | Construct a pattern consisting of a sequence of 'RulePatternNT's.
pattern :: NonEmpty RulePatternNT -> NonEmpty RulePattern
pattern = flip (:|) [] . GenPatternSequence . plainList

-- | Construct a 'RulePatternChoice' by evaluting a 'Data.List.NonEmpty.NonEmpty' list of
-- 'RulePattern's that have been joined together using the @('Data.Semigroup.<>')@ operator.
choice :: NonEmpty RulePattern -> RulePatternChoice
choice = GenPatternChoice . plainList

-- | Construct a 'RulePatternChoice' with only one choice.
pattern1 :: NonEmpty RulePatternNT -> RulePatternChoice
pattern1 = choice . pattern

----------------------------------------------------------------------------------------------------

-- | Evaluates the pattern match functionon the 'rulePattern' defined for the given 'Rule'.
matchRulePattern
  :: (a -> Dict -> RuleMatcher a) -> a
  -> Rule
  -> RuleMatcher a
matchRulePattern f a = matchGenPatternChoice f a . rulePattern

-- | This is a function type used to express functions that can match an 'RulePatternChoice' against a
-- list of 'DaoExpr' values. This is the function type used to evaluate pattern matches against
-- rules.
newtype RuleMatcher a
  = RuleMatcher { unwrapRuleMatcher :: PatternMatcher DaoExpr DaoExpr DaoEval a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance MonadCont RuleMatcher where
  callCC f = RuleMatcher $ callCC $ fmap unwrapRuleMatcher f . fmap RuleMatcher

runRuleMatcher :: RuleMatcher DaoExpr -> [DaoExpr] -> DaoEval (MatchResult DaoExpr)
runRuleMatcher (RuleMatcher f) = liftM fst . runPatternMatch f

-- | Evaluate a 'DaoEval' function within a 'RuleMatcher' function.
ruleMatchLift :: DaoEval a -> RuleMatcher a
ruleMatchLift = RuleMatcher . lift

-- | When defining your own rule matching functions, evaluate this rule to indicate the match
-- failed.
noMatch :: RuleMatcher void
noMatch = RuleMatcher $ matchQuit $ _daoError "pattern-match" []

matchExprPatUnit :: ExprPatUnit -> RuleMatcher DaoExpr
matchExprPatUnit pat = RuleMatcher $ matchStep [] $ \ next -> unwrapRuleMatcher $ case pat of
  AnyExpr           -> return next
  TestExpr     form -> ruleMatchLift $ evalPartial form [next]
  EqExpr       expr -> if next == expr then return next else noMatch
  NotEqExpr    expr -> if next == expr then noMatch else return next
  IsPrimType    typ -> if typ == primitiveType next then return next else noMatch
  IsNotPrimType typ -> if typ == primitiveType next then noMatch else return next
  AnyPrimType  typs -> if primitiveType next `elem` typs then return next else noMatch
  NoPrimType   typs -> if primitiveType next `elem` typs then noMatch else return next

-- | This is one of the most complicated pattern matching algorithms there could ever be. It is a
-- @O(n^m)@ time complexity algorithm in the worst case, where @n@ is the size of the input and @m@
-- is the size of the pattern. A "worst-case" pattern is one where every pattern element is an
-- 'AtLeast', 'AtMost', or 'FewerThan' pattern with a 'KleeneType' of 'CheckGreedy' or 'CheckLazy'.
--
-- That said, a well-designed pattern with mostly static pattern matches (single patterns that match
-- single elements) and maybe one or two dynamic patterns ('AtLeast', 'AtMost', 'FewerThan'), will
-- quickly match a pattern that results in an action.
matchGenPatternUnit
  :: GenPatternUnit ExprPatUnit
  -> (Int -> [DaoExpr] -> RuleMatcher a)
  -> RuleMatcher a
matchGenPatternUnit pat next = case pat of
  Single            pat -> matchExprPatUnit pat >>= next 1 . pure
  ZeroOrOne      k  pat ->
    (case k of { CheckOnce -> mplus; CheckGreedy -> mplus; CheckLazy -> flip mplus; })
      (matchExprPatUnit pat >>= next 1 . pure) (next 0 [])
  AtLeast   lo   k  pat -> _matchAtLeast lo pat >>= \ (lo, stack) -> case k of
    CheckOnce             -> _matchMore maxBound pat (lo, stack) >>= uncurry next
    CheckLazy             -> _tryLazy lo maxBound stack pat next
    CheckGreedy           -> do
      (count, stack) <- _matchMore maxBound pat (lo, stack)
      _tryGreedy lo count stack next
  AtMost    lo hi k pat -> _matchAtLeast lo pat >>= \ (lo, stack) -> case k of
    CheckOnce             -> do
      (count, stack) <- _matchMore (hi + 1) pat (lo, stack)
      if count > hi then mzero else next count stack
    CheckLazy             -> _tryLazy lo hi stack pat next
    CheckGreedy           -> do
      (count, stack) <- _matchMore (hi + 1) pat (lo, stack)
      if count > hi then mzero else _tryGreedy lo count stack next
  FewerThan lo hi k pat -> _matchAtLeast lo pat >>= \ (lo, stack) -> case k of
    CheckOnce             -> _matchMore hi pat (lo, stack) >>= uncurry next
    CheckLazy             -> _tryLazy lo hi stack pat next
    CheckGreedy           -> do
      (hi, stack) <- _matchMore hi pat (lo, stack)
      _tryGreedy lo hi stack next

-- | Similar to 'matchGenPatternUnit' but instead of taking a continuation to construct an arbitrary
-- type @a@, it constructs a 'List' and returns it as a 'DaoList' or a 'DaoNull' value if the list
-- is empty.
matchGenPatternUnit' :: GenPatternUnit ExprPatUnit -> RuleMatcher DaoExpr
matchGenPatternUnit' = flip matchGenPatternUnit $ \ i ->
  pure . if i <= 0 then const DaoNull else DaoList . _list (i - 1)

_matchAtLeast :: Int -> ExprPatUnit -> RuleMatcher (Int, [DaoExpr])
_matchAtLeast lim pat = loop 0 [] where
  loop i stack = if lim <= i then return (i, stack) else
    matchExprPatUnit pat >>= (loop $! i + 1) . (: stack)

_matchMore :: Int -> ExprPatUnit -> (Int, [DaoExpr]) -> RuleMatcher (Int, [DaoExpr])
_matchMore lim pat (i, stack) = callCC $ \ halt -> loop halt i stack where
  loop halt i stack = if i >= lim then return (i, stack) else
    matchExprPatUnit pat <|> halt (i, stack) >>= (loop halt $! i + 1) . (: stack)
 
_tryGreedy :: Int -> Int -> [DaoExpr] -> (Int -> [DaoExpr] -> RuleMatcher a) -> RuleMatcher a
_tryGreedy lo hi stack next = msum $ ((uncurry next) $) <$> loop hi stack where
  loop i stack = if i <= lo then [] else case stack of
    []       -> []
    _ : more -> (i, stack) : (loop $! i - 1) more

_tryLazy
  :: Int -> Int -> [DaoExpr] -> ExprPatUnit
  -> (Int -> [DaoExpr] -> RuleMatcher a)
  -> RuleMatcher a
_tryLazy lo lim stack pat next = loop lo stack where
  loop i stack = next i stack <|> if i >= lim then mzero else
    matchExprPatUnit pat >>= (loop $! i + 1) . (: stack)

-- | Evaluate a 'TypedPattern', which first evaluates a pattern match ('matchGenPatternUnit'), then
-- the matched arguments are passed as arguments to a given function that performs a type check
-- computation. The result of the type check computation is passed to the given continuation.
matchTypedPattern :: TypedPattern ExprPatUnit Form -> (DaoExpr -> RuleMatcher a) -> RuleMatcher a
matchTypedPattern pat f = case pat of
  UntypedPattern pat      -> matchGenPatternUnit pat $ \ i -> \ case
    [a] -> f a
    ax  -> f $ DaoForm $ _form i ax
  TypedPattern   pat typf -> matchGenPatternUnit pat $ \ _ expr -> do
    let (a :| ax) = unwrapForm typf
    let args = reverse expr
    result <- ruleMatchLift $ daoCatch (evalDaoExprWith a $ ax ++ args) $ pure . DaoError
    case result of
      DaoError err -> RuleMatcher $ matchQuit err
      result       -> f result

-- | Evaluate a 'NamedPattern', which first evaluates a typed pattern match ('matchTypedPattern'),
-- then the matched arguments are passed as arguments to a given function that performs a type check
-- computation. The result of the type check computation is assigned to a variable name into the
-- given 'Dict', and the given continuation is called with the updated 'Dict' and the matching
-- 'DaoExpr'. For unnamed patterns the given 'Dict' is not updated.
matchNamedPattern
  :: NamedPattern ExprPatUnit Form
  -> Dict
  -> (DaoExpr -> Dict -> RuleMatcher a)
  -> RuleMatcher a
matchNamedPattern pat dict f = case pat of
  PatConst        pat -> matchTypedPattern pat $ flip f dict
  NamedPattern nm pat -> matchTypedPattern pat $ \ expr ->
    f expr $ insertDict (flip const) nm expr dict

-- | Evaluates an entire 'GenPatternSequence' against an entire list of arguments.
matchGenPatternSequence
  :: GenPatternSequence ExprPatUnit Form
  -> Dict -> (Dict -> RuleMatcher a)
  -> RuleMatcher a
matchGenPatternSequence (GenPatternSequence list) dict f = init $ unwrapList list where
  init (a :| ax) = matchNamedPattern a dict $ const $ loop ax
  loop ax dict = case ax of
    []   -> f dict
    a:ax -> matchNamedPattern a dict $ const $ loop ax

-- | This function performs a fold over all patterns in the 'GenPatternChoice' that match the given
-- input. Each pattern in the 'GenPatternChoice' is matched, and if successful, the match result is
-- passed to the given continuation along with the value to be folded.
matchGenPatternChoice
  :: (a -> Dict -> RuleMatcher a) -> a
  -> GenPatternChoice ExprPatUnit Form
  -> RuleMatcher a
matchGenPatternChoice f fold (GenPatternChoice list) = init $ unwrapList list where
  init (a :| ax) = evalPat a fold >>= loop ax
  evalPat a fold = matchGenPatternSequence a emptyDict (f fold) <|> return fold
  loop = \ case { [] -> return; a:ax -> evalPat a >=> loop ax; }

