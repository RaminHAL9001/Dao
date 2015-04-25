-- "Dao/Object.hs"  Includes a simple AJAX-like data type, and a handy wrapper
-- around 'Data.Dynamic.Dynamic'.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see the URL:
-- <http://www.gnu.org/licenses/agpl.html>.

-- | An 'Object' is a wrapper around a 'Data.Dynamic.Dynamic' data, with a few additional functions
-- attached to the data to make it more useful. These functions allow for compairson using
-- 'Prelude.Eq', ordering using 'Prelude.Ord', and printing using 'Dao.PPrint.PPrintable'.
--
-- The two functions you will use most in this module are:
--
-- * 'obj' which lets you construct an 'Object' from any 'Data.Typeable.Typable', 'Prelude.Eq',
--   'Prelude.Ord' data type
-- * 'fromObj' which lets you convert from an 'Object' to a concrete data type in a pattern matching
--    function like 'Dao.Rule.Rule'.
--
-- Both 'obj' and 'fromObj' are members of the class 'ObjectData'. You can make any of your own data
-- types an 'Object' by instantiating your object into this class. To define an instance of
-- 'ObjectData', the 'fromForeign' function will be useful.
module Dao.Object
  ( ErrorObject(ErrorObject), errorObjectArray, objError, throwObject, showUnquoted, pprintUnquoted,
    -- * Improving on 'Data.Dynamic.Dynamic'
    HasTypeRep(objTypeOf),
    -- * Simple Data Modeling
    Simple(OVoid, ONull, OTrue, OInt, OLong, OChar, OList, OMap, OTree, OStruct),
    SimpleData(simple, fromSimple), simpleNumInfixOp, simpleSetInfixOp,
    T_int, T_long, T_float, T_string, T_char, T_list, T_map, struct, fromStruct,
    stringToTypeMap,
    -- * Data Types as 'Object's
    Object, fromForeign, toForeign, objDynamic, objEquality, objOrdering, objPrinted,
    printable, matchable,
    ObjectData(obj, fromObj), defaultFromObj,
    -- * The Class of Pattern Matching Data Types
    TypePattern(TypePattern), patternTypeRep, infer,
    -- * Working with Void (undefined) Values
    MaybeVoid(voidValue, testVoid), firstNonVoid, allNonVoids
  )
  where

import           Dao.Array
import           Dao.Int
import           Dao.Lens
import           Dao.Logic
import           Dao.Rule
import           Dao.Text.PPrint
import           Dao.Predicate
import           Dao.TestNull
import           Dao.Text
import qualified Dao.Tree       as T

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity

import qualified Data.Array     as A
import           Data.Binary
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.Char
import           Data.Dynamic
import           Data.List (intercalate)
import qualified Data.Map       as M
import qualified Data.Text      as Strict
import qualified Data.Text.Lazy as Lazy

-- | Void values are undefined values. Unliked Haskell's 'Prelude.undefined', 'voidValue' is a concrete
-- value you can test for.
class MaybeVoid o where
  voidValue :: o
  testVoid  :: o -> Bool

-- | This function will scan through a list of values and evaluate to the first value in the list
-- that does not satisfy 'testVoid' predicate. If the list is entirely void, this function evaluates
-- to 'voidValue'.
firstNonVoid :: MaybeVoid o => [o] -> o
firstNonVoid ox = case ox of { [] -> voidValue; o:ox | testVoid o -> firstNonVoid ox; o:_ -> o; }

allNonVoids :: MaybeVoid o => [o] -> [o]
allNonVoids = filter (not . testVoid)

----------------------------------------------------------------------------------------------------

-- | This is a class for dynamic objects, such as 'Data.Dynamic.Dynamic', 'Simple,' and 'Object'.
class HasTypeRep o where
  -- | The 'objTypeOf' function is used to extract the 'Data.Typeable.TypeRep' from the dynamic type
  -- instantiating this class.
  objTypeOf :: o -> TypeRep

instance HasTypeRep TypeRep where { objTypeOf = id; }

instance HasTypeRep Dynamic where { objTypeOf = dynTypeRep; }

----------------------------------------------------------------------------------------------------

-- | This data type is a token containing a 'Data.Typeable.TypeRep'. When constructing a 'RuleTree',
-- this pattern will match any object that matches the type it contains.
newtype TypePattern = TypePattern { patternTypeRep :: TypeRep } deriving (Eq, Ord, Typeable)

instance HasTypeRep TypePattern where { objTypeOf (TypePattern o) = o; }

instance Show TypePattern where { show (TypePattern o) = show o; }

instance PPrintable TypePattern where { pPrint = return . pShow; }

instance SimpleData TypePattern where
  simple (TypePattern o) = simple o
  fromSimple = fmap TypePattern . fromSimple

instance PatternClass TypePattern where
  patternCompare (TypePattern p) o = if p==objTypeOf o then Similar 0.0 else Dissimilar

instance ObjectData TypePattern where
  obj o = printable o $ matchable o $ fromForeign o
  fromObj = defaultFromObj

-- | Use 'next' to take the next item from the current 'Query', evaluate the 'Data.Typeable.TypeRep'
-- of the 'next' token using 'objTypeOf', compare this to the to the
-- 'Data.Typeable.TypeRep' of @t@ inferred by 'Data.Typeable.typeOf'. Compare these two types using
-- @('Prelude.==')@, and if 'Prelude.True' evaluate a function on it.  This function makes a new
-- 'RuleTree' where the pattern in the branch is a 'TypePattern'. For example, if you pass a
-- function to 'infer' which is of the type @('Prelude.String' -> 'Rule' m a)@, 'infer' will create
-- a 'RuleTree' that matches if the token returned by 'next' can be cast to a value of
-- 'Prelude.String'.
infer
  :: forall st t m a . (Functor m, Monad m, Typeable t, ObjectData t)
  => (t -> Rule ErrorObject Object st m a) -> Rule ErrorObject Object st m a
infer f = edges T.BreadthFirst [[obj $ typ f err]] just1 where
  just1 ox = case ox of
    [o] -> predicate (fmapPError RuleError $ fromObj o) >>= f
    _   -> mzero
  typ :: (t -> Rule ErrorObject Object st m a) -> t -> TypePattern
  typ _ ~t = TypePattern $ typeOf t
  err :: t
  err = error "in Dao.Rule.infer: typeOf evaluated undefined"

----------------------------------------------------------------------------------------------------

-- | This data type wraps a value of a foreign data type in a 'Data.Dynamic.Dynamic' data type along
-- with it's 'Prelude.Eq' and 'Prelude.Ord' instance functions. This allows 'Object' to itself be
-- instantiated into 'Prelude.==' and 'Prelude.compare'. To retrieve the foreign data type contained
-- within, use the 'fromForeign' function.
--
-- If your data type instantiates 'Dao.PPrint.PPrintable', you can use 'printable' to store the
-- 'Dao.PPrint.pPrint' instance with this 'Object' data constructor.
--
-- If your data type instantiates 'SimpleData', you can use 'simplifyable' to store the 'simple'
-- instance with this 'Object' data constructor.
data Object
  = Object
    { objDynamic      :: Dynamic
    , objEquality     :: Dynamic -> Bool
    , objOrdering     :: Dynamic -> Ordering
    , objPrinted      :: [PPrint]
    , objSimplified   :: Simple
    , objPatternMatch :: Maybe (Object -> Similarity)
    }
  deriving Typeable

instance Eq  Object where { a == b = objEquality a $ objDynamic b; }

instance Ord Object where { compare a b = objOrdering a $ objDynamic b; }

instance PPrintable Object where { pPrint = objPrinted; }

instance Show Object where { show = showPPrint 4 80 . pPrint; }

instance HasTypeRep Object where { objTypeOf = objTypeOf . objDynamic }

instance SimpleData Object where
  simple = objSimplified
  fromSimple s = return
    Object
    { objDynamic      = toDyn s
    , objEquality     = maybe False (s ==) . fromDynamic
    , objOrdering     = \d ->
        maybe (compare (typeOf s) (dynTypeRep d)) (compare s) (fromDynamic d)
    , objPrinted      = pPrint s
    , objSimplified   = s
    , objPatternMatch = Nothing
    }

-- | Construct an arbitrary 'Data.Typeable.Typeable' data type into a 'Object' data type along with
-- the 'Prelude.==' and 'Prelude.compare' instances for this data type. Use this function with
-- 'printable' and 'simplifyable' to extend this foreign data type with instances for
-- 'Dao.PPrintable' and 'SimpleData'.
--
-- To define an instance of 'obj' for you own custom data type, use this function along with 'obj',
-- as the 'obj' function can convert a 'Object' data type to an 'Object'. Likewise, to define an
-- instance of 'fromObj' for you own custom data type, use 'toForeign' with 'fromObj', as the
-- 'fromObj' function can convert a 'Object' data type to an 'Object' data type.
fromForeign :: (Eq o, Ord o, Typeable o, SimpleData o) => o -> Object
fromForeign o = let d = toDyn o in
  Object
  { objDynamic      = d
  , objEquality     = maybe False (o ==) . fromDynamic
  , objOrdering     = \p -> maybe (compare (dynTypeRep d) (dynTypeRep p)) (compare o) (fromDynamic p)
  , objPrinted      = []
  , objSimplified   = simple o
  , objPatternMatch = Nothing
  }

-- | To define an instance of 'obj' for you own custom data type, use this function along with
-- 'obj', as the 'obj' function can convert a 'Object' data type to an 'Object'. Likewise, to
-- define an instance of 'fromObj' for you own custom data type, use 'toForeign' with 'fromObj',
-- as the 'fromObj' function can convert a 'Object' data type to an 'Object' data type.
toForeign
  :: (Eq o, Ord o, Typeable o, SimpleData o,
      Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m)
  => Object -> m o
toForeign f = maybe mzero return (fromDynamic $ objDynamic f) <|> fromSimple (objSimplified f)

-- | *'Foriegn' data, optional property.* If your data type instantiates 'Dao.PPrint.PPrintable',
-- you can use 'printable' to store the 'Dao.PPrint.pPrint' instance with this 'Object' data
-- constructor.
printable :: PPrintable o => o -> Object -> Object
printable o dat = dat{ objPrinted=pPrint o }

-- | *'Foriegn' data, optional property.* If your data type can act as a pattern that can match
-- arbitrary objects using 'patternCompare', consider modifying your instance of 'obj' to make use of this
-- function. The 'matchable' function lets you define a predicate that can match an arbitrary
-- 'Object'. Specifying 'matchable' will only make use of 'patternCompare' for pattern matching, it will
-- not be used for equality testing ('Prelude.==').
matchable :: (ObjectData o, PatternClass o) => o -> Object -> Object
matchable o dat =
  dat { objPatternMatch = Just $ \p -> case fromObj p of
          PTrue p -> patternCompare o p
          _       -> Dissimilar
      }

----------------------------------------------------------------------------------------------------

-- | An 'ErrorObject' is itself just an 'Object', but usually it is good to make it something that
-- is descriptive of the problem that occurred.
newtype ErrorObject = ErrorObject (Array Object) deriving (Eq, Ord, Typeable)

instance PPrintable ErrorObject where { pPrint (ErrorObject o) = elems o >>= pPrint; }

instance Show ErrorObject where { show = showPPrint 4 80 . pPrint; }

instance SimpleData ErrorObject where
  simple (ErrorObject o) = simple o
  fromSimple = fmap ErrorObject . fromSimple

instance ObjectData ErrorObject where
  obj o = printable o $ fromForeign o
  fromObj = toForeign

errorObjectArray :: Monad m => Lens m ErrorObject (Array Object)
errorObjectArray = newLens (\ (ErrorObject o) -> o) (\o _ -> ErrorObject o)

-- | Construct an 'ErrorObject' from a list of 'Object's.
objError :: [Object] -> ErrorObject
objError = ErrorObject . array

-- | Using this might be more convenient than always writing
-- @'Control.Monad.Error.throwError' $ 'obj' ...)@.
throwObject :: (MonadError ErrorObject m, ObjectData o) => o -> m err
throwObject = throwError . objError . return . obj

-- | 'ErrorObject's thrown are usually error messages. This is a very simple kind of pretty-printer
-- for printing 'ErrorObject's where strings are not quoted. That is, if you catch an error thrown
-- by an expression like @('Control.Monad.Error.throwError' ["Error message\n..."])@, using
-- 'Prelude.show' will output a string like this:
--
-- > ["Error message\n..."]
--
-- But if you use 'showUnquoted' instead of 'Prelude.show' to formulate the output, it will look
-- like this:
--
-- > Error message
-- > ...
showUnquoted :: ErrorObject -> String
showUnquoted = showPPrint 4 100 . pprintUnquoted

-- | Like 'showUnquoted' but outputs the error message as a more structured @['Dao.PPrint.PPrint']@
-- value.
pprintUnquoted :: ErrorObject -> [PPrint]
pprintUnquoted (ErrorObject ox) = cleanSpaces $ elems ox >>= \o ->
  case runIdentity $ runPredicateT $ fromObj o of
    PTrue o -> [pSpace, pText (o::Strict.Text), pSpace]
    _       -> pPrint o

type T_int    = Int
type T_long   = Integer
type T_float  = Double
type T_string = Strict.Text
type T_char   = Char
type T_list   = Array Simple
type T_map    = M.Map Simple Simple
type T_tree   = T.Tree Simple Simple

-- | A 'Simple' object is a similar to a JSON data types: it provides constructors for a few
-- fundamental data type, including integers, floating point numbers, characters, strings, lists,
-- maps, and trees, and these simple types can be combined to create arbitrarily complex data types
-- can be constructed from these fundamental types. Like JSON, it provides a consistent protocol for
-- serializing and deserializing data types.
data Simple
  = OVoid -- ^ this is the 'Simple' analogue of 'Prelude.undefined'.
  | ONull -- ^ this is the 'Simple' analogue of @()@, it is also equivalent to 'Prelude.False'.
  | OTrue 
  | OInt     T_int
  | OLong    T_long
  | OFloat   T_float
  | OChar    T_char
  | OString  T_string
  | OList    T_list
  | OMap     T_map
  | OTree    T_tree
  | OStruct  StrictText Simple
  deriving (Eq, Ord, Typeable)

instance HasTypeRep Simple where
  objTypeOf o = case o of
    OVoid      -> error "Dao.Object.objTypeOf evaluated on void"
    ONull      -> typeOf ()
    OTrue      -> typeOf True
    OInt    ~o -> typeOf o
    OLong   ~o -> typeOf o
    OFloat  ~o -> typeOf o
    OChar   ~o -> typeOf o
    OString ~o -> typeOf o
    OList   ~o -> typeOf o
    OMap    ~o -> typeOf o
    OTree   ~o -> typeOf o
    o          -> typeOf o

instance TestNull Simple where
  nullValue      = ONull
  testNull ONull = True
  testNull _     = False

instance MaybeVoid Simple where
  voidValue      = OVoid
  testVoid OVoid = True
  testVoid _     = False

instance PPrintable Simple where
  pPrint o = case o of
    OVoid       -> []
    ONull       -> [pText "null"]
    OTrue       -> [pText "true"]
    OInt      o -> [pShow o]
    OLong     o -> [pShow o]
    OFloat    o -> [pShow o]
    OChar     o -> [pShow o]
    OString   o -> [pShow o]
    OList     o ->
      [ pChar '['
      , pIndent $ intercalate [pChar ',', pSpace, pNewLine] $ pPrint <$> elems o
      , pChar ']'
      ]
    OMap      o -> pPrintMap pPrint pPrint o
    OTree     o -> pPrintTree o
    OStruct l o -> [pText l, pSpace] ++ pPrint o

instance Show Simple where { show = showPPrint 4 80 . pPrint; }

instance Binary Simple where
  put o = let w = putWord8 in case o of
    OVoid       -> return ()
    ONull       -> w 0
    OTrue       -> w 1
    OInt      o -> w 2 >> put o
    OLong     o -> w 3 >> vlPutInteger o
    OFloat    o -> w 4 >> putFloat64be o
    OChar     o -> w 5 >> vlPutInteger (toInteger $ ord o)
    OString   o -> w 6 >> binaryPutText o
    OList     o -> w 7 >> vlPutInteger (toInteger $ size o) >> mapM_ put (elems o)
    OMap      o -> w 8 >> vlPutInteger (toInteger $ M.size o) >> mapM_ put (M.assocs o)
    OTree     o -> w 9 >> encodeTree o
    OStruct l o -> w 20 >> binaryPutText l >> put o
  get = do
    w <- getWord8
    guard $ A.inRange (A.bounds decoderArray) w
    decoderArray A.! w

pPrintMap :: (key -> [PPrint]) -> (o -> [PPrint]) -> M.Map key o -> [PPrint]
pPrintMap kprin pprin o = 
  [ pChar '{'
  , pIndent $ intercalate [pChar ',', pSpace, pNewLine] $
      fmap (\ (a, b) -> kprin a ++ [pChar ':', pSpace, pIndent (pprin b)]) (M.assocs o)
  , pChar '}'
  ]

pPrintTree :: T_tree -> [PPrint]
pPrintTree o = concat
  [ maybe [pText "()"] (\o -> [pChar '(', pIndent (pPrint o), pNewLine, pChar ')']) (o~>T.leaf)
  , [pSpace, pText "->", pSpace]
  , pPrintMap pPrint pPrintTree (o~>T.branches)
  ]

decoderArray :: A.Array Word8 (Get Simple)
decoderArray = A.array (0, 9)
  [( 0, return ONull)
  ,( 1, return OTrue)
  ,( 2, (OInt    <$> get) <|> fail "expecting Int")
  ,( 3, (OLong   <$> vlGetInteger) <|> fail "expecting Integer")
  ,( 4, (OFloat  <$> getFloat64be) <|> fail "expecting Double")
  ,( 5, (OChar . chr . fromIntegral <$> vlGetInteger) <|> fail "expecting Char")
  ,( 6, (OString <$> binaryGetText) <|> fail "expecting string")
  ,( 7, (OList   <$> decodeArray  ) <|> fail "expecting Array")
  ,( 8, (OMap    <$> decodeMap get) <|> fail "expecting Map")
  ,( 9, (OTree   <$> decodeTree 9 ) <|> _tree_err)
  ,(20, (OStruct <$> binaryGetText <*> get))
  ]

_tree_err :: Get ig
_tree_err = fail "expecting Tree"

decodeArray :: Get (Array Simple)
decodeArray = vlGetInteger >>= loop [] where
  loop ox i = if i==0 then return $ array ox else get >>= \o -> loop (ox++[o]) (i-1)

decodeMap :: Get o -> Get (M.Map Simple o)
decodeMap get1 = vlGetInteger >>= loop [] where
  loop ox i =
    if i==0
    then return $ M.fromList ox
    else (,) <$> get <*> get1 >>= \o -> loop (ox++[o]) (i-1)

decodeTree :: Word8 -> Get (T.Tree Simple Simple)
decodeTree w = let f = decodeMap (getWord8 >>= decodeTree) in case w of
  9  -> T.Tree . (,) Nothing <$> f
  10 -> T.Tree <$> ((,) <$> (Just <$> get) <*> f)
  _  -> _tree_err

encodeTree :: T.Tree Simple Simple -> Put
encodeTree o = case o of
  T.Tree (Nothing, map) -> putWord8  9 >> f map
  T.Tree (Just  o, map) -> putWord8 10 >> put o >> f map
  where
    f map = do
      vlPutInteger (fromIntegral $ M.size map)
      mapM_ (\ (a, b) -> put a >> encodeTree b) (M.assocs map)

-- | Apply a numerical infix operator function to two 'Simple' data types.
simpleNumInfixOp :: (forall a . Num a => a -> a -> a) -> Simple -> Simple -> Simple
simpleNumInfixOp num a b = case a of
  ONull     -> case b of
    ONull     -> ONull
    OInt    b -> OInt $ num 0 b
    OLong   b -> OLong $ num 0 b
    OFloat  b -> OFloat $ num 0 b
    _         -> OVoid
  OInt    a -> case b of
    ONull     -> OInt $ num a 0
    OInt    b -> OInt $ num a b
    OLong   b -> OLong $ num (toInteger a) b
    OFloat  b -> OFloat $ num (fromRational $ toRational a) b
    _         -> OVoid
  OLong   a -> case b of
    ONull     -> OLong $ num a 0
    OInt    b -> OLong $ num a $ toInteger b
    OLong   b -> OLong $ num a b
    OFloat  b -> OFloat $ fromRational (toRational a) + b
    _         -> OVoid
  OFloat  a -> case b of
    ONull     -> OFloat $ num a 0
    OInt    b -> OFloat $ num a $ fromRational (toRational b)
    OLong   b -> OFloat $ num a $ fromRational (toRational b)
    OFloat  b -> OFloat $ num a b
    _         -> OVoid
  _         -> OVoid

-- | Perform a set infix operation on two 'Simple' data types.
simpleSetInfixOp
  :: (forall a . Bits a => a -> a -> a)
  -> (T_map -> T_map -> T_map)
  -> Simple -> Simple -> Simple
simpleSetInfixOp bit set a b = case a of
  OMap  a -> case b of
    OMap  b -> let map = set a b in if M.null map then ONull else OMap map
    _       -> OVoid
  OInt  a -> case b of
    OInt  b -> OInt $ bit a b
    OLong b -> OLong $ bit (toInteger a) b
    _       -> OVoid
  OLong a -> case b of
    OInt  b -> OLong $ bit a $ toInteger b
    OLong b -> OLong $ bit a b
    _       -> OVoid
  _       -> OVoid

----------------------------------------------------------------------------------------------------

-- | This class defines methods for converting a data type to and from a 'Simple' data type. The
-- 'Simple' data type is much like a JSON object in that it is composed of fundamental data types
-- that can be combined to create arbitrarily complex data structures. All objects that instantiate
-- 'SimpleData' can be converted to a 'Simple' type, then "parsed" back into it's original data
-- type.
class (Eq o, Ord o, Typeable o) => SimpleData o where
  simple :: o -> Simple
  fromSimple
    :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m)
    => Simple -> m o

instance SimpleData ()       where
  simple    () = ONull
  fromSimple o = case o of { ONull -> return (); _ -> mzero; }

instance SimpleData Bool     where
  simple     o = if o then OTrue else ONull
  fromSimple o = case o of { ONull -> return False; OTrue -> return True; _ -> mzero; }

instance SimpleData T_int    where
  simple       = OInt
  fromSimple o = case o of { OInt o -> return o; _ -> mzero; }

instance SimpleData T_long   where
  simple       = OLong
  fromSimple o = case o of { OLong o -> return o; _ -> mzero; }

instance SimpleData T_float  where
  simple       = OFloat
  fromSimple o = case o of { OFloat o -> return o; _ -> mzero; }

instance SimpleData T_char   where
  simple       = OChar
  fromSimple o = case o of { OChar o -> return o; _ -> mzero; }

instance SimpleData String   where
  simple       = OString . Strict.pack
  fromSimple o = case o of { OString o -> return $ Strict.unpack o; _ -> mzero; }

instance SimpleData T_string where
  simple       = OString
  fromSimple o = case o of { OString o -> return o; _ -> mzero; }

instance SimpleData o => SimpleData [o] where
  simple       = OList . array . fmap simple
  fromSimple o = case o of { OList o -> mapM fromSimple $ elems o; _ -> mzero; }

instance SimpleData o => SimpleData (Array o) where
  simple       = OList . fmap simple
  fromSimple o = case o of { OList o -> fmap array $ mapM fromSimple $ elems o; _ -> mzero; }

instance (SimpleData key, SimpleData val) => SimpleData (M.Map key val) where
  simple       = OMap . M.fromList . fmap (simple *** simple) . M.assocs
  fromSimple o = case o of
    OMap o -> M.fromList <$> mapM (\ (a, b) -> (,) <$> fromSimple a <*> fromSimple b) (M.assocs o)
    _ -> mzero

instance (SimpleData key, SimpleData val) => SimpleData (T.Tree key val) where
  simple       = OTree . T.fromList . fmap (fmap simple *** simple) . T.assocs T.BreadthFirst
  fromSimple o = case o of
    OTree o -> fmap T.fromList $ forM (T.assocs T.BreadthFirst o) $ \ (a, b) ->
      (,) <$> mapM fromSimple a <*> fromSimple b
    _ -> mzero

instance SimpleData Simple   where
  simple       = id
  fromSimple   = return

stringToTypeMap :: M.Map StrictText TypeRep
stringToTypeMap = M.fromList $ fmap (first toText) $ concat $
  [ [ ("bool"               , typeOf True)
    , ("int"                , typeOf (0::T_int))
    , ("char"               , typeOf ('\0'::T_char))
    , ("float"              , typeOf (0::T_float))
    , ("str"                , typeOf (nullValue :: T_string))
    , ("string"             , typeOf (nullValue :: T_string))
    , ("array"              , typeOf (nullValue :: Array Object))
    , ("list"               , typeOf (nullValue :: T_list))
    , ("map"                , typeOf (nullValue :: T_map))
    , ("dict"               , typeOf (nullValue :: T_map))
    , ("tree"               , typeOf (nullValue :: T_map))
    , ("()"                 , typeOf ())
    , ("Bool"               , typeOf True)
    , ("Int"                , typeOf (0::Int))
    , ("Integer"            , typeOf (0::Integer))
    , ("Double"             , typeOf (0::Double))
    , ("Float"              , typeOf (0::Float))
    , ("Char"               , typeOf '\0')
    , ("String"             , typeOf (""::String))
    , ("StrictText"         , typeOf (nullValue :: StrictText))
    , ("Text"               , typeOf (nullValue :: Strict.Text))
    , ("Data.Text.Text"     , typeOf (nullValue :: Strict.Text))
    , ("Data.Text.Lazy.Text", typeOf (nullValue :: Lazy.Text))
    , ("Simple"             , typeOf OVoid)
    , ("Dao.Object.Simple"  , typeOf OVoid)
    , ("[Simple]"           , typeOf [OVoid])
    , ("[Dao.Object.Simple]", typeOf [OVoid])
    ]
  , do a <- ["Array", "Dao.Array.Array"]
       m <- ["Map"  , "Data.Map.Map"]
       t <- ["Tree" , "Dao.Tree.Tree"]
       let x = ["Object", "Dao.Object.Object"]
       o <- x
       ox <- ([] : (return <$> x))
       ox2 <- ([] : (x >>= \a -> x >>= \b -> [[a, b]]))
       [   (o, typeOf (obj OVoid))
         , ('[':unwords ox++"]", typeOf [obj OVoid])
         , (unwords $ a:ox , typeOf (nullValue :: Array Object))
         , (unwords $ m:ox2, typeOf (nullValue :: M.Map Object Object))
         , (unwords $ t:ox2, typeOf (nullValue :: T.Tree Object Object))
         ]
  ]

instance SimpleData TypeRep  where
  simple       = OString . toText . show
  fromSimple o = case o of
    OString o -> case M.lookup o stringToTypeMap of
      Nothing -> return $ typeOf (obj OVoid)
      Just  o -> return o
    _ -> mzero

instance (Eq pat, SimpleData pat) => SimpleData (Satisfy pat) where
  simple (Satisfy o) = struct "Satisfy" o
  fromSimple = fromStruct "Satisfy" Satisfy

instance (Eq pat, SimpleData pat) => SimpleData (Conjunction pat) where
  simple (Conjunction o) = struct "AND" o
  fromSimple = fromStruct "AND" Conjunction

instance (Eq pat, SimpleData pat) => SimpleData (Statement pat) where
  simple (Statement o) = struct "OR" o
  fromSimple = fromStruct "OR" Statement

instance (SimpleData a, SimpleData b) => SimpleData (a, b) where
  simple (a, b) = OList $ array [simple a, simple b]
  fromSimple o = elems <$> fromSimple o >>= \o -> case o of
    [a, b] -> (,) <$> fromSimple a <*> fromSimple b
    _ -> mzero

instance (SimpleData a, SimpleData b, SimpleData c) => SimpleData (a, b, c) where
  simple (a, b, c) = OList $ array [simple a, simple b, simple c]
  fromSimple o = elems <$> fromSimple o >>= \o -> case o of
    [a, b, c] -> (,,) <$> fromSimple a <*> fromSimple b <*> fromSimple c
    _ -> mzero

instance (SimpleData a, SimpleData b, SimpleData c, SimpleData d) => SimpleData (a, b, c, d) where
  simple (a, b, c, d) = OList $ array [simple a, simple b, simple c, simple d]
  fromSimple o = elems <$> fromSimple o >>= \o -> case o of
    [a, b, c, d] -> (,,,) <$> fromSimple a <*> fromSimple b <*> fromSimple c <*> fromSimple d
    _ -> mzero

instance (SimpleData a, SimpleData b, SimpleData c, SimpleData d, SimpleData e) => SimpleData (a, b, c, d, e) where
  simple (a, b, c, d, e) = OList $ array [simple a, simple b, simple c, simple d, simple e]
  fromSimple o = elems <$> fromSimple o >>= \o -> case o of
    [a, b, c, d, e] -> (,,,,) <$> fromSimple a <*> fromSimple b <*> fromSimple c <*> fromSimple d <*> fromSimple e
    _ -> mzero

instance (SimpleData a, SimpleData b, SimpleData c, SimpleData d, SimpleData e, SimpleData f) => SimpleData (a, b, c, d, e, f) where
  simple (a, b, c, d, e, f) = OList $ array [simple a, simple b, simple c, simple d, simple e, simple f]
  fromSimple o = elems <$> fromSimple o >>= \o -> case o of
    [a, b, c, d, e, f] -> (,,,,,) <$> fromSimple a <*> fromSimple b <*> fromSimple c <*> fromSimple d <*> fromSimple e <*> fromSimple f
    _ -> mzero

instance (SimpleData a, SimpleData b, SimpleData c, SimpleData d, SimpleData e, SimpleData f, SimpleData g) => SimpleData (a, b, c, d, e, f, g) where
  simple (a, b, c, d, e, f, g) = OList $ array [simple a, simple b, simple c, simple d, simple e, simple f, simple g]
  fromSimple o = elems <$> fromSimple o >>= \o -> case o of
    [a, b, c, d, e, f, g] -> (,,,,,,) <$> fromSimple a <*> fromSimple b <*> fromSimple c <*> fromSimple d <*> fromSimple e <*> fromSimple f <*> fromSimple g
    _ -> mzero

-- | The 'struct' and 'fromStruct' functions lets you convert your Haskell data types to and from
-- a 'Simple' data type guarded by a label for the data.
--
-- Lets say you have a data type made of three 'Simple' data types:
--
-- @
-- newtype MyData = MyData ('Prelude.Integer', 'Data.Text.Text', 'Prelude.Bool') deriving 'Data.Typeable.Typeable'
-- @
--
-- This is a good candidate for instantiation of 'StructData'. You could just instantiate
-- 'SimpleData' directly, like so:
--
-- @
-- instance 'SimpleData' MyData where
--     'simple' (MyData tuple) = 'simple' tuple
--     'fromSimple' = MyData 'Control.Applicative.<$>' 'fromSimple'
-- @
--
-- But there is a chance that other data types that are also constructable from an
-- @('Prelude.Integer', 'Data.Text.Text', 'Prelude.Bool')@ tuple could accidentally be constructed
-- from this data. You could create a kind of guard by always making the first element of the
-- 'Simple' container 'Dao.Array.Array' a constant string like so:
--
-- @
-- instance 'SimpleData' MyData where
--     'simple' (MyData (int, txt, bool)) = 'simple' $ 'Dao.Array.array' ['simple' "MyData", simple int, simple txt, simple bool]
--     'fromSimple' simp = do
--         components <- 'Dao.Array.elems' $ 'fromSimple' simp
--         case components of
--             [label, int, txt, bool] | label == 'Dao.Text.toText' "MyData" -> 'Control.Monad.return' $ MyData (int, txt, bool)
--             _ -> 'Control.Monad.mzero'
-- @
--
-- But obviously this is much more tedious and error prone.
--
-- Fortunately, we can automate this structuring convention by using the 'struct' and 'fromStruct'
-- functions:
--
-- @
-- instance 'SimpleData' MyData where
--     'simple' (MyData tuple) = 'struct' "MyData" tuple
--     'fromSimple' = 'fromStruct' "MyData" MyData
-- @
struct :: (ToText label, SimpleData dat) => label -> dat -> Simple
struct label o = OStruct (toText label) (simple o)

-- | See the documentation for the 'struct' function on how to use this function.
fromStruct
  :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m,
      ToText label, SimpleData simp)
  => label -> (simp -> mydata) -> Simple -> m mydata
fromStruct label constr o = case o of
  OStruct name o | name==toText label -> constr <$> fromSimple o
  _ -> mzero

----------------------------------------------------------------------------------------------------

-- | The 'ObjectData' type class lets you define the 'obj' and 'fromObj' functions for your own data
-- type. You could always use the 'fromForeign' or 'toForeign' functions, but you would also have to
-- remember to always call 'matchable' and 'printable' composed with 'fromForeign' for data types
-- that are in the 'PatternClass' and 'Dao.Text.PPrint.PPrintable' class -- if you didn't
-- 'matchable' 'Object's would not be able to access their 'patternCompare' or 'pPrint' instances.
--
-- By instantiating the 'ObjectData' type class, you can call 'fromForeign' and 'toForeign' with as
-- many other initializing functions ('matchable', 'printable') as necessary, so anyone who wraps
-- your data type in an 'Object' with 'obj' will know it can access all the additional functions you
-- provided to it.
class (Eq o, Ord o, Typeable o, SimpleData o) => ObjectData o where
  obj :: o -> Object
  fromObj
    :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m)
    => Object -> m o

-- | This is the function you should use to instantiate the 'fromObj' function for most any custom
-- data type.
defaultFromObj
  :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m,
      Eq o, Ord o, Typeable o, SimpleData o)
  => Object -> m o
defaultFromObj = fromObj >=> toForeign

instance ObjectData Object   where { obj = id; fromObj = return; }

instance ObjectData Simple   where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData ()       where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData Bool   where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData T_int    where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData T_long   where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData T_float  where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData T_char   where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData T_string where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData String where
  obj   o = printable o $ fromForeign o
  fromObj = toForeign

instance ObjectData o => ObjectData (Array o) where
  obj   o = printable (simple o) $ fromForeign o
  fromObj = toForeign

instance ObjectData o => ObjectData [o] where
  obj   o = printable (simple o) $ fromForeign o
  fromObj = toForeign

instance (ObjectData key, ObjectData val) => ObjectData (M.Map key val) where
  obj   o = printable (simple o) $ fromForeign o
  fromObj = toForeign

instance (ObjectData key, ObjectData val) => ObjectData (T.Tree key val) where
  obj   o = printable (simple o) $ fromForeign o
  fromObj = toForeign

instance ObjectData TypeRep where
  obj   o = matchable o $ printable (simple o) $ fromForeign o
  fromObj = toForeign

instance ObjectData pat => ObjectData (Satisfy pat) where
  obj   o = printable (simple o) $ fromForeign o
  fromObj = toForeign

instance ObjectData pat => ObjectData (Conjunction pat) where
  obj   o = printable (simple o) $ fromForeign o
  fromObj = toForeign

instance ObjectData pat => ObjectData (Statement pat) where
  obj   o = printable (simple o) $ fromForeign o
  fromObj = toForeign

