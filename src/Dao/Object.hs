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
    Simple(OVoid, ONull), SimpleData(simple, fromSimple), simpleNumInfixOp, simpleSetInfixOp,
    T_int, T_long, T_float, T_string, T_char, T_list, T_map,
    -- * Wrapping 'Data.Dynamic.Dynamic' Data Types
    Passport, fromForeign, toForeign, objDynamic, objEquality, objOrdering, objPrinted,
    printable, matchable,
    -- * 'Object': Union Type of 'Simple' and 'Passport'
    Object(OSimple, OForeign),
    TypePattern(TypePattern), patternTypeRep, infer,
    -- * Custom data types as 'Object's
    ObjectData(obj, fromObj), defaultFromObj,
    objectMapUnion, objectMapIntersection,
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
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity

import qualified Data.Array     as A
import           Data.Binary
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.Char
import           Data.Dynamic
import           Data.List (intercalate, nub)
import qualified Data.Map       as M
import           Data.Monoid
import qualified Data.Text      as Strict

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
  obj p = obj $ printable p $ matchable p $ fromForeign p
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
infer f = tree T.BreadthFirst [[obj $ typ f err]] just1 where
  just1 ox = case ox of
    [o] -> predicate (fmapPError RuleError $ fromObj o) >>= f
    _   -> mzero
  typ :: (t -> Rule ErrorObject Object st m a) -> t -> TypePattern
  typ _ ~t = TypePattern $ typeOf t
  err :: t
  err = error "in Dao.Rule.infer: typeOf evaluated undefined"

----------------------------------------------------------------------------------------------------

-- | This data type wraps a value of a foreign data type in a 'Data.Dynamic.Dynamic' data type along
-- with it's 'Prelude.Eq' and 'Prelude.Ord' instance functions. This allows 'Passport' to itself be
-- instantiated into 'Prelude.==' and 'Prelude.compare'. To retrieve the foreign data type contained
-- within, use the 'fromForeign' function.
--
-- If your data type instantiates 'Dao.PPrint.PPrintable', you can use 'printable' to store the
-- 'Dao.PPrint.pPrint' instance with this 'Passport' data constructor.
--
-- If your data type instantiates 'SimpleData', you can use 'simplifyable' to store the 'simple'
-- instance with this 'Passport' data constructor.
data Passport
  = Passport
    { objDynamic      :: Dynamic
    , objEquality     :: Dynamic -> Bool
    , objOrdering     :: Dynamic -> Ordering
    , objPrinted      :: [PPrint]
    , objSimplified   :: Simple
    , objPatternMatch :: Maybe (Object -> Similarity)
    }
  deriving Typeable

instance Eq  Passport where { a == b = objEquality a $ objDynamic b; }

instance Ord Passport where { compare a b = objOrdering a $ objDynamic b; }

instance PPrintable Passport where { pPrint = objPrinted; }

instance Show Passport where { show = showPPrint 4 80 . pPrint; }

instance HasTypeRep Passport where { objTypeOf = objTypeOf . objDynamic }

instance SimpleData Passport where
  simple = objSimplified
  fromSimple s = return
    Passport
    { objDynamic      = toDyn s
    , objEquality     = maybe False (s ==) . fromDynamic
    , objOrdering     = \d ->
        maybe (compare (typeOf s) (dynTypeRep d)) (compare s) (fromDynamic d)
    , objPrinted      = pPrint s
    , objSimplified   = s
    , objPatternMatch = Nothing
    }

-- | Construct an arbitrary 'Data.Typeable.Typeable' data type into a 'Passport' data type along with
-- the 'Prelude.==' and 'Prelude.compare' instances for this data type. Use this function with
-- 'printable' and 'simplifyable' to extend this foreign data type with instances for
-- 'Dao.PPrintable' and 'SimpleData'.
--
-- To define an instance of 'obj' for you own custom data type, use this function along with 'obj',
-- as the 'obj' function can convert a 'Passport' data type to an 'Object'. Likewise, to define an
-- instance of 'fromObj' for you own custom data type, use 'toForeign' with 'fromObj', as the
-- 'fromObj' function can convert a 'Passport' data type to an 'Object' data type.
fromForeign :: (Eq o, Ord o, Typeable o, SimpleData o) => o -> Passport
fromForeign o = let d = toDyn o in
  Passport
  { objDynamic      = d
  , objEquality     = maybe False (o ==) . fromDynamic
  , objOrdering     = \p -> maybe (compare (dynTypeRep d) (dynTypeRep p)) (compare o) (fromDynamic p)
  , objPrinted      = []
  , objSimplified   = simple o
  , objPatternMatch = Nothing
  }

-- | To define an instance of 'obj' for you own custom data type, use this function along with
-- 'obj', as the 'obj' function can convert a 'Passport' data type to an 'Object'. Likewise, to
-- define an instance of 'fromObj' for you own custom data type, use 'toForeign' with 'fromObj',
-- as the 'fromObj' function can convert a 'Passport' data type to an 'Object' data type.
toForeign
  :: (Eq o, Ord o, Typeable o, SimpleData o,
      Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m)
  => Passport -> m o
toForeign f = maybe mzero return (fromDynamic $ objDynamic f) <|> fromSimple (objSimplified f)

-- | *'Foriegn' data, optional property.* If your data type instantiates 'Dao.PPrint.PPrintable',
-- you can use 'printable' to store the 'Dao.PPrint.pPrint' instance with this 'Passport' data
-- constructor.
printable :: PPrintable o => o -> Passport -> Passport
printable o dat = dat{ objPrinted=pPrint o }

-- | *'Foriegn' data, optional property.* If your data type can act as a pattern that can match
-- arbitrary objects using 'patternCompare', consider modifying your instance of 'obj' to make use of this
-- function. The 'matchable' function lets you define a predicate that can match an arbitrary
-- 'Object'. Specifying 'matchable' will only make use of 'patternCompare' for pattern matching, it will
-- not be used for equality testing ('Prelude.==').
matchable :: (ObjectData o, PatternClass o) => o -> Passport -> Passport
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
  obj = obj . simple
  fromObj = fromObj >=> fromSimple

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
type T_list   = Array Object
type T_map    = M.Map Object Object
type T_tree   = T.Tree Object Object
type T_type   = TypeRep

-- | A 'Simple' object is a essentially a model of JSON data types. It models any data type that
-- contains a few fundamental types, including integers, floating point numbers, characters,
-- strings, lists, and maps. All complex data types can be constructed from these fundamental types.
--
-- You use 'simple' to wrap an arbitrary 'Data.Typeable.Typeable' data type into an 'Object'
-- constructor, and 'fromSimple' to unwrap the data type from the 'Object' constructor.
data Simple
  = OVoid -- ^ this is the 'Simple' analogue of 'Prelude.undefined'.
  | ONull -- ^ this is the 'Simple' analogue of @()@
  | OTrue 
  | OInt     T_int
  | OLong    T_long
  | OFloat   T_float
  | OChar    T_char
  | OString  T_string
  | OList    T_list
  | OMap     T_map
  | OTree    T_tree
  | OType    T_type
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
    OType   ~o -> typeOf o

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
    OVoid     -> []
    ONull     -> [pText "null"]
    OTrue     -> [pText "true"]
    OInt    o -> [pShow o]
    OLong   o -> [pShow o]
    OFloat  o -> [pShow o]
    OChar   o -> [pShow o]
    OString o -> [pShow o]
    OList   o ->
      [ pChar '['
      , pIndent $ intercalate [pChar ',', pSpace, pNewLine] $ pPrint <$> elems o
      , pChar ']'
      ]
    OMap    o -> pPrintMap pPrint o
    OTree   o -> pPrintTree o
    OType   o -> [pShow o]

instance Show Simple where { show = showPPrint 4 80 . pPrint; }

instance Binary Simple where
  put o = let w = putWord8 in case o of
    OVoid     -> return ()
    ONull     -> w 0
    OTrue     -> w 1
    OInt    o -> w 2 >> put o
    OLong   o -> w 3 >> vlPutInteger o
    OFloat  o -> w 4 >> putFloat64be o
    OChar   o -> w 5 >> vlPutInteger (toInteger $ ord o)
    OString o -> w 6 >> binaryPutText o
    OList   o -> w 7 >> vlPutInteger (toInteger $ size o) >> mapM_ put (elems o)
    OMap    o -> w 8 >> vlPutInteger (toInteger $ M.size o) >> mapM_ put (M.assocs o)
    OTree   o -> w 9 >> encodeTree o
    OType   _ -> w 10
  get = do
    w <- getWord8
    guard $ A.inRange (A.bounds decoderArray) w
    decoderArray A.! w

instance Monoid Simple where
  mempty = ONull
  mappend a b = case a of
    ONull     -> case b of
      ONull     -> ONull
      _         -> OVoid
    OChar   a -> case b of
      ONull     -> OChar a
      OChar   b -> OString $ Strict.pack [a, b]
      OString b -> OString $ Strict.cons a b
      _         -> OVoid
    OString a -> case b of
      ONull     -> OString a
      OChar   b -> OString $ Strict.snoc a b
      OString b -> OString $ a <> b
      _         -> OVoid
    OList   a -> case b of
      OList   b -> OList $ a <> b
      _         -> OVoid
    _         -> OVoid

instance Monoid (Sum Simple) where
  mempty = Sum ONull
  mappend (Sum a) (Sum b) =
    let f a b = firstNonVoid
          [ simpleNumInfixOp (+) a b
          , simpleSetInfixOp (.|.) objectMapUnion a b
          , a <> b
          ]
    in  Sum $ case a of
          ONull  -> case b of { ONull -> ONull ; OTrue -> OTrue; OMap b -> OMap b; _ -> f a b; }
          OTrue  -> case b of { ONull -> OTrue ; OTrue -> OTrue; OMap _ -> OTrue ; _ -> f a b; }
          OMap a -> case b of { ONull -> OMap a; OTrue -> OTrue;                   _ -> f (OMap a) b; }
          _      -> f a b

instance Monoid (Product Simple) where
  mempty = Product OTrue
  mappend (Product a) (Product b) =
    let f a b = firstNonVoid
          [ simpleNumInfixOp (*) a b
          , simpleSetInfixOp (.&.) objectMapIntersection a b
          ]
    in  Product $ case a of
          ONull  -> case b of { OTrue -> ONull; ONull -> ONull ; OMap _ -> ONull ; _ -> f a b; }
          OTrue  -> case b of { OTrue -> OTrue; ONull -> ONull ; OMap b -> OMap b; _ -> f a b; }
          OMap a -> case b of { ONull -> ONull; OTrue -> OMap a;                   _ -> f (OMap a) b; }
          _      -> f a b

pPrintMap :: (o -> [PPrint]) -> M.Map Object o -> [PPrint]
pPrintMap pprin o = 
  [ pChar '{'
  , pIndent $ intercalate [pChar ',', pSpace, pNewLine] $
      fmap (\ (a, b) -> pPrint a ++ [pChar ':', pSpace, pIndent (pprin b)]) (M.assocs o)
  , pChar '}'
  ]

pPrintTree :: T_tree -> [PPrint]
pPrintTree o = concat
  [ maybe [pText "()"] (\o -> [pChar '(', pIndent (pPrint o), pNewLine, pChar ')']) (o~>T.leaf)
  , [pSpace, pText "->", pSpace]
  , pPrintMap pPrintTree (o~>T.branches)
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
  ,(10, (OTree   <$> decodeTree 10) <|> _tree_err)
  ,(11, return $ OType (typeOf OVoid))
  ]

_tree_err :: Get ig
_tree_err = fail "expecting Tree"

decodeArray :: Get (Array Object)
decodeArray = vlGetInteger >>= loop [] where
  loop ox i = if i==0 then return $ array ox else get >>= \o -> loop (ox++[o]) (i-1)

decodeMap :: Get o -> Get (M.Map Object o)
decodeMap get1 = vlGetInteger >>= loop [] where
  loop ox i =
    if i==0
    then return $ M.fromList ox
    else (,) <$> get <*> get1 >>= \o -> loop (ox++[o]) (i-1)

decodeTree :: Word8 -> Get (T.Tree Object Object)
decodeTree w = let f = decodeMap (getWord8 >>= decodeTree) in case w of
  9  -> T.Tree . (,) Nothing <$> f
  10 -> T.Tree <$> ((,) <$> (Just <$> get) <*> f)
  _  -> _tree_err

encodeTree :: T.Tree Object Object -> Put
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

objectMapUnion :: T_map -> T_map -> T_map
objectMapUnion = _combineMaps M.unionWith

objectMapIntersection :: T_map -> T_map -> T_map
objectMapIntersection = _combineMaps M.intersectionWith

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

_combineMaps
  :: ((Object -> Object -> Object) -> T_map -> T_map -> T_map)
  -> T_map -> T_map -> T_map
_combineMaps f a b = M.mapMaybe removeVoids $ f setJoin a b where
  removeVoids o = if testVoid o then Nothing else Just o
  setJoin a b = case a of
    OSimple OVoid     -> case b of
      OSimple OVoid     -> OSimple OVoid
      b                 -> b
    OSimple (OMap  a) -> case b of
      OSimple OVoid     -> OSimple $ OMap a
      OSimple (OMap  b) -> OSimple $ OMap $ _combineMaps f a b
      OSimple (OList b) -> obj $ OSimple (OMap a) : elems b
      _                 -> obj [OSimple $ OMap  a, b]
    OSimple (OList a) -> case b of
      OSimple OVoid     -> OSimple (OList a)
      OSimple (OList b) -> OSimple $ OList $ a <> b
      _                 -> obj [OSimple $ OList a, b]
    _                 -> obj [a, b]

----------------------------------------------------------------------------------------------------

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

instance SimpleData T_list   where
  simple       = OList
  fromSimple o = case o of { OList o -> return o; _ -> mzero; }

instance SimpleData [Object] where
  simple       = OList . array
  fromSimple o = case o of { OList o -> return $ elems o; _ -> mzero; }

instance SimpleData T_map    where
  simple       = OMap
  fromSimple o = case o of { OMap o -> return o; _ -> mzero; }

instance SimpleData T_tree   where
  simple       = OTree
  fromSimple o = case o of { OTree o -> return o; _ -> mzero; }

instance SimpleData T_type   where
  simple       = OType
  fromSimple o = case o of { OType o -> return o; _ -> mzero; }

instance SimpleData Simple   where
  simple       = id
  fromSimple   = return

instance (Eq pat, ObjectData pat) => SimpleData (Satisfy pat) where
  simple (Satisfy (a, b)) = simple $ array [obj a, obj b]
  fromSimple = fmap elems . fromSimple >=> \pair -> case pair of
    [a, b] -> Satisfy <$> ((,) <$> fromObj a <*> fromObj b)
    _      -> mzero

instance (Eq pat, ObjectData pat) => SimpleData (Conjunction pat) where
  simple (Conjunction ox) = simple $ array $ obj "AND" : (obj <$> elems ox)
  fromSimple = fmap elems . fromSimple >=> \list -> case list of
    header:list | header == obj "AND" -> Conjunction . array . nub <$> mapM fromObj list
    _                                 -> mzero

instance (Eq pat, ObjectData pat) => SimpleData (Statement pat) where
  simple (Statement ox) = simple $ array $ obj "OR" : (obj <$> elems ox)
  fromSimple = fmap elems . fromSimple >=> \list -> case list of
    header:list | header == obj "OR" -> Statement . array . nub <$> mapM fromObj list
    _                                -> mzero

----------------------------------------------------------------------------------------------------

-- | An 'Object' is either a 'Simple' data type or a 'Passport' data type. Since 'Passport' lets you
-- store any arbitrary Haskell data type (so long as it instantiates 'Prelude.Eq', 'Prelude.Ord',
-- and 'Data.Typeable.Typeable'), 'Object's provide much more flexibility in the kinds of data that
-- can be used at runtime. The drawback to using 'Object' as opposed to 'Simple' is that 'Object's
-- may not serialize very well, and thus you may have difficulty transmitting 'Object's to other
-- processes.
data Object = OSimple Simple | OForeign Passport deriving (Eq, Ord, Typeable) 

instance HasTypeRep Object where
  objTypeOf o = case o of
    OSimple  o -> objTypeOf o
    OForeign o -> objTypeOf o

instance MaybeVoid Object where
  voidValue                = OSimple OVoid
  testVoid (OSimple OVoid) = True
  testVoid _               = False

instance TestNull Object where
  nullValue                = OSimple ONull
  testNull (OSimple ONull) = True
  testNull _               = False

instance PPrintable Object where
  pPrint o = case o of { OSimple o -> pPrint o; OForeign o -> pPrint o; }

instance Show Object where { show = showPPrint 4 80 . pPrint; }

instance SimpleData Object   where
  fromSimple = return . OSimple
  simple   o = case o of
    OForeign o -> objSimplified o
    OSimple  o -> o

instance Binary Object where
  put o = case o of
    OSimple  o -> put o
    OForeign o -> put $ objSimplified o
  get = OSimple <$> get

instance Monoid Object where
  mempty = OSimple ONull
  mappend a b = case a of
    OSimple a -> case b of
      OSimple b -> OSimple $ a <> b
      _         -> OSimple OVoid
    _         -> OSimple OVoid

instance Monoid (Sum Object) where
  mempty = Sum $ OSimple ONull
  mappend (Sum a) (Sum b) = Sum $ case a of
    OSimple a -> case b of
      OSimple b -> OSimple $ getSum $ Sum a <> Sum b
      _         -> OSimple OVoid
    _         -> OSimple OVoid

instance Monoid (Product Object) where
  mempty = Product $ OSimple ONull
  mappend (Product a) (Product b) = Product $ case a of
    OSimple a -> case b of
      OSimple b -> OSimple $ getProduct $ Product a <> Product b
      _         -> OSimple OVoid
    _         -> OSimple OVoid

instance PatternClass Object where
  patternCompare a b = case a of
    OForeign a' -> maybe (boolSimilar $ a==b) ($ b) (objPatternMatch a')
    a           -> boolSimilar $ a==b

----------------------------------------------------------------------------------------------------

-- | The 'ObjectData' type class is almost identical to the 'SimpleData' type class, you use 'obj'
-- to wrap an arbitrary 'Data.Typeable.Typeable' data type into an 'Object' constructor, and
-- 'fromObj' to unwrap the data type from the 'Object' constructor.
--
-- The custom data must be expressible as a 'Simple' data type, and this is necessary for
-- serialization. When writing an 'Object' as a bit stream, the 'Simple' form of the 'Object' is
-- used, and it is expected that when reading from a bit stream, the 'Simple' form of the 'Object'
-- is just as good as having created the 'Object' in memory in the local program.
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

instance ObjectData Passport  where
  obj       = OForeign
  fromObj o = case o of { OForeign o -> return o; _ -> mzero; }

instance ObjectData Simple   where
  obj       = OSimple
  fromObj o = return $ case o of
    OForeign o -> objSimplified o
    OSimple  o -> o

instance ObjectData ()       where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple ONull -> return (); _ -> mzero; }

instance ObjectData Bool   where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple o -> fromSimple o; _ -> mzero; }

instance ObjectData T_int    where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OInt o) -> return o; _ -> mzero; }

instance ObjectData T_long   where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OLong o) -> return o; _ -> mzero; }

instance ObjectData T_float  where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OFloat o) -> return o; _ -> mzero; }

instance ObjectData T_char   where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OChar o) -> return o; _ -> mzero; }

instance ObjectData T_string where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OString o) -> return o; _ -> mzero; }

instance ObjectData String where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OString o) -> return $ Strict.unpack o; _ -> mzero; }

instance ObjectData T_list   where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OList o) -> return o; _ -> mzero; }

instance ObjectData [Object] where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OList o) -> return $ elems o; _ -> mzero; }

instance ObjectData T_map    where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OMap o) -> return o; _ -> mzero; }

instance ObjectData T_type   where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OType o) -> return o; _ -> mzero; }

instance ObjectData T_tree   where
  obj       = OSimple . simple
  fromObj o = case o of { OSimple (OTree o) -> return o; _ -> mzero; }

instance ObjectData pat => ObjectData (Satisfy pat) where
  obj     = OSimple . simple
  fromObj = fromObj >=> fromSimple

instance ObjectData pat => ObjectData (Conjunction pat) where
  obj     = obj . fromForeign
  fromObj = fromObj >=> fromSimple

instance ObjectData pat => ObjectData (Statement pat) where
  obj     = obj . fromForeign
  fromObj = fromObj >=> fromSimple

