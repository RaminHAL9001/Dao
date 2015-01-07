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
-- 'ObjectData', the 'toForeign' function will be useful.
module Dao.Object
  ( ErrorObject, throwObject,
    -- * Improving on 'Data.Dynamic.Dynamic'
    HasTypeRep(objTypeOf),
    -- * Simple Data Modeling
    Simple(OVoid, ONull), SimpleData(simple, fromSimple), simpleNumInfixOp, simpleSetInfixOp,
    T_int, T_long, T_float, T_string, T_char, T_list, T_map,
    -- * Wrapping 'Data.Dynamic.Dynamic' Data Types
    Foreign, toForeign, fromForeign, objDynamic, objEquality, objOrdering, objPrinting,
    objSimplifying,
    printable, simplifyable, matchable,
    -- * Object: Union of 'Simple' and 'Foreign'
    Object(OSimple, OForeign), ObjectPattern(objMatch), ObjectData(obj, fromObj), defaultFromObj,
    objectMapUnion, objectMapIntersection,
    -- * Working with Void (undefined) Values
    MaybeVoid(voidValue, testVoid), firstNonVoid, allNonVoids
  )
  where

import           Dao.Array
import           Dao.PPrint
import           Dao.TestNull

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except

import           Data.Bits
import           Data.Dynamic
import           Data.List (intercalate)
import qualified Data.Map  as M
import           Data.Monoid
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

-- | This class allows you to define a pattern matching predicate for your data type. The data type
-- must take an 'Object' as input and use data of your custom pattern type to decide whether your
-- pattern matches the 'Object'. You usually make use of 'fromObj' to convert the 'Object' to a type
-- which you can actually evaluate.
--
-- The 'Object' data type itself instantiates this class, in which case if 'matchable' has not been
-- defined for the data type stored in the 'Dao.Object.Object', 'objMatch' evaluates to
-- @('Prelude.==')@
class ObjectPattern o where { objMatch :: o -> Object -> Bool; }

instance ObjectPattern Object where
  objMatch a b = case a of { OForeign a' -> maybe (a==b) ($ b) (objPatternMatch a'); a -> a==b; }

----------------------------------------------------------------------------------------------------

-- | This data type wraps an object in a 'Data.Dynamic.Dynamic' data type along with it's
-- 'Prelude.Eq' and 'Prelude.Ord' instance functions. This allows 'Foreign' to itself be
-- instantiated into 'Prelude.==' and 'Prelude.compare'. To create a foreign data type, use the
-- 'toForeign' function.
--
-- If your data type instantiates 'Dao.PPrint.PPrintable', you can use 'printable' to store the
-- 'Dao.PPrint.pPrint' instance with this 'Foreign' data constructor.
--
-- If your data type instantiates 'SimpleData', you can use 'simplifyable' to store the 'simple'
-- instance with this 'Foreign' data constructor.
data Foreign
  = Foreign
    { objDynamic      :: Dynamic
    , objEquality     :: Dynamic -> Bool
    , objOrdering     :: Dynamic -> Ordering
    , objPrinting     :: [PPrint]
    , objSimplifying  :: Simple
    , objPatternMatch :: Maybe (Object -> Bool)
    }
  deriving Typeable

instance Eq  Foreign where { a == b = objEquality a $ objDynamic b; }

instance Ord Foreign where { compare a b = objOrdering a $ objDynamic b; }

instance PPrintable Foreign where { pPrint o = objPrinting o; }

instance Show Foreign where { show = Lazy.unpack . runTextPPrinter 4 80 . pPrint; }

instance HasTypeRep Foreign where { objTypeOf = objTypeOf . objDynamic }

-- | Construct an arbitrary 'Data.Typeable.Typeable' data type into a 'Foreign' data type along with
-- the 'Prelude.==' and 'Prelude.compare' instances for this data type. Use this function with
-- 'printable' and 'simplifyable' to extend this foreign data type with instances for
-- 'Dao.PPrintable' and 'SimpleData'.
--
-- To define an instance of 'obj' for you own custom data type, use this function along with 'obj',
-- as the 'obj' function can convert a 'Foreign' data type to an 'Object'. Likewise, to define an
-- instance of 'fromObj' for you own custom data type, use 'fromForeign' with 'fromObj', as the
-- 'fromObj' function can convert a 'Foreign' data type to an 'Object' data type.
toForeign :: (Eq o, Ord o, Typeable o) => o -> Foreign
toForeign o = let d = toDyn o in
  Foreign
  { objDynamic  = d
  , objEquality = maybe False (o ==) . fromDynamic
  , objOrdering = \p -> maybe (compare (dynTypeRep d) (dynTypeRep p)) (compare o) (fromDynamic p)
  , objPrinting = []
  , objSimplifying = OVoid
  , objPatternMatch = Nothing
  }

-- | To define an instance of 'obj' for you own custom data type, use this function along with
-- 'obj', as the 'obj' function can convert a 'Foreign' data type to an 'Object'. Likewise, to
-- define an instance of 'fromObj' for you own custom data type, use 'fromForeign' with 'fromObj',
-- as the 'fromObj' function can convert a 'Foreign' data type to an 'Object' data type.
fromForeign
  :: (Eq o, Ord o, Typeable o,
      Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m)
  => Foreign -> m o
fromForeign = maybe mzero return . fromDynamic . objDynamic

-- | *'Foriegn' data, optional property.* If your data type instantiates 'Dao.PPrint.PPrintable',
-- you can use 'printable' to store the 'Dao.PPrint.pPrint' instance with this 'Foreign' data
-- constructor.
printable :: PPrintable o => o -> Foreign -> Foreign
printable o dat = dat{ objPrinting=pPrint o }

-- | *'Foriegn' data, optional property.* If your data type instantiates 'SimpleData', you can use
-- 'simplifyable' to store the 'simple' instance with this 'Foreign' data constructor.
simplifyable :: SimpleData o => o -> Foreign -> Foreign
simplifyable o dat = dat{ objSimplifying=simple o }

-- | *'Foriegn' data, optional property.* If your data type can act as a pattern that can match
-- arbitrary objects using 'objMatch', consider modifying your instance of 'obj' to make use of this
-- function. The 'matchable' function lets you define a predicate that can match an arbitrary
-- 'Object'. Specifying 'matchable' will only make use of 'objMatch' for pattern matching, it will
-- not be used for equality testing ('Prelude.==').
matchable :: ObjectPattern o => o -> Foreign -> Foreign
matchable o dat = dat{ objPatternMatch=Just (objMatch o) }

----------------------------------------------------------------------------------------------------

-- | An 'ErrorObject' is itself just an 'Object', but usually it is good to make it something that
-- is descriptive of the problem that occurred.
type ErrorObject = [Object]

throwObject :: MonadError ErrorObject m => ObjectData o => o -> m err
throwObject = throwError . return . obj

type T_int    = Int
type T_long   = Integer
type T_float  = Double
type T_string = Strict.Text
type T_char   = Char
type T_list   = Array Object
type T_map    = M.Map Object Object
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
      [ pText "["
      , pIndent $ intercalate [pText ",", pSpace, pNewLine] $ fmap pPrint $ elems o
      , pText "]"
      ]
    OMap    o ->
      [ pText "{"
      , pIndent $ intercalate [pText ",", pSpace, pNewLine] $
          fmap (\ (a, b) -> pPrint a ++ [pText ":", pSpace, pIndent (pPrint b)]) (M.assocs o)
      , pText "}"
      ]
    OType   o -> [pShow o]

instance Show Simple where { show = Lazy.unpack . runTextPPrinter 4 80 . pPrint; }

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
    let f a b = firstNonVoid $
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
    let f a b = firstNonVoid $
          [ simpleNumInfixOp (*) a b
          , simpleSetInfixOp (.&.) objectMapIntersection a b
          ]
    in  Product $ case a of
          ONull  -> case b of { OTrue -> ONull; ONull -> ONull ; OMap _ -> ONull ; _ -> f a b; }
          OTrue  -> case b of { OTrue -> OTrue; ONull -> ONull ; OMap b -> OMap b; _ -> f a b; }
          OMap a -> case b of { ONull -> ONull; OTrue -> OMap a;                   _ -> f (OMap a) b; }
          _      -> f a b

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
    OFloat  b -> OFloat $ num a $ b
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

instance SimpleData T_type   where
  simple       = OType
  fromSimple o = case o of { OType o -> return o; _ -> mzero; }

----------------------------------------------------------------------------------------------------

-- | An 'Object' is either a 'Simple' data type or a 'Foreign' data type. Since 'Foreign' lets you
-- store any arbitrary Haskell data type (so long as it instantiates 'Prelude.Eq', 'Prelude.Ord',
-- and 'Data.Typeable.Typeable'), 'Object's provide much more flexibility in the kinds of data that
-- can be used at runtime. The drawback to using 'Object' as opposed to 'Simple' is that 'Object's
-- may not serialize very well, and thus you may have difficulty transmitting 'Object's to other
-- processes.
data Object = OSimple Simple | OForeign Foreign deriving (Eq, Ord, Typeable) 

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

instance Show Object where { show = Lazy.unpack . runTextPPrinter 4 80 . pPrint; }

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

----------------------------------------------------------------------------------------------------

-- | The 'ObjectData' type class is almost identical to the 'SimpleData' type class, you use 'obj'
-- to wrap an arbitrary 'Data.Typeable.Typeable' data type into an 'Object' constructor, and
-- 'fromObj' to unwrap the data type from the 'Object' constructor.
class (Eq o, Ord o, Typeable o) => ObjectData o where
  obj :: o -> Object
  fromObj
    :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m)
    => Object -> m o

-- | This is the function you should use to instantiate the 'fromObj' function for most any custom
-- data type.
defaultFromObj
  :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m, MonadError ErrorObject m,
      Eq o, Ord o, Typeable o)
  => Object -> m o
defaultFromObj = fromObj >=> fromForeign

instance ObjectData Object   where { obj = id; fromObj = return; }

instance ObjectData Foreign  where
  obj       = OForeign
  fromObj o = case o of { OForeign o -> return o; _ -> mzero; }

instance ObjectData Simple   where
  obj       = OSimple
  fromObj o = case o of { OSimple o -> return o; _ -> mzero; }

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

