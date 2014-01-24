-- "src/Dao/Object.hs"  declares the "Object" data type which is the
-- fundamental data type used througout the Dao System.
-- 
-- Copyright (C) 2008-2014  Ramin Honary.
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

module Dao.Object
  ( module Dao.String
  , module Dao.Object
  ) where

import           Dao.String
import           Dao.Predicate
import qualified Dao.EnumSet   as Es
import qualified Dao.Binary    as B
import           Dao.PPrint
import           Dao.Random

import           Data.Typeable
import           Data.Monoid
import           Data.List
import qualified Data.Complex  as C
import qualified Data.Map      as M
import           Data.Ix
import           Data.Char
import           Data.Word
import           Data.Bits
import           Data.Array.IArray
import           Data.Time hiding (parseTime)

import qualified Data.ByteString.Lazy as B

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State

----------------------------------------------------------------------------------------------------

-- | This is the data type used as the intermediary between Haskell objects and Dao objects. If you
-- would like your Haskell data type to be used as a non-opaque data type in a Dao language script,
-- the first step is to instantiate your data type into this class. The next step would be to
-- instantiate your object into the 'Dao.Evaluator.HaskellDataClass' class. Instantiating the
-- 'Dao.Evaluator.HaskellDataClass' class alone will make your object usable in Dao language scripts, but
-- it will be an opaque type. Instantiating 'Struct' and declaring 'autoDefStruct' in the
-- 'defObjectInterface' will allow functions in the Dao language script to read and write
-- information to your data structure, modifying it during runtime.
-- 
-- 'Struct' values are used lazily, so your data types will only be converted to and from 'Struct's
-- when absolutely necessary. This helps to conserver memory usage.
data Struct value
  = Nullary{ structName :: Name }
    -- ^ models a constructor with no fields, for example 'Prelude.EQ', 'Prelude.GT' and
    -- 'Prelude.LT'.
  | Struct
    { structName :: Name -- ^ provide the name for this constructor.
    , fieldMap   :: M.Map Name value
    }
  deriving (Eq, Ord, Show, Typeable)
instance Functor Struct where
  fmap f st = case st of
    Nullary{ structName=name } -> Nullary{ structName=name }
    Struct{ fieldMap=value   } -> st{ fieldMap = fmap f value }

instance NFData value => NFData (Struct value) where
  rnf (Nullary a  ) = deepseq a ()
  rnf (Struct  a b) = deepseq a $! deepseq b ()

instance HasNullValue value => HasNullValue (Struct value) where
  nullValue = Nullary{ structName=ustr "NULL" }
  testNull (Nullary{ structName=name }) = name == ustr "NULL"
  testNull _ = False

instance B.Binary o mtab => B.Binary (Struct (Value o)) mtab where
  put o = case o of
    Nullary  o -> B.putWord8 0x20 >> B.put o
    Struct n o -> B.putWord8 0x21 >> B.put n >> B.put o
  get = B.word8PrefixTable <|> fail "expecting Struct"

instance B.Binary o mtab => B.HasPrefixTable (Struct (Value o)) B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "QualRef" 0x20 0x21 $
    [ Nullary <$> B.get
    , pure Struct <*> B.get <*> B.get
    ]

instance PPrintable value => PPrintable (Struct value) where
  pPrint o = case o of
    Nullary{ structName=name } -> pString ('#' : uchars (toUStr name))
    Struct{ structName=name, fieldMap=dict } ->
      pList (pString ('#' : uchars (toUStr name))) "{" ", " "}" $
        flip map (M.assocs dict) $ \ (left, right) -> pInline $
          [pPrint left, pString " = ", pPrint right]

instance HasRandGen value => HasRandGen (Struct value) where
  randO = countRunRandChoice 
  randChoice = randChoiceList $
    [ pure Struct <*> randO <*> (M.fromList <$> randListOf 1 4 (pure (,) <*> randO <*> randO))
    , Nullary <$> randO
    ]

-- | You can make your data type readable but not writable in the Dao runtime. That means a Dao
-- script can inspect elements of your data type, but not modify them As an example lets say you
-- have a 3D-point data type you would like to use in your Dao script.
-- > data Point =
-- >     Point2D{ get_x::'T_float', get_y::'T_float' }
-- >   | Point3D{ get_x::'T_float', get_y::'T_float', get_z::'T_float' }
-- 
-- Lets say you have already instantiated the 'HaskellDataClass' class and provided the Dao runtime with
-- a 'Dao.Evaluator.DaoFunc' (via 'Dao.Evaluator.setupDao') that constructs a Point3D at runtime:
-- > p = Point3D(1.9, 4.4, -2.1);
-- Now you would like to extend the 'HaskellDataClass' of your Point3D to also be readable at runtime.
-- If you instantiate 'ToDaoStructClass' your Dao language script could also read elements from the
-- point like so:
-- > distFromOrigin = sqrt(p.x*p.x + p.y*p.y + p.z*p.z);
-- However you cannot modify the point unless you also instantiate 'FromDaoStructClass'. So a statement
-- like this would result in an error:
-- > p.x /= distFromOrigin;
-- > p.y /= distFromOrigin;
-- > p.z /= distFromOrigin;
-- 
-- You can convert this to a 'Struct' type using the 'fromData' function. There are many ways to
-- define fields in a 'Struct', here are a few:
-- > instance 'ToDaoStructClass' Point3D 'Dao.Evaluator.Object' where
-- >     'toDaoStruct' = 'fromData' "@Point2D@" $ do
-- >         'Dao.Evaluator.putPrimField' "x" get_x
-- >         'Dao.Evaluator.putPrimField' "y" get_y
-- >          obj <- 'Control.Monad.Reader.Class.ask'
-- >          case obj of
-- >             Point3D _ _ z -> do
-- >                 'renameConstructor' "@Point3D@"
-- >                 'define' "z" ('Dao.Evaluator.obj' z)
-- >             _             -> return ()
-- 
-- Finally, you should define the instantiation of Point3D into the 'HaskellDataClass' class so it
-- includes the directive 'Dao.Evaluator.autoDefToStruct'.
class ToDaoStructClass haskData value where
  toDaoStruct :: ToDaoStruct value haskData ()

-- | Continuing the example from above, if you do want your data type to be modifyable by functions
-- running in the Dao language runtime, you must instantiate this class, which is facilitated by the
-- 'toData' function.
-- > instance 'FromDaoStructClass' 'Point3D' 'Dao.Evaluator.Object' where
-- >     fromDaoStruct = 'toData' $ 'Control.Monad.msum' $
-- >         [ do 'constructor' "@Point2D@"
-- >              'Control.Applicative.pure' Point3D <*> 'Dao.Evaluator.?' "x" <*> 'Dao.Evaluator.?' "y"
-- >         , do 'constructor' "@Point3D@"
-- >              'Control.Applicative.pure' Point3D <*> 'Dao.Evaluator.?' "x" <*> 'Dao.Evaluator.?' "y" <*> 'Dao.Evaluator.@' "z"
-- >         ]
-- 
-- Do not forget to define the instantiation of Point3D into the 'HaskellDataClass' class so it
-- includes the directive 'Dao.Evaluator.autoDefFromStruct'.
-- 
-- Note that an instance of 'FromDaoStructClass' must also instantiate 'ToDaoStructClass'. I can see no
-- use for objects that are only writable, that is they can be created at runtime but never
-- inspected at runtime.
class ToDaoStructClass haskData value => FromDaoStructClass haskData value where
  fromDaoStruct :: FromDaoStruct value haskData

-- | If there is ever an error converting to or from your Haskell data type, you can
-- 'Control.Monad.Error.throwError' a 'StructError'.
data StructError value
  = StructError
    { structErrMsg    :: Maybe UStr
    , structErrName   :: Maybe UStr
    , structErrField  :: Maybe UStr
    , structErrValue  :: Maybe value
    , structErrExtras :: [Name]
    }
  deriving (Eq, Ord, Typeable)

instance HasNullValue (StructError value) where
  nullValue =
    StructError
    { structErrMsg=Nothing
    , structErrName=Nothing
    , structErrField=Nothing
    , structErrValue=Nothing
    , structErrExtras=[]
    }
  testNull
    ( StructError
      { structErrMsg=Nothing
      , structErrName=Nothing
      , structErrField=Nothing
      , structErrValue=Nothing
      , structErrExtras=[]
      }
    ) = True
  testNull _ = False

-- | Used to convert a 'Prelude.String' to a 'Dao.String.Name' by functions like 'define' and
-- 'setField'. Usually you will not need to use it.
mkLabel :: (UStrType name, MonadPlus m) => name -> m Name
mkLabel name = maybe mzero return $ maybeFromUStr (toUStr name)

mkStructName :: (UStrType name, MonadPlus m) => name -> m Name
mkStructName name = mplus (mkLabel name) $ fail "invalid constructor name"

mkFieldName :: (UStrType name, MonadPlus m) => name -> m Name
mkFieldName name = mplus (mkLabel name) $ fail "invalid field name"

-- | This is a handy monadic and 'Data.Functor.Applicative' interface for instantiating
-- 'toDaoStruct' in the 'ToDaoStructClass' class.
newtype ToDaoStruct value haskData a
  = ToDaoStruct
    { run_toDaoStruct :: PredicateT (StructError value) (State (Struct value, haskData)) a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad (ToDaoStruct value haskData) where
  return = ToDaoStruct . return
  m >>= f = ToDaoStruct $ run_toDaoStruct m >>= run_toDaoStruct . f
  fail msg = throwError $ nullValue{ structErrMsg = Just $ ustr msg }

instance MonadState (Struct value) (ToDaoStruct value haskData) where
  state f = ToDaoStruct $ lift $ state $ \ (struct, haskData) ->
    let (a, struct') = f struct in (a, (struct', haskData))

instance MonadReader haskData (ToDaoStruct value haskData) where
  ask = ToDaoStruct $ lift $ fmap snd get
  local upd f = ToDaoStruct $ PredicateT $ do
    haskData <- gets snd
    modify (\ (struct, _) -> (struct, upd haskData))
    a <- runPredicateT $ run_toDaoStruct f
    modify (\ (struct, _) -> (struct, haskData))
    return a

instance MonadError (StructError value) (ToDaoStruct value haskData) where
  throwError err = get >>= \struct -> ToDaoStruct $ throwError $
    err{ structErrName = structErrName err <|> (Just $ toUStr $ structName struct) }
  catchError f catch = ToDaoStruct $ catchError (run_toDaoStruct f) (run_toDaoStruct . catch)

instance MonadPlusError (StructError value) (ToDaoStruct value haskData) where
  catchPredicate = ToDaoStruct . catchPredicate . run_toDaoStruct
  predicate      = ToDaoStruct . predicate

-- | This function is typically used to evaluate the instantiation of 'toDaoStruct'. It takes two
-- parameters: first a computation to convert your data type to the 'Struct' using the 'ToDaoStruct'
-- monad, and second the data type you want to convert. You can use functions like 'defineWith' and
-- 'setField' to build your 'ToDaoStruct' computation. For example, lets say you have a Haskell data
-- type called @mydat::MyData@ where @MyData@ instantiates 'ToDaoStruct', you can convert it to a
-- Dao 'Struct' like so:
-- > 'fromData' 'toDaoStruct' mydat
-- Notice how it reads similar to ordinary English, "convert from (Haskell) data to a Dao 'Struct'"
fromData
  :: ToDaoStruct value haskData x
  -> haskData
  -> Predicate (StructError value) (Struct value)
fromData pred hask = evalState (runPredicateT $ run_toDaoStruct $ pred >> get) $
  (Struct{ structName=nil, fieldMap=M.empty }, hask)

-- | Overwrite the current 'Struct' with a 'Struct' produced by a 'toDaoStruct' instance of a
-- different type. This is useful when instantiating a newtype or a data type constructor that
-- contains only one item (the "inner" item), and the data type of the inner item instantiates
-- 'ToDaoStructClass', you can simply use the instance of 'toDaoStruct' for that data type to
-- instantiate 'toDaoStruct' for the outer data type. Just be sure that the constructor name for the
-- inner type does not conflict with the constructor name for the outer data type. For example:
-- > data X = X1 { ... } | X2 { ... }
-- > instance 'DaoToStructClass' X ('Value' any) where { ... }
-- > data Y = Y1 { ... } | Y2 { ... }
-- > instance 'DaoToStructClass' Y ('Value' any) where { ... }
-- > 
-- > newtype WrapX = WrapX { unwrapX :: X }
-- > instance 'DaoToStructClass' WrapX ('Value' any) where
-- >     'toDaoStruct' = 'Control.Monad.Reader.ask' >>= 'innerToStruct' . unwrapX
-- > 
-- > data X_or_Y = Is_X { getX :: X } | Is_Y { getY :: Y }
-- > instance 'DaoToStructClass' X_or_Y ('Value' any) where
-- >     'toDaoStruct' = 'Control.Monad.Reader.ask' >>= \xy -> case xy of
-- >         Is_X x -> 'innerToStruct' x
-- >         Is_Y y -> 'innerToStruct' y
-- 
-- The inverse of this operation in the 'FromDaoStructClass' is 'Prelude.fmap', or equivalently the
-- 'Control.Applicative.<$>' operator. Here is an example using 'Control.Applicative.<$>':
-- > instance 'FromDaoStructClass' WrapX ('Value' any) where
-- >     'fromDaoStruct' = WrapX <$> 'fromDaoStruct'
-- > 
-- > instance 'FromDaoStructClass' X_or_Y ('Value' any) where
-- >     'fromDaoStruct' = Is_X <$> 'fromDaoStruct' <|> Is_Y <$> 'fromDaoStruct'
-- 
-- Another way to do exactly the same thing as the example above is:
-- > instance 'FromDaoStructClass' WrapX ('Value' any) where
-- >     'fromDaoStruct' = 'Prelude.fmap' WrapX 'fromDaoStruct'
-- > 
-- > instance 'FromDaoStructClass' X_or_Y ('Value' any) where
-- >     'fromDaoStruct' = 'Prelude.fmap' Is_X 'fromDaoStruct' `'Control.Monad.mplus'` 'Prelude.fmap' Is_Y 'fromDaoStruct'
-- 
-- It is possible to use 'renameConstructor' after evaluating 'innerToStruct' to use a different
-- constructor name while keeping all of the fields set by the evaluation of 'innerToStruct',
-- however if this is done, 'Prelude.fmap' will backtrack, so you should use 'innerFromStruct'
-- instead.
innerToStruct :: ToDaoStructClass inner value => inner -> ToDaoStruct value haskData ()
innerToStruct o = ask >>= \haskData ->
  predicate (fromData toDaoStruct o) >>= ToDaoStruct . lift . put . flip (,) haskData

-- | Use this function to set the 'structName' name of the constructor at some point, for example
-- when you observe some condition of the @haskData@ type that merits an alternative constructor
-- name.
renameConstructor :: UStrType name => name -> ToDaoStruct value haskData ()
renameConstructor name = mkStructName name >>= \name -> modify $ \struct -> struct{ structName=name }

-- | Like 'renameConstructor' but deletes everything and makes the 'Struct' being constructed into a
-- 'Nullary'. You would typically do this only when you are instantiating 'toDaoStruct' and you
-- only have one constructor to define.
makeNullary :: UStrType name => name -> ToDaoStruct value haskData ()
makeNullary name = mkStructName name >>= \name -> put $ Nullary{ structName=name }

-- | Use this when you have derived the "Prelude.Show" class for a data type where every constructor
-- in that data type takes no parameters, for example, the 'Prelude.Ordering' data type.
putNullaryUsingShow :: Show haskData => ToDaoStruct value haskData ()
putNullaryUsingShow = ask >>= makeNullary . show

define :: UStrType name => name -> value -> ToDaoStruct value haskData value
define name value = do
  name <- mkFieldName name
  modify $ \struct -> struct{ fieldMap = M.insert name value (fieldMap struct) }
  return value

-- | Defines an optional field. If the value given is 'Prelude.Nothing', nothing happens. Otherwise
-- the value is placed into the 'Struct' at the given @name@d field. This is the inverse opreation
-- of using 'Control.Applicative.optional' in the 'FromDaoStruct' monad.
optionalField :: UStrType name => name -> Maybe value -> ToDaoStruct value haskData (Maybe value)
optionalField name = maybe (return Nothing) (fmap Just . define name)

setField :: UStrType name => name -> (haskData -> value) -> ToDaoStruct value haskData value
setField name f = ask >>= define name . f

-- | This is a handy monadic and 'Data.Functor.Applicative' interface for instantiating
-- 'fromDaoStruct' in the 'FromDaoStructClass' class. It takes the form of a reader because what you
-- /read/ from the 'Struct' here in the Haskell language was /written/ by the Dao language
-- runtime. Think of it as "this is the data type used when the Dao runtime wants to write
-- information to my data structure."
-- 
-- Because Dao is such a messy, fuzzy, not statically typed, interpreted language, the information
-- coming in from the Dao runtime requires a lot of sanitization. Therefore this monad provides
-- several functions for checking the type of information you are using to build your Haskell data
-- type.
--
-- Be sure to make ample use of the 'Control.Monad.guard', 'Control.Monad.Error.throwError', and
-- 'Control.Monad.fail' functions.
-- 
-- /NOTE:/ refer to the documentation of the 'constructor' monad for an important note on reading
-- Haskell data types with multiple constructors.
newtype FromDaoStruct value a =
  FromDaoStruct{ run_fromDaoStruct :: PredicateT (StructError value) (State (Struct value)) a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad (FromDaoStruct value) where
  return = FromDaoStruct . return
  m >>= f = FromDaoStruct $ run_fromDaoStruct m >>= run_fromDaoStruct . f
  fail msg = throwError $ nullValue{ structErrMsg = Just (ustr msg) }

instance MonadReader (Struct value) (FromDaoStruct value) where
  ask = FromDaoStruct $ lift get
  local upd f = FromDaoStruct $ PredicateT $ get >>= \st ->
   return $ evalState (runPredicateT $ run_fromDaoStruct f) (upd st)

instance MonadError (StructError value) (FromDaoStruct value) where
  throwError err = ask >>= \struct -> FromDaoStruct $ throwError $
    err { structErrName = structErrName err <|> (Just $ toUStr $ structName struct) }
  catchError (FromDaoStruct f) catch = FromDaoStruct $ catchError f (run_fromDaoStruct . catch)

instance MonadPlusError (StructError value) (FromDaoStruct value) where
  catchPredicate = FromDaoStruct . catchPredicate . run_fromDaoStruct
  predicate = FromDaoStruct . predicate

-- | This function is typically used to evaluate the instantiation of 'fromDaoStruct'. It takes two
-- parameters: first a computation to convert your data type to the Haskell data type from a
-- 'Struct' using the 'FromDaoStruct' monad, and second the 'Struct' you want to convert. For
-- example, if you have a Haskell data type 'MyData' which instantiates 'FromDaoStruct', you could
-- construct it from a properly formatted Dao 'Struct' using this statement:
-- > 'toData' 'fromDaoStruct' struct
-- Notice that this reads similar to ordinary English: "convert to (Haskell) data from a dao
-- struct."
toData :: FromDaoStruct value haskData -> Struct value -> Predicate (StructError value) haskData
toData (FromDaoStruct f) = evalState (runPredicateT f)

-- | Checks if the 'structName' is equal to the given name, and if not then backtracks. This is
-- important when constructing Haskell data types with multiple constructors.
--
-- A haskell data type with multiple constructors should be constructed with the
-- 'Control.Monad.msum' function like so:
-- > data MyData = A | B Int | C Int Int
-- > instance 'FromDaoStruct' ('Dao.Evaluator.Object) where
-- >     'fromDaoStruct' = 'toData' $ 'Control.Monad.msum' $
-- >         [ 'constructor' "A" >> return a,
-- >           do 'constructor' "B"
-- >              B 'Control.Applicative.<$>' ('field' "b1" >>= 'Dao.Evaluator.primType')
-- >           do 'constructor' "C"
-- >              'Control.Applicative.pure' C
-- >                  'Control.Applicative.<*>' 'Dao.Evaluator.required' ('field' "c1" >>= 'Dao.Evaluator.primType')
-- >                  'Control.Applicative.<*>' 'Dao.Evaluator.required' ('field' "c2" >>= 'Dao.Evaluator.primType')
-- >         ]
-- /NOTE/ that if all three 'constructor's backtrack (evaluate to 'Control.Monad.mzero') the whole
-- monad will backtrack. By convention, you should let the monad backtrack, rather than writing a
-- 'Control.Monad.fail' statement as the final item in the 'Control.Monad.msum' list.
constructor :: UStrType name => name -> FromDaoStruct value ()
constructor name = (pure (==) <*> mkStructName name <*> asks structName) >>= guard

-- | The inverse operation of 'innerToStruct', but looks for a constructor of a different name. This
-- is important because every 'toDaoStruct' should set it's own unique constructor name, and if you
-- set a different constructor name while using the same 'fromDaoStruct' function to read the fields
-- of the struct, the 'fromDaoStruct' function will backtrack seeing the wrong constructor name.
-- If you have not renamed the constructor with 'renameConstructor' after using 'innerToStruct', do
-- not use this function, simply use 'Prelude.fmap' or the 'Control.Applicative.<$>' operator
-- instead.
-- 
-- This function temporarily changes the constructor name to the constructor set by the @inner@
-- type, that way the 'fromDaoStruct' instance of the @inner@ type will be fooled and read the
-- 'Struct' fields without backtracking. For example:
-- > newtype X = X{ getX :: Int }
-- > instance 'ToDataStruct' X where
-- >     'toDaoStruct' = do
-- >         'renameConstructor' "X"
-- >         "getX" 'Dao.Object..=@' getX
-- > 
-- > newtype Y = Y{ innerX :: X }
-- > instance 'ToDataStruct' Y where
-- >     'toDaoStruct' = do
-- >         -- the 'innerToStruct' function will use the 'toDaoStruct' for X
-- >         'Control.Monad.Reader.ask' >>= 'innerToStruct' . innerX
-- >         -- then rename the constructor from "X" to "Y"
-- >         'renameConstructor' "Y"
-- > 
-- Now when we want to define the accompanying 'FromDaoStructClass', we need to remember that we
-- used 'innerToStruct' and changed the 'structName' from "X" to "Y". Simply using 'Prelude.fmap'
-- (or equivalently 'Control.Applicative.<$>') will not work because the instance of 'fromDaoStruct'
-- for the @X@ data type will backtrack when it sees the 'structName' is "Y".
-- > instance 'FromDaoStructClass' Y where
-- >     'fromDaoStruct' = Y 'Control.Applicative.<$>' 'fromDaoStruct' -- /WRONG!/ This will always backtrack.
-- 
-- The correct way to do it is to use 'innerFromStruct' like so:
-- > instance 'FromDaoStructClass' Y where
-- >     'fromDaoStruct' = Y 'Control.Applicative.<$> 'innerFromStruct' "X" -- CORRECT!
-- 
innerFromStruct :: (UStrType name, FromDaoStructClass inner value) => name -> FromDaoStruct value inner
innerFromStruct tempName = do
  name     <- asks structName
  tempName <- mkStructName tempName
  let setname name = FromDaoStruct $ lift $ modify $ \struct -> struct{ structName=name }
  o <- setname tempName >> mplus fromDaoStruct (setname name >> mzero)
  setname name >> return o

-- | Succeeds if the current 'Struct' is a 'Nullary' where the 'structName' is equal to the name
-- given to this function.
nullary :: UStrType name => name -> FromDaoStruct value ()
nullary name = ask >>= \struct -> case struct of
  Nullary{} -> constructor name
  _         -> mzero

-- | Use the instantiation of 'Prelude.Read' derived for a type @haskData@ to construct the
-- @haskData from the 'structName' stored in a 'Nullary' 'Struct'.
getNullaryWithRead :: Read haskData => FromDaoStruct value haskData
getNullaryWithRead = ask >>= \struct -> case struct of
  Nullary{ structName=name } -> case readsPrec 0 (uchars name) of
    [(haskData, "")] -> return haskData
    _ -> mzero
  _ -> mzero

-- | If an error is thrown using 'Control.Monad.Error.throwError' or 'Control.Monad.fail' within the
-- given 'FromDaoStruct' function, the 'structErrField' will automatically be set to the provided
-- 'Name' value.
structCurrentField :: Name -> FromDaoStruct value o -> FromDaoStruct value o
structCurrentField name (FromDaoStruct f) = FromDaoStruct $ catchPredicate f >>= \o -> case o of
  PFail err -> throwError $ err{ structErrField=Just (toUStr name) }
  Backtrack -> mzero
  OK      o -> return o

-- | Retrieves an arbitrary @value@ by it's field name, and backtraks if no such field is defined.
-- The value of the field is copied, and can be copied again after this operation. It is best not to
-- use this function, rather use 'tryField' to make sure each field is retrieved exactly once, then
-- use 'checkEmpty' to make sure there is no hidden extraneous data in the struct.
tryCopyField :: UStrType name => name -> (value -> FromDaoStruct value o) -> FromDaoStruct value o
tryCopyField name f = (pure M.lookup <*> mkFieldName name <*> asks fieldMap) >>=
  maybe mzero return >>= structCurrentField (fromUStr $ toUStr name) . f

-- | Like 'copyField', retrieves an arbitrary @value@ by it's field name, and backtraks if no such
-- field is defined. However unlike 'tryCopyField', if the item is retrieved, it is deleted from the
-- inner 'Struct' so that it may not be used again. The reason for this is to use 'checkEmpty' and
-- 'requireEmpty', which can backtrack or fail if there are extraneous fields in the structure.
tryField :: UStrType name => name -> (value -> FromDaoStruct value o) -> FromDaoStruct value o
tryField name f = do
  name <- mkFieldName name
  o    <- tryCopyField name f
  FromDaoStruct $ lift $ modify $ \st ->
    case st of{ Struct{ fieldMap=m } -> st{ fieldMap=M.delete name m }; s -> s; }
  return o

_throwMissingFieldError :: Name -> FromDaoStruct value o
_throwMissingFieldError name = throwError $
  nullValue
  { structErrMsg   = Just $ ustr "missing required field"
  , structErrField = Just $ toUStr name
  }

-- | Like 'field' but evaluates 'Control.Monad.Error.throwError' if the 'FromDaoStruct' function
-- backtracks or throws it's own error. Internally, this function makes use of 'copyField' and /not/
-- 'tryField', so the field is preserved if it exists.
copyField :: forall name value o . UStrType name => name -> (value -> FromDaoStruct value o) -> FromDaoStruct value o
copyField name f = mkFieldName name >>= \name ->
  mplus (tryCopyField name f) (_throwMissingFieldError name)

-- | Like 'field' but evaluates 'Control.Monad.Error.throwError' if the 'FromDaoStruct' function
-- backtracks or throws it's own error. Internally, this function makes use of 'tryField' and /not/
-- 'tryCopyField', so the field is removed if it exists -- two consecutive calls to this function
-- with the same key absolutely will fail.
field :: UStrType name => name -> (value -> FromDaoStruct value o) -> FromDaoStruct value o
field name f = mkFieldName name >>= \name -> mplus (tryField name f) (_throwMissingFieldError name)

-- | As you make calls to 'field' and 'tryField', the items in these fields in the 'Struct' are
-- being removed. Once you have all of the nata neccessary to construct the data @value@, you can
-- check to make sure there are no extraneous unused data fields. If the 'Struct' is empty, this
-- function evaluates to @return ()@. If there are extranous fields in the 'Struct', 'throwError' is
-- evaluated.
checkEmpty :: FromDaoStruct value ()
checkEmpty = FromDaoStruct (lift get) >>= \st -> case st of
  Struct{ fieldMap=m } -> if M.null m then return () else throwError $
    nullValue
    { structErrMsg    = Just $ ustr "extraneous data fields"
    , structErrExtras = M.keys m
    }
  Nullary{} -> return ()

instance ToDaoStructClass (StructError (Value any)) (Value any) where
  toDaoStruct = do
    asks structErrMsg    >>= optionalField "message" . fmap OString
    asks structErrName   >>= optionalField "structName" . fmap OString
    asks structErrField  >>= optionalField "field" . fmap OString
    asks structErrValue  >>= optionalField "value"
    asks structErrExtras >>= optionalField "extras" .
      Just . OList . fmap (ORef . Unqualified . Reference . (:[]))
    return ()

instance FromDaoStructClass (StructError (Value any)) (Value any) where
  fromDaoStruct = do
    constructor "StructError"
    let str o = case o of
          OString o -> return o
          _         -> fail "expecting string value"
    let ref o = case o of
          ORef    o -> case o of
            Unqualified (Reference [o]) -> return o
            _ -> fail "not an unqualified reference singleton"
          _ -> fail "not a reference type"
    let lst o = case o of
          OList   o -> forM o ref
          _         -> fail "expecting list value"
    pure StructError
      <*> optional (tryField "message" $ str)
      <*> optional (tryField "structName" $ str)
      <*> optional (tryField "field" $ str)
      <*> optional (tryField "value" return)
      <*> (tryField "extras" $ lst)

----------------------------------------------------------------------------------------------------

-- | The 'Object' type extends the 'Data.Dynamic.Dynamic' data type with a few more constructors for
-- data types that are fundamental to a programming language, like integers, strings, and lists.
data Value o
  = ONull
  | OTrue
  | OType      T_type
  | OInt       T_int
  | OWord      T_word
  | OLong      T_long
  | OFloat     T_float
  | ORatio     T_ratio
  | OComplex   T_complex
  | OAbsTime   T_time
  | ORelTime   T_diffTime
  | OChar      T_char
  | OString    T_string
  | ORef       T_ref
  | OList      [Value o]
  | ODict      (M.Map Name (Value o))
  | OTree      (Struct (Value o))
  | OBytes     T_bytes
  | OHaskell   o
  deriving (Eq, Ord, Typeable, Show)

type T_type     = ObjType
type T_int      = Int
type T_word     = Word64
type T_long     = Integer
type T_ratio    = Rational
type T_complex  = Complex
type T_float    = Double
type T_time     = UTCTime
type T_diffTime = NominalDiffTime
type T_char     = Char
type T_string   = UStr
type T_ref      = QualRef
type T_bytes    = B.ByteString

instance NFData obj => NFData (Value obj) where
  rnf  ONull         = ()
  rnf  OTrue         = ()
  rnf (OType      a) = deepseq a ()
  rnf (OInt       a) = deepseq a ()
  rnf (OWord      a) = deepseq a ()
  rnf (OLong      a) = deepseq a ()
  rnf (OFloat     a) = deepseq a ()
  rnf (ORatio     a) = deepseq a ()
  rnf (OComplex   a) = deepseq a ()
  rnf (OAbsTime   a) = deepseq a ()
  rnf (ORelTime   a) = deepseq a ()
  rnf (OChar      a) = deepseq a ()
  rnf (OString    a) = deepseq a ()
  rnf (ORef       a) = deepseq a ()
  rnf (OList      a) = deepseq a ()
  rnf (ODict      a) = deepseq a ()
  rnf (OTree      a) = deepseq a ()
  rnf (OBytes     a) = seq a ()
  rnf (OHaskell   a) = deepseq a ()

instance HasNullValue obj => HasNullValue (Value obj) where
  nullValue = ONull
  testNull a = case a of
    ONull        -> True
    OInt      i  -> testNull i
    OWord     i  -> testNull i
    OLong     i  -> testNull i
    ORatio    r  -> testNull r
    OComplex  c  -> testNull c
    OString   s  -> testNull s
    OChar     c  -> testNull c
    ORelTime  s  -> testNull s
    OList     s  -> testNull s
    ODict     m  -> testNull m
    OTree     t  -> testNull t
    OBytes    o  -> testNull o
    OHaskell  o  -> testNull o
    _            -> False

instance B.Binary o mtab => B.Binary (Value o) mtab where
  put o = do
    let t   = B.put (typeOfObj o)
        p o = t >> B.put o
    case o of
      ONull      -> t
      OTrue      -> t
      OType    o -> p o
      OInt     o -> p o
      OWord    o -> p o
      OLong    o -> p o
      OFloat   o -> p o
      ORatio   o -> p o
      OComplex o -> p o
      OAbsTime o -> p o
      ORelTime o -> p o
      OChar    o -> p o
      OString  o -> p o
      ORef     o -> p o
      OList    o -> t >> B.putUnwrapped o
      ODict    o -> p o
      OTree    o -> p o
      OBytes   o -> p o
      OHaskell o -> B.put o
  get = B.word8PrefixTable <|> fail "expecting Object"

instance B.Binary o mtab => B.HasPrefixTable (Value o) B.Byte mtab where
  prefixTable =
    let g f = fmap f B.get
    in  mappend (OTree <$> B.prefixTable) $ B.mkPrefixTableWord8 "Object" 0x08 0x1A $
          [ return ONull
          , return OTrue
          , g OType
          , g OInt
          , g OWord
          , g OLong
          , g OFloat
          , g ORatio
          , g OComplex
          , g OAbsTime
          , g ORelTime
          , g OChar
          , g OString
          , g ORef
          , OList <$> B.getUnwrapped
          , g ODict
          , g OTree
          , g OBytes
          , mplus (OHaskell <$> B.get)
                  (B.get >>= \ (B.BlockStream1M bs1m) -> return (OBytes bs1m))
          ]

isNumeric :: Value o -> Bool
isNumeric o = case o of
  OWord     _ -> True
  OInt      _ -> True
  OLong     _ -> True
  ORelTime _ -> True
  OFloat    _ -> True
  ORatio    _ -> True
  OComplex  _ -> True
  _           -> False

isIntegral :: Value o -> Bool
isIntegral o = case o of
  OWord _ -> True
  OInt  _ -> True
  OLong _ -> True
  _       -> False

isRational :: Value o -> Bool
isRational o = case o of
  OWord     _ -> True
  OInt      _ -> True
  OLong     _ -> True
  ORelTime _ -> True
  OFloat    _ -> True
  ORatio    _ -> True
  _           -> False

isFloating :: Value o -> Bool
isFloating o = case o of
  OFloat   _ -> True
  OComplex _ -> True
  _          -> False

objToIntegral :: Value o -> Maybe Integer
objToIntegral o = case o of
  OWord o -> return $ toInteger o
  OInt  o -> return $ toInteger o
  OLong o -> return o
  _       -> mzero

objToRational :: Value o -> Maybe Rational
objToRational o = case o of
  OWord     o -> return $ toRational o
  OInt      o -> return $ toRational o
  ORelTime  o -> return $ toRational o
  OFloat    o -> return $ toRational o
  OLong     o -> return $ toRational o
  ORatio    o -> return o
  _           -> mzero

objToInt :: Value o -> Maybe Int
objToInt a = objToIntegral a >>= \a ->
  if minInt <= a && a <= maxInt then return (fromIntegral a) else mzero
  where
    minInt = fromIntegral (minBound::Int)
    maxInt = fromIntegral (maxBound::Int)

-- | Used to implement a version of 'Data.Bits.shift' and 'Data.Bits.rotate', but with an object as
-- the second parameter to these functions.
bitsMove :: (forall a . Bits a => a -> Int -> a) -> Value o -> Value o -> Maybe (Value o)
bitsMove fn a b = objToInt b >>= \b -> bitsMoveInt fn a b

bitsMoveInt :: (forall a . Bits a => a -> Int -> a) -> Value o -> Int -> Maybe (Value o)
bitsMoveInt fn a b = case a of
  OInt  a -> return (OInt  (fn a b))
  OWord a -> return (OWord (fn a b))
  OLong a -> return (OLong (fn a b))
  _       -> mzero

-- | Used to implement a version of 'Data.Bits.testBit' but with an object as the second parameter
-- to these functions.
objTestBit :: Value o -> Value o -> Maybe (Value o)
objTestBit a i = objToInt i >>= \i -> case a of
  OInt  a -> return (oBool (testBit a i))
  OWord a -> return (oBool (testBit a i))
  OLong a -> return (oBool (testBit a i))
  _       -> mzero

-- | Create an 'Object' by stacking up 'ODict' constructors to create a directory-like structure,
-- then store the Object at the top of the path, returning the directory-like object.
insertAtPath :: [Name] -> Value o -> Value o
insertAtPath px o = case px of
  []   -> o
  p:px -> ODict (M.singleton p (insertAtPath px o))

----------------------------------------------------------------------------------------------------

-- | Direct a reference at a particular tree in the runtime.
data RefQualifier
  = LOCAL -- ^ the default unless in a "with" statement, refers to the current local variable stack
  | GLODOT -- ^ a relative reference, gets it's name because it begins with a dot (".") character.
           -- Similar to the "this" keyword in C++ and Java, refers to the object of the current
           -- context set by the "with" statement, but defaults to the global variable space when
           -- not within a "with" statement. This is necessary to differentiate between local
           -- variables and references to the "with" context.
  | STATIC -- ^ a local variable stack specific to a 'Subroutine' that lives on even after the
           -- subroutine has completed.
  | GLOBAL -- ^ the global variable space for the current module.
  deriving (Eq, Ord, Typeable, Enum, Ix, Show, Read)

instance Bounded RefQualifier where { minBound=LOCAL; maxBound=GLOBAL; }

instance NFData RefQualifier where { rnf a = seq a () }

instance PPrintable RefQualifier where { pPrint = pUStr . toUStr }

instance PrecedeWithSpace RefQualifier where
   precedeWithSpace o = case o of
     LOCAL    -> True
     STATIC   -> True
     GLOBAL   -> True
     _        -> False

instance UStrType RefQualifier where
  toUStr a = ustr $ case a of
    LOCAL  -> "local"
    STATIC -> "static"
    GLOBAL -> "global"
    GLODOT -> "."
  maybeFromUStr str = case uchars str of
    "local"  -> Just LOCAL
    "static" -> Just STATIC
    "global" -> Just GLOBAL
    "."      -> Just GLODOT
    _        -> Nothing
  fromUStr str = maybe (error (show str++" is not a reference qualifier")) id (maybeFromUStr str)

instance HasRandGen RefQualifier where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::RefQualifier)))

----------------------------------------------------------------------------------------------------

data QualRef
  = Unqualified Reference
  | Qualified RefQualifier Reference
  deriving (Eq, Ord, Typeable, Show)

instance NFData QualRef where
  rnf (Unqualified r) = deepseq r $! ()
  rnf (Qualified q r) = seq q $! deepseq r ()

instance HasNullValue QualRef where
  nullValue = Unqualified (Reference [])
  testNull o = case o of
    Unqualified     r -> testNull r
    Qualified LOCAL r -> testNull r
    _                 -> False

instance PPrintable QualRef where
  pPrint o = case o of
    Unqualified r -> pInline $ dots r
    Qualified q r -> pInline $ ([pPrint q, pString " "]++) $ dots r
    where { dots r = intercalate [pString "."] $ map (return . pPrint) $ refNameList r }

instance B.Binary QualRef mtab where
  put o = case o of
    Unqualified ref -> B.put ref
    Qualified q ref -> B.prefixByte pfx $ B.put ref where
      pfx = case q of
        LOCAL  -> 0x22
        GLODOT -> 0x23
        STATIC -> 0x24
        GLOBAL -> 0x25
  get = B.word8PrefixTable <|> fail "expecting QualRef"

instance B.HasPrefixTable QualRef B.Byte mtab where
  prefixTable = mconcat $
    [ Unqualified <$> B.prefixTable
    , B.mkPrefixTableWord8 "QualRef" 0x22 0x25 $
        [ Qualified LOCAL  <$> B.get
        , Qualified GLODOT <$> B.get
        , Qualified STATIC <$> B.get
        , Qualified GLOBAL <$> B.get
        ]
    ]

instance HasRandGen QualRef where
  randO = do
    let maxbnd = fromEnum(maxBound::RefQualifier)
    i   <- nextInt (2*(maxbnd-fromEnum(minBound::RefQualifier)))
    let (d, m) = divMod i 2
    if m==0 then Unqualified <$> randO else Qualified (toEnum d) <$> randO

refNames :: UStrType str => [str] -> QualRef
refNames nx = Unqualified $ Reference (fmap (fromUStr . toUStr) nx)

maybeRefNames :: UStrType str => [str] -> Maybe QualRef
maybeRefNames nx = fmap (Unqualified . Reference) $ sequence $ fmap (maybeFromUStr . toUStr) nx

fmapQualRef :: (Reference -> Reference) -> QualRef -> QualRef
fmapQualRef fn r = case r of
  Unqualified r -> Unqualified (fn r)
  Qualified q r -> Qualified q (fn r)

setQualifier :: RefQualifier -> QualRef -> QualRef
setQualifier q ref = case ref of
  Unqualified ref -> Qualified q ref
  Qualified _ ref -> Qualified q ref

delQualifier :: QualRef -> QualRef
delQualifier ref = case ref of
  Unqualified r -> Unqualified r
  Qualified _ r -> Unqualified r

----------------------------------------------------------------------------------------------------

-- $Object_types
-- Here we have a lambda calculus for describing types. Computationally, it is very similar to the
-- Prolog programming language, however an 'ObjType' is written using a subset the Dao scripting
-- langauge.

data CoreType
  = NullType
  | TrueType
  | TypeType
  | IntType
  | WordType
  | DiffTimeType
  | FloatType
  | LongType
  | RatioType
  | ComplexType
  | TimeType
  | CharType
  | StringType
  | RefType
  | ListType
  | DictType
  | TreeType
  | BytesType
  | HaskellType
  deriving (Eq, Ord, Typeable, Enum, Bounded)

instance Show CoreType where
  show t = case t of
    NullType     -> "null"
    TrueType     -> "true"
    TypeType     -> "type"
    IntType      -> "int"
    WordType     -> "word"
    FloatType    -> "float"
    LongType     -> "long"
    RatioType    -> "ratio"
    DiffTimeType -> "diff"
    TimeType     -> "time"
    CharType     -> "char"
    StringType   -> "string"
    RefType      -> "ref"
    ListType     -> "list"
    DictType     -> "dict"
    TreeType     -> "tree"
    BytesType    -> "bytes"
    ComplexType  -> "complex"
    HaskellType  -> "haskell"

instance Read CoreType where
  readsPrec _ str = map (\a -> (a, "")) $ case str of
    "null"    -> [NullType]
    "true"    -> [TrueType]
    "type"    -> [TypeType]
    "int"     -> [IntType]
    "diff"    -> [DiffTimeType]
    "time"    -> [TimeType]
    "char"    -> [CharType]
    "string"  -> [StringType]
    "ref"     -> [RefType]
    "list"    -> [ListType]
    "dict"    -> [DictType]
    "tree"    -> [TreeType]
    "bytes"   -> [BytesType]
    "haskell" -> [HaskellType]
    _         -> []

instance NFData CoreType where { rnf a = seq a () }

instance UStrType CoreType where
  toUStr = derive_ustr
  maybeFromUStr a = case readsPrec 0 (uchars a) of
    [(o, "")] -> Just o
    _         -> Nothing
  fromUStr a = case maybeFromUStr a of
    Nothing -> error (show a++" is not a valid type identifier")
    Just  a -> a

instance Es.InfBound CoreType where
  minBoundInf = Es.Point minBound
  maxBoundInf = Es.Point maxBound

instance PPrintable CoreType where
  pPrint t = pString $ case t of
    NullType     -> "Null"
    TrueType     -> "True"
    TypeType     -> "Type"
    IntType      -> "Int"
    WordType     -> "Word"
    DiffTimeType -> "Difftime"
    FloatType    -> "Float"
    LongType     -> "Long"
    RatioType    -> "Ratio"
    ComplexType  -> "Complex"
    TimeType     -> "Time"
    CharType     -> "Char"
    StringType   -> "String"
    RefType      -> "Ref"
    ListType     -> "List"
    DictType     -> "Dict"
    TreeType     -> "Tree"
    BytesType    -> "Data"
    HaskellType  -> "Foreign"

instance B.Binary CoreType mtab where
  put t = B.putWord8 $ case t of
    NullType     -> 0x08
    TrueType     -> 0x09
    TypeType     -> 0x0A
    IntType      -> 0x0B
    WordType     -> 0x0C
    LongType     -> 0x0D
    FloatType    -> 0x0E
    RatioType    -> 0x0F
    ComplexType  -> 0x10
    TimeType     -> 0x11
    DiffTimeType -> 0x12
    CharType     -> 0x13
    StringType   -> 0x14
    RefType      -> 0x15
    ListType     -> 0x16
    DictType     -> 0x17
    TreeType     -> 0x18
    BytesType    -> 0x19
    HaskellType  -> 0x1A
  get = B.word8PrefixTable <|> fail "expecting CoreType"

instance B.HasPrefixTable CoreType B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "CoreType" 0x08 0x1A $ map return $
    [ NullType
    , TrueType
    , TypeType
    , IntType
    , WordType
    , LongType
    , FloatType
    , RatioType
    , ComplexType
    , TimeType
    , DiffTimeType
    , CharType
    , StringType
    , RefType
    , ListType
    , DictType
    , TreeType
    , BytesType
    , HaskellType
    ]

instance HasRandGen CoreType where { randO = toEnum <$> nextInt (fromEnum (maxBound::CoreType)) }

typeOfObj :: Value o -> CoreType
typeOfObj o = case o of
  ONull      -> NullType
  OTrue      -> TrueType
  OType    _ -> TypeType
  OInt     _ -> IntType
  OWord    _ -> WordType
  OLong    _ -> LongType
  OFloat   _ -> FloatType
  ORatio   _ -> RatioType
  OComplex _ -> ComplexType
  OAbsTime _ -> TimeType
  ORelTime _ -> DiffTimeType
  OChar    _ -> CharType
  OString  _ -> StringType
  ORef     _ -> RefType
  OList    _ -> ListType
  ODict    _ -> DictType
  OTree    _ -> TreeType
  OBytes   _ -> BytesType
  OHaskell _ -> HaskellType

oBool :: Bool -> Value o
oBool a = if a then OTrue else ONull

----------------------------------------------------------------------------------------------------

-- | A symbol in the type calculus.
data TypeSym
  = CoreType CoreType
    -- ^ used when the type of an object is equal to it's value, for example Null and True,
    -- or in situations where the type of an object has a value, for example the dimentions of a
    -- matrix.
  | TypeVar  Reference [ObjType]
    -- ^ a polymorphic type, like 'AnyType' but has a name.
  deriving (Eq, Ord, Show, Typeable)

instance NFData TypeSym where
  rnf (CoreType a  ) = deepseq a ()
  rnf (TypeVar  a b) = deepseq a $! deepseq b ()

instance HasRandGen TypeSym where
  randO = countRunRandChoice
  randChoice = randChoiceList [CoreType <$> randO, pure TypeVar <*> randO <*> randList 1 4]

instance PPrintable TypeSym where
  pPrint t = case t of
    CoreType t     -> pPrint t
    TypeVar  t ctx -> pInline $
      concat [[pPrint t], guard (not (null ctx)) >> [pList_ "[" ", " "]" (map pPrint ctx)]]

instance B.Binary TypeSym mtab where
  put o = case o of
    CoreType o       -> B.prefixByte 0x1D $ B.put o
    TypeVar  ref ctx -> B.prefixByte 0x1E $ B.put ref >> B.put ctx
  get = B.word8PrefixTable <|> fail "expecting TypeSym"

instance B.HasPrefixTable TypeSym B.Byte mtab where
  prefixTable =
    B.mkPrefixTableWord8 "TypeSym" 0x1D 0x1E [CoreType <$> B.get, pure TypeVar <*> B.get <*> B.get]

----------------------------------------------------------------------------------------------------

-- | Complex type structures can be programmed by combining 'ObjSimpleType's.
newtype TypeStruct = TypeStruct [TypeSym] deriving (Eq, Ord, Show, Typeable)

instance NFData TypeStruct where { rnf (TypeStruct a) = deepseq a () }

instance HasNullValue TypeStruct where { nullValue = TypeStruct []; testNull (TypeStruct a) = null a; }

instance PPrintable TypeStruct where
  pPrint (TypeStruct tx) = pList (pString "type") "(" ", " ")" (map pPrint tx)

instance B.Binary TypeStruct mtab where
  put (TypeStruct o) = B.prefixByte 0x1C $ B.put o
  get = B.word8PrefixTable <|> fail "expecting TypeStruct"

instance B.HasPrefixTable TypeStruct B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "TypeStruct" 0x1C 0x1C [TypeStruct <$> B.get]

instance HasRandGen TypeStruct where { randO = TypeStruct <$> randList 1 4 }

----------------------------------------------------------------------------------------------------

-- | The fundamental 'Type' used to reason about whether an object is fit to be used for a
-- particular function.
newtype ObjType = ObjType { typeChoices :: [TypeStruct] } deriving (Eq, Ord, Show, Typeable)

instance NFData ObjType where { rnf (ObjType a) = deepseq a () }

instance HasNullValue ObjType where { nullValue = ObjType []; testNull (ObjType a) = null a; }

instance PPrintable ObjType where
  pPrint (ObjType tx) = pList (pString "anyOf") "(" ", " ")" (map pPrint tx)

instance B.Binary ObjType mtab where
  put (ObjType o) = B.prefixByte 0x1B $ B.put o
  get = B.word8PrefixTable <|> fail "expecting ObjType"

instance B.HasPrefixTable ObjType B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "ObjType" 0x1B 0x1B [ObjType <$> B.get]

instance HasRandGen ObjType where { randO = ObjType <$> randList 1 3 }

----------------------------------------------------------------------------------------------------

-- | A 'Reference' is basically an address that can be used to lookup and update various objects in
-- the runtime.
newtype Reference = Reference { refNameList :: [Name] } deriving (Eq, Ord, Typeable, Show)

instance Monoid Reference where
  mempty = Reference []
  mappend (Reference a) (Reference b) = Reference (a++b)

instance HasNullValue Reference where
  nullValue = mempty
  testNull (Reference a) = null a

instance Read Reference where
  readsPrec _ str = case str of
    c:_  | isAlpha c ->
      case break (\c -> c=='.' || isAlphaNum c) str of
        (cx, str) ->
          maybe [] (return . (\ref -> (Reference ref, str))) $ sequence $
            fix (\loop str -> case break (=='.') str of
                    (cx, str) -> case cx of
                      [] -> []
                      cx -> maybeFromUStr (ustr (dropWhile (=='.') cx)) : loop str
                ) cx
    _ -> mzero

instance UStrType Reference where
  toUStr (Reference nx) = ustr $ intercalate "." $ map uchars nx
  maybeFromUStr str = case readsPrec 0 (uchars str) of
    [(ref, "")] -> Just ref
    _           -> Nothing
  nil = mempty

instance NFData Reference where { rnf (Reference  a) = deepseq a () }

instance PPrintable Reference where
  pPrint (Reference r) = pInline $ intercalate [pString "."] (fmap (return . pPrint) r)

instance HasRandGen Reference where { randO = fmap Reference (randList 1 6) }

instance B.Binary Reference mtab where
  put (Reference o) = B.prefixByte 0x1F $ B.put o
  get = B.word8PrefixTable <|> fail "expecting Reference"

instance B.HasPrefixTable Reference B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "Reference" 0x1F 0x1F [Reference <$> B.get]

----------------------------------------------------------------------------------------------------

newtype Complex = Complex (C.Complex Double)
  deriving (Eq, Typeable, Floating, Fractional, Num)

-- | Since 'Object' requires all of it's types instantiate 'Prelude.Ord', I have defined
-- 'Prelude.Ord' of 'Data.Complex.Complex' numbers to be the distance from 0, that is, the radius of
-- the polar form of the 'Data.Complex.Complex' number, ignoring the angle argument.
instance Ord Complex where
  compare (Complex a) (Complex b) = compare (C.polar a) (C.polar b)

instance Show Complex where
  show (Complex a) = "("++show re++(if im<0 then "-" else "+")++show im++"i)" where
    re = C.realPart a
    im = C.imagPart a

instance NFData Complex where { rnf (Complex a) = deepseq a $! () }

instance HasNullValue Complex where
  nullValue = Complex (0 C.:+ 0)
  testNull (Complex c) = C.realPart c == 0 && C.imagPart c == 0

instance B.Binary Complex mtab where
  put o = B.put (realPart o) >> B.put (imagPart o)
  get   = pure complex <*> B.get <*> B.get

instance PPrintable Complex where
  pPrint (Complex (a C.:+ b))
    | a==0.0 && b==0.0 = pString "0i"
    | a==0.0           = pString (show b++"i")
    | b==0.0           = pShow a
    | otherwise        = pInline [pShow a, pString (if b<0 then "-" else "+"), pShow b]

realPart :: Complex -> Double
realPart (Complex o) = C.realPart o

imagPart :: Complex -> Double
imagPart (Complex o) = C.imagPart o

mkPolar :: Double -> Double -> Complex
mkPolar a b = Complex (C.mkPolar a b)

cis :: Double -> Complex
cis = Complex . C.cis

polar :: Complex -> (Double, Double)
polar (Complex o) = C.polar o

magnitude :: Complex -> Double
magnitude (Complex o) = C.magnitude o

phase :: Complex -> Double
phase (Complex o) = C.phase o

conjugate :: Complex -> Complex
conjugate (Complex o) = Complex (C.conjugate o)

complex :: Double -> Double -> Complex
complex a b = Complex (a C.:+ b)

----------------------------------------------------------------------------------------------------

-- | Create the minimum-sized array that can store all of the indices in the given list, setting the
-- 'Data.Array.IArray.bounds' of the array automatically. Evaluates to 'Prelude.Nothing' if the
-- given list of elements is empty.
minAccumArray :: Ix i => (e -> e' -> e) -> e -> [(i, e')] -> Maybe (Array i e)
minAccumArray accfn deflt elems =
  if null elems then Nothing else Just (accumArray accfn deflt bnds elems) where
    idxs = map fst elems
    i0   = head idxs
    bnds = foldl (\ (lo, hi) i -> (min lo i, max hi i)) (i0, i0) (tail idxs)

-- | Create the minimum-sized array that can store all of the indices in the given list, and setting
-- the 'Data.Array.IArray.bounds' of the array automatically. Evaluates to 'Prelude.Nothing' if the
-- given list of elements is empty.
minArray :: Ix i => e -> [(i, e)] -> Maybe (Array i e)
minArray deflt elems = minAccumArray (flip const) deflt elems

