-- "src/Dao/Interpreter.hs"  declares the "Object" data type which is the
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

import           Dao.Glob
import           Dao.PPrint
import           Dao.Predicate
import           Dao.Procedural
import           Dao.Random
import           Dao.Stack
import           Dao.String
import           Dao.Token hiding (asString)
import qualified Dao.Binary  as B
import qualified Dao.EnumSet as Es
import qualified Dao.Tree    as T

import           Data.Array.IArray
import           Data.Binary (encode)
import           Data.Bits
import           Data.Char
import           Data.Dynamic
import           Data.IORef
import           Data.List
import           Data.Monoid
import           Data.Ratio
import           Data.Time.Clock
import           Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.Complex         as C
import qualified Data.IntMap          as I
import qualified Data.Map             as M
import qualified Data.Set             as S

import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State

import           System.IO

----------------------------------------------------------------------------------------------------

-- | This is the data type used as the intermediary between Haskell objects and Dao objects. If you
-- would like your Haskell data type to be used as a non-opaque data type in a Dao language script,
-- the first step is to instantiate your data type into this class. The next step would be to
-- instantiate your object into the 'HaskellDataClass' class. Instantiating the
-- 'HaskellDataClass' class alone will make your object usable in Dao language scripts, but
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
-- a 'DaoFunc' (via 'setupDao') that constructs a Point3D at runtime:
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
-- > instance 'ToDaoStructClass' Point3D 'Object' where
-- >     'toDaoStruct' = 'fromData' "@Point2D@" $ do
-- >         'putPrimField' "x" get_x
-- >         'putPrimField' "y" get_y
-- >          obj <- 'Control.Monad.Reader.Class.ask'
-- >          case obj of
-- >             Point3D _ _ z -> do
-- >                 'renameConstructor' "@Point3D@"
-- >                 'define' "z" ('obj' z)
-- >             _             -> return ()
-- 
-- Finally, you should define the instantiation of Point3D into the 'HaskellDataClass' class so it
-- includes the directive 'autoDefToStruct'.
class ToDaoStructClass haskData value where
  toDaoStruct :: ToDaoStruct value haskData ()

-- | Continuing the example from above, if you do want your data type to be modifyable by functions
-- running in the Dao language runtime, you must instantiate this class, which is facilitated by the
-- 'toData' function.
-- > instance 'FromDaoStructClass' 'Point3D' 'Object' where
-- >     fromDaoStruct = 'toData' $ 'Control.Monad.msum' $
-- >         [ do 'constructor' "@Point2D@"
-- >              'Control.Applicative.pure' Point3D <*> '?' "x" <*> '?' "y"
-- >         , do 'constructor' "@Point3D@"
-- >              'Control.Applicative.pure' Point3D <*> '?' "x" <*> '?' "y" <*> '@' "z"
-- >         ]
-- 
-- Do not forget to define the instantiation of Point3D into the 'HaskellDataClass' class so it
-- includes the directive 'autoDefFromStruct'.
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
-- > instance 'FromDaoStruct' ('Object) where
-- >     'fromDaoStruct' = 'toData' $ 'Control.Monad.msum' $
-- >         [ 'constructor' "A" >> return a,
-- >           do 'constructor' "B"
-- >              B 'Control.Applicative.<$>' ('field' "b1" >>= 'primType')
-- >           do 'constructor' "C"
-- >              'Control.Applicative.pure' C
-- >                  'Control.Applicative.<*>' 'required' ('field' "c1" >>= 'primType')
-- >                  'Control.Applicative.<*>' 'required' ('field' "c2" >>= 'primType')
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
-- >         "getX" '.=@' getX
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

instance ToDaoStructClass RefQualifier Object where { toDaoStruct=putNullaryUsingShow; }

instance FromDaoStructClass RefQualifier Object where { fromDaoStruct=getNullaryWithRead; }

instance ObjectClass      RefQualifier where { obj=new; fromObj=objFromHaskellData; }

instance HaskellDataClass RefQualifier where
  haskellDataInterface = interface LOCAL $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

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

----------------------------------------------------------------------------------------------------

-- "src/Dao/Evaluator.hs"  provides functions for executing the Dao
-- scripting language, i.e. functions evaluating the parsed abstract
-- syntax tree.
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

-- not for export
lu :: Location
lu  = LocationUnknown
fd :: HasLocation a => a -> a
fd = delLocation
fd1 :: (HasLocation a, Functor f) => f a -> f a
fd1 = fmap delLocation

----------------------------------------------------------------------------------------------------

-- | Object was originally it's own @data@ type, but now it is a synonym for a 'Value'
-- polymorphic over the 'HaskellData' @data@ type.
type Object = Value HaskellData
type T_list     = [Value Object]
type T_dict     = M.Map Name Object
type T_struct   = Struct Object

instance HaskellDataClass (Struct Object) where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

instance ToDaoStructClass (Struct Object) Object where { toDaoStruct = return () }
instance FromDaoStructClass (Struct Object) Object where { fromDaoStruct = FromDaoStruct $ lift get }

instance HaskellDataClass (StructError Object) where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest
    autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | An alternative to 'Glob' expressions containing ordinary 'Dao.String.UStr's is a 'Glob'
-- expression containing 'FuzzyStr's. These strings approximately match the input string, ignoring
-- minor spelling errors and transposed characters.
newtype FuzzyStr = FuzzyStr UStr deriving (Ord, Typeable)

instance Eq FuzzyStr where
  a==b = 
    let ax = S.map toLower (S.fromList (uchars a))
        bx = S.map toLower (S.fromList (uchars b))
    in     a == b
        || ax == bx
        || S.size (S.difference (S.union ax bx) (if S.size ax < S.size bx then ax else bx)) <= 1

instance Show FuzzyStr where { show (FuzzyStr str) = show str }

instance Read FuzzyStr where
  readsPrec p input = readsPrec p input >>= \ (s, rem) -> return (FuzzyStr (ustr s), rem)

instance Monoid FuzzyStr where
  mempty = FuzzyStr mempty
  mappend (FuzzyStr a) (FuzzyStr b) = FuzzyStr (a<>b)

instance HasNullValue FuzzyStr where
  nullValue = FuzzyStr nullValue
  testNull (FuzzyStr s) = testNull s

instance UStrType FuzzyStr where { fromUStr = FuzzyStr; toUStr (FuzzyStr u) = u; }

instance Show (Glob FuzzyStr) where { show = show . fmap toUStr }

instance Read (Glob FuzzyStr) where
  readsPrec prec str = readsPrec prec str >>= \ (glob, str) -> [(fmap fromUStr glob, str)]

instance Show (GlobUnit Object) where
  show o = case o of
    Single o -> show o
    globunit -> show (fmap (const "") globunit)

instance Show (Glob Object) where
  show glob = (++"\"") $ ('"':) $ do
    o <- getPatUnits glob
    let other o = "$("++prettyShow o++")"
    case o of
      Single o -> case o of
        OString  o -> uchars o
        OHaskell (HaskellData dyn _ifc) -> case fromDynamic dyn of
          Nothing           -> other o
          Just (FuzzyStr o) -> uchars o
        _ -> other o
      globunit -> show (fmap (const "") globunit)

instance PPrintable (Glob Object) where { pPrint = pShow }

----------------------------------------------------------------------------------------------------

-- | This function is a placeholder used by the type system. The value of this function is
-- undefined, so strictly evaluating it will throw an exception. Fortunately, the only time you will
-- ever use this function is with the 'daoClass' function, which uses the type of this function but
-- never it's value. Refer to the documentation on 'daoClass' to see how to properly use this
-- function.
haskellType :: HaskellDataClass o => o
haskellType = error $ unwords $
  [ "the haskellType function is just a placeholder"
  , "used by the type system, it must not be evaluated."
  ]

-- The stateful data for the 'DaoSetup' monad.
data SetupModState
  = SetupModState
    { daoSatisfies     :: T.Tree Name ()
      -- ^ a set of references that can satisfy "required" statements in Dao scripts.
    , daoTopLevelFuncs :: M.Map Name DaoFunc
    , daoClasses       :: MethodTable
    , daoEntryPoint    :: Exec ()
    }

-- | This monadic type allows you to define a built-in module using procedural
-- programming syntax. Simply define each addition to the module one line at a time. Functions that
-- you can use include 'modProvides', 'modFunction', 'daoClass', and 'daoInitalize'.
-- 
-- Define clever names for every 'DaoSetup' you write, then 
type DaoSetup = DaoSetupM ()
newtype DaoSetupM a = DaoSetup{ daoSetupM :: State SetupModState a }
  deriving (Functor, Applicative, Monad)

-- not for export
updateSetupModState :: (SetupModState -> SetupModState) -> DaoSetup
updateSetupModState f = DaoSetup (modify f)

-- | Dao programs can declare "requires" statements along with it's imports. If your built-in module
-- provides what Dao programs might "required", then declare that this module provides that feature
-- using this function.
daoProvides :: UStrType s => s -> DaoSetup
daoProvides label = updateSetupModState $ \st ->
  st{ daoSatisfies = T.insert (refNameList $ read $ uchars label) () (daoSatisfies st) }

-- | Associate an 'HaskellDataClass' with a 'Name'. This 'Name' will be callable from within Dao scripts.
-- > newtype MyClass = MyClass { ... } deriving (Eq, Ord)
-- >
-- > instance 'HaskellDataClass' MyClass where
-- >     'haskellDataInterface' = 'interface' $ do
-- >         'autoDefEquality'
-- >         'autoDefOrdering'
-- >         ...
-- >
-- > setupDao :: 'DaoSetup'
-- > setupDao = do
-- >     daoClass "myClass" (haskellType::MyClass)
-- >     ...
daoClass :: (UStrType name, Typeable o, HaskellDataClass o) => name -> o -> DaoSetup
daoClass name ~o = updateSetupModState $ \st ->
    st{ daoClasses = insertMethodTable o (fromUStr $ toUStr name) haskellDataInterface (daoClasses st) }

-- | Define a built-in function. Examples of built-in functions provided in this module are
-- "print()", "join()", and "exec()".
daoFunction :: (Show name, UStrType name) => name -> DaoFunc -> DaoSetup
daoFunction name func = updateSetupModState $ \st ->
  st{ daoTopLevelFuncs = M.insert (fromUStr $ toUStr name) func (daoTopLevelFuncs st) }

-- | Provide an 'Exec' monad to perform when 'setupDao' is evaluated. You may use this function as
-- many times as you wish, every 'Exec' monad will be executed in the order they are specified. This
-- is a good way to create a read-eval-print loop.
daoInitialize :: Exec () -> DaoSetup
daoInitialize f = updateSetupModState $ \st -> st{ daoEntryPoint = daoEntryPoint st >> f }

-- | Use this function evaluate a 'DaoSetup' in the IO () monad. Use this to define the 'main'
-- function of your program.
setupDao :: DaoSetup -> IO ()
setupDao setup0 = do
  let setup = execState (daoSetupM setup0) $
        SetupModState
        { daoSatisfies     = T.Void
        , daoTopLevelFuncs = M.empty
        , daoClasses       = mempty
        , daoEntryPoint    = return ()
        }
  xunit  <- initExecUnit
  result <- ioExec (daoEntryPoint setup) $
    xunit
    { providedAttributes = daoSatisfies setup
    , builtinFunctions   = daoTopLevelFuncs setup
    , globalMethodTable  = daoClasses setup
    }
  case result of
    OK    ()                -> return ()
    PFail (ExecReturn  obj) -> maybe (return ()) (putStrLn . prettyShow) obj
    PFail (err@ExecError{}) -> hPutStrLn stderr (prettyShow err)
    Backtrack               -> hPutStrLn stderr "(does not compute)"

----------------------------------------------------------------------------------------------------

-- | This is the data type used to associate a Haskell data type with the 'Interface' used by the
-- runtime to read and modify the data. Whenever an non-primitive 'Object' is created, the data is
-- converted to a 'Data.Dynamic.Dynamic' value and paired with a copy of the 'Interface'.
data HaskellData = HaskellData Dynamic (Interface Dynamic) deriving Typeable

instance Eq HaskellData where
  HaskellData a ifcA == HaskellData b ifcB =
    ((ifcA==ifcB)&&) $ maybe False id $ objEquality ifcA >>= \eq -> return (eq a b)

instance Ord HaskellData where
  compare (HaskellData a ifcA) (HaskellData b ifcB) = maybe err id $
    guard (ifcA==ifcB) >> objOrdering ifcA >>= \comp -> return (comp a b) where
      err = error $ unwords $
        [ "cannot compare object of type", show (objHaskellType ifcA)
        , "with obejct of type", show (objHaskellType ifcB)
        ]

instance Show HaskellData where { show (HaskellData _ o) = show (objHaskellType o) }

instance NFData HaskellData where { rnf (HaskellData _ _) = () }

instance PPrintable Object where
  pPrint o = case o of
    ONull            -> pString "null"
    OTrue            -> pString "true"
    OType      o     -> pPrint o
    OInt       o     -> pShow o
    OWord      o     -> pString (show o++"U")
    OLong      o     -> pString (show o++"L")
    OFloat     o     -> pString (show o++"f")
    ORatio     o     ->
      if denominator o == 1
        then  pString (show (numerator o)++"R")
        else  pWrapIndent $
                [ pString "(", pString (show (numerator o)), pString "/"
                , pString (show (denominator o)++"R"), pString ")"
                ]
    OComplex   o     -> pPrint o
    ORelTime   o     -> pShow o
    OAbsTime   o     -> pString ("date "++show o)
    OChar      o     -> pShow o
    OString    o     -> pShow o
    ORef       o     -> pPrint o
    OList      ox    -> if null ox then pString "list{}" else pContainer "list " pPrint ox
    ODict      o     ->
      if M.null o
      then pString "dict{}"
      else pContainer "dict " (\ (a, b) -> pWrapIndent [pPrint a, pString " = ", pPrint b]) (M.assocs o)
    OTree      o     -> pPrint o
    OBytes     o     ->
      if B.null o
        then  pString "data{}"
        else  pList (pString "data") "{" ", " "}" (map (pString . showHex) (B.unpack o))
    OHaskell   (HaskellData o ifc) -> case objPPrinter ifc of
      Nothing -> fail $ "cannot pretty print Haskell data type: "++show (objHaskellType ifc)
      Just pp -> pp o

instance B.Binary HaskellData MTab where
  put (HaskellData o ifc) = do
    let typ = objHaskellType ifc 
    let tid = typeRepToUStr typ
    mtab <- B.getCoderTable
    case B.getEncoderForType tid mtab of
      Just fn -> do
        tid  <- B.newInStreamID tid
        B.put tid >> B.putWithBlockStream1M (fn o)
      Nothing -> fail $ unwords ["no binary format method defied for Haskell type", show typ]
  get = do
    B.updateTypes
    mtab <- B.getCoderTable
    tid  <- B.get >>= B.decodeIndexLookup
    maybe mzero id $ do
      tid <- tid
      fn  <- B.getDecoderForType tid mtab
      tab <- lookupMethodTable (fromUStr tid) mtab
      return (flip HaskellData tab <$> B.getWithBlockStream1M fn)

----------------------------------------------------------------------------------------------------

instance HasNullValue HaskellData where
  nullValue = toHaskellData ()
  testNull (HaskellData o ifc) = case objNullTest ifc of
    Nothing -> error ("to check whether objects of type "++show (objHaskellType ifc)++" are null is undefined behavior")
    Just fn -> fn o

showObj :: PPrintable a => a -> String
showObj = prettyPrint 80 "    "

----------------------------------------------------------------------------------------------------

-- | This class provides a consistent interface, the 'obj' function, for converting a wide range of
-- types to an 'Object' type.
class ObjectClass o where
  obj   :: o -> Object
  fromObj :: Object -> Maybe o

instance ObjectClass () where
  obj () = ONull
  fromObj o = case o of { ONull -> return (); _ -> mzero; }

instance ObjectClass Bool where
  obj true = if true then OTrue else ONull
  fromObj o = case o of { OTrue -> return True; ONull -> return False; _ -> mzero }

instance ObjectClass [Object] where
  obj = OList
  fromObj o = case o of { OList o -> return o; _ -> mzero; }

instance ObjectClass QualRef where
  obj = ORef
  fromObj o = case o of { ORef o -> return o; _ -> mzero; }

instance ObjectClass (Struct Object) where
  obj = OTree
  fromObj o = case o of { OTree o -> return o; _ -> mzero; }

instance ObjectClass UStr where
  obj = OString
  fromObj o = case o of { OString o -> return o; _ -> mzero; }

instance ObjectClass String where
  obj = obj . toUStr
  fromObj = fromObj >=> maybeFromUStr

instance ObjectClass Int where
  obj = OInt
  fromObj o = case o of { OInt o -> return o; _ -> mzero; }

instance ObjectClass Word where
  obj = OWord . fromIntegral
  fromObj o = case o of { OWord o -> return (fromIntegral o); _ -> mzero; }

instance ObjectClass Word64 where
  obj = OWord
  fromObj o = case o of { OWord o -> return o; _ -> mzero; }

instance ObjectClass Name where
  obj = ORef . Unqualified . Reference . return
  fromObj o = case o of
    ORef (Unqualified (Reference [name])) -> return name
    _ -> mzero

instance ObjectClass Reference where
  obj = ORef . Unqualified
  fromObj o = case o of
    ORef (Unqualified ref) -> return ref
    _ -> mzero

instance ObjectClass Double where
  obj = OFloat
  fromObj o = case o of { OFloat o -> return o; _ -> mzero; }

instance ObjectClass Integer where
  obj = OLong
  fromObj o = case o of { OLong o -> return o; _ -> mzero; }

instance ObjectClass HaskellData where
  obj = OHaskell
  fromObj o = case o of { OHaskell o -> return o; _ -> mzero; }

instance ObjectClass Dynamic where
  obj = opaque
  fromObj o = case o of { OHaskell (HaskellData o _) -> return o; _ -> mzero; }

instance ObjectClass (Value HaskellData) where { obj = id; fromObj = return; }

instance ObjectClass Location where { obj=new; fromObj=objFromHaskellData; }

listToObj :: ObjectClass o => [o] -> Object
listToObj = OList . map obj

listFromObj :: ObjectClass o => Object -> Maybe [o]
listFromObj o = case o of
  OList o -> mapM fromObj o
  _       -> mzero

-- | Create a new 'Object' containing the original value and a reference to the 'Interface'
-- retrieved by the instance of 'haskellDataInterface' for the data type.
new :: (HaskellDataClass typ, Typeable typ) => typ -> Object
new = OHaskell . toHaskellData

-- | Create a completely opaque haskell data type that can be used stored to a Dao language
-- variable, but never inspected or modified in any way.
opaque :: Typeable typ => typ -> Object
opaque o = OHaskell $ HaskellData (toDyn o) $
  interfaceToDynamic $ interface o $ return ()

-- | The inverse operation of 'new', uses 'fromObj' and 'fromHaskellData' to extract the data type
-- wrapped up in the 'Object', assuming the 'Object' is the 'OHaskell' constructor holding a
-- 'HaskellData' container.
objFromHaskellData :: (Typeable o, HaskellDataClass o) => Object -> Maybe o
objFromHaskellData = fromObj >=> fromHaskellData

----------------------------------------------------------------------------------------------------

-- $Building_structs
-- Here are all the basic functions for converting between Haskell language data types and Dao
-- language structures.
-- 
-- Most 'FromDaoStruct' functions will backtrack when they fail to get the necessary data. This
-- function can make a backtracking function fail. For example:
-- > 'tryField' "x" >>= 'objType'
-- backtracks in any case
-- 
-- > required (tryField "x" >>= objType)
-- > tryField "x" >>= required objType
-- These two forms do the same thing: fails if 'objType' backtracks, but not if the field doesn't
-- exist.
-- 
-- > 'Control.Applicative.optional' ('tryField' "x" >>= 'objType')
-- returns 'Prelude.Nothing' if the field does not exist or if 'objType' backtracks
-- 
-- > 'field' "x" >>= 'objType'
-- fails if the field does not exist, backtracks if it exists but is the wrong type
-- (you probably don't ever want to do this).
-- 
-- > 'required' ('field' "x" >>= 'objType')
-- > 'field' "x" >>= 'required' 'objType'
-- These two forms are the same: fails if either the field does not exist or if 'objType'
-- backtracks.

-- | Takes a conversion as the first parameter. The second parameter will be provided by 'field' or
-- 'tryField' when you pass it as a partial function application. If the conversion function
-- backtracks, 'Control.Monad.Error.throwError' is evaluated with the appropriate error data set.
-- This function should usually not be required, as it is called by functions like 'opt', 'req', and
-- 'reqList'.
convertFieldData :: (Object -> FromDaoStruct Object o) -> Object -> FromDaoStruct Object o
convertFieldData f o = mplus (f o) $ throwError $ nullValue{ structErrValue=Just o }

-- | A required 'Struct' 'field'. This function is defined as
req :: (UStrType name, Typeable o, ObjectClass o) => name -> FromDaoStruct Object o
req name = field name (convertFieldData (maybe mzero return . fromObj))

-- | Check if a 'Struct' field exists using 'tryField', if it exists, convert it to the necessary
-- data type using 'fromObj' (which fails if an unexpected type is stored in that field).
opt :: (UStrType name, Typeable o, ObjectClass o) => name -> FromDaoStruct Object (Maybe o)
opt name = Just <$> tryField name (convertFieldData (maybe mzero return . fromObj)) <|> return Nothing

-- | Like 'req' but internally uses 'listFromObj' instead of 'fromObj'. The field must exist, if it
-- does not this function evaluates to 'Control.Monad.Error.throwError'. Use 'optList' instead if
-- you can accept an empty list when the field is not defined.
reqList :: (UStrType name, Typeable o, ObjectClass o) => name -> FromDaoStruct Object [o]
reqList name = field name $ convertFieldData (maybe mzero return . listFromObj)

-- | Like 'opt' but internally uses 'listFromObj' instead of 'fromObj'. The field may not exist, and
-- if it does not this function returns an empty list. Use 'reqList' to evaluate to
-- 'Control.Monad.Error.throwError' in the case the field does not exist.
optList :: (UStrType name, Typeable o, ObjectClass o) => name -> FromDaoStruct Object [o]
optList name = tryField name $ convertFieldData (maybe (return []) return . listFromObj)

-- | This is an important function for instantiating 'ToDaoStructClass'. It takes any
-- value instantiating 'HaskellDataClass', converts it to an 'Object' using the 'new'
-- function. It is the inverse of 'objType'.
--
-- It is recommended you use this function instead of 'defStructField', 'defPrimField', or
-- 'defDynField' whenever it is possible, i.e. whenever the data type you are putting instantiated
-- the 'HaskellDataClass' class.
defObjField
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> o -> ToDaoStruct Object haskData Object
defObjField name o = define name (obj o)

-- | Synonym for 'defObjField'
(.=) :: (UStrType name, Typeable o, ObjectClass o) => name -> o -> ToDaoStruct Object haskData Object
(.=) = defObjField
infixr 2 .=

-- | Like 'defObjField' but takes a field accessor to extract the data to be stored from the object
-- being converted. This function is defined as:
-- > \name accessor -> asks accessor >>= defObjField name
putObjField
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> (haskData -> o) -> ToDaoStruct Object haskData Object
putObjField name which = asks which >>= defObjField name

-- | Synonym for 'putObjField'
(.=@)
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> (haskData -> o) -> ToDaoStruct Object haskData Object
(.=@) = putObjField
infixr 2 .=@

-- | Like 'putObjField' but operates on an object wrapped in a 'Prelude.Maybe', not doing anything
-- in the case of 'Prelude.Nothing'.
defMaybeObjField
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> Maybe o -> ToDaoStruct Object haskData (Maybe Object)
defMaybeObjField name = maybe (return Nothing) (fmap Just . defObjField name)

(.=?) 
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> Maybe o -> ToDaoStruct Object haskData (Maybe Object)
(.=?) = defMaybeObjField

----------------------------------------------------------------------------------------------------

instance ToDaoStructClass Location Object where
  toDaoStruct = ask >>= \lo -> case lo of
    LocationUnknown -> makeNullary "Void"
    Location{} -> void $ do
      renameConstructor "Location"
      "startingLine"   .=@ startingLine
      "startingColumn" .=@ startingColumn
      "endingLine"     .=@ endingLine
      "endingColumn"   .=@ endingColumn

instance FromDaoStructClass Location Object where
  fromDaoStruct = msum $
    [ nullary "Void" >> return LocationUnknown
    , do  constructor "Location"
          pure Location
            <*> req "startingLine"
            <*> req "startingColumn"
            <*> req "endingLine"
            <*> req "endingColumn"
    ]

putLocation :: Location -> ToDaoStruct Object haskData ()
putLocation loc = case loc of
  LocationUnknown -> return ()
  Location{} -> void $ "location" .= loc

location :: FromDaoStruct Object Location
location = req "location"

putComments :: [Comment] -> ToDaoStruct Object haskData ()
putComments = void . defObjField "comments"

comments :: FromDaoStruct Object [Comment]
comments = req "comments"

optComments :: FromDaoStruct Object (Maybe [Comment])
optComments = opt "comments"

instance HasRandGen Object where
  randO = recurseRunRandChoice ONull
  randChoice = randChoiceList $
    [ return ONull, return OTrue
    , fmap OInt randInt
    , fmap ORef   randO
    , fmap OType  randO
    , fmap OList (randList 0 40)
    , fmap OAbsTime randO
    , fmap ORelTime randO
    , fmap OTree randO
      -- OBytes
    , do  i <- nextInt 10
          fmap (OBytes . B.concat) $
            replicateM i (fmap (encode . (\i -> fromIntegral i :: Word32)) randInt)
    ]

----------------------------------------------------------------------------------------------------

-- | This is the state that is used to run the evaluation algorithm. Every Dao program file that has
-- been loaded will have a single 'ExecUnit' assigned to it. Parameters that are stored in
-- 'Dao.Debug.DMVar's or 'Dao.Type.Resource's will be shared across all rules which are executed in
-- parallel, so for example 'execHeap' contains the variables global to all rules in a given
-- program. The remainder of the parameters, those not stored in 'Dao.Debug.DMVar's or
-- 'Dao.Type.Resource's, will have a unique copy of those values assigned to each rule as it
-- executes.
data ExecUnit
  = ExecUnit
    { globalMethodTable  :: MethodTable
      -- ^ In this slot will be stored a read-only @'Data.Map.Lazy.Map' 'Dao.String.UStr'
      -- 'Interface'@ object that will allow any method with access to this
      -- 'GenRuntime' to retrieve a 'Interface' by it's name string. Specifically,
      -- this will be used by objects stored in the 'OHaskell' constructor.
    , pathIndex          :: MVar (M.Map UPath ExecUnit)
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , defaultTimeout     :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , importGraph        :: MVar (M.Map UPath ExecUnit)
    , currentWithRef     :: WithRefStore
      -- ^ the current document is set by the @with@ statement during execution of a Dao script.
    , taskForExecUnits   :: Task
    , currentQuery       :: Maybe UStr
    , currentPattern     :: Maybe (Glob Object)
    , currentCodeBlock   :: StaticStore
      -- ^ when evaluating a 'Subroutine' selected by a string query, the action resulting from
      -- that query is defnied here. It is only 'Data.Maybe.Nothing' when the module is first being
      -- loaded from source code.
    , currentBranch      :: [Name]
      -- ^ set by the @with@ statement during execution of a Dao script. It is used to prefix this
      -- to all global-dot references before reading from or writing to those references.
    , importsTable       :: [(Name, ExecUnit)]
      -- ^ a pointer to the ExecUnit of every Dao program imported with the @import@ keyword.
    , execStack          :: LocalStore
      -- ^ stack of local variables used during evaluation
    , globalData         :: GlobalStore
    , providedAttributes :: T.Tree Name ()
    , builtinFunctions   :: M.Map Name DaoFunc
      -- ^ global variables cleared after every string execution
    , execOpenFiles      :: IORef (M.Map UPath ExecUnit)
    , uncaughtErrors     :: IORef [Object]
    , programModuleName  :: Maybe UPath
    , programImports     :: [UPath]
    , preExec            :: [Subroutine]
      -- ^ the "guard scripts" that are executed before every string execution.
    , postExec           :: [Subroutine]
      -- ^ the "guard scripts" that are executed after every string execution.
    , quittingTime       :: [Subroutine]
    , programTokenizer   :: Object -- TODO: needs to be set after evaluating module top-level
    , ruleSet            :: IORef (PatternTree Object [Subroutine])
    , lambdaSet          :: IORef [CallableCode]
    }

-- not for export -- initializes a completely empty 'ExecUnit' and returns it wrapped up in a
-- 'ParentExecUnit'.
initExecUnit :: IO ExecUnit
initExecUnit = do
  paths    <- newMVar mempty
  igraph   <- newMVar mempty
  unctErrs <- newIORef []
  global   <- newMVar M.empty
  execTask <- initTask
  xstack   <- newIORef emptyStack
  files    <- newIORef M.empty
  rules    <- newIORef T.Void
  lambdas  <- newIORef []
  return $
    ExecUnit
    { globalMethodTable  = mempty
    , pathIndex          = paths
    , defaultTimeout     = Nothing
    , importGraph        = igraph
    , currentWithRef     = WithRefStore Nothing
    , currentQuery       = Nothing
    , currentPattern     = Nothing
    , currentCodeBlock   = StaticStore Nothing
    , currentBranch      = []
    , importsTable       = []
    , globalData         = GlobalStore global
    , providedAttributes = T.Void
    , builtinFunctions   = M.empty
    , taskForExecUnits   = execTask
    , execStack          = LocalStore xstack
    , execOpenFiles      = files
    , uncaughtErrors     = unctErrs
      ---- items that were in the Program data structure ----
    , programModuleName = Nothing
    , programImports    = []
    , preExec           = []
    , quittingTime      = mempty
    , programTokenizer  = ONull
--    , programComparator = (==)
    , postExec          = []
    , ruleSet           = rules
    , lambdaSet         = lambdas
    }

-- | Creates a new 'ExecUnit'. This is the only way to create a new 'ExecUnit', and it must be run
-- within the 'Exec' monad. The 'ExecUnit' produced by this function will have it's parent
-- 'ExecUnit' set to the value returned by the 'Control.Monad.Reader.Class.ask' instance of the
-- 'Exec' monad.
--
-- The parent of all other 'ExecUnit's, the root of the family tree, is initalized internally by the
-- 'startDao' function.
newExecUnit :: Maybe UPath -> Exec ExecUnit
newExecUnit modName = ask >>= \parent -> liftIO initExecUnit >>= \child -> return $
  child
  { programModuleName = modName
  , builtinFunctions  = builtinFunctions  parent
  , defaultTimeout    = defaultTimeout    parent
  , globalMethodTable = globalMethodTable parent
  }

----------------------------------------------------------------------------------------------------

-- | A 'Task' is simply a group of threads executing in parallel, but evaluating a task is still
-- synchronous, i.e. evaluating 'taskLoop' on a 'Task' will block until every thread in the task has
-- completed.
data Task
  = Task
    { taskWaitChan       :: Chan (ThreadId, Int)
    , taskRunningThreads :: MVar (S.Set ThreadId)
    }

-- | Create a new 'Task'.
initTask :: IO Task
initTask = do
  wait    <- newChan
  running <- newMVar S.empty
  return $ Task{ taskWaitChan=wait, taskRunningThreads=running }

-- | To halt a single thread in a 'Task', simply signal it with 'Control.Concurrent.killThread'. But
-- to halt everything the task is doing, use this function. Use of this function will never result in
-- deadlocks (I hope).
throwToTask :: Exception e => Task -> e -> IO ()
throwToTask task e = do
  let mvar = taskRunningThreads task
  ((S.elems <$> readMVar mvar) >>= mapM_ (flip throwTo e))
    `finally` getChanContents (taskWaitChan task) >> return ()

-- | Like 'throwToTask', but throws 'Control.Exception.ThreadKilled'.
killTask :: Task -> IO ()
killTask = flip throwToTask ThreadKilled

-- | This is a better way to manage a 'Task' because all tasks evaluated are waited for
-- synchronously, but you can provide a callback that is evaluated after each task completes. This
-- prevents exceptions from occurring, for example:
-- > "thread blocked indefinitely in an MVar operation"
-- 
-- Provide a list of IO functions to be evaluated in parallel. Also provide a callback function
-- that will be evaluated after each thread completes. This function should take two parameters and
-- return a bool: the 'Control.Concurrent.ThreadId' of the thread that completed and a positive
-- integer value indicating the number of threads that are still running, and the bool returned
-- should indicate whether or not the loop should continue. If you should halt the loop by returning
-- 'Prelude.False', the threads in the task that are still running will continue running, and you
-- should call 'killTask' after 'taskLoop' to halt them if halting them should be necessary.
-- 
-- This function is also exception safe. All tasks evaluated in parallel will not fail to singal the
-- callback, even if the thread halts with an exception or asynchronous signal from a function like
-- 'Control.Concurrent.killThread'. If the thread evaluating this function is halted by an
-- exception, all threads in the 'Task' are also killed.
taskLoop :: Task -> [IO ()] -> (ThreadId -> Int -> IO Bool) -> IO ()
taskLoop task parallelIO threadHaltedEvent = unless (null parallelIO) $
  (do mapM_ (forkInTask task) parallelIO
      fix $ \loop -> waitFirst task >>= \ (thread, remain) ->
        threadHaltedEvent thread remain >>= \contin -> unless (not contin || remain==0) loop
  ) `onException` killTask task
  where
    waitFirst :: Task -> IO (ThreadId, Int)
    waitFirst task = readChan (taskWaitChan task)
    forkInTask :: Task -> IO () -> IO ThreadId
    forkInTask task run = forkIO $ do
      self <- myThreadId
      bracket
        (modifyMVar (taskRunningThreads task) $ \s' -> do
            let s = S.delete self s'
            return (s, S.size s)
        )
        (\i -> writeChan (taskWaitChan task) (self, i))
        (\ _i -> run)

-- | Works exactly like 'taskLoop', except you do not need to provide a callback function to be
-- evaluated after every task completes. Essentially, every IO function is evaluated in the 'Task'
-- in parallel, and this function blocks until all tasks have completed.
taskLoop_ :: Task -> [IO ()] -> IO ()
taskLoop_ task inits = taskLoop task inits (\ _ _ -> return True)

----------------------------------------------------------------------------------------------------

-- | This simple, humble little class is one of the most important in the Dao program because it
-- defines the 'execute' function. Any data type that can result in procedural execution in the
-- 'Exec' monad can instantiate this class. This will allow the instnatiated data type to be used as
-- a kind of executable code that can be passed around and evaluated at arbitrary points in your Dao
-- program.
-- 
-- Note that there the @result@ type parameter is functionally dependent on the @exec@ type
-- parameter. This guarantees there is a one-to-one mapping from independent @exec@ types to
-- dependent @result@ types, i.e. if you data type @MyDat@ maps to a data type @Rzlt@, then @Rzlt@
-- is the only possible data type that could ever be evaluated by 'execute'-ing the @MyDat@
-- function.
--
-- As a reminder, functional dependencies do not necessitate a one-to-one mapping from the
-- dependent type to the independent type, so the @result@ parameter may be the same for many
-- different @exec@ types. But once the compiler infers that the @exec@ parameter of the 'Executable'
-- class is @MyDat@, the @result@ type /must/ be @Rzlt@ and nothing else.
-- > instance Executable MyDat Rzlt
-- > instance Executable A     () -- OK (different @exec@ parameters, same @result@ parameters)
-- > instance Executable B     () -- OK
-- > instance Executable C     () -- OK
-- > 
-- > instance Executable D     ()   -- COMPILER ERROR (same @exec@ parameters, different @result@ parameters)
-- > instance Executable D     Int  -- COMPILER ERROR
-- > instance Executable D     Char -- COMPILER ERROR
-- In this example, should D instantiate () or Int or Char as it's result? You must choose only one.
class Executable exec result | exec -> result where { execute :: exec -> Exec result }

-- 'execute'-ing a 'QualRefExpr' will dereference it, essentially reading the
-- value associated with that reference from the 'ExecUnit'.
instance Executable QualRef (Maybe Object) where
  execute qref = _doWithRefQualifier qref getLocal getStatic getGlobal getGloDot where
    getLocal  ref = asks execStack  >>= doLookup ref
    getGlobal ref = asks globalData >>= doLookup ref
    getStatic ref = asks currentCodeBlock >>= doLookup ref
    getGloDot ref = asks currentWithRef >>= doLookup ref
    doLookup :: Store store => Reference -> store -> Exec (Maybe Object)
    doLookup (Reference rx) store = case rx of
      []   -> mzero
      r:rx -> do
        top <- storeLookup store r >>= maybe mzero return
        _objectAccess [r] rx top
      -- TODO: on exception, update the exception structure with information about the 'QualRef'
      -- given above.

-- | This function performs an update on a 'QualRef', it is the complement to the instantiation of
-- 'QualRef' in the 'Executable' monad, that is to say evaluating 'execute' on a 'QualRef' will
-- "read" the value associated with it, evaluating 'qualRefUpdate' on a 'QualRef' will write/update
-- the value associated with it.
qualRefUpdate :: QualRef -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
qualRefUpdate qref upd = _doWithRefQualifier qref onLocal onStatic onGlobal onGloDot where
  onLocal  ref = asks execStack >>= doUpdate ref
  onGlobal ref = asks globalData >>= doUpdate ref
  onStatic ref = asks currentCodeBlock >>= doUpdate ref
  onGloDot ref = asks currentWithRef >>= doUpdate ref
  doUpdate :: Store store => Reference -> store -> Exec (Maybe Object)
  doUpdate (Reference rx) store = case rx of
    []   -> mzero
    r:rx -> storeUpdate store r $ _objectUpdate upd [r] rx
      -- TODO: on exception, update the exception structure with information about the 'QualRef'
      -- given above.

-- Pass a 'QualRef' and four functions, a function to be evaluated in the case of a 'LOCAL' ref, a
-- function to be evaluated in case of a 'STATIC' ref, a function to be evaluated in the case of a
-- 'GLOBAL' ref, and a function to be evaluated in the case of a 'GLODOT' ref. Based on the
-- 'RefQualifier' of the 'QualRef', the appropriate actions are evaluated. In the case of an
-- 'Unqualified' reference, the actions associated with 'LOCAL', 'STATIC', and 'GLOBAL' qualifiers
-- are evaluated in that order, so these functions should all evaluated to 'Control.Monad.mzero' if
-- the 'Reference' is not defined.
_doWithRefQualifier
  :: QualRef
  -> (Reference -> Exec (Maybe Object)) -- local store
  -> (Reference -> Exec (Maybe Object)) -- static store
  -> (Reference -> Exec (Maybe Object)) -- global store
  -> (Reference -> Exec (Maybe Object)) -- with-ref store
  -> Exec (Maybe Object)
_doWithRefQualifier qref onLocal onStatic onGlobal onGloDot = do
  result <- catchPredicate $ msum $ case qref of
    Unqualified ref -> [onLocal ref, onStatic ref, onGlobal ref]
    Qualified q ref -> case q of
      LOCAL  -> [onLocal ref]
      STATIC -> [onStatic ref]
      GLOBAL -> [onGlobal ref]
      GLODOT -> [onGloDot ref, onGlobal ref]
  case result of
    Backtrack -> return Nothing
    PFail err -> throwError err
    OK      o -> return o

----------------------------------------------------------------------------------------------------

-- | Since the 'ExecUnit' deals with a few different kinds of pointer values, namely
-- 'Data.IORef.IORef' and 'MVar', which all have similar functions for reading and updating, I have
-- defined this class to provide a consistent set of functions for working with the various pointers
-- data types.
class ExecRef var where
  execReadRef    :: var a -> Exec a
  execTakeRef    :: var a -> Exec a
  execPutRef     :: var a -> a -> Exec ()
  execSwapRef    :: var a -> a -> Exec a
  execModifyRef  :: var a -> (a -> Exec (a, b)) -> Exec b
  execModifyRef_ :: var a -> (a -> Exec  a    ) -> Exec ()
  execModifyRef_ var upd = execModifyRef var (\a -> upd a >>= \a -> return (a, ()))

instance ExecRef MVar where
  execModifyRef mvar upd = do
    xunit <- ask
    (>>=predicate) $ liftIO $ modifyMVar mvar $ \var -> do
      result <- flip ioExec xunit $ execCatchIO (upd var) $
        [ newExecIOHandler $ \e -> execThrow $ obj $
            [obj "ErrorCall:", obj (show (e::ErrorCall))]
        , newExecIOHandler $ \e -> execThrow $ obj $
            [obj "IOException:" , obj (show (e::IOException))]
        ]
      return $ case result of
        Backtrack   -> (var, Backtrack )
        OK (var, o) -> (var, OK       o)
        PFail   err -> (var, PFail  err)
  execModifyRef_ mvar upd = execModifyRef mvar (\var -> upd var >>= \var -> return (var, ()))
  execReadRef      = liftIO . readMVar
  execTakeRef      = liftIO . takeMVar
  execPutRef  mvar = liftIO . putMVar  mvar
  execSwapRef mvar = liftIO . swapMVar mvar

instance ExecRef IORef where
  execModifyRef  ref upd = liftIO (readIORef ref) >>= upd >>= \ (var, b) -> liftIO (writeIORef ref var) >> return b
  execModifyRef_ ref upd = liftIO (readIORef ref) >>= upd >>= liftIO . writeIORef ref
  execReadRef            = liftIO . readIORef
  execTakeRef            = execReadRef
  execPutRef     ref     = liftIO . writeIORef ref
  execSwapRef    ref obj = liftIO (readIORef ref >>= \sw -> writeIORef ref obj >> return sw)

----------------------------------------------------------------------------------------------------

class Store store where
  storeLookup :: store -> Name -> Exec (Maybe Object)
  storeUpdate :: store -> Name -> (Maybe Object -> Exec (Maybe Object)) -> Exec (Maybe Object)
  storeDefine :: store -> Name -> Object -> Exec ()
  storeDefine store nm o = storeUpdate store nm (return . const (Just o)) >> return ()
  storeDelete :: store -> Name -> Exec ()
  storeDelete store nm = storeUpdate store nm (return . const Nothing) >> return ()

instance ExecRef ref => Store (ref T_dict) where
  storeLookup store ref     = fmap (M.lookup ref    ) (execReadRef    store)
  storeDefine store ref obj = execModifyRef_ store (return . M.insert ref obj)
  storeDelete store ref     = execModifyRef_ store (return . M.delete ref    )
  storeUpdate store ref upd = execModifyRef  store $ \tree -> do
    obj <- upd (M.lookup ref tree)
    return $ case obj of
      Nothing  -> (M.delete ref     tree, Nothing)
      Just obj -> (M.insert ref obj tree, Just obj)

instance ExecRef ref => Store (ref (Stack Name Object)) where
  storeLookup store ref     = execReadRef store >>= return . stackLookup ref
  storeDefine store ref obj = execModifyRef_ store (return . stackDefine ref (Just obj))
  storeDelete store ref     = execModifyRef_ store (return . stackDefine ref Nothing)
  storeUpdate store ref upd = execModifyRef  store (stackUpdateTopM upd ref)

newtype LocalStore  = LocalStore  (IORef (Stack Name Object))

instance Store LocalStore where
  storeLookup (LocalStore  store) = storeLookup store
  storeDefine (LocalStore  store) = storeDefine store
  storeDelete (LocalStore  store) = storeDelete store
  storeUpdate (LocalStore  store) = storeUpdate store

newtype GlobalStore = GlobalStore (MVar T_dict)

instance Store GlobalStore where
  storeLookup (GlobalStore store) = storeLookup store
  storeDefine (GlobalStore store) = storeDefine store
  storeDelete (GlobalStore store) = storeDelete store
  storeUpdate (GlobalStore store) = storeUpdate store

newtype StaticStore = StaticStore (Maybe Subroutine)

instance Store StaticStore where
  storeLookup (StaticStore store) ref     =
    maybe (return Nothing) (\store -> storeLookup store ref    ) (fmap staticVars store)
  storeDefine (StaticStore store) ref obj =
    maybe (return ())      (\store -> storeDefine store ref obj) (fmap staticVars store)
  storeDelete (StaticStore store) ref     =
    maybe (return ())      (\store -> storeDelete store ref    ) (fmap staticVars store)
  storeUpdate (StaticStore store) ref upd =
    maybe (return Nothing) (\store -> storeUpdate store ref upd) (fmap staticVars store)

newtype WithRefStore = WithRefStore (Maybe (IORef Object))

_withRefStore :: WithRefStore -> (IORef Object -> Exec b) -> Exec b
_withRefStore (WithRefStore o) upd = maybe mzero upd o

instance Store WithRefStore where
  storeLookup sto ref     = _withRefStore sto (liftIO . readIORef >=> _objectAccess [] [ref])
  storeUpdate sto ref upd = _withRefStore sto $ \sto -> do
    o <- liftIO (readIORef sto) >>= _objectUpdate upd [] [ref] . Just
    liftIO $ writeIORef sto $ maybe ONull id o
    return o

-- | Read elements within a Dao 'Struct' with a 'Reference'. 
objectAccess :: Reference -> Object -> Exec (Maybe Object)
objectAccess (Reference rx) o = _objectAccess [] rx o

-- Read elements within a Dao 'Struct' with a 'Reference'. The 'QualRef' parameter is only for
-- error reporting purposes, passing 'nullValue' will not cause any problems. The first
-- @['Dao.String.Name']@ parameter is the current path, the second @['Dao.String.Name']@ parameter
-- is the reference to be accessed.
_objectAccess :: [Name] -> [Name] -> Object -> Exec (Maybe Object)
_objectAccess back rx o = Just <$> loop back rx o <|> return Nothing where
  loop :: [Name] -> [Name] -> Object -> Exec Object
  loop back rx o = case rx of
    []   -> return o
    r:rx -> case o of
      ODict   o -> dictLookup   back r rx o
      OTree   o -> structLookup back r rx o
      OHaskell (HaskellData o ifc) ->
        maybe mzero (\read -> read o >>= structLookup back r rx) (objToStruct ifc)
      _         -> mzero
  structLookup back r rx o = case o of
    Struct{ fieldMap=o } -> dictLookup back r rx o
    Nullary{} -> mzero
  dictLookup back r rx o = maybe mzero (loop (back++[r]) rx) (M.lookup r o)

-- | Update elements within a Dao 'Struct' with a 'Reference'.
objectUpdate :: (Maybe Object -> Exec (Maybe Object)) -> Reference -> Maybe Object -> Exec (Maybe Object)
objectUpdate upd (Reference rx) o = _objectUpdate upd [] rx o

-- Update elements within a Dao 'Struct' with a 'Reference'. The first @['Dao.String.Name']@
-- parameter is the current path, the second @['Dao.String.Name']@ parameter is the reference to be
-- accessed.
_objectUpdate
  :: (Maybe Object -> Exec (Maybe Object))
  -> [Name] -> [Name] -> Maybe Object -> Exec (Maybe Object)
_objectUpdate upd back rx o = loop back rx o where
  atRef = obj . Unqualified . Reference
  loop back rx o = case o of
    Nothing -> upd Nothing >>= maybe (return Nothing) (return . Just . insertAtPath rx)
    Just  o -> case rx of
      []   -> upd (Just o)
      r:rx -> updObj back r rx o
  putBack constr = maybe (return Nothing) (return . Just . constr)
  updObj back r rx o = case o of
    OTree   o -> updStruct back r rx o >>= putBack OTree
    ODict   o -> updDict   back r rx o >>= putBack ODict
    OHaskell (HaskellData o ifc) -> case pure (,) <*> objToStruct ifc <*> objFromStruct ifc of
      Just (toStruct, fromStruct) -> toStruct o >>= updStruct back r rx >>=
        maybe (return Nothing) (fmap (Just . OHaskell . flip HaskellData ifc) . fromStruct)
      Nothing -> execThrow $ obj $
        [ obj "cannot update opaque data type"
        , obj $ Unqualified $ Reference back
        ]
    o -> execThrow $ obj $
      [ obj "cannot update atomic data type", obj o
      , obj "at reference", atRef back
      ]
  updStruct back r rx o = case o of
    Nullary{ structName=name } -> execThrow $ obj $
      [ obj "on structure", obj name
      , obj "no element named", atRef $ back++[r]
      ]
    Struct{ fieldMap=inner } -> updDict back r rx inner >>=
      maybe (return Nothing) (\inner -> return $ Just $ o{ fieldMap=inner })
  updDict   back r rx o = do
    o <- (\item -> M.alter (const item) r o) <$> loop (back++[r]) rx (M.lookup r o)
    return (if M.null o then Nothing else Just o)

----------------------------------------------------------------------------------------------------

-- | This data type is use to halt normal evaluation and force the result of evaluating the code to
-- be a particular value of this type. The 'Exec' monad instantiates
-- 'Control.Monad.Error.Class.MonadError' such that 'Control.Monad.Error.Class.throwError' throws a
-- value of this type. However, it is not only used for exceptions. The Dao scripting language's
-- "return" statement throws an 'ExecReturn' value which is caught using
-- 'Control.Monad.Error.Class.catchError' when evaluating function calls.
data ExecControl
  = ExecReturn { execReturnValue :: Maybe Object }
  | ExecError
    { execReturnValue :: Maybe Object
    , execErrorInfo   :: ExecErrorInfo
    }
  deriving Typeable

data ExecErrorInfo
  = ExecErrorInfo
    { execUnitAtError :: Maybe ExecUnit
    , execErrExpr     :: Maybe ObjectExpr
    , execErrScript   :: Maybe ScriptExpr
    , execErrTopLevel :: Maybe TopLevelExpr
    }
  deriving Typeable

mkExecErrorInfo :: ExecErrorInfo
mkExecErrorInfo = ExecErrorInfo Nothing Nothing Nothing Nothing

mkExecError :: ExecControl
mkExecError = ExecError Nothing mkExecErrorInfo

-- | Evaluate an 'Exec', but if it throws an exception, set record an 'ObjectExpr' where
-- the exception occurred in the exception information.
updateExecErrorInfo :: (ExecErrorInfo -> ExecErrorInfo) -> Exec a -> Exec a
updateExecErrorInfo upd fn = catchError fn $ \err -> case err of
  ExecReturn{} -> throwError err
  ExecError{ execErrorInfo=info } -> throwError $ err{ execErrorInfo = upd info }

instance HasNullValue ExecControl where
  nullValue = ExecReturn Nothing
  testNull (ExecReturn Nothing) = True
  testNull  _                   = False

instance PPrintable ExecControl where
  pPrint err = case err of 
    ExecError{ execReturnValue=o, execErrorInfo=info } -> maybe (return ()) pperr o where
      fileName = execUnitAtError info >>= programModuleName
      apLabel which label =
        fmap (\o -> (pInline [pString label, pString " ", pPrint o], getLocation o)) (which info)
      errInfo = msum
        [ apLabel execErrExpr     "in expression" 
        , apLabel execErrScript   "in statement"
        , apLabel execErrTopLevel "in top-level directive"
        ]
      unquot o = case o of
        OString o -> pUStr o
        o         -> pPrint o
      pplist o = case o of
        OList   o -> pInline $ intersperse (pString " ") (map unquot o)
        o         -> unquot o
      ppmap o = case o of
        ODict o -> forM_ (M.assocs o) $ \ (ref, o) -> pWrapIndent [pPrint ref, pplist o]
        o -> pplist o
      pperr o = do
        pWrapIndent $
          [ case errInfo of
              Nothing -> pString "Error: "
              Just  o -> pString $ concat $
                [maybe "" ((++":") . uchars) fileName , show (snd o) ++ ": "]
          , ppmap o
          ]
        pEndLine
        maybe (return ()) fst errInfo
    ExecReturn{ execReturnValue=o } ->
      maybe (return ()) (\o -> pWrapIndent [pString "Evaluated to: ", pPrint o]) o

instance ToDaoStructClass ExecControl Object where
  toDaoStruct = ask >>= \o -> case o of
    ExecReturn a -> flip (maybe (void $ makeNullary "ExecReturn")) a $ \a ->
      renameConstructor "ExecReturn" >> void ("value" .= a)
    ExecError o (ExecErrorInfo _ a b c) -> void $ renameConstructor "ExecError" >>
      "value" .=? o >> "objectExpr" .=? a >> "scriptExpr" .=? b >> "topLevelExpr" .=? c

instance FromDaoStructClass ExecControl Object where
  fromDaoStruct = msum $
    [ constructor "ExecReturn" >> ExecReturn <$> opt "value"
    , do  constructor "ExecError"
          pure ExecError <*> opt "value" <*>
            (pure ExecErrorInfo
              <*> pure Nothing
              <*> opt "objectExpr"
              <*> opt "scriptExpr"
              <*> opt "topLevelExpr"
            )
    ]

instance HaskellDataClass ExecControl where
  haskellDataInterface = interface nullValue $ do
    autoDefPPrinter
    -- autoDefNullTest >> autoDefToStruct

setCtrlReturnValue :: Object -> ExecControl -> ExecControl
setCtrlReturnValue obj ctrl = case ctrl of
  ExecReturn{}   -> ExecReturn (Just obj)
  ExecError{ execErrorInfo=info } -> ExecError{ execReturnValue=Just obj, execErrorInfo=info }

_setErrorInfoExpr :: (ExecErrorInfo -> ExecErrorInfo) -> Exec a -> Exec a
_setErrorInfoExpr upd exec = catchPredicate exec >>= \a -> case a of
  OK      a -> return a
  Backtrack -> mzero
  PFail err -> case err of
    ExecReturn{} -> throwError err
    ExecError{ execErrorInfo=info } -> throwError $ err{ execErrorInfo=upd info }

_setObjectExprError :: ObjectExpr -> Exec (Maybe Object) -> Exec (Maybe Object)
_setObjectExprError o = _setErrorInfoExpr (\info -> info{ execErrExpr=Just o })

_setScriptExprError :: ScriptExpr -> Exec () -> Exec ()
_setScriptExprError o = _setErrorInfoExpr (\info -> info{ execErrScript=Just o })

_setTopLevelExprError :: TopLevelExpr -> Exec (ExecUnit -> ExecUnit) -> Exec (ExecUnit ->ExecUnit)
_setTopLevelExprError o = _setErrorInfoExpr (\info -> info{ execErrTopLevel=Just o })

----------------------------------------------------------------------------------------------------

-- | All evaluation of the Dao language takes place in the 'Exec' monad. It instantiates
-- 'Control.Monad.MonadIO.MonadIO' to allow @IO@ functions to be lifeted into it. It instantiates
-- 'Control.Monad.Error.MonadError' and provides it's own exception handling mechanism completely
-- different from the Haskell runtime, so as to allow for more control over exception handling in
-- the Dao runtime.
newtype Exec a  = Exec{ execToPredicate :: PredicateT ExecControl (ReaderT ExecUnit IO) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance MonadReader ExecUnit Exec where
  local upd (Exec (PredicateT fn)) = Exec $ PredicateT (local upd fn)
  ask                              = Exec $ lift ask

instance MonadError ExecControl Exec where
  throwError = Exec . throwError
  catchError (Exec try) catch = Exec (catchError try (execToPredicate . catch))

instance MonadPlusError ExecControl Exec where
  catchPredicate (Exec f) = Exec (catchPredicate f)
  predicate = Exec . predicate

----------------------------------------------------------------------------------------------------

class ExecThrowable o where
  toExecError :: o -> ExecControl
  -- | Like 'Prelude.error' but works for the 'Exec' monad, throws an 'ExecControl' using
  -- 'Control.Monad.Error.throwError' constructed using the given 'Object' value as the
  -- 'execReturnValue'.
  execThrow :: ExecThrowable o => o -> Exec ig
  execThrow o = ask >>= \xunit -> throwError $
    let err = toExecError o
    in  err{execErrorInfo=(execErrorInfo err){execUnitAtError=Just xunit}}

instance ExecThrowable Object where
  toExecError err = setCtrlReturnValue err mkExecError

instance ExecThrowable ExecControl where { toExecError = id }

ioExec :: Exec a -> ExecUnit -> IO (Predicate ExecControl a)
ioExec func xunit = runReaderT (runPredicateT (execToPredicate func)) xunit

----------------------------------------------------------------------------------------------------

newtype ExecHandler a = ExecHandler { execHandler :: ExecUnit -> Handler (Predicate ExecControl a) }

instance Functor ExecHandler where { fmap f (ExecHandler h) = ExecHandler (fmap (fmap (fmap f)) h) }

-- | Create an 'ExecHandler'.
newExecIOHandler :: Exception e => (e -> Exec a) -> ExecHandler a
newExecIOHandler h = ExecHandler (\xunit -> Handler (\e -> ioExec (h e) xunit))

-- | Using an 'ExecHandler' like 'execIOHandler', catch any exceptions thrown by the Haskell
-- language runtime and wrap them up in the 'Exec' monad.
execCatchIO :: Exec a -> [ExecHandler a] -> Exec a
execCatchIO tryFunc handlers = do
  xunit <- ask
  ctrl  <- liftIO $ catches (ioExec tryFunc xunit) (fmap (\h -> execHandler h xunit) handlers)
  predicate ctrl

-- | Like 'execCatchIO' but with the arguments 'Prelude.flip'ped.
execHandleIO :: [ExecHandler a] -> Exec a -> Exec a
execHandleIO = flip execCatchIO

-- | An 'ExecHandler' for catching 'Control.Exception.ErrorCall's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execIOHandler :: ExecHandler ()
execIOHandler = newExecIOHandler $ \e -> execThrow (obj $ show (e::IOException))

-- | An 'ExecHandler' for catching 'Control.Exception.ErrorCall's and re-throwing them to the
-- 'Procedural' monad using 'Control.Monad.Error.throwError', allowing the exception to be caught
-- and handled by Dao script code.
execErrorHandler :: ExecHandler ()
execErrorHandler = newExecIOHandler $ \e -> execThrow (obj $ show (e::ErrorCall))

catchReturn :: (Maybe Object -> Exec a) -> Exec a -> Exec a
catchReturn catch f = catchPredicate f >>= \pval -> case pval of
  PFail (ExecReturn a) -> catch a
  pval                 -> predicate pval

----------------------------------------------------------------------------------------------------
-- $StackOperations
-- Operating on the local stack.

-- | Push a new empty local-variable context onto the stack. Does NOT 'catchReturnObj', so it can be
-- used to push a new context for every level of nested if/else/for/try/catch statement, or to
-- evaluate a macro, but not a function call. Use 'execFuncPushStack' to perform a function call within
-- a function call.
execNested :: forall a . T_dict -> Exec a -> Exec a
execNested init exe = do
  (LocalStore stack) <- asks execStack
  liftIO $ modifyIORef stack (stackPush init)
  result <- exe
  liftIO $ modifyIORef stack (fst . stackPop)
  return result

-- | Keep the current 'execStack', but replace it with a new empty stack before executing the given
-- function. This function is different from 'nestedExecStak' in that it acually removes the current
-- execution stack so a function call cannot modify the local variables of the function which called
-- it. Furthermore it catches evaluation of a "return" statement allowing the function which called
-- it to procede with execution after this call has returned.
execFuncPushStack :: T_dict -> Exec (Maybe Object) -> Exec (Maybe Object)
execFuncPushStack dict exe = do
  pval <- catchPredicate $ do
    stack <- liftIO (newIORef emptyStack)
    local (\xunit -> xunit{execStack=LocalStore stack}) (execNested dict exe)
  case pval of
    OK                obj  -> return obj
    Backtrack              -> mzero
    PFail (ExecReturn obj) -> return obj
    PFail             err  -> throwError err

----------------------------------------------------------------------------------------------------

-- | Elements of the symantic data structures that instantiate 'Executable' and do not instantiate
-- 'Dao.PPrint.PPrintable', 'Dao.Struct.Structured', or any parsers. Elements of the abstract syntax
-- tree (AST) instantiate 'Dao.PPrint.PPrintable', 'Dao.Struct.Structured', and all of the parsers,
-- but are not executable and do not instantiate 'Executable'. This separates concerns pretty well,
-- but leaves us with the problem of having to convert back and forth between these various data
-- types.
--
-- The 'Intermediate' class allows us to declare a one-to-one relationship between AST types and
-- executable types. For example, 'ObjectExpr' is the intermediate representation of
-- 'AST_Object', so our instance for this relationship is @instane 'Intermediate'
-- 'ObjectExpr' 'AST_Object'@.
class Intermediate obj ast | obj -> ast, ast -> obj where
  toInterm   :: ast -> [obj]
  fromInterm :: obj -> [ast]
  -- | The default implementation is to convert an @ast@ to an @[obj]@ using 'toInterm' and then
  -- immediately convert the @[obj]@ back to an @[ast]@ using 'fromInterm'.
  canonicalize :: ast -> [ast]
  canonicalize ast = toInterm ast >>= fromInterm

instance Intermediate Name Name where { toInterm = return; fromInterm = return; }

-- Not for export: here are a bunch of shortcuts to converting the AST to the intermediate data
-- type. Sinec 'toInterm' returns a single item in a list to indicate success and an empty list to
-- indicate failure, all of these items have their evaluated type wrapped in a list type. This is to
-- allow the 'toInterm' instances use the 'Control.Monad.liftM' family of functions.
ti :: Intermediate obj ast => ast -> [obj]
ti = toInterm
uc :: Com a -> [a]
uc = return . unComment
uc0 :: Intermediate obj ast =>  Com ast  -> [obj]
uc0 = toInterm . unComment
uc1 :: Intermediate obj ast => [Com ast] -> [[obj]]
uc1 = return . concatMap (toInterm . unComment)
uc2 :: Intermediate obj ast => Com [Com ast] -> [[obj]]
uc2 = uc1 . unComment
um0 :: Maybe (Com a) -> [Maybe a]
um0 = maybe [Nothing] (return . Just . unComment)
um1 :: Intermediate obj ast => Maybe ast -> [Maybe obj]
um1 = maybe [Nothing] (fmap Just . toInterm)
um2 :: Intermediate obj ast => Maybe (Com ast) -> [Maybe obj]
um2 = maybe [Nothing] (fmap Just . toInterm . unComment)

fi :: Intermediate obj ast => obj -> [ast]
fi = fromInterm
nc :: a -> [Com a]
nc = return . Com
nc0 :: Intermediate obj ast => obj -> [Com ast]
nc0 = fmap Com . fromInterm
nc1 :: Intermediate obj ast => [obj] -> [[Com ast]]
nc1 = return . map Com . concatMap fromInterm
nc2 :: Intermediate obj ast => [obj] -> [Com [Com ast]]
nc2 = fmap Com . nc1
nm0 :: Maybe a -> [Maybe (Com a)]
nm0 = maybe [Nothing] (return . Just . Com)
nm1 :: Intermediate obj ast => Maybe obj -> [Maybe ast]
nm1 = maybe [Nothing] (fmap Just . fromInterm)
nm2 :: Intermediate obj ast => Maybe obj -> [Maybe (Com ast)]
nm2 = maybe [Nothing] (fmap (Just . Com) . fromInterm)

ll :: Location -> [Location]
ll = return

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is pretty-printable ('Dao.PPrint.PPrintable').
pPrintInterm :: (Intermediate o ast, PPrintable ast) => o -> PPrint
pPrintInterm = mapM_ pPrint . fromInterm

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is 'Dao.Binary.GPut'.
putAST :: (Intermediate obj ast, B.Binary obj mtab) => ast -> B.GPut mtab
putAST ast = case toInterm ast of
    [obj] -> B.put obj
    _     -> fail "binary encoder could not convert AST to intermediate expression"

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is 'Dao.Binary.GGet'.
getAST :: (Intermediate obj ast, B.Binary obj mtab) => B.GGet mtab ast
getAST = B.get >>= \obj -> case fromInterm obj of
    [ast] -> return ast
    _     -> fail "binary decoder constructed object that could not be converted to an AST representation"

----------------------------------------------------------------------------------------------------

-- | Comments in the Dao language are not interpreted, but they are not disgarded either. Dao is
-- intended to manipulate natural language, and itself, so that it can "learn" new semantic
-- structures. Dao scripts can manipulate the syntax tree of other Dao scripts, and so it might be
-- helpful if the syntax tree included comments.
data Comment
  = InlineComment  UStr
  | EndlineComment UStr
  deriving (Eq, Ord, Typeable, Show)

commentString :: Comment -> UStr
commentString com = case com of
  InlineComment  a -> a
  EndlineComment a -> a

instance NFData Comment where
  rnf (InlineComment  a) = seq a ()
  rnf (EndlineComment a) = seq a ()

instance HasNullValue Comment where
  nullValue = EndlineComment nil
  testNull (EndlineComment c) = c==nil
  testNull (InlineComment  c) = c==nil

instance PPrintable Comment where
  pPrint com = do
    case com of
      EndlineComment c -> pString ("//"++uchars c) >> pForceNewLine
      InlineComment  c -> pGroup True $ pInline $
        concat [[pString " /*"], map pString (lines (uchars c)), [pString "*/ "]]

instance PrecedeWithSpace a => PrecedeWithSpace (Com a) where
  precedeWithSpace o = case o of
    Com         b   -> precedeWithSpace b
    ComBefore a b   -> precedeWithSpace a || precedeWithSpace b
    ComAfter    b _ -> precedeWithSpace b
    ComAround a b _ -> precedeWithSpace a || precedeWithSpace b
    -- there should always be a space before a comment.

instance ToDaoStructClass Comment Object where
  toDaoStruct = let nm = renameConstructor in ask >>= \co -> void $ case co of
    InlineComment  o -> nm "InlineComment"  >> "comment" .= o
    EndlineComment o -> nm "EndlineComment" >> "comment" .= o

instance FromDaoStructClass Comment Object where
  fromDaoStruct = msum $
    [ constructor "InlineComment"  >> InlineComment  <$> req "comment"
    , constructor "EndlineComment" >> EndlineComment <$> req "comment"
    ]

instance ObjectClass Comment where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass Comment where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

instance PPrintable [Comment] where { pPrint = mapM_ pPrint }

instance PrecedeWithSpace [Comment] where { precedeWithSpace = not . null }

instance HasRandGen [Comment] where { randO = return [] }
--  randO = do
--    i0 <- randInt
--    let (i1, many) = divMod i0 4
--        (i2, typn) = divMod i1 16
--        typx = take many (randToBase 2 typn ++ replicate 4 0)
--        lenx = map (+1) (randToBase 29 i2)
--        com typ = if typ==0 then EndlineComment else InlineComment
--    forM (zip typx lenx) $ \ (typ, len) ->
--      fmap (com typ . ustr . unwords . map (B.unpack . getRandomWord)) (replicateM len randInt)

instance ObjectClass [Comment] where { obj=listToObj; fromObj=listFromObj; }

----------------------------------------------------------------------------------------------------

-- | Symbols in the Dao syntax tree that can actually be manipulated can be surrounded by comments.
-- The 'Com' structure represents a space-efficient means to surround each syntactic element with
-- comments that can be ignored without disgarding them.
data Com a = Com a | ComBefore [Comment] a | ComAfter a [Comment] | ComAround [Comment] a [Comment]
  deriving (Eq, Ord, Typeable, Show)

instance Functor Com where
  fmap fn c = case c of
    Com          a    -> Com          (fn a)
    ComBefore c1 a    -> ComBefore c1 (fn a)
    ComAfter     a c2 -> ComAfter     (fn a) c2
    ComAround c1 a c2 -> ComAround c1 (fn a) c2

instance NFData a => NFData (Com a) where
  rnf (Com         a  ) = deepseq a ()
  rnf (ComBefore a b  ) = deepseq a $! deepseq b ()
  rnf (ComAfter    a b) = deepseq a $! deepseq b ()
  rnf (ComAround a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue a => HasNullValue (Com a) where
  nullValue = Com nullValue
  testNull (Com a) = testNull a
  testNull _ = False

instance HasLocation a => HasLocation (Com a) where
  getLocation = getLocation . unComment
  setLocation com loc = fmap (\a -> setLocation a loc) com
  delLocation = fmap delLocation

instance PPrintable a => PPrintable (Com a) where { pPrint = pPrintComWith pPrint }

instance (Typeable a, ObjectClass a) => ToDaoStructClass (Com a) Object where
  toDaoStruct = do
    renameConstructor "Commented"
    ask >>= \co -> void $ case co of
      Com          a    ->                   "data" .= a
      ComBefore c1 a    -> "before" .= c1 >> "data" .= a
      ComAfter     a c2 ->                   "data" .= a >> "after" .= c2
      ComAround c1 a c2 -> "before" .= c1 >> "data" .= a >> "after" .= c2

instance HasRandGen a => HasRandGen (Com a) where { randO = randComWith randO }

instance (Typeable a, ObjectClass a) =>
  ObjectClass (Com a) where { obj=new; fromObj=objFromHaskellData; }
instance Typeable a => HaskellDataClass (Com a) where
  haskellDataInterface = interface (Com $ error "undefined Com") $ do
    return ()
    -- autoDefToStruct >> autoDefFromStruct

instance (Typeable a, ObjectClass a) =>
  ObjectClass [Com a] where { obj=listToObj; fromObj=listFromObj; }

pPrintComWith :: (a -> PPrint) -> Com a -> PPrint
pPrintComWith prin com = case com of
  Com          c    -> prin c
  ComBefore ax c    -> pcom ax >> prin c
  ComAfter     c bx -> prin c >> pcom bx
  ComAround ax c bx -> pcom ax >> prin c >> pcom bx
  where { pcom = pInline . map pPrint }

pListOfComsWith :: (a -> PPrint) -> [Com a] -> PPrint
pListOfComsWith prin = sequence_ . map (pPrintComWith prin)

pListOfComs :: PPrintable a => [Com a] -> PPrint
pListOfComs = pListOfComsWith pPrint

randComWith :: RandO a -> RandO (Com a)
randComWith rand = fmap Com rand
--  randComWith :: RandO a -> RandO (Com a)
--  randComWith rand = do
--    typ <- fmap (flip mod 24 . unsign) randInt
--    a <- rand
--    case typ of
--      0 -> do
--        before <- randO
--        after  <- randO
--        return (ComAround before a after)
--      1 -> do
--        before <- randO
--        return (ComBefore before a)
--      2 -> do
--        after <- randO
--        return (ComAfter a after)
--      _ -> return (Com a)

-- not for export
no :: RandO Location
no = return LocationUnknown

appendComments :: Com a -> [Comment] -> Com a
appendComments com cx = case com of
  Com          a    -> ComAfter     a cx
  ComAfter     a ax -> ComAfter     a (ax++cx)
  ComBefore ax a    -> ComAround ax a cx
  ComAround ax a bx -> ComAround ax a (bx++cx)

com :: [Comment] -> a -> [Comment] -> Com a
com before a after = case before of
  [] -> case after of
    [] -> Com a
    dx -> ComAfter a dx
  cx -> case after of
    [] -> ComBefore cx a
    dx -> ComAround cx a dx

setCommentBefore :: [Comment] -> Com a -> Com a
setCommentBefore cx com = case com of
  Com         a    -> ComBefore cx a
  ComBefore _ a    -> ComBefore cx a
  ComAfter    a dx -> ComAround cx a dx
  ComAround _ a dx -> ComAround cx a dx

setCommentAfter :: [Comment] -> Com a -> Com a
setCommentAfter cx com = case com of
  Com          a   -> ComAfter     a cx
  ComBefore dx a   -> ComAround dx a cx
  ComAfter     a _ -> ComAfter     a cx
  ComAround dx a _ -> ComAround dx a cx

unComment :: Com a -> a
unComment com = case com of
  Com         a   -> a
  ComBefore _ a   -> a
  ComAfter    a _ -> a
  ComAround _ a _ -> a

getComment :: Com a -> ([Comment], [Comment])
getComment com = case com of
  Com         _   -> ([], [])
  ComBefore a _   -> (a, [])
  ComAfter    _ b -> ([], b)
  ComAround a _ b -> (a, b)

----------------------------------------------------------------------------------------------------

-- | Defined such that the instantiation of 'CodeBlock' into the 'Executable' class executes each
-- 'ScriptExpr' in the 'CodeBlock', one after the other. Execution does not
-- occur within a 'execNested' because many other expressions which execute 'CodeBlock's,
-- especially 'TryCatch' expressions and 'ForLoop's need to be able to choose
-- when the stack is pushed so they can define temporary local variables.
newtype CodeBlock = CodeBlock { codeBlock :: [ScriptExpr] } deriving (Eq, Ord, Show, Typeable)

setupCodeBlock :: CodeBlock -> Exec Subroutine
setupCodeBlock scrp = do
  -- create the 'Data.IORef.IORef' for storing static variables
  statvars    <- liftIO (newIORef mempty)
  statrules   <- liftIO (newIORef mempty)
  statlambdas <- liftIO (newIORef mempty)
  return $
    Subroutine
    { origSourceCode = scrp
    , staticVars     = statvars
    , staticRules    = statrules
    , staticLambdas  = statlambdas
    , executable     = execute scrp >> return Nothing
    }

instance Monoid CodeBlock where
  mempty      = CodeBlock []
  mappend a b = CodeBlock (mappend (codeBlock a) (codeBlock b))

instance HasNullValue CodeBlock where
  nullValue = mempty
  testNull (CodeBlock []) = True
  testNull _ = False

instance HasLocation CodeBlock where
  getLocation o = case codeBlock o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = CodeBlock (fmap delLocation (codeBlock o))

instance B.Binary CodeBlock MTab where
  put (CodeBlock o) = B.prefixByte 0x3D $ B.put o
  get = B.tryWord8 0x3D $ CodeBlock <$> B.get

instance PPrintable CodeBlock where { pPrint = pPrintInterm }

instance Executable CodeBlock () where { execute (CodeBlock ox) = mapM_ execute ox }

instance ObjectClass CodeBlock where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass CodeBlock where
  haskellDataInterface = interface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> autoDefPPrinter
    defDeref $ \o -> catchError (execute o >> return Nothing) $ \e -> case e of
      ExecReturn o -> return o
      ExecError{}  -> throwError e
    -- TODO: define autoDefIterator, defIndexer, autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A subroutine is contains a 'CodeBlock' and an 'Data.IORef.IORef' to it's own static data. It
-- also has a reference to the last evaluation of 'execute' over it's 'CodeBlock', which provides a
-- hint to the Haskell runtime system that this code can be cached rather than evaluating the
-- 'CodeBlock' fresh every time. In a sense, it is a "live" 'CodeBlock' that can actually be
-- executed.
data Subroutine
  = Subroutine
    { origSourceCode :: CodeBlock
    , staticVars     :: IORef (M.Map Name Object)
    , staticRules    :: IORef (PatternTree Object [Subroutine])
    , staticLambdas  :: IORef [CallableCode]
    , executable     :: Exec (Maybe Object)
    }

instance Show Subroutine where { show o = "Subroutine "++show (codeBlock (origSourceCode o)) }

instance NFData Subroutine where { rnf (Subroutine a _ _ _ _) = deepseq a () }

instance HasNullValue Subroutine where
  nullValue =
    Subroutine
    { origSourceCode = nullValue
    , staticVars = error "accessed staticVars or null Subroutine"
    , staticRules = error "accessed staticRules of null Subroutine"
    , staticLambdas = error "accessed staticLambdas of null Subroutine"
    , executable = return Nothing
    }
  testNull (Subroutine a _ _ _ _) = testNull a

instance PPrintable Subroutine where { pPrint = mapM_ pPrint . codeBlock . origSourceCode }

instance Executable Subroutine (Maybe Object) where
  execute sub = local (\x->x{currentCodeBlock=StaticStore(Just sub)}) $
    catchReturn return ((execute (origSourceCode sub) :: Exec ()) >> return Nothing) :: Exec (Maybe Object)

-- | Although 'Subroutine' instantiates 'Executable', this function allows you to easily place a
-- group of defined local variables onto the call stack before and the have the 'Subroutine'
-- executed.
runCodeBlock :: T_dict -> Subroutine -> Exec (Maybe Object)
runCodeBlock initStack exe = local (\xunit -> xunit{currentCodeBlock = StaticStore (Just exe)}) $!
  execFuncPushStack initStack (executable exe >>= liftIO . evaluate)

----------------------------------------------------------------------------------------------------

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion with Haskell's "Data.Function"). 
data CallableCode
  = CallableCode
    { argsPattern    :: ParamListExpr
    , returnType     :: ObjType
    , codeSubroutine :: Subroutine
    }
  deriving (Show, Typeable)

-- Used by the instantiation of CallableCode and GlobAction into the PPrintable class.
ppCallableAction :: String -> PPrint -> ObjType -> Subroutine -> PPrint
ppCallableAction what pats typ exe =
  pClosure (pString what >> pats >> pPrint typ) "{" "}" (map pPrint (codeBlock (origSourceCode exe)))

-- | Interface used during evaluation of Dao scripts to determine whether or not an if or while
-- statement should continue. Also, turns out to be handy for plenty of other reasons.
instance HasNullValue CallableCode where
  nullValue =
    CallableCode{argsPattern=nullValue, returnType=nullValue, codeSubroutine=nullValue}
  testNull (CallableCode a b c) = testNull a && testNull b && testNull c

instance NFData CallableCode  where { rnf (CallableCode  a b _) = deepseq a $! deepseq b () }

instance PPrintable CallableCode where 
  pPrint (CallableCode pats ty exe) = ppCallableAction "function" (pPrint pats) ty exe

instance ObjectClass CallableCode where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass CallableCode where
  haskellDataInterface = interface (CallableCode undefined undefined undefined) $ do
    autoDefNullTest >> autoDefPPrinter
    defCallable (return . return)

----------------------------------------------------------------------------------------------------

-- A subroutine that is executed when a query string matches it's @['Dao.Glob.Glob']@ expression.
data GlobAction
  = GlobAction
    { globPattern    :: [Glob Object]
    , globSubroutine :: Subroutine
    }
  deriving (Show, Typeable)

instance NFData GlobAction where { rnf (GlobAction a b) = deepseq a $! deepseq b () }

instance HasNullValue GlobAction where
  nullValue = GlobAction{globPattern=[], globSubroutine=nullValue}
  testNull (GlobAction a b) = null a && testNull b

instance PPrintable GlobAction where
  pPrint (GlobAction pats exe) = (\a -> ppCallableAction "rule" a nullValue exe) $ case pats of
    []    -> pString "()"
    [pat] -> pPrint pat
    pats  -> pList_ "(" ", " ")" (map pPrint pats)

----------------------------------------------------------------------------------------------------

-- | This node in the AST typically represents the list of 'AST_Script' expressions found between
-- curly-brackets in expressions like "if" and "else" statement, "for" statements and "while"
-- statements, "with" satements, "try" and "catch" statements and function declrataions.
newtype AST_CodeBlock = AST_CodeBlock{ getAST_CodeBlock :: [AST_Script] } deriving (Eq, Ord, Typeable, Show)
  -- A code block is never standing on it's own, it is always part of a larger expression, so there
  -- is no 'Dao.Token.Location' parameter for 'AST_CodeBlock'.

instance Monoid AST_CodeBlock where
  mempty      = AST_CodeBlock []
  mappend a b = AST_CodeBlock (mappend (getAST_CodeBlock a) (getAST_CodeBlock b))

instance NFData AST_CodeBlock where { rnf (AST_CodeBlock a) = deepseq a () }

instance HasNullValue AST_CodeBlock where
  nullValue = AST_CodeBlock []
  testNull (AST_CodeBlock a) = null a

instance HasLocation AST_CodeBlock where                                      
  getLocation o = case getAST_CodeBlock o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = AST_CodeBlock (fmap delLocation (getAST_CodeBlock o))

-- 'pPrintComWith' wasn't good enough for this, because the comments might occur after the header
-- but before the opening bracket.
pPrintComCodeBlock :: PPrint -> Com AST_CodeBlock -> PPrint
pPrintComCodeBlock header c = case c of
  Com          c    -> run [] c []
  ComBefore bx c    -> run bx c []
  ComAfter     c ax -> run [] c ax
  ComAround bx c ax -> run bx c ax
  where
    run :: [Comment] -> AST_CodeBlock -> [Comment] -> PPrint
    run before cx after = case getAST_CodeBlock cx of
      [] -> header >> pInline (map pPrint before) >> pString " {}" >> pInline (map pPrint after)
      cx -> do
        pClosure (header >> pInline (map pPrint before)) " { " " }" (map (pGroup True . pPrint) cx)
        pInline (map pPrint after)

pPrintSubBlock :: PPrint -> AST_CodeBlock -> PPrint
pPrintSubBlock header px = pPrintComCodeBlock header (Com px)

instance PPrintable AST_CodeBlock where { pPrint o = mapM_ pPrint (getAST_CodeBlock o) }

instance ToDaoStructClass AST_CodeBlock Object where
  toDaoStruct = void $ renameConstructor "CodeBlock" >> "block" .=@ getAST_CodeBlock

instance FromDaoStructClass AST_CodeBlock Object where
  fromDaoStruct = constructor "CodeBlock" >> AST_CodeBlock <$> req "block"

instance HasRandGen AST_CodeBlock where { randO = countNode $ fmap AST_CodeBlock (randList 0 30) }

instance Intermediate CodeBlock AST_CodeBlock where
  toInterm   (AST_CodeBlock ast) = return $ CodeBlock     (ast >>= toInterm  )
  fromInterm (CodeBlock     obj) = return $ AST_CodeBlock (obj >>= fromInterm)

instance ObjectClass AST_CodeBlock where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_CodeBlock where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Functions and function parameters can specify optional type-checking expressions. This is a
-- data type that wraps a dao-typeable expression with type information.
data TyChkExpr a
  = NotTypeChecked{tyChkItem::a}
    -- ^ no type information was specified for this item
  | TypeChecked   {tyChkItem::a, tyChkExpr::ArithExpr, tyChkLoc::Location}
    -- ^ type check information was specified and should be checked every time it is evaluated.
  | DisableCheck  {tyChkItem::a, tyChkExpr::ArithExpr, typChkResult::Object, tyChkLoc::Location}
    -- ^ type check information was specified but has been disabled for efficiency reasons because
    -- we have verified that the item will always return a succesfull type-check.
  deriving (Eq, Ord, Typeable, Show)

checkedExpr :: TyChkExpr a -> a
checkedExpr o = case o of
  NotTypeChecked o       -> o
  TypeChecked    o _ _   -> o
  DisableCheck   o _ _ _ -> o

instance Functor TyChkExpr where
  fmap f (NotTypeChecked   a  ) = NotTypeChecked (f a)
  fmap f (TypeChecked  a b c  ) = TypeChecked  (f a) b c
  fmap f (DisableCheck a b c d) = DisableCheck (f a) b c d

instance NFData a => NFData (TyChkExpr a) where
  rnf (NotTypeChecked   a  ) = deepseq a ()
  rnf (TypeChecked  a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (DisableCheck a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue a => HasNullValue (TyChkExpr a) where
  nullValue = NotTypeChecked nullValue
  testNull (NotTypeChecked a) = testNull a
  testNull _ = False

instance HasLocation a => HasLocation (TyChkExpr a) where
  getLocation a     = case a of
    NotTypeChecked a         -> getLocation a
    TypeChecked    a _   loc -> getLocation a <> loc
    DisableCheck   a _ _ loc -> getLocation a <> loc
  setLocation a loc = case a of
    NotTypeChecked a       -> NotTypeChecked (setLocation a loc)
    TypeChecked    a b   _ -> TypeChecked  a b loc
    DisableCheck   a b c _ -> DisableCheck a b c loc
  delLocation a     = case a of
    NotTypeChecked a       -> NotTypeChecked (delLocation a)
    TypeChecked    a b   _ -> TypeChecked (delLocation a) (delLocation b) LocationUnknown
    DisableCheck   a b c _ -> DisableCheck a b c LocationUnknown

instance PPrintable a => PPrintable (TyChkExpr a) where
  pPrint a = case a of
    NotTypeChecked a        -> pPrint a
    TypeChecked    a expr _ -> pInline [pPrint a, pString ": ", pPrint expr]
    DisableCheck   a  _ _ _ -> pInline [pPrint a]

instance B.Binary a MTab => B.Binary (TyChkExpr a) MTab where
  put o = case o of
    NotTypeChecked a       -> B.prefixByte 0x4B $ B.put a
    TypeChecked    a b c   -> B.prefixByte 0x4C $ B.put a >> B.put b >> B.put c
    DisableCheck   a b c d -> B.prefixByte 0x4D $ B.put a >> B.put b >> B.put c >> B.put d
  get = B.word8PrefixTable <|> fail "expecting TyChkExpr"

instance B.Binary a MTab => B.HasPrefixTable (TyChkExpr a) B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "TyChkExpr" 0x4B 0x4D $
    [ NotTypeChecked <$> B.get
    , pure TypeChecked  <*> B.get <*> B.get <*> B.get
    , pure DisableCheck <*> B.get <*> B.get <*> B.get <*> B.get
    ]

instance (Eq a, Ord a, Typeable a, ObjectClass a) =>
  ObjectClass (TyChkExpr a) where { obj=new; fromObj=objFromHaskellData; }
instance (Eq a, Ord a, Typeable a, ObjectClass a) =>
  HaskellDataClass (TyChkExpr a) where
    haskellDataInterface = interface (NotTypeChecked $ error "undefined TyChkExpr") $ do
      autoDefEquality >> autoDefOrdering

----------------------------------------------------------------------------------------------------

-- | This node can be found in a few different syntactic structures. When a name or function or
-- expression is followed by a colon and some type checking information, this node is used for that
-- purpose.
data AST_TyChk a
  = AST_NotChecked a
  | AST_Checked    a (Com ()) AST_Arith Location
  deriving (Eq, Ord, Typeable, Show)

checkedAST :: AST_TyChk a -> a
checkedAST a = case a of { AST_NotChecked a -> a; AST_Checked a _ _ _ -> a; }

astTyChkDelLocWith :: (a -> a) -> AST_TyChk a -> AST_TyChk a
astTyChkDelLocWith del a = case a of
  AST_NotChecked a       -> AST_NotChecked (del a)
  AST_Checked    a b c _ -> AST_Checked    (del a) b (delLocation c) LocationUnknown

instance Functor AST_TyChk where
  fmap f (AST_NotChecked a      ) = AST_NotChecked (f a)
  fmap f (AST_Checked    a b c d) = AST_Checked    (f a) b c d

instance NFData a => NFData (AST_TyChk a) where
  rnf (AST_NotChecked    a) = deepseq a ()
  rnf (AST_Checked a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue a => HasNullValue (AST_TyChk a) where
  nullValue = AST_NotChecked nullValue
  testNull (AST_NotChecked  a  ) = testNull a
  testNull (AST_Checked _ _ a _) = testNull a

instance PPrintable a => PPrintable (AST_TyChk a) where
  pPrint a = case a of
    AST_NotChecked a          -> pPrint a
    AST_Checked    a coms expr _ -> pInline $
      [ pPrint a
      , pPrintComWith (\ () -> pString ": ") coms
      , pPrint expr
      ]

instance ObjectClass a => ToDaoStructClass (AST_TyChk a) Object where
  toDaoStruct = ask >>= \o -> renameConstructor "TypeChecked" >> case o of
    AST_NotChecked o              -> void $ define "data" (obj o)
    AST_Checked    o coms typ loc -> do
      define "data" (obj o)
      "colon"    .= coms
      "typeExpr" .= typ
      putLocation loc

instance HasLocation a => HasLocation (AST_TyChk a) where
  getLocation a     = case a of
    AST_NotChecked a         -> getLocation a
    AST_Checked    a _ _ loc -> getLocation a <> loc
  setLocation a loc = case a of
    AST_NotChecked a         -> AST_NotChecked (setLocation a loc)
    AST_Checked    a b c _   -> AST_Checked a b c loc
  delLocation       = astTyChkDelLocWith delLocation

instance HasRandGen a => HasRandGen (AST_TyChk a) where
  randO = countNode $ AST_NotChecked <$> randO
  --randChoice = randChoiceList [AST_NotChecked <$> randO, pure AST_Checked <*> randO <*> randO <*> randO <*> no]

instance (Eq a, Ord a, PPrintable a, Typeable a, ObjectClass a) =>
  ObjectClass (AST_TyChk a) where { obj=new; fromObj=objFromHaskellData; }
instance (Eq a, Ord a, PPrintable a, Typeable a) => HaskellDataClass (AST_TyChk a) where
  haskellDataInterface = interface (AST_NotChecked $ error "undefined AST_TyChk") $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

tyChkToInterm :: Intermediate a b => AST_TyChk b -> [TyChkExpr a]
tyChkToInterm a = case a of
  AST_NotChecked a         -> liftM  NotTypeChecked (ti a)
  AST_Checked    a _ b loc -> liftM3 TypeChecked (ti a) (ti b) [loc]

tyChkFromInterm :: Intermediate a b => TyChkExpr a -> [AST_TyChk b]
tyChkFromInterm a = case a of
    NotTypeChecked a         -> liftM  AST_NotChecked (fi a)
    TypeChecked    a b   loc -> liftM4 AST_Checked (fi a) [Com ()] (fi b) [loc]
    DisableCheck   a b _ loc -> liftM4 AST_Checked (fi a) [Com ()] (fi b) [loc]

----------------------------------------------------------------------------------------------------

-- | 'ParamExpr' is a part of the Dao language semantics, and is also used in the the 'CallableCode'
-- data type when evaluating parameters to be passed to the callable code function execution. The
-- boolean parameter here indicates whether or not the parameter should be passed by reference.
data ParamExpr = ParamExpr Bool (TyChkExpr Name) Location deriving (Eq, Ord, Typeable, Show)

instance NFData ParamExpr where
  rnf (ParamExpr       a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasLocation ParamExpr where
  getLocation (ParamExpr _ _ loc)     = loc
  setLocation (ParamExpr a b _  ) loc = ParamExpr a b loc
  delLocation (ParamExpr a b _  )     = ParamExpr a b LocationUnknown

instance PPrintable ParamExpr where
  pPrint (ParamExpr byRef tychk _) = when byRef (pString "$") >> pPrint tychk

instance PPrintable [ParamExpr] where { pPrint lst = pList_ "(" ", " ")" (fmap pPrint lst) }

instance B.Binary ParamExpr MTab where
  put (ParamExpr True  a b) = B.prefixByte 0x4E $ B.put a >> B.put b
  put (ParamExpr False a b) = B.prefixByte 0x4F $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting ParamExpr"

instance B.HasPrefixTable ParamExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParamExpr" 0x4E 0x4F $
    [ pure (ParamExpr True ) <*> B.get <*> B.get
    , pure (ParamExpr False) <*> B.get <*> B.get
    ]

instance ObjectClass ParamExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ParamExpr where
  haskellDataInterface = interface (ParamExpr False (error "undefined ParamExpr") LocationUnknown) $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_Param
  = AST_NoParams
  | AST_Param (Maybe [Comment]) (AST_TyChk Name) Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Param where
  rnf  AST_NoParams     = ()
  rnf (AST_Param a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_Param where
  nullValue = AST_NoParams
  testNull AST_NoParams = True
  testNull _ = False

instance HasLocation AST_Param where
  getLocation a     = case a of
    AST_NoParams      -> LocationUnknown
    AST_Param _ _ loc -> loc
  setLocation a loc = case a of
    AST_NoParams    -> AST_NoParams
    AST_Param a b _ -> AST_Param a b loc
  delLocation a     = case a of
    AST_NoParams    -> AST_NoParams
    AST_Param a b _ -> AST_Param a (delLocation b) LocationUnknown

instance PPrintable AST_Param where
  pPrint o = case o of
    AST_NoParams            -> return ()
    AST_Param mcoms tychk _ -> pInline $
      [ maybe (return ()) (\coms -> pString "$" >> pPrint coms) mcoms
      , pPrint tychk
      ]

instance PPrintable [Com AST_Param] where
  pPrint lst = pList_ "(" ", " ")" (fmap pPrint lst)

instance ToDaoStructClass AST_Param Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_NoParams             -> makeNullary "Void"
    AST_Param coms tychk loc -> do
      renameConstructor "Parameter"
      maybe (return ()) putComments coms
      "typeCheck" .= tychk
      putLocation loc

instance FromDaoStructClass AST_Param Object where
  fromDaoStruct = msum $
    [ nullary "Void" >> return AST_NoParams
    , constructor "Parameter" >> pure AST_Param <*> optComments <*> req "typeCheck" <*> location
    ]

instance HasRandGen AST_Param where
  randO = countNode $ pure AST_Param <*> randO <*> randO <*> no

instance HasRandGen [Com AST_Param] where { randO = recurse [] $ randList 0 8 }

instance Intermediate ParamExpr AST_Param where
  toInterm   a = case a of
    AST_NoParams      -> []
    AST_Param a b loc -> liftM3 ParamExpr [maybe False (const True) a] (tyChkToInterm b) [loc]
  fromInterm o = case o of
    ParamExpr a b loc -> liftM3 AST_Param [if a then Just [] else Nothing] (tyChkFromInterm b) [loc]

instance Intermediate [ParamExpr] [Com AST_Param] where
  toInterm   ax = [ax >>= toInterm . unComment]
  fromInterm ax = [ax >>= fmap Com . fromInterm]

instance ObjectClass AST_Param where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Param where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A list of function parameters (arguments) to a function in an object representing a function
-- expression.
data ParamListExpr = ParamListExpr (TyChkExpr [ParamExpr]) Location
  deriving (Eq, Ord, Typeable, Show)

getTypeCheckList :: ParamListExpr -> [ParamExpr]
getTypeCheckList (ParamListExpr tychk _) = tyChkItem tychk 

instance NFData ParamListExpr where { rnf (ParamListExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue ParamListExpr where
  nullValue = ParamListExpr (NotTypeChecked []) LocationUnknown
  testNull (ParamListExpr (NotTypeChecked []) _) = True
  testNull _ = False

-- Here there is a gap of 3 prefix bytes (from 0x36 to 0x38) where the 'ObjectExpr' 
-- data type may be expanded to include more nodes.
instance B.Binary ParamListExpr MTab where
  put (ParamListExpr tyChk loc) = B.prefixByte 0x39 $ B.put tyChk >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting ParamListExpr"

instance B.HasPrefixTable ParamListExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParamListExpr" 0x39 0x39 $ [pure ParamListExpr <*> B.get <*> B.get]

instance HasLocation ParamListExpr where
  getLocation (ParamListExpr _ loc)     = loc
  setLocation (ParamListExpr a _  ) loc = ParamListExpr a loc
  delLocation (ParamListExpr a _  )     = ParamListExpr a LocationUnknown

instance PPrintable ParamListExpr where { pPrint (ParamListExpr lst _) = pPrint lst }

instance ObjectClass ParamListExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ParamListExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_ParamList
  = AST_ParamList (AST_TyChk [Com AST_Param]) Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_ParamList where { rnf (AST_ParamList a b) = deepseq a $! deepseq b () }

instance HasNullValue AST_ParamList where
  nullValue = AST_ParamList nullValue LocationUnknown
  testNull (AST_ParamList a _) = testNull a

instance HasLocation AST_ParamList where
  getLocation (AST_ParamList _ loc)     = loc
  setLocation (AST_ParamList a _  ) loc = AST_ParamList a loc
  delLocation (AST_ParamList a _  )     = AST_ParamList (astTyChkDelLocWith (fmap delLocation) a) LocationUnknown

instance PPrintable AST_ParamList where
  pPrint (AST_ParamList lst _) = pInline [pPrint lst]

instance ToDaoStructClass AST_ParamList Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_ParamList tychk loc -> do
      renameConstructor "ParamList"
      "typeCheck" .= tychk
      putLocation loc

instance FromDaoStructClass AST_ParamList Object where
  fromDaoStruct = constructor "ParamList" >> pure AST_ParamList <*> req "typeCheck" <*> location

instance HasRandGen AST_ParamList where { randO = countNode $ pure AST_ParamList <*> randO <*> no }

instance Intermediate ParamListExpr AST_ParamList where
  toInterm   (AST_ParamList ox loc) = liftM2 ParamListExpr (tyChkToInterm ox) [loc]
  fromInterm (ParamListExpr ox loc) = liftM2 AST_ParamList (tyChkFromInterm ox) [loc]

instance ObjectClass AST_ParamList where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_ParamList where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Convert an 'ObjectExpr' to an 'Dao.Glob.Glob'.
paramsToGlobExpr :: ObjectExpr -> Exec (Glob UStr)
paramsToGlobExpr o = case o of
  ObjLiteralExpr (LiteralExpr (OString str) _) -> return (read (uchars str))
  _ -> execThrow $ obj $ [obj "does not evaluate to a \"glob\" pattern"]

data RuleStrings = RuleStrings [UStr] Location deriving (Eq, Ord, Typeable, Show)

instance HasNullValue RuleStrings where
  nullValue = RuleStrings [] LocationUnknown
  testNull (RuleStrings a _) = null a

instance HasLocation RuleStrings where
  getLocation (RuleStrings _ o)     = o
  setLocation (RuleStrings a _) loc = RuleStrings a loc
  delLocation (RuleStrings a _)     = RuleStrings a LocationUnknown

instance NFData RuleStrings where { rnf (RuleStrings a b) = deepseq a $! deepseq b () }

instance B.Binary RuleStrings MTab where
  put (RuleStrings a b) = B.prefixByte 0x3A $ B.put a >> B.put b
  get = (B.tryWord8 0x3A $ pure RuleStrings <*> B.get <*> B.get) <|> fail "expecting RuleStrings"

instance ObjectClass RuleStrings where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass RuleStrings where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_StringList
  = AST_NoStrings  [Comment]  Location
  | AST_StringList [Com UStr] Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_StringList where
  rnf (AST_NoStrings  a b) = deepseq a $! deepseq b ()
  rnf (AST_StringList a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_StringList where
  nullValue = AST_NoStrings [] LocationUnknown
  testNull (AST_NoStrings _ _) = True
  testNull _ = False

instance HasLocation AST_StringList where
  getLocation o     = case o of
    AST_NoStrings  _ o -> o
    AST_StringList _ o -> o
  setLocation o loc = case o of
    AST_NoStrings  a _ -> AST_NoStrings  a loc
    AST_StringList a _ -> AST_StringList a loc
  delLocation o     = case o of
    AST_NoStrings  a _ -> AST_NoStrings  a LocationUnknown
    AST_StringList a _ -> AST_StringList a LocationUnknown

instance PPrintable AST_StringList where
  pPrint o = case o of
    AST_NoStrings  coms _ -> pInline [pString "rule(", pPrint coms, pString ")"]
    AST_StringList [r]  _ -> pInline [pString "rule ", pPrint r]
    AST_StringList ruls _ -> pList (pString "rule") "(" ", " ")" (fmap pPrint ruls)

instance ToDaoStructClass AST_StringList Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_NoStrings coms loc -> do
      renameConstructor "NoStrings"
      putComments coms >> putLocation loc
    AST_StringList lst loc -> do
      renameConstructor "StringList"
      "items" .= lst >> putLocation loc

instance FromDaoStructClass AST_StringList Object where
  fromDaoStruct = msum $
    [ constructor "NoStrings"  >> pure AST_NoStrings  <*> comments <*> location
    , constructor "StringList" >> pure AST_StringList <*> reqList "items" <*> location
    ]

instance HasRandGen AST_StringList where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ pure AST_StringList <*> randListOf 1 4 (randComWith (fmap (ustr . show) (randO::RandO (Glob UStr)))) <*> no
    , pure AST_NoStrings  <*> randO <*> no
    ]

instance Intermediate RuleStrings AST_StringList where
  toInterm   o = case o of
    AST_NoStrings  _ loc -> liftM2 RuleStrings [[]] [loc]
    AST_StringList o loc -> liftM2 RuleStrings [fmap unComment o] [loc]
  fromInterm o = case o of
    RuleStrings [] loc -> liftM2 AST_NoStrings  [[]]         [loc]
    RuleStrings o  loc -> liftM2 AST_StringList [fmap Com o] [loc]

instance ObjectClass AST_StringList where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_StringList where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- $Exec_helpers
-- Functions for working with object values when building built-in functions callable from within a
-- dao script.

asReference :: Object -> Exec QualRef
asReference = maybe mzero return . fromObj

asInteger :: Object -> Exec Integer
asInteger o = case o of
  OWord    o -> return (toInteger o)
  OInt     o -> return (toInteger o)
  OLong    o -> return o
  OFloat   o -> return (round o)
  ORatio   o -> return (round o)
  ORelTime o -> return (round (toRational o))
  _          -> mzero

asRational :: Object -> Exec Rational
asRational o = case o of
  OInt     o -> return (toRational o)
  OWord    o -> return (toRational o)
  OLong    o -> return (toRational o)
  OFloat   o -> return (toRational o)
  ORelTime o -> return (toRational o)
  ORatio   o -> return o
  OComplex o | imagPart o == 0 -> return (toRational (realPart o))
  _          -> mzero

asComplex :: Object -> Exec T_complex
asComplex o = case o of
  OComplex o -> return o
  o          -> asRational o >>= return . flip complex 0 . fromRational

asStringNoConvert :: Object -> Exec UStr
asStringNoConvert o = case o of
  OString o -> return o
  _         -> mzero

asString :: Object -> Exec UStr
asString o = case o of
  OString o -> return o
  o         -> return (ustr (showObj o))

asListNoConvert :: Object -> Exec [Object]
asListNoConvert o = case o of
  OList o -> return o
  _       -> mzero

asList :: Object -> Exec [Object]
asList o = case o of
  OList   o -> return o
--OTree   o -> return (map (\ (i, o) -> OPair (OList (map OString i), o)) (T.assocs o))
  _         -> mzero

-- | Combines two lists of objects, then removes one "layer of lists", that is, if the combined
-- lists are of the form:
-- @list {a, b, ... , list {c, d, ... , list {e, f, ...}, ...} }@ 
-- the resulting list will be @list {a, b, ... , c, d, ... , list {e, f, ... }, ...}@
objListAppend :: [Object] -> [Object] -> Object
objListAppend ax bx = OList $ flip concatMap (ax++bx) $ \a -> case a of
  OList ax -> ax
  a        -> [a]

asHaskellInt :: Object -> Exec Int
asHaskellInt o = asInteger o >>= \o ->
  if (toInteger (minBound::Int)) <= o && o <= (toInteger (maxBound::Int))
    then return (fromIntegral o)
    else mzero

evalInt :: (Integer -> Integer -> Integer) -> Object -> Object -> Exec Object
evalInt ifunc a b = do
  ia <- asInteger a
  ib <- asInteger b
  let x = ifunc ia ib
  case max (fromEnum (typeOfObj a)) (fromEnum (typeOfObj b)) of
    t | t == fromEnum WordType -> return $ OWord (fromIntegral x)
    t | t == fromEnum IntType  -> return $ OInt  (fromIntegral x)
    t | t == fromEnum LongType -> return $ OLong (fromIntegral x)
    _ -> fail "asInteger returned a value for an object of an unexpected type"

evalNum
  :: (Integer -> Integer -> Integer)
  -> (Rational -> Rational -> Rational)
  -> Object -> Object -> Exec Object
evalNum ifunc rfunc a b = msum $
  [ evalInt ifunc a b
  , do  ia <- asRational a
        ib <- asRational b
        let x = rfunc ia ib
        case (max (fromEnum (typeOfObj a)) (fromEnum (typeOfObj b))) of
          t | t == fromEnum FloatType    -> return $ OFloat    (fromRational x)
          t | t == fromEnum DiffTimeType -> return $ ORelTime (fromRational x)
          t | t == fromEnum RatioType    -> return $ ORatio    (fromRational x)
          t | t == fromEnum ComplexType  -> return $ OComplex  (fromRational x)
          _ -> fail "asRational returned a value for an object of an unexpected type"
  ]

eval_ADD :: Object -> Object -> Exec Object
eval_ADD a b = msum
  [ evalNum (+) (+) a b
  , timeAdd a b, timeAdd b a
  , listAdd a b, listAdd b a
  , stringAdd (++) a b, stringAdd (flip (++)) b a
  ]
  where
    timeAdd a b = case (a, b) of
      (OAbsTime a, ORelTime b) -> return (OAbsTime (addUTCTime b a))
--    (OAbsTime a, ORatio    b) -> return (OAbsTime (addUTCTime (fromRational (toRational b)) a))
--    (OAbsTime a, OFloat    b) -> return (OAbsTime (addUTCTime (fromRational (toRational b)) a))
      _                      -> mzero
    listAdd a b = do
      ax <- asListNoConvert a
      bx <- case b of
        OList  bx -> return bx
--      OSet   b  -> return (S.elems b)
--      OArray b  -> return (elems b)
        _         -> mzero
      return (objListAppend ax bx)
    stringAdd add a b = case a of
      OString a -> do
        b <- asString b
        return (obj (add (uchars a) (uchars b)))
      _         -> mzero

eval_SUB :: Object -> Object -> Exec Object
eval_SUB a b = msum $
  [ evalNum (-) (-) a b
  , case (a, b) of
      (OAbsTime a, OAbsTime     b) -> return (ORelTime (diffUTCTime a b))
      (OAbsTime a, ORelTime b) -> return (OAbsTime (addUTCTime (negate b) a))
--    (OAbsTime a, ORatio    b) -> return (OAbsTime (addUTCTime (fromRational (toRational (negate b))) a))
--    (OAbsTime a, OFloat    b) -> return (OAbsTime (addUTCTime (fromRational (toRational (negate b))) a))
      _                  -> mzero
  ]

evalDistNum
  :: (Integer  -> Integer  -> Integer )
  -> (Rational -> Rational -> Rational) 
  -> Object -> Object -> Exec Object
evalDistNum intFn rnlFn a b = evalNum intFn rnlFn a b

eval_MULT :: Object -> Object -> Exec Object
eval_MULT a b = evalDistNum (*) (*) a b

eval_DIV :: Object -> Object -> Exec Object
eval_DIV a b = evalDistNum div (/) a b

eval_MOD :: Object -> Object -> Exec Object
eval_MOD a b = evalDistNum mod (\a b -> let r = a/b in (abs r - abs (floor r % 1)) * signum r) a b

eval_POW :: Object -> Object -> Exec Object
eval_POW = evalNum (^) (\ a b -> toRational ((fromRational a :: Double) ** (fromRational b :: Double)))

evalBitsOrSets
  :: ([Object]  -> Object)
  -> (([Object] -> [Object] -> [Object]) -> M.Map Name [Object] -> M.Map Name [Object] -> M.Map Name [Object])
  -> (([Object] -> [Object] -> [Object]) -> I.IntMap   [Object] -> I.IntMap   [Object] -> I.IntMap   [Object])
-- -> (T_set -> T_set  -> T_set)
  -> (Integer -> Integer -> Integer)
  -> Object -> Object -> Exec Object
evalBitsOrSets _combine _dict _intmap {-set-} num a b = evalInt num a b

eval_ORB :: Object -> Object -> Exec Object
eval_ORB  a b = evalBitsOrSets OList M.unionWith        I.unionWith        {-S.union-}        (.|.) a b

eval_ANDB :: Object -> Object -> Exec Object
eval_ANDB a b = evalBitsOrSets OList M.intersectionWith I.intersectionWith {-S.intersection-} (.&.) a b

eval_XORB :: Object -> Object -> Exec Object
eval_XORB a b = evalBitsOrSets (\a -> head a) mfn ifn {-sfn-} xor a b where
--sfn = fn S.union S.intersection S.difference head
  mfn = fn M.union M.intersection M.difference
  ifn = fn I.union I.intersection I.difference
  fn u n del _ a b = (a `u` b) `del` (a `n` b)

evalShift :: (Int -> Int) -> Object -> Object -> Exec Object
evalShift fn a b = asHaskellInt b >>= \b -> case a of
  OInt  a -> return (OInt  (shift a (fn b)))
  OWord a -> return (OWord (shift a (fn b)))
  OLong a -> return (OLong (shift a (fn b)))
  _       -> mzero

evalCompare
  :: (Integer -> Integer -> Bool) -> (Rational -> Rational -> Bool) -> Object -> Object -> Exec Object
evalCompare compI compR a b = msum $
  [ asInteger  a >>= \a -> asInteger  b >>= \b -> done (compI a b)
  , asRational a >>= \a -> asRational b >>= \b -> done (compR a b)
  ]
  where { done true = if true then return OTrue else return ONull }

eval_EQUL :: Object -> Object -> Exec Object
eval_EQUL a b = evalCompare (==) (==) a b

eval_NEQUL :: Object -> Object -> Exec Object
eval_NEQUL a b = evalCompare (/=) (/=) a b

eval_GTN :: Object -> Object -> Exec Object
eval_GTN a b = evalCompare (>) (>) a b

eval_LTN :: Object -> Object -> Exec Object
eval_LTN a b = evalCompare (<) (<) a b

eval_GTEQ :: Object -> Object -> Exec Object
eval_GTEQ a b = evalCompare (>=) (>=) a b

eval_LTEQ :: Object -> Object -> Exec Object
eval_LTEQ a b = evalCompare (<=) (<=) a b

eval_SHR :: Object -> Object -> Exec Object
eval_SHR = evalShift negate

eval_SHL :: Object -> Object -> Exec Object
eval_SHL = evalShift id

eval_DOT :: Object -> Object -> Exec Object
eval_DOT _a _b = error "eval_DOT is not defined"

eval_NEG :: Object -> Exec Object
eval_NEG o = case o of
  OWord     o -> return $
    let n = negate (toInteger o)
    in  if n < toInteger (minBound::T_int)
           then  OLong n
           else  OInt (fromIntegral n)
  OInt      o -> return $ OInt     (negate o)
  OLong     o -> return $ OLong    (negate o)
  ORelTime  o -> return $ ORelTime (negate o)
  OFloat    o -> return $ OFloat   (negate o)
  ORatio    o -> return $ ORatio   (negate o)
  OComplex  o -> return $ OComplex (negate o)
  _           -> mzero

eval_INVB :: Object -> Exec Object
eval_INVB o = case o of
  OWord o -> return $ OWord (complement o)
  OInt  o -> return $ OInt  (complement o)
  OLong o -> return $ OLong (complement o)
  _       -> mzero

eval_NOT :: Object -> Exec Object
eval_NOT o = (oBool . not) <$> objToBool o

objToBool :: Object -> Exec Bool
objToBool o = case o of
  OHaskell (HaskellData d ifc) -> case objNullTest ifc of
    Nothing   -> execThrow $ obj [obj "cannot be used as a boolean value:", o]
    Just test -> return (test d)
  o -> return (testNull o)

-- | Traverse the entire object, returning a list of all 'OString' elements.
extractStringElems :: Object -> [UStr]
extractStringElems o = case o of
  OString  o   -> [o]
  OList    o   -> concatMap extractStringElems o
  _            -> []

----------------------------------------------------------------------------------------------------

data UpdateOp
  = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UPOW | UORB | UANDB | UXORB | USHL | USHR | UARROW
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)
instance NFData UpdateOp where { rnf a = seq a () }

allUpdateOpStrs :: String
allUpdateOpStrs = " = += -= *= /= %= **= |= &= ^= <<= >>= <- "

instance UStrType UpdateOp where
  toUStr a = ustr $ case a of
    UCONST -> "="
    UADD   -> "+="
    USUB   -> "-="
    UMULT  -> "*="
    UDIV   -> "/="
    UMOD   -> "%="
    UPOW   -> "**="
    UORB   -> "|="
    UANDB  -> "&="
    UXORB  -> "^="
    USHL   -> "<<="
    USHR   -> ">>="
    UARROW -> "<-"
  maybeFromUStr str = case uchars str of
    "="   -> Just UCONST
    "+="  -> Just UADD  
    "-="  -> Just USUB  
    "*="  -> Just UMULT 
    "/="  -> Just UDIV  
    "%="  -> Just UMOD  
    "**=" -> Just UPOW
    "|="  -> Just UORB  
    "&="  -> Just UANDB 
    "^="  -> Just UXORB 
    "<<=" -> Just USHL  
    ">>=" -> Just USHR  
    "<-"  -> Just UARROW
    _     -> Nothing
  fromUStr str =
    maybe (error (show str++" is not an assignment/update operator")) id (maybeFromUStr str)

instance PPrintable UpdateOp where { pPrint op = pString (' ':uchars op++" ") }

instance B.Binary UpdateOp MTab where
  put o = B.putWord8 $ case o of
    UCONST -> 0x61
    UADD   -> 0x62
    USUB   -> 0x63
    UMULT  -> 0x64
    UDIV   -> 0x65
    UMOD   -> 0x66
    UPOW   -> 0x67
    UORB   -> 0x68
    UANDB  -> 0x69
    UXORB  -> 0x6A
    USHL   -> 0x6B
    USHR   -> 0x6C
    UARROW -> 0x6D
  get = B.word8PrefixTable <|> fail "expecting UpdateOp"
instance B.HasPrefixTable UpdateOp B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "UpdateOp" 0x61 0x6D $ map return $
    [UCONST, UADD, USUB, UMULT, UDIV, UMOD, UPOW, UORB, UANDB, UXORB, USHL, USHR, UARROW]

instance HasRandGen UpdateOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::UpdateOp)))

instance ToDaoStructClass UpdateOp Object where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass UpdateOp Object where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass UpdateOp where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass UpdateOp where
  haskellDataInterface = interface UCONST $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data RefPfxOp = REF | DEREF deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance NFData RefPfxOp where { rnf a = seq a () }

instance UStrType RefPfxOp where
  toUStr op = ustr $ case op of
    REF    -> "$"
    DEREF  -> "@"
  maybeFromUStr str = case uchars str of
    "$" -> Just REF
    "@" -> Just DEREF
    _   -> Nothing
  fromUStr str = maybe (error (show str++" is not a prefix opretor")) id (maybeFromUStr str)

instance PPrintable RefPfxOp where { pPrint = pUStr . toUStr }

instance B.Binary RefPfxOp MTab where
  put o = B.putWord8 $ case o of { REF -> 0x5E; DEREF  -> 0x5F }
  get = B.word8PrefixTable <|> fail "expecting RefPfxOp"

instance B.HasPrefixTable RefPfxOp B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RefPfxOp" 0x5E 0x5F $ map return [REF, DEREF]

instance HasRandGen RefPfxOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::RefPfxOp)))

instance ToDaoStructClass RefPfxOp Object where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass RefPfxOp Object where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass RefPfxOp where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass RefPfxOp where
  haskellDataInterface = interface REF $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Unary operators.
data ArithPfxOp = INVB | NOT | NEGTIV | POSTIV deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance NFData ArithPfxOp where { rnf a = seq a () }

instance UStrType ArithPfxOp where
  toUStr op = ustr $ case op of
    INVB   -> "~"
    NOT    -> "!"
    NEGTIV -> "-"
    POSTIV -> "+"
  maybeFromUStr str = case uchars str of
    "~" -> Just INVB
    "!" -> Just NOT
    "-" -> Just NEGTIV
    "+" -> Just POSTIV
    _   -> Nothing
  fromUStr str = maybe (error (show str++" is not a prefix opretor")) id (maybeFromUStr str)

instance PPrintable ArithPfxOp where { pPrint = pUStr . toUStr }

instance B.Binary ArithPfxOp MTab where
  put o = B.putWord8 $ case o of { INVB -> 0x5A; NOT -> 0x5B; NEGTIV -> 0x5C; POSTIV -> 0x5D }
  get = B.word8PrefixTable <|> fail "expecting ArithPfxOp"

instance B.HasPrefixTable ArithPfxOp B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ArithPfxOp" 0x5A 0x5D $
    map return [INVB, NOT, NEGTIV, POSTIV]

instance HasRandGen ArithPfxOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::ArithPfxOp)))

instance ToDaoStructClass ArithPfxOp Object where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass ArithPfxOp Object where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass ArithPfxOp where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ArithPfxOp where
  haskellDataInterface = interface POSTIV $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

allPrefixOpChars :: String
allPrefixOpChars = "$@~!-+"

allPrefixOpStrs :: String
allPrefixOpStrs = " $ @ ~ - + ! "

----------------------------------------------------------------------------------------------------

-- | Binary operators.
data InfixOp
  = ADD   | SUB   | MULT
  | DIV   | MOD   | POW
  | ORB   | ANDB  | XORB
  | SHL   | SHR
  | OR    | AND
  | EQUL  | NEQUL      
  | GTN   | LTN
  | GTEQ  | LTEQ
  | ARROW
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show, Read)

instance UStrType InfixOp where
  toUStr a = ustr $ case a of
    { ADD  -> "+" ; SUB  -> "-" ; MULT  -> "*"
    ; DIV  -> "/" ; MOD  -> "%" ; POW   -> "**"
    ; ORB  -> "|" ; ANDB -> "&" ; XORB  -> "^"
    ; SHL  -> "<<"; SHR  -> ">>"
    ; OR   -> "||"; AND  -> "&&"
    ; EQUL -> "=="; NEQUL-> "!="
    ; LTN  -> "<" ; GTN  -> ">"
    ; LTEQ -> "<="; GTEQ -> ">="
    ; ARROW -> "->";
    }
  maybeFromUStr str = case uchars str of
    { "+"  -> Just ADD  ; "-"  -> Just SUB  ; "*"  -> Just MULT 
    ; "/"  -> Just DIV  ; "%"  -> Just MOD  ; "**" -> Just POW  
    ; "|"  -> Just ORB  ; "&"  -> Just ANDB ; "^"  -> Just XORB 
    ; "<<" -> Just SHL  ; ">>" -> Just SHR
    ; "||" -> Just OR   ; "&&" -> Just AND
    ; "==" -> Just EQUL ; "!=" -> Just NEQUL
    ; "<"  -> Just LTN  ; ">"  -> Just GTN
    ; "<=" -> Just LTEQ ; ">=" -> Just GTEQ 
    ; "->" -> Just ARROW;
    ; _    -> Nothing
    }
  fromUStr str = maybe (error (show str++" is not an infix operator")) id (maybeFromUStr str)

instance NFData InfixOp  where { rnf a = seq a () }

instance PPrintable InfixOp  where { pPrint = pUStr . toUStr }

-- The byte prefixes overlap with the update operators of similar function to
-- the operators, except for the comparison opeators (EQUL, NEQUL, GTN, LTN,
-- GTEQ, LTEQ) which overlap with the prefix operators (INVB, NOT, NEGTIV, POSTIV, REF, DEREF)
instance B.Binary InfixOp MTab where
  put o = B.putWord8 $ case o of
    { EQUL -> 0x5A; NEQUL -> 0x5B; GTN -> 0x5C; LTN -> 0x5D; GTEQ -> 0x5E; LTEQ -> 0x5F
    ; ADD  -> 0x62; SUB -> 0x63; MULT -> 0x64; DIV   -> 0x65
    ; MOD  -> 0x66; POW -> 0x67; ORB  -> 0x68; ANDB  -> 0x69
    ; XORB -> 0x6A; SHL -> 0x6B; SHR  -> 0x6C; ARROW -> 0x6D
    ; OR   -> 0x6E; AND -> 0x6F } 
  get = B.word8PrefixTable <|> fail "expecting InfixOp"

instance B.HasPrefixTable InfixOp B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "InfixOp" 0x5A 0x6F $ let {r=return; z=mzero} in
    [ r EQUL , r NEQUL, r GTN , r LTN, r GTEQ , r LTEQ , z, z
    , r ADD  , r SUB  , r MULT, r DIV, r MOD  , r POW  , r ORB
    , r ANDB , r XORB , r SHL , r SHR, r ARROW, r OR   , r AND
    ]

instance HasRandGen InfixOp where
  randO = fmap toEnum (nextInt (1+fromEnum (maxBound::InfixOp)))

instance ToDaoStructClass InfixOp Object where { toDaoStruct=putNullaryUsingShow; }

instance FromDaoStructClass InfixOp Object where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass InfixOp where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass InfixOp where
  haskellDataInterface = interface ADD $ do
    autoDefEquality >> autoDefOrdering >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

allInfixOpChars :: String
allInfixOpChars = "+-*/%<>^&|."

allInfixOpStrs :: String
allInfixOpStrs = " + - * / % ** -> . || && == != | & ^ << >> < > <= >= . -> <- "

----------------------------------------------------------------------------------------------------

data TopLevelEventType
  = BeginExprType | EndExprType | ExitExprType
  deriving (Eq, Ord, Typeable, Enum)

instance Show TopLevelEventType where
  show t = case t of
    BeginExprType -> "BEGIN"
    EndExprType   -> "END"
    ExitExprType  -> "EXIT"

instance Read TopLevelEventType where
  readsPrec _ str = map (\t -> (t, "")) $ case str of
    "BEGIN" -> [BeginExprType]
    "END"   -> [EndExprType]
    "EXIT"  -> [ExitExprType]
    _       -> []

instance NFData TopLevelEventType where { rnf a = seq a () }

instance HasRandGen TopLevelEventType where
  randO = fmap toEnum (nextInt 3)

instance ToDaoStructClass TopLevelEventType Object where { toDaoStruct = putNullaryUsingShow }

instance FromDaoStructClass TopLevelEventType Object where { fromDaoStruct = getNullaryWithRead }

instance ObjectClass TopLevelEventType where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass TopLevelEventType where
  haskellDataInterface = interface BeginExprType $ do
    autoDefEquality >> autoDefOrdering
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

arithPrefixOps :: Array ArithPfxOp (Object -> Exec Object)
arithPrefixOps = array (minBound, maxBound) $ defaults ++
  [ o NEGTIV eval_NEG
  , o POSTIV return
  , o INVB  eval_INVB
  , o NOT   eval_NOT
  ]
  where
    o = (,)
    defaults = flip map [minBound..maxBound] $ \op ->
      (op, \_ -> error $ "no builtin function for prefix "++show op++" operator")

infixOps :: Array InfixOp (Object -> Object -> Exec Object)
infixOps = array (minBound, maxBound) $ defaults ++
  [ o ADD   eval_ADD
  , o SUB   eval_SUB
  , o MULT  eval_MULT
  , o DIV   eval_DIV
  , o MOD   eval_MOD
  , o POW   eval_POW
  , o SHL   eval_SHL
  , o SHR   eval_SHR
  , o ORB   eval_ORB
  , o ANDB  eval_ANDB
  , o XORB  eval_XORB
  , o OR    (error (e "logical-OR" )) -- These probably wont be evaluated. Locgical and/or is a
  , o AND   (error (e "logical-AND")) -- special case to be evaluated in 'evalObjectExprWithLoc'.
  , o EQUL  eval_EQUL
  , o NEQUL eval_NEQUL
  , o GTN   eval_GTN
  , o LTN   eval_LTN
  , o GTEQ  eval_GTEQ
  , o LTEQ  eval_LTEQ
  , o ARROW (error (e "ARROW"))
--  , o DOT   eval_DOT
  ]
  where
    o = (,)
    defaults = flip map [minBound..maxBound] $ \op ->
      (op, \_ _ -> error $ "no builtin function for infix "++show op++" operator")
    e msg = msg ++
      " operator should have been evaluated within the 'execute' function."

updatingOps :: Array UpdateOp (Object -> Object -> Exec Object)
updatingOps = let o = (,) in array (minBound, maxBound) $ defaults ++
  [ o UCONST (\_ b -> return b)
  , o UADD   eval_ADD
  , o USUB   eval_SUB
  , o UMULT  eval_MULT
  , o UDIV   eval_DIV
  , o UMOD   eval_MOD
  , o UORB   eval_ORB
  , o UANDB  eval_ANDB
  , o UXORB  eval_XORB
  , o USHL   eval_SHL
  , o USHR   eval_SHR
  ]
  where
    defaults = flip map [minBound..maxBound] $ \op ->
      (op, \_ _ -> error $ "no builtin function for update operator "++show op)

----------------------------------------------------------------------------------------------------

requireAllStringArgs :: [Object] -> Exec [UStr]
requireAllStringArgs ox = case mapM check (zip (iterate (+(1::Integer)) 0) ox) of
  OK      obj -> return obj
  Backtrack   -> execThrow $ obj [obj "all input parameters must be strings"]
  PFail   err -> execThrow err
  where
    check (i, o) = case o of
      OString o -> return o
      _         -> throwError $
        mkExecError
        { execReturnValue = Just $ obj [obj "requires string parameter, param number:", obj i] }

-- | Given an object, if it is a string return the string characters. If it not a string,
-- depth-recurse into it and extract strings, or if there is an object into which recursion is not
-- possible, pretty-print the object and return the pretty-printed string. The first integer
-- parameter is a depth limit, if recursion into the object exceeds this limit, recursion no longer
-- steps into these objects, the strings returned are the pretty-printed representation of the
-- objects. A pair of 'Data.Either.Either's are returned, references are 'Data.Either.Left',
-- 'Prelude.String's are 'Data.Either.Right'. References are accompanied with their depth so you can
-- choose whether or not you want to dereference or pretty-print them.
getStringsToDepth :: Int -> Object -> [Either (Int, QualRef) String]
getStringsToDepth maxDepth o = loop (0::Int) maxDepth o where
  loop depth remDep o = case o of
    OString   o -> return (Right (uchars o))
    OList    ox -> recurse o ox
    o           -> return (Right (prettyShow o))
    where
      recurse o ox =
        if remDep==0
          then  return (Right (prettyShow o))
          else  ox >>= loop (depth+1) (if remDep>0 then remDep-1 else remDep)

-- | Calls 'getStringsToDepth' and dereferences all 'Data.Either.Left' values below a depth limit,
-- this depth limit is specified by the first argument to this function. The second and third
-- argument to this function are passed directly to 'getStringsToDepth'. Pass a handler to handle
-- references that are undefined.
derefStringsToDepth :: (QualRef -> Object -> Exec [String]) -> Int -> Int -> Object -> Exec [String]
derefStringsToDepth handler maxDeref maxDepth o =
  fmap concat (mapM deref (getStringsToDepth maxDepth o)) where
    deref o = case o of
      Right    o    -> return [o]
      Left (i, ref) ->
        if i>=maxDeref
          then  return [prettyShow ref]
          else  do
            let newMax = if maxDepth>=0 then (if i>=maxDepth then 0 else maxDepth-i) else (0-1)
                recurse = fmap concat . mapM (derefStringsToDepth handler (maxDeref-i) newMax)
            catchReturn (\ _ -> return Nothing) (execute ref) >>= recurse . maybe [] (:[])

-- | Returns a list of all string objects that can be found from within the given list of objects.
-- This function might fail if objects exist that cannot resonably contain strings. If you want to
-- pretty-print non-string objects, try using 'getStringsToDepth'.
recurseGetAllStrings :: Object -> Exec [UStr]
recurseGetAllStrings o = catch (loop [] o) where
  loop ix o = case o of
    OString  o   -> return [o]
    OList    o   -> next OInt (zip [0..] o)
    o            -> throwError $ OList $
      [ obj "object at index", OList ix
      , obj "cannot be evaluated to a string", o
      ]
    where
      next fn = fmap concat . mapM (\ (i, o) -> loop (fn i : ix) o)
  catch ox = case ox of
    FlowErr  err -> execThrow err
    FlowOK    ox -> return ox
    FlowReturn _ -> undefined

----------------------------------------------------------------------------------------------------

-- | All functions that are built-in to the Dao language, or built-in to a library extending the Dao
-- language, are stored in 'Data.Map.Map's from the functions name to an object of this type.
-- Functions of this type are called by 'evalObject' to evaluate expressions written in the Dao
-- language.
data DaoFunc = DaoFunc { autoDerefParams :: Bool, daoForeignCall :: [Object] -> Exec (Maybe Object) }

-- | Execute a 'DaoFunc' 
executeDaoFunc :: Name -> DaoFunc -> ObjListExpr -> Exec (Maybe Object)
executeDaoFunc _op fn params = do
  args <- execute params >>= (if autoDerefParams fn then mapM derefObject else return)
  pval <- catchPredicate (daoForeignCall fn args)
  case pval of
    OK                 obj  -> return obj
    PFail (ExecReturn  obj) -> return obj
    PFail (err@ExecError{}) -> throwError err
    Backtrack               -> mzero

----------------------------------------------------------------------------------------------------

builtin_print :: DaoFunc
builtin_print = DaoFunc True $ \ox_ -> do
  let ox = flip map ox_ $ \o -> case o of
        OString o -> o
        o         -> ustr (showObj o)
  liftIO $ mapM_ (putStrLn . uchars) ox
  return $ Just $ OList $ map OString ox

-- join string elements of a container, pretty prints non-strings and joins those as well.
builtin_join :: DaoFunc
builtin_join = DaoFunc True $ \ox -> case ox of
  [OString j, a] -> joinWith (uchars j) a
  [a]            -> joinWith "" a
  _ -> execThrow $ OList [OList ox, obj "join() function requires one or two parameters"]
  where
    joinWith j =
      fmap (Just . OString . ustr . intercalate j) . derefStringsToDepth (\ _ o -> execThrow o) 1 1

builtin_check_ref :: DaoFunc
builtin_check_ref = DaoFunc True $ \args -> do
  fmap (Just . oBool . and) $ forM args $ \arg -> case arg of
    ORef o -> fmap (maybe False (const True)) (execute o)
    _      -> return True

builtin_delete :: DaoFunc
builtin_delete = DaoFunc True $ \args -> do
  forM_ args $ \arg -> case arg of
    ORef o -> void $ qualRefUpdate o (const (return Nothing))
    _      -> return ()
  return (Just ONull)

-- | Evaluate this function as one of the instructions in the monadic function passed to the
-- 'setupDao' function in order to install the most fundamental functions into the Dao evaluator.
-- This function must be evaluated in order to have access to the following functions:
-- > print, join, defined, delete
evalFuncs :: DaoSetup
evalFuncs = do
  daoFunction "print"   builtin_print
  daoFunction "join"    builtin_join
  daoFunction "defined" builtin_check_ref
  daoFunction "delete"  builtin_delete

----------------------------------------------------------------------------------------------------

data RefExpr = RefExpr Reference Location deriving (Eq, Ord, Typeable, Show)

referenceFromExpr :: RefExpr -> Reference
referenceFromExpr (RefExpr r _) = r

refFromExpr :: RefExpr -> Reference
refFromExpr (RefExpr ref _) = ref

instance Read RefExpr where
  readsPrec prec = readsPrec prec >=> (\ (ref, str) -> return (RefExpr ref LocationUnknown, str))

instance NFData RefExpr where { rnf (RefExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue RefExpr where
  nullValue = RefExpr nullValue LocationUnknown
  testNull (RefExpr a _) = testNull a

instance HasLocation RefExpr where
  getLocation (RefExpr _ loc)     = loc
  setLocation (RefExpr o _  ) loc = RefExpr o loc
  delLocation (RefExpr o _  )     = RefExpr o LocationUnknown

instance UStrType RefExpr where
  toUStr (RefExpr ref _) = toUStr ref
  maybeFromUStr str = fmap (flip RefExpr LocationUnknown) (maybeFromUStr str)
  nil = RefExpr nil LocationUnknown

instance B.Binary RefExpr MTab where
  put (RefExpr a b) = B.put a >> B.put b
  get = pure RefExpr <*> B.get <*> B.get

----------------------------------------------------------------------------------------------------

data QualRefExpr
  = UnqualRefExpr            RefExpr
  | QualRefExpr RefQualifier RefExpr Location
  deriving (Eq, Ord, Typeable, Show)

qualRefFromExpr :: QualRefExpr -> QualRef
qualRefFromExpr o = case o of
  UnqualRefExpr r   -> Unqualified (refFromExpr r)
  QualRefExpr q r _ -> Qualified q (refFromExpr r)

instance NFData QualRefExpr where
  rnf (UnqualRefExpr b  ) = deepseq b ()
  rnf (QualRefExpr a b c) = seq a $! deepseq b $! deepseq c ()

instance HasNullValue QualRefExpr where
  nullValue = UnqualRefExpr nullValue
  testNull (UnqualRefExpr a) = testNull a
  testNull (QualRefExpr _ a _) = testNull a

instance HasLocation QualRefExpr where
  getLocation o     = case o of
    UnqualRefExpr r     -> getLocation r
    QualRefExpr _ _ loc -> loc
  setLocation o loc = case o of
    UnqualRefExpr r     -> UnqualRefExpr (setLocation r loc)
    QualRefExpr q r _   -> QualRefExpr q r loc
  delLocation o     = case o of
    UnqualRefExpr r     -> UnqualRefExpr (delLocation r)
    QualRefExpr q r _   -> QualRefExpr q r LocationUnknown

instance B.Binary QualRefExpr mtab where
  put o = B.prefixByte 0x26 $ case o of
    UnqualRefExpr (RefExpr o loc)     -> B.put (Unqualified o) >> B.put loc
    QualRefExpr q (RefExpr o lo1) lo2 -> B.put (Qualified q o) >> B.put lo1 >> B.put lo2
  get = B.word8PrefixTable <|> fail "expecting QualRefExpr"
instance B.HasPrefixTable QualRefExpr B.Byte mtab where
  prefixTable = B.mkPrefixTableWord8 "QualRefExpr" 0x26 0x26 $
    [ B.get >>= \q -> case q of
        Unqualified o -> UnqualRefExpr . RefExpr o <$> B.get
        Qualified q o -> pure (\lo1 lo2 -> QualRefExpr q (RefExpr o lo1) lo2) <*> B.get <*> B.get
    ]

instance Executable QualRefExpr (Maybe Object) where { execute = return . Just . ORef . qualRefFromExpr }

----------------------------------------------------------------------------------------------------

-- | To evaluate an 'Object' value against a type expression, you can store the
-- 'Object' into a 'TyChkExpr' and 'execute' it. This instance of
-- 'execute' evaluates a type checking monad computing over the 'tyChkExpr' in
-- the 'TyChkExpr'. If the type check determines the 'Object' value does not match, this
-- function backtracks. If the type check is successful, the most general type value for the object
-- if that type value is less-general or as-general as the 'TyChkExpr' provided.
instance Executable (TyChkExpr Object) Object where
  execute tc = case tc of
    NotTypeChecked _          -> return OTrue -- TODO: this needs to return the 'AnyType', not 'OTrue'.
    TypeChecked    _ _ _      -> return OTrue -- TODO: evaluate the actual type checking algorithm here
    DisableCheck   _ _ rslt _ -> return rslt

-- | Called by 'callFunction' to match the list of 'Object's passed as arguments to the function.
-- Returns two 'T_dict's: the first is the 'T_dict' to be passed to 'execFuncPushStack', the second
-- is the dictionary of local variables passed by reference. Backtracks if any types do not match,
-- or if there are an incorrect number of parameters. Backtracking is important because of function
-- overloading.
matchFuncParams :: ParamListExpr -> [Object] -> Exec T_dict
matchFuncParams (ParamListExpr params _) ox =
  loop (0::Int) M.empty (tyChkItem params) ox where
    loop i dict params ox = case ox of
      [] | null params -> return dict
      [] -> mzero -- not enough parameters passed to function
      o:ox -> case params of
        [] -> mzero -- too many parameters passed to function
        ParamExpr passByRef tychk _ : params -> do
          let name = tyChkItem tychk
          execute $ fmap (const o) tychk -- execute (TyChkExpr Object)
          o <- if passByRef then (case o of { ORef _ -> return o; _ -> mzero }) else derefObject o
          loop (i+1) (M.insert name o dict) params ox

-- | A guard script is some Dao script that is executed before or after some event, for example, the
-- code found in the @BEGIN@ and @END@ blocks.
execGuardBlock :: [ScriptExpr] -> Exec ()
execGuardBlock block = void (execFuncPushStack M.empty (mapM_ execute block >> return Nothing) >> return ())

-- | Dereferences the give 'QualRef' and checks if the 'Object' returned has defined a calling
-- routine using 'defCallable' in it's 'haskellDataInterface' table.
callFunction :: QualRef -> ObjListExpr -> Exec (Maybe Object)
callFunction qref params = do
  o <- execute qref
  o <- case o of
    Just  o -> return o
    Nothing -> execThrow $ obj $
      [ obj "function called on undefined reference:", obj qref
      , obj "called with parameters:", new params
      ]
  let err = execThrow $ obj $
        [ obj "reference does not point to callable object:", ORef qref
        , obj "called with parameters:", new params
        , obj "value of object at reference is:", o
        ]
  case o of
    OHaskell (HaskellData o ifc) -> case objCallable ifc of
      Just getFuncs -> do
        params <- execute params
        funcs  <- getFuncs o
        msum $ flip map funcs $ \f -> matchFuncParams (argsPattern f) params >>=
          flip execFuncPushStack (execute $ codeSubroutine f)
      Nothing -> err
    _ -> err

----------------------------------------------------------------------------------------------------

data AST_Ref = AST_RefNull | AST_Ref Name [Com Name] Location deriving (Eq, Ord, Typeable, Show)

astRef :: [Name] -> AST_Ref
astRef nx = if null nx then AST_RefNull else AST_Ref (head nx) (map Com (tail nx)) LocationUnknown

instance HasNullValue AST_Ref where
  nullValue = AST_RefNull
  testNull AST_RefNull = True
  testNull _ = False

instance HasLocation AST_Ref where
  getLocation o     = case o of
    AST_RefNull     -> LocationUnknown
    AST_Ref _ _ loc -> loc
  setLocation o loc = case o of
    AST_RefNull   -> AST_RefNull
    AST_Ref a b _ -> AST_Ref a b loc
  delLocation o     = case o of
    AST_RefNull   -> AST_RefNull
    AST_Ref a b _ -> AST_Ref a b lu

instance PPrintable AST_Ref where
  pPrint ref = case ref of
    AST_RefNull    -> return ()
    AST_Ref n nx _ -> pInline $ pPrint n :
      fmap (\n -> pPrintComWith (\ () -> pString ".") (fmap (const ()) n) >> pPrint (unComment n)) nx

instance PrecedeWithSpace AST_Ref where
  precedeWithSpace r = case r of
    AST_RefNull   -> False
    AST_Ref _ _ _ -> True

instance NFData AST_Ref where
  rnf  AST_RefNull    = ()
  rnf (AST_Ref a b c) = deepseq a $! deepseq b $! deepseq c ()

instance ToDaoStructClass AST_Ref Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_RefNull        -> makeNullary "Null"
    AST_Ref nm nms loc -> renameConstructor "Ref" >>
      "head" .= nm >> "tail" .= nms >> putLocation loc

instance FromDaoStructClass AST_Ref Object where
  fromDaoStruct = msum
    [ constructor "Null" >> return AST_RefNull
    , constructor "Ref"  >> pure AST_Ref <*> req "head" <*> req "tail" <*> location
    ]

instance Intermediate RefExpr AST_Ref where
  toInterm   ast = case ast of
    AST_RefNull             -> [RefExpr (Reference []) LocationUnknown]
    AST_Ref  n nx  loc -> [RefExpr (Reference (n : fmap unComment nx)) loc]
  fromInterm (RefExpr (Reference nx) _) = case nx of
    []   -> [AST_RefNull]
    n:nx -> [AST_Ref n (fmap Com nx) LocationUnknown]

instance ObjectClass AST_Ref where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Ref where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

instance PPrintable RefExpr where pPrint = mapM_ pPrint . fromInterm

instance HasRandGen AST_Ref where
  randO = do
    countNode_
    r <- randList 1 6
    case r of
      []   -> return AST_RefNull
      r:rx -> mapM (randComWith . return) rx >>= \rx -> return (AST_Ref r rx LocationUnknown)

----------------------------------------------------------------------------------------------------

data AST_QualRef
  = AST_Unqualified                      AST_Ref
  | AST_Qualified RefQualifier [Comment] AST_Ref Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_QualRef where
  rnf (AST_Unqualified   c  ) = deepseq c ()
  rnf (AST_Qualified a b c d) = seq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue AST_QualRef where
  nullValue = AST_Unqualified nullValue
  testNull (AST_Unqualified a    ) = testNull a
  testNull (AST_Qualified q a _ _) = case q of { LOCAL -> null a; _ -> False; }

instance HasLocation AST_QualRef where
  getLocation o     = case o of
    AST_Unqualified   r     -> getLocation r
    AST_Qualified _ _ _ loc -> loc
  setLocation o loc = case o of
    AST_Unqualified   r     -> AST_Unqualified (setLocation r loc)
    AST_Qualified q c r _   -> AST_Qualified q c r loc
  delLocation o     = case o of
    AST_Unqualified   r     -> AST_Unqualified   (fd r)
    AST_Qualified q c r _   -> AST_Qualified q c (fd r) LocationUnknown

instance PPrintable AST_QualRef where
  pPrint ref = case ref of
    AST_Unqualified     r   -> pPrint r
    AST_Qualified q com r _ -> pInline [pPrint q, pString " ", pPrint com, pPrint r]

instance ToDaoStructClass AST_QualRef Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_Unqualified r -> innerToStruct r
    AST_Qualified q coms r loc -> do
      renameConstructor "QualRef"
      "qualifier" .= q
      putComments coms
      "ref"       .= r
      putLocation loc

instance FromDaoStructClass AST_QualRef Object where
  fromDaoStruct = msum $
    [ AST_Unqualified <$> mplus (innerFromStruct "Null") (innerFromStruct "Ref")
    , pure AST_Qualified <*> req "qualifier" <*> comments <*> req "ref" <*> location
    ]

instance PrecedeWithSpace AST_QualRef where
  precedeWithSpace r = case r of
    AST_Unqualified   r   -> precedeWithSpace r
    AST_Qualified _ _ r _ -> precedeWithSpace r

instance HasRandGen AST_QualRef where
  randO = do
    countNode_
    n <- nextInt (fromEnum (maxBound::RefQualifier) - fromEnum (minBound::RefQualifier) + 1)
    if n==0
      then  AST_Unqualified <$> randO
      else  pure (AST_Qualified (toEnum (n-1))) <*> randO <*> randO <*> no

instance Intermediate QualRefExpr AST_QualRef where
  toInterm   ast = case ast of
    AST_Unqualified   r     -> liftM  UnqualRefExpr   (ti r)
    AST_Qualified q _ r loc -> liftM3 QualRefExpr [q] (ti r) [loc]
  fromInterm obj = case obj of
    UnqualRefExpr r     -> liftM  AST_Unqualified        (fi r)
    QualRefExpr q r loc -> liftM4 AST_Qualified [q] [[]] (fi r) [loc]

----------------------------------------------------------------------------------------------------

-- $ErrorReporting
-- The 'Procedural' is a continuation monad that can evaluate to an error message without evaluating
-- to "bottom". The error message is any value of type 'Object'. These functions provide
-- a simplified method for constructing error 'Object's.

-- | Convert a 'Dao.Token.Location' to an 'Object' value.
errAt :: Location -> [Object]
errAt loc = case loc of
  LocationUnknown -> []
  loc -> [ OInt (fromIntegral (startingLine loc)), OInt (fromIntegral (startingColumn loc))
         , OInt (fromIntegral (endingLine   loc)), OInt (fromIntegral (endingColumn   loc))
         ]

-- | Evaluate to 'procErr' if the given 'Predicate' is 'Backtrack' or 'PFail'. You must pass a
-- 'Prelude.String' as the message to be used when the given 'Predicate' is 'Backtrack'. You can also
-- pass a list of 'Object's that you are checking, these objects will be included in the
-- 'procErr' value.
--     This function should be used for cases when you have converted 'Object' to a
-- Haskell value, because 'Backtrack' values indicate type exceptions, and 'PFail' values indicate a
-- value error (e.g. out of bounds, or some kind of assert exception), and the messages passed to
-- 'procErr' will indicate this.
checkPredicate :: String -> [Object] -> Exec a -> Exec a
checkPredicate altmsg tried f = do
  pval <- catchPredicate f
  let err = fail (altmsg++" evaulated to void expression")
  case pval of
    OK    a                     -> return a
    Backtrack                   -> err
    PFail (ExecReturn Nothing)  -> err
    PFail  err                  -> throwError $
      err{  execReturnValue = Just $ case execReturnValue err of
              Just (OList ox) -> obj $ tried ++ ox
              Just        o   -> obj $ tried ++ [o]
              Nothing         -> obj tried
         }

-- | 'evalObjectExprExpr' can return 'Data.Maybe.Nothing', and usually this happens when something has
-- failed (e.g. reference lookups), but it is not always an error (e.g. a void list of argument to
-- functions). If you want 'Data.Maybe.Nothing' to cause an error, evaluate your
-- @'Exec' ('Data.Maybe.Maybe' 'Object')@ as a parameter to this function.
checkVoid :: Location -> String -> Maybe a -> Exec a
checkVoid loc msg fn = case fn of
  Nothing -> execThrow $ obj $ errAt loc ++ [obj msg, obj "evaluates to a void"]
  Just  a -> return a

----------------------------------------------------------------------------------------------------

-- | Required parenthesese.
data ParenExpr = ParenExpr AssignExpr Location deriving (Eq, Ord, Typeable, Show)

evalConditional :: ParenExpr -> Exec Bool
evalConditional obj =
  (execute obj :: Exec (Maybe Object)) >>=
    checkVoid (getLocation obj) "conditional expression to if statement" >>=
      execHandleIO [fmap (const False) execIOHandler] . return . testNull

instance HasLocation ParenExpr where
  getLocation (ParenExpr _ loc)     = loc
  setLocation (ParenExpr o _  ) loc = ParenExpr o loc
  delLocation (ParenExpr o _  )     = ParenExpr (delLocation o) LocationUnknown

instance HasNullValue ParenExpr where
  nullValue = ParenExpr nullValue LocationUnknown
  testNull (ParenExpr a _) = testNull a

instance NFData ParenExpr where { rnf (ParenExpr a b) = deepseq a $! deepseq b () }

instance B.Binary ParenExpr MTab where
  put (ParenExpr a b) = B.prefixByte 0x27 $ B.put a >> B.put b
  get = B.word8PrefixTable <|> fail "expecting ParenExpr"
instance B.HasPrefixTable ParenExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "ParenExpr" 0x27 0x27 $
    [pure ParenExpr <*> B.get <*> B.get]

instance Executable ParenExpr (Maybe Object) where { execute (ParenExpr a _) = execute a }

instance ObjectClass ParenExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ParenExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AST_Paren = AST_Paren (Com AST_Assign) Location deriving (Eq, Ord, Typeable, Show)

instance HasLocation AST_Paren where
  getLocation (AST_Paren _ loc)     = loc
  setLocation (AST_Paren o _  ) loc = AST_Paren o loc
  delLocation (AST_Paren o _  )     = AST_Paren (delLocation o) LocationUnknown

instance HasNullValue AST_Paren where
  nullValue = AST_Paren nullValue LocationUnknown
  testNull (AST_Paren a _) = testNull a

instance NFData AST_Paren where { rnf (AST_Paren a b) = deepseq a $! deepseq b () }

instance PPrintable AST_Paren where
  pPrint (AST_Paren o _) = pInline [pString "(", pPrint o, pString ")"]

instance Intermediate ParenExpr AST_Paren where
  toInterm   (AST_Paren o loc) = liftM2 ParenExpr (uc0 o) [loc]
  fromInterm (ParenExpr o loc) = liftM2 AST_Paren (nc0 o) [loc]

instance ToDaoStructClass AST_Paren Object where
  toDaoStruct = renameConstructor "Paren" >> ask >>= \o -> case o of
    AST_Paren paren loc -> "inside" .= paren >> putLocation loc

instance FromDaoStructClass AST_Paren Object where
  fromDaoStruct = constructor "Paren" >> pure AST_Paren <*> req "inside" <*> location

instance HasRandGen AST_Paren where { randO = recurse nullValue $ pure AST_Paren <*> randO <*> no }

instance ObjectClass AST_Paren where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Paren where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data IfExpr = IfExpr ParenExpr CodeBlock Location deriving (Eq, Ord, Typeable, Show)

instance NFData IfExpr where { rnf (IfExpr a b c) = deepseq a $! deepseq b $! deepseq c () }

instance NFData CodeBlock where { rnf (CodeBlock a) = deepseq a () }

instance HasNullValue IfExpr where
  nullValue = IfExpr nullValue nullValue LocationUnknown
  testNull (IfExpr a b _) = testNull a && testNull b

instance HasLocation IfExpr where
  getLocation (IfExpr _ _ loc)     = loc
  setLocation (IfExpr a b _  ) loc = IfExpr a b loc
  delLocation (IfExpr a b _  )     = IfExpr (delLocation a) (delLocation b) LocationUnknown

instance B.Binary IfExpr MTab where
  put (IfExpr a b c) = B.put a >> B.put b >> B.put c
  get = pure IfExpr <*> B.get <*> B.get <*> B.get

instance Executable IfExpr Bool where
  execute (IfExpr ifn thn _) = execNested M.empty $
    evalConditional ifn >>= \test -> when test (execute thn) >> return test

instance ObjectClass IfExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass IfExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_If = AST_If (Com AST_Paren) AST_CodeBlock Location deriving (Eq, Ord, Typeable, Show)

instance NFData AST_If where { rnf (AST_If a b c) = deepseq a $! deepseq b $! deepseq c () }

instance HasLocation AST_If where
  getLocation (AST_If _ _ loc)     = loc
  setLocation (AST_If a b _  ) loc = AST_If a b loc
  delLocation (AST_If a b _  )     = AST_If (delLocation a) (delLocation b) LocationUnknown

instance HasNullValue AST_If where
  nullValue = AST_If nullValue nullValue LocationUnknown
  testNull (AST_If a b _) = testNull a && testNull b

instance PPrintable AST_If where
  pPrint (AST_If ifn thn _) =
    pClosure (pString "if" >> pPrint ifn) "{" "}" [pPrint thn]

instance ToDaoStructClass AST_If Object where
  toDaoStruct = renameConstructor "Conditional" >> ask >>= \o -> case o of
    AST_If ifn thn loc -> "condition" .= ifn >> "action" .= thn >> putLocation loc

instance FromDaoStructClass AST_If Object where
  fromDaoStruct = constructor "Conditional" >>
    pure AST_If <*> req "condition" <*> req "action" <*> location

instance HasRandGen AST_If where { randO = countNode $ pure AST_If <*> randO <*> randO <*> no }

instance Intermediate IfExpr AST_If where
  toInterm   (AST_If a b loc) = liftM3 IfExpr (uc0 a) (ti  b) [loc]
  fromInterm (IfExpr a b loc) = liftM3 AST_If (nc0 a) (fi  b) [loc]

instance ObjectClass AST_If where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_If where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data ElseExpr = ElseExpr IfExpr Location deriving (Eq, Ord, Typeable, Show)

instance NFData ElseExpr where { rnf (ElseExpr   a b  ) = deepseq a $! deepseq b $! () }

instance HasNullValue ElseExpr where
  nullValue = ElseExpr nullValue LocationUnknown
  testNull (ElseExpr a _) = testNull a

instance HasLocation ElseExpr where
  getLocation (ElseExpr _ loc)     = loc
  setLocation (ElseExpr a _  ) loc = ElseExpr a loc
  delLocation (ElseExpr a _  )     = ElseExpr (delLocation a) LocationUnknown

instance B.Binary ElseExpr MTab where
  put (ElseExpr a b) = B.prefixByte 0x3C $ B.put a >> B.put b
  get = (B.tryWord8 0x3C $ pure ElseExpr <*> B.get <*> B.get) <|> fail "expecting ElseExpr"

instance Executable ElseExpr Bool where { execute (ElseExpr ifn _) = execute ifn }

instance ObjectClass ElseExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ElseExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_Else = AST_Else (Com ()) AST_If Location deriving (Eq, Ord, Typeable, Show)
  -- ^ @/**/ else /**/ if /**/ obj /**/ {}@

instance NFData AST_Else where { rnf (AST_Else a b c) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue AST_Else where
  nullValue = AST_Else nullValue nullValue LocationUnknown
  testNull (AST_Else a b _) = testNull a && testNull b

instance HasLocation AST_Else where
  getLocation (AST_Else _ _ loc)     = loc
  setLocation (AST_Else a b _  ) loc = AST_Else a b loc
  delLocation (AST_Else a b _  )     = AST_Else a (delLocation b) LocationUnknown

instance PPrintable AST_Else where
  pPrint (AST_Else coms (AST_If ifn thn _) _) =
    pClosure (pPrintComWith (\ () -> pString "else ") coms >> pString "if" >> pPrint ifn) "{" "}" [pPrint thn]

instance ToDaoStructClass AST_Else Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_Else coms ifn loc ->
      renameConstructor "ElseIf" >> "comments" .= coms >> "elseIf" .= ifn >> putLocation loc

instance FromDaoStructClass AST_Else Object where
  fromDaoStruct = constructor "ElseIf" >>
    pure AST_Else <*> req "comments" <*> req "elseIf" <*> location

instance HasRandGen AST_Else where { randO = countNode $ pure AST_Else <*> randO <*> randO <*> no }

instance Intermediate ElseExpr AST_Else where
  toInterm   (AST_Else _ a loc) = liftM2 ElseExpr          (ti  a) [loc]
  fromInterm (ElseExpr   a loc) = liftM3 AST_Else [Com ()] (fi  a) [loc]

instance ObjectClass AST_Else where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Else where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data IfElseExpr = IfElseExpr IfExpr [ElseExpr] (Maybe CodeBlock) Location deriving (Eq, Ord, Typeable, Show)

instance NFData IfElseExpr where
  rnf (IfElseExpr a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue IfElseExpr where
  nullValue = IfElseExpr nullValue [] Nothing LocationUnknown
  testNull (IfElseExpr a [] Nothing _) = testNull a
  testNull _ = False

instance HasLocation IfElseExpr where
  getLocation (IfElseExpr _ _ _ loc)     = loc
  setLocation (IfElseExpr a b c _  ) loc = IfElseExpr a b c loc
  delLocation (IfElseExpr a b c _  )     =
    IfElseExpr (delLocation a) (fmap delLocation b) (fmap delLocation c) LocationUnknown

instance B.Binary IfElseExpr MTab where
  put (IfElseExpr a b c d) = B.prefixByte 0x41 $ B.put a >> B.put b >> B.put c >> B.put d
  get = B.word8PrefixTable <|> fail "expecting IfElseExpr"

instance B.HasPrefixTable IfElseExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "IfElseExpr" 0x41 0x41 $
    [pure IfElseExpr <*> B.get <*> B.get <*> B.get <*> B.get]

instance Executable IfElseExpr () where
  execute (IfElseExpr ifn elsx final _loc) = do
    let tryEach elsx = case elsx of
          []       -> return False
          els:elsx -> execute els >>= \ok -> if ok then return ok else tryEach elsx
    (execute ifn >>= \ok ->
      if ok then return Nothing
            else tryEach elsx >>= \ok ->
                 if ok then return Nothing
                       else return final) >>= maybe (return ()) execute

instance ObjectClass IfElseExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass IfElseExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_IfElse = AST_IfElse AST_If [AST_Else] (Com ()) (Maybe AST_CodeBlock) Location
  -- ^ @if /**/ obj /**/ {} /**/ else /**/ if /**/ obj /**/ {} /**/ else {}@
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_IfElse where
  rnf (AST_IfElse a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()

instance HasNullValue AST_IfElse where
  nullValue = AST_IfElse nullValue [] nullValue Nothing LocationUnknown
  testNull (AST_IfElse a [] (Com ()) Nothing _) = testNull a
  testNull _ = False

instance HasLocation AST_IfElse where
  getLocation (AST_IfElse _ _ _ _ loc)     = loc
  setLocation (AST_IfElse a b c d _  ) loc = AST_IfElse a b c d loc
  delLocation (AST_IfElse a b c d _  )     = AST_IfElse (delLocation a) (fmap delLocation b) c (fmap delLocation d) LocationUnknown

instance PPrintable AST_IfElse where
  pPrint (AST_IfElse ifn els coms deflt _) = do
    pPrint ifn >> pNewLine
    mapM_ pPrint els >> pNewLine
    case deflt of
      Nothing    -> return ()
      Just deflt -> pClosure (pPrintComWith (\ () -> pString "else") coms) "{" "}" [pPrint deflt]

instance ToDaoStructClass AST_IfElse Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_IfElse ifn els coms block loc -> do
      renameConstructor "If"
      "test" .= ifn >> "alt" .= (listToObj els) >> "comments" .= coms
      maybe (return ()) (void . defObjField "finalElse") block
      putLocation loc

instance FromDaoStructClass AST_IfElse Object where
  fromDaoStruct = constructor "If" >>
    pure AST_IfElse
      <*> req "test"
      <*> reqList "alt"
      <*> req "comments"
      <*> opt "finalElse"
      <*> location

instance HasRandGen AST_IfElse where { randO = countNode $ pure AST_IfElse <*> randO <*> randList 0 4 <*> randO <*> randO <*> no }

instance Intermediate IfElseExpr AST_IfElse where
  toInterm   (AST_IfElse a b _ c loc) = liftM4 IfElseExpr (ti  a) [b>>=ti]          (um1 c) [loc]
  fromInterm (IfElseExpr a b   c loc) = liftM5 AST_IfElse (fi  a) [b>>=fi] [Com ()] (nm1 c) [loc]

instance ObjectClass AST_IfElse where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_IfElse where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

newtype WhileExpr = WhileExpr  IfExpr deriving (Eq, Ord, Typeable, Show)

instance NFData WhileExpr  where { rnf (WhileExpr (IfExpr a b c)) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue WhileExpr where
  nullValue = WhileExpr nullValue
  testNull (WhileExpr a) = testNull a

instance HasLocation WhileExpr where
  getLocation (WhileExpr a)     = getLocation a
  setLocation (WhileExpr a) loc = WhileExpr (setLocation a loc)
  delLocation (WhileExpr a)     = WhileExpr (delLocation a)

instance B.Binary WhileExpr MTab where
  put (WhileExpr o) = B.prefixByte 0x42 $ B.put o
  get = B.word8PrefixTable <|> fail "expecting WhileExpr"

instance B.HasPrefixTable WhileExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "WhileExpr" 0x42 0x42 [WhileExpr <$> B.get]

instance Executable WhileExpr () where
  execute (WhileExpr ifn) = let loop = execute ifn >>= flip when loop in loop

instance ObjectClass WhileExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass WhileExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

newtype AST_While = AST_While AST_If deriving (Eq, Ord, Typeable, Show)

instance NFData AST_While where { rnf (AST_While (AST_If a b c)) = deepseq a $! deepseq b $! deepseq c () }

instance HasNullValue AST_While where
  nullValue = AST_While nullValue
  testNull (AST_While a) = testNull a

instance HasLocation AST_While where
  getLocation (AST_While a) = getLocation a
  setLocation (AST_While a) loc = AST_While (setLocation a loc)
  delLocation (AST_While a)     = AST_While (delLocation a)

instance PPrintable AST_While where
  pPrint (AST_While (AST_If ifn thn _)) =
    pClosure (pInline [pString "while", pPrint ifn]) "{" "}" [pPrint thn]

instance Intermediate WhileExpr AST_While where
  toInterm   (AST_While a) = liftM WhileExpr (ti a)
  fromInterm (WhileExpr a) = liftM AST_While (fi a)

instance ToDaoStructClass AST_While Object where
  toDaoStruct = ask >>= \ (AST_While o) -> innerToStruct o >> renameConstructor "While"

instance FromDaoStructClass AST_While Object where
  fromDaoStruct = constructor "While" >> AST_While <$> innerFromStruct "Conditional"

instance HasRandGen AST_While  where { randO = AST_While <$> randO }

instance ObjectClass AST_While where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_While where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data ScriptExpr
  = IfThenElse   IfElseExpr
  | WhileLoop    WhileExpr
  | RuleFuncExpr RuleFuncExpr
  | EvalObject   AssignExpr                               Location -- location of the semicolon
  | TryCatch     CodeBlock (Maybe Name) (Maybe CodeBlock) Location
  | ForLoop      Name       ParenExpr    CodeBlock        Location
  | ContinueExpr Bool       AssignExpr                    Location
  | ReturnExpr   Bool       AssignExpr                    Location
  | WithDoc      ParenExpr  CodeBlock                     Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData ScriptExpr where
  rnf (IfThenElse   a      ) = deepseq a ()
  rnf (WhileLoop    a      ) = deepseq a ()
  rnf (RuleFuncExpr a      ) = deepseq a ()
  rnf (EvalObject   a b    ) = deepseq a $! deepseq b ()
  rnf (TryCatch     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ForLoop      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (ContinueExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (ReturnExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (WithDoc      a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue ScriptExpr where
  nullValue = EvalObject nullValue LocationUnknown
  testNull (EvalObject a _) = testNull a
  testNull _ = False

instance HasLocation ScriptExpr where
  getLocation o = case o of
    EvalObject   _     o -> o
    IfThenElse         o -> getLocation o
    RuleFuncExpr       o -> getLocation o
    WhileLoop          o -> getLocation o
    TryCatch     _ _ _ o -> o
    ForLoop      _ _ _ o -> o
    ContinueExpr _ _   o -> o
    ReturnExpr   _ _   o -> o
    WithDoc      _ _   o -> o
  setLocation o loc = case o of
    EvalObject   a     _ -> EvalObject   a     loc
    IfThenElse   a       -> IfThenElse   (setLocation a loc)
    WhileLoop    a       -> WhileLoop    (setLocation a loc)
    RuleFuncExpr a       -> RuleFuncExpr (setLocation a loc)
    TryCatch     a b c _ -> TryCatch     a b c loc
    ForLoop      a b c _ -> ForLoop      a b c loc
    ContinueExpr a b   _ -> ContinueExpr a b   loc
    ReturnExpr   a b   _ -> ReturnExpr   a b   loc
    WithDoc      a b   _ -> WithDoc      a b   loc
  delLocation o = case o of
    EvalObject   a     _ -> EvalObject   (fd a)                   lu
    IfThenElse   a       -> IfThenElse   (fd a)
    WhileLoop    a       -> WhileLoop    (fd a)
    RuleFuncExpr a       -> RuleFuncExpr (fd a)
    TryCatch     a b c _ -> TryCatch     (fd a)     b (fmap fd c) lu
    ForLoop      a b c _ -> ForLoop          a  (fd b)     (fd c) lu
    ContinueExpr a b   _ -> ContinueExpr     a  (fd b)            lu
    ReturnExpr   a b   _ -> ReturnExpr       a  (fd b)            lu
    WithDoc      a b   _ -> WithDoc      (fd a) (fd b)            lu
    where
      lu = LocationUnknown
      fd :: HasLocation a => a -> a
      fd = delLocation

script_intrm :: String
script_intrm = "script intermedaite node"

instance B.Binary ScriptExpr MTab where
  put o = case o of
    IfThenElse   a           -> B.put a
    WhileLoop    a           -> B.put a
    RuleFuncExpr a           -> B.put a
    EvalObject   a         z -> B.prefixByte 0x43 $ B.put a >> B.put z
    TryCatch     a     b c z -> B.prefixByte 0x44 $ B.put a >> B.put b >> B.put c >> B.put z
    ForLoop      a     b c z -> B.prefixByte 0x45 $ B.put a >> B.put b >> B.put c >> B.put z
    ContinueExpr True  b   z -> B.prefixByte 0x46 $ B.put b >> B.put z
    ContinueExpr False b   z -> B.prefixByte 0x47 $ B.put b >> B.put z
    ReturnExpr   True  b   z -> B.prefixByte 0x48 $ B.put b >> B.put z
    ReturnExpr   False b   z -> B.prefixByte 0x49 $ B.put b >> B.put z
    WithDoc      a     b   z -> B.prefixByte 0x4A $ B.put a >> B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting ScriptExpr"

instance B.HasPrefixTable ScriptExpr B.Byte MTab where
  prefixTable = mconcat $
    [ fmap IfThenElse B.prefixTable
    , fmap WhileLoop  B.prefixTable
    , fmap RuleFuncExpr B.prefixTable
    , B.mkPrefixTableWord8 "ScriptExpr" 0x43 0x4A $
        [ pure EvalObject   <*> B.get <*> B.get
        , pure TryCatch     <*> B.get <*> B.get <*> B.get <*> B.get
        , pure ForLoop      <*> B.get <*> B.get <*> B.get <*> B.get
        , pure (ContinueExpr True ) <*> B.get <*> B.get
        , pure (ContinueExpr False) <*> B.get <*> B.get
        , pure (ReturnExpr   True ) <*> B.get <*> B.get
        , pure (ReturnExpr   False) <*> B.get <*> B.get
        , pure WithDoc      <*> B.get <*> B.get <*> B.get
        ]
    ]

localVarDefine :: Name -> Object -> Exec ()
localVarDefine nm obj = asks execStack >>= \sto -> storeDefine sto nm obj

-- | Convert a single 'ScriptExpr' into a function of value @'Exec' 'Object'@.
instance Executable ScriptExpr () where
  execute script = _setScriptExprError script $ case script of
    IfThenElse   ifn    -> execute ifn
    WhileLoop    ifn    -> execute ifn
    EvalObject   o _loc -> void (execute o :: Exec (Maybe Object))
    RuleFuncExpr rulfn  -> do
      o <- execute rulfn
      (StaticStore sub) <- asks currentCodeBlock
      let dyn o = case o of
            OHaskell (HaskellData h _) -> fromDynamic h
            _ -> Nothing
      let pushItem insert = case o >>= dyn of
            Just  o -> liftIO (insert o)
            Nothing -> error "executing RuleFuncExpr does not produce object of correct data type"
      case sub of
        Nothing  -> case rulfn of
          LambdaExpr{} -> asks lambdaSet >>= \s -> pushItem $ \o -> modifyIORef s (++[o])
          RuleExpr{}   -> asks ruleSet >>= \s ->
            pushItem $ \o -> modifyIORef s (insertMultiPattern (++) (globPattern o) [globSubroutine o])
          FuncExpr{}   -> return ()
            -- function expressions are placed in the correct store by 'execute'
        Just sub -> case rulfn of
          LambdaExpr{} -> pushItem $ \o -> modifyIORef (staticLambdas sub) (++[o])
          RuleExpr{}   -> pushItem $ \o ->
            modifyIORef (staticRules sub) (insertMultiPattern (++) (globPattern o) [globSubroutine o])
          FuncExpr{}   -> return ()
            -- function expressions are placed in the correct store by 'execute'
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    TryCatch try  name  catch _loc -> do
      ce <- catchPredicate (execNested M.empty $ execute try)
      case ce of
        OK               ()  -> return ()
        Backtrack            -> mzero
        PFail (ExecReturn{}) -> return ()
        PFail            err -> do
          let tryCatch = maybe (return ()) (execNested M.empty . execute) catch
          case name of
            Nothing -> tryCatch
            Just nm -> case catch of
              Nothing    -> tryCatch
              Just catch -> execNested M.empty (localVarDefine nm (new err) >> execute catch)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ForLoop varName inObj thn _loc -> do
      let loop newList ox = case ox of
            []   -> return newList
            o:ox -> do
              (shouldContinue, o) <- execute (ForLoopBlock varName o thn)
              let next = newList ++ maybe [] (:[]) o
              if shouldContinue then loop next ox else return (next++ox)
      objRef <- execute inObj >>=
        checkVoid (getLocation inObj) "value over which to iterate \"for\" statement"
      case objRef of
        ORef qref -> void $ qualRefUpdate qref $ \o -> case o of
          Nothing -> return Nothing
          Just _o -> execute qref >>=
            checkVoid (getLocation inObj) "reference over which to iterate evaluates to void" >>= \objRef ->
              fmap Just (iterateObject objRef >>= loop [] >>= foldObject objRef)
        o         -> void (iterateObject o >>= loop [])
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ContinueExpr a    _      _loc -> execThrow $ obj $
      '"':(if a then "continue" else "break")++"\" expression is not within a \"for\" or \"while\" loop"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ReturnExpr returnStmt o _loc -> do
      o <- (execute o :: Exec (Maybe Object)) >>= maybe (return Nothing) (fmap Just . derefObject)
      if returnStmt then throwError (ExecReturn o) else execThrow (maybe ONull id o)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    WithDoc   expr   _thn    _loc -> void $ execNested M.empty $ do
      o   <- execute expr >>= checkVoid (getLocation expr) "expression in the focus of \"with\" statement"
      ref <- mplus (asReference o) $ execThrow $ obj $
        [obj "expression in \"with\" statement does not evaluate to a reference, evaluates to a", o]
      qualRefUpdate ref $ \o -> case o of
        Nothing -> execThrow $ obj [obj "undefined reference:", ORef ref]
        Just o  -> do
          let ok = do
                ioref <- liftIO (newIORef o)
                execNested M.empty $
                  local (\x->x{currentWithRef=WithRefStore (Just ioref)}) (execute expr)
                liftIO (fmap Just (readIORef ioref))
          case o of
            OTree    _ -> ok
            OHaskell (HaskellData _ ifc) -> case objFromStruct ifc of
              Just  _ -> ok
              Nothing -> execThrow $ obj $
                [ obj "object of type:", obj (toUStr (show (objHaskellType ifc)))
                , obj "is not defined to be used in \"with\" statements"
                ]
            _ -> execThrow $ obj $
              [ obj "object value at reference:", ORef ref
              , obj "has no method of being used in a \"with\" statement, value is", o
              ]
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass ScriptExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ScriptExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

-- | This data type instantates the 'execute' function for use in for-loop expressions.
data ForLoopBlock = ForLoopBlock Name Object CodeBlock

-- | Like evaluating 'execute' on a value of 'QualRefExpr', except the you are evaluating an
-- 'Object' type. If the value of the 'Object' is not constructed with
-- 'ORef', the object value is returned unmodified.
derefObject :: Object -> Exec Object
derefObject o = do
  let cantDeref msg = execThrow $ obj [obj msg, o]
  maybe (cantDeref "undefined reference:") return =<< case o of
    ORef o -> execute o
    OHaskell (HaskellData o ifc) -> case objDereferencer ifc of
      Nothing    -> cantDeref "value cannot be used as reference:"
      Just deref -> deref o
    o -> return (Just o)

instance Executable ForLoopBlock (Bool, Maybe Object) where
  execute (ForLoopBlock name o block) = 
    execNested (M.singleton name o) $ loop (codeBlock block) where
      done cont = do
        (LocalStore ref) <- asks execStack
        newValue <- liftIO (fmap (M.lookup name . head . mapList) (readIORef ref))
        return (cont, newValue)
      loop ex = case ex of
        []   -> done True
        e:ex -> case e of
          ContinueExpr a cond _loc -> case cond of
            EvalExpr (ObjectExpr VoidExpr) -> done a
            cond -> execute cond >>= maybe err objToBool >>= done . (if a then id else not) where
              err = fail "expression does not evaluate to boolean"
          e -> execute e >> loop ex

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that controls the flow of script
-- exectuion.
data AST_Script
  = AST_Comment     [Comment] 
  | AST_IfThenElse  AST_IfElse
  | AST_WhileLoop   AST_While
  | AST_RuleFunc    AST_RuleFunc
  | AST_EvalObject  AST_Assign  [Comment]                                              Location
    -- ^ @some.object.expression = for.example - equations || function(calls) /**/ ;@
  | AST_TryCatch     (Com AST_CodeBlock)     (Maybe (Com Name)) (Maybe AST_CodeBlock)  Location
    -- ^ @try /**/ {} /**/ catch /**/ errVar /**/ {}@              
  | AST_ForLoop      (Com Name)              (Com AST_Paren)           AST_CodeBlock   Location
    -- ^ @for /**/ var /**/ in /**/ objExpr /**/ {}@
  | AST_ContinueExpr Bool  [Comment]         (Com AST_Assign)                          Location
    -- ^ The boolean parameter is True for a "continue" statement, False for a "break" statement.
    -- @continue /**/ ;@ or @continue /**/ if /**/ objExpr /**/ ;@
  | AST_ReturnExpr   Bool                    (Com AST_Assign)                          Location
    -- ^ The boolean parameter is True for a "return" statement, False for a "throw" statement.
    -- ^ @return /**/ ;@ or @return /**/ objExpr /**/ ;@
  | AST_WithDoc      (Com AST_Paren)         AST_CodeBlock                             Location
    -- ^ @with /**/ objExpr /**/ {}@
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Script where
  rnf (AST_Comment      a      ) = deepseq a ()
  rnf (AST_IfThenElse   a      ) = deepseq a ()
  rnf (AST_WhileLoop    a      ) = deepseq a ()
  rnf (AST_RuleFunc     a      ) = deepseq a ()
  rnf (AST_EvalObject   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_TryCatch     a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ForLoop      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ContinueExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_ReturnExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_WithDoc      a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_Script where
  nullValue = AST_EvalObject nullValue [] LocationUnknown
  testNull (AST_EvalObject a _ _) = testNull a
  testNull _ = False

instance HasLocation AST_Script where
  getLocation o = case o of
    AST_Comment      _       -> lu
    AST_EvalObject   _   _ o -> o
    AST_IfThenElse         o -> getLocation o
    AST_WhileLoop          o -> getLocation o
    AST_RuleFunc           o -> getLocation o
    AST_TryCatch     _ _ _ o -> o
    AST_ForLoop      _ _ _ o -> o
    AST_ContinueExpr _ _ _ o -> o
    AST_ReturnExpr   _ _   o -> o
    AST_WithDoc      _ _   o -> o
  setLocation o loc = case o of
    AST_Comment      a       -> AST_Comment      a
    AST_EvalObject   a b   _ -> AST_EvalObject   a b   loc
    AST_IfThenElse   a       -> AST_IfThenElse   (setLocation a loc)
    AST_WhileLoop    a       -> AST_WhileLoop    (setLocation a loc)
    AST_RuleFunc     a       -> AST_RuleFunc     (setLocation a loc)
    AST_TryCatch     a b c _ -> AST_TryCatch     a b c loc
    AST_ForLoop      a b c _ -> AST_ForLoop      a b c loc
    AST_ContinueExpr a b c _ -> AST_ContinueExpr a b c loc
    AST_ReturnExpr   a b   _ -> AST_ReturnExpr   a b   loc
    AST_WithDoc      a b   _ -> AST_WithDoc      a b   loc
  delLocation o = case o of
    AST_Comment      a       -> AST_Comment           a
    AST_EvalObject   a b   _ -> AST_EvalObject   (fd  a)      b              lu
    AST_IfThenElse   a       -> AST_IfThenElse   (fd  a)
    AST_WhileLoop    a       -> AST_WhileLoop    (fd  a)
    AST_RuleFunc     a       -> AST_RuleFunc     (fd  a)
    AST_TryCatch     a b c _ -> AST_TryCatch     (fd1 a)      b  (fmap fd c) lu
    AST_ForLoop      a b c _ -> AST_ForLoop           a  (fd1 b) (fd  c)     lu
    AST_ContinueExpr a b c _ -> AST_ContinueExpr      a       b  (fd1 c)     lu
    AST_ReturnExpr   a b   _ -> AST_ReturnExpr        a  (fd1 b)             lu
    AST_WithDoc      a b   _ -> AST_WithDoc      (fd1 a) (fd  b)             lu

instance PPrintable AST_Script where
  pPrint expr = pGroup True $ case expr of
    AST_Comment             coms -> mapM_ pPrint coms
    AST_EvalObject   objXp  coms                    _ ->
      pPrint objXp >> mapM_ pPrint coms >> pString ";"
    AST_IfThenElse   ifXp                             -> pPrint ifXp
    AST_WhileLoop    whileLoop                        -> pPrint whileLoop
    AST_RuleFunc     ruleOrFunc                       -> pPrint ruleOrFunc
    AST_TryCatch     cxcScrpXp  cQRef     xcScrpXp  _ -> do
      pClosure (pString "try") "{" "}" [pPrint cxcScrpXp]
      maybe (return ()) id $ msum $
        [ cQRef >>= \qref -> xcScrpXp >>= \xscrp -> Just $
            pClosure (pString "catch " >> pPrint qref) "{" "}" [pPrint xscrp]
        , cQRef    >>= \qref  -> Just $ pString "catch " >> pPrint qref >> pString ";"
        , xcScrpXp >>= \xscrp -> Just $ pClosure (pString "catch ") "{" "}" [pPrint xscrp]
        ]
    AST_ForLoop      cNm        cObjXp    xcScrpXp  _ ->
      pPrintSubBlock (pString "for " >> pPrint cNm >> pString " in " >> pPrint cObjXp) xcScrpXp
    AST_ContinueExpr contin     coms      cObjXp    _ -> pWrapIndent $
      [ pString (if contin then "continue" else "break")
      , pInline (map pPrint coms)
      , pString " if" >> when (precedeWithSpace cObjXp) (pString " ") >> pPrint cObjXp
      , pString ";"
      ]
    AST_ReturnExpr   retrn                cObjXp    _ -> pWrapIndent $
      [pString (if retrn then "return " else "throw "), pPrint cObjXp, pString ";"]
    AST_WithDoc      cObjXp               xcScrpXp  _ ->
      pPrintSubBlock (pString "with " >> pPrint cObjXp) xcScrpXp

instance ToDaoStructClass AST_Script Object where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Comment      a         -> nm "Comment" >> putComments a
    AST_IfThenElse   a         -> innerToStruct a
    AST_WhileLoop    a         -> innerToStruct a
    AST_RuleFunc     a         -> innerToStruct a
    AST_EvalObject   a b   loc -> nm "ObjectExpr" >> "expr" .= a >> putComments b >> putLocation loc
    AST_TryCatch     a b c loc -> nm "TryCatch" >>
      "tryBlock" .= a >> "varName" .=? b >> "catchBlock" .=? c >> putLocation loc
    AST_ForLoop      a b c loc -> nm "ForLoop" >>
      "varName" .= a >> "iterate" .= b >> "block" .= c >> putLocation loc
    AST_ContinueExpr a b c loc -> do
      nm (if a then "Continue" else "Break")
      putComments b >> "condition" .= c >> putLocation loc
    AST_ReturnExpr   a b   loc -> do
      nm (if a then "Return" else "Throw")
      "expr" .= b >> putLocation loc
    AST_WithDoc      a b   loc -> nm "WithDoc" >> "expr" .= a >> "block" .= b >> putLocation loc

instance FromDaoStructClass AST_Script Object where
  fromDaoStruct = msum $
    [ constructor "Comment" >> AST_Comment <$> comments
    , AST_IfThenElse <$> fromDaoStruct
    , AST_WhileLoop  <$> fromDaoStruct
    , AST_RuleFunc   <$> fromDaoStruct
    , constructor "ObjectExpr" >> pure AST_EvalObject <*> req "expr" <*> comments <*> location
    , constructor "TryCatch" >>
        pure AST_TryCatch <*> req "tryBlock" <*> opt "varName" <*> opt "catchBlock" <*> location
    , constructor "ForLoop" >>
        pure AST_ForLoop <*> req "varName" <*> req "iterate" <*> req "block" <*> location
    , constructor "Continue" >>
        pure (AST_ContinueExpr True ) <*> comments <*> req "condition" <*> location
    , constructor "Break" >>
        pure (AST_ContinueExpr False) <*> comments <*> req "condition" <*> location
    , constructor "Return" >> pure (AST_ReturnExpr True ) <*> req "expr" <*> location
    , constructor "Throw"  >> pure (AST_ReturnExpr False) <*> req "expr" <*> location
    , constructor "WithDoc" >> pure AST_WithDoc <*> req "expr" <*> req "block" <*> location
    ]

instance HasRandGen AST_Script where
  randO = recurseRunRandChoice nullValue
  randChoice = randChoiceList $
    [ pure AST_EvalObject   <*> randO <*> randO <*> no
    , pure AST_IfThenElse   <*> randO
    , pure AST_WhileLoop    <*> randO
    , pure AST_RuleFunc     <*> randO
    , pure AST_TryCatch     <*> randO <*> randO <*> randO <*> no
    , pure AST_ForLoop      <*> randO <*> randO <*> randO <*> no
    , pure AST_ContinueExpr <*> randO <*> randO <*> randO <*> no
    , pure AST_ReturnExpr   <*> randO <*> randO <*> no
    , pure AST_WithDoc      <*> randO <*> randO <*> no
    ]

instance PPrintable ScriptExpr where { pPrint = pPrintInterm }

instance Intermediate ScriptExpr AST_Script where
  toInterm   ast = case ast of
    AST_Comment      _         -> mzero
    AST_EvalObject   a _   loc -> liftM2 EvalObject   (ti  a)                 [loc]
    AST_IfThenElse   a         -> liftM  IfThenElse   (ti  a)
    AST_WhileLoop    a         -> liftM  WhileLoop    (ti  a)
    AST_RuleFunc     a         -> liftM  RuleFuncExpr (ti  a)
    AST_TryCatch     a b c loc -> liftM4 TryCatch     (uc0 a) (um0 b) (um1 c) [loc]
    AST_ForLoop      a b c loc -> liftM4 ForLoop      (uc  a) (uc0 b) (ti  c) [loc]
    AST_ContinueExpr a _ c loc -> liftM3 ContinueExpr [a]             (uc0 c) [loc]
    AST_ReturnExpr   a b   loc -> liftM3 ReturnExpr   [a]     (uc0 b)         [loc]
    AST_WithDoc      a b   loc -> liftM3 WithDoc      (uc0 a) (ti  b)         [loc]
  fromInterm obj = case obj of
    EvalObject   a     loc -> liftM3 AST_EvalObject   (fi  a) [[]]            [loc]
    IfThenElse   a         -> liftM  AST_IfThenElse   (fi  a)
    WhileLoop    a         -> liftM  AST_WhileLoop    (fi  a)
    RuleFuncExpr a         -> liftM  AST_RuleFunc     (fi  a)
    TryCatch     a b c loc -> liftM4 AST_TryCatch     (nc0 a) (nm0 b) (nm1 c) [loc]
    ForLoop      a b c loc -> liftM4 AST_ForLoop      (nc  a) (nc0 b) (fi  c) [loc]
    ContinueExpr a b   loc -> liftM4 AST_ContinueExpr [a]     [[]]    (nc0 b) [loc]
    ReturnExpr   a b   loc -> liftM3 AST_ReturnExpr   [a]     (nc0 b)         [loc]
    WithDoc      a b   loc -> liftM3 AST_WithDoc      (nc0 a) (fi  b)         [loc]

instance ObjectClass [AST_Script] where { obj=listToObj; fromObj=listFromObj; }

instance ObjectClass AST_Script where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Script where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Contains a list of 'ObjectExpr's, which are used to encode parameters to function calls, and
-- intialization lists.
data ObjListExpr = ObjListExpr [AssignExpr] Location deriving (Eq, Ord, Typeable)

instance Show ObjListExpr where { show (ObjListExpr o loc) = show o++show loc }

instance Monoid ObjListExpr where
  mempty = ObjListExpr [] LocationUnknown
  mappend (ObjListExpr a locA) (ObjListExpr b locB) = ObjListExpr (a++b) (locA<>locB)

instance NFData ObjListExpr where { rnf (ObjListExpr a b) = deepseq a $! deepseq b () }

instance HasNullValue ObjListExpr where
  nullValue = mempty
  testNull (ObjListExpr a _) = null a

instance HasLocation ObjListExpr where
  getLocation (ObjListExpr _ loc)     = loc
  setLocation (ObjListExpr a _  ) loc = ObjListExpr (fmap delLocation a) loc
  delLocation (ObjListExpr a _  )     = ObjListExpr (fmap delLocation a) LocationUnknown

instance B.Binary ObjListExpr MTab where
  put (ObjListExpr lst loc) = B.prefixByte 0x3B $ B.putUnwrapped lst >> B.put loc
  get = (B.tryWord8 0x3B $ pure ObjListExpr <*> B.getUnwrapped <*> B.get) <|> fail "expecting ObjListExpr"

instance Executable ObjListExpr [Object] where
  execute (ObjListExpr exprs _) = do
    fmap concat $ forM (zip [1..] exprs) $ \ (i, expr) -> execute expr >>= \o -> case o of
      Nothing -> execThrow $ obj $
        [obj "expression used in list evaluated to void", obj "function parameter index", obj (i::Int)]
      Just  o -> return [o]

instance PPrintable ObjListExpr where { pPrint = pPrintInterm }

instance ObjectClass ObjListExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ObjListExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_ObjList = AST_ObjList [Comment] [Com AST_Assign] Location deriving (Eq, Ord, Typeable, Show)

mkObjList :: [Com AST_Assign] -> AST_ObjList
mkObjList ox = AST_ObjList [] ox (mconcat $ fmap (getLocation . unComment) ox)

setObjListPreComments :: [Comment] -> AST_ObjList -> AST_ObjList
setObjListPreComments coms (AST_ObjList _ a loc) = AST_ObjList coms a loc

instance Monoid AST_ObjList where
  mempty = AST_ObjList [] [] LocationUnknown
  mappend (AST_ObjList a1 a2 aloc) (AST_ObjList b1 b2 bloc) = AST_ObjList (a1++b1) (a2++b2) (aloc<>bloc)

instance HasNullValue AST_ObjList where
  nullValue = mempty
  testNull (AST_ObjList [] [] _) = True
  testNull _ = False

instance HasLocation AST_ObjList where
  getLocation (AST_ObjList _ _ loc)     = loc
  setLocation (AST_ObjList a b _  ) loc = AST_ObjList a      b  loc
  delLocation (AST_ObjList a b _  )     = AST_ObjList a (fd1 b) lu

instance PPrintable AST_ObjList where
  pPrint (AST_ObjList coms lst _) = pPrint coms >>
    pInline (intersperse (pString ", ") (map pPrint lst))

instance NFData AST_ObjList where { rnf (AST_ObjList a b c) = deepseq a $! deepseq b $! deepseq c () }

instance ToDaoStructClass AST_ObjList Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_ObjList coms lst loc -> renameConstructor "ObjectList" >>
      putComments coms >> defObjField "items" (listToObj lst) >> putLocation loc

instance FromDaoStructClass AST_ObjList Object where
  fromDaoStruct = constructor "ObjectList" >>
    pure AST_ObjList <*> comments <*> reqList "items" <*> location

instance HasRandGen AST_ObjList where
  randO = recurse nullValue $ AST_ObjList <$> randO <*> randListOf 0 5 (randComWith randO) <*> no

instance Intermediate ObjListExpr AST_ObjList where
  toInterm   (AST_ObjList _ lst loc) = liftM2 ObjListExpr      [lst>>=uc0] [loc]
  fromInterm (ObjListExpr   lst loc) = liftM3 AST_ObjList [[]] [lst>>=nc0] [loc]

instance ObjectClass AST_ObjList where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_ObjList where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

newtype OptObjListExpr = OptObjListExpr (Maybe ObjListExpr) deriving (Eq, Ord, Typeable, Show)

instance NFData OptObjListExpr where { rnf (OptObjListExpr a) = deepseq a () }

instance HasLocation OptObjListExpr where
  getLocation (OptObjListExpr o)     = maybe LocationUnknown getLocation o
  setLocation (OptObjListExpr o) loc = OptObjListExpr (setLocation o loc)
  delLocation (OptObjListExpr o)     = OptObjListExpr (delLocation o    )

instance HasNullValue OptObjListExpr where
  nullValue = OptObjListExpr Nothing
  testNull (OptObjListExpr Nothing) = True
  testNull _ = False

instance B.Binary OptObjListExpr MTab where
  put (OptObjListExpr o) = B.put o
  get = OptObjListExpr <$> B.get

instance Executable OptObjListExpr [Object] where
  execute (OptObjListExpr lst) = maybe (return []) execute lst

instance ObjectClass OptObjListExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass OptObjListExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

-- | Evaluate an 'Exec', but if it throws an exception, set record an 'ObjectExpr' where
-- the exception occurred in the exception information.
updateExecError :: (ExecControl -> ExecControl) -> Exec a -> Exec a
updateExecError upd fn = catchError fn (\err -> throwError (upd err))

----------------------------------------------------------------------------------------------------

data AST_OptObjList = AST_OptObjList [Comment] (Maybe AST_ObjList) deriving (Eq, Ord, Typeable, Show)

instance NFData AST_OptObjList where
  rnf (AST_OptObjList a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_OptObjList where
  nullValue = AST_OptObjList [] Nothing
  testNull (AST_OptObjList _ a) = maybe True testNull a

pPrintObjList :: String -> String -> String -> AST_ObjList -> PPrint
pPrintObjList open comma close (AST_ObjList coms lst _) = pList (pPrint coms) open comma close (map pPrint lst)

pPrintOptObjList :: String -> String -> String -> AST_OptObjList -> PPrint
pPrintOptObjList open comma close (AST_OptObjList coms o) =
  maybe (return ()) (\o -> pPrint coms >> pPrintObjList open comma close o) o

instance HasLocation AST_OptObjList where
  getLocation (AST_OptObjList _ o)     = maybe LocationUnknown getLocation o
  setLocation (AST_OptObjList c o) loc = AST_OptObjList c (fmap (flip setLocation loc) o)
  delLocation (AST_OptObjList c o)     = AST_OptObjList c (fmap delLocation o)

instance PPrintable AST_OptObjList where { pPrint o = pPrintOptObjList "{" ", " "}" o }

instance ToDaoStructClass AST_OptObjList Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_OptObjList coms o -> renameConstructor "OptObjList" >> "params" .=? o >> putComments coms

instance FromDaoStructClass AST_OptObjList Object where
  fromDaoStruct = constructor "OptObjList" >> pure AST_OptObjList <*> comments <*> opt "params"

instance HasRandGen AST_OptObjList where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ pure AST_OptObjList <*> randO <*> pure Nothing
    , pure AST_OptObjList <*> randO <*> (Just <$> randO)
    ]

instance Intermediate OptObjListExpr AST_OptObjList where
  toInterm   (AST_OptObjList _ o) = liftM OptObjListExpr (um1 o)
  fromInterm (OptObjListExpr   o) = liftM (AST_OptObjList []) (nm1 o)

instance ObjectClass AST_OptObjList where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_OptObjList where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data LiteralExpr = LiteralExpr Object Location deriving (Eq, Ord, Typeable, Show)

instance NFData LiteralExpr where
  rnf (LiteralExpr a b) = deepseq a $! deepseq b ()

instance HasNullValue LiteralExpr where
  nullValue = LiteralExpr ONull LocationUnknown
  testNull (LiteralExpr ONull _) = True
  testNull _ = False

instance HasLocation LiteralExpr where
  getLocation (LiteralExpr _ loc)     = loc
  setLocation (LiteralExpr o _  ) loc = LiteralExpr o loc
  delLocation (LiteralExpr o _  )     = LiteralExpr o LocationUnknown

instance B.Binary LiteralExpr MTab where
  put (LiteralExpr a loc) = B.put a >> B.put loc
  get = B.word8PrefixTable <|> fail "expecting LiteralExpr"

instance B.HasPrefixTable LiteralExpr B.Byte MTab where
  prefixTable = B.bindPrefixTable B.prefixTable $ \o -> LiteralExpr o <$> B.get

instance Executable LiteralExpr (Maybe Object) where { execute (LiteralExpr o _) = return (Just o) }

instance ObjectClass LiteralExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass LiteralExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> defDeref execute
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AST_Literal = AST_Literal Object Location deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Literal where
  rnf (AST_Literal a b) = deepseq a $! deepseq b ()

instance HasNullValue AST_Literal where
  nullValue = AST_Literal ONull LocationUnknown
  testNull (AST_Literal ONull _) = True
  testNull _                     = False

instance HasLocation AST_Literal where
  getLocation (AST_Literal _ loc)     = loc
  setLocation (AST_Literal a _  ) loc = AST_Literal a loc
  delLocation (AST_Literal a _  )     = AST_Literal a lu

instance PPrintable AST_Literal where
  pPrint (AST_Literal o _  ) = pPrint o

instance PrecedeWithSpace AST_Literal where
  precedeWithSpace (AST_Literal _ _) = True

instance ToDaoStructClass AST_Literal Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_Literal o loc -> renameConstructor "Literal" >> "obj" .= o >> putLocation loc

instance FromDaoStructClass AST_Literal Object where
  fromDaoStruct = constructor "Literal" >> pure AST_Literal <*> req "obj" <*> location

instance HasRandGen AST_Literal where
  randO = countRunRandChoice
  randChoice = randChoiceList $ fmap (\gen -> pure AST_Literal <*> gen <*> no) $
    [ OString <$> randO
    , return ONull, return OTrue
    , randInteger (OInt   0) $ \i ->
        randInt >>= \j -> return (OInt  $ fromIntegral $ i*j)
    , randInteger (OWord  0) $ \i ->
        randInt >>= \j -> return (OWord $ fromIntegral $ abs $ i*j)
    , randInteger (OLong  0) $ \i ->
        replicateM (mod i 4 + 1) randInt >>= return . OLong . longFromInts
    , randInteger (ORatio 0) $ \i -> return (ORatio (toInteger i % 1))
    , randInteger (OFloat 0) (fmap (OFloat . fromRational) . randRational)
    , randInteger (OComplex (complex 0 0)) $
        fmap (OComplex . complex 0 . abs . fromRational) . randRational
    , randInteger (OChar '\n') $ \i ->
        return (OChar $ chr $ mod i $ ord (maxBound::Char))
    ]

instance Intermediate LiteralExpr AST_Literal where
  toInterm   (AST_Literal a loc) = liftM2 LiteralExpr [a] [loc]
  fromInterm (LiteralExpr a loc) = liftM2 AST_Literal [a] [loc]

instance ObjectClass AST_Literal where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Literal where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data RefOpExpr
  = ObjParenExpr  ParenExpr
  | PlainRefExpr  QualRefExpr
  | ArraySubExpr  RefOpExpr ObjListExpr Location
  | FuncCall      RefOpExpr ObjListExpr Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData RefOpExpr where
  rnf (ObjParenExpr a    ) = deepseq a ()
  rnf (PlainRefExpr a    ) = deepseq a ()
  rnf (ArraySubExpr a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (FuncCall     a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue RefOpExpr where
  nullValue = ObjParenExpr nullValue
  testNull (ObjParenExpr a) = testNull a
  testNull _                = False

instance HasLocation RefOpExpr where
  getLocation o     = case o of
    ObjParenExpr     o -> getLocation o
    PlainRefExpr     o -> getLocation o
    ArraySubExpr _ _ o -> o
    FuncCall     _ _ o -> o
  setLocation o loc = case o of
    ObjParenExpr   a     -> ObjParenExpr (setLocation a loc)
    PlainRefExpr   a     -> PlainRefExpr (setLocation a loc)
    ArraySubExpr   a b _ -> ArraySubExpr a b loc
    FuncCall       a b _ -> FuncCall     a b loc
  delLocation o     = case o of
    ObjParenExpr  a     -> ObjParenExpr (delLocation a)
    PlainRefExpr  a     -> PlainRefExpr (fd a)
    ArraySubExpr  a b _ -> ArraySubExpr a b lu
    FuncCall      a b _ -> FuncCall     a b lu

instance B.Binary RefOpExpr MTab where
  put o = case o of
    ObjParenExpr a     -> B.put a
    PlainRefExpr a     -> B.put a
    ArraySubExpr a b z -> B.prefixByte 0x29 $ B.put a >> B.put b >> B.put z
    FuncCall     a b z -> B.prefixByte 0x2A $ B.put a >> B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting SingleExpr"

instance B.HasPrefixTable RefOpExpr Word8 MTab where
  prefixTable = fmap ObjParenExpr B.prefixTable <> fmap PlainRefExpr B.prefixTable <>
    B.mkPrefixTableWord8 "RefOpExpr" 0x29 0x2A
      [ pure ArraySubExpr <*> B.get <*> B.get <*> B.get
      , pure FuncCall     <*> B.get <*> B.get <*> B.get
      ]

instance Executable RefOpExpr (Maybe Object) where
  execute o = case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ObjParenExpr  o -> execute o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    PlainRefExpr  r -> execute r
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ArraySubExpr o i loc -> do
      o <- execute o >>= checkVoid loc "operand of subscript expression" >>= derefObject
      fmap Just $ execute i >>= mapM derefObject >>= foldM indexObject o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    FuncCall op args loc -> do -- a built-in function call
      o  <- execute op >>= checkVoid loc "function selector evaluates to void"
      op <- mplus (asReference o) $ execThrow $ obj $
        [obj "function selector does not evaluate to reference:", o]
      let nonBuiltin = callFunction op args
      builtins <- asks builtinFunctions
      case op of
        Unqualified (Reference [name]) -> case M.lookup name builtins of
          Just func -> executeDaoFunc name func args
          Nothing   -> nonBuiltin
        _ -> nonBuiltin
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --


instance ObjectClass RefOpExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass RefOpExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> defDeref execute
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AST_RefOperand
  = AST_ObjParen  AST_Paren 
  | AST_PlainRef  AST_QualRef
  | AST_ArraySub  AST_RefOperand AST_ObjList Location
  | AST_FuncCall  AST_RefOperand AST_ObjList Location
  deriving (Eq, Ord, Show, Typeable)

instance NFData AST_RefOperand where
  rnf (AST_ObjParen   a) = deepseq a ()
  rnf (AST_PlainRef   a  ) = deepseq a ()
  rnf (AST_ArraySub a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_FuncCall a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_RefOperand where
  nullValue = AST_ObjParen $ AST_Paren nullValue LocationUnknown
  testNull (AST_ObjParen (AST_Paren a LocationUnknown)) = testNull a
  testNull _ = False

instance HasLocation AST_RefOperand where
  getLocation o     = case o of
    AST_ObjParen     o -> getLocation o
    AST_PlainRef     o -> getLocation o
    AST_ArraySub _ _ o -> o
    AST_FuncCall _ _ o -> o
  setLocation o loc = case o of
    AST_ObjParen   a   -> AST_ObjParen   (setLocation a loc)
    AST_PlainRef   a   -> AST_PlainRef  (setLocation a loc)
    AST_ArraySub a b _ -> AST_ArraySub a b loc
    AST_FuncCall a b _ -> AST_FuncCall a b loc
  delLocation o     = case o of
    AST_ObjParen   a   -> AST_ObjParen   (fd  a)
    AST_PlainRef   a   -> AST_PlainRef  (fd a)
    AST_ArraySub a b _ -> AST_ArraySub (fd a) (fd b) lu
    AST_FuncCall a b _ -> AST_FuncCall (fd a) (fd b) lu

instance PPrintable AST_RefOperand where
  pPrint o = case o of
    AST_ObjParen  paren           -> pPrint paren
    AST_PlainRef  ref             -> pPrint ref
    AST_ArraySub  objXp xcObjXp _ -> pInline [pPrint objXp, pPrintObjList "[" ", " "]" xcObjXp]
    AST_FuncCall  objXp xcObjXp _ -> pInline [pPrint objXp, pPrintObjList "(" ", " ")" xcObjXp]

instance PrecedeWithSpace AST_RefOperand where
  precedeWithSpace o = case o of
    AST_ObjParen  _     -> False
    AST_PlainRef  o     -> precedeWithSpace o
    AST_ArraySub  o _ _ -> precedeWithSpace o
    AST_FuncCall  o _ _ -> precedeWithSpace o

instance ToDaoStructClass AST_RefOperand Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_ObjParen a       -> innerToStruct a
    AST_PlainRef a       -> innerToStruct a
    AST_ArraySub a b loc -> renameConstructor "Subscript" >>
      "head" .= a >> "params" .= b >> putLocation loc
    AST_FuncCall a b loc -> renameConstructor "FuncCall" >>
      "head" .= a >> "params" .= b >> putLocation loc

instance FromDaoStructClass AST_RefOperand Object where
  fromDaoStruct = msum $
    [ AST_ObjParen <$> fromDaoStruct
    , AST_PlainRef <$> fromDaoStruct
    , constructor "Subscript" >> pure AST_ArraySub <*> req "head" <*> req "params" <*> location
    , constructor "FuncCall"  >> pure AST_FuncCall <*> req "head" <*> req "params" <*> location
    ]

instance HasRandGen AST_RefOperand where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ AST_ObjParen <$> randO
    , AST_PlainRef <$> randO
    , pure AST_ArraySub <*> randO <*> randO <*> no
    , pure AST_FuncCall <*> randO <*> randO <*> no
    ]

instance Intermediate RefOpExpr AST_RefOperand where
  toInterm ast = case ast of
    AST_ObjParen  a       -> liftM  ObjParenExpr   (ti  a)
    AST_PlainRef  a       -> liftM  PlainRefExpr  (ti a)
    AST_ArraySub  a b loc -> liftM3 ArraySubExpr  (ti a) (ti b) [loc]
    AST_FuncCall  a b loc -> liftM3 FuncCall      (ti a) (ti b) [loc]
  fromInterm o = case o of
    ObjParenExpr  a       -> liftM  AST_ObjParen  (fi  a)
    PlainRefExpr  a       -> liftM  AST_PlainRef  (fi a)
    ArraySubExpr  a b loc -> liftM3 AST_ArraySub  (fi a) (fi b) [loc]
    FuncCall      a b loc -> liftM3 AST_FuncCall  (fi a) (fi b) [loc]

instance ObjectClass AST_RefOperand where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_RefOperand where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data SingleExpr
  = SingleExpr RefOpExpr
  | RefPfxExpr RefPfxOp RefOpExpr Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData SingleExpr where
  rnf (SingleExpr a    ) = deepseq a ()
  rnf (RefPfxExpr a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue SingleExpr where
  nullValue = SingleExpr $ ObjParenExpr nullValue
  testNull (SingleExpr (ObjParenExpr a)) = testNull a
  testNull _ = False

instance HasLocation SingleExpr where
  getLocation o     = case o of
    SingleExpr     o -> getLocation o
    RefPfxExpr _ _ o -> o
  setLocation o loc = case o of
    SingleExpr a      -> SingleExpr (setLocation a loc)
    RefPfxExpr a b  _ -> RefPfxExpr a b loc
  delLocation o     = case o of
    SingleExpr a      -> SingleExpr (fd a)
    RefPfxExpr a b  _ -> RefPfxExpr a (fd b) lu

instance B.Binary SingleExpr MTab where
  put o = case o of
    SingleExpr     a       -> B.put a
    RefPfxExpr     a b   z -> B.prefixByte 0x28 $ B.put a >> B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting SingleExpr"

instance B.HasPrefixTable SingleExpr Word8 MTab where
  prefixTable = fmap SingleExpr B.prefixTable <>
    B.mkPrefixTableWord8 "SingleExpr" 0x28 0x28 [pure RefPfxExpr <*> B.get <*> B.get <*> B.get]

instance Executable SingleExpr (Maybe Object) where
  execute o = case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    SingleExpr o -> execute o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    RefPfxExpr op o loc -> case op of
      REF   -> do
        let evalRef o = execute o >>= \r -> case r of
              Nothing       -> return $ Just $ ORef $ Unqualified $ Reference []
              Just (ORef _) -> return r
              Just  r       -> execThrow $ obj [obj "cannot use result as reference:", r]
        case o of
          PlainRefExpr o -> return $ Just $ ORef $ qualRefFromExpr o
          ObjParenExpr o -> evalRef o
          ArraySubExpr{} -> evalRef o
          FuncCall{}     -> evalRef o
      DEREF -> fmap Just $ execute o >>= checkVoid loc "dereferenced void value"
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass SingleExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass SingleExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefBinaryFmt

----------------------------------------------------------------------------------------------------

data AST_Single
  = AST_Single !AST_RefOperand
  | AST_RefPfx RefPfxOp [Comment] AST_RefOperand Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Single where
  rnf (AST_Single a      ) = deepseq a ()
  rnf (AST_RefPfx a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d  ()

instance HasNullValue AST_Single where
  nullValue = AST_Single nullValue
  testNull (AST_Single a) = testNull a
  testNull _ = False

instance HasLocation AST_Single where
  getLocation o     = case o of
    AST_Single       o -> getLocation o
    AST_RefPfx _ _ _ o -> o
  setLocation o loc = case o of
    AST_Single a       -> AST_Single (setLocation a loc)
    AST_RefPfx a b c _ -> AST_RefPfx   a b c loc
  delLocation o     = case o of
    AST_Single a       -> AST_Single (fd a)
    AST_RefPfx a b c _ -> AST_RefPfx a b (fd c) lu

instance PPrintable AST_Single where
  pPrint o = case o of
    AST_Single refOp              -> pPrint refOp
    AST_RefPfx ariOp coms objXp _ -> pWrapIndent [pPrint ariOp, pPrint coms, pPrint objXp]

instance PrecedeWithSpace AST_Single where
  precedeWithSpace o = case o of
    AST_Single o       -> precedeWithSpace o
    AST_RefPfx _ _ _ _ -> True

instance ToDaoStructClass AST_Single Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_Single a         -> innerToStruct a
    AST_RefPfx a b c loc -> renameConstructor "RefPrefix" >>
      "op" .= a >> putComments b >> "expr" .= c >> putLocation loc

instance FromDaoStructClass AST_Single Object where
  fromDaoStruct = msum $
    [ AST_Single <$> fromDaoStruct
    , constructor "RefPrefix" >>
        pure AST_RefPfx <*> req "op" <*> req "expr" <*> req "expr" <*> location
    ]

instance HasRandGen AST_Single where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ AST_Single <$> randO
    , pure (AST_RefPfx REF)   <*> randO <*> randO <*> no
    , pure (AST_RefPfx DEREF) <*> randO <*> randO <*> no
    ]

instance Intermediate SingleExpr AST_Single where
  toInterm ast = case ast of
    AST_Single a         -> liftM  SingleExpr (ti a)
    AST_RefPfx a _ c loc -> liftM3 RefPfxExpr [a] (ti c) [loc]
  fromInterm o = case o of
    SingleExpr a       -> liftM  AST_Single (fi a)
    RefPfxExpr a c loc -> liftM4 AST_RefPfx [a] [[]] (fi c) [loc]

instance ObjectClass AST_Single where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Single where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data RuleFuncExpr
  = LambdaExpr    ParamListExpr CodeBlock Location
  | FuncExpr Name ParamListExpr CodeBlock Location
  | RuleExpr RuleStrings        CodeBlock Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData RuleFuncExpr where
  rnf (LambdaExpr a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (FuncExpr   a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (RuleExpr   a b c  ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue RuleFuncExpr where
  nullValue = LambdaExpr nullValue nullValue LocationUnknown
  testNull (RuleExpr a b _) = testNull a && testNull b
  testNull _                = False

instance HasLocation RuleFuncExpr where
  getLocation o = case o of
    LambdaExpr _ _   o -> o
    FuncExpr   _ _ _ o -> o
    RuleExpr   _ _   o -> o
  setLocation o loc = case o of
    LambdaExpr a b   _ -> LambdaExpr      a b   loc
    FuncExpr   a b c _ -> FuncExpr       a b c loc
    RuleExpr   a b   _ -> RuleExpr       a b   loc
  delLocation o = case o of
    LambdaExpr a b   _ -> LambdaExpr      (fd a) (fd b)        lu
    FuncExpr   a b c _ -> FuncExpr           a  (fd b) (fd c) lu
    RuleExpr   a b   _ -> RuleExpr           a  (fd b)        lu

instance PPrintable RuleFuncExpr where { pPrint = pPrintInterm }

instance B.Binary RuleFuncExpr MTab where
  put o = case o of
    LambdaExpr a b   z -> B.prefixByte 0x2B $ B.put a >> B.put b >> B.put z
    FuncExpr   a b c z -> B.prefixByte 0x2C $ B.put a >> B.put b >> B.put c >> B.put z
    RuleExpr   a b   z -> B.prefixByte 0x2D $ B.put a >> B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting RuleFuncExpr"

instance B.HasPrefixTable RuleFuncExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "RuleFuncExpr" 0x2B 0x2D $
    [ pure LambdaExpr <*> B.get <*> B.get <*> B.get
    , pure FuncExpr   <*> B.get <*> B.get <*> B.get <*> B.get
    , pure RuleExpr   <*> B.get <*> B.get <*> B.get
    ]

instance Executable RuleFuncExpr (Maybe Object) where
  execute o = case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    LambdaExpr params scrpt _ -> do
      exec <- setupCodeBlock scrpt
      let callableCode = CallableCode{argsPattern=params, codeSubroutine=exec, returnType=nullValue}
      return $ Just $ new callableCode
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    FuncExpr name params scrpt _ -> do
      exec <- setupCodeBlock scrpt
      let callableCode = CallableCode{argsPattern=params, codeSubroutine=exec, returnType=nullValue}
      let o = Just $ new callableCode
      store <- asks execStack
      storeUpdate store name (return . const o)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    RuleExpr (RuleStrings params _) scrpt _ -> do
      exec  <- setupCodeBlock scrpt
      globs <- forM params $ \param -> do
        let pars = do -- the string parameter is still a quoted
              (str, rem) <- readsPrec 0 (uchars param) -- first parse a 'Prelude.String'
              guard (null rem) >> readsPrec 0 (str::String) -- then parse a 'Dao.Glob.Glob'
        case pars of
          [(pat, "")] -> do
            -- TODO: tokenize the pattern Single's with the 'programTokenizer'
            return $ parseOverSingles pat (fmap (OString . ustr) . simpleTokenizer)
          _           -> execThrow $ obj [obj "cannot parse pattern expression:", obj param]
      return $ Just $ new $ GlobAction globs exec
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance HaskellDataClass RuleFuncExpr where
  haskellDataInterface = interface nullValue $
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AST_RuleFunc
  = AST_Lambda              (Com AST_ParamList)  AST_CodeBlock Location
  | AST_Func [Comment] Name (Com AST_ParamList)  AST_CodeBlock Location
  | AST_Rule                (Com AST_StringList) AST_CodeBlock Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_RuleFunc where
  rnf (AST_Lambda a b c    ) = deepseq a $! deepseq b $! deepseq c () 
  rnf (AST_Func   a b c d e) = deepseq a $! deepseq b $! deepseq c $! deepseq d $! deepseq e ()
  rnf (AST_Rule   a b c    ) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_RuleFunc where
  nullValue = AST_Lambda nullValue nullValue LocationUnknown
  testNull (AST_Lambda a b _) = testNull a && testNull b
  testNull _                = False

instance HasLocation AST_RuleFunc where
  getLocation o = case o of
    AST_Lambda _ _     o -> o
    AST_Func   _ _ _ _ o -> o
    AST_Rule   _ _     o -> o
  setLocation o loc = case o of
    AST_Lambda a b     _ -> AST_Lambda a b     loc
    AST_Func   a b c d _ -> AST_Func   a b c d loc
    AST_Rule   a b     _ -> AST_Rule   a b     loc
  delLocation o = case o of                            
    AST_Lambda a b     _ -> AST_Lambda (fd  a) (fd  b)                 lu
    AST_Func   a b c d _ -> AST_Func        a       b  (fd  c) (fd  d) lu
    AST_Rule   a b     _ -> AST_Rule   (fd  a) (fd  b)                 lu

instance PPrintable AST_RuleFunc where
  pPrint expr = case expr of
    AST_Lambda         ccNmx   xcObjXp     _ ->
      pPrintSubBlock (pInline [pString "function", pPrintComWith pPrint ccNmx]) xcObjXp
    AST_Func     co nm ccNmx   xcObjXp     _ ->
      pClosure (pInline [pString "function ", pPrint co, pPrint nm, pPrint ccNmx]) "{" "}" [pPrint xcObjXp]
    AST_Rule           ccNmx   xcObjXp     _ -> pClosure (pPrint ccNmx) "{" "}" [pPrint xcObjXp]

instance ToDaoStructClass AST_RuleFunc Object where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Lambda a b     loc -> nm "Lambda"   >> "params" .= a >> "block" .= b >> putLocation loc
    AST_Func   a b c d loc -> nm "Function" >>
      putComments a >> "name"  .= b >> "params" .= c >> "block" .= d >> putLocation loc
    AST_Rule   a b     loc -> nm "Rule" >> "params" .= a >> "block" .= b >> putLocation loc

instance FromDaoStructClass AST_RuleFunc Object where
  fromDaoStruct = msum $
    [ constructor "Lambda" >> pure AST_Lambda <*> req "params" <*> req "block"  <*> location
    , constructor "Function" >>
        pure AST_Func <*> comments <*> req "name" <*> req "params" <*> req "block" <*> location
    , constructor "Rule" >> pure AST_Rule <*> req "params" <*> req "block" <*> location
    ]

instance HasRandGen AST_RuleFunc where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ pure AST_Lambda <*> randO <*> randO <*> no
    , pure AST_Func   <*> randO <*> randO <*> randO <*> randO <*> no
    , pure AST_Rule   <*> randO <*> randO <*> no
    ]

instance Intermediate RuleFuncExpr AST_RuleFunc where
  toInterm ast = case ast of
    AST_Lambda a b   loc -> liftM3 LambdaExpr      (uc0 a)         (ti  b) [loc]
    AST_Func _ a b c loc -> liftM4 FuncExpr [a] (uc0 b) (ti  c) [loc]
    AST_Rule   a b   loc -> liftM3 RuleExpr     (uc0 a) (ti  b) [loc]
  fromInterm o = case o of
    LambdaExpr a b   loc -> liftM3 AST_Lambda              (nc0 a) (fi  b) [loc]
    FuncExpr   a b c loc -> liftM5 AST_Func [[]] [a] (nc0 b) (fi  c) [loc]
    RuleExpr   a b   loc -> liftM3 AST_Rule          (nc0 a) (fi  b) [loc]

instance ObjectClass AST_RuleFunc where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_RuleFunc where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data ObjectExpr
  = VoidExpr
  | ObjLiteralExpr  LiteralExpr
  | ObjSingleExpr   SingleExpr
  | ObjRuleFuncExpr RuleFuncExpr
  | ArithPfxExpr                 ArithPfxOp      ObjectExpr   Location
  | InitExpr        RefExpr      OptObjListExpr  ObjListExpr  Location
  | StructExpr      Name         OptObjListExpr               Location
  | MetaEvalExpr                                 CodeBlock    Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData ObjectExpr where
  rnf  VoidExpr                 = ()
  rnf (ObjLiteralExpr  a      ) = deepseq a ()
  rnf (ObjSingleExpr   a      ) = deepseq a $! ()
  rnf (ObjRuleFuncExpr a      ) = deepseq a $! ()
  rnf (ArithPfxExpr    a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (InitExpr        a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (StructExpr      a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (MetaEvalExpr    a b    ) = deepseq a $! deepseq b ()

instance HasNullValue ObjectExpr where
  nullValue = VoidExpr
  testNull VoidExpr = True
  testNull _        = False

instance HasLocation ObjectExpr where
  getLocation o = case o of
    VoidExpr                -> LocationUnknown
    ObjLiteralExpr        o -> getLocation o
    ObjSingleExpr         o -> getLocation o
    ObjRuleFuncExpr       o -> getLocation o
    ArithPfxExpr    _ _   o -> o
    InitExpr        _ _ _ o -> o
    StructExpr      _ _   o -> o
    MetaEvalExpr    _     o -> o
  setLocation o loc = case o of
    VoidExpr                -> VoidExpr
    ObjLiteralExpr  a       -> ObjLiteralExpr  (setLocation a loc)
    ObjSingleExpr   a       -> ObjSingleExpr   (setLocation a loc)
    ObjRuleFuncExpr a       -> ObjRuleFuncExpr (setLocation a loc)
    ArithPfxExpr    a b   _ -> ArithPfxExpr    a b   loc
    InitExpr        a b c _ -> InitExpr        a b c loc
    StructExpr      a b   _ -> StructExpr      a b   loc
    MetaEvalExpr    a     _ -> MetaEvalExpr    a     loc
  delLocation o = case o of
    VoidExpr                -> VoidExpr
    ObjLiteralExpr  a       -> ObjLiteralExpr  (fd a)
    ObjSingleExpr   a       -> ObjSingleExpr   (fd a)
    ObjRuleFuncExpr a       -> ObjRuleFuncExpr (fd a)
    ArithPfxExpr    a b   _ -> ArithPfxExpr        a  (fd b)        lu
    InitExpr        a b c _ -> InitExpr        (fd a) (fd b) (fd c) lu
    StructExpr      a b   _ -> StructExpr      (fd a) (fd b)        lu
    MetaEvalExpr    a     _ -> MetaEvalExpr    (fd a)               lu
    where
      lu = LocationUnknown
      fd :: HasLocation a => a -> a
      fd = delLocation

instance PPrintable ObjectExpr where { pPrint = pPrintInterm }

instance B.Binary ObjectExpr MTab where
  put o = case o of
    ObjSingleExpr   a       -> B.put a
    ObjLiteralExpr  a       -> B.put a
    ObjRuleFuncExpr a       -> B.put a
    VoidExpr                -> B.putWord8   0x2E
    ArithPfxExpr    a b   z -> B.prefixByte 0x2F $ B.put a >> B.put b >> B.put z
    InitExpr        a b c z -> B.prefixByte 0x30 $ B.put a >> B.put b >> B.put c >> B.put z
    StructExpr      a b   z -> B.prefixByte 0x31 $ B.put a >> B.put b >> B.put z
    MetaEvalExpr    a     z -> B.prefixByte 0x32 $ B.put a >> B.put z
  get = B.word8PrefixTable <|> fail "expecting ObjectExpr"

instance B.HasPrefixTable ObjectExpr B.Byte MTab where
  prefixTable = mconcat $
    [ ObjSingleExpr   <$> B.prefixTable
    , ObjLiteralExpr  <$> B.prefixTable
    , ObjRuleFuncExpr <$> B.prefixTable
    , B.mkPrefixTableWord8 "ObjectExpr" 0x2E 0x32 $
        [ return VoidExpr
        , pure ArithPfxExpr <*> B.get <*> B.get <*> B.get
        , pure InitExpr     <*> B.get <*> B.get <*> B.get <*> B.get
        , pure StructExpr   <*> B.get <*> B.get <*> B.get
        , pure MetaEvalExpr <*> B.get <*> B.get
        ]
    ]

indexObject :: Object -> Object -> Exec Object
indexObject o idx = case o of
  OList []  -> execThrow $ obj [obj "indexing empty list:", o, idx]
  OList lst -> do
    i <- mplus (asInteger idx) $
      execThrow (obj [obj "must index list with integer:", idx, obj "indexing:", o])
    if i<0
      then  execThrow $ obj [obj "list index value is negative:", idx, obj "indexing:", o]
      else  case dropWhile ((<i) . fst) (zip [0..] lst) of
              []       -> execThrow $ obj [obj "index out of bounds:", idx, obj "indexing:", o]
              (_, o):_ -> return o
  ODict dict -> case idx of
    OString                      i   -> case maybeFromUStr i of
      Nothing -> execThrow $ obj [obj "string does not form valid identifier:", OString i]
      Just  i -> doIdx o idx i dict
    ORef (Unqualified (Reference [r])) -> doIdx o idx r dict
    _  -> execThrow $ obj [obj "cannot index dict with value:", idx, obj "indexing:", o]
    where
      doIdx o idx i t = case M.lookup i t of
        Nothing -> execThrow $ obj [obj "dict has no branch index: ", idx, obj "indexing", o]
        Just  o -> return o
  o         -> join $ fmap ($ idx) $ evalObjectMethod errmsg o objIndexer where
    errmsg = obj [obj "cannot index object:", o]

instance Executable ObjectExpr (Maybe Object) where
  execute o = _setObjectExprError o $ case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    VoidExpr -> return Nothing
      -- 'VoidExpr's only occur in return statements. Returning 'ONull' where nothing exists is
      -- probably the most intuitive thing to do on an empty return statement.
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ObjLiteralExpr  o -> execute o
    ObjSingleExpr   o -> execute o
    ObjRuleFuncExpr o -> execute o
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    ArithPfxExpr op expr loc -> do
      expr <- execute expr >>= checkVoid loc ("operand to prefix operator "++show op )
      fmap Just ((arithPrefixOps!op) expr)
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    InitExpr ref bnds initMap _ -> do
      let (ObjListExpr items _) = initMap
      let (RefExpr erf _) = ref
      erf <- pure $ obj erf
      ref <- pure $ toUStr ref
      bnds <- execute bnds
      let cantUseBounds msg = execThrow $ obj $
            [obj msg, obj "must be defined without bounds parameters", OList bnds]
      let initBacktracked = fail "backtracked during initialization"
      case uchars ref of
        "list" -> case bnds of
          [] -> execNested M.empty $ fmap (Just . OList) $ execute initMap >>= mapM derefObject
          _  -> cantUseBounds "for list constructor"
        "dict" -> case bnds of
          [] -> do
            execNested M.empty $ do
              mapM_ execute items
              (LocalStore stack) <- asks execStack
              liftIO $ (Just . ODict . head . mapList) <$> readIORef stack
          _ -> cantUseBounds "for dict constructor"
        _ -> execGetObjTable ref >>= \tab -> case tab of
          Nothing  -> execThrow $ obj [obj "unknown object constructor", erf]
          Just tab -> do
            let make = Just . OHaskell . flip HaskellData tab
            execNested M.empty $ msum $
              [ case objDictInit tab of
                  Nothing           -> mzero
                  Just (init, fold) -> init bnds >>= \o -> flip mplus initBacktracked $ do
                    items <- forM items $ \item -> case item of
                      AssignExpr a op b _ -> do
                        let check msg expr = do
                              o <- execute expr
                              case o of
                                Just  o -> return o
                                Nothing -> execThrow $ obj $
                                  [ obj $ msg++
                                      "-hand side of initalizer expression evaluates to void"
                                  , new expr
                                  ]
                        pure (,,) <*> check "left" a <*> pure op <*> check "right" b
                      EvalExpr arith -> execThrow $ obj $
                        [obj "expression does not assign a value to a key", new arith]
                    make <$> fold o items
              , case objListInit tab of
                  Nothing           -> mzero
                  Just (init, fold) -> init bnds >>= \o -> flip mplus initBacktracked $
                    fmap make (execute initMap >>= mapM derefObject >>= fold o)
              , execThrow $ obj [obj "cannot declare constant object of type", erf]
              ]
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    StructExpr name items _ -> execNested M.empty $ do
      execute items
      (LocalStore stack) <- asks execStack
      items <- liftIO $ (head . mapList) <$> readIORef stack
      return $ Just $ OTree $
        if M.null items
        then Nullary{ structName=name }
        else Struct{ fieldMap=items, structName=name }
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    MetaEvalExpr expr _ -> return $ Just $ new expr
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass ObjectExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass ObjectExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | Part of the Dao language abstract syntax tree: any expression that evaluates to an Object.
data AST_Object
  = AST_Void -- ^ Not a language construct, but used where an object expression is optional.
  | AST_ObjLiteral  AST_Literal
  | AST_ObjSingle   AST_Single
  | AST_ObjRuleFunc AST_RuleFunc
  | AST_ArithPfx    ArithPfxOp [Comment]       AST_Object    Location
  | AST_Init        AST_Ref    AST_OptObjList  AST_ObjList   Location
  | AST_Struct      Name       AST_OptObjList                Location
  | AST_MetaEval                               AST_CodeBlock Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Object where
  rnf AST_Void = ()
  rnf (AST_ObjLiteral  a      ) = deepseq a ()
  rnf (AST_ObjSingle   a      ) = deepseq a ()
  rnf (AST_ObjRuleFunc a      ) = deepseq a ()
  rnf (AST_ArithPfx    a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Init        a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_Struct      a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_MetaEval    a b    ) = deepseq a $! deepseq b ()

instance HasNullValue AST_Object where
  nullValue = AST_Void
  testNull AST_Void = True
  testNull _        = False

instance HasLocation AST_Object where
  getLocation o = case o of
    AST_Void               -> LocationUnknown
    AST_ObjLiteral       o -> getLocation o
    AST_ObjSingle        o -> getLocation o
    AST_ObjRuleFunc      o -> getLocation o
    AST_ArithPfx _ _ _   o -> o
    AST_Init     _ _ _   o -> o
    AST_Struct   _ _     o -> o
    AST_MetaEval _       o -> o
  setLocation o loc = case o of
    AST_Void                  -> AST_Void
    AST_ObjLiteral  a         -> AST_ObjLiteral  (setLocation a loc)
    AST_ObjSingle   a         -> AST_ObjSingle   (setLocation a loc)
    AST_ObjRuleFunc a         -> AST_ObjRuleFunc (setLocation a loc)
    AST_ArithPfx    a b c   _ -> AST_ArithPfx    a b c   loc
    AST_Init        a b c   _ -> AST_Init        a b c   loc
    AST_Struct      a b     _ -> AST_Struct      a b     loc
    AST_MetaEval    a       _ -> AST_MetaEval    a       loc
  delLocation o = case o of                            
    AST_Void                  -> AST_Void
    AST_ObjLiteral  a         -> AST_ObjLiteral  (fd  a)
    AST_ObjSingle   a         -> AST_ObjSingle   (fd  a)
    AST_ObjRuleFunc a         -> AST_ObjRuleFunc (fd  a)
    AST_ArithPfx    a b c   _ -> AST_ArithPfx         a       b  (fd  c)         lu
    AST_Init        a b c   _ -> AST_Init                (fd  a) (fd  b) (fd  c) lu
    AST_Struct      a b     _ -> AST_Struct           a  (fd  b)                 lu
    AST_MetaEval    a       _ -> AST_MetaEval    (fd  a)                         lu

instance PPrintable AST_Object where
  pPrint expr = case expr of
    AST_Void                         -> return ()
    AST_ObjLiteral  o                -> pPrint o
    AST_ObjSingle   o                -> pPrint o
    AST_ObjRuleFunc o                -> pPrint o
    AST_ArithPfx    op coms objXp  _ -> pWrapIndent $
      [pPrint op, pPrint coms, pPrint objXp]
    AST_Init          ref objs     elems   _ ->
      pInline [pPrint ref, pPrintOptObjList "(" ", " ")" objs, pPrintObjList "{" ", " "}" elems]
    AST_Struct      nm itms        _ -> case itms of
      AST_OptObjList coms items -> do
        let name = pString $ '#' : uchars (toUStr nm)
        pPrint coms
        case items of
          Nothing -> name
          Just (AST_ObjList coms items _) -> do
            pPrint coms
            pList name "{" ", " "}" $ map pPrint items
    AST_MetaEval cObjXp                    _ -> pInline [pString "${", pPrint cObjXp, pString "}"]

instance PrecedeWithSpace AST_Object where
  precedeWithSpace o = case o of
    AST_Void         -> False
    AST_MetaEval{}   -> False
    AST_ObjSingle  o -> precedeWithSpace o
    _                -> True

instance ToDaoStructClass AST_Object Object where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Void                   -> makeNullary "Void"
    AST_ObjLiteral   a         -> innerToStruct a
    AST_ObjSingle    a         -> innerToStruct a
    AST_ObjRuleFunc  a         -> innerToStruct a
    AST_ArithPfx     a b c loc -> nm "ArithPrefix" >>
      "op" .= a >> putComments b >> "expr" .= c >> putLocation loc
    AST_Init         a b c loc -> nm "Init" >>
      "name" .= a >> "params" .= b >> "initList" .= c >> putLocation loc
    AST_Struct       a b   loc -> nm "Struct" >> "name" .= a >> "initList" .= b >> putLocation loc
    AST_MetaEval     a     loc -> nm "MetaEval" >> "block" .= a >> putLocation loc

instance FromDaoStructClass AST_Object Object where
  fromDaoStruct = msum $
    [ nullary "Void" >> return AST_Void
    , AST_ObjLiteral  <$> fromDaoStruct
    , AST_ObjSingle   <$> fromDaoStruct
    , AST_ObjRuleFunc <$> fromDaoStruct
    , constructor "ArithPrefix" >>
        pure AST_ArithPfx <*> req "op" <*> comments <*> req "expr" <*> location
    , constructor "Init" >>
        pure AST_Init <*> req "name" <*> req "params" <*> req "initList" <*> location
    , constructor "Struct" >> pure AST_Struct <*> req "name" <*> req "initList" <*> location
    , constructor "MetaEval" >> pure AST_MetaEval <*> req "block" <*> location
    ]

instance HasRandGen AST_Object where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ AST_ObjLiteral  <$> randO
    , AST_ObjSingle   <$> randO
    , AST_ObjRuleFunc <$> randO
    , pure AST_ArithPfx <*> randO <*> randO <*> randO <*> no
    , pure AST_Init     <*> randO <*> randO <*> randO <*> no
    , pure AST_MetaEval <*> randO <*> no
    ]

instance HasRandGen [Com AST_Object] where { randO = countNode $ randList 1 20 }

instance Intermediate ObjectExpr AST_Object where
  toInterm ast = case ast of
    AST_Void                  -> return VoidExpr
    AST_ObjLiteral  a         -> liftM  ObjLiteralExpr  (ti  a)
    AST_ObjSingle   a         -> liftM  ObjSingleExpr   (ti  a)
    AST_ObjRuleFunc a         -> liftM  ObjRuleFuncExpr (ti  a)
    AST_ArithPfx    a _ c loc -> liftM3 ArithPfxExpr        [a]         (ti  c) [loc]
    AST_Init        a b c loc -> liftM4 InitExpr        (ti  a) (ti  b) (ti  c) [loc]
    AST_Struct      a b   loc -> liftM3 StructExpr          [a] (ti  b)         [loc]
    AST_MetaEval    a     loc -> liftM2 MetaEvalExpr                    (ti  a) [loc]
  fromInterm o = case o of
    VoidExpr                  -> return AST_Void
    ObjLiteralExpr  a         -> liftM  AST_ObjLiteral  (fi  a)
    ObjSingleExpr   a         -> liftM  AST_ObjSingle   (fi  a)
    ObjRuleFuncExpr a         -> liftM  AST_ObjRuleFunc (fi  a)
    ArithPfxExpr    a b   loc -> liftM4 AST_ArithPfx        [a] [[]]    (fi  b) [loc]
    InitExpr        a b c loc -> liftM4 AST_Init        (fi  a) (fi  b) (fi  c) [loc]
    StructExpr      a b   loc -> liftM3 AST_Struct          [a] (fi  b)         [loc]
    MetaEvalExpr    a     loc -> liftM2 AST_MetaEval    (fi  a)                 [loc]

instance ObjectClass AST_Object where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Object where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data ArithExpr
  = ObjectExpr ObjectExpr
  | ArithExpr  ArithExpr InfixOp ArithExpr Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData ArithExpr where
  rnf (ObjectExpr a      ) = deepseq a ()
  rnf (ArithExpr  a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue ArithExpr where
  nullValue = ObjectExpr nullValue
  testNull (ObjectExpr a) = testNull a
  testNull _ = False

instance HasLocation ArithExpr where
  getLocation o     = case o of
    ObjectExpr      o -> getLocation o
    ArithExpr _ _ _ o -> o
  setLocation o loc = case o of
    ObjectExpr  a     -> ObjectExpr (setLocation a loc)
    ArithExpr a b c _ -> ArithExpr a b c loc
  delLocation o     = case o of
    ObjectExpr  a     -> ObjectExpr (delLocation a)
    ArithExpr a b c _ -> ArithExpr (delLocation a) b (delLocation c) LocationUnknown

instance PPrintable ArithExpr where { pPrint = pPrintInterm }

instance B.Binary ArithExpr MTab where
  put o = case o of
    ObjectExpr  a     -> B.put a
    ArithExpr a b c z -> B.prefixByte 0x33 $ B.put a >> B.put b >> B.put c >> B.put z
  get = B.word8PrefixTable <|> fail "expecting arithmetic expression"

instance B.HasPrefixTable ArithExpr B.Byte MTab where
  prefixTable = mappend (ObjectExpr <$> B.prefixTable) $
    B.mkPrefixTableWord8 "ObjectExpr" 0x33 0x33 $
      [pure ArithExpr <*> B.get <*> B.get <*> B.get <*> B.get]

instance Executable ArithExpr (Maybe Object) where
  execute o = case o of
    ObjectExpr o -> execute o
    ArithExpr left' op right' loc -> do
      let err1 msg = msg++"-hand operand of "++show op++ "operator "
          evalLeft   = execute left'  >>= checkVoid loc (err1 "left" )
          evalRight  = execute right' >>= checkVoid loc (err1 "right")
          derefLeft  = evalLeft  >>= derefObject
          derefRight = evalRight >>= derefObject
          logical isAndOp = fmap Just $ do
            left <- derefLeft >>= objToBool
            if left
              then  if isAndOp then derefRight else return OTrue
              else  if isAndOp then return ONull else derefRight
      case op of
        AND -> logical True
        OR  -> logical False
        op  -> do
          (left, right) <- case op of
            ARROW -> liftM2 (,) derefLeft evalRight
            _     -> liftM2 (,) derefLeft derefRight
          fmap Just ((infixOps!op) left right)

instance HaskellDataClass ArithExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AST_Arith 
  = AST_Object AST_Object
  | AST_Arith  AST_Arith (Com InfixOp) AST_Arith Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Arith where
  rnf (AST_Object   a        ) = deepseq a ()
  rnf (AST_Arith a b c d  ) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue AST_Arith where
  nullValue = AST_Object nullValue
  testNull (AST_Object a) = testNull a
  testNull _ = False

instance HasLocation AST_Arith where
  getLocation o     = case o of
    AST_Object      o -> getLocation o
    AST_Arith _ _ _ o -> o
  setLocation o loc = case o of
    AST_Object   a    -> AST_Object (setLocation a loc)
    AST_Arith a b c _ -> AST_Arith a b c loc
  delLocation o     = case o of
    AST_Object   a    -> AST_Object (delLocation a)
    AST_Arith a b c _ -> AST_Arith (fd a) b (fd c) lu

instance PPrintable AST_Arith where
  pPrint o = case o of
    AST_Object o -> pPrint o
    AST_Arith objXp1 comAriOp objXp2 _ -> pWrapIndent [pPrint objXp1, pPrint comAriOp, pPrint objXp2]

instance PrecedeWithSpace AST_Arith where
  precedeWithSpace o = case o of
    AST_Object o       -> precedeWithSpace o
    AST_Arith  o _ _ _ -> precedeWithSpace o

instance ToDaoStructClass AST_Arith Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_Object a         -> innerToStruct a
    AST_Arith  a b c loc -> renameConstructor "Arithmetic" >>
      "left" .= a >> "op" .= b >> "right" .= c >> putLocation loc

instance FromDaoStructClass AST_Arith Object where
  fromDaoStruct = msum $
    [ AST_Object <$> fromDaoStruct
    , constructor "Arithmetic" >>
        pure AST_Arith <*> req "left" <*> req "op" <*> req "right" <*> location
    ]

instance HasRandGen AST_Arith where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ AST_Object <$> randO
    , do  left  <- AST_Object <$> randO
          ops   <- randListOf 0 4 (pure (,) <*> randInfixOp <*> (AST_Object <$> randO))
          return $ foldPrec left ops
    ] where
      randInfixOp :: RandO (Com InfixOp, Int, Bool)
      randInfixOp = do
        (op, prec, assoc) <- runRandChoice opGroups
        op <- randComWith (return op)
        return (op, prec, assoc)
      left  op = (True , op)
      right op = (False, op)
      opGroups :: RandChoice (InfixOp, Int, Bool)
      opGroups = randChoiceList $ map return $ do
        (precedence, (associativity, operators)) <- zip [1..] $ concat $
          [ map right [[OR], [AND], [EQUL, NEQUL]]
          , map left $
              [ [GTN, LTN, GTEQ, LTEQ], [SHL, SHR]
              , [ORB], [XORB], [ANDB]
              , [ADD, SUB], [MULT, DIV, MOD]
              ]
          , map right [[POW], [ARROW]]
          ]
        operator <- operators
        return (operator, precedence, associativity)
      bind left op right = AST_Arith left op right LocationUnknown
      foldPrec left ops = case ops of
        [] -> left
        ((op, prec, _), right):ops -> case scanRight prec right ops of
          (right, ops) -> foldPrec (bind left op right) ops
      scanRight prevPrec left ops = case ops of
        [] -> (left, [])
        ((op, prec, assoc), right):next -> 
          if prevPrec<prec || (prevPrec==prec && not assoc)
          then  case scanRight prec right next of
                  (right, next) -> scanRight prevPrec (bind left op right) next
          else  (left, ops)

instance Intermediate ArithExpr AST_Arith where
  toInterm o = case o of
    AST_Object  a       -> liftM  ObjectExpr (ti a)
    AST_Arith a b c loc -> liftM4 ArithExpr  (ti a) (uc b) (ti c) [loc]
  fromInterm o = case o of
    ObjectExpr  a       -> liftM  AST_Object (fi a)
    ArithExpr a b c loc -> liftM4 AST_Arith  (fi a) (nc b) (fi c) [loc]

instance ObjectClass AST_Arith where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Arith where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AssignExpr
  = EvalExpr   ArithExpr
  | AssignExpr ArithExpr UpdateOp AssignExpr Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AssignExpr where
  rnf (EvalExpr   a      ) = deepseq a ()
  rnf (AssignExpr a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue AssignExpr where
  nullValue = EvalExpr nullValue
  testNull (EvalExpr a) = testNull a
  testNull _ = False

instance HasLocation AssignExpr where
  getLocation o     = case o of
    EvalExpr         o -> getLocation o
    AssignExpr _ _ _ o -> o
  setLocation o loc = case o of
    EvalExpr   a       -> EvalExpr  (setLocation a loc)
    AssignExpr a b c _ -> AssignExpr a b c loc
  delLocation o     = case o of
    EvalExpr   a       -> EvalExpr   (delLocation a)
    AssignExpr a b c _ -> AssignExpr (delLocation a) b (delLocation c) LocationUnknown

instance PPrintable AssignExpr where { pPrint = pPrintInterm }

instance B.Binary AssignExpr MTab where
  put o = case o of
    EvalExpr   a       -> B.put a
    AssignExpr a b c z -> B.prefixByte 0x34 $ B.put a >> B.put b >> B.put c >> B.put z
  get = B.word8PrefixTable <|> fail "expecting AssignExpr"

instance B.HasPrefixTable AssignExpr B.Byte MTab where
  prefixTable = mappend (EvalExpr <$> B.prefixTable) $
    B.mkPrefixTableWord8 "AssignExpr" 0x34 0x34 $
      [pure AssignExpr <*> B.get <*> B.get <*> B.get <*> B.get]

instance Executable AssignExpr (Maybe Object) where
  execute o = case o of
    EvalExpr   o -> execute o
    AssignExpr nm op expr loc -> do
      let lhs = "left-hand side of "++show op
      nm <- msum $
        [ execute nm >>= checkVoid (getLocation nm) (lhs++" evaluates to void") >>= asReference
        , execThrow $ obj [obj $ lhs++" is not a reference value"]
        ]
      expr <- execute expr >>= checkVoid loc "right-hand side of assignment"
      qualRefUpdate nm $ \maybeObj -> case maybeObj of
        Nothing      -> case op of
          UCONST -> return (Just expr)
          _      -> execThrow $ obj [obj "undefined refence:", obj nm]
        Just prevVal -> fmap Just $
          checkPredicate "assignment expression" [prevVal, expr] $ (updatingOps!op) prevVal expr

instance HaskellDataClass AssignExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    defDeref execute >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

data AST_Assign
  = AST_Eval   AST_Arith
  | AST_Assign AST_Arith (Com UpdateOp) AST_Assign Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Assign where
  rnf (AST_Eval   a      ) = deepseq a ()
  rnf (AST_Assign a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()

instance HasNullValue AST_Assign where
  nullValue = AST_Eval  nullValue
  testNull (AST_Eval  a) = testNull a
  testNull _ = False

instance HasLocation AST_Assign where
  getLocation o = case o of
    AST_Eval      o -> getLocation o
    AST_Assign _ _ _ o -> o
  setLocation o loc = case o of
    AST_Eval      o -> AST_Eval  (setLocation o loc)
    AST_Assign a b c _ -> AST_Assign a b c loc
  delLocation o = case o of                            
    AST_Eval      o -> AST_Eval  (delLocation o)
    AST_Assign a b c _ -> AST_Assign (delLocation a) b (delLocation c) lu

instance PPrintable AST_Assign where
  pPrint expr = case expr of
    AST_Eval  eq -> pPrint eq
    AST_Assign objXp1 comUpdOp objXp2 _ -> pWrapIndent $
      [pPrint objXp1, pPrint comUpdOp, pPrint objXp2]

instance PrecedeWithSpace AST_Assign where
  precedeWithSpace o = case o of
    AST_Eval   o       -> precedeWithSpace o
    AST_Assign o _ _ _ -> precedeWithSpace o

instance ToDaoStructClass AST_Assign Object where
  toDaoStruct = ask >>= \o -> case o of
    AST_Eval o ->  innerToStruct o
    AST_Assign to op from loc -> renameConstructor "Assign" >>
      "to" .= to >> "op" .= op >> "from" .= from >> putLocation loc

instance FromDaoStructClass AST_Assign Object where
  fromDaoStruct = msum $
    [ AST_Eval <$> fromDaoStruct
    , pure AST_Assign <*> req "to" <*> req "op" <*> req "from" <*> location
    ]

instance HasRandGen AST_Assign where
  randO = recurseRunRandChoice nullValue
  randChoice = randChoiceList $
    [ AST_Eval  <$> randO
    , do ox <- randListOf 0 3 (pure (,) <*> randO <*> randO)
         o  <- randO
         return (foldr (\(left, op) right -> AST_Assign left op right LocationUnknown) o ox)
    ]

instance Intermediate AssignExpr AST_Assign where
  toInterm ast = case ast of
    AST_Eval   a         -> liftM  EvalExpr   (ti a)
    AST_Assign a b c loc -> liftM4 AssignExpr (ti a) (uc b) (ti c) [loc]
  fromInterm o = case o of
    EvalExpr   a         -> liftM  AST_Eval   (fi a)
    AssignExpr a b c loc -> liftM4 AST_Assign (fi a) (nc b) (fi c) [loc]

instance ObjectClass AST_Assign where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_Assign where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A 'TopLevelExpr' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data TopLevelExpr
  = Attribute Name              AssignExpr Location
  | TopScript ScriptExpr                   Location
  | EventExpr TopLevelEventType CodeBlock  Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData TopLevelExpr where
  rnf (Attribute      a b c) = deepseq a $! deepseq b $! deepseq c ()
  rnf (TopScript      a b  ) = deepseq a $! deepseq b ()
  rnf (EventExpr      a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue TopLevelExpr where
  nullValue = TopScript nullValue LocationUnknown
  testNull (TopScript a LocationUnknown) = testNull a
  testNull _ = False

isAttribute :: TopLevelExpr -> Bool
isAttribute toplevel = case toplevel of { Attribute _ _ _ -> True; _ -> False; }

instance HasLocation TopLevelExpr where
  getLocation o = case o of
    Attribute _ _  o -> o
    TopScript _    o -> o
    EventExpr _ _  o -> o
  setLocation o loc = case o of
    Attribute a b  _ -> Attribute a b loc
    TopScript a    _ -> TopScript a   loc
    EventExpr a b  _ -> EventExpr a b loc
  delLocation o = case o of
    Attribute a b  _ -> Attribute     a  (fd b) lu
    TopScript a    _ -> TopScript (fd a)        lu
    EventExpr a b  _ -> EventExpr     a  (fd b) lu
    where
      lu = LocationUnknown
      fd :: HasLocation a => a -> a
      fd = delLocation

instance PPrintable TopLevelExpr where { pPrint = pPrintInterm }

toplevel_intrm :: String
toplevel_intrm = "top-level intermedaite node"

instance B.Binary TopLevelExpr MTab where
  put o = case o of
    Attribute a             b z -> B.prefixByte 0x51 $ B.put a >> B.put b >> B.put z
    TopScript a               z -> B.prefixByte 0x52 $ B.put a >> B.put z
    EventExpr BeginExprType b z -> B.prefixByte 0x53 $ B.put b >> B.put z
    EventExpr ExitExprType  b z -> B.prefixByte 0x54 $ B.put b >> B.put z
    EventExpr EndExprType   b z -> B.prefixByte 0x55 $ B.put b >> B.put z
  get = B.word8PrefixTable <|> fail "expecting TopLevelExpr"

instance B.HasPrefixTable TopLevelExpr B.Byte MTab where
  prefixTable = B.mkPrefixTableWord8 "TopLevelExpr" 0x51 0x55 $
    [ pure Attribute <*> B.get <*> B.get <*> B.get
    , pure TopScript <*> B.get <*> B.get
    , pure (EventExpr BeginExprType) <*> B.get <*> B.get
    , pure (EventExpr ExitExprType ) <*> B.get <*> B.get
    , pure (EventExpr EndExprType  ) <*> B.get <*> B.get
    ]

-- Since 'TopLevelExpr's can modify the 'ExecUnit', and since 'Exec' is not a stateful monad, a
-- simple hack is used: every update that should occur on executing the expression is returned as a
-- function which can be applied by the context which called it. Refer to the instance for
-- 'Executable' for the 'Program' type to see how the calling context is used to update the state.
instance Executable TopLevelExpr (ExecUnit -> ExecUnit) where
  execute o = _setTopLevelExprError o $ ask >>= \xunit -> case o of
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Attribute a b c -> execThrow $ obj $ concat $
      [ maybe [] ((:[]) . obj . (++(show c)) . uchars) (programModuleName xunit)
      , [obj $ uchars a ++ " expression must occur only at the top of a dao script"]
      , [obj $ prettyShow (Attribute a b c)]
      ]
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    TopScript scrpt _ -> do
      let (LocalStore stor) = execStack xunit
      -- push a namespace onto the stack
      liftIO $ modifyIORef stor (stackPush M.empty)
      -- get the functions declared this far
      pval <- catchPredicate $ execute scrpt
      case pval of
        OK                _  -> return ()
        PFail (ExecReturn _) -> return ()
        PFail           err  -> throwError err
        Backtrack            -> return () -- do not backtrack at the top-level
      -- pop the namespace, keep any local variable declarations
      dict <- liftIO $ atomicModifyIORef stor stackPop
      -- merge the local variables into the global varaibles resource.
      --lift (modifyUnlocked_ (globalData xunit) (return . T.union dict))
      let (GlobalStore stor) = globalData xunit
      liftIO $ modifyMVar_ stor (return . flip M.union dict)
      return id
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    EventExpr typ scrpt _ -> do
      exec <- setupCodeBlock scrpt
      let f = (++[exec])
      return $ case typ of
        BeginExprType -> \xunit -> xunit{ preExec      = f (preExec      xunit) }
        EndExprType   -> \xunit -> xunit{ postExec     = f (postExec     xunit) }
        ExitExprType  -> \xunit -> xunit{ quittingTime = f (quittingTime xunit) }
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass TopLevelExpr where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass TopLevelExpr where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt >> autoDefPPrinter
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A 'AST_TopLevel' is a single declaration for the top-level of the program file. A Dao 'SourceCode'
-- is a list of these directives.
data AST_TopLevel
  = AST_Attribute  Name                   (Com AST_Assign)         Location
  | AST_TopScript  AST_Script                                      Location
  | AST_Event      TopLevelEventType [Comment] AST_CodeBlock       Location
  | AST_TopComment [Comment]
  deriving (Eq, Ord, Typeable, Show)

isAST_Attribute :: AST_TopLevel -> Bool
isAST_Attribute o = case o of { AST_Attribute _ _ _ -> True; _ -> False; }

attributeToList :: AST_TopLevel -> [(Name, Com AST_Assign, Location)]
attributeToList o = case o of { AST_Attribute a b c -> return (a,b,c); _ -> mzero; }

instance NFData AST_TopLevel where
  rnf (AST_Attribute  a b c  ) = deepseq a $! deepseq b $! deepseq c ()
  rnf (AST_TopScript  a b    ) = deepseq a $! deepseq b ()
  rnf (AST_Event      a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d ()
  rnf (AST_TopComment a      ) = deepseq a ()

instance HasNullValue AST_TopLevel where
  nullValue = AST_TopScript nullValue LocationUnknown
  testNull (AST_TopScript a _) = testNull a
  testNull _ = False

instance HasLocation AST_TopLevel where
  getLocation o = case o of
    AST_Attribute  _ _   o -> o
    AST_TopScript  _     o -> o
    AST_Event      _ _ _ o -> o
    AST_TopComment _       -> lu
  setLocation o loc = case o of
    AST_Attribute  a b    _ -> AST_Attribute  a b     loc
    AST_TopScript  a      _ -> AST_TopScript  a       loc
    AST_Event      a b c  _ -> AST_Event      a b c   loc
    AST_TopComment a        -> AST_TopComment a
  delLocation o = case o of
    AST_Attribute  a b    _ -> AST_Attribute     a (fd1 b)                lu
    AST_TopScript  a      _ -> AST_TopScript (fd a)                       lu
    AST_Event      a b c  _ -> AST_Event         a      b  (fd c)         lu
    AST_TopComment a        -> AST_TopComment    a

instance PPrintable AST_TopLevel where
  pPrint o = case o of
    AST_Attribute a b    _ -> pInline [pPrint a, pString "  ", pPrint b, pString ";"]
    AST_TopScript a      _ -> pPrint a
    AST_Event     a b c  _ -> pClosure (pShow a >> mapM_ pPrint b) " { " " }" (map pPrint (getAST_CodeBlock c))
    AST_TopComment a       -> mapM_ (\a -> pPrint a >> pNewLine) a

instance ToDaoStructClass AST_TopLevel Object where
  toDaoStruct = let nm = renameConstructor in ask >>= \o -> case o of
    AST_Attribute  a b   loc -> nm "Attribute" >> "type" .= a >> "expr" .= b >> putLocation loc
    AST_TopScript  a     loc -> nm "TopLevel" >> "script" .= a >> putLocation loc
    AST_Event      a b c loc -> nm "Event" >>
      "type" .= a >> "block" .= c >> putComments b >> putLocation loc
    AST_TopComment a         -> nm "Comment" >> putComments a

instance FromDaoStructClass AST_TopLevel Object where
  fromDaoStruct = msum $
    [ constructor "Attribute" >> pure AST_Attribute <*> req "type" <*> req "expr" <*> location
    , constructor "TopLevel" >> pure AST_TopScript <*> req "script" <*> location
    , constructor "Event" >> pure AST_Event <*> req "type" <*> comments <*> req "block" <*> location
    , constructor "Comment" >> AST_TopComment <$> comments
    ]

instance HasRandGen AST_TopLevel where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ pure AST_Attribute <*> pure (ustr "import")  <*> randO <*> no
    , pure AST_Attribute <*> pure (ustr "require") <*> randO <*> no
    , pure AST_TopScript <*> randO <*> no
    , pure AST_Event     <*> randO <*> randO <*> randO <*> no
    ]

instance Intermediate TopLevelExpr AST_TopLevel where
  toInterm   ast = case ast of
    AST_Attribute a b   loc -> liftM3 Attribute [a]    (uc0 b) (ll loc)
    AST_TopScript a     loc -> liftM2 TopScript (ti a)         (ll loc)
    AST_Event     a _ b loc -> liftM3 EventExpr [a]    (ti  b) (ll loc)
    AST_TopComment _         -> mzero
  fromInterm obj = case obj of
    Attribute a b loc -> liftM3 AST_Attribute [a]         (nc0 b) [loc]
    TopScript a   loc -> liftM2 AST_TopScript (fi a)              [loc]
    EventExpr a b loc -> liftM4 AST_Event     [a]    [[]] (fi  b) [loc]

instance ObjectClass AST_TopLevel where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_TopLevel where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A program is just a list of 'TopLevelExpr's. It serves as the 'Intermediate'
-- representation of a 'AST_SourceCode'.
newtype Program = Program { topLevelExprs :: [TopLevelExpr] } deriving (Eq, Ord, Typeable)

instance Show Program where { show (Program o) = unlines (map show o) }

instance HasNullValue Program where
  nullValue = Program []
  testNull (Program p) = null p

instance HasLocation Program where
  getLocation o = case topLevelExprs o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = Program (fmap delLocation (topLevelExprs o))

-- the number is encoded by the ASCII encoded string "DaoProg\0"
_program_magic_number :: Word64
_program_magic_number = 0x44616f50726f6700

instance B.Binary Program MTab where
  put o = do
    -- place a magic number first, 
    B.putWord64be _program_magic_number
    mapM_ B.put $ topLevelExprs o
  get = do
    magic <- B.lookAhead B.getWord64be
    guard (magic == _program_magic_number)
    B.getWord64be >> fmap Program B.get

-- | Initialized the current 'ExecUnit' by evaluating all of the 'TopLevel' data in a
-- 'AST.AST_SourceCode'.
instance Executable Program ExecUnit where
  execute (Program ast) = do
    (LocalStore stackRef) <- asks execStack
    liftIO $ modifyIORef stackRef (stackPush M.empty)
    updxunit  <- foldl (.) id <$> mapM execute (dropWhile isAttribute ast)
    -- Now, the local variables that were defined in the top level need to be moved to the global
    -- variable store.
    localVars <- liftIO $ atomicModifyIORef stackRef stackPop
    (GlobalStore globalVars) <- asks globalData
    liftIO $ modifyMVar_ globalVars $ \dict -> return (M.union dict localVars)
    fmap updxunit ask

instance ObjectClass Program where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass Program where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefNullTest >> autoDefBinaryFmt
    -- autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A 'SourceCode' is the structure loaded from source code. An 'ExecUnit' object is constructed from
-- 'SourceCode'.
data AST_SourceCode
  = AST_SourceCode
    { sourceModified :: Int
    , sourceFullPath :: UStr
      -- ^ the URL (full file path) from where this source code was received.
    , directives     :: [AST_TopLevel]
    }
  deriving (Eq, Ord, Typeable)

instance NFData AST_SourceCode where
  rnf (AST_SourceCode a b c) = deepseq a $! deepseq b $! deepseq c ()

instance HasNullValue AST_SourceCode where
  nullValue = (AST_SourceCode 0 nil [])
  testNull (AST_SourceCode 0 a []) | a==nil = True
  testNull _ = False

instance PPrintable AST_SourceCode where
  pPrint sc = do
    let (attrs, dirs) = span isAST_Attribute (directives sc)
    mapM_ pPrint attrs
    pForceNewLine
    mapM_ (\dir -> pPrint dir >> pForceNewLine) dirs

instance ToDaoStructClass AST_SourceCode Object where
  toDaoStruct = void $ do
    renameConstructor "SourceCode"
    "modified" .=@ sourceModified
    "path"     .=@ sourceFullPath
    asks directives >>= define "code" . listToObj

instance FromDaoStructClass AST_SourceCode Object where
  fromDaoStruct = constructor "SourceCode" >>
    pure AST_SourceCode <*> req "modified" <*> req "path" <*> reqList "code"

instance Intermediate Program AST_SourceCode where
  toInterm   ast = return $ Program (directives ast >>= toInterm)
  fromInterm obj = return $
    AST_SourceCode
    { sourceModified = 0
    , sourceFullPath = nil
    , directives     = topLevelExprs obj >>= fromInterm
    }

instance ObjectClass AST_SourceCode where { obj=new; fromObj=objFromHaskellData; }
instance HaskellDataClass AST_SourceCode where
  haskellDataInterface = interface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest >> autoDefPPrinter
    autoDefToStruct -- >> autoDefFromStruct

----------------------------------------------------------------------------------------------------
-- $Builtin_object_interfaces
-- The following functions provide object interfaces for essential data types.

instance HaskellDataClass () where { haskellDataInterface = interface () (return ()) }

instance HaskellDataClass GlobAction where
  haskellDataInterface = interface (GlobAction [] undefined) $ do
    defCallable $ \rule -> do
      let vars o = case o of {Wildcard _ -> 1; AnyOne _ -> 1; Single _ -> 0; }
      let m = maximum $ map (sum . map vars . getPatUnits) $ globPattern rule
      let lu = LocationUnknown
      let params = flip ParamListExpr lu $ NotTypeChecked $
            map (flip (ParamExpr False) lu . NotTypeChecked . ustr . ("var"++) . show) [(1::Int)..m]
      return $ return $
        CallableCode
        { argsPattern    = params
        , returnType     = nullValue
        , codeSubroutine = globSubroutine rule
            -- TODO: the subroutine should be scanned for integer references and replaced with local
            -- variables called "varN" where N is the number of the integer reference.
        }

----------------------------------------------------------------------------------------------------

-- | Nearly every accesssor function in the 'Interface' data type take the form
-- > 'Interface' 'Data.Dynamic.Dynamic' -> Maybe ('Data.Dynamic.Dynamic' -> method)
-- where the first 'Data.Dynamic.Dynamic' value is analogous to the @this@" pointer in C++-like
-- languages, and where @method@ is any function, for example an equality function @a -> a -> Bool@
-- or an iterator function @Exec [Object]@. This function takes the @this@ value, an
-- 'Interface', and an 'Interface' accessor (for example 'objEquality' or
-- 'objIterator') and if the accessor is not 'Prelude.Nothing', the @this@ object is applied to the
-- method and the partial application is returned. If the accessor does evaluate to
-- 'Prelude.Nothing' the exception value is thrown. If the @this@ object has not been constructed
-- with 'OHaskell', the exception value is thrown.
evalObjectMethod :: Object -> Object -> (Interface Dynamic -> Maybe (Dynamic -> method)) -> Exec method
evalObjectMethod errmsg this getter = case this of
  OHaskell (HaskellData this ifc) -> case getter ifc of
    Nothing -> execThrow errmsg
    Just fn -> return (fn this)
  _ -> execThrow errmsg

----------------------------------------------------------------------------------------------------

type Get     a = B.GGet  MethodTable a
type PutM    a = B.GPutM MethodTable
type Put       = B.GPut  MethodTable

-- This is only necessary to shorten the name 'MethodTable' because it is used throughout so many
-- instance declarations and type contexts.
type MTab = MethodTable

----------------------------------------------------------------------------------------------------

newtype MethodTable = MethodTable (M.Map UStr (Interface Dynamic))

instance Monoid MethodTable where
  mempty  = MethodTable mempty
  mappend (MethodTable a) (MethodTable b) = MethodTable (M.union b a)

-- | Lookup an 'Interface' by it's name from within the 'Exec' monad.
execGetObjTable :: UStr -> Exec (Maybe (Interface Dynamic))
execGetObjTable nm = asks (lookupMethodTable nm . globalMethodTable)

lookupMethodTable :: UStr -> MethodTable -> Maybe (Interface Dynamic)
lookupMethodTable nm (MethodTable tab) = M.lookup nm tab

-- not for export, use 'daoClass'
insertMethodTable
  :: (Typeable o, HaskellDataClass o)
  => o
  -> UStr
  -> Interface o
  -> MethodTable
  -> MethodTable
insertMethodTable _ nm ifc = flip mappend $
  MethodTable (M.singleton nm (interfaceToDynamic ifc))

typeRepToUStr :: TypeRep -> UStr
typeRepToUStr a = let con = typeRepTyCon a in ustr (tyConModule con ++ '.' : tyConName con)

instance B.HasCoderTable MethodTable where
  getEncoderForType nm mtab = fmap fst $ lookupMethodTable nm mtab >>= objBinaryFormat
  getDecoderForType nm mtab = fmap snd $ lookupMethodTable nm mtab >>= objBinaryFormat

----------------------------------------------------------------------------------------------------

-- | Instantiate your data type into this class when it makes sense for your data type to be used in
-- a "for" statement in the Dao programming language.
class HasIterator obj where
  -- | This function converts your object to a list. Conversion is done as lazily as possible to
  -- prevent "for" statements from eating up a huge amount of memory when iteration produces a large
  -- number of objects. For example, lets say your @obj@ is an association list. This function
  -- should return a list of every assocaition in the @obj@. Each association object will be stored
  -- in a local variable and the body of the "for" statement will be evaluated with that local
  -- variable.
  iterateObject :: obj -> Exec [Object]
  -- | This function takes the list of objects that was produced after evaluating the "for"
  -- statement and uses it to create a new @obj@ data. For example, if your data type is an
  -- association list and the "for" loop eliminates every 'Object' not satisfying a predicate, the
  -- object list passed here will be the lists of 'Object's that did satisfy the predicate and you
  -- should construct a new @obj@ using this new list of 'Object's.
  foldObject :: obj -> [Object] -> Exec obj

instance HasIterator [Object] where { iterateObject = return; foldObject _ = return; }

instance HasIterator UStr where
  iterateObject = return . fmap OChar . uchars
  foldObject  _ = fmap toUStr . foldM f "" where
    f str o = case o of
      OString o -> return (str++uchars o)
      OChar   o -> return (str++[o])
      o         -> execThrow $ obj [obj "object cannot be used to construct string:", o]

----------------------------------------------------------------------------------------------------

cant_iterate :: Object -> String -> Exec ig
cant_iterate o ifc = execThrow $ obj
  [obj $ "object of type "++ifc++" cannot be iterated in a \"for\" statement:", o]

instance HasIterator HaskellData where
  iterateObject obj@(HaskellData o ifc) = case objIterator ifc of
    Just (iter, _) -> iter o
    Nothing        -> cant_iterate (OHaskell obj) (show (objHaskellType ifc))
  foldObject obj@(HaskellData o ifc) ox = case objIterator ifc of
    Just (_, fold) -> fmap (flip HaskellData ifc) (fold o ox)
    Nothing        -> cant_iterate (OHaskell obj) (show (objHaskellType ifc))

instance HasIterator Object where
  iterateObject obj = case obj of
    OString  o -> iterateObject o
    OList    o -> iterateObject o
    OHaskell o -> iterateObject o
    _ -> cant_iterate obj (show $ typeOfObj obj)
  foldObject obj ox = case obj of
    OString  o -> fmap OString  (foldObject o ox)
    OList    o -> fmap OList    (foldObject o ox)
    OHaskell o -> fmap OHaskell (foldObject o ox)
    _ -> cant_iterate obj (show $ typeOfObj obj)

----------------------------------------------------------------------------------------------------

-- | This class only exists to allow many different Haskell data types to declare their
-- 'Interface' under the same funcion name: 'haskellDataInterface'. Instantiate this function with
-- the help of the 'interface' function.
class HaskellDataClass typ where { haskellDataInterface :: Interface typ }

instance HaskellDataClass Location where
  haskellDataInterface = interface LocationUnknown $ do
    autoDefEquality >> autoDefOrdering
    autoDefToStruct -- >> autoDefFromStruct

-- | This is a convenience function for calling 'OHaskell' using just an initial value of type
-- @typ@. The 'Interface' is retrieved automatically using the instance of 'haskellDataInterface' for
-- the @typ@.
toHaskellData :: (HaskellDataClass typ, Typeable typ) => typ -> HaskellData
toHaskellData t = HaskellData (toDyn t) (interfaceTo t haskellDataInterface) where
  interfaceTo :: Typeable typ => typ -> Interface typ -> Interface Dynamic
  interfaceTo _ ifc = interfaceToDynamic ifc

-- | Inverse operation of 'toHaskellData', useful when instantiating 'ObjectClass', uses
-- 'Data.Dynamic.fromDynamic' to extract the value that has been wrapped in up the 'HaskellData'
-- constructor.
fromHaskellData :: (HaskellDataClass typ, Typeable typ) => HaskellData -> Maybe typ
fromHaskellData (HaskellData o _) = fromDynamic o

-- | This is all of the functions used by the "Dao.Evaluator" when manipulating objects in a Dao
-- program. Behavior of objects when they are used in "for" statements or "with" statements, or when
-- they are dereferenced using the "@" operator, or when they are used in equations are all defined
-- here.
-- 
-- So this table is the reason you instantiate 'HaskellDataClass'.
-- 
-- @obj@ specifies the container type that will wrap-up data of type @typ@. @obj@ is the type used
-- throughout the runtime system to symbolize the basic unit of information operated on by
-- computations.
-- 
-- @typ@ specifies the type that you want to wrap-up into an @obj@ constructor. When you want to,
-- for example, check for equality between object of type @typ@, you can define a function for
-- 'objEquality'. All of the other polymorphic types are bound to the @typ@ types by the functional
-- dependencies mechanism of the Haskell language.
-- 
-- @exec@ specifies a monad in which to evaluate functions which may need to cause side-effects.
-- This should usually be a 'Control.Monad.Monad'ic type like @IO@ or 'Exec'.
data Interface typ =
  Interface
  { objHaskellType     :: TypeRep -- ^ this type is deduced from the initial value provided to the 'interface'.
  , objCastFrom        :: Maybe (Object -> typ)                                                     -- ^ defined by 'defCastFrom'
  , objEquality        :: Maybe (typ -> typ -> Bool)                                                -- ^ defined by 'defEquality'
  , objOrdering        :: Maybe (typ -> typ -> Ordering)                                            -- ^ defined by 'defOrdering'
  , objBinaryFormat    :: Maybe (typ -> Put, Get typ)                                               -- ^ defined by 'defBinaryFmt'
  , objNullTest        :: Maybe (typ -> Bool)                                                       -- ^ defined by 'defNullTest'
  , objPPrinter        :: Maybe (typ -> PPrint)                                                     -- ^ defined by 'defPPrinter'
  , objIterator        :: Maybe (typ -> Exec [Object], typ -> [Object] -> Exec typ)                 -- ^ defined by 'defIterator'
  , objIndexer         :: Maybe (typ -> Object -> Exec Object)                                      -- ^ defined by 'defIndexer'
  , objToStruct        :: Maybe (typ -> Exec T_struct)                                              -- ^ defined by 'defStructFormat'
  , objFromStruct      :: Maybe (T_struct -> Exec typ)                                              -- ^ defined by 'defStructFormat'
  , objDictInit        :: Maybe ([Object] -> Exec typ, typ -> [(Object, UpdateOp, Object)] -> Exec typ) -- ^ defined by 'defDictInit'
  , objListInit        :: Maybe ([Object] -> Exec typ, typ -> [Object] -> Exec typ)                     -- ^ defined by 'defDictInit'
  , objUpdateOpTable   :: Maybe (Array UpdateOp (Maybe (UpdateOp -> typ -> Object -> Exec Object))) -- ^ defined by 'defUpdateOp'
  , objInfixOpTable    :: Maybe (Array InfixOp  (Maybe (InfixOp  -> typ -> Object -> Exec Object))) -- ^ defined by 'defInfixOp'
  , objArithPfxOpTable :: Maybe (Array ArithPfxOp (Maybe (ArithPfxOp -> typ -> Exec Object)))       -- ^ defined by 'defPrefixOp'
  , objCallable        :: Maybe (typ -> Exec [CallableCode])                                             -- ^ defined by 'defCallable'
  , objDereferencer    :: Maybe (typ -> Exec (Maybe Object))
  }
  deriving Typeable

instance Eq  (Interface typ) where { a==b = objHaskellType a == objHaskellType b }

instance Ord (Interface typ) where { compare a b = compare (objHaskellType a) (objHaskellType b) }

-- | This function works a bit like 'Data.Functor.fmap', but maps an 'Interface' from one type
-- to another. This requires two functions: one that can cast from the given type to the adapted
-- type (to convert outputs of functions), and one that can cast back from the adapted type to the
-- original type (to convert inputs of functions). Each coversion function takes a string as it's
-- first parameter, this is a string containing the name of the function that is currently making
-- use of the conversion operation. Should you need to use 'Prelude.error' or 'mkExecError', this
-- string will allow you to throw more informative error messages. WARNING: this function leaves
-- 'objHaskellType' unchanged, the calling context must change it.
interfaceAdapter
  :: (Typeable typ_a, Typeable typ_b)
  => (String -> typ_a -> typ_b)
  -> (String -> typ_b -> typ_a)
  -> Interface typ_a
  -> Interface typ_b
interfaceAdapter a2b b2a ifc = 
  ifc
  { objCastFrom        = let n="objCastFrom"      in fmap (fmap (a2b n)) (objCastFrom ifc)
  , objEquality        = let n="objEquality"      in fmap (\eq  a b -> eq  (b2a n a) (b2a n b)) (objEquality ifc)
  , objOrdering        = let n="objOrdering"      in fmap (\ord a b -> ord (b2a n a) (b2a n b)) (objOrdering ifc)
  , objBinaryFormat    = let n="objBinaryFormat"  in fmap (\ (toBin , fromBin) -> (toBin . b2a n, fmap (a2b n) fromBin)) (objBinaryFormat ifc)
  , objNullTest        = let n="objNullTest"      in fmap (\null b -> null (b2a n b)) (objNullTest ifc)
  , objPPrinter        = let n="objPPrinter"      in fmap (\eval -> eval . b2a n) (objPPrinter ifc)
  , objIterator        = let n="objIterator"      in fmap (\ (iter, fold) -> (iter . b2a n, \typ -> fmap (a2b n) . fold (b2a n typ))) (objIterator ifc)
  , objIndexer         = let n="objIndexer"       in fmap (\indx b -> indx (b2a n b)) (objIndexer  ifc)
  , objToStruct        = let n="objToStruct"      in fmap (\toTree -> toTree . b2a n) (objToStruct ifc)
  , objFromStruct      = let n="objFromStruct"    in fmap (\fromTree -> fmap (a2b n) . fromTree) (objFromStruct ifc)
  , objDictInit        = let n="objDictInit"      in fmap (\ (init, eval) -> (\ox -> fmap (a2b n) (init ox), \typ ox -> fmap (a2b n) (eval (b2a n typ) ox))) (objDictInit ifc)
  , objListInit        = let n="objListInit"      in fmap (\ (init, eval) -> (\ox -> fmap (a2b n) (init ox), \typ ox -> fmap (a2b n) (eval (b2a n typ) ox))) (objListInit ifc)
  , objUpdateOpTable   = let n="objUpdateOpTable" in fmap (fmap (fmap (\updt op b -> updt op (b2a n b)))) (objUpdateOpTable ifc)
  , objInfixOpTable    = let n="objInfixOpTable"  in fmap (fmap (fmap (\infx op b -> infx op (b2a n b)))) (objInfixOpTable  ifc)
  , objArithPfxOpTable = let n="objPrefixOpTabl"  in fmap (fmap (fmap (\prfx op b -> prfx op (b2a n b)))) (objArithPfxOpTable ifc)
  , objCallable        = let n="objCallable"      in fmap (\eval -> eval . b2a n) (objCallable ifc)
  , objDereferencer    = let n="objDerferencer"   in fmap (\eval -> eval . b2a n) (objDereferencer ifc)
  }

interfaceToDynamic :: Typeable typ => Interface typ -> Interface Dynamic
interfaceToDynamic oi = interfaceAdapter (\ _ -> toDyn) (from oi) oi where
  from :: Typeable typ => Interface typ -> String -> Dynamic -> typ
  from oi msg dyn = fromDyn dyn (dynErr oi msg dyn)
  dynErr :: Typeable typ => Interface typ -> String -> Dynamic -> typ
  dynErr oi msg dyn = error $ concat $
    [ "The '", msg
    , "' function defined for objects of type ", show (objHaskellType oi)
    , " was evaluated on an object of type ", show (dynTypeRep dyn)
    ]

-- Used to construct an 'Interface' in a "Control.Monad.State"-ful way. Instantiates
-- 'Data.Monoid.Monoid' to provide 'Data.Monoid.mempty' an allows multiple inheritence by use of the
-- 'Data.Monoid.mappend' function in the same way as
data HDIfcBuilder typ =
  HDIfcBuilder
  { objIfcCastFrom      :: Maybe (Object -> typ)
  , objIfcEquality      :: Maybe (typ -> typ -> Bool)
  , objIfcOrdering      :: Maybe (typ -> typ -> Ordering)
  , objIfcBinaryFormat  :: Maybe (typ -> Put, Get typ)
  , objIfcNullTest      :: Maybe (typ -> Bool)
  , objIfcPPrinter      :: Maybe (typ -> PPrint)
  , objIfcIterator      :: Maybe (typ -> Exec [Object], typ -> [Object] -> Exec typ)
  , objIfcIndexer       :: Maybe (typ -> Object -> Exec Object)
  , objIfcToStruct      :: Maybe (typ -> Exec T_struct)
  , objIfcFromStruct    :: Maybe (T_struct -> Exec typ)
  , objIfcDictInit      :: Maybe ([Object] -> Exec typ, typ -> [(Object, UpdateOp, Object)] -> Exec typ)
  , objIfcListInit      :: Maybe ([Object] -> Exec typ, typ -> [Object] -> Exec typ)
  , objIfcUpdateOpTable :: [(UpdateOp, UpdateOp -> typ -> Object -> Exec Object)]
  , objIfcInfixOpTable  :: [(InfixOp , InfixOp  -> typ -> Object -> Exec Object)]
  , objIfcPrefixOpTable :: [(ArithPfxOp, ArithPfxOp -> typ -> Exec Object)]
  , objIfcCallable      :: Maybe (typ -> Exec [CallableCode])
  , objIfcDerefer       :: Maybe (typ -> Exec (Maybe Object))
  }

initHDIfcBuilder :: HDIfcBuilder typ
initHDIfcBuilder =
  HDIfcBuilder
  { objIfcCastFrom      = Nothing
  , objIfcEquality      = Nothing
  , objIfcOrdering      = Nothing
  , objIfcBinaryFormat  = Nothing
  , objIfcNullTest      = Nothing
  , objIfcPPrinter      = Nothing
  , objIfcIterator      = Nothing
  , objIfcIndexer       = Nothing
  , objIfcToStruct      = Nothing
  , objIfcFromStruct    = Nothing
  , objIfcDictInit      = Nothing
  , objIfcListInit      = Nothing
  , objIfcUpdateOpTable = []
  , objIfcInfixOpTable  = []
  , objIfcPrefixOpTable = []
  , objIfcCallable      = Nothing
  , objIfcDerefer       = Nothing
  }

-- | A handy monadic interface for defining an 'Interface' using nice, clean procedural
-- syntax.
type    DaoClassDef typ = DaoClassDefM typ ()
newtype DaoClassDefM typ a = DaoClassDefM { daoClassDefState :: State (HDIfcBuilder typ) a }
instance Typeable typ => Functor (DaoClassDefM typ) where
  fmap f (DaoClassDefM m) = DaoClassDefM (fmap f m)
instance Typeable typ => Monad (DaoClassDefM typ) where
  return = DaoClassDefM . return
  (DaoClassDefM m) >>= f = DaoClassDefM (m >>= daoClassDefState . f)
instance Typeable typ => Applicative (DaoClassDefM typ) where { pure=return; (<*>)=ap; }

_updHDIfcBuilder :: Typeable typ => (HDIfcBuilder typ -> HDIfcBuilder typ) -> DaoClassDefM typ ()
_updHDIfcBuilder = DaoClassDefM . modify

-- | The callback function defined here is used when objects of your @typ@ can be constructed from
-- some other 'Object'. This function is used to convert an 'Object' of another types to an data
-- type of your @typ@ when it is necessary to do so (for example, evaluating the @==@ or @!=@
-- operator).
defCastFrom :: Typeable typ => (Object -> typ) -> DaoClassDefM typ ()
defCastFrom fn = _updHDIfcBuilder(\st->st{objIfcCastFrom=Just fn})

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @==@ and @!=@ operators in Dao programs. However using this is slightly
-- different than simply overriding the @==@ or @!=@ operators. Defining an equality reliation with
-- this function also allows Haskell language programs to compare your object to other objects
-- without unwrapping them from the 'Object' wrapper.
--
-- This function automatically define an equality operation over your @typ@ using the
-- instantiation of 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function.
-- The 'defCastFrom' function is used to cast 'Object's to a value of your @typ@, and then the
-- @Prelude.==@ function is evaluated. If you eventually never define a type casting funcion using
-- 'defCastFrom', this function will fail, but it will fail lazily and at runtime, perhaps when you
-- least expect it, so be sure to define 'defCastFrom' at some point.
autoDefEquality :: (Typeable typ, Eq typ) => DaoClassDefM typ ()
autoDefEquality = defEquality (==)

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @==@ and @!=@ operators in Dao programs. However using this is slightly
-- different than simply overriding the @==@ or @!=@ operators. Defining an equality relation with
-- this function also allows Haskell language programs to compare your object to other objects
-- without unwrapping them from the 'Object' wrapper.
--
-- This function differs from 'autoDefEquality' because you must provide a customized equality
-- relation for your @typ@, if the 'autoDefEquality' and 'defCastFrom' functions are to be avoided
-- for some reason.
defEquality :: (Typeable typ, Eq typ) => (typ -> typ -> Bool) -> DaoClassDefM typ ()
defEquality fn = _updHDIfcBuilder(\st->st{objIfcEquality=Just fn})

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @<@, @>@, @<=@, and @>=@ operators in Dao programs. However using this is
-- slightly different than simply overriding the @<@, @>@, @<=@, or @>=@ operators. Defining an
-- equality relation with this function also allows Haskell language programs to compare your obejct
-- to other objects without unwrapping them from the 'Object' wrapper.
-- 
-- Automatically define an ordering for your @typ@ using the instantiation of
-- 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function. The 'defCastFrom'
-- function is used to cast 'Object's to a value of your @typ@, and then the @Prelude.==@ function
-- is evaluated. If you eventually never define a type casting funcion using 'defCastFrom', this
-- function will fail, but it will fail lazily and at runtime, perhaps when you least expect it, so
-- be sure to define 'defCastFrom' at some point.
autoDefOrdering :: (Typeable typ, Ord typ) => DaoClassDefM typ ()
autoDefOrdering = defOrdering compare

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @<@, @>@, @<=@, and @>=@ operators in Dao programs. However using this is
-- slightly different than simply overriding the @<@, @>@, @<=@, or @>=@ operators. Defining an
-- equality relation with this function also allows Haskell language programs to compare your obejct
-- to other objects without unwrapping them from the 'Object' wrapper.
-- 
-- Define a customized ordering for your @typ@, if the 'autoDefEquality' and 'defCastFrom'
-- functions are to be avoided for some reason.
defOrdering :: (Typeable typ) => (typ -> typ -> Ordering) -> DaoClassDefM typ ()
defOrdering fn = _updHDIfcBuilder(\st->st{objIfcOrdering=Just fn})

-- | The callback function defined here is used if an object of your @typ@ should ever need to be
-- stored into a binary file in persistent storage (like your filesystem) or sent across a channel
-- (like a UNIX pipe or a socket).
-- 
-- It automatically define the binary encoder and decoder using the 'Data.Binary.Binary' class
-- instantiation for this @typ@.
autoDefBinaryFmt :: (Typeable typ, B.Binary typ MethodTable) => DaoClassDefM typ ()
autoDefBinaryFmt = defBinaryFmt B.put B.get

-- | This function is used if an object of your @typ@ should ever need to be stored into a binary
-- file in persistent storage (like your filesystem) or sent across a channel (like a UNIX pipe or a
-- socket).
-- 
-- If you have binary coding and decoding methods for your @typ@ but for some silly reason not
-- instantiated your @typ@ into the 'Data.Binary.Binary' class, your @typ@ can still be used as a
-- binary formatted object by the Dao system if you define the encoder and decoder using this
-- function. However, it would be better if you instantiated 'Data.Binary.Binary' and used
-- 'autoDefBinaryFmt' instead.
defBinaryFmt :: (Typeable typ) => (typ -> Put) -> Get typ -> DaoClassDefM typ ()
defBinaryFmt put get = _updHDIfcBuilder(\st->st{objIfcBinaryFormat=Just(put,get)})

autoDefNullTest :: (Typeable typ, HasNullValue typ) => DaoClassDefM typ ()
autoDefNullTest = defNullTest testNull

-- | The callback function defined here is used if an object of your @typ@ is ever used in an @if@
-- or @while@ statement in a Dao program. This function will return @Prelude.True@ if the object is
-- of a null value, which will cause the @if@ or @while@ test to fail and execution of the Dao
-- program will branch accordingly. There is no default method for this function so it must be
-- defined by this function, otherwise your object cannot be tested by @if@ or @while@ statements.
defNullTest :: Typeable typ => (typ -> Bool) -> DaoClassDefM typ ()
defNullTest fn = _updHDIfcBuilder(\st->st{objIfcNullTest=Just fn})

-- | The callback function to be called when the "print" built-in function is used.
defPPrinter :: Typeable typ => (typ -> PPrint) -> DaoClassDefM typ ()
defPPrinter fn = _updHDIfcBuilder(\st->st{objIfcPPrinter=Just fn})

-- | The callback function to be called when the "print" built-in function is used.
autoDefPPrinter :: (Typeable typ, PPrintable typ) => DaoClassDefM typ ()
autoDefPPrinter = defPPrinter pPrint

-- | The callback function defined here is used if an object of your @typ@ is ever used in a @for@
-- statement in a Dao program. However it is much better to instantiate your @typ@ into the
-- 'HasIterator' class and use 'autoDefIterator' instead.
defIterator :: Typeable typ => (typ -> Exec [Object]) -> (typ -> [Object] -> Exec typ) -> DaoClassDefM typ ()
defIterator iter fold = _updHDIfcBuilder(\st->st{objIfcIterator=Just(iter,fold)})

-- | Using the instantiation of the 'HasIterator' class for your @typ@, installs the necessary
-- callbacks into the 'Interface' to allow your data type to be iterated over in the Dao
-- programming language when it is used in a "for" statement.
autoDefIterator :: (Typeable typ, HasIterator typ) => DaoClassDefM typ ()
autoDefIterator = defIterator iterateObject foldObject

-- | The callback function defined here is used at any point in a Dao program where an expression
-- containing your object typ is subscripted with square brackets, for example in the statement:
-- @x[0] = t[1][A][B];@ The object passed to your callback function is the object containing the
-- subscript value. So in the above example, if the local variables @x@ and @t@ are both values of
-- your @typ@, this callback function will be evaluated four times:
-- 1.  with the given 'Object' parameter being @('OInt' 0)@ and the @typ@ parameter as the value stored in
--     the local variable @x@.
-- 2.  with the given 'Object' parameter being @('OInt' 1)@ and the @typ@ parameter as the value
--     stored in the local variable @y@.
-- 3.  once with the 'Object' parameter being the result of dereferencing the local varaible @A@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 4.  once the given 'Object' parameter being the result of dereferencing the local variable @B@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 
-- Statements like this:
-- > a[0,1,2]
-- are evaluated by the "Dao.Evaluator" module in the exact same way as statements like this:
-- > a[0][1][2]
defIndexer :: Typeable typ => (typ -> Object -> Exec Object) -> DaoClassDefM typ ()
defIndexer fn = _updHDIfcBuilder(\st->st{objIfcIndexer=Just fn})

-- | Use your data type's instantiation of 'ToDaoStructClass' to call 'defToStruct'.
autoDefToStruct :: forall typ . (Typeable typ, ToDaoStructClass typ Object) => DaoClassDefM typ ()
autoDefToStruct = defToStruct ((predicate :: Predicate ExecControl T_struct -> Exec T_struct) . fmapPFail ((\o -> mkExecError{ execReturnValue=Just o}) . new) . fromData toDaoStruct)

-- | When a label referencing your object has a field record accessed, for example:
-- > c = a.b;
-- if your object is referenced by @a@ and the script expression wants to access a record called @b@
-- from within it, then function defined here will be used.
defToStruct :: Typeable typ => (typ -> Exec T_struct) -> DaoClassDefM typ ()
defToStruct encode = _updHDIfcBuilder (\st -> st{ objIfcToStruct=Just encode })

-- | When a label referencing your object has a field record updated, for example:
-- > a.b = c;
-- if your object is referenced by @a@ and the script expression wants to update a record called @b@
-- within it by assigning it the value referenced by @c@, then the function defined here will be
-- used.
autoDefFromStruct :: (Typeable typ, FromDaoStructClass typ Object) => DaoClassDefM typ ()
autoDefFromStruct = defFromStruct (predicate . fmapPFail ((\o -> mkExecError{ execReturnValue=Just o }) . new) . toData fromDaoStruct)

-- | If for some reason you need to define a tree encoder and decoder for the 'Interface' of your
-- @typ@ without instnatiating 'ToDaoStructClass' or 'FromDaoStructClass', use
-- this function to define the tree encoder an decoder directly
defFromStruct :: Typeable typ => (T_struct -> Exec typ) -> DaoClassDefM typ ()
defFromStruct decode = _updHDIfcBuilder (\st -> st{ objIfcFromStruct=Just decode })

-- | The callback defined here is used when a Dao program makes use of the static initialization
-- syntax of the Dao programming language, which are expression of this form:
-- > a = MyType { paramA=initA, paramB=initB, .... };
-- > a = MyType(param1, param2, ...., paramN) { paramA=initA, paramB=initB, .... };
-- When the interpreter sees this form of expression, it looks up the 'Interface' for your
-- @typ@ and checks if a callback has been defined by 'defDictInit'. If so, then the callback is
-- evaluated with a list of object values passed as the first parameter which contain the object
-- values written in the parentheses, and a 'T_dict' as the second paramter containing the tree
-- structure that was constructed with the expression in the braces.
defDictInit :: Typeable typ => ([Object] -> Exec typ) -> (typ -> [(Object, UpdateOp, Object)] -> Exec typ) -> DaoClassDefM typ ()
defDictInit fa fb = _updHDIfcBuilder(\st->st{objIfcDictInit=Just (fa, fb)})

-- | The callback defined here is used when a Dao program makes use of the static initialization
-- syntax of the Dao programming language, which are expression of this form:
-- > a = MyType { initA, initB, .... };
-- > a = MyType(param1, param2, ...., paramN) { initA, initB, .... };
-- When the interpreter sees this form of expression, it looks up the 'Interface' for your
-- @typ@ and checks if a callback has been defined by 'defDictInit'. If so, then the callback is
-- evaluated with a list of object values passed as the first parameter which contain the object
-- values written in the parentheses, and a 'T_dict' as the second paramter containing the tree
-- structure that was constructed with the expression in the braces.
-- 
-- You can define both 'defDictInit' and 'defListInit', but if both are defined, only 'defDictInit'
-- will be used.
defListInit :: Typeable typ => ([Object] -> Exec typ) -> (typ -> [Object] -> Exec typ) -> DaoClassDefM typ ()
defListInit fa fb = _updHDIfcBuilder(\st->st{objIfcListInit=Just(fa,fb)})

-- | Overload update/assignment operators in the Dao programming language, for example @=@, @+=@,
-- @<<=@ and so on. Call this method as many times with as many different 'UpdateOp's as necessary.
-- 
-- Like with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
-- 
-- If you define two callbacks for the same 'UpdateOp', this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defUpdateOp :: Typeable typ => UpdateOp -> (UpdateOp -> typ -> Object -> Exec Object) -> DaoClassDefM typ ()
defUpdateOp op fn = _updHDIfcBuilder(\st->st{objIfcUpdateOpTable=objIfcUpdateOpTable st++[(op, fn)]})

-- | Overload infix operators in the Dao programming language, for example @+@, @*@, or @<<@.
-- 
-- Like with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
--
-- If you define two callbacks for the same 'UpdateOp', this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defInfixOp :: Typeable typ => InfixOp -> (InfixOp -> typ -> Object -> Exec Object) -> DaoClassDefM typ ()
defInfixOp op fn = _updHDIfcBuilder $ \st -> st{objIfcInfixOpTable  = objIfcInfixOpTable  st ++ [(op, fn)] }

-- | Overload prefix operators in the Dao programming language, for example @!@, @~@, @-@, and @+@.
-- 
-- Like with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
-- 
-- If you define two callbacks for the same 'UpdateOp', this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defPrefixOp :: Typeable typ => ArithPfxOp -> (ArithPfxOp -> typ -> Exec Object) -> DaoClassDefM typ ()
defPrefixOp op fn = _updHDIfcBuilder $ \st -> st{objIfcPrefixOpTable = objIfcPrefixOpTable st ++ [(op, fn)] }

defCallable :: Typeable typ => (typ -> Exec [CallableCode]) -> DaoClassDefM typ ()
defCallable fn = _updHDIfcBuilder (\st -> st{objIfcCallable=Just fn})

defDeref :: Typeable typ => (typ -> Exec (Maybe Object)) -> DaoClassDefM typ ()
defDeref  fn = _updHDIfcBuilder (\st -> st{objIfcDerefer=Just fn})

-- | Rocket. Yeah. Sail away with you.
defLeppard :: Typeable typ => rocket -> yeah -> DaoClassDefM typ ()
defLeppard _ _ = return ()

-- | This is the Dao 'Object' interface to the Haskell language. Every function in this data type
-- allows you to customize the behavior of the Dao evaluator for a particular Haskell data type
-- @typ@. In order for your type to be useful, it must be possible to pass your data type to the
-- 'OHaskell' constructor, which requires a data type of 'Data.Dynamic.Dynamic', which means your
-- @typ@ must derive a class instance for 'Data.Typeable.Typeable'. The first parameter of type
-- @typ@ is not used except to retrieve it's 'Data.Typeable.TypeRep' using the
-- 'Data.Typealble.typeOf' function, it is safe to pass any data constructor with all of it's fields
-- 'Prelude.undefined', just the constructor itself must not be 'Prelude.undefined'.
-- 
-- The @'DaoClassDefM'@ parameter you pass to this function is a monadic function so you can simply
-- declare the functionality you would like to include in this object one line at a time using
-- the procedural coding style. Each line in the "procedure" will be one of the @def*@ functions,
-- for example 'autoDefEquality' or 'autoDefOrdering'.
interface :: Typeable typ => typ -> DaoClassDefM typ ig -> Interface typ
interface init defIfc =
  Interface
  { objHaskellType     = typ
  , objCastFrom        = objIfcCastFrom     ifc
  , objEquality        = objIfcEquality     ifc
  , objOrdering        = objIfcOrdering     ifc
  , objBinaryFormat    = objIfcBinaryFormat ifc
  , objNullTest        = objIfcNullTest     ifc
  , objPPrinter        = objIfcPPrinter     ifc
  , objIterator        = objIfcIterator     ifc
  , objIndexer         = objIfcIndexer      ifc
  , objToStruct        = objIfcToStruct     ifc
  , objFromStruct      = objIfcFromStruct   ifc
  , objDictInit        = objIfcDictInit     ifc
  , objListInit        = objIfcListInit     ifc
  , objUpdateOpTable   = mkArray "defUpdateOp" $ objIfcUpdateOpTable ifc
  , objInfixOpTable    = mkArray "defInfixOp"  $ objIfcInfixOpTable  ifc
  , objArithPfxOpTable = mkArray "defPrefixOp" $ objIfcPrefixOpTable ifc
  , objCallable        = objIfcCallable     ifc
  , objDereferencer    = objIfcDerefer      ifc
  }
  where
    typ               = typeOf init
    ifc               = execState (daoClassDefState defIfc) initHDIfcBuilder
    mkArray oiName elems =
      if null elems
        then  Nothing
        else  minAccumArray (onlyOnce oiName) Nothing $ map (\ (i, e) -> (i, (i, Just e))) elems
    onlyOnce oiName a b  = case b of
      (_, Nothing) -> a
      (i, Just  _) -> conflict oiName ("the "++show i++" operator")
    conflict oiName funcName = error $ concat $
      [ "'", oiName
      , "' has conflicting functions for ", funcName
      , " for the 'HaskellDataClass' instantiation of the '", show typ
      , "' Haskell data type."
      ]

