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
-- along St.with St.this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

module Dao.Evaluator where

import           Dao.String
import qualified Dao.Tree as T
import           Dao.Stack
import           Dao.Token  hiding (asString)
import           Dao.Object
import           Dao.PPrint
import qualified Dao.Binary  as B
import qualified Dao.EnumSet as Es
import qualified Dao.Struct  as St
import           Dao.Random
import           Dao.Glob
import           Dao.Predicate
import           Dao.Procedural

import           Control.Exception
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Error
import           Control.DeepSeq

import           Data.Dynamic
import           Data.Monoid
import           Data.Array.IArray
import           Data.Bits
import           Data.Word
import           Data.Char
import           Data.List
import           Data.Time.Clock
import           Data.Ratio
import           Data.IORef
import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.IntMap as I
import qualified Data.ByteString.Lazy as B
import           Data.Binary (encode)

import           System.IO

-- not for export
lu :: Location
lu  = LocationUnknown
fd :: HasLocation a => a -> a
fd = delLocation
fd1 :: (HasLocation a, Functor f) => f a -> f a
fd1 = fmap delLocation

----------------------------------------------------------------------------------------------------

-- | Object was originally it's own @data@ type, but now it is a synonym for a 'Dao.Object.Value'
-- polymorphic over the 'HaskellData' @data@ type.
type Object = Value HaskellData
type T_list     = [Value Object]
type T_dict     = M.Map Name Object
type T_struct   = Struct Object

instance ObjectClass (Struct Object) where
  objectMethods = defObjectInterface nullValue $ do
    autoDefEquality >> autoDefOrdering >> autoDefPPrinter
    autoDefToStruct >> autoDefFromStruct

instance ToDaoStructClass (Struct Object) Object where { dataToStruct  = return }
instance FromDaoStructClass (Struct Object) Object where { dataFromStruct = return }

instance ObjectClass (StructError Object) where
  objectMethods = defObjectInterface nullValue $ do
    autoDefEquality >> autoDefOrdering
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

-- | This function is a placeholder used by the type system. The value of St.this function is
-- undefined, so strictly evaluating it will throw an exception. Fortunately, the only time you will
-- ever use St.this function is St.with the 'daoClass' function, which uses the type of St.this function but
-- never it's value. Refer to the documentation on 'daoClass' to see how to properly use St.this
-- function.
haskellType :: ObjectClass o => o
haskellType = error $ unwords $
  [ "the Dao.Evaluator.haskellType function is just a placeholder"
  , "used by the type system, it must not be evaluated."
  ]

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

-- | Dao programs can declare "requires" statements along St.with it's imports. If your built-in module
-- provides what Dao programs might "required", then declare that St.this module provides that feature
-- using St.this function.
daoProvides :: UStrType s => s -> DaoSetup
daoProvides label = updateSetupModState $ \st ->
  st{ daoSatisfies = T.insert (refNameList $ read $ uchars label) () (daoSatisfies st) }

-- | Associate an 'ObjectClass' St.with a 'Name'. This 'Name' will be callable from within Dao scripts.
-- > newtype MyClass = MyClass { ... } deriving (Eq, Ord)
-- >
-- > instance 'ObjectClass' MyClass where
-- >     'objectMethods' = 'defObjectInterface' $ do
-- >         'autoDefEquality'
-- >         'autoDefOrdering'
-- >         ...
-- >
-- > setupDao :: 'DaoSetup'
-- > setupDao = do
-- >     daoClass "myClass" (haskellType::MyClass)
-- >     ...
daoClass :: (UStrType name, Typeable o, ObjectClass o) => name -> o -> DaoSetup
daoClass name ~o = updateSetupModState $ \st ->
    st{ daoClasses = insertMethodTable o (fromUStr $ toUStr name) objectMethods (daoClasses st) }

-- | Define a built-in function. Examples of built-in functions provided in St.this module are
-- "print()", "join()", and "exec()".
daoFunction :: (Show name, UStrType name) => name -> DaoFunc -> DaoSetup
daoFunction name func = updateSetupModState $ \st ->
  st{ daoTopLevelFuncs = M.insert (fromUStr $ toUStr name) func (daoTopLevelFuncs st) }

-- | Provide an 'Exec' monad to perform when 'setupDao' is evaluated. You may use St.this function as
-- many times as you wish, every 'Exec' monad will be executed in the order they are specified. This
-- is a good way to create a read-eval-print loop.
daoInitialize :: Exec () -> DaoSetup
daoInitialize f = updateSetupModState $ \st -> st{ daoEntryPoint = daoEntryPoint st >> f }

-- | Use St.this function evaluate a 'DaoSetup' in the IO () monad. Use St.this to define the 'main'
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

data HaskellData = HaskellData Dynamic (ObjectInterface Dynamic) deriving Typeable

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
  nullValue = mkHaskellData ()
  testNull (HaskellData o ifc) = case objNullTest ifc of
    Nothing -> error ("to check whether objects of type "++show (objHaskellType ifc)++" are null is undefined behavior")
    Just fn -> fn o

showObj :: PPrintable a => a -> String
showObj = prettyPrint 80 "    "

----------------------------------------------------------------------------------------------------

-- | This class provides a consistent interface, the 'obj' function, for converting a wide range of
-- types to an 'Object' type.
class ObjectValue o where
  obj   :: o -> Object
  fromObj :: Object -> Maybe o

instance ObjectValue [Object] where
  obj = OList
  fromObj o = case o of { OList o -> Just o; _ -> Nothing; }

instance ObjectValue QualRef where
  obj = ORef
  fromObj o = case o of { ORef o -> Just o; _ -> Nothing; }

instance ObjectValue (Struct Object) where
  obj = OTree
  fromObj o = case o of { OTree o -> Just o; _ -> Nothing; }

instance ObjectValue UStr where
  obj = OString
  fromObj o = case o of { OString o -> Just o; _ -> Nothing; }

instance ObjectValue String where
  obj = obj . toUStr
  fromObj = fromObj >=> maybeFromUStr

instance ObjectValue Int where
  obj = OInt
  fromObj o = case o of { OInt o -> Just o; _ -> Nothing; }

instance ObjectValue Name where
  obj = ORef . Unqualified . Reference . return
  fromObj o = case o of
    ORef (Unqualified (Reference [name])) -> Just name
    _ -> Nothing

instance ObjectValue Reference where
  obj = ORef . Unqualified
  fromObj o = case o of
    ORef (Unqualified ref) -> Just ref
    _ -> Nothing

instance ObjectValue Double where
  obj = OFloat
  fromObj o = case o of { OFloat o -> Just o; _ -> Nothing; }

instance ObjectValue Integer where
  obj = OLong
  fromObj o = case o of { OLong o -> Just o; _ -> Nothing; }

instance ObjectValue Dynamic where
  obj = opaque
  fromObj o = case o of { OHaskell (HaskellData o _) -> Just o; _ -> Nothing; }

instance ObjectValue (Value HaskellData) where { obj = id; fromObj = Just; }

----------------------------------------------------------------------------------------------------

-- $Building_structs
-- Here are all the basic functions for converting between Haskell language data types and Dao
-- language structures.
-- 
-- Most 'FromDaoStruct' functions will backtrack when they fail to get the necessary data. This
-- function can make a backtracking function fail. For example:
-- > 'Dao.Object.tryField' "x" >>= 'objType'
-- backtracks in any case
-- 
-- > required (tryField "x" >>= objType)
-- > tryField "x" >>= required objType
-- These two forms do the same thing: fails if 'objType' backtracks, but not if the field doesn't
-- exist.
-- 
-- > 'Control.Applicative.optional' ('Dao.Object.tryField' "x" >>= 'objType')
-- returns 'Prelude.Nothing' if the field does not exist or if 'objType' backtracks
-- 
-- > 'Dao.Object.field' "x" >>= 'objType'
-- fails if the field does not exist, backtracks if it exists but is the wrong type
-- (you probably don't ever want to do St.this).
-- 
-- > 'required' ('field' "x" >>= 'objType')
-- > 'field' "x" >>= 'required' 'objType'
-- These two forms are the same: fails if either the field does not exist or if 'objType'
-- backtracks.

-- | This function is intended to be used St.with a type checking function 'objType', 'structType',
-- 'primType', and 'dynType' passed as the first parameter, and is intended to receive an object
-- value from 'Dao.Object.field' or 'Dao.Object.tryField' computation. 'require' will pass the
-- object value to the type checking function, and if the type checking function backtracks
-- 'required' will throw an error St.with the correct error information.
required :: (Object -> FromDaoStruct Object o) -> Object -> FromDaoStruct Object o
required f o = mplus (f o) $ throwError $ nullValue{ structErrValue=Just o }

-- | This is an important function for instantiating 'Dao.Object.ToDaoStructClass'. It takes any
-- value instantiating 'Dao.Object.ObjectClass', converts it to an 'Object' using the 'new'
-- function. It is the inverse of 'objType'.
--
-- It is recommended you use St.this function instead of 'defStructField', 'defPrimField', or
-- 'defDynField' whenever it is possible, i.e. whenever the data type you are putting instantiated
-- the 'Dao.Object.ObjectClass' class.
defObjField
  :: (UStrType name, Typeable o, ObjectClass o)
  => name -> o -> ToDaoStruct Object haskData Object
defObjField name o = define name (new o)

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

-- | This is an important function for instantiating 'Dao.Object.FromDaoStructClass'. It is the inverse
-- of 'defObjField'. It retrieves an 'Object' field from a field in a 'Struct' and tries to convert
-- St.this 'Object' to a value of the needed type by extracting it from the 'HaskellData' constructor.
-- Of course the type of the value to which it is converting must instantiate the 'ObjectValue'
-- class. This function backtracks on the incorrect type.
objType :: (Typeable o, ObjectClass o) => Object -> FromDaoStruct Object o
objType o = case o of
  OHaskell (HaskellData o _ifc) -> maybe mzero return $ fromDynamic o
  _                             -> mzero

-- | Synonym for @'required' 'objType'@
(.?) :: (Typeable o, ObjectClass o) => Object -> FromDaoStruct Object o
(.?) = required objType

-- | Synonym for 'objType'
(.??) :: (Typeable o, ObjectClass o) => Object -> FromDaoStruct Object o
(.??) = objType

-- | Like 'defObjField' but places a data type instantiating the 'ToDaoStructClass' class by
-- converting the data type to another 'Dao.Object.Struct' and storing it into a 'OTree'
-- constructor. This is the inverse of 'structType'.
-- 
-- It is recommended that St.this function be used for non-primitive types that do not instantiate the
-- 'ObjectClass' class.
defStructField
  :: (UStrType name, ToDaoStructClass o Object)
  => name -> o -> ToDaoStruct Object haskData Object
defStructField name innerObj = predicate (dataToStruct innerObj) >>= define name . OTree

-- | Synonym for 'defStructField'
(^=)
  :: (UStrType name, ToDaoStructClass o Object)
  => name -> o -> ToDaoStruct Object haskData Object
(^=) = defStructField
infixr 2 ^=

-- | Like 'defStructField' but takes a field accessor to extract the data to be stored from the
-- object being converted. This function is defined as:
-- > \name accessor -> asks accessor >>= defStructField name
putStructField
  :: (UStrType name, ToDaoStructClass o Object)
  => name -> (haskData -> o) -> ToDaoStruct Object haskData Object
putStructField name which = asks which >>= defStructField name

-- | Synonym for 'putStructField'
(^=@)
  :: (UStrType name, ToDaoStructClass o Object)
  => name -> (haskData -> o) -> ToDaoStruct Object haskData Object
(^=@) = putStructField
infixr 2 ^=@

-- | Like 'objType' but converts from the 'Object' value to the needed data type using
-- 'dataFromStruct', where the needed data type instantiates the 'FromDaoStructClass' class using the
-- 'new' function. This function backtracks on the incorrect data type.
structType :: FromDaoStructClass o Object => Object -> FromDaoStruct Object o
structType o = case o of
  OTree struct -> predicate (dataFromStruct struct)
  _            -> mzero

-- | Synonym for @'required' 'structType'@
(^?) :: FromDaoStructClass o Object => Object -> FromDaoStruct Object o
(^?) = required structType

-- | Synonym for 'structType'
(^??) :: FromDaoStructClass o Object => Object -> FromDaoStruct Object o
(^??) = structType

-- | Like 'defObjField' but works on primitive values instantiating 'ObjectValue', values
-- that do not instantiate 'Dao.Object.RuntimeReader'. It is the inverse of 'primType'.
-- 
-- Since this data type is only possible for types which instantiate 'ObjectValue' it only works on
-- primitive data types like 'Dao.String.UStr' and 'Prelude.Int', and it is the only function that
-- can be used with these data types as primitive types should not instantiate 'ObjectClass' or
-- 'Dao.Object.FromDaoStructClass'.
defPrimField :: (UStrType name, ObjectValue o) => name -> o -> ToDaoStruct Object haskData Object
defPrimField name o = define name (obj o)

-- | Synonym for 'defPrimField'
(#=) :: (UStrType name, ObjectValue o) => name -> o -> ToDaoStruct Object haskData Object
(#=) = defPrimField
infixr 2 #=

-- | Like 'defPrimField' but takes a field accessor to extract the data to be stored from the
-- object being converted. This function is defined as:
-- > \name accessor -> asks accessor >>= defPrimField name
putPrimField :: (UStrType name, ObjectValue o) => name -> (haskData -> o) -> ToDaoStruct Object haskData Object
putPrimField name which = asks which >>= defPrimField name

-- | Synonym for 'putPrimField'
(#=@) :: (UStrType name, ObjectValue o) => name -> (haskData -> o) -> ToDaoStruct Object haskData Object
(#=@) = putPrimField
infixr 2 #=@

-- | Like 'objType' but works on primitive values instantiating 'ObjectValue', It is the inverse of
-- 'defPrimField'. This function backtracks on failure.
primType :: ObjectValue o => Object -> FromDaoStruct Object o
primType = maybe mzero return . fromObj

-- | Synonym for @'required' 'primType'@
(#?):: ObjectValue o => Object -> FromDaoStruct Object o
(#?) = required primType

-- | Synonym for 'primType'
(#??) :: ObjectValue o => Object -> FromDaoStruct Object o
(#??) = primType

-- | Like 'defObjField', but uses the data type's instantiation of 'Data.Typeable.Typable' to St.place
-- a completely opaque 'Data.Dynamic.Dynamic' data type, converted to an 'Object' using the 'opaque'
-- function. This function is the inverse of 'dynType'.
--
-- It is strongly recommended St.this function never be used.
defDynField :: (UStrType name, Typeable o) => name -> o -> ToDaoStruct Object haskData Object
defDynField name o = define name (opaque o)

-- | Synonym for 'defDynField'
(*=) :: (UStrType name, Typeable o) => name -> o -> ToDaoStruct Object haskData Object
(*=) = defDynField
infixr 2 *=

-- | Like 'defDynField' but takes a field accessor to extract the data to be stored from the
-- object being converted. This function is defined as:
-- > \name accessor -> asks accessor >>= defDynField name
putDynField :: (UStrType name, Typeable o) => name -> (haskData -> o) -> ToDaoStruct Object haskData Object
putDynField name which = asks which >>= defDynField name

-- | Synonym for 'putDynField'
(*=@) :: (UStrType name, Typeable o) => name -> (haskData -> o) -> ToDaoStruct Object haskData Object
(*=@) = putDynField
infixr 2 *=@

-- | Like 'objType' but converts from object of 'Data.Dynamic.Dynamic' data stored in a
-- 'Dao.Object.OHaskell' constructor. It is the inverse of 'defDynField'.
dynType :: Typeable o => Object -> FromDaoStruct Object o
dynType = maybe mzero return . (fromObj >=> fromDynamic)

-- | Synonym for @'required' 'dynType'@
(*?) :: Typeable o => Object -> FromDaoStruct Object o
(*?) = required dynType

-- | Synonym for 'dynType'
(*??) :: Typeable o => Object -> FromDaoStruct Object o
(*??) = dynType

-- | Tries both 'objType' and 'structType'.
daoType
  :: (Typeable o, ObjectClass o, FromDaoStructClass o Object)
  => Object -> FromDaoStruct Object o
daoType o = mplus (objType o) (structType o)

-- | Synonym for 'daoType'
(??) :: (Typeable o, ObjectClass o, FromDaoStructClass o Object) => Object -> FromDaoStruct Object o
(??) = daoType

-- | Synonym for @'required' 'daoType'@
(?) :: (Typeable o, ObjectClass o, FromDaoStructClass o Object) => Object -> FromDaoStruct Object o
(?) = required daoType

----------------------------------------------------------------------------------------------------

putUStrData :: UStrType str => str -> Update ()
putUStrData s =
  let u = toUStr s in St.place (if ulength u == 1 then OChar (head (uchars u)) else OString u)

getUStrData :: UStrType str => str -> Update UStr
getUStrData msg = do
  a <- St.this
  case a of
    OString a -> return a
    OChar   c -> return (ustr [c])
    _         -> fail ("was expecting a string for constructing a "++uchars msg++" object")

getIntegerData :: Integral a => String -> Update a
getIntegerData msg = do
  a <- St.this
  case a of
    OLong a -> return (fromIntegral a)
    OInt  a -> return (fromIntegral a)
    OWord a -> return (fromIntegral a)
    _ -> fail ("was expecting an integer value for constructing a "++msg++" object")

getBoolData :: String -> String -> String -> Update Bool
getBoolData msg tru fals = do
  a <- St.this
  case a of
    ONull -> return False
    OTrue -> return True
    OString str
      | uchars str == tru  -> return True
      | uchars str == fals -> return False
      | uchars str == "true" -> return True
      | uchars str == "false" -> return True
      | uchars str == "yes" -> return True
      | uchars str == "no" -> return True
    OInt i -> return (i/=0)
    OWord i -> return (i/=0)
    OLong i -> return (i/=0)
    _ -> fail $ concat $
      [ "was expecting a boolean value ("
      , show tru, " or ", fals
      , ") for constructing ", msg, " object"
      ]

instance St.Structured () Object where
  dataToStruct _ = St.deconstruct $ St.place ONull
  structToData = St.reconstruct $ St.this >>= \o -> case o of
    ONull -> return ()
    _     -> fail $ "expecting () as ONull value"

instance St.Structured UStr Object where
  dataToStruct a = St.deconstruct $ St.place (OString a)
  structToData = St.reconstruct $ do
    a <- St.this
    case a of
      OString a -> return a
      _         -> fail "expecing string constant"

instance St.Structured Bool Object where
  dataToStruct a = St.deconstruct $ St.place (if a then OTrue else ONull)
  structToData = St.reconstruct $ getBoolData "strucutred boolean" "true" "false"

instance St.Structured Word Object where
  dataToStruct a = St.deconstruct $ St.place (OInt (fromIntegral a))
  structToData = St.reconstruct (getIntegerData "unsigned integer")

instance St.Structured Int Object where
  dataToStruct a = St.deconstruct $ St.place (OInt (fromIntegral a))
  structToData = St.reconstruct (getIntegerData "integer")

newtype StructChar = StructChar Char
instance St.Structured StructChar Object where
  dataToStruct (StructChar c) = St.deconstruct (St.place (OChar c))
  structToData = St.reconstruct $ St.this >>= \c -> case c of
    OChar c -> return (StructChar c)
    _       -> fail "singleton character"

putListWith :: (a -> Update ()) -> [a] -> Update ()
putListWith _dat2srct _ox = undefined

getListWith :: Update a -> Update [a]
getListWith _srct2dat = do
  o <- St.this
  case o of
    OList ox -> forM ox $ \o -> case o of
      OTree  _ -> undefined
      _        -> fail "was expecting structured data in each list item"
    _        -> fail "was expecting a list object"

instance St.Structured a Object => St.Structured [a] Object where
  dataToStruct ax = St.deconstruct (putListWith St.putData ax)
  structToData    = St.reconstruct (getListWith St.getData)

instance St.Structured (T.Tree Name Object) Object where
  dataToStruct _ = undefined
  structToData = undefined

instance (Ord a, Enum a, St.Structured a Object) => St.Structured (Es.Inf a) Object where
  dataToStruct a = St.deconstruct $ case a of
    Es.PosInf  -> putUStrData "+inf"
    Es.NegInf  -> putUStrData "-inf"
    Es.Point a -> St.putData a
  structToData = St.reconstruct $ msum $
    [ fmap Es.Point St.getData
    , getUStrData msg >>= \a -> case uchars a of
        "+inf" -> return Es.PosInf
        "-inf" -> return Es.NegInf
        _      -> fail msg
    , fail msg
    ]
    where { msg = "unit of a segment of an enum set" }

instance (Ord a, Enum a, Bounded a, St.Structured a Object) => St.Structured (Es.Segment a) Object where
  dataToStruct a = St.deconstruct $
    mplus (maybe mzero return (Es.singular a) >>= St.putDataAt "at") $
      maybe mzero return (Es.plural a) >>= \ (a, b) -> St.putDataAt "to" a >> St.putDataAt "from" b
  structToData = St.reconstruct $ msum $
    [ St.getDataAt "to" >>= \a -> St.getDataAt "from" >>= \b -> return (Es.segment a b)
    , fmap Es.single (St.getDataAt "at")
    , fail "unit segment of an enum set"
    ]

instance (Ord a, Enum a, Bounded a, Es.InfBound a, St.Structured a Object) => St.Structured (Es.Set a) Object where
  dataToStruct a = St.deconstruct (St.putData (Es.toList a))
  structToData = St.reconstruct (fmap Es.fromList St.getData)

instance St.Structured (Glob UStr) Object where
  dataToStruct a = St.deconstruct $ case a of
    Glob       a _   -> St.putData a
  structToData = St.reconstruct $ St.getData >>= \a -> return (Glob a (length a))

instance St.Structured (GlobUnit UStr) Object where
  dataToStruct a = T.Leaf $ obj (toUStr a)
  structToData = St.reconstruct $ do
    let errmsg = "expecting glob-unit item, string expression does not parse to valid glob-unit"
    getUStrData "glob-unit" >>= maybe (fail errmsg) return . maybeFromUStr

allStrings :: [Object] -> Predicate UpdateErr [UStr]
allStrings ox = forM (zip [(0::Integer)..] ox) $ \ (i, o) -> case o of
  OString o -> return o
  OChar   c -> return (ustr [c])
  _         -> fail ("in list, item number "++show i++" must be a string value")

putUStrList :: [UStr] -> Update ()
putUStrList = St.place . OList . map OString 

getUStrList :: Update [UStr]
getUStrList = do
  ls <- St.this
  case ls of
    OList ls -> predicate (allStrings ls)
    _        -> fail "expecting a list of strings of base64-encoded data"

instance St.Structured Name Object where
  dataToStruct = ustrToStruct
  structToData = fmap fromUStr . structToUStr "a valid label"

instance St.Structured UpdateOp Object where
  dataToStruct = ustrToStruct
  structToData = structToUStr "assignment operator"

instance St.Structured RefPfxOp Object where
  dataToStruct = ustrToStruct
  structToData = structToUStr "unary prefix operator"

instance St.Structured ArithPfxOp Object where
  dataToStruct = ustrToStruct
  structToData = structToUStr "unary prefix operator"
  
instance St.Structured InfixOp Object where
  dataToStruct = ustrToStruct
  structToData = structToUStr "binary infix operator"

instance St.Structured CoreType Object where
  dataToStruct = ustrToStruct
  structToData = structToUStr "type identifier"

-- This instance never places data in the immediate ('Data.Struct.St.this') node, so it is safe to call
-- 'St.putData' on a value of type 'Data.Token.Location' even if you have already placed data in the
-- immedate node St.with 'St.putData'.
instance St.Structured Location Object where
  dataToStruct loc = case loc of
    LocationUnknown  -> T.Void
    Location _ _ _ _ -> St.deconstruct $ St.with "location" $ do
      St.with "from" $ do
        St.with "line"   (St.place $ OInt $ fromIntegral $ startingLine   loc)
        St.with "column" (St.place $ OInt $ fromIntegral $ startingColumn loc)
      if   startingLine   loc == endingLine   loc
        && startingColumn loc == endingColumn loc
      then  return ()
      else  St.with "to"   $ do
              St.with "line"   (St.place $ OInt $ fromIntegral $ endingLine     loc)
              St.with "column" (St.place $ OInt $ fromIntegral $ endingColumn   loc)
  structToData = St.reconstruct $ flip mplus (return LocationUnknown) $ St.tryWith "location" $ do
    let getPos = pure (,) <*> St.getDataAt "line" <*> St.getDataAt "column"
    (a,b) <- St.with "from" getPos
    flip mplus (return (Location a b a b)) $ St.tryWith "to" $ do
      (c,d) <- getPos
      return (Location a b c d)

instance St.Structured Object Object where
  dataToStruct = St.deconstruct . St.place
  structToData = St.reconstruct St.this

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
      -- ^ In St.this slot will be stored a read-only @'Data.Map.Lazy.Map' 'Dao.String.UStr'
      -- 'Dao.Object.ObjectInterface'@ object that will allow any method St.with access to St.this
      -- 'GenRuntime' to retrieve a 'Dao.Object.ObjectInterface' by it's name string. Specifically,
      -- St.this will be used by objects stored in the 'Dao.Object.OHaskell' constructor.
    , pathIndex          :: MVar (M.Map UPath ExecUnit)
      -- ^ every file opened, whether it is a data file or a program file, is registered here under
      -- it's file path (file paths map to 'File's).
    , defaultTimeout     :: Maybe Int
      -- ^ the default time-out value to use when evaluating 'execInputString'
    , importGraph        :: MVar (M.Map UPath ExecUnit)
    , currentWithRef     :: WithRefStore
      -- ^ the current document is set by the @St.with@ statement during execution of a Dao script.
    , taskForExecUnits   :: Task
    , currentQuery       :: Maybe UStr
    , currentPattern     :: Maybe (Glob Object)
    , currentCodeBlock   :: StaticStore
      -- ^ when evaluating a 'Subroutine' selected by a string query, the action resulting from
      -- that query is defnied here. It is only 'Data.Maybe.Nothing' when the module is first being
      -- loaded from source code.
    , currentBranch      :: [Name]
      -- ^ set by the @St.with@ statement during execution of a Dao script. It is used to prefix St.this
      -- to all global-dot references before reading from or writing to those references.
    , importsTable       :: [(Name, ExecUnit)]
      -- ^ a pointer to the ExecUnit of every Dao program imported St.with the @import@ keyword.
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
-- within the 'Exec' monad. The 'ExecUnit' produced by St.this function will have it's parent
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

-- | To halt a single thread in a 'Task', simply signal it St.with 'Control.Concurrent.killThread'. But
-- to halt everything the task is doing, use St.this function. Use of St.this function will never result in
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
-- in parallel, and St.this function blocks until all tasks have completed.
taskLoop_ :: Task -> [IO ()] -> IO ()
taskLoop_ task inits = taskLoop task inits (\ _ _ -> return True)

----------------------------------------------------------------------------------------------------

-- | This simple, humble little class is one of the most important in the Dao program because it
-- defines the 'execute' function. Any data type that can result in procedural execution in the
-- 'Exec' monad can instantiate St.this class. This will allow the instnatiated data type to be used as
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
-- In St.this example, should D instantiate () or Int or Char as it's result? You must choose only one.
class Executable exec result | exec -> result where { execute :: exec -> Exec result }

-- 'Dao.Object.execute'-ing a 'Dao.Object.QualRefExpr' will dereference it, essentially reading the
-- value associated St.with that reference from the 'Dao.Object.ExecUnit'.
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

-- | Since the 'ExecUnit' deals St.with a few different kinds of pointer values, namely
-- 'Data.IORef.IORef' and 'MVar', which all have similar functions for reading and updating, I have
-- defined St.this class to provide a consistent set of functions for working St.with the various pointers
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
-- be a particular value of St.this type. The 'Exec' monad instantiates
-- 'Control.Monad.Error.Class.MonadError' such that 'Control.Monad.Error.Class.throwError' throws a
-- value of St.this type. However, it is not only used for exceptions. The Dao scripting language's
-- "return" statement throws an 'ExecReturn' value which is caught using
-- 'Control.Monad.Error.Class.catchError' when evaluating function calls.
data ExecControl
  = ExecReturn { execReturnValue :: Maybe Object }
  | ExecError
    { execReturnValue   :: Maybe Object
    , execUnitAtError   :: Maybe ExecUnit
    , execErrExpr       :: Maybe ObjectExpr
    , execErrScript     :: Maybe ScriptExpr
    , execErrTopLevel   :: Maybe TopLevelExpr
    }
  deriving Typeable

execError :: ExecControl
execError = ExecError Nothing Nothing Nothing Nothing Nothing

instance HasNullValue ExecControl where
  nullValue = ExecReturn Nothing
  testNull (ExecReturn Nothing) = True
  testNull  _                   = False

instance PPrintable ExecControl where
  pPrint err = case err of 
    ExecError{ execReturnValue=o, execUnitAtError=xunit } -> maybe (return ()) pperr o where
      fileName = xunit >>= programModuleName
      apLabel which label =
        fmap (\o -> (pInline [pString label, pString " ", pPrint o], getLocation o)) (which err)
      info = msum
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
          [ case info of
              Nothing -> pString "Error: "
              Just  o -> pString $ concat $
                [maybe "" ((++":") . uchars) fileName , show (snd o) ++ ": "]
          , ppmap o
          ]
        pEndLine
        maybe (return ()) fst info
    ExecReturn{ execReturnValue=o } ->
      maybe (return ()) (\o -> pWrapIndent [pString "Evaluated to: ", pPrint o]) o

instance St.Structured ExecControl Object where
  dataToStruct o = St.deconstruct $ do
    let put str = maybe (return ()) (St.putDataAt str)
    case o of
      ExecReturn o         -> put "return" o
      ExecError  a _ b c d -> put "error" a >> put "objExpr" b >> put "scriptExpr" c >> put "topLevelExpr" d
  structToData = St.reconstruct $ msum $
    [ do  St.guardBranchCount (==4)
          pure ExecError
            <*> optional (St.getDataAt "error")
            <*> return Nothing
            <*> optional (St.getDataAt "objExpr")
            <*> optional (St.getDataAt "scriptExpr")
            <*> optional (St.getDataAt "topLevelExpr")
    , do  St.guardBranchCount (==1)
          ExecReturn <$> optional (St.getDataAt "return")
    , fail "expecting ExecControl"
    ]

instance ObjectClass ExecControl where
  objectMethods = defObjectInterface nullValue $ do
    autoDefPPrinter
    -- autoDefNullTest >> autoDefToStruct

setCtrlReturnValue :: Object -> ExecControl -> ExecControl
setCtrlReturnValue obj ctrl = case ctrl of
  ExecReturn _         -> ExecReturn (Just obj)
  ExecError  _ b c d e ->
    ExecError
    { execReturnValue = Just obj
    , execUnitAtError = b
    , execErrExpr     = c
    , execErrScript   = d
    , execErrTopLevel = e
    }

setCtrlAtError :: ExecUnit -> ExecControl -> ExecControl
setCtrlAtError xunit ctrl = case ctrl of
  ExecReturn a         -> ExecReturn a
  ExecError  a _ c d e ->
    ExecError
    { execReturnValue = a
    , execUnitAtError = Just xunit
    , execErrExpr     = c
    , execErrScript   = d
    , execErrTopLevel = e
    }

setCtrlExpr :: ObjectExpr -> ExecControl -> ExecControl
setCtrlExpr expr ctrl = case ctrl of
  ExecReturn a         -> ExecReturn a
  ExecError  a b _ d e ->
    ExecError
    { execReturnValue = a
    , execUnitAtError = b
    , execErrExpr     = Just expr
    , execErrScript   = d
    , execErrTopLevel = e
    }

setErrScript :: ScriptExpr -> ExecControl -> ExecControl
setErrScript expr ctrl = case ctrl of
  ExecReturn a         -> ExecReturn a
  ExecError  a b c _ e ->
    ExecError
    { execReturnValue = a
    , execUnitAtError = b
    , execErrExpr     = c
    , execErrScript   = Just expr
    , execErrTopLevel = e
    }

setErrTopLevel :: TopLevelExpr -> ExecControl -> ExecControl
setErrTopLevel expr ctrl = case ctrl of
  ExecReturn a         -> ExecReturn a
  ExecError  a b c d _ ->
    ExecError
    { execReturnValue = a
    , execUnitAtError = b
    , execErrExpr     = c
    , execErrScript   = d
    , execErrTopLevel = Just expr
    }

----------------------------------------------------------------------------------------------------

-- | All evaluation of the Dao language takes St.place in the 'Exec' monad. It instantiates
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

class ExecThrowable a where
  toExecError :: a -> ExecControl
  -- | Like 'Prelude.error' but works for the 'Exec' monad, throws an 'ExecControl' using
  -- 'Control.Monad.Error.throwError' constructed using the given 'Object' value as the
  -- 'execReturnValue'.
  execThrow :: ExecThrowable a => a -> Exec ig
  execThrow obj = ask >>= \xunit -> throwError $ (toExecError obj){execUnitAtError=Just xunit}

instance ExecThrowable Object where
  toExecError err = setCtrlReturnValue err execError

instance ExecThrowable (St.GenUpdateErr Object) where
  toExecError err = toExecError $ obj $ concat $
    [ maybe [] ((:[]) . obj) (St.updateErrMsg err)
    , [obj $ Unqualified $ Reference $ St.updateErrAddr err]
    ]

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

-- | Like 'execCatchIO' but St.with the arguments 'Prelude.flip'ped.
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

-- | Keep the current 'execStack', but replace it St.with a new empty stack before executing the given
-- function. This function is different from 'nestedExecStak' in that it acually removes the current
-- execution stack so a function call cannot modify the local variables of the function which called
-- it. Furthermore it catches evaluation of a "return" statement allowing the function which called
-- it to procede St.with execution after St.this call has returned.
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
-- 'Dao.PPrint.PPrintable', 'Dao.Struct.St.Structured', or any parsers. Elements of the abstract syntax
-- tree (AST) instantiate 'Dao.PPrint.PPrintable', 'Dao.Struct.St.Structured', and all of the parsers,
-- but are not executable and do not instantiate 'Executable'. This separates concerns pretty well,
-- but leaves us St.with the problem of having to convert back and forth between these various data
-- types.
--
-- The 'Intermediate' class allows us to declare a one-to-one relationship between AST types and
-- executable types. For example, 'Dao.Object.ObjectExpr' is the intermediate representation of
-- 'AST_Object', so our instance for St.this relationship is @instane 'Intermediate'
-- 'Dao.Object.ObjectExpr' 'AST_Object'@.
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

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is 'Dao.Struct.St.Structured'.
putIntermediate :: (Intermediate obj ast, St.Structured ast Object) => String -> obj -> T.Tree Name Object
putIntermediate typ a = St.deconstruct $ case fromInterm a of
  []  -> return ()
  [a] -> St.putTree (St.dataToStruct a)
  _   -> error ("fromInterm returned more than one possible intermediate data structure for "++typ)

-- | If there is a type that instantiates 'Intermediate', it can be converted to and from a type
-- that is 'Dao.Struct.St.Structured'.
getIntermediate :: (Intermediate obj ast, St.Structured ast Object) => String -> T.Tree Name Object -> Predicate UpdateErr obj
getIntermediate typ = St.reconstruct $ do
  a <- St.getData
  case toInterm a of
    []  -> fail ("could not convert "++typ++" expression to it's intermediate")
    [a] -> return a
    _   -> error ("toInterm returned more than one possible abstract syntax tree for "++typ)

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

instance St.Structured Comment Object where
  dataToStruct a = St.deconstruct $ case a of
    InlineComment  a -> St.putDataAt "inline"  (obj a)
    EndlineComment a -> St.putDataAt "endline" (obj a)
  structToData = St.reconstruct $ msum $
    [ fmap InlineComment  (St.getDataAt "inline")
    , fmap EndlineComment (St.getDataAt "endline")
    , fail "must be a comment string or typed comment string"
    ]

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

putComments :: [Comment] -> Update ()
putComments coms = if null coms then return () else St.putDataAt "comments" coms

getComments :: Update [Comment]
getComments = mplus (St.tryGetDataAt "comments") (return [])

----------------------------------------------------------------------------------------------------

-- | Symbols in the Dao syntax tree that can actually be manipulated can be surrounded by comments.
-- The 'Com' structure represents a space-efficient means to surround each syntactic element St.with
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

instance St.Structured a Object => St.Structured (Com a) Object where
  dataToStruct a = St.deconstruct $ case a of
    Com         c   -> St.putData c
    ComBefore b c   -> St.putData c >> com "before" b
    ComAfter    c a -> St.putData c >> com "after"  a
    ComAround b c a -> St.putData c >> com "before" b >> com "after" a
    where { com msg = St.putDataAt ("comments"++msg) }
  structToData = St.reconstruct $ do
    c <- St.getData
    befor <- mplus (St.getDataAt "commentsBefore") (return [])
    after <- mplus (St.getDataAt "commentsAfter")  (return [])
    return $ case (befor, after) of
      ([], []) -> Com c
      (ax, []) -> ComBefore ax c
      ([], bx) -> ComAfter     c bx
      (ax, bx) -> ComAround ax c bx

instance HasRandGen a => HasRandGen (Com a) where { randO = randComWith randO }

ustrToStruct :: UStrType a => a -> T.Tree Name Object
ustrToStruct = St.deconstruct . St.place . obj . toUStr

structToUStr :: UStrType a => String -> T.Tree Name Object -> Predicate UpdateErr a
structToUStr msg = St.reconstruct $ do
  a <- getUStrData msg
  case maybeFromUStr a of
    Just  a -> return a
    Nothing -> fail ("was expecting "++msg)

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
-- 'Dao.Object.ScriptExpr' in the 'Dao.Object.CodeBlock', one after the other. Execution does not
-- occur within a 'execNested' because many other expressions which execute 'Dao.Object.CodeBlock's,
-- especially 'Dao.Object.TryCatch' expressions and 'Dao.Object.ForLoop's need to be able to choose
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

instance ObjectClass CodeBlock where
  objectMethods = defObjectInterface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefBinaryFmt >> autoDefPPrinter
    defDeref $ \o -> catchError (execute o >> return Nothing) $ \e -> case e of
      ExecReturn o -> return o
      ExecError{}  -> throwError e
    -- TODO: define autoDefIterator, defIndexer, autoDefToStruct >> autoDefFromStruct

----------------------------------------------------------------------------------------------------

-- | A subroutine is contains a 'CodeBlock' and an 'Data.IORef.IORef' to it's own static data. It
-- also has a reference to the last evaluation of 'execute' over it's 'CodeBlock', which provides a
-- hint to the Haskell runtime system that St.this code can be cached rather than evaluating the
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

-- | Although 'Subroutine' instantiates 'Executable', St.this function allows you to easily St.place a
-- group of defined local variables onto the call stack before and the have the 'Subroutine'
-- executed.
runCodeBlock :: T_dict -> Subroutine -> Exec (Maybe Object)
runCodeBlock initStack exe = local (\xunit -> xunit{currentCodeBlock = StaticStore (Just exe)}) $!
  execFuncPushStack initStack (executable exe >>= liftIO . evaluate)

----------------------------------------------------------------------------------------------------

-- | A subroutine is specifically a callable function (but we don't use the name Function to avoid
-- confusion St.with Haskell's "Data.Function"). 
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

instance ObjectClass CallableCode where
  objectMethods = defObjectInterface (CallableCode undefined undefined undefined) $ do
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

-- 'pPrintComWith' wasn't good enough for St.this, because the comments might occur after the header
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

instance St.Structured AST_CodeBlock Object where
  dataToStruct a = St.deconstruct (St.putDataAt "block" (getAST_CodeBlock a))
  structToData = St.reconstruct (fmap AST_CodeBlock (St.getDataAt "block"))

instance HasRandGen AST_CodeBlock where { randO = countNode $ fmap AST_CodeBlock (randList 0 30) }

instance Intermediate CodeBlock AST_CodeBlock where
  toInterm   (AST_CodeBlock ast) = return $ CodeBlock     (ast >>= toInterm  )
  fromInterm (CodeBlock     obj) = return $ AST_CodeBlock (obj >>= fromInterm)

----------------------------------------------------------------------------------------------------

-- | Functions and function parameters can specify optional type-checking expressions. This is a
-- data type that wraps a dao-typeable expression St.with type information.
data TyChkExpr a
  = NotTypeChecked{tyChkItem::a}
    -- ^ no type information was specified for St.this item
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

----------------------------------------------------------------------------------------------------

-- | This node can be found in a few different syntactic structures. When a name or function or
-- expression is followed by a colon and some type checking information, St.this node is used for that
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

instance St.Structured a Object => St.Structured (AST_TyChk a) Object where
  dataToStruct o = St.deconstruct $ case o of
    AST_NotChecked o              -> St.putData o
    AST_Checked    o coms obj loc -> St.putData o >> St.putDataAt "colon" coms >> St.putDataAt "typeExpr" obj >> St.putData loc
  structToData   = St.reconstruct $ msum $
    [ St.getDataAt "colon" >>= \coms -> St.getDataAt "typeExpr" >>= \obj -> St.getData >>= \o -> fmap (AST_Checked o coms obj) St.getData
    , fmap AST_NotChecked St.getData
    ]

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

instance St.Structured AST_Param  Object where
  dataToStruct o = St.deconstruct $ case o of
    AST_NoParams          -> St.place ONull
    AST_Param coms nm loc -> do
      St.with "passByRef" (maybe (St.place ONull) (putComments >=> \ () -> St.place OTrue) coms)
      St.putData nm >> St.putData loc
  structToData = St.reconstruct $ msum $
    [ let getfn = St.this >>= \o -> case o of
            ONull -> return Nothing
            OTrue -> fmap Just getComments
            _     -> fail "expecting boolean St.with optional comments"
      in  pure AST_Param <*> St.with "passByRef" getfn <*> St.getData <*> St.getData
    , St.this >>= \o -> case o of
        ONull -> return AST_NoParams
        _     -> fail "expecting function parameter declaration"
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

instance St.Structured AST_ParamList  Object where
  dataToStruct (AST_ParamList lst loc) = St.deconstruct $ St.putData lst >> St.putData loc
  structToData = St.reconstruct $ liftM2 AST_ParamList St.getData St.getData

instance HasRandGen AST_ParamList where { randO = countNode $ pure AST_ParamList <*> randO <*> no }

instance Intermediate ParamListExpr AST_ParamList where
  toInterm   (AST_ParamList ox loc) = liftM2 ParamListExpr (tyChkToInterm ox) [loc]
  fromInterm (ParamListExpr ox loc) = liftM2 AST_ParamList (tyChkFromInterm ox) [loc]

----------------------------------------------------------------------------------------------------

-- | Convert an 'Dao.Object.ObjectExpr' to an 'Dao.Glob.Glob'.
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

instance St.Structured AST_StringList Object where
  dataToStruct o = St.deconstruct $ case o of
    AST_NoStrings  coms loc -> St.place ONull >> St.putData coms >> St.putData loc
    AST_StringList strs loc -> St.putData strs >> St.putData loc
  structToData   = St.reconstruct $ St.this >>= \o -> case o of
    ONull -> pure AST_NoStrings  <*> St.getData <*> St.getData
    _     -> pure AST_StringList <*> St.getData <*> St.getData

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

----------------------------------------------------------------------------------------------------

-- $Exec_helpers
-- Functions for working St.with object values when building built-in functions callable from within a
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

-- | Traverse the entire object, returning a list of all 'Dao.Object.OString' elements.
extractStringElems :: Object -> [UStr]
extractStringElems o = case o of
  OString  o   -> [o]
  OList    o   -> concatMap extractStringElems o
  _            -> []

----------------------------------------------------------------------------------------------------

data UpdateOp
  = UCONST | UADD | USUB | UMULT | UDIV | UMOD | UPOW | UORB | UANDB | UXORB | USHL | USHR | UARROW
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show)
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

----------------------------------------------------------------------------------------------------

data RefPfxOp = REF | DEREF deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show)

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

----------------------------------------------------------------------------------------------------

-- | Unary operators.
data ArithPfxOp = INVB | NOT | NEGTIV | POSTIV deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show)

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
  deriving (Eq, Ord, Typeable, Enum, Ix, Bounded, Show)

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

-- The byte prefixes overlap St.with the update operators of similar function to
-- the operators, except for the comparison opeators (EQUL, NEQUL, GTN, LTN,
-- GTEQ, LTEQ) which overlap St.with the prefix operators (INVB, NOT, NEGTIV, POSTIV, REF, DEREF)
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

instance St.Structured TopLevelEventType  Object where
  dataToStruct a = St.deconstruct $ St.place $ obj $ case a of
    BeginExprType -> "BEGIN"
    EndExprType   -> "END"
    ExitExprType  -> "EXIT"
  structToData = St.reconstruct $ getUStrData "event type" >>= \a -> case uchars a of
    "BEGIN" -> return BeginExprType
    "END"   -> return EndExprType
    "EXIT"  -> return ExitExprType
    "QUIT"  -> return ExitExprType
    _       -> fail "top-level event type"

instance HasRandGen TopLevelEventType where
  randO = fmap toEnum (nextInt 3)

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
        execError
        { execReturnValue = Just $ obj [obj "requires string parameter, param number:", obj i] }

-- | Given an object, if it is a string return the string characters. If it not a string,
-- depth-recurse into it and extract strings, or if there is an object into which recursion is not
-- possible, pretty-print the object and return the pretty-printed string. The first integer
-- parameter is a depth limit, if recursion into the object exceeds St.this limit, recursion no longer
-- steps into these objects, the strings returned are the pretty-printed representation of the
-- objects. A pair of 'Data.Either.Either's are returned, references are 'Data.Either.Left',
-- 'Prelude.String's are 'Data.Either.Right'. References are accompanied St.with their depth so you can
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
-- St.this depth limit is specified by the first argument to St.this function. The second and third
-- argument to St.this function are passed directly to 'getStringsToDepth'. Pass a handler to handle
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
-- language, are stored in 'Data.Map.Map's from the functions name to an object of St.this type.
-- Functions of St.this type are called by 'evalObject' to evaluate expressions written in the Dao
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

-- | Evaluate St.this function as one of the instructions in the monadic function passed to the
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

-- | To evaluate an 'Dao.Object.Object' value against a type expression, you can store the
-- 'Dao.Object.Object' into a 'Dao.Object.TyChkExpr' and 'Dao.Object.execute' it. This instance of
-- 'Dao.Object.execute' evaluates a type checking monad computing over the 'Dao.Object.tyChkExpr' in
-- the 'Dao.Object.TyChkExpr'. If the type check determines the 'Object' value does not match, this
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
-- routine using 'defCallable' in it's 'objectMethods' table.
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

data AST_Ref = AST_RefNull | AST_Ref  Name [Com Name] Location deriving (Eq, Ord, Typeable, Show)

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

instance St.Structured AST_Ref Object where
  dataToStruct a = St.deconstruct $ case a of
    AST_RefNull          -> St.place ONull
    AST_Ref r comref loc -> St.putData r >>
      St.with "refExpr" (putListWith St.putData comref >> St.putData loc)
  structToData = St.reconstruct $ St.this >>= \o -> case o of
    ONull -> return AST_RefNull
    _     -> do
      ref <- St.getData
      loc <- St.getData
      multi <- St.with "refExpr" $ mplus (fmap Just (getListWith St.getData)) (return Nothing)
      case multi of
        Nothing    -> return (AST_Ref ref []    loc)
        Just multi -> return (AST_Ref ref multi loc)

instance Intermediate RefExpr AST_Ref where
  toInterm   ast = case ast of
    AST_RefNull             -> [RefExpr (Reference []) LocationUnknown]
    AST_Ref  n nx  loc -> [RefExpr (Reference (n : fmap unComment nx)) loc]
  fromInterm (RefExpr (Reference nx) _) = case nx of
    []   -> [AST_RefNull]
    n:nx -> [AST_Ref n (fmap Com nx) LocationUnknown]

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

instance St.Structured AST_QualRef Object where
  dataToStruct ref = St.deconstruct $ case ref of
    AST_Qualified q com ref loc -> do
      let qref addr = St.with addr $ putComments com >> St.putData ref >> St.putData loc
      qref $ case q of
        LOCAL  -> "local"
        GLODOT -> "globalDot"
        STATIC -> "static"
        GLOBAL -> "global"
    AST_Unqualified ref -> St.with "unqualified" $ St.putData ref
  structToData = St.reconstruct $ msum $ map qref tries ++
    [St.with "unqualfied" $ pure AST_Unqualified <*> St.getData] where
      tries = zip (words "local qtime globalDot static global") [LOCAL, GLODOT, STATIC, GLOBAL]
      qref (addr, q) = St.with addr $ pure (AST_Qualified q) <*> getComments <*> St.getData <*> St.getData

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
-- to "bottom". The error message is any value of type 'Dao.Object.Object'. These functions provide
-- a simplified method for constructing error 'Dao.Object.Object's.

-- | Convert a 'Dao.Token.Location' to an 'Dao.Object.Object' value.
errAt :: Location -> [Object]
errAt loc = case loc of
  LocationUnknown -> []
  loc -> [ OInt (fromIntegral (startingLine loc)), OInt (fromIntegral (startingColumn loc))
         , OInt (fromIntegral (endingLine   loc)), OInt (fromIntegral (endingColumn   loc))
         ]

-- | Evaluate to 'procErr' if the given 'Predicate' is 'Backtrack' or 'PFail'. You must pass a
-- 'Prelude.String' as the message to be used when the given 'Predicate' is 'Backtrack'. You can also
-- pass a list of 'Dao.Object.Object's that you are checking, these objects will be included in the
-- 'procErr' value.
--     This function should be used for cases when you have converted 'Dao.Object.Object' to a
-- Haskell value, because 'Backtrack' values indicate type exceptions, and 'PFail' values indicate a
-- value error (e.g. out of bounds, or some kind of assert exception), and the messages passed to
-- 'procErr' will indicate St.this.
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

-- | 'evalObjectExprExpr' can return 'Data.Maybe.Nothing', and usually St.this happens when something has
-- failed (e.g. reference lookups), but it is not always an error (e.g. a void list of argument to
-- functions). If you want 'Data.Maybe.Nothing' to cause an error, evaluate your
-- @'Dao.Object.Exec' ('Data.Maybe.Maybe' 'Dao.Object.Object')@ as a parameter to St.this function.
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
  prefixTable = B.mkPrefixTableWord8 "Dao.Object.ParenExpr" 0x27 0x27 $
    [pure ParenExpr <*> B.get <*> B.get]

instance Executable ParenExpr (Maybe Object) where { execute (ParenExpr a _) = execute a }

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

instance St.Structured AST_Paren  Object where
  dataToStruct (AST_Paren a loc) = St.deconstruct $ St.with "paren" $ St.putData a >> St.putData loc
  structToData = St.reconstruct $ St.with "paren" $ pure AST_Paren <*> St.getData <*> St.getData

instance HasRandGen AST_Paren  where { randO = recurse nullValue $ pure AST_Paren <*> randO <*> no }

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

----------------------------------------------------------------------------------------------------

data AST_If = AST_If (Com AST_Paren) AST_CodeBlock Location deriving (Eq, Ord, Typeable, Show)

instance NFData AST_If where { rnf (AST_If     a b c) = deepseq a $! deepseq b $! deepseq c () }

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

instance St.Structured AST_If Object where
  dataToStruct (AST_If ifn thn loc) = St.deconstruct $
    St.with "ifExpr" $ St.putData ifn >> St.putDataAt "then" thn >> St.putData loc
  structToData = St.reconstruct $ St.with "ifExpr" $ liftM3 AST_If St.getData (St.getDataAt "then") St.getData

instance HasRandGen AST_If where { randO = countNode $ pure AST_If <*> randO <*> randO <*> no }

instance Intermediate IfExpr AST_If where
  toInterm   (AST_If a b loc) = liftM3 IfExpr (uc0 a) (ti  b) [loc]
  fromInterm (IfExpr a b loc) = liftM3 AST_If (nc0 a) (fi  b) [loc]

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

----------------------------------------------------------------------------------------------------

data AST_Else = AST_Else (Com ()) AST_If Location deriving (Eq, Ord, Typeable, Show)
  -- ^ @/**/ else /**/ if /**/ obj /**/ {}@

instance NFData AST_Else where { rnf (AST_Else   a b c) = deepseq a $! deepseq b $! deepseq c () }

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

instance St.Structured AST_Else  Object where
  dataToStruct (AST_Else coms ifn loc) = St.deconstruct $
    St.with "elseIfExpr" $ St.putData coms >> St.putData ifn >> St.putData loc
  structToData = St.reconstruct $ St.with "elseIfExpr" $ liftM3 AST_Else St.getData St.getData St.getData

instance HasRandGen AST_Else where { randO = countNode $ pure AST_Else <*> randO <*> randO <*> no }

instance Intermediate ElseExpr AST_Else where
  toInterm   (AST_Else _ a loc) = liftM2 ElseExpr          (ti  a) [loc]
  fromInterm (ElseExpr   a loc) = liftM3 AST_Else [Com ()] (fi  a) [loc]

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
  delLocation (IfElseExpr a b c _  )     = IfElseExpr (delLocation a) (fmap delLocation b) (fmap delLocation c) LocationUnknown

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

instance St.Structured AST_IfElse  Object where
  dataToStruct (AST_IfElse ifn els coms deflt loc) = St.deconstruct $ St.with "ifExpr" $
    St.putData ifn >> putListWith St.putData els >> St.putData coms >> maybe (return ()) (St.putDataAt "elseExpr") deflt >> St.putData loc
  structToData = St.reconstruct $ St.with "ifExpr" $
    liftM5 AST_IfElse St.getData (getListWith St.getData) St.getData (optional (St.getDataAt "elseExpr")) St.getData

instance HasRandGen AST_IfElse where { randO = countNode $ pure AST_IfElse <*> randO <*> randList 0 4 <*> randO <*> randO <*> no }

instance Intermediate IfElseExpr AST_IfElse where
  toInterm   (AST_IfElse a b _ c loc) = liftM4 IfElseExpr (ti  a) [b>>=ti]          (um1 c) [loc]
  fromInterm (IfElseExpr a b   c loc) = liftM5 AST_IfElse (fi  a) [b>>=fi] [Com ()] (nm1 c) [loc]

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

instance St.Structured AST_While  Object where
  dataToStruct (AST_While (AST_If ifn thn loc)) = St.deconstruct $ St.with "whileExpr" $
    St.putData ifn >> St.putDataAt "script" thn >> St.putData loc
  structToData = St.reconstruct $ St.with "whileExpr" $
    liftM3 (\a b c -> AST_While (AST_If a b c)) St.getData (St.getDataAt "script") St.getData

instance HasRandGen AST_While  where { randO = AST_While <$> randO }

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

instance St.Structured ScriptExpr  Object where
  dataToStruct = putIntermediate script_intrm
  structToData = getIntermediate script_intrm

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

-- | Convert a single 'ScriptExpr' into a function of value @'Exec' 'Dao.Object.Object'@.
instance Executable ScriptExpr () where
  execute script = updateExecError (\err->err{execErrScript=Just script}) $ case script of
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

----------------------------------------------------------------------------------------------------

-- | This data type instantates the 'execute' function for use in for-loop expressions.
data ForLoopBlock = ForLoopBlock Name Object CodeBlock

-- | Like evaluating 'execute' on a value of 'Dao.Object.QualRefExpr', except the you are evaluating an
-- 'Dao.Object.Object' type. If the value of the 'Dao.Object.Object' is not constructed St.with
-- 'Dao.Object.ORef', the object value is returned unmodified.
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
    -- ^ @St.with /**/ objExpr /**/ {}@
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

instance St.Structured AST_Script Object where
  dataToStruct a = St.deconstruct $ case a of
    AST_Comment      a         -> putComments a
    AST_EvalObject   a b   loc -> St.with "equation"  $  St.putData a >> putComments           b                                      >> St.putData loc
    AST_TryCatch     a b c loc -> St.with "tryExpr"   $  St.putData a >> St.putMaybeAt "varName"  b >> St.putMaybeAt "catchBlock" c         >> St.putData loc
    AST_ForLoop      a b c loc -> St.with "forExpr"   $  St.putDataAt "varName"   a >> St.putDataAt "iterator" b >> St.putDataAt "script" c >> St.putData loc
    AST_ContinueExpr a b c loc -> St.with (if a then "continueExpr" else "breakExpr") $ putComments b      >> St.putData c            >> St.putData loc
    AST_ReturnExpr   a b   loc -> St.with (if a then "returnExpr"   else "throwExpr") $ St.putData     b                              >> St.putData loc
    AST_WithDoc      a b   loc -> St.with "withExpr"  $  St.putDataAt  "reference" a >> St.putDataAt "script"   b                        >> St.putData loc
    AST_IfThenElse   a         -> St.putData a
    AST_WhileLoop    a         -> St.putData a
    AST_RuleFunc     a         -> St.putData a
  structToData = St.reconstruct $ msum $
    [ fmap AST_Comment getComments
    , St.tryWith "equation"    $ pure AST_EvalObject <*> St.getDataAt "equation"  <*> getComments            <*> St.getData
    , St.tryWith "tryExpr"     $ pure AST_TryCatch   <*> St.getDataAt "script"    <*> St.getMaybeAt "varName"   <*> St.getMaybe           <*> St.getData
    , St.tryWith "forExpr"     $ pure AST_ForLoop    <*> St.getDataAt "varName"   <*> St.getDataAt  "iterator"  <*> St.getDataAt "script" <*> St.getData
    , St.tryWith "continueExpr"$ getContinue True
    , St.tryWith "breakExpr"   $ getContinue False
    , St.tryWith "returnExpr"  $ getReturn   True
    , St.tryWith "throwExpr"   $ getReturn   False
    , St.tryWith "withExpr"    $ pure AST_WithDoc    <*> St.getDataAt "reference" <*> St.getDataAt "script"     <*> St.getData
    , St.guardBranch "ifExpr"     >> liftM AST_IfThenElse St.getData
    , St.guardBranch "whileExpr"  >> liftM AST_WhileLoop  St.getData
    , St.guardBranch "ruleOrFunc" >> liftM AST_RuleFunc   St.getData
    , fail "script expression"
    ]
    where
      getContinue tf = pure (AST_ContinueExpr tf) <*> getComments        <*> St.getDataAt "condition"     <*> St.getData
      getReturn   tf = pure (AST_ReturnExpr   tf) <*> St.getDataAt "object" <*> St.getData

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

instance PPrintable ScriptExpr  where { pPrint = pPrintInterm }

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

instance ObjectClass ObjListExpr where
  objectMethods = defObjectInterface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefOrdering >> autoDefPPrinter

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

instance St.Structured AST_ObjList  Object where
  dataToStruct (AST_ObjList coms lst loc) = St.deconstruct $ St.with "objList" (putComments coms >> St.putData lst >> St.putData loc)
  structToData = St.reconstruct $ St.with "objList" (pure AST_ObjList <*> getComments <*> St.getData <*> St.getData)

instance HasRandGen AST_ObjList where
  randO = recurse nullValue $ AST_ObjList <$> randO <*> randListOf 0 5 (randComWith randO) <*> no

instance Intermediate ObjListExpr AST_ObjList where
  toInterm   (AST_ObjList _ lst loc) = liftM2 ObjListExpr      [lst>>=uc0] [loc]
  fromInterm (ObjListExpr   lst loc) = liftM3 AST_ObjList [[]] [lst>>=nc0] [loc]

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

-- | Evaluate an 'Exec', but if it throws an exception, set record an 'Dao.Object.ObjectExpr' where
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

--instance St.Structured (Maybe AST_ObjList) Object where
--  St.dataToStruct a = St.deconstruct $ case a of
--    Nothing -> St.place ONull
--    Just  a -> St.putData a
--  St.structToData   = St.reconstruct $ mplus St.getData $ St.this >>= \a -> case a of
--    ONull -> return Nothing
--    _     -> fail "expecting either null value or optional AST_ObjList expression"

instance St.Structured AST_OptObjList Object where
  dataToStruct (AST_OptObjList c o) = St.deconstruct $
    St.with "initParams" $ maybe (return ()) St.putData o >> putComments c
  structToData   = St.reconstruct $ msum $
    [ St.with "initParams" $ pure AST_OptObjList <*> getComments <*> optional St.getData
    , fail "expecting initParams"
    ]

instance HasRandGen AST_OptObjList where
  randO = countRunRandChoice
  randChoice = randChoiceList $
    [ pure AST_OptObjList <*> randO <*> pure Nothing
    , pure AST_OptObjList <*> randO <*> (Just <$> randO)
    ]

instance Intermediate OptObjListExpr AST_OptObjList where
  toInterm   (AST_OptObjList _ o) = liftM OptObjListExpr (um1 o)
  fromInterm (OptObjListExpr   o) = liftM (AST_OptObjList []) (nm1 o)

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

instance St.Structured AST_Literal Object where
  dataToStruct a = St.deconstruct $ case a of
    AST_Literal a loc -> St.with "literal"   $ St.place a >> St.putData loc
  structToData   = St.reconstruct $ msum $
    [ St.tryWith "literal" $ pure AST_Literal <*> St.this <*> St.getData
    , fail "singleton expression"
    ]

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

instance St.Structured AST_RefOperand Object where
  dataToStruct o = St.deconstruct $ case o of
    AST_ObjParen   a     -> St.putData a
    AST_PlainRef   a     -> St.putData a
    AST_ArraySub a b loc -> St.with "subscript" $ St.putDataAt "header" a >> St.putDataAt "params" b >> St.putData loc
    AST_FuncCall a b loc -> St.with "funcCall"  $ St.putDataAt "header" a >> St.putDataAt "params" b >> St.putData loc
  structToData = St.reconstruct $ msum $
    [ St.tryWith "subscript" $ pure AST_ArraySub  <*> St.getDataAt "header" <*> St.getData            <*> St.getData
    , St.tryWith "funcCall"  $ pure AST_FuncCall  <*> St.getDataAt "header" <*> St.getDataAt "params" <*> St.getData
    , AST_PlainRef <$> St.getData
    , AST_ObjParen <$> St.getData
    , fail "AST_RefOperand expression"
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

----------------------------------------------------------------------------------------------------

data SingleExpr
  = SingleExpr RefOpExpr
  | RefPfxExpr RefPfxOp RefOpExpr Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData SingleExpr where
  rnf (SingleExpr a    ) = deepseq a ()
  rnf (RefPfxExpr a b c) = deepseq a $! deepseq b $! deepseq c ()

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

instance St.Structured SingleExpr Object where
  dataToStruct = putIntermediate "LValue from intermediate"
  structToData = getIntermediate "LValue from intermediate"

----------------------------------------------------------------------------------------------------

data AST_Single
  = AST_Single !AST_RefOperand
  | AST_RefPfx RefPfxOp [Comment] AST_RefOperand Location
  deriving (Eq, Ord, Typeable, Show)

instance NFData AST_Single where
  rnf (AST_Single a      ) = deepseq a ()
  rnf (AST_RefPfx a b c d) = deepseq a $! deepseq b $! deepseq c $! deepseq d  ()

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

instance St.Structured AST_Single Object where
  dataToStruct o = St.deconstruct $ case o of
    AST_Single a         -> St.putData a
    AST_RefPfx a b c loc -> St.with "refPrefix" $ St.putDataAt "op" a >> putComments b >> St.putDataAt "right"  c >> St.putData loc
  structToData = St.reconstruct $ msum $
    [ St.tryWith "refPrefix" $ pure AST_RefPfx <*> St.getDataAt "op" <*> getComments <*> St.getDataAt "right" <*> St.getData
    , AST_Single <$> St.getData
    , fail "L-Value expression"
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

instance St.Structured RuleFuncExpr Object where
  dataToStruct = putIntermediate "object intermedaite node"
  structToData = getIntermediate "object intermedaite node"

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
            -- TODO: tokenize the pattern Single's St.with the 'programTokenizer'
            return $ parseOverSingles pat (fmap (OString . ustr) . simpleTokenizer)
          _           -> execThrow $ obj [obj "cannot parse pattern expression:", obj param]
      return $ Just $ new $ GlobAction globs exec
    --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

instance ObjectClass RuleFuncExpr where
  objectMethods = defObjectInterface nullValue $
    autoDefNullTest >> autoDefEquality >> autoDefBinaryFmt >> autoDefPPrinter
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

instance St.Structured AST_RuleFunc Object where
  dataToStruct a = St.deconstruct $ case a of
    AST_Lambda a b     loc -> St.with "lambdaExpr"$                  St.putDataAt "params" a >> St.putDataAt "script" b >> St.putData loc
    AST_Func   a b c d loc -> St.with "funcExpr"  $ putComments a >> St.putDataAt "name"   b >> St.putDataAt "params" c >> St.putDataAt "script" d >> St.putData loc
    AST_Rule   a b     loc -> St.with "ruleExpr"  $                  St.putDataAt "params" a >> St.putDataAt "script" b >> St.putData loc
  structToData =   St.reconstruct $ msum $
    [ St.tryWith "lambdaExpr"$ pure AST_Lambda                             <*> St.getDataAt "params" <*> St.getDataAt "script" <*> St.getData
    , St.tryWith "funcExpr"  $ pure AST_Func <*> getComments <*> St.getDataAt "name"   <*> St.getDataAt "params" <*> St.getDataAt "script" <*> St.getData
    , St.tryWith "ruleExpr"  $ pure AST_Rule                 <*> St.getDataAt "params" <*> St.getDataAt "script" <*> St.getData
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

instance St.Structured ObjectExpr Object where
  dataToStruct = putIntermediate "object intermedaite node"
  structToData = getIntermediate "object intermedaite node"

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
  execute o = (updateExecError (\err->err{execErrExpr=Just o}) :: Exec (Maybe Object) -> Exec (Maybe Object)) $ case o of
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

instance ObjectClass ObjectExpr where
  objectMethods = defObjectInterface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefBinaryFmt
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

instance St.Structured AST_Object Object where
  dataToStruct a = St.deconstruct $ case a of
    AST_Void                    -> St.place ONull
    AST_ObjLiteral  a           -> St.putData a
    AST_ObjSingle   a           -> St.putData a
    AST_ObjRuleFunc a           -> St.putData a
    AST_ArithPfx    a b c   loc -> St.with "arithPfx"  $ St.putDataAt "op"     a >> putComments        b >> St.putDataAt "to"     c >> St.putData loc
    AST_Init        a b c   loc -> St.with "initExpr"  $ St.putDataAt "header" a >> St.putDataAt "params" b >> St.putDataAt "elems"  c >> St.putData loc
    AST_Struct      _ _     _   -> undefined
    AST_MetaEval    a       loc -> St.with "metaEval"  $ St.putDataAt "inner"  a                                                 >> St.putData loc
  structToData =   St.reconstruct $ msum $
    [ St.tryWith "arithPfx"  $ pure AST_ArithPfx <*> St.getDataAt "op"     <*> getComments        <*> St.getDataAt "to"     <*> St.getData
    , St.tryWith "initExpr"  $ pure AST_Init     <*> St.getDataAt "header" <*> St.getDataAt "params" <*> St.getDataAt "elems"  <*> St.getData
    , St.tryWith "metaEval"  $ pure AST_MetaEval <*> St.getDataAt "inner"  <*> St.getData
    , AST_ObjLiteral  <$> St.getData
    , AST_ObjSingle   <$> St.getData
    , AST_ObjRuleFunc <$> St.getData
    , St.this >>= \o -> case o of
        ONull -> return AST_Void
        _     -> fail "object expression"
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

instance St.Structured ArithExpr Object where
  dataToStruct = putIntermediate "object intermedaite node"
  structToData = getIntermediate "object intermedaite node"

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

instance ObjectClass ArithExpr where
  objectMethods = defObjectInterface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefBinaryFmt
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

instance St.Structured AST_Arith Object where
  dataToStruct a = St.deconstruct $ case a of
    AST_Object a         -> St.putData a
    AST_Arith  a b c loc -> St.with "equation" $ St.putDataAt "left" a >> St.putDataAt "op" b >> St.putDataAt "right" c >> St.putData loc
  structToData =   St.reconstruct $ msum $
    [ St.tryWith "equation" $ pure AST_Arith <*> St.getDataAt "left" <*> St.getDataAt "op" <*> St.getDataAt "right"  <*> St.getData
    , AST_Object <$> St.getData
    , fail "expecting arithmetic expression"
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

instance St.Structured AssignExpr Object where
  dataToStruct = putIntermediate "object intermedaite node"
  structToData = getIntermediate "object intermedaite node"

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

instance ObjectClass AssignExpr where
  objectMethods = defObjectInterface nullValue $ do
    autoDefNullTest >> autoDefEquality >> autoDefBinaryFmt
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

instance St.Structured AST_Assign Object where
  dataToStruct a = St.deconstruct $ case a of
    AST_Eval  o       -> St.putData o
    AST_Assign a b c loc -> St.with "assign" $ St.putDataAt "to" a >> St.putDataAt "op" b >> St.putDataAt "from" c >> St.putData loc
  structToData =   St.reconstruct $ msum $
    [ St.tryWith "assign" $ pure AST_Assign <*> St.getDataAt "to" <*> St.getDataAt "op" <*> St.getDataAt "from" <*> St.getData
    , AST_Eval <$> St.getData
    , fail "expecting assignment expression"
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

instance St.Structured TopLevelExpr  Object where
  dataToStruct = putIntermediate toplevel_intrm
  structToData = getIntermediate toplevel_intrm

-- Since 'TopLevelExpr's can modify the 'ExecUnit', and since 'Exec' is not a stateful monad, a
-- simple hack is used: every update that should occur on executing the expression is returned as a
-- function which can be applied by the context which called it. Refer to the instance for
-- 'Executable' for the 'Program' type to see how the calling context is used to update the state.
instance Executable TopLevelExpr (ExecUnit -> ExecUnit) where
  execute o = ask >>= \xunit -> case o of
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
      -- get the functions declared St.this far
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
    AST_Attribute  a b    _ -> AST_Attribute       a (fd1 b)                 lu
    AST_TopScript  a      _ -> AST_TopScript (fd   a)                        lu
    AST_Event      a b c  _ -> AST_Event           a      b  (fd  c)         lu
    AST_TopComment a        -> AST_TopComment      a

instance PPrintable AST_TopLevel where
  pPrint o = case o of
    AST_Attribute a b    _ -> pInline [pPrint a, pString "  ", pPrint b, pString ";"]
    AST_TopScript a      _ -> pPrint a
    AST_Event     a b c  _ -> pClosure (pShow a >> mapM_ pPrint b) " { " " }" (map pPrint (getAST_CodeBlock c))
    AST_TopComment a       -> mapM_ (\a -> pPrint a >> pNewLine) a

instance St.Structured AST_TopLevel  Object where
  dataToStruct a = St.deconstruct $ case a of
    AST_TopComment a         -> putComments a
    AST_Attribute  a b   loc -> St.with "attribute" $ St.putDataAt "type" a >> St.putDataAt "value"    b                          >> St.putData loc
    AST_TopScript  a     loc -> St.with "directive" $ St.putData          a                                                    >> St.putData loc
    AST_Event      a b c loc -> St.with "event"     $ St.putDataAt "type" a >> putComments          b >> St.putDataAt "script" c  >> St.putData loc
  structToData = St.reconstruct $ msum $
    [ pure AST_TopComment <*> getComments
    , St.with "attribute" $ pure AST_Attribute <*> St.getDataAt "type" <*> St.getDataAt "value" <*> St.getData
    , St.with "directive" $ pure AST_TopScript <*> St.getData          <*> St.getData
    , St.with "event"     $ pure AST_Event     <*> St.getDataAt "type" <*> getComments       <*> St.getDataAt "script" <*> St.getData
    , fail "top-level directive"
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

----------------------------------------------------------------------------------------------------

-- | A program is just a list of 'TopLevelExpr's. It serves as the 'Dao.Object.AST.Intermediate'
-- representation of a 'Dao.Object.AST.AST_SourceCode'.
newtype Program = Program { topLevelExprs :: [TopLevelExpr] } deriving (Eq, Ord, Typeable)

instance Show Program where { show (Program o) = unlines (map show o) }

instance HasLocation Program where
  getLocation o = case topLevelExprs o of
    [] -> LocationUnknown
    [o] -> getLocation o
    o:ox -> mappend (getLocation o) (getLocation (foldl (flip const) o ox))
  setLocation o _ = o
  delLocation o = Program (fmap delLocation (topLevelExprs o))

-- | Initialized the current 'ExecUnit' by evaluating all of the 'Dao.Object.TopLevel' data in a
-- 'Dao.Object.AST.AST_SourceCode'.
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

----------------------------------------------------------------------------------------------------

-- | A 'SourceCode' is the structure loaded from source code. An 'ExecUnit' object is constructed from
-- 'SourceCode'.
data AST_SourceCode
  = AST_SourceCode
    { sourceModified :: Int
    , sourceFullPath :: UStr
      -- ^ the URL (full file path) from where St.this source code was received.
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

instance Intermediate Program AST_SourceCode where
  toInterm   ast = return $ Program (directives ast >>= toInterm)
  fromInterm obj = return $
    AST_SourceCode
    { sourceModified = 0
    , sourceFullPath = nil
    , directives     = topLevelExprs obj >>= fromInterm
    }

----------------------------------------------------------------------------------------------------
-- $Builtin_object_interfaces
-- The following functions provide object interfaces for essential data types.

instance ObjectClass () where { objectMethods = defObjectInterface () (return ()) }

instance ObjectClass GlobAction where
  objectMethods = defObjectInterface (GlobAction [] undefined) $ do
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
            -- TODO: the subroutine should be scanned for integer references and replaced St.with local
            -- variables called "varN" where N is the number of the integer reference.
        }

----------------------------------------------------------------------------------------------------

-- | Nearly every accesssor function in the 'ObjectInterface' data type take the form
-- > 'ObjectInterface' 'Data.Dynamic.Dynamic' -> Maybe ('Data.Dynamic.Dynamic' -> method)
-- where the first 'Data.Dynamic.Dynamic' value is analogous to the @St.this@" pointer in C++-like
-- languages, and where @method@ is any function, for example an equality function @a -> a -> Bool@
-- or an iterator function @Exec [Object]@. This function takes the @St.this@ value, an
-- 'ObjectInterface', and an 'ObjectInterface' accessor (for example 'objEquality' or
-- 'objIterator') and if the accessor is not 'Prelude.Nothing', the @St.this@ object is applied to the
-- method and the partial application is returned. If the accessor does evaluate to
-- 'Prelude.Nothing' the exception value is thrown. If the @St.this@ object has not been constructed
-- St.with 'OHaskell', the exception value is thrown.
evalObjectMethod :: Object -> Object -> (ObjectInterface Dynamic -> Maybe (Dynamic -> method)) -> Exec method
evalObjectMethod errmsg this getter = case this of
  OHaskell (HaskellData this ifc) -> case getter ifc of
    Nothing -> execThrow errmsg
    Just fn -> return (fn this)
  _ -> execThrow errmsg

----------------------------------------------------------------------------------------------------

type Get     a = B.GGet  MethodTable a
type PutM    a = B.GPutM MethodTable
type Put       = B.GPut  MethodTable
type Update  a = St.GenUpdate Object a
type UpdateErr = St.GenUpdateErr Object

-- This is only necessary to shorten the name 'MethodTable' because it is used throughout so many
-- instance declarations and type contexts.
type MTab = MethodTable

----------------------------------------------------------------------------------------------------

newtype MethodTable = MethodTable (M.Map UStr (ObjectInterface Dynamic))

instance Monoid MethodTable where
  mempty  = MethodTable mempty
  mappend (MethodTable a) (MethodTable b) = MethodTable (M.union b a)

-- | Lookup an 'ObjectInterface' by it's name from within the 'Exec' monad.
execGetObjTable :: UStr -> Exec (Maybe (ObjectInterface Dynamic))
execGetObjTable nm = asks (lookupMethodTable nm . globalMethodTable)

lookupMethodTable :: UStr -> MethodTable -> Maybe (ObjectInterface Dynamic)
lookupMethodTable nm (MethodTable tab) = M.lookup nm tab

-- not for export, use 'daoClass'
insertMethodTable
  :: (Typeable o, ObjectClass o)
  => o
  -> UStr
  -> ObjectInterface o
  -> MethodTable
  -> MethodTable
insertMethodTable _ nm ifc = flip mappend $
  MethodTable (M.singleton nm (objectInterfaceToDynamic ifc))

typeRepToUStr :: TypeRep -> UStr
typeRepToUStr a = let con = typeRepTyCon a in ustr (tyConModule con ++ '.' : tyConName con)

instance B.HasCoderTable MethodTable where
  getEncoderForType nm mtab = fmap fst $ lookupMethodTable nm mtab >>= objBinaryFormat
  getDecoderForType nm mtab = fmap snd $ lookupMethodTable nm mtab >>= objBinaryFormat

----------------------------------------------------------------------------------------------------

-- | Instantiate your data type into St.this class when it makes sense for your data type to be used in
-- a "for" statement in the Dao programming language.
class HasIterator obj where
  -- | This function converts your object to a list. Conversion is done as lazily as possible to
  -- prevent "for" statements from eating up a huge amount of memory when iteration produces a large
  -- number of objects. For example, lets say your @obj@ is an association list. This function
  -- should return a list of every assocaition in the @obj@. Each association object will be stored
  -- in a local variable and the body of the "for" statement will be evaluated St.with that local
  -- variable.
  iterateObject :: obj -> Exec [Object]
  -- | This function takes the list of objects that was produced after evaluating the "for"
  -- statement and uses it to create a new @obj@ data. For example, if your data type is an
  -- association list and the "for" loop eliminates every 'Object' not satisfying a predicate, the
  -- object list passed here will be the lists of 'Object's that did satisfy the predicate and you
  -- should construct a new @obj@ using St.this new list of 'Object's.
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
-- 'ObjectInterface' under the same funcion name: 'objectMethods'. Instantiate St.this function St.with
-- the help of the 'defObjectInterface' function.
class ObjectClass typ where { objectMethods :: ObjectInterface typ }

-- | This is a convenience function for calling 'OHaskell' using just an initial value of type
-- @typ@. The 'ObjectInterface' is retrieved automatically using the instance of 'objectMethods' for
-- the @typ@.
mkHaskellData :: (ObjectClass typ, Typeable typ) => typ -> HaskellData
mkHaskellData t = HaskellData (toDyn t) (interfaceTo t objectMethods) where
  interfaceTo :: Typeable typ => typ -> ObjectInterface typ -> ObjectInterface Dynamic
  interfaceTo _ ifc = objectInterfaceToDynamic ifc

-- | Create a new 'Object' containing the original value and a reference to the 'ObjectInterface'
-- retrieved by the instance of 'objectMethods' for the data type.
new :: (ObjectClass typ, Typeable typ) => typ -> Object
new = OHaskell . mkHaskellData

-- | Create a completely opaque haskell data type that can be used stored to a Dao language
-- variable, but never read or modified in any way.
opaque :: Typeable typ => typ -> Object
opaque o = OHaskell $ HaskellData (toDyn o) $
  objectInterfaceToDynamic $ defObjectInterface o $ return ()

-- | This is all of the functions used by the "Dao.Evaluator" when manipulating objects in a Dao
-- program. Behavior of objects when they are used in "for" statements or "with" statements, or when
-- they are dereferenced using the "@" operator, or when they are used in equations are all defined
-- here.
-- 
-- So St.this table is the reason you instantiate 'ObjectClass'.
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
-- This should usually be a 'Control.Monad.Monad'ic type like @IO@ or 'Dao.Object.Exec'.
data ObjectInterface typ =
  ObjectInterface
  { objHaskellType     :: TypeRep -- ^ St.this type is deduced from the initial value provided to the 'defObjectInterface'.
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

instance Eq  (ObjectInterface typ) where { a==b = objHaskellType a == objHaskellType b }

instance Ord (ObjectInterface typ) where { compare a b = compare (objHaskellType a) (objHaskellType b) }

-- | This function works a bit like 'Data.Functor.fmap', but maps an 'ObjectInterface' from one type
-- to another. This requires two functions: one that can cast from the given type to the adapted
-- type (to convert outputs of functions), and one that can cast back from the adapted type to the
-- original type (to convert inputs of functions). Each coversion function takes a string as it's
-- first parameter, St.this is a string containing the name of the function that is currently making
-- use of the conversion operation. Should you need to use 'Prelude.error' or 'execError', St.this
-- string will allow you to throw more informative error messages. WARNING: St.this function leaves
-- 'objHaskellType' unchanged, the calling context must change it.
objectInterfaceAdapter
  :: (Typeable typ_a, Typeable typ_b)
  => (String -> typ_a -> typ_b)
  -> (String -> typ_b -> typ_a)
  -> ObjectInterface typ_a
  -> ObjectInterface typ_b
objectInterfaceAdapter a2b b2a ifc = 
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

objectInterfaceToDynamic :: Typeable typ => ObjectInterface typ -> ObjectInterface Dynamic
objectInterfaceToDynamic oi = objectInterfaceAdapter (\ _ -> toDyn) (from oi) oi where
  from :: Typeable typ => ObjectInterface typ -> String -> Dynamic -> typ
  from oi msg dyn = fromDyn dyn (dynErr oi msg dyn)
  dynErr :: Typeable typ => ObjectInterface typ -> String -> Dynamic -> typ
  dynErr oi msg dyn = error $ concat $
    [ "The 'Dao.Object.", msg
    , "' function defined for objects of type ", show (objHaskellType oi)
    , " was evaluated on an object of type ", show (dynTypeRep dyn)
    ]

-- Used to construct an 'ObjectInterface' in a "Control.Monad.State"-ful way. Instantiates
-- 'Data.Monoid.Monoid' to provide 'Data.Monoid.mempty' an allows multiple inheritence by use of the
-- 'Data.Monoid.mappend' function in the same way as
data ObjIfc typ =
  ObjIfc
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

initObjIfc :: ObjIfc typ
initObjIfc =
  ObjIfc
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

-- | A handy monadic interface for defining an 'ObjectInterface' using nice, clean procedural
-- syntax.
type    DaoClassDef typ = DaoClassDefM typ ()
newtype DaoClassDefM typ a = DaoClassDefM { daoClassDefState :: State (ObjIfc typ) a }
instance Typeable typ => Functor (DaoClassDefM typ) where
  fmap f (DaoClassDefM m) = DaoClassDefM (fmap f m)
instance Typeable typ => Monad (DaoClassDefM typ) where
  return = DaoClassDefM . return
  (DaoClassDefM m) >>= f = DaoClassDefM (m >>= daoClassDefState . f)
instance Typeable typ => Applicative (DaoClassDefM typ) where { pure=return; (<*>)=ap; }

-- not for export
updObjIfc :: Typeable typ => (ObjIfc typ -> ObjIfc typ) -> DaoClassDefM typ ()
updObjIfc = DaoClassDefM . modify

-- | The callback function defined here is used when objects of your @typ@ can be constructed from
-- some other 'Object'. This function is used to convert an 'Object' of another types to an data
-- type of your @typ@ when it is necessary to do so (for example, evaluating the @==@ or @!=@
-- operator).
defCastFrom :: Typeable typ => (Object -> typ) -> DaoClassDefM typ ()
defCastFrom fn = updObjIfc(\st->st{objIfcCastFrom=Just fn})

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @==@ and @!=@ operators in Dao programs. However using St.this is slightly
-- different than simply overriding the @==@ or @!=@ operators. Defining an equality reliation St.with
-- St.this function also allows Haskell language programs to compare your object to other objects
-- without unwrapping them from the 'Object' wrapper.
--
-- This function automatically define an equality operation over your @typ@ using the
-- instantiation of 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function.
-- The 'defCastFrom' function is used to cast 'Object's to a value of your @typ@, and then the
-- @Prelude.==@ function is evaluated. If you eventually never define a type casting funcion using
-- 'defCastFrom', St.this function will fail, but it will fail lazily and at runtime, perhaps when you
-- least expect it, so be sure to define 'defCastFrom' at some point.
autoDefEquality :: (Typeable typ, Eq typ) => DaoClassDefM typ ()
autoDefEquality = defEquality (==)

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @==@ and @!=@ operators in Dao programs. However using St.this is slightly
-- different than simply overriding the @==@ or @!=@ operators. Defining an equality relation St.with
-- St.this function also allows Haskell language programs to compare your object to other objects
-- without unwrapping them from the 'Object' wrapper.
--
-- This function differs from 'autoDefEquality' because you must provide a customized equality
-- relation for your @typ@, if the 'autoDefEquality' and 'defCastFrom' functions are to be avoided
-- for some reason.
defEquality :: (Typeable typ, Eq typ) => (typ -> typ -> Bool) -> DaoClassDefM typ ()
defEquality fn = updObjIfc(\st->st{objIfcEquality=Just fn})

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @<@, @>@, @<=@, and @>=@ operators in Dao programs. However using St.this is
-- slightly different than simply overriding the @<@, @>@, @<=@, or @>=@ operators. Defining an
-- equality relation St.with St.this function also allows Haskell language programs to compare your obejct
-- to other objects without unwrapping them from the 'Object' wrapper.
-- 
-- Automatically define an ordering for your @typ@ using the instantiation of
-- 'Prelude.Eq' and the function you have provided to the 'defCastFrom' function. The 'defCastFrom'
-- function is used to cast 'Object's to a value of your @typ@, and then the @Prelude.==@ function
-- is evaluated. If you eventually never define a type casting funcion using 'defCastFrom', St.this
-- function will fail, but it will fail lazily and at runtime, perhaps when you least expect it, so
-- be sure to define 'defCastFrom' at some point.
autoDefOrdering :: (Typeable typ, Ord typ) => DaoClassDefM typ ()
autoDefOrdering = defOrdering compare

-- | The callback function defined here is used where objects of your @typ@ might be compared to
-- other objects using the @<@, @>@, @<=@, and @>=@ operators in Dao programs. However using St.this is
-- slightly different than simply overriding the @<@, @>@, @<=@, or @>=@ operators. Defining an
-- equality relation St.with St.this function also allows Haskell language programs to compare your obejct
-- to other objects without unwrapping them from the 'Object' wrapper.
-- 
-- Define a customized ordering for your @typ@, if the 'autoDefEquality' and 'defCastFrom'
-- functions are to be avoided for some reason.
defOrdering :: (Typeable typ) => (typ -> typ -> Ordering) -> DaoClassDefM typ ()
defOrdering fn = updObjIfc(\st->st{objIfcOrdering=Just fn})

-- | The callback function defined here is used if an object of your @typ@ should ever need to be
-- stored into a binary file in persistent storage (like your filesystem) or sent across a channel
-- (like a UNIX pipe or a socket).
-- 
-- It automatically define the binary encoder and decoder using the 'Data.Binary.Binary' class
-- instantiation for St.this @typ@.
autoDefBinaryFmt :: (Typeable typ, B.Binary typ MethodTable) => DaoClassDefM typ ()
autoDefBinaryFmt = defBinaryFmt B.put B.get

-- | This function is used if an object of your @typ@ should ever need to be stored into a binary
-- file in persistent storage (like your filesystem) or sent across a channel (like a UNIX pipe or a
-- socket).
-- 
-- If you have binary coding and decoding methods for your @typ@ but for some silly reason not
-- instantiated your @typ@ into the 'Data.Binary.Binary' class, your @typ@ can still be used as a
-- binary formatted object by the Dao system if you define the encoder and decoder using St.this
-- function. However, it would be better if you instantiated 'Data.Binary.Binary' and used
-- 'autoDefBinaryFmt' instead.
defBinaryFmt :: (Typeable typ) => (typ -> Put) -> Get typ -> DaoClassDefM typ ()
defBinaryFmt put get = updObjIfc(\st->st{objIfcBinaryFormat=Just(put,get)})

autoDefNullTest :: (Typeable typ, HasNullValue typ) => DaoClassDefM typ ()
autoDefNullTest = defNullTest testNull

-- | The callback function defined here is used if an object of your @typ@ is ever used in an @if@
-- or @while@ statement in a Dao program. This function will return @Prelude.True@ if the object is
-- of a null value, which will cause the @if@ or @while@ test to fail and execution of the Dao
-- program will branch accordingly. There is no default method for St.this function so it must be
-- defined by St.this function, otherwise your object cannot be tested by @if@ or @while@ statements.
defNullTest :: Typeable typ => (typ -> Bool) -> DaoClassDefM typ ()
defNullTest fn = updObjIfc(\st->st{objIfcNullTest=Just fn})

-- | The callback function to be called when the "print" built-in function is used.
defPPrinter :: Typeable typ => (typ -> PPrint) -> DaoClassDefM typ ()
defPPrinter fn = updObjIfc(\st->st{objIfcPPrinter=Just fn})

-- | The callback function to be called when the "print" built-in function is used.
autoDefPPrinter :: (Typeable typ, PPrintable typ) => DaoClassDefM typ ()
autoDefPPrinter = defPPrinter pPrint

-- | The callback function defined here is used if an object of your @typ@ is ever used in a @for@
-- statement in a Dao program. However it is much better to instantiate your @typ@ into the
-- 'HasIterator' class and use 'autoDefIterator' instead.
defIterator :: Typeable typ => (typ -> Exec [Object]) -> (typ -> [Object] -> Exec typ) -> DaoClassDefM typ ()
defIterator iter fold = updObjIfc(\st->st{objIfcIterator=Just(iter,fold)})

-- | Using the instantiation of the 'HasIterator' class for your @typ@, installs the necessary
-- callbacks into the 'ObjectInterface' to allow your data type to be iterated over in the Dao
-- programming language when it is used in a "for" statement.
autoDefIterator :: (Typeable typ, HasIterator typ) => DaoClassDefM typ ()
autoDefIterator = defIterator iterateObject foldObject

-- | The callback function defined here is used at any point in a Dao program where an expression
-- containing your object typ is subscripted St.with square brackets, for example in the statement:
-- @x[0] = t[1][A][B];@ The object passed to your callback function is the object containing the
-- subscript value. So in the above example, if the local variables @x@ and @t@ are both values of
-- your @typ@, St.this callback function will be evaluated four times:
-- 1.  St.with the given 'Object' parameter being @('OInt' 0)@ and the @typ@ parameter as the value stored in
--     the local variable @x@.
-- 2.  St.with the given 'Object' parameter being @('OInt' 1)@ and the @typ@ parameter as the value
--     stored in the local variable @y@.
-- 3.  once St.with the 'Object' parameter being the result of dereferencing the local varaible @A@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 4.  once the given 'Object' parameter being the result of dereferencing the local variable @B@ and
--     the @typ@ parameter as the value stored in the local variable @y@.
-- 
-- Statements like St.this:
-- > a[0,1,2]
-- are evaluated by the "Dao.Evaluator" module in the exact same way as statements like St.this:
-- > a[0][1][2]
defIndexer :: Typeable typ => (typ -> Object -> Exec Object) -> DaoClassDefM typ ()
defIndexer fn = updObjIfc(\st->st{objIfcIndexer=Just fn})

-- | Use your data type's instantiation of 'ToDaoStructClass' to call 'defToStruct'.
autoDefToStruct :: forall typ . (Typeable typ, ToDaoStructClass typ Object) => DaoClassDefM typ ()
autoDefToStruct = defToStruct ((predicate :: Predicate ExecControl T_struct -> Exec T_struct) . fmapPFail ((\o -> execError{ execReturnValue=Just o}) . new) . dataToStruct)

-- | When a label referencing your object has a field record accessed, for example:
-- > c = a.b;
-- if your object is referenced by @a@ and the script expression wants to access a record called @b@
-- from within it, then function defined here will be used.
defToStruct :: Typeable typ => (typ -> Exec T_struct) -> DaoClassDefM typ ()
defToStruct encode = updObjIfc (\st -> st{ objIfcToStruct=Just encode })

-- | When a label referencing your object has a field record updated, for example:
-- > a.b = c;
-- if your object is referenced by @a@ and the script expression wants to update a record called @b@
-- within it by assigning it the value referenced by @c@, then the function defined here will be
-- used.
autoDefFromStruct :: (Typeable typ, FromDaoStructClass typ Object) => DaoClassDefM typ ()
autoDefFromStruct = defFromStruct (predicate . fmapPFail ((\o -> execError{ execReturnValue=Just o }) . new) . dataFromStruct)

-- | If for some reason you need to define a tree encoder and decoder for the 'ObjectInterface' of
-- your @typ@ without instnatiating 'St.Structured', use St.this function to define the tree encoder an
-- decoder directly
defFromStruct :: Typeable typ => (T_struct -> Exec typ) -> DaoClassDefM typ ()
defFromStruct decode = updObjIfc (\st -> st{ objIfcFromStruct=Just decode })

-- | The callback defined here is used when a Dao program makes use of the static initialization
-- syntax of the Dao programming language, which are expression of this form:
-- > a = MyType { paramA=initA, paramB=initB, .... };
-- > a = MyType(param1, param2, ...., paramN) { paramA=initA, paramB=initB, .... };
-- When the interpreter sees St.this form of expression, it looks up the 'ObjectInterface' for your
-- @typ@ and checks if a callback has been defined by 'defDictInit'. If so, then the callback is
-- evaluated St.with a list of object values passed as the first parameter which contain the object
-- values written in the parentheses, and a 'T_dict' as the second paramter containing the tree
-- structure that was constructed St.with the expression in the braces.
defDictInit :: Typeable typ => ([Object] -> Exec typ) -> (typ -> [(Object, UpdateOp, Object)] -> Exec typ) -> DaoClassDefM typ ()
defDictInit fa fb = updObjIfc(\st->st{objIfcDictInit=Just (fa, fb)})

-- | The callback defined here is used when a Dao program makes use of the static initialization
-- syntax of the Dao programming language, which are expression of this form:
-- > a = MyType { initA, initB, .... };
-- > a = MyType(param1, param2, ...., paramN) { initA, initB, .... };
-- When the interpreter sees St.this form of expression, it looks up the 'ObjectInterface' for your
-- @typ@ and checks if a callback has been defined by 'defDictInit'. If so, then the callback is
-- evaluated St.with a list of object values passed as the first parameter which contain the object
-- values written in the parentheses, and a 'T_dict' as the second paramter containing the tree
-- structure that was constructed with the expression in the braces.
-- 
-- You can define both 'defDictInit' and 'defListInit', but if both are defined, only 'defDictInit'
-- will be used.
defListInit :: Typeable typ => ([Object] -> Exec typ) -> (typ -> [Object] -> Exec typ) -> DaoClassDefM typ ()
defListInit fa fb = updObjIfc(\st->st{objIfcListInit=Just(fa,fb)})

-- | Overload update/assignment operators in the Dao programming language, for example @=@, @+=@,
-- @<<=@ and so on. Call St.this method as many times St.with as many different 'UpdateOp's as necessary.
-- 
-- Like St.with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
-- 
-- If you define two callbacks for the same 'UpdateOp', St.this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defUpdateOp :: Typeable typ => UpdateOp -> (UpdateOp -> typ -> Object -> Exec Object) -> DaoClassDefM typ ()
defUpdateOp op fn = updObjIfc(\st->st{objIfcUpdateOpTable=objIfcUpdateOpTable st++[(op, fn)]})

-- | Overload infix operators in the Dao programming language, for example @+@, @*@, or @<<@.
-- 
-- Like St.with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
--
-- If you define two callbacks for the same 'UpdateOp', St.this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defInfixOp :: Typeable typ => InfixOp -> (InfixOp -> typ -> Object -> Exec Object) -> DaoClassDefM typ ()
defInfixOp op fn = updObjIfc $ \st -> st{objIfcInfixOpTable  = objIfcInfixOpTable  st ++ [(op, fn)] }

-- | Overload prefix operators in the Dao programming language, for example @@@, @$@, @-@, and @+@.
-- 
-- Like St.with C++, the operator prescedence and associativity is permanently defined by the parser
-- and cannot be changed by the overloading mechanism. You can only change how the operator behaves
-- based on the type of it's left and right hand parameters.
-- 
-- If you define two callbacks for the same 'UpdateOp', St.this will result in a runtime error,
-- hopefully the error will occur during the Dao runtime's object loading phase, and not while
-- actually executing a program.
defPrefixOp :: Typeable typ => ArithPfxOp -> (ArithPfxOp -> typ -> Exec Object) -> DaoClassDefM typ ()
defPrefixOp op fn = updObjIfc $ \st -> st{objIfcPrefixOpTable = objIfcPrefixOpTable st ++ [(op, fn)] }

-- | Define static functions which can be called on objects of your @typ@. If you define a function here
-- St.with the string "funcName", then in a Dao program, you will be able to write an expression such
-- as: @myObj.funcName(param1, param2, ..., paramN);@.
--
-- It is also possible to instantiate your @typ@ into the 'St.Structured' class in such a way that the
-- dao expression: @myObj.name1.name2.name3@ evaluates to a 'DaoFunc', in which case that
-- 'DaoFunc' will be evaluated using parameters should a function expression be evaluated, such as:
-- @myObj.name1.name2.name3(param1, param2)@. This means if your 'St.Structured' instance
-- evaluates to a function for a label @"name1@", the expression @myObj.name1(param1, param2)@
-- will conflict St.with any method defined St.with 'defMethod'. Should St.this happen, the function returned
-- by the 'St.Structured' instance will be used, rather than the function defined St.with 'defMethod'. The
-- reasoning for St.this is that the Dao program may want to modify the functions of an object at
-- runtime, and so the functions defined at runtime should not be masked by functions defined by
-- 'defMethod'. The 'defMethod' functions, therefore, are the immutable default functions for those
-- function names.
--defMethod :: Typeable typ => Name -> (typ -> DaoFunc) -> DaoClassDefM typ ()
--defMethod nm fn = updObjIfc $ \st ->
--  st{objIfcMethods = T.execUpdateTree (T.goto [nm] >> T.modifyLeaf(<>(Just [fn]))) (objIfcMethods st)}

defCallable :: Typeable typ => (typ -> Exec [CallableCode]) -> DaoClassDefM typ ()
defCallable fn = updObjIfc (\st -> st{objIfcCallable=Just fn})

defDeref :: Typeable typ => (typ -> Exec (Maybe Object)) -> DaoClassDefM typ ()
defDeref  fn = updObjIfc (\st -> st{objIfcDerefer=Just fn})

-- | Rocket. Yeah. Sail away with you.
defLeppard :: Typeable typ => rocket -> yeah -> DaoClassDefM typ ()
defLeppard _ _ = return ()

-- | This is the Dao 'Object' interface to the Haskell language. Every function in St.this data type
-- allows you to customize the behavior of the Dao evaluator for a particular Haskell data type
-- @typ@. In order for your type to be useful, it must be possible to pass your data type to the
-- 'OHaskell' constructor, which requires a data type of 'Data.Dynamic.Dynamic', which means your
-- @typ@ must derive a class instance for 'Data.Typeable.Typeable'. The first parameter of type
-- @typ@ is not used except to retrieve it's 'Data.Typeable.TypeRep' using the
-- 'Data.Typealble.typeOf' function, it is safe to pass any data constructor St.with all of it's fields
-- 'Prelude.undefined', just the constructor itself must not be 'Prelude.undefined'.
-- 
-- The @'DaoClassDefM'@ parameter you pass to St.this function is a monadic function so you can simply
-- declare the functionality you would like to include in St.this object one line at a time using
-- the procedural coding style. Each line in the "procedure" will be one of the @def*@ functions,
-- for example 'autoDefEquality' or 'autoDefOrdering'.
defObjectInterface :: Typeable typ => typ -> DaoClassDefM typ ig -> ObjectInterface typ
defObjectInterface init defIfc =
  ObjectInterface
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
    ifc               = execState (daoClassDefState defIfc) initObjIfc
    mkArray oiName elems =
      if null elems
        then  Nothing
        else  minAccumArray (onlyOnce oiName) Nothing $ map (\ (i, e) -> (i, (i, Just e))) elems
    onlyOnce oiName a b  = case b of
      (_, Nothing) -> a
      (i, Just  _) -> conflict oiName ("the "++show i++" operator")
    conflict oiName funcName = error $ concat $
      [ "'Dao.Object.", oiName
      , "' has conflicting functions for ", funcName
      , " for the 'Dao.Object.ObjectClass' instantiation of the '", show typ
      , "' Haskell data type."
      ]

