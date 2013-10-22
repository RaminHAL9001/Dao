-- "src/Dao/Struct.hs"  provides a class that can convert between
-- Hasekll "data" types and 'Dao.Tree.Tree's.
-- 
-- Copyright (C) 2008-2013  Ramin Honary.
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
-- along atAddress this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A module with utility functions to help define instanes to the 'Dao.Object.Structured' class.
-- A stateful monadic interface built on zippers is provided to allow leaves and branches in
-- 'Dao.Tree.Tree's to be read and written using a syntax similar to that of a procedural
-- programming language, which is more intuitive for the Dao programming language which is
-- procedural. Please also see the 'Dao.Object.Structured' class and the 'Dao.Object.UpdateErr' data
-- type.
module Dao.Struct where

import           Prelude hiding (lookup)

import           Dao.String
import           Dao.Object
import           Dao.Tree
import           Dao.Predicate
import           Dao.Object.Math

import           Data.Monoid
import           Data.List (intercalate)
import           Data.Word
import           Data.Int
import           Data.Ratio
import qualified Data.Map    as M
import qualified Data.IntMap as I
import qualified Data.Set    as S

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Error

import Debug.Trace

-- | This is the fundamental data type for Dao's foreign interface. This monad is used to convert
-- data types in your Haskell program into a form that can be manipulated by Dao scripts.
--
-- Essentially, this is a simple zipper monad for traversing 'Dao.Object.T_tree' objects, which are 'Dao.Tree.Tree's with
-- 'Dao.String.Name's on the branches and 'Dao.Object.Object's on the leaves (nodes).
newtype Update a = Update { updateToPTrans :: PTrans UpdateErr (UpdateTreeT Name Object Identity) a }
instance Monad Update where
  (Update fn) >>= mfn = Update (fn >>= updateToPTrans . mfn)
  return a = Update (return a)
  fail msg = newUpdateErr (Just msg) >>= throwError
instance Functor Update where
  fmap fn (Update a) = Update (fmap fn a)
instance MonadPlus Update where
  mzero = Update mzero
  mplus (Update a) (Update b) = Update (mplus a b)
instance MonadState (Tree Name Object) Update where
  get = Update $ PTrans $ gets focus >>= return . OK
  put tree = Update $ PTrans $ modify (\st -> st{focus=tree}) >> return (OK ())
instance MonadError UpdateErr Update where
  throwError = Update . throwError
  catchError (Update func) catcher = Update (catchError func (updateToPTrans . catcher))
instance MonadPlusError UpdateErr Update where
  catchPValue (Update func) = Update (catchPValue func)
  assumePValue = Update . assumePValue
instance Applicative Update where { pure = return; (<*>) = ap; }
instance Alternative Update where { empty = mzero; (<|>) = mplus; }
instance Monoid a => Monoid (Update a) where
  mempty      = return mempty
  mappend a b = liftM2 mappend a b

----------------------------------------------------------------------------------------------------

-- | If you would like to create a detailed error message, create an error using the current
-- information, then throw it using 'Control.Monad.Error.throwError'.
newUpdateErr :: Maybe String -> Update UpdateErr
newUpdateErr msg = Update (lift get) >>= \st -> return (err st) where
  err st =
    UpdateErr
    { updateErrMsg  = fmap ustr msg
    , updateErrAddr = reverse $ fmap fst $ history st
    , updateErrTree = focus st
    }

updateError :: Update ig
updateError = newUpdateErr Nothing >>= throwError

-- | Goto an address. This 'Update' never fails, even if the address does not exist. Immediately
-- returns the sub-tree to which we traversed.
goto :: [Name] -> Update T_tree
goto path = Update (lift (Dao.Tree.goto path))

-- | Go up one node level.
back :: Update ()
back = Update (lift Dao.Tree.back)

-- | Go to the root node.
home :: Update ()
home = Update (lift Dao.Tree.home)

-- | Like 'Control.Monad.StateT.runStateT', evaluates the 'Update' monad as a pure function,
-- returning a pair containing first the 'Dao.Predicate.PValue' of that was returned and second the
-- updated 'Dao.Tree.Tree'.
runUpdate :: Update a -> Tree Name Object -> (PValue UpdateErr a, Tree Name Object)
runUpdate upd = runUpdateTree (runPTrans (updateToPTrans upd))

-- | Update a data type in the 'Structured' class using an 'Update' monadic function.
onStruct :: Structured a => Update ig -> a -> PValue UpdateErr a
onStruct ufn a = (fst . runUpdate (ufn>>get)) (dataToStruct a) >>= structToData

-- | Useful for instantiating the 'dataToStruct' function of the 'Structured' class, this is
-- essentially the same function as 'Control.Monad.State.execState'.
deconstruct :: Update a -> Tree Name Object
deconstruct fn = snd (runUpdate fn Void)

-- | Useful for instantiating 'structToData', for example:
-- @'structToData' = 'reconstruct' $ do { ... }@
-- Then write everything in the @do@ statement that reconstructs your Haskell data type from a Dao
-- structure. This is essentially the same function as 'Control.Monad.State.evalState'.
reconstruct :: Update a -> Tree Name Object -> PValue UpdateErr a
reconstruct fn tree = fst (runUpdate fn tree)

----------------------------------------------------------------------------------------------------
-- $Fundamentals
-- These are the most important functions for building instances of 'Structured'.

-- | Return the value stored in the current node. Evaluates to 'Control.Monad.mzero' if the current
-- node is empty, so it can be used to check if an item exists at the current node as well. This
-- function is the counter operation of 'place'.
this :: Update Object
this = get >>= maybe mzero return . getLeaf

-- | This is a 'Control.Monad.guard'-like function. Because 'tryWith' actually steps onto the
-- branch, and there maye be situations where you want to check if a branch exists without actually
-- stepping on to that branch, use this function to check if a branch exists and backtrack (evaluate
-- to 'Control.Monad.mzero') if it does not exist.
guardBranch :: String -> Update ()
guardBranch addr = tryWith addr (return ())

-- | Same as 'atAddress' but but more convenient as it takes just one string and passes it to
-- 'atAddress' as @['Dao.String.ustr' address]@. This function is the counter operation of itself.
-- In other words, @'with' addr putFunc@ is the counter operation of @'with' addr getFunc@ where
-- @getFunc@ and @putFunc@ are any function which are counter operations of each other.
tryWith :: String -> Update a -> Update a
tryWith = atAddress . (:[]) . ustr

-- | Like 'tryWith' but throws fails if nothing exists at the given address.
with :: String -> Update a -> Update a
with addr upd = mplus (tryWith addr upd) updateError

-- | Use 'structToData' to construct data from the current node. This function is the counter
-- operation of 'putData'. 'Dao.Predicate.Backtrack's if the current node is 'Dao.Tree.Void'.
tryGetData :: Structured a => Update a
tryGetData = get >>= assumePValue . structToData

-- | Use 'structToData' to construct data from the current node. This function is the counter
-- operation of 'putData'. 'Dao.Predicate.Backtrack's if the current node is 'Dao.Tree.Void'.
getData :: Structured a => Update a
getData = mplus tryGetData updateError

-- | Like 'getData' but takes a default value as a parameter, and if the current 'Dao.Tree.Tree'
-- node returned by 'Control.Monad.State.get' is 'Dao.Tree.Void', the default parameter is returned.
getOptional :: Structured a => a -> Update a
getOptional opt = mplus getData (return opt)

-- | Shortcut for @'with' addr 'getData'@. This function is the counter operation of 'putDataAt'.
getDataAt :: Structured a => String -> Update a
getDataAt addr = with addr getData

tryGetDataAt :: Structured a => String -> Update a
tryGetDataAt addr = tryWith addr tryGetData

getMaybe :: Structured a => Update (Maybe a)
getMaybe = mplus (fmap Just getData) (return Nothing)

getMaybeAt :: Structured a => String -> Update (Maybe a)
getMaybeAt addr = with addr $ getMaybe

-- | Place an object at in current node. This function is the counter opreation of 'this'.
place :: Object -> Update ()
place obj = placeWith (const (Just obj))

-- | Use 'dataToStruct' to convert a data type to a 'Structured' 'Dao.Tree.Tree' node, then union
-- it with the current node. If the current node is a 'Dao.Tree.Leaf', the leaf might be
-- overwritten if you write a new 'Dao.Tree.Leaf'. This function is the couner operation of
-- 'getData'.
putData :: Structured a => a -> Update ()
putData = putTree . dataToStruct

-- | Shortcut for @'with' addr ('putData' a)@. This function is the counter opreation of
-- 'getDataAt'.
putDataAt :: Structured a => String -> a -> Update ()
putDataAt addr obj = with addr (putData obj)

putMaybe :: Structured a => Maybe a -> Update ()
putMaybe = maybe (return ()) putData

putMaybeAt :: Structured a => String -> Maybe a -> Update ()
putMaybeAt addr = with addr . putMaybe

-- | Update an object at the current node.
placeWith :: ModLeaf Object -> Update ()
placeWith fn = modify (alterLeaf fn)

-- | Applies a function only if there is an item at the current node.
mapThis :: (Object -> Object) -> Update ()
mapThis fn = placeWith (\item -> fmap fn item)

-- | Put an 'Dao.Object.Object' in the current ('this') location, overwriting what is already here.
putObjAt :: Object -> Update ()
putObjAt = modify . alterLeaf . const . Just

-- | Union a tree node with the current node. If the current node is a 'Dao.Tree.Leaf', the leaf
-- might be overwritten if you write a new 'Dao.Tree.Leaf'. *IMPORTANT:* Use this instead of
-- 'Control.Monad.State.put'.
putTree :: Tree Name Object -> Update ()
putTree = modify . union

-- | Modify or write a new a data structure at a given address using the given 'Update' function.
atAddress :: [Name] -> Update a -> Update a
atAddress path doUpdate = do
  Dao.Struct.goto path
  a <- doUpdate
  sequence_ (fmap (const Dao.Struct.back) path)
  return a

-- | Goes to a given address and tries to return the value stored at that node,
-- 'Dao.Predicate.Backtrack's if nothing is there.
peekAddress :: [Name] -> Update Object
peekAddress addr = atAddress addr this

----------------------------------------------------------------------------------------------------
-- $Helpers
-- Here are some handy helper functions for common data types which you can use to construct your
-- own 'dataToStruct' and 'structToData' instances of 'Structured'.

putUStrData :: UStrType str => str -> Update ()
putUStrData s =
  let u = toUStr s in place (if ulength u == 1 then OChar (head (uchars u)) else OString u)

getUStrData :: UStrType str => str -> Update UStr
getUStrData msg = do
  a <- this
  case a of
    OString a -> return a
    OChar   c -> return (ustr [c])
    _         -> fail ("was expecting a string for constructing a "++uchars msg++" object")

getIntegerData :: Integral a => String -> Update a
getIntegerData msg = do
  a <- this
  case a of
--  OLong a -> return (fromIntegral a)
    OInt  a -> return (fromIntegral a)
--  OWord a -> return (fromIntegral a)
    _ -> fail ("was expecting an integer value for constructing a "++msg++" object")

getBoolData :: String -> String -> String -> Update Bool
getBoolData msg tru fals = do
  a <- this
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
--  OWord i -> return (i/=0)
--  OLong i -> return (i/=0)
    _ -> fail $ concat $
      [ "was expecting a boolean value ("
      , show tru, " or ", fals
      , ") for constructing ", msg, " object"
      ]

----------------------------------------------------------------------------------------------------
-- $Instances
-- Instantiation of common data types into the 'Strcutured' class. Although you may be better off
-- instantiation your own lists or 'Data.Map.Map's, you can use these default instantiations if you
-- would rather not bother writing your own instances.

instance Structured () where
  dataToStruct _ = deconstruct $ place ONull
  structToData = reconstruct $ this >>= \o -> case o of
    ONull -> return ()
    o     -> fail $ "expecting () as ONull value, instead got "++show (objType o)

instance Structured Object where
  dataToStruct = deconstruct . place
  structToData = reconstruct this

instance Structured UStr where
  dataToStruct a = deconstruct $ place (OString a)
  structToData = reconstruct $ do
    a <- this
    case a of
      OString a -> return a
      _         -> fail "expecing string constant"

instance Structured Bool where
  dataToStruct a = deconstruct $ place (if a then OTrue else ONull)
  structToData = reconstruct $ getBoolData "strucutred boolean" "true" "false"

--instance Structured Word64 where
--  dataToStruct a = deconstruct $ place (OWord a)
----  structToData = reconstruct (fmap fromIntegral (getIntegerData "unsigned integer"))

instance Structured Word where
  dataToStruct a = deconstruct $ place (OInt (fromIntegral a))
  structToData = reconstruct (fmap fromIntegral (getIntegerData "unsigned integer"))

--instance Structured Int64 where
--  dataToStruct a = deconstruct $ place (OInt a)
--  structToData = reconstruct (fmap fromIntegral (getIntegerData "integer"))

instance Structured Int where
  dataToStruct a = deconstruct $ place (OInt (fromIntegral a))
  structToData = reconstruct (fmap fromIntegral (getIntegerData "integer"))

--instance Structured Integer where
--  dataToStruct a = deconstruct $ place (OLong a)
--  structToData = reconstruct (fmap toInteger (getIntegerData "long-integer"))

newtype StructChar = StructChar Char
instance Structured StructChar where
  dataToStruct (StructChar c) = deconstruct (place (OChar c))
  structToData = reconstruct $ this >>= \c -> case c of
    OChar c -> return (StructChar c)
    _       -> fail "singleton character"

--instance Structured (Ratio Integer) where
--  dataToStruct a = deconstruct (place (OPair (OLong (numerator a), OLong (denominator a))))
--  structToData = reconstruct $ this >>= \a -> case a of
--    OPair (a, b) -> assumePValue $
--      objToIntegral a >>= \a -> objToIntegral b >>= \b -> return (a % b)

putListWith :: (a -> Update ()) -> [a] -> Update ()
putListWith dat2srct ox = place (OList (fmap (OTree . deconstruct . dat2srct) ox))

getListWith :: Update a -> Update [a]
getListWith srct2dat = do
  o <- this
  case o of
    OList ox -> forM ox $ \o -> case o of
      OTree o -> assumePValue (reconstruct srct2dat o)
      _       -> fail "was expecting structured data in each list item"
    _        -> fail "was expecting a list object"

instance Structured a => Structured [a] where
  dataToStruct ax = deconstruct (putListWith putData ax)
  structToData    = reconstruct (getListWith getData)

instance Structured (Tree Name Object) where
  dataToStruct a = deconstruct $ place (OTree a)
  structToData = reconstruct $ do
    o <- this
    case o of
      OTree o -> return o
      _       -> fail $
        "was expecting an 'OTree' object containing structured data to construct "

-- | It might be useful to have a general instantiation of polymorphic 'Dao.Tree.Tree' types where
-- the keys and values are both instances of 'Structured', but this instantiation would overlap with
-- the instantiation of @('Dao.Tree.Tree' 'Dao.String.Name' 'Dao.Object.Object')@. For convenience,
-- if you want to use the more default instantiation for 'Structured'izing 'Dao.Tree.Tree's so you
-- don't have to write your own, you can wrap your tree in this @newtype@ and use it with 'getData'
-- and 'putData' like so:
-- > instance 'Structured' MyKey { 'structToData' = .... ; 'dataToStruct' = .... ; }
-- > instance 'Structured' MyVal { 'structToData' = .... ; 'dataToStruct' = .... ; }
-- >
-- > putMyTree :: 'Dao.Tree.Tree' MyKey MyVal -> 'Update' ()
-- > putMyTree myTree = 'putData' (StructuredTree MyKey MyVal)
-- >
-- > getMyTree :: 'Update' ('Dao.Tree.Tree' MyKey MyVal)
-- > getMyTree = 'Control.Monad.fmap' 'fromStructuredTree' 'getData'
--newtype StructuredTree a b = StructuredTree { fromStructuredTree :: Tree a b }
--instance (Ord a, Structured a, Structured b) => Structured (StructuredTree a b) where
--  dataToStruct (StructuredTree a) = deconstruct $ case a of
--    Void           -> return ()
--    Leaf       a   -> putData a
--    Branch       b -> putBranch b
--    LeafBranch a b -> putData a >> putBranch b
--    where { putBranch b = with "branch" (putData (StructuredMap (M.map StructuredTree b))) }
--  structToData = reconstruct $ do
--    a <- mplus (fmap Just getData) (return Nothing)
--    b <- flip mplus (return Nothing) $ do
--      b <- getDataAt "branch"
--      return (Just (M.map fromStructuredTree (fromStructuredMap b)))
--    case (a, b) of
--      (Nothing, Nothing) -> fail "structured tree"
--      (Just  a, Nothing) -> return (StructuredTree (Leaf a))
--      (Nothing, Just b) -> return (StructuredTree (Branch b))
--      (Just  a, Just b) -> return (StructuredTree (LeafBranch a b))
    
-- | Like 'StructuredTree' but for 'Data.Map.Map's.
--newtype StructuredMap a b = StructuredMap { fromStructuredMap :: M.Map a b }
--instance (Ord a, Structured a, Structured b) => Structured (StructuredMap a b) where
--  dataToStruct (StructuredMap mp) = deconstruct $ place $ OList $
--    Prelude.map (\ (a, b) -> OPair (OTree (dataToStruct a), OTree (dataToStruct b))) (M.assocs mp)
--  structToData = reconstruct $ do
--    ax <- this
--    case ax of
--      OList ax -> do
--        ax <- forM ax $ \a -> case a of
--          OPair (OTree a, OTree b) ->
--            liftM2 (,) (assumePValue $ structToData a) (assumePValue $ structToData b)
--          a                        -> fail "structured map item"
--        return (StructuredMap (M.fromList ax))
--      _        -> fail "list of map items"

