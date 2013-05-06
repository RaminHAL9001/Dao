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

module Dao.Struct where

import           Prelude hiding (lookup)

import           Dao.String
import           Dao.Object
import           Dao.Tree
import           Dao.Predicate
import           Dao.Object.Math

import           Data.Maybe (fromMaybe)
import           Data.Word
import           Data.Int
import           Data.Ratio
import qualified Data.Map    as M
import qualified Data.IntMap as I
import qualified Data.Set    as S

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Error

import Debug.Trace

type UpdateErr = ([Name], Object)

-- | Can be used with 'Dao.Predicate.fmapFailed' to get an error message expressed as an
-- 'Dao.Object.Object' value.
objFromUpdateErr :: UpdateErr -> Object
objFromUpdateErr err = OList [ostr "at address", ORef (GlobalRef (fst err)), snd err]

class Structured a where
  dataToStruct :: a -> Tree Name Object
  structToData :: Tree Name Object -> PValue UpdateErr a

newtype Update a = Update { updateToPTrans :: PTrans UpdateErr (State (Tree Name Object)) a }

-- | Like 'Control.Monad.StateT.runStateT', evaluates the 'Update' monad as a pure function,
-- returning a pair containing first the 'Dao.Predicate.PValue' of that was returned and second the
-- updated 'Dao.Tree.Tree'.
runUpdate :: Update a -> Tree Name Object -> (PValue UpdateErr a, Tree Name Object)
runUpdate upd tree = runState (runPTrans (updateToPTrans upd)) tree

instance Monad Update where
  (Update fn) >>= mfn = Update (fn >>= updateToPTrans . mfn)
  return a = Update (return a)

instance Functor Update where
  fmap fn (Update a) = Update (fmap fn a)

instance MonadPlus Update where
  mzero = Update mzero
  mplus (Update a) (Update b) = Update (mplus a b)

instance MonadState (Tree UStr Object) Update where
  get = Update $ PTrans $ get >>= return . OK
  put st = Update $ PTrans $ put st >> return (OK ())

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

instance MonadState (Tree Name Object) (PTrans UpdateErr (State (Tree Name Object))) where
  get = PTrans (fmap OK get)
  put = PTrans . (fmap OK) . put

-- | Like 'Dao.Predicate.maybeToBacktrack', if the function parameter is 'Nothing' this monad
-- evaluates to 'Dao.Predicate.Backtrack' (except wrapped in the 'Udpate' monad).
maybeToUpdate :: Maybe a -> Update a
maybeToUpdate = Update . pvalue . maybeToBacktrack

-- | Like 'Dao.Predicate.pvalue', evaluates this monad to the 'Dao.Predicate.PValue' predicate value
-- you specify and wraps it up in the 'Update' monad.
updatePValue :: PValue UpdateErr a -> Update a
updatePValue = Update . pvalue

----------------------------------------------------------------------------------------------------
-- $Fundamentals
-- These are the most important functions for building instances of 'Structured'.

-- | Return the value stored in the current node. Evaluates to 'Control.Monad.mzero' if the current
-- node is empty, so it can be used to check if an item exists at the current node as well. This
-- function is the counter operation of 'place'.
this :: Update Object
this = get >>= maybeToUpdate . getLeaf

-- | Same as 'atAddress' but but more convenient as it takes just one string and passes it to
-- 'atAddress' as @['Dao.String.ustr' address]@. This function is the counter operation of itself.
-- In other words, @'with' addr putFunc@ is the counter operation of @'with' addr getFunc@ where
-- @getFunc@ and @putFunc@ are any function which are counter operations of each other.
with :: String -> Update a -> Update a
with = atAddress . (:[]) . ustr

-- | Use 'structToData' to construct data from the current node. This function is the counter
-- operation of 'putData'. 'Dao.Predicate.Backtrack's if the current node is 'Dao.Tree.Void'.
getData :: Structured a => Update a
getData = get >>= \tree -> case tree of
  Void -> mzero
  tree -> updatePValue (structToData tree)

-- | Like 'getData' but takes a default value as a parameter, and if the current 'Dao.Tree.Tree'
-- node returned by 'Control.Monad.State.get' is 'Dao.Tree.Void', the default parameter is returned.
getOptional :: Structured a => a -> Update a
getOptional opt = mplus getData (return opt)

-- | Shortcut for @'with' addr 'getData'@. This function is the counter operation of 'putDataAt'.
getDataAt :: Structured a => String -> Update a
getDataAt addr = with addr getData

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

-- | Update an object at the current node.
placeWith :: ModLeaf Object -> Update ()
placeWith fn = modify (alterData fn)

-- | Applies a function only if there is an item at the current node.
mapThis :: (Object -> Object) -> Update ()
mapThis fn = placeWith (\item -> fmap fn item)

-- | Put an 'Dao.Object.Object' in the current ('this') location, overwriting what is already here.
putObjAt :: Object -> Update ()
putObjAt = modify . alterData . const . Just

-- | Union a tree node with the current node. If the current node is a 'Dao.Tree.Leaf', the leaf
-- might be overwritten if you write a new 'Dao.Tree.Leaf'. *IMPORTANT:* Use this instead of
-- 'Control.Monad.State.put'.
putTree :: Tree Name Object -> Update ()
putTree = modify . merge union const

-- | Same as 'peekAddress' but more convenient as it takes just one string and passes it to
-- 'atAddress' as @['Dao.String.ustr' address]@.
peek :: String -> Update Object
peek addr = peekAddress [ustr addr]

-- | Modify or write a new a data structure at a given address using the given 'Update' function.
atAddress :: [Name] -> Update a -> Update a
atAddress nm doUpdate = Update $ PTrans $ state $ alterNodeWith (runUpdate doUpdate) nm

-- | Goes to a given address and tries to return the value stored at that node,
-- 'Dao.Predicate.Backtrack's if nothing is there.
peekAddress :: [Name] -> Update Object
peekAddress addr = atAddress addr this

-- | Report a failure with an 'Dao.Object.Object' value that occurred at the current address.
updateFailed :: Object -> String -> Update ig
updateFailed obj msg = updatePValue $ PFail ([], obj)

----------------------------------------------------------------------------------------------------
-- $Helpers
-- Here are some handy helper functions for common data types which you can use to construct your
-- own 'dataToStruct' and 'structToData' instances of 'Structured'.

putStringData :: String -> Update ()
putStringData = putData . ustr

getStringData :: String -> Update String
getStringData msg = do
  a <- this
  case a of
    OString a -> return (uchars a)
    OChar   c -> return [c]
    _         -> updateFailed a ("was expecting a string for constructing a "++msg++" object")

getIntegerData :: Integral a => String -> Update a
getIntegerData msg = do
  a <- this
  case a of
    OLong a -> return (fromIntegral a)
    OInt  a -> return (fromIntegral a)
    OWord a -> return (fromIntegral a)
    _ -> updateFailed a ("was expecting an integer value for constructing a "++msg++" object")

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
    OWord i -> return (i/=0)
    OLong i -> return (i/=0)
    _ -> updateFailed a $ concat $
      [ "was expecting a boolean value ("
      , show tru, " or ", fals
      , ") for constructing ", msg, " object"
      ]

----------------------------------------------------------------------------------------------------
-- $Instances
-- Instantiation of common data types into the 'Strcutured' class. Although you may be better off
-- instantiation your own lists or 'Data.Map.Map's, you can use these default instantiations if you
-- would rather not bother writing your own instances.

instance Structured Object where
  dataToStruct = deconstruct . place
  structToData = reconstruct this

instance Structured UStr where
  dataToStruct a = deconstruct $ place (OString a)
  structToData = reconstruct $ do
    a <- this
    case a of
      OString a -> return a
      _         -> updateFailed a "expecing string constant"

instance Structured Bool where
  dataToStruct a = deconstruct $ place (if a then OTrue else ONull)
  structToData = reconstruct $ getBoolData "strucutred boolean" "true" "false"

instance Structured Word64 where
  dataToStruct a = deconstruct $ place (OWord a)
  structToData = reconstruct (fmap fromIntegral (getIntegerData "unsigned integer"))

instance Structured Word where
  dataToStruct a = deconstruct $ place (OWord (fromIntegral a))
  structToData = reconstruct (fmap fromIntegral (getIntegerData "unsigned integer"))

instance Structured Int64 where
  dataToStruct a = deconstruct $ place (OInt a)
  structToData = reconstruct (fmap fromIntegral (getIntegerData "integer"))

instance Structured Int where
  dataToStruct a = deconstruct $ place (OInt (fromIntegral a))
  structToData = reconstruct (fmap fromIntegral (getIntegerData "integer"))

instance Structured Integer where
  dataToStruct a = deconstruct $ place (OLong a)
  structToData = reconstruct (fmap toInteger (getIntegerData "long-integer"))

newtype StructChar = StructChar Char
instance Structured StructChar where
  dataToStruct (StructChar c) = deconstruct (place (OChar c))
  structToData = reconstruct $ this >>= \c -> case c of
    OChar c -> return (StructChar c)
    _       -> updateFailed c "singleton character"

instance Structured (Ratio Integer) where
  dataToStruct a = deconstruct (place (OPair (OLong (numerator a), OLong (denominator a))))
  structToData = reconstruct $ this >>= \a -> case a of
    OPair (a, b) -> updatePValue $
      objToIntegral a >>= \a -> objToIntegral b >>= \b -> return (a % b)

instance Structured a => Structured [a] where
  dataToStruct ox = deconstruct $ place (OList (map (OTree . dataToStruct) ox))
  structToData = reconstruct $ do
    o <- this
    case o of
      OList ox -> forM ox $ \o -> case o of
        OTree o -> updatePValue $ structToData o
        _ -> updateFailed o "was expecting structured data in each list item"
      _ -> updateFailed o "was expecting a list object"

instance Structured (Tree Name Object) where
  dataToStruct a = deconstruct $ place (OTree a)
  structToData = reconstruct $ do
    o <- this
    case o of
      OTree o -> return o
      _       -> updateFailed o $
        "was expecting an 'OTree' object containing structured data to construct "

-- | It might be useful to have a general instantiation of polymorphic 'Dao.Tree.Tree' types where
-- the keys and values are both instances of 'Structured', but this instantiation would overlap with
-- the instantiation of @('Dao.Tree.Tree' 'Dao.String.Name' 'Dao.Object.Object')@. For convenience,
-- if you want to use the more default instantiation for 'Structured'izing 'Dao.Tree.Tree's so you
-- don't have to write your own, you can wrap your tree in this @newtype@ and use it with 'getData'
-- and 'putData' like so:
-- @instance Structured MyKey@
-- @instance Structured MyVal@
-- 
-- @putMyTree :: 'Dao.Tree.Tree' MyKey MyVal -> 'Update' ()@
-- @putMyTree myTree = putData (StructuredTree myTree)@
--
-- @getMyTree :: 'Update' ('Dao.Tree.Tree' MyKey MyVal)@
-- @getMyTree = fmap fromStructuredTree getData@
newtype StructuredTree a b = StructuredTree { fromStructuredTree :: Tree a b }
instance (Ord a, Structured a, Structured b) => Structured (StructuredTree a b) where
  dataToStruct (StructuredTree a) = deconstruct $ case a of
    Void           -> return ()
    Leaf       a   -> putData a
    Branch       b -> putBranch b
    LeafBranch a b -> putData a >> putBranch b
    where { putBranch b = with "branch" (putData (StructuredMap (M.map StructuredTree b))) }
  structToData = reconstruct $ do
    a <- mplus (fmap Just getData) (return Nothing)
    b <- flip mplus (return Nothing) $ do
      b <- getDataAt "branch"
      return (Just (M.map fromStructuredTree (fromStructuredMap b)))
    case (a, b) of
      (Nothing, Nothing) -> updateFailed (OTree Void) "structured tree"
      (Just  a, Nothing) -> return (StructuredTree (Leaf a))
      (Nothing, Just b) -> return (StructuredTree (Branch b))
      (Just  a, Just b) -> return (StructuredTree (LeafBranch a b))
    
-- | Like 'StructuredTree' but for 'Data.Map.Map's.
newtype StructuredMap a b = StructuredMap { fromStructuredMap :: M.Map a b }
instance (Ord a, Structured a, Structured b) => Structured (StructuredMap a b) where
  dataToStruct (StructuredMap mp) = deconstruct $ place $ OList $
    map (\ (a, b) -> OPair (OTree (dataToStruct a), OTree (dataToStruct b))) (M.assocs mp)
  structToData = reconstruct $ do
    ax <- this
    case ax of
      OList ax -> do
        ax <- forM ax $ \a -> case a of
          OPair (OTree a, OTree b) ->
            liftM2 (,) (updatePValue $ structToData a) (updatePValue $ structToData b)
          a                        -> updateFailed a "structured map item"
        return (StructuredMap (M.fromList ax))
      _        -> updateFailed ax "list of map items"

