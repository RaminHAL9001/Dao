-- "src/Dao/Lib/Array.hs"  built-in array object
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

-- | This module provides the Dao programming language implementation of the 'Array' built-in data
-- type. 'Array's can be indexed from zero to (n-1) where n is the number of elements in the array.
-- Reads and updates are O(n). Resizing an array is not possible.
module Dao.Lib.Array where

import           Dao.Predicate
import           Dao.Interpreter

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State

import           Data.Array.IO
import           Data.Monoid
import           Data.Typeable

import qualified Data.IntMap          as I

----------------------------------------------------------------------------------------------------

-- | The Dao programming language wrapper around Haskell's 'Data.Array.IO.IOArray' data type.
-- To inspect elements of the array outside of the IO monad, the only way for pure functions like
-- 'Dao.PPrint.PPrint' and 'Dao.Binary.Put' to work without being unsafe is to lazily evaluate these
-- functions after every update to the array. These functions are evaluated extra-lazily to ensure
-- that nothing actually happens until it becomes absolutely necessary to use them, if it ever is
-- necessary.
newtype Array = Array { toObjArray :: IOArray Int Object } deriving (Eq, Typeable)

arrayFromList :: Int -> [(Int, Object)] -> IO Array
arrayFromList len ox = do
  arr <- newArray (0, len) ONull
  forM_ ox $ \ (i, o) -> writeArray arr i o
  return $ Array{ toObjArray=arr }

arraySize :: Array -> IO Int
arraySize arr = uncurry subtract <$> getBounds (toObjArray arr)

arrayElems :: Array -> IO [Object]
arrayElems = getElems . toObjArray

arrayCheckIndex :: Array -> Int -> IO (Predicate ExecControl ())
arrayCheckIndex arr i = do
  bounds <- getBounds (toObjArray arr)
  return $
    if inRange bounds i
    then OK ()
    else PFail $ mkExecError{ execReturnValue=Just $ obj "array index out of bounds" }

arrayLookup :: Array -> Int -> IO (Predicate ExecControl Object)
arrayLookup arr i = arrayCheckIndex arr i >>= \result -> case result of
  PFail err -> return (PFail err)
  Backtrack -> return Backtrack
  OK     () -> OK <$> readArray (toObjArray arr) i

arrayUpdate :: Array -> Int -> Object -> IO (Predicate ExecControl ())
arrayUpdate arr i o = arrayCheckIndex arr i >>= \result -> case result of
  PFail err -> return $ PFail err
  Backtrack -> return Backtrack
  OK     () -> OK <$> writeArray (toObjArray arr) i o

instance ReadIterable Array Object where
  readForLoop (Array arr) f = liftIO (getBounds arr) >>=
    mapM_ (liftIO . readArray arr >=> f) . range

instance UpdateIterable Array (Maybe Object) where
  updateForLoop a@(Array arr) f = do
    let err = "for loop iteration attempted to delete an item from an Array"
    liftIO (getBounds arr) >>=
      mapM_ (\i -> liftIO (readArray arr i) >>= f . Just >>=
              maybe (fail err) return >>= liftIO . writeArray arr i) . range
    return a

instance ObjectFunctor Array Int where
  objectFMap f = do
    arr <- get
    let a = toObjArray arr
    (bounds, elems) <- liftIO $ return (,) <*> getBounds a <*> getElems a
    forM_ (zip (range bounds) elems) $ \ (i, o) -> focalPathSuffix (Subscript [obj i] NullRef) $
      withInnerLens [] (f i o) >>=
        mapM_ (liftIO . uncurry (writeArray $ toObjArray arr)) . snd

instance ObjectFunctor Array  Object  where { objectFMap f = objectFMap (\i -> f (obj i)) }
instance ObjectFunctor Array [Object] where { objectFMap f = objectFMap (\i -> f [obj i]) }

_objToInt :: [Object] -> Exec Int
_objToInt i = case i of
  [i] -> (derefObject i >>= xmaybe . fromObj) <|> fail "Array index value cannot be cast to integer"
  _   -> fail "Array index is not one-dimensional"

arrayFromArgs :: [Object] -> IO Array
arrayFromArgs lists =
  forM lists
    (\o -> maybe (return (1, [o])) id $ msum $
        [ fromObj o >>= \ox  -> Just $ return (length ox, ox)
        , fromObj o >>= \arr -> Just $ return (,) <*> arraySize arr <*> arrayElems arr
        ]
    ) >>= uncurry arrayFromList . (\ (ix, ox) -> (sum ix, zip [0..] $ concat ox)) . unzip

loadLibrary_Array :: DaoSetup
loadLibrary_Array = do
  daoClass "Array" (haskellType::Array)
  daoFunction "Array" $
    daoFunc
    { funcAutoDerefParams=True
    , daoForeignFunc = \ () -> fmap (Just . obj) . liftIO . arrayFromArgs
    }

instance ObjectClass Array where { obj=new; fromObj=objFromHata; }

instance HataClass Array where
  haskellDataInterface = interface (Array $ error "uninitialized Array") $ do
    autoDefEquality >> autoDefReadIterable >> autoDefUpdateIterable >> autoDefTraverse
    defSizer (fmap OInt . liftIO . arraySize)
    defIndexer $ \arr i -> _objToInt i >>= liftIO . arrayLookup arr >>= predicate
    defIndexUpdater $ \i f -> focusLiftExec (_objToInt i) >>= \i -> do
      arr <- get
      (_arr, o) <- focusLiftExec (liftIO (arrayLookup arr i) >>= predicate) >>=
        flip withInnerLens f . Just
      case o of
        Nothing -> fail "cannot delete items from array"
        Just  o -> liftIO (writeArray (toObjArray arr) i o) >> return (Just o)
    let init ox = case ox of -- initializer list in round-brackets must be empty
          [] -> return (Array $ error "uninitialized Array") -- SUCCESS: return an uninitialized Array
          _  -> execThrow $ obj "cannot initialize array with parameters" -- FAIL
    let fromList m i maxbnd arr ox = case ox of
          []   -> liftIO $ arrayFromList maxbnd (I.assocs m)
          o:ox -> case o of
            InitAssign ref op o -> referenceLookup ref >>= \i -> case i of
              Nothing -> execThrow $ obj $
                [obj "assigning to array index", obj ref, obj "index evaluated to null"]
              Just (ref, i) -> do
                i <- _objToInt [i]
                if i<0
                then execThrow $ obj [obj "assigned to negative index value", obj i]
                else do
                  o <- evalUpdateOp (Just ref) op o (I.lookup i m)
                  case o of
                    Just  o -> do
                      a <- fromList (I.insert i o m) (i+1) (max i maxbnd) arr ox
                      return $ arr{ toObjArray=toObjArray a }
                    Nothing -> fail $ concat $
                      [ "evaluating assignment operation ", show op
                      , "resulted in void value, cannot be used to initialized Array"
                      ]
            InitSingle o -> fromList (I.insert i o m) (i+1) (max i maxbnd) arr ox
    defInitializer init (fromList mempty 0 0)

