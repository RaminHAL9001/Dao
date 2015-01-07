-- "Dao/Tree.hs"  provides a fundamental data type used by Dao.
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

module Dao.IOArray
  (IOArray, newArray, getSize, getIndexOK, getElems, readArray, writeArray)
  where

import           Dao.Text
import           Dao.TestNull

import           Control.Applicative
import           Control.Monad

import qualified Data.Array.IO as A
import           Data.Monoid
import           Data.Typeable

----------------------------------------------------------------------------------------------------

newtype IOArray o = IOArray (Maybe (A.IOArray Int o)) deriving (Eq, Typeable)

instance Monoid (IO (IOArray o)) where
  mempty = return (IOArray Nothing)
  mappend a b = do
    let appendNull = Just . return . IOArray . Just
    arrA@(IOArray a) <- a
    arrB@(IOArray b) <- b
    maybe (return $ IOArray Nothing) id $ msum $
      [do a <- a
          b <- b
          Just $ do
            s <- (+) <$> getSize arrA <*> getSize arrB
            (++) <$> A.getElems a <*> A.getElems b >>=
              fmap (IOArray . Just) . A.newListArray (0, s)
      , a >>= appendNull
      , b >>= appendNull
      ]

instance TestNullIO (IOArray o) where
  nullValueIO = mempty
  testNullIO (IOArray o) = return $ maybe False (const True) o

instance ToTextIO o => ToTextIO (IOArray o) where
  toTextIO (IOArray o) = case o of
    Nothing -> return $ toText "()"
    Just  o -> A.getElems o >>= listToTextIO

newArray :: [o] -> IO (IOArray o)
newArray ox = case ox of
  [] -> mempty
  ox -> IOArray . Just <$> A.newListArray (0, length ox + 1) ox

getSize :: IOArray o -> IO Int
getSize (IOArray a) = maybe (return 0) (fmap ((1+) . uncurry subtract) . A.getBounds) a

getIndexOK :: IOArray o -> Int -> IO Bool
getIndexOK (IOArray o) i = maybe (return False) (A.getBounds >=> return . flip A.inRange i) o

getElems :: IOArray o -> IO [o]
getElems (IOArray o) = maybe (return []) A.getElems o

readArray :: IOArray o -> Int -> IO (Maybe o)
readArray (IOArray a) i = case a of
  Nothing -> return Nothing
  Just  a -> (`A.inRange` i) <$> A.getBounds a >>= \ok ->
    if ok then Just <$> A.readArray a i else return Nothing

writeArray :: IOArray o -> Int -> o -> IO Bool
writeArray (IOArray a) i o = case a of
  Nothing -> return False
  Just  a -> (`A.inRange` i) <$> A.getBounds a >>= \ok ->
    if ok then A.writeArray a i o >> return True else return False

