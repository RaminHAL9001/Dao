-- "src/Dao/Prelude.hs"  a module to provide convenient APIs to the
-- internals of the Dao system, especially designed for use in GHCi.
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
-- along with this program (see the file called "LICENSE"). If not, see
-- <http://www.gnu.org/licenses/agpl.html>.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Dao.Prelude
  ( module Dao.Prelude
  , module Dao.Predicate
  , module Dao.Token
  , module Dao.Parser
  , module Dao.String
  , module Dao.Object
  , module Dao.Object.AST
  , module Dao.Object.PPrint
  , module Dao.PPrint
  , module Control.Monad
  , module Control.Monad.State
  , module Control.Applicative
  ) where

-- | This type  for all commands in this module to "pipe" everything together like in the Bourne
-- scripting language where you can pipe the output of one command to the input of another, or like
-- Haskell "arrows", except with the monadic bind 'Control.Monad.(>>=)' operator (monads are a
-- subcategory of arrows anyway).

import           Dao.String
import qualified Dao.Tree as T
import           Dao.Object
import           Dao.PPrint
import           Dao.Struct
import           Dao.Predicate
import           Dao.Token
import           Dao.Parser
import           Dao.Random
import           Dao.Binary

import           Dao.Object.PPrint
import           Dao.Object.AST
import           Dao.Object.Parser
import           Dao.Object.Struct
import           Dao.Object.Random
import           Dao.Object.Binary

import           Control.Exception
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative

import           Data.Monoid
import           Data.Typeable
import           Data.Dynamic
import           Data.IORef
import           Data.Char
import           Data.List (intercalate)
import qualified Data.ByteString.Lazy as Z

import           System.IO
import           System.IO.Unsafe

----------------------------------------------------------------------------------------------------

-- | Everything is wrapped in this type to prevent GHCi from printing stuff all the time, or
-- reporting errors about types not instantiating the 'Prelude.Show' class.
newtype Dao a = Dao { daoUnwrap :: a }
instance Eq a       => Eq       (Dao a) where { (Dao a)==(Dao b)        = a==b            }
instance Ord a      => Ord      (Dao a) where { compare (Dao a) (Dao b) = compare a b     }
instance               Functor   Dao    where { fmap fn (Dao a)         = Dao (fn a)      }
instance Monoid a   => Monoid   (Dao a) where
  mempty                  = Dao mempty
  mappend (Dao a) (Dao b) = Dao (mappend a b)
instance               Applicative Dao  where
  pure                    = Dao
  (Dao fn) <*> (Dao a)    = Dao (fn a)
instance Typeable a => Show     (Dao a) where
  show (Dao a)            = case typeOf a of
    t | t == typeOf OTrue ->
      let d = toDyn a
      in  show $ objType $ fromDyn d $ error $
            "converting Object to Dynamic and back yields a non-object value"
    t                     -> show t

cast :: (a -> b) -> Dao a -> IO (Dao b)
cast f = return . fmap f

exampleMethodTable :: IORef MethodTable
exampleMethodTable = unsafePerformIO (newIORef mempty)

randMaxDepth :: Int
randMaxDepth = 6

-- | Generate a random object of a polymorphic type with a specified maximum depth.
ioRandOMax :: HasRandGen o => Int -> Int -> IO (Dao o)
ioRandOMax max i = return (Dao (evalState randO (initRandOState max i)))

-- | Generate a random object of a polymorphic type.
ioRandO :: HasRandGen o => Int -> IO (Dao o)
ioRandO i = ioRandOMax randMaxDepth i

-- | Generate a random 'Dao.Object.AST.AST_TopLevel' expression from a seed value.
randTopExpr :: Int -> IO (Dao AST_TopLevel)
randTopExpr = ioRandO

-- | Generate a random 'Dao.Object.AST.AST_Script' from a seed value.
randScriptExpr :: Int -> IO (Dao AST_Script)
randScriptExpr = ioRandO

-- | Generate a random 'Dao.Object.AST.AST_Object' from a seed value.
randObjExpr :: Int -> IO (Dao AST_Object)
randObjExpr = ioRandO

-- | Generate a random 'Dao.Object.Object' value from a seed value.
randObj :: Int -> IO (Dao Object)
randObj = ioRandO

-- | Generate a random 'Dao.Object.AST.AST_TopLevel' expression from a seed value with a specified
-- maximum depth.
randTopExprMax :: Int -> Int -> IO (Dao AST_TopLevel)
randTopExprMax = ioRandOMax

-- | Generate a random 'Dao.Object.AST.AST_Script' from a seed value with a specified maximum depth.
randScriptExprMax :: Int -> Int -> IO (Dao AST_Script)
randScriptExprMax = ioRandOMax

-- | Generate a random 'Dao.Object.AST.AST_Object' from a seed value with a specified maximum depth.
randObjExprMax :: Int -> Int -> IO (Dao AST_Object)
randObjExprMax = ioRandOMax

-- | Generate a random 'Dao.Object.Object' value from a seed value with a specified maximum depth.
randObjMax :: Int -> Int -> IO (Dao Object)
randObjMax = ioRandOMax

-- | Re-export 'Dao.PPring.prettyShow'
-- prettyShow :: PPrintable a => a -> String
-- prettyShow = Dao.PPrintM.prettyShow

-- | Pretty-print anything output by one of the functions in this module.
pp :: PPrintable a => (Dao a) -> IO (Dao a)
pp (Dao o) = putStrLn (prettyShow o) >> return (Dao o)

-- | Convert an object to a string using the pretty printer.
toString :: PPrintable a => (Dao a) -> IO (Dao String)
toString (Dao o) = return (Dao (prettyShow o))

-- | Write the text from pretty-printing an object to a file. I have always disliked how the
-- 'System.IO.IOMode' comes after the file path in the 'System.IO.openFile' function, so the
-- 'System.IO.IOMode' and 'System.IO.FilePath' parameters are flipped.
fileOutputTextWith :: (a -> String) -> IOMode -> FilePath -> Dao a -> IO (Dao a)
fileOutputTextWith toString mode path (Dao obj) = openFile path mode >>= \h ->
  hPutStr h (toString obj) >>= evaluate >> hFlush h >>= evaluate >> hClose h >> return (Dao obj)

-- | Like 'fileOutputTextWith' but uses 'System.IO.WriteMode'.
writeTextWith :: (a -> String) -> FilePath -> Dao a -> IO (Dao a)
writeTextWith toString path obj = fileOutputTextWith toString WriteMode path obj

-- | Like 'fileOutputTextWith' but uses 'System.IO.AppendMode'.
appendTextWith :: (a -> String) -> FilePath -> Dao a -> IO (Dao a)
appendTextWith toString path obj = fileOutputTextWith toString AppendMode path obj

-- | Like 'writeTextWith' but uses 'Dao.PPrintM.prettyShow'.
writeText :: PPrintable a => FilePath -> Dao a -> IO (Dao a)
writeText path obj = writeTextWith prettyShow path obj

-- | Like 'appendTextWith' but uses 'Dao.PPrintM.prettyShow'.
appendText :: PPrintable a => FilePath -> Dao a -> IO (Dao a)
appendText path obj = appendTextWith prettyShow path obj

-- | Parser a polymorphic type from a string expression.
parseWith :: DaoParser a -> Dao String -> IO (Dao a)
parseWith p (Dao str) = case parse (daoGrammar{mainParser=p}) mempty str of
  OK      a -> seq a $! return (Dao a)
  Backtrack -> error "parser backtracks"
  PFail err -> error (show err)

-- | Parse a 'Dao.Object.AST.AST_TopLevel'.
parseTopExpr :: Dao String -> IO (Dao AST_TopLevel)
parseTopExpr = parseWith toplevel

-- | Parse a 'Dao.Object.AST.AST_Script'.
parseScriptExpr :: Dao String -> IO (Dao AST_Script)
parseScriptExpr = parseWith script

-- | Parse a 'Dao.Object.AST.AST_Object'.
parseObjExpr :: Dao String -> IO (Dao AST_Object)
parseObjExpr = parseWith equation

-- | Parse a 'Dao.Object.AST.AST_TopLevel' from an ordinary 'Prelude.String'.
readTopExpr :: String -> IO (Dao AST_TopLevel)
readTopExpr = parseTopExpr . Dao

-- | Parse a 'Dao.Object.AST.AST_TopLeve' from an ordinary 'Prelude.String'.
readScriptExpr :: String -> IO (Dao AST_Script)
readScriptExpr = Dao.Prelude.parseScriptExpr . Dao

-- | Parse a 'Dao.Object.AST.AST_TopLeve' from an ordinary 'Prelude.String'.
readObjExpr :: String -> IO (Dao AST_Object)
readObjExpr = parseObjExpr . Dao

-- | Convert a structured item to its structured form.
toStruct :: Structured o Object => (Dao o) -> IO (Dao (T.Tree Name Object))
toStruct (Dao o) = return (Dao (dataToStruct o))

instance (PPrintable a, PPrintable b) => PPrintable (T.TreeDiff a b) where
  pPrint diff = pEndLine >> case diff of
    T.LeftOnly   a -> pString "< " >> pPrint a
    T.RightOnly  b -> pString "> " >> pPrint b
    T.TreeDiff a b -> pString "< " >> pPrint a >> pEndLine >> pString "> " >> pPrint b

instance (PPrintable a, PPrintable b) => PPrintable (T.Tree UStr (T.TreeDiff a b)) where
  pPrint t = forM_ (T.assocs t) $ \ (addr, obj) -> do
    pString (intercalate "." (map uchars addr) ++ ":")
    pForceNewLine >> pPrint obj >> pForceNewLine

--diff :: (Dao (T.Tree Name Object)) -> (Dao (T.Tree Name Object)) -> IO (Dao (T.Tree Name (T.TreeDiff Object Object)))
--diff (Dao a) (Dao b) = return $ Dao $ T.treeDiff a b

-- | Construct a random object of a polymorphic type from its 'Dao.Struct.Structured' form.
fromStruct :: Structured o Object => (Dao (T.Tree Name Object)) -> IO (Dao o)
fromStruct (Dao t) = case structToData t of
  OK      o -> seq o $! return (Dao o)
  Backtrack -> error "constructor backtracked"
  PFail err -> error (show err)

-- | Construct a random 'Dao.Object.AST.AST_TopLevel' expression from its 'Dao.Struct.Structured' form.
structTopExpr :: Dao (T.Tree Name Object) -> IO (Dao AST_TopLevel)
structTopExpr = fromStruct

-- | Construct a random 'Dao.Object.AST.AST_Script' expression from its 'Dao.Struct.Structured' form.
structScriptExpr :: Dao (T.Tree Name Object) -> IO (Dao AST_Script)
structScriptExpr = fromStruct 

-- | Construct a random 'Dao.Object.AST.AST_Object' expression from its 'Dao.Struct.Structured' form.
structObjExpr :: Dao (T.Tree Name Object) -> IO (Dao AST_Object)
structObjExpr = fromStruct 

-- | Convert a polymorphic type to a 'Data.ByteString.Lazy.ByteString'.
toBinary :: Binary o MethodTable => Dao o -> IO (Dao Z.ByteString)
toBinary (Dao o) = readIORef exampleMethodTable >>= \mtab -> return (Dao (encode mtab o))

-- | Convert a polymorphic type to a 'Data.ByteString.Lazy.ByteString'.
fromBinary :: Binary o MethodTable => Dao Z.ByteString -> IO (Dao o)
fromBinary (Dao o) = readIORef exampleMethodTable >>= \mtab -> return (Dao (decode mtab o))

-- | Parse a 'Dao.Object.TopLevelExpr' from it's binary representation.
unpackTopExpr :: Dao Z.ByteString -> IO (Dao TopLevelExpr)
unpackTopExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unpackScriptExpr :: Dao Z.ByteString -> IO (Dao ScriptExpr)
unpackScriptExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unpackObjExpr :: Dao Z.ByteString -> IO (Dao ObjectExpr)
unpackObjExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unpackObj :: Dao Z.ByteString -> IO (Dao Object)
unpackObj = fromBinary

-- | Reduce a Dao language expression from it's "Dao.Object.AST" representation to it's "Dao.Object"
-- representation using its instantiation of methods 'Dao.Object.AST.Intermediate'. See also:
-- 'expand', the inverse of this function.
reduce :: Intermediate obj ast => Dao ast -> IO (Dao obj)
reduce (Dao ast) = case toInterm ast of
  [obj] -> return (Dao obj)
  []    -> error "could not reduce"
  _     -> error "ambiguous reduction"

-- | Apply 'reduce' to an 'Dao.Object.AST.AST_TopLevel' expression, producing an
-- 'Dao.Object.TopLevelExpr'.
reduceTopExpr :: Dao AST_TopLevel -> IO (Dao TopLevelExpr)
reduceTopExpr = reduce

-- | Apply 'reduce' to an 'Dao.Object.AST.AST_Script' expression, producing an
-- 'Dao.Object.ScriptExpr'.
reduceScriptExpr :: Dao AST_TopLevel -> IO (Dao TopLevelExpr)
reduceScriptExpr = reduce

-- | Apply 'reduce' to an 'Dao.Object.AST.AST_Object' expression, producing an
-- 'Dao.Object.ObjectExpr'.
reduceObjExpr :: Dao AST_TopLevel -> IO (Dao TopLevelExpr)
reduceObjExpr = reduce

-- | Expand a Dao language expression into it's "Dao.Object.AST" representation from it's
-- "Dao.Object" representation using its instantiation of methods 'Dao.Object.AST.Intermediate'. See
-- also: 'reduce', the inverse of this function.
expand :: Intermediate obj ast => Dao obj -> IO (Dao ast)
expand (Dao obj) = case fromInterm obj of
  [ast] -> return (Dao ast)
  []    -> error "could not expand"
  _     -> error "ambiguous expansion"

-- | Apply 'expand' to an 'Dao.Object.AST.AST_TopLevel' expression, producing an
-- 'Dao.Object.TopLevelExpr'.
expandTopExpr :: Dao TopLevelExpr -> IO (Dao AST_TopLevel)
expandTopExpr = expand

-- | Apply 'expand' to an 'Dao.Object.AST.AST_Script' expression, producing an
-- 'Dao.Object.ScriptExpr'.
expandScriptExpr :: Dao ScriptExpr -> IO (Dao AST_Script)
expandScriptExpr = expand

-- | Apply 'expand' to an 'Dao.Object.AST.AST_Object' expression, producing an
-- 'Dao.Object.ObjectExpr'.
expandObjExpr :: Dao ObjectExpr -> IO (Dao AST_Object)
expandObjExpr = expand

-- | Pure function, convert a 'Data.ByteString.Lazy.ByteString' to a string that is easy to look
-- through with your own eyes, all hexadecimal numbers, letters A-F capitalized, spaces between
-- every byte, and split into lines of 32 bytes each.
showBinary :: Z.ByteString -> String
showBinary = show . Base16String

-- | Apply 'showBinary' to a 'Data.ByteString.Lazy.ByteString'.
hexdump :: Dao Z.ByteString -> IO (Dao Z.ByteString)
hexdump (Dao bytes) = putStrLn (showBinary bytes) >> return (Dao bytes)

----------------------------------------------------------------------------------------------------

class IsList a where { listItems :: a b -> [b] }

instance IsList [] where { listItems = id }

-- | Stores items in lists in such a way that GHCi can always pretty-print each item in the list on
-- a single line.
newtype LL a = LL { unwrapLL :: [a] }
instance IsList LL where { listItems = unwrapLL }
instance PPrintable a => Show (LL a) where { show = intercalate "\n" . map prettyShow . listItems }

----------------------------------------------------------------------------------------------------

-- | Object tree: store 'Dao.Object.Object's at addresses. In the Dao scripting language, objects
-- can be stored at address with an expression like:
-- > aaa.bbb.ccc = list {1,2,3};
-- In this "Dao.Prelude" module, functions are provided to easily construct trees of
-- @address->object@ associations. Refer to 'newObjTree', 'defObjTree', and 'delObjTree'.
newtype ObjTree = ObjTree { getObjTreeMVar :: MVar (T.Tree Name Object) } deriving Typeable

data ObjTreePair = ObjTreePair { getObjTreeAddress :: [Name], getObjTreeValue :: Object }
instance Show ObjTreePair where
  show otp = show (getObjTreeAddress otp) ++ " := " ++ prettyShow (getObjTreeValue otp)
instance PPrintable ObjTreePair where
  pPrint otp = pInline [pShow (getObjTreeAddress otp), pString ":=", pPrint (getObjTreeValue otp)]

-- | Create a new tree for relating objects to names.
newObjTree :: IO (Dao ObjTree)
newObjTree = fmap (Dao . ObjTree) (newMVar T.Void)

-- | Define an address storing a value. For example, in the Dao scripting language a global variable
-- can be defined with the following expression:
-- > aaa.bbb.ccc = 0;
-- To define the same address and value assoication, you could use this 'defObjTree' function like so:
-- > 'defineObjTree' [ustr "aaa", ustr "bbb", ustr "ccc"] OInt 0
defineObjTree :: Dao ObjTree -> [Name] -> (a -> Object) -> a -> IO ()
defineObjTree (Dao (ObjTree mvar)) addr construct a =
  modifyMVar_ mvar $ \tree -> return (T.insert addr (construct a) tree)

-- | Define an address storing a value. For example, in the Dao scripting language a global variable
-- can be defined with the following expression:
-- > aaa.bbb.ccc = 0;
-- To define the same address and value assoication, you could use this 'defObjTree' function like so:
-- > 'defObjTree' "aaa bbb ccc" OInt 0
-- Notice each segemtn of the address is separated by spaces, not dots like in the Dao scripting
-- language. This is a simple way to prevent spaces from showing up in names, although an expression
-- like:
-- > 'defObjTree' "aaa 1234 *** +++" OInt 0
-- is also valid. To write the above expression in the Dao scripting language you would have to
-- write:
-- > aaa.$"1234".$"***".$"+++" = 0;
defOT :: Dao ObjTree -> String -> (a -> Object) -> a -> IO ()
defOT daomvar addr construct a = defineObjTree daomvar (map ustr (words addr)) construct a

-- | Delete any item existing at the address given by the address string.
deleteObjTree :: Dao ObjTree -> [Name] -> IO ()
deleteObjTree (Dao (ObjTree mvar)) addr = modifyMVar_ mvar $ \tree -> return (T.delete addr tree)

delOT :: Dao ObjTree -> String -> IO ()
delOT daomvar addr = deleteObjTree daomvar (map ustr (words addr))

listObjTree :: Dao ObjTree -> IO (LL ObjTreePair)
listObjTree (Dao (ObjTree mvar)) = fmap (LL . map pair . T.assocs) (readMVar mvar) where
  pair (path, objVal) = ObjTreePair{ getObjTreeAddress = path, getObjTreeValue = objVal }

