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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Dao.Prelude
  ( module Dao.Prelude
  , module Dao.Predicate
  , module Dao.Token
  , module Dao.String
  , module Dao.Object
  , module Dao.Object.AST
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

import           Dao.Object.PPrint
import           Dao.Object.AST
import           Dao.Object.Parser
import           Dao.Object.Struct
import           Dao.Object.Random
import           Dao.Object.Binary

import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative

import           Data.Monoid
import           Data.Typeable
import           Data.Dynamic
import           Data.Char
import           Data.List (intercalate)
import qualified Data.ByteString.Lazy as B
import           Data.Binary

import           Numeric

import           System.IO

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
-- prettyShow = Dao.PPrint.prettyShow

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

-- | Like 'writeTextWith' but uses 'Dao.PPrint.prettyShow'.
writeText :: PPrintable a => FilePath -> Dao a -> IO (Dao a)
writeText path obj = writeTextWith prettyShow path obj

-- | Like 'appendTextWith' but uses 'Dao.PPrint.prettyShow'.
appendText :: PPrintable a => FilePath -> Dao a -> IO (Dao a)
appendText path obj = appendTextWith prettyShow path obj

-- | Parser a polymorphic type from a string expression.
parseWith :: Parser a -> Dao String -> IO (Dao a)
parseWith parser (Dao str) = case runParser parser str of
  (OK          a, _) -> seq a $! return (Dao a)
  (Backtrack    , s) -> error ("parser backtracks:\n\t"++show s)
  (PFail loc msg, s) -> error (show loc ++ ": "++uchars msg++"\n\t"++show s)

-- | Parse a 'Dao.Object.AST.AST_TopLevel'.
parseTopExpr :: Dao String -> IO (Dao AST_TopLevel)
parseTopExpr = parseWith parseDirective

-- | Parse a 'Dao.Object.AST.AST_Script'.
parseScriptExpr :: Dao String -> IO (Dao AST_Script)
parseScriptExpr = parseWith Dao.Object.Parser.parseScriptExpr

-- | Parse a 'Dao.Object.AST.AST_Object'.
parseObjExpr :: Dao String -> IO (Dao AST_Object)
parseObjExpr = parseWith (fmap fst parseObjectExpr)

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
toStruct :: Structured o => (Dao o) -> IO (Dao (T.Tree Name Object))
toStruct (Dao o) = return (Dao (dataToStruct o))

-- | Construct a random object of a polymorphic type from its 'Dao.Struct.Structured' form.
fromStruct :: Structured o => (Dao (T.Tree Name Object)) -> IO (Dao o)
fromStruct (Dao t) = case structToData t of
  OK              o   -> seq o $! return (Dao o)
  Backtrack           -> error "constructor backtracked"
  PFail (idx,obj) msg -> error $ concat $
    [ "constructor failed: ", uchars msg
    , "\nat index: ", intercalate "." (map uchars idx)
    , "\nwith value: ", prettyShow obj
    ]

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
toBinary :: Binary o => Dao o -> IO (Dao B.ByteString)
toBinary (Dao o) = return (Dao (encode o))

-- | Convert a polymorphic type to a 'Data.ByteString.Lazy.ByteString'.
fromBinary :: Binary o => Dao B.ByteString -> IO (Dao o)
fromBinary (Dao o) = return (Dao (decode o))

-- | Parse a 'Dao.Object.TopLevelExpr' from it's binary representation.
unpackTopExpr :: Dao B.ByteString -> IO (Dao TopLevelExpr)
unpackTopExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unpackScriptExpr :: Dao B.ByteString -> IO (Dao ScriptExpr)
unpackScriptExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unpackObjExpr :: Dao B.ByteString -> IO (Dao ObjectExpr)
unpackObjExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unpackObj :: Dao B.ByteString -> IO (Dao Object)
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
showBinary :: B.ByteString -> String
showBinary b = intercalate "\n" $ breakInto (32*3) $ (" "++) $ map toUpper $
  intercalate " " $ map (\b -> (if b<0x10 then ('0':) else id) (flip showHex "" b)) $ B.unpack b

-- | Apply 'showBinary' to a 'Data.ByteString.Lazy.ByteString'.
hexdump :: Dao B.ByteString -> IO (Dao B.ByteString)
hexdump (Dao bytes) = putStrLn (showBinary bytes) >> return (Dao bytes)

