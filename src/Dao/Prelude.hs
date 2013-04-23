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

module Dao.Prelude where

import qualified Data.ByteString.Lazy   as B
 
-- | This type  for all commands in this module to "pipe" everything together like in the Bourne
-- scripting language where you can pipe the output of one command to the input of another, or like
-- Haskell "arrows", except with the monadic bind 'Control.Monad.(>>=)' operator (monads are a
-- subcategory of arrows anyway).

import           RandObj

import           Dao.String
import qualified Dao.Tree as T
import           Dao.Object
import           Dao.PPrint
import           Dao.Struct
import           Dao.Token
import           Dao.Parser

import           Dao.Object.PPrint
import           Dao.Object.AST
import           Dao.Object.Parser
import           Dao.Object.Struct

import           Control.Exception
import           Control.Monad
import           Control.Monad.State

----------------------------------------------------------------------------------------------------

-- | Everything is wrapped in this type to prevent GHCi from printing stuff all the time, or
-- reporting errors about types not instantiating the 'Prelude.Show' class.
newtype O a = O { uno :: a }
instance Show (O a) where { show = "OK" }

randMaxDepth :: Int
randMaxDepth = 6

-- | Generate a random object of a polymorphic type with a specified maximum depth.
ioRandOMax :: HasRandGen o => Int -> Int -> IO (O o)
ioRandOMax max i = return (O (evalState randO (initRandState max i)))

-- | Generate a random object of a polymorphic type.
ioRandO :: HasRandGen o => Int -> IO (O o)
ioRandO i = ioRandOMax randMaxDepth i

-- | Generate a random 'Dao.Object.AST.AST_TopLevel' expression from a seed value.
randTopExpr :: Int -> IO (O AST_TopLevel)
randTopExpr = ioRandO

-- | Generate a random 'Dao.Object.AST.AST_Script' from a seed value.
randScriptExpr :: Int -> IO (O AST_Script)
randScriptExpr = ioRandO

-- | Generate a random 'Dao.Object.AST.AST_Object' from a seed value.
randObjExpr :: Int -> IO (O AST_Object)
randObjExpr = ioRandO

-- | Generate a random 'Dao.Object.Object' value from a seed value.
randObj :: Int -> IO (O Object)
randObj = ioRandO

-- | Generate a random 'Dao.Object.AST.AST_TopLevel' expression from a seed value with a specified
-- maximum depth.
randTopExprMax :: Int -> Int -> IO (O AST_TopLevel)
randTopExprMax = ioRandOMax

-- | Generate a random 'Dao.Object.AST.AST_Script' from a seed value with a specified maximum depth.
randScriptExprMax :: Int -> Int -> IO (O AST_Script)
randScriptExprMax = ioRandOMax

-- | Generate a random 'Dao.Object.AST.AST_Object' from a seed value with a specified maximum depth.
randObjExprMax :: Int -> Int -> IO (O AST_Object)
randObjExprMax = ioRandOMax

-- | Generate a random 'Dao.Object.Object' value from a seed value with a specified maximum depth.
randObjMax :: Int -> Int -> IO (O Object)
randObjMax = ioRandOMax

-- | Pretty-print anything output by one of the functions in this module.
pp :: PPrintable a => (O a) -> IO (O a)
pp (O o) = putStrLn (prettyShow o) >> return (O o)

-- | Convert an object to a string using the pretty printer.
toString :: PPrintable a => (O a) -> IO (O String)
toString (O o) = return (O (prettyShow o))

-- | Parser a polymorphic type from a string expression.
parseWith :: Parser a -> O String -> IO (O a)
parseWith parser (O str) = case runParser parser str of
  (OK          a, _) -> seq a $! return (O a)
  (Backtrack    , s) -> ioError ("parser backtracks:\n\t"++show s)
  (PFail loc msg, s) -> ioError (show loc ++ ": "++uchars msg++"\n\t"++show s)

-- | Parse a 'Dao.Object.AST.AST_TopLevel'.
parseTopExpr :: O String -> IO (O AST_TopLevel)
parseTopExpr = parseWith parseDirective

-- | Parse a 'Dao.Object.AST.AST_Script'.
parseScriptExpr :: O String -> IO (O AST_TopLevel)
parseScriptExpr = parseWith parseScriptExpr

-- | Parse a 'Dao.Object.AST.AST_Object'.
parseScriptExpr :: O String -> IO (O AST_Object)
parseScriptExpr = parseWith parseObjectExpr

-- | Parse a 'Dao.Object.AST.AST_TopLevel' from an ordinary 'Prelude.String'.
readTopExpr :: String -> IO (O AST_TopLevel)
readTopExpr = parseTopExpr . O

-- | Parse a 'Dao.Object.AST.AST_TopLeve' from an ordinary 'Prelude.String'.
readScriptExpr :: String -> IO (O AST_TopLevel)
readScriptExpr = parseScriptExpr . O

-- | Parse a 'Dao.Object.AST.AST_TopLeve' from an ordinary 'Prelude.String'.
readObjExpr :: String -> IO (O AST_TopLevel)
readObjExpr = parseObjExpr . O

-- | Convert a structured item to its structured form.
toStruct :: Structured o => (O o) -> IO (O (T.Tree Name Object))
toStruct (O o) = return (O (dataToStruct o))

-- | Construct a random object of a polymorphic type from its 'Dao.Struct.Structured' form.
fromStruct :: Structured o => (O (T.Tree Name Object)) -> IO (O o)
fromStruct (O t) = return (O (structToData t))

-- | Construct a random 'Dao.Object.AST.AST_TopLevel' expression from its 'Dao.Struct.Structured' form.
structTopExpr :: O (T.Tree Name Object) -> IO (O AST_TopLevel)
structTopExpr = fromStruct

-- | Construct a random 'Dao.Object.AST.AST_Script' expression from its 'Dao.Struct.Structured' form.
structScriptExpr :: O (T.Tree Name Object) -> IO (O AST_Script)
structScriptExpr = fromStruct 

-- | Construct a random 'Dao.Object.AST.AST_Object' expression from its 'Dao.Struct.Structured' form.
structObjExpr :: O (T.Tree Name Object) -> IO (O AST_Object)
structObjExpr = fromStruct 

-- | Construct a random 'Dao.Object.Object.AST_Object' expression from its 'Dao.Struct.Structured' form.
structObjExpr :: O (T.Tree Name Object) -> IO (O AST_Object)
structObjExpr = fromStruct 

-- | Convert a polymorphic type to a 'Data.ByteString.Lazy.ByteString'.
toBinary :: Binary o => O o -> IO (O B.ByteString)
toBinary o = return (encode o)

-- | Convert a polymorphic type to a 'Data.ByteString.Lazy.ByteString'.
fromBinary :: Binary o => O B.ByteString -> IO (O o)
fromBinary (O o) = return (encode o)

-- | Parse a 'Dao.Object.TopLevelExpr' from it's binary representation.
unbinTopExpr :: O B.ByteString -> IO TopLevelExpr
unbinTopExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unbinScriptExpr :: O B.ByteString -> IO ScriptExpr
unbinScriptExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unbinObjExpr :: O B.ByteString -> IO ObjectExpr
unbinObjExpr = fromBinary

-- | Parse a 'Dao.Object.ScriptExpr' from it's binary representation.
unbinObj :: O B.ByteString -> IO Object
unbinObj = fromBinary



