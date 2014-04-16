-- "src/Dao/Lib/File.hs"  built-in plain file object
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

module Dao.Lib.File where

import           Dao.String
import           Dao.PPrint
import qualified Dao.Binary as B
import           Dao.Interpreter

import           Data.Typeable

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           System.IO

data File = File { filePath :: UStr, fileHandle :: Maybe Handle } deriving Typeable

instance Eq File where { a==b = filePath a == filePath b }

instance Ord File where { compare a b = compare (filePath a) (filePath b) }

instance Show File where { show a = "File("++show (filePath a)++")" }

instance HasNullValue File where
  nullValue = File{ filePath=nil, fileHandle=Nothing }
  testNull (File{ filePath=name, fileHandle=Nothing }) = nil==name
  testNull (File{}) = False

instance PPrintable File where { pPrint = pShow }

instance B.Binary File MethodTable where
  get    = return File <*> B.get <*> pure Nothing
  put f = B.put (filePath f)

_openParamFail :: String -> Exec ig
_openParamFail func = fail ("the "++func++"() function expects a file path as the only paremeter")

_paramPath :: String -> [Object] -> Exec UStr
_paramPath func ox = case ox of
  [path] -> xmaybe (fromObj path <|> (filePath <$> fromObj path)) <|> _openParamFail func
  _      -> _openParamFail func

_openFile :: String -> UStr -> IOMode -> Exec File
_openFile func path mode =
  fmap (File path . Just) $ execCatchIO (liftIO $ openFile (uchars path) mode) $
    [ newExecIOHandler $ \e -> execThrow $ obj $
        [obj "in function", obj func, obj path, obj (show (e::IOException))]
    ]

loadLibrary_File :: DaoSetup
loadLibrary_File = do
  let fileOpener func mode = daoFunction func $
        daoFunc
        { funcAutoDerefParams = True
        , daoForeignFunc = \ () -> _paramPath func >=> fmap (Just . obj) . flip (_openFile func) mode
        }
  fileOpener "readFile"   ReadMode
  fileOpener "writeFile"  WriteMode
  fileOpener "appendFile" AppendMode
  daoFunction "File" $
    daoFunc
    { funcAutoDerefParams = True
    , daoForeignFunc = \ () -> fmap (Just . obj . flip File Nothing) . _paramPath "File"
    }

instance ObjectClass File where { obj=new; fromObj=objFromHata; }

instance HataClass File where
  haskellDataInterface = interface (File nil Nothing) $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest
    autoDefPPrinter >> autoDefBinaryFmt
    let fileOpener func mode = defMethod func $
          daoFunc
          { funcAutoDerefParams = False
          , daoForeignFunc = \typ ox -> case ox of
              [] -> case fileHandle typ of
                Nothing -> Just . obj <$> _openFile func (filePath typ) mode
                Just  _ -> execThrow $ obj [obj "file already open", obj typ]
              _  -> fail (func++"() method must be passed no parameters")
          }
    fileOpener "openRead"   ReadMode
    fileOpener "openWrite"  WriteMode
    fileOpener "openAppend" AppendMode
    defMethod "close" $
      daoFunc
      { funcAutoDerefParams = False
      , daoForeignFunc = \typ ox -> case ox of
          [] -> case fileHandle typ of
            Nothing -> execThrow $ obj [obj typ, obj "not open, cannot close"]
            Just  h -> execHandleIO [execIOHandler] (liftIO $ hClose h) >> return Nothing
          _  -> fail "close() method must be passed no parameters"
      }

