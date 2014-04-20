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
import           Dao.Predicate
import           Dao.PPrint
import qualified Dao.Binary as B
import           Dao.Interpreter

import qualified Data.ByteString.Lazy as B
import           Data.Typeable

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State

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

_catchIOException :: String -> File -> IO a -> Exec a
_catchIOException func file f = execCatchIO (liftIO f) $
  [ newExecIOHandler $ \e -> execThrow $ obj $
      [ obj "in function", obj func, obj "while reading file"
      , obj (filePath file), obj (show (e::IOException))
      ]
  ]

_openFile :: String -> File -> IOMode -> Exec File
_openFile func file mode = _catchIOException func file $
  (\o -> file{ fileHandle=Just o}) <$> openFile (uchars $ filePath file) mode

_getHandle :: String -> File -> Exec Handle
_getHandle func file = case fileHandle file of
  Just  h -> return h
  Nothing -> execThrow $ obj $
    [obj func, obj "function evaluated on a file handle which has not been opened", obj file]

_withClosedHandle :: String -> File -> Exec ()
_withClosedHandle func file = case fileHandle file of
  Nothing -> return ()
  Just  _ -> execThrow $ obj $
    [obj func, obj "function cannot operate on open file handle", obj (filePath file)]

gGetErrToExecError :: B.GGetErr -> ExecControl
gGetErrToExecError (B.GetErr{ B.gGetErrOffset=offset, B.gGetErrMsg=msg }) =
  mkExecError
  { execReturnValue = Just $ obj $
      [obj "failed decoding binary file", obj msg, obj "offset", obj (toInteger offset)]
  }

loadLibrary_File :: DaoSetup
loadLibrary_File = do
  let fileOpener func mode = daoFunction func $
        daoFunc
        { funcAutoDerefParams = True
        , daoForeignFunc = \ () ->
            _paramPath func >=> fmap (flip (,) () . Just . obj) . flip (_openFile func . flip File Nothing) mode
        }
  fileOpener "readFile"   ReadMode
  fileOpener "writeFile"  WriteMode
  fileOpener "appendFile" AppendMode
  daoFunction "File" $
    daoFunc
    { funcAutoDerefParams = True
    , daoForeignFunc = \ () -> fmap (flip (,) () . Just . obj . flip File Nothing) . _paramPath "File"
    }

instance ObjectClass File where { obj=new; fromObj=objFromHata; }

instance HataClass File where
  haskellDataInterface = interface (File nil Nothing) $ do
    autoDefEquality >> autoDefOrdering >> autoDefNullTest
    autoDefPPrinter >> autoDefBinaryFmt
    let fileOpener func mode = defMethod func $
          daoFunc
          { funcAutoDerefParams = False
          , daoForeignFunc = \file ox ->
              case ox of
                [] -> do
                  _withClosedHandle func file
                  f <- _openFile func file mode
                  return (Just $ obj f, f)
                _  -> execThrow $ obj [obj func, obj "method must be passed no parameters"]
          }
    fileOpener "openRead"   ReadMode
    fileOpener "openWrite"  WriteMode
    fileOpener "openAppend" AppendMode
    defMethod "close" $
      daoFunc
      { funcAutoDerefParams = False
      , daoForeignFunc = \file ox -> fmap (flip (,) (file{ fileHandle=Nothing })) $ do
          case ox of
            [] -> _getHandle "close" file >>=
              _catchIOException "close" file . liftIO . hClose >> return Nothing
            _  -> execThrow $ obj $
              [obj "close", obj file, obj "function must be passed no parameters"]
      }
    defMethod "writeBinary" $
      daoFunc
      { funcAutoDerefParams = True
      , daoForeignFunc = \file ox -> do
          mtab  <- gets globalMethodTable
          handl <- _getHandle "writeBinary" file
          forM_ ox $ \o -> do
            bin <- execCatchIO (liftIO $ return (B.encode mtab o)) $
              [ newExecIOHandler $ \e -> execThrow $ obj $
                  [ obj "in function", obj "writeBinary", obj file
                  , obj "while encoding object", obj (show (e::ErrorCall))
                  ]
              ]
            _catchIOException "writeBinary" file (B.hPutStr handl bin)
          return (Nothing, file)
      }
    defMethod "readBinary" $
      daoFunc
      { funcAutoDerefParams = True
      , daoForeignFunc = \file ox -> case ox of
          [] -> do
            _withClosedHandle "readBinary" file
            mtab <- gets globalMethodTable
            bin <- _catchIOException "readBinary" file (liftIO $ B.readFile $ uchars $ filePath file)
            result <- execCatchIO (return $ fmapPFail gGetErrToExecError $ B.decode mtab bin) $
              [ newExecIOHandler $ \e -> execThrow $ obj $
                  [ obj "in function", obj "readBinary"
                  , obj "while decoding binary data", obj (show (e::ErrorCall))
                  ]
              ]
            predicate result
          _ -> execThrow $ obj [obj "readBinary", obj file, obj "function must be passed no parameter"]
      }
    let writeFunc func putstr = defMethod func $
          daoFunc
          { funcAutoDerefParams = True
          , daoForeignFunc = \file ox -> do
              ox <- requireAllStringArgs func ox
              handl <- _getHandle func file
              forM_ ox $ _catchIOException "write" file . putstr handl . uchars
              return (Nothing, file)
          }
    writeFunc "write" hPutStr
    defMethod "read" $
      daoFunc
      { funcAutoDerefParams = True
      , daoForeignFunc = \file ox -> case ox of
          [] -> do
            _withClosedHandle "read" file
            _catchIOException "read" file (flip (,) file . Just . obj <$> readFile (uchars $ filePath file))
          _ -> execThrow $ obj [obj "read", obj file, obj "function must be passed no parameter"]
      }
    writeFunc "writeLine" hPutStrLn
    defMethod "readLine" $
      daoFunc
      { funcAutoDerefParams = True
      , daoForeignFunc = \file ox -> case ox of
          [] -> do
            handl <- _getHandle "readLine" file
            _catchIOException "readLine" file (flip (,) file . Just . obj <$> hGetLine handl)
          _  -> execThrow $ obj $
            [obj "readLine", obj file, obj "function must be passed no parameters"]
      }
    let defPrinter func print = defMethod func $
          makePrintFunc $ \file str -> _getHandle func file >>= \h -> liftIO (print h str)
    defPrinter "print"   hPutStr
    defPrinter "println" hPutStrLn

