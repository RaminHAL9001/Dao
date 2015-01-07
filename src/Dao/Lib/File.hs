-- "src/Dao/Lib/File.hs"  built-in plain file object
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
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
import qualified Data.Map.Lazy        as M
import           Data.Typeable

import           Control.Applicative
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

errFilePath :: Name
errFilePath = ustr "filePath"

_openParamFail :: String -> [Object] -> Exec ig
_openParamFail func ox =
  throwArityError "expecting a file path as the only parameter" 1 ox [(errInFunc, obj (ustr func :: Name))]

_paramPath :: String -> [Object] -> Exec UStr
_paramPath func ox = case ox of
  [path] -> xmaybe (fromObj path <|> (filePath <$> fromObj path)) <|> _openParamFail func ox
  ox     -> _openParamFail func ox

_catchIOException :: String -> File -> IO a -> Exec a
_catchIOException func file f = execCatchIO (liftIO f) $
  [ newExecIOHandler $ \e -> execThrow "" (ExecIOException e) $
      [(errInFunc, obj (ustr func :: Name)), (errFilePath, obj $ filePath file)]
  ]

_openFile :: String -> File -> IOMode -> Exec File
_openFile func file mode = _catchIOException func file $
  (\o -> file{ fileHandle=Just o}) <$> openFile (uchars $ filePath file) mode

_getHandle :: String -> File -> Exec Handle
_getHandle func file = case fileHandle file of
  Just  h -> return h
  Nothing ->
    execThrow "function evaluated on a file handle which has not been opened" ExecErrorUntyped
      [(errInFunc, obj (ustr func :: Name)), (errFilePath, obj $ filePath file)]

_withClosedHandle :: String -> File -> Exec ()
_withClosedHandle func file = case fileHandle file of
  Nothing -> return ()
  Just  _ -> execThrow "function cannot operate on open file handle" ExecErrorUntyped
    [(errInFunc, obj (ustr func :: Name)), (errFilePath, obj $ filePath file)]

_withContents :: (String -> IO Object) -> DaoFunc File
_withContents f =
  daoFunc
  { daoForeignFunc = \file ox -> case ox of
      [] -> do
        _withClosedHandle "read" file
        _catchIOException "read" file $ fmap (flip (,) file . Just) $ readFile (uchars $ filePath file) >>= f
      ox -> throwArityError "" 0 ox [(errInFunc, obj (ustr "read" :: Name))]
  }

gGetErrToExecError :: B.GGetErr -> ExecControl
gGetErrToExecError (B.GetErr{ B.gGetErrOffset=offset, B.gGetErrMsg=msg }) =
  newError
  { execErrorMessage = msg
  , execErrorInfo    = M.fromList [(ustr "byteOffset", OLong (toInteger offset))]
  }

loadLibrary_File :: DaoSetup
loadLibrary_File = do
  let fileOpener func mode = daoFunction func $
        daoFunc
        { daoForeignFunc = \ () ->
            _paramPath func >=> fmap (flip (,) () . Just . obj) . flip (_openFile func . flip File Nothing) mode
        }
  fileOpener "readFile"   ReadMode
  fileOpener "writeFile"  WriteMode
  fileOpener "appendFile" AppendMode
  daoClass (haskellType::File)
  daoFunction "File" $
    daoFunc
    { daoForeignFunc = \ () -> fmap (flip (,) () . Just . obj . flip File Nothing) . _paramPath "File"
    }

instance ObjectClass File where { obj=new; fromObj=objFromHata; }

instance HataClass File where
  haskellDataInterface = interface "File" $ do
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
                ox -> throwArityError "" 0 ox [(errInFunc, obj (ustr "File" :: Name))]
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
            ox -> throwArityError "" 0 ox [(errInFunc, obj (ustr "close" :: Name))]
      }
    defMethod "writeBinary" $
      daoFunc
      { daoForeignFunc = \file ox -> do
          mtab  <- gets globalMethodTable
          handl <- _getHandle "writeBinary" file
          forM_ ox $ \o -> do
            bin <- execCatchIO (liftIO $ return (B.encode mtab o)) $
              [ newExecIOHandler $ \e -> execThrow "while encoding object" (ExecHaskellError e) $
                  [ (errInFunc, obj (ustr "writeBinary" :: Name))
                  , (errFilePath, obj $ filePath file)
                  ]
              ]
            _catchIOException "writeBinary" file (B.hPutStr handl bin)
          return (Nothing, file)
      }
    defMethod "readBinary" $
      daoFunc
      { daoForeignFunc = \file ox -> case ox of
          [] -> do
            _withClosedHandle "readBinary" file
            mtab   <- gets globalMethodTable
            bin    <- _catchIOException "readBinary" file (liftIO $ B.readFile $ uchars $ filePath file)
            result <- execCatchIO (return $ fmapPFail gGetErrToExecError $ B.decode mtab bin) $
              [ newExecIOHandler $ \e -> execThrow "while decoding object" (ExecHaskellError e) $
                  [(errInFunc, obj (ustr "readBinary" :: Name)), (errFilePath, obj $ filePath file)]
              ]
            predicate result
          ox -> throwArityError "" 0 ox [(errInFunc, obj (ustr "readBinary" :: Name))]
      }
    let writeFunc func putstr = defMethod func $
          daoFunc
          { daoForeignFunc = \file ox -> do
              ox <- requireAllStringArgs func ox
              handl <- _getHandle func file
              forM_ ox $ _catchIOException "write" file . putstr handl . uchars
              return (Nothing, file)
          }
    writeFunc "write" hPutStr
    defMethod "read" $ _withContents (return . obj)
    defMethod "readAllLines" $ _withContents (return . obj . fmap obj . lines)
    writeFunc "writeLine" hPutStrLn
    defMethod "readLine" $
      daoFunc
      { daoForeignFunc = \file ox -> case ox of
          [] -> do
            handl <- _getHandle "readLine" file
            _catchIOException "readLine" file (flip (,) file . Just . obj <$> hGetLine handl)
          ox -> throwArityError "" 0 ox [(errInFunc, obj (ustr "readLine" :: Name))]
      }
    let defPrinter func print = defMethod func $
          makePrintFunc $ \file str -> _getHandle func file >>= \h -> liftIO (print h str)
    defPrinter "print"   hPutStr
    defPrinter "println" hPutStrLn

