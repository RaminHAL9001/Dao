-- | This is the main module to import into your project when you want to make use of the Dao
-- database kernel. It exports all of "Language.Interpreter.Dao.Kernel" along with serveral
-- functions for defining databases.
module Language.Interpreter.Dao
  ( -- * Setting up the Database Session
    Session, SessionState(..), DB_URL, URLConnector, localFileConnector, sessionState,
    runSession, onDatabase,
    -- * Creating a New Database
    newDatabase, newDatabaseSized, loadDatabase, storeDatabase,
    StoreBinaryFormat, storedBinary, storedText,
    SizePowerOf2, maxSizePowerOf2, minSizePowerOf2, defaultSizePowerOf2,
    -- * Working on Databases
    Database, DB, dbURL, dbIsStored, dbIsBinary, dbRuleCount, dbTotalWeight,
    -- * Logging
    LogMessage(..),
    LogLevel(..), LoggingFunction, newStderrLogger, newStringStderrLogger,
    module Language.Interpreter.Dao.Kernel,
  )
  where

import           Language.Interpreter.Dao.Kernel

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Array.IO
import           Data.Bits
import           Data.Char
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import qualified Data.Text.Lazy.IO  as Lazy
--import           Data.Typeable

import           System.IO

----------------------------------------------------------------------------------------------------

-- | Database universal resource locators are stored as a plain 'Strict.Text'.
type DB_URL = Strict.Text

-- | A function type for opening a 'System.IO.Handle' given a 'DB_URL'.
type URLConnector = DB_URL -> IOMode -> IO Handle

localFileConnector :: URLConnector
localFileConnector = openFile . Strict.unpack

-- | An integer power of 2 used to define the in-memory size of a 'Database'.
type SizePowerOf2 = Int

-- | A flag indicating the whether storage format should be binary. If it isn't binary, it is UTF-8
-- text.
type StoreBinaryFormat = Bool

storedBinary :: StoreBinaryFormat
storedBinary = True

storedText :: StoreBinaryFormat
storedText = False

-- | The 'SizePowerOf2' has hard coded limit of 22, which is an upper bound of about 4 million
-- rules. On 64-bit computer hardware an array big enough to hold 4 million rules would require a
-- contiguous block of memory 256 mebibytes large (1/4th of a gibibyte), and this does not include
-- the space required to hold the actual rules themselves, this is just the space for the index to
-- all rules.
--
-- But keep in mind that a database with that many rules would take a consider amount of computing
-- power to execute even a single query, it would be much better to split databases that large into
-- smaller ones spread across multiple computing resources and execute queries in parallel.
maxSizePowerOf2 :: SizePowerOf2
maxSizePowerOf2 = 22

-- | The minimum 'SizePowerOf2' has a hard coded limit of 9, which is a lower bound of 512 rules. On
-- 64-bit computer hardware an array big enough to hold 512 rules requires a contiguous block of
-- memory 4096 bytes in size on 64-bit computer hardware.
minSizePowerOf2 :: SizePowerOf2
minSizePowerOf2 = 9

-- | Used by the 'newDatabase' function to call the 'newDatabaseSized' function with a default size
-- value. The default size is @2 ^ 13 == 8192@ rules, which requires a contiguous block of memory 64
-- kibibytes in size on 64-bit computer hardware.
defaultSizePowerOf2 :: SizePowerOf2
defaultSizePowerOf2 = 13

-- integer logarithm base 2
ilog2 :: Int -> Int
ilog2 = loop 0 where
  loop i x = seq i $! seq x $! if x == 0 then i else (loop $! i + 1) $! shiftR x 1

----------------------------------------------------------------------------------------------------

class Monad m => LogMessage m where
  logMessage :: LogLevel -> Strict.Text -> m ()

-- | A simple tag to add type information to logging messages.
data LogLevel
  = INFO
    -- ^ Used to record changes that occur normally during operation of the database.
  | WARN
    -- ^ Used to indicate errors that were corrected without halting normal operation of the
    -- database. This is usually cause by parameters that can be corrected with reasonable default
    -- values.
  | FAIL
    -- ^ Used to record a failure of ordinary database operation that requires user intervention.
    -- This include things like loading a corrupted database file (where checksums do not match).
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type LoggingFunction r = Report -> IO r

-- | Construct a unique 'LoggingFunction' that can be used to log error messages to the
-- 'System.IO.stderr' file handle. This function constructs a lock to ensure multiple threads using
-- this logger will not cause race conditions while writing messages to 'System.IO.stderr', but this
-- also means you must __only__ call this function once throughout the execution of your program.
newStderrLogger :: IO (LoggingFunction ())
newStderrLogger = newStringStderrLogger $ \ report -> return $
  ('[' :) . showsPrec 0 (reportLogLevel report) . ("] " ++) . showsPrec 0 report $ ""

-- | Similar to 'newStderrLogger' but allows you to pass your own logging function that returns a
-- 'Prelude.String' which is then written to 'System.IO.stderr'.
newStringStderrLogger :: LoggingFunction String -> IO (LoggingFunction ())
newStringStderrLogger log = do
  lock <- newMVar stderr
  return $ \ report -> modifyMVar_ lock $ \ stderr ->
    log report >>= hPutStrLn stderr >> return stderr

----------------------------------------------------------------------------------------------------

-- | Reports are sent to the logging function. If the 'LogLevel' of a 'Report' is 'FAIL', it is
-- usually 'Control.Exception.throw'n as well, usually after some clean-up has been performed.
data Report
  = Dao_locked_database   ThreadId
  | Dao_unlocked_database ThreadId
  | Dao_new_db_size_param ThreadId SizePowerOf2
  | Dao_bad_magic_number  ThreadId DB_URL
  | Dao_bad_header_size   ThreadId DB_URL Int
  | Dao_db_parser_error   ThreadId DB_URL
  | Dao_db_duplicate_name ThreadId DB_URL Atom
  | Dao_db_parsed_success ThreadId DB_URL
  | Dao_session_fail      ThreadId Strict.Text
  | Dao_db_fail           ThreadId DB_URL Strict.Text
  | Dao_db_report         ThreadId LogLevel (Maybe DB_URL) Strict.Text
  deriving (Eq, Ord, Show)

instance Exception Report where {}

reportLogLevel :: Report -> LogLevel
reportLogLevel = \ case
  Dao_locked_database   {} -> INFO
  Dao_unlocked_database {} -> INFO
  Dao_new_db_size_param {} -> WARN
  Dao_bad_magic_number  {} -> FAIL
  Dao_bad_header_size   {} -> FAIL
  Dao_db_parser_error   {} -> FAIL
  Dao_db_duplicate_name {} -> FAIL
  Dao_db_parsed_success {} -> INFO
  Dao_session_fail      {} -> FAIL
  Dao_db_fail           {} -> FAIL
  Dao_db_report _ lvl _ _  -> lvl

----------------------------------------------------------------------------------------------------

-- | This is the type of function that performs opreations within production rule database session.
-- The session manages which databases are loaded into memory and allows you select a 'Database'
-- against which you may execute 'DBIO' type functions.
--
-- Logging facilities are also provided.
newtype Session a = Session (StateT SessionState IO a)
  deriving (Functor, Applicative, Monad)

instance MonadFail Session where
  fail msg = Session $ gets loggingFunction >>= \ logger -> liftIO $ do
    thid <- myThreadId
    let err = Dao_session_fail thid $ Strict.pack msg
    logger err >> throw err

instance LogMessage Session where
  logMessage level msg = Session $ do
    logger <- gets loggingFunction
    liftIO $ do
      thid <- myThreadId
      logger $ Dao_db_report thid level Nothing msg

-- | Declare this data type in the @main@ function of your project using the 'sessionState'
-- function, and pass it to the 'runSession' function at any point in your program where you need to
-- execute a Dao function.
data SessionState
  = SessionState
    { urlConnector    :: URLConnector
    , loggingFunction :: LoggingFunction ()
    }

-- | Construct a new 'SessionState'. The only parameter you need to supply is the 'LoggingFunction'.
sessionState :: URLConnector -> LoggingFunction () -> SessionState
sessionState connector logger = SessionState
  { urlConnector    = connector
  , loggingFunction = logger
  }

-- | This function will usually be evaluated in the @main@ function of your program. It initializes
-- a database session with the 'SessionState' given to it and runs synchronously in the current
-- thread until the session exits. 'SessionState's may not be preserved between calls to this
-- function, as can be plainly seen by the return type of this function which does not return a
-- 'SessionState' to be used for later calls to 'runSession'.
runSession :: SessionState -> Session a -> IO a
runSession st (Session f) = evalStateT f st

-- | This function runs a 'DB' function on a given 'DB_URL'. The 'DB_URL' must have been loaded by
-- 'loadDatabase' or created anew from 'newDatabase' before this function may be called. If the
-- Database does not exist prior to this call, a 'FAIL' 'logMessage' is reported, and
-- 'Prelude.Nothing' is returned. Otherwise, an 'INFO' 'logMessage' is reported and the return value
-- of the given 'DB' function is returned wrapepd in a 'Prelude.Just' constructor.
onDatabase :: Database -> DB a -> Session a
onDatabase (Database lock) (DB f) = Session $ do
  st <- get 
  liftIO $ modifyMVar lock $ \ db -> do
    thid <- myThreadId
    loggingFunction st $ Dao_locked_database thid
    catch (liftM (\ (a, db) -> (db, a)) $ runSession st $ runStateT f db)
      (\ (SomeException err) -> do
          loggingFunction st $ Dao_unlocked_database thid
          throw err
      )

-- | Calls 'newDatabaseSized' with the 'defaultSizePowerOf2' value.
newDatabase :: StoreBinaryFormat -> Session Database
newDatabase = newDatabaseSized defaultSizePowerOf2

-- | Construct a new empty database with a preset size for elements, where the size is expressed as
-- a power of 2. For example, to create a database where you expect fewer than 4096 rules to be
-- stored into it, you should pass a value of 12 as the 'SizePowerOf2' since @4096 == 2 ^ 12@.
--
-- The 'SizePowerOf2' has hard-coded limit of 22, which is a limit of about 4 million rules, and on
-- 64-bit computer hardware an array big enough to hold 4 million rules would require a contiguous
-- block of memory 256 mebibytes large (1/4th of a gibibyte), and this does not include the space
-- required to hold the actual rules themselves, this is just the space for the index to all rules.
--
-- Passing a 'SizePowerOf2' smaller than 'minSizePowerOf2' or larger than 'maxSizePowerOf2' will
-- result in a log 'WARN'-ning called on the given 'LoggingFunction'.
--
-- The 'Database' created is in-memory only until you store it to the given URL (usually a file in
-- the local filesystem) with "dbStoreToURL" function.
newDatabaseSized :: SizePowerOf2 -> StoreBinaryFormat -> Session Database
newDatabaseSized size = _newDB size >=> liftM Database . Session . liftIO . newMVar

_newDB :: SizePowerOf2 -> StoreBinaryFormat -> Session LoadedDB
_newDB size bin = Session $ do
  logger <- gets loggingFunction
  liftIO $ do
    thid  <- myThreadId
    unless (minSizePowerOf2 < size && size < maxSizePowerOf2) $
      logger $ Dao_new_db_size_param thid size
    size  <- pure $ min maxSizePowerOf2 $ max minSizePowerOf2 size
    table <- newArray_ (0, 2 ^ size - 1)
    return LoadedDB
      { dbURL         = ""
      , dbIsStored    = False
      , dbIsBinary    = bin
      , dbShbang      = ""
      , dbRuleCount   = 0
      , dbTotalWeight = 0.0
      , dbNamedExprs  = Map.empty
      , dbIOArray     = table
      }

-- | This function loads a 'Database' from a given 'DB_URL'. The 'DB_URL' must exist and be a valid
-- Dao production rule database, in either binary or text format. A 'logMessage' is produced
-- indicating success or failure, and a 'Prelude.Bool'-ean value is returned indicating success
-- or failure.
loadDatabase :: DB_URL -> Session Database
loadDatabase url = Session $ do
  st <- get
  let logger = loggingFunction st
  let connector = urlConnector st
  liftIO $ do
    thid     <- myThreadId
    handle   <- connector url ReadMode
    initLine <- Lazy.toStrict . Lazy.dropWhile isSpace <$> Lazy.hGetLine handle
    (shbang, initLine) <- case dropWhile isSpace $ Strict.unpack initLine of
      '#' : _ -> (,) initLine . Lazy.toStrict . Lazy.dropWhile isSpace <$> Lazy.hGetLine handle
      _       -> return ("", initLine)
    let fail = let err = Dao_bad_magic_number thid url in logger err >> throw err
    case span isAlphaNum $ dropWhile isSpace $ Strict.unpack initLine of
      ('d':'a':'o':'D':'B':' ':"", more) -> do
        case span isAlphaNum more of
          (size, "") -> case readsPrec 0 size of
            [(size, "")] -> do
              db <- (\ db -> db{ dbURL = url, dbShbang = shbang }) <$>
                runSession st (_newDB (ilog2 size + 1) storedText) 
              dbmvar <- newMVar db
              let dbHandle = Database dbmvar
              let arr = dbIOArray db
              let badsize = let err = Dao_bad_header_size thid url size in logger err >> throw err
              let loop dict total i str = if total > size then badsize else
                    if null str
                     then if total /= size then badsize else do
                      hClose handle
                      logger $ Dao_db_parsed_success thid url
                      modifyMVar_ dbmvar $ \ db -> return db{ dbNamedExprs = dict }
                      return dbHandle
                     else case readsPrec 0 str of
                        [(expr, str)] -> case expr of
                          NamedEntry nm form -> case Map.lookup nm dict of
                            Nothing -> ((loop $! Map.insert nm form dict) $! total + 1) i str
                            Just {} -> do
                              let err = Dao_db_duplicate_name thid url nm
                              logger err >> throw err
                          UnnamedEntry  rule -> do
                            writeArray arr i rule
                            ((loop dict $! total + 1) $! i + 1) str
                        _             -> do
                          let err = Dao_db_parser_error thid url
                          logger err >> throw err
              hGetContents handle >>= loop Map.empty 0 0
            _           -> fail
          _           -> fail
      _ -> fail

storeDatabase :: DB_URL -> Database -> Session ()
storeDatabase url (Database lock) = Session $ do
  st <- get
  liftIO $ modifyMVar_ lock $ \ db -> do
    handle <- urlConnector st url WriteMode
    let shbang = dbShbang db
    unless (Strict.null shbang) $ Lazy.hPutStrLn handle $ Lazy.fromStrict shbang
    hPutStrLn handle $ "daoDB " ++ show (dbRuleCount db)
    let arr = dbIOArray db
    (lo, hi) <- getBounds arr
    let loop i = if i > hi then return () else
          readArray arr i >>= hPrint handle >> hPutStrLn handle ";" >> (loop $! i + 1)
    loop lo >> hFlush handle >> hClose handle
    return $ db{ dbIsStored = True }

----------------------------------------------------------------------------------------------------

-- | This is a Dao production rule database. This data type is usually associated with a database
-- stored in a file on disk or received from over network.
newtype Database = Database (MVar LoadedDB)
  deriving Eq

-- | This is the data structure representing the state of an open database connection.
data LoadedDB
  = LoadedDB
    { dbURL         :: DB_URL
      -- ^ The URL to which this database has been, or will be, bound.
    , dbIsStored    :: Bool
      -- ^ Indicates whether all updates to this 'Database' have been stored to it's 'dbURL' or not.
      -- If it is not stored, there are records that have been added that are in memory only which
      -- will be lost if the program halts.
    , dbIsBinary    :: StoreBinaryFormat
      -- ^ Indicates whether this 'Database' should be stored in the binary format, rather than
      -- UTF-8 text format.
    , dbShbang      :: Strict.Text
    , dbRuleCount   :: Int
      -- ^ The number of rules in this database.
    , dbTotalWeight :: Double
      -- ^ The sum total of the weight of all the rules in this database. Normalizing the rule
      -- weights in the database is a process of scaling all weights such that this total value is
      -- "exactly" equal to @1.0@ -- "exactly" being limited to tolerances of the IEEE-754
      -- 'Prelude.Double' percision floating point arithmetic, of course.
    , dbNamedExprs  :: Map.Map Atom Form
      -- ^ This is the table of named 'DaoExpr's in the module.
    , dbIOArray     :: IOArray Int Rule
      -- ^ This is the actual array containing the production rules. The actual size of the array is
      -- some even power of 2 so that it is not necessary to re-allocate the entire array every time
      -- a new production rule is added.
    }

-- | This is the function type used to perform operations on individual 'Database's.
newtype DB a = DB (StateT LoadedDB Session a)
  deriving (Functor, Applicative, Monad)

instance MonadFail DB where
  fail msg = DB $ do
    url <- gets dbURL
    lift $ Session $ do
      logger <- gets $ loggingFunction
      liftIO $ do
        thid <- myThreadId
        let err = Dao_db_fail thid url $ Strict.pack msg
        logger err >> throw err

instance LogMessage DB where
  logMessage level msg = DB $ do
    url <- gets dbURL
    lift $ Session $ do
      logger <- gets loggingFunction
      liftIO $ do
        thid <- myThreadId
        logger $ Dao_db_report thid level (Just url) msg




