-- | This is the main module to import into your project when you want to make use of the Dao
-- database kernel. It exports all of "Language.Interpreter.Dao.Kernel" along with serveral
-- functions for defining databases.
--
-- The database parser is defined in this module, although it mostly just parses the header before
-- launching into a loop that parses as many 'Language.Interpreter.Dao.Kernel.DBEntry's.
module Language.Interpreter.Dao.Database
  ( -- * Setting up the Database Session
    Session, SessionState(..), DB_URL, URLConnector, sessionState, localFileConnector,
    runSession, onDatabase,
    -- * Creating a New Database
    newDatabase, newDatabaseSized, loadDatabase, storeDatabase,
    StoreBinaryFormat, storedBinary, storedText,
    SizePowerOf2, maxSizePowerOf2, minSizePowerOf2, defaultSizePowerOf2,
    -- * Working on Databases
    Database, DB, dbNameExpr, dbLookup, dbUpdatRules, dbQuery,
    -- * Information About a Database
    DBParams, dbGetParam, dbSetParam, dbURL, dbIsBinary, dbShbang,
    LoadedDB, dbParams, dbNewParams, dbRuleCount, dbTotalWeight, dbEntriesUpdated, dbNamedExprs,
    dbParamsUpdated, dbIsModified, dbNewRule,
    -- * Logging
    LogMessage(..),
    LogLevel(..), LoggingFunction, newStderrLogger, newStringStderrLogger,
    module Language.Interpreter.Dao.Kernel,
  )
  where

import           Language.Interpreter.Dao.Kernel

import           Control.Arrow
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Array.IO
import           Data.Bits
import           Data.Char
import           Data.List.NonEmpty (toList)
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import qualified Data.Text.Lazy.IO  as Lazy
--import           Data.Typeable

import           Lens.Micro
import           Lens.Micro.Mtl

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
  | Dao_session_fail      ThreadId Strict.Text
  | Dao_session_report    ThreadId LogLevel Strict.Text
  | Dao_bad_magic_number  ThreadId DB_URL
  | Dao_bad_header_size   ThreadId DB_URL Int
  | Dao_db_parser_error   ThreadId DB_URL
  | Dao_db_duplicate_name ThreadId DB_URL Atom
  | Dao_db_parsed_success ThreadId DB_URL
  | Dao_db_resolved_name  ThreadId DB_URL Atom DaoExpr
  | Dao_db_created_name   ThreadId DB_URL Atom DaoExpr
  | Dao_db_overwrite_name ThreadId DB_URL Atom DaoExpr DaoExpr
  | Dao_db_bad_name       ThreadId DB_URL Atom
  | Dao_db_store          ThreadId DB_URL
  | Dao_db_already_stored ThreadId DB_URL
  | Dao_db_fail           ThreadId DB_URL Strict.Text
  | Dao_db_report         ThreadId DB_URL LogLevel Strict.Text
  deriving (Eq, Ord, Show)

instance Exception Report where {}

reportLogLevel :: Report -> LogLevel
reportLogLevel = \ case
  Dao_locked_database   {}     -> INFO
  Dao_unlocked_database {}     -> INFO
  Dao_new_db_size_param {}     -> WARN
  Dao_session_fail      {}     -> FAIL
  Dao_session_report _ lvl _   -> lvl
  Dao_bad_magic_number  {}     -> FAIL
  Dao_bad_header_size   {}     -> FAIL
  Dao_db_parser_error   {}     -> FAIL
  Dao_db_duplicate_name {}     -> FAIL
  Dao_db_parsed_success {}     -> INFO
  Dao_db_resolved_name  {}     -> INFO
  Dao_db_created_name   {}     -> INFO
  Dao_db_overwrite_name {}     -> WARN
  Dao_db_bad_name       {}     -> FAIL
  Dao_db_store          {}     -> INFO
  Dao_db_already_stored {}     -> WARN
  Dao_db_fail           {}     -> FAIL
  Dao_db_report     _ _ lvl _  -> lvl

----------------------------------------------------------------------------------------------------

-- | This is the type of function that performs opreations within production rule database session.
-- The session manages which databases are loaded into memory and allows you select a 'Database'
-- against which you may execute 'DB' type functions.
--
-- Logging facilities are also provided.
newtype Session a = Session (StateT SessionState IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadFail Session where
  fail msg = Session $ gets loggingFunction >>= \ logger -> liftIO $ do
    thid <- myThreadId
    let err = Dao_session_fail thid $ Strict.pack msg
    logger err >> throw err

instance LogMessage Session where
  logMessage lvl msg = Session $ do
    logger <- gets loggingFunction
    liftIO $ do
      thid <- myThreadId
      logger $ Dao_session_report thid lvl msg

-- | Declare this data type in the @main@ function of your project, and pass it to the 'runSession'
-- function at any point in your program where you need to execute a Dao function. It is usually
-- better to use Haskell record syntax to override the default values provided in the 'sessionState'
-- function.
data SessionState
  = SessionState
    { urlConnector       :: URLConnector
      -- ^ This is a function you must supply which can translate 'DB_URL's into 'System.IO.Handle's
      -- that can be read and parsed as Dao 'Database's. The 'localFileConnector' is a good default
      -- value for this parameter.
    , loggingFunction    :: LoggingFunction ()
      -- ^ Supply a custom logging function. The easiest way to do this is to call 'newStderrLogger'
      -- and the return value of that function can be supplied as this parameter.
    , sessionEnvironment :: Environment
      -- ^ Supply a 'Language.Interpreter.Dao.Kernel.Environment' for evaluating pattern matches and
      -- Built-In Functions (BIFs). 'Language.Interpreter.Dao.Environment.defaultEnvironment' is a
      -- good default value for this parameter.
    }

-- | This is the default 'SessionState'. Pass this to the 'runSession' function, unless you want to
-- customize the 'SessionState', in which case override the parameters of this 'SessionState' data
-- constructor using Haskell's record syntax.
sessionState :: SessionState
sessionState = SessionState
  { urlConnector = localFileConnector
  , loggingFunction = \ msg -> if reportLogLevel msg < WARN then return () else
      hPrint stderr msg >> hFlush stderr >>= evaluate
  , sessionEnvironment = environment
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
    let params = DBParams
          { __dbIsBinary = bin
          , __dbShbang   = ""
          , __dbURL      = ""
          }
    return LoadedDB
      { __dbParams         = params
      , __dbNewParams      = params
      , __dbEntriesUpdated = False
      , __dbRuleCount      = 0
      , __dbTotalWeight    = 0.0
      , __dbNamedExprs     = Map.empty
      , __dbIOArray        = table
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
              db <- runSession st (_newDB (ilog2 size) storedText) <&>
                _dbParams %~ (dbURL .~ url) . (dbShbang .~ shbang)
              let arr = db ^. _dbIOArray 
              let badsize = let err = Dao_bad_header_size thid url size in logger err >> throw err
              let loop dict total i str = if total > size then badsize else
                    if null str
                     then if total /= size then badsize else do
                      hClose handle
                      logger $ Dao_db_parsed_success thid url
                      liftM Database $ newMVar $ db & _dbNamedExprs .~ dict
                     else case readsPrec 0 str of
                        [(expr, str)] -> case expr of
                          NamedEntry nm expr -> case Map.lookup nm dict of
                            Nothing -> case str of
                              ';':str -> ((loop $! Map.insert nm expr dict) $! total + 1) i str
                              _       -> do
                                let err = Dao_db_parser_error thid url
                                logger err >> throw err
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

-- | Commit any changes made durring this session to the Database to storage.
storeDatabase :: Database -> Session ()
storeDatabase (Database lock) = Session $ do
  st <- get
  liftIO $ modifyMVar_ lock $ \ db -> do
    thid <- myThreadId
    let url = db ^. dbNewParams . dbURL
    if not $ db ^. dbIsModified
     then loggingFunction st (Dao_db_already_stored thid url) >> return db
     else do
      loggingFunction st $ Dao_db_store thid url
      handle <- urlConnector st url WriteMode
      let shbang = db ^. dbNewParams . dbShbang
      unless (Strict.null shbang) $ Lazy.hPutStrLn handle $ Lazy.fromStrict shbang
      hPutStrLn handle $ "daoDB " ++ show (db ^. dbRuleCount)
      let arr = db ^. _dbIOArray
      (lo, hi) <- getBounds arr
      let loop i = if i > hi then return () else
            readArray arr i >>= hPrint handle >> hPutStrLn handle ";" >> (loop $! i + 1)
      loop lo >> hFlush handle >> hClose handle
      return $ db & (_dbEntriesUpdated .~ False) . (_dbParams .~ (db ^. dbNewParams))

----------------------------------------------------------------------------------------------------

-- | This is a Dao production rule database. This data type is usually associated with a database
-- stored in a file on disk or received from over network.
newtype Database = Database (MVar LoadedDB)
  deriving Eq

-- | This is the data structure representing the state of an open database connection.
data LoadedDB
  = LoadedDB
    { __dbParams         :: DBParams
    , __dbNewParams      :: DBParams
    , __dbEntriesUpdated :: Bool
    , __dbNamedExprs     :: Map.Map Atom DaoExpr
    , __dbTotalWeight    :: Double
    , __dbRuleCount      :: Int
    , __dbIOArray        :: IOArray Int Rule
      -- ^ This is the actual array containing the production rules. The actual size of the array is
      -- some even power of 2 so that it is not necessary to re-allocate the entire array every time
      -- a new production rule is added.
    }

_dbParams :: Lens' LoadedDB DBParams
_dbParams = lens __dbParams $ \ db a -> db{ __dbParams = a }

-- | A lens to lookup the current 'DBParams' which describe how a 'Database' is stored.
dbParams :: SimpleGetter LoadedDB DBParams
dbParams = to __dbParams

-- | A lens to lookup or updated the 'DBParams' which have been modified but not committed to a
-- data store.
dbNewParams :: Lens' LoadedDB DBParams
dbNewParams = lens __dbNewParams $ \ db a -> db{ __dbNewParams = a }

_dbEntriesUpdated :: Lens' LoadedDB Bool
_dbEntriesUpdated = lens __dbEntriesUpdated $ \ db a -> db{ __dbEntriesUpdated = a }

-- | This is set to 'Prelude.True' when elements from 'dbNamedExprs' or the 'dbIOArray' has been
-- modified in some way.
dbEntriesUpdated :: SimpleGetter LoadedDB Bool
dbEntriesUpdated = to __dbEntriesUpdated

-- | This is the table of named 'DaoExpr's in the module.
dbNamedExprs :: SimpleGetter LoadedDB (Map.Map Atom DaoExpr)
dbNamedExprs = to __dbNamedExprs

-- | The sum total of the weight of all the rules in this database. Normalizing the rule weights in
-- the database is a process of scaling all weights such that this total value is "exactly" equal to
-- @1.0@ -- "exactly" being limited to tolerances of the IEEE-754 'Prelude.Double' percision
-- floating point arithmetic, of course.
dbTotalWeight :: SimpleGetter LoadedDB Double
dbTotalWeight = to __dbTotalWeight

-- | The number of rules in this database.
dbRuleCount :: SimpleGetter LoadedDB Int
dbRuleCount = to __dbRuleCount

_dbRuleCount :: Lens' LoadedDB Int
_dbRuleCount = lens __dbRuleCount $ \ db a -> db{ __dbRuleCount = a }

_dbIOArray :: Lens' LoadedDB (IOArray Int Rule)
_dbIOArray = lens __dbIOArray $ \ db a -> db{ __dbIOArray = a }

----------------------------------------------------------------------------------------------------

-- | These are parameters that may be modified, but are not committed to storage until a 'storeDB'
-- operation is evaluated.
data DBParams
  = DBParams
    { __dbIsBinary :: StoreBinaryFormat
    , __dbURL      :: DB_URL
    , __dbShbang   :: Strict.Text
    }
  deriving (Eq, Ord, Show, Read)

-- | Indicates whether this 'Database' should be stored in the binary format, rather than
-- UTF-8 text format.
dbIsBinary :: Lens' DBParams Bool
dbIsBinary = lens __dbIsBinary $ \ params a -> params{ __dbIsBinary = a }

-- | The URL to which this database has been, or will be, bound.
dbURL :: Lens' DBParams DB_URL
dbURL = lens __dbURL $ \ params a -> params{ __dbURL = a }

-- | If the database is not stored in binary format, it might have a "shbang" line as the very first
-- line in the file, which is a signal in UNIX-like systems for a file with it's executable flag set
-- to execute the file as a program using the interpreter program named in the shbang.
dbShbang :: Lens' DBParams DB_URL
dbShbang = lens __dbShbang $ \ params a -> params{ __dbShbang = a }

_dbNamedExprs :: Lens' LoadedDB (Map.Map Atom DaoExpr)
_dbNamedExprs = lens __dbNamedExprs $ \ db dict -> db{ __dbNamedExprs = dict }

-- | Unlike 'dbEntriesUpdated', this flag inidcates whether the parameters of the database, namely
-- the 'dbIsBinary', 'dbURL', or 'dbShbang' have been latered
dbParamsUpdated :: SimpleGetter LoadedDB Bool
dbParamsUpdated = to $ \ db -> __dbParams db == __dbNewParams db

-- | Is 'Prelude.True' if either 'dbParamsUpdated' or 'dbEntriesUpdated' are 'Prelude.True'.
dbIsModified :: SimpleGetter LoadedDB Bool
dbIsModified = to $ \ db -> db ^. dbParamsUpdated || db ^. dbEntriesUpdated

----------------------------------------------------------------------------------------------------

-- | This is the function type used to perform operations on individual 'Database's.
newtype DB a = DB { _unwrapDB :: StateT LoadedDB Session a }
  deriving (Functor, Applicative, Monad)

instance MonadFail DB where
  fail msg = _dblog Dao_db_fail $ \ log info -> do
    let err = info $ Strict.pack msg
    log err >> throw err

instance LogMessage DB where
  logMessage lvl msg = _dblog Dao_db_report $ \ log info -> log $ info lvl msg

_dblog :: (ThreadId -> DB_URL -> report) -> (LoggingFunction () -> report -> IO a) -> DB a
_dblog report f = DB $ do
  url <- use $ dbParams . dbURL
  lift $ Session $ do
    logger <- gets loggingFunction
    liftIO $ do
      thid <- myThreadId
      f logger $ report thid url

-- | This function takes one of the 'DBParams' 'Lens.Micro.Lens''s and uses the given function
-- @(a -> a)@ to modify one of the 'DBParam's for the current database.
dbSetParam :: ASetter' DBParams a -> (a -> a) -> DB ()
dbSetParam lens = DB . modifying (dbNewParams . lens)

-- | This function takes a 'Lens.Micro.Lens'' or a 'Lens.Micro.SimpleGetter' and retrieves one of
-- the 'DBParam's for the current database.
dbGetParam :: SimpleGetter DBParams a -> DB a
dbGetParam lens = DB $ use $ dbNewParams . lens

-- | Lookup a named entry in a 'Database' from the dictionary of 'dbNamedExprs'.
dbLookup :: Atom -> DB DaoExpr
dbLookup nm = do
  form <- DB $ use $ dbNamedExprs . to (Map.lookup nm)
  case form of
    Nothing   -> _dblog Dao_db_bad_name $ \ log -> ($ nm) >>> \ err -> log err >> throw err
    Just form -> _dblog Dao_db_resolved_name $ \ log info -> log (info nm form) >> return form

-- | Insert a named entry into the 'Database', overwriting any previously inserted named entries.
dbNameExpr :: Atom -> DaoExpr -> DB ()
dbNameExpr nm newexpr = do
  map <- DB $ use dbNamedExprs
  let doInsert = DB $ do
        _dbNamedExprs %= Map.insert nm newexpr
        _dbEntriesUpdated .= True
  case Map.lookup nm map of
    Nothing      -> do
      doInsert
      _dblog Dao_db_created_name $ \ log info -> log (info nm newexpr)
    Just oldexpr -> unless (oldexpr == newexpr) $ do
      _dblog Dao_db_overwrite_name $ \ log info -> log (info nm oldexpr newexpr)
      doInsert

-- | Add a new 'Language.Interpreter.Dao.Kernel.Rule' to the 'Database'.
dbNewRule :: Rule -> DB ()
dbNewRule rule = DB $ do
  arr   <- use _dbIOArray
  count <- use _dbRuleCount
  (lo, hi) <- liftIO $ getBounds arr
  let size = hi - lo + 1
  arr <- if count < size then return arr else do
    newarr <- liftIO $ newArray_ (0, 2 * size - 1)
    liftIO $ forM_ [lo .. hi] $ \ i -> readArray arr i >>= writeArray newarr i
    _dbIOArray .= newarr
    return newarr
  liftIO $ writeArray arr count rule
  _dbRuleCount .= count + 1
  _dbEntriesUpdated .= True

_resizeArray :: Int -> IOArray Int Rule -> IO (IOArray Int Rule)
_resizeArray newsize arr = do
  let newstoresize = max minSizePowerOf2 $ 2 ^ ilog2 newsize - 1
  oldstoresize <- uncurry subtract <$> getBounds arr
  if oldstoresize == newstoresize then return arr else do
    arr <- newArray_ (0, newstoresize)
    let loop i = seq i $! if i >= newsize then return arr else
          readArray arr i >>= writeArray arr i >> (loop $! i + 1)
    loop 0

-- | Update all rules matching a given 'Language.Interpreter.Dao.Kernel.List' query. Provide a
-- continuation that returns the updated list of 'Language.Interpreter.Dao.Kernel.Rule's wrapped in
-- a 'Prelude.Just' constructor, or return 'Prelude.Nothing' to indicate that the current
-- 'Language.Interpreter.Dao.Kernel.Rule' should not change. The continuation takes the matching
-- 'Rule' and the 'Language.Interpreter.Dao.Kernel.Dict' of variable names assigned during matching.
dbUpdatRules :: [DaoExpr] -> (Rule -> Dict -> Maybe [Rule]) -> DB ()
dbUpdatRules query f = DB $ loop [] 0 False 0 0 where
  fill stack stksz upd wr rd = seq stksz $! seq wr $! seq rd $!
    if wr == rd then loop stack stksz upd (wr + 1) (rd + 1) else case stack of
      []         -> loop stack stksz upd wr (rd + 1)
      rule:stack -> do
        arr <- use _dbIOArray
        liftIO $ writeArray arr wr rule
        fill stack (stksz - 1) True (wr + 1) rd
  loop stack stksz upd wr rd = seq stksz $! seq upd $! seq rd $! seq wr $! do
    arr   <- use _dbIOArray
    count <- use _dbRuleCount
    named <- use _dbNamedExprs
    if rd >= count
     then do
      let newsize = stksz + wr
      arr <- liftIO $ _resizeArray newsize arr
      liftIO $ forM (zip [wr ..] stack) $ uncurry $ writeArray arr
      _dbIOArray        .= arr
      _dbRuleCount      .= newsize
      _dbEntriesUpdated .= upd
     else do
      env    <- lift $ Session $ gets sessionEnvironment
      result <- liftIO $ do
        rule   <- readArray arr rd
        evalDaoExprIO (env{ locals = named }) $ do
          result <- flip runRuleMatcher query $ matchRulePattern
            (\ _ -> pure . maybe DaoVoid (daoList . fmap DaoRule) . f rule)
            DaoVoid rule
          return $ case result of
            MatchOK        DaoNull     -> DaoNull
            MatchOK (list@(DaoList{})) -> list
            _                          -> DaoVoid
      case result of
        DaoNull      -> fill stack stksz True wr rd
        DaoList list -> do
          let (count, rules) = first sum $ unzip $ toList (unwrapList list) >>= \ case
                DaoRule rule -> [(1, rule)]
                _            -> []
          fill (stack ++ rules) (stksz + count) True wr rd
        _ -> loop stack stksz upd (wr + 1) (rd + 1) -- the rule will be ignored

-- | Perform a query on the current production rule 'Database'. The query operation acts as a fold
-- operation, where you pass a continuation to be evaluated on the return value of each successful
-- 'Language.Interpreter.Dao.Kernel.rulePattern' that matches the query.
dbQuery :: (fold -> DaoExpr -> DB fold) -> fold -> [DaoExpr] -> DB fold
dbQuery f fold query = DB $ loop fold 0 where
  loop fold i = seq i $! use _dbRuleCount >>= \ count -> if i >= count then return fold else do
    arr    <- use _dbIOArray
    named  <- use _dbNamedExprs
    env    <- lift $ Session $ gets sessionEnvironment
    rule   <- liftIO $ readArray arr i
    result <- liftIO $ evalDaoExprIO (env{ locals = named }) $
      evalDaoRule rule query >>= return . \ case
        MatchOK  expr -> expr
        MatchQuit err -> DaoError err
        MatchFail err -> DaoError err
    fold   <- _unwrapDB $ f fold result
    loop fold $! i + 1 

