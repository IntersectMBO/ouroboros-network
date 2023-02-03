{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB (
    DbErr (..)
  , LMDBBackingStore
  , LMDBInit (..)
  , LMDBValueHandle
  , TraceDb (..)
  , newLMDBBackingStoreInitialiser
    -- * Configuration
  , LMDBLimits (LMDBLimits, lmdbMapSize, lmdbMaxDatabases, lmdbMaxReaders)
    -- * Exported for ledger-db-backends-checker
  , DbState (..)
  , LMDBMK (..)
  ) where

import qualified Codec.Serialise as S (Serialise (..))
import qualified Control.Concurrent.Class.MonadSTM.TVar as IOLike
import           Control.Monad (unless, void, when)
import qualified Control.Monad.Class.MonadSTM as IOLike
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Tracer as Trace
import           Data.Foldable (for_)
import           Data.Functor (($>), (<&>))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Strict
import           GHC.Generics (Generic)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (At))

import           Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Storage.FS.API as FS
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB.Bridge as Bridge
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB.TransactionHandle as TrH
import           Ouroboros.Consensus.Util (foldlM', unComp2, (:..:) (..))
import           Ouroboros.Consensus.Util.IOLike (Exception (..), IOLike,
                     MonadCatch (..), MonadThrow (..), bracket)

import qualified Database.LMDB.Raw as LMDB
import qualified Database.LMDB.Simple as LMDB
import qualified Database.LMDB.Simple.Cursor as LMDB.Cursor
import qualified Database.LMDB.Simple.Extra as LMDB
import qualified Database.LMDB.Simple.Internal as LMDB.Internal

{-------------------------------------------------------------------------------
 Backing Store interface
-------------------------------------------------------------------------------}

type LMDBBackingStoreInitialiser l m =
  HD.BackingStoreInitialiser m
    (LedgerTables l KeysMK)
    (LedgerTables l ValuesMK)
    (LedgerTables l DiffMK)

type LMDBBackingStore l m =
  HD.BackingStore m
    (LedgerTables l KeysMK)
    (LedgerTables l ValuesMK)
    (LedgerTables l DiffMK)

type LMDBValueHandle l m =
  HD.BackingStoreValueHandle m
    (LedgerTables l KeysMK)
    (LedgerTables l ValuesMK)

{-------------------------------------------------------------------------------
 Tracing
-------------------------------------------------------------------------------}

data TraceDb
  = TDBOpening
  | TDBOpened      !FilePath
  | TDBClosing     !FilePath
  | TDBClosed      !FilePath
  | TDBCopying     !FilePath !FilePath
  | TDBCopied      !FilePath !FilePath
  | TDBWrite       !(WithOrigin SlotNo) !SlotNo
  | TDBValueHandle Int TraceValueHandle
  | TDBTableOp     TraceTableOp
  | TDBInitialisingFromLMDB !FS.FsPath
  | TDBInitialisedFromLMDBD !FS.FsPath
  deriving (Show, Eq)

data TraceTableOp = TTO
  deriving (Show, Eq)

data TDBTableOp
  = TTORead
  | TTOInit
  | TTOWrite
  deriving (Show)

{-------------------------------------------------------------------------------
 Errors
-------------------------------------------------------------------------------}

data DbErr =
    -- | General-purpose error.
    DbErrStr !String
    -- | The database state can not be found on-disk.
  | DbErrNoDbState
    -- | The sequence number of a @`Db`@ should be monotonically increasing
    -- across calls to @`bsWrite`@, since we use @`bsWrite`@ to flush
    -- /immutable/ changes. That is, we can only flush with a newer sequence
    -- number because the changes should be /immutable/. Note that this does
    -- not mean that values can not be changed in the future, only that we
    -- can not change values in the past.
  | DbErrNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
    -- | The database table that is being initialised is non-empty.
  | DbErrInitialisingNonEmpty !String
    -- | Trying to use a non-existing value handle.
  | DbErrNoValueHandle !Int
    -- | Failed to read a value from a database table.
  | DbErrBadRead
    -- | Failed to read a range of values from a database table.
  | DbErrBadRangeRead
    -- | A database directory should not exist already.
  | DbErrDirExists !FilePath
    -- | A database directory should exist already.
  | DbErrDirDoesntExist !FilePath
    -- | The directory exists but is not an LMDB directory!
  | DbErrDirIsNotLMDB !FilePath
    -- | The database has been closed, so all backing store operations should
    -- throw an error.
  | DbErrClosed

instance Exception DbErr

-- | Show instance for pretty printing @`DbErr`@s as error messages that
-- include: (i) an indication of the probable cause of the error, and
-- (ii) a descriptive error message for the specific @`DbErr`@.
instance Show DbErr where
  show dbErr = mconcat
      [ "[LMDB-ERROR] "
      , "The LMDB Backing store has encountered a fatal exception. "
      , "Possibly, the LMDB database is corrupted.\n"
      , "[ERROR-MSG] "
      , prettyPrintDbErr dbErr
      ]

-- | Pretty print a @`DbErr`@ with a descriptive error message.
prettyPrintDbErr :: DbErr -> String
prettyPrintDbErr = \case
  DbErrStr s ->
    "General-purpose error: " <> s
  DbErrNoDbState ->
    "Can not find the database state on-disk."
  DbErrNonMonotonicSeq s1 s2 ->
    "Trying to write to the database with a non-monotonic sequence number: "
    <> showParen True (shows s1) ""
    <> " is not <= "
    <> showParen True (shows s2) ""
  DbErrInitialisingNonEmpty s ->
    "The database table that is being initialised is non-empty: " <> s
  DbErrNoValueHandle vh_id ->
    "Trying to use non-existing value handle: " <> show vh_id
  DbErrBadRead ->
    "Failed to read a value from a database table."
  DbErrBadRangeRead ->
    "Failed to read a range of values from a database table."
  DbErrDirExists path ->
    "Database directory should not exist already: " <> show path
  DbErrDirDoesntExist path ->
    "Database directory should already exist: " <> show path
  DbErrDirIsNotLMDB path ->
    "Database directory doesn't contain an LMDB database: "
    <> show path
    <> "\nPre-UTxO-HD and In-Memory implementations are incompatible with the LMDB implementation, please delete your ledger database if you want to run with LMDB"
  DbErrClosed -> "The database has been closed."

{-------------------------------------------------------------------------------
  Database definition
-------------------------------------------------------------------------------}

-- | The LMDB database that underlies the backing store.
data Db m l = Db {
    -- | The LMDB environment is a pointer to the directory that contains the
    -- @`Db`@.
    dbEnv           :: !(LMDB.Environment LMDB.ReadWrite)
  , dbSettings      :: !(LMDB.Database () DbSettings)
    -- | The on-disk state of the @`Db`@.
    --
    -- The state is kept in an LDMB table with only one key and one value:
    -- The current sequence number of the @`Db`@.
  , dbState         :: !(LMDB.Database () DbState)
    -- | The LMDB tables with the key-value stores.
  , dbBackingTables :: !(LedgerTables l LMDBMK)
  , dbFilePath      :: !FilePath
  , dbTracer        :: !(Trace.Tracer m TraceDb)
  , dbClosed        :: !(IOLike.TVar m Bool)
  , dbOpenHandles   :: !(IOLike.TVar m (Map Int (LMDBValueHandle l m)))
  , dbNextId        :: !(IOLike.TVar m Int)
  }

newtype LMDBLimits = MkLMDBLimits {unLMDBLimits :: LMDB.Limits}

{-# COMPLETE LMDBLimits #-}
-- | Configuration to use for LMDB backing store initialisation.
--
-- Keep the following in mind:
-- * @'lmdbMapSize'@ should be a multiple of the OS page size.
-- * @'lmdbMaxDatabases'@ should be set to at least 2, since the backing store
--    has 2 internal LMDB databases by default: 1 for the database settings
--    @'DbSettings'@, and 1 for the database state @'DbState'@.
pattern LMDBLimits :: Int -> Int -> Int -> LMDBLimits
pattern LMDBLimits{lmdbMapSize, lmdbMaxDatabases, lmdbMaxReaders} =
  MkLMDBLimits LMDB.Limits {
    LMDB.mapSize = lmdbMapSize
  , LMDB.maxDatabases = lmdbMaxDatabases
  , LMDB.maxReaders = lmdbMaxReaders
  }

-- | The database settings.
--
-- Todo(jdral): The settings are currently empty, but this may change in the
-- future depending on changes that make this module incompatible with existing
-- LMDB databases. For example: Changes to the seralisation format. One setting
-- that should/could be included is a versioning scheme, which would signal
-- incompatibility if versions don't match. Possibly, we would also have to
-- implement a way to migrate between databases that have different versions.
data DbSettings = DbSettings
  deriving (Show, Generic)

instance S.Serialise DbSettings

-- | The database state consists of only the database sequence number @dbsSeq@.
-- @dbsSeq@ represents the slot up to which we have flushed changes to disk.
-- Note that we only flush changes to disk if they have become immutable.
newtype DbState = DbState {
    dbsSeq :: WithOrigin SlotNo
  } deriving (Show, Generic)

instance S.Serialise DbState

{-------------------------------------------------------------------------------
  LMDB Interface specialized for ApplyMapKinds
-------------------------------------------------------------------------------}

data LMDBMK k v = LMDBMK String !(LMDB.Database k v)

getDb ::
     LMDB.Mode mode
  => NameMK k v
  -> LMDB.Transaction mode (LMDBMK k v)
getDb (NameMK name) = LMDBMK name <$> LMDB.getDatabase (Just name)

-- | @'rangeRead' n db codec ksMay@ performs a range read of @count@ values from
-- database @db@, starting from some key depending on @ksMay@.
--
-- The @codec@ argument defines how to serialise/deserialise keys and values.
--
-- A range read can return less than @count@ values if there are not enough
-- values to read.
--
-- Note: See @`RangeQuery`@ for more information about range queries. In
-- particular, @'rqPrev'@ describes the role of @ksMay@.
--
-- What the "first" key in the database is, and more generally in which order
-- keys are read, depends on the lexographical ordering of the /serialised/
-- keys. Care should be taken such that the @'Ord'@ instance for @k@ matches the
-- lexicographical ordering of the serialised keys, or the result of this
-- function will be unexpected.
rangeRead ::
     forall k v mode. Ord k
  => Int
  -> LMDBMK k v
  -> CodecMK k v
  -> (Maybe :..: KeysMK) k v
  -> LMDB.Transaction mode (ValuesMK k v)
rangeRead count dbMK codecMK ksMK =
    ApplyValuesMK <$> case unComp2 ksMK of
      Nothing -> runCursorHelper Nothing
      Just (ApplyKeysMK ks) -> case Set.lookupMax ks of
        Nothing -> pure mempty
        Just lastExcludedKey ->
          runCursorHelper $ Just (lastExcludedKey, LMDB.Cursor.Exclusive)
  where
    LMDBMK _ db = dbMK

    runCursorHelper ::
         Maybe (k, LMDB.Cursor.Bound)    -- ^ Lower bound on read range
      -> LMDB.Transaction mode (Map k v)
    runCursorHelper lb =
      Bridge.runCursorAsTransaction'
        (LMDB.Cursor.cgetMany lb count)
        db
        codecMK

initLMDBTable ::
     LMDBMK   k v
  -> CodecMK  k v
  -> ValuesMK k v
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK k v)
initLMDBTable (LMDBMK tblName db) codecMK (ApplyValuesMK utxoVals) =
    ApplyEmptyMK <$ lmdbInitTable
  where
    lmdbInitTable  = do
      isEmpty <- LMDB.null db
      unless isEmpty $ liftIO . throwIO $ DbErrInitialisingNonEmpty tblName
      void $ Map.traverseWithKey
                 (Bridge.put codecMK db)
                 utxoVals

readLMDBTable ::
     Ord k
  => LMDBMK  k v
  -> CodecMK k v
  -> KeysMK  k v
  -> LMDB.Transaction mode (ValuesMK k v)
readLMDBTable (LMDBMK _ db) codecMK (ApplyKeysMK keys) =
    ApplyValuesMK <$> lmdbReadTable
  where
    lmdbReadTable = foldlM' go Map.empty (Set.toList keys)
      where
        go m k = Bridge.get codecMK db k <&> \case
          Nothing -> m
          Just v  -> Map.insert k v m

writeLMDBTable ::
     LMDBMK  k v
  -> CodecMK k v
  -> DiffMK  k v
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK k v)
writeLMDBTable (LMDBMK _ db) codecMK (ApplyDiffMK d) =
    ApplyEmptyMK <$ lmdbWriteTable
  where
    lmdbWriteTable = void $ DS.traverseDiffEntryWithKey_ go d
      where
        go k de = case de of
          DS.Delete _            -> void $ Bridge.delete codecMK db k
          DS.Insert v            -> Bridge.put codecMK db k v
          DS.UnsafeAntiDelete _v -> error "Found anti-delete."
          DS.UnsafeAntiInsert _v -> error "Found anti-insert."

{-------------------------------------------------------------------------------
 Db Settings
-------------------------------------------------------------------------------}

writeDbSettings ::
     LMDB.Database () DbSettings
  -> LMDB.Transaction LMDB.ReadWrite ()
writeDbSettings db = LMDB.put db () (Just DbSettings)

{-------------------------------------------------------------------------------
 Db state
-------------------------------------------------------------------------------}

readDbStateMaybeNull ::
     LMDB.Database () DbState
  -> LMDB.Transaction mode (Maybe DbState)
readDbStateMaybeNull db = LMDB.get db ()

withDbStateRW ::
     LMDB.Database () DbState
  -> (DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState))
  -> LMDB.Transaction LMDB.ReadWrite a
withDbStateRW db f = withDbStateRWMaybeNull db $ maybe (liftIO . throwIO $ DbErrNoDbState) f

withDbStateRWMaybeNull ::
      LMDB.Database () DbState
   -> (Maybe DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState))
   -> LMDB.Transaction LMDB.ReadWrite a
withDbStateRWMaybeNull db f  =
  readDbStateMaybeNull db >>= f >>= \(r, sNew) -> LMDB.put db () (Just sNew) $> r

{-------------------------------------------------------------------------------
 Db directory guards
-------------------------------------------------------------------------------}

data GuardDbDir  = GDDMustExist | GDDMustNotExist

-- | Guard for the existence/non-existence of a database directory,
-- and create it if missing.
guardDbDir ::
     (MonadIO m, IOLike m)
  => GuardDbDir
  -> FS.SomeHasFS m
  -> FS.FsPath
  -> m FilePath
guardDbDir gdd (FS.SomeHasFS fs) path = do
  fileEx <- FS.doesFileExist fs path
  when fileEx $
    throwIO $ DbErrStr $ "guardDbDir: must be a directory: " <> show path
  dirEx <- FS.doesDirectoryExist fs path
  lmdbFileExists <- FS.doesFileExist fs path { FS.fsPathToList = FS.fsPathToList path ++ [Strict.pack "data.mdb"] }
  filepath <- FS.unsafeToFilePath fs path
  case dirEx of
    True  | GDDMustNotExist <- gdd -> throwIO $ DbErrDirExists filepath
          | not lmdbFileExists     -> throwIO $ DbErrDirIsNotLMDB filepath
    False | GDDMustExist    <- gdd -> throwIO $ DbErrDirDoesntExist filepath
    _                              -> pure ()
  FS.createDirectoryIfMissing fs True path
  pure filepath

-- | Same as @`guardDbDir`@, but retries the guard if we can make meaningful
-- changes to the filesystem before we perform the retry.
--
-- Note: We only retry if a database directory exists while it shoudn't. In
-- this case, we remove the directory recursively before retrying the guard.
-- This is necessary for initialisation of the LMDB backing store, since the
-- (non-snapshot) tables will probably still be on-disk. These tables are not
-- removed when stopping the node, so they should be "overwritten".
guardDbDirWithRetry ::
     (MonadIO m, IOLike m)
  => GuardDbDir
  -> FS.SomeHasFS m
  -> FS.FsPath
  -> m FilePath
guardDbDirWithRetry gdd shfs@(FS.SomeHasFS fs) path =
    handle retryHandler (guardDbDir gdd shfs path)
  where
    retryHandler e = case (gdd, e) of
      (GDDMustNotExist, DbErrDirExists _path) -> do
        FS.removeDirectoryRecursive fs path
        guardDbDir GDDMustNotExist shfs path
      _ -> throwIO e

{-------------------------------------------------------------------------------
 Initialize an LMDB
-------------------------------------------------------------------------------}

-- | How to initialise an LMDB Backing store.
data LMDBInit l =
    -- | Initialise with these values.
    LIInitialiseFromMemory (WithOrigin SlotNo) (LedgerTables l ValuesMK)
    -- | Initialise by copying from an LMDB database at a given path.
  | LIInitialiseFromLMDB FS.FsPath
    -- | The database is already initialised.
  | LINoInitialise


-- | Initialise an LMDB database from these provided values.
initFromVals ::
     (TableStuff l, SufficientSerializationForAnyBackingStore l, MonadIO m)
  => WithOrigin SlotNo
     -- ^ The slot number up to which the ledger tables contain values.
  -> LedgerTables l ValuesMK
     -- ^ The ledger tables to initialise the LMDB database tables with.
  -> Db m l
     -- ^ The LMDB database.
  -> m ()
initFromVals dbsSeq vals Db{..} = liftIO $ LMDB.readWriteTransaction dbEnv $
  withDbStateRWMaybeNull dbState $ \case
    Nothing -> zipLedgerTables2A initLMDBTable dbBackingTables codecLedgerTables vals
                $> ((), DbState{dbsSeq})
    Just _ -> liftIO . throwIO $ DbErrStr "initFromVals: db already had state"

-- | Initialise an LMDB database from an existing LMDB database.
initFromLMDBs ::
     (MonadIO m, IOLike m)
  => Trace.Tracer m TraceDb
  -> LMDBLimits
     -- ^ Configuration for the LMDB database that we initialise from.
  -> FS.SomeHasFS m
     -- ^ Abstraction over the filesystem.
  -> FS.FsPath
     -- ^ The path that contains the LMDB database that we want to initialise from.
  -> FS.FsPath
     -- ^ The path where the new LMDB database should be initialised.
  -> m ()
initFromLMDBs tracer limits shfs@(FS.SomeHasFS fs) from0 to0 = do
    Trace.traceWith tracer $ TDBInitialisingFromLMDB from0
    from <- guardDbDir GDDMustExist shfs from0
    -- On Windows, if we don't choose the mapsize carefully it will make the
    -- snapshot grow. Therefore we are using the current filesize as mapsize
    -- when opening the snapshot to avoid this.
    stat <- FS.withFile fs (from0 { FS.fsPathToList = FS.fsPathToList from0 ++ [Strict.pack "data.mdb"] }) FS.ReadMode (FS.hGetSize fs)
    to <- guardDbDirWithRetry GDDMustNotExist shfs to0
    bracket
      (liftIO $ LMDB.openEnvironment from ((unLMDBLimits limits) { LMDB.mapSize = fromIntegral stat }))
      (liftIO . LMDB.closeEnvironment)
      (flip (lmdbCopy tracer) to)
    Trace.traceWith tracer $ TDBInitialisedFromLMDBD to0

-- | Copy an existing LMDB database to a given directory.
lmdbCopy :: MonadIO m
  => Trace.Tracer m TraceDb
  -> LMDB.Environment LMDB.ReadWrite
     -- ^ The environment in which the LMDB database lives.
  -> FilePath
     -- ^ The path where the copy should reside.
  -> m ()
lmdbCopy tracer dbEnv to = do
  let LMDB.Internal.Env e = dbEnv
  from <- liftIO $ LMDB.mdb_env_get_path e
  Trace.traceWith tracer $ TDBCopying from to
  liftIO $ LMDB.mdb_env_copy e to
  Trace.traceWith tracer $ TDBCopied from to

-- TODO We don't know how much time is spent in I/O

-- TODO 50% of total time is spent somewhere else. Where?

-- | Initialise a backing store.
newLMDBBackingStoreInitialiser ::
     forall m l. (TableStuff l, SufficientSerializationForAnyBackingStore l, MonadIO m, IOLike m)
  => Trace.Tracer m TraceDb
  -> LMDBLimits
     -- ^ Configuration parameters for the LMDB database that we
     -- initialise. In case we initialise the LMDB database from
     -- an existing LMDB database, we use these same configuration parameters
     -- to open the existing LMDB database.
  -> LMDBBackingStoreInitialiser l m
newLMDBBackingStoreInitialiser dbTracer limits = HD.BackingStoreInitialiser $ \sfs initFrom -> do
  Trace.traceWith dbTracer TDBOpening

  dbOpenHandles <- IOLike.newTVarIO Map.empty
  dbClosed      <- IOLike.newTVarIO False

  let
    path = FS.mkFsPath ["tables"]

    (gdd, copyDbAction :: m (), initAction :: Db m l -> m ()) = case initFrom of
      HD.InitFromCopy (HD.BackingStorePath fp)
        -- If fp == path then this is the LINoInitialise case
        | fp /= path -> (GDDMustNotExist, initFromLMDBs dbTracer limits sfs fp path, \_ -> pure ())
      HD.InitFromValues slot vals -> (GDDMustNotExist, pure (), initFromVals slot vals)
      _ -> (GDDMustExist, pure (), \_ -> pure ())

  -- get the filepath for this db creates the directory if appropriate
  dbFilePath <- guardDbDirWithRetry gdd sfs path

  -- copy from another lmdb path if appropriate
  copyDbAction

  -- open this database
  dbEnv <- liftIO $ LMDB.openEnvironment dbFilePath (unLMDBLimits limits)

  -- Create the settings table.
  dbSettings <- liftIO $ LMDB.readWriteTransaction dbEnv $ LMDB.getDatabase (Just "_dbsettings")
  liftIO $ LMDB.readWriteTransaction dbEnv $ writeDbSettings dbSettings

  -- The LMDB.Database that holds the @`DbState`@ (i.e. sequence number)
  -- This transaction must be read-write because on initialisation it creates the database
  dbState <- liftIO $ LMDB.readWriteTransaction dbEnv $ LMDB.getDatabase (Just "_dbstate")

  -- TODO: at some point Javier was able to get an LMDB which didn't have this
  -- database. How is it possible? maybe some copy function is not copying this
  -- one?

  -- Here we get the LMDB.Databases for the tables of the ledger state
  -- Must be read-write transaction because tables may need to be created
  dbBackingTables <- liftIO $ LMDB.readWriteTransaction dbEnv $
    traverseLedgerTables getDb namesLedgerTables

  dbNextId <- IOLike.newTVarIO 0

  let
    db = Db{..}
    bsClose :: m ()
    bsClose = do
      guardDbClosed dbClosed
      Trace.traceWith dbTracer $ TDBClosing dbFilePath
      openHandles <- IOLike.readTVarIO dbOpenHandles
      for_ openHandles HD.bsvhClose
      IOLike.atomically $ IOLike.modifyTVar dbClosed (const True)
      liftIO $ LMDB.closeEnvironment dbEnv
      Trace.traceWith dbTracer $ TDBClosed dbFilePath

    bsCopy shfs (HD.BackingStorePath to0) = do
      guardDbClosed dbClosed
      to <- guardDbDir GDDMustNotExist shfs to0
      lmdbCopy dbTracer dbEnv to

    bsValueHandle =
      guardDbClosed dbClosed >>
      mkLMDBBackingStoreValueHandle db

    bsWrite :: SlotNo -> LedgerTables l DiffMK -> m ()
    bsWrite slot diffs = do
      guardDbClosed dbClosed
      oldSlot <- liftIO $ LMDB.readWriteTransaction dbEnv $ withDbStateRW dbState $ \s@DbState{dbsSeq} -> do
        -- TODO This should be <. However the test harness does call bsWrite with the same slot
        unless (dbsSeq <= At slot) $ liftIO . throwIO $ DbErrNonMonotonicSeq (At slot) dbsSeq
        void $ zipLedgerTables2A writeLMDBTable dbBackingTables codecLedgerTables diffs
        pure (dbsSeq, s {dbsSeq = At slot})
      Trace.traceWith dbTracer $ TDBWrite oldSlot slot

  -- now initialise those tables if appropriate
  initAction db

  Trace.traceWith dbTracer $ TDBOpened dbFilePath
  pure HD.BackingStore{..}

guardDbClosed :: IOLike m => IOLike.TVar m Bool -> m ()
guardDbClosed tb = do
  b <- IOLike.readTVarIO tb
  when b $ throwIO DbErrClosed

guardBsvhClosed :: IOLike m => Int -> IOLike.TVar m (Map Int (LMDBValueHandle l m)) -> m ()
guardBsvhClosed vhId tvhs = do
  b <- Map.member vhId <$> IOLike.readTVarIO tvhs
  unless b $ throwIO (DbErrNoValueHandle vhId)

data TraceValueHandle
  = TVHOpening
  | TVHOpened
  | TVHClosing
  | TVHClosed
  | TVHReadStarted
  | TVHReadEnded
  | TVHRangeReadStarted
  | TVHRangeReadEnded
  deriving stock(Show, Eq)

-- | Create a backing store value handle that has a consistent view of the
-- current database state (i.e., the database contents, not to be confused
-- with @`DbState`@).
mkLMDBBackingStoreValueHandle ::
     forall l m.
     (TableStuff l, SufficientSerializationForAnyBackingStore l, MonadIO m, IOLike m)
  => Db m l
     -- ^ The LMDB database for which the backing store value handle is
     -- created.
  -> m (WithOrigin SlotNo, LMDBValueHandle l m)
mkLMDBBackingStoreValueHandle Db{..} = do
  vhId <- IOLike.atomically $ do
    vhId <- IOLike.readTVar dbNextId
    IOLike.modifyTVar' dbNextId (+1)
    pure vhId

  let
    dbEnvRo = LMDB.readOnlyEnvironment dbEnv
    tracer = Trace.contramap (TDBValueHandle vhId) dbTracer

  Trace.traceWith tracer TVHOpening

  trh <- liftIO $ TrH.newReadOnly dbEnvRo
  mbInitSlot <- liftIO $ TrH.submitReadOnly trh $ readDbStateMaybeNull dbState
  initSlot <- liftIO $ maybe (throwIO $ DbErrStr "mkLMDBBackingStoreValueHandle ") (pure . dbsSeq) mbInitSlot

  let
    bsvhClose :: m ()
    bsvhClose = do
      Trace.traceWith tracer TVHClosing
      guardDbClosed dbClosed
      guardBsvhClosed vhId dbOpenHandles
      liftIO $ TrH.commit trh
      IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.delete vhId)
      Trace.traceWith tracer TVHClosed

    bsvhRead :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
    bsvhRead keys = do
      Trace.traceWith tracer TVHReadStarted
      guardDbClosed dbClosed
      guardBsvhClosed vhId dbOpenHandles
      res <- liftIO $ TrH.submitReadOnly trh (zipLedgerTables2A readLMDBTable dbBackingTables codecLedgerTables keys)
      Trace.traceWith tracer TVHReadEnded
      pure res

    bsvhRangeRead ::
         HD.RangeQuery (LedgerTables l KeysMK)
      -> m (LedgerTables l ValuesMK)
    bsvhRangeRead HD.RangeQuery{rqPrev, rqCount} = do
      Trace.traceWith tracer TVHRangeReadStarted

      let
        outsideIn ::
             Maybe (LedgerTables l mk1)
          -> LedgerTables l (Maybe :..: mk1)
        outsideIn Nothing       = pureLedgerTables (Comp2 Nothing)
        outsideIn (Just tables) = mapLedgerTables (Comp2 . Just) tables

        transaction =
          zipLedgerTables2A
            (rangeRead rqCount)
            dbBackingTables
            codecLedgerTables
            (outsideIn rqPrev)

      guardDbClosed dbClosed
      guardBsvhClosed vhId dbOpenHandles
      res <- liftIO $ TrH.submitReadOnly trh transaction
      Trace.traceWith tracer TVHRangeReadEnded
      pure res

    bsvh = HD.BackingStoreValueHandle{..}

  IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.insert vhId bsvh)

  Trace.traceWith tracer TVHOpened
  pure (initSlot, bsvh)
