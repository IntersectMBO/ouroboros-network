{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB (
    -- * Backing Store interface
    LMDBBackingStore
  , LMDBValueHandle
    -- * Database definition
  , LMDBLimits (LMDBLimits, lmdbMapSize, lmdbMaxDatabases, lmdbMaxReaders)
    -- * Initialization
  , newLMDBBackingStoreInitialiser
    -- * Tracing
  , TraceLMDB (..)
    -- * Errors
  , DbErr (..)
    -- * Exported for `ledger-db-backends-checker`
  , DbState (..)
  , LMDBMK (..)
  ) where

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (At))
import qualified Codec.Serialise as S (Serialise (..))
import qualified Control.Concurrent.Class.MonadSTM.TVar as IOLike
import           Control.Monad (forM_, unless, void, when)
import qualified Control.Monad.Class.MonadSTM as IOLike
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Tracer as Trace
import           Data.Functor (($>), (<&>))
import           Data.Map (Map)
import           Data.Map.Diff.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Strict
import qualified Database.LMDB.Simple as LMDB
import qualified Database.LMDB.Simple.Cursor as LMDB.Cursor
import qualified Database.LMDB.Simple.Extra as LMDB
import qualified Database.LMDB.Simple.Internal as LMDB.Internal
import qualified Database.LMDB.Simple.TransactionHandle as TrH
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Ledger.Tables
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB.Bridge as Bridge
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB.Status
                     (Status (..), StatusLock)
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB.Status as Status
import           Ouroboros.Consensus.Util (foldlM', unComp2, (:..:) (..))
import           Ouroboros.Consensus.Util.IOLike (Exception (..), IOLike,
                     MonadCatch (..), MonadThrow (..), bracket)
import qualified System.FS.API as FS
import qualified System.FS.API.Types as FS

{-------------------------------------------------------------------------------
 Backing Store interface
-------------------------------------------------------------------------------}

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
  Database definition
-------------------------------------------------------------------------------}

-- | The LMDB database that underlies the backing store.
data Db m l = Db {
    -- | The LMDB environment is a pointer to the directory that contains the
    -- @`Db`@.
    dbEnv           :: !(LMDB.Environment LMDB.ReadWrite)
    -- | The on-disk state of the @`Db`@.
    --
    -- The state is kept in an LDMB table with only one key and one value:
    -- The current sequence number of the @`Db`@.
  , dbState         :: !(LMDB.Database () DbState)
    -- | The LMDB tables with the key-value stores.
  , dbBackingTables :: !(LedgerTables l LMDBMK)
  , dbFilePath      :: !FilePath
  , dbTracer        :: !(Trace.Tracer m TraceLMDB)
    -- | Status of the LMDB backing store. When 'Closed', all backing store
    -- (value handle) operations will fail.
  , dbStatusLock    :: !(StatusLock m)
    -- | Map of open value handles to cleanup actions. When closing the backing
    -- store, these cleanup actions are used to ensure all value handles cleaned
    -- up.
    --
    -- Note: why not use 'bsvhClose' here? We would get nested lock acquisition
    -- on 'dbStatusLock', which causes a deadlock:
    -- * 'bsClose' acquires a write lock
    -- * 'bsvhClose' is called on a value handle
    -- * 'bsvhClose' tries to acquire a read lock, but it has to wait for
    --   'bsClose' to give up its write lock
  , dbOpenHandles   :: !(IOLike.TVar m (Map Int (Cleanup m)))
  , dbNextId        :: !(IOLike.TVar m Int)
  }

newtype LMDBLimits = MkLMDBLimits {unLMDBLimits :: LMDB.Limits}

{-# COMPLETE LMDBLimits #-}
-- | Configuration to use for LMDB backing store initialisation.
--
-- Keep the following in mind:
-- * @'lmdbMapSize'@ should be a multiple of the OS page size.
-- * @'lmdbMaxDatabases'@ should be set to at least 2, since the backing store
--    has 2 internal LMDB databases by default: 1 for the actual tables, and
--    1 for the database state @'DbState'@.
pattern LMDBLimits :: Int -> Int -> Int -> LMDBLimits
pattern LMDBLimits{lmdbMapSize, lmdbMaxDatabases, lmdbMaxReaders} =
  MkLMDBLimits LMDB.Limits {
    LMDB.mapSize = lmdbMapSize
  , LMDB.maxDatabases = lmdbMaxDatabases
  , LMDB.maxReaders = lmdbMaxReaders
  }

-- | The database state consists of only the database sequence number @dbsSeq@.
-- @dbsSeq@ represents the slot up to which we have flushed changes to disk.
-- Note that we only flush changes to disk if they have become immutable.
newtype DbState = DbState {
    dbsSeq :: WithOrigin SlotNo
  }
  deriving stock (Show, Generic)
  deriving anyclass S.Serialise

-- | A 'MapKind' that represents an LMDB database
data LMDBMK k v = LMDBMK String !(LMDB.Database k v)

{-------------------------------------------------------------------------------
  Low-level API
-------------------------------------------------------------------------------}

getDb ::
     LMDB.Internal.IsMode mode
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
    ValuesMK <$> case unComp2 ksMK of
      Nothing -> runCursorHelper Nothing
      Just (KeysMK ks) -> case Set.lookupMax ks of
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
initLMDBTable (LMDBMK tblName db) codecMK (ValuesMK utxoVals) =
    EmptyMK <$ lmdbInitTable
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
readLMDBTable (LMDBMK _ db) codecMK (KeysMK keys) =
    ValuesMK <$> lmdbReadTable
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
writeLMDBTable (LMDBMK _ db) codecMK (DiffMK d) =
    EmptyMK <$ lmdbWriteTable
  where
    lmdbWriteTable = void $ traverseDiffEntryWithKey_ go d
      where
        go k de = case de of
          Delete _v           -> void $ Bridge.delete codecMK db k
          Insert v            -> Bridge.put codecMK db k v
          UnsafeAntiDelete _v -> error "Found anti-delete. See https://github.com/input-output-hk/anti-diffs/blob/main/diff-containers/README.md for an explanation why this should never happen"
          UnsafeAntiInsert _v -> error "Found anti-insert. See https://github.com/input-output-hk/anti-diffs/blob/main/diff-containers/README.md for an explanation why this should never happen"

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
 Guards
-------------------------------------------------------------------------------}

data GuardDbDir  = DirMustExist | DirMustNotExist

-- | Guard for the existence/non-existence of a database directory,
-- and create it if missing.
guardDbDir ::
     (MonadIO m, IOLike m)
  => GuardDbDir
  -> FS.SomeHasFS m
  -> FS.FsPath
  -> m FilePath
guardDbDir mustExistDir (FS.SomeHasFS fs) path = do
  fileEx <- FS.doesFileExist fs path
  when fileEx $
    throwIO $ DbErrNotADir path
  dirEx <- FS.doesDirectoryExist fs path
  lmdbFileExists <- FS.doesFileExist fs path { FS.fsPathToList = FS.fsPathToList path ++ [Strict.pack "data.mdb"] }
  filepath <- FS.unsafeToFilePath fs path
  case dirEx of
    True  | DirMustNotExist <- mustExistDir -> throwIO $ DbErrDirExists filepath
          | not lmdbFileExists              -> throwIO $ DbErrDirIsNotLMDB filepath
    False | DirMustExist    <- mustExistDir -> throwIO $ DbErrDirDoesntExist filepath
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
      (DirMustNotExist, DbErrDirExists _path) -> do
        FS.removeDirectoryRecursive fs path
        guardDbDir DirMustNotExist shfs path
      _ -> throwIO e

{-------------------------------------------------------------------------------
 Initialize an LMDB
-------------------------------------------------------------------------------}

-- | Initialise an LMDB database from these provided values.
initFromVals ::
     (HasLedgerTables l, CanSerializeLedgerTables l, MonadIO m)
  => Trace.Tracer m TraceLMDB
  -> WithOrigin SlotNo
     -- ^ The slot number up to which the ledger tables contain values.
  -> LedgerTables l ValuesMK
     -- ^ The ledger tables to initialise the LMDB database tables with.
  -> LMDB.Environment LMDB.Internal.ReadWrite
     -- ^ The LMDB environment.
  -> LMDB.Database () DbState
     -- ^ The state of the tables we are going to initialize the db with.
  -> LedgerTables l LMDBMK
  -> m ()
initFromVals tracer dbsSeq vals env st backingTables = do
  Trace.traceWith tracer $ TDBInitialisingFromValues dbsSeq
  liftIO $ LMDB.readWriteTransaction env $
    withDbStateRWMaybeNull st $ \case
      Nothing -> zipLedgerTables3A initLMDBTable backingTables codecLedgerTables vals
                 $> ((), DbState{dbsSeq})
      Just _ -> liftIO . throwIO $ DbErrInitialisingAlreadyHasState
  Trace.traceWith tracer $ TDBInitialisedFromValues dbsSeq

-- | Initialise an LMDB database from an existing LMDB database.
initFromLMDBs ::
     (MonadIO m, IOLike m)
  => Trace.Tracer m TraceLMDB
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
    from <- guardDbDir DirMustExist shfs from0
    -- On Windows, if we don't choose the mapsize carefully it will make the
    -- snapshot grow. Therefore we are using the current filesize as mapsize
    -- when opening the snapshot to avoid this.
    stat <- FS.withFile fs (from0 { FS.fsPathToList = FS.fsPathToList from0 ++ [Strict.pack "data.mdb"] }) FS.ReadMode (FS.hGetSize fs)
    to <- guardDbDirWithRetry DirMustNotExist shfs to0
    bracket
      (liftIO $ LMDB.openEnvironment from ((unLMDBLimits limits) { LMDB.mapSize = fromIntegral stat }))
      (liftIO . LMDB.closeEnvironment)
      (flip (lmdbCopy tracer) to)
    Trace.traceWith tracer $ TDBInitialisedFromLMDB to0

-- | Copy an existing LMDB database to a given directory.
lmdbCopy :: MonadIO m
  => Trace.Tracer m TraceLMDB
  -> LMDB.Environment LMDB.ReadWrite
     -- ^ The environment in which the LMDB database lives.
  -> FilePath
     -- ^ The path where the copy should reside.
  -> m ()
lmdbCopy tracer e to = do
  from <- liftIO $ LMDB.getPathEnvironment e
  Trace.traceWith tracer $ TDBCopying from to
  liftIO $ LMDB.copyEnvironment e to
  Trace.traceWith tracer $ TDBCopied from to

-- | Initialise a backing store.
newLMDBBackingStoreInitialiser ::
     forall m l. (HasLedgerTables l, CanSerializeLedgerTables l, MonadIO m, IOLike m)
  => Trace.Tracer m TraceLMDB
  -> LMDBLimits
     -- ^ Configuration parameters for the LMDB database that we
     -- initialise. In case we initialise the LMDB database from
     -- an existing LMDB database, we use these same configuration parameters
     -- to open the existing LMDB database.
  -> FS.SomeHasFS m
  -> HD.InitFrom (LedgerTables l ValuesMK)
  -> m (LMDBBackingStore l m)
newLMDBBackingStoreInitialiser dbTracer limits sfs initFrom = do
   Trace.traceWith dbTracer TDBOpening

   db@Db { dbEnv
         , dbState
         , dbBackingTables
         , dbFilePath
         } <- createOrGetDB

   maybePopulate dbEnv dbState dbBackingTables

   Trace.traceWith dbTracer $ TDBOpened dbFilePath

   pure $ mkBackingStore db
 where
   createOrGetDB :: m (Db m l)
   createOrGetDB = do

     dbOpenHandles <- IOLike.newTVarIO Map.empty
     dbStatusLock  <- Status.new Open

     let path = FS.mkFsPath ["tables"]

     -- get the filepath for this db creates the directory if appropriate
     dbFilePath <- guardDbDirWithRetry DirMustNotExist sfs path

     -- copy from another lmdb path if appropriate
     case initFrom of
       HD.InitFromCopy (HD.BackingStorePath fp) -> initFromLMDBs dbTracer limits sfs fp path
       _                                        -> pure ()

     -- open this database
     dbEnv <- liftIO $ LMDB.openEnvironment dbFilePath (unLMDBLimits limits)

     -- The LMDB.Database that holds the @`DbState`@ (i.e. sequence number)
     -- This transaction must be read-write because on initialisation it creates the database
     dbState <- liftIO $ LMDB.readWriteTransaction dbEnv $ LMDB.getDatabase (Just "_dbstate")

     -- Here we get the LMDB.Databases for the tables of the ledger state
     -- Must be read-write transaction because tables may need to be created
     dbBackingTables <- liftIO $ LMDB.readWriteTransaction dbEnv $
       traverseLedgerTables getDb namesLedgerTables

     dbNextId <- IOLike.newTVarIO 0

     pure $ Db { dbEnv
               , dbState
               , dbBackingTables
               , dbFilePath
               , dbTracer
               , dbStatusLock
               , dbOpenHandles
               , dbNextId
               }

   maybePopulate :: LMDB.Internal.Environment  LMDB.Internal.ReadWrite
                 -> LMDB.Internal.Database () DbState
                 -> LedgerTables l LMDBMK
                 -> m ()
   maybePopulate dbEnv dbState dbBackingTables = do
     -- now initialise those tables if appropriate
     case initFrom of
       HD.InitFromValues slot vals -> initFromVals dbTracer slot vals dbEnv dbState dbBackingTables
       _                           -> pure ()

   mkBackingStore :: Db m l -> LMDBBackingStore l m
   mkBackingStore db =
       let bsClose :: m ()
           bsClose = Status.withWriteAccess dbStatusLock DbErrClosed $ do
             Trace.traceWith dbTracer $ TDBClosing dbFilePath
             openHandles <- IOLike.readTVarIO dbOpenHandles
             forM_ openHandles runCleanup
             IOLike.atomically $ IOLike.writeTVar dbOpenHandles mempty
             liftIO $ LMDB.closeEnvironment dbEnv
             Trace.traceWith dbTracer $ TDBClosed dbFilePath
             pure (Closed, ())

           bsCopy shfs bsp = Status.withReadAccess dbStatusLock DbErrClosed $ do
             let HD.BackingStorePath to0 = bsp
             to <- guardDbDir DirMustNotExist shfs to0
             lmdbCopy dbTracer dbEnv to

           bsValueHandle = Status.withReadAccess dbStatusLock DbErrClosed $ do
             mkLMDBBackingStoreValueHandle db

           bsWrite :: SlotNo -> LedgerTables l DiffMK -> m ()
           bsWrite slot diffs = Status.withReadAccess dbStatusLock DbErrClosed $ do
             oldSlot <- liftIO $ LMDB.readWriteTransaction dbEnv $ withDbStateRW dbState $ \s@DbState{dbsSeq} -> do
               unless (dbsSeq <= At slot) $ liftIO . throwIO $ DbErrNonMonotonicSeq (At slot) dbsSeq
               void $ zipLedgerTables3A writeLMDBTable dbBackingTables codecLedgerTables diffs
               pure (dbsSeq, s {dbsSeq = At slot})
             Trace.traceWith dbTracer $ TDBWrite oldSlot slot

       in HD.BackingStore { HD.bsClose = bsClose
                           , HD.bsCopy = bsCopy
                           , HD.bsValueHandle = bsValueHandle
                           , HD.bsWrite = bsWrite
                           }

      where
        Db { dbEnv
           , dbState
           , dbBackingTables
           , dbFilePath
           , dbStatusLock
           , dbOpenHandles
           } = db

-- | Create a backing store value handle that has a consistent view of the
-- current database state (i.e., the database contents, not to be confused
-- with @`DbState`@).
mkLMDBBackingStoreValueHandle ::
     forall l m.
     (HasLedgerTables l, CanSerializeLedgerTables l, MonadIO m, IOLike m)
  => Db m l
     -- ^ The LMDB database for which the backing store value handle is
     -- created.
  -> m (WithOrigin SlotNo, LMDBValueHandle l m)
mkLMDBBackingStoreValueHandle db = do
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
  initSlot <- liftIO $ maybe (throwIO DbErrUnableToReadSeqNo) (pure . dbsSeq) mbInitSlot

  vhStatusLock <- Status.new Open

  let
    -- | Clean up a backing store value handle by committing its transaction
    -- handle.
    cleanup :: Cleanup m
    cleanup = Cleanup $ do
      liftIO $ TrH.commit trh

    bsvhClose :: m ()
    bsvhClose =
      Status.withReadAccess dbStatusLock DbErrClosed $ do
      Status.withWriteAccess vhStatusLock (DbErrNoValueHandle vhId) $ do
        Trace.traceWith tracer TVHClosing
        runCleanup cleanup
        IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.delete vhId)
        Trace.traceWith tracer TVHClosed
        pure (Closed, ())

    bsvhRead :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
    bsvhRead keys =
      Status.withReadAccess dbStatusLock DbErrClosed $ do
      Status.withReadAccess vhStatusLock (DbErrNoValueHandle vhId) $ do
        Trace.traceWith tracer TVHReadStarted
        res <- liftIO $ TrH.submitReadOnly trh (zipLedgerTables3A readLMDBTable dbBackingTables codecLedgerTables keys)
        Trace.traceWith tracer TVHReadEnded
        pure res

    bsvhRangeRead ::
         HD.RangeQuery (LedgerTables l KeysMK)
      -> m (LedgerTables l ValuesMK)
    bsvhRangeRead rq =
      Status.withReadAccess dbStatusLock DbErrClosed $ do
      Status.withReadAccess vhStatusLock (DbErrNoValueHandle vhId) $ do
        Trace.traceWith tracer TVHRangeReadStarted

        let
          outsideIn ::
              Maybe (LedgerTables l mk1)
            -> LedgerTables l (Maybe :..: mk1)
          outsideIn Nothing       = pureLedgerTables (Comp2 Nothing)
          outsideIn (Just tables) = mapLedgerTables (Comp2 . Just) tables

          transaction =
            zipLedgerTables3A
              (rangeRead rqCount)
              dbBackingTables
              codecLedgerTables
              (outsideIn rqPrev)

        res <- liftIO $ TrH.submitReadOnly trh transaction
        Trace.traceWith tracer TVHRangeReadEnded
        pure res
     where
      HD.RangeQuery rqPrev rqCount = rq

    bsvh = HD.BackingStoreValueHandle { HD.bsvhClose = bsvhClose
                                      , HD.bsvhRead = bsvhRead
                                      , HD.bsvhRangeRead = bsvhRangeRead
                                      }

  IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.insert vhId cleanup)

  Trace.traceWith tracer TVHOpened
  pure (initSlot, bsvh)

 where
   Db { dbEnv
      , dbTracer
      , dbState
      , dbOpenHandles
      , dbBackingTables
      , dbNextId
      , dbStatusLock
      } = db

-- | A monadic action used for cleaning up resources.
newtype Cleanup m = Cleanup { runCleanup :: m () }

{-------------------------------------------------------------------------------
 Tracing
-------------------------------------------------------------------------------}

data TraceLMDB
  = TDBOpening
  | TDBOpened      !FilePath
  | TDBClosing     !FilePath
  | TDBClosed      !FilePath
  | TDBCopying     !FilePath -- ^ From
                   !FilePath -- ^ To
  | TDBCopied      !FilePath -- ^ From
                   !FilePath -- ^ To
  | TDBWrite       !(WithOrigin SlotNo) !SlotNo
  | TDBValueHandle Int TraceValueHandle
  | TDBInitialisingFromLMDB !FS.FsPath
  | TDBInitialisedFromLMDB !FS.FsPath
  | TDBInitialisingFromValues !(WithOrigin SlotNo)
  | TDBInitialisedFromValues !(WithOrigin SlotNo)
  deriving (Show, Eq)

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

{-------------------------------------------------------------------------------
 Errors
-------------------------------------------------------------------------------}

data DbErr =
    -- | The database state can not be found on-disk.
    DbErrNoDbState
    -- | The sequence number of a @`Db`@ should be monotonically increasing
    -- across calls to @`bsWrite`@, since we use @`bsWrite`@ to flush
    -- /immutable/ changes. That is, we can only flush with a newer sequence
    -- number because the changes should be /immutable/. Note that this does
    -- not mean that values can not be changed in the future, only that we
    -- can not change values in the past.
  | DbErrNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
    -- | The database table that is being initialised is non-empty.
  | DbErrInitialisingNonEmpty !String
    -- | The database that is being initialized already had a DbState table
  | DbErrInitialisingAlreadyHasState
    -- | Trying to use a non-existing value handle.
  | DbErrNoValueHandle !Int
    -- | Couldn't create a value handle because we couldn't read the sequence
    -- number
  | DbErrUnableToReadSeqNo
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
    -- | What should be a directory is in fact a file
  | DbErrNotADir !FS.FsPath
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
  DbErrNoDbState ->
    "Can not find the database state on-disk."
  DbErrNonMonotonicSeq s1 s2 ->
    "Trying to write to the database with a non-monotonic sequence number: "
    <> showParen True (shows s1) ""
    <> " is not <= "
    <> showParen True (shows s2) ""
  DbErrInitialisingNonEmpty s ->
    "The database table that is being initialised is non-empty: " <> s
  DbErrInitialisingAlreadyHasState ->
    "The database contains no values but still has a table with a sequence number."
  DbErrNoValueHandle vh_id ->
    "Trying to use non-existing value handle: " <> show vh_id
  DbErrUnableToReadSeqNo ->
    "Reading the sequence number failed thus we couldn't create a value handle."
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
    <> "\nPre-UTxO-HD and In-Memory implementations are incompatible \
       \ with the LMDB implementation, please delete your ledger database \
       \ if you want to run with LMDB"
  DbErrNotADir path ->
    "The path " <> show path <> " should be a directory but it is a file instead."
  DbErrClosed -> "The database has been closed."
