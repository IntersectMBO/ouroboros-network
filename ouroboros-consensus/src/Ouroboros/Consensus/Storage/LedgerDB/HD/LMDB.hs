{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB
  ( newLMDBBackingStore
  , LMDBBackingStore
  , LMDBValueHandle
  , DbErr
  , LMDBInit(..)
  , LMDBLimits
  , TraceDb(..)
  , defaultLMDBLimits -- TODO this is just for convenience, should remove
  ) where

import qualified Codec.Serialise as S (Serialise(..))
import qualified Control.Exception as Exn
import qualified Control.Monad.Class.MonadSTM as IOLike
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad (unless, when, void)
import           Data.Functor ( ($>), (<&>) )
import           Data.Foldable (for_)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Unsafe.Coerce(unsafeCoerce)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (At))
import qualified Cardano.Binary as CBOR(ToCBOR(..), FromCBOR(..))
import qualified Control.Tracer as Trace

import           Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import qualified Ouroboros.Consensus.Storage.FS.API as FS
import           Ouroboros.Consensus.Util.IOLike (IOLike, bracket, onException)
import           Ouroboros.Consensus.Util (foldlM')

import qualified Database.LMDB.Raw as LMDB
import qualified Database.LMDB.Simple as LMDB
import qualified Database.LMDB.Simple.Extra as LMDB
import qualified Database.LMDB.Simple.Internal as LMDB (Environment(Env), Transaction(Txn), isReadOnlyEnvironment)

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

data DbErr = DbErrStr !String
  | DbErrNoSettings
  | DbErrNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
  | DbErrNoDbNamed !String
  | DbErrBadDynamic !String
  | DbErrInitialisingNonEmpty !String
  | DbErrBadRead
  | DbErrBadRangeRead
  | DbErrDbExists !FS.FsPath
  | DbErrDbDoesntExist !FS.FsPath
  deriving stock (Show)

instance Exn.Exception DbErr

{-------------------------------------------------------------------------------
  Database definition
-------------------------------------------------------------------------------}

data Db m l = Db
  { dbEnv           :: !(LMDB.Environment LMDB.ReadWrite) -- ^ The LMDB environment is a pointer to the directory that contains the DB.
  , dbSettings      :: !(LMDB.Database () DbState) -- ^ A database with only one key and one value, for the current sequence number on the DB.
  , dbBackingTables :: !(LedgerTables l LMDBMK) -- ^ The LMDB database with the key-value store
  , dbFilePath      :: !FilePath
  , dbTracer        :: !(Trace.Tracer m TraceDb)
  , dbOpenHandles   :: !(IOLike.TVar m (Map Int (ValueHandle m)))
  }

newVHId :: Map Int (ValueHandle m) -> Int
newVHId openHdls = maybe 0 ((+1) . fst) $ Map.lookupMax openHdls

type LMDBLimits = LMDB.Limits

defaultLMDBLimits :: LMDBLimits
defaultLMDBLimits = LMDB.defaultLimits
  { LMDB.mapSize = 6_000_000_000
  , LMDB.maxDatabases = 10 -- We use 1 database per field + one for the state (i.e. sidetable)
  , LMDB.maxReaders = 16
  }

newtype DbState  = DbState
  { dbsSeq :: WithOrigin SlotNo  -- TODO a version field
  } deriving (Show, Generic)

instance S.Serialise DbState

newtype LmdbBox a = LmdbBox a
  deriving newtype (Eq, Show, Ord)

instance (CBOR.ToCBOR a, CBOR.FromCBOR a) => S.Serialise (LmdbBox a) where
  encode (LmdbBox a) = CBOR.toCBOR a
  decode = LmdbBox <$> CBOR.fromCBOR

coerceDatabase :: LMDB.Database k v -> LMDB.Database (LmdbBox k) (LmdbBox v)
coerceDatabase = unsafeCoerce

-- | ValueHandles hold an Async which is holding a transaction open.
data ValueHandle m = ValueHandle
  { vhClose :: !(m ())
  , vhSubmit :: !(forall a. LMDB.Transaction LMDB.ReadOnly (Maybe a) -> m (Maybe a))
  -- , vhRefCount :: !(IOLike.TVar IO Int)
  }

{-------------------------------------------------------------------------------
 LMDB Interface
-------------------------------------------------------------------------------}

lmdbReadTable :: LedgerConstraint k v
  => LMDB.Database k v
  -> HD.UtxoKeys k v
  -> LMDB.Transaction mode (HD.UtxoValues k v)
lmdbReadTable db0 (HD.UtxoKeys keys) =
    HD.UtxoValues <$> foldlM' go Map.empty (Set.toList keys)
  where
    db = coerceDatabase db0
    go m k = LMDB.get db (LmdbBox k) <&> \case
      Nothing -> m
      Just (LmdbBox v) -> Map.insert k v m

lmdbWriteTable :: LedgerConstraint k v
  => LMDB.Database k v
  -> HD.UtxoDiff k v
  -> LMDB.Transaction LMDB.ReadWrite ()
lmdbWriteTable db0 (HD.UtxoDiff m) =
    void $ Map.traverseWithKey go m
  where
    db = coerceDatabase db0
    go k (HD.UtxoEntryDiff v reason) = LMDB.put db (LmdbBox k) $
      case reason of
        HD.UedsDel ->
          Nothing
        HD.UedsInsAndDel ->
          Nothing
        HD.UedsIns ->
          Just (LmdbBox v)

-- | @`lmdbInitRangeReadTable` count db0@ performs a range read of roughly
-- @count@ values from database @db0@.
--
-- Note: See @`RangeQuery`@ for more information about range queries.
--
-- Todo(jdral): Test this function.
lmdbInitRangeReadTable :: LedgerConstraint k v
  => Int
  -> LMDB.Database k v
  -> LMDB.Transaction mode (HD.UtxoValues k v)
lmdbInitRangeReadTable count db0 =
  -- This is inadequate. We are folding over the whole table, the
  -- fiddling with either is to short circuit as best we can.
  -- TODO improve lmdb-simple bindings to give better access to cursors
    wrangle <$> LMDB.foldrWithKey go (Right Map.empty) db
  where
    wrangle = either HD.UtxoValues HD.UtxoValues
    db = coerceDatabase db0
    go (LmdbBox k) (LmdbBox v) acc = do
      m <- acc
      when (Map.size m >= count) $ Left m
      pure $ Map.insert k v m

-- | @`lmdbRangeReadTable` count db0 ks@ performs a range read of roughly
-- @count@ values from database @db0@.
--
-- Note: See @`RangeQuery`@ for more information about range queries.
--
-- Todo(jdral): Test this function.
lmdbRangeReadTable :: LedgerConstraint k v
  => Int
  -> LMDB.Database k v
  -> HD.UtxoKeys k v
  -> LMDB.Transaction mode (HD.UtxoValues k v)
lmdbRangeReadTable count db0 (HD.UtxoKeys keys) = case Set.lookupMax keys of
  -- This is inadequate. We are folding over the whole table, the
  -- fiddling with either is to short circuit as best we can.
  -- TODO improve lmdb-simple bindings to give better access to cursors
  Nothing -> pure $ HD.UtxoValues Map.empty
  Just last_excluded_key ->
    let
      db      = coerceDatabase db0
      wrangle = either HD.UtxoValues HD.UtxoValues
      go (LmdbBox k) (LmdbBox v) acc
        | k <= last_excluded_key = acc
        | otherwise = do
            m <- acc
            when (Map.size m >= count) $ Left m
            pure $ Map.insert k v m
    in
      wrangle <$> LMDB.foldrWithKey go (Right Map.empty) db

lmdbInitTable :: LedgerConstraint k v
  => String
  -> LMDB.Database k v
  -> HD.UtxoValues k v
  -> LMDB.Transaction LMDB.ReadWrite ()
lmdbInitTable tbl_name db0 (HD.UtxoValues m) = do
  let db = coerceDatabase db0
  is_empty <- LMDB.null db
  unless is_empty $ Exn.throw $ DbErrInitialisingNonEmpty tbl_name
  void $ Map.traverseWithKey (\k v -> LMDB.put db (LmdbBox k) (Just (LmdbBox v))) m

{-------------------------------------------------------------------------------
  LMDB Interface specialized for ApplyMapKinds
-------------------------------------------------------------------------------}

data LMDBMK k v = LMDBMK String !(LMDB.Database k v)

getDb                  :: LMDB.Mode mode       =>                      NameMK   k v -> LMDB.Transaction mode           (LMDBMK   k v)
initLMDBTable          :: LedgerConstraint k v =>        LMDBMK k v -> ValuesMK k v -> LMDB.Transaction LMDB.ReadWrite (EmptyMK  k v)
writeLMDBTable         :: LedgerConstraint k v =>        LMDBMK k v -> DiffMK   k v -> LMDB.Transaction LMDB.ReadWrite (EmptyMK  k v)
readLMDBTable          :: LedgerConstraint k v =>        LMDBMK k v -> KeysMK   k v -> LMDB.Transaction mode           (ValuesMK k v)
initRangeReadLMDBTable :: LedgerConstraint k v => Int -> LMDBMK k v                 -> LMDB.Transaction mode           (ValuesMK k v)
rangeReadLMDBTable     :: LedgerConstraint k v => Int -> LMDBMK k v -> KeysMK   k v -> LMDB.Transaction mode           (ValuesMK k v)

getDb                                                  (NameMK   name) = LMDBMK name      <$> LMDB.getDatabase (Just name)
initLMDBTable                (LMDBMK tbl_name db) (ApplyValuesMK vals) = ApplyEmptyMK     <$  lmdbInitTable  tbl_name db vals
writeLMDBTable               (LMDBMK _        db) (ApplyDiffMK   diff) = ApplyEmptyMK     <$  lmdbWriteTable          db diff
readLMDBTable                (LMDBMK _        db) (ApplyKeysMK   keys) = ApplyValuesMK    <$> lmdbReadTable           db keys
initRangeReadLMDBTable count (LMDBMK _        db)                      = ApplyValuesMK    <$> lmdbInitRangeReadTable count db
rangeReadLMDBTable     count (LMDBMK _        db) (ApplyKeysMK   keys) = ApplyValuesMK    <$> lmdbRangeReadTable count db keys

{-------------------------------------------------------------------------------
 Db settings
-------------------------------------------------------------------------------}

readDbSettingsMaybeNull ::
     LMDB.Database () DbState
  -> LMDB.Transaction mode (Maybe DbState)
readDbSettingsMaybeNull db = LMDB.get db ()

withDbSettingsRW ::
     LMDB.Database () DbState
  -> (DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState))
  -> LMDB.Transaction LMDB.ReadWrite a
withDbSettingsRW db f = withDbSettingsRWMaybeNull db $ maybe (Exn.throw DbErrNoSettings) f

withDbSettingsRWMaybeNull ::
      LMDB.Database () DbState
   -> (Maybe DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState))
   -> LMDB.Transaction LMDB.ReadWrite a
withDbSettingsRWMaybeNull db f  =
  readDbSettingsMaybeNull db >>= f >>= \(r, new_s) -> LMDB.put db () (Just new_s) $> r

data GuardDbDir  = GDDMustExist | GDDMustNotExist

guardDbDir :: MonadIO m => GuardDbDir -> FS.SomeHasFS m -> FS.FsPath -> m FilePath
guardDbDir gdd (FS.SomeHasFS fs) path = do
  FS.doesFileExist fs path >>= \b -> when b $ Exn.throw $ DbErrStr $ "guardDbDir:must be a directory:" <> show path
  FS.doesDirectoryExist fs path >>= \case
    True | GDDMustNotExist <- gdd ->
           -- TODO Should throw a DbErrDbExists exception
           -- Callers should delete the directory if they want to restore a snapshot
           FS.removeDirectoryRecursive fs path
    False | GDDMustExist <- gdd -> Exn.throw $ DbErrDbDoesntExist path
    _ -> pure ()
  FS.createDirectoryIfMissing fs True path
  FS.unsafeToFilePath fs path

{-------------------------------------------------------------------------------
 Initialize an LMDB
-------------------------------------------------------------------------------}

-- | How to initialise an LMDB Backing store
data LMDBInit l
  = LIInitialiseFromMemory (WithOrigin SlotNo) (LedgerTables l ValuesMK) -- ^ Initialise with these values
  | LIInitialiseFromLMDB FS.FsPath -- ^ Initialise by copying from an LMDB db at this path
  | LINoInitialise -- ^ The database is already initialised

-- | Initialize the LMDB from these provided values
initFromVals ::
     (TableStuff l, MonadIO m)
  => WithOrigin SlotNo
  -> LedgerTables l ValuesMK
  -> Db m l
  -> m ()
initFromVals dbsSeq vals Db{..} = liftIO $ LMDB.readWriteTransaction dbEnv $
  withDbSettingsRWMaybeNull dbSettings $ \case
    Nothing -> zipLedgerTablesA initLMDBTable dbBackingTables vals $> ((), DbState{dbsSeq})
    Just _ -> Exn.throw $ DbErrStr "initFromVals: db already had state"

initFromLMDBs :: (MonadIO m, IOLike m)
  => Trace.Tracer m TraceDb
  -> FS.SomeHasFS m
  -> FS.FsPath
  -> FS.FsPath
  -> m ()
initFromLMDBs tracer shfs from0 to0 = do
  Trace.traceWith tracer $ TDBInitialisingFromLMDB from0
  from <- guardDbDir GDDMustExist shfs from0
  to <- guardDbDir GDDMustNotExist shfs to0
  bracket
    (liftIO $ LMDB.openEnvironment from defaultLMDBLimits) -- TODO assess limits, in particular this could fail if the db is too big
    (liftIO . LMDB.closeEnvironment)
    (flip (lmdbCopy tracer) to)

lmdbCopy :: MonadIO m
  => Trace.Tracer m TraceDb
  -> LMDB.Environment LMDB.ReadWrite
  -> FilePath
  -> m ()
lmdbCopy tracer dbEnv to = do
  -- TODO This copying is gross. Tests depend on it, but I wish I was only responsible for copying the database here
  let LMDB.Env e = dbEnv
  from <- liftIO $ LMDB.mdb_env_get_path e
  Trace.traceWith tracer $ TDBCopying from to
  liftIO $ LMDB.mdb_env_copy e to
  Trace.traceWith tracer $ TDBCopied from to

-- TODO We don't know how much time is spent in I/O

-- TODO 50% of total time is spent somewhere else. Where?

-- | Initialise a backing store
newLMDBBackingStore :: forall m l. (TableStuff l, MonadIO m, IOLike m)
  => Trace.Tracer m TraceDb
  -> LMDBLimits
  -> FS.SomeHasFS m
  -> LMDBInit l
  -> m (LMDBBackingStore l m)
newLMDBBackingStore dbTracer limits sfs init_db = do
  Trace.traceWith dbTracer TDBOpening

  dbOpenHandles <- IOLike.newTVarIO Map.empty
  let
    path = FS.mkFsPath ["tables"]

    (gdd, copy_db_action :: m (), init_action :: Db m l -> m ()) = case init_db of
      LIInitialiseFromLMDB fp
        -- If fp == path then this is the LINoInitialise case
        | fp /= path -> (GDDMustNotExist, initFromLMDBs dbTracer sfs fp path, \_ -> pure ())
      LIInitialiseFromMemory slot vals -> (GDDMustNotExist, pure (), initFromVals slot vals)
      _ -> (GDDMustExist, pure (), \_ -> pure ())

  -- get the filepath for this db creates the directory if appropriate
  dbFilePath <- guardDbDir gdd sfs path

  -- copy from another lmdb path if appropriate
  copy_db_action

  -- open this database
  dbEnv <- liftIO $ LMDB.openEnvironment dbFilePath limits

  -- the LMDB.Database that holds the DbState (i.e. sequence number)
  -- This transaction must be read-write because on initialisation it creates the database
  dbSettings <- liftIO $ LMDB.readWriteTransaction dbEnv $ LMDB.getDatabase (Just "_dbstate")

  -- TODO: at some point Javier was able to get an LMDB which didn't have this
  -- database. How is it possible? maybe some copy function is not copying this
  -- one?

  -- Here we get the LMDB.Databases for the tables of the ledger state
  -- Must be read-write transaction because tables may need to be created
  dbBackingTables <- liftIO $ LMDB.readWriteTransaction dbEnv $
    traverseLedgerTables getDb namesLedgerTables

  let
    db = Db{..}
    bsClose :: m ()
    bsClose = do
      Trace.traceWith dbTracer $ TDBClosing dbFilePath
      open_handles <- IOLike.atomically $ IOLike.readTVar dbOpenHandles
      for_ open_handles vhClose
      liftIO $ LMDB.closeEnvironment dbEnv
      Trace.traceWith dbTracer $ TDBClosed dbFilePath

    bsCopy shfs (HD.BackingStorePath to0) = do
      to <- guardDbDir GDDMustNotExist shfs to0
      lmdbCopy dbTracer dbEnv to

    bsValueHandle = mkLMDBBackingStoreValueHandle db

    bsWrite :: SlotNo -> LedgerTables l DiffMK -> m ()
    bsWrite slot diffs = do
      old_slot <- liftIO $ LMDB.readWriteTransaction dbEnv $ withDbSettingsRW dbSettings $ \s@DbState{dbsSeq} -> do
        -- TODO This should be <. However the test harness does call bsWrite with the same slot
        unless (dbsSeq <= At slot) $ Exn.throw $ DbErrNonMonotonicSeq (At slot) dbsSeq
        void $ zipLedgerTablesA writeLMDBTable dbBackingTables diffs
        pure (dbsSeq, s {dbsSeq = At slot})
      Trace.traceWith dbTracer $ TDBWrite old_slot slot

  -- now initialise those tables if appropriate
  init_action db

  Trace.traceWith dbTracer $ TDBOpened dbFilePath
  pure HD.BackingStore{..}


data TraceValueHandle
  = TVHOpened
  | TVHClosing
  | TVHClosed
  | TVHSubmissionStarted
  | TVHSubmissionEnded
  deriving stock(Show, Eq)

mkValueHandle :: forall m. (MonadIO m, IOLike m)
  => Trace.Tracer m TraceDb
  -> LMDB.Environment LMDB.ReadOnly
  -> IOLike.TVar m (Map Int (ValueHandle m))
  -> m (ValueHandle m)
mkValueHandle dbTracer0 dbEnv dbOpenHandles = do
  let LMDB.Env env = dbEnv
  readOnlyTx <- liftIO $ LMDB.mdb_txn_begin env Nothing (LMDB.isReadOnlyEnvironment dbEnv)

  (r, traces) <- IOLike.atomically $ IOLike.stateTVar dbOpenHandles $ \x0 ->
    let
      vh_id = newVHId x0
      tracer = Trace.contramap (TDBValueHandle vh_id) dbTracer0

      vhSubmit :: forall a. LMDB.Transaction LMDB.ReadOnly (Maybe a) -> m (Maybe a)
      vhSubmit (LMDB.Txn t) = do
        present <- Map.member vh_id <$> IOLike.atomically (IOLike.readTVar dbOpenHandles)
        unless present $ Exn.throw DbErrBadRead
        flip onException vhClose $ do
          Trace.traceWith tracer TVHSubmissionStarted
          r <- liftIO $ t readOnlyTx
          Trace.traceWith tracer TVHSubmissionEnded
          pure r

      vhClose = do
        Trace.traceWith tracer TVHClosing
        -- Read-only transactions can be either committed or aborted with the
        -- same result. We chose commit for readability but they perform the
        -- same operations in this case.
        liftIO $ LMDB.mdb_txn_commit readOnlyTx
        IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.delete vh_id)
        Trace.traceWith tracer TVHClosed
      vh = ValueHandle{..}
    in
      ( (vh, [TDBValueHandle vh_id TVHOpened] )
      , Map.insert vh_id vh x0 )
  for_ traces $ Trace.traceWith dbTracer0
  pure r

mkLMDBBackingStoreValueHandle :: forall l m. (MonadIO m, TableStuff l, IOLike m)
  => Db m l
  -> m (WithOrigin SlotNo, LMDBValueHandle l m)
mkLMDBBackingStoreValueHandle Db{..} = do
  let
    dbEnvRO = LMDB.readOnlyEnvironment dbEnv
  vh <- mkValueHandle dbTracer dbEnvRO dbOpenHandles
  mb_init_slot <- vhSubmit vh $ readDbSettingsMaybeNull dbSettings
  init_slot <- liftIO $ maybe (Exn.throwIO $ DbErrStr "mkLMDBBackingStoreValueHandle ") (pure . dbsSeq) mb_init_slot
  let
    bsvhClose :: m ()
    bsvhClose = vhClose vh

    bsvhRead :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
    bsvhRead keys = vhSubmit vh (Just <$> zipLedgerTablesA readLMDBTable dbBackingTables keys) >>= \case
      Nothing -> Exn.throw DbErrBadRead
      Just x -> pure x

    bsvhRangeRead :: HD.RangeQuery (LedgerTables l KeysMK) -> m (LedgerTables l ValuesMK)
    bsvhRangeRead HD.RangeQuery{rqPrev, rqCount} = let
      transaction = Just <$> case rqPrev of
        Nothing -> traverseLedgerTables (initRangeReadLMDBTable rqCount) dbBackingTables
        Just keys -> zipLedgerTablesA (rangeReadLMDBTable rqCount) dbBackingTables keys
      in vhSubmit vh transaction >>= \case
        Nothing -> Exn.throw DbErrBadRangeRead
        Just x -> pure x
  pure (init_slot, HD.BackingStoreValueHandle{..})
