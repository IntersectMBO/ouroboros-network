{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NumericUnderscores #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB
  ( newLMDBBackingStore
  , LMDBBackingStore
  , LMDBValueHandle
  , DbErr
  , LMDBInit(..)
  , LMDBLimits
  , defaultLMDBLimits -- TODO this is just for convenience, should remove
  ) where

import qualified Database.LMDB.Simple as LMDB
import qualified Database.LMDB.Simple.DBRef as LMDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Ledger.Basics as Tables
import qualified Control.Exception as Exn
import qualified Ouroboros.Consensus.Storage.FS.API as FS
import Cardano.Slotting.Slot (SlotNo, WithOrigin (At))
import Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Control.Monad.Class.MonadAsync as IOLike
import qualified Control.Monad.Class.MonadSTM as IOLike
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad ( forever, unless, when, void )
import Data.Functor ( ($>), (<&>) )
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified Codec.Serialise as S(Serialise(..))
import Data.Map (Map)
import qualified Data.Set as Set
import Ouroboros.Consensus.Util (foldlM')
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Database.LMDB.Simple.Extra as LMDB
import Data.Foldable (for_)
import Database.LMDB.Simple (ReadWrite)
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import qualified Cardano.Binary as CBOR(ToCBOR(..), FromCBOR(..))
import Unsafe.Coerce(unsafeCoerce)
import qualified System.Directory as Dir
import qualified Control.Tracer as Trace
import qualified Control.Monad.Trans.Writer.CPS as Writer

{-

TODO Don't use DbRef

-}

{-# HLINT ignore #-}

type LMDBBackingStore l m = HD.BackingStore m
  (Tables.LedgerTables l Tables.KeysMK)
  (Tables.LedgerTables l Tables.ValuesMK)
  (Tables.LedgerTables l Tables.DiffMK)
-- type LMDBBackingStore k v = HD.BackingStore IO (UtxoKeys k v) (UtxoValues k v) (UtxoDiff k v)

type LMDBValueHandle l m = HD.BackingStoreValueHandle m
  (Tables.LedgerTables l Tables.KeysMK)
  (Tables.LedgerTables l Tables.ValuesMK)
  

data TraceDb
  = TDBOpening
  | TDBOpened
  | TDBClosing
  | TDBClosed
  | TDBCopying
  | TDBCopied
  | TDBWrite !(WithOrigin SlotNo) !(SlotNo)
  | TDBValueHandle Int TraceValueHandle
  | TBDTableOp TraceTableOp
  deriving (Show)

data TraceTableOp = TTO
  deriving (Show)

data TDBTableOp
  = TTORead
  | TTOInit
  | TTOWrite
  deriving (Show)

data Db l = Db
  { dbEnv :: !(LMDB.Environment LMDB.ReadWrite)
  , dbSettings :: !(LMDB.DBRef LMDB.ReadWrite DbState)
  , dbBackingTables :: !(Tables.LedgerTables l Tables.LMDBMK)
  , dbFilePath :: !FilePath
  , dbTracer :: !(Trace.Tracer IO TraceDb)
  , dbOpenHandles :: !(IOLike.TVar IO (Map Int ValueHandle))
  }

data DbErr = DbErrStr !String
  | DbErrNoSettings
  | DbErrNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
  | DbErrNoDbNamed !String
  | DbErrBadDynamic !String
  | DbErrInitialisingNonEmpty !String
  | DbErrBadRead
  | DbErrBadRangeRead
  deriving stock (Show)

instance Exn.Exception DbErr

data DbState  = DbState
  { dbsSeq :: !(WithOrigin SlotNo)
  -- TODO a version field
  } deriving (Show, Generic)

instance S.Serialise (DbState)

newtype LmdbBox a = LmdbBox a
  deriving newtype (Eq, Show, Ord) -- i.e. Shows as if it were a

instance (CBOR.ToCBOR a, CBOR.FromCBOR a) => S.Serialise (LmdbBox a) where
  encode (LmdbBox a) = CBOR.toCBOR a
  decode = LmdbBox <$> CBOR.fromCBOR

coerceDatabase :: LMDB.Database k v -> LMDB.Database (LmdbBox k) (LmdbBox v)
coerceDatabase = unsafeCoerce
  -- the type parameters to Database are phantom

-- | ValueHandles hold an Async which is holding a transaction open.
data ValueHandle = ValueHandle
  { vhClose :: !(IO ())
  , vhSubmit :: !(forall a. LMDB.Transaction LMDB.ReadOnly a -> IO (Maybe a))
  -- , vhRefCount :: !(IOLike.TVar IO Int)
  }

readLMDBTable :: (LMDB.Mode mode, Tables.LedgerConstraint k v)
  => LMDB.Database k v
  -> HD.UtxoKeys k v
  -> LMDB.Transaction mode (HD.UtxoValues k v)
readLMDBTable db0 (HD.UtxoKeys keys) =
  HD.UtxoValues <$> foldlM' go Map.empty (Set.toList keys) where
  db = coerceDatabase db0
  go m k = LMDB.get db (LmdbBox k) <&> \case
    Nothing -> m
    Just (LmdbBox v) -> Map.insert k v m

writeLMDBTable :: Tables.LedgerConstraint k v
  => LMDB.Database k v
  -> HD.UtxoDiff k v
  -> LMDB.Transaction LMDB.ReadWrite ()
writeLMDBTable db0 (HD.UtxoDiff m) = void $ Map.traverseWithKey go m where
  db = coerceDatabase db0
  go k (HD.UtxoEntryDiff v reason) = LMDB.put db (LmdbBox k) new_val where
    new_val
      | reason `elem` [HD.UedsDel, HD.UedsInsAndDel]
      = Nothing
      | otherwise = Just (LmdbBox v)

initRangeReadLMDBTable :: (LMDB.Mode mode, Tables.LedgerConstraint k v)
  => Int
  -> LMDB.Database k v
  -> LMDB.Transaction mode (HD.UtxoValues k v)
initRangeReadLMDBTable count db0 =
  -- This is inadequate. We are folding over the whole table, the
  -- fiddling with either is to short circuit as best we can.
  -- TODO improve llvm-simple bindings to give better access to cursors
  wrangle <$> LMDB.foldrWithKey go (Right Map.empty) db where
  db = coerceDatabase db0
  wrangle = either HD.UtxoValues HD.UtxoValues
  go (LmdbBox k) (LmdbBox v) acc = do
    m <- acc
    when (Map.size m >= count) $ Left m
    pure $ Map.insert k v m

rangeReadLMDBTable :: (LMDB.Mode mode, Tables.LedgerConstraint k v)
  => Int
  -> LMDB.Database k v
  -> HD.UtxoKeys k v
  -> LMDB.Transaction mode (HD.UtxoValues k v)
rangeReadLMDBTable count db0 (HD.UtxoKeys keys) = case Set.lookupMax keys of
  -- This is inadequate. We are folding over the whole table, the
  -- fiddling with either is to short circuit as best we can.
  -- TODO improve llvm-simple bindings to give better access to cursors
  Nothing -> pure $ HD.UtxoValues Map.empty
  Just last_excluded_key -> let
    db = coerceDatabase db0
    wrangle = either HD.UtxoValues HD.UtxoValues
    go (LmdbBox k) (LmdbBox v) acc
      | k <= last_excluded_key = acc
      | otherwise = do
          m <- acc
          when (Map.size m >= count) $ Left m
          pure $ Map.insert k v m
    in wrangle <$> LMDB.foldrWithKey go (Right Map.empty) db

initLMDBTable :: Tables.LedgerConstraint k v
  => String
  -> LMDB.Database k v
  -> HD.UtxoValues k v
  -> LMDB.Transaction LMDB.ReadWrite ()
initLMDBTable tbl_name db0 (HD.UtxoValues m) = do
  let db = coerceDatabase db0
  is_empty <- LMDB.null db
  unless is_empty $ Exn.throw $ DbErrInitialisingNonEmpty tbl_name
  void $ let
    go k v = LMDB.put db (LmdbBox k) (Just (LmdbBox v))
    in Map.traverseWithKey go m
    
getDb_amk :: ( LMDB.Mode mode)
  => Tables.ApplyMapKind Tables.NameMK k v
  -> LMDB.Transaction mode (Tables.ApplyMapKind Tables.LMDBMK k v)
getDb_amk (Tables.ApplyNameMK name) = Tables.ApplyLMDBMK name <$>
  LMDB.getDatabase (Just name)
  
initLMDBTable_amk :: Tables.LedgerConstraint k v
  => Tables.ApplyMapKind Tables.LMDBMK k v
  -> Tables.ApplyMapKind Tables.ValuesMK k v
  -> LMDB.Transaction ReadWrite (Tables.ApplyMapKind Tables.EmptyMK k v)
initLMDBTable_amk (Tables.ApplyLMDBMK tbl_name db) (Tables.ApplyValuesMK vals) =
  initLMDBTable tbl_name db vals $> Tables.ApplyEmptyMK 

readLMDBTable_amk :: (LMDB.Mode mode, Tables.LedgerConstraint k v)
  => Tables.ApplyMapKind Tables.LMDBMK k v
  -> Tables.ApplyMapKind Tables.KeysMK k v
  -> LMDB.Transaction mode (Tables.ApplyMapKind Tables.ValuesMK k v)
readLMDBTable_amk (Tables.ApplyLMDBMK _ db) (Tables.ApplyKeysMK keys) =
  Tables.ApplyValuesMK <$> readLMDBTable db keys

writeLMDBTable_amk :: Tables.LedgerConstraint k v
  => Tables.ApplyMapKind Tables.LMDBMK k v
  -> Tables.ApplyMapKind Tables.DiffMK k v
  -> LMDB.Transaction ReadWrite (Tables.ApplyMapKind Tables.EmptyMK k v)
writeLMDBTable_amk (Tables.ApplyLMDBMK _tbl_name db) (Tables.ApplyDiffMK diff) =
  writeLMDBTable db diff $> Tables.ApplyEmptyMK 

initRangeReadLMDBTable_amk :: (LMDB.Mode mode, Tables.LedgerConstraint k v)
  => Int -- ^ number of rows to return
  -> Tables.ApplyMapKind Tables.LMDBMK k v
  -> LMDB.Transaction mode (Tables.ApplyMapKind Tables.ValuesMK k v)
initRangeReadLMDBTable_amk count (Tables.ApplyLMDBMK _tbl_name db)  =
  Tables.ApplyValuesMK <$> initRangeReadLMDBTable count db

rangeReadLMDBTable_amk :: (LMDB.Mode mode, Tables.LedgerConstraint k v)
  => Int -- ^ number of rows to return
  -> Tables.ApplyMapKind Tables.LMDBMK k v
  -> Tables.ApplyMapKind Tables.KeysMK k v
  -> LMDB.Transaction mode (Tables.ApplyMapKind Tables.ValuesMK k v)
rangeReadLMDBTable_amk count (Tables.ApplyLMDBMK _tbl_name db) (Tables.ApplyKeysMK keys) =
  Tables.ApplyValuesMK <$> rangeReadLMDBTable count db keys

readDbSettings :: LMDB.DBRef LMDB.ReadWrite DbState -> LMDB.Transaction mode DbState
readDbSettings dbSettings = liftIO (LMDB.readDBRef dbSettings) >>= \case
  Just x -> pure x
  Nothing -> Exn.throw $ DbErrNoSettings

withDbSettingsRW :: LMDB.DBRef LMDB.ReadWrite DbState -> (DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState)) -> LMDB.Transaction LMDB.ReadWrite a
withDbSettingsRW dbSettings f  =
  readDbSettings dbSettings >>= f >>= \(r, new_s) ->
    liftIO $ LMDB.writeDBRef dbSettings (Just new_s) $> r

readDbSettingsMaybeNull :: LMDB.DBRef LMDB.ReadWrite DbState -> LMDB.Transaction mode (Maybe DbState)
readDbSettingsMaybeNull dbSettings = liftIO (LMDB.readDBRef dbSettings)

withDbSettingsRWMaybeNull :: LMDB.DBRef LMDB.ReadWrite DbState -> (Maybe DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState)) -> LMDB.Transaction LMDB.ReadWrite a
withDbSettingsRWMaybeNull dbSettings f  =
  readDbSettingsMaybeNull dbSettings >>= f >>= \(r, new_s) ->
    liftIO $ LMDB.writeDBRef dbSettings (Just new_s) $> r

data GuardDbDir  = GDDMustExist | GDDMustNotExist

guardDbDir :: IOLike m => GuardDbDir -> FS.SomeHasFS m -> FS.FsPath -> m FilePath
guardDbDir gdd (FS.SomeHasFS fs) path = do
  FS.doesFileExist fs path >>= \b -> when b $ Exn.throw $ DbErrStr $ "guardDbDir:must be a directory:" <> show path
  FS.doesDirectoryExist fs path >>= \case
    True | GDDMustNotExist <- gdd ->Exn.throw $ DbErrStr $ "guardDbDir: Must not exist: " <> show path
    _ -> pure ()
  FS.createDirectoryIfMissing fs True path
  FS.unsafeToFilePath fs path

defaultLMDBLimits :: LMDB.Limits
defaultLMDBLimits = LMDB.defaultLimits
  { LMDB.mapSize = 6_000_000_000
  , LMDB.maxDatabases = 10 -- We use 1 database per field + one for the state (i.e. sidetable)
  , LMDB.maxReaders = 16
  }

initFromVals :: Tables.TableStuff l
  => WithOrigin SlotNo
  -> Tables.LedgerTables l Tables.ValuesMK
  -> Db l
  -> IO ()
initFromVals dbsSeq vals Db{..} = do
  putStrLn "initFromVals:1"
  LMDB.readWriteTransaction dbEnv $ do
    liftIO $ putStrLn "initFromVals:3"
    withDbSettingsRWMaybeNull dbSettings $ \case
      Nothing -> Tables.zipLedgerTablesA initLMDBTable_amk dbBackingTables vals $> ((),DbState{dbsSeq})
      Just _ -> Exn.throw $ DbErrStr "initFromVals: db already had state"
    liftIO $ putStrLn "initFromVals:4"
  putStrLn "initFromVals:2"


initFromLMDBs :: MonadIO m => FS.SomeHasFS m -> FS.FsPath -> FS.FsPath -> m ()
initFromLMDBs _shfs _from _to= do
  error "unimplemented"
  -- copy lmdb from _from to _to
  -- the new dir has been created
  -- need to validate


lmdbCopy :: Db l -> FS.SomeHasFS m -> HD.BackingStorePath -> m ()
lmdbCopy = error "unimplemented"

-- | How to initialise an LMDB Backing store
data LMDBInit l
  = LIInitialiseFromMemory (WithOrigin SlotNo) (Tables.LedgerTables l Tables.ValuesMK) -- ^ Initialise with these values
  | LIInitialiseFromLMDB FS.FsPath -- ^ Initialise by copying from an LMDB db at this path
  | LINoInitialise -- ^ The database is already initialised

type LMDBLimits = LMDB.Limits
-- | Initialise a backing store

data TraceLMDBBackingStore
  = Trace

newLMDBBackingStore :: forall m l. (Tables.TableStuff l, MonadIO m, IOLike m)
  => Trace.Tracer IO TraceDb
  -> LMDBLimits
  -> FS.SomeHasFS m
  -> FS.FsPath -- ^ The path for the store we are opening
  -> LMDBInit l 
  -> m (LMDBBackingStore l m)
newLMDBBackingStore dbTracer limits sfs path init_db = do
  liftIO $ Trace.traceWith dbTracer TDBOpening
  dbOpenHandles <- liftIO $ IOLike.newTVarIO Map.empty
  let
    (gdd, copy_db_action :: m (), init_action :: Db l -> m ()) = case init_db of
      LIInitialiseFromLMDB fp
        -- If fp == path then this is the LINoInitialise case
        | fp /= path -> (GDDMustNotExist, initFromLMDBs sfs fp path, \_ -> pure ())
      LIInitialiseFromMemory slot vals -> (GDDMustNotExist, pure (), liftIO . initFromVals slot vals)
      _ -> (GDDMustExist, pure (), \_ -> pure ())
  -- get the filepath for this db creates the directory if appropriate
  dbFilePath <- guardDbDir gdd sfs path

  -- putStrLn $ "lmdb path:" <> show dbFilePath

  -- copy from another lmdb path if appropriate
  copy_db_action

    -- putStrLn "opening db"
    -- x <- Dir.doesDirectoryExist dbFilePath
      -- putStrLn $ "does path exist? " <> show x
    -- open this database
  dbEnv <- liftIO $ LMDB.openEnvironment dbFilePath limits

  -- putStrLn "initting db1"
  -- the LMDB.Database that holds the DbState (i.e. sequence number)
  -- This transaction must be read-write because on initialisation it creates the database
  dbstate_db <- liftIO $ LMDB.readWriteTransaction  dbEnv $ LMDB.getDatabase (Just "_dbstate")
  liftIO $ putStrLn "initting db2"
  dbSettings <- liftIO $ LMDB.newDBRef dbEnv dbstate_db (0 :: Int)
  liftIO $ putStrLn "initting db3"

    -- Here we get the LMDB.Databases for the tables of the ledger state
    -- Must be read-write transaction because tables may need to be created
  dbBackingTables <- liftIO $ LMDB.readWriteTransaction dbEnv $
    Tables.traverseLedgerTables getDb_amk Tables.namesLedgerTables

  liftIO $ putStrLn "initting db5"
  let
    db = Db{..}
    bsClose :: m ()
    bsClose = liftIO $ do
      Trace.traceWith dbTracer TDBClosing
      open_handles <- IOLike.atomically $ IOLike.readTVar dbOpenHandles
      for_ open_handles vhClose
      Trace.traceWith dbTracer TDBClosed

    -- bsCopy :: LMDBBackingStore l m -> m ()
    bsCopy = lmdbCopy db

    bsValueHandle = mkLMDBBackingStoreValueHandle db

    bsWrite :: SlotNo -> Tables.LedgerTables l Tables.DiffMK -> m ()
    bsWrite slot diffs = liftIO $ do
      old_slot <- LMDB.readWriteTransaction dbEnv $ withDbSettingsRW dbSettings $ \s@DbState{dbsSeq} -> do
        when (At slot <= dbsSeq) $ Exn.throw $ DbErrNonMonotonicSeq (At slot) dbsSeq
        void $ Tables.zipLedgerTablesA writeLMDBTable_amk dbBackingTables diffs
        pure (dbsSeq, s {dbsSeq = At slot})
      Trace.traceWith dbTracer $ TDBWrite old_slot slot

  -- now initialise those tables if appropriate
  init_action db

  -- putStrLn "finished newLMDBBackingSktore"
  liftIO $ Trace.traceWith dbTracer TDBOpened
  pure HD.BackingStore{..}


data SomeDbSubmission where
  SomeDbSubmission :: forall a. LMDB.Transaction LMDB.ReadOnly a -> IOLike.TMVar IO (Maybe a) -> SomeDbSubmission

data TraceValueHandle
  = TVHOpened
  | TVHClosing
  | TVHClosed
  | TVHSubmissionStarted
  | TVHSubmissionEnded
  deriving stock(Show)

mkValueHandle :: (MonadIO m)
  => Trace.Tracer IO TraceDb
  -> LMDB.Environment LMDB.ReadOnly
  -> IOLike.TVar IO (Map Int ValueHandle)
  -> m ValueHandle
mkValueHandle tracer0 dbEnv dbOpenHandles = liftIO $ do
  q <- IOLike.newTBQueueIO 1 -- TODO 1?
  the_async <- IOLike.async $ LMDB.readOnlyTransaction dbEnv $ do
    let
      loop :: MaybeT (LMDB.Transaction LMDB.ReadOnly) ()
      loop = do
        SomeDbSubmission t ret_tmvar <- MaybeT . liftIO $ IOLike.atomically (IOLike.readTBQueue q)
        r <- lift t
        liftIO . IOLike.atomically $ IOLike.putTMVar ret_tmvar (Just r)
    void . runMaybeT $ forever loop
    LMDB.abort

  (r, traces) <- IOLike.atomically $ IOLike.stateTVar dbOpenHandles $ \x0 -> let

    vh_id = maybe 0 ((+1) . fst) $ Map.lookupMax x0
    tracer = Trace.contramap (\x -> TDBValueHandle vh_id x) tracer0

    vhSubmit :: forall a. LMDB.Transaction LMDB.ReadOnly a -> IO (Maybe a)
    vhSubmit t = do
      Trace.traceWith tracer TVHSubmissionStarted
      ret_tmvar <- IOLike.newEmptyTMVarIO
      IOLike.atomically . IOLike.writeTBQueue q . Just $ SomeDbSubmission t ret_tmvar
      r <- IOLike.atomically $ IOLike.takeTMVar ret_tmvar
      Trace.traceWith tracer TVHSubmissionEnded
      pure r

    vhClose = do
      Trace.traceWith tracer TVHClosing
      IOLike.cancel the_async
      IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.delete vh_id)
      Trace.traceWith tracer TVHClosed
    vh = ValueHandle{..}
    in ((vh, [TDBValueHandle vh_id TVHOpened] ), Map.insert vh_id vh x0)
  for_ traces $ Trace.traceWith tracer0
  pure r

mkLMDBBackingStoreValueHandle :: forall l m. (MonadIO m, Tables.TableStuff l)
  => Db l
  -> m (WithOrigin SlotNo, LMDBValueHandle l m)
mkLMDBBackingStoreValueHandle Db{..} = do
  let
    dbe = LMDB.readOnlyEnvironment dbEnv
  vh <- mkValueHandle dbTracer dbe dbOpenHandles
  mb_init_slot <- liftIO $ vhSubmit vh $ do
    DbState{dbsSeq} <- readDbSettings dbSettings
    pure dbsSeq
  init_slot <- liftIO $ maybe (Exn.throwIO $ DbErrStr "mkLMDBBackingStoreValueHandle ") pure mb_init_slot
  let
    bsvhClose :: m ()
    bsvhClose = liftIO $ vhClose vh

    bsvhRead :: Tables.LedgerTables l Tables.KeysMK -> m (Tables.LedgerTables l Tables.ValuesMK)
    bsvhRead keys = liftIO $ vhSubmit vh (Tables.zipLedgerTablesA readLMDBTable_amk dbBackingTables keys) >>= \case
      Nothing -> Exn.throw DbErrBadRead
      Just x -> pure x

    bsvhRangeRead :: HD.RangeQuery (Tables.LedgerTables l Tables.KeysMK) -> m (Tables.LedgerTables l Tables.ValuesMK)
    bsvhRangeRead HD.RangeQuery{rqPrev, rqCount} = let
      transaction = case rqPrev of
        Nothing -> Tables.traverseLedgerTables (initRangeReadLMDBTable_amk rqCount) dbBackingTables
        Just keys -> Tables.zipLedgerTablesA (rangeReadLMDBTable_amk rqCount) dbBackingTables keys
      in liftIO $ vhSubmit vh transaction >>= \case
        Nothing -> Exn.throw DbErrBadRangeRead
        Just x -> pure x
  pure (init_slot, HD.BackingStoreValueHandle{..})
