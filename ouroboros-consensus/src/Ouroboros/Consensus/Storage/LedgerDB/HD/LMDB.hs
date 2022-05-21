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

module Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB (
    DbErr
  , LMDBBackingStore
  , LMDBInit (..)
  , LMDBLimits
  , LMDBValueHandle
  , TraceDb (..)
  , defaultLMDBLimits
  , newLMDBBackingStore
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toStrictByteString)
import qualified Codec.Serialise as S (Serialise (..))
import           Control.Exception (assert)
import qualified Control.Exception as Exn
import           Control.Monad (unless, void, when, (>=>))
import qualified Control.Monad.Class.MonadSTM as IOLike
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Control.Tracer as Trace
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Foldable (for_)
import           Data.Functor (($>), (<&>))
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Foreign (Ptr, alloca, castPtr, copyBytes, peek)
import           GHC.Generics (Generic)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (At))

import           Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Storage.FS.API as FS
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as HD
import           Ouroboros.Consensus.Util (foldlM')
import           Ouroboros.Consensus.Util.IOLike (IOLike, bracket, onException)

import qualified Database.LMDB.Raw as LMDB
import qualified Database.LMDB.Simple as LMDB
import qualified Database.LMDB.Simple.Extra as LMDB
import qualified Database.LMDB.Simple.Internal as LMDB.Internal

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
  | TDBCopyingFile !FilePath !FilePath
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

data DbErr =
    DbErrStr !String
  | DbErrNoSettings
  | DbErrNoDbState
  -- | The sequence number of a @`Db`@ should be monotonically increasing
  -- across calls to @`bsWrite`@, since we use @`bsWrite`@ to flush
  -- /immutable/ changes.
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

data Db m l = Db {
    -- | The LMDB environment is a pointer to the directory that contains the DB.
    dbEnv           :: !(LMDB.Environment LMDB.ReadWrite)
  , dbSettings      :: !(LMDB.Database () DbSettings)
    -- | The on-disk state of the @`Db`@.
    --
    -- The state is itself an LDMB database with only one key and one value:
    -- The current sequence number of the DB.
  , dbState         :: !(LMDB.Database () DbState)
    -- | The LMDB database with the key-value store.
  , dbBackingTables :: !(LedgerTables l LMDBMK)
  , dbFilePath      :: !FilePath
  , dbTracer        :: !(Trace.Tracer m TraceDb)
  , dbOpenHandles   :: !(IOLike.TVar m (Map Int (ValueHandle m)))
  }

type LMDBLimits = LMDB.Limits

defaultLMDBLimits :: LMDBLimits
defaultLMDBLimits = LMDB.defaultLimits
  { LMDB.mapSize = 6_000_000_000
  , LMDB.maxDatabases = 10 -- We use 1 database per field + one for the state (i.e. sidetable)
  , LMDB.maxReaders = 16
  }

-- TODO a version field.
-- TODO(jdral): I've made a distinction between DbSettings and DbState because
-- /settings/ should probably not be updated on every flush.
data DbSettings = DbSettings

newtype DbState = DbState {
    dbsSeq :: WithOrigin SlotNo
  } deriving (Show, Generic)

instance S.Serialise DbState

-- | ValueHandles hold an Async which is holding a transaction open.
data ValueHandle m = ValueHandle
  { vhClose :: !(m ())
  , vhSubmit :: !(forall a. LMDB.Transaction LMDB.ReadOnly (Maybe a) -> m (Maybe a))
  -- , vhRefCount :: !(IOLike.TVar IO Int)
  }

{-------------------------------------------------------------------------------
 LMDB Interface that uses CodecMK
-------------------------------------------------------------------------------}

get ::
     CodecMK k v
  -> LMDB.Database k v
  -> k
  -> LMDB.Transaction mode (Maybe v)
get (CodecMK encKey _ _ decVal) db k =
  fmap (fmap (deserialiseBS "a value" decVal)) $ getBS db (toStrictByteString (encKey k))

put ::
     CodecMK k v
  -> LMDB.Database k v
  -> k
  -> Maybe v
  -> LMDB.Transaction LMDB.ReadWrite ()
put (CodecMK encKey encVal _ _) db k =
    maybe (void $ LMDB.Internal.deleteBS db keyBS) (putBS db keyBS . toStrictByteString . encVal)
  where
    keyBS = toStrictByteString (encKey k)

foldrWithKey ::
     (k -> v -> b -> b)
  -> b
  -> CodecMK k v
  -> LMDB.Database k v
  -> LMDB.Transaction mode b
foldrWithKey f b (CodecMK _ _ decKey decVal) = foldrWithKeyBS fBS b
  where
    fBS keyBS valBS =
      f (deserialiseBS "a key" decKey keyBS) (deserialiseBS "a value" decVal valBS)

-- | Deserialise a 'BS.ByteString' using the provided decoder.
deserialiseBS ::
     String
  -- ^ Label to be used for error reporting. This should describe the value to be deserialised.
  -> (forall s . CBOR.Decoder s a)
  -> BS.ByteString
  -> a
deserialiseBS label decoder bs = either err snd $ deserialiseFromBytes decoder $ LBS.fromStrict bs
  where
    err = error $ "foldrWithKey: error deserialising " ++ label ++ " from the database."

{-------------------------------------------------------------------------------
 Alternatives to LMDB operations that do not rely on Serialise instances

 We cannot (easily and without runtime overhead) satisfy the Serialise
 constraints that the LMDB.Simple operations require. We have access to the
 codification and decodification functions provided in CodecMK, thus, we operate
 directly on ByteStrings.

 TODO: we might want to submit a patch against the upstream LMDB simple package
 with these new functions.
-------------------------------------------------------------------------------}

getBS ::
     LMDB.Database k v
  -> BS.ByteString
  -> LMDB.Transaction mode (Maybe BS.ByteString)
getBS db k = LMDB.Internal.getBS' db k >>=
    maybe (return Nothing) (liftIO . fmap Just . marshalInBS)

putBS ::
     LMDB.Database k v
  -> BS.ByteString
  -> BS.ByteString
  -> LMDB.Transaction LMDB.ReadWrite ()
putBS (LMDB.Internal.Db _ dbi) keyBS valueBS =  LMDB.Internal.Txn $ \txn ->
    LMDB.Internal.marshalOutBS keyBS $ \kval -> do
      let sz = BS.length valueBS
      LMDB.MDB_val len ptr <- LMDB.mdb_reserve' LMDB.Internal.defaultWriteFlags txn dbi kval sz
      let len' = fromIntegral len
      assert (len' == sz) $
        unsafeUseAsCStringLen valueBS $
          \(bsp, lenToCopy) -> copyBytes ptr (castPtr bsp) lenToCopy

foldrWithKeyBS ::
     (BS.ByteString -> BS.ByteString -> b -> b)
  -> b
  -> LMDB.Database k v
  -> LMDB.Transaction mode b
foldrWithKeyBS f z (LMDB.Internal.Db _ dbi)  = LMDB.Internal.Txn $ \txn ->
    alloca $ \kptr ->
    alloca $ \vptr ->
      LMDB.Internal.forEachForward txn dbi kptr vptr z $ \rest ->
        f <$> peekVal' kptr <*> peekVal' vptr <*> rest
  where
    peekVal' :: Ptr LMDB.MDB_val -> IO BS.ByteString
    peekVal' = peek >=> marshalInBS

marshalInBS :: LMDB.MDB_val -> IO BS.ByteString
marshalInBS (LMDB.MDB_val len ptr) = BS.packCStringLen (castPtr ptr, fromIntegral len)

{-------------------------------------------------------------------------------
  LMDB Interface specialized for ApplyMapKinds
-------------------------------------------------------------------------------}

getDb ::
     LMDB.Mode mode
  => NameMK k v
  -> LMDB.Transaction mode (LMDBMK   k v)
getDb (NameMK name) = LMDBMK name <$> LMDB.getDatabase (Just name)

initRangeReadLMDBTable ::
     Ord k
  => Int
  -> LMDBMK  k v
  -> CodecMK k v
  -> LMDB.Transaction mode (ValuesMK k v)
initRangeReadLMDBTable count (LMDBMK _ db) codecMK =
    ApplyValuesMK <$> lmdbInitRangeReadTable
  where
    lmdbInitRangeReadTable =
      -- This is inadequate. We are folding over the whole table, the
      -- fiddling with either is to short circuit as best we can.
      -- TODO improve lmdb-simple bindings to give better access to cursors
      wrangle <$> foldrWithKey go (Right Map.empty) codecMK db
      where
        wrangle = either HD.UtxoValues HD.UtxoValues
        go k v acc = do
          m <- acc
          when (Map.size m >= count) $ Left m
          pure $ Map.insert k v m

rangeReadLMDBTable ::
     Ord k
  => Int
  -> LMDBMK  k v
  -> CodecMK k v
  -> KeysMK  k v
  -> LMDB.Transaction mode (ValuesMK k v)
rangeReadLMDBTable count (LMDBMK _ db) codecMK (ApplyKeysMK (HD.UtxoKeys keys)) =
    ApplyValuesMK <$> lmdbRangeReadTable
  where
    lmdbRangeReadTable =
      case Set.lookupMax keys of
          -- This is inadequate. We are folding over the whole table, the
          -- fiddling with either is to short circuit as best we can.
          -- TODO improve llvm-simple bindings to give better access to cursors
          Nothing -> pure $ HD.UtxoValues Map.empty
          Just last_excluded_key ->
            let
              wrangle = either HD.UtxoValues HD.UtxoValues
              go k v acc
                | k <= last_excluded_key = acc
                | otherwise = do
                    m <- acc
                    when (Map.size m >= count) $ Left m
                    pure $ Map.insert k v m
            in
              wrangle <$> foldrWithKey go (Right Map.empty) codecMK db

initLMDBTable ::
     LMDBMK   k v
  -> CodecMK  k v
  -> ValuesMK k v
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK  k v)
initLMDBTable (LMDBMK tbl_name db) codecMK (ApplyValuesMK (HD.UtxoValues utxoVals)) =
    ApplyEmptyMK <$ lmdbInitTable
  where
    lmdbInitTable  = do
      is_empty <- LMDB.null db
      unless is_empty $ Exn.throw $ DbErrInitialisingNonEmpty tbl_name
      void $ Map.traverseWithKey
                 (\k v -> put codecMK db k (Just v))
                 utxoVals

readLMDBTable ::
     Ord k
  => LMDBMK  k v
  -> CodecMK k v
  -> KeysMK  k v
  -> LMDB.Transaction mode (ValuesMK k v)
readLMDBTable (LMDBMK _ db) codecMK (ApplyKeysMK (HD.UtxoKeys keys)) =
    ApplyValuesMK <$> lmdbReadTable
  where
    lmdbReadTable = HD.UtxoValues <$> foldlM' go Map.empty (Set.toList keys)
      where
        go m k = get codecMK db k <&> \case
          Nothing -> m
          Just v  -> Map.insert k v m

writeLMDBTable ::
     LMDBMK  k v
  -> CodecMK k v
  -> DiffMK  k v
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK  k v)
writeLMDBTable (LMDBMK _ db) codecMK (ApplyDiffMK (HD.UtxoDiff diff)) =
    ApplyEmptyMK <$ lmdbWriteTable
  where
    lmdbWriteTable = void $ Map.traverseWithKey go diff
      where
        go k (HD.UtxoEntryDiff v reason) = put codecMK db k value
          where
            value = if reason `elem` [HD.UedsDel, HD.UedsInsAndDel]
                    then Nothing
                    else Just v

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
withDbStateRW db f = withDbStateRWMaybeNull db $ maybe (Exn.throw DbErrNoDbState) f

withDbStateRWMaybeNull ::
      LMDB.Database () DbState
   -> (Maybe DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState))
   -> LMDB.Transaction LMDB.ReadWrite a
withDbStateRWMaybeNull db f  =
  readDbStateMaybeNull db >>= f >>= \(r, new_s) -> LMDB.put db () (Just new_s) $> r

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
     (TableStuff l, SufficientSerializationForAnyBackingStore l, MonadIO m)
  => WithOrigin SlotNo
  -> LedgerTables l ValuesMK
  -> Db m l
  -> m ()
initFromVals dbsSeq vals Db{..} = liftIO $ LMDB.readWriteTransaction dbEnv $
  withDbStateRWMaybeNull dbState $ \case
    Nothing -> zipLedgerTables2A initLMDBTable dbBackingTables codecLedgerTables vals
                $> ((), DbState{dbsSeq})
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
    let LMDB.Internal.Env e = dbEnv
    from <- liftIO $ LMDB.mdb_env_get_path e
    Trace.traceWith tracer $ TDBCopying from to
    liftIO $ LMDB.mdb_env_copy e to
    -- others <- liftIO $ Dir.listDirectory from <&> filter ((/= ".mdb") . Dir.takeExtension)
    -- for_ others $ \f0 -> do
    --   let f = from Dir.</> f0
    --       t = to Dir.</> f0
    --   Trace.traceWith tracer $ TDBCopyingFile f t
    --   liftIO $ Dir.copyFile f t
    Trace.traceWith tracer $ TDBCopied from to

-- TODO We don't know how much time is spent in I/O

-- TODO 50% of total time is spent somewhere else. Where?

-- | Initialise a backing store
newLMDBBackingStore ::
     forall m l. (TableStuff l, SufficientSerializationForAnyBackingStore l, MonadIO m, IOLike m)
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

  dbSettings <- liftIO $ LMDB.readWriteTransaction dbEnv $ LMDB.getDatabase (Just "_dbsettings")
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
      old_slot <- liftIO $ LMDB.readWriteTransaction dbEnv $ withDbStateRW dbState $ \s@DbState{dbsSeq} -> do
        -- TODO This should be <. However the test harness does call bsWrite with the same slot
        unless (dbsSeq <= At slot) $ Exn.throw $ DbErrNonMonotonicSeq (At slot) dbsSeq
        void $ zipLedgerTables2A writeLMDBTable dbBackingTables codecLedgerTables diffs
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
  let LMDB.Internal.Env env = dbEnv
  readOnlyTx <- liftIO $ LMDB.mdb_txn_begin env Nothing (LMDB.Internal.isReadOnlyEnvironment dbEnv)

  (r, traces) <- IOLike.atomically $ IOLike.stateTVar dbOpenHandles $ \x0 ->
    let
      vh_id = maybe 0 ((+1) . fst) $ Map.lookupMax x0
      tracer = Trace.contramap (TDBValueHandle vh_id) dbTracer0

      vhSubmit :: forall a. LMDB.Transaction LMDB.ReadOnly (Maybe a) -> m (Maybe a)
      vhSubmit (LMDB.Internal.Txn t) = do
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

mkLMDBBackingStoreValueHandle ::
     forall l m. (TableStuff l, SufficientSerializationForAnyBackingStore l, MonadIO m, IOLike m)
  => Db m l
  -> m (WithOrigin SlotNo, LMDBValueHandle l m)
mkLMDBBackingStoreValueHandle Db{..} = do
  let
    dbe = LMDB.readOnlyEnvironment dbEnv
  vh <- mkValueHandle dbTracer dbe dbOpenHandles
  mb_init_slot <- vhSubmit vh $ readDbStateMaybeNull dbState
  init_slot <- liftIO $ maybe (Exn.throwIO $ DbErrStr "mkLMDBBackingStoreValueHandle ") (pure . dbsSeq) mb_init_slot
  let
    bsvhClose :: m ()
    bsvhClose = vhClose vh

    bsvhRead :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
    bsvhRead keys = vhSubmit vh (Just <$> zipLedgerTables2A readLMDBTable dbBackingTables codecLedgerTables keys)
        >>= \case
              Nothing -> Exn.throw DbErrBadRead
              Just x  -> pure x

    bsvhRangeRead :: HD.RangeQuery (LedgerTables l KeysMK) -> m (LedgerTables l ValuesMK)
    bsvhRangeRead HD.RangeQuery{rqPrev, rqCount} = let
      transaction = Just <$> case rqPrev of
        Nothing -> zipLedgerTablesA (initRangeReadLMDBTable rqCount) dbBackingTables codecLedgerTables
        Just keys -> zipLedgerTables2A (rangeReadLMDBTable rqCount) dbBackingTables codecLedgerTables keys
      in vhSubmit vh transaction >>= \case
        Nothing -> Exn.throw DbErrBadRangeRead
        Just x  -> pure x
  pure (init_slot, HD.BackingStoreValueHandle{..})
