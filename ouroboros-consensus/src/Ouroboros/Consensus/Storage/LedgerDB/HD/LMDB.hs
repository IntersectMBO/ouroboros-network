{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB (
    DbErr
  , LMDBBackingStore
  , LMDBInit (..)
  , LMDBValueHandle
  , TraceDb (..)
  , newLMDBBackingStore
    -- * Configuration
  , LMDBLimits (LMDBLimits, lmdbMapSize, lmdbMaxDatabases, lmdbMaxReaders)
    -- * Exported for ledger-db-backends-checker
  , DbState (..)
  , LMDBMK (..)
  , foldrWithKey
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toStrictByteString)
import qualified Codec.Serialise as S (Serialise (..))
import           Control.Exception (assert)
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
import qualified Data.Text as Strict
import           Foreign (Ptr, alloca, castPtr, copyBytes, peek)
import           GHC.Generics (Generic)

import           Cardano.Slotting.Slot (SlotNo, WithOrigin (At))

import           Ouroboros.Consensus.Ledger.Basics
import qualified Ouroboros.Consensus.Storage.FS.API as FS
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as HD
import           Ouroboros.Consensus.Util (foldlM')
import           Ouroboros.Consensus.Util.IOLike (Exception (..), IOLike,
                     MonadCatch (..), MonadThrow (..), bracket, onException)

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
  | DbErrDirExists !FS.FsPath
    -- | A database directory should exist already.
  | DbErrDirDoesntExist !FS.FsPath

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
  , dbOpenHandles   :: !(IOLike.TVar m (Map Int (ValueHandle m)))
  }

newVHId :: Map Int (ValueHandle m) -> Int
newVHId openHdls = maybe 0 ((+1) . fst) $ Map.lookupMax openHdls

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

-- | ValueHandles hold a transaction open.
data ValueHandle m = ValueHandle
  { vhClose :: !(m ())
  , vhSubmit :: !(forall a. LMDB.Transaction LMDB.ReadOnly (Maybe a) -> m (Maybe a))
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

data LMDBMK k v = LMDBMK String !(LMDB.Database k v)

getDb ::
     LMDB.Mode mode
  => NameMK k v
  -> LMDB.Transaction mode (LMDBMK k v)
getDb (NameMK name) = LMDBMK name <$> LMDB.getDatabase (Just name)

-- | @`lmdbInitRangeReadTable` count db0@ performs a range read of roughly
-- @count@ values from database @db0@, starting at the smallest key.
--
-- Note: See @`RangeQuery`@ for more information about range queries.
--
-- Todo(jdral): Test this function; could be buggy.
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

-- | @`lmdbRangeReadTable` count db0 ks@ performs a range read of roughly
-- @count@ values from database @db0@, starting at the largest key in @ks@.
--
-- Note: See @`RangeQuery`@ for more information about range queries.
--
-- Todo(jdral): Test this function; could be buggy.
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
          Just lastExcludedKey ->
            let
              wrangle = either HD.UtxoValues HD.UtxoValues
              go k v acc
                | k <= lastExcludedKey = acc
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
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK k v)
initLMDBTable (LMDBMK tblName db) codecMK (ApplyValuesMK (HD.UtxoValues utxoVals)) =
    ApplyEmptyMK <$ lmdbInitTable
  where
    lmdbInitTable  = do
      isEmpty <- LMDB.null db
      unless isEmpty $ liftIO . throwIO $ DbErrInitialisingNonEmpty tblName
      void $ Map.traverseWithKey
                 (\k v -> put codecMK db k (Just v))
                 utxoVals

--  Todo(jdral/dnadales): Would it be possible to issue a single @C@ call to
-- get a bunch of keys in one go? We are currently folding over the set of
-- keys and issuing a @get@ for each one separately. LMDB cursors could be
-- of help here.
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
  -> LMDB.Transaction LMDB.ReadWrite (EmptyMK k v)
writeLMDBTable (LMDBMK _ db) codecMK (ApplyDiffMK (HD.UtxoDiff diff)) =
    ApplyEmptyMK <$ lmdbWriteTable
  where
    lmdbWriteTable = void $ Map.traverseWithKey go diff
      where
        go k (HD.UtxoEntryDiff v reason) = put codecMK db k value
          where
            value = case reason of
              HD.UedsDel ->
                Nothing
              HD.UedsInsAndDel ->
                Nothing
              HD.UedsIns ->
                Just v

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
  case dirEx of
    True  | GDDMustNotExist <- gdd -> throwIO $ DbErrDirExists path
    False | GDDMustExist    <- gdd -> throwIO $ DbErrDirDoesntExist path
    _                              -> pure ()
  FS.createDirectoryIfMissing fs True path
  FS.unsafeToFilePath fs path

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
newLMDBBackingStore ::
     forall m l. (TableStuff l, SufficientSerializationForAnyBackingStore l, MonadIO m, IOLike m)
  => Trace.Tracer m TraceDb
  -> LMDBLimits
     -- ^ Configuration parameters for the LMDB database that we
     -- initialise. In case we initialise the LMDB database from
     -- an existing LMDB database, we use these same configuration parameters
     -- to open the existing LMDB database.
  -> FS.SomeHasFS m
     -- ^ Abstraction over the filesystem.
  -> LMDBInit l
     -- ^ Determines how the LMDB database should be initialised.
  -> m (LMDBBackingStore l m)
newLMDBBackingStore dbTracer limits sfs initDb = do
  Trace.traceWith dbTracer TDBOpening

  dbOpenHandles <- IOLike.newTVarIO Map.empty
  let
    path = FS.mkFsPath ["tables"]

    (gdd, copyDbAction :: m (), initAction :: Db m l -> m ()) = case initDb of
      LIInitialiseFromLMDB fp
        -- If fp == path then this is the LINoInitialise case
        | fp /= path -> (GDDMustNotExist, initFromLMDBs dbTracer limits sfs fp path, \_ -> pure ())
      LIInitialiseFromMemory slot vals -> (GDDMustNotExist, pure (), initFromVals slot vals)
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

  let
    db = Db{..}
    bsClose :: m ()
    bsClose = do
      Trace.traceWith dbTracer $ TDBClosing dbFilePath
      openHandles <- IOLike.atomically $ IOLike.readTVar dbOpenHandles
      for_ openHandles vhClose
      liftIO $ LMDB.closeEnvironment dbEnv
      Trace.traceWith dbTracer $ TDBClosed dbFilePath

    bsCopy shfs (HD.BackingStorePath to0) = do
      to <- guardDbDir GDDMustNotExist shfs to0
      lmdbCopy dbTracer dbEnv to

    bsValueHandle = mkLMDBBackingStoreValueHandle db

    bsWrite :: SlotNo -> LedgerTables l DiffMK -> m ()
    bsWrite slot diffs = do
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


data TraceValueHandle
  = TVHOpened
  | TVHClosing
  | TVHClosed
  | TVHSubmissionStarted
  | TVHSubmissionEnded
  deriving stock(Show, Eq)

-- | Create a value handle that has a consistent view of the current database
-- state (i.e., the database contents, not to be confused with @`DbState`@).
mkValueHandle :: forall m. (MonadIO m, IOLike m)
  => Trace.Tracer m TraceDb
  -> LMDB.Environment LMDB.ReadOnly
     -- ^ The read-only environment in which the LMDB database lives.
  -> IOLike.TVar m (Map Int (ValueHandle m))
     -- ^ Value handles that are already open.
  -> m (ValueHandle m)
mkValueHandle dbTracer0 dbEnv dbOpenHandles = do
  let LMDB.Internal.Env env = dbEnv
  readOnlyTx <- liftIO $ LMDB.mdb_txn_begin env Nothing (LMDB.Internal.isReadOnlyEnvironment dbEnv)

  (r, traces) <- IOLike.atomically $ IOLike.stateTVar dbOpenHandles $ \x0 ->
    let
      vhId = newVHId x0
      tracer = Trace.contramap (TDBValueHandle vhId) dbTracer0

      vhSubmit :: forall a. LMDB.Transaction LMDB.ReadOnly (Maybe a) -> m (Maybe a)
      vhSubmit (LMDB.Internal.Txn t) = do
        present <- Map.member vhId <$> IOLike.atomically (IOLike.readTVar dbOpenHandles)
        unless present $ throwIO $ DbErrNoValueHandle vhId
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
        IOLike.atomically $ IOLike.modifyTVar' dbOpenHandles (Map.delete vhId)
        Trace.traceWith tracer TVHClosed
      vh = ValueHandle{..}
    in
      ( (vh, [TDBValueHandle vhId TVHOpened] )
      , Map.insert vhId vh x0 )
  for_ traces $ Trace.traceWith dbTracer0
  pure r

-- | Create a backing store value handle that has a consistent view of the
-- current database state (i.e., the database contents, not to be confused
-- with @`DbState`@).
--
-- Note(jdral): A backing store value handle is different with respect to
-- a value handle in the sense that @`ValueHandle`@ exposes an interface
-- for submitting LMDB transactions to the LMDB database, while the
-- @`LMDBValueHandle`@ exposes an interface for performing reads and range
-- queries /using/ a value handle perform them.
mkLMDBBackingStoreValueHandle ::
     forall l m. (TableStuff l, SufficientSerializationForAnyBackingStore l, MonadIO m, IOLike m)
  => Db m l
     -- ^ The LMDB database for which the backing store value handle is
     -- created.
  -> m (WithOrigin SlotNo, LMDBValueHandle l m)
mkLMDBBackingStoreValueHandle Db{..} = do
  let
    dbEnvRo = LMDB.readOnlyEnvironment dbEnv
  vh <- mkValueHandle dbTracer dbEnvRo dbOpenHandles
  mbInitSlot <- vhSubmit vh $ readDbStateMaybeNull dbState
  initSlot <- liftIO $ maybe (throwIO $ DbErrStr "mkLMDBBackingStoreValueHandle ") (pure . dbsSeq) mbInitSlot
  let
    bsvhClose :: m ()
    bsvhClose = vhClose vh

    bsvhRead :: LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)
    bsvhRead keys = vhSubmit vh (Just <$> zipLedgerTables2A readLMDBTable dbBackingTables codecLedgerTables keys)
        >>= \case
              Nothing -> throwIO DbErrBadRead
              Just x  -> pure x

    bsvhRangeRead :: HD.RangeQuery (LedgerTables l KeysMK) -> m (LedgerTables l ValuesMK)
    bsvhRangeRead HD.RangeQuery{rqPrev, rqCount} = let
      transaction = Just <$> case rqPrev of
        Nothing -> zipLedgerTablesA (initRangeReadLMDBTable rqCount) dbBackingTables codecLedgerTables
        Just keys -> zipLedgerTables2A (rangeReadLMDBTable rqCount) dbBackingTables codecLedgerTables keys
      in vhSubmit vh transaction >>= \case
        Nothing -> throwIO DbErrBadRangeRead
        Just x  -> pure x
  pure (initSlot, HD.BackingStoreValueHandle{..})
