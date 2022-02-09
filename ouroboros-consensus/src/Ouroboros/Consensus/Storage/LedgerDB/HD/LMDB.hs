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
  , DbErr
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
import Codec.Serialise (Serialise)
import Data.Map (Map)
import qualified Data.Set as Set
import Ouroboros.Consensus.Util (foldlM')
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Database.LMDB.Simple.Extra as LMDB
import Data.Foldable (for_)
import Database.LMDB.Simple (ReadWrite)
import Cardano.Prelude (isJust)
import qualified Ouroboros.Consensus.Storage.FS.API.Types as FS

{-

TODO Don't use DbRef

-}
{-# HLINT ignore #-}

type LMDBBackingStore l = HD.BackingStore IO
  (Tables.LedgerTables l Tables.KeysMK)
  (Tables.LedgerTables l Tables.ValuesMK)
  (Tables.LedgerTables l Tables.DiffMK)
-- type LMDBBackingStore k v = HD.BackingStore IO (UtxoKeys k v) (UtxoValues k v) (UtxoDiff k v)

type LMDBValueHandle l = HD.BackingStoreValueHandle IO
  (Tables.LedgerTables l Tables.KeysMK)
  (Tables.LedgerTables l Tables.ValuesMK)
  


data Db l = Db
  { dbEnv :: !(LMDB.Environment LMDB.ReadWrite)
  , dbSettings :: !(LMDB.DBRef LMDB.ReadWrite DbState)
  , dbValueHandles :: !(IOLike.TMVar IO (Map (WithOrigin SlotNo) ValueHandle))
  , dbBackingTables :: !(Tables.LedgerTables l Tables.LMDBMK)
  , dbFilePath :: !FilePath
  }

data DbErr = DbErrStr !String
  | DbErrNoSettings
  | DbErrNonMonotonicSeq !(WithOrigin SlotNo) !(WithOrigin SlotNo)
  | DbErrNoDbNamed !String
  | DbErrBadDynamic !String
  | DbErrInitialisingNonEmpty !String
  | DbErrBadRead
  deriving stock (Show)

instance Exn.Exception DbErr

data DbState  = DbState
  { dbsSeq :: !(WithOrigin SlotNo)
  } deriving (Show, Generic)

instance Serialise (DbState)

readLMDBTable :: (Ord k, Serialise k, Serialise v)
  => LMDB.Database k v
  -> HD.UtxoKeys k v
  -> LMDB.Transaction ReadWrite (HD.UtxoValues k v)
readLMDBTable db (HD.UtxoKeys keys) =
  HD.UtxoValues <$> foldlM' go Map.empty (Set.toList keys) where
  go m k = LMDB.get db k <&> \case
    Nothing -> m
    Just v -> Map.insert k v m

writeLMDBTable :: (Serialise k, Serialise v)
  => LMDB.Database k v
  -> HD.UtxoDiff k v
  -> LMDB.Transaction LMDB.ReadWrite ()
writeLMDBTable db (HD.UtxoDiff m) = void $ Map.traverseWithKey go m where
  go k (HD.UtxoEntryDiff v reason) = LMDB.put db k new_val where
    new_val
      | reason `elem` [HD.UedsDel, HD.UedsInsAndDel]
      = Nothing
      | otherwise = Just v

initLMDBTable :: (Serialise k, Serialise v)
  => String
  -> LMDB.Database k v
  -> HD.UtxoValues k v
  -> LMDB.Transaction LMDB.ReadWrite ()
initLMDBTable tbl_name db (HD.UtxoValues m) = do
  is_empty <- LMDB.null db
  unless is_empty $ Exn.throw $ DbErrInitialisingNonEmpty tbl_name
  void $ let
    go k v = LMDB.put db k (Just v) 
    in Map.traverseWithKey go m
    
getDb_amk :: ( LMDB.Mode mode)
  =>Tables.ApplyMapKind Tables.NamesMK k v
  -> LMDB.Transaction mode (Tables.ApplyMapKind Tables.LMDBMK k v)
getDb_amk (Tables.ApplyNamesMK name) = Tables.ApplyLMDBMK name <$>
  LMDB.getDatabase (Just name)
  
initLMDBTable_amk :: (Serialise k, Serialise v)
  => Tables.ApplyMapKind Tables.LMDBMK k v
  -> Tables.ApplyMapKind Tables.ValuesMK k v
  -> LMDB.Transaction ReadWrite (Tables.ApplyMapKind Tables.EmptyMK k v)
initLMDBTable_amk (Tables.ApplyLMDBMK tbl_name db) (Tables.ApplyValuesMK vals) =
  initLMDBTable tbl_name db vals $> Tables.ApplyEmptyMK 

readLMDBTable_amk :: (Ord k, Serialise k, Serialise v)
  => Tables.ApplyMapKind Tables.LMDBMK k v
  -> Tables.ApplyMapKind Tables.KeysMK k v
  -> LMDB.Transaction ReadWrite (Tables.ApplyMapKind Tables.ValuesMK k v)
readLMDBTable_amk (Tables.ApplyLMDBMK _ db) (Tables.ApplyKeysMK keys) =
  Tables.ApplyValuesMK <$> readLMDBTable db keys

writeLMDBTable_amk :: (Serialise k, Serialise v)
  => Tables.ApplyMapKind Tables.LMDBMK k v
  -> Tables.ApplyMapKind Tables.DiffMK k v
  -> LMDB.Transaction ReadWrite (Tables.ApplyMapKind Tables.EmptyMK k v)
writeLMDBTable_amk (Tables.ApplyLMDBMK _tbl_name db) (Tables.ApplyDiffMK diff) =
  writeLMDBTable db diff $> Tables.ApplyEmptyMK 

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
  , LMDB.maxDatabases = 10
  , LMDB.maxReaders = 16
  }

-- | Initialise a backing store
-- An initial value should be passed if and only if this store is being
-- initialised for the first time
-- This isn't quite right: We also need to be able to take a snapshot to load from
newLMDBBackingStore :: forall l. (Tables.TableStuff l)
  => (FS.SomeHasFS IO, FS.FsPath)
  -> Maybe (WithOrigin SlotNo, Tables.LedgerTables l Tables.ValuesMK)   -- ^ initial seqno and contents. Just iff db is empty
  -> IO (LMDBBackingStore l)
newLMDBBackingStore (sfs, path) mb_init = do
  let
    limits = defaultLMDBLimits
    needs_init = isJust mb_init
  dbFilePath <- guardDbDir (if needs_init then GDDMustNotExist else GDDMustExist) sfs path
  dbEnv <- LMDB.openEnvironment dbFilePath limits

  dbstate_db <- LMDB.readOnlyTransaction dbEnv $ LMDB.getDatabase (Just "_dbstate")

  dbSettings <- LMDB.newDBRef dbEnv dbstate_db (0 :: Int)
  dbValueHandles <- IOLike.newTMVarIO Map.empty
  dbBackingTables <- LMDB.readOnlyTransaction dbEnv $
    Tables.traverseLedgerTables getDb_amk Tables.namesLedgerTables

  -- initLMDBTable will fail on trying to init a non-empty table
  for_ mb_init $ \(seqno, vals) -> LMDB.readWriteTransaction dbEnv $ do
    void $ Tables.zip2ALedgerTables initLMDBTable_amk dbBackingTables vals
    liftIO $ LMDB.writeDBRef dbSettings (Just $ DbState seqno)
  let
    db = Db{..}
    bsClose = do
      mb_vhs <- IOLike.atomically $ IOLike.tryTakeTMVar dbValueHandles
      for_ mb_vhs $ traverse forceCloseValueHandle

    bsCopy = lmdbCopy db
    
    bsValueHandle = getLMDBBackingStoreValueHandle db

    bsWrite :: SlotNo -> Tables.LedgerTables l Tables.DiffMK -> IO ()
    bsWrite slot diffs = LMDB.readWriteTransaction dbEnv $
      withDbSettingsRW dbSettings $ \s@DbState{dbsSeq} -> do
        when (At slot <= dbsSeq) $ Exn.throw $ DbErrNonMonotonicSeq (At slot) dbsSeq
        void $ Tables.zip2ALedgerTables writeLMDBTable_amk dbBackingTables diffs
        pure ((), s {dbsSeq = At slot})

  pure HD.BackingStore{..}


-- | ValueHandles hold an Async which is holding a transaction open.
-- We maintain a map of them in the Db, (keyed on their Seq No) and
-- they have a reference counting TVar.
-- This means that multiple BackingStoreValueHandles will reference the
-- same ValueHandle and thus we keep only one transaction per seq no alive.
data ValueHandle = ValueHandle
  { vhAsync :: !(IOLike.Async IO ())
  , vhSubmit :: !(forall a. LMDB.Transaction LMDB.ReadWrite a -> IO (Maybe a))
  , vhRefCount :: !(IOLike.TVar IO Int)
  }

closeValueHandle :: ValueHandle -> IO Bool
closeValueHandle ValueHandle{..} = do
  should <- IOLike.atomically $ do
    IOLike.modifyTVar vhRefCount (\x -> x - 1)
    x <- IOLike.readTVar vhRefCount
    pure $ x <= 0
  when should $ IOLike.cancel vhAsync
  pure should

-- TODO we should specify what happens to the ref count
forceCloseValueHandle :: ValueHandle -> IO ()
forceCloseValueHandle ValueHandle{..} = IOLike.cancel vhAsync

data SomeDbSubmission where
  SomeDbSubmission :: forall a. LMDB.Transaction LMDB.ReadWrite a -> IOLike.TMVar IO (Maybe a) -> SomeDbSubmission

withValueHandles :: IOLike.TMVar IO (Map (WithOrigin SlotNo) ValueHandle)
  -> (Map (WithOrigin SlotNo) ValueHandle
  -> IO (a, Map (WithOrigin SlotNo) ValueHandle))
  -> IO a
withValueHandles tmv op = Exn.bracketOnError
  (IOLike.atomically $ IOLike.takeTMVar tmv)
  (IOLike.atomically . IOLike.tryPutTMVar tmv) $ \x -> do
    (r, s) <- op x
    IOLike.atomically $ IOLike.putTMVar tmv s $> r

backingStoreValueHandle :: forall l. (Tables.TableStuff l)
  => IOLike.TMVar IO (Map (WithOrigin SlotNo) ValueHandle)
  -> WithOrigin SlotNo
  -> ValueHandle
  -> Tables.LedgerTables l Tables.LMDBMK
  -> IO (LMDBValueHandle l)
backingStoreValueHandle tmv slot vh tbls = withValueHandles tmv $ \hs -> do
  let
    (h@ValueHandle{..}, new_hs) = case slot `Map.lookup` hs of
      Nothing -> let
        in (vh, Map.insert slot vh hs)
      Just vh' -> do
        (vh', hs)
    bsvhClose = do
      is_last_handle <- closeValueHandle h
      when is_last_handle $ withValueHandles tmv $ pure . ((),) . Map.delete slot
    bsvhRead :: Tables.LedgerTables l Tables.KeysMK -> IO (Tables.LedgerTables l Tables.ValuesMK)
    bsvhRead keys = vhSubmit (Tables.zip2ALedgerTables readLMDBTable_amk tbls keys) >>= \case
      Nothing -> Exn.throw DbErrBadRead
      Just x -> pure x

  r <- IOLike.atomically $ do
    IOLike.modifyTVar vhRefCount (+1)
    pure $ HD.BackingStoreValueHandle {..}
  pure (r, new_hs)

getLMDBBackingStoreValueHandle :: forall l. Tables.TableStuff l
  => Db l
  -> IO (WithOrigin SlotNo, LMDBValueHandle l)
getLMDBBackingStoreValueHandle Db{..} = do
  q <- IOLike.newTBQueueIO 1
  init_slot_tmv <- IOLike.newEmptyTMVarIO
  vhAsync <- IOLike.async $ LMDB.readWriteTransaction dbEnv $ do
    this_state <- readDbSettings dbSettings
    liftIO $ IOLike.atomically $ IOLike.putTMVar init_slot_tmv (dbsSeq this_state)
    let
      loop :: MaybeT (LMDB.Transaction LMDB.ReadWrite) ()
      loop = do
        SomeDbSubmission t ret_tmvar <- MaybeT . liftIO $ IOLike.atomically (IOLike.readTBQueue q)
        mb_r <- lift $ LMDB.nestTransaction t
        liftIO . IOLike.atomically $ IOLike.putTMVar ret_tmvar mb_r
    void . runMaybeT $ forever loop
    LMDB.abort

  let
    vhSubmit :: forall a. LMDB.Transaction LMDB.ReadWrite a -> IO (Maybe a)
    vhSubmit t = do
      ret_tmvar <- IOLike.newEmptyTMVarIO
      IOLike.atomically . IOLike.writeTBQueue q . Just $ SomeDbSubmission t ret_tmvar
      IOLike.atomically $ IOLike.takeTMVar ret_tmvar
  vh <- do
    vhRefCount <- IOLike.newTVarIO 1
    pure ValueHandle{..}

  init_slot <- IOLike.atomically $ IOLike.takeTMVar init_slot_tmv
  (init_slot,) <$> backingStoreValueHandle dbValueHandles init_slot vh dbBackingTables

-- data LMDBField k v where
--   LMDBField :: (Serialise k, Serialise v, Typeable k, Typeable v) => LMDB.Database k v -> LMDBField k v

lmdbCopy = undefined

readDbSettings :: LMDB.DBRef LMDB.ReadWrite DbState -> LMDB.Transaction mode DbState
readDbSettings dbSettings = liftIO (LMDB.readDBRef dbSettings) >>= \case
  Just x -> pure x
  Nothing -> Exn.throw $ DbErrNoSettings

withDbSettingsRW :: LMDB.DBRef LMDB.ReadWrite DbState -> (DbState -> LMDB.Transaction LMDB.ReadWrite (a, DbState)) -> LMDB.Transaction LMDB.ReadWrite a
withDbSettingsRW dbSettings f  =
  readDbSettings dbSettings >>= f >>= \(r, new_s) ->
    liftIO $ LMDB.writeDBRef dbSettings (Just new_s) $> r

