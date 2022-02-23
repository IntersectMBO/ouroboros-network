{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

{-|
  = Snapshot managing and contents

  Snapshotting a ledger state means saving a copy of the in-memory part of the
  ledger state serialized as a file on disk, as well as flushing differences
  between the last snapshotted ledger state and the one that we are snapshotting
  now and making a copy of that resulting on-disk state. While the coupled
  implementation is in place, the serialized file might also contain a
  serialized version of the in-memory UTxO map carried by the legacy ledger
  states, as explained below.

  == Startup

  On startup, the node will:

  1. Find the latest snapshot which will be a directory identified by the slot
     number of the snapshot:

        > <cardano-node data dir>
        > ├── volatile
        > ├── immutable
        > └── ledger
        >     ├── <slotNumber1>
        >     │   ├── tables
        >     │   └── state
        >     ├── <slotNumber2>
        >     │   ├── tables
        >     │   └── state
        >     └── <slotNumber3>
        >         ├── tables
        >         └── state

  2. Depending on the snapshots found, there are two possibilities:

       - If there is no snapshot to load, create a new @'BackingStore'@ with the
         contents of the Genesis UTxO and finish.

       - If there is a snapshot found, then deserialize the @state@ file, which
         contains either:

            - If the node was running __with the legacy database enabled__, it
              contains a full LedgerState snapshot in the same style as before
              the UTxO HD implementation (i.e. in-memory part of the LedgerState
              (equal among implementations) + UTxO map).

            - Otherwise, it contains a serialization of the in-memory part of the
              LedgerState, with an empty UTxO map.

        In both cases, the snapshot will be deserialized as @ExtLedgerState blk
        EmptyMK@. In case we found an snapshot, we will overwrite (either
        literally overwriting it or using some feature the specific backend
        used) the @BackingStore@ tables with the @tables@ file from said
        snapshot as it was left in whatever state it was when the node shut
        down.

  3. Depending on whether the node is requested to run both databases and whether
     or not the snapshot has a UTxO inside, the node will:

       - __Run both__ + snapshot has UTxO inside: Extract the tables from the
         deserialized snapshot to create a @ExtLedgerState blk ValuesMK@ which
         can be 1. used in the legacy database and 2. after stripping out the
         @ValuesMK@, used in the modern database.

       - __Run both__ + snapshot has an empty UTxO map: an error will be
         thrown as we cannot reconstruct an in-memory UTxO directly from the
         @BackingStore@.

       - Otherwise: Strip out the values inside the @ExtLedgerState blk EmptyMK@
         to make sure that we don't have an in-memory UTxO map and use it as the
         anchor for the modern database.

  4. Replay on top of this ledger database all blocks up to the immutable
     database tip.

  At this point, the node carries a @LedgerDB@ that is initialized and ready to
  be applied blocks on the volatile database.

  == Taking snapshots during normal operation

  Snapshots are taken by the @'copyAndSnapshotRunner'@ when the disk policy
  dictates to do so. Whenever the chain grows past @k@ blocks, said runner will
  copy the blocks which are more than @k@ blocks from the tip (i.e. the ones
  that must be considered immutable) to the immutable database and then:

  1. Flush differences in the modern DB up to the immutable db tip.

  2. If dictated by the disk policy, serialize an @ExtLedgerState blk EmptyMK@ containing:

       - If the legacy database is enabled, the state to serialize is the anchor
         of the legacy database (which should be the same as the one from the
         modern database) with the tables stowed.

       - Otherwise, the state to serialize is the modern database anchor which
         requires no further modifications.

     A directory is created named after the slot number of the ledger state
     being snapshotted, and the serialization from above is written into the
     @\<slotNumber\>/state@ file and the @BackingStore@ tables are copied into
     the @\<slotNumber\>/tables@ file.

  3. There is a maximum number of snapshots that should exist in the disk at any
     time, dictated by the @DiskPolicy@, so if needed, we will trim out old
     snapshots.

  == Flush during startup and snapshot at the end of startup

  Due to the nature of the modern database having to carry around all the
  differences between the last snapshotted state and the current tip, there is a
  need to flush when replaying the chain as otherwise, for example on a replay
  from genesis to the tip, we would carry millions of differences in memory.

  Because of this, when we are replaying blocks we will flush regularly. As the
  last snapshot that was taken lives in a @\<slotNumber\>/tables@ file, there is
  no risk of destroying it (overwriting tables at another earlier snapshot) by
  flushing. Only when we finish replaying blocks and start the background
  threads (and specifically the @copyAndSnapshotRunner@), we will take a
  snapshot of the current immutable database anchor as described above.

-}
module Ouroboros.Consensus.Storage.LedgerDB.OnDisk (
    -- * Opening the database
    InitFailure (..)
  , InitLog (..)
  , initLedgerDB
    -- ** Instantiate in-memory to @blk@
  , AnnLedgerError'
  , LedgerDB'
    -- ** Abstraction over the stream API
  , NextBlock (..)
  , StreamAPI (..)
    -- * Abstraction over the ledger-state HD interface
  , LedgerBackingStore (..)
  , LedgerBackingStoreValueHandle (..)
  , flush
  , readKeySets
  , readKeySetsVH
    -- * Snapshots
    -- ** Read from disk
  , readSnapshot
    -- ** Write to disk
  , takeSnapshot
  , trimSnapshots
  , writeSnapshot
    -- ** Low-level API (primarily exposed for testing)
  , deleteSnapshot
  , snapshotToStatePath
  , snapshotToTablesPath
    -- ** Opaque
  , DiskSnapshot
    -- * Trace events
  , ReplayGoal (..)
  , ReplayStart (..)
  , TraceEvent (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
    -- * For testing
  , newBackingStore
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad.Except
import           Control.Tracer
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (isJust, mapMaybe)
import           Data.Ord (Down (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           Text.Read (readMaybe)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Slot (WithOrigin (..))

import           Ouroboros.Network.Block (Point (Point))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
                     (HeaderState (headerStateTip), annTipPoint)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     readIncremental)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as HD
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory

{-------------------------------------------------------------------------------
  Instantiate the in-memory DB to @blk@
-------------------------------------------------------------------------------}

type LedgerDB'       blk = LedgerDB       (ExtLedgerState blk)
type AnnLedgerError' blk = AnnLedgerError (ExtLedgerState blk) blk

{-------------------------------------------------------------------------------
  Abstraction over the streaming API provided by the Chain DB
-------------------------------------------------------------------------------}

-- | Next block returned during streaming
data NextBlock blk = NoMoreBlocks | NextBlock blk

-- | Stream blocks from the immutable DB
--
-- When we initialize the ledger DB, we try to find a snapshot close to the
-- tip of the immutable DB, and then stream blocks from the immutable DB to its
-- tip to bring the ledger up to date with the tip of the immutable DB.
--
-- In CPS form to enable the use of 'withXYZ' style iterator init functions.
data StreamAPI m blk = StreamAPI {
      -- | Start streaming after the specified block
      streamAfter :: forall a. HasCallStack
        => Point blk
        -- Reference to the block corresponding to the snapshot we found
        -- (or 'GenesisPoint' if we didn't find any)

        -> (Either (RealPoint blk) (m (NextBlock blk)) -> m a)
        -- Get the next block (by value)
        --
        -- Should be @Left pt@ if the snapshot we found is more recent than the
        -- tip of the immutable DB. Since we only store snapshots to disk for
        -- blocks in the immutable DB, this can only happen if the immutable DB
        -- got truncated due to disk corruption. The returned @pt@ is a
        -- 'RealPoint', not a 'Point', since it must always be possible to
        -- stream after genesis.
        -> m a
    }

-- | Stream all blocks
streamAll ::
     forall m blk e a. (Monad m, HasCallStack)
  => StreamAPI m blk
  -> Point blk             -- ^ Starting point for streaming
  -> (RealPoint blk -> e)  -- ^ Error when tip not found
  -> a                     -- ^ Starting point when tip /is/ found
  -> (blk -> a -> m a)     -- ^ Update function for each block
  -> ExceptT e m a
streamAll StreamAPI{..} tip notFound e f = ExceptT $
    streamAfter tip $ \case
      Left tip' -> return $ Left (notFound tip')

      Right getNext -> do
        let go :: a -> m a
            go a = do mNext <- getNext
                      case mNext of
                        NoMoreBlocks -> return a
                        NextBlock b  -> go =<< f b a
        Right <$> go e

{-------------------------------------------------------------------------------
  Initialize the DB
-------------------------------------------------------------------------------}

-- | Initialization log
--
-- The initialization log records which snapshots from disk were considered,
-- in which order, and why some snapshots were rejected. It is primarily useful
-- for monitoring purposes.
data InitLog blk =
    -- | Defaulted to initialization from genesis
    --
    -- NOTE: Unless the blockchain is near genesis, we should see this /only/
    -- if data corrupted occurred.
    InitFromGenesis

    -- | Used a snapshot corresponding to the specified tip
  | InitFromSnapshot DiskSnapshot (RealPoint blk)

    -- | Initialization skipped a snapshot
    --
    -- We record the reason why it was skipped.
    --
    -- NOTE: We should /only/ see this if data corrupted occurred.
  | InitFailure DiskSnapshot (InitFailure blk) (InitLog blk)
  deriving (Show, Eq, Generic)

-- | Internal errors which are recoverable and exposed through the chain of
-- failures recorded in 'InitLog'.
data InitFailure blk =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadIncrementalErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (RealPoint blk)

    -- | This snapshot was of the ledger state at genesis, even though we never
    -- take snapshots at genesis, so this is unexpected.
  | InitFailureGenesis
  deriving (Show, Eq, Generic)

-- | Initialize the ledger DB from the most recent snapshot on disk
--
-- If no such snapshot can be found, use the genesis ledger DB. Returns the
-- initialized DB as well as the block reference corresponding to the snapshot
-- we found on disk (the latter primarily for testing/monitoring purposes).
--
-- We do /not/ catch any exceptions thrown during streaming; should any be
-- thrown, it is the responsibility of the 'ChainDB' to catch these
-- and trigger (further) validation. We only discard snapshots if
--
-- * We cannot deserialise them, or
-- * they are /ahead/ of the chain
--
-- It is possible that the Ledger DB will not be able to roll back @k@ blocks
-- after initialization if the chain has been truncated (data corruption).
--
-- We do /not/ attempt to use multiple ledger states from disk to construct the
-- ledger DB. Instead we load only a /single/ ledger state from disk, and
-- /compute/ all subsequent ones. This is important, because the ledger states
-- obtained in this way will (hopefully) share much of their memory footprint
-- with their predecessors.
initLedgerDB ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , FromCBOR (LedgerTables (ExtLedgerState blk) ValuesMK)
       , ToCBOR   (LedgerTables (ExtLedgerState blk) ValuesMK)
       , HasCallStack
       )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> LedgerDbCfg (ExtLedgerState blk)
  -> m (ExtLedgerState blk ValuesMK) -- ^ Genesis ledger state
  -> StreamAPI m blk
  -> RunAlsoLegacy
  -> m (InitLog blk, LedgerDB' blk, Word64, LedgerBackingStore' m blk)
initLedgerDB replayTracer
             tracer
             hasFS
             decLedger
             decHash
             cfg
             getGenesisLedger
             streamAPI
             runAlsoLegacy = do
    listSnapshots hasFS >>= tryNewestFirst id
  where
    tryNewestFirst :: (InitLog blk -> InitLog blk)
                   -> [DiskSnapshot]
                   -> m ( InitLog   blk
                        , LedgerDB' blk
                        , Word64
                        , LedgerBackingStore' m blk
                        )
    tryNewestFirst acc [] = do
      -- We're out of snapshots. Start at genesis
      traceWith replayTracer ReplayFromGenesis
      genesisLedger <- getGenesisLedger
      let replayTracer' = decorateReplayTracerWithStart (Point Origin) replayTracer
          initDb        =
            case runAlsoLegacy of
              RunBoth    -> ledgerDbWithAnchor runAlsoLegacy (stowLedgerTables genesisLedger)
              RunOnlyNew -> ledgerDbWithAnchor runAlsoLegacy (forgetLedgerStateTables genesisLedger)
      backingStore <- newBackingStore hasFS (projectLedgerTables genesisLedger) -- TODO: needs to go into ResourceRegistry
      eDB <- runExceptT $ replayStartingWith
                            replayTracer'
                            cfg
                            backingStore
                            streamAPI
                            initDb
      case eDB of
        Left err -> error $ "Invariant violation: invalid immutable chain " <> show err
        Right (db, replayed) ->
          return ( acc InitFromGenesis
                 , db
                 , replayed
                 , backingStore
                 )

    tryNewestFirst acc (s:ss) = do
      eExtLedgerSt <- runExceptT $ readSnapshot hasFS decLedger decHash s
      case eExtLedgerSt of
        Left err -> do
          when (diskSnapshotIsTemporary s) $
            deleteSnapshot hasFS s
          traceWith tracer . InvalidSnapshot s . InitFailureRead $ err
          tryNewestFirst (acc . InitFailure s (InitFailureRead err)) ss
        Right extLedgerSt -> do
          let initialPoint =
                withOrigin (Point Origin) annTipPoint
                $ headerStateTip
                $ headerState
                $ extLedgerSt
          case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
            Origin        -> do
              -- Delete the snapshot of the Genesis ledger state. It should have
              -- never existed.
              deleteSnapshot hasFS s
              traceWith tracer . InvalidSnapshot s $ InitFailureGenesis
              tryNewestFirst (acc . InitFailure s InitFailureGenesis) []

            NotOrigin tip -> do
              backingStore <- restoreBackingStore hasFS s -- TODO this needs to go in the resource registry
              traceWith replayTracer $
                ReplayFromSnapshot s tip (ReplayStart initialPoint)
              let tracer' = decorateReplayTracerWithStart initialPoint replayTracer
                  initDb  = ledgerDbWithAnchor runAlsoLegacy extLedgerSt
              eDB <- runExceptT $ replayStartingWith
                                    tracer'
                                    cfg
                                    backingStore
                                    streamAPI
                                    initDb
              case eDB of
                Left err -> do
                  traceWith tracer . InvalidSnapshot s $ err
                  when (diskSnapshotIsTemporary s) $ deleteSnapshot hasFS s
                  tryNewestFirst (acc . InitFailure s err) ss
                Right (db, replayed) ->
                  return (acc (InitFromSnapshot s tip), db, replayed, backingStore)


-- | Replay all blocks in the Immutable database using the ''StreamAPI' provided
-- on top of the given @LedgerDB' blk@.
--
-- It will also return the number of blocks that were replayed.
replayStartingWith ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
  -> LedgerDbCfg (ExtLedgerState blk)
  -> LedgerBackingStore' m blk
  -> StreamAPI m blk
  -> LedgerDB' blk
  -> ExceptT (InitFailure blk) m (LedgerDB' blk, Word64)
replayStartingWith tracer cfg backingStore streamAPI initDb = do
    streamAll streamAPI (castPoint (ledgerDbTip initDb))
        InitFailureTooRecent
        (initDb, 0)
        push
  where
    push :: blk -> (LedgerDB' blk, Word64) -> m (LedgerDB' blk, Word64)
    push blk !(!db, !replayed) = do
        !db' <- defaultReadKeySets (readKeySets backingStore) $
                  ledgerDbPush cfg (ReapplyVal blk) db

        -- TODO flush policy: flush less often?

        -- It's OK to flush without a lock here, since the `LgrDB` has not
        -- finishined initializing: only this thread has access to the backing
        -- store.
        db'' <- do
          let (toFlush, toKeep) =
                ledgerDbFlush DbChangelogFlushAllImmutable db'
          flush backingStore toFlush
          pure toKeep

        -- TODO snapshot policy: create snapshots during replay?

        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg (ledgerDbCfg cfg))
                       (ledgerState (ledgerDbCurrent db))
                       (ledgerState (ledgerDbCurrent db''))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db'', replayed')

{-------------------------------------------------------------------------------
  BackingStore utilities
-------------------------------------------------------------------------------}

-- | Overwrite the ChainDB tables with the snapshot's tables
restoreBackingStore ::
     ( IOLike m
     , LedgerSupportsProtocol blk
     , FromCBOR (LedgerTables (ExtLedgerState blk) ValuesMK)
     , ToCBOR   (LedgerTables (ExtLedgerState blk) ValuesMK)
     )
  => SomeHasFS m
  -> DiskSnapshot
  -> m (LedgerBackingStore' m blk)
restoreBackingStore (SomeHasFS hasFS) snapshot = do
    -- TODO a backing store that actually resides on-disk during use should have
    -- a @new*@ function that receives both the location it should load from (ie
    -- 'loadPath') and also the location at which it should maintain itself (ie
    -- '_tablesPath'). Perhaps the specific logic could detect that the two are
    -- equivalent. Or perhaps a specific back-end might be able to efficiently
    -- reset the second location to the value of the first (a la @rsync@). Etc.
    -- The in-memory backing store, on the other hand, keeps nothing at
    -- '_tablesPath'.

    store <- HD.newTVarBackingStore
               (zipLedgerTables lookup_)
               (\rq values -> case HD.rqPrev rq of
                   Nothing   -> mapLedgerTables (rangeRead0_ (HD.rqCount rq))      values
                   Just keys -> zipLedgerTables (rangeRead_  (HD.rqCount rq)) keys values
               )
               (zipLedgerTables applyDiff_)
               toCBOR
               fromCBOR
               (Left (SomeHasFS hasFS, HD.BackingStorePath loadPath))
    pure (LedgerBackingStore store)
  where
    loadPath = snapshotToTablesPath snapshot

-- | Create a backing store from the given genesis ledger state
newBackingStore ::
     ( IOLike m
     , TableStuff l
     , NoThunks (LedgerTables l ValuesMK)
     , FromCBOR (LedgerTables l ValuesMK)
     , ToCBOR   (LedgerTables l ValuesMK)
     )
  => SomeHasFS m -> LedgerTables l ValuesMK -> m (LedgerBackingStore m l)
newBackingStore _someHasFS tables = do
    store <- HD.newTVarBackingStore
               (zipLedgerTables lookup_)
               (\rq values -> case HD.rqPrev rq of
                   Nothing   -> mapLedgerTables (rangeRead0_ (HD.rqCount rq))      values
                   Just keys -> zipLedgerTables (rangeRead_  (HD.rqCount rq)) keys values
               )
               (zipLedgerTables applyDiff_)
               toCBOR
               fromCBOR
               (Right (Origin, tables))
    pure (LedgerBackingStore store)

lookup_ ::
     Ord k
  => ApplyMapKind KeysMK   k v
  -> ApplyMapKind ValuesMK k v
  -> ApplyMapKind ValuesMK k v
lookup_ (ApplyKeysMK ks) (ApplyValuesMK vs) =
  ApplyValuesMK (HD.restrictValues vs ks)

rangeRead0_ ::
     Int
  -> ApplyMapKind ValuesMK k v
  -> ApplyMapKind ValuesMK k v
rangeRead0_ n (ApplyValuesMK (HD.UtxoValues vs)) =
  ApplyValuesMK $ HD.UtxoValues $ Map.take n vs

rangeRead_ ::
     Ord k
  => Int
  -> ApplyMapKind KeysMK   k v
  -> ApplyMapKind ValuesMK k v
  -> ApplyMapKind ValuesMK k v
rangeRead_ n prev (ApplyValuesMK (HD.UtxoValues vs)) =
    case Set.lookupMax ks of
      Nothing -> ApplyValuesMK $ HD.UtxoValues Map.empty
      Just  k -> ApplyValuesMK $ HD.UtxoValues $ Map.take n $ snd $ Map.split k vs
  where
    ApplyKeysMK (HD.UtxoKeys ks) = prev

applyDiff_ ::
     Ord k
  => ApplyMapKind ValuesMK k v
  -> ApplyMapKind DiffMK   k v
  -> ApplyMapKind ValuesMK k v
applyDiff_ (ApplyValuesMK values) (ApplyDiffMK diff) =
  ApplyValuesMK (HD.forwardValues values diff)

-- | A handle to the backing store for the ledger tables
newtype LedgerBackingStore m l = LedgerBackingStore
    (HD.BackingStore m
      (LedgerTables l KeysMK)
      (LedgerTables l ValuesMK)
      (LedgerTables l DiffMK)
    )
  deriving newtype (NoThunks)

-- | A handle to the backing store for the ledger tables
data LedgerBackingStoreValueHandle m l = LedgerBackingStoreValueHandle
    !(WithOrigin SlotNo)
    !(HD.BackingStoreValueHandle m
      (LedgerTables l KeysMK)
      (LedgerTables l ValuesMK)
    )
  deriving stock    (Generic)
  deriving anyclass (NoThunks)

type LedgerBackingStore' m blk = LedgerBackingStore m (ExtLedgerState blk)

readKeySets :: forall m l.
     (IOLike m, TableStuff l)
  => LedgerBackingStore m l
  -> RewoundTableKeySets l
  -> m (UnforwardedReadSets l)
readKeySets (LedgerBackingStore backingStore) rew = do
    readKeySetsVH (HD.bsRead backingStore) rew

readKeySetsVH :: forall m l.
     (IOLike m, TableStuff l)
  => (LedgerTables l KeysMK -> m (WithOrigin SlotNo, LedgerTables l ValuesMK))
  -> RewoundTableKeySets l
  -> m (UnforwardedReadSets l)
readKeySetsVH readKeys (RewoundTableKeySets _seqNo rew) = do
    (slot, values) <- readKeys (mapLedgerTables prj rew)
    pure UnforwardedReadSets {
        ursSeqNo  = slot
      , ursValues = zipLedgerTables comb rew values
    }
  where
    prj :: ApplyMapKind RewoundMK k v -> ApplyMapKind KeysMK k v
    prj (ApplyRewoundMK rew') = ApplyKeysMK (HD.rkUnknown rew')

    comb ::
         Ord k
      => ApplyMapKind RewoundMK k v
      -> ApplyMapKind ValuesMK k v
      -> ApplyMapKind ValuesMK k v
    comb (ApplyRewoundMK rew') (ApplyValuesMK vs) =
          ApplyValuesMK (HD.rkPresent rew' <> vs)

-- | Flush the immutable changes to the backing store
--
-- The 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.LgrDb'
-- 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.flushLock' write lock must be
-- held before calling this function.
flush ::
     (Applicative m, TableStuff l, GetTip (l EmptyMK))
  => LedgerBackingStore m l -> DbChangelog l -> m ()
flush (LedgerBackingStore backingStore) dblog =
    case youngestImmutableSlotDbChangelog dblog of
      Origin  -> pure ()   -- the diff is necessarily empty
      At slot ->
        HD.bsWrite
          backingStore
          slot
          (mapLedgerTables prj $ changelogDiffs dblog)
  where
    prj ::
         Ord k
      => ApplyMapKind SeqDiffMK k v
      -> ApplyMapKind DiffMK k v
    prj (ApplySeqDiffMK sq) = ApplyDiffMK (HD.cumulativeDiffSeqUtxoDiff sq)

{-------------------------------------------------------------------------------
  Disk snapshots
-------------------------------------------------------------------------------}

data DiskSnapshot = DiskSnapshot {
      -- | Snapshots are numbered. We will try the snapshots with the highest
      -- number first.
      --
      -- When creating a snapshot, we use the slot number of the ledger state it
      -- corresponds to as the snapshot number. This gives an indication of how
      -- recent the snapshot is.
      --
      -- Note that the snapshot names are only indicative, we don't rely on the
      -- snapshot number matching the slot number of the corresponding ledger
      -- state. We only use the snapshots numbers to determine the order in
      -- which we try them.
      dsNumber :: Word64

      -- | Snapshots can optionally have a suffix, separated by the snapshot
      -- number with an underscore, e.g., @4492799_last_Byron@. This suffix acts
      -- as metadata for the operator of the node. Snapshots with a suffix will
      -- /not be trimmed/.
    , dsSuffix :: Maybe String
    }
  deriving (Show, Eq, Ord, Generic)

-- | Named snapshot are permanent, they will never be deleted when trimming.
diskSnapshotIsPermanent :: DiskSnapshot -> Bool
diskSnapshotIsPermanent = isJust . dsSuffix

-- | The snapshots that are periodically created are temporary, they will be
-- deleted when trimming
diskSnapshotIsTemporary :: DiskSnapshot -> Bool
diskSnapshotIsTemporary = not . diskSnapshotIsPermanent

snapshotToDirName :: DiskSnapshot -> String
snapshotToDirName DiskSnapshot { dsNumber, dsSuffix } =
    show dsNumber <> suffix
  where
    suffix = case dsSuffix of
      Nothing -> ""
      Just s  -> "_" <> s

-- | The path within the LgrDB's filesystem to the snapshot's directory
snapshotToDirPath :: DiskSnapshot -> FsPath
snapshotToDirPath = mkFsPath . (:[]) . snapshotToDirName

-- | The path within the LgrDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

-- | The path within the LgrDB's filesystem to the directory that contains a
-- snapshot's backing store
snapshotToTablesPath :: DiskSnapshot -> FsPath
snapshotToTablesPath = mkFsPath . (\x -> [x, "tables"]) . snapshotToDirName

-- | The path within the LgrDB's filesystem to the directory that contains the
-- backing store
_tablesPath :: FsPath
_tablesPath = mkFsPath ["tables"]

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath fileName = do
    number <- readMaybe prefix
    return $ DiskSnapshot number suffix'
  where
    (prefix, suffix) = break (== '_') fileName

    suffix' :: Maybe String
    suffix' = case suffix of
      ""      -> Nothing
      _ : str -> Just str

{-------------------------------------------------------------------------------
  Snapshots: Writing to disk
-------------------------------------------------------------------------------}

-- | Take a snapshot of the /oldest ledger state/ in the ledger DB
--
-- We write the /oldest/ ledger state to disk because the intention is to only
-- write ledger states to disk that we know to be immutable. Primarily for
-- testing purposes, 'takeSnapshot' returns the block reference corresponding
-- to the snapshot that we wrote.
--
-- If a snapshot with the same number already exists on disk or if the tip is at
-- genesis, no snapshot is taken.
--
-- Note that an EBB can have the same slot number and thus snapshot number as
-- the block after it. This doesn't matter. The one block difference in the
-- ledger state doesn't warrant an additional snapshot. The number in the name
-- of the snapshot is only indicative, we don't rely on it being correct.
--
-- NOTE: This is a lower-level API that takes a snapshot independent from
-- whether this snapshot corresponds to a state that is more than @k@ back.
--
-- TODO: Should we delete the file if an error occurs during writing?
takeSnapshot ::
     forall m blk.
     ( MonadThrow m
     , IsLedger (LedgerState blk)
     , LedgerSupportsProtocol blk
     )
  => Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> LedgerBackingStore' m blk
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> LedgerDB' blk
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot tracer hasFS backingStore encLedger db =
    case pointToWithOriginRealPoint (castPoint (getTip oldest)) of
      Origin ->
        return Nothing
      NotOrigin tip -> do
        let number   = unSlotNo (realPointSlot tip)
            snapshot = DiskSnapshot number Nothing
        snapshots <- listSnapshots hasFS
        if List.any ((== number) . dsNumber) snapshots then
          return Nothing
        else do
          writeSnapshot hasFS backingStore encLedger snapshot oldest
          traceWith tracer $ TookSnapshot snapshot tip
          return $ Just (snapshot, tip)
  where
    oldest :: ExtLedgerState blk EmptyMK
    oldest = ledgerDbOldest db

-- | Write snapshot to disk
writeSnapshot ::
     forall m blk. MonadThrow m
  => SomeHasFS m
  -> LedgerBackingStore' m blk
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk EmptyMK
  -> m ()
writeSnapshot (SomeHasFS hasFS) backingStore encLedger snapshot cs = do
    createDirectory hasFS (snapshotToDirPath snapshot)
    withFile hasFS (snapshotToStatePath snapshot) (WriteMode MustBeNew) $ \h ->
      void $ hPut hasFS h $ CBOR.toBuilder (encode cs)
    HD.bsCopy
      (let LedgerBackingStore store = backingStore in store)
      (SomeHasFS hasFS)
      (HD.BackingStorePath (snapshotToTablesPath snapshot))
  where
    encode :: ExtLedgerState blk EmptyMK -> Encoding
    encode = encodeSnapshot encLedger

-- | Trim the number of on disk snapshots so that at most 'onDiskNumSnapshots'
-- snapshots are stored on disk. The oldest snapshots are deleted.
--
-- The deleted snapshots are returned.
trimSnapshots ::
     Monad m
  => Tracer m (TraceEvent blk)
  -> SomeHasFS m
  -> DiskPolicy
  -> m [DiskSnapshot]
trimSnapshots tracer hasFS DiskPolicy{..} = do
    -- We only trim temporary snapshots
    snapshots <- filter diskSnapshotIsTemporary <$> listSnapshots hasFS
    -- The snapshot are most recent first, so we can simply drop from the
    -- front to get the snapshots that are "too" old.
    forM (drop (fromIntegral onDiskNumSnapshots) snapshots) $ \snapshot -> do
      deleteSnapshot hasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot
      return snapshot

-- | Delete snapshot from disk
deleteSnapshot :: HasCallStack => SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot (SomeHasFS HasFS{..}) =
    removeDirectoryRecursive . snapshotToDirPath

{-------------------------------------------------------------------------------
  Snapshots: reading from disk
-------------------------------------------------------------------------------}

-- | Read snapshot from disk
readSnapshot ::
     forall m blk. IOLike m
  => SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DiskSnapshot
  -> ExceptT ReadIncrementalErr m (ExtLedgerState blk EmptyMK)
readSnapshot hasFS decLedger decHash = do
      ExceptT
    . readIncremental hasFS decoder
    . snapshotToStatePath
  where
    decoder :: Decoder s (ExtLedgerState blk EmptyMK)
    decoder = decodeSnapshotBackwardsCompatible (Proxy @blk) decLedger decHash

-- | List on-disk snapshots, highest number first.
listSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
listSnapshots (SomeHasFS HasFS{..}) =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortOn (Down . dsNumber) . mapMaybe snapshotFromPath . Set.toList

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

data TraceEvent blk
  = InvalidSnapshot DiskSnapshot (InitFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (RealPoint blk)
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted
  deriving (Generic, Eq, Show)

-- | Add the tip of the Immutable DB to the trace event
--
-- Between the tip of the immutable DB and the point of the starting block,
-- the node could (if it so desired) easily compute a "percentage complete".
decorateReplayTracerWithGoal
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceReplayEvent blk)
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithGoal immTip = contramap ($ (ReplayGoal immTip))

-- | Add the block at which a replay started.
--
-- This allows to compute a "percentage complete" when tracing the events.
decorateReplayTracerWithStart
  :: Point blk -- ^ Starting point of the replay
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithStart start = contramap ($ (ReplayStart start))

-- | Which point the replay started from
newtype ReplayStart blk = ReplayStart (Point blk) deriving (Eq, Show)

-- | Which point the replay is expected to end at
newtype ReplayGoal blk = ReplayGoal (Point blk) deriving (Eq, Show)

-- | Events traced while replaying blocks against the ledger to bring it up to
-- date w.r.t. the tip of the ImmutableDB during initialisation. As this
-- process takes a while, we trace events to inform higher layers of our
-- progress.
data TraceReplayEvent blk =
    -- | There were no LedgerDB snapshots on disk, so we're replaying all blocks
    -- starting from Genesis against the initial ledger.
    ReplayFromGenesis
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
    -- | There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
  | ReplayFromSnapshot
        DiskSnapshot      -- ^ The snapshot we are replaying from
        (RealPoint blk)   -- ^ The tip at which the snapshot is
        (ReplayStart blk) -- ^ the block at which this replay started
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  -- | We replayed the given block (reference) on the genesis snapshot during
  -- the initialisation of the LedgerDB. Used during ImmutableDB replay.
  | ReplayedBlock
        (RealPoint blk)   -- ^ the block being replayed
        [LedgerEvent blk]
        (ReplayStart blk) -- ^ the block at which this replay started
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  deriving (Generic, Eq, Show)
