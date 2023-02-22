{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LedgerDB initialization either from a LedgerState or from a DiskSnapshot
module Ouroboros.Consensus.Storage.LedgerDB.Init (
    -- * Initialization
    BackingStoreSelector (..)
  , InitLog (..)
  , ReplayStart (..)
  , initialize
  , newBackingStore
  , newBackingStoreInitialiser
  , restoreBackingStore
    -- * Trace
  , ReplayGoal (..)
  , TraceLedgerDBEvent (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
  ) where

import           Codec.Serialise.Decoding (Decoder)
import           Control.Monad.Except
import           Control.Tracer
import qualified Data.Map.Diff.Strict.Internal as Diff.Internal
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Network.Block (Point (Point))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
                     (HeaderState (headerStateTip), annTipPoint)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Storage.FS.API

import           Data.Functor.Contravariant ((>$<))
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Impl
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.InMemory as BackingStore
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB as LMDB
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.LedgerDB as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.Query
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.Update (Ap (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.Update as LedgerDB

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
  | InitFailure DiskSnapshot (SnapshotFailure blk) (InitLog blk)
  deriving (Show, Eq, Generic)

data TraceLedgerDBEvent blk =
    LedgerDBSnapshotEvent (TraceSnapshotEvent blk)
  | BackingStoreEvent (BackingStoreTrace)
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
initialize ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , CanSerializeLedgerTables (LedgerState blk)
       , HasCallStack
       )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceLedgerDBEvent blk)
  -> SomeHasFS m
  -> ResourceRegistry m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> LedgerDB.LedgerDbCfg (ExtLedgerState blk)
  -> m (ExtLedgerState blk ValuesMK) -- ^ Genesis ledger state
  -> StreamAPI m blk blk
  -> BackingStoreSelector m
  -> m (InitLog blk, LedgerDB' blk, Word64, LedgerBackingStore' m blk)
initialize replayTracer
           tracer
           hasFS
           reg
           decLedger
           decHash
           cfg
           getGenesisLedger
           streamAPI
           bss =
    listSnapshots hasFS >>= tryNewestFirst id
  where
    lbsi ::
         (HasLedgerTables l, NoThunks (LedgerTables l ValuesMK)
         , CanSerializeLedgerTables l)
      => BackingStoreInitializer m l
    lbsi = newBackingStoreInitialiser (BackingStoreEvent >$< tracer) bss

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
          initDb        = LedgerDB.mkWithAnchor (forgetLedgerTables genesisLedger)
      (_, backingStore) <- allocate
                             reg
                             (\_ -> newBackingStore lbsi hasFS (projectLedgerTables genesisLedger))
                             (\(LedgerBackingStore bs) -> bsClose bs)
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
          traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s . InitFailureRead $ err
          tryNewestFirst (acc . InitFailure s (InitFailureRead err)) ss
        Right extLedgerSt -> do
          let initialPoint =
                  withOrigin (Point Origin) annTipPoint
                . headerStateTip
                . headerState
                $ extLedgerSt
          case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
            Origin        -> do
              -- Delete the snapshot of the Genesis ledger state. It should have
              -- never existed.
              deleteSnapshot hasFS s
              traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s $ InitFailureGenesis
              tryNewestFirst (acc . InitFailure s InitFailureGenesis) []

            NotOrigin pt -> do
              (_, backingStore) <-
                allocate
                  reg
                  (\_ -> restoreBackingStore lbsi hasFS s)
                  (\(LedgerBackingStore bs) -> bsClose bs)
              traceWith replayTracer $
                ReplayFromSnapshot s pt (ReplayStart initialPoint)
              let tracer' = decorateReplayTracerWithStart initialPoint replayTracer
                  initDb  = LedgerDB.mkWithAnchor extLedgerSt
              eDB <- runExceptT $ replayStartingWith
                                    tracer'
                                    cfg
                                    backingStore
                                    streamAPI
                                    initDb
              case eDB of
                Left err -> do
                  traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s $ err
                  when (diskSnapshotIsTemporary s) $ deleteSnapshot hasFS s
                  tryNewestFirst (acc . InitFailure s err) ss
                Right (db, replayed) ->
                  return (acc (InitFromSnapshot s pt), db, replayed, backingStore)


-- | Replay all blocks in the Immutable database using the 'StreamAPI' provided
-- on top of the given @LedgerDB' blk@.
--
-- It will also return the number of blocks that were replayed.
--
-- Note we do flush differences into the 'BackingStore' as we go, but we don't
-- take snapshots of the in-memory parts.
--
-- TODO: #4402 expose the flushing frequence as a configuration
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
  -> StreamAPI m blk blk
  -> LedgerDB' blk
  -> ExceptT (SnapshotFailure blk) m (LedgerDB' blk, Word64)
replayStartingWith tracer cfg backingStore streamAPI initDb = do
    (\(a, b, _) -> (a, b)) <$> streamAll streamAPI (castPoint (tip initDb))
        InitFailureTooRecent
        (initDb, 0, 0)
        push
  where
    push :: blk -> (LedgerDB' blk, Word64, Word64) -> m (LedgerDB' blk, Word64, Word64)
    push blk (!db, !replayed, !sinceLast) = do
        !db' <- LedgerDB.push cfg (ReapplyVal blk) (readKeySets backingStore) db

        -- It's OK to flush without a lock here, since the `LgrDB` has not
        -- finishined initializing: only this thread has access to the backing
        -- store.
        (db'', sinceLast') <-
          if sinceLast == 100
          then do
            let (toFlush, toKeep) =
                  LedgerDB.flush FlushAllImmutable db'
            flushIntoBackingStore backingStore toFlush
            pure (toKeep, 0)
          else pure (db', sinceLast + 1)

        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg (LedgerDB.ledgerDbCfg cfg))
                       (ledgerState (current db))
                       (ledgerState (current db''))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db'', replayed', sinceLast')

{-------------------------------------------------------------------------------
  BackingStore utilities
-------------------------------------------------------------------------------}

type BackingStoreInitializer m l =
     SomeHasFS m
  -> InitFrom (LedgerTables l ValuesMK)
  -> m (BackingStore m (LedgerTables l KeysMK) (LedgerTables l ValuesMK) (LedgerTables l DiffMK))

-- | Overwrite the ChainDB tables with the snapshot's tables
restoreBackingStore ::
     IOLike m
  => BackingStoreInitializer m l
  -> SomeHasFS m
  -> DiskSnapshot
  -> m (LedgerBackingStore m l)
restoreBackingStore bsi someHasFs snapshot = do
    LedgerBackingStore <$> bsi someHasFs (InitFromCopy (BackingStore.BackingStorePath loadPath))
  where
    loadPath = snapshotToTablesPath snapshot

-- | Create a backing store from the given genesis ledger state
newBackingStore ::
     IOLike m
  => BackingStoreInitializer m l
  -> SomeHasFS m
  -> LedgerTables l ValuesMK
  -> m (LedgerBackingStore m l)
newBackingStore bsi someHasFS tables = do
    LedgerBackingStore <$> bsi someHasFS (InitFromValues Origin tables)
  where

newBackingStoreInitialiser ::
     ( IOLike m
     , NoThunks (LedgerTables l ValuesMK)
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     )
  => Tracer m BackingStoreTrace
  -> BackingStoreSelector m
  -> BackingStoreInitializer m l
newBackingStoreInitialiser tracer bss =
  case bss of
    LMDBBackingStore limits ->
      LMDB.newLMDBBackingStoreInitialiser
        (LMDBTrace >$< tracer)
        limits
    InMemoryBackingStore -> BackingStore.newTVarBackingStoreInitialiser
      (InMemoryTrace >$< tracer)
      (zipLedgerTables lookup_)
      (\rq values -> case BackingStore.rqPrev rq of
          Nothing   ->
            mapLedgerTables (rangeRead0_ (BackingStore.rqCount rq))      values
          Just keys ->
            zipLedgerTables (rangeRead_  (BackingStore.rqCount rq)) keys values
      )
      (zipLedgerTables applyDiff_)
      valuesMKEncoder
      valuesMKDecoder
  where
    lookup_ ::
         Ord k
      => KeysMK   k v
      -> ValuesMK k v
      -> ValuesMK k v
    lookup_ (KeysMK ks) (ValuesMK vs) =
      ValuesMK (Map.restrictKeys vs ks)

    rangeRead0_ ::
         Int
      -> ValuesMK k v
      -> ValuesMK k v
    rangeRead0_ n (ValuesMK vs) =
      ValuesMK $ Map.take n vs

    rangeRead_ ::
         Ord k
      => Int
      -> KeysMK   k v
      -> ValuesMK k v
      -> ValuesMK k v
    rangeRead_ n (KeysMK ks) (ValuesMK vs) =
        case Set.lookupMax ks of
          Nothing -> ValuesMK Map.empty
          Just  k -> ValuesMK  $ Map.take n $ snd $ Map.split k vs

    applyDiff_ ::
         Ord k
      => ValuesMK k v
      -> DiffMK   k v
      -> ValuesMK k v
    applyDiff_ (ValuesMK values) (DiffMK diff) =
      ValuesMK (Diff.Internal.unsafeApplyDiff values diff)

-- | The backing store selector
data BackingStoreSelector m where
  LMDBBackingStore     :: MonadIO m => !LMDB.LMDBLimits -> BackingStoreSelector m
  InMemoryBackingStore ::                                  BackingStoreSelector m

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

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
data TraceReplayEvent blk
  = -- | There were no LedgerDB snapshots on disk, so we're replaying all blocks
    -- starting from Genesis against the initial ledger.
    ReplayFromGenesis
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
    -- | There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
  | ReplayFromSnapshot
        DiskSnapshot
        (RealPoint blk)
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
