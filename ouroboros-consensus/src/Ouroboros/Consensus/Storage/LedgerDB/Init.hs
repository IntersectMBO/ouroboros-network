{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LedgerDB initialization either from a LedgerState or from a DiskSnapshot
module Ouroboros.Consensus.Storage.LedgerDB.Init (
    -- * Initialization
    InitLog (..)
  , ReplayStart (..)
  , initLedgerDB
    -- * Trace
  , ReplayGoal (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
  ) where

import           Codec.Serialise.Decoding (Decoder)
import           Control.Monad.Except
import           Control.Tracer
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           System.FS.API

import           Ouroboros.Network.Block (Point (Point))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
                     (HeaderState (headerStateTip), annTipPoint)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.Query
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.Update

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
       , HasCallStack
       )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (HeaderHash blk))
  -> LedgerDbCfg (ExtLedgerState blk)
  -> m (ExtLedgerState blk) -- ^ Genesis ledger state
  -> StreamAPI m blk
  -> m (InitLog blk, LedgerDB' blk, Word64)
initLedgerDB replayTracer
             tracer
             hasFS
             decLedger
             decHash
             cfg
             getGenesisLedger
             streamAPI = do
    snapshots <- listSnapshots hasFS
    tryNewestFirst id snapshots
  where
    tryNewestFirst :: (InitLog blk -> InitLog blk)
                   -> [DiskSnapshot]
                   -> m (InitLog blk, LedgerDB' blk, Word64)
    tryNewestFirst acc [] = do
        -- We're out of snapshots. Start at genesis
        traceWith replayTracer ReplayFromGenesis
        initDb <- ledgerDbWithAnchor <$> getGenesisLedger
        let replayTracer' = decorateReplayTracerWithStart (Point Origin) replayTracer
        ml     <- runExceptT $ initStartingWith replayTracer' cfg streamAPI initDb
        case ml of
          Left _  -> error "invariant violation: invalid current chain"
          Right (l, replayed) -> return (acc InitFromGenesis, l, replayed)
    tryNewestFirst acc (s:ss) = do
        -- If we fail to use this snapshot, delete it and try an older one
        ml <- runExceptT $ initFromSnapshot
                             replayTracer
                             hasFS
                             decLedger
                             decHash
                             cfg
                             streamAPI
                             s
        case ml of
          Left err -> do
            when (diskSnapshotIsTemporary s) $
              -- We don't delete permanent snapshots, even if we couldn't parse
              -- them
              deleteSnapshot hasFS s
            traceWith tracer $ InvalidSnapshot s err
            tryNewestFirst (acc . InitFailure s err) ss
          Right (r, l, replayed) ->
            return (acc (InitFromSnapshot s r), l, replayed)

{-------------------------------------------------------------------------------
  Internal: initialize using the given snapshot
-------------------------------------------------------------------------------}

-- | Attempt to initialize the ledger DB from the given snapshot
--
-- If the chain DB or ledger layer reports an error, the whole thing is aborted
-- and an error is returned. This should not throw any errors itself (ignoring
-- unexpected exceptions such as asynchronous exceptions, of course).
initFromSnapshot ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (HeaderHash blk))
  -> LedgerDbCfg (ExtLedgerState blk)
  -> StreamAPI m blk
  -> DiskSnapshot
  -> ExceptT (SnapshotFailure blk) m (RealPoint blk, LedgerDB' blk, Word64)
initFromSnapshot tracer hasFS decLedger decHash cfg streamAPI ss = do
    initSS <- withExceptT InitFailureRead $
                readSnapshot hasFS decLedger decHash ss
    let initialPoint = withOrigin (Point Origin) annTipPoint $ headerStateTip $ headerState $ initSS
    case pointToWithOriginRealPoint (castPoint (getTip initSS)) of
      Origin        -> throwError InitFailureGenesis
      NotOrigin tip -> do
        lift $ traceWith tracer $ ReplayFromSnapshot ss tip (ReplayStart initialPoint)
        let tracer' = decorateReplayTracerWithStart initialPoint tracer
        (initDB, replayed) <-
          initStartingWith
            tracer'
            cfg
            streamAPI
            (ledgerDbWithAnchor initSS)
        return (tip, initDB, replayed)

-- | Attempt to initialize the ledger DB starting from the given ledger DB
initStartingWith ::
     forall m blk. (
         Monad m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
  -> LedgerDbCfg (ExtLedgerState blk)
  -> StreamAPI m blk
  -> LedgerDB' blk
  -> ExceptT (SnapshotFailure blk) m (LedgerDB' blk, Word64)
initStartingWith tracer cfg streamAPI initDb = do
    streamAll streamAPI (castPoint (ledgerDbTip initDb))
      InitFailureTooRecent
      (initDb, 0)
      push
  where
    push :: blk -> (LedgerDB' blk, Word64) -> m (LedgerDB' blk, Word64)
    push blk !(!db, !replayed) = do
        !db' <- ledgerDbPush cfg (ReapplyVal blk) db

        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg (ledgerDbCfg cfg))
                       (ledgerState (ledgerDbCurrent db))
                       (ledgerState (ledgerDbCurrent db'))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db', replayed')

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
