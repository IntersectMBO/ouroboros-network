{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Node.Tracers
  ( -- * All tracers of a node bundled together
    Tracers' (..)
  , Tracers
  , nullTracers
  , showTracers
    -- * Specific tracers
  , TraceForgeEvent (..)
  , TraceLabelCreds (..)
  ) where

import           Control.Tracer (Tracer, nullTracer, showTracing)
import           Data.Text (Text)
import           Data.Time (UTCTime)

import           Ouroboros.Network.BlockFetch (FetchDecision,
                     TraceFetchClientState, TraceLabelPeer)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient)
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Forecast (OutsideForecastRange)
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Node.Types

import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (InvalidBlockReason, TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                     (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))

{-------------------------------------------------------------------------------
  All tracers of a node bundled together
-------------------------------------------------------------------------------}

data Tracers' remotePeer localPeer blk f = Tracers
  { chainSyncClientTracer         :: f (TraceChainSyncClientEvent blk)
  , chainSyncServerHeaderTracer   :: f (TraceChainSyncServerEvent blk)
  , chainSyncServerBlockTracer    :: f (TraceChainSyncServerEvent blk)
  , blockFetchDecisionTracer      :: f [TraceLabelPeer remotePeer (FetchDecision CandidateDecline [Point (Header blk)])]
  , blockFetchClientTracer        :: f (TraceLabelPeer remotePeer (TraceFetchClientState (Header blk)))
  , blockFetchServerTracer        :: f (TraceBlockFetchServerEvent blk)
  , txInboundTracer               :: f (TraceLabelPeer remotePeer (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk)))
  , txOutboundTracer              :: f (TraceLabelPeer remotePeer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)))
  , localTxSubmissionServerTracer :: f (TraceLocalTxSubmissionServerEvent blk)
  , mempoolTracer                 :: f (TraceEventMempool blk)
  , forgeTracer                   :: f (TraceLabelCreds (TraceForgeEvent blk))
  , blockchainTimeTracer          :: f (TraceBlockchainTimeEvent UTCTime)
  , forgeStateInfoTracer          :: f (TraceLabelCreds (ForgeStateInfo blk))
  , keepAliveClientTracer         :: f (TraceKeepAliveClient remotePeer)
  }

instance (forall a. Semigroup (f a))
      => Semigroup (Tracers' remotePeer localPeer blk f) where
  l <> r = Tracers
      { chainSyncClientTracer         = f chainSyncClientTracer
      , chainSyncServerHeaderTracer   = f chainSyncServerHeaderTracer
      , chainSyncServerBlockTracer    = f chainSyncServerBlockTracer
      , blockFetchDecisionTracer      = f blockFetchDecisionTracer
      , blockFetchClientTracer        = f blockFetchClientTracer
      , blockFetchServerTracer        = f blockFetchServerTracer
      , txInboundTracer               = f txInboundTracer
      , txOutboundTracer              = f txOutboundTracer
      , localTxSubmissionServerTracer = f localTxSubmissionServerTracer
      , mempoolTracer                 = f mempoolTracer
      , forgeTracer                   = f forgeTracer
      , blockchainTimeTracer          = f blockchainTimeTracer
      , forgeStateInfoTracer          = f forgeStateInfoTracer
      , keepAliveClientTracer         = f keepAliveClientTracer
      }
    where
      f :: forall a. Semigroup a
        => (Tracers' remotePeer localPeer blk f -> a) -> a
      f prj = prj l <> prj r

-- | A record of 'Tracer's for the node.
type Tracers m remotePeer localPeer blk =
     Tracers'  remotePeer localPeer blk (Tracer m)

-- | Use a 'nullTracer' for each of the 'Tracer's in 'Tracers'
nullTracers :: Monad m => Tracers m remotePeer localPeer blk
nullTracers = Tracers
    { chainSyncClientTracer         = nullTracer
    , chainSyncServerHeaderTracer   = nullTracer
    , chainSyncServerBlockTracer    = nullTracer
    , blockFetchDecisionTracer      = nullTracer
    , blockFetchClientTracer        = nullTracer
    , blockFetchServerTracer        = nullTracer
    , txInboundTracer               = nullTracer
    , txOutboundTracer              = nullTracer
    , localTxSubmissionServerTracer = nullTracer
    , mempoolTracer                 = nullTracer
    , forgeTracer                   = nullTracer
    , blockchainTimeTracer          = nullTracer
    , forgeStateInfoTracer          = nullTracer
    , keepAliveClientTracer         = nullTracer
    }

showTracers :: ( Show blk
               , Show (GenTx blk)
               , Show (GenTxId blk)
               , Show (ApplyTxErr blk)
               , Show (Header blk)
               , Show (ForgeStateInfo blk)
               , Show (ForgeStateUpdateError blk)
               , Show (CannotForge blk)
               , Show remotePeer
               , LedgerSupportsProtocol blk
               )
            => Tracer m String -> Tracers m remotePeer localPeer blk
showTracers tr = Tracers
    { chainSyncClientTracer         = showTracing tr
    , chainSyncServerHeaderTracer   = showTracing tr
    , chainSyncServerBlockTracer    = showTracing tr
    , blockFetchDecisionTracer      = showTracing tr
    , blockFetchClientTracer        = showTracing tr
    , blockFetchServerTracer        = showTracing tr
    , txInboundTracer               = showTracing tr
    , txOutboundTracer              = showTracing tr
    , localTxSubmissionServerTracer = showTracing tr
    , mempoolTracer                 = showTracing tr
    , forgeTracer                   = showTracing tr
    , blockchainTimeTracer          = showTracing tr
    , forgeStateInfoTracer          = showTracing tr
    , keepAliveClientTracer         = showTracing tr
    }

{-------------------------------------------------------------------------------
  Specific tracers
-------------------------------------------------------------------------------}

-- | Trace the forging of a block as a slot leader.
--
-- The flow of trace events here can be visualized as follows:
--
-- > TraceStartLeadershipCheck
-- >          |
-- >          +--- TraceSlotIsImmutable (leadership check failed)
-- >          |
-- >          +--- TraceBlockFromFuture (leadership check failed)
-- >          |
-- >  TraceBlockContext
-- >          |
-- >          +--- TraceNoLedgerState (leadership check failed)
-- >          |
-- >   TraceLedgerState
-- >          |
-- >          +--- TraceNoLedgerView (leadership check failed)
-- >          |
-- >   TraceLedgerView
-- >          |
-- >          +--- TraceForgeStateUpdateError (leadership check failed)
-- >          |
-- >          +--- TraceNodeCannotForge (leadership check failed)
-- >          |
-- >          +--- TraceNodeNotLeader
-- >          |
-- >   TraceNodeIsLeader
-- >          |
-- >    TraceForgedBlock
-- >          |
-- >          +--- TraceDidntAdoptBlock
-- >          |
-- >          +--- TraceForgedInvalidBlock
-- >          |
-- >  TraceAdoptedBlock
data TraceForgeEvent blk
    -- | Start of the leadership check
    --
    -- We record the current slot number.
  = TraceStartLeadershipCheck SlotNo

    -- | Leadership check failed: the tip of the ImmutableDB inhabits the
    -- current slot
    --
    -- This might happen in two cases.
    --
    --  1. the clock moved backwards, on restart we ignored everything from the
    --     VolatileDB since it's all in the future, and now the tip of the
    --     ImmutableDB points to a block produced in the same slot we're trying
    --     to produce a block in
    --
    --  2. k = 0 and we already adopted a block from another leader of the same
    --     slot.
    --
    -- We record both the current slot number as well as the tip of the
    -- ImmutableDB.
    --
    -- See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
  | TraceSlotIsImmutable SlotNo (Point blk) BlockNo

    -- | Leadership check failed: the current chain contains a block from a slot
    -- /after/ the current slot
    --
    -- This can only happen if the system is under heavy load.
    --
    -- We record both the current slot number as well as the slot number of the
    -- block at the tip of the chain.
    --
    -- See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
  | TraceBlockFromFuture SlotNo SlotNo

    -- | We found out to which block we are going to connect the block we are about
    -- to forge.
    --
    -- We record the current slot number, the block number of the block to
    -- connect to and its point.
    --
    -- Note that block number of the block we will try to forge is one more than
    -- the recorded block number.
  | TraceBlockContext SlotNo BlockNo (Point blk)

    -- | Leadership check failed: we were unable to get the ledger state for the
    -- point of the block we want to connect to
    --
    -- This can happen if after choosing which block to connect to the node
    -- switched to a different fork. We expect this to happen only rather
    -- rarely, so this certainly merits a warning; if it happens a lot, that
    -- merits an investigation.
    --
    -- We record both the current slot number as well as the point of the block
    -- we attempt to connect the new block to (that we requested the ledger
    -- state for).
  | TraceNoLedgerState SlotNo (Point blk)

    -- | We obtained a ledger state for the point of the block we want to
    -- connect to
    --
    -- We record both the current slot number as well as the point of the block
    -- we attempt to connect the new block to (that we requested the ledger
    -- state for).
  | TraceLedgerState SlotNo (Point blk)

    -- | Leadership check failed: we were unable to get the ledger view for the
    -- current slot number
    --
    -- This will only happen if there are many missing blocks between the tip of
    -- our chain and the current slot.
    --
    -- We record also the failure returned by 'forecastFor'.
  | TraceNoLedgerView SlotNo OutsideForecastRange

    -- | We obtained a ledger view for the current slot number
    --
    -- We record the current slot number.
  | TraceLedgerView SlotNo

    -- | Updating the forge state failed.
    --
    -- For example, the KES key could not be evolved anymore.
    --
    -- We record the error returned by 'updateForgeState'.
  | TraceForgeStateUpdateError SlotNo (ForgeStateUpdateError blk)

    -- | We did the leadership check and concluded that we should lead and forge
    -- a block, but cannot.
    --
    -- This should only happen rarely and should be logged with warning severity.
    --
    -- Records why we cannot forge a block.
  | TraceNodeCannotForge SlotNo (CannotForge blk)

    -- | We did the leadership check and concluded we are not the leader
    --
    -- We record the current slot number
  | TraceNodeNotLeader SlotNo

    -- | We did the leadership check and concluded we /are/ the leader
    --
    -- The node will soon forge; it is about to read its transactions from the
    -- Mempool. This will be followed by TraceForgedBlock.
  | TraceNodeIsLeader SlotNo

    -- | We forged a block
    --
    -- We record the current slot number, the point of the predecessor, the block
    -- itself, and the total size of the mempool snapshot at the time we produced
    -- the block (which may be significantly larger than the block, due to
    -- maximum block size)
    --
    -- This will be followed by one of three messages:
    --
    -- * TraceAdoptedBlock (normally)
    -- * TraceDidntAdoptBlock (rarely)
    -- * TraceForgedInvalidBlock (hopefully never -- this would indicate a bug)
  | TraceForgedBlock SlotNo (Point blk) blk MempoolSize

    -- | We did not adopt the block we produced, but the block was valid. We
    -- must have adopted a block that another leader of the same slot produced
    -- before we got the chance of adopting our own block. This is very rare,
    -- this warrants a warning.
  | TraceDidntAdoptBlock SlotNo blk

    -- | We forged a block that is invalid according to the ledger in the
    -- ChainDB. This means there is an inconsistency between the mempool
    -- validation and the ledger validation. This is a serious error!
  | TraceForgedInvalidBlock SlotNo blk (InvalidBlockReason blk)

    -- | We adopted the block we produced, we also trace the transactions
    -- that were adopted.
  | TraceAdoptedBlock SlotNo blk [GenTx blk]

deriving instance ( LedgerSupportsProtocol blk
                  , Eq blk
                  , Eq (GenTx blk)
                  , Eq (ForgeStateUpdateError blk)
                  , Eq (CannotForge blk)
                  ) => Eq (TraceForgeEvent blk)
deriving instance ( LedgerSupportsProtocol blk
                  , Show blk
                  , Show (GenTx blk)
                  , Show (ForgeStateUpdateError blk)
                  , Show (CannotForge blk)
                  ) => Show (TraceForgeEvent blk)

-- | Label a forge-related trace event with the label associated with its
-- credentials.
--
-- This is useful when a node is running with multiple sets of credentials.
data TraceLabelCreds a = TraceLabelCreds Text a
  deriving (Eq, Show, Functor)
