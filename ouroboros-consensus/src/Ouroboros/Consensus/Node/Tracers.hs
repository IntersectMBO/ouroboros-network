{-# LANGUAGE FlexibleContexts #-}
module Ouroboros.Consensus.Node.Tracers
  ( -- * All tracers of a node bundled together
    Tracers' (..)
  , Tracers
  , nullTracers
  , showTracers
    -- * Specific tracers
  , TraceForgeEvent (..)
  ) where

import           Control.Monad.Class.MonadTime (Time)
import           Control.Tracer (Tracer, nullTracer, showTracing)

import           Ouroboros.Network.Block (Point, SlotNo)
import           Ouroboros.Network.BlockFetch (FetchDecision,
                     TraceFetchClientState, TraceLabelPeer)
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.BlockchainTime (TraceBlockchainTimeEvent)
import           Ouroboros.Consensus.BlockFetchServer
                     (TraceBlockFetchServerEvent)
import           Ouroboros.Consensus.ChainSyncClient (InvalidBlockReason,
                     TraceChainSyncClientEvent)
import           Ouroboros.Consensus.ChainSyncServer (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.Ledger.Abstract (AnachronyFailure,
                     ProtocolLedgerView)
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId,
                     TraceEventMempool)
import           Ouroboros.Consensus.TxSubmission
                     (TraceLocalTxSubmissionServerEvent (..))

{-------------------------------------------------------------------------------
  All tracers of a node bundled together
-------------------------------------------------------------------------------}

data Tracers' peer blk f = Tracers
  { chainSyncClientTracer         :: f (TraceChainSyncClientEvent blk)
  , chainSyncServerHeaderTracer   :: f (TraceChainSyncServerEvent blk (Header blk))
  , chainSyncServerBlockTracer    :: f (TraceChainSyncServerEvent blk blk)
  , blockFetchDecisionTracer      :: f [TraceLabelPeer peer (FetchDecision [Point (Header blk)])]
  , blockFetchClientTracer        :: f (TraceLabelPeer peer (TraceFetchClientState (Header blk)))
  , blockFetchServerTracer        :: f (TraceBlockFetchServerEvent blk)
  , txInboundTracer               :: f (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk))
  , txOutboundTracer              :: f (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
  , localTxSubmissionServerTracer :: f (TraceLocalTxSubmissionServerEvent blk)
  , mempoolTracer                 :: f (TraceEventMempool blk)
  , forgeTracer                   :: f (TraceForgeEvent blk (GenTx blk))
  , blockchainTimeTracer          :: f  TraceBlockchainTimeEvent
  }

-- | A record of 'Tracer's for the node.
type Tracers m peer blk = Tracers' peer blk (Tracer m)

-- | Use a 'nullTracer' for each of the 'Tracer's in 'Tracers'
nullTracers :: Monad m => Tracers m peer blk
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
  }

showTracers :: ( Show blk
               , Show (GenTx blk)
               , Show (GenTxId blk)
               , Show (ApplyTxErr blk)
               , Show (Header blk)
               , Show peer
               , ProtocolLedgerView blk
               )
            => Tracer m String -> Tracers m peer blk
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
  }

{-------------------------------------------------------------------------------
  Specific tracers
-------------------------------------------------------------------------------}

-- | Trace the forging of a block as a slot leader.
data TraceForgeEvent blk tx
  -- | The node will soon forge; it is about to read its transactions and
  -- current DB.
  = TraceForgeAboutToLead SlotNo

  -- | The forged block and at which slot it was forged.
  | TraceForgeEvent SlotNo blk

  -- | We should have produced a block, but didn't, due to too many missing
  -- blocks between the tip of our chain and the current slot
  --
  -- As a sanity check, we record also the failure returned by
  -- 'anachronisticProtocolLedgerView', although we expect this to be
  -- 'TooFarAhead', never 'TooFarBehind'.
  | TraceCouldNotForge SlotNo AnachronyFailure

  -- | We adopted the block we produced, we also trace the transactions
  -- and the time the block with the transactions was adopted.
  | TraceAdoptedBlock SlotNo blk [tx] Time

  -- | We did not adopt the block we produced, but the block was valid. We
  -- must have adopted a block that another leader of the same slot produced
  -- before we got the chance of adopting our own block. This is very rare,
  -- this warrants a warning.
  | TraceDidntAdoptBlock SlotNo blk

  -- | We forged a block that is invalid according to the ledger in the
  -- ChainDB. This means there is an inconsistency between the mempool
  -- validation and the ledger validation. This is a serious error!
  | TraceForgedInvalidBlock SlotNo blk (InvalidBlockReason blk)
  deriving (Eq, Show)
