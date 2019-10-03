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

import           Control.Tracer (Tracer, nullTracer, showTracing)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (Point, SlotNo, Tip)
import           Ouroboros.Network.BlockFetch (FetchDecision,
                     TraceFetchClientState, TraceLabelPeer)
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)

import           Ouroboros.Consensus.Block (Header, SupportedBlock)
import           Ouroboros.Consensus.BlockFetchServer
                     (TraceBlockFetchServerEvent)
import           Ouroboros.Consensus.ChainSyncClient (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.ChainSyncServer (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTx, GenTxId,
                     TraceEventMempool)
import           Ouroboros.Consensus.TxSubmission
                     (TraceLocalTxSubmissionServerEvent (..))

{-------------------------------------------------------------------------------
  All tracers of a node bundled together
-------------------------------------------------------------------------------}

data Tracers' peer blk tip f = Tracers
  { chainSyncClientTracer         :: f (TraceChainSyncClientEvent blk tip)
  , chainSyncServerHeaderTracer   :: f (TraceChainSyncServerEvent blk (Header blk))
  , chainSyncServerBlockTracer    :: f (TraceChainSyncServerEvent blk blk)
  , blockFetchDecisionTracer      :: f [TraceLabelPeer peer (AnchoredFragment (Header blk), FetchDecision [Point (Header blk)])]
  , blockFetchClientTracer        :: f (TraceLabelPeer peer (TraceFetchClientState (Header blk)))
  , blockFetchServerTracer        :: f (TraceBlockFetchServerEvent blk)
  , txInboundTracer               :: f (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk))
  , txOutboundTracer              :: f (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
  , localTxSubmissionServerTracer :: f (TraceLocalTxSubmissionServerEvent blk)
  , mempoolTracer                 :: f (TraceEventMempool blk)
  , forgeTracer                   :: f (TraceForgeEvent blk)
  }

-- | A record of 'Tracer's for the node.
type Tracers m peer blk = Tracers' peer blk (Tip blk) (Tracer m)

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
  }

showTracers :: ( Show blk
               , Show (GenTx blk)
               , Show (ApplyTxErr blk)
               , Show (Header blk)
               , Show peer
               , SupportedBlock blk
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
  }

{-------------------------------------------------------------------------------
  Specific tracers
-------------------------------------------------------------------------------}

-- | Trace the forging of a block as a slot leader.
data TraceForgeEvent blk
  -- | The forged block and at which slot it was forged.
  = TraceForgeEvent SlotNo blk
  deriving (Eq, Show)
