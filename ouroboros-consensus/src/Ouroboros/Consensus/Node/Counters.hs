{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Node.Counters
  ( -- * All Counters of a node bundled together
    Counters' (..)
  , Counters
  , nullCounters
  , showCounters
  ) where

import           Control.Tracer (Tracer, nullTracer, showTracing)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId)
import           Ouroboros.Network.BlockFetch (TraceLabelPeer)
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)

{-------------------------------------------------------------------------------
  All Counters of a node bundled together
-------------------------------------------------------------------------------}

newtype Counters' remotePeer localPeer blk f = Counters
  { txInboundCounter :: f (TraceLabelPeer remotePeer (TraceTxSubmissionInbound  (GenTxId blk) (GenTx blk)))
  }

instance (forall a. Semigroup (f a))
      => Semigroup (Counters' remotePeer localPeer blk f) where
  l <> r = Counters
      { txInboundCounter = f txInboundCounter
      }
    where
      f :: forall a. Semigroup a
        => (Counters' remotePeer localPeer blk f -> a) -> a
      f prj = prj l <> prj r

-- | A record of 'Tracer's for the node.
type Counters m remotePeer localPeer blk =
     Counters'  remotePeer localPeer blk (Tracer m)

-- | Use a 'nullTracer' for each of the 'Tracer's in 'Counters'
nullCounters :: Monad m => Counters m remotePeer localPeer blk
nullCounters = Counters
    { txInboundCounter = nullTracer
    }

showCounters :: Show remotePeer => Tracer m String -> Counters m remotePeer localPeer blk
showCounters tr = Counters
    { txInboundCounter = showTracing tr
    }
