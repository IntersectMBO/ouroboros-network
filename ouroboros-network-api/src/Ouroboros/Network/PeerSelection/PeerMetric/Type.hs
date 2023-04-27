{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.PeerSelection.PeerMetric.Type
  ( HeaderMetricsTracer
  , FetchedMetricsTracer
  , ReportPeerMetrics (..)
  , TraceLabelPeer (..)
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer (Tracer)

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.SizeInBytes (SizeInBytes)

import           Network.Mux.Trace (TraceLabelPeer (..))


-- | Report arrival time of a header for a given slot.
--
type HeaderMetricsTracer  m = Tracer (STM m) (SlotNo, Time)

-- | Report block arrival of given size for some slot at a given time.
--
type FetchedMetricsTracer m = Tracer (STM m) (SizeInBytes, SlotNo, Time)


data ReportPeerMetrics m peerAddr = ReportPeerMetrics {
    reportHeader :: Tracer (STM m) (TraceLabelPeer peerAddr ( SlotNo
                                                            , Time
                                                            ))
  , reportFetch  :: Tracer (STM m) (TraceLabelPeer peerAddr ( SizeInBytes
                                                            , SlotNo
                                                            , Time
                                                            ))
  }
