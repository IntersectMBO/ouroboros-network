{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.PeerSelection.PeerMetric.Type where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer)

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.DeltaQ ( SizeInBytes )

type HeaderMetricsTracer  m = Tracer (STM m) (SlotNo, Time)
type FetchedMetricsTracer m = Tracer (STM m) (SizeInBytes, SlotNo, Time)

data WithPeer peerAddr a
  = WithPeer { wpPeerAddr :: peerAddr
             , wpData     :: a
             }

mapPeerAddr :: (peerAddr -> peerAddr')
            -> WithPeer peerAddr  a
            -> WithPeer peerAddr' a
mapPeerAddr f (WithPeer peerAddr a) = WithPeer (f peerAddr) a


data ReportPeerMetrics m peerAddr = ReportPeerMetrics {
    reportHeader :: Tracer (STM m) (WithPeer peerAddr ( SlotNo
                                                      , Time
                                                      ))
  , reportFetch  :: Tracer (STM m) (WithPeer peerAddr ( SizeInBytes
                                                      , SlotNo
                                                      , Time
                                                      ))
  }
