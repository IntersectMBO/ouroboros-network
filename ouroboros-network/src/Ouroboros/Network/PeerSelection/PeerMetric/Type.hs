module Ouroboros.Network.PeerSelection.PeerMetric.Type where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.DeltaQ ( SizeInBytes )

type ReportHeaderMetricsSTM m = (SlotNo -> Time -> STM m ())
type ReportFetchedMetricsSTM m = (SizeInBytes -> SlotNo -> Time -> STM m ())

data ReportPeerMetrics m p = ReportPeerMetrics {
    reportHeader :: p -> ReportHeaderMetricsSTM m
  , reportFetch  :: p -> ReportFetchedMetricsSTM m
  }
