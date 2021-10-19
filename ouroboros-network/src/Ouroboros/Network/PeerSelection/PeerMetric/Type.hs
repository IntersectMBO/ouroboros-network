module Ouroboros.Network.PeerSelection.PeerMetric.Type where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime

import           Cardano.Slotting.Slot (SlotNo (..))
import           Ouroboros.Network.DeltaQ ( SizeInBytes )

-- | Report arrival time of a header for a given slot.
--
type ReportHeaderMetricsSTM m = (SlotNo -> Time -> STM m ())

-- | Report block arrival of given size for some slot at a given time.
--
type ReportFetchedMetricsSTM m = (SizeInBytes -> SlotNo -> Time -> STM m ())

data ReportPeerMetrics m p = ReportPeerMetrics {
    reportHeader :: p -> ReportHeaderMetricsSTM m
  , reportFetch  :: p -> ReportFetchedMetricsSTM m
  }
