module Cardano.Network.ArgumentsExtra where

import Cardano.Network.ConsensusMode (ConsensusMode)
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Cardano.Network.Types (MinBigLedgerPeersForTrustedState)
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))

-- | Cardano Node specific (extra) arguments to be passed to diffusion.
--
data CardanoArgumentsExtra m =
  CardanoArgumentsExtra {
    -- | selection targets for the peer governor
    caeSyncPeerTargets                  :: PeerSelectionTargets
  , caeReadUseBootstrapPeers            :: STM m UseBootstrapPeers

  -- | For Genesis, this sets the floor for minimum number of
  --   active big ledger peers we must be connected to in order
  --   to be able to signal trusted state (OutboundConnectionsState)
  , caeMinBigLedgerPeersForTrustedState :: MinBigLedgerPeersForTrustedState
    -- | When syncing up, ie. ledgerStateJudgement == TooOld,
    -- when this is True we will maintain connection with many big ledger peers
    -- to get a strong guarantee that when syncing up we will finish with a true
    -- ledger state. When false, we will fall back on the previous algorithms
    -- that leverage UseBootstrapPeers flag
  , caeConsensusMode                    :: ConsensusMode
  }
