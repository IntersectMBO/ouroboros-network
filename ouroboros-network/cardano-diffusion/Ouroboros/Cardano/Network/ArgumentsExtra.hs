{-# LANGUAGE NamedFieldPuns #-}
module Ouroboros.Cardano.Network.ArgumentsExtra where

import Cardano.Network.ConsensusMode (ConsensusMode)
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Cardano.Network.Types (NumberOfBigLedgerPeers)
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionActions
           (ExtraPeerSelectionActions (ExtraPeerSelectionActions))
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionActions qualified as Cardano
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))

-- | Cardano Node specific (extra) arguments to be passed to diffusion.
--
data ExtraArguments m =
  ExtraArguments {
    -- | Genesis selection targets for the peer governor
    genesisPeerTargets     :: PeerSelectionTargets

  , readUseBootstrapPeers  :: STM m UseBootstrapPeers

    -- | For Genesis, this sets the floor for minimum number of
    --   active big ledger peers we must be connected to in order
    --   to be able to signal trusted state (OutboundConnectionsState)
  , numberOfBigLedgerPeers :: NumberOfBigLedgerPeers

    -- | When syncing up, ie. ledgerStateJudgement == TooOld,
    -- when this is True we will maintain connection with many big ledger peers
    -- to get a strong guarantee that when syncing up we will finish with a true
    -- ledger state. When false, we will fall back on the previous algorithms
    -- that leverage UseBootstrapPeers flag
  , consensusMode          :: ConsensusMode
  }

cardanoExtraArgsToPeerSelectionActions :: ExtraArguments m
                                       -> ExtraPeerSelectionActions m
cardanoExtraArgsToPeerSelectionActions ExtraArguments {
                                         genesisPeerTargets,
                                         readUseBootstrapPeers
                                       } =
  ExtraPeerSelectionActions {
    Cardano.genesisPeerTargets
  , Cardano.readUseBootstrapPeers
  }

