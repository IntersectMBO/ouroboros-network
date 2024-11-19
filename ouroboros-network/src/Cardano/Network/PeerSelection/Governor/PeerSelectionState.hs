module Cardano.Network.PeerSelection.Governor.PeerSelectionState where

import Cardano.Network.ConsensusMode (ConsensusMode)
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.Types (LedgerStateJudgement (..),
           MinBigLedgerPeersForTrustedState (..))
import Control.Monad.Class.MonadTime.SI (Time (..))

-- | Cardano Node PeerSelection State extension data type.
-- It contain specific PeerSelection state parameters to guide the Outbound
-- Governor.
--
data CardanoPeerSelectionState =
  CardanoPeerSelectionState {
    -- | Current ledger state judgement
    cpstLedgerStateJudgement             :: !LedgerStateJudgement
    -- | Flag whether to sync in genesis mode when ledgerStateJudgement == TooOld
    -- this comes from node configuration and should be treated as read-only
    --
  , cpstConsensusMode                    :: !ConsensusMode
    -- | Current value of 'UseBootstrapPeers'.
    --
  , cpstBootstrapPeersFlag               :: !UseBootstrapPeers
    -- | Has the governor fully reset its state
    --
  , cpstHasOnlyBootstrapPeers            :: !Bool
  , cpstBlockedAt                        :: !Time
    -- | Has the governor fully reset its state
    -- TODO: Use strict Maybe type from cardano-base
  , cpstBootstrapPeersTimeout            :: !(Maybe Time)
    -- | Use in Genesis mode to check whether we can signal to
    --   consensus that we met criteria of trusted state to enter
    --   deadline mode. This parameter comes from node configuration,
    --   with a default value in the `Configuration` module.
    --
  , cpstMinBigLedgerPeersForTrustedState :: MinBigLedgerPeersForTrustedState
  }
  deriving Show

empty :: ConsensusMode -> MinBigLedgerPeersForTrustedState -> CardanoPeerSelectionState
empty consensusMode minActiveBigLedgerPeers =
    CardanoPeerSelectionState {
      cpstLedgerStateJudgement             = TooOld,
      cpstConsensusMode                    = consensusMode,
      cpstBootstrapPeersFlag               = DontUseBootstrapPeers,
      cpstHasOnlyBootstrapPeers            = False,
      cpstBlockedAt                        = Time 0,
      cpstBootstrapPeersTimeout            = Nothing,
      cpstMinBigLedgerPeersForTrustedState = minActiveBigLedgerPeers
    }

data CardanoDebugPeerSelectionState =
  CardanoDebugPeerSelectionState {
    cdpssLedgerStateJudgement :: !LedgerStateJudgement
  }
  deriving Show

