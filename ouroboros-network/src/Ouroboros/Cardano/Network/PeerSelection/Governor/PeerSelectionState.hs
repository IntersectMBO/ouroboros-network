module Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState where

import Cardano.Network.ConsensusMode (ConsensusMode)
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.Types (LedgerStateJudgement (..),
           NumberOfBigLedgerPeers (..))
import Control.Monad.Class.MonadTime.SI (Time (..))

-- | Cardano Node PeerSelection State extension data type.
-- It contain specific PeerSelection state parameters to guide the Outbound
-- Governor.
--
data ExtraState =
  ExtraState {
    -- | Current ledger state judgement
    ledgerStateJudgement      :: !LedgerStateJudgement

    -- | Flag whether to sync in genesis mode when ledgerStateJudgement == TooOld
    -- this comes from node configuration and should be treated as read-only
    --
  , consensusMode             :: !ConsensusMode

    -- | Current value of 'UseBootstrapPeers'.
    --
  , bootstrapPeersFlag        :: !UseBootstrapPeers

    -- | Has the governor fully reset its state
    --
  , hasOnlyBootstrapPeers     :: !Bool

    -- | Has the governor fully reset its state
    -- TODO: Use strict Maybe type from cardano-base
  , bootstrapPeersTimeout     :: !(Maybe Time)

    -- | Use in Genesis mode to check whether we can signal to
    --   consensus that we met criteria of trusted state to enter
    --   deadline mode. This parameter comes from node configuration,
    --   with a default value in the `Configuration` module.
    --
  , minNumberOfBigLedgerPeers :: NumberOfBigLedgerPeers
  }
  deriving (Eq, Show)

empty :: ConsensusMode -> NumberOfBigLedgerPeers -> ExtraState
empty cm minActiveBigLedgerPeers =
    ExtraState {
      ledgerStateJudgement      = TooOld,
      consensusMode             = cm,
      bootstrapPeersFlag        = DontUseBootstrapPeers,
      hasOnlyBootstrapPeers     = False,
      bootstrapPeersTimeout     = Nothing,
      minNumberOfBigLedgerPeers = minActiveBigLedgerPeers
    }

data DebugPeerSelectionState =
  DebugPeerSelectionState {
    debugLedgerStateJudgement :: !LedgerStateJudgement
  }
  deriving Show

