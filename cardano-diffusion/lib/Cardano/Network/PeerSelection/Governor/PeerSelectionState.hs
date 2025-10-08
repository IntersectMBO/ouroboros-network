{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Network.PeerSelection.Governor.PeerSelectionState where

import Control.Monad.Class.MonadTime.SI (Time (..))
import Data.Aeson (FromJSON)

import Cardano.Network.ConsensusMode (ConsensusMode)
import Cardano.Network.LedgerStateJudgement
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))


-- | Minimum number of hot big ledger peers in Genesis mode
--   for trusted state to be signalled to Consensus. This number
--   should be smaller than the `targetNumberOfActiveBigLedgerPeers`
--   but greater than 1. In Genesis, we may demote a big ledger peer
--   for underperformance, but not promote a replacement immediately
--   to guard against adversaries which may want to slow down our
--   progress.
--
newtype NumberOfBigLedgerPeers =
  NumberOfBigLedgerPeers { getNumberOfBigLedgerPeers :: Int }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON)



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

