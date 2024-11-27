{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Cardano.Network.ConsensusMode where

import Data.Aeson
import GHC.Generics (Generic)

-- | Diffusion layer configuration parameter.
--
-- The two modes determine which `PeerSelectionTargets` basis is used
-- by churn and OG. The node's configuration sets this, and diffusion
-- is initiated and remains only in this mode.
--
data ConsensusMode =
    GenesisMode
  -- ^ When `LedgerStateJudgement` is `TooOld`, the targets basis is changed
  -- from default to one specific for this mode, which uses more big ledger peers
  -- until syncing is complete.

  | PraosMode
  -- ^ The legacy mode which depends on official relays and/or bootstrap peers
  -- configuration. This mode uses only the default target basis irrespective
  -- ledger state.
  deriving (Eq, Show, Generic, FromJSON)


defaultConsensusMode :: ConsensusMode
defaultConsensusMode = PraosMode
