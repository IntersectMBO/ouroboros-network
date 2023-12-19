{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Network.PeerSelection.Bootstrap
  ( UseBootstrapPeers (..)
  , isBootstrapPeersEnabled
  , isInSensitiveState
  , isNodeAbleToMakeProgress
  ) where

import           GHC.Generics (Generic)
import           Ouroboros.Network.PeerSelection.LedgerPeers.Type
                     (LedgerStateJudgement (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
                     (DomainAccessPoint)

data UseBootstrapPeers = DontUseBootstrapPeers
                       | UseBootstrapPeers [DomainAccessPoint]
  deriving (Eq, Show, Ord, Generic)

isBootstrapPeersEnabled :: UseBootstrapPeers -> Bool
isBootstrapPeersEnabled DontUseBootstrapPeers = False
isBootstrapPeersEnabled _                     = True

-- | Determines if the system is in a sensitive state based on bootstrap peer
-- usage and ledger state.
--
-- * When bootstrap peers are not in use, the system is considered to never be
-- in a sensitive state.
-- * When bootstrap peers are in use and the ledger is in 'YoungEnough' state,
-- the system is not in a sensitive state.
-- * When bootstrap peers are in use and the ledger is in 'TooOld' state,
-- the system is considered to be in a sensitive state.
isInSensitiveState :: UseBootstrapPeers -> LedgerStateJudgement -> Bool
isInSensitiveState ubp lsj =
  isBootstrapPeersEnabled ubp && lsj == TooOld

-- | A node is able to make progress either if it isn't in a sensitive state
-- _or_ if it is in a sensitive state and has reached a clean state from which
-- it now only uses trustable peers
--
isNodeAbleToMakeProgress :: UseBootstrapPeers -> LedgerStateJudgement -> Bool -> Bool
isNodeAbleToMakeProgress ubp lsj hasOnlyBootstrapPeers =
    not (isInSensitiveState ubp lsj) || hasOnlyBootstrapPeers
