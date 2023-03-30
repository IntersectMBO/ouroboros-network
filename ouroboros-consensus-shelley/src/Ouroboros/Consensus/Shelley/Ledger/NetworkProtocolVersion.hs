{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion (
    ShelleyNodeToClientVersion (..)
  , ShelleyNodeToNodeVersion (..)
  ) where

import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Shelley.Ledger.Block

data ShelleyNodeToNodeVersion = ShelleyNodeToNodeVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

data ShelleyNodeToClientVersion =
    ShelleyNodeToClientVersion1

    -- | New queries introduced
  | ShelleyNodeToClientVersion2

    -- | New query introduced
  | ShelleyNodeToClientVersion3

    -- | New queries introduced
  | ShelleyNodeToClientVersion4

    -- | New queries introduced: GetRewardInfoPools
  | ShelleyNodeToClientVersion5

    -- | New queries introduced: GetPoolDistr, GetPoolState, GetStakeSnapshots
  | ShelleyNodeToClientVersion6

    -- | New queries introduced: GetStakeDelegDeposits
  | ShelleyNodeToClientVersion7
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion (ShelleyBlock proto era) where
  type BlockNodeToNodeVersion   (ShelleyBlock proto era) = ShelleyNodeToNodeVersion
  type BlockNodeToClientVersion (ShelleyBlock proto era) = ShelleyNodeToClientVersion

-- TODO #2668 make this era-specific
instance SupportedNetworkProtocolVersion (ShelleyBlock proto era) where
  supportedNodeToNodeVersions   _ = Map.fromList [
        (NodeToNodeV_7, ShelleyNodeToNodeVersion1)
      , (NodeToNodeV_8, ShelleyNodeToNodeVersion1)
      ]
  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_9,  ShelleyNodeToClientVersion5)
      , (NodeToClientV_10, ShelleyNodeToClientVersion5)
      , (NodeToClientV_11, ShelleyNodeToClientVersion5)
      , (NodeToClientV_12, ShelleyNodeToClientVersion5)
      , (NodeToClientV_13, ShelleyNodeToClientVersion5)
      , (NodeToClientV_14, ShelleyNodeToClientVersion6)
      , (NodeToClientV_15, ShelleyNodeToClientVersion7)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
