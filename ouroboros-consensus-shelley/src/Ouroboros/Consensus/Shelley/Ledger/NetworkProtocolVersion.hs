{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion (
    ShelleyNodeToNodeVersion(..)
  , ShelleyNodeToClientVersion(..)
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
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion (ShelleyBlock era) where
  type BlockNodeToNodeVersion   (ShelleyBlock era) = ShelleyNodeToNodeVersion
  type BlockNodeToClientVersion (ShelleyBlock era) = ShelleyNodeToClientVersion

-- TODO #2668 make this era-specific
instance SupportedNetworkProtocolVersion (ShelleyBlock era) where
  supportedNodeToNodeVersions   _ = Map.fromList [
        (NodeToNodeV_1, ShelleyNodeToNodeVersion1)
        -- V_2 enables block size hints for Byron headers and the hard fork
        -- combinator, unused by Shelley-only
      ]
  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_1, ShelleyNodeToClientVersion1)
        -- Enable the LocalStateQuery protocol, no serialisation changes
      , (NodeToClientV_2, ShelleyNodeToClientVersion1)
        -- V_3 enables the hard fork, which didn't affect Shelley-only when
        -- introduced. However, we have retroactively claimed V_3 to enable
        -- 'ShelleyNodeToClientVersion2'.
      , (NodeToClientV_3, ShelleyNodeToClientVersion2)
      ]
