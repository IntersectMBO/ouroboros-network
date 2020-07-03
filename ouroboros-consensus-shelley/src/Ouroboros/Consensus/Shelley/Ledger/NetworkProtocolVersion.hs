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

data ShelleyNodeToClientVersion = ShelleyNodeToClientVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion (ShelleyBlock c) where
  type BlockNodeToNodeVersion   (ShelleyBlock c) = ShelleyNodeToNodeVersion
  type BlockNodeToClientVersion (ShelleyBlock c) = ShelleyNodeToClientVersion

instance SupportedNetworkProtocolVersion (ShelleyBlock c) where
  supportedNodeToNodeVersions   _ = Map.fromList [
        (NodeToNodeV_1, ShelleyNodeToNodeVersion1)
        -- V_2 enables block size hints for Byron headers and the hard fork
        -- combinator, unused by Shelley-only
      ]
  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_1, ShelleyNodeToClientVersion1)
        -- Enable the LocalStateQuery protocol, no serialisation changes
      , (NodeToClientV_2, ShelleyNodeToClientVersion1)
        -- V_3 enables the hard fork, unused by Shelley-only
      ]
