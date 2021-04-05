{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion (
    ByronNodeToClientVersion (..)
  , ByronNodeToNodeVersion (..)
  ) where

import qualified Data.Map.Strict as Map

import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.Byron.Ledger.Block

data ByronNodeToNodeVersion =
    -- | We send headers without a size hint
    ByronNodeToNodeVersion1

    -- | We send headers /with/ a size hint
  | ByronNodeToNodeVersion2
  deriving (Show, Eq, Ord, Enum, Bounded)

data ByronNodeToClientVersion =
    ByronNodeToClientVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion ByronBlock where
  type BlockNodeToNodeVersion   ByronBlock = ByronNodeToNodeVersion
  type BlockNodeToClientVersion ByronBlock = ByronNodeToClientVersion

instance SupportedNetworkProtocolVersion ByronBlock where
  supportedNodeToNodeVersions   _ = Map.fromList [
        (NodeToNodeV_1, ByronNodeToNodeVersion1)
        -- V_2 enables block size hints for Byron headers within the hard fork
        -- combinator, not supported by Byron-only.
      ]
  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_1, ByronNodeToClientVersion1)
        -- Enable the LocalStateQuery protocol, no serialisation changes
      , (NodeToClientV_2, ByronNodeToClientVersion1)
        -- V_3 enables the hard fork, not supported by Byron-only.
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
