{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion (
    ByronNodeToClientVersion (..)
  , ByronNodeToNodeVersion (..)
  ) where

import qualified Data.Map.Strict as Map
import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Node.NetworkProtocolVersion

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
        (NodeToNodeV_7, ByronNodeToNodeVersion1)
      , (NodeToNodeV_8, ByronNodeToNodeVersion1)
      ]
  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_9,  ByronNodeToClientVersion1)
      , (NodeToClientV_10, ByronNodeToClientVersion1)
      , (NodeToClientV_11, ByronNodeToClientVersion1)
      , (NodeToClientV_12, ByronNodeToClientVersion1)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
