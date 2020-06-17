{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.NetworkProtocolVersion (
    ByronNodeToNodeVersion(..)
  , ByronNodeToClientVersion(..)
  ) where

import           Data.List.NonEmpty (NonEmpty (..))

import qualified Ouroboros.Network.NodeToClient as N
import qualified Ouroboros.Network.NodeToNode as N

import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.Byron.Ledger.Block

data ByronNodeToNodeVersion =
    -- | We send headers without a size hint
    ByronNodeToNodeVersion1

    -- | We send headers /with/ a size hint
  | ByronNodeToNodeVersion2
  deriving (Show, Eq, Ord, Enum, Bounded)

data ByronNodeToClientVersion =
    -- | No local state query protocol
    ByronNodeToClientVersion1

    -- | Use local state query protocol
  | ByronNodeToClientVersion2
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion ByronBlock where
  type BlockNodeToNodeVersion   ByronBlock = ByronNodeToNodeVersion
  type BlockNodeToClientVersion ByronBlock = ByronNodeToClientVersion

instance TranslateNetworkProtocolVersion ByronBlock where
  supportedNodeToNodeVersions   _ = ByronNodeToNodeVersion1
                                  :| [ ByronNodeToNodeVersion2 ]
  supportedNodeToClientVersions _ = ByronNodeToClientVersion1
                                  :| [ ByronNodeToClientVersion2 ]

  mostRecentSupportedNodeToNode   _ = ByronNodeToNodeVersion2
  mostRecentSupportedNodeToClient _ = ByronNodeToClientVersion2

  nodeToNodeProtocolVersion _ ByronNodeToNodeVersion1 = N.NodeToNodeV_1
  nodeToNodeProtocolVersion _ ByronNodeToNodeVersion2 = N.NodeToNodeV_2

  nodeToClientProtocolVersion _ ByronNodeToClientVersion1 = N.NodeToClientV_1
  nodeToClientProtocolVersion _ ByronNodeToClientVersion2 = N.NodeToClientV_2
