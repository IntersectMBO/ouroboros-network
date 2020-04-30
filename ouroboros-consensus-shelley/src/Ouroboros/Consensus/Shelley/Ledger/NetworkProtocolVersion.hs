{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion (
    ShelleyNodeToNodeVersion(..)
  , ShelleyNodeToClientVersion(..)
  ) where

import           Data.List.NonEmpty (NonEmpty (..))

import qualified Ouroboros.Network.NodeToClient as N
import qualified Ouroboros.Network.NodeToNode as N

import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.Shelley.Ledger.Block

data ShelleyNodeToNodeVersion = ShelleyNodeToNodeVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

data ShelleyNodeToClientVersion = ShelleyNodeToClientVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion (ShelleyBlock c) where
  type NodeToNodeVersion   (ShelleyBlock c) = ShelleyNodeToNodeVersion
  type NodeToClientVersion (ShelleyBlock c) = ShelleyNodeToClientVersion

  supportedNodeToNodeVersions   _ = ShelleyNodeToNodeVersion1
                                  :| []
  supportedNodeToClientVersions _ = ShelleyNodeToClientVersion1
                                  :| []

  mostRecentNodeToNodeVersion   _ = ShelleyNodeToNodeVersion1
  mostRecentNodeToClientVersion _ = ShelleyNodeToClientVersion1

  nodeToNodeProtocolVersion _ ShelleyNodeToNodeVersion1 = N.NodeToNodeV_1

  -- From the beginning, Shelley supports version 2 of the node-to-client
  -- protocol (i.e. the local state query protocol is enabled from the start).
  nodeToClientProtocolVersion _ ShelleyNodeToClientVersion1 = N.NodeToClientV_2
