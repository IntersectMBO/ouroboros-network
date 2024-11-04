{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.PublicRootPeers where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)

data CardanoPublicRootPeers peeraddr =
  CardanoPublicRootPeers
    { cprpGetPublicConfigPeers :: !(Map peeraddr PeerAdvertise)
    , cprpGetBootstrapPeers    :: !(Set peeraddr)
    }
  deriving (Eq, Show, Typeable)

instance Ord peeraddr => Semigroup (CardanoPublicRootPeers peeraddr) where
  (CardanoPublicRootPeers a b) <> (CardanoPublicRootPeers a' b') =
    let -- Combine the sets, prioritizing bootstrapPeers
        combinedSet = b `Set.union` b'
        -- Combine the maps and remove any peers that are now in the set
        combinedMap = (a `Map.union` a') `Map.withoutKeys` combinedSet
    in CardanoPublicRootPeers combinedMap combinedSet

instance Ord peeraddr => Monoid (CardanoPublicRootPeers peeraddr) where
  mempty = empty

-- Map and Set are disjoint
--
invariant :: Ord peeraddr => CardanoPublicRootPeers peeraddr -> Bool
invariant (CardanoPublicRootPeers a b) = all (`Map.notMember` a) b

empty :: CardanoPublicRootPeers peeraddr
empty = CardanoPublicRootPeers Map.empty Set.empty

nullPublicConfig :: CardanoPublicRootPeers peeraddr -> Bool
nullPublicConfig CardanoPublicRootPeers { cprpGetPublicConfigPeers } =
  Map.null cprpGetPublicConfigPeers

nullBootstrap :: CardanoPublicRootPeers peeraddr -> Bool
nullBootstrap CardanoPublicRootPeers { cprpGetBootstrapPeers } =
  Set.null cprpGetBootstrapPeers

