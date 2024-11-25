{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Network.PublicRootPeers where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.Types

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

-- Cardano Public Root Peers Actions
cardanoPublicRootPeersActions :: Ord peeraddr => PublicExtraPeersActions (CardanoPublicRootPeers peeraddr) peeraddr
cardanoPublicRootPeersActions =
  PublicExtraPeersActions {
    nullExtraPeers         = nullAll
  , invariantExtraPeers    = invariant
  , memberExtraPeers       = member
  , extraPeersToSet        = toSet
  , sizeExtraPeers         = size
  , differenceExtraPeers   = difference
  , intersectionExtraPeers = intersection
  , toAdvertise            = toAdvertisePeersMap
  }

-- Map and Set are disjoint
--
invariant :: Ord peeraddr => CardanoPublicRootPeers peeraddr -> Bool
invariant (CardanoPublicRootPeers a b) = all (`Map.notMember` a) b

fromMapAndSet :: Ord peeraddr
              => Map peeraddr PeerAdvertise
              -> Set peeraddr
              -> CardanoPublicRootPeers peeraddr
fromMapAndSet pp bsp =
  let newPP = pp `Map.withoutKeys` bsp
   in CardanoPublicRootPeers newPP bsp

empty :: CardanoPublicRootPeers peeraddr
empty = CardanoPublicRootPeers Map.empty Set.empty

nullPublicConfig :: CardanoPublicRootPeers peeraddr -> Bool
nullPublicConfig CardanoPublicRootPeers { cprpGetPublicConfigPeers } =
  Map.null cprpGetPublicConfigPeers

nullBootstrap :: CardanoPublicRootPeers peeraddr -> Bool
nullBootstrap CardanoPublicRootPeers { cprpGetBootstrapPeers } =
  Set.null cprpGetBootstrapPeers

nullAll :: CardanoPublicRootPeers peeraddr -> Bool
nullAll cprp = nullPublicConfig cprp && nullBootstrap cprp

member :: Ord peeraddr => peeraddr -> CardanoPublicRootPeers peeraddr -> Bool
member addr (CardanoPublicRootPeers a b) =
  Map.member addr a || Set.member addr b

toSet :: Ord peeraddr => CardanoPublicRootPeers peeraddr -> Set peeraddr
toSet (CardanoPublicRootPeers a b) = Map.keysSet a <> b

size :: CardanoPublicRootPeers peeraddr -> Int
size (CardanoPublicRootPeers a b) = Map.size a + Set.size b

difference :: Ord peeraddr
           => CardanoPublicRootPeers peeraddr
           -> Set peeraddr
           -> CardanoPublicRootPeers peeraddr
difference (CardanoPublicRootPeers a b) addrs =
  CardanoPublicRootPeers (Map.withoutKeys a addrs)
                         (Set.difference b addrs)


intersection :: Ord peeraddr
             => CardanoPublicRootPeers peeraddr
             -> Set peeraddr
             -> CardanoPublicRootPeers peeraddr
intersection (CardanoPublicRootPeers a b) addrs =
  CardanoPublicRootPeers (Map.restrictKeys a addrs)
                         (Set.intersection b addrs)

toAdvertisePeersMap :: Ord peeraddr
                    => CardanoPublicRootPeers peeraddr
                    -> Map peeraddr PeerAdvertise
toAdvertisePeersMap (CardanoPublicRootPeers a b) =
  a <> Set.foldl (\m p -> Map.insert p DoNotAdvertisePeer m) Map.empty b
