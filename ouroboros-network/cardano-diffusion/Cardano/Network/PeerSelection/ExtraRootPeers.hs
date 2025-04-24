{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Network.PeerSelection.ExtraRootPeers where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.Types

data ExtraPeers peeraddr =
  ExtraPeers
    { publicConfigPeers :: !(Map peeraddr PeerAdvertise)
    , bootstrapPeers    :: !(Set peeraddr)
    }
  deriving (Eq, Show)

instance Ord peeraddr => Semigroup (ExtraPeers peeraddr) where
  (ExtraPeers a b) <> (ExtraPeers a' b') =
    let -- Combine the sets, prioritizing bootstrapPeers
        combinedSet = b `Set.union` b'
        -- Combine the maps and remove any peers that are now in the set
        combinedMap = (a `Map.union` a') `Map.withoutKeys` combinedSet
    in ExtraPeers combinedMap combinedSet

instance Ord peeraddr => Monoid (ExtraPeers peeraddr) where
  mempty = empty

-- Cardano Public Root Peers Actions
cardanoPublicRootPeersAPI :: Ord peeraddr => PublicExtraPeersAPI (ExtraPeers peeraddr) peeraddr
cardanoPublicRootPeersAPI =
  PublicExtraPeersAPI {
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
invariant :: Ord peeraddr => ExtraPeers peeraddr -> Bool
invariant (ExtraPeers a b) = all (`Map.notMember` a) b

fromMapAndSet :: Ord peeraddr
              => Map peeraddr PeerAdvertise
              -> Set peeraddr
              -> ExtraPeers peeraddr
fromMapAndSet pp bsp =
  let newPP = pp `Map.withoutKeys` bsp
   in ExtraPeers newPP bsp

empty :: ExtraPeers peeraddr
empty = ExtraPeers Map.empty Set.empty

nullPublicConfig :: ExtraPeers peeraddr -> Bool
nullPublicConfig ExtraPeers { publicConfigPeers } =
  Map.null publicConfigPeers

nullBootstrap :: ExtraPeers peeraddr -> Bool
nullBootstrap ExtraPeers { bootstrapPeers } =
  Set.null bootstrapPeers

nullAll :: ExtraPeers peeraddr -> Bool
nullAll cprp = nullPublicConfig cprp && nullBootstrap cprp

member :: Ord peeraddr => peeraddr -> ExtraPeers peeraddr -> Bool
member addr (ExtraPeers a b) =
  Map.member addr a || Set.member addr b

toSet :: Ord peeraddr => ExtraPeers peeraddr -> Set peeraddr
toSet (ExtraPeers a b) = Map.keysSet a <> b

size :: ExtraPeers peeraddr -> Int
size (ExtraPeers a b) = Map.size a + Set.size b

difference :: Ord peeraddr
           => ExtraPeers peeraddr
           -> Set peeraddr
           -> ExtraPeers peeraddr
difference (ExtraPeers a b) addrs =
  ExtraPeers (Map.withoutKeys a addrs)
                         (Set.difference b addrs)

intersection :: Ord peeraddr
             => ExtraPeers peeraddr
             -> Set peeraddr
             -> ExtraPeers peeraddr
intersection (ExtraPeers a b) addrs =
  ExtraPeers (Map.restrictKeys a addrs)
                         (Set.intersection b addrs)

toAdvertisePeersMap :: Ord peeraddr
                    => ExtraPeers peeraddr
                    -> Map peeraddr PeerAdvertise
toAdvertisePeersMap (ExtraPeers a b) =
  a <> Set.foldl (\m p -> Map.insert p DoNotAdvertisePeer m) Map.empty b
