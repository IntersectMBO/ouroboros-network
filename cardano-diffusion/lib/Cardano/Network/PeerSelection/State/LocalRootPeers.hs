{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Network.PeerSelection.State.LocalRootPeers
  ( clampToTrustable
  , isPeerTrustable
  , trustableKeysSet
  , module Ouroboros.Network.PeerSelection.State.LocalRootPeers
  ) where


import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers

clampToTrustable :: Ord peeraddr
                 => LocalRootPeers PeerTrustable peeraddr
                 -> LocalRootPeers PeerTrustable peeraddr
clampToTrustable (LocalRootPeers m gs) =
  let trustedMap = Map.filter (\LocalRootConfig { extraLocalRootFlags } ->
                                case extraLocalRootFlags of
                                  IsTrustable    -> True
                                  IsNotTrustable -> False
                              )
                              m
   in LocalRootPeers trustedMap (trustedGroups gs)
  where
    trustedGroups [] = []
    trustedGroups ((h, w, g):gss) =
      let trusted = Map.filter (\LocalRootConfig { extraLocalRootFlags } ->
                                 case extraLocalRootFlags of
                                   IsTrustable    -> True
                                   IsNotTrustable -> False
                               )
                               m
          trustedSet = Map.keysSet trusted
          trustedGroup = Set.intersection g trustedSet
          w' = min w (WarmValency (Set.size trustedGroup))
          h' = HotValency (getHotValency h `min` getWarmValency w')
       in if Set.null trustedGroup
             then trustedGroups gss
             else (h', w', trustedGroup) : trustedGroups gss

isPeerTrustable :: Ord peeraddr
                => peeraddr
                -> LocalRootPeers PeerTrustable peeraddr
                -> Bool
isPeerTrustable peeraddr lrp =
  case Map.lookup peeraddr (toMap lrp) of
    Just LocalRootConfig { extraLocalRootFlags = IsTrustable }
      -> True
    _ -> False

trustableKeysSet :: LocalRootPeers PeerTrustable peeraddr
                 -> Set peeraddr
trustableKeysSet (LocalRootPeers m _) =
    Map.keysSet
  . Map.filter (\LocalRootConfig { extraLocalRootFlags } ->
                 case extraLocalRootFlags of
                   IsTrustable    -> True
                   IsNotTrustable -> False)
  $ m
