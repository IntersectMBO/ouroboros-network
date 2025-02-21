{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Cardano.Network.PublicRootPeers where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Ouroboros.Cardano.Network.ExtraRootPeers (ExtraPeers (..))
import Ouroboros.Cardano.Network.ExtraRootPeers qualified as ExtraPeers
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers

type CardanoPublicRootPeers peeraddr =
  PublicRootPeers (ExtraPeers peeraddr) peeraddr

instance ( Ord peeraddr
         ) => Semigroup (CardanoPublicRootPeers peeraddr) where
  (<>) = merge

instance ( Ord peeraddr
         ) => Monoid (CardanoPublicRootPeers peeraddr) where
  mempty = empty ExtraPeers.empty

merge :: Ord peeraddr
      => CardanoPublicRootPeers peeraddr
      -> CardanoPublicRootPeers peeraddr
      -> CardanoPublicRootPeers peeraddr
merge a b = mergeG ExtraPeers.toSet a b

getPublicConfigPeers :: CardanoPublicRootPeers peeraddr -> Map peeraddr PeerAdvertise
getPublicConfigPeers PublicRootPeers { getExtraPeers = (ExtraPeers pp _) } = pp

getBootstrapPeers :: CardanoPublicRootPeers peeraddr -> Set peeraddr
getBootstrapPeers PublicRootPeers { getExtraPeers = (ExtraPeers _ bsp) } = bsp

toPublicConfigPeerSet :: CardanoPublicRootPeers peeraddr -> Set peeraddr
toPublicConfigPeerSet PublicRootPeers {
                        getExtraPeers = ExtraPeers pp _
                      } = Map.keysSet pp

-- | Preserves PublicRootPeers invariant. If the two sets are not disjoint,
-- removes the common ones from the bootstrap peers set since its the most
-- sensitive set.
--
fromMapAndSet :: Ord peeraddr
              => Map peeraddr PeerAdvertise -- ^ public configured root peers
              -> Set peeraddr -- ^ bootstrap peers
              -> Set peeraddr -- ^ ledger peers
              -> Set peeraddr -- ^ big ledger peers
              -> CardanoPublicRootPeers peeraddr
fromMapAndSet pp bsp =
  fromDisjointSets ExtraPeers.toSet (ExtraPeers.fromMapAndSet pp bsp)

fromPublicRootPeers :: Map peeraddr PeerAdvertise
                    -> CardanoPublicRootPeers peeraddr
fromPublicRootPeers pp =
  empty (ExtraPeers.empty { publicConfigPeers = pp })

fromBootstrapPeers :: Set peeraddr
                   -> CardanoPublicRootPeers peeraddr
fromBootstrapPeers bsp =
  empty (ExtraPeers.empty { bootstrapPeers = bsp })

insertPublicConfigPeer :: Ord peeraddr
                       => peeraddr
                       -> PeerAdvertise
                       -> CardanoPublicRootPeers peeraddr
                       -> CardanoPublicRootPeers peeraddr
insertPublicConfigPeer p pa prp@PublicRootPeers {
                              getExtraPeers = ExtraPeers {
                                bootstrapPeers = getBootstrapPeers'
                              }
                            } =
  let prp'@PublicRootPeers {
        getExtraPeers = ExtraPeers {
          publicConfigPeers = pp
        }
      } = difference ExtraPeers.difference prp (Set.singleton p)
  in if Set.member p getBootstrapPeers'
     then prp
     else prp' { getExtraPeers = (getExtraPeers prp') {
                   publicConfigPeers = Map.insert p pa pp
                 }
               }

insertBootstrapPeer :: Ord peeraddr
                    => peeraddr
                    -> CardanoPublicRootPeers peeraddr
                    -> CardanoPublicRootPeers peeraddr
insertBootstrapPeer p prp =
  let prp'@PublicRootPeers {
        getExtraPeers = ExtraPeers {
          bootstrapPeers = bsp
        }
      } = difference ExtraPeers.difference prp (Set.singleton p)
  in prp' { getExtraPeers = (getExtraPeers prp') {
              bootstrapPeers = Set.insert p bsp
            }
          }

insertLedgerPeer :: Ord peeraddr
                 => peeraddr
                 -> CardanoPublicRootPeers peeraddr
                 -> CardanoPublicRootPeers peeraddr
insertLedgerPeer p prp@PublicRootPeers {
                     getExtraPeers = ExtraPeers {
                       bootstrapPeers = getBootstrapPeers'
                     }
                   } =
  let prp'@PublicRootPeers { getLedgerPeers } =
        difference ExtraPeers.difference prp (Set.singleton p)
   in if Set.member p getBootstrapPeers'
         then prp
         else prp' {getLedgerPeers = Set.insert p getLedgerPeers }

insertBigLedgerPeer :: Ord peeraddr
                    => peeraddr
                    -> CardanoPublicRootPeers peeraddr
                    -> CardanoPublicRootPeers peeraddr
insertBigLedgerPeer p prp@PublicRootPeers{
                        getExtraPeers = ExtraPeers {
                          bootstrapPeers = getBootstrapPeers'
                        }
                      } =
  let prp'@PublicRootPeers { getBigLedgerPeers } =
        difference ExtraPeers.difference prp (Set.singleton p)
   in if Set.member p getBootstrapPeers'
         then prp
         else prp' { getBigLedgerPeers = Set.insert p getBigLedgerPeers }
