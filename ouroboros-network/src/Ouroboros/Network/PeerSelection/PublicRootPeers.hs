{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.PeerSelection.PublicRootPeers
  ( -- * Types
    PublicRootPeers (..)
    -- Export constructors for defining tests.
  , invariant
    -- * Basic operations
  , empty
  , null
  , size
  , member
  , merge
  , difference
  , intersection
  , toSet
  , toPublicConfigPeerSet
  , toAllLedgerPeerSet
  , insertPublicConfigPeer
  , insertBootstrapPeer
  , insertLedgerPeer
  , insertBigLedgerPeer
  , fromPublicRootPeers
  , fromBootstrapPeers
  , fromLedgerPeers
  , fromBigLedgerPeers
  , fromMapAndSet
  ) where

import Prelude hiding (null)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)

---------------------------------------
-- Public root peer set representation
--
-- This data type is useful to abstract several public root peer sources into
-- one type. This helps cleaning up the code and centralizing everything under
-- a nice API.

-- | Public Root Peers consist of either a set of manually configured
-- bootstrap peers.
--
-- There's an implicit precedence that will priorise bootstrap peers over the
-- other sets, so if we are adding a bootstrap peer and that peer is already a
-- member of other public root set, it is going to be removed from that set
-- and added to the bootstrap peer set.
--
data PublicRootPeers peeraddr =
  PublicRootPeers { getPublicConfigPeers :: !(Map peeraddr PeerAdvertise)
                  , getBootstrapPeers    :: !(Set peeraddr)
                  , getLedgerPeers       :: !(Set peeraddr)
                  , getBigLedgerPeers    :: !(Set peeraddr)
                  }
  deriving (Eq, Show)

invariant :: Ord peeraddr => PublicRootPeers peeraddr -> Bool
invariant (PublicRootPeers pp bsp lp blp) =
     -- bootstrap peers and ledger peers should not overlap
     Set.null (Set.intersection bsp lp)
     -- bootstrap peers and big ledger peers should not overlap
  && Set.null (Set.intersection bsp blp)
     -- bootstrap peers and public config peers should not overlap
  && Set.null (Set.intersection bsp (Map.keysSet pp))
    -- ledger and big ledger peers should not overlap
  && Set.null (Set.intersection lp blp)
    -- ledger peers and public config peers should not overlap
  && Set.null (Set.intersection lp (Map.keysSet pp))
    -- big ledger peers and public config peers should not overlap
  && Set.null (Set.intersection blp (Map.keysSet pp))

empty :: PublicRootPeers peeraddr
empty = PublicRootPeers Map.empty Set.empty Set.empty Set.empty

nullPublicConfig :: PublicRootPeers peeraddr -> Bool
nullPublicConfig PublicRootPeers { getPublicConfigPeers } =
  Map.null getPublicConfigPeers

nullBootstrap :: PublicRootPeers peeraddr -> Bool
nullBootstrap PublicRootPeers { getBootstrapPeers } =
  Set.null getBootstrapPeers

nullLedgerPeers :: PublicRootPeers peeraddr -> Bool
nullLedgerPeers PublicRootPeers { getLedgerPeers } =
  Set.null getLedgerPeers

nullBigLedgerPeers :: PublicRootPeers peeraddr -> Bool
nullBigLedgerPeers PublicRootPeers { getBigLedgerPeers } =
  Set.null getBigLedgerPeers

nullAllLedgerPeers :: PublicRootPeers peeraddr -> Bool
nullAllLedgerPeers prp =
  nullLedgerPeers prp && nullBigLedgerPeers prp

null :: PublicRootPeers peeraddr -> Bool
null prp = nullPublicConfig prp
        && nullBootstrap prp
        && nullAllLedgerPeers prp

size :: PublicRootPeers peeraddr -> Int
size (PublicRootPeers pp bsp lp blp) =
    Map.size pp
  + Set.size bsp
  + Set.size lp
  + Set.size blp

member :: Ord peeraddr => peeraddr -> PublicRootPeers peeraddr -> Bool
member p (PublicRootPeers pp bsp lp blp) =
     Map.member p pp
  || Set.member p bsp
  || Set.member p lp
  || Set.member p blp

instance Ord peeraddr => Semigroup (PublicRootPeers peeraddr) where
  (<>) = merge

instance Ord peeraddr => Monoid (PublicRootPeers peeraddr) where
  mempty = empty

merge :: Ord peeraddr
      => PublicRootPeers peeraddr
      -> PublicRootPeers peeraddr
      -> PublicRootPeers peeraddr
merge (PublicRootPeers pp bsp lp blp) (PublicRootPeers pp' bsp' lp' blp') =
  fromMapAndSet (pp <> pp') (bsp <> bsp') (lp <> lp') (blp <> blp')

difference :: Ord peeraddr
           => PublicRootPeers peeraddr
           -> Set peeraddr
           -> PublicRootPeers peeraddr
difference (PublicRootPeers pp bsp lp blp) addrs =
  PublicRootPeers (pp `Map.withoutKeys` addrs)
                  (bsp \\ addrs)
                  (lp \\ addrs)
                  (blp \\ addrs)

intersection :: Ord peeraddr
             => PublicRootPeers peeraddr
             -> Set peeraddr
             -> PublicRootPeers peeraddr
intersection (PublicRootPeers pp bsp lp blp) addrs =
  PublicRootPeers (pp `Map.restrictKeys` addrs)
                  (bsp `Set.intersection` addrs)
                  (lp `Set.intersection` addrs)
                  (blp `Set.intersection` addrs)

toSet :: Ord peeraddr => PublicRootPeers peeraddr -> Set peeraddr
toSet (PublicRootPeers pp bsp lp blp) =
  Map.keysSet pp <> bsp <> lp <> blp

toPublicConfigPeerSet :: PublicRootPeers peeraddr -> Set peeraddr
toPublicConfigPeerSet (PublicRootPeers pp _ _ _) = Map.keysSet pp

toAllLedgerPeerSet :: Ord peeraddr => PublicRootPeers peeraddr -> Set peeraddr
toAllLedgerPeerSet (PublicRootPeers _ _ lp blp) = lp <> blp

-- | Preserves PublicRootPeers invariant. If the two sets are not disjoint,
-- removes the common ones from the bootstrap peers set since its the most
-- sensitive set.
--
fromMapAndSet :: Ord peeraddr
              => Map peeraddr PeerAdvertise -- ^ public configured root peers
              -> Set peeraddr -- ^ bootstrap peers
              -> Set peeraddr -- ^ ledger peers
              -> Set peeraddr -- ^ big ledger peers
              -> PublicRootPeers peeraddr
fromMapAndSet pp bsp lp blp =
  -- Enforcing invariants
  let newPP = pp `Map.withoutKeys` (bsp <> lp <> blp)
      newLP = lp \\ (bsp <> blp)
      newBLP = blp \\ bsp
   in PublicRootPeers newPP bsp newLP newBLP

fromPublicRootPeers :: Map peeraddr PeerAdvertise
                    -> PublicRootPeers peeraddr
fromPublicRootPeers pp =
  empty { getPublicConfigPeers = pp }

fromBootstrapPeers :: Set peeraddr
                   -> PublicRootPeers peeraddr
fromBootstrapPeers bsp =
  empty { getBootstrapPeers = bsp }

fromLedgerPeers :: Set peeraddr
                -> PublicRootPeers peeraddr
fromLedgerPeers lp =
  empty { getLedgerPeers = lp }

fromBigLedgerPeers :: Set peeraddr
                   -> PublicRootPeers peeraddr
fromBigLedgerPeers blp =
  empty { getBigLedgerPeers = blp }

insertPublicConfigPeer :: Ord peeraddr
                       => peeraddr
                       -> PeerAdvertise
                       -> PublicRootPeers peeraddr
                       -> PublicRootPeers peeraddr
insertPublicConfigPeer p pa prp@PublicRootPeers { getBootstrapPeers } =
  let prp'@PublicRootPeers{ getPublicConfigPeers } = difference prp (Set.singleton p)
   in if Set.member p getBootstrapPeers
         then prp
         else prp' { getPublicConfigPeers = Map.insert p pa getPublicConfigPeers }

insertBootstrapPeer :: Ord peeraddr
                    => peeraddr
                    -> PublicRootPeers peeraddr
                    -> PublicRootPeers peeraddr
insertBootstrapPeer p prp =
  let prp'@PublicRootPeers { getBootstrapPeers } = difference prp (Set.singleton p)
   in prp' { getBootstrapPeers = Set.insert p getBootstrapPeers }

insertLedgerPeer :: Ord peeraddr
                 => peeraddr
                 -> PublicRootPeers peeraddr
                 -> PublicRootPeers peeraddr
insertLedgerPeer p prp@PublicRootPeers{ getBootstrapPeers } =
  let prp'@PublicRootPeers { getLedgerPeers } = difference prp (Set.singleton p)
   in if Set.member p getBootstrapPeers
         then prp
         else prp' {getLedgerPeers = Set.insert p getLedgerPeers }

insertBigLedgerPeer :: Ord peeraddr
                    => peeraddr
                    -> PublicRootPeers peeraddr
                    -> PublicRootPeers peeraddr
insertBigLedgerPeer p prp@PublicRootPeers { getBootstrapPeers } =
  let prp'@PublicRootPeers { getBigLedgerPeers } = difference prp (Set.singleton p)
   in if Set.member p getBootstrapPeers
         then prp
         else prp' { getBigLedgerPeers = Set.insert p getBigLedgerPeers }
