{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Ouroboros.Network.PeerSelection.PublicRootPeers
  ( -- * Types
    PublicRootPeers (..)
    -- Export constructors for defining tests.
  , invariant
    -- ** Polymorphic operations
  , member
  , empty
  , null
  , size
  , toAllLedgerPeerSet
  , fromLedgerPeers
  , fromBigLedgerPeers
  , toSet
  , difference
  , intersection
  , fromDisjointSets
  , mergeG
    -- ** Cardano Node specific operations
  , merge
  , getPublicConfigPeers
  , getBootstrapPeers
  , toSet
  , toPublicConfigPeerSet
  , insertPublicConfigPeer
  , insertBootstrapPeer
  , insertLedgerPeer
  , insertBigLedgerPeer
  , fromPublicRootPeers
  , fromBootstrapPeers
  , fromMapAndSet
  ) where

import Prelude hiding (null)

import Cardano.Network.PublicRootPeers (CardanoPublicRootPeers (..))
import Cardano.Network.PublicRootPeers qualified as CPRP
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
data PublicRootPeers extraPeers peeraddr =
  PublicRootPeers { getLedgerPeers    :: !(Set peeraddr)
                  , getBigLedgerPeers :: !(Set peeraddr)
                  , getExtraPeers     :: !extraPeers
                  }
  deriving (Eq, Show)

empty :: extraPeers -> PublicRootPeers extraPeers peeraddr
empty emptyExtraPeers = PublicRootPeers Set.empty Set.empty emptyExtraPeers

member :: Ord peeraddr
       => peeraddr
       -> (peeraddr -> extraPeers -> Bool)
       -> PublicRootPeers extraPeers peeraddr
       -> Bool
member p memberExtraPeers (PublicRootPeers lp blp ep) =
     memberExtraPeers p ep
  || Set.member p lp
  || Set.member p blp

fromLedgerPeers :: Monoid extraPeers
                => Set peeraddr
                -> PublicRootPeers extraPeers peeraddr
fromLedgerPeers lp =
  (empty mempty) { getLedgerPeers = lp }

fromBigLedgerPeers :: Monoid extraPeers
                   => Set peeraddr
                   -> PublicRootPeers extraPeers peeraddr
fromBigLedgerPeers blp =
  (empty mempty) { getBigLedgerPeers = blp }

nullLedgerPeers :: PublicRootPeers extraPeers peeraddr -> Bool
nullLedgerPeers PublicRootPeers { getLedgerPeers } =
  Set.null getLedgerPeers

nullBigLedgerPeers :: PublicRootPeers extraPeers peeraddr -> Bool
nullBigLedgerPeers PublicRootPeers { getBigLedgerPeers } =
  Set.null getBigLedgerPeers

nullAllLedgerPeers :: PublicRootPeers extraPeers peeraddr -> Bool
nullAllLedgerPeers prp =
  nullLedgerPeers prp && nullBigLedgerPeers prp

toAllLedgerPeerSet :: Ord peeraddr => PublicRootPeers extraPeers peeraddr -> Set peeraddr
toAllLedgerPeerSet (PublicRootPeers lp blp _) = lp <> blp

toSet :: Ord peeraddr
      => (extraPeers -> Set peeraddr)
      -> PublicRootPeers extraPeers peeraddr
      -> Set peeraddr
toSet extraPeersToSet (PublicRootPeers lp blp ep) =
  extraPeersToSet ep <> lp <> blp

invariant :: Ord peeraddr
          => (extraPeers -> Bool)
          -> (extraPeers -> Set peeraddr)
          -> PublicRootPeers extraPeers peeraddr
          -> Bool
invariant invariantExtraPeers extraPeersToSet (PublicRootPeers lp blp ep) =
  -- Invariant of extraPeers hold and its set is disjoint
     invariantExtraPeers ep
  && Set.null (Set.intersection (extraPeersToSet ep) lp)
  && Set.null (Set.intersection (extraPeersToSet ep) blp)

    -- ledger and big ledger peers should not overlap
  && Set.null (Set.intersection lp blp)

null :: (extraPeers -> Bool)
     -> PublicRootPeers extraPeers peeraddr -> Bool
null nullExtraPeers prp@(PublicRootPeers _ _ ep)  =
    nullExtraPeers ep
  && nullAllLedgerPeers prp

size :: (extraPeers -> Int)
     -> PublicRootPeers extraPeers peeraddr -> Int
size sizeExtraPeers (PublicRootPeers lp blp ep) =
    sizeExtraPeers ep
  + Set.size lp
  + Set.size blp

difference :: Ord peeraddr
           => (extraPeers -> Set peeraddr -> extraPeers)
           -> PublicRootPeers extraPeers peeraddr
           -> Set peeraddr
           -> PublicRootPeers extraPeers peeraddr
difference differenceExtraPeers (PublicRootPeers lp blp ep) addrs =
  PublicRootPeers (lp \\ addrs) (blp \\ addrs) (differenceExtraPeers ep addrs)

intersection :: Ord peeraddr
             => (extraPeers -> Set peeraddr -> extraPeers)
             -> PublicRootPeers extraPeers peeraddr
             -> Set peeraddr
             -> PublicRootPeers extraPeers peeraddr
intersection intersectionExtraPeers (PublicRootPeers lp blp ep) addrs =
  PublicRootPeers (lp `Set.intersection` addrs)
                  (blp `Set.intersection` addrs)
                  (intersectionExtraPeers ep addrs)

-- Generalized fromMapAndSet function
fromDisjointSets :: Ord peeraddr
                 => (extraPeers -> Set peeraddr)
                 -> extraPeers
                 -> Set peeraddr -- ^ ledger peers
                 -> Set peeraddr -- ^ big ledger peers
                 -> PublicRootPeers extraPeers peeraddr
fromDisjointSets extraPeersToSet ep lp blp =
    let epSet = extraPeersToSet ep
        newBLP = blp `Set.difference` (epSet <> lp)
        newLP = lp `Set.difference` (epSet <> newBLP)
    in PublicRootPeers newLP newBLP ep

-- Generalized merge function
mergeG :: ( Ord peeraddr
          , Semigroup extraPeers
          )
       => (extraPeers -> Set peeraddr)
       -> PublicRootPeers extraPeers peeraddr
       -> PublicRootPeers extraPeers peeraddr
       -> PublicRootPeers extraPeers peeraddr
mergeG extraPeersToSet
       (PublicRootPeers lp1 blp1 ep1) (PublicRootPeers lp2 blp2 ep2) =
    fromDisjointSets
        extraPeersToSet
        (ep1 <> ep2)
        (lp1 <> lp2)
        (blp1 <> blp2)


-- Cardano Node Public Roots

instance ( Ord peeraddr
         , extraPeers ~ CardanoPublicRootPeers peeraddr
         ) => Semigroup (PublicRootPeers extraPeers peeraddr) where
  (<>) = merge

instance ( Ord peeraddr
         , extraPeers ~ CardanoPublicRootPeers peeraddr
         ) => Monoid (PublicRootPeers extraPeers peeraddr) where
  mempty = empty CPRP.empty

merge :: Ord peeraddr
      => PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
      -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
      -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
merge a b = mergeG CPRP.toSet a b

getPublicConfigPeers :: PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Map peeraddr PeerAdvertise
getPublicConfigPeers PublicRootPeers { getExtraPeers = (CardanoPublicRootPeers pp _) } = pp

getBootstrapPeers :: PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Set peeraddr
getBootstrapPeers PublicRootPeers { getExtraPeers = (CardanoPublicRootPeers _ bsp) } = bsp

toPublicConfigPeerSet :: PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Set peeraddr
toPublicConfigPeerSet PublicRootPeers {
                        getExtraPeers = CardanoPublicRootPeers pp _
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
              -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
fromMapAndSet pp bsp =
  fromDisjointSets CPRP.toSet (CPRP.fromMapAndSet pp bsp)

fromPublicRootPeers :: Map peeraddr PeerAdvertise
                    -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
fromPublicRootPeers pp =
  empty (CPRP.empty { cprpGetPublicConfigPeers = pp })

fromBootstrapPeers :: Set peeraddr
                   -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
fromBootstrapPeers bsp =
  empty (CPRP.empty { cprpGetBootstrapPeers = bsp })

insertPublicConfigPeer :: Ord peeraddr
                       => peeraddr
                       -> PeerAdvertise
                       -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
                       -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
insertPublicConfigPeer p pa prp@PublicRootPeers {
    getExtraPeers = CardanoPublicRootPeers { cprpGetBootstrapPeers } } =
  let prp'@PublicRootPeers {
        getExtraPeers = CardanoPublicRootPeers {
          cprpGetPublicConfigPeers = pp
        }
      } = difference CPRP.difference prp (Set.singleton p)
  in if Set.member p cprpGetBootstrapPeers
     then prp
     else prp' { getExtraPeers = (getExtraPeers prp') {
                   cprpGetPublicConfigPeers = Map.insert p pa pp
                 }
               }

insertBootstrapPeer :: Ord peeraddr
                    => peeraddr
                    -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
                    -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
insertBootstrapPeer p prp =
  let prp'@PublicRootPeers {
        getExtraPeers = CardanoPublicRootPeers {
          cprpGetBootstrapPeers = bsp
        }
      } = difference CPRP.difference prp (Set.singleton p)
  in prp' { getExtraPeers = (getExtraPeers prp') {
              cprpGetBootstrapPeers = Set.insert p bsp
            }
          }

insertLedgerPeer :: Ord peeraddr
                 => peeraddr
                 -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
                 -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
insertLedgerPeer p prp@PublicRootPeers {
                     getExtraPeers = CardanoPublicRootPeers {
                       cprpGetBootstrapPeers
                     }
                   } =
  let prp'@PublicRootPeers { getLedgerPeers } =
        difference CPRP.difference prp (Set.singleton p)
   in if Set.member p cprpGetBootstrapPeers
         then prp
         else prp' {getLedgerPeers = Set.insert p getLedgerPeers }

insertBigLedgerPeer :: Ord peeraddr
                    => peeraddr
                    -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
                    -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
insertBigLedgerPeer p prp@PublicRootPeers{ getExtraPeers = CardanoPublicRootPeers { cprpGetBootstrapPeers } } =
  let prp'@PublicRootPeers { getBigLedgerPeers } =
        difference CPRP.difference prp (Set.singleton p)
   in if Set.member p cprpGetBootstrapPeers
         then prp
         else prp' { getBigLedgerPeers = Set.insert p getBigLedgerPeers }
