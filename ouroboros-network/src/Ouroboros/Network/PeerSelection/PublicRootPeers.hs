{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}

module Ouroboros.Network.PeerSelection.PublicRootPeers
  ( -- * Types
    PublicRootPeers (..)
    -- Export constructors for defining tests.
  , invariant
    -- ** Polymorphic operations
  , empty
  , toAllLedgerPeerSet
  , fromLedgerPeers
  , fromBigLedgerPeers
    -- ** Cardano Node specific operations
  , null
  , size
  , member
  , merge
  , difference
  , intersection
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

import Cardano.Node.PublicRootPeers (CardanoPublicRootPeers (..))
import Cardano.Node.PublicRootPeers qualified as CPRP
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

-- Cardano Node Public Roots

invariant :: Ord peeraddr
          => PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
          -> Bool
invariant (PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp)) =
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

null :: PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Bool
null prp = CPRP.nullPublicConfig (getExtraPeers prp)
        && CPRP.nullBootstrap (getExtraPeers prp)
        && nullAllLedgerPeers prp

size :: PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Int
size (PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp)) =
    Map.size pp
  + Set.size bsp
  + Set.size lp
  + Set.size blp

member :: Ord peeraddr => peeraddr -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Bool
member p (PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp)) =
     Map.member p pp
  || Set.member p bsp
  || Set.member p lp
  || Set.member p blp

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
merge (PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp))
      (PublicRootPeers lp' blp' (CardanoPublicRootPeers pp' bsp')) =
  fromMapAndSet (pp <> pp') (bsp <> bsp') (lp <> lp') (blp <> blp')

difference :: Ord peeraddr
           => PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
           -> Set peeraddr
           -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
difference (PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp)) addrs =
  fromMapAndSet (pp `Map.withoutKeys` addrs)
                (bsp \\ addrs)
                (lp \\ addrs)
                (blp \\ addrs)

intersection :: Ord peeraddr
             => PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
             -> Set peeraddr
             -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
intersection (PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp)) addrs =
  fromMapAndSet (pp `Map.restrictKeys` addrs)
                (bsp `Set.intersection` addrs)
                (lp `Set.intersection` addrs)
                (blp `Set.intersection` addrs)

getPublicConfigPeers :: PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Map peeraddr PeerAdvertise
getPublicConfigPeers PublicRootPeers { getExtraPeers = (CardanoPublicRootPeers pp _) } = pp

getBootstrapPeers :: PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Set peeraddr
getBootstrapPeers PublicRootPeers { getExtraPeers = (CardanoPublicRootPeers _ bsp) } = bsp

toSet :: Ord peeraddr => PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr -> Set peeraddr
toSet (PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp)) =
  Map.keysSet pp <> bsp <> lp <> blp

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
fromMapAndSet pp bsp lp blp =
  -- Enforcing invariants
  let newPP = pp `Map.withoutKeys` (bsp <> lp <> blp)
      newLP = lp \\ (bsp <> blp)
      newBLP = blp \\ bsp
   in PublicRootPeers newLP newBLP (CardanoPublicRootPeers newPP bsp)

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
      } = difference prp (Set.singleton p)
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
      } = difference prp (Set.singleton p)
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
  let prp'@PublicRootPeers { getLedgerPeers } = difference prp (Set.singleton p)
   in if Set.member p cprpGetBootstrapPeers
         then prp
         else prp' {getLedgerPeers = Set.insert p getLedgerPeers }

insertBigLedgerPeer :: Ord peeraddr
                    => peeraddr
                    -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
                    -> PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr
insertBigLedgerPeer p prp@PublicRootPeers{ getExtraPeers = CardanoPublicRootPeers { cprpGetBootstrapPeers } } =
  let prp'@PublicRootPeers { getBigLedgerPeers } = difference prp (Set.singleton p)
   in if Set.member p cprpGetBootstrapPeers
         then prp
         else prp' { getBigLedgerPeers = Set.insert p getBigLedgerPeers }
