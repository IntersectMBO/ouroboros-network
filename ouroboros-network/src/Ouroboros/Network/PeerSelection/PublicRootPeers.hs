{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}

-- | The module is designed to be imported qualified.
--
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
  ) where

import Prelude hiding (null)

import Data.Set (Set, (\\))
import Data.Set qualified as Set

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

