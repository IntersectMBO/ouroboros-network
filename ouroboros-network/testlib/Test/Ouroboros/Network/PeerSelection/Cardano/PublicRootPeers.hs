{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.Cardano.PublicRootPeers
  ( arbitraryCardanoExtraPeers
  , tests
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers

import Test.Ouroboros.Network.PeerSelection.Instances
import Test.Ouroboros.Network.Utils (ShrinkCarefully, prop_shrink_nonequal,
           prop_shrink_valid)

import Ouroboros.Cardano.Network.PublicRootPeers qualified as Cardano
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "PublicRootPeers"
    [ testProperty "arbitrary"              prop_arbitrary_CardanoExtraPeers
    , testProperty "fromMapAndSet"          prop_fromMapAndSet
    , testProperty "fromMapAndSet'"         prop_fromMapAndSet'
    , testProperty "fromToMapAndSet"        prop_fromToMapAndSet
    , testProperty "merge"                  prop_merge
    , testProperty "insertPublicConfigPeer" prop_insertPublicConfigPeer
    , testProperty "insertBootstrapPeer"    prop_insertBootstrapPeer
    , testProperty "insertLedgerPeer"       prop_insertLedgerPeer
    , testProperty "insertBigLedgerPeer"    prop_insertBigLedgerPeer
    , testProperty "fromPeers"              prop_fromPeers
    , testProperty "shrink"                 prop_shrink_CardanoExtraPeers
    ]
  ]

arbitraryCardanoExtraPeers :: (Ord peeraddr, Arbitrary peeraddr)
                                => Gen (PublicRootPeers (Cardano.ExtraPeers peeraddr) peeraddr)
arbitraryCardanoExtraPeers = do
  peeraddrs <- arbitrary
  let peersSize = Set.size peeraddrs
      (publicConfigPeers, otherPeers) = Set.splitAt (peersSize `div` 2) peeraddrs
      (bootstrapPeers, ledgerPeers) = Set.splitAt (Set.size otherPeers `div` 2) otherPeers
      (normalLedgerPeers, bigLedgerPeers) = Set.splitAt (Set.size ledgerPeers `div` 2) ledgerPeers

  advertiseInfo <- vectorOf (Set.size publicConfigPeers) arbitrary

  let publicConfigPeersMap = Map.fromList
                           $ zip (Set.toList publicConfigPeers) advertiseInfo

  return (PublicRootPeers.fromMapAndSet publicConfigPeersMap bootstrapPeers normalLedgerPeers bigLedgerPeers)

instance ( Arbitrary peeraddr
         , Ord peeraddr
         ) => Arbitrary (PublicRootPeers (Cardano.ExtraPeers peeraddr) peeraddr) where
    arbitrary = arbitraryCardanoExtraPeers

    shrink (PublicRootPeers lp blp (Cardano.ExtraPeers pp bsp)) =
        PublicRootPeers.fromMapAndSet <$> shrink pp
                                      <*> shrink bsp
                                      <*> shrink lp
                                      <*> shrink blp

prop_arbitrary_CardanoExtraPeers :: PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr -> Property
prop_arbitrary_CardanoExtraPeers = property . PublicRootPeers.invariant Cardano.invariant Cardano.toSet


prop_shrink_CardanoExtraPeers
  :: ShrinkCarefully (PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr)
  -> Property
prop_shrink_CardanoExtraPeers x =
      prop_shrink_valid (PublicRootPeers.invariant Cardano.invariant Cardano.toSet) x
 .&&. prop_shrink_nonequal x

prop_fromMapAndSet :: Map PeerAddr PeerAdvertise
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Bool
prop_fromMapAndSet pp bsp lp =
  let newPP = pp `Map.withoutKeys` bsp
   in PublicRootPeers.invariant Cardano.invariant Cardano.toSet
    . PublicRootPeers.fromMapAndSet newPP bsp lp

prop_fromToMapAndSet :: PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr -> Bool
prop_fromToMapAndSet prp@(PublicRootPeers lp blp (Cardano.ExtraPeers pp bsp)) =
  PublicRootPeers.fromMapAndSet pp bsp lp blp == prp

prop_fromMapAndSet' :: Map PeerAddr PeerAdvertise
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Property
prop_fromMapAndSet' pp bsp lp blp =
  -- Make sets disjoint
  let pp'  = Map.withoutKeys pp (bsp <> lp <> blp)
      bsp' = Set.difference bsp (lp <> blp)
      lp'  = Set.difference lp blp
      prp = PublicRootPeers.fromMapAndSet pp' bsp' lp' blp
   in  PublicRootPeers.getPublicConfigPeers prp === pp'
    .&&. PublicRootPeers.getBootstrapPeers prp  === bsp'
    .&&. PublicRootPeers.getLedgerPeers prp       === lp'
    .&&. PublicRootPeers.getBigLedgerPeers prp    === blp

prop_merge :: PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr
           -> PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr
           -> Bool
prop_merge prp = PublicRootPeers.invariant Cardano.invariant Cardano.toSet
               . (<> prp)

prop_insertPublicConfigPeer :: PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr
                            -> PeerAddr
                            -> PeerAdvertise
                            -> Bool
prop_insertPublicConfigPeer prp p pa =
  PublicRootPeers.invariant Cardano.invariant Cardano.toSet (PublicRootPeers.insertPublicConfigPeer p pa prp)

prop_insertBootstrapPeer :: PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr
                         -> PeerAddr
                         -> Bool
prop_insertBootstrapPeer prp p =
  PublicRootPeers.invariant Cardano.invariant Cardano.toSet (PublicRootPeers.insertBootstrapPeer p prp)

prop_insertLedgerPeer :: PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr
                      -> PeerAddr
                      -> Bool
prop_insertLedgerPeer prp p =
  PublicRootPeers.invariant Cardano.invariant Cardano.toSet (PublicRootPeers.insertLedgerPeer p prp)

prop_insertBigLedgerPeer :: PublicRootPeers (Cardano.ExtraPeers PeerAddr) PeerAddr
                         -> PeerAddr
                         -> Bool
prop_insertBigLedgerPeer prp p =
  PublicRootPeers.invariant Cardano.invariant Cardano.toSet (PublicRootPeers.insertBigLedgerPeer p prp)

prop_fromPeers :: PeerAddr
               -> PeerAdvertise
               -> Bool
prop_fromPeers peer peerAdvertise =
  let insertPP  = PublicRootPeers.insertPublicConfigPeer peer peerAdvertise (PublicRootPeers.empty Cardano.empty)
      insertBSP = PublicRootPeers.insertBootstrapPeer peer (PublicRootPeers.empty Cardano.empty)
      insertLP  = PublicRootPeers.insertLedgerPeer peer (PublicRootPeers.empty Cardano.empty)
      insertBLP = PublicRootPeers.insertBigLedgerPeer peer (PublicRootPeers.empty Cardano.empty)
   in    PublicRootPeers.getPublicConfigPeers insertPP == Map.singleton peer peerAdvertise
      && PublicRootPeers.getBootstrapPeers insertBSP   == Set.singleton peer
      && PublicRootPeers.getLedgerPeers insertLP       == Set.singleton peer
      && PublicRootPeers.getBigLedgerPeers insertBLP   == Set.singleton peer
