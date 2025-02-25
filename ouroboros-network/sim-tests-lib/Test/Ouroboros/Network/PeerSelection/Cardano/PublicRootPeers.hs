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

import Test.Ouroboros.Network.PeerSelection.Instances
import Test.Ouroboros.Network.Utils (ShrinkCarefully, prop_shrink_nonequal,
           prop_shrink_valid)

import Ouroboros.Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.PublicRootPeers
           (CardanoPublicRootPeers)
import Ouroboros.Cardano.Network.PeerSelection.PublicRootPeers qualified as Cardano.PublicRootPeers
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
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
                                => Gen (CardanoPublicRootPeers peeraddr)
arbitraryCardanoExtraPeers = do
  peeraddrs <- arbitrary
  let peersSize = Set.size peeraddrs
      (publicConfigPeers, otherPeers) = Set.splitAt (peersSize `div` 2) peeraddrs
      (bootstrapPeers, ledgerPeers) = Set.splitAt (Set.size otherPeers `div` 2) otherPeers
      (normalLedgerPeers, bigLedgerPeers) = Set.splitAt (Set.size ledgerPeers `div` 2) ledgerPeers

  advertiseInfo <- vectorOf (Set.size publicConfigPeers) arbitrary

  let publicConfigPeersMap = Map.fromList
                           $ zip (Set.toList publicConfigPeers) advertiseInfo

  return (Cardano.PublicRootPeers.fromMapAndSet publicConfigPeersMap bootstrapPeers normalLedgerPeers bigLedgerPeers)

instance ( Arbitrary peeraddr
         , Ord peeraddr
         ) => Arbitrary (CardanoPublicRootPeers peeraddr) where
    arbitrary = arbitraryCardanoExtraPeers

    shrink (PublicRootPeers lp blp (Cardano.ExtraPeers pp bsp)) =
        Cardano.PublicRootPeers.fromMapAndSet <$> shrink pp
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
    . Cardano.PublicRootPeers.fromMapAndSet newPP bsp lp

prop_fromToMapAndSet :: CardanoPublicRootPeers PeerAddr -> Bool
prop_fromToMapAndSet prp@(PublicRootPeers lp blp (Cardano.ExtraPeers pp bsp)) =
  Cardano.PublicRootPeers.fromMapAndSet pp bsp lp blp == prp

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
      prp = Cardano.PublicRootPeers.fromMapAndSet pp' bsp' lp' blp
   in  Cardano.PublicRootPeers.getPublicConfigPeers prp === pp'
    .&&. Cardano.PublicRootPeers.getBootstrapPeers prp  === bsp'
    .&&. PublicRootPeers.getLedgerPeers prp       === lp'
    .&&. PublicRootPeers.getBigLedgerPeers prp    === blp

prop_merge :: CardanoPublicRootPeers PeerAddr
           -> CardanoPublicRootPeers PeerAddr
           -> Bool
prop_merge prp = PublicRootPeers.invariant Cardano.invariant Cardano.toSet
               . (<> prp)

prop_insertPublicConfigPeer :: CardanoPublicRootPeers PeerAddr
                            -> PeerAddr
                            -> PeerAdvertise
                            -> Bool
prop_insertPublicConfigPeer prp p pa =
  PublicRootPeers.invariant Cardano.invariant Cardano.toSet (Cardano.PublicRootPeers.insertPublicConfigPeer p pa prp)

prop_insertBootstrapPeer :: CardanoPublicRootPeers PeerAddr
                         -> PeerAddr
                         -> Bool
prop_insertBootstrapPeer prp p =
  PublicRootPeers.invariant Cardano.invariant Cardano.toSet (Cardano.PublicRootPeers.insertBootstrapPeer p prp)

prop_insertLedgerPeer :: CardanoPublicRootPeers PeerAddr
                      -> PeerAddr
                      -> Bool
prop_insertLedgerPeer prp p =
  PublicRootPeers.invariant Cardano.invariant Cardano.toSet (Cardano.PublicRootPeers.insertLedgerPeer p prp)

prop_insertBigLedgerPeer :: CardanoPublicRootPeers PeerAddr
                         -> PeerAddr
                         -> Bool
prop_insertBigLedgerPeer prp p =
  PublicRootPeers.invariant Cardano.invariant Cardano.toSet (Cardano.PublicRootPeers.insertBigLedgerPeer p prp)

prop_fromPeers :: PeerAddr
               -> PeerAdvertise
               -> Bool
prop_fromPeers peer peerAdvertise =
  let insertPP  = Cardano.PublicRootPeers.insertPublicConfigPeer peer peerAdvertise (PublicRootPeers.empty Cardano.empty)
      insertBSP = Cardano.PublicRootPeers.insertBootstrapPeer peer (PublicRootPeers.empty Cardano.empty)
      insertLP  = Cardano.PublicRootPeers.insertLedgerPeer peer (PublicRootPeers.empty Cardano.empty)
      insertBLP = Cardano.PublicRootPeers.insertBigLedgerPeer peer (PublicRootPeers.empty Cardano.empty)
   in    Cardano.PublicRootPeers.getPublicConfigPeers insertPP == Map.singleton peer peerAdvertise
      && Cardano.PublicRootPeers.getBootstrapPeers insertBSP   == Set.singleton peer
      && PublicRootPeers.getLedgerPeers insertLP       == Set.singleton peer
      && PublicRootPeers.getBigLedgerPeers insertBLP   == Set.singleton peer
