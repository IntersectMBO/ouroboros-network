{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeOperators #-}

module Test.Ouroboros.Network.PeerSelection.Cardano.PublicRootPeers
  ( arbitraryCardanoPublicRootPeers
  , tests
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers

import Ouroboros.Network.Testing.Utils (ShrinkCarefully, prop_shrink_nonequal,
           prop_shrink_valid)
import Test.Ouroboros.Network.PeerSelection.Instances


import Cardano.Network.PublicRootPeers (CardanoPublicRootPeers (..))
import Cardano.Network.PublicRootPeers qualified as CNPRP
import Cardano.Network.PublicRootPeers qualified as CPRP
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "PublicRootPeers"
    [ testProperty "arbitrary"              prop_arbitrary_CardanoPublicRootPeers
    , testProperty "fromMapAndSet"          prop_fromMapAndSet
    , testProperty "fromMapAndSet'"         prop_fromMapAndSet'
    , testProperty "fromToMapAndSet"        prop_fromToMapAndSet
    , testProperty "merge"                  prop_merge
    , testProperty "insertPublicConfigPeer" prop_insertPublicConfigPeer
    , testProperty "insertBootstrapPeer"    prop_insertBootstrapPeer
    , testProperty "insertLedgerPeer"       prop_insertLedgerPeer
    , testProperty "insertBigLedgerPeer"    prop_insertBigLedgerPeer
    , testProperty "fromPeers"              prop_fromPeers
    , testProperty "shrink"                 prop_shrink_CardanoPublicRootPeers
    ]
  ]

arbitraryCardanoPublicRootPeers :: (Ord peeraddr, Arbitrary peeraddr)
                                => Gen (PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr)
arbitraryCardanoPublicRootPeers = do
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
         , extraPeers ~ CardanoPublicRootPeers peeraddr
         ) => Arbitrary (PublicRootPeers extraPeers peeraddr) where
    arbitrary = arbitraryCardanoPublicRootPeers

    shrink (PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp)) =
        PublicRootPeers.fromMapAndSet <$> shrink pp
                                      <*> shrink bsp
                                      <*> shrink lp
                                      <*> shrink blp

prop_arbitrary_CardanoPublicRootPeers :: PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr -> Property
prop_arbitrary_CardanoPublicRootPeers = property . PublicRootPeers.invariant CPRP.invariant CPRP.toSet


prop_shrink_CardanoPublicRootPeers :: ShrinkCarefully (PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr) -> Property
prop_shrink_CardanoPublicRootPeers x =
      prop_shrink_valid (PublicRootPeers.invariant CPRP.invariant CPRP.toSet) x
 .&&. prop_shrink_nonequal x

prop_fromMapAndSet :: Map PeerAddr PeerAdvertise
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Bool
prop_fromMapAndSet pp bsp lp =
  let newPP = pp `Map.withoutKeys` bsp
   in PublicRootPeers.invariant CPRP.invariant CPRP.toSet
    . PublicRootPeers.fromMapAndSet newPP bsp lp

prop_fromToMapAndSet :: PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr -> Bool
prop_fromToMapAndSet prp@(PublicRootPeers lp blp (CardanoPublicRootPeers pp bsp)) =
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

prop_merge :: PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr
           -> PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr
           -> Bool
prop_merge prp = PublicRootPeers.invariant CPRP.invariant CPRP.toSet
               . (<> prp)

prop_insertPublicConfigPeer :: PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr
                            -> PeerAddr
                            -> PeerAdvertise
                            -> Bool
prop_insertPublicConfigPeer prp p pa =
  PublicRootPeers.invariant CPRP.invariant CPRP.toSet (PublicRootPeers.insertPublicConfigPeer p pa prp)

prop_insertBootstrapPeer :: PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr
                         -> PeerAddr
                         -> Bool
prop_insertBootstrapPeer prp p =
  PublicRootPeers.invariant CPRP.invariant CPRP.toSet (PublicRootPeers.insertBootstrapPeer p prp)

prop_insertLedgerPeer :: PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr
                      -> PeerAddr
                      -> Bool
prop_insertLedgerPeer prp p =
  PublicRootPeers.invariant CPRP.invariant CPRP.toSet (PublicRootPeers.insertLedgerPeer p prp)

prop_insertBigLedgerPeer :: PublicRootPeers (CardanoPublicRootPeers PeerAddr) PeerAddr
                         -> PeerAddr
                         -> Bool
prop_insertBigLedgerPeer prp p =
  PublicRootPeers.invariant CPRP.invariant CPRP.toSet (PublicRootPeers.insertBigLedgerPeer p prp)

prop_fromPeers :: PeerAddr
               -> PeerAdvertise
               -> Bool
prop_fromPeers peer peerAdvertise =
  let insertPP  = PublicRootPeers.insertPublicConfigPeer peer peerAdvertise (PublicRootPeers.empty CNPRP.empty)
      insertBSP = PublicRootPeers.insertBootstrapPeer peer (PublicRootPeers.empty CNPRP.empty)
      insertLP  = PublicRootPeers.insertLedgerPeer peer (PublicRootPeers.empty CNPRP.empty)
      insertBLP = PublicRootPeers.insertBigLedgerPeer peer (PublicRootPeers.empty CNPRP.empty)
   in    PublicRootPeers.getPublicConfigPeers insertPP == Map.singleton peer peerAdvertise
      && PublicRootPeers.getBootstrapPeers insertBSP   == Set.singleton peer
      && PublicRootPeers.getLedgerPeers insertLP       == Set.singleton peer
      && PublicRootPeers.getBigLedgerPeers insertBLP   == Set.singleton peer
