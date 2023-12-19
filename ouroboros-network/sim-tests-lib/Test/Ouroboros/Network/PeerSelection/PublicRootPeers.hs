{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.PublicRootPeers
  ( arbitraryPublicRootPeers
  , tests
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Network.PeerSelection.PublicRootPeers
                     (PublicRootPeers)
import qualified Ouroboros.Network.PeerSelection.PublicRootPeers as PublicRootPeers

import           Ouroboros.Network.Testing.Utils (ShrinkCarefully,
                     prop_shrink_nonequal, prop_shrink_valid)
import           Test.Ouroboros.Network.PeerSelection.Instances


import           Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import           Ouroboros.Network.PeerSelection.PublicRootPeers
                     (PublicRootPeers (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "PublicRootPeers"
    [ testProperty "arbitrary"              prop_arbitrary_PublicRootPeers
    , testProperty "fromMapAndSet"          prop_fromMapAndSet
    , testProperty "fromMapAndSet'"         prop_fromMapAndSet'
    , testProperty "fromToMapAndSet"        prop_fromToMapAndSet
    , testProperty "merge"                  prop_merge
    , testProperty "insertPublicConfigPeer" prop_insertPublicConfigPeer
    , testProperty "insertBootstrapPeer"    prop_insertBootstrapPeer
    , testProperty "insertLedgerPeer"       prop_insertLedgerPeer
    , testProperty "insertBigLedgerPeer"    prop_insertBigLedgerPeer
    , testProperty "fromPeers"              prop_fromPeers
    , testProperty "shrink"                 prop_shrink_PublicRootPeers
    ]
  ]

arbitraryPublicRootPeers :: Ord peeraddr
                        => Set peeraddr -> Gen (PublicRootPeers peeraddr)
arbitraryPublicRootPeers peeraddrs = do
  let peersSize = Set.size peeraddrs
      (publicConfigPeers, otherPeers) = Set.splitAt (peersSize `div` 2) peeraddrs
      (bootstrapPeers, ledgerPeers) = Set.splitAt (Set.size otherPeers `div` 2) otherPeers
      (normalLedgerPeers, bigLedgerPeers) = Set.splitAt (Set.size ledgerPeers `div` 2) ledgerPeers

  advertiseInfo <- vectorOf (Set.size publicConfigPeers) arbitrary

  let publicConfigPeersMap = Map.fromList
                           $ zip (Set.toList publicConfigPeers) advertiseInfo

  return (PublicRootPeers.fromMapAndSet publicConfigPeersMap bootstrapPeers normalLedgerPeers bigLedgerPeers)

instance (Arbitrary peeraddr, Ord peeraddr) =>
         Arbitrary (PublicRootPeers peeraddr) where
    arbitrary = do
        peeraddrs <- arbitrary
        arbitraryPublicRootPeers peeraddrs

    shrink (PublicRootPeers pp bsp lp blp) =
        PublicRootPeers.fromMapAndSet <$> shrink pp
                                      <*> shrink bsp
                                      <*> shrink lp
                                      <*> shrink blp

prop_arbitrary_PublicRootPeers :: PublicRootPeers PeerAddr -> Property
prop_arbitrary_PublicRootPeers = property . PublicRootPeers.invariant


prop_shrink_PublicRootPeers :: ShrinkCarefully (PublicRootPeers PeerAddr) -> Property
prop_shrink_PublicRootPeers x =
      prop_shrink_valid PublicRootPeers.invariant x
 .&&. prop_shrink_nonequal x

prop_fromMapAndSet :: Map PeerAddr PeerAdvertise
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Bool
prop_fromMapAndSet pp bsp lp = PublicRootPeers.invariant
                             . PublicRootPeers.fromMapAndSet pp bsp lp

prop_fromToMapAndSet :: PublicRootPeers PeerAddr -> Bool
prop_fromToMapAndSet prp@(PublicRootPeers pp bsp lp blp) =
  PublicRootPeers.fromMapAndSet pp bsp lp blp == prp

prop_fromMapAndSet' :: Map PeerAddr PeerAdvertise
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Set PeerAddr
                   -> Bool
prop_fromMapAndSet' pp bsp lp blp =
  -- Make sets disjoint
  let pp'  = Map.withoutKeys pp (bsp <> lp <> blp)
      bsp' = Set.difference bsp (lp <> blp)
      lp'  = Set.difference lp blp
      prp = PublicRootPeers.fromMapAndSet pp' bsp' lp' blp
   in  PublicRootPeers.getPublicConfigPeers prp == pp'
    && PublicRootPeers.getBootstrapPeers prp    == bsp'
    && PublicRootPeers.getLedgerPeers prp       == lp'
    && PublicRootPeers.getBigLedgerPeers prp    == blp

prop_merge :: PublicRootPeers PeerAddr -> PublicRootPeers PeerAddr -> Bool
prop_merge prp = PublicRootPeers.invariant
               . (<> prp)

prop_insertPublicConfigPeer :: PublicRootPeers PeerAddr
                            -> PeerAddr
                            -> PeerAdvertise
                            -> Bool
prop_insertPublicConfigPeer prp p pa =
  PublicRootPeers.invariant (PublicRootPeers.insertPublicConfigPeer p pa prp)

prop_insertBootstrapPeer :: PublicRootPeers PeerAddr
                         -> PeerAddr
                         -> Bool
prop_insertBootstrapPeer prp p =
  PublicRootPeers.invariant (PublicRootPeers.insertBootstrapPeer p prp)

prop_insertLedgerPeer :: PublicRootPeers PeerAddr
                      -> PeerAddr
                      -> Bool
prop_insertLedgerPeer prp p =
  PublicRootPeers.invariant (PublicRootPeers.insertLedgerPeer p prp)

prop_insertBigLedgerPeer :: PublicRootPeers PeerAddr
                         -> PeerAddr
                         -> Bool
prop_insertBigLedgerPeer prp p =
  PublicRootPeers.invariant (PublicRootPeers.insertBigLedgerPeer p prp)

prop_fromPeers :: PeerAddr
               -> PeerAdvertise
               -> Bool
prop_fromPeers peer peerAdvertise =
  let insertPP  = PublicRootPeers.insertPublicConfigPeer peer peerAdvertise PublicRootPeers.empty
      insertBSP = PublicRootPeers.insertBootstrapPeer peer PublicRootPeers.empty
      insertLP  = PublicRootPeers.insertLedgerPeer peer PublicRootPeers.empty
      insertBLP = PublicRootPeers.insertBigLedgerPeer peer PublicRootPeers.empty
   in    PublicRootPeers.getPublicConfigPeers insertPP == Map.singleton peer peerAdvertise
      && PublicRootPeers.getBootstrapPeers insertBSP   == Set.singleton peer
      && PublicRootPeers.getLedgerPeers insertLP       == Set.singleton peer
      && PublicRootPeers.getBigLedgerPeers insertBLP   == Set.singleton peer
