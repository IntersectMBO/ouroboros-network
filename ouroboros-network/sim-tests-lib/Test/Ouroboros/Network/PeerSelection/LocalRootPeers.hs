{-# LANGUAGE NamedFieldPuns     #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Ouroboros.Network.PeerSelection.LocalRootPeers
  ( arbitraryLocalRootPeers
  , restrictKeys
  , tests
  , LocalRootPeers (..)
  , HotValency (..)
  , WarmValency (..)
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           LocalRootPeers (..), WarmValency (..))
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers

import Ouroboros.Network.PeerSelection.Governor

import Ouroboros.Network.Testing.Utils (ShrinkCarefully, prop_shrink_nonequal,
           prop_shrink_valid, renderRanges)
import Test.Ouroboros.Network.PeerSelection.Instances


import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "LocalRootPeers"
    [ testProperty "arbitrary"    prop_arbitrary_LocalRootPeers
    , testProperty "fromToGroups" prop_fromToGroups
    , testProperty "fromGroups"   prop_fromGroups
    , testProperty "shrink"       prop_shrink_LocalRootPeers
    , testProperty "clampToLimit" prop_clampToLimit
    , testProperty "clampToTrustable" prop_clampToTrustable
    ]
  ]


-- | Check that we can forcibly bring the localRootPeers into compliance
-- with the governor's state invariant, which requires that the local
-- root peers fit within the current targets.
--
-- The precondition on the targets is that they are sane.
-- See sanePeerSelectionTargets.
prop_clampToLimit :: LocalRootPeers PeerAddr -> PeerSelectionTargets -> Property
prop_clampToLimit localRootPeers targets =

    let sizeLimit       = targetNumberOfKnownPeers targets
        localRootPeers' = LocalRootPeers.clampToLimit sizeLimit localRootPeers

     in counterexample ("sizeLimit   = " ++ show sizeLimit) $

        LocalRootPeers.size localRootPeers'
          === min sizeLimit
                 (LocalRootPeers.size localRootPeers)

prop_clampToTrustable :: LocalRootPeers PeerAddr -> Property
prop_clampToTrustable localRootPeers =

    let trustedPeers = LocalRootPeers.keysSet
                     $ LocalRootPeers.clampToTrustable localRootPeers

     in counterexample (show $ Map.restrictKeys (LocalRootPeers.toMap localRootPeers) trustedPeers)
      $ all (`LocalRootPeers.isPeerTrustable` localRootPeers) trustedPeers


arbitraryLocalRootPeers :: Ord peeraddr
                        => Set peeraddr -> Gen (LocalRootPeers peeraddr)
arbitraryLocalRootPeers peeraddrs = do
    -- divide into a few disjoint groups
    ngroups     <- choose (1, 5 :: Int)
    gassignment <- vectorOf (Set.size peeraddrs) (choose (1, ngroups))
    advertise   <- vectorOf (Set.size peeraddrs) arbitrary
    let groups = Map.elems $
                 Map.fromListWith Map.union
                   [ (gn, Map.singleton p a)
                   | (p, gn, a) <- zip3 (Set.toList peeraddrs)
                                         gassignment
                                         advertise
                   ]
    targets <- mapM (\g -> do
                      warmValency <- WarmValency <$> choose (0, Map.size g)
                      hotValency <- HotValency <$> choose (0, getWarmValency warmValency)
                      return (hotValency, warmValency)
                    ) groups

    return (LocalRootPeers.fromGroups (zipWith (\(h, w) g -> (h, w, g))
                                               targets
                                               groups))

instance Arbitrary HotValency where
  arbitrary = HotValency <$> arbitrary

instance Arbitrary WarmValency where
  arbitrary = WarmValency <$> arbitrary

instance (Arbitrary peeraddr, Ord peeraddr) =>
         Arbitrary (LocalRootPeers peeraddr) where
    arbitrary = do
        peeraddrs <- scale (`div` 4) arbitrary
        arbitraryLocalRootPeers peeraddrs

    shrink lrps =
        map LocalRootPeers.fromGroups (shrink (LocalRootPeers.toGroups lrps))

restrictKeys :: Ord peeraddr
             => LocalRootPeers peeraddr
             -> Set peeraddr
             -> LocalRootPeers peeraddr
restrictKeys lrps ks =
    LocalRootPeers.fromGroups
  . map (\(h, w, g) -> (h, w, Map.restrictKeys g ks))
  . LocalRootPeers.toGroups
  $ lrps

prop_arbitrary_LocalRootPeers :: LocalRootPeers PeerAddr -> Property
prop_arbitrary_LocalRootPeers lrps =
    tabulate "total size"    [size] $
    tabulate "num groups"    [numGroups] $
    tabulate "group size"    sizeGroups $
    tabulate "targets"       targets $

    LocalRootPeers.invariant lrps
  where
    thrd (_, _, c) = c
    size       = renderRanges 5 (LocalRootPeers.size lrps)
    numGroups  = show (length (LocalRootPeers.toGroups lrps))
    sizeGroups = map (show . Set.size . thrd) (LocalRootPeers.toGroupSets lrps)
    targets    = [ case () of
                    _ | h == 0                        -> "none active"
                      | w == 0                        -> "none established"
                      | h == HotValency (Set.size g)  -> "all active"
                      | w == WarmValency (Set.size g) -> "all established"
                      | otherwise                     -> "some"
                 | (h, w, g) <- LocalRootPeers.toGroupSets lrps ]


prop_shrink_LocalRootPeers :: ShrinkCarefully (LocalRootPeers PeerAddr) -> Property
prop_shrink_LocalRootPeers x =
      prop_shrink_valid LocalRootPeers.invariant x
 .&&. prop_shrink_nonequal x

prop_fromGroups :: [(HotValency, WarmValency, Map PeerAddr (PeerAdvertise, PeerTrustable))] -> Bool
prop_fromGroups = LocalRootPeers.invariant . LocalRootPeers.fromGroups

prop_fromToGroups :: LocalRootPeers PeerAddr -> Bool
prop_fromToGroups lrps =
    LocalRootPeers.fromGroups (LocalRootPeers.toGroups lrps) == lrps

