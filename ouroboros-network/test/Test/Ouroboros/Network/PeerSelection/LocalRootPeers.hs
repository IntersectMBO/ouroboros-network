{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.LocalRootPeers (
  arbitraryLocalRootPeers,
  restrictKeys,
  tests,
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import qualified Ouroboros.Network.PeerSelection.LocalRootPeers as LocalRootPeers
import           Ouroboros.Network.PeerSelection.LocalRootPeers (LocalRootPeers)

import           Ouroboros.Network.PeerSelection.Governor
import           Ouroboros.Network.PeerSelection.Types

import           Test.Ouroboros.Network.PeerSelection.Instances


import           Test.QuickCheck
import           Test.QuickCheck.Utils
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "LocalRootPeers"
  [ testProperty "arbitrary"    prop_arbitrary_LocalRootPeers
  , testProperty "fromToGroups" prop_fromToGroups
  , testProperty "fromGroups"   prop_fromGroups
  , testProperty "shrink"       prop_shrink_LocalRootPeers
  , testProperty "clampToLimit" prop_clampToLimit
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
    targets <- mapM (\g -> choose (0, Map.size g)) groups

    return (LocalRootPeers.fromGroups (zip targets groups))


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
  . map (\(t,g) -> (t, Map.restrictKeys g ks))
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
    size       = renderRanges 5 (LocalRootPeers.size lrps)
    numGroups  = show (length (LocalRootPeers.toGroups lrps))
    sizeGroups = map (show . Set.size . snd) (LocalRootPeers.toGroupSets lrps)
    targets    = [ case () of
                    _ | t == 0          -> "none"
                      | t == Set.size g -> "all"
                      | otherwise       -> "some"
                 | (t, g) <- LocalRootPeers.toGroupSets lrps ]


prop_shrink_LocalRootPeers :: LocalRootPeers PeerAddr -> Bool
prop_shrink_LocalRootPeers =
    all LocalRootPeers.invariant . shrink

prop_fromGroups :: [(Int, Map PeerAddr PeerAdvertise)] -> Bool
prop_fromGroups = LocalRootPeers.invariant . LocalRootPeers.fromGroups

prop_fromToGroups :: LocalRootPeers PeerAddr -> Bool
prop_fromToGroups lrps =
    LocalRootPeers.fromGroups (LocalRootPeers.toGroups lrps) == lrps

