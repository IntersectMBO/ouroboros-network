{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Ouroboros.Network.PeerSelection.LocalRootPeers (
  arbitraryLocalRootPeers,
  toGroups',
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
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "LocalRootPeers"
  [ testProperty "arbitrary"    prop_arbitrary_LocalRootPeers
  , testProperty "fromGroups"   prop_fromToGroups
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


arbitraryLocalRootPeers :: Set PeerAddr -> Gen (LocalRootPeers PeerAddr)
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


instance Arbitrary (LocalRootPeers PeerAddr) where

    arbitrary = do
        peeraddrs <- Set.map (PeerAddr . getNonNegative)
                 <$> scale (`div` 4) arbitrary
        arbitraryLocalRootPeers peeraddrs

    shrink lrps =
        map LocalRootPeers.fromGroups (shrink (toGroups' lrps))

toGroups' :: Ord peeraddr
          => LocalRootPeers peeraddr
          -> [(Int, Map peeraddr PeerAdvertise)]
toGroups' lrps =
    [ (t, Map.fromSet (m Map.!) g)
    | let m = LocalRootPeers.toMap lrps
    , (t, g) <- LocalRootPeers.toGroups lrps ]

restrictKeys :: Ord peeraddr
             => LocalRootPeers peeraddr
             -> Set peeraddr
             -> LocalRootPeers peeraddr
restrictKeys lrps ks =
    LocalRootPeers.fromGroups
  . map (\(t,g) -> (t, Map.restrictKeys g ks))
  . toGroups'
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
    sizeGroups = map (show . Set.size . snd) (LocalRootPeers.toGroups lrps)
    targets    = [ case () of
                    _ | t == 0          -> "none"
                      | t == Set.size g -> "all"
                      | otherwise       -> "some"
                 | (t, g) <- LocalRootPeers.toGroups lrps ]


prop_shrink_LocalRootPeers :: LocalRootPeers PeerAddr -> Bool
prop_shrink_LocalRootPeers =
    all LocalRootPeers.invariant . shrink


prop_fromToGroups :: LocalRootPeers PeerAddr -> Bool
prop_fromToGroups lrps =
    LocalRootPeers.fromGroups (toGroups' lrps) == lrps

