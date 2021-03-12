module Test.Ouroboros.Storage.LedgerDB.DiskPolicy (tests) where

import           Data.Function ((&))
import           Data.Word
import           Data.Time.Clock (DiffTime, secondsToDiffTime)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam(..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (DiskPolicy(..), defaultDiskPolicy, RequestedSnapshotInterval(..))
import           Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary ()

tests :: TestTree
tests = testGroup "DiskPolicy/defaultDiskPolicy" [
      testGroup "onDiskNumSnapshots" [
          testProperty "should always be equal to 2" prop_onDiskNumSnapshots
        ]
    , testGroup "onDiskShouldTakeSnapshot" [
          testGroup "haven't take a snapshot yet" [
            testProperty
              "should take snapshot if it processed at least k blocks"
              prop_shouldSnapshot_case1
        ]
        , testGroup "have previously taken a snapshot" [
              testProperty
                "should take snapshot if time since last is greater then explicitly requested interval"
                prop_shouldSnapshot_case2
            , testProperty
                "should take snapshot if time since last is greater then 2 * k if snapshot interval is set to default"
                prop_shouldSnapshot_case3
        ]
      ]
    ]

prop_shouldSnapshot_case1 :: Word64 -> SecurityParam -> RequestedSnapshotInterval -> Property
prop_shouldSnapshot_case1 blocksSinceLast securityParam@(SecurityParam k) requestedInterval =
  shouldSnapshot blocksSinceLast === (blocksSinceLast >= k)
  where
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
    shouldSnapshot = (onDiskShouldTakeSnapshot diskPolicy) Nothing

prop_shouldSnapshot_case2 :: DiffTime -> Word64 -> SecurityParam -> RequestedSnapshotInterval -> Property
prop_shouldSnapshot_case2 _ _ _ DefaultSnapshotInterval = discard
prop_shouldSnapshot_case2 timeSinceLast blocksSinceLast securityParam requestedInterval@(RequestedSnapshotInterval interval) =
  shouldSnapshot === (timeSinceLast >= interval)
  where
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
    shouldSnapshot = (onDiskShouldTakeSnapshot diskPolicy) (Just timeSinceLast) blocksSinceLast

prop_shouldSnapshot_case3 :: DiffTime -> Word64 -> SecurityParam -> RequestedSnapshotInterval -> Property
prop_shouldSnapshot_case3 _ _ _ (RequestedSnapshotInterval _) = discard
prop_shouldSnapshot_case3 timeSinceLast blocksSinceLast securityParam@(SecurityParam k) requestedInterval@DefaultSnapshotInterval =
  let
    kTimes2 = secondsToDiffTime $ fromIntegral $ k * 2
  in
    shouldSnapshot === (timeSinceLast >= kTimes2)
  where
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
    shouldSnapshot = (onDiskShouldTakeSnapshot diskPolicy) (Just timeSinceLast) blocksSinceLast

prop_onDiskNumSnapshots :: SecurityParam -> RequestedSnapshotInterval -> Property
prop_onDiskNumSnapshots securityParam requestedInterval =
  (diskPolicy & onDiskNumSnapshots) === 2
  where
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
