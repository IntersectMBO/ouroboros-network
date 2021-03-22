module Test.Ouroboros.Storage.LedgerDB.DiskPolicy (tests) where

import           Data.Function ((&))
import           Data.Word
import           Data.Time.Clock (DiffTime, secondsToDiffTime)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam(..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (DiskPolicy(..), defaultDiskPolicy, SnapshotInterval(..))
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

prop_shouldSnapshot_case1 :: Word64 -> SecurityParam -> SnapshotInterval -> Property
prop_shouldSnapshot_case1 blocksSinceLast securityParam@(SecurityParam k) requestedInterval = do
  -- given
  let
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
  -- when
    shouldSnapshot = (onDiskShouldTakeSnapshot diskPolicy) Nothing blocksSinceLast
  -- then
  shouldSnapshot === (blocksSinceLast >= k)

prop_shouldSnapshot_case2 :: DiffTime -> Word64 -> SecurityParam -> SnapshotInterval -> Property
prop_shouldSnapshot_case2 _ _ _ DefaultSnapshotInterval = discard
prop_shouldSnapshot_case2 timeSinceLast blocksSinceLast securityParam requestedInterval@(RequestedSnapshotInterval interval) = do
  -- given
  let
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
  -- when
    shouldSnapshot = (onDiskShouldTakeSnapshot diskPolicy) (Just timeSinceLast) blocksSinceLast
  -- then
  shouldSnapshot === (timeSinceLast >= interval)

prop_shouldSnapshot_case3 :: DiffTime -> Word64 -> SecurityParam -> SnapshotInterval -> Property
prop_shouldSnapshot_case3 _ _ _ (RequestedSnapshotInterval _) = discard
prop_shouldSnapshot_case3 timeSinceLast blocksSinceLast securityParam@(SecurityParam k) requestedInterval@DefaultSnapshotInterval = do
  -- given
  let
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
  -- when
    shouldSnapshot = (onDiskShouldTakeSnapshot diskPolicy) (Just timeSinceLast) blocksSinceLast
  -- then
    kTimes2 = secondsToDiffTime $ fromIntegral $ k * 2
  shouldSnapshot === (timeSinceLast >= kTimes2)


prop_onDiskNumSnapshots :: SecurityParam -> SnapshotInterval -> Property
prop_onDiskNumSnapshots securityParam requestedInterval =
  (diskPolicy & onDiskNumSnapshots) === 2
  where
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
