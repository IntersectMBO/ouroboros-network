module Test.Ouroboros.Storage.LedgerDB.DiskPolicy (tests) where

import           Data.Function ((&))

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
    ]

prop_onDiskNumSnapshots :: SecurityParam -> RequestedSnapshotInterval -> Property
prop_onDiskNumSnapshots securityParam requestedInterval =
  (diskPolicy & onDiskNumSnapshots) === 2
  where
    diskPolicy = defaultDiskPolicy securityParam requestedInterval
