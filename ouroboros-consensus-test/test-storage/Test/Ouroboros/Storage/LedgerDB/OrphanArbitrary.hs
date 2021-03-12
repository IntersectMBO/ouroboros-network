{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary where

import           Test.QuickCheck
import           Data.Time.Clock (DiffTime, secondsToDiffTime, diffTimeToPicoseconds)
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam(..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy (RequestedSnapshotInterval(..))

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (0, 6)
  shrink (SecurityParam k) = SecurityParam <$> shrink k

instance Arbitrary RequestedSnapshotInterval where
  arbitrary =
    oneof [ requestedSnapshotInterval
          , defaultSnapshotInterval
          ]
  shrink DefaultSnapshotInterval = []
  shrink (RequestedSnapshotInterval v) = RequestedSnapshotInterval <$> shrink v

requestedSnapshotInterval :: Gen RequestedSnapshotInterval
requestedSnapshotInterval =
  RequestedSnapshotInterval <$> arbitrary

defaultSnapshotInterval :: Gen RequestedSnapshotInterval
defaultSnapshotInterval = pure DefaultSnapshotInterval

instance Arbitrary DiffTime where
  arbitrary = fromIntegral @Int <$> arbitrary
  shrink dt = secondsToDiffTime <$> shrink (diffTimeToPicoseconds dt)
