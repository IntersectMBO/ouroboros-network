{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Ouroboros.Storage.LedgerDB.OrphanArbitrary where

import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds,
                     secondsToDiffTime)
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (SnapshotInterval (..))
import           Test.QuickCheck

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}
instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (0, 6)
  shrink (SecurityParam k) = SecurityParam <$> shrink k

-------------------------------------------------------------------------------}

instance Arbitrary SnapshotInterval where
  arbitrary =
    frequency [ (9,  requestedSnapshotInterval)
              , (1, defaultSnapshotInterval)
              ]
  shrink DefaultSnapshotInterval = []
  shrink (RequestedSnapshotInterval v) = RequestedSnapshotInterval <$> shrink v

requestedSnapshotInterval :: Gen SnapshotInterval
requestedSnapshotInterval =
  RequestedSnapshotInterval <$> arbitrary

defaultSnapshotInterval :: Gen SnapshotInterval
defaultSnapshotInterval = pure DefaultSnapshotInterval

newtype AlwaysDefaultSnapshotInterval = AlwaysDefaultSnapshotInterval SnapshotInterval
    deriving Show

instance Arbitrary AlwaysDefaultSnapshotInterval where
  arbitrary = AlwaysDefaultSnapshotInterval <$> defaultSnapshotInterval

newtype AlwaysRequestedSnapshotInterval = AlwaysRequestedSnapshotInterval SnapshotInterval
    deriving Show

instance Arbitrary AlwaysRequestedSnapshotInterval where
  arbitrary = AlwaysRequestedSnapshotInterval <$> requestedSnapshotInterval

-------------------------------------------------------------------------------}

instance Arbitrary DiffTime where
  arbitrary = fromIntegral @Int <$> arbitrary
  shrink dt = secondsToDiffTime <$> shrink (diffTimeToPicoseconds dt)
