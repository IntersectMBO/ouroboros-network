{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util.Orphans.Arbitrary
    ( genLimitedEpochSize
    , genLimitedSlotNo
    , genSmallEpochNo
    , genSmallSlotNo
    ) where

import           Data.Time
import           Data.Word (Word64)
import           Test.QuickCheck hiding (Fixed (..))

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import           Ouroboros.Consensus.Mempool.Expiry (ExpirySlotNo (..),
                     ExpiryThreshold (..))
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.Random (Seed (..))

import           Ouroboros.Storage.Common (EpochNo (..), EpochSize (..))
import           Ouroboros.Storage.ImmutableDB.Layout


minNumCoreNodes, minNumSlots :: Int
minNumCoreNodes = 2
minNumSlots     = 1

instance Arbitrary NumCoreNodes where
  arbitrary = NumCoreNodes <$> choose (minNumCoreNodes, 5)
  shrink (NumCoreNodes n) = NumCoreNodes <$> (filter (>= minNumCoreNodes) $ shrink n)

-- TODO: We shouldn't really pick the number of slots independent from k
instance Arbitrary NumSlots where
  arbitrary = NumSlots <$> choose (minNumSlots, 100)
  shrink (NumSlots n) = NumSlots <$> (filter (>= minNumSlots) $ shrink n)

-- | Picks time span between 0 seconds and (roughly) 50 years
instance Arbitrary NominalDiffTime where
  arbitrary = conv <$> choose (0, 50 * daysPerYear * secondsPerDay)
    where
      conv :: Double -> NominalDiffTime
      conv = realToFrac

-- | Picks moment between 'dawnOfTime' and (roughly) 50 years later
--
-- Uses instance for 'NominalDiffTime'
instance Arbitrary UTCTime where
  arbitrary = (`addUTCTime` dawnOfTime) <$> arbitrary

-- | Length between 0.001 and 20 seconds, millisecond granularity
instance Arbitrary SlotLength where
  arbitrary = slotLengthFromMillisec <$> choose (1, 20 * 1_000)

deriving via UTCTime         instance Arbitrary SystemStart
deriving via Positive Word64 instance Arbitrary SlotNo
deriving via Word64          instance Arbitrary EpochNo
deriving via Positive Word64 instance Arbitrary EpochSize
deriving via Word64          instance Arbitrary RelativeSlot

-- | The functions 'slotAtTime' and 'timeUntilNextSlot' suffer from arithmetic
-- overflow for very large values, so generate values that avoid overflow when
-- used in these two functions. The largest value generated is still sufficently
-- large to allow for 5e12 years worth of slots at a slot interval of 20
-- seconds.
genLimitedSlotNo :: Gen SlotNo
genLimitedSlotNo =
    SlotNo <$> arbitrary `suchThat` (< 0x8000000000000000)

-- | Generate a small SlotNo for the state machine tests. The runtime of the
-- StateMachine prop_sequential tests is proportional the the upper bound.
genSmallSlotNo :: Gen SlotNo
genSmallSlotNo =
    SlotNo <$> choose (0, 1000)

-- | The tests for 'CumulEpochSizes' requires that the sum of a list of these
-- values does not overflow.
--
-- An epoch size must be > 0.
genLimitedEpochSize :: Gen EpochSize
genLimitedEpochSize =
    EpochSize <$> choose (1, 100_000)

genSmallEpochNo :: Gen EpochNo
genSmallEpochNo =
    EpochNo <$> choose (0, 10000)

instance Arbitrary EpochSlot where
  arbitrary = EpochSlot <$> arbitrary <*> arbitrary
  shrink    = genericShrink

instance Arbitrary ClockSkew where
  arbitrary = ClockSkew <$> choose (0, 5)
  shrink (ClockSkew n) = [ ClockSkew n' | n' <- shrink n ]

{-------------------------------------------------------------------------------
  Mempool
-------------------------------------------------------------------------------}

instance Arbitrary ExpiryThreshold where
  arbitrary = oneof
    [ pure NoExpiryThreshold
    , ExpiryThreshold . NumSlots . getPositive <$> arbitrary
    ]
  shrink NoExpiryThreshold    = []
  shrink (ExpiryThreshold et) =
    NoExpiryThreshold : [ExpiryThreshold et' | et' <- shrink et]

instance Arbitrary ExpirySlotNo where
  arbitrary = oneof
    [ pure NoExpirySlot
    , ExpirySlotNo <$> arbitrary
    ]
  shrink NoExpirySlot = []
  shrink (ExpirySlotNo esn) =
    NoExpirySlot : [ExpirySlotNo esn' | esn' <- shrink esn]

{-------------------------------------------------------------------------------
  Crypto
-------------------------------------------------------------------------------}

instance Arbitrary Seed where

    arbitrary = do  (\w1 w2 w3 w4 w5 -> Seed (w1, w2, w3, w4, w5))
                <$> gen <*> gen <*> gen <*> gen <*> gen
      where
        gen = arbitraryBoundedIntegral

    shrink = const []

{-------------------------------------------------------------------------------
  Auxiliary: time
-------------------------------------------------------------------------------}

-- | Average number of days per year
--
-- <https://en.wikipedia.org/wiki/Year>
daysPerYear :: Double
daysPerYear = 365.2425

-- | Seconds per day
secondsPerDay :: Double
secondsPerDay = 24 * 60 * 60

-- | Dawn of time
--
-- Everybody knows nothing happened before 2000-01-01 00:00:00
dawnOfTime :: UTCTime
dawnOfTime = UTCTime (fromGregorian 2000 01 01) 0
