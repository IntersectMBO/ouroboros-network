{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.Arbitrary
    ( genLimitedEpochSize
    , genLimitedSlotNo
    , genSmallEpochNo
    , genSmallSlotNo
    , SmallDiffTime (..)
    ) where

import           Data.Time
import           Data.Word (Word64)
import           Test.QuickCheck hiding (Fixed (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Fragment.InFuture (ClockSkew)
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (ChunkNo (..), ChunkSize (..), RelativeSlot (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout

import           Test.Util.Time

minNumCoreNodes :: Word64
minNumCoreNodes = 2

instance Arbitrary a => Arbitrary (WithOrigin a) where
  arbitrary = oneof [
        pure Origin
      , NotOrigin <$> arbitrary
      ]

  shrink Origin        = []
  shrink (NotOrigin x) = Origin : map NotOrigin (shrink x)

instance Arbitrary NumCoreNodes where
  arbitrary = NumCoreNodes <$> choose (minNumCoreNodes, 5)
  shrink (NumCoreNodes n) = NumCoreNodes <$> (filter (>= minNumCoreNodes) $ shrink n)

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

  -- Try to shrink the slot length to just "1", for tests where the slot length
  -- really doesn't matter very much
  shrink slotLen = if slotLen /= oneSec then [oneSec] else []
    where
      oneSec = slotLengthFromSec 1

deriving via UTCTime         instance Arbitrary SystemStart
deriving via Positive Word64 instance Arbitrary BlockNo

instance Arbitrary RelativeSlot where
  arbitrary = RelativeSlot <$> arbitrary <*> arbitrary <*> arbitrary

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

-- | This picks an 'EpochNo' between 0 and 10000
--
-- We don't pick larger values because we're not interested in testing overflow
-- due to huge epoch numbers and even huger slot numbers.
instance Arbitrary ChunkNo where
  arbitrary = ChunkNo <$> choose (0, 10000)
  shrink    = genericShrink

-- | Picks a 'ChunkSize' between 1 and 100, and randomly choose to enable EBBs
instance Arbitrary ChunkSize where
  arbitrary = ChunkSize <$> arbitrary <*> choose (1, 100)
  shrink    = genericShrink

instance Arbitrary ChunkSlot where
  arbitrary = UnsafeChunkSlot <$> arbitrary <*> arbitrary
  shrink    = genericShrink

instance Arbitrary ClockSkew where
  arbitrary = InFuture.clockSkewInSeconds <$> choose (0, 5)
  shrink skew = concat [
     -- Shrink to some simple values, including 0
     -- (it would be useful to know if a test fails only when having non-zero
     -- clock skew)
       [ skew0 | skew0 < skew ]
     , [ skew1 | skew1 < skew ]
     ]
    where
      skew0, skew1 :: ClockSkew
      skew0 = InFuture.clockSkewInSeconds 0
      skew1 = InFuture.clockSkewInSeconds 1

{-------------------------------------------------------------------------------
  SmallDiffTime
-------------------------------------------------------------------------------}

-- | Wrapper around NominalDiffTime with custom 'Arbitrary' instance
--
-- The default 'Arbitrary' instance for 'NominalDiffTime' isn't very useful:
--
-- * It tends to pick huge values
-- * It tends not to pick integer values
-- * It does not shrink
--
-- Our custom instance
--
-- * Picks values between 0 and (1000 * 20 * 10) seconds:
--   - Maximum segment length: 1000
--   - Maximum slot length: 20 seconds
--   - Maximum number of segments: 10
-- * With a 0.1 second precision
-- * Shrinks
newtype SmallDiffTime = SmallDiffTime NominalDiffTime
  deriving (Show)

instance Arbitrary SmallDiffTime where
  arbitrary = conv <$> choose (0, 1000 * 20 * 10 * 10)
    where
      -- NominalDiffTime conversion functions treat it as seconds
      conv :: Integer -> SmallDiffTime
      conv n = SmallDiffTime $ realToFrac seconds
        where
          seconds :: Double
          seconds = fromInteger n / 10

  -- try to shrink to some small, simple values
  -- (include 1.5 so that we can shrink to a simple, but yet not whole, value)
  shrink (SmallDiffTime d) = map SmallDiffTime $
      filter (< d) [1, 1.5, 2, 3, 100]

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
