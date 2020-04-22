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
    , dawnOfTime
    ) where

import           Data.Time
import           Data.Word (Word64)
import           Test.QuickCheck hiding (Fixed (..))

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (ClockSkew (..))
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Util.Random (Seed (..))

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
                     (ChunkNo (..), ChunkSize (..), RelativeSlot (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout

import           Test.Util.Time

minNumCoreNodes :: Word64
minNumCoreNodes = 2

minNumSlots :: Word64
minNumSlots = 1

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

  -- Try to shrink the slot length to just "1", for tests where the slot length
  -- really doesn't matter very much
  shrink (SlotLength n) = if n /= 1 then [SlotLength 1] else []

deriving via UTCTime         instance Arbitrary SystemStart
deriving via Positive Word64 instance Arbitrary SlotNo
deriving via Word64          instance Arbitrary EpochNo

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
  arbitrary = ClockSkew <$> choose (0, 5)
  shrink (ClockSkew n) = [ ClockSkew n' | n' <- shrink n ]

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
  SlotLengths
-------------------------------------------------------------------------------}

instance Arbitrary SlotLengths where
  arbitrary = slotLengthsFromList <$> arbitrary <*> arbitrary
  shrink (SlotLengths l ls) = concat [
        -- Shrink the slot length in this segment
        [ SlotLengths l' ls
        | l' <- shrink l
        ]

        -- Shrink the tail
      , [ SlotLengths l ls'
        | ls' <- shrink ls
        ]

        -- Drop the head
      , [ ls'
        | Just (_, ls') <- [ls]
        ]
      ]

instance Arbitrary SegmentLength where
  -- No point choosing very large segments (not interested in testing overflow)
  -- Segments of length 0 make no sense (important for 'tickSlotLengths').
  arbitrary = SegmentLength <$> choose (1, 1000)
  shrink (SegmentLength l) = SegmentLength <$> shrink l


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

-- | Dawn of time
--
-- Everybody knows nothing happened before 2000-01-01 00:00:00
dawnOfTime :: UTCTime
dawnOfTime = UTCTime (fromGregorian 2000 01 01) 0
