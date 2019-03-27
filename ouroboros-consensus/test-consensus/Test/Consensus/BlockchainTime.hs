{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module Test.Consensus.BlockchainTime (tests) where

import           Data.Bifunctor
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime

import           Test.Util.Orphans.Arbitrary (genLimitedSlotNo)

tests :: TestTree
tests = testGroup "BlockchainTime" [
      testProperty "startOfSlot"           prop_startOfSlot
    , testProperty "slotAtTime"            prop_slotAtTime
    , testProperty "timeUntilNextSlot"     prop_timeUntilNextSlot
    , testProperty "timeFromStartOfSlot"   prop_timeFromStartOfSlot
    , testProperty "slotAfterTime"         prop_slotAfterTime
    , testProperty "delayNextSlot"         prop_delayNextSlot
    ]

{-------------------------------------------------------------------------------
  Properties of the time utilities
-------------------------------------------------------------------------------}

-- > slotAtTime (startOfSlot slot)) == (slot, 0)
prop_startOfSlot :: SlotLength -> SystemStart -> Property
prop_startOfSlot slotLen start = forAll genLimitedSlotNo $ \ slot ->
    let slotStart = startOfSlot slotLen start slot in
    counterexample ("slotStart: " ++ show slotStart)
        $ slotAtTime slotLen start slotStart === (slot, 0)

-- > now - slotLen < startOfSlot (fst (slotAtTime now)) <= now
-- > 0 <= snd (slotAtTime now) < slotLen
prop_slotAtTime :: SlotLength -> SystemStart -> FixedDiffTime -> Property
prop_slotAtTime slotLen start execTime =
      counterexample ("now:         " ++ show now)
    . counterexample ("currentSlot: " ++ show currentSlot)
    . counterexample ("timeSpent:   " ++ show timeSpent)
    . counterexample ("slotStart:   " ++ show slotStart)
    $    addFixedUTC (negate slotLen') now < startOfSlot slotLen start currentSlot
    .&&. slotStart <= now
    .&&. 0 <= timeSpent
    .&&. timeSpent < getSlotLength slotLen
  where
    now                      = addFixedUTC execTime (getSystemStart start)
    (currentSlot, timeSpent) = slotAtTime slotLen start now
    slotStart                = startOfSlot slotLen start currentSlot
    slotLen'                 = getSlotLength slotLen

-- > 0 < fst (timeUntilNextSlot now) <= slotLen
prop_timeUntilNextSlot :: SlotLength -> SystemStart -> FixedDiffTime -> Property
prop_timeUntilNextSlot slotLen start execTime =
      counterexample ("now:      " ++ show now)
    . counterexample ("timeLeft: " ++ show timeLeft)
    . tabulate "Edge case:" [edgeCase]
    $    0 < timeLeft
    .&&. timeLeft <= getSlotLength slotLen
  where
    now               = addFixedUTC execTime (getSystemStart start)
    (timeLeft, _next) = timeUntilNextSlot slotLen start now

    edgeCase :: String
    edgeCase | timeLeft <= 1                     = "<= 1"
             | timeLeft >= getSlotLength slotLen = ">= slotLen"
             | otherwise                         = "in the middle"

-- > timeUntilNextSlot (startOfSlot slot) == (slotLen, slot + 1)
prop_timeFromStartOfSlot :: SlotLength -> SystemStart -> Property
prop_timeFromStartOfSlot slotLen start = forAll genLimitedSlotNo $ \ slot ->
    let slotStart = startOfSlot slotLen start slot in
    counterexample ("slotStart: " ++ show slotStart)
      $ timeUntilNextSlot slotLen start slotStart ===
        (getSlotLength slotLen, slot + 1)

-- > slotAtTime (now + fst (timeUntilNextSlot now)) == bimap (+1) (const 0) (slotAtTime now)
-- > fst (slotAtTime (now + fst (timeUntilNextSlot now))) == snd (timeUntilNextSlot now)
prop_slotAfterTime :: SlotLength -> SystemStart -> FixedDiffTime -> Property
prop_slotAfterTime slotLen start execTime =
      counterexample ("now:        " ++ show now)
    . counterexample ("timeLeft:   " ++ show timeLeft)
    . counterexample ("afterDelay: " ++ show afterDelay)
    $    slotAtTime slotLen start afterDelay ===
         bimap (+1) (const 0) (slotAtTime slotLen start now)
    .&&. fst (slotAtTime slotLen start afterDelay) === next
  where
    now              = addFixedUTC execTime (getSystemStart start)
    (timeLeft, next) = timeUntilNextSlot slotLen start now
    afterDelay       = addFixedUTC timeLeft now

{-------------------------------------------------------------------------------
  Test for IO
-------------------------------------------------------------------------------}

-- | Parameters for testing 'timeUntilNextSlot' in some actual IO code
data TestDelayIO = TestDelayIO {
      -- | System start
      --
      -- Since we don't actually " start " the system in any way, we specify
      -- this as an offset _before_ the start of the test.
      tdioStart'  :: FixedDiffTime

      -- | SlotNo length
      --
      -- Since this test is run in IO, we will keep the slot length short.
    , tdioSlotLen :: SlotLength
    }
  deriving (Show)

instance Arbitrary TestDelayIO where
  arbitrary = do
      tdioStart'  <- arbitrary
      tdioSlotLen <- slotLengthFromMillisec <$> choose (1, 1_000)
      return TestDelayIO{..}

-- | Just as a sanity check, also run the tests in IO
--
-- We override the maxinum number of tests since there are slow.
prop_delayNextSlot :: TestDelayIO -> Property
prop_delayNextSlot TestDelayIO{..} = withMaxSuccess 10 $ ioProperty $ do
    tdioStart         <- pickSystemStart
    slotAtStartOfTest <- getCurrentSlotIO    tdioSlotLen tdioStart
    nextSlot          <- waitUntilNextSlotIO tdioSlotLen tdioStart
    slotAfterDelay    <- getCurrentSlotIO    tdioSlotLen tdioStart
    assertEqual "slotAtStartOfTest + 1" (slotAtStartOfTest + 1) slotAfterDelay
    assertEqual "nextSlot"              nextSlot                slotAfterDelay
  where
    pickSystemStart :: IO SystemStart
    pickSystemStart = pick <$> getCurrentFixedUTC
      where
        pick :: FixedUTC -> SystemStart
        pick = SystemStart . addFixedUTC (negate tdioStart')
