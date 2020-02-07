{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Consensus.BlockchainTime.WallClock (tests) where

import qualified Data.Time.Clock as Time
import           Test.QuickCheck hiding (Fixed)
import           Test.Tasty hiding (after)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding (Fixed)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Util.IOLike

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testGroup "WallClock" [
      testProperty "delayNextSlot"     prop_delayNextSlot
    ]

{-------------------------------------------------------------------------------
  Test for IO
-------------------------------------------------------------------------------}

-- | Parameters for testing 'timeUntilNextSlot' in some actual IO code
data TestDelayIO = TestDelayIO {
      -- | System start
      --
      -- Since we don't actually " start " the system in any way, we specify
      -- this as an offset _before_ the start of the test.
      tdioStart'  :: Time.NominalDiffTime

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
-- We override the maximum number of tests since there are slow.
--
-- NOTE: If the system is under very heavy load, this test /could/ fail:
-- the slot number after the delay could be later than the one we expect.
-- We don't relax the test because this is highly unlikely, and the stronger
-- test gives us a more useful property.
prop_delayNextSlot :: TestDelayIO -> Property
prop_delayNextSlot TestDelayIO{..} =
    withMaxSuccess 10 $ ioProperty test
  where
    test :: IO ()
    test = do
        tdioStart  <- pickSystemStart
        lsVar      <- mkLsVar
        atStart    <- fst <$> getWallClockSlot  tdioStart lsVar
        nextSlot   <-         waitUntilNextSlot tdioStart lsVar atStart
        afterDelay <- fst <$> getWallClockSlot  tdioStart lsVar
        assertEqual "atStart + 1" (atStart + 1) afterDelay
        assertEqual "nextSlot"    nextSlot      afterDelay

    mkLsVar :: IO (StrictTVar IO FocusedSlotLengths)
    mkLsVar = newTVarM $ focusSlotLengths (singletonSlotLengths tdioSlotLen)

    pickSystemStart :: IO SystemStart
    pickSystemStart = pick <$> getCurrentTime
      where
        pick :: UTCTime -> SystemStart
        pick = SystemStart . Time.addUTCTime (negate tdioStart')
