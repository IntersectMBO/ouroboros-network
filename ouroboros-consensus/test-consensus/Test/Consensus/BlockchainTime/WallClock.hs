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

import           Control.Exception (SomeException, fromException)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Tracer
import           Data.Fixed
import           Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import           Test.QuickCheck hiding (Fixed)
import           Test.Tasty hiding (after)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding (Fixed)

import           Cardano.Prelude (AllowThunk (..), NoUnexpectedThunks)
import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IOSim

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Range

tests :: TestTree
tests = testGroup "WallClock" [
      testProperty "delayNextSlot"     prop_delayNextSlot
    , testProperty "delayClockShift"   prop_delayClockShift
    , testProperty "delayNoClockShift" prop_delayNoClockShift
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

{-------------------------------------------------------------------------------
  Test delay using mock time
-------------------------------------------------------------------------------}

-- | Schedule defines the system time as offsets (in seconds) from the start
--
-- We limit the resolution of the offsets to 0.1 seconds to make the tests
-- easier to interpret and shrink (slot length is set to 1 seconds).
newtype Schedule = Schedule { getSchedule :: [Fixed E1] }
  deriving stock (Show)
  deriving NoUnexpectedThunks via AllowThunk Schedule

-- | Translate an offset in the schedule to a slot
--
-- Assumes slot length is 1.
offsetToSlot :: Fixed E1 -> SlotNo
offsetToSlot = SlotNo . floor

-- | Does a schedule ever go back?
--
-- Used for labelling.
scheduleGoesBack :: Schedule -> Bool
scheduleGoesBack (Schedule [])     = False
scheduleGoesBack (Schedule (t:ts)) = go t ts
  where
    go :: Ord a => a -> [a] -> Bool
    go _ []     = False
    go x (y:ys) = y < x || go y ys

-- | How often do two subsequent time entries fall into the same slot?
--
-- Used for labelling.
scheduleCountSkips :: Schedule -> Int
scheduleCountSkips (Schedule [])     = 0
scheduleCountSkips (Schedule (t:ts)) = go t ts
  where
    go :: Fixed E1 -> [Fixed E1] -> Int
    go _ []     = 0
    go x (y:ys) = (if offsetToSlot x == offsetToSlot y then 1 else 0) + go y ys

-- | Predict the outcome of a given schedule
--
-- Returns the set of slot numbers that 'BlockchainTime' should report or,
-- if time moved backwards, the @(before, after)@ slot pair where @after@ is
-- (strictly) less than @before@.
--
-- NOTE: Assumes the slot length is 1 for these sets.
model :: Int -> Schedule -> Either (SlotNo, SlotNo) [SlotNo]
model = \need (Schedule s) -> runExcept $ go need s (SlotNo 0)
  where
    go :: Int        -- How many slots do we still need to collect?
       -> [Fixed E1] -- Remaining schedule
       -> SlotNo     -- Current slot
       -> Except (SlotNo, SlotNo) [SlotNo]

    -- No more slots required
    go 0 _ _ =
        return []

    -- If we don't override the delays, everything just works as expected
    go need [] now =
        return [SlotNo (unSlotNo now + n) | n <- take need [0 ..]]

    go need (s:ss) now
      -- Time didn't actually move according to the schedule, 'BlockchainTime'
      -- should wait until it does.
      | now' == now = go need ss now

      -- If time did move forward, 'BlockchainTime' should report the next slot
      -- (which might not be the successor of the previous)
      | now' >  now = (now :) <$> go (need - 1) ss now'

      -- If time went backwards, we should see an exception
      | otherwise   = throwError (now, now')
      where
        now' :: SlotNo
        now' = offsetToSlot s

instance Arbitrary Schedule where
  arbitrary =
     -- We only collect 100 samples. Generate a few more, potentially, but also
     -- often generate fewer (which would give us the default behaviour).
      Schedule <$> (go 0 =<< choose (0, 110))
    where
      go :: Fixed E1 -> Int -> Gen [Fixed E1]
      go _   0 = return []
      go now n = do
          now' <- frequency [
              -- If time goes back too often, most runs end in an exception
              (100, (\delta -> now + fixedFromDeci delta) <$> choose (0, 30))

              -- Occassionally just pick an entirely random time
            , (1, fixedFromDeci <$> choose (0, 100))
            ]
          (now':) <$> go now' (n - 1)

      fixedFromDeci :: Integer -> Fixed E1
      fixedFromDeci = MkFixed

  shrink (Schedule s) = Schedule <$> shrinkList shrinkOffset s
    where
      shrinkOffset :: Fixed E1 -> [Fixed E1]
      shrinkOffset (MkFixed o) = MkFixed <$> shrink o

prop_delayClockShift :: Schedule -> Property
prop_delayClockShift schedule =
    tabulate "schedule length"    [show $ range (length (getSchedule schedule))] $
    tabulate "schedule goes back" [show $ scheduleGoesBack schedule]             $
    tabulate "schedule skips"     [show $ range (scheduleCountSkips schedule)]   $
    case model numSlots schedule of
      Left (before, after) ->
        case testResult of
          Left (FailureException e) ->
            checkException before after e
          Left e ->
            counterexample ("Unexpected simulator failure " ++ show e) $
            property False
          Right slots' ->
            counterexample ("Expected exception but got " ++ show slots') $
            property False

      Right slots ->
        case testResult of
          Left e ->
            counterexample ("Expected normal termination, but got " ++ show e) $
            property False
          Right slots' ->
            slots' === slots
  where
    numSlots :: Int
    numSlots = 100

    testResult :: Either Failure [SlotNo]
    testResult = overrideDelay dawnOfTime schedule $
        testOverrideDelay (SystemStart dawnOfTime) (SlotLength 1) numSlots

    checkException :: SlotNo -> SlotNo -> SomeException -> Property
    checkException before after e
      | Just (ExceptionInLinkedThread _ e') <- fromException e =
          checkException before after e'
      | Just (SystemClockMovedBack _ before' after') <- fromException e =
          counterexample ("Got expected exception " ++ show e) $
          conjoin [
              before' === before
            , after'  === after
            ]
      | otherwise =
          counterexample ("Unexpected exception: " ++ show e) $
            property False

-- | Just as a sanity check, verify that this works in IO
prop_delayNoClockShift :: Property
prop_delayNoClockShift =
    withMaxSuccess 1 $ ioProperty $ do
      now   <- getCurrentTime
      slots <- originalDelay $
                 testOverrideDelay (SystemStart now) (SlotLength 0.1) 5
      assertEqual "slots" slots [SlotNo n | n <- [0..4]]

testOverrideDelay :: forall m. (IOLike m, MonadDelay (OverrideDelay m))
                  => SystemStart
                  -> SlotLength
                  -> Int  -- ^ Number of slots to collect
                  -> OverrideDelay m [SlotNo]
testOverrideDelay systemStart slotLength numSlots = do
    result <- withRegistry $ \registry -> do
      time <- realBlockchainTime
                registry
                nullTracer
                systemStart
                (focusSlotLengths $ singletonSlotLengths slotLength)
      slotsVar <- uncheckedNewTVarM []
      cancelCollection <-
        onKnownSlotChange registry time "testOverrideDelay" $ \slotNo ->
          atomically $ modifyTVar slotsVar (slotNo :)
      -- Wait to collect the required number of slots
      slots <- atomically $ do
        slots <- readTVar slotsVar
        when (length slots < numSlots) $ retry
        return slots
      cancelCollection
      return $ reverse slots
    return result

dawnOfTime :: UTCTime
dawnOfTime = Time.UTCTime (Time.ModifiedJulianDay 0) 0

{-------------------------------------------------------------------------------
  Test-programmable time
-------------------------------------------------------------------------------}

-- | IO wrapper where we can program the effect of 'threadDelay'
newtype OverrideDelay m a = OverrideDelay {
      unOverrideDelay :: ReaderT (StrictTVar m Schedule) m a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadEventlog
           , MonadThrow
           , MonadCatch
           , MonadMask
           , MonadSTM
           , MonadTime
           , MonadThread
           , MonadFork
           , MonadAsync
           , MonadST
           )

deriving via AllowThunk (OverrideDelay s a)
         instance NoUnexpectedThunks (OverrideDelay s a)

deriving via AllowThunk (StrictTVar (OverrideDelay s) a)
         instance NoUnexpectedThunks (StrictTVar (OverrideDelay s) a)

deriving via AllowThunk (StrictMVar (OverrideDelay s) a)
         instance NoUnexpectedThunks (StrictMVar (OverrideDelay s) a)

instance MonadDelay (OverrideDelay (SimM s)) where
  threadDelay d = OverrideDelay $ ReaderT $ \schedule -> do
      -- Do the original delay. This is important, because otherwise this
      -- turns into a busy loop in the simulator
      threadDelay d
      -- However, the time /after/ the delay will be determined by the
      -- schedule (unless it is empty, in which case the threadDelay behaves
      -- as normal).
      mOverride <- atomically $ updateTVar schedule nextDelay
      case mOverride of
        Nothing -> return ()
        Just t  -> setCurrentTime t
    where
      nextDelay :: Schedule -> (Schedule, Maybe UTCTime)
      nextDelay = \case
          Schedule []     -> (Schedule [], Nothing)
          Schedule (t:ts) -> (Schedule ts, Just $ offsetToTime t)

      offsetToTime :: Fixed E1 -> UTCTime
      offsetToTime t = Time.addUTCTime (realToFrac t) dawnOfTime

-- | The IO instance just uses the default delay
instance MonadDelay (OverrideDelay IO) where
  threadDelay d = OverrideDelay $ ReaderT $ \_schedule -> threadDelay d

instance (IOLike m, MonadDelay (OverrideDelay m)) => IOLike (OverrideDelay m)

overrideDelay :: UTCTime
              -> Schedule
              -> (forall s. OverrideDelay (SimM s) a)
              -> Either Failure a
overrideDelay start schedule ma = runSim $ do
    setCurrentTime start
    scheduleVar <- newTVarM schedule
    runReaderT (unOverrideDelay ma) scheduleVar

originalDelay :: OverrideDelay IO a -> IO a
originalDelay ma = runReaderT (unOverrideDelay ma) (error "schedule unused")
