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

module Test.Consensus.BlockchainTime.Simple (
    tests
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Tracer
import           Data.Fixed
import qualified Data.Time.Clock as Time
import           NoThunks.Class (AllowThunk (..))
import           Test.QuickCheck hiding (Fixed)
import           Test.Tasty hiding (after)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding (Fixed)

import           Control.Monad.Class.MonadTime
import           Control.Monad.IOSim

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (withWatcher)
import           Ouroboros.Consensus.Util.Time

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Range
import           Test.Util.Time

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
      tdioSlotLen <- slotLengthFromMillisec <$> choose (100, 1_000)
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
        let time = defaultSystemTime tdioStart nullTracer
        atStart    <- fst <$> getWallClockSlot  time tdioSlotLen
        nextSlot   <-         waitUntilNextSlot time tdioSlotLen maxClockRewind atStart
        afterDelay <- fst <$> getWallClockSlot  time tdioSlotLen
        assertEqual "atStart + 1" (atStart + 1) afterDelay
        assertEqual "nextSlot"    nextSlot      afterDelay

    pickSystemStart :: IO SystemStart
    pickSystemStart = pick <$> getCurrentTime
      where
        pick :: UTCTime -> SystemStart
        pick = SystemStart . Time.addUTCTime (negate tdioStart')

    -- Will only be needed when the system clock rolls back during the execution
    -- of this test, which is rather unlikely.
    maxClockRewind :: NominalDiffTime
    maxClockRewind = secondsToNominalDiffTime 20

{-------------------------------------------------------------------------------
  Test delay using mock time
-------------------------------------------------------------------------------}

-- | Schedule defines the system time as offsets (in seconds) from the start
--
-- We limit the resolution of the offsets to 0.1 seconds to make the tests
-- easier to interpret and shrink (slot length is set to 1 seconds). We allow
-- the clock to go back at most 2 seconds.
newtype Schedule = Schedule { getSchedule :: [Fixed E1] }
  deriving stock (Show)
  deriving NoThunks via AllowThunk Schedule

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
-- more than the @maxClockRewind@ less than @before@.
--
-- NOTE: Assumes the slot length is 1 and max clock rewind is 2 for these sets.
model :: Int -> Schedule -> Either (SlotNo, SlotNo) [SlotNo]
model = \need (Schedule ss) ->
    -- Establish the invariant that the 'Schedule' is never empty
    let ss' = case ss of
          [] -> [0.0]
          _  -> ss
    in runExcept $
      (SlotNo 0 :) <$> go (need - 1) (Schedule ss') (0.0, SlotNo 0)
  where
    -- | This let's us treat the schedule as an infinite stream of offsets.
    --
    -- INVARIANT: 'Schedule' is never empty
    --
    -- When there is no offset after the current one in the schedule, create
    -- one, exactly one slot length after the current one.
    advanceSchedule :: Schedule -> (Fixed E1, Schedule)
    advanceSchedule (Schedule ss) =
        case ss of
          []    -> error "invariant broken: empty schedule"
          [s]   -> (s,   Schedule [s + 1.0])
          s:ss' -> (s,   Schedule ss')

    go ::
         Int
      -> Schedule
      -> (Fixed E1, SlotNo)
      -> Except (SlotNo, SlotNo) [SlotNo]
    go n ss (prevOffset, prevSlot)
        | n <= 0
        = return []
        | nextSlot == prevSlot
        = go n ss' (offset, nextSlot)
        | nextSlot >  prevSlot
        = (nextSlot :) <$> go (n - 1) ss' (offset, nextSlot)
        -- If time moved back, but not more than 2s, we don't throw an exception
        | prevOffset - offset <= 2
        = go n ss' (offset, prevSlot)
        -- If time moved back too much, we should see an exception
        | otherwise
        = throwError (prevSlot, nextSlot)
      where
        (offset, ss') = advanceSchedule ss
        nextSlot      = offsetToSlot offset

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

              -- Go back a bit without exceeding the max clock rewind
            , (10, (\delta -> max 0 (now - fixedFromDeci delta)) <$> choose (0, 2))

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
        testOverrideDelay
          (SystemStart dawnOfTime)
          (slotLengthFromSec 1)
          (secondsToNominalDiffTime 2)
          numSlots

    checkException :: SlotNo -> SlotNo -> SomeException -> Property
    checkException before after e
      | Just (ExceptionInLinkedThread _ e') <- fromException e =
          checkException before after e'
      | Just (SystemClockMovedBack before' after') <- fromException e =
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
                 testOverrideDelay
                   (SystemStart now)
                   (slotLengthFromMillisec 100)
                   (secondsToNominalDiffTime 20)
                   5
      assertEqual "slots" slots [SlotNo n | n <- [0..4]]

testOverrideDelay :: forall m. (IOLike m, MonadTime m, MonadDelay (OverrideDelay m))
                  => SystemStart
                  -> SlotLength
                  -> NominalDiffTime
                  -> Int  -- ^ Number of slots to collect
                  -> OverrideDelay m [SlotNo]
testOverrideDelay systemStart slotLength maxClockRewind numSlots = do
    bracketWithPrivateRegistry
      (\registry -> simpleBlockchainTime
                      registry
                      (defaultSystemTime systemStart nullTracer)
                      slotLength
                      maxClockRewind)
      (\_btime   -> pure ())
      $ \btime   -> do
      slotsVar <- uncheckedNewTVarM []
      withWatcher
        "testOverrideDelay"
        ( knownSlotWatcher btime $ \slotNo -> do
            atomically $ modifyTVar slotsVar (slotNo :)
        ) $ do
        -- Wait to collect the required number of slots
        atomically $ do
          slots <- readTVar slotsVar
          when (length slots < numSlots) $ retry
          return $ reverse slots

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
           , MonadMonotonicTime
           , MonadTime
           , MonadThread
           , MonadFork
           , MonadAsync
           , MonadST
           , MonadEvaluate
           )

deriving via AllowThunk (OverrideDelay s a)
         instance NoThunks (OverrideDelay s a)

deriving via AllowThunk (StrictTVar (OverrideDelay s) a)
         instance NoThunks (StrictTVar (OverrideDelay s) a)

deriving via AllowThunk (StrictMVar (OverrideDelay s) a)
         instance NoThunks (StrictMVar (OverrideDelay s) a)

instance MonadDelay (OverrideDelay (IOSim s)) where
  threadDelay d = OverrideDelay $ ReaderT $ \schedule -> do
      -- Do the original delay. This is important, because otherwise this
      -- turns into a busy loop in the simulator
      threadDelay d
      -- However, the time /after/ the delay will be determined by the
      -- schedule (unless it is empty, in which case the threadDelay behaves
      -- as normal).
      mOverride <- atomically $ stateTVar schedule nextDelay
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

instance (IOLike m, MonadDelay (OverrideDelay m)) => IOLike (OverrideDelay m) where
  forgetSignKeyKES = OverrideDelay . lift . forgetSignKeyKES

overrideDelay :: UTCTime
              -> Schedule
              -> (forall s. OverrideDelay (IOSim s) a)
              -> Either Failure a
overrideDelay start schedule ma = runSim $ do
    setCurrentTime start
    scheduleVar <- newTVarIO schedule
    runReaderT (unOverrideDelay ma) scheduleVar

originalDelay :: OverrideDelay IO a -> IO a
originalDelay ma = runReaderT (unOverrideDelay ma) (error "schedule unused")
