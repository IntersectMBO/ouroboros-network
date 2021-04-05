{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
module Test.Ouroboros.Storage.ChainDB.GcSchedule (
    example
  , tests
  ) where

import           Control.Monad (forM)
import           Control.Tracer (nullTracer)
import           Data.Fixed (div')
import           Data.List (foldl', partition, sort)
import           Data.Time.Clock
import           Data.Void (Void)

import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util (lastMaybe, safeMaximum)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB.Impl.Background
                     (GcParams (..), ScheduledGc (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Background as Impl

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Orphans.IOLike ()
import           Test.Util.QuickCheck

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "GcSchedule"
    [ testProperty "queueLength"        prop_queueLength
    , testProperty "overlap"            prop_overlap
    , testProperty "unnecessaryOverlap" prop_unnecessaryOverlap
    , testProperty "model vs impl"      prop_model_vs_impl
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Property 1
--
-- 'queueLength' <= 'gcDelay' `div` 'gcInterval' + @slack@
--
-- Where:
-- * @slack = 1@ when 'gcInterval' divides 'gcDelay'. In this case, the delay
--   divides the interval nicely in different buckets. However, if we're not
--   at the start of a bucket and part of it is in the past, we'll need one
--   extra bucket to compensate, hence 1.
-- * @slack = 2@ in the other cases: in addition to the 1 of the previous
--   case, we must also account for one extra bucket because 'gcInterval'
--   doesn't nicely divide 'gcDelay' into buckets. In other words: we need to
--   round up.
prop_queueLength :: TestSetup -> Property
prop_queueLength TestSetup{..} =
    testDelay >= testInterval ==>
      conjoin
        [ gcSummaryQueueLength `le` (gcDelay `div'` gcInterval) + slack
        | GcStateSummary { gcSummaryQueueLength } <- testTrace
        ]
  where
    GcParams{..} = testGcParams
    slack
      | testDelay `mod` testInterval == 0
      = 1
      | otherwise
      = 2

-- | Property 2:
--
-- 'overlap' < the number of blocks that could arrive 'gcDelay' +
-- 'gcInterval'.
prop_overlap :: TestSetup -> Property
prop_overlap TestSetup{..} =
    conjoin
      [ gcSummaryOverlap `lt` blocksInInterval (gcDelay + gcInterval)
      | GcStateSummary { gcSummaryOverlap } <- testTrace
      ]
  where
    GcParams{..} = testGcParams

-- | Property 3:
--
-- 'unnecessaryOverlap' < the number of blocks that could arrive in
-- 'gcInterval'.
prop_unnecessaryOverlap :: TestSetup -> Property
prop_unnecessaryOverlap TestSetup{..} =
    conjoin
      [ gcSummaryUnnecessary `lt` blocksInInterval gcInterval
      | GcStateSummary { gcSummaryUnnecessary } <- testTrace
      ]
  where
    GcParams{..} = testGcParams

-- TODO the unnecessaryOverlap should at some point go back to 0 after it has
-- increased: test this property

blocksInInterval :: DiffTime -> Int
blocksInInterval interval = round (realToFrac interval :: Double)

-- | Verify that the queue of the real implementation matches the model queue
-- at each point in the trace.
--
-- Moreover, verify that the real implementation will have performed all its
-- garbage collections at the same times as the model implementation.
prop_model_vs_impl :: TestSetup -> Property
prop_model_vs_impl TestSetup {..} = conjoin
    [ counterexample "Expected queue evolution /= actual" $
        map (reverseQueue . gcSummaryQueue) testTrace === gcQueueTrace
    , counterexample "Expected final garbage collections /= actual" $
        testGcGarbageCollections === gcs
    ]
  where
    (gcQueueTrace, gcs) = runGcSchedule testGcParams (genBlocks testNumBlocks)

    -- In the model we store the queue in reverse order, so we have to reverse
    -- it to match the order of the implementation's queue.
    reverseQueue :: GcQueue -> GcQueue
    reverseQueue (GcQueue q) = GcQueue (reverse q)

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

newtype Block = Block Int
  deriving stock   (Show)
  deriving newtype (Condense)

blockArrivalTime :: Block -> Time
blockArrivalTime (Block n) = Time (secondsToDiffTime (fromIntegral n))

blockSlotNo :: Block -> SlotNo
blockSlotNo (Block n) = SlotNo (fromIntegral n)

{-------------------------------------------------------------------------------
  GcQueue, GcBlocks, GcGarbageCollected, GcState
-------------------------------------------------------------------------------}

-- | Queue of scheduled GCs, in reverse order
newtype GcQueue = GcQueue { unGcQueue :: [ScheduledGc] }
  deriving newtype (Eq, Condense)

instance Show GcQueue where
  show = condense

-- | Blocks still to GC, together with the earliest time at which the block
-- could have been GC'ed.
--
-- In no particular order.
--
-- NOTE: in the real implementation, a GC for slot @s@ means removing all
-- blocks with a slot number < @s@ (because of EBBs, which share the slot with
-- the regular block after it). In this test, we ignore this and use <=, so a
-- GC for the slot of the block will remove the block.
newtype GcBlocks = GcBlocks { unGcBlocks :: [(Block, Time)] }
  deriving newtype (Condense)

instance Show GcBlocks where
  show = condense

-- | Garbage collections that have happened. A garbage collection is triggered
-- for a slot number. We remember at which time it happened.
--
-- In no particular order.
--
-- The NOTE of 'GcBlocks' also applies here.
newtype GcGarbageCollections = GcGarbageCollections [(SlotNo, Time)]
  deriving newtype (Eq, Condense, NoThunks)

instance Show GcGarbageCollections where
  show = condense

data GcState = GcState {
      gcQueue              :: GcQueue
    , gcBlocks             :: GcBlocks
    , gcGarbageCollections :: GcGarbageCollections
    }
  deriving (Show)

emptyGcState :: GcState
emptyGcState =
    GcState
      (GcQueue [])
      (GcBlocks [])
      (GcGarbageCollections [])

-- | The length of the queue
queueLength :: GcState -> Int
queueLength = length . unGcQueue . gcQueue

-- | The overlap (number of blocks) between ImmutableDB and VolatileDB
overlap :: GcState -> Int
overlap = length . unGcBlocks . gcBlocks

-- | Number of blocks that could be GC'ed but haven't been
unnecessaryOverlap
  :: Time  -- ^ The current time
  -> GcState
  -> Int
unnecessaryOverlap now =
    length . filter ((<= now) . snd) . unGcBlocks . gcBlocks

-- | Run all garbage collections schedule before or at the given time.
runGc :: Time -> GcState -> GcState
runGc now gcState = GcState {
      gcQueue              = GcQueue gcQueueLater
    , gcBlocks             = case mbHighestGCedSlot of
        Nothing              -> gcBlocks gcState
        Just highestGCedSlot -> GcBlocks $
          filter
            ((> highestGCedSlot) . blockSlotNo . fst)
            (unGcBlocks (gcBlocks gcState))
    , gcGarbageCollections = GcGarbageCollections $
        map toGarbageCollection gcQueueNow <> pastGarbageCollections
    }
  where
    (gcQueueLater, gcQueueNow) =
      partition ((> now) . scheduledGcTime) (unGcQueue (gcQueue gcState))
    mbHighestGCedSlot = safeMaximum $ map scheduledGcSlot gcQueueNow
    GcGarbageCollections pastGarbageCollections =
      gcGarbageCollections gcState

    toGarbageCollection :: ScheduledGc -> (SlotNo, Time)
    toGarbageCollection (ScheduledGc time slot) = (slot, time)

step
  :: GcParams
  -> Block
  -> GcState
  -> GcState
step gcParams block =
    -- Note the two calls to 'runGc': we simulate the behaviour of two threads
    -- (schedule GCs, execute schedule) from this (single-threaded) function.
    --
    -- The first (innermost) 'runGc' is needed to run any outstanding GCs at
    -- @now@. In other words, we run the "execute schedule" thread. Otherwise,
    -- we will see GCs scheduled in the past in the queue when we schedule a
    -- new one.
    --
    -- The second (outermost) 'runGc' is needed to immediately run the
    -- scheduled GCs in case we have a 'gcDelay' of 0.
      runGc now
    . schedule
    . runGc now
  where
    slot = blockSlotNo block
    now  = blockArrivalTime block

    schedule :: GcState -> GcState
    schedule gcState = GcState {
          gcQueue              = GcQueue gcQueue'
        , gcBlocks             = GcBlocks $
              (block, gcDelay gcParams `addTime` now)
            : unGcBlocks (gcBlocks gcState)
        , gcGarbageCollections = gcGarbageCollections gcState
        }
      where
        scheduledTime = Impl.computeTimeForGC gcParams now
        gcQueue' = case unGcQueue (gcQueue gcState) of
          ScheduledGc prevScheduledTime _prevSlot:queue'
            | scheduledTime == prevScheduledTime
            -> ScheduledGc scheduledTime slot:queue'
          queue
            -> ScheduledGc scheduledTime slot:queue

{-------------------------------------------------------------------------------
  GcStateSummary
-------------------------------------------------------------------------------}

data GcStateSummary = GcStateSummary {
      gcSummaryNow         :: Time
    , gcSummaryQueue       :: GcQueue
    , gcSummaryQueueLength :: Int
    , gcSummaryOverlap     :: Int
    , gcSummaryUnnecessary :: Int
    }
  deriving (Show)

computeGcStateSummary :: Time -> GcState -> GcStateSummary
computeGcStateSummary now gcState = GcStateSummary {
      gcSummaryNow         = now
    , gcSummaryQueue       = gcQueue                gcState
    , gcSummaryQueueLength = queueLength            gcState
    , gcSummaryOverlap     = overlap                gcState
    , gcSummaryUnnecessary = unnecessaryOverlap now gcState
    }

{-------------------------------------------------------------------------------
  Trace
-------------------------------------------------------------------------------}

type Trace a = [a]

computeTrace :: GcParams -> [Block] -> Trace (Time, GcState)
computeTrace gcParams blocks =
    zip
      (map blockArrivalTime blocks)
      -- Remember:
      -- scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
      (drop 1 (scanl (flip (step gcParams)) emptyGcState blocks))

summarise :: GcParams -> Int -> Trace GcStateSummary
summarise gcParams numBlocks =
   map (uncurry computeGcStateSummary) $
     computeTrace gcParams (genBlocks numBlocks)

example :: GcParams -> Trace GcStateSummary
example gcParams = summarise gcParams 1000

-- | Process the remaining scheduled garbage collections in the queue. The
-- already performed garbage collections ('gcGarbageCollections') are included
-- in the final 'GcGarbageCollections'.
processQueueToEnd :: GcState -> GcGarbageCollections
processQueueToEnd gcState@GcState { gcQueue = GcQueue queue } =
    gcGarbageCollections (foldl' (flip runGc) gcState timesToGcAt)
  where
    timesToGcAt = sort (map scheduledGcTime queue)

{-------------------------------------------------------------------------------
  Run the real GcSchedule
-------------------------------------------------------------------------------}

runGcSchedule :: GcParams -> [Block] -> (Trace GcQueue, GcGarbageCollections)
runGcSchedule gcParams blocks = runSimOrThrow test
  where
    test :: IOLike m => m (Trace GcQueue, GcGarbageCollections)
    test = do
      varGCs <- uncheckedNewTVarM (GcGarbageCollections [])
      gcSchedule <- Impl.newGcSchedule
      withAsync (gcThread varGCs gcSchedule) $ \asyncGcThread -> do
        link asyncGcThread

        gcQueueTrace <- forM blocks $ \block -> do
          waitUntil (blockArrivalTime block)
          Impl.scheduleGC nullTracer (blockSlotNo block) gcParams gcSchedule
          -- Just the minimal number of time so that the background thread
          -- gets its chance to run. Since this is the IO simulator, it will
          -- run instantly.
          threadDelay (picosecondsToDiffTime 1)
          GcQueue <$> atomically (Impl.dumpGcSchedule gcSchedule)

        -- Wait until the implementation's queue is empty
        atomically $ do
          queue <- Impl.dumpGcSchedule gcSchedule
          check (null queue)

        cancel asyncGcThread
        gcs <- atomically $ readTVar varGCs
        return (gcQueueTrace, gcs)

    gcThread
      :: IOLike m
      => StrictTVar m GcGarbageCollections
      -> Impl.GcSchedule m
      -> m Void
    gcThread varGCs gcSchedule =
      Impl.gcScheduleRunner gcSchedule $ \slotNo -> do
        -- Record the time at which a GC for @slotNo@ was triggered in a TVar
        now <- getMonotonicTime
        atomically $ modifyTVar varGCs $ \(GcGarbageCollections gcs) ->
          GcGarbageCollections $ (slotNo, now) : gcs

    waitUntil :: IOLike m => Time -> m ()
    waitUntil t = do
      now <- getMonotonicTime
      let toWait = max 0 (t `diffTime` now)
      threadDelay toWait

{-------------------------------------------------------------------------------
  TestSetup
-------------------------------------------------------------------------------}

data TestSetup = TestSetup {
    -- | Number of blocks
    --
    -- This determines the length of the trace. Shrinking this value means
    -- we find the smallest trace that yields the error
    testNumBlocks            :: Int

    -- | GC delay in seconds
    --
    -- We keep this as a separate value /in seconds/ so that (1) it is easily
    -- shrinkable and (2) we can meaningfully use 'blocksInInterval'
  , testDelay                :: Integer

    -- | GC interval in seconds
    --
    -- See 'testDelay'
  , testInterval             :: Integer

    -- Derived
  , testGcParams             :: GcParams
  , testTrace                :: Trace GcStateSummary
    -- | The garbage collections that will have been performed after
    -- processing the whole queue.
  , testGcGarbageCollections :: GcGarbageCollections
  }
  deriving (Show)

genBlocks :: Int -> [Block]
genBlocks numBlocks = map Block [1..numBlocks]

mkTestSetup :: Int -> Integer -> Integer -> TestSetup
mkTestSetup numBlocks delay interval = TestSetup {
      testNumBlocks            = numBlocks
    , testDelay                = delay
    , testInterval             = interval
      -- Derived values
    , testGcParams             = gcParams
    , testTrace                = map (uncurry computeGcStateSummary) trace
    , testGcGarbageCollections = processQueueToEnd finalState
    }
  where
    trace = computeTrace gcParams (genBlocks numBlocks)

    finalState = maybe emptyGcState snd (lastMaybe trace)

    gcParams :: GcParams
    gcParams = GcParams {
          gcDelay    = secondsToDiffTime delay
        , gcInterval = secondsToDiffTime interval
        }


instance Arbitrary TestSetup where
  arbitrary =
      mkTestSetup
        <$> ((* 10) <$> getSize) -- Number of blocks
        <*> choose (0, 100)      -- Delay
        <*> choose (1, 120)      -- Interval

  shrink TestSetup{..} = concat [
        [ mkTestSetup testNumBlocks' testDelay testInterval
        | testNumBlocks' <- shrink testNumBlocks
        ]

      , [ mkTestSetup testNumBlocks testDelay' testInterval
        | testDelay' <- shrink testDelay
        ]

      , [ mkTestSetup testNumBlocks testDelay testInterval'
        | testInterval' <- shrink testInterval
        , testInterval' > 0
        ]

        -- Shrink two values shrink /together/
        -- Note: we don't compute all possible combinations, we shrink both
      , [ mkTestSetup testNumBlocks testDelay' testInterval'
        | testDelay    > 0
        , testInterval > 1
        , let testDelay'    = testDelay    - 1
        , let testInterval' = testInterval - 1
        ]
      ]
