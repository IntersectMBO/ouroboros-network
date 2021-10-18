{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus.Util.MonadSTM.RAWLock (tests) where

import           Control.Exception (throw)
import           Control.Monad.Except
import           Data.Time.Clock (picosecondsToDiffTime)

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.MonadSTM.RAWLock (RAWLock)
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as RAWLock
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Control.Monad.IOSim (IOSim, SimEventType (..), SimTrace,
                     runSimTrace, selectTraceEvents, traceResult)

import           Test.QuickCheck
import           Test.QuickCheck.Gen.Unsafe (Capture (..), capture)
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testProperty "RAWLock correctness" prop_RAWLock_correctness

-- | Test the correctness of the RAWLock
--
-- For a generated number of readers, appenders, and writers: spawn a thread
-- for each. Each thread will process a list of 'ThreadDelays'. For each
-- 'ThreadDelays': wait the generated 'beforeLockTime', then lock the RAWLock
-- with the respective @RAWLock.withXAccess@, increment a 'TVar' that stores
-- the number of readers/appenders/writers that have access, hold the lock for
-- a generated 'withLockTime', decrement the 'TVar', and release the lock.
--
-- In a separate thread, we watch for any changes in the three 'TVar's and
-- write each changed 'RAWState' to a trace (in a separate 'TVar').
-- Afterwards, we check the 'RAWState's in the trace for consistency (using
-- 'isConsistent'), e.g., not more than one concurrent appender.
prop_RAWLock_correctness :: TestSetup -> Property
prop_RAWLock_correctness (TestSetup rawDelays) =
    monadicSimWithTrace tabulateBlockeds test
  where
    RAW readerDelays appenderDelays writerDelays = rawDelays

    test :: forall m. IOLike m => PropertyM m ()
    test = do
      rawVars@(RAW varReaders varAppenders varWriters) <- run newRAWVars

      trace <- run $ withRegistry $ \registry -> do
        rawLock  <- RAWLock.new ()
        varTrace <- uncheckedNewTVarM []

        let traceState :: STM m ()
            traceState = do
              rawState <- readRAWState rawVars
              modifyTVar varTrace (rawState:)

        threads <- mapM (forkLinkedThread registry "testThread") $
          map (runReader   rawLock traceState varReaders)   readerDelays   <>
          map (runAppender rawLock traceState varAppenders) appenderDelays <>
          map (runWriter   rawLock traceState varWriters)   writerDelays

        mapM_ waitThread threads
        reverse <$> atomically (readTVar varTrace)

      checkRAWTrace trace

    runReader
      :: IOLike m
      => RAWLock m ()
      -> STM m ()  -- ^ Trace the 'RAWState'
      -> StrictTVar m Int
      -> [ThreadDelays]
      -> m ()
    runReader rawLock traceState varReaders =
      mapM_ $ \(ThreadDelays before with) -> do
        threadDelay before
        RAWLock.withReadAccess rawLock $ const $ do
          atomically $ modifyTVar varReaders succ *> traceState
          threadDelay with
          atomically $ modifyTVar varReaders pred *> traceState

    runAppender
      :: IOLike m
      => RAWLock m ()
      -> STM m ()  -- ^ Trace the 'RAWState'
      -> StrictTVar m Int
      -> [ThreadDelays]
      -> m ()
    runAppender rawLock traceState varAppenders =
      mapM_ $ \(ThreadDelays before with) -> do
        threadDelay before
        RAWLock.withAppendAccess rawLock $ const $ do
          atomically $ modifyTVar varAppenders succ *> traceState
          threadDelay with
          atomically $ modifyTVar varAppenders pred *> traceState
          return ((), ())

    runWriter
      :: IOLike m
      => RAWLock m ()
      -> STM m ()  -- ^ Trace the 'RAWState'
      -> StrictTVar m Int
      -> [ThreadDelays]
      -> m ()
    runWriter rawLock traceState varWriters =
      mapM_ $ \(ThreadDelays before with) -> do
        threadDelay before
        RAWLock.withWriteAccess rawLock $ const $ do
          atomically $ modifyTVar varWriters succ *> traceState
          threadDelay with
          atomically $ modifyTVar varWriters pred *> traceState
          return ((), ())

-- | Like 'monadicSim' (which is like 'monadicIO' for the IO simulator), but
-- allows inspecting the trace for labelling purposes.
monadicSimWithTrace
  :: Testable a
  => (forall x. SimTrace x -> Property -> Property)
  -> (forall s. PropertyM (IOSim s) a)
  -> Property
monadicSimWithTrace attachTrace m = property $ do
    tr <- runSimGenWithTrace (monadic' m)
    case traceResult False tr of
      Left failure -> throw failure
      Right prop   -> return $ attachTrace tr prop
  where
    runSimGenWithTrace :: (forall s. Gen (IOSim s a)) -> Gen (SimTrace a)
    runSimGenWithTrace f = do
      Capture eval <- capture
      return $ runSimTrace (eval f)

-- | Tabulate the number of times a thread is blocked.
--
-- The higher this number, the higher the contention. If there's no
-- contention, we're not testing the lock properly.
tabulateBlockeds :: SimTrace a -> Property -> Property
tabulateBlockeds tr =
    tabulate "number of times blocked" [classifyBand (count isBlocked tr)]
  where
    isBlocked (EventTxBlocked {}) = Just ()
    isBlocked _                   = Nothing

    count :: (SimEventType -> Maybe x) -> SimTrace a -> Int
    count p = length . selectTraceEvents p

    classifyBand :: Int -> String
    classifyBand n
      | n < 10
      = "n < 10"
      | n < 100
      = "n < 100"
      | n < 1000
      = "n < 1,000"
      | n < 10_000
      = "1,000 < n < 10,000"
      | n < 100_000
      = "10,000 < n < 100,000"
      | n < 1000_000
      = "100,000 < n < 1,000,000"
      | otherwise
      = "1,000,000 < n"

{-------------------------------------------------------------------------------
  State checking
-------------------------------------------------------------------------------}

-- | Data type reused whenever we need something for all three of them.
data RAW a = RAW
    { readers   :: a
    , appenders :: a
    , writers   :: a
    }
  deriving (Show, Eq, Functor)

type RAWVars m = RAW (StrictTVar m Int)

newRAWVars :: IOLike m => m (RAWVars m)
newRAWVars = RAW <$> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO 0

type RAWState = RAW Int

readRAWState :: IOLike m => RAWVars m -> STM m RAWState
readRAWState RAW { readers, appenders, writers } =
    RAW
      <$> readTVar readers
      <*> readTVar appenders
      <*> readTVar writers

isConsistent :: RAWState -> Except String ()
isConsistent RAW { readers, appenders, writers }
    | appenders > 1
    = throwError $ show appenders <> " appenders while at most 1 is allowed"
    | writers > 1
    = throwError $ show writers <> " writers while at most 1 is allowed"
    | writers == 1, readers > 0
    = throwError $ "writer concurrent with " <> show readers <> "reader(s)"
    | writers == 1, appenders > 0
    = throwError $ "writer concurrent with an appender"
    | otherwise
    = return ()

type RAWTrace = [RAWState]

checkRAWTrace :: Monad m => RAWTrace -> PropertyM m ()
checkRAWTrace = mapM_ $ \rawState ->
    case runExcept $ isConsistent rawState of
      Left msg -> do
        monitor (counterexample msg)
        assert False
      Right () ->
        return ()

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

newtype TestSetup = TestSetup (RAW [[ThreadDelays]])
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
    nbReaders   <- choose (0, 3)
    nbAppenders <- choose (0, 3)
    nbWriters   <- choose (0, 3)
    readers   <- vectorOf nbReaders   arbitrary
    appenders <- vectorOf nbAppenders arbitrary
    writers   <- vectorOf nbWriters   arbitrary
    return $ TestSetup RAW { readers, appenders, writers }
  shrink (TestSetup raw@RAW { readers, appenders, writers }) =
    [TestSetup raw { readers   = readers'   } | readers'   <- shrink readers  ] <>
    [TestSetup raw { appenders = appenders' } | appenders' <- shrink appenders] <>
    [TestSetup raw { writers   = writers'   } | writers'   <- shrink writers  ]

data ThreadDelays = ThreadDelays
    { beforeLockTime :: DiffTime
      -- ^ How long the thread should wait before it starts to take the lock
    , withLockTime   :: DiffTime
      -- ^ How long the thread should wait while holding the lock
    }
  deriving (Eq, Show)

instance Arbitrary ThreadDelays where
  arbitrary = do
    beforeLockTime <- picosecondsToDiffTime <$> choose (0, 1000)
    withLockTime   <- picosecondsToDiffTime <$> choose (0, 2000)
    return ThreadDelays { beforeLockTime, withLockTime }
