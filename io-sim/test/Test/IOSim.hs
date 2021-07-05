{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IOSim
    ( tests
    , TestThreadGraph (..)
    , arbitraryAcyclicGraph
    ) where

import           Data.Array
import           Data.Either (isLeft)
import           Data.Fixed (Fixed (..), Micro)
import           Data.Graph
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Time.Clock (picosecondsToDiffTime)

import           Control.Exception (ArithException (..))
import           Control.Monad
import           System.IO.Error (ioeGetErrorString, isUserError)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim

import           Test.STM

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck


tests :: TestTree
tests =
  testGroup "IO simulator"
  [ testProperty "read/write graph (IO)"    prop_stm_graph_io
  , testProperty "read/write graph (IOSim)" (withMaxSuccess 1000 prop_stm_graph_sim)
  , testProperty "timers (IOSim)"           (withMaxSuccess 1000 prop_timers_ST)
  -- fails since we just use `threadDelay` to schedule timers in `IO`.
  , testProperty "timers (IO)"              (expectFailure prop_timers_IO)
  , testProperty "timeout (IOSim): no deadlock"
                                            prop_timeout_no_deadlock_Sim
  , testProperty "timeout (IO): no deadlock"
                                            prop_timeout_no_deadlock_IO
  , testProperty "threadId order (IOSim)"   (withMaxSuccess 1000 prop_threadId_order_order_Sim)
  , testProperty "forkIO order (IOSim)"     (withMaxSuccess 1000 prop_fork_order_ST)
  , testProperty "order (IO)"               (expectFailure prop_fork_order_IO)
  , testProperty "STM wakeup order"         prop_wakeup_order_ST
  , testGroup "throw/catch unit tests"
    [ testProperty "0" unit_catch_0
    , testProperty "1" unit_catch_1
    , testProperty "2" unit_catch_2
    , testProperty "3" unit_catch_3
    , testProperty "4" unit_catch_4
    , testProperty "5" unit_catch_5
    , testProperty "6" unit_catch_6
    ]
  , testProperty "evaluate unit test" unit_evaluate_0
  , testGroup "forkIO unit tests"
    [ testProperty "1" unit_fork_1
    , testProperty "2" unit_fork_2
    ]
  , testGroup "async exception unit tests"
    [ testProperty "1"  unit_async_1
    , testProperty "2"  unit_async_2
    , testProperty "3"  unit_async_3
    , testProperty "4"  unit_async_4
    , testProperty "5"  unit_async_5
    , testProperty "6"  unit_async_6
    , testProperty "7"  unit_async_7
    , testProperty "8"  unit_async_8
    , testProperty "9"  unit_async_9
    , testProperty "10" unit_async_10
    , testProperty "11" unit_async_11
    , testProperty "12" unit_async_12
    , testProperty "13" unit_async_13
    , testProperty "14" unit_async_14
    , testProperty "15" unit_async_15
    , testProperty "16" unit_async_16
    ]
  , testGroup "STM reference semantics"
    [ testProperty "Reference vs IO"    prop_stm_referenceIO
    , testProperty "Reference vs Sim"   prop_stm_referenceSim
    ]
  ]


--
-- Read/Write graph
--

prop_stm_graph_io :: TestThreadGraph -> Property
prop_stm_graph_io g =
  ioProperty $
    prop_stm_graph g

prop_stm_graph_sim :: TestThreadGraph -> Bool
prop_stm_graph_sim g =
    case runSim (prop_stm_graph g) of
       Right () -> True
       _        -> False
    -- TODO: Note that we do not use runSimStrictShutdown here to check
    -- that all other threads finished, but perhaps we should and structure
    -- the graph tests so that's the case.

prop_stm_graph :: (MonadFork m, MonadSTM m) => TestThreadGraph -> m ()
prop_stm_graph (TestThreadGraph g) = do
    vars <- listArray (bounds g) <$>
            sequence [ newTVarIO False | _ <- vertices g ]
    forM_ (vertices g) $ \v ->
      void $ forkIO $ do
        -- read all the inputs and wait for them to become true
        -- then write to all the outputs
        let incomming = g' ! v
            outgoing  = g  ! v
        atomically $ do
          sequence_ [ readTVar  (vars ! var) >>= check | var <- incomming ]
          sequence_ [ writeTVar (vars ! var) True      | var <- outgoing  ]

    let -- the vertices with outgoing but no incoming edges
        inputs  = [ v
                  | v <- vertices g
                  , not (null (g  ! v))
                  ,      null (g' ! v) ]
        -- the vertices with incoming but no outgoing edges
        outputs = [ v
                  | v <- vertices g
                  , not (null (g' ! v))
                  ,      null (g  ! v) ]

    -- write to the inputs and wait for the outputs
    void $ forkIO $ atomically $ sequence_ [ writeTVar (vars ! var) True | var <- inputs  ]
    atomically $ sequence_ [ readTVar (vars ! var) >>= check | var <- outputs ]
  where
    g' = transposeG g -- for incoming edges

newtype TestThreadGraph = TestThreadGraph Graph
  deriving Show

instance Arbitrary TestThreadGraph where
  arbitrary =
    sized $ \sz ->
    TestThreadGraph <$> arbitraryAcyclicGraph
                          (choose (2, 8 `min` (sz `div` 3)))
                          (choose (1, 8 `min` (sz `div` 3)))
                          0.3

arbitraryAcyclicGraph :: Gen Int -> Gen Int -> Float -> Gen Graph
arbitraryAcyclicGraph genNRanks genNPerRank edgeChance = do
    nranks    <- genNRanks
    rankSizes <- replicateM nranks genNPerRank
    let rankStarts = scanl (+) 0 rankSizes
        rankRanges = drop 1 (zip rankStarts (tail rankStarts))
        totalRange = sum rankSizes
    rankEdges <- mapM (uncurry genRank) rankRanges
    return $ buildG (0, totalRange-1) (concat rankEdges)
  where
    genRank :: Vertex -> Vertex -> Gen [Edge]
    genRank rankStart rankEnd =
      filterM (const (pick edgeChance))
        [ (i,j)
        | i <- [0..rankStart-1]
        , j <- [rankStart..rankEnd-1]
        ]

    pick :: Float -> Gen Bool
    pick chance = (< chance) <$> choose (0,1)


--
-- Timers
--

newtype TestMicro = TestMicro [Micro]
  deriving Show

-- |
-- Arbitrary non negative micro numbers with a high probability of
-- repetitions.
instance Arbitrary TestMicro where
  arbitrary = sized $ \n -> TestMicro <$> genN n []
    where
      genN :: Int -> [Micro] -> Gen [Micro]
      genN 0 rs = return rs
      genN n [] = do
        r <- genMicro
        genN (n - 1) [r]
      genN n rs = do
        r <- frequency
          [ (2, elements rs)
          , (1, genMicro)
          ]
        genN (n - 1) (r : rs)

      genMicro :: Gen Micro
      genMicro = MkFixed <$> arbitrary

  shrink (TestMicro rs) = [ TestMicro rs' | rs' <- shrinkList (const []) rs ]

test_timers :: forall m.
               ( MonadFork m
               , MonadSTM m
               , MonadTimer m
               )
            => [DiffTime]
            -> m Property
test_timers xs =
    label (lbl xs) . isValid <$> withProbe experiment
  where
    countUnique :: Eq a => [a] -> Int
    countUnique [] = 0
    countUnique (a:as) =
      let as' = filter (== a) as
      in 1 + countUnique as'

    lbl :: Eq a => [a] -> String
    lbl as =
      let p = (if null as then 0 else (100 * countUnique as) `div` length as) `mod` 10 * 10
      in show p ++ "% unique"

    experiment :: Probe m (DiffTime, Int) -> m ()
    experiment p = do
      tvars <- forM (zip xs [0..]) $ \(t, idx) -> do
        v <- newTVarIO False
        void $ forkIO $ threadDelay t >> do
          probeOutput p (t, idx)
          atomically $ writeTVar v True
        return v

      -- wait for all tvars
      forM_ tvars $ \v -> atomically (readTVar v >>= check)

    isValid :: [(DiffTime, Int)] -> Property
    isValid tr =
         -- all timers should fire
         (length tr === length xs)
         -- timers should fire in the right order
      .&&. (sortBy (on sortFn fst) tr === tr)

    -- timers with negative timeout never fired, so we treat them as they would
    -- all fired at once at @-∞@.  This is to say that the following function is
    -- a well defined partial order.
    sortFn :: DiffTime -> DiffTime -> Ordering
    sortFn a b | a >= 0 && b >= 0 = a `compare` b
               | a  < 0 && b  < 0 = EQ
               | otherwise = a `compare` b

prop_timers_ST :: TestMicro -> Property
prop_timers_ST (TestMicro xs) =
  let ds = map (realToFrac :: Micro -> DiffTime) xs
  in runSimOrThrow $ test_timers ds

prop_timers_IO :: [Positive Int] -> Property
prop_timers_IO = ioProperty . test_timers
               . map (microsecondsToDiffTime . (*100) . getPositive)
  where
    microsecondsToDiffTime :: Int -> DiffTime
    microsecondsToDiffTime = picosecondsToDiffTime . (* 1000000) . toInteger

--
-- Forking
--

test_fork_order :: forall m.
                   ( MonadFork m
                   , MonadSTM m
                   , MonadTimer m
                   )
                => Positive Int
                -> m Property
test_fork_order = \(Positive n) -> isValid n <$> withProbe (experiment n)
  where
    experiment :: Int -> Probe m Int -> m ()
    experiment 0 _ = return ()
    experiment n p = do
      v <- newTVarIO False

      void $ forkIO $ do
        probeOutput p n
        atomically $ writeTVar v True
      experiment (n - 1) p

      -- wait for the spawned thread to finish
      atomically $ readTVar v >>= check

    isValid :: Int -> [Int] -> Property
    isValid n tr = tr === [n,n-1..1]

prop_fork_order_ST :: Positive Int -> Property
prop_fork_order_ST n = runSimOrThrow $ test_fork_order n

prop_fork_order_IO :: Positive Int -> Property
prop_fork_order_IO = ioProperty . test_fork_order


test_threadId_order :: forall m.
                   ( MonadFork m
                   , MonadSTM m
                   , MonadTimer m
                   )
                => Positive Int
                -> m Property
test_threadId_order = \(Positive n) -> do
    isValid n <$> (forM [1..n] (const experiment))
  where
    experiment :: m (ThreadId m)
    experiment = do
      v <- newTVarIO False

      tid <- forkIO $ atomically $ writeTVar v True

      -- wait for the spawned thread to finish
      atomically $ readTVar v >>= check
      return tid

    isValid :: Int -> [ThreadId m] -> Property
    isValid n tr = map show tr === map (("ThreadId " ++ ) . show) [1..n]

prop_threadId_order_order_Sim :: Positive Int -> Property
prop_threadId_order_order_Sim n = runSimOrThrow $ test_threadId_order n


prop_wakeup_order_ST :: Property
prop_wakeup_order_ST = runSimOrThrow $ test_wakeup_order

-- This property is not actually deterministic in IO. Uncomment the following
-- and try it! Arguably therefore, this property does not need to be true for
-- the Sim either. Perhaps we should introduce random scheduling and abandon
-- this property. In the meantime it's a helpful sanity check.

--prop_wakeup_order_IO = ioProperty test_wakeup_order

test_wakeup_order :: ( MonadFork m
                     , MonadSTM m
                     , MonadTimer m
                     )
                => m Property
test_wakeup_order = do
    v          <- newTVarIO False
    wakupOrder <-
      withProbe $ \p -> do
        sequence_
          [ do _ <- forkIO $ do
                 atomically $ do
                   x <- readTVar v
                   check x
                 probeOutput p (n :: Int)
               threadDelay 0.1
          | n <- [0..9] ]
        atomically $ writeTVar v True
        threadDelay 0.1
    return (wakupOrder === [0..9]) --FIFO order


--
-- Probe mini-abstraction
--

-- | Where returning results directly is not convenient, we can build up
-- a trace of events we want to observe, and can do probe output from
-- multiple threads.
--
type Probe m x = StrictTVar m [x]

withProbe :: MonadSTM m => (Probe m x -> m ()) -> m [x]
withProbe action = do
    probe <- newTVarIO []
    action probe
    reverse <$> atomically (readTVar probe)

probeOutput :: MonadSTM m => Probe m x -> x -> m ()
probeOutput probe x = atomically (modifyTVar probe (x:))


--
-- Syncronous exceptions
--

unit_catch_0, unit_catch_1, unit_catch_2, unit_catch_3, unit_catch_4,
  unit_catch_5, unit_catch_6,
  unit_fork_1, unit_fork_2
  :: Property

-- unhandled top level exception
unit_catch_0 =
      runSimTraceSay example === ["before"]
 .&&. case traceResult True (runSimTrace example) of
        Left (FailureException e) -> property (maybe False (==DivideByZero) $ fromException e)
        _                         -> property False

 where
  example :: IOSim s ()
  example = do
    say "before"
    _ <- throwIO DivideByZero
    say "after"

-- normal execution of a catch frame
unit_catch_1 =
    runSimTraceSay
      (do catch (say "inner") (\(_e :: IOError) -> say "handler")
          say "after"
      )
 ===
    ["inner", "after"]


-- catching an exception thrown in a catch frame
unit_catch_2 =
    runSimTraceSay
      (do catch (do say "inner1"
                    _ <- throwIO DivideByZero
                    say "inner2")
                (\(_e :: ArithException) -> say "handler")
          say "after"
      )
 ===
    ["inner1", "handler", "after"]


-- not catching an exception of the wrong type
unit_catch_3 =
    runSimTraceSay
      (do catch (do say "inner"
                    throwIO DivideByZero)
                (\(_e :: IOError) -> say "handler")
          say "after"
      )
 ===
    ["inner"]


-- catching an exception in an outer handler
unit_catch_4 =
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(_e :: IOError) -> say "handler1"))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler2", "after"]


-- catching an exception in the inner handler
unit_catch_5 =
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(_e :: ArithException) -> say "handler1"))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler1", "after"]


-- catching an exception in the inner handler, rethrowing and catching in outer
unit_catch_6 =
    runSimTraceSay
      (do catch (catch (do say "inner"
                           throwIO DivideByZero)
                       (\(e :: ArithException) -> do
                           say "handler1"
                           throwIO e))
                (\(_e :: ArithException) -> say "handler2")
          say "after"
      )
 ===
    ["inner", "handler1", "handler2", "after"]


-- evaluate should catch pure errors
unit_evaluate_0 :: Property
unit_evaluate_0 =
    -- This property also fails if the @error@ is not caught by the sim monad
    -- and instead reaches the QuickCheck driver.
    property $ isLeft $ runSim $ evaluate (error "boom" :: ())


-- The sim terminates when the main thread terminates
unit_fork_1 =
      runSimTraceSay example === ["parent"]
 .&&. case traceResult True (runSimTrace example) of
        Left FailureSloppyShutdown{} -> property True
        _                            -> property False
  where
    example :: IOSim s ()
    example = do
      void $ forkIO $ say "child"
      say "parent"

-- Try works and we can pass exceptions back from threads.
-- And terminating with an exception is reported properly.
unit_fork_2 =
      runSimTraceSay example === ["parent", "user error (oh noes!)"]
 .&&. case traceResult True (runSimTrace example) of
        Left (FailureException e)
          | Just ioe <- fromException e
          , isUserError ioe
          , ioeGetErrorString ioe == "oh noes!" -> property True
        _                                       -> property False
  where
    example :: IOSim s ()
    example = do
      resVar <- newEmptyTMVarIO
      void $ forkIO $ do
        res <- try (fail "oh noes!")
        atomically (putTMVar resVar (res :: Either SomeException ()))
      say "parent"
      Left e <- atomically (takeTMVar resVar)
      say (show e)
      throwIO e


--
-- Asyncronous exceptions
--

unit_async_1, unit_async_2, unit_async_3, unit_async_4, unit_async_5,
  unit_async_6, unit_async_7, unit_async_8, unit_async_9, unit_async_10,
  unit_async_11, unit_async_12, unit_async_13, unit_async_14, unit_async_15,
  unit_async_16
  :: Property


unit_async_1 =
    runSimTraceSay
      (do mtid <- myThreadId
          say ("main " ++ show mtid)
          ctid <- forkIO $ do tid <- myThreadId
                              say ("child " ++ show tid)
          say ("parent " ++ show ctid)
          threadDelay 1
      )
 ===
   ["main ThreadId 0", "parent ThreadId 1", "child ThreadId 1"]


unit_async_2 =
    runSimTraceSay
      (do tid <- myThreadId
          say "before"
          throwTo tid DivideByZero
          say "after"
      )
 ===
   ["before"]


unit_async_3 =
    runSimTraceSay
      (do tid <- myThreadId
          catch (do say "before"
                    throwTo tid DivideByZero
                    say "never")
                (\(_e :: ArithException) -> say "handler"))
 ===
   ["before", "handler"]


unit_async_4 =
    runSimTraceSay
      (do tid <- forkIO $ say "child"
          threadDelay 1
          -- child has already terminated when we throw the async exception
          throwTo tid DivideByZero
          say "parent done")
 ===
   ["child", "parent done"]


unit_async_5 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   say "child"
                   catch (atomically retry)
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "handler", "child done", "parent done"]


unit_async_6 =
    runSimTraceSay
      (do tid <- forkIO $ mask_ $
                   do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- while masked, do a blocking (interruptible) operation
                     catch (atomically retry)
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_7 =
    runSimTraceSay
      (do tid <- forkIO $
                   mask $ \restore -> do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- restore mask state, allowing interrupt
                     catch (restore (say "never"))
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_8 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   catch (do mask_ $ do
                               say "child"
                               threadDelay 1
                               say "child masked"
                               -- exception raised when we leave mask frame
                             say "child unmasked")
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_9 =
    runSimTraceSay
      (do tid <- forkIO $
                   mask_ $ do
                     say "child"
                     threadDelay 1
                     fail "oh noes!"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          -- throwTo blocks but then unblocks because the child dies
          say "parent done")
 ===
   ["child", "parent done"]


unit_async_10 =
    runSimTraceSay
      (do tid1 <- forkIO $ do
                    mask_ $ do
                      threadDelay 1
                      say "child 1"
                      yield
                      say "child 1 running"
                    say "never 1"
          tid2 <- forkIO $ do
                      threadDelay 1
                      say "child 2"
                      -- this one blocks, since child 1 is running with
                      -- async exceptions masked
                      throwTo tid1 DivideByZero
                      say "never 2"
          threadDelay 1
          yield
          -- this one does not block, child 2 does not have exceptions
          -- masked (and it is blocked in an interruptible throwTo)
          throwTo tid2 DivideByZero
          threadDelay 1
          say "parent done"
          )
 ===
   ["child 1", "child 2", "child 1 running", "parent done"]
  where
    yield :: IOSim s ()
    yield = atomically (return ())  -- yield, go to end of runqueue


unit_async_11 =
    runSimTraceSay
      (do tid1 <- forkIO $ do
                    mask_ $ do
                      threadDelay 1
                      say "child 1"
                      yield
                      say "child 1 running"
                    say "never 1"
          tid2 <- forkIO $
                    -- Same as unit_async_10 but we run masked here
                    -- this is subtle: when the main thread throws the
                    -- exception it raises the exception here even though
                    -- it is masked because this thread is blocked in the
                    -- throwTo and so is interruptible.
                    mask_ $ do
                      threadDelay 1
                      say "child 2"
                      throwTo tid1 DivideByZero
                      say "never 2"
          threadDelay 1
          yield
          -- this one does not block, even though child 2 has exceptions
          -- masked, since it is blocked in an interruptible throwTo
          throwTo tid2 DivideByZero
          threadDelay 1
          say "parent done"
          )
 ===
   ["child 1", "child 2", "child 1 running", "parent done"]
  where
    yield :: IOSim s ()
    yield = atomically (return ())  -- yield, go to end of runqueue


unit_async_12 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   uninterruptibleMask_ $ do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- while masked, do a blocking (interruptible) operation
                     catch (threadDelay 1)
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
                   say "never"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "child done", "parent done"]


unit_async_13 =
    case runSim
           (uninterruptibleMask_ $ do
              tid <- forkIO $ atomically retry
              throwTo tid DivideByZero)
       of Left FailureDeadlock {} -> property True
          _                       -> property False


unit_async_14 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   uninterruptibleMask_ $ do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- while masked do a blocking operation, but this is
                     -- an uninterruptible mask so nothing happens
                     catch (threadDelay 1)
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
                   say "never"
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "child done", "parent done"]


unit_async_15 =
    runSimTraceSay
      (do tid <- forkIO $
                   uninterruptibleMask $ \restore -> do
                     say "child"
                     threadDelay 1
                     say "child masked"
                     -- restore mask state, allowing interrupt
                     catch (restore (say "never"))
                         (\(_e :: ArithException) -> say "handler")
                     say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


unit_async_16 =
    runSimTraceSay
      (do tid <- forkIO $ do
                   catch (do uninterruptibleMask_ $ do
                               say "child"
                               threadDelay 1
                               say "child masked"
                               -- exception raised when we leave mask frame
                             say "child unmasked")
                         (\(_e :: ArithException) -> say "handler")
                   say "child done"
          -- parent and child wake up on the runqueue at the same time
          threadDelay 1
          throwTo tid DivideByZero
          threadDelay 1
          say "parent done")
 ===
   ["child", "child masked", "handler", "child done", "parent done"]


--
-- Tests vs STM operational semantics
--

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of the real IO STM implementation.
--
prop_stm_referenceIO :: SomeTerm -> Property
prop_stm_referenceIO t =
    ioProperty (prop_stm_referenceM t)

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of the IO simulator's STM implementation.
--
prop_stm_referenceSim :: SomeTerm -> Property
prop_stm_referenceSim t =
    runSimOrThrow (prop_stm_referenceM t)

--TODO: would be nice to also have stronger tests here:
-- * compare all the tvar values in the heap
-- * compare the read and write sets

-- | Compare the behaviour of the STM reference operational semantics with
-- the behaviour of any 'MonadSTM' STM implementation.
--
prop_stm_referenceM :: (MonadSTM m, MonadThrow (STM m), MonadCatch m)
                    => SomeTerm -> m Property
prop_stm_referenceM (SomeTerm _tyrep t) = do
    let (r1, _heap) = evalAtomically t
    r2 <- execAtomically t
    return (r1 === r2)


-- | Check that 'timeout' does not deadlock when executed with asynchronous
-- exceptions uninterruptibly masked.
--
prop_timeout_no_deadlockM :: forall m. ( MonadFork m, MonadSTM m, MonadTimer m, MonadMask m )
                          => m Bool
prop_timeout_no_deadlockM = do
    v <- registerDelay' 0.01
    r <- uninterruptibleMask_ $ timeout 0.02 $ do
      atomically $ do
        readTVar v >>= check
        return True
    case r of
      Nothing -> return False
      Just b  -> return b
  where
    -- Like 'registerDelay', but does not require threaded RTS in the @m ~ IO@
    -- case.
    registerDelay' :: DiffTime -> m (StrictTVar m Bool)
    registerDelay' delta = do
      v <- newTVarIO False
      _ <- forkIO $ do
             threadDelay delta
             atomically (writeTVar v True)
      return v

prop_timeout_no_deadlock_Sim :: Bool
prop_timeout_no_deadlock_Sim = runSimOrThrow prop_timeout_no_deadlockM

prop_timeout_no_deadlock_IO :: Property
prop_timeout_no_deadlock_IO = ioProperty prop_timeout_no_deadlockM

--
-- Utils
--

runSimTraceSay :: (forall s. IOSim s a) -> [String]
runSimTraceSay action = selectTraceSay (runSimTrace action)

selectTraceSay :: SimTrace a -> [String]
selectTraceSay (SimTrace _ _ _ (EventSay msg) trace) = msg : selectTraceSay trace
selectTraceSay (SimTrace _ _ _ _              trace) = selectTraceSay trace
selectTraceSay  _                                    = []
