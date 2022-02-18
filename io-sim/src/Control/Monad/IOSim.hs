{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Control.Monad.IOSim
  ( -- * Simulation monad
    IOSim
  , STMSim
    -- ** Run simulation
  , runSim
  , runSimOrThrow
  , runSimStrictShutdown
  , Failure (..)
  , runSimTrace
  , controlSimTrace
  , exploreSimTrace
  , ScheduleMod (..)
  , ScheduleControl (..)
  , runSimTraceST
  , liftST
  , traceM
  , traceSTM
    -- * Simulation time
  , setCurrentTime
  , unshareClock
    -- * Simulation trace
  , type SimTrace
  , Trace (Cons, Nil, Trace, SimTrace, TraceDeadlock, TraceLoop,
           TraceMainReturn, TraceMainException, TraceRacesFound)
  , SimResult (..)
  , SimEvent (..)
  , SimEventType (..)
  , ThreadLabel
  , Labelled (..)
    -- ** Pretty printers
  , ppTrace
  , ppTrace_
  , ppEvents
  , ppSimEvent
    -- ** Selectors
  , traceEvents
  , traceResult
    -- *** list selectors
  , selectTraceEvents
  , selectTraceEvents'
  , selectTraceEventsDynamic
  , selectTraceEventsDynamic'
  , selectTraceEventsSay
  , selectTraceEventsSay'
  , selectTraceRaces
    -- *** trace selectors
  , traceSelectTraceEvents
  , traceSelectTraceEventsDynamic
  , traceSelectTraceEventsSay
    -- ** IO printer
  , printTraceEventsSay
    -- * Exploration options
  , ExplorationSpec
  , ExplorationOptions
  , stdExplorationOptions
  , withScheduleBound
  , withBranching
  , withStepTimelimit
  , withReplay
    -- * Eventlog
  , EventlogEvent (..)
  , EventlogMarker (..)
    -- * Low-level API
  , execReadTVar
    -- * Deprecated interfaces
  , SimM
  , SimSTM
  , TraceEvent
  ) where

import           Prelude

import           Data.Bifoldable
import           Data.Dynamic (fromDynamic)
import           Data.List (intercalate)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)

import           Data.List.Trace (Trace (..))

import           Control.Exception (throw)

import           Control.Monad.ST.Lazy

import           Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.Class.MonadTime

import           Control.Monad.IOSim.Types
import           Control.Monad.IOSim.Internal
import           Control.Monad.IOSimPOR.Internal (controlSimTraceST)
import           Control.Monad.IOSimPOR.QuickCheckUtils

import           Test.QuickCheck


import           System.IO.Unsafe
import           Data.IORef


selectTraceEvents
    :: (SimEventType -> Maybe b)
    -> SimTrace a
    -> [b]
selectTraceEvents fn =
      bifoldr ( \ v _
               -> case v of
                    MainException _ e _       -> throw (FailureException e)
                    Deadlock      _   threads -> throw (FailureDeadlock threads)
                    MainReturn    _ _ _       -> []
                    Loop                      -> error "Impossible: selectTraceEvents _ TraceLoop{}"
              )
              ( \ b acc -> b : acc )
              []
    . traceSelectTraceEvents fn

selectTraceEvents'
    :: (SimEventType -> Maybe b)
    -> SimTrace a
    -> [b]
selectTraceEvents' fn =
      bifoldr ( \ _ _   -> []  )
              ( \ b acc -> b : acc )
              []
    . traceSelectTraceEvents fn

selectTraceRaces :: SimTrace a -> [ScheduleControl]
selectTraceRaces = go
  where
    go (SimTrace _ _ _ _ trace)      = go trace
    go (TraceRacesFound races trace) =
      races ++ go trace
    go _                             = []

-- Extracting races from a trace.  There is a subtlety in doing so: we
-- must return a defined list of races even in the case where the
-- trace is infinite, and there are no races occurring in it! For
-- example, if the system falls into a deterministic infinite loop,
-- then there will be no races to find.

-- In reality we only want to extract races from *the part of the
-- trace used in a test*. We can only observe that by tracking lazy
-- evaluation: only races that were found in the evaluated prefix of
-- an infinite trace should contribute to the "races found". Hence we
-- return a function that returns the races found "so far". This is
-- unsafe, of course, since that function may return different results
-- at different times.

detachTraceRaces :: SimTrace a -> (() -> [ScheduleControl], SimTrace a)
detachTraceRaces trace = unsafePerformIO $ do
  races <- newIORef []
  let readRaces ()  = concat . reverse . unsafePerformIO $ readIORef races
      saveRaces r t = unsafePerformIO $ do
                        modifyIORef races (r:)
                        return t
  let go (SimTrace a b c d trace)  = SimTrace a b c d $ go trace
      go (TraceRacesFound r trace) = saveRaces r   $ go trace
      go t                         = t
  return (readRaces,go trace)

-- | Select all the traced values matching the expected type. This relies on
-- the sim's dynamic trace facility.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
selectTraceEventsDynamic :: forall a b. Typeable b => SimTrace a -> [b]
selectTraceEventsDynamic = selectTraceEvents fn
  where
    fn :: SimEventType -> Maybe b
    fn (EventLog dyn) = fromDynamic dyn
    fn _              = Nothing

-- | Like 'selectTraceEventsDynamic' but returns partial trace if an exception
-- is found in it.
--
selectTraceEventsDynamic' :: forall a b. Typeable b => SimTrace a -> [b]
selectTraceEventsDynamic' = selectTraceEvents' fn
  where
    fn :: SimEventType -> Maybe b
    fn (EventLog dyn) = fromDynamic dyn
    fn _              = Nothing

-- | Get a trace of 'EventSay'.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
selectTraceEventsSay :: SimTrace a -> [String]
selectTraceEventsSay = selectTraceEvents fn
  where
    fn :: SimEventType -> Maybe String
    fn (EventSay s) = Just s
    fn _            = Nothing

-- | Like 'selectTraceEventsSay' but return partial trace if an exception is
-- found in it.
--
selectTraceEventsSay' :: SimTrace a -> [String]
selectTraceEventsSay' = selectTraceEvents' fn
  where
    fn :: SimEventType -> Maybe String
    fn (EventSay s) = Just s
    fn _            = Nothing

-- | Print all 'EventSay' to the console.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
printTraceEventsSay :: SimTrace a -> IO ()
printTraceEventsSay = mapM_ print . selectTraceEventsSay


-- | The most general select function.  It is a _total_ function.
--
traceSelectTraceEvents
    :: (SimEventType -> Maybe b)
    -> SimTrace a
    -> Trace (SimResult a) b
traceSelectTraceEvents fn = bifoldr ( \ v _acc -> Nil v )
                                    ( \ eventCtx acc
                                     -> case eventCtx of
                                          SimRacesFound _ -> acc
                                          SimEvent{} ->
                                            case fn (seType eventCtx) of
                                              Nothing -> acc
                                              Just b  -> Cons b acc
                                    )
                                    undefined -- it is ignored

-- | Select dynamic events.  It is a _total_ function.
--
traceSelectTraceEventsDynamic :: forall a b. Typeable b
                              => SimTrace a -> Trace (SimResult a) b
traceSelectTraceEventsDynamic = traceSelectTraceEvents fn
  where
    fn :: SimEventType -> Maybe b
    fn (EventLog dyn) = fromDynamic dyn
    fn _              = Nothing


-- | Select say events.  It is a _total_ function.
--
traceSelectTraceEventsSay :: forall a.  SimTrace a -> Trace (SimResult a) String
traceSelectTraceEventsSay = traceSelectTraceEvents fn
  where
    fn :: SimEventType -> Maybe String
    fn (EventSay s) = Just s
    fn _            = Nothing

-- | Simulation termination with failure
--
data Failure =
       -- | The main thread terminated with an exception
       FailureException SomeException

       -- | The threads all deadlocked
     | FailureDeadlock ![Labelled ThreadId]

       -- | The main thread terminated normally but other threads were still
       -- alive, and strict shutdown checking was requested.
       -- See 'runSimStrictShutdown'
     | FailureSloppyShutdown [Labelled ThreadId]
  deriving Show

instance Exception Failure where
    displayException (FailureException err) = displayException  err
    displayException (FailureDeadlock threads) =
      concat [ "<<io-sim deadlock: "
             , intercalate ", " (show `map` threads)
             , ">>"
             ]
    displayException (FailureSloppyShutdown threads) =
      concat [ "<<io-sim sloppy shutdown: "
             , intercalate ", " (show `map` threads)
             , ">>"
             ]

-- | 'IOSim' is a pure monad.
--
runSim :: forall a. (forall s. IOSim s a) -> Either Failure a
runSim mainAction = traceResult False (runSimTrace mainAction)

-- | For quick experiments and tests it is often appropriate and convenient to
-- simply throw failures as exceptions.
--
runSimOrThrow :: forall a. (forall s. IOSim s a) -> a
runSimOrThrow mainAction =
    case runSim mainAction of
      Left  e -> throw e
      Right x -> x

-- | Like 'runSim' but also fail if when the main thread terminates, there
-- are other threads still running or blocked. If one is trying to follow
-- a strict thread cleanup policy then this helps testing for that.
--
runSimStrictShutdown :: forall a. (forall s. IOSim s a) -> Either Failure a
runSimStrictShutdown mainAction = traceResult True (runSimTrace mainAction)

traceResult :: Bool -> SimTrace a -> Either Failure a
traceResult strict = go
  where
    go (SimTrace _ _ _ _ t)             = go t
    go (TraceRacesFound _ t)            = go t
    go (TraceMainReturn _ _ tids@(_:_))
                               | strict = Left (FailureSloppyShutdown tids)
    go (TraceMainReturn _ x _)          = Right x
    go (TraceMainException _ e _)       = Left (FailureException e)
    go (TraceDeadlock   _   threads)    = Left (FailureDeadlock threads)
    go TraceLoop{}                      = error "Impossible: traceResult TraceLoop{}"

traceEvents :: SimTrace a -> [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
traceEvents (SimTrace time tid tlbl event t) = (time, tid, tlbl, event)
                                             : traceEvents t
traceEvents _                                = []


ppEvents :: [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
         -> String
ppEvents events =
    intercalate "\n"
      [ ppSimEvent width
                   SimEvent {seTime, seThreadId, seThreadLabel, seType }
      | (seTime, seThreadId, seThreadLabel, seType) <- events
      ]
  where
    width = maximum
              [ maybe 0 length threadLabel
              | (_, _, threadLabel, _) <- events
              ]


-- | See 'runSimTraceST' below.
--
runSimTrace :: forall a. (forall s. IOSim s a) -> SimTrace a
runSimTrace mainAction = runST (runSimTraceST mainAction)

controlSimTrace :: forall a.
                   Maybe Int
                -> ScheduleControl
                -- ^ note: must be either `ControlDefault` or `ControlAwait`. 
                -> (forall s. IOSim s a)
                -> SimTrace a
controlSimTrace limit control mainAction =
    runST (controlSimTraceST limit control mainAction)

exploreSimTrace
  :: forall a test. Testable test
  => (ExplorationOptions -> ExplorationOptions)
  -> (forall s. IOSim s a)
  -> (Maybe (SimTrace a) -> SimTrace a -> test)
  -> Property
exploreSimTrace optsf mainAction k =
  case explorationReplay opts of
    Nothing ->
      explore (explorationScheduleBound opts) (explorationBranching opts) ControlDefault Nothing .&&.
      let size = cacheSize() in size `seq`
      tabulate "Modified schedules explored" [bucket size] True
    Just control ->
      replaySimTrace opts mainAction control k
  where
    opts = optsf stdExplorationOptions

    explore n m control passingTrace =

      -- ALERT!!! Impure code: readRaces must be called *after* we have
      -- finished with trace.
      let (readRaces,trace0) = detachTraceRaces $
                                 controlSimTrace (explorationStepTimelimit opts) control mainAction
          (sleeper,trace) = compareTraces passingTrace trace0
      in (counterexample ("Schedule control: " ++ show control) $
          counterexample (case sleeper of Nothing -> "No thread delayed"
                                          Just ((t,tid,lab),racing) ->
                                            showThread (tid,lab) ++
                                            " delayed at time "++
                                            show t ++
                                            "\n  until after:\n" ++
                                            unlines (map (("    "++).showThread) $ Set.toList racing)
                                            ) $
          k passingTrace trace) .&&|
         let limit     = (n+m-1) `div` m
             -- To ensure the set of schedules explored is deterministic, we filter out
             -- cached ones *after* selecting the children of this node.
             races     = filter (not . cached) . take limit $ readRaces()
             branching = length races
         in -- tabulate "Races explored" (map show races) $
            tabulate "Branching factor" [bucket branching] $
            tabulate "Race reversals per schedule" [bucket (raceReversals control)] $
            conjoinPar
              [ --Debug.trace "New schedule:" $
                --Debug.trace ("  "++show r) $
                --counterexample ("Schedule control: " ++ show r) $
                explore n' ((m-1) `max` 1) r (Just trace0)
              | (r,n') <- zip races (divide (n-branching) branching) ]

    bucket n | n<10  = show n
             | n>=10 = buck n 1
             | otherwise = error "Ord Int is not a total order!"  -- GHC made me do it!
    buck n t | n<10      = show (n*t) ++ "-" ++ show ((n+1)*t-1)
             | n>=10     = buck (n `div` 10) (t*10)
             | otherwise = error "Ord Int is not a total order!"  -- GHC made me do it!

    divide n k =
      [ n `div` k + if i<n `mod` k then 1 else 0
      | i <- [0..k-1] ]

    showThread :: (ThreadId,Maybe ThreadLabel) -> String
    showThread (tid,lab) =
      show tid ++ (case lab of Nothing -> ""
                               Just l  -> " ("++l++")")

    -- It is possible for the same control to be generated several times.
    -- To avoid exploring them twice, we keep a cache of explored schedules.
    cache = unsafePerformIO $ newIORef $
              -- we use opts here just to be sure the reference cannot be
              -- lifted out of exploreSimTrace
              if explorationScheduleBound opts>=0
                then Set.empty
                else error "exploreSimTrace: negative schedule bound"
    cached m = unsafePerformIO $ atomicModifyIORef' cache $ \set ->
      (Set.insert m set, Set.member m set)
    cacheSize () = unsafePerformIO $ Set.size <$> readIORef cache

replaySimTrace :: forall a test. (Testable test)
               => ExplorationOptions
               -> (forall s. IOSim s a)
               -> ScheduleControl
               -> (Maybe (SimTrace a) -> SimTrace a -> test)
               -> Property
replaySimTrace opts mainAction control k =
  let (_,trace) = detachTraceRaces $
                            controlSimTrace (explorationStepTimelimit opts) control mainAction
      in property (k Nothing trace)

raceReversals :: ScheduleControl -> Int
raceReversals ControlDefault      = 0
raceReversals (ControlAwait mods) = length mods
raceReversals ControlFollow{}     = error "Impossible: raceReversals ControlFollow{}"

-- compareTraces is given (maybe) a passing trace and a failing trace,
-- and identifies the point at which they diverge, where it inserts a
-- "sleep" event for the thread that is delayed in the failing case,
-- and a "wake" event before its next action. It also returns the
-- identity and time of the sleeping thread. Since we expect the trace
-- to be consumed lazily (and perhaps only partially), and since the
-- sleeping thread is not of interest unless the trace is consumed
-- this far, then we collect its identity only if it is reached using
-- unsafePerformIO.

compareTraces :: Maybe (SimTrace a1)
              -> SimTrace a2
              -> (Maybe ((Time, ThreadId, Maybe ThreadLabel),
                         Set.Set (ThreadId, Maybe ThreadLabel)),
                  SimTrace a2)
compareTraces Nothing trace = (Nothing, trace)
compareTraces (Just passing) trace = unsafePerformIO $ do
  sleeper <- newIORef Nothing
  return (unsafePerformIO $ readIORef sleeper,
          go sleeper passing trace)
  where go sleeper (SimTrace tpass tidpass _ _ pass')
           (SimTrace tfail tidfail tlfail evfail fail')
          | (tpass,tidpass) == (tfail,tidfail) =
              SimTrace tfail tidfail tlfail evfail $
                go sleeper pass' fail'
        go sleeper (SimTrace tpass tidpass tlpass _ _) fail =
          unsafePerformIO $ do
            writeIORef sleeper $ Just ((tpass, tidpass, tlpass),Set.empty)
            return $ SimTrace tpass tidpass tlpass EventThreadSleep $
                       wakeup sleeper tidpass fail
        go _ _ fail = fail
        wakeup sleeper tidpass (SimTrace tfail tidfail tlfail evfail fail')
          | tidpass == tidfail =
              SimTrace tfail tidfail tlfail EventThreadWake fail'
          | otherwise = unsafePerformIO $ do
              Just (slp,racing) <- readIORef sleeper
              writeIORef sleeper $ Just (slp,Set.insert (tidfail,tlfail) racing)
              return $ SimTrace tfail tidfail tlfail evfail
                     $ wakeup sleeper tidpass fail'
        wakeup _ _ fail = fail
