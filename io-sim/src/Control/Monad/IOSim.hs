{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , runSimTraceST
  , liftST
  , traceM
  , traceSTM
    -- * Simulation time
  , setCurrentTime
  , unshareClock
    -- * Simulation trace
  , type SimTrace
  , Trace (Cons, Nil, Trace, SimTrace, TraceMainReturn, TraceMainException, TraceDeadlock)
  , ppTrace
  , ppTrace_
  , ppEvents
  , SimResult (..)
  , SimEvent (..)
  , SimEventType (..)
  , ThreadLabel
  , Labelled (..)
  , traceEvents
  , traceResult
  , selectTraceEvents
  , selectTraceEvents'
  , selectTraceEventsDynamic
  , selectTraceEventsDynamic'
  , selectTraceEventsSay
  , selectTraceEventsSay'
  , traceSelectTraceEvents
  , traceSelectTraceEventsDynamic
  , traceSelectTraceEventsSay
  , printTraceEventsSay
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
import           Data.Typeable (Typeable)

import           Data.List.Trace (Trace (..))

import           Control.Exception (throw)

import           Control.Monad.ST.Lazy

import           Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.Class.MonadTime

import           Control.Monad.IOSim.Internal


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
                                     -> case fn (seType eventCtx) of
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
    go (TraceMainReturn _ _ tids@(_:_))
                               | strict = Left (FailureSloppyShutdown tids)
    go (TraceMainReturn _ x _)          = Right x
    go (TraceMainException _ e _)       = Left (FailureException e)
    go (TraceDeadlock   _   threads)    = Left (FailureDeadlock threads)

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
