{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
-- incomplete uni patterns in 'schedule' (when interpreting 'StmTxCommitted')
-- and 'reschedule'.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Control.Monad.IOSim.Internal
  ( IOSim (..)
  , SimM
  , runIOSim
  , runSimTraceST
  , traceM
  , traceSTM
  , STM
  , STMSim
  , SimSTM
  , setCurrentTime
  , unshareClock
  , TimeoutException (..)
  , EventlogEvent (..)
  , EventlogMarker (..)
  , ThreadId
  , ThreadLabel
  , Labelled (..)
  , SimTrace
  , Trace.Trace (SimTrace, Trace, TraceMainReturn, TraceMainException, TraceDeadlock)
  , SimEvent (..)
  , SimResult (..)
  , SimEventType (..)
  , TraceEvent
  , ppTrace
  , ppTrace_
  , ppSimEvent
  , liftST
  , execReadTVar
  ) where

import           Prelude hiding (read)

import           Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.List.Trace as Trace
import           Data.Maybe (mapMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Dynamic

import           Control.Exception (NonTermination (..),
                   assert, throw)
import           Control.Monad (join)

import           Control.Monad (when)
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Lazy.Unsafe (unsafeIOToST, unsafeInterleaveST)
import           Data.STRef.Lazy

import           Control.Monad.Class.MonadSTM hiding (STM, TVar)
import           Control.Monad.Class.MonadThrow hiding (getMaskingState)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Control.Monad.IOSim.Types (SimEvent)
import           Control.Monad.IOSim.Types hiding (SimEvent (SimPOREvent),
                   Trace(SimPORTrace))
import           Control.Monad.IOSim.InternalTypes

--
-- Simulation interpreter
--

data Thread s a = Thread {
    threadId      :: !ThreadId,
    threadControl :: !(ThreadControl s a),
    threadBlocked :: !Bool,
    threadMasking :: !MaskingState,
    -- other threads blocked in a ThrowTo to us because we are or were masked
    threadThrowTo :: ![(SomeException, Labelled ThreadId)],
    threadClockId :: !ClockId,
    threadLabel   :: Maybe ThreadLabel,
    threadNextTId :: !Int
  }

labelledTVarId :: TVar s a -> ST s (Labelled TVarId)
labelledTVarId TVar { tvarId, tvarLabel } = (Labelled tvarId) <$> readSTRef tvarLabel

labelledThreads :: Map ThreadId (Thread s a) -> [Labelled ThreadId]
labelledThreads threadMap =
    -- @Map.foldr'@ (and alikes) are not strict enough, to not ratain the
    -- original thread map we need to evaluate the spine of the list.
    -- TODO: https://github.com/haskell/containers/issues/749
    Map.foldr'
      (\Thread { threadId, threadLabel } !acc -> Labelled threadId threadLabel : acc)
      [] threadMap


-- | Timers mutable variables.  First one supports 'newTimeout' api, the second
-- one 'registerDelay'.
--
data TimerVars s = TimerVars !(TVar s TimeoutState) !(TVar s Bool)


-- | Internal state.
--
data SimState s a = SimState {
       runqueue :: ![ThreadId],
       -- | All threads other than the currently running thread: both running
       -- and blocked threads.
       threads  :: !(Map ThreadId (Thread s a)),
       -- | current time
       curTime  :: !Time,
       -- | ordered list of timers
       timers   :: !(OrdPSQ TimeoutId Time (TimerVars s)),
       -- | list of clocks
       clocks   :: !(Map ClockId UTCTime),
       nextVid  :: !TVarId,     -- ^ next unused 'TVarId'
       nextTmid :: !TimeoutId   -- ^ next unused 'TimeoutId'
     }

initialState :: SimState s a
initialState =
    SimState {
      runqueue = [],
      threads  = Map.empty,
      curTime  = Time 0,
      timers   = PSQ.empty,
      clocks   = Map.singleton (ClockId []) epoch1970,
      nextVid  = TVarId 0,
      nextTmid = TimeoutId 0
    }
  where
    epoch1970 = UTCTime (fromGregorian 1970 1 1) 0

invariant :: Maybe (Thread s a) -> SimState s a -> Bool

invariant (Just running) simstate@SimState{runqueue,threads,clocks} =
    not (threadBlocked running)
 && threadId running `Map.notMember` threads
 && threadId running `List.notElem` runqueue
 && threadClockId running `Map.member` clocks
 && invariant Nothing simstate

invariant Nothing SimState{runqueue,threads,clocks} =
    all (`Map.member` threads) runqueue
 && and [ threadBlocked t == (threadId t `notElem` runqueue)
        | t <- Map.elems threads ]
 && runqueue == List.nub runqueue
 && and [ threadClockId t `Map.member` clocks
        | t <- Map.elems threads ]

-- | Interpret the simulation monotonic time as a 'NominalDiffTime' since
-- the start.
timeSinceEpoch :: Time -> NominalDiffTime
timeSinceEpoch (Time t) = fromRational (toRational t)


-- | Schedule / run a thread.
--
schedule :: Thread s a -> SimState s a -> ST s (SimTrace a)
schedule thread@Thread{
           threadId      = tid,
           threadControl = ThreadControl action ctl,
           threadMasking = maskst,
           threadLabel   = tlbl
         }
         simstate@SimState {
           runqueue,
           threads,
           timers,
           clocks,
           nextVid, nextTmid,
           curTime  = time
         } =
  assert (invariant (Just thread) simstate) $
  case action of

    Return x -> case ctl of
      MainFrame ->
        -- the main thread is done, so we're done
        -- even if other threads are still running
        return $ SimTrace time tid tlbl EventThreadFinished
               $ TraceMainReturn time x (labelledThreads threads)

      ForkFrame -> do
        -- this thread is done
        trace <- deschedule Terminated thread simstate
        return $ SimTrace time tid tlbl EventThreadFinished
               $ SimTrace time tid tlbl (EventDeschedule Terminated)
               $ trace

      MaskFrame k maskst' ctl' -> do
        -- pop the control stack, restore thread-local state
        let thread' = thread { threadControl = ThreadControl (k x) ctl'
                             , threadMasking = maskst' }
        -- but if we're now unmasked, check for any pending async exceptions
        trace <- deschedule Interruptable thread' simstate
        return $ SimTrace time tid tlbl (EventMask maskst')
               $ SimTrace time tid tlbl (EventDeschedule Interruptable)
               $ trace

      CatchFrame _handler k ctl' -> do
        -- pop the control stack and continue
        let thread' = thread { threadControl = ThreadControl (k x) ctl' }
        schedule thread' simstate

    Throw e -> case unwindControlStack e thread of
      Right thread'@Thread { threadMasking = maskst' } -> do
        -- We found a suitable exception handler, continue with that
        trace <- schedule thread' simstate
        return (SimTrace time tid tlbl (EventThrow e) $
                SimTrace time tid tlbl (EventMask maskst') trace)

      Left isMain
        -- We unwound and did not find any suitable exception handler, so we
        -- have an unhandled exception at the top level of the thread.
        | isMain ->
          -- An unhandled exception in the main thread terminates the program
          return (SimTrace time tid tlbl (EventThrow e) $
                  SimTrace time tid tlbl (EventThreadUnhandled e) $
                  TraceMainException time e (labelledThreads threads))

        | otherwise -> do
          -- An unhandled exception in any other thread terminates the thread
          trace <- deschedule Terminated thread simstate
          return $ SimTrace time tid tlbl (EventThrow e)
                 $ SimTrace time tid tlbl (EventThreadUnhandled e)
                 $ SimTrace time tid tlbl (EventDeschedule Terminated)
                 $ trace

    Catch action' handler k -> do
      -- push the failure and success continuations onto the control stack
      let thread' = thread { threadControl = ThreadControl action'
                                               (CatchFrame handler k ctl) }
      schedule thread' simstate

    Evaluate expr k -> do
      mbWHNF <- unsafeIOToST $ try $ evaluate expr
      case mbWHNF of
        Left e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
          schedule thread' simstate
        Right whnf -> do
          -- continue with the resulting WHNF
          let thread' = thread { threadControl = ThreadControl (k whnf) ctl }
          schedule thread' simstate

    Say msg k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventSay msg) trace)

    Output x k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventLog x) trace)

    LiftST st k -> do
      x <- strictToLazyST st
      let thread' = thread { threadControl = ThreadControl (k x) ctl }
      schedule thread' simstate

    GetMonoTime k -> do
      let thread' = thread { threadControl = ThreadControl (k time) ctl }
      schedule thread' simstate

    GetWallTime k -> do
      let clockid  = threadClockId thread
          clockoff = clocks Map.! clockid
          walltime = timeSinceEpoch time `addUTCTime` clockoff
          thread'  = thread { threadControl = ThreadControl (k walltime) ctl }
      schedule thread' simstate

    SetWallTime walltime' k -> do
      let clockid   = threadClockId thread
          clockoff  = clocks Map.! clockid
          walltime  = timeSinceEpoch time `addUTCTime` clockoff
          clockoff' = addUTCTime (diffUTCTime walltime' walltime) clockoff
          thread'   = thread { threadControl = ThreadControl k ctl }
          simstate' = simstate { clocks = Map.insert clockid clockoff' clocks }
      schedule thread' simstate'

    UnshareClock k -> do
      let clockid   = threadClockId thread
          clockoff  = clocks Map.! clockid
          clockid'  = let ThreadId i = tid in ClockId i -- reuse the thread id
          thread'   = thread { threadControl = ThreadControl k ctl
                             , threadClockId = clockid' }
          simstate' = simstate { clocks = Map.insert clockid' clockoff clocks }
      schedule thread' simstate'

    -- we treat negative timers as cancelled ones; for the record we put
    -- `EventTimerCreated` and `EventTimerCancelled` in the trace; This differs
    -- from `GHC.Event` behaviour.
    NewTimeout d k | d < 0 -> do
      let t       = NegativeTimeout nextTmid
          expiry  = d `addTime` time
          thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventTimerCreated nextTmid nextVid expiry) $
              SimTrace time tid tlbl (EventTimerCancelled nextTmid) $
              trace)

    NewTimeout d k -> do
      tvar  <- execNewTVar nextVid
                           (Just $ "<<timeout-state " ++ show (unTimeoutId nextTmid) ++ ">>")
                           TimeoutPending
      tvar' <- execNewTVar (succ nextVid)
                           (Just $ "<<timeout " ++ show (unTimeoutId nextTmid) ++ ">>")
                           False
      let expiry  = d `addTime` time
          t       = Timeout tvar tvar' nextTmid
          timers' = PSQ.insert nextTmid expiry (TimerVars tvar tvar') timers
          thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                         , nextVid  = succ (succ nextVid)
                                         , nextTmid = succ nextTmid }
      return (SimTrace time tid tlbl (EventTimerCreated nextTmid nextVid expiry) trace)

    -- we do not follow `GHC.Event` behaviour here; updating a timer to the past
    -- effectively cancels it.
    UpdateTimeout (Timeout _tvar _tvar' tmid) d k | d < 0 -> do
      let timers' = PSQ.delete tmid timers
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (SimTrace time tid tlbl (EventTimerCancelled tmid) trace)

    UpdateTimeout (Timeout _tvar _tvar' tmid) d k -> do
          -- updating an expired timeout is a noop, so it is safe
          -- to race using a timeout with updating or cancelling it
      let updateTimeout_  Nothing       = ((), Nothing)
          updateTimeout_ (Just (_p, v)) = ((), Just (expiry, v))
          expiry  = d `addTime` time
          timers' = snd (PSQ.alter updateTimeout_ tmid timers)
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (SimTrace time tid tlbl (EventTimerUpdated tmid expiry) trace)

    -- updating a negative timer is a no-op, unlike in `GHC.Event`.
    UpdateTimeout (NegativeTimeout _tmid) _d k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    CancelTimeout (Timeout tvar _tvar' tmid) k -> do
      let timers' = PSQ.delete tmid timers
          thread' = thread { threadControl = ThreadControl k ctl }
      written <- execAtomically' (runSTM $ writeTVar tvar TimeoutCancelled)
      (wakeup, wokeby) <- threadsUnblockedByWrites written
      mapM_ (\(SomeTVar var) -> unblockAllThreadsFromTVar var) written
      let (unblocked,
           simstate') = unblockThreads wakeup simstate
      trace <- schedule thread' simstate' { timers = timers' }
      return $ SimTrace time tid tlbl (EventTimerCancelled tmid)
             $ traceMany
                 [ (time, tid', tlbl', EventTxWakeup vids)
                 | tid' <- unblocked
                 , let tlbl' = lookupThreadLabel tid' threads
                 , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
             $ trace

    -- cancelling a negative timer is a no-op
    CancelTimeout (NegativeTimeout _tmid) k -> do
      -- negative timers are promptly removed from the state
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    Fork a k -> do
      let nextId   = threadNextTId thread
          tid'     = childThreadId tid nextId
          thread'  = thread { threadControl = ThreadControl (k tid') ctl
                            , threadNextTId = succ nextId }
          thread'' = Thread { threadId      = tid'
                            , threadControl = ThreadControl (runIOSim a)
                                                            ForkFrame
                            , threadBlocked = False
                            , threadMasking = threadMasking thread
                            , threadThrowTo = []
                            , threadClockId = threadClockId thread
                            , threadLabel   = Nothing
                            , threadNextTId = 1
                            }
          threads' = Map.insert tid' thread'' threads
      trace <- schedule thread' simstate { runqueue = runqueue ++ [tid']
                                         , threads  = threads' }
      return (SimTrace time tid tlbl (EventThreadForked tid') trace)

    Atomically a k -> execAtomically time tid tlbl nextVid (runSTM a) $ \res ->
      case res of
        StmTxCommitted x written _read created
                         tvarDynamicTraces tvarStringTraces nextVid' -> do
          (wakeup, wokeby) <- threadsUnblockedByWrites written
          mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written
          let thread'     = thread { threadControl = ThreadControl (k x) ctl }
              (unblocked,
               simstate') = unblockThreads wakeup simstate
          written' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) written
          created' <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) created
              -- We don't interrupt runnable threads to provide fairness
              -- anywhere else. We do it here by putting the tx that committed
              -- a transaction to the back of the runqueue, behind all other
              -- runnable threads, and behind the unblocked threads.
              -- For testing, we should have a more sophisticated policy to show
              -- that algorithms are not sensitive to the exact policy, so long
              -- as it is a fair policy (all runnable threads eventually run).
          trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
          return $ SimTrace time tid tlbl (EventTxCommitted
                                             written' created' Nothing)
                 $ traceMany
                     [ (time, tid', tlbl', EventTxWakeup vids')
                     | tid' <- unblocked
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids' = Set.toList <$> Map.lookup tid' wokeby ]
                 $ traceMany
                     [ (time, tid, tlbl, EventLog tr)
                     | tr <- tvarDynamicTraces ]
                 $ traceMany
                     [ (time, tid, tlbl, EventSay str)
                     | str <- tvarStringTraces ]
                 $ SimTrace time tid tlbl (EventUnblocked unblocked)
                 $ SimTrace time tid tlbl (EventDeschedule Yield)
                 $ trace

        StmTxAborted _read e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
          trace <- schedule thread' simstate
          return $ SimTrace time tid tlbl (EventTxAborted Nothing) trace

        StmTxBlocked read -> do
          mapM_ (\(SomeTVar tvar) -> blockThreadOnTVar tid tvar) read
          vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) read
          trace <- deschedule Blocked thread simstate
          return $ SimTrace time tid tlbl (EventTxBlocked vids Nothing)
                 $ SimTrace time tid tlbl (EventDeschedule Blocked)
                 $ trace

    GetThreadId k -> do
      let thread' = thread { threadControl = ThreadControl (k tid) ctl }
      schedule thread' simstate

    LabelThread tid' l k | tid' == tid -> do
      let thread' = thread { threadControl = ThreadControl k ctl
                           , threadLabel   = Just l }
      schedule thread' simstate

    LabelThread tid' l k -> do
      let thread'  = thread { threadControl = ThreadControl k ctl }
          threads' = Map.adjust (\t -> t { threadLabel = Just l }) tid' threads
      schedule thread' simstate { threads = threads' }

    GetMaskState k -> do
      let thread' = thread { threadControl = ThreadControl (k maskst) ctl }
      schedule thread' simstate

    SetMaskState maskst' action' k -> do
      let thread' = thread { threadControl = ThreadControl
                                               (runIOSim action')
                                               (MaskFrame k maskst ctl)
                           , threadMasking = maskst' }
      trace <-
        case maskst' of
          -- If we're now unmasked then check for any pending async exceptions
          Unmasked -> SimTrace time tid tlbl (EventDeschedule Interruptable)
                  <$> deschedule Interruptable thread' simstate
          _        -> schedule                 thread' simstate
      return $ SimTrace time tid tlbl (EventMask maskst')
             $ trace

    ThrowTo e tid' _ | tid' == tid -> do
      -- Throw to ourself is equivalent to a synchronous throw,
      -- and works irrespective of masking state since it does not block.
      let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventThrowTo e tid) trace)

    ThrowTo e tid' k -> do
      let thread'   = thread { threadControl = ThreadControl k ctl }
          willBlock = case Map.lookup tid' threads of
                        Just t -> not (threadInterruptible t)
                        _      -> False
      if willBlock
        then do
          -- The target thread has async exceptions masked so we add the
          -- exception and the source thread id to the pending async exceptions.
          let adjustTarget t = t { threadThrowTo = (e, Labelled tid tlbl) : threadThrowTo t }
              threads'       = Map.adjust adjustTarget tid' threads
          trace <- deschedule Blocked thread' simstate { threads = threads' }
          return $ SimTrace time tid tlbl (EventThrowTo e tid')
                 $ SimTrace time tid tlbl EventThrowToBlocked
                 $ SimTrace time tid tlbl (EventDeschedule Blocked)
                 $ trace
        else do
          -- The target thread has async exceptions unmasked, or is masked but
          -- is blocked (and all blocking operations are interruptible) then we
          -- raise the exception in that thread immediately. This will either
          -- cause it to terminate or enter an exception handler.
          -- In the meantime the thread masks new async exceptions. This will
          -- be resolved if the thread terminates or if it leaves the exception
          -- handler (when restoring the masking state would trigger the any
          -- new pending async exception).
          let adjustTarget t@Thread{ threadControl = ThreadControl _ ctl' } =
                t { threadControl = ThreadControl (Throw e) ctl'
                  , threadBlocked = False
                  }
              simstate'@SimState { threads = threads' }
                         = snd (unblockThreads [tid'] simstate)
              threads''  = Map.adjust adjustTarget tid' threads'
              simstate'' = simstate' { threads = threads'' }

          trace <- schedule thread' simstate''
          return $ SimTrace time tid tlbl (EventThrowTo e tid')
                 $ trace

    -- ExploreRaces is ignored by this simulator
    ExploreRaces k -> schedule thread{ threadControl = ThreadControl k ctl } simstate

    Fix f k -> do
      r <- newSTRef (throw NonTermination)
      x <- unsafeInterleaveST $ readSTRef r
      let k' = unIOSim (f x) $ \x' ->
                  LiftST (lazyToStrictST (writeSTRef r x')) (\() -> k x')
          thread' = thread { threadControl = ThreadControl k' ctl }
      schedule thread' simstate


threadInterruptible :: Thread s a -> Bool
threadInterruptible thread =
    case threadMasking thread of
      Unmasked                 -> True
      MaskedInterruptible
        | threadBlocked thread -> True  -- blocking operations are interruptible
        | otherwise            -> False
      MaskedUninterruptible    -> False

deschedule :: Deschedule -> Thread s a -> SimState s a -> ST s (SimTrace a)
deschedule Yield thread simstate@SimState{runqueue, threads} =

    -- We don't interrupt runnable threads to provide fairness anywhere else.
    -- We do it here by putting the thread to the back of the runqueue, behind
    -- all other runnable threads.
    --
    -- For testing, we should have a more sophisticated policy to show that
    -- algorithms are not sensitive to the exact policy, so long as it is a
    -- fair policy (all runnable threads eventually run).

    let runqueue' = runqueue ++ [threadId thread]
        threads'  = Map.insert (threadId thread) thread threads in
    reschedule simstate { runqueue = runqueue', threads  = threads' }

deschedule Interruptable thread@Thread {
                           threadId      = tid,
                           threadControl = ThreadControl _ ctl,
                           threadMasking = Unmasked,
                           threadThrowTo = (e, tid') : etids,
                           threadLabel   = tlbl
                         }
                         simstate@SimState{ curTime = time, threads } = do

    -- We're unmasking, but there are pending blocked async exceptions.
    -- So immediately raise the exception and unblock the blocked thread
    -- if possible.
    let thread' = thread { threadControl = ThreadControl (Throw e) ctl
                         , threadMasking = MaskedInterruptible
                         , threadThrowTo = etids }
        (unblocked,
         simstate') = unblockThreads [l_labelled tid'] simstate
    trace <- schedule thread' simstate'
    return $ SimTrace time tid tlbl (EventThrowToUnmasked tid')
           $ traceMany [ (time, tid'', tlbl'', EventThrowToWakeup)
                       | tid'' <- unblocked
                       , let tlbl'' = lookupThreadLabel tid'' threads ]
             trace

deschedule Interruptable thread simstate =
    -- Either masked or unmasked but no pending async exceptions.
    -- Either way, just carry on.
    schedule thread simstate

deschedule Blocked thread@Thread { threadThrowTo = _ : _
                                 , threadMasking = maskst } simstate
    | maskst /= MaskedUninterruptible =
    -- We're doing a blocking operation, which is an interrupt point even if
    -- we have async exceptions masked, and there are pending blocked async
    -- exceptions. So immediately raise the exception and unblock the blocked
    -- thread if possible.
    deschedule Interruptable thread { threadMasking = Unmasked } simstate

deschedule Blocked thread simstate@SimState{threads} =
    let thread'  = thread { threadBlocked = True }
        threads' = Map.insert (threadId thread') thread' threads in
    reschedule simstate { threads = threads' }

deschedule Terminated thread simstate@SimState{ curTime = time, threads } = do
    -- This thread is done. If there are other threads blocked in a
    -- ThrowTo targeted at this thread then we can wake them up now.
    let wakeup      = map (l_labelled . snd) (reverse (threadThrowTo thread))
        (unblocked,
         simstate') = unblockThreads wakeup simstate
    trace <- reschedule simstate'
    return $ traceMany
               [ (time, tid', tlbl', EventThrowToWakeup)
               | tid' <- unblocked
               , let tlbl' = lookupThreadLabel tid' threads ]
               trace

deschedule Sleep _thread _simstate =
    error "IOSim: impossible happend"

-- When there is no current running thread but the runqueue is non-empty then
-- schedule the next one to run.
reschedule :: SimState s a -> ST s (SimTrace a)
reschedule simstate@SimState{ runqueue = tid:runqueue', threads } =
    assert (invariant Nothing simstate) $

    let thread = threads Map.! tid in
    schedule thread simstate { runqueue = runqueue'
                             , threads  = Map.delete tid threads }

-- But when there are no runnable threads, we advance the time to the next
-- timer event, or stop.
reschedule simstate@SimState{ runqueue = [], threads, timers, curTime = time } =
    assert (invariant Nothing simstate) $

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return (TraceDeadlock time (labelledThreads threads))

      Just (tmids, time', fired, timers') -> assert (time' >= time) $ do

        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        written <- execAtomically' (runSTM $ mapM_ timeoutAction fired)
        (wakeup, wokeby) <- threadsUnblockedByWrites written
        mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written

        let (unblocked,
             simstate') = unblockThreads wakeup simstate
        trace <- reschedule simstate' { curTime = time'
                                      , timers  = timers' }
        return $
          traceMany ([ (time', ThreadId [-1], Just "timer", EventTimerExpired tmid)
                     | tmid <- tmids ]
                  ++ [ (time', tid', tlbl', EventTxWakeup vids)
                     | tid' <- unblocked
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids = Set.toList <$> Map.lookup tid' wokeby ])
                    trace
  where
    timeoutAction (TimerVars var bvar) = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var  TimeoutFired
                         >> writeTVar bvar True
        TimeoutFired     -> error "MonadTimer(Sim): invariant violation"
        TimeoutCancelled -> return ()

unblockThreads :: [ThreadId] -> SimState s a -> ([ThreadId], SimState s a)
unblockThreads wakeup simstate@SimState {runqueue, threads} =
    -- To preserve our invariants (that threadBlocked is correct)
    -- we update the runqueue and threads together here
    (unblocked, simstate {
                  runqueue = runqueue ++ unblocked,
                  threads  = threads'
                })
  where
    -- can only unblock if the thread exists and is blocked (not running)
    unblocked = [ tid
                | tid <- wakeup
                , case Map.lookup tid threads of
                       Just Thread { threadBlocked = True } -> True
                       _                                    -> False
                ]
    -- and in which case we mark them as now running
    threads'  = List.foldl'
                  (flip (Map.adjust (\t -> t { threadBlocked = False })))
                  threads unblocked


-- | Iterate through the control stack to find an enclosing exception handler
-- of the right type, or unwind all the way to the top level for the thread.
--
-- Also return if it's the main thread or a forked thread since we handle the
-- cases differently.
--
unwindControlStack :: forall s a.
                      SomeException
                   -> Thread s a
                   -> Either Bool (Thread s a)
unwindControlStack e thread =
    case threadControl thread of
      ThreadControl _ ctl -> unwind (threadMasking thread) ctl
  where
    unwind :: forall s' c. MaskingState
           -> ControlStack s' c a -> Either Bool (Thread s' a)
    unwind _  MainFrame                 = Left True
    unwind _  ForkFrame                 = Left False
    unwind _ (MaskFrame _k maskst' ctl) = unwind maskst' ctl

    unwind maskst (CatchFrame handler k ctl) =
      case fromException e of
        -- not the right type, unwind to the next containing handler
        Nothing -> unwind maskst ctl

        -- Ok! We will be able to continue the thread with the handler
        -- followed by the continuation after the catch
        Just e' -> Right thread {
                      -- As per async exception rules, the handler is run masked
                     threadControl = ThreadControl (handler e')
                                                   (MaskFrame k maskst ctl),
                     threadMasking = atLeastInterruptibleMask maskst
                   }

    atLeastInterruptibleMask :: MaskingState -> MaskingState
    atLeastInterruptibleMask Unmasked = MaskedInterruptible
    atLeastInterruptibleMask ms       = ms


removeMinimums :: (Ord k, Ord p)
               => OrdPSQ k p a
               -> Maybe ([k], p, [a], OrdPSQ k p a)
removeMinimums = \psq ->
    case PSQ.minView psq of
      Nothing              -> Nothing
      Just (k, p, x, psq') -> Just (collectAll [k] p [x] psq')
  where
    collectAll ks p xs psq =
      case PSQ.minView psq of
        Just (k, p', x, psq')
          | p == p' -> collectAll (k:ks) p (x:xs) psq'
        _           -> (reverse ks, p, reverse xs, psq)

traceMany :: [(Time, ThreadId, Maybe ThreadLabel, SimEventType)]
          -> SimTrace a -> SimTrace a
traceMany []                      trace = trace
traceMany ((time, tid, tlbl, event):ts) trace =
    SimTrace time tid tlbl event (traceMany ts trace)

lookupThreadLabel :: ThreadId -> Map ThreadId (Thread s a) -> Maybe ThreadLabel
lookupThreadLabel tid threads = join (threadLabel <$> Map.lookup tid threads)


-- | The most general method of running 'IOSim' is in 'ST' monad.  One can
-- recover failures or the result from 'SimTrace' with 'traceResult', or access
-- 'SimEventType's generated by the computation with 'traceEvents'.  A slightly
-- more convenient way is exposed by 'runSimTrace'.
--
runSimTraceST :: forall s a. IOSim s a -> ST s (SimTrace a)
runSimTraceST mainAction = schedule mainThread initialState
  where
    mainThread =
      Thread {
        threadId      = ThreadId [],
        threadControl = ThreadControl (runIOSim mainAction) MainFrame,
        threadBlocked = False,
        threadMasking = Unmasked,
        threadThrowTo = [],
        threadClockId = ClockId [],
        threadLabel   = Just "main",
        threadNextTId = 1
      }


--
-- Executing STM Transactions
--

execAtomically :: forall s a c.
                  Time
               -> ThreadId
               -> Maybe ThreadLabel
               -> TVarId
               -> StmA s a
               -> (StmTxResult s a -> ST s (SimTrace c))
               -> ST s (SimTrace c)
execAtomically time tid tlbl nextVid0 action0 k0 =
    go AtomicallyFrame Map.empty Map.empty [] [] nextVid0 action0
  where
    go :: forall b.
          StmStack s b a
       -> Map TVarId (SomeTVar s)  -- set of vars read
       -> Map TVarId (SomeTVar s)  -- set of vars written
       -> [SomeTVar s]             -- vars written in order (no dups)
       -> [SomeTVar s]             -- vars created in order
       -> TVarId                   -- var fresh name supply
       -> StmA s b
       -> ST s (SimTrace c)
    go ctl !read !written writtenSeq createdSeq !nextVid action = assert localInvariant $
                                                       case action of
      ReturnStm x -> case ctl of
        AtomicallyFrame -> do
          -- Trace each created TVar
          ds  <- traverse (\(SomeTVar tvar) -> traceTVarST tvar True) createdSeq
          -- Trace & commit each TVar
          ds' <- Map.elems <$> traverse
                    (\(SomeTVar tvar) -> do
                        tr <- traceTVarST tvar False
                        commitTVar tvar
                        -- Also assert the data invariant that outside a tx
                        -- the undo stack is empty:
                        undos <- readTVarUndos tvar
                        assert (null undos) $ return tr
                    ) written

          -- Return the vars written, so readers can be unblocked
          k0 $ StmTxCommitted x (reverse writtenSeq)
                                []
                                (reverse createdSeq)
                                (mapMaybe (\TraceValue { traceDynamic }
                                            -> toDyn <$> traceDynamic)
                                          $ ds ++ ds')
                                (mapMaybe traceString $ ds ++ ds')
                                nextVid

        OrElseLeftFrame _b k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
          -- Commit the TVars written in this sub-transaction that are also
          -- in the written set of the outer transaction
          traverse_ (\(SomeTVar tvar) -> commitTVar tvar)
                    (Map.intersection written writtenOuter)
          -- Merge the written set of the inner with the outer
          let written'    = Map.union written writtenOuter
              writtenSeq' = filter (\(SomeTVar tvar) ->
                                      tvarId tvar `Map.notMember` writtenOuter)
                                    writtenSeq
                         ++ writtenOuterSeq
          -- Skip the orElse right hand and continue with the k continuation
          go ctl' read written' writtenSeq' createdOuterSeq nextVid (k x)

        OrElseRightFrame k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
          -- Commit the TVars written in this sub-transaction that are also
          -- in the written set of the outer transaction
          traverse_ (\(SomeTVar tvar) -> commitTVar tvar)
                    (Map.intersection written writtenOuter)
          -- Merge the written set of the inner with the outer
          let written'    = Map.union written writtenOuter
              writtenSeq' = filter (\(SomeTVar tvar) ->
                                      tvarId tvar `Map.notMember` writtenOuter)
                                    writtenSeq
                         ++ writtenOuterSeq
              createdSeq' = createdSeq ++ createdOuterSeq
          -- Continue with the k continuation
          go ctl' read written' writtenSeq' createdSeq' nextVid (k x)

      ThrowStm e -> do
        -- Revert all the TVar writes
        traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
        k0 $ StmTxAborted [] (toException e)

      Retry -> case ctl of
        AtomicallyFrame -> do
          -- Revert all the TVar writes
          traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
          -- Return vars read, so the thread can block on them
          k0 $ StmTxBlocked (Map.elems read)

        OrElseLeftFrame b k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
          -- Revert all the TVar writes within this orElse
          traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
          -- Execute the orElse right hand with an empty written set
          let ctl'' = OrElseRightFrame k writtenOuter writtenOuterSeq createdOuterSeq ctl'
          go ctl'' read Map.empty [] [] nextVid b

        OrElseRightFrame _k writtenOuter writtenOuterSeq createdOuterSeq ctl' -> do
          -- Revert all the TVar writes within this orElse branch
          traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
          -- Skip the continuation and propagate the retry into the outer frame
          -- using the written set for the outer frame
          go ctl' read writtenOuter writtenOuterSeq createdOuterSeq nextVid Retry

      OrElse a b k -> do
        -- Execute the left side in a new frame with an empty written set
        let ctl' = OrElseLeftFrame b k written writtenSeq createdSeq ctl
        go ctl' read Map.empty [] [] nextVid a

      NewTVar !mbLabel x k -> do
        v <- execNewTVar nextVid mbLabel x
        go ctl read written writtenSeq (SomeTVar v : createdSeq) (succ nextVid) (k v)

      LabelTVar !label tvar k -> do
        writeSTRef (tvarLabel tvar) $! (Just label)
        go ctl read written writtenSeq createdSeq nextVid k

      TraceTVar tvar f k -> do
        writeSTRef (tvarTrace tvar) (Just f)
        go ctl read written writtenSeq createdSeq nextVid k

      ReadTVar v k
        | tvarId v `Map.member` read -> do
            x <- execReadTVar v
            go ctl read written writtenSeq createdSeq nextVid (k x)
        | otherwise -> do
            x <- execReadTVar v
            let read' = Map.insert (tvarId v) (SomeTVar v) read
            go ctl read' written writtenSeq createdSeq nextVid (k x)

      WriteTVar v x k
        | tvarId v `Map.member` written -> do
            execWriteTVar v x
            go ctl read written writtenSeq createdSeq nextVid k
        | otherwise -> do
            saveTVar v
            execWriteTVar v x
            let written' = Map.insert (tvarId v) (SomeTVar v) written
            go ctl read written' (SomeTVar v : writtenSeq) createdSeq nextVid k

      SayStm msg k -> do
        trace <- go ctl read written writtenSeq createdSeq nextVid k
        return $ SimTrace time tid tlbl (EventSay msg) trace

      OutputStm x k -> do
        trace <- go ctl read written writtenSeq createdSeq nextVid k
        return $ SimTrace time tid tlbl (EventLog x) trace

      where
        localInvariant =
            Map.keysSet written
         == Set.fromList [ tvarId tvar | SomeTVar tvar <- writtenSeq ]


-- | Special case of 'execAtomically' supporting only var reads and writes
--
execAtomically' :: StmA s () -> ST s [SomeTVar s]
execAtomically' = go Map.empty
  where
    go :: Map TVarId (SomeTVar s)  -- set of vars written
       -> StmA s ()
       -> ST s [SomeTVar s]
    go !written action = case action of
      ReturnStm () -> do
        traverse_ (\(SomeTVar tvar) -> commitTVar tvar) written
        return (Map.elems written)
      ReadTVar v k  -> do
        x <- execReadTVar v
        go written (k x)
      WriteTVar v x k
        | tvarId v `Map.member` written -> do
            execWriteTVar v x
            go written k
        | otherwise -> do
            saveTVar v
            execWriteTVar v x
            let written' = Map.insert (tvarId v) (SomeTVar v) written
            go written' k
      _ -> error "execAtomically': only for special case of reads and writes"


execNewTVar :: TVarId -> Maybe String -> a -> ST s (TVar s a)
execNewTVar nextVid !mbLabel x = do
    tvarLabel   <- newSTRef mbLabel
    tvarCurrent <- newSTRef x
    tvarUndo    <- newSTRef []
    tvarBlocked <- newSTRef ([], Set.empty)
    tvarVClock  <- newSTRef (VectorClock Map.empty)
    tvarTrace   <- newSTRef Nothing
    return TVar {tvarId = nextVid, tvarLabel,
                 tvarCurrent, tvarUndo, tvarBlocked, tvarVClock,
                 tvarTrace}

execReadTVar :: TVar s a -> ST s a
execReadTVar TVar{tvarCurrent} = readSTRef tvarCurrent

execWriteTVar :: TVar s a -> a -> ST s ()
execWriteTVar TVar{tvarCurrent} = writeSTRef tvarCurrent

saveTVar :: TVar s a -> ST s ()
saveTVar TVar{tvarCurrent, tvarUndo} = do
    -- push the current value onto the undo stack
    v  <- readSTRef tvarCurrent
    vs <- readSTRef tvarUndo
    writeSTRef tvarUndo (v:vs)

revertTVar :: TVar s a -> ST s ()
revertTVar TVar{tvarCurrent, tvarUndo} = do
    -- pop the undo stack, and revert the current value
    (v:vs) <- readSTRef tvarUndo
    writeSTRef tvarCurrent v
    writeSTRef tvarUndo    vs

commitTVar :: TVar s a -> ST s ()
commitTVar TVar{tvarUndo} = do
    -- pop the undo stack, leaving the current value unchanged
    (_:vs) <- readSTRef tvarUndo
    writeSTRef tvarUndo vs

readTVarUndos :: TVar s a -> ST s [a]
readTVarUndos TVar{tvarUndo} = readSTRef tvarUndo

-- | Trace a 'TVar'.  It must be called only on 'TVar's that were new or
-- 'written.
traceTVarST :: TVar s a
            -> Bool -- true if it's a new 'TVar'
            -> ST s TraceValue
traceTVarST TVar{tvarCurrent, tvarUndo, tvarTrace} new = do
    mf <- readSTRef tvarTrace
    case mf of
      Nothing -> return TraceValue { traceDynamic = (Nothing :: Maybe ())
                                   , traceString = Nothing }
      Just f  -> do
        vs <- readSTRef tvarUndo
        v <- readSTRef tvarCurrent
        case (new, vs) of
          (True, _) -> f Nothing v
          (_, _:_)  -> f (Just $ last vs) v
          _ -> error "traceTVarST: unexpected tvar state"



--
-- Blocking and unblocking on TVars
--

readTVarBlockedThreads :: TVar s a -> ST s [ThreadId]
readTVarBlockedThreads TVar{tvarBlocked} = fst <$> readSTRef tvarBlocked

blockThreadOnTVar :: ThreadId -> TVar s a -> ST s ()
blockThreadOnTVar tid TVar{tvarBlocked} = do
    (tids, tidsSet) <- readSTRef tvarBlocked
    when (tid `Set.notMember` tidsSet) $ do
      let !tids'    = tid : tids
          !tidsSet' = Set.insert tid tidsSet
      writeSTRef tvarBlocked (tids', tidsSet')

unblockAllThreadsFromTVar :: TVar s a -> ST s ()
unblockAllThreadsFromTVar TVar{tvarBlocked} = do
    writeSTRef tvarBlocked ([], Set.empty)

-- | For each TVar written to in a transaction (in order) collect the threads
-- that blocked on each one (in order).
--
-- Also, for logging purposes, return an association between the threads and
-- the var writes that woke them.
--
threadsUnblockedByWrites :: [SomeTVar s]
                         -> ST s ([ThreadId], Map ThreadId (Set (Labelled TVarId)))
threadsUnblockedByWrites written = do
  tidss <- sequence
             [ (,) <$> labelledTVarId tvar <*> readTVarBlockedThreads tvar
             | SomeTVar tvar <- written ]
  -- Threads to wake up, in wake up order, annotated with the vars written that
  -- caused the unblocking.
  -- We reverse the individual lists because the tvarBlocked is used as a stack
  -- so it is in order of last written, LIFO, and we want FIFO behaviour.
  let wakeup = ordNub [ tid | (_vid, tids) <- tidss, tid <- reverse tids ]
      wokeby = Map.fromListWith Set.union
                                [ (tid, Set.singleton vid)
                                | (vid, tids) <- tidss
                                , tid <- tids ]
  return (wakeup, wokeby)

ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go !_ [] = []
    go !s (x:xs)
      | x `Set.member` s = go s xs
      | otherwise        = x : go (Set.insert x s) xs
