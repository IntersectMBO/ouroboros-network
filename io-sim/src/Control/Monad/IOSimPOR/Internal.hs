{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving         #-}
 
{-# OPTIONS_GHC -Wno-orphans            #-}
-- incomplete uni patterns in 'schedule' (when interpreting 'StmTxCommitted')
-- and 'reschedule'.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-matches #-}

module Control.Monad.IOSimPOR.Internal
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
  , liftST
  , execReadTVar
  , controlSimTraceST
  , ScheduleControl (..)
  , ScheduleMod (..)
  ) where

import           Prelude hiding (read)

import           Data.Ord
import           Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.List.Trace as Trace
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)

import           Control.Exception (assert)
import           Control.Monad (join)

import           Control.Monad (when)
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Lazy.Unsafe (unsafeIOToST)
import           Data.STRef.Lazy

import           Control.Monad.Class.MonadSTM hiding (STM, TVar)
import           Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Control.Monad.IOSim.Types
import           Control.Monad.IOSim.InternalTypes
import           Control.Monad.IOSimPOR.Timeout(unsafeTimeout)

--
-- Simulation interpreter
--

data Thread s a = Thread {
    threadId      :: !ThreadId,
    threadControl :: !(ThreadControl s a),
    threadBlocked :: !Bool,
    threadDone    :: !Bool,
    threadMasking :: !MaskingState,
    -- other threads blocked in a ThrowTo to us because we are or were masked
    threadThrowTo :: ![(SomeException, Labelled ThreadId, VectorClock)],
    threadClockId :: !ClockId,
    threadLabel   :: Maybe ThreadLabel,
    threadNextTId :: !Int,
    threadStep    :: !Int,
    threadVClock  :: VectorClock,
    threadEffect  :: Effect,  -- in the current step
    threadRacy    :: !Bool
  }
  deriving Show

isTestThreadId :: ThreadId -> Bool
isTestThreadId (TestThreadId _) = True
isTestThreadId _                = False

bottomVClock :: VectorClock
bottomVClock = VectorClock Map.empty

insertVClock :: ThreadId -> Int -> VectorClock -> VectorClock
insertVClock tid !step (VectorClock m) = VectorClock (Map.insert tid step m)

lubVClock :: VectorClock -> VectorClock -> VectorClock
lubVClock (VectorClock m) (VectorClock m') = VectorClock (Map.unionWith max m m')

-- hbfVClock :: VectorClock -> VectorClock -> Bool
-- hbfVClock (VectorClock m) (VectorClock m') = Map.isSubmapOfBy (<=) m m'

happensBeforeStep :: Step -- ^ an earlier step
                  -> Step -- ^ a later step
                  -> Bool
happensBeforeStep step step' =
       Just (stepStep step)
    <= Map.lookup (stepThreadId step)
                  (getVectorClock $ stepVClock step')

labelledTVarId :: TVar s a -> ST s (Labelled TVarId)
labelledTVarId TVar { tvarId, tvarLabel } = Labelled tvarId <$> readSTRef tvarLabel

labelledThreads :: Map ThreadId (Thread s a) -> [Labelled ThreadId]
labelledThreads threadMap =
    -- @Map.foldr'@ (and alikes) are not strict enough, to not retain the
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
       nextTmid :: !TimeoutId,  -- ^ next unused 'TimeoutId'
       -- | previous steps (which we may race with).
       -- Note this is *lazy*, so that we don't compute races we will not reverse.
       races    :: Races,
       -- | control the schedule followed, and initial value
       control  :: !ScheduleControl,
       control0 :: !ScheduleControl,
       -- | limit on the computation time allowed per scheduling step, for
       -- catching infinite loops etc
       perStepTimeLimit :: Maybe Int

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
      nextTmid = TimeoutId 0,
      races    = noRaces,
      control  = ControlDefault,
      control0 = ControlDefault,
      perStepTimeLimit = Nothing
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
 && and [ (threadBlocked t || threadDone t) == (threadId t `notElem` runqueue)
        | t <- Map.elems threads ]
 && and (zipWith (>) runqueue (drop 1 runqueue))
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
           threadLabel   = tlbl,
           threadStep    = tstep,
           threadVClock  = vClock,
           threadEffect  = effect
         }
         simstate@SimState {
           runqueue,
           threads,
           timers,
           clocks,
           nextVid, nextTmid,
           curTime  = time,
           control,
           perStepTimeLimit
         }

  | controlTargets (tid,tstep) control =
      -- The next step is to be delayed according to the
      -- specified schedule. Switch to following the schedule.
      --Debug.trace ("Triggering control: "++show control++"\n") $
      schedule thread simstate{ control = followControl control }

  | not $ controlFollows (tid,tstep) control =
      -- the control says this is not the next step to
      -- follow. We should be at the beginning of a step;
      -- we put the present thread to sleep and reschedule
      -- the correct thread.
      assert (effect == mempty) $
      --Debug.trace ("Switching away from "++show (tid,tstep)++"\n"++
      --             "  control = "++show control++"\n") $
      deschedule Sleep thread simstate

  | otherwise =
  assert (invariant (Just thread) simstate) $
  case control of
    ControlFollow (s:_) _ ->
      id --Debug.trace ("Performing action in step "++show s++"\n")
    _ -> id
  $
  -- The next line forces the evaluation of action, which should be unevaluated up to
  -- this point. This is where we actually *run* user code.
  case maybe Just unsafeTimeout perStepTimeLimit action of
   Nothing -> return TraceLoop
   Just _  -> case action of

    Return x -> case ctl of
      MainFrame ->
        -- the main thread is done, so we're done
        -- even if other threads are still running
        return $ SimTrace time tid tlbl EventThreadFinished
               $ traceFinalRacesFound simstate
               $ TraceMainReturn time x (labelledThreads threads)

      ForkFrame -> do
        -- this thread is done
        trace <- deschedule Terminated thread simstate
        return $ SimTrace time tid tlbl EventThreadFinished trace

      MaskFrame k maskst' ctl' -> do
        -- pop the control stack, restore thread-local state
        let thread' = thread { threadControl = ThreadControl (k x) ctl'
                             , threadMasking = maskst' }
        -- but if we're now unmasked, check for any pending async exceptions
        deschedule Interruptable thread' simstate

      CatchFrame _handler k ctl' -> do
        -- pop the control stack and continue
        let thread' = thread { threadControl = ThreadControl (k x) ctl' }
        schedule thread' simstate

    Throw e -> case unwindControlStack e thread of
      Right thread0 -> do
        -- We found a suitable exception handler, continue with that
        -- We record a step, in case there is no exception handler on replay.
        let thread'  = stepThread thread0
            control' = advanceControl (threadStepId thread0) control
            races'   = updateRacesInSimState thread0 simstate
        trace <- schedule thread' simstate{ races = races', control = control' }
        return (SimTrace time tid tlbl (EventThrow e) trace)

      Left isMain
        -- We unwound and did not find any suitable exception handler, so we
        -- have an unhandled exception at the top level of the thread.
        | isMain ->
          -- An unhandled exception in the main thread terminates the program
          return (SimTrace time tid tlbl (EventThrow e) $
                  SimTrace time tid tlbl (EventThreadUnhandled e) $
                  traceFinalRacesFound simstate $
                  TraceMainException time e (labelledThreads threads))

        | otherwise -> do
          -- An unhandled exception in any other thread terminates the thread
          trace <- deschedule Terminated thread simstate
          return (SimTrace time tid tlbl (EventThrow e) $
                  SimTrace time tid tlbl (EventThreadUnhandled e) trace)

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
      let thread' = thread { threadControl = ThreadControl (k x) ctl,
                             threadEffect  = effect <> liftSTEffect }
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

    CancelTimeout (Timeout _tvar _tvar' tmid) k -> do
      let timers' = PSQ.delete tmid timers
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (SimTrace time tid tlbl (EventTimerCancelled tmid) trace)

    -- cancelling a negative timer is a no-op
    CancelTimeout (NegativeTimeout _tmid) k -> do
      -- negative timers are promptly removed from the state
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    Fork a k -> do
      let nextTId = threadNextTId thread
          tid' | threadRacy thread = setNonTestThread $ childThreadId tid nextTId
               | otherwise         = childThreadId tid nextTId
          thread'  = thread { threadControl = ThreadControl (k tid') ctl,
                              threadNextTId = nextTId + 1,
                              threadEffect  = effect <> forkEffect tid' }
          thread'' = Thread { threadId      = tid'
                            , threadControl = ThreadControl (runIOSim a)
                                                            ForkFrame
                            , threadBlocked = False
                            , threadDone    = False
                            , threadMasking = threadMasking thread
                            , threadThrowTo = []
                            , threadClockId = threadClockId thread
                            , threadLabel   = Nothing
                            , threadNextTId = 1
                            , threadStep    = 0
                            , threadVClock  = insertVClock tid' 0 vClock
                            , threadEffect  = mempty
                            , threadRacy    = threadRacy thread
                            }
          threads' = Map.insert tid' thread'' threads
      -- A newly forked thread may have a higher priority, so we deschedule this one.
      trace <- deschedule Yield thread'
                 simstate { runqueue = List.insertBy (comparing Down) tid' runqueue
                          , threads  = threads' }
      return (SimTrace time tid tlbl (EventThreadForked tid') trace)

    Atomically a k -> execAtomically time tid tlbl nextVid (runSTM a) $ \res ->
      case res of
        StmTxCommitted x read written nextVid' -> do
          (wakeup, wokeby) <- threadsUnblockedByWrites written
          mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written
          vClockRead <- lubTVarVClocks read
          let vClock'     = vClock `lubVClock` vClockRead
              effect'     = effect
                         <> readEffects read
                         <> writeEffects written
                         <> wakeupEffects unblocked
              thread'     = thread { threadControl = ThreadControl (k x) ctl,
                                     threadVClock  = vClock',
                                     threadEffect  = effect' }
              (unblocked,
               simstate') = unblockThreads vClock' wakeup simstate
          sequence_ [ modifySTRef (tvarVClock r) (lubVClock vClock') | SomeTVar r <- written ]
          vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) written
              -- We deschedule a thread after a transaction... another may have woken up.
          trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
          return $
            SimTrace time tid tlbl (EventTxCommitted vids [nextVid..pred nextVid']) $
            traceMany
              [ (time, tid', tlbl', EventTxWakeup vids')
              | tid' <- unblocked
              , let tlbl' = lookupThreadLabel tid' threads
              , let Just vids' = Set.toList <$> Map.lookup tid' wokeby ]
              trace

        StmTxAborted read e -> do
          -- schedule this thread to immediately raise the exception
          vClockRead <- lubTVarVClocks read
          let effect' = effect <> readEffects read
              thread' = thread { threadControl = ThreadControl (Throw e) ctl,
                                 threadVClock  = vClock `lubVClock` vClockRead,
                                 threadEffect  = effect' }
          trace <- schedule thread' simstate
          return $ SimTrace time tid tlbl EventTxAborted trace

        StmTxBlocked read -> do
          mapM_ (\(SomeTVar tvar) -> blockThreadOnTVar tid tvar) read
          vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) read
          vClockRead <- lubTVarVClocks read
          let effect' = effect <> readEffects read
              thread' = thread { threadVClock  = vClock `lubVClock` vClockRead,
                                 threadEffect  = effect' }
          trace <- deschedule Blocked thread' simstate
          return $ SimTrace time tid tlbl (EventTxBlocked vids) trace

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

    ExploreRaces k -> do
      let thread'  = thread { threadControl = ThreadControl k ctl
                            , threadRacy    = True }
      schedule thread' simstate

    GetMaskState k -> do
      let thread' = thread { threadControl = ThreadControl (k maskst) ctl }
      schedule thread' simstate

    SetMaskState maskst' action' k -> do
      let thread' = thread { threadControl = ThreadControl
                                               (runIOSim action')
                                               (MaskFrame k maskst ctl)
                           , threadMasking = maskst' }
      case maskst' of
        -- If we're now unmasked then check for any pending async exceptions
        Unmasked -> deschedule Interruptable thread' simstate
        _        -> schedule                 thread' simstate

    ThrowTo e tid' _ | tid' == tid -> do
      -- Throw to ourself is equivalent to a synchronous throw,
      -- and works irrespective of masking state since it does not block.
      let thread' = thread { threadControl = ThreadControl (Throw e) ctl
                           , threadMasking = MaskedInterruptible }
      trace <- schedule thread' simstate
      return (SimTrace time tid tlbl (EventThrowTo e tid) trace)

    ThrowTo e tid' k -> do
      let thread'    = thread { threadControl = ThreadControl k ctl,
                                threadEffect  = effect <> throwToEffect tid' <> wakeUpEffect,
                                threadVClock  = vClock `lubVClock` vClockTgt }
          (vClockTgt,
           wakeUpEffect,
           willBlock) = (threadVClock t,
                         if threadBlocked t then wakeupEffects [tid'] else mempty,
                         not (threadInterruptible t || threadDone t))
            where Just t = Map.lookup tid' threads

      if willBlock
        then do
          -- The target thread has async exceptions masked so we add the
          -- exception and the source thread id to the pending async exceptions.
          let adjustTarget t =
                t { threadThrowTo = (e, Labelled tid tlbl, vClock) : threadThrowTo t }
              threads'       = Map.adjust adjustTarget tid' threads
          trace <- deschedule Blocked thread' simstate { threads = threads' }
          return $ SimTrace time tid tlbl (EventThrowTo e tid')
                 $ SimTrace time tid tlbl EventThrowToBlocked
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
          let adjustTarget t@Thread{ threadControl = ThreadControl _ ctl',
                                     threadVClock  = vClock' } =
                t { threadControl = ThreadControl (Throw e) ctl'
                  , threadBlocked = False
                  , threadMasking = MaskedInterruptible
                  , threadVClock  = vClock' `lubVClock` vClock }
              simstate'@SimState { threads = threads' }
                         = snd (unblockThreads vClock [tid'] simstate)
              threads''  = Map.adjust adjustTarget tid' threads'
              simstate'' = simstate' { threads = threads'' }

          -- We yield at this point because the target thread may be higher
          -- priority, so this should be a step for race detection.
          trace <- deschedule Yield thread' simstate''
          return $ SimTrace time tid tlbl (EventThrowTo e tid')
                 $ trace


threadInterruptible :: Thread s a -> Bool
threadInterruptible thread =
    case threadMasking thread of
      Unmasked                 -> True
      MaskedInterruptible
        | threadBlocked thread -> True  -- blocking operations are interruptible
        | otherwise            -> False
      MaskedUninterruptible    -> False

data Deschedule = Yield | Interruptable | Blocked | Terminated | Sleep

deschedule :: Deschedule -> Thread s a -> SimState s a -> ST s (SimTrace a)

deschedule Yield thread@Thread { threadId     = tid }
                 simstate@SimState{runqueue, threads, control} =

    -- We don't interrupt runnable threads anywhere else.
    -- We do it here by inserting the current thread into the runqueue in priority order.

    let thread'   = stepThread thread
        runqueue' = List.insertBy (comparing Down) tid runqueue
        threads'  = Map.insert tid thread' threads
        control'  = advanceControl (threadStepId thread) control in
    reschedule simstate { runqueue = runqueue', threads  = threads',
                          races    = updateRacesInSimState thread simstate,
                          control  = control' }

deschedule Interruptable thread@Thread {
                           threadId      = tid,
                           threadControl = ThreadControl _ ctl,
                           threadMasking = Unmasked,
                           threadThrowTo = (e, tid', vClock') : etids,
                           threadLabel   = tlbl,
                           threadVClock  = vClock
                         }
                         simstate@SimState{ curTime = time, threads } = do

    -- We're unmasking, but there are pending blocked async exceptions.
    -- So immediately raise the exception and unblock the blocked thread
    -- if possible.
    let thread' = thread { threadControl = ThreadControl (Throw e) ctl
                         , threadMasking = MaskedInterruptible
                         , threadThrowTo = etids
                         , threadVClock  = vClock `lubVClock` vClock' }
        (unblocked,
         simstate') = unblockThreads vClock [l_labelled tid'] simstate
    -- the thread is stepped when we Yield
    trace <- deschedule Yield thread' simstate'
    return $ SimTrace time tid tlbl (EventThrowToUnmasked tid')
           $ traceMany [ (time, tid'', tlbl'', EventThrowToWakeup)
                       | tid'' <- unblocked
                       , let tlbl'' = lookupThreadLabel tid'' threads ]
             trace

deschedule Interruptable thread simstate@SimState{ control } =
    -- Either masked or unmasked but no pending async exceptions.
    -- Either way, just carry on.
    -- Record a step, though, in case on replay there is an async exception.
    let thread' = stepThread thread in
    schedule thread'
             simstate{ races   = updateRacesInSimState thread simstate,
                       control = advanceControl (threadStepId thread) control }

deschedule Blocked thread@Thread { threadThrowTo = _ : _
                                 , threadMasking = maskst } simstate
    | maskst /= MaskedUninterruptible =
    -- We're doing a blocking operation, which is an interrupt point even if
    -- we have async exceptions masked, and there are pending blocked async
    -- exceptions. So immediately raise the exception and unblock the blocked
    -- thread if possible.
    deschedule Interruptable thread { threadMasking = Unmasked } simstate

deschedule Blocked thread simstate@SimState{threads, control} =
    let thread1 = thread { threadBlocked = True }
        thread'  = stepThread $ thread1
        threads' = Map.insert (threadId thread') thread' threads in
    reschedule simstate { threads = threads',
                          races   = updateRacesInSimState thread1 simstate,
                          control = advanceControl (threadStepId thread1) control }

deschedule Terminated thread@Thread { threadId = tid, threadVClock = vClock }
                      simstate@SimState{ curTime = time, control } = do
    -- This thread is done. If there are other threads blocked in a
    -- ThrowTo targeted at this thread then we can wake them up now.
    let thread'     = stepThread $ thread{ threadDone = True }
        wakeup      = map (\(_,tid',_) -> l_labelled tid') (reverse (threadThrowTo thread))
        (unblocked,
         simstate'@SimState{threads}) =
                      unblockThreads vClock wakeup simstate
        threads'    = Map.insert tid thread' threads
    -- We must keep terminated threads in the state to preserve their vector clocks,
    -- which matters when other threads throwTo them.
    trace <- reschedule simstate' { races = threadTerminatesRaces tid $
                                              updateRacesInSimState thread simstate,
                                    control = advanceControl (threadStepId thread) control,
                                    threads = threads' }
    return $ traceMany
               [ (time, tid', tlbl', EventThrowToWakeup)
               | tid' <- unblocked
               , let tlbl' = lookupThreadLabel tid' threads ]
               trace

deschedule Sleep thread@Thread { threadId = tid }
                 simstate@SimState{runqueue, threads} =

    -- Schedule control says we should run a different thread. Put
    -- this one to sleep without recording a step.

    let runqueue' = List.insertBy (comparing Down) tid runqueue
        threads'  = Map.insert tid thread threads in
    reschedule simstate { runqueue = runqueue', threads  = threads' }


-- Choose the next thread to run.
reschedule :: SimState s a -> ST s (SimTrace a)

-- If we are following a controlled schedule, just do that.
reschedule simstate@SimState{ runqueue, threads,
                              control=control@(ControlFollow ((tid,tstep):_) _)
                              } =
    assert (tid `elem` runqueue) $
    assert (tid `Map.member` threads) $
    assert (invariant Nothing simstate) $
    let thread = threads Map.! tid in
    assert (threadId thread == tid) $
    --assert (threadStep thread == tstep) $
    if threadStep thread /= tstep then
      error $ "Thread step out of sync\n"
           ++ "  runqueue:    "++show runqueue++"\n"
           ++ "  follows:     "++show tid++", step "++show tstep++"\n"
           ++ "  actual step: "++show (threadStep thread)++"\n"
           ++ "Thread:\n" ++ show thread ++ "\n"
    else
    schedule thread simstate { runqueue = List.delete tid runqueue
                             , threads  = Map.delete tid threads }

-- When there is no current running thread but the runqueue is non-empty then
-- schedule the next one to run.
reschedule simstate@SimState{ runqueue = tid:runqueue', threads } =
    assert (invariant Nothing simstate) $

    let thread = threads Map.! tid in
    schedule thread simstate { runqueue = runqueue'
                             , threads  = Map.delete tid threads }

-- But when there are no runnable threads, we advance the time to the next
-- timer event, or stop.
reschedule simstate@SimState{ runqueue = [], threads, timers, curTime = time, races } =
    assert (invariant Nothing simstate) $

    -- time is moving on
    --Debug.trace ("Rescheduling at "++show time++", "++
      --show (length (concatMap stepInfoRaces (activeRaces races++completeRaces races)))++" races") $

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return (traceFinalRacesFound simstate $
                         TraceDeadlock time (labelledThreads threads))

      Just (tmids, time', fired, timers') -> assert (time' >= time) $ do

        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        written <- execAtomically' (runSTM $ mapM_ timeoutAction fired)
        (wakeup, wokeby) <- threadsUnblockedByWrites written
        mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written

        -- TODO: the vector clock below cannot be right, can it?
        let (unblocked,
             simstate') = unblockThreads bottomVClock wakeup simstate
            -- all open races will be completed and reported at this time
            simstate''  = simstate'{ races = noRaces }
        trace <- reschedule simstate'' { curTime = time'
                                       , timers  = timers' }
        let traceEntries =
                     [ (time', ThreadId [-1], Just "timer", EventTimerExpired tmid)
                     | tmid <- tmids ]
                  ++ [ (time', tid', tlbl', EventTxWakeup vids)
                     | tid' <- unblocked
                     , let tlbl' = lookupThreadLabel tid' threads
                     , let Just vids = Set.toList <$> Map.lookup tid' wokeby ]
        return $
          traceFinalRacesFound simstate $
          traceMany traceEntries trace
  where
    timeoutAction (TimerVars var bvar) = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var  TimeoutFired
                         >> writeTVar bvar True
        TimeoutFired     -> error "MonadTimer(Sim): invariant violation"
        TimeoutCancelled -> return ()

unblockThreads :: VectorClock -> [ThreadId] -> SimState s a -> ([ThreadId], SimState s a)
unblockThreads vClock wakeup simstate@SimState {runqueue, threads} =
    -- To preserve our invariants (that threadBlocked is correct)
    -- we update the runqueue and threads together here
    (unblocked, simstate {
                  runqueue = foldr (List.insertBy (comparing Down)) runqueue unblocked,
                  threads  = threads'
                })
  where
    -- can only unblock if the thread exists and is blocked (not running)
    unblocked = [ tid
                | tid <- wakeup
                , case Map.lookup tid threads of
                       Just Thread { threadDone    = True } -> False
                       Just Thread { threadBlocked = True } -> True
                       _                                    -> False
                ]
    -- and in which case we mark them as now running
    threads'  = List.foldl'
                  (flip (Map.adjust
                    (\t -> t { threadBlocked = False,
                               threadVClock = vClock `lubVClock` threadVClock t })))
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
traceMany []                            trace = trace
traceMany ((time, tid, tlbl, event):ts) trace =
    SimTrace time tid tlbl event (traceMany ts trace)

lookupThreadLabel :: ThreadId -> Map ThreadId (Thread s a) -> Maybe ThreadLabel
lookupThreadLabel tid threads = join (threadLabel <$> Map.lookup tid threads)


-- | The most general method of running 'IOSim' is in 'ST' monad.  One can
-- recover failures or the result from 'SimTrace' with 'traceResult', or access
-- 'TraceEvent's generated by the computation with 'traceEvents'.  A slightly
-- more convenient way is exposed by 'runSimTrace'.
--
runSimTraceST :: forall s a. IOSim s a -> ST s (SimTrace a)
runSimTraceST mainAction = controlSimTraceST Nothing ControlDefault mainAction

controlSimTraceST :: Maybe Int -> ScheduleControl -> IOSim s a -> ST s (SimTrace a)
controlSimTraceST limit control mainAction =
  schedule mainThread initialState { control  = control,
                                     control0 = control,
                                     perStepTimeLimit = limit
                                   }
  where
    mainThread =
      Thread {
        threadId      = TestThreadId [],
        threadControl = ThreadControl (runIOSim mainAction) MainFrame,
        threadBlocked = False,
        threadDone    = False,
        threadMasking = Unmasked,
        threadThrowTo = [],
        threadClockId = ClockId [],
        threadLabel   = Just "main",
        threadNextTId = 1,
        threadStep    = 0,
        threadVClock  = insertVClock (TestThreadId []) 0 bottomVClock,
        threadEffect  = mempty,
        threadRacy    = False
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
    go AtomicallyFrame Map.empty Map.empty [] nextVid0 action0
  where
    go :: forall b.
          StmStack s b a
       -> Map TVarId (SomeTVar s)  -- set of vars read
       -> Map TVarId (SomeTVar s)  -- set of vars written
       -> [SomeTVar s]             -- vars written in order (no dups)
       -> TVarId                   -- var fresh name supply
       -> StmA s b
       -> ST s (SimTrace c)
    go ctl !read !written writtenSeq !nextVid action = assert localInvariant $
                                                       case action of
      ReturnStm x -> case ctl of
        AtomicallyFrame -> do
          -- Commit each TVar
          traverse_ (\(SomeTVar tvar) -> do
                        commitTVar tvar
                        -- Also assert the data invariant that outside a tx
                        -- the undo stack is empty:
                        undos <- readTVarUndos tvar
                        assert (null undos) $ return ()
                    ) written

          -- Return the vars written, so readers can be unblocked
          k0 $ StmTxCommitted x (Map.elems read)
                                (reverse writtenSeq) nextVid

        OrElseLeftFrame _b k writtenOuter writtenOuterSeq ctl' -> do
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
          go ctl' read written' writtenSeq' nextVid (k x)

        OrElseRightFrame k writtenOuter writtenOuterSeq ctl' -> do
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
          -- Continue with the k continuation
          go ctl' read written' writtenSeq' nextVid (k x)

      ThrowStm e -> do
        -- Revert all the TVar writes
        traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
        k0 $ StmTxAborted (Map.elems read) (toException e)

      Retry -> case ctl of
        AtomicallyFrame -> do
          -- Revert all the TVar writes
          traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
          -- Return vars read, so the thread can block on them
          k0 $ StmTxBlocked (Map.elems read)

        OrElseLeftFrame b k writtenOuter writtenOuterSeq ctl' -> do
          -- Revert all the TVar writes within this orElse
          traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
          -- Execute the orElse right hand with an empty written set
          let ctl'' = OrElseRightFrame k writtenOuter writtenOuterSeq ctl'
          go ctl'' read Map.empty [] nextVid b

        OrElseRightFrame _k writtenOuter writtenOuterSeq ctl' -> do
          -- Revert all the TVar writes within this orElse branch
          traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
          -- Skip the continuation and propagate the retry into the outer frame
          -- using the written set for the outer frame
          go ctl' read writtenOuter writtenOuterSeq nextVid Retry

      OrElse a b k -> do
        -- Execute the left side in a new frame with an empty written set
        let ctl' = OrElseLeftFrame b k written writtenSeq ctl
        go ctl' read Map.empty [] nextVid a

      NewTVar !mbLabel x k -> do
        v <- execNewTVar nextVid mbLabel x
        -- record a write to the TVar so we know to update its VClock
        let written' = Map.insert (tvarId v) (SomeTVar v) written
        -- save the value: it will be committed or reverted
        saveTVar v
        go ctl read written' (SomeTVar v : writtenSeq) (succ nextVid) (k v)

      LabelTVar !label tvar k -> do
        writeSTRef (tvarLabel tvar) $! (Just label)
        go ctl read written writtenSeq nextVid k

      ReadTVar v k
        | tvarId v `Map.member` read || tvarId v `Map.member` written -> do
            x <- execReadTVar v
            go ctl read written writtenSeq nextVid (k x)
        | otherwise -> do
            x <- execReadTVar v
            let read' = Map.insert (tvarId v) (SomeTVar v) read
            go ctl read' written writtenSeq nextVid (k x)

      WriteTVar v x k
        | tvarId v `Map.member` written -> do
            execWriteTVar v x
            go ctl read written writtenSeq nextVid k
        | otherwise -> do
            saveTVar v
            execWriteTVar v x
            let written' = Map.insert (tvarId v) (SomeTVar v) written
            go ctl read written' (SomeTVar v : writtenSeq) nextVid k

      SayStm msg k -> do
        trace <- go ctl read written writtenSeq nextVid k
        return $ SimTrace time tid tlbl (EventSay msg) trace

      OutputStm x k -> do
        trace <- go ctl read written writtenSeq nextVid k
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
    tvarVClock  <- newSTRef bottomVClock
    return TVar {tvarId = nextVid, tvarLabel,
                 tvarCurrent, tvarUndo, tvarBlocked, tvarVClock}

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

lubTVarVClocks :: [SomeTVar s] -> ST s VectorClock
lubTVarVClocks tvars =
  foldr lubVClock bottomVClock <$>
    sequence [readSTRef (tvarVClock r) | SomeTVar r <- tvars]

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

-- Effects

data Effect = Effect {
    effectReads  :: !(Set TVarId),
    effectWrites :: !(Set TVarId),
    effectForks  :: !(Set ThreadId),
    effectLiftST :: !Bool,
    effectThrows :: ![ThreadId],
    effectWakeup :: ![ThreadId]
  }
  deriving (Eq, Show)

instance Semigroup Effect where
  Effect r w s b ts wu <> Effect r' w' s' b' ts' wu' =
    Effect (r<>r') (w<>w') (s<>s') (b||b') (ts++ts') (wu++wu')

instance Monoid Effect where
  mempty = Effect Set.empty Set.empty Set.empty False [] []

-- readEffect :: SomeTVar s -> Effect
-- readEffect r = mempty{effectReads = Set.singleton $ someTvarId r }

readEffects :: [SomeTVar s] -> Effect
readEffects rs = mempty{effectReads = Set.fromList (map someTvarId rs)}

-- writeEffect :: SomeTVar s -> Effect
-- writeEffect r = mempty{effectWrites = Set.singleton $ someTvarId r }

writeEffects :: [SomeTVar s] -> Effect
writeEffects rs = mempty{effectWrites = Set.fromList (map someTvarId rs)}

forkEffect :: ThreadId -> Effect
forkEffect tid = mempty{effectForks = Set.singleton $ tid}

liftSTEffect :: Effect
liftSTEffect = mempty{ effectLiftST = True }

throwToEffect :: ThreadId -> Effect
throwToEffect tid = mempty{ effectThrows = [tid] }

wakeupEffects :: [ThreadId] -> Effect
wakeupEffects tids = mempty{effectWakeup = tids}

someTvarId :: SomeTVar s -> TVarId
someTvarId (SomeTVar r) = tvarId r

onlyReadEffect :: Effect -> Bool
onlyReadEffect e = e { effectReads = effectReads mempty } == mempty

racingEffects :: Effect -> Effect -> Bool
racingEffects e e' =
      (effectLiftST e  && racesWithLiftST e')
   || (effectLiftST e' && racesWithLiftST e )
   || (not $ null $ effectThrows e `List.intersect` effectThrows e')
   || (not $ effectReads  e `Set.disjoint` effectWrites e'
          && effectWrites e `Set.disjoint` effectReads  e'
          && effectWrites e `Set.disjoint` effectWrites e')
  where racesWithLiftST eff =
             effectLiftST eff
          || not (Set.null (effectReads eff) && Set.null (effectWrites eff))

-- Steps

data Step = Step {
    stepThreadId :: !ThreadId,
    stepStep     :: !Int,
    stepEffect   :: !Effect,
    stepVClock   :: !VectorClock
  }
  deriving Show

-- steps race if they can be reordered with a possibly different outcome
racingSteps :: Step -- ^ an earlier step
            -> Step -- ^ a later step
            -> Bool
racingSteps s s' =
     stepThreadId s /= stepThreadId s'
  && not (stepThreadId s' `elem` effectWakeup (stepEffect s))
  && (stepEffect s `racingEffects` stepEffect s'
   || throwsTo s s'
   || throwsTo s' s)
  where throwsTo s1 s2 =
             stepThreadId s1 `elem` effectThrows (stepEffect s2)
          && stepEffect s1 /= mempty

currentStep :: Thread s a -> Step
currentStep Thread { threadId     = tid,
                     threadStep   = tstep,
                     threadEffect = teffect,
                     threadVClock = vClock
                   } =
  Step { stepThreadId = tid,
         stepStep     = tstep,
         stepEffect   = teffect,
         stepVClock   = vClock
       }

stepThread :: Thread s a -> Thread s a
stepThread thread@Thread { threadId     = tid,
                           threadStep   = tstep,
                           threadVClock = vClock } =
  thread { threadStep   = tstep+1,
           threadEffect = mempty,
           threadVClock = insertVClock tid (tstep+1) vClock
         }

-- As we run a simulation, we collect info about each previous step
data StepInfo = StepInfo {
    stepInfoStep       :: Step,
    -- Control information when we reached this step
    stepInfoControl    :: ScheduleControl,
    -- threads that are still concurrent with this step
    stepInfoConcurrent :: Set ThreadId,
    -- steps following this one that did not happen after it
    -- (in reverse order)
    stepInfoNonDep     :: [Step],
    -- later steps that race with this one
    stepInfoRaces      :: [Step]
  }
  deriving Show

data Races = Races { -- These steps may still race with future steps
                     activeRaces   :: ![StepInfo],
                     -- These steps cannot be concurrent with future steps
                     completeRaces :: ![StepInfo]
                   }
  deriving Show

noRaces :: Races
noRaces = Races [] []

updateRacesInSimState :: Thread s a -> SimState s a -> Races
updateRacesInSimState thread SimState{ control, threads, races } =
  traceRaces $
  updateRaces (currentStep thread)
              (threadBlocked thread)
              control
              (Map.keysSet (Map.filter (not . threadDone) threads))
              races

-- | 'updateRaces' turns a current 'Step' into 'StepInfo', and updates all
-- 'activeRaces'.
--
-- We take care that steps can only race against threads in their
-- concurrent set. When this becomes empty, a step can be retired into
-- the "complete" category, but only if there are some steps racing
-- with it.
updateRaces :: Step -> Bool -> ScheduleControl -> Set ThreadId -> Races -> Races
updateRaces newStep@Step{ stepThreadId = tid, stepEffect = newEffect }
            blocking
            control
            newConcurrent0
            races@Races{ activeRaces } =

  let justBlocking :: Bool
      justBlocking = blocking && onlyReadEffect newEffect

      -- a new step cannot race with any threads that it just woke up
      new :: [StepInfo]
      new | isTestThreadId tid  = []  -- test threads do not race
          | Set.null newConcurrent = []  -- cannot race with anything
          | justBlocking           = []  -- no need to defer a blocking transaction
          | otherwise              =
              [StepInfo { stepInfoStep       = newStep,
                          stepInfoControl    = control,
                          stepInfoConcurrent = newConcurrent,
                          stepInfoNonDep     = [],
                          stepInfoRaces      = []
                        }]
        where
          newConcurrent :: Set ThreadId
          newConcurrent = foldr Set.delete newConcurrent0 (effectWakeup newEffect)

      activeRaces' :: [StepInfo]
      activeRaces' =
        [ -- if this step depends on the previous step, or is not concurrent,
          -- then any threads that it wakes up become non-concurrent also.
          let lessConcurrent = foldr Set.delete concurrent (effectWakeup newEffect) in
          if tid `elem` concurrent then
            let theseStepsRace = not (isTestThreadId tid) && racingSteps step newStep
                happensBefore  = step `happensBeforeStep` newStep
                nondep' | happensBefore = nondep
                        | otherwise     = newStep : nondep
                -- We will only record the first race with each thread---reversing
                -- the first race makes the next race detectable. Thus we remove a
                -- thread from the concurrent set after the first race.
                concurrent' | happensBefore  = Set.delete tid lessConcurrent
                            | theseStepsRace = Set.delete tid concurrent
                            | otherwise      = concurrent
                -- Here we record discovered races.
                -- We only record a new race if we are following the default schedule,
                -- to avoid finding the same race in different parts of the search space.
                stepRaces' | (control == ControlDefault ||
                              control == ControlFollow [] []) &&
                             theseStepsRace  = newStep : stepRaces
                           | otherwise       = stepRaces

            in stepInfo { stepInfoConcurrent = effectForks newEffect
                                             `Set.union` concurrent',
                          stepInfoNonDep     = nondep',
                          stepInfoRaces      = stepRaces'
                        }

          else stepInfo { stepInfoConcurrent = lessConcurrent }

        | stepInfo@StepInfo { stepInfoStep       = step,
                              stepInfoConcurrent = concurrent,
                              stepInfoNonDep     = nondep,
                              stepInfoRaces      = stepRaces
                            }
            <- activeRaces ]
  in normalizeRaces $ races { activeRaces = new ++ activeRaces' }

-- When a thread terminates, we remove it from the concurrent thread
-- sets of active races.

threadTerminatesRaces :: ThreadId -> Races -> Races
threadTerminatesRaces tid races@Races{ activeRaces } =
  let activeRaces' = [ s{stepInfoConcurrent = Set.delete tid stepInfoConcurrent}
                     | s@StepInfo{ stepInfoConcurrent } <- activeRaces ]
  in normalizeRaces $ races{ activeRaces = activeRaces' }

normalizeRaces :: Races -> Races
normalizeRaces Races{ activeRaces, completeRaces } =
  let activeRaces' = filter (not . null. stepInfoConcurrent) activeRaces
      completeRaces' = filter (not . null. stepInfoRaces)
                         (filter (null . stepInfoConcurrent) activeRaces)
                    ++ completeRaces
  in Races{ activeRaces = activeRaces', completeRaces = completeRaces' }

-- We assume that steps do not race with later steps after a quiescent
-- period. Quiescent periods end when simulated time advances, thus we
-- are assuming here that all work is completed before a timer
-- triggers.

quiescentRacesInSimState :: SimState s a -> SimState s a
quiescentRacesInSimState simstate@SimState{ races } =
  simstate{ races = quiescentRaces races }

quiescentRaces :: Races -> Races
quiescentRaces Races{ activeRaces, completeRaces } =
  Races{ activeRaces = [],
         completeRaces = [ s{stepInfoConcurrent = Set.empty}
                         | s <- activeRaces
                         , not (null (stepInfoRaces s))
                         ] ++ completeRaces }

traceRaces :: Races -> Races
traceRaces r = r
-- traceRaces r@Races{activeRaces,completeRaces} =
--   Debug.trace ("Tracking "++show (length (concatMap stepInfoRaces activeRaces)) ++" races") r

-- Schedule modifications

stepStepId :: Step -> (ThreadId, Int)
stepStepId Step{ stepThreadId = tid, stepStep = n } = (tid,n)

threadStepId :: Thread s a -> (ThreadId, Int)
threadStepId Thread{ threadId, threadStep } = (threadId, threadStep)

stepInfoToScheduleMods :: StepInfo -> [ScheduleMod]
stepInfoToScheduleMods
  StepInfo{ stepInfoStep    = step,
            stepInfoControl = control,
            stepInfoNonDep  = nondep,
            stepInfoRaces   = races
          } =
  -- It is actually possible for a later step that races with an earlier one
  -- not to *depend* on it in a happens-before sense. But we don't want to try
  -- to follow any steps *after* the later one.
  [ ScheduleMod 
      { scheduleModTarget    = stepStepId step
      , scheduleModControl   = control
      , scheduleModInsertion = takeWhile (/=stepStepId step')
                                         (map stepStepId (reverse nondep))
                            ++ [stepStepId step']
                            -- It should be unnecessary to include the delayed
                            -- step in the insertion, since the default
                            -- scheduling should run it anyway. Removing it may
                            -- help avoid redundant schedules.
                            -- ++ [stepStepId step]
      }
  | step' <- races ]

traceFinalRacesFound :: SimState s a -> SimTrace a -> SimTrace a
traceFinalRacesFound simstate@SimState{ control0 = control } =
  TraceRacesFound [extendScheduleControl control m | m <- scheduleMods]
  where SimState{ races } =
          quiescentRacesInSimState simstate
        scheduleMods =
          concatMap stepInfoToScheduleMods $ completeRaces races


-- Schedule control

controlTargets :: StepId -> ScheduleControl -> Bool
controlTargets stepId
               (ControlAwait (ScheduleMod{ scheduleModTarget }:_)) =
  stepId == scheduleModTarget
controlTargets _stepId _ = False

controlFollows :: StepId -> ScheduleControl -> Bool
controlFollows _stepId  ControlDefault               = True
controlFollows _stepId (ControlFollow [] _)          = True
controlFollows stepId  (ControlFollow (stepId':_) _) = stepId == stepId'
controlFollows stepId  (ControlAwait (smod:_))       = stepId /= scheduleModTarget smod
controlFollows _       (ControlAwait [])             = error "Impossible: controlFollows _ (ControlAwait [])"

advanceControl :: StepId -> ScheduleControl -> ScheduleControl
advanceControl (tid,step) control@(ControlFollow ((tid',step'):sids') tgts)
  | tid /= tid' =
      -- we are switching threads to follow the schedule
      --Debug.trace ("Switching threads from "++show (tid,step)++" to "++show (tid',step')++"\n") $
      control
  | step == step' =
      ControlFollow sids' tgts
  | otherwise =
      error $ concat
            [ "advanceControl ", show (tid,step)
            , " cannot follow step ", show step'
            , "\n"
            ]
advanceControl stepId (ControlFollow [] []) =
  ControlDefault
advanceControl stepId (ControlFollow [] tgts) =
  ControlAwait tgts
advanceControl stepId control =
  assert (not $ controlTargets stepId control) $
  control

followControl :: ScheduleControl -> ScheduleControl
followControl (ControlAwait (ScheduleMod { scheduleModInsertion } : mods)) =
               ControlFollow scheduleModInsertion mods
followControl (ControlAwait []) = error "Impossible: followControl (ControlAwait [])"
followControl ControlDefault{}  = error "Impossible: followControl ControlDefault{}"
followControl ControlFollow{}   = error "Impossible: followControl ControlFollow{}"

-- Extend an existing schedule control with a newly discovered schedule mod
extendScheduleControl' :: ScheduleControl -> ScheduleMod -> ScheduleControl
extendScheduleControl' ControlDefault m = ControlAwait [m]
extendScheduleControl' (ControlAwait mods) m =
  case scheduleModControl m of
    ControlDefault     -> ControlAwait (mods++[m])
    ControlAwait mods' ->
      let common = length mods - length mods' in
      assert (common >= 0 && drop common mods==mods') $
      ControlAwait (take common mods++[m{ scheduleModControl = ControlDefault }])
    ControlFollow stepIds mods' ->
      let common = length mods - length mods' - 1
          m'     = mods !! common
          isUndo = scheduleModTarget m' `elem` scheduleModInsertion m
          m''    = m'{ scheduleModInsertion =
                         takeWhile (/=scheduleModTarget m)
                                   (scheduleModInsertion m')
                         ++
                         scheduleModInsertion m }
      in
      assert (common >= 0) $
      assert (drop (common+1) mods == mods') $
      if isUndo
        then ControlAwait mods          -- reject this mod... it's undoing a previous one
        else ControlAwait (take common mods++[m''])
extendScheduleControl' ControlFollow{} ScheduleMod{} = error "Impossible: extendScheduleControl' ControlFollow{} ScheduleMod{}"

extendScheduleControl :: ScheduleControl -> ScheduleMod -> ScheduleControl
extendScheduleControl control m =
  let control' = extendScheduleControl' control m in
  {- Debug.trace (unlines ["",
                        "Extending "++show control,
                        "     with "++show m,
                        "   yields "++show control']) -}
              control'
