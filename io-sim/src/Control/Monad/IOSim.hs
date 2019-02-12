{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Control.Monad.IOSim (
  SimM,
  runSim,
  runSimStrictShutdown,
  Failure(..),
  runSimTrace,
  runSimTraceST,
  liftST,
  VTime(..),
  VTimeDuration(..),
  ThreadId,
  Trace(..),
  TraceEvent(..),
  traceEvents,
  traceResult,
  ) where

import           Prelude hiding (read)

import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified Data.List as List
import           Data.Fixed (Micro)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Control.Exception
                   ( Exception(..), SomeException, ErrorCall(..), assert )
import qualified System.IO.Error as IO.Error (userError)

import           Control.Monad
import           Control.Monad.ST.Lazy
import qualified Control.Monad.ST.Strict as StrictST
import           Data.STRef.Lazy

import           Control.Monad.Fail as MonadFail
import qualified Control.Monad.Catch as Exceptions

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM hiding (TVar)
import qualified Control.Monad.Class.MonadSTM as MonadSTM
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadProbe hiding (Probe)
import qualified Control.Monad.Class.MonadProbe as MonadProbe

{-# ANN module "HLint: ignore Use readTVarIO" #-}


--
-- Simulation monad for protocol testing
--

newtype SimM s a = SimM { unSimM :: forall r. (a -> SimA s r) -> SimA s r }

runSimM :: SimM s a -> SimA s a
runSimM (SimM k) = k Return

data SimA s a where
  Return       :: a -> SimA s a

  Say          :: String -> SimA s b -> SimA s b
  Output       :: Probe s o -> o -> SimA s b -> SimA s b

  LiftST       :: StrictST.ST s a -> (a -> SimA s b) -> SimA s b

  GetTime      :: (VTime -> SimA s b) -> SimA s b
  NewTimeout   :: VTimeDuration -> (Timeout (SimM s) -> SimA s b) -> SimA s b
  UpdateTimeout:: Timeout (SimM s) -> VTimeDuration -> SimA s b -> SimA s b
  CancelTimeout:: Timeout (SimM s) -> SimA s b -> SimA s b

  Throw        :: SomeException -> SimA s a
  Catch        :: Exception e =>
                  SimA s a -> (e -> SimA s a) -> (a -> SimA s b) -> SimA s b

  Fork         :: SimM s () -> (ThreadId -> SimA s b) -> SimA s b
  Atomically   :: STM  s a -> (a -> SimA s b) -> SimA s b

newtype STM s a = STM { unSTM :: forall r. (a -> StmA s r) -> StmA s r }

runSTM :: STM s a -> StmA s a
runSTM (STM k) = k ReturnStm

data StmA s a where
  ReturnStm    :: a -> StmA s a
  ThrowStm     :: SomeException -> StmA s a

  NewTVar      :: x -> (TVar s x -> StmA s b) -> StmA s b
  ReadTVar     :: TVar s a -> (a -> StmA s b) -> StmA s b
  WriteTVar    :: TVar s a ->  a -> StmA s b  -> StmA s b
  Retry        :: StmA s b


type ProbeTrace a = [(VTime, a)]

newtype Probe s a = Probe (STRef s (ProbeTrace a))


instance Functor (SimM s) where
    {-# INLINE fmap #-}
    fmap f = \d -> SimM $ \k -> unSimM d (k . f)

instance Applicative (SimM s) where
    {-# INLINE pure #-}
    pure = \x -> SimM $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> SimM $ \k ->
                        unSimM df (\f -> unSimM dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> SimM $ \k -> unSimM dm (\_ -> unSimM dn k)

instance Monad (SimM s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> SimM $ \k -> unSimM dm (\m -> unSimM (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

    fail = MonadFail.fail


instance Functor (STM s) where
    {-# INLINE fmap #-}
    fmap f = \d -> STM $ \k -> unSTM d (k . f)

instance Applicative (STM s) where
    {-# INLINE pure #-}
    pure = \x -> STM $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> STM $ \k ->
                        unSTM df (\f -> unSTM dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> STM $ \k -> unSTM dm (\_ -> unSTM dn k)

instance Monad (STM s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> STM $ \k -> unSTM dm (\m -> unSTM (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

    fail = MonadFail.fail


--
-- Monad class instances
--

newtype VTime         = VTime Micro
  deriving (Eq, Ord, Show)
newtype VTimeDuration = VTimeDuration Micro
  deriving (Eq, Ord, Show, Num, Real, Fractional)

instance TimeMeasure VTime where
  type Duration VTime = VTimeDuration

  diffTime (VTime t) (VTime t') = VTimeDuration (t-t')
  addTime  (VTimeDuration d) (VTime t) = VTime (t+d)
  zero = VTime 0

instance MonadFail (SimM s) where
  fail msg = SimM $ \_ -> Throw (toException (IO.Error.userError msg))

instance MonadFail (STM s) where
  fail msg = STM $ \_ -> ThrowStm (toException (ErrorCall msg))

instance MonadSay (SimM s) where
  say msg = SimM $ \k -> Say msg (k ())

instance MonadFork (SimM s) where
  fork task = SimM $ \k -> Fork task (\_tid -> k ())

instance MonadThrow (SimM s) where
  throwM e = SimM $ \_ -> Throw (toException e)

  --TODO: remove this definition once we add the instance MonadMask
  bracket before after thing = do
    a <- before
    r <- thing a `onException` after a
    _ <- after a
    return r

  --TODO: remove this definition once we add the instance MonadMask
  finally thing after = do
    r <- thing `onException` after
    _ <- after
    return r

instance Exceptions.MonadThrow (SimM s) where
  throwM = MonadThrow.throwM

instance MonadThrow (STM s) where
  throwM e = STM $ \_ -> ThrowStm (toException e)

  -- Since these involve re-throwing the exception and we don't provide
  -- CatchSTM at all, then we can get away with trivial versions:
  bracket before after thing = do
    a <- before
    r <- thing a
    _ <- after a
    return r

  finally thing after = do
    r <- thing
    _ <- after
    return r

instance Exceptions.MonadThrow (STM s) where
  throwM = MonadThrow.throwM

instance MonadCatch (SimM s) where
  catch action handler =
    SimM $ \k -> Catch (runSimM action) (runSimM . handler) k

  --TODO: remove this definition once we add the instance MonadMask
  bracketOnError before after thing = do
    a <- before
    thing a `onException` after a

instance Exceptions.MonadCatch (SimM s) where
  catch = MonadThrow.catch


instance MonadSTM (SimM s) where
  type Tr    (SimM s)   = STM s
  type TVar  (SimM s)   = TVar s
  type TMVar (SimM s)   = TMVarDefault (SimM s)
  type TBQueue (SimM s) = TBQueueDefault (SimM s)

  atomically action = SimM $ \k -> Atomically action k

  newTVar         x = STM $ \k -> NewTVar x k
  readTVar   tvar   = STM $ \k -> ReadTVar tvar k
  writeTVar  tvar x = STM $ \k -> WriteTVar tvar x (k ())
  retry             = STM $ \_ -> Retry

  newTMVar          = newTMVarDefault
  newTMVarIO        = newTMVarIODefault
  newEmptyTMVar     = newEmptyTMVarDefault
  newEmptyTMVarIO   = newEmptyTMVarIODefault
  takeTMVar         = takeTMVarDefault
  tryTakeTMVar      = tryTakeTMVarDefault
  putTMVar          = putTMVarDefault
  tryPutTMVar       = tryPutTMVarDefault
  readTMVar         = readTMVarDefault
  tryReadTMVar      = tryReadTMVarDefault
  swapTMVar         = swapTMVarDefault
  isEmptyTMVar      = isEmptyTMVarDefault

  newTBQueue        = newTBQueueDefault
  readTBQueue       = readTBQueueDefault
  writeTBQueue      = writeTBQueueDefault

instance MonadST (SimM s) where
  withLiftST f = f liftST

liftST :: StrictST.ST s a -> SimM s a
liftST action = SimM $ \k -> LiftST action k

instance MonadTime (SimM s) where
  type Time (SimM s) = VTime

  getMonotonicTime = SimM $ \k -> GetTime k

instance MonadTimer (SimM s) where
  data Timeout (SimM s) = Timeout !(TVar s TimeoutState) !TimeoutId

  readTimeout (Timeout var _key) = readTVar var

  newTimeout      d = SimM $ \k -> NewTimeout      d k
  updateTimeout t d = SimM $ \k -> UpdateTimeout t d (k ())
  cancelTimeout t   = SimM $ \k -> CancelTimeout t   (k ())

instance MonadProbe (SimM s) where
  type Probe (SimM s) = Probe s
  probeOutput p o = SimM $ \k -> Output p o (k ())

instance MonadRunProbe (SimM s) (ST s) where
  newProbe = Probe <$> newSTRef []
  readProbe (Probe p) = reverse <$> readSTRef p
  runM = void . runSimTraceST

--
-- Simulation interpreter
--

data Thread s a = Thread {
    threadId      :: !ThreadId,
    threadControl :: !(ThreadControl s a)
  }

-- We hide the type @b@ here, so it's useful to bundle these two parts
-- together, rather than having Thread have an extential type, which
-- makes record updates awkward.
data ThreadControl s a where
  ThreadControl :: SimA s b
                -> ControlStack s b a
                -> ThreadControl s a

data ControlStack s b a where
  MainFrame  :: ControlStack s a  a
  ForkFrame  :: ControlStack s () a
  ContFrame  :: (b -> SimA s c)         -- subsequent continuation
             -> ControlStack s c a
             -> ControlStack s b a
  CatchFrame :: Exception e
             => (e -> SimA s b)         -- exception continuation
             -> (b -> SimA s c)         -- subsequent continuation
             -> ControlStack s c a
             -> ControlStack s b a

newtype ThreadId  = ThreadId  Int deriving (Eq, Ord, Enum, Show)
newtype TVarId    = TVarId    Int deriving (Eq, Ord, Enum, Show)
newtype TimeoutId = TimeoutId Int deriving (Eq, Ord, Enum, Show)

data Trace a = Trace !VTime !ThreadId !TraceEvent (Trace a)
             | TraceMainReturn    !VTime a             ![ThreadId]
             | TraceMainException !VTime SomeException ![ThreadId]
             | TraceDeadlock      !VTime               ![ThreadId]
  deriving Show

data TraceEvent
  = EventSay  String

  | EventThrow SomeException

  | EventThreadForked    ThreadId
  | EventThreadFinished                  -- terminated normally
  | EventThreadUnhandled SomeException   -- terminated due to unhandled exception

  | EventTxComitted    [TVarId] -- tx wrote to these
                       [TVarId] -- and created these
  | EventTxAborted
  | EventTxBlocked     [TVarId] -- tx blocked reading these
  | EventTxWakeup      [TVarId] -- changed vars causing retry

  | EventTimerCreated   TimeoutId TVarId VTime
  | EventTimerUpdated   TimeoutId        VTime
  | EventTimerCancelled TimeoutId
  | EventTimerExpired   TimeoutId
  deriving Show

-- | Simulation termination with failure
--
data Failure =
       -- | The main thread terminated with an exception
       FailureException SomeException

       -- | The threads all deadlocked
     | FailureDeadlock

       -- | The main thread terminated normally but other threads were still
       -- alive, and strict shutdown checking was requested.
       -- See 'runSimStrictShutdown'
     | FailureSloppyShutdown
  deriving Show

runSim :: forall a. (forall s. SimM s a) -> Either Failure a
runSim mainAction = traceResult False (runSimTrace mainAction)

-- | Like 'runSim' but also fail if when the main thread terminates, there
-- are other threads still running or blocked. If one is trying to follow
-- a strict thread cleanup policy then this helps testing for that.
--
runSimStrictShutdown :: forall a. (forall s. SimM s a) -> Either Failure a
runSimStrictShutdown mainAction = traceResult True (runSimTrace mainAction)

traceResult :: Bool -> Trace a -> Either Failure a
traceResult strict = go
  where
    go (Trace _ _ _ t)                      = go t
    go (TraceMainReturn _ _ (_:_)) | strict = Left FailureSloppyShutdown
    go (TraceMainReturn _ x _)              = Right x
    go (TraceMainException _ e _)           = Left (FailureException e)
    go (TraceDeadlock   _   _)              = Left FailureDeadlock

traceEvents :: Trace a -> [(VTime, ThreadId, TraceEvent)]
traceEvents (Trace time tid event t) = (time, tid, event) : traceEvents t
traceEvents _                        = []



runSimTrace :: forall a. (forall s. SimM s a) -> Trace a
runSimTrace mainAction = runST (runSimTraceST mainAction)

runSimTraceST :: forall s a. SimM s a -> ST s (Trace a)
runSimTraceST mainAction = schedule mainThread initialState
  where
    mainThread =
      Thread {
        threadId      = ThreadId 0,
        threadControl = ThreadControl (runSimM mainAction) MainFrame
      }

data SimState s a = SimState {
       runqueue :: ![ThreadId],
       -- | All threads other than the currently running thread: both running
       -- and blocked threads.
       threads  :: !(Map ThreadId (Thread s a)),
       curTime  :: !VTime,
       timers   :: !(OrdPSQ TimeoutId VTime (TVar s TimeoutState)),
       nextTid  :: !ThreadId,   -- ^ next unused 'ThreadId'
       nextVid  :: !TVarId,     -- ^ next unused 'TVarId'
       nextTmid :: !TimeoutId   -- ^ next unused 'TimeoutId'
     }

initialState :: SimState s a
initialState =
    SimState {
      runqueue = [],
      threads  = Map.empty,
      curTime  = VTime 0,
      timers   = PSQ.empty,
      nextTid  = ThreadId 1,
      nextVid  = TVarId 0,
      nextTmid = TimeoutId 0
    }

invariant :: Maybe (Thread s a) -> SimState s a -> Bool

invariant (Just running) simstate@SimState{runqueue,threads} =
    threadId running `Map.notMember` threads
 && threadId running `List.notElem` runqueue
 && invariant Nothing simstate

invariant Nothing SimState{runqueue,threads} =
    all (`Map.member` threads) runqueue
 && runqueue == List.nub runqueue


schedule :: Thread s a -> SimState s a -> ST s (Trace a)
schedule thread@Thread{
           threadId      = tid,
           threadControl = ThreadControl action ctl
         }
         simstate@SimState {
           runqueue,
           threads,
           timers,
           nextTid, nextVid, nextTmid,
           curTime  = time
         } =
  assert (invariant (Just thread) simstate) $
  case action of

    Return x -> case ctl of
      MainFrame ->
        -- the main thread is done, so we're done
        -- even if other threads are still running
        return $ Trace time tid EventThreadFinished
               $ TraceMainReturn time x (Map.keys threads)

      ForkFrame -> do
        -- this thread is done
        trace <- reschedule simstate { threads = Map.delete tid threads }
        return $ Trace time tid EventThreadFinished trace

      ContFrame k ctl' -> do
        -- pop the control stack and continue
        let thread' = thread { threadControl = ThreadControl (k x) ctl' }
        schedule thread' simstate

      CatchFrame _handler k ctl' -> do
        -- pop the control stack and continue
        let thread' = thread { threadControl = ThreadControl (k x) ctl' }
        schedule thread' simstate

    Throw e -> case unwindControlStack tid e ctl of
      Right thread' ->
        -- We found a suitable exception handler, continue with that
        schedule thread' simstate

      Left isMain
        -- We unwound and did not find any suitable exception handler, so we
        -- have an unhandled exception at the top level of the thread.
        | isMain ->
          -- An unhandled exception in the main thread terminates the program
          return (Trace time tid (EventThrow e) $
                  Trace time tid (EventThreadUnhandled e) $
                  TraceMainException time e (Map.keys threads))

        | otherwise -> do
          -- An unhandled exception in any other thread terminates the thread
          trace <- reschedule simstate { threads = Map.delete tid threads }
          return (Trace time tid (EventThrow e) $
                  Trace time tid (EventThreadUnhandled e) trace)

    Catch action' handler k -> do
      -- push the failure and success continuations onto the control stack
      let thread' = thread { threadControl = ThreadControl action' 
                                               (CatchFrame handler k ctl) }
      schedule thread' simstate

    Say msg k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (Trace time tid (EventSay msg) trace)

    Output (Probe p) o k -> do
      modifySTRef p ((time, o):)
      let thread' = thread { threadControl = ThreadControl k ctl }
      schedule thread' simstate

    LiftST st k -> do
      x <- strictToLazyST st
      let thread' = thread { threadControl = ThreadControl (k x) ctl }
      schedule thread' simstate

    GetTime k -> do
      let thread' = thread { threadControl = ThreadControl (k time) ctl }
      schedule thread' simstate

    NewTimeout d k -> do
      tvar <- execNewTVar nextVid TimeoutPending
      let expiry  = d `addTime` time
          timeout = Timeout tvar nextTmid
          timers' = PSQ.insert nextTmid expiry tvar timers
          thread' = thread { threadControl = ThreadControl (k timeout) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                         , nextVid  = succ nextVid
                                         , nextTmid = succ nextTmid }
      return (Trace time tid (EventTimerCreated nextTmid nextVid expiry) trace)

    UpdateTimeout (Timeout _tvar tmid) d k -> do
          -- updating an expired timeout is a noop, so it is safe
          -- to race using a timeout with updating or cancelling it
      let updateTimout  Nothing       = ((), Nothing)
          updateTimout (Just (_p, v)) = ((), Just (expiry, v))
          expiry  = d `addTime` time
          timers' = snd (PSQ.alter updateTimout tmid timers)
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (Trace time tid (EventTimerUpdated tmid expiry) trace)

    CancelTimeout (Timeout _tvar tmid) k -> do
      let timers' = PSQ.delete tmid timers
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (Trace time tid (EventTimerCancelled tmid) trace)

    Fork a k -> do
      let tid'     = nextTid
          thread'  = thread { threadControl = ThreadControl (k tid') ctl }
          thread'' = Thread { threadId      = tid'
                            , threadControl = ThreadControl (runSimM a)
                                                            ForkFrame }
          threads' = Map.insert tid' thread'' threads
      trace <- schedule thread' simstate { runqueue = runqueue ++ [tid']
                                         , threads  = threads'
                                         , nextTid  = succ nextTid }
      return (Trace time tid (EventThreadForked tid') trace)

    Atomically a k -> do
      res <- execAtomically tid nextVid (runSTM a)
      case res of
        StmTxComitted x written wakeup nextVid' -> do
          let thread'   = thread { threadControl = ThreadControl (k x) ctl }
              unblocked = [ tid' | (tid', _) <- reverse wakeup
                                 , Map.member tid' threads ]
              -- We don't interrupt runnable threads to provide fairness
              -- anywhere else. We do it here by putting the tx that committed
              -- a transaction to the back of the runqueue, behind all other
              -- runnable threads, and behind the unblocked threads.
              -- For testing, we should have a more sophisticated policy to show
              -- that algorithms are not sensitive to the exact policy, so long
              -- as it is a fair policy (all runnable threads eventually run).
              runqueue' = List.nub (runqueue ++ unblocked) ++ [tid]
              threads'  = Map.insert tid thread' threads
          trace <- reschedule simstate { runqueue = runqueue'
                                       , threads  = threads'
                                       , nextVid  = nextVid' }
          return $
            Trace time tid (EventTxComitted written [nextVid..pred nextVid']) $
            traceMany
              [ (time, tid', EventTxWakeup vids) | (tid', vids) <- wakeup ]
              trace

        StmTxAborted e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
          trace <- schedule thread' simstate
          return (Trace time tid EventTxAborted trace)

        StmTxBlocked vids -> do
          let threads' = Map.insert tid thread threads
          trace <- reschedule simstate { threads = threads' }
          return (Trace time tid (EventTxBlocked vids) trace)


-- When there is no current running thread but the runqueue is non-empty then
-- schedule the next one to run.
reschedule :: SimState s a -> ST s (Trace a)
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
      Nothing -> return (TraceDeadlock time (Map.keys threads))

      Just (tmids, time', fired, timers') -> assert (time' >= time) $ do

        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        wakeup <- execAtomically' (runSTM $ mapM_ timeoutAction fired)

        let runqueue' = [ tid' | (tid', _) <- reverse wakeup
                               , Map.member tid' threads ]
        trace <- reschedule simstate {
                   runqueue = runqueue',
                   curTime  = time',
                   timers   = timers'
                 }
        return $
          traceMany ([ (time', ThreadId (-1), EventTimerExpired tmid)
                     | tmid <- tmids ]
                  ++ [ (time', tid', EventTxWakeup vids)
                     | (tid', vids) <- wakeup ])
                    trace
  where
    timeoutAction var = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var TimeoutFired
        TimeoutFired     -> error "MonadTimer(Sim): invariant violation"
        TimeoutCancelled -> return ()


-- | Iterate through the control stack to find an enclosing exception handler
-- of the right type, or unwind all the way to the top level for the thread.
--
-- Also return if it's the main thread or a forked thread since we handle the
-- cases differently.
--
unwindControlStack :: ThreadId
                   -> SomeException
                   -> ControlStack s b a
                   -> Either Bool (Thread s a)
unwindControlStack _ _ MainFrame = Left True
unwindControlStack _ _ ForkFrame = Left False
unwindControlStack tid e (ContFrame _k ctl) =
    unwindControlStack tid e ctl

unwindControlStack tid e (CatchFrame handler k ctl) =
    case fromException e of
      -- not the right type, unwind to the next containing handler
      Nothing -> unwindControlStack tid e ctl

      -- Ok! We will be able to continue the thread with the handler
      -- followed by the continuation after the catch
      Just e' -> Right Thread {
                   threadId      = tid,
                   threadControl = ThreadControl (handler e')
                                                 (ContFrame k ctl)
                 }


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

traceMany :: [(VTime, ThreadId, TraceEvent)] -> Trace a -> Trace a
traceMany []                      trace = trace
traceMany ((time, tid, event):ts) trace =
    Trace time tid event (traceMany ts trace)


--
-- Executing STM Transactions
--

data TVar s a = TVar !TVarId
                     !(STRef s a)          -- current value
                     !(STRef s (Maybe a))  -- saved revert value
                     !(STRef s [ThreadId]) -- threads blocked on read

data StmTxResult s a =
       StmTxComitted a
                     [TVarId]
                     [(ThreadId, [TVarId])] -- wake up
                     TVarId                 -- updated TVarId name supply
     | StmTxAborted  SomeException
     | StmTxBlocked  [TVarId]               -- blocked on

data SomeTVar s where
  SomeTVar :: !(TVar s a) -> SomeTVar s

execAtomically :: ThreadId
               -> TVarId
               -> StmA s a
               -> ST s (StmTxResult s a)
execAtomically mytid = go [] []
  where
    go :: [SomeTVar s] -> [SomeTVar s] -> TVarId
       -> StmA s a -> ST s (StmTxResult s a)
    go read written nextVid action = case action of
      ReturnStm x     -> do (vids, tids) <- finaliseCommit written
                            return (StmTxComitted x vids tids nextVid)
      ThrowStm e      -> do finaliseAbort written
                            return (StmTxAborted (toException e))
      Retry           -> do vids <- finaliseRetry read written
                            return (StmTxBlocked vids)
      NewTVar x k     -> do v <- execNewTVar nextVid x
                            go read written (succ nextVid) (k v)
      ReadTVar v k    -> do x <- execReadTVar v
                            go (SomeTVar v : read) written nextVid (k x)
      WriteTVar v x k -> do execWriteTVar v x
                            go read (SomeTVar v : written) nextVid k

    -- Revert all the TVar writes
    finaliseAbort written =
      sequence_ [ revertTVar  tvar | SomeTVar tvar <- reverse written ]

    -- Revert all the TVar writes and put this thread on the blocked queue for
    -- all the TVars read in this transaction
    finaliseRetry :: [SomeTVar s] -> [SomeTVar s] -> ST s [TVarId]
    finaliseRetry read written = do
      sequence_ [ revertTVar  tvar | SomeTVar tvar <- reverse written ]
      sequence_ [ blockOnTVar tvar | SomeTVar tvar <- read ]
      return [ vid | SomeTVar (TVar vid _ _ _) <- read ]

    blockOnTVar :: TVar s a -> ST s ()
    blockOnTVar (TVar _vid _vcur _vsaved blocked) =
      --TODO: avoid duplicates!
      modifySTRef blocked (mytid:)


execAtomically' :: StmA s () -> ST s [(ThreadId, [TVarId])]
execAtomically' = go []
  where
    go :: [SomeTVar s] -> StmA s () -> ST s [(ThreadId, [TVarId])]
    go written action = case action of
      ReturnStm ()    -> do (_vids, tids) <- finaliseCommit written
                            return tids
      ReadTVar v k    -> do x <- execReadTVar v
                            go written (k x)
      WriteTVar v x k -> do execWriteTVar v x
                            go (SomeTVar v : written) k
      _ -> error "execAtomically': only for special case of reads and writes"


-- Commit each TVar and collect all the other threads blocked on the TVars
-- written to in this transaction.
finaliseCommit :: [SomeTVar s] -> ST s ([TVarId], [(ThreadId, [TVarId])])
finaliseCommit written = do
  tidss <- sequence [ (,) vid <$> commitTVar tvar
                    | SomeTVar tvar@(TVar vid _ _ _) <- written ]
  let -- for each thread, what var writes woke it up
      wokeVars    = Map.fromListWith (\l r -> List.nub $ l ++ r)
                      [ (tid, [vid]) | (vid, tids) <- tidss, tid <- tids ]
      -- threads to wake up, in wake up order, with associated vars
      wokeThreads = [ (tid, wokeVars Map.! tid)
                    | tid <- ordNub [ tid | (_, tids) <- tidss, tid <- reverse tids ]
                    ]
      writtenVids = [ vid | SomeTVar (TVar vid _ _ _) <- written ]
  return (writtenVids, wokeThreads)

execNewTVar :: TVarId -> a -> ST s (TVar s a)
execNewTVar nextVid x = do
  vcur    <- newSTRef x
  vsaved  <- newSTRef Nothing
  blocked <- newSTRef []
  return (TVar nextVid vcur vsaved blocked)

execReadTVar :: TVar s a -> ST s a
execReadTVar (TVar _vid vcur _vsaved _blocked) =
  readSTRef vcur

execWriteTVar :: TVar s a -> a -> ST s ()
execWriteTVar (TVar _vid vcur vsaved _blocked) x = do
  msaved <- readSTRef vsaved
  case msaved of
    -- on first write, save original value
    Nothing -> writeSTRef vsaved . Just =<< readSTRef vcur
    -- on subsequent writes do nothing
    Just _  -> return ()
  writeSTRef vcur x

revertTVar :: TVar s a -> ST s ()
revertTVar (TVar _vid vcur vsaved _blocked) = do
  msaved <- readSTRef vsaved
  case msaved of
    -- on first revert, restore original value
    Just saved -> writeSTRef vcur saved >> writeSTRef vsaved Nothing
    -- on subsequent reverts do nothing
    Nothing    -> return ()

commitTVar :: TVar s a -> ST s [ThreadId]
commitTVar (TVar _vid _vcur vsaved blocked) = do
  msaved <- readSTRef vsaved
  case msaved of
    -- on first commit, forget saved value and collect blocked threads
    Just _  -> writeSTRef vsaved Nothing >> readSTRef blocked
    -- on subsequent commits do nothing
    Nothing -> return []

ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go !_ [] = []
    go !s (x:xs)
      | x `Set.member` s = go s xs
      | otherwise        = x : go (Set.insert x s) xs


--
-- Examples
--
{-
example0 :: (MonadSay m, MonadTimer m, MonadSTM m) => m ()
example0 = do
  say "starting"
  t <- atomically (newTVar (0 :: Int))
  fork $ threadDelay 2 >> do
    say "timer fired!"
    atomically $
      writeTVar t 1
  atomically $ do
    x <- readTVar t
    unless (x == 1) retry
  say "main done"

example1 :: SimM s ()
example1 = do
  say "starting"
  chan <- atomically (newTVar ([] :: [Int]))
  fork $ forever $ do
    atomically $ do
      x <- readTVar chan
      writeTVar chan (1:x)
  fork $ forever $ do
    atomically $ do
      x <- readTVar chan
      writeTVar chan (2:x)
  -- This demonstrates an interesting aspect of the simulator:
  -- it is quite happy to let you do an infinite amount of work
  -- in zero time. These two busy loops do not progress in time.

  threadDelay 1 >> do
    x <- atomically $ readTVar chan
    say $ show x

-- the trace should contain "1" followed by "2"
example2 :: (MonadSay m, MonadSTM m) => m ()
example2 = do
  say "starting"
  v <- atomically $ newTVar Nothing
  fork $ do
    atomically $ do
      x <- readTVar v
      case x of
        Nothing -> retry
        Just _  -> return ()
    say "1"
  fork $ do
    atomically $ do
      x <- readTVar v
      case x of
        Nothing -> retry
        Just _  -> return ()
    say "2"
  atomically $ writeTVar v (Just ())
-}
