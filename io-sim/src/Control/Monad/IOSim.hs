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
  runSimOrThrow,
  runSimStrictShutdown,
  Failure(..),
  runSimTrace,
  runSimTraceST,
  liftST,
  traceM,
  VTime(..),
  Trace(..),
  TraceEvent(..),
  traceEvents,
  traceResult,
  ) where

import           Prelude hiding (read)

import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified Data.List as List
import           Data.Maybe (maybeToList)
import           Data.Fixed (Pico)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Dynamic (Dynamic, toDyn)

import           Control.Exception
                   ( Exception(..), SomeException
                   , ErrorCall(..), throw, assert
                   , asyncExceptionToException
                   , asyncExceptionFromException )
import qualified System.IO.Error as IO.Error (userError)

import           Control.Monad.ST.Lazy
import qualified Control.Monad.ST.Strict as StrictST
import           Data.STRef.Lazy

import           Control.Monad.Fail as MonadFail
import qualified Control.Monad.Catch as Exceptions

import           Control.Monad.Class.MonadFork hiding (ThreadId)
import qualified Control.Monad.Class.MonadFork as MonadFork
import           Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM hiding (STM, TVar)
import qualified Control.Monad.Class.MonadSTM as MonadSTM
import           Control.Monad.Class.MonadAsync hiding (Async)
import qualified Control.Monad.Class.MonadAsync as MonadAsync
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

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
  Output       :: Dynamic -> SimA s b -> SimA s b

  LiftST       :: StrictST.ST s a -> (a -> SimA s b) -> SimA s b

  GetTime      :: (VTime -> SimA s b) -> SimA s b
  NewTimeout   :: DiffTime -> (Timeout (SimM s) -> SimA s b) -> SimA s b
  UpdateTimeout:: Timeout (SimM s) -> DiffTime -> SimA s b -> SimA s b
  CancelTimeout:: Timeout (SimM s) -> SimA s b -> SimA s b

  Throw        :: SomeException -> SimA s a
  Catch        :: Exception e =>
                  SimA s a -> (e -> SimA s a) -> (a -> SimA s b) -> SimA s b

  Fork         :: SimM s () -> (ThreadId -> SimA s b) -> SimA s b
  GetThreadId  :: (ThreadId -> SimA s b) -> SimA s b

  Atomically   :: STM  s a -> (a -> SimA s b) -> SimA s b

  ThrowTo      :: SomeException -> ThreadId -> SimA s a -> SimA s a
  SetMaskState :: MaskingState  -> SimM s a -> (a -> SimA s b) -> SimA s b
  GetMaskState :: (MaskingState -> SimA s b) -> SimA s b


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


data MaskingState = Unmasked | MaskedInterruptible | MaskedUninterruptible
  deriving (Eq, Ord, Show)

--
-- Monad class instances
--

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

newtype VTime = VTime Pico
  deriving (Eq, Ord, Show)

instance TimeMeasure VTime where

  diffTime (VTime t) (VTime t') = realToFrac (t-t')
  addTime d (VTime t) = VTime (t + realToFrac d)
  zeroTime = VTime 0

instance MonadFail (SimM s) where
  fail msg = SimM $ \_ -> Throw (toException (IO.Error.userError msg))

instance MonadFail (STM s) where
  fail msg = STM $ \_ -> ThrowStm (toException (ErrorCall msg))

instance MonadSay (SimM s) where
  say msg = SimM $ \k -> Say msg (k ())

instance MonadThrow (SimM s) where
  throwM e = SimM $ \_ -> Throw (toException e)

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

instance Exceptions.MonadCatch (SimM s) where
  catch = MonadThrow.catch

instance MonadMask (SimM s) where
  mask action = do
      b <- getMaskingState
      case b of
        Unmasked              -> block $ action unblock
        MaskedInterruptible   -> action block
        MaskedUninterruptible -> action blockUninterruptible

  uninterruptibleMask action = do
      b <- getMaskingState
      case b of
        Unmasked              -> blockUninterruptible $ action unblock
        MaskedInterruptible   -> blockUninterruptible $ action block
        MaskedUninterruptible -> action blockUninterruptible

instance Exceptions.MonadMask (SimM s) where
  mask                = MonadThrow.mask
  uninterruptibleMask = MonadThrow.uninterruptibleMask

  generalBracket acquire release use =
    mask $ \unmasked -> do
      resource <- acquire
      b <- unmasked (use resource) `catch` \e -> do
        _ <- release resource (Exceptions.ExitCaseException e)
        throwM e
      c <- release resource (Exceptions.ExitCaseSuccess b)
      return (b, c)


getMaskingState :: SimM s MaskingState
unblock, block, blockUninterruptible :: SimM s a -> SimM s a

getMaskingState        = SimM  GetMaskState
unblock              a = SimM (SetMaskState Unmasked a)
block                a = SimM (SetMaskState MaskedInterruptible a)
blockUninterruptible a = SimM (SetMaskState MaskedUninterruptible a)

instance MonadFork (SimM s) where
  type ThreadId (SimM s) = ThreadId

  fork task     = SimM $ \k -> Fork task k
  myThreadId    = SimM $ \k -> GetThreadId k
  throwTo tid e = SimM $ \k -> ThrowTo (toException e) tid (k ())

instance MonadSTM (SimM s) where
  type STM   (SimM s)   = STM s
  type TVar  (SimM s)   = TVar s
  type TMVar (SimM s)   = TMVarDefault (SimM s)
  type TQueue (SimM s)  = TQueueDefault (SimM s)
  type TBQueue (SimM s) = TBQueueDefault (SimM s)

  atomically action = SimM $ \k -> Atomically action k

  newTVar         x = STM $ \k -> NewTVar x k
  readTVar   tvar   = STM $ \k -> ReadTVar tvar k
  writeTVar  tvar x = STM $ \k -> WriteTVar tvar x (k ())
  retry             = STM $ \_ -> Retry

  newTMVar          = newTMVarDefault
  newTMVarM         = newTMVarMDefault
  newEmptyTMVar     = newEmptyTMVarDefault
  newEmptyTMVarM    = newEmptyTMVarMDefault
  takeTMVar         = takeTMVarDefault
  tryTakeTMVar      = tryTakeTMVarDefault
  putTMVar          = putTMVarDefault
  tryPutTMVar       = tryPutTMVarDefault
  readTMVar         = readTMVarDefault
  tryReadTMVar      = tryReadTMVarDefault
  swapTMVar         = swapTMVarDefault
  isEmptyTMVar      = isEmptyTMVarDefault

  newTQueue         = newTQueueDefault
  readTQueue        = readTQueueDefault
  tryReadTQueue     = tryReadTQueueDefault
  writeTQueue       = writeTQueueDefault
  isEmptyTQueue     = isEmptyTQueueDefault

  newTBQueue        = newTBQueueDefault
  readTBQueue       = readTBQueueDefault
  tryReadTBQueue    = tryReadTBQueueDefault
  writeTBQueue      = writeTBQueueDefault
  isEmptyTBQueue    = isEmptyTBQueueDefault
  isFullTBQueue     = isFullTBQueueDefault

data Async s a = Async !ThreadId (TMVar (SimM s) (Either SomeException a))

data AsyncCancelled = AsyncCancelled
  deriving Show

instance Exception AsyncCancelled where
  fromException = asyncExceptionFromException
  toException   = asyncExceptionToException

instance MonadAsync (SimM s) where
  type Async (SimM s) = Async s

  async action = do
    var <- newEmptyTMVarM
    tid <- mask $ \restore ->
             fork $ try (restore action) >>= atomically . putTMVar var
    return (Async tid var)

  cancel a@(Async tid _) = throwTo tid AsyncCancelled <* waitCatch a

  waitCatchSTM (Async _ var) = readTMVar var
  pollSTM      (Async _ var) = tryReadTMVar var

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

traceM :: Typeable a => a -> SimM s ()
traceM x = SimM $ \k -> Output (toDyn x) (k ())


--
-- Simulation interpreter
--

data Thread s a = Thread {
    threadId      :: !ThreadId,
    threadControl :: !(ThreadControl s a),
    threadBlocked :: !Bool,
    threadMasking :: !MaskingState,
    -- other threads blocked in a ThrowTo to us because we are or were masked
    threadThrowTo :: ![(SomeException, ThreadId)]
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
  MaskFrame  :: (b -> SimA s c)         -- subsequent continuation
             -> MaskingState            -- thread local state to restore
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
  | EventLog  Dynamic

  | EventThrow          SomeException
  | EventThrowTo        SomeException ThreadId -- This thread used ThrowTo
  | EventThrowToBlocked                        -- The ThrowTo blocked
  | EventThrowToWakeup                         -- The ThrowTo resumed
  | EventThrowToUnmasked ThreadId              -- A pending ThrowTo was activated

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

instance Exception Failure

runSim :: forall a. (forall s. SimM s a) -> Either Failure a
runSim mainAction = traceResult False (runSimTrace mainAction)

-- | For quick experiments and tests it is often appropriate and convenient to
-- simply throw failures as exceptions.
--
runSimOrThrow :: forall a. (forall s. SimM s a) -> a
runSimOrThrow mainAction =
    case runSim mainAction of
      Left  e -> throw e
      Right x -> x

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
        threadControl = ThreadControl (runSimM mainAction) MainFrame,
        threadBlocked = False,
        threadMasking = Unmasked,
        threadThrowTo = []
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
    not (threadBlocked running)
 && threadId running `Map.notMember` threads
 && threadId running `List.notElem` runqueue
 && invariant Nothing simstate

invariant Nothing SimState{runqueue,threads} =
    all (`Map.member` threads) runqueue
 && and [ threadBlocked t == (threadId t `notElem` runqueue)
        | t <- Map.elems threads ]
 && runqueue == List.nub runqueue


schedule :: Thread s a -> SimState s a -> ST s (Trace a)
schedule thread@Thread{
           threadId      = tid,
           threadControl = ThreadControl action ctl,
           threadMasking = maskst
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
        trace <- deschedule Terminated thread simstate
        return $ Trace time tid EventThreadFinished trace

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
      Right thread' -> do
        -- We found a suitable exception handler, continue with that
        trace <- schedule thread' simstate
        return (Trace time tid (EventThrow e) trace)

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
          trace <- deschedule Terminated thread simstate
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

    Output x k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (Trace time tid (EventLog x) trace)

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
                                                            ForkFrame
                            , threadBlocked = False
                            , threadMasking = threadMasking thread
                            , threadThrowTo = [] }
          threads' = Map.insert tid' thread'' threads
      trace <- schedule thread' simstate { runqueue = runqueue ++ [tid']
                                         , threads  = threads'
                                         , nextTid  = succ nextTid }
      return (Trace time tid (EventThreadForked tid') trace)

    Atomically a k -> do
      res <- execAtomically tid nextVid (runSTM a)
      case res of
        StmTxComitted x written wakeup nextVid' -> do
          let thread'     = thread { threadControl = ThreadControl (k x) ctl }
              (unblocked,
               simstate') = unblockThreads (map fst (reverse wakeup)) simstate
              -- We don't interrupt runnable threads to provide fairness
              -- anywhere else. We do it here by putting the tx that committed
              -- a transaction to the back of the runqueue, behind all other
              -- runnable threads, and behind the unblocked threads.
              -- For testing, we should have a more sophisticated policy to show
              -- that algorithms are not sensitive to the exact policy, so long
              -- as it is a fair policy (all runnable threads eventually run).
          trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
          return $
            Trace time tid (EventTxComitted written [nextVid..pred nextVid']) $
            traceMany
              [ (time, tid', EventTxWakeup vids)
              | tid' <- unblocked
              , vids <- maybeToList (List.lookup tid' wakeup) ]
              trace

        StmTxAborted e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
          trace <- schedule thread' simstate
          return (Trace time tid EventTxAborted trace)

        StmTxBlocked vids -> do
          trace <- deschedule Blocked thread simstate
          return (Trace time tid (EventTxBlocked vids) trace)

    GetThreadId k -> do
      let thread' = thread { threadControl = ThreadControl (k tid) ctl }
      schedule thread' simstate

    GetMaskState k -> do
      let thread' = thread { threadControl = ThreadControl (k maskst) ctl }
      schedule thread' simstate

    SetMaskState maskst' action' k -> do
      let thread' = thread { threadControl = ThreadControl
                                               (runSimM action')
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
      return (Trace time tid (EventThrowTo e tid) trace)

    ThrowTo e tid' k -> do
      let thread'   = thread { threadControl = ThreadControl k ctl }
          willBlock = case Map.lookup tid' threads of
                        Just t -> not (threadInterruptible t)
                        _      -> False
      if willBlock
        then do
          -- The target thread has async exceptions masked so we add the
          -- exception and the source thread id to the pending async exceptions.
          let adjustTarget t = t { threadThrowTo = (e, tid) : threadThrowTo t }
              threads'       = Map.adjust adjustTarget tid' threads
          trace <- deschedule Blocked thread' simstate { threads = threads' }
          return $ Trace time tid (EventThrowTo e tid')
                 $ Trace time tid EventThrowToBlocked
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
                  , threadMasking = MaskedInterruptible }
              simstate'@SimState { threads = threads' }
                         = snd (unblockThreads [tid'] simstate)
              threads''  = Map.adjust adjustTarget tid' threads'
              simstate'' = simstate' { threads = threads'' }

          trace <- schedule thread' simstate''
          return $ Trace time tid (EventThrowTo e tid')
                 $ trace

threadInterruptible :: Thread s a -> Bool
threadInterruptible thread =
    case threadMasking thread of
      Unmasked                 -> True
      MaskedInterruptible
        | threadBlocked thread -> True  -- blocking operations are interruptible
        | otherwise            -> False
      MaskedUninterruptible    -> False

data Deschedule = Yield | Interruptable | Blocked | Terminated

deschedule :: Deschedule -> Thread s a -> SimState s a -> ST s (Trace a)
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
                           threadThrowTo = (e, tid') : etids
                         }
                         simstate@SimState{ curTime = time } = do

    -- We're unmasking, but there are pending blocked async exceptions.
    -- So immediately raise the exception and unblock the blocked thread
    -- if possible.
    let thread' = thread { threadControl = ThreadControl (Throw e) ctl
                         , threadMasking = MaskedInterruptible
                         , threadThrowTo = etids }
        (unblocked,
         simstate') = unblockThreads [tid'] simstate
    trace <- schedule thread' simstate'
    return $ Trace time tid (EventThrowToUnmasked tid')
           $ traceMany [ (time, tid'', EventThrowToWakeup)
                       | tid'' <- unblocked ]
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

deschedule Terminated thread simstate@SimState{ curTime = time } = do
    -- This thread is done. If there are other threads blocked in a
    -- ThrowTo targeted at this thread then we can wake them up now.
    let wakeup      = map snd (reverse (threadThrowTo thread))
        (unblocked,
         simstate') = unblockThreads wakeup simstate
    trace <- reschedule simstate'
    return $ traceMany
               [ (time, tid', EventThrowToWakeup) | tid' <- unblocked ]
               trace

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

        let (unblocked,
             simstate') = unblockThreads (map fst (reverse wakeup)) simstate
        trace <- reschedule simstate' { curTime = time'
                                      , timers  = timers' }
        return $
          traceMany ([ (time', ThreadId (-1), EventTimerExpired tmid)
                     | tmid <- tmids ]
                  ++ [ (time', tid', EventTxWakeup vids)
                     | tid' <- unblocked
                     , vids <- maybeToList (List.lookup tid' wakeup) ])
                    trace
  where
    timeoutAction var = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var TimeoutFired
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
                     threadMasking = max maskst MaskedInterruptible
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
