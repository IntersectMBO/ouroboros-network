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
  -- * Simulation monad
  SimM,
  -- ** Run simulation
  runSim,
  runSimOrThrow,
  runSimStrictShutdown,
  Failure(..),
  runSimTrace,
  runSimTraceST,
  liftST,
  traceM,
  -- * Simulation time
  setCurrentTime,
  unshareClock,
  -- * Simulation trace
  Trace(..),
  TraceEvent(..),
  traceEvents,
  traceResult,
  selectTraceEvents,
  selectTraceEventsDynamic,
  selectTraceEventsSay,
  printTraceEventsSay,
  -- * Low-level API
  execReadTVar
  ) where

import           Prelude hiding (read)

import           Data.Functor (void)
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import qualified Data.List as List
import           Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Dynamic (Dynamic, toDyn, fromDynamic)
import           Data.Time
                   ( DiffTime, NominalDiffTime, UTCTime(..)
                   , diffUTCTime, addUTCTime, fromGregorian )

import           Control.Monad (mapM_)
import           Control.Exception
                   ( Exception(..), SomeException
                   , ErrorCall(..), throw, assert
                   , asyncExceptionToException, asyncExceptionFromException )
import qualified System.IO.Error as IO.Error (userError)

import           Control.Monad (when)
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
import           Control.Monad.Class.MonadSTM hiding (STM)
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

  GetMonoTime  :: (Time    -> SimA s b) -> SimA s b
  GetWallTime  :: (UTCTime -> SimA s b) -> SimA s b
  SetWallTime  ::  UTCTime -> SimA s b  -> SimA s b
  UnshareClock :: SimA s b -> SimA s b

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
  OrElse       :: StmA s a -> StmA s a -> (a -> StmA s b) -> StmA s b


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

  fork task        = SimM $ \k -> Fork task k
  forkWithUnmask f = fork (f unblock)
  myThreadId       = SimM $ \k -> GetThreadId k
  throwTo tid e    = SimM $ \k -> ThrowTo (toException e) tid (k ())

instance MonadSTM (SimM s) where
  type STM       (SimM s) = STM s
  type LazyTVar  (SimM s) = TVar s
  type LazyTMVar (SimM s) = TMVarDefault (SimM s)
  type TQueue    (SimM s) = TQueueDefault (SimM s)
  type TBQueue   (SimM s) = TBQueueDefault (SimM s)

  atomically action = SimM $ \k -> Atomically action k

  newTVar         x = STM $ \k -> NewTVar x k
  readTVar   tvar   = STM $ \k -> ReadTVar tvar k
  writeTVar  tvar x = STM $ \k -> WriteTVar tvar x (k ())
  retry             = STM $ \_ -> Retry
  orElse        a b = STM $ \k -> OrElse (runSTM a) (runSTM b) k

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

data Async s a = forall b. Async !ThreadId (b -> a) (LazyTMVar (SimM s) (Either SomeException b))

instance Eq (Async s a) where
    Async tid _ _ == Async tid' _ _ = tid == tid'

instance Ord (Async s a) where
    compare (Async tid _ _) (Async tid' _ _) = compare tid tid'

instance Functor (Async s) where
  fmap f (Async tid g var) = Async tid (f . g) var

instance MonadAsync (SimM s) where
  type Async (SimM s) = Async s

  async action = do
    var <- newEmptyTMVarM
    tid <- mask $ \restore ->
             fork $ try (restore action) >>= atomically . putTMVar var
    return (Async tid id var)

  asyncThreadId _proxy (Async tid _ _) = tid

  cancel a@(Async tid _ _) = throwTo tid AsyncCancelled <* waitCatch a
  cancelWith a@(Async tid _ _) e = throwTo tid e <* waitCatch a

  waitCatchSTM (Async _ f var) = fmap f <$> readTMVar var
  pollSTM      (Async _ f var) = fmap (fmap f) <$> tryReadTMVar var

instance MonadST (SimM s) where
  withLiftST f = f liftST

liftST :: StrictST.ST s a -> SimM s a
liftST action = SimM $ \k -> LiftST action k

instance MonadTime (SimM s) where

  getMonotonicTime = SimM $ \k -> GetMonoTime k
  getCurrentTime   = SimM $ \k -> GetWallTime k

-- | Set the current wall clock time for the thread's clock domain.
--
setCurrentTime :: UTCTime -> SimM s ()
setCurrentTime t = SimM $ \k -> SetWallTime t (k ())

-- | Put the thread into a new wall clock domain, not shared with the parent
-- thread. Changing the wall clock time in the new clock domain will not affect
-- the other clock of other threads. All threads forked by this thread from
-- this point onwards will share the new clock domain.
--
unshareClock :: SimM s ()
unshareClock = SimM $ \k -> UnshareClock (k ())

instance Eq (Timeout (SimM s)) where
  Timeout _ key == Timeout _ key' = key == key'

instance MonadTimer (SimM s) where
  data Timeout (SimM s) = Timeout !(TVar s TimeoutState) !TimeoutId

  readTimeout (Timeout var _key) = readTVar var

  newTimeout      d = SimM $ \k -> NewTimeout      d k
  updateTimeout t d = SimM $ \k -> UpdateTimeout t d (k ())
  cancelTimeout t   = SimM $ \k -> CancelTimeout t   (k ())

  timeout d action
    | d <  0    = Just <$> action
    | d == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        t@(Timeout _ tid) <- newTimeout d
        handleJust
          (\(TimeoutException tid') -> if tid' == tid
                                         then Just ()
                                         else Nothing)
          (\_ -> return Nothing) $
          bracket
            (void $ fork $ do
                fired <- atomically $ awaitTimeout t
                when fired $ throwTo pid (TimeoutException tid))
            (\_ -> cancelTimeout t)
            (\_ -> Just <$> action)

newtype TimeoutException = TimeoutException TimeoutId deriving Eq

instance Show TimeoutException where
    show _ = "<<timeout>>"

instance Exception TimeoutException where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException

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
    threadThrowTo :: ![(SomeException, ThreadId)],
    threadClockId :: !ClockId
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
newtype ClockId   = ClockId   Int deriving (Eq, Ord, Enum, Show)

data Trace a = Trace !Time !ThreadId !TraceEvent (Trace a)
             | TraceMainReturn    !Time a             ![ThreadId]
             | TraceMainException !Time SomeException ![ThreadId]
             | TraceDeadlock      !Time               ![ThreadId]
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

  | EventTxCommitted   [TVarId] -- tx wrote to these
                       [TVarId] -- and created these
  | EventTxAborted
  | EventTxBlocked     [TVarId] -- tx blocked reading these
  | EventTxWakeup      [TVarId] -- changed vars causing retry

  | EventTimerCreated   TimeoutId TVarId Time
  | EventTimerUpdated   TimeoutId        Time
  | EventTimerCancelled TimeoutId
  | EventTimerExpired   TimeoutId
  deriving Show

selectTraceEvents
    :: (TraceEvent -> Maybe b)
    -> Trace a
    -> [b]
selectTraceEvents fn = go
  where
    go (Trace _ _ ev trace) = case fn ev of
      Just x  -> x : go trace
      Nothing ->     go trace
    go (TraceMainException _ e _) = throw (FailureException e)
    go (TraceDeadlock      _   _) = throw FailureDeadlock
    go (TraceMainReturn    _ _ _) = []

-- | Select all the traced values matching the expected type. This relies on
-- the sim's dynamic trace facility.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
selectTraceEventsDynamic :: forall a b. Typeable b => Trace a -> [b]
selectTraceEventsDynamic = selectTraceEvents fn
  where
    fn :: TraceEvent -> Maybe b
    fn (EventLog dyn) = fromDynamic dyn
    fn _              = Nothing

-- | Get a trace of 'EventSay'.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
selectTraceEventsSay :: Trace a -> [String]
selectTraceEventsSay = selectTraceEvents fn
  where
    fn :: TraceEvent -> Maybe String
    fn (EventSay s) = Just s
    fn _            = Nothing

-- | Print all 'EventSay' to the console.
--
-- For convenience, this throws exceptions for abnormal sim termination.
--
printTraceEventsSay :: Trace a -> IO ()
printTraceEventsSay = mapM_ print . selectTraceEventsSay

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
     | FailureSloppyShutdown [ThreadId]
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
    go (Trace _ _ _ t)                  = go t
    go (TraceMainReturn _ _ tids@(_:_))
                               | strict = Left (FailureSloppyShutdown tids)
    go (TraceMainReturn _ x _)          = Right x
    go (TraceMainException _ e _)       = Left (FailureException e)
    go (TraceDeadlock   _   _)          = Left FailureDeadlock

traceEvents :: Trace a -> [(Time, ThreadId, TraceEvent)]
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
        threadThrowTo = [],
        threadClockId = ClockId 0
      }

data SimState s a = SimState {
       runqueue :: ![ThreadId],
       -- | All threads other than the currently running thread: both running
       -- and blocked threads.
       threads  :: !(Map ThreadId (Thread s a)),
       curTime  :: !Time,
       timers   :: !(OrdPSQ TimeoutId Time (TVar s TimeoutState)),
       clocks   :: !(Map ClockId UTCTime),
       nextTid  :: !ThreadId,   -- ^ next unused 'ThreadId'
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
      clocks   = Map.singleton (ClockId 0) epoch1970,
      nextTid  = ThreadId 1,
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
timeSiceEpoch :: Time -> NominalDiffTime
timeSiceEpoch (Time t) = fromRational (toRational t)


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
           clocks,
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

    GetMonoTime k -> do
      let thread' = thread { threadControl = ThreadControl (k time) ctl }
      schedule thread' simstate

    GetWallTime k -> do
      let clockid  = threadClockId thread
          clockoff = clocks Map.! clockid
          walltime = timeSiceEpoch time `addUTCTime` clockoff
          thread'  = thread { threadControl = ThreadControl (k walltime) ctl }
      schedule thread' simstate

    SetWallTime walltime' k -> do
      let clockid   = threadClockId thread
          clockoff  = clocks Map.! clockid
          walltime  = timeSiceEpoch time `addUTCTime` clockoff
          clockoff' = addUTCTime (diffUTCTime walltime' walltime) clockoff
          thread'   = thread { threadControl = ThreadControl k ctl }
          simstate' = simstate { clocks = Map.insert clockid clockoff' clocks }
      schedule thread' simstate'

    UnshareClock k -> do
      let clockid   = threadClockId thread
          clockoff  = clocks Map.! clockid
          clockid'  = (toEnum . fromEnum) tid -- reuse the thread id
          thread'   = thread { threadControl = ThreadControl k ctl
                             , threadClockId = clockid' }
          simstate' = simstate { clocks = Map.insert clockid' clockoff clocks }
      schedule thread' simstate'

    NewTimeout d k -> do
      tvar <- execNewTVar nextVid TimeoutPending
      let expiry  = d `addTime` time
          t       = Timeout tvar nextTmid
          timers' = PSQ.insert nextTmid expiry tvar timers
          thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { timers   = timers'
                                         , nextVid  = succ nextVid
                                         , nextTmid = succ nextTmid }
      return (Trace time tid (EventTimerCreated nextTmid nextVid expiry) trace)

    UpdateTimeout (Timeout _tvar tmid) d k -> do
          -- updating an expired timeout is a noop, so it is safe
          -- to race using a timeout with updating or cancelling it
      let updateTimeout_  Nothing       = ((), Nothing)
          updateTimeout_ (Just (_p, v)) = ((), Just (expiry, v))
          expiry  = d `addTime` time
          timers' = snd (PSQ.alter updateTimeout_ tmid timers)
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
                            , threadThrowTo = []
                            , threadClockId = threadClockId thread }
          threads' = Map.insert tid' thread'' threads
      trace <- schedule thread' simstate { runqueue = runqueue ++ [tid']
                                         , threads  = threads'
                                         , nextTid  = succ nextTid }
      return (Trace time tid (EventThreadForked tid') trace)

    Atomically a k -> do
      res <- execAtomically nextVid (runSTM a)
      case res of
        StmTxCommitted x written nextVid' -> do
          (wakeup, wokeby) <- threadsUnblockedByWrites written
          mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written
          let thread'     = thread { threadControl = ThreadControl (k x) ctl }
              (unblocked,
               simstate') = unblockThreads wakeup simstate
              vids        = [ tvarId tvar | SomeTVar tvar <- written ]
              -- We don't interrupt runnable threads to provide fairness
              -- anywhere else. We do it here by putting the tx that committed
              -- a transaction to the back of the runqueue, behind all other
              -- runnable threads, and behind the unblocked threads.
              -- For testing, we should have a more sophisticated policy to show
              -- that algorithms are not sensitive to the exact policy, so long
              -- as it is a fair policy (all runnable threads eventually run).
          trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
          return $
            Trace time tid (EventTxCommitted vids [nextVid..pred nextVid']) $
            traceMany
              [ (time, tid', EventTxWakeup vids')
              | tid' <- unblocked
              , let Just vids' = Set.toList <$> Map.lookup tid' wokeby ]
              trace

        StmTxAborted e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
          trace <- schedule thread' simstate
          return (Trace time tid EventTxAborted trace)

        StmTxBlocked read -> do
          mapM_ (\(SomeTVar tvar) -> blockThreadOnTVar tid tvar) read
          let vids = [ tvarId tvar | SomeTVar tvar <- read ]
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
        written <- execAtomically' (runSTM $ mapM_ timeoutAction fired)
        (wakeup, wokeby) <- threadsUnblockedByWrites written
        mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written

        let (unblocked,
             simstate') = unblockThreads wakeup simstate
        trace <- reschedule simstate' { curTime = time'
                                      , timers  = timers' }
        return $
          traceMany ([ (time', ThreadId (-1), EventTimerExpired tmid)
                     | tmid <- tmids ]
                  ++ [ (time', tid', EventTxWakeup vids)
                     | tid' <- unblocked
                     , let Just vids = Set.toList <$> Map.lookup tid' wokeby ])
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

traceMany :: [(Time, ThreadId, TraceEvent)] -> Trace a -> Trace a
traceMany []                      trace = trace
traceMany ((time, tid, event):ts) trace =
    Trace time tid event (traceMany ts trace)


--
-- Executing STM Transactions
--

data TVar s a = TVar {

       -- | The identifier of this var.
       --
       tvarId :: !TVarId,

       -- | The var's current value
       --
       tvarCurrent :: !(STRef s a),

       -- | A stack of undo values. This is only used while executing a
       -- transaction.
       --
       tvarUndo    :: !(STRef s [a]),

       -- | Thread Ids of threads blocked on a read of this var. It is
       -- represented in reverse order of thread wakeup, without duplicates.
       --
       -- To avoid duplicates efficiently, the operations rely on a copy of the
       -- thread Ids represented as a set.
       --
       tvarBlocked :: !(STRef s ([ThreadId], Set ThreadId))
     }

data StmTxResult s a =
       -- | A committed transaction reports the vars that were written (in order
       -- of first write) so that the scheduler can unblock other threads that
       -- were blocked in STM transactions that read any of these vars.
       --
       -- It also includes the updated TVarId name supply.
       --
       StmTxCommitted a [SomeTVar s] TVarId -- updated TVarId name supply

       -- | A blocked transaction reports the vars that were read so that the
       -- scheduler can block the thread on those vars.
       --
     | StmTxBlocked  [SomeTVar s]
     | StmTxAborted  SomeException

data SomeTVar s where
  SomeTVar :: !(TVar s a) -> SomeTVar s

data StmStack s b a where
  -- | Executing in the context of a top level 'atomically'.
  AtomicallyFrame  :: StmStack s a a
  
  -- | Executing in the context of the /left/ hand side of an 'orElse'
  OrElseLeftFrame  :: StmA s a                -- orElse right alternative
                   -> (a -> StmA s b)         -- subsequent continuation
                   -> Map TVarId (SomeTVar s) -- saved written vars set
                   -> [SomeTVar s]            -- saved written vars list
                   -> StmStack s b c
                   -> StmStack s a c

  -- | Executing in the context of the /right/ hand side of an 'orElse'
  OrElseRightFrame :: (a -> StmA s b)         -- subsequent continuation
                   -> Map TVarId (SomeTVar s) -- saved written vars set
                   -> [SomeTVar s]            -- saved written vars list
                   -> StmStack s b c
                   -> StmStack s a c

execAtomically :: forall s a.
                  TVarId
               -> StmA s a
               -> ST s (StmTxResult s a)
execAtomically =
    go AtomicallyFrame Map.empty Map.empty []
  where
    go :: forall b.
          StmStack s b a
       -> Map TVarId (SomeTVar s)  -- set of vars read
       -> Map TVarId (SomeTVar s)  -- set of vars written
       -> [SomeTVar s]             -- vars written in order (no dups)
       -> TVarId                   -- var fresh name supply
       -> StmA s b
       -> ST s (StmTxResult s a)
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
          return (StmTxCommitted x (reverse writtenSeq) nextVid)

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
        return (StmTxAborted (toException e))

      Retry -> case ctl of
        AtomicallyFrame -> do
          -- Revert all the TVar writes
          traverse_ (\(SomeTVar tvar) -> revertTVar tvar) written
          -- Return vars read, so the thread can block on them
          return (StmTxBlocked (Map.elems read))

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

      NewTVar x k -> do
        v <- execNewTVar nextVid x
        go ctl read written writtenSeq (succ nextVid) (k v)

      ReadTVar v k
        | tvarId v `Map.member` read -> do
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


execNewTVar :: TVarId -> a -> ST s (TVar s a)
execNewTVar nextVid x = do
    tvarCurrent <- newSTRef x
    tvarUndo    <- newSTRef []
    tvarBlocked <- newSTRef ([], Set.empty)
    return TVar {tvarId = nextVid, tvarCurrent, tvarUndo, tvarBlocked}

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
                         -> ST s ([ThreadId], Map ThreadId (Set TVarId))
threadsUnblockedByWrites written = do
  tidss <- sequence
             [ (,) (tvarId tvar) <$> readTVarBlockedThreads tvar
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


--
-- Examples
--
{-
example0 :: (MonadSay m, MonadTimer m, MonadFork m, MonadSTM m) => m ()
example0 = do
  say "starting"
  t <- atomically (newTVar (0 :: Int))
  _ <- fork $ threadDelay 2 >> do
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
  _ <- fork $ forever $ do
    atomically $ do
      x <- readTVar chan
      writeTVar chan (1:x)
  _ <- fork $ forever $ do
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
example2 :: (MonadSay m, MonadFork m, MonadSTM m) => m ()
example2 = do
  say "starting"
  v <- atomically $ newTVar Nothing
  _ <- fork $ do
    atomically $ do
      x <- readTVar v
      case x of
        Nothing -> retry
        Just _  -> return ()
    say "1"
  _ <- fork $ do
    atomically $ do
      x <- readTVar v
      case x of
        Nothing -> retry
        Just _  -> return ()
    say "2"
  atomically $ writeTVar v (Just ())

example3 :: SimM s ()
example3 = do
  say "starting"
  threadDelay 1
  mt1 <- getMonotonicTime
  ct1 <- getCurrentTime
  say (show (mt1, ct1))

  setCurrentTime (UTCTime (fromGregorian 2000 1 1) 0)
  ct1' <- getCurrentTime
  say (show ct1')

  threadDelay 1
  mt2 <- getMonotonicTime
  ct2 <- getCurrentTime
  say (show (mt2, ct2))

example4 :: SimM s ()
example4 = do

  say "starting"
  setCurrentTime (UTCTime (fromGregorian 2000 1 1) 0)
  do t <- (,) <$> getMonotonicTime <*> getCurrentTime
     say $ "start: " ++ show t

  v <- atomically (newTVar (0 :: Int))
  _ <- fork $ do
    unshareClock
    setCurrentTime (UTCTime (fromGregorian 2011 11 11) 11)
    do t <- (,) <$> getMonotonicTime <*> getCurrentTime
       say $ "unshared: " ++ show t

    atomically $
      writeTVar v 1

  atomically $ do
    x <- readTVar v
    unless (x == 1) retry

  do t <- (,) <$> getMonotonicTime <*> getCurrentTime
     say $ "end: " ++ show t

  say "main done"
-}
