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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans            #-}
-- incomplete uni patterns in 'schedule' (when interpreting 'StmTxCommitted')
-- and 'reschedule'.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Control.Monad.IOSim.Internal (
  IOSim (..),
  SimM,
  runIOSim,
  runSimTraceST,
  traceM,
  traceSTM,
  STM,
  STMSim,
  SimSTM,
  setCurrentTime,
  unshareClock,
  TimeoutException (..),
  EventlogEvent (..),
  EventlogMarker (..),
  ThreadId,
  ThreadLabel,
  Labelled (..),
  Trace (..),
  TraceEvent (..),
  liftST,
  execReadTVar
  ) where

import           Prelude hiding (read)

import           Data.Dynamic (Dynamic, toDyn)
import           Data.Foldable (traverse_)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime (..), fromGregorian)
import           Data.Typeable (Typeable)
import           Quiet (Quiet (..))
import           GHC.Generics (Generic)

import           Control.Applicative (Alternative (..), liftA2)
import           Control.Exception (ErrorCall (..), assert,
                     asyncExceptionFromException, asyncExceptionToException)
import           Control.Monad (MonadPlus, join)
import qualified System.IO.Error as IO.Error (userError)

import           Control.Monad (when)
import           Control.Monad.ST.Lazy
import           Control.Monad.ST.Lazy.Unsafe (unsafeIOToST)
import qualified Control.Monad.ST.Strict as StrictST
import           Data.STRef.Lazy

import qualified Control.Monad.Catch as Exceptions
import qualified Control.Monad.Fail as Fail

import           Control.Monad.Class.MonadAsync hiding (Async)
import qualified Control.Monad.Class.MonadAsync as MonadAsync
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadFork hiding (ThreadId)
import qualified Control.Monad.Class.MonadFork as MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM hiding (STM, TVar)
import qualified Control.Monad.Class.MonadSTM as MonadSTM
import           Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

{-# ANN module "HLint: ignore Use readTVarIO" #-}
newtype IOSim s a = IOSim { unIOSim :: forall r. (a -> SimA s r) -> SimA s r }

type SimM s = IOSim s
{-# DEPRECATED SimM "Use IOSim" #-}

runIOSim :: IOSim s a -> SimA s a
runIOSim (IOSim k) = k Return

traceM :: Typeable a => a -> IOSim s ()
traceM x = IOSim $ \k -> Output (toDyn x) (k ())

traceSTM :: Typeable a => a -> STMSim s ()
traceSTM x = STM $ \k -> OutputStm (toDyn x) (k ())

data SimA s a where
  Return       :: a -> SimA s a

  Say          :: String -> SimA s b -> SimA s b
  Output       :: Dynamic -> SimA s b -> SimA s b

  LiftST       :: StrictST.ST s a -> (a -> SimA s b) -> SimA s b

  GetMonoTime  :: (Time    -> SimA s b) -> SimA s b
  GetWallTime  :: (UTCTime -> SimA s b) -> SimA s b
  SetWallTime  ::  UTCTime -> SimA s b  -> SimA s b
  UnshareClock :: SimA s b -> SimA s b

  NewTimeout   :: DiffTime -> (Timeout (IOSim s) -> SimA s b) -> SimA s b
  UpdateTimeout:: Timeout (IOSim s) -> DiffTime -> SimA s b -> SimA s b
  CancelTimeout:: Timeout (IOSim s) -> SimA s b -> SimA s b

  Throw        :: SomeException -> SimA s a
  Catch        :: Exception e =>
                  SimA s a -> (e -> SimA s a) -> (a -> SimA s b) -> SimA s b
  Evaluate     :: a -> (a -> SimA s b) -> SimA s b

  Fork         :: IOSim s () -> (ThreadId -> SimA s b) -> SimA s b
  GetThreadId  :: (ThreadId -> SimA s b) -> SimA s b
  LabelThread  :: ThreadId -> String -> SimA s b -> SimA s b

  Atomically   :: STM  s a -> (a -> SimA s b) -> SimA s b

  ThrowTo      :: SomeException -> ThreadId -> SimA s a -> SimA s a
  SetMaskState :: MaskingState  -> IOSim s a -> (a -> SimA s b) -> SimA s b
  GetMaskState :: (MaskingState -> SimA s b) -> SimA s b


newtype STM s a = STM { unSTM :: forall r. (a -> StmA s r) -> StmA s r }

runSTM :: STM s a -> StmA s a
runSTM (STM k) = k ReturnStm

data StmA s a where
  ReturnStm    :: a -> StmA s a
  ThrowStm     :: SomeException -> StmA s a

  NewTVar      :: Maybe String -> x -> (TVar s x -> StmA s b) -> StmA s b
  LabelTVar    :: String -> TVar s a -> StmA s b -> StmA s b
  ReadTVar     :: TVar s a -> (a -> StmA s b) -> StmA s b
  WriteTVar    :: TVar s a ->  a -> StmA s b  -> StmA s b
  Retry        :: StmA s b
  OrElse       :: StmA s a -> StmA s a -> (a -> StmA s b) -> StmA s b

  SayStm       :: String -> StmA s b -> StmA s b
  OutputStm    :: Dynamic -> StmA s b -> StmA s b

-- Exported type
type STMSim = STM

type SimSTM = STM
{-# DEPRECATED SimSTM "Use STMSim" #-}

data MaskingState = Unmasked | MaskedInterruptible | MaskedUninterruptible
  deriving (Eq, Ord, Show)

--
-- Monad class instances
--

instance Functor (IOSim s) where
    {-# INLINE fmap #-}
    fmap f = \d -> IOSim $ \k -> unIOSim d (k . f)

instance Applicative (IOSim s) where
    {-# INLINE pure #-}
    pure = \x -> IOSim $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> IOSim $ \k ->
                        unIOSim df (\f -> unIOSim dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> IOSim $ \k -> unIOSim dm (\_ -> unIOSim dn k)

instance Monad (IOSim s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> IOSim $ \k -> unIOSim dm (\m -> unIOSim (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Semigroup a => Semigroup (IOSim s a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (IOSim s a) where
    mempty = pure mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = liftA2 mappend
#endif

instance Fail.MonadFail (IOSim s) where
  fail msg = IOSim $ \_ -> Throw (toException (IO.Error.userError msg))


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

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Fail.MonadFail (STM s) where
  fail msg = STM $ \_ -> ThrowStm (toException (ErrorCall msg))

instance Alternative (STM s) where
    empty = retry
    (<|>) = orElse

instance MonadPlus (STM s) where

instance MonadSay (IOSim s) where
  say msg = IOSim $ \k -> Say msg (k ())

instance MonadThrow (IOSim s) where
  throwIO e = IOSim $ \_ -> Throw (toException e)

instance MonadEvaluate (IOSim s) where
  evaluate a = IOSim $ \k -> Evaluate a k

instance Exceptions.MonadThrow (IOSim s) where
  throwM = MonadThrow.throwIO

instance MonadThrow (STM s) where
  throwIO e = STM $ \_ -> ThrowStm (toException e)

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
  throwM = MonadThrow.throwIO

instance MonadCatch (IOSim s) where
  catch action handler =
    IOSim $ \k -> Catch (runIOSim action) (runIOSim . handler) k

instance Exceptions.MonadCatch (IOSim s) where
  catch = MonadThrow.catch

instance MonadMask (IOSim s) where
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

instance Exceptions.MonadMask (IOSim s) where
  mask                = MonadThrow.mask
  uninterruptibleMask = MonadThrow.uninterruptibleMask

  generalBracket acquire release use =
    mask $ \unmasked -> do
      resource <- acquire
      b <- unmasked (use resource) `catch` \e -> do
        _ <- release resource (Exceptions.ExitCaseException e)
        throwIO e
      c <- release resource (Exceptions.ExitCaseSuccess b)
      return (b, c)


getMaskingState :: IOSim s MaskingState
unblock, block, blockUninterruptible :: IOSim s a -> IOSim s a

getMaskingState        = IOSim  GetMaskState
unblock              a = IOSim (SetMaskState Unmasked a)
block                a = IOSim (SetMaskState MaskedInterruptible a)
blockUninterruptible a = IOSim (SetMaskState MaskedUninterruptible a)

instance MonadThread (IOSim s) where
  type ThreadId (IOSim s) = ThreadId
  myThreadId       = IOSim $ \k -> GetThreadId k
  labelThread t l  = IOSim $ \k -> LabelThread t l (k ())

instance MonadFork (IOSim s) where
  forkIO task        = IOSim $ \k -> Fork task k
  forkIOWithUnmask f = forkIO (f unblock)
  throwTo tid e      = IOSim $ \k -> ThrowTo (toException e) tid (k ())

instance MonadSay (STMSim s) where
  say msg = STM $ \k -> SayStm msg (k ())

instance MonadSTMTx (STM s) where
  type TVar_      (STM s) = TVar s
  type TMVar_     (STM s) = TMVarDefault (IOSim s)
  type TQueue_    (STM s) = TQueueDefault (IOSim s)
  type TBQueue_   (STM s) = TBQueueDefault (IOSim s)

  newTVar         x = STM $ \k -> NewTVar Nothing x k
  readTVar   tvar   = STM $ \k -> ReadTVar tvar k
  writeTVar  tvar x = STM $ \k -> WriteTVar tvar x (k ())
  retry             = STM $ \_ -> Retry
  orElse        a b = STM $ \k -> OrElse (runSTM a) (runSTM b) k

  newTMVar          = newTMVarDefault
  newEmptyTMVar     = newEmptyTMVarDefault
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
  flushTBQueue      = flushTBQueueDefault
  writeTBQueue      = writeTBQueueDefault
  lengthTBQueue     = lengthTBQueueDefault
  isEmptyTBQueue    = isEmptyTBQueueDefault
  isFullTBQueue     = isFullTBQueueDefault

instance MonadLabelledSTMTx (STM s) where
  labelTVar tvar label = STM $ \k -> LabelTVar label tvar (k ())
  labelTMVar   = labelTMVarDefault
  labelTQueue  = labelTQueueDefault
  labelTBQueue = labelTBQueueDefault

instance MonadLabelledSTM (IOSim s) where

instance MonadSTM (IOSim s) where
  type STM       (IOSim s) = STM s

  atomically action = IOSim $ \k -> Atomically action k

  newTMVarIO        = newTMVarIODefault
  newEmptyTMVarIO   = newEmptyTMVarIODefault

data Async s a = Async !ThreadId (STM s (Either SomeException a))

instance Eq (Async s a) where
    Async tid _ == Async tid' _ = tid == tid'

instance Ord (Async s a) where
    compare (Async tid _) (Async tid' _) = compare tid tid'

instance Functor (Async s) where
  fmap f (Async tid a) = Async tid (fmap f <$> a)

instance MonadAsyncSTM (Async s) (STM s) where
  waitCatchSTM (Async _ w) = w
  pollSTM      (Async _ w) = (Just <$> w) `orElse` return Nothing

instance MonadAsync (IOSim s) where
  type Async (IOSim s) = Async s

  async action = do
    var <- newEmptyTMVarIO
    tid <- mask $ \restore ->
             forkIO $ try (restore action) >>= atomically . putTMVar var
    return (Async tid (readTMVar var))

  asyncThreadId _proxy (Async tid _) = tid

  cancel a@(Async tid _) = throwTo tid AsyncCancelled <* waitCatch a
  cancelWith a@(Async tid _) e = throwTo tid e <* waitCatch a

  asyncWithUnmask k = async (k unblock)

instance MonadST (IOSim s) where
  withLiftST f = f liftST

liftST :: StrictST.ST s a -> IOSim s a
liftST action = IOSim $ \k -> LiftST action k

instance MonadMonotonicTime (IOSim s) where
  getMonotonicTime = IOSim $ \k -> GetMonoTime k

instance MonadTime (IOSim s) where
  getCurrentTime   = IOSim $ \k -> GetWallTime k

-- | Set the current wall clock time for the thread's clock domain.
--
setCurrentTime :: UTCTime -> IOSim s ()
setCurrentTime t = IOSim $ \k -> SetWallTime t (k ())

-- | Put the thread into a new wall clock domain, not shared with the parent
-- thread. Changing the wall clock time in the new clock domain will not affect
-- the other clock of other threads. All threads forked by this thread from
-- this point onwards will share the new clock domain.
--
unshareClock :: IOSim s ()
unshareClock = IOSim $ \k -> UnshareClock (k ())

instance MonadDelay (IOSim s) where
  -- Use default in terms of MonadTimer

instance MonadTimer (IOSim s) where
  data Timeout (IOSim s) = Timeout !(TVar s TimeoutState) !(TVar s Bool) !TimeoutId
                         -- ^ a timeout; we keep both 'TVar's to support
                         -- `newTimer` and 'registerTimeout'.
                         | NegativeTimeout !TimeoutId
                         -- ^ a negative timeout

  readTimeout (Timeout var _bvar _key) = readTVar var
  readTimeout (NegativeTimeout _key)   = pure TimeoutCancelled

  newTimeout      d = IOSim $ \k -> NewTimeout      d k
  updateTimeout t d = IOSim $ \k -> UpdateTimeout t d (k ())
  cancelTimeout t   = IOSim $ \k -> CancelTimeout t   (k ())

  timeout d action
    | d <  0    = Just <$> action
    | d == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        t@(Timeout _ _ tid) <- newTimeout d
        handleJust
          (\(TimeoutException tid') -> if tid' == tid
                                         then Just ()
                                         else Nothing)
          (\_ -> return Nothing) $
          bracket
            (forkIO $ do
                labelThisThread "<<timeout>>"
                fired <- atomically $ awaitTimeout t
                when fired $ throwTo pid (TimeoutException tid))
            (\pid' -> do
                  cancelTimeout t
                  throwTo pid' AsyncCancelled)
            (\_ -> Just <$> action)

  registerDelay d = IOSim $ \k -> NewTimeout d (\(Timeout _var bvar _) -> k bvar)

newtype TimeoutException = TimeoutException TimeoutId deriving Eq

instance Show TimeoutException where
    show _ = "<<timeout>>"

instance Exception TimeoutException where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Wrapper for Eventlog events so they can be retrieved from the trace with
-- 'selectTraceEventsDynamic'.
newtype EventlogEvent = EventlogEvent String

-- | Wrapper for Eventlog markers so they can be retrieved from the trace with
-- 'selectTraceEventsDynamic'.
newtype EventlogMarker = EventlogMarker String

instance MonadEventlog (IOSim s) where
  traceEventIO = traceM . EventlogEvent
  traceMarkerIO = traceM . EventlogMarker

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
    threadLabel   :: Maybe ThreadLabel
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

unTimeoutId :: TimeoutId -> Int
unTimeoutId (TimeoutId a) = a

type ThreadLabel = String
type TVarLabel   = String

data Labelled a = Labelled {
    l_labelled :: !a,
    l_label    :: !(Maybe String)
  }
  deriving (Eq, Ord, Generic)
  deriving Show via Quiet (Labelled a)

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


-- | 'Trace' is a recursive data type, it is the trace of a 'IOSim' computation.
-- The trace will contain information about thread sheduling, blocking on
-- 'TVar's, and other internal state changes of 'IOSim'.  More importantly it
-- also supports traces generated by the computation with 'say' (which
-- corresponds to using 'putStrLn' in 'IO'), 'traceEventM', or dynamically typed
-- traces with 'traceM' (which generalise the @base@ library
-- 'Debug.Trace.traceM')
--
-- See also: 'traceEvents', 'traceResult', 'selectTraceEvents',
-- 'selectTraceEventsDynamic' and 'printTraceEventsSay'.
--
data Trace a = Trace !Time !ThreadId !(Maybe ThreadLabel) !TraceEvent (Trace a)
             | TraceMainReturn    !Time a             ![Labelled ThreadId]
             | TraceMainException !Time SomeException ![Labelled ThreadId]
             | TraceDeadlock      !Time               ![Labelled ThreadId]
  deriving Show

data TraceEvent
  = EventSay  String
  | EventLog  Dynamic

  | EventThrow          SomeException
  | EventThrowTo        SomeException ThreadId -- This thread used ThrowTo
  | EventThrowToBlocked                        -- The ThrowTo blocked
  | EventThrowToWakeup                         -- The ThrowTo resumed
  | EventThrowToUnmasked (Labelled ThreadId)   -- A pending ThrowTo was activated

  | EventThreadForked    ThreadId
  | EventThreadFinished                  -- terminated normally
  | EventThreadUnhandled SomeException   -- terminated due to unhandled exception

  | EventTxCommitted   [Labelled TVarId] -- tx wrote to these
                       [TVarId]          -- and created these
  | EventTxAborted
  | EventTxBlocked     [Labelled TVarId] -- tx blocked reading these
  | EventTxWakeup      [Labelled TVarId] -- changed vars causing retry

  | EventTimerCreated   TimeoutId TVarId Time
  | EventTimerUpdated   TimeoutId        Time
  | EventTimerCancelled TimeoutId
  | EventTimerExpired   TimeoutId
  deriving Show


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


-- | Schedule / run a thread.
--
schedule :: Thread s a -> SimState s a -> ST s (Trace a)
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
           nextTid, nextVid, nextTmid,
           curTime  = time
         } =
  assert (invariant (Just thread) simstate) $
  case action of

    Return x -> case ctl of
      MainFrame ->
        -- the main thread is done, so we're done
        -- even if other threads are still running
        return $ Trace time tid tlbl EventThreadFinished
               $ TraceMainReturn time x (labelledThreads threads)

      ForkFrame -> do
        -- this thread is done
        trace <- deschedule Terminated thread simstate
        return $ Trace time tid tlbl EventThreadFinished trace

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
        return (Trace time tid tlbl (EventThrow e) trace)

      Left isMain
        -- We unwound and did not find any suitable exception handler, so we
        -- have an unhandled exception at the top level of the thread.
        | isMain ->
          -- An unhandled exception in the main thread terminates the program
          return (Trace time tid tlbl (EventThrow e) $
                  Trace time tid tlbl (EventThreadUnhandled e) $
                  TraceMainException time e (labelledThreads threads))

        | otherwise -> do
          -- An unhandled exception in any other thread terminates the thread
          trace <- deschedule Terminated thread simstate
          return (Trace time tid tlbl (EventThrow e) $
                  Trace time tid tlbl (EventThreadUnhandled e) trace)

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
      return (Trace time tid tlbl (EventSay msg) trace)

    Output x k -> do
      let thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate
      return (Trace time tid tlbl (EventLog x) trace)

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

    -- we treat negative timers as cancelled ones; for the record we put
    -- `EventTimerCreated` and `EventTimerCancelled` in the trace; This differs
    -- from `GHC.Event` behaviour.
    NewTimeout d k | d < 0 -> do
      let t       = NegativeTimeout nextTmid
          expiry  = d `addTime` time
          thread' = thread { threadControl = ThreadControl (k t) ctl }
      trace <- schedule thread' simstate { nextTmid = succ nextTmid }
      return (Trace time tid tlbl (EventTimerCreated nextTmid nextVid expiry) $
              Trace time tid tlbl (EventTimerCancelled nextTmid) $
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
      return (Trace time tid tlbl (EventTimerCreated nextTmid nextVid expiry) trace)

    -- we do not follow `GHC.Event` behaviour here; updating a timer to the past
    -- effectively cancels it.
    UpdateTimeout (Timeout _tvar _tvar' tmid) d k | d < 0 -> do
      let timers' = PSQ.delete tmid timers
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (Trace time tid tlbl (EventTimerCancelled tmid) trace)

    UpdateTimeout (Timeout _tvar _tvar' tmid) d k -> do
          -- updating an expired timeout is a noop, so it is safe
          -- to race using a timeout with updating or cancelling it
      let updateTimeout_  Nothing       = ((), Nothing)
          updateTimeout_ (Just (_p, v)) = ((), Just (expiry, v))
          expiry  = d `addTime` time
          timers' = snd (PSQ.alter updateTimeout_ tmid timers)
          thread' = thread { threadControl = ThreadControl k ctl }
      trace <- schedule thread' simstate { timers = timers' }
      return (Trace time tid tlbl (EventTimerUpdated tmid expiry) trace)

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
      return $ Trace time tid tlbl (EventTimerCancelled tmid)
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
      let tid'     = nextTid
          thread'  = thread { threadControl = ThreadControl (k tid') ctl }
          thread'' = Thread { threadId      = tid'
                            , threadControl = ThreadControl (runIOSim a)
                                                            ForkFrame
                            , threadBlocked = False
                            , threadMasking = threadMasking thread
                            , threadThrowTo = []
                            , threadClockId = threadClockId thread
                            , threadLabel   = Nothing
                            }
          threads' = Map.insert tid' thread'' threads
      trace <- schedule thread' simstate { runqueue = runqueue ++ [tid']
                                         , threads  = threads'
                                         , nextTid  = succ nextTid }
      return (Trace time tid tlbl (EventThreadForked tid') trace)

    Atomically a k -> execAtomically time tid tlbl nextVid (runSTM a) $ \res ->
      case res of
        StmTxCommitted x written nextVid' -> do
          (wakeup, wokeby) <- threadsUnblockedByWrites written
          mapM_ (\(SomeTVar tvar) -> unblockAllThreadsFromTVar tvar) written
          let thread'     = thread { threadControl = ThreadControl (k x) ctl }
              (unblocked,
               simstate') = unblockThreads wakeup simstate
          vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) written
              -- We don't interrupt runnable threads to provide fairness
              -- anywhere else. We do it here by putting the tx that committed
              -- a transaction to the back of the runqueue, behind all other
              -- runnable threads, and behind the unblocked threads.
              -- For testing, we should have a more sophisticated policy to show
              -- that algorithms are not sensitive to the exact policy, so long
              -- as it is a fair policy (all runnable threads eventually run).
          trace <- deschedule Yield thread' simstate' { nextVid  = nextVid' }
          return $
            Trace time tid tlbl (EventTxCommitted vids [nextVid..pred nextVid']) $
            traceMany
              [ (time, tid', tlbl', EventTxWakeup vids')
              | tid' <- unblocked
              , let tlbl' = lookupThreadLabel tid' threads
              , let Just vids' = Set.toList <$> Map.lookup tid' wokeby ]
              trace

        StmTxAborted e -> do
          -- schedule this thread to immediately raise the exception
          let thread' = thread { threadControl = ThreadControl (Throw e) ctl }
          trace <- schedule thread' simstate
          return $ Trace time tid tlbl EventTxAborted trace

        StmTxBlocked read -> do
          mapM_ (\(SomeTVar tvar) -> blockThreadOnTVar tid tvar) read
          vids <- traverse (\(SomeTVar tvar) -> labelledTVarId tvar) read
          trace <- deschedule Blocked thread simstate
          return $ Trace time tid tlbl (EventTxBlocked vids) trace

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
      return (Trace time tid tlbl (EventThrowTo e tid) trace)

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
          return $ Trace time tid tlbl (EventThrowTo e tid')
                 $ Trace time tid tlbl EventThrowToBlocked
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
          return $ Trace time tid tlbl (EventThrowTo e tid')
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
    return $ Trace time tid tlbl (EventThrowToUnmasked tid')
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
          traceMany ([ (time', ThreadId (-1), Just "timer", EventTimerExpired tmid)
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

traceMany :: [(Time, ThreadId, Maybe ThreadLabel, TraceEvent)]
          -> Trace a -> Trace a
traceMany []                      trace = trace
traceMany ((time, tid, tlbl, event):ts) trace =
    Trace time tid tlbl event (traceMany ts trace)

lookupThreadLabel :: ThreadId -> Map ThreadId (Thread s a) -> Maybe ThreadLabel
lookupThreadLabel tid threads = join (threadLabel <$> Map.lookup tid threads)


-- | The most general method of running 'IOSim' is in 'ST' monad.  One can
-- recover failures or the result from 'Trace' with 'traceResult', or access
-- 'TraceEvent's generated by the computation with 'traceEvents'.  A slightly
-- more convenient way is exposed by 'runSimTrace'.
--
runSimTraceST :: forall s a. IOSim s a -> ST s (Trace a)
runSimTraceST mainAction = schedule mainThread initialState
  where
    mainThread =
      Thread {
        threadId      = ThreadId 0,
        threadControl = ThreadControl (runIOSim mainAction) MainFrame,
        threadBlocked = False,
        threadMasking = Unmasked,
        threadThrowTo = [],
        threadClockId = ClockId 0,
        threadLabel   = Just "main"
      }


--
-- Executing STM Transactions
--

data TVar s a = TVar {

       -- | The identifier of this var.
       --
       tvarId      :: !TVarId,

       -- | Label.
       tvarLabel   :: !(STRef s (Maybe TVarLabel)),

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

execAtomically :: forall s a c.
                  Time
               -> ThreadId
               -> Maybe ThreadLabel
               -> TVarId
               -> StmA s a
               -> (StmTxResult s a -> ST s (Trace c))
               -> ST s (Trace c)
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
       -> ST s (Trace c)
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
          k0 $ StmTxCommitted x (reverse writtenSeq) nextVid

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
        k0 $ StmTxAborted (toException e)

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
        go ctl read written writtenSeq (succ nextVid) (k v)

      LabelTVar !label tvar k -> do
        writeSTRef (tvarLabel tvar) $! (Just label)
        go ctl read written writtenSeq nextVid k

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

      SayStm msg k -> do
        trace <- go ctl read written writtenSeq nextVid k
        return $ Trace time tid tlbl (EventSay msg) trace

      OutputStm x k -> do
        trace <- go ctl read written writtenSeq nextVid k
        return $ Trace time tid tlbl (EventLog x) trace

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
    return TVar {tvarId = nextVid, tvarLabel,
                 tvarCurrent, tvarUndo, tvarBlocked}

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
