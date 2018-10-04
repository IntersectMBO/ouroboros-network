{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module MonadClass (
  MonadSay(..),
  MonadProbe(..),
  MonadRunProbe(..),
  ProbeTrace,
  TimeMeasure(..),
  MonadTimer(..),
  MonadFork(..),
  MonadConc(..),
  MonadSendRecv(..),
  MonadSTM(..),
  newEmptyMVar,
  newMVar
  ) where

import qualified Control.Concurrent as IO
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Monad (void)
import qualified Control.Monad.STM as STM
import System.Clock (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)

import qualified GHC.Event as GHC (TimeoutCallback, TimeoutKey, getSystemTimerManager,
                                   registerTimeout, unregisterTimeout, updateTimeout)

class Monad m => MonadSay m where
  say :: String -> m ()

instance MonadSay IO where
  say = print

type ProbeTrace m a = [(Time m, a)]

class MonadTimer m => MonadProbe m where
  type Probe m :: * -> *
  probeOutput :: Probe m a -> a -> m ()

class (MonadProbe m, Monad n) => MonadRunProbe m n | m -> n, n -> m where
  newProbe    :: n (Probe m a)
  readProbe   :: Probe m a -> n (ProbeTrace m a)
  runM        :: m () -> n ()

class (Monad m, TimeMeasure (Time m)) => MonadTimer m where
  type Time m :: *
  timer :: Duration (Time m) -> m () -> m ()

class (Ord t, Ord (Duration t), Num (Duration t)) => TimeMeasure t where
  type Duration t :: *

  diffTime :: t -> t -> Duration t
  addTime  :: Duration t -> t -> t

class Monad m => MonadFork m where
  fork    :: m () -> m ()

class MonadFork m => MonadConc m where
  type MVar m :: * -> *

  newEmptyNamedMVar :: Maybe String -> m (MVar m a)
  newNamedMVar :: Maybe String -> a -> m (MVar m a)
  takeMVar     :: MVar m a -> m a
  tryTakeMVar  :: MVar m a -> m (Maybe a)
  putMVar      :: MVar m a -> a -> m ()
  tryPutMVar   :: MVar m a -> a -> m Bool

  readMVar     :: MVar m a -> m a
  readMVar v = do x <- takeMVar v; putMVar v x; return x

  modifyMVar   :: MVar m a -> (a -> m (a, b)) -> m b
  modifyMVar v a = do
    x <- takeMVar v
    (x', y) <- a x
    putMVar v x'
    return y

  modifyMVar_   :: MVar m a -> (a -> m a) -> m ()
  modifyMVar_ v a = do
    x  <- takeMVar v
    x' <- a x
    putMVar v x'

newEmptyMVar :: MonadConc m => m (MVar m a)
newEmptyMVar = newEmptyNamedMVar Nothing

newMVar :: MonadConc m => a -> m (MVar m a)
newMVar = newNamedMVar Nothing

class Monad m => MonadSendRecv m where
  type BiChan m :: * -> * -> *

  newChan :: m (BiChan m s r)
  sendMsg :: BiChan m s r -> s -> m ()
  recvMsg :: BiChan m s r -> m r

class (MonadFork m, Monad stm) => MonadSTM m stm | m -> stm, stm -> m where
  type TVar m :: * -> *

  atomically   :: stm a -> m a
  newTVar      :: a -> stm (TVar m a)
  readTVar     :: TVar m a -> stm a
  writeTVar    :: TVar m a -> a -> stm ()
  modifyTVar   :: TVar m a -> (a -> a) -> stm ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f
  modifyTVar'  :: TVar m a -> (a -> a) -> stm ()
  modifyTVar' v f = do
    a <- readTVar v
    writeTVar v $! f a
  retry        :: stm a
--orElse       :: stm a -> stm a -> stm a --TODO

  check        :: Bool -> stm ()
  check True  = return ()
  check False = retry


data TimeoutState = TimeoutPending | TimeoutFired | TimeoutCancelled

class MonadSTM m stm => MonadSTMTimer m stm where
  data Timeout m :: *

  timeoutState   :: Timeout m -> stm TimeoutState

  newTimeout     :: Duration (Time m) -> m (Timeout m)
  updateTimeout  :: Timeout m -> Duration (Time m) -> m ()
  cancelTimeout  :: Timeout m -> m ()

instance MonadFork IO where
  fork a = void $ IO.forkIO a

instance MonadSTM IO STM.STM where
  type TVar IO = STM.TVar

  atomically  = STM.atomically
  newTVar     = STM.newTVar
  readTVar    = STM.readTVar
  writeTVar   = STM.writeTVar
  retry       = STM.retry
  modifyTVar  = STM.modifyTVar
  modifyTVar' = STM.modifyTVar'
  check       = STM.check

instance MonadTimer IO where
  type Time IO = Int -- microseconds

  timer t a = void $ IO.forkIO (IO.threadDelay t >> a)

instance TimeMeasure Int where
  type Duration Int = Int -- microseconds

  diffTime t t' = t-t'
  addTime  d t  = t+d

instance MonadSTMTimer IO STM.STM where
  data Timeout IO = TimeoutIO !(STM.TVar TimeoutState) !GHC.TimeoutKey

  timeoutState (TimeoutIO var _key) = STM.readTVar var

  newTimeout = \usec -> do
      var <- STM.newTVarIO TimeoutPending
      mgr <- GHC.getSystemTimerManager
      key <- GHC.registerTimeout mgr usec (STM.atomically (timeoutAction var))
      return (TimeoutIO var key)
    where
      timeoutAction var = do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutFired
          TimeoutFired     -> error "MonadSTMTimer(IO): invariant violation"
          TimeoutCancelled -> return ()

  updateTimeout (TimeoutIO _var key) usec = do
      mgr <- GHC.getSystemTimerManager
      GHC.updateTimeout mgr key usec

  cancelTimeout (TimeoutIO var key) = do
      STM.atomically $ do
        x <- STM.readTVar var
        case x of
          TimeoutPending   -> STM.writeTVar var TimeoutCancelled
          TimeoutFired     -> return ()
          TimeoutCancelled -> return ()
      mgr <- GHC.getSystemTimerManager
      GHC.unregisterTimeout mgr key

newtype ProbeIO a = ProbeIO { getProbeIO :: STM.TVar [(Int, a)] }

instance MonadProbe IO IO where
  type Probe IO = ProbeIO
  newProbe  = ProbeIO <$> STM.newTVarIO []
  readProbe (ProbeIO p) = STM.readTVarIO p
  probeOutput (ProbeIO p) a = do
    t <- toMicroseconds <$> getTime Monotonic
    -- the user is not exposed to the inner TVar, so it should never block for
    -- too long.
    atomically $ STM.modifyTVar' p ((0,a):)
    where
      toMicroseconds :: TimeSpec -> Int
      toMicroseconds = fromIntegral . (div 1000) . toNanoSecs

  -- In the above the starting state is pending, there is only one transaction
  -- that goes from pending to fired, and only one that goes from pending to
  -- cancelled. We can see from this that the state changes at most once, even
  -- if there were multiple uses of cancel or update, or even if the ghc timer
  -- manager ran the action multiple times. For example, there can be a race
  -- between firing and cancelling, but one will win and the state will change
  -- only once, so the final state is stable.
