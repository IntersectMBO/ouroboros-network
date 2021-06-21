{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Control.Monad.Class.MonadSTM
  ( MonadSTM (..)
  , MonadSTMTx (..)
  , MonadLabelledSTM (..)
  , MonadLabelledSTMTx (..)
  , LazyTVar
  , LazyTMVar
  , TVar
  , TMVar
  , TQueue
  , TBQueue

  -- * Default 'TMVar' implementation
  , TMVarDefault (..)
  , labelTMVarDefault
  , newTMVarDefault
  , newTMVarIODefault
  , newEmptyTMVarDefault
  , newEmptyTMVarIODefault
  , takeTMVarDefault
  , tryTakeTMVarDefault
  , putTMVarDefault
  , tryPutTMVarDefault
  , readTMVarDefault
  , tryReadTMVarDefault
  , swapTMVarDefault
  , isEmptyTMVarDefault

  -- * Default 'TBQueue' implementation
  , TQueueDefault (..)
  , labelTQueueDefault
  , newTQueueDefault
  , readTQueueDefault
  , tryReadTQueueDefault
  , peekTQueueDefault
  , tryPeekTQueueDefault
  , writeTQueueDefault
  , isEmptyTQueueDefault

  -- * Default 'TBQueue' implementation
  , TBQueueDefault (..)
  , labelTBQueueDefault
  , newTBQueueDefault
  , readTBQueueDefault
  , tryReadTBQueueDefault
  , peekTBQueueDefault
  , tryPeekTBQueueDefault
  , writeTBQueueDefault
  , isEmptyTBQueueDefault
  , isFullTBQueueDefault
  , lengthTBQueueDefault
  , flushTBQueueDefault

  -- * MonadThrow aliases
  , throwSTM
  , catchSTM

  -- * Deprecated API
  , newTVarM
  , newTMVarM
  , newTMVarMDefault
  , newEmptyTMVarM
  , newEmptyTMVarMDefault
  ) where

import           Prelude hiding (read)

import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import qualified Control.Monad.Class.MonadThrow as MonadThrow

import           Control.Applicative (Alternative (..))
import           Control.Exception
import           Control.Monad.Reader
import           Data.Kind (Type)
import           GHC.Stack
import           Numeric.Natural (Natural)


{-# DEPRECATED LazyTVar  "Renamed back to 'TVar'" #-}
{-# DEPRECATED LazyTMVar "Renamed back to 'TMVar'" #-}
type LazyTVar  m = TVar m
type LazyTMVar m = TMVar m

-- The STM primitives
class ( Monad stm
      , Alternative stm
      , MonadPlus stm
      ) => MonadSTMTx stm where
  type TVar_ stm :: Type -> Type

  newTVar      :: a -> stm (TVar_ stm a)
  readTVar     :: TVar_ stm a -> stm a
  writeTVar    :: TVar_ stm a -> a -> stm ()
  retry        :: stm a
  orElse       :: stm a -> stm a -> stm a

  modifyTVar   :: TVar_ stm a -> (a -> a) -> stm ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f

  modifyTVar'  :: TVar_ stm a -> (a -> a) -> stm ()
  modifyTVar' v f = readTVar v >>= \x -> writeTVar v $! f x

  -- | @since io-classes-0.2.0.0
  stateTVar    :: TVar_ stm s -> (s -> (a, s)) -> stm a
  stateTVar    = stateTVarDefault

  swapTVar     :: TVar_ stm a -> a -> stm a
  swapTVar     = swapTVarDefault

  check        :: Bool -> stm ()
  check True = return ()
  check _    = retry

  -- Additional derived STM APIs
  type TMVar_ stm :: Type -> Type
  newTMVar        :: a -> stm (TMVar_ stm a)
  newEmptyTMVar   ::      stm (TMVar_ stm a)
  takeTMVar       :: TMVar_ stm a      -> stm a
  tryTakeTMVar    :: TMVar_ stm a      -> stm (Maybe a)
  putTMVar        :: TMVar_ stm a -> a -> stm ()
  tryPutTMVar     :: TMVar_ stm a -> a -> stm Bool
  readTMVar       :: TMVar_ stm a      -> stm a
  tryReadTMVar    :: TMVar_ stm a      -> stm (Maybe a)
  swapTMVar       :: TMVar_ stm a -> a -> stm a
  isEmptyTMVar    :: TMVar_ stm a      -> stm Bool

  type TQueue_ stm :: Type -> Type
  newTQueue      :: stm (TQueue_ stm a)
  readTQueue     :: TQueue_ stm a -> stm a
  tryReadTQueue  :: TQueue_ stm a -> stm (Maybe a)
  peekTQueue     :: TQueue_ stm a -> stm a
  tryPeekTQueue  :: TQueue_ stm a -> stm (Maybe a)
  writeTQueue    :: TQueue_ stm a -> a -> stm ()
  isEmptyTQueue  :: TQueue_ stm a -> stm Bool

  type TBQueue_ stm :: Type -> Type
  newTBQueue     :: Natural -> stm (TBQueue_ stm a)
  readTBQueue    :: TBQueue_ stm a -> stm a
  tryReadTBQueue :: TBQueue_ stm a -> stm (Maybe a)
  peekTBQueue    :: TBQueue_ stm a -> stm a
  tryPeekTBQueue :: TBQueue_ stm a -> stm (Maybe a)
  flushTBQueue   :: TBQueue_ stm a -> stm [a]
  writeTBQueue   :: TBQueue_ stm a -> a -> stm ()
  -- | @since 0.2.0.0
  lengthTBQueue  :: TBQueue_ stm a -> stm Natural
  isEmptyTBQueue :: TBQueue_ stm a -> stm Bool
  isFullTBQueue  :: TBQueue_ stm a -> stm Bool


stateTVarDefault :: MonadSTMTx stm => TVar_ stm s -> (s -> (a, s)) -> stm a
stateTVarDefault var f = do
   s <- readTVar var
   let (a, s') = f s
   writeTVar var s'
   return a

swapTVarDefault :: MonadSTMTx stm => TVar_ stm a -> a -> stm a
swapTVarDefault var new = do
    old <- readTVar var
    writeTVar var new
    return old

type TVar    m = TVar_    (STM m)
type TMVar   m = TMVar_   (STM m)
type TQueue  m = TQueue_  (STM m)
type TBQueue m = TBQueue_ (STM m)

class (Monad m, MonadSTMTx (STM m)) => MonadSTM m where
  -- STM transactions
  type STM m :: Type -> Type

  atomically :: HasCallStack => STM m a -> m a

  -- Helpful derived functions with default implementations

  newTVarIO        :: a -> m (TVar  m a)
  readTVarIO       :: TVar m a -> m a
  newTMVarIO       :: a -> m (TMVar m a)
  newEmptyTMVarIO  ::      m (TMVar m a)
  newTQueueIO      :: m (TQueue m a)
  newTBQueueIO     :: Natural -> m (TBQueue m a)

  newTVarIO       = atomically . newTVar
  readTVarIO      = atomically . readTVar
  newTMVarIO      = atomically . newTMVar
  newEmptyTMVarIO = atomically   newEmptyTMVar
  newTQueueIO     = atomically newTQueue
  newTBQueueIO    = atomically . newTBQueue


newTVarM :: MonadSTM m => a -> m (TVar  m a)
newTVarM = newTVarIO
{-# DEPRECATED newTVarM "Use newTVarIO" #-}

newTMVarM :: MonadSTM m => a -> m (TMVar m a)
newTMVarM = newTMVarIO
{-# DEPRECATED newTMVarM "Use newTMVarIO" #-}

newEmptyTMVarM  :: MonadSTM m => m (TMVar m a)
newEmptyTMVarM = newEmptyTMVarIO
{-# DEPRECATED newEmptyTMVarM "Use newEmptyTMVarIO" #-}


-- | Labelled 'TVar's, 'TMVar's, 'TQueue's and 'TBQueue's.
--
class MonadSTMTx stm => MonadLabelledSTMTx stm where
  labelTVar    :: TVar_    stm a -> String -> stm ()
  labelTMVar   :: TMVar_   stm a -> String -> stm ()
  labelTQueue  :: TQueue_  stm a -> String -> stm ()
  labelTBQueue :: TBQueue_ stm a -> String -> stm ()

-- | A convenience class which provides 'MonadSTM' and 'MonadLabelledSTMTx'
-- constraints.
--
class (MonadSTM m, MonadLabelledSTMTx (STM m))
   => MonadLabelledSTM m where
  labelTVarIO    :: TVar    m a -> String -> m ()
  labelTMVarIO   :: TMVar   m a -> String -> m ()
  labelTQueueIO  :: TQueue  m a -> String -> m ()
  labelTBQueueIO :: TBQueue m a -> String -> m ()

  default labelTVarIO :: TVar m a -> String -> m ()
  labelTVarIO = \v l -> atomically (labelTVar v l)

  default labelTMVarIO :: TMVar m a -> String -> m ()
  labelTMVarIO = \v l -> atomically (labelTMVar v l)

  default labelTQueueIO :: TQueue m a -> String -> m ()
  labelTQueueIO = \v l -> atomically (labelTQueue v l)

  default labelTBQueueIO :: TBQueue m a -> String -> m ()
  labelTBQueueIO = \v l -> atomically (labelTBQueue v l)

--
-- Instance for IO uses the existing STM library implementations
--

instance MonadSTMTx STM.STM where
  type TVar_    STM.STM = STM.TVar
  type TMVar_   STM.STM = STM.TMVar
  type TQueue_  STM.STM = STM.TQueue
  type TBQueue_ STM.STM = STM.TBQueue

  newTVar        = STM.newTVar
  readTVar       = STM.readTVar
  writeTVar      = STM.writeTVar
  retry          = STM.retry
  orElse         = STM.orElse
  modifyTVar     = STM.modifyTVar
  modifyTVar'    = STM.modifyTVar'
  stateTVar      = STM.stateTVar
  swapTVar       = STM.swapTVar
  check          = STM.check
  newTMVar       = STM.newTMVar
  newEmptyTMVar  = STM.newEmptyTMVar
  takeTMVar      = STM.takeTMVar
  tryTakeTMVar   = STM.tryTakeTMVar
  putTMVar       = STM.putTMVar
  tryPutTMVar    = STM.tryPutTMVar
  readTMVar      = STM.readTMVar
  tryReadTMVar   = STM.tryReadTMVar
  swapTMVar      = STM.swapTMVar
  isEmptyTMVar   = STM.isEmptyTMVar
  newTQueue      = STM.newTQueue
  readTQueue     = STM.readTQueue
  tryReadTQueue  = STM.tryReadTQueue
  peekTQueue     = STM.peekTQueue
  tryPeekTQueue  = STM.tryPeekTQueue
  flushTBQueue   = STM.flushTBQueue
  writeTQueue    = STM.writeTQueue
  isEmptyTQueue  = STM.isEmptyTQueue
  newTBQueue     = STM.newTBQueue
  readTBQueue    = STM.readTBQueue
  tryReadTBQueue = STM.tryReadTBQueue
  peekTBQueue    = STM.peekTBQueue
  tryPeekTBQueue = STM.tryPeekTBQueue
  writeTBQueue   = STM.writeTBQueue
  lengthTBQueue  = STM.lengthTBQueue
  isEmptyTBQueue = STM.isEmptyTBQueue
  isFullTBQueue  = STM.isFullTBQueue


instance MonadSTM IO where
  type STM IO = STM.STM

  atomically = wrapBlockedIndefinitely . STM.atomically

  newTVarIO       = STM.newTVarIO
  readTVarIO      = STM.readTVarIO
  newTMVarIO      = STM.newTMVarIO
  newEmptyTMVarIO = STM.newEmptyTMVarIO
  newTQueueIO     = STM.newTQueueIO
  newTBQueueIO    = STM.newTBQueueIO

-- | noop instance
--
instance MonadLabelledSTMTx STM.STM where
  labelTVar    = \_  _ -> return ()
  labelTMVar   = \_  _ -> return ()
  labelTQueue  = \_  _ -> return ()
  labelTBQueue = \_  _ -> return ()

-- | noop instance
--
instance MonadLabelledSTM IO where
  labelTVarIO    = \_  _ -> return ()
  labelTMVarIO   = \_  _ -> return ()
  labelTQueueIO  = \_  _ -> return ()
  labelTBQueueIO = \_  _ -> return ()

-- | Wrapper around 'BlockedIndefinitelyOnSTM' that stores a call stack
data BlockedIndefinitely = BlockedIndefinitely {
      blockedIndefinitelyCallStack :: CallStack
    , blockedIndefinitelyException :: BlockedIndefinitelyOnSTM
    }
  deriving Show

instance Exception BlockedIndefinitely where
  displayException (BlockedIndefinitely cs e) = unlines [
        displayException e
      , prettyCallStack cs
      ]

wrapBlockedIndefinitely :: HasCallStack => IO a -> IO a
wrapBlockedIndefinitely = handle (throwIO . BlockedIndefinitely callStack)

--
-- Lift to monad transformers
--

instance MonadSTM m => MonadSTM (ReaderT r m) where
  type STM (ReaderT r m) = STM m
  atomically      = lift . atomically
  newTVarIO       = lift . newTVarM
  readTVarIO      = lift . readTVarIO
  newTMVarIO      = lift . newTMVarM
  newEmptyTMVarIO = lift   newEmptyTMVarM
  newTQueueIO     = lift   newTQueueIO
  newTBQueueIO    = lift . newTBQueueIO

--
-- Default TMVar implementation in terms of TVars (used by sim)
--

newtype TMVarDefault m a = TMVar (TVar m (Maybe a))

labelTMVarDefault
  :: MonadLabelledSTM m
  => TMVarDefault m a -> String -> STM m ()
labelTMVarDefault (TMVar tvar) = labelTVar tvar

newTMVarDefault :: MonadSTM m => a -> STM m (TMVarDefault m a)
newTMVarDefault a = do
  t <- newTVar (Just a)
  return (TMVar t)

newTMVarIODefault :: MonadSTM m => a -> m (TMVarDefault m a)
newTMVarIODefault a = do
  t <- newTVarM (Just a)
  return (TMVar t)

newTMVarMDefault :: MonadSTM m => a -> m (TMVarDefault m a)
newTMVarMDefault = newTMVarIODefault
{-# DEPRECATED newTMVarMDefault "Use newTMVarIODefault" #-}

newEmptyTMVarDefault :: MonadSTM m => STM m (TMVarDefault m a)
newEmptyTMVarDefault = do
  t <- newTVar Nothing
  return (TMVar t)

newEmptyTMVarIODefault :: MonadSTM m => m (TMVarDefault m a)
newEmptyTMVarIODefault = do
  t <- newTVarIO Nothing
  return (TMVar t)

newEmptyTMVarMDefault :: MonadSTM m => m (TMVarDefault m a)
newEmptyTMVarMDefault = newEmptyTMVarIODefault
{-# DEPRECATED newEmptyTMVarMDefault "Use newEmptyTMVarIODefault" #-}

takeTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m a
takeTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> do writeTVar t Nothing; return a

tryTakeTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m (Maybe a)
tryTakeTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return Nothing
    Just a  -> do writeTVar t Nothing; return (Just a)

putTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> STM m ()
putTMVarDefault (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return ()
    Just _  -> retry

tryPutTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> STM m Bool
tryPutTMVarDefault (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return True
    Just _  -> return False

readTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m a
readTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> return a

tryReadTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m (Maybe a)
tryReadTMVarDefault (TMVar t) = readTVar t

swapTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> STM m a
swapTMVarDefault (TMVar t) new = do
  m <- readTVar t
  case m of
    Nothing  -> retry
    Just old -> do writeTVar t (Just new); return old

isEmptyTMVarDefault :: MonadSTM m => TMVarDefault m a -> STM m Bool
isEmptyTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return True
    Just _  -> return False

--
-- Default TQueue implementation in terms of TVars (used by sim)
--

data TQueueDefault m a = TQueue !(TVar m [a])
                                !(TVar m [a])

labelTQueueDefault
  :: MonadLabelledSTM m
  => TQueueDefault m a -> String -> STM m ()
labelTQueueDefault (TQueue read write) label = do
  labelTVar read (label ++ "-read")
  labelTVar write (label ++ "-write")

newTQueueDefault :: MonadSTM m => STM m (TQueueDefault m a)
newTQueueDefault = do
  read  <- newTVar []
  write <- newTVar []
  return (TQueue read write)

writeTQueueDefault :: MonadSTM m => TQueueDefault m a -> a -> STM m ()
writeTQueueDefault (TQueue _read write) a = do
  listend <- readTVar write
  writeTVar write (a:listend)

readTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m a
readTQueueDefault queue = maybe retry return =<< tryReadTQueueDefault queue

tryReadTQueueDefault :: MonadSTMTx (STM m) => TQueueDefault m a -> STM m (Maybe a)
tryReadTQueueDefault (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (x:xs') -> do
      writeTVar read xs'
      return (Just x)
    [] -> do
      ys <- readTVar write
      case reverse ys of
        []     -> return Nothing
        (z:zs) -> do
          writeTVar write []
          writeTVar read zs
          return (Just z)

isEmptyTQueueDefault :: MonadSTMTx (STM m) => TQueueDefault m a -> STM m Bool
isEmptyTQueueDefault (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readTVar write
             case ys of
               [] -> return True
               _  -> return False

peekTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m a
peekTQueueDefault (TQueue read _write) = do
    xs <- readTVar read
    case xs of
      (x:_) -> return x
      _     -> retry

tryPeekTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
tryPeekTQueueDefault (TQueue read _write) = do
    xs <- readTVar read
    case xs of
      (x:_) -> return (Just x)
      _     -> return Nothing

--
-- Default TBQueue implementation in terms of TVars (used by sim)
--

data TBQueueDefault m a = TBQueue
  !(TVar m Natural) -- read capacity
  !(TVar m [a])     -- elements waiting for read
  !(TVar m Natural) -- write capacity
  !(TVar m [a])     -- written elements
  !Natural

labelTBQueueDefault
  :: MonadLabelledSTM m
  => TBQueueDefault m a -> String -> STM m ()
labelTBQueueDefault (TBQueue rsize read wsize write _size) label = do
  labelTVar rsize (label ++ "-rsize")
  labelTVar read (label ++ "-read")
  labelTVar wsize (label ++ "-wsize")
  labelTVar write (label ++ "-write")

newTBQueueDefault :: MonadSTM m => Natural -> STM m (TBQueueDefault m a)
newTBQueueDefault size = do
  rsize <- newTVar 0
  read  <- newTVar []
  wsize <- newTVar size
  write <- newTVar []
  return (TBQueue rsize read wsize write size)

readTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
readTBQueueDefault queue = maybe retry return =<< tryReadTBQueueDefault queue

tryReadTBQueueDefault :: MonadSTMTx (STM m) => TBQueueDefault m a -> STM m (Maybe a)
tryReadTBQueueDefault (TBQueue rsize read _wsize write _size) = do
  xs <- readTVar read
  r <- readTVar rsize
  writeTVar rsize $! r + 1
  case xs of
    (x:xs') -> do
      writeTVar read xs'
      return (Just x)
    [] -> do
      ys <- readTVar write
      case reverse ys of
        [] -> return Nothing

        -- NB. lazy: we want the transaction to be 
        -- short, otherwise it will conflict       
        (z:zs)  -> do
          writeTVar write []
          writeTVar read zs
          return (Just z)

peekTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
peekTBQueueDefault (TBQueue _rsize read _wsize _write _size) = do
    xs <- readTVar read
    case xs of
      (x:_) -> return x
      _     -> retry

tryPeekTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m (Maybe a)
tryPeekTBQueueDefault (TBQueue _rsize read _wsize _write _size) = do
    xs <- readTVar read
    case xs of
      (x:_) -> return (Just x)
      _     -> return Nothing

writeTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> a -> STM m ()
writeTBQueueDefault (TBQueue rsize _read wsize write _size) a = do
  w <- readTVar wsize
  if (w > 0)
    then do writeTVar wsize $! w - 1
    else do
          r <- readTVar rsize
          if (r > 0)
            then do writeTVar rsize 0
                    writeTVar wsize $! r - 1
            else retry
  listend <- readTVar write
  writeTVar write (a:listend)

isEmptyTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Bool
isEmptyTBQueueDefault (TBQueue _rsize read _wsize write _size) = do
  xs <- readTVar read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readTVar write
             case ys of
               [] -> return True
               _  -> return False

isFullTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Bool
isFullTBQueueDefault (TBQueue rsize _read wsize _write _size) = do
  w <- readTVar wsize
  if (w > 0)
     then return False
     else do
         r <- readTVar rsize
         if (r > 0)
            then return False
            else return True

lengthTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m Natural
lengthTBQueueDefault (TBQueue rsize _read wsize _write size) = do
  r <- readTVar rsize
  w <- readTVar wsize
  return $! size - r - w


flushTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m [a]
flushTBQueueDefault (TBQueue rsize read wsize write size) = do
  xs <- readTVar read
  ys <- readTVar write
  if null xs && null ys
    then return []
    else do
      writeTVar read []
      writeTVar write []
      writeTVar rsize 0
      writeTVar wsize size
      return (xs ++ reverse ys)


-- | 'throwIO' specialised to @stm@ monad.
--
throwSTM :: (MonadSTMTx stm, MonadThrow.MonadThrow stm, Exception e)
         => e -> stm a
throwSTM = MonadThrow.throwIO


-- | 'catch' speclialized for an @stm@ monad.
--
catchSTM :: (MonadSTMTx stm, MonadThrow.MonadCatch stm, Exception e)
         => stm a -> (e -> stm a) -> stm a
catchSTM = MonadThrow.catch
