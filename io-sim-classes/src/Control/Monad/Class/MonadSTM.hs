{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Control.Monad.Class.MonadSTM
  ( MonadSTM (..)
  , Tr

  -- * Helpers defined in terms of 'MonadSTM'
  , updateTVar

  -- * Default 'TMVar' implementation
  , TMVarDefault (..)
  , newTMVarDefault
  , newTMVarMDefault
  , newEmptyTMVarDefault
  , newEmptyTMVarMDefault
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
  , newTQueueDefault
  , readTQueueDefault
  , tryReadTQueueDefault
  , writeTQueueDefault
  , isEmptyTQueueDefault

  -- * Default 'TBQueue' implementation
  , TBQueueDefault (..)
  , newTBQueueDefault
  , readTBQueueDefault
  , tryReadTBQueueDefault
  , writeTBQueueDefault
  , isEmptyTBQueueDefault
  , isFullTBQueueDefault
  ) where

import           Prelude hiding (read)

import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TQueue as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Control.Monad.STM as STM

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           GHC.Stack
import           Numeric.Natural (Natural)

{-# DEPRECATED Tr "Now simply called 'STM'" #-}
type Tr m = STM m

class (Monad m, Monad (STM m)) => MonadSTM m where

  -- STM transactions
  type STM  m = (n :: * -> *) | n -> m
  -- The STM primitives
  type LazyTVar m :: * -> *

  atomically   :: HasCallStack => STM m a -> m a
  newTVar      :: a -> STM m (LazyTVar m a)
  readTVar     :: LazyTVar m a -> STM m a
  writeTVar    :: LazyTVar m a -> a -> STM m ()
  retry        :: STM m a
--orElse       :: STM m a -> STM m a -> STM m a --TODO

  -- Helpful derived functions with default implementations
  newTVarM     :: a -> m (LazyTVar m a)
  newTVarM     = atomically . newTVar

  modifyTVar   :: LazyTVar m a -> (a -> a) -> STM m ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f

  check        :: Bool -> STM m ()
  check True = return ()
  check _    = retry

  -- Additional derived STM APIs
  type LazyTMVar m :: * -> *
  newTMVar        :: a -> STM m (LazyTMVar m a)
  newTMVarM       :: a -> m     (LazyTMVar m a)
  newEmptyTMVar   ::      STM m (LazyTMVar m a)
  newEmptyTMVarM  ::      m     (LazyTMVar m a)
  takeTMVar       :: LazyTMVar m a      -> STM m a
  tryTakeTMVar    :: LazyTMVar m a      -> STM m (Maybe a)
  putTMVar        :: LazyTMVar m a -> a -> STM m ()
  tryPutTMVar     :: LazyTMVar m a -> a -> STM m Bool
  readTMVar       :: LazyTMVar m a      -> STM m a
  tryReadTMVar    :: LazyTMVar m a      -> STM m (Maybe a)
  swapTMVar       :: LazyTMVar m a -> a -> STM m a
  isEmptyTMVar    :: LazyTMVar m a      -> STM m Bool

  type TQueue m  :: * -> *
  newTQueue      :: STM m (TQueue m a)
  readTQueue     :: TQueue m a -> STM m a
  tryReadTQueue  :: TQueue m a -> STM m (Maybe a)
  writeTQueue    :: TQueue m a -> a -> STM m ()
  isEmptyTQueue  :: TQueue m a -> STM m Bool

  type TBQueue m :: * -> *
  newTBQueue     :: Natural -> STM m (TBQueue m a)
  readTBQueue    :: TBQueue m a -> STM m a
  tryReadTBQueue :: TBQueue m a -> STM m (Maybe a)
  writeTBQueue   :: TBQueue m a -> a -> STM m ()
  isEmptyTBQueue :: TBQueue m a -> STM m Bool
  isFullTBQueue  :: TBQueue m a -> STM m Bool

instance MonadSTM m => MonadSTM (ReaderT e m) where
  type STM       (ReaderT e m) = ReaderT e (STM m)
  type LazyTVar  (ReaderT e m) = LazyTVar m
  type LazyTMVar (ReaderT e m) = LazyTMVar m
  type TQueue    (ReaderT e m) = TQueue m
  type TBQueue   (ReaderT e m) = TBQueue m

  atomically (ReaderT t) = ReaderT $ \e -> atomically (t e)
  newTVar          = lift . newTVar
  readTVar         = lift . readTVar
  writeTVar t a    = lift $ writeTVar t a
  retry            = lift retry

  newTMVar         = lift . newTMVar
  newTMVarM        = lift . newTMVarM
  newEmptyTMVar    = lift newEmptyTMVar
  newEmptyTMVarM   = lift newEmptyTMVarM
  takeTMVar        = lift . takeTMVar
  tryTakeTMVar     = lift . tryTakeTMVar
  putTMVar   t a   = lift $ putTMVar t a
  tryPutTMVar t a  = lift $ tryPutTMVar t a
  readTMVar        = lift . readTMVar
  tryReadTMVar     = lift . tryReadTMVar
  swapTMVar t a    = lift $ swapTMVar t a
  isEmptyTMVar     = lift . isEmptyTMVar

  newTQueue        = lift $ newTQueue
  readTQueue       = lift . readTQueue
  tryReadTQueue    = lift . tryReadTQueue
  writeTQueue q a  = lift $ writeTQueue q a
  isEmptyTQueue    = lift . isEmptyTQueue

  newTBQueue       = lift . newTBQueue
  readTBQueue      = lift . readTBQueue
  tryReadTBQueue   = lift . tryReadTBQueue
  writeTBQueue q a = lift $ writeTBQueue q a
  isEmptyTBQueue   = lift . isEmptyTBQueue
  isFullTBQueue    = lift . isFullTBQueue

instance (Show e, MonadSTM m) => MonadSTM (ExceptT e m) where
  type STM       (ExceptT e m) = ExceptT e (STM m)
  type LazyTVar  (ExceptT e m) = LazyTVar m
  type LazyTMVar (ExceptT e m) = LazyTMVar m
  type TQueue    (ExceptT e m) = TQueue m
  type TBQueue   (ExceptT e m) = TBQueue m

  atomically (ExceptT t) = ExceptT $ atomically t
  newTVar                = lift . newTVar
  readTVar               = lift . readTVar
  writeTVar t a          = lift $ writeTVar t a
  retry                  = lift retry

  newTMVar               = lift . newTMVar
  newTMVarM              = lift . newTMVarM
  newEmptyTMVar          = lift newEmptyTMVar
  newEmptyTMVarM         = lift newEmptyTMVarM
  takeTMVar              = lift . takeTMVar
  tryTakeTMVar           = lift . tryTakeTMVar
  putTMVar   t a         = lift $ putTMVar t a
  tryPutTMVar t a        = lift $ tryPutTMVar t a
  readTMVar              = lift . readTMVar
  tryReadTMVar           = lift . tryReadTMVar
  swapTMVar t a          = lift $ swapTMVar t a
  isEmptyTMVar           = lift . isEmptyTMVar

  newTQueue        = lift $ newTQueue
  readTQueue       = lift . readTQueue
  tryReadTQueue    = lift . tryReadTQueue
  writeTQueue q a  = lift $ writeTQueue q a
  isEmptyTQueue    = lift . isEmptyTQueue

  newTBQueue       = lift . newTBQueue
  readTBQueue      = lift . readTBQueue
  tryReadTBQueue   = lift . tryReadTBQueue
  writeTBQueue q a = lift $ writeTBQueue q a
  isEmptyTBQueue   = lift . isEmptyTBQueue
  isFullTBQueue    = lift . isFullTBQueue


--
-- Instance for IO uses the existing STM library implementations
--

instance MonadSTM IO where
  type STM      IO = STM.STM
  type LazyTVar IO = STM.TVar

  atomically  = wrapBlockedIndefinitely . STM.atomically
  newTVar     = STM.newTVar
  readTVar    = STM.readTVar
  writeTVar   = STM.writeTVar
  retry       = STM.retry

  newTVarM    = STM.newTVarIO
  modifyTVar  = STM.modifyTVar
  check       = STM.check

  type LazyTMVar IO = STM.TMVar

  newTMVar        = STM.newTMVar
  newTMVarM       = STM.newTMVarIO
  newEmptyTMVar   = STM.newEmptyTMVar
  newEmptyTMVarM  = STM.newEmptyTMVarIO
  takeTMVar       = STM.takeTMVar
  tryTakeTMVar    = STM.tryTakeTMVar
  putTMVar        = STM.putTMVar
  tryPutTMVar     = STM.tryPutTMVar
  readTMVar       = STM.readTMVar
  tryReadTMVar    = STM.tryReadTMVar
  swapTMVar       = STM.swapTMVar
  isEmptyTMVar    = STM.isEmptyTMVar

  type TQueue IO  = STM.TQueue

  newTQueue       = STM.newTQueue
  readTQueue      = STM.readTQueue
  tryReadTQueue   = STM.tryReadTQueue
  writeTQueue     = STM.writeTQueue
  isEmptyTQueue   = STM.isEmptyTQueue

  type TBQueue IO = STM.TBQueue

#if MIN_VERSION_stm(2,5,0)
  newTBQueue     = STM.newTBQueue
#else
  -- STM prior to 2.5.0 takes an Int
  newTBQueue     = STM.newTBQueue . fromEnum
#endif
  readTBQueue    = STM.readTBQueue
  tryReadTBQueue = STM.tryReadTBQueue
  writeTBQueue   = STM.writeTBQueue
  isEmptyTBQueue = STM.isEmptyTBQueue
  isFullTBQueue  = STM.isFullTBQueue


-- | Wrapper around 'BlockedIndefinitelyOnSTM' that stores a call stack
data BlockedIndefinitely = BlockedIndefinitely {
      blockedIndefinitelyCallStack :: CallStack
    , blockedIndefinitelyException :: BlockedIndefinitelyOnSTM
    }
  deriving (Show)

instance Exception BlockedIndefinitely where
  displayException (BlockedIndefinitely cs e) = unlines [
        displayException e
      , prettyCallStack cs
      ]

wrapBlockedIndefinitely :: HasCallStack => IO a -> IO a
wrapBlockedIndefinitely = handle (throwIO . BlockedIndefinitely callStack)

--
-- Helpers defined in terms of MonadSTM
--

updateTVar :: MonadSTM m => LazyTVar m a -> (a -> (a, b)) -> STM m b
updateTVar t f = do
    a <- readTVar t
    let (a', b) = f a
    writeTVar t a'
    return b

--
-- Default TMVar implementation in terms of TVars (used by sim)
--

newtype TMVarDefault m a = TMVar (LazyTVar m (Maybe a))

newTMVarDefault :: MonadSTM m => a -> STM m (TMVarDefault m a)
newTMVarDefault a = do
  t <- newTVar (Just a)
  return (TMVar t)

newTMVarMDefault :: MonadSTM m => a -> m (TMVarDefault m a)
newTMVarMDefault a = do
  t <- newTVarM (Just a)
  return (TMVar t)

newEmptyTMVarDefault :: MonadSTM m => STM m (TMVarDefault m a)
newEmptyTMVarDefault = do
  t <- newTVar Nothing
  return (TMVar t)

newEmptyTMVarMDefault :: MonadSTM m => m (TMVarDefault m a)
newEmptyTMVarMDefault = do
  t <- newTVarM Nothing
  return (TMVar t)

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

data TQueueDefault m a = TQueue !(LazyTVar m [a])
                                !(LazyTVar m [a])

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

tryReadTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
tryReadTQueueDefault (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (x:xs') -> do
      writeTVar read xs'
      return (Just x)
    [] -> do
      ys <- readTVar write
      case ys of
        [] -> return Nothing
        _  -> do
          let (z:zs) = reverse ys
          writeTVar write []
          writeTVar read zs
          return (Just z)

isEmptyTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m Bool
isEmptyTQueueDefault (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readTVar write
             case ys of
               [] -> return True
               _  -> return False

--
-- Default TBQueue implementation in terms of TVars (used by sim)
--

data TBQueueDefault m a = TBQueue
  !(LazyTVar m Natural) -- read capacity
  !(LazyTVar m [a])     -- elements waiting for read
  !(LazyTVar m Natural) -- write capacity
  !(LazyTVar m [a])     -- written elements
  !Natural

newTBQueueDefault :: MonadSTM m => Natural -> STM m (TBQueueDefault m a)
newTBQueueDefault size = do
  rsize <- newTVar 0
  read  <- newTVar []
  wsize <- newTVar size
  write <- newTVar []
  return (TBQueue rsize read wsize write size)

readTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m a
readTBQueueDefault queue = maybe retry return =<< tryReadTBQueueDefault queue

tryReadTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> STM m (Maybe a)
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
      case ys of
        [] -> return Nothing
        _  -> do
          let (z:zs) = reverse ys -- NB. lazy: we want the transaction to be
                                  -- short, otherwise it will conflict
          writeTVar write []
          writeTVar read zs
          return (Just z)

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
