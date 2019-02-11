{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Control.Monad.Class.MonadSTM
  ( MonadSTM (..)
  , MonadFork (..)
  , TMVarDefault (..)
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
  , TBQueueDefault (..)
  , newTBQueueDefault
  , readTBQueueDefault
  , tryReadTBQueueDefault
  , writeTBQueueDefault
  ) where

import           Prelude hiding (read)

import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TMVar as STM
import qualified Control.Concurrent.STM.TVar as STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Control.Monad.STM as STM
import           GHC.Stack
import           Numeric.Natural (Natural)

import           Control.Monad.Class.MonadFork

class (MonadFork m, Monad (Tr m)) => MonadSTM m where

  -- STM transactions
  type Tr   m = (n :: * -> *) | n -> m
  -- The STM primitives
  type TVar m :: * -> *

  atomically   :: HasCallStack => Tr m a -> m a
  newTVar      :: a -> Tr m (TVar m a)
  readTVar     :: TVar m a -> Tr m a
  writeTVar    :: TVar m a -> a -> Tr m ()
  retry        :: Tr m a
--orElse       :: Tr m a -> Tr m a -> Tr m a --TODO

  -- Helpful derived functions with default implementations
  newTVarIO     :: a -> m (TVar m a)
  newTVarIO     = atomically . newTVar

  modifyTVar   :: TVar m a -> (a -> a) -> Tr m ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f

  modifyTVar'  :: TVar m a -> (a -> a) -> Tr m ()
  modifyTVar' v f = do
    a <- readTVar v
    writeTVar v $! f a

  check        :: Bool -> Tr m ()
  check True = return ()
  check _    = retry

  -- Additional derived STM APIs
  type TMVar m :: * -> *
  newTMVar        :: a -> Tr m (TMVar m a)
  newTMVarIO      :: a -> m   (TMVar m a)
  newEmptyTMVar   ::      Tr m (TMVar m a)
  newEmptyTMVarIO ::      m   (TMVar m a)
  takeTMVar       :: TMVar m a      -> Tr m a
  tryTakeTMVar    :: TMVar m a      -> Tr m (Maybe a)
  putTMVar        :: TMVar m a -> a -> Tr m ()
  tryPutTMVar     :: TMVar m a -> a -> Tr m Bool
  readTMVar       :: TMVar m a      -> Tr m a
  tryReadTMVar    :: TMVar m a      -> Tr m (Maybe a)
  swapTMVar       :: TMVar m a -> a -> Tr m a
  isEmptyTMVar    :: TMVar m a      -> Tr m Bool

  type TBQueue m :: * -> *
  newTBQueue     :: Natural -> Tr m (TBQueue m a)
  readTBQueue    :: TBQueue m a -> Tr m a
  tryReadTBQueue :: TBQueue m a -> Tr m (Maybe a)
  writeTBQueue   :: TBQueue m a -> a -> Tr m ()

instance MonadSTM m => MonadSTM (ReaderT e m) where
  type Tr (ReaderT e m)    = ReaderT e (Tr m)
  type TVar (ReaderT e m)  = TVar m
  type TMVar (ReaderT e m) = TMVar m
  type TBQueue (ReaderT e m) = TBQueue m

  atomically (ReaderT t) = ReaderT $ \e -> atomically (t e)
  newTVar          = lift . newTVar
  readTVar         = lift . readTVar
  writeTVar t a    = lift $ writeTVar t a
  retry            = lift retry

  newTMVar         = lift . newTMVar
  newTMVarIO       = lift . newTMVarIO
  newEmptyTMVar    = lift newEmptyTMVar
  newEmptyTMVarIO  = lift newEmptyTMVarIO
  takeTMVar        = lift . takeTMVar
  tryTakeTMVar     = lift . tryTakeTMVar
  putTMVar   t a   = lift $ putTMVar t a
  tryPutTMVar t a  = lift $ tryPutTMVar t a
  readTMVar        = lift . readTMVar
  tryReadTMVar     = lift . tryReadTMVar
  swapTMVar t a    = lift $ swapTMVar t a
  isEmptyTMVar     = lift . isEmptyTMVar

  newTBQueue       = lift . newTBQueue
  readTBQueue      = lift . readTBQueue
  tryReadTBQueue   = lift . tryReadTBQueue
  writeTBQueue q a = lift $ writeTBQueue q a

instance (Show e, MonadSTM m) => MonadSTM (ExceptT e m) where
  type Tr (ExceptT e m)      = ExceptT e (Tr m)
  type TVar (ExceptT e m)    = TVar m
  type TMVar (ExceptT e m)   = TMVar m
  type TBQueue (ExceptT e m) = TBQueue m

  atomically (ExceptT t) = ExceptT $ atomically t
  newTVar                = lift . newTVar
  readTVar               = lift . readTVar
  writeTVar t a          = lift $ writeTVar t a
  retry                  = lift retry

  newTMVar               = lift . newTMVar
  newTMVarIO             = lift . newTMVarIO
  newEmptyTMVar          = lift newEmptyTMVar
  newEmptyTMVarIO        = lift newEmptyTMVarIO
  takeTMVar              = lift . takeTMVar
  tryTakeTMVar           = lift . tryTakeTMVar
  putTMVar   t a         = lift $ putTMVar t a
  tryPutTMVar t a        = lift $ tryPutTMVar t a
  readTMVar              = lift . readTMVar
  tryReadTMVar           = lift . tryReadTMVar
  swapTMVar t a          = lift $ swapTMVar t a
  isEmptyTMVar           = lift . isEmptyTMVar

  newTBQueue       = lift . newTBQueue
  readTBQueue      = lift . readTBQueue
  tryReadTBQueue   = lift . tryReadTBQueue
  writeTBQueue q a = lift $ writeTBQueue q a

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
-- Instance for IO uses the existing STM library implementations
--

instance MonadSTM IO where
  type Tr   IO = STM.STM
  type TVar IO = STM.TVar

  atomically  = wrapBlockedIndefinitely . STM.atomically
  newTVar     = STM.newTVar
  readTVar    = STM.readTVar
  writeTVar   = STM.writeTVar
  retry       = STM.retry

  newTVarIO   = STM.newTVarIO
  modifyTVar  = STM.modifyTVar
  modifyTVar' = STM.modifyTVar'
  check       = STM.check

  type TMVar IO = STM.TMVar

  newTMVar        = STM.newTMVar
  newTMVarIO      = STM.newTMVarIO
  newEmptyTMVar   = STM.newEmptyTMVar
  newEmptyTMVarIO = STM.newEmptyTMVarIO
  takeTMVar       = STM.takeTMVar
  tryTakeTMVar    = STM.tryTakeTMVar
  putTMVar        = STM.putTMVar
  tryPutTMVar     = STM.tryPutTMVar
  readTMVar       = STM.readTMVar
  tryReadTMVar    = STM.tryReadTMVar
  swapTMVar       = STM.swapTMVar
  isEmptyTMVar    = STM.isEmptyTMVar

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

--
-- Default TMVar implementation in terms of TVars (used by sim)
--

newtype TMVarDefault m a = TMVar (TVar m (Maybe a))

newTMVarDefault :: MonadSTM m => a -> Tr m (TMVarDefault m a)
newTMVarDefault a = do
  t <- newTVar (Just a)
  return (TMVar t)

newTMVarIODefault :: MonadSTM m => a -> m (TMVarDefault m a)
newTMVarIODefault a = do
  t <- newTVarIO (Just a)
  return (TMVar t)

newEmptyTMVarDefault :: MonadSTM m => Tr m (TMVarDefault m a)
newEmptyTMVarDefault = do
  t <- newTVar Nothing
  return (TMVar t)

newEmptyTMVarIODefault :: MonadSTM m => m (TMVarDefault m a)
newEmptyTMVarIODefault = do
  t <- newTVarIO Nothing
  return (TMVar t)

takeTMVarDefault :: MonadSTM m => TMVarDefault m a -> Tr m a
takeTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> do writeTVar t Nothing; return a

tryTakeTMVarDefault :: MonadSTM m => TMVarDefault m a -> Tr m (Maybe a)
tryTakeTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return Nothing
    Just a  -> do writeTVar t Nothing; return (Just a)

putTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> Tr m ()
putTMVarDefault (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return ()
    Just _  -> retry

tryPutTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> Tr m Bool
tryPutTMVarDefault (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> do writeTVar t (Just a); return True
    Just _  -> return False

readTMVarDefault :: MonadSTM m => TMVarDefault m a -> Tr m a
readTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> retry
    Just a  -> return a

tryReadTMVarDefault :: MonadSTM m => TMVarDefault m a -> Tr m (Maybe a)
tryReadTMVarDefault (TMVar t) = readTVar t

swapTMVarDefault :: MonadSTM m => TMVarDefault m a -> a -> Tr m a
swapTMVarDefault (TMVar t) new = do
  m <- readTVar t
  case m of
    Nothing  -> retry
    Just old -> do writeTVar t (Just new); return old

isEmptyTMVarDefault :: MonadSTM m => TMVarDefault m a -> Tr m Bool
isEmptyTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return True
    Just _  -> return False

data TBQueueDefault m a = TBQueue
  !(TVar m Natural) -- read capacity
  !(TVar m [a])     -- elements waiting for read
  !(TVar m Natural) -- write capacity
  !(TVar m [a])     -- written elements
  !Natural

newTBQueueDefault :: MonadSTM m => Natural -> Tr m (TBQueueDefault m a)
newTBQueueDefault size = do
  rsize <- newTVar 0
  read  <- newTVar []
  wsize <- newTVar size
  write <- newTVar []
  return (TBQueue rsize read wsize write size)

readTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> Tr m a
readTBQueueDefault queue = maybe retry return =<< tryReadTBQueueDefault queue

tryReadTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> Tr m (Maybe a)
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

writeTBQueueDefault :: MonadSTM m => TBQueueDefault m a -> a -> Tr m ()
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
