{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Control.Monad.Class.MonadSTM
  ( MonadSTM (..)
  , MonadLabelledSTM (..)
  , LazyTVar
  , LazyTMVar

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
import           Control.Monad (MonadPlus (..))

import qualified Control.Monad.Class.MonadThrow as MonadThrow

import           Control.Applicative (Alternative (..))
import           Control.Exception
import           Data.Kind (Type)
import           GHC.Stack
import           Numeric.Natural (Natural)


{-# DEPRECATED LazyTVar  "Renamed back to 'TVar'" #-}
{-# DEPRECATED LazyTMVar "Renamed back to 'TMVar'" #-}
type LazyTVar  m = TVar m
type LazyTMVar m = TMVar m

-- The STM primitives
class ( Monad m
      , Alternative (STM m)
      , MonadPlus   (STM m)
      ) => MonadSTM m where
  -- STM transactions
  type STM  m = (stm :: Type -> Type)  | stm -> m
  atomically :: HasCallStack => STM m a -> m a

  type TVar m  :: Type -> Type

  newTVar      :: a -> STM m (TVar m a)
  readTVar     :: TVar m a -> STM m a
  writeTVar    :: TVar m a -> a -> STM m ()
  retry        :: STM m a
  orElse       :: STM m a -> STM m a -> STM m a

  modifyTVar   :: TVar m a -> (a -> a) -> STM m ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f

  modifyTVar'  :: TVar m a -> (a -> a) -> STM m ()
  modifyTVar' v f = readTVar v >>= \x -> writeTVar v $! f x

  -- | @since io-classes-0.2.0.0
  stateTVar    :: TVar m s -> (s -> (a, s)) -> STM m a
  stateTVar    = stateTVarDefault

  swapTVar     :: TVar m a -> a -> STM m a
  swapTVar     = swapTVarDefault

  check        :: Bool -> STM m ()
  check True = return ()
  check _    = retry

  -- Additional derived STM APIs
  type TMVar m    :: Type -> Type
  newTMVar        :: a -> STM m (TMVar m a)
  newEmptyTMVar   ::      STM m (TMVar m a)
  takeTMVar       :: TMVar m a      -> STM m a
  tryTakeTMVar    :: TMVar m a      -> STM m (Maybe a)
  putTMVar        :: TMVar m a -> a -> STM m ()
  tryPutTMVar     :: TMVar m a -> a -> STM m Bool
  readTMVar       :: TMVar m a      -> STM m a
  tryReadTMVar    :: TMVar m a      -> STM m (Maybe a)
  swapTMVar       :: TMVar m a -> a -> STM m a
  isEmptyTMVar    :: TMVar m a      -> STM m Bool

  type TQueue m  :: Type -> Type
  newTQueue      :: STM m (TQueue m a)
  readTQueue     :: TQueue m a -> STM m a
  tryReadTQueue  :: TQueue m a -> STM m (Maybe a)
  peekTQueue     :: TQueue m a -> STM m a
  tryPeekTQueue  :: TQueue m a -> STM m (Maybe a)
  writeTQueue    :: TQueue m a -> a -> STM m ()
  isEmptyTQueue  :: TQueue m a -> STM m Bool

  type TBQueue m ::  Type -> Type
  newTBQueue     :: Natural -> STM m (TBQueue m a)
  readTBQueue    :: TBQueue m a -> STM m a
  tryReadTBQueue :: TBQueue m a -> STM m (Maybe a)
  peekTBQueue    :: TBQueue m a -> STM m a
  tryPeekTBQueue :: TBQueue m a -> STM m (Maybe a)
  flushTBQueue   :: TBQueue m a -> STM m [a]
  writeTBQueue   :: TBQueue m a -> a -> STM m ()
  -- | @since 0.2.0.0
  lengthTBQueue  :: TBQueue m a -> STM m Natural
  isEmptyTBQueue :: TBQueue m a -> STM m Bool
  isFullTBQueue  :: TBQueue m a -> STM m Bool

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


stateTVarDefault :: MonadSTM m => TVar m s -> (s -> (a, s)) -> STM m a
stateTVarDefault var f = do
   s <- readTVar var
   let (a, s') = f s
   writeTVar var s'
   return a

swapTVarDefault :: MonadSTM m => TVar m a -> a -> STM m a
swapTVarDefault var new = do
    old <- readTVar var
    writeTVar var new
    return old


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
class MonadSTM m
   => MonadLabelledSTM m where
  labelTVar    :: TVar    m a -> String -> STM m ()
  labelTMVar   :: TMVar   m a -> String -> STM m ()
  labelTQueue  :: TQueue  m a -> String -> STM m ()
  labelTBQueue :: TBQueue m a -> String -> STM m ()

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

instance MonadSTM IO where
  type STM IO = STM.STM

  atomically = wrapBlockedIndefinitely . STM.atomically

  type TVar    IO = STM.TVar
  type TMVar   IO = STM.TMVar
  type TQueue  IO = STM.TQueue
  type TBQueue IO = STM.TBQueue

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

  newTVarIO       = STM.newTVarIO
  readTVarIO      = STM.readTVarIO
  newTMVarIO      = STM.newTMVarIO
  newEmptyTMVarIO = STM.newEmptyTMVarIO
  newTQueueIO     = STM.newTQueueIO
  newTBQueueIO    = STM.newTBQueueIO

-- | noop instance
--
instance MonadLabelledSTM IO where
  labelTVar    = \_  _ -> return ()
  labelTMVar   = \_  _ -> return ()
  labelTQueue  = \_  _ -> return ()
  labelTBQueue = \_  _ -> return ()

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

tryReadTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m (Maybe a)
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

isEmptyTQueueDefault :: MonadSTM m => TQueueDefault m a -> STM m Bool
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
throwSTM :: (MonadSTM m, MonadThrow.MonadThrow (STM m), Exception e)
         => e -> STM m a
throwSTM = MonadThrow.throwIO


-- | 'catch' speclialized for an @stm@ monad.
--
catchSTM :: (MonadSTM m, MonadThrow.MonadCatch (STM m), Exception e)
         => STM m a -> (e -> STM m a) -> STM m a
catchSTM = MonadThrow.catch
