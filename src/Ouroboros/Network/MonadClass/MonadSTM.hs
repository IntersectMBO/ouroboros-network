{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DefaultSignatures      #-}
module Ouroboros.Network.MonadClass.MonadSTM
  ( MonadSTM (..) ) where

import qualified Control.Concurrent.STM.TVar    as STM
import qualified Control.Concurrent.STM.TMVar   as STM
import qualified Control.Monad.STM              as STM

import           Ouroboros.Network.MonadClass.MonadFork

class (MonadFork m, Monad (Tr m)) => MonadSTM m where

  -- STM transactions
  type Tr   m = (n :: * -> *) | n -> m
  -- The STM primitives
  type TVar m :: * -> *

  atomically   :: Tr m a -> m a
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

  -- Default impl
  type instance TMVar m = TMVarDefault m
  default newTMVar        :: a -> Tr m (TMVarDefault m a)
  default newTMVarIO      :: a -> m   (TMVarDefault m a)
  default newEmptyTMVar   ::      Tr m (TMVarDefault m a)
  default newEmptyTMVarIO ::      m   (TMVarDefault m a)
  default takeTMVar       :: TMVarDefault m a      -> Tr m a
  default tryTakeTMVar    :: TMVarDefault m a      -> Tr m (Maybe a)
  default putTMVar        :: TMVarDefault m a -> a -> Tr m ()
  default tryPutTMVar     :: TMVarDefault m a -> a -> Tr m Bool
  default readTMVar       :: TMVarDefault m a      -> Tr m a
  default tryReadTMVar    :: TMVarDefault m a      -> Tr m (Maybe a)
  default swapTMVar       :: TMVarDefault m a -> a -> Tr m a
  default isEmptyTMVar    :: TMVarDefault m a      -> Tr m Bool

  newTMVar        = newTMVarDefault
  newTMVarIO      = newTMVarIODefault
  newEmptyTMVar   = newEmptyTMVarDefault
  newEmptyTMVarIO = newEmptyTMVarIODefault
  takeTMVar       = takeTMVarDefault
  tryTakeTMVar    = tryTakeTMVarDefault
  putTMVar        = putTMVarDefault
  tryPutTMVar     = tryPutTMVarDefault
  readTMVar       = readTMVarDefault
  tryReadTMVar    = tryReadTMVarDefault
  swapTMVar       = swapTMVarDefault
  isEmptyTMVar    = isEmptyTMVarDefault


--
-- Instance for IO uses the existing STM library implementations
--

instance MonadSTM IO where
  type Tr   IO = STM.STM
  type TVar IO = STM.TVar

  atomically  = STM.atomically
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
    Nothing -> retry
    Just old -> do writeTVar t (Just new); return old

isEmptyTMVarDefault :: MonadSTM m => TMVarDefault m a -> Tr m Bool
isEmptyTMVarDefault (TMVar t) = do
  m <- readTVar t
  case m of
    Nothing -> return True
    Just _  -> return False

