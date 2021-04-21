{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
-- 'Eq' instance for 'TMVarDefault', 'TQueueDefault' and 'TBQueueDefault'.
{-# LANGUAGE UndecidableInstances    #-}

module Control.Monad.Class.MonadSTM
  ( MonadSTM (..)
  , MonadSTMTx (..)
  , MonadLabelledSTM (..)
  , MonadLabelledSTMTx (..)
  , LazyTVar
  , LazyTMVar
  , EqTVar
  , eqTVar
  , EqTMVar
  , eqTMVar
  , EqTQueue
  , eqTQueue
  , EqTBQueue
  , eqTBQueue

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
import           Data.Proxy (Proxy (..))
import           GHC.Stack
import           Numeric.Natural (Natural)


{-# DEPRECATED LazyTVar  "Renamed back to 'TVar'" #-}
{-# DEPRECATED LazyTMVar "Renamed back to 'TMVar'" #-}
type LazyTVar  m = TVar m
type LazyTMVar m = TMVar m

-- | The STM primitive operations and associated types.
--
class ( Monad stm
      , Alternative stm
      , MonadPlus stm
      , forall a. Eq (tvar a)
      , forall a. Eq (tmvar a)
      , forall a. Eq (tqueue a)
      , forall a. Eq (tbqueue a)
      ) => MonadSTMTx stm tvar tmvar tqueue tbqueue
                    | stm -> tvar
                    , stm -> tmvar
                    , stm -> tqueue
                    , stm -> tbqueue
                    where
  newTVar      :: a -> stm (tvar a)
  readTVar     :: tvar a -> stm a
  writeTVar    :: tvar a -> a -> stm ()
  retry        :: stm a
  orElse       :: stm a -> stm a -> stm a

  modifyTVar   :: tvar a -> (a -> a) -> stm ()
  modifyTVar  v f = readTVar v >>= writeTVar v . f

  modifyTVar'  :: tvar a -> (a -> a) -> stm ()
  modifyTVar' v f = readTVar v >>= \x -> writeTVar v $! f x

  -- | @since io-sim-classes-0.2.0.0
  stateTVar    :: tvar s -> (s -> (a, s)) -> stm a
  stateTVar    = stateTVarDefault

  check        :: Bool -> stm ()
  check True = return ()
  check _    = retry

  -- Additional derived STM APIs
  newTMVar        :: a -> stm (tmvar a)
  newEmptyTMVar   ::      stm (tmvar a)
  takeTMVar       :: tmvar a      -> stm a
  tryTakeTMVar    :: tmvar a      -> stm (Maybe a)
  putTMVar        :: tmvar a -> a -> stm ()
  tryPutTMVar     :: tmvar a -> a -> stm Bool
  readTMVar       :: tmvar a      -> stm a
  tryReadTMVar    :: tmvar a      -> stm (Maybe a)
  swapTMVar       :: tmvar a -> a -> stm a
  isEmptyTMVar    :: tmvar a      -> stm Bool

  newTQueue      :: stm (tqueue a)
  readTQueue     :: tqueue a -> stm a
  tryReadTQueue  :: tqueue a -> stm (Maybe a)
  peekTQueue     :: tqueue a -> stm a
  tryPeekTQueue  :: tqueue a -> stm (Maybe a)
  writeTQueue    :: tqueue a -> a -> stm ()
  isEmptyTQueue  :: tqueue a -> stm Bool

  newTBQueue     :: Natural -> stm (tbqueue a)
  readTBQueue    :: tbqueue a -> stm a
  tryReadTBQueue :: tbqueue a -> stm (Maybe a)
  peekTBQueue    :: tbqueue a -> stm a
  tryPeekTBQueue :: tbqueue a -> stm (Maybe a)
  flushTBQueue   :: tbqueue a -> stm [a]
  writeTBQueue   :: tbqueue a -> a -> stm ()
  -- | @since 0.2.0.0
  lengthTBQueue  :: tbqueue a -> stm Natural
  isEmptyTBQueue :: tbqueue a -> stm Bool
  isFullTBQueue  :: tbqueue a -> stm Bool


-- class MonadSTMTx' stm (TVar_ stm) => MonadSTMTx stm where
-- instance MonadSTMTx' stm (TVar_ stm) => MonadSTMTx stm

stateTVarDefault :: MonadSTMTx stm tvar tmvar tqueue tbqueue
                 => tvar s -> (s -> (a, s)) -> stm a
stateTVarDefault var f = do
   s <- readTVar var
   let (a, s') = f s
   writeTVar var s'
   return a

-- | 'MonadSTM' provides the @'STM' m@ monad as well as all operations to
-- execute it. 
--
class (Monad m, MonadSTMTx (STM m) (TVar m) (TMVar m) (TQueue m) (TBQueue m))
   => MonadSTM m where
  -- STM transactions
  type STM  m    :: Type -> Type
  type TVar m    :: Type -> Type
  type TMVar m   :: Type -> Type
  type TQueue m  :: Type -> Type
  type TBQueue m :: Type -> Type

  atomically :: HasCallStack => STM m a -> m a

  -- Helpful derived functions with default implementations

  newTVarIO        :: a -> m (TVar  m a)
  newTMVarIO       :: a -> m (TMVar m a)
  newEmptyTMVarIO  ::      m (TMVar m a)
  newTBQueueIO     :: Natural -> m (TBQueue m a)

  newTVarIO       = atomically . newTVar
  newTMVarIO      = atomically . newTMVar
  newEmptyTMVarIO = atomically   newEmptyTMVar
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


-- | Constraint which provides quantified constraint: @forall x. Eq (TVar m x)@.
-- (see <https://gitlab.haskell.org/ghc/ghc/-/issues/14860>)
--
-- Call sites require 'ImpredicativeTypes' @GHC@ extension.
--
type EqTVar m tvar
    = ( tvar ~ TVar m
      , forall a. Eq (tvar a)
      )

eqTVar' :: forall stm tvar tmvar tqueue tbqueue a.
           MonadSTMTx stm tvar tmvar tqueue tbqueue
        => Proxy stm
        -> tvar a -> tvar a -> Bool
eqTVar' _ = (==)
{-# INLINE eqTVar' #-}


-- | Polymorphic equality of 'TVar's.
--
eqTVar :: forall m a.
          MonadSTM m
       => Proxy m
       -> TVar m a -> TVar m a -> Bool
eqTVar p = eqTVar' (f p)
  where
    f :: Proxy m -> Proxy (STM m)
    f Proxy = Proxy


-- | Constraint which provides quantified constraint: @forall x. Eq (TMVar m x)@.
-- (see <https://gitlab.haskell.org/ghc/ghc/-/issues/14860>)
--
-- Call sites require 'ImpredicativeTypes' @GHC@ extension.
--
type EqTMVar m tmvar
    = ( tmvar ~ TMVar m
      , forall a. Eq (tmvar a)
      )

eqTMVar' :: forall stm tvar tmvar tqueue tbqueue a.
            MonadSTMTx stm tvar tmvar tqueue tbqueue
         => Proxy stm
         -> tmvar a -> tmvar a -> Bool
eqTMVar' _ = (==)
{-# INLINE eqTMVar' #-}


-- | Polymorphic equality of 'TMVar's.
--
eqTMVar :: forall m a.
           MonadSTM m
        => Proxy m
        -> TMVar m a -> TMVar m a -> Bool
eqTMVar p = eqTMVar' (f p)
  where
    f :: Proxy m -> Proxy (STM m)
    f Proxy = Proxy


-- | Constraint which provides quantified constraint: @forall x. Eq (TVar m x)@.
-- (see <https://gitlab.haskell.org/ghc/ghc/-/issues/14860>)
--
-- Call sites require 'ImpredicativeTypes' @GHC@ extension.
--
type EqTQueue m tqueue
    = ( tqueue ~ TQueue m
      , forall a. Eq (tqueue a)
      )

eqTQueue' :: forall stm tvar tmvar tqueue tbqueue a.
             MonadSTMTx stm tvar tmvar tqueue tbqueue
          => Proxy stm
          -> tqueue a -> tqueue a -> Bool
eqTQueue' _ = (==)
{-# INLINE eqTQueue' #-}


-- | Polymorphic equality of 'TQueue's.
--
eqTQueue :: forall m a.
            MonadSTM m
         => Proxy m
         -> TQueue m a -> TQueue m a -> Bool
eqTQueue p = eqTQueue' (f p)
  where
    f :: Proxy m -> Proxy (STM m)
    f Proxy = Proxy


-- | Constraint which provides quantified constraint: @forall x. Eq (TVar m x)@.
-- (see <https://gitlab.haskell.org/ghc/ghc/-/issues/14860>)
--
-- Call sites require 'ImpredicativeTypes' @GHC@ extension.
--
type EqTBQueue m tbqueue
    = ( tbqueue ~ TBQueue m
      , forall a. Eq (tbqueue a)
      )

eqTBQueue' :: forall stm tvar tmvar tqueue tbqueue a.
              MonadSTMTx stm tvar tmvar tqueue tbqueue
           => Proxy stm
           -> tbqueue a -> tbqueue a -> Bool
eqTBQueue' _ = (==)
{-# INLINE eqTBQueue' #-}


-- | Polymorphic equality of 'TBQueue's.
--
eqTBQueue :: forall m a.
             MonadSTM m
          => Proxy m
          -> TBQueue m a -> TBQueue m a -> Bool
eqTBQueue p = eqTBQueue' (f p)
  where
    f :: Proxy m -> Proxy (STM m)
    f Proxy = Proxy

-- | Labelled 'TVar's, 'TMVar's, 'TQueue's and 'TBQueue's.
--
class MonadSTMTx stm tvar tmvar tqueue tbqueue
   => MonadLabelledSTMTx stm tvar tmvar tqueue tbqueue where
  labelTVar    :: tvar    a -> String -> stm ()
  labelTMVar   :: tmvar   a -> String -> stm ()
  labelTQueue  :: tqueue  a -> String -> stm ()
  labelTBQueue :: tbqueue a -> String -> stm ()

-- | A convenience class which provides 'MonadSTM' and 'MonadLabelledSTMTx'
-- constraints.
--
class ( MonadSTM m
      , MonadLabelledSTMTx (STM m) (TVar m) (TMVar m) (TQueue m) (TBQueue m)
      )
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

instance MonadSTMTx STM.STM STM.TVar STM.TMVar STM.TQueue STM.TBQueue where
  newTVar        = STM.newTVar
  readTVar       = STM.readTVar
  writeTVar      = STM.writeTVar
  retry          = STM.retry
  orElse         = STM.orElse
  modifyTVar     = STM.modifyTVar
  modifyTVar'    = STM.modifyTVar'
  stateTVar      = STM.stateTVar
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
  type STM     IO = STM.STM
  type TVar    IO = STM.TVar
  type TMVar   IO = STM.TMVar
  type TQueue  IO = STM.TQueue
  type TBQueue IO = STM.TBQueue

  atomically = wrapBlockedIndefinitely . STM.atomically

  newTVarIO       = STM.newTVarIO
  newTMVarIO      = STM.newTMVarIO
  newEmptyTMVarIO = STM.newEmptyTMVarIO

-- | noop instance
--
instance MonadLabelledSTMTx STM.STM
                            STM.TVar STM.TMVar
                            STM.TQueue STM.TBQueue where
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
  type STM     (ReaderT r m) = STM m
  type TVar    (ReaderT r m) = TVar m
  type TMVar   (ReaderT r m) = TMVar m
  type TQueue  (ReaderT r m) = TQueue m
  type TBQueue (ReaderT r m) = TBQueue m
  atomically      = lift . atomically
  newTVarIO       = lift . newTVarM
  newTMVarIO      = lift . newTMVarM
  newEmptyTMVarIO = lift   newEmptyTMVarM

--
-- Default TMVar implementation in terms of TVars (used by sim)
--

newtype TMVarDefault m a = TMVar (TVar m (Maybe a))

deriving instance Eq (TVar m (Maybe a)) => Eq (TMVarDefault m a)


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

deriving instance Eq (TVar m [a]) => Eq (TQueueDefault m a)

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

tryReadTQueueDefault :: MonadSTMTx (STM m)
                                   (TVar m) (TMVar m)
                                   (TQueue m) (TBQueue m)
                     => TQueueDefault m a -> STM m (Maybe a)
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

isEmptyTQueueDefault :: MonadSTMTx (STM m)
                                   (TVar m) (TMVar m)
                                   (TQueue m) (TBQueue m)
                     => TQueueDefault m a -> STM m Bool
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
  !(TVar m Natural) -- read capacity
  !(TVar m [a])     -- elements waiting for read
  !(TVar m Natural) -- write capacity
  !(TVar m [a])     -- written elements
  !Natural

deriving instance ( Eq (TVar m [a])
                  , Eq (TVar m Natural)
                  ) => Eq (TBQueueDefault m a)

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

tryReadTBQueueDefault :: MonadSTMTx (STM m)
                                    (TVar m) (TMVar m)
                                    (TQueue m) (TBQueue m)
                      => TBQueueDefault m a -> STM m (Maybe a)
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
throwSTM :: ( MonadSTMTx stm tvar tmvar tqueue tbqueue
            , MonadThrow.MonadThrow stm, Exception e
            )
         => e -> stm a
throwSTM = MonadThrow.throwIO


-- | 'catch' speclialized for an @stm@ monad.
--
catchSTM :: ( MonadSTMTx stm tvar tmvar tqueue tbqueue
            , MonadThrow.MonadCatch stm, Exception e
            )
         => stm a -> (e -> stm a) -> stm a
catchSTM = MonadThrow.catch
