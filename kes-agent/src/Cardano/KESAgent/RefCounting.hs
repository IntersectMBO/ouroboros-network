-- | References with a counter-based lifecycle, similar to reference-counted
-- shared pointers.
-- This is useful to implement shared access to scarce resources that require
-- timely destruction (i.e., we cannot rely on GC to run finalizers).
module Cardano.KESAgent.RefCounting
( CRef
, ReferenceCountUnderflow (..)
, acquireCRef
, releaseCRef
, newCRef
, readCRef
, withCRef
, withCRefValue
, withNewCRef
, withNewCRefValue
, getCRefCount
)
where

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad (when)

data CRef m a =
  CRef
    { cDeref :: !a
    , cFinalize :: a -> m ()
    , cCount :: !(TMVar m Int)
    }

data ReferenceCountUnderflow =
  ReferenceCountUnderflow
  deriving (Show, Eq)

instance Exception ReferenceCountUnderflow where

{- HLINT ignore "Deprecated: Use throwIO" -}
-- | Gets the current reference count for the 'CRef'.
getCRefCount :: (MonadSTM m) => CRef m a -> m Int
getCRefCount = atomically . readTMVar . cCount

-- | Increment the 'CRef' counter such that it will not be finalized until we
-- 'releaseCRef' it again.
acquireCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> m a
acquireCRef cref = do
  (count, val) <- atomically $ do
    count <- takeTMVar (cCount cref)
    let count' = succ count
    putTMVar (cCount cref) count'
    return (count, cDeref cref)
  when (count <= 0) (throwIO ReferenceCountUnderflow)
  return val

-- | Release a 'CRef' by decrementing its counter. When the counter reaches
-- zero, the resource is finalized. Releasing a 'CRef' more often than it has
-- been acquired is an error, and will (eventually) throwIO
-- 'ReferenceCountUnderflow' exceptions.
releaseCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> m ()
releaseCRef cref = do
  count' <- atomically $ do
    count <- takeTMVar (cCount cref)
    let count' = pred count
    putTMVar (cCount cref) count'
    return count'
  when (count' < 0) (throwIO ReferenceCountUnderflow)
  when (count' == 0) (cFinalize cref (cDeref cref))

-- | Read a 'CRef' without acquiring it. The caller is responsible for making
-- sure the 'CRef' has been acquired appropriately for the duration of the
-- call, and while using the wrapped resource.
-- It is generally preferable to use 'withCRefValue' instead.
readCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> m a
readCRef cref = bracket
  (atomically $ readTMVar (cCount cref))
  (const $ return ())
  (\count -> do
    when (count <= 0) (throwIO ReferenceCountUnderflow)
    return (cDeref cref)
  )

-- | Create a fresh 'CRef'. Its counter will be initialized to 1; the caller is
-- responsible for calling 'releaseCRef' to release it.
newCRef :: (MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> m (CRef m a)
newCRef finalizer val = do
  counter <- newTMVarIO 1
  return CRef
    { cDeref = val
    , cFinalize = finalizer
    , cCount = counter
    }

-- | Operate on a 'CRef'. The 'CRef' is guaranteed to not finalize for the
-- duration of the wrapped function call.
withCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> (CRef m a -> m b) -> m b
withCRef cref action =
  bracket_
    (acquireCRef cref)
    (releaseCRef cref)
    (action cref)

withNewCRef :: (MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> (CRef m a -> m b) -> m b
withNewCRef finalizer val action = do
  bracket
    (newCRef finalizer val)
    releaseCRef
    action

-- | Operate on a 'CRef'. The 'CRef' is guaranteed to not finalize for the
-- duration of the wrapped function call.
withCRefValue :: (MonadSTM m, MonadThrow m) => CRef m a -> (a -> m b) -> m b
withCRefValue cref action =
  bracket_
    (acquireCRef cref)
    (releaseCRef cref)
    (action $ cDeref cref)

withNewCRefValue :: (MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> (a -> m b) -> m b
withNewCRefValue finalizer val action = do
  bracket
    (newCRef finalizer val)
    releaseCRef
    (action . cDeref)
