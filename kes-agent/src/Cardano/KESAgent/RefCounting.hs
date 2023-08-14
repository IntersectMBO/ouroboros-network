-- | References with a counter-based lifecycle, similar to reference-counted
-- shared pointers.
-- This is useful to implement shared access to scarce resources that require
-- timely destruction (i.e., we cannot rely on GC to run finalizers).
module Cardano.KESAgent.RefCounting
  ( CRef
  , CRefCount
  , CRefEvent (..)
  , CRefID
  , ReferenceCountUnderflow (..)
  , acquireCRef
  , getCRefCount
  , newCRef
  , newCRefWith
  , readCRef
  , releaseCRef
  , withCRef
  , withCRefValue
  , withNewCRef
  , withNewCRefValue
  , withNewCRefValueWith
  , withNewCRefWith
  ) where

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM
import Control.Monad ( when )
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.ST.Unsafe ( unsafeIOToST )
import Control.Tracer
import Data.Word
import System.IO.Unsafe ( unsafePerformIO )

type CRefCount = Int

type CRefID = Word32

{-# NOINLINE nextCRefIDVar #-}
nextCRefIDVar :: MVar IO CRefID
nextCRefIDVar = unsafePerformIO $ newMVar 0

data CRef m a =
  CRef
    { cDeref :: !a
    , cFinalize :: a -> m ()
    , cCount :: !(TMVar m CRefCount)
    , cID :: !CRefID
    , cTracer :: Tracer m CRefEvent
    }

data CRefEvent
  = CRefCreate !CRefID !CRefCount
  | CRefAcquire !CRefID !CRefCount
  | CRefRelease !CRefID !CRefCount
  deriving (Show, Eq)

traceCRef :: MonadSTM m => (CRefID -> CRefCount -> CRefEvent) -> CRef m a -> CRefCount -> m ()
traceCRef event cref count = do
  traceWith (cTracer cref) (event (cID cref) count)

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
  traceCRef CRefAcquire cref count
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
  when (count' == 0) (cFinalize cref (cDeref cref))
  traceCRef CRefRelease cref count'
  when (count' < 0) (throwIO ReferenceCountUnderflow)

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
newCRef :: (MonadST m, MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> m (CRef m a)
newCRef = newCRefWith nullTracer

genCRefID :: (MonadST m) => m CRefID
genCRefID = withLiftST $ \liftST ->
  liftST . unsafeIOToST $ do
    n <- takeMVar nextCRefIDVar
    putMVar nextCRefIDVar (succ n)
    return n

newCRefWith :: (MonadST m, MonadSTM m, MonadThrow m)
            => Tracer m CRefEvent
            -> (a -> m ())
            -> a
            -> m (CRef m a)
newCRefWith tracer finalizer val = do
  counter <- newTMVarIO 1
  cid <- genCRefID
  let cref = CRef
        { cDeref = val
        , cFinalize = finalizer
        , cCount = counter
        , cID = cid
        , cTracer = tracer
        }
  traceCRef CRefCreate cref 1
  return cref

-- | Operate on a 'CRef'. The 'CRef' is guaranteed to not finalize for the
-- duration of the wrapped function call.
withCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> (CRef m a -> m b) -> m b
withCRef cref action =
  bracket_
    (acquireCRef cref)
    (releaseCRef cref)
    (action cref)

withNewCRef :: (MonadST m, MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> (CRef m a -> m b) -> m b
withNewCRef = withNewCRefWith nullTracer

withNewCRefWith :: (MonadST m, MonadSTM m, MonadThrow m) => Tracer m CRefEvent -> (a -> m ()) -> a -> (CRef m a -> m b) -> m b
withNewCRefWith tracer finalizer val action = do
  bracket
    (newCRefWith tracer finalizer val)
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

withNewCRefValue :: (MonadST m, MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> (a -> m b) -> m b
withNewCRefValue = withNewCRefValueWith nullTracer

withNewCRefValueWith :: (MonadST m, MonadSTM m, MonadThrow m) => Tracer m CRefEvent -> (a -> m ()) -> a -> (a -> m b) -> m b
withNewCRefValueWith tracer finalizer val action = do
  bracket
    (newCRefWith tracer finalizer val)
    releaseCRef
    (action . cDeref)
