-- | References with a counter-based lifecycle, similar to reference-counted
-- shared pointers.
-- This is useful to implement shared access to scarce resources that require
-- timely destruction (i.e., we cannot rely on GC to run finalizers).
module Cardano.KESAgent.Util.RefCounting (
  CRef,
  CRefCount,
  CRefEvent (..),
  CRefEventType (..),
  CRefID,
  ReferenceCountUnderflow (..),
  acquireCRef,
  getCRefCount,
  newCRef,
  newCRefWith,
  readCRef,
  releaseCRef,
  withCRef,
  withCRefValue,
  withNewCRef,
  withNewCRefValue,
  withNewCRefValueWith,
  withNewCRefWith,
) where

import Cardano.KESAgent.Util.Pretty (Pretty (..))

import Control.Concurrent.Class.MonadMVar
import Control.Concurrent.Class.MonadSTM
import Control.Monad (when)
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Tracer
import Data.Word
import System.IO.Unsafe (unsafePerformIO)

-- | Reference count. We use a signed integer so that we can catch cases where
-- a reference is released beyond zero (which shouldn't happen, but we want to
-- be able to detect it when it does).
type CRefCount = Int

-- | Unique identifier for a 'CRef'. Mainly used for debugging.
type CRefID = Word32

{-# NOINLINE nextCRefIDVar #-}

-- | Hacky global variable, used to generate unique 'CRefID's.
nextCRefIDVar :: MVar IO CRefID
nextCRefIDVar = unsafePerformIO $ newMVar 0

-- | A reference-counted value.
data CRef m a
  = CRef
  { cDeref :: !a
  -- ^ The wrapped resource
  , cFinalize :: a -> m ()
  -- ^ Finalizer; this should deallocate the wrapped resource. It will run
  -- when the reference count reaches zero.
  , cCount :: !(TMVar m CRefCount)
  , -- Reference count.
    cID :: !CRefID
  , -- ID, used for debugging etc.
    cTracer :: Tracer m CRefEvent
    -- Tracer used for debugging 'CRef' lifecycles.
  }

-- | 'CRef' lifecycle event types.
data CRefEventType
  = CRefCreate
  | CRefAcquire
  | CRefRelease
  deriving (Show, Eq)

-- | 'CRef' lifecycle events.
data CRefEvent
  = CRefEvent
  { creType :: !CRefEventType
  -- ^ what happened
  , creID :: !CRefID
  -- ^ which CRef was affected
  , creCountBefore :: !CRefCount
  -- ^ refcount before the event
  , creCountAfter :: !CRefCount
  -- ^ refcount after the event
  }
  deriving (Show, Eq)

instance Pretty CRefEvent where
  pretty e =
    drop (length "CRef") (show $ creType e)
      ++ " #"
      ++ show (creID e)
      ++ " "
      ++ show (creCountBefore e)
      ++ " -> "
      ++ show (creCountAfter e)

traceCRef :: MonadSTM m => CRefEventType -> CRef m a -> CRefCount -> CRefCount -> m ()
traceCRef event cref before after = do
  traceWith (cTracer cref) (CRefEvent event (cID cref) before after)

-- | Reference underflow exception: this will be thrown when a reference count
-- is released beyond zero (i.e., becomes negative). This is generally a bug,
-- caused by user code releasing a reference that it hasn't acquired.
data ReferenceCountUnderflow
  = ReferenceCountUnderflow
  deriving (Show, Eq)

instance Exception ReferenceCountUnderflow

{- HLINT ignore "Deprecated: Use throwIO" -}

-- | Gets the current reference count for the 'CRef'.
getCRefCount :: MonadSTM m => CRef m a -> m Int
getCRefCount = atomically . readTMVar . cCount

-- | Increment the 'CRef' counter such that it will not be finalized until we
-- 'releaseCRef' it again.
acquireCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> m a
acquireCRef cref = do
  (count, count', val) <- atomically $ do
    count <- takeTMVar (cCount cref)
    let count' = succ count
    putTMVar (cCount cref) count'
    return (count, count', cDeref cref)
  traceCRef CRefAcquire cref count count'
  when (count <= 0) (throwIO ReferenceCountUnderflow)
  return val

-- | Release a 'CRef' by decrementing its counter. When the counter reaches
-- zero, the resource is finalized. Releasing a 'CRef' more often than it has
-- been acquired is an error, and will (eventually) throwIO
-- 'ReferenceCountUnderflow' exceptions.
releaseCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> m ()
releaseCRef cref = do
  (count, count') <- atomically $ do
    count <- takeTMVar (cCount cref)
    let count' = pred count
    putTMVar (cCount cref) count'
    return (count, count')
  when (count' == 0) (cFinalize cref (cDeref cref))
  traceCRef CRefRelease cref count count'
  when (count' < 0) (throwIO ReferenceCountUnderflow)

-- | Read a 'CRef' without acquiring it. The caller is responsible for making
-- sure the 'CRef' has been acquired appropriately for the duration of the
-- call, and while using the wrapped resource.
-- It is generally preferable to use 'withCRefValue' instead.
readCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> m a
readCRef cref =
  bracket
    (atomically $ readTMVar (cCount cref))
    (const $ return ())
    ( \count -> do
        when (count <= 0) (throwIO ReferenceCountUnderflow)
        return (cDeref cref)
    )

-- | Create a fresh 'CRef'. Its counter will be initialized to 1; the caller is
-- responsible for calling 'releaseCRef' to release it.
-- @newCRef f i@ creates a 'CRef' with an initial value of @i@, and a finalizer
-- @f@.
newCRef :: (MonadST m, MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> m (CRef m a)
newCRef = newCRefWith nullTracer

-- | Generate a unique 'CRefID' from the global supply.
genCRefID :: MonadST m => m CRefID
genCRefID = stToIO . unsafeIOToST $ do
  n <- takeMVar nextCRefIDVar
  putMVar nextCRefIDVar (succ n)
  return n

-- | Create a fresh 'CRef' attaching a custom tracer (see 'newCRef').
newCRefWith ::
  (MonadST m, MonadSTM m, MonadThrow m) =>
  Tracer m CRefEvent ->
  (a -> m ()) ->
  a ->
  m (CRef m a)
newCRefWith tracer finalizer val = do
  counter <- newTMVarIO 1
  cid <- genCRefID
  let cref =
        CRef
          { cDeref = val
          , cFinalize = finalizer
          , cCount = counter
          , cID = cid
          , cTracer = tracer
          }
  traceCRef CRefCreate cref 0 1
  return cref

-- | Operate on a 'CRef'. The 'CRef' is guaranteed to not finalize for the
-- duration of the wrapped function call.
withCRef :: (MonadSTM m, MonadThrow m) => CRef m a -> (CRef m a -> m b) -> m b
withCRef cref action =
  bracket_
    (acquireCRef cref)
    (releaseCRef cref)
    (action cref)

-- | Create a fresh 'CRef' and operate on it immediately. See 'newCRef'.
withNewCRef :: (MonadST m, MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> (CRef m a -> m b) -> m b
withNewCRef = withNewCRefWith nullTracer

-- | Create a fresh 'CRef' with a tracer and operate on it immediately. See 'newCRefWith'.
withNewCRefWith ::
  (MonadST m, MonadSTM m, MonadThrow m) =>
  Tracer m CRefEvent ->
  (a -> m ()) ->
  a ->
  (CRef m a -> m b) ->
  m b
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

-- | Create a fresh 'CRef' and operate on its value immediately. See 'newCRef'.
withNewCRefValue :: (MonadST m, MonadSTM m, MonadThrow m) => (a -> m ()) -> a -> (a -> m b) -> m b
withNewCRefValue = withNewCRefValueWith nullTracer

-- | Create a fresh 'CRef' with a tracer and operate on its value immediately.
-- See 'newCRefWith'.
withNewCRefValueWith ::
  (MonadST m, MonadSTM m, MonadThrow m) => Tracer m CRefEvent -> (a -> m ()) -> a -> (a -> m b) -> m b
withNewCRefValueWith tracer finalizer val action = do
  bracket
    (newCRefWith tracer finalizer val)
    releaseCRef
    (action . cDeref)
