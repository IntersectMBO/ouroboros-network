-- | References with a counter-based lifecycle, similar to reference-counted
-- shared pointers.
-- This is useful to implement shared access to scarce resources that require
-- timely destruction (i.e., we cannot rely on GC to run finalizers).
module Cardano.KESAgent.RefCounting
  ( CRef
  , CRefCount
  , CRefEvent (..)
  , CRefEventType (..)
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

import Cardano.KESAgent.Pretty ( Pretty (..) )

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

data CRefEventType
  = CRefCreate
  | CRefAcquire
  | CRefRelease
  deriving (Show, Eq)

data CRefEvent
  = CRefEvent
      { creType :: !CRefEventType
      , creID :: !CRefID
      , creCountBefore :: !CRefCount
      , creCountAfter :: !CRefCount
      }
      deriving (Show, Eq)

instance Pretty CRefEvent where
  pretty e =
    drop (length "CRef") (show $ creType e) ++
    " #" ++ show (creID e) ++ " " ++
    show (creCountBefore e) ++ " -> " ++
    show (creCountAfter e)

traceCRef :: MonadSTM m => CRefEventType -> CRef m a -> CRefCount -> CRefCount -> m ()
traceCRef event cref before after = do
  traceWith (cTracer cref) (CRefEvent event (cID cref) before after)

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
