{-#LANGUAGE LambdaCase #-}

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

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when)

data CRef a =
  CRef
    { cDeref :: !a
    , cFinalize :: a -> IO ()
    , cCount :: !(MVar Int)
    }

data ReferenceCountUnderflow =
  ReferenceCountUnderflow
  deriving (Show, Eq)

instance Exception ReferenceCountUnderflow where

-- | Gets the current reference count for the 'CRef'.
getCRefCount :: CRef a -> IO Int
getCRefCount = readMVar . cCount

-- | Increment the 'CRef' counter such that it will not be finalized until we
-- 'releaseCRef' it again.
acquireCRef :: CRef a -> IO a
acquireCRef cref = do
  count <- takeMVar (cCount cref)
  when (count <= 0) (throw ReferenceCountUnderflow)
  putMVar (cCount cref) (succ count)
  return (cDeref cref)

-- | Release a 'CRef' by decrementing its counter. When the counter reaches
-- zero, the resource is finalized. Releasing a 'CRef' more often than it has
-- been acquired is an error, and will (eventually) throw
-- 'ReferenceCountUnderflow' exceptions.
releaseCRef :: CRef a -> IO ()
releaseCRef cref = do
  count <- pred <$> takeMVar (cCount cref)
  when (count < 0) (throw ReferenceCountUnderflow)
  when (count == 0) (cFinalize cref (cDeref cref))
  putMVar (cCount cref) count

-- | Read a 'CRef' without acquiring it. The caller is responsible for making
-- sure the 'CRef' has been acquired appropriately for the duration of the
-- call, and while using the wrapped resource.
-- It is generally preferable to use 'withCRefValue' instead.
readCRef :: CRef a -> IO a
readCRef cref = bracket
  (readMVar (cCount cref))
  (const $ return ())
  (\count -> do
    when (count <= 0) (throw ReferenceCountUnderflow)
    return (cDeref cref)
  )

-- | Create a fresh 'CRef'. Its counter will be initialized to 1; the caller is
-- responsible for calling 'releaseCRef' to release it.
newCRef :: (a -> IO ()) -> a -> IO (CRef a)
newCRef finalizer val = do
  counter <- newMVar 1
  return CRef
    { cDeref = val
    , cFinalize = finalizer
    , cCount = counter
    }

-- | Operate on a 'CRef'. The 'CRef' is guaranteed to not finalize for the
-- duration of the wrapped function call.
withCRef :: CRef a -> (CRef a -> IO b) -> IO b
withCRef cref action =
  bracket_
    (acquireCRef cref)
    (releaseCRef cref)
    (action cref)

withNewCRef :: (a -> IO ()) -> a -> (CRef a -> IO b) -> IO b
withNewCRef finalizer val action = do
  bracket
    (newCRef finalizer val)
    releaseCRef
    action

-- | Operate on a 'CRef'. The 'CRef' is guaranteed to not finalize for the
-- duration of the wrapped function call.
withCRefValue :: CRef a -> (a -> IO b) -> IO b
withCRefValue cref action =
  bracket_
    (acquireCRef cref)
    (releaseCRef cref)
    (action $ cDeref cref)

withNewCRefValue :: (a -> IO ()) -> a -> (a -> IO b) -> IO b
withNewCRefValue finalizer val action = do
  bracket
    (newCRef finalizer val)
    releaseCRef
    (action . cDeref)
