{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}

-- | A resource registry that allows registering clean-up actions that will be
-- run when the resource registry is closed.
--
-- Inspired by @resource-simple@.
module Ouroboros.Consensus.Util.ResourceRegistry
  ( ResourceRegistry
  , ResourceKey -- Abstract
  , allocate
  , release
  , new
  , close
  , with
    -- * Forking threads
  , fork
  , forkLinked
  , forkLinkedTransfer
  , ExceptionInForkedThread (..)
    -- * For testing purposes
  , nbCleanups
  ) where

import           Control.Exception (asyncExceptionFromException,
                     asyncExceptionToException)
import           Control.Monad (forM, unless)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Tuple (swap)
import           GHC.Stack

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork hiding (fork)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow hiding (handle)

-- | A registry of resources with clean-up actions associated to them.
data ResourceRegistry m = ResourceRegistry
  { _registered   :: !(TVar m (Map ResourceKey (m ())))
    -- ^ The registered clean-up actions
  , _nextKey      :: !(TVar m Int)
    -- ^ The next value to use for the 'ResourceKey' of the next clean-up
    -- action.
  , _suffix       :: [Int]
    -- ^ Suffix used for resource keys
    --
    -- Avoids confusion between keys from parent and child registries.
  , _creationInfo :: !(CreationInfo m)
    -- ^ Information about the context in which the registry was created
    --
    -- This is used to check for thread safety.
  }

-- | A key given after registering a clean-up action. This key can be used to
-- 'unregister' it.
newtype ResourceKey = ResourceKey [Int]
  deriving (Show, Eq, Ord)

-- | Allocate a resource and register the corresponding clean-up action so
-- that it will be run when the 'ResourceRegistry' is closed.
--
-- This deals with masking asynchronous exceptions.
allocate :: (MonadSTM m, MonadMask m, MonadFork m, HasCallStack)
         => ResourceRegistry m
         -> (ResourceKey -> m a)  -- ^ Create resource
         -> (a -> m ())           -- ^ Clean-up resource
         -> m (ResourceKey, a)
allocate ResourceRegistry { _registered, _nextKey, _creationInfo, _suffix }
         create
         cleanup
       = do
    checkThreadId _creationInfo
    mask $ \restore -> do
      key      <- atomically $ updateTVar' _nextKey $ \k -> (succ k, mkKey k)
      resource <- restore $ create key
      atomically $ modifyTVar' _registered $ Map.insertWith
        (error $ "bug: key already registered: " <> show key)
        key
        (cleanup resource)
      return (key, resource)
  where
    mkKey :: Int -> ResourceKey
    mkKey k = ResourceKey (k : _suffix)

-- | Call a clean-up action and unregister it from the 'ResourceRegistry'.
--
-- Idempotent: noop when the clean-up action has already been unregistered.
--
-- Any exception thrown by the clean-up action is propagated.
release :: forall m. (MonadSTM m, MonadFork m, MonadThrow m)
        => ResourceRegistry m -> ResourceKey -> m ()
release ResourceRegistry { _registered, _creationInfo } key = do
    checkThreadId _creationInfo
    mbCleanup <- atomically $ updateTVar' _registered $
      swap . Map.updateLookupWithKey (\_ _ -> Nothing) key
    sequence_ mbCleanup

-- | Create new resource registry.
new :: (MonadSTM m, MonadFork m, HasCallStack) => m (ResourceRegistry m)
new = do
    _creationInfo <- mkCreationInfo
    atomically $ do
      _registered <- newTVar Map.empty
      _nextKey    <- newTVar 1
      return ResourceRegistry {
          _registered
        , _nextKey
        , _creationInfo
        , _suffix = []
        }

-- | Close the resource registry. All registered clean-up actions are run with
-- exceptions masked. Depending on the resources managed, this may or may not
-- be time consuming.
--
-- In case one or more clean-up actions throw an exception, the remaining
-- clean-up actions will be executed first, and one of the thrown exceptions
-- will be rethrown (which one is undefined) at the end.
--
-- After closing a 'ResourceRegistry', it should no longer be used. This means
-- that a 'ResourceRegistry' can only be closed once.
close :: (MonadSTM m, MonadMask m, MonadFork m) => ResourceRegistry m -> m ()
close ResourceRegistry { _registered, _creationInfo } = do
    checkThreadId _creationInfo
    cleanups <- atomically $ do
      cleanups <- Map.elems <$> readTVar _registered
      writeTVar _registered $ error msg
      return cleanups
    mbEx <- fmap firstJust $ mask_ $ forM cleanups $ \cleanup ->
      either (\(e :: SomeException) -> Just e) (const Nothing) <$> try cleanup
    mapM_ throwM mbEx
  where
    msg = "ResourceRegistry used after closing"

    firstJust :: forall a. [Maybe a] -> Maybe a
    firstJust = listToMaybe . catMaybes

-- | Bracketed variant.
with :: (MonadSTM m, MonadMask m, MonadFork m, HasCallStack)
     => (ResourceRegistry m -> m a) -> m a
with = bracket new close

{-------------------------------------------------------------------------------
  Forking threads
-------------------------------------------------------------------------------}

-- | Fork a new thread using the 'ResourceRegistry'.
--
-- You can use the returned 'Async' handle to wait on it and to obtain the
-- result or exception.
--
-- The 'ResourceRegistry' will cancel the thread using 'uninterruptibleCancel'
-- when it is closed.
fork :: (MonadAsync m, MonadMask m, MonadFork m, HasCallStack)
     => ResourceRegistry m
     -> m a
     -> m (Async m a)
fork registry action = do
    (_key, handle) <- allocate registry
      (\key -> async $ mask $ \restore ->
        restore action
          `finally`
        release registry key)
      uninterruptibleCancel
    return handle

-- | Same as 'fork', but any exception thrown in the forked thread will be
-- rethrown wrapped in 'ExceptionInForkedThread' to the thread that called
-- this function, like 'link'.
forkLinked :: (MonadAsync m, MonadFork m, MonadMask m, HasCallStack)
           => ResourceRegistry m
           -> m a
           -> m (Async m a)
forkLinked registry action = do
    me <- myThreadId
    fork registry $
      action `catch` \e -> do
        case fromException e of
          Just AsyncCancelled{} -> return ()
          Nothing               -> throwTo me $ ExceptionInForkedThread e
        throwM e

-- | An exception thrown by a thread forked using 'forkLinked' or
-- 'forkNonTerminating'.
data ExceptionInForkedThread = ExceptionInForkedThread SomeException
    deriving (Show)

instance Exception ExceptionInForkedThread where
  fromException = asyncExceptionFromException
  toException   = asyncExceptionToException

{-------------------------------------------------------------------------------
  Subregistry
-------------------------------------------------------------------------------}

-- | Reserve key in registry without allocating a resource
--
-- Internal auxiliary used in 'forkFinallyTransfer'
allocateKey :: ( MonadSTM  m
               , MonadMask m
               , MonadFork m
               )
            => ResourceRegistry m -> m ResourceKey
allocateKey r = fst <$> allocate r (\_ -> return ()) (\() -> return ())

-- | Fork a thread and transfer resource ownership on termination to parent
--
-- The resource registry is not thread safe: when thread B allocates a resource
-- in a registry held by thread A, and thread A terminates, thread B may now
-- continue to run with its resources deallocated. For this reason, we insist
-- that resource registries can be used by a single thread only; threads should
-- always use their own, private, resource registry.
--
-- However, it is occassionally useful to allow a thread to transfer ownership
-- of a resource to a parent thread: on termination of the child thread, its
-- resources are not cleaned up, but transferred to the resource registry of
-- the parent.
--
-- After transfer, resource keys from the child registry can be used for the
-- parent one. In other words, it is safe to return resource keys as the result
-- of the child thread.
--
-- Note that there is intentionally no non-linked variant of this function:
-- if the intention is that the child thread creates a resource that is used
-- by the parent thread on termination, a non-linked version does not make much
-- sense: if the child thread throws an exception, it may have allocated /some/
-- resources but not others, making its results basically unusable to the
-- parent thread.
forkLinkedTransfer :: forall m a.
                       ( MonadSTM   m
                       , MonadMask  m
                       , MonadFork  m
                       , MonadAsync m
                       )
                    => ResourceRegistry m
                    -> (ResourceRegistry m -> m a)
                    -> m (Async m a)
forkLinkedTransfer parent action = do
   ResourceKey parentKey <- allocateKey parent
   forkLinked parent $ with $ \child ->
     action (child { _suffix = parentKey }) `finally` transferFrom child
  where
    transferFrom :: ResourceRegistry m -> m ()
    transferFrom child = atomically $ do
        rs <- updateTVar' (_registered child) $ \rs -> (Map.empty, rs)
        modifyTVar' (_registered parent) $ \rs' -> Map.union rs' rs

{-------------------------------------------------------------------------------
  For testing purposes
-------------------------------------------------------------------------------}

-- | Return the number of registered clean-up actions in the
-- 'ResourceRegistry'.
--
-- Useful for testing purposes.
nbCleanups :: MonadSTM m => ResourceRegistry m -> m Int
nbCleanups ResourceRegistry { _registered } = atomically $
    Map.size <$> readTVar _registered

{-------------------------------------------------------------------------------
  Internal: thread safety
-------------------------------------------------------------------------------}

-- | Information about the context in which the registry was created
--
-- This allows us to give more detailed exceptions
data CreationInfo m = CreationInfo {
      -- | Thread ID of the thread that created this registry
      _creationThreadId  :: !(ThreadId m)

      -- | Call stack when the registry was created
    , _creationCallStack :: !CallStack
    }

deriving instance MonadFork m => Show (CreationInfo m)

mkCreationInfo :: (MonadFork m, HasCallStack) => m (CreationInfo m)
mkCreationInfo = do
    _creationThreadId <- myThreadId
    return $ CreationInfo { _creationThreadId, _creationCallStack = callStack }

data ResourceRegistryThreadException =
    -- | The resource registry should only be used from the thread that
    -- created it
    forall m. MonadFork m => ResourceRegistryUsedFromDifferentThread {
          -- | Information about the context in which the registry was created
          registryExceptionCreationInfo :: CreationInfo m

          -- | The thread ID of the thread that tried to use the registry
        , registryExceptionUsedIn       :: ThreadId m

          -- | CallStack of the invalid call
        , registryExceptionCallstack    :: CallStack
        }

deriving instance Show ResourceRegistryThreadException

instance Exception ResourceRegistryThreadException where
  displayException (ResourceRegistryUsedFromDifferentThread
                      (CreationInfo createdIn creationStack)
                      usedIn
                      stack) = concat [
      "Thread registry created in thread "
    , show createdIn
    , " with call stack "
    , prettyCallStack creationStack
    , " used in thread "
    , show usedIn
    , " with call stack "
    , prettyCallStack stack
    ]

-- | Internal helper making sure registry is not used in different thread
checkThreadId :: forall m. (MonadFork m, MonadThrow m, HasCallStack)
              => CreationInfo m -> m ()
checkThreadId info@CreationInfo{ _creationThreadId } = do
    tid <- myThreadId
    unless (tid == _creationThreadId) $ do
      throwM $ ResourceRegistryUsedFromDifferentThread info tid callStack
