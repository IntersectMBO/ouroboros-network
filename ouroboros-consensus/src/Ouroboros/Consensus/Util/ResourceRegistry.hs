{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

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
    -- * Subregistry
  , withSubregistry
    -- * Forking threads
  , fork
  , forkLinked
  , ExceptionInForkedThread (..)
    -- * For testing purposes
  , nbCleanups
  ) where

import           Control.Exception (asyncExceptionFromException,
                     asyncExceptionToException)
import           Control.Monad (forM)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Tuple (swap)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork hiding (fork)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow hiding (handle)

-- | A registry of resources with clean-up actions associated to them.
data ResourceRegistry m = ResourceRegistry
  { _registered :: !(TVar m (IntMap (m ())))
    -- ^ The registered clean-up actions
  , _nextKey    :: !(TVar m Int)
    -- ^ The next value to use for the 'ResourceKey' of the next clean-up
    -- action.
  }

-- | A key given after registering a clean-up action. This key can be used to
-- 'unregister' it.
newtype ResourceKey = ResourceKey Int

-- | Allocate a resource and register the corresponding clean-up action so
-- that it will be run when the 'ResourceRegistry' is closed.
--
-- This deals with masking asynchronous exceptions.
allocate :: (MonadSTM m, MonadMask m)
         => ResourceRegistry m
         -> (ResourceKey -> m a)  -- ^ Create resource
         -> (a -> m ())           -- ^ Clean-up resource
         -> m (ResourceKey, a)
allocate ResourceRegistry { _registered, _nextKey } create cleanup =
    mask $ \restore -> do
      key      <- atomically $ updateTVar' _nextKey $ \k -> (succ k, k)
      resource <- restore $ create $ ResourceKey key
      atomically $ modifyTVar' _registered $ IntMap.insertWith
        (error $ "bug: key already registered: " <> show key)
        key
        (cleanup resource)
      return (ResourceKey key, resource)

-- | Call a clean-up action and unregister it from the 'ResourceRegistry'.
--
-- Idempotent: noop when the clean-up action has already been unregistered.
--
-- Any exception thrown by the clean-up action is propagated.
release :: forall m. MonadSTM m => ResourceRegistry m -> ResourceKey -> m ()
release ResourceRegistry { _registered } (ResourceKey key) = do
    mbCleanup <- atomically $ updateTVar' _registered $
      swap . IntMap.updateLookupWithKey (\_ _ -> Nothing) key
    sequence_ mbCleanup

-- | Create new resource registry.
new :: MonadSTM m => m (ResourceRegistry m)
new = atomically $ do
    _registered <- newTVar IntMap.empty
    _nextKey    <- newTVar 1
    return ResourceRegistry { _registered, _nextKey }

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
close :: (MonadSTM m, MonadMask m) => ResourceRegistry m -> m ()
close ResourceRegistry { _registered } = do
    cleanups <- atomically $ do
      cleanups <- IntMap.elems <$> readTVar _registered
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
with :: (MonadSTM m, MonadMask m) => (ResourceRegistry m -> m a) -> m a
with = bracket new close

{-------------------------------------------------------------------------------
  Subregistry
-------------------------------------------------------------------------------}

-- | Create a subregistry.
--
-- Similar to 'with'. The subregistry and all the resources that are allocated
-- in it will be released when the parent (or any ancestor) registry is
-- closed.
withSubregistry :: forall m a. (MonadSTM m, MonadMask m)
                => ResourceRegistry m
                -> (ResourceRegistry m -> m a)
                -> m a
withSubregistry registry k = bracket
    createSubregistry
    closeSubregistry
    (\(_key, subregistry) -> k subregistry)
  where
    createSubregistry :: m (ResourceKey, ResourceRegistry m)
    createSubregistry = allocate
      registry
      (const new)
      close

    closeSubregistry :: (ResourceKey, ResourceRegistry m) -> m ()
    closeSubregistry (key, _) = release
      registry
      key

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
fork :: (MonadAsync m, MonadMask m)
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
forkLinked :: (MonadAsync m, MonadFork m, MonadMask m)
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
  For testing purposes
-------------------------------------------------------------------------------}

-- | Return the number of registered clean-up actions in the
-- 'ResourceRegistry'.
--
-- Useful for testing purposes.
nbCleanups :: MonadSTM m => ResourceRegistry m -> m Int
nbCleanups ResourceRegistry { _registered } = atomically $
    IntMap.size <$> readTVar _registered
