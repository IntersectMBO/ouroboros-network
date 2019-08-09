{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    -- * For testing purposes
  , nbCleanups
  ) where

import           Control.Monad (forM)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Tuple (swap)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

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

-- | Register a clean-up action that will be run when the 'ResourceRegistry'
-- is closed, unless it is 'unregister'ed before that happens.
register :: MonadSTM m => ResourceRegistry m -> m () -> STM m ResourceKey
register ResourceRegistry { _registered, _nextKey } cleanup = do
    key <- readTVar _nextKey
    writeTVar _nextKey $! succ key
    modifyTVar' _registered $ IntMap.insertWith
      (error $ "bug: key already registered: " <> show key)
      key
      cleanup
    return $ ResourceKey key

-- | Allocate a resource and register the corresponding clean-up action so
-- that it will be run when the 'ResourceRegistry' is closed.
--
-- This deals with masking asynchronous exceptions.
allocate :: (MonadSTM m, MonadMask m)
         => ResourceRegistry m
         -> m a         -- ^ Create resource
         -> (a -> m ()) -- ^ Clean-up resource
         -> m (ResourceKey, a)
allocate registry create cleanup = mask $ \unmask -> do
    resource <- unmask create
    key      <- atomically $ register registry (cleanup resource)
    return (key, resource)

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

-- | Return the number of registered clean-up actions in the
-- 'ResourceRegistry'.
--
-- Useful for testing purposes.
nbCleanups :: MonadSTM m => ResourceRegistry m -> m Int
nbCleanups ResourceRegistry { _registered } = atomically $
    IntMap.size <$> readTVar _registered
