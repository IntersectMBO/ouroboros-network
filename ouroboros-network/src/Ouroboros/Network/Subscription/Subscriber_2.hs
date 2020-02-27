
module Ouroboros.Network.Subscription.Subscriber
    ( SubscriptionTarget (..)
    , constantSubscriptionTarget
    , listSubscriptionTarget
    ) where

-- | Generate subscription targets in some monad.
-- Examples include obtaining targets from a fixed list, or from a DNS lookup.
--
-- This is in continuation passing style because this admits resource-like
-- semantics: a user can take the next target for the duration of some action
-- and then replace it. This is useful when concurrency is involved.
-- See `concurrentWithReplacement`.
newtype SubscriptionTarget m target = SubscriptionTarget
    { withSubscriptionTarget :: forall t . (Maybe (target, SubscriptionTarget m target) -> m t) -> m t
    }

-- | Gives the same target forever.
constantSubscriptionTarget :: Applicative m => target -> SubscriptionTarget m target
constantSubscriptionTarget target = SubscriptionTarget $ \k ->
  k (Just (target, constantSubscriptionTarget target))

-- | Gives a list of targets in order and then no more.
listSubscriptionTarget
    :: Applicative m
    => [target]
    -> SubscriptionTarget m target
listSubscriptionTarget []     = SubscriptionTarget $ \k -> k Nothing
listSubscriptionTarget (t:ts) = SubscriptionTarget $ \k -> k (Just (t, listSubscriptionTarget ts))

-- Requirements: to get subscription targets from
-- - DNS domain, port and valency (DnsSubscriptionTarget)
-- - List of IP addresses and valency
--
-- See `ipSubscriptionTarget`.

-- | Distributes the targets from one subscription target over n subscription
-- targets.
concurrentWithReplacement
  :: ( MonadSTM m )
  => Word
  -> SubscriptionTarget m target
  -> m [SubscriptionTarget m target]
concurrentWithReplacement valency target = do
  sharedState <- atomically $ newTMVar target
  pure $ fmap [0..valency] $ \_ -> SubscriptionTarget $ \k -> mask $ \restore -> do
    -- Take something from the shared state.
    -- Use it to call `k`.
    -- When `k` returns put it back into the shared state at the end.
    --
    -- If we want to do this with SubscriptionTarget as a parameter, then we
    -- need to immediately get the next targets so that shared state will be
    --
    --   MVar (SubscriptionTarget m target)
    --
    --   nextTarget :: SubscriptionTarget m target -> m (Maybe (Target m target, SubscriptionTarget m target))
    --   withTarget :: Target m target -> (target -> m t) -> m t
    --
    -- Ah no it can work as is now.
    target' <- atomically (takeTMVar sharedState)
    let action = withSubscriptionTarget target' $ \next -> case next of
          -- Must ensure the next target is put back into the TMVar.
          Nothing -> k Nothing
          Just (t, target'') -> do
            k (Just (t, _this target_))
    target'' <- restore action `onException` atomically (putTMVar sharedState target')
    atomically (putTMVar sharedState target'')
    restore (withSubscriptionTarget target') $ \next -> case next of
      Nothing -> 
