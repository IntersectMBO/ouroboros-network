-- | Definition of a modular subscriber action, which maintains subscriptions
-- through failures etc.

module Ouroboros.Network.Subscription.Subscriber
    ( SubscriptionTarget (..)
    , constantSubscriptionTarget
    , listSubscriptionTarget
    ) where

-- | Generate subscription targets in some monad.
-- TBD any value in using a streaming solution like conduit?
newtype SubscriptionTarget m target = SubscriptionTarget
    { getSubscriptionTarget :: m (Maybe (target, SubscriptionTarget m target))
    }

constantSubscriptionTarget :: Applicative m => target -> SubscriptionTarget m target
constantSubscriptionTarget target =
    SubscriptionTarget (pure (Just (target, constantSubscriptionTarget target)))

listSubscriptionTarget
    :: Applicative m
    => [target]
    -> SubscriptionTarget m target
listSubscriptionTarget []     = SubscriptionTarget $ pure Nothing
listSubscriptionTarget (t:ts) = SubscriptionTarget $ pure (Just (t, listSubscriptionTarget ts))

