-- | Public interface of 'Ouroboros.Network.Subscription' workers.
--
module Ouroboros.Network.Subscription
  ( -- * IP Subscription Worker
    ipSubscriptionWorker
  , IPSubscriptionTarget (..)
    -- * DNS Subscription Worker
  , dnsSubscriptionWorker
  , DnsSubscriptionTarget (..)
  , ConnectResult (..)
    -- * Constants
  , defaultConnectionAttemptDelay
  , minConnectionAttemptDelay
  , maxConnectionAttemptDelay
  , ipRetryDelay
  , resolutionDelay
    -- * Errors
  , SubscriberError (..)
    -- * Tracing
  , SubscriptionTrace (..)
  , WithIPList (..)
  , DnsTrace (..)
  , WithDomainName (..)
  ) where

import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Worker
