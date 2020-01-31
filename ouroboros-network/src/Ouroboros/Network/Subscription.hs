-- | Public interface of 'Ouroboros.Network.Subscription' workers.
--
module Ouroboros.Network.Subscription
    ( worker

      -- * IP Subscription Worker
    , IPSubscriptionTarget (..)
      -- * DNS Subscription Worker
    , DnsSubscriptionTarget (..)
    , ConnectResult (..)
    , LocalAddresses (..)

      -- * Constants
    , defaultConnectionAttemptDelay
    , minConnectionAttemptDelay
    , maxConnectionAttemptDelay
    , ipRetryDelay

      -- * Errors
    , SubscriberError (..)

      -- * Tracing
    , SubscriptionTrace (..)
    , WithIPList (..)
    , DnsTrace (..)
    , WithDomainName (..)
    ) where

import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Worker
