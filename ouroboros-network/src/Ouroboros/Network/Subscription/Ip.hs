{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IP subscription worker implentation.
module Ouroboros.Network.Subscription.Ip
    ( ipSubscriptionWorker
    , subscriptionWorker
    , SubscriptionTrace
    , IPSubscriptionTarget (..)
    , WithIPList
    ) where


{- The parallel connection attemps implemented in this module is inspired by
 - RFC8305, https://tools.ietf.org/html/rfc8305 .
 -}

import           Control.Monad.Class.MonadSTM
import           Control.Tracer
import           Data.Time.Clock (DiffTime)
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Subscriber
import           Ouroboros.Network.Subscription.Worker


data IPSubscriptionTarget = IPSubscriptionTarget {
    -- | List of destinations to possibly connect to
      ispIps     :: ![Socket.SockAddr]
    -- | Number of parallel connections to keep actice.
    , ispValency :: !Int
    } deriving (Eq, Show)



-- | Spawns a subscription worker which will attempt to keep the specified
-- number of connections (Valency) active towards the list of IP addresses
-- given in IPSubscriptionTarget.
--
ipSubscriptionWorker
    :: forall a x.
       Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> ConnectionTable IO Socket.SockAddr
    -> Maybe Socket.SockAddr
    -- ^ Local IPv4 address to use, Nothing indicates don't use IPv4
    -> Maybe Socket.SockAddr
    -- ^ Local IPv6 address to use, Nothing indicates don't use IPv6
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> IPSubscriptionTarget
    -> Main IO () x
    -> (Socket.Socket -> IO a)
    -> IO x
ipSubscriptionWorker tracer tbl localIPv4 localIPv6 connectionAttemptDelay ips main k = do
    sVar <- newTVarM ()
    subscriptionWorker
            (ipListTracer localIPv4 localIPv6 (ispIps ips) tracer)
            tbl
            sVar
            ioSocket
            (\_ s -> pure s)
            (\_ s -> pure (s, pure ()))
            main
            localIPv4
            localIPv6
            connectionAttemptDelay
            getTargets
            (ispValency ips)
            k
  where
    getTargets :: IO (SubscriptionTarget IO Socket.SockAddr)
    getTargets = pure $ listSubscriptionTarget $ ispIps ips


-- | Like 'worker' but in 'IO'; It only instantness local address selection.
--
subscriptionWorker
    :: Tracer IO (SubscriptionTrace Socket.SockAddr)
    -> ConnectionTable IO Socket.SockAddr
    -> StateVar IO s

    -> Socket IO Socket.SockAddr Socket.Socket
    -- callbacks
    -> SocketStateChange   IO s Socket.SockAddr
    -> CompleteApplication IO s Socket.SockAddr a
    -- ^ complete connection callback
    -> Main IO s t
    -- ^ main callback

    -> Maybe Socket.SockAddr
    -- ^ local IPv4 address
    -> Maybe Socket.SockAddr
    -- ^ local IPv6 address
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> IO (SubscriptionTarget IO Socket.SockAddr)
    -- ^ subscription targets
    -> Int
    -- ^ valency
    -> (Socket.Socket -> IO a)
    -- ^ application to run on each connection
    -> IO t
subscriptionWorker
  tr tbl sVar socket
  socketStateChangeTx
  completeApplicationTx mainTx
  mbLocalIPv4 mbLocalIPv6
  connectionAttemptDelay getTargets valency k =
    worker tr tbl sVar socket
           socketStateChangeTx
           completeApplicationTx mainTx
           mbLocalIPv4 mbLocalIPv6
           selectAddr connectionAttemptDelay
           getTargets valency k

  where
    selectAddr :: Socket.SockAddr
               -> Maybe Socket.SockAddr
               -- ^ IPv4 address
               -> Maybe Socket.SockAddr
               -- ^ IPv6 address
               -> Maybe Socket.SockAddr
    selectAddr Socket.SockAddrInet{} (Just localAddr) _ = Just localAddr
    selectAddr Socket.SockAddrInet6{} _ (Just localAddr) = Just localAddr
    selectAddr _ _ _ = Nothing

data WithIPList a = WithIPList {
      wilIPv4  :: !(Maybe Socket.SockAddr)
    , wilIPv6  :: !(Maybe Socket.SockAddr)
    , wilDsts  :: ![Socket.SockAddr]
    , wilEvent :: !a
    }

instance (Show a) => Show (WithIPList a) where
    show (WithIPList Nothing (Just wilIPv6) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show wilIPv6) (show wilDsts) (show wilEvent)
    show (WithIPList (Just wilIPv4) Nothing wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show wilIPv4) (show wilDsts) (show wilEvent)
    show WithIPList {wilIPv4, wilIPv6, wilDsts, wilEvent}
      = printf  "IPs: %s %s %s %s" (show wilIPv4) (show wilIPv6)
                                   (show wilDsts) (show wilEvent)

ipListTracer
    :: Maybe Socket.SockAddr
    -> Maybe Socket.SockAddr
    -> [Socket.SockAddr]
    -> Tracer IO (WithIPList a)
    -> Tracer IO a
ipListTracer ipv4 ipv6 ips tr = Tracer $ \s -> traceWith tr $ WithIPList ipv4 ipv6 ips s
