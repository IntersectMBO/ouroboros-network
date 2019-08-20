{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IP subscription worker implentation.
module Ouroboros.Network.Subscription.Ip
    ( ipSubscriptionWorker
    , subscriptionWorker
    , SubscriptionTrace
    , IPSubscriptionTarget (..)
    , ipSubscriptionTarget
    , WithIPList (..)

    -- * 'PeerState' STM transactions
    , BeforeConnect
    , runBeforeConnect
    , beforeConnectTx
    , completeApplicationTx
    , socketStateChangeTx
    , mainTx
    ) where


{- The parallel connection attemps implemented in this module is inspired by
 - RFC8305, https://tools.ietf.org/html/rfc8305 .
 -}

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadThrow
import           Control.Tracer
import           Data.Time.Clock (DiffTime)
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.Socket
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Subscription.PeerState
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
    :: forall a.
       Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr (Time IO))
    -> Maybe Socket.SockAddr
    -- ^ Local IPv4 address to use, Nothing indicates don't use IPv4
    -> Maybe Socket.SockAddr
    -- ^ Local IPv6 address to use, Nothing indicates don't use IPv6
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> [ErrorPolicy]
    -> (Time IO -> Socket.SockAddr -> a -> SuspendDecision DiffTime)
    -> IPSubscriptionTarget
    -> (Socket.Socket -> IO a)
    -> IO Void
ipSubscriptionWorker tracer errTracer tbl peerStatesVar localIPv4 localIPv6 connectionAttemptDelay errPolicies returnCallback ips k = do
    subscriptionWorker
            tracer'
            errTracer
            tbl
            peerStatesVar
            localIPv4
            localIPv6
            connectionAttemptDelay
            (pure $ ipSubscriptionTarget tracer' peerStatesVar $ ispIps ips)
            (ispValency ips)
            errPolicies
            returnCallback
            mainTx
            k
  where
    tracer' =  WithIPList localIPv4 localIPv6 (ispIps ips) `contramap` tracer

ipSubscriptionTarget :: forall m addr.
                        ( MonadSTM  m
                        , MonadTime m
                        , Ord addr
                        )
                     => Tracer m (SubscriptionTrace addr)
                     -> StrictTVar m (PeerStates m addr (Time m))
                     -> [addr]
                     -> SubscriptionTarget m addr
ipSubscriptionTarget tr peerStatesVar ips = go ips
  where
    go :: [addr]
       -> SubscriptionTarget m addr
    go [] = SubscriptionTarget $ pure Nothing
    go (a : as) = SubscriptionTarget $ do
      b <- runBeforeConnect peerStatesVar beforeConnectTx a
      if b
        then do
          traceWith tr $ SubscriptionTraceTryConnectToPeer a
          pure $ Just (a, go as)
        else do
          traceWith tr $ SubscriptionTraceSkippingPeer a
          getSubscriptionTarget $ go as


-- when creating a new socket: register consumer thread
-- when tearing down a socket: unregister consumer thread
socketStateChangeTx
    :: ( Ord addr
       , Show addr
       )
    => SocketStateChange IO
        (PeerStates IO addr (Time IO))
        addr

socketStateChangeTx (CreatedSocket addr thread) ps =
  pure (registerConsumer addr thread ps)

socketStateChangeTx ClosedSocket{} ps@ThrowException{} =
  pure ps

socketStateChangeTx (ClosedSocket addr thread) ps =
  pure $ unregisterConsumer addr thread ps


-- | Main callback.  It throws an exception when the state becomes
-- 'ThrowException'.  This exception is thrown from the main thread.
--
mainTx :: ( MonadThrow m
          , MonadThrow (STM m)
          , MonadSTM m
          )
       => Main m (PeerStates m addr (Time m)) Void
mainTx (ThrowException e) = throwM e
mainTx PeerStates{}       = retry


-- | Like 'worker' but in 'IO'; It provides address selection function,
-- 'SocketStateChange' and 'CompleteApplication' callbacks.  The 'Main'
-- callback is left as it's useful for testing purposes.
--
subscriptionWorker
    :: Tracer IO (SubscriptionTrace Socket.SockAddr)
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> ConnectionTable IO Socket.SockAddr
    -> StateVar IO (PeerStates IO Socket.SockAddr (Time IO))

    -> Maybe Socket.SockAddr
    -- ^ local IPv4 address
    -> Maybe Socket.SockAddr
    -- ^ local IPv6 address
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> IO (SubscriptionTarget IO Socket.SockAddr)
    -- ^ subscription targets
    -> Int
    -- ^ valency
    -> [ErrorPolicy]
    -> (Time IO -> Socket.SockAddr -> a -> SuspendDecision DiffTime)
    -> Main IO (PeerStates IO Socket.SockAddr (Time IO)) x
    -- ^ main callback
    -> (Socket.Socket -> IO a)
    -- ^ application to run on each connection
    -> IO x
subscriptionWorker
  tracer errTracer tbl sVar mbLocalIPv4 mbLocalIPv6
  connectionAttemptDelay getTargets valency errPolicies returnCallback main k =
    worker tracer
           tbl
           sVar
           ioSocket
           socketStateChangeTx
           (completeApplicationTx errTracer returnCallback errPolicies)
           main
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
