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
    , IPSubscriptionTarget (..)
    , ipSubscriptionTarget

    --  * Traces
    , SubscriptionTrace (..)
    , ErrorPolicyTrace (..)
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
    :: forall a void.
       Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> NetworkMutableState
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> ErrorPolicies Socket.SockAddr a
    -> IPSubscriptionTarget
    -> (Socket.Socket -> IO a)
    -> IO void
ipSubscriptionWorker tracer errorPolicyTracer networkState@NetworkMutableState { nmsPeerStates } localAddresses connectionAttemptDelay errPolicies ips k = do
    subscriptionWorker
            tracer'
            errorPolicyTracer
            networkState
            localAddresses
            connectionAttemptDelay
            (pure $ ipSubscriptionTarget tracer' nmsPeerStates $ ispIps ips)
            (ispValency ips)
            errPolicies
            mainTx
            k
  where
    tracer' = (WithIPList localAddresses (ispIps ips)
                `contramap` tracer)

ipSubscriptionTarget :: forall m addr.
                        ( MonadSTM  m
                        , MonadTime m
                        , Ord addr
                        )
                     => Tracer m (SubscriptionTrace addr)
                     -> StrictTVar m (PeerStates m addr)
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
        (PeerStates IO addr)
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
       => Main m (PeerStates m addr) void
mainTx (ThrowException e) = throwM e
mainTx PeerStates{}       = retry


-- | Like 'worker' but in 'IO'; It provides address selection function,
-- 'SocketStateChange' and 'CompleteApplication' callbacks.  The 'Main'
-- callback is left as it's useful for testing purposes.
--
subscriptionWorker
    :: Tracer IO (SubscriptionTrace Socket.SockAddr)
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> NetworkMutableState

    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> IO (SubscriptionTarget IO Socket.SockAddr)
    -- ^ subscription targets
    -> Int
    -- ^ valency
    -> ErrorPolicies Socket.SockAddr a
    -> Main IO (PeerStates IO Socket.SockAddr) x
    -- ^ main callback
    -> (Socket.Socket -> IO a)
    -- ^ application to run on each connection
    -> IO x
subscriptionWorker
  tracer errorPolicyTracer NetworkMutableState { nmsConnectionTable, nmsPeerStates } localAddresses
  connectionAttemptDelay getTargets valency errPolicies main k =
    worker tracer
           errorPolicyTracer
           nmsConnectionTable
           nmsPeerStates
           ioSocket
           socketStateChangeTx
           (completeApplicationTx errPolicies)
           main
           localAddresses
           selectAddr connectionAttemptDelay
           getTargets valency k

  where
    selectAddr :: Socket.SockAddr
               -> LocalAddresses Socket.SockAddr
               -> Maybe Socket.SockAddr
    selectAddr Socket.SockAddrInet{}  (LocalAddresses (Just localAddr) _ _ ) = Just localAddr
    selectAddr Socket.SockAddrInet6{} (LocalAddresses _ (Just localAddr) _ ) = Just localAddr
    selectAddr Socket.SockAddrUnix{}  (LocalAddresses _ _ (Just localAddr) ) = Just localAddr
    selectAddr _ _ = Nothing

data WithIPList a = WithIPList {
      wilSrc   :: !(LocalAddresses Socket.SockAddr)
    , wilDsts  :: ![Socket.SockAddr]
    , wilEvent :: !a
    }

instance (Show a) => Show (WithIPList a) where
    show (WithIPList (LocalAddresses Nothing (Just ipv6) Nothing) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show ipv6) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses (Just ipv4) Nothing Nothing) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show ipv4) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses Nothing Nothing (Just unix)) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show unix) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses (Just ipv4) (Just ipv6) Nothing) wilDsts wilEvent) =
        printf  "IPs: %s %s %s %s" (show ipv4) (show ipv6)
                                   (show wilDsts) (show wilEvent)
    show WithIPList {wilSrc, wilDsts, wilEvent} =
        printf "IPs: %s %s %s" (show wilSrc) (show wilDsts) (show wilEvent)

