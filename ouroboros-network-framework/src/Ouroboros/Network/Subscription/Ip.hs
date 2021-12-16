{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IP subscription worker implentation.
module Ouroboros.Network.Subscription.Ip
  ( SubscriptionParams (..)
  , IPSubscriptionParams
  , ipSubscriptionWorker
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
    -- * Utilitity functions
  , selectSockAddr
  ) where


{- The parallel connection attemps implemented in this module is inspired by
 - RFC8305, https://tools.ietf.org/html/rfc8305 .
 -}

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Snocket (Snocket)
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.PeerState
import           Ouroboros.Network.Subscription.Subscriber
import           Ouroboros.Network.Subscription.Worker


data IPSubscriptionTarget = IPSubscriptionTarget {
    -- | List of destinations to possibly connect to
      ispIps     :: ![Socket.SockAddr]
    -- | Number of parallel connections to keep actice.
    , ispValency :: !Int
    } deriving (Eq, Show)


-- | 'ipSubscriptionWorker' and 'dnsSubscriptionWorker' parameters
--
data SubscriptionParams a target = SubscriptionParams
  { spLocalAddresses         :: LocalAddresses Socket.SockAddr
  , spConnectionAttemptDelay :: Socket.SockAddr -> Maybe DiffTime
    -- ^ should return expected delay for the given address
  , spErrorPolicies          :: ErrorPolicies
  , spSubscriptionTarget     :: target
  }

type IPSubscriptionParams a = SubscriptionParams a IPSubscriptionTarget

-- | Spawns a subscription worker which will attempt to keep the specified
-- number of connections (Valency) active towards the list of IP addresses
-- given in IPSubscriptionTarget.
--
ipSubscriptionWorker
    :: forall a.
       Snocket IO Socket.Socket Socket.SockAddr
    -> Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> NetworkMutableState Socket.SockAddr
    -> IPSubscriptionParams a
    -> (Socket.Socket -> IO a)
    -> IO Void
ipSubscriptionWorker snocket subscriptionTracer errorPolicyTracer
                     networkState@NetworkMutableState { nmsPeerStates }
                     SubscriptionParams { spLocalAddresses
                                        , spConnectionAttemptDelay
                                        , spSubscriptionTarget
                                        , spErrorPolicies
                                        }
                     k =
    subscriptionWorker snocket
                       subscriptionTracer'
                       errorPolicyTracer
                       networkState
                       workerParams
                       spErrorPolicies
                       mainTx
                       k
  where
    workerParams = WorkerParams {
        wpLocalAddresses         = spLocalAddresses,
        wpConnectionAttemptDelay = spConnectionAttemptDelay,
        wpSubscriptionTarget     =
          pure $ ipSubscriptionTarget subscriptionTracer' nmsPeerStates
                                      (ispIps spSubscriptionTarget),
        wpValency                = ispValency spSubscriptionTarget,
        wpSelectAddress          = selectSockAddr
      }

    subscriptionTracer' = (WithIPList spLocalAddresses (ispIps spSubscriptionTarget)
              `contramap` subscriptionTracer)

selectSockAddr :: Socket.SockAddr
               -> LocalAddresses Socket.SockAddr
               -> Maybe Socket.SockAddr
selectSockAddr Socket.SockAddrInet{}  (LocalAddresses (Just localAddr) _ _ ) = Just localAddr
selectSockAddr Socket.SockAddrInet6{} (LocalAddresses _ (Just localAddr) _ ) = Just localAddr
selectSockAddr Socket.SockAddrUnix{}  (LocalAddresses _ _ (Just localAddr) ) = Just localAddr
selectSockAddr _ _ = Nothing


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
    :: Ord addr
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
mainTx :: ( MonadThrow (STM m)
          , MonadSTM m
          )
       => Main m (PeerStates m addr) Void
mainTx (ThrowException e) = throwIO e
mainTx PeerStates{}       = retry


-- | Like 'worker' but in 'IO'; It provides address selection function,
-- 'SocketStateChange' and 'CompleteApplication' callbacks.  The 'Main'
-- callback is left as it's useful for testing purposes.
--
subscriptionWorker
    :: Snocket IO Socket.Socket Socket.SockAddr
    -> Tracer IO (SubscriptionTrace Socket.SockAddr)
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> NetworkMutableState Socket.SockAddr
    -> WorkerParams IO LocalAddresses Socket.SockAddr
    -> ErrorPolicies
    -> Main IO (PeerStates IO Socket.SockAddr) x
    -- ^ main callback
    -> (Socket.Socket -> IO a)
    -- ^ application to run on each connection
    -> IO x
subscriptionWorker snocket
                   tracer
                   errorPolicyTracer
                   NetworkMutableState { nmsConnectionTable, nmsPeerStates }
                   workerParams
                   errorPolicies
                   main k =
    worker tracer
           errorPolicyTracer
           nmsConnectionTable
           nmsPeerStates
           snocket
           WorkerCallbacks
             { wcSocketStateChangeTx   = socketStateChangeTx
             , wcCompleteApplicationTx = completeApplicationTx errorPolicies
             , wcMainTx                = main
             }
           workerParams
           k

data WithIPList a = WithIPList {
      wilSrc   :: (LocalAddresses Socket.SockAddr)
    , wilDsts  :: [Socket.SockAddr]
    , wilEvent :: a
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

