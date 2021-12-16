{-# LANGUAGE NamedFieldPuns #-}

-- Subscription worker for client applications connecting with 'LocalSnocket'
-- which is using either unix sockets or Windows' named pipes.
--
module Ouroboros.Network.Subscription.Client
  ( ClientSubscriptionParams (..)
  , clientSubscriptionWorker
  ) where

import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Data.Functor.Identity (Identity (..))
import           Data.Void (Void)

import           Ouroboros.Network.ErrorPolicy (ErrorPolicies, ErrorPolicyTrace,
                     WithAddr, completeApplicationTx)
import           Ouroboros.Network.Snocket (LocalAddress, LocalSnocket,
                     LocalSocket)
import           Ouroboros.Network.Socket (NetworkMutableState (..))
import           Ouroboros.Network.Subscription.Ip (mainTx, socketStateChangeTx)
import           Ouroboros.Network.Subscription.Subscriber
import           Ouroboros.Network.Subscription.Worker


data ClientSubscriptionParams a = ClientSubscriptionParams
  { cspAddress                :: !LocalAddress
  -- ^ unix socket or named pipe address
  , cspConnectionAttemptDelay :: !(Maybe DiffTime)
  -- ^ delay between connection attempts
  , cspErrorPolicies          :: !ErrorPolicies
  -- ^ error policies for subscription worker
  }

-- | Client subscription worker keeps subsribing to the 'LocalAddress' using
-- either unix socket or named pipe.
--
clientSubscriptionWorker
    :: LocalSnocket
    -> Tracer IO (SubscriptionTrace LocalAddress)
    -> Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)
    -> NetworkMutableState LocalAddress
    -> ClientSubscriptionParams a
    -> (LocalSocket -> IO a)
    -> IO Void
clientSubscriptionWorker snocket
                         tracer
                         errorPolicyTracer
                         NetworkMutableState { nmsConnectionTable, nmsPeerStates }
                         ClientSubscriptionParams { cspAddress
                                                  , cspConnectionAttemptDelay
                                                  , cspErrorPolicies
                                                  }
                         k =
    worker tracer
           errorPolicyTracer
           nmsConnectionTable
           nmsPeerStates
           snocket
           WorkerCallbacks
            { wcSocketStateChangeTx   = socketStateChangeTx
            , wcCompleteApplicationTx = completeApplicationTx cspErrorPolicies
            , wcMainTx                = mainTx
            }
           workerParams
           k
  where
    workerParams :: WorkerParams IO Identity LocalAddress
    workerParams = WorkerParams {
        wpLocalAddresses         = Identity cspAddress,
        wpSelectAddress          = \_ (Identity addr) -> Just addr,
        wpConnectionAttemptDelay = const cspConnectionAttemptDelay,
        wpSubscriptionTarget     = pure (listSubscriptionTarget [cspAddress]),
        wpValency                = 1
      }
