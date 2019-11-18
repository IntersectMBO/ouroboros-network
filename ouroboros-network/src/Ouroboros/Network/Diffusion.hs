{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Ouroboros.Network.Diffusion
  ( DiffusionTracers (..)
  , DiffusionArguments (..)
  , DiffusionApplications (..)
  , OuroborosApplication (..)
  , runDataDiffusion
    -- * re-exports
  , simpleSingletonVersions
  , IPSubscriptionTarget (..)
  , DnsSubscriptionTarget (..)
  , ConnectionId (..)
  )
  where

import qualified Control.Concurrent.Async as Async
import           Control.Tracer (Tracer)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Data.Functor (void)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)

import           Network.TypedProtocol.Driver (TraceSendRecv (..))
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput (..))
import           Network.Mux.Types (MuxTrace (..), WithMuxBearer (..))
import           Network.Socket (SockAddr, AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient ( NodeToClientProtocols (..)
                                                , NodeToClientVersion (..)
                                                )
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode ( NodeToNodeProtocols (..)
                                              , NodeToNodeVersion (..)
                                              )
import qualified Ouroboros.Network.NodeToNode   as NodeToNode
import           Ouroboros.Network.Socket ( ConnectionId (..)
                                          , NetworkMutableState
                                          , newNetworkMutableState
                                          , cleanNetworkMutableState
                                          , NetworkServerTracers (..)
                                          )
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))
import           Ouroboros.Network.Tracers

data DiffusionTracers = DiffusionTracers {
      dtIpSubscriptionTracer  :: Tracer IO (WithIPList (SubscriptionTrace SockAddr))
       -- ^ IP subscription tracer
    , dtDnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace SockAddr))
      -- ^ DNS subscription tracer
    , dtDnsResolverTracer     :: Tracer IO (WithDomainName DnsTrace)
      -- ^ DNS resolver tracer
    , dtMuxTracer             :: Tracer IO (WithMuxBearer ConnectionId MuxTrace)
      -- ^ Mux tracer
    , dtMuxLocalTracer        :: Tracer IO (WithMuxBearer ConnectionId MuxTrace)
      -- ^ Mux tracer for local clients
    , dtHandshakeTracer       :: Tracer IO (TraceSendRecv
                                              (Handshake NodeToNodeVersion CBOR.Term)
                                              ConnectionId
                                              (DecoderFailureOrTooMuchInput CBOR.DeserialiseFailure))
      -- ^ Handshake protocol tracer
    , dtHandshakeLocalTracer  :: Tracer IO (TraceSendRecv
                                              (Handshake NodeToClientVersion CBOR.Term)
                                              ConnectionId
                                              (DecoderFailureOrTooMuchInput CBOR.DeserialiseFailure))
      -- ^ Handshake protocol tracer for local clients
    , dtErrorPolicyTracer     :: Tracer IO (WithAddr SockAddr ErrorPolicyTrace)
    }


-- | Network Node argumets
--
data DiffusionArguments = DiffusionArguments {
      daAddresses    :: [AddrInfo]
      -- ^ diffusion addresses
    , daLocalAddress :: AddrInfo
      -- ^ address for local clients
    , daIpProducers  :: IPSubscriptionTarget
      -- ^ ip subscription addresses
    , daDnsProducers :: [DnsSubscriptionTarget]
      -- ^ list of domain names to subscribe to
    }

data DiffusionApplications = DiffusionApplications {

      daResponderApplication      :: Versions
                                       NodeToNodeVersion
                                       DictVersion
                                       (OuroborosApplication
                                         'ResponderApp
                                         ConnectionId
                                         NodeToNodeProtocols
                                         IO
                                         ByteString
                                         Void
                                         ())
      -- ^ NodeToNode reposnder application (server role)

    , daInitiatorApplication      :: Versions
                                       NodeToNodeVersion
                                       DictVersion 
                                       (OuroborosApplication
                                         'InitiatorApp
                                         ConnectionId
                                         NodeToNodeProtocols
                                         IO
                                         ByteString
                                         () Void)
      -- ^ NodeToNode initiator application (client role)

    , daLocalResponderApplication :: Versions
                                       NodeToClientVersion
                                       DictVersion
                                       (OuroborosApplication
                                          'ResponderApp
                                          ConnectionId
                                          NodeToClientProtocols
                                          IO
                                          ByteString
                                          Void
                                          ())
      -- ^ NodeToClient responder applicaton (server role)

    , daErrorPolicies :: ErrorPolicies SockAddr ()
      -- ^ error policies
      --
      -- TODO: one cannot use `forall a. ErrorPolicies SockAddr a`
    }

runDataDiffusion
    :: DiffusionTracers
    -> DiffusionArguments 
    -> DiffusionApplications
    -> IO ()
runDataDiffusion tracers
                 DiffusionArguments { daAddresses
                                    , daLocalAddress
                                    , daIpProducers
                                    , daDnsProducers
                                    }
                 applications@DiffusionApplications { daErrorPolicies } = do
    -- networking mutable state
    networkState <- newNetworkMutableState
    networkLocalState <- newNetworkMutableState

    void $
      -- clean state thread
      Async.withAsync (cleanNetworkMutableState networkState) $ \cleanNetworkStateThread ->

        -- clean local state thread
        Async.withAsync (cleanNetworkMutableState networkLocalState) $ \cleanLocalNetworkStateThread ->

          -- fork server for local clients
          Async.withAsync (runLocalServer networkLocalState) $ \_ ->

            -- fork servers for remote peers
            withAsyncs (runServer networkState <$> daAddresses) $ \_ ->

              -- fork ip subscription
              Async.withAsync (runIpSubscriptionWorker networkState) $ \_ ->

                -- fork dns subscriptions
                withAsyncs (runDnsSubscriptionWorker networkState <$> daDnsProducers) $ \_ ->

                  -- If any other threads throws 'cleanNetowrkStateThread' and
                  -- 'cleanLocalNetworkStateThread' threads will will finish.
                  Async.waitEither_ cleanNetworkStateThread cleanLocalNetworkStateThread

  where

    DiffusionTracers { dtIpSubscriptionTracer
                     , dtDnsSubscriptionTracer
                     , dtDnsResolverTracer
                     , dtMuxTracer
                     , dtMuxLocalTracer
                     , dtHandshakeTracer
                     , dtHandshakeLocalTracer
                     , dtErrorPolicyTracer
                     } = tracers

    initiatorLocalAddresses :: LocalAddresses SockAddr
    initiatorLocalAddresses = LocalAddresses
      { laIpv4 =
          -- IPv4 address
          --
          -- We can't share portnumber with our server since we run separate
          -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
          -- applications instead of a 'MuxInitiatorAndResponderApplication'.
          -- This means we don't utilise full duplex connection.
          if any (\ai -> Socket.addrFamily ai == Socket.AF_INET) daAddresses
            then Just (Socket.SockAddrInet 0 0)
            else Nothing
      , laIpv6 =
          -- IPv6 address
          if any (\ai -> Socket.addrFamily ai == Socket.AF_INET6) daAddresses
            then Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0)
            else Nothing
      , laUnix = Nothing
      }

    remoteErrorPolicy, localErrorPolicy :: ErrorPolicies SockAddr ()
    remoteErrorPolicy = NodeToNode.remoteNetworkErrorPolicy <> daErrorPolicies
    localErrorPolicy  = NodeToNode.localNetworkErrorPolicy <> daErrorPolicies

    runLocalServer :: NetworkMutableState -> IO Void
    runLocalServer networkLocalState =
      NodeToClient.withServer
        (NetworkServerTracers
          dtMuxLocalTracer
          dtHandshakeLocalTracer
          dtErrorPolicyTracer)
        networkLocalState
        daLocalAddress
        (daLocalResponderApplication applications)
        localErrorPolicy

    runServer :: NetworkMutableState -> AddrInfo -> IO Void
    runServer networkState address =
      NodeToNode.withServer
        (NetworkServerTracers
          dtMuxTracer
          dtHandshakeTracer
          dtErrorPolicyTracer)
        networkState
        address
        (daResponderApplication applications)
        remoteErrorPolicy

    runIpSubscriptionWorker :: NetworkMutableState -> IO Void
    runIpSubscriptionWorker networkState = NodeToNode.ipSubscriptionWorker
      (NetworkIPSubscriptionTracers
        dtMuxTracer
        dtHandshakeTracer
        dtErrorPolicyTracer
        dtIpSubscriptionTracer)
      networkState
      SubscriptionParams
        { spLocalAddresses         = initiatorLocalAddresses
        , spConnectionAttemptDelay = const Nothing
        , spErrorPolicies          = remoteErrorPolicy
        , spSubscriptionTarget     = daIpProducers
        }
      (daInitiatorApplication applications)

    runDnsSubscriptionWorker :: NetworkMutableState -> DnsSubscriptionTarget -> IO Void
    runDnsSubscriptionWorker networkState dnsProducer = NodeToNode.dnsSubscriptionWorker
      (NetworkDNSSubscriptionTracers
        dtMuxTracer
        dtHandshakeTracer
        dtErrorPolicyTracer
        dtDnsSubscriptionTracer
        dtDnsResolverTracer)
      networkState
      SubscriptionParams
        { spLocalAddresses         = initiatorLocalAddresses
        , spConnectionAttemptDelay = const Nothing
        , spErrorPolicies          = remoteErrorPolicy
        , spSubscriptionTarget     = dnsProducer
        }
      (daInitiatorApplication applications)


--
-- Auxilary functions
--

-- | Structural fold using 'Async.withAsync'.
--
withAsyncs :: [IO a] -> ([Async.Async a] -> IO b) -> IO b
withAsyncs as0 k = go [] as0
  where
    go threads []       = k threads
    go threads (a : as) = Async.withAsync a $ \thread -> go (thread : threads) as
