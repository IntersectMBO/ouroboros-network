module Ouroboros.Network.Tracers
  ( NetworkSubscriptionTracers (..)
  , NetworkIPSubscriptionTracers
  , nullNetworkSubscriptionTracers
  , NetworkDNSSubscriptionTracers (..)
  , nullNetworkDNSSubscriptionTracers
  ) where

import qualified Codec.CBOR.Term as CBOR
import           Control.Tracer (Tracer, nullTracer)

import           Network.Mux.Trace

import           Ouroboros.Network.Driver (TraceSendRecv)
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Socket (ConnectionId)
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Ip

-- | IP subscription tracers.
--
data NetworkSubscriptionTracers withIPList addr vNumber = NetworkSubscriptionTracers {
      nsMuxTracer          :: Tracer IO (WithMuxBearer (ConnectionId addr) MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nsHandshakeTracer    :: Tracer IO (WithMuxBearer (ConnectionId addr)
                                            (TraceSendRecv (Handshake vNumber CBOR.Term))),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      nsErrorPolicyTracer  :: Tracer IO (WithAddr addr ErrorPolicyTrace),
      -- ^ error policy tracer; must not be 'nullTracer', otherwise all the
      -- exceptions which are not matched by any error policy will be caught
      -- and not logged or rethrown.
      nsSubscriptionTracer :: Tracer IO (withIPList (SubscriptionTrace addr))
      -- ^ subscription tracers; it is infrequent it should not be 'nullTracer'
      -- by default.
    }

type NetworkIPSubscriptionTracers addr vNumber =
     NetworkSubscriptionTracers WithIPList addr vNumber

nullNetworkSubscriptionTracers :: NetworkSubscriptionTracers withIPList addr vNumber
nullNetworkSubscriptionTracers = NetworkSubscriptionTracers {
      nsMuxTracer          = nullTracer,
      nsHandshakeTracer    = nullTracer,
      nsErrorPolicyTracer  = nullTracer,
      nsSubscriptionTracer = nullTracer
    }

-- | DNS subscription tracers.
--
data NetworkDNSSubscriptionTracers vNumber addr = NetworkDNSSubscriptionTracers {
      ndstMuxTracer          :: Tracer IO (WithMuxBearer (ConnectionId addr) MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      ndstHandshakeTracer    :: Tracer IO (WithMuxBearer (ConnectionId addr)
                                            (TraceSendRecv (Handshake vNumber CBOR.Term))),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      ndstErrorPolicyTracer  :: Tracer IO (WithAddr addr ErrorPolicyTrace),
      -- ^ error policy tracer; must not be 'nullTracer', otherwise all the
      -- exceptions which are not matched by any error policy will be caught
      -- and not logged or rethrown.
      ndstSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace addr)),
      -- ^ subscription tracer; it is infrequent it should not be 'nullTracer'
      -- by default.
      ndstDnsTracer          :: Tracer IO (WithDomainName DnsTrace)
      -- ^ dns resolver tracer; it is infrequent it should not be 'nullTracer'
      -- by default.

    }

nullNetworkDNSSubscriptionTracers :: NetworkDNSSubscriptionTracers vNumber peerid
nullNetworkDNSSubscriptionTracers = NetworkDNSSubscriptionTracers {
      ndstMuxTracer          = nullTracer,
      ndstHandshakeTracer    = nullTracer,
      ndstErrorPolicyTracer  = nullTracer,
      ndstSubscriptionTracer = nullTracer,
      ndstDnsTracer          = nullTracer
    }
