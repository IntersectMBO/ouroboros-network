module Ouroboros.Network.Tracers
  ( NetworkIPSubscriptionTracers (..)
  , nullNetworkIPSubscriptionTracers
  , NetworkDNSSubscriptionTracers (..)
  , nullNetworkDNSSubscriptionTracers
  ) where

import           Control.Tracer (Tracer, nullTracer)
import qualified Codec.CBOR.Term as CBOR

import           Network.Mux.Trace

import           Network.TypedProtocol.Driver (TraceSendRecv)

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Socket (ConnectionId)
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns

-- | IP subscription tracers.
--
data NetworkIPSubscriptionTracers addr ptcl vNumber = NetworkIPSubscriptionTracers {
      nistMuxTracer          :: Tracer IO (WithMuxBearer (ConnectionId addr) MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nistHandshakeTracer    :: Tracer IO (WithMuxBearer (ConnectionId addr)
                                            (TraceSendRecv (Handshake vNumber CBOR.Term))),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      nistErrorPolicyTracer  :: Tracer IO (WithAddr addr ErrorPolicyTrace),
      -- ^ error policy tracer; must not be 'nullTracer', otherwise all the
      -- exceptions which are not matched by any error policy will be caught
      -- and not logged or rethrown.
      nistSubscriptionTracer :: Tracer IO (WithIPList (SubscriptionTrace addr))
      -- ^ subscription tracers; it is infrequent it should not be 'nullTracer'
      -- by default.
    }

nullNetworkIPSubscriptionTracers :: NetworkIPSubscriptionTracers addr ptcl vNumber
nullNetworkIPSubscriptionTracers = NetworkIPSubscriptionTracers {
      nistMuxTracer          = nullTracer,
      nistHandshakeTracer    = nullTracer,
      nistErrorPolicyTracer  = nullTracer,
      nistSubscriptionTracer = nullTracer
    }

-- | DNS subscription tracers.
--
data NetworkDNSSubscriptionTracers ptcl vNumber addr = NetworkDNSSubscriptionTracers {
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

nullNetworkDNSSubscriptionTracers :: NetworkDNSSubscriptionTracers ptcl vNumber peerid
nullNetworkDNSSubscriptionTracers = NetworkDNSSubscriptionTracers {
      ndstMuxTracer          = nullTracer,
      ndstHandshakeTracer    = nullTracer,
      ndstErrorPolicyTracer  = nullTracer,
      ndstSubscriptionTracer = nullTracer,
      ndstDnsTracer          = nullTracer
    }
