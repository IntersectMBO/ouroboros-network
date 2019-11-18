module Ouroboros.Network.Tracers
  ( NetworkIPSubscriptionTracers (..)
  , nullNetworkIPSubscriptionTracers
  , NetworkDNSSubscriptionTracers (..)
  , nullNetworkDNSSubscriptionTracers
  ) where

import           Control.Tracer (Tracer, nullTracer)
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (DeserialiseFailure)

import           Network.Socket (SockAddr)
import           Network.Mux.Types

import           Network.TypedProtocol.Driver (TraceSendRecv)
import           Network.TypedProtocol.Driver.ByteLimit

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Socket (ConnectionId)
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns

-- | IP subscription tracers.
--
data NetworkIPSubscriptionTracers ptcl vNumber = NetworkIPSubscriptionTracers {
      nistMuxTracer          :: Tracer IO (WithMuxBearer ConnectionId MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nistHandshakeTracer    :: Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term)
                                                         ConnectionId
                                                         (DecoderFailureOrTooMuchInput DeserialiseFailure)),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      nistErrorPolicyTracer  :: Tracer IO (WithAddr SockAddr ErrorPolicyTrace),
      -- ^ error policy tracer; must not be 'nullTracer', otherwise all the
      -- exceptions which are not matched by any error policy will be caught
      -- and not logged or rethrown.
      nistSubscriptionTracer :: Tracer IO (WithIPList (SubscriptionTrace SockAddr))
      -- ^ subscription tracers; it is infrequent it should not be 'nullTracer'
      -- by default.
    }

nullNetworkIPSubscriptionTracers :: NetworkIPSubscriptionTracers ptcl vNumber
nullNetworkIPSubscriptionTracers = NetworkIPSubscriptionTracers {
      nistMuxTracer          = nullTracer,
      nistHandshakeTracer    = nullTracer,
      nistErrorPolicyTracer  = nullTracer,
      nistSubscriptionTracer = nullTracer
    }

-- | DNS subscription tracers.
--
data NetworkDNSSubscriptionTracers ptcl vNumber peerid = NetworkDNSSubscriptionTracers {
      ndstMuxTracer          :: Tracer IO (WithMuxBearer peerid MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      ndstHandshakeTracer    :: Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term)
                                                       peerid
                                                       (DecoderFailureOrTooMuchInput DeserialiseFailure)),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      ndstErrorPolicyTracer  :: Tracer IO (WithAddr SockAddr ErrorPolicyTrace),
      -- ^ error policy tracer; must not be 'nullTracer', otherwise all the
      -- exceptions which are not matched by any error policy will be caught
      -- and not logged or rethrown.
      ndstSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace SockAddr)),
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
