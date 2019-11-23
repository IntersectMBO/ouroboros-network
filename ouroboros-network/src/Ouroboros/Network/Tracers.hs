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

data NetworkIPSubscriptionTracers ptcl vNumber = NetworkIPSubscriptionTracers {
      nistMuxTracer          :: Tracer IO (WithMuxBearer ConnectionId (MuxTrace ptcl)),
      nistHandshakeTracer    :: Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term)
                                                         ConnectionId
                                                         (DecoderFailureOrTooMuchInput DeserialiseFailure)),
      nistErrorPolicyTracer  :: Tracer IO (WithAddr SockAddr ErrorPolicyTrace),
      nistSubscriptionTracer :: Tracer IO (WithIPList (SubscriptionTrace SockAddr))
    }

nullNetworkIPSubscriptionTracers :: NetworkIPSubscriptionTracers ptcl vNumber
nullNetworkIPSubscriptionTracers = NetworkIPSubscriptionTracers {
      nistMuxTracer          = nullTracer,
      nistHandshakeTracer    = nullTracer,
      nistErrorPolicyTracer  = nullTracer,
      nistSubscriptionTracer = nullTracer
    }

data NetworkDNSSubscriptionTracers ptcl vNumber peerid = NetworkDNSSubscriptionTracers {
      ndstMuxTracer          :: Tracer IO (WithMuxBearer peerid (MuxTrace ptcl)),
      ndstHandshakeTracer    :: Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term)
                                                       peerid
                                                       (DecoderFailureOrTooMuchInput DeserialiseFailure)),
      ndstErrorPolicyTracer  :: Tracer IO (WithAddr SockAddr ErrorPolicyTrace),
      ndstSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace SockAddr)),
      ndstDnsTracer          :: Tracer IO (WithDomainName DnsTrace)
    }

nullNetworkDNSSubscriptionTracers :: NetworkDNSSubscriptionTracers ptcl vNumber peerid
nullNetworkDNSSubscriptionTracers = NetworkDNSSubscriptionTracers {
      ndstMuxTracer          = nullTracer,
      ndstHandshakeTracer    = nullTracer,
      ndstErrorPolicyTracer  = nullTracer,
      ndstSubscriptionTracer = nullTracer,
      ndstDnsTracer          = nullTracer
    }
