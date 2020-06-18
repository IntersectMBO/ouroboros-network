{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | API for running 'Handshake' protocol.
--
module Ouroboros.Network.Protocol.Handshake
  ( runHandshakeClient
  , runHandshakeServer
  , HandshakeArguments (..)
  , HandshakeException (..)
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import           Control.Tracer (Tracer, contramap)
import qualified Data.ByteString.Lazy as BL
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Term     as CBOR

import           Network.Mux.Timeout
import           Network.Mux.Trace
import           Network.Mux.Types
import           Network.TypedProtocol.Codec

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Limits

import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Client
import           Ouroboros.Network.Protocol.Handshake.Server


-- | The handshake protocol number.
--
handshakeProtocolNum :: MiniProtocolNum
handshakeProtocolNum = MiniProtocolNum 0

-- | Timeout for the complete handshake exchange.
--
handshakeTimeout :: DiffTime
handshakeTimeout = 10 -- 10 seconds

-- | Wrapper around initiator and responder errors experienced by tryHandshake.
--
data HandshakeException a =
    HandshakeProtocolLimit ProtocolLimitFailure
  | HandshakeProtocolError a
  | HandshakeTimeout


-- | Try to complete either initiator or responder side of the Handshake protocol
-- within `handshakeTimeout` seconds.
--
tryHandshake :: ( MonadAsync m
                , MonadFork m
                , MonadMonotonicTime m
                , MonadTimer m
                , MonadMask m
                , MonadThrow (STM m)
                )
             => m (Either a r)
             -> m (Either (HandshakeException a) r)
tryHandshake doHandshake = do
    mapp <- withTimeoutSerial $ \timeoutFn -> timeoutFn handshakeTimeout $ try doHandshake
    case mapp of
         Nothing -> return $ Left HandshakeTimeout
         Just (Left (err :: ProtocolLimitFailure)) ->
             return $ Left $ HandshakeProtocolLimit err
         Just (Right (Left err)) ->
             return $ Left $ HandshakeProtocolError err
         Just (Right (Right r)) -> return $ Right r


--
-- Record arguemnts
--

-- | Common arguments for both 'Handshake' client & server.
--
data HandshakeArguments connectionId vNumber extra m application = HandshakeArguments {
      -- | 'Handshake' tracer
      --
      haHandshakeTracer :: Tracer m (WithMuxBearer connectionId
                                     (TraceSendRecv (Handshake vNumber CBOR.Term))),
      -- | Codec for protocol messages.
      --
      haHandshakeCodec
        ::  Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure m BL.ByteString,

      -- | A codec for protocol parameters.
      --
      haVersionDataCodec
        ::  VersionDataCodec extra CBOR.Term,

      -- | versioned application aggreed upon with the 'Handshake' protocol.
      haVersions :: Versions vNumber extra application
    }


-- | Run client side of the 'Handshake' protocol
--
runHandshakeClient
    :: ( MonadAsync m
       , MonadFork m
       , MonadMonotonicTime m
       , MonadTimer m
       , MonadMask m
       , MonadThrow (STM m)
       , Ord vNumber
       )
    => MuxBearer m
    -> connectionId
    -> HandshakeArguments connectionId vNumber extra m application
    -> m (Either (HandshakeException (HandshakeClientProtocolError vNumber)) application)
runHandshakeClient bearer
                   connectionId
                   HandshakeArguments {
                     haHandshakeTracer,
                     haHandshakeCodec,
                     haVersionDataCodec,
                     haVersions
                   } =
    tryHandshake
      (fst <$>
        runPeerWithLimits
          (WithMuxBearer connectionId `contramap` haHandshakeTracer)
          haHandshakeCodec
          byteLimitsHandshake
          timeLimitsHandshake
          (fromChannel (muxBearerAsChannel bearer handshakeProtocolNum InitiatorDir))
          (handshakeClientPeer haVersionDataCodec haVersions))


-- | Run server side of the 'Handshake' protocol.
--
runHandshakeServer
    :: ( MonadAsync m
       , MonadFork m
       , MonadMonotonicTime m
       , MonadTimer m
       , MonadMask m
       , MonadThrow (STM m)
       , Ord vNumber
       )
    => MuxBearer m
    -> connectionId
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> HandshakeArguments connectionId vNumber extra m application
    -> m (Either (HandshakeException (RefuseReason vNumber)) application)
runHandshakeServer bearer
                   connectionId
                   acceptVersion
                   HandshakeArguments {
                     haHandshakeTracer,
                     haHandshakeCodec,
                     haVersionDataCodec,
                     haVersions
                   } =
    tryHandshake
      (fst <$>
        runPeerWithLimits
          (WithMuxBearer connectionId `contramap` haHandshakeTracer)
          haHandshakeCodec
          byteLimitsHandshake
          timeLimitsHandshake
          (fromChannel (muxBearerAsChannel bearer handshakeProtocolNum ResponderDir))
          (handshakeServerPeer haVersionDataCodec acceptVersion haVersions))
