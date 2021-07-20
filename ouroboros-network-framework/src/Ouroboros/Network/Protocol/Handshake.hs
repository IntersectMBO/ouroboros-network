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
  , Versions (..)
  , HandshakeException (..)
  , HandshakeProtocolError (..)
  , RefuseReason (..)
  , Accept (..)
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

-- | Wrapper around initiator and responder errors experienced by tryHandshake.
--
data HandshakeException vNumber =
    HandshakeProtocolLimit ProtocolLimitFailure
  | HandshakeProtocolError (HandshakeProtocolError vNumber)
  deriving Show


-- | Try to complete either initiator or responder side of the Handshake protocol
-- within `handshakeTimeout` seconds.
--
tryHandshake :: forall m vNumber r.
                ( MonadAsync m
                , MonadMask m
                )
             => m (Either (HandshakeProtocolError vNumber) r)
             -> m (Either (HandshakeException vNumber)     r)
tryHandshake doHandshake = do
    mapp <- try doHandshake
    case mapp of
      Left err ->
          return $ Left $ HandshakeProtocolLimit err
      Right (Left err) ->
          return $ Left $ HandshakeProtocolError err
      Right (Right r) -> return $ Right r


--
-- Record arguemnts
--

-- | Common arguments for both 'Handshake' client & server.
--
data HandshakeArguments connectionId vNumber vData m = HandshakeArguments {
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
        ::  VersionDataCodec CBOR.Term vNumber vData,

      -- | accept version, first argument is our version data the second
      -- argument is the remote version data.
      haAcceptVersion :: vData -> vData -> Accept vData,

      -- | 'Driver' timeouts for 'Handshake' protocol.
      --
      haTimeLimits
        :: ProtocolTimeLimits (Handshake vNumber CBOR.Term)
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
    -> HandshakeArguments connectionId vNumber vData m
    -> Versions vNumber vData application
    -> m (Either (HandshakeException vNumber)
                 (application, vNumber, vData))
runHandshakeClient bearer
                   connectionId
                   HandshakeArguments {
                     haHandshakeTracer,
                     haHandshakeCodec,
                     haVersionDataCodec,
                     haAcceptVersion,
                     haTimeLimits
                   }
                   versions  =
    tryHandshake
      (fst <$>
        runPeerWithLimits
          (WithMuxBearer connectionId `contramap` haHandshakeTracer)
          haHandshakeCodec
          byteLimitsHandshake
          haTimeLimits
          (fromChannel (muxBearerAsChannel bearer handshakeProtocolNum InitiatorDir))
          (handshakeClientPeer haVersionDataCodec haAcceptVersion versions))


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
    -> HandshakeArguments connectionId vNumber vData m
    -> Versions vNumber vData application
    -> m (Either
           (HandshakeException vNumber)
           (application, vNumber, vData))
runHandshakeServer bearer
                   connectionId
                   HandshakeArguments {
                     haHandshakeTracer,
                     haHandshakeCodec,
                     haVersionDataCodec,
                     haAcceptVersion,
                     haTimeLimits
                   }
                   versions  =
    tryHandshake
      (fst <$>
        runPeerWithLimits
          (WithMuxBearer connectionId `contramap` haHandshakeTracer)
          haHandshakeCodec
          byteLimitsHandshake
          haTimeLimits
          (fromChannel (muxBearerAsChannel bearer handshakeProtocolNum ResponderDir))
          (handshakeServerPeer haVersionDataCodec haAcceptVersion versions))
