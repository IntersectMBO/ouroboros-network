{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
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
  , HandshakeResult (..)
  , RefuseReason (..)
  , Accept (..)
  , handshake_QUERY_SHUTDOWN_DELAY
    -- * Re-exports
  , module Ouroboros.Network.Protocol.Handshake.Type
  , module Ouroboros.Network.Protocol.Handshake.Codec
  , module Ouroboros.Network.Protocol.Handshake.Version
  , Acceptable (..)
  , Queryable (..)
  ) where

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Control.Tracer (Tracer, contramap)
import Data.ByteString.Lazy qualified as BL

import Network.Mux.Trace qualified as Mx
import Network.Mux.Types qualified as Mx
import Network.TypedProtocol.Codec

import Ouroboros.Network.Driver.Limits

import Ouroboros.Network.Protocol.Handshake.Client
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Server
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version


-- | The handshake protocol number.
--
handshakeProtocolNum :: Mx.MiniProtocolNum
handshakeProtocolNum = Mx.MiniProtocolNum 0

-- | Wrapper around initiator and responder errors experienced by tryHandshake.
--
-- TODO: should we have `Exception` instance?
-- It would be handly in `prop_socket_send_recgtv`.
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
-- Record arguments
--

-- | Common arguments for both 'Handshake' client & server.
--
data HandshakeArguments connectionId vNumber vData m = HandshakeArguments {
      -- | 'Handshake' tracer
      --
      haHandshakeTracer
        :: Tracer m (Mx.WithBearer connectionId
                                   (TraceSendRecv (Handshake vNumber CBOR.Term))),

      haBearerTracer
        :: Tracer m (Mx.WithBearer connectionId Mx.BearerTrace),

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

      -- | Whether version data requested a query of support version.
      --
      haQueryVersion :: vData -> Bool,

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
       , MonadTimer m
       , MonadMask m
       , MonadThrow (STM m)
       , Ord vNumber
       )
    => Mx.Bearer m
    -> connectionId
    -> HandshakeArguments connectionId vNumber vData m
    -> Versions vNumber vData application
    -> m (Either (HandshakeException vNumber)
                 (HandshakeResult application vNumber vData))
runHandshakeClient bearer
                   connectionId
                   HandshakeArguments {
                     haHandshakeTracer,
                     haBearerTracer,
                     haHandshakeCodec,
                     haVersionDataCodec,
                     haAcceptVersion,
                     haTimeLimits
                   }
                   versions  =
    tryHandshake
      (fst <$>
        runPeerWithLimits
          (Mx.WithBearer connectionId `contramap` haHandshakeTracer)
          haHandshakeCodec
          byteLimitsHandshake
          haTimeLimits
          (Mx.bearerAsChannel (Mx.WithBearer connectionId `contramap` haBearerTracer)
                              bearer handshakeProtocolNum Mx.InitiatorDir)
          (handshakeClientPeer haVersionDataCodec haAcceptVersion versions))


-- | Run server side of the 'Handshake' protocol.
--
runHandshakeServer
    :: ( MonadAsync m
       , MonadFork m
       , MonadTimer m
       , MonadMask m
       , MonadThrow (STM m)
       , Ord vNumber
       )
    => Mx.Bearer m
    -> connectionId
    -> HandshakeArguments connectionId vNumber vData m
    -> Versions vNumber vData application
    -> m (Either (HandshakeException vNumber)
                 (HandshakeResult application vNumber vData))
runHandshakeServer bearer
                   connectionId
                   HandshakeArguments {
                     haHandshakeTracer,
                     haBearerTracer,
                     haHandshakeCodec,
                     haVersionDataCodec,
                     haAcceptVersion,
                     haQueryVersion,
                     haTimeLimits
                   }
                   versions  =
    tryHandshake
      (fst <$>
        runPeerWithLimits
          (Mx.WithBearer connectionId `contramap` haHandshakeTracer)
          haHandshakeCodec
          byteLimitsHandshake
          haTimeLimits
          (Mx.bearerAsChannel (Mx.WithBearer connectionId `contramap` haBearerTracer)
                              bearer handshakeProtocolNum Mx.ResponderDir)
          (handshakeServerPeer haVersionDataCodec haAcceptVersion haQueryVersion versions))

-- | A 20s delay after query result was send back, before we close the
-- connection.  After that delay we close the connection.
--
handshake_QUERY_SHUTDOWN_DELAY :: DiffTime
handshake_QUERY_SHUTDOWN_DELAY = 20
