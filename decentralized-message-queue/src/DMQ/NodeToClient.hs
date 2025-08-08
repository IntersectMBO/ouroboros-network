{-# LANGUAGE NamedFieldPuns #-}

module DMQ.NodeToClient
  ( LocalAddress (..)
  , module DMQ.NodeToClient.Version
  , Protocols (..)
  , HandshakeTr
  , ntcHandshakeArguments
  ) where

import Control.Monad.Class.MonadST (MonadST)
import Control.Tracer (Tracer, nullTracer)

import Codec.CBOR.Term qualified as CBOR

import Network.Mux qualified as Mx

import Ouroboros.Network.ConnectionId (ConnectionId)
import Ouroboros.Network.Driver.Simple (TraceSendRecv)
import Ouroboros.Network.Handshake.Acceptable (Acceptable (..))
import Ouroboros.Network.Handshake.Queryable (Queryable (..))
import Ouroboros.Network.Protocol.Handshake (Handshake, HandshakeArguments (..))
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
           codecHandshake, noTimeLimitsHandshake)
import Ouroboros.Network.Snocket (LocalAddress (..))

import DMQ.NodeToClient.Version


data Protocols =
  Protocols {
  }

type HandshakeTr ntcAddr = Mx.WithBearer (ConnectionId ntcAddr) (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))

ntcHandshakeArguments
  :: MonadST m
  => Tracer m (HandshakeTr ntcAddr)
  -> HandshakeArguments
      (ConnectionId ntcAddr)
      NodeToClientVersion
      NodeToClientVersionData
      m
ntcHandshakeArguments tracer =
  HandshakeArguments {
    haHandshakeTracer  = tracer
  , haBearerTracer     = nullTracer -- TODO
  , haHandshakeCodec   = codecHandshake nodeToClientVersionCodec
  , haVersionDataCodec =
      cborTermVersionDataCodec
        nodeToClientCodecCBORTerm
  , haAcceptVersion = acceptableVersion
  , haQueryVersion  = queryVersion
  , haTimeLimits    = noTimeLimitsHandshake
  }
