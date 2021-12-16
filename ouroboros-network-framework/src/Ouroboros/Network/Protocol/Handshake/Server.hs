{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.Handshake.Server (handshakeServerPeer) where

import qualified Codec.CBOR.Term as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.Handshake.Client (acceptOrRefuse)
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version


-- | Server following the handshake protocol; it accepts highest version offered
-- by the peer that also belongs to the server @versions@.
--
handshakeServerPeer
  :: ( Ord vNumber
     )
  => VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Peer (Handshake vNumber CBOR.Term)
          AsServer StPropose m
          (Either (HandshakeProtocolError vNumber) (r, vNumber, vData))
handshakeServerPeer codec@VersionDataCodec {encodeData} acceptVersion versions =
    Await (ClientAgency TokPropose) $ \msg -> case msg of
      MsgProposeVersions vMap  ->
        case acceptOrRefuse codec acceptVersion versions vMap of
          (Right r@(_, vNumber, agreedData)) ->
            Yield (ServerAgency TokConfirm)
                  (MsgAcceptVersion vNumber (encodeData vNumber agreedData))
                  (Done TokDone (Right r))
          (Left vReason) ->
            Yield (ServerAgency TokConfirm)
                  (MsgRefuse vReason)
                  (Done TokDone (Left (HandshakeError vReason)))
