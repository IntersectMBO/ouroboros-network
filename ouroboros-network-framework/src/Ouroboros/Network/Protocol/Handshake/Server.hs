{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.Handshake.Server (handshakeServerPeer) where

import qualified Codec.CBOR.Term as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.Handshake.Client (acceptOrRefuse, encodeVersions)
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
  -> (vData -> Bool)
  -> Versions vNumber vData r
  -> Peer (Handshake vNumber CBOR.Term)
          AsServer StPropose m
          (Either (HandshakeProtocolError vNumber) (r, vNumber, vData))
handshakeServerPeer codec@VersionDataCodec {encodeData} acceptVersion query versions =
    Await (ClientAgency TokPropose) $ \msg -> case msg of
      MsgProposeVersions vMap  ->
        case acceptOrRefuse codec acceptVersion versions vMap of
          (Right r@(_, vNumber, agreedData)) ->
            let response = if query agreedData then
                    (MsgReplyVersions $ encodeVersions encodeData versions)
                  else
                    (MsgAcceptVersion vNumber (encodeData vNumber agreedData))
            in
            Yield (ServerAgency TokConfirm)
                  response
                  (Done TokDone (Right r))
          (Left vReason) ->
            Yield (ServerAgency TokConfirm)
                  (MsgRefuse vReason)
                  (Done TokDone (Left (HandshakeError vReason)))
