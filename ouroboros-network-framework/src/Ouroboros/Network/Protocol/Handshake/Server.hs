{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.Handshake.Server (handshakeServerPeer) where

import qualified Codec.CBOR.Term as CBOR

import           Network.TypedProtocol.Peer.Server

import           Ouroboros.Network.Protocol.Handshake.Client (acceptOrRefuse,
                     decodeQueryResult, encodeVersions)
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
  -> Server (Handshake vNumber CBOR.Term)
            NonPipelined Empty StPropose m stm
            (Either (HandshakeProtocolError vNumber)
                    (HandshakeResult r vNumber vData))
handshakeServerPeer codec@VersionDataCodec {encodeData, decodeData} acceptVersion query versions =
    Await $ \msg -> case msg of
      MsgProposeVersions vMap  ->
        case acceptOrRefuse codec acceptVersion versions vMap of
          Right (_, _, agreedData) | query agreedData ->
            Yield (MsgQueryReply $ encodeVersions encodeData versions)
                  (Done (Right $ decodeQueryResult decodeData vMap))

          Right (r, vNumber, agreedData) ->
            Yield (MsgAcceptVersion vNumber $ encodeData vNumber agreedData)
                  (Done (Right $ HandshakeNegotiationResult r vNumber agreedData))

          Left vReason ->
            Yield (MsgRefuse vReason)
                  (Done (Left (HandshakeError vReason)))
