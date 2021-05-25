{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Protocol.Handshake.Client
  ( handshakeClientPeer
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Codec.CBOR.Term     as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version


-- | Handshake client which offers @'Versions' vNumber vData@ to the
-- remote peer.
--
-- TODO: GADT encoding of the client (@Handshake.Client@ module).
--
handshakeClientPeer
  :: Ord vNumber
  => VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Peer (Handshake vNumber CBOR.Term)
          AsClient StPropose m
          (Either
            (HandshakeProtocolError vNumber)
            (r, vNumber, vData))
handshakeClientPeer VersionDataCodec {encodeData, decodeData} acceptVersion versions =
  -- send known versions
  Yield (ClientAgency TokPropose) (MsgProposeVersions $ encodeVersions encodeData versions) $

    Await (ServerAgency TokConfirm) $ \msg -> case msg of

      -- the server refused common highest version
      MsgRefuse vReason ->
        Done TokDone (Left $ HandshakeError vReason)

      -- the server accepted a version, sent back the version number and its
      -- version data blob
      MsgAcceptVersion vNumber vParams ->
        case vNumber `Map.lookup` getVersions versions of
          Nothing -> Done TokDone (Left $ NotRecognisedVersion vNumber)
          Just (Version app vData) ->
            case decodeData vNumber vParams of

              Left err ->
                Done TokDone (Left (HandshakeError $ HandshakeDecodeError vNumber err))

              Right vData' ->
                case acceptVersion vData vData' of
                  Accept agreedData ->
                    Done TokDone $ Right $ ( app agreedData
                                           , vNumber
                                           , agreedData
                                           )
                  Refuse err ->
                    Done TokDone (Left (InvalidServerSelection vNumber err))


encodeVersions
  :: forall vNumber r vParams vData.
     (vNumber -> vData -> vParams)
  -> Versions vNumber vData r
  -> Map vNumber vParams
encodeVersions encoder (Versions vs) = go `Map.mapWithKey` vs
    where
      go :: vNumber -> Version vData r -> vParams
      go vNumber Version {versionData} = encoder vNumber versionData
