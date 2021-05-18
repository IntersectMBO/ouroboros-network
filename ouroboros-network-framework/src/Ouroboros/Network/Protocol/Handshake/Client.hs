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
import           Data.Bifunctor (first)

import qualified Codec.CBOR.Term     as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Server (accept)


-- | Handshake client which offers @'Versions' vNumber vData@ to the
-- remote peer.
--
-- TODO: GADT encoding of the client (@Handshake.Client@ module).
--
handshakeClientPeer
  :: ( Ord vNumber
     , Functor m
     )
  => VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Peer (Handshake vNumber CBOR.Term)
          AsClient StPropose m
          (Either
            (HandshakeClientProtocolError vNumber)
            (r, vNumber, vData))
handshakeClientPeer codec@VersionDataCodec {encodeData, decodeData} acceptVersion versions =
  -- send known versions
  Yield (ClientAgency TokPropose) (MsgProposeVersions $ encodeVersions encodeData versions) $

    Await (ServerAgency TokConfirmServer) $ \msg -> case msg of
      MsgProposeVersions' versionMap -> 
        -- simultanous open; 'accept' will choose version (the greatest common
        -- version), and check if we can accept received version data.
        f <$> accept TokAsClient codec acceptVersion versions versionMap

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
  where
    f :: Either (RefuseReason vNumber)
                (r, vNumber, vData)
      -> Either (HandshakeClientProtocolError vNumber)
                (r, vNumber, vData)
    f = first HandshakeError


encodeVersions
  :: forall vNumber r vParams vData.
     (vNumber -> vData -> vParams)
  -> Versions vNumber vData r
  -> Map vNumber vParams
encodeVersions encoder (Versions vs) = go `Map.mapWithKey` vs
    where
      go :: vNumber -> Version vData r -> vParams
      go vNumber Version {versionData} = encoder vNumber versionData
