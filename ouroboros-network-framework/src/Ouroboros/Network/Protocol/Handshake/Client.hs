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
  => VersionDataCodec extra CBOR.Term vNumber
  -> Versions vNumber extra r vData agreedOptions
  -> Peer (Handshake vNumber CBOR.Term)
          AsClient StPropose m
          (Either
            (HandshakeClientProtocolError vNumber)
            r)
handshakeClientPeer VersionDataCodec {encodeData, decodeData, getLocalData} versions =
  -- send known versions
  Yield (ClientAgency TokPropose) (MsgProposeVersions $ encodeVersions encodeData getLocalData versions) $

    Await (ServerAgency TokConfirm) $ \msg -> case msg of

      -- the server refused common highest version
      MsgRefuse vReason ->
        Done TokDone (Left $ HandshakeError vReason)

      -- the server accepted a version, sent back the version number and its
      -- version data blob
      MsgAcceptVersion vNumber vParams ->
        case vNumber `Map.lookup` getVersions versions of
          Nothing -> Done TokDone (Left $ NotRecognisedVersion vNumber)
          --Just (Sigma vData version) ->
          Just version ->
            -- let vData = versionData version in
            case decodeData (versionExtra version) vParams of

              Left err ->
                Done TokDone (Left (HandshakeError $ HandshakeDecodeError vNumber err))

              Right vData' ->
                -- TODO: we should check that we agree on received @vData'@,
                -- this might be less trivial than testing for equality.
                Done TokDone $ Right $ ( runApplication (versionApplication version) undefined)

encodeVersions
  :: forall vNumber extra r vParams vData agreedOptions.
     (forall vData. extra vData -> vData -> vParams)
  -> (forall vData. extra vData -> vNumber -> vData)
  -> Versions vNumber extra r vData agreedOptions
  -> Map vNumber vParams
encodeVersions encoder lData (Versions vs) = Map.mapWithKey go vs
    where
      --go :: Sigma (Version extra r) -> vParams
      --go (Sigma vData Version {versionExtra}) = encoder versionExtra vData
      go vNumber (Version {versionExtra}) = encoder versionExtra (lData versionExtra vNumber) 
