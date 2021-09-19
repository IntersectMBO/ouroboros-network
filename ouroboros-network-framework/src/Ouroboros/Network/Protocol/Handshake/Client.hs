{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.Handshake.Client
  ( handshakeClientPeer
  , decodeQueryResult
  , encodeVersions
  , acceptOrRefuse
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import qualified Codec.CBOR.Term as CBOR

import           Network.TypedProtocol.Peer.Client

import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version


-- | Handshake client which offers @'Versions' vNumber vData@ to the
-- remote peer.
--
-- TODO: GADT encoding of the client (@Handshake.Client@ module).
--
handshakeClientPeer
  :: ( Ord vNumber
     )
  => VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Client (Handshake vNumber CBOR.Term)
            NonPipelined Empty StPropose m stm
            (Either
              (HandshakeProtocolError vNumber)
              (HandshakeResult r vNumber vData))
handshakeClientPeer codec@VersionDataCodec {encodeData, decodeData}
                    acceptVersion versions =
  -- send known versions
  Yield (MsgProposeVersions $ encodeVersions encodeData versions) $

    Await $ \msg -> case msg of
      MsgReplyVersions vMap ->
        -- simultaneous open; 'accept' will choose version (the greatest common
        -- version), and check if we can accept received version data.
        Done $ case acceptOrRefuse codec acceptVersion versions vMap of
          Right (r, vNumber, vData) -> Right $ HandshakeNegotiationResult r vNumber vData
          Left vReason              -> Left (HandshakeError vReason)

      MsgQueryReply vMap ->
        Done $ Right $ decodeQueryResult decodeData vMap

      -- the server refused common highest version
      MsgRefuse vReason ->
        Done (Left $ HandshakeError vReason)

      -- the server accepted a version, sent back the version number and its
      -- version data blob
      MsgAcceptVersion vNumber vParams ->
        case vNumber `Map.lookup` getVersions versions of
          Nothing -> Done (Left $ NotRecognisedVersion vNumber)
          Just (Version app vData) ->
            case decodeData vNumber vParams of

              Left err ->
                Done (Left (HandshakeError $ HandshakeDecodeError vNumber err))

              Right vData' ->
                case acceptVersion vData vData' of
                  Accept agreedData ->
                    Done $ Right $ HandshakeNegotiationResult (app agreedData)
                                                              vNumber
                                                              agreedData
                  Refuse err ->
                    Done (Left (InvalidServerSelection vNumber err))


decodeQueryResult :: (vNumber -> bytes -> Either Text vData)
                  -> Map vNumber bytes
                  -> HandshakeResult r vNumber vData
decodeQueryResult decodeData vMap = HandshakeQueryResult $ Map.mapWithKey decodeData vMap

encodeVersions
  :: forall vNumber r vParams vData.
     (vNumber -> vData -> vParams)
  -> Versions vNumber vData r
  -> Map vNumber vParams
encodeVersions encoder (Versions vs) = go `Map.mapWithKey` vs
    where
      go :: vNumber -> Version vData r -> vParams
      go vNumber Version {versionData} = encoder vNumber versionData


acceptOrRefuse
  :: forall vParams vNumber vData r.
     Ord vNumber
  => VersionDataCodec vParams vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Map vNumber vParams
  -- ^ proposed versions received either with `MsgProposeVersions` or
  -- `MsgReplyVersions`
  -> Either (RefuseReason vNumber) (r, vNumber, vData)
acceptOrRefuse VersionDataCodec {decodeData}
               acceptVersion versions versionMap =
    case lookupGreatestCommonKey versionMap (getVersions versions) of
      Nothing ->
        Left $ VersionMismatch (Map.keys $ getVersions versions) []

      Just (vNumber, (vParams, Version app vData)) ->
        case decodeData vNumber vParams of
          Left err ->
            Left (HandshakeDecodeError vNumber err)

          Right vData' ->
            case acceptVersion vData vData' of
              Accept agreedData ->
                Right (app agreedData, vNumber, agreedData)

              Refuse err ->
                Left (Refused vNumber err)


lookupGreatestCommonKey :: Ord k => Map k a -> Map k b -> Maybe (k, (a, b))
lookupGreatestCommonKey l r = Map.lookupMax $ Map.intersectionWith (,) l r
