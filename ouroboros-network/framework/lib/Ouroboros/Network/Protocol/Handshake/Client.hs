{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.Handshake.Client
  ( handshakeClientPeer
  , handshakeClientPeerWithRTT
  , decodeQueryResult
  , encodeVersions
  , acceptOrRefuse
  ) where

import Control.Monad.Class.MonadTime.SI
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)

import Codec.CBOR.Term qualified as CBOR

import Network.TypedProtocol.Peer.Client

import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version

-- | Handshake client which offers @'Versions' vNumber vData@ to the
-- remote peer.
handshakeClientPeer
  :: ( Monad m
     , Ord vNumber
     )
  => VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Client (Handshake vNumber CBOR.Term)
            NonPipelined StPropose m
            (Either
              (HandshakeProtocolError vNumber)
              (HandshakeResult r vNumber vData)
            )
handshakeClientPeer codec acceptVersion versions =
  fst <$> handshakeClientPeer' nullTimeAPI
                               codec acceptVersion versions

-- | Handshake client which offers @'Versions' vNumber vData@ to the
-- remote peer and computes round trip time.
--
-- TODO: GADT encoding of the client (@Handshake.Client@ module).
--
handshakeClientPeerWithRTT
  :: ( Ord vNumber
     , MonadMonotonicTime m
     )
  => VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Client (Handshake vNumber CBOR.Term)
            NonPipelined StPropose m
            ( Either
                (HandshakeProtocolError vNumber)
                (HandshakeResult r vNumber vData)
            , DiffTime
            )
     -- ^ the client which offers the versions, does the negotiation and
     -- provides round trip time

handshakeClientPeerWithRTT = handshakeClientPeer' monotonicTimeAPI


data TimeAPI time diffTime m = TimeAPI {
    getTime  :: m time,
    timeDiff :: time -> time -> diffTime
 }

monotonicTimeAPI :: MonadMonotonicTime m => TimeAPI Time DiffTime m
monotonicTimeAPI = TimeAPI {
    getTime = getMonotonicTime,
    timeDiff = diffTime
  }

nullTimeAPI :: Applicative m => TimeAPI () () m
nullTimeAPI = TimeAPI {
    getTime  = pure (),
    timeDiff = \() () -> ()
  }

-- | A generic handshake client.
--
handshakeClientPeer'
  :: ( Ord vNumber
     , Monad m
     )
  => TimeAPI time diffTime m
  -> VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Client (Handshake vNumber CBOR.Term)
            NonPipelined StPropose m
            ( Either
                (HandshakeProtocolError vNumber)
                (HandshakeResult r vNumber vData)
            , diffTime
            )
     -- ^ the client which offers the versions, does the negotiation and
     -- provides round trip time
handshakeClientPeer' TimeAPI {getTime, timeDiff}
                     codec@VersionDataCodec {encodeData, decodeData}
                     acceptVersion versions =
  Effect $ do
    start <- getTime
    return $
      -- send known versions
      Yield (MsgProposeVersions $ encodeVersions encodeData versions) $

        Await $ \msg -> case msg of
          MsgReplyVersions vMap -> Effect $ do
            end <- getTime
            -- simultaneous open; 'accept' will choose version (the greatest common
            -- version), and check if we can accept received version data.
            return $ Done $ case acceptOrRefuse codec acceptVersion versions vMap of
              Right (r, vNumber, vData) -> ( Right $ HandshakeNegotiationResult r vNumber vData
                                           , end `timeDiff` start
                                           )
              Left vReason              -> ( Left (HandshakeError vReason)
                                           , end `timeDiff` start
                                           )

          MsgQueryReply vMap -> Effect $ do
            end <- getTime
            return $ Done ( Right $ decodeQueryResult decodeData vMap
                          , end `timeDiff` start
                          )

          -- the server refused common highest version
          MsgRefuse vReason -> Effect $ do
            end <- getTime
            return $ Done ( Left $ HandshakeError vReason
                          , end `timeDiff` start
                          )

          -- the server accepted a version, sent back the version number and its
          -- version data blob
          MsgAcceptVersion vNumber vParams -> Effect $ do
            end <- getTime
            return $ case vNumber `Map.lookup` getVersions versions of
              Nothing -> Done ( Left $ NotRecognisedVersion vNumber
                              , end `timeDiff` start
                              )
              Just (Version app vData) ->
                case decodeData vNumber vParams of

                  Left err ->
                    Done ( Left (HandshakeError $ HandshakeDecodeError vNumber err)
                         , end `timeDiff` start
                         )

                  Right vData' ->
                    case acceptVersion vData vData' of
                      Accept agreedData ->
                        Done ( Right $ HandshakeNegotiationResult (app agreedData)
                                                                  vNumber
                                                                  agreedData
                              , end `timeDiff` start
                              )
                      Refuse err ->
                        Done ( Left (InvalidServerSelection vNumber err)
                             , end `timeDiff` start
                             )


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
