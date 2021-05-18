{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Protocol.Handshake.Server
  ( handshakeServerPeer
  , accept
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version


-- | Server following the handshake protocol; it accepts highest version offered
-- by the peer that also belongs to the server @versions@.
--
-- TODO: GADT encoding of the server (@Handshake.Server@ module).
--
handshakeServerPeer
  :: Ord vNumber
  => VersionDataCodec vParams vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Peer (Handshake vNumber vParams)
          AsServer StPropose m
          (Either (RefuseReason vNumber) (r, vNumber, vData))
handshakeServerPeer codec acceptVersion versions =
    -- await for versions proposed by a client
    Await (ClientAgency TokPropose) $ \msg -> case msg of

      MsgProposeVersions versionMap ->
        accept TokAsServer codec acceptVersion versions versionMap

-- | Accept a proposed version, or refuse any of them.
--
accept
  :: forall (peerRole :: PeerRole) vParams vNumber vData r m.
     Ord vNumber
  => TokPeerRole peerRole
  -> VersionDataCodec vParams vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Map vNumber vParams
  -- ^ proposed versions received either with `MsgProposeVersions` or
  -- `MsgProposeVersions'`
  -> Peer (Handshake vNumber vParams)
          peerRole (StConfirm peerRole) m
          (Either (RefuseReason vNumber) (r, vNumber, vData))
accept role VersionDataCodec {encodeData, decodeData} acceptVersion
       versions versionMap =
    case lookupGreatestCommonKey versionMap (getVersions versions) of
      Nothing ->
        let vReason = VersionMismatch (Map.keys $ getVersions versions) []
        in Yield tok
                 (MsgRefuse vReason)
                 (Done TokDone (Left vReason))

      Just (vNumber, (vParams, Version app vData)) ->
          case decodeData vNumber vParams of
            Left err ->
              let vReason = HandshakeDecodeError vNumber err
              in Yield tok
                       (MsgRefuse vReason)
                       (Done TokDone $ Left vReason)

            Right vData' ->
              case acceptVersion vData vData' of

                -- We agree on the version; send back the agreed version
                -- number @vNumber@ and encoded data associated with our
                -- version.
                Accept agreedData ->
                  Yield tok
                        (MsgAcceptVersion vNumber (encodeData vNumber agreedData))
                        (Done TokDone $ Right $
                          ( app agreedData
                          , vNumber
                          , agreedData
                          ))

                -- We disagree on the version.
                Refuse err ->
                  let vReason = Refused vNumber err
                  in Yield tok
                           (MsgRefuse vReason)
                           (Done TokDone $ Left $ vReason)
  where
    tok = case role of
      TokAsClient -> ClientAgency TokConfirmClient
      TokAsServer -> ServerAgency TokConfirmServer


lookupGreatestCommonKey :: Ord k => Map k a -> Map k b -> Maybe (k, (a, b))
lookupGreatestCommonKey l r = Map.lookupMax $ Map.intersectionWith (,) l r
