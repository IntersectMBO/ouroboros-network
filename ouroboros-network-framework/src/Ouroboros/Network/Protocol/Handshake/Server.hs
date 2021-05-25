{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Protocol.Handshake.Server
  ( handshakeServerPeer
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
          (Either (HandshakeProtocolError vNumber) (r, vNumber, vData))
handshakeServerPeer VersionDataCodec {encodeData, decodeData} acceptVersion versions =
    -- await for versions proposed by a client
    Await (ClientAgency TokPropose) $ \msg -> case msg of

      MsgProposeVersions vMap ->
        -- Compute intersection of local and remote versions.
        case lookupGreatestCommonKey vMap (getVersions versions) of
          Nothing ->
            let vReason = VersionMismatch (Map.keys $ getVersions versions) []
            in Yield (ServerAgency TokConfirm)
                     (MsgRefuse vReason)
                     (Done TokDone (Left $ HandshakeError vReason))

          Just (vNumber, (vParams, Version app vData)) ->
              case decodeData vNumber vParams of
                Left err ->
                  let vReason = HandshakeDecodeError vNumber err
                  in Yield (ServerAgency TokConfirm)
                           (MsgRefuse vReason)
                           (Done TokDone $ Left $ HandshakeError vReason)

                Right vData' ->
                  case acceptVersion vData vData' of

                    -- We agree on the version; send back the agreed version
                    -- number @vNumber@ and encoded data associated with our
                    -- version.
                    Accept agreedData ->
                      Yield (ServerAgency TokConfirm)
                            (MsgAcceptVersion vNumber (encodeData vNumber agreedData))
                            (Done TokDone $ Right $
                              ( app agreedData
                              , vNumber
                              , agreedData
                              ))

                    -- We disagree on the version.
                    Refuse err ->
                      let vReason = Refused vNumber err
                      in Yield (ServerAgency TokConfirm)
                               (MsgRefuse vReason)
                               (Done TokDone $ Left $ HandshakeError vReason)

lookupGreatestCommonKey :: Ord k => Map k a -> Map k b -> Maybe (k, (a, b))
lookupGreatestCommonKey l r = Map.lookupMax $ Map.intersectionWith (,) l r
