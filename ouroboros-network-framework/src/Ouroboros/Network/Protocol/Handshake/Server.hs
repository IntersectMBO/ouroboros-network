{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Protocol.Handshake.Server
  ( handshakeServerPeer
  ) where

import qualified Data.Map as Map
import           Data.List (intersect)

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
  :: forall extra vParams vNumber agreedOptions r m. Ord vNumber
  => VersionDataCodec extra vParams vNumber agreedOptions
  -> (forall vData. extra vData -> vNumber -> vData -> vData -> Accept agreedOptions)
  -> Versions vNumber extra r agreedOptions
  -> Peer (Handshake vNumber vParams)
          AsServer StPropose m
          (Either (RefuseReason vNumber) r)
handshakeServerPeer VersionDataCodec {encodeData, decodeData, getAgreedOptions} acceptVersion versions =
    -- await for versions proposed by a client
    Await (ClientAgency TokPropose) $ \msg -> case msg of

      MsgProposeVersions vMap ->
        -- Compute intersection of local and remote versions.  We cannot
        -- intersect @vMap@ and @getVersions versions@ as the values have
        -- different types.
        case map fst (Map.toDescList vMap) `intersect` map fst (Map.toDescList (getVersions versions)) of
          [] ->
            let vReason = VersionMismatch (Map.keys $ getVersions versions) []
            in Yield (ServerAgency TokConfirm)
                     (MsgRefuse vReason)
                     (Done TokDone (Left vReason))

          vNumber:_ ->
            case (getVersions versions Map.! vNumber, vMap Map.! vNumber) of
              (Sigma vData version, vParams) -> case decodeData (versionExtra version) vParams of
                Left err ->
                  let vReason = HandshakeDecodeError vNumber err
                  in Yield (ServerAgency TokConfirm)
                           (MsgRefuse vReason)
                           (Done TokDone $ Left vReason)

                Right vData' ->
                  case acceptVersion (versionExtra version) vNumber undefined vData' of

                    -- We agree on the version; send back the agreed version
                    -- number @vNumber@ and encoded data associated with our
                    -- version.
                    Accept agreedOptions ->
                      Yield (ServerAgency TokConfirm)
                            (MsgAcceptVersion vNumber (encodeData (versionExtra version) vData))
                            (Done TokDone $ Right $
                              (runApplication (versionApplication version) undefined)
                              )

                    -- We disagree on the version.
                    Refuse err ->
                      let vReason = Refused vNumber err
                      in Yield (ServerAgency TokConfirm)
                               (MsgRefuse vReason)
                               (Done TokDone $ Left $ vReason)

