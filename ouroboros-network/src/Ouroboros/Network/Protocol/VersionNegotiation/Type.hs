{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Ouroboros.Network.Protocol.VersionNegotiation.Type
  (
  -- * Version Negotiation Protocol
    VersionNegotiationProtocol (..)
  , Message (..)
  , ClientHasAgency (..)
  , ServerHasAgency (..)
  , NobodyHasAgency (..)
  , RefuseReason (..)

  -- ** Version Negotiation client
  , versionNegotiationClientPeer
  -- ** Version Negotiation server
  , versionNegotiationServerPeer
  , Accept (..)

  -- ** Tests
  , pureVersionNegotiation
  )
  where


import           Data.Text (Text)
import           Data.Typeable (Typeable, cast)
import           Data.List (intersect)
import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term     as CBOR
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise     as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Server.Version

-- |
-- State transitions of the version negotation protocol.
--
data VersionNegotiationProtocol vNumber vParams where
    StPropose :: VersionNegotiationProtocol vNumber vParams
    StConfirm :: VersionNegotiationProtocol vNumber vParams
    StDone    :: VersionNegotiationProtocol vNumber vParams

-- |
-- Reasons by which a server can refuse proposed version.
--
data RefuseReason vNumber
  -- |
  -- All of the prosed versions where not known to the server.
  = VersionMismatch [vNumber]
  -- |
  -- The server failed to decode version parameters.
  | VersionNegotiationDecodeError vNumber Text
  -- |
  -- The server refused to run the proposed version parameters
  | Refused vNumber Text
  deriving (Eq, Show)

instance Serialise vNumber => Serialise (RefuseReason vNumber) where
    encode (VersionMismatch vs) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 0
      <> CBOR.encode vs
    encode (VersionNegotiationDecodeError vNumber vError) =
         CBOR.encodeListLen 3
      <> CBOR.encodeWord 1
      <> CBOR.encode vNumber
      <> CBOR.encodeString vError
    encode (Refused vNumber vReason) =
         CBOR.encodeListLen 3
      <> CBOR.encodeWord 2
      <> CBOR.encode vNumber
      <> CBOR.encodeString vReason

    decode = do
      _ <- CBOR.decodeListLen
      tag <- CBOR.decodeWord
      case tag of
        0 -> VersionMismatch <$> CBOR.decode
        1 -> VersionNegotiationDecodeError <$> CBOR.decode <*> CBOR.decodeString
        2 -> Refused <$> CBOR.decode <*> CBOR.decodeString
        _ -> fail $ "decode RefuseReason: unknown tag " ++ show tag


instance Protocol (VersionNegotiationProtocol vNumber vParams) where

    data Message (VersionNegotiationProtocol vNumber vParams) from to where

      -- |
      -- Propose versions together with version parameters.  It must be
      -- encoded to a sorted list.
      --
      MsgProposeVersions
        :: Map vNumber vParams
        -> Message (VersionNegotiationProtocol vNumber vParams) StPropose StConfirm

      -- |
      -- The remote end decides which version to use and sends choosen version.
      -- The server is allowed to modify version parameters. 
      --
      MsgAcceptVersion
        :: vNumber
        -> vParams
        -> Message (VersionNegotiationProtocol vNumber vParams) StConfirm StDone

      -- |
      -- or it refuses to run any version,
      --
      MsgRefuse
        :: RefuseReason vNumber
        -> Message (VersionNegotiationProtocol vNumber vParams) StConfirm StDone

    data ClientHasAgency st where
      TokPropose :: ClientHasAgency StPropose

    data ServerHasAgency st where
      TokConfirm :: ServerHasAgency StConfirm

    data NobodyHasAgency st where
      TokDone    :: NobodyHasAgency StDone

    exclusionLemma_ClientAndServerHaveAgency TokPropose tok = case tok of {}
    exclusionLemma_NobodyAndClientHaveAgency TokDone    tok = case tok of {}
    exclusionLemma_NobodyAndServerHaveAgency TokDone    tok = case tok of {}

deriving instance (Show vNumber, Show vParams)
    => Show (Message (VersionNegotiationProtocol vNumber vParams) from to)

encodeVersions
  :: forall vNumber extra r vParams.
     (forall vData. extra vData -> vData -> vParams)
  -> Versions vNumber extra r
  -> Map vNumber vParams
encodeVersions encoder (Versions vs) = go <$> vs
    where
      go :: Sigma (Version extra r) -> vParams
      go (Sigma vData Version {versionExtra}) = encoder versionExtra vData

-- |
-- Client errors, which extends version negotation error @'RefuseReason'@ type,
-- by client specific errors.
--
data ClientProtocolError vNumber
  = VersionNegotiationError (RefuseReason vNumber)
  | NotRecognisedVersion vNumber
  deriving (Eq, Show)

-- |
-- Version negotiation client which offers @'KnownVersions' vNumber r@ to the
-- remote peer.
--
-- TODO: GADT encoding of the client (@VersionNegotiation.Client@ module).
versionNegotiationClientPeer
  :: Ord vNumber
  => (forall vData. extra vData -> vData -> CBOR.Term)
  -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
  -> Versions vNumber extra r
  -> Peer (VersionNegotiationProtocol vNumber CBOR.Term) AsClient StPropose m (Either (ClientProtocolError vNumber) r)
versionNegotiationClientPeer encodeData decodeData versions =
  -- send known versions
  Yield (ClientAgency TokPropose) (MsgProposeVersions $ encodeVersions encodeData versions) $

    Await (ServerAgency TokConfirm) $ \msg -> case msg of

      -- the server refused common highest version
      MsgRefuse vReason ->
        Done TokDone (Left $ VersionNegotiationError vReason)

      -- the server accepted a version, sent back the version number and its
      -- version data blob
      MsgAcceptVersion vNumber vParams ->
        case vNumber `Map.lookup` getVersions versions of
          Nothing -> Done TokDone (Left $ NotRecognisedVersion vNumber)
          Just (Sigma vData version) ->
            case decodeData (versionExtra version) vParams of

              Left err ->
                Done TokDone (Left (VersionNegotiationError $ VersionNegotiationDecodeError vNumber err))

              Right vData' ->
                Done TokDone $ Right $ runApplication (versionApplication version) vData vData'

-- |
-- A @'Maybe'@ like type which better explains its purpose.
--
data Accept
  = Accept
  | Refuse Text
  deriving (Eq, Show)

-- |
-- Version negotiation server which accepts highest version offered by the peer
-- that also belongs to @knownVersion@.
--
-- TODO: GADT encoding of the server (@VersionNegotiation.Server@ module).
versionNegotiationServerPeer
  :: Ord vNumber
  => (forall vData. extra vData -> vData -> vParams)
  -> (forall vData. extra vData -> vParams -> Either Text vData)
  -> (forall vData. extra vData -> vData -> vData -> Accept)
  -> Versions vNumber extra r
  -> Peer (VersionNegotiationProtocol vNumber vParams) AsServer StPropose m (Either (RefuseReason vNumber) r)
versionNegotiationServerPeer encodeData decodeData acceptVersion versions =
    -- await for versions proposed by a client
    Await (ClientAgency TokPropose) $ \msg -> case msg of

      MsgProposeVersions vMap ->
        -- Compute intersection of local and remote versions.  We cannot
        -- intersect @vMap@ and @getVersions versions@ as the values have
        -- different types.
        case map fst (Map.toDescList vMap) `intersect` map fst (Map.toDescList (getVersions versions)) of
          [] ->
            let vReason = VersionMismatch $ Map.keys $ getVersions versions
            in Yield (ServerAgency TokConfirm)
                     (MsgRefuse vReason)
                     (Done TokDone (Left vReason))

          vNumber:_ ->
            case (getVersions versions Map.! vNumber, vMap Map.! vNumber) of
              (Sigma vData version, vParams) -> case decodeData (versionExtra version) vParams of
                Left err ->
                  let vReason = VersionNegotiationDecodeError vNumber err
                  in Yield (ServerAgency TokConfirm)
                           (MsgRefuse vReason)
                           (Done TokDone $ Left vReason)

                Right vData' ->
                  case acceptVersion (versionExtra version) vData vData' of

                    -- We agree on the version; send back the aggreed version
                    -- number @vNumber@ and encoded data associated with our
                    -- version.
                    Accept ->
                      Yield (ServerAgency TokConfirm)
                            (MsgAcceptVersion vNumber (encodeData (versionExtra version) vData))
                            (Done TokDone $ Right $ runApplication (versionApplication version) vData vData')

                    -- We disagree on the version.
                    Refuse err ->
                      let vReason = Refused vNumber err
                      in Yield (ServerAgency TokConfirm)
                               (MsgRefuse vReason)
                               (Done TokDone $ Left $ vReason)

-- |
-- Pure computation which serves as a reference impementation of the
-- @'VersionNegotiationProtocol'@ protocol. Useful for testing
-- @'versionNegotiationClientPeer'@ against @'versionNegotiationServerPeer'@
-- using  @'connect'@
--
pureVersionNegotiation
  ::forall vNumber extra r. Ord vNumber
  => (forall vData. extra vData -> Dict Typeable vData)
  -> (forall vData. extra vData -> vData -> vData -> Bool)
  -> Versions vNumber extra r -- reponders's \/ server's known versions
  -> Versions vNumber extra r -- initiator's \/ client's known versions
  -> (Maybe r, Maybe r)
pureVersionNegotiation isTypeable acceptVersion (Versions serverVersions) (Versions clientVersions) =

      case Map.toDescList $ serverVersions `Map.intersection` clientVersions of

        []             -> (Nothing, Nothing)

        (vNumber, _):_ -> 
          case (serverVersions Map.! vNumber, clientVersions Map.! vNumber) of

            ( Sigma vData version, Sigma vData' version' ) ->
                  case (isTypeable (versionExtra version), isTypeable (versionExtra version')) of
                        (Dict, Dict) -> case (cast vData, cast vData') of
                          (Just d, Just d') ->
                            ( if acceptVersion (versionExtra version) vData d'
                                then Just $ runApplication (versionApplication version) vData d'
                                else Nothing

                            , Just $ runApplication (versionApplication version') vData' d
                            )
                          _ -> (Nothing, Nothing)
