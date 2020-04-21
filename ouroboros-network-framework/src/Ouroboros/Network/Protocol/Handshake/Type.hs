{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Ouroboros.Network.Protocol.Handshake.Type
  (
  -- * Handshake Protocol
    Handshake (..)
  , Message (..)
  , ClientHasAgency (..)
  , ServerHasAgency (..)
  , NobodyHasAgency (..)
  , RefuseReason (..)
  , HandshakeClientProtocolError (..)

  -- ** Handshake client
  , handshakeClientPeer
  -- ** Handshake server
  , handshakeServerPeer

  -- ** Version data codec
  , VersionDataCodec (..)
  , cborTermVersionDataCodec

  -- ** Tests
  , pureHandshake
  )
  where


import           Control.Exception
import           Data.Text (Text)
import           Data.Typeable (Typeable, cast)
import           Data.List (intersect)
import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Codec.CBOR.Term     as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Protocol.Handshake.Version

-- |
-- The handshake mini-protocol is used initially to agree the version and
-- associated parameters of the protocol to use for all subsequent
-- communication.
--
data Handshake vNumber vParams where
    StPropose :: Handshake vNumber vParams
    StConfirm :: Handshake vNumber vParams
    StDone    :: Handshake vNumber vParams

-- |
-- Reasons by which a server can refuse proposed version.
--
data RefuseReason vNumber
  -- | All of the prosed versions where not known to the server.
  -- Since the server sends all versions that it can knows about, some of them
  -- we might not be able to decode, so we include raw tags @[Int]@.
  --
  = VersionMismatch [vNumber] [Int]

  -- | The server failed to decode version parameters.
  --
  | HandshakeDecodeError vNumber Text

  -- | The server refused to run the proposed version parameters
  --
  | Refused vNumber Text
  deriving (Eq, Show)

instance (Typeable vNumber, Show vNumber) => Exception (RefuseReason vNumber)


instance Protocol (Handshake vNumber vParams) where

    data Message (Handshake vNumber vParams) from to where

      -- |
      -- Propose versions together with version parameters.  It must be
      -- encoded to a sorted list.
      --
      MsgProposeVersions
        :: Map vNumber vParams
        -> Message (Handshake vNumber vParams) StPropose StConfirm

      -- |
      -- The remote end decides which version to use and sends chosen version.
      -- The server is allowed to modify version parameters. 
      --
      MsgAcceptVersion
        :: vNumber
        -> vParams
        -> Message (Handshake vNumber vParams) StConfirm StDone

      -- |
      -- or it refuses to run any version,
      --
      MsgRefuse
        :: RefuseReason vNumber
        -> Message (Handshake vNumber vParams) StConfirm StDone

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
    => Show (Message (Handshake vNumber vParams) from to)

instance Show (ClientHasAgency (st :: Handshake vNumber vParams)) where
    show TokPropose = "TokPropose"

instance Show (ServerHasAgency (st :: Handshake vNumber vParams)) where
    show TokConfirm = "TokConfirm"

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
-- Client errors, which extends handshake error @'RefuseReason'@ type,
-- by client specific errors.
--
data HandshakeClientProtocolError vNumber
  = HandshakeError (RefuseReason vNumber)
  | NotRecognisedVersion vNumber
  deriving (Eq, Show)

instance (Typeable vNumber, Show vNumber)
    => Exception (HandshakeClientProtocolError vNumber)

-- | Codec for version data ('vData' in code) exchanged by the handshake
-- protocol.
--
data VersionDataCodec extra bytes = VersionDataCodec {
    encodeData :: forall vData. extra vData -> vData -> bytes,
    -- ^ encoder of 'vData' which has access to 'extra vData' which can bring
    -- extra instances into the scope (by means of pattern matching on a GADT).
    decodeData :: forall vData. extra vData -> bytes -> Either Text vData
    -- ^ decoder of 'vData'.
  }

-- TODO: remove this from top level API, this is the only way we encode or
-- decode version data.
cborTermVersionDataCodec :: VersionDataCodec DictVersion CBOR.Term
cborTermVersionDataCodec = VersionDataCodec {
      encodeData = \(DictVersion codec) -> encodeTerm codec,
      decodeData = \(DictVersion codec) -> decodeTerm codec
    }


-- |
-- Handshake client which offers @'Versions' vNumber vData@ to the
-- remote peer.
--
-- TODO: GADT encoding of the client (@Handshake.Client@ module).
handshakeClientPeer
  :: Ord vNumber
  => VersionDataCodec extra CBOR.Term
  -> Versions vNumber extra r
  -> Peer (Handshake vNumber CBOR.Term) AsClient StPropose m (Either (HandshakeClientProtocolError vNumber) r)
handshakeClientPeer VersionDataCodec {encodeData, decodeData} versions =
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
          Just (Sigma vData version) ->
            case decodeData (versionExtra version) vParams of

              Left err ->
                Done TokDone (Left (HandshakeError $ HandshakeDecodeError vNumber err))

              Right vData' ->
                Done TokDone $ Right $ runApplication (versionApplication version) vData vData'


-- |
-- Server following the handshake protocol; it accepts highest version offered
-- by the peer that also belongs to the server @versions@.
--
-- TODO: GADT encoding of the server (@Handshake.Server@ module).
handshakeServerPeer
  :: Ord vNumber
  => VersionDataCodec extra vParams
  -> (forall vData. extra vData -> vData -> vData -> Accept)
  -> Versions vNumber extra r
  -> Peer (Handshake vNumber vParams) AsServer StPropose m (Either (RefuseReason vNumber) r)
handshakeServerPeer VersionDataCodec {encodeData, decodeData} accVersion versions =
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
                  case accVersion (versionExtra version) vData vData' of

                    -- We agree on the version; send back the agreed version
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
-- Pure computation which serves as a reference implementation of the
-- @'Handshake'@ protocol. Useful for testing
-- @'handshakeClientPeer'@ against @'handshakeServerPeer'@
-- using  @'connect'@
--
pureHandshake
  ::forall vNumber extra r. Ord vNumber
  => (forall vData. extra vData -> Dict Typeable vData)
  -> (forall vData. extra vData -> vData -> vData -> Bool)
  -> Versions vNumber extra r -- reponders's \/ server's known versions
  -> Versions vNumber extra r -- initiator's \/ client's known versions
  -> (Maybe r, Maybe r)
pureHandshake isTypeable acceptVersion (Versions serverVersions) (Versions clientVersions) =

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
