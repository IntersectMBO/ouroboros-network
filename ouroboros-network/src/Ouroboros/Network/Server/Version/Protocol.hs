{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}
-- The cost of inventing new names is way higher than the small risk of
-- making a mistake due to a shadow.
{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Server.Version.Protocol where

import qualified Data.Map as Map

import Network.TypedProtocol.Core

import Ouroboros.Network.Server.Version

-- The following is for `cborCodec`, which should be defined in another module.
import Control.Monad.Class.MonadST (MonadST)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Ouroboros.Network.Codec (Codec, SomeMessage (..), mkCodecCborLazyBS)

-- FIXME
-- This is just an MVP draft alpha sort of thing. It does not deal properly
-- with a case in which client and server have a different newest version.
-- What are the options?
-- - Send all of the supported versions. Then the server can definitely say
--   what is the newest common version.
--   This isn't good because as we add more versions we front-load the protocol
--   even though it will almost always be a waste.
-- - Allow the negotiation to carry on in a loop. If the first one is not
--   known to the server, the server can counter-offer, and if the
--   counter-offer is not known to the client, the client can counter-offer.
-- A failure to decode a blob is always an end-of-protocol. It means they
-- agree on the number but have different definitions for that version, which
-- is simply an incompatibility.
-- What we want is a flipping of the same thing:
--
--   Offer >-> Accept
--         >-> Reject
--         >-> Counter-Offer >-> Accept
--                           >-> Reject
--                           >-> <recurse>
--
-- In the best case, the client can exit the version negotiation successfully
-- after 1 round-trip: send the offer, receive the accept. It can then send
-- its accept confirmation and eagerly start on the actual application. The
-- server must wait for the accept. But that's fine. Presumably it would
-- have to wait for the client anyway, as the client/server nomenclature
-- suggests.
--
-- We can also dump the "hopeless" transitions, if we use a `Versions` type
-- which is always non-empty, by putting a "principal" version (newest)
-- alongside a possibly empty map of older versions. When we recurse, we can
-- use `maxView` on the map to get a smaller `Versions` type.

-- | State type is also used as a key on the Protocol typeclass.
-- So even though the `vNumber` parameter has nothing at all to do with the
-- state types, we have to put it here.
data StVersion vNumber vBlob where
  StInit :: StVersion vNumber vBlob
  StOffered :: StVersion vNumber vBlob
  StAccepted :: StVersion vNumber vBlob
  StRejected :: StVersion vNumber vBlob
  StConfirmed :: StVersion vNumber vBlob
  StDenied :: StVersion vNumber vBlob
  StRetrying :: StVersion vNumber vBlob
  StHopeless :: StVersion vNumber vBlob

-- So here's a difficulty: the protocol apparently depends upon the codec.
-- We can overcome it by having, in the offer message, a blob type.

instance Protocol (StVersion vNumber vBlob) where

  data Message (StVersion vNumber vBlob) from to where
    TrClientHopeless :: Message (StVersion vNumber vBlob) StInit StHopeless
    -- Initiator offers this version number and data.
    TrOffer  :: vNumber -> vBlob -> Message (StVersion vNumber vBlob) StInit    StOffered
    TrServerHopeless :: Message (StVersion vNumber vBlob) StOffered StHopeless
    -- Receiver accepts the version and gives its own blob
    TrAccept ::            vBlob -> Message (StVersion vNumber vBlob) StOffered StAccepted
    -- Receiver rejects the version and counter-offers.
    TrReject :: vNumber -> vBlob -> Message (StVersion vNumber vBlob) StOffered StRejected
    -- Initiator confirms the receiver's accept
    TrConfirm :: Message (StVersion vNumber vBlob) StAccepted StConfirmed
    -- Initiator denies the receiver's accept.
    TrDeny    :: Message (StVersion vNumber vBlob) StAccepted StDenied
    -- Initiator accepts the receiver's counter offer. Must give its own
    -- blob for that version
    TrRetry  :: vBlob -> Message (StVersion vNumber vBlob) StRejected StRetrying
    -- Initiator rejects the receiver's counter offer
    TrLeave   :: Message (StVersion vNumber vBlob) StRejected StDenied
    -- Receiver's response to the retry
    TrConfirmRetry :: Message (StVersion vNumber vBlob) StRetrying StConfirmed
    TrDenyRetry    :: Message (StVersion vNumber vBlob) StRetrying StDenied

  -- Initiator states.
  data ClientHasAgency st where
    InitIsClient     :: ClientHasAgency 'StInit
    AcceptedIsClient :: ClientHasAgency 'StAccepted
    RejectedIsClient :: ClientHasAgency 'StRejected

  -- Receiver states.
  data ServerHasAgency st where
    OfferedIsServer    :: ServerHasAgency 'StOffered
    RetryingIsServer :: ServerHasAgency 'StRetrying

  data NobodyHasAgency st where
    ConfirmedIsNobody :: NobodyHasAgency 'StConfirmed
    DeniedIsNobody    :: NobodyHasAgency 'StDenied
    HopelessIsNobody  :: NobodyHasAgency 'StHopeless

  exclusionLemma_ClientAndServerHaveAgency client server = case client of
    InitIsClient -> case server of { }
    AcceptedIsClient -> case server of { }
    RejectedIsClient -> case server of { }

  exclusionLemma_NobodyAndClientHaveAgency nobody client = case client of
    InitIsClient -> case nobody of { }
    AcceptedIsClient -> case nobody of { }
    RejectedIsClient -> case nobody of { }

  exclusionLemma_NobodyAndServerHaveAgency nobody server = case server of
    OfferedIsServer -> case nobody of { }
    RetryingIsServer -> case nobody of { }

-- TODO move everything below to another module.

serverPeerFromVersions
  :: ( Ord vNumber )
  => (forall vData . extra vData -> vData -> vBlob)
  -> (forall vData . extra vData -> vBlob -> Maybe vData)
  -> Versions vNumber extra r
  -> Peer (StVersion vNumber vBlob) 'AsServer 'StInit m (Maybe r)
serverPeerFromVersions writeBlob readBlob versions = Await (ClientAgency InitIsClient) $ \tr -> case tr of
  TrClientHopeless -> Done HopelessIsNobody Nothing
  TrOffer vNum vBlob -> case Map.lookup vNum (getVersions versions) of
    -- Server doesn't have this version number.
    -- Counter-offer if the map is not empty. Otherwise, it's hopeless.
    Nothing -> case Map.toDescList (getVersions versions) of
      [] ->
        Yield (ServerAgency OfferedIsServer) TrServerHopeless (Done HopelessIsNobody Nothing)
      ((bestVersionNumber, Sigma vData version) : _) ->
        Yield (ServerAgency OfferedIsServer) (TrReject bestVersionNumber (writeBlob (versionExtra version) vData)) $
          Await (ClientAgency RejectedIsClient) $ \tr -> case tr of
            TrRetry vBlob -> case readBlob (versionExtra version) vBlob of
              Nothing -> Yield (ServerAgency RetryingIsServer) TrDenyRetry $
                Done DeniedIsNobody Nothing
              Just vData' -> Yield (ServerAgency RetryingIsServer) TrConfirmRetry $
                Done ConfirmedIsNobody $ Just $
                  runApplication (versionApplication version) vData vData'
            TrLeave -> Done DeniedIsNobody Nothing
    -- Server does have this version number.
    -- Send the blob of version data, but only if the blob that was offered
    -- is successfully read.
    Just (Sigma vData version) -> case readBlob (versionExtra version) vBlob of
      Nothing -> case Map.toDescList (getVersions versions) of
        [] ->
          Yield (ServerAgency OfferedIsServer) TrServerHopeless (Done HopelessIsNobody Nothing)
        ((bestVersionNumber, Sigma vData version) : _) ->
          Yield (ServerAgency OfferedIsServer) (TrReject bestVersionNumber (writeBlob (versionExtra version) vData)) $
            Await (ClientAgency RejectedIsClient) $ \tr -> case tr of
              TrRetry vBlob -> case readBlob (versionExtra version) vBlob of
                Nothing -> Yield (ServerAgency RetryingIsServer) TrDenyRetry $
                  Done DeniedIsNobody Nothing
                Just vData' -> Yield (ServerAgency RetryingIsServer) TrConfirmRetry $
                  Done ConfirmedIsNobody $ Just $
                    runApplication (versionApplication version) vData vData'
              TrLeave -> Done DeniedIsNobody Nothing
      Just vData' -> Yield (ServerAgency OfferedIsServer) (TrAccept (writeBlob (versionExtra version) vData)) $
        Await (ClientAgency AcceptedIsClient) $ \tr -> case tr of
          TrDeny -> Done DeniedIsNobody Nothing
          TrConfirm -> Done ConfirmedIsNobody $ Just $
            runApplication (versionApplication version) vData vData'

clientPeerFromVersions
  :: ( Ord vNumber )
  => (forall vData . extra vData -> vData -> vBlob)
  -> (forall vData . extra vData -> vBlob -> Maybe vData)
  -> Versions vNumber extra r
  -> Peer (StVersion vNumber vBlob) 'AsClient 'StInit m (Maybe r)
clientPeerFromVersions writeBlob readBlob versions = case Map.toDescList (getVersions versions) of
  [] ->
    Yield (ClientAgency InitIsClient) TrClientHopeless $ Done HopelessIsNobody Nothing
  ((bestVersionNumber, Sigma vData version) : _) ->
    Yield (ClientAgency InitIsClient) (TrOffer bestVersionNumber (writeBlob (versionExtra version) vData)) $
      Await (ServerAgency OfferedIsServer) $ \tr -> case tr of
        TrServerHopeless -> Done HopelessIsNobody Nothing
        TrAccept vBlob -> case readBlob (versionExtra version) vBlob of
          Nothing -> Yield (ClientAgency AcceptedIsClient) TrDeny $ Done DeniedIsNobody Nothing
          Just vData' -> Yield (ClientAgency AcceptedIsClient) TrConfirm $
            Done ConfirmedIsNobody $ Just $
              runApplication (versionApplication version) vData vData'
        TrReject counterVersionNumber counterVersionBlob ->
          case Map.lookup counterVersionNumber (getVersions versions) of
            Nothing -> Yield (ClientAgency RejectedIsClient) TrLeave $ Done DeniedIsNobody Nothing
            Just (Sigma vData version) -> case readBlob (versionExtra version) counterVersionBlob of
              Nothing -> Yield (ClientAgency RejectedIsClient) TrLeave $ Done DeniedIsNobody Nothing
              Just vData' -> Yield (ClientAgency RejectedIsClient) (TrRetry (writeBlob (versionExtra version) vData)) $
                Await (ServerAgency RetryingIsServer) $ \tr -> case tr of
                  TrDenyRetry -> Done DeniedIsNobody Nothing
                  TrConfirmRetry -> Done ConfirmedIsNobody $ Just $
                    runApplication (versionApplication version) vData vData'

-- One can use
--   vNumber ~ Word32
--   vBlob   ~ ByteString
-- and a CBOR codec for the message type on `StVersion vNumber vBlob` can
-- then use length-prefixing to encode the `ByteString`s. This way, the
-- encoding/decoding of the version-specific data happens entirely within
-- the protocol application itself, rather than mixed up inside the message
-- codec.
-- The protocol given here could be modified to send multiple version numbers
-- and blobs with the "offer" message.

cborCodec
  :: forall vNumber vBlob m .
     ( MonadST m )
  => (vNumber -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s vNumber)
  -> (vBlob -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s vBlob)
  -> Codec (StVersion vNumber vBlob) CBOR.DeserialiseFailure m Lazy.ByteString
cborCodec numEncoder numDecoder blobEncoder blobDecoder = mkCodecCborLazyBS encode decode
  where

  encode
    :: forall (pr :: PeerRole) (st :: StVersion vNumber vBlob) (st' :: StVersion vNumber vBlob) .
       PeerHasAgency pr st
    -> Message (StVersion vNumber vBlob) st st'
    -> CBOR.Encoding
  encode agency = case agency of
    ClientAgency InitIsClient     -> \tr -> case tr of
      TrClientHopeless -> CBOR.encodeWord8 0
      TrOffer vNumber vBlob ->
           CBOR.encodeWord8 1
        <> numEncoder vNumber
        <> blobEncoder vBlob
    ClientAgency AcceptedIsClient -> \tr -> case tr of
      TrDeny    -> CBOR.encodeWord8 0
      TrConfirm -> CBOR.encodeWord8 1
    ClientAgency RejectedIsClient -> \tr -> case tr of
      TrLeave -> CBOR.encodeWord8 0
      TrRetry vBlob -> CBOR.encodeWord8 1 <> blobEncoder vBlob
    ServerAgency OfferedIsServer  -> \tr -> case tr of
      TrServerHopeless -> CBOR.encodeWord8 0
      TrAccept vBlob ->
           CBOR.encodeWord8 1
        <> blobEncoder vBlob
      TrReject vNumber vBlob ->
           CBOR.encodeWord8 2
        <> numEncoder vNumber
        <> blobEncoder vBlob
    ServerAgency RetryingIsServer -> \tr -> case tr of
      TrDenyRetry    -> CBOR.encodeWord8 0
      TrConfirmRetry -> CBOR.encodeWord8 1

  decode
    :: forall (pr :: PeerRole) (st :: StVersion vNumber vBlob) s .
       PeerHasAgency pr st
    -> CBOR.Decoder s (SomeMessage st)
  decode agency = case agency of
    ClientAgency InitIsClient     -> do
      n <- CBOR.decodeWord8
      case n of
        0 -> pure $ SomeMessage $ TrClientHopeless
        1 -> SomeMessage <$> (TrOffer <$> numDecoder <*> blobDecoder)
        _ -> fail "unexpected"
    ClientAgency AcceptedIsClient -> do
      n <- CBOR.decodeWord8
      case n of
        0 -> pure $ SomeMessage $ TrDeny
        1 -> pure $ SomeMessage $ TrConfirm
        _ -> fail "unexpected"
    ClientAgency RejectedIsClient -> do
      n <- CBOR.decodeWord8
      case n of
        0 -> pure $ SomeMessage $ TrLeave
        1 -> SomeMessage . TrRetry <$> blobDecoder
        _ -> fail "unexpected"
    ServerAgency OfferedIsServer  -> do
      n <- CBOR.decodeWord8
      case n of
        0 -> pure $ SomeMessage $ TrServerHopeless
        1 -> SomeMessage . TrAccept <$> blobDecoder
        2 -> SomeMessage <$> (TrReject <$> numDecoder <*> blobDecoder)
        _ -> fail "unexpected"
    ServerAgency RetryingIsServer -> do
      n <- CBOR.decodeWord8
      case n of
        0 -> pure $ SomeMessage $ TrDenyRetry
        1 -> pure $ SomeMessage $ TrConfirmRetry
        _ -> fail "unexpected"
