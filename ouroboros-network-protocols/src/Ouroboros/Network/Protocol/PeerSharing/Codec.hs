{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.PeerSharing.Codec where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)

import qualified Codec.CBOR.Read as CBOR

import           Network.TypedProtocol.Codec.CBOR

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.Serialise.Class as CBOR
import           Ouroboros.Network.Protocol.PeerSharing.Type
                     (ClientHasAgency (..), Message (..), PeerSharing,
                     ServerHasAgency (..))

import           Control.Monad.Class.MonadTime.SI (DiffTime)
import           Ouroboros.Network.Protocol.Limits

codecPeerSharing :: forall m peerAddress.
                    MonadST m
                 => (peerAddress -> CBOR.Encoding)
                 -> (forall s . CBOR.Decoder s peerAddress)
                 -> Codec (PeerSharing peerAddress)
                         CBOR.DeserialiseFailure
                         m
                         ByteString
codecPeerSharing encodeAddress decodeAddress = mkCodecCborLazyBS encodeMsg decodeMsg
  where
    encodeMsg :: PeerHasAgency pr st
              -> Message (PeerSharing peerAddress) st st'
              -> CBOR.Encoding
    encodeMsg (ClientAgency TokIdle) (MsgShareRequest amount) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 0
      <> CBOR.encode amount
    encodeMsg (ServerAgency TokBusy) (MsgSharePeers peers) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 1
      <> encodeListWith encodeAddress peers
    encodeMsg (ClientAgency TokIdle) MsgDone =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 2

    decodeMsg :: PeerHasAgency pr (st :: PeerSharing peerAddress)
              -> CBOR.Decoder s (SomeMessage st)
    decodeMsg stok = do
      _ <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, key) of
        (ClientAgency TokIdle, 0) -> SomeMessage . MsgShareRequest
                                  <$> CBOR.decode
        (ServerAgency TokBusy, 1) -> SomeMessage . MsgSharePeers
                                  <$> decodeListWith decodeAddress
        (ClientAgency TokIdle, 2) -> return
                                  $ SomeMessage MsgDone

        (ClientAgency TokIdle, _) ->
          fail ("codecPeerSharing.StIdle: unexpected key: " ++ show key)
        (ServerAgency TokBusy, _) ->
          fail ("codecPeerSharing.StBusy: unexpected key: " ++ show key)

    -- Definition as in Codec.Serialise.defaultEncodeList but indexed by an
    -- external encoder
    encodeListWith :: (a -> CBOR.Encoding) -> [a] -> CBOR.Encoding
    encodeListWith _   [] = CBOR.encodeListLen 0
    encodeListWith enc xs = CBOR.encodeListLenIndef
                         <> foldr (\x r -> enc x <> r) CBOR.encodeBreak xs

    -- Definition as in Codec.Serialise.defaultDecodeList but indexed by an
    -- external encoder
    decodeListWith :: CBOR.Decoder s a -> CBOR.Decoder s [a]
    decodeListWith dec= do
        mn <- CBOR.decodeListLenOrIndef
        case mn of
          Nothing -> CBOR.decodeSequenceLenIndef (flip (:)) [] reverse   dec
          Just n  -> CBOR.decodeSequenceLenN     (flip (:)) [] reverse n dec

codecPeerSharingId
  :: forall peerAddress m.
     Monad m
  => Codec (PeerSharing peerAddress) CodecFailure m (AnyMessage (PeerSharing peerAddress))
codecPeerSharingId = Codec encodeMsg decodeMsg
   where
     encodeMsg :: forall (pr :: PeerRole) st st'.
                  PeerHasAgency pr st
               -> Message (PeerSharing peerAddress) st st'
               -> AnyMessage (PeerSharing peerAddress)
     encodeMsg _ = AnyMessage

     decodeMsg :: forall (pr :: PeerRole) (st :: (PeerSharing peerAddress)).
                  PeerHasAgency pr st
               -> m (DecodeStep (AnyMessage (PeerSharing peerAddress))
                          CodecFailure m (SomeMessage st))
     decodeMsg stok = return $ DecodePartial $ \bytes -> return $
       case (stok, bytes) of
         (ClientAgency TokIdle, Just (AnyMessage msg@(MsgShareRequest {})))
             -> DecodeDone (SomeMessage msg) Nothing
         (ServerAgency TokBusy, Just (AnyMessage msg@(MsgSharePeers {})))
             -> DecodeDone (SomeMessage msg) Nothing
         (ClientAgency TokIdle, Just (AnyMessage msg@(MsgDone)))
             -> DecodeDone (SomeMessage msg) Nothing
         (_, _) -> DecodeFail (CodecFailure "codecPeerSharingId: no matching message")

-- | We assume that a TCP segment size of 1440 bytes with initial window of size
-- 4. This sets upper limit of 5760 bytes on each message of peer sharing
-- protocol, which means request and response should be done in a single RTT
--
maxTransmissionUnit :: Word
maxTransmissionUnit = 4 * 1440

byteLimitsPeerSharing :: (bytes -> Word)
                      -> ProtocolSizeLimits (PeerSharing peerAddress) bytes
byteLimitsPeerSharing = ProtocolSizeLimits sizeLimitForState
  where
    sizeLimitForState :: PeerHasAgency (pr :: PeerRole)
                                       (st :: PeerSharing peerAddress)
                      -> Word
    sizeLimitForState (ClientAgency TokIdle) = maxTransmissionUnit
    sizeLimitForState (ServerAgency TokBusy) = maxTransmissionUnit


timeLimitsPeerSharing :: ProtocolTimeLimits (PeerSharing peerAddress)
timeLimitsPeerSharing = ProtocolTimeLimits { timeLimitForState }
  where
    timeLimitForState :: PeerHasAgency (pr :: PeerRole)
                                       (st :: PeerSharing peerAddress)
                      -> Maybe DiffTime
    timeLimitForState (ClientAgency TokIdle) = waitForever
    timeLimitForState (ServerAgency TokBusy) = longWait
