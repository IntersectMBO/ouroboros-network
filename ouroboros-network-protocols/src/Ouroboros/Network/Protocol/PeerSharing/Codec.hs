{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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
    encodeMsg :: Message (PeerSharing peerAddress) st st'
              -> CBOR.Encoding
    encodeMsg (MsgShareRequest amount) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 0
      <> CBOR.encode amount
    encodeMsg (MsgSharePeers peers) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 1
      <> encodeListWith encodeAddress peers
    encodeMsg MsgDone =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 2

    decodeMsg :: forall (st :: PeerSharing peerAddress) s.
                 ActiveState st
              => StateToken st
              -> CBOR.Decoder s (SomeMessage st)
    decodeMsg stok = do
      _ <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, key) of
        (SingIdle, 0) -> SomeMessage . MsgShareRequest
                                  <$> CBOR.decode
        (SingBusy, 1) -> SomeMessage . MsgSharePeers
                                  <$> decodeListWith decodeAddress
        (SingIdle, 2) -> return
                                  $ SomeMessage MsgDone

        (SingIdle, _) ->
          fail ("codecPeerSharing.StIdle: unexpected key: " ++ show key)
        (SingBusy, _) ->
          fail ("codecPeerSharing.StBusy: unexpected key: " ++ show key)

        (a@SingDone, _) -> notActiveState a

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
     encodeMsg :: forall st st'.
                  StateTokenI st
               => ActiveState st
               => Message (PeerSharing peerAddress) st st'
               -> AnyMessage (PeerSharing peerAddress)
     encodeMsg = AnyMessage

     decodeMsg :: forall (st :: PeerSharing peerAddress).
                  ActiveState st
               => StateToken st
               -> m (DecodeStep (AnyMessage (PeerSharing peerAddress))
                          CodecFailure m (SomeMessage st))
     decodeMsg stok = return $ DecodePartial $ \bytes -> return $
       case (stok, bytes) of
         (SingIdle, Just (AnyMessage msg@(MsgShareRequest {})))
             -> DecodeDone (SomeMessage msg) Nothing
         (SingBusy, Just (AnyMessage msg@(MsgSharePeers {})))
             -> DecodeDone (SomeMessage msg) Nothing
         (SingIdle, Just (AnyMessage msg@(MsgDone)))
             -> DecodeDone (SomeMessage msg) Nothing
         (_, _) -> DecodeFail (CodecFailure "codecPeerSharingId: no matching message")

-- | We assume that a TCP segment size of 1440 bytes with initial window of size
-- 4. This sets upper limit of 5760 bytes on each message of peer sharing
-- protocol, which means request and response should be done in a single RTT
--
maxTransmissionUnit :: Word
maxTransmissionUnit = 4 * 1440

byteLimitsPeerSharing :: forall peerAddress bytes.
                         (bytes -> Word)
                      -> ProtocolSizeLimits (PeerSharing peerAddress) bytes
byteLimitsPeerSharing = ProtocolSizeLimits sizeLimitForState
  where
    sizeLimitForState :: forall (st :: PeerSharing peerAddress).
                         ActiveState st
                      => StateToken st -> Word
    sizeLimitForState SingIdle   = maxTransmissionUnit
    sizeLimitForState SingBusy   = maxTransmissionUnit
    sizeLimitForState a@SingDone = notActiveState a


timeLimitsPeerSharing :: forall peerAddress. ProtocolTimeLimits (PeerSharing peerAddress)
timeLimitsPeerSharing = ProtocolTimeLimits { timeLimitForState }
  where
    timeLimitForState :: forall (st :: PeerSharing peerAddress).
                         ActiveState st
                      => StateToken st
                      -> Maybe DiffTime
    timeLimitForState SingIdle   = waitForever
    timeLimitForState SingBusy   = longWait
    timeLimitForState a@SingDone = notActiveState a
