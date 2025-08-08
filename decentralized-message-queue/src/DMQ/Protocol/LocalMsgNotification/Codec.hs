{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}

-- | The codec for the local message notification miniprotocol
--
module DMQ.Protocol.LocalMsgNotification.Codec (codecLocalMsgNotification) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad.Class.MonadST
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Text.Printf

import Ouroboros.Network.Protocol.Codec.Utils qualified as Utils

import DMQ.Protocol.LocalMsgNotification.Type
import DMQ.Protocol.SigSubmission.Codec qualified as SigSubmission
import DMQ.Protocol.SigSubmission.Type (Sig (..), SigRaw (..))
import Network.TypedProtocol.Codec.CBOR


-- | The codec for the loca message notification miniprotocol instantiated for dmq-node.
--
codecLocalMsgNotification
  :: forall m.
     MonadST m
  => AnnotatedCodec (LocalMsgNotification Sig) CBOR.DeserialiseFailure m ByteString
codecLocalMsgNotification =
    codecLocalMsgNotification' mkSigWithBytes SigSubmission.encodeSig SigSubmission.decodeSig
  where
    mkSigWithBytes
      :: ByteString
      -> Utils.WithByteSpan (ByteString -> SigRaw)
      -> Sig
    mkSigWithBytes bytes (Utils.WithByteSpan (f, start, end)) =
      SigWithBytes {
        sigRawBytes = Utils.bytesBetweenOffsets start end bytes,
        sigRaw = f bytes
      }


-- | A polymorphic annotated codec for the local message notification
-- miniprotocol.  Useful for testing.
--
codecLocalMsgNotification'
  :: forall msg msgWithBytes m.
     MonadST m
  => (ByteString -> Utils.WithByteSpan (ByteString -> msgWithBytes) -> msg)
  -> (msg -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s (ByteString -> msgWithBytes))
  -> AnnotatedCodec (LocalMsgNotification msg) CBOR.DeserialiseFailure m ByteString
codecLocalMsgNotification' mkWithBytes encodeMsg decodeMsgWithBytes =
    mkCodecCborLazyBS
      encode
      decode
  where
    encode :: forall st st'.
              Message (LocalMsgNotification msg) st st'
           -> CBOR.Encoding
    encode (MsgRequest blocking) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> CBOR.encodeBool (case blocking of
                           SingBlocking    -> True
                           SingNonBlocking -> False)

    encode (MsgReply msgs hasMore) =
        CBOR.encodeListLen 3
     <> CBOR.encodeWord 1
     <> CBOR.encodeListLenIndef
     <> foldr (\msg r -> encodeMsg msg <> r)
              CBOR.encodeBreak
              msgs
     <> CBOR.encodeBool hasMore'
     where
       hasMore' = case hasMore of
                    HasMore         -> True
                    DoesNotHaveMore -> False

    encode MsgClientDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 2


    decode :: forall (st :: LocalMsgNotification msg).
              ActiveState st
           => StateToken st
           -> forall s. CBOR.Decoder s (Annotator ByteString st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (SingIdle, 1, 2) -> return (Annotator \_ -> SomeMessage MsgClientDone)

        (SingIdle, 2, 0) -> do
          blocking <- CBOR.decodeBool
          return $! if blocking
                      then Annotator \_ -> SomeMessage (MsgRequest SingBlocking)
                      else Annotator \_ -> SomeMessage (MsgRequest SingNonBlocking)

        (SingBusy blocking, 3, 1) -> do
          CBOR.decodeListLenIndef
          msgs <- CBOR.decodeSequenceLenIndef
                    (flip (:)) [] reverse
                    (Utils.decodeWithByteSpan decodeMsgWithBytes)
          more <- CBOR.decodeBool <&> \case
                                        True  -> HasMore
                                        False -> DoesNotHaveMore
          case (blocking, msgs) of
            (SingBlocking, _:_) ->
              return (Annotator \bytes ->
                       SomeMessage $ MsgReply (BlockingReply (mkWithBytes bytes <$> NonEmpty.fromList msgs))
                                              more)
            (SingNonBlocking, _) ->
              return (Annotator \bytes -> SomeMessage $ MsgReply (NonBlockingReply $ mkWithBytes bytes <$> msgs) more)

            (SingBlocking, []) -> fail "codecLocalMsgNotification: MsgReply: empty list not permitted"

        (SingDone, _, _) -> notActiveState stok

        --
        -- failure
        --
        (_, _, _) ->
          fail (printf "codecLocalMsgNotification (%s, %s) unexpected key (%d, %d)"
                       (show (activeAgency :: ActiveAgency st)) (show stok) key len)
