{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}

-- | The codec for the local message notification miniprotocol
--
module DMQ.Protocol.LocalMsgNotification.Codec
  ( codecLocalMsgNotification
  , decodeLocalMsgNotification
  ) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad.Class.MonadST
import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.Kind
import Data.List.NonEmpty qualified as NonEmpty
import Text.Printf

import DMQ.Protocol.LocalMsgNotification.Type
import Network.TypedProtocol.Codec.CBOR

codecLocalMsgNotification
  :: forall sig m.
     MonadST m
  => (sig -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s sig)
  -> Codec (LocalMsgNotification sig) CBOR.DeserialiseFailure m ByteString
codecLocalMsgNotification encodeMsg decodeMsg =
  mkCodecCborLazyBS
    encode
    decode
  where
    encode :: forall st st'.
              Message (LocalMsgNotification sig) st st'
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
     <> foldr (\sig r ->
                 encodeMsg sig <> r)
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

    decode :: forall (st :: LocalMsgNotification sig).
              ActiveState st
           => StateToken st
           -> forall s. CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      decodeLocalMsgNotification decodeMsg stok len key


decodeLocalMsgNotification :: forall (sig :: Type) (st' :: LocalMsgNotification sig) s.
                              ActiveState st'
                           => (forall s'. CBOR.Decoder s' sig)
                           -> StateToken st'
                           -> Int
                           -> Word
                           -> CBOR.Decoder s (SomeMessage st')
decodeLocalMsgNotification decodeMsg = decode
  where
    decode :: StateToken st'
           -> Int
           -> Word
           -> CBOR.Decoder s (SomeMessage st')
    decode stok len key = case (stok, len, key) of
      (SingIdle, 1, 2) -> return $ SomeMessage MsgClientDone
      (SingIdle, 2, 0) -> do
        blocking <- CBOR.decodeBool
        return $! if blocking
                    then SomeMessage (MsgRequest SingBlocking)
                    else SomeMessage (MsgRequest SingNonBlocking)
      (SingBusy blocking, 3, 1) -> do
        CBOR.decodeListLenIndef
        msgs <- CBOR.decodeSequenceLenIndef
                  (flip (:)) [] reverse
                  decodeMsg
        more <- CBOR.decodeBool <&> \case
                                      True  -> HasMore
                                      False -> DoesNotHaveMore
        case (blocking, msgs) of
          (SingBlocking, m:msgs') ->
            return . SomeMessage $ MsgReply (BlockingReply (m NonEmpty.:| msgs'))
                                            more
          (SingNonBlocking, _) ->
            return . SomeMessage $ MsgReply (NonBlockingReply msgs)
                                            more
          (SingBlocking, []) -> fail "codecLocalMsgNotification: MsgReply: empty list not permitted"
      (SingDone, _, _) -> notActiveState stok
      --
      -- failure
      --
      (_, _, _) ->
        fail (printf "codecLocalMsgNotification (%s, %s) unexpected key (%d, %d)"
                     (show (activeAgency :: ActiveAgency st')) (show stok) key len)
