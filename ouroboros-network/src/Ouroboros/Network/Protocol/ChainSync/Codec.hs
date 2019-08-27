{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ChainSync.Codec
  ( codecChainSync
  , codecChainSyncId
  ) where

import           Control.Monad (when)
import           Control.Monad.Class.MonadST

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.Cbor

import           Ouroboros.Network.Block (Point)
import           Ouroboros.Network.Protocol.ChainSync.Type

import qualified Data.ByteString.Lazy as LBS

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Write    as CBOR


-- | The main CBOR 'Codec' for the 'ChainSync' protocol.
--
codecChainSync :: forall header tip m.
                  (MonadST m)
               => (header -> CBOR.Encoding)
               -> (forall s . CBOR.Decoder s (LBS.ByteString -> header))
               -> (Point header -> CBOR.Encoding)
               -> (forall s . CBOR.Decoder s (Point header))
               -> (tip -> CBOR.Encoding)
               -> (forall s. CBOR.Decoder s tip)
               -> Codec (ChainSync header tip)
                        CBOR.DeserialiseFailure m LBS.ByteString
codecChainSync encodeHeader decodeHeader
               encodePoint  decodePoint
               encodeTip    decodeTip =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall (pr  :: PeerRole)
                     (st  :: ChainSync header tip)
                     (st' :: ChainSync header tip).
              PeerHasAgency pr st
           -> Message (ChainSync header tip) st st'
           -> CBOR.Encoding

    encode (ClientAgency TokIdle) MsgRequestNext =
      encodeListLen 1 <> encodeWord 0

    encode (ServerAgency TokNext{}) MsgAwaitReply =
      encodeListLen 1 <> encodeWord 1

    encode (ServerAgency TokNext{}) (MsgRollForward h tip) =
      encodeListLen 3
        <> encodeWord 2
        <> encodeHeaderWrapped h
        <> encodeTip tip

    encode (ServerAgency TokNext{}) (MsgRollBackward p tip) =
      encodeListLen 3
        <> encodeWord 3
        <> encodePoint p
        <> encodeTip tip

    encode (ClientAgency TokIdle) (MsgFindIntersect ps) =
      encodeListLen 2 <> encodeWord 4 <> encodeList encodePoint ps

    encode (ServerAgency TokIntersect) (MsgIntersectFound p tip) =
      encodeListLen 3
        <> encodeWord 5
        <> encodePoint p
        <> encodeTip tip

    encode (ServerAgency TokIntersect) (MsgIntersectNotFound tip) =
      encodeListLen 2
        <> encodeWord 6
        <> encodeTip tip

    encode (ClientAgency TokIdle) MsgDone =
      encodeListLen 1 <> encodeWord 7

    decode :: forall (pr :: PeerRole) (st :: ChainSync header tip) s.
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- decodeListLen
      key <- decodeWord
      case (key, len, stok) of
        (0, 1, ClientAgency TokIdle) ->
          return (SomeMessage MsgRequestNext)

        (1, 1, ServerAgency (TokNext TokCanAwait)) ->
          return (SomeMessage MsgAwaitReply)

        (2, 3, ServerAgency (TokNext _)) -> do
          h <- decodeHeaderWrapped
          tip <- decodeTip
          return (SomeMessage (MsgRollForward h tip))

        (3, 3, ServerAgency (TokNext _)) -> do
          p <- decodePoint
          tip  <- decodeTip
          return (SomeMessage (MsgRollBackward p tip))

        (4, 2, ClientAgency TokIdle) -> do
          ps <- decodeList decodePoint
          return (SomeMessage (MsgFindIntersect ps))

        (5, 3, ServerAgency TokIntersect) -> do
          p <- decodePoint
          tip  <- decodeTip
          return (SomeMessage (MsgIntersectFound p tip))

        (6, 2, ServerAgency TokIntersect) -> do
          tip <- decodeTip
          return (SomeMessage (MsgIntersectNotFound tip))

        (7, 1, ClientAgency TokIdle) ->
          return (SomeMessage MsgDone)

        _ -> fail ("codecChainSync: unexpected key " ++ show (key, len))

    encodeHeaderWrapped :: header -> CBOR.Encoding
    encodeHeaderWrapped header =
      --TODO: replace with encodeEmbeddedCBOR from cborg-0.2.4 once
      -- it is available, since that will be faster.
        CBOR.encodeTag 24
     <> CBOR.encodeBytes (CBOR.toStrictByteString (encodeHeader header))

    decodeHeaderWrapped :: forall s. CBOR.Decoder s header
    decodeHeaderWrapped = do
      --TODO: replace this with decodeEmbeddedCBOR from cborg-0.2.4 once
      -- it is available, since that will be faster.
      tag <- CBOR.decodeTag
      when (tag /= 24) $ fail "expected tag 24 (CBOR-in-CBOR)"
      payload <- LBS.fromStrict <$> CBOR.decodeBytes
      case CBOR.deserialiseFromBytes decodeHeader payload of
        Left (CBOR.DeserialiseFailure _ reason) -> fail reason
        Right (trailing, header)
          | not (LBS.null trailing) -> fail "trailing bytes in CBOR-in-CBOR"
          | otherwise               -> return (header payload)


encodeList :: (a -> CBOR.Encoding) -> [a] -> CBOR.Encoding
encodeList _   [] = CBOR.encodeListLen 0
encodeList enc xs = CBOR.encodeListLenIndef
                 <> Prelude.foldr (\x r -> enc x <> r) CBOR.encodeBreak xs

decodeList :: CBOR.Decoder s a -> CBOR.Decoder s [a]
decodeList dec = do
  mn <- CBOR.decodeListLenOrIndef
  case mn of
    Nothing -> CBOR.decodeSequenceLenIndef (flip (:)) [] reverse   dec
    Just n  -> CBOR.decodeSequenceLenN     (flip (:)) [] reverse n dec

-- | An identity 'Codec' for the 'ChainSync' protocol. It does not do any
-- serialisation. It keeps the typed messages, wrapped in 'AnyMessage'.
--
codecChainSyncId :: forall header tip m. Monad m
                 => Codec (ChainSync header tip)
                          CodecFailure m (AnyMessage (ChainSync header tip))
codecChainSyncId = Codec encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (ChainSync header tip) st st'
         -> AnyMessage (ChainSync header tip)
  encode _ = AnyMessage

  decode :: forall (pr :: PeerRole) st.
            PeerHasAgency pr st
         -> m (DecodeStep (AnyMessage (ChainSync header tip))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of

    (_, Nothing) -> return $ DecodeFail CodecFailureOutOfInput

    (ClientAgency TokIdle, Just (AnyMessage msg@MsgRequestNext)) -> return (DecodeDone (SomeMessage msg) Nothing)

    (ServerAgency (TokNext TokCanAwait), Just (AnyMessage msg@MsgAwaitReply)) -> return (DecodeDone (SomeMessage msg) Nothing)

    (ServerAgency (TokNext _), Just (AnyMessage (MsgRollForward h tip))) -> return (DecodeDone (SomeMessage (MsgRollForward h tip)) Nothing)

    (ServerAgency (TokNext _), Just (AnyMessage (MsgRollBackward p tip))) -> return (DecodeDone (SomeMessage (MsgRollBackward p tip)) Nothing)

    (ClientAgency TokIdle, Just (AnyMessage (MsgFindIntersect ps))) -> return (DecodeDone (SomeMessage (MsgFindIntersect ps)) Nothing)

    (ServerAgency TokIntersect, Just (AnyMessage (MsgIntersectFound p tip))) -> return (DecodeDone (SomeMessage (MsgIntersectFound p tip)) Nothing)

    (ServerAgency TokIntersect, Just (AnyMessage (MsgIntersectNotFound tip))) -> return (DecodeDone (SomeMessage (MsgIntersectNotFound tip)) Nothing)

    (ClientAgency TokIdle, Just (AnyMessage MsgDone)) -> return (DecodeDone (SomeMessage MsgDone) Nothing)

    (_, _) -> return $ DecodeFail (CodecFailure "codecChainSync: no matching message")
