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
import           Ouroboros.Network.Codec
import           Ouroboros.Network.Protocol.ChainSync.Type

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Write    as CBOR


-- | The main CBOR 'Codec' for the 'ChainSync' protocol.
--
codecChainSync :: forall header point m.
                  (MonadST m)
               => (header -> CBOR.Encoding)
               -> (forall s . BS.ByteString
                           -> CBOR.Decoder s header)
               -> (point -> CBOR.Encoding)
               -> (forall s . CBOR.Decoder s point)
               -> Codec (ChainSync header point)
                        CBOR.DeserialiseFailure m LBS.ByteString
codecChainSync encodeHeader decodeHeader encodePoint decodePoint =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall (pr  :: PeerRole) 
                     (st  :: ChainSync header point)
                     (st' :: ChainSync header point).
              PeerHasAgency pr st
           -> Message (ChainSync header point) st st'
           -> CBOR.Encoding

    encode (ClientAgency TokIdle) MsgRequestNext =
      encodeListLen 1 <> encodeWord 0

    encode (ServerAgency TokNext{}) MsgAwaitReply =
      encodeListLen 1 <> encodeWord 1

    encode (ServerAgency TokNext{}) (MsgRollForward h p) =
      encodeListLen 3 <> encodeWord 2 <> encodeHeaderWrapped h <> encodePoint p

    encode (ServerAgency TokNext{}) (MsgRollBackward p1 p2) =
      encodeListLen 3 <> encodeWord 3 <> encodePoint p1 <> encodePoint p2

    encode (ClientAgency TokIdle) (MsgFindIntersect ps) =
      encodeListLen 2 <> encodeWord 4 <> encodeList encodePoint ps

    encode (ServerAgency TokIntersect) (MsgIntersectImproved p1 p2) =
      encodeListLen 3 <> encodeWord 5 <> encodePoint p1 <> encodePoint p2

    encode (ServerAgency TokIntersect) (MsgIntersectUnchanged p) =
      encodeListLen 2 <> encodeWord 6 <> encodePoint p

    encode (ClientAgency TokIdle) MsgDone =
      encodeListLen 1 <> encodeWord 7

    decode :: forall (pr :: PeerRole) (st :: ChainSync header point) s.
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
          p <- decodePoint
          return (SomeMessage (MsgRollForward h p))

        (3, 3, ServerAgency (TokNext _)) -> do
          p1 <- decodePoint
          p2 <- decodePoint
          return (SomeMessage (MsgRollBackward p1 p2))

        (4, 2, ClientAgency TokIdle) -> do
          ps <- decodeList decodePoint
          return (SomeMessage (MsgFindIntersect ps))

        (5, 3, ServerAgency TokIntersect) -> do
          p1 <- decodePoint
          p2 <- decodePoint
          return (SomeMessage (MsgIntersectImproved p1 p2))

        (6, 2, ServerAgency TokIntersect) -> do
          p <- decodePoint
          return (SomeMessage (MsgIntersectUnchanged p))

        (7, 1, ClientAgency TokIdle) ->
          return (SomeMessage MsgDone)

        _ -> fail ("codecChainSync: unexpected key " ++ show (key, len))

    encodeHeaderWrapped :: header -> CBOR.Encoding
    encodeHeaderWrapped header =
        CBOR.encodeTag 24
     <> CBOR.encodeBytes (CBOR.toStrictByteString (encodeHeader header))

    decodeHeaderWrapped :: forall s. CBOR.Decoder s header
    decodeHeaderWrapped = do
      tag <- CBOR.decodeTag
      when (tag /= 24) $ fail "expected tag 24 (CBOR-in-CBOR)"
      payload <- CBOR.decodeBytes
      case CBOR.deserialiseFromBytes (decodeHeader payload)
                                     (LBS.fromStrict payload) of
        Left (CBOR.DeserialiseFailure _ reason) -> fail reason
        Right (trailing, header)
          | not (LBS.null trailing) -> fail "trailing bytes in CBOR-in-CBOR"
          | otherwise               -> return header


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
codecChainSyncId :: forall header point m. Monad m
                 => Codec (ChainSync header point)
                          CodecFailure m (AnyMessage (ChainSync header point))
codecChainSyncId = Codec encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (ChainSync header point) st st'
         -> AnyMessage (ChainSync header point)
  encode _ = AnyMessage

  decode :: forall (pr :: PeerRole) st.
            PeerHasAgency pr st
         -> m (DecodeStep (AnyMessage (ChainSync header point))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of

    (_, Nothing) -> return $ DecodeFail CodecFailureOutOfInput

    (ClientAgency TokIdle, Just (AnyMessage msg@MsgRequestNext)) -> return (DecodeDone (SomeMessage msg) Nothing)

    (ServerAgency (TokNext TokCanAwait), Just (AnyMessage msg@MsgAwaitReply)) -> return (DecodeDone (SomeMessage msg) Nothing)

    (ServerAgency (TokNext _), Just (AnyMessage (MsgRollForward h p))) -> return (DecodeDone (SomeMessage (MsgRollForward h p)) Nothing)

    (ServerAgency (TokNext _), Just (AnyMessage (MsgRollBackward p1 p2))) -> return (DecodeDone (SomeMessage (MsgRollBackward p1 p2)) Nothing)

    (ClientAgency TokIdle, Just (AnyMessage (MsgFindIntersect ps))) -> return (DecodeDone (SomeMessage (MsgFindIntersect ps)) Nothing)

    (ServerAgency TokIntersect, Just (AnyMessage (MsgIntersectImproved p1 p2))) -> return (DecodeDone (SomeMessage (MsgIntersectImproved p1 p2)) Nothing)

    (ServerAgency TokIntersect, Just (AnyMessage (MsgIntersectUnchanged p))) -> return (DecodeDone (SomeMessage (MsgIntersectUnchanged p)) Nothing)

    (ClientAgency TokIdle, Just (AnyMessage MsgDone)) -> return (DecodeDone (SomeMessage MsgDone) Nothing)

    (_, _) -> return $ DecodeFail (CodecFailure "codecChainSync: no matching message")
