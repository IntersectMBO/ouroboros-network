{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ChainSync.Codec
  ( codecChainSync
  , codecChainSyncId
  , byteLimitsChainSync
  , timeLimitsChainSync
  , ChainSyncTimeout (..)
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.Limits

import qualified Data.ByteString.Lazy as LBS

import           Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen, encodeWord)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Text.Printf


-- | Byte Limits
byteLimitsChainSync :: forall bytes header point tip .
                       (bytes -> Word)
                    -> ProtocolSizeLimits (ChainSync header point tip) bytes
byteLimitsChainSync = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: ChainSync header point tip).
                    PeerHasAgency pr st -> Word
    stateToLimit (ClientAgency TokIdle)                = smallByteLimit
    stateToLimit (ServerAgency (TokNext TokCanAwait))  = smallByteLimit
    stateToLimit (ServerAgency (TokNext TokMustReply)) = smallByteLimit
    stateToLimit (ServerAgency TokIntersect)           = smallByteLimit

-- | Configurable timeouts
--
-- These are configurable for at least the following reasons.
--
-- o So that deployment and testing can use different values.
--
-- o So that a net running Praos can better cope with streaks of empty slots.
--   (See @input-output-hk/ouroboros-network#2245@.)
data ChainSyncTimeout = ChainSyncTimeout
  { canAwaitTimeout  :: Maybe DiffTime
  , intersectTimeout :: Maybe DiffTime
  , mustReplyTimeout :: Maybe DiffTime
  }

-- | Time Limits
--
-- > 'TokIdle'               'waitForever' (ie never times out)
-- > 'TokNext TokCanAwait'   the given 'canAwaitTimeout'
-- > 'TokNext TokMustReply'  the given 'mustReplyTimeout'
-- > 'TokIntersect'          the given 'intersectTimeout'
timeLimitsChainSync :: forall header point tip.
                       ChainSyncTimeout
                    -> ProtocolTimeLimits (ChainSync header point tip)
timeLimitsChainSync csTimeouts = ProtocolTimeLimits stateToLimit
  where
    ChainSyncTimeout
      { canAwaitTimeout
      , intersectTimeout
      , mustReplyTimeout
      } = csTimeouts

    stateToLimit :: forall (pr :: PeerRole) (st :: ChainSync header point tip).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokIdle)                = waitForever
    stateToLimit (ServerAgency (TokNext TokCanAwait))  = canAwaitTimeout
    stateToLimit (ServerAgency (TokNext TokMustReply)) = mustReplyTimeout
    stateToLimit (ServerAgency TokIntersect)           = intersectTimeout

-- | Codec for chain sync that encodes/decodes headers
--
-- NOTE: See 'wrapCBORinCBOR' and 'unwrapCBORinCBOR' if you want to use this
-- with a header type that has annotations.
codecChainSync
  :: forall header point tip m.
     (MonadST m)
  => (header -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s header)
  -> (point -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s point)
  -> (tip -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s tip)
  -> Codec (ChainSync header point tip)
           CBOR.DeserialiseFailure m LBS.ByteString
codecChainSync encodeHeader decodeHeader
               encodePoint  decodePoint
               encodeTip    decodeTip =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall (pr  :: PeerRole)
                     (st  :: ChainSync header point tip)
                     (st' :: ChainSync header point tip).
              PeerHasAgency pr st
           -> Message (ChainSync header point tip) st st'
           -> CBOR.Encoding

    encode (ClientAgency TokIdle) MsgRequestNext =
      encodeListLen 1 <> encodeWord 0

    encode (ServerAgency TokNext{}) MsgAwaitReply =
      encodeListLen 1 <> encodeWord 1

    encode (ServerAgency TokNext{}) (MsgRollForward h tip) =
      encodeListLen 3
        <> encodeWord 2
        <> encodeHeader h
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

    decode :: forall (pr :: PeerRole) (st :: ChainSync header point tip) s.
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
          h <- decodeHeader
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

        --
        -- failures per protcol state
        --

        (_, _, ClientAgency TokIdle) ->
          fail (printf "codecChainSync (%s) unexpected key (%d, %d)" (show stok) key len)
        (_, _, ServerAgency (TokNext TokCanAwait)) ->
          fail (printf "codecChainSync (%s) unexpected key (%d, %d)" (show stok) key len)
        (_, _, ServerAgency (TokNext TokMustReply)) ->
          fail (printf "codecChainSync (%s) unexpected key (%d, %d)" (show stok) key len)
        (_, _, ServerAgency TokIntersect) ->
          fail (printf "codecChainSync (%s) unexpected key (%d, %d)" (show stok) key len)

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
codecChainSyncId :: forall header point tip m. Monad m
                 => Codec (ChainSync header point tip)
                          CodecFailure m (AnyMessage (ChainSync header point tip))
codecChainSyncId = Codec encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (ChainSync header point tip) st st'
         -> AnyMessage (ChainSync header point tip)
  encode _ = AnyMessage

  decode :: forall (pr :: PeerRole) (st :: ChainSync header point tip).
            PeerHasAgency pr st
         -> m (DecodeStep (AnyMessage (ChainSync header point tip))
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
