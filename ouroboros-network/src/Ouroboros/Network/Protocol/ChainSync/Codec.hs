{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
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

import           Data.Singletons
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
    stateToLimit :: forall (st :: ChainSync header point tip).
                    SingPeerHasAgency st -> Word
    stateToLimit (SingClientHasAgency SingIdle)                 = smallByteLimit
    stateToLimit (SingServerHasAgency (SingNext SingCanAwait))  = smallByteLimit
    stateToLimit (SingServerHasAgency (SingNext SingMustReply)) = smallByteLimit
    stateToLimit (SingServerHasAgency SingIntersect)            = smallByteLimit

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

    stateToLimit :: forall (st :: ChainSync header point tip).
                    SingPeerHasAgency st -> Maybe DiffTime
    stateToLimit (SingClientHasAgency SingIdle)                 = waitForever
    stateToLimit (SingServerHasAgency (SingNext SingCanAwait))  = canAwaitTimeout
    stateToLimit (SingServerHasAgency (SingNext SingMustReply)) = mustReplyTimeout
    stateToLimit (SingServerHasAgency SingIntersect)            = intersectTimeout

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
    encode :: forall (st  :: ChainSync header point tip)
                     (st' :: ChainSync header point tip).
              Message (ChainSync header point tip) st st'
           -> CBOR.Encoding

    encode MsgRequestNext =
      encodeListLen 1 <> encodeWord 0

    encode MsgAwaitReply =
      encodeListLen 1 <> encodeWord 1

    encode (MsgRollForward h tip) =
      encodeListLen 3
        <> encodeWord 2
        <> encodeHeader h
        <> encodeTip tip

    encode (MsgRollBackward p tip) =
      encodeListLen 3
        <> encodeWord 3
        <> encodePoint p
        <> encodeTip tip

    encode (MsgFindIntersect ps) =
      encodeListLen 2 <> encodeWord 4 <> encodeList encodePoint ps

    encode (MsgIntersectFound p tip) =
      encodeListLen 3
        <> encodeWord 5
        <> encodePoint p
        <> encodeTip tip

    encode (MsgIntersectNotFound tip) =
      encodeListLen 2
        <> encodeWord 6
        <> encodeTip tip

    encode MsgDone =
      encodeListLen 1 <> encodeWord 7

    decode :: forall (st :: ChainSync header point tip) s.
              SingI (PeerHasAgency st)
           => CBOR.Decoder s (SomeMessage st)
    decode = do
      let stok :: SingPeerHasAgency st
          stok = sing
      len <- decodeListLen
      key <- decodeWord
      case (key, len, stok) of
        (0, 1, SingClientHasAgency SingIdle) ->
          return (SomeMessage MsgRequestNext)

        (1, 1, SingServerHasAgency (SingNext SingCanAwait)) ->
          return (SomeMessage MsgAwaitReply)

        (2, 3, SingServerHasAgency (SingNext SingCanAwait)) -> do
          h <- decodeHeader
          tip <- decodeTip
          return (SomeMessage (MsgRollForward h tip))

        (2, 3, SingServerHasAgency (SingNext SingMustReply)) -> do
          h <- decodeHeader
          tip <- decodeTip
          return (SomeMessage (MsgRollForward h tip))

        (3, 3, SingServerHasAgency (SingNext SingCanAwait)) -> do
          p <- decodePoint
          tip  <- decodeTip
          return (SomeMessage (MsgRollBackward p tip))

        (3, 3, SingServerHasAgency (SingNext SingMustReply)) -> do
          p <- decodePoint
          tip  <- decodeTip
          return (SomeMessage (MsgRollBackward p tip))

        (4, 2, SingClientHasAgency SingIdle) -> do
          ps <- decodeList decodePoint
          return (SomeMessage (MsgFindIntersect ps))

        (5, 3, SingServerHasAgency SingIntersect) -> do
          p <- decodePoint
          tip  <- decodeTip
          return (SomeMessage (MsgIntersectFound p tip))

        (6, 2, SingServerHasAgency SingIntersect) -> do
          tip <- decodeTip
          return (SomeMessage (MsgIntersectNotFound tip))

        (7, 1, SingClientHasAgency SingIdle) ->
          return (SomeMessage MsgDone)

        --
        -- failures per protcol state
        --

        (_, _, SingClientHasAgency SingIdle) ->
          fail (printf "codecChainSync (%s) unexpected key (%d, %d)" (show stok) key len)
        (_, _, SingServerHasAgency (SingNext SingCanAwait)) ->
          fail (printf "codecChainSync (%s) unexpected key (%d, %d)" (show stok) key len)
        (_, _, SingServerHasAgency (SingNext SingMustReply)) ->
          fail (printf "codecChainSync (%s) unexpected key (%d, %d)" (show stok) key len)
        (_, _, SingServerHasAgency SingIntersect) ->
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
  encode :: forall st st'.
            SingI (PeerHasAgency st)
         => Message (ChainSync header point tip) st st'
         -> AnyMessage (ChainSync header point tip)
  encode = AnyMessage

  decode :: forall (st :: ChainSync header point tip).
            SingI (PeerHasAgency st)
         => m (DecodeStep (AnyMessage (ChainSync header point tip))
                          CodecFailure m (SomeMessage st))
  decode = do
    let stok :: SingPeerHasAgency st
        stok = sing
    return $ DecodePartial $ \bytes -> case (stok, bytes) of

      (_, Nothing) -> return $ DecodeFail CodecFailureOutOfInput

      (SingClientHasAgency SingIdle, Just (AnyMessage msg@MsgRequestNext)) -> return (DecodeDone (SomeMessage msg) Nothing)

      (SingServerHasAgency (SingNext SingCanAwait), Just (AnyMessage msg@MsgAwaitReply)) -> return (DecodeDone (SomeMessage msg) Nothing)

      (SingServerHasAgency (SingNext SingCanAwait), Just (AnyMessage (MsgRollForward h tip))) -> return (DecodeDone (SomeMessage (MsgRollForward h tip)) Nothing)

      (SingServerHasAgency (SingNext SingCanAwait), Just (AnyMessage (MsgRollBackward p tip))) -> return (DecodeDone (SomeMessage (MsgRollBackward p tip)) Nothing)

      (SingServerHasAgency (SingNext SingMustReply), Just (AnyMessage (MsgRollForward h tip))) -> return (DecodeDone (SomeMessage (MsgRollForward h tip)) Nothing)

      (SingServerHasAgency (SingNext SingMustReply), Just (AnyMessage (MsgRollBackward p tip))) -> return (DecodeDone (SomeMessage (MsgRollBackward p tip)) Nothing)

      (SingClientHasAgency SingIdle, Just (AnyMessage (MsgFindIntersect ps))) -> return (DecodeDone (SomeMessage (MsgFindIntersect ps)) Nothing)

      (SingServerHasAgency SingIntersect, Just (AnyMessage (MsgIntersectFound p tip))) -> return (DecodeDone (SomeMessage (MsgIntersectFound p tip)) Nothing)

      (SingServerHasAgency SingIntersect, Just (AnyMessage (MsgIntersectNotFound tip))) -> return (DecodeDone (SomeMessage (MsgIntersectNotFound tip)) Nothing)

      (SingClientHasAgency SingIdle, Just (AnyMessage MsgDone)) -> return (DecodeDone (SomeMessage MsgDone) Nothing)

      (_, _) -> return $ DecodeFail (CodecFailure $ "codecChainSync: no matching message")
