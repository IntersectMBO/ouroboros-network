{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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
import           Control.Monad.Class.MonadTime.SI

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.Limits

import qualified Data.ByteString.Lazy as LBS
import           Data.Singletons (withSingI)

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
                    ActiveState st => StateToken st -> Word
    stateToLimit SingIdle                 = smallByteLimit
    stateToLimit (SingNext SingCanAwait)  = smallByteLimit
    stateToLimit (SingNext SingMustReply) = smallByteLimit
    stateToLimit SingIntersect            = smallByteLimit
    stateToLimit a@SingDone               = notActiveState a

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
                    ActiveState st => StateToken st -> Maybe DiffTime
    stateToLimit SingIdle                 = Just 3673
    stateToLimit (SingNext SingCanAwait)  = canAwaitTimeout
    stateToLimit (SingNext SingMustReply) = mustReplyTimeout
    stateToLimit SingIntersect            = intersectTimeout
    stateToLimit a@SingDone               = notActiveState a

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
              ActiveState st
           => StateToken st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- decodeListLen
      key <- decodeWord
      case (key, len, stok) of
        (0, 1, SingIdle) ->
          return (SomeMessage MsgRequestNext)

        (1, 1, SingNext SingCanAwait) ->
          return (SomeMessage MsgAwaitReply)

        (2, 3, SingNext tok) -> withSingI tok $ do
          h <- decodeHeader
          tip <- decodeTip
          return (SomeMessage (MsgRollForward h tip))

        (3, 3, SingNext tok) -> withSingI tok $ do
          p <- decodePoint
          tip  <- decodeTip
          return (SomeMessage (MsgRollBackward p tip))

        (4, 2, SingIdle) -> do
          ps <- decodeList decodePoint
          return (SomeMessage (MsgFindIntersect ps))

        (5, 3, SingIntersect) -> do
          p <- decodePoint
          tip  <- decodeTip
          return (SomeMessage (MsgIntersectFound p tip))

        (6, 2, SingIntersect) -> do
          tip <- decodeTip
          return (SomeMessage (MsgIntersectNotFound tip))

        (7, 1, SingIdle) ->
          return (SomeMessage MsgDone)

        --
        -- failures per protocol state
        --

        (_, _, SingIdle) ->
          fail (printf "codecChainSync (%s, %s) unexpected key (%d, %d)"
                       (show (activeAgency :: ActiveAgency st)) (show stok) key len)
        (_, _, SingNext next) ->
          case next of
            SingCanAwait ->
              fail (printf "codecChainSync (%s) unexpected key (%d, %d)"
                           (show (activeAgency :: ActiveAgency st)) (show stok) key len)
            SingMustReply ->
              fail (printf "codecChainSync (%s) unexpected key (%d, %d)"
                           (show (activeAgency :: ActiveAgency st)) (show stok) key len)
        (_, _, SingIntersect) ->
          fail (printf "codecChainSync (%s) unexpected key (%d, %d)"
                       (show (activeAgency :: ActiveAgency st)) (show stok) key len)
        (_, _, SingDone) -> notActiveState stok

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
            StateTokenI st
         => ActiveState st
         => Message (ChainSync header point tip) st st'
         -> AnyMessage (ChainSync header point tip)
  encode = AnyMessage

  decode :: forall (st :: ChainSync header point tip).
            ActiveState st
         => StateToken st
         -> m (DecodeStep (AnyMessage (ChainSync header point tip))
                          CodecFailure m (SomeMessage st))
  decode stok = do
    return $ DecodePartial $ \bytes -> case (stok, bytes) of

      (_, Nothing) -> return $ DecodeFail CodecFailureOutOfInput

      (SingIdle, Just (AnyMessage msg@MsgRequestNext)) -> return (DecodeDone (SomeMessage msg) Nothing)

      (SingNext SingCanAwait, Just (AnyMessage msg@MsgAwaitReply)) -> return (DecodeDone (SomeMessage msg) Nothing)

      (SingNext SingCanAwait, Just (AnyMessage (MsgRollForward h tip))) -> return (DecodeDone (SomeMessage (MsgRollForward h tip)) Nothing)

      (SingNext SingCanAwait, Just (AnyMessage (MsgRollBackward p tip))) -> return (DecodeDone (SomeMessage (MsgRollBackward p tip)) Nothing)

      (SingNext SingMustReply, Just (AnyMessage (MsgRollForward h tip))) -> return (DecodeDone (SomeMessage (MsgRollForward h tip)) Nothing)

      (SingNext SingMustReply, Just (AnyMessage (MsgRollBackward p tip))) -> return (DecodeDone (SomeMessage (MsgRollBackward p tip)) Nothing)

      (SingIdle, Just (AnyMessage (MsgFindIntersect ps))) -> return (DecodeDone (SomeMessage (MsgFindIntersect ps)) Nothing)

      (SingIntersect, Just (AnyMessage (MsgIntersectFound p tip))) -> return (DecodeDone (SomeMessage (MsgIntersectFound p tip)) Nothing)

      (SingIntersect, Just (AnyMessage (MsgIntersectNotFound tip))) -> return (DecodeDone (SomeMessage (MsgIntersectNotFound tip)) Nothing)

      (SingIdle, Just (AnyMessage MsgDone)) -> return (DecodeDone (SomeMessage MsgDone) Nothing)

      (SingDone, _) -> notActiveState stok

      (_, _) -> return $ DecodeFail (CodecFailure $ "codecChainSync: no matching message")
