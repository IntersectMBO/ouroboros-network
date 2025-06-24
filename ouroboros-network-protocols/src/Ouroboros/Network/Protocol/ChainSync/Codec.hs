{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ChainSync.Codec
  ( codecChainSync
  , codecChainSyncId
  , byteLimitsChainSync
  , timeLimitsChainSync
  , maxChainSyncTimeout
  , minChainSyncTimeout
  ) where

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI

import Network.TypedProtocol.Codec.CBOR hiding (decode, encode)

import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.Limits

import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Singletons (withSingI)
import System.Random (StdGen, randomR)

import Codec.CBOR.Decoding (decodeListLen, decodeWord)
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding (encodeListLen, encodeWord)
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Text.Printf


-- | Byte Limits
byteLimitsChainSync :: forall bytes (header :: Type) (point :: Type) (tip :: Type) .
                       (bytes -> Word) -- ^ compute size of `bytes`
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


-- | Chain sync `mustReplayTimeout` lower bound.
--
minChainSyncTimeout :: DiffTime
minChainSyncTimeout = 135


-- | Chain sync `mustReplayTimeout` upper bound.
--
maxChainSyncTimeout :: DiffTime
maxChainSyncTimeout = 269


-- | Time Limits
--
-- +----------------------------+-------------------------------------------------------------+
-- | ChainSync State            | timeout (s)                                                 |
-- +============================+=============================================================+
-- | @'StIdle'@                 | 'waitForever' (i.e. never times out)                        |
-- +----------------------------+-------------------------------------------------------------+
-- | @'StNext' 'StCanAwait'@    | 'shortWait'                                                 |
-- +----------------------------+-------------------------------------------------------------+
-- | @'StNext' 'StMustReply'@   | randomly picked using uniform distribution from             |
-- |                            | the range @('minChainSyncTimeout', 'maxChainSyncTimeout')@, |
-- |                            | which corresponds to a chance of an empty streak of slots   |
-- |                            | between `0.0001%` and `1%` probability.                     |
-- +----------------------------+-------------------------------------------------------------+
-- | @'StIntersect'@            | 'shortWait'                                                 |
-- +----------------------------+-------------------------------------------------------------+
--
timeLimitsChainSync :: forall (header :: Type) (point :: Type) (tip :: Type).
                       ProtocolTimeLimitsWithRnd (ChainSync header point tip)
timeLimitsChainSync = ProtocolTimeLimitsWithRnd stateToLimit
  where
    stateToLimit :: forall (st :: ChainSync header point tip).
                    ActiveState st
                 => StateToken st -> StdGen -> (Maybe DiffTime, StdGen)
    stateToLimit SingIdle                 rnd = (Just 3673, rnd)
    stateToLimit SingIntersect            rnd = (shortWait, rnd)
    stateToLimit (SingNext SingCanAwait)  rnd = (shortWait, rnd)
    stateToLimit (SingNext SingMustReply) rnd =
      -- We draw from a range for which streaks of empty slots ranges
      -- from 0.0001% up to 1% probability.
      -- t = T_s [log (1-Y) / log (1-f)]
      -- Y = [0.99, 0.999...]
      -- T_s = slot length of 1s.
      -- f = 0.05
      -- The timeout is randomly picked per state to avoid all peers go down at
      -- the same time in case of a long streak of empty slots, and thus to
      -- avoid global synchronisation.  The timeout is picked uniformly from
      -- the interval 135 - 269, which corresponds to 99.9% to
      -- 99.9999% thresholds.
      let timeout :: DiffTime
          (timeout, rnd') = first realToFrac
                          . randomR ( realToFrac minChainSyncTimeout :: Double
                                    , realToFrac maxChainSyncTimeout :: Double
                                    )
                          $ rnd
      in (Just timeout, rnd')
    stateToLimit a@SingDone rnd = (notActiveState a, rnd)

-- | Codec for chain sync that encodes/decodes headers, points & tips.
--
-- /NOTE:/ See 'wrapCBORinCBOR' and 'unwrapCBORinCBOR' if you want to use this
-- with a header type that has annotations.
codecChainSync
  :: forall header point tip m.
     (MonadST m)
  => (header -> CBOR.Encoding)
  -- ^ encode header
  -> (forall s . CBOR.Decoder s header)
  -- ^ decode header
  -> (point -> CBOR.Encoding)
  -- ^ encode point
  -> (forall s . CBOR.Decoder s point)
  -- ^ decode point
  -> (tip -> CBOR.Encoding)
  -- ^ encode tip
  -> (forall s. CBOR.Decoder s tip)
  -- ^ decode tip
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

        (2, 3, SingNext tok) -> withSingI tok $
              (\h tip -> SomeMessage (MsgRollForward h tip))
          <$> decodeHeader
          <*> decodeTip

        (3, 3, SingNext tok) -> withSingI tok $
              (\p tip -> SomeMessage (MsgRollBackward p tip))
          <$> decodePoint
          <*> decodeTip

        (4, 2, SingIdle) ->
          SomeMessage . MsgFindIntersect <$> decodeList decodePoint

        (5, 3, SingIntersect) -> do
              (\p tip -> SomeMessage (MsgIntersectFound p tip))
          <$> decodePoint
          <*> decodeTip

        (6, 2, SingIntersect) -> do
              SomeMessage . MsgIntersectNotFound
          <$> decodeTip

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
codecChainSyncId :: forall (header :: Type) (point :: Type) (tip :: Type) m. Monad m
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
