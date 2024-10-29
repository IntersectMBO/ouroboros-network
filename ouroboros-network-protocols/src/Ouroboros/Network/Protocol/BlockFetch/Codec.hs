{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Protocol.BlockFetch.Codec
  ( codecBlockFetch
  , codecBlockFetchId
  , byteLimitsBlockFetch
  , timeLimitsBlockFetch
  ) where

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI

import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Text.Printf

import Network.TypedProtocol.Codec.CBOR hiding (decode, encode)

import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.Limits

-- | Byte Limit.
byteLimitsBlockFetch :: forall bytes (block :: Type) (point :: Type).
                        (bytes -> Word) -- ^ compute size of bytes
                     -> ProtocolSizeLimits (BlockFetch block point) bytes
byteLimitsBlockFetch = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (st :: BlockFetch block point).
                    ActiveState st => StateToken st -> Word
    stateToLimit SingBFIdle      = smallByteLimit
    stateToLimit SingBFBusy      = smallByteLimit
    stateToLimit SingBFStreaming = largeByteLimit
    stateToLimit a@SingBFDone    = notActiveState a

-- | Time Limits
--
-- +------------------+---------------+
-- | BlockFetch state | timeout (s)   |
-- +==================+===============+
-- | `BFIdle`         | `waitForever` |
-- +------------------+---------------+
-- | `BFBusy`         | `longWait`    |
-- +------------------+---------------+
-- | `BFStreaming`    | `longWait`    |
-- +------------------+---------------+
--
timeLimitsBlockFetch :: forall (block :: Type) (point :: Type).
                        ProtocolTimeLimits (BlockFetch block point)
timeLimitsBlockFetch = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st :: BlockFetch block point).
                    ActiveState st => StateToken st -> Maybe DiffTime
    stateToLimit SingBFIdle      = waitForever
    stateToLimit SingBFBusy      = longWait
    stateToLimit SingBFStreaming = longWait
    stateToLimit a@SingBFDone    = notActiveState a

-- | Codec for chain sync that encodes/decodes blocks and points.
--
-- /NOTE:/ See 'wrapCBORinCBOR' and 'unwrapCBORinCBOR' if you want to use this
-- with a block type that has annotations.
codecBlockFetch
  :: forall block point m.
     MonadST m
  => (block            -> CBOR.Encoding)
  -- ^ encode block
  -> (forall s. CBOR.Decoder s block)
  -- ^ decode block
  -> (point -> CBOR.Encoding)
  -- ^ encode point
  -> (forall s. CBOR.Decoder s point)
  -- ^ decode point
  -> Codec (BlockFetch block point) CBOR.DeserialiseFailure m LBS.ByteString
codecBlockFetch encodeBlock decodeBlock
                encodePoint decodePoint =
    mkCodecCborLazyBS encode decode
 where
  encode :: forall st st'.
            Message (BlockFetch block point) st st'
         -> CBOR.Encoding
  encode (MsgRequestRange (ChainRange from to)) =
    CBOR.encodeListLen 3 <> CBOR.encodeWord 0 <> encodePoint from
                                              <> encodePoint to
  encode MsgClientDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 1
  encode MsgStartBatch =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 2
  encode MsgNoBlocks =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 3
  encode (MsgBlock block) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 4 <> encodeBlock block
  encode MsgBatchDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 5

  decode :: forall s (st :: BlockFetch block point).
            ActiveState st
         => StateToken st
         -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (stok, key, len) of
      (SingBFIdle, 0, 3) -> do
        from <- decodePoint
        to   <- decodePoint
        return $ SomeMessage $ MsgRequestRange (ChainRange from to)
      (SingBFIdle, 1, 1) -> return $ SomeMessage MsgClientDone
      (SingBFBusy, 2, 1) -> return $ SomeMessage MsgStartBatch
      (SingBFBusy, 3, 1) -> return $ SomeMessage MsgNoBlocks
      (SingBFStreaming, 4, 2) -> SomeMessage . MsgBlock
                                                 <$> decodeBlock
      (SingBFStreaming, 5, 1) -> return $ SomeMessage MsgBatchDone

      --
      -- failures per protocol state
      --

      (SingBFIdle, _, _) ->
        fail (printf "codecBlockFetch (%s, %s) unexpected key (%d, %d)"
                     (show (activeAgency :: ActiveAgency st)) (show stok) key len)
      (SingBFStreaming, _ , _) ->
        fail (printf "codecBlockFetch (%s, %s) unexpected key (%d, %d)"
                     (show (activeAgency :: ActiveAgency st)) (show stok) key len)
      (SingBFBusy, _, _) ->
        fail (printf "codecBlockFetch (%s, %s) unexpected key (%d, %d)"
                     (show (activeAgency :: ActiveAgency st)) (show stok) key len)

      (SingBFDone, _, _) -> notActiveState stok


codecBlockFetchId
  :: forall (block :: Type) (point :: Type) m.
     Monad m
  => Codec (BlockFetch block point) CodecFailure m
           (AnyMessage (BlockFetch block point))
codecBlockFetchId = Codec encode decode
 where
  encode :: forall st st'.
            StateTokenI st
         => ActiveState st
         => Message (BlockFetch block point) st st'
         -> AnyMessage (BlockFetch block point)
  encode = AnyMessage

  decode :: forall (st :: BlockFetch block point).
            ActiveState st
         => StateToken st
         -> m (DecodeStep (AnyMessage (BlockFetch block point))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes ->
    case (stok, bytes) of
      (_, Nothing) -> return $ DecodeFail CodecFailureOutOfInput
      (SingBFIdle,      Just (AnyMessage msg@(MsgRequestRange {}))) -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingBFIdle,      Just (AnyMessage msg@(MsgClientDone {})))   -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingBFBusy,      Just (AnyMessage msg@(MsgStartBatch {})))   -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingBFBusy,      Just (AnyMessage msg@(MsgNoBlocks {})))     -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingBFStreaming, Just (AnyMessage msg@(MsgBlock {})))        -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingBFStreaming, Just (AnyMessage msg@(MsgBatchDone {})))    -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingBFDone,      _)                                          -> notActiveState stok
      (_, _) -> return $ DecodeFail (CodecFailure "codecBlockFetchId: no matching message")
