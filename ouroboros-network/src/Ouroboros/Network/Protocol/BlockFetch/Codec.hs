{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
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

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime

import qualified Data.ByteString.Lazy as LBS
import           Data.Singletons

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Text.Printf

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.Limits

-- | Byte Limit.
byteLimitsBlockFetch :: forall bytes block point.
                        (bytes -> Word)
                     -> ProtocolSizeLimits (BlockFetch block point) bytes
byteLimitsBlockFetch = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (st :: BlockFetch block point).
                    SingPeerHasAgency st -> Word
    stateToLimit (SingClientHasAgency SingBFIdle)      = smallByteLimit
    stateToLimit (SingServerHasAgency SingBFBusy)      = smallByteLimit
    stateToLimit (SingServerHasAgency SingBFStreaming) = largeByteLimit

-- | Time Limits
--
-- `TokIdle' No timeout
-- `TokBusy` `longWait` timeout
-- `TokStreaming` `longWait` timeout
timeLimitsBlockFetch :: forall block point.
                        ProtocolTimeLimits (BlockFetch block point)
timeLimitsBlockFetch = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st :: BlockFetch block point).
                    SingPeerHasAgency st -> Maybe DiffTime
    stateToLimit (SingClientHasAgency SingBFIdle)      = waitForever
    stateToLimit (SingServerHasAgency SingBFBusy)      = longWait
    stateToLimit (SingServerHasAgency SingBFStreaming) = longWait

-- | Codec for chain sync that encodes/decodes blocks
--
-- NOTE: See 'wrapCBORinCBOR' and 'unwrapCBORinCBOR' if you want to use this
-- with a block type that has annotations.
codecBlockFetch
  :: forall block point m.
     MonadST m
  => (block            -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s block)
  -> (point -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s point)
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
            SingI (PeerHasAgency st)
         => CBOR.Decoder s (SomeMessage st)
  decode = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (sing :: SingPeerHasAgency st, key, len) of
      (SingClientHasAgency SingBFIdle, 0, 3) -> do
        from <- decodePoint
        to   <- decodePoint
        return $ SomeMessage $ MsgRequestRange (ChainRange from to)
      (SingClientHasAgency SingBFIdle, 1, 1) -> return $ SomeMessage MsgClientDone
      (SingServerHasAgency SingBFBusy, 2, 1) -> return $ SomeMessage MsgStartBatch
      (SingServerHasAgency SingBFBusy, 3, 1) -> return $ SomeMessage MsgNoBlocks
      (SingServerHasAgency SingBFStreaming, 4, 2) -> SomeMessage . MsgBlock
                                                 <$> decodeBlock
      (SingServerHasAgency SingBFStreaming, 5, 1) -> return $ SomeMessage MsgBatchDone

      --
      -- failures per protocol state
      --

      (stok@(SingClientHasAgency SingBFIdle), _, _) ->
        fail (printf "codecBlockFetch (%s) unexpected key (%d, %d)" (show stok) key len)
      (stok@(SingServerHasAgency SingBFStreaming), _ , _) ->
        fail (printf "codecBlockFetch (%s) unexpected key (%d, %d)" (show stok) key len)
      (stok@(SingServerHasAgency SingBFBusy), _, _) ->
        fail (printf "codecBlockFetch (%s) unexpected key (%d, %d)" (show stok) key len)


codecBlockFetchId
  :: forall block point m.
     Monad m
  => Codec (BlockFetch block point) CodecFailure m
           (AnyMessage (BlockFetch block point))
codecBlockFetchId = Codec encode decode
 where
  encode :: forall st st'.
            SingI (PeerHasAgency st)
         => Message (BlockFetch block point) st st'
         -> AnyMessage (BlockFetch block point)
  encode = AnyMessage

  decode :: forall (st :: BlockFetch block point).
            SingI (PeerHasAgency st)
         => m (DecodeStep (AnyMessage (BlockFetch block point))
                          CodecFailure m (SomeMessage st))
  decode = return $ DecodePartial $ \bytes ->
    case (sing :: SingPeerHasAgency st, bytes) of
      (_, Nothing) -> return $ DecodeFail CodecFailureOutOfInput
      (SingClientHasAgency SingBFIdle,      Just (AnyMessage msg@(MsgRequestRange {}))) -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingClientHasAgency SingBFIdle,      Just (AnyMessage msg@(MsgClientDone {})))   -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingServerHasAgency SingBFBusy,      Just (AnyMessage msg@(MsgStartBatch {})))   -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingServerHasAgency SingBFBusy,      Just (AnyMessage msg@(MsgNoBlocks {})))     -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingServerHasAgency SingBFStreaming, Just (AnyMessage msg@(MsgBlock {})))        -> return (DecodeDone (SomeMessage msg) Nothing)
      (SingServerHasAgency SingBFStreaming, Just (AnyMessage msg@(MsgBatchDone {})))    -> return (DecodeDone (SomeMessage msg) Nothing)
      (_, _) -> return $ DecodeFail (CodecFailure "codecBlockFetchId: no matching message")
