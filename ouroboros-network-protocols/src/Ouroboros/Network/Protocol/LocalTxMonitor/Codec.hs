{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Network.Protocol.LocalTxMonitor.Codec
  ( codecLocalTxMonitor
  , codecLocalTxMonitorId
  ) where

import Control.Monad
import Control.Monad.Class.MonadST
import Data.Functor ((<&>))
import Data.Word (Word32)

import Network.TypedProtocol.Codec.CBOR

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Text.Printf

import Ouroboros.Network.Protocol.LocalTxMonitor.Type

codecLocalTxMonitor ::
     forall txid tx slot m ptcl.
     ( MonadST m
     , ptcl ~ LocalTxMonitor txid tx slot
     )
  => (txid -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s txid)
  -> (tx -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s tx)
  -> (slot -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s slot)
  -> Codec (LocalTxMonitor txid tx slot) CBOR.DeserialiseFailure m ByteString
codecLocalTxMonitor encodeTxId decodeTxId
                    encodeTx   decodeTx
                    encodeSlot decodeSlot =
    mkCodecCborLazyBS encode decode
  where
    encode ::
         forall (pr :: PeerRole) (st :: ptcl) (st' :: ptcl). ()
      => PeerHasAgency pr st
      -> Message ptcl st st'
      -> CBOR.Encoding
    encode (ClientAgency TokIdle) = \case
      MsgDone ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 0
      MsgAcquire ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 1

    encode (ClientAgency TokAcquired) = \case
      MsgAwaitAcquire ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 1
      MsgRelease ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 3
      MsgNextTx ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 5
      MsgHasTx txid ->
        CBOR.encodeListLen 2 <> CBOR.encodeWord 7 <> encodeTxId txid
      MsgGetSizes ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 9
      MsgGetMeasures ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 11 -- why increments of 2?

    encode (ServerAgency TokAcquiring) = \case
      MsgAcquired slot ->
        CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> encodeSlot slot

    encode (ServerAgency (TokBusy TokNextTx)) = \case
      MsgReplyNextTx Nothing ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 6
      MsgReplyNextTx (Just tx) ->
        CBOR.encodeListLen 2 <> CBOR.encodeWord 6 <> encodeTx tx

    encode (ServerAgency (TokBusy TokHasTx)) = \case
      MsgReplyHasTx has ->
        CBOR.encodeListLen 2 <> CBOR.encodeWord 8 <> CBOR.encodeBool has

    encode (ServerAgency (TokBusy TokGetSizes)) = \case
      MsgReplyGetSizes sz ->
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 10
        <> CBOR.encodeListLen 3
        <> CBOR.encodeWord32 (capacityInBytes sz)
        <> CBOR.encodeWord32 (sizeInBytes sz)
        <> CBOR.encodeWord32 (numberOfTxs sz)

    encode (ServerAgency (TokBusy TokGetMeasures)) = \case
      MsgReplyGetMeasures measures ->
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 12
        <> CBOR.encodeListLen 2
        <> CBOR.encodeWord32 (txCount measures)
        <> encodeMeasureMap (measuresMap measures)

    decode ::
         forall s (pr :: PeerRole) (st :: ptcl). ()
      => PeerHasAgency pr st
      -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (ClientAgency TokIdle, 1, 0) ->
          return (SomeMessage MsgDone)
        (ClientAgency TokIdle, 1, 1) ->
          return (SomeMessage MsgAcquire)

        (ClientAgency TokAcquired, 1, 1) ->
          return (SomeMessage MsgAwaitAcquire)
        (ClientAgency TokAcquired, 1, 3) ->
          return (SomeMessage MsgRelease)
        (ClientAgency TokAcquired, 1, 5) ->
          return (SomeMessage MsgNextTx)
        (ClientAgency TokAcquired, 2, 7) -> do
          txid <- decodeTxId
          return (SomeMessage (MsgHasTx txid))
        (ClientAgency TokAcquired, 1, 9) ->
          return (SomeMessage MsgGetSizes)
        (ClientAgency TokAcquired, 1, 11) ->
          return (SomeMessage MsgGetMeasures)

        (ServerAgency TokAcquiring, 2, 2) -> do
          slot <- decodeSlot
          return (SomeMessage (MsgAcquired slot))

        (ServerAgency (TokBusy TokNextTx), 1, 6) ->
          return (SomeMessage (MsgReplyNextTx Nothing))
        (ServerAgency (TokBusy TokNextTx), 2, 6) -> do
          tx <- decodeTx
          return (SomeMessage (MsgReplyNextTx (Just tx)))

        (ServerAgency (TokBusy TokHasTx), 2, 8) -> do
          has <- CBOR.decodeBool
          return (SomeMessage (MsgReplyHasTx has))

        (ServerAgency (TokBusy TokGetSizes), 2, 10) -> do
          _len <- CBOR.decodeListLen
          capacityInBytes <- CBOR.decodeWord32
          sizeInBytes <- CBOR.decodeWord32
          numberOfTxs <- CBOR.decodeWord32
          let sizes = MempoolSizeAndCapacity { capacityInBytes, sizeInBytes, numberOfTxs }
          return (SomeMessage (MsgReplyGetSizes sizes))

        (ServerAgency (TokBusy TokGetMeasures), 2, 12) -> do
          _len <- CBOR.decodeListLen
          txCount <- CBOR.decodeWord32
          measuresMap <- decodeMeasureMap
          let measures = MempoolMeasures { txCount, measuresMap }
          pure (SomeMessage (MsgReplyGetMeasures measures))

        (ClientAgency TokIdle, _, _) ->
          fail (printf "codecLocalTxMonitor (%s) unexpected key (%d, %d)" (show stok) key len)
        (ClientAgency TokAcquired, _, _) ->
          fail (printf "codecLocalTxMonitor (%s) unexpected key (%d, %d)" (show stok) key len)
        (ServerAgency TokAcquiring, _, _) ->
          fail (printf "codecLocalTxMonitor (%s) unexpected key (%d, %d)" (show stok) key len)
        (ServerAgency (TokBusy _), _, _) ->
          fail (printf "codecLocalTxMonitor (%s) unexpected key (%d, %d)" (show stok) key len)

encodeMeasureMap :: Map MeasureName (SizeAndCapacity Word32) -> CBOR.Encoding
encodeMeasureMap m =
  CBOR.encodeMapLen (fromIntegral (Map.size m)) <>
  Map.foldMapWithKey f m
  where
    f mn sc =
      encodeMeasureName mn <> encodeSizeAndCapacity sc

decodeMeasureMap :: CBOR.Decoder s (Map MeasureName (SizeAndCapacity Word32))
decodeMeasureMap = do
  len <- CBOR.decodeMapLen
  mapContents <- replicateM len $
    (,) <$> decodeMeasureName <*> decodeSizeAndCapacity
  pure $ Map.fromList mapContents

encodeMeasureName :: MeasureName -> CBOR.Encoding
encodeMeasureName = CBOR.encodeString . \case
  TransactionBytes -> "transaction_bytes"
  ExUnitsMemory -> "ex_units_memory"
  ExUnitsSteps -> "ex_units_steps"
  ReferenceScriptsBytes -> "reference_scripts_bytes"
  MeasureNameFromFuture (UnknownMeasureName n) -> n

decodeMeasureName :: CBOR.Decoder s MeasureName
decodeMeasureName = CBOR.decodeString <&> \case
  "transaction_bytes" -> TransactionBytes
  "ex_units_memory" -> ExUnitsMemory
  "ex_units_steps" -> ExUnitsSteps
  "reference_scripts_bytes" -> ReferenceScriptsBytes
  unknownKey -> MeasureNameFromFuture (UnknownMeasureName unknownKey)

encodeSizeAndCapacity :: SizeAndCapacity Word32 -> CBOR.Encoding
encodeSizeAndCapacity sc =
  CBOR.encodeListLen 2 <>
  CBOR.encodeWord32 (size sc) <>
  CBOR.encodeWord32 (capacity sc)

decodeSizeAndCapacity :: CBOR.Decoder s (SizeAndCapacity Word32)
decodeSizeAndCapacity = do
  _len <- CBOR.decodeListLen
  size <- CBOR.decodeWord32
  capacity <- CBOR.decodeWord32
  pure SizeAndCapacity { size, capacity }

-- | An identity 'Codec' for the 'LocalTxMonitor' protocol. It does not do
-- any serialisation. It keeps the typed messages, wrapped in 'AnyMessage'.
--
codecLocalTxMonitorId ::
     forall txid tx slot m ptcl.
     ( Monad m
     , ptcl ~ LocalTxMonitor txid tx slot
     )
  => Codec ptcl CodecFailure m (AnyMessage ptcl)
codecLocalTxMonitorId =
    Codec { encode, decode }
  where
    encode ::
         forall (pr :: PeerRole) st st'. ()
      => PeerHasAgency pr st
      -> Message ptcl st st'
      -> AnyMessage ptcl
    encode _ =
      AnyMessage

    decode ::
         forall (pr :: PeerRole) (st :: ptcl). ()
      => PeerHasAgency pr st
      -> m (DecodeStep (AnyMessage ptcl) CodecFailure m (SomeMessage st))
    decode stok =
      let res :: Message ptcl st st' -> m (DecodeStep bytes failure m (SomeMessage st))
          res msg = return (DecodeDone (SomeMessage msg) Nothing)
       in return $ DecodePartial $ \bytes -> case (stok, bytes) of
        (ClientAgency TokIdle,     Just (AnyMessage msg@MsgAcquire{}))      -> res msg
        (ClientAgency TokIdle,     Just (AnyMessage msg@MsgDone{}))         -> res msg
        (ClientAgency TokAcquired, Just (AnyMessage msg@MsgAwaitAcquire{})) -> res msg
        (ClientAgency TokAcquired, Just (AnyMessage msg@MsgNextTx{}))       -> res msg
        (ClientAgency TokAcquired, Just (AnyMessage msg@MsgHasTx{}))        -> res msg
        (ClientAgency TokAcquired, Just (AnyMessage msg@MsgRelease{}))      -> res msg

        (ServerAgency TokAcquiring,        Just (AnyMessage msg@MsgAcquired{}))    -> res msg
        (ServerAgency (TokBusy TokNextTx), Just (AnyMessage msg@MsgReplyNextTx{})) -> res msg
        (ServerAgency (TokBusy TokHasTx),  Just (AnyMessage msg@MsgReplyHasTx{}))  -> res msg

        (_, Nothing) ->
          return (DecodeFail CodecFailureOutOfInput)
        (_, _) ->
          return (DecodeFail (CodecFailure "codecLocalTxMonitorId: no matching message"))
