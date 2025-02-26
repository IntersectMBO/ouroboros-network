{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
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

import Network.TypedProtocol.Codec.CBOR
import Network.TypedProtocol.Core

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Text.Printf

import Ouroboros.Network.NodeToClient.Version qualified as V
import Ouroboros.Network.Protocol.LocalTxMonitor.Type

codecLocalTxMonitor ::
     forall txid tx slot m ptcl.
     ( MonadST m
     , ptcl ~ LocalTxMonitor txid tx slot
     )
  => V.NodeToClientVersion
     -- ^ Whether to accept `MsgGetMeasures`
  -> (txid -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s txid)
  -> (tx -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s tx)
  -> (slot -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s slot)
  -> Codec (LocalTxMonitor txid tx slot) CBOR.DeserialiseFailure m ByteString
codecLocalTxMonitor version
                    encodeTxId decodeTxId
                    encodeTx   decodeTx
                    encodeSlot decodeSlot =
    mkCodecCborLazyBS encode decode
  where
    canHandleMeasures = version >= V.NodeToClientV_20

    encode ::
         forall (st  :: ptcl) (st' :: ptcl).
         Message ptcl st st'
      -> CBOR.Encoding
    encode = \case
      MsgDone ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 0
      MsgAcquire ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 1
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
      MsgGetMeasures
        | canHandleMeasures ->
            CBOR.encodeListLen 1 <> CBOR.encodeWord 11
        | otherwise ->
            error $ unwords
              [ "encodeFailure: local tx monitor:"
              , "MsgGetMeasures is only supported on NodeToClient"
              , "versions >= v20"
              ]
      MsgAcquired slot ->
        CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> encodeSlot slot
      MsgReplyNextTx Nothing ->
        CBOR.encodeListLen 1 <> CBOR.encodeWord 6
      MsgReplyNextTx (Just tx) ->
        CBOR.encodeListLen 2 <> CBOR.encodeWord 6 <> encodeTx tx
      MsgReplyHasTx has ->
        CBOR.encodeListLen 2 <> CBOR.encodeWord 8 <> CBOR.encodeBool has
      MsgReplyGetSizes sz ->
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 10
        <> CBOR.encodeListLen 3
        <> CBOR.encodeWord32 (capacityInBytes sz)
        <> CBOR.encodeWord32 (sizeInBytes sz)
        <> CBOR.encodeWord32 (numberOfTxs sz)
      MsgReplyGetMeasures measures
        | canHandleMeasures ->
               CBOR.encodeListLen 3
            <> CBOR.encodeWord 12
            <> CBOR.encodeWord32 (txCount measures)
            <> encodeMeasureMap (measuresMap measures)
        | otherwise ->
            error $ unwords
              [ "encodeFailure: local tx monitor:"
              , "MsgReplyGetMeasures is only supported on NodeToClient"
              , "versions >= v20"
              ]

    decode ::
         forall s (st :: ptcl).
         ActiveState st
      => StateToken st
      -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (SingIdle, 1, 0) ->
          return (SomeMessage MsgDone)
        (SingIdle, 1, 1) ->
          return (SomeMessage MsgAcquire)

        (SingAcquired, 1, 1) ->
          return (SomeMessage MsgAwaitAcquire)
        (SingAcquired, 1, 3) ->
          return (SomeMessage MsgRelease)
        (SingAcquired, 1, 5) ->
          return (SomeMessage MsgNextTx)
        (SingAcquired, 2, 7) -> do
          txid <- decodeTxId
          return (SomeMessage (MsgHasTx txid))
        (SingAcquired, 1, 9) ->
          return (SomeMessage MsgGetSizes)
        (SingAcquired, 1, 11) ->
          return (SomeMessage MsgGetMeasures)

        (SingAcquiring, 2, 2) -> do
          slot <- decodeSlot
          return (SomeMessage (MsgAcquired slot))

        (SingBusy SingNextTx, 1, 6) ->
          return (SomeMessage (MsgReplyNextTx Nothing))
        (SingBusy SingNextTx, 2, 6) -> do
          tx <- decodeTx
          return (SomeMessage (MsgReplyNextTx (Just tx)))

        (SingBusy SingHasTx, 2, 8) -> do
          has <- CBOR.decodeBool
          return (SomeMessage (MsgReplyHasTx has))

        (SingBusy SingGetSizes, 2, 10) -> do
          _len <- CBOR.decodeListLen
          capacityInBytes <- CBOR.decodeWord32
          sizeInBytes <- CBOR.decodeWord32
          numberOfTxs <- CBOR.decodeWord32
          let sizes = MempoolSizeAndCapacity { capacityInBytes, sizeInBytes, numberOfTxs }
          return (SomeMessage (MsgReplyGetSizes sizes))

        (SingBusy SingGetMeasures, 3, 12) -> do
          txCount <- CBOR.decodeWord32
          measuresMap <- decodeMeasureMap
          let measures = MempoolMeasures { txCount, measuresMap }
          pure (SomeMessage (MsgReplyGetMeasures measures))

        (SingDone, _, _) -> notActiveState stok

        (_, _, _) ->
          fail (printf "codecLocalTxMonitor (%s, %s) unexpected key (%d, %d)"
                       (show (activeAgency :: ActiveAgency st)) (show stok) key len)

encodeMeasureMap :: Map MeasureName (SizeAndCapacity Integer) -> CBOR.Encoding
encodeMeasureMap m =
  CBOR.encodeMapLen (fromIntegral (Map.size m)) <>
  Map.foldMapWithKey f m
  where
    f mn sc =
      encodeMeasureName mn <> encodeSizeAndCapacity sc

decodeMeasureMap :: CBOR.Decoder s (Map MeasureName (SizeAndCapacity Integer))
decodeMeasureMap = do
  len <- CBOR.decodeMapLen
  mapContents <- replicateM len $
    (,) <$> decodeMeasureName <*> decodeSizeAndCapacity
  pure $ Map.fromList mapContents

encodeMeasureName :: MeasureName -> CBOR.Encoding
encodeMeasureName (MeasureName t) = CBOR.encodeString t

decodeMeasureName :: CBOR.Decoder s MeasureName
decodeMeasureName = MeasureName <$> CBOR.decodeString

encodeSizeAndCapacity :: SizeAndCapacity Integer -> CBOR.Encoding
encodeSizeAndCapacity sc =
  CBOR.encodeListLen 2 <>
  CBOR.encodeInteger (size sc) <>
  CBOR.encodeInteger (capacity sc)

decodeSizeAndCapacity :: CBOR.Decoder s (SizeAndCapacity Integer)
decodeSizeAndCapacity = do
  _len <- CBOR.decodeListLen
  size <- CBOR.decodeInteger
  capacity <- CBOR.decodeInteger
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
    Codec encode decode
  where
    encode ::
         forall st st'.
         StateTokenI st
      => ActiveState st
      => Message ptcl st st'
      -> AnyMessage ptcl
    encode = AnyMessage

    decode ::
         forall (st :: ptcl).
         ActiveState st
      => StateToken st
      -> m (DecodeStep (AnyMessage ptcl) CodecFailure m (SomeMessage st))
    decode stok =
      let res :: (StateTokenI st, StateTokenI st')
              => Message ptcl st st' -> m (DecodeStep bytes failure m (SomeMessage st))
          res msg = return (DecodeDone (SomeMessage msg) Nothing)
      in return $ DecodePartial $ \bytes -> case (stok, bytes) of
        (SingIdle,                 Just (AnyMessage msg@MsgAcquire{}))          -> res msg
        (SingIdle,                 Just (AnyMessage msg@MsgDone{}))             -> res msg
        (SingAcquired,             Just (AnyMessage msg@MsgAwaitAcquire{}))     -> res msg
        (SingAcquired,             Just (AnyMessage msg@MsgNextTx{}))           -> res msg
        (SingAcquired,             Just (AnyMessage msg@MsgHasTx{}))            -> res msg
        (SingAcquired,             Just (AnyMessage msg@MsgRelease{}))          -> res msg
        (SingAcquired,             Just (AnyMessage msg@MsgGetSizes{}))         -> res msg
        (SingAcquired,             Just (AnyMessage msg@MsgGetMeasures{}))      -> res msg
        (SingAcquiring,            Just (AnyMessage msg@MsgAcquired{}))         -> res msg
        (SingBusy SingNextTx,      Just (AnyMessage msg@MsgReplyNextTx{}))      -> res msg
        (SingBusy SingHasTx,       Just (AnyMessage msg@MsgReplyHasTx{}))       -> res msg
        (SingBusy SingGetSizes,    Just (AnyMessage msg@MsgReplyGetSizes{}))    -> res msg
        (SingBusy SingGetMeasures, Just (AnyMessage msg@MsgReplyGetMeasures{})) -> res msg

        (SingDone, _) -> notActiveState stok

        (_, Nothing) ->
          return (DecodeFail CodecFailureOutOfInput)
        (_, _) ->
          return (DecodeFail (CodecFailure "codecLocalTxMonitorId: no matching message"))
