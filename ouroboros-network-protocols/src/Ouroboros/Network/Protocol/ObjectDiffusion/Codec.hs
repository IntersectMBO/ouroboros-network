{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Network.Protocol.ObjectDiffusion.Codec
  ( codecObjectDiffusion,
    codecObjectDiffusionId,
    byteLimitsObjectDiffusion,
    timeLimitsObjectDiffusion,
  )
where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Network.TypedProtocol.Codec.CBOR
import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import Text.Printf

-- | Byte Limits.
byteLimitsObjectDiffusion ::
  forall bytes objectId object.
  (bytes -> Word) ->
  ProtocolSizeLimits (ObjectDiffusion objectId object) bytes
byteLimitsObjectDiffusion = ProtocolSizeLimits stateToLimit
  where
    stateToLimit ::
      forall (st :: ObjectDiffusion objectId object).
      (ActiveState st) =>
      StateToken st ->
      Word
    stateToLimit SingInit = smallByteLimit
    stateToLimit (SingObjectIds SingBlocking) = largeByteLimit
    stateToLimit (SingObjectIds SingNonBlocking) = largeByteLimit
    stateToLimit SingObjects = largeByteLimit
    stateToLimit SingIdle = smallByteLimit
    stateToLimit a@SingDone = notActiveState a

-- | 'ObjectDiffusion' time limits.
--
-- +---------------------------------+---------------+
-- | 'ObjectDiffusion' state         | timeout (s)   |
-- +=================================+===============+
-- | `StInit`                        | `waitForever` |
-- +---------------------------------+---------------+
-- | `StIdle`                        | `waitForever` |
-- +---------------------------------+---------------+
-- | @'StObjectIds' 'StBlocking'@    | `waitForever` |
-- +---------------------------------+---------------+
-- | @'StObjectIds' 'StNonBlocking'@ | `shortWait`   |
-- +---------------------------------+---------------+
-- | `StObjects`                     | `shortWait`   |
-- +---------------------------------+---------------+
timeLimitsObjectDiffusion :: forall (objectId :: Type) (object :: Type). ProtocolTimeLimits (ObjectDiffusion objectId object)
timeLimitsObjectDiffusion = ProtocolTimeLimits stateToLimit
  where
    stateToLimit ::
      forall (st :: ObjectDiffusion objectId object).
      (ActiveState st) =>
      StateToken st ->
      Maybe DiffTime
    stateToLimit SingInit = waitForever
    stateToLimit (SingObjectIds SingBlocking) = waitForever
    stateToLimit (SingObjectIds SingNonBlocking) = shortWait
    stateToLimit SingObjects = shortWait
    stateToLimit SingIdle = waitForever
    stateToLimit a@SingDone = notActiveState a

codecObjectDiffusion ::
  forall (objectId :: Type) (object :: Type) m.
  (MonadST m) =>
  -- | encode 'objectId'
  (objectId -> CBOR.Encoding) ->
  -- | decode 'objectId'
  (forall s. CBOR.Decoder s objectId) ->
  -- | encode object
  (object -> CBOR.Encoding) ->
  -- | decode object
  (forall s. CBOR.Decoder s object) ->
  Codec (ObjectDiffusion objectId object) CBOR.DeserialiseFailure m ByteString
codecObjectDiffusion encodeObjectId decodeObjectId encodeObject decodeObject =
  mkCodecCborLazyBS
    (encodeObjectDiffusion encodeObjectId encodeObject)
    decode
  where
    decode ::
      forall (st :: ObjectDiffusion objectId object).
      (ActiveState st) =>
      StateToken st ->
      forall s. CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      decodeObjectDiffusion decodeObjectId decodeObject stok len key

encodeObjectDiffusion ::
  forall (objectId :: Type) (object :: Type) (st :: ObjectDiffusion objectId object) (st' :: ObjectDiffusion objectId object).
  -- | encode 'objectId'
  (objectId -> CBOR.Encoding) ->
  -- | encode 'object'
  (object -> CBOR.Encoding) ->
  Message (ObjectDiffusion objectId object) st st' ->
  CBOR.Encoding
encodeObjectDiffusion encodeObjectId encodeObject = encode
  where
    encode ::
      forall st0 st1.
      Message (ObjectDiffusion objectId object) st0 st1 ->
      CBOR.Encoding
    encode MsgInit =
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 6
    encode (MsgRequestObjectIds blocking (NumObjectIdsToAck ackNo) (NumObjectIdsToReq reqNo)) =
      CBOR.encodeListLen 4
        <> CBOR.encodeWord 0
        <> CBOR.encodeBool
          ( case blocking of
              SingBlocking -> True
              SingNonBlocking -> False
          )
        <> CBOR.encodeWord16 ackNo
        <> CBOR.encodeWord16 reqNo
    encode (MsgReplyObjectIds objIds) =
      CBOR.encodeListLen 2
        <> CBOR.encodeWord 1
        <> CBOR.encodeListLenIndef
        <> foldr
          ( \(objId, SizeInBytes sz) r ->
              CBOR.encodeListLen 2
                <> encodeObjectId objId
                <> CBOR.encodeWord32 sz
                <> r
          )
          CBOR.encodeBreak
          objIds'
      where
        objIds' :: [(objectId, SizeInBytes)]
        objIds' = case objIds of
          BlockingReply xs -> NonEmpty.toList xs
          NonBlockingReply xs -> xs
    encode (MsgRequestObjects objIds) =
      CBOR.encodeListLen 2
        <> CBOR.encodeWord 2
        <> CBOR.encodeListLenIndef
        <> foldr (\objId r -> encodeObjectId objId <> r) CBOR.encodeBreak objIds
    encode (MsgReplyObjects objects) =
      CBOR.encodeListLen 2
        <> CBOR.encodeWord 3
        <> CBOR.encodeListLenIndef
        <> foldr (\objId r -> encodeObject objId <> r) CBOR.encodeBreak objects
    encode MsgDone =
      CBOR.encodeListLen 1
        <> CBOR.encodeWord 4

decodeObjectDiffusion ::
  forall (objectId :: Type) (object :: Type) (st :: ObjectDiffusion objectId object) s.
  (ActiveState st) =>
  -- | decode 'objectId'
  (forall s'. CBOR.Decoder s' objectId) ->
  -- | decode object
  (forall s'. CBOR.Decoder s' object) ->
  StateToken st ->
  Int ->
  Word ->
  CBOR.Decoder s (SomeMessage st)
decodeObjectDiffusion decodeObjectId decodeObject = decode
  where
    decode ::
      forall (st' :: ObjectDiffusion objectId object).
      (ActiveState st') =>
      StateToken st' ->
      Int ->
      Word ->
      CBOR.Decoder s (SomeMessage st')
    decode stok len key = do
      case (stok, len, key) of
        (SingInit, 1, 6) ->
          return (SomeMessage MsgInit)
        (SingIdle, 4, 0) -> do
          blocking <- CBOR.decodeBool
          ackNo <- NumObjectIdsToAck <$> CBOR.decodeWord16
          reqNo <- NumObjectIdsToReq <$> CBOR.decodeWord16
          return $! case blocking of
            True -> SomeMessage (MsgRequestObjectIds SingBlocking ackNo reqNo)
            False -> SomeMessage (MsgRequestObjectIds SingNonBlocking ackNo reqNo)
        (SingObjectIds b, 2, 1) -> do
          CBOR.decodeListLenIndef
          objIds <-
            CBOR.decodeSequenceLenIndef
              (flip (:))
              []
              reverse
              ( do
                  CBOR.decodeListLenOf 2
                  objId <- decodeObjectId
                  sz <- CBOR.decodeWord32
                  return (objId, SizeInBytes sz)
              )
          case (b, objIds) of
            (SingBlocking, t : ts) ->
              return $
                SomeMessage (MsgReplyObjectIds (BlockingReply (t NonEmpty.:| ts)))
            (SingNonBlocking, ts) ->
              return $
                SomeMessage (MsgReplyObjectIds (NonBlockingReply ts))
            (SingBlocking, []) ->
              fail "codecObjectDiffusion: MsgReplyObjectIds: empty list not permitted"
        (SingIdle, 2, 2) -> do
          CBOR.decodeListLenIndef
          objIds <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeObjectId
          return (SomeMessage (MsgRequestObjects objIds))
        (SingObjects, 2, 3) -> do
          CBOR.decodeListLenIndef
          objIds <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeObject
          return (SomeMessage (MsgReplyObjects objIds))
        (SingObjectIds SingBlocking, 1, 4) ->
          return (SomeMessage MsgDone)
        (SingDone, _, _) -> notActiveState stok
        --
        -- failures per protocol state
        --

        (SingInit, _, _) ->
          fail (printf "codecObjectDiffusion (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingObjectIds SingBlocking, _, _) ->
          fail (printf "codecObjectDiffusion (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingObjectIds SingNonBlocking, _, _) ->
          fail (printf "codecObjectDiffusion (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingObjects, _, _) ->
          fail (printf "codecObjectDiffusion (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingIdle, _, _) ->
          fail (printf "codecObjectDiffusion (%s) unexpected key (%d, %d)" (show stok) key len)

codecObjectDiffusionId ::
  forall objectId object m.
  (Monad m) =>
  Codec (ObjectDiffusion objectId object) CodecFailure m (AnyMessage (ObjectDiffusion objectId object))
codecObjectDiffusionId = Codec {encode, decode}
  where
    encode ::
      forall st st'.
      (ActiveState st) =>
      (StateTokenI st) =>
      Message (ObjectDiffusion objectId object) st st' ->
      AnyMessage (ObjectDiffusion objectId object)
    encode = AnyMessage

    decode ::
      forall (st :: ObjectDiffusion objectId object).
      (ActiveState st) =>
      StateToken st ->
      m
        ( DecodeStep
            (AnyMessage (ObjectDiffusion objectId object))
            CodecFailure
            m
            (SomeMessage st)
        )
    decode stok = return $ DecodePartial $ \bytes -> return $ case (stok, bytes) of
      (SingInit, Just (AnyMessage msg@MsgInit)) -> DecodeDone (SomeMessage msg) Nothing
      (SingIdle, Just (AnyMessage msg@(MsgRequestObjectIds SingBlocking _ _))) -> DecodeDone (SomeMessage msg) Nothing
      (SingIdle, Just (AnyMessage msg@(MsgRequestObjectIds SingNonBlocking _ _))) -> DecodeDone (SomeMessage msg) Nothing
      (SingIdle, Just (AnyMessage msg@(MsgRequestObjects {}))) -> DecodeDone (SomeMessage msg) Nothing
      (SingObjects, Just (AnyMessage msg@(MsgReplyObjects {}))) -> DecodeDone (SomeMessage msg) Nothing
      (SingObjectIds b, Just (AnyMessage msg)) -> case (b, msg) of
        (SingBlocking, MsgReplyObjectIds (BlockingReply {})) -> DecodeDone (SomeMessage msg) Nothing
        (SingNonBlocking, MsgReplyObjectIds (NonBlockingReply {})) -> DecodeDone (SomeMessage msg) Nothing
        (SingBlocking, MsgDone {}) -> DecodeDone (SomeMessage msg) Nothing
        (_, _) -> DecodeFail (CodecFailure "codecObjectDiffusionId: no matching message")
      (SingDone, _) -> notActiveState stok
      (_, _) -> DecodeFail (CodecFailure "codecObjectDiffusionId: no matching message")
