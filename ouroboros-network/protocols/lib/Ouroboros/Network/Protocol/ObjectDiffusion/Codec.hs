{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Protocol.ObjectDiffusion.Codec
  ( codecObjectDiffusion
  , codecObjectDiffusionId
  , byteLimitsObjectDiffusion
  , timeLimitsObjectDiffusion
  ) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Singletons (withSingI)
import Formatting (formatToString, (%+))
import Formatting qualified as F
import Network.TypedProtocol.Codec.CBOR
import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

-- | Byte Limits.
byteLimitsObjectDiffusion
  :: forall bytes objectId object.
     (bytes -> Word)
  -> ProtocolSizeLimits (ObjectDiffusion objectId object) bytes
byteLimitsObjectDiffusion = ProtocolSizeLimits stateToLimit
  where
    stateToLimit
      :: forall (st :: ObjectDiffusion objectId object).
         ActiveState st
      => StateToken st
      -> Word
    stateToLimit SingInit          = smallByteLimit
    stateToLimit (SingObjectIds _) = largeByteLimit
    stateToLimit SingObjects       = largeByteLimit
    stateToLimit SingIdle          = smallByteLimit
    stateToLimit a@SingDone        = notActiveState a

-- | 'ObjectDiffusion' time limits.
--
-- +-------------------------------------------------------+---------------+
-- | 'ObjectDiffusion' state                               | timeout (s)   |
-- +=======================================================+===============+
-- | `StInit`                                              | `waitForever` |
-- +-------------------------------------------------------+---------------+
-- | `StIdle`                                              | `waitForever` |
-- +-------------------------------------------------------+---------------+
-- | @'StObjectIds' 'StObjectIdsNonBlocking'@              | `shortWait`   |
-- +-------------------------------------------------------+---------------+
-- | @'StObjectIds' ('StObjectIdsBlocking' 'StCanAwait')@  | `shortWait`   |
-- +-------------------------------------------------------+---------------+
-- | @'StObjectIds' ('StObjectIdsBlocking' 'StMustReply')@ | `longWait`    |
-- +-------------------------------------------------------+---------------+
-- | `StObjects`                                           | `shortWait`   |
-- +-------------------------------------------------------+---------------+
timeLimitsObjectDiffusion
  :: forall (objectId :: Type) (object :: Type).
     ProtocolTimeLimits (ObjectDiffusion objectId object)
timeLimitsObjectDiffusion = ProtocolTimeLimits stateToLimit
  where
    stateToLimit
      :: forall (st :: ObjectDiffusion objectId object).
         ActiveState st
      => StateToken st
      -> Maybe DiffTime
    stateToLimit SingInit = waitForever
    stateToLimit (SingObjectIds SingObjectIdsNonBlocking) = shortWait
    stateToLimit (SingObjectIds (SingObjectIdsBlocking SingCanAwait)) = shortWait
    stateToLimit (SingObjectIds (SingObjectIdsBlocking SingMustReply)) = longWait
    stateToLimit SingObjects = shortWait
    stateToLimit SingIdle = waitForever
    stateToLimit a@SingDone = notActiveState a

codecObjectDiffusion
  :: forall (objectId :: Type) (object :: Type) m.
     MonadST m
  => (objectId -> CBOR.Encoding)         -- ^ encode 'objectId'
  -> (forall s. CBOR.Decoder s objectId) -- ^ decode 'objectId'
  -> (object   -> CBOR.Encoding)         -- ^ encode object
  -> (forall s. CBOR.Decoder s object)   -- ^ decode object
  -> Codec (ObjectDiffusion objectId object) CBOR.DeserialiseFailure m ByteString
codecObjectDiffusion encodeObjectId decodeObjectId encodeObject decodeObject =
  mkCodecCborLazyBS
    (encodeObjectDiffusion encodeObjectId encodeObject)
    decode
    where
      decode
        :: forall (st :: ObjectDiffusion objectId object).
           ActiveState st
        => StateToken st
        -> forall s. CBOR.Decoder s (SomeMessage st)
      decode stok = do
        len <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        decodeObjectDiffusion decodeObjectId decodeObject stok len key

encodeObjectDiffusion
  :: forall (objectId :: Type) (object :: Type)
            (st  :: ObjectDiffusion objectId object)
            (st' :: ObjectDiffusion objectId object).
     (objectId -> CBOR.Encoding) -- ^ encode 'objectId'
  -> (object   -> CBOR.Encoding) -- ^ encode 'object'
  -> Message (ObjectDiffusion objectId object) st st'
  -> CBOR.Encoding
encodeObjectDiffusion encodeObjectId encodeObject = encode
  where
    encode
      :: forall st0 st1.
         Message (ObjectDiffusion objectId object) st0 st1
      -> CBOR.Encoding
    encode MsgInit =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 0
    encode (MsgRequestObjectIds requestKind (NumObjectIdsAck ackNo) (NumObjectIdsReq reqNo)) =
         CBOR.encodeListLen 4
      <> CBOR.encodeWord 1
      <> CBOR.encodeBool
           ( case requestKind of
               RequestObjectIdsBlocking    -> True
               RequestObjectIdsNonBlocking -> False
           )
      <> CBOR.encodeWord16 ackNo
      <> CBOR.encodeWord16 reqNo
    encode (MsgReplyObjectIds objIds) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 2
      <> CBOR.encodeListLenIndef
      <> foldMap encodeObjectId objIds
      <> CBOR.encodeBreak
    encode (MsgRequestObjects objIds) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 3
      <> CBOR.encodeListLenIndef
      <> foldMap encodeObjectId objIds
      <> CBOR.encodeBreak
    encode (MsgReplyObjects objects) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 4
      <> CBOR.encodeListLenIndef
      <> foldMap encodeObject objects
      <> CBOR.encodeBreak
    encode MsgDone =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 5
    encode MsgServerIdle =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 6
    encode MsgAwaitReply =
         CBOR.encodeListLen 1
      <> CBOR.encodeWord 7

decodeObjectDiffusion
  :: forall (objectId :: Type) (object :: Type)
            (st :: ObjectDiffusion objectId object) s.
     ActiveState st
  => (forall s'. CBOR.Decoder s' objectId) -- ^ decode 'objectId'
  -> (forall s'. CBOR.Decoder s' object)   -- ^ decode object
  -> StateToken st
  -> Int
  -> Word
  -> CBOR.Decoder s (SomeMessage st)
decodeObjectDiffusion decodeObjectId decodeObject = decode
  where
    decode
      :: forall (st' :: ObjectDiffusion objectId object).
         ActiveState st'
      => StateToken st'
      -> Int
      -> Word
      -> CBOR.Decoder s (SomeMessage st')
    decode stok len key = do
      case (stok, len, key) of
        (SingInit, 1, 0) ->
          return $ SomeMessage MsgInit
        (SingIdle, 4, 1) -> do
          blocking <- CBOR.decodeBool
          ackNo <- NumObjectIdsAck <$> CBOR.decodeWord16
          reqNo <- NumObjectIdsReq <$> CBOR.decodeWord16
          return $! if blocking
            then SomeMessage $ MsgRequestObjectIds RequestObjectIdsBlocking ackNo reqNo
            else SomeMessage $ MsgRequestObjectIds RequestObjectIdsNonBlocking ackNo reqNo
        (SingObjectIds kind, 2, 2) -> withSingI kind $ do
          CBOR.decodeListLenIndef
          objIds <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      decodeObjectId
          case (kind, objIds) of
            (SingObjectIdsBlocking _, t : ts) ->
              return
                $ SomeMessage
                $ MsgReplyObjectIds (BlockingReply (t NonEmpty.:| ts))
            (SingObjectIdsNonBlocking, ts) ->
              return
                $ SomeMessage
                $ MsgReplyObjectIds (NonBlockingReply ts)
            (SingObjectIdsBlocking _, []) ->
              fail "codecObjectDiffusion: MsgReplyObjectIds: empty list not permitted"
        (SingIdle, 2, 3) -> do
          CBOR.decodeListLenIndef
          objIds <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      decodeObjectId
          return $ SomeMessage $ MsgRequestObjects objIds
        (SingObjects, 2, 4) -> do
          CBOR.decodeListLenIndef
          objIds <- CBOR.decodeSequenceLenIndef
                      (flip (:))
                      []
                      reverse
                      decodeObject
          return $ SomeMessage $ MsgReplyObjects objIds
        (SingIdle, 1, 5) ->
          return $ SomeMessage MsgDone
        (SingObjectIds (SingObjectIdsBlocking SingMustReply), 1, 6) ->
          return $ SomeMessage MsgServerIdle
        (SingObjectIds (SingObjectIdsBlocking SingCanAwait), 1, 7) ->
          return $ SomeMessage MsgAwaitReply
        (SingDone, _, _) -> notActiveState stok
        -- failures per protocol state
        (SingInit, _, _) ->
          fail (formatToString fmterr (activeAgency :: ActiveAgency st') stok key len)
        (SingObjectIds _, _, _) ->
          fail (formatToString fmterr (activeAgency :: ActiveAgency st') stok key len)
        (SingObjects, _, _) ->
          fail (formatToString fmterr (activeAgency :: ActiveAgency st') stok key len)
        (SingIdle, _, _) ->
          fail (formatToString fmterr (activeAgency :: ActiveAgency st') stok key len)

    fmterr :: forall (st' :: ObjectDiffusion objectId object) r.
              F.Format r (ActiveAgency st' -> StateToken st' -> Word -> Int -> r)
    fmterr = "codecObjectDiffusion"
          %+ F.parenthesised (F.shown F.% "," %+ F.shown)
          %+ "unexpected key"
          %+ F.parenthesised (F.int F.% "," %+ F.int)

codecObjectDiffusionId
  :: forall objectId object m.
     Monad m
  => Codec
       (ObjectDiffusion objectId object)
       CodecFailure
       m
       (AnyMessage (ObjectDiffusion objectId object))
codecObjectDiffusionId = Codec {encode, decode}
  where
    encode
      :: forall st st'.
         ( ActiveState st
         , StateTokenI st
         )
      => Message (ObjectDiffusion objectId object) st st'
      -> AnyMessage (ObjectDiffusion objectId object)
    encode = AnyMessage

    decode
      :: forall (st :: ObjectDiffusion objectId object).
         ActiveState st
      => StateToken st
      -> m (DecodeStep
             (AnyMessage (ObjectDiffusion objectId object))
             CodecFailure
             m
             (SomeMessage st)
           )
    decode stok = return $ DecodePartial $ \bytes ->
      return $ case (stok, bytes) of
        (SingInit, Just (AnyMessage msg@MsgInit)) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingIdle, Just (AnyMessage msg@(MsgRequestObjectIds RequestObjectIdsBlocking _ _))) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingIdle, Just (AnyMessage msg@(MsgRequestObjectIds RequestObjectIdsNonBlocking _ _))) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingIdle, Just (AnyMessage msg@(MsgRequestObjects {}))) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingObjects, Just (AnyMessage msg@(MsgReplyObjects {}))) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingObjectIds kind, Just (AnyMessage msg)) -> withSingI kind $
          case (kind, msg) of
            (SingObjectIdsBlocking _, MsgReplyObjectIds (BlockingReply objectIds)) ->
              DecodeDone
                (SomeMessage (MsgReplyObjectIds (BlockingReply objectIds)))
                Nothing
            (SingObjectIdsBlocking SingCanAwait, MsgAwaitReply) ->
              DecodeDone (SomeMessage MsgAwaitReply) Nothing
            (SingObjectIdsBlocking SingMustReply, MsgServerIdle) ->
              DecodeDone (SomeMessage MsgServerIdle) Nothing
            (SingObjectIdsNonBlocking, MsgReplyObjectIds (NonBlockingReply objectIds)) ->
              DecodeDone
                (SomeMessage (MsgReplyObjectIds (NonBlockingReply objectIds)))
                Nothing
            (_, _) ->
              DecodeFail $ CodecFailure "codecObjectDiffusionId: no matching message"
        (SingIdle, Just (AnyMessage msg@MsgDone)) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingDone, _) ->
          notActiveState stok
        (_, _) ->
          DecodeFail $ CodecFailure "codecObjectDiffusionId: no matching message"
