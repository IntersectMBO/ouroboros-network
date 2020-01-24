{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Codec (
    codecLocalStateQuery
  , codecLocalStateQueryId
  ) where

import           Control.Monad.Class.MonadST

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Data.ByteString.Lazy (ByteString)

import           Network.TypedProtocol.Codec.Cbor

import           Ouroboros.Network.Protocol.LocalStateQuery.Type

import           Ouroboros.Network.Block (Point)

codecLocalStateQuery
  :: forall block query result m.
     MonadST m
  => (Point block -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s (Point block))
  -> (query -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s query)
  -> (result -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s result)
  -> Codec (LocalStateQuery block query result) CBOR.DeserialiseFailure m ByteString
codecLocalStateQuery encodePoint  decodePoint
                     encodeQuery  decodeQuery
                     encodeResult decodeResult =
    mkCodecCborLazyBS encode decode
  where
    encodeFailure :: AcquireFailure -> CBOR.Encoding
    encodeFailure AcquireFailurePointTooOld     = CBOR.encodeWord8 0
    encodeFailure AcquireFailurePointNotOnChain = CBOR.encodeWord8 1

    decodeFailure :: forall s. CBOR.Decoder s AcquireFailure
    decodeFailure = do
      tag <- CBOR.decodeWord8
      case tag of
        0 -> return AcquireFailurePointTooOld
        1 -> return AcquireFailurePointNotOnChain
        _ -> fail $ "decodeFailure: invalid tag " <> show tag

    encode :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (LocalStateQuery block query result) st st'
           -> CBOR.Encoding
    encode (ClientAgency TokIdle) (MsgAcquire pt) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> encodePoint pt

    encode (ServerAgency TokAcquiring) MsgAcquired =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 1

    encode (ServerAgency TokAcquiring) (MsgFailure failure) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> encodeFailure failure

    encode (ClientAgency TokAcquired) (MsgQuery query) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 3
     <> encodeQuery query

    encode (ServerAgency TokQuerying) (MsgResult result) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 4
     <> encodeResult result

    encode (ClientAgency TokAcquired) MsgRelease =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 5

    encode (ClientAgency TokAcquired) (MsgReAcquire pt) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 6
     <> encodePoint pt

    encode (ClientAgency TokIdle) MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 7

    decode :: forall (pr :: PeerRole) s (st :: LocalStateQuery block query result).
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (ClientAgency TokIdle, 2, 0) -> do
          pt <- decodePoint
          return (SomeMessage (MsgAcquire pt))

        (ServerAgency TokAcquiring, 1, 1) ->
          return (SomeMessage MsgAcquired)

        (ServerAgency TokAcquiring, 2, 2) -> do
          failure <- decodeFailure
          return (SomeMessage (MsgFailure failure))

        (ClientAgency TokAcquired, 2, 3) -> do
          query <- decodeQuery
          return (SomeMessage (MsgQuery query))

        (ServerAgency TokQuerying, 2, 4) -> do
          result <- decodeResult
          return (SomeMessage (MsgResult result))

        (ClientAgency TokAcquired, 1, 5) ->
          return (SomeMessage MsgRelease)

        (ClientAgency TokAcquired, 2, 6) -> do
          pt <- decodePoint
          return (SomeMessage (MsgReAcquire pt))

        (ClientAgency TokIdle, 1, 7) ->
          return (SomeMessage MsgDone)

        (ClientAgency TokIdle, _, _) ->
          fail "codecLocalStateQuery.Idle: unexpected key"
        (ClientAgency TokAcquired, _, _) ->
          fail "codecLocalStateQuery.Acquired: unexpected key"
        (ServerAgency TokAcquiring, _, _) ->
          fail "codecLocalStateQuery.Acquiring: unexpected key"
        (ServerAgency TokQuerying, _, _) ->
          fail "codecLocalStateQuery.Querying: unexpected key"


-- | An identity 'Codec' for the 'LocalStateQuery' protocol. It does not do
-- any serialisation. It keeps the typed messages, wrapped in 'AnyMessage'.
--
codecLocalStateQueryId
  :: forall block query result m.
     Monad m
  => Codec (LocalStateQuery block query result)
           CodecFailure m
           (AnyMessage (LocalStateQuery block query result))
codecLocalStateQueryId =
  Codec encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (LocalStateQuery block query result) st st'
         -> AnyMessage (LocalStateQuery block query result)
  encode _ = AnyMessage

  decode :: forall (pr :: PeerRole) st.
            PeerHasAgency pr st
         -> m (DecodeStep (AnyMessage (LocalStateQuery block query result))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of
    (ClientAgency TokIdle,      Just (AnyMessage msg@(MsgAcquire{})))   -> res msg
    (ClientAgency TokIdle,      Just (AnyMessage msg@(MsgDone{})))      -> res msg
    (ClientAgency TokAcquired,  Just (AnyMessage msg@(MsgQuery{})))     -> res msg
    (ClientAgency TokAcquired,  Just (AnyMessage msg@(MsgReAcquire{}))) -> res msg
    (ClientAgency TokAcquired,  Just (AnyMessage msg@(MsgRelease{})))   -> res msg
    (ServerAgency TokAcquiring, Just (AnyMessage msg@(MsgAcquired{})))  -> res msg
    (ServerAgency TokAcquiring, Just (AnyMessage msg@(MsgFailure{})))   -> res msg
    (ServerAgency TokQuerying,  Just (AnyMessage msg@(MsgResult{})))    -> res msg
    (_, Nothing) -> return (DecodeFail CodecFailureOutOfInput)
    (_, _)       -> return (DecodeFail (CodecFailure failmsg))

  res :: Message (LocalStateQuery block query result) st st' -> m (DecodeStep bytes failure m (SomeMessage st))
  res msg = return (DecodeDone (SomeMessage msg) Nothing)
  failmsg = "codecLocalStateQueryId: no matching message"
