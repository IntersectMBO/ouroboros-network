{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Ouroboros.Network.Protocol.LocalStateQuery.Codec (
    codecLocalStateQuery
  , codecLocalStateQueryId
  , Some (..)
  ) where

import           Control.Monad.Class.MonadST

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Kind (Type)
import           Data.Type.Equality ((:~:) (..))
import           Text.Printf

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Protocol.LocalStateQuery.Type


data Some (f :: k -> Type) where
    Some :: f a -> Some f

codecLocalStateQuery
  :: forall block point query m.
     ( MonadST m
     , ShowQuery query
     )
  => Bool -- allow @Maybe 'Point'@ in 'MsgAcquire' and 'MsgReAcquire'.
  -> (point -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s point)
  -> (forall result . query result -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s (Some query))
  -> (forall result . query result -> result -> CBOR.Encoding)
  -> (forall result . query result -> forall s . CBOR.Decoder s result)
  -> Codec (LocalStateQuery block point query) CBOR.DeserialiseFailure m ByteString
codecLocalStateQuery canAcquireTip
                     encodePoint  decodePoint
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

    encode :: forall (pr  :: PeerRole)
                     (st  :: LocalStateQuery block point query)
                     (st' :: LocalStateQuery block point query).
              PeerHasAgency pr st
           -> Message (LocalStateQuery block point query) st st'
           -> CBOR.Encoding
    encode (ClientAgency TokIdle) (MsgAcquire (Just pt)) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> encodePoint pt

    encode (ClientAgency TokIdle) (MsgAcquire Nothing) | canAcquireTip =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 8

    encode (ClientAgency TokIdle) (MsgAcquire Nothing) =
      error $ "encodeFailure: local state query: using acquire without a "
           ++ "Point must be conditional on negotiating v8 of the "
           ++ "node-to-client protocol"

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

    encode (ServerAgency (TokQuerying _query)) (MsgResult query result) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 4
     <> encodeResult query result

    encode (ClientAgency TokAcquired) MsgRelease =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 5

    encode (ClientAgency TokAcquired) (MsgReAcquire (Just pt)) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 6
     <> encodePoint pt

    encode (ClientAgency TokAcquired) (MsgReAcquire Nothing) | canAcquireTip =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 9

    encode (ClientAgency TokAcquired) (MsgReAcquire Nothing) =
      error "encodeFailure: this version does not support re-acquiring tip"

    encode (ClientAgency TokIdle) MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 7

    decode :: forall (pr :: PeerRole) s (st :: LocalStateQuery block point query).
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (ClientAgency TokIdle, 2, 0) -> do
          pt <- decodePoint
          return (SomeMessage (MsgAcquire (Just pt)))

        (ClientAgency TokIdle, 1, 8) | canAcquireTip -> do
          return (SomeMessage (MsgAcquire Nothing))

        (ServerAgency TokAcquiring, 1, 1) ->
          return (SomeMessage MsgAcquired)

        (ServerAgency TokAcquiring, 2, 2) -> do
          failure <- decodeFailure
          return (SomeMessage (MsgFailure failure))

        (ClientAgency TokAcquired, 2, 3) -> do
          Some query <- decodeQuery
          return (SomeMessage (MsgQuery query))

        (ServerAgency (TokQuerying query), 2, 4) -> do
          result <- decodeResult query
          return (SomeMessage (MsgResult query result))

        (ClientAgency TokAcquired, 1, 5) ->
          return (SomeMessage MsgRelease)

        (ClientAgency TokAcquired, 2, 6) -> do
          pt <- decodePoint
          return (SomeMessage (MsgReAcquire (Just pt)))

        (ClientAgency TokAcquired, 1, 9) | canAcquireTip -> do
          return (SomeMessage (MsgReAcquire Nothing))

        (ClientAgency TokIdle, 1, 7) ->
          return (SomeMessage MsgDone)

        --
        -- failures per protocol state
        --

        (ClientAgency TokIdle, _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)
        (ClientAgency TokAcquired, _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)
        (ServerAgency TokAcquiring, _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)
        (ServerAgency (TokQuerying _), _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)


-- | An identity 'Codec' for the 'LocalStateQuery' protocol. It does not do
-- any serialisation. It keeps the typed messages, wrapped in 'AnyMessage'.
--
codecLocalStateQueryId
  :: forall block point (query :: Type -> Type) m.
     Monad m
  => (forall result1 result2.
          query result1
       -> query result2
       -> Maybe (result1 :~: result2)
     )
  -> Codec (LocalStateQuery block point query)
           CodecFailure m
           (AnyMessage (LocalStateQuery block point query))
codecLocalStateQueryId eqQuery =
  Codec encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (LocalStateQuery block point query) st st'
         -> AnyMessage (LocalStateQuery block point query)
  encode _ = AnyMessage

  decode :: forall (pr :: PeerRole) (st :: LocalStateQuery block point query).
            PeerHasAgency pr st
         -> m (DecodeStep (AnyMessage (LocalStateQuery block point query))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of
    (ClientAgency TokIdle,         Just (AnyMessage msg@(MsgAcquire{})))   -> res msg
    (ClientAgency TokIdle,         Just (AnyMessage msg@(MsgDone{})))      -> res msg
    (ClientAgency TokAcquired,     Just (AnyMessage msg@(MsgQuery{})))     -> res msg
    (ClientAgency TokAcquired,     Just (AnyMessage msg@(MsgReAcquire{}))) -> res msg
    (ClientAgency TokAcquired,     Just (AnyMessage msg@(MsgRelease{})))   -> res msg
    (ServerAgency TokAcquiring,    Just (AnyMessage msg@(MsgAcquired{})))  -> res msg
    (ServerAgency TokAcquiring,    Just (AnyMessage msg@(MsgFailure{})))   -> res msg
    (ServerAgency (TokQuerying q), Just (AnyMessage msg@(MsgResult query _)))
       | Just Refl <- eqQuery q query
       -> res msg
    (_, Nothing) -> return (DecodeFail CodecFailureOutOfInput)
    (_, _)       -> return (DecodeFail (CodecFailure failmsg))

  res :: Message (LocalStateQuery block point query) st st'
      -> m (DecodeStep bytes failure m (SomeMessage st))
  res msg = return (DecodeDone (SomeMessage msg) Nothing)
  failmsg = "codecLocalStateQueryId: no matching message"
