{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Network.Protocol.LocalStateQuery.Codec
  ( codecLocalStateQuery
  , codecLocalStateQueryId
  , Some (..)
  ) where

import Control.Monad.Class.MonadST

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.Singletons.Decide
import Text.Printf

import Network.TypedProtocol.Codec (CodecFailure (..), DecodeStep (..),
           SomeMessage (..))
import Network.TypedProtocol.Core
import Network.TypedProtocol.Stateful.Codec qualified as Stateful
import Network.TypedProtocol.Stateful.Codec.CBOR qualified as Stateful

import Ouroboros.Network.NodeToClient.Version qualified as NodeToClient
import Ouroboros.Network.Protocol.LocalStateQuery.Type


data Some (f :: k -> Type) where
    Some :: f a -> Some f

codecLocalStateQuery
  :: forall block point query m.
     ( MonadST m
     , ShowQuery query
     )
  => NodeToClient.Version
     -- ^ eg whether to allow 'ImmutableTip' in @'MsgAcquire'
  -> (point -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s point)
  -> (forall result . query result -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s (Some query))
  -> (forall result . query result -> result -> CBOR.Encoding)
  -> (forall result . query result -> forall s . CBOR.Decoder s result)
  -> Stateful.Codec (LocalStateQuery block point query) CBOR.DeserialiseFailure State m ByteString
codecLocalStateQuery version
                     encodePoint  decodePoint
                     encodeQuery  decodeQuery
                     encodeResult decodeResult =
    Stateful.mkCodecCborLazyBS encode decode
  where
    canAcquireImmutable = version >= NodeToClient.V_16

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

    encode :: forall (st  :: LocalStateQuery block point query)
                     (st' :: LocalStateQuery block point query).
              State st
           -> Message (LocalStateQuery block point query) st st'
           -> CBOR.Encoding
    encode _ (MsgAcquire (SpecificPoint pt)) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> encodePoint pt

    encode _ (MsgAcquire VolatileTip) =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 8

    encode _ (MsgAcquire ImmutableTip)
      | canAcquireImmutable =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 10
      | otherwise =
      error $ "encodeFailure: local state query: acquiring the immutable tip "
           ++ "must be conditional on negotiating v16 of the node-to-client "
           ++ "protocol"

    encode _ MsgAcquired =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 1

    encode _ (MsgFailure failure) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> encodeFailure failure

    encode _ (MsgQuery query) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 3
     <> encodeQuery query

    encode (StateQuerying query) (MsgResult result) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 4
     <> encodeResult query result

    encode _ MsgRelease =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 5

    encode _ (MsgReAcquire (SpecificPoint pt)) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 6
     <> encodePoint pt

    encode _ (MsgReAcquire VolatileTip) =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 9

    encode _ (MsgReAcquire ImmutableTip)
      | canAcquireImmutable =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 11
      | otherwise =
      error $ "encodeFailure: local state query: re-acquiring the immutable "
           ++ "tip must be conditional on negotiating v16 of the "
           ++ "node-to-client protocol"

    encode _ MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 7

    decode :: forall s (st :: LocalStateQuery block point query).
              ActiveState st
           => StateToken st
           -> State st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok f = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, f, len, key) of
        (SingIdle, _, 2, 0) -> do
          pt <- decodePoint
          return (SomeMessage (MsgAcquire (SpecificPoint pt)))

        (SingIdle, _, 1, 8) -> do
          return (SomeMessage (MsgAcquire VolatileTip))

        (SingIdle, _, 1, 10) -> do
          return (SomeMessage (MsgAcquire ImmutableTip))

        (SingAcquiring, _, 1, 1) ->
          return (SomeMessage MsgAcquired)

        (SingAcquiring, _, 2, 2) -> do
          failure <- decodeFailure
          return (SomeMessage (MsgFailure failure))

        (SingAcquired, _, 2, 3) -> do
          Some query <- decodeQuery
          return (SomeMessage (MsgQuery query))

        (SingQuerying, StateQuerying query, 2, 4) -> do
          result <- decodeResult query
          return (SomeMessage (MsgResult result))

        (SingAcquired, _, 1, 5) ->
          return (SomeMessage MsgRelease)

        (SingAcquired, _, 2, 6) -> do
          pt <- decodePoint
          return (SomeMessage (MsgReAcquire (SpecificPoint pt)))

        (SingAcquired, _, 1, 9) -> do
          return (SomeMessage (MsgReAcquire VolatileTip))

        (SingAcquired, _, 1, 11) -> do
          return (SomeMessage (MsgReAcquire ImmutableTip))

        (SingIdle, _, 1, 7) ->
          return (SomeMessage MsgDone)

        --
        -- failures per protocol state
        --

        (SingIdle, _, _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingAcquired, _, _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingAcquiring, _, _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingQuerying {}, _, _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)

        (SingDone, _, _, _) -> notActiveState stok


-- | An identity 'Codec' for the 'LocalStateQuery' protocol. It does not do
-- any serialisation. It keeps the typed messages, wrapped in 'AnyMessage'.
--
codecLocalStateQueryId
  :: forall (block :: Type) (point :: Type) (query :: Type -> Type) m.
     Monad m
     => (forall (result1 :: Type) (result2 :: Type).
          query result1
       -> query result2
       -> Maybe (result1 :~: result2)
     )
  -> Stateful.Codec (LocalStateQuery block point query)
                    CodecFailure State m
                    (Stateful.AnyMessage (LocalStateQuery block point query) State)
codecLocalStateQueryId eqQuery =
   Stateful.Codec { Stateful.encode, Stateful.decode }
 where
   encode :: forall st st'.
             ActiveState st
          => StateTokenI st
          => State st
          -> Message (LocalStateQuery block point query) st st'
          -> Stateful.AnyMessage (LocalStateQuery block point query) State
   encode = Stateful.AnyMessage

   decode :: forall (st :: LocalStateQuery block point query).
             ActiveState st
          => StateToken st
          -> State st
          -> m (DecodeStep (Stateful.AnyMessage (LocalStateQuery block point query) State)
                           CodecFailure m (SomeMessage st))
   decode stok f = return $ DecodePartial $ \bytes ->
     return $ case (stok, f, bytes) of
       (SingIdle,       _, Just (Stateful.AnyMessage _ msg@(MsgAcquire{})))   -> DecodeDone (SomeMessage msg) Nothing
       (SingIdle,       _, Just (Stateful.AnyMessage _ msg@(MsgDone{})))      -> DecodeDone (SomeMessage msg) Nothing
       (SingAcquired,   _, Just (Stateful.AnyMessage _ msg@(MsgQuery{})))     -> DecodeDone (SomeMessage msg) Nothing
       (SingAcquired,   _, Just (Stateful.AnyMessage _ msg@(MsgReAcquire{}))) -> DecodeDone (SomeMessage msg) Nothing
       (SingAcquired,   _, Just (Stateful.AnyMessage _ msg@(MsgRelease{})))   -> DecodeDone (SomeMessage msg) Nothing
       (SingAcquiring,  _, Just (Stateful.AnyMessage _ msg@(MsgAcquired{})))  -> DecodeDone (SomeMessage msg) Nothing
       (SingAcquiring,  _, Just (Stateful.AnyMessage _ msg@(MsgFailure{})))   -> DecodeDone (SomeMessage msg) Nothing
       (SingQuerying,  StateQuerying q, Just (Stateful.AnyMessage (StateQuerying q') msg@(MsgResult _)))
            |  Just Refl <- q `eqQuery` q'
            -> DecodeDone (SomeMessage msg) Nothing
       (SingDone, _, _) -> notActiveState stok
       (_, _, Nothing) -> DecodeFail CodecFailureOutOfInput
       (_, _, _)       -> DecodeFail (CodecFailure failmsg)

   failmsg = "codecLocalStateQueryId: no matching message"
