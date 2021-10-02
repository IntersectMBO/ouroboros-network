{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

module Ouroboros.Network.Protocol.LocalStateQuery.Codec
  ( codecLocalStateQuery
  , codecLocalStateQueryId
  , Some (..)
  ) where

import           Control.Monad.Class.MonadST

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Kind (Type)
import           Data.Singletons.Decide
import           Text.Printf

import           Network.TypedProtocol.Codec (AnyMessage (..),
                     CodecFailure (..), DecodeStep (..), SomeMessage (..))
import           Network.TypedProtocol.Core
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import qualified Network.TypedProtocol.Stateful.Codec.CBOR as Stateful

import           Ouroboros.Network.Protocol.LocalStateQuery.Type



data Some (f :: k -> Type) where
    Some :: f a -> Some f

codecLocalStateQuery
  :: forall block point query m.
     ( MonadST m
     , ShowQuery query
     )
  => (point -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s point)
  -> (forall result . query result -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s (Some query))
  -> (forall result . query result -> result -> CBOR.Encoding)
  -> (forall result . query result -> forall s . CBOR.Decoder s result)
  -> Stateful.Codec (LocalStateQuery block point query) CBOR.DeserialiseFailure State m ByteString
codecLocalStateQuery encodePoint  decodePoint
                     encodeQuery  decodeQuery
                     encodeResult decodeResult =
    Stateful.mkCodecCborLazyBS encodeMsg decodeMsg
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

    encodeMsg :: forall (st  :: LocalStateQuery block point query)
                        (st' :: LocalStateQuery block point query).
                 State st'
              -> Message (LocalStateQuery block point query) st st'
              -> CBOR.Encoding
    encodeMsg _ (MsgAcquire (Just pt)) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> encodePoint pt

    encodeMsg _ (MsgAcquire Nothing) =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 8

    encodeMsg _ MsgAcquired =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 1

    encodeMsg _ (MsgFailure failure) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> encodeFailure failure

    encodeMsg _ (MsgQuery query) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 3
     <> encodeQuery query

    encodeMsg _ (MsgResult query result) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 4
     <> encodeResult query result

    encodeMsg _ MsgRelease =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 5

    encodeMsg _ (MsgReAcquire (Just pt)) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 6
     <> encodePoint pt

    encodeMsg _ (MsgReAcquire Nothing) =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 9

    encodeMsg _ MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 7

    decodeMsg :: forall s (st :: LocalStateQuery block point query).
                 ActiveState st
              => StateToken st
              -> State st
              -> CBOR.Decoder s (SomeMessage st)
    decodeMsg stok f = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, f, len, key) of
        (SingIdle, _, 2, 0) ->
          SomeMessage . MsgAcquire . Just <$> decodePoint

        (SingIdle, _, 1, 8) -> do
          return (SomeMessage (MsgAcquire Nothing))

        (SingAcquiring, _, 1, 1) ->
          return (SomeMessage MsgAcquired)

        (SingAcquiring, _, 2, 2) ->
          SomeMessage . MsgFailure <$> decodeFailure

        (SingAcquired, _, 2, 3) -> do
          Some query <- decodeQuery
          return $ SomeMessage (MsgQuery query)

        (SingQuerying, StateQuerying query, 2, 4) -> do
          result <- decodeResult query
          return $ SomeMessage (MsgResult query result)

        (SingAcquired, _, 1, 5) ->
          return (SomeMessage MsgRelease)

        (SingAcquired, _, 2, 6) ->
          SomeMessage . MsgReAcquire . Just <$> decodePoint

        (SingAcquired, _, 1, 9) -> do
          return (SomeMessage (MsgReAcquire Nothing))

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

        (SingQuerying, _, _, _) ->
          fail (printf "codecLocalStateQuery (%s) unexpected key (%d, %d)" (show stok) key len)

        (SingDone, _, _, _) -> notActiveState stok


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
  -> Stateful.Codec (LocalStateQuery block point query)
           CodecFailure
           State m
           (AnyMessage (LocalStateQuery block point query))
codecLocalStateQueryId eqQuery =
  Stateful.Codec encodeMsg decodeMsg
 where
  encodeMsg :: forall st st'.
            ActiveState st
         => StateTokenI st
         => State st'
         -> Message (LocalStateQuery block point query) st st'
         -> AnyMessage (LocalStateQuery block point query)
  encodeMsg _ = AnyMessage

  decodeMsg :: forall (st :: LocalStateQuery block point query).
               ActiveState st
            => StateToken st
            -> State st
            -> m (DecodeStep (AnyMessage (LocalStateQuery block point query))
                             CodecFailure m (SomeMessage st))
  decodeMsg stok f = return $ DecodePartial $ \bytes ->
    case (stok, f, bytes) of
      (SingIdle,      _, Just (AnyMessage msg@MsgAcquire{}))   -> rewrapMsg msg
      (SingIdle,      _, Just (AnyMessage msg@MsgDone{}))      -> rewrapMsg msg
      (SingAcquired,  _, Just (AnyMessage msg@(MsgQuery {})))  -> rewrapMsg msg
      (SingAcquired,  _, Just (AnyMessage msg@MsgReAcquire{})) -> rewrapMsg msg
      (SingAcquired,  _, Just (AnyMessage msg@MsgRelease{}))   -> rewrapMsg msg
      (SingAcquiring, _, Just (AnyMessage msg@MsgAcquired{}))  -> rewrapMsg msg
      (SingAcquiring, _, Just (AnyMessage msg@MsgFailure{}))   -> rewrapMsg msg
      (SingQuerying, StateQuerying q, Just (AnyMessage msg@(MsgResult query _)))
          | Just Refl <- eqQuery q query
          -> rewrapMsg msg

      (SingDone, _, _) -> notActiveState stok
      (_, _, Nothing) -> return (DecodeFail CodecFailureOutOfInput)
      (_, _, _)       -> return (DecodeFail (CodecFailure failmsg))

  failmsg = "codecLocalStateQueryId: no matching message"

rewrapMsg :: forall block point query
                    (st  :: LocalStateQuery block point query)
                    (st' :: LocalStateQuery block point query)
                    m bytes failure.
             ( Monad m
             , StateTokenI st
             , StateTokenI st'
             , ActiveState st
             )
          => Message (LocalStateQuery block point query) st st'
          -> m (DecodeStep bytes failure m (SomeMessage st))
rewrapMsg msg = return (DecodeDone (SomeMessage msg) Nothing)
