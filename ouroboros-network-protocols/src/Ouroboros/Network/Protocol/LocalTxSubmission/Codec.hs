{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Network.Protocol.LocalTxSubmission.Codec
  ( codecLocalTxSubmission
  , anncodecLocalTxSubmission
  , anncodecLocalTxSubmission'
  , codecLocalTxSubmissionId
  , WithBytes (..)
  ) where

import Control.Monad.Class.MonadST

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Data.Constraint
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Type.Equality
import Text.Printf

import Network.TypedProtocol.Codec.CBOR

import Ouroboros.Network.Protocol.Codec.Utils (WithByteSpan (..),
           WithBytes (..), encodeWithBytes)
import Ouroboros.Network.Protocol.Codec.Utils qualified as Utils
import Ouroboros.Network.Protocol.LocalTxSubmission.Type


anncodecLocalTxSubmission
  :: forall tx reject m.
     MonadST m
  => (forall s . CBOR.Decoder s (ByteString -> tx))
  -- ^ decode transaction
  -> (reject -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s reject)
  -> AnnotatedCodec (LocalTxSubmission (WithBytes tx) reject) CBOR.DeserialiseFailure m ByteString
anncodecLocalTxSubmission              decodeTx
                          encodeReject decodeReject
    =
    anncodecLocalTxSubmission' WithBytes encodeTx     decodeTx
                                         encodeReject decodeReject
  where
    encodeTx :: WithBytes tx -> CBOR.Encoding
    encodeTx = encodeWithBytes


anncodecLocalTxSubmission'
  :: forall tx reject txWithBytes m.
     MonadST m
  => (ByteString -> tx -> txWithBytes)
  -> (txWithBytes -> CBOR.Encoding)
  -- ^ encode 'tx'
  -> (forall s. CBOR.Decoder s (ByteString -> tx))
  -- ^ decode 'tx'
  -> (reject -> CBOR.Encoding)
  -- ^ encode rejection reason
  -> (forall s. CBOR.Decoder s reject)
  -- ^ decode rejection reason
  -> AnnotatedCodec (LocalTxSubmission txWithBytes reject) CBOR.DeserialiseFailure m ByteString
anncodecLocalTxSubmission' mkWithBytes encodeTx     decodeTx
                                       encodeReject decodeReject
    =
    mkCodecCborLazyBS
      (encodeLocalTxSubmission encodeTx encodeReject)
      decode
  where
    decode :: forall (st :: LocalTxSubmission txWithBytes reject).
              ActiveState st
           => StateToken st
           -> forall s. CBOR.Decoder s (Annotator ByteString st)
    decode =
      decodeLocalTxSubmission @tx
                              @txWithBytes
                              @WithByteSpan
                              @ByteString
                              @reject
                              mkWithBytes'
                              Utils.decodeWithByteSpan
                              decodeTx decodeReject

    mkWithBytes' :: ByteString
                 -> WithByteSpan (ByteString -> tx)
                 -> txWithBytes
    mkWithBytes' bytes (WithByteSpan (fn, start, end)) =
      mkWithBytes (Utils.bytesBetweenOffsets start end bytes) (fn bytes)


encodeLocalTxSubmission
  :: forall tx reject (st :: LocalTxSubmission tx reject) (st' :: LocalTxSubmission tx reject).
     (tx -> CBOR.Encoding)
  -> (reject -> CBOR.Encoding)
  -> Message (LocalTxSubmission tx reject) st st'
  -> CBOR.Encoding
encodeLocalTxSubmission encodeTx encodeReject = encode
  where
    encode :: forall st0 st1.
              Message (LocalTxSubmission tx reject) st0 st1
           -> CBOR.Encoding
    encode (MsgSubmitTx tx) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> encodeTx tx

    encode MsgAcceptTx =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 1

    encode (MsgRejectTx reject) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> encodeReject reject

    encode MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 3


decodeLocalTxSubmission
  :: forall (tx           :: Type)
            (txWithBytes  :: Type)
            (withByteSpan :: Type -> Type)
            (bytes        :: Type)
            (reject       :: Type)
            (st           :: LocalTxSubmission txWithBytes reject)
            s.
     ActiveState st
  => (bytes -> withByteSpan (bytes -> tx) -> txWithBytes)
  -> (forall a s'. CBOR.Decoder s' a -> CBOR.Decoder s' (withByteSpan a))
  -> (forall s'. CBOR.Decoder s' (bytes -> tx))
  -- ^ decode transaction
  -> (forall s'. CBOR.Decoder s' reject)
  -- ^ decode rejection reason
  -> StateToken st
  -> CBOR.Decoder s (Annotator bytes st)
decodeLocalTxSubmission mkWithBytes decodeWithByteSpan decodeTx decodeReject stok
  = do
  len <- CBOR.decodeListLen
  key <- CBOR.decodeWord
  case (stok, len, key) of
    (SingIdle, 2, 0) -> do
      tx <- decodeWithByteSpan decodeTx
      return (Annotator $ \bytes -> SomeMessage (MsgSubmitTx $ mkWithBytes bytes tx))

    (SingBusy, 1, 1) ->
      return (Annotator $ \_ -> SomeMessage MsgAcceptTx)

    (SingBusy, 2, 2) -> do
      reject <- decodeReject
      return (Annotator $ \_ -> SomeMessage (MsgRejectTx reject))

    (SingIdle, 1, 3) ->
      return (Annotator $ \_ -> SomeMessage MsgDone)

    (SingDone, _, _) -> notActiveState stok

    (_, _, _) -> fail (printf "codecLocalTxSubmission (%s, %s) unexpected key (%d, %d)"
                              (show (activeAgency :: ActiveAgency st)) (show stok) key len)

--
-- Map protocol state & messages from `LocalTxSubmission (Identity tx) reject` to
-- `LocalTxSubmission tx reject`
--

type family FromIdentity (st :: LocalTxSubmission (Identity tx) reject) :: LocalTxSubmission tx reject where
  FromIdentity StIdle        = StIdle
  FromIdentity StBusy        = StBusy
  FromIdentity StDone        = StDone

type family ToIdentity (st :: LocalTxSubmission tx reject) :: LocalTxSubmission (Identity tx) reject where
  ToIdentity StIdle        = StIdle
  ToIdentity StBusy        = StBusy
  ToIdentity StDone        = StDone

singToIdentity
  :: forall tx reject (st :: LocalTxSubmission tx reject).
     StateToken st
  -> StateToken (ToIdentity st)
singToIdentity SingIdle = SingIdle
singToIdentity SingBusy = SingBusy
singToIdentity SingDone = SingDone

--
-- Proofs
--

-- | A proof that `FromIdentity` is a left inverse of `ToIdentity`.
--
proof_FromTo
  :: forall tx reject (st :: LocalTxSubmission tx reject).
     StateToken st
  -> FromIdentity (ToIdentity st) :~: st
proof_FromTo SingIdle = Refl
proof_FromTo SingBusy = Refl
proof_FromTo SingDone = Refl
{-# INLINE proof_FromTo #-}

-- | A proof that `ActiveState` constraint is preserved by `ToIdentity`.
--
proof_activeState
  :: forall tx reject (st :: LocalTxSubmission tx reject).
     StateToken st
  -> Dict (ActiveState st)
  -> Dict (ActiveState (ToIdentity st))
proof_activeState SingIdle      Dict = Dict
proof_activeState SingBusy      Dict = Dict
proof_activeState sing@SingDone Dict = notActiveState sing
{-# INLINE proof_activeState #-}

msgFromIdentity
  :: forall tx reject (st :: LocalTxSubmission (Identity tx) reject).
     SomeMessage st
  -> SomeMessage (FromIdentity st)
msgFromIdentity (SomeMessage (MsgSubmitTx tx))
              =  SomeMessage (MsgSubmitTx (runIdentity tx))
msgFromIdentity (SomeMessage MsgAcceptTx)
              =  SomeMessage MsgAcceptTx
msgFromIdentity (SomeMessage (MsgRejectTx reject))
              =  SomeMessage (MsgRejectTx reject)
msgFromIdentity (SomeMessage MsgDone)
              =  SomeMessage MsgDone
{-# INLINE msgFromIdentity #-}

codecLocalTxSubmission
  :: forall tx reject m.
     MonadST m
  => (tx -> CBOR.Encoding)
  -- ^ encode transaction
  -> (forall s . CBOR.Decoder s tx)
  -- ^ decode transaction
  -> (reject -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s reject)
  -> Codec (LocalTxSubmission tx reject) CBOR.DeserialiseFailure m ByteString
codecLocalTxSubmission encodeTx     decodeTx
                       encodeReject decodeReject
  = mkCodecCborLazyBS
      (encodeLocalTxSubmission encodeTx encodeReject)
      decode
  where
    decode :: forall (st :: LocalTxSubmission tx reject) s.
                         ActiveState st
                      => StateToken st
                      -> CBOR.Decoder s (SomeMessage st)
    decode sing =
      case proof_FromTo sing of { Refl ->
        case proof_activeState sing Dict of { Dict ->
          msgFromIdentity <$> decode' (singToIdentity sing)
        }
      }

    decode' :: forall (st :: LocalTxSubmission (Identity tx) reject).
              ActiveState st
           => StateToken st
           -> forall s. CBOR.Decoder s (SomeMessage st)
    decode' stok =
      mapAnnotator <$>
        decodeLocalTxSubmission @tx            -- tx without bytes
                                @(Identity tx) -- tx with bytes
                                @Identity      -- withByteSpan functor
                                @()            -- bytes
                                @reject
                                mkWithBytes
                                decodeWithByteSpan
                                (const <$> decodeTx)
                                decodeReject
                                stok

    mapAnnotator :: Annotator () st -> SomeMessage st
    mapAnnotator Annotator { runAnnotator } = runAnnotator ()

    mkWithBytes :: forall a. () -> Identity (() -> a) -> Identity a
    mkWithBytes _ (Identity f) = Identity (f ())

    decodeWithByteSpan :: CBOR.Decoder s a -> CBOR.Decoder s (Identity a)
    decodeWithByteSpan = fmap Identity


codecLocalTxSubmissionId
  :: forall tx reject m.
     Monad m
  => Codec (LocalTxSubmission tx reject)
            CodecFailure m
           (AnyMessage (LocalTxSubmission tx reject))
codecLocalTxSubmissionId =
    Codec encode decode
  where
    encode :: forall st st'.
              ActiveState st
           => StateTokenI st
           => Message (LocalTxSubmission tx reject) st st'
           -> AnyMessage (LocalTxSubmission tx reject)
    encode = AnyMessage

    decode :: forall (st :: LocalTxSubmission tx reject).
              ActiveState st
           => StateToken st
           -> m (DecodeStep (AnyMessage (LocalTxSubmission tx reject))
                            CodecFailure m (SomeMessage st))
    decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of
      (SingIdle, Just (AnyMessage msg@(MsgSubmitTx{}))) -> res msg
      (SingBusy, Just (AnyMessage msg@(MsgAcceptTx{}))) -> res msg
      (SingBusy, Just (AnyMessage msg@(MsgRejectTx{}))) -> res msg
      (SingIdle, Just (AnyMessage msg@(MsgDone{})))     -> res msg
      (SingDone, _)                                     -> notActiveState stok
      (_, Nothing) -> return (DecodeFail CodecFailureOutOfInput)
      (_, _)       -> return (DecodeFail (CodecFailure failmsg))
    res msg = return (DecodeDone (SomeMessage msg) Nothing)
    failmsg = "codecLocalTxSubmissionId: no matching message"
