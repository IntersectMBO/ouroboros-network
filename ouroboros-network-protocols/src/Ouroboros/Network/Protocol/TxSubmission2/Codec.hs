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

module Ouroboros.Network.Protocol.TxSubmission2.Codec
  ( codecTxSubmission2
  , anncodecTxSubmission2
  , anncodecTxSubmission2'
  , codecTxSubmission2Id
  , byteLimitsTxSubmission2
  , timeLimitsTxSubmission2
  , WithBytes (..)
  ) where

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI
import Data.ByteString.Lazy (ByteString)
import Data.Constraint
import Data.Functor.Identity
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Type.Equality
import Text.Printf

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Network.TypedProtocol.Codec.CBOR

import Ouroboros.Network.Protocol.Codec.Utils (WithByteSpan (..),
           WithBytes (..), encodeWithBytes)
import Ouroboros.Network.Protocol.Codec.Utils qualified as Utils
import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Protocol.TxSubmission2.Type

-- | Byte Limits.
byteLimitsTxSubmission2 :: forall bytes txid tx.
                           (bytes -> Word)
                        -> ProtocolSizeLimits (TxSubmission2 txid tx) bytes
byteLimitsTxSubmission2 = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (st :: TxSubmission2 txid tx).
                    ActiveState st => StateToken st -> Word
    stateToLimit SingInit                    = smallByteLimit
    stateToLimit (SingTxIds SingBlocking)    = largeByteLimit
    stateToLimit (SingTxIds SingNonBlocking) = largeByteLimit
    stateToLimit SingTxs                     = largeByteLimit
    stateToLimit SingIdle                    = smallByteLimit
    stateToLimit a@SingDone                  = notActiveState a


-- | 'TxSubmission2' time limits.
--
-- +-----------------------------+---------------+
-- | 'TxSubmission2' state       | timeout (s)   |
-- +=============================+===============+
-- | `StInit`                    | `waitForever` |
-- +-----------------------------+---------------+
-- | `StIdle`                    | `waitForever` |
-- +-----------------------------+---------------+
-- | @'StTxIds' 'StBlocking'@    | `waitForever` |
-- +-----------------------------+---------------+
-- | @'StTxIds' 'StNonBlocking'@ | `shortWait`   |
-- +-----------------------------+---------------+
-- | `StTxs`                     | `shortWait`   |
-- +-----------------------------+---------------+
--
timeLimitsTxSubmission2 :: forall (txid :: Type) (tx :: Type). ProtocolTimeLimits (TxSubmission2 txid tx)
timeLimitsTxSubmission2 = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st :: TxSubmission2 txid tx).
                    ActiveState st => StateToken st -> Maybe DiffTime
    stateToLimit SingInit                    = waitForever
    stateToLimit (SingTxIds SingBlocking)    = waitForever
    stateToLimit (SingTxIds SingNonBlocking) = shortWait
    stateToLimit SingTxs                     = shortWait
    stateToLimit SingIdle                    = waitForever
    stateToLimit a@SingDone                  = notActiveState a

-- | An 'AnnotatedCodec' paired with `WithBytes` functor.
--
anncodecTxSubmission2
  :: forall (txid :: Type) (tx :: Type) m.
     MonadST m
  => (txid -> CBOR.Encoding)
  -- ^ encode 'txid'
  -> (forall s . CBOR.Decoder s txid)
  -- ^ decode 'txid'
  -> (forall s . CBOR.Decoder s (ByteString -> tx))
  -- ^ decode transaction
  -> AnnotatedCodec (TxSubmission2 txid (WithBytes tx)) CBOR.DeserialiseFailure m ByteString
anncodecTxSubmission2 encodeTxId decodeTxId
                                 decodeTx
    =
    anncodecTxSubmission2' WithBytes encodeTxId decodeTxId
                                     encodeTx   decodeTx
  where
    encodeTx :: WithBytes tx -> CBOR.Encoding
    encodeTx = encodeWithBytes


-- | An 'AnnotatedCodec' with a custom `txWithBytes` wrapper of `tx`,
-- e.g. `txWithBytes ~ WithBytes tx` as in `anncodecTxSubmission2`.
--
anncodecTxSubmission2'
  :: forall (txid :: Type) (tx :: Type) (txWithBytes :: Type) m.
     MonadST m
  => (ByteString -> tx -> txWithBytes)
  -- ^ `withBytes` constructor
  -> (txid -> CBOR.Encoding)
  -- ^ encode 'txid'
  -> (forall s . CBOR.Decoder s txid)
  -- ^ decode 'txid'
  -> (txWithBytes -> CBOR.Encoding)
  -- ^ encode `tx`
  -> (forall s . CBOR.Decoder s (ByteString -> tx))
  -- ^ decode transaction
  -> AnnotatedCodec (TxSubmission2 txid txWithBytes) CBOR.DeserialiseFailure m ByteString
anncodecTxSubmission2' mkWithBytes encodeTxId decodeTxId
                                   encodeTx   decodeTx =
    mkCodecCborLazyBS
      (encodeTxSubmission2 encodeTxId encodeTx)
      decode
  where
    decode :: forall (st :: TxSubmission2 txid txWithBytes).
              ActiveState st
           => StateToken st
           -> forall s. CBOR.Decoder s (Annotator ByteString st)
    decode =
      decodeTxSubmission2 @tx
                          @txWithBytes
                          @WithByteSpan
                          @ByteString
                          mkWithBytes'
                          Utils.decodeWithByteSpan
                          decodeTxId decodeTx

    mkWithBytes' :: ByteString
                 -> WithByteSpan (ByteString -> tx)
                 -> txWithBytes
    mkWithBytes' bytes (WithByteSpan (fn, start, end)) =
        mkWithBytes (Utils.bytesBetweenOffsets start end bytes) (fn bytes)



--
-- Map protocol state & messages from `TxSubmission2 txid (Identity tx)` to
-- `TxSubmission2 txid tx`
--

type family FromIdentity (st :: TxSubmission2 txid (Identity tx)) :: TxSubmission2 txid tx where
  FromIdentity StInit        = StInit
  FromIdentity StIdle        = StIdle
  FromIdentity (StTxIds blk) = StTxIds blk
  FromIdentity StTxs         = StTxs
  FromIdentity StDone        = StDone

type family ToIdentity (st :: TxSubmission2 txid tx) :: TxSubmission2 txid (Identity tx) where
  ToIdentity StInit        = StInit
  ToIdentity StIdle        = StIdle
  ToIdentity (StTxIds blk) = StTxIds blk
  ToIdentity StTxs         = StTxs
  ToIdentity StDone        = StDone

singToIdentity
  :: forall txid tx (st :: TxSubmission2 txid tx).
     StateToken st
  -> StateToken (ToIdentity st)
singToIdentity SingInit        = SingInit
singToIdentity SingIdle        = SingIdle
singToIdentity (SingTxIds blk) = SingTxIds blk
singToIdentity SingTxs         = SingTxs
singToIdentity SingDone        = SingDone

--
-- Proofs
--

-- | A proof that `FromIdentity` is a left inverse of `ToIdentity`.
--
proof_FromTo
  :: forall txid tx (st :: TxSubmission2 txid tx).
     StateToken st
  -> FromIdentity (ToIdentity st) :~: st
proof_FromTo SingInit    = Refl
proof_FromTo SingIdle    = Refl
proof_FromTo SingTxIds{} = Refl
proof_FromTo SingTxs     = Refl
proof_FromTo SingDone    = Refl
{-# INLINE proof_FromTo #-}

-- | A proof that `ActiveState` constraint is preserved by `ToIdentity`.
--
proof_activeState
  :: forall txid tx (st :: TxSubmission2 txid tx).
     StateToken st
  -> Dict (ActiveState st)
  -> Dict (ActiveState (ToIdentity st))
proof_activeState SingInit      Dict = Dict
proof_activeState SingIdle      Dict = Dict
proof_activeState SingTxIds{}   Dict = Dict
proof_activeState SingTxs       Dict = Dict
proof_activeState sing@SingDone Dict = notActiveState sing
{-# INLINE proof_activeState #-}


msgFromIdentity
  :: forall txid tx (st :: TxSubmission2 txid (Identity tx)).
     SomeMessage st
  -> SomeMessage (FromIdentity st)
msgFromIdentity (SomeMessage MsgInit)
              =  SomeMessage MsgInit
msgFromIdentity (SomeMessage (MsgRequestTxIds blk@SingBlocking ack req))
              =  SomeMessage (MsgRequestTxIds blk ack req)
msgFromIdentity (SomeMessage (MsgRequestTxIds blk@SingNonBlocking ack req))
              =  SomeMessage (MsgRequestTxIds blk ack req)
msgFromIdentity (SomeMessage (MsgReplyTxIds txids@BlockingReply{}))
              =  SomeMessage (MsgReplyTxIds txids)
msgFromIdentity (SomeMessage (MsgReplyTxIds txids@NonBlockingReply{}))
              =  SomeMessage (MsgReplyTxIds txids)
msgFromIdentity (SomeMessage (MsgRequestTxs txids))
              =  SomeMessage (MsgRequestTxs txids)
msgFromIdentity (SomeMessage (MsgReplyTxs txs))
              =  SomeMessage (MsgReplyTxs (runIdentity <$> txs))
msgFromIdentity (SomeMessage MsgDone)
              =  SomeMessage MsgDone
{-# INLINE msgFromIdentity #-}


codecTxSubmission2
  :: forall (txid :: Type) (tx :: Type) m.
     MonadST m
  => (txid -> CBOR.Encoding)
  -- ^ encode 'txid'
  -> (forall s . CBOR.Decoder s txid)
  -- ^ decode 'txid'
  -> (tx -> CBOR.Encoding)
  -- ^ encode transaction
  -> (forall s . CBOR.Decoder s tx)
  -- ^ decode transaction
  -> Codec (TxSubmission2 txid tx) CBOR.DeserialiseFailure m ByteString
codecTxSubmission2 encodeTxId decodeTxId
                   encodeTx   decodeTx
  = mkCodecCborLazyBS
      (encodeTxSubmission2 encodeTxId encodeTx)
      decode
  where
    decode :: forall (st :: TxSubmission2 txid tx) s.
                         ActiveState st
                      => StateToken st
                      -> CBOR.Decoder s (SomeMessage st)
    decode sing =
      case proof_FromTo sing of { Refl ->
        case proof_activeState sing Dict of { Dict ->
          -- The `decode'` function requires the `ActiveState (Identity st)` constraint,
          -- which is provided by the `proof_activeState` proof for the given
          -- state token.
          --
          -- The `msgFromIdentity` function is applied to transform the result of `decode'`
          -- into a value of type `SomeMessage (FromIdentity (ToIdentity st))`. The
          -- `proof_FromTo` proof is then used to establish the type equality between
          -- `SomeMessage (FromIdentity (ToIdentity st))` and `SomeMessage st`, allowing
          -- GHC to infer the correct type.
          msgFromIdentity <$> decode' (singToIdentity sing)
        }
      }

    decode' :: forall (st :: TxSubmission2 txid (Identity tx)).
              ActiveState st
           => StateToken st
           -> forall s. CBOR.Decoder s (SomeMessage st)
    decode' stok =
      mapAnnotator <$>
        decodeTxSubmission2 @tx            -- tx without bytes
                            @(Identity tx) -- tx with bytes
                            @Identity      -- withByteSpan functor
                            @()            -- bytes
                            mkWithBytes
                            decodeWithByteSpan
                            decodeTxId
                            (const <$> decodeTx)
                            stok

    mapAnnotator :: Annotator () st -> SomeMessage st
    mapAnnotator Annotator { runAnnotator } = runAnnotator ()

    mkWithBytes :: forall a. () -> Identity (() -> a) -> Identity a
    mkWithBytes _ (Identity f) = Identity (f ())

    decodeWithByteSpan :: CBOR.Decoder s a -> CBOR.Decoder s (Identity a)
    decodeWithByteSpan = fmap Identity


encodeTxSubmission2
    :: forall (txid :: Type) (tx :: Type) (st :: TxSubmission2 txid tx) (st' :: TxSubmission2 txid tx).
       (txid -> CBOR.Encoding)
    -- ^ encode 'txid'
    -> (tx -> CBOR.Encoding)
    -- ^ encode 'tx'
    -> Message (TxSubmission2 txid tx) st st'
    -> CBOR.Encoding
encodeTxSubmission2 encodeTxId encodeTx = encode
  where
    encode :: forall st0 st1.
              Message (TxSubmission2 txid tx) st0 st1
           -> CBOR.Encoding
    encode MsgInit =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 6

    encode (MsgRequestTxIds blocking (NumTxIdsToAck ackNo) (NumTxIdsToReq reqNo)) =
        CBOR.encodeListLen 4
     <> CBOR.encodeWord 0
     <> CBOR.encodeBool (case blocking of
                           SingBlocking    -> True
                           SingNonBlocking -> False)
     <> CBOR.encodeWord16 ackNo
     <> CBOR.encodeWord16 reqNo

    encode (MsgReplyTxIds txids) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 1
     <> CBOR.encodeListLenIndef
     <> foldr (\(txid, SizeInBytes sz) r ->
                                CBOR.encodeListLen 2
                             <> encodeTxId txid
                             <> CBOR.encodeWord32 sz
                             <> r)
              CBOR.encodeBreak
              txids'
      where
        txids' :: [(txid, SizeInBytes)]
        txids' = case txids of
                   BlockingReply    xs -> NonEmpty.toList xs
                   NonBlockingReply xs -> xs

    encode (MsgRequestTxs txids) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> CBOR.encodeListLenIndef
     <> foldr (\txid r -> encodeTxId txid <> r) CBOR.encodeBreak txids

    encode (MsgReplyTxs txs) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 3
     <> CBOR.encodeListLenIndef
     <> foldr (\txid r -> encodeTx txid <> r) CBOR.encodeBreak txs

    encode MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 4


-- | Decode tx-submission mini-protocol message.  This decoder is polymorphic
-- in `txid`, `tx`, but also in:
--
-- * annotator's bytes
-- * tx's annotation, e.g. `withBytes`
-- * offsets used by the tx's annotator, e.g. `byteOffsetF`
--
decodeTxSubmission2
    :: forall (tx           :: Type)         -- tx without bytes
              (txWithBytes  :: Type)         -- tx with bytes
              (withByteSpan :: Type -> Type) -- withByteSpan functor
              (bytes        :: Type)         -- annotation bytes
              (txid :: Type)
              (st :: TxSubmission2 txid txWithBytes)
              s.
       ActiveState st
    => (bytes -> withByteSpan (bytes -> tx) -> txWithBytes)
    -- ^ mkWithBytes: smart constructor for `withBytes` functor.
    -> (forall a s'. CBOR.Decoder s' a -> CBOR.Decoder s' (withByteSpan a))
    -- ^ turn a `CBOR.Decoder` into a decoder of `withByteSpan a`, e.g.
    -- `CBOR.decodeWithByteSpan`.
    -> (forall s'. CBOR.Decoder s' txid)
    -- ^ decode a transaction id
    -> (forall s'. CBOR.Decoder s' (bytes -> tx))
    -- ^ decode a transaction
    -> StateToken st
    -- ^ protocol state token
    -> CBOR.Decoder s (Annotator bytes st)
decodeTxSubmission2 mkWithBytes decodeWithByteSpan
                    decodeTxId decodeTx
                    stok
  = do
  len <- CBOR.decodeListLen
  key <- CBOR.decodeWord
  case (stok, len, key) of
    (SingInit, 1, 6) ->
      return (Annotator $ \_ -> SomeMessage MsgInit)

    (SingIdle, 4, 0) -> do
      blocking <- CBOR.decodeBool
      ackNo    <- NumTxIdsToAck <$> CBOR.decodeWord16
      reqNo    <- NumTxIdsToReq <$> CBOR.decodeWord16
      return $!
        if blocking
        then Annotator $ \_ -> SomeMessage (MsgRequestTxIds SingBlocking    ackNo reqNo)
        else Annotator $ \_ -> SomeMessage (MsgRequestTxIds SingNonBlocking ackNo reqNo)

    (SingTxIds b, 2, 1) -> do
      CBOR.decodeListLenIndef
      txids <- CBOR.decodeSequenceLenIndef
                 (flip (:)) [] reverse
                 (do CBOR.decodeListLenOf 2
                     txid <- decodeTxId
                     sz   <- CBOR.decodeWord32
                     return (txid, SizeInBytes sz))
      case (b, txids) of
        (SingBlocking, t:ts) ->
          return $ Annotator $ \_ ->
            SomeMessage (MsgReplyTxIds (BlockingReply (t NonEmpty.:| ts)))

        (SingNonBlocking, ts) ->
          return $ Annotator $ \_ ->
            SomeMessage (MsgReplyTxIds (NonBlockingReply ts))

        (SingBlocking, []) ->
          fail "codecTxSubmission: MsgReplyTxIds: empty list not permitted"

    (SingIdle, 2, 2) -> do
      CBOR.decodeListLenIndef
      txids <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeTxId
      return (Annotator $ \_ -> SomeMessage (MsgRequestTxs txids))

    (SingTxs, 2, 3) -> do
      CBOR.decodeListLenIndef
      txs <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse (decodeWithByteSpan decodeTx)
      return (Annotator  $
               \bytes -> SomeMessage (MsgReplyTxs $ mkWithBytes bytes <$> txs))

    (SingTxIds SingBlocking, 1, 4) ->
      return (Annotator $ \_ -> SomeMessage MsgDone)

    (SingDone, _, _) -> notActiveState stok

    --
    -- failures per protocol state
    --

    (SingInit, _, _) ->
        fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
    (SingTxIds SingBlocking, _, _) ->
        fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
    (SingTxIds SingNonBlocking, _, _) ->
        fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
    (SingTxs, _, _) ->
        fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
    (SingIdle, _, _) ->
        fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)


codecTxSubmission2Id
  :: forall txid tx m. Monad m
  => Codec (TxSubmission2 txid tx) CodecFailure m (AnyMessage (TxSubmission2 txid tx))
codecTxSubmission2Id = Codec { encode, decode }
 where
  encode :: forall st st'.
            ActiveState st
         => StateTokenI st
         => Message (TxSubmission2 txid tx) st st'
         -> AnyMessage (TxSubmission2 txid tx)
  encode = AnyMessage

  decode :: forall (st :: TxSubmission2 txid tx).
            ActiveState st
         => StateToken st
         -> m (DecodeStep (AnyMessage (TxSubmission2 txid tx))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes -> return $ case (stok, bytes) of
    (SingInit,      Just (AnyMessage msg@MsgInit))              -> DecodeDone (SomeMessage msg) Nothing
    (SingIdle,      Just (AnyMessage msg@(MsgRequestTxIds SingBlocking _ _)))    -> DecodeDone (SomeMessage msg) Nothing
    (SingIdle,      Just (AnyMessage msg@(MsgRequestTxIds SingNonBlocking _ _))) -> DecodeDone (SomeMessage msg) Nothing
    (SingIdle,      Just (AnyMessage msg@(MsgRequestTxs {})))   -> DecodeDone (SomeMessage msg) Nothing
    (SingTxs,       Just (AnyMessage msg@(MsgReplyTxs {})))     -> DecodeDone (SomeMessage msg) Nothing
    (SingTxIds b, Just (AnyMessage msg)) -> case (b, msg) of
      (SingBlocking,    MsgReplyTxIds (BlockingReply {}))    -> DecodeDone (SomeMessage msg) Nothing
      (SingNonBlocking, MsgReplyTxIds (NonBlockingReply {})) -> DecodeDone (SomeMessage msg) Nothing
      (SingBlocking,    MsgDone {})                          -> DecodeDone (SomeMessage msg) Nothing
      (_, _) -> DecodeFail (CodecFailure "codecTxSubmissionId: no matching message")
    (SingDone, _) -> notActiveState stok
    (_, _) -> DecodeFail (CodecFailure "codecTxSubmissionId: no matching message")
