{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Network.Protocol.TipSample.Codec
  ( codecTipSample
  , codecTipSampleId

  , byteLimitsTipSample
  , timeLimitsTipSample
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime

import           Network.TypedProtocol.Pipelined (Nat (Succ, Zero), natToInt, unsafeIntToNat)
import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.TipSample.Type
import           Ouroboros.Network.Protocol.Limits

import qualified Data.ByteString.Lazy as LBS

import           Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen, encodeWord)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.Serialise as Serialise (Serialise (..))
import           Text.Printf

import           Unsafe.Coerce (unsafeCoerce)


codecTipSample
  :: forall m tip. MonadST m
  => (tip -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s tip)
  -> Codec (TipSample tip)
           CBOR.DeserialiseFailure m LBS.ByteString
codecTipSample encodeTip decodeTip =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall (pr  :: PeerRole)
                     (st  :: TipSample tip)
                     (st' :: TipSample tip).
              PeerHasAgency pr st
           -> Message (TipSample tip) st st'
           -> CBOR.Encoding
    encode (ClientAgency TokIdle) (MsgFollowTip n slotNo) =
      encodeListLen 2
        <> encodeWord 0
        <> CBOR.encodeInt (natToInt n)
        <> Serialise.encode slotNo

    encode (ServerAgency (TokFollowTip _)) (MsgNextTip tip) =
      encodeListLen 2
        <> encodeWord 1
        <> encodeTip tip

    encode (ServerAgency (TokFollowTip _)) (MsgNextTipDone tip) =
      encodeListLen 2
        <> encodeWord 2
        <> encodeTip tip

    encode (ClientAgency TokIdle) MsgDone =
      encodeListLen 1 <> encodeWord 3


    decode :: forall (pr :: PeerRole) (st :: TipSample tip) s.
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- decodeListLen
      key <- decodeWord

      case (key, len, stok) of
        (0, 2, ClientAgency TokIdle) ->
          (fmap SomeMessage . MsgFollowTip)
            <$> (unsafeIntToNat <$> CBOR.decodeInt)
            <*> Serialise.decode

        (1, 2, ServerAgency (TokFollowTip (Succ (Succ _)))) ->
          SomeMessage . MsgNextTip <$> decodeTip

        (2, 2, ServerAgency (TokFollowTip (Succ Zero))) ->
          SomeMessage . MsgNextTipDone <$> decodeTip

        (3, 1, ClientAgency TokIdle) ->
          pure (SomeMessage MsgDone)

        --
        -- failures
        --

        (_, _, _) ->
          fail (printf "codecTipSample (%s) unexpected key (%d, %d)" (show stok) key len)


codecTipSampleId :: forall tip m.
                    Applicative m
                 => Codec (TipSample tip)
                          CodecFailure m (AnyMessage (TipSample tip))
codecTipSampleId = Codec encode decode
  where
    encode :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (TipSample tip) st st'
           -> AnyMessage (TipSample tip)
    encode _ = AnyMessage

    decode :: forall (pr :: PeerRole) (st :: TipSample tip).
              PeerHasAgency pr st
           -> m (DecodeStep (AnyMessage (TipSample tip))
                            CodecFailure m (SomeMessage st))
    decode stok = pure $ DecodePartial $ \bytes -> case (stok, bytes) of
      (_, Nothing) -> pure $ DecodeFail CodecFailureOutOfInput

      (ClientAgency TokIdle, Just (AnyMessage msg@MsgFollowTip{})) ->
        pure (DecodeDone (SomeMessage msg) Nothing)

      (ServerAgency (TokFollowTip (Succ (Succ _))), Just (AnyMessage msg@MsgNextTip{})) ->
        -- TODO: we are using `unsafeCoerce` :/ it's not easy (might not be
        -- possible) to make it work with `Typeable`; A solution
        -- that will work for sure is to add `Nat n` to `MsgNextTip`
        -- constructor and pattern match on it;
        pure (DecodeDone (SomeMessage $ unsafeCoerce msg) Nothing)

      (ServerAgency (TokFollowTip _), Just (AnyMessage msg@MsgNextTipDone{})) ->
        -- as above.
        pure (DecodeDone (SomeMessage $ unsafeCoerce msg) Nothing)

      (ClientAgency TokIdle, Just (AnyMessage msg@MsgDone)) ->
        pure (DecodeDone (SomeMessage msg) Nothing)

      (_, _) -> pure $ DecodeFail (CodecFailure "codecTipSample: no matching message")


-- | Byte limits for 'TipSample' protocol.
--
byteLimitsTipSample :: forall tip bytes.
                       (bytes -> Word)
                    -> ProtocolSizeLimits (TipSample tip) bytes
byteLimitsTipSample = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: TipSample tip).
                    PeerHasAgency pr st -> Word
    stateToLimit (ClientAgency TokIdle)          = smallByteLimit
    stateToLimit (ServerAgency (TokFollowTip _)) = smallByteLimit


-- | Time limits for 'TipSample' protocol.
--
timeLimitsTipSample :: forall tip.
                       ProtocolTimeLimits (TipSample tip)
timeLimitsTipSample = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: TipSample tip).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokIdle)          = waitForever
    -- todo: initial time limit should be `waitForever` next one should be
    -- `longWait`
    stateToLimit (ServerAgency (TokFollowTip _)) = waitForever
