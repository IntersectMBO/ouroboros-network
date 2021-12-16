{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}

module Ouroboros.Network.Protocol.TxSubmission2.Codec
  ( codecTxSubmission2
  , codecTxSubmission2Id
  , byteLimitsTxSubmission2
  , timeLimitsTxSubmission2
  ) where

import           Control.Monad.Class.MonadST

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Data.ByteString.Lazy (ByteString)

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.Trans.Hello.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission2.Type
import           Ouroboros.Network.Util.ShowProxy


-- | Byte Limits.
--
-- Preserves byte limits of the original 'TxSubmission' protocol, see
-- 'timeLimitsTxSubmission'.  'MsgHello' is using 'smallByteLimit' limit.
--
byteLimitsTxSubmission2 :: forall bytes txid tx.
                           (bytes -> Word)
                        -> ProtocolSizeLimits (TxSubmission2 txid tx) bytes
byteLimitsTxSubmission2 = byteLimitsHello . byteLimitsTxSubmission


-- | Time limits.
--
-- Preserves the timeouts of 'TxSubmission' protocol, see
-- 'timeLimitsTxSubmission'.  'MsgHello' does not have a timeout.
--
timeLimitsTxSubmission2 :: forall txid tx. ProtocolTimeLimits (TxSubmission2 txid tx)
timeLimitsTxSubmission2 = timeLimitsHello timeLimitsTxSubmission


codecTxSubmission2
  :: forall txid tx m.
     ( MonadST m
     , ShowProxy txid
     , ShowProxy tx
     )
  => (txid -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s txid)
  -> (tx -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s tx)
  -> Codec (TxSubmission2 txid tx) CBOR.DeserialiseFailure m ByteString
codecTxSubmission2 encodeTxId decodeTxId
                   encodeTx   decodeTx =
    codecHello
      6
      (encodeTxSubmission encodeTxId encodeTx)
      (decodeTxSubmission decodeTxId decodeTx)

codecTxSubmission2Id
  :: forall txid tx m. Monad m
  => Codec (TxSubmission2 txid tx) CodecFailure m (AnyMessage (TxSubmission2 txid tx))
codecTxSubmission2Id = codecHelloId codecTxSubmissionId
