module Ouroboros.Network.Protocol.TxSubmission2.Codecs where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise.Class qualified as Serialise
import Data.ByteString.Lazy qualified as BL
import Network.TypedProtocol.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Test (Tx, TxId)
import Ouroboros.Network.Protocol.TxSubmission2.Type

txSubmissionCodec2 :: Codec (TxSubmission2 TxId Tx)
                            CBOR.DeserialiseFailure IO BL.ByteString
txSubmissionCodec2 =
    codecTxSubmission2
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode
