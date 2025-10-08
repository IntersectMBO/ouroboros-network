module Ouroboros.Network.Protocol.LocalTxSubmission.Codec.CDDL where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise.Class qualified as Serialise
import Data.ByteString.Lazy qualified as BL
import Network.TypedProtocol.Codec
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import Ouroboros.Network.Protocol.LocalTxSubmission.Test
import Ouroboros.Network.Protocol.LocalTxSubmission.Type

localTxSubmissionCodec :: Codec (LocalTxSubmission Tx Reject)
                                CBOR.DeserialiseFailure IO BL.ByteString
localTxSubmissionCodec =
    codecLocalTxSubmission
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode


