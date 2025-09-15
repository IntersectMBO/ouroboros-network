module Ouroboros.Network.Protocol.LocalTxMonitor.Codec.CDDL where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise.Class qualified as Serialise
import Data.ByteString.Lazy qualified as BL
import Network.TypedProtocol.Codec
import Ouroboros.Network.Block (SlotNo)
import Ouroboros.Network.Protocol.LocalTxMonitor.Codec
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
import Ouroboros.Network.Protocol.TxSubmission2.Test (Tx, TxId)

localTxMonitorCodec :: Codec (LocalTxMonitor TxId Tx SlotNo)
                            CBOR.DeserialiseFailure IO BL.ByteString
localTxMonitorCodec =
    codecLocalTxMonitor
      maxBound
      Serialise.encode Serialise.decode
      Serialise.encode Serialise.decode
      Serialise.encode Serialise.decode


