module Ouroboros.Network.Protocol.ObjectDiffusion.Codec.CDDL where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise.Class qualified as Serialise
import Data.ByteString.Lazy qualified as BL
import Network.TypedProtocol.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Test (Object, ObjectId)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

objectDiffusionCodec :: Codec (ObjectDiffusion ObjectId Object)
                            CBOR.DeserialiseFailure IO BL.ByteString
objectDiffusionCodec =
    codecObjectDiffusion
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode
