{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Network.Protocol.ObjectDiffusion.Codec.CDDL
  ( objectDiffusionCodec
  , ObjectId
  , Object
  ) where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise (Serialise)
import Codec.Serialise.Class qualified as Serialise
import Control.DeepSeq (NFData)
import Data.ByteString.Lazy qualified as BL
import GHC.Generics (Generic)
import Network.TypedProtocol.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import Test.Data.CDDL (Any (..))
import Test.QuickCheck (Arbitrary)

-- | Simple wrapper around 'Any' matching @objectId = any@ in the CDDL spec.
-- This is intentionally different from the 'ObjectId' in
-- 'ObjectDiffusion.Test' which wraps @Maybe Word64@ for the caught-up sentinel.
--
newtype ObjectId = ObjectId Any
  deriving (Eq, Ord, Show, Arbitrary, Serialise, Generic, NFData)

instance ShowProxy ObjectId where
    showProxy _ = "ObjectId"

-- | Simple wrapper around 'Any' matching @object = any@ in the CDDL spec.
--
newtype Object = Object Any
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

instance ShowProxy Object where
    showProxy _ = "Object"

objectDiffusionCodec :: Codec (ObjectDiffusion ObjectId Object)
                            CBOR.DeserialiseFailure IO BL.ByteString
objectDiffusionCodec =
    codecObjectDiffusion
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode
