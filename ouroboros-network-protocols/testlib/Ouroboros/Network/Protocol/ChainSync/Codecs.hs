{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Network.Protocol.ChainSync.Codecs where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise (Serialise)
import Codec.Serialise.Class qualified as Serialise
import Control.DeepSeq
import Data.ByteString.Lazy qualified as BL
import GHC.Generics (Generic)
import Network.TypedProtocol.Codec
import Ouroboros.Network.Protocol.ChainSync.Codec
import Ouroboros.Network.Protocol.ChainSync.Type
import Test.Data.CDDL
import Test.QuickCheck

newtype BlockHeader = BlockHeader Any
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

newtype HeaderPoint = HeaderPoint Any
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

newtype HeaderTip = HeaderTip Any
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

chainSyncCodec :: Codec (ChainSync BlockHeader HeaderPoint HeaderTip)
                        CBOR.DeserialiseFailure IO BL.ByteString
chainSyncCodec =
    codecChainSync
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode

