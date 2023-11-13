{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Network.Protocol.ChainSync.Codecs where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise (Serialise)
import Codec.Serialise.Class qualified as Serialise
import Data.ByteString.Lazy qualified as BL
import Network.TypedProtocol.Codec
import Ouroboros.Network.Protocol.ChainSync.Codec
import Ouroboros.Network.Protocol.ChainSync.Type
import Test.Data.CDDL
import Test.QuickCheck

newtype BlockHeader = BlockHeader Any
  deriving (Eq, Show, Arbitrary, Serialise)

newtype HeaderPoint = HeaderPoint Any
  deriving (Eq, Show, Arbitrary, Serialise)

newtype HeaderTip = HeaderTip Any
  deriving (Eq, Show, Arbitrary, Serialise)

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

