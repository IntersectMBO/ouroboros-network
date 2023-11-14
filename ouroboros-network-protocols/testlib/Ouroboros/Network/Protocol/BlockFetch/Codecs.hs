{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Ouroboros.Network.Protocol.BlockFetch.Codecs where

import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise (Serialise)
import Codec.Serialise.Class qualified as Serialise
import Control.DeepSeq
import Data.ByteString.Lazy qualified as BL
import GHC.Generics
import Network.TypedProtocol.Codec
import Ouroboros.Network.Protocol.BlockFetch.Codec
import Ouroboros.Network.Protocol.BlockFetch.Type
import Test.Data.CDDL
import Test.QuickCheck

newtype Block = Block Any
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

newtype BlockPoint = BlockPoint Any
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

blockFetchCodec :: Codec (BlockFetch Block BlockPoint)
                         CBOR.DeserialiseFailure IO BL.ByteString
blockFetchCodec =
    codecBlockFetch
      Serialise.encode
      Serialise.decode
      Serialise.encode
      Serialise.decode


