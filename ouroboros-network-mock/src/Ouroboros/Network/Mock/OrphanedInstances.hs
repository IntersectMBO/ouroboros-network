{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Mock.OrphanedInstances () where

import Control.DeepSeq (NFData (..))
import Network.TypedProtocol.Codec (CodecFailure (..))

instance NFData CodecFailure where
    rnf CodecFailureOutOfInput = ()
    rnf (CodecFailure failure) = rnf failure
