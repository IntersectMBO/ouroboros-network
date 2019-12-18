{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A type in which each value is the byte serialisation of a header hash.
--
-- This is an implementation detail of
-- "Ouroboros.Consensus.Protocol.PBFT.ChainState".
module Ouroboros.Consensus.Protocol.PBFT.ChainState.HeaderHashBytes (
  HeaderHashBytes,
  headerHashBytes,
  -- * For testing
  mkHeaderHashBytesForTestingOnly,
  ) where

import           Codec.Serialise (Serialise (..), serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (HeaderHash)

newtype HeaderHashBytes = HeaderHashBytes BS.ByteString
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, NoUnexpectedThunks, Serialise)

-- | The safe way to construct 'HeaderHashBytes'
headerHashBytes
  :: Serialise (HeaderHash hdr)
  => proxy hdr -> HeaderHash hdr -> HeaderHashBytes
headerHashBytes _ = HeaderHashBytes . BSL.toStrict . serialise

mkHeaderHashBytesForTestingOnly :: BSL.ByteString -> HeaderHashBytes
mkHeaderHashBytesForTestingOnly = HeaderHashBytes . BSL.toStrict
