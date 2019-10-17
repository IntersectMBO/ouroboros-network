{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Network.Magic where

import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

-- | NetworkMagic is used to differentiate between different networks during the initial handshake.
newtype NetworkMagic  = NetworkMagic { unNetworkMagic :: Word32 }
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

