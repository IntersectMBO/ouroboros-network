{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Network.Magic where

import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Cardano.Prelude (NoUnexpectedThunks)

-- | NetworkMagic is used to differentiate between different networks during the initial handshake.
newtype NetworkMagic  = NetworkMagic { unNetworkMagic :: Word32 }
  deriving (Eq, Generic, NoUnexpectedThunks)
  deriving (Show) via (Quiet NetworkMagic)
