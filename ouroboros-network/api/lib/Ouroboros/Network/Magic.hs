{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Network.Magic where

import Control.DeepSeq (NFData)
import Data.Word (Word32)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Quiet (Quiet (..))


-- | NetworkMagic is used to differentiate between different networks during the initial handshake.
newtype NetworkMagic  = NetworkMagic { unNetworkMagic :: Word32 }
  deriving (Eq, Generic, NoThunks)
  deriving Show via (Quiet NetworkMagic)

instance NFData NetworkMagic
