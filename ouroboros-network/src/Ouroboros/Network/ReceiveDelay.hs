module Ouroboros.Network.ReceiveDelay (
    HasReceiveDelay (..)
  ) where

import           Data.Time (NominalDiffTime)

class HasReceiveDelay a where
  -- | The time between production and reception via the network.
  --
  -- For example, the difference between the time at which a block was produced
  -- (derived from its slot number) and the time at which we received the block
  -- or its header via the network.
  --
  -- When a block or header was read from disk and we no longer know when we
  -- received it over the network, 'Nothing' is returned.
  receiveDelay :: a -> Maybe NominalDiffTime
