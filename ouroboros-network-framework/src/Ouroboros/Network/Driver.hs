
-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Ouroboros.Network.Driver
  ( runPeer
  , runPeerWithLimits
  , TraceSendRecv (..)
  ) where

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Driver.Simple
