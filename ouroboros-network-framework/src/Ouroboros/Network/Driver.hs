
-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Ouroboros.Network.Driver
  ( -- * Normal peers
    runPeer
  , runPeerWithLimits
  , TraceSendRecv (..)
    -- * Pipelined peers
  , runPipelinedPeer
  , runPipelinedPeerWithLimits
  ) where

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Driver.Simple

