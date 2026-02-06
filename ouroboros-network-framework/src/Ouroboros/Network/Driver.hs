
-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Ouroboros.Network.Driver
  ( runPeer
  , runPipelinedPeer
  , runPeerWithLimits
  , runPeerWithLimitsRnd
  , runPipelinedPeerWithLimits
  , runPipelinedPeerWithLimitsRnd
  , TraceSendRecv (..)
  ) where

import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.Driver.Simple
