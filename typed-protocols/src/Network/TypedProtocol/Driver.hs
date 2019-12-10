
-- | Drivers for running 'Peer's.
--
module Network.TypedProtocol.Driver (

  -- * Introduction
  -- $intro

  -- * Normal peers
  runPeer,
  runPeer',
  runPeerWithDriver,
  TraceSendRecv(..),
  runPeerWithLimits,

  -- * Pipelined peers
  runPipelinedPeer,
  runPipelinedPeer',
  runPipelinedPeerWithDriver,
  runPipelinedPeerWithLimits,

  -- * Connected peers
  runConnectedPeers,
  runConnectedPeersPipelined,

  -- * Driver utilities
  -- | This may be useful if you want to write your own driver.
  Driver(..),
  runDecoderWithChannel,
  ) where

import Network.TypedProtocol.Driver.General
import Network.TypedProtocol.Driver.Simple
import Network.TypedProtocol.Driver.Limits

