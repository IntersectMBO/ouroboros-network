
-- | Drivers for running 'Peer's.
--
module Network.TypedProtocol.Driver (

  -- * Introduction
  -- $intro

  -- * Normal peers
  runPeer,
  runPeerWithDriver,
  TraceSendRecv(..),

  -- * Pipelined peers
  runPipelinedPeer,
  runPipelinedPeerWithDriver,

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

