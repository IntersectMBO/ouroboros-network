module Ouroboros.Network.Channel
  ( module Network.TypedProtocol.Channel
  , createPipeConnectedChannels
  ) where

import           Data.ByteString (ByteString)
import qualified System.Process as IO (createPipe)

import           Network.TypedProtocol.Channel

-- | Create a local pipe, with both ends in this process, and expose that as
-- a pair of 'ByteChannel's, one for each end.
--
-- This is primarily for testing purposes since it does not allow actual IPC.
--
createPipeConnectedChannels :: IO (Channel IO ByteString,
                                   Channel IO ByteString)
createPipeConnectedChannels = do
    -- Create two pipes (each one is unidirectional) to make both ends of
    -- a bidirectional channel
    (hndReadA, hndWriteB) <- IO.createPipe
    (hndReadB, hndWriteA) <- IO.createPipe

    return (handlesAsChannel hndReadA hndWriteA,
            handlesAsChannel hndReadB hndWriteB)
