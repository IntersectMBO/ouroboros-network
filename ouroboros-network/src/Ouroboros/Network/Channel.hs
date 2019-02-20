module Ouroboros.Network.Channel
  ( module Network.TypedProtocol.Channel
  , createPipeConnectedChannels
  , withFifosAsChannel
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified System.Process as IO (createPipe)
import qualified System.IO      as IO
                   ( withFile, IOMode(..) )

import           Network.TypedProtocol.Channel

-- | Create a local pipe, with both ends in this process, and expose that as
-- a pair of 'ByteChannel's, one for each end.
--
-- This is primarily for testing purposes since it does not allow actual IPC.
--
createPipeConnectedChannels :: IO (Channel IO LBS.ByteString,
                                   Channel IO LBS.ByteString)
createPipeConnectedChannels = do
    -- Create two pipes (each one is unidirectional) to make both ends of
    -- a bidirectional channel
    (hndReadA, hndWriteB) <- IO.createPipe
    (hndReadB, hndWriteA) <- IO.createPipe

    return (handlesAsChannel hndReadA hndWriteA,
            handlesAsChannel hndReadB hndWriteB)

-- | Open a pair of Unix FIFOs, and expose that as a 'Channel'.
--
-- The peer process needs to open the same files but the other way around,
-- for writing and reading.
--
-- This is primarily for the purpose of demonstrations that use communication
-- between multiple local processes. It is Unix specific.
--
withFifosAsChannel :: FilePath -- ^ FIFO for reading
                   -> FilePath -- ^ FIFO for writing
                   -> (Channel IO LBS.ByteString -> IO a) -> IO a
withFifosAsChannel fifoPathRead fifoPathWrite action =
    IO.withFile fifoPathRead  IO.ReadMode  $ \hndRead  ->
    IO.withFile fifoPathWrite IO.WriteMode $ \hndWrite ->
      let channel = handlesAsChannel hndRead hndWrite
       in action channel
