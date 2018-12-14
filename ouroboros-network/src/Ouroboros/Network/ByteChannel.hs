{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.ByteChannel (
      ByteChannel(..)
    , ReadResult(..)
    , fixedInputByteChannel
    , handleAsByteChannel
    , socketAsByteChannel
    , createPipeByteChannel
    , createLocalsocketAsByteChannel
    , withFifosByteChannel
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Internal (smallChunkSize)

import qualified System.IO      as IO
                   ( Handle, withFile, IOMode(..), hFlush )
import qualified System.Process as IO (createPipe)

import qualified Network.Socket            as Socket hiding (recv)
import qualified Network.Socket.ByteString as Socket (sendMany, recv)

import Prelude hiding (read)


-- | One end of a bidirectional channel. It is a reliable, ordered channel
-- of bytes without message boundaries.
--
-- This corresponds with IPC facilities like TCP sockets or local pipes.
--
data ByteChannel bytes m = ByteChannel {
       -- | Read some number of bytes from the channel, or @Nothing@ for EOF.
       --
       -- This is a blocking I\/O operation. It may raise exceptions (as
       -- appropriate for the monad and kind of channel).
       --
       read  :: m (ReadResult bytes (ByteChannel bytes m)),

       -- | Write a vector of chunks to the channel. This allows for (but does
       -- not guarante) vectored writes. All bytes are written.
       --
       -- This is a blocking I\/O operation.  It may raise exceptions (as
       -- appropriate for the monad and kind of channel).
       --
       write :: [bytes] -> m (ByteChannel bytes m)
     }

data ReadResult bytes channel = ReadChunk bytes channel | ChannelClosed

-- | A 'ByteChannel' with a fixed input, and where all output is discarded.
--
-- The input is guaranteed to be supplied via 'read' with the given chunk
-- boundaries.
--
-- Only useful for testing. In particular the fixed chunk boundaries can be
-- used to test that framing and other codecs work with any possible chunking.
--
fixedInputByteChannel :: Applicative m => [bytes] -> ByteChannel bytes m
fixedInputByteChannel chunks =
    ByteChannel {read, write}
  where
    -- This is basically an unfoldr
    read = case chunks of
             []              -> pure ChannelClosed
             (chunk:chunks') -> pure (ReadChunk chunk channel')
               where
                 channel' = fixedInputByteChannel chunks'

    write _ = pure (fixedInputByteChannel chunks)

{-
bufferedByteChannel :: MonadSTM m stm
                    => m (ByteChannel bytes m,
                          ByteChannel bytes m)
bufferedByteChannel = do
    mvar1 <- newEmptyMVar
    mvar2 <- newEmptyMVar

    let channelA = mvarsAsChannel mvar1 mvar2
        channelB = mvarsAsChannel mvar2 mvar1

    return (channelA, channelB)

  where
    mvarsAsChannel mvarRead mvarWrite =
      where
        channel     = ByteChannel {read, write}
        read        = do mchunk <- atomically $ takeTMVar mvarRead
                         case mchunk of
                           Nothing    -> return Nothing
                           Just chunk -> return (Just (chunk, channel))

        write chunk = do atomically $ putTMVar mvarWrite chunk
                         return channel
-}

-- | Make a 'ByteChannel' from a pair of IO 'Handle's, one for reading and one
-- for writing.
--
-- The Handles should be open in the appropriate read or write mode, and in
-- binary mode. Writes are flushed after each batch of writes, so it is safe to
-- use a buffering mode.
--
-- For bidirectional handles it is safe to pass the same handle for both.
--
handleAsByteChannel :: IO.Handle -- ^ Read handle
                    -> IO.Handle -- ^ Write handle
                    -> ByteChannel ByteString IO
handleAsByteChannel hndRead hndWrite =
    channel
  where
    channel = ByteChannel {read, write}

    read = do
      chunk <- BS.hGetSome hndRead smallChunkSize
      if BS.null chunk
        then return ChannelClosed
        else return (ReadChunk chunk channel)

    write chunks = do
      -- No special vectored write support.
      mapM_ (BS.hPut hndWrite) chunks
      IO.hFlush hndWrite
      return channel


-- | Make a 'ByteChannel' from a 'Socket'. The socket must be a stream socket
-- type and status connected.
--
socketAsByteChannel :: Socket.Socket -> ByteChannel ByteString IO
socketAsByteChannel socket =
    channel
  where
    channel = ByteChannel {read, write}

    read = do
      -- We rely on the behaviour of stream sockets that a zero length chunk
      -- indicates EOF.
      chunk <- Socket.recv socket smallChunkSize
      if BS.null chunk
        then return ChannelClosed
        else return (ReadChunk chunk channel)

    write chunk = do
      -- Use vectored writes.
      -- TODO: limit write sizes, or break them into multiple sends.
      Socket.sendMany socket chunk
      return channel


-- | Create a local pipe, with both ends in this process, and expose that as
-- a pair of 'ByteChannel's, one for each end.
--
-- This is primarily for testing purposes since it does not allow actual IPC.
--
createPipeByteChannel :: IO (ByteChannel ByteString IO,
                             ByteChannel ByteString IO)
createPipeByteChannel = do
    -- Create two pipes (each one is unidirectional) to make both ends of
    -- a bidirectional channel
    (hndReadA, hndWriteB) <- IO.createPipe
    (hndReadB, hndWriteA) <- IO.createPipe

    let channelA = handleAsByteChannel hndReadA hndWriteA
        channelB = handleAsByteChannel hndReadB hndWriteB

    return (channelA, channelB)


-- | Create a local socket, with both ends in this process, and expose that as
-- a pair of 'ByteChannel's, one for each end.
--
-- This is primarily for testing purposes since it does not allow actual IPC.
--
createLocalsocketAsByteChannel :: Socket.Family -- ^ Usually AF_UNIX or AF_INET
                               -> IO (ByteChannel ByteString IO,
                                      ByteChannel ByteString IO)
createLocalsocketAsByteChannel family = do
    -- Create a socket pair to make both ends of a bidirectional channel
    (socketA, socketB) <- Socket.socketPair family Socket.Stream
                                            Socket.defaultProtocol

    let channelA = socketAsByteChannel socketA
        channelB = socketAsByteChannel socketB

    return (channelA, channelB)


-- | Open a pair of Unix FIFOs, and expose that as a 'ByteChannel'.
--
-- The peer process needs to open the same files but the other way around,
-- for writing and reading.
--
-- This is primarily for the purpose of demonstrations that use communication
-- between multiple local processes. It is Unix specific.
--
withFifosByteChannel :: FilePath -- ^ FIFO for reading
                     -> FilePath -- ^ FIFO for writing
                     -> (ByteChannel ByteString IO -> IO a) -> IO a
withFifosByteChannel fifoPathRead fifoPathWrite action =
    IO.withFile fifoPathRead  IO.ReadMode  $ \hndRead  ->
    IO.withFile fifoPathWrite IO.WriteMode $ \hndWrite ->
      let channel = handleAsByteChannel hndRead hndWrite
       in action channel

