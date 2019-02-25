{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}

-- | An extension of 'Network.TypedProtocol.Channel', with additional 'Channel'
-- implementations.
--
module Ouroboros.Network.Channel
  ( module Network.TypedProtocol.Channel
  , newMuxBufferedConnectedChannels
  , createPipeConnectedChannels
  , createSocketConnectedChannels
  , withFifosAsChannel
  , socketAsChannel
  , delayChannel
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)
import qualified System.Process as IO (createPipe)
import qualified System.IO      as IO
                   ( withFile, IOMode(..) )
import qualified Network.Socket            as Socket hiding (send, recv)
import qualified Network.Socket.ByteString as Socket

import           Control.Monad (when)
import           Control.Exception (assert)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer

import           Network.TypedProtocol.Channel


-- | Create a pair of 'Channel's that are connected internally with a buffering
-- strategy designed for inter-thread communication between a multiplexing
-- thread and a thread running a peer.
--
-- Multiple writes to the buffer are allowed up to a maximum buffer size. Once
-- it reaches the maximum buffer size then the writer blocks and has to wait.
--
-- On the reader side, the entire buffer is returned, which can of course be up
-- to the maximum buffer size. Only if the buffer is empty will the reader
-- block.
--
-- It uses lazy 'ByteString's but it ensures that data written to the channel
-- is /fully evaluated/ first. This ensures that any work to serialise the data
-- takes place on the /writer side and not the reader side/.
--
newMuxBufferedConnectedChannels :: forall m. MonadSTM m
                                => Int -- ^ maximum size of @recv@ chunk
                                -> m (Channel m LBS.ByteString,
                                      Channel m LBS.ByteString)
newMuxBufferedConnectedChannels maxBufferSize =
    assert (0 < maxBufferSize) $ do
    bufferA <- newTVarM []
    bufferB <- newTVarM []

    return (buffersAsChannel bufferB bufferA,
            buffersAsChannel bufferA bufferB)
  where
    buffersAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send :: LBS.ByteString -> m ()
        send x = mapM_ (writeToBuffer bufferWrite) (LBS.toChunks x)

        recv :: m (Maybe LBS.ByteString)
        recv   = Just <$> readFromBuffer bufferRead

    -- Strategy is we keep the buffer within the max buffer size at all times.
    -- When we write a new chunk, we take a slice of it that fits and return
    -- the rest. We block if the buffer is now full.
    writeToBuffer :: TVar m [BS.ByteString] -> BS.ByteString -> m BS.ByteString
    writeToBuffer buffer !chunk =
      atomically $ do
        bufContent <- readTVar buffer
        let bufferSize = sum (map BS.length bufContent)
            freeSpace  = maxBufferSize - bufferSize
            isFull     = freeSpace <= 0
        when isFull retry
        let (chunk', remaining) = BS.splitAt freeSpace chunk
        writeTVar buffer (chunk' : bufContent)
        return remaining -- if any

    -- On the read side we can just grab the whole lot, since it's always within
    -- the maximum buffer size.
    readFromBuffer :: TVar m [BS.ByteString] -> m LBS.ByteString
    readFromBuffer buffer =
      atomically $ do
        bufContent <- readTVar buffer
        when (null bufContent) retry
        writeTVar buffer []
        return (LBS.fromChunks (reverse bufContent))


-- | Create a local pipe, with both ends in this process, and expose that as
-- a pair of 'Channel's, one for each end.
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


-- | Make a 'Channel' from a 'Socket'. The socket must be a stream socket
--- type and status connected.
---
socketAsChannel :: Socket.Socket -> Channel IO LBS.ByteString
socketAsChannel socket =
    Channel{send, recv}
  where
    send :: LBS.ByteString -> IO ()
    send chunks =
     -- Use vectored writes.
     Socket.sendMany socket (LBS.toChunks chunks)
     -- TODO: limit write sizes, or break them into multiple sends.

    recv :: IO (Maybe LBS.ByteString)
    recv = do
      -- We rely on the behaviour of stream sockets that a zero length chunk
      -- indicates EOF.
      chunk <- Socket.recv socket LBS.smallChunkSize
      if BS.null chunk
        then return Nothing
        else return (Just (LBS.fromStrict chunk))


--- | Create a local socket, with both ends in this process, and expose that as
--- a pair of 'ByteChannel's, one for each end.
---
--- This is primarily for testing purposes since it does not allow actual IPC.
---
createSocketConnectedChannels :: Socket.Family -- ^ Usually AF_UNIX or AF_INET
                              -> IO (Channel IO LBS.ByteString,
                                     Channel IO LBS.ByteString)
createSocketConnectedChannels family = do
   -- Create a socket pair to make both ends of a bidirectional channel
   (socketA, socketB) <- Socket.socketPair family Socket.Stream
                                           Socket.defaultProtocol

   return (socketAsChannel socketA,
           socketAsChannel socketB)


-- | Delay a channel on the receiver end.
--
-- This is intended for testing, as a crude approximation of network delays.
-- More accurate models along these lines are of course possible.
--
delayChannel :: ( MonadSTM m
                , MonadTimer m
                )
             => Duration (Time m)
             -> Channel m a
             -> Channel m a
delayChannel delay = channelEffect (\_ -> return ())
                                   (\_ -> threadDelay delay)
