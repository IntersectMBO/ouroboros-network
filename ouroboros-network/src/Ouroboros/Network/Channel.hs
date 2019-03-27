{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}

-- | An extension of 'Network.TypedProtocol.Channel', with additional 'Channel'
-- implementations.
--
module Ouroboros.Network.Channel
  ( module Network.TypedProtocol.Channel
  , createBufferConnectedChannels
  , createPipeConnectedChannels
  , createSocketConnectedChannels
  , withFifosAsChannel
  , socketAsChannel
  , delayChannel
  , loggingChannel
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)
import           Data.Time.Clock (DiffTime)
import qualified System.Process as IO (createPipe)
import qualified System.IO      as IO
                   ( withFile, IOMode(..) )
import qualified Network.Socket            as Socket hiding (send, recv)
import qualified Network.Socket.ByteString as Socket

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadSay

import           Network.TypedProtocol.Channel


-- | Create a pair of 'Channel's that are connected internally.
--
-- This is intended for inter-thread communication, such as between a
-- multiplexing thread and a thread running a peer.
--
-- It uses lazy 'ByteString's but it ensures that data written to the channel
-- is /fully evaluated/ first. This ensures that any work to serialise the data
-- takes place on the /writer side and not the reader side/.
--
createBufferConnectedChannels :: forall m. MonadSTM m
                              => m (Channel m LBS.ByteString,
                                    Channel m LBS.ByteString)
createBufferConnectedChannels = do
    bufferA <- newEmptyTMVarM
    bufferB <- newEmptyTMVarM

    return (buffersAsChannel bufferB bufferA,
            buffersAsChannel bufferA bufferB)
  where
    buffersAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send :: LBS.ByteString -> m ()
        send x = sequence_ [ atomically (putTMVar bufferWrite c)
                           | !c <- LBS.toChunks x ]
                           -- Evaluate the chunk c /before/ doing the STM
                           -- transaction to write it to the buffer.

        recv :: m (Maybe LBS.ByteString)
        recv   = Just . LBS.fromStrict <$> atomically (takeTMVar bufferRead)


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
             => DiffTime
             -> Channel m a
             -> Channel m a
delayChannel delay = channelEffect (\_ -> return ())
                                   (\_ -> threadDelay delay)


-- | Channel which logs sent and received messages.
--
-- TODO: use a proper logger rather than @'MonadSay'@ constraint.
loggingChannel :: ( MonadSay m
                  , Show id
                  , Show a
                  )
               => id
               -> Channel m a
               -> Channel m a
loggingChannel ident Channel{send,recv} =
  Channel {
    send = loggingSend,
    recv = loggingRecv
  }
 where
  loggingSend a = do
    say (show ident ++ ":send:" ++ show a)
    send a

  loggingRecv = do
    msg <- recv
    case msg of
      Nothing -> return ()
      Just a  -> say (show ident ++ ":recv:" ++ show a)
    return msg

