{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.TypedProtocol.Channel
  ( Channel (..)
  , fixedInputChannel
  , mvarsAsChannel
  , handlesAsChannel
  , createConnectedChannels
  , createConnectedBufferedChannels
  , createPipelineTestChannels
  , channelEffect
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Internal (smallChunkSize)
import           Numeric.Natural

import qualified System.IO as IO
                   ( Handle, hFlush, hIsEOF )

import           Control.Monad.Class.MonadSTM


-- | One end of a duplex channel. It is a reliable, ordered channel of some
-- medium. The medium does not imply message boundaries, it can be just bytes.
--
data Channel m a = Channel {

       -- | Write output to the channel.
       --
       -- It may raise exceptions (as appropriate for the monad and kind of
       -- channel).
       --
       send :: a -> m (),

       -- | Read some input from the channel, or @Nothing@ to indicate EOF.
       --
       -- Note that having received EOF it is still possible to send.
       -- The EOF condition is however monotonic.
       --
       -- It may raise exceptions (as appropriate for the monad and kind of
       -- channel).
       --
       recv :: m (Maybe a)
     }



-- | A 'Channel' with a fixed input, and where all output is discarded.
--
-- The input is guaranteed to be supplied via 'read' with the given chunk
-- boundaries.
--
-- This is only useful for testing. In particular the fixed chunk boundaries
-- can be used to test that framing and other codecs work with any possible
-- chunking.
--
fixedInputChannel :: MonadSTM m => [a] -> m (Channel m a)
fixedInputChannel xs0 = do
    v <- atomically $ newTVar xs0
    return Channel {send, recv = recv v}
  where
    recv v = atomically $ do
               xs <- readTVar v
               case xs of
                 []      -> return Nothing
                 (x:xs') -> writeTVar v xs' >> return (Just x)

    send _ = return ()


-- | Make a 'Channel' from a pair of 'TMVar's, one for reading and one for
-- writing.
--
mvarsAsChannel :: MonadSTM m
               => TMVar m (Maybe a)
               -> TMVar m (Maybe a)
               -> Channel m a 
mvarsAsChannel bufferRead bufferWrite =
    Channel{send, recv}
  where
    send x = atomically (putTMVar bufferWrite (Just x))
    recv   = atomically (takeTMVar bufferRead)


-- | Create a pair of channels that are connected via one-place buffers.
--
-- This is primarily useful for testing protocols.
--
createConnectedChannels :: MonadSTM m => m (Channel m a, Channel m a)
createConnectedChannels = do
    -- Create two TMVars to act as the channel buffer (one for each direction)
    -- and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newEmptyTMVar
    bufferB <- atomically $ newEmptyTMVar

    return (mvarsAsChannel bufferB bufferA,
            mvarsAsChannel bufferA bufferB)


-- | Create a pair of channels that are connected via N-place buffers.
--
-- This variant /blocks/ when 'send' would exceed the maximum buffer size.
-- Use this variant when you want the environment rather than the 'Peer' to
-- limit the pipelining.
--
-- This is primarily useful for testing protocols.
--
createConnectedBufferedChannels :: MonadSTM m
                                => Natural -> m (Channel m a, Channel m a)
createConnectedBufferedChannels sz = do
    -- Create two TBQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newTBQueue sz
    bufferB <- atomically $ newTBQueue sz

    return (queuesAsChannel bufferB bufferA,
            queuesAsChannel bufferA bufferB)
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x = atomically (writeTBQueue bufferWrite x)
        recv   = atomically (Just <$> readTBQueue bufferRead)


-- | Create a pair of channels that are connected via N-place buffers.
--
-- This variant /fails/ when  'send' would exceed the maximum buffer size.
-- Use this variant when you want the 'PeerPipelined' to limit the pipelining
-- itself, and you want to check that it does not exceed the expected level of
-- pipelining.
--
-- This is primarily useful for testing protocols.
--
createPipelineTestChannels :: MonadSTM m
                           => Natural -> m (Channel m a, Channel m a)
createPipelineTestChannels sz = do
    -- Create two TBQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newTBQueue sz
    bufferB <- atomically $ newTBQueue sz

    return (queuesAsChannel bufferB bufferA,
            queuesAsChannel bufferA bufferB)
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x = atomically $ do
                   full <- isFullTBQueue bufferWrite
                   if full then fail failureMsg
                           else writeTBQueue bufferWrite x
        recv   = atomically (Just <$> readTBQueue bufferRead)

    failureMsg = "createPipelineTestChannels: "
              ++ "maximum pipeline depth exceeded: " ++ show sz


-- | Make a 'Channel' from a pair of IO 'Handle's, one for reading and one
-- for writing.
--
-- The Handles should be open in the appropriate read or write mode, and in
-- binary mode. Writes are flushed after each write, so it is safe to use
-- a buffering mode.
--
-- For bidirectional handles it is safe to pass the same handle for both.
--
handlesAsChannel :: IO.Handle -- ^ Read handle
                 -> IO.Handle -- ^ Write handle
                 -> Channel IO LBS.ByteString
handlesAsChannel hndRead hndWrite =
    Channel{send, recv}
  where
    send :: LBS.ByteString -> IO ()
    send chunk = do
      LBS.hPut hndWrite chunk
      IO.hFlush hndWrite

    recv :: IO (Maybe LBS.ByteString)
    recv = do
      eof <- IO.hIsEOF hndRead
      if eof
        then return Nothing
        else Just . LBS.fromStrict <$> BS.hGetSome hndRead smallChunkSize


-- | Transform a channel to add an extra action before /every/ send and after
-- /every/ receive.
--
channelEffect :: forall m a. 
                 Monad m
              => (a -> m ())        -- ^ Action before 'send'
              -> (Maybe a -> m ())  -- ^ Action after 'recv'
              -> Channel m a
              -> Channel m a
channelEffect beforeSend afterRecv Channel{send, recv} =
    Channel{
      send = \x -> do
        beforeSend x
        send x

    , recv = do
        mx <- recv
        afterRecv mx
        return mx
    }

