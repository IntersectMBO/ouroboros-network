{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.TypedProtocol.Channel
  ( Channel (..)
  , fixedInputChannel
  , mvarsAsChannel
  , handlesAsChannel
  , createConnectedChannels
  , withFifosAsChannel
  , channelEffect
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Internal (smallChunkSize)

import qualified System.IO      as IO
                   ( Handle, withFile, IOMode(..), hFlush, hIsEOF )

import           Control.Monad.Class.MonadSTM
                   ( MonadSTM, atomically
                   , newTVar, readTVar, writeTVar
                   , TMVar, newEmptyTMVar, putTMVar, takeTMVar )


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
-- This is primarily useful for testing protocols in a simple environment.
--
createConnectedChannels :: MonadSTM m => m (Channel m a, Channel m a)
createConnectedChannels = do
    -- Create two TMVars to act as the channel buffer (one for each direction)
    -- and use them to make both ends of a bidirectional channel
    bufferA <- atomically $ newEmptyTMVar
    bufferB <- atomically $ newEmptyTMVar

    return (mvarsAsChannel bufferB bufferA,
            mvarsAsChannel bufferA bufferB)


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
                 -> Channel IO ByteString
handlesAsChannel hndRead hndWrite =
    Channel{send, recv}
  where
    send :: ByteString -> IO ()
    send chunk = do
      BS.hPut hndWrite chunk
      IO.hFlush hndWrite

    recv :: IO (Maybe ByteString)
    recv = do
      eof <- IO.hIsEOF hndRead
      if eof
        then return Nothing
        else Just <$> BS.hGetSome hndRead smallChunkSize


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
                   -> (Channel IO ByteString -> IO a) -> IO a
withFifosAsChannel fifoPathRead fifoPathWrite action =
    IO.withFile fifoPathRead  IO.ReadMode  $ \hndRead  ->
    IO.withFile fifoPathWrite IO.WriteMode $ \hndWrite ->
      let channel = handlesAsChannel hndRead hndWrite
       in action channel


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

