{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.TypedProtocol.Channel where
{-
  ( Duplex (..)
  , fmapDuplex
  , contramapDuplex
  , hoistDuplex
  , prependDuplexRecv
  , uniformDuplex
  , Channel
  , uniformChannel
  , fixedInputChannel
  , mvarChannels
  , withMVarChannels
  , channelEffect
  , channelSendEffect
  , channelRecvEffect
  ) where
-}

import           Data.Functor (($>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Internal (smallChunkSize)

import qualified System.IO      as IO
                   ( Handle, withFile, IOMode(..), hFlush, hIsEOF )
import qualified System.Process as IO (createPipe)

import           Control.Monad.Class.MonadSTM
                   ( MonadSTM, atomically
                   , TMVar, newEmptyTMVar, putTMVar, takeTMVar )

-- | One end of a duplex channel. It is a reliable, ordered channel of some
-- medium. The medium does not imply message boundaries, it can be just bytes.
--
-- Note that 'send' and 'recv' produce a new 'Channel m a' so that it's
-- possible to have pure channels without using a state monad. Correspondingly,
-- you cannot use /both/ 'send' and 'recv' on a single channel value: if you
-- 'send', you can 'recv' from the 'Channel' that it returns, /not/ the
-- original 'Channel'.
--
data Channel m a = Channel {

       -- | Write output to the channel.
       --
       -- It may raise exceptions (as appropriate for the monad and kind of channel).
       --
       send :: a -> m (Channel m a),

       -- | Read some input from the channel, or @Nothing@ to indicate EOF.
       --
       -- Note that having recieved EOF it is still possible to send, which
       -- is why the @Maybe@ only covers the input and not the tail of the
       -- channel. The EOF condition is however monotonic.
       --
       -- It may raise exceptions (as appropriate for the monad and kind of
       -- channel).
       --
       recv :: m (Maybe a, Channel m a)
     }

createChannel :: (a -> m (Channel m a))   -- ^ @send@ action
              -> m (Maybe a, Channel m a) -- ^ @recv@ action
              -> Channel m a
createChannel = Channel


-- | The 'Channel' abstraction allows context to be carried forward as the
-- channel is unfolded. This generality is not needed however for many
-- imperitive effectful channel implementations.
--
-- This channel construction utility is for such implementations that only
-- need to provide a @send@ and @recv@ action that does not need to return
-- any updated channel context.
--
-- Many I\/O channels are of this style, using some I\/O handle and just
-- performing effects.
--
createSimpleChannel :: Functor m
                    => (a -> m ())  -- ^ @send@ action
                    -> m (Maybe a)  -- ^ @recv@ action
                    -> Channel m a
createSimpleChannel sendSimple recvSimple = channel
  where
    channel = Channel { send, recv }
    send x  = sendSimple x $> channel
    recv    = (\mx -> (mx, channel)) <$> recvSimple


-- | A 'Channel' with a fixed input, and where all output is discarded.
--
-- The input is guaranteed to be supplied via 'read' with the given chunk
-- boundaries.
--
-- This is only useful for testing. In particular the fixed chunk boundaries
-- can be used to test that framing and other codecs work with any possible
-- chunking.
--
fixedInputChannel :: Applicative m => [a] -> Channel m a
fixedInputChannel xs =
    Channel {send, recv}
  where
    -- This is basically an unfoldr
    recv = case xs of
             []      -> pure (Nothing, fixedInputChannel [])
             (x:xs') -> pure (Just x,  fixedInputChannel xs')

    send _ = pure (fixedInputChannel xs)


-- | Make a 'Channel' from a pair of 'TMVar's, one for reading and one for
-- writing.
--
mvarsAsChannel :: MonadSTM m
               => TMVar m (Maybe a)
               -> TMVar m (Maybe a)
               -> Channel m a 
mvarsAsChannel bufferRead bufferWrite =
  createSimpleChannel
    (\x -> atomically (putTMVar bufferWrite (Just x)))
    (atomically (takeTMVar bufferRead))


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
    createSimpleChannel send recv
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
                     -> (Channel IO ByteString -> IO a) -> IO a
withFifosByteChannel fifoPathRead fifoPathWrite action =
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
channelEffect beforeSend afterRecv = go
  where
    go :: Channel m a -> Channel m a
    go chan =
      chan {
        send = \x -> do
          beforeSend x
          chan' <- send chan x
          return (go chan')

      , recv = do
          (mx, chan') <- recv chan
          afterRecv mx
          return (mx, go chan')
      }

{-
-- | Prepend extra data at the head of a 'Channel's receive side.
--
-- This 
--
prependDuplexRecv
  :: ( Functor sm, Applicative rm )
  => [recv]
  -> Duplex sm rm send recv
  -> Duplex sm rm send recv
prependDuplexRecv lst duplex = case lst of
  [] -> duplex
  (i : is) -> Duplex
    { send = fmap (prependDuplexRecv lst) . send duplex
    , recv = pure (Just i, prependDuplexRecv is duplex)
    }
-}
