{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Network.TypedProtocol.Channel
  ( Channel (..)
  , hoistChannel
  , isoKleisliChannel
  , fixedInputChannel
  , mvarsAsChannel
  , handlesAsChannel
  , createConnectedChannels
  , createConnectedBoundedChannels
  , createConnectedDelayChannels
  , createPipelineTestChannels
  , channelEffect
  , delayChannel
  , loggingChannel
  ) where

import           Control.Monad ((>=>), when)
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Internal (smallChunkSize)
import           Data.Time.Clock (DiffTime,
                                  diffTimeToPicoseconds, picosecondsToDiffTime)
import           Numeric.Natural
import           System.Random (RandomGen(..), Random(..))

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


-- | Given an isomorphism between @a@ and @b@ (in Kleisli category), transform
-- a @'Channel' m a@ into @'Channel' m b@.
--
isoKleisliChannel
  :: forall a b m. Monad m
  => (a -> m b)
  -> (b -> m a)
  -> Channel m a
  -> Channel m b
isoKleisliChannel f finv Channel{send, recv} = Channel {
    send = finv >=> send,
    recv = recv >>= traverse f
  }


hoistChannel
  :: (forall x . m x -> n x)
  -> Channel m a
  -> Channel n a
hoistChannel nat channel = Channel
  { send = nat . send channel
  , recv = nat (recv channel)
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
               => LazyTMVar m a
               -> LazyTMVar m a
               -> Channel m a 
mvarsAsChannel bufferRead bufferWrite =
    Channel{send, recv}
  where
    send x = atomically (putTMVar bufferWrite x)
    recv   = atomically (Just <$> takeTMVar bufferRead)


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


-- | Create a pair of channels that are connected via a bounded queue.
--
-- This variant /blocks/ when 'send' would exceed the maximum buffer size.
-- Use this variant when you want the environment rather than the 'Peer' to
-- limit the pipelining.
--
-- This is primarily useful for testing protocols.
--
createConnectedBoundedChannels :: MonadSTM m
                               => Natural -> m (Channel m a, Channel m a)
createConnectedBoundedChannels sz = do
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


-- | Create a pair of channels that are connected via buffers with delays.
--
-- This is a crude approximation of asynchronous network connections where
-- there is delay across the connection, and a limit on in-flight data.
--
-- The buffer size is bounded. It /blocks/ when 'send' would exceed the maximum
-- buffer size. The size per channel element is provided by a function, so this
-- can be size in bytes or a fixed size (e.g. 1 per element), so the max size
-- should be interpreted accordingly.
--
-- The delays are modeled in a simplistic "GSV" style:
--
-- * The G is the minimum latency for a 0 sized message.
-- * The S is the time per message size unit.
-- * The V is the variance in latency, which is assumed to be uniform in the
--   range @0..v@.
--
-- The sender is delayed by S. The receiver is delayed until the arrival time
-- which is G + S + V.
--
-- Note that this implementation does not handle the delays correctly if there
-- are multiple writers or multiple readers.
--
-- This is primarily useful for testing protocols.
--
createConnectedDelayChannels
  :: forall m prng a.
     (MonadSTM m, MonadTime m, MonadTimer m, RandomGen prng)
  => (a -> Int)   -- ^ Data size measure
  -> Int          -- ^ Max size of data in-flight
  -> (DiffTime, DiffTime, DiffTime) -- ^ GSV
  -> prng         -- ^ PRNG for sampling V
  -> m (Channel m a, Channel m a)
createConnectedDelayChannels size maxsize (g, s, v) prng0 = do
    let (prngA, prngB) = split prng0
    -- For each direction, create:
    --  - a TQueue for the messages
    --  - a TVar Int to track the size in-flight
    --  - a TVar prng for the sampling from v
    bufferA <- atomically $ (,,) <$> newTQueue <*> newTVar 0 <*> newTVar prngA
    bufferB <- atomically $ (,,) <$> newTQueue <*> newTVar 0 <*> newTVar prngB

    return (asChannel bufferB bufferA,
            asChannel bufferA bufferB)
  where
    asChannel (rBuffer, rSize, _rPRNG)
              (wBuffer, wSize,  wPRNG) =
        Channel{send, recv}
      where
        send x = do
          atomically $ do
            sz <- readTVar wSize
            let !sz' = sz + size x
            check (sz' <= maxsize)
            writeTVar wSize sz'
          threadDelay (s * fromIntegral (size x))
          now <- getMonotonicTime
          atomically $ do
            prng <- readTVar wPRNG
            let (vsample, prng') = randomR (0, diffTimeToPicoseconds v) prng
                delay :: DiffTime
                delay = g + picosecondsToDiffTime vsample
                arrive :: Time m
                arrive = delay `addTime` now
            writeTVar wPRNG prng'
            writeTQueue wBuffer (arrive, x)

        recv = do
          (arrive, x) <- atomically $ readTQueue rBuffer
          now <- getMonotonicTime
          let delay = arrive `diffTime` now
          when (delay > 0) (threadDelay delay)
          atomically $ do
            sz <- readTVar rSize
            let !sz' = sz - size x
            writeTVar rSize sz'
          return (Just x)


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
