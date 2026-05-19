{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Channel
  ( Channel (..)
  , module Mx
  , fixedInputChannel
  , createConnectedBufferedChannelsUnbounded
  , createConnectedBufferedChannels
  , createConnectedBufferedChannelsSTM
  , createPipelineTestChannels
  ) where

import Numeric.Natural

import Control.Concurrent.Class.MonadSTM.Strict

import Network.Mux.Channel as Mx


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
    v <- newTVarIO xs0
    return Channel {send, recv = recv v}
  where
    recv v = atomically $ do
               xs <- readTVar v
               case xs of
                 []      -> return Nothing
                 (x:xs') -> writeTVar v xs' >> return (Just x)

    send _ = return ()



-- | Create a pair of channels that are connected via two unbounded buffers.
--
-- This is primarily useful for testing protocols.
--
createConnectedBufferedChannelsUnbounded :: forall m a. MonadSTM m
                                         => m (Channel m a, Channel m a)
createConnectedBufferedChannelsUnbounded = do
    -- Create two TQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- atomically newTQueue
    bufferB <- atomically newTQueue

    return (queuesAsChannel bufferB bufferA,
            queuesAsChannel bufferA bufferB)
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x = atomically (writeTQueue bufferWrite x)
        recv   = atomically (Just <$> readTQueue bufferRead)


-- | Create a pair of channels that are connected via N-place buffers.
--
-- This variant /blocks/ when 'send' would exceed the maximum buffer size.
-- Use this variant when you want the environment rather than the 'Peer' to
-- limit the pipelining.
--
-- This is primarily useful for testing protocols.
--
createConnectedBufferedChannels :: forall m a. MonadLabelledSTM m
                                => Natural -> m (Channel m a, Channel m a)
createConnectedBufferedChannels sz = do
    (chan1, chan2) <- atomically $ createConnectedBufferedChannelsSTM sz
    pure (wrap chan1, wrap chan2)
  where
    wrap :: Channel (STM m) a -> Channel m a
    wrap Channel{send, recv} = Channel
      { send    = atomically . send
      , recv    = atomically recv
      }

-- | As 'createConnectedBufferedChannels', but in 'STM'.
--
-- TODO: it should return a pair of `Channel m a`.
createConnectedBufferedChannelsSTM :: MonadLabelledSTM m
                                   => Natural -> STM m (Channel (STM m) a, Channel (STM m) a)
createConnectedBufferedChannelsSTM sz = do
    -- Create two TBQueues to act as the channel buffers (one for each
    -- direction) and use them to make both ends of a bidirectional channel
    bufferA <- newTBQueue sz
    labelTBQueue bufferA "chann-a"
    bufferB <- newTBQueue sz
    labelTBQueue bufferB "chann-b"

    return (queuesAsChannel bufferB bufferA,
            queuesAsChannel bufferA bufferB)
  where
    queuesAsChannel bufferRead bufferWrite =
        Channel{send, recv}
      where
        send x  = writeTBQueue bufferWrite x
        recv    = Just <$> readTBQueue bufferRead


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
                   if full then error failureMsg
                           else writeTBQueue bufferWrite x
        recv   = atomically (Just <$> readTBQueue bufferRead)

    failureMsg = "createPipelineTestChannels: "
              ++ "maximum pipeline depth exceeded: " ++ show sz
