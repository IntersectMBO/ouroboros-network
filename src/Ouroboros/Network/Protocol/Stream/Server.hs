{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.Stream.Server
  ( Server (..)
  , newServer
  , streamServer
  ) where

import Numeric.Natural (Natural)
import Streaming (Stream, Of (..), streamFold)

import Protocol.Core
import Ouroboros.Network.Protocol.Stream.Type

import Ouroboros.Network.MonadClass.MonadSTM (MonadSTM (..))

-- |
-- Streaming server, which consists of:
--   * stream of values generated for a given range
--   * a mutable cell which holds the current window size
data Server m rng a = Server
  { serverStream       :: rng -> Stream (Of a) m ()
  , serverReadWindow   :: m Natural
  , serverUpdateWindow :: Natural -> m ()
  }

-- |
-- Create a new streaming server from a stream.
--
newServer
  :: MonadSTM m
  => (rng -> Stream (Of a) m ())
  -> m (Server m rng a)
newServer serverStream = do
  var <- atomically $ newTVar 0
  return $ Server { serverStream
                  , serverReadWindow    = atomically (readTVar var)
                  , serverUpdateWindow  = atomically . writeTVar var
                  }
    
-- |
-- Run @'Server'@ until its stream has finished.
-- Note: @pred (0 :: Natural)@ is an error.
--
streamServer
  :: forall m rng a. Monad m
  => Server m rng a
  -> Peer StreamProtocol (StreamMessage rng a)
     ('Awaiting 'StIdle) ('Finished 'StDone)
     m ()
streamServer Server{..} =
  await $ \req ->
    case req of
      MsgRequest range window ->
        lift $ do
          serverUpdateWindow window
          return $ streamFold done_ lift (construct window) (serverStream range)
 where
  -- finish the stream with @'MsgStreamEnd'@
  done_
    :: () 
    -> Peer StreamProtocol (StreamMessage rng a)
       ('Yielding 'StBusy) ('Finished 'StDone)
       m ()
  done_ x = out MsgStreamEnd (done x)

  -- consume a single value from the stream
  construct
    :: Natural
    -> Of a (Peer StreamProtocol (StreamMessage rng a)
             ('Yielding 'StBusy) next
             m ())
    -> Peer StreamProtocol (StreamMessage rng a)
       ('Yielding 'StBusy) next
       m ()
  construct window (a :> next) = lift $ do
    curWindow <- serverReadWindow
    if curWindow <= 0
      -- release control flow to the consumer and ask to
      -- update the window size; after a response from the client update
      -- the window with the new size and resume streaming.
      then pure $ do
        over MsgRenewWindow $ await $ \req' ->
          case req' of
            MsgUpdateWindow -> lift $ do
              serverUpdateWindow (pred window)
              pure $ part (MsgData a) next
      -- update the window size, send next @'MsgData'@, and loop over the
      -- stream
      else do
        serverUpdateWindow (pred curWindow)
        pure $ part (MsgData a) next
