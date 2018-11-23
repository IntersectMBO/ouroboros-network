{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.BlockStream.Server
  ( runStreamServerPeer
  ) where

import           Control.Monad (forever)

import Protocol.Transition (SomeTransition)
import Protocol.Channel (Channel, useChannelHomogeneous)
import Ouroboros.Network.MonadClass (MonadFork (..))

import Ouroboros.Network.Protocol.Stream.Type (StreamMessage)
import Ouroboros.Network.Protocol.Stream.Server (StreamServer, streamServerPeer)

-- | Spin a stream server which handles all the requests on a single channel.
-- The channel must handle framing of messages, since it is reused.
--
runStreamServerPeer
  :: forall m point body t. MonadFork m
  => Channel m (SomeTransition (StreamMessage (point, point) (point, body)))
  -> StreamServer m (point, point) (point, body) t
  -> m ()
runStreamServerPeer channel server =
  forever $ useChannelHomogeneous channel peer
 where
  peer = streamServerPeer server
