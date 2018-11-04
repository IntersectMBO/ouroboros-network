{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Network.Protocol.PingPong.Type where

import Ouroboros.Network.Protocol.Typed


-- | States in the ping pong system.
data PingPongState where
  StIdle :: PingPongState
  StBusy :: PingPongState
  StDone :: PingPongState

-- | A type to identify the client\/server partition of states in our protocol.
data PingPongProtocol

-- | We have to explain to the framework what our states mean, in terms of
-- who is expected to send and receive in the different states.
--
type instance Partition PingPongProtocol st client server terminal =
  PingPongStatePartition st client server terminal

-- | Idle states are where it is for the client to send a message,
-- busy states are where the server is expected to send a reply.
--
type family PingPongStatePartition st client server terminal :: k where
  PingPongStatePartition StIdle client server terminal = client
  PingPongStatePartition StBusy client server terminal = server
  PingPongStatePartition StDone client server terminal = terminal

-- | The actual messages in our protocol.
--
-- These involve transitions between different states within the 'StPingPong'
-- states. A ping request goes from idle to busy, and a pong response go from
-- busy to idle.
--
-- This example is so simple that we have all the messages directly as
-- constructors within this type. In more complex cases it may be better to
-- factor all (or related) requests and all responses within one case (in which
-- case the state transitions may depend on the particular message via the
-- usual GADT tricks).
--
data PingPongMessage from to where
  MsgPing :: PingPongMessage StIdle StBusy
  MsgPong :: PingPongMessage StBusy StIdle
  MsgDone :: PingPongMessage StIdle StDone

instance Show (PingPongMessage from to) where
  show MsgPing = "MsgPing"
  show MsgPong = "MsgPong"
  show MsgDone = "MsgDone"

