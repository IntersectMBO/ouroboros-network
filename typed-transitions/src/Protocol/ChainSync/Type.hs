{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Protocol.ChainSync.Type where

import Protocol.Core

-- | States in the chain sync system.
data ChainSyncState where

  -- | Both client and server are idle. The client can send a request and
  -- the server is waiting for a request.
  StIdle          :: ChainSyncState

  -- | The client has sent a next update request. The client is now waiting
  -- for a response, and the server is busy getting ready to send a response.
  -- There are two possibilities here, since the server can send a reply
  -- immediately or it can send an initial await message followed later by
  -- the normal reply.
  StNext          :: StNextKind -> ChainSyncState

  -- | The client has sent an intersection request. The client is now waiting
  -- for a response, and the server is busy getting ready to send a response.
  StIntersect     :: ChainSyncState

  -- | Both the client and server are in the terminal state. They're done.
  StDone          :: ChainSyncState
  --TODO: currently there is no termination mechanism

data StNextKind where
  StCanAwait  :: StNextKind -- ^ The server can reply or send an await msg.
  StMustReply :: StNextKind -- ^ The server must now reply, having already
                                -- sent an await message.


-- | A type to identify the client\/server partition of states in our protocol.
data ChainSyncProtocol

-- | We have to explain to the framework what our states mean, in terms of
-- which party has agency in each state.
--
type instance Partition ChainSyncProtocol st client server terminal =
  ChainSyncStatePartition st client server terminal

-- | Idle states are where it is for the client to send a message,
-- busy states are where the server is expected to send a reply.
--
type family ChainSyncStatePartition st client server terminal where
  ChainSyncStatePartition  StIdle      client server terminal = client
  ChainSyncStatePartition (StNext k)   client server terminal = server
  ChainSyncStatePartition  StIntersect client server terminal = server
  ChainSyncStatePartition  StDone      client server terminal = terminal


-- | The actual messages in our protocol.
--
-- These involve transitions between different states within the 'StChainSync'
-- states.
--
data ChainSyncMessage header point from to where
  MsgRequestNext  ::                    ChainSyncMessage header point  StIdle             (StNext StCanAwait)
  MsgAwaitReply   ::                    ChainSyncMessage header point (StNext StCanAwait) (StNext StMustReply)
  MsgRollForward  :: header -> point -> ChainSyncMessage header point (StNext _any)        StIdle
  MsgRollBackward :: point  -> point -> ChainSyncMessage header point (StNext _any)        StIdle


  MsgFindIntersect      :: [point]        -> ChainSyncMessage header point StIdle StIntersect
  MsgIntersectImproved  :: point -> point -> ChainSyncMessage header point StIntersect StIdle
  MsgIntersectUnchanged ::          point -> ChainSyncMessage header point StIntersect StIdle

instance Show (ChainSyncMessage header point from to) where
  show MsgRequestNext          = "MsgRequestNext"
  show MsgAwaitReply           = "MsgAwaitReply"
  show MsgRollForward{}        = "MsgRollForward"
  show MsgRollBackward{}       = "MsgRollBackward"
  show MsgFindIntersect{}      = "MsgFindIntersect"
  show MsgIntersectImproved{}  = "MsgIntersectImproved"
  show MsgIntersectUnchanged{} = "MsgIntersectUnchanged"

