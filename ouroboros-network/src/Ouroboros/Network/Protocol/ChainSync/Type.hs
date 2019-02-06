{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase         #-}

-- | The type of the chain synchronisation protocol.
--
-- Since we are using a typed protocol framework this is in some sense /the/
-- definition of the protocol: what is allowed and what is not allowed.
--
module Ouroboros.Network.Protocol.ChainSync.Type where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Proofs


-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
data ChainSync header point where

  -- | Both client and server are idle. The client can send a request and
  -- the server is waiting for a request.
  StIdle      :: ChainSync header point

  -- | The client has sent a next update request. The client is now waiting
  -- for a response, and the server is busy getting ready to send a response.
  -- There are two possibilities here, since the server can send a reply
  -- immediately or it can send an initial await message followed later by
  -- the normal reply.
  StNext      :: StNextKind -> ChainSync header point

  -- | The client has sent an intersection request. The client is now waiting
  -- for a response, and the server is busy getting ready to send a response.
  StIntersect :: ChainSync header point

  -- | Both the client and server are in the terminal state. They're done.
  StDone      :: ChainSync header point

-- | Sub-cases of the 'StNext' state. This is needed since the server can
-- either send one reply back, or two.
--
data StNextKind where
  -- | The server can reply or send an await msg.
  StCanAwait  :: StNextKind
  -- | The server must now reply, having already sent an await message.
  StMustReply :: StNextKind


instance Protocol (ChainSync header point) where

  -- | The messages in the chain sync protocol.
  --
  -- In this protocol the consumer always initiates things and the producer
  -- replies.
  --
  data Message (ChainSync header point) from to where

    -- | Request the next update from the producer. The response can be a roll
    -- forward, a roll back or wait.
    --
    MsgRequestNext :: Message (ChainSync header point)
                              StIdle (StNext StCanAwait)

    -- | Acknowledge the request but require the consumer to wait for the next
    -- update. This means that the consumer is synced with the producer, and
    -- the producer is waiting for its own chain state to change.
    --
    MsgAwaitReply :: Message (ChainSync header point)
                             (StNext StCanAwait) (StNext StMustReply)

    -- | Tell the consumer to extend their chain with the given header.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgRollForward :: header -> point
                   -> Message (ChainSync header point)
                              (StNext any) StIdle

    -- | Tell the consumer to roll back to a given point on their chain.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgRollBackward :: point -> point
                    -> Message (ChainSync header point)
                               (StNext any) StIdle

    -- | Ask the producer to try to find an improved intersection point between
    -- the consumer and producer's chains. The consumer sends a sequence of
    -- points and it is up to the producer to find the first intersection point
    -- on its chain and send it back to the consumer.
    --
    MsgFindIntersect :: [point]
                     -> Message (ChainSync header point)
                                StIdle StIntersect

    -- | The reply to the consumer about an intersection found, but /only/ if this
    -- is an improvement over previously established intersection point. The
    -- consumer can decide weather to send more points.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgIntersectImproved  :: point -> point
                          -> Message (ChainSync header point)
                                     StIntersect StIdle

    -- | The reply to the consumer that no intersection was found: none of the
    -- points the consumer supplied are on the producer chain.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgIntersectUnchanged :: point
                          -> Message (ChainSync header point)
                                     StIntersect StIdle

    -- | Terminating messages
    --
    MsgDone :: Message (ChainSync header point)
                       StIdle StDone

  -- | We have to explain to the framework what our states mean, in terms of
  -- which party has agency in each state.
  --
  -- Idle states are where it is for the client to send a message,
  -- busy states are where the server is expected to send a reply.
  --
  data ClientHasAgency st where
    TokIdle      :: ClientHasAgency StIdle

  data ServerHasAgency st where
    TokNext      :: TokNextKind k -> ServerHasAgency (StNext k)
    TokIntersect :: ServerHasAgency StIntersect

  data NobodyHasAgency st where
    TokDone      :: NobodyHasAgency StDone


data TokNextKind (k :: StNextKind) where
  TokCanAwait  :: TokNextKind StCanAwait
  TokMustReply :: TokNextKind StMustReply


instance Show (Message (ChainSync header point) from to) where
  show MsgRequestNext          = "MsgRequestNext"
  show MsgAwaitReply           = "MsgAwaitReply"
  show MsgRollForward{}        = "MsgRollForward"
  show MsgRollBackward{}       = "MsgRollBackward"
  show MsgFindIntersect{}      = "MsgFindIntersect"
  show MsgIntersectImproved{}  = "MsgIntersectImproved"
  show MsgIntersectUnchanged{} = "MsgIntersectUnchanged"
  show MsgDone{}               = "MsgDone"


chainSyncAgencyProofs :: AgencyProofs (ChainSync header point)
chainSyncAgencyProofs = AgencyProofs {
    proofByContradiction_ClientAndServerHaveAgency = \TokIdle tok -> case tok of {},
    proofByContradiction_NobodyAndClientHaveAgency = \TokDone tok -> case tok of {},
    proofByContradiction_NobodyAndServerHaveAgency = \TokDone tok -> case tok of {}
  }
