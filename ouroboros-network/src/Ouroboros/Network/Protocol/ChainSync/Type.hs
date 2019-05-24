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
module Ouroboros.Network.Protocol.ChainSync.Type (
    ChainSync(..),
    Point,
    StNextKind(..),
    TokNextKind(..),
    Message(..),
    ClientHasAgency(..),
    ServerHasAgency(..),
    NobodyHasAgency(..),
  ) where

import Network.TypedProtocol.Core
import Ouroboros.Network.Block (Point, StandardHash)


-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
data ChainSync header where

  -- | Both client and server are idle. The client can send a request and
  -- the server is waiting for a request.
  StIdle      :: ChainSync header

  -- | The client has sent a next update request. The client is now waiting
  -- for a response, and the server is busy getting ready to send a response.
  -- There are two possibilities here, since the server can send a reply
  -- immediately or it can send an initial await message followed later by
  -- the normal reply.
  StNext      :: StNextKind -> ChainSync header

  -- | The client has sent an intersection request. The client is now waiting
  -- for a response, and the server is busy getting ready to send a response.
  StIntersect :: ChainSync header

  -- | Both the client and server are in the terminal state. They're done.
  StDone      :: ChainSync header

-- | Sub-cases of the 'StNext' state. This is needed since the server can
-- either send one reply back, or two.
--
data StNextKind where
  -- | The server can reply or send an await msg.
  StCanAwait  :: StNextKind
  -- | The server must now reply, having already sent an await message.
  StMustReply :: StNextKind


instance Protocol (ChainSync header) where

  -- | The messages in the chain sync protocol.
  --
  -- In this protocol the consumer always initiates things and the producer
  -- replies.
  --
  data Message (ChainSync header) from to where

    -- | Request the next update from the producer. The response can be a roll
    -- forward, a roll back or wait.
    --
    MsgRequestNext :: Message (ChainSync header)
                              StIdle (StNext StCanAwait)

    -- | Acknowledge the request but require the consumer to wait for the next
    -- update. This means that the consumer is synced with the producer, and
    -- the producer is waiting for its own chain state to change.
    --
    MsgAwaitReply :: Message (ChainSync header)
                             (StNext StCanAwait) (StNext StMustReply)

    -- | Tell the consumer to extend their chain with the given header.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgRollForward :: header -> Point header
                   -> Message (ChainSync header)
                              (StNext any) StIdle

    -- | Tell the consumer to roll back to a given point on their chain.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgRollBackward :: Point header -> Point header
                    -> Message (ChainSync header)
                               (StNext any) StIdle

    -- | Ask the producer to try to find an improved intersection point between
    -- the consumer and producer's chains. The consumer sends a sequence of
    -- points and it is up to the producer to find the first intersection point
    -- on its chain and send it back to the consumer.
    --
    MsgFindIntersect :: [Point header]
                     -> Message (ChainSync header)
                                StIdle StIntersect

    -- | The reply to the consumer about an intersection found, but /only/ if this
    -- is an improvement over previously established intersection point. The
    -- consumer can decide weather to send more points.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgIntersectImproved  :: Point header -> Point header
                          -> Message (ChainSync header)
                                     StIntersect StIdle

    -- | The reply to the consumer that no intersection was found: none of the
    -- points the consumer supplied are on the producer chain.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgIntersectUnchanged :: Point header
                          -> Message (ChainSync header)
                                     StIntersect StIdle

    -- | Terminating messages
    --
    MsgDone :: Message (ChainSync header)
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

  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}


data TokNextKind (k :: StNextKind) where
  TokCanAwait  :: TokNextKind StCanAwait
  TokMustReply :: TokNextKind StMustReply


instance (Show header, StandardHash header)
      => Show (Message (ChainSync header) from to) where
  show MsgRequestNext               = "MsgRequestNext"
  show MsgAwaitReply                = "MsgAwaitReply"
  show (MsgRollForward h tip)       = "MsgRollForward " ++ show h ++ " " ++ show tip
  show (MsgRollBackward p tip)      = "MsgRollBackward " ++ show p ++ " " ++ show tip
  show (MsgFindIntersect ps)        = "MsgFindIntersect " ++ show ps
  show (MsgIntersectImproved p tip) = "MsgIntersectImproved " ++ show p ++ " " ++ show tip
  show (MsgIntersectUnchanged p)    = "MsgIntersectUnchanged " ++ show p
  show MsgDone{}                    = "MsgDone"
