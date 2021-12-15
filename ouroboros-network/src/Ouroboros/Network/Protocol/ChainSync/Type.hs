{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | The type of the chain synchronisation protocol.
--
-- Since we are using a typed protocol framework this is in some sense /the/
-- definition of the protocol: what is allowed and what is not allowed.
--
module Ouroboros.Network.Protocol.ChainSync.Type where

import           Data.Proxy (Proxy (..))

import           Network.TypedProtocol.Core (Protocol (..))

import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))


-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
data ChainSync header point tip where

  -- | Both client and server are idle. The client can send a request and
  -- the server is waiting for a request.
  StIdle      :: ChainSync header point tip

  -- | The client has sent a next update request. The client is now waiting
  -- for a response, and the server is busy getting ready to send a response.
  -- There are two possibilities here, since the server can send a reply
  -- immediately or it can send an initial await message followed later by
  -- the normal reply.
  StNext      :: StNextKind -> ChainSync header point tip

  -- | The client has sent an intersection request. The client is now waiting
  -- for a response, and the server is busy getting ready to send a response.
  StIntersect :: ChainSync header point tip

  -- | Both the client and server are in the terminal state. They're done.
  StDone      :: ChainSync header point tip


instance (ShowProxy header, ShowProxy tip)
      => ShowProxy (ChainSync header point tip) where
    showProxy _ = concat
      [ "ChainSync ("
      , showProxy (Proxy :: Proxy header)
      , ") ("
      , showProxy (Proxy :: Proxy tip)
      , ")"
      ]

-- | Sub-cases of the 'StNext' state. This is needed since the server can
-- either send one reply back, or two.
--
data StNextKind where
  -- | The server can reply or send an await msg.
  StCanAwait  :: StNextKind
  -- | The server must now reply, having already sent an await message.
  StMustReply :: StNextKind


instance Protocol (ChainSync header point tip) where

  -- | The messages in the chain sync protocol.
  --
  -- In this protocol the consumer always initiates things and the producer
  -- replies.
  --
  data Message (ChainSync header point tip) from to where

    -- | Request the next update from the producer. The response can be a roll
    -- forward, a roll back or wait.
    --
    MsgRequestNext :: Message (ChainSync header point tip)
                              StIdle (StNext StCanAwait)

    -- | Acknowledge the request but require the consumer to wait for the next
    -- update. This means that the consumer is synced with the producer, and
    -- the producer is waiting for its own chain state to change.
    --
    MsgAwaitReply :: Message (ChainSync header point tip)
                             (StNext StCanAwait) (StNext StMustReply)

    -- | Tell the consumer to extend their chain with the given header.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgRollForward :: header -> tip
                   -> Message (ChainSync header point tip)
                              (StNext any) StIdle

    -- | Tell the consumer to roll back to a given point on their chain.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgRollBackward :: point -> tip
                    -> Message (ChainSync header point tip)
                               (StNext any) StIdle

    -- | Ask the producer to try to find an improved intersection point between
    -- the consumer and producer's chains. The consumer sends a sequence of
    -- points and it is up to the producer to find the first intersection point
    -- on its chain and send it back to the consumer.
    --
    MsgFindIntersect :: [point]
                     -> Message (ChainSync header point tip)
                                StIdle StIntersect

    -- | The reply to the consumer about an intersection found.
    -- The consumer can decide weather to send more points.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgIntersectFound  :: point -> tip
                       -> Message (ChainSync header point tip)
                                  StIntersect StIdle

    -- | The reply to the consumer that no intersection was found: none of the
    -- points the consumer supplied are on the producer chain.
    --
    -- The message also tells the consumer about the head point of the producer.
    --
    MsgIntersectNotFound :: tip
                         -> Message (ChainSync header point tip)
                                    StIntersect StIdle

    -- | Terminating messages
    --
    MsgDone :: Message (ChainSync header point tip)
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


instance (Show header, Show point, Show tip)
      => Show (Message (ChainSync header point tip) from to) where
  show MsgRequestNext             = "MsgRequestNext"
  show MsgAwaitReply              = "MsgAwaitReply"
  show (MsgRollForward h tip)     = "MsgRollForward "
                                  ++ show h
                                  ++ " "
                                  ++ show tip
  show (MsgRollBackward p tip)    = "MsgRollBackward "
                                  ++ show p
                                  ++ " "
                                  ++ show tip
  show (MsgFindIntersect ps)      = "MsgFindIntersect " ++ show ps
  show (MsgIntersectFound p tip)  = "MsgIntersectFound "
                                  ++ show p
                                  ++ " "
                                  ++ show tip
  show (MsgIntersectNotFound tip) = "MsgIntersectNotFound "
                                  ++ show tip
  show MsgDone{}                  = "MsgDone"

instance Show (ClientHasAgency (st :: ChainSync header point tip)) where
    show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: ChainSync header point tip)) where
    show (TokNext TokCanAwait)  = "TokNext TokCanAwait"
    show (TokNext TokMustReply) = "TokNext TokMustReply"
    show TokIntersect           = "TokIntersect"
