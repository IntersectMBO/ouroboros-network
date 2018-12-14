{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol
  ( MsgConsumer(..)
  , MsgProducer(..)
  , loggingSend
  , loggingRecv
  )where

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Point (..))
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Serialise

{-# ANN module "HLint: ignore Use readTVarIO" #-}

--
-- IPC based protocol
--

-- | In this protocol the consumer always initiates things and the producer
-- replies. This is the type of messages that the consumer sends.
data MsgConsumer block
  = MsgRequestNext
  -- ^ Request next block from the producer
  | MsgSetHead [Point block]
  -- ^
  -- Send set of points, it is up to the producer to find the intersection
  -- point on its chain and send it back to the consumer.
    deriving (Eq, Show)

-- | This is the type of messages that the producer sends.
data MsgProducer block
  = MsgRollForward  block
  -- ^ Ask the consumer to roll forward to a given block
  | MsgRollBackward (Point block)
  -- ^
  -- Ask the consumer to roll back to a given Point on its chain
  | MsgAwaitReply
  -- ^
  -- Inform the consumer to await for next instructions; This means that the
  -- producer is synced with the consumer end and its awaiting for its chain to
  -- be changed.
  | MsgIntersectImproved (Point block) (Point block)
  -- ^
  -- Sends to consumer found intersection, but only if this is an improvement
  -- over previously established intersection point.  The consumer
  -- will decide weather to send more points.  They should all be newer than the
  -- received intersection.  The first point is the improved point, the second
  -- is the current tip.
  | MsgIntersectUnchanged
  -- ^
  -- After receiving intersection points from the consumer it maybe happen that
  -- none of the points is on the producer chain; in this case
  -- @'MsgIntersectUnchanged'@ is send back.
    deriving (Eq, Show)

-- | A wrapper for send that logs the messages
--
loggingSend :: (Show msg, MonadSay m, Show id) => id -> (msg -> m a) -> msg -> m a
loggingSend ident send msg = do
    say $ (show ident) ++ ":send: " ++ show msg
    send msg

-- | A wrapper for recv that logs the messages
--
loggingRecv :: (Show msg, MonadSay m, Show id) => id -> m msg -> m msg
loggingRecv ident recv = do
    msg <- recv
    say $ (show ident) ++ ":recv: " ++ show msg
    return msg

--
-- Serialisation
--

encodeMessage :: Word -> Word -> Encoding -> Encoding
encodeMessage conversationId messageTag messageBody =
    encodeListLen 3
 <> encodeWord conversationId
 <> encodeWord messageTag
 <> messageBody

instance HasHeader block => Serialise (MsgConsumer block) where

    encode MsgRequestNext  = encodeMessage 1 0 $ encodeNull
    encode (MsgSetHead ps) = encodeMessage 1 1 $ encode ps

    decode = do
      decodeListLenOf 3
      decodeWordOf 1
      tag <- decodeWord
      case tag of
        0 -> MsgRequestNext <$ decodeNull
        1 -> MsgSetHead <$> decode
        _ -> fail "MsgConsumer unexpected tag"

instance (Serialise block, HasHeader block) => Serialise (MsgProducer block) where

    encode (MsgRollForward  b)        = encodeMessage 2 0 $ encode b
    encode (MsgRollBackward p)        = encodeMessage 2 1 $ encode p
    encode  MsgAwaitReply             = encodeMessage 2 2 $ encodeNull
    encode (MsgIntersectImproved p t) = encodeMessage 2 3 $ encode (p, t)
    encode  MsgIntersectUnchanged     = encodeMessage 2 4 $ encodeNull

    decode = do
      decodeListLenOf 3
      decodeWordOf 2
      tag <- decodeWord
      case tag of
        0 -> MsgRollForward        <$> decode
        1 -> MsgRollBackward       <$> decode
        2 -> MsgAwaitReply         <$  decodeNull
        3 -> uncurry MsgIntersectImproved <$> decode
        4 -> MsgIntersectUnchanged <$  decodeNull
        _ -> fail "MsgProducer unexpected tag"
