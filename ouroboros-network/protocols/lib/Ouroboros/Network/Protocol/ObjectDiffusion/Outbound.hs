{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the object diffusion protocol from the point of view of
-- the outbound/server peer.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, 'objectDiffusionOutboundPeer' is provided for conversion
-- into the typed protocol.
module Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
  ( -- * Protocol type for the outbound
    ObjectDiffusionOutbound (..)
  , OutboundStIdle (..)
  , OutboundStObjectIds (..)
  , OutboundStObjects (..)
    -- * Execution as a typed protocol
  , objectDiffusionOutboundPeer
  ) where

import Data.Singletons (withSingI)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer (Peer)
import Network.TypedProtocol.Peer.Server
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

-- | The outbound side of the object diffusion protocol.
--
-- The peer in the outbound/server role submits objects to the peer in the
-- inbound/client role.
newtype ObjectDiffusionOutbound objectId object m a = ObjectDiffusionOutbound {
      runObjectDiffusionOutbound :: m (OutboundStIdle objectId object m a)
    }

-- | In the 'StIdle' protocol state, the outbound does not have agency. Instead
-- it is waiting for:
--
-- * a request for object ids (blocking or non-blocking)
-- * a request for a given list of objects
-- * a termination message
--
-- It must be prepared to handle any of these.
data OutboundStIdle objectId object m a = OutboundStIdle {
      recvMsgRequestObjectIds :: forall kind.
                                 ObjectIdsRequestKind kind
                              -> NumObjectIdsAck
                              -> NumObjectIdsReq
                              -> m (OutboundStObjectIds kind objectId object m a),
      recvMsgRequestObjects   :: [objectId]
                              -> m (OutboundStObjects objectId object m a),
      recvMsgDone             :: m a
    }

data OutboundStObjectIds kind objectId object m a where
  SendMsgReplyObjectIds
    :: ObjectIdsReplyList kind objectId
    -> OutboundStIdle objectId object m a
    -> OutboundStObjectIds kind objectId object m a
  SendMsgAwaitReply
    :: m (OutboundStObjectIds ('StObjectIdsBlocking 'StMustReply) objectId object m a)
    -> OutboundStObjectIds ('StObjectIdsBlocking 'StCanAwait) objectId object m a
  SendMsgServerIdle
    :: OutboundStIdle objectId object m a
    -> OutboundStObjectIds ('StObjectIdsBlocking 'StMustReply) objectId object m a

data OutboundStObjects objectId object m a where
  SendMsgReplyObjects
    :: [object]
    -> OutboundStIdle objectId object m a
    -> OutboundStObjects objectId object m a

-- | A non-pipelined 'Peer' representing the 'ObjectDiffusionOutbound'.
objectDiffusionOutboundPeer
  :: forall objectId object m a.
     Monad m
  => ObjectDiffusionOutbound objectId object m a
  -> Peer (ObjectDiffusion objectId object) AsServer NonPipelined StInit m a
objectDiffusionOutboundPeer (ObjectDiffusionOutbound outboundSt) =
    Await
      (\MsgInit -> Effect (run <$> outboundSt))
  where
    run
      :: OutboundStIdle objectId object m a
      -> Peer (ObjectDiffusion objectId object) AsServer NonPipelined StIdle m a
    run OutboundStIdle {recvMsgRequestObjectIds, recvMsgRequestObjects, recvMsgDone} =
      Await $ \case
        MsgRequestObjectIds requestKind ackNo reqNo ->
          withSingI (singObjectIdsRequestKind requestKind) $ Effect $ do
            reply <- recvMsgRequestObjectIds requestKind ackNo reqNo
            case reply of
              SendMsgAwaitReply waitForReply ->
                return $
                  Yield
                    MsgAwaitReply
                    (Effect $ do
                      finalReply <- waitForReply
                      pure $ case finalReply of
                        SendMsgServerIdle k ->
                          Yield MsgServerIdle (run k)
                        SendMsgReplyObjectIds objectIds k ->
                          Yield (MsgReplyObjectIds objectIds) (run k))
              SendMsgReplyObjectIds objectIds k ->
                return $
                  Yield
                    (MsgReplyObjectIds objectIds)
                    (run k)
              -- Matching 'SendMsgServerIdle' refines @kind@ to the blocking
              -- 'StMustReply' state, but 'ObjectIdsRequestKind' has no
              -- constructor for that state. A request can only enter the
              -- non-blocking state or blocking 'StCanAwait'; 'StMustReply' is
              -- reached later by sending 'MsgAwaitReply'. The empty case
              -- discharges this statically impossible branch.
              SendMsgServerIdle _ -> case requestKind of {}
        MsgRequestObjects objectIds -> Effect $ do
          SendMsgReplyObjects objects k <- recvMsgRequestObjects objectIds
          return $
            Yield
              (MsgReplyObjects objects)
              (run k)
        MsgDone -> Effect $ Done <$> recvMsgDone
