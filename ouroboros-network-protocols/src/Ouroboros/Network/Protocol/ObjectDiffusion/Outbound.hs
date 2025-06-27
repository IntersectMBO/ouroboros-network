{-# LANGUAGE DataKinds           #-}
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
  , SingBlockingStyle (..)
  , BlockingReplyList (..)
    -- * Execution as a typed protocol
  , objectDiffusionOutboundPeer
  ) where

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
      recvMsgRequestObjectIds :: forall blocking.
                                  SingBlockingStyle blocking
                              -> NumObjectIdsAck
                              -> NumObjectIdsReq
                              -> m (OutboundStObjectIds blocking objectId object m a),
      recvMsgRequestObjects   :: [objectId]
                              -> m (OutboundStObjects objectId object m a),
      recvMsgDone             :: m a
    }

data OutboundStObjectIds blocking objectId object m a where
  SendMsgReplyObjectIds
    :: BlockingReplyList blocking objectId
    -> OutboundStIdle objectId object m a
    -> OutboundStObjectIds blocking objectId object m a

data OutboundStObjects objectId object m a where
  SendMsgReplyObjects
    :: [object]
    -> OutboundStIdle objectId object m a
    -> OutboundStObjects objectId object m a

outboundRun
  :: forall objectId object m a.
     (Monad m)
  => OutboundStIdle objectId object m a
  -> Peer (ObjectDiffusion objectId object) AsServer NonPipelined StIdle m a
outboundRun OutboundStIdle {recvMsgRequestObjectIds, recvMsgRequestObjects, recvMsgDone} =
  Await $ \case
    MsgRequestObjectIds blocking ackNo reqNo -> Effect $ do
      reply <- recvMsgRequestObjectIds blocking ackNo reqNo
      case reply of
        SendMsgReplyObjectIds objectIds k ->
          -- TODO: investigate why GHC cannot infer `SingI`; it used to in
          -- `coot/typed-protocols-rewrite` branch
          return $ case blocking of
            SingBlocking ->
              Yield
                (MsgReplyObjectIds objectIds)
                (outboundRun k)
            SingNonBlocking ->
              Yield
                (MsgReplyObjectIds objectIds)
                (outboundRun k)
    MsgRequestObjects objectIds -> Effect $ do
      SendMsgReplyObjects objects k <- recvMsgRequestObjects objectIds
      return $
        Yield
          (MsgReplyObjects objects)
          (outboundRun k)
    MsgDone -> Effect $ Done <$> recvMsgDone

-- | A non-pipelined 'Peer' representing the 'ObjectDiffusionOutbound'.
objectDiffusionOutboundPeer
  :: forall objectId object m a.
     (Monad m)
  => ObjectDiffusionOutbound objectId object m a
  -> Peer (ObjectDiffusion objectId object) AsServer NonPipelined StInit m a
objectDiffusionOutboundPeer (ObjectDiffusionOutbound outboundSt) =
    Await
      (\MsgInit -> Effect (outboundRun <$> outboundSt))
