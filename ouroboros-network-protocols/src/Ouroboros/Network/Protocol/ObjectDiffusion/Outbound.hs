{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the object diffusion protocol from the point of view of
-- the outbound peer.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, 'objectDiffusionOutboundPeer' is provided for conversion
-- into the typed protocol.
module Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
  ( -- * Protocol type for the outbound

    -- | The protocol states from the point of view of the outbound.
    ObjectDiffusionOutbound (..),
    OutboundStIdle (..),
    OutboundStObjectIds (..),
    OutboundStObjects (..),
    SingBlockingStyle (..),
    BlockingReplyList (..),

    -- * Execution as a typed protocol
    objectDiffusionOutboundServerPeer,
    objectDiffusionOutboundClientPeer,
  )
where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

-- | The outbound side of the object diffusion protocol.
--
-- The peer in the outbound role submits objects to the peer in the server
-- role.
newtype ObjectDiffusionOutbound objectId object m a = ObjectDiffusionOutbound
  { runObjectDiffusionOutbound :: m (OutboundStIdle objectId object m a)
  }

-- | In the 'StIdle' protocol state, the outbound does not have agency. Instead
-- it is waiting for:
--
-- * a request for object ids (blocking or non-blocking)
-- * a request for a given list of objects
-- * a termination message
--
-- It must be prepared to handle any of these.
data OutboundStIdle objectId object m a = OutboundStIdle
  { recvMsgRequestObjectIds ::
      forall blocking.
      SingBlockingStyle blocking ->
      NumObjectIdsToAck ->
      NumObjectIdsToReq ->
      m (OutboundStObjectIds blocking objectId object m a),
    recvMsgRequestObjects ::
      [objectId] ->
      m (OutboundStObjects objectId object m a)
  }

data OutboundStObjectIds blocking objectId object m a where
  SendMsgReplyObjectIds ::
    BlockingReplyList blocking (objectId, SizeInBytes) ->
    OutboundStIdle objectId object m a ->
    OutboundStObjectIds blocking objectId object m a
  -- | In the blocking case, the outbound can terminate the protocol. This could
  -- be used when the outbound knows there will be no more objects to submit.
  SendMsgDone :: a -> OutboundStObjectIds StBlocking objectId object m a

data OutboundStObjects objectId object m a where
  SendMsgReplyObjects ::
    [object] ->
    OutboundStIdle objectId object m a ->
    OutboundStObjects objectId object m a

outboundRun ::
  forall (initAgency :: Agency) objectId object m a.
  (Monad m) =>
  OutboundStIdle objectId object m a ->
  Peer (ObjectDiffusion initAgency objectId object) AsOutbound NonPipelined StIdle m a
outboundRun OutboundStIdle {recvMsgRequestObjectIds, recvMsgRequestObjects} =
  Await ReflInboundAgency $ \msg -> case msg of
    MsgRequestObjectIds blocking ackNo reqNo -> Effect $ do
      reply <- recvMsgRequestObjectIds blocking ackNo reqNo
      case reply of
        SendMsgReplyObjectIds objectIds k ->
          -- TODO: investigate why GHC cannot infer `SingI`; it used to in
          -- `coot/typed-protocols-rewrite` branch
          return $ case blocking of
            SingBlocking ->
              Yield ReflOutboundAgency
                (MsgReplyObjectIds objectIds)
                (outboundRun k)
            SingNonBlocking ->
              Yield ReflOutboundAgency
                (MsgReplyObjectIds objectIds)
                (outboundRun k)
        SendMsgDone result ->
          return $
            Yield ReflOutboundAgency
              MsgDone
              (Done ReflNobodyAgency result)
    MsgRequestObjects objectIds -> Effect $ do
      SendMsgReplyObjects objects k <- recvMsgRequestObjects objectIds
      return $
        Yield ReflOutboundAgency
          (MsgReplyObjects objects)
          (outboundRun k)

-- | A non-pipelined 'Peer' representing the 'ObjectDiffusionOutbound'.
objectDiffusionOutboundServerPeer ::
  forall objectId object m a.
  (Monad m) =>
  ObjectDiffusionOutbound objectId object m a ->
  Peer (ObjectDiffusion InboundAgency objectId object) AsOutbound NonPipelined StInit m a
objectDiffusionOutboundServerPeer (ObjectDiffusionOutbound outboundSt) =
    -- We need to assist GHC to infer the existentially quantified `c` as
    -- `Collect objectId object`
    Await ReflInboundAgency
      (\MsgInit -> Effect (outboundRun <$> outboundSt))

objectDiffusionOutboundClientPeer ::
  forall objectId object m a.
  (Monad m) =>
  ObjectDiffusionOutbound objectId object m a ->
  Peer (ObjectDiffusion OutboundAgency objectId object) AsOutbound NonPipelined StInit m a
objectDiffusionOutboundClientPeer (ObjectDiffusionOutbound outboundSt) =
  Yield ReflOutboundAgency MsgInit $
    Effect $ outboundRun <$> outboundSt
