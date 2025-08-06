{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A view of the object diffusion protocol from the point of view of
-- the inbound.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
module Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
  ( -- * Protocol type for the inbound

    -- | The protocol states from the point of view of the inbound.
    ObjectDiffusionInboundPipelined (..),
    InboundStIdle (..),
    Collect (..),

    -- * Execution as a typed protocol
    objectDiffusionInboundClientPeerPipelined,
    objectDiffusionInboundServerPeerPipelined,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

data ObjectDiffusionInboundPipelined objectId object m a where
  ObjectDiffusionInboundPipelined ::
    m (InboundStIdle Z objectId object m a) ->
    ObjectDiffusionInboundPipelined objectId object m a

-- | This is the type of the pipelined results, collected by 'CollectPipelined'.
-- This protocol can pipeline requests for object ids and objects,
-- so we use a sum of either for collecting the responses.
data Collect objectId object
  = -- | The result of 'SendMsgRequestObjectIdsPipelined'. It also carries
    -- the number of objectIds originally requested.
    CollectObjectIds NumObjectIdsToReq [(objectId, SizeInBytes)]
  | -- | The result of 'SendMsgRequestObjectsPipelined'. The actual reply only
    -- contains the objects sent, but this pairs them up with the
    -- objects requested. This is because the peer can determine that
    -- some objects are no longer needed.
    CollectObjects [objectId] [object]

data InboundStIdle (n :: N) objectId object m a where
  SendMsgRequestObjectIdsBlocking ::
    -- | number of objectIds to acknowledge
    NumObjectIdsToAck ->
    -- | number of objectIds to request
    NumObjectIdsToReq ->
    -- | Result if done
    m a ->
    ( NonEmpty (objectId, SizeInBytes) ->
      m (InboundStIdle Z objectId object m a)
    ) ->
    InboundStIdle Z objectId object m a
  SendMsgRequestObjectIdsPipelined ::
    NumObjectIdsToAck ->
    NumObjectIdsToReq ->
    m (InboundStIdle (S n) objectId object m a) ->
    InboundStIdle n objectId object m a
  SendMsgRequestObjectsPipelined ::
    [objectId] ->
    m (InboundStIdle (S n) objectId object m a) ->
    InboundStIdle n objectId object m a
  -- | Collect a pipelined result.
  CollectPipelined ::
    Maybe (InboundStIdle (S n) objectId object m a) ->
    (Collect objectId object -> m (InboundStIdle n objectId object m a)) ->
    InboundStIdle (S n) objectId object m a

inboundRun :: forall (initAgency :: Agency) (n :: N) objectId object m a.
  (Functor m) =>
      InboundStIdle n objectId object m a ->
      Peer (ObjectDiffusion initAgency objectId object) AsInbound (Pipelined n (Collect objectId object)) StIdle m a

inboundRun (SendMsgRequestObjectIdsBlocking ackNo reqNo kDone k) =
      Yield ReflInboundAgency
        (MsgRequestObjectIds SingBlocking ackNo reqNo)
        $ Await ReflOutboundAgency
        $ \case
            MsgDone -> Effect (Done ReflNobodyAgency <$> kDone)
            MsgReplyObjectIds (BlockingReply objectIds) -> Effect (inboundRun <$> k objectIds)
inboundRun (SendMsgRequestObjectIdsPipelined ackNo reqNo k) =
      YieldPipelined ReflInboundAgency
        (MsgRequestObjectIds SingNonBlocking ackNo reqNo)
        (ReceiverAwait ReflOutboundAgency $ \(MsgReplyObjectIds (NonBlockingReply objectIds)) -> ReceiverDone (CollectObjectIds reqNo objectIds))
        (Effect (inboundRun <$> k))
inboundRun (SendMsgRequestObjectsPipelined objectIds k) =
      YieldPipelined ReflInboundAgency
        (MsgRequestObjects objectIds)
        (ReceiverAwait ReflOutboundAgency $ \(MsgReplyObjects objects) -> ReceiverDone (CollectObjects objectIds objects))
        (Effect (inboundRun <$> k))
inboundRun (CollectPipelined mNone collect) =
      Collect
        (fmap inboundRun mNone)
        (Effect . fmap inboundRun . collect)

-- | Transform a 'ObjectDiffusionInboundPipelined' into a 'PeerPipelined'.
objectDiffusionInboundClientPeerPipelined ::
  forall objectId object m a.
  (Functor m) =>
  ObjectDiffusionInboundPipelined objectId object m a ->
  PeerPipelined (ObjectDiffusion InboundAgency objectId object) AsInbound StInit m a
objectDiffusionInboundClientPeerPipelined (ObjectDiffusionInboundPipelined inboundSt) =
  PeerPipelined $
    Yield ReflInboundAgency MsgInit $
    Effect $
      inboundRun <$> inboundSt

objectDiffusionInboundServerPeerPipelined ::
  forall objectId object m a.
  (Functor m) =>
  ObjectDiffusionInboundPipelined objectId object m a ->
  PeerPipelined (ObjectDiffusion OutboundAgency objectId object) AsInbound StInit m a
objectDiffusionInboundServerPeerPipelined (ObjectDiffusionInboundPipelined inboundSt) =
  PeerPipelined $
     Await @_ @_ @(Pipelined Z (Collect objectId object)) ReflOutboundAgency
      (\MsgInit -> Effect (inboundRun <$> inboundSt))
