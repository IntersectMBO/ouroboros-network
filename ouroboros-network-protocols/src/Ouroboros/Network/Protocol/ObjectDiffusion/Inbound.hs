{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | A view of the object diffusion protocol from the point of view of
-- the inbound.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
module Ouroboros.Network.Protocol.ObjectDiffusion.Inbound
  ( -- * Protocol type for the inbound
    ObjectDiffusionInboundPipelined (..)
  , InboundStIdle (..)
  , Collect (..)
    -- * Execution as a typed protocol
  , objectDiffusionInboundPeerPipelined
  ) where

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
    CollectObjectIds NumObjectIdsReq [objectId]
  | -- | The result of 'SendMsgRequestObjectsPipelined'. The actual reply only
    -- contains the objects sent, but this pairs them up with the
    -- objects requested. This is because the peer can determine that
    -- some objects are no longer needed.
    CollectObjects [objectId] [object]

data InboundStIdle (n :: N) objectId object m a where
  SendMsgRequestObjectIdsBlocking ::
    -- | number of objectIds to acknowledge
    NumObjectIdsAck ->
    -- | number of objectIds to request
    NumObjectIdsReq ->
    -- | Result if done
    m a ->
    ( NonEmpty objectId ->
      m (InboundStIdle Z objectId object m a)
    ) ->
    InboundStIdle Z objectId object m a
  SendMsgRequestObjectIdsPipelined ::
    NumObjectIdsAck ->
    NumObjectIdsReq ->
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

inboundRun :: forall (n :: N) objectId object m a.
  (Functor m) =>
      InboundStIdle n objectId object m a ->
      Peer (ObjectDiffusion objectId object) AsClient (Pipelined n (Collect objectId object)) StIdle m a

inboundRun (SendMsgRequestObjectIdsBlocking ackNo reqNo kDone k) =
      Yield ReflClientAgency
        (MsgRequestObjectIds SingBlocking ackNo reqNo)
        $ Await ReflServerAgency
        $ \case
            MsgDone -> Effect (Done ReflNobodyAgency <$> kDone)
            MsgReplyObjectIds (BlockingReply objectIds) -> Effect (inboundRun <$> k objectIds)
inboundRun (SendMsgRequestObjectIdsPipelined ackNo reqNo k) =
      YieldPipelined ReflClientAgency
        (MsgRequestObjectIds SingNonBlocking ackNo reqNo)
        (ReceiverAwait ReflServerAgency $ \(MsgReplyObjectIds (NonBlockingReply objectIds)) -> ReceiverDone (CollectObjectIds reqNo objectIds))
        (Effect (inboundRun <$> k))
inboundRun (SendMsgRequestObjectsPipelined objectIds k) =
      YieldPipelined ReflClientAgency
        (MsgRequestObjects objectIds)
        (ReceiverAwait ReflServerAgency $ \(MsgReplyObjects objects) -> ReceiverDone (CollectObjects objectIds objects))
        (Effect (inboundRun <$> k))
inboundRun (CollectPipelined mNone collect) =
      Collect
        (fmap inboundRun mNone)
        (Effect . fmap inboundRun . collect)

-- | Transform a 'ObjectDiffusionInboundPipelined' into a 'PeerPipelined'.
objectDiffusionInboundPeerPipelined ::
  forall objectId object m a.
  (Functor m) =>
  ObjectDiffusionInboundPipelined objectId object m a ->
  PeerPipelined (ObjectDiffusion objectId object) AsClient StInit m a
objectDiffusionInboundPeerPipelined (ObjectDiffusionInboundPipelined inboundSt) =
  PeerPipelined $
    Yield ReflClientAgency MsgInit $
      Effect $ inboundRun <$> inboundSt
