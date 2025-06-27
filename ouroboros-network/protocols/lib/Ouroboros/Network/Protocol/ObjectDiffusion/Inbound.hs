{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the object diffusion protocol from the point of view of
-- the inbound/client peer.
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
import Network.TypedProtocol.Peer (Peer, PeerPipelined (..))
import Network.TypedProtocol.Peer.Client
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

data ObjectDiffusionInboundPipelined objectId object m a where
  ObjectDiffusionInboundPipelined
    :: InboundStIdle Z objectId object m a
    -> ObjectDiffusionInboundPipelined objectId object m a

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
  SendMsgRequestObjectIdsBlocking
    :: NumObjectIdsAck -- ^ number of objectIds to acknowledge
    -> NumObjectIdsReq -- ^ number of objectIds to request
    -> (NonEmpty objectId -> InboundStIdle Z objectId object m a)
    -> InboundStIdle Z objectId object m a
  SendMsgRequestObjectIdsPipelined
    :: NumObjectIdsAck
    -> NumObjectIdsReq
    -> InboundStIdle (S n) objectId object m a
    -> InboundStIdle n objectId object m a
  SendMsgRequestObjectsPipelined
    :: [objectId]
    -> InboundStIdle (S n) objectId object m a
    -> InboundStIdle n objectId object m a
  CollectPipelined
    :: Maybe (InboundStIdle (S n) objectId object m a)
    -> (Collect objectId object -> InboundStIdle n objectId object m a)
    -> InboundStIdle (S n) objectId object m a
  SendMsgDone
    :: a
    -> InboundStIdle Z objectId object m a
  WithEffect :: m (InboundStIdle n objectId object m a)
    -> InboundStIdle n objectId object m a

inboundRun
  :: forall (n :: N) objectId object m a.
     (Functor m)
  => InboundStIdle n objectId object m a
  -> Peer (ObjectDiffusion objectId object) AsClient (Pipelined n (Collect objectId object)) StIdle m a

inboundRun (SendMsgRequestObjectIdsBlocking ackNo reqNo k) =
      Yield (MsgRequestObjectIds SingBlocking ackNo reqNo)
        $ Await
        $ \case
            MsgReplyObjectIds (BlockingReply objectIds) ->
              inboundRun (k objectIds)
inboundRun (SendMsgRequestObjectIdsPipelined ackNo reqNo k) =
      YieldPipelined
        (MsgRequestObjectIds SingNonBlocking ackNo reqNo)
        (ReceiverAwait
          $ \(MsgReplyObjectIds (NonBlockingReply objectIds)) ->
              ReceiverDone (CollectObjectIds reqNo objectIds)
        )
        (inboundRun k)
inboundRun (SendMsgRequestObjectsPipelined objectIds k) =
      YieldPipelined
        (MsgRequestObjects objectIds)
        (ReceiverAwait
          $ \(MsgReplyObjects objects) ->
              ReceiverDone (CollectObjects objectIds objects)
        )
        (inboundRun k)
inboundRun (CollectPipelined none collect) =
      Collect
        (inboundRun <$> none)
        (inboundRun . collect)
inboundRun (SendMsgDone done) =
      Yield MsgDone $ Done done
inboundRun (WithEffect mNext) =
      Effect $ inboundRun <$> mNext

-- | Transform a 'ObjectDiffusionInboundPipelined' into a 'PeerPipelined'.
objectDiffusionInboundPeerPipelined
  :: forall objectId object m a.
     (Functor m)
  => ObjectDiffusionInboundPipelined objectId object m a
  -> PeerPipelined (ObjectDiffusion objectId object) AsClient StInit m a
objectDiffusionInboundPeerPipelined (ObjectDiffusionInboundPipelined inboundSt) =
  PeerPipelined $ Yield MsgInit $ inboundRun  inboundSt
