{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A view of the object diffusion protocol from the point of view of
-- the server.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
module Ouroboros.Network.Protocol.ObjectDiffusion.Server
  ( -- * Protocol type for the server

    -- | The protocol states from the point of view of the server.
    ObjectDiffusionServerPipelined (..),
    ServerStIdle (..),
    Collect (..),

    -- * Execution as a typed protocol
    objectDiffusionServerPeerPipelined,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Server
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

data ObjectDiffusionServerPipelined objectId object m a where
  ObjectDiffusionServerPipelined ::
    m (ServerStIdle Z objectId object m a) ->
    ObjectDiffusionServerPipelined objectId object m a

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

data ServerStIdle (n :: N) objectId object m a where
  SendMsgRequestObjectIdsBlocking ::
    -- | number of objectIds to acknowledge
    NumObjectIdsToAck ->
    -- | number of objectIds to request
    NumObjectIdsToReq ->
    -- | Result if done
    m a ->
    ( NonEmpty (objectId, SizeInBytes) ->
      m (ServerStIdle Z objectId object m a)
    ) ->
    ServerStIdle Z objectId object m a
  SendMsgRequestObjectIdsPipelined ::
    NumObjectIdsToAck ->
    NumObjectIdsToReq ->
    m (ServerStIdle (S n) objectId object m a) ->
    ServerStIdle n objectId object m a
  SendMsgRequestObjectsPipelined ::
    [objectId] ->
    m (ServerStIdle (S n) objectId object m a) ->
    ServerStIdle n objectId object m a
  -- | Collect a pipelined result.
  CollectPipelined ::
    Maybe (ServerStIdle (S n) objectId object m a) ->
    (Collect objectId object -> m (ServerStIdle n objectId object m a)) ->
    ServerStIdle (S n) objectId object m a

-- | Transform a 'ObjectDiffusionServerPipelined' into a 'PeerPipelined'.
objectDiffusionServerPeerPipelined ::
  forall objectId object m a.
  (Functor m) =>
  ObjectDiffusionServerPipelined objectId object m a ->
  ServerPipelined (ObjectDiffusion objectId object) StInit m a
objectDiffusionServerPeerPipelined (ObjectDiffusionServerPipelined server) =
  ServerPipelined $
    -- We need to assist GHC to infer the existentially quantified `c` as
    -- `Collect objectId object`
    Await @_ @(Pipelined Z (Collect objectId object))
      (\MsgInit -> Effect (go <$> server))
  where
    go ::
      forall (n :: N).
      ServerStIdle n objectId object m a ->
      Server (ObjectDiffusion objectId object) (Pipelined n (Collect objectId object)) StIdle m a

    go (SendMsgRequestObjectIdsBlocking ackNo reqNo kDone k) =
      Yield
        (MsgRequestObjectIds SingBlocking ackNo reqNo)
        $ Await
        $ \case
          MsgDone -> Effect (Done <$> kDone)
          MsgReplyObjectIds (BlockingReply objectIds) -> Effect (go <$> k objectIds)
    go (SendMsgRequestObjectIdsPipelined ackNo reqNo k) =
      YieldPipelined
        (MsgRequestObjectIds SingNonBlocking ackNo reqNo)
        (ReceiverAwait $ \(MsgReplyObjectIds (NonBlockingReply objectIds)) -> ReceiverDone (CollectObjectIds reqNo objectIds))
        (Effect (go <$> k))
    go (SendMsgRequestObjectsPipelined objectIds k) =
      YieldPipelined
        (MsgRequestObjects objectIds)
        (ReceiverAwait $ \(MsgReplyObjects objects) -> ReceiverDone (CollectObjects objectIds objects))
        (Effect (go <$> k))
    go (CollectPipelined mNone collect) =
      Collect
        (fmap go mNone)
        (Effect . fmap go . collect)
