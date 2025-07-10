{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | A view of the object diffusion protocol from the point of view of
-- the client.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, 'objectDiffusionClientPeer' is provided for conversion
-- into the typed protocol.
module Ouroboros.Network.Protocol.ObjectDiffusion.ClientServer
  ( -- * Protocol type for the client

    -- | The protocol states from the point of view of the client.
    ObjectDiffusionClient (..)
  , InboundStIdle (..)
  , InboundStObjectIds (..)
  , InboundStObjects (..)
  , SingBlockingStyle (..)
  , BlockingReplyList (..)
  , ObjectDiffusionServerPipelined (..)
  , OutboundStIdle (..)
  , Collect (..)

    -- * Execution as a typed protocol
  , objectDiffusionClientPeer
  )
where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client (Client)
import           Ouroboros.Network.Protocol.ObjectDiffusion.Type
import Data.List.NonEmpty
import Network.TypedProtocol.Peer.Server (Server, ServerPipelined (..))

-- | The client side of the object diffusion protocol.
--
-- The peer in the client role submits objects to the peer in the server
-- role.

newtype ObjectDiffusionInbound objectId object m a =
  ObjectDiffusionInbound
    { runObjectDiffusionInbound :: m (InboundStIdle objectId object m a)
    }
newtype ObjectDiffusionOutbound objectId object m a =
  ObjectDiffusionOutbound
    { runObjectDiffusionOutbound :: m (OutboundStIdle Z objectId object m a)
    }

type family ObjectDiffusionClient polarity objectId object m a where
  ObjectDiffusionClient ClientInbound objectId object m a = ObjectDiffusionInbound objectId object m a
  ObjectDiffusionClient ClientOutbound objectId object m a = ObjectDiffusionOutbound objectId object m a

type family ObjectDiffusionServer polarity objectId object m a where
  ObjectDiffusionServer ClientInbound objectId object m a = ObjectDiffusionOutbound objectId object m a
  ObjectDiffusionServer ClientOutbound objectId object m a = ObjectDiffusionInbound objectId object m a

-- | In the 'StIdle' protocol state, the client does not have agency. Instead
-- it is waiting for:
--
-- * a request for object ids (blocking or non-blocking)
-- * a request for a given list of objects
-- * a termination message
--
-- It must be prepared to handle any of these.
data InboundStIdle txid tx m a = InboundStIdle
  { recvMsgRequestObjectIds ::
      forall blocking.
      SingBlockingStyle blocking ->
      NumObjectIdsToAck ->
      NumObjectIdsToReq ->
      m (InboundStObjectIds blocking txid tx m a)
  , recvMsgRequestObjects ::
      [txid] ->
      m (InboundStObjects txid tx m a)
  }

data InboundStObjectIds blocking txid tx m a where
  SendMsgReplyObjectIds ::
    BlockingReplyList blocking (txid, SizeInBytes) ->
    InboundStIdle txid tx m a ->
    InboundStObjectIds blocking txid tx m a
  -- | In the blocking case, the client can terminate the protocol. This could
  -- be used when the client knows there will be no more objects to submit.
  SendMsgDone :: a -> InboundStObjectIds StBlocking txid tx m a

data InboundStObjects txid tx m a where
  SendMsgReplyObjects ::
    [tx] ->
    InboundStIdle txid tx m a ->
    InboundStObjects txid tx m a

-- -- | A non-pipelined 'Peer' representing the 'ObjectDiffusionClient'.
-- objectDiffusionClientPeer ::
--   forall polarity txid tx m a.
--   Monad m =>
--   ObjectDiffusionClient txid tx m a ->
--   Client (ObjectDiffusion polarity txid tx) NonPipelined StInit m a
-- objectDiffusionClientPeer (ObjectDiffusionClient client) =
--   Yield MsgInit $
--     Effect $
--       go <$> client

goInbound ::
  InboundStIdle txid tx m a ->
  Client (ObjectDiffusion polarity txid tx) NonPipelined StIdle m a
goInbound InboundStIdle{recvMsgRequestObjectIds, recvMsgRequestObjects} =
  Await $ \msg -> case msg of
    MsgRequestObjectIds blocking ackNo reqNo -> Effect $ do
      reply <- recvMsgRequestObjectIds blocking ackNo reqNo
      case reply of
        SendMsgReplyObjectIds txids k ->
          -- TODO: investigate why GHC cannot infer `SingI`; it used to in
          -- `coot/typed-protocols-rewrite` branch
          return $ case blocking of
            SingBlocking ->
              Yield
                (MsgReplyObjectIds txids)
                (goInbound k)
            SingNonBlocking ->
              Yield
                (MsgReplyObjectIds txids)
                (goInbound k)
        SendMsgDone result ->
          return $
            Yield
              MsgDone
              (Done result)
    MsgRequestObjects txids -> Effect $ do
      SendMsgReplyObjects txs k <- recvMsgRequestObjects txids
      return $
        Yield
          (MsgReplyObjects txs)
          (goInbound k)

data ObjectDiffusionServerPipelined objectId object m a where
  ObjectDiffusionServerPipelined ::
    m (OutboundStIdle Z objectId object m a) ->
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

data OutboundStIdle (n :: N) objectId object m a where
  SendMsgRequestObjectIdsBlocking ::
    -- | number of objectIds to acknowledge
    NumObjectIdsToAck ->
    -- | number of objectIds to request
    NumObjectIdsToReq ->
    -- | Result if done
    m a ->
    ( NonEmpty (objectId, SizeInBytes) ->
      m (OutboundStIdle Z objectId object m a)
    ) ->
    OutboundStIdle Z objectId object m a
  SendMsgRequestObjectIdsPipelined ::
    NumObjectIdsToAck ->
    NumObjectIdsToReq ->
    m (OutboundStIdle (S n) objectId object m a) ->
    OutboundStIdle n objectId object m a
  SendMsgRequestObjectsPipelined ::
    [objectId] ->
    m (OutboundStIdle (S n) objectId object m a) ->
    OutboundStIdle n objectId object m a
  -- | Collect a pipelined result.
  CollectPipelined ::
    Maybe (OutboundStIdle (S n) objectId object m a) ->
    (Collect objectId object -> m (OutboundStIdle n objectId object m a)) ->
    OutboundStIdle (S n) objectId object m a

-- | Transform a 'ObjectDiffusionServerPipelined' into a 'PeerPipelined'.
objectDiffusionServerPeerPipelined ::
  forall polarity objectId object m a.
  Functor m =>
  ObjectDiffusionServerPipelined objectId object m a ->
  ServerPipelined (ObjectDiffusion polarity objectId object) StInit m a
objectDiffusionServerPeerPipelined (ObjectDiffusionServerPipelined server) =
  ServerPipelined $
    -- We need to assist GHC to infer the existentially quantified `c` as
    -- `Collect objectId object`
    Await @_ @(Pipelined Z (Collect objectId object))
      (\MsgInit -> Effect (go <$> server))
 where
  goOutbound ::
    forall (n :: N).
    OutboundStIdle n objectId object m a ->
    Server (ObjectDiffusion polarity objectId object) (Pipelined n (Collect objectId object)) StIdle m a

  goOutbound (SendMsgRequestObjectIdsBlocking ackNo reqNo kDone k) =
    Yield
      (MsgRequestObjectIds SingBlocking ackNo reqNo)
      $ Await
      $ \case
        MsgDone -> Effect (Done <$> kDone)
        MsgReplyObjectIds (BlockingReply objectIds) -> Effect (goOutbound <$> k objectIds)
  goOutbound (SendMsgRequestObjectIdsPipelined ackNo reqNo k) =
    YieldPipelined
      (MsgRequestObjectIds SingNonBlocking ackNo reqNo)
      ( ReceiverAwait $ \(MsgReplyObjectIds (NonBlockingReply objectIds)) -> ReceiverDone (CollectObjectIds reqNo objectIds)
      )
      (Effect (goOutbound <$> k))
  goOutbound (SendMsgRequestObjectsPipelined objectIds k) =
    YieldPipelined
      (MsgRequestObjects objectIds)
      (ReceiverAwait $ \(MsgReplyObjects objects) -> ReceiverDone (CollectObjects objectIds objects))
      (Effect (goOutbound <$> k))
  goOutbound (CollectPipelined mNone collect) =
    Collect
      (fmap goOutbound mNone)
      (Effect . fmap goOutbound . collect)
