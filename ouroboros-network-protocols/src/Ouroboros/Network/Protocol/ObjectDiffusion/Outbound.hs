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
    objectDiffusionNonInitOutboundPeer,
    objectDiffusionInitOutboundPeer,
  )
where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

-- | The outbound side of the object diffusion protocol.
--
-- The peer in the outbound role submits objects to the peer in the server
-- role.
newtype ObjectDiffusionOutbound txid tx m a = ObjectDiffusionOutbound
  { runObjectDiffusionOutbound :: m (OutboundStIdle txid tx m a)
  }

-- | In the 'StIdle' protocol state, the outbound does not have agency. Instead
-- it is waiting for:
--
-- * a request for object ids (blocking or non-blocking)
-- * a request for a given list of objects
-- * a termination message
--
-- It must be prepared to handle any of these.
data OutboundStIdle txid tx m a = OutboundStIdle
  { recvMsgRequestObjectIds ::
      forall blocking.
      SingBlockingStyle blocking ->
      NumObjectIdsToAck ->
      NumObjectIdsToReq ->
      m (OutboundStObjectIds blocking txid tx m a),
    recvMsgRequestObjects ::
      [txid] ->
      m (OutboundStObjects txid tx m a)
  }

data OutboundStObjectIds blocking txid tx m a where
  SendMsgReplyObjectIds ::
    BlockingReplyList blocking (txid, SizeInBytes) ->
    OutboundStIdle txid tx m a ->
    OutboundStObjectIds blocking txid tx m a
  -- | In the blocking case, the outbound can terminate the protocol. This could
  -- be used when the outbound knows there will be no more objects to submit.
  SendMsgDone :: a -> OutboundStObjectIds StBlocking txid tx m a

data OutboundStObjects txid tx m a where
  SendMsgReplyObjects ::
    [tx] ->
    OutboundStIdle txid tx m a ->
    OutboundStObjects txid tx m a

outboundRun ::
  forall (initAgency :: Agency) txid tx m a.
  (Monad m) =>
  OutboundStIdle txid tx m a ->
  Peer (ObjectDiffusion initAgency txid tx) AsOutbound NonPipelined StIdle m a
outboundRun OutboundStIdle {recvMsgRequestObjectIds, recvMsgRequestObjects} =
  Await ReflInboundAgency $ \msg -> case msg of
    MsgRequestObjectIds blocking ackNo reqNo -> Effect $ do
      reply <- recvMsgRequestObjectIds blocking ackNo reqNo
      case reply of
        SendMsgReplyObjectIds txids k ->
          -- TODO: investigate why GHC cannot infer `SingI`; it used to in
          -- `coot/typed-protocols-rewrite` branch
          return $ case blocking of
            SingBlocking ->
              Yield ReflOutboundAgency
                (MsgReplyObjectIds txids)
                (outboundRun k)
            SingNonBlocking ->
              Yield ReflOutboundAgency
                (MsgReplyObjectIds txids)
                (outboundRun k)
        SendMsgDone result ->
          return $
            Yield ReflOutboundAgency
              MsgDone
              (Done ReflNobodyAgency result)
    MsgRequestObjects txids -> Effect $ do
      SendMsgReplyObjects txs k <- recvMsgRequestObjects txids
      return $
        Yield ReflOutboundAgency
          (MsgReplyObjects txs)
          (outboundRun k)

-- | A non-pipelined 'Peer' representing the 'ObjectDiffusionOutbound'.
objectDiffusionNonInitOutboundPeer ::
  forall txid tx m a.
  (Monad m) =>
  ObjectDiffusionOutbound txid tx m a ->
  Peer (ObjectDiffusion InboundAgency txid tx) AsOutbound NonPipelined StInit m a
objectDiffusionNonInitOutboundPeer (ObjectDiffusionOutbound outboundSt) =
    -- We need to assist GHC to infer the existentially quantified `c` as
    -- `Collect objectId object`
    Await ReflInboundAgency
      (\MsgInit -> Effect (outboundRun <$> outboundSt))

objectDiffusionInitOutboundPeer ::
  forall txid tx m a.
  (Monad m) =>
  ObjectDiffusionOutbound txid tx m a ->
  Peer (ObjectDiffusion OutboundAgency txid tx) AsOutbound NonPipelined StInit m a
objectDiffusionInitOutboundPeer (ObjectDiffusionOutbound outboundSt) =
  Yield ReflOutboundAgency MsgInit $
    Effect $ outboundRun <$> outboundSt
