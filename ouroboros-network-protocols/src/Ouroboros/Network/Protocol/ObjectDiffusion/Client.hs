{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A view of the object diffusion protocol from the point of view of
-- the client.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, 'objectDiffusionClientPeer' is provided for conversion
-- into the typed protocol.
module Ouroboros.Network.Protocol.ObjectDiffusion.Client
  ( -- * Protocol type for the client

    -- | The protocol states from the point of view of the client.
    ObjectDiffusionClient (..),
    ClientStIdle (..),
    ClientStObjectIds (..),
    ClientStObjects (..),
    SingBlockingStyle (..),
    BlockingReplyList (..),

    -- * Execution as a typed protocol
    objectDiffusionClientPeer,
  )
where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

-- | The client side of the object diffusion protocol.
--
-- The peer in the client role submits objects to the peer in the server
-- role.
newtype ObjectDiffusionClient txid tx m a = ObjectDiffusionClient
  { runObjectDiffusionClient :: m (ClientStIdle txid tx m a)
  }

-- | In the 'StIdle' protocol state, the client does not have agency. Instead
-- it is waiting for:
--
-- * a request for object ids (blocking or non-blocking)
-- * a request for a given list of objects
-- * a termination message
--
-- It must be prepared to handle any of these.
data ClientStIdle txid tx m a = ClientStIdle
  { recvMsgRequestObjectIds ::
      forall blocking.
      SingBlockingStyle blocking ->
      NumObjectIdsToAck ->
      NumObjectIdsToReq ->
      m (ClientStObjectIds blocking txid tx m a),
    recvMsgRequestObjects ::
      [txid] ->
      m (ClientStObjects txid tx m a)
  }

data ClientStObjectIds blocking txid tx m a where
  SendMsgReplyObjectIds ::
    BlockingReplyList blocking (txid, SizeInBytes) ->
    ClientStIdle txid tx m a ->
    ClientStObjectIds blocking txid tx m a
  -- | In the blocking case, the client can terminate the protocol. This could
  -- be used when the client knows there will be no more objects to submit.
  SendMsgDone :: a -> ClientStObjectIds StBlocking txid tx m a

data ClientStObjects txid tx m a where
  SendMsgReplyObjects ::
    [tx] ->
    ClientStIdle txid tx m a ->
    ClientStObjects txid tx m a

-- | A non-pipelined 'Peer' representing the 'ObjectDiffusionClient'.
objectDiffusionClientPeer ::
  forall txid tx m a.
  (Monad m) =>
  ObjectDiffusionClient txid tx m a ->
  Client (ObjectDiffusion txid tx) NonPipelined StInit m a
objectDiffusionClientPeer (ObjectDiffusionClient client) =
  Yield MsgInit $
    Effect $
      go <$> client
  where
    go ::
      ClientStIdle txid tx m a ->
      Client (ObjectDiffusion txid tx) NonPipelined StIdle m a
    go ClientStIdle {recvMsgRequestObjectIds, recvMsgRequestObjects} =
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
                    (go k)
                SingNonBlocking ->
                  Yield
                    (MsgReplyObjectIds txids)
                    (go k)
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
              (go k)
