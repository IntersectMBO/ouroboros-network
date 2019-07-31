{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | A view of the chain synchronisation protocol from the point of view of the
-- client.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Ouroboros.Network.Protocol.ChainSync.Client (
      -- * Protocol type for the client
      -- | The protocol states from the point of view of the client.
      ChainSyncClient(..)
    , ClientStIdle(..)
    , ClientStNext(..)
    , ClientStIntersect(..)

      -- * Execution as a typed protocol
    , chainSyncClientPeer
    ) where

import Network.TypedProtocol.Core
import Ouroboros.Network.Protocol.ChainSync.Type


-- | A chain sync protocol client, on top of some effect 'm'.
-- The first choice of request is within that 'm'.
newtype ChainSyncClient header point m a = ChainSyncClient {
    runChainSyncClient :: m (ClientStIdle header point m a)
  }


-- | In the 'StIdle' protocol state, the server does not have agency and can choose to
-- send a request next, or a find intersection message.
--
data ClientStIdle header point m a where

  -- | Send the 'MsgRequestNext', with handlers for the replies.
  --
  -- The handlers for this message are more complicated than most RPCs because
  -- the server can either send us a reply immediately or it can send us a
  -- 'MsgAwaitReply' to indicate that the server itself has to block for a
  -- state change before it can send us the reply.
  --
  -- In the waiting case, the client gets the chance to take a local action.
  --
  SendMsgRequestNext
    ::    ClientStNext header point m a
    -> m (ClientStNext header point m a) -- after MsgAwaitReply
    -> ClientStIdle header point m a

  -- | Send the 'MsgFindIntersect', with handlers for the replies.
  --
  SendMsgFindIntersect
    :: [point]
    -> ClientStIntersect header point m a
    -> ClientStIdle header point m a

  -- | The client decided to end the protocol.
  --
  SendMsgDone
    :: a
    -> ClientStIdle header point m a

-- | In the 'StNext' protocol state, the client does not have agency and is
-- waiting to receive either
--
--  * a roll forward,
--  * roll back message,
--  * the terminating message.
--
-- It must be prepared to handle any of these.
--
data ClientStNext header point m a =
     ClientStNext {
       recvMsgRollForward  :: header -> point -> ChainSyncClient header point m a,
       recvMsgRollBackward :: point  -> point -> ChainSyncClient header point m a
     }

-- | In the 'StIntersect' protocol state, the client does not have agency and
-- is waiting to receive:
--
--  * an intersection improved,
--  * unchanged message,
--  * the termination message.
--
-- It must be prepared to handle any of these.
--
data ClientStIntersect header point m a =
     ClientStIntersect {
       recvMsgIntersectFound    :: point -> point -> ChainSyncClient header point m a,
       recvMsgIntersectNotFound ::          point -> ChainSyncClient header point m a
     }


-- | Interpret a 'ChainSyncClient' action sequence as a 'Peer' on the client
-- side of the 'ChainSyncProtocol'.
--
chainSyncClientPeer
  :: forall header point m a .
     Monad m
  => ChainSyncClient header point m a
  -> Peer (ChainSync header point) AsClient StIdle m a
chainSyncClientPeer (ChainSyncClient mclient) =
    Effect $ fmap chainSyncClientPeer_ mclient
  where
    chainSyncClientPeer_
      :: ClientStIdle header point m a
      -> Peer (ChainSync header point) AsClient StIdle m a
    chainSyncClientPeer_ (SendMsgRequestNext stNext stAwait) =
        Yield (ClientAgency TokIdle) MsgRequestNext $
        Await (ServerAgency (TokNext TokCanAwait)) $ \resp ->
        case resp of
          MsgRollForward header pHead ->
              chainSyncClientPeer (recvMsgRollForward header pHead)
            where
              ClientStNext{recvMsgRollForward} = stNext

          MsgRollBackward pRollback pHead ->
              chainSyncClientPeer (recvMsgRollBackward pRollback pHead)
            where
              ClientStNext{recvMsgRollBackward} = stNext

          -- This code could be factored more easily by changing the protocol type
          -- to put both roll forward and back under a single constructor.
          MsgAwaitReply ->
            Effect $ do
              ClientStNext{recvMsgRollForward, recvMsgRollBackward} <- stAwait
              pure $ Await (ServerAgency (TokNext TokMustReply)) $ \resp' ->
                case resp' of
                  MsgRollForward header pHead ->
                    chainSyncClientPeer (recvMsgRollForward header pHead)
                  MsgRollBackward pRollback pHead ->
                    chainSyncClientPeer (recvMsgRollBackward pRollback pHead)

    chainSyncClientPeer_ (SendMsgFindIntersect points stIntersect) =
        Yield (ClientAgency TokIdle) (MsgFindIntersect points) $
        Await (ServerAgency TokIntersect) $ \resp ->
        case resp of
          MsgIntersectFound pIntersect pHead ->
            chainSyncClientPeer (recvMsgIntersectFound pIntersect pHead)

          MsgIntersectNotFound pHead ->
            chainSyncClientPeer (recvMsgIntersectNotFound pHead)
      where
        ClientStIntersect {
          recvMsgIntersectFound,
          recvMsgIntersectNotFound
        } = stIntersect

    chainSyncClientPeer_ (SendMsgDone a) =
      Yield (ClientAgency TokIdle) MsgDone (Done TokDone a)
