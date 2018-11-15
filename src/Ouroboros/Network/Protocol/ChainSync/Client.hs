{-# LANGUAGE GADTs           #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

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
      ChainSyncClient
    , ClientStIdle(..)
    , ClientStNext(..)
    , ClientStIntersect(..)

      -- * Execution as a typed protocol
    , chainSyncClientPeer
    ) where

import Protocol.Core
import Ouroboros.Network.Protocol.ChainSync.Type


-- | A chain sync protocol client, on top of some effect 'm'.
--
type ChainSyncClient header point m a = ClientStIdle header point m a


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
       recvMsgRollForward  :: header -> point -> m (ClientStIdle header point m a),
       recvMsgRollBackward :: point  -> point -> m (ClientStIdle header point m a),
       recvMsgDoneServer   :: a
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
       recvMsgIntersectImproved  :: point -> point -> m (ClientStIdle header point m a),
       recvMsgIntersectUnchanged ::          point -> m (ClientStIdle header point m a),
       recvMsgIntersectDone      :: a
     }


-- | Interpret a 'ChainSyncClient' action sequence as a 'Peer' on the client
-- side of the 'ChainSyncProtocol'.
--
chainSyncClientPeer
  :: Monad m
  => ChainSyncClient header point m a
  -> Peer ChainSyncProtocol (ChainSyncMessage header point)
          (Yielding StIdle) (Finished StDone)
          m a

chainSyncClientPeer (SendMsgRequestNext stNext stAwait) =
    over MsgRequestNext $
    await $ \resp ->
    case resp of
      MsgRollForward header pHead -> lift $ do
          next <- recvMsgRollForward header pHead
          pure $ chainSyncClientPeer next
        where
          ClientStNext{recvMsgRollForward} = stNext

      MsgRollBackward pRollback pHead -> lift $ do
          next <- recvMsgRollBackward pRollback pHead
          pure $ chainSyncClientPeer next
        where
          ClientStNext{recvMsgRollBackward} = stNext

      -- This code could be factored more easily by changing the protocol type
      -- to put both roll forward and back under a single constructor.
      MsgAwaitReply ->
        lift $ do
          ClientStNext{recvMsgRollForward, recvMsgRollBackward, recvMsgDoneServer} <- stAwait
          pure $ await $ \resp' ->
            case resp' of
              MsgRollForward header pHead -> lift $ do
                next <- recvMsgRollForward header pHead
                pure $ chainSyncClientPeer next
              MsgRollBackward pRollback pHead -> lift $ do
                next <- recvMsgRollBackward pRollback pHead
                pure $ chainSyncClientPeer next
              MsgDone ->
                done recvMsgDoneServer

      -- The server decided to end the protocol.
      MsgDone -> done recvMsgDoneServer
        where
          ClientStNext{recvMsgDoneServer} = stNext

chainSyncClientPeer (SendMsgFindIntersect points stIntersect) =
    over (MsgFindIntersect points) $
    await $ \resp ->
    case resp of
      MsgIntersectImproved pIntersect pHead -> lift $ do
        next <- recvMsgIntersectImproved pIntersect pHead
        pure $ chainSyncClientPeer next

      MsgIntersectUnchanged pHead -> lift $ do
        next <- recvMsgIntersectUnchanged pHead
        pure $ chainSyncClientPeer next

      -- The server decided to end the protocol
      MsgDone -> done recvMsgIntersectDone
  where
    ClientStIntersect {
      recvMsgIntersectImproved,
      recvMsgIntersectUnchanged,
      recvMsgIntersectDone
    } = stIntersect

chainSyncClientPeer (SendMsgDone a) = 
  out MsgDone (done a)
