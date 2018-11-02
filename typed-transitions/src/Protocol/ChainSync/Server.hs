{-# LANGUAGE GADTs           #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}

-- | A view of the chain synchronisation protocol from the point of view of the
-- server.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Protocol.ChainSync.Server  (
      -- * Protocol type for the server
      -- | The protocol states from the point of view of the server.
      ChainSyncServer
    , ServerStIdle(..)
    , ServerStNext(..)
    , ServerStIntersect(..)

      -- * Execution as a typed protocol
    , chainSyncServerPeer
    ) where

import Protocol.Core
import Protocol.ChainSync.Type


-- | A chain sync protocol server, on top of some effect 'm'.
--
type ChainSyncServer header point m a = ServerStIdle header point m a


-- | In the 'StIdle' protocol state, the server does not have agency. Instead
-- it is waiting for either a next update request or a find intersection
-- request. It must be prepared to handle either.
--
data ServerStIdle header point m a = ServerStIdle {

       recvMsgRequestNext   :: m (Either (ServerStNext header point m a)
                                      (m (ServerStNext header point m a))),

       recvMsgFindIntersect :: [point]
                            -> m (ServerStIntersect header point m a)
     }

-- | In the 'StNext' protocol state, the server has agency and must send either
-- a roll forward or roll back message.
--
data ServerStNext header point m a where
   SendMsgRollForward  :: header -> point
                       -> ServerStIdle header point m a
                       -> ServerStNext header point m a

   SendMsgRollBackward :: point -> point
                       -> ServerStIdle header point m a
                       -> ServerStNext header point m a

-- | In the 'StIntersect' protocol state, the server has agency and must send
-- either an intersection improved or unchanged message.
--
data ServerStIntersect header point m a where
   SendMsgIntersectImproved  :: point -> point
                             -> ServerStIdle header point m a
                             -> ServerStIntersect header point m a

   SendMsgIntersectUnchanged :: point
                             -> ServerStIdle header point m a
                             -> ServerStIntersect header point m a


-- | Interpret a 'ChainSyncServer' action sequence as a 'Peer' on the server
-- side of the 'ChainSyncProtocol'.
--
chainSyncServerPeer
  :: Monad m
  => ChainSyncServer header point m a
  -> Peer ChainSyncProtocol (ChainSyncMessage header point)
          (Awaiting StIdle) (Finished StDone)
          m a
chainSyncServerPeer ServerStIdle{recvMsgRequestNext, recvMsgFindIntersect} =

    await $ \req ->
    case req of
      MsgRequestNext -> hole $ do
        mresp <- recvMsgRequestNext
        pure $ case mresp of
          Left  resp    -> handleStNext resp
          Right waiting -> part MsgAwaitReply $ hole $ do
                             resp <- waiting
                             pure $ handleStNext resp

      MsgFindIntersect points -> hole $ do
        resp <- recvMsgFindIntersect points
        pure $ handleStIntersect resp 

  where
    handleStNext (SendMsgRollForward  header pHead next) =
      over (MsgRollForward header pHead)
           (chainSyncServerPeer next)

    handleStNext (SendMsgRollBackward pIntersect pHead next) =
      over (MsgRollBackward pIntersect pHead)
           (chainSyncServerPeer next)

    handleStIntersect (SendMsgIntersectImproved pIntersect pHead next) =
      over (MsgIntersectImproved pIntersect pHead)
           (chainSyncServerPeer next)

    handleStIntersect (SendMsgIntersectUnchanged pHead next) =
      over (MsgIntersectUnchanged pHead)
           (chainSyncServerPeer next)

