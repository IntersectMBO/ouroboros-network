{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | A view of the chain synchronisation protocol from the point of view of the
-- server.
--
-- This provides a view that uses less complex types and should be easier to
-- use than the underlying typed protocol itself.
--
-- For execution, a conversion into the typed protocol is provided.
--
module Ouroboros.Network.Protocol.ChainSync.Server  (
      -- * Protocol type for the server
      -- | The protocol states from the point of view of the server.
      ChainSyncServer(..)
    , ServerStIdle(..)
    , ServerStNext(..)
    , ServerStIntersect(..)

      -- * Execution as a typed protocol
    , chainSyncServerPeer
    ) where

import Protocol.Core
import Ouroboros.Network.Protocol.ChainSync.Type


-- | A chain sync protocol server, on top of some effect 'm'.
--
newtype ChainSyncServer header point m a = ChainSyncServer {
    runChainSyncServer :: m (ServerStIdle header point m a)
  }


-- | In the 'StIdle' protocol state, the server does not have agency. Instead
-- it is waiting for:
--
--  * a next update request
--  * a find intersection request
--  * a termination messge
--
-- It must be prepared to handle either.
--
data ServerStIdle header point m a = ServerStIdle {

       recvMsgRequestNext   :: m (Either (ServerStNext header point m a)
                                      (m (ServerStNext header point m a))),

       recvMsgFindIntersect :: [point]
                            -> m (ServerStIntersect header point m a),

       recvMsgDoneClient :: a
     }

-- | In the 'StNext' protocol state, the server has agency and must send either:
--
--  * a roll forward
--  * a roll back message
--  * a termination message
--
data ServerStNext header point m a where
  SendMsgRollForward  :: header -> point
                      -> ChainSyncServer header point m a
                      -> ServerStNext header point m a

  SendMsgRollBackward :: point -> point
                      -> ChainSyncServer header point m a
                      -> ServerStNext header point m a

-- | In the 'StIntersect' protocol state, the server has agency and must send
-- either:
--
--  * an intersection improved,
--  * unchanged message,
--  * termination message
--
data ServerStIntersect header point m a where
  SendMsgIntersectImproved  :: point -> point
                            -> ChainSyncServer header point m a
                            -> ServerStIntersect header point m a

  SendMsgIntersectUnchanged :: point
                            -> ChainSyncServer header point m a
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
chainSyncServerPeer (ChainSyncServer mterm) = lift $ mterm >>=
    \(ServerStIdle{recvMsgRequestNext, recvMsgFindIntersect, recvMsgDoneClient}) ->

    pure $ await $ \req ->
    case req of
      MsgRequestNext -> lift $ do
        mresp <- recvMsgRequestNext
        pure $ case mresp of
          Left  resp    -> handleStNext resp
          Right waiting -> part MsgAwaitReply $ lift $ do
                             resp <- waiting
                             pure $ handleStNext resp

      MsgFindIntersect points -> lift $ do
        resp <- recvMsgFindIntersect points
        pure $ handleStIntersect resp 

      MsgDone -> done recvMsgDoneClient

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
