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

import Network.TypedProtocol.Core
import Ouroboros.Network.Protocol.ChainSync.Type


-- | A chain sync protocol server, on top of some effect 'm'.
--
newtype ChainSyncServer header m a = ChainSyncServer {
    runChainSyncServer :: m (ServerStIdle header m a)
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
data ServerStIdle header m a = ServerStIdle {

       recvMsgRequestNext   :: m (Either (ServerStNext header m a)
                                      (m (ServerStNext header m a))),

       recvMsgFindIntersect :: [Point header]
                            -> m (ServerStIntersect header m a),

       recvMsgDoneClient :: m a
     }

-- | In the 'StNext' protocol state, the server has agency and must send either:
--
--  * a roll forward
--  * a roll back message
--  * a termination message
--
data ServerStNext header m a where
  SendMsgRollForward  :: header -> Point header
                      -> ChainSyncServer header m a
                      -> ServerStNext header m a

  SendMsgRollBackward :: Point header -> Point header
                      -> ChainSyncServer header m a
                      -> ServerStNext header m a

-- | In the 'StIntersect' protocol state, the server has agency and must send
-- either:
--
--  * an intersection improved,
--  * unchanged message,
--  * termination message
--
data ServerStIntersect header m a where
  SendMsgIntersectImproved  :: Point header -> Point header
                            -> ChainSyncServer header m a
                            -> ServerStIntersect header m a

  SendMsgIntersectUnchanged :: Point header
                            -> ChainSyncServer header m a
                            -> ServerStIntersect header m a


-- | Interpret a 'ChainSyncServer' action sequence as a 'Peer' on the server
-- side of the 'ChainSyncProtocol'.
--
chainSyncServerPeer
  :: Monad m
  => ChainSyncServer header m a
  -> Peer (ChainSync header) AsServer StIdle m a
chainSyncServerPeer (ChainSyncServer mterm) = Effect $ mterm >>=
    \(ServerStIdle{recvMsgRequestNext, recvMsgFindIntersect, recvMsgDoneClient}) ->

    pure $ Await (ClientAgency TokIdle) $ \req ->
    case req of
      MsgRequestNext -> Effect $ do
        mresp <- recvMsgRequestNext
        pure $ case mresp of
          Left  resp    -> handleStNext TokCanAwait resp
          Right waiting -> Yield (ServerAgency (TokNext TokCanAwait))
                                 MsgAwaitReply $ Effect $ do
                             resp <- waiting
                             pure $ handleStNext TokMustReply resp

      MsgFindIntersect points -> Effect $ do
        resp <- recvMsgFindIntersect points
        pure $ handleStIntersect resp 

      MsgDone -> Effect $ fmap (Done TokDone) recvMsgDoneClient

  where
    handleStNext toknextkind (SendMsgRollForward  header pHead next) =
      Yield (ServerAgency (TokNext toknextkind))
            (MsgRollForward header pHead)
            (chainSyncServerPeer next)

    handleStNext toknextkind (SendMsgRollBackward pIntersect pHead next) =
      Yield (ServerAgency (TokNext toknextkind))
            (MsgRollBackward pIntersect pHead)
            (chainSyncServerPeer next)

    handleStIntersect (SendMsgIntersectImproved pIntersect pHead next) =
      Yield (ServerAgency TokIntersect)
            (MsgIntersectImproved pIntersect pHead)
            (chainSyncServerPeer next)

    handleStIntersect (SendMsgIntersectUnchanged pHead next) =
      Yield (ServerAgency TokIntersect)
            (MsgIntersectUnchanged pHead)
            (chainSyncServerPeer next)
