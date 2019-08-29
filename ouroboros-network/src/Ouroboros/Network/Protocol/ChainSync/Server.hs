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

import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Protocol.ChainSync.Type


-- | A chain sync protocol server, on top of some effect 'm'.
--
newtype ChainSyncServer header tip m a = ChainSyncServer {
    runChainSyncServer :: m (ServerStIdle header tip m a)
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
data ServerStIdle header tip m a = ServerStIdle {

       recvMsgRequestNext   :: m (Either (ServerStNext header tip m a)
                                      (m (ServerStNext header tip m a))),

       recvMsgFindIntersect :: [Point header]
                            -> m (ServerStIntersect header tip m a),

       recvMsgDoneClient :: m a
     }

-- | In the 'StNext' protocol state, the server has agency and must send either:
--
--  * a roll forward
--  * a roll back message
--  * a termination message
--
data ServerStNext header tip m a where
  SendMsgRollForward  :: header -> tip
                      -> ChainSyncServer header tip m a
                      -> ServerStNext header tip m a

  SendMsgRollBackward :: Point header -> tip
                      -> ChainSyncServer header tip m a
                      -> ServerStNext header tip m a

-- | In the 'StIntersect' protocol state, the server has agency and must send
-- either:
--
--  * an intersection improved,
--  * unchanged message,
--  * termination message
--
data ServerStIntersect header tip m a where
  SendMsgIntersectFound     :: Point header -> tip
                            -> ChainSyncServer header tip m a
                            -> ServerStIntersect header tip m a

  SendMsgIntersectNotFound  :: tip
                            -> ChainSyncServer header tip m a
                            -> ServerStIntersect header tip m a


-- | Interpret a 'ChainSyncServer' action sequence as a 'Peer' on the server
-- side of the 'ChainSyncProtocol'.
--
chainSyncServerPeer
  :: forall header tip m a.
     Monad m
  => ChainSyncServer header tip m a
  -> Peer (ChainSync header tip) AsServer StIdle m a
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
    handleStNext
      :: TokNextKind nextKind
      -> ServerStNext header tip m a
      -> Peer (ChainSync header tip) AsServer (StNext nextKind) m a

    handleStNext toknextkind (SendMsgRollForward  header tip next) =
      Yield (ServerAgency (TokNext toknextkind))
            (MsgRollForward header tip)
            (chainSyncServerPeer next)

    handleStNext toknextkind (SendMsgRollBackward pIntersect tip next) =
      Yield (ServerAgency (TokNext toknextkind))
            (MsgRollBackward pIntersect tip)
            (chainSyncServerPeer next)


    handleStIntersect
      :: ServerStIntersect header tip m a
      -> Peer (ChainSync header tip) AsServer StIntersect m a

    handleStIntersect (SendMsgIntersectFound pIntersect tip next) =
      Yield (ServerAgency TokIntersect)
            (MsgIntersectFound pIntersect tip)
            (chainSyncServerPeer next)

    handleStIntersect (SendMsgIntersectNotFound tip next) =
      Yield (ServerAgency TokIntersect)
            (MsgIntersectNotFound tip)
            (chainSyncServerPeer next)
