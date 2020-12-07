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
    , ServerStSparse(..)

      -- * Execution as a typed protocol
    , chainSyncServerPeer
    ) where

import Network.TypedProtocol.Core

import Ouroboros.Network.Protocol.ChainSync.Type


-- | A chain sync protocol server, on top of some effect 'm'.
--
newtype ChainSyncServer block header point tip m a = ChainSyncServer {
    runChainSyncServer :: m (ServerStIdle block header point tip m a)
  }


-- | In the 'StIdle' protocol state, the server does not have agency. Instead
-- it is waiting for:
--
--  * a next update request
--  * a find intersection request
--  * a termination messge
--  * a block request
--
-- It must be prepared to handle either.
--
data ServerStIdle block header point tip m a = ServerStIdle {

       recvMsgRequestNext   :: m (Either (ServerStNext block header point tip m a)
                                      (m (ServerStNext block header point tip m a))),

       recvMsgFindIntersect :: [point]
                            -> m (ServerStIntersect block header point tip m a),

       recvMsgDoneClient :: m a,

       recvMsgRequestBlock :: point -> m (ServerStSparse block header point tip m a)

     }

-- | In the 'StNext' protocol state, the server has agency and must send either:
--
--  * a roll forward
--  * a roll back message
--  * a termination message
--
data ServerStNext block header point tip m a where
  SendMsgRollForward  :: header -> tip
                      -> ChainSyncServer block header point tip m a
                      -> ServerStNext block header point tip m a

  SendMsgRollBackward :: point -> tip
                      -> ChainSyncServer block header point tip m a
                      -> ServerStNext block header point tip m a

-- | In the 'StIntersect' protocol state, the server has agency and must send
-- either:
--
--  * an intersection improved,
--  * unchanged message,
--  * termination message
--
data ServerStIntersect block header point tip m a where
  SendMsgIntersectFound     :: point -> tip
                            -> ChainSyncServer block header point tip m a
                            -> ServerStIntersect block header point tip m a

  SendMsgIntersectNotFound  :: tip
                            -> ChainSyncServer block header point tip m a
                            -> ServerStIntersect block header point tip m a

data ServerStSparse block header point tip m a where

  SendMsgBlock        :: block
                      -> ChainSyncServer block header point tip m a
                      -> ServerStSparse block header point tip m a


-- | Interpret a 'ChainSyncServer' action sequence as a 'Peer' on the server
-- side of the 'ChainSyncProtocol'.
--
chainSyncServerPeer
  :: forall block header point tip m a.
     Monad m
  => ChainSyncServer block header point tip m a
  -> Peer (ChainSync block header point tip) AsServer StIdle m a
chainSyncServerPeer (ChainSyncServer mterm) = Effect $ mterm >>=
    \(ServerStIdle{recvMsgRequestNext, recvMsgFindIntersect, recvMsgDoneClient, recvMsgRequestBlock}) ->

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

      MsgRequestBlock point -> Effect $ do
        resp <- recvMsgRequestBlock point
        pure $ handleStSparse resp

  where
    handleStNext
      :: TokNextKind nextKind
      -> ServerStNext block header point tip m a
      -> Peer (ChainSync block header point tip) AsServer (StNext nextKind) m a

    handleStNext toknextkind (SendMsgRollForward  header tip next) =
      Yield (ServerAgency (TokNext toknextkind))
            (MsgRollForward header tip)
            (chainSyncServerPeer next)

    handleStNext toknextkind (SendMsgRollBackward pIntersect tip next) =
      Yield (ServerAgency (TokNext toknextkind))
            (MsgRollBackward pIntersect tip)
            (chainSyncServerPeer next)


    handleStIntersect
      :: ServerStIntersect block header point tip m a
      -> Peer (ChainSync block header point tip) AsServer StIntersect m a

    handleStIntersect (SendMsgIntersectFound pIntersect tip next) =
      Yield (ServerAgency TokIntersect)
            (MsgIntersectFound pIntersect tip)
            (chainSyncServerPeer next)

    handleStIntersect (SendMsgIntersectNotFound tip next) =
      Yield (ServerAgency TokIntersect)
            (MsgIntersectNotFound tip)
            (chainSyncServerPeer next)

    handleStSparse
      :: ServerStSparse block header point tip m a
      -> Peer (ChainSync block header point tip) AsServer StSparse m a

    handleStSparse (SendMsgBlock block next) =
      Yield (ServerAgency TokSparse)
            (MsgBlock block)
            (chainSyncServerPeer next)
