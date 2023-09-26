{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
module Ouroboros.Network.Protocol.ChainSync.Server
  ( -- * Protocol type for the server
    -- | The protocol states from the point of view of the server.
    ChainSyncServer (..)
  , ServerStIdle (..)
  , ServerStNext (..)
  , ServerStIntersect (..)
    -- * Execution as a typed protocol
  , chainSyncServerPeer
  ) where

import           Data.Singletons

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Server

import           Ouroboros.Network.Protocol.ChainSync.Type


-- | A chain sync protocol server, on top of some effect 'm'.
--
newtype ChainSyncServer header point tip m a = ChainSyncServer {
    runChainSyncServer :: m (ServerStIdle header point tip m a)
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
data ServerStIdle header point tip m a = ServerStIdle {

       recvMsgRequestNext   :: m (Either (ServerStNext header point tip m a)
                                      (m (ServerStNext header point tip m a))),

       recvMsgFindIntersect :: [point]
                            -> m (ServerStIntersect header point tip m a),

       recvMsgDoneClient :: m a
     }

-- | In the 'StNext' protocol state, the server has agency and must send either:
--
--  * a roll forward
--  * a roll back message
--  * a termination message
--
data ServerStNext header point tip m a where
  SendMsgRollForward  :: header -> tip
                      -> ChainSyncServer header point tip m a
                      -> ServerStNext header point tip m a

  SendMsgRollBackward :: point -> tip
                      -> ChainSyncServer header point tip m a
                      -> ServerStNext header point tip m a

-- | In the 'StIntersect' protocol state, the server has agency and must send
-- either:
--
--  * an intersection improved,
--  * unchanged message,
--  * termination message
--
data ServerStIntersect header point tip m a where
  SendMsgIntersectFound     :: point -> tip
                            -> ChainSyncServer header point tip m a
                            -> ServerStIntersect header point tip m a

  SendMsgIntersectNotFound  :: tip
                            -> ChainSyncServer header point tip m a
                            -> ServerStIntersect header point tip m a


-- | Interpret a 'ChainSyncServer' action sequence as a 'Peer' on the server
-- side of the 'ChainSyncProtocol'.
--
chainSyncServerPeer
  :: forall header point tip m stm a.
     Monad m
  => ChainSyncServer header point tip m a
  -> Server (ChainSync header point tip) 'NonPipelined Empty StIdle m stm a
chainSyncServerPeer (ChainSyncServer mterm) = Effect $ mterm >>=
    \(ServerStIdle{recvMsgRequestNext, recvMsgFindIntersect, recvMsgDoneClient}) ->

    pure $ Await $ \req ->
    case req of
      MsgRequestNext -> Effect $ do
        mresp <- recvMsgRequestNext
        pure $ case mresp of
          Left  resp    -> handleStNext SingCanAwait resp
          Right waiting -> Yield MsgAwaitReply $ Effect $ do
                             resp <- waiting
                             pure $ handleStNext SingMustReply resp

      MsgFindIntersect points -> Effect $ do
        resp <- recvMsgFindIntersect points
        pure $ handleStIntersect resp

      MsgDone -> Effect $ Done <$> recvMsgDoneClient

  where
    handleStNext
      :: SingI nextKind
      => SingNextKind nextKind
      -> ServerStNext header point tip m a
      -> Server (ChainSync header point tip) 'NonPipelined Empty (StNext nextKind) m stm a

    handleStNext _ (SendMsgRollForward  header tip next) =
      Yield (MsgRollForward header tip)
            (chainSyncServerPeer next)

    handleStNext _ (SendMsgRollBackward pIntersect tip next) =
      Yield (MsgRollBackward pIntersect tip)
            (chainSyncServerPeer next)


    handleStIntersect
      :: ServerStIntersect header point tip m a
      -> Server (ChainSync header point tip) 'NonPipelined Empty StIntersect m stm a

    handleStIntersect (SendMsgIntersectFound pIntersect tip next) =
      Yield (MsgIntersectFound pIntersect tip)
            (chainSyncServerPeer next)

    handleStIntersect (SendMsgIntersectNotFound tip next) =
      Yield (MsgIntersectNotFound tip)
            (chainSyncServerPeer next)
