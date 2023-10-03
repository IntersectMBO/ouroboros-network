{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
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
module Ouroboros.Network.Protocol.ChainSync.Client
  ( -- * Protocol type for the client
    -- | The protocol states from the point of view of the client.
    ChainSyncClient (..)
  , ClientStIdle (..)
  , ClientStNext (..)
  , ClientStIntersect (..)
    -- * Execution as a typed protocol
  , chainSyncClientPeer
    -- * Null chain sync client
  , chainSyncClientNull
    -- * Utilities
  , mapChainSyncClient
  , mapChainSyncClientSt
  ) where

import           Control.Monad (forever)
import           Control.Monad.Class.MonadTimer

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client

import           Ouroboros.Network.Protocol.ChainSync.Type


-- | A chain sync protocol client, on top of some effect 'm'.
-- The first choice of request is within that 'm'.
newtype ChainSyncClient header point tip m a = ChainSyncClient {
    runChainSyncClient :: m (ClientStIdle header point tip m a)
  }

-- | A chain sync client which never sends any message.
--
chainSyncClientNull :: MonadTimer m => ChainSyncClient header point tip m a
chainSyncClientNull = ChainSyncClient $ forever $ threadDelay 43200 {- one day in seconds -}

{-# DEPRECATED chainSyncClientNull "Use Ouroboros.Network.NodeToClient.chainSyncPeerNull" #-}

-- | In the 'StIdle' protocol state, the server does not have agency and can choose to
-- send a request next, or a find intersection message.
--
data ClientStIdle header point tip m a where

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
    ::    ClientStNext header point tip m a
    -> m (ClientStNext header point tip m a) -- after MsgAwaitReply
    -> ClientStIdle header point tip m a

  -- | Send the 'MsgFindIntersect', with handlers for the replies.
  --
  SendMsgFindIntersect
    :: [point]
    -> ClientStIntersect header point tip m a
    -> ClientStIdle header point tip m a

  -- | The client decided to end the protocol.
  --
  SendMsgDone
    :: a
    -> ClientStIdle header point tip m a

-- | In the 'StNext' protocol state, the client does not have agency and is
-- waiting to receive either
--
--  * a roll forward,
--  * roll back message,
--
-- It must be prepared to handle any of these.
--
data ClientStNext header point tip m a =
     ClientStNext {
       recvMsgRollForward  :: header -- header to add to the chain
                           -> tip    -- information about tip of the chain
                           -> ChainSyncClient header point tip m a,

       recvMsgRollBackward :: point        -- rollback point
                           -> tip          -- information about tip of the chain
                           -> ChainSyncClient header point tip m a
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
data ClientStIntersect header point tip m a =
     ClientStIntersect {
       recvMsgIntersectFound    :: point        -- found intersection point
                                -> tip          -- information about tip of the chain
                                -> ChainSyncClient header point tip m a,

       recvMsgIntersectNotFound :: tip          -- information about tip of the chain
                                -> ChainSyncClient header point tip m a
     }


-- | Transform a 'ChainSyncClient' by mapping over the tx header and the
-- chain tip values.
--
-- Note the direction of the individual mapping functions corresponds to
-- whether the types are used as protocol inputs or outputs (or both, as is
-- the case for points).
--
mapChainSyncClient :: forall header header' point point' tip tip' m a.
                      Functor m
                   => (point  -> point')
                   -> (point' -> point)
                   -> (header' -> header)
                   -> (tip' -> tip)
                   -> ChainSyncClient header  point  tip  m a
                   -> ChainSyncClient header' point' tip' m a
mapChainSyncClient fpoint fpoint' fheader ftip =
    goClient
  where
    goClient :: ChainSyncClient header  point  tip  m a
             -> ChainSyncClient header' point' tip' m a
    goClient (ChainSyncClient c) = ChainSyncClient (fmap goIdle c)

    goIdle :: ClientStIdle header  point  tip  m a
           -> ClientStIdle header' point' tip' m a
    goIdle (SendMsgRequestNext stNext stAwait) =
      SendMsgRequestNext (goNext stNext) (fmap goNext stAwait)

    goIdle (SendMsgFindIntersect points stIntersect) =
      SendMsgFindIntersect (map fpoint points) (goIntersect stIntersect)

    goIdle (SendMsgDone a) = SendMsgDone a

    goNext :: ClientStNext header  point  tip  m a
           -> ClientStNext header' point' tip' m a
    goNext ClientStNext{recvMsgRollForward, recvMsgRollBackward} =
      ClientStNext {
        recvMsgRollForward  = \hdr tip ->
          goClient (recvMsgRollForward (fheader hdr) (ftip tip)),

        recvMsgRollBackward = \pt  tip ->
          goClient (recvMsgRollBackward (fpoint' pt) (ftip tip))
      }

    goIntersect :: ClientStIntersect header  point  tip  m a
                -> ClientStIntersect header' point' tip' m a
    goIntersect ClientStIntersect { recvMsgIntersectFound,
                                    recvMsgIntersectNotFound } =
      ClientStIntersect {
        recvMsgIntersectFound = \pt tip ->
          goClient (recvMsgIntersectFound (fpoint' pt) (ftip tip)),

        recvMsgIntersectNotFound = \tip ->
          goClient (recvMsgIntersectNotFound (ftip tip))
      }

mapChainSyncClientSt
  :: forall header header' point point' tip tip' state m a.
     Functor m
  => (point -> point')
  -> (point' -> point)
  -> (tip' -> tip)
  -> (state -> header' -> (header, state))
  -- ^ called on very 'MsgRollForward'
  -> (state -> point'  -> state)
  -- ^ called on very 'MsgRollBackward'
  -> state
  -> ChainSyncClient header  point  tip  m a
  -> ChainSyncClient header' point' tip' m a
mapChainSyncClientSt toPoint' toPoint toTip forwardStFn backwardStFn state0 = goClient state0
  where
    goClient :: state
             -> ChainSyncClient header  point  tip  m a
             -> ChainSyncClient header' point' tip' m a
    goClient state (ChainSyncClient c) = ChainSyncClient (fmap (goIdle state) c)

    goIdle :: state
           -> ClientStIdle header  point  tip  m a
           -> ClientStIdle header' point' tip' m a
    goIdle state (SendMsgRequestNext stNext stAwait) =
      SendMsgRequestNext (goNext state stNext) (fmap (goNext state) stAwait)

    goIdle state (SendMsgFindIntersect points stIntersect) =
      SendMsgFindIntersect (map toPoint' points) (goIntersect state stIntersect)

    goIdle _state (SendMsgDone a) = SendMsgDone a

    goNext :: state
           -> ClientStNext header  point  tip  m a
           -> ClientStNext header' point' tip' m a
    goNext state ClientStNext{recvMsgRollForward, recvMsgRollBackward} =
      ClientStNext {
        recvMsgRollForward  = \header' tip' ->
          case forwardStFn state header' of
            (header, state') -> goClient state' (recvMsgRollForward header (toTip tip')),

        recvMsgRollBackward = \point' tip' ->
          let state' = backwardStFn state point' in
          goClient state' (recvMsgRollBackward (toPoint point') (toTip tip'))
      }

    goIntersect :: state
                -> ClientStIntersect header  point  tip  m a
                -> ClientStIntersect header' point' tip' m a
    goIntersect state ClientStIntersect { recvMsgIntersectFound,
                                          recvMsgIntersectNotFound } =
      ClientStIntersect {
        recvMsgIntersectFound = \point' tip' ->
          goClient state (recvMsgIntersectFound (toPoint point') (toTip tip')),

        recvMsgIntersectNotFound = \tip' ->
          goClient state (recvMsgIntersectNotFound (toTip tip'))
      }

-- | Interpret a 'ChainSyncClient' action sequence as a 'Peer' on the client
-- side of the 'ChainSyncProtocol'.
--
chainSyncClientPeer
  :: forall header point tip m stm a.
     Monad m
  => ChainSyncClient header point tip m a
  -> Client (ChainSync header point tip) 'NonPipelined Empty StIdle m stm a
chainSyncClientPeer (ChainSyncClient mclient) =
    Effect $ fmap chainSyncClientPeer_ mclient
  where
    chainSyncClientPeer_
      :: ClientStIdle header point tip m a
      -> Client (ChainSync header point tip) 'NonPipelined Empty StIdle m stm a
    chainSyncClientPeer_ (SendMsgRequestNext stNext stAwait) =
        Yield MsgRequestNext $
        Await $ \resp ->
        case resp of
          MsgRollForward header tip ->
              chainSyncClientPeer (recvMsgRollForward header tip)
            where
              ClientStNext{recvMsgRollForward} = stNext

          MsgRollBackward pRollback tip ->
              chainSyncClientPeer (recvMsgRollBackward pRollback tip)
            where
              ClientStNext{recvMsgRollBackward} = stNext

          -- This code could be factored more easily by changing the protocol type
          -- to put both roll forward and back under a single constructor.
          MsgAwaitReply ->
            Effect $ do
              ClientStNext{recvMsgRollForward, recvMsgRollBackward} <- stAwait
              pure $ Await $ \resp' ->
                case resp' of
                  MsgRollForward header tip ->
                    chainSyncClientPeer (recvMsgRollForward header tip)
                  MsgRollBackward pRollback tip ->
                    chainSyncClientPeer (recvMsgRollBackward pRollback tip)

    chainSyncClientPeer_ (SendMsgFindIntersect points stIntersect) =
        Yield (MsgFindIntersect points) $
        Await $ \resp ->
        case resp of
          MsgIntersectFound pIntersect tip ->
            chainSyncClientPeer (recvMsgIntersectFound pIntersect tip)

          MsgIntersectNotFound tip ->
            chainSyncClientPeer (recvMsgIntersectNotFound tip)
      where
        ClientStIntersect {
          recvMsgIntersectFound,
          recvMsgIntersectNotFound
        } = stIntersect

    chainSyncClientPeer_ (SendMsgDone a) =
      Yield MsgDone (Done a)
