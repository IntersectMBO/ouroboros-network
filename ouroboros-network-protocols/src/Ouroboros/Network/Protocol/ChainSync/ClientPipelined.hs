{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ > 810 && __GLASGOW_HASKELL__ < 904
{-# OPTIONS -fno-full-laziness #-}
#endif

module Ouroboros.Network.Protocol.ChainSync.ClientPipelined
  ( ChainSyncClientPipelined (..)
  , ClientPipelinedStIdle (..)
  , ClientStNext (..)
  , ClientPipelinedStIntersect (..)
  , ChainSyncInstruction (..)
  , chainSyncClientPeerPipelined
  , chainSyncClientPeerSender
  , mapChainSyncClientPipelined
  ) where

import Data.Kind (Type)

import Network.TypedProtocol.Core
import Network.TypedProtocol.Peer.Client

import Ouroboros.Network.Protocol.ChainSync.Type


-- | Pipelined chain sync client.  It can only pipeline 'MsgRequestNext'
-- messages, while the 'MsgFindIntersect' are non pipelined.  This has a penalty
-- cost of an RTT, but they are sent relatively seldom and their response might
-- impact how many messages one would like to pipeline.  It also simplifies the
-- receiver callback.
--
newtype ChainSyncClientPipelined header point tip m a =
  ChainSyncClientPipelined {
    runChainSyncClientPipelined :: m (ClientPipelinedStIdle Z header point tip m a)
  }


-- | Pipelined sender which starts in 'StIdle' state.  It can either
--
-- * Send 'MsgRequestNext' (no pipelining), which might be useful when we are at
--   the tip of the chain.  It can only be send when there is no pipelined
--   message in flight (all responses were collected);
--
-- * Pipeline 'MsgRequestNext';
--
-- * Send 'MsgFindIntersect' (no pipelining); It can only be send when there is
--   no pipelined message in flight (all responses were collected);
--
-- * Collect responses of pipelined message;
--
-- * Terminate the protocol with by sending 'MsgDone'.
--
data ClientPipelinedStIdle n header point tip  m a where

    SendMsgRequestNext
      :: m ()   -- ^ promptly invoked when 'MsgAwaitReply' is received
      -> ClientStNext          Z header point tip m a
      -> ClientPipelinedStIdle Z header point tip m a

    SendMsgRequestNextPipelined
      :: m ()   -- ^ promptly invoked when 'MsgAwaitReply' is received
      -> ClientPipelinedStIdle (S n) header point tip m a
      -> ClientPipelinedStIdle    n  header point tip m a

    SendMsgFindIntersect
      :: [point]
      -> ClientPipelinedStIntersect   header point tip m a
      -> ClientPipelinedStIdle      Z header point tip m a

    CollectResponse
      :: Maybe (m (ClientPipelinedStIdle (S n) header point tip m a))
      -> ClientStNext                       n  header point tip m a
      -> ClientPipelinedStIdle           (S n) header point tip m a

    SendMsgDone
      :: a
      -> ClientPipelinedStIdle Z header point tip m a

-- | Callback for responses received after sending 'MsgRequestNext'.
--
-- We could receive 'MsgAwaitReply'. In this case we will wait for the next
-- message which must be 'MsgRollForward' or 'MsgRollBackward'; thus we need
-- only the two callbacks.
--
data ClientStNext n header point tip m a =
     ClientStNext {
       -- | Callback for 'MsgRollForward' message.
       --
       recvMsgRollForward
         :: header
         -> tip
         -> m (ClientPipelinedStIdle n header point tip m a),

       -- | Callback for 'MsgRollBackward' message.
       --
       recvMsgRollBackward
         :: point
         -> tip
         -> m (ClientPipelinedStIdle n header point tip m a)
     }

-- | Callbacks for messages received after sending 'MsgFindIntersect'.
--
-- We might receive either 'MsgIntersectFound' or 'MsgIntersectNotFound'.
--
data ClientPipelinedStIntersect header point tip m a =
     ClientPipelinedStIntersect {

       recvMsgIntersectFound
         :: point
         -> tip
          -> m (ClientPipelinedStIdle Z header point tip m a),

       recvMsgIntersectNotFound
         :: tip
         -> m (ClientPipelinedStIdle Z header point tip m a)
     }

-- | Transform a 'ChainSyncClientPipelined' by mapping over the tx header and
-- the chain tip values.
--
-- Note the direction of the individual mapping functions corresponds to
-- whether the types are used as protocol inputs or outputs (or both, as is
-- the case for points).
--
mapChainSyncClientPipelined :: forall header header' point point' tip tip' (m :: Type -> Type) a.
  Functor m
  => (point -> point')
  -> (point' -> point)
  -> (header' -> header)
  -> (tip' -> tip)
  -> ChainSyncClientPipelined header point tip m a
  -> ChainSyncClientPipelined header' point' tip' m a
mapChainSyncClientPipelined toPoint' toPoint toHeader toTip (ChainSyncClientPipelined mInitialIdleClient)
  = ChainSyncClientPipelined (goIdle <$> mInitialIdleClient)
  where
    goIdle :: ClientPipelinedStIdle n header point tip  m a
           -> ClientPipelinedStIdle n header' point' tip'  m a
    goIdle client = case client of
      SendMsgRequestNext stAwait stNext -> SendMsgRequestNext stAwait (goNext stNext)
      SendMsgRequestNextPipelined await idle -> SendMsgRequestNextPipelined await (goIdle idle)
      SendMsgFindIntersect points inter -> SendMsgFindIntersect (toPoint' <$> points) (goIntersect inter)
      CollectResponse idleMay next -> CollectResponse (fmap goIdle <$> idleMay) (goNext next)
      SendMsgDone a -> SendMsgDone a

    goNext :: ClientStNext n header point tip  m a
           -> ClientStNext n header' point' tip'  m a
    goNext ClientStNext{ recvMsgRollForward, recvMsgRollBackward } = ClientStNext
      { recvMsgRollForward = \header' tip' -> goIdle <$> recvMsgRollForward (toHeader header') (toTip tip')
      , recvMsgRollBackward = \point' tip' -> goIdle <$> recvMsgRollBackward (toPoint point') (toTip tip')
      }

    goIntersect :: ClientPipelinedStIntersect header point tip m a
                -> ClientPipelinedStIntersect header' point' tip' m a
    goIntersect ClientPipelinedStIntersect{ recvMsgIntersectFound, recvMsgIntersectNotFound } = ClientPipelinedStIntersect
      { recvMsgIntersectFound = \point' tip' -> goIdle <$> recvMsgIntersectFound (toPoint point') (toTip tip')
      , recvMsgIntersectNotFound = \tip' -> goIdle <$> recvMsgIntersectNotFound (toTip tip')
      }


--
-- Pipelined Peer
--

-- | Data received through pipelining: either roll forward or roll backward
-- instruction. If the server replied with 'MsgAwaitReply' the pipelined
-- receiver will await for the next message which must come with an instruction
-- how to update our chain.
--
-- Note: internal API, not exposed by this module.
--
data ChainSyncInstruction header point tip
    = RollForward  !header !tip
    | RollBackward !point  !tip


chainSyncClientPeerPipelined
    :: forall header point tip m a.
       Monad m
    => ChainSyncClientPipelined header point tip m a
    -> ClientPipelined (ChainSync header point tip) StIdle m a

chainSyncClientPeerPipelined (ChainSyncClientPipelined mclient) =
    ClientPipelined $ Effect $ chainSyncClientPeerSender Zero <$> mclient


chainSyncClientPeerSender
    :: forall n header point tip m a.
       Monad m
    => Nat n
    -> ClientPipelinedStIdle n header point tip m a
    -> Client (ChainSync header point tip)
              (Pipelined n (ChainSyncInstruction header point tip)) StIdle
              m a

chainSyncClientPeerSender n@Zero (SendMsgRequestNext stAwait stNext) =

    Yield
      MsgRequestNext
      (Await $ \case
        MsgRollForward header tip -> Effect $
            chainSyncClientPeerSender n
              <$> recvMsgRollForward header tip
          where
            ClientStNext {recvMsgRollForward} = stNext

        MsgRollBackward pRollback tip -> Effect $
            chainSyncClientPeerSender n
              <$> recvMsgRollBackward pRollback tip
          where
            ClientStNext {recvMsgRollBackward} = stNext

        MsgAwaitReply -> Effect $ do
          stAwait
          pure $ Await $ \case
            MsgRollForward header tip -> Effect $
                chainSyncClientPeerSender n
                  <$> recvMsgRollForward header tip
              where
                ClientStNext {recvMsgRollForward} = stNext

            MsgRollBackward pRollback tip -> Effect $
                chainSyncClientPeerSender n
                  <$> recvMsgRollBackward pRollback tip
              where
                ClientStNext {recvMsgRollBackward} = stNext)


chainSyncClientPeerSender n (SendMsgRequestNextPipelined await next) =

    -- pipeline 'MsgRequestNext', the receiver will await for an instruction.
    YieldPipelined
      MsgRequestNext
      (ReceiverAwait
        -- await for the reply
        $ \case
          MsgRollForward  header    tip -> ReceiverDone (RollForward header tip)
          MsgRollBackward pRollback tip -> ReceiverDone (RollBackward pRollback tip)

          -- we need to wait for the next message; this time it must come with
          -- an instruction
          MsgAwaitReply -> ReceiverEffect $ do
            await
            pure $ ReceiverAwait $ \case
              MsgRollForward  header    tip -> ReceiverDone (RollForward header tip)
              MsgRollBackward pRollback tip -> ReceiverDone (RollBackward pRollback tip))

      (chainSyncClientPeerSender (Succ n) next)

chainSyncClientPeerSender n (SendMsgFindIntersect points
                              ClientPipelinedStIntersect
                                { recvMsgIntersectFound
                                , recvMsgIntersectNotFound
                                }) =

    -- non pipelined 'MsgFindIntersect'
    Yield
      (MsgFindIntersect points)
      (Await
        -- await for the response and recurse
        $ \case
          MsgIntersectFound pIntersect tip -> Effect $
              chainSyncClientPeerSender n <$> recvMsgIntersectFound pIntersect tip
          MsgIntersectNotFound tip -> Effect $
              chainSyncClientPeerSender n <$> recvMsgIntersectNotFound tip
          )

chainSyncClientPeerSender n@(Succ n')
                          (CollectResponse mStIdle
                            ClientStNext
                              { recvMsgRollForward
                              , recvMsgRollBackward
                              }) =

    Collect
      (Effect . fmap (chainSyncClientPeerSender n) <$> mStIdle)
      (\instr -> Effect $ chainSyncClientPeerSender n' <$> collect instr)
    where
      collect (RollForward header point) =
        recvMsgRollForward header point
      collect (RollBackward pRollback tip) =
        recvMsgRollBackward pRollback tip

chainSyncClientPeerSender Zero (SendMsgDone a) =
    Yield MsgDone (Done a)
