{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.ChainSync.ClientPipelined
  ( ChainSyncClientPipelined (..)
  , ClientPipelinedStIdle (..)
  , ClientStNext (..)
  , ClientPipelinedStIntersect (..)
  , ChainSyncInstruction (..)
  , TraceChainsyncClientObservation (..)
  , TraceChainSyncClientReqRsp
  , TraceChainSyncClientTag (..)

  , chainSyncClientPeerPipelined
  , chainSyncClientPeerSender
  , mapChainSyncClientPipelined
  , peerReqRspTracer
  ) where

import Data.Functor (void)
import Data.Kind (Type)
import Control.Monad.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime
import Control.Tracer (Tracer (..), traceWith)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined

import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Util.TaggedObserve


-- | TraceChainsyncClientObservation indicates which request-response exchange
-- we're observing, either 'MsgFindIntersect' or a non-pipelined 'MsgRequestNext'.
--
data TraceChainsyncClientObservation = IntersectObservation ObserveIndicator
                                     | NextObservation ObserveIndicator
                                     deriving Show


data TraceChainSyncClientTag peer = IntersectTag peer
                                  | NextTag peer
                                  deriving Show


-- | 'TraceChainSyncClientReqRsp' traces the start, end and duration of
-- 'TraceChainSyncClientTag':ed observations.
type TraceChainSyncClientReqRsp peer =
    TaggedObservable (TraceChainSyncClientTag peer) Time Time DiffTime


-- | Transform a request response tracer into a tracer
-- that trace the start end end of observations.
peerReqRspTracer
    :: forall m peer.
       ( MonadSTM m
       , MonadMonotonicTime m
       )
    => Tracer m (TraceChainSyncClientReqRsp peer)
    -> peer
    -> m (Tracer m TraceChainsyncClientObservation)
peerReqRspTracer tracer peer = do
    intersectState <- newTMVarIO Nothing
    nextState <- newTMVarIO Nothing

    return $ transform $ matchTaggedObservations
        (\case
            (IntersectTag _) -> atomically $ swapTMVar intersectState Nothing
            (NextTag _)      -> atomically $ swapTMVar nextState Nothing
        )
        (\case
            (IntersectTag _) -> void . atomically . swapTMVar intersectState . Just
            (NextTag _)      -> void . atomically . swapTMVar nextState . Just
        )
        (flip diffTime)
        tracer

  where
    transform :: Tracer m (TraceChainSyncClientReqRsp peer)
              -> Tracer m TraceChainsyncClientObservation
    transform tr = Tracer $ \observeIndicator -> do
      now <- getMonotonicTime
      case observeIndicator of
        IntersectObservation ObserveBefore ->
            traceWith tr $ TOStart (IntersectTag peer) now
        IntersectObservation ObserveAfter  ->
            traceWith tr $ TOEnd   (IntersectTag peer) now Nothing
        NextObservation      ObserveBefore ->
            traceWith tr $ TOStart (NextTag peer) now
        NextObservation      ObserveAfter  ->
            traceWith tr $ TOEnd   (NextTag peer) now Nothing


-- | Pipelined chain sync client.  It can only pipeline 'MsgRequestNext'
-- messages, while the 'MsgFindIntersect' are non pipelined.  This has a penalty
-- cost of an RTT, but they are send relatively seldom and their resposne might
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
      ::    ClientStNext       Z header point tip m a
      -> m (ClientStNext       Z header point tip m a)
      -> ClientPipelinedStIdle Z header point tip m a

    SendMsgRequestNextPipelined
      :: ClientPipelinedStIdle (S n) header point tip m a
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
      SendMsgRequestNext next mNext -> SendMsgRequestNext (goNext next) (goNext <$> mNext)
      SendMsgRequestNextPipelined idle -> SendMsgRequestNextPipelined (goIdle idle)
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
-- instcutrion.  If the server replied with 'MsgAwaitReply' the pipelined
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
    => Tracer m TraceChainsyncClientObservation
    -> ChainSyncClientPipelined header point tip m a
    -> PeerPipelined (ChainSync header point tip) AsClient StIdle m a

chainSyncClientPeerPipelined tracer (ChainSyncClientPipelined mclient) =
    PeerPipelined $ SenderEffect $ chainSyncClientPeerSender tracer Zero <$> mclient


chainSyncClientPeerSender
    :: forall n header point tip m a.
       Monad m
    => Tracer m TraceChainsyncClientObservation
    -> Nat n
    -> ClientPipelinedStIdle n header point tip m a
    -> PeerSender (ChainSync header point tip)
                  AsClient StIdle n
                  (ChainSyncInstruction header point tip)
                  m a

chainSyncClientPeerSender tracer
                   n@Zero
                   (SendMsgRequestNext stNext stAwait) = SenderEffect $ do
    traceWith tracer $ NextObservation ObserveBefore
    return $ SenderYield
      (ClientAgency TokIdle)
      MsgRequestNext
      (SenderAwait
        (ServerAgency (TokNext TokCanAwait))
          $ \case
            MsgRollForward header tip ->
              SenderEffect $ do
                  traceWith tracer $ NextObservation ObserveAfter
                  chainSyncClientPeerSender tracer n
                    <$> recvMsgRollForward header tip
                where
                  ClientStNext {recvMsgRollForward} = stNext

            MsgRollBackward pRollback tip ->
              SenderEffect $ do
                  traceWith tracer $ NextObservation ObserveAfter
                  chainSyncClientPeerSender tracer n
                    <$> recvMsgRollBackward pRollback tip
                where
                  ClientStNext {recvMsgRollBackward} = stNext

            MsgAwaitReply -> SenderEffect $ do
              traceWith tracer $ NextObservation ObserveAfter
              return $ SenderAwait
                (ServerAgency (TokNext TokMustReply))
                $ \case
                  MsgRollForward header tip ->
                    SenderEffect $ do
                      ClientStNext {recvMsgRollForward} <- stAwait
                      chainSyncClientPeerSender tracer n
                        <$> recvMsgRollForward header tip

                  MsgRollBackward pRollback tip ->
                    SenderEffect $ do
                      ClientStNext {recvMsgRollBackward} <- stAwait
                      chainSyncClientPeerSender tracer n
                        <$> recvMsgRollBackward pRollback tip)


chainSyncClientPeerSender tracer n (SendMsgRequestNextPipelined next) =

    -- pipeline 'MsgRequestNext', the receiver will await for an instruction.
    SenderPipeline
      (ClientAgency TokIdle)
      MsgRequestNext

      (ReceiverAwait (ServerAgency (TokNext TokCanAwait))
        -- await for the reply
        $ \case
          MsgRollForward  header    tip -> ReceiverDone (RollForward header tip)
          MsgRollBackward pRollback tip -> ReceiverDone (RollBackward pRollback tip)

          -- we need to wait for the next message; this time it must come with
          -- an instruction
          MsgAwaitReply -> ReceiverAwait (ServerAgency (TokNext TokMustReply))
            $ \case
              MsgRollForward  header    tip -> ReceiverDone (RollForward header tip)
              MsgRollBackward pRollback tip -> ReceiverDone (RollBackward pRollback tip))

      (chainSyncClientPeerSender tracer (Succ n) next)

chainSyncClientPeerSender tracer n (SendMsgFindIntersect points
                              ClientPipelinedStIntersect
                                { recvMsgIntersectFound
                                , recvMsgIntersectNotFound
                                }) = SenderEffect $ do
    traceWith tracer $ IntersectObservation ObserveBefore

    -- non pipelined 'MsgFindIntersect'
    return $ SenderYield
      (ClientAgency TokIdle)
      (MsgFindIntersect points)
      (SenderAwait (ServerAgency TokIntersect)
        -- await for the response and recurse
        $ \case
          MsgIntersectFound pIntersect tip ->
            SenderEffect $ do
              traceWith tracer $ IntersectObservation ObserveAfter
              chainSyncClientPeerSender tracer n <$> recvMsgIntersectFound pIntersect tip
          MsgIntersectNotFound tip ->
            SenderEffect $ do
              traceWith tracer $ IntersectObservation ObserveAfter
              chainSyncClientPeerSender tracer n <$> recvMsgIntersectNotFound tip
          )

chainSyncClientPeerSender tracer n@(Succ n')
                          (CollectResponse mStIdle
                            ClientStNext
                              { recvMsgRollForward
                              , recvMsgRollBackward
                              }) =

    SenderCollect
      (SenderEffect . fmap (chainSyncClientPeerSender tracer n) <$> mStIdle)
      (\instr -> SenderEffect $ chainSyncClientPeerSender tracer n' <$> collect instr)
    where
      collect (RollForward header point) =
        recvMsgRollForward header point
      collect (RollBackward pRollback tip) =
        recvMsgRollBackward pRollback tip

chainSyncClientPeerSender _tracer Zero (SendMsgDone a) =
    SenderYield
      (ClientAgency TokIdle)
      MsgDone
      (SenderDone TokDone a)
