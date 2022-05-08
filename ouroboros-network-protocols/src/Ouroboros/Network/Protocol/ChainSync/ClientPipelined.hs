{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE TypeOperators            #-}

#if __GLASGOW_HASKELL__ > 810 && __GLASGOW_HASKELL__ < 904
{-# OPTIONS -fno-full-laziness #-}
#endif
-- `coerceClientPipelinedStIdle` is not used and it's not exported.
{-# OPTIONS -Wno-unused-top-binds #-}

module Ouroboros.Network.Protocol.ChainSync.ClientPipelined
  ( ChainSyncClientPipelined (..)
  , ClientPipelinedStIdle (..)
  , ClientStNext (..)
  , ClientPipelinedStIntersect (..)
  , ChainSyncInstruction (..)
  , chainSyncClientPeerPipelined
  , chainSyncClientPeerSender
  , mapChainSyncClientPipelined
  , mapChainSyncClientPipelinedSt
  , F (..)
  , PipelinedTr
  , singPipelinedTr
  ) where

import           Data.Kind (Type)
import           Data.Singletons
import           Data.Type.Equality
import           Data.Type.Queue

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client

import           Ouroboros.Network.Protocol.ChainSync.Type


-- | Pipelined chain sync client.  It can only pipeline 'MsgRequestNext'
-- messages, while the 'MsgFindIntersect' are non pipelined.  This has a penalty
-- cost of an RTT, but they are sent relatively seldom and their response might
-- impact how many messages one would like to pipeline.  It also simplifies the
-- receiver callback.
--
newtype ChainSyncClientPipelined header point tip m a =
  ChainSyncClientPipelined {
    runChainSyncClientPipelined :: m (ClientPipelinedStIdle header point tip Empty m a)
  }


type PipelinedTr :: StNextKind -> Transition (ChainSync header point tip)
type PipelinedTr k = Tr (StNext k) StIdle

singPipelinedTr :: SingTrans (PipelinedTr StCanAwait)
singPipelinedTr = SingTr

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
data ClientPipelinedStIdle header point tip (queue :: Queue (ChainSync header point tip)) m a where

    SendMsgRequestNext
      ::    ClientStNext       header point tip Empty m a
      -> m (ClientStNext       header point tip Empty m a)
      -> ClientPipelinedStIdle header point tip Empty m a

    SendMsgRequestNextPipelined
      :: ClientPipelinedStIdle header point tip (q |> PipelinedTr StCanAwait) m a
      -> ClientPipelinedStIdle header point tip  q                            m a

    SendMsgFindIntersect
      :: [point]
      -> ClientPipelinedStIntersect header point tip       m a
      -> ClientPipelinedStIdle      header point tip Empty m a

    CollectResponse
      :: forall header point tip (k :: StNextKind)
                (q :: Queue (ChainSync header point tip))
                m a.
         SingI k
      => (Maybe
             (m (ClientPipelinedStIdle header point tip (PipelinedTr k <| q) m a)))
      -> ClientStNext                  header point tip                   q  m a
      -> ClientPipelinedStIdle         header point tip (PipelinedTr k <| q) m a

    SendMsgDone
      :: a
      -> ClientPipelinedStIdle header point tip Empty m a

-- | Callback for responses received after sending 'MsgRequestNext'.
--
-- We could receive 'MsgAwaitReply'. In this case we will wait for the next
-- message which must be 'MsgRollForward' or 'MsgRollBackward'; thus we need
-- only the two callbacks.
--
data ClientStNext header point tip (q :: Queue (ChainSync header point tip)) m a =
     ClientStNext {
       -- | Callback for 'MsgRollForward' message.
       --
       recvMsgRollForward
         :: header
         -> tip
         -> m (ClientPipelinedStIdle header point tip q m a),

       -- | Callback for 'MsgRollBackward' message.
       --
       recvMsgRollBackward
         :: point
         -> tip
         -> m (ClientPipelinedStIdle header point tip q m a)
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
          -> m (ClientPipelinedStIdle header point tip Empty m a),

       recvMsgIntersectNotFound
         :: tip
         -> m (ClientPipelinedStIdle header point tip Empty m a)
     }


lemma_cons_snoc_proof
    :: forall ps (q :: Queue ps) (x :: Transition ps) (y :: Transition ps).
       SingTrans y
    -> SingQueue q
    -> (    (x <| (q  |> y))
       :~: ((x <|  q) |> y)
       )
lemma_cons_snoc_proof _  SingEmpty   = Refl
lemma_cons_snoc_proof _ (SingCons _) = Refl
  -- sketch of the proof
  -- ```
  --  x <| ((a <| q)  |> y) -- by definition of (|>)
  -- (x <|  (a <| q)) |> y
  -- ```
  -- where `a = Tr st st'`, ...


type        MapSt :: ChainSync header  point  tip
                  -> ChainSync header' point' tip'
type family MapSt st = r where
  MapSt  StIdle              = StIdle
  MapSt (StNext StCanAwait)  = StNext (StCanAwait)
  MapSt (StNext StMustReply) = StNext (StMustReply)
  MapSt  StIntersect         = StIntersect
  MapSt  StDone              = StDone

type        MapTr :: Transition (ChainSync header  point  tip)
                  -> Transition (ChainSync header' point' tip')
type family MapTr tr = r where
  MapTr (Tr from to) = Tr (MapSt from) (MapSt to)

type        MapQueue :: Queue (ChainSync header  point  tip)
                     -> Queue (ChainSync header' point' tip')
type family MapQueue q where
  MapQueue Empty = Empty
  MapQueue (tr <| q) = MapTr tr <| MapQueue q



-- | It is needed to aid GHC in kind inference.
--
data MapSnocLemma header  point  tip
                   header' point' tip'
                   (q0  :: Queue (ChainSync header  point  tip))
                   (q1  :: Queue (ChainSync header' point' tip'))
                   (q2  :: Queue (ChainSync header' point' tip'))
                   (st  :: ChainSync header point tip)
                   (st' :: ChainSync header point tip)
                   where
    MapSnocLemma :: forall header  point  tip
                            header' point' tip'
                            (q0  :: Queue (ChainSync header  point  tip))
                            (q1  :: Queue (ChainSync header' point' tip'))
                            (q2  :: Queue (ChainSync header' point' tip'))
                            (st  :: ChainSync header point tip)
                            (st' :: ChainSync header point tip).
                     ( q1 ~ (MapQueue  q0 |> MapTr (Tr st st'))
                     , q2 ~  MapQueue (q0 |>        Tr st st')
                     , q1 ~ q2
                     )
                  => MapSnocLemma header point tip header' point' tip' q0 q1 q2 st st'

lemma_map_snoc_proof :: forall header  point  tip
                               header' point' tip'
                               (q   :: Queue (ChainSync header point tip))
                               (st  :: ChainSync header point tip)
                               (st' :: ChainSync header point tip).
                        SingTrans (Tr st st')
                     -> SingQueue q
                     -> MapSnocLemma header  point  tip
                                     header' point' tip'
                                     q
                                     (MapQueue  q |> MapTr (Tr st st'))
                                     (MapQueue (q |>        Tr st st'))
                                     st st'
lemma_map_snoc_proof _    SingEmpty    = MapSnocLemma
lemma_map_snoc_proof !tr (SingCons q') = f q' $ lemma_map_snoc_proof tr q'
  where
    f :: forall (q'    :: Queue (ChainSync header point tip))
                (st''  :: ChainSync header point tip)
                (st''' :: ChainSync header point tip).
         SingQueue q'
      -> MapSnocLemma header  point  tip
                      header' point' tip'
                      q'
                      (MapQueue  q' |> MapTr (Tr st st'))
                      (MapQueue (q' |>        Tr st st'))
                      st st'
      -> MapSnocLemma header  point  tip
                      header' point' tip'
                      (Tr st'' st''' <| q')
                      (MapTr (Tr st'' st''') <| (MapQueue  q' |> MapTr (Tr st st')))
                      (MapTr (Tr st'' st''') <| (MapQueue (q' |>        Tr st st')))
                      st st'
    f _ MapSnocLemma = case lemma_cons_snoc_proof SingTr q' of
          Refl -> MapSnocLemma
      -- sketch of the proof
      -- ```
      -- QMap ((x <|  q) |> y)  -- ConsSnocLemma
      -- QMap ( x <| (q  |> y))
      -- x'  <|  QMap (q |> y)  -- inductive assumption
      -- x'  <| (QMap q  |> y') -- ConsSnocLemma
      -- (x' <| QMap q)  |> y'
      -- ```




coerceClientPipelinedStIdle :: forall header  point  tip
                                      header' point' tip'
                                      (q   :: Queue (ChainSync header point tip))
                                      (st  :: ChainSync header point tip)
                                      (st' :: ChainSync header point tip)
                                      m a.
                               SingQueue q
                            -> SingTrans (Tr st st')
                            -> ClientPipelinedStIdle header' point' tip'
                                                     (MapQueue (q |>        Tr st st'))
                                                     m a
                            -> ClientPipelinedStIdle header' point' tip'
                                                     (MapQueue  q |> MapTr (Tr st st'))
                                                     m a
coerceClientPipelinedStIdle !q tr idle =
      case lemma_map_snoc_proof tr q :: MapSnocLemma header  point  tip
                                                     header' point' tip'
                                                     q
                                                     (MapQueue  q |> MapTr (Tr st st'))
                                                     (MapQueue (q |>        Tr st st'))
                                                     st st' of
        MapSnocLemma -> idle


data F st st' where
    FCanAwait  :: F (StNext StCanAwait)  StIdle
    FMustReply :: F (StNext StMustReply) StIdle

deriving instance Show (F st st')


-- | Transform a 'ChainSyncClientPipelined' by mapping over the tx header and
-- the chain tip values.
--
-- Note the direction of the individual mapping functions corresponds to
-- whether the types are used as protocol inputs or outputs (or both, as is
-- the case for points).
--
mapChainSyncClientPipelined
  :: forall header header' point point' tip tip' (m :: Type -> Type) a.
     Functor m
  => (point -> point')
  -> (point' -> point)
  -> (header' -> header)
  -> (tip' -> tip)
  -> ChainSyncClientPipelined header  point  tip  m a
  -> ChainSyncClientPipelined header' point' tip' m a
mapChainSyncClientPipelined toPoint' toPoint toHeader toTip (ChainSyncClientPipelined mInitialIdleClient)
  = ChainSyncClientPipelined (goIdle SingEmptyF <$> mInitialIdleClient)
  where
    goIdle :: forall (q :: Queue (ChainSync header point tip)).
              SingQueueF F q
           -> ClientPipelinedStIdle header  point  tip            q  m a
           -> ClientPipelinedStIdle header' point' tip' (MapQueue q) m a

    goIdle q (SendMsgRequestNext next mNext) =
        SendMsgRequestNext (goNext q next) (goNext q <$> mNext)

    goIdle q (SendMsgRequestNextPipelined idle) =
          let idle'  :: ClientPipelinedStIdle
                           header' point' tip'
                           (MapQueue (q |> PipelinedTr StCanAwait))
                           m a
              idle' = goIdle (q |> FCanAwait) idle

              idle'' :: ClientPipelinedStIdle
                           header' point' tip'
                           (MapQueue q |> PipelinedTr StCanAwait)
                           m a
              idle'' = coerceClientPipelinedStIdle
                         (toSingQueue q) singPipelinedTr idle'

          in SendMsgRequestNextPipelined idle''
    goIdle _ (SendMsgFindIntersect points inter) =
        SendMsgFindIntersect (toPoint' <$> points) (goIntersect inter)

    goIdle q@(SingConsF FCanAwait q') (CollectResponse idleMay next) =
        let idleMay'  = fmap (goIdle q) <$> idleMay
            next'     = goNext q' next
        in CollectResponse idleMay' next'

    goIdle q@(SingConsF FMustReply q') (CollectResponse idleMay next) =
        let idleMay'  = fmap (goIdle q) <$> idleMay
            next'     = goNext q' next
        in CollectResponse idleMay' next'

    goIdle SingEmptyF (SendMsgDone a) = SendMsgDone a


    goNext :: forall (q :: Queue (ChainSync header point tip)).
              SingQueueF F q
           -> ClientStNext header  point  tip            q  m a
           -> ClientStNext header' point' tip' (MapQueue q) m a

    goNext q ClientStNext{ recvMsgRollForward, recvMsgRollBackward } = ClientStNext
      { recvMsgRollForward = \header' tip' ->
          goIdle q <$> recvMsgRollForward (toHeader header') (toTip tip')
      , recvMsgRollBackward = \point' tip' ->
          goIdle q <$> recvMsgRollBackward (toPoint point') (toTip tip')
      }

    goIntersect :: ClientPipelinedStIntersect header  point  tip  m a
                -> ClientPipelinedStIntersect header' point' tip' m a
    goIntersect ClientPipelinedStIntersect{ recvMsgIntersectFound, recvMsgIntersectNotFound } =
      ClientPipelinedStIntersect
        { recvMsgIntersectFound = \point' tip' ->
            goIdle SingEmptyF <$> recvMsgIntersectFound (toPoint point') (toTip tip')
        , recvMsgIntersectNotFound = \tip' ->
            goIdle SingEmptyF <$> recvMsgIntersectNotFound (toTip tip')
        }


-- | A stateful transition of a 'ChainSyncClientPipelined' client.
--
mapChainSyncClientPipelinedSt
  :: forall header header' point point' tip tip' (m :: Type -> Type) state a.
     Functor m
  => (point -> point')
  -> (point' -> point)
  -> (tip' -> tip)
  -> (state -> header' -> (header, state))
  -- ^ called on very 'MsgRollForward'
  -> (state -> point'  -> state)
  -- ^ called on very 'MsgRollBackward'
  -> state
  -> ChainSyncClientPipelined header  point  tip  m a
  -> ChainSyncClientPipelined header' point' tip' m a
mapChainSyncClientPipelinedSt toPoint' toPoint toTip forwardStFn backwardStFn state0 (ChainSyncClientPipelined mInitialIdleClient)
  = ChainSyncClientPipelined (goIdle SingEmptyF state0 <$> mInitialIdleClient)
  where
    goIdle :: forall (q :: Queue (ChainSync header point tip)).
              SingQueueF F q
           -> state
           -> ClientPipelinedStIdle header  point  tip            q  m a
           -> ClientPipelinedStIdle header' point' tip' (MapQueue q) m a

    goIdle q state (SendMsgRequestNext next mNext) =
        SendMsgRequestNext (goNext q state next) (goNext q state <$> mNext)

    goIdle q state (SendMsgRequestNextPipelined idle) =
          let idle'  :: ClientPipelinedStIdle
                           header' point' tip'
                           (MapQueue (q |> PipelinedTr StCanAwait))
                           m a
              idle' = goIdle (q |> FCanAwait) state idle

              idle'' :: ClientPipelinedStIdle
                           header' point' tip'
                           (MapQueue q |> PipelinedTr StCanAwait)
                           m a
              idle'' = coerceClientPipelinedStIdle
                         (toSingQueue q) singPipelinedTr idle'

          in SendMsgRequestNextPipelined idle''
    goIdle _ state (SendMsgFindIntersect points inter) =
        SendMsgFindIntersect (toPoint' <$> points) (goIntersect state inter)

    goIdle q@(SingConsF FCanAwait q') state (CollectResponse idleMay next) =
        let idleMay' = fmap (goIdle q state) <$> idleMay
            next'    = goNext q' state next
        in CollectResponse idleMay' next'

    goIdle q@(SingConsF FMustReply q') state (CollectResponse idleMay next) =
        let idleMay'  = fmap (goIdle q state) <$> idleMay
            next'     = goNext q' state next
        in CollectResponse idleMay' next'

    goIdle SingEmptyF _state (SendMsgDone a) = SendMsgDone a


    goNext :: forall (q :: Queue (ChainSync header point tip)).
              SingQueueF F q
           -> state
           -> ClientStNext header  point  tip            q  m a
           -> ClientStNext header' point' tip' (MapQueue q) m a

    goNext q state ClientStNext{ recvMsgRollForward, recvMsgRollBackward } = ClientStNext
      { recvMsgRollForward = \header' tip' ->
          case forwardStFn state header' of
            (header, state') -> goIdle q state' <$> recvMsgRollForward header (toTip tip')
      , recvMsgRollBackward = \point' tip' ->
          let state' = backwardStFn state point' in
          goIdle q state' <$> recvMsgRollBackward (toPoint point') (toTip tip')
      }

    goIntersect :: state
                -> ClientPipelinedStIntersect header  point  tip  m a
                -> ClientPipelinedStIntersect header' point' tip' m a
    goIntersect state ClientPipelinedStIntersect{ recvMsgIntersectFound, recvMsgIntersectNotFound } =
      ClientPipelinedStIntersect
        { recvMsgIntersectFound = \point' tip' ->
            goIdle SingEmptyF state <$> recvMsgIntersectFound (toPoint point') (toTip tip')
        , recvMsgIntersectNotFound = \tip' ->
            goIdle SingEmptyF state <$> recvMsgIntersectNotFound (toTip tip')
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
    :: forall header point tip m stm a.
       ( Monad m
       , Functor stm
       )
    => ChainSyncClientPipelined header point tip m a
    -> Client (ChainSync header point tip) 'Pipelined Empty StIdle m stm a

chainSyncClientPeerPipelined (ChainSyncClientPipelined mclient) =
    Effect $ chainSyncClientPeerSender SingEmpty <$> mclient


chainSyncClientPeerSender
    :: forall header point tip (q :: Queue (ChainSync header point tip)) m stm a.
       ( Monad m
       , Functor stm
       )
    => SingQueue q
    -> ClientPipelinedStIdle header point tip q m a
    -> Client (ChainSync header point tip)
              'Pipelined
              q StIdle m stm a

chainSyncClientPeerSender q@SingEmpty (SendMsgRequestNext stNext stAwait) =

    Yield
      MsgRequestNext
      (Await $ \case
        MsgRollForward header tip ->
          Effect $
              chainSyncClientPeerSender q
                <$> recvMsgRollForward header tip
            where
              ClientStNext {recvMsgRollForward} = stNext

        MsgRollBackward pRollback tip ->
          Effect $
              chainSyncClientPeerSender q
                <$> recvMsgRollBackward pRollback tip
            where
              ClientStNext {recvMsgRollBackward} = stNext

        MsgAwaitReply ->
          Await $ \case
            MsgRollForward header tip ->
              Effect $ do
                ClientStNext {recvMsgRollForward} <- stAwait
                chainSyncClientPeerSender q
                  <$> recvMsgRollForward header tip

            MsgRollBackward pRollback tip ->
              Effect $ do
                ClientStNext {recvMsgRollBackward} <- stAwait
                chainSyncClientPeerSender q
                  <$> recvMsgRollBackward pRollback tip)


chainSyncClientPeerSender q (SendMsgRequestNextPipelined next) =

    -- pipeline 'MsgRequestNext', the receiver will await for an instruction.
    YieldPipelined
      MsgRequestNext
        (chainSyncClientPeerSender (q `snoc` singPipelinedTr) next)

chainSyncClientPeerSender q (SendMsgFindIntersect points
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
          MsgIntersectFound pIntersect tip ->
            Effect $
              chainSyncClientPeerSender q <$> recvMsgIntersectFound pIntersect tip
          MsgIntersectNotFound tip ->
            Effect $
              chainSyncClientPeerSender q <$> recvMsgIntersectNotFound tip
          )

chainSyncClientPeerSender q@(SingCons q')
                          (CollectResponse idleMay
                            ClientStNext
                              { recvMsgRollForward
                              , recvMsgRollBackward
                              }) =

    Collect
      (Effect . fmap (chainSyncClientPeerSender q) <$> idleMay)
      (\case
          MsgRollForward header tip ->
            CollectDone $ Effect $
              chainSyncClientPeerSender q' <$> recvMsgRollForward header tip
          MsgRollBackward pRollBack tip ->
            CollectDone $ Effect $
              chainSyncClientPeerSender q' <$> recvMsgRollBackward pRollBack tip
          MsgAwaitReply ->
            Collect
              (Effect . fmap (goClient . chainSyncClientPeerSender q) <$> idleMay)
              (\case
                  MsgRollForward header tip ->
                    CollectDone $ Effect $
                      chainSyncClientPeerSender q' <$> recvMsgRollForward header tip
                  MsgRollBackward pRollBack tip ->
                    CollectDone $ Effect $
                      chainSyncClientPeerSender q' <$> recvMsgRollBackward pRollBack tip
              )
      )

chainSyncClientPeerSender SingEmpty (SendMsgDone a) =
    Yield MsgDone (Done a)


goClient :: forall header point tip (q :: Queue (ChainSync header point tip))
                   (st :: ChainSync header point tip)
                   m stm a.
            ( Functor m
            , Functor stm
            )
         => Client (ChainSync header point tip) 'Pipelined
                   (Tr (StNext StCanAwait)  StIdle <| q) st m stm a
         -> Client (ChainSync header point tip) 'Pipelined
                   (Tr (StNext StMustReply) StIdle <| q) st m stm a
goClient (Effect next)             = Effect (goClient <$> next)
goClient (YieldPipelined msg next) = YieldPipelined msg (goClient next)
goClient (Collect more next)       = Collect (goClient <$> more) $ \msg ->
  case msg of
    MsgRollForward header tip     -> next (MsgRollForward header tip)
    MsgRollBackward pRollBack tip -> next (MsgRollBackward pRollBack tip)
goClient (CollectSTM more next)    = CollectSTM (goClient <$> more) $ \msg ->
  case msg of
    MsgRollForward header tip     -> next (MsgRollForward header tip)
    MsgRollBackward pRollBack tip -> next (MsgRollBackward pRollBack tip)
-- TODO: the function is total, GHC has a problem to realised this.  There is
-- a chance that GHC-9.2 will be able to infer this.
goClient _ = error "goClient: impossible!"
