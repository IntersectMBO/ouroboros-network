{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utility module which provides wrappers which map a 'Peer' or 'PiplinedPeer'
-- of an original protocol 'ps' into 'Peer' \/ 'PiplinedPeer' of
-- @'Hello' ps stIdle@ protocol.
--
-- The module should be imported qualified.
--
module Ouroboros.Network.Protocol.Trans.Hello.Util
  ( -- * Wrap 'Peer' in 'Hello' protocol
    wrapClientPeer
  , wrapServerPeer
    -- * Wrap 'PipelinedPeer' in 'Hello' protcol.
  , wrapClientPeerPipelined
  , wrapServerPeerPipelined
  ) where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined
import           Ouroboros.Network.Protocol.Trans.Hello.Type


--
-- 'Peer'
--


wrapClientPeer
    :: forall ps (stIdle :: ps) m a.
       Functor m
    => Peer        ps         AsClient stIdle m a
    -> Peer (Hello ps stIdle) AsClient StHello m a
wrapClientPeer peer = Yield (ClientAgency TokHello) MsgHello (wrapPeer peer)

wrapServerPeer
    :: forall ps (stIdle :: ps) m a.
       Functor m
    => Peer        ps         AsServer         stIdle  m a
    -> Peer (Hello ps stIdle) AsServer (StTalk stIdle) m a
wrapServerPeer = wrapPeer


wrapPeer
    :: forall ps (stIdle :: ps) (st :: ps) (pr :: PeerRole) m a.
        Functor m
    => Peer        ps         pr            st  m a
    -> Peer (Hello ps stIdle) pr (StTalk st) m a
wrapPeer (Effect mnext) = Effect (wrapPeer <$> mnext)

wrapPeer (Yield (ClientAgency tok) msg next) =
  Yield (ClientAgency (TokClientTalk tok)) (MsgTalk msg) (wrapPeer next)

wrapPeer (Yield (ServerAgency tok) msg next) =
  Yield (ServerAgency (TokServerTalk tok)) (MsgTalk msg) (wrapPeer next)

wrapPeer (Await (ClientAgency tok) k) =
  Await (ClientAgency (TokClientTalk tok)) $ \(MsgTalk msg) ->
    wrapPeer (k msg)

wrapPeer (Await (ServerAgency tok) k) =
   Await (ServerAgency (TokServerTalk tok)) $ \(MsgTalk msg) ->
     wrapPeer (k msg)

wrapPeer (Done tok a) = Done (TokDone tok) a


--
-- 'PipelinedPeer'
--


wrapClientPeerPipelined
  :: forall ps (stIdle :: ps) m a.
     Functor m
  => PeerPipelined        ps         AsClient stIdle m a
  -> PeerPipelined (Hello ps stIdle) AsClient StHello m a
wrapClientPeerPipelined (PeerPipelined peer) =
    PeerPipelined (SenderYield (ClientAgency TokHello) MsgHello (wrapPeerSender peer))


wrapServerPeerPipelined
  :: forall ps (stIdle :: ps) m a.
     Functor m
  => PeerPipelined        ps         AsServer stIdle m a
  -> PeerPipelined (Hello ps stIdle) AsServer StHello m a
wrapServerPeerPipelined = fmapPeerPipelined f
  where
    -- We must use 'fmapPeerPipelined'; directly matching on the constructor
    -- confuses GHC with the existentially quantified 'c' under
    -- 'PeerPipelined'.
    f :: PeerSender        ps         AsServer stIdle Z c m a
      -> PeerSender (Hello ps stIdle) AsServer StHello Z c m a
    f peer =
        SenderAwait (ClientAgency TokHello) $ \msg ->
          case msg of
            MsgHello -> wrapPeerSender peer


wrapPeerSender
  :: forall ps (pr :: PeerRole) (stIdle :: ps) (st :: ps) (n :: Outstanding) c m a.
     Functor m
  => PeerSender        ps         pr         st  n c m a
  -> PeerSender (Hello ps stIdle) pr (StTalk st) n c m a
wrapPeerSender (SenderEffect mnext) = SenderEffect (wrapPeerSender <$> mnext)

wrapPeerSender (SenderDone tok a)   = SenderDone (TokDone tok) a

wrapPeerSender (SenderYield (ClientAgency tok) msg next) =
    SenderYield (ClientAgency (TokClientTalk tok)) (MsgTalk msg) (wrapPeerSender next)

wrapPeerSender (SenderYield (ServerAgency tok) msg next) =
    SenderYield (ServerAgency (TokServerTalk tok)) (MsgTalk msg) (wrapPeerSender next)

wrapPeerSender (SenderAwait (ClientAgency tok) k) =
    SenderAwait (ClientAgency (TokClientTalk tok)) $ \(MsgTalk msg) ->
      wrapPeerSender (k msg)

wrapPeerSender (SenderAwait (ServerAgency tok) k) =
    SenderAwait (ServerAgency (TokServerTalk tok)) $ \(MsgTalk msg) ->
      wrapPeerSender (k msg)

wrapPeerSender (SenderPipeline (ClientAgency tok) msg recv send) =
    SenderPipeline (ClientAgency (TokClientTalk tok))
                   (MsgTalk msg)
                   (wrapPeerReceiver recv)
                   (wrapPeerSender send)

wrapPeerSender (SenderPipeline (ServerAgency tok) msg recv send) =
    SenderPipeline (ServerAgency (TokServerTalk tok))
                   (MsgTalk msg)
                   (wrapPeerReceiver recv)
                   (wrapPeerSender send)

wrapPeerSender (SenderCollect pipelineMore collect) =
    SenderCollect (wrapPeerSender <$> pipelineMore)
                  (wrapPeerSender . collect)


wrapPeerReceiver
  :: forall ps (pr :: PeerRole) (stIdle :: ps) (st :: ps) (stdone :: ps) m c.
     Functor m
  => PeerReceiver        ps         pr         st          stdone  m c
  -> PeerReceiver (Hello ps stIdle) pr (StTalk st) (StTalk stdone) m c

wrapPeerReceiver (ReceiverEffect m) =
    ReceiverEffect (wrapPeerReceiver <$> m)

wrapPeerReceiver (ReceiverDone c) = ReceiverDone c

wrapPeerReceiver (ReceiverAwait (ClientAgency tok) k) =
    ReceiverAwait (ClientAgency (TokClientTalk tok)) $
      \(MsgTalk msg) -> wrapPeerReceiver (k msg)

wrapPeerReceiver (ReceiverAwait (ServerAgency tok) k) =
    ReceiverAwait (ServerAgency (TokServerTalk tok)) $
      \(MsgTalk msg) -> wrapPeerReceiver (k msg)
