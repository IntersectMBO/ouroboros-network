{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE KindSignatures      #-}

module Ouroboros.Network.Protocol.TipSample.Server
  ( -- * Protocol type for the server
    TipSampleServer (..)
  , ServerStIdle (..)
  , SendTips (..)
    -- * Execution as a typed protocol
  , tipSampleServerPeer
  ) where

import           Cardano.Slotting.Slot (SlotNo)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero))

import           Ouroboros.Network.Protocol.TipSample.Type


-- | A 'TipSample' protocol server .
--
newtype TipSampleServer tip m a = TipSampleServer {
    -- | 'RunTipSampleServer' must not block for a long time.
    --
    runTipSampleServer :: m (ServerStIdle tip m a)
  }

-- | In 'StIdle' protocol state the server does not have agency, it is await
-- for one of:
--
-- * 'MsgFollowTip'
-- * 'MsgDone'
--
-- The 'ServerStIdle' contains handlers for all these messages.
--
data ServerStIdle tip m a = ServerStIdle {
    -- | The server is requested to return the first tip after 'SlotNo' (at or
    -- after to be precise); 'handleTipAfterSlotNo' might block.
    handleFollowTip      :: forall (n :: N). Nat (S n) -> SlotNo -> m (SendTips (S n) tip m a),
    handleDone           :: m a
  }

data SendTips (n :: N) tip m a where
    SendNextTip :: Nat (S (S n))
                -> tip
                -> m (SendTips (S n) tip m a)
                -> SendTips (S (S n)) tip m a

    SendLastTip :: Nat (S Z)
                -> tip
                -> ServerStIdle tip m a
                -> SendTips (S Z) tip m a


-- | Interpret 'TipSampleServer' action sequence as a 'Peer' on the server side
-- of the 'TipSample' protocol.
--
tipSampleServerPeer
  :: forall tip m a.
     Monad m
  => TipSampleServer tip m a
  -> Peer (TipSample tip) AsServer StIdle m a
tipSampleServerPeer (TipSampleServer mserver) =
    Effect $ serverStIdlePeer <$> mserver
  where
    -- (guarded) recursive step
    serverStIdlePeer :: ServerStIdle tip m a
                     -> Peer (TipSample tip) AsServer StIdle m a

    serverStIdlePeer ServerStIdle { handleFollowTip, handleDone } =
      Await (ClientAgency TokIdle) $ \msg -> case msg of
        MsgFollowTip n slotNo ->
          Effect $ serverStFollowTipPeer <$> handleFollowTip n slotNo

        MsgDone ->
          Effect $ Done TokDone <$> handleDone


    serverStFollowTipPeer :: SendTips (S n) tip m a
                          -> Peer (TipSample tip) AsServer (StFollowTip (S n)) m a

    serverStFollowTipPeer (SendNextTip n@(Succ (Succ _)) tip mnext) =
      Yield
        (ServerAgency (TokFollowTip n))
        (MsgNextTip tip)
        (Effect $ serverStFollowTipPeer <$> mnext)

    serverStFollowTipPeer (SendLastTip n@(Succ Zero) tip next) =
        Yield
          (ServerAgency (TokFollowTip n))
          (MsgNextTipDone tip)
          (serverStIdlePeer next)


