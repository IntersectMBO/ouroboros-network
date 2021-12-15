{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.TipSample.Client
  ( -- * Protocol type for the client
    TipSampleClient (..)
  , ClientStIdle (..)
  , HandleTips (..)
    -- * Execution as a typed protocol
  , tipSampleClientPeer
    -- * Null tip sample client
  , tipSampleClientNull
  ) where


import           Control.Monad (forever)
import           Control.Monad.Class.MonadTimer

import           Cardano.Slotting.Slot (SlotNo)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined (N (..), Nat (Succ, Zero))

import           Ouroboros.Network.Protocol.TipSample.Type


newtype TipSampleClient tip m a = TipSampleClient {
    runTipSampleClient :: m (ClientStIdle tip m a)
  }

tipSampleClientNull :: MonadTimer m => TipSampleClient tip m a
tipSampleClientNull = TipSampleClient $ forever $ threadDelay 43200 {- one day in seconds -}


data ClientStIdle tip m a where
    SendMsgFollowTip
      :: Nat (S n)
      -> SlotNo
      -> HandleTips (S n) tip m a
      -> ClientStIdle tip m a

    SendMsgDone
      :: a
      -> ClientStIdle tip m a


-- | Handle incoming tips after sending 'MsgFollowTip' message.
--
data HandleTips (n :: N) tip m a where
    -- | Receive a tip, await for more.
    --
    ReceiveTip
      :: (tip -> m (HandleTips (S n) tip m a))
      -> HandleTips (S (S n)) tip m a

    -- | Receive last tip, continue in 'StIdle' state.
    --
    ReceiveLastTip
      :: (tip -> m (ClientStIdle tip m a))
      -> HandleTips (S Z) tip m a


tipSampleClientPeer
    :: forall tip m a.
       Functor m
    => TipSampleClient tip m a
    -> Peer (TipSample tip) AsClient StIdle m a
tipSampleClientPeer (TipSampleClient mclient) =
    Effect $ idlePeer <$> mclient
  where
    idlePeer :: ClientStIdle tip m a
             -> Peer (TipSample tip) AsClient StIdle m a

    idlePeer (SendMsgFollowTip n slotNo k) =
      Yield (ClientAgency TokIdle) (MsgFollowTip n slotNo) $
      followTipPeer n k

    idlePeer (SendMsgDone a) =
      Yield (ClientAgency TokIdle) MsgDone $
        Done TokDone a


    followTipPeer :: Nat (S n)
                  -> HandleTips (S n) tip m a
                  -> Peer (TipSample tip) AsClient (StFollowTip (S n)) m a

    followTipPeer n@(Succ Zero) (ReceiveLastTip k) =
      Await (ServerAgency (TokFollowTip n)) $ \(MsgNextTipDone tip) ->
        Effect $ idlePeer <$> k tip

    followTipPeer n@(Succ m@(Succ _)) (ReceiveTip k) =
      Await (ServerAgency (TokFollowTip n)) $ \(MsgNextTip tip) ->
        Effect $ followTipPeer m <$> k tip
