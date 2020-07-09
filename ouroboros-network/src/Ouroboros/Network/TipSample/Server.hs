{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TipSample.Server where

import           Control.Monad.Class.MonadSTM

import           Network.TypedProtocol.Pipelined (Nat (Succ, Zero), N (..))

import           Cardano.Slotting.Slot (SlotNo)

import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.Protocol.TipSample.Server


-- | 'tip-sample' server application.
--
tipServer :: forall header m.
             MonadSTM m
          => STM m (Tip header)
          -- ^ 'ChainDB' 'getCurrentSlot' method.
          -> TipSampleServer (Tip header) m ()
tipServer getCurrentTip = TipSampleServer (pure serverStIdle)
  where
    serverStIdle :: ServerStIdle (Tip header) m ()
    serverStIdle = ServerStIdle {
        handleFollowTip,
        handleDone
      }

    handleFollowTip :: forall (n :: N).
                       Nat (S n)
                    -> SlotNo
                    -> m (SendTips (S n) (Tip header) m ())
    handleFollowTip n@(Succ Zero) slotNo = do
      atomically $ do
        tip <- getCurrentTip
        case tip of
          Tip slotNo' _ _ | slotNo' > slotNo -> pure $ SendLastTip n tip serverStIdle
                          | otherwise -> retry
          TipGenesis      -> retry
    handleFollowTip n@(Succ m@Succ{}) slotNo = do
        atomically $ do
          tip <- getCurrentTip
          case tip of
            Tip slotNo' _ _ | slotNo' > slotNo -> pure $ SendNextTip n tip (go m)
                            | otherwise -> retry
            TipGenesis      -> retry
      where
        go :: forall (k :: N).
              Nat (S k)
           -> m (SendTips (S k) (Tip header) m ())
        go k@(Succ Zero) =
          atomically $ do
            tip <- getCurrentTip
            pure (SendLastTip k tip serverStIdle)
        go k@(Succ l@Succ{}) =
          atomically $ do
            tip <- getCurrentTip
            pure $ SendNextTip k tip (go l)

    handleDone :: m ()
    handleDone = pure ()
