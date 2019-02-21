{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Protocol.Channel.Sim
  ( simStmChannels
  ) where

import qualified Data.Sequence as Seq

import Control.Monad.Class.MonadSTM
import Protocol.Channel


-- | Use the simulation STM (via MonadClass) to get 2 channels, one for each
-- peer. There's no notion of EOF for such a channel, so receiving never gives
-- 'Nothing'.
--
-- The channels are unbounded.
simStmChannels
  :: ( Monad m, MonadSTM m )
  => Tr m (Channel m t, Channel m t)
simStmChannels = do
  varA <- newTVar mempty
  varB <- newTVar mempty
  let send = \it m -> atomically (modifyTVar' it (Seq.|> m))
      recv = \it -> atomically $ do
        msgs <- readTVar it
        case Seq.viewl msgs of
          Seq.EmptyL       -> retry
          msg Seq.:< msgs' -> writeTVar it msgs' >> return (Just msg)
      channelA = uniformChannel (send varA) (recv varB)
      channelB = uniformChannel (send varB) (recv varA)
  pure (channelA, channelB)

