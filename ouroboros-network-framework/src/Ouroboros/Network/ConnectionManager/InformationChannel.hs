{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Ouroboros.Network.ConnectionManager.InformationChannel
  ( InformationChannel (..)
  , InboundGovernorInfoChannel
  , newInformationChannel
  ) where

import Control.Concurrent.Class.MonadSTM.Strict

import Data.Functor (($>))
import GHC.Natural (Natural)
import Ouroboros.Network.ConnectionHandler (Handle)
import Ouroboros.Network.Context (ResponderContext)
import Ouroboros.Network.InboundGovernor.Event (NewConnectionInfo)
import Ouroboros.Network.Mux (MuxMode)

-- | Information channel.
--
data InformationChannel a m =
  InformationChannel {
    -- | Read a single value from the channel.
    --
    readMessage  :: STM m a,

    -- | Write a value to the channel.
    --
    writeMessage :: a -> STM m ()
  }

-- | A channel which instantiates to 'NewConnectionInfo' and
-- 'Handle'.
--
-- * /Producer:/ connection manger for duplex outbound connections.
-- * /Consumer:/ inbound governor.
--
type InboundGovernorInfoChannel (muxMode :: MuxMode) initiatorCtx peerAddr versionData bytes m a b =
    InformationChannel (NewConnectionInfo peerAddr (Handle muxMode initiatorCtx (ResponderContext peerAddr) versionData bytes m a b)) m


-- | Create a new 'InformationChannel' backed by a `TBQueue`.
--
newInformationChannel :: forall a m. MonadLabelledSTM m
                      => m (InformationChannel a m)
newInformationChannel = do
    channel <-
      atomically $
        newTBQueue cc_QUEUE_BOUND
        >>= \q -> labelTBQueue q "server-cc" $> q
    pure $ InformationChannel {
        readMessage  = readTBQueue channel,
        writeMessage = writeTBQueue channel
      }


-- | The 'InformationChannel's 'TBQueue' depth.
--
cc_QUEUE_BOUND :: Natural
cc_QUEUE_BOUND = 10
