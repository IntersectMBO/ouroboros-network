{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
module Ouroboros.Network.ConnectionManager.InformationChannel where

import           Control.Concurrent.Class.MonadSTM.Strict

import           Data.Functor (($>))
import           GHC.Natural (Natural)
import           Ouroboros.Network.ConnectionHandler (Handle)
import           Ouroboros.Network.InboundGovernor.Event (NewConnectionInfo)
import           Ouroboros.Network.Mux (MuxMode)
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)

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

-- | A Server control channel which instantiates to 'NewConnection' and 'Handle'.
--
-- It allows to pass 'STM' transactions which will resolve to 'NewConnection'.
-- Server's monitoring thread is the consumer of these messages; there are two
-- producers: accept loop and connection handler for outbound connections.
--
type InboundGovernorInfoChannel (muxMode :: MuxMode) peerAddr versionData bytes m a b =
    InformationChannel (NewConnectionInfo peerAddr (Handle muxMode peerAddr versionData bytes m a b)) m

-- | Control Channel between Server and Outbound Governor.
--
-- Control channel that is meant to share inbound connections with the Peer
-- Selection Governor. So the consumer is the Governor and Producer is the
-- Server.
--
type OutboundGovernorInfoChannel peerAddr m =
    InformationChannel (peerAddr, PeerSharing) m


newInformationChannel :: forall a m.
                        MonadLabelledSTM m
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
