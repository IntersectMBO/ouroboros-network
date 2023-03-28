{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | Intended to be imported qualified.
--
module Ouroboros.Network.InboundGovernor.ControlChannel
  ( NewConnection (..)
  , ControlChannel (..)
  , ServerControlChannel
  , newControlChannel
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict

import           Data.Functor (($>))
import           GHC.Natural (Natural)

import           Network.Mux.Types (MuxMode)

import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types


-- | Announcement message for a new connection.
--
data NewConnection peerAddr handle

    -- | Announce a new connection.  /Inbound protocol governor/ will start
    -- responder protocols using 'StartOnDemand' strategy and monitor remote
    -- transitions: @PromotedToWarm^{Duplex}_{Remote}@ and
    -- @DemotedToCold^{dataFlow}_{Remote}@.
    = NewConnection
      !Provenance
      !(ConnectionId peerAddr)
      !DataFlow
      !handle

instance Show peerAddr
      => Show (NewConnection peerAddr handle) where
      show (NewConnection provenance connId dataFlow _) =
        concat [ "NewConnection "
               , show provenance
               , " "
               , show connId
               , " "
               , show dataFlow
               ]



-- | A Server control channel which instantiates 'handle'.
--
type ServerControlChannel (muxMode :: MuxMode) peerAddr versionData bytes m a b =
    ControlChannel peerAddr (Handle muxMode peerAddr versionData bytes m a b) m

-- | Control channel.  It allows to pass 'STM' transactions which will
-- resolve to 'NewConnection'.   Server's monitoring thread is the consumer
-- of these messages; there are two producers: accept loop and connection
-- handler for outbound connections.
--
data ControlChannel peerAddr handle m =
  ControlChannel {
    -- | Read a single 'NewConnection' instruction from the channel.
    --
    readMessage  :: STM m (NewConnection peerAddr handle),

    -- | Write a 'NewConnection' to the channel.
    --
    writeMessage :: NewConnection peerAddr handle -> STM m ()
  }


newControlChannel :: forall peerAddr handle m.
                     MonadLabelledSTM m
                  => m (ControlChannel peerAddr handle m)
newControlChannel = do
    channel <-
      atomically $
        newTBQueue cc_QUEUE_BOUND
        >>= \q -> labelTBQueue q "server-cc" $> q
    pure $ ControlChannel {
        readMessage  = readTBQueue channel,
        writeMessage = writeTBQueue channel
      }


-- | The 'ControlChannel's 'TBQueue' depth.
--
cc_QUEUE_BOUND :: Natural
cc_QUEUE_BOUND = 10
