{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- | Intended to be imported qualified.
--
module Ouroboros.Network.InboundGovernor.ControlChannel
  ( NewConnection (..)
  , ControlChannel (..)
  , ServerControlChannel
  , newControlChannel
  , newOutboundConnection
  , newInboundConnection
  ) where

import           Control.Monad.Class.MonadSTM.Strict

import           Data.Functor (($>))

import           Network.Mux.Types (MuxMode)

import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types


-- | Announcment message for a new connection.
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



-- | Server control channel.  It allows to pass 'STM' transactions which will
-- resolve to 'NewConnection'.   Server's monitoring thread is the consumer
-- of this messages; there are two produceres: accept loop and connection
-- handler for outbound connections.
--
data ControlChannel m msg =
  ControlChannel {
    -- | Read a single 'NewConnection' instructrion from the channel.
    --
    readMessage  :: STM m msg,

    -- | Write a 'NewConnection' to the channel.
    --
    writeMessage :: msg -> STM m ()
  }


type ServerControlChannel (muxMode :: MuxMode) peerAddr bytes m a b =
    ControlChannel m (NewConnection peerAddr (Handle muxMode peerAddr bytes m a b))


newControlChannel :: forall m srvCntrlMsg.
                     MonadLabelledSTM m
                  => m (ControlChannel m srvCntrlMsg)
newControlChannel = do
    -- Queue size: events will come eihter from the accept loop or from the
    -- connection manager (when it included an outbound duplex connection).
    channel <-
      atomically $
        newTBQueue 10
        >>= \q -> labelTBQueue q "server-cc" $> q
    pure $ ControlChannel {
        readMessage  = readMessage channel,
        writeMessage = writeMessage channel
      }
  where
    readMessage
      :: TBQueue m srvCntrlMsg
      -> STM     m srvCntrlMsg
    readMessage = readTBQueue

    writeMessage
      :: TBQueue m srvCntrlMsg
      -> srvCntrlMsg
      -> STM m ()
    writeMessage q a = writeTBQueue q a


newOutboundConnection
    :: ControlChannel m (NewConnection peerAddr handle)
    -> ConnectionId peerAddr
    -> DataFlow
    -> handle
    -> STM m ()
newOutboundConnection channel connId dataFlow handle =
    writeMessage channel
                (NewConnection Outbound connId dataFlow handle)

newInboundConnection
    :: ControlChannel m (NewConnection peerAddr handle)
    -> ConnectionId peerAddr
    -> DataFlow
    -> handle
    -> STM m ()
newInboundConnection channel connId dataFlow handle =
    writeMessage channel
                 (NewConnection Inbound connId dataFlow handle)
