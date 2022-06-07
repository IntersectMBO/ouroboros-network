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
  , newOutboundConnection
  , newInboundConnection
  ) where

import           Control.Monad.Class.MonadSTM.Strict

import           Data.Functor (($>))

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



-- | Server control channel.  It allows to pass 'STM' transactions which will
-- resolve to 'NewConnection'.   Server's monitoring thread is the consumer
-- of these messages; there are two producers: accept loop and connection
-- handler for outbound connections.
--
-- GR-FIXME[D2]: Last sentence is DOU (see code-review-doc)
type ServerControlChannel (muxMode :: MuxMode) peerAddr bytes m a b =
    ControlChannel m (NewConnection peerAddr (Handle muxMode peerAddr bytes m a b))

data ControlChannel m msg =
  ControlChannel {
    -- | Read a single 'NewConnection' instruction from the channel.
    --
    readMessage  :: STM m msg,

    -- | Write a 'NewConnection' to the channel.
    --
    writeMessage :: msg -> STM m ()
  }
  -- GR-FIXME[C2]: the 'msg' tyvar appears to be always instantiated to
  --   'NewConnection ...'
  --   i.e., unnecessary polymorphism: reason for? to avoid dup of (NewConnection ...)?


newControlChannel :: forall m srvCntrlMsg.
                     MonadLabelledSTM m
                  => m (ControlChannel m srvCntrlMsg)
newControlChannel = do
    -- Queue size: events will come either from the accept loop or from the
    -- connection manager (when it includes an outbound duplex connection).
    -- GR-FIXME[D2]: above comment is DOU (see code-review-doc)
    channel <-
      atomically $
        newTBQueue 10                             -- G-FIXME[R]: magic number
        >>= \q -> labelTBQueue q "server-cc" $> q
    pure $ ControlChannel {
        readMessage  = readTBQueue channel,
        writeMessage = writeTBQueue channel
      }


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

-- GR-FIXME[C3]: Possibly inline the last two at their call sites, each
--  is just called 1 time in ....ConnectionManager.Core
