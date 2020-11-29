{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- | Intended to be imported qualified.
--
module Ouroboros.Network.ConnectionManager.Server.ControlChannel
  ( ControlMessage (..)
  , SomeControlMessage (..)
  , ControlChannel (..)
  , ServerControlChannel
  , newControlChannel
  , newOutboundConnection
  ) where

import           Control.Monad.Class.MonadSTM.Strict

import           Data.ByteString.Lazy (ByteString)

import           Network.Mux.Types (MuxMode)

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionHandler


-- | The monoitor can receive events from
--
data ControlMessage (muxMode :: MuxMode) peerAddr versionNumber m a b

    -- | After accepting a connection and awaiting for 'Handle' we either
    -- can start all mini-protocols or handle errors.  This is also used to
    -- announce new duple outbound connection.
    = NewConnection
      !(ConnectionId peerAddr)
      !(Handle muxMode peerAddr ByteString m a b)

instance (Show peerAddr, Show versionNumber)
      => Show (ControlMessage muxMode peerAddr versionNumber m a b) where
      show (NewConnection connId _) =
        concat [ "NewConnection "
               , show connId
               ]


data SomeControlMessage peerAddr versionNumber where
    SomeControlMessage :: forall muxMode peerAddr versionNumber m a b.
                          ControlMessage muxMode peerAddr versionNumber m a b
                       -> SomeControlMessage peerAddr versionNumber

instance (Show peerAddr, Show versionNumber)
    => Show (SomeControlMessage peerAddr versionNumber) where
  show (SomeControlMessage msg) = show msg



-- | Server control channel.  It allows to pass 'STM' transactions which will
-- resolve to 'ControlMessages'.   Server's monitoring thread is the consumer
-- of this messages; there are two produceres: accept loop and connection
-- handler for outbound connections.
--
data ControlChannel m srvCntrlMsg =
  ControlChannel {
    -- | Read a single 'ControlMessage' instructrion from the channel.
    --
    readControlMessage :: STM m srvCntrlMsg,

    -- | Write a 'ControlMessage' to the channel.
    --
    writeControlMessage :: srvCntrlMsg -> m ()
  }


type ServerControlChannel m muxMode peerAddr versionNumber a b =
    ControlChannel m (ControlMessage muxMode peerAddr versionNumber m a b)


newControlChannel :: forall m srvCntrlMsg.
                     MonadSTM m
                  => m (ControlChannel m srvCntrlMsg)
newControlChannel = do
    -- Queue size: events will come eihter from the accept loop or from the
    -- connection manager (when it included an outbound duplex connection).
    channel <- newTBQueueIO 10
    pure $ ControlChannel {
        readControlMessage  = readControlMessage channel,
        writeControlMessage = writeControlMessage channel
      }
  where
    readControlMessage
      :: TBQueue m srvCntrlMsg
      -> STM     m srvCntrlMsg
    readControlMessage = readTBQueue

    writeControlMessage
      :: TBQueue m srvCntrlMsg
      -> srvCntrlMsg
      -> m ()
    writeControlMessage q a = atomically $ writeTBQueue q a


newOutboundConnection
    :: ControlChannel m (ControlMessage muxMode peerAddr versionNumber m a b)
    -> ConnectionId peerAddr
    -> Handle muxMode peerAddr ByteString m a b
    -> m ()
newOutboundConnection serverControlChannel connId muxHandle =
    writeControlMessage serverControlChannel (NewConnection connId muxHandle)
