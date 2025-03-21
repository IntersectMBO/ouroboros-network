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
import Network.Mux qualified as Mux
import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.Context (ResponderContext)
import Ouroboros.Network.InboundGovernor.Event

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


-- | Create a new 'InformationChannel' backed by a `TBQueue`.
--
newInformationChannel :: forall a m. (Show a, MonadLabelledSTM m, MonadTraceSTM m)
                      => m (InformationChannel a m)
newInformationChannel = do
    channel <-
      atomically $
        newTBQueue cc_QUEUE_BOUND
        >>= \q -> labelTBQueue q "server-cc" $> q
    traceTBQueueIO channel $ \_ -> return . TraceString . show
    pure $ InformationChannel {
        readMessage  = readTBQueue channel,
        writeMessage = writeTBQueue channel
      }

-- | todo: fixup comment A channel which instantiates to 'NewConnectionInfo' and
-- 'Handle'.
--
-- * /Producer:/ connection manger for duplex outbound connections.
-- * /Consumer:/ inbound governor.
--
type InboundGovernorInfoChannel (muxMode :: Mux.Mode) peerAddr initiatorCtx versionData bytes m a b =
    InformationChannel (Event (muxMode :: Mux.Mode) (Handle muxMode initiatorCtx (ResponderContext peerAddr) versionData bytes m a b) initiatorCtx peerAddr versionData m a b) m

-- | The 'InformationChannel's 'TBQueue' depth.
--
cc_QUEUE_BOUND :: Natural
cc_QUEUE_BOUND = 10
