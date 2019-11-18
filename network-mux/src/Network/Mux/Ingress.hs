{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Mux.Ingress (
    -- $ingress
      demux
    ) where

import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           GHC.Stack
import           Text.Printf

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow

import           Network.Mux.Types

negMiniProtocolMode :: MiniProtocolMode -> MiniProtocolMode
negMiniProtocolMode ModeInitiator = ModeResponder
negMiniProtocolMode ModeResponder = ModeInitiator

-- $ingress
-- = Ingress Path
--
-- >                  ●
-- >                  │
-- >                  │ ByteStrings
-- >                  │
-- >         ░░░░░░░░░▼░░░░░░░░░
-- >         ░┌───────────────┐░
-- >         ░│ Bearer.read() │░ Mux Bearer implementation (Socket, Pipes, etc.)
-- >         ░└───────────────┘░
-- >         ░░░░░░░░░│░░░░░░░░░
-- >                 ░│░         MuxSDUs
-- >         ░░░░░░░░░▼░░░░░░░░░
-- >         ░┌───────────────┐░
-- >         ░│     demux     │░ For a given Mux Bearer there is a single demux
-- >         ░└───────┬───────┘░ thread reading from the underlying bearer.
-- >         ░░░░░░░░░│░░░░░░░░░
-- >                 ░│░
-- >        ░░░░░░░░░░▼░░░░░░░░░░
-- >        ░ ╭────┬────┬─────╮ ░ There is a limited queue (in bytes) for each mode
-- >        ░ │    │    │     │ ░ (responder/initiator) per miniprotocol. Overflowing
-- >        ░ ▼    ▼    ▼     ▼ ░ a queue is a protocol violation and a
-- >        ░│  │ │  │ │  │ │  │░ MuxIngressQueueOverRun exception is thrown
-- >        ░│ci│ │  │ │bi│ │br│░ and the bearer torn down.
-- >        ░│ci│ │cr│ │bi│ │br│░
-- >        ░└──┘ └──┘ └──┘ └──┘░ Every ingress queue has a dedicated thread which will
-- >        ░░│░░░░│░░░░│░░░░│░░░ read application encoded data from its queue.
-- >          │    │    │    │
-- >           application data
-- >          │    │    │    │
-- >          ▼    │    │    ▼
-- > ┌───────────┐ │    │  ┌───────────┐
-- > │ muxDuplex │ │    │  │ muxDuplex │
-- > │ Initiator │ │    │  │ Responder │
-- > │ ChainSync │ │    │  │ BlockFetch│
-- > └───────────┘ │    │  └───────────┘
-- >               ▼    ▼
-- >    ┌───────────┐  ┌───────────┐
-- >    │ muxDuplex │  │ muxDuplex │
-- >    │ Responder │  │ Initiator │
-- >    │ ChainSync │  │ BlockFetch│
-- >    └───────────┘  └───────────┘

-- | demux runs as a single separate thread and reads complete 'MuxSDU's from
-- the underlying Mux Bearer and forwards it to the matching ingress queue.
demux :: (MonadSTM m, MonadThrow m, MonadThrow (STM m), HasCallStack)
      => PerMuxSharedState m -> m ()
demux pmss = forever $ do
    (sdu, _) <- Network.Mux.Types.read $ bearer pmss
    -- say $ printf "demuxing sdu on mid %s mode %s lenght %d " (show $ msId sdu) (show $ msMode sdu)
    --             (BL.length $ msBlob sdu)
    case lookupMiniProtocol (dispatchTable pmss) (msCode sdu)
                            -- Notice the mode reversal, ModeResponder is
                            -- delivered to ModeInitiator and vice versa:
                            (negMiniProtocolMode $ msMode sdu) of
      Nothing   -> throwM (MuxError MuxUnknownMiniProtocol
                          ("id = " ++ show (msCode sdu)) callStack)
      Just (MiniProtocolDispatchInfo q qMax) ->
        atomically $ do
          buf <- readTVar q
          if BL.length buf + BL.length (msBlob sdu) <= qMax
              then writeTVar q $ BL.append buf (msBlob sdu)
              else throwM $ MuxError MuxIngressQueueOverRun
                                (printf "Ingress Queue overrun on %s %s"
                                 (show $ msCode sdu) (show $ msMode sdu))
                                callStack

