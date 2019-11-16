{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Mux.Ingress (
    -- $ingress
      demux
    , ingressQueue
    ) where

import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Array
import           GHC.Stack
import           Text.Printf

import           Control.Monad.Class.MonadSay
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
demux :: (MonadSTM m, MonadSay m, MonadThrow m, MonadThrow (STM m)
         , Ord ptcl, Enum ptcl, Show ptcl, ProtocolEnum ptcl
         , MiniProtocolLimits ptcl, HasCallStack)
      => PerMuxSharedState ptcl m -> m ()
demux pmss = forever $ do
    (sdu, _) <- Network.Mux.Types.read $ bearer pmss
    -- say $ printf "demuxing sdu on mid %s mode %s lenght %d " (show $ msId sdu) (show $ msMode sdu)
    --             (BL.length $ msBlob sdu)
    msId <- case toProtocolEnum (msCode sdu) of
              Nothing   -> throwM (MuxError MuxUnknownMiniProtocol
                                     ("id = " ++ show (msCode sdu)) callStack)
              Just msId -> return msId
    atomically $ do
        -- Notice the mode reversal, ModeResponder is delivered to ModeInitiator and vice versa.
        let q = ingressQueue (dispatchTable pmss) msId (negMiniProtocolMode $ msMode sdu)
        buf <- readTVar q
        if BL.length buf + BL.length (msBlob sdu) <= maximumIngressQueue msId
            then writeTVar q $ BL.append buf (msBlob sdu)
            else throwM $ MuxError MuxIngressQueueOverRun
                              (printf "Ingress Queue overrun on %s %s"
                               (show msId) (show $ msMode sdu))
                              callStack

-- | Return the ingress queueu for a given 'MiniProtocolId' and 'MiniProtocolMode'.
ingressQueue :: (MonadSTM m, Ord ptcl, Enum ptcl)
             => MiniProtocolDispatch ptcl m
             -> MiniProtocolId ptcl
             -> MiniProtocolMode
             -> StrictTVar m BL.ByteString
ingressQueue (MiniProtocolDispatch tbl) dis mode =
    tbl ! (dis, mode)
    -- We can use array indexing here, because we constructed the dispatch
    -- table to cover the full range of the type given by 'MiniProtocolId ptcl'



