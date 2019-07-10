{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Mux.Ingress (
      demux
    , ingressQueue
    ) where

import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.Array
import           GHC.Stack
import           Text.Printf

import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Network.Mux.Types
import           Network.Mux.Codec

negMiniProtocolMode :: MiniProtocolMode -> MiniProtocolMode
negMiniProtocolMode ModeInitiator = ModeResponder
negMiniProtocolMode ModeResponder = ModeInitiator

-- $ingress
-- = Ingress Path
--
-- >                  o
-- >                  |
-- >                  | ByteStrings
-- >                  |
-- >                  V
-- >          +-------+-------+
-- >          | Bearer.read() | Mux Bearer implementation (Socket, Pipes, etc.)
-- >          +---------------+
-- >                  |
-- >                  | MuxSDUs
-- >                  |
-- >                  V
-- >          +-------+-------+
-- >          |     demux     | For a given Mux Bearer there is a single demux thread reading from
-- >          +-------+-------+ the underlying berarer.
-- >                  |
-- >            +---+-+--+--+
-- >           /    |    |   \
-- >           v    v    v    v
-- >         |  | |  | |  | |  | There is a limited queue (in bytes) for each mode (responder/initiator) per
-- >         |ci| |  | |bi| |br| miniprotocol. Overflowing a queue is a protocol violation and a
-- >         |ci| |cr| |bi| |br| MuxIngressQueueOverRun exception is thrown and the bearer torn down.
-- >         +--+ +--+ +--+ +--+
-- >          |              |
-- >          | CBOR data    |
-- >          V              |
-- >      +---+-------+ Every ingress queue has a dedicated thread which will read
-- >      | muxDuplex | CBOR encoded data from its queue.
-- >      | Initiator |      |
-- >      | ChainSync |      |
-- >      +-----------+      |
-- >                         V
-- >                    +----+------+
-- >                    | muxDuplex |
-- >                    | Responder |
-- >                    | BlockFetch|
-- >                    +-----------+

-- | demux runs as a single separate thread and reads complete 'MuxSDU's from
-- the underlying Mux Bearer and forwards it to the matching ingress queue.
demux :: (MonadSTM m, MonadSay m, MonadThrow (STM m), Ord ptcl, Enum ptcl, Show
      ptcl
         , MiniProtocolLimits ptcl, HasCallStack)
      => PerMuxSharedState ptcl m -> m ()
demux pmss = forever $ do
    (sdu, _) <- Network.Mux.Types.read $ bearer pmss
    -- say $ printf "demuxing sdu on mid %s mode %s lenght %d " (show $ msId sdu) (show $ msMode sdu)
    --             (BL.length $ msBlob sdu)
    atomically $ do
        -- Notice the mode reversal, ModeResponder is delivered to ModeInitiator and vice versa.
        let q = ingressQueue (dispatchTable pmss) (msId sdu) (negMiniProtocolMode $ msMode sdu)
        buf <- readTVar q
        if BL.length buf + BL.length (msBlob sdu) <= maximumIngressQueue (msId sdu)
            then writeTVar q $ BL.append buf (msBlob sdu)
            else throwM $ MuxError MuxIngressQueueOverRun
                              (printf "Ingress Queue overrun on %s %s"
                               (show $ msId sdu) (show $ msMode sdu))
                              callStack

-- | Return the ingress queueu for a given 'MiniProtocolId' and 'MiniProtocolMode'.
ingressQueue :: (MonadSTM m, Ord ptcl, Enum ptcl)
             => MiniProtocolDispatch ptcl m
             -> MiniProtocolId ptcl
             -> MiniProtocolMode
             -> TVar m BL.ByteString
ingressQueue (MiniProtocolDispatch tbl) dis mode =
    tbl ! (dis, mode)
    -- We can use array indexing here, because we constructed the dispatch
    -- table to cover the full range of the type given by 'MiniProtocolId ptcl'



