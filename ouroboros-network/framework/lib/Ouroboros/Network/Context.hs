{-# LANGUAGE DeriveFunctor #-}

-- | Initiator and responder execution context.
--
module Ouroboros.Network.Context
  ( ExpandedInitiatorContext (..)
  , MinimalInitiatorContext (..)
  , ResponderContext (..)
    -- * Re-exports
  , PeerRTT (..)
  , noPeerRTT
  , ConnectionId (..)
  , ControlMessageSTM
  , IsBigLedgerPeer (..)
  ) where

import Network.Mux (PeerRTT (..), noPeerRTT)

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ControlMessage
import Ouroboros.Network.PeerSelection.LedgerPeers.Type


-- | Context passed to initiator mini-protocol execution.
--
data ExpandedInitiatorContext addr extraFlags m = ExpandedInitiatorContext {
    eicConnectionId    :: !(ConnectionId addr),
    eicControlMessage  :: !(ControlMessageSTM m),
    eicIsBigLedgerPeer :: !IsBigLedgerPeer,
    eicExtraFlags      :: !extraFlags,
    -- ^ in `cardano-diffusion` it's instantiated to `PeerTrustable`, in `dmq-node` to `NoExtraFlags`
    eicPeerRTT         :: !(PeerRTT m)
    -- ^ read handle for the per-peer RTT distribution
  }

-- | A context passed to initiator mini-protocol execution for non-p2p
-- applications.
--
newtype MinimalInitiatorContext addr = MinimalInitiatorContext {
    micConnectionId   :: ConnectionId addr
  }
  deriving Functor

-- | Context passed to each responder mini-protocol execution.
--
data ResponderContext addr m = ResponderContext {
    rcConnectionId :: !(ConnectionId addr),
    rcPeerRTT      :: !(PeerRTT m)
    -- ^ read handle for the per-peer RTT distribution
  }
