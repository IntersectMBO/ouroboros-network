{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}

module Ouroboros.Network.PeerSelection.PeerSelectionActions
  ( withPeerSelectionActions
    -- * Re-exports
  , PeerSelectionTargets (..)
  , PeerAdvertise (..)
  , PeerSelectionActionsArgs (..)
  , PeerSelectionActionsDiffusionMode (..)
  ) where


import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Void (Void)

import Network.DNS qualified as DNS

import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers hiding (getLedgerPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingController, requestPeers)
     )
  => StrictTVar m (Config extraFlags peeraddr)
  -> PeerActionsDNS peeraddr resolver exception m
  -> PeerSelectionActionsArgs extraActions extraAPI extraPeers extraFlags peeraddr peerconn exception m
  -> WithLedgerPeersArgs extraAPI m
  -> PeerSelectionActionsDiffusionMode peeraddr peerconn m
  -> (   (Async m Void, Async m Void)
      -> PeerSelectionActions extraState extraActions extraPeers extraFlags extraAPI extraCounters peeraddr peerconn m
      -> m a)
  -- ^ continuation, receives a handle to the local roots peer provider thread
  -- (only if local root peers were non-empty).
  -> m a
withPeerSelectionActions
  => Tracer m (TraceLocalRootPeers extraFlags peeraddr exception)
  -> StrictTVar m (Config extraFlags peeraddr)
  paDNS@PeerActionsDNS { paToPeerAddr = toPeerAddr, paDnsActions = dnsActions }
  PeerSelectionActionsArgs {
    psLocalRootPeersTracer      = localTracer,
    psReadTargets               = selectionTargets,
    psReadLocalRootPeers        = localRootPeers,
    psPeerSharing               = sharing,
    psPeerConnToPeerSharing     = peerConnToPeerSharing,
    psReadPeerSharingController = sharingController,
    psReadInboundPeers          = readInboundPeers,
    psRequestPublicRootPeers,
    psExtraActions,
    readLedgerPeerSnapshot,
    getLedgerStateCtx
  }
  ledgerPeersArgs
  PeerSelectionActionsDiffusionMode { psPeerStateActions = peerStateActions }
  k = do
    withLedgerPeers
      paDNS
      ledgerPeersArgs
      (\getLedgerPeers lpThread -> do
          let peerSelectionActions =
          let peerSelectionActions@PeerSelectionActions
                { readOriginalLocalRootPeers
                } = getPeerSelectionActions getLedgerPeers
            (localRootPeersProvider
              localTracer
              toPeerAddr
              -- NOTE: we don't set `resolvConcurrent` because
              -- of https://github.com/kazu-yamamoto/dns/issues/174
              DNS.defaultResolvConf
              dnsActions
              localRootPeers
              localRootsVar)
            (\lrppThread -> k (lpThread, lrppThread) peerSelectionActions))

    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/intersectmbo/cardano-node/issues/731
    requestConfiguredPublicRootPeers :: Int -> m (Map peeraddr PeerAdvertise, DiffTime)
    requestConfiguredPublicRootPeers n =
      -- NOTE: we don't set `resolvConcurrent` because of
      -- https://github.com/kazu-yamamoto/dns/issues/174
      publicRootPeersProvider publicTracer
                              toPeerAddr
                              dnsSemaphore
                              -- NOTE: we don't set `resolveConcurrent` because
                              -- of https://github.com/kazu-yamamoto/dns/issues/174
                              DNS.defaultResolvConf
                              publicRootPeers
                              dnsActions
                              ($ n)

    requestConfiguredBootstrapPeers :: Int -> m (Set peeraddr, DiffTime)
    requestConfiguredBootstrapPeers n = do
      let readBootstrapPeersMap =
            fmap (\case
                    DontUseBootstrapPeers     -> Map.empty
                    UseBootstrapPeers domains ->
                      Map.fromList ((,DoNotAdvertisePeer) <$> domains)
                 )
                 useBootstrapped

      publicRootPeersProvider publicTracer
                              toPeerAddr
                              dnsSemaphore
                              DNS.defaultResolvConf
                              readBootstrapPeersMap
                              dnsActions
                              (fmap (first Map.keysSet) . ($ n))

    requestPeerShare :: PeerSharingAmount -> peeraddr -> m (PeerSharingResult peeraddr)
    requestPeerShare amount peer = do
      controllerMap <- atomically sharingController
      case Map.lookup peer controllerMap of
        -- Peer Registering happens asynchronously with respect to
        -- requestPeerShare. This means that there's a possible race where the
        -- Peer Selection Governor can decide to peer share request to a peer
        -- for the peer is registered. When this happens this map lookup is
        -- going to fail, so instead of erroring we report this to the governor
        -- so it can deal with this particular case accordingly.
        Nothing -> return PeerSharingNotRegisteredYet
        Just psController ->
          PeerSharingResult <$> requestPeers psController amount
