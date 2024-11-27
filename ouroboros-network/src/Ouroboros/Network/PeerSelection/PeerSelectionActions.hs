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
  , getPeerShare
  , getPublicRootPeers
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
import Ouroboros.Network.PeerSelection.State.LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingController, requestPeers)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))

withPeerSelectionActions
  :: forall extraActions extraPeers extraFlags extraAPI peeraddr peerconn resolver exception m a.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadDelay m
     , MonadThrow m
     , Ord peeraddr
     , Exception exception
     , Eq extraFlags
     )
  => Tracer m (TraceLocalRootPeers extraFlags peeraddr exception)
  -> StrictTVar m (Config extraFlags peeraddr)
  -> PeerActionsDNS peeraddr resolver exception m
  -> ( (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
     -> PeerSelectionActions extraActions extraPeers extraFlags extraAPI peeraddr peerconn m)
  -> WithLedgerPeersArgs extraAPI m
  -> (   (Async m Void, Async m Void)
      -> PeerSelectionActions extraActions extraPeers extraFlags extraAPI peeraddr peerconn m
      -> m a)
  -- ^ continuation, receives a handle to the local roots peer provider thread
  -- (only if local root peers were non-empty).
  -> m a
withPeerSelectionActions
  localTracer
  localRootsVar
  paDNS
  getPeerSelectionActions
  ledgerPeersArgs
  k = do
    withLedgerPeers
      paDNS
      ledgerPeersArgs
      (\getLedgerPeers lpThread -> do
          let peerSelectionActions@PeerSelectionActions
                { readOriginalLocalRootPeers
                } = getPeerSelectionActions getLedgerPeers
          withAsync
            (localRootPeersProvider
              localTracer
              paDNS
              -- NOTE: we don't set `resolvConcurrent` because
              -- of https://github.com/kazu-yamamoto/dns/issues/174
              DNS.defaultResolvConf
              readOriginalLocalRootPeers
              localRootsVar)
            (\lrppThread -> k (lpThread, lrppThread) peerSelectionActions))

getPeerShare :: ( MonadSTM m
                , MonadMVar m
                , Ord peeraddr
                )
             => STM m (Map peeraddr (PeerSharingController peeraddr m))
             -> PeerSharingAmount
             -> peeraddr
             -> m (PeerSharingResult peeraddr)
getPeerShare sharingController amount peer = do
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

-- We start by reading the current ledger state judgement, if it is
-- YoungEnough we only care about fetching for ledger peers, otherwise we
-- aim to fetch bootstrap peers.
getPublicRootPeers
  :: ( Monad m
     , Monoid extraPeers
     )
  => (NumberOfPeers -> m (extraPeers, DiffTime))
  -> (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
  -> LedgerPeersKind
  -> Int
  -> m (PublicRootPeers extraPeers peeraddr, DiffTime)
getPublicRootPeers getExtraPeers getLedgerPeers ledgerPeersKind n = do
  mbLedgerPeers <- getLedgerPeers (NumberOfPeers $ fromIntegral n) ledgerPeersKind
  case mbLedgerPeers of
    -- no peers from the ledger
    Nothing -> do
      (extraPeers, dt) <- getExtraPeers (NumberOfPeers $ fromIntegral n)
      pure (PublicRootPeers.empty extraPeers, dt)
    Just (ledgerPeers, dt) ->
      case ledgerPeersKind of
        AllLedgerPeers ->
          pure (PublicRootPeers.fromLedgerPeers ledgerPeers, dt)
        BigLedgerPeers ->
          pure (PublicRootPeers.fromBigLedgerPeers ledgerPeers, dt)
