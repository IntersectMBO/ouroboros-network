{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Network.PeerSelection.PeerSelectionActions
  ( withPeerSelectionActions
  , requestPeerSharingResult
  , requestPublicRootPeers
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Void (Void)

import Network.DNS qualified as DNS

import Data.Bifunctor (first)
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionActions (PeerSelectionActions, readLocalRootPeersFromFile))
import Ouroboros.Network.PeerSelection.LedgerPeers hiding (getLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore (DNSSemaphore)
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
           (TracePublicRootPeers, publicRootPeersProvider)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingController,
           PeerSharingResult (..), requestPeers)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))

withPeerSelectionActions
  :: forall extraState extraFlags extraPeers extraAPI extraCounters peeraddr peerconn resolver exception m a.
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
     -> PeerSelectionActions extraState extraFlags extraPeers extraAPI extraCounters peeraddr peerconn m)
  -> WithLedgerPeersArgs extraAPI m
  -> (   (Async m Void, Async m Void)
      -> PeerSelectionActions extraState extraFlags extraPeers extraAPI extraCounters peeraddr peerconn m
      -> m a)
  -- ^ continuation, receives a handle to the local roots peer provider thread
  -- (only if local root peers were non-empty).
  -> m a
withPeerSelectionActions
  localTracer
  localRootsVar
  peerActionsDNS
  getPeerSelectionActions
  ledgerPeersArgs
  k = do
    flip finally (traceWith localTracer $ TraceLocalRootError (DomainAccessPoint "" 1) (toException . userError $ "exiting withPeerSelectionActions")) $ withLedgerPeers
      peerActionsDNS
      ledgerPeersArgs
      (\getLedgerPeers lpThread -> do
          let peerSelectionActions@PeerSelectionActions
                { readLocalRootPeersFromFile
                } = getPeerSelectionActions getLedgerPeers
          withAsync
            ((do
             labelThisThread "local-roots-peers"
             localRootPeersProvider
              localTracer
              peerActionsDNS
              -- NOTE: we don't set `resolvConcurrent` because
              -- of https://github.com/kazu-yamamoto/dns/issues/174
              DNS.defaultResolvConf
              readLocalRootPeersFromFile
              localRootsVar) `finally` (traceWith localTracer $ TraceLocalRootError (DomainAccessPoint "" 1) (toException . userError $ "exiting async localRootPeersProvider")))
            (\lrppThread -> flip finally (traceWith localTracer $ TraceLocalRootError (DomainAccessPoint "" 1) (toException . userError $ "withPeerSelectionActions continuation exit")) $ k (lpThread, lrppThread) peerSelectionActions))

requestPeerSharingResult :: ( MonadSTM m
                            , MonadMVar m
                            , Ord peeraddr
                            )
                         => STM m (Map peeraddr (PeerSharingController peeraddr m))
                         -> PeerSharingAmount
                         -> peeraddr
                         -> m (PeerSharingResult peeraddr)
requestPeerSharingResult sharingController amount peer = do
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


-- | Retrieves public root peers
--
-- This function attempts to fetch ledger peers of a specified kind. If no ledger peers
-- are found, it retrieves extra peers instead. The result includes the
-- public root peers and the time taken for the operation.
--
requestPublicRootPeers
  :: forall m peeraddr extraPeers resolver exception .
    ( MonadThrow m
    , MonadAsync m
    , Exception exception
    , Monoid extraPeers
    , Ord peeraddr
    )
  => Tracer m TracePublicRootPeers
  -> STM m (Map RelayAccessPoint PeerAdvertise)
  -> PeerActionsDNS peeraddr resolver exception m
  -> DNSSemaphore m
  -> (Map peeraddr PeerAdvertise -> extraPeers)
  -- ^ Function to convert DNS result into extra peers
  -> (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
  -> LedgerPeersKind
  -> Int
  -> m (PublicRootPeers extraPeers peeraddr, DiffTime)
requestPublicRootPeers
  publicTracer readPublicRootPeers
  PeerActionsDNS { paToPeerAddr = toPeerAddr
                 , paDnsActions = dnsActions
                 }
  dnsSemaphore toExtraPeers getLedgerPeers ledgerPeersKind n = do
  mbLedgerPeers <- getLedgerPeers (NumberOfPeers $ fromIntegral n)
                                   ledgerPeersKind
  case mbLedgerPeers of
    -- no peers from the ledger
    Nothing -> do
      (extraPeers, dt) <- first toExtraPeers
                      <$> getExtraPeers n
      pure (PublicRootPeers.empty extraPeers, dt)
    Just (ledgerPeers, dt) ->
      case ledgerPeersKind of
        AllLedgerPeers ->
          pure (PublicRootPeers.fromLedgerPeers ledgerPeers, dt)
        BigLedgerPeers ->
          pure (PublicRootPeers.fromBigLedgerPeers ledgerPeers, dt)
  where
    getExtraPeers :: Int -> m (Map peeraddr PeerAdvertise, DiffTime)
    getExtraPeers x =
      -- NOTE: we don't set `resolvConcurrent` because of
      -- https://github.com/kazu-yamamoto/dns/issues/174
      publicRootPeersProvider publicTracer
                              toPeerAddr
                              dnsSemaphore
                              -- NOTE: we don't set `resolveConcurrent` because
                              -- of https://github.com/kazu-yamamoto/dns/issues/174
                              DNS.defaultResolvConf
                              readPublicRootPeers
                              dnsActions
                              ($ x)
