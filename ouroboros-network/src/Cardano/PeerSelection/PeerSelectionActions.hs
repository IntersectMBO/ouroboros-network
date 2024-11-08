{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}

module Cardano.PeerSelection.PeerSelectionActions (requestPublicRootPeers) where


import Cardano.Node.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Cardano.Node.PublicRootPeers (CardanoPublicRootPeers (..))
import Cardano.Node.Types (LedgerStateJudgement)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync (MonadAsync)
import Control.Monad.Class.MonadThrow (Exception, MonadThrow)
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Network.DNS qualified as DNS
import Network.Socket (PortNumber)
import Ouroboros.Network.PeerSelection.LedgerPeers hiding (getLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSelectionActions (getPublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSActions)
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore (DNSSemaphore)
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers

-- We start by reading the current ledger state judgement, if it is
-- YoungEnough we only care about fetching for ledger peers, otherwise we
-- aim to fetch bootstrap peers.
requestPublicRootPeers
  :: forall m peeraddr resolver exception .
    ( MonadThrow m
    , MonadAsync m
    , Exception exception
    , Ord peeraddr
    )
  => Tracer m TracePublicRootPeers
  -> STM m UseBootstrapPeers
  -> STM m LedgerStateJudgement
  -> (IP -> PortNumber -> peeraddr)
  -> DNSSemaphore m
  -> STM m (Map RelayAccessPoint PeerAdvertise)
  -> DNSActions resolver exception m
  -> (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
  -> LedgerPeersKind
  -> Int
  -> m (PublicRootPeers (CardanoPublicRootPeers peeraddr) peeraddr, DiffTime)
requestPublicRootPeers
  publicTracer useBootstrapped getLedgerStateJudgement
  toPeerAddr dnsSemaphore readPublicRootPeers dnsActions
  getLedgerPeers ledgerPeersKind n = do
  -- Check if the node is in a sensitive state
  usingBootstrapPeers <- atomically
                       $ requiresBootstrapPeers <$> useBootstrapped
                                                <*> getLedgerStateJudgement
  if usingBootstrapPeers
     then do
      -- If the ledger state is in sensitive state we should get trustable peers.
      (bootstrapPeers, dt) <- requestConfiguredBootstrapPeers n
      pure (PublicRootPeers.fromBootstrapPeers bootstrapPeers, dt)
     else do
      getPublicRootPeers ( fmap (first (flip CardanoPublicRootPeers Set.empty))
                         . getExtraPeers
                         . fromIntegral
                         . getNumberOfPeers
                         )
                         getLedgerPeers
                         ledgerPeersKind n
  where
    -- If the ledger state is not in a sensitive state we should get ledger
    -- peers, the Nothing case should not happen but there can be a race
    -- condition. If that's the case we try again soon enough.
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

    requestConfiguredBootstrapPeers :: Int -> m (Set peeraddr, DiffTime)
    requestConfiguredBootstrapPeers x = do
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
                              (fmap (first Map.keysSet) . ($ x))
