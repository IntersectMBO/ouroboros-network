{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}

module Ouroboros.Cardano.PeerSelection.PeerSelectionActions (requestPublicRootPeers) where


import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Cardano.Network.Types (LedgerStateJudgement)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync (MonadAsync)
import Control.Monad.Class.MonadThrow (Exception, MonadThrow)
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Network.DNS qualified as DNS
import Ouroboros.Cardano.Network.PublicRootPeers qualified as Cardano
import Ouroboros.Cardano.Network.Types (CardanoPublicRootPeers)
import Ouroboros.Network.PeerSelection.LedgerPeers hiding (getLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSelectionActions qualified as Ouroboros
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS (PeerActionsDNS (..))
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
  -> STM m (Map RelayAccessPoint PeerAdvertise)
  -> PeerActionsDNS peeraddr resolver exception m
  -> DNSSemaphore m
  -> (Map peeraddr PeerAdvertise -> Cardano.ExtraPeers peeraddr)
  -- ^ Function to convert DNS result into extra peers
  -> (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
  -> LedgerPeersKind
  -> Int
  -> m (CardanoPublicRootPeers peeraddr, DiffTime)
requestPublicRootPeers
  publicTracer useBootstrapped
  getLedgerStateJudgement readPublicRootPeers
  pad@PeerActionsDNS { paToPeerAddr = toPeerAddr
                     , paDnsActions = dnsActions
                     }
  dnsSemaphore
  toExtraPeers
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
      Ouroboros.requestPublicRootPeers
        publicTracer
        readPublicRootPeers
        pad
        dnsSemaphore
        toExtraPeers
        getLedgerPeers
        ledgerPeersKind n
  where
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
