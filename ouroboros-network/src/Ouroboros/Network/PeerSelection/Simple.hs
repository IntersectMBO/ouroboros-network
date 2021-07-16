{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Simple
  ( withPeerSelectionActions
  -- * Re-exports
  , PeerSelectionTargets (..)
  , PeerAdvertise (..)
  ) where


import           Data.Foldable (toList)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer)

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Void (Void)

import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Network.Mux.Timeout

import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.RootPeersDNS


withPeerSelectionActions
  :: forall peeraddr peerconn resolver exception m a.
     ( MonadAsync m
     , MonadDelay m
     , MonadThrow m
     , Ord peeraddr
     , Exception exception
     )
  => Tracer m (TraceLocalRootPeers peeraddr exception)
  -> Tracer m TracePublicRootPeers
  -> (IP -> Socket.PortNumber -> peeraddr)
  -> DNSActions resolver exception m
  -> TimeoutFn m
  -> STM m PeerSelectionTargets
  -> STM m [(Int, Map RelayAddress PeerAdvertise)]
  -- ^ local root peers
  -> STM m [RelayAddress]
  -- ^ public root peers
  -> PeerStateActions peeraddr peerconn m
  -> (NumberOfPeers -> STM m ())
  -> STM m (Maybe (Set peeraddr, DiffTime))
  -> (Maybe (Async m Void)
      -> PeerSelectionActions peeraddr peerconn m
      -> m a)
  -- ^ continuation, recieves a handle to the local roots peer provider thread
  -- (only if local root peers where non-empty).
  -> m a
withPeerSelectionActions
  localRootTracer
  publicRootTracer
  toPeerAddr
  dnsActions
  timeoutFn
  readTargets
  readLocalRootPeers
  readPublicRootPeers
  peerStateActions
  reqLedgerPeers
  getLedgerPeers
  k = do
    localRootsVar <- newTVarIO mempty
    let peerSelectionActions = PeerSelectionActions {
            readPeerSelectionTargets = readTargets,
            readLocalRootPeers = toList <$> readTVar localRootsVar,
            requestPublicRootPeers = requestLedgerPeers,
            requestPeerGossip = \_ -> pure [],
            peerStateActions
          }
    withAsync
      (localRootPeersProvider
        localRootTracer
        toPeerAddr
        timeoutFn
        DNS.defaultResolvConf
        localRootsVar
        readLocalRootPeers
        dnsActions)
      (\thread -> k (Just thread) peerSelectionActions)
  where
    -- We first try to get poublic root peers from the ledger, but if it fails
    -- (for example because the node hasn't synced far enough) we fall back
    -- to using the manually configured bootstrap root peers.
    requestLedgerPeers :: Int -> m (Set peeraddr, DiffTime)
    requestLedgerPeers n = do
        atomically $ reqLedgerPeers $ NumberOfPeers $ fromIntegral n
        peers_m <- atomically getLedgerPeers
        case peers_m of
             Nothing    -> requestPublicRootPeers n
             Just peers -> return peers

    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/input-output-hk/cardano-node/issues/731
    requestPublicRootPeers :: Int -> m (Set peeraddr, DiffTime)
    requestPublicRootPeers n =
      publicRootPeersProvider publicRootTracer
                              toPeerAddr
                              timeoutFn
                              DNS.defaultResolvConf
                              readPublicRootPeers
                              dnsActions
                              ($ n)
