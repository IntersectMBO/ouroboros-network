{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Ouroboros.Network.PeerSelection.Simple
  ( withPeerSelectionActions
  -- * Re-exports
  , PeerSelectionTargets (..)
  , PeerAdvertise (..)
  ) where


import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.Void (Void)

import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Network.Mux.Timeout

import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS


withPeerSelectionActions
  :: Tracer IO TraceLocalRootPeers
  -> Tracer IO TracePublicRootPeers
  -> TimeoutFn IO
  -> STM IO PeerSelectionTargets
  -> Map Socket.SockAddr PeerAdvertise
  -- ^ static local root peers
  -> StrictTVar IO [(Int, Map RelayAddress PeerAdvertise)]
  -- ^ local root peers
  -> StrictTVar IO [RelayAddress]
  -- ^ public root peers
  -> PeerStateActions Socket.SockAddr peerconn IO
  -> (NumberOfPeers -> STM IO ())
  -> STM IO (Maybe (Set Socket.SockAddr, DiffTime))
  -> (Maybe (Async IO Void) -> PeerSelectionActions Socket.SockAddr peerconn IO -> IO a)
  -- ^ continuation, recieves a handle to the local roots peer provider thread
  -- (only if local root peers where non-empty).
  -> IO a
withPeerSelectionActions localRootTracer publicRootTracer timeout readTargets staticLocalRootPeers
  localRootPeersVar publicRootPeersVar peerStateActions reqLedgerPeers getLedgerPeers k = do
    localRootsVar <- newTVarIO []
    let peerSelectionActions = PeerSelectionActions {
            readPeerSelectionTargets = readTargets,
            readLocalRootPeers = do
              localRoots <- readTVar localRootsVar

              -- TODO: Config support
              -- For now use 1 target per ipaddress/domain name.
              let staticLocalRootPeers' = map (\(sa, a) ->
                    (1, Map.singleton sa a)) $ Map.toList staticLocalRootPeers

              pure $ staticLocalRootPeers' ++ localRoots,
            requestPublicRootPeers = requestLedgerPeers,
            requestPeerGossip = \_ -> pure [],
            peerStateActions
          }
    withAsync
      (localRootPeersProvider
        localRootTracer
        timeout
        DNS.defaultResolvConf
        localRootsVar
        localRootPeersVar)
      (\thread -> k (Just thread) peerSelectionActions)
  where
    -- We first try to get poublic root peers from the ledger, but if it fails
    -- (for example because the node hasn't synced far enough) we fall back
    -- to using the manually configured bootstrap root peers.
    requestLedgerPeers :: Int -> IO (Set Socket.SockAddr, DiffTime)
    requestLedgerPeers n = do
        atomically $ reqLedgerPeers $ NumberOfPeers $ fromIntegral n
        peers_m <- atomically getLedgerPeers
        case peers_m of
             Nothing -> requestPublicRootPeers n
             Just peers -> return peers

    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/input-output-hk/cardano-node/issues/731
    requestPublicRootPeers :: Int -> IO (Set Socket.SockAddr, DiffTime)
    requestPublicRootPeers n =
      publicRootPeersProvider publicRootTracer timeout DNS.defaultResolvConf publicRootPeersVar ($ n)

