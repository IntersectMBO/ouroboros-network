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

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map (Map)
import qualified Data.Map as Map
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
  :: Tracer IO TraceLocalRootPeers
  -> Tracer IO TracePublicRootPeers
  -> TimeoutFn IO
  -> STM IO PeerSelectionTargets
  -> [(Int, Map RelayAddress PeerAdvertise)]
  -- ^ static local root peers
  -> [(DomainAddress, PeerAdvertise)]
  -- ^ local root peers
  -> [DomainAddress]
  -- ^ public root peers
  -> PeerStateActions Socket.SockAddr peerconn IO
  -> (NumberOfPeers -> STM IO ())
  -> STM IO (Maybe (Set Socket.SockAddr, DiffTime))
  -> (Maybe (Async IO Void) -> PeerSelectionActions Socket.SockAddr peerconn IO -> IO a)
  -- ^ continuation, recieves a handle to the local roots peer provider thread
  -- (only if local root peers where non-empty).
  -> IO a
withPeerSelectionActions localRootTracer publicRootTracer timeout readTargets _staticLocalRootPeers
  localRootPeers publicRootPeers peerStateActions reqLedgerPeers getLedgerPeers k = do
    localRootsVar <- newTVarIO Map.empty
    let peerSelectionActions = PeerSelectionActions {
            readPeerSelectionTargets = readTargets,
            readLocalRootPeers = pure [],
            requestPublicRootPeers,
            requestPeerGossip = \_ -> pure [],
            peerStateActions
          }
    case localRootPeers of
      []       -> k Nothing peerSelectionActions
      (a : as) ->
        withAsync
          (localRootPeersProvider
            localRootTracer
            timeout
            DNS.defaultResolvConf
            localRootsVar
            (a :| as))
          (\thread -> k (Just thread) peerSelectionActions)
  where
    -- We first try to get poublic root peers from the ledger, but if it fails
    -- (for example because the node hasn't synced far enough) we fall back
    -- to using the manually configured bootstrap root peers.
    requestPublicRootPeers :: Int -> IO (Set Socket.SockAddr, DiffTime)
    requestPublicRootPeers n = do
      atomically $ reqLedgerPeers $ NumberOfPeers $ fromIntegral n
      peers_m <- atomically getLedgerPeers
      case peers_m of
        Nothing    -> requestConfiguredRootPeers n
        Just peers -> return peers

    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/input-output-hk/cardano-node/issues/731
    requestConfiguredRootPeers :: Int -> IO (Set Socket.SockAddr, DiffTime)
    requestConfiguredRootPeers n =
      publicRootPeersProvider publicRootTracer
                              timeout
                              DNS.defaultResolvConf
                              publicRootPeers
                              ($ n)

