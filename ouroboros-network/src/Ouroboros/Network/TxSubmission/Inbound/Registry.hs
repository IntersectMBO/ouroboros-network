{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.Registry where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)

import Ouroboros.Network.DeltaQ (PeerGSV (..))
import Ouroboros.Network.TxSubmission.Inbound.State
import Ouroboros.Network.TxSubmission.Inbound.Decision


type PeerRegistry m peeraddr = StrictTVar m (Set peeraddr)

newtype PeerRegistryAPI m peeraddr = PeerRegistryAPI {
    getPeers :: STM m (Set peeraddr)
  }

-- TODO: can we re-use one of the existing registries?
--
withPeerRegistry :: ( MonadSTM   m
                    , MonadThrow m
                    , Ord peeraddr
                    )
                 => PeerRegistry m peeraddr
                 -> peeraddr
                 -> m a
                 -> m a
withPeerRegistry registry peeraddr k =
    bracket_ (atomically $ modifyTVar registry (Set.insert peeraddr))
             (atomically $ modifyTVar registry (Set.delete peeraddr))
             k
                           

-- | Filter peers which can either download a `tx` or acknowledge `txid`s
--
filterActivePeers
  :: forall peeraddr txid tx.
     Ord peeraddr
  => TxDecisionPolicy
  -> SharedTxState peeraddr txid tx
  -> Set peeraddr
  -> Map peeraddr (PeerTxState txid tx)
filterActivePeers
  TxDecisionPolicy { txsSizeInflightPerPeer,
                     maxTxsSizeInflight,
                     txInflightMultiplicity }
  SharedTxState { peerTxStates }
  peers
  =
    peerTxStates
    `Map.restrictKeys`
    Set.filter fn peers
  where
    fn :: peeraddr -> Bool
    fn peeraddr = undefined


decisionLogic :: forall m peeraddr txid tx.
                 ( MonadSTM m
                 , Ord peeraddr
                 , Ord txid
                 , Show txid
                 )
              => TxDecisionPolicy
              -> PeerRegistryAPI m peeraddr
              -> StrictTVar m (Map peeraddr PeerGSV)
              -> SharedTxStateVar m peeraddr txid tx
              -> m Void
decisionLogic policy PeerRegistryAPI { getPeers } gsvVar sharedStateVar = go
  where
    go :: m Void
    go = do
      atomically $ do
        sharedCtx <- SharedDecisionContext
          <$> readTVar gsvVar
          <*> readTVar sharedStateVar
        activePeers <- filterActivePeers policy (sdcSharedTxState sharedCtx) <$> getPeers
        -- TODO:
        -- limit peers to ones that are actually worth reading
        -- this can give an opportunity to split `SharedTxState` into two `TVar`s
        let (sharedState, decisions) = makeDecisions policy sharedCtx activePeers
        writeTVar sharedStateVar sharedState
        -- TODO:
        -- communicate with all clients, e.g. write decisions
      go

