{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BlockGeneration (forkCoreNode) where

import           Control.Monad.State
import           Crypto.Random

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), headBlockNo, headHash)
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.MonadClass

import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random

import           LedgerState
import           Mock.Mempool (Mempool, collect)


forkCoreNode :: forall m p c.
                ( Mock.SimpleBlockCrypto c
                , MonadSTM m
                , MonadTimer m
                , MonadIO m
                , ProtocolLedgerView (Mock.SimpleBlock p c)
                )
             => TVar m (DemoLedgerState (Mock.SimpleBlock p c))
             -> OuroborosNodeConfig p
             -> OuroborosNodeState p
             -> TVar m (Mempool Mock.Tx)
             -> TVar m (ChainProducerState (Mock.SimpleBlock p c))
             -> Duration (Time m)
             -> m ()
forkCoreNode ledgerVar cfg initState mempoolVar varCPS slotDuration = do
    slotVar   <- atomically $ newTVar 1
    initDRG   <- liftIO getSystemDRG

    let runProtocol :: forall a. MonadPseudoRandomT SystemDRG (OuroborosNodeStateT p (Tr m)) a
                    -> Tr m a
        runProtocol m =
            -- TODO: This is not correct, this re-runs from the initial state
            -- each time.
            fst <$> runOuroborosNodeStateT (fst <$> withDRGT m initDRG) initState

    fork $ forever $ do
        threadDelay slotDuration
        atomically $ do
            currentLedger <- readTVar ledgerVar
            slot          <- readTVar slotVar
            mIsLeader     <- runProtocol $
                               checkIsLeader
                                 cfg
                                 (Slot slot)
                                 (protocolLedgerView cfg . ledgerState $ extLedgerState currentLedger)
                                 (ouroborosChainState $ extLedgerState currentLedger)
            case mIsLeader of
              Nothing    -> return ()
              Just proof -> do
                cps    <- readTVar varCPS
                let chain    = chainState cps
                    prevHash = castHash (headHash chain)
                    prevNo   = headBlockNo chain
                    curNo    = succ prevNo

                -- Before generating a new block, look for incoming transactions.
                -- If there are, check if the mempool is consistent and, if it is,
                -- grab the valid new transactions and incorporate them into a
                -- new block.
                mp  <- readTVar mempoolVar
                txs <- do let (ts, mp') = collect (Mock.utxo chain) mp
                                                $ (Mock.confirmed chain)
                          writeTVar mempoolVar mp'
                          return ts

                block <- runProtocol $ Mock.forgeBlock cfg (Slot slot) curNo prevHash txs proof
                writeTVar varCPS cps{ chainState = chain :> block }
            writeTVar slotVar (succ slot)
