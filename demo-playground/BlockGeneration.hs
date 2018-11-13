{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BlockGeneration (forkCoreNode) where

import           Control.Monad.State
import           Crypto.Random

import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain (Chain (..), headBlockNo, headHash)
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.MonadClass
import           Ouroboros.Network.Serialise

import           LedgerState
import           Mock.Mempool (Mempool, collect)


forkCoreNode :: forall m p c. ( Mock.SimpleBlockCrypto c
                  , Serialise (OuroborosPayload p (Mock.SimplePreHeader p c))
                  , MonadSTM m
                  , MonadTimer m
                  , MonadFork m
                  , MonadIO m
                  , RunOuroboros p DemoLedgerState
                  )
               => DemoLedgerState
               -> OuroborosState p
               -> TVar m (Mempool Mock.Tx)
               -> TVar m (ChainProducerState (Mock.SimpleBlock p c))
               -> Duration (Time m)
               -> m ()
forkCoreNode initLedger protocolState mempoolVar varCPS slotDuration = do
    ledgerVar <- atomically $ newTVar initLedger
    slotVar   <- atomically $ newTVar 1
    initDRG   <- liftIO getSystemDRG

    let runProtocol :: MonadPseudoRandomT SystemDRG (OuroborosStateT p (Tr m)) a
                    -> Tr m a
        runProtocol m =
            fst <$> runOuroborosStateT (fst <$> withDRGT m initDRG) protocolState

    fork $ forever $ do
        threadDelay slotDuration
        atomically $ do
            currentLedger <- readTVar ledgerVar
            slot          <- readTVar slotVar
            mIsLeader <- runProtocol $ checkIsLeader (Slot slot) currentLedger
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

                block <- runProtocol $ Mock.forgeBlock (Slot slot) curNo prevHash txs proof
                writeTVar varCPS cps{ chainState = chain :> block }
            writeTVar slotVar (succ slot)
