{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module BlockGeneration (blockGenerator) where

import           Control.Monad
import           Data.Tuple (swap)

import           Ouroboros.Consensus.UTxO.Mempool (Mempool, collect)
import qualified Ouroboros.Consensus.UTxO.Mock as Mock
import           Ouroboros.Network.Block
import           Ouroboros.Network.ChainProducerState
import           Ouroboros.Network.MonadClass

import           Payload

blockGenerator :: ( HasHeader (Payload pt)
                  , MonadSTM m stm
                  , MonadTimer m
                  , MonadConc m
                  , Mock.HasUtxo (Payload pt)
                  , PayloadImplementation pt
                  )
               => MVar m (Mempool Mock.Tx)
               -> TVar m (ChainProducerState (Payload pt))
               -> Duration (Time m)
               -> [Payload pt]
               -> m (TVar m (Maybe (Payload pt)))
blockGenerator mempoolVar cps slotDuration chain = do
    outputVar <- atomically (newTVar Nothing)
    forM_ chain $ \b ->
        timer (slotDuration * fromIntegral (getSlot $ blockSlot b)) $ do
            currentChain <- chainState <$> atomically (readTVar cps)
            -- Before generating a new block, look for incoming transactions.
            -- If there are, check if the mempool is consistent and, if it is,
            -- grab the valid new transactions and incorporate them into a
            -- new block.
            valid <- modifyMVar mempoolVar $ \mempool ->
                         return $ swap
                                $ collect (Mock.utxo currentChain) mempool
                                $ (Mock.confirmed currentChain)

            let b' = addTxs valid b
            atomically (writeTVar outputVar (Just b'))
    return outputVar
