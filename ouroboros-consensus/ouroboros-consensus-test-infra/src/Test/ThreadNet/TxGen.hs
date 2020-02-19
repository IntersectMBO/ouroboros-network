-- | Transaction generator for testing
module Test.ThreadNet.TxGen
  ( TxGen (..)
  ) where

import           Control.Monad (replicateM)
import           Crypto.Number.Generate (generateBetween)

import           Cardano.Slotting.Slot (SlotNo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (GenTx)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random

{-------------------------------------------------------------------------------
  TxGen class
-------------------------------------------------------------------------------}

class TxGen blk where
  -- | Generate a transaction, valid or invalid, that can be submitted to a
  -- node's Mempool.
  testGenTx :: MonadRandom m
            => NumCoreNodes
            -> SlotNo
            -> NodeConfig (BlockProtocol blk)
            -> LedgerState blk
            -> m (GenTx blk)

  -- | Generate a number of transactions, valid or invalid, that can be
  -- submitted to a node's Mempool.
  --
  -- This function (not 'testGenTx') will be called to generate transactions
  -- in consensus tests.
  testGenTxs :: MonadRandom m
             => NumCoreNodes
             -> SlotNo
             -> NodeConfig (BlockProtocol blk)
             -> LedgerState blk
             -> m [GenTx blk]
  testGenTxs  numCoreNodes curSlotNo cfg ledger = do
    -- Currently 0 to 1 txs
    n <- generateBetween 0 20
    replicateM (fromIntegral n) $ testGenTx numCoreNodes curSlotNo cfg ledger

  {-# MINIMAL testGenTx #-}
