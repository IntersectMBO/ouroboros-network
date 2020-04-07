{-# LANGUAGE TypeFamilies #-}
-- | Transaction generator for testing
module Test.ThreadNet.TxGen
  ( TxGen (..)
  ) where

import           Cardano.Slotting.Slot (SlotNo)
import           Data.Kind (Type)

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API (GenTx)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Util.Random

{-------------------------------------------------------------------------------
  TxGen class
-------------------------------------------------------------------------------}

class TxGen blk where

  -- | Extra information required to generate transactions
  type TxGenExtra blk :: Type
  type TxGenExtra blk = ()

  -- | Generate a number of transactions, valid or invalid, that can be
  -- submitted to a node's Mempool.
  --
  -- This function will be called to generate transactions in consensus tests.
  --
  -- Note: this function returns a list so that an empty list can be returned
  -- in case we are unable to generate transactions for a @blk@.
  testGenTxs :: MonadRandom m
             => NumCoreNodes
             -> SlotNo
             -> TopLevelConfig blk
             -> TxGenExtra blk
             -> LedgerState blk
             -> m [GenTx blk]
