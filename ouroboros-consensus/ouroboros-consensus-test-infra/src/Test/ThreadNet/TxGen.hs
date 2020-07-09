{-# LANGUAGE TypeFamilies #-}
-- | Transaction generator for testing
module Test.ThreadNet.TxGen
  ( TxGen (..)
  ) where

import           Data.Kind (Type)

import           Test.QuickCheck (Gen)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))

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
  testGenTxs :: NumCoreNodes
             -> SlotNo
             -> TopLevelConfig blk
             -> TxGenExtra blk
             -> LedgerState blk
             -> Gen [GenTx blk]
