{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Consensus.Ledger.ByronSpec.Forge (
    forgeByronSpecBlock
  ) where

import           Data.Maybe (listToMaybe)

import qualified Cardano.Spec.Chain.STS.Block as Spec
import qualified Ledger.Core as Spec
import qualified Ledger.Update as Spec

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.ByronSpec.Block
import qualified Ouroboros.Consensus.Ledger.ByronSpec.ChainState as ChainState
import           Ouroboros.Consensus.Ledger.ByronSpec.Conversions
import qualified Ouroboros.Consensus.Ledger.ByronSpec.GenTx as GenTx
import           Ouroboros.Consensus.Ledger.ByronSpec.Ledger
import           Ouroboros.Consensus.Ledger.ByronSpec.Mempool
import           Ouroboros.Consensus.Ledger.ByronSpec.Orphans ()

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeByronSpecBlock :: SlotNo
                    -> BlockNo
                    -> LedgerState ByronSpecBlock
                    -> [GenTx ByronSpecBlock]
                    -> Spec.VKey
                    -> ByronSpecBlock
forgeByronSpecBlock curSlotNo curBlockNo ByronSpecLedgerState{..} txs vkey =
    ByronSpecBlock {
        byronSpecBlock     = block
      , byronSpecBlockNo   = curBlockNo
      , byronSpecBlockHash = Spec.bhHash $ Spec._bHeader block
      }
  where
    (ds, ts, us, vs) = GenTx.partition (map unByronSpecGenTx txs)

    block :: Spec.Block
    block = Spec.mkBlock
              (ChainState.getHash byronSpecLedgerState)
              (toByronSpecSlotNo curSlotNo)
              vkey
              (Spec.protocolVersion $
                 ChainState.getUPIState byronSpecLedgerState)
              ds
              (listToMaybe us)
              vs
              ts
