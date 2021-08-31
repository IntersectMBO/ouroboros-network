module Ouroboros.Consensus.ByronSpec.Ledger.Forge (forgeByronSpecBlock) where

import qualified Byron.Spec.Chain.STS.Block as Spec
import qualified Byron.Spec.Ledger.Core as Spec
import qualified Byron.Spec.Ledger.Update as Spec

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.ByronSpec.Ledger.Accessors
import           Ouroboros.Consensus.ByronSpec.Ledger.Block
import           Ouroboros.Consensus.ByronSpec.Ledger.Conversions
import qualified Ouroboros.Consensus.ByronSpec.Ledger.GenTx as GenTx
import           Ouroboros.Consensus.ByronSpec.Ledger.Ledger
import           Ouroboros.Consensus.ByronSpec.Ledger.Mempool
import           Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeByronSpecBlock :: BlockNo
                    -> SlotNo
                    -> Ticked (LedgerState ByronSpecBlock)
                    -> [Validated (GenTx ByronSpecBlock)]
                    -> Spec.VKey
                    -> ByronSpecBlock
forgeByronSpecBlock curBlockNo curSlotNo (TickedByronSpecLedgerState _ st) txs vkey =
    ByronSpecBlock {
        byronSpecBlock     = block
      , byronSpecBlockNo   = curBlockNo
      , byronSpecBlockHash = Spec.bhHash $ Spec._bHeader block
      }
  where
    (ds, ts, us, vs) =
        GenTx.partition
          (map (unByronSpecGenTx . forgetValidatedByronSpecGenTx) txs)

    -- TODO: Don't take protocol version from ledger state
    -- <https://github.com/input-output-hk/ouroboros-network/issues/1495>
    block :: Spec.Block
    block = Spec.mkBlock
              (getChainStateHash st)
              (toByronSpecSlotNo curSlotNo)
              vkey
              (Spec.protocolVersion $ getChainStateUPIState st)
              ds
              (case us of
                 []  -> Nothing
                 [u] -> Just u
                 _   -> error "forgeByronSpecBlock: multiple update proposals")
              vs
              ts
