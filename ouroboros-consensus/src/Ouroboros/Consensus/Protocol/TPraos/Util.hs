-- | Assorted utility functions for TPraos integration.
--
-- In particular, various things we need for integration with the `delegation`
-- package from cardano-ledger-specs.
module Ouroboros.Consensus.Protocol.TPraos.Util where

import Ouroboros.Consensus.Protocol.TPraos.Crypto
import Ouroboros.Network.Block (SlotNo (..))

import Slot (Slot(..))
import qualified STS.Prtcl as STS

-- Useful type alias
type PRTCL c = STS.PRTCL (TPraosHash c) (TPraosDSIGN c) (TPraosKES c)

-- | Convert a ouroboros-consensus `SlotNo` to a cardano-ledger-specs `Slot`.
convertSlot :: SlotNo -> Slot
convertSlot (SlotNo n) = Slot $ fromIntegral n

-- | Convert a cardano-ledger-specs `Slot` to an ouroboros-consensus `SlotNo`.
convertSlotNo :: Slot -> SlotNo
convertSlotNo (Slot n) = SlotNo $ fromIntegral n
