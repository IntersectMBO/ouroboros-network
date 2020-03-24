-- | Assorted utility functions for Shelley protocol integration.
--
-- In particular, various things we need for integration with the @delegation@
-- package from cardano-ledger-specs.
module Ouroboros.Consensus.Shelley.Protocol.Util (
    isNewEpoch
  , prtclStateHash
  , prtclStateSlot
  , prtclStateEta0
  ) where

import           Cardano.Slotting.EpochInfo
import           Cardano.Slotting.Slot
import           Data.Functor.Identity (Identity (..))

import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS

-- | Verify whether a slot represents a change to a new epoch with regard to
-- some other slot.
isNewEpoch
  :: EpochInfo Identity
  -> SlotNo
      -- ^ Slot we want to check
  -> WithOrigin SlotNo
     -- ^ Slot we are comparing a new epoch against
  -> Bool
isNewEpoch ei newSlot referenceWO = runIdentity $ do
    oldEpoch <- epochInfoEpoch ei reference
    newEpoch <- epochInfoEpoch ei newSlot
    pure $ newEpoch > oldEpoch
  where
    reference = fromWithOrigin genesisSlotNo referenceWO
    -- TODO
    genesisSlotNo = SlotNo 0

prtclStateHash
  :: STS.State (STS.PRTCL c)
  -> WithOrigin (SL.HashHeader c)
prtclStateHash (STS.PrtclState _ lastAppliedBlock _ _ _ _) =
    SL.labHash <$> lastAppliedBlock

prtclStateSlot
  :: STS.State (STS.PRTCL c)
  -> WithOrigin SlotNo
prtclStateSlot (STS.PrtclState _ lastAppliedBlock _ _ _ _) =
    SL.labSlotNo <$> lastAppliedBlock

prtclStateEta0
  :: STS.State (STS.PRTCL c)
  -> SL.Nonce
prtclStateEta0 (STS.PrtclState _ _ eta0 _ _ _) = eta0
