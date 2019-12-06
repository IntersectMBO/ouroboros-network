module Ouroboros.Consensus.Ledger.Byron.Conversions (
    -- * From @cardano-ledger@ to @ouroboros-consensus@
    fromByronPrevHash
  , fromByronSlotNo
  , fromByronBlockNo
    -- From @ouroboros-consensus@ to @cardano-ledger@
  , toByronSlotNo
  ) where

import           Data.Coerce

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Slotting as CC

import           Ouroboros.Network.Block (BlockNo(BlockNo),
                     ChainHash(BlockHash, GenesisHash), HeaderHash,
                     SlotNo(SlotNo))

import           Ouroboros.Consensus.Ledger.Byron.Orphans ()

{-------------------------------------------------------------------------------
  From @cardano-ledger@ to @ouroboros-consensus@
-------------------------------------------------------------------------------}

fromByronPrevHash :: (CC.HeaderHash -> HeaderHash b)
                  -> Maybe CC.HeaderHash -> ChainHash b
fromByronPrevHash _ Nothing  = GenesisHash
fromByronPrevHash f (Just h) = BlockHash (f h)

fromByronSlotNo :: CC.SlotNumber -> SlotNo
fromByronSlotNo = coerce

fromByronBlockNo :: CC.ChainDifficulty -> BlockNo
fromByronBlockNo = coerce

{-------------------------------------------------------------------------------
  From @ouroboros-consensus@ to @cardano-ledger@
-------------------------------------------------------------------------------}

toByronSlotNo :: SlotNo -> CC.SlotNumber
toByronSlotNo = coerce
