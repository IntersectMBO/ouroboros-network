module Ouroboros.Consensus.Byron.Ledger.Conversions (
    -- * From @cardano-ledger@ to @ouroboros-consensus@
    fromByronPrevHash
  , fromByronSlotNo
  , fromByronBlockNo
  , fromByronBlockCount
  , fromByronEpochSlots
    -- * From @ouroboros-consensus@ to @cardano-ledger@
  , toByronSlotNo
  , toByronBlockCount
    -- * Extract info from the genesis config
  , genesisSecurityParam
  , genesisNumCoreNodes
  ) where

import           Data.Coerce
import qualified Data.Set as Set

import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as CC

import           Ouroboros.Network.Block (ChainHash (..), HeaderHash)

import           Ouroboros.Consensus.Byron.Ledger.Orphans ()
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract

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

fromByronBlockCount :: CC.BlockCount -> SecurityParam
fromByronBlockCount (CC.BlockCount k) = SecurityParam k

fromByronEpochSlots :: CC.EpochSlots -> EpochSize
fromByronEpochSlots (CC.EpochSlots n) = EpochSize n

{-------------------------------------------------------------------------------
  From @ouroboros-consensus@ to @cardano-ledger@
-------------------------------------------------------------------------------}

toByronSlotNo :: SlotNo -> CC.SlotNumber
toByronSlotNo = coerce

toByronBlockCount :: SecurityParam -> CC.BlockCount
toByronBlockCount (SecurityParam k) = CC.BlockCount k

{-------------------------------------------------------------------------------
  Extract info from genesis
-------------------------------------------------------------------------------}

genesisSecurityParam :: Genesis.Config -> SecurityParam
genesisSecurityParam =
      fromByronBlockCount
    . Genesis.gdK
    . Genesis.configGenesisData

genesisNumCoreNodes :: Genesis.Config -> NumCoreNodes
genesisNumCoreNodes =
      NumCoreNodes
    . fromIntegral
    . Set.size
    . Genesis.unGenesisKeyHashes
    . Genesis.gdGenesisKeyHashes
    . Genesis.configGenesisData
