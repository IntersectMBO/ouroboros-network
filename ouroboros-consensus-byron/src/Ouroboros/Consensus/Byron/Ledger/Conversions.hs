module Ouroboros.Consensus.Byron.Ledger.Conversions (
    -- * From @cardano-ledger@ to @ouroboros-consensus@
    fromByronPrevHash
  , fromByronSlotNo
  , fromByronBlockNo
  , fromByronBlockCount
    -- * From @ouroboros-consensus@ to @cardano-ledger@
  , toByronSlotNo
    -- * Extract info from the genesis config
  , genesisSecurityParam
  , genesisNumCoreNodes
  ) where

import           Data.Coerce
import qualified Data.Set as Set

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as CC

import           Ouroboros.Network.Block

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
fromByronBlockCount (CC.BlockCount k) = SecurityParam (fromIntegral k)

{-------------------------------------------------------------------------------
  From @ouroboros-consensus@ to @cardano-ledger@
-------------------------------------------------------------------------------}

toByronSlotNo :: SlotNo -> CC.SlotNumber
toByronSlotNo = coerce

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
