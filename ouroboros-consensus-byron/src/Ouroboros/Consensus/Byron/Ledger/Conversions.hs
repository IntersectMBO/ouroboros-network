module Ouroboros.Consensus.Byron.Ledger.Conversions (
    -- * From @cardano-ledger-byron@ to @ouroboros-consensus@
    fromByronSlotNo
  , fromByronBlockNo
  , fromByronBlockCount
  , fromByronEpochSlots
  , fromByronSlotLength
    -- * From @ouroboros-consensus@ to @cardano-ledger-byron@
  , toByronSlotNo
  , toByronBlockCount
  , toByronSlotLength
    -- * Extract info from the genesis config
  , genesisSecurityParam
  , genesisNumCoreNodes
  , genesisSlotLength
  ) where

import           Data.Coerce
import qualified Data.Set as Set
import           Numeric.Natural (Natural)

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Chain.Update as CC

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Byron.Ledger.Orphans ()
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Node.ProtocolInfo

{-------------------------------------------------------------------------------
  From @cardano-ledger-byron@ to @ouroboros-consensus@
-------------------------------------------------------------------------------}

fromByronSlotNo :: CC.SlotNumber -> SlotNo
fromByronSlotNo = coerce

fromByronBlockNo :: CC.ChainDifficulty -> BlockNo
fromByronBlockNo = coerce

fromByronBlockCount :: CC.BlockCount -> SecurityParam
fromByronBlockCount (CC.BlockCount k) = SecurityParam k

fromByronEpochSlots :: CC.EpochSlots -> EpochSize
fromByronEpochSlots (CC.EpochSlots n) = EpochSize n

fromByronSlotLength :: Natural -> SlotLength
fromByronSlotLength = slotLengthFromMillisec
                    . (fromIntegral :: Natural -> Integer)

{-------------------------------------------------------------------------------
  From @ouroboros-consensus@ to @cardano-ledger-byron@
-------------------------------------------------------------------------------}

toByronSlotNo :: SlotNo -> CC.SlotNumber
toByronSlotNo = coerce

toByronBlockCount :: SecurityParam -> CC.BlockCount
toByronBlockCount (SecurityParam k) = CC.BlockCount k

toByronSlotLength :: SlotLength -> Natural
toByronSlotLength = (fromIntegral :: Integer -> Natural)
                  . slotLengthToMillisec

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

genesisSlotLength :: Genesis.Config -> Natural
genesisSlotLength =
      CC.ppSlotDuration
    . Genesis.configProtocolParameters
