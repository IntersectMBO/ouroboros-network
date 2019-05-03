{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}

{-# OPTIONS_ghc -fno-warn-orphans #-}

module Ouroboros.Consensus.Ledger.Byron where

import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Slotting as CC.Slot
import Data.ByteString (ByteString)
import Data.Coerce
import Data.FingerTree (Measured(..))
import Data.Word
import Ouroboros.Network.Block

-- | Hard-coded number of slots per epoch in the Byron era
byronEpochSlots :: CC.Slot.EpochSlots
byronEpochSlots = CC.Slot.EpochSlots 21600

-- | Newtype wrapper to avoid orphan instances
newtype ByronBlock = ByronBlock { unByronBlock :: CC.Block.ABlock ByteString }
  deriving (Eq, Show)

instance StandardHash ByronBlock

instance Measured BlockMeasure ByronBlock where
  measure = blockMeasure

instance HasHeader ByronBlock where
  type HeaderHash ByronBlock = CC.Block.HeaderHash

  blockHash = CC.Block.blockHashAnnotated . unByronBlock
  -- TODO distinguish the genesis hash? How do we do this after the fact?
  blockPrevHash = BlockHash . CC.Block.blockPrevHash . unByronBlock
  blockSlot = fromIntegral @Word64 . coerce . CC.Block.blockSlot . unByronBlock
  blockNo = BlockNo . CC.Common.unChainDifficulty . CC.Block.blockDifficulty . unByronBlock
  blockInvariant = const True
