{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.HeaderValidation () where

import           Control.Arrow ((&&&))
import           Control.Monad.Except
import qualified Data.Text as T
import           Data.Word

import           Cardano.Slotting.Slot (WithOrigin (..), withOrigin)

import qualified Cardano.Chain.Slotting as CC

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Protocol.ExtConfig

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Config
import           Ouroboros.Consensus.Byron.Ledger.Orphans ()
import           Ouroboros.Consensus.Byron.Ledger.PBFT ()

{-------------------------------------------------------------------------------
  Envelope
-------------------------------------------------------------------------------}

instance HasAnnTip ByronBlock where
  type TipInfo ByronBlock = IsEBB
  getTipInfo = byronHeaderIsEBB

instance ValidateEnvelope ByronBlock where
  validateEnvelope cfg oldTip hdr = do
      when (actualBlockNo /= expectedBlockNo) $
        throwError $ UnexpectedBlockNo expectedBlockNo actualBlockNo
      when (actualSlotNo < expectedSlotNo) $
        throwError $ UnexpectedSlotNo expectedSlotNo actualSlotNo
      when (actualPrevHash /= expectedPrevHash) $
        throwError $ UnexpectedPrevHash expectedPrevHash actualPrevHash
      when (fromIsEBB newIsEBB && not (canBeEBB actualSlotNo)) $
        throwError $ OtherEnvelopeError . T.pack $
          "Unexpected EBB in slot " ++ show actualSlotNo
    where
      newIsEBB :: IsEBB
      newIsEBB = byronHeaderIsEBB hdr

      actualSlotNo   :: SlotNo
      actualBlockNo  :: BlockNo
      actualPrevHash :: ChainHash ByronBlock

      actualSlotNo   =            blockSlot     hdr
      actualBlockNo  =            blockNo       hdr
      actualPrevHash = castHash $ blockPrevHash hdr

      expectedSlotNo   :: SlotNo           -- Lower bound only
      expectedBlockNo  :: BlockNo
      expectedPrevHash :: ChainHash ByronBlock

      (expectedSlotNo, expectedBlockNo, expectedPrevHash) = (
            nextSlotNo  ((annTipInfo &&& annTipSlotNo)  <$> oldTip) newIsEBB
          , nextBlockNo ((annTipInfo &&& annTipBlockNo) <$> oldTip) newIsEBB
          , withOrigin GenesisHash (BlockHash . annTipHash) oldTip
          )

      -- EBB shares its slot number with its successor
      nextSlotNo :: WithOrigin (IsEBB, SlotNo) -> IsEBB -> SlotNo
      nextSlotNo Origin          _        = SlotNo 0
      nextSlotNo (At (IsEBB, s)) IsNotEBB = s
      nextSlotNo (At (_    , s)) _        = succ s

      -- EBB shares its block number with its predecessor
      nextBlockNo :: WithOrigin (IsEBB, BlockNo) -> IsEBB -> BlockNo
      nextBlockNo Origin             _     = BlockNo 0
      nextBlockNo (At (IsNotEBB, b)) IsEBB = b
      nextBlockNo (At (_       , b)) _     = succ b

      canBeEBB :: SlotNo -> Bool
      canBeEBB (SlotNo s) = s `mod` epochSlots == 0

      epochSlots :: Word64
      epochSlots = CC.unEpochSlots $ pbftEpochSlots $
                     extNodeConfig (configConsensus cfg)
