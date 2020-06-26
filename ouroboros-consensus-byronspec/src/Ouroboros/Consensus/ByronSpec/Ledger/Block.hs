{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.ByronSpec.Ledger.Block (
    ByronSpecBlock(..)
  , ByronSpecHeader -- type alias
  , Header(..)
  , BlockConfig(..)
  ) where

import           Codec.Serialise
import           Data.FingerTree.Strict (Measured (..))
import           GHC.Generics (Generic)

import qualified Byron.Spec.Chain.STS.Block as Spec
import qualified Byron.Spec.Ledger.Core as Spec

import           Ouroboros.Consensus.Block

import           Ouroboros.Consensus.ByronSpec.Ledger.Conversions
import           Ouroboros.Consensus.ByronSpec.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Block according to the Byron spec
--
-- Just like we do for 'ByronBlock', we cache the header hash. In addition, we
-- also add the 'BlockNo', as this is entirely absent from the spec but we need
-- it for the 'HasHeader' abstraction, which is ubiquitous in
-- @ouroboros-consensus@ and @-network@.
data ByronSpecBlock = ByronSpecBlock {
      byronSpecBlock     :: Spec.Block
    , byronSpecBlockNo   :: BlockNo
    , byronSpecBlockHash :: Spec.Hash
    }
  deriving (Show, Eq, Generic, Serialise)

{-------------------------------------------------------------------------------
  GetHeader
-------------------------------------------------------------------------------}

instance GetHeader ByronSpecBlock where
  data Header ByronSpecBlock = ByronSpecHeader {
        byronSpecHeader     :: Spec.BlockHeader
      , byronSpecHeaderNo   :: BlockNo
      , byronSpecHeaderHash :: Spec.Hash
      }
    deriving (Show, Eq, Generic, Serialise)

  getHeader ByronSpecBlock{..} = ByronSpecHeader {
        byronSpecHeader     = Spec._bHeader byronSpecBlock
      , byronSpecHeaderNo   = byronSpecBlockNo
      , byronSpecHeaderHash = byronSpecBlockHash
      }

  -- We don't care about integrity checks, so we don't bother checking whether
  -- the hashes of the body are correct
  blockMatchesHeader hdr blk = blockHash hdr == blockHash blk

  -- No EBBs
  headerIsEBB = const Nothing

type ByronSpecHeader = Header ByronSpecBlock

{-------------------------------------------------------------------------------
  HasHeader
-------------------------------------------------------------------------------}

type instance HeaderHash ByronSpecBlock = Spec.Hash
instance StandardHash ByronSpecBlock

instance Measured BlockMeasure ByronSpecBlock where
  measure = blockMeasure

instance HasHeader ByronSpecBlock where
  blockHash = blockHash . getHeader
  blockNo   = blockNo   . getHeader
  blockSlot = blockSlot . getHeader

instance HasHeader ByronSpecHeader where
  blockHash = byronSpecHeaderHash
  blockNo   = byronSpecHeaderNo
  blockSlot = fromByronSpecSlotNo . Spec._bhSlot . byronSpecHeader

instance GetPrevHash ByronSpecBlock where
  getPrevHash = castHash . getPrevHash . getHeader

instance GetPrevHash ByronSpecHeader where
  getPrevHash = fromByronSpecPrevHash id . Spec._bhPrevHash . byronSpecHeader

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data instance BlockConfig ByronSpecBlock = ByronSpecBlockConfig
