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

import           Ouroboros.Network.Block

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

type ByronSpecHeader = Header ByronSpecBlock

{-------------------------------------------------------------------------------
  HasHeader
-------------------------------------------------------------------------------}

type instance HeaderHash ByronSpecBlock = Spec.Hash
instance StandardHash ByronSpecBlock

instance Measured BlockMeasure ByronSpecBlock where
  measure = blockMeasure

instance HasHeader ByronSpecBlock where
  blockHash      =             blockHash     . getHeader
  blockNo        =             blockNo       . getHeader
  blockPrevHash  = castHash  . blockPrevHash . getHeader
  blockSlot      =             blockSlot     . getHeader

  blockInvariant = const True

instance HasHeader ByronSpecHeader where
  blockHash      = byronSpecHeaderHash
  blockNo        = byronSpecHeaderNo
  blockPrevHash  = fromByronSpecPrevHash id . Spec._bhPrevHash . byronSpecHeader
  blockSlot      = fromByronSpecSlotNo      . Spec._bhSlot     . byronSpecHeader

  blockInvariant = const True

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data instance BlockConfig ByronSpecBlock = ByronSpecBlockConfig
