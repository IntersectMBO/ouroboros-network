{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Ledger.Shelley.Block where

import           BlockChain (BHBody (..), BHeader (..), Block (..), HashHeader,
                     bhHash, bhbody, bheader)
import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.Coerce (coerce)
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy (Proxy (..))
import           Data.Typeable (typeRep)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Block

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ShelleyHash = ShelleyHash { unShelleyHash :: HashHeader TPraosStandardCrypto }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR)
  deriving anyclass NoUnexpectedThunks

instance Condense ShelleyHash where
  condense = show . unShelleyHash

{-------------------------------------------------------------------------------
  Shelley blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain.
newtype ShelleyBlock = ShelleyBlock
  { unShelleyBlock :: Block TPraosStandardCrypto
  }
  deriving (Eq, Show)

instance GetHeader ShelleyBlock where
  data Header ShelleyBlock = ShelleyHeader
    { shelleyHeader :: !(BHeader TPraosStandardCrypto)
      -- Cached hash
    , shelleyHeaderHash :: ShelleyHash
    } deriving (Eq, Show)

  getHeader (ShelleyBlock b) = ShelleyHeader
    { shelleyHeader     = bheader b
    , shelleyHeaderHash = ShelleyHash . bhHash . bheader $ b
    }

instance NoUnexpectedThunks (Header ShelleyBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ShelleyBlock ))

-- We explicitly allow the hash to be a thunk
  whnfNoUnexpectedThunks ctxt (ShelleyHeader hdr _hash) =
    noUnexpectedThunks ctxt hdr

type instance HeaderHash ShelleyBlock = ShelleyHash

instance HasHeader ShelleyBlock  where
  blockHash      = blockHash . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      = blockSlot . getHeader
  blockNo        = blockNo . getHeader
  blockInvariant = const True

instance HasHeader (Header ShelleyBlock) where
  blockHash = shelleyHeaderHash

  blockPrevHash =
    BlockHash . ShelleyHash . bheaderPrev . bhbody . shelleyHeader

  blockSlot      = bheaderSlotNo . bhbody . shelleyHeader
  blockNo        = coerce . bheaderBlockNo . bhbody . shelleyHeader
  blockInvariant = const True

instance Measured BlockMeasure ShelleyBlock where
  measure = blockMeasure

instance StandardHash ShelleyBlock
