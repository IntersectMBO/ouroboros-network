{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Various things common to iterations of the Praos protocol.
module Ouroboros.Consensus.Protocol.Praos.Common (
    MaxMajorProtVer (..)
  , PraosCanBeLeader (..)
  , PraosChainSelectView (..)
  ) where

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.Crypto (Crypto, VRF)
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Protocol.TPraos.OCert as OCert
import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo)
import           Data.Function (on)
import           Data.Ord (Down (Down))
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)

-- | The maximum major protocol version.
--
-- Must be at least the current major protocol version. For Cardano mainnet, the
-- Shelley era has major protocol verison __2__.
newtype MaxMajorProtVer = MaxMajorProtVer
  { getMaxMajorProtVer :: Natural
  }
  deriving (Eq, Show, Generic)
  deriving newtype NoThunks

-- | View of the ledger tip for chain selection.
--
-- We order between chains as follows:
--
-- 1. By chain length, with longer chains always preferred.
-- 2. If the tip of each chain was issued by the same agent, then we prefer
--    the chain whose tip has the highest ocert issue number.
-- 3. By the leader value of the chain tip, with lower values preferred.
data PraosChainSelectView c = PraosChainSelectView
  { csvChainLength :: BlockNo,
    csvSlotNo      :: SlotNo,
    csvIssuer      :: SL.VKey 'SL.BlockIssuer c,
    csvIssueNo     :: Word64,
    csvLeaderVRF   :: VRF.OutputVRF (VRF c)
  }
  deriving (Show, Eq, Generic, NoThunks)

instance Crypto c => Ord (PraosChainSelectView c) where
  compare =
    mconcat
      [ compare `on` csvChainLength,
        whenSame csvIssuer (compare `on` csvIssueNo),
        compare `on` Down . csvLeaderVRF
      ]
    where
      -- When the @a@s are equal, use the given comparison function,
      -- otherwise, no preference.
      whenSame ::
        Eq a =>
        (view -> a) ->
        (view -> view -> Ordering) ->
        (view -> view -> Ordering)
      whenSame f comp v1 v2
        | f v1 == f v2 =
            comp v1 v2
        | otherwise =
            EQ
