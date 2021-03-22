{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Placed here to separate them from the other orphan instances due to a
-- conflict with other instances in cardano-ledger-specs.
module Test.Util.Orphans.Slotting.Arbitrary (

  ) where

import           Data.Word
import           Test.QuickCheck

import           Cardano.Slotting.Slot

deriving via Word64 instance Arbitrary SlotNo
deriving via Word64 instance Arbitrary EpochNo
