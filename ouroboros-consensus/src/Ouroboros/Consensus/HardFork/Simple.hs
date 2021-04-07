{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.HardFork.Simple
  ( TriggerHardFork (..)
  ) where

import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary
import           Cardano.Slotting.Slot (EpochNo)

-- | The trigger condition that will cause the hard fork transition.
data TriggerHardFork =
    -- | Trigger the transition when the on-chain protocol major version (from
    -- the ledger state) reaches this number.
    TriggerHardForkAtVersion !Word16
    -- | For testing only, trigger the transition at a specific hard-coded
    -- epoch, irrespective of the ledger state.
  | TriggerHardForkAtEpoch !EpochNo
    -- | Never trigger a hard fork
  | TriggerHardForkNever
  deriving (Show, Generic, NoThunks)

instance ToCBOR TriggerHardFork where
  toCBOR triggerHardFork = case triggerHardFork of
    TriggerHardForkAtVersion v -> encodeTag 0 <> toCBOR v
    TriggerHardForkAtEpoch e   -> encodeTag 1 <> toCBOR e
    TriggerHardForkNever       -> encodeTag 2

instance FromCBOR TriggerHardFork where
  fromCBOR = do
    decodeTag >>= \case
      0   -> TriggerHardForkAtVersion <$> fromCBOR @Word16
      1   -> TriggerHardForkAtEpoch <$> fromCBOR @EpochNo
      2   -> return TriggerHardForkNever
      tag -> fail $ "TriggerHardFork: unknown tag " ++ show tag
