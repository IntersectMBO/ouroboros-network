{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Ouroboros.Consensus.HardFork.Simple (TriggerHardFork (..)) where

import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary
import           Cardano.Slotting.Slot (EpochNo)

import           Ouroboros.Consensus.Node.Serialisation

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

instance SerialiseNodeToClient blk TriggerHardFork where
  encodeNodeToClient _ _ triggerHardFork = case triggerHardFork of
    TriggerHardForkAtVersion v -> encodeListLen 2 <> encodeWord8 0 <> toCBOR v
    TriggerHardForkAtEpoch e   -> encodeListLen 2 <> encodeWord8 1 <> toCBOR e
    TriggerHardForkNever       -> encodeListLen 1 <> encodeWord8 2
  decodeNodeToClient _ _ = do
    len <- decodeListLen
    tag <- decodeWord8
    case (len, tag) of
      (2, 0)   -> TriggerHardForkAtVersion <$> fromCBOR @Word16
      (2, 1)   -> TriggerHardForkAtEpoch <$> fromCBOR @EpochNo
      (1, 2)   -> return TriggerHardForkNever
      _ -> fail $ "TriggerHardFork: invalid (len, tag): " <> show (len, tag)
