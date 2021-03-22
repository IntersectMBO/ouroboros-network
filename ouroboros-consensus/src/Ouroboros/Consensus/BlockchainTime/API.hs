{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime.API (
    BlockchainTime (..)
  , CurrentSlot (..)
  , knownSlotWatcher
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (OnlyCheckWhnfNamed (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Watcher (..))

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Blockchain time
--
-- When we run the blockchain, there is a single, global time. We abstract over
-- this here to allow to query this time (in terms of the current slot), and
-- execute an action each time we advance a slot.
data BlockchainTime m = BlockchainTime {
      -- | Get current slot
      getCurrentSlot :: STM m CurrentSlot
    }
  deriving NoThunks
       via OnlyCheckWhnfNamed "BlockchainTime" (BlockchainTime m)

data CurrentSlot =
    -- | The current slot is known
    CurrentSlot !SlotNo

    -- | The current slot is not yet known
    --
    -- This only happens when the tip of the ledger is so far behind that we
    -- lack the information necessary to translate the current 'UTCTime' into a
    -- 'SlotNo'. This should only be the case during syncing.
  | CurrentSlotUnknown
  deriving stock    (Generic, Show)
  deriving anyclass (NoThunks)

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | Watches for changes in the current slot
--
-- The action will not be called until the current slot becomes known
-- (if the tip of our ledger is too far away from the current wallclock time,
-- we may not know what the current 'SlotNo' is).
knownSlotWatcher :: forall m. IOLike m
                 => BlockchainTime m
                 -> (SlotNo -> m ())  -- ^ Action to execute
                 -> Watcher m SlotNo SlotNo
knownSlotWatcher btime notify =
    Watcher {
        wFingerprint = id
      , wInitial     = Nothing
      , wNotify      = notify
      , wReader      = getCurrentSlot'
      }
  where
    getCurrentSlot' :: STM m SlotNo
    getCurrentSlot' = do
        mSlot <- getCurrentSlot btime
        case mSlot of
          CurrentSlotUnknown -> retry
          CurrentSlot s      -> return s
