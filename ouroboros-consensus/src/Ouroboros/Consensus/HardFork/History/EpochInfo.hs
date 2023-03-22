{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Derive 'EpochInfo'
module Ouroboros.Consensus.HardFork.History.EpochInfo (
    dummyEpochInfo
  , interpreterToEpochInfo
  , summaryToEpochInfo
  , toPureEpochInfo
  ) where

import           Cardano.Slotting.EpochInfo.API
import           Control.Exception (throw)
import           Control.Monad.Except (Except, runExcept, throwError)
import           Data.Functor.Identity
import           GHC.Stack
import           Ouroboros.Consensus.HardFork.History.Qry
import           Ouroboros.Consensus.HardFork.History.Summary

{-------------------------------------------------------------------------------
  Translation to EpochInfo
-------------------------------------------------------------------------------}

-- | Construct an 'EpochInfo' for a /snapshot/ of the ledger state
summaryToEpochInfo :: forall xs. Summary xs -> EpochInfo (Except PastHorizonException)
summaryToEpochInfo = interpreterToEpochInfo . mkInterpreter

-- | Construct an 'EpochInfo' for a /snapshot/ of the ledger state
interpreterToEpochInfo :: forall xs. Interpreter xs
                       -> EpochInfo (Except PastHorizonException)
interpreterToEpochInfo i = EpochInfo {
      epochInfoSize_  = \e -> interpretQuery' (epochToSize  e)
    , epochInfoFirst_ = \e -> interpretQuery' (epochToSlot' e)
    , epochInfoEpoch_ = \s -> interpretQuery' (fst <$> slotToEpoch' s)

    , epochInfoSlotToRelativeTime_ = \s ->
        interpretQuery' (fst <$> slotToWallclock s)

    , epochInfoSlotLength_ = \s -> interpretQuery' (slotToSlotLength s)
    }
  where
    interpretQuery' :: HasCallStack => Qry a -> Except PastHorizonException a
    interpretQuery' q = either throwError pure $ interpretQuery i q

-- | A dummy 'EpochInfo' that always throws an 'error'.
--
-- To be used as a placeholder before a summary is available.
dummyEpochInfo :: EpochInfo (Except PastHorizonException)
dummyEpochInfo = EpochInfo {
      epochInfoSize_               = \_ -> error "dummyEpochInfo used"
    , epochInfoFirst_              = \_ -> error "dummyEpochInfo used"
    , epochInfoEpoch_              = \_ -> error "dummyEpochInfo used"
    , epochInfoSlotToRelativeTime_ = \_ -> error "dummyEpochInfo used"
    , epochInfoSlotLength_         = \_ -> error "dummyEpochInfo used"
    }

-- | Interpret the 'PastHorizonException' as a _pure exception_ via 'throw'
--
-- As per usual, this should only be used when the pure exception would
-- indicate a bug.
toPureEpochInfo :: EpochInfo (Except PastHorizonException) -> EpochInfo Identity
toPureEpochInfo = hoistEpochInfo (either throw pure . runExcept)
