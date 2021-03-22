{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Derive 'EpochInfo'
module Ouroboros.Consensus.HardFork.History.EpochInfo (
    dummyEpochInfo
  , snapshotEpochInfo
  , summaryToEpochInfo
  ) where

import           Data.Functor.Identity
import           GHC.Stack

import           Cardano.Slotting.EpochInfo.API

import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.History.Caching
import           Ouroboros.Consensus.HardFork.History.Qry
import           Ouroboros.Consensus.HardFork.History.Summary

{-------------------------------------------------------------------------------
  Translation to EpochInfo
-------------------------------------------------------------------------------}

-- | Construct 'EpochInfo' from a function that returns the hard fork summary
--
-- When a particular request fails with a 'PastHorizon' error, we ask for an
-- updated summary, in the hope that the ledger state has advanced. If the query
-- /still/ fails with that updated summary, the error is thrown as an exception.
summaryToEpochInfo :: forall m xs. (MonadSTM m, MonadThrow (STM m))
                   => STM m (Summary xs) -> m (EpochInfo (STM m))
summaryToEpochInfo =
    fmap go . runWithCachedSummary
  where
    go :: RunWithCachedSummary xs m -> EpochInfo (STM m)
    go run = EpochInfo {
          epochInfoSize_  = \e -> cachedRunQueryThrow run (epochToSize  e)
        , epochInfoFirst_ = \e -> cachedRunQueryThrow run (epochToSlot' e)
        , epochInfoEpoch_ = \s -> cachedRunQueryThrow run (fst <$> slotToEpoch' s)
        }

-- | Construct an 'EpochInfo' for a /snapshot/ of the ledger state
--
-- When a particular request fails with a 'PastHorizon' error, we throw the
-- error as a /pure/ exception. Such an exception would indicate a bug.
snapshotEpochInfo :: forall xs. Summary xs -> EpochInfo Identity
snapshotEpochInfo summary = EpochInfo {
      epochInfoSize_  = \e -> runQueryPure' (epochToSize  e)
    , epochInfoFirst_ = \e -> runQueryPure' (epochToSlot' e)
    , epochInfoEpoch_ = \s -> runQueryPure' (fst <$> slotToEpoch' s)
    }
  where
    runQueryPure' :: HasCallStack => Qry a -> Identity a
    runQueryPure' = Identity . flip runQueryPure summary

-- | A dummy 'EpochInfo' that always throws an 'error'.
--
-- To be used as a placeholder before a summary is available.
dummyEpochInfo :: EpochInfo Identity
dummyEpochInfo = EpochInfo {
      epochInfoSize_  = \_ -> error "dummyEpochInfo used"
    , epochInfoFirst_ = \_ -> error "dummyEpochInfo used"
    , epochInfoEpoch_ = \_ -> error "dummyEpochInfo used"
    }
