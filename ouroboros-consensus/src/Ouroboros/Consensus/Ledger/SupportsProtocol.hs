{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Ledger.SupportsProtocol (
    LedgerSupportsProtocol(..)
  , ledgerViewForecastAt
  , ledgerViewForecastAtTip
  , lemma_ledgerViewForecastAt_applyChainTick
  ) where

import           Control.Monad.Except
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Block (SlotNo)
import           Ouroboros.Network.Point hiding (at)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Assert

-- | Link protocol to ledger
class ( BlockSupportsProtocol blk
      , UpdateLedger          blk
      , ValidateEnvelope      blk
      ) => LedgerSupportsProtocol blk where
  -- | Extract ledger view from the ledger state
  protocolLedgerView :: LedgerConfig blk
                     -> LedgerState blk
                     -> LedgerView (BlockProtocol blk)

  -- | Get a (historical) forecast at the given slot
  --
  -- Returns 'Nothing' if the given slot is too far in the past.
  -- It is an 'error' to call this with a slot in the future.
  --
  -- This forecast can be used to validate headers of blocks within the range
  -- of the forecast. These blocks do not necessarily need to live on the same
  -- branch as the current ledger, but the slot at which the forecast is
  -- obtained must be within @k@ blocks from the tip.
  --
  -- The range of the forecast should allow to validate a sufficient number of
  -- headers to validate an alternative chain longer than ours, so that chain
  -- selection can decide whether or not we prefer the alternative chain to our
  -- current chain. In addition, it would be helpful, though not essential, if
  -- we can look further ahead than that, as this would improve sync
  -- performance.
  --
  -- NOTE (difference between 'ledgerViewForecastAt' and 'applyChainTick'):
  -- Both 'ledgerViewForecastAt' and 'applyChainTick' can be used to obtain
  -- a protocol ledger view for a future slot. The difference between the two
  -- is that 'applyChainTick' assumes no blocks are present between the current
  -- ledger tip and the specified 'SlotNo', whereas 'ledgerViewForecastAt'
  -- cannot make such an assumption. Thus, 'applyChainTick' cannot fail, whereas
  -- the forecast returned by 'ledgerViewForecastAt' might report an
  -- 'OutsideForecastRange' for the same 'SlotNo'. We expect the two functions
  -- to produce the same view whenever the 'SlotNo' /is/ in range, however;
  -- see 'lemma_ledgerViewForecastAt_applyChainTick'.
  ledgerViewForecastAt_ :: HasCallStack
                        => LedgerConfig blk
                        -> LedgerState blk
                        -> WithOrigin SlotNo
                        -> Maybe (Forecast (LedgerView (BlockProtocol blk)))

-- | Wrapper around 'forecastAt' that checks 'lemma_forecast_chaintic'.
ledgerViewForecastAt :: forall blk. (LedgerSupportsProtocol blk, HasCallStack)
                     => LedgerConfig blk
                     -> LedgerState blk
                     -> WithOrigin SlotNo
                     -> Maybe (Forecast (LedgerView (BlockProtocol blk)))
ledgerViewForecastAt cfg st at =
    checkLemma <$> ledgerViewForecastAt_ cfg st at
  where
    checkLemma :: Forecast (LedgerView (BlockProtocol blk))
               -> Forecast (LedgerView (BlockProtocol blk))
    checkLemma forecast = forecast { forecastFor = \for ->
        assertWithMsg
          (lemma_ledgerViewForecastAt_applyChainTick cfg st forecast for)
          (forecastFor forecast for)
      }

-- | Get a 'Forecast' from the tip of the ledger
--
-- This is currently only used in block production, and as an indirect way to
-- avoid block production when the wall clock is too far ahead of the ledger.
--
-- TODO: If/when we remove that check, we can probably remove this function.
-- <https://github.com/input-output-hk/ouroboros-network/issues/1941>
ledgerViewForecastAtTip :: LedgerSupportsProtocol blk
                        => LedgerConfig blk
                        -> LedgerState blk
                        -> Forecast (LedgerView (BlockProtocol blk))
ledgerViewForecastAtTip cfg st =
    case ledgerViewForecastAt cfg st (ledgerTipSlot st) of
      Just forecast -> forecast
      Nothing       -> error "ledgerViewForecastAtTip: impossible"

-- | Relation between 'ledgerViewForecastAt' and 'applyChainTick'
--
-- For all slots @s@ such that @At s >= ledgerTip st@, if
--
-- >    predictionFor (ledgerViewForecastAt cfg st at) s
-- > == Right view
--
-- then
--
-- >    protocolLedgerView cfg (tickedLedgerState (applyChainTick cfg s st))
-- > == view
--
-- This should be true for each ledger because consensus depends on it.
lemma_ledgerViewForecastAt_applyChainTick
  :: LedgerSupportsProtocol blk
  => LedgerConfig blk
  -> LedgerState blk
  -> Forecast (LedgerView (BlockProtocol blk))
  -> SlotNo
  -> Either String ()
lemma_ledgerViewForecastAt_applyChainTick cfg st forecast for
    | At for >= ledgerTipSlot st
    , let lhs = forecastFor forecast for
          rhs = protocolLedgerView cfg
              . tickedLedgerState
              . applyChainTick cfg for
              $ st
    , Right lhs' <- runExcept lhs
    , lhs' /= rhs
    = Left $ unlines
      [ "ledgerViewForecastAt /= protocolLedgerView . applyChainTick:"
      , show lhs'
      , " /= "
      , show rhs
      ]
    | otherwise
    = Right ()
