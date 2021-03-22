{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Ledger.SupportsProtocol (
    LedgerSupportsProtocol (..)
  ) where

import           Control.Monad.Except
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

-- | Link protocol to ledger
class ( BlockSupportsProtocol blk
      , UpdateLedger          blk
      , ValidateEnvelope      blk
      ) => LedgerSupportsProtocol blk where
  -- | Extract ticked ledger view from ticked ledger state
  --
  -- See 'ledgerViewForecastAt' for a discussion and precise definition of the
  -- relation between this and forecasting.
  protocolLedgerView :: LedgerConfig blk
                     -> Ticked (LedgerState blk)
                     -> Ticked (LedgerView (BlockProtocol blk))

  -- | Get a forecast at the given ledger state.
  --
  -- This forecast can be used to validate headers of blocks within the range of
  -- the forecast. These blocks need to live on a chain that fits on the last
  -- applied block of the given ledger.
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
  -- to produce the same view whenever the 'SlotNo' /is/ in range, however.
  -- More precisely:
  --
  -- If
  --
  -- >    forecastFor (ledgerViewForecastAt cfg st) for
  -- > == Just view
  --
  -- then
  --
  -- >    protocolLedgerView cfg (applyChainTick cfg for st)
  -- > == view
  --
  -- See 'lemma_ledgerViewForecastAt_applyChainTick'.
  ledgerViewForecastAt ::
       HasCallStack
    => LedgerConfig blk
    -> LedgerState blk
    -> Forecast (LedgerView (BlockProtocol blk))

-- | Relation between 'ledgerViewForecastAt' and 'applyChainTick'
_lemma_ledgerViewForecastAt_applyChainTick
  :: ( LedgerSupportsProtocol blk
     , Eq   (Ticked (LedgerView (BlockProtocol blk)))
     , Show (Ticked (LedgerView (BlockProtocol blk)))
     )
  => LedgerConfig blk
  -> LedgerState blk
  -> Forecast (LedgerView (BlockProtocol blk))
  -> SlotNo
  -> Either String ()
_lemma_ledgerViewForecastAt_applyChainTick cfg st forecast for
    | NotOrigin for >= ledgerTipSlot st
    , let lhs = forecastFor forecast for
          rhs = protocolLedgerView cfg
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
