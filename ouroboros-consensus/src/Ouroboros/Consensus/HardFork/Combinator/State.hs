{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.State (HardForkState_(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
module Ouroboros.Consensus.HardFork.Combinator.State (
    module X
    -- * Serialisation support
  , recover
    -- * EpochInfo
  , mostRecentTransitionInfo
  , reconstructSummaryLedger
  , epochInfoLedger
  , epochInfoLedgerView
    -- * Ledger specific functionality
  , extendToSlot
  ) where

import           Prelude hiding (sequence)

import           Control.Monad (guard)
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.SOP.Strict hiding (shape)

import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs,
                     Requiring (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Extend (..), ScanNext (..), Telescope)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

import           Ouroboros.Consensus.HardFork.Combinator.State.Infra as X
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances as X ()
import           Ouroboros.Consensus.HardFork.Combinator.State.Types as X

{-------------------------------------------------------------------------------
  Recovery
-------------------------------------------------------------------------------}

-- | Recover 'HardForkState_' from partial information
--
-- The primary goal of this is to make sure that for the /current/ state we
-- really only need to store the underlying @f@. It is not strictly essential
-- that this is possible but it helps with the unary hardfork case, and it may
-- in general help with binary compatibility.
recover :: forall g f xs. CanHardFork xs
        => Telescope (Past g) f xs -> HardForkState_ g f xs
recover =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ ->
          HardForkState
        . Telescope.bihmap
            (\(Pair _ past) -> past)
            recoverCurrent
        . Telescope.scanl
            (InPairs.hpure $ ScanNext $ const $ K . pastEnd)
            (K History.initBound)
  where
    recoverCurrent :: Product (K History.Bound) f blk -> Current f blk
    recoverCurrent (Pair (K prevEnd) st) = Current {
          currentStart = prevEnd
        , currentState = st
        }

{-------------------------------------------------------------------------------
  Reconstruct EpochInfo
-------------------------------------------------------------------------------}

mostRecentTransitionInfo :: CanHardFork xs
                         => HardForkLedgerConfig xs
                         -> HardForkState_ g LedgerState xs
                         -> TransitionInfo
mostRecentTransitionInfo HardForkLedgerConfig{..} st =
    hcollapse $
      hczipWith3
        proxySingle
        getTransition
        cfgs
        (History.getShape hardForkLedgerConfigShape)
        (Telescope.tip (getHardForkState st))
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    getTransition :: SingleEraBlock          blk
                  => WrapPartialLedgerConfig blk
                  -> K History.EraParams     blk
                  -> Current LedgerState     blk
                  -> K TransitionInfo        blk
    getTransition cfg (K eraParams) Current{..} = K $
        case singleEraTransition' cfg eraParams currentStart currentState of
          Nothing -> TransitionUnknown (ledgerTipSlot currentState)
          Just e  -> TransitionKnown e

reconstructSummaryLedger :: CanHardFork xs
                         => HardForkLedgerConfig xs
                         -> HardForkState_ g LedgerState xs
                         -> History.Summary xs
reconstructSummaryLedger cfg@HardForkLedgerConfig{..} st =
    reconstructSummary
      hardForkLedgerConfigShape
      (mostRecentTransitionInfo cfg st)
      st

-- | Construct 'EpochInfo' from the ledger state
--
-- NOTE: The resulting 'EpochInfo' is a snapshot only, with a limited range.
-- It should not be stored.
epochInfoLedger :: CanHardFork xs
                => HardForkLedgerConfig xs
                -> HardForkState_ g LedgerState xs
                -> EpochInfo Identity
epochInfoLedger cfg st =
    History.snapshotEpochInfo $
      reconstructSummaryLedger cfg st

-- | Construct 'EpochInfo' from a ledger view
--
-- The same comments apply as for 'epochInfoLedger', except more so: the range
-- of the 'EpochInfo' is determined by the ledger that the view was derived
-- from; when the view is a forecast, that range does not extend (obviously).
epochInfoLedgerView :: History.Shape xs
                    -> HardForkLedgerView_ f xs
                    -> EpochInfo Identity
epochInfoLedgerView shape HardForkLedgerView{..} =
    History.snapshotEpochInfo $
      reconstructSummary
        shape
        hardForkLedgerViewTransition
        hardForkLedgerViewPerEra

{-------------------------------------------------------------------------------
  Extending
-------------------------------------------------------------------------------}

-- | Extend the telescope until the specified slot is within the era at the tip
extendToSlot :: forall xs. CanHardFork xs
             => HardForkLedgerConfig xs
             -> SlotNo
             -> HardForkState LedgerState xs -> HardForkState LedgerState xs
extendToSlot ledgerCfg@HardForkLedgerConfig{..} slot ledgerSt@(HardForkState st) =
      HardForkState . unI
    . Telescope.extend
        ( InPairs.hmap (\f -> Require $ \(K t)
                           -> Extend  $ \cur
                           -> I $ howExtend f t cur)
        $ translate
        )
        (hczipWith
           proxySingle
           (fn .: whenExtend)
           pcfgs
           (History.getShape hardForkLedgerConfigShape))
    $ st
  where
    pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
    ei    = epochInfoLedger ledgerCfg ledgerSt

    -- Return the end of this era if we should transition to the next
    whenExtend :: SingleEraBlock              blk
               => WrapPartialLedgerConfig     blk
               -> K History.EraParams         blk
               -> Current LedgerState         blk
               -> (Maybe :.: K History.Bound) blk
    whenExtend pcfg (K eraParams) cur = Comp $ K <$> do
        transition <- singleEraTransition'
                        pcfg
                        eraParams
                        (currentStart cur)
                        (currentState cur)
        let endBound = History.mkUpperBound
                         eraParams
                         (currentStart cur)
                         transition
        guard (slot >= History.boundSlot endBound)
        return endBound

    howExtend :: Translate LedgerState blk blk'
              -> History.Bound
              -> Current LedgerState blk
              -> (Past LedgerState blk, Current LedgerState blk')
    howExtend f currentEnd cur = (
          Past {
              pastStart    = currentStart cur
            , pastEnd      = currentEnd
            , pastSnapshot = Snapshot 0 (currentState cur)
            }
        , Current {
              currentStart = currentEnd
            , currentState = translateWith f
                               (History.boundEpoch currentEnd)
                               (currentState cur)
            }
        )

    translate :: InPairs (Translate LedgerState) xs
    translate = InPairs.requiringBoth cfgs $
                  translateLedgerState hardForkEraTranslation
