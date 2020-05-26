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
    module Ouroboros.Consensus.HardFork.Combinator.State.Infra
    -- * Serialisation support
  , recover
    -- * EpochInfo
  , reconstructSummaryLedger
  , epochInfoLedger
  , epochInfoLedgerView
    -- * Ledger specific functionality
  , extendToSlot
  , transitions
  ) where

import           Prelude hiding (sequence)

import           Control.Monad (guard)
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.SOP.Strict hiding (shape)

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import           Ouroboros.Consensus.HardFork.Combinator.State.Infra
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs,
                     Requiring (..), RequiringBoth (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Extend (..), ScanNext (..), Telescope)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

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
        => SystemStart
        -> Telescope (Past g) f xs
        -> HardForkState_ g f xs
recover systemStart =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ ->
          HardForkState
        . Telescope.bihmap
            (\(Pair _ past) -> past)
            recoverCurrent
        . Telescope.scanl
            (InPairs.hpure $ ScanNext $ const $ K . pastEnd)
            (K (History.initBound systemStart))
  where
    recoverCurrent :: Product (K History.Bound) f blk -> Current f blk
    recoverCurrent (Pair (K prevEnd) st) = Current {
          currentStart = prevEnd
        , currentState = st
        }

{-------------------------------------------------------------------------------
  Reconstruct EpochInfo
-------------------------------------------------------------------------------}

reconstructSummaryLedger :: CanHardFork xs
                         => HardForkLedgerConfig xs
                         -> HardForkState LedgerState xs
                         -> History.Summary xs
reconstructSummaryLedger HardForkLedgerConfig{..} =
    reconstructSummary
        hardForkLedgerConfigShape
        (hcmap proxySingle (fn . (K .: transitionOrTip)) cfgs)
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

-- | Construct 'EpochInfo' from the ledger state
--
-- NOTE: The resulting 'EpochInfo' is a snapshot only, with a limited range.
-- It should not be stored.
epochInfoLedger :: CanHardFork xs
                => HardForkLedgerConfig xs
                -> HardForkState LedgerState xs
                -> EpochInfo Identity
epochInfoLedger cfg =
      History.snapshotEpochInfo
    . reconstructSummaryLedger cfg

-- | Construct 'EpochInfo' from a ledger view
--
-- The same comments apply as for 'epochInfoLedger', except more so: the range
-- of the 'EpochInfo' is determined by the ledger that the view was derived
-- from; when the view is a forecast, that range does not extend (obviously).
epochInfoLedgerView :: CanHardFork xs
                    => History.Shape xs
                    -> HardForkLedgerView xs
                    -> EpochInfo Identity
epochInfoLedgerView shape =
      History.snapshotEpochInfo
    . reconstructSummary
        shape
        (hpure (fn $ K . hardForkEraTransition))

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
        ( InPairs.requiringBoth cfgs
        $ InPairs.hcmap proxySingle (\f -> RequireBoth $ \cfg cfg'
                                        -> Require $ \(K t)
                                        -> Extend $ \cur
                                        -> I $ howExtend f cfg cfg' t cur)
        $ translate
        )
        (hcmap proxySingle (fn . whenExtend) cfgs)
    $ st
  where
    -- Return the end of this era if we should transition to the next
    whenExtend :: SingleEraBlock              blk
               => WrapPartialLedgerConfig     blk
               -> Current LedgerState         blk
               -> (Maybe :.: K History.Bound) blk
    whenExtend pcfg cur = Comp $ K <$> do
        transition <- singleEraTransition' pcfg (currentState cur)
        let endBound = History.mkUpperBound
                         (singleEraParams' pcfg)
                         (currentStart cur)
                         transition
        guard (slot >= History.boundSlot endBound)
        return endBound

    howExtend :: (SingleEraBlock blk, SingleEraBlock blk')
              => TranslateEraLedgerState blk blk'
              -> WrapPartialLedgerConfig blk
              -> WrapPartialLedgerConfig blk'
              -> History.Bound
              -> Current LedgerState blk
              -> (Past LedgerState blk, Current LedgerState blk')
    howExtend f pcfg pcfg' currentEnd cur = (
          Past {
              pastStart    = currentStart cur
            , pastEnd      = currentEnd
            , pastSnapshot = Snapshot 0 (currentState cur)
            }
        , Current {
              currentStart = currentEnd
            , currentState = translateLedgerStateWith f
                               (completeLedgerConfig' ei pcfg )
                               (completeLedgerConfig' ei pcfg')
                               (History.boundEpoch currentEnd)
                               (currentState cur)
            }
        )

    cfgs :: NP WrapPartialLedgerConfig xs
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    ei :: EpochInfo Identity
    ei = epochInfoLedger ledgerCfg ledgerSt

    translate :: InPairs TranslateEraLedgerState xs
    translate = translateLedgerState hardForkEraTranslation

{-------------------------------------------------------------------------------
  Collect all current hard fork transitions

  TODO: If we make 'hardForkSummary' the primitive function in
  'HasHardForkHistory', ideally this should not be necessary anymore: the
  summary is trivially derivable from the ledger state. This would then
  also obsolete the need for caching.
-------------------------------------------------------------------------------}

transitions :: forall g xs. CanHardFork xs
            => HardForkLedgerConfig xs
            -> HardForkState_ g LedgerState xs -> History.Transitions xs
transitions HardForkLedgerConfig{..} (HardForkState st) =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty _ ->
        History.Transitions $
          shiftTransitions (getPerEraLedgerConfig cfg) $
            allTransitions (getPerEraLedgerConfig cfg) st
  where
    cfg = hardForkLedgerConfigPerEra

-- | Find transition points in all eras
--
-- This associates each transition with the era it transitions /from/.
-- See also 'shiftTransitions'.
allTransitions :: CanHardFork                                xs
               => NP WrapPartialLedgerConfig                 xs
               -> Telescope (Past f) (Current (LedgerState)) xs
               -> AtMost                                     xs EpochNo
allTransitions cfgs st =
    Telescope.toAtMost $
      Telescope.bihap
        (hpure (fn past))
        (hcmap proxySingle (fn . cur) cfgs)
        st
  where
    past :: Past f blk -> K EpochNo blk
    past = K . History.boundEpoch . pastEnd

    cur :: SingleEraBlock          blk
        => WrapPartialLedgerConfig blk
        -> Current LedgerState     blk
        -> K (Maybe EpochNo)       blk
    cur cfg = K . singleEraTransition' cfg . currentState

-- | Associate transitions with the era they transition /to/
--
-- 'allTransitions' associates transitions with the era in which they occur,
-- but the hard fork history infrastructure expects them to be associated with
-- the era that they transition /to/. 'shiftTransitions' implements this
-- shift of perspective, and also verifies that the final era cannot have
-- a transition.
shiftTransitions :: NP f (x ': xs) -- Just as an index
                 -> AtMost (x ': xs) EpochNo -> AtMost xs EpochNo
shiftTransitions = go
  where
    go :: NP f (x ': xs)
       -> AtMost (x ': xs) EpochNo -> AtMost xs EpochNo
    go _                  AtMostNil                = AtMostNil
    go (_ :* cs@(_ :* _)) (AtMostCons t ts)        = AtMostCons t (go cs ts)
    go (_ :* Nil)         (AtMostCons _ AtMostNil) = error invalidTransition

    invalidTransition :: String
    invalidTransition = "Unexpected transition in final era"
