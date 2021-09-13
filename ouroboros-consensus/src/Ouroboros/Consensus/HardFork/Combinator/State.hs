{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.State (HardForkState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
module Ouroboros.Consensus.HardFork.Combinator.State (
    module X
    -- * Support for defining instances
  , getTip
    -- * Serialisation support
  , recover
    -- * EpochInfo
  , epochInfoLedger
  , epochInfoPrecomputedTransitionInfo
  , mostRecentTransitionInfo
  , reconstructSummaryLedger
    -- * Ledger specific functionality
  , extendToSlot
  ) where

import           Prelude hiding (sequence)

import           Control.Monad (guard)
import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP.Strict hiding (shape)

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract hiding (getTip)
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Counting (getExactly)

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
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
  GetTip
-------------------------------------------------------------------------------}

getTip :: forall f xs. CanHardFork xs
       => (forall blk. SingleEraBlock blk => f blk -> Point blk)
       -> HardForkState f xs -> Point (HardForkBlock xs)
getTip getLedgerTip =
      hcollapse
    . hcmap proxySingle (K . injPoint . getLedgerTip)
    . tip
  where
    injPoint :: forall blk. SingleEraBlock blk
             => Point blk -> Point (HardForkBlock xs)
    injPoint GenesisPoint     = GenesisPoint
    injPoint (BlockPoint s h) = BlockPoint s $ OneEraHash $
                                  toShortRawHash (Proxy @blk) h

{-------------------------------------------------------------------------------
  Recovery
-------------------------------------------------------------------------------}

-- | Recover 'HardForkState' from partial information
--
-- The primary goal of this is to make sure that for the /current/ state we
-- really only need to store the underlying @f@. It is not strictly essential
-- that this is possible but it helps with the unary hardfork case, and it may
-- in general help with binary compatibility.
recover :: forall f xs. CanHardFork xs
        => Telescope (K Past) f xs -> HardForkState f xs
recover =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty {} ->
          HardForkState
        . Telescope.bihmap
            (\(Pair _ past) -> past)
            recoverCurrent
        . Telescope.scanl
            (InPairs.hpure $ ScanNext $ const $ K . pastEnd . unK)
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

mostRecentTransitionInfo :: All SingleEraBlock xs
                         => HardForkLedgerConfig xs
                         -> HardForkState LedgerState xs
                         -> TransitionInfo
mostRecentTransitionInfo HardForkLedgerConfig{..} st =
    hcollapse $
      hczipWith3
        proxySingle
        getTransition
        cfgs
        (getExactly (History.getShape hardForkLedgerConfigShape))
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

reconstructSummaryLedger :: All SingleEraBlock xs
                         => HardForkLedgerConfig xs
                         -> HardForkState LedgerState xs
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
epochInfoLedger :: All SingleEraBlock xs
                => HardForkLedgerConfig xs
                -> HardForkState LedgerState xs
                -> EpochInfo (Except PastHorizonException)
epochInfoLedger cfg st =
    History.summaryToEpochInfo $
      reconstructSummaryLedger cfg st

-- | Construct 'EpochInfo' given precomputed 'TransitionInfo'
epochInfoPrecomputedTransitionInfo ::
     History.Shape xs
  -> TransitionInfo
  -> HardForkState f xs
  -> EpochInfo (Except PastHorizonException)
epochInfoPrecomputedTransitionInfo shape transition st =
    History.summaryToEpochInfo $
      reconstructSummary shape transition st

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
           (getExactly (History.getShape hardForkLedgerConfigShape)))
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
              -> (K Past blk, Current LedgerState blk')
    howExtend f currentEnd cur = (
          K Past {
              pastStart    = currentStart cur
            , pastEnd      = currentEnd
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
