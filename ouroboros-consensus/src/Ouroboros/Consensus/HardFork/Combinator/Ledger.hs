{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger (
    HardForkLedgerError(..)
  , HardForkEnvelopeErr(..)
    -- * Low-level API (exported for the benefit of testing)
  , AnnForecast(..)
  , mkHardForkForecast
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.SOP.Strict hiding (shape)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams)
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
                     (HardForkLedgerView_ (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope (..))

{-------------------------------------------------------------------------------
  IsLedger
-------------------------------------------------------------------------------}

data HardForkLedgerError xs =
    -- | Validation error from one of the eras
    HardForkLedgerErrorFromEra (OneEraLedgerError xs)

    -- | We tried to apply a block from the wrong era
  | HardForkLedgerErrorWrongEra (MismatchEraInfo xs)
  deriving (Generic, Show, Eq, NoUnexpectedThunks)

instance CanHardFork xs => IsLedger (LedgerState (HardForkBlock xs)) where
  type LedgerErr (LedgerState (HardForkBlock xs)) = HardForkLedgerError  xs

  applyChainTick cfg@HardForkLedgerConfig{..} slot (HardForkLedgerState st) =
        fmap HardForkLedgerState
      . State.sequence
      . hczipWith proxySingle (tickOne ei slot) cfgs
      . State.extendToSlot cfg slot
      $ st
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoLedger cfg st

tickOne :: forall blk. SingleEraBlock blk
        => EpochInfo Identity
        -> SlotNo
        -> WrapPartialLedgerConfig  blk
        -> LedgerState              blk
        -> (Ticked :.: LedgerState) blk
tickOne ei slot pcfg st = Comp $
    applyChainTick (completeLedgerConfig' ei pcfg) slot st

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance CanHardFork xs
      => ApplyBlock (LedgerState (HardForkBlock xs)) (HardForkBlock xs) where

  applyLedgerBlock ledgerCfg@HardForkLedgerConfig{..}
                   (HardForkBlock (OneEraBlock block))
                   (Ticked slot (HardForkLedgerState st)) =
      case State.match block (hmap (Comp . Ticked slot) st) of
        Left mismatch ->
          -- Block from the wrong era (note that 'applyChainTick' will already
          -- have initiated the transition to the next era if appropriate).
          throwError $ HardForkLedgerErrorWrongEra . MismatchEraInfo $
                         Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
        Right matched ->
          fmap (HardForkLedgerState . State.tickAllPast k) $ hsequence' $
            hczipWith3 proxySingle (apply ei) cfgs injections matched
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      k    = hardForkLedgerConfigK
      ei   = State.epochInfoLedger ledgerCfg st

  reapplyLedgerBlock ledgerCfg@HardForkLedgerConfig{..}
                     (HardForkBlock (OneEraBlock block))
                     (Ticked slot (HardForkLedgerState st)) =
      case State.match block (hmap (Comp . Ticked slot) st) of
        Left _mismatch ->
          -- We already applied this block to this ledger state,
          -- so it can't be from the wrong era
          error "reapplyLedgerBlock: can't be from other era"
        Right matched ->
          HardForkLedgerState . State.tickAllPast k $
            hczipWith proxySingle (reapply ei) cfgs matched
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      k    = hardForkLedgerConfigK
      ei   = State.epochInfoLedger ledgerCfg st

  ledgerTipPoint =
        hcollapse
      . hcmap proxySingle (K . getOne)
      . State.tip
      . getHardForkLedgerState
    where
      getOne :: forall blk. SingleEraBlock blk
             => LedgerState blk -> Point (HardForkBlock xs)
      getOne = injPoint . ledgerTipPoint' (Proxy @blk)

      injPoint :: forall blk. SingleEraBlock blk
               => Point blk -> Point (HardForkBlock xs)
      injPoint GenesisPoint     = GenesisPoint
      injPoint (BlockPoint s h) = BlockPoint s $ OneEraHash $
                                    toRawHash (Proxy @blk) h

apply :: SingleEraBlock blk
      => EpochInfo Identity
      -> WrapPartialLedgerConfig                           blk
      -> Injection WrapLedgerErr xs                        blk
      -> Product I (Ticked :.: LedgerState)                blk
      -> (Except (HardForkLedgerError xs) :.: LedgerState) blk
apply ei cfg injectErr (Pair (I block) (Comp st)) = Comp $
    withExcept (injectLedgerError injectErr) $
      applyLedgerBlock (completeLedgerConfig' ei cfg) block st

reapply :: SingleEraBlock blk
        => EpochInfo Identity
        -> WrapPartialLedgerConfig            blk
        -> Product I (Ticked :.: LedgerState) blk
        -> LedgerState                        blk
reapply ei cfg (Pair (I block) (Comp st)) =
    reapplyLedgerBlock (completeLedgerConfig' ei cfg) block st

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

instance CanHardFork xs => UpdateLedger (HardForkBlock xs)

{-------------------------------------------------------------------------------
  HasHardForkHistory
-------------------------------------------------------------------------------}

instance CanHardFork xs => HasHardForkHistory (HardForkBlock xs) where
  type HardForkIndices (HardForkBlock xs) = xs

  hardForkSummary cfg = State.reconstructSummaryLedger cfg
                      . getHardForkLedgerState

{-------------------------------------------------------------------------------
  HeaderValidation
-------------------------------------------------------------------------------}

data HardForkEnvelopeErr xs =
    -- | Validation error from one of the eras
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkEnvelopeErrWrongEra (MismatchEraInfo xs)
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance CanHardFork xs => ValidateEnvelope (HardForkBlock xs) where
  type OtherHeaderEnvelopeError (HardForkBlock xs) = HardForkEnvelopeErr xs

  additionalEnvelopeChecks tlc
                           (Ticked slot hardForkView) =
                          \(HardForkHeader (OneEraHeader hdr)) ->
      case Match.matchNS
             hdr
             (hmap (Comp . Ticked slot)
                   (State.tip (hardForkLedgerViewPerEra hardForkView))) of
        Left mismatch ->
          throwError $
            HardForkEnvelopeErrWrongEra . MismatchEraInfo $
              Match.bihcmap proxySingle singleEraInfo ledgerViewInfo mismatch
        Right matched ->
          hcollapse $ hczipWith3 proxySingle aux cfgs injections matched
    where
      ei :: EpochInfo Identity
      ei = State.epochInfoLedgerView
             (hardForkLedgerConfigShape $ configLedger tlc)
             hardForkView

      cfgs :: NP TopLevelConfig xs
      cfgs = distribTopLevelConfig ei tlc

      aux :: forall blk. SingleEraBlock blk
          => TopLevelConfig blk
          -> Injection WrapEnvelopeErr xs blk
          -> Product Header (Ticked :.: WrapLedgerView) blk
          -> K (Except (HardForkEnvelopeErr xs) ()) blk
      aux cfg injErr (Pair hdr (Comp view)) = K $
          withExcept injErr' $
            additionalEnvelopeChecks
              cfg
              (unwrapLedgerView <$> view)
              hdr
        where
          injErr' :: OtherHeaderEnvelopeError blk -> HardForkEnvelopeErr xs
          injErr' = HardForkEnvelopeErrFromEra
                  . OneEraEnvelopeErr
                  . unK . apFn injErr
                  . WrapEnvelopeErr

{-------------------------------------------------------------------------------
  LedgerSupportsProtocol
-------------------------------------------------------------------------------}

instance CanHardFork xs => LedgerSupportsProtocol (HardForkBlock xs) where
  protocolLedgerView ledgerCfg@HardForkLedgerConfig{..} (HardForkLedgerState st) =
      HardForkLedgerView {
          hardForkLedgerViewPerEra =
            State.dropAllPast $ hczipWith proxySingle viewOne cfgs st
        , hardForkLedgerViewTransition =
            State.mostRecentTransitionInfo ledgerCfg st
        }
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoLedger ledgerCfg st

      viewOne :: forall blk. SingleEraBlock blk
              => WrapPartialLedgerConfig blk
              -> LedgerState blk
              -> WrapLedgerView blk
      viewOne (WrapPartialLedgerConfig cfg) =
            WrapLedgerView
          . protocolLedgerView (completeLedgerConfig (Proxy @blk) ei cfg)

  ledgerViewForecastAt ledgerCfg@HardForkLedgerConfig{..} (HardForkLedgerState st) p = do
      st'      <- State.retractToSlot p st
      forecast <- hsequence' $
                    hczipWith3
                      proxySingle
                      forecastOne
                      pcfgs
                      (History.getShape hardForkLedgerConfigShape)
                      (getHardForkState st')
      return $ mkHardForkForecast
                 (InPairs.requiringBoth cfgs $
                    translateLedgerView hardForkEraTranslation)
                 forecast
    where
      ei    = State.epochInfoLedger ledgerCfg st
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs

      -- Forecast of a single era, as well as the end of that era (if any)
      --
      -- See comment of 'hardForkEraTransition' for justification of the
      -- use of @st'@ to determine the transition/tip.
      forecastOne :: forall blk. SingleEraBlock                       blk
                  => WrapPartialLedgerConfig                          blk
                  -> K EraParams                                      blk
                  -> Current LedgerState                              blk
                  -> (Maybe :.: Current (AnnForecast WrapLedgerView)) blk
      forecastOne pcfg (K eraParams) Current{..} = Comp $
          ann <$> ledgerViewForecastAt cfg currentState p
        where
          cfg :: LedgerConfig blk
          cfg = completeLedgerConfig' ei pcfg

          ann :: Forecast (LedgerView (BlockProtocol blk))
              -> Current (AnnForecast WrapLedgerView) blk
          ann forecast = Current {
                currentStart = currentStart
              , currentState = AnnForecast {
                    annForecast          = WrapLedgerView <$> forecast
                  , annForecastEraParams = eraParams
                  , annForecastNext      = singleEraTransition'
                                             pcfg
                                             eraParams
                                             currentStart
                                             currentState
                  }
              }

{-------------------------------------------------------------------------------
  Auxiliary: constructing the forecast
-------------------------------------------------------------------------------}

-- | Annotated forecast
data AnnForecast f blk = AnnForecast {
      -- | The 'Forecast' proper
      annForecast          :: Forecast (f blk)

      -- | The transition to the next era
      --
      -- where " next " is relative to the era used to construct the forecast
    , annForecastNext      :: Maybe EpochNo

      -- | The era parameters of the era in which the forecast was constructed
    , annForecastEraParams :: EraParams
    }

-- | Change a telescope of a forecast into a forecast of a telescope
mkHardForkForecast :: InPairs (Translate f) xs
                   -> Telescope (Past g) (Current (AnnForecast f)) xs
                   -> Forecast (HardForkLedgerView_ f xs)
mkHardForkForecast =
    go
  where
    go :: InPairs (Translate f) xs
       -> Telescope (Past g) (Current (AnnForecast f)) xs
       -> Forecast (HardForkLedgerView_ f xs)
    go PNil         (TZ f)      = forecastFinalEra f
    go (PCons g _)  (TZ f)      = forecastNotFinal g f
    go (PCons _ gs) (TS past f) = shiftView past <$> go gs f
    go PNil         (TS _ f)    = case f of {}

-- | Construct forecast when we're in the final era.
--
-- Since we're in the final era, no translation is required.
forecastFinalEra :: Current (AnnForecast f) blk
                 -> Forecast (HardForkLedgerView_ f '[blk])
forecastFinalEra (Current start AnnForecast{..}) =
    Forecast (forecastAt annForecast) $ \slot ->
      aux <$> forecastFor annForecast slot
  where
    aux :: f blk -> HardForkLedgerView_ f '[blk]
    aux era = HardForkLedgerView {
          hardForkLedgerViewPerEra     = HardForkState . TZ $ Current start era
        , hardForkLedgerViewTransition = TransitionImpossible
        }

-- | Make forecast with potential need to translate to next era
--
-- NOTE 1: It is possible that we had to go to a previous era to get this
-- forecast (point @p@ might not have been in the current era). If that is
-- the case, the forecast should nonetheless be anchored in that previous
-- era, /and not have any knowledge of anything that happened in current
-- era/. Therefore, we take the forecast from that previous era, apply it
-- as normal, and only /then/ translate to the next era if appropriate.
--
-- NOTE 2: If we did have to go to a previous era to get a forecast, then
-- that ledger must certainly have been aware of the transition.
--
-- NOTE 3: We assume that we only ever have to translate to the /next/
-- era (as opposed to /any/ subsequent era).
forecastNotFinal :: forall f blk blk' blks.
                    Translate f blk blk'
                 -> Current (AnnForecast f) blk
                 -> Forecast (HardForkLedgerView_ f (blk ': blk' ': blks))
forecastNotFinal g (Current start AnnForecast{..}) =
    Forecast (forecastAt annForecast) $ \for ->
      translateIf annForecastNext for <$> forecastFor annForecast for
  where
    -- Translate if the slot is past the end of the epoch
    translateIf :: Maybe EpochNo
                -> SlotNo
                -> f blk
                -> HardForkLedgerView_ f (blk ': blk' ': blks)
    translateIf (Just transition) for view | for >= boundSlot end =
        HardForkLedgerView {
            hardForkLedgerViewPerEra = HardForkState $
              TS (Past start end NoSnapshot) $
              TZ (Current end view')
          , hardForkLedgerViewTransition =
              TransitionImpossible
          }
      where
        end :: Bound
        end = History.mkUpperBound annForecastEraParams start transition

        view' :: f blk'
        view' = translateWith g (boundEpoch end) view

    -- We pretend that the anchor of the forecast is the tip
    translateIf mTransition _for view =
       HardForkLedgerView {
           hardForkLedgerViewPerEra = HardForkState $
             TZ (Current start view)
         , hardForkLedgerViewTransition =
             case mTransition of
               Nothing -> TransitionUnknown (forecastAt annForecast)
               Just t  -> TransitionKnown t
         }

shiftView :: Past g blk
          -> HardForkLedgerView_ f (blk' : blks)
          -> HardForkLedgerView_ f (blk : blk' : blks)
shiftView past HardForkLedgerView{..} = HardForkLedgerView {
      hardForkLedgerViewPerEra =
          HardForkState
        . TS (past { pastSnapshot = NoSnapshot })
        . getHardForkState
        $ hardForkLedgerViewPerEra
    , ..
    }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => Current (Ticked :.: LedgerState) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

ledgerViewInfo :: forall blk f. SingleEraBlock blk
               => (Ticked :.: f) blk -> LedgerEraInfo blk
ledgerViewInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectLedgerError :: Injection WrapLedgerErr xs blk
                  -> LedgerError blk
                  -> HardForkLedgerError xs
injectLedgerError inj =
      HardForkLedgerErrorFromEra
    . OneEraLedgerError
    . unK
    . apFn inj
    . WrapLedgerErr
