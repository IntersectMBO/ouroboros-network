{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger (
    HardForkLedgerError(..)
  , HardForkEnvelopeErr(..)
    -- * Type family instances
  , Ticked(..)
    -- * Low-level API (exported for the benefit of testing)
  , AnnForecast(..)
  , mkHardForkForecast
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP.Strict hiding (shape)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams)
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
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
                     (HardForkLedgerView_ (..), Ticked (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Telescope (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data HardForkLedgerError xs =
    -- | Validation error from one of the eras
    HardForkLedgerErrorFromEra (OneEraLedgerError xs)

    -- | We tried to apply a block from the wrong era
  | HardForkLedgerErrorWrongEra (MismatchEraInfo xs)
  deriving (Generic, Show, Eq, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance CanHardFork xs => GetTip (LedgerState (HardForkBlock xs)) where
  getTip = castPoint
         . State.getTip (castPoint . getTip)
         . hardForkLedgerStatePerEra

instance CanHardFork xs => GetTip (Ticked (LedgerState (HardForkBlock xs))) where
  getTip = castPoint
         . State.getTip (castPoint . getTip . unComp)
         . tickedHardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

data instance Ticked (LedgerState (HardForkBlock xs)) =
    TickedHardForkLedgerState {
        tickedHardForkLedgerStateTransition :: !TransitionInfo
      , tickedHardForkLedgerStatePerEra     ::
          !(HardForkState_ LedgerState (Ticked :.: LedgerState) xs)
      }
  deriving (Generic)

deriving anyclass instance
     CanHardFork xs
  => NoUnexpectedThunks (Ticked (LedgerState (HardForkBlock xs)))

instance CanHardFork xs => IsLedger (LedgerState (HardForkBlock xs)) where
  type LedgerErr (LedgerState (HardForkBlock xs)) = HardForkLedgerError  xs

  applyChainTick cfg@HardForkLedgerConfig{..} slot (HardForkLedgerState st) =
      TickedHardForkLedgerState {
          tickedHardForkLedgerStateTransition =
            -- We are bundling a 'TransitionInfo' with a /ticked/ ledger state,
            -- but /derive/ that 'TransitionInfo' from the /unticked/  (albeit
            -- extended) state. That requires justification. Three cases:
            --
            -- o 'TransitionUnknown'. If the transition is unknown, then it
            --   cannot become known due to ticking. In this case, we record
            --   the tip of the ledger, which ticking also does not modify
            --   (this is an explicit postcondition of 'applyChainTick').
            -- o 'TransitionKnown'. If the transition to the next epoch is
            --   already known, then ticking does not change that information.
            --   It can't be the case that the 'SlotNo' we're ticking to is
            --   /in/ that next era, because if was, then 'extendToSlot' would
            --   have extended the telescope further.
            --   (This does mean however that it is important to use the
            --   /extended/ ledger state, not the original, to determine the
            --   'TransitionInfo'.)
            -- o 'TransitionImpossible'. This has two subcases: either we are
            --   in the final era, in which case ticking certainly won't be able
            --   to change that, or we're forecasting, which is simply not
            --   applicable here.
            State.mostRecentTransitionInfo cfg extended
        , tickedHardForkLedgerStatePerEra =
            hczipWith proxySingle (tickOne ei slot) cfgs extended
        }
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoLedger cfg st

      extended :: HardForkState LedgerState xs
      extended = State.extendToSlot cfg slot st

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

  applyLedgerBlock cfg
                   (HardForkBlock (OneEraBlock block))
                   (TickedHardForkLedgerState transition st) =
      case State.match block st of
        Left mismatch ->
          -- Block from the wrong era (note that 'applyChainTick' will already
          -- have initiated the transition to the next era if appropriate).
          throwError $ HardForkLedgerErrorWrongEra . MismatchEraInfo $
            Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
        Right matched ->
          fmap (HardForkLedgerState . State.tickAllPast k) $ hsequence' $
            hczipWith3 proxySingle apply cfgs errInjections matched
    where
      cfgs = distribFullBlockConfig ei cfg
      lcfg = blockConfigLedger cfg
      k    = hardForkLedgerConfigK lcfg
      ei   = State.epochInfoPrecomputedTransitionInfo
               (hardForkLedgerConfigShape (blockConfigLedger cfg))
               transition
               st

      errInjections :: NP (Injection WrapLedgerErr xs) xs
      errInjections = injections

  reapplyLedgerBlock cfg
                     (HardForkBlock (OneEraBlock block))
                     (TickedHardForkLedgerState transition st) =
      case State.match block st of
        Left _mismatch ->
          -- We already applied this block to this ledger state,
          -- so it can't be from the wrong era
          error "reapplyLedgerBlock: can't be from other era"
        Right matched ->
          HardForkLedgerState . State.tickAllPast k $
            hczipWith proxySingle reapply cfgs matched
    where
      cfgs = distribFullBlockConfig ei cfg
      lcfg = blockConfigLedger cfg
      k    = hardForkLedgerConfigK lcfg
      ei   = State.epochInfoPrecomputedTransitionInfo
               (hardForkLedgerConfigShape (blockConfigLedger cfg))
               transition
               st

apply :: SingleEraBlock blk
      => WrapFullBlockConfig                               blk
      -> Injection WrapLedgerErr xs                        blk
      -> Product I (Ticked :.: LedgerState)                blk
      -> (Except (HardForkLedgerError xs) :.: LedgerState) blk
apply (WrapFullBlockConfig cfg) injectErr (Pair (I block) (Comp st)) = Comp $
    withExcept (injectLedgerError injectErr) $
      applyLedgerBlock cfg block st

reapply :: SingleEraBlock blk
        => WrapFullBlockConfig                blk
        -> Product I (Ticked :.: LedgerState) blk
        -> LedgerState                        blk
reapply (WrapFullBlockConfig cfg) (Pair (I block) (Comp st)) =
    reapplyLedgerBlock cfg block st

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

instance CanHardFork xs => UpdateLedger (HardForkBlock xs)

{-------------------------------------------------------------------------------
  HasHardForkHistory
-------------------------------------------------------------------------------}

instance All SingleEraBlock xs => HasHardForkHistory (HardForkBlock xs) where
  type HardForkIndices (HardForkBlock xs) = xs

  hardForkSummary cfg = State.reconstructSummaryLedger cfg
                      . hardForkLedgerStatePerEra

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
                           (TickedHardForkLedgerView transition hardForkView) =
                          \(HardForkHeader (OneEraHeader hdr)) ->
      case Match.matchNS hdr (State.tip hardForkView) of
        Left mismatch ->
          throwError $
            HardForkEnvelopeErrWrongEra . MismatchEraInfo $
              Match.bihcmap proxySingle singleEraInfo ledgerViewInfo mismatch
        Right matched ->
          hcollapse $ hczipWith3 proxySingle aux cfgs errInjections matched
    where
      ei :: EpochInfo Identity
      ei = State.epochInfoPrecomputedTransitionInfo
             (hardForkLedgerConfigShape $ configLedger tlc)
             transition
             hardForkView

      cfgs :: NP TopLevelConfig xs
      cfgs = distribTopLevelConfig ei tlc

      errInjections :: NP (Injection WrapEnvelopeErr xs) xs
      errInjections = injections

      aux :: forall blk. SingleEraBlock blk
          => TopLevelConfig blk
          -> Injection WrapEnvelopeErr xs blk
          -> Product Header (Ticked :.: WrapLedgerView) blk
          -> K (Except (HardForkEnvelopeErr xs) ()) blk
      aux cfg injErr (Pair hdr (Comp view)) = K $
          withExcept injErr' $
            additionalEnvelopeChecks
              cfg
              (unwrapTickedLedgerView view)
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
  protocolLedgerView HardForkLedgerConfig{..}
                     (TickedHardForkLedgerState transition ticked) =
      TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition = transition
        , tickedHardForkLedgerViewPerEra     =
            State.dropAllPast $
              hczipWith
                proxySingle
                tickedViewOne
                cfgs
                ticked
        }
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoPrecomputedTransitionInfo
               hardForkLedgerConfigShape
               transition
               ticked

      tickedViewOne :: SingleEraBlock              blk
                    => WrapPartialLedgerConfig     blk
                    -> (Ticked :.: LedgerState)    blk
                    -> (Ticked :.: WrapLedgerView) blk
      tickedViewOne cfg (Comp st) = Comp $
          WrapTickedLedgerView $
            protocolLedgerView (completeLedgerConfig' ei cfg) st

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
                    annForecast          = mapForecast WrapTickedLedgerView $
                                             forecast
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
mkHardForkForecast :: InPairs (TranslateForecast f) xs
                   -> Telescope (Past g) (Current (AnnForecast f)) xs
                   -> Forecast (HardForkLedgerView_ f xs)
mkHardForkForecast =
    go
  where
    go :: InPairs (TranslateForecast f) xs
       -> Telescope (Past g) (Current (AnnForecast f)) xs
       -> Forecast (HardForkLedgerView_ f xs)
    go PNil         (TZ f)      = forecastFinalEra f
    go (PCons g _)  (TZ f)      = forecastNotFinal g f
    go (PCons _ gs) (TS past f) = mapForecast (shiftView past) $ go gs f
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
    aux :: Ticked (f blk) -> Ticked (HardForkLedgerView_ f '[blk])
    aux era = TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition = TransitionImpossible
        , tickedHardForkLedgerViewPerEra     = HardForkState . TZ $
                                                 Current start (Comp era)
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
                    TranslateForecast f blk blk'
                 -> Current (AnnForecast f) blk
                 -> Forecast (HardForkLedgerView_ f (blk ': blk' ': blks))
forecastNotFinal g (Current start AnnForecast{..}) =
    Forecast (forecastAt annForecast) $ \for ->
      case mEnd of
        Just end | for >= boundSlot end -> do
          -- The forecast is trying to emulate what happens "in reality", where
          -- the translation from the ledger state of the first era to the next
          -- era will happen precisely at the transition point. So, we do the
          -- same in the forecast: we ask the first era for its final ledger
          -- view (i.e., the view in the final slot in this era), and then
          -- translate that to a ledger view in the next era. We pass 'for' to
          -- that translation function so that if any other changes were still
          -- scheduled to happen in the final ledger view of the first era, it
          -- can take those into account.
          --
          -- NOTE: Upper bound is exclusive so the final slot in this era is
          -- the predecessor of @boundSlot end@.

          final :: Ticked (f blk) <- forecastFor annForecast (pred (boundSlot end))
          let translated :: Ticked (f blk')
              translated = translateForecastWith g (boundEpoch end) for final

          return $ TickedHardForkLedgerView {
              tickedHardForkLedgerViewPerEra = HardForkState $
                TS (Past start end NoSnapshot) $
                TZ (Current end (Comp translated))
              -- See documentation of 'TransitionImpossible' for motivation
            , tickedHardForkLedgerViewTransition =
                TransitionImpossible
            }

        _otherwise -> do
          -- The end of this era is not yet known, or the slot we're
          -- constructing a forecast for is still within this era.
          view :: Ticked (f blk) <- forecastFor annForecast for

          return TickedHardForkLedgerView {
              tickedHardForkLedgerViewPerEra = HardForkState $
                TZ (Current start (Comp view))

              -- We pretend that the anchor of the forecast is the tip.
            , tickedHardForkLedgerViewTransition =
                case annForecastNext of
                  Nothing -> TransitionUnknown (forecastAt annForecast)
                  Just t  -> TransitionKnown t
            }
  where
    mEnd :: Maybe Bound
    mEnd = History.mkUpperBound annForecastEraParams start <$> annForecastNext

shiftView :: Past g blk
          -> Ticked (HardForkLedgerView_ f (blk' : blks))
          -> Ticked (HardForkLedgerView_ f (blk : blk' : blks))
shiftView past TickedHardForkLedgerView{..} = TickedHardForkLedgerView {
      tickedHardForkLedgerViewTransition = tickedHardForkLedgerViewTransition
    , tickedHardForkLedgerViewPerEra =
          HardForkState
        . TS (past { pastSnapshot = NoSnapshot })
        . getHardForkState
        $ tickedHardForkLedgerViewPerEra
    }

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data HardForkLedgerWarning xs =
    HardForkLedgerWarningInEra (OneEraLedgerWarning xs)

    -- | The transition to the next era does not match the 'EraParams'
    --
    -- The 'EraParams' can specify a lower bound on when the transition to the
    -- next era will happen. If the actual transition, when confirmed, is
    -- /before/ this lower bound, the node is misconfigured and will likely
    -- not work correctly. This should be taken care of as soon as possible
    -- (before the transition happens).
  | HardForkLedgerWarningUnexpectedTransition EraParams EpochNo

deriving instance CanHardFork xs => Show (HardForkLedgerWarning xs)
deriving instance CanHardFork xs => Eq   (HardForkLedgerWarning xs)

instance CanHardFork xs => InspectLedger (HardForkBlock xs) where
  type LedgerWarning (HardForkBlock xs) = HardForkLedgerWarning xs

  inspectLedger cfg (HardForkLedgerState st) = concat [
        -- Inspect the underlying ledger
          hcollapse
        . hczipWith3 proxySingle inspectOne cfgs injections
        $ State.tip st

        -- Hard fork specific warnings
      ,   hcollapse
        . hczipWith3 proxySingle additionalChecks pcfgs shape
        $ Telescope.tip (getHardForkState st)
      ]
    where
      HardForkLedgerConfig{..} = configLedger cfg

      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      shape = History.getShape hardForkLedgerConfigShape
      cfgs  = distribTopLevelConfig ei cfg
      ei    = State.epochInfoLedger (configLedger cfg) st

inspectOne :: forall blk xs. SingleEraBlock blk
           => TopLevelConfig blk
           -> Injection WrapLedgerWarning xs blk
           -> LedgerState blk
           -> K [HardForkLedgerWarning xs] blk
inspectOne cfg inj st = K $ map aux $ inspectLedger cfg st
  where
    aux :: LedgerWarning blk -> HardForkLedgerWarning xs
    aux = HardForkLedgerWarningInEra
        . OneEraLedgerWarning
        . unK
        . apFn inj
        . WrapLedgerWarning

additionalChecks :: SingleEraBlock blk
                 => WrapPartialLedgerConfig blk
                 -> K EraParams blk
                 -> Current LedgerState blk
                 -> K [HardForkLedgerWarning xs] blk
additionalChecks (WrapPartialLedgerConfig pcfg) (K eraParams) Current{..} = K $
    concat [
        [ HardForkLedgerWarningUnexpectedTransition eraParams transition
        | Just transition <- [singleEraTransition
                                pcfg
                                eraParams
                                currentStart
                                currentState]
        , not $ validLowerBound
                  (History.safeBeforeEpoch (History.eraSafeZone eraParams))
                  transition
        ]
      ]
  where
    validLowerBound :: Maybe History.SafeBeforeEpoch -> EpochNo -> Bool
    validLowerBound Nothing                       _  = False
    validLowerBound (Just History.NoLowerBound  ) _  = True
    validLowerBound (Just (History.LowerBound e)) e' = e' >= e

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
