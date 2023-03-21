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
    HardForkEnvelopeErr (..)
  , HardForkLedgerError (..)
  , HardForkLedgerUpdate (..)
  , HardForkLedgerWarning (..)
    -- * Type family instances
  , FlipTickedLedgerState (..)
  , LedgerTables (..)
  , Ticked1 (..)
    -- * Low-level API (exported for the benefit of testing)
  , AnnForecast (..)
  , mkHardForkForecast
  ) where

import           Control.Monad.Except
import           Data.Coerce
import           Data.Functor ((<&>))
import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP.Counting (getExactly)
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index
import           Data.SOP.InPairs (InPairs (..))
import qualified Data.SOP.InPairs as InPairs
import qualified Data.SOP.Match as Match
import           Data.SOP.Strict hiding (shape, tl)
import           Data.SOP.Telescope (Telescope (..))
import qualified Data.SOP.Telescope as Telescope
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams,
                     SafeZone (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data HardForkLedgerError xs =
    -- | Validation error from one of the eras
    HardForkLedgerErrorFromEra (OneEraLedgerError xs)

    -- | We tried to apply a block from the wrong era
  | HardForkLedgerErrorWrongEra (MismatchEraInfo xs)
  deriving (Generic, Show, Eq, NoThunks)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance CanHardFork xs => GetTip (LedgerState (HardForkBlock xs)) where
  getTip = castPoint
         . State.getTip (castPoint . getTip . unFlip)
         . hardForkLedgerStatePerEra

instance CanHardFork xs => GetTip (Ticked1 (LedgerState (HardForkBlock xs))) where
  getTip = castPoint
         . State.getTip (castPoint . getTip . getFlipTickedLedgerState)
         . tickedHardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

newtype FlipTickedLedgerState mk blk = FlipTickedLedgerState {
  getFlipTickedLedgerState :: (Ticked1 (LedgerState blk) mk)
  } deriving (Generic)

deriving newtype instance NoThunks (Ticked1 (LedgerState blk) mk)
                       => NoThunks (FlipTickedLedgerState mk blk)

data instance Ticked1 (LedgerState (HardForkBlock xs)) mk =
    TickedHardForkLedgerState {
        tickedHardForkLedgerStateTransition :: !TransitionInfo
      , tickedHardForkLedgerStatePerEra     ::
          !(HardForkState (FlipTickedLedgerState mk) xs)
      } deriving (Generic)

deriving instance All SingleEraBlock xs
               => NoThunks (Ticked1 (LedgerState (HardForkBlock xs)) TrackingMK)

instance CanHardFork xs => IsLedger (LedgerState (HardForkBlock xs)) where
  type LedgerErr (LedgerState (HardForkBlock xs)) = HardForkLedgerError  xs

  type AuxLedgerEvent (LedgerState (HardForkBlock xs)) = OneEraLedgerEvent xs

  applyChainTickLedgerResult cfg@HardForkLedgerConfig{..} slot (HardForkLedgerState st) =
      sequenceHardForkState
        (hcizipWith proxySingle (tickOne ei slot) cfgs extended) <&> \l' ->
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
        , tickedHardForkLedgerStatePerEra = l'
        }
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoLedger cfg st

      extended :: HardForkState (Flip LedgerState DiffMK) xs
      extended = State.extendToSlot cfg slot st

tickOne :: SingleEraBlock blk
        => EpochInfo (Except PastHorizonException)
        -> SlotNo
        -> Index                                          xs   blk
        -> WrapPartialLedgerConfig                             blk
        -> (Flip LedgerState DiffMK)                           blk
        -> (     LedgerResult (LedgerState (HardForkBlock xs))
             :.: FlipTickedLedgerState DiffMK
           )                                                   blk
tickOne ei slot sopIdx partialCfg st =
      Comp
    . fmap ( FlipTickedLedgerState
           . prependLedgerTablesDiffsTicked (unFlip st)
           )
    . embedLedgerResult (injectLedgerEvent sopIdx)
    . applyChainTickLedgerResult (completeLedgerConfig' ei partialCfg) slot
    . forgetLedgerTables
    . unFlip
    $ st

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance ( CanHardFork xs
         , HasTickedLedgerTables (LedgerState (HardForkBlock xs))
         , LedgerTablesCanHardFork xs
         )
      => ApplyBlock (LedgerState (HardForkBlock xs)) (HardForkBlock xs) where

  applyBlockLedgerResult cfg
                    (HardForkBlock (OneEraBlock block))
                    (TickedHardForkLedgerState transition st) =
      case State.match block st of
        Left mismatch ->
          -- Block from the wrong era (note that 'applyChainTick' will already
          -- have initiated the transition to the next era if appropriate).
            throwError
          $ HardForkLedgerErrorWrongEra . MismatchEraInfo
          $ Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
        Right matched ->
            fmap (fmap HardForkLedgerState . sequenceHardForkState)
          $ hsequence'
          $ hcizipWith proxySingle apply cfgs matched
    where
      cfgs = distribLedgerConfig ei cfg
      ei   = State.epochInfoPrecomputedTransitionInfo
               (hardForkLedgerConfigShape cfg)
               transition
               st

  reapplyBlockLedgerResult cfg
                      (HardForkBlock (OneEraBlock block))
                      (TickedHardForkLedgerState transition st) =
      case State.match block st of
        Left _mismatch ->
          -- We already applied this block to this ledger state,
          -- so it can't be from the wrong era
          error "reapplyBlockLedgerResult: can't be from other era"
        Right matched ->
            fmap HardForkLedgerState
          $ sequenceHardForkState
          $ hcizipWith proxySingle reapply cfgs matched
    where
      cfgs = distribLedgerConfig ei cfg
      ei   = State.epochInfoPrecomputedTransitionInfo
               (hardForkLedgerConfigShape cfg)
               transition
               st

  getBlockKeySets (HardForkBlock (OneEraBlock ns)) =
        hcollapse
      $ hczipWith proxySingle f hardForkInjectLedgerTables ns
    where
      f ::
           SingleEraBlock                                           x
        => InjectLedgerTables xs                                    x
        -> I                                                        x
        -> K (LedgerTables (LedgerState (HardForkBlock xs)) KeysMK) x
      f inj (I blk) = K $ applyInjectLedgerTables inj $ getBlockKeySets blk

apply :: SingleEraBlock blk
      => Index xs                                           blk
      -> WrapLedgerConfig                                   blk
      -> Product I (FlipTickedLedgerState ValuesMK)         blk
      -> (    Except (HardForkLedgerError xs)
          :.: LedgerResult (LedgerState (HardForkBlock xs))
          :.: Flip LedgerState DiffMK
         )                                                  blk
apply index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
      Comp
    $ withExcept (injectLedgerError index)
    $ fmap (Comp . fmap Flip . embedLedgerResult (injectLedgerEvent index))
    $ applyBlockLedgerResult cfg block st

reapply :: SingleEraBlock blk
        => Index xs                                           blk
        -> WrapLedgerConfig                                   blk
        -> Product I (FlipTickedLedgerState ValuesMK)         blk
        -> (    LedgerResult (LedgerState (HardForkBlock xs))
            :.: Flip LedgerState DiffMK
           )                                                  blk
reapply index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
      Comp
    $ fmap Flip
    $ embedLedgerResult (injectLedgerEvent index)
    $ reapplyBlockLedgerResult cfg block st

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

instance ( CanHardFork xs
         , HasTickedLedgerTables (LedgerState (HardForkBlock xs))
         , LedgerTablesCanHardFork xs
         )
      => UpdateLedger (HardForkBlock xs)

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
  deriving (Eq, Show, Generic, NoThunks)

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
          hcollapse $ hcizipWith proxySingle aux cfgs matched
    where
      ei :: EpochInfo (Except PastHorizonException)
      ei = State.epochInfoPrecomputedTransitionInfo
             (hardForkLedgerConfigShape $ configLedger tlc)
             transition
             hardForkView

      cfgs :: NP TopLevelConfig xs
      cfgs = distribTopLevelConfig ei tlc

      aux :: forall blk. SingleEraBlock blk
          => Index xs blk
          -> TopLevelConfig blk
          -> Product Header (Ticked :.: WrapLedgerView) blk
          -> K (Except (HardForkEnvelopeErr xs) ()) blk
      aux index cfg (Pair hdr (Comp view)) = K $
          withExcept injErr' $
            additionalEnvelopeChecks
              cfg
              (unwrapTickedLedgerView view)
              hdr
        where
          injErr' :: OtherHeaderEnvelopeError blk -> HardForkEnvelopeErr xs
          injErr' = HardForkEnvelopeErrFromEra
                  . OneEraEnvelopeErr
                  . injectNS index
                  . WrapEnvelopeErr

{-------------------------------------------------------------------------------
  LedgerSupportsProtocol
-------------------------------------------------------------------------------}

instance ( CanHardFork xs
         , HasTickedLedgerTables (LedgerState (HardForkBlock xs))
         , LedgerTablesCanHardFork xs
         )
      => LedgerSupportsProtocol (HardForkBlock xs) where
  protocolLedgerView HardForkLedgerConfig{..}
                     (TickedHardForkLedgerState transition ticked) =
      TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition = transition
        , tickedHardForkLedgerViewPerEra     =
            hczipWith proxySingle tickedViewOne cfgs ticked
        }
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoPrecomputedTransitionInfo
               hardForkLedgerConfigShape
               transition
               ticked

      tickedViewOne :: SingleEraBlock              blk
                    => WrapPartialLedgerConfig     blk
                    -> FlipTickedLedgerState mk    blk
                    -> (Ticked :.: WrapLedgerView) blk
      tickedViewOne cfg (FlipTickedLedgerState st) = Comp $
          WrapTickedLedgerView $
            protocolLedgerView (completeLedgerConfig' ei cfg) st

  ledgerViewForecastAt ledgerCfg@HardForkLedgerConfig{..}
                       (HardForkLedgerState ledgerSt) =
      mkHardForkForecast
        (InPairs.requiringBoth cfgs $ translateLedgerView hardForkEraTranslation)
        annForecast
    where
      ei    = State.epochInfoLedger ledgerCfg ledgerSt
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs

      annForecast :: HardForkState (AnnForecast LedgerState WrapLedgerView) xs
      annForecast = HardForkState $
          hczipWith3
            proxySingle
            forecastOne
            pcfgs
            (getExactly (History.getShape hardForkLedgerConfigShape))
            (getHardForkState ledgerSt)

      forecastOne ::
             forall blk mk. SingleEraBlock blk
          => WrapPartialLedgerConfig blk
          -> K EraParams blk
          -> Current (Flip LedgerState mk) blk
          -> Current (AnnForecast LedgerState WrapLedgerView) blk
      forecastOne cfg (K params) (Current start (Flip st)) = Current {
            currentStart = start
          , currentState = AnnForecast {
                annForecast      = mapForecast WrapTickedLedgerView $
                                     ledgerViewForecastAt cfg' st
              , annForecastState = forgetLedgerTables st
              , annForecastTip   = ledgerTipSlot st
              , annForecastEnd   = History.mkUpperBound params start <$>
                                     singleEraTransition' cfg params start st
              }
          }
        where
          cfg' :: LedgerConfig blk
          cfg' = completeLedgerConfig' ei cfg

{-------------------------------------------------------------------------------
  Annotated forecasts
-------------------------------------------------------------------------------}

-- | Forecast annotated with details about the ledger it was derived from
data AnnForecast state view blk = AnnForecast {
      annForecast      :: Forecast (view blk)
    , annForecastState :: state blk EmptyMK
    , annForecastTip   :: WithOrigin SlotNo
    , annForecastEnd   :: Maybe Bound
    }

-- | Change a telescope of a forecast into a forecast of a telescope
mkHardForkForecast ::
     forall state view xs.
     SListI xs
  => InPairs (TranslateForecast state view) xs
  -> HardForkState (AnnForecast state view) xs
  -> Forecast (HardForkLedgerView_ view xs)
mkHardForkForecast translations st = Forecast {
      forecastAt  = hcollapse (hmap (K . forecastAt . annForecast) st)
    , forecastFor = \sno -> go sno translations (getHardForkState st)
    }
  where
    go :: SlotNo
       -> InPairs (TranslateForecast state view) xs'
       -> Telescope (K Past) (Current (AnnForecast state view)) xs'
       -> Except OutsideForecastRange (Ticked (HardForkLedgerView_ view xs'))
    go sno pairs        (TZ cur)       = oneForecast sno pairs cur
    go sno (PCons _ ts) (TS past rest) = shiftView past <$> go sno ts rest

oneForecast ::
     forall state view blk blks.
     SlotNo
  -> InPairs (TranslateForecast state view) (blk : blks)
     -- ^ this function uses at most the first translation
  -> Current (AnnForecast state view) blk
  -> Except OutsideForecastRange (Ticked (HardForkLedgerView_ view (blk : blks)))
oneForecast sno pairs (Current start AnnForecast{..}) =
    case annForecastEnd of
      Nothing  -> endUnknown <$> forecastFor annForecast sno
      Just end ->
        if sno < boundSlot end
        then beforeKnownEnd end <$> forecastFor annForecast sno
        else case pairs of
          PCons translate _ ->
                afterKnownEnd end
            <$> translateForecastWith translate end sno annForecastState
          PNil              ->
            -- The requested slot is after the last era the code knows about.
            throwError OutsideForecastRange {
                outsideForecastAt     = forecastAt annForecast
              , outsideForecastMaxFor = boundSlot end
              , outsideForecastFor    = sno
              }
  where
    endUnknown ::
         Ticked (f blk)
      -> Ticked (HardForkLedgerView_ f (blk : blks))
    endUnknown view = TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition =
            TransitionUnknown annForecastTip
        , tickedHardForkLedgerViewPerEra = HardForkState $
            TZ (Current start (Comp view))
        }

    beforeKnownEnd ::
         Bound
      -> Ticked (f blk)
      -> Ticked (HardForkLedgerView_ f (blk : blks))
    beforeKnownEnd end view = TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition =
            TransitionKnown (boundEpoch end)
        , tickedHardForkLedgerViewPerEra = HardForkState $
            TZ (Current start (Comp view))
        }

    afterKnownEnd ::
         Bound
      -> Ticked (f blk')
      -> Ticked (HardForkLedgerView_ f (blk : blk' : blks'))
    afterKnownEnd end view = TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition =
            -- We assume that we only ever have to translate to the /next/ era
            -- (as opposed to /any/ subsequent era)
            TransitionImpossible
        , tickedHardForkLedgerViewPerEra = HardForkState $
            TS (K (Past start end)) $
            TZ (Current end (Comp view))
        }

shiftView :: K Past blk
          -> Ticked (HardForkLedgerView_ f blks)
          -> Ticked (HardForkLedgerView_ f (blk : blks))
shiftView past TickedHardForkLedgerView{..} = TickedHardForkLedgerView {
      tickedHardForkLedgerViewTransition = tickedHardForkLedgerViewTransition
    , tickedHardForkLedgerViewPerEra =
          HardForkState
        . TS past
        . getHardForkState
        $ tickedHardForkLedgerViewPerEra
    }

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data HardForkLedgerWarning xs =
    -- | Warning from the underlying era
    HardForkWarningInEra (OneEraLedgerWarning xs)

    -- | The transition to the next era does not match the 'EraParams'
    --
    -- The 'EraParams' can specify a lower bound on when the transition to the
    -- next era will happen. If the actual transition, when confirmed, is
    -- /before/ this lower bound, the node is misconfigured and will likely
    -- not work correctly. This should be taken care of as soon as possible
    -- (before the transition happens).
  | HardForkWarningTransitionMismatch (EraIndex xs) EraParams EpochNo

    -- | Transition in the final era
    --
    -- The final era should never confirm any transitions. For clarity, we also
    -- record the index of that final era.
  | HardForkWarningTransitionInFinalEra (EraIndex xs) EpochNo

    -- | An already-confirmed transition got un-confirmed
  | HardForkWarningTransitionUnconfirmed (EraIndex xs)

    -- | An already-confirmed transition got changed
    --
    -- We record the indices of the era we are transitioning from and to,
    -- as well as the old and new 'EpochNo' of that transition, in that order.
  | HardForkWarningTransitionReconfirmed (EraIndex xs) (EraIndex xs) EpochNo EpochNo

data HardForkLedgerUpdate xs =
    HardForkUpdateInEra (OneEraLedgerUpdate xs)

    -- | Hard fork transition got confirmed
  | HardForkUpdateTransitionConfirmed (EraIndex xs) (EraIndex xs) EpochNo

    -- | Hard fork transition happened
    --
    -- We record the 'EpochNo' at the start of the era after the transition
  | HardForkUpdateTransitionDone (EraIndex xs) (EraIndex xs) EpochNo

    -- | The hard fork transition rolled back
  | HardForkUpdateTransitionRolledBack (EraIndex xs) (EraIndex xs)

deriving instance CanHardFork xs => Show (HardForkLedgerWarning xs)
deriving instance CanHardFork xs => Eq   (HardForkLedgerWarning xs)

deriving instance CanHardFork xs => Show (HardForkLedgerUpdate xs)
deriving instance CanHardFork xs => Eq   (HardForkLedgerUpdate xs)

instance CanHardFork xs => Condense (HardForkLedgerUpdate xs) where
  condense (HardForkUpdateInEra (OneEraLedgerUpdate update)) =
      hcollapse $ hcmap proxySingle (K . condense . unwrapLedgerUpdate) update
  condense (HardForkUpdateTransitionConfirmed ix ix' t) =
      "confirmed " ++ condense (ix, ix', t)
  condense (HardForkUpdateTransitionDone ix ix' e) =
      "done " ++ condense (ix, ix', e)
  condense (HardForkUpdateTransitionRolledBack ix ix') =
      "rolled back " ++ condense (ix, ix')

instance CanHardFork xs => InspectLedger (HardForkBlock xs) where
  type LedgerWarning (HardForkBlock xs) = HardForkLedgerWarning xs
  type LedgerUpdate  (HardForkBlock xs) = HardForkLedgerUpdate  xs

  inspectLedger cfg
                (HardForkLedgerState before)
                (HardForkLedgerState after) =
      inspectHardForkLedger
        pcfgs
        (getExactly shape)
        cfgs
        (Telescope.tip (getHardForkState before))
        (Telescope.tip (getHardForkState after))
    where
      HardForkLedgerConfig{..} = configLedger cfg

      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      shape = History.getShape hardForkLedgerConfigShape
      cfgs  = distribTopLevelConfig ei cfg
      ei    = State.epochInfoLedger (configLedger cfg) after

inspectHardForkLedger ::
     CanHardFork xs
  => NP WrapPartialLedgerConfig xs
  -> NP (K EraParams) xs
  -> NP TopLevelConfig xs
  -> NS (Current (Flip LedgerState mk1)) xs
  -> NS (Current (Flip LedgerState mk2)) xs
  -> [LedgerEvent (HardForkBlock xs)]
inspectHardForkLedger = go
  where
    go :: All SingleEraBlock xs
       => NP WrapPartialLedgerConfig xs
       -> NP (K EraParams) xs
       -> NP TopLevelConfig xs
       -> NS (Current (Flip LedgerState mk1)) xs
       -> NS (Current (Flip LedgerState mk2)) xs
       -> [LedgerEvent (HardForkBlock xs)]

    go (pc :* _) (K ps :* pss) (c :* _) (Z before) (Z after) = concat [
          map liftEvent $
            inspectLedger
              c
              (unFlip $ currentState before)
              (unFlip $ currentState after)

        , case (pss, confirmedBefore, confirmedAfter) of
            (_, Nothing, Nothing) ->
              []
            (_, Just _, Nothing) ->
              -- TODO: This should be a warning, but this can currently happen
              -- in Byron.
              []
              -- return $ LedgerWarning $
              --   HardForkWarningTransitionUnconfirmed eraIndexZero
            (Nil, Nothing, Just transition) ->
              return $ LedgerWarning $
                HardForkWarningTransitionInFinalEra eraIndexZero transition
            (Nil, Just transition, Just transition') -> do
              -- Only warn if the transition has changed
              guard (transition /= transition')
              return $ LedgerWarning $
                HardForkWarningTransitionInFinalEra eraIndexZero transition
            ((:*){}, Nothing, Just transition) ->
              return $
                if validLowerBound (History.eraSafeZone ps)
                  then LedgerUpdate $
                         HardForkUpdateTransitionConfirmed
                           eraIndexZero
                           (eraIndexSucc eraIndexZero)
                           transition
                  else LedgerWarning $
                         HardForkWarningTransitionMismatch
                           eraIndexZero
                           ps
                           transition
            ((:*){}, Just transition, Just transition') -> do
              guard (transition /= transition')
              return $ LedgerWarning $
                HardForkWarningTransitionReconfirmed
                  eraIndexZero
                  (eraIndexSucc eraIndexZero)
                  transition
                  transition'
        ]
      where
        confirmedBefore, confirmedAfter :: Maybe EpochNo
        confirmedBefore = singleEraTransition
                            (unwrapPartialLedgerConfig pc)
                            ps
                            (currentStart before)
                            (unFlip $ currentState before)
        confirmedAfter  = singleEraTransition
                            (unwrapPartialLedgerConfig pc)
                            ps
                            (currentStart after)
                            (unFlip $ currentState after)

    go Nil _ _ before _ =
        case before of {}
    go (_ :* pcs) (_ :* pss) (_ :* cs) (S before) (S after) =
        map shiftEvent $ go pcs pss cs before after
    go _ _ _ (Z _) (S after) =
        return $
          LedgerUpdate $
            HardForkUpdateTransitionDone
              eraIndexZero
              (eraIndexSucc $ eraIndexFromNS after)
              (hcollapse $ hmap (K . boundEpoch . currentStart) after)
    go _ _ _ (S before) (Z _) =
        return $
          LedgerUpdate $
            HardForkUpdateTransitionRolledBack
              (eraIndexSucc $ eraIndexFromNS before)
              eraIndexZero

    validLowerBound :: SafeZone -> Bool
    validLowerBound (StandardSafeZone _)     = True
    validLowerBound UnsafeIndefiniteSafeZone = False

{-------------------------------------------------------------------------------
  Internal auxiliary: lifting and shifting events
-------------------------------------------------------------------------------}

liftEvent :: LedgerEvent x
          -> LedgerEvent (HardForkBlock (x ': xs))
liftEvent (LedgerWarning warning) = LedgerWarning $ liftWarning warning
liftEvent (LedgerUpdate  update)  = LedgerUpdate  $ liftUpdate  update

liftWarning :: LedgerWarning x -> HardForkLedgerWarning (x ': xs)
liftWarning =
      HardForkWarningInEra
    . OneEraLedgerWarning
    . Z
    . WrapLedgerWarning

liftUpdate :: LedgerUpdate x -> HardForkLedgerUpdate (x ': xs)
liftUpdate =
      HardForkUpdateInEra
    . OneEraLedgerUpdate
    . Z
    . WrapLedgerUpdate

shiftEvent :: LedgerEvent (HardForkBlock xs)
           -> LedgerEvent (HardForkBlock (x ': xs))
shiftEvent (LedgerWarning warning) = LedgerWarning $ shiftWarning warning
shiftEvent (LedgerUpdate  update)  = LedgerUpdate  $ shiftUpdate  update

shiftWarning :: HardForkLedgerWarning xs -> HardForkLedgerWarning (x ': xs)
shiftWarning = go
  where
    go (HardForkWarningInEra (OneEraLedgerWarning warning)) =
        HardForkWarningInEra
          (OneEraLedgerWarning (S warning))
    go (HardForkWarningTransitionMismatch ix ps t) =
        HardForkWarningTransitionMismatch
          (eraIndexSucc ix)
          ps
          t
    go (HardForkWarningTransitionInFinalEra ix t) =
        HardForkWarningTransitionInFinalEra
          (eraIndexSucc ix)
          t
    go (HardForkWarningTransitionUnconfirmed ix) =
        HardForkWarningTransitionUnconfirmed
          (eraIndexSucc ix)
    go (HardForkWarningTransitionReconfirmed ix ix' t t') =
        HardForkWarningTransitionReconfirmed
          (eraIndexSucc ix)
          (eraIndexSucc ix')
          t
          t'

shiftUpdate :: HardForkLedgerUpdate xs -> HardForkLedgerUpdate (x ': xs)
shiftUpdate = go
  where
    go :: HardForkLedgerUpdate xs -> HardForkLedgerUpdate (x ': xs)
    go (HardForkUpdateInEra (OneEraLedgerUpdate update)) =
        HardForkUpdateInEra
          (OneEraLedgerUpdate (S update))
    go (HardForkUpdateTransitionConfirmed ix ix' t) =
        HardForkUpdateTransitionConfirmed
          (eraIndexSucc ix)
          (eraIndexSucc ix')
          t
    go (HardForkUpdateTransitionDone ix ix' e) =
        HardForkUpdateTransitionDone
          (eraIndexSucc ix)
          (eraIndexSucc ix')
          e
    go (HardForkUpdateTransitionRolledBack ix ix') =
        HardForkUpdateTransitionRolledBack
          (eraIndexSucc ix)
          (eraIndexSucc ix')

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk mk. SingleEraBlock blk
           => Current (FlipTickedLedgerState mk) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

ledgerViewInfo :: forall blk f. SingleEraBlock blk
               => (Ticked :.: f) blk -> LedgerEraInfo blk
ledgerViewInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectLedgerError :: Index xs blk -> LedgerError blk -> HardForkLedgerError xs
injectLedgerError index =
      HardForkLedgerErrorFromEra
    . OneEraLedgerError
    . injectNS index
    . WrapLedgerErr

injectLedgerEvent :: Index xs blk -> AuxLedgerEvent (LedgerState blk) -> OneEraLedgerEvent xs
injectLedgerEvent index =
      OneEraLedgerEvent
    . injectNS index
    . WrapLedgerEvent

{-------------------------------------------------------------------------------
  Ledger Tables for the unary HardForkBlock

  In particular this is needed because we will instantiate the Shelley unary
  HF Block, which will require to be able to serialize ledger tables in order to
  comply with the required classes, therefore bringing in all these instances.

  Instead of defining this on Shelley, it sounds reasonable to define it here
  for the unary HF block.
-------------------------------------------------------------------------------}

instance HasLedgerTables (LedgerState blk)
      => HasLedgerTables (LedgerState (HardForkBlock '[blk])) where

  newtype LedgerTables (LedgerState (HardForkBlock '[blk])) mk = OneEraLedgerTables {
      unOneEraLedgerTables :: LedgerTables (LedgerState blk) mk
      }
    deriving (Generic)

  projectLedgerTables (HardForkLedgerState (HardForkState (Telescope.TZ (Current _ (Flip lstate))))) =
      OneEraLedgerTables (projectLedgerTables lstate)
  withLedgerTables (HardForkLedgerState (HardForkState (Telescope.TZ (Current s (Flip lstate))))) (OneEraLedgerTables tables) =
      HardForkLedgerState (HardForkState (Telescope.TZ (Current s (Flip $ lstate `withLedgerTables` tables))))

  traverseLedgerTables f (OneEraLedgerTables  l) =
    OneEraLedgerTables  <$> traverseLedgerTables f l

  pureLedgerTables  f = coerce $ pureLedgerTables  @(LedgerState blk) f
  mapLedgerTables   f = coerce $ mapLedgerTables   @(LedgerState blk) f
  zipLedgerTables   f = coerce $ zipLedgerTables   @(LedgerState blk) f
  zipLedgerTables3  f = coerce $ zipLedgerTables3  @(LedgerState blk) f
  foldLedgerTables  f = coerce $ foldLedgerTables  @(LedgerState blk) f
  foldLedgerTables2 f = coerce $ foldLedgerTables2 @(LedgerState blk) f
  namesLedgerTables   = coerce $ namesLedgerTables @(LedgerState blk)
  zipLedgerTablesA  f (OneEraLedgerTables l) (OneEraLedgerTables r) =
    OneEraLedgerTables <$> zipLedgerTablesA f l r
  zipLedgerTables3A  f tl tc tr =
     OneEraLedgerTables <$> zipLedgerTables3A f l c r
   where
     OneEraLedgerTables l = tl
     OneEraLedgerTables c = tc
     OneEraLedgerTables r = tr

deriving instance Eq (LedgerTables (LedgerState blk) mk)
               => Eq (LedgerTables (LedgerState (HardForkBlock '[blk])) mk)
deriving newtype instance NoThunks (LedgerTables (LedgerState blk) mk)
                       => NoThunks (LedgerTables (LedgerState (HardForkBlock '[blk])) mk)
deriving instance Show (LedgerTables (LedgerState blk) mk)
               => Show (LedgerTables (LedgerState (HardForkBlock '[blk])) mk)

instance CanSerializeLedgerTables (LedgerState blk)
      => CanSerializeLedgerTables (LedgerState (HardForkBlock '[blk])) where
  codecLedgerTables = OneEraLedgerTables codecLedgerTables

instance LedgerTablesAreTrivial (LedgerState blk)
      => LedgerTablesAreTrivial (LedgerState (HardForkBlock '[blk])) where
  convertMapKind (HardForkLedgerState (HardForkState (Telescope.TZ (Current s (Flip lstate))))) =
      HardForkLedgerState (HardForkState (Telescope.TZ (Current s (Flip $ convertMapKind lstate))))

  trivialLedgerTables = OneEraLedgerTables trivialLedgerTables

instance HasTickedLedgerTables (LedgerState blk)
      => HasTickedLedgerTables (LedgerState (HardForkBlock '[blk])) where
  projectLedgerTablesTicked (TickedHardForkLedgerState _ (HardForkState (Telescope.TZ (Current _ (FlipTickedLedgerState lstate))))) =
      OneEraLedgerTables (projectLedgerTablesTicked lstate)
  withLedgerTablesTicked
    (TickedHardForkLedgerState i (HardForkState (Telescope.TZ (Current s (FlipTickedLedgerState lstate)))))
    (OneEraLedgerTables tables) =
      TickedHardForkLedgerState i (HardForkState (Telescope.TZ (Current s (FlipTickedLedgerState $ lstate `withLedgerTablesTicked` tables))))

instance CanStowLedgerTables (LedgerState blk)
      => CanStowLedgerTables (LedgerState (HardForkBlock '[blk])) where
   stowLedgerTables (HardForkLedgerState (HardForkState (Telescope.TZ (Current s (Flip lstate))))) =
      HardForkLedgerState (HardForkState (Telescope.TZ (Current s (Flip $ stowLedgerTables lstate))))

   unstowLedgerTables (HardForkLedgerState (HardForkState (Telescope.TZ (Current s (Flip lstate))))) =
      HardForkLedgerState (HardForkState (Telescope.TZ (Current s (Flip $ unstowLedgerTables lstate))))

instance LedgerTablesCanHardFork '[blk] where
  hardForkInjectLedgerTables =
       (InjectLedgerTables { applyInjectLedgerTables = OneEraLedgerTables
                           , applyDistribLedgerTables = unOneEraLedgerTables
                           })
    :* Nil
