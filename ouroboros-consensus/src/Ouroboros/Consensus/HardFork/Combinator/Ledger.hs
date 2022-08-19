{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
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
  , Ticked1 (..)
    -- * Low-level API (exported for the benefit of testing)
  , AnnForecast (..)
  , mkHardForkForecast
    -- * Misc
  , CanHardFork
  ) where

import           Control.Monad.Except
import           Data.Coerce
import           Data.Functor ((<&>))
import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP.Strict hiding (shape)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams,
                     SafeZone (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Counting (getExactly)
import           Ouroboros.Consensus.Util.SOP

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
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
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
  deriving (Generic, Show, Eq, NoThunks)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

type instance HeaderHash (Flip2 LedgerState wt mk blk) = HeaderHash blk

instance GetTip (LedgerState blk wt mk) => GetTip (Flip2 LedgerState wt mk blk) where
  getTip = castPoint . getTip @(LedgerState blk wt mk) . unFlip2

instance ( CanHardFork xs
         , All SingleEraBlock xs
         ) => GetTip (LedgerState (HardForkBlock xs) wt mk) where
  getTip = hcollapse
         . hcmap proxySingle f
         . State.tip
         . hardForkLedgerStatePerEra
    where

      f :: forall x .
           ( GetTip (LedgerState x wt EmptyMK)
           , TableStuff (LedgerState x) wt
           , ConvertRawHash x
           )
        =>     Flip2 LedgerState wt mk x
        -> K (Point (LedgerState (HardForkBlock xs) wt mk)) x
      f = K . (\case
                  GenesisPoint -> GenesisPoint
                  BlockPoint slot h -> BlockPoint slot $ OneEraHash $ toShortRawHash (Proxy @x) h) . getTip . forgetLedgerTables . unFlip2

instance ( CanHardFork xs
         , All SingleEraBlock xs
         ) => GetTip (Ticked1 (LedgerState (HardForkBlock xs) wt) mk) where
  getTip = hcollapse
         . hcmap proxySingle f
         . tickedHardForkLedgerStatePerEra
    where

      f :: forall x.
           ( GetTip (Ticked1 (LedgerState x wt) EmptyMK)
           , TickedTableStuff (LedgerState x) wt
           , ConvertRawHash x
           )
        => FlipTickedLedgerState mk wt x
        -> K (Point (Ticked1 (LedgerState (HardForkBlock xs) wt) mk)) x
      f = K . (\case
                  GenesisPoint -> GenesisPoint
                  BlockPoint slot h -> BlockPoint slot $ OneEraHash $ toShortRawHash (Proxy @x) h) . getTip . forgetLedgerTablesTicked . getFlipTickedLedgerState

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

newtype FlipTickedLedgerState mk wt blk = FlipTickedLedgerState {getFlipTickedLedgerState :: Ticked1 (LedgerState blk wt) mk}

deriving newtype instance NoThunks (Ticked1 (LedgerState blk wt) mk) => NoThunks (FlipTickedLedgerState mk wt blk)

data instance Ticked1 (LedgerState (HardForkBlock xs) wt) mk =
    TickedHardForkLedgerState {
        tickedHardForkLedgerStateTransition :: !TransitionInfo
      , tickedHardForkLedgerStatePerEra     ::
          !(HardForkState (FlipTickedLedgerState mk wt) xs)
      }
  deriving (Generic)

instance ( CanHardFork xs
         )
  => IsLedger (LedgerState (HardForkBlock xs)) where
  type LedgerErr (LedgerState (HardForkBlock xs)) = HardForkLedgerError  xs

  type AuxLedgerEvent (LedgerState (HardForkBlock xs)) = OneEraLedgerEvent xs

  applyChainTickLedgerResult :: forall wt.
                              LedgerCfg (LedgerState (HardForkBlock xs))
                             -> SlotNo
                             -> LedgerState (HardForkBlock xs) wt EmptyMK
                             -> LedgerResult
                                  (LedgerState (HardForkBlock xs))
                                  (Ticked1 (LedgerState (HardForkBlock xs) wt) DiffMK)
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

      extended = State.extendToSlot cfg slot st

tickOne :: ( SingleEraBlock blk
           , TickedTableStuff (LedgerState blk) wt
           )
        => EpochInfo (Except PastHorizonException)
        -> SlotNo
        -> Index                                          xs   blk
        -> WrapPartialLedgerConfig                             blk
        -> (Flip2 LedgerState wt DiffMK)                           blk
        -> (     LedgerResult (LedgerState (HardForkBlock xs))
             :.: FlipTickedLedgerState DiffMK wt
           )                                                   blk
tickOne ei slot sopIdx partialCfg st =
      Comp
    . fmap ( FlipTickedLedgerState
           . prependLedgerTablesDiffsTicked (unFlip2 st)
           )
    . embedLedgerResult (injectLedgerEvent sopIdx)
    . applyChainTickLedgerResult (completeLedgerConfig' ei partialCfg) slot
    . forgetLedgerTables
    . unFlip2
    $ st

instance LedgerTablesCanHardFork '[x] where
  hardForkInjectLedgerTablesKeysMK = InjectLedgerTables LedgerTablesOne :* Nil

instance (SingleEraBlock x, TableStuff (LedgerState x) WithLedgerTables) => TableStuff (LedgerState (HardForkBlock '[x])) WithLedgerTables where
  newtype LedgerTables (LedgerState (HardForkBlock '[x])) WithLedgerTables mk = LedgerTablesOne (LedgerTables (LedgerState x) WithLedgerTables mk)
    deriving (Generic)

  projectLedgerTables = LedgerTablesOne . projectLedgerTables . projectOneState

  withLedgerTables st (LedgerTablesOne tables) =
      withOneState st $ projectOneState st `withLedgerTables` tables

  traverseLedgerTables f (LedgerTablesOne tables) =
      LedgerTablesOne <$> traverseLedgerTables f tables

  pureLedgerTables  f = coerce $ pureLedgerTables  @(LedgerState x) f
  mapLedgerTables   f = coerce $ mapLedgerTables   @(LedgerState x) f
  zipLedgerTables   f = coerce $ zipLedgerTables   @(LedgerState x) f
  zipLedgerTables2  f = coerce $ zipLedgerTables2  @(LedgerState x) f
  foldLedgerTables  f = coerce $ foldLedgerTables  @(LedgerState x) f
  foldLedgerTables2 f = coerce $ foldLedgerTables2 @(LedgerState x) f
  namesLedgerTables   = coerce $ namesLedgerTables @(LedgerState x)
  zipLedgerTablesA   f (LedgerTablesOne l) (LedgerTablesOne r) =
      LedgerTablesOne <$> zipLedgerTablesA f l r
  zipLedgerTables2A  f (LedgerTablesOne l) (LedgerTablesOne c) (LedgerTablesOne r) =
      LedgerTablesOne <$> zipLedgerTables2A f l c r

deriving instance Eq (LedgerTables (LedgerState x) WithLedgerTables mk) => Eq (LedgerTables (LedgerState (HardForkBlock '[x])) WithLedgerTables mk)

instance ( SingleEraBlock x
         , SufficientSerializationForAnyBackingStore (LedgerState x) WithLedgerTables
         )
      => SufficientSerializationForAnyBackingStore (LedgerState (HardForkBlock '[x])) WithLedgerTables where
    codecLedgerTables = LedgerTablesOne codecLedgerTables

projectOneState :: LedgerState (HardForkBlock '[x]) wt mk -> LedgerState x wt mk
projectOneState (HardForkLedgerState (HardForkState (TZ current))) =
    unFlip2 $ currentState $ current

withOneState ::
     LedgerState (HardForkBlock '[x]) wt any
  -> LedgerState x wt mk
  -> LedgerState (HardForkBlock '[x]) wt mk
withOneState
  (HardForkLedgerState (HardForkState (TZ current)))
  st'
    = HardForkLedgerState $ HardForkState $ TZ current{currentState = Flip2 st'}

instance ( TickedTableStuff (LedgerState x) WithLedgerTables
         , SingleEraBlock x
         ) => TickedTableStuff (LedgerState (HardForkBlock '[x])) WithLedgerTables where

  projectLedgerTablesTicked st =
      LedgerTablesOne $ projectLedgerTablesTicked x
    where
      HardForkState (TZ current) = tickedHardForkLedgerStatePerEra st
      FlipTickedLedgerState x    = currentState current

  withLedgerTablesTicked st (LedgerTablesOne tables) =
      st{tickedHardForkLedgerStatePerEra = HardForkState $ TZ current'}
    where
      HardForkState (TZ current) = tickedHardForkLedgerStatePerEra st
      FlipTickedLedgerState x    = currentState current

      x'       = withLedgerTablesTicked x tables
      current' = current{currentState = FlipTickedLedgerState x'}

-- TODO @js: resurrect ShowLedgerState
-- instance ShowLedgerState (LedgerTables (LedgerState (HardForkBlock '[x])) WithLedgerTables) where
--   showsLedgerState sing =
--         showString "LedgerTablesOne"
--       . showSpace
--       . showParen True (showsLedgerState sing x)

instance
     ( TableStuff (LedgerState x) WithLedgerTables
     , Typeable mk
     , NoThunks (LedgerTables (LedgerState x) WithLedgerTables mk)
     )
  => NoThunks (LedgerTables (LedgerState (HardForkBlock '[x])) WithLedgerTables mk)

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance ( CanHardFork xs )
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

apply :: SingleEraBlock blk
      => Index xs                                           blk
      -> WrapLedgerConfig                                   blk
      -> Product I (FlipTickedLedgerState ValuesMK wt)         blk
      -> (    Except (HardForkLedgerError xs)
          :.: LedgerResult (LedgerState (HardForkBlock xs))
          :.: Flip2 LedgerState wt DiffMK
         )                                                  blk
apply index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
      Comp
    $ withExcept (injectLedgerError index)
    $ fmap (Comp . fmap Flip2 . embedLedgerResult (injectLedgerEvent index))
    $ applyBlockLedgerResult cfg block st

reapply :: SingleEraBlock blk
        => Index xs                                           blk
        -> WrapLedgerConfig                                   blk
        -> Product I (FlipTickedLedgerState ValuesMK wt)         blk
        -> (    LedgerResult (LedgerState (HardForkBlock xs))
            :.: Flip2 LedgerState wt DiffMK
           )                                                  blk
reapply index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
      Comp
    $ fmap Flip2
    $ embedLedgerResult (injectLedgerEvent index)
    $ reapplyBlockLedgerResult cfg block st

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

instance ( BlockSupportsProtocol  (HardForkBlock xs)
         , ApplyBlock (LedgerState (HardForkBlock xs)) (HardForkBlock xs)
         , ValidateEnvelope      (HardForkBlock xs)
         , CanHardFork xs
         ) => LedgerSupportsProtocol (HardForkBlock xs) where
  protocolLedgerView :: forall wt mk. LedgerConfig (HardForkBlock xs)
                     -> TickedLedgerState (HardForkBlock xs) wt mk
                     -> Ticked (LedgerView (BlockProtocol (HardForkBlock xs)))
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
                    -> FlipTickedLedgerState mk wt blk
                    -> (Ticked :.: WrapLedgerView) blk
      tickedViewOne cfg (FlipTickedLedgerState st) = Comp $
          WrapTickedLedgerView $
            protocolLedgerView (completeLedgerConfig' ei cfg) st

  ledgerViewForecastAt ::
       forall wt mk. LedgerConfig (HardForkBlock xs)
    -> LedgerState (HardForkBlock xs) wt mk
    -> Forecast (LedgerView (BlockProtocol (HardForkBlock xs)))
  ledgerViewForecastAt ledgerCfg@HardForkLedgerConfig{..}
                       (HardForkLedgerState ledgerSt) =
      mkHardForkForecast
        (InPairs.requiringBoth cfgs $ translateLedgerView hardForkEraTranslation)
        annForecast
    where
      ei    = State.epochInfoLedger ledgerCfg ledgerSt
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs

      annForecast :: HardForkState (AnnForecast LedgerState WrapLedgerView wt) xs
      annForecast = HardForkState $
          hczipWith3
            proxySingle
            forecastOne
            pcfgs
            (getExactly (History.getShape hardForkLedgerConfigShape))
            (getHardForkState ledgerSt)

      forecastOne ::
             forall blk. ( SingleEraBlock blk
                         , GetTip (LedgerState blk wt EmptyMK)
                         , TableStuff (LedgerState blk) wt)
          => WrapPartialLedgerConfig blk
          -> K EraParams blk
          -> Current (Flip2 LedgerState wt mk) blk
          -> Current (AnnForecast LedgerState WrapLedgerView wt) blk
      forecastOne cfg (K params) (Current start (Flip2 st)) = Current {
            currentStart = start
          , currentState = AnnForecast {
                annForecast      = mapForecast WrapTickedLedgerView $
                                     ledgerViewForecastAt cfg' st
              , annForecastState = forgetLedgerTables st
              , annForecastTip   = ledgerTipSlot $ forgetLedgerTables st
              , annForecastEnd   = History.mkUpperBound params start <$>
                                     singleEraTransition' cfg params start st
              }
          }
        where
          cfg' :: LedgerConfig blk
          cfg' = completeLedgerConfig' ei cfg

instance
     CanHardFork xs
  => StowableLedgerTables (LedgerState (HardForkBlock xs)) WithLedgerTables where
  stowLedgerTables =
        HardForkLedgerState
      . hcmap
          proxySingle
          (Flip2 . stowLedgerTables . unFlip2)
      . hardForkLedgerStatePerEra
  unstowLedgerTables =
        HardForkLedgerState
      . hcmap
          proxySingle
          (Flip2 . unstowLedgerTables . unFlip2)
      . hardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  Annotated forecasts
-------------------------------------------------------------------------------}

-- | Forecast annotated with details about the ledger it was derived from
data AnnForecast state view (wt :: SwitchLedgerTables) blk = AnnForecast {
      annForecast      :: Forecast (view blk)
    , annForecastState :: state blk wt EmptyMK
    , annForecastTip   :: WithOrigin SlotNo
    , annForecastEnd   :: Maybe Bound
    }

-- | Change a telescope of a forecast into a forecast of a telescope
mkHardForkForecast ::
     forall state view xs wt.
     SListI xs
  => InPairs (TranslateForecast state view) xs
  -> HardForkState (AnnForecast state view wt) xs
  -> Forecast (HardForkLedgerView_ view xs)
mkHardForkForecast translations st = Forecast {
      forecastAt  = hcollapse (hmap (K . forecastAt . annForecast) st)
    , forecastFor = \sno -> go sno translations (getHardForkState st)
    }
  where
    go :: SlotNo
       -> InPairs (TranslateForecast state view) xs'
       -> Telescope (K Past) (Current (AnnForecast state view wt)) xs'
       -> Except OutsideForecastRange (Ticked (HardForkLedgerView_ view xs'))
    go sno pairs        (TZ cur)       = oneForecast sno pairs cur
    go sno (PCons _ ts) (TS past rest) = shiftView past <$> go sno ts rest

oneForecast ::
     forall state view blk blks wt.
     SlotNo
  -> InPairs (TranslateForecast state view) (blk : blks)
     -- ^ this function uses at most the first translation
  -> Current (AnnForecast state view wt) blk
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
     forall wt xs mk1 mk2. CanHardFork xs
  => NP WrapPartialLedgerConfig xs
  -> NP (K EraParams) xs
  -> NP TopLevelConfig xs
  -> NS (Current (Flip2 LedgerState wt mk1)) xs
  -> NS (Current (Flip2 LedgerState wt mk2)) xs
  -> [LedgerEvent (HardForkBlock xs)]
inspectHardForkLedger = go
  where
    go :: forall xs'. All SingleEraBlock xs'
       => NP WrapPartialLedgerConfig xs'
       -> NP (K EraParams) xs'
       -> NP TopLevelConfig xs'
       -> NS (Current (Flip2 LedgerState wt mk1)) xs'
       -> NS (Current (Flip2 LedgerState wt mk2)) xs'
       -> [LedgerEvent (HardForkBlock xs')]

    go (pc :* _) (K ps :* pss) (c :* _) (Z before) (Z after) = concat [
          map liftEvent $
            inspectLedger
              c
              (unFlip2 $ currentState before)
              (unFlip2 $ currentState after)

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
                            (unFlip2 $ currentState before)
        confirmedAfter  = singleEraTransition
                            (unwrapPartialLedgerConfig pc)
                            ps
                            (currentStart after)
                            (unFlip2 $ currentState after)

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

ledgerInfo :: forall blk mk wt. SingleEraBlock blk
           => Current (FlipTickedLedgerState mk wt) blk -> LedgerEraInfo blk
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
-------------------------------------------------------------------------------}

instance CanHardFork xs
      => IgnoresMapKind (LedgerState (HardForkBlock xs)) where
  convertMapKind (HardForkLedgerState st) = HardForkLedgerState $ hcmap proxySingle (Flip2 . convertMapKind . unFlip2) st

instance CanHardFork xs
      => IgnoresMapKindTicked (LedgerState (HardForkBlock xs)) where
  convertMapKindTicked st = st { tickedHardForkLedgerStatePerEra = hcmap proxySingle (FlipTickedLedgerState . convertMapKindTicked . getFlipTickedLedgerState) $ tickedHardForkLedgerStatePerEra st }

instance CanHardFork xs => ExtractLedgerTables (LedgerState (HardForkBlock xs)) where
  extractLedgerTables (HardForkLedgerState st) = HardForkLedgerState $ hcmap proxySingle (Flip2 . extractLedgerTables . unFlip2) st
  destroyTables (HardForkLedgerState st) = HardForkLedgerState $ hcmap proxySingle (Flip2 . destroyTables . unFlip2) st
