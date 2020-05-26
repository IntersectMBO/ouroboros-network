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
    HardForkEnvelopeErr(..)
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.SOP.Strict
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
                     (HardForkEraLedgerView (..), HardForkLedgerView,
                     mkHardForkEraLedgerView)
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.State (Current (..),
                     HardForkState_ (..), Past (..), Snapshot (..),
                     TransitionOrTip (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
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
                                    getRawHash (Proxy @blk) h

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

  -- We can ignore the system start, because the 'SystemStart' of the first
  -- era is already known from the 'Bound's embedded in the 'HardForkState'.
  hardForkSummary _start cfg = State.reconstructSummaryLedger cfg
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
      case Match.matchNS hdr (hmap (Comp . Ticked slot) (State.tip hardForkView)) of
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
          => TopLevelConfig                                    blk
          -> Injection WrapEnvelopeErr xs                      blk
          -> Product Header (Ticked :.: HardForkEraLedgerView) blk
          -> K (Except (HardForkEnvelopeErr xs) ())            blk
      aux cfg injErr (Pair hdr (Comp view)) = K $
          withExcept injErr' $
            additionalEnvelopeChecks cfg (hardForkEraLedgerView <$> view) hdr
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
      State.bihczipWith (\_ _ -> K ()) (mkHardForkEraLedgerView ei) cfgs st
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoLedger ledgerCfg st

  ledgerViewForecastAt ledgerCfg@HardForkLedgerConfig{..} (HardForkLedgerState st) p = do
      st' <- State.retractToSlot p st
      f   <- hsequence' $ hczipWith proxySingle forecastOne cfgs st'
      return $ mkForecast
                 (translateLedgerView hardForkEraTranslation)
                 cfgs
                 (getHardForkState f)
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoLedger ledgerCfg st

      -- Change a telescope of a forecast into a forecast of a telescope
      mkForecast :: All SingleEraBlock xs'
                 => InPairs TranslateEraLedgerView                                    xs'
                 -> NP WrapPartialLedgerConfig                                        xs'
                 -> Telescope (Past g) (Current (Forecast :.: HardForkEraLedgerView)) xs'
                 -> Forecast (HardForkLedgerView                                      xs')
      mkForecast PNil _ (TZ (Current start (Comp f))) =
          forecastFinalEra start f
      mkForecast (PCons g _) (cfg :* cfg' :* _) (TZ (Current start f)) =
          forecastNotFinal g cfg cfg' start (unComp f)
      mkForecast (PCons _ gs) (_ :* cfgs') (TS past f) =
          shiftView past <$> mkForecast gs cfgs' f
      mkForecast PNil _ (TS _ f) =
          case f of {}

      shiftView :: Past g blk
                -> HardForkLedgerView (blk' : blks)
                -> HardForkLedgerView (blk : blk' : blks)
      shiftView past =
            HardForkState
          . TS (past { pastSnapshot = NoSnapshot })
          . getHardForkState

      -- We're in the final era. No translation required
      forecastFinalEra :: History.Bound
                       -> Forecast (HardForkEraLedgerView blk)
                       -> Forecast (HardForkLedgerView '[blk])
      forecastFinalEra start f =
          Forecast (forecastAt f) $ \slot ->
            aux <$> forecastFor f slot
        where
          aux :: HardForkEraLedgerView blk -> HardForkLedgerView '[blk]
          aux = HardForkState . TZ . Current start

      -- Make forecast with potential need to translate to next era
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
      forecastNotFinal :: forall blk blk' blks.
                          (SingleEraBlock blk, SingleEraBlock blk')
                       => TranslateEraLedgerView blk blk'
                       -> WrapPartialLedgerConfig blk
                       -> WrapPartialLedgerConfig blk'
                       -> History.Bound     -- Forecast era start
                       -> Forecast (HardForkEraLedgerView blk)
                       -> Forecast (HardForkLedgerView (blk ': blk' ': blks))
      forecastNotFinal g pcfg pcfg' start f =
          Forecast (forecastAt f) $ \slot ->
            translateIf slot <$> forecastFor f slot
        where
          cfg  :: LedgerConfig blk
          cfg' :: LedgerConfig blk'
          cfg  = completeLedgerConfig' ei pcfg
          cfg' = completeLedgerConfig' ei pcfg'

          -- Translate if the slot is past the end of the epoch
          translateIf :: SlotNo
                      -> HardForkEraLedgerView blk
                      -> HardForkLedgerView (blk ': blk' ': blks)
          translateIf slot view = HardForkState $
            case hardForkEraTransition view of
              TransitionAt tip epoch ->
                let end = History.mkUpperBound
                            (singleEraParams' pcfg)
                            start
                            epoch
                in if slot >= History.boundSlot end then
                     let view' :: HardForkEraLedgerView blk'
                         view' = HardForkEraLedgerView {
                                     hardForkEraLedgerView =
                                       translateLedgerViewWith g
                                         cfg
                                         cfg'
                                         (History.boundEpoch end)
                                         (hardForkEraLedgerView view)
                                   , hardForkEraTransition =
                                       -- We don't know the transition to the
                                       -- /next/ era, and it's important that
                                       -- any safe zones are applied to the
                                       -- tip in the /previous/ era.
                                       LedgerTip tip
                                   }
                     in TS (Past start end NoSnapshot) $
                        TZ (Current end view')
                   else
                     TZ (Current end view)
              LedgerTip _tip ->
                -- End of the era is not yet known. We don't have to worry about
                -- the safe zone here, because we are limited by whatever the
                -- range of the forecast is for this era.
                TZ (Current start view)

      -- | Forecast of a single era, as well as the end of that era (if any)
      forecastOne :: SingleEraBlock blk
                  => WrapPartialLedgerConfig                          blk
                  -> LedgerState                                      blk
                  -> (Maybe :.: (Forecast :.: HardForkEraLedgerView)) blk
      forecastOne pcfg st' = Comp $
          (Comp . fmap mkView) <$>
            ledgerViewForecastAt (completeLedgerConfig' ei pcfg) st' p
        where
          transition :: TransitionOrTip
          transition = State.transitionOrTip pcfg st'

          -- See comment of 'hardForkEraTransition' for justification of the
          -- use of @st'@ to determine the transition/tip.
          mkView :: LedgerView (BlockProtocol blk)
                 -> HardForkEraLedgerView blk
          mkView view = HardForkEraLedgerView {
                hardForkEraTransition = transition
              , hardForkEraLedgerView = view
              }

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => Current (Ticked :.: LedgerState) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

ledgerViewInfo :: forall blk. SingleEraBlock blk
               => (Ticked :.: HardForkEraLedgerView) blk -> LedgerEraInfo blk
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
