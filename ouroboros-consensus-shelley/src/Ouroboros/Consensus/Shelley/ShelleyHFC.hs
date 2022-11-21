{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is the Shelley Hard Fork Combinator
module Ouroboros.Consensus.Shelley.ShelleyHFC (
    ProtocolShelley
  , ShelleyBlockHFC
  , ShelleyPartialLedgerConfig (..)
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  , translateLedgerViewAcrossShelley
  ) where

import           Control.Monad (guard)
import           Control.Monad.Except (runExcept, throwError, withExceptT)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.SOP.Strict
import qualified Data.Text as T (pack)
import           Data.Void (Void)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.EpochInfo (hoistEpochInfo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot))
import           Ouroboros.Consensus.HardFork.Simple
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.TypeFamilyWrappers

import qualified Cardano.Ledger.Era as SL
import qualified Cardano.Ledger.Shelley.API as SL

import qualified Cardano.Protocol.TPraos.API as SL
import qualified Ouroboros.Consensus.Forecast as Forecast
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol, ledgerViewForecastAt)
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.TPraos hiding (PraosCrypto)
import           Ouroboros.Consensus.Protocol.Translate (TranslateProto)
import qualified Ouroboros.Consensus.Protocol.Translate as Proto
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect as Shelley.Inspect
import           Ouroboros.Consensus.Shelley.Node ()

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Shelley as the single era in the hard fork combinator
type ShelleyBlockHFC proto era = HardForkBlock '[ShelleyBlock proto era]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  ) => NoHardForks (ShelleyBlock proto era) where
  getEraParams topLevelConfig = shelleyEraParamsNeverHardForks epochSize slotLength
    where ledgerConfig = configLedger topLevelConfig
          epochSize = shelleyLedgerEpochSize ledgerConfig
          slotLength = shelleyLedgerSlotLength ledgerConfig

  toPartialLedgerConfig _ cfg = ShelleyPartialLedgerConfig {
        shelleyLedgerConfig    = cfg
      , shelleyTriggerHardFork = TriggerHardForkNever
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | Forward to the ShelleyBlock instance. Only supports
-- 'HardForkNodeToNodeDisabled', which is compatible with nodes running with
-- 'ShelleyBlock'.
instance (ShelleyCompatible proto era, LedgerSupportsProtocol (ShelleyBlock proto era))
      => SupportedNetworkProtocolVersion (ShelleyBlockHFC proto era) where
  supportedNodeToNodeVersions _ =
      Map.map HardForkNodeToNodeDisabled $
      supportedNodeToNodeVersions (Proxy @(ShelleyBlock proto era))

  supportedNodeToClientVersions _ =
      Map.map HardForkNodeToClientDisabled $
      supportedNodeToClientVersions (Proxy @(ShelleyBlock proto era))

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

-- | Use the default implementations. This means the serialisation of blocks
-- includes an era wrapper. Each block should do this from the start to be
-- prepared for future hard forks without having to do any bit twiddling.
instance (ShelleyCompatible proto era, LedgerSupportsProtocol (ShelleyBlock proto era))
 => SerialiseHFC '[ShelleyBlock proto era]
instance (ShelleyCompatible proto era, LedgerSupportsProtocol (ShelleyBlock proto era))
  => SerialiseConstraintsHFC (ShelleyBlock proto era)

{-------------------------------------------------------------------------------
  Protocol type definition
-------------------------------------------------------------------------------}

type ProtocolShelley = HardForkProtocol '[ ShelleyBlock (TPraos StandardCrypto) StandardShelley ]

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

shelleyTransition ::
     forall era proto. ShelleyCompatible proto era
  => PartialLedgerConfig (ShelleyBlock proto era)
  -> Word16   -- ^ Next era's major protocol version
  -> LedgerState (ShelleyBlock proto era)
  -> Maybe EpochNo
shelleyTransition ShelleyPartialLedgerConfig{..}
                  transitionMajorVersion
                  state =
      takeAny
    . mapMaybe isTransition
    . Shelley.Inspect.protocolUpdates shelleyLedgerConfig
    $ state
  where
    ShelleyTransitionInfo{..} = shelleyLedgerTransition state

    -- 'shelleyLedgerConfig' contains a dummy 'EpochInfo' but this does not
    -- matter for extracting the genesis config

    k :: Word64
    k = SL.securityParameter $ shelleyLedgerGlobals shelleyLedgerConfig

    isTransition :: Shelley.Inspect.ProtocolUpdate era -> Maybe EpochNo
    isTransition Shelley.Inspect.ProtocolUpdate{..} = do
         SL.ProtVer major _minor <- proposalVersion
         guard $ fromIntegral major == transitionMajorVersion
         guard $ proposalReachedQuorum
         guard $ shelleyAfterVoting >= fromIntegral k
         return proposalEpoch
       where
         Shelley.Inspect.UpdateProposal{..} = protocolUpdateProposal
         Shelley.Inspect.UpdateState{..}    = protocolUpdateState

    -- In principle there could be multiple proposals that all change the
    -- major protocol version. In practice this can't happen because each
    -- delegate can only vote for one proposal, but the types don't guarantee
    -- this. We don't need to worry about this, and just pick any of them.
    takeAny :: [a] -> Maybe a
    takeAny = listToMaybe

instance
  ( ShelleyCompatible proto era,
    LedgerSupportsProtocol (ShelleyBlock proto era)
  ) => SingleEraBlock (ShelleyBlock proto era) where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      -- TODO: We might be evaluating 'singleEraTransition' more than once when
      -- replaying blocks. We should investigate if this is the case, and if so,
      -- whether this is the desired behaviour. If it is not, then we need to
      -- fix it.
      --
      -- For evidence of this behaviour, replace the cased-on expression by:
      -- > @traceShowId $ shelleyTriggerHardFork pcf@
      case shelleyTriggerHardFork pcfg of
        TriggerHardForkNever                         -> Nothing
        TriggerHardForkAtEpoch   epoch               -> Just epoch
        TriggerHardForkAtVersion shelleyMajorVersion ->
            shelleyTransition
              pcfg
              shelleyMajorVersion
              ledgerState

  singleEraInfo _ = SingleEraInfo {
      singleEraName = shelleyBasedEraName (Proxy @era)
    }

instance PraosCrypto c => HasPartialConsensusConfig (Praos c) where
  type PartialConsensusConfig (Praos c) = PraosParams

  completeConsensusConfig _ praosEpochInfo praosParams = PraosConfig {..}

  toPartialConsensusConfig _ = praosParams

instance SL.PraosCrypto c => HasPartialConsensusConfig (TPraos c) where
  type PartialConsensusConfig (TPraos c) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

  toPartialConsensusConfig _ = tpraosParams

data ShelleyPartialLedgerConfig era = ShelleyPartialLedgerConfig {
      -- | We cache the non-partial ledger config containing a dummy
      -- 'EpochInfo' that needs to be replaced with the correct one.
      --
      -- We do this to avoid recomputing the ledger config each time
      -- 'completeLedgerConfig' is called, as 'mkShelleyLedgerConfig' does
      -- some rather expensive computations that shouldn't be repeated too
      -- often (e.g., 'sgActiveSlotCoeff').
      shelleyLedgerConfig    :: !(ShelleyLedgerConfig era)
    , shelleyTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic, NoThunks)

instance ShelleyCompatible proto era => HasPartialLedgerConfig (ShelleyBlock proto era) where
  type PartialLedgerConfig (ShelleyBlock proto era) = ShelleyPartialLedgerConfig era

  -- Replace the dummy 'EpochInfo' with the real one
  completeLedgerConfig _ epochInfo (ShelleyPartialLedgerConfig cfg _) =
      cfg {
          shelleyLedgerGlobals = (shelleyLedgerGlobals cfg) {
              SL.epochInfo =
                  hoistEpochInfo
                    (runExcept . withExceptT (T.pack . show))
                    epochInfo
            }
        }

-- | Forecast from a Shelley-based era to the next Shelley-based era.
forecastAcrossShelley ::
     forall protoFrom protoTo eraFrom eraTo.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => ShelleyLedgerConfig eraFrom
  -> ShelleyLedgerConfig eraTo
  -> Bound  -- ^ Transition between the two eras
  -> SlotNo -- ^ Forecast for this slot
  -> LedgerState (ShelleyBlock protoFrom eraFrom)
  -> Except OutsideForecastRange (Ticked (WrapLedgerView (ShelleyBlock protoTo eraTo)))
forecastAcrossShelley cfgFrom cfgTo transition forecastFor ledgerStateFrom
    | forecastFor < maxFor
    = return $ futureLedgerView forecastFor
    | otherwise
    = throwError $ OutsideForecastRange {
          outsideForecastAt     = ledgerTipSlot ledgerStateFrom
        , outsideForecastMaxFor = maxFor
        , outsideForecastFor    = forecastFor
        }
  where
    -- | 'SL.futureLedgerView' imposes its own bounds. Those bounds could
    -- /exceed/ the 'maxFor' we have computed, but should never be /less/.
    futureLedgerView :: SlotNo -> Ticked (WrapLedgerView (ShelleyBlock protoTo era))
    futureLedgerView =
          WrapTickedLedgerView
        . either
            (\e -> error ("futureLedgerView failed: " <> show e))
            (Proto.translateTickedLedgerView @protoFrom @protoTo)
        . runExcept
        . Forecast.forecastFor (ledgerViewForecastAt cfgFrom ledgerStateFrom)

    -- Exclusive upper bound
    maxFor :: SlotNo
    maxFor = crossEraForecastBound
               (ledgerTipSlot ledgerStateFrom)
               (boundSlot transition)
               (SL.stabilityWindow (shelleyLedgerGlobals cfgFrom))
               (SL.stabilityWindow (shelleyLedgerGlobals cfgTo))

translateChainDepStateAcrossShelley ::
     forall eraFrom eraTo protoFrom protoTo.
     ( TranslateProto protoFrom protoTo
     )
  => RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       (ShelleyBlock protoFrom eraFrom)
       (ShelleyBlock protoTo eraTo)
translateChainDepStateAcrossShelley =
    ignoringBoth $
      Translate $ \_epochNo (WrapChainDepState chainDepState) ->
        -- Same protocol, same 'ChainDepState'. Note that we don't have to apply
        -- any changes related to an epoch transition, this is already done when
        -- ticking the state.
        WrapChainDepState $ Proto.translateChainDepState @protoFrom @protoTo chainDepState

translateLedgerViewAcrossShelley ::
     forall eraFrom eraTo protoFrom protoTo.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => RequiringBoth
       WrapLedgerConfig
       (TranslateForecast LedgerState WrapLedgerView)
       (ShelleyBlock protoFrom eraFrom)
       (ShelleyBlock protoTo eraTo)
translateLedgerViewAcrossShelley =
    RequireBoth $ \(WrapLedgerConfig cfgFrom)
                   (WrapLedgerConfig cfgTo) ->
      TranslateForecast $ forecastAcrossShelley cfgFrom cfgTo

{-------------------------------------------------------------------------------
  Translation from one Shelley-based era to another Shelley-based era
-------------------------------------------------------------------------------}

instance ( ShelleyBasedEra era
         , ShelleyBasedEra (SL.PreviousEra era)
         , EraCrypto (SL.PreviousEra era) ~ EraCrypto era
         ) => SL.TranslateEra era (ShelleyTip proto) where
  translateEra _ (ShelleyTip sno bno (ShelleyHash hash)) =
      return $ ShelleyTip sno bno (ShelleyHash hash)

instance ( ShelleyBasedEra era
         , SL.TranslateEra era (ShelleyTip proto)
         , SL.TranslateEra era SL.NewEpochState
         , SL.TranslationError era SL.NewEpochState ~ Void
         ) => SL.TranslateEra era (LedgerState :.: ShelleyBlock proto) where
  translateEra ctxt (Comp (ShelleyLedgerState tip state _transition)) = do
      tip'   <- mapM (SL.translateEra ctxt) tip
      state' <- SL.translateEra ctxt state
      return $ Comp $ ShelleyLedgerState {
          shelleyLedgerTip        = tip'
        , shelleyLedgerState      = state'
        , shelleyLedgerTransition = ShelleyTransitionInfo 0
        }

instance ( ShelleyBasedEra era
         , SL.TranslateEra era WrapTx
         ) => SL.TranslateEra era (GenTx :.: ShelleyBlock proto) where
  type TranslationError era (GenTx :.: ShelleyBlock proto) = SL.TranslationError era WrapTx
  translateEra ctxt (Comp (ShelleyTx _txId tx)) =
        Comp . mkShelleyTx . unwrapTx @era
    <$> SL.translateEra ctxt (WrapTx @(SL.PreviousEra era) tx)

instance ( ShelleyBasedEra era
         , SL.TranslateEra era WrapTx
         ) => SL.TranslateEra era (WrapValidatedGenTx :.: ShelleyBlock proto) where
  type TranslationError era (WrapValidatedGenTx :.: ShelleyBlock proto) = SL.TranslationError era WrapTx
  translateEra ctxt (Comp (WrapValidatedGenTx (ShelleyValidatedTx _txId vtx))) =
        Comp . WrapValidatedGenTx
      . mkShelleyValidatedTx . SL.coerceValidated
    <$> SL.translateValidated @era @WrapTx ctxt (SL.coerceValidated vtx)
