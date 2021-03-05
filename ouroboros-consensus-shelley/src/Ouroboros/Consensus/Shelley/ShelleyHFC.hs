{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.ShelleyHFC (
    ShelleyBlockHFC
  , ShelleyPartialLedgerConfig (..)
  , ProtocolShelley
  , RunProtocol(..)
  , RunProtocolClient(..)
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  , translateLedgerViewAcrossShelley
  ) where

import           Control.Monad (guard)
import           Control.Monad.Except (Except, throwError)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.SOP.Strict
import           Data.Void (Void)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Node.NetworkProtocolVersion

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Ledger.Inspect as Shelley.Inspect
import           Ouroboros.Consensus.Shelley.Node (ProtocolParamsShelley, ProtocolParamsShelleyBased, protocolClientInfoShelley, protocolInfoShelley)
import           Ouroboros.Consensus.Shelley.Protocol

import           Ouroboros.Consensus.Util.IOLike (IOLike)

import qualified Cardano.Ledger.Era as SL
import qualified Shelley.Spec.Ledger.API as SL

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Shelley as the single era in the hard fork combinator
type ShelleyBlockHFC era = HardForkBlock '[ShelleyBlock era]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance ShelleyBasedEra era => NoHardForks (ShelleyBlock era) where
  getEraParams =
        shelleyEraParamsNeverHardForks
      . shelleyLedgerGenesis
      . configLedger
  toPartialConsensusConfig _  = tpraosParams
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
instance ShelleyBasedEra era
      => SupportedNetworkProtocolVersion (ShelleyBlockHFC era) where
  supportedNodeToNodeVersions _ =
      Map.map HardForkNodeToNodeDisabled $
      supportedNodeToNodeVersions (Proxy @(ShelleyBlock era))

  supportedNodeToClientVersions _ =
      Map.map HardForkNodeToClientDisabled $
      supportedNodeToClientVersions (Proxy @(ShelleyBlock era))

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

-- | Use the default implementations. This means the serialisation of blocks
-- includes an era wrapper. Each block should do this from the start to be
-- prepared for future hard forks without having to do any bit twiddling.
instance ShelleyBasedEra era => SerialiseHFC '[ShelleyBlock era]
instance ShelleyBasedEra era => SerialiseConstraintsHFC (ShelleyBlock era)

{-------------------------------------------------------------------------------
  Protocol Instances
-------------------------------------------------------------------------------}

type ProtocolShelley = HardForkProtocol '[ ShelleyBlock StandardShelley ]

-- | Run TPraos against the Shelley ledger
instance IOLike m => Protocol m (ShelleyBlockHFC StandardShelley) ProtocolShelley where
  data RunProtocol m (ShelleyBlockHFC StandardShelley) ProtocolShelley = RunProtocolShelley
    (ProtocolParamsShelleyBased StandardShelley)
    (ProtocolParamsShelley)
  protocolInfo (RunProtocolShelley paramsShelleyBased paramsShelley) =
    inject $ protocolInfoShelley paramsShelleyBased paramsShelley

instance ProtocolClient (ShelleyBlockHFC StandardShelley) ProtocolShelley where
  data RunProtocolClient (ShelleyBlockHFC StandardShelley) ProtocolShelley =
    RunProtocolClientShelley
  protocolClientInfo RunProtocolClientShelley =
    inject $ protocolClientInfoShelley

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

shelleyTransition ::
     forall era. ShelleyBasedEra era
  => PartialLedgerConfig (ShelleyBlock era)
  -> Word16   -- ^ Next era's major protocol version
  -> LedgerState (ShelleyBlock era)
  -> Maybe EpochNo
shelleyTransition ShelleyPartialLedgerConfig{..}
                  transitionMajorVersion
                  state =
      takeAny
    . mapMaybe isTransition
    . Shelley.Inspect.protocolUpdates genesis
    $ state
  where
    ShelleyTransitionInfo{..} = shelleyLedgerTransition state

    -- 'shelleyLedgerConfig' contains a dummy 'EpochInfo' but this does not
    -- matter for extracting the genesis config
    genesis :: SL.ShelleyGenesis era
    genesis = shelleyLedgerGenesis shelleyLedgerConfig

    k :: Word64
    k = SL.sgSecurityParam genesis

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

instance ShelleyBasedEra era => SingleEraBlock (ShelleyBlock era) where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
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

instance PraosCrypto c => HasPartialConsensusConfig (TPraos c) where
  type PartialConsensusConfig (TPraos c) = TPraosParams

  completeConsensusConfig _ tpraosEpochInfo tpraosParams = TPraosConfig {..}

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

instance ShelleyBasedEra era => HasPartialLedgerConfig (ShelleyBlock era) where
  type PartialLedgerConfig (ShelleyBlock era) = ShelleyPartialLedgerConfig era

  -- Replace the dummy 'EpochInfo' with the real one
  completeLedgerConfig _ epochInfo (ShelleyPartialLedgerConfig cfg _) =
      cfg {
          shelleyLedgerGlobals = (shelleyLedgerGlobals cfg) {
              SL.epochInfo = epochInfo
            }
        }

-- | Forecast from a Shelley-based era to the next Shelley-based era.
forecastAcrossShelley ::
     forall eraFrom eraTo.
     ( EraCrypto eraFrom ~ EraCrypto eraTo
     , ShelleyBasedEra eraFrom
     )
  => ShelleyLedgerConfig eraFrom
  -> ShelleyLedgerConfig eraTo
  -> Bound  -- ^ Transition between the two eras
  -> SlotNo -- ^ Forecast for this slot
  -> LedgerState (ShelleyBlock eraFrom)
  -> Except OutsideForecastRange (Ticked (WrapLedgerView (ShelleyBlock eraTo)))
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
    futureLedgerView :: SlotNo -> Ticked (WrapLedgerView (ShelleyBlock eraTo))
    futureLedgerView =
          WrapTickedLedgerView
        . TickedPraosLedgerView
        . either
            (\e -> error ("futureLedgerView failed: " <> show e))
            id
        . SL.futureLedgerView
            (shelleyLedgerGlobals cfgFrom)
            (shelleyLedgerState ledgerStateFrom)

    -- Exclusive upper bound
    maxFor :: SlotNo
    maxFor = crossEraForecastBound
               (ledgerTipSlot ledgerStateFrom)
               (boundSlot transition)
               (SL.stabilityWindow (shelleyLedgerGlobals cfgFrom))
               (SL.stabilityWindow (shelleyLedgerGlobals cfgTo))

translateChainDepStateAcrossShelley ::
     forall eraFrom eraTo.
     EraCrypto eraFrom ~ EraCrypto eraTo
  => RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       (ShelleyBlock eraFrom)
       (ShelleyBlock eraTo)
translateChainDepStateAcrossShelley =
    ignoringBoth $
      Translate $ \_epochNo (WrapChainDepState chainDepState) ->
        -- Same protocol, same 'ChainDepState'. Note that we don't have to apply
        -- any changes related to an epoch transition, this is already done when
        -- ticking the state.
        WrapChainDepState chainDepState

translateLedgerViewAcrossShelley ::
     forall eraFrom eraTo.
     ( EraCrypto eraFrom ~ EraCrypto eraTo
     , ShelleyBasedEra eraFrom
     )
  => RequiringBoth
       WrapLedgerConfig
       (TranslateForecast LedgerState WrapLedgerView)
       (ShelleyBlock eraFrom)
       (ShelleyBlock eraTo)
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
         ) => SL.TranslateEra era ShelleyTip where
  translateEra _ (ShelleyTip sno bno (ShelleyHash hash)) =
      return $ ShelleyTip sno bno (ShelleyHash hash)

instance ( ShelleyBasedEra era
         , SL.TranslateEra era ShelleyTip
         , SL.TranslateEra era SL.NewEpochState
         , SL.TranslationError era SL.NewEpochState ~ Void
         ) => SL.TranslateEra era (LedgerState :.: ShelleyBlock) where
  translateEra ctxt (Comp (ShelleyLedgerState tip state _transition)) = do
      tip'   <- mapM (SL.translateEra ctxt) tip
      state' <- SL.translateEra ctxt state
      return $ Comp $ ShelleyLedgerState {
          shelleyLedgerTip        = tip'
        , shelleyLedgerState      = state'
        , shelleyLedgerTransition = ShelleyTransitionInfo 0
        }

instance ( ShelleyBasedEra era
         , SL.TranslateEra era SL.Tx
         ) => SL.TranslateEra era (GenTx :.: ShelleyBlock) where
  type TranslationError era (GenTx :.: ShelleyBlock) = SL.TranslationError era SL.Tx
  translateEra ctxt (Comp (ShelleyTx _txId tx)) =
    -- TODO will the txId stay the same? If so, we could avoid recomputing it
    Comp . mkShelleyTx <$> SL.translateEra ctxt tx
