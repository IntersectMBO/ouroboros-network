{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
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
  , ShelleyTxOut (..)
  , forecastAcrossShelley
  , translateChainDepStateAcrossShelley
  , translateLedgerViewAcrossShelley
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Monad (guard)
import           Control.Monad.Except (runExcept, throwError, withExceptT)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Monoid as Monoid
import           Data.SOP.Strict
import qualified Data.SOP.Strict as SOP
import qualified Data.Text as T (pack)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.EpochInfo (hoistEpochInfo)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Flip (..))
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Ouroboros.Consensus.HardFork.History (Bound (boundSlot))
import           Ouroboros.Consensus.HardFork.Simple
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.TypeFamilyWrappers
import qualified Ouroboros.Consensus.Util.SOP as SOP

import qualified Cardano.Ledger.Core
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
  getEraParams =
        shelleyEraParamsNeverHardForks
      . shelleyLedgerGenesis
      . configLedger
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
     forall era proto mk. ShelleyCompatible proto era
  => PartialLedgerConfig (ShelleyBlock proto era)
  -> Word16   -- ^ Next era's major protocol version
  -> LedgerState (ShelleyBlock proto era) mk
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
     forall protoFrom protoTo eraFrom eraTo mk.
     ( TranslateProto protoFrom protoTo
     , LedgerSupportsProtocol (ShelleyBlock protoFrom eraFrom)
     )
  => ShelleyLedgerConfig eraFrom
  -> ShelleyLedgerConfig eraTo
  -> Bound  -- ^ Transition between the two eras
  -> SlotNo -- ^ Forecast for this slot
  -> LedgerState (ShelleyBlock protoFrom eraFrom) mk
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
         , ShelleyBasedEra (SL.PreviousEra era)
         , SL.TranslateEra era (ShelleyTip proto)
         , SL.TranslateEra era SL.NewEpochState
         , SL.TranslateEra era TxOutWrapper
         , SL.TranslationError era SL.NewEpochState ~ Void
         , SL.TranslationError era TxOutWrapper ~ Void
         , EraCrypto (SL.PreviousEra era) ~ EraCrypto era
         ) => SL.TranslateEra era (Flip LedgerState (ApplyMapKind' mk) :.: ShelleyBlock proto) where
  translateEra ctxt (Comp (Flip (ShelleyLedgerState tip state _transition tables))) = do
      tip'   <- mapM (SL.translateEra ctxt) tip
      state' <- SL.translateEra ctxt state
      return $ Comp $ Flip $ ShelleyLedgerState {
          shelleyLedgerTip        = tip'
        , shelleyLedgerState      = state'
        , shelleyLedgerTransition = ShelleyTransitionInfo 0
        , shelleyLedgerTables     = translateShelleyTables ctxt tables
        }

translateShelleyTables ::
     ( SL.TranslateEra     era TxOutWrapper
     , SL.TranslationError era TxOutWrapper ~ Void
     , EraCrypto (SL.PreviousEra era) ~ EraCrypto era
     , Eq (Cardano.Ledger.Core.TxOut (SL.PreviousEra era))
     , Eq (Cardano.Ledger.Core.TxOut era)
     )
  => SL.TranslationContext era
  -> LedgerTables (LedgerState (ShelleyBlock proto (SL.PreviousEra era))) (ApplyMapKind' mk)
  -> LedgerTables (LedgerState (ShelleyBlock proto                 era))  (ApplyMapKind' mk)
translateShelleyTables ctxt (ShelleyLedgerTables utxoTable) =
      ShelleyLedgerTables
    $ mapValuesAppliedMK
        (unTxOutWrapper . SL.translateEra' ctxt . TxOutWrapper)
        utxoTable

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

{-------------------------------------------------------------------------------
  A wrapper helpful for Ledger HD
-------------------------------------------------------------------------------}

-- | We use this type for clarity, and because we don't want to declare
-- 'FromCBOR' and 'ToCBOR' for 'NS'; serializations of sum involves design
-- decisions that cannot be captured by an all-purpose 'NS' instance
newtype ShelleyTxOut eras =
    ShelleyTxOut {unShelleyTxOut :: NS TxOutWrapper eras}
  deriving (Generic)

-- TODO Can't reuse the 'NS' instance because of its use of 'SOP.Compose', so I
-- inlined it
instance SOP.All ShelleyBasedEra eras => Eq       (ShelleyTxOut eras) where
  ShelleyTxOut (SOP.Z l) == ShelleyTxOut (SOP.Z r) = l == r
  ShelleyTxOut (SOP.S l) == ShelleyTxOut (SOP.S r) = ShelleyTxOut l == ShelleyTxOut r
  _                      == _                      = False

-- TODO Can't reuse the 'NS' instance because of its use of 'SOP.Compose', so I
-- inlined it
instance SOP.All ShelleyBasedEra eras => NoThunks (ShelleyTxOut eras) where
  wNoThunks ctxt = (. unShelleyTxOut) $ \case
      Z l -> noThunks ("Z" : ctxt) l
      S r -> noThunks ("S" : ctxt) (ShelleyTxOut r)

-- TODO Can't reuse the 'NS' instance because of its use of 'SOP.Compose', so I
-- inlined it
instance SOP.All ShelleyBasedEra eras => Show (ShelleyTxOut eras) where
  showsPrec =
      \p (ShelleyTxOut ns) -> showParen (p > 10) $ showString "ShelleyTxOut " . go ns
    where
      go :: SOP.All ShelleyBasedEra eras' => NS TxOutWrapper eras' -> ShowS
      go = showParen True . \case
        Z l -> showString "Z " . shows l
        S r -> showString "S " . go r

-- unline SOP.nsToIndex, this is not restricted to the interval [0, 24)
idxLength :: SOP.Index xs x -> Int
idxLength = \case
    SOP.IZ     -> 0
    SOP.IS idx -> 1 + idxLength idx

instance (SOP.All ShelleyBasedEra eras, Typeable eras) => ToCBOR (ShelleyTxOut eras) where
  toCBOR (ShelleyTxOut x) =
        SOP.hcollapse
      $ SOP.hcimap (Proxy @ShelleyBasedEra) each x
    where
      each ::
           ShelleyBasedEra era
        => SOP.Index eras era
        -> TxOutWrapper era
        -> SOP.K CBOR.Encoding era
      each idx (TxOutWrapper txout) = SOP.K $
           CBOR.encodeListLen 2
        <> CBOR.encodeWord (toEnum (idxLength idx))
        <> toCBOR txout

instance (SOP.All ShelleyBasedEra eras, Typeable eras) => FromCBOR (ShelleyTxOut eras) where
  fromCBOR = do
      CBOR.decodeListLenOf 2
      tag <- CBOR.decodeWord
      let aDecoder =
              mconcat
            $ SOP.hcollapse
            $ SOP.hcmap
                (Proxy @ShelleyBasedEra)
                each
                (SOP.indices @eras)
      case Monoid.getFirst $ aDecoder tag of
        Nothing -> error $ "FromCBOR ShelleyTxOut, unknown tag: " <> show tag
        Just x  -> unADecoder x
    where
      each ::
           ShelleyBasedEra x
        => SOP.Index eras x
        -> SOP.K (Word -> Monoid.First (ADecoder eras)) x
      each idx = SOP.K $ \w -> Monoid.First $
        if w /= toEnum (idxLength idx) then Nothing else
        Just
          $ ADecoder
          $ (ShelleyTxOut . SOP.injectNS idx . TxOutWrapper) <$> fromCBOR

newtype ADecoder eras =
  ADecoder {unADecoder :: forall s. CBOR.Decoder s (ShelleyTxOut eras)}
