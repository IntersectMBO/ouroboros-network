{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test infrastructure to test hard-forking from one Shelley-based era to
-- another, e.g., Shelley to Allegra.
module Test.ThreadNet.Infra.ShelleyBasedHardFork (
    -- * Blocks
    ShelleyBasedHardForkBlock
  , ShelleyBasedHardForkEras
    -- * Transactions
  , pattern GenTxShelley1
  , pattern GenTxShelley2
    -- * Node
  , ShelleyBasedHardForkConstraints
  , protocolInfoShelleyBasedHardFork
    -- * Data families
  , LedgerTables (..)
  ) where

import           Control.Monad.Except (runExcept)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (Proxy))
import           Data.SOP.Strict (NP (..), NS (..), type (-.->), unComp,
                     (:.:) (..))
import qualified Data.SOP.Strict as SOP
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Binary (fromCBOR, toCBOR)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.SOP as SOP

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Binary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import           Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Flip (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as SL
import qualified Cardano.Ledger.Shelley.API as SL

import           Ouroboros.Consensus.Mempool.TxLimits (TxLimits)
import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.ShelleyHFC (ShelleyTxOut (..))

import           Ouroboros.Consensus.Cardano.CanHardFork (IsShelleyTele (..),
                     ShelleyPartialLedgerConfig (..), forecastAcrossShelley,
                     translateChainDepStateAcrossShelley)
import           Ouroboros.Consensus.Cardano.Node
                     (ProtocolTransitionParamsShelleyBased (..),
                     TriggerHardFork (..))

import           Ouroboros.Consensus.Util.SOP
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.TxGen.Shelley ()

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

-- | Two eras, both Shelley-based.
type ShelleyBasedHardForkEras proto1 era1 proto2 era2 =
    '[ShelleyBlock proto1 era1, ShelleyBlock proto2 era2]

type ShelleyBasedHardForkBlock proto1 era1 proto2 era2 =
  HardForkBlock (ShelleyBasedHardForkEras proto1 era1 proto2 era2)

{-------------------------------------------------------------------------------
  Pattern synonyms, for encapsulation and legibility
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkGenTx proto1 era1 proto2 era2 =
  GenTx (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)

pattern GenTxShelley1 ::
     GenTx (ShelleyBlock proto1 era1)
  -> ShelleyBasedHardForkGenTx proto1 era1 proto2 era2
pattern GenTxShelley1 tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley2 ::
     GenTx (ShelleyBlock proto2 era2)
  -> ShelleyBasedHardForkGenTx proto1 era1 proto2 era2
pattern GenTxShelley2 tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley1, GenTxShelley2 #-}

pattern ShelleyBasedHardForkNodeToNodeVersion1 ::
     BlockNodeToNodeVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
pattern ShelleyBasedHardForkNodeToNodeVersion1 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* Nil
      )

pattern ShelleyBasedHardForkNodeToClientVersion1 ::
     BlockNodeToClientVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
pattern ShelleyBasedHardForkNodeToClientVersion1 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* EraNodeToClientEnabled ShelleyNodeToClientVersion2
      :* Nil
      )

{-------------------------------------------------------------------------------
  Consensus instances
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 =
  ( ShelleyCompatible proto1 era1
  , ShelleyCompatible proto2 era2
  , LedgerSupportsProtocol (ShelleyBlock proto1 era1)
  , LedgerSupportsProtocol (ShelleyBlock proto2 era2)
  , TxLimits (ShelleyBlock proto1 era1)
  , TxLimits (ShelleyBlock proto2 era2)
  , SL.PreviousEra era2 ~ era1

  , SL.TranslateEra       era2 SL.NewEpochState
  , SL.TranslateEra       era2 SL.ShelleyGenesis
  , SL.TranslateEra       era2 WrapTx
  , SL.TranslateEra       era2 TxOutWrapper

  , SL.TranslationError   era2 SL.NewEpochState  ~ Void
  , SL.TranslationError   era2 SL.ShelleyGenesis ~ Void
  , SL.TranslationError   era2 TxOutWrapper      ~ Void

  , SL.TranslationContext era1 ~ ()
    -- At the moment, fix the protocols together
  , EraCrypto era1 ~ EraCrypto era2
  , PraosCrypto (EraCrypto era1)
  , proto1 ~ TPraos (EraCrypto era1)
  , proto1 ~ proto2
  )

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => SerialiseHFC (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
   -- use defaults

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => CanHardFork (ShelleyBasedHardForkEras proto1 era1 proto2 era2) where
  hardForkEraTranslation = EraTranslation {
        translateLedgerState   = PCons translateLedgerState                PNil
      , translateChainDepState = PCons translateChainDepStateAcrossShelley PNil
      , translateLedgerView    = PCons translateLedgerView                 PNil
      }
    where
      translateLedgerState ::
           InPairs.RequiringBoth
             WrapLedgerConfig
             TranslateLedgerState
             (ShelleyBlock proto1 era1)
             (ShelleyBlock proto2 era2)
      translateLedgerState =
          InPairs.RequireBoth
        $ \_cfg1 cfg2 ->
          HFC.TranslateLedgerState {
            translateLedgerStateWith =  \_epochNo ->
                noNewTickingDiffs
              . unFlip
              . unComp
              . SL.translateEra'
                  (shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2))
              . Comp
              . Flip
          , translateLedgerTablesWith =
            \ShelleyLedgerTables { shelleyUTxOTable = diffMK } -> ShelleyLedgerTables {
                shelleyUTxOTable = fmap
                                      ( unTxOutWrapper
                                      . SL.translateEra' (shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2))
                                      . TxOutWrapper
                                      )
                                      diffMK
              }
        }

      translateLedgerView ::
           InPairs.RequiringBoth
              WrapLedgerConfig
              (HFC.TranslateForecast LedgerState WrapLedgerView)
              (ShelleyBlock proto1 era1)
              (ShelleyBlock proto2 era2)
      translateLedgerView =
          InPairs.RequireBoth $ \(WrapLedgerConfig cfg1) (WrapLedgerConfig cfg2) ->
            HFC.TranslateForecast $ forecastAcrossShelley cfg1 cfg2

  hardForkChainSel = Tails.mk2 CompareSameSelectView

  hardForkInjectTxs =
        InPairs.mk2
      $ InPairs.RequireBoth $ \_cfg1 cfg2 ->
        let ctxt = shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2)
        in
          Pair2
            (InjectTx          (translateTx          ctxt))
            (InjectValidatedTx (translateValidatedTx ctxt))
    where
      translateTx ::
           SL.TranslationContext era2
        ->        GenTx (ShelleyBlock proto era1)
        -> Maybe (GenTx (ShelleyBlock proto era2))
      translateTx transCtxt =
          fmap unComp
        . eitherToMaybe . runExcept . SL.translateEra transCtxt
        . Comp

      translateValidatedTx ::
           SL.TranslationContext era2
        ->        WrapValidatedGenTx (ShelleyBlock proto era1)
        -> Maybe (WrapValidatedGenTx (ShelleyBlock proto era2))
      translateValidatedTx transCtxt =
            fmap unComp
          . eitherToMaybe . runExcept . SL.translateEra transCtxt
          . Comp

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => SupportedNetworkProtocolVersion (ShelleyBasedHardForkBlock proto1 era1 proto2 era2) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToNodeVersion1)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToClientVersion1)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  TableStuff
-------------------------------------------------------------------------------}

projectLedgerTablesHelper :: forall proto1 era1 proto2 era2 fmk mk.
     (ShelleyBasedHardForkConstraints proto1 era1 proto2 era2, IsApplyMapKind mk)
  => (forall x.
         TickedTableStuff (LedgerState x)
      => fmk x -> LedgerTables (LedgerState x) mk
     )
  -> HardForkState fmk (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  -> LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) mk
projectLedgerTablesHelper prj (HardForkState tele) =
      SOP.hcollapse
    $ SOP.hcimap
        (Proxy @(SndShelleyBasedEra era1))
        projectOne
        (Telescope.tip (consolidateShelleyTele tele))
  where
    projectOne :: forall a.
         (SL.Crypto (Snd a) ~ SL.Crypto era1, ShelleyBasedEra (Snd a))
      => SOP.Index '[ '(proto1, era1), '(proto2, era2)]                              a
         -- ^ the current era of the ledger state we're projecting from
      -> UncurryComp (HFC.Current fmk) ShelleyBlock a
         -- ^ the ledger state we're projecting from
      -> SOP.K (LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) mk) a
    projectOne idx (UncurryComp current) =
        SOP.K $ ShelleyBasedHardForkLedgerTables $ inj appliedMK
      where
        ShelleyLedgerTables appliedMK =
            prj $ HFC.currentState current

        inj ::
             ApplyMapKind mk (SL.TxIn (EraCrypto era1)) (Core.TxOut (Snd a))
          -> ApplyMapKind mk (SL.TxIn (EraCrypto era1)) (ShelleyTxOut (MapSnd '[ '(proto1, era1), '(proto2, era2)]))
        inj = mapMK (ShelleyTxOut . SOP.injectNS (castSndIdx idx) . TxOutWrapper)

class (SL.Crypto (Snd a) ~ SL.Crypto c, ShelleyBasedEra (Snd a)) => SndShelleyBasedEra c a
instance (SL.Crypto (Snd a) ~ SL.Crypto c, ShelleyBasedEra (Snd a)) => SndShelleyBasedEra c a

-- Same note regarding the @TickedTableStuff x@ constraint as
-- 'projectLedgerTablesHelper'
withLedgerTablesHelper ::
  forall proto1 era1 proto2 era2 mk fany fmk.
     (ShelleyBasedHardForkConstraints proto1 era1 proto2 era2, IsApplyMapKind mk)
  => (forall x.
         TickedTableStuff (LedgerState x)
      => fany x -> LedgerTables (LedgerState x) mk -> fmk x
     )
  -> HardForkState fany (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  -> LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) mk
  -> HardForkState fmk (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
withLedgerTablesHelper with (HardForkState tele) (ShelleyBasedHardForkLedgerTables appliedMK) =
      HardForkState
    $ unconsolidateShelleyTele
    $ SOP.hap
        updateOne
        (consolidateShelleyTele tele)
  where
    -- how to update the ledger table of each possible individual era
    updateOne ::
      NP
        (     UncurryComp (HFC.Current fany) ShelleyBlock
         -.-> UncurryComp (HFC.Current fmk) ShelleyBlock
        )
        '[ '(proto1, era1), '(proto2, era2)]
    updateOne =
        ($ translations)
      $ SOP.hcmap (Proxy @(SndShelleyBasedEra era1))
      $ \(ApOnlySnd translate) -> SOP.fn $ \(UncurryComp current) ->
        let HFC.Current{HFC.currentState = innerSt} = current
            newInnerSt =
                with innerSt
              $ ShelleyLedgerTables
              $ mapMK
                  (unTxOutWrapper . SOP.apFn translate . SOP.K . unShelleyTxOut)
                  appliedMK
        in UncurryComp $ current{HFC.currentState = newInnerSt}

    -- the composed translations for each possible era; see
    -- 'composeTxOutTranslationPairs' to understand why this is partial but
    -- is safe in the absence of Consensus bugs
    translations ::
      NP
        (ApOnlySnd (     SOP.K (NS TxOutWrapper '[era1, era2])
                     -.-> TxOutWrapper
        ))
        '[ '(proto1, era1), '(proto2, era2)]
    translations =
         ApOnlySnd (SOP.fn (\case
             SOP.K (Z txo) -> txo
             _             -> error "bad ShelleyBasedHardForkBlock txout translation"
         ))
      :* ApOnlySnd (SOP.fn (\case
             SOP.K (Z txo)     -> error "ShelleyBasedHardForkBlock txout translation" txo   -- need a new class for every Shelley era but the first...
             SOP.K (S (Z txo)) -> txo
         ))
      :* Nil

-- Note that this is a HardForkBlock instance, but it's not compositional. This
-- is because the LedgerTables relies on knowledge specific to Shelley and we
-- have so far not found a pleasant way to express that compositionally.
instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => TableStuff (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) where
  newtype LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) mk = ShelleyBasedHardForkLedgerTables {
        shelleyBasedHardForkUTxOTable ::
            ApplyMapKind
              mk
              (SL.TxIn (EraCrypto era1))
              (ShelleyTxOut '[era1, era2])
      }
    deriving (Generic)

  projectLedgerTables (HardForkLedgerState hfstate) =
      projectLedgerTablesHelper
        (projectLedgerTables . unFlip)
        hfstate

  withLedgerTables (HardForkLedgerState hfstate) tables =
        HardForkLedgerState
      $ withLedgerTablesHelper
          (\(Flip st) tables' -> Flip $ withLedgerTables st tables')
          hfstate
          tables

  pureLedgerTables     f                                                                                                                = ShelleyBasedHardForkLedgerTables f
  mapLedgerTables      f                                                                           (ShelleyBasedHardForkLedgerTables x) = ShelleyBasedHardForkLedgerTables (f x)
  traverseLedgerTables f                                                                           (ShelleyBasedHardForkLedgerTables x) = ShelleyBasedHardForkLedgerTables <$> f x
  zipLedgerTables      f                                      (ShelleyBasedHardForkLedgerTables l) (ShelleyBasedHardForkLedgerTables r) = ShelleyBasedHardForkLedgerTables (f l r)
  zipLedgerTables2     f (ShelleyBasedHardForkLedgerTables l) (ShelleyBasedHardForkLedgerTables c) (ShelleyBasedHardForkLedgerTables r) = ShelleyBasedHardForkLedgerTables (f l c r)
  zipLedgerTablesA     f                                      (ShelleyBasedHardForkLedgerTables l) (ShelleyBasedHardForkLedgerTables r) = ShelleyBasedHardForkLedgerTables <$> f l r
  zipLedgerTables2A    f (ShelleyBasedHardForkLedgerTables l) (ShelleyBasedHardForkLedgerTables c) (ShelleyBasedHardForkLedgerTables r) = ShelleyBasedHardForkLedgerTables <$> f l c r
  foldLedgerTables     f                                                                           (ShelleyBasedHardForkLedgerTables x) = f x
  foldLedgerTables2    f                                      (ShelleyBasedHardForkLedgerTables l) (ShelleyBasedHardForkLedgerTables r) = f l r
  namesLedgerTables = ShelleyBasedHardForkLedgerTables { shelleyBasedHardForkUTxOTable = NameMK "shelleyBasedHardForkUTxOTable" }

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => SufficientSerializationForAnyBackingStore (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) where
    codecLedgerTables = ShelleyBasedHardForkLedgerTables $ CodecMK toCBOR toCBOR fromCBOR fromCBOR

deriving instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 => Eq       (LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) EmptyMK)
deriving instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 => Eq       (LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) ValuesMK)
deriving instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 => Eq       (LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) DiffMK)

deriving instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 => NoThunks (LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) EmptyMK)
deriving instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 => NoThunks (LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) ValuesMK)
deriving instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2 => NoThunks (LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) SeqDiffMK)

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => ShowLedgerState (LedgerTables (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2))) where
  showsLedgerState (ShelleyBasedHardForkLedgerTables utxo) =
        showParen True
      $ showString "ShelleyBasedHardForkLedgerTables " . showsApplyMapKind utxo

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => TickedTableStuff (LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)) where

  projectLedgerTablesTicked st =
      projectLedgerTablesHelper
        (\(FlipTickedLedgerState st') -> projectLedgerTablesTicked st')
        (tickedHardForkLedgerStatePerEra st)

  withLedgerTablesTicked TickedHardForkLedgerState{..} tables =
      TickedHardForkLedgerState {
          tickedHardForkLedgerStateTransition = tickedHardForkLedgerStateTransition
        , tickedHardForkLedgerStatePerEra     =
            withLedgerTablesHelper
              (\(FlipTickedLedgerState st) tables' ->
                 FlipTickedLedgerState $ withLedgerTablesTicked st tables')
              tickedHardForkLedgerStatePerEra
              tables
        }

instance
     ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  => LedgerTablesCanHardFork (ShelleyBasedHardForkEras proto1 era1 proto2 era2) where
  hardForkInjectLedgerTablesKeysMK =
         shelley SOP.IZ
      :* shelley (SOP.IS SOP.IZ)
      :* Nil
    where
      shelley ::
           (SndShelleyBasedEra era1 a)
        => SOP.Index '[ '(proto1, era1), '(proto2, era2)] a
        -> InjectLedgerTables (ShelleyBasedHardForkEras proto1 era1 proto2 era2) (ShelleyBlock (Fst a) (Snd a))
      shelley idx =
          InjectLedgerTables
        $ \(ShelleyLedgerTables lt) ->
            ShelleyBasedHardForkLedgerTables $ mapMK (ShelleyTxOut . SOP.injectNS (castSndIdx idx) . TxOutWrapper) lt

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyBasedHardFork ::
     forall m proto1 era1 proto2 era2.
     (IOLike m, ShelleyBasedHardForkConstraints proto1 era1 proto2 era2)
  => ProtocolParamsShelleyBased era1
  -> SL.ProtVer
  -> SL.ProtVer
  -> ProtocolTransitionParamsShelleyBased era2
  -> ProtocolInfo m (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
protocolInfoShelleyBasedHardFork protocolParamsShelleyBased
                                 protVer1
                                 protVer2
                                 protocolTransitionParams =
    protocolInfoBinary
      -- Era 1
      protocolInfo1
      eraParams1
      tpraosParams
      toPartialLedgerConfig1
      -- Era 2
      protocolInfo2
      eraParams2
      tpraosParams
      toPartialLedgerConfig2
  where
    ProtocolParamsShelleyBased {
        shelleyBasedGenesis
      , shelleyBasedInitialNonce
      , shelleyBasedLeaderCredentials
      } = protocolParamsShelleyBased

    -- Era 1

    genesis1 :: SL.ShelleyGenesis era1
    genesis1 = shelleyBasedGenesis

    protocolInfo1 :: ProtocolInfo m (ShelleyBlock proto1 era1)
    protocolInfo1 =
        protocolInfoTPraosShelleyBased
          protocolParamsShelleyBased
          ()  -- trivial translation context
          protVer1
          (TxLimits.mkOverrides TxLimits.noOverridesMeasure)

    eraParams1 :: History.EraParams
    eraParams1 = shelleyEraParams genesis1

    ProtocolTransitionParamsShelleyBased {
        transitionTranslationContext = transCtxt2
      , transitionTrigger
      } = protocolTransitionParams

    toPartialLedgerConfig1 ::
         LedgerConfig (ShelleyBlock proto1 era1)
      -> PartialLedgerConfig (ShelleyBlock proto1 era1)
    toPartialLedgerConfig1 cfg = ShelleyPartialLedgerConfig {
          shelleyLedgerConfig    = cfg
        , shelleyTriggerHardFork = transitionTrigger
        }

    -- Era 2

    genesis2 :: SL.ShelleyGenesis era2
    genesis2 = SL.translateEra' transCtxt2 genesis1

    protocolInfo2 :: ProtocolInfo m (ShelleyBlock proto2 era2)
    protocolInfo2 =
        protocolInfoTPraosShelleyBased
          ProtocolParamsShelleyBased {
              shelleyBasedGenesis = genesis2
            , shelleyBasedInitialNonce
            , shelleyBasedLeaderCredentials
            }
          transCtxt2
          protVer2
          (TxLimits.mkOverrides TxLimits.noOverridesMeasure)

    eraParams2 :: History.EraParams
    eraParams2 = shelleyEraParams genesis2

    toPartialLedgerConfig2 ::
         LedgerConfig (ShelleyBlock proto2 era2)
      -> PartialLedgerConfig (ShelleyBlock proto2 era2)
    toPartialLedgerConfig2 cfg = ShelleyPartialLedgerConfig {
          shelleyLedgerConfig    = cfg
        , shelleyTriggerHardFork = TriggerHardForkNever
        }

{-------------------------------------------------------------------------------
  TxGen instance
-------------------------------------------------------------------------------}

-- | Use a generic implementation for 'TxGen'
instance ( TxGen (ShelleyBlock proto1 era1)
         , TxGen (ShelleyBlock proto2 era2)
         , ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
         ) => TxGen (ShelleyBasedHardForkBlock proto1 era1 proto2 era2) where
  type TxGenExtra (ShelleyBasedHardForkBlock proto1 era1 proto2 era2) =
    NP WrapTxGenExtra (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  testGenTxs = testGenTxsHfc
