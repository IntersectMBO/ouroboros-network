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

import           Cardano.Binary (fromCBOR, toCBOR)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as SL
import qualified Cardano.Ledger.Shelley.API as SL
import           Control.Monad.Except (runExcept)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (Proxy))
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index
import qualified Data.SOP.Index as SOP
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.Strict (NP (..), NS (..), type (-.->), unComp,
                     (:.:) (..))
import qualified Data.SOP.Strict as SOP
import qualified Data.SOP.Tails as Tails
import qualified Data.SOP.Telescope as Telescope
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ShelleyPartialLedgerConfig (..), forecastAcrossShelley,
                     translateChainDepStateAcrossShelley)
import           Ouroboros.Consensus.Cardano.Node
                     (ProtocolTransitionParamsShelleyBased (..),
                     TriggerHardFork (..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Binary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import           Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Mempool (TxLimits)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol.Abstract (ProtoCrypto)
import           Ouroboros.Consensus.Shelley.ShelleyHFC (ShelleyTxOut (..))
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
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
  , SL.TranslateEra       era2 WrapTx
  , SL.TranslateEra       era2 TxOutWrapper

  , SL.TranslationError   era2 SL.NewEpochState ~ Void
  , SL.TranslationError   era2 TxOutWrapper     ~ Void

  , SL.AdditionalGenesisConfig era1 ~ ()
  , SL.AdditionalGenesisConfig era2 ~ SL.TranslationContext era2
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
                ShelleyLedgerTables
              . fmap
                ( unTxOutWrapper
                . SL.translateEra' (shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2))
                . TxOutWrapper
                )
              . shelleyUTxOTable
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
  Ledger tables
-------------------------------------------------------------------------------}

type LedgerStateShelley proto1 era1 proto2 era2 =
     LedgerState (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)

projectLedgerTablesHelper :: forall proto1 era1 proto2 era2 fmk mk.
     (ShelleyBasedHardForkConstraints proto1 era1 proto2 era2, IsMapKind mk)
  => (forall x.
         HasTickedLedgerTables (LedgerState x)
      => fmk x -> LedgerTables (LedgerState x) mk
     )
  -> HardForkState fmk (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  -> LedgerTables (LedgerStateShelley proto1 era1 proto2 era2) mk
projectLedgerTablesHelper prj (HardForkState tele) =
      SOP.hcollapse
    $ Telescope.tip
    $ hcimap
        (Proxy @(SOP.Compose HasTickedLedgerTables LedgerState))
        projectOne
        tele
  where
    projectOne :: HasTickedLedgerTables (LedgerState x)
               => Index (ShelleyBasedHardForkEras proto1 era1 proto2 era2) x
               -> Current fmk           x
               -> SOP.K (LedgerTables
                      (LedgerStateShelley proto1 era1 proto2 era2) mk)
                                        x
    projectOne i Current{currentState = innerSt} =
        SOP.K
      $ applyInjectLedgerTables (projectNP i hardForkInjectLedgerTables)
      $ prj innerSt

withLedgerTablesHelper ::
  forall proto1 era1 proto2 era2 mk fany fmk.
     (ShelleyBasedHardForkConstraints proto1 era1 proto2 era2, IsMapKind mk)
  => (forall x.
         HasTickedLedgerTables (LedgerState x)
      => fany x -> LedgerTables (LedgerState x) mk -> fmk x
     )
  -> HardForkState fany (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
  -> LedgerTables (LedgerStateShelley proto1 era1 proto2 era2) mk
  -> HardForkState fmk (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
withLedgerTablesHelper with (HardForkState tele) tbs =
      HardForkState
    $ hcimap
        (Proxy @(SOP.Compose HasTickedLedgerTables LedgerState))
        updateOne
        tele
  where
    updateOne :: HasTickedLedgerTables (LedgerState x)
              => Index (ShelleyBasedHardForkEras proto1 era1 proto2 era2) x
              -> Current fany          x
              -> Current fmk           x
    updateOne i current@Current{currentState = innerSt} =
      current { currentState =
                  with innerSt
                    $ applyDistribLedgerTables
                        (projectNP i hardForkInjectLedgerTables)
                        tbs
              }

-- Note that this is a HardForkBlock instance, but it's not compositional. This
-- is because the LedgerTables relies on knowledge specific to Shelley and we
-- have so far not found a pleasant way to express that compositionally.
instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => HasLedgerTables (LedgerStateShelley proto1 era1 proto2 era2) where
  newtype LedgerTables (LedgerStateShelley proto1 era1 proto2 era2) mk =
    ShelleyBasedHardForkLedgerTables {
        shelleyBasedHardForkUTxOTable ::
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

  pureLedgerTables = ShelleyBasedHardForkLedgerTables
  mapLedgerTables      f (ShelleyBasedHardForkLedgerTables x) =
    ShelleyBasedHardForkLedgerTables (f x)
  traverseLedgerTables f (ShelleyBasedHardForkLedgerTables x) =
    ShelleyBasedHardForkLedgerTables <$> f x
  zipLedgerTables      f (ShelleyBasedHardForkLedgerTables l)
                         (ShelleyBasedHardForkLedgerTables r) =
    ShelleyBasedHardForkLedgerTables (f l r)
  zipLedgerTables3     f (ShelleyBasedHardForkLedgerTables l)
                         (ShelleyBasedHardForkLedgerTables c)
                         (ShelleyBasedHardForkLedgerTables r) =
    ShelleyBasedHardForkLedgerTables (f l c r)
  zipLedgerTablesA     f (ShelleyBasedHardForkLedgerTables l)
                         (ShelleyBasedHardForkLedgerTables r) =
    ShelleyBasedHardForkLedgerTables <$> f l r
  zipLedgerTables3A    f (ShelleyBasedHardForkLedgerTables l)
                         (ShelleyBasedHardForkLedgerTables c)
                         (ShelleyBasedHardForkLedgerTables r) =
    ShelleyBasedHardForkLedgerTables <$> f l c r
  foldLedgerTables     f (ShelleyBasedHardForkLedgerTables x) = f x
  foldLedgerTables2    f (ShelleyBasedHardForkLedgerTables l)
                         (ShelleyBasedHardForkLedgerTables r) = f l r
  namesLedgerTables = ShelleyBasedHardForkLedgerTables {
      shelleyBasedHardForkUTxOTable = NameMK "shelleyBasedHardForkUTxOTable"
    }

type ShelleyTables proto1 era1 proto2 era2 mk =
     LedgerTables (LedgerStateShelley proto1 era1 proto2 era2) mk

deriving instance ( ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
                  , IsMapKind mk
                  ) => Eq (ShelleyTables proto1 era1 prot2 era2 mk)
deriving instance ( ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
                  , IsMapKind mk
                  ) => NoThunks (ShelleyTables proto1 era1 proto2 era2 mk)
deriving instance ( ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
                  , IsMapKind mk
                  ) => Show (ShelleyTables proto1 era1 proto2 era2 mk)

instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => CanSerializeLedgerTables (LedgerStateShelley proto1 era1 proto2 era2) where
    codecLedgerTables =
      ShelleyBasedHardForkLedgerTables (CodecMK
                                         (Core.toEraCBOR @era1)
                                         toCBOR
                                         (Core.fromEraCBOR @era2)
                                         fromCBOR)


instance ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
      => HasTickedLedgerTables (LedgerStateShelley proto1 era1 proto2 era2) where

  projectLedgerTablesTicked st =
      projectLedgerTablesHelper
        (\(FlipTickedLedgerState st') -> projectLedgerTablesTicked st')
        (tickedHardForkLedgerStatePerEra st)

  withLedgerTablesTicked ths tables =
      TickedHardForkLedgerState {
          tickedHardForkLedgerStateTransition
        , tickedHardForkLedgerStatePerEra     =
            withLedgerTablesHelper
              (\(FlipTickedLedgerState st) tables' ->
                 FlipTickedLedgerState $ withLedgerTablesTicked st tables')
              tickedHardForkLedgerStatePerEra
              tables
        }
    where
      TickedHardForkLedgerState {
          tickedHardForkLedgerStateTransition
        , tickedHardForkLedgerStatePerEra
        } = ths

instance
     ShelleyBasedHardForkConstraints proto1 era1 proto2 era2
  => LedgerTablesCanHardFork (ShelleyBasedHardForkEras proto1 era1 proto2 era2) where
  hardForkInjectLedgerTables =
         shelley SOP.IZ
      :* shelley (SOP.IS SOP.IZ)
      :* Nil
    where
      shelley ::
           forall era proto. ( EraCrypto era ~ ProtoCrypto proto2
                             , Eq (Core.TxOut era)
                             )
        => SOP.Index '[ era1, era2 ] era
        -> InjectLedgerTables
             (ShelleyBasedHardForkEras proto1 era1 proto2 era2)
             (ShelleyBlock proto era)
      shelley idx =
          InjectLedgerTables
          { applyInjectLedgerTables =
                ShelleyBasedHardForkLedgerTables
              . mapMK (ShelleyTxOut . SOP.injectNS idx . TxOutWrapper)
              . shelleyUTxOTable
          , applyDistribLedgerTables =
                ShelleyLedgerTables
              . mapMK ( unTxOutWrapper
                      . SOP.apFn (projectNP idx translations)
                      . SOP.K
                      . unShelleyTxOut
                      )
              . shelleyBasedHardForkUTxOTable
          }

translations ::
      NP
        (SOP.K (NS TxOutWrapper '[era1, era2]) -.-> TxOutWrapper )
        '[ era1, era2 ]
translations =
         SOP.fn (\case
             SOP.K (Z txo) -> txo
             _             -> e
         )
      :* SOP.fn (\case
             SOP.K (Z _)     -> e
             SOP.K (S (Z txo)) -> txo
         )
      :* Nil
  where e = error "bad ShelleyBasedHardForkBlock txout translation"

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyBasedHardFork ::
     forall m proto1 era1 proto2 era2.
     (IOLike m, ShelleyBasedHardForkConstraints proto1 era1 proto2 era2)
  => ProtocolParamsShelleyBased era1
  -> SL.ProtVer
  -> SL.ProtVer
  -> SL.TranslationContext era1
  -> ProtocolTransitionParamsShelleyBased era2
  -> ProtocolInfo m (ShelleyBasedHardForkBlock proto1 era1 proto2 era2)
protocolInfoShelleyBasedHardFork protocolParamsShelleyBased
                                 protVer1
                                 protVer2
                                 transCtx1
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

    genesis :: SL.ShelleyGenesis (EraCrypto era1)
    genesis = shelleyBasedGenesis

    protocolInfo1 :: ProtocolInfo m (ShelleyBlock proto1 era1)
    protocolInfo1 =
        protocolInfoTPraosShelleyBased
          protocolParamsShelleyBased
          ((), transCtx1)
          protVer1
          (Mempool.mkOverrides Mempool.noOverridesMeasure)

    eraParams1 :: History.EraParams
    eraParams1 = shelleyEraParams genesis

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

    protocolInfo2 :: ProtocolInfo m (ShelleyBlock proto2 era2)
    protocolInfo2 =
        protocolInfoTPraosShelleyBased
          ProtocolParamsShelleyBased {
              shelleyBasedGenesis = genesis
            , shelleyBasedInitialNonce
            , shelleyBasedLeaderCredentials
            }
          (transCtxt2, transCtxt2)
          protVer2
          (Mempool.mkOverrides Mempool.noOverridesMeasure)

    eraParams2 :: History.EraParams
    eraParams2 = shelleyEraParams genesis

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
