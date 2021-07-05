{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
  ) where

import           Control.Monad.Except (runExcept)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict
import           Data.Void (Void)

import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (eitherToMaybe)
import           Ouroboros.Consensus.Util.IOLike (IOLike)

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Binary
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation
import qualified Ouroboros.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
import qualified Ouroboros.Consensus.HardFork.History as History

import qualified Cardano.Ledger.Era as SL
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Mempool.TxLimits
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node
import           Ouroboros.Consensus.Shelley.Protocol

import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ShelleyPartialLedgerConfig (..), forecastAcrossShelley,
                     translateChainDepStateAcrossShelley)
import           Ouroboros.Consensus.Cardano.Node
                     (ProtocolTransitionParamsShelleyBased (..),
                     TriggerHardFork (..))

import           Test.ThreadNet.TxGen
import           Test.ThreadNet.TxGen.Shelley ()

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

-- | Two eras, both Shelley-based.
type ShelleyBasedHardForkEras era1 era2 =
    '[ShelleyBlock era1, ShelleyBlock era2]

type ShelleyBasedHardForkBlock era1 era2 =
  HardForkBlock (ShelleyBasedHardForkEras era1 era2)

{-------------------------------------------------------------------------------
  Pattern synonyms, for encapsulation and legibility
-------------------------------------------------------------------------------}

type ShelleyBasedHardForkGenTx era1 era2 =
  GenTx (ShelleyBasedHardForkBlock era1 era2)

pattern GenTxShelley1 ::
     GenTx (ShelleyBlock era1)
  -> ShelleyBasedHardForkGenTx era1 era2
pattern GenTxShelley1 tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxShelley2 ::
     GenTx (ShelleyBlock era2)
  -> ShelleyBasedHardForkGenTx era1 era2
pattern GenTxShelley2 tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxShelley1, GenTxShelley2 #-}

pattern ShelleyBasedHardForkNodeToNodeVersion1 ::
     BlockNodeToNodeVersion (ShelleyBasedHardForkBlock era1 era2)
pattern ShelleyBasedHardForkNodeToNodeVersion1 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1
      :* Nil
      )

pattern ShelleyBasedHardForkNodeToClientVersion1 ::
     BlockNodeToClientVersion (ShelleyBasedHardForkBlock era1 era2)
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

type ShelleyBasedHardForkConstraints era1 era2 =
  ( ShelleyBasedEra era1
  , ShelleyBasedEra era2
  , TxLimits (ShelleyBlock era1)
  , TxLimits (ShelleyBlock era2)
  , EraCrypto era1 ~ EraCrypto era2
  , SL.PreviousEra era2 ~ era1

  , SL.TranslateEra       era2 SL.Tx
  , SL.TranslateEra       era2 SL.NewEpochState
  , SL.TranslateEra       era2 SL.ShelleyGenesis
  , SL.TranslateEra       era2 WrapTxInBlock

  , SL.TranslationError   era2 SL.NewEpochState  ~ Void
  , SL.TranslationError   era2 SL.ShelleyGenesis ~ Void

  , SL.TranslationContext era1 ~ ()
  )

instance ShelleyBasedHardForkConstraints era1 era2
      => SerialiseHFC (ShelleyBasedHardForkEras era1 era2)
   -- use defaults

instance ShelleyBasedHardForkConstraints era1 era2
      => CanHardFork (ShelleyBasedHardForkEras era1 era2) where
  hardForkEraTranslation = EraTranslation {
        translateLedgerState   = PCons translateLedgerState                PNil
      , translateChainDepState = PCons translateChainDepStateAcrossShelley PNil
      , translateLedgerView    = PCons translateLedgerView                 PNil
      }
    where
      translateLedgerState ::
           InPairs.RequiringBoth
             WrapLedgerConfig
             (HFC.Translate LedgerState)
             (ShelleyBlock era1)
             (ShelleyBlock era2)
      translateLedgerState =
          InPairs.RequireBoth
        $ \_cfg1 cfg2 -> HFC.Translate
        $ \_epochNo ->
              unComp
            . SL.translateEra'
                (shelleyLedgerTranslationContext (unwrapLedgerConfig cfg2))
            . Comp

      translateLedgerView ::
           InPairs.RequiringBoth
              WrapLedgerConfig
              (HFC.TranslateForecast LedgerState WrapLedgerView)
              (ShelleyBlock era1)
              (ShelleyBlock era2)
      translateLedgerView =
          InPairs.RequireBoth $ \(WrapLedgerConfig cfg1) (WrapLedgerConfig cfg2) ->
            HFC.TranslateForecast $ forecastAcrossShelley cfg1 cfg2

  hardForkChainSel = Tails.mk2 SelectSameProtocol

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
        ->        GenTx (ShelleyBlock era1)
        -> Maybe (GenTx (ShelleyBlock era2))
      translateTx transCtxt =
          fmap unComp
        . eitherToMaybe . runExcept . SL.translateEra transCtxt
        . Comp

      translateValidatedTx ::
           SL.TranslationContext era2
        ->        WrapValidatedGenTx (ShelleyBlock era1)
        -> Maybe (WrapValidatedGenTx (ShelleyBlock era2))
      translateValidatedTx transCtxt =
            fmap unComp
          . eitherToMaybe . runExcept . SL.translateEra transCtxt
          . Comp

instance ShelleyBasedHardForkConstraints era1 era2
      => SupportedNetworkProtocolVersion (ShelleyBasedHardForkBlock era1 era2) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToNodeVersion1)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (maxBound, ShelleyBasedHardForkNodeToClientVersion1)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoShelleyBasedHardFork ::
     forall m era1 era2. (IOLike m, ShelleyBasedHardForkConstraints era1 era2)
  => ProtocolParamsShelleyBased era1
  -> SL.ProtVer
  -> SL.ProtVer
  -> ProtocolTransitionParamsShelleyBased era2
  -> ProtocolInfo m (ShelleyBasedHardForkBlock era1 era2)
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

    protocolInfo1 :: ProtocolInfo m (ShelleyBlock era1)
    protocolInfo1 =
        protocolInfoShelleyBased
          protocolParamsShelleyBased
          ()  -- trivial translation context
          protVer1

    eraParams1 :: History.EraParams
    eraParams1 = shelleyEraParams genesis1

    ProtocolTransitionParamsShelleyBased {
        transitionTranslationContext = transCtxt2
      , transitionTrigger
      } = protocolTransitionParams

    toPartialLedgerConfig1 ::
         LedgerConfig (ShelleyBlock era1)
      -> PartialLedgerConfig (ShelleyBlock era1)
    toPartialLedgerConfig1 cfg = ShelleyPartialLedgerConfig {
          shelleyLedgerConfig    = cfg
        , shelleyTriggerHardFork = transitionTrigger
        }

    -- Era 2

    genesis2 :: SL.ShelleyGenesis era2
    genesis2 = SL.translateEra' transCtxt2 genesis1

    protocolInfo2 :: ProtocolInfo m (ShelleyBlock era2)
    protocolInfo2 =
        protocolInfoShelleyBased
          ProtocolParamsShelleyBased {
              shelleyBasedGenesis = genesis2
            , shelleyBasedInitialNonce
            , shelleyBasedLeaderCredentials
            }
          transCtxt2
          protVer2

    eraParams2 :: History.EraParams
    eraParams2 = shelleyEraParams genesis2

    toPartialLedgerConfig2 ::
         LedgerConfig (ShelleyBlock era2)
      -> PartialLedgerConfig (ShelleyBlock era2)
    toPartialLedgerConfig2 cfg = ShelleyPartialLedgerConfig {
          shelleyLedgerConfig    = cfg
        , shelleyTriggerHardFork = TriggerHardForkNever
        }

{-------------------------------------------------------------------------------
  TxGen instance
-------------------------------------------------------------------------------}

-- | Use a generic implementation for 'TxGen'
instance ( TxGen (ShelleyBlock era1)
         , TxGen (ShelleyBlock era2)
         , ShelleyBasedHardForkConstraints era1 era2
         ) => TxGen (ShelleyBasedHardForkBlock era1 era2) where
  type TxGenExtra (ShelleyBasedHardForkBlock era1 era2) =
    NP WrapTxGenExtra (ShelleyBasedHardForkEras era1 era2)
  testGenTxs = testGenTxsHfc
