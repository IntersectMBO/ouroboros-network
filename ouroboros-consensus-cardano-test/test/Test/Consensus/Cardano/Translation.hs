{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Test.Consensus.Cardano.Translation (tests) where

import           Data.Int (Int64)
import qualified Data.ListMap as ListMap
import           Data.Map.Diff.Strict (Diff (..))
import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Strict as Map
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.EpochInfo (fixedEpochInfo)
import           Cardano.Slotting.Slot (EpochNo (..))

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron

import           Cardano.Ledger.Alonzo ()
import           Cardano.Ledger.Alonzo.Genesis (AlonzoGenesis (..))
import           Cardano.Ledger.Alonzo.Scripts (CostModels (..), ExUnits (..),
                     Prices (..))
import           Cardano.Ledger.BaseTypes (Network (Testnet), TxIx (..))
import           Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Shelley.API
                     (NewEpochState (stashedAVVMAddresses), ShelleyGenesis (..),
                     ShelleyGenesisStaking (..), TxIn (..),
                     translateCompactTxOutByronToShelley,
                     translateTxIdByronToShelley)
import           Cardano.Ledger.Shelley.LedgerState (_utxo, esLState,
                     lsUTxOState, nesEs)
import           Cardano.Ledger.Shelley.PParams (emptyPParams)
import           Cardano.Ledger.Shelley.UTxO (UTxO (..))

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
                     (slotLengthFromSec)
import           Ouroboros.Consensus.Cardano.Block (CardanoEras)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.HardFork.Combinator (InPairs (..),
                     hardForkEraTranslation, translateLedgerState)
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
                     (TranslateLedgerState (translateLedgerStateWith))
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth, provideBoth)
import           Ouroboros.Consensus.Ledger.Basics (LedgerCfg, LedgerConfig,
                     LedgerState)
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.Praos.Common
                     (MaxMajorProtVer (..))
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util (dimap)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock, byronLedgerState)

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyLedgerConfig, mkShelleyLedgerConfig,
                     shelleyLedgerState, shelleyLedgerTables, shelleyUTxOTable)
import           Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

import           Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import           Test.Cardano.Ledger.Shelley.Utils (unsafeBoundRational)
import           Test.Consensus.Byron.Generators (genByronLedgerConfig,
                     genByronLedgerState)
import           Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)
import           Test.Consensus.Shelley.Generators ()
import           Test.Consensus.Shelley.MockCrypto

import           Test.Util.Time (dawnOfTime)

-- Definitions to make the signatures a bit less unwieldy
type Crypto = MockCryptoCompatByron
type Proto  = TPraos Crypto

tests :: TestTree
tests = testGroup "UpdateTablesOnEraTransition"
  [ testTablesTranslation "Byron to Shelley"
                          byronToShelleyLedgerStateTranslation
                          byronUtxosAreInsertsInShelleyUtxoDiff
                          (\st -> cover 50 (      nonEmptyUtxosByron st) "UTxO set is not empty"
                                -- FIXME: we should test with empyt UTxO!
                                -- . cover 1  (not $ nonEmptyUtxosByron st) "UTxO set is empty"
                          )
  , testTablesTranslation "Shelley to Allegra"
                          shelleyToAllegraLedgerStateTranslation
                          shelleyAvvmAddressesAreDeletesInUtxoDiff
                          (\st -> cover 50 (nonEmptyAvvmAddresses st) "AVVM set is not empty")
  , testTablesTranslation "Allegra to Mary"
                          allegraToMaryLedgerStateTranslation
                          utxoTablesAreEmpty
                          (\st -> cover 50 (nonEmptyUtxosShelley st) "UTxO set is not empty")
  , testTablesTranslation "Mary to Alonzo"
                          maryToAlonzoLedgerStateTranslation
                          utxoTablesAreEmpty
                          (\st -> cover 50 (nonEmptyUtxosShelley st) "UTxO set is not empty")
  , testTablesTranslation "Alonzo to Babbage"
                          alonzoToBabbageLedgerStateTranslation
                          utxoTablesAreEmpty
                          (\st -> cover 50 (nonEmptyUtxosShelley st) "UTxO set is not empty")
  ]


{-------------------------------------------------------------------------------
  Ledger-state translations between eras that we test in this module
-------------------------------------------------------------------------------}

-- | TODO: we should simply expose 'translateLedgerStateByronToShelleyWrapper'
-- and other translations in ' Ouroboros.Consensus.Cardano.CanHardFork'.
byronToShelleyLedgerStateTranslation ::
  RequiringBoth
        WrapLedgerConfig
        TranslateLedgerState
        ByronBlock
        (ShelleyBlock (TPraos Crypto) (ShelleyEra Crypto))
shelleyToAllegraLedgerStateTranslation :: RequiringBoth
  WrapLedgerConfig
  TranslateLedgerState
  (ShelleyBlock (TPraos Crypto) (ShelleyEra Crypto))
  (ShelleyBlock (TPraos Crypto) (AllegraEra Crypto))
allegraToMaryLedgerStateTranslation :: RequiringBoth
  WrapLedgerConfig
  TranslateLedgerState
  (ShelleyBlock (TPraos Crypto) (AllegraEra Crypto))
  (ShelleyBlock (TPraos Crypto) (MaryEra Crypto))
maryToAlonzoLedgerStateTranslation :: RequiringBoth
  WrapLedgerConfig
  TranslateLedgerState
  (ShelleyBlock (TPraos Crypto) (MaryEra Crypto))
  (ShelleyBlock (TPraos Crypto) (AlonzoEra Crypto))
alonzoToBabbageLedgerStateTranslation :: RequiringBoth
  WrapLedgerConfig
  TranslateLedgerState
  (ShelleyBlock (TPraos Crypto) (AlonzoEra Crypto))
  (ShelleyBlock (Praos Crypto) (BabbageEra Crypto))
PCons byronToShelleyLedgerStateTranslation
      (PCons shelleyToAllegraLedgerStateTranslation
       (PCons allegraToMaryLedgerStateTranslation
        (PCons maryToAlonzoLedgerStateTranslation
         (PCons alonzoToBabbageLedgerStateTranslation
          PNil)))) = tls
  where
    tls :: InPairs
             (RequiringBoth WrapLedgerConfig TranslateLedgerState)
             (CardanoEras Crypto)
    tls = translateLedgerState hardForkEraTranslation

-- | Check that the tables are correctly translated from one era to the next.
testTablesTranslation ::
     forall srcBlk dstBlk.
     ( Arbitrary (TestSetup srcBlk dstBlk)
     , Show (LedgerCfg (LedgerState srcBlk))
     , Show (LedgerCfg (LedgerState dstBlk))
     , Show (LedgerState srcBlk EmptyMK)
     )
  => String
  -- ^ Property label
  -> RequiringBoth
        WrapLedgerConfig
        TranslateLedgerState
        srcBlk
        dstBlk
  -> (LedgerState srcBlk EmptyMK -> LedgerState dstBlk DiffMK -> Bool)
  -> (LedgerState srcBlk EmptyMK -> Property -> Property)
  -- ^ Coverage testing function (TODO: it can be abused)
  -> TestTree
testTablesTranslation propLabel translateWithConfig translationShouldSatisfy ledgerStateShouldCover =
    testProperty propLabel withTestSetup
  where
    withTestSetup :: TestSetup srcBlk dstBlk -> Property
    withTestSetup ts =
        checkCoverage $ ledgerStateShouldCover tsSrcLedgerState
                      $ property
                      $ translationShouldSatisfy tsSrcLedgerState destState
      where
        TestSetup {tsSrcLedgerConfig, tsDestLedgerConfig, tsSrcLedgerState, tsEpochNo} = ts
        destState = translateLedgerStateWith translation tsEpochNo tsSrcLedgerState
          where
            translation :: TranslateLedgerState srcBlk dstBlk
            translation = provideBoth translateWithConfig
                                      (WrapLedgerConfig tsSrcLedgerConfig)
                                      (WrapLedgerConfig tsDestLedgerConfig)

{-------------------------------------------------------------------------------
    Specific predicates
-------------------------------------------------------------------------------}

byronUtxosAreInsertsInShelleyUtxoDiff
  :: LedgerState ByronBlock EmptyMK
  -> LedgerState (ShelleyBlock Proto (ShelleyEra Crypto)) DiffMK
  -> Bool
byronUtxosAreInsertsInShelleyUtxoDiff srcLedgerState destLedgerState =
    toNextUtxoDiff srcLedgerState == extractUtxoDiff destLedgerState
  where
    toNextUtxoDiff
      :: LedgerState ByronBlock mk
      -> Diff (TxIn Crypto) (Core.TxOut (ShelleyEra Crypto))
    toNextUtxoDiff ledgerState =
      let
        Byron.UTxO utxo = Byron.cvsUtxo $ byronLedgerState ledgerState
        keyFn = translateTxInByronToShelley . Byron.fromCompactTxIn
        valFn = Diff.singletonInsert . translateCompactTxOutByronToShelley
      in
        Diff $ dimap keyFn valFn utxo

    translateTxInByronToShelley :: Byron.TxIn -> TxIn Crypto
    translateTxInByronToShelley byronTxIn =
      let
        Byron.TxInUtxo txId txIx = byronTxIn
        shelleyTxId' = translateTxIdByronToShelley txId
      in
        TxIn shelleyTxId' (TxIx $ fromIntegral txIx)

shelleyAvvmAddressesAreDeletesInUtxoDiff
  :: LedgerState (ShelleyBlock Proto (ShelleyEra Crypto)) EmptyMK
  -> LedgerState (ShelleyBlock Proto (AllegraEra Crypto)) DiffMK
  -> Bool
shelleyAvvmAddressesAreDeletesInUtxoDiff srcLedgerState destLedgerState =
    toNextUtxoDiff srcLedgerState == extractUtxoDiff destLedgerState
  where
    toNextUtxoDiff
      :: LedgerState (ShelleyBlock Proto (ShelleyEra Crypto)) EmptyMK
      -> Diff (TxIn Crypto) (Core.TxOut (AllegraEra Crypto))
    toNextUtxoDiff = avvmAddressesToUtxoDiff . stashedAVVMAddresses . shelleyLedgerState
    avvmAddressesToUtxoDiff (UTxO m) =
      let func txOut = Diff.singletonDelete (Core.translateEra' () txOut)
      in Diff $ dimap id func m

utxoTablesAreEmpty
  :: LedgerState (ShelleyBlock srcProto srcEra) EmptyMK
  -> LedgerState (ShelleyBlock destProto destEra) DiffMK
  -> Bool
utxoTablesAreEmpty _ destLedgerState = Diff.null $ extractUtxoDiff destLedgerState

nonEmptyUtxosByron :: LedgerState ByronBlock EmptyMK -> Bool
nonEmptyUtxosByron ledgerState =
  let Byron.UTxO utxo = Byron.cvsUtxo $ byronLedgerState ledgerState
  in not $ Map.null utxo

nonEmptyUtxosShelley :: LedgerState (ShelleyBlock proto era) EmptyMK -> Bool
nonEmptyUtxosShelley ledgerState =
  let UTxO m = _utxo $ lsUTxOState $ esLState $ nesEs $ shelleyLedgerState ledgerState
  in not $ Map.null m

nonEmptyAvvmAddresses :: LedgerState (ShelleyBlock Proto (ShelleyEra Crypto)) EmptyMK -> Bool
nonEmptyAvvmAddresses ledgerState =
  let UTxO m = stashedAVVMAddresses $ shelleyLedgerState ledgerState
  in not $ Map.null m

{-------------------------------------------------------------------------------
    Utilities
-------------------------------------------------------------------------------}

extractUtxoDiff
  :: LedgerState (ShelleyBlock proto era) DiffMK
  -> Diff (TxIn (EraCrypto era)) (Core.TxOut era)
extractUtxoDiff shelleyLedgerState =
  let ApplyDiffMK tables = shelleyUTxOTable $ shelleyLedgerTables shelleyLedgerState
  in tables

{-------------------------------------------------------------------------------
    TestSetup
-------------------------------------------------------------------------------}

data TestSetup src dest = TestSetup {
    tsSrcLedgerConfig  :: LedgerConfig src
  , tsDestLedgerConfig :: LedgerConfig dest
  , tsSrcLedgerState   :: LedgerState src EmptyMK
  , tsEpochNo          :: EpochNo
}

deriving instance ( Show (LedgerConfig src)
                  , Show (LedgerConfig dest)
                  , Show (LedgerState src EmptyMK)) => Show (TestSetup src dest)

-- TODO: Useful to merge some of these instances?
instance Arbitrary (TestSetup ByronBlock (ShelleyBlock Proto (ShelleyEra Crypto))) where
  arbitrary =
    let ledgerConfig = fixedShelleyLedgerConfig ()
    in TestSetup <$> genByronLedgerConfig
                 <*> pure ledgerConfig
                 <*> genByronLedgerState
                 <*> (EpochNo <$> arbitrary)

instance Arbitrary (TestSetup (ShelleyBlock Proto (ShelleyEra Crypto))
                              (ShelleyBlock Proto (AllegraEra Crypto))) where
  arbitrary = TestSetup (fixedShelleyLedgerConfig ())
                        (fixedShelleyLedgerConfig ())
                        <$> genShelleyLedgerState
                        <*> (EpochNo <$> arbitrary)

instance Arbitrary (TestSetup (ShelleyBlock Proto (AllegraEra Crypto))
                              (ShelleyBlock Proto (MaryEra Crypto))) where
  arbitrary = TestSetup (fixedShelleyLedgerConfig ())
                        (fixedShelleyLedgerConfig ())
                        <$> genShelleyLedgerState
                        <*> (EpochNo <$> arbitrary)

instance Arbitrary (TestSetup (ShelleyBlock Proto (MaryEra Crypto))
                              (ShelleyBlock Proto (AlonzoEra Crypto))) where
  arbitrary = TestSetup (fixedShelleyLedgerConfig ())
                        <$> (fixedShelleyLedgerConfig <$> genAlonzoGenesis)
                        <*> genShelleyLedgerState
                        <*> (EpochNo <$> arbitrary)

instance Arbitrary (TestSetup (ShelleyBlock (TPraos Crypto) (AlonzoEra Crypto))
                              (ShelleyBlock (Praos Crypto) (BabbageEra Crypto))) where
  arbitrary = TestSetup <$> (fixedShelleyLedgerConfig <$> genAlonzoGenesis)
                        <*> (fixedShelleyLedgerConfig <$> genAlonzoGenesis)
                        <*> genShelleyLedgerState
                        <*> (EpochNo <$> arbitrary)

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}

genShelleyLedgerState :: CanMock proto era => Gen (LedgerState (ShelleyBlock proto era) EmptyMK)
genShelleyLedgerState = arbitrary

-- | A fixed ledger config should be sufficient as the updating of the ledger
-- tables on era transitions does not depend on the configurations of any of
-- the ledgers involved.
fixedShelleyLedgerConfig :: Core.TranslationContext era -> ShelleyLedgerConfig era
fixedShelleyLedgerConfig translationContext = mkShelleyLedgerConfig
      shelleyGenesis
      translationContext
      (fixedEpochInfo (sgEpochLength shelleyGenesis) (slotLengthFromSec 2))
      (MaxMajorProtVer 1000)
  where
    shelleyGenesis = ShelleyGenesis {
          sgSystemStart       = dawnOfTime
        , sgNetworkMagic      = 0
        , sgNetworkId         = Testnet
        , sgActiveSlotsCoeff  = unsafeBoundRational 0.8
        , sgSecurityParam     = 10
        , sgEpochLength       = 10
        , sgSlotsPerKESPeriod = 10
        , sgMaxKESEvolutions  = 10
        , sgSlotLength        = 10
        , sgUpdateQuorum      = 6
        , sgMaxLovelaceSupply = 10
        , sgProtocolParams    = emptyPParams
        , sgGenDelegs         = Map.empty
        , sgInitialFunds      = ListMap.empty
        , sgStaking           = ShelleyGenesisStaking ListMap.empty ListMap.empty
    }

genAlonzoGenesis :: Gen AlonzoGenesis
genAlonzoGenesis = do
  prices <- Prices <$> arbitrary <*> arbitrary
  maxTxUnits <- ExUnits <$> genFromIntegral <*> genFromIntegral
  maxBlockExUnits <- ExUnits <$> genFromIntegral <*> genFromIntegral
  pure $ AlonzoGenesis {
      coinsPerUTxOWord = Coin 10
    , costmdls = CostModels Map.empty
    , prices = prices
    , maxTxExUnits = maxTxUnits
    , maxBlockExUnits = maxBlockExUnits
    , maxValSize = 10
    , collateralPercentage = 10
    , maxCollateralInputs = 10
    }
  where
    genFromIntegral = fromIntegral <$> choose (0, maxBound :: Int64)
