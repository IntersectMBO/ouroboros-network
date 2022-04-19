{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

module Test.Consensus.Shelley.Examples (
    -- * Setup
    codecConfig
  , testShelleyGenesis
    -- * Examples
  , examplesAllegra
  , examplesAlonzo
  , examplesMary
  , examplesShelley
  ) where

import           Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           GHC.Records (HasField (getField))

import qualified Cardano.Ledger.Core as LC
import           Cardano.Ledger.Era (Crypto, getAllTxInputs)
import           Cardano.Ledger.Shelley.Tx (TxIn)

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Basics
                     (ApplyMapKind' (ApplyEmptyMK))
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Storage.Serialisation

import           Test.Cardano.Ledger.Shelley.Orphans ()

import qualified Ouroboros.Consensus.Ledger.Basics as Basics
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Golden (labelled, unlabelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import           Ouroboros.Consensus.Protocol.TPraos (TPraosState (TPraosState))

import           Test.Cardano.Ledger.Allegra.Examples.Consensus
                     (ledgerExamplesAllegra)
import           Test.Cardano.Ledger.Alonzo.Examples.Consensus
                     (ledgerExamplesAlonzo)
import           Test.Cardano.Ledger.Mary.Examples.Consensus
                     (ledgerExamplesMary)
import           Test.Cardano.Ledger.Shelley.Examples.Consensus
                     (ShelleyLedgerExamples (..), ShelleyResultExamples (..),
                     ledgerExamplesShelley, testShelleyGenesis)

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

codecConfig :: CodecConfig (ShelleyBlock Ouroboros.Consensus.Shelley.Eras.StandardShelley)
codecConfig = ShelleyCodecConfig

fromShelleyLedgerExamples
  :: forall era.
     ( ShelleyBasedEra era
     )
  => ShelleyLedgerExamples era
  -> Golden.Examples (ShelleyBlock era)
fromShelleyLedgerExamples ShelleyLedgerExamples {
                            sleResultExamples = ShelleyResultExamples{..}
                            , ..} =
  Golden.Examples {
      exampleBlock            = unlabelled blk
    , exampleSerialisedBlock  = unlabelled serialisedBlock
    , exampleHeader           = unlabelled $ getHeader blk
    , exampleSerialisedHeader = unlabelled serialisedHeader
    , exampleHeaderHash       = unlabelled hash
    , exampleGenTx            = unlabelled tx
    , exampleGenTxId          = unlabelled $ txId tx
    , exampleApplyTxErr       = unlabelled sleApplyTxError
    , exampleQuery            = queries
    , exampleResult           = results
    , exampleAnnTip           = unlabelled annTip
    , exampleLedgerState      = unlabelled ledgerState
    , exampleChainDepState    = unlabelled chainDepState
    , exampleExtLedgerState   = unlabelled extLedgerState
    , exampleSlotNo           = unlabelled slotNo
    , examplesLedgerTables    = unlabelled ledgerTables
    }
  where
    blk = mkShelleyBlock sleBlock
    hash = ShelleyHash sleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkShelleyTx sleTx
    slotNo = SlotNo 42
    serialisedHeader =
      SerialisedHeaderFromDepPair $ GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")
    queries = labelled [
          ("GetLedgerTip",              SomeQuery GetLedgerTip)
        , ("GetEpochNo",                SomeQuery GetEpochNo)
        , ("GetCurrentPParams",         SomeQuery GetCurrentPParams)
        , ("GetProposedPParamsUpdates", SomeQuery GetProposedPParamsUpdates)
        , ("GetStakeDistribution",      SomeQuery GetStakeDistribution)
        , ("GetNonMyopicMemberRewards", SomeQuery $ GetNonMyopicMemberRewards sleRewardsCredentials)
        , ("GetGenesisConfig",          SomeQuery GetGenesisConfig)
      ]
    results = labelled [
          ("LedgerTip",              SomeResult GetLedgerTip (blockPoint blk))
        , ("EpochNo",                SomeResult GetEpochNo 10)
        , ("EmptyPParams",           SomeResult GetCurrentPParams srePParams)
        , ("ProposedPParamsUpdates", SomeResult GetProposedPParamsUpdates sreProposedPPUpdates)
        , ("StakeDistribution",      SomeResult GetStakeDistribution srePoolDistr)
        , ("NonMyopicMemberRewards", SomeResult (GetNonMyopicMemberRewards Set.empty)
                                     (NonMyopicMemberRewards $ sreNonMyopicRewards))
        , ("GenesisConfig",          SomeResult GetGenesisConfig (compactGenesis sreShelleyGenesis))
        ]
    annTip = AnnTip {
        annTipSlotNo  = SlotNo 14
      , annTipBlockNo = BlockNo 6
      , annTipInfo    = hash
      }
    ledgerState = ShelleyLedgerState {
        shelleyLedgerTip        = NotOrigin ShelleyTip {
                                    shelleyTipSlotNo  = SlotNo 9
                                  , shelleyTipBlockNo = BlockNo 3
                                  , shelleyTipHash    = hash
                                  }
    , shelleyLedgerState      = sleNewEpochState
    , shelleyLedgerTransition = ShelleyTransitionInfo {shelleyAfterVoting = 0}
    , shelleyLedgerTables     = ShelleyLedgerTables ApplyEmptyMK
    }
    chainDepState = TPraosState (NotOrigin 1) sleChainDepState
    extLedgerState = ExtLedgerState
                       ledgerState
                       (genesisHeaderState chainDepState)
    ledgerTables = ShelleyLedgerTables
                 $ Basics.ApplyValuesMK
                 $ HD.UtxoValues
                 $ Map.fromList
                 $ zip exampleTxIns exampleTxOuts
      where
        exampleTxIns :: [TxIn (Crypto era)]
        exampleTxIns  =
          case toList $ getAllTxInputs $ getField @"body" sleTx of
            [] -> error "No transaction inputs were provided to construct the ledger tables"
                  -- We require at least one transaction input (and one
                  -- transaction output) in the example provided by
                  -- cardano-ledger to make sure that we test the serialization
                  -- of ledger tables with at least one non-trivial example.
            xs -> xs

        exampleTxOuts :: [LC.TxOut era]
        exampleTxOuts =
          case toList $ getField @"outputs" $ getField @"body" sleTx of
            [] -> error "No transaction outputs were provided to construct the ledger tables"
            xs -> xs

examplesShelley :: Golden.Examples (ShelleyBlock StandardShelley)
examplesShelley = fromShelleyLedgerExamples ledgerExamplesShelley

examplesAllegra :: Golden.Examples (ShelleyBlock StandardAllegra)
examplesAllegra = fromShelleyLedgerExamples ledgerExamplesAllegra

examplesMary :: Golden.Examples (ShelleyBlock StandardMary)
examplesMary = fromShelleyLedgerExamples ledgerExamplesMary

examplesAlonzo :: Golden.Examples (ShelleyBlock StandardAlonzo)
examplesAlonzo = fromShelleyLedgerExamples ledgerExamplesAlonzo
