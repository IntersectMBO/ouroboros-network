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

import qualified Data.Set as Set

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Storage.Serialisation

import           Test.Cardano.Ledger.Shelley.Orphans ()

import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger

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
  :: ShelleyBasedEra era
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
    }
  where
    blk = mkShelleyBlock sleBlock
    hash = ShelleyHash sleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkShelleyTx sleTx
    serialisedHeader =
      SerialisedHeaderFromDepPair $ GenDepPair (NestedCtxt CtxtShelley) (Serialised "<HEADER>")
    queries = labelled [
          ("GetLedgerTip",              SomeSecond GetLedgerTip)
        , ("GetEpochNo",                SomeSecond GetEpochNo)
        , ("GetCurrentPParams",         SomeSecond GetCurrentPParams)
        , ("GetProposedPParamsUpdates", SomeSecond GetProposedPParamsUpdates)
        , ("GetStakeDistribution",      SomeSecond GetStakeDistribution)
        , ("GetNonMyopicMemberRewards", SomeSecond $ GetNonMyopicMemberRewards sleRewardsCredentials)
        , ("GetGenesisConfig",          SomeSecond GetGenesisConfig)
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
    }
    chainDepState = TPraosState (NotOrigin 1) sleChainDepState
    extLedgerState = ExtLedgerState
                       ledgerState
                       (genesisHeaderState chainDepState)

examplesShelley :: Golden.Examples (ShelleyBlock StandardShelley)
examplesShelley = fromShelleyLedgerExamples ledgerExamplesShelley

examplesAllegra :: Golden.Examples (ShelleyBlock StandardAllegra)
examplesAllegra = fromShelleyLedgerExamples ledgerExamplesAllegra

examplesMary :: Golden.Examples (ShelleyBlock StandardMary)
examplesMary = fromShelleyLedgerExamples ledgerExamplesMary

examplesAlonzo :: Golden.Examples (ShelleyBlock StandardAlonzo)
examplesAlonzo = fromShelleyLedgerExamples ledgerExamplesAlonzo
