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
  , examplesBabbage
  , examplesConway
  , examplesMary
  , examplesShelley
  ) where

import qualified Cardano.Ledger.Block as SL
import           Cardano.Ledger.Crypto (Crypto)
import qualified Cardano.Protocol.TPraos.BHeader as SL
import           Data.Coerce (coerce)
import qualified Data.Set as Set
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.Praos.Header
                     (HeaderBody (HeaderBody))
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import           Ouroboros.Consensus.Protocol.Praos.Translate ()
import           Ouroboros.Consensus.Protocol.TPraos (TPraos,
                     TPraosState (TPraosState))
import           Ouroboros.Consensus.Protocol.Translate (TranslateProto,
                     translateChainDepState)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.HFEras
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol.TPraos ()
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Network.Block (Serialised (..))
import           Test.Cardano.Ledger.Allegra.Examples.Consensus
                     (ledgerExamplesAllegra)
import           Test.Cardano.Ledger.Alonzo.Examples.Consensus
                     (ledgerExamplesAlonzo)
import           Test.Cardano.Ledger.Babbage.Examples.Consensus
                     (ledgerExamplesBabbage)
import           Test.Cardano.Ledger.Conway.Examples.Consensus
                     (ledgerExamplesConway)
import           Test.Cardano.Ledger.Mary.Examples.Consensus
                     (ledgerExamplesMary)
import           Test.Cardano.Ledger.Shelley.Examples.Consensus
                     (ShelleyLedgerExamples (..), ShelleyResultExamples (..),
                     ledgerExamplesShelley, testShelleyGenesis)
import           Test.Util.Orphans.Arbitrary ()
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Golden (labelled, unlabelled)
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))


{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

codecConfig :: CodecConfig StandardShelleyBlock
codecConfig = ShelleyCodecConfig

fromShelleyLedgerExamples
  :: ShelleyCompatible (TPraos (EraCrypto era)) era
  => ShelleyLedgerExamples era
  -> Golden.Examples (ShelleyBlock (TPraos (EraCrypto era)) era)
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
    }
  where
    blk = mkShelleyBlock sleBlock
    hash = ShelleyHash $ SL.unHashHeader sleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkShelleyTx sleTx
    slotNo = SlotNo 42
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

-- | TODO Factor this out into something nicer.
fromShelleyLedgerExamplesPraos ::
  forall era.
  ( ShelleyCompatible (Praos (EraCrypto era)) era,
    TranslateProto (TPraos (EraCrypto era)) (Praos (EraCrypto era))
  )
  => ShelleyLedgerExamples era
  -> Golden.Examples (ShelleyBlock (Praos (EraCrypto era)) era)
fromShelleyLedgerExamplesPraos ShelleyLedgerExamples {
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
    }
  where
    blk = mkShelleyBlock $ let
      SL.Block hdr1 bdy = sleBlock in SL.Block (translateHeader hdr1) bdy

    translateHeader :: Crypto c => SL.BHeader c -> Praos.Header c
    translateHeader (SL.BHeader bhBody bhSig) =
        Praos.Header hBody hSig
      where
        hBody = HeaderBody {
          hbBlockNo = SL.bheaderBlockNo bhBody,
          hbSlotNo = SL.bheaderSlotNo bhBody,
          hbPrev = SL.bheaderPrev bhBody,
          hbVk = SL.bheaderVk bhBody,
          hbVrfVk = SL.bheaderVrfVk bhBody,
          hbVrfRes = coerce $ SL.bheaderEta bhBody,
          hbBodySize = fromIntegral $ SL.bsize bhBody,
          hbBodyHash = SL.bhash bhBody,
          hbOCert = SL.bheaderOCert bhBody,
          hbProtVer = SL.bprotver bhBody
        }
        hSig = coerce bhSig
    hash = ShelleyHash $ SL.unHashHeader sleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkShelleyTx sleTx
    slotNo = SlotNo 42
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
    chainDepState = translateChainDepState @(TPraos (EraCrypto era)) @(Praos (EraCrypto era))
      $ TPraosState (NotOrigin 1) sleChainDepState
    extLedgerState = ExtLedgerState
                       ledgerState
                       (genesisHeaderState chainDepState)



examplesShelley :: Golden.Examples StandardShelleyBlock
examplesShelley = fromShelleyLedgerExamples ledgerExamplesShelley

examplesAllegra :: Golden.Examples StandardAllegraBlock
examplesAllegra = fromShelleyLedgerExamples ledgerExamplesAllegra

examplesMary :: Golden.Examples StandardMaryBlock
examplesMary = fromShelleyLedgerExamples ledgerExamplesMary

examplesAlonzo :: Golden.Examples StandardAlonzoBlock
examplesAlonzo = fromShelleyLedgerExamples ledgerExamplesAlonzo

examplesBabbage :: Golden.Examples StandardBabbageBlock
examplesBabbage = fromShelleyLedgerExamplesPraos ledgerExamplesBabbage

examplesConway :: Golden.Examples StandardConwayBlock
examplesConway = fromShelleyLedgerExamplesPraos ledgerExamplesConway
