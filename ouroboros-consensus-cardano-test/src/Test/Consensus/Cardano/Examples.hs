{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Test.Consensus.Cardano.Examples (
    -- * Setup
    codecConfig
    -- * Examples
  , examples
  , exampleEraMismatchByron
  , exampleEraMismatchShelley
  , exampleApplyTxErrWrongEraByron
  , exampleApplyTxErrWrongEraShelley
  , exampleQueryEraMismatchByron
  , exampleQueryEraMismatchShelley
  , exampleQueryAnytimeShelley
  , exampleResultEraMismatchByron
  , exampleResultEraMismatchShelley
  , exampleResultAnytimeShelley
  ) where

import           Data.Bifunctor (first)
import           Data.SOP.Strict

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation (AnnTip, HeaderState (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Counting (Exactly (..))

import           Ouroboros.Consensus.HardFork.Combinator
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger as Byron

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork ()

import           Test.Util.Serialisation.Golden (Examples, Labelled, labelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import qualified Test.Consensus.Byron.Examples as Byron

import qualified Test.Consensus.Shelley.Examples as Shelley

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

type Crypto = StandardCrypto

-- TODO #2669
eraExamples :: NP Examples (CardanoEras Crypto)
eraExamples = Byron.examples :* Shelley.examples :* mempty :* mempty :* Nil

-- | By using this function, we can't forget to update this test when adding a
-- new era to 'CardanoEras'.
combineEras ::
     NP Examples (CardanoEras Crypto)
  -> Examples (CardanoBlock Crypto)
combineEras = mconcat . hcollapse . hap eraInjections
  where
    eraInjections :: NP (Examples -.-> K (Examples (CardanoBlock Crypto)))
                        (CardanoEras Crypto)
    eraInjections =
           fn (K . injExamplesByron)
        :* fn (K . injExamplesShelley)
        -- TODO #2669
        :* fn (K . const mempty)
        :* fn (K . const mempty)
        :* Nil

-- Generalise in #2368
injExamplesByron :: Examples ByronBlock -> Examples (CardanoBlock Crypto)
injExamplesByron Golden.Examples {..} = Golden.Examples {
      exampleBlock            = inj exampleBlock            BlockByron
    , exampleSerialisedBlock  = inj exampleSerialisedBlock  serialisedBlockByron
    , exampleHeader           = inj exampleHeader           HeaderByron
    , exampleSerialisedHeader = inj exampleSerialisedHeader serialisedHeaderByron
    , exampleHeaderHash       = inj exampleHeaderHash       headerHashByron
    , exampleGenTx            = inj exampleGenTx            GenTxByron
    , exampleGenTxId          = inj exampleGenTxId          GenTxIdByron
    , exampleApplyTxErr       = inj exampleApplyTxErr       ApplyTxErrByron
    , exampleQuery            = inj exampleQuery            someQueryByron
    , exampleResult           = inj exampleResult           someResultByron
    , exampleAnnTip           = inj exampleAnnTip           annTipByron
    , exampleLedgerState      = inj exampleLedgerState      ledgerStateByron
    , exampleChainDepState    = inj exampleChainDepState    chainDepStateByron
    , exampleExtLedgerState   = inj exampleExtLedgerState   extLedgerStateByron
    }
  where
    inj :: Labelled byron -> (byron -> cardano) -> Labelled cardano
    inj byron f = [
        (Just label, f x)
      | (mbLabel, x) <- byron
      , let label = case mbLabel of
              Nothing  -> "Byron"
              Just lbl -> "Byron_" <> lbl
      ]

-- Generalise in #2368
injExamplesShelley ::
     Examples (ShelleyBlock StandardShelley)
  -> Examples (CardanoBlock Crypto)
injExamplesShelley Golden.Examples {..} = Golden.Examples {
      exampleBlock            = inj exampleBlock            BlockShelley
    , exampleSerialisedBlock  = inj exampleSerialisedBlock  serialisedBlockShelley
    , exampleHeader           = inj exampleHeader           HeaderShelley
    , exampleSerialisedHeader = inj exampleSerialisedHeader serialisedHeaderShelley
    , exampleHeaderHash       = inj exampleHeaderHash       headerHashShelley
    , exampleGenTx            = inj exampleGenTx            GenTxShelley
    , exampleGenTxId          = inj exampleGenTxId          GenTxIdShelley
    , exampleApplyTxErr       = inj exampleApplyTxErr       ApplyTxErrShelley
    , exampleQuery            = inj exampleQuery            someQueryShelley
    , exampleResult           = inj exampleResult           someResultShelley
    , exampleAnnTip           = inj exampleAnnTip           annTipShelley
    , exampleLedgerState      = inj exampleLedgerState      ledgerStateShelley
    , exampleChainDepState    = inj exampleChainDepState    chainDepStateShelley
    , exampleExtLedgerState   = inj exampleExtLedgerState   extLedgerStateShelley
    }
  where
    inj :: Labelled shelley -> (shelley -> cardano) -> Labelled cardano
    inj shelley f = [
        (Just label, f x)
      | (mbLabel, x) <- shelley
      , let label = case mbLabel of
              Nothing  -> "Shelley"
              Just lbl -> "Shelley_" <> lbl
      ]

byronEraParams :: History.EraParams
byronEraParams = Byron.byronEraParams History.NoLowerBound Byron.ledgerConfig

shelleyEraParams :: History.EraParams
shelleyEraParams = Shelley.shelleyEraParams History.NoLowerBound Shelley.testShelleyGenesis

allegraEraParams :: History.EraParams
allegraEraParams = Shelley.shelleyEraParams History.NoLowerBound Shelley.testShelleyGenesis

maryEraParams :: History.EraParams
maryEraParams = Shelley.shelleyEraParams History.NoLowerBound Shelley.testShelleyGenesis

transitionEpoch :: EpochNo
transitionEpoch = 10

byronStartBound :: History.Bound
byronStartBound = History.initBound

byronEndBound :: History.Bound
byronEndBound =
    History.mkUpperBound
      byronEraParams
      byronStartBound
      transitionEpoch

shelleyStartBound :: History.Bound
shelleyStartBound = byronEndBound

summary :: History.Summary (CardanoEras Crypto)
summary =
    State.reconstructSummary
      (History.Shape
        (Exactly
          (  K byronEraParams
          :* K shelleyEraParams
          :* K allegraEraParams
          :* K maryEraParams
          :* Nil
          )))
      (State.TransitionKnown transitionEpoch)
      (hardForkLedgerStatePerEra (ledgerStateByron byronLedger))
  where
    (_, byronLedger) = head $ Golden.exampleLedgerState Byron.examples

eraInfoByron :: SingleEraInfo ByronBlock
eraInfoByron = singleEraInfo (Proxy @ByronBlock)

eraInfoShelley :: SingleEraInfo (ShelleyBlock StandardShelley)
eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock StandardShelley))

codecConfig :: CardanoCodecConfig Crypto
codecConfig =
    CardanoCodecConfig
      Byron.codecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig
      Shelley.ShelleyCodecConfig

{-------------------------------------------------------------------------------
  Additional injections
-------------------------------------------------------------------------------}

-- | In reality, an era tag would be prepended, but we're testing that the
-- encoder doesn't care what the bytes are.
serialisedBlockByron :: Serialised ByronBlock -> Serialised (CardanoBlock Crypto)
serialisedBlockByron (Serialised _) = Serialised "<CARDANO_BLOCK>"

-- | In reality, an era tag would be prepended, but we're testing that the
-- encoder doesn't care what the bytes are.
serialisedBlockShelley ::
     Serialised (ShelleyBlock StandardShelley)
  -> Serialised (CardanoBlock Crypto)
serialisedBlockShelley (Serialised _) = Serialised "<CARDANO_BLOCK>"

serialisedHeaderByron ::
     SerialisedHeader ByronBlock
  -> SerialisedHeader (CardanoBlock Crypto)
serialisedHeaderByron =
      serialisedHeaderFromPair
    . first (mapSomeNestedCtxt NCZ)
    . serialisedHeaderToPair

serialisedHeaderShelley ::
     SerialisedHeader (ShelleyBlock StandardShelley)
  -> SerialisedHeader (CardanoBlock Crypto)
serialisedHeaderShelley =
      serialisedHeaderFromPair
    . first (mapSomeNestedCtxt (NCS . NCZ))
    . serialisedHeaderToPair

headerHashByron :: HeaderHash ByronBlock -> HeaderHash (CardanoBlock Crypto)
headerHashByron = OneEraHash . toShortRawHash (Proxy @ByronBlock)

headerHashShelley :: HeaderHash (ShelleyBlock StandardShelley) -> HeaderHash (CardanoBlock Crypto)
headerHashShelley = OneEraHash . toShortRawHash (Proxy @(ShelleyBlock StandardShelley))

someQueryByron ::
     SomeSecond Query ByronBlock
  -> SomeSecond Query (CardanoBlock Crypto)
someQueryByron (SomeSecond q) = SomeSecond (QueryIfCurrentByron q)

someQueryShelley ::
     SomeSecond Query (ShelleyBlock StandardShelley)
  -> SomeSecond Query (CardanoBlock Crypto)
someQueryShelley (SomeSecond q) = SomeSecond (QueryIfCurrentShelley q)

someResultByron :: SomeResult ByronBlock -> SomeResult (CardanoBlock Crypto)
someResultByron (SomeResult q r) =
    SomeResult (QueryIfCurrentByron q) (QueryResultSuccess r)

someResultShelley :: SomeResult (ShelleyBlock StandardShelley) -> SomeResult (CardanoBlock Crypto)
someResultShelley (SomeResult q r) =
    SomeResult (QueryIfCurrentShelley q) (QueryResultSuccess r)

annTipByron :: AnnTip ByronBlock -> AnnTip (CardanoBlock Crypto)
annTipByron = undistribAnnTip . Z

annTipShelley :: AnnTip (ShelleyBlock StandardShelley) -> AnnTip (CardanoBlock Crypto)
annTipShelley = undistribAnnTip . S . Z

ledgerStateByron ::
     LedgerState ByronBlock
  -> LedgerState (CardanoBlock Crypto)
ledgerStateByron stByron =
    HardForkLedgerState $ HardForkState $ TZ cur
  where
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stByron
        }

ledgerStateShelley ::
     LedgerState (ShelleyBlock StandardShelley)
  -> LedgerState (CardanoBlock Crypto)
ledgerStateShelley stShelley =
    HardForkLedgerState $ HardForkState $ TS (K past) (TZ cur)
  where
    past = State.Past {
          pastStart    = History.initBound
        , pastEnd      = byronEndBound
        }
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stShelley
        }

chainDepStateByron ::
     ChainDepState (BlockProtocol ByronBlock)
  -> ChainDepState (BlockProtocol (CardanoBlock Crypto))
chainDepStateByron stByron =
    HardForkState $ TZ cur
  where
    cur = State.Current {
          currentStart = History.initBound
        , currentState = WrapChainDepState stByron
        }

chainDepStateShelley ::
     ChainDepState (BlockProtocol (ShelleyBlock StandardShelley))
  -> ChainDepState (BlockProtocol (CardanoBlock Crypto))
chainDepStateShelley stShelley =
    HardForkState $ TS (K past) (TZ cur)
  where
    past = State.Past {
          pastStart    = History.initBound
        , pastEnd      = byronEndBound
        }
    cur = State.Current {
          currentStart = History.initBound
        , currentState = WrapChainDepState stShelley
        }

headerStateByron ::
     HeaderState ByronBlock
  -> HeaderState (CardanoBlock Crypto)
headerStateByron HeaderState {..} = HeaderState {
      headerStateTip      = annTipByron        <$> headerStateTip
    , headerStateChainDep = chainDepStateByron  $  headerStateChainDep
    }

headerStateShelley ::
     HeaderState (ShelleyBlock StandardShelley)
  -> HeaderState (CardanoBlock Crypto)
headerStateShelley HeaderState {..} = HeaderState {
      headerStateTip      = annTipShelley        <$> headerStateTip
    , headerStateChainDep = chainDepStateShelley  $  headerStateChainDep
    }

extLedgerStateByron ::
     ExtLedgerState ByronBlock
  -> ExtLedgerState (CardanoBlock Crypto)
extLedgerStateByron ExtLedgerState {..} = ExtLedgerState {
      ledgerState = ledgerStateByron ledgerState
    , headerState = headerStateByron headerState
    }

extLedgerStateShelley ::
     ExtLedgerState (ShelleyBlock StandardShelley)
  -> ExtLedgerState (CardanoBlock Crypto)
extLedgerStateShelley ExtLedgerState {..} = ExtLedgerState {
      ledgerState = ledgerStateShelley ledgerState
    , headerState = headerStateShelley headerState
    }

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | Multi-era examples, e.g., applying a transaction to the wrong era.
multiEraExamples :: Examples (CardanoBlock Crypto)
multiEraExamples = mempty {
      Golden.exampleApplyTxErr = labelled [
          ("WrongEraByron",   exampleApplyTxErrWrongEraByron)
        , ("WrongEraShelley", exampleApplyTxErrWrongEraShelley)
        ]
    , Golden.exampleQuery = labelled [
          ("AnytimeByron",   exampleQueryAnytimeByron)
        , ("AnytimeShelley", exampleQueryAnytimeShelley)
        , ("HardFork",       exampleQueryHardFork)
        ]
    , Golden.exampleResult = labelled [
          ("EraMismatchByron",   exampleResultEraMismatchByron)
        , ("EraMismatchShelley", exampleResultEraMismatchShelley)
        , ("AnytimeByron",       exampleResultAnytimeByron)
        , ("AnytimeShelley",     exampleResultAnytimeShelley)
        , ("HardFork",           exampleResultHardFork)
        ]
    }

-- | The examples: the examples from each individual era lifted in to
-- 'CardanoBlock' /and/ the multi-era examples.
examples :: Examples (CardanoBlock Crypto)
examples = combineEras eraExamples <> multiEraExamples

-- | Applying a Shelley thing to a Byron ledger
exampleEraMismatchByron :: MismatchEraInfo (CardanoEras Crypto)
exampleEraMismatchByron =
    MismatchEraInfo $ MR (Z eraInfoShelley) (LedgerEraInfo eraInfoByron)

-- | Applying a Byron thing to a Shelley ledger
exampleEraMismatchShelley :: MismatchEraInfo (CardanoEras Crypto)
exampleEraMismatchShelley =
    MismatchEraInfo $ ML eraInfoByron (Z (LedgerEraInfo eraInfoShelley))

exampleApplyTxErrWrongEraByron :: ApplyTxErr (CardanoBlock Crypto)
exampleApplyTxErrWrongEraByron =
      HardForkApplyTxErrWrongEra exampleEraMismatchByron

exampleApplyTxErrWrongEraShelley :: ApplyTxErr (CardanoBlock Crypto)
exampleApplyTxErrWrongEraShelley =
      HardForkApplyTxErrWrongEra exampleEraMismatchShelley

exampleQueryEraMismatchByron :: SomeSecond Query (CardanoBlock Crypto)
exampleQueryEraMismatchByron =
    SomeSecond (QueryIfCurrentShelley Shelley.GetLedgerTip)

exampleQueryEraMismatchShelley :: SomeSecond Query (CardanoBlock Crypto)
exampleQueryEraMismatchShelley =
    SomeSecond (QueryIfCurrentByron Byron.GetUpdateInterfaceState)

exampleQueryAnytimeByron :: SomeSecond Query (CardanoBlock Crypto)
exampleQueryAnytimeByron =
    SomeSecond (QueryAnytimeByron GetEraStart)

exampleQueryAnytimeShelley :: SomeSecond Query (CardanoBlock Crypto)
exampleQueryAnytimeShelley =
    SomeSecond (QueryAnytimeShelley GetEraStart)

exampleQueryHardFork :: SomeSecond Query (CardanoBlock Crypto)
exampleQueryHardFork =
    SomeSecond (QueryHardFork GetInterpreter)

exampleResultEraMismatchByron :: SomeResult (CardanoBlock Crypto)
exampleResultEraMismatchByron =
    SomeResult
      (QueryIfCurrentShelley Shelley.GetLedgerTip)
      (Left exampleEraMismatchByron)

exampleResultEraMismatchShelley :: SomeResult (CardanoBlock Crypto)
exampleResultEraMismatchShelley =
    SomeResult
      (QueryIfCurrentByron Byron.GetUpdateInterfaceState)
      (Left exampleEraMismatchShelley)

exampleResultAnytimeByron :: SomeResult (CardanoBlock Crypto)
exampleResultAnytimeByron =
    SomeResult (QueryAnytimeByron GetEraStart) (Just byronStartBound)

exampleResultAnytimeShelley :: SomeResult (CardanoBlock Crypto)
exampleResultAnytimeShelley =
    SomeResult (QueryAnytimeShelley GetEraStart) (Just shelleyStartBound)

exampleResultHardFork :: SomeResult (CardanoBlock Crypto)
exampleResultHardFork =
    SomeResult (QueryHardFork GetInterpreter) (History.mkInterpreter summary)
