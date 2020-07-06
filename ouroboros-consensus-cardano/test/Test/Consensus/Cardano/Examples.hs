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
  , exampleLedgerStateWithSnapshot
  , exampleChainDepStateWithSnapshot
  ) where

import           Data.Bifunctor (first)
import           Data.Proxy (Proxy (..))
import           Data.SOP.Strict

import           Cardano.Crypto.Hash (ShortHash)

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation (AnnTip, HeaderState (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
                     (undistribAnnTip)
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
import           Test.Consensus.Shelley.MockCrypto

import           Test.Consensus.Cardano.Generators (toTelescope')

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

type Crypto = TPraosMockCrypto ShortHash

eraExamples :: NP Examples (CardanoEras Crypto)
eraExamples = Byron.examples :* Shelley.examples :* Nil

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
     Examples (ShelleyBlock Crypto)
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
byronEraParams = Byron.byronEraParams History.NoLowerBound Byron.dummyConfig

transitionEpoch :: EpochNo
transitionEpoch = 10

byronEndBound :: History.Bound
byronEndBound =
    History.mkUpperBound
      byronEraParams
      History.initBound
      transitionEpoch

shelleyStartBound :: History.Bound
shelleyStartBound = byronEndBound

eraInfoByron :: SingleEraInfo ByronBlock
eraInfoByron = singleEraInfo (Proxy @ByronBlock)

eraInfoShelley :: SingleEraInfo (ShelleyBlock Crypto)
eraInfoShelley = singleEraInfo (Proxy @(ShelleyBlock Crypto))

codecConfig :: CardanoCodecConfig Crypto
codecConfig = CardanoCodecConfig Byron.codecConfig Shelley.codecConfig

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
     Serialised (ShelleyBlock Crypto)
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
     SerialisedHeader (ShelleyBlock Crypto)
  -> SerialisedHeader (CardanoBlock Crypto)
serialisedHeaderShelley =
      serialisedHeaderFromPair
    . first (mapSomeNestedCtxt (NCS . NCZ))
    . serialisedHeaderToPair

headerHashByron :: HeaderHash ByronBlock -> HeaderHash (CardanoBlock Crypto)
headerHashByron = OneEraHash . toRawHash (Proxy @ByronBlock)

headerHashShelley :: HeaderHash (ShelleyBlock Crypto) -> HeaderHash (CardanoBlock Crypto)
headerHashShelley = OneEraHash . toRawHash (Proxy @(ShelleyBlock Crypto))

someQueryByron ::
     SomeBlock Query ByronBlock
  -> SomeBlock Query (CardanoBlock Crypto)
someQueryByron (SomeBlock q) = SomeBlock (QueryIfCurrentByron q)

someQueryShelley ::
     SomeBlock Query (ShelleyBlock Crypto)
  -> SomeBlock Query (CardanoBlock Crypto)
someQueryShelley (SomeBlock q) = SomeBlock (QueryIfCurrentShelley q)

someResultByron :: SomeResult ByronBlock -> SomeResult (CardanoBlock Crypto)
someResultByron (SomeResult q r) =
    SomeResult (QueryIfCurrentByron q) (QueryResultSuccess r)

someResultShelley :: SomeResult (ShelleyBlock Crypto) -> SomeResult (CardanoBlock Crypto)
someResultShelley (SomeResult q r) =
    SomeResult (QueryIfCurrentShelley q) (QueryResultSuccess r)

annTipByron :: AnnTip ByronBlock -> AnnTip (CardanoBlock Crypto)
annTipByron = undistribAnnTip . Z

annTipShelley :: AnnTip (ShelleyBlock Crypto) -> AnnTip (CardanoBlock Crypto)
annTipShelley = undistribAnnTip . S . Z

ledgerStateByron ::
     LedgerState ByronBlock
  -> LedgerState (CardanoBlock Crypto)
ledgerStateByron stByron =
    HardForkLedgerState $ toTelescope' (Proxy @LedgerState) (Left cur)
  where
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stByron
        }

ledgerStateShelley ::
     LedgerState (ShelleyBlock Crypto)
  -> LedgerState (CardanoBlock Crypto)
ledgerStateShelley stShelley =
    HardForkLedgerState $ toTelescope' (Proxy @LedgerState) (Right (past, cur))
  where
    past = State.Past {
          pastStart    = History.initBound
        , pastEnd      = byronEndBound
        , pastSnapshot = State.NoSnapshot
        }
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stShelley
        }

chainDepStateByron ::
     ChainDepState (BlockProtocol ByronBlock)
  -> ChainDepState (BlockProtocol (CardanoBlock Crypto))
chainDepStateByron stByron =
    toTelescope' (Proxy @WrapChainDepState) (Left cur)
  where
    cur = State.Current {
          currentStart = History.initBound
        , currentState = WrapChainDepState stByron
        }

chainDepStateShelley ::
     ChainDepState (BlockProtocol (ShelleyBlock Crypto))
  -> ChainDepState (BlockProtocol (CardanoBlock Crypto))
chainDepStateShelley stShelley =
    toTelescope' (Proxy @WrapChainDepState) (Right (past, cur))
  where
    past = State.Past {
          pastStart    = History.initBound
        , pastEnd      = byronEndBound
        , pastSnapshot = State.NoSnapshot
        }
    cur = State.Current {
          currentStart = History.initBound
        , currentState = WrapChainDepState stShelley
        }

extLedgerStateByron ::
     ExtLedgerState ByronBlock
  -> ExtLedgerState (CardanoBlock Crypto)
extLedgerStateByron ExtLedgerState {..} = ExtLedgerState {
      ledgerState = ledgerStateByron ledgerState
    , headerState = HeaderState {
          headerStateConsensus = chainDepStateByron (headerStateConsensus headerState)
        , headerStateTips      = annTipByron <$> headerStateTips   headerState
        , headerStateAnchor    = annTipByron <$> headerStateAnchor headerState
        }
    }

extLedgerStateShelley ::
     ExtLedgerState (ShelleyBlock Crypto)
  -> ExtLedgerState (CardanoBlock Crypto)
extLedgerStateShelley ExtLedgerState {..} = ExtLedgerState {
      ledgerState = ledgerStateShelley ledgerState
    , headerState = HeaderState {
          headerStateConsensus = chainDepStateShelley (headerStateConsensus headerState)
        , headerStateTips      = annTipShelley <$> headerStateTips   headerState
        , headerStateAnchor    = annTipShelley <$> headerStateAnchor headerState
        }
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
          ("AnytimeShelley", exampleQueryAnytimeShelley)
        ]
    , Golden.exampleResult = labelled [
          ("EraMismatchByron",   exampleResultEraMismatchByron)
        , ("EraMismatchShelley", exampleResultEraMismatchShelley)
        , ("AnytimeShelley",     exampleResultAnytimeShelley)
        ]
    , Golden.exampleLedgerState = labelled [
          ("WithSnapshot", exampleLedgerStateWithSnapshot)
        ]
    , Golden.exampleChainDepState = labelled [
          ("WithSnapshot", exampleChainDepStateWithSnapshot)
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

exampleQueryEraMismatchByron :: SomeBlock Query (CardanoBlock Crypto)
exampleQueryEraMismatchByron =
    SomeBlock (QueryIfCurrentShelley Shelley.GetLedgerTip)

exampleQueryEraMismatchShelley :: SomeBlock Query (CardanoBlock Crypto)
exampleQueryEraMismatchShelley =
    SomeBlock (QueryIfCurrentByron Byron.GetUpdateInterfaceState)

exampleQueryAnytimeShelley :: SomeBlock Query (CardanoBlock Crypto)
exampleQueryAnytimeShelley =
    SomeBlock (QueryAnytimeShelley EraStart)

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

exampleResultAnytimeShelley :: SomeResult (CardanoBlock Crypto)
exampleResultAnytimeShelley =
    SomeResult (QueryAnytimeShelley EraStart) (Just shelleyStartBound)

exampleLedgerStateWithSnapshot :: LedgerState (CardanoBlock Crypto)
exampleLedgerStateWithSnapshot =
    HardForkLedgerState $ toTelescope' (Proxy @LedgerState) (Right (past, cur))
  where
    (_, stByron)   = head $ Golden.exampleLedgerState Byron.examples
    (_, stShelley) = head $ Golden.exampleLedgerState Shelley.examples

    past = State.Past {
          pastStart    = History.initBound
        , pastEnd      = byronEndBound
        , pastSnapshot = State.Snapshot 1 stByron
        }
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stShelley
        }

exampleChainDepStateWithSnapshot ::
    ChainDepState (BlockProtocol (CardanoBlock Crypto))
exampleChainDepStateWithSnapshot =
    toTelescope' (Proxy @WrapChainDepState) (Right (past, cur))
  where
    (_, stByron)   = head $ Golden.exampleChainDepState Byron.examples
    (_, stShelley) = head $ Golden.exampleChainDepState Shelley.examples

    past = State.Past {
          pastStart    = History.initBound
        , pastEnd      = byronEndBound
        , pastSnapshot = State.Snapshot 1 (WrapChainDepState stByron)
        }
    cur = State.Current {
          currentStart = History.initBound
        , currentState = WrapChainDepState stShelley
        }
