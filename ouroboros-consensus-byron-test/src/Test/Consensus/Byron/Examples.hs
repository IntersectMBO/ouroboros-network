{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Byron.Examples (
    -- * Setup
    secParam
  , windowSize
  , CC.dummyConfig
  , cfg
  , codecConfig
  , leaderCredentials
    -- * Examples
  , examples
  , exampleChainDepState
  , exampleLedgerState
  , exampleHeaderState
  , exampleExtLedgerState
  , exampleHeaderHash
  , exampleGenTx
  , exampleGenTxId
  , exampleApplyTxErr
  ) where

import           Control.Monad.Except (runExcept)
import qualified Data.ByteString.Lazy.Char8 as Lazy8
import qualified Data.Sequence.Strict as Seq

import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Byron.API as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import qualified Cardano.Chain.UTxO as CC

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Protocol.PBFT.State.HeaderHashBytes
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation

import           Ouroboros.Consensus.Byron.Crypto.DSIGN (SignKeyDSIGN (..))
import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.DelegationHistory as DH
import           Ouroboros.Consensus.Byron.Node (ByronLeaderCredentials (..))

import           Test.Util.Serialisation.Golden (Labelled, labelled, unlabelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import qualified Test.Cardano.Chain.Common.Example as CC
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import qualified Test.Cardano.Chain.Update.Example as CC
import qualified Test.Cardano.Chain.UTxO.Example as CC

import           Test.ThreadNet.Infra.Byron.ProtocolInfo (mkLeaderCredentials)

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

-- | Note that we must use the same value for the 'SecurityParam' as for the
-- 'S.WindowSize', because 'decodeByronChainDepState' only takes the
-- 'SecurityParam' and uses it as the basis for the 'S.WindowSize'.
secParam :: SecurityParam
secParam = SecurityParam 2

windowSize :: S.WindowSize
windowSize = S.WindowSize 2

cfg :: BlockConfig ByronBlock
cfg = ByronConfig {
      byronGenesisConfig   = CC.dummyConfig
    , byronProtocolVersion = CC.exampleProtocolVersion
    , byronSoftwareVersion = CC.exampleSoftwareVersion
    }

codecConfig :: CodecConfig ByronBlock
codecConfig = mkByronCodecConfig CC.dummyConfig

fullBlockConfig :: FullBlockConfig (LedgerState ByronBlock) ByronBlock
fullBlockConfig = FullBlockConfig {
      blockConfigLedger = CC.dummyConfig
    , blockConfigBlock  = cfg
    , blockConfigCodec  = codecConfig
    }

leaderCredentials :: ByronLeaderCredentials
leaderCredentials =
    mkLeaderCredentials
      CC.dummyConfig
      CC.dummyGeneratedSecrets
      (CoreNodeId 0)

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: Golden.Examples ByronBlock
examples = Golden.Examples {
      exampleBlock            = regularAndEBB exampleBlock            exampleEBB
    , exampleSerialisedBlock  = regularAndEBB exampleSerialisedBlock  exampleSerialisedEBB
    , exampleHeader           = regularAndEBB exampleHeader           exampleEBBHeader
    , exampleSerialisedHeader = regularAndEBB exampleSerialisedHeader exampleSerialisedEBBHeader
    , exampleHeaderHash       = unlabelled exampleHeaderHash
    , exampleGenTx            = unlabelled exampleGenTx
    , exampleGenTxId          = unlabelled exampleGenTxId
    , exampleApplyTxErr       = unlabelled exampleApplyTxErr
    , exampleQuery            = unlabelled exampleQuery
    , exampleResult           = unlabelled exampleResult
    , exampleAnnTip           = unlabelled exampleAnnTip
    , exampleLedgerState      = unlabelled exampleLedgerState
    , exampleChainDepState    = unlabelled exampleChainDepState
    , exampleExtLedgerState   = unlabelled exampleExtLedgerState
    }
  where
    regularAndEBB :: a -> a -> Labelled a
    regularAndEBB regular ebb = labelled [("regular", regular), ("EBB", ebb)]

    exampleQuery  = SomeBlock GetUpdateInterfaceState
    exampleResult = SomeResult GetUpdateInterfaceState exampleUPIState

exampleBlock :: ByronBlock
exampleBlock =
    forgeRegularBlock
      cfg
      (BlockNo 1)
      (SlotNo 1)
      (applyChainTick CC.dummyConfig (SlotNo 1) ledgerStateAfterEBB)
      [exampleGenTx]
      (fakeMkIsLeader leaderCredentials)
  where
    -- | Normally, we'd have to use 'checkIsLeader' to produce this proof.
    fakeMkIsLeader (ByronLeaderCredentials signKey dlgCert _) = PBftIsLeader {
          pbftIsLeaderSignKey = SignKeyByronDSIGN signKey
        , pbftIsLeaderDlgCert = dlgCert
        }

exampleEBB :: ByronBlock
exampleEBB = forgeEBB cfg (SlotNo 0) (BlockNo 0) GenesisHash

exampleSerialisedBlock :: Serialised ByronBlock
exampleSerialisedBlock = Serialised "<BLOCK>"

exampleSerialisedEBB :: Serialised ByronBlock
exampleSerialisedEBB = Serialised "<EBB>"

exampleHeader :: Header ByronBlock
exampleHeader = getHeader exampleBlock

exampleEBBHeader :: Header ByronBlock
exampleEBBHeader = getHeader exampleEBB

exampleSerialisedHeader :: SerialisedHeader ByronBlock
exampleSerialisedHeader = SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt (CtxtByronRegular 100)) (Serialised "<HEADER>")

exampleSerialisedEBBHeader :: SerialisedHeader ByronBlock
exampleSerialisedEBBHeader = SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt (CtxtByronBoundary 100)) (Serialised "<EBB_HEADER>")

exampleAnnTip :: AnnTip ByronBlock
exampleAnnTip = AnnTip {
      annTipSlotNo  = SlotNo 37
    , annTipBlockNo = BlockNo 23
    , annTipInfo    = TipInfoIsEBB exampleHeaderHash IsNotEBB
    }

exampleChainDepState :: ChainDepState (BlockProtocol ByronBlock)
exampleChainDepState = withEBB
  where
    signers = map (`S.PBftSigner` CC.exampleKeyHash) [1..4]

    withoutEBB = S.fromList
      secParam
      windowSize
      (NotOrigin 2, Seq.fromList signers, S.NothingEbbInfo)

    -- info about an arbitrary hypothetical EBB
    exampleEbbSlot            :: SlotNo
    exampleEbbHeaderHashBytes :: HeaderHashBytes
    exampleEbbSlot            = 6
    exampleEbbHeaderHashBytes = mkHeaderHashBytesForTestingOnly
                                  (Lazy8.pack "test_golden_ChainDepState6")

    withEBB = S.appendEBB secParam windowSize
                exampleEbbSlot exampleEbbHeaderHashBytes
                withoutEBB

emptyLedgerState :: LedgerState ByronBlock
emptyLedgerState = ByronLedgerState {
      byronLedgerState       = initState
    , byronDelegationHistory = DH.empty
    }
  where
    initState :: CC.Block.ChainValidationState
    Right initState = runExcept $
      CC.Block.initialChainValidationState CC.dummyConfig

ledgerStateAfterEBB :: LedgerState ByronBlock
ledgerStateAfterEBB =
      reapplyLedgerBlock fullBlockConfig exampleEBB
    . applyChainTick CC.dummyConfig (SlotNo 0)
    $ emptyLedgerState

exampleLedgerState :: LedgerState ByronBlock
exampleLedgerState =
      reapplyLedgerBlock fullBlockConfig exampleBlock
    . applyChainTick CC.dummyConfig (SlotNo 1)
    $ ledgerStateAfterEBB

exampleHeaderState :: HeaderState ByronBlock
exampleHeaderState = (genesisHeaderState S.empty) {
      headerStateTips = Seq.singleton exampleAnnTip
    }

exampleExtLedgerState :: ExtLedgerState ByronBlock
exampleExtLedgerState = ExtLedgerState {
      ledgerState = exampleLedgerState
    , headerState = exampleHeaderState
    }

exampleHeaderHash :: ByronHash
exampleHeaderHash = blockHash exampleBlock

exampleGenTx :: GenTx ByronBlock
exampleGenTx = ByronTx CC.exampleTxId (CC.annotateTxAux CC.exampleTxAux)

exampleGenTxId :: TxId (GenTx ByronBlock)
exampleGenTxId = ByronTxId CC.exampleTxId

exampleUPIState :: CC.UPI.State
exampleUPIState = CC.UPI.initialState CC.dummyConfig

exampleApplyTxErr :: CC.ApplyMempoolPayloadErr
exampleApplyTxErr =
      CC.MempoolTxErr
    $ CC.UTxOValidationTxValidationError
    $ CC.TxValidationLovelaceError "a"
    $ CC.LovelaceOverflow 0
