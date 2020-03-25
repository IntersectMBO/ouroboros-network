{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
module Test.Consensus.Shelley.Ledger.Golden (
    tests
  , mkDummyHash
  ) where

import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Write as CBOR
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))

import           Cardano.Binary (toCBOR)

import           Ouroboros.Network.Block (SlotNo (..), blockHash, genesisPoint)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract

import qualified Cardano.Ledger.Shelley.Crypto as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.STS.Prtcl as STS
import qualified Test.Shelley.Spec.Ledger.Examples.Examples as Examples
import qualified Test.Shelley.Spec.Ledger.Utils as SL (testGlobals)

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import           Test.Tasty
import           Test.Tasty.Golden

import           Test.Util.Orphans.Arbitrary ()

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)

tests :: TestTree
tests = testGroup "Golden tests"
    [ test_golden_Block
    , test_golden_Header
    , test_golden_HeaderHash
    , test_golden_GenTx
    , test_golden_GenTxId
    , test_golden_ApplyTxErr
    , test_golden_ConsensusState
    , test_golden_LedgerState
      -- TODO Query and result
    ]

type Block = ShelleyBlock TPraosMockCrypto

exampleBlock :: Block
exampleBlock = mkShelleyBlock Examples.blockEx3B

exampleGenTx :: GenTx Block
exampleGenTx = mkShelleyTx Examples.txEx2A

test_golden_Block :: TestTree
test_golden_Block = goldenTestCBOR
    "Block"
    toCBOR
    exampleBlock
    "test/golden/cbor/shelley/Block"

test_golden_Header :: TestTree
test_golden_Header = goldenTestCBOR
    "Header"
    toCBOR
    (getHeader exampleBlock)
    "test/golden/cbor/shelley/Header"

test_golden_HeaderHash :: TestTree
test_golden_HeaderHash = goldenTestCBOR
    "HeaderHash"
    toCBOR
    (blockHash exampleBlock)
    "test/golden/cbor/shelley/HeaderHash"

test_golden_GenTx :: TestTree
test_golden_GenTx = goldenTestCBOR
    "GenTx"
    toCBOR
    exampleGenTx
    "test/golden/cbor/shelley/GenTx"

test_golden_GenTxId :: TestTree
test_golden_GenTxId = goldenTestCBOR
    "GenTxId"
    toCBOR
    (txId exampleGenTx)
    "test/golden/cbor/shelley/GenTxId"

test_golden_ApplyTxErr :: TestTree
test_golden_ApplyTxErr = goldenTestCBOR
    "ApplyTxErr"
    toCBOR
    exampleApplyTxErr
    "test/golden/cbor/shelley/ApplyTxErr"
  where
    -- TODO incomplete, this type has tons of constructors that can all
    -- change.
    exampleApplyTxErr :: ApplyTxErr Block
    exampleApplyTxErr =
        ApplyTxError
      $ pure
      $ STS.LedgerFailure
      $ STS.UtxowFailure
      $ STS.InvalidWitnessesUTXOW

test_golden_ConsensusState :: TestTree
test_golden_ConsensusState = goldenTestCBOR
    "ConsensusState"
    toCBOR
    exampleConsensusState
    "test/golden/cbor/shelley/ConsensusState"
  where
    exampleConsensusState :: ConsensusState (BlockProtocol Block)
    exampleConsensusState =
      TPraosState.append (mkPrtclState 2) $
      TPraosState.empty  (mkPrtclState 1)

    mkPrtclState :: SlotNo -> STS.PrtclState TPraosMockCrypto
    mkPrtclState slot = STS.PrtclState
      (Map.fromList
       [ (SL.DiscKeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 1), 1)
       , (SL.DiscKeyHash (mkDummyHash (Proxy @TPraosMockCrypto) 2), 2)
       ])
      (At SL.LastAppliedBlock {
          labBlockNo = 0
        , labSlotNo  = slot
        , labHash    = SL.HashHeader (mkDummyHash (Proxy @TPraosMockCrypto) 1)
        })
      SL.NeutralNonce
      (SL.mkNonce 1)
      (SL.mkNonce 2)
      (SL.mkNonce 3)

test_golden_LedgerState :: TestTree
test_golden_LedgerState = goldenTestCBOR
    "LedgerState"
    encodeShelleyLedgerState
    exampleLedgerState
    "test/golden/cbor/shelley/LedgerState"
  where
    Examples.CHAINExample { startState, newBlock } = Examples.ex2A

    exampleLedgerState :: LedgerState Block
    exampleLedgerState = reapplyLedgerBlock
      (ShelleyLedgerConfig SL.testGlobals)
      (mkShelleyBlock newBlock)
      (ShelleyLedgerState {
          ledgerTip    = genesisPoint
        , history      = History.empty
        , shelleyState = STS.chainNes startState
        })

goldenTestCBOR :: String -> (a -> Encoding) -> a -> FilePath -> TestTree
goldenTestCBOR name enc a path =
    goldenVsString name path (return bs)
  where
    bs = CBOR.toLazyByteString (enc a)


{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

mkDummyHash :: forall c a. Crypto c => Proxy c -> Int -> SL.Hash (SL.HASH c) a
mkDummyHash _ = coerce . SL.hash @(SL.HASH c)
