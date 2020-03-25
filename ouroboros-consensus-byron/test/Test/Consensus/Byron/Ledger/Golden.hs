{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Byron.Ledger.Golden (tests) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Except (runExcept)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy8
import qualified Data.Sequence.Strict as Seq

import           Cardano.Binary (toCBOR)
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI

import           Ouroboros.Network.Block (SlotNo)
import           Ouroboros.Network.Point (WithOrigin (At))

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Protocol.PBFT.State.HeaderHashBytes

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.DelegationHistory as DH

import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit

import qualified Test.Cardano.Chain.Common.Example as CC
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import qualified Test.Cardano.Chain.UTxO.Example as CC

tests :: TestTree
tests = testGroup "Golden tests"
    -- Note that for most Byron types, we simply wrap the en/decoders from
    -- cardano-ledger, which already has golden tests for them.
    [ test_golden_ConsensusState
    , test_golden_LedgerState
    , test_golden_GenTxId
    , test_golden_UPIState
    ]

-- | Note that we must use the same value for the 'SecurityParam' as for the
-- 'S.WindowSize', because 'decodeByronConsensusState' only takes the
-- 'SecurityParam' and uses it as the basis for the 'S.WindowSize'.
secParam :: SecurityParam
secParam = SecurityParam 2

windowSize :: S.WindowSize
windowSize = S.WindowSize 2

exampleConsensusState :: ConsensusState (BlockProtocol ByronBlock)
exampleConsensusState = withEBB
  where
    signers = map (`S.PBftSigner` CC.exampleKeyHash) [1..4]

    withoutEBB = S.fromList
      secParam
      windowSize
      (At 2, Seq.fromList signers, S.NothingEbbInfo)

    -- info about an arbitrary hypothetical EBB
    exampleEbbSlot            :: SlotNo
    exampleEbbHeaderHashBytes :: HeaderHashBytes
    exampleEbbSlot            = 6
    exampleEbbHeaderHashBytes = mkHeaderHashBytesForTestingOnly
                                  (Lazy8.pack "test_golden_ConsensusState6")

    withEBB = S.appendEBB secParam windowSize
                exampleEbbSlot exampleEbbHeaderHashBytes
                withoutEBB

test_golden_ConsensusState :: TestTree
test_golden_ConsensusState = goldenTestCBOR
    "ConsensusState"
    encodeByronConsensusState
    exampleConsensusState
    "test/golden/cbor/byron/ConsensusState0"

test_golden_LedgerState :: TestTree
test_golden_LedgerState = goldenTestCBOR
    "LedgerState"
    encodeByronLedgerState
    exampleLedgerState
    "test/golden/cbor/byron/LedgerState"
  where
    exampleLedgerState = ByronLedgerState
      { byronLedgerState       = initState
      , byronDelegationHistory = DH.empty
      }

    initState :: CC.Block.ChainValidationState
    Right initState = runExcept $
      CC.Block.initialChainValidationState CC.dummyConfig

test_golden_GenTxId :: TestTree
test_golden_GenTxId = goldenTestCBOR
    "GenTxId"
    encodeByronGenTxId
    exampleGenTxId
    "test/golden/cbor/byron/GenTxId"
  where
    exampleGenTxId = ByronTxId CC.exampleTxId

test_golden_UPIState :: TestTree
test_golden_UPIState = goldenTestCBOR
    "CC.UPI.State"
    toCBOR
    exampleUPIState
    "test/golden/cbor/byron/UPIState"
  where
    exampleUPIState = CC.UPI.initialState CC.dummyConfig

goldenTestCBOR :: String -> (a -> Encoding) -> a -> FilePath -> TestTree
goldenTestCBOR name enc a path =
    goldenVsString name path (return bs)
  where
    bs = toLazyByteString (enc a)

-- | Check whether we can successfully decode the contents of the given file.
-- This file will typically contain an older serialisation format.
_goldenTestCBORBackwardsCompat
  :: (Eq a, Show a)
  => (forall s. Decoder s a)
  -> a
  -> FilePath
  -> Assertion
_goldenTestCBORBackwardsCompat dec a path = do
    bytes <- Lazy.readFile path
    case deserialiseFromBytes dec bytes of
      Left failure
        -> assertFailure (show failure)
      Right (leftover, a')
        | Lazy.null leftover
        -> a' @?= a
        | otherwise
        -> assertFailure $ "Left-over bytes: " <> show leftover
