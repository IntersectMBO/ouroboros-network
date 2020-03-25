{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Byron.Ledger.Golden (tests) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Read (deserialiseFromBytes)
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
import           Test.Tasty.HUnit

import           Test.Util.Golden

import qualified Test.Cardano.Chain.Common.Example as CC
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import qualified Test.Cardano.Chain.UTxO.Example as CC

tests :: TestTree
tests = testGroup "Golden tests"
    -- Note that for most Byron types, we simply wrap the en/decoders from
    -- cardano-ledger, which already has golden tests for them.
    [ testCase "ConsensusState" test_golden_ConsensusState
    , testCase "LedgerState"    test_golden_LedgerState
    , testCase "GenTxId"        test_golden_GenTxId
    , testCase "UPIState"       test_golden_UPIState
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

test_golden_ConsensusState :: Assertion
test_golden_ConsensusState = goldenTestCBOR
    encodeByronConsensusState
    exampleConsensusState
    "5KsVUPk4C9gPGzk962FEKaoU6kPaz2k4KVpi2fE5qaiaxo1MgXJqJDnTMoweHxYtEHvFs1rFeT\
    \4RJjr3KJjbQnk8HhiXMPsTfvFWpB21c5n1ahrriCLLZZJobKAA5CfPTsxQDrjpzgSUapATa3sm\
    \v5HfbodCC1zhsPshcXAgCJPKMpYnUSex1RbLhUBprEEUeVf3Bw3MZTKzAfvtUpVNEccHrwTxuB\
    \1uhVZMGVSnfD"

test_golden_LedgerState :: Assertion
test_golden_LedgerState = goldenTestCBOR
    encodeByronLedgerState
    exampleLedgerState
    "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\
    \31AbsSd929BCMfEwLiu6rdVVUHVjLVxMcB"
  where
    exampleLedgerState = ByronLedgerState
      { byronLedgerState       = initState
      , byronDelegationHistory = DH.empty
      }

    initState :: CC.Block.ChainValidationState
    Right initState = runExcept $
      CC.Block.initialChainValidationState CC.dummyConfig

test_golden_GenTxId :: Assertion
test_golden_GenTxId = goldenTestCBOR
    encodeByronGenTxId
    exampleGenTxId
    "zFiB1AioTx9ZECNWnow6QF1zAY5GZ1GiCcrUPjr7VLbhwCiks"
  where
    exampleGenTxId = ByronTxId CC.exampleTxId

test_golden_UPIState :: Assertion
test_golden_UPIState = goldenTestCBOR
    toCBOR
    exampleUPIState
    "oMJKmAEX4RGnt3C6se8QPSkJdq9sxGgj285fafwYAVJRAqJANEwmoeXVYC2CTAHnoxh8rW6Wtv\
    \aaoykPREqSC2vMQEjaRXC9GFKAj2PPxrmfy7xBhoEE3zoEyQq4Lox4vXpFRrCYJnW3S3YDVSMa\
    \JrYaPLAfZw1b9yVzAN1sSDfaXDsbRrU8RxraEy5"
  where
    exampleUPIState = CC.UPI.initialState CC.dummyConfig

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
