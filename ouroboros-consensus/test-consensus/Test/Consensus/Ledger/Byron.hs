{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Ledger.Byron (tests) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import           Control.Monad.Except (runExcept)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy8
import qualified Data.Sequence.Strict as Seq

import           Cardano.Binary (fromCBOR, toCBOR)
import           Cardano.Chain.Block (ABlockOrBoundary (..))
import qualified Cardano.Chain.Block as CC.Block
import           Cardano.Chain.Common (KeyHash)
import           Cardano.Chain.Slotting (EpochSlots (..))
import qualified Cardano.Chain.Update as CC.Update
import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Network.Block (HeaderHash, SlotNo)
import           Ouroboros.Network.Point (WithOrigin (At))

import           Ouroboros.Consensus.Block (BlockProtocol, Header)
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Auxiliary
import qualified Ouroboros.Consensus.Ledger.Byron.DelegationHistory as DH
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Protocol.Abstract (ChainState,
                     SecurityParam (..))
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS
import           Ouroboros.Consensus.Protocol.PBFT.ChainState.HeaderHashBytes

import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..))

import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Test.Cardano.Chain.Block.Gen as CC
import qualified Test.Cardano.Chain.Common.Example as CC
import qualified Test.Cardano.Chain.Common.Gen as CC
import qualified Test.Cardano.Chain.Delegation.Gen as CC
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import qualified Test.Cardano.Chain.MempoolPayload.Gen as CC
import qualified Test.Cardano.Chain.Update.Gen as CC
import qualified Test.Cardano.Chain.UTxO.Example as CC
import qualified Test.Cardano.Chain.UTxO.Gen as CC
import qualified Test.Cardano.Crypto.Gen as CC

import           Test.Consensus.Protocol.PBFT (TestChainState (..))
import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Byron"
  [ testGroup "Serialisation roundtrips"
      [ testProperty "roundtrip Block"       prop_roundtrip_Block
      , testProperty "roundtrip Header"      prop_roundtrip_Header
      , testProperty "roundtrip HeaderHash"  prop_roundtrip_HeaderHash
      , testProperty "roundtrip ChainState"  prop_roundtrip_ChainState
      , testProperty "roundtrip GenTx"       prop_roundtrip_GenTx
      , testProperty "roundtrip GenTxId"     prop_roundtrip_GenTxId
      , testProperty "roundtrip ApplyTxErr"  prop_roundtrip_ApplyTxErr
      ]
      -- TODO LedgerState

  , testProperty "BinaryInfo sanity check"   prop_encodeByronBlockWithInfo
  , testGroup "HashInfo sanity check"
      [ testProperty "putHash/getHash roundtrip" prop_byronHashInfo_roundtrip
      , testProperty "hashSize"                  prop_byronHashInfo_hashSize
      ]

  , testGroup "Golden tests"
      -- Note that for most Byron types, we simply wrap the en/decoders from
      -- cardano-ledger, which already has golden tests for them.
      [ test_golden_ChainState
      , test_golden_ChainState_backwardsCompat_version0
      , test_golden_ChainState_backwardsCompat_version1
      , test_golden_ChainState_backwardsCompat_version2
      , test_golden_LedgerState
      , test_golden_GenTxId
      ]

  , testGroup "Integrity"
      [ testProperty "detect corruption in RegularBlock" prop_detectCorruption_RegularBlock
      ]

  ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

roundtrip :: (Eq a, Show a)
          => (a -> Encoding)
          -> (forall s. Decoder s a)
          -> a
          -> Property
roundtrip enc dec = roundtrip' enc (const <$> dec)

-- | Roundtrip property for values annotated with their serialized form
--
-- NOTE: Suppose @a@ consists of a pair of the unannotated value @a'@ and some
-- 'Lazy.ByteString'. The roundtrip property will fail if that
-- 'Lazy.ByteString' encoding is not equal to @enc a'@. One way in which this
-- might happen is if the annotation is not canonical CBOR, but @enc@ does
-- produce canonical CBOR.
roundtrip' :: (Eq a, Show a)
           => (a -> Encoding)  -- ^ @enc@
           -> (forall s. Decoder s (Lazy.ByteString -> a))
           -> a
           -> Property
roundtrip' enc dec a = case deserialiseFromBytes dec bs of
    Right (bs', a')
      | Lazy.null bs'
      -> a === a' bs
      | otherwise
      -> counterexample ("left-over bytes: " <> show bs') False
    Left e
      -> counterexample (show e) False
  where
    bs = toLazyByteString (enc a)

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: ByronBlock -> Property
prop_roundtrip_Block b =
    roundtrip' encodeByronBlock (decodeByronBlock epochSlots) b

prop_roundtrip_Header :: Header ByronBlock -> Property
prop_roundtrip_Header h =
    roundtrip' encodeByronHeader (decodeByronHeader epochSlots) h

prop_roundtrip_HeaderHash :: HeaderHash ByronBlock -> Property
prop_roundtrip_HeaderHash =
    roundtrip encodeByronHeaderHash decodeByronHeaderHash

-- | For now we test the roundtrip using mock crypto instead of real crypto
prop_roundtrip_ChainState :: TestChainState -> Property
prop_roundtrip_ChainState TestChainState{..} =
    roundtrip
      (CS.encodePBftChainState)
      (CS.decodePBftChainState testChainStateK testChainStateN)
      testChainState

prop_roundtrip_GenTx :: GenTx ByronBlock -> Property
prop_roundtrip_GenTx =
    roundtrip encodeByronGenTx decodeByronGenTx

prop_roundtrip_GenTxId :: GenTxId ByronBlock -> Property
prop_roundtrip_GenTxId =
    roundtrip encodeByronGenTxId decodeByronGenTxId

prop_roundtrip_ApplyTxErr :: ApplyTxErr ByronBlock -> Property
prop_roundtrip_ApplyTxErr =
    roundtrip encodeByronApplyTxError decodeByronApplyTxError

{-------------------------------------------------------------------------------
  BinaryInfo
-------------------------------------------------------------------------------}

prop_encodeByronBlockWithInfo :: ByronBlock -> Property
prop_encodeByronBlockWithInfo blk =
    headerAnnotation === extractedHeader
  where
    BinaryInfo { binaryBlob, headerOffset, headerSize } =
      encodeByronBlockWithInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        toLazyByteString binaryBlob

    headerAnnotation :: Lazy.ByteString
    headerAnnotation = Lazy.fromStrict $ case byronBlockRaw blk of
      ABOBBoundary b -> CC.Block.boundaryHeaderAnnotation $ CC.Block.boundaryHeader b
      ABOBBlock    b -> CC.Block.headerAnnotation         $ CC.Block.blockHeader    b

{-------------------------------------------------------------------------------
  HashInfo
-------------------------------------------------------------------------------}

prop_byronHashInfo_roundtrip :: ByronHash -> Property
prop_byronHashInfo_roundtrip h =
    case Get.runGetOrFail getHash serialisedHash of
      Left (_, _, e)
        -> counterexample e $ property False
      Right (unconsumed, _, h')
        | Lazy.null unconsumed
        -> h === h'
        | otherwise
        -> counterexample ("unconsumed bytes: " <> show unconsumed) $
           property False
  where
    HashInfo { getHash, putHash } = byronHashInfo

    serialisedHash = Put.runPut (putHash h)

prop_byronHashInfo_hashSize :: ByronHash -> Property
prop_byronHashInfo_hashSize h =
    hashSize === fromIntegral (Lazy.length serialisedHash)
  where
    HashInfo { hashSize, putHash } = byronHashInfo

    serialisedHash = Put.runPut (putHash h)

{-------------------------------------------------------------------------------
  Golden tests
-------------------------------------------------------------------------------}

-- | Note that we must use the same value for the 'SecurityParam' as for the
-- 'CS.WindowSize', because 'decodeByronChainState' only takes the
-- 'SecurityParam' and uses it as the basis for the 'CS.WindowSize'.
secParam :: SecurityParam
secParam = SecurityParam 2

windowSize :: CS.WindowSize
windowSize = CS.WindowSize 2

exampleChainStateWithoutEBB, exampleChainStateWithEBB :: ChainState (BlockProtocol ByronBlock)
(exampleChainStateWithoutEBB, exampleChainStateWithEBB) =
    (withoutEBB, withEBB)
  where
    signers = map (`CS.PBftSigner` CC.exampleKeyHash) [1..4]

    withoutEBB = CS.fromList
      secParam
      windowSize
      (At 2, Seq.fromList signers, CS.NothingEbbInfo)

    -- info about an arbitrary hypothetical EBB
    exampleEbbSlot            :: SlotNo
    exampleEbbHeaderHashBytes :: HeaderHashBytes
    exampleEbbSlot            = 6
    exampleEbbHeaderHashBytes = mkHeaderHashBytesForTestingOnly
                                  (Lazy8.pack "test_golden_ChainState6")

    withEBB = CS.appendEBB secParam windowSize
                exampleEbbSlot exampleEbbHeaderHashBytes
                withoutEBB

test_golden_ChainState :: TestTree
test_golden_ChainState = goldenTestCBOR
    "ChainState"
    encodeByronChainState
    exampleChainStateWithEBB
    "test-consensus/golden/cbor/byron/ChainState2"

test_golden_ChainState_backwardsCompat_version0 :: TestTree
test_golden_ChainState_backwardsCompat_version0 =
    testCase "ChainState version 0" $ goldenTestCBORBackwardsCompat
      (decodeByronChainState secParam)
      exampleChainStateWithoutEBB
      "test-consensus/golden/cbor/byron/ChainState0"

test_golden_ChainState_backwardsCompat_version1 :: TestTree
test_golden_ChainState_backwardsCompat_version1 =
    testCase "ChainState version 1" $ goldenTestCBORBackwardsCompat
      (decodeByronChainState secParam)
      exampleChainStateWithoutEBB
      "test-consensus/golden/cbor/byron/ChainState1"

test_golden_ChainState_backwardsCompat_version2 :: TestTree
test_golden_ChainState_backwardsCompat_version2 =
    testCase "ChainState version 2" $ goldenTestCBORBackwardsCompat
      (decodeByronChainState secParam)
      exampleChainStateWithEBB
      "test-consensus/golden/cbor/byron/ChainState2"

test_golden_LedgerState :: TestTree
test_golden_LedgerState = goldenTestCBOR
    "LedgerState"
    encodeByronLedgerState
    exampleLedgerState
    "test-consensus/golden/cbor/byron/LedgerState"
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
    "test-consensus/golden/cbor/byron/GenTxId"
  where
    exampleGenTxId = ByronTxId CC.exampleTxId

goldenTestCBOR :: String -> (a -> Encoding) -> a -> FilePath -> TestTree
goldenTestCBOR name enc a path =
    goldenVsString name path (return bs)
  where
    bs = toLazyByteString (enc a)

-- | Check whether we can successfully decode the contents of the given file.
-- This file will typically contain an older serialisation format.
goldenTestCBORBackwardsCompat
  :: (Eq a, Show a)
  => (forall s. Decoder s a)
  -> a
  -> FilePath
  -> Assertion
goldenTestCBORBackwardsCompat dec a path = do
    bytes <- Lazy.readFile path
    case deserialiseFromBytes dec bytes of
      Left failure
        -> assertFailure (show failure)
      Right (leftover, a')
        | Lazy.null leftover
        -> a' @?= a
        | otherwise
        -> assertFailure $ "Left-over bytes: " <> show leftover

{-------------------------------------------------------------------------------
  Integrity
-------------------------------------------------------------------------------}

-- | Test that we can detect random bitflips in blocks.
--
-- We cannot do this for EBBs, as they are not signed nor have a hash, so we
-- only test with regular blocks.
prop_detectCorruption_RegularBlock :: RegularBlock -> Corruption -> Property
prop_detectCorruption_RegularBlock (RegularBlock blk) =
    detectCorruption
      encodeByronBlock
      (decodeByronBlock epochSlots)
      (verifyBlockIntegrity testCfg)
      blk

testCfg :: NodeConfig ByronConsensusProtocol
testCfg = pInfoConfig $ protocolInfo prot
  where
    prot = ProtocolRealPBFT
      CC.dummyConfig
      (Just (PBftSignatureThreshold 0.5))
      (CC.Update.ProtocolVersion 1 0 0)
      (CC.Update.SoftwareVersion (CC.Update.ApplicationName "Cardano Test") 2)
      Nothing

newtype Corruption = Corruption Word
  deriving stock   (Show)
  deriving newtype (Arbitrary)

-- | Increment (overflow if necessary) the byte at position @i@ in the
-- bytestring, where @i = n `mod` length bs@.
--
-- If the bytestring is empty, return it unmodified.
applyCorruption :: Corruption -> Lazy.ByteString -> Lazy.ByteString
applyCorruption (Corruption n) bs
    | Lazy.null bs
    = bs
    | otherwise
    = before <> Lazy.cons (Lazy.head atAfter + 1) (Lazy.tail atAfter)
  where
    offset = fromIntegral n `mod` Lazy.length bs
    (before, atAfter) = Lazy.splitAt offset bs

-- | Serialise @a@, apply the given corruption, deserialise it, when that
-- fails, the corruption was detected. When deserialising the corrupted
-- bytestring succeeds, pass the deserialised value to the integrity checking
-- function. If that function returns 'False', the corruption was detected, if
-- it returns 'True', the corruption was not detected and the test fails.
detectCorruption
  :: Show a
  => (a -> Encoding)
  -> (forall s. Decoder s (Lazy.ByteString -> a))
  -> (a -> Bool)
     -- ^ Integrity check that should detect the corruption. Return 'False'
     -- when corrupt.
  -> a
  -> Corruption
  -> Property
detectCorruption enc dec isValid a cor = case deserialiseFromBytes dec bs of
    Right (_, mkA')
        | not (isValid a')
        -> label "corruption detected" $ property True
        | otherwise
        -> label "corruption not detected" $
           counterexample ("Corruption not detected: " <> show a') False
      where
        a' = mkA' bs
    Left _ -> label "corruption detected by decoder" $ property True
  where
    bs = applyCorruption cor $ toLazyByteString (enc a)

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

epochSlots :: EpochSlots
epochSlots = EpochSlots 21600

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 100

-- | A 'ByronBlock' that is never an EBB.
newtype RegularBlock = RegularBlock { unRegularBlock :: ByronBlock }
  deriving (Eq, Show)

instance Arbitrary RegularBlock where
  arbitrary =
    RegularBlock .annotateByronBlock epochSlots <$>
    hedgehog (CC.genBlock protocolMagicId epochSlots)

instance Arbitrary ByronBlock where
  arbitrary = frequency
      [ (3, genBlock)
      , (1, genBoundaryBlock)
      ]
    where
      genBlock :: Gen ByronBlock
      genBlock = unRegularBlock <$> arbitrary
      genBoundaryBlock :: Gen ByronBlock
      genBoundaryBlock =
        mkByronBlock epochSlots . ABOBBoundary . reAnnotateBoundary protocolMagicId <$>
        hedgehog (CC.genBoundaryBlock)


instance Arbitrary (Header ByronBlock) where
  arbitrary = frequency
      [ (3, genHeader)
      , (1, genBoundaryHeader)
      ]
    where
      genHeader :: Gen (Header ByronBlock)
      genHeader = do
        blockSize <- arbitrary
        mkByronHeader epochSlots . ABOBBlockHdr blockSize .
          reAnnotateUsing
            (CC.Block.toCBORHeader epochSlots)
            (CC.Block.fromCBORAHeader epochSlots) <$>
          hedgehog (CC.genHeader protocolMagicId epochSlots)

      genBoundaryHeader :: Gen (Header ByronBlock)
      genBoundaryHeader = do
        blockSize <- arbitrary
        mkByronHeader epochSlots . ABOBBoundaryHdr blockSize .
          reAnnotateUsing
            (CC.Block.toCBORABoundaryHeader protocolMagicId)
            CC.Block.fromCBORABoundaryHeader <$>
          hedgehog CC.genBoundaryHeader

instance Arbitrary ByronHash where
  arbitrary = ByronHash <$> hedgehog CC.genHeaderHash

instance Arbitrary KeyHash where
  arbitrary = hedgehog CC.genKeyHash

instance Arbitrary (GenTx ByronBlock) where
  arbitrary =
    fromMempoolPayload . reAnnotateUsing toCBOR fromCBOR <$>
    hedgehog (CC.genMempoolPayload protocolMagicId)

instance Arbitrary (GenTxId ByronBlock) where
  arbitrary = oneof
      [ ByronTxId             <$> hedgehog CC.genTxId
      , ByronDlgId            <$> hedgehog genCertificateId
      , ByronUpdateProposalId <$> hedgehog (CC.genUpId protocolMagicId)
      , ByronUpdateVoteId     <$> hedgehog genUpdateVoteId
      ]
    where
      genCertificateId = CC.genAbstractHash (CC.genCertificate protocolMagicId)
      genUpdateVoteId  = CC.genAbstractHash (CC.genVote protocolMagicId)

instance Arbitrary ApplyMempoolPayloadErr where
  arbitrary = oneof
    [ MempoolTxErr  <$> hedgehog CC.genUTxOValidationError
    , MempoolDlgErr <$> hedgehog CC.genError
    -- TODO there is no generator for
    -- Cardano.Chain.Update.Validation.Interface.Error and we can't write one
    -- either because the different Error types it wraps are not exported.
    -- , MempoolUpdateProposalErr <$> arbitrary
    -- , MempoolUpdateVoteErr     <$> arbitrary
    ]
