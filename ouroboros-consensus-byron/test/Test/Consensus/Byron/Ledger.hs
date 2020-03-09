{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Byron.Ledger (tests) where

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
import           Cardano.Chain.Block (ABlockOrBoundary (..),
                     ABlockOrBoundaryHdr (..))
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Byron.API as API
import           Cardano.Chain.Common (KeyHash)
import           Cardano.Chain.Slotting (EpochNumber, EpochSlots (..),
                     SlotNumber)
import qualified Cardano.Chain.Update as CC.Update
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI
import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Network.Block (HeaderHash, SlotNo)
import           Ouroboros.Network.Point (WithOrigin (At))
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block (BlockProtocol, Header)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import qualified Ouroboros.Consensus.Protocol.PBFT.State as S
import           Ouroboros.Consensus.Protocol.PBFT.State.HeaderHashBytes

import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..),
                     HashInfo (..))

import           Ouroboros.Consensus.Byron.Ledger
import qualified Ouroboros.Consensus.Byron.Ledger.DelegationHistory as DH
import           Ouroboros.Consensus.Byron.Node

import           Test.QuickCheck hiding (Result)
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
import qualified Test.Cardano.Chain.Slotting.Gen as CC
import qualified Test.Cardano.Chain.Update.Gen as UG
import qualified Test.Cardano.Chain.UTxO.Example as CC
import qualified Test.Cardano.Chain.UTxO.Gen as CC
import qualified Test.Cardano.Crypto.Gen as CC

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Roundtrip

tests :: TestTree
tests = testGroup "Byron"
  [ testGroup "Serialisation roundtrips"
      [ testProperty "roundtrip Block"       prop_roundtrip_Block
      , testProperty "roundtrip Header"      prop_roundtrip_Header
      , testProperty "roundtrip HeaderHash"  prop_roundtrip_HeaderHash
      , testProperty "roundtrip GenTx"       prop_roundtrip_GenTx
      , testProperty "roundtrip GenTxId"     prop_roundtrip_GenTxId
      , testProperty "roundtrip ApplyTxErr"  prop_roundtrip_ApplyTxErr
      , testProperty "roundtrip Query"       prop_roundtrip_Query
      , testProperty "roundtrip Result"      prop_roundtrip_Result
      ]
      -- TODO ConsensusState
      -- TODO LedgerState

  , testProperty "BinaryInfo sanity check"   prop_encodeByronBlockWithInfo
  , testGroup "HashInfo sanity check"
      [ testProperty "putHash/getHash roundtrip" prop_byronHashInfo_roundtrip
      , testProperty "hashSize"                  prop_byronHashInfo_hashSize
      ]

  , testGroup "Golden tests"
      -- Note that for most Byron types, we simply wrap the en/decoders from
      -- cardano-ledger, which already has golden tests for them.
      [ test_golden_ConsensusState
      , test_golden_LedgerState
      , test_golden_GenTxId
      , test_golden_UPIState
      ]

  , testGroup "Integrity"
      [ testProperty "detect corruption in RegularBlock" prop_detectCorruption_RegularBlock
      ]

  ]

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: ByronBlock -> Property
prop_roundtrip_Block b =
    roundtrip' encodeByronBlock (decodeByronBlock epochSlots) b

prop_roundtrip_Header :: SerialisationVersion ByronNetworkProtocolVersion
                      -> Header ByronBlock -> Property
prop_roundtrip_Header v h =
    roundtrip'
      (encodeByronHeader v)
      (decodeByronHeader epochSlots v)
      h'
  where
    h' = case v of
           SentAcrossNetwork ByronNetworkProtocolVersion1 ->
             -- This is a lossy format
             h { byronHeaderBlockSizeHint = fakeByronBlockSizeHint }
           _otherwise ->
             h

prop_roundtrip_HeaderHash :: HeaderHash ByronBlock -> Property
prop_roundtrip_HeaderHash =
    roundtrip encodeByronHeaderHash decodeByronHeaderHash

prop_roundtrip_GenTx :: GenTx ByronBlock -> Property
prop_roundtrip_GenTx =
    roundtrip encodeByronGenTx decodeByronGenTx

prop_roundtrip_GenTxId :: GenTxId ByronBlock -> Property
prop_roundtrip_GenTxId =
    roundtrip encodeByronGenTxId decodeByronGenTxId

prop_roundtrip_ApplyTxErr :: ApplyTxErr ByronBlock -> Property
prop_roundtrip_ApplyTxErr =
    roundtrip encodeByronApplyTxError decodeByronApplyTxError

prop_roundtrip_Query :: Some (Query ByronBlock) -> Property
prop_roundtrip_Query =
    roundtrip
      (\case { Some query -> encodeByronQuery query })
      decodeByronQuery

prop_roundtrip_Result :: CC.UPI.State -> Property
prop_roundtrip_Result =
    roundtrip
      (encodeByronResult GetUpdateInterfaceState)
      (decodeByronResult GetUpdateInterfaceState)

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
      (verifyBlockIntegrity (configBlock testCfg))
      blk

testCfg :: TopLevelConfig ByronBlock
testCfg = pInfoConfig $ protocolInfoByron
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
        mkByronBlock epochSlots . ABOBBoundary . API.reAnnotateBoundary protocolMagicId <$>
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
        flip (mkByronHeader epochSlots) blockSize . ABOBBlockHdr .
          API.reAnnotateUsing
            (CC.Block.toCBORHeader epochSlots)
            (CC.Block.fromCBORAHeader epochSlots) <$>
          hedgehog (CC.genHeader protocolMagicId epochSlots)

      genBoundaryHeader :: Gen (Header ByronBlock)
      genBoundaryHeader = do
        blockSize <- arbitrary
        flip (mkByronHeader epochSlots) blockSize . ABOBBoundaryHdr .
          API.reAnnotateUsing
            (CC.Block.toCBORABoundaryHeader protocolMagicId)
            CC.Block.fromCBORABoundaryHeader <$>
          hedgehog CC.genBoundaryHeader

instance Arbitrary ByronHash where
  arbitrary = ByronHash <$> hedgehog CC.genHeaderHash

instance Arbitrary KeyHash where
  arbitrary = hedgehog CC.genKeyHash

instance Arbitrary (GenTx ByronBlock) where
  arbitrary =
    fromMempoolPayload . API.reAnnotateUsing toCBOR fromCBOR <$>
    hedgehog (CC.genMempoolPayload protocolMagicId)

instance Arbitrary (GenTxId ByronBlock) where
  arbitrary = oneof
      [ ByronTxId             <$> hedgehog CC.genTxId
      , ByronDlgId            <$> hedgehog genCertificateId
      , ByronUpdateProposalId <$> hedgehog (UG.genUpId protocolMagicId)
      , ByronUpdateVoteId     <$> hedgehog genUpdateVoteId
      ]
    where
      genCertificateId = CC.genAbstractHash (CC.genCertificate protocolMagicId)
      genUpdateVoteId  = CC.genAbstractHash (UG.genVote protocolMagicId)

instance Arbitrary API.ApplyMempoolPayloadErr where
  arbitrary = oneof
    [ API.MempoolTxErr  <$> hedgehog CC.genUTxOValidationError
    , API.MempoolDlgErr <$> hedgehog CC.genError
    -- TODO there is no generator for
    -- Cardano.Chain.Update.Validation.Interface.Error and we can't write one
    -- either because the different Error types it wraps are not exported.
    -- , MempoolUpdateProposalErr <$> arbitrary
    -- , MempoolUpdateVoteErr     <$> arbitrary
    ]

instance Arbitrary (Some (Query ByronBlock)) where
  arbitrary = pure $ Some GetUpdateInterfaceState

instance Arbitrary EpochNumber where
  arbitrary = hedgehog CC.genEpochNumber

instance Arbitrary SlotNumber where
  arbitrary = hedgehog CC.genSlotNumber

instance Arbitrary CC.Update.UpId where
  arbitrary = hedgehog (UG.genUpId protocolMagicId)

instance Arbitrary CC.Update.ApplicationName where
  arbitrary = hedgehog UG.genApplicationName

instance Arbitrary CC.Update.SystemTag where
  arbitrary = hedgehog UG.genSystemTag

instance Arbitrary CC.Update.InstallerHash where
  arbitrary = hedgehog UG.genInstallerHash

instance Arbitrary CC.Update.ProtocolVersion where
  arbitrary = hedgehog UG.genProtocolVersion

instance Arbitrary CC.Update.ProtocolParameters where
  arbitrary = hedgehog UG.genProtocolParameters

instance Arbitrary CC.Update.SoftwareVersion where
  arbitrary = hedgehog UG.genSoftwareVersion

instance Arbitrary CC.UPI.State where
  arbitrary = CC.UPI.State
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure mempty -- TODO CandidateProtocolUpdate's constructor is not exported
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure mempty -- TODO Endorsement is not exported
    <*> arbitrary

instance Arbitrary (SerialisationVersion ByronNetworkProtocolVersion) where
  arbitrary = elements $ SerialisedToDisk : map SentAcrossNetwork versions
    where
      -- We enumerate /all/ versions here, not just the ones returned by
      -- 'supportedNetworkProtocolVersions', which may be fewer.
      -- We might want to reconsider that later.
      versions :: [ByronNetworkProtocolVersion]
      versions = [minBound .. maxBound]

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance Eq (Some (Query ByronBlock)) where
  Some GetUpdateInterfaceState == Some GetUpdateInterfaceState = True

deriving instance Show (Some (Query ByronBlock))
