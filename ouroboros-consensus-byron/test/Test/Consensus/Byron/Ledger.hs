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
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Byron.Ledger (tests) where

import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy

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

import           Ouroboros.Network.Block (HeaderHash)
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr, GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..),
                     HashInfo (..))

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node

import           Test.QuickCheck hiding (Result)
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Cardano.Chain.Block.Gen as CC
import qualified Test.Cardano.Chain.Common.Gen as CC
import qualified Test.Cardano.Chain.Delegation.Gen as CC
import qualified Test.Cardano.Chain.Genesis.Dummy as CC
import qualified Test.Cardano.Chain.MempoolPayload.Gen as CC
import qualified Test.Cardano.Chain.Slotting.Gen as CC
import qualified Test.Cardano.Chain.Update.Gen as UG
import qualified Test.Cardano.Chain.UTxO.Gen as CC
import qualified Test.Cardano.Crypto.Gen as CC

import           Test.Util.Corruption
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Roundtrip

import qualified Test.Consensus.Byron.Ledger.Golden as Golden

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

  , testGroup "Integrity"
      [ testProperty "detect corruption in RegularBlock" prop_detectCorruption_RegularBlock
      ]

  , Golden.tests
  ]

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: ByronBlock -> Property
prop_roundtrip_Block b =
    roundtrip' encodeByronBlock (decodeByronBlock epochSlots) b

prop_roundtrip_Header :: SerialisationVersion ByronBlock
                      -> Header ByronBlock -> Property
prop_roundtrip_Header v h =
    not (isNodeToClientVersion v) ==>
    roundtrip'
      (encodeByronHeader v)
      (decodeByronHeader epochSlots v)
      h'
  where
    h' = case v of
           SerialisedAcrossNetwork (SerialisedNodeToNode ByronNodeToNodeVersion1) ->
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

instance Arbitrary (SerialisationVersion ByronBlock) where
  arbitrary =
      elements $ concat [
          [SerialisedToDisk]
        , map (SerialisedAcrossNetwork . SerialisedNodeToNode)   nodeToNode
        , map (SerialisedAcrossNetwork . SerialisedNodeToClient) nodeToClient
        ]
    where
      -- We enumerate /all/ nodeToNode here, not just the ones returned by
      -- 'supportedNetworkProtocolVersions', which may be fewer.
      -- We might want to reconsider that later.

      nodeToNode :: [ByronNodeToNodeVersion]
      nodeToNode = [minBound .. maxBound]

      nodeToClient :: [ByronNodeToClientVersion]
      nodeToClient = [minBound .. maxBound]

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

instance Eq (Some (Query ByronBlock)) where
  Some GetUpdateInterfaceState == Some GetUpdateInterfaceState = True

deriving instance Show (Some (Query ByronBlock))
