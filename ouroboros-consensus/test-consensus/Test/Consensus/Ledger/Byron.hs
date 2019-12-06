{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Ledger.Byron (tests) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy

import           Cardano.Binary (fromCBOR, toCBOR)
import           Cardano.Chain.Block (ABlockOrBoundary (..))
import qualified Cardano.Chain.Block as CC.Block
import           Cardano.Chain.Common (KeyHash)
import           Cardano.Chain.Slotting (EpochSlots (..))
import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Network.Block (HeaderHash)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Byron.Auxiliary
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS

import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..))

import           Test.QuickCheck
import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Cardano.Chain.Block.Gen as CC
import qualified Test.Cardano.Chain.Common.Gen as CC
import qualified Test.Cardano.Chain.Delegation.Gen as CC
import qualified Test.Cardano.Chain.MempoolPayload.Gen as CC
import qualified Test.Cardano.Chain.Update.Gen as CC
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
  Generators
-------------------------------------------------------------------------------}

epochSlots :: EpochSlots
epochSlots = EpochSlots 21600

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 100

instance Arbitrary ByronBlock where
  arbitrary = frequency
      [ (3, genBlock)
      , (1, genBoundaryBlock)
      ]
    where
      genBlock :: Gen ByronBlock
      genBlock =
        annotateByronBlock epochSlots <$>
        hedgehog (CC.genBlock protocolMagicId epochSlots)
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
      genHeader =
        mkByronHeader epochSlots . ABOBBlockHdr .
        reAnnotateUsing
          (CC.Block.toCBORHeader epochSlots)
          (CC.Block.fromCBORAHeader epochSlots) <$>
        hedgehog (CC.genHeader protocolMagicId epochSlots)
      genBoundaryHeader :: Gen (Header ByronBlock)
      genBoundaryHeader =
        mkByronHeader epochSlots . ABOBBoundaryHdr .
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
