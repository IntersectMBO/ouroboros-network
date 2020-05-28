{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Consensus.Shelley.Serialisation (tests) where

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)

import           Cardano.Binary (fromCBOR, toCBOR)

import           Ouroboros.Network.Block (HeaderHash)
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block (ConvertRawHash (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..))

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Protocol
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Corruption
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Roundtrip

import           Test.Consensus.Shelley.Generators
import           Test.Consensus.Shelley.MockCrypto

tests :: TestTree
tests = testGroup "Shelley"
    [ testGroup "Serialisation roundtrips"
        [ testProperty "roundtrip Block"          prop_roundtrip_Block
        , testProperty "roundtrip Header"         prop_roundtrip_Header
        , testProperty "roundtrip HeaderHash"     prop_roundtrip_HeaderHash
        , testProperty "roundtrip GenTx"          prop_roundtrip_GenTx
        , testProperty "roundtrip GenTxId"        prop_roundtrip_GenTxId
        , testProperty "roundtrip ApplyTxErr"     prop_roundtrip_ApplyTxErr
        , testProperty "roundtrip Query"          prop_roundtrip_Query
        , testProperty "roundtrip Result"         prop_roundtrip_Result
        , testProperty "roundtrip ConsensusState" prop_roundtrip_ConsensusState
        , testProperty "roundtrip LedgerState"    prop_roundtrip_LedgerState
        ]

    , testProperty "BinaryInfo sanity check" prop_encodeShelleyBlockWithInfo


    , testHashInfo (Proxy @TPraosStandardCrypto) "Real crypto"
    , testHashInfo (Proxy @TPraosMockCrypto)     "Mock crypto"

    , testGroup "Integrity"
        [ testProperty "generate non-corrupt blocks"  prop_blockIntegrity
        , testProperty "generate non-corrupt headers" prop_headerIntegrity
        , testProperty "detect corruption in blocks"  prop_detectCorruption_Block
        , testProperty "detect corruption in headers" prop_detectCorruption_Header
        ]
    ]
  where
    testHashInfo :: forall c. Crypto c => Proxy c -> String -> TestTree
    testHashInfo _ name = testGroup ("ConvertRawHash sanity check: " <> name)
      [ testProperty "fromRawHash/toRawHash" (prop_fromRawHash_toRawHash @c)
      , testProperty "hashSize"              (prop_hashSize  @c)
      ]

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: Block -> Property
prop_roundtrip_Block = roundtrip' encodeShelleyBlock decodeShelleyBlock

prop_roundtrip_Header :: Header Block -> Property
prop_roundtrip_Header = roundtrip' encodeShelleyHeader decodeShelleyHeader

prop_roundtrip_HeaderHash :: HeaderHash Block -> Property
prop_roundtrip_HeaderHash = roundtrip toCBOR fromCBOR

prop_roundtrip_GenTx :: GenTx Block -> Property
prop_roundtrip_GenTx = roundtrip toCBOR fromCBOR

prop_roundtrip_GenTxId :: GenTxId Block -> Property
prop_roundtrip_GenTxId = roundtrip toCBOR fromCBOR

prop_roundtrip_ApplyTxErr :: ApplyTxErr Block -> Property
prop_roundtrip_ApplyTxErr = roundtrip toCBOR fromCBOR

prop_roundtrip_Query :: Some (Query Block) -> Property
prop_roundtrip_Query =
    roundtrip
      (\case { Some query -> encodeShelleyQuery query })
      decodeShelleyQuery

prop_roundtrip_Result :: SomeResult -> Property
prop_roundtrip_Result (SomeResult query result) =
    roundtrip
      (encodeShelleyResult query)
      (decodeShelleyResult query)
      result

prop_roundtrip_ConsensusState
  :: TPraosState TPraosMockCrypto
  -> Property
prop_roundtrip_ConsensusState = roundtrip toCBOR fromCBOR

prop_roundtrip_LedgerState :: LedgerState Block -> Property
prop_roundtrip_LedgerState =
    roundtrip encodeShelleyLedgerState decodeShelleyLedgerState

{-------------------------------------------------------------------------------
  BinaryInfo
-------------------------------------------------------------------------------}

prop_encodeShelleyBlockWithInfo :: Block -> Property
prop_encodeShelleyBlockWithInfo blk =
    encodedHeader === shelleyAddHeaderEnvelope extractedHeader
  where
    BinaryInfo { binaryBlob, headerOffset, headerSize } =
      encodeShelleyBlockWithInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        CBOR.toLazyByteString binaryBlob

    encodedHeader :: Lazy.ByteString
    encodedHeader = CBOR.toLazyByteString $ encodeShelleyHeader (getHeader blk)

{-------------------------------------------------------------------------------
  ConvertRawHash
-------------------------------------------------------------------------------}

prop_fromRawHash_toRawHash
  :: forall c. Crypto c
  => HeaderHash (ShelleyBlock c) -> Property
prop_fromRawHash_toRawHash h =
    h === fromRawHash p (toRawHash p h)
  where
    p = Proxy @(ShelleyBlock c)

prop_hashSize
  :: forall c. Crypto c
  => HeaderHash (ShelleyBlock c) -> Property
prop_hashSize h =
    hashSize p === fromIntegral (Strict.length (toRawHash p h))
  where
    p = Proxy @(ShelleyBlock c)

{-------------------------------------------------------------------------------
  Integrity
-------------------------------------------------------------------------------}

-- TODO test with real crypto

testTPraosSlotsPerKESPeriod :: Word64
testTPraosSlotsPerKESPeriod = maxBound

-- | Test that the block we generate pass the 'verifyBlockIntegrity' check
prop_blockIntegrity :: Block -> Bool
prop_blockIntegrity = verifyBlockIntegrity testTPraosSlotsPerKESPeriod

-- | Test that the block we generate pass the 'verifyHeaderIntegrity' check
prop_headerIntegrity :: Header Block -> Bool
prop_headerIntegrity = verifyHeaderIntegrity testTPraosSlotsPerKESPeriod

-- | Test that we can detect random bitflips in blocks.
prop_detectCorruption_Block :: Block -> Corruption -> Property
prop_detectCorruption_Block =
    detectCorruption
      encodeShelleyBlock
      decodeShelleyBlock
      (verifyBlockIntegrity testTPraosSlotsPerKESPeriod)

-- | Test that we can detect random bitflips in blocks.
prop_detectCorruption_Header :: Header Block -> Corruption -> Property
prop_detectCorruption_Header =
    detectCorruption
      encodeShelleyHeader
      decodeShelleyHeader
      (verifyHeaderIntegrity testTPraosSlotsPerKESPeriod)
