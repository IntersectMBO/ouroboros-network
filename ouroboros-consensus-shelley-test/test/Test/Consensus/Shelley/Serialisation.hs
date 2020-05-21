{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Consensus.Shelley.Serialisation (tests) where

import qualified Codec.CBOR.Write as CBOR
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)

import           Cardano.Binary (Annotator (..), FullByteString (..), fromCBOR,
                     serialize, toCBOR)

import           Ouroboros.Network.Block (HeaderHash)
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..),
                     HashInfo (..))

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
    testHashInfo _ name = testGroup ("HashInfo sanity check: " <> name)
      [ testProperty "putHash/getHash roundtrip" (prop_shelleyHashInfo_roundtrip @c)
      , testProperty "hashSize"                  (prop_shelleyHashInfo_hashSize  @c)
      ]

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: Block -> Property
prop_roundtrip_Block = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

prop_roundtrip_Header :: Header Block -> Property
prop_roundtrip_Header = roundtrip' toCBOR ((. Full) . runAnnotator <$> fromCBOR)

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
    encodedHeader = serialize (getHeader blk)

{-------------------------------------------------------------------------------
  HashInfo
-------------------------------------------------------------------------------}

prop_shelleyHashInfo_roundtrip :: Crypto c => ShelleyHash c -> Property
prop_shelleyHashInfo_roundtrip h =
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
    HashInfo { getHash, putHash } = shelleyHashInfo

    serialisedHash = Put.runPut (putHash h)

prop_shelleyHashInfo_hashSize :: Crypto c => ShelleyHash c -> Property
prop_shelleyHashInfo_hashSize h =
    hashSize === fromIntegral (Lazy.length serialisedHash)
  where
    HashInfo { hashSize, putHash } = shelleyHashInfo

    serialisedHash = Put.runPut (putHash h)

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
      toCBOR
      ((. Full) . runAnnotator <$> fromCBOR)
      (verifyBlockIntegrity testTPraosSlotsPerKESPeriod)

-- | Test that we can detect random bitflips in blocks.
prop_detectCorruption_Header :: Header Block -> Corruption -> Property
prop_detectCorruption_Header =
    detectCorruption
      toCBOR
      ((. Full) . runAnnotator <$> fromCBOR)
      (verifyHeaderIntegrity testTPraosSlotsPerKESPeriod)
