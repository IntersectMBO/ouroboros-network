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

import           Ouroboros.Network.Block (HeaderHash)

import           Ouroboros.Consensus.Block (ConvertRawHash (..))
import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node.Serialisation ()
import           Ouroboros.Consensus.Shelley.Protocol

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Corruption
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation

import           Test.Consensus.Shelley.Generators ()
import           Test.Consensus.Shelley.MockCrypto

tests :: TestTree
tests = testGroup "Shelley"
    [ roundtrip_all testCodecCfg

    , testProperty "BinaryBlockInfo sanity check" prop_shelleyBinaryBlockInfo

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

    testCodecCfg :: CodecConfig Block
    testCodecCfg = ShelleyCodecConfig

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_shelleyBinaryBlockInfo :: Block -> Property
prop_shelleyBinaryBlockInfo blk =
    encodedHeader === extractedHeader
  where
    BinaryBlockInfo { headerOffset, headerSize } =
      shelleyBinaryBlockInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        CBOR.toLazyByteString (encodeShelleyBlock blk)

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
