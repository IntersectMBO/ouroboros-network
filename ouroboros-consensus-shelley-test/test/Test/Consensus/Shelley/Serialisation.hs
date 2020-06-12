{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus.Shelley.Serialisation (tests) where

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)

import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))
import           Ouroboros.Consensus.Util (Dict (..))

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
    [ roundtrip_all testCodecCfg dictNestedHdr

      -- 'roundtrip_ConvertRawHash' for mock crypto is included in
      -- 'roundtrip_all', but 'prop_hashSize' is not
    , testProperty "hashSize mock crypto" $ prop_hashSize pMock

      -- Test for real crypto too
    , testProperty "hashSize real crypto"       $ prop_hashSize pReal
    , testProperty "ConvertRawHash real crypto" $ roundtrip_ConvertRawHash pReal

    , testProperty "BinaryBlockInfo sanity check" prop_shelleyBinaryBlockInfo

    , testGroup "Integrity"
        [ testProperty "generate non-corrupt blocks"  prop_blockIntegrity
        , testProperty "generate non-corrupt headers" prop_headerIntegrity
        , testProperty "detect corruption in blocks"  prop_detectCorruption_Block
        , testProperty "detect corruption in headers" prop_detectCorruption_Header
        ]
    ]
  where
    pMock :: Proxy (ShelleyBlock TPraosMockCrypto)
    pMock = Proxy

    pReal :: Proxy (ShelleyBlock TPraosStandardCrypto)
    pReal = Proxy

    testCodecCfg :: CodecConfig Block
    testCodecCfg = ShelleyCodecConfig

    dictNestedHdr :: forall a c. Crypto c
                  => NestedCtxt_ (ShelleyBlock c) Header a -> Dict (Eq a, Show a)
    dictNestedHdr CtxtShelley = Dict

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
