{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Ledger.Mock (tests) where

import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise (Serialise, encode)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy
import           Data.Typeable

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash

import           Ouroboros.Network.Block (BlockNo (..), ChainHash (..),
                     HeaderHash, SlotNo (..))

import           Ouroboros.Consensus.Block (getHeader)
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.UTxO

import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "Mock"
    [ props (Proxy @SimpleStandardCrypto) "SimpleStandardCrypto"
    , props (Proxy @SimpleMockCrypto)     "SimpleMockCrypto"
    ]
  where
    props :: forall c proxy.
             ( SimpleCrypto c
             , Arbitrary (HeaderHash (SimpleBlock c ()))
             )
          => proxy c -> String -> TestTree
    props _ title = testGroup title
      [ testProperty "BinaryInfo sanity check"       (prop_simpleBlockBinaryInfo @c @())
      , testGroup "HashInfo sanity check"
          [ testProperty "putHash/getHash roundtrip" (prop_simpleBlockHashInfo_roundtrip @c @())
          , testProperty "hashSize"                  (prop_simpleBlockHashInfo_hashSize @c @())
          ]
      ]

{-------------------------------------------------------------------------------
  BinaryInfo
-------------------------------------------------------------------------------}

prop_simpleBlockBinaryInfo
  :: (SimpleCrypto c, Serialise ext) => SimpleBlock c ext -> Property
prop_simpleBlockBinaryInfo blk =
    serialisedHeader === extractedHeader
  where
    BinaryInfo { binaryBlob, headerOffset, headerSize } =
      simpleBlockBinaryInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        toLazyByteString binaryBlob

    serialisedHeader :: Lazy.ByteString
    serialisedHeader = toLazyByteString $
      encodeSimpleHeader encode (getHeader blk)

{-------------------------------------------------------------------------------
  HashInfo
-------------------------------------------------------------------------------}

prop_simpleBlockHashInfo_roundtrip
  :: (SimpleCrypto c, Typeable ext)
  => HeaderHash (SimpleBlock c ext) -> Property
prop_simpleBlockHashInfo_roundtrip h =
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
    HashInfo { getHash, putHash } = simpleBlockHashInfo

    serialisedHash = Put.runPut (putHash h)

prop_simpleBlockHashInfo_hashSize
  :: (SimpleCrypto c, Typeable ext)
  => HeaderHash (SimpleBlock c ext) -> Property
prop_simpleBlockHashInfo_hashSize h =
    hashSize === fromIntegral (Lazy.length serialisedHash)
  where
    HashInfo { hashSize, putHash } = simpleBlockHashInfo

    serialisedHash = Put.runPut (putHash h)

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- These generators blindly create random values, so the block will not be
-- valid, but this does not matter for serialisation tests.

instance HashAlgorithm (SimpleHash c)
      => Arbitrary (SimpleBlock c ()) where
  arbitrary = SimpleBlock <$> arbitrary <*> arbitrary

instance HashAlgorithm (SimpleHash c)
      => Arbitrary (Header (SimpleBlock c ())) where
  arbitrary = do
    simpleStdHeader <- arbitrary
    let simpleHeader = SimpleHeader headerHash simpleStdHeader ()
        headerHash   = Hash.hashWithSerialiser
          (encodeSimpleHeader encode) simpleHeader
    return simpleHeader

instance HashAlgorithm (SimpleHash c)
      => Arbitrary (SimpleStdHeader c ()) where
  arbitrary = SimpleStdHeader
          <$> (pure GenesisHash) -- For simplicity
          <*> (SlotNo    <$> arbitrary)
          <*> (BlockNo   <$> arbitrary)
          <*> (Hash.hash <$> arbitrary)
          <*> arbitrary

instance Arbitrary SimpleBody where
  arbitrary = SimpleBody <$> listOf arbitrary

instance Arbitrary Tx where
  arbitrary = Tx
         <$> pure mempty  -- For simplicity
         <*> arbitrary

instance (HashAlgorithm h, HashAlgorithm (SimpleHash c))
      => Arbitrary (Hash h (Header (SimpleBlock c ()))) where
  arbitrary = Hash.hashWithSerialiser (encodeSimpleHeader encode) <$> arbitrary
