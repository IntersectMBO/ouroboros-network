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
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash

import           Ouroboros.Network.Block (BlockNo (..), ChainHash (..),
                     HeaderHash, SlotNo (..))

import           Ouroboros.Consensus.Block (ConvertRawHash (..), getHeader)
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.UTxO

import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))

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
      [ testProperty "BinaryBlockInfo sanity check" (prop_simpleBlockBinaryBlockInfo @c @())
      , testGroup "ConvertRawHash sanity check"
          [ testProperty "fromRawHash/toRawHash" (prop_simpleBlock_fromRawHash_toRawHash @c @())
          , testProperty "hashSize sanity check" (prop_simpleBlock_hashSize @c @())
          ]
      ]

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_simpleBlockBinaryBlockInfo
  :: (SimpleCrypto c, Serialise ext) => SimpleBlock c ext -> Property
prop_simpleBlockBinaryBlockInfo blk =
    serialisedHeader === extractedHeader
  where
    BinaryBlockInfo { headerOffset, headerSize } =
      simpleBlockBinaryBlockInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        toLazyByteString (encode blk)

    serialisedHeader :: Lazy.ByteString
    serialisedHeader = toLazyByteString $
      encodeSimpleHeader encode (getHeader blk)

{-------------------------------------------------------------------------------
  ConvertRawHash
-------------------------------------------------------------------------------}

prop_simpleBlock_fromRawHash_toRawHash
  :: forall c ext. SimpleCrypto c
  => HeaderHash (SimpleBlock c ext) -> Property
prop_simpleBlock_fromRawHash_toRawHash h =
    h === fromRawHash p (toRawHash p h)
  where
    p = Proxy @(SimpleBlock c ext)

prop_simpleBlock_hashSize
  :: forall c ext. SimpleCrypto c
  => HeaderHash (SimpleBlock c ext) -> Property
prop_simpleBlock_hashSize h =
    hashSize p === fromIntegral (Strict.length (toRawHash p h))
  where
    p = Proxy @(SimpleBlock c ext)

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
  arbitrary = Tx DoNotExpire
         <$> pure mempty  -- For simplicity
         <*> arbitrary

instance (HashAlgorithm h, HashAlgorithm (SimpleHash c))
      => Arbitrary (Hash h (Header (SimpleBlock c ()))) where
  arbitrary = Hash.hashWithSerialiser (encodeSimpleHeader encode) <$> arbitrary
