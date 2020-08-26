{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Consensus.Cardano.Serialisation (tests) where

import           Cardano.Crypto.Hash (ShortHash)
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as Lazy

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Util (Dict (..))

import           Ouroboros.Consensus.HardFork.Combinator (NestedCtxt_ (..))

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()
import           Ouroboros.Consensus.Shelley.Protocol

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node ()

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

import           Test.Consensus.Shelley.MockCrypto hiding (Block)

import           Test.Consensus.Cardano.Generators (epochSlots, k)

tests :: TestTree
tests = testGroup "Cardano"
    [ roundtrip_all testCodecCfg dictNestedHdr

      -- Note: the above roundtrip tests use 'TPraosMockCrypto' (because we
      -- most generators only work with mock crypto, not with real crypto).
      -- But because the Byron hash (currently not parameterised over crypto)
      -- is 32 bytes while the mock Shelley hash is 4 bytes, the
      -- 'prop_hashSize' is not included in 'roundtrip_all', as it would fail.
      --
      -- Therefore, we test 'prop_hashSize' separately, but with
      -- 'TPraosStandardCrypto', where the Shelley hash is 32 bytes. We also
      -- run 'roundtrip_ConvertRawHash' for the real crypto.
    , testProperty "hashSize real crypto"       $ prop_hashSize            pReal
    , testProperty "ConvertRawHash real crypto" $ roundtrip_ConvertRawHash pReal

    , testProperty "BinaryBlockInfo sanity check" prop_CardanoBinaryBlockInfo

    ]
  where
    pReal :: Proxy (CardanoBlock TPraosStandardCrypto)
    pReal = Proxy

testCodecCfg :: CardanoCodecConfig (TPraosMockCrypto ShortHash)
testCodecCfg =
  CardanoCodecConfig (ByronCodecConfig epochSlots k) ShelleyCodecConfig

dictNestedHdr
  :: forall a.
     NestedCtxt_ (CardanoBlock (TPraosMockCrypto ShortHash)) Header a
  -> Dict (Eq a, Show a)
dictNestedHdr = \case
    NCZ (CtxtByronBoundary {}) -> Dict
    NCZ (CtxtByronRegular  {}) -> Dict
    NCS (NCZ CtxtShelley)      -> Dict
    NCS (NCS x)                -> case x of {}

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_CardanoBinaryBlockInfo :: CardanoBlock (TPraosMockCrypto ShortHash) -> Property
prop_CardanoBinaryBlockInfo blk =
    encodedNestedHeader === extractedHeader
  where
    BinaryBlockInfo { headerOffset, headerSize } =
      getBinaryBlockInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        CBOR.toLazyByteString (encodeDisk testCodecCfg blk)

    encodedNestedHeader :: Lazy.ByteString
    encodedNestedHeader = case encodeDepPair testCodecCfg (unnest (getHeader blk)) of
        GenDepPair _ (Serialised bytes) -> bytes
