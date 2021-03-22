{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Consensus.Cardano.Serialisation (
    tests
  ) where

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as Lazy

import           Ouroboros.Network.Block (Serialised (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util (Dict (..))

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node ()

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node ()

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.Node ()

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

import           Test.Consensus.Cardano.Generators (epochSlots)
import           Test.Consensus.Cardano.MockCrypto (MockCryptoCompatByron)

tests :: TestTree
tests = testGroup "Cardano"
    [ roundtrip_all testCodecCfg dictNestedHdr
    , testProperty "BinaryBlockInfo sanity check" prop_CardanoBinaryBlockInfo
    ]

testCodecCfg :: CardanoCodecConfig MockCryptoCompatByron
testCodecCfg =
  CardanoCodecConfig
    (ByronCodecConfig epochSlots)
    ShelleyCodecConfig
    ShelleyCodecConfig
    ShelleyCodecConfig

dictNestedHdr
  :: forall a.
     NestedCtxt_ (CardanoBlock MockCryptoCompatByron) Header a
  -> Dict (Eq a, Show a)
dictNestedHdr = \case
    NCZ (CtxtByronBoundary {})        -> Dict
    NCZ (CtxtByronRegular  {})        -> Dict
    NCS (NCZ CtxtShelley)             -> Dict
    NCS (NCS (NCZ CtxtShelley))       -> Dict
    NCS (NCS (NCS (NCZ CtxtShelley))) -> Dict

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_CardanoBinaryBlockInfo :: CardanoBlock MockCryptoCompatByron -> Property
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
