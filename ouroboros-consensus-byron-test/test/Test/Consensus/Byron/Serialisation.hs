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
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Byron.Serialisation (tests) where

import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor.Identity
import           Data.Proxy (Proxy (..))

import           Cardano.Chain.Block (ABlockOrBoundary (..))
import qualified Cardano.Chain.Block as CC.Block
import qualified Cardano.Chain.Update as CC.Update

import           Ouroboros.Network.Block (HeaderHash)

import           Ouroboros.Consensus.Block (ConvertRawHash (..), getCodecConfig)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Serialisation ()

import           Ouroboros.Consensus.Storage.Common (BinaryBlockInfo (..))

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node

import           Test.QuickCheck hiding (Result)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Cardano.Chain.Genesis.Dummy as CC

import           Test.Util.Corruption
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation

import           Test.Consensus.Byron.Generators

tests :: TestTree
tests = testGroup "Byron"
    [ testGroup "Serialisation roundtrips"
      [ testGroup "SerialiseDisk"         $ roundtrip_SerialiseDisk         testCodecCfg
      , testGroup "SerialiseNodeToNode"   $ roundtrip_SerialiseNodeToNode   testCodecCfg
      , testGroup "SerialiseNodeToClient" $ roundtrip_SerialiseNodeToClient testCodecCfg
      ]

    , testProperty "BinaryBlockInfo sanity check" prop_byronBinaryBlockInfo
    , testGroup "ConvertRawHashInfo sanity check"
        [ testProperty "fromRawHash/toRawHash" prop_fromRawHash_toRawHash
        , testProperty "hashSize sanity check" prop_hashSize
        ]

    , testGroup "Integrity"
        [ testProperty "detect corruption in RegularBlock" prop_detectCorruption_RegularBlock
        ]
    ]

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_byronBinaryBlockInfo :: ByronBlock -> Property
prop_byronBinaryBlockInfo blk =
    headerAnnotation === extractedHeader
  where
    BinaryBlockInfo { headerOffset, headerSize } =
      byronBinaryBlockInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        toLazyByteString (encodeByronBlock blk)

    headerAnnotation :: Lazy.ByteString
    headerAnnotation = Lazy.fromStrict $ case byronBlockRaw blk of
      ABOBBoundary b -> CC.Block.boundaryHeaderAnnotation $ CC.Block.boundaryHeader b
      ABOBBlock    b -> CC.Block.headerAnnotation         $ CC.Block.blockHeader    b


{-------------------------------------------------------------------------------
  ConvertRawHash
-------------------------------------------------------------------------------}

prop_fromRawHash_toRawHash
  :: HeaderHash ByronBlock -> Property
prop_fromRawHash_toRawHash h =
    h === fromRawHash p (toRawHash p h)
  where
    p = Proxy @ByronBlock

prop_hashSize
  :: HeaderHash ByronBlock -> Property
prop_hashSize h =
    hashSize p === fromIntegral (Strict.length (toRawHash p h))
  where
    p = Proxy @ByronBlock

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

-- | Matches the values used for the generators.
testCfg :: TopLevelConfig ByronBlock
testCfg = pInfoConfig protocolInfo
  where
    protocolInfo :: ProtocolInfo Identity ByronBlock
    protocolInfo =
      protocolInfoByron
        CC.dummyConfig
        (Just (PBftSignatureThreshold 0.5))
        (CC.Update.ProtocolVersion 1 0 0)
        (CC.Update.SoftwareVersion (CC.Update.ApplicationName "Cardano Test") 2)
        Nothing

-- | Matches the values used for the generators.
testCodecCfg :: CodecConfig ByronBlock
testCodecCfg = getCodecConfig (configBlock testCfg)
