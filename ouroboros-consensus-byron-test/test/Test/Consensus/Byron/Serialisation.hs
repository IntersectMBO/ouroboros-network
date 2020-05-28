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
import qualified Cardano.Chain.Update.Validation.Interface as CC.UPI

import           Ouroboros.Network.Block (HeaderHash)
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block (ConvertRawHash (..), Header)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo

import           Ouroboros.Consensus.Storage.ImmutableDB (BinaryInfo (..))

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node

import           Test.QuickCheck hiding (Result)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Cardano.Chain.Genesis.Dummy as CC

import           Test.Util.Corruption
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Roundtrip

import           Test.Consensus.Byron.Generators

tests :: TestTree
tests = testGroup "Byron"
    [ testGroup "Serialisation roundtrips"
        [ testProperty "roundtrip Block"       prop_roundtrip_Block
        , testProperty "roundtrip Header"      prop_roundtrip_Header
        , testProperty "roundtrip HeaderHash"  prop_roundtrip_HeaderHash
        , testProperty "roundtrip GenTx"       prop_roundtrip_GenTx
        , testProperty "roundtrip GenTxId"     prop_roundtrip_GenTxId
        , testProperty "roundtrip ApplyTxErr"  prop_roundtrip_ApplyTxErr
        , testProperty "roundtrip Query"       prop_roundtrip_Query
        , testProperty "roundtrip Result"      prop_roundtrip_Result
        ]
        -- TODO ConsensusState
        -- TODO LedgerState

    , testProperty "BinaryInfo sanity check"   prop_encodeByronBlockWithInfo
    , testGroup "ConvertRawHashInfo sanity check"
        [ testProperty "fromRawHash/toRawHash" prop_fromRawHash_toRawHash
        , testProperty "hashSize sanity check" prop_hashSize
        ]

    , testGroup "Integrity"
        [ testProperty "detect corruption in RegularBlock" prop_detectCorruption_RegularBlock
        ]
    ]

{-------------------------------------------------------------------------------
  Serialisation roundtrips
-------------------------------------------------------------------------------}

prop_roundtrip_Block :: ByronBlock -> Property
prop_roundtrip_Block b =
    roundtrip' encodeByronBlock (decodeByronBlock epochSlots) b

prop_roundtrip_Header :: SerialisationVersion ByronBlock
                      -> Header ByronBlock -> Property
prop_roundtrip_Header v h =
    not (isNodeToClientVersion v) ==>
    roundtrip'
      (encodeByronHeader v)
      (decodeByronHeader epochSlots v)
      h'
  where
    h' = case v of
           SerialisedAcrossNetwork (SerialisedNodeToNode ByronNodeToNodeVersion1) ->
             -- This is a lossy format
             h { byronHeaderBlockSizeHint = fakeByronBlockSizeHint }
           _otherwise ->
             h

prop_roundtrip_HeaderHash :: HeaderHash ByronBlock -> Property
prop_roundtrip_HeaderHash =
    roundtrip encodeByronHeaderHash decodeByronHeaderHash

prop_roundtrip_GenTx :: GenTx ByronBlock -> Property
prop_roundtrip_GenTx =
    roundtrip encodeByronGenTx decodeByronGenTx

prop_roundtrip_GenTxId :: GenTxId ByronBlock -> Property
prop_roundtrip_GenTxId =
    roundtrip encodeByronGenTxId decodeByronGenTxId

prop_roundtrip_ApplyTxErr :: ApplyTxErr ByronBlock -> Property
prop_roundtrip_ApplyTxErr =
    roundtrip encodeByronApplyTxError decodeByronApplyTxError

prop_roundtrip_Query :: Some (Query ByronBlock) -> Property
prop_roundtrip_Query =
    roundtrip
      (\case { Some query -> encodeByronQuery query })
      decodeByronQuery

prop_roundtrip_Result :: CC.UPI.State -> Property
prop_roundtrip_Result =
    roundtrip
      (encodeByronResult GetUpdateInterfaceState)
      (decodeByronResult GetUpdateInterfaceState)

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
