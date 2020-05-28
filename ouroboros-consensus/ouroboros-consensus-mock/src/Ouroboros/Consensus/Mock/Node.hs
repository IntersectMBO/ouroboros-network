{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node () where

import           Codec.Serialise (Serialise, decode, encode)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Data.Typeable (Typeable)

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.HeaderValidation (defaultDecodeAnnTip,
                     defaultEncodeAnnTip)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run

import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

{-------------------------------------------------------------------------------
  RunNode instance for the mock ledger
-------------------------------------------------------------------------------}

instance HasNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  -- Use defaults

instance RunMockBlock SimpleMockCrypto ext
      => ConfigSupportsNode (SimpleBlock SimpleMockCrypto ext) where

  -- | No additional codec information is required for simple blocks
  data CodecConfig (SimpleBlock SimpleMockCrypto ext) = SimpleCodecConfig

  getCodecConfig = const SimpleCodecConfig
  getSystemStart = const $ SystemStart dummyDate
    where
      --  This doesn't matter much
      dummyDate = UTCTime (fromGregorian 2019 8 13) 0
  getNetworkMagic    = const $ NetworkMagic 0x0000ffff
  getProtocolMagicId = mockProtocolMagicId

instance ( LedgerSupportsProtocol (SimpleBlock SimpleMockCrypto ext)
           -- The below constraint seems redundant but is not! When removed,
           -- some of the tests loop, but only when compiled with @-O2@ ; with
           -- @-O0@ it is perfectly fine. ghc bug?!
         , BlockSupportsProtocol (SimpleBlock SimpleMockCrypto ext)
         , CanForge (SimpleBlock SimpleMockCrypto ext)
         , Typeable ext
         , Serialise ext
         , RunMockBlock SimpleMockCrypto ext
         ) => RunNode (SimpleBlock SimpleMockCrypto ext) where
  nodeBlockFetchSize        = fromIntegral . simpleBlockSize . simpleHeaderStd
  nodeImmDbChunkInfo        = \cfg -> simpleChunkInfo $
    EpochSize $ 10 * maxRollbacks (configSecurityParam cfg)
  nodeHashInfo              = const simpleBlockHashInfo
  nodeCheckIntegrity        = \_ _ -> True

  nodeEncodeBlockWithInfo   = const simpleBlockBinaryInfo
  nodeEncodeHeader          = \_ _ -> encode
  nodeEncodeWrappedHeader   = \_ _ -> encode
  nodeEncodeGenTx           =       encode
  nodeEncodeGenTxId         =       encode
  nodeEncodeHeaderHash      = const encode
  nodeEncodeLedgerState     = const encode
  nodeEncodeConsensusState  = mockEncodeConsensusState
  nodeEncodeApplyTxError    = const encode
  nodeEncodeAnnTip          = const $ defaultEncodeAnnTip encode
  nodeEncodeQuery           = \case {}
  nodeEncodeResult          = \case {}

  nodeDecodeBlock           = const (const <$> decode)
  nodeDecodeHeader          = \_ _ -> (const <$> decode)
  nodeDecodeWrappedHeader   = \_ _ -> decode
  nodeDecodeGenTx           =       decode
  nodeDecodeGenTxId         =       decode
  nodeDecodeHeaderHash      = const decode
  nodeDecodeLedgerState     = const decode
  nodeDecodeConsensusState  = mockDecodeConsensusState
  nodeDecodeApplyTxError    = const decode
  nodeDecodeAnnTip          = const $ defaultDecodeAnnTip decode
  nodeDecodeQuery           = error "Mock.nodeDecodeQuery"
  nodeDecodeResult          = \case {}
