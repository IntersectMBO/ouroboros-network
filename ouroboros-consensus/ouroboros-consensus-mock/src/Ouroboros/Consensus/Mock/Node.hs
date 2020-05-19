{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node () where

import           Codec.Serialise (Serialise, decode, encode)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Data.Typeable (Typeable)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.HeaderValidation (defaultDecodeAnnTip,
                     defaultEncodeAnnTip)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.State

import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

{-------------------------------------------------------------------------------
  RunNode instance for the mock ledger
-------------------------------------------------------------------------------}

instance HasNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  -- Use defaults

instance ( LedgerSupportsProtocol (SimpleBlock SimpleMockCrypto ext)
           -- The below constraint seems redundant but is not! When removed,
           -- some of the tests loop, but only when compiled with @-O2@ ; with
           -- @-O0@ it is perfectly fine. ghc bug?!
         , BlockSupportsProtocol (SimpleBlock SimpleMockCrypto ext)
         , NoUnexpectedThunks (NodeState (SimpleBlock SimpleMockCrypto ext))
         , Typeable ext
         , Serialise ext
         , RunMockBlock SimpleMockCrypto ext
         ) => RunNode (SimpleBlock SimpleMockCrypto ext) where
  nodeForgeBlock            = forgeSimple
  nodeBlockMatchesHeader    = matchesSimpleHeader
  nodeBlockFetchSize        = fromIntegral . simpleBlockSize . simpleHeaderStd
  nodeIsEBB                 = const Nothing
  nodeImmDbChunkInfo        = \cfg -> simpleChunkInfo $
    EpochSize $ 10 * maxRollbacks (configSecurityParam cfg)
  nodeStartTime             = \_cfg -> SystemStart dummyDate
    where
      --  This doesn't matter much
      dummyDate = UTCTime (fromGregorian 2019 8 13) 0
  nodeNetworkMagic          = \_ -> NetworkMagic 0x0000ffff

  nodeProtocolMagicId       = mockProtocolMagicId
  nodeHashInfo              = const simpleBlockHashInfo
  nodeMaxBlockSize          = const 2000000 -- TODO
  nodeBlockEncodingOverhead = const 1000 -- TODO
  nodeTxInBlockSize         = txSize
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
