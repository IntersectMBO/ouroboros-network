{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node (
    CodecConfig (..)
  ) where

import           Codec.Serialise (Serialise, decode, encode)
import           Data.Typeable (Typeable)

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Block
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
  nodeCheckIntegrity        = \_ _ -> True

  nodeEncodeBlockWithInfo   = \_   -> simpleBlockBinaryInfo
  nodeEncodeHeader          = \_ _ -> encode
  nodeEncodeWrappedHeader   = \_ _ -> encode
  nodeEncodeGenTx           = \_   -> encode
  nodeEncodeGenTxId         = \_   -> encode
  nodeEncodeHeaderHash      = \_   -> encode
  nodeEncodeLedgerState     = \_   -> encode
  nodeEncodeConsensusState  = mockEncodeConsensusState
  nodeEncodeApplyTxError    = \_   -> encode
  nodeEncodeAnnTip          = \_   -> defaultEncodeAnnTip encode
  nodeEncodeQuery           = \_   -> \case {}
  nodeEncodeResult          = \_   -> \case {}

  nodeDecodeBlock           = \_   -> (const <$> decode)
  nodeDecodeHeader          = \_ _ -> (const <$> decode)
  nodeDecodeWrappedHeader   = \_ _ -> decode
  nodeDecodeGenTx           = \_   -> decode
  nodeDecodeGenTxId         = \_   -> decode
  nodeDecodeHeaderHash      = \_   -> decode
  nodeDecodeLedgerState     = \_   -> decode
  nodeDecodeConsensusState  = mockDecodeConsensusState
  nodeDecodeApplyTxError    = \_   -> decode
  nodeDecodeAnnTip          = \_   -> defaultDecodeAnnTip decode
  nodeDecodeQuery           = \_   -> error "Mock.nodeDecodeQuery"
  nodeDecodeResult          = \_   -> \case {}
