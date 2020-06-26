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

import           Codec.Serialise (Serialise)
import           Data.Typeable (Typeable)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Mock.Node.Serialisation ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run

import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

{-------------------------------------------------------------------------------
  RunNode instance for the mock ledger
-------------------------------------------------------------------------------}

instance HasNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  -- Use defaults

instance TranslateNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  nodeToNodeProtocolVersion   _ _ = NodeToNodeV_1
  nodeToClientProtocolVersion _ _ = NodeToClientV_2

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
  nodeGetBinaryBlockInfo    = simpleBlockBinaryBlockInfo
