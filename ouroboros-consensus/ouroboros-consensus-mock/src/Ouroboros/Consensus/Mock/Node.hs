{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Mock.Node (
    CodecConfig (..)
  , simpleBlockForging
  ) where

import           Codec.Serialise (Serialise)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node.Abstract
import           Ouroboros.Consensus.Mock.Node.Serialisation ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.....:))

import           Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)

{-------------------------------------------------------------------------------
  RunNode instance for the mock ledger
-------------------------------------------------------------------------------}

instance HasNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  -- Use defaults

instance SupportedNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  supportedNodeToNodeVersions   _ = Map.singleton maxBound ()
  supportedNodeToClientVersions _ = Map.singleton maxBound ()

instance ( LedgerSupportsProtocol (SimpleBlock SimpleMockCrypto ext)
           -- The below constraint seems redundant but is not! When removed,
           -- some of the tests loop, but only when compiled with @-O2@ ; with
           -- @-O0@ it is perfectly fine. ghc bug?!
         , BlockSupportsProtocol (SimpleBlock SimpleMockCrypto ext)
         , Show (CannotForge     (SimpleBlock SimpleMockCrypto ext))
         , Show (ForgeStateInfo  (SimpleBlock SimpleMockCrypto ext))
         , Typeable ext
         , Serialise ext
         , RunMockBlock SimpleMockCrypto ext
         ) => RunNode (SimpleBlock SimpleMockCrypto ext) where
  nodeBlockFetchSize        = fromIntegral . simpleBlockSize . simpleHeaderStd
  nodeImmDbChunkInfo        = \cfg -> simpleChunkInfo $
    EpochSize $ 10 * maxRollbacks (configSecurityParam cfg)
  nodeCheckIntegrity        = \_ _ -> True

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Can be used when 'CanBeLeader' is static
simpleBlockForging ::
     ( RunMockBlock c ext
     , CannotForge    (SimpleBlock c ext) ~ Void
     , ForgeStateInfo (SimpleBlock c ext) ~ ()
     , Monad m
     )
  => CanBeLeader (BlockProtocol (SimpleBlock c ext))
  -> ForgeExt c ext
  -> BlockForging m (SimpleBlock c ext)
simpleBlockForging canBeLeader forgeExt = BlockForging {
      canBeLeader      = canBeLeader
    , updateForgeState = \_ -> return ()
    , checkCanForge    = \_ _ _ _ -> return Nothing
    , forgeBlock       = return .....: forgeSimple forgeExt
    }
