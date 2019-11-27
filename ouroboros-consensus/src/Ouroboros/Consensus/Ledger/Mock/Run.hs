{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Ouroboros.Consensus.Ledger.Mock.Run (
    RunMockProtocol(..)
  , RunMockBlock(..)
  , constructMockProtocolMagicId
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Crypto.Random (MonadRandom)
import           Data.Hashable (hash)
import           GHC.Stack

import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Protocol.Abstract

-- | The part of 'RunMock' that depends only on @p@
class RunMockProtocol p where
  mockProtocolMagicId  ::           NodeConfig p -> ProtocolMagicId
  mockEncodeChainState ::           NodeConfig p -> ChainState p -> Encoding
  mockDecodeChainState :: forall s. NodeConfig p -> Decoder s (ChainState p)

-- | Protocol specific functionality required to run consensus with mock blocks
class RunMockProtocol p => RunMockBlock p c ext where
  -- | Construct the protocol specific part of the block
  --
  -- This is used in 'forgeSimple', which takes care of the generic part of
  -- the mock block.
  forgeExt :: (HasNodeState p m, MonadRandom m)
           => NodeConfig p
           -> IsLeader p
           -> SimpleBlock' c ext ()
           -> m (SimpleBlock c ext)

-- | Construct protocol magic ID depending on where in the code this is called
--
-- The sole purpose of this is to make sure that these mock protocols have
-- different IDs from each other and from regular protocols.
constructMockProtocolMagicId :: HasCallStack => ProtocolMagicId
constructMockProtocolMagicId =
    ProtocolMagicId $ fromIntegral $ hash (prettyCallStack callStack)
