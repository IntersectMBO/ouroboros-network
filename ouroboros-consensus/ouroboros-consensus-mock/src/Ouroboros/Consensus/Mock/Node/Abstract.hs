{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Mock.Node.Abstract (
    RunMockBlock(..)
  , constructMockProtocolMagicId
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Crypto.Random (MonadRandom)
import           Data.Hashable (hash)
import           GHC.Stack

import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Node.State
import           Ouroboros.Consensus.Protocol.Abstract

-- | Protocol specific functionality required to run consensus with mock blocks
class RunMockBlock c ext where
  -- | Construct the protocol specific part of the block
  --
  -- This is used in 'forgeSimple', which takes care of the generic part of
  -- the mock block.
  forgeExt :: MonadRandom m
           => TopLevelConfig          (SimpleBlock c ext)
           -> Update m     (NodeState (SimpleBlock c ext))
           -> IsLeader (BlockProtocol (SimpleBlock c ext))
           -> SimpleBlock' c ext ()
           -> m (SimpleBlock c ext)

  mockProtocolMagicId  :: TopLevelConfig (SimpleBlock c ext)
                       -> ProtocolMagicId
  mockEncodeChainState :: TopLevelConfig (SimpleBlock c ext)
                       -> ChainState (BlockProtocol (SimpleBlock c ext)) -> Encoding
  mockDecodeChainState :: TopLevelConfig (SimpleBlock c ext)
                       -> Decoder s (ChainState (BlockProtocol (SimpleBlock c ext)))

-- | Construct protocol magic ID depending on where in the code this is called
--
-- The sole purpose of this is to make sure that these mock protocols have
-- different IDs from each other and from regular protocols.
constructMockProtocolMagicId :: HasCallStack => ProtocolMagicId
constructMockProtocolMagicId =
    ProtocolMagicId $ fromIntegral $ hash (prettyCallStack callStack)
